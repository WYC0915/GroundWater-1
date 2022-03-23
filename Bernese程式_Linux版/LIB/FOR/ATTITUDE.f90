MODULE s_ATTITUDE
CONTAINS

! ------------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! ------------------------------------------------------------------------------

  SUBROUTINE attitude(leosvn,epo,apposv,ityp,attit)

! ------------------------------------------------------------------------------
!  Purpose    :  Returns attitude rotation matrix relating Leo body  fixed
!                system and true system of epoch. Attitude rotation matrix
!                in additional file should rotate from Leo body fixed
!                system to true system of epoch.
!
!  Remarks    :  Additional parameters xsun, bmin are used only for
!                attflag=3 and 4. Sun position is given in J2000.0, not
!                in the true system!
!                Excluded i_leoprj.
!                bmin not defined => see change on 05-Jul-2002
!                Be careful with attflag = 3 (GIPSY model)!!!!!
!
!  Author     :  H.Bock, U.Hugentobler, D.Svehla, M.Rothacher
!
!  Created    :  20-Mar-2001
!
!  Changes    :  21-Dec-2001 HU: Use d_const
!                07-Jan-2002 HB: new parameter for SR readatt
!                02-Jul-2002 DS: New attitude model when using velocity:
!                                reference vector:VELOCITY,attflg=2.
!                                attflg=1 remains for reference vector: RADIAL
!                05-Jul-2002 HB: set bmin to 15 degree (TOPEX and JASON)
!                30-Jul-2002 HU: Use interface for readatt
!                18-Aug-2002 DS: Check external attitude information (quaternions)
!                                with attitude from a priori orbit
!                22-Aug-2002 MR: Allow for attflag=0: no attitude
!                23-Aug-2002 DS: Check velocity vector
!                23-Aug-2002 DS: Write warning if attflag=0
!                12-Oct-2002 DS: Allow attitude for LEO constellation
!                01-Mar-2003 DS: "Cosmetic" change
!                24-Apr-2003 RD: Corrected format statement
!                08-Jun-2003 HU: Check absolute value of beta
!                09-Jun-2003 HU: CNES attitude model corrected
!                10-Apr-2004 HU: DACOS of argument > 1 avoided
!                22-Nov-2004 AJ: Allow for negative attflag
!                10-Jun-2005 HU: epoatt is in/out for readatt
!                22-Jul-2005 HU: Initialize eposav=0.D0
!                            HB: Check epoDif from readatt
!                30-Apr-2007 HB: Increase MaxAngl from 5 to 10
!                                Add attitude flag 5 for GOCE
!                30-May-2007 HB: Add attitude flag 6 for GRAS/MetOp
!                                Move extatt=0 to the right place
!                30-May-2007 AG: Use s_suneff
!                01-Apr-2008 HB: Interpolate attitude up to 5 min
!                                (before only 1 min)
!                02-Sep-2008 HB: Interpolate attitude up to 10 min
!                01-OCT-2010 CR: New call of SUNEFF
!                08-Dec-2010 HB: Write warning message only once
!                03-Aug-2011 HB: Deactivate attitude check for
!                                MetOp-A (914) and Jason-2 (927)
!                14-Nov-2011 HB: Deactivate attitude check for GOCE (915)
!                07-Dec-2012 RD: Each warning messages is printed only once
!                07-Dec-2012 RD: Remove unused variables; use M_BERN with ONLY
!
!  Copyright  :  Astronomical Institute
!                University of Bern
!                Switzerland
! ------------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnerr, lfnprt
  USE d_const,  ONLY: pi
  USE s_readatt
  USE s_vprod
  USE s_suneff
  USE s_gtattflg
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
! IN :
! ----
  INTEGER(i4b)             :: leosvn   ! LEO number
  REAL(r8b)                :: epo      ! epoch
  REAL(r8b),DIMENSION(*)   :: apposv   ! A priori position and velocities
                                       !  see remark
  INTEGER(i4b)             :: ityp     ! Direction of rotation, rotates
                                       !  =0:from Leo body fixed system to
                                       !     true system of epoch
                                       !  =1:from true system of epoch to
                                       !     Leo body fixed system
! OUT :
! -----
  REAL(r8b),DIMENSION(3,3) :: attit    ! Attitude rotation matrix

! List of functions
! -----------------
! Local Types
! -----------
! Local Parameters
! ----------------

! Local Variables
! ---------------
  INTEGER(i4b), SAVE       :: attflag  ! Attitude flag
                                       !=0:No attitude information applied
                                       !=1:Use velocity vector (apposv(4:6))
                                       !   for attitude matrix
                                       !   - reference RADIAL direction
                                       !=2:Use velocity vector (apposv(4:6))
                                       !   for attitude matrix
                                       !   - reference VELOCITY direction
                                       !=3:Nominal Sun tracking attitude, with
                                       !   Solar panel axis Y perpendicular
                                       !   to Sun direction, (yaw-steering,
                                       !   pitch = roll =0) GIPSY MODEL
                                       !=4:Nominal Sun tracking attitude, with
                                       !   Solar panel axis Y perpendicular
                                       !   to Sun direction, (yaw-steering,
                                       !   pitch = roll =0) CNES MODEL
  INTEGER(i4b), SAVE       :: attsign  != 1: Use velocity vector
                                       !=-1: Use negativ velocity vector
  REAL(r8b),DIMENSION(4)   :: xsun     ! Sun position in J2000.0
                                       !   not in true system!   (m)
  REAL(r8b),SAVE           :: bmin     ! Minimum beta angle    (rad)

  INTEGER(i4b)             :: IORSYS,iattchk,ICOL,intPol,extAtt
  INTEGER(i4b),SAVE        :: irc

  REAL(r8b)                :: epoDif
  REAL(r8b)                :: epoatt, eposav=0D0
  REAL(r8b)                :: ev,er,r,v,en,r2,rv,coso,TDT
  REAL(r8b)                :: rsat,vvsat,vrsat
  REAL(r8b)                :: beta,cosy,siny
  REAL(r8b)                :: picos
  REAL(r8b)                :: alat,offang,sinoff,cosoff
  REAL(r8b)                :: rotxci,rotyci,rotx,roty,rotx2,roty2,rotxy
  REAL(r8b)                :: sinomg,cosomg,sinbet,betap
  REAL(r8b)                :: yawang,cosyaw,sinyaw
  REAL(r8b)                :: MaxAngl,angle,anglec

  REAL(r8b),DIMENSION(3)   :: ssun,dum3
  REAL(r8b),DIMENSION(3)   :: nvec,vorb
  REAL(r8b),DIMENSION(3,3) :: spf,sbfp,sbf,hlp
  REAL(r8b),DIMENSION(3,3) :: ersw,exyz,attitA

  LOGICAL,  DIMENSION(6), SAVE :: prtMsg = .TRUE.


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  iattchk=1
  MaxAngl=10.0D0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



!
! look for attitude file and attitude definition flag
! ===================================================
  CALL gtattflg(leosvn,epo,attflag)
  IF (attflag < 0) THEN
    attflag = ABS(attflag)
    attsign = -1
  ELSE
    attsign = 1
  END IF
  IF (attflag == 3.OR.attflag == 4)bmin = 15.D0*PI/180.D0

!
! Read attitude
! =============
  epoatt=epo
  CALL readatt(leosvn,2,0,epoatt,attit,epoDif,irc)

  IF (irc == 0 .AND. epoDif*1440.D0 <= 10.D0) THEN
    intPol = 1
  ELSE
    intPol = 0
  ENDIF
  IF (iattchk==1) THEN
    attitA(1:3,:)=attit(1:3,:)
  END IF

! Use external attitude if interpolation allowed and no check wanted
! Jump over computation of nominal attitude
! ------------------------------------------------------------------
  IF (irc == 0 .AND. intPol == 1 .AND. iattchk /= 1) GOTO 905
!
! Attitude rotation matrix not found, proceed with attitude flag
! ==============================================================
! Yaw-steering
! ------------
  IF (attflag==3.OR.attflag==4) THEN
!
! Get sun position
! -----------------
    IORSYS=2
    TDT=epo+(19.D0+32.184D0)/86400.D0
    CALL SUNEFF(IORSYS,2.D0,TDT,xsun,dum3)
  ENDIF
!
! Attitude model
! ==============
  SELECT CASE (attflag)
!
! No Attitude at all (unit matrix)
! ================================
  CASE (0)
    attit=0.d0
!    attit(1,1)=1.d0
!    attit(2,2)=1.d0
!    attit(3,3)=1.d0
    attit(1:3,1:3)=0.d0
    IF (prtMsg(1)) THEN
      prtMsg(1)=.FALSE.
      WRITE(LFNERR,"(/,'### SR ATTITUDE: Attitude flag in the Satellite info &
        &file set to zero! -> ATTITUDE MATRIX SET TO ZERO FOR ALL EPOCHS!',/)")
    END IF
!
! Use velocity vector (apposv(4:6)) to get attitude matrix
! reference RADIAL direction
! =========================================================
  CASE (1)
!
! Check velocity vector
! ---------------------
    IF (apposv(4)==1.D20) THEN
      IF (prtMsg(2)) THEN
        prtMsg(2)=.FALSE.
        WRITE(LFNERR,"(/,'### SR ATTITUDE: No velocity information &
                     &to compute attitude, ATTITUDE SET TO ZERO')")
      END IF
      attit(1:3,1:3)=0.D0
      GOTO 907
    END IF
!
! Unit vector ER (radial "R")(geocenter-to-CoM)
! ---------------------------------------------
    rsat=DSQRT(apposv(1)**2+apposv(2)**2+apposv(3)**2)
    ersw(1,:)=apposv(1:3)/rsat
!
! Unit vector ES (alongtrack "S")
! -------------------------------
    vvsat=apposv(4)**2+apposv(5)**2+apposv(6)**2
    vrsat=DOT_PRODUCT(apposv(4:6),ersw(1,:))
    ersw(2,:)=(apposv(4:6)-vrsat*ersw(1,:))/DSQRT(vvsat-vrsat**2)
!
! Unit vector EW (crosstrack "W")
! -------------------------------
    CALL vprod(ersw(1,:),ersw(2,:),ersw(3,:))
!
! Attitude rotation matrix
! ------------------------
    attit(:,1)=ersw(2,:)
    attit(:,2)=-ersw(3,:)
    attit(:,3)=-ersw(1,:)

    IF (attsign == -1) THEN
      attit(:,1)=-attit(:,1)
      attit(:,2)=-attit(:,2)
    END IF
!
! Use velocity vector (apposv(4:6)) to get attitude matrix
! reference VELOCITY direction
! =========================================================
  CASE (2)
!
! Check velocity vector
! ---------------------
    IF (apposv(4)==1.D20) THEN
      IF (prtMsg(3)) THEN
        prtMsg(3)=.FALSE.
        WRITE(LFNERR,"(/,'### SR ATTITUDE: No velocity information &
                     &to compute attitude, ATTITUDE SET TO ZERO')")
      END IF
      attit(1:3,1:3)=0.D0
      GOTO 907
    END IF
!
! Unit vector - radial (geocenter-to-CoM)
! ---------------------------------------
    rsat=DSQRT(apposv(1)**2+apposv(2)**2+apposv(3)**2)
    ersw(1,:)=apposv(1:3)/rsat
!
! Unit vector - velocity (alongtrack "T")
! ---------------------------------------
    vvsat=DSQRT(apposv(4)**2+apposv(5)**2+apposv(6)**2)
    ersw(2,:)=apposv(4:6)/vvsat
!
! Unit vector EW (crosstrack "W")
! -------------------------------
    CALL vprod(ersw(1,:),ersw(2,:),ersw(3,:))
!
! Unit vector EN (nadir "N")
! --------------------------
    CALL vprod(ersw(2,:),ersw(3,:),ersw(1,:))
!
! Attitude rotation matrix
! ------------------------
    attit(:,1)=ersw(2,:)
    attit(:,2)=-ersw(3,:)
    attit(:,3)=-ersw(1,:)
!
    IF (attsign == -1) THEN
      attit(:,1)=-attit(:,1)
      attit(:,2)=-attit(:,2)
    END IF
!!!
!!! GPS like nominal yaw steering
!!! =============================
!!  CASE (3)
!!!
!!! Check velocity vector
!!! ---------------------
!!    IF (apposv(4)==1.D20) THEN
!!      IF (prtMsg(4)) THEN
!!        prtMsg(4)=.FALSE.
!!        WRITE(LFNERR,"(/,'### SR ATTITUDE: No velocity information &
!!                     &to compute attitude, ATTITUDE SET TO ZERO')")
!!      END IF
!!      attit(1:3,1:3)=0.D0
!!      GOTO 907
!!    END IF
!!!
!!! Unit vector ER (radial "R")(geocenter-to-CoM)
!!! ---------------------------------------------
!!    rsat=DSQRT(apposv(1)**2+apposv(2)**2+apposv(3)**2)
!!    exyz(3,:)=apposv(1:3)/rsat
!!!
!!! Unit vector EY (perpendicular to Sun and R)
!!! -------------------------------------------
!!    CALL vprod(exyz(3,1:3),xsun(1:3),exyz(2,:))
!!    r=DSQRT(DOT_PRODUCT(exyz(2,:),exyz(2,:)))
!!    exyz(2,:)=exyz(2,:)/r
!!!
!!! Unit vector EW (crosstrack "W")
!!! -------------------------------
!!    CALL vprod(exyz(3,:),exyz(2,:),exyz(1,:))
!!!
!!! Attitude rotation matrix
!!! ------------------------
!!    attit(:,1)= exyz(1,:)    ! - for gps
!!    attit(:,2)= exyz(2,:)    ! - for gps
!!    attit(:,3)=-exyz(3,:)

! Nominal Sun tracking attitude, with Solar panel axis Y perpendicular
! to Sun direction, (yaw-steering,  pitch = roll =0) GIPSY MODEL
! ====================================================================
  CASE (3)                              ! do not use

! Unit vector ER (radial "R")
! ---------------------------
    rsat=DSQRT(apposv(1)**2+apposv(2)**2+apposv(3)**2)
    ersw(1,:)=apposv(1:3)/rsat
!
! Unit vector ES (alongtrack "S")
! -------------------------------
    vvsat=apposv(4)**2+apposv(5)**2+apposv(6)**2
    vrsat=DOT_PRODUCT(apposv(4:6),ersw(1,:))
    ersw(2,:)=(apposv(4:6)-vrsat*ersw(1,:))/DSQRT(vvsat-vrsat**2)
!
! Unit vector EW (crosstrack "W")
! -------------------------------
    CALL vprod(ersw(1,:),ersw(2,:),ersw(3,:))
!
! Transform sun direction into RSW
! ---------------------------------
    ssun=matmul(ersw,xsun(1:3))
!
! Elevation angle beta of sun above orbital plane
! -----------------------------------------------
    beta=DASIN(DMIN1(ssun(3)/xsun(4),1d0))

    r2 = dot_product(apposv(1:3),apposv(1:3))
    r  = sqrt(r2)
    v  = sqrt(dot_product(apposv(4:6),apposv(4:6)))
    rv = dot_product(apposv(1:3),apposv(4:6))
    er = dot_product(xsun(1:3),apposv(1:3))
    ev = dot_product(xsun(1:3),apposv(4:6))
    call vprod(apposv(1:3),apposv(4:6),nvec)
    en = dot_product(xsun(1:3),nvec)
    rv = rv/(r*v)
    er = er/(xsun(4)*r)
    ev = ev/(xsun(4)*v)
    en = en/(xsun(4)*r*v)

    coso=(ev-rv*er)/(1-en**2)
    IF (DABS(beta) <= bmin) THEN
      picos = 0.D0
    ELSE
      picos=pi/2+(pi/2-beta)*coso
    ENDIF
    cosy=-cos(picos)
    siny= sin(picos)
!
! Transformation of unit vectors from RSW to XYZ
! ----------------------------------------------
    exyz(1,:)=-cosy*ersw(2,:)-siny*ersw(3,:)
    exyz(2,:)=-siny*ersw(2,:)+cosy*ersw(3,:)
    exyz(3,:)=-ersw(1,:)
!
! Attitude rotation matrix
! ------------------------
    attit=transpose(exyz)
!
! Nominal Sun tracking attitude, with Solar panel axis Y perpendicular
! to Sun direction, (yaw-steering,  pitch = roll =0) CNES MODEL
! ====================================================================
  CASE (4)
!
! transf spf -> inertial
! ----------------------
    r  = sqrt(dot_product(apposv(1:3),apposv(1:3)))
    spf(1:3,3)=-apposv(1:3)/r
    call vprod(apposv(4:6),apposv(1:3),nvec)
    r  = sqrt(dot_product(nvec,nvec))
    spf(1:3,2)=nvec/r
    call vprod(spf(1:3,2),spf(1:3,3),spf(1:3,1))
!
! transf spf1 -> spf
! ------------------
    r = sqrt(apposv(1)**2+apposv(2)**2)
    alat=atan2(apposv(3),r)
    offang=-2.7720686e-3*sin(2*alat)
    sinoff=sin(offang)
    cosoff=cos(offang)
    rotxci=-apposv(2)/r
    rotyci= apposv(1)/r
    rotx=rotxci*spf(1,1)+rotyci*spf(2,1)
    roty=rotxci*spf(1,2)+rotyci*spf(2,2)
    rotx2=rotx*rotx
    roty2=roty*roty
    rotxy=rotx*roty
    sbfp(1,1)=rotx2+roty2*cosoff
    sbfp(2,1)=rotxy*(1-cosoff)
    sbfp(3,1)=roty*sinoff
    sbfp(1,2)=sbfp(2,1)
    sbfp(2,2)=roty2+rotx2*cosoff
    sbfp(3,2)=-rotx*sinoff
    sbfp(1,3)=-roty*sinoff
    sbfp(2,3)= rotx*sinoff
    sbfp(3,3)= cosoff
!
! Angles omega and beta
! ---------------------
    call vprod(spf(1:3,2),xsun(1:3),vorb)
    r = sqrt(dot_product(vorb,vorb))
    vorb=-vorb/r
    sinomg=dot_product(spf(1:3,1),vorb(1:3))
    cosomg=dot_product(spf(1:3,3),vorb(1:3))
!!  sinbet=-dot_product(spf(1:3,2),xsun(1:3))/xsun(4)
    sinbet=dot_product(spf(1:3,2),xsun(1:3))/xsun(4)
    betap =asin(sinbet)

    IF (DABS(betap) <= bmin) THEN
      yawang = 0.D0
    ELSE
!!  yawang=pi/2+(pi/2-betap)*cosomg
    yawang=-pi/2-(pi/2-betap)*cosomg
    ENDIF
    cosyaw=cos(yawang)
    sinyaw=sin(yawang)
!
! sbf -> sbf1
! -----------
    sbf(1,1)= cosyaw
    sbf(2,1)= sinyaw
    sbf(3,1)= 0d0
    sbf(1,2)=-sinyaw
    sbf(2,2)= cosyaw
    sbf(3,2)= 0d0
    sbf(1,3)= 0d0
    sbf(2,3)= 0d0
    sbf(3,3)= 1d0
!
! Attitude rotation matrix
! ------------------------
    attit=matmul(matmul(spf,sbfp),sbf)
!
! Use velocity vector (apposv(4:6)) to get attitude matrix
! reference RADIAL direction (GOCE attitude)
! =========================================================
  CASE (5)
!
! Check velocity vector
! ---------------------
    IF (apposv(4)==1.D20) THEN
      IF (prtMsg(5)) THEN
        prtMsg(5)=.FALSE.
        WRITE(LFNERR,"(/,'### SR ATTITUDE: No velocity information &
                     &to compute attitude, ATTITUDE SET TO ZERO')")
      END IF
      attit(1:3,1:3)=0.D0
      GOTO 907
    END IF
!
! Unit vector ER (radial "R")(geocenter-to-CoM)
! ---------------------------------------------
    rsat=DSQRT(apposv(1)**2+apposv(2)**2+apposv(3)**2)
    ersw(1,:)=apposv(1:3)/rsat
!
! Unit vector ES (alongtrack "S")
! -------------------------------
    vvsat=apposv(4)**2+apposv(5)**2+apposv(6)**2
    vrsat=DOT_PRODUCT(apposv(4:6),ersw(1,:))
    ersw(2,:)=(apposv(4:6)-vrsat*ersw(1,:))/DSQRT(vvsat-vrsat**2)
!
! Unit vector EW (crosstrack "W")
! -------------------------------
    CALL vprod(ersw(1,:),ersw(2,:),ersw(3,:))
!
! Attitude rotation matrix
! ------------------------
    attit(:,1)=ersw(2,:)
    attit(:,2)=-ersw(3,:)
    attit(:,3)=-ersw(1,:)

    IF (attsign == -1) THEN
      attit(:,2)=-attit(:,2)
      attit(:,3)=-attit(:,3)
    END IF
!
! GRAS attitude
! =========================================================
  CASE (6)
!
! Check velocity vector
! ---------------------
    IF (apposv(4)==1.D20) THEN
      IF (prtMsg(6)) THEN
        prtMsg(6)=.FALSE.
        WRITE(LFNERR,"(/,'### SR ATTITUDE: No velocity information &
                     &to compute attitude, ATTITUDE SET TO ZERO')")
      END IF
      attit(1:3,1:3)=0.D0
      GOTO 907
    END IF
!
! Unit vector ER (radial "R")(geocenter-to-CoM)
! ---------------------------------------------
    rsat=DSQRT(apposv(1)**2+apposv(2)**2+apposv(3)**2)
    ersw(1,:)=apposv(1:3)/rsat
!
! Unit vector ES (alongtrack "S")
! -------------------------------
    vvsat=apposv(4)**2+apposv(5)**2+apposv(6)**2
    vrsat=DOT_PRODUCT(apposv(4:6),ersw(1,:))
    ersw(2,:)=(apposv(4:6)-vrsat*ersw(1,:))/DSQRT(vvsat-vrsat**2)
!
! Unit vector EW (crosstrack "W")
! -------------------------------
    CALL vprod(ersw(1,:),ersw(2,:),ersw(3,:))
!
! Attitude rotation matrix
! ------------------------
    attit(:,1)=ersw(2,:)
    attit(:,2)=-ersw(3,:)
    attit(:,3)=-ersw(1,:)

! Special definition of GRAS-METOP satellite fixed body system
    hlp=attit
    attit(:,1)=-hlp(:,2)
    attit(:,2)=-hlp(:,1)
    attit(:,3)=-hlp(:,3)
!
! If attitude flag not defined
! =============================
  CASE DEFAULT

    GOTO 910
  END SELECT

! Check the external attitude
  IF (irc==0 .AND. iattchk==1) THEN
     extAtt = 1
    DO ICOL=1,3
      anglec=DOT_PRODUCT(attitA(1:3,ICOL),attit(1:3,ICOL))
      angle=DACOS(DMIN1(DABS(anglec),1D0))*180.D0/PI
      IF (leosvn /= 914 .AND. leosvn /= 915 .AND. leosvn /= 927) THEN
        IF (angle.GE.MaxAngl .AND. intPol == 1 ) THEN
          IF (eposav .NE. epo) THEN
            WRITE(LFNPRT,'(A56,/,17X,A57,F15.6,/,2(17X,A16,F14.7,/))')          &
                 '### SR ATTITUDE: Difference to a priori attitude too big',      &
                 'A priori attitude used instead of extern. attitude EPOCH=',epo, &
                 'Differ. in deg= ',angle,                                        &
                 'Max Difference= ',MaxAngl
            eposav=epo
          ENDIF
          extAtt = 0
        END IF
      ENDIF
    END DO
! Use external attitude only if okay
! ===========================
    IF (extAtt == 1 .AND. intPol == 1) THEN
      attit(1:3,:)=attitA(1:3,:)
    ENDIF
  END IF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Direction of rotation
! =====================
905 IF (ityp==1) THEN
      attit=transpose(attit)
    END IF

907 RETURN
!
! Errors
! ------
910 WRITE(LFNERR,"(/,'*** SR ATTITUDE: No attitude model for the &
                   &requested flag:',I6)") attflag

  CALL EXITRC(2)

  END SUBROUTINE attitude

END MODULE
