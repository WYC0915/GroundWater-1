MODULE s_PRIKIN
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE prikin(mxclcq,iPart,nStat,iCentr,stName,xStat,xStEll,xStEcc,     &
                  aell,bell,dxell,drell,scell,clkhed,clkrec,nepObs,parflg,  &
                  jPar,locq,rms,anor,xxx0,xxx)

! -------------------------------------------------------------------------
! Purpose:    Prints kinematic coordinate results in GPSEST
!
! Author:     R. Dach, D. Svehla
!
! Created:    23-Jan-2002
! Last mod.:  29-Jan-2009
!
! Changes:    26-Jun-2002 DS: Print LEO kinematic orbit
!             27-Jun-2002 DS: Print plane kinematic positions
!             23-Aug-2002 DS: LEO kin. coord. &  var/cov in true system
!             18-Sep-2002 DS: Kinematics for Airplane, Ship, Ground station
!             20-Sep-2002 DS: Print number of satellites
!             07-Oct-2002 DS: Use standard orbit to get LEO a priori orbit
!                             when KININP in missing
!             07-OCT-2002 DS: Use 1D20 when singular coord.
!             08-Mar-2003 HU: Interface for sr attitude used
!             19-Jun-2003 RD: Add number of observations to output
!                             Singular parameters from "main neq"
!             23-Jun-2003 HB: Interface for SR staFlg
!             24-Jun-2003 RD: Get apriori if no KININP available for the sta.
!             26-Jun-2003 RD: New formatted output for "non-LEOs"
!             29-Sep-2004 AJ: Bugs fixed for ISYSOUT=1
!             11-Jun-2006 HB: Get a priori values from scratch-file (pre-el)
!             29-Jun-2006 HB: ... or from xxx0-array
!             29-Jan-2009 RD: Correct singularity handling for EVERY-EPOCH
!
! SR used:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_const,  ONLY: pi
  USE d_stacrx, ONLY: MTypeSPACE
  USE d_clkrnx, ONLY: t_clkhead,t_clkrec
  USE f_ikf
  USE s_cootra
  USE s_attitude
  USE s_err3d
  USE s_truearth
  USE s_staflg
  USE s_readvel
  USE s_readkin
  USE s_gtleoco
  USE s_leoprn
  USE s_vprod
  USE s_radgms
  USE s_gtflna
  USE s_xyzell
  IMPLICIT NONE
!
! Global dimensions
! -----------------

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                     :: mxclcq ! size of locq
  INTEGER(i4b)                     :: iPart  ! Printing part number (1/2)
  INTEGER(i4b)                     :: nStat  ! Number of stations
  INTEGER(i4b),     DIMENSION(*)   :: iCentr ! (internal) station numbers
  CHARACTER(LEN=*), DIMENSION(*)   :: stName ! List of station names
  REAL(r8b),        DIMENSION(3,*) :: xStat  ! XYZ coordinates of the stations
  REAL(r8b),        DIMENSION(3,*) :: xStEll ! Ell. coordinates of the stations
  REAL(r8b),        DIMENSION(3,*) :: xStEcc ! Station eccentricities
  REAL(r8b)                        :: aell   ! Semimajor axes of ellipsoid
  REAL(r8b)                        :: bell   ! Minor axes of ellipsoid
  REAL(r8b),        DIMENSION(3)   :: dxell  ! Shift of datum to wgs-84
  REAL(r8b),        DIMENSION(3)   :: drell  ! Rotations to wgs-84
  REAL(r8b)                        :: scell  ! Scale factor to wgs-84
  TYPE(T_CLKHEAD)                  :: CLKHED
  TYPE(T_CLKREC)                   :: CLKREC
  INTEGER(i4b)                     :: nEpObs ! Min # of obs. for epoch param.s
  INTEGER(i4b),     DIMENSION(*)   :: parFlg ! Flag for singular par.
  INTEGER(i4b)                     :: jPar   ! Actual parameter number
                                             ! jPar==0: only 1 epoch in Anor,
                                             !    and xxx (from RESEPO)
  INTEGER(i4b),DIMENSION(mxclcq,*) :: locq   ! Parameter characterization
  REAL(r8b)                        :: rms    ! a posteriori rms
  REAL(r8b),        DIMENSION(*)   :: Anor   ! COV matrix
  REAL(r8b),        DIMENSION(*)   :: XXX0   ! A priori values
  REAL(r8b),        DIMENSION(*)   :: XXX    ! solution vector
!  INTEGER(i4b)                     :: ISYSOUT ! Flag for coordinate system
!                                             !  to display LEO coordinates
!                                             !  =0 earth-fixed - cartesian
!                                             !  =1 spacecraft body system

! output:

! Local Variables
! ---------------
  CHARACTER(LEN=fileNameLength),SAVE :: filscr,FILSTD,FILVEL,filkin
  CHARACTER(LEN=20)                :: MarTyp
  CHARACTER(LEN=1)                 :: vpo,vlo,chr1
!
  INTEGER(i4b)                     :: iPar
  INTEGER(i4b), DIMENSION(2), SAVE :: istold = (/ -1,-1 /)
  INTEGER(i4b)                     :: if1st,ip1st
  INTEGER(i4b)                     :: iSta
  INTEGER(i4b)                     :: iEpo
  INTEGER(i4b)                     :: iStat
  INTEGER(i4b)                     :: iFlag
  INTEGER(i4b)                     :: iCrd1,iCrd2
  INTEGER(i4b)                     :: ip1o,ip2o,l1o,l2o
  INTEGER(i4b)                     :: i0,k0,i1,k1
  INTEGER(i4b)                     :: ik00,ik01,ik10,ik11
  INTEGER(i4b)                     :: irc,ircScr,i,ii,kk,IRCORB2,IRCVEL2
  INTEGER(i4b)                     :: iSingul
  INTEGER(i4b),SAVE                :: IRCORB,IRCVEL
  INTEGER(i4b)                     :: IORSYS,leosvn,j
  INTEGER(i4b)                     :: ISYSOUT,nObs
  INTEGER(i4b),SAVE                :: IRCKIN
!
  REAL(r8b),    DIMENSION(3)       :: xPart,xPell,xApr,rmsKin,DXtrue,xAprEll,xAprT
  REAL(r8b),    DIMENSION(3), SAVE :: xAprEl0
  REAL(r8b),    DIMENSION(3,3)     :: covm1,covm2,ROT,QXX,QXXT,attit,QXXL
  REAL(r8b),    DIMENSION(9)       :: XLEO,XLEOT
  REAL(r8b)                        :: phi,xlong,hh,dsec,TOBS
  REAL(r8b)                        :: dPhi,dLon,dH
  REAL(r8b)                        :: delPhi,delLon,delH
  REAL(r8b),    SAVE               :: dPhi1,dLon1,dH1
  REAL(r8b)                        :: sigPhi,sigLon,sigHgt
  REAL(r8b)                        :: sigPhr,sigLor,sigHgr
  REAL(r8b)                        :: radius
  REAL(r8b)                        :: p3o,xl3o,hho,SZ,XPOL,YPOL,UT1GPS
  REAL(r8b),    DIMENSION(6),SAVE  :: Atmp
  REAL(r8b)                        :: rsat,vvsat,vrsat
  REAL(r8b),DIMENSION(3,3)         :: ersw

  LOGICAL,      SAVE               :: first= .TRUE.

! First call
! ----------
  ISYSOUT=0
  IF (ISYSOUT==1 .AND. first) THEN
    CALL GTFLNA(0,'LEOSTD ',FILSTD,IRCORB)
    CALL GTFLNA(0,'KINVEL ',FILVEL,IRCVEL)
  END IF
!
  iPar = jPar
  IF (iPar == 0) iPar = 1

! Return if nothing to do (X-component of kin. coord)
! -----------------------
  IF(locq(1,iPar) /= 21 .OR. locq(3,iPar) /= 1) RETURN

! Set some variables
! ------------------
  iSta=locq(2,iPar)
  iEpo=locq(4,iPar)
  nObs=locq(6,iPar)

! Check the other coordinate components
! -------------------------------------
  IF (locq(1,iPar+1) /=   21 .OR. locq(1,iPar+2) /=   21 .OR. &
      locq(2,iPar+1) /= iSta .OR. locq(2,iPar+2) /= iSta .OR. &
      locq(3,iPar+1) /=    2 .OR. locq(3,iPar+2) /=    3 .OR. &
      locq(4,iPar+1) /= iEpo .OR. locq(4,iPar+2) /= iEpo) THEN
    WRITE(lfnerr,'(/,A,2(/,16X,A),/,16X,A,I10,/)')                 &
    ' ### SR PRIKIN: A kinematic coordinate component is missing.',&
                    '(may be due problems inverting the NEQ).',    &
                    'Station name:  '//TRIM(stName(iSta)),         &
                    'Epoch number:  ',iepo
    RETURN
  ENDIF

! Loop all stations
! -----------------
  iStaLoop: DO iStat=1,nStat
    IF(icentr(iStat) /= iSta) CYCLE iStaLoop

! Time of epoch
! -------------
    dsec=DBLE(iEpo-1)*CLKREC%EPOCH(1)/CLKREC%NEPO
    TOBS=CLKHED%TFIRST+dsec/86400.D0

! Kinematic type
! --------------
    CALL staflg(stName(iStat),TOBS,iFlag,MarTyp)
    IF (first) THEN
      first = .FALSE.
      CALL GTFLNA(0,'KINSCR ',filscr,ircScr)
      CALL GTFLNA(0,'KININP ',filkin,IRCKIN)
    END IF

! Is the request singular?
! ------------------------
    iSingul = 0
    IF (jPar == 0) THEN
      IF (XXX(1) == 1D20) iSingul = 1
    ELSE IF (parFlg(iPar  ) == 1 .OR.  &
             parFlg(iPar+1) == 1 .OR.  &
             parFlg(iPar+2) == 1) THEN
      iSingul = 1
    ENDIF

! Estimated kinematic coordinates: LEO, AIRPLANE
! ----------------------------------------------
    IF (iSingul == 0) THEN
      irc = 2
      IF (IRCKIN==0) &
        CALL  readkin(filscr,stName(iStat),TOBS,1,0,xApr,irc)
      IF (irc /= 0) THEN
        XAPR(1:3)=XXX0(1:3)
      END IF
      xPart(1:3)=xApr(1:3)+XXX(iPar:iPar+2)

! RMS of kinematic solution: LEO, AIRPLANE
! ----------------------------------------
      DO i=1,3
        ii=IKF(i+iPar-1,i+iPar-1)
        rmsKin(i)=rms*DSQRT(Anor(ii))
      END DO

! Write an empty line between stations
! ------------------------------------
      IF (iStOld(iPart) /= -1 .AND. iStOld(iPart) /= iStat) &
        WRITE(lfnprt,*)

! Write kinematic estimates to the GPSEST output file
! ---------------------------------------------------
      IF (MarTyp == MTypeSPACE) THEN
        iStOld(iPart) = iStat
        IF (ISYSOUT==0) THEN
          WRITE (lfnprt,'(I6,1X,F14.6,I6,1X,A16,1X,                    &
          & 3(F10.3," +- ",F7.3),3X,A19)')                             &
          & iEpo,TOBS,nObs,stName(iStat),                              &
          & (XXX(iPar+kk-1),rmsKin(kk),kk=1,3),'LEO Earth-fixed XYZ'
        ELSE
!
! LEO coord and var/cov in True system
! ------------------------------------
! GET LEO ORBIT SYSTEM: IORSYS
!            CALL GETORB(SATNUM(1,1),0,0,1,TOBS,ICRARC,IORSYS,
!     1                          XDUMMY,DUMMY,XDUMMY,IRCGTO)
! GET: SZ,XPOL,YPOL
          IORSYS=2
          XLEO(1:9)=0.0D0
          CALL COOTRA(IORSYS,0,TOBS,XLEO,SZ,XPOL,YPOL,UT1GPS)
! Get velocity
          IRCVEL2=1
          IRCORB2=1
          IF (IRCVEL==0) THEN
            CALL READVEL(FILVEL,stName(iStat),TOBS,1,0,XLEO(4:6),IRCVEL2)
            XLEO(1:3)=xPart(1:3)
            CALL TRUEARTH(XLEO,XLEOT,1,1,SZ,XPOL,YPOL)
          END IF
          IF (IRCORB==0 .AND. IRCVEL2/=0) THEN
            CALL GTLEOCO(stName(iStat),TOBS,1,1,XLEO,SZ,XPOL,YPOL,IRCORB2)
            XLEOT(4:6)=XLEO(4:6)
            XLEO(1:3)=xPart(1:3)
            CALL TRUEARTH(XLEO,XLEOT,1,0,SZ,XPOL,YPOL)
          END IF
          CALL TRUEARTH(xApr,xAprT,1,0,SZ,XPOL,YPOL)

          IF (IRCVEL2/=0 .AND. IRCORB2/=0) XLEOT(6)=1.D20

! TRANSFORMATION FROM EARTH-FIXED TO TRUE SYSTEM OF EPOCH
          CALL LEOPRN(stName(iStat),TOBS,leosvn)
!!          CALL ATTITUDE(leosvn,TOBS,XLEOT,1,attit)
!!          DXtrue=matmul(attit,XXX(iPar:iPar+2))
          rsat=DSQRT(XLEOT(1)**2+XLEOT(2)**2+XLEOT(3)**2)
          ersw(1,:)=XLEOT(1:3)/rsat
          vvsat=XLEOT(4)**2+XLEOT(5)**2+XLEOT(6)**2
          vrsat=DOT_PRODUCT(XLEOT(4:6),ersw(1,:))
          ersw(2,:)=(XLEOT(4:6)-vrsat*ersw(1,:))/DSQRT(vvsat-vrsat**2)
          CALL vprod(ersw(1,:),ersw(2,:),ersw(3,:))
          attit(:,1)=ersw(1,:)
          attit(:,2)=ersw(2,:)
          attit(:,3)=ersw(3,:)
          attit=transpose(attit)
!
          DXtrue=matmul(attit,XLEOT(1:3)-xAprT(1:3))
!
          ROT(1,1)=DCOS(SZ)
          ROT(2,1)=DSIN(SZ)
          ROT(3,1)=XPOL
          ROT(1,2)=-ROT(2,1)
          ROT(2,2)=ROT(1,1)
          ROT(3,2)=-YPOL
          ROT(1,3)=-DCOS(SZ)*XPOL-DSIN(SZ)*YPOL
          ROT(2,3)=-DSIN(SZ)*XPOL+DCOS(SZ)*YPOL
          ROT(3,3)=1.D0
!
          DO i=1,3
            DO j=1,3
              ii=IKF(i+iPar-1,j+iPar-1)
              QXX(i,j)=Anor(ii)
            END DO
          END DO
!
          QXXT=MATMUL(MATMUL(ROT,QXX(1:3,1:3)),TRANSPOSE(ROT))
          QXXL=MATMUL(MATMUL(attit,QXXT(1:3,1:3)),TRANSPOSE(attit))
          rmsKin(1)=rms*DSQRT(QXXL(1,1))
          rmsKin(2)=rms*DSQRT(QXXL(2,2))
          rmsKin(3)=rms*DSQRT(QXXL(3,3))
!            QXXE(4)=QXXT(2,1)
!            QXXE(5)=QXXT(3,1)
!            QXXE(6)=QXXT(3,2)
!
          WRITE (lfnprt,'(I6,1X,F14.6,I6,1X,A16,1X,                     &
             &      3(F10.3," +- ",F8.4),3X,A16)')                      &
             &      iEpo,TOBS,nObs,stName(iStat),                       &
             &      (DXtrue(kk),rmsKin(kk),kk=1,3),'RSW             '
!
        END IF

      ! Output for "non-LEOs"
      ELSE IF (1==1) THEN

! Transform the ell. coordinates
! ------------------------------
        CALL xyzell(aell,bell,dxell,drell,scell,xApr,xAprEll)
        CALL radgms(1,xAprEll(1),vpo,ip1o,ip2o,p3o)
        CALL radgms(1,xAprEll(2),vlo,l1o,l2o,xl3o)
        hho = xAprEll(3)

! First call for this station in the part (print coordinates)
! ---------------------------------------
        IF (iStOld(iPart) /= iStat) THEN
          iStOld(iPart) = iStat
          WRITE(lfnprt,'(8X,A16,10X, 2(A1,2I3,F10.6,2X), F15.4)') &
                stName(iStat),vpo,ip1o,ip2o,p3o, vlo,l1o,l2o,xl3o, hho
          xAprEl0 = xAprEll
        ENDIF

! Get the actual epoch solution
! -----------------------------
        CALL xyzell(aell,bell,dxell,drell,scell,xPart,xPell)
        phi   = xPell(1)
        xlong = xPell(2)
        hh    = xPell(3)

! Copy the COV-information for this epoch
! ---------------------------------------
        DO iCrd1 = 1,3
          DO iCrd2 = 1,3
            covm1(iCrd1,iCrd2) = rms**2*Anor(IKF(iCrd1+iPar-1,iCrd2+iPar-1))
          ENDDO
        ENDDO
        CALL err3d(phi,xlong,hh,aell,bell,-1,covm1,covm2)

! Compute the estimated coordinate differences in NEU
! ---------------------------------------------------
        radius = DSQRT(xPart(1)**2+xPart(2)**2+xPart(3)**2)

        dPhi   = radius*(phi-xAprEll(1))
        dLon   = xlong-xAprEll(2)
        IF (dLon  >  pi) dLon = dLon-2*pi
        IF (dLon  < -PI) dLon = dLon+2*pi
        dLon   = radius*DCOS(phi)*dLon

        dH     = hh-xAprEll(3)

! Get the sigmas for the coord. diff. in NEU
! ------------------------------------------
        sigPhi = radius*DSQRT(covm2(1,1))
        sigLon = radius*DCOS(phi)*DSQRT(covm2(2,2))
        sigHgt = DSQRT(covm2(3,3))

! Compute the total coordinate differences in NEU
! -----------------------------------------------
        delPhi   = radius*(phi-xAprEl0(1))
        delLon   = xlong-xAprEl0(2)
        IF (delLon >  pi) delLon = delLon-2*pi
        IF (delLon < -PI) delLon = delLon+2*pi
        delLon   = radius*DCOS(phi)*delLon

        delH     = hh-xAprEl0(3)

! Indicate too few observations
! -----------------------------
         chr1 = ' '
         IF (nObs < nepobs) chr1 = '*'

         WRITE(lfnprt,'(I5,1X,F14.6,I5,A2,1X,A4,3(F10.4,A,F5.3),1X,3F14.4)') &
               iEpo,TOBS,nObs,chr1,stName(iStat)(1:4),                       &
               dPhi,' +- ',sigPhi, dLon,' +- ',sigLon, dh,' +- ',sigHgt,     &
               delPhi, delLon, delH
!

      ! Old output for "non-LEOs"
      ELSE

! Transform the ell. coordinates
! ------------------------------
        CALL xyzell(aell,bell,dxell,drell,scell,xApr,xAprEll)
        CALL radgms(1,xAprEll(1),vpo,ip1o,ip2o,p3o)
        CALL radgms(1,xAprEll(2),vlo,l1o,l2o,xl3o)
        hho = xAprEll(3)

! First call for this station in the part (print coordinates)
! ---------------------------------------
        IF (iStOld(iPart) /= iStat) THEN
          ip1st = jPar
          if1st = 1
          iStOld(iPart) = iStat
          WRITE(lfnprt,'(/,7X,A16,1X, 2(A1,2I3,F10.6,1X), 1X,F15.4)') &
                stName(iStat),vpo,ip1o,ip2o,p3o, vlo,l1o,l2o,xl3o, hho
        ELSE
          if1st = 0
        ENDIF

! Get the actual epoch solution
! -----------------------------
        CALL xyzell(aell,bell,dxell,drell,scell,xPart,xPell)
        phi   = xPell(1)
        xlong = xPell(2)
        hh    = xPell(3)

! Copy the COV-information for this epoch
! ---------------------------------------
        DO iCrd1 = 1,3
          DO iCrd2 = 1,3
            covm1(iCrd1,iCrd2) = rms**2*Anor(IKF(iCrd1+iPar-1,iCrd2+iPar-1))
          ENDDO
        ENDDO
        CALL err3d(phi,xlong,hh,aell,bell,-1,covm1,covm2)

! Compute the coordinate differences in NEU
! -----------------------------------------
        radius = DSQRT(xPart(1)**2+xPart(2)**2+xPart(3)**2)

        dPhi   = radius*(phi-xAprEll(1))
        dLon   = xlong-xAprEll(2)
        IF (dLon  >  pi) dLon = dLon-2*pi
        IF (dLon  < -PI) dLon = dLon+2*pi
        dLon   = radius*DCOS(phi)*dLon

        dH     = hh-hho

! Get the sigmas for the coord. diff. in NEU
! ------------------------------------------
        sigPhi = radius*DSQRT(covm2(1,1))
        sigLon = radius*DCOS(phi)*DSQRT(covm2(2,2))
        sigHgt = DSQRT(covm2(3,3))

! Indicate too few observations
! -----------------------------
        chr1 = ' '
        IF (nObs < nepobs) chr1 = '*'

! Write the line for the first epoch
! ----------------------------------
        IF (if1st == 1) THEN
          dPhi1=dPhi
          dLon1=dLon
          dH1  =dH
          WRITE(lfnprt,'(I6,16X,3(F10.4,A,F5.3))')       &
                iEpo,                                    &
                dPhi,' +- ',sigPhi, dLon,' +- ',sigLon, dh,' +- ',sigHgt
          IF (jPar == 0) Atmp(:) = Anor(1:6)
        ELSE
!
! rms of the coordinates relative to the first epoch
          IF (ip1st > 0) THEN
            DO iCrd1 = 1,3
              DO iCrd2 = 1,3
                i0   = ip1st-1 + iCrd1
                k0   = ip1st-1 + iCrd2
                i1   = iPar -1 + iCrd1
                k1   = iPar -1 + iCrd2

                ik00 = IKF(i0,k0)
                ik01 = IKF(i0,k1)
                ik10 = IKF(i1,k0)
                ik11 = IKF(i1,k1)

                covm1(iCrd1,iCrd2) = (rms**2) * &
                        (Anor(ik00)-Anor(ik10)-Anor(ik01)+Anor(ik11))
              ENDDO
            ENDDO
          ELSE
            DO iCrd1 = 1,3
              DO iCrd2 = 1,3
                covm1(iCrd1,iCrd2) = (rms**2) * &
                        (Anor(IKF(iCrd1,iCrd2))+ Atmp(IKF(iCrd1,iCrd2)))
              ENDDO
            ENDDO
          ENDIF

          CALL ERR3D(phi,xlong,hh,aell,bell,-1,covm1,covm2)

          sigPhr=radius*DSQRT(covm2(1,1))
          sigLor=radius*DCOS(phi)*DSQRT(covm2(2,2))
          sigHgr=DSQRT(covm2(3,3))

          WRITE(lfnprt,'(I6,16X,6(F10.4,A,F5.3))')                           &
          iEpo,                                                                    &
          dPhi,      ' +- ',sigPhi, dLon,      ' +- ',sigLon, dH,    ' +- ',sigHgt,&
          dPhi-dPhi1,' +- ',sigPhr, dLon-dLon1,' +- ',sigLor, dH-dH1,' +- ',sigHgr
        ENDIF
      END IF
    ELSE
      IF (MarTyp == MTypeSPACE) THEN
        WRITE(lfnprt,'(I6,1X,F14.6,I6,1X,A16,1X,1X,A22)') &
              iEpo,TOBS,nObs,stName(iStat),'### SINGULAR EPOCH ###'
        iStOld(iPart) = iStat
      ELSE
        WRITE(lfnprt,'(I5,1X,F14.6,I5,3X,A4,4X,A22)') &
              iEpo,TOBS,nObs,stName(iStat)(1:4),'### SINGULAR EPOCH ###'
      ENDIF
    END IF
  ENDDO iStaLoop

  RETURN
END SUBROUTINE prikin

END MODULE
