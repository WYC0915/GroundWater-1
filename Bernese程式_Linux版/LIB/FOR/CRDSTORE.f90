MODULE s_CRDSTORE
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE crdstore(neq)

! -------------------------------------------------------------------------
! Purpose:    This subroutine prints coordinates and velocities and
!             writes the coordinate and velocity output files
!
! Author:     M.Meindl
!
! Created:    15-Dec-2003
!
! Changes:    10-Jun-2003 mm: Minor changes
!             26-Jun-2003 mm: Bugfix wrt Qxx
!             07-Jul-2003 mm: Small changes (flag in result file)
!             15-Dec-2003 mm: Headerline corrected
!             09-Feb-2003 mm: Skip stations with singular coordinates
!             24-Nov-2004 mm: Bugfix wrt singular coordinates
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             22-Jul-2007 AG: If nsta = 0 dummy call to wtstat and wtvelo
!             13-Feb-2008 RD: Adapt to par%name == chr*20
!             28-Jul-2009 DT: Merge Local ties to BERN version
!             27-Sep-2010 RD: Corrected all of TIMST2
!             03-Dez-2010 MM: Include GNSS-specific parameters
!             20-Jan-2011 RD: Correct output for small negative "gms"
!             20-Jan-2011 SS/MM: Bugfix concerning latest bugfix
!             03-Feb-2011 SL: print 5 digits instead of 4, use m_bern with ONLY
!             08-Feb-2011 RD: New call of WTSTAT
!             08-Feb-2011 RD: WTSTAT replaces WTVELO
!             10-Feb-2011 MM: Do not display GSP title if no GSPs estimated
!             15-Feb-2011 SL: lon/lat in decimal notation
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b,r8b,lfnPrt,lfnErr
  USE m_global, ONLY: g_strSys3
  USE d_const,  ONLY: PI
  USE d_datum,  ONLY: datum
  USE d_neq,    ONLY: t_neq
  USE p_addneq, ONLY: comstat,opt,staInfo
  USE d_const,  ONLY: filTitle

  USE f_ikf
  USE s_alcerr
  USE s_covdia
  USE s_err3d
  USE s_timst2
  USE f_gtweight
  USE s_eccell
  USE s_wtstat
  USE s_radgms
  USE s_xyzell

  IMPLICIT NONE

! List of parameters
! ------------------
  TYPE(t_neq)                        :: neq

! Local variables
! ---------------
! misc
  CHARACTER(LEN=20)                            :: oldNam
  CHARACTER(LEN=19)                            :: tStr
  INTEGER(i4b)                                 :: ik
  INTEGER(i4b)                                 :: nSta, iSta
  INTEGER(i4b)                                 :: nGsp
  INTEGER(i4b)                                 :: iac
  LOGICAL                                      :: velo

! loop variables
  INTEGER(i4b)                                 :: iPar, iPar1, iPar2
  INTEGER(i4b)                                 :: iCmp, iCov, iSys
  INTEGER(i4b)                                 :: ii, jj

! arrays for wtstat, wtstat
  CHARACTER(LEN=16),DIMENSION(:),  ALLOCATABLE :: staNam
  CHARACTER(LEN=16)                            :: datumtemp = ' '
  CHARACTER(LEN=4), DIMENSION(:),  ALLOCATABLE :: plate
  CHARACTER(LEN=1), DIMENSION(:),  ALLOCATABLE :: staFlg
  INTEGER(i4b),     DIMENSION(:),  ALLOCATABLE :: staNum
  REAL(r8b),        DIMENSION(:,:),ALLOCATABLE :: xStat, xVelo

! vectors, matrices
  REAL(r8b),DIMENSION(3)                       :: xStEll, xAprEll
  REAL(r8b),DIMENSION(3)                       :: xApr , xEst
  REAL(r8b),DIMENSION(3)                       :: xApr1, xEst1
  REAL(r8b),DIMENSION(3)                       :: xApr2, xEst2
  REAL(r8b),DIMENSION(3)                       :: xAprM, xEstM
  REAL(r8b),DIMENSION(6)                       :: xvApr, xvEst
  REAL(r8b),DIMENSION(3)                       :: xAmEll, xEmEll
  REAL(r8b),DIMENSION(3)                       :: vApr, vEst
  REAL(r8b),DIMENSION(3)                       :: vAEll, vEELL
  REAL(r8b),DIMENSION(3)                       :: xCor, xCorEll
  REAL(r8b),DIMENSION(3)                       :: xRms, xRmsEll
  REAL(r8b),DIMENSION(3)                       :: vCor, vCorEll
  REAL(r8b),DIMENSION(3)                       :: vRms, vRmsEll
  REAL(r8b),DIMENSION(3,3)                     :: xCov, xCovEll
  REAL(r8b),DIMENSION(3,3)                     :: xCov1, xCov2, xCov12, vCov
  REAL(r8b),DIMENSION(3,3)                     :: xCovM, xCmEll, vCEll
  REAL(r8b),DIMENSION(6,6)                     :: Qxx, Mxx, hlpCov, xMvCov

! time
  REAL(r8b)                                    :: dT, dTm

! transformations
  REAL(r8b)                                    :: radius, cosPhi
  CHARACTER(LEN=1)                             :: v0, vv0
  INTEGER(i4b)                                 :: i1, i2, ii1, ii2
  REAL(r8b)                                    :: x3, xx3
  REAL(r8b)                                    :: alpha
  REAL(r8b),DIMENSION(3)                       :: rEuler, diag
  REAL(r8b),DIMENSION(2)                       :: diag2

! local ties
!!!  REAL(r8b),DIMENSION(3)                       :: tie
  INTEGER(i4b)                                 :: ntie

! Count number of stations
! ------------------------
  nSta = 0
  nGsp = 0
  DO ipar1 = 1,neq%misc%npar
    IF (neq%par(ipar1)%locq(1)==1 .AND.                                     &
        neq%par(ipar1)%locq(3)==1 .AND.                                     &
        neq%par(ipar1)%locq(4)==1      ) nSta = nSta+1
    IF (neq%par(ipar1)%locq(1)==30     ) nGsp = nGsp+1
  END DO

! Return if no stations are available
! -----------------------------------
  IF (nSta==0) THEN
    nSta = 1
    ALLOCATE(staNam(nSta),stat=iac)
    CALL alcerr(iac,'staNam',(/nSta/),'crdstore')
    ALLOCATE(staNum(nSta),stat=iac)
    CALL alcerr(iac,'staNum',(/nSta/),'crdstore')
    ALLOCATE(staFlg(nSta),stat=iac)
    CALL alcerr(iac,'staFlg',(/nSta/),'crdstore')
    ALLOCATE(xStat(3,nSta),stat=iac)
    CALL alcerr(iac,'xStat',(/3,nSta/),'crdstore')
    ALLOCATE(plate(nSta),stat=iac)
    CALL alcerr(iac,'plate',(/nSta/),'crdstore')

    IF (opt%coordrs/='')                                     &
      CALL wtstat(1,opt%coordrs,filTitle,datumtemp,          &
                  0,staNam,xStat,staNum,staFlg,opt%timRefCrd)
    IF (opt%velors/='')                                      &
      CALL wtstat(1,opt%velors,filTitle,datumtemp,          &
                  0,staNam,xStat,staNum,staFlg)

    DEALLOCATE(staNam,stat=iac)
    DEALLOCATE(staNum,stat=iac)
    DEALLOCATE(staFlg,stat=iac)
    DEALLOCATE(xStat, stat=iac)
    DEALLOCATE(plate, stat=iac)

    RETURN
  ENDIF

! Allocate arrays
! ---------------
  ALLOCATE(staNam(nSta),stat=iac)
  CALL alcerr(iac,'staNam',(/nSta/),'crdstore')
  ALLOCATE(staNum(nSta),stat=iac)
  CALL alcerr(iac,'staNum',(/nSta/),'crdstore')
  ALLOCATE(staFlg(nSta),stat=iac)
  CALL alcerr(iac,'staFlg',(/nSta/),'crdstore')
  ALLOCATE(xStat(3,nSta),stat=iac)
  CALL alcerr(iac,'xStat',(/3,nSta/),'crdstore')
  ALLOCATE(xVelo(3,nSta),stat=iac)
  CALL alcerr(iac,'xVelo',(/3,nSta/),'crdstore')
  ALLOCATE(plate(nSta),stat=iac)
  CALL alcerr(iac,'plate',(/nSta/),'crdstore')

  xVelo = 0.d0
  plate = ' '
  iSta  = 0

! Get epoch string
! ----------------
  CALL timst2(1,1,opt%timRefCrd,tStr)

! Write title line
! ----------------
  WRITE(lfnPrt,fmt=1) &
    'Station coordinates and velocities:','Reference epoch:',TRIM(tStr), &
    'Station name        ','Typ','A priori value','Estimated value', &
    'Correction','RMS error','3-D ellipsoid','2-D ellipse'
1 FORMAT(//,1X,A,/,1X,34('-'),/,1X,A,1X,A,//, &
         1X,A20,2X,A3,2(2X,A15),2(2X,A12),2(4X,A15),/,1X,132('-'))

! Loop over all coordinates
! -------------------------
  DO iPar1=1,neq%misc%nPar
    IF (neq%par(iPar1)%locq(1)==1 .AND. neq%par(iPar1)%locq(3)==1 .AND.    &
        neq%par(iPar1)%locq(4)/=2) THEN

! Check for complete coordinate set
! ---------------------------------
      IF (neq%par(iPar1+1)%name    /= neq%par(iPar1)%name .OR.             &
          neq%par(iPar1+1)%locq(1) /= 1                   .OR.             &
          neq%par(iPar1+1)%locq(3) /= 2                   .OR.             &
          neq%par(iPar1+1)%locq(4) == 2                   .OR.             &
          neq%par(iPar1+2)%name    /= neq%par(iPar1)%name .OR.             &
          neq%par(iPar1+2)%locq(1) /= 1                   .OR.             &
          neq%par(iPar1+2)%locq(3) /= 3                   .OR.             &
          neq%par(iPar1+2)%locq(4) == 2                       ) THEN

        WRITE(lfnerr,'(/,A,/,18X,A,/)')                                    &
         ' ### SR CRDSTORE: Incomplete station coordinate set.',           &
                           'Station name:  ' // TRIM(neq%par(iPar1)%name)
        CYCLE
      ENDIF

! Skip station with singular coordinates
! --------------------------------------
      IF (neq%aNor(ikf(iPar1,iPar1))     == 0.d0 .OR.                      &
          neq%aNor(ikf(iPar1+1,iPar1+1)) == 0.d0 .OR.                      &
          neq%aNor(ikf(iPar1+2,iPar1+2)) == 0.d0) THEN
            WRITE(lfnerr,'(/,A,/,18X,A,//)')                               &
             ' ### SR CRDSTORE: Singular station coordinate set.',         &
                               'Station name:  '//TRIM(neq%par(iPar1)%name)
            CYCLE
       END IF

! Get station name, number and flag
! ---------------------------------
      iSta         = iSta+1
      staNam(iSta) = TRIM(neq%par(iPar1)%name)
      staNum(iSta) = iSta
      staFlg(iSta) = 'A'
      IF (gtweight(neq%par(iPar1),'A')  /=0.d0 .OR.                        &
          gtweight(neq%par(iPar1+1),'A')/=0.d0 .OR.                        &
          gtweight(neq%par(iPar1+2),'A')/=0.d0     ) staFlg(iSta) = 'W'

! Initialize some variables
! -------------------------
      xApr1 = 0.d0
      xEst1 = 0.d0
      xCov1 = 0.d0
      xApr2 = 0.d0
      xEst2 = 0.d0
      xCov2 = 0.d0
      vApr  = 0.d0
      vEst  = 0.d0
      vCov  = 0.d0
      velo  = .FALSE.

! Station 1: Get coordinates and cov matrix
! -----------------------------------------
      DO iCmp=0,2
        xApr1(iCmp+1) = neq%par(iPar1+iCmp)%x0
        xEst1(iCmp+1) = neq%par(iPar1+iCmp)%x0+neq%xxx(iPar1+iCmp)
        DO iCov=0,2
          ik = ikf(iPar1+iCmp,iPar1+iCov)
          xCov1(iCmp+1,iCov+1) = neq%aNor(ik)
        END DO
      END DO

      xAprM = xApr1
      xEstM = xEst1
      xCovM = xCov1

! Look for second coordinate set
! ------------------------------
      DO iPar2=iPar1,neq%misc%nPar
        IF (neq%par(iPar2)%locq(1)==1 .AND. neq%par(iPar2)%locq(3)==1 .AND.&
            neq%par(iPar2)%name==neq%par(iPar1)%name                  .AND.&
            neq%par(iPar2)%locq(4)==2) THEN

! Check for complete coordinate set
! ---------------------------------
          IF (neq%par(iPar2+1)%name    /= neq%par(iPar2)%name .OR.         &
              neq%par(iPar2+1)%locq(1) /= 1                   .OR.         &
              neq%par(iPar2+1)%locq(3) /= 2                   .OR.         &
              neq%par(iPar2+1)%locq(4) /= 2                   .OR.         &
              neq%par(iPar2+2)%name    /= neq%par(iPar1)%name .OR.         &
              neq%par(iPar2+2)%locq(1) /= 1                   .OR.         &
              neq%par(iPar2+2)%locq(3) /= 3                   .OR.         &
              neq%par(iPar2+2)%locq(4) /= 2                       ) THEN

            WRITE(lfnerr,'(/,A,/,18X,A,/)')                                &
             ' ### SR CRDSTORE: Incomplete station coordinate set.',       &
                               'Station name:  ' // TRIM(neq%par(iPar1)%name)
            CYCLE
          ENDIF

! Station 2: Get coordinates and cov matrix
! -----------------------------------------
          DO iCmp=0,2
            xApr2(iCmp+1) = neq%par(iPar2+iCmp)%x0
            xEst2(iCmp+1) = neq%par(iPar2+iCmp)%x0+neq%xxx(iPar2+iCmp)
            DO iCov=0,2
              ik = ikf(iPar2+iCmp,iPar2+iCov)
              xCov2(iCmp+1,iCov+1) = neq%aNor(ik)
              ik = ikf(iPar1+iCmp,iPar2+iCov)
              xCov12(iCmp+1,iCov+1) = neq%aNor(ik)
            END DO
          END DO

! Compute mean position and velocity
! ----------------------------------
          Qxx(1:3,1:3) = xCov1
          Qxx(1:3,4:6) = xCov12
          Qxx(4:6,1:3) = transpose(xCov12)
          Qxx(4:6,4:6) = xCov2

! time intervals
          dT  = (neq%par(iPar2)%time%mean-neq%par(iPar1)%time%mean)/365.25d0
          dTm = (opt%timRefCrd-neq%par(iPar1)%time%mean)/365.25d0

! transformation matrix
          Mxx = 0.d0
          DO ii=1,3
            Mxx(ii,ii)     = 1-dTm/dT
            Mxx(ii+3,ii)   =    -1/dT
            Mxx(ii,ii+3)   =   dTm/dT
            Mxx(ii+3,ii+3) =     1/dT
          END DO

! transformation
          xvApr  = matmul(Mxx,(/xApr1,xApr2/))
          xvEst  = matmul(Mxx,(/xEst1,xEst2/))
          hlpCov = matmul(Mxx,Qxx)
          xMvCov = matmul(hlpCov,transpose(Mxx))

          xAprM  = xvApr(1:3)
          vApr   = xvApr(4:6)
          xEstM  = xvEst(1:3)
          vEst   = xvEst(4:6)
          xCovM  = xMvCov(1:3,1:3)
          vCov   = xMvCov(4:6,4:6)

          velo = .TRUE.
          EXIT
        END IF

      END DO

! Assign coordinates and velocities
! ---------------------------------
      xStat(:,iSta) = xEstM
      xVelo(:,iSta) = vEst

! Create output (coordinates)
! ---------------------------
! Correction and RMS (XYZ)
      xCor = xEstM-xAprM
      DO jj=1,3
        xRms(jj) = comstat%rms*SQRT(xCovM(jj,jj))
      END DO

! Transformation to NEU
      CALL xyzell(datum%aEll,datum%bEll,datum%dxEll,datum%drEll,           &
                  datum%scEll,xAprM,xAmEll)
      CALL xyzell(datum%aEll,datum%bEll,datum%dxEll,datum%drEll,           &
                  datum%scEll,xEstM,xEmEll)
      CALL err3d(xEmEll(1),xEmEll(2),xEmEll(3),datum%aEll,                 &
                 datum%bEll,-1,comstat%rms**2*xCovM,xCmEll)

! Diagonal transformation of covariance matrix
      CALL covdia(datum%aEll,datum%bEll,datum%dxEll,datum%drEll,           &
                  datum%scEll,xEstM,xCovM,comstat%rms,                     &
                  diag,rEuler,diag2,alpha)

! Correction and RMS (NEU)
      radius = SQRT(xEstM(1)**2+xEstM(2)**2+xEstM(3)**2)
      cosPhi = COS(xEmEll(1))
      xRmsEll(1) = radius*SQRT(xCmEll(1,1))
      xRmsEll(2) = radius*cosPhi*SQRT(xCmEll(2,2))
      xRmsEll(3) = SQRT(xCmEll(3,3))
      xCorEll(1) = radius*(xEmEll(1)-xAmEll(1))
      xCorEll(2) = xEmEll(2)-xAmEll(2)
      ! Modulo 2 PI
      IF (xCorEll(2)> PI) xCorEll(2) = xCorEll(2)-2*PI
      IF (xCorEll(2)<-PI) xCorEll(2) = xCorEll(2)+2*PI
      xCorEll(2) = radius*cosPhi*xCorEll(2)
      xCorEll(3) = xEmEll(3)-xAmEll(3)

! Write output lines (XYZ)
      WRITE(lfnprt,"(1X,A20,2X,A3,2(2X,F15.5),2(2X,F12.5))") &
            neq%par(iPar1)%name, &
        "X  ",xAprM(1),xEstM(1),xCor(1),xRms(1)
      WRITE(lfnprt,"(23X,A3,2(2X,F15.5),2(2X,F12.5))") &
        "Y  ",xAprM(2),xEstM(2),xCor(2),xRms(2)
      WRITE(lfnprt,"(23X,A3,2(2X,F15.5),2(2X,F12.5))") &
        "Z  ",xAprM(3),xEstM(3),xCor(3),xRms(3)

! Write output lines (NEU)
      WRITE(lfnprt,"(/,23X,A3,2(2X,F15.5),2(2X,F12.5),2X,F10.5,2X,F5.1)") &
         "U  ",xAmEll(3),xEmEll(3),xCorEll(3),xRmsEll(3),diag(3),rEuler(2)

      CALL radgms(1,xAmEll(1),v0,i1,i2,x3)
      CALL radgms(1,xEmEll(1),vv0,ii1,ii2,xx3)
      WRITE(lfnprt,"(23X,A3,2(2X,F15.7),2(2X,F12.5),2(2X,F10.5,2X,F5.1))") &
         "N  ",SIGN(i1+(i2+x3/60)/60,xAmEll(1)), &
               SIGN(ii1+(ii2+xx3/60)/60,xEmEll(1)), &
               xCorEll(1),xRmsEll(1),diag(1),rEuler(1),diag2(1),alpha

      CALL radgms(1,xAmEll(2),v0,i1,i2,x3)
      CALL radgms(1,xEmEll(2),vv0,ii1,ii2,xx3)
      WRITE(lfnprt,"(23X,A3,2(2X,F15.7),2(2X,F12.5),2X,F10.5,2X,F5.1,2X,F10.5,/)") &
         "E  ",SIGN(i1+(i2+x3/60)/60,xAmEll(2)), &
               SIGN(ii1+(ii2+xx3/60)/60,xEmEll(2)), &
               xCorEll(2),xRmsEll(2),diag(2),rEuler(3),diag2(2)

! Create output (velocities)
! --------------------------
      IF(.NOT.velo) CYCLE

! Correction and RMS (XYZ)
      vCor = vEst-vApr
      DO jj=1,3
        vRms(jj) = comstat%rms*SQRT(vCov(jj,jj))
      END DO

! Transformation to NEU
      CALL eccell(xAmEll,vApr,vAEll)
      CALL eccell(xEmEll,vEst,vEEll)
      CALL err3d(xEmEll(1),xEmEll(2),xEmEll(3),datum%aEll,                 &
                 datum%bEll,-1,comstat%rms**2*vCov,vCEll)


! Diagonal transformation of covariance matrix
      CALL covdia(datum%aEll,datum%bEll,datum%dxEll,datum%drEll,           &
                  datum%scEll,xEstM,vCov,comstat%rms,                      &
                  diag,rEuler,diag2,alpha)

! Correction and RMS (NEU)
      vCorEll = vEEll-vAEll
      vRmsEll(1) = radius*SQRT(vCEll(1,1))
      vRmsEll(2) = radius*cosPhi*SQRT(vCEll(2,2))
      vRmsEll(3) = SQRT(vCEll(3,3))

! Write output lines (XYZ)
      WRITE(lfnprt,"(1X,A20,2X,A3,2(2X,F15.5),2(2X,F12.5))") &
            neq%par(iPar1)%name, &
        "VX ",vApr(1),vEst(1),vCor(1),vRms(1)
      WRITE(lfnprt,"(23X,A3,2(2X,F15.5),2(2X,F12.5))") &
        "VY ",vApr(2),vEst(2),vCor(2),vRms(2)
      WRITE(lfnprt,"(23X,A3,2(2X,F15.5),2(2X,F12.5))") &
        "VZ ",vApr(3),vEst(3),vCor(3),vRms(3)

! Write output lines (NEU)
      WRITE(lfnprt,"(/,23X,A3,2(2X,F15.5),2(2X,F12.5),2X,F10.5,2X,F5.1)") &
         "VU ",vAEll(3),vEEll(3),vCorEll(3),vRmsEll(3),diag(3),rEuler(2)
      WRITE(lfnprt,"(23X,A3,2(2X,F15.5),2(2X,F12.5),2(2X,F10.5,2X,F5.1))") &
         "VN ",vAEll(1),vEEll(1),vCorEll(1),vRmsEll(1),diag(1),rEuler(1), &
         diag2(1),alpha
      WRITE(lfnprt,"(23X,A3,2(2X,F15.5),2(2X,F12.5),2X,F10.5,2X,F5.1,2X,F10.5,/)") &
         "VE ",vAEll(2),vEEll(2),vCorEll(2),vRmsEll(2),diag(2),rEuler(3), &
         diag2(2)
    END IF
  END DO

  nSta = iSta

! GNSS-specific parameters
! ------------------------
  IF (nGsp > 0) THEN

! Write title line
    WRITE(lfnprt,"(//,A,/,A/)") " GNSS-specific parameters:",     &
                                " ------------------------"

    WRITE(lfnprt,"(2A,A)") " Station name         Sys Typ  A priori value", &
                           "    Estimated value  Correction   RMS error  "
    WRITE(lfnprt,"(3A44)") " -------------------------------------------",  &
                           "--------------------------------------------",  &
                           "--------------------------------------------"

! Loop over all parameters
! ------------------------
    oldNam = ""
    DO iPar=1,neq%misc%nPar
      IF (neq%par(iPar)%locq(1)/=30) CYCLE
      IF (neq%par(iPar)%locq(3)==1) THEN

! Check for complete parameter set
        IF (neq%par(iPar+1)%name    /= neq%par(iPar)%name    .OR.      &
            neq%par(iPar+1)%locq(4) /= neq%par(iPar)%locq(4) .OR.      &
            neq%par(iPar+1)%locq(1) /= 30                    .OR.      &
            neq%par(iPar+1)%locq(3) /= 2                     .OR.      &
            neq%par(iPar+2)%name    /= neq%par(iPar)%name    .OR.      &
            neq%par(iPar+2)%locq(4) /= neq%par(iPar)%locq(4) .OR.      &
            neq%par(iPar+2)%locq(1) /= 30                    .OR.      &
            neq%par(iPar+2)%locq(3) /= 3                          ) THEN
          WRITE(lfnerr,'(/,A,/,18X,A,/)')                                    &
       ' ### SR CRDSTORE: Incomplete set of GNSS-specific station translations.',&
                         'Station name:  '//TRIM(neq%par(iPar)%name)
          CYCLE
        ENDIF

! Skip station with singular coordinates
        IF (neq%aNor(ikf(iPar,iPar))     == 0.d0 .OR.                      &
            neq%aNor(ikf(iPar+1,iPar+1)) == 0.d0 .OR.                      &
            neq%aNor(ikf(iPar+2,iPar+2)) == 0.d0) THEN
          WRITE(lfnerr,'(/,A,/,18X,A,//)')                               &
       ' ### SR CRDSTORE: Singular set of GNSS-specific station translations.', &
                         'Station name:  '//TRIM(neq%par(iPar)%name)
          CYCLE
        END IF

! Initialize some variables
        oldNam = neq%par(iPar)%name
        xApr = 0.d0
        xEst = 0.d0
        xCov = 0.d0

! Find corresponding station
        iSta = 0
        DO ii=1,nSta
          IF (TRIM(staNam(ii))==TRIM(neq%par(iPar)%name)) THEN
            iSta = ii
            EXIT
          ENDIF
        ENDDO
        IF (iSta == 0) THEN
          WRITE(lfnerr,'(/,A,/,2A,/)')                          &
         ' ### SR CRDSTORE: No corresponding station found.', &
         '                  Station name: ',TRIM(neq%par(iPar)%name)
          CYCLE
        ENDIF

! Get a priori, estimates, and cov matrix
        DO iCmp=0,2
          xApr(iCmp+1) = neq%par(iPar+iCmp)%x0
          xEst(iCmp+1) = neq%par(iPar+iCmp)%x0+neq%xxx(iPar+iCmp)
          DO iCov=0,2
            ik = ikf(iPar+iCmp,iPar+iCov)
            xCov(iCmp+1,iCov+1) = neq%aNor(ik)
          END DO
        END DO

! Correction and RMS (XYZ)
        xCor = xEst-xApr
        DO jj=1,3
          xRms(jj) = comstat%rms*SQRT(xCov(jj,jj))
        END DO

! Transformation to NEU
        CALL xyzell(datum%aEll,datum%bEll,datum%dxEll,datum%drEll,           &
                    datum%scEll,xStat(:,iSta),xStEll)
        CALL eccEll(xStEll,xCor,xCorEll)
        CALL eccEll(xStEll,xApr,xAprEll)
        CALL err3d(xStEll(1),xStEll(2),xStEll(3),datum%aEll,                 &
                   datum%bEll,-1,comstat%rms**2*xCov,xCovEll)

! Correction and RMS (NEU)
        radius = SQRT(xStat(1,iSta)**2+xStat(2,iSta)**2+xStat(3,iSta)**2)
        cosPhi = COS(xStEll(1))
        xRmsEll(1) = radius*SQRT(xCovEll(1,1))
        xRmsEll(2) = radius*cosPhi*SQRT(xCovEll(2,2))
        xRmsEll(3) = SQRT(xCovEll(3,3))

! Write output lines (XYZ)
        iSys = neq%par(iPar)%locq(4)
        WRITE(lfnprt,"(1X,A20,1X,A3,1X,A3,2X,F12.5,4X,F15.5,2X,2F12.5)")     &
              neq%par(iPar)%name,g_strSys3(iSys)," X ",                      &
              xApr(1),xEst(1),xCor(1),xRms(1)
        WRITE(lfnprt,"(26X,A3,2X,F12.5,4X,F15.5,2X,2F12.5)")                 &
              " Y ",xApr(2),xEst(2),xCor(2),xRms(2)
        WRITE(lfnprt,"(26X,A3,2X,F12.5,4X,F15.5,2X,2F12.5)")                 &
              " Z ",xApr(3),xEst(3),xCor(3),xRms(3)

! Write output lines (NEU)
        WRITE(lfnprt,"(/,26X,A3,2X,F12.5,4X,F15.5,2X,2F12.5)")               &
              " U ",xAprEll(3),xAprEll(3)+xCorEll(3),xCorEll(3),xRmsEll(3)
        WRITE(lfnprt,"(26X,A3,2X,F12.5,4X,F15.5,2X,2F12.5)")                 &
              " N ",xAprEll(1),xAprEll(1)+xCorEll(1),xCorEll(1),xRmsEll(1)
        WRITE(lfnprt,"(26X,A3,2X,F12.5,4X,F15.5,2X,2F12.5,/)")               &
              " E ",xAprEll(2),xAprEll(2)+xCorEll(2),xCorEll(2),xRmsEll(2)

! Troposphere biases
      ELSEIF (neq%par(iPar)%locq(3)==4) THEN
        xApr(1) = neq%par(iPar)%x0
        xEst(1) = neq%par(iPar)%x0+neq%xxx(iPar)
        xCor(1) = xEst(1)-xApr(1)
        xRms(1) = comstat%rms*SQRT(neq%aNor(ikf(iPar,iPar)))
        IF (neq%par(iPar)%name == oldNam) THEN
         WRITE(lfnprt,"(26X,A3,2X,F12.5,4X,F15.5,2X,2F12.5,/)")              &
               " T ",xApr(1),xEst(1),xCor(1),xRms(1)
       ELSE
         iSys = neq%par(iPar)%locq(4)
         WRITE(lfnprt,"(1X,A20,1X,A3,1X,A3,2X,F12.5,4X,F15.5,2X,2F12.5)")     &
               neq%par(iPar)%name,g_strSys3(iSys)," T  ",                     &
               xApr(1),xEst(1),xCor(1),xRms(1)
       ENDIF
     ENDIF

   ENDDO

 ENDIF


! Write coordinate file
! ---------------------
  IF (opt%coordrs/='')                                                     &
    CALL wtstat(1,opt%coordrs,filTitle,datum%name,                         &
                nSta,staNam,xStat,staNum,staFlg,opt%timRefCrd)

! Write velocity file
! -------------------
  IF (opt%velors/='')                                                      &
    CALL wtstat(1,opt%velors,filTitle,datum%name,                          &
                nSta,staNam,xVelo,staNum,staFlg)

! Create output lines for local ties (according to stainfo file)
! --------------------------------------------------------------
  ntie = 0
  DO ii=1, staInfo%nrelpar

    IF ( .NOT. staInfo%staRelPar(ii)%applied ) CYCLE

    IF ( staInfo%staRelPar(ii)%parTyp(1:3) /= 'CRD' )  CYCLE

    ntie = ntie + 1

  ! Write title line
  ! ----------------
    IF ( ntie == 1 ) THEN

      write(lfnprt,'(//A,/,A,/)') ' Local Ties at co-located sites:',    &
                                  ' -------------------------------'
      write(lfnprt,'(A,A,A)') ' Station 1            Station 2             SYS',       &
                                    '   Applied local tie (STAINFO)            ',      &
                                    'Difference in local tie (solution - apriori) [m]'
      write(lfnprt,'(91X,A,9X,A)') 'dX       dY       dZ',                  &
                                   'dN       dE       dU'
      write(lfnprt,'(1X,144("-"))')
    END IF

!!! Wird dann nach Umstellung auf neues Coordinaten-Format mit unterem Teil ersetzt !!!!
!!! ------------------------------------------------------------------------------------
    WRITE(lfnprt,'(1X,2(A16,5X),1X,A3,2X,3(F11.5,1X))')                             &
                 staInfo%staRelPar(ii)%stanam(1), staInfo%staRelPar(ii)%stanam(2), &
                 staInfo%staRelPar(ii)%sys(1), staInfo%staRelPar(ii)%locTie(1),    &
                 staInfo%staRelPar(ii)%locTie(2), staInfo%staRelPar(ii)%locTie(3)

!!!    DO iPar1=1, crdvel%nCrd
!!!      IF ( crdvel%crdRec(iPar1)%stanam == staInfo%staRelPar(ii)%stanam(1) ) THEN
!!!
!!!        DO iPar2=1, crdvel%nCrd
!!!          IF ( crdvel%crdRec(iPar2)%stanam == staInfo%staRelPar(ii)%stanam(2) ) THEN
!!!
!!!           ! ellipsoidal coordinates of station 1
!!!            CALL xyzell(datum%aell, datum%bell, datum%dxell, datum%drell, &
!!!                        datum%scell, crdvel%crdRec(iPar1)%xstat, xEmEll )
!!!
!!!           ! coordinate differences from solution
!!!            tie = crdvel%crdRec(iPar2)%xstat - crdvel%crdRec(iPar1)%xstat
!!!
!!!           ! Compute difference to apriori local tie (XYZ and NEU)
!!!            IF ( staInfo%staRelPar(ii)%sys(1) == 'NEU' ) THEN
!!!
!!!              CALL xyzloc(xEmEll(1),xEmEll(2),xEmEll(3), datum%aell, &
!!!                          datum%bell, +1, xApr1, staInfo%staRelPar(ii)%locTie, &
!!!                          hmat)
!!!
!!!              xCor = tie - xApr1
!!!
!!!            ELSE
!!!              xCor = tie - staInfo%staRelPar(ii)%locTie
!!!
!!!            END IF
!!!
!!!            CALL xyzloc(xEmEll(1), xEmEll(2), xEmEll(3), datum%aell, &
!!!                        datum%bell, -1, xCor, xCorEll, hmat)
!!!
!!!           ! Write Output
!!!           ! ------------
!!!            WRITE(lfnprt,"(1X,2(A16,5X),1X,A3,2X,3(F11.5,1X),3X,3(F8.5,1X),3X,3(F8.5,1X))") &
!!!                  staInfo%staRelPar(ii)%stanam(1), staInfo%staRelPar(ii)%stanam(2),         &
!!!                  staInfo%staRelPar(ii)%sys(1), staInfo%staRelPar(ii)%locTie(1),            &
!!!                  staInfo%staRelPar(ii)%locTie(2), staInfo%staRelPar(ii)%locTie(3),         &
!!!                  xCor(1), xCor(2), xCor(3), xCorEll(1), xCorEll(2), xCorEll(3)
!!!
!!!          END IF
!!!        END DO
!!!
!!!      END IF
!!!    END DO
!!!

  END DO

! Free some memory
! ----------------
  DEALLOCATE(xStat)
  DEALLOCATE(xVelo)
  DEALLOCATE(staNam)
  DEALLOCATE(staNum)
  DEALLOCATE(staFlg)
  DEALLOCATE(plate)

! The end
! -------
  RETURN

END SUBROUTINE crdstore

END MODULE
