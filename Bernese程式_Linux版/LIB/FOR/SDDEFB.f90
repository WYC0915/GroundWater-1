MODULE s_SDDEFB
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE sddefb(plusFlag, nZeroFil, staZero, xstat, nSngFil, icombi)

! -------------------------------------------------------------------------
!
! Purpose:    This subroutine defines the baselines created by program
!             SNGDIF according to baseline definition file (strategies
!             DEFINED and PLUS)
!
! Author:     L. Mervart
!
! Created:    02-Jun-2000
!
! Changes:    20-Nov-2001 MM: Completely revised, correct handling of
!                             missing baselines
!             29-Dec-2001 HU: Interface to alcerr added
!             15-Feb-2011 RD: GETSTA is used as a module now
!             20-Sep-2012 RD: Correctly deallocate arrays
!             20-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnprt, lfnerr, lfn002, &
                      fileNameLength

  USE s_dimtst
  USE s_opnfil
  USE s_alcerr
  USE s_getsta
  USE s_gtflna
  USE s_opnerr
  IMPLICIT NONE

! Commons
! -------
  CHARACTER*6  mxnfil
  INTEGER(i4b) mxcfil
  COMMON/mcmfil/mxcfil,mxnfil


! List of Parameters
! ------------------
  INTEGER(i4b)                     :: plusFlag ! PLUS strategy or not
  INTEGER(i4b)                     :: nZeroFil ! number of zero-dif. files
  CHARACTER(LEN=*), DIMENSION(*)   :: staZero  ! station names
  REAL(r8b)       , DIMENSION(3,*) :: xstat    ! station coordinates
  INTEGER(i4b)                     :: nSngFil  ! number of single-dif. files
  INTEGER(i4b)    , DIMENSION(2,*) :: icombi   ! file numbers to be combined
                                               ! to form a single-diff. file

! Variables used for getsta subroutine
! ------------------------------------
  CHARACTER(LEN=16), DIMENSION(1)   :: stamis
  INTEGER(i4b),      DIMENSION(1)   :: stnmis
  INTEGER(i4b)                      :: ncemis
  INTEGER(i4b),      DIMENSION(1)   :: icemis
  REAL(r8b),         DIMENSION(3,1) :: xstmis
  REAL(r8b),         DIMENSION(3,1) :: xsemis
  REAL(r8b),         DIMENSION(3,1) :: xscmis
  CHARACTER(LEN=16)                 :: datum
  REAL(r8b)                         :: aell
  REAL(r8b)                         :: bell
  REAL(r8b), DIMENSION(3)           :: dxell
  REAL(r8b), DIMENSION(3)           :: drell
  REAL(r8b)                         :: scell

! Local Variables
! ---------------
  CHARACTER(LEN=fileNameLength)                  :: basDefFil
  CHARACTER(LEN=80)                              :: dummy
  CHARACTER(LEN=16), DIMENSION(:,:), ALLOCATABLE :: baseLine, orgLine
  CHARACTER(LEN=16)                              :: newSta
  CHARACTER(LEN=4)                               :: flag
  CHARACTER(LEN=1)                               :: which1, which2
  REAL(r8b)                                      :: distmin
  REAL(r8b)                                      :: dist
  INTEGER(i4b)     , DIMENSION(2)                :: ind
  INTEGER(i4b)                                   :: ii, jj, kk, def, nBasLin
  INTEGER(i4b)                                   :: ios, irc, iac



! Some initializations
! --------------------
  nSngFil     = 0
  nBasLin     = 0

! Get basline file
! ----------------
  CALL gtflna(1,'BASDEF', basDefFil, irc)
  CALL opnfil(lfn002, basDefFil, 'OLD', 'FORMATTED', 'READONLY', ' ', ios)
  CALL opnerr(lfnerr, lfn002, ios, basDefFil, 'sddefb')

! Get all besalines from file
! ---------------------------
  DO
    nBasLin = nBasLin+1
    READ(lfn002,'(A)',IOSTAT=ios) dummy
    IF (ios/=0 .OR. dummy==' ') EXIT
  ENDDO
  nBasLin = nBasLin-1

  ALLOCATE(baseLine(2,nBasLin),stat=iac)
  CALL alcerr (iac, 'baseLine',(/nBasLin/),'sddefb')
  ALLOCATE(orgLine(2,nBasLin),stat=iac)
  CALL alcerr (iac, 'orgLine',(/nBasLin/),'sddefb')

  REWIND(lfn002)
  baseLine(:,:) = ' '

  DO ii=1,nBasLin
    READ(lfn002,'(A16,1X,A16)',IOSTAT=ios)                                &
                baseLine(1,ii),baseLine(2,ii)
  ENDDO
  CLOSE (lfn002)

  orgLine(:,:) = baseLine(:,:)

! Check stations which should be replaced
!è---------------------------------------
  IF (plusFlag==1) THEN
    DO ii=1, nBasLin

comp_loop: DO jj=1,2
! Ignore missing stations which could not be replaced
        IF (baseLine(jj,ii)==' ') CYCLE comp_loop

! Find station in zero-diff-files
        DO kk=1,nZeroFil
          IF (baseLine(jj,ii) == staZero(kk)) CYCLE comp_loop
        ENDDO

! Station is missing
        staMis(1) = baseLine(jj,ii)
        newSta = ' '
        CALL getsta(1,staMis,stnMis,nceMis,iceMis,xstMis,xseMis,        &
                    xscMis,datum,aEll,bEll,dxEll,drEll,scEll)
        distMin = HUGE(0D0)

sta_loop: DO def=1, nZeroFil

! Check if station is already in baseline file
          DO kk=1, nBasLin
            IF (staZero(def)==baseLine(1,kk) .OR.                       &
                staZero(def)==baseLine(2,kk)) CYCLE sta_loop
          ENDDO

          dist = (xstMis(1,1)-xStat(1,def))**2+                         &
                 (xstMis(2,1)-xStat(2,def))**2+                         &
                 (xstMis(3,1)-xStat(3,def))**2
          dist = SQRT(dist)

          IF (dist<distMin) THEN
            distMin = dist
            newSta = staZero(def)
          ENDIF
        ENDDO sta_loop

! Replace all occurences of this missing station
        DO kk=1, nBasLin
          IF (baseLine(1,kk)==staMis(1)) baseLine(1,kk) = newSta
          IF (baseLine(2,kk)==staMis(1)) baseLine(2,kk) = newSta
        ENDDO

        IF (newSta/=' ') THEN
          WRITE(lfnprt,'(A,/,16X,A,A,/,16X,A,A,/,16X,A,F8.1,A,/)')          &
           ' ### SR SDFFIL: A station from the baseline-file is missing.',  &
                           'Missing station: ',staMis(1),                      &
                           'Replaced by    : ',newSta,                      &
                           'Distance       : ',distMin/1000d0," km"
        ELSE
          WRITE(lfnprt,'(A,/,16X,A,/,16X,A,A,/)')          &
           ' ### SR SDFFIL: A station from the baseline-file is missing ',  &
                           'and cannot be replaced.',                       &
                           'Missing station: ',staMis(1)
        ENDIF

      ENDDO comp_loop
    END DO
  ENDIF

! Print predefined baselines inoutput file
! ----------------------------------------
    write(lfnprt,'(50("-"))')
    write(lfnprt,'(11X,"LIST OF PREDEFINED BASELINES")')
    write(lfnprt,'(2X,"Station 1",12X,"Station 2",12X,"Stat.")')
    write(lfnprt,'(50("-"))')

! Count baselines and create iCombi
! ---------------------------------
  DO ii=1,nBasLin
    ind(:) = 0
    DO jj=1, nZeroFil
      IF (baseLine(1,ii)==staZero(jj)) ind(1) = jj
      IF (baseLine(2,ii)==staZero(jj)) ind(2) = jj
    ENDDO

    IF (ind(1)/=0 .AND. ind(2)/=0) THEN
      nSngFil = nSngFil+1
      CALL DIMTST(1,2,2,'sddefb','nSngFil',                             &
                  'Number of single difference files',' ',nSngFil,      &
                  mxcfil*(mxcfil-1)/2,irc)

      iCombi(1,nSngFil) = ind(1)
      iCombi(2,nSngFil) = ind(2)
    END IF

! Check status of baseline
    which1 = ' '
    which2 = ' '
    IF (ind(1)==0.OR.ind(2)==0) THEN
      flag = 'MISS'
      baseLine(1,ii)=orgLine(1,ii)
      baseLine(2,ii)=orgLine(2,ii)
      IF (ind(1)==0) which1='*'
      IF (ind(2)==0) which2='*'
    ELSEIF (baseLine(1,ii)==orgLine(1,ii).AND.                           &
            baseLine(2,ii)==orgLine(2,ii)) THEN
      flag = ' OK '
    ELSE
      flag = 'REPL'
      IF (baseLine(1,ii)/=orgLine(1,ii)) which1='*'
      IF (baseLine(2,ii)/=orgLine(2,ii)) which2='*'
    ENDIF

    write(lfnprt,'(1X,A1,A16,4X,A1,A16,5X,A4)') which1,baseLine(1,ii),    &
                                                which2,baseLine(2,ii),flag

  END DO

  write(lfnprt,'(50("-"),/)')

  DEALLOCATE(baseLine,stat=iac)
  DEALLOCATE(orgLine, stat=iac)

! The end
! -------
END SUBROUTINE sddefb

END MODULE
