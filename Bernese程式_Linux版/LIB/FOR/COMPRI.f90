MODULE s_COMPRI
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE compri(nFil,filnam,nflag,flags,titgen,iopbas,iopcov,refepo)

! -------------------------------------------------------------------------
! Purpose:    This is a new version of the old subroutine COMPRI.f that
!             reads the input options of the program COMPAR
!
! Author:     C. Urschl
!
! Created:    06-Oct-2000
! Last mod.:  07-Feb-2011
!
! Changes:    14-Sep-2001 RD: Generate list of files in this SR
!             22-Dec-2001 HU: Interface to prfile added
!             23-Apr-2003 CU: Nullify local pointers
!             07-Feb-2011 RD: Specify an epoch for comparison
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern

  USE s_alcerr
  USE s_gtfile
  USE s_prfile
  USE s_readkeys
  USE s_gtflna
  USE s_ckoptc
  USE s_ckoptz
  USE s_getco3
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
  INTEGER(i4b)                 :: nFil    ! Number of files
  CHARACTER(LEN=fileNameLength),&
    DIMENSION(:,:), POINTER    :: filNam  ! File names (1: crd, 2: cov)
  INTEGER(i4b)                 :: nflag   ! number of flags
  INTEGER(i4b)                 :: iopbas  ! baseline repeatability options
                                          ! 0: no baselines
                                          ! 1: baselines in L, B, H, LENGTH
                                          ! 2: baselines in X, Y, Z, LENGTH
  INTEGER(i4b)                 :: iopcov  ! saving scale factor
                                          ! 0: save GPS-RMS in covariances
                                          ! 1: save CRD-RMS in covariances
  CHARACTER(LEN=staFlagLength), &
    DIMENSION(:),  POINTER     :: flags   ! flags of coordinates to be
                                          ! included in the comparison
                                             ! @: all
                                             ! #: all non-blank flags
  CHARACTER(LEN=55)            :: titgen  ! general title for plot file
  REAL(r8b)                    :: refEpo  ! Reference epoch for comparison
                                          ! 0d0: use epochs from CRD-files


! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER  :: srName = 'compri'

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), &
    DIMENSION(:), POINTER       :: keyValue
  CHARACTER(LEN=staNameLength),  &
    DIMENSION(:), POINTER       :: cDummy
  CHARACTER(LEN=fileNameLength) :: velFil

  REAL(r8b)                     :: ref1,ref2

  INTEGER(i4b)                  :: nDummy
  INTEGER(i4b)                  :: refHlp
  INTEGER(i4b)                  :: ii
  INTEGER(i4b)                  :: irc, ioerr

! Init variables
! --------------
  ioerr = 0
  NULLIFY(keyValue)
  NULLIFY(cDummy)

! Get the file names
! ------------------
  CALL readKeys('COOFIL',keyValue,irc)

  ALLOCATE(filNam(2,SIZE(keyValue)),stat=irc)
  CALL alcerr(irc,'filNam',(/2,SIZE(keyValue)/),'compri')

  CALL gtfile('COOFIL',2,SIZE(filNam,2),nFil,filnam)

! Covar files and Coord file have identical names
! -----------------------------------------------
  CALL readKeys('SAMENA',keyValue,irc)
  IF (irc == 0 .AND. keyValue(1) == '1') THEN
    CALL prfile('COOFIL',' ',2,131)

! No cavariance file specified
! ----------------------------
  ELSE
    CALL readKeys('COVFIL',keyValue,irc)
    IF (irc == 0 .AND. LEN_TRIM(keyValue(1)) == 0) THEN
      filNam(2,:) = ' '
      CALL prfile('COOFIL',' ',1,131)

! Number of COVFIL and COOFIL is not equal
! ----------------------------------------
    ELSE IF (irc == 0 .AND. SIZE(keyValue) /= SIZE(filnam,2)) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,2(/,16X,A,I6),/)')                     &
      ' *** SR COMPRI: Different number of coordinate and covariance ', &
                      'files specified. They have to be equal!'       , &
                      'Number of coordinate files:  ',SIZE(filnam,2)  , &
                      'Number of covariance files:  ',SIZE(keyValue)
      CALL exitrc(2)

! Complete the filname list with the COVFILes
! -------------------------------------------
    ELSE IF (irc == 0) THEN
      filNam(2,:) = keyValue(:)

      CALL prfile('COOFIL',' ',1,131)
      CALL prfile('COVFIL',' ',1,131)
    ENDIF

  ENDIF

! Read the list of flags
! ----------------------
  CALL readKeys('FLAG', keyValue, irc)

! Special flags found
! -------------------
  IF (irc == 0 .AND. &
      (keyvalue(1) == 'ALL' .OR. keyValue(1) == 'NONBLANK')) THEN
    nFlag = 1

    ALLOCATE(flags(nFlag),stat=irc);
    CALL alcerr(irc,'flags',(/nFlag/),'compri')

    IF (keyValue(1) == 'ALL') THEN
      flags(1) = '@'
      IF (LEN_TRIM(filNam(2,1)) > 0)                                   &
        WRITE(lfnerr,'(/,A,3(/,16X,A),/)')                             &
        ' ### SR COMPRI: If all site coordinates (also without fla' // &
                                                       'gs) are used', &
                        'no covariance information is used.',          &
                        'To include covariance information use the' // &
                                                       ' "NONBLANK"',  &
                        'or "SPECIAL" option.'
      filNam(2,:) = ' '
    ENDIF

    IF (keyValue(1) == 'NONBLANK') flags(1) = '#'

! A real list of flags
! --------------------
  ELSE IF (irc == 0 .AND. keyValue(1) == 'SPECIAL') THEN

    CALL readKeys('FLAGSPEC', keyValue, irc)
    nFlag = 0
    DO ii = 1,SIZE(keyValue)
      IF (LEN_TRIM(keyValue(ii)) > 0) nFlag = nFlag + 1
    ENDDO

    IF (nFlag == 0) THEN
      WRITE(lfnerr,'(/,A,/)') &
      ' ### SR COMPRI: No flags selected. Nothing to do for PGM compar!'
      CALL exitrc(0)
    ENDIF

    ALLOCATE(flags(nFlag),stat=irc);
    CALL alcerr(irc,'flags',(/nFlag/),'compri')

    nFlag = 0
    DO ii = 1,SIZE(keyValue)
      IF (LEN_TRIM(keyValue(ii)) > 0) THEN
        nFlag = nFlag + 1
        flags(nFlag) = keyValue(ii)
      ENDIF
    ENDDO


! Incorrect flag specification
! ----------------------------
  ELSE
    WRITE(lfnerr,*) ' *** SR COMPRI: Incorrect flag specification: ' // &
                      TRIM(keyValue(1))
    ioerr = ioerr + 1
  END IF

! Get the title line
! ------------------
  CALL readKeys('TITLE', keyValue, irc)
  titgen = keyValue(1)

! Baseline repeatibility
! ----------------------
  CALL readKeys('BASREP', keyValue, irc)

  CALL ckoptc(1,'BASREP', keyValue,                              &
              (/'NO        ','LOCAL     ','GEOCENTRIC'/),        &
              srName,'Repeatability option',irc,ioerr,           &
              maxVal = 1, valList=(/0,1,2/),result1=iopbas)

! Type of RMS for COV output file
! -------------------------------
  CALL readKeys('RMSVAL', keyValue, irc)

  CALL ckoptc(1,'RMSVAL', keyValue,(/'GPS_DERIVED','COORD_COMBO'/),        &
              srName,'RMS value is taken from',irc,ioerr,                  &
              maxVal = 1, valList=(/0,1/),result1=iopcov)

! Reference coordincate correction
! --------------------------------
  refEpo = 0d0
  CALL gtflna(0,'VELOS',velFil,irc)

  IF (LEN_TRIM(velFil) > 0) THEN
    CALL readKeys('REFEPO', keyValue, irc)

    CALL ckoptc(1,'REFEPO', keyValue,                                      &
               (/'AS_IT_IS','FIRST   ','LAST    ','MEAN    ','MANUAL  '/), &
               srName,'Reference epoch correction',irc,ioerr,              &
               maxVal = 1, result1=refHlp)

    ! First
    IF (refHlp == 2) THEN
      CALL getco3(filnam(1,1),1,(/'@'/),nDummy,cDummy,timCrd=refEpo)
    ! Last
    ELSE IF (refHlp == 3) THEN
      CALL getco3(filnam(1,nFil),1,(/'@'/),nDummy,cDummy,timCrd=refEpo)
    ELSE IF (refHlp == 4) THEN
      CALL getco3(filnam(1,1),1,(/'@'/),nDummy,cDummy,timCrd=ref1)
      CALL getco3(filnam(1,nFil),1,(/'@'/),nDummy,cDummy,timCrd=ref2)
      refEpo = (ref1+ref2) / 2d0
    ELSE IF (refHlp == 5) THEN
      CALL readKeys('EPOCH', keyValue, irc)

      CALL ckoptz(1,'EPOCH', keyValue,                                     &
                 srName,'Manually specified reference epoch',irc,ioerr,    &
                 maxVal = 1, empty = 0d0, result1=refEpo)
    ENDIF
  ENDIF

  DEALLOCATE(keyValue,stat=irc)

! Problems reading input options
! ------------------------------
  IF (ioerr /= 0) THEN
    WRITE(lfnerr,"(/,'  Number of errors: ',I2)")ioerr
    CALL exitrc(2)
  END IF

END SUBROUTINE compri


END MODULE
