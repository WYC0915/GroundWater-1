MODULE s_CDOINF
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE cdoinf(maxfil, kinest, nftot,  codhed, codobs, phahed, phaobs, &
                  filfrq, iclpol, iclksv, ioutsv, irCode)

! -------------------------------------------------------------------------
!
! Purpose:    This is a new version of the old subroutine CDOINF.f that
!             reads the names of input and output files of program CODSPP
!
! Author:     L. Mervart
!
! Created:    31-May-2000
! Last mod.:  24-Nov-2003
!
! Changes:    08-Jan-2001 RD: Use gtfile to read the file types
!                             Error handling
!             21-Mar-2001 RD: ADD 1 TO CLOCK POLYNOM
!             26-Jun-2001 RD: Use alcerr for allocation
!             12-Dec-2002 RD: "ioutsv" works only if (iclksv /= 0)
!             22-Jan-2003 RD: Read "ioutsv" if outlier rejection requested
!             12-Mar-2003 RD: No LEO files if no LEO processing selected
!             23-Apr-2003 CU: Nullify local pointers
!             28-Apr-2003 RD: Stop program if no observatino file was selected
!             16-May-2003 AJ: Initialize structure
!             23-Jun-2003 HB: Interface for SR staFlg
!             13-Aug-2003 AJ: Test clock polynomial degree in kinematic case
!             24-Nov-2003 HU: LEO warning improved
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_gpsobs, ONLY: t_obsHead,init_obshead
  USE d_stacrx, ONLY: MtypeSPACE

  USE s_alcerr
  USE s_gtfile
  USE s_staflg
  USE s_rdhead2
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  IMPLICIT NONE

! List of Parameters
! ------------------
! input
  INTEGER(i4b)                   :: maxfil ! maximum number of files
  INTEGER(i4b)                   :: kinest ! =0 no kinematic estimation
                                           ! =1 estimate kinematic coordinates
! output
  INTEGER(i4b)                   :: nftot  ! total number of files
  CHARACTER(LEN=*), DIMENSION(*) :: codhed ! header file names (code)
  CHARACTER(LEN=*), DIMENSION(*) :: codobs ! observation file names (code)
  CHARACTER(LEN=*), DIMENSION(*) :: phahed ! header file names (phase)
                                           ! blank: no corresponding file
  CHARACTER(LEN=*), DIMENSION(*) :: phaobs ! observation file names (phase)
  INTEGER(i4b)    , DIMENSION(*) :: filfrq ! frequency   to be processed
  INTEGER(i4b)    , DIMENSION(*) :: iclpol ! type of clock modeling
                                           ! >0: polynomial degree
                                           ! -1: one offset per epoch
  INTEGER(i4b)    , DIMENSION(*) :: iclksv ! save clock estimates into obs file
                                           ! 0: no save
                                           ! 1: save code obs.files only
                                           ! 2: save phase obs.files only
                                           ! 3: save code+phase obs.files
  INTEGER(i4b)    , DIMENSION(*) :: ioutsv ! mark outliers in the obs file
                                           ! 0: no save
                                           ! 1: save code obs.files only
                                           ! 2: save phase obs.files only
                                           ! 3: save code+phase obs.files
  INTEGER(i4b)                   :: irCode ! return code (0 = O.K.)

! Functions
! ---------

! Local Types
! -----------

! Local Parameters
! ----------------

! Local Variables
! ---------------
  TYPE(t_obsHead)                                        :: obsHead

  CHARACTER(LEN=keyValueLength), DIMENSION(:),   POINTER :: keyValue
  CHARACTER(LEN=filePathLength), DIMENSION(:,:), POINTER :: fNames
  CHARACTER(LEN=20)                                      :: marTyp

  INTEGER(i4b)                                           :: ii
  INTEGER(i4b)                                           :: localFreq
  INTEGER(i4b)                                           :: localDegree
  INTEGER(i4b)                                           :: localClkSv
  INTEGER(i4b)                                           :: localOutSv
  INTEGER(i4b)                                           :: iFlag
  INTEGER(i4b)                                           :: nFil
  INTEGER(i4b)                                           :: irc
  INTEGER(i4b)                                           :: iac
  INTEGER(i4b)                                           :: ios

  LOGICAL                                                :: useLEO

!
  irCode=0
  CALL init_obshead(obsHead)
  NULLIFY(keyValue)
  NULLIFY(fNames)
!
! Read Frequency, Polynomial Degree and Receiver Clocks Options
! -------------------------------------------------------------
  CALL readkeys('FREQ', keyValue, irc)
  irCode=irCode+irc
  IF      (keyValue(1) == 'L1') THEN
    localFreq = 1
  ELSE IF (keyValue(1) == 'L2') THEN
    localFreq = 2
  ELSE IF (keyValue(1) == 'L3') THEN
    localFreq = 3
  ELSE
    write(lfnerr,'(A)')                               &
          ' ### CDOINF : WRONG ENTRY FOR KEYWORD "FREQ" IN INPUT FILE'
    irCode=irCode+1
  END IF

  CALL readkeys('CLKPOLY', keyValue, irc)
  irCode=irCode+irc
  IF (keyValue(1) == 'E') THEN
    localDegree = -1
  ELSE
    READ(keyValue(1),*,iostat=ios) localDegree
    irCode=irCode+ios
    IF (localDegree > 7 .OR. localDegree < 0) THEN
      write(lfnerr,'(A)')                               &
            ' ### CDOINF : WRONG ENTRY FOR KEYWORD "CLKPOLY" IN INPUT FILE'
      irCode=irCode+1
    END IF
  END IF

  CALL readkeys('CLKSAVE', keyValue, irc)
  irCode=irCode+irc
  IF (keyValue(1) == 'NO') THEN
    localClkSv = 0
  ELSE IF (keyValue(1) == 'CODE') THEN
    localClkSv = 1
  ELSE IF (keyValue(1) == 'PHASE') THEN
    localClkSv = 2
  ELSE IF (keyValue(1) == 'BOTH') THEN
    localClkSv = 3
  ELSE
    write(lfnerr,'(A)')                               &
          ' ### CDOINF : WRONG ENTRY FOR KEYWORD "CLKSAVE" IN INPUT FILE'
    irCode=irCode+1
  END IF

  localOutSv = 0
  CALL readkeys('OUTDET', keyValue, irc)
  IF (keyValue(1) == '1') THEN
    CALL readkeys('OUTLSAVE', keyValue, irc)
    irCode=irCode+irc
    IF (keyValue(1) == 'NO') THEN
      localOutSv = 0
    ELSE IF (keyValue(1) == 'CODE') THEN
      localOutSv = 1
    ELSE IF (keyValue(1) == 'PHASE') THEN
      localOutSv = 2
    ELSE IF (keyValue(1) == 'BOTH') THEN
      localOutSv = 3
    ELSE
      write(lfnerr,'(A)')                               &
            ' ### CDOINF : WRONG ENTRY FOR KEYWORD "OUTLSAVE" IN INPUT FILE'
      irCode=irCode+1
    END IF

    IF ((localOutSv /= 0) .AND. (localOutSv /= localClkSv) .AND. &
        (localOutSv == 0 .OR.  localClkSv /= 3)) THEN
      WRITE(lfnerr,'(A,/,16X,A)') &
            ' ### SR CDOINF: Marking of outliers in observation files will', &
                            'only be applied if the clock estimates are stored.'
      localOutSv = 0
    ENDIF
  ENDIF

! Use LEOs?
! ---------
  CALL ckoptb(1,(/'LEOPROC'/),'cdoinf','LEO processing',irCode, &
              resultL=useLEO)

! Read the Names of Code Header Files
! -----------------------------------
  CALL readkeys('CODEFILE', keyValue, irc)
  irCode=irCode+irc
  nftot = SIZE( keyValue)
  IF (nftot > maxfil) THEN
    WRITE(lfnerr,*) ' *** cdoinf: too many files'
    CALL exitrc(2)
  END IF

! Create All Filenames
! --------------------
  ALLOCATE(fNames(4,nftot), stat=iac)
  CALL alcerr(iac, 'fNames', (/4,nftot/), 'cdoinf')
  CALL gtfile('CODEFILE',4,maxfil,nftot,fNames)
!
  nFil = 0
  DO ii = 1, nftot

    CALL rdhead2(fNames(1,ii),obsHead)

    CALL staflg(obsHead%sta(1)%staNam,obsHead%timref,iFlag,marTyp)

    IF ((marTyp == MTypeSPACE .AND. useLEO) .OR. &
        (marTyp /= MTypeSPACE .AND. .NOT. useLEO)) THEN

      nFil = nFil+1

      codhed(nFil) = fNames(1,ii)
      codobs(nFil) = fNames(2,ii)
      phahed(nFil) = fNames(3,ii)
      phaobs(nFil) = fNames(4,ii)

      filfrq(nFil) = localFreq
      iclpol(nFil) = localDegree
      IF (iclpol(nFil) >= 0) iclpol(nFil) = iclpol(nFil) + 1
      iclksv(nFil) = localClkSv
      ioutsv(nFil) = localOutSv

! Warning if files ignored
! ------------------------
    ELSE IF (useLEO) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,/)') &
      ' ### SR CDOINF: File not used because LEO processing was selected', &
                      'File does not contain a LEO: ' // TRIM(fNames(1,ii))
    ELSE
      WRITE(lfnerr,'(/,A,/,16X,A,/)') &
      ' ### SR CDOINF: File not used because no LEO processing was selected.',   &
                      'File contains a LEO: ' // TRIM(fNames(1,ii))
    ENDIF

    DEALLOCATE(obsHead%sat,   stat=irc)
    DEALLOCATE(obsHead%ambigu,stat=irc)

  END DO

  nftot=nFil

  DEALLOCATE(fNames,stat=irc)
  DEALLOCATE(keyValue,stat=irc)

  IF (irCode /= 0) irCode=2

! No observation file selected
! ----------------------------
  IF (nftot == 0) THEN
    WRITE(lfnerr,'(/,A,/)') &
    ' ### SR CDOINF: No observation file selected. Program has been stopped.'
    CALL exitrc(0)
  ENDIF

! No polynomial clock allowed in kinematic case
! ---------------------------------------------
  IF (kinest == 1 .AND. localDegree .ge. 0) THEN
    WRITE(lfnerr,'(/,A,/)') &
    ' *** SR CDOINF: Clock polynomial degree not allowed in kinematic case.'
    CALL exitrc(2)
  ENDIF

END SUBROUTINE cdoinf

END MODULE
