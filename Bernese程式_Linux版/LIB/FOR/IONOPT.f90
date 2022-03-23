MODULE s_IONOPT
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE ionopt(title, prtlev,icarr, iq,    intmax,rmsobs,modtyp,   &
                  minelv,iresid,idevla,idevha,idevmi,hion,meatype)

! -------------------------------------------------------------------------
! Purpose:    This is a new version of the old subroutine IONOPT.f that
!             reads the input options of the program IONEST
!
! Author:     C. Urschl
!
! Created:    19-Oct-2000
! Last mod.:  06-May-2011
!
! Changes:    21-Dec-2001  HU: Use d_const
!             23-Apr-2003  AJ: Nullify local pointers
!             14-Oct-2003  RD: Do not read keyValue without iostat
!             06-May-2011 HB: Add rdstdh to initialize model names
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_const,  ONLY: date,time
  USE d_stdorb, ONLY: t_stdhead, init_stdHead

  USE s_readkeys
  USE s_rdstdh
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
  CHARACTER(LEN=80)   :: title             ! title line
                                           ! PREPROCESSING PARAMETERS:
  INTEGER(i4b)        :: prtlev            ! print level (0:no,1:yes)
  INTEGER(i4b)        :: icarr             ! carrier used to detect breaks/gaps
  INTEGER(i4b)        :: iq                ! degree of polynomial (iq<4,
                                           ! skip test = -1)
  INTEGER(i4b)        :: intmax            ! max. interval length for test
  REAL(r8b)           :: rmsobs            ! RMS of one observation (m)
                                           ! PROCESSING PARAMETERS:
  INTEGER(i4b)        :: modtyp            ! type of ionosphere modeling
                                           ! (single layer = 1, ....=2)
  INTEGER(i4b)        :: minelv            ! minimal elevation (degrees)
  INTEGER(i4b)        :: iresid            ! compute residuals (0:no,1:yes)
                                           ! MODEL PARAMETERS:
  INTEGER(i4b)        :: idevla            ! deg. of development in latidude
  INTEGER(i4b)        :: idevha            ! deg. of development in hour angle
  INTEGER(i4b)        :: idevmi            ! max. degree of mixed coeff.
  REAL(r8b)           :: hion              ! height of ionospheric layer (m)
  INTEGER(i4b)        :: meatype           ! measurement type (1:phase, 2:code)

! Local Variables
! ---------------
  TYPE(t_stdhead)        :: stdHead   ! Structure of std header info
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER   :: keyValue
  CHARACTER(LEN=fileNameLength)                          :: filNam
  INTEGER(i4b)                                           :: irc, ioerr, ios

  NULLIFY(keyValue)
  CALL init_stdHead(stdHead)

  ioerr = 0
  ios   = 0

! Read Title
! ----------
  CALL readKeys('TITLE', keyValue, irc)
  title(1:64)  = keyValue(1)
  title(65:65) = ' '
  title(66:74) = DATE
  title(75:75) = ' '
  title(76:80) = TIME

! Read Preprocessing Options
! --------------------------
  prtlev=0
  CALL readKeys('PRINTMES', keyValue, irc)
  IF (irc == 0) THEN
    IF (keyValue(1) == '1') prtlev = 1
  ENDIF

  CALL readKeys('CARRIER', keyValue, irc)
  IF (keyValue(1) == 'L3') THEN
    icarr = 3
  ELSE IF (keyValue(1) == 'L4') THEN
    icarr = 4
  ELSE
    WRITE(lfnerr,*) ' *** SR IONOPT: invalid entry for carrier: ', keyValue(1)
    ioerr = ioerr + 1
  END IF

  CALL readKeys('POLYNOM', keyValue, irc)
  READ(keyValue(1),*,iostat=ios) iq
  IF (ios /= 0) THEN
    WRITE(lfnerr,*) ' *** SR IONOPT: invalid entry for polynomial degree: ', &
                      keyValue(1)
    ioerr = ioerr +1
  END IF

  CALL readKeys('TESTINT', keyValue, irc)
  READ(keyValue(1),*,iostat=ios) intmax
  IF (ios /= 0) THEN
    WRITE(lfnerr,*) ' *** SR IONOPT: invalid entry for max. interval for '   &
                      //'test: ',keyValue(1)
    ioerr = ioerr + 1
  END IF

  CALL readKeys('RMSOBS', keyValue, irc)
  READ(keyValue(1),*,iostat=ios) rmsobs
  IF (ios /= 0) THEN
    WRITE(lfnerr,*) ' *** SR IONOPT: invalid entry for RMS of one obs.: ',   &
                      keyValue(1)
    ioerr = ioerr + 1
  END IF

! Read Processing Options
! -----------------------
  CALL readKeys('IONTYP', keyValue, irc)
  READ (keyValue(1),*,iostat=ios) modtyp
  IF (ios /= 0) THEN
    WRITE(lfnerr,*) ' *** SR IONOPT: invalid entry for modeling type: ',   &
                      keyValue(1)
    ioerr = ioerr + 1
  END IF

  CALL readKeys('MINEL', keyValue, irc)
  READ(keyValue(1),*,iostat=ios) minelv
  IF (ios /= 0) THEN
    WRITE(lfnerr,*) ' *** SR IONOPT: invalid entry for min. elevation ',     &
                      keyValue(1)
    ioerr = ioerr + 1
  END IF

  iresid = 0
  CALL readKeys('PRINTRES', keyValue, irc)
  IF (irc == 0) THEN
    IF (keyValue(1) == '1') iresid  = 1
  ENDIF

! Read Parameters for Single Layer Model
! --------------------------------------
  IF (modtyp == 1) THEN

    CALL readKeys('DEGLAT', keyValue, irc)
    READ(keyValue(1),*,iostat=ios) idevla
    IF (ios /= 0) THEN
      WRITE(lfnerr,*) ' *** SR IONOPT: invalid entry for degree of '         &
                        //'development in latidude: ',keyValue(1)
      ioerr = ioerr + 1
    END IF

    CALL readKeys('DEGHOUR', keyValue, irc)
    READ(keyValue(1),*,iostat=ios) idevha
    IF (ios /= 0) THEN
      WRITE(lfnerr,*) ' *** SR IONOPT: invalid entry for degree of '         &
                        //'development in hour angle: ',keyValue(1)
      ioerr = ioerr + 1
    END IF

    CALL readKeys('DEGMIXED', keyValue, irc)
    READ(keyValue(1),*,iostat=ios) idevmi
    IF (ios /= 0) THEN
      WRITE(lfnerr,*) ' *** SR IONOPT: invalid entry for max. degree in '    &
                        //'mixed coefficients: ',keyValue(1)
      ioerr = ioerr + 1
    END IF

    CALL readKeys('HEIGHT', keyValue, irc)
    READ(keyValue(1),*,iostat=ios) hion
    hion = hion * 1.D3
    IF (ios /= 0) THEN
      WRITE(lfnerr,*) ' *** SR IONOPT: invalid entry for height of layer: ', &
                        keyValue(1)
      ioerr = ioerr + 1
    END IF

  END IF

  CALL readKeys('MEATYP', keyValue, irc)
  IF (keyValue(1) == 'PHASE') THEN
    meatype = 1
  ELSE IF (keyValue(1) == 'CODE') THEN
    meatype = 2
  ELSE
    WRITE(lfnerr,*) ' *** SR IONOPT: invalid entry for measurement type: ', &
                      keyValue(1)
    ioerr = ioerr + 1
  END IF

! Get model information from standard orbit file header
! -----------------------------------------------------
  filNam = ' '
  CALL rdstdh(filNam,stdHead,irc)

  IF (ioerr /= 0) THEN
    WRITE(lfnerr,"(/,'  Number of errors: ',I2)")ioerr
    CALL exitrc(2)
  END IF

  DEALLOCATE(keyValue,stat=irc)

END SUBROUTINE ionopt

END MODULE
