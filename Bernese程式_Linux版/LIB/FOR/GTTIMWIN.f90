MODULE s_GTTIMWIN
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE gtTimWin(usewin,radioKeys,sessKeys,dateKeys,window)

! -------------------------------------------------------------------------
! Purpose:    Reads time window information in a standard way
!
! Remark:     Replacement of SR readSess
!             - Uses "CKOPT" SR series now
!             - "USEWIN"-buttom is added
!             - Complete date and time information are required
!             - Init time window to standard (/ 0d0,1d20 /)
!
! Author:     R. Dach
!
! Created:    11-Apr-2003
! Last mod.:  08-Aug-2005
!
! Changes:    08-Aug-2005 HB: Use new SR TIMST2 (module)
!
! SR called:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_time,   ONLY: t_timint
  USE d_sess,   ONLY: t_sesLst, init_sesLst

  USE s_ckoptt
  USE s_alcerr
  USE s_rdsess
  USE s_timst2
  USE s_sestim
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  USE s_ckoptd
  USE s_gtflna
  USE s_ckopti
  USE s_ckoptl
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)                :: usewin     ! Keywords for checkbox to
                                                ! switch the time window
                                                ! selection off
                                                ! Empty: no such checkbox
  CHARACTER(LEN=*),DIMENSION(:)   :: radioKeys  ! Keywords for radiobuttons:
                                                ! 2 items:
                                                !   1: Use session
                                                !   2: Use date
                                                ! 3 items (only if no "usewin"):
                                                !   1: No window
                                                !   2: Use session
                                                !   3: Use date
  CHARACTER(LEN=*),DIMENSION(2)   :: sessKeys   ! Keywords for session window
                                                !   1: year   2: session string
                                                ! Both keywords need to have
                                                !   entries (n Sessions allowed)
  CHARACTER(LEN=*),DIMENSION(4)   :: dateKeys   ! Keywords for date/time window
                                                !   1: start date
                                                !   2: start time
                                                !   3: end date
                                                !   4: end time
                                                ! Start or end may be open.
                                                ! Each date and time must be
                                                ! filled or be empty.

! output:
  REAL(r8b), DIMENSION(2)         :: window     ! Resulting time window (MJD)
                                                ! or (/0d0,1d20/) for open

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=8),PARAMETER      :: srName = 'gtTimWin'


! Local Variables
! ---------------
  TYPE(t_sesLst)                  :: sesTbl   ! Session table
  TYPE(t_timint)                  :: timint
  TYPE(t_timint)                  :: lstInt

  CHARACTER(LEN=keyValueLength),   &
            DIMENSION(:), POINTER :: keyValue
  CHARACTER(LEN=timStrgLength2)   :: epoStr
  CHARACTER(LEN=fileNameLength)   :: sesFil
  CHARACTER(LEN=4),DIMENSION(:),   &
                  ALLOCATABLE     :: sessID

  INTEGER(i4b)                    :: iYear
  INTEGER(i4b),   DIMENSION(:),    &
                  ALLOCATABLE     :: sesYear
  INTEGER(i4b)                    :: iWindow
  INTEGER(i4b)                    :: iSess
  INTEGER(i4b)                    :: nSess
  INTEGER(i4b)                    :: irCode
  INTEGER(i4b)                    :: irc,iac

  REAL(r8b)                       :: rHlp


! Init time window
! ----------------
  window = (/ 0d0,1d20 /)

  irCode = 0
  NULLIFY(keyValue)

! Is a time window selected, which type?
! --------------------------------------
  IF (LEN_TRIM(usewin) > 0) THEN

    CALL ckoptb(1,(/ usewin /),srName,'Use time window',irCode,       &
                result1 = iWindow)

    IF (iWindow == 1) THEN

      CALL ckoptb(1,radioKeys,srName,                                 &
                  'Select type of time window specification',irCode,  &
                  result1 = iWindow)
    ENDIF

  ELSE

      CALL ckoptb(1,radioKeys,srName,                                 &
                  'Select type of time window specification',irCode,  &
                  result1 = iWindow)

      IF (SIZE(radioKeys) == 3) iWindow = iWindow-1

  ENDIF

! No time window selected
! -----------------------
  IF (iWindow == 0) THEN

    window = (/ 0d0,1d20 /)

! Time window from session
! ------------------------
  ELSEIF (iWindow == 1) THEN

    ! Init session structure
    CALL init_sesLst(sesTbl)

    ! Read the session table
    CALL gtflna(1,'SESSION_TABLE',sesFil,irc)
    CALL rdsess(sesFil,sesTbl)

    ! Session strings
    CALL readKeys(sessKeys(2),keyValue,irc)

    nSess=SIZE(keyValue)
    ALLOCATE(sessID(nSess),stat=iac)
    CALL alcerr(iac,'sessID',(/nSess/),srName)

    CALL ckoptl(1,sessKeys(2),keyValue,srName,                        &
                'Time window: Sessions',irc,irCode,                   &
                maxLength=4,maxVal=nSess,result2=sessID)


    ! Fixed defined sessions in the session table
    ! -------------------------------------------
    IF (sesTbl%fix) THEN
      DO iSess = 1,nSess

        CALL sesTim(sesFil,sesTbl,sessID(iSess),0,timint%t)

        ! Session not found
        IF (timint%t(1) == 0d0 .OR. timint%t(2) == 1d20) THEN
          CALL exitrc(2)

        ! Update resulting time window
        ELSE

          IF (window(1) ==  0d0 .OR. window(1) > timint%t(1)) &
            window(1) = timint%t(1)
          IF (window(2) == 1d20 .OR. window(2) < timint%t(2)) &
            window(2) = timint%t(2)

        ENDIF

      ENDDO

    ! Open session definition in the session table
    ! --------------------------------------------
    ELSE

      ! Year of the sessions
      CALL readKeys(sessKeys(1),keyValue,irc)

      ALLOCATE(sesYear(SIZE(keyValue)),stat=iac)
      CALL alcerr(iac,'sesYear',(/SIZE(keyValue)/),srName)

      CALL ckopti(1,sessKeys(1),keyValue,srName,                        &
                  'Time window: Year of the session',irc,irCode,        &
                  ge=1,maxVal=SIZE(sesYear),result2=sesYear)


      ! Compute MJD for the session
      iYear=1
      lstInt%t = 1D20
      DO iSess=1,nSess

        ! Number of entries for "year" is equal to the number of sessions
        IF (SIZE(sesYear) == nSess) THEN

          CALL sesTim(sesFil,sesTbl,sessID(iSess),sesYear(iSess),timint%t)

        ! Automatic setting of the year
        ELSE

          CALL sesTim(sesFil,sesTbl,sessID(iSess),sesYear(iYear),timint%t)

          IF (iSess > 1 .AND. timint%t(1) < lstInt%t(1)) THEN
            iYear = iYear + 1
            IF (iYear > SIZE(sesYear)) THEN
              WRITE(lfnerr,'(/,A,/)')  &
                   ' *** SR GTTIMWIN: Not enough session years specified'
              CALL exitrc(2)
            ENDIF
            CALL sesTim(sesFil,sesTbl,sessID(iSess),sesYear(iYear),timint%t)
          ENDIF

          IF (iSess == nSess .AND. iYear < SIZE(sesYear)) THEN
            WRITE(lfnerr,'(/,A,/)')  &
                 ' *** SR GTTIMWIN: Too many session years specified'
            CALL exitrc(2)
          ENDIF

          lstInt = timint

        ENDIF

        ! Session not found
        IF (timint%t(1) == 0d0 .OR. timint%t(2) == 1d20) THEN
          CALL exitrc(2)

        ! Update resulting time window
        ELSE

          IF (window(1) ==  0d0 .OR. window(1) > timint%t(1)) &
            window(1) = timint%t(1)
          IF (window(2) == 1d20 .OR. window(2) < timint%t(2)) &
            window(2) = timint%t(2)

        ENDIF

      ENDDO ! Next session from list

      DEALLOCATE(sesYear,stat=iac)

    ENDIF ! Fixed or open session definition

! Deallocate arrays
! -----------------
    DEALLOCATE(sessId,stat=iac)
    DEALLOCATE(sesTbl%sess,stat=iac)


! Time window from date/time
! --------------------------
  ELSEIF (iWindow == 2) THEN

    ! Start date
    CALL readKeys(dateKeys(1),keyValue,irc)

    CALL ckoptd(1,dateKeys(1),keyValue,srName,                        &
                'Time window: start date',irc,irCode,                 &
                empty=0d0,gt=0d0,maxVal=1,result1=window(1))

    ! Read start time if start date is present
    IF (window(1) /= 0d0) THEN

      CALL readKeys(dateKeys(2),keyValue,irc)

      CALL ckoptt(1,dateKeys(2),keyValue,srName,                      &
                  'Time window: start time',irc,irCode,               &
                  ge=0d0,maxVal=1,result1=rHlp)

      window(1) = window(1) + rHlp/24d0
    ENDIF


    ! End date
    CALL readKeys(dateKeys(3),keyValue,irc)

    CALL ckoptd(1,dateKeys(3),keyValue,srName,                        &
                'Time window: end date',irc,irCode,                   &
                empty=1d20,maxVal=1,result1=window(2))

    ! Read start time if start date is present
    IF (window(2) /= 1d20) THEN

      CALL readKeys(dateKeys(4),keyValue,irc)

      CALL ckoptt(1,dateKeys(4),keyValue,srName,                      &
                  'Time window: end time',irc,irCode,                 &
                  ge=0d0,maxVal=1,result1=rHlp)

      window(2) = window(2) + rHlp/24d0
    ENDIF

    ! Wrong time window
    IF (window(1) >= window(2)) THEN

      CALL timst2(1,2,window,epoStr)
      WRITE(lfnerr,'(/,A,2(/,18X,A),/)')                                      &
      ' *** SR GTTIMWIN: The start epoch of the time window must be smaller', &
                        'than the end epoch.',                                &
                        'Time window: ' // epoStr
      irCode = irCode + 1

    ENDIF

  ENDIF

! Stop program in an input was wrong
  IF (irCode /= 0) CALL exitrc(2)

  DEALLOCATE(keyValue,stat=iac)

  RETURN
END SUBROUTINE gtTimWin

END MODULE
