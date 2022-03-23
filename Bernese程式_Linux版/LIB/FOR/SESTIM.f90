MODULE s_SESTIM
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE sestim(sesFil,sesTbl,sessId,sessYr,window)

! -------------------------------------------------------------------------
! Purpose:    Extract of the time widnow of sessID from the session table
!             sesTbl
!
! Author:     R. Dach
!
! Created:    16-Apr-2003
! Last mod.:  16-Apr-2003
!
! Changes:    __-___-____ __:
!
! SR called:  exitrc, djul
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_sess,   ONLY: t_sesLst

  USE s_exitrc
  USE f_djul
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=fileNameLength)   :: sesFil     ! Name of the session table
  TYPE(t_sesLst)                  :: sesTbl     ! Session table
  CHARACTER(LEN=4)                :: sessId     ! Session ID
  INTEGER(i4b)                    :: sessYr     ! Year of the session
                                                ! Necessary for open session
                                                ! definition only

! output:
  REAL(r8b), DIMENSION(2)         :: window     ! Resulting time window (MJD)
                                                ! or (/0d0,1d20/) for undef.

! Local Parameters
! ----------------
  CHARACTER(LEN=6),PARAMETER      :: srName = 'sestim'


! Local Variables
! ---------------
  INTEGER(i4b)                    :: jSess
  INTEGER(i4b)                    :: iDay
  INTEGER(i4b)                    :: ios

  LOGICAL                         :: sesFound


! Init time window
! ----------------
  window=(/0d0,1d20/)
  sesFound = .FALSE.

! Fixed defined sessions in the session table
! -------------------------------------------
  IF (sesTbl%fix) THEN

      ! Get time windows from session table
      DO jSess = 1,sesTbl%nSess
        IF (sesTbl%sess(jSess)%sessID == sessID) THEN
          window(1) = sesTbl%sess(jSess)%timint%t(1)
          window(2) = sesTbl%sess(jSess)%timint%t(2)
          sesFound = .TRUE.
          EXIT
        ENDIF
      ENDDO

      ! Session not in table
      IF (.NOT. sesFound) THEN
        WRITE(lfnerr, '(/,A,/,2(18X,A,A,/))')                            &
        ' *** SR GTTIMWIN: Session was not found in the session table',  &
                          'Session table: ',TRIM(sesFil),                &
                          'Session ID:    ',TRIM(sessID)
        CALL exitrc(2)
      ENDIF

! Open session definition in the session table
! --------------------------------------------
  ELSE

    ! Get the day of year
    READ(sessID(1:3),*,iostat=ios) iDay

    IF (ios /= 0) THEN
      WRITE(lfnerr,'(/,A,3(/,18X,A),/)')                                  &
      ' *** SR GTTIMWIN: Cannot extract day of year ' //                  &
      'from session string "' // sessID // '"',                           &
      'An open session definition in the session table requires',         &
      'Session-IDs in the format ddds (ddd:day of year, s:session chr.)', &
      'Session table:  ' // TRIM(sesFil)
      CALL exitrc(2)
    ENDIF


    ! Get time windows from session table
    DO jSess = 1,sesTbl%nSess

      IF (sesTbl%sess(jSess)%sessID(4:4) == sessID(4:4)) THEN

        window(1) = DJUL(sessYr,1,1d0)-1d0 + &
                    DBLE(iDay)+sesTbl%sess(jSess)%timint%t(1)
        window(2) = DJUL(sessYr,1,1d0)-1d0 + &
                    DBLE(iDay)+sesTbl%sess(jSess)%timint%t(2)

        IF (window(2)<window(1))  window(2) = window(2) + 1

        sesFound = .TRUE.
        EXIT

      ENDIF
    ENDDO

    ! Session not in table
    IF (.NOT. sesFound) THEN
      WRITE(lfnerr, '(/,A,/,2(18X,A,A,/))')                            &
      ' *** SR GTTIMWIN: Session was not found in the session table',  &
                        'Session table: ',TRIM(sesFil),                &
                        'Session ID:    ',TRIM(sessID)
      CALL exitrc(2)
    ENDIF



  ENDIF ! Fixed or open session definition


  RETURN
END SUBROUTINE sestim

END MODULE
