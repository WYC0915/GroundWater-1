MODULE s_PRN2SVN
CONTAINS

! -----------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -----------------------------------------------------------------------------

SUBROUTINE prn2svn(isel,prn,epo,svnnr,timint,irc)

! -----------------------------------------------------------------------------
! Purpose:    Conversion of PRN number to SVN number
!             e.g.  51 (GPS 2000-025A) -> 20, with output of satellites start and end time,
!             using satellite info file, abort if more than one PRN at epoch,
!             warning and no output if epoch is outside the satellite availability
!
! Author:     A.Gaede
!
! Created:    27-Jul-2005
!
! Changes:    11-Jan-2006 HB: USE s_timst2
!             05-Oct-2011 SL: use m_bern with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -----------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, fileNameLength, lfnErr
  USE m_time,   ONLY: t_timint
  USE d_satfil, ONLY: t_satfil,init_satfil
  USE s_exitrc
  USE s_gtflna
  USE s_rdsatfil
  USE s_alcerr
  USE s_timst2
  IMPLICIT NONE

! Dummy list
! ----------
  TYPE(t_satfil), SAVE :: satfil

! Parameters
! ----------
! In:
  INTEGER(i4b)         :: isel     ! What should do in case of error
                                   ! 0: no warning/aborting
                                   ! 1: warning if SVN not at epoch or no SVN found
                                   !    abort if more than one SVN at epoch found
                                   ! 2: warning if SVN not at epoch
                                   !    abort if no SVN or more than one SVN at epoch found
                                   ! 3: warning if no SVN found
                                   !    abort if SVN not at epoch
                                   !    abort if more than one SVN at epoch found
                                   ! 4: abort if no SVN or SVN not at epoch or
                                   !    more than one SVN at epoch found
  INTEGER(i4b)         :: prn      ! Satellite number
  REAL(r8b)            :: epo      ! Epoch

! Out:
  CHARACTER(Len=4)     :: svnnr    ! System flag with SVN number
  TYPE(t_timint)       :: timint   ! Time intervals in JULIAN DATE
  INTEGER(i4b)         :: irc      ! Error return code
                                   ! 0: PRN at epoch found
                                   ! 1: PRN found but not at epoch
                                   ! 2: more than one PRN at epoch found
                                   ! 3: no PRN found

! Local Variables
! ---------------
  CHARACTER(LEN=fileNameLength)  :: filename
  CHARACTER(LEN=19)              :: tstrng
  INTEGER(i4b)                   :: icrx,found
  INTEGER(i4b)                   :: count    ! number of PRN found within time interval
  LOGICAL,      SAVE             :: first= .TRUE.


! If called for the first time, read the entire sat.info file
! -----------------------------------------------------------
  IF (first) THEN
    first = .FALSE.

! Get the satellite info file name
    CALL gtflna(1,'SATELL ',filename,irc)

! Read satellite info file
    CALL init_satfil(satfil)
    CALL rdsatfil(filename,satfil)
  END IF

! Searching the satellite in the satfil-structure
! -----------------------------------------------
  found=0
  count=0
  svnnr='    '
  timint%t=0D0

  DO icrx = 1,satfil%nsatellite
    IF ( satfil%satellite(icrx)%svn == prn ) THEN
      IF (found == 0) found = -icrx
        IF ( epo .GE. satfil%satellite(icrx)%timint%t(1) .AND. &
             epo .LE. satfil%satellite(icrx)%timint%t(2) ) THEN
          found = icrx
          count=count+1
        END IF
    END IF
  END DO

! No SPRN found
  IF (found == 0) THEN
    irc = 3

! PRN found, but not for epoch
  ELSEIF (found < 0) THEN
    irc = 1

! PRN found for epoch
  ELSEIF (found > 0) THEN
    irc = 0
    svnnr = satfil%satellite(found)%svnnr
    timint%t(1) = satfil%satellite(found)%timint%t(1)
    timint%t(2) = satfil%satellite(found)%timint%t(2)
  ENDIF

! more than one SPRN found for epoch
  IF (count > 1) THEN
    irc = 2
  ENDIF


!ERROR handling
!=================================================
  If (irc .NE. 0 .AND. isel .NE. 0) THEN
    CALL timst2(1,1,epo,tstrng)
  END IF

  IF (irc == 1) THEN
    IF (isel == 1 .OR. isel == 2)  THEN
      WRITE(lfnerr, &
        "(/,' ### SR PRN2SVN: PRN not found at epoch',                  &
        & /,'                 PRN number: ',I4,                         &
        & /,'                 Epoch     : ',A,/)") prn, tstrng
    ELSE IF (isel == 3) THEN
      WRITE(lfnerr, &
        "(/,' *** SR PRN2SVN: PRN not found at epoch',                  &
        & /,'                 PRN number: ',I4,                         &
        & /,'                 Epoch     : ',A,/)") prn, tstrng
      CALL exitrc(2)
    END IF
  ELSEIF (irc == 3) THEN
    IF (isel == 1 .OR. isel == 3)  THEN
      WRITE(lfnerr, &
        "(/,' ### SR PRN2SVN: PRN not found in satellite info file',    &
        & /,'                 PRN number: ',I4,                         &
        & /,'                 Epoch     : ',A,/)") prn, tstrng
    ELSE IF (isel == 2) THEN
      WRITE(lfnerr, &
        "(/,' *** SR PRN2SVN: PRN not found in satellite info file',    &
        & /,'                 PRN number: ',I4,                         &
        & /,'                 Epoch     : ',A,/)") prn, tstrng
      CALL exitrc(2)
    END IF
  ELSE IF (irc == 2) THEN
    IF (isel .NE. 0) THEN
      WRITE(lfnerr, &
        "(/,' *** SR PRN2SVN:',I2,' PRN numbers found at epoch',        &
        & /,'                 PRN number: ',I4,                         &
        & /,'                 Epoch     : ',A,/)") count, prn, tstrng
      CALL exitrc(2)
    END IF
  ENDIF

  RETURN

END SUBROUTINE prn2svn

END MODULE
