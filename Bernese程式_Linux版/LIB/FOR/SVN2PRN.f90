MODULE s_SVN2PRN
CONTAINS

! -----------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -----------------------------------------------------------------------------

SUBROUTINE svn2prn(isel,svnnr,epo,prn,timint,irc)

! -----------------------------------------------------------------------------
! Purpose:    Conversion of SVN number to PRN number
!             e.g. 20 -> 51 (GPS 2000-025A) with output of satellites start and end time,
!             using satellite info file, works with given epoch or epoch = Zero
!            - warning/abort if no SVN found
!            - abort if more than one SVN for epoch
!            - if epo /= 0, warning/abort if epoch is outside the satellite availability
!            - if more than one line with same SVN warning/abort if PRN is equal
!
! Author:     A. Gaede
!
! Created:    15-AUG-2005
! Last mod.:  12-Apr-2011
!
! Changes:    11-Jan-2006 HB: USE s_timst2
!             19-Jan-2007 AG: irc = 6 added (more than one PRN for SVN)
!             12-Apr-2011 SS: Renaming concerning first GLONASS-K1
!
! SR used:    gtflna, rdsatfil, timst2, m_time
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -----------------------------------------------------------------------------

  USE m_bern
  USE m_time,   ONLY : t_timint
  USE d_satfil, ONLY : t_satfil,init_satfil
  USE s_exitrc
  USE s_gtflna
  USE s_rdsatfil
  USE s_alcerr
  USE s_timst2
  IMPLICIT NONE

! Dummy list
! ----------
  TYPE(t_satfil), SAVE             :: satfil


! Parameters
! ----------
! In:
  INTEGER(i4b)                     :: isel     ! What to do in case of error?
! general (indpendent of given isel):
! (irc=4) always aborting if more than one SVN for epoch found
! (irc=6) always warning if mor than one PRN for SVN found
!
! general (independent of epo=Zero or not)
! 0: no warning/abort
! 7: abort if any of the following errors occurred
!
! for epo /= Zero!!!
! 1: (irc=1) warning if one SVN found, but not for epoch
!    (irc=2) warning if more than one SVN entries not for epoch but unique
!    (irc=5) warning if no SVN found
!
! 2: (irc=1) warning if one SVN found, but not for epoch
!    (irc=2) warning if more than one SVN entries not for epoch but unique
!    (irc=5) abort if no SVN found
!
! 3: (irc=1) abort if one SVN found, but not for epoch
!    (irc=2) abort if more than one SVN entries not for epoch but unique
!    (irc=5) warning if no SVN found
!
! for epo = ZERO!!!
! 4: (irc=3) warning if more than one SVN entries but unique
!    (irc=5) warning if no SVN found
! 5: (irc=3) abort if more than one SVN entries but unique
!    (irc=5) warning if no SVN found
! 6: (irc=3) warning if more than one SVN entries but unique
!    (irc=5) abort if no SVN found
!
!
  CHARACTER(Len=4)                 :: svnnr    ! System flag with SVN number
  REAL(r8b)                        :: epo      ! Epoch

! Out:
  INTEGER(i4b)                     :: prn      ! PRN number
  TYPE(t_timint)                   :: timint   ! Time intervals in JULIAN DATE
  INTEGER(i4b)                     :: irc      ! Error return code
                                               ! 0: SVN at epoch found resp. SVN found if epo=0
                                               ! 1: SVN unique found but not at epoch
                                               ! 2: if epo>0, more than one SVN found at epoch, but PRN is unique
                                               ! 3: if epo=0, more than one SVN found, but PRN is unique
                                               ! 4: more than one SVN at epoch found and PRN are different
                                               ! 5: no SVN found
                                               ! 6: more than one SVN entry and different PRN found
! Local Variables
! ---------------
  CHARACTER(LEN=fileNameLength)    :: filename
  CHARACTER(LEN=19)                :: tstrng
  INTEGER(i4b)                     :: icrx,found
  INTEGER(i4b)                     :: count1,count2
  INTEGER(i4b)                     :: count3,count4 ! number of SVN found (see below)
  LOGICAL,      SAVE               :: first= .TRUE.


! If called for the first time, read the entire SATELL file
! =========================================================
  IF (first) THEN
    first = .FALSE.

! Get the satellite info file name
    CALL gtflna(1,'SATELL ',filename,IRC)

! Read satellite info file (SATELL)
    CALL init_satfil(satfil)
    CALL rdsatfil(filename,satfil)
  END IF

! Searching the satellite in the satfil-structure
! ===============================================
  found=0
  count1=0  !for number of found SVN for epoch
  count2=0  !for number of found SVN if epoch is ZERO and prn is equal
  count3=0  !for number of found SVN if epoch is outside the time window
  count4=0  !for number of found SVN if epoch is ZERO and prn is not equal
  prn=0
  timint%t=0D0

  IF (svnnr(1:1) == ' ') svnnr(1:1)='G'
  IF (svnnr(2:2) == ' ') svnnr(2:2)='0'
  IF (svnnr(3:3) == ' ') svnnr(3:3)='0'
! Renaming concerning first GLONASS-K1
  IF (svnnr == 'E801') svnnr='R801'
  DO icrx = 1,satfil%nsatellite
    IF ( satfil%satellite(icrx)%svnnr == svnnr ) THEN
      IF (found < 0 .AND. epo == 0) THEN
        IF (satfil%satellite(-found)%svn == satfil%satellite(icrx)%svn) THEN
          IF (satfil%satellite(-found)%timint%t(2) < satfil%satellite(icrx)%timint%t(2)) &
            satfil%satellite(-found)%timint%t(2) = satfil%satellite(icrx)%timint%t(2)
          IF (satfil%satellite(-found)%timint%t(1) > satfil%satellite(icrx)%timint%t(1)) &
            satfil%satellite(-found)%timint%t(1) = satfil%satellite(icrx)%timint%t(1)
          count2 = count2 + 1
        ELSE
          count4 = count4 + 1
        ENDIF
      ELSEIF (found < 0 .AND. epo > 0) THEN
        IF (satfil%satellite(-found)%svn == satfil%satellite(icrx)%svn) THEN
          found = -icrx
          count3 = count3 + 1
        ENDIF
      ELSEIF (found == 0) THEN
        found = -icrx
        IF (epo /= 0) count3 = 1
        IF (epo == 0) THEN
          count2 = 1
          count4 = 1
        ENDIF
      ENDIF
      IF ( epo .GE. satfil%satellite(icrx)%timint%t(1) .AND. &
           epo .LE. satfil%satellite(icrx)%timint%t(2) ) THEN
        found = icrx
        count1 = count1 + 1
      END IF
    END IF
  END DO
!
! No SVN found
  IF (found == 0) THEN
    irc = 5

! SVN unique found, but not for epoch
  ELSEIF (found < 0) THEN
    IF (count3 == 1) irc = 1
    IF (count3 > 1) irc = 2
    IF (count2 == 1) irc = 0
    IF (count2 > 1) irc = 3
    prn = satfil%satellite(-found)%svn
    timint%t(1) = satfil%satellite(-found)%timint%t(1)
    timint%t(2) = satfil%satellite(-found)%timint%t(2)

! SVN found for epoch
  ELSEIF (found > 0) THEN
    irc = 0
    prn = satfil%satellite(found)%svn
    timint%t(1) = satfil%satellite(found)%timint%t(1)
    timint%t(2) = satfil%satellite(found)%timint%t(2)
  ENDIF

! more than one SVN found for epoch
  IF (count1 > 1) THEN
    irc = 4
  ENDIF
! more than one PRN found for SVN
  IF (count4 > 1) THEN
    irc = 6
  ENDIF

! ERROR handling
! ==============
  IF (irc /= 0 .AND. isel /= 0) CALL timst2(1,1,epo,tstrng)

  IF (irc == 4) THEN
    IF (isel .NE. 0) THEN
      WRITE(lfnerr, &
        "(/,' *** SR SVN2PRN: SVN number',I2,' times found for epoch',       &
        & /,'                 SVN number: ',A4,                         &
        & /,'                 Epoch     : ',A,/)") count1, svnnr, tstrng
      CALL exitrc(2)
    END IF
!
  ELSEIF (irc == 1) THEN
    IF (isel == 1 .OR. isel == 2) THEN
      WRITE(lfnerr, &
        "(/,' ### SR SVN2PRN: SVN found, but not for epoch',            &
        & /,'                 SVN number: ',A4,                         &
        & /,'                 Epoch     : ',A,/)") svnnr, tstrng
    ELSE IF (isel == 3 .OR.isel == 7) THEN
      WRITE(lfnerr, &
        "(/,' *** SR SVN2PRN: SVN found, but not for epoch',            &
        & /,'                 SVN number: ',A4,                         &
        & /,'                 Epoch     : ',A,/)") svnnr, tstrng
      CALL exitrc(2)
    END IF
!
  ELSEIF (irc == 2) THEN
    IF (isel == 1 .OR. isel == 2) THEN
      WRITE(lfnerr, &
        "(/,' ### SR SVN2PRN: SVN number',I2,' times with unique PRN found, but not for epoch', &
        & /,'                 SVN number: ',A4,                         &
        & /,'                 Epoch     : ',A,/)") count3,svnnr, tstrng
    ELSE IF (isel == 3 .OR. isel == 7) THEN
      WRITE(lfnerr, &
        "(/,' *** SR SVN2PRN: SVN number',I2,' times with unique PRN found, but not for epoch', &
        & /,'                 SVN number: ',A4,                         &
        & /,'                 Epoch     : ',A,/)") count3,svnnr, tstrng
      CALL exitrc(2)
    END IF
!
  ELSEIF (irc == 3) THEN
    IF (isel == 4 .OR. isel == 6) THEN
      WRITE(lfnerr, &
        "(/,' ### SR SVN2PRN: SVN number',I2,' times with unique PRN found in SATTELITE file',    &
        & /,'                 time windows merged!!!',                         &
        & /,'                 SVN number: ',A4,/)")count2,svnnr
    ELSE IF (isel == 5 .OR. isel == 7) THEN
      WRITE(lfnerr, &
        "(/,' *** SR SVN2PRN: SVN number',I2,' times with unique PRN found in SATTELITE file',    &
        & /,'                 SVN number: ',A4,/)")count2,svnnr
      CALL exitrc(2)
    END IF
!
  ELSEIF (irc == 5) THEN
    IF (isel == 1 .OR. isel == 3 .OR. isel == 4 .OR. ISEL == 5) THEN
      WRITE(lfnerr, &
        "(/,' ### SR SVN2PRN: SVN not found in satellite info file',    &
        & /,'                 SVN number: ',A4,/)") svnnr
    ELSE IF (isel == 2 .OR. isel == 6 .OR. isel == 7) THEN
      WRITE(lfnerr, &
        "(/,' *** SR SVN2PRN: SVN not found in satellite info file',    &
        & /,'                 SVN number: ',A4,/)") svnnr
      CALL exitrc(2)
    END IF
  ELSEIF (irc == 6) THEN
    IF (isel .NE. 0) THEN
      WRITE(lfnerr, &
        "(/,' ### SR SVN2PRN: Without given epoch SVN number',I2,' times found',       &
        & /,'                 SVN number: ',A4)") count4, svnnr
!      CALL exitrc(2)
    ENDIF
  ENDIF

  RETURN

END SUBROUTINE svn2prn

END MODULE
