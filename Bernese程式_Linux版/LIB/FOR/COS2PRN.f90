MODULE s_COS2PRN
CONTAINS

! -----------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -----------------------------------------------------------------------------

SUBROUTINE cos2prn(isel,cospar,epo,prn,irc)

! -----------------------------------------------------------------------------
! Purpose:    Conversion of COSPAR number to PRN number
!             e.g. 2000-039B -> 906 (CHAMP), using satellite info file
!
! Author:     D. Svehla
!
! Created:    06-Oct-2002
! Last mod.:  08-Aug-2005
!
! Changes:    03-May-2003 HU: General routine, not LEO specific
!                         HU: Epoch in error output
!             16-May-2003 AJ: Initialize structure
!             14-Mar-2005 CU: Extend error handling
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!
! SR used:    gtflna, rdsatfil, timst2
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -----------------------------------------------------------------------------

  USE m_bern
  USE d_satfil, ONLY: t_satfil,init_satfil

  USE s_timst2
  USE s_exitrc
  USE s_gtflna
  USE s_rdsatfil
  IMPLICIT NONE

! Dummy list
! ----------
  TYPE(t_satfil), SAVE  :: satfil


! Parameters
! ----------
! In:
  INTEGER(i4b)                 :: isel    ! Problem handling:
                                          ! 0: none
                                          ! 1: warning
                                          ! 2: error
  CHARACTER(LEN=9)             :: cospar  ! Cospar number
  REAL(r8b)                    :: epo     ! Epoch

! Out:
  INTEGER(i4b)                 :: prn     ! Satellite number
  INTEGER(i4b)                 :: irc     ! Error return code

! Local Variables
! ---------------
  CHARACTER(LEN=fileNameLength):: filename
  CHARACTER(LEN=19)            :: tstrng
  INTEGER(i4b)                 :: icrx

  LOGICAL,      SAVE           :: first= .TRUE.


! If called for the first time, read the entire SATELL file
! =========================================================
  IF (first) THEN
    first = .FALSE.

! Get the satellite info file name
! ---------------------------------------------------
    CALL gtflna(1,'SATELL ',filename,IRC)

! Read satellite info file (SATELL)
! ---------------------------------------------------
    CALL init_satfil(satfil)
    CALL rdsatfil(filename,satfil)
  END IF


! Searching the satellite in the satfil-structure
! ===============================================
  prn=0
  DO icrx = 1, satfil%nsatellite
    IF ( satfil%satellite(icrx)%cospar(1:9)==cospar(1:9) ) THEN
      IF ( epo .GE. satfil%satellite(icrx)%timint%t(1) .AND. &
           epo .LE. satfil%satellite(icrx)%timint%t(2) ) THEN
        prn = satfil%satellite(icrx)%svn
        EXIT
      END IF
    END IF
  END DO

  IF (prn == 0) THEN
    CALL timst2(1,1,epo,tstrng)
    IF (isel == 1) THEN
      WRITE(lfnerr, &
        "(/,' ### SR COS2PRN: COSPAR number not found in satellite info file', &
        & /,'                 COSPAR number: ',A9,    &
        & /,'                 Epoch        : ',A,/)") cospar, tstrng
      irc = 1
    ELSEIF (isel == 2) THEN
      WRITE(lfnerr, &
        "(/,' *** SR COS2PRN: COSPAR number not found in satellite info file', &
        & /,'                 COSPAR number: ',A9,    &
        & /,'                 Epoch        : ',A,/)") cospar, tstrng
      CALL exitrc(2)
    ENDIF
  ELSE
    irc = 0
  ENDIF


  RETURN

END SUBROUTINE cos2prn

END MODULE
