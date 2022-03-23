MODULE f_nextline
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

CHARACTER(LEN=255) FUNCTION nextline(lfn,adjFlg)

! -------------------------------------------------------------------------
! Purpose:    Read one line from the (already opened) file lfn
!
! Author:     L. Mervart
!
! Created:    22-NOV-1997
!
! Changes:    13-Jan-2009 RD: Use '' as EOF-identifier in NEXTLINE
!             29-Feb-2012 RD: Unreliable "EOF" reporting by iostat (workaround)
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern

  IMPLICIT NONE

! List of Parameters
! ------------------
  INTEGER(i4b), INTENT(IN) :: lfn     ! logical file number
  INTEGER(i4b), INTENT(IN) :: adjFlg  ! 1 ... adjust left

! Local Variables
! ---------------
  INTEGER(i4b) :: iBlank
  INTEGER(i4b) :: irc

  iBlank = 0
  DO
    READ(lfn,'(A)',iostat=irc) nextline

    IF (irc /= 0 .OR. iBlank > 1000) THEN
      nextline = ''
      RETURN
    END IF

    IF (adjFlg == 1) nextline = ADJUSTL(nextline)

    IF (nextline      == '')  THEN
      iBlank = iBlank + 1
      CYCLE
    ENDIF
    IF (nextline(1:1) == '#') CYCLE
    IF (nextline(1:1) == '!') CYCLE
    IF (nextline(1:1) == '*') CYCLE

    RETURN

  END DO

END FUNCTION nextline


END MODULE
