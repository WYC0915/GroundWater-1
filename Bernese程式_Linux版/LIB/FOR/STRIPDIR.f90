MODULE s_STRIPDIR
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE stripDir(string)

! -------------------------------------------------------------------------
!
! Purpose:    Strip the path (directory) from the string
!
! Author:     L. Mervart
!
! Created:    14-Jun-2000
! Last mod.:  18-Feb-2003
!
! Changes:    15-Jul-2002 HU: Take larger value of ind1 and ind2
!             18-Feb-2003 LM: Use backslash from m_bern
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern

  IMPLICIT NONE

! List of Parameters
! ------------------
  CHARACTER(LEN=*) :: string

! Local Variables
! ---------------
  INTEGER(i4b) :: ind1
  INTEGER(i4b) :: ind2
  INTEGER(i4b) :: ind

  IF (string == '') RETURN

  ind1  = INDEX(string, "/", BACK=.TRUE.)
  ind2  = INDEX(string, backslash, BACK=.TRUE.)
  ind   = MAX(ind1,ind2)

  IF ( ind /= 0 ) string = string(ind+1:)

END SUBROUTINE stripDir


END MODULE
