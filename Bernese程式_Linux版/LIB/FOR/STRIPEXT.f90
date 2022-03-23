MODULE s_STRIPEXT
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE stripext(string)

! -------------------------------------------------------------------------
!
! Purpose:    Strip the path (directory) and extension from the string
!             (Addapted version of STRIPDIR.F)
!
! Author:     L. Mervart
!
! Created:    18-Apr-2005
! Last mod.:  18-Apr-2005
!
! Changes:    15-Jul-2002 HU: Take larger value of ind1 and ind2
!             18-Feb-2003 LM: Use backslash from m_bern
!             18-Apr-2005 CU: Strip extension too
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
  INTEGER(i4b) :: ind3

  IF (string == '') RETURN

  ind1  = INDEX(string, "/", BACK=.TRUE.)
  ind2  = INDEX(string, backslash, BACK=.TRUE.)
  ind   = MAX(ind1,ind2)

  ind3  = INDEX(string, ".", BACK=.TRUE.)
  IF (ind3 == 0) ind3 = LEN(string)+1

  string = string(ind+1:ind3-1)

END SUBROUTINE stripext


END MODULE
