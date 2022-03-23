MODULE s_INQUIRE
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE inquire(UNIT, FILE, NAME, EXIST, OPENED, NUMBER)

! -------------------------------------------------------------------------
!
! Purpose:    Our sophisticated INQUIRE routine
!
! Author:     U. Hugentobler
!
! Created:    09-Aug-2000
! Last mod.:  31-Jul-2009
!
! Changes:    19-Feb-2003 RD: Stop if RPLENVAR failed
!             30-Jul-2009 SL: IF one-liners to IF blocks
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern

  USE s_rplenvar
  IMPLICIT NONE

! List of Parameters
! ------------------
! input
  INTEGER(i4b)    , OPTIONAL, INTENT(IN)  :: UNIT
  CHARACTER(LEN=*), OPTIONAL, INTENT(IN)  :: FILE

! output
  CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: NAME
  LOGICAL,          OPTIONAL, INTENT(OUT) :: EXIST
  LOGICAL,          OPTIONAL, INTENT(OUT) :: OPENED
  INTEGER(i4b)    , OPTIONAL, INTENT(OUT) :: NUMBER

! Functions
! ---------

! Local types
! -----------

! Local parameters
! ----------------

! Local Variables
! ---------------
  CHARACTER(LEN=255)         :: filnam

  IF (PRESENT(UNIT)) THEN
    IF (PRESENT(NAME)  ) THEN
      INQUIRE(UNIT=UNIT, NAME=NAME)
    ENDIF
    IF (PRESENT(EXIST) ) THEN
      INQUIRE(UNIT=UNIT, EXIST=EXIST)
    ENDIF
    IF (PRESENT(OPENED)) THEN
      INQUIRE(UNIT=UNIT, OPENED=OPENED)
    ENDIF
  ELSE IF (PRESENT(FILE)) THEN
    filnam = FILE
    CALL RplEnVar(2,filnam)
    IF (PRESENT(EXIST) ) THEN
      INQUIRE(FILE=filnam, EXIST=EXIST)
    ENDIF
    IF (PRESENT(OPENED)) THEN
      INQUIRE(FILE=filnam, OPENED=OPENED)
    ENDIF
    IF (PRESENT(NUMBER)) THEN
      INQUIRE(FILE=filnam, NUMBER=NUMBER)
    ENDIF
  END IF

  RETURN

END SUBROUTINE inquire

END MODULE
