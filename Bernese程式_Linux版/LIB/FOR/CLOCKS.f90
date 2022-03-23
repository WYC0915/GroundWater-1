MODULE s_CLOCKS
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE clocks(mm)

! -------------------------------------------------------------------------
! Purpose:    Replacement of the C-routine clocks
!
! Author:     L. Mervart
!
! Created:    05-FEB-2001
! Last mod.:  __-___-____
!
! Changes:    __-___-____ __:
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern

  IMPLICIT NONE

! List of Parameters
! ------------------
  INTEGER(i4b), DIMENSION(*) :: mm

! Local Variables
! ---------------
  CHARACTER(LEN=8)  :: dateStr
  CHARACTER(LEN=10) :: timeStr
  CHARACTER(LEN=5)  :: zoneStr

  CALL DATE_AND_TIME(dateStr, timeStr, zoneStr, mm(1:8))

END SUBROUTINE clocks


END MODULE
