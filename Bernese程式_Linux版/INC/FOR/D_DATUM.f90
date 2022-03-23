
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE d_datum

! -------------------------------------------------------------------------
! Purpose:    This module defines the structure for the geodetic datum
!
! Author:     U. Hugentobler
!
! Created:    18-Feb-2003
! Last mod.:  11-Dec-2006
!
! Changes:    11-Dec-2006  RD: Add SAVE to module variables
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern

! Geodetic datum
! --------------
  TYPE t_datum
    REAL(r8b)               :: aell
    REAL(r8b)               :: bell
    REAL(r8b), DIMENSION(3) :: dxell
    REAL(r8b), DIMENSION(3) :: drell
    REAL(r8b)               :: scell
    CHARACTER(LEN=16)       :: name
  END TYPE t_datum

  TYPE(t_datum), SAVE :: datum

END MODULE d_datum
