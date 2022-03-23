
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE p_menaux

! -------------------------------------------------------------------------
! Purpose:    This module defines structures for program MENUAUX
!
! Author:     R. Dach
!
! Created:    20-Jun-2001
! Last mod.:  11-Dec-2006
!
! Changes:    10-Dec-2001  RD: A new indicator for inactive uniline fields
!             08-Jan-2003  RD: Define counting strings
!             11-Dec-2006  RD: Add SAVE to module variables
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  IMPLICIT NONE

! Parameters
! ----------
  CHARACTER(LEN=1), PARAMETER :: qt = '"'
  CHARACTER(LEN=1), PARAMETER :: q2 = '#'  ! qt // q2 ... q2 // qt indicates
                                           ! a single inactive uniline field

  CHARACTER(LEN=5), DIMENSION(11), PARAMETER :: menuCount =         &
  (/  'zero ','one  ','two  ','three','four ','five ','six  ',      &
      'seven','eight','nine ','ten  ' /)

  CHARACTER(LEN=255), SAVE    :: inpFileName

END MODULE p_menaux
