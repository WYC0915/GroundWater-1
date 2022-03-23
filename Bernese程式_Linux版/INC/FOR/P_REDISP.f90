
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE p_redisp

! -------------------------------------------------------------------------
! Purpose:    This module defines structures for program REDISP
!
! Author:     R. Dach
!
! Created:    10-Oct-2001
! Last mod.:  27-Aug-2002
!
! Changes:    27-Aug-2002  RD: Handle new formatted residual files
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
  CHARACTER(LEN=3),  DIMENSION(0:8), PARAMETER :: freqID = &
                     (/ '???','L1 ','L2 ','L3 ','L4 ','L5 ','RAD','LON','OUT' /)

  CHARACTER(LEN=5),  DIMENSION(3), PARAMETER :: filtyp = &
                     (/ 'PHASE','CODE ','RANGE' /)

  CHARACTER(LEN=10), DIMENSION(4), PARAMETER :: difTyp = &
                     (/ 'zero-diff.','sng-diff. ','dble-diff.','trpl-diff.' /)

END MODULE p_redisp
