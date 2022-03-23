MODULE f_ut1_ut1r
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

FUNCTION ut1_ut1r(par)

! -------------------------------------------------------------------------
! Purpose:    This function returns the difference UT1 - UT1R
!
! Author:     L. Mervart
!
! Created:    22-Nov-1997
! Last mod.:  22-Sep-2005
!
! Changes:    21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             22-Sep-2005 RD: Use new modules D_NEQ.f90 and D_PAR.f90
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_par,    ONLY: t_par

  USE s_ut1red
  IMPLICIT NONE

  REAL(r8b)               :: ut1_ut1r

! List of Parameters
! ------------------
  TYPE(t_par)             :: par  ! Parameter description structure

! Local Variables
! ---------------
  REAL(r8b), DIMENSION(2) :: hlp

  ut1_ut1r = 0.d0

  IF (par%locq(1) == 10 .AND. par%locq(4) == 3 ) THEN
    IF ( par%locq(5) == 1 ) THEN
      CALL ut1red( par%time%mean,hlp(1) )
      ut1_ut1r = hlp(1)
    ELSE
      CALL ut1red( par%time%mean - par%time%half,hlp(1) )
      CALL ut1red( par%time%mean + par%time%half,hlp(2) )
      IF (par%time%half /= 0.0) THEN
        ut1_ut1r = (hlp(2) - hlp(1)) / (2.d0 * par%time%half )
      END IF
    END IF
  END IF

END FUNCTION ut1_ut1r


END MODULE
