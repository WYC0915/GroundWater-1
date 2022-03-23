MODULE s_MENDPAV
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE mendpav(lat, H, T, zen, mapfun)

! -------------------------------------------------------------------------
! Purpose:    Mapping function according to Mendes-Pavlis
!
! Remark:     According to the formulas given in Chapter 9 of the updated
!             IERS Conventions 2003 (as of 22 June 2007): FCUL_b
!
! Author:     D. Thaller
!
! Created:    12-Nov-2008
! Last mod.:
!
! Changes:    xx-xxx-xxxx  XX:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_const,  ONLY: pi

  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  REAL(r8b)  :: lat    ! Latitude of station [rad]
  REAL(r8b)  :: H      ! Height of station [m]
  REAL(r8b)  :: T      ! Temperature [°C]
  REAL(r8b)  :: zen    ! Zenith distance [rad]

! output:
  REAL(r8b)  :: mapfun


! Local Parameters
! ----------------
  REAL(r8b), DIMENSION(4), PARAMETER  :: a1j =   &
             (/ 1.21008d-3, 1.7295d-6, 3.191d-5, -1.8478d-8 /)
  REAL(r8b), DIMENSION(4), PARAMETER  :: a2j =   &
             (/ 3.04965d-3, 2.346d-6, -1.035d-4, -1.856d-8 /)
  REAL(r8b), DIMENSION(4), PARAMETER  :: a3j =   &
             (/ 6.8777d-2, 1.972d-5, -3.458d-3, 1.060d-7 /)


! Local Variables
! ---------------
  REAL(r8b)  :: a1, a2, a3
  REAL(r8b)  :: el, sinE, cosL
  REAL(r8b)  :: m1, m2


! Compute coefficients
! --------------------
  cosL = dcos(lat)

  a1 = a1j(1) + a1j(2)*T + a1j(3)*cosL + a1j(4)*H

  a2 = a2j(1) + a2j(2)*T + a2j(3)*cosL + a2j(4)*H

  a3 = a3j(1) + a3j(2)*T + a3j(3)*cosL + a3j(4)*H

! Continued fraction
! ------------------
  el = pi/2.d0 - zen
  sinE = dsin(el)

  m1 = 1.d0 + a1 / (1.d0 + a2 / (1.d0 + a3))

  m2 = sinE + a1 / (sinE + a2 / (sinE + a3))

  mapfun = m1 / m2

  RETURN
END SUBROUTINE mendpav

END MODULE
