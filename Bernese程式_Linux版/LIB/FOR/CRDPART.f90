MODULE s_CRDPART
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE crdpart(neq, indp, isph, part)

! -------------------------------------------------------------------------
! Purpose:    Compute the partials -- Jacobi matrix of the transition
!             between spherical and rectangular coordinates
!
! Author:     L. Mervart
!
! Created:    22-Dec-1997
! Last mod.:  22-Sep-2005
!
! Changes:    21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             27-Feb-2003 HU: DATUM from D_DATUM
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_datum,  ONLY: datum
  USE d_neq,    ONLY: t_neq

  USE s_xyzell
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_neq)                :: neq
  INTEGER(i4b), DIMENSION(3) :: indp   ! Parameter indices
  INTEGER(i4b)               :: isph   ! Spherical coordinate index
  REAL(r8b),    DIMENSION(3) :: part   ! Partials

! Local Variables
! ---------------
  REAL(r8b), DIMENSION(3) :: xyz
  REAL(r8b), DIMENSION(3) :: ell
  REAL(r8b)               :: rr
  REAL(r8b)               :: sinPhi
  REAL(r8b)               :: cosPhi
  REAL(r8b)               :: cosLam


  xyz(1) = neq%par(indp(1))%x0
  xyz(2) = neq%par(indp(2))%x0
  xyz(3) = neq%par(indp(3))%x0

  CALL xyzell(datum%aell, datum%bell, datum%dxell, datum%drell, &
              datum%scell, xyz, ell)

  rr     = SQRT(xyz(1)**2 + xyz(2)**2 + xyz(3)**2)
  sinPhi = SIN(ell(1))
  cosPhi = COS(ell(1))
  cosLam = COS(ell(2))

  SELECT CASE ( ABS(isph) )

  CASE (1)  ! Latitude
    part(1) = - sinPhi * cosPhi * xyz(1) / (xyz(1)**2 + xyz(2)**2)
    part(2) = - sinPhi * cosPhi * xyz(2) / (xyz(1)**2 + xyz(2)**2)
    part(3) =   cosPhi**2 / SQRT(xyz(1)**2 + xyz(2)**2)
    part    = part * rr

  CASE (2)  ! Longitude
    part(1) = - cosLam**2 * xyz(2) / xyz(1)**2
    part(2) =   cosLam**2 / xyz(1)
    part(3) =   0.d0
    part    = part * rr

  CASE (3)  ! Height
    part(1) = xyz(1) / rr
    part(2) = xyz(2) / rr
    part(3) = xyz(3) / rr

  END SELECT

END SUBROUTINE crdpart


END MODULE
