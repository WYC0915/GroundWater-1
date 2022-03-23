MODULE s_SINTRAN3
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE sintran3(neq,aNor,bNor,ipar1,ipar2,sintim)

! -------------------------------------------------------------------------
! Purpose:    The subroutine performs the parameter transformation from
!             the SINEX convention (offset + drift) into
!             the ADDNEQ2 convention (offset + offset)
!
! Author:     L. Mervart
!
! Created:    03-Jan-1999
! Last mod.:  12-Mar-2009
!
! Changes:    21-Dec-2001  HU: Use m_bern, ONLY for modules
!             21-Dec-2001  HU: m_addneq replaced by p_addneq
!             22-Sep-2005  RD: Use new module D_NEQ.f90
!             12-Mar-2009  SL: new parameter sintim added for t1 and t2
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_neq,    ONLY: t_neq
  USE m_time,   ONLY: t_timint

  USE f_ikf
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_neq)                           :: neq
  REAL(r8b),    DIMENSION(*)            :: aNor
  REAL(r8b),    DIMENSION(*)            :: bNor
  INTEGER(i4b),              INTENT(IN) :: ipar1
  INTEGER(i4b),              INTENT(IN) :: ipar2
  TYPE(t_timint)                        :: sintim ! Start/end epoch of SINEX

! Local Variables
! ---------------
  REAL(r8b), DIMENSION(2,2)               :: C_Mat
  REAL(r8b), DIMENSION(neq%misc%npar-2,2) :: N12
  REAL(r8b), DIMENSION(2,2)               :: N22
  REAL(r8b), DIMENSION(2)                 :: b2
  REAL(r8b)                               :: t1, t2
  REAL(r8b)                               :: x01, x02

  INTEGER(i4b) :: ii
  INTEGER(i4b) :: ip1

! Change time and a priori values
! -------------------------------
!!!  t1 = neq%par(ipar1)%time%mean
!!!  t2 = neq%par(ipar1)%time%mean + 365.25d0
  t1 = sintim%t(1)
  t2 = sintim%t(2)

  neq%par(ipar1)%time%mean = t1
  neq%par(ipar1)%time%half = 0.d0
  neq%par(ipar2)%time%mean = t2
  neq%par(ipar2)%time%half = 0.d0

  x01 = neq%par(ipar1)%x0
  x02 = neq%par(ipar2)%x0

  neq%par(ipar1)%x0        = x01
  neq%par(ipar2)%x0        = x01 + x02 * (t2-t1) / 365.25d0

! Create the necessary matrices
! -----------------------------
  C_Mat(1,1) =  1.d0
  C_Mat(1,2) =  0.d0
  C_Mat(2,1) = -1.d0 / (t2-t1) * 365.25d0
  C_Mat(2,2) =  1.d0 / (t2-t1) * 365.25d0

  b2(1) = bNor(ipar1)
  b2(2) = bNor(ipar2)

  N22(1,1) = aNor(ikf(ipar1,ipar1))
  N22(1,2) = aNor(ikf(ipar1,ipar2))
  N22(2,1) = aNor(ikf(ipar2,ipar1))
  N22(2,2) = aNor(ikf(ipar2,ipar2))

  ip1 = 0
  DO ii = 1, neq%misc%npar

    IF (ii == ipar1 .OR. ii == ipar2) CYCLE

    ip1 = ip1 + 1

    N12(ip1,1) = aNor(ikf(ii,ipar1))
    N12(ip1,2) = aNor(ikf(ii,ipar2))

  END DO

! Compute the transformation
! --------------------------
  b2(1:2)      = MATMUL( TRANSPOSE(C_Mat), b2)

  N22(1:2,1:2) = MATMUL( TRANSPOSE(C_Mat), MATMUL(N22,C_Mat) )

  IF (neq%misc%npar-2 > 1) THEN
    N12(1:neq%misc%npar-2,1:2) = MATMUL(N12, C_Mat)
  END IF

! Backward substitution
! ---------------------
  bNor(ipar1)            = b2(1)
  bNor(ipar2)            = b2(2)

  aNor(ikf(ipar1,ipar1)) = N22(1,1)
  aNor(ikf(ipar1,ipar2)) = N22(1,2)
  aNor(ikf(ipar2,ipar1)) = N22(2,1)
  aNor(ikf(ipar2,ipar2)) = N22(2,2)

  ip1 = 0
  DO ii = 1, neq%misc%npar

    IF (ii == ipar1 .OR. ii == ipar2) CYCLE

    ip1 = ip1 + 1

    aNor(ikf(ii,ipar1)) = N12(ip1,1)
    aNor(ikf(ii,ipar2)) = N12(ip1,2)

  END DO

END SUBROUTINE sintran3

END MODULE
