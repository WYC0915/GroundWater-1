MODULE s_SINTRAN2
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE sintran2(neq,aNor,bNor,ipar1,ipar2,bFlag,parFlag)

! -------------------------------------------------------------------------
! Purpose:    The subroutine performs the parameter transformation from
!             the ADDNEQ2 convention (offset + offset) into the SINEX
!             convention (offset + drift)
!
! Author:     L. Mervart
!
! Created:    14-Aug-1998
!
! Changes:    21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             22-Oct-2012 DT: Distinguish between changes in bNor and neq%par
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b
  USE d_neq,    ONLY: t_neq

  USE f_ikf
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_neq)                           :: neq
  REAL(r8b),    DIMENSION(*)            :: aNor
  REAL(r8b),    DIMENSION(*)            :: bNor
  INTEGER(i4b),              INTENT(IN) :: ipar1
  INTEGER(i4b),              INTENT(IN) :: ipar2
  INTEGER(i4b)                          :: bFlag
  INTEGER(i4b)                          :: parFlag

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

  t1 = neq%par(ipar1)%time%mean
  t2 = neq%par(ipar2)%time%mean

! Create the necessary matrices
! -----------------------------
  IF (neq%par(ipar1)%locq(1) == 1) THEN
    C_Mat(1,1) = 1.d0
    C_Mat(1,2) = 0.d0
    C_Mat(2,1) = 1.d0
    C_Mat(2,2) = (t2-t1) / 365.25d0
  ELSE
    C_Mat(1,1) = 1.d0
    C_Mat(1,2) = (t1-t2)/2.d0
    C_Mat(2,1) = 1.d0
    C_Mat(2,2) = (t2-t1)/2.d0
  END IF

  IF (bFlag == 1) THEN
    b2(1) = bNor(ipar1)
    b2(2) = bNor(ipar2)
  END IF

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
  IF (bFlag == 1) THEN
    b2(1:2)      = MATMUL( TRANSPOSE(C_Mat), b2)
  END IF

  N22(1:2,1:2) = MATMUL( TRANSPOSE(C_Mat), MATMUL(N22,C_Mat) )

  IF (neq%misc%npar-2 > 1) THEN
    N12(1:neq%misc%npar-2,1:2) = MATMUL(N12, C_Mat)
  END IF

! Backward substitution
! ---------------------
  IF (bFlag == 1) THEN
    bNor(ipar1)            = b2(1)
    bNor(ipar2)            = b2(2)
  END IF

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

! Change time and a priori values
! -------------------------------
  IF (parFlag == 1) THEN
    x01 = neq%par(ipar1)%x0
    x02 = neq%par(ipar2)%x0

    IF (neq%par(ipar1)%locq(1) == 1) THEN
      neq%par(ipar1)%time%mean = t1
      neq%par(ipar1)%time%half = 0.d0
      neq%par(ipar2)%time%mean = (t1+t2)/2.d0
      neq%par(ipar2)%time%half = (t2-t1)/2.d0
      neq%par(ipar1)%x0        = x01
      neq%par(ipar2)%x0        = (x02 - x01) / (t2-t1) * 365.25d0
    ELSE
      neq%par(ipar1)%time%mean = (t1+t2)/2.d0
      neq%par(ipar1)%time%half = 0.d0
      neq%par(ipar2)%time%mean = (t1+t2)/2.d0
      neq%par(ipar2)%time%half = (t2-t1)/2.d0

      ! Test leap second
      ! ----------------
      IF      (x02 - x01 >  500.d0) THEN
        x02 = x02 - 1000.d0
      ELSE IF (x02 - x01 < -500.d0) THEN
        x02 = x02 + 1000.d0
      END IF

      neq%par(ipar1)%x0        = (x02 + x01) / 2.d0
      neq%par(ipar2)%x0        = (x02 - x01) / (t2-t1)

    END IF

  END IF

END SUBROUTINE sintran2


END MODULE
