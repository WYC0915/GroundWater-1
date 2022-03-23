MODULE s_FREENET2
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE freenet2(neq, xyz, sigma, wfact)

! -------------------------------------------------------------------------
! Purpose:    This subroutine removes the free network restriction from the
!             normal equation system stored in the SINEX-File
!             (as modification of the subroutine freenet, L. Mervart)
!             IN/OUT: neq-structure
!             IN    : xyz    station-coordinates
!                     sigma  constraints of Helmert-parameters
!                            (t1, t2, t3, r1, r2, r3, sc)
!                     wfact  variance-factor
!
! Author:     D. Thaller
!
! Created:    27-Mar-2001
! Last mod.:  22-Sep-2005
!
! Changes:    03-Nov-2001 HU: Use alcerr for allocation
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             19-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b
  USE d_neq,    ONLY: t_neq

  USE f_ikf
  USE s_alcerr
  USE f_matinv
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_neq)  :: neq
  REAL(r8b), DIMENSION(neq%misc%npar)  :: xyz
  REAL(r8b), DIMENSION(7)              :: sigma
  REAL(r8b)                            :: wfact

! Local Variables
! ---------------
  REAL(r8b), DIMENSION(:,:), ALLOCATABLE  :: Bmat1
  REAL(r8b), DIMENSION(:,:), ALLOCATABLE  :: Bmat2
  REAL(r8b), DIMENSION(:,:), ALLOCATABLE  :: Pmat
  REAL(r8b), DIMENSION(:,:), ALLOCATABLE  :: BTB
  REAL(r8b), DIMENSION(3,3)               :: regMat
  REAL(r8b)                               :: X1, Y1, Z1, X2, Y2, Z2

  INTEGER(i4b), DIMENSION(:), ALLOCATABLE :: indsta
  INTEGER(i4b)                            :: nstat, ista1, ista2
  INTEGER(i4b)                            :: ipar, ipar1, ipar2
  INTEGER(i4b)                            :: ip1, ip2, i1, i2
  INTEGER(i4b)                            :: ii, iac


! Count applied Helmert-parameters and allocate memory
! ----------------------------------------------------
  ii = 0
  DO i1=1, 7
    IF ( sigma(i1) /= 0.d0 ) THEN
      ii = ii + 1
    END IF
  END DO

  ALLOCATE (Bmat1(3,ii), stat=iac)
  CALL alcerr(iac, 'Bmat1', (/3,ii/), 'freenet2')
  ALLOCATE (Bmat2(3,ii), stat=iac)
  CALL alcerr(iac, 'Bmat2', (/3,ii/), 'freenet2')
  ALLOCATE (Pmat(ii,ii), stat=iac)
  CALL alcerr(iac, 'Pmat', (/ii,ii/), 'freenet2')
  Pmat = 0.d0
  ALLOCATE (BTB(ii,ii), stat=iac)
  CALL alcerr(iac, 'BTB', (/ii,ii/), 'freenet2')
  BTB = 0.d0


! Count number of stations
! ------------------------
  nstat = 0
  DO ipar = 1, neq%misc%npar
    IF ( neq%par(ipar)%locq(1) == 1  .AND. &
         neq%par(ipar)%locq(3) == 1          ) THEN
       nstat = nstat + 1
    ENDIF
  ENDDO

! Create station index
! --------------------
  ALLOCATE( indsta(nstat), stat=iac)
  CALL alcerr(iac, 'indsta', (/nstat/), 'freenet2')
  nstat = 0
  DO ipar = 1, neq%misc%npar
    IF ( neq%par(ipar)%locq(1) == 1  .AND. &
         neq%par(ipar)%locq(3) == 1          ) THEN
      nstat = nstat + 1
      indsta(nstat) = ipar
    ENDIF
  ENDDO

! Create weight-matrix P
! (only for those Helmert-parameters which are stored in SINEX)
! --------------------------------------------------------------
  ii = 1
  DO i1=1, 7
    IF ( sigma(i1) /= 0.d0 ) THEN
      Pmat(ii,ii) = wfact / (sigma(i1))**2
      ii = ii + 1
    ENDIF
  ENDDO

! Create the coefficient-matrix B
!  (only for those Helmert-parameters which are stored in SINEX)
! and compute the (B'B) Matrix
! --------------------------------------------------------------
  DO ista1 = 1, nstat
    ip1 = indsta(ista1)
    X1  = xyz(ip1  )
    Y1  = xyz(ip1+1)
    Z1  = xyz(ip1+2)

    ii = 1
    IF ( sigma(1) /= 0.d0 ) THEN
      Bmat1(:,ii) = (/ 1.d0, 0.d0, 0.d0 /)
      ii = ii + 1
    ENDIF
    IF ( sigma(2) /= 0.d0 ) THEN
      Bmat1(:,ii) = (/ 0.d0, 1.d0, 0.d0 /)
      ii = ii + 1
    ENDIF
    IF ( sigma(3) /= 0.d0 ) THEN
      Bmat1(:,ii) = (/ 0.d0, 0.d0, 1.d0 /)
      ii = ii + 1
    ENDIF
    IF ( sigma(4) /= 0.d0 ) THEN
      Bmat1(:,ii) = (/ 0.d0, Z1, -Y1 /)
      ii = ii + 1
    ENDIF
    IF ( sigma(5) /= 0.d0 ) THEN
      Bmat1(:,ii) = (/ -Z1, 0.d0, X1 /)
      ii = ii + 1
    ENDIF
    IF ( sigma(6) /= 0.d0 ) THEN
      Bmat1(:,ii) = (/ Y1, -X1, 0.d0 /)
      ii = ii + 1
    ENDIF
    IF ( sigma(7) /= 0.d0 ) THEN
      Bmat1(:,ii) = (/ X1, Y1, Z1 /)
      ii = ii + 1
    ENDIF


    BTB = BTB + MATMUL( TRANSPOSE(Bmat1), Bmat1 )

  ENDDO !ista1


! Compute the matrix (B'B)^-1 * P * (B'B)^-1
! ------------------------------------------------
  ii = matinv(BTB,ii-1)
  Pmat = MATMUL( BTB, MATMUL( Pmat, BTB ) )

! Create the regularization matrix B (B'B)^-1 * P * (B'B)^-1 B' and
! subtract the inner constraints from the normal-equation-matrix
! -----------------------------------------------------------------
  DO ista1 = 1, nstat
    ip1 = indsta(ista1)
    X1  = xyz(ip1  )
    Y1  = xyz(ip1+1)
    Z1  = xyz(ip1+2)

    ii = 1
    IF ( sigma(1) /= 0.d0 ) THEN
      Bmat1(:,ii) = (/ 1.d0, 0.d0, 0.d0 /)
      ii = ii + 1
    ENDIF
    IF ( sigma(2) /= 0.d0 ) THEN
      Bmat1(:,ii) = (/ 0.d0, 1.d0, 0.d0 /)
      ii = ii + 1
    ENDIF
    IF ( sigma(3) /= 0.d0 ) THEN
      Bmat1(:,ii) = (/ 0.d0, 0.d0, 1.d0 /)
      ii = ii + 1
    ENDIF
    IF ( sigma(4) /= 0.d0 ) THEN
      Bmat1(:,ii) = (/ 0.d0, Z1, -Y1 /)
      ii = ii + 1
    ENDIF
    IF ( sigma(5) /= 0.d0 ) THEN
      Bmat1(:,ii) = (/ -Z1, 0.d0, X1 /)
      ii = ii + 1
    ENDIF
    IF ( sigma(6) /= 0.d0 ) THEN
      Bmat1(:,ii) = (/ Y1, -X1, 0.d0 /)
      ii = ii + 1
    ENDIF
    IF ( sigma(7) /= 0.d0 ) THEN
      Bmat1(:,ii) = (/ X1, Y1, Z1 /)
    ENDIF


    DO ista2 = 1, nstat
      ip2 = indsta(ista2)
      X2  = xyz(ip2  )
      Y2  = xyz(ip2+1)
      Z2  = xyz(ip2+2)

      ii = 1
      IF ( sigma(1) /= 0.d0 ) THEN
        Bmat2(:,ii) = (/ 1.d0, 0.d0, 0.d0 /)
        ii = ii + 1
      ENDIF
      IF ( sigma(2) /= 0.d0 ) THEN
        Bmat2(:,ii) = (/ 0.d0, 1.d0, 0.d0 /)
        ii = ii + 1
      ENDIF
      IF ( sigma(3) /= 0.d0 ) THEN
        Bmat2(:,ii) = (/ 0.d0, 0.d0, 1.d0 /)
        ii = ii + 1
      ENDIF
      IF ( sigma(4) /= 0.d0 ) THEN
        Bmat2(:,ii) = (/ 0.d0, Z2, -Y2 /)
        ii = ii + 1
      ENDIF
      IF ( sigma(5) /= 0.d0 ) THEN
        Bmat2(:,ii) = (/ -Z2, 0.d0, X2 /)
        ii = ii + 1
      ENDIF
      IF ( sigma(6) /= 0.d0 ) THEN
        Bmat2(:,ii) = (/ Y2, -X2, 0.d0 /)
        ii = ii + 1
      ENDIF
      IF ( sigma(7) /= 0.d0 ) THEN
        Bmat2(:,ii) = (/ X2, Y2, Z2 /)
      ENDIF


      regMat = MATMUL( Bmat1, MATMUL( Pmat, TRANSPOSE(Bmat2) ) )

      DO i1 = 1, 3
        ipar1 = ip1+i1-1
        DO i2 = 1, 3
          ipar2 = ip2+i2-1
          IF (ipar2 < ipar1) CYCLE
          neq%aNor(ikf(ipar1,ipar2)) = neq%aNor(ikf(ipar1,ipar2)) - &
                                       regMat(i1,i2)
        ENDDO
      ENDDO

    ENDDO
  ENDDO

  DEALLOCATE (Bmat1, stat=iac)
  DEALLOCATE (Bmat2, stat=iac)
  DEALLOCATE (Pmat,  stat=iac)
  DEALLOCATE (BTB,   stat=iac)
  DEALLOCATE (indsta,stat=iac)

END SUBROUTINE freenet2

END MODULE
