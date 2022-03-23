MODULE s_BLOCKRET
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE blockret(neq)

! -------------------------------------------------------------------------
! Purpose:    This subroutine blocks retrograde diurnal terms in sub-daily
!             estimates of polar wobble series X and Y
!
! Author:     L. Mervart
!
! Created:    11-Sep-1998
!
! Changes:    26-Jun-2001 RD: Use alcerr for allocation
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             19-Sep-2012 RD: Use M_BERN with ONLY
!             16-Oct-2012 SS: opt%blkRet with user-defined sigma value
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnerr
  USE d_neq,    ONLY: t_neq
  USE p_addneq, ONLY: opt
  USE d_const,  ONLY: omega

  USE f_ikf
  USE s_alcerr
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_neq)                               :: neq

! Local Variables
! ---------------
  INTEGER(i4b)                              :: ipar
  INTEGER(i4b)                              :: ipar1
  INTEGER(i4b)                              :: ipar2
  INTEGER(i4b)                              :: npolX
  INTEGER(i4b)                              :: npolY
  INTEGER(i4b)                              :: ip
  INTEGER(i4b)                              :: ip1
  INTEGER(i4b)                              :: ip2
  INTEGER(i4b)                              :: iac
  INTEGER(i4b), DIMENSION(:),   ALLOCATABLE :: indp

  REAL(r8b),    DIMENSION(:,:), ALLOCATABLE :: Hmat
  REAL(r8b),    DIMENSION(:,:), ALLOCATABLE :: regMat
  REAL(r8b)                                 :: tArg
  REAL(r8b)                                 :: cwt
  REAL(r8b)                                 :: swt
  REAL(r8b)                                 :: weight
  REAL(r8b)                                 :: t0

! Count number of pole parameters
! -------------------------------
  npolX = 0
  npolY = 0

  DO ipar = 1, neq%misc%npar
    IF ( neq%par(ipar)%locq(1) /= 10 ) CYCLE

    IF (neq%par(ipar)%locq(5) == 2) THEN
      WRITE(lfnerr,*) ' *** SR blockret: drifts are not handled'
      RETURN
    END IF

    IF ( neq%par(ipar)%locq(4) == 1 ) npolX = npolX + 1
    IF ( neq%par(ipar)%locq(4) == 2 ) npolY = npolY + 1
  END DO

  IF ( npolX /= npolY ) THEN
    WRITE(lfnerr,*) ' *** SR blockret : npolX /= npolY'
    RETURN
  END IF

  IF ( npolX == 0 ) RETURN

! Create pole parameter index
! ---------------------------
  t0 = HUGE(r8b)
  ip = 0
  ALLOCATE(indp(2*npolX), stat=iac)
  CALL alcerr(iac, 'indp', (/2*npolX/), 'blockret')

  DO ipar1 = 1, neq%misc%npar
    IF ( neq%par(ipar1)%locq(1) /= 10 ) CYCLE

    IF ( neq%par(ipar1)%locq(4) == 1 ) THEN
      ip = ip + 1
      indp(ip) = ipar1
      IF ( neq%par(ipar1)%time%mean < t0 ) t0 = neq%par(ipar1)%time%mean

      DO ipar2 = 1, neq%misc%npar
        IF ( neq%par(ipar2)%locq(1) /= 10 ) CYCLE

        IF ( neq%par(ipar2)%locq(4)   == 2                          .AND. &
             neq%par(ipar2)%time%mean == neq%par(ipar1)%time%mean ) THEN
          ip = ip + 1
          indp(ip) = ipar2
        END IF
      END DO

    END IF
  END DO

! Check the consistency of pole parameter epochs
! ----------------------------------------------
  IF ( ip /= 2*npolX ) THEN
    WRITE(lfnerr,*) ' *** SR blockret: different epochs'
    RETURN
  END IF

! Create the condition matrix H = (C' * C)^-1 * C'
! ------------------------------------------------
  ALLOCATE(Hmat(2,2*npolX), stat=iac)
  CALL alcerr( iac, 'Hmat', (/2,2*npolX/), 'blockret')
  ALLOCATE(regMat(2*npolX,2*npolX), stat=iac)
  CALL alcerr(iac, 'regMat', (/2*npolX,2*npolX/), 'blockret')

  DO ip = 1, 2*npolX-1, 2

    tArg = omega * 86400.D0 * ( neq%par(indp(ip))%time%mean - t0 )
    cwt  = COS(tArg)
    swt  = SIN(tArg)

    Hmat(1,ip  ) =  cwt / npolX    !     cwt / npolX
    Hmat(2,ip  ) = -swt / npolX    !    -swt / npolX
    Hmat(1,ip+1) =  swt / npolX    !    -swt / npolX
    Hmat(2,ip+1) =  cwt / npolX    !    -cwt / npolX

  END DO

! Compute the regularization matrix and add the weights
! -----------------------------------------------------
  weight = (opt%sigma0 / opt%blkRet)**2

  regMat = weight * MATMUL( TRANSPOSE(Hmat), Hmat )

  DO ip1 = 1, 2*npolX
    DO ip2 = ip1, 2*npolX
      neq%aNor( ikf( indp(ip1), indp(ip2) ) ) =  &
                    neq%aNor( ikf( indp(ip1), indp(ip2) ) ) + regMat(ip1,ip2)
    END DO
  END DO

  DEALLOCATE(regMat, stat=iac)
  DEALLOCATE(Hmat, stat=iac)
  DEALLOCATE(indp, stat=iac)

END SUBROUTINE blockret




END MODULE
