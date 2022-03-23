MODULE s_ADD_ROT
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE add_rot(neq)

! -------------------------------------------------------------------------
! Purpose:    This subroutine introduces 3 additional rotation-parameters
!             and removes their influence on the normal-equation-system
!
! Author:     D. Thaller
!
! Created:    09-Apr-2001
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

! Local Variables
! ---------------
  REAL(r8b), DIMENSION(3,3)               :: Bmat1
  REAL(r8b), DIMENSION(3,3)               :: Bmat2
  REAL(r8b), DIMENSION(3,3)               :: Nmat1
  REAL(r8b), DIMENSION(3,3)               :: Nmat2
  REAL(r8b), DIMENSION(:,:), ALLOCATABLE  :: BTN
  REAL(r8b), DIMENSION(3,3)               :: BTNB
  REAL(r8b), DIMENSION(3,3)               :: regMat
  REAL(r8b), DIMENSION(3)                 :: lvek
  REAL(r8b)                               :: X1, Y1, Z1, X2, Y2, Z2

  INTEGER(i4b), DIMENSION(:), ALLOCATABLE :: indsta
  INTEGER(i4b)                            :: nstat, ista1, ista2
  INTEGER(i4b)                            :: ipar, ipar1, ipar2
  INTEGER(i4b)                            :: ip1, ip2, i1, i2
  INTEGER(i4b)                            :: ii,iac

! Count number of stations and allocate memory
! --------------------------------------------
  nstat = 0
  DO ipar = 1, neq%misc%npar
    IF ( neq%par(ipar)%locq(1) == 1  .AND. &
         neq%par(ipar)%locq(3) == 1         ) THEN
      nstat = nstat + 1
    ENDIF
  ENDDO

  ALLOCATE (BTN(3,3*nstat), stat=iac)
  CALL alcerr(iac, 'BTN', (/3,3*nstat/), 'add_rot')
  BTN = 0.d0

  regMat = 0.d0
  Bmat1 = 0.d0
  Bmat2 = 0.d0
  Nmat1 = 0.d0
  Nmat2 = 0.d0
  BTNB  = 0.d0
  lvek  = 0.d0

! Create station index
! --------------------
  ALLOCATE (indsta(nstat), stat=iac)
  CALL alcerr(iac, 'indsta', (/nstat/), 'add_rot')
  nstat = 0
  DO ipar = 1, neq%misc%npar
    IF ( neq%par(ipar)%locq(1) == 1 .AND. &
         neq%par(ipar)%locq(3) == 1        ) THEN
      nstat = nstat + 1
      indsta(nstat) = ipar
    ENDIF
  ENDDO


! Create the Matrices B'N and B'NB
! --------------------------------
  DO ista1 = 1, nstat
    ip1 = indsta(ista1)
    X1  = neq%par(ip1  )%x0
    Y1  = neq%par(ip1+1)%x0
    Z1  = neq%par(ip1+2)%x0

    Bmat1(1,:) = (/ 0.d0,  Z1,  -Y1  /)
    Bmat1(2,:) = (/ -Z1,  0.d0,  X1  /)
    Bmat1(3,:) = (/  Y1,  -X1,  0.d0 /)

    DO ista2 = 1, nstat
      ip2 = indsta(ista2)
      X2  = neq%par(ip2  )%x0
      Y2  = neq%par(ip2+1)%x0
      Z2  = neq%par(ip2+2)%x0

      Nmat1(1,:) = (/ neq%aNor(ikf(ip1,  ip2  )),  &
                      neq%aNor(ikf(ip1,  ip2+1)),  &
                      neq%aNor(ikf(ip1,  ip2+2))  /)
      Nmat1(2,:) = (/ neq%aNor(ikf(ip1+1,ip2  )),  &
                      neq%aNor(ikf(ip1+1,ip2+1)),  &
                      neq%aNor(ikf(ip1+1,ip2+2))  /)
      Nmat1(3,:) = (/ neq%aNor(ikf(ip1+2,ip2  )),  &
                      neq%aNor(ikf(ip1+2,ip2+1)),  &
                      neq%aNor(ikf(ip1+2,ip2+2))  /)

      Nmat2 = MATMUL( Bmat1, Nmat1 )

      BTN(:, ip2:ip2+2) = BTN(:, ip2:ip2+2) + Nmat2

      Bmat2(1,:) = (/ 0.d0, -Z2,   Y2  /)
      Bmat2(2,:) = (/ Z2,   0.d0, -X2  /)
      Bmat2(3,:) = (/ -Y2,  X2,   0.d0 /)

      BTNB = BTNB + MATMUL( BTN(:,ip2:ip2+2) , Bmat2 )

    ENDDO
  ENDDO

! compute the normal-equation-system without the influence of
!  the 3 rotation-parameters
! -----------------------------------------------------------
  ii = matinv(BTNB, 3)

  DO ista1 = 1, nstat
    ip1 = indsta(ista1)

    DO ista2 = 1, nstat
      ip2 = indsta(ista2)

      regMat = MATMUL( TRANSPOSE(BTN(:,ip1:ip1+2)),    &
                       MATMUL( BTNB, BTN(:,ip2:ip2+2) ) )

      lvek(:) = neq%xxx(ip2:ip2+2)

      neq%bNor(ip1:ip1+2) = neq%bNor(ip1:ip1+2) - MATMUL( regMat, lvek )

      DO i1 = 1, 3
        ipar1 = ip1 + i1 - 1
        DO i2 = 1, 3
          ipar2 = ip2 + i2 - 1

          IF ( ipar1 > ipar2 )  CYCLE

          neq%aNor(ikf(ipar1,ipar2)) = neq%aNor(ikf(ipar1,ipar2)) &
                                                  - regMat(i1,i2)

        ENDDO
      ENDDO

    ENDDO
  ENDDO

  DEALLOCATE (BTN,    stat=iac)
  DEALLOCATE (indsta, stat=iac)

END SUBROUTINE add_rot

END MODULE
