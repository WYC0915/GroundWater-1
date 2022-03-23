MODULE s_QXXLEO
CONTAINS

! ------------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! ------------------------------------------------------------------------------

SUBROUTINE QxxLEO(NPAEPO,NPAR12,NPAR,                                       &
           &      ATMP,ATMP12,ANOR,INDE12,MAXPAE,QXX)

! ------------------------------------------------------------------------------
!
!  NAME       :  QxxLEO
!
!  PURPOSE    :  To compute VAR/COV of epoch positions/position differences with
!                respect to Cartesian Earth fixed XYZ and Leo body fixed system
!
!  SR CALLED  :  IKF
!
!
!  REMARKS    :
!
!
!  AUTHOR     :  D.Svehla, M.Rothacher
!
!
!  CREATED    :  24-Nov-01                       LAST MODIFIED : 28-Jun-05
!
!  CHANGES    :  28-Jun-2005 MM: Unused variables removed
!
!
!  COPYRIGHT  :  ASTRONOMICAL INSTITUTE
!                 UNIVERSITY OF BERN
!                     SWITZERLAND
! ------------------------------------------------------------------------------


  USE m_bern
  USE f_ikf
  IMPLICIT NONE

! Parameters
! ----------
!
! IN :
! ----
  INTEGER(i4b)             :: NPAEPO  ! Total number of parameters (N22)
  INTEGER(i4b)             :: NPAR12  ! Total number of parameters (N12)
  INTEGER(i4b)             :: NPAR    ! Total number of parameters (N11)
  REAL(r8b),DIMENSION(*)   :: ATMP    ! N22 matrix of neq. system, inverted
  REAL(r8b),DIMENSION(*)   :: ATMP12  ! N12 matrix of neq. system
                                      ! dimension NPARN*MAXAMP (per session)
  REAL(r8b),DIMENSION(*)   :: ANOR    ! I=1,..,NPAR*(NPAR+1)/2,
                                      ! inverted normal equat. matrix (Q11)
  INTEGER(i4b),DIMENSION(*):: INDE12  ! Aux. array for parameter list (N12)
  INTEGER(i4b)             :: MAXPAE  ! Maximum number of epoch parameters

!
! OUT :
! -----
  REAL(r8b),DIMENSION(MAXPAE,*)       :: QXX    ! VAR/COV matrix of epoch positions
                                                ! (upper triangle, linearized)
                                                ! in XYZ system
!  REAL(r8b),DIMENSION(*)             :: Qacr   ! VAR/COV matrix of epoch positions
!                                               ! (upper triangle, linearized)
!                                               ! in  Leo body fixed system
!  REAL(r8b),DIMENSION(*)             :: QDxx   ! VAR/COV matrix of epoch position diff.
!                                               ! (upper triangle, linearized)
!                                               ! in XYZ system
!  REAL(r8b),DIMENSION(*)             :: QDacr  ! VAR/COV matrix of epoch position diff.
!                                               ! (upper triangle, linearized)
!                                               ! in Leo body fixed system


! Local variables
! ---------------
  REAL(r8b),DIMENSION(NPAEPO*NPAR)    :: N21Q11
  REAL(r8b),DIMENSION(NPAEPO*NPAEPO)  :: NQN
  REAL(r8b),DIMENSION(NPAEPO,NPAEPO)  :: N22I,NQNM
  REAL(r8b)                           :: NQNij,ATMPij

  INTEGER(i4b)                        :: j,ipar,iparI,iparK,KI,K,I,KPAR
!
! Compute N21Q11=N21*Q11*N12
! --------------------------
    DO ipar=1,NPAEPO
      DO I=1,NPAR12
        iparI=NPAEPO*(I-1)+ipar
        N21Q11(iparI)=0.D0
          DO K=1,NPAR12
              iparK=NPAR12*(ipar-1)+K
              KPAR=IKF(INDE12(K),INDE12(I))
              N21Q11(iparI)=N21Q11(iparI)+                   &
                            ATMP12(iparK)*ANOR(KPAR)
          END DO
      END DO
    END DO
    DO ipar=1,NPAEPO
      DO I=1,NPAEPO
        iparI=NPAEPO*(I-1)+ipar
        NQN(iparI)=0.D0
        DO K=1,NPAR12
            iparK=NPAEPO*(K-1)+ipar
            KI=NPAR12*(I-1)+K
            NQN(iparI)=NQN(iparI)+                        &
                       N21Q11(iparK)*ATMP12(KI)
        END DO
      END DO
    END DO
!        -1     -1              -1
! Q22=N22 +  N22*N21*Q11*N12*N22
! --------------------------------------
  DO i=1,NPAEPO
    DO j=1,i
      ATMPij=ATMP(IKF(i,j))
      N22I(i,j)=ATMPij
      N22I(j,i)=ATMPij
    END DO
  END DO
  DO i=1,NPAEPO
    DO j=1,i
      iparI=NPAEPO*(j-1)+i
      NQNij=NQN(iparI)
      NQNM(i,j)=NQNij
      NQNM(j,i)=NQNij
    END DO
  END DO
!
  QXX(1:NPAEPO,1:NPAEPO)=N22I(:,:)+matmul(matmul(N22I,NQNM),N22I)
!
  RETURN
  END SUBROUTINE

END MODULE
