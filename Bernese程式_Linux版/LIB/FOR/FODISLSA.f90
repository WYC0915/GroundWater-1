MODULE s_FODISLSA
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE fodislsa(varCovInfoMode, lsa)

! -------------------------------------------------------------------------
! Purpose:    Compute Least Square Adjustment
!             - Inputs: lsa%... nnMjd,nnPar,A,y,iQyy(=P)
!             - varCovInfoMode = - 0 : no variance-coovariance info considered
!                                - 1 : only space variance considered
!                                - 2 : space variance-coovariance considered
!                                      ( of three dimensional coordinates).
!
! Computes:   - lsa%detN = det(At*iQyy*A) or det(At*P*A) or det(At*A)
!             - lsa%Qxx  = inv(At*iQyy*A) or inv(At*P*A) or inv(At*A)
!             - lsa%x = Qxx*At*iQyy*y or Qxx*At*P*y or Qxx*At*y
!             - lsa%mod = A*x
!             - lsa%v = y - mod
!             - lsa%vtv = vt*v
!             - lsa%m0 = sqrt(vt*iQyy*v/dof)
!
! Author:     Luca Ostini
!
! Created:    14-Aug-2008
!
! Changes:    14-Aug-2008 LO: Created this file
!             02-Oct-2008 LO: First revision
!             09-Oct-2008 LO: Third revision
!             05-Dec-2008 LO: Fourth revisio: velocity changes allowed
!             11-Feb-2009 LO: Fifth revision: major changes
!             25-Sep-2009 LO: Changes for F90 consistency
!             18-Nov-2009 LO: Major changes for consistency
!             16-Jun-2010 LO: Time series diveded by component
!             09-Aug-2010 RD: New syminvg used
!             26-Aug-2010 LO: Architectural changes
!             03-Jan-2010 LO: INTENT(INOUT) removed and bug fixed
!             19-Sep-2012 RD: Correctly deallocate arrays
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Used Modules
! ------------
! structures, variables:
  USE m_bern,    ONLY: i4b, r8b, shortLineLength
  USE p_fodits,  ONLY: t_lsa

! operator, methods:
!  USE d_debug,   ONLY: debug_entry, debug_exit

! subroutines, functions:
  USE f_ikf
  USE s_alcerr
  USE s_syminvg

! no implicit
  IMPLICIT NONE

! subroutine name
  CHARACTER(LEN=shortLineLength), PARAMETER    :: srName = 'fodislsa'


! List of Arguments
! -----------------
! input:
  INTEGER(i4b),INTENT(IN)        :: varCovInfoMode
  TYPE(t_lsa)                    :: lsa

! input/output:

! output:


! Local Types
! -----------


! Local Parameters
! ----------------


! Local Variables
! ---------------
  INTEGER(i4b)                   :: iac
  INTEGER(i4b)                   :: ret
  INTEGER(i4b)                   :: nParLin
  INTEGER(i4b)                   :: iMjd
  INTEGER(i4b)                   :: indM
  INTEGER(i4b)                   :: jPar
  INTEGER(i4b)                   :: iPar
  INTEGER(i4b)                   :: indP
  INTEGER(i4b)                   :: iiMjd

  REAL(r8b), DIMENSION(:,:),     &
             ALLOCATABLE         :: At
  REAL(r8b), DIMENSION(:,:),     &
             ALLOCATABLE         :: N
  REAL(r8b), DIMENSION(:),       &
             ALLOCATABLE         :: b
  REAL(r8b), DIMENSION(:),       &
             ALLOCATABLE         :: vt
  REAL(r8b), DIMENSION(:),       &
             ALLOCATABLE         :: linQxx


! Call debug routine
! ------------------
!  CALL debug_entry(srName)


! Initialization of all variables
! -------------------------------

  ! Deallocation
!  DEALLOCATE(At,stat=iac)
!  DEALLOCATE(N,stat=iac)
!  DEALLOCATE(b,stat=iac)
!  DEALLOCATE(vt,stat=iac)

  ! Transpose of the first design matrix A -> At
  ALLOCATE(At(lsa%nnPar,lsa%nnMjd),stat=iac)
  CALL alcerr(iac, 'At', (/lsa%nnPar,lsa%nnMjd/), srName)
  ALLOCATE(N(lsa%nnPar,lsa%nnPar),stat=iac)
  CALL alcerr(iac, 'N', (/lsa%nnPar,lsa%nnPar/), srName)
  ALLOCATE(b(lsa%nnPar),stat=iac)
  CALL alcerr(iac, 'b', (/lsa%nnPar/), srName)
  ALLOCATE(vt(lsa%nnMjd),stat=iac)
  CALL alcerr(iac, 'vt', (/lsa%nnMjd/), srName)

  ! Execute N = MATMUL(MATMUL(At,lsa%iQyy),lsa%A)
  At(:,:) = TRANSPOSE(lsa%A(:,:))
  IF( varCovInfoMode == 2 )THEN
     DO iMjd = 1,INT(lsa%nnMjd/3)
        indM = 3*(iMjd-1)
        DO iPar = 1,INT(lsa%nnPar/3)
           indP = 3*(iPar-1)
           At(indP+1:indP+3,indM+1:indM+3) = &
                MATMUL(At(indP+1:indP+3,indM+1:indM+3),&
                       lsa%iQyy(iMjd,1:3,1:3) )
        END DO
     END DO
  ELSE IF( varCovInfoMode == 1 )THEN
     DO iiMjd = 1,lsa%nnMjd
        At(:,iiMjd) = At(:,iiMjd) * lsa%P(iiMjd)
     END DO
  END IF
  N(:,:) = MATMUL(At(:,:),lsa%A(:,:))
  ! Execute b = MATMUL(MATMUL(At,lsa%iQyy),lsa%y)
  b(:) = MATMUL(At(:,:),lsa%y(:))

  ! Cofactor matrix Qxx
  lsa%Qxx(:,:) = N(:,:)

  ! Convert the 2-D Qxx matrix to 1-D for syminvg
  nParLin = ((lsa%nnPar+1)*lsa%nnPar)/2
  ALLOCATE(linQxx(nParLin),stat=iac)
  CALL alcerr(iac, 'linQxx', (/nParLin/), srName)
  DO iPar = 1,lsa%nnPar
     DO jPar = 1,iPar
        linQxx(ikf(jPar,iPar)) = lsa%Qxx(jPar,iPar)
     END DO
  END DO

  ! Matrix inversion
  CALL syminvg(lsa%nnPar,linQxx(:),0,ret)
  IF( ret > 0 ) THEN
     lsa%detN = 0.0D0
  ELSE
     lsa%detN = 1.0D0
  END IF

  ! Recompose the 1-D Qxx matrix to 2-D and store it into lsa%Qxx
  lsa%Qxx(:,:) = 0.0D0
  DO iPar = 1,lsa%nnPar
     DO jPar = 1,iPar
        lsa%Qxx(jPar,iPar) = linQxx(ikf(jPar,iPar))
        lsa%Qxx(iPar,jPar) = linQxx(ikf(jPar,iPar))
     END DO
  END DO
  DEALLOCATE(linQxx,stat=iac)

  ! Execute lsa%x = MATMUL(lsa%Qxx,b)
  lsa%x = MATMUL(lsa%Qxx,b)

  ! Execute Mod = A*x
  lsa%mod = MATMUL(lsa%A,lsa%x)

  ! Compute observed minus computed
  lsa%v = lsa%y - lsa%mod

  ! Estimated standard deviation of unit weight
  ! Execute lsa%m0 = SQRT(DOT_PRODUCT(MATMUL(lsa%v,lsa%iQyy),lsa%v)/lsa%dof)
  IF( varCovInfoMode == 2 )THEN
     DO iMjd = 1,INT(lsa%nnMjd/3)
        indM = 3*(iMjd-1)
        vt(indM+1:indM+3) = MATMUL(lsa%v(indM+1:indM+3),&
                                   lsa%iQyy(iMjd,1:3,1:3))
     END DO
  ELSE IF( varCovInfoMode == 1 )THEN
     lsa%m0 = 0.0D0
     DO iiMjd = 1,lsa%nnMjd
        vt(iiMjd) = lsa%v(iiMjd) * lsa%P(iiMjd)
     END DO
  ELSE
     vt(:) = lsa%v(:)
  END IF
  lsa%m0 = SQRT(DOT_PRODUCT(vt,lsa%v)/lsa%dof)

  ! Compute vt*v
  lsa%vtv = DOT_PRODUCT(vt(:),lsa%v(:))

  ! Deallocation
  DEALLOCATE(At,stat=iac)
  DEALLOCATE(N,stat=iac)
  DEALLOCATE(b,stat=iac)
  DEALLOCATE(vt,stat=iac)

! End of subroutine
! -----------------
!  CALL debug_exit(srName)
  RETURN

END SUBROUTINE fodislsa

END MODULE s_FODISLSA
