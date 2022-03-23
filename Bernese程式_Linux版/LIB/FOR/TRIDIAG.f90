MODULE s_TRIDIAG
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE tridiag(npar,diag,offdia,rhs,sol)

!--------------------------------------------------------------------------
!
! Purpose  :  SOLVE NORMAL EQUATION SYSTEM WITH
!             SYMMETRIC TRIDIAGONAL NEQ-MATRIX
!             (ALGORITHM: MODIFIED SR TRIDAG IN
!             NUMERICAL RECIPES, W.H. Press et al.,
!             1992, pp. 42, 43)
!
! Author   :  H. Bock and G.Beutler
!
! Created  :  10-Sep-1999
!
! Changes  :  25-Nov-2003 HB: Do no longer check for maxPar,
!                             allocate gamma with nPar
!             20-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
!
!---------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,  ONLY: i4b, r8b, lfnerr
  USE s_alcerr
  USE s_exitrc
  IMPLICIT NONE

! List of parameters
! ------------------
! IN:
  INTEGER(i4b) :: npar              ! Number of parameters

! OUT:
  REAL(r8b),DIMENSION(:) :: diag    ! Diagonal elements
  REAL(r8b),DIMENSION(:) :: offdia  ! Offdiagonal elements (nPar-1)
  REAL(r8b),DIMENSION(:) :: rhs     ! Right hand side
  REAL(r8b),DIMENSION(:) :: sol     ! Solution vector

! Local variables
! ---------------
  INTEGER(i4b) :: jj,ios
  REAL(r8b)    :: beta
  REAL(r8b),DIMENSION(:),ALLOCATABLE :: gamma

! Check for dimensions
! --------------------
  IF ( nPar > SIZE(diag) ) THEN
    write(lfnErr,'(/,A,I7,16X,A,16X,A,I7,/)')&
         ' *** SR TRIDIAG: Local dimension npar = ',nPar,&
                         ' is larger than size of parameters: ',&
                         ' SIZE(diag) = ',SIZE(diag)
  ENDIF

! Allocate gamma
! --------------
  ALLOCATE(gamma(nPar),stat=ios)
  CALL alcerr(ios,'gamma',(/nPar/),'tridiag')
!
! COMPUTE SOLUTION VECTOR
! -----------------------
  beta=diag(1)

  IF(beta == 0.D0)THEN
    WRITE(lfnerr,'(//,A,//)')&
         ' *** SR TRIDIAG: beta=0 FOR ELE 1'
    CALL exitrc(2)
  ENDIF

  sol(1)=rhs(1)/beta
  DO jj=2,npar
    gamma(jj)=offdia(jj-1)/beta
    beta=diag(jj)-offdia(jj-1)*gamma(jj)
    IF(beta == 0.D0)THEN
      WRITE(lfnerr,'(//,A,I7,//)')&
           ' *** SR TRIDIAG: beta=0 FOR ELE',jj
      CALL exitrc(2)
    ENDIF
    sol(jj)=(rhs(jj)-offdia(jj-1)*sol(jj-1))/beta
  ENDDO

  DO jj=npar-1,1,-1
    sol(jj)=sol(jj)-gamma(jj+1)*sol(jj+1)
  ENDDO

  DEALLOCATE(gamma)
  RETURN
END SUBROUTINE tridiag

END MODULE
