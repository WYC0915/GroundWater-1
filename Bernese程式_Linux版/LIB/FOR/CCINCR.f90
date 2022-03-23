MODULE s_CCINCR
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE ccincr(A,P,Y,Idx,new)

! -------------------------------------------------------------------------
! Purpose:    Increases matrices for SR CCOMBO
!
! Parameters:
!    in/out : A   : Design matrix                            i4b(:,:)
!             P   : Diagonal matrix with apriori weights     r8b(:)
!             Y   : Observation vector                       r8b(:)
!             Idx : Index for observations                  t_index(:)
!             new : new size (possible # of obs.)            i4b
!
!
! Author:     R. Dach
!
! Created:    15-Feb-2001
! Last mod.:  09-Feb-2004
!
! Changes:    21-Dec-2001  HU: Use m_bern, ONLY for modules
!             23-Apr-2003  CU: Nullify local pointers
!             09-Feb-2004  RD: No more overwriting when an outlier in CCOMBO
!
! SR used:    alcerr
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

!
  USE m_bern
  USE p_ccrnxc, ONLY: t_index
  USE s_alcerr
  IMPLICIT NONE
!
! Variables from parameter list
! -----------------------------
  TYPE(t_index), DIMENSION(:) , POINTER    &
                                 :: Idx,Idx1   ! Index for the observations
  INTEGER(i4b), DIMENSION(:,:), POINTER    &
                                 :: A, A1      ! Disgn matrix
  REAL(r8b)   , DIMENSION(:)  , POINTER    &
                                 :: P, P1      ! Diagonal matrix with apriori weights
  REAL(r8b)   , DIMENSION(:)  , POINTER    &
                                 :: Y, Y1      ! Observation vector
  INTEGER(i4b) :: new                          ! new size of the matrices
!*****************************************************************************
  INTEGER(i4b) :: iold, jold                   ! old size of the matrices
  INTEGER(i4b) :: ios                          ! io status
!
  NULLIFY(Idx1)
  NULLIFY(A1)
  NULLIFY(P1)
  NULLIFY(Y1)
!
! Get old size of the matrices
! ----------------------------
  iold=SIZE(Y)
  jold=SIZE(A,1)
!
! Increase design matrix: A
! -------------------------
  ALLOCATE(A1(jold,iold),stat=ios)
  CALL alcerr(ios,'A1',(/jold,iold/),'ccincr')
!
  A1(:,:)=A(:,:)
!
  DEALLOCATE(A,stat=ios)
  ALLOCATE(A(jold,new),stat=ios)
  CALL alcerr(ios,'A',(/jold,new/),'ccincr')
!
  A(:,:)=0d0
  A(1:jold,1:iold)=A1(:,:)
!
  DEALLOCATE(A1,stat=ios)
!
!
! Increase weight matrix: P
! -------------------------
  ALLOCATE(P1(iold),stat=ios)
  CALL alcerr(ios,'P1',(/iold/),'ccincr')
!
  P1(:)=P(:)
!
  DEALLOCATE(P,stat=ios)
  ALLOCATE(P(new),stat=ios)
  CALL alcerr(ios,'P',(/new/),'ccincr')
!
  P(:)=0d0
  P(1:iold)=P1(:)
!
  DEALLOCATE(P1,stat=ios)
!
!
! Increase observation vector: Y
! ------------------------------
  ALLOCATE(Y1(iold),stat=ios)
  CALL alcerr(ios,'Y1',(/iold/),'ccincr')
!
  Y1(:)=Y(:)
!
  DEALLOCATE(Y,stat=ios)
  ALLOCATE(Y(new),stat=ios)
  CALL alcerr(ios,'Y',(/new/),'ccincr')
!
  Y(:)=0d0
  Y(1:iold)=Y1(:)
!
  DEALLOCATE(Y1,stat=ios)
!
!
! Increase observation index: Idx
! -------------------------------
  ALLOCATE(Idx1(iold),stat=ios)
  CALL alcerr(ios,'Idx1',(/iold/),'ccincr')
!
  Idx1(:)=Idx(:)
!
  DEALLOCATE(Idx,stat=ios)
  ALLOCATE(Idx(new),stat=ios)
  CALL alcerr(ios,'Idx',(/new/),'ccincr')
!
  Idx(:)%iFil(1)= 0
  Idx(:)%iFil(2)= 0
  Idx(:)%iEpo   = 0
  Idx(:)%ClkNam = ''
  Idx(:)%ClkTyp = 0
  Idx(1:iold)=Idx1(:)
!
  DEALLOCATE(Idx1,stat=ios)
!
  RETURN
  END SUBROUTINE

END MODULE
