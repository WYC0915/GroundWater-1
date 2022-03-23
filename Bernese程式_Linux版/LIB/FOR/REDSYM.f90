MODULE s_REDSYM
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.0
! -------------------------------------------------------------------------

SUBROUTINE redsym(n,indred,q,nsing,parflg,sum_abs,rhs,bigajj_limit)

! -------------------------------------------------------------------------
! Purpose:    Pre-elininate parameters "i" with "indred(i)=1" (lines and columnbs)
!             of matrix "q". If the problem corresponds to a least squares problem,
!             the variable sum_abs contains the sum of the squares of the terms
!             "observed-a priori values",
!              - the reduced sums of the same terms (for further processing)
!              - after a full inverison inversion (see below) the weighted sum of
!             residuals squares.
!             If "indred(i)=1, i=1,2,...,n", the array rhs(i),i=1,2,...,n contains
!             after the inversion the negative of the solution vector of the linear,
!             symmetric system of eqns, i.e.
!             rhs(i)=-x(i), i=1,2,...,n, if the system is written as
!             q*x = rhs. Moreover, the full inverse matrix is available in this case
!             The algorithm is based on the inversion of a symmetric matrix with
!             pivot search on the diagonal. Algorithm acm 150, H. Rutishauser 1963
!             Change: upper triangle of matrix in vektor q(n*(n+1)/2)
!
! Author:     G. Beutler, H. Rutishauser, C. Urschl
!
! Created:    11-Aug-2006:
!
! Changes:    24-Nov_2007 GB: bigajj_limit introduced
!             26-Dec-2007 GB: SR add_dble added
!             08-Jan-2008 GB: Pre-eliminate parameters with indred(ipar) > 1 first (group2)
!             11-Jul-2012 RD: Special handling for diagonal elements with NaN
!             11-Jul-2012 RD: Remove unused variables and modules
!             11-Jul-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b
  USE s_alcerr

  IMPLICIT NONE

! List of Parameters
! ------------------
  INTEGER(i4b),INTENT(IN)        :: n       ! Number of parameters
  INTEGER(i4b),DIMENSION(:)      :: indred ! Index indicating elements to be pre-eliminated
  INTEGER(i4b),INTENT(OUT)       :: nsing   ! Number of singular parameters
  INTEGER(i4b),DIMENSION(:)      :: parflg  ! Flag for singular parameters
                                            ! =0: parameter not singular
                                            ! =1: parameter singular
  REAL(r8b),DIMENSION(:)         :: q       ! Upper triangle, columnwise
  REAL(r8b)                      :: sum_abs ! sum of squares of terms "o-c"
  REAL(r8b),DIMENSION(:)         :: rhs     ! Right-hand sides of eqn-system
  REAL(r8b)                      :: bigajj_limit ! singularity limit


! Local Variables
! ---------------
  CHARACTER(LEN=1), DIMENSION(n) :: ls1

  REAL(r8b)       , DIMENSION(n) :: hs1
  REAL(r8b)       , DIMENSION(n) :: hs2
  REAL(r8b)                      :: ajj
  REAL(r8b)                      :: bigajj,help_rhs,rhs_jjj
  REAL(r8b)                      :: bigajj_group2

  INTEGER(i4b)    , DIMENSION(n) :: parnum, parnum_t
  INTEGER(i4b)                   :: i, j, k, n_elim, n_t
  INTEGER(i4b)                   :: jj, jk, kj, kk, j1, kk_0,ll
  INTEGER(i4b)                   :: kp1, km1
  INTEGER(i4b)                   :: jkmax
  INTEGER(i4b)                   :: ising
  INTEGER(i4b)                   :: k_group2

! number of elements to be eliminated, array with parameter number numbers
  n_elim=0
  do i=1,n
    if(indred(i) /= 0)then
      n_elim=n_elim+1
      parnum(n_elim)=i
    endif
  enddo
  if(n_elim==0)return
! mark all columns fo matrix q as "un-processed"
  DO i = 1, n
    ls1(i)    = 'T'
    parflg(i) = 0
  ENDDO

  ising = 0
  nsing = 0
  jkmax = n * (n+1)/2

  Grand_loop: DO i = 1, n_elim

! Define current pivot element

! Search for pivot
! ----------------
    bigajj = 0.d0
    bigajj_group2=0.d0
    k_group2 = 0
    k      = 0
!
    DO j = 1, n_elim
      j1=parnum(j)
      jj=(j1+1)*j1/2
      ajj = DABS(q(jj))
      IF ( ls1(j1) == 'T' .AND. &
          (ajj >= bigajj .OR. k == 0) ) THEN
        bigajj = ajj
        k      = j1
      ENDIF
! look for largest pivot in group2
      IF ( ls1(j1) == 'T' .AND. indred(j1) > 1 .AND. &
          (ajj >= bigajj_group2.OR.k_group2 == 0) ) THEN
        bigajj_group2 = ajj
        k_group2      = j1
      ENDIF
    ENDDO
! eliminate paramters of group2 2 first
    IF(k_group2 > 0)THEN
      bigajj = bigajj_group2
      k      = k_group2
    ENDIF

! Remark: "bigajj /= bigajj" (I had no beer before: useful to detect NaN)
    IF (bigajj < bigajj_limit .OR. bigajj /= bigajj) THEN
      parflg(k) = 1
      ising     = i
      nsing     = nsing+1
    END IF

! Preparation of elimination step i
! ---------------------------------
    ls1(k) = 'F'
    kk     = k * (k+1)/2
!
! check whether element is singular
    IF (ising == 0) THEN
      hs2(k) = 1d0/q(kk)
    ELSE
      hs2(k) = 0d0
      ising  = 0
    ENDIF
    hs1(k) = 1d0
    q(kk)  = 0d0
    km1    = k-1
!
! reduce sum_abs
    help_rhs=hs2(k)*rhs(k)
    sum_abs = sum_abs - rhs(k)*help_rhs
!
! reduce right-hand sides
    do j=1,n
      if(j <=k)then
        jk=k*km1/2+j
      else
        jk=j*(j-1)/2+k
      endif

      rhs_jjj=q(jk)*help_rhs
      if(j/=k)then
          if(ls1(j) == 'T')then
            rhs(j)=rhs(j)-rhs_jjj
          else
            if(j<k)then
              rhs(j)=rhs(j)-rhs_jjj
            else
             rhs(j)=rhs(j)+rhs_jjj
            endif
          endif
      else
        rhs(k)=-help_rhs
      endif
    enddo
!
    IF (km1 >= 1) THEN
      jk = k * km1/2+1
      DO j = 1, km1
        hs1(j) = q(jk)
        hs2(j) = q(jk)
        IF (ls1(j) == 'T') hs2(j) = -q(jk)
        hs2(j) = hs2(j)*hs2(k)
        q(jk)  = 0d0
        jk     = jk+1
      ENDDO
    ENDIF

    kp1 = k + 1
    IF (kp1 <= n) THEN
      kj = kp1 * (kp1-1)/2+k
      DO j = kp1, n
        hs1(j) = -q(kj)
        IF (ls1(j) == 'T') hs1(j) = q(kj)
        hs2(j) = -q(kj)*hs2(k)
        q(kj)  = 0d0
        kj     = kj+j
      ENDDO
    ENDIF

! Elimination ok, define new matrix elements
! ------------------------------------------
!    if(n == n_elim)then
    if(n > 0)then
      j = 1
      ll = 1
      DO jk = 1, jkmax
        q(jk) = q(jk) + hs1(j)*hs2(ll)
        IF (j == ll) THEN
          ll = ll+1
          j = 1
        ELSE
          j = j+1
        ENDIF
      ENDDO
    else
      n_t=0
      do j=1,n
        if(ls1(j) == 'T')then
          n_t=n_t+1
          parnum_t(n_t)=j
        endif
      enddo

      do kk = 1,n_t
        k = parnum_t(kk)
        kk_0=(k-1)*k/2
        do jj=1,kk
          j=parnum_t(jj)
          jk=kk_0+j
          q(jk) = q(jk) + hs1(j)*hs2(k)
        enddo
      enddo
    endif

  ENDDO Grand_loop

  RETURN

END SUBROUTINE redsym

END MODULE
