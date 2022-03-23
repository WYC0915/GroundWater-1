MODULE s_SYMINVG
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE syminvg(n,q,norm,nsing,parflg,sbrName,bigajj_limit,utest)

! -------------------------------------------------------------------------
! Purpose:    Inversion symm. matrix with pivot search on the diagonal.
!             Algorithm acm 150, H. Rutishauser 1963
!             Change: upper triangle of matrix in vektor q(n*(n+1)/2)
!
! Author:     H. Rutishauser, C. Urschl
!
! Created:    24-Jan-2003
!
! Changes:    09-Aug-2010 RD: Generic use, only one version for all purposes
!             27-Oct-2010 SL: use m_bern with ONLY
!             23-May-2011 RD: Special handling for diagonal elements with NaN
!             30-Jul-2012 RD: Evaluate the inversion (only for GRP_AIUB)
!             22-Sep-2012 RD: Change variable name "unit" to "utest"
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnErr

  USE s_matcop
  USE s_statis
  USE f_ikf

  IMPLICIT NONE

! List of Parameters
! ------------------
  INTEGER(i4b),INTENT(IN)        :: n       ! Number of parameters
  REAL(r8b),DIMENSION(n*(n+1)/2) :: q       ! Upper trianle, columnwise
  INTEGER(i4b)                   :: norm    ! 1: normalize the diagonale of q
  INTEGER(i4b),INTENT(OUT)       :: nsing   ! Number of singular parameters
  INTEGER(i4b),DIMENSION(n), &
               OPTIONAL          :: parflg  ! Flag for singular parameters
                                            ! =0: parameter not singular
                                            ! =1: parameter singular
  CHARACTER(LEN=*), OPTIONAL     :: sbrName ! Name of calling routine to enable
                                            ! reporting of singularities
  REAL(r8b),        OPTIONAL     :: bigajj_limit  ! limit for singulatiy detection
  CHARACTER(LEN=*), OPTIONAL     :: utest    ! Name of calling routine to check
                                            ! the numerical performance


! Local Variables
! ---------------
  CHARACTER(LEN=1), DIMENSION(n) :: ls1

  REAL(r8b),        DIMENSION(n) :: hs1
  REAL(r8b),        DIMENSION(n) :: hs2
  REAL(r8b),        DIMENSION(n) :: scl_tot
  REAL(r8b)                      :: xRms1,xRms0
  REAL(r8b)                      :: xMin1,xMin0
  REAL(r8b)                      :: xMax1,xMax0
  REAL(r8b),        POINTER,      &
                    DIMENSION(:) :: q0
  REAL(r8b)                      :: ajj
  REAL(r8b)                      :: bigajj
  REAL(r8b)                      :: limit = 1.0d-10 ! limit to detect a singular
                                                    ! parameter

  INTEGER(i4b)                   :: i, j, k
  INTEGER(i4b)                   :: ij, jj, jk, kj, kk
  INTEGER(i4b)                   :: kp1, km1
  INTEGER(i4b)                   :: jkmax
  INTEGER(i4b)                   :: ising

  LOGICAL                        :: prtFlg
  LOGICAL                        :: isSing
  LOGICAL                        :: doNorm = .TRUE.

! Overwrite the limitation of singular parameters
! if the optional parameter is set
  IF (PRESENT(bigajj_limit)) THEN
    limit = bigajj_limit
  ENDIF

! Write or not write warning message
  prtFlg = (PRESENT(sbrName))

! Make a copy of the input matrix
! -------------------------------
  NULLIFY(q0)
#ifdef GRP_AIUB
  IF (PRESENT(utest)) THEN
!! Print matrix "q" to screen formatted for MATLBA
!!    write(*,'(A)') '>>q0<< q =  [ ...'
!!    DO i = 1,n
!!      DO j = 1,n-1
!!        WRITE(*,'(A,E30.20,A)') '>>q0<< ',q(IKF(i,j)),' ...'
!!      ENDDO
!!      IF ( i < n ) THEN
!!        WRITE(*,'(A,E30.20)') '>>q0<< ',q(IKF(i,j))
!!      ELSE
!!        WRITE(*,'(A,E30.20,A)') '>>q0<< ',q(IKF(i,j)),' ]'
!!      ENDIF
!!    ENDDO
!! End of printing section
    CALL matcop(1,IKF(n,n),q,q0)
  ENDIF
#endif


! Scale matrix q
! --------------
  IF (norm == 2) doNorm = .FALSE.  ! To supress normalization in specific calls
                                   ! (To support numerical experiments)
  IF (norm == 1 .OR. doNorm) THEN
    DO jj = 1, n
      jk = ikf(jj,jj)
      IF ( q(jk) > 0.d0 ) THEN
        scl_tot(jj) = 1.d0 / DSQRT(q(jk))
      ELSE
        scl_tot(jj) = 0.d0
      ENDIF
    ENDDO

    jk = 0
    DO jj = 1,n
      DO kk = 1, jj-1
        jk = jk + 1
        q(jk) = q(jk) * scl_tot(jj) * scl_tot(kk)
      ENDDO
      jk = jk + 1
      IF ( scl_tot(jj) == 0d0 ) THEN
        q(jk) = 0d0
      ELSE
        q(jk) = 1d0
      ENDIF
    ENDDO
  ELSE
    scl_tot = 1d0
  ENDIF

! Initialisaition for inversion
  DO i = 1, n
    ls1(i)    = 'T'
    IF (PRESENT(parFlg)) parflg(i) = 0
  ENDDO

  ising = 0
  nsing = 0
  jkmax = n * (n+1)/2

  Grand_loop: DO i = 1, n

! Search for pivot
! ----------------
    bigajj = 0d0
    jj     = 1
    k      = 0

    ! Start with the biggest element (independent from the normalization)
    ! Remark: otherwise we may start in GPSEST with an ambiguity; may become
    !         problematic in case of single-difference GLONASS ambiguities
    IF ( i == 1 ) THEN
      DO j = 1, n
        IF ( scl_tot(j) /= 0d0 ) THEN
          ajj = DABS(q(jj))/(scl_tot(j))**2
          IF ( ajj >= bigajj .OR. k == 0 ) THEN
            bigajj = ajj
            k      = j
          ENDIF
        ENDIF
        jj = jj+j+1
      ENDDO
      IF ( k == 0 ) k = 1  ! To make sure to have at least one element
      bigajj = DABS(q(IKF(k,k)))

    ! Search the biggest remaining element
    ELSE
      DO j = 1, n
        ajj = DABS(q(jj))
        IF ( ls1(j) == 'T' .AND. &
            (ajj >= bigajj .OR. k == 0)) THEN
          bigajj = ajj
          k      = j
        ENDIF
        jj = jj+j+1
      ENDDO
    ENDIF

    isSing = .FALSE.
    ! Remark on "bigajj /= bigajj":
    ! I had no beer before: useful to detect NaN
    IF (bigajj /= bigajj) THEN
      isSing = .TRUE.
    ELSEIF ( scl_tot(k) == 0d0) THEN
      isSing = .TRUE.
    ELSEIF (bigajj/(scl_tot(k))**2 < limit) THEN
      isSing = .TRUE.
    ENDIF

    IF (isSing) THEN
      IF (PRESENT(parFlg)) parflg(k) = 1
      ising     = i
      nsing     = nsing+1
      IF (prtFlg) THEN
        prtFlg = .FALSE.
        WRITE (lfnerr,'(/,A,/,17X,A,/)') &
              ' ### SR SYMINVG: matrix had to be regularized ', &
                               'called from SR ' // TRIM(sbrName)
      END IF
    END IF

! Preparation of elimination step i
! ---------------------------------
    ls1(k) = 'F'
    kk     = k * (k+1)/2
    IF (ising == 0) THEN
      hs2(k) = 1d0/q(kk)
    ELSE
      hs2(k) = 0d0
      ising  = 0
    ENDIF
    hs1(k) = 1d0
    q(kk)  = 0d0
    km1    = k-1

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

! Elimination proper
! ------------------
    j = 1
    k = 1
    DO jk = 1, jkmax
      q(jk) = q(jk) + hs1(j)*hs2(k)
      IF (j == k) THEN
        k = k+1
        j = 1
      ELSE
        j = j+1
      ENDIF
    ENDDO

  ENDDO Grand_loop

! Re-scale matrix q
! -----------------
  IF (norm == 1 .OR. doNorm) THEN
    jk = 0
    DO jj = 1,n
      DO kk = 1, jj
        jk = jk + 1
        q(jk) = q(jk) * scl_tot(jj) * scl_tot(kk)
      ENDDO
    ENDDO
  ENDIF

! Check product of input and output for the deviation from an unit matrix
! -----------------------------------------------------------------------
#ifdef GRP_AIUB
  IF (PRESENT(utest)) THEN
    DO i = 1,n ! Columns
      DO j = 1,n ! Copy column into hs1
        hs1(j) = q0(IKF(i,j))
      ENDDO
      DO j = 1,i
        ij = IKF(i,j)
        q0(ij) = 0d0
        DO jj = 1,n
          q0(ij) = q0(ij) + q(IKF(j,jj)) * hs1(jj)
        ENDDO
      ENDDO
    ENDDO

! Copy diagonal elements into hs1
! -------------------------------
    DO j = 1,n
      jj = IKF(j,j)
      IF ( scl_tot(j) /= 0d0 ) q0(jj) = q0(jj) - 1d0
      hs1(j) = q0(jj)
    ENDDO

! Report the statistics on the resulting unit matrix
! --------------------------------------------------
    CALL statis(n,hs1,xRms = xrms1, xMin = xmin1, xMax = xmax1)
    CALL statis(IKF(n,n),q0,xRms = xrms0, xMin = xmin0, xMax = xmax0)
    WRITE(*,'(/,A,3(/,17X,A),3(/,17X,3A,2D15.5),/)') &
    ' ### SR SYMINVG: Statistics on capability to recover the unit matrix ',  &
                     'from the product of the input and inverted matrix.',    &
                     'Statistics on the deviation from a unit matrix:',       &
                     'Calling routine           Diagonal      All elements',  &
                     'SYMINVG - ',TRIM(utest),' RMS: ',xRms1,xRms0,            &
                     'SYMINVG - ',TRIM(utest),' Min: ',xMin1,xMin0,            &
                     'SYMINVG - ',TRIM(utest),' Max: ',xMax1,xMax0

    CALL matcop(-1,IKF(n,n),q,q0)
  ENDIF
#endif

  RETURN

END SUBROUTINE syminvg

END MODULE
