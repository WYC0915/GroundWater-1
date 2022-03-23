MODULE s_STATIS
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.2
! -------------------------------------------------------------------------

SUBROUTINE statis(n, x, xMed, xMean, xRms, xSigma, xMax, xMin)

! -------------------------------------------------------------------------
! Purpose:    Determine mean, median, min., max., rms and std.dev.
!             of the input data series
!
! Author:     T.A. Springer
!
! Created:    18-Jun-1996
!
! Changes:    25-Jun-1996 TS: Added median search
!             09-Sep-1998 TS: Only if n.gt.1
!             21-Oct-1998 TS: Dummy array for x because of sort
!             23-Jun-2005 MM: Implicit none and declarations added
!             01-Jul-2008 DT: Set xmed=x if only 1 input value
!             19-Jul-2010 SL: Tab characters removed
!             30-Jul-2012 RD: Conversion to F90, optional arguments
!             30-Jul-2012 RD: Add more statistical parameters
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b

  USE s_sort

  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)             :: n      ! Number of input values
  REAL(r8b), DIMENSION(n),  &
             INTENT(IN)    :: x      ! Input data series
! output
  REAL(r8b), OPTIONAL      :: xMed   ! Median of the input series
  REAL(r8b), OPTIONAL      :: xMean  ! Mean value of input series
  REAL(r8b), OPTIONAL      :: xRms   ! Rms of input series
  REAL(r8b), OPTIONAL      :: xSigma ! Std.dev. of input series
  REAL(r8b), OPTIONAL      :: xMax   ! Max. value from input series
  REAL(r8b), OPTIONAL      :: xMin   ! Min. value from input series


! Local Variables
! ---------------
  INTEGER(i4b)             :: i

  REAL(r8b)                :: sum, sum2
  REAL(r8b), DIMENSION(n)  :: xDum

! Init statistics results
! -----------------------
  IF (PRESENT(xMean))  xMean  = 0.D0
  IF (PRESENT(xRms))   xRms   = 0.D0
  IF (PRESENT(xSigma)) xSigma = 0.D0
  IF (PRESENT(xMed))   xMed   = 0.D0
  IF (PRESENT(xMax))   xMax   = 0.D0
  IF (PRESENT(xMin))   xMin   = 0.D0

! Not enough elements
! -------------------
  IF (n == 1) THEN
    IF (PRESENT(xMed))  xMed  = x(1)
    IF (PRESENT(xMean)) xMean = x(1)
    IF (PRESENT(xMax))  xMax  = x(1)
    IF (PRESENT(xMin))  xMin  = x(1)
  ENDIF
  IF ( n<=1 ) RETURN

! Compute sum and sum of squares
! ------------------------------
  sum  = 0.D0
  sum2 = 0.D0

  DO i = 1,n
    sum  = sum  + x(i)
    sum2 = sum2 + x(i)**2

    IF (PRESENT(xMax)) THEN
      IF ( x(i) > xMax .OR. xMax == 0d0) xMax = x(i)
    ENDIF

    IF (PRESENT(xMin)) THEN
      IF ( x(i) < xMin .OR. xMin == 0d0) xMin = x(i)
    ENDIF
  ENDDO

  IF (PRESENT(xMean)) xMean = sum/n
  IF (PRESENT(xRms))  xRms  = DSQRT(sum2/n)

  IF (PRESENT(xSigma)) THEN
    IF (sum2 > sum**2/DBLE(n)) xSigma = DSQRT((sum2-sum**2/n)/(n-1))
  ENDIF

! Median search
! -------------
  IF (PRESENT(xMed)) THEN
    xdum(1:n)=x(1:n)
    CALL sort(n,xdum)
    i=n/2
    IF (i*2 == n) THEN
      xMed=(xdum(i)+xdum(i+1))/2.d0
    ELSE
      xMed=xdum(i+1)
    ENDIF
  ENDIF

  RETURN

END SUBROUTINE statis

END MODULE
