MODULE s_QZEROS
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE qzeros(isel,val,nevent,ievent,tevent,valext)

! -------------------------------------------------------------------------
! Purpose:    Determine zeros and extremas of a function given by
!             equidistant tabular values. Input are three consecutive
!             values, zeros and extremas are determined through a
!             quadratic polynomial.
!
! Remark:     Output events are chronologically ordered
!
! Author:     U. Hugentobler
!
! Created:    30-Dec-2001
! Last mod.:  13-Jan-2002
!
! Changes:    13-Jan-2002  HU: Determine extrema in interval (-2,0]
!
! SR used:    ---
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern

  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)               :: isel       ! =1: check interval (1...3]
                                           ! >1: check interval (2...3]
  REAL(r8b),DIMENSION(3)     :: val        ! Three consecutive values of
                                           ! tabular function
! output:
  INTEGER(i4b)               :: nevent     ! Number of events in interval
                                           ! between val(1) and val(3), 0...3
  INTEGER(i4b),DIMENSION(3)  :: ievent     ! Type of event, i=1,nevent
                                           ! =-1: zero + --> -
                                           ! =+1: zero - --> +
                                           ! =-2: minimum
                                           ! =+2: maximum
  REAL(r8b),DIMENSION(3)     :: tevent     ! epoch of event in units of
                                           ! tabular interval, relative to
                                           ! last epoch, i.e.,
                                           !   tevent=(-2...0] for isel=1
                                           !   tevent-(-1...0] for isel>1
                                           ! i=1...nevent,
  REAL(r8b)                  :: valext     ! extremal value (if any)

! Local Variables
! ---------------
  INTEGER(i4b)               :: iext,izero1,izero2
  REAL(r8b)                  :: a0,a1,a2,det,dtest
  REAL(r8b)                  :: text,tzero1,tzero2
  REAL(r8b)                  :: trel = 1D0

  nevent = 0
  valext = 0
  IF (isel == 1) THEN
    dtest=-1D0
  ELSE
    dtest=0
  ENDIF

! Prepare Coefficients
! --------------------
  a0  = val(2)
  a1  = (val(3)-val(1))/2
  a2  = (val(3)-2*val(2)+val(1))/2

! Linear Function
! ---------------
  IF (ABS(a2) <= EPSILON(a2) .AND. ABS(a1) > EPSILON(a1)) THEN
    tzero1= -a0/a1
    IF (tzero1 > dtest .AND. tzero1 <= +1) THEN
      IF (val(1) >= 0 .AND. val(3) <= 0D0) THEN
        nevent    =  1
        ievent(1) = -1
        tevent(1) = tzero1-trel
      ELSE
        nevent    =  1
        ievent(1) = +1
        tevent(1) = tzero1-trel
      ENDIF
    ENDIF

! Find Extremum
! -------------
  ELSEIF (ABS(a2) > EPSILON(a2)) THEN
    iext   = 0                                      !  extremum found
    text=-a1/(2*a2)
    IF (text > -1 .AND. text <= +1) THEN
      valext = a0+text*(a1+text*a2)
      iext   = 1
    ENDIF

! Find Roots
! ----------
    izero1 = 0          !  zero found + --> -
    izero2 = 0          !  zero found - --> +
    IF (iext /= 0 .OR. val(1)*val(3) <= 0D0) THEN
      det = a1**2-4*a0*a2
      IF (det >= 0D0) THEN
        det = SQRT(det)
        tzero1 = (-a1-det)/(2*a2)
        tzero2 = (-a1+det)/(2*a2)

        IF (tzero1 > dtest .AND. tzero1 <= 1D0) izero1 = 1
        IF (tzero2 > dtest .AND. tzero2 <= 1D0) izero2 = 1
      ENDIF
    ENDIF

! Prepare Output
! --------------
    IF (a2 > 0) THEN
      IF (izero1 == 1) THEN
        nevent         = nevent+1
        ievent(nevent) = -1
        tevent(nevent) = tzero1-trel
      ENDIF
      IF (iext == 1) THEN
        nevent         = nevent+1
        ievent(nevent) = -2
        tevent(nevent) = text-trel
      ENDIF
      IF (izero2 == 1) THEN
        nevent         = nevent+1
        ievent(nevent) = +1
        tevent(nevent) = tzero2-trel
      ENDIF
    ELSE
      IF (izero2 == 1) THEN
        nevent         = nevent+1
        ievent(nevent) = +1
        tevent(nevent) = tzero2-trel
      ENDIF
      IF (iext == 1) THEN
        nevent         = nevent+1
        ievent(nevent) = +2
        tevent(nevent) = text-trel
      ENDIF
      IF (izero1 == 1) THEN
        nevent         = nevent+1
        ievent(nevent) = -1
        tevent(nevent) = tzero1-trel
      ENDIF
    ENDIF
  ENDIF
  RETURN
END SUBROUTINE qzeros

END MODULE
