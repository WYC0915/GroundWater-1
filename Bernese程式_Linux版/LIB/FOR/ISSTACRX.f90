MODULE f_isstacrx
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

FUNCTION isStacrx(par,delList)

! -------------------------------------------------------------------------
! Purpose:    This function decides if the specified parameter should be
!             pre-eliminated befause of "station problem" in the STAINFO file
!
! Author:     R.Dach
!
! Created:    10-May-2005
!
! Changes:    22-Sep-2005 RD: Use new module D_PAR.f90
!             13-Feb-2008 RD: Adapt to par%name == chr*20
!             14-Aug-2009 SL: staProblem to staInfo
!             04-Mar-2010 RD: Add optional statistics record
!             26-Oct-2010 RD: Modify the definition of bad parameters
!             26-Oct-2010 SL: use m_bern with ONLY, removal of unused params
!             24-Nov-2011 SS/SL: speed optimization, old logic to switch on/off
!             28-Nov-2011 RD: Add a tolerance (1d-4 sec)
!             17-Oct-2012 SS: Do not touch parameters to be deleted
!             18-Oct-2012 SS: Exceptions concerning high-end monitoring parameters
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, staNameLength
  USE m_time,   ONLY: OPERATOR(.isIn.)
  USE d_par,    ONLY: t_par, isParTyp, &
                      parType_linear, parType_linearMiddlePoint,  &
                      parType_linearLeftPoint, parType_linearRightPoint
  USE p_addneq, ONLY: staInfo, opt
  IMPLICIT NONE

! List of Parameters
! ------------------
! input
  TYPE(t_par)                              :: par

! input/output
  INTEGER(i4b), DIMENSION(:,:), OPTIONAL   :: delList

! output
  LOGICAL                                  :: isStaCrx

! List of functions
! -----------------

! Local types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER              :: srName = 'ISSTACRX'
  REAL(r8b),        PARAMETER              :: d = 1d-4/86400d0

! Local Variables
! ---------------
  INTEGER(i4b)                             :: ieli,i
  REAL(r8b), DIMENSION(2)                  :: parTim
  LOGICAL                                  :: old = .FALSE.     ! use old logic

! Elimination Requests in STAINFO File
! ------------------------------------
  isStaCrx = .FALSE.

  DO ieli = 1, staInfo%nProb

    IF(old) THEN
      IF(par%name(1:staNameLength) == staInfo%staProb(ieli)%stanam .AND. &
         (par%time%mean .isIn. staInfo%staProb(ieli)%timint)) THEN
        isStaCrx = .TRUE.
      ENDIF
    ELSE
      IF(par%name(1:staNameLength) /= staInfo%staProb(ieli)%stanam) CYCLE
      IF(isParTyp(par,parType_linear)) THEN
        IF(isParTyp(par,parType_linearMiddlePoint) .AND. &
          (par%time%mean+par%omega-d < staInfo%staProb(ieli)%timint%t(1) .OR. &
           par%time%mean-par%omega+d > staInfo%staProb(ieli)%timint%t(2))) CYCLE
        IF(isParTyp(par,parType_linearLeftPoint) .AND. &
          (par%time%mean+par%omega-d < staInfo%staProb(ieli)%timint%t(1) .OR. &
           par%time%mean          +d >= staInfo%staProb(ieli)%timint%t(2))) CYCLE
        IF(isParTyp(par,parType_linearRightPoint) .AND. &
          (par%time%mean          -d <= staInfo%staProb(ieli)%timint%t(1) .OR. &
           par%time%mean-par%omega+d > staInfo%staProb(ieli)%timint%t(2))) CYCLE
      ELSE
        parTim(1) = par%time%mean-par%time%half+d
        IF(parTim(1) > staInfo%staProb(ieli)%timint%t(2)) CYCLE
        parTim(2) = par%time%mean+par%time%half-d
        IF(parTim(2) < staInfo%staProb(ieli)%timint%t(1)) CYCLE
      ENDIF
      isStaCrx = .TRUE.
    ENDIF

! Do not touch parameters to be deleted
! -------------------------------------
    IF (isStaCrx .AND. ASSOCIATED(opt%elimi)) THEN
      DO i=1,SIZE(opt%elimi)
        IF (opt%elimi(i)%locq(1) /= par%locq(1)) CYCLE
        IF (opt%elimi(i)%mode == -1) isStaCrx = .FALSE.

! Exceptions concerning high-end monitoring parameters
! ----------------------------------------------------
! - GRD
        IF (par%locq(1) == 22) isStaCrx = .FALSE.
! - HOI
        IF (par%locq(1) == 27) isStaCrx = .FALSE.
! - GSP
        IF (par%locq(1) == 30) isStaCrx = .FALSE.
      ENDDO
    ENDIF

    IF(isStaCrx .AND. PRESENT(delList)) THEN
      delList(iEli,par%locq(1)) = delList(iEli,par%locq(1)) + 1
    ENDIF

    EXIT

  ENDDO

END FUNCTION isstacrx

END MODULE
