MODULE f_istobeel
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

FUNCTION isToBeEl(par,ipart,ifil,delList)

! -------------------------------------------------------------------------
! Purpose:    This function decides if the specified parameter should be
!             pre-eliminated
!
! Author:     L. Mervart
!
! Created:    30-Aug-1998
!
! Changes:    19-Oct-2001 RD: "except of boundary" with a variable deltaT
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             12-Feb-2002 MM: new "except of files"-handling
!             29-Nov-2002 RD: except of file/boundaries if ipart == 1 only
!             09-Jul-2003 RD: Use staInfo instead of opt%crux
!             26-Feb-2004 RD: Preeliminate all parameters outside a time window
!             10-May-2005 RD: Use staInfo-problem via function isStacrx
!             22-Sep-2005 RD: Use new module D_PAR.f90
!             14-Aug-2009 SL: Elimination request in STAINFO only for ipart=3
!             04-Mar-2010 RD: Add optional statistics record
!             29-Nov-2011 SL: m_bern with ONLY
!             26-Jun-2012 RD: Check availability of parameters
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnerr
  USE m_time,   ONLY: OPERATOR(.IsIn.)
  USE d_par,    ONLY: t_par, maxlcq
  USE p_addneq, ONLY: opt, comstat
  USE f_isstacrx
  IMPLICIT NONE

! List of Parameters
! ------------------
! input
  TYPE(t_par)                            :: par
  INTEGER(i4b), INTENT(IN)               :: ipart
  INTEGER(i4b), INTENT(IN)               :: ifil

! input/output
  INTEGER(i4b), DIMENSION(:,:), OPTIONAL :: delList

! output
  LOGICAL                                :: isToBeEl

! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER            :: srName = 'ISTOBEEL'

! Local Variables
! ---------------
  INTEGER(i4b) :: ii
  INTEGER(i4b) :: ieli
  INTEGER(i4b) :: ilcq
  REAL(r8b)    :: tbeg
  REAL(r8b)    :: tend
  REAL(r8b)    :: dt

  isToBeEl = .FALSE.

! Elimination Requests in STAINFO File
! ------------------------------------
!!!  IF ( ipart == 0 .OR. ipart == 1 ) &
  IF ( ipart == 3 ) THEN
    IF (PRESENT(delList)) THEN
      isToBeEl = isToBeEl .OR. isStacrx(par,delList)
    ELSE
      isToBeEl = isToBeEl .OR. isStacrx(par)
    ENDIF
  ELSE

! Elimination Request in Input File
! ---------------------------------
    ieli_loop: DO ieli = 1, SIZE(opt%elimi)

      IF (ipart /= 999 .AND. opt%elimi(ieli)%mode == -1) CYCLE

      isToBeEl = .FALSE.

      ! Check the part
      IF ( ipart /= 0 .AND. opt%elimi(ieli)%part /= ipart ) CYCLE ieli_loop
      IF ( ipart == 0 .AND. opt%elimi(ieli)%part == 999   ) CYCLE ieli_loop

      ! Check the locq
      DO ilcq = 1, maxLcq
        IF ( par%locq(ilcq)  /= opt%elimi(ieli)%locq(ilcq) .AND. &
             opt%elimi(ieli)%locq(ilcq) /= 0 ) CYCLE ieli_loop
      ENDDO

      ! Check the name
      IF ( par%name  /= opt%elimi(ieli)%name .AND. &
           opt%elimi(ieli)%name /= '' ) CYCLE ieli_loop

      ! Exceptions
      IF (ipart == 1) THEN

        ! Exception of some files
        ! -----------------------
        IF (opt%elimi(ieli)%mode == 4 .OR. opt%elimi(ieli)%mode == 5) THEN
          DO ii = 1, SIZE(opt%elimi(ieli)%excp)
            IF (opt%elimi(ieli)%excp(ii) == ifil) CYCLE ieli_loop
          ENDDO
        ENDIF

        ! Exception of the file boundaries
        ! --------------------------------
        IF (opt%elimi(ieli)%mode == 3 .OR. opt%elimi(ieli)%mode == 5) THEN
          tbeg = par%time%mean - par%time%half;
          tend = par%time%mean + par%time%half;
          dt   = opt%elimi(ieli)%deltaT / 86400d0
          IF ( tbeg - comstat%taecml(1,1,ifil) <= dt .OR. &
               comstat%taecml(2,1,ifil) - tend <= dt  ) CYCLE ieli_loop
        ENDIF

        ! Outside a time window
        ! ---------------------
        IF (opt%elimi(ieli)%mode == 6) THEN
          tbeg = par%time%mean - par%time%half;
          tend = par%time%mean + par%time%half;
          IF ( tbeg <= opt%elimi(ieli)%eliwin%t(2) .AND. &
               tend >= opt%elimi(ieli)%eliwin%t(1) ) CYCLE ieli_loop
        ENDIF
      ENDIF

      ! Indicate that the entry got a hit
      ! ---------------------------------
      opt%elimi(ieli)%wParam = .TRUE.

      ! Give a message in case of wrong input option regarding the parameter
      ! --------------------------------------------------------------------
      IF (LEN_TRIM(opt%elimi(ieli)%descr) > 0 .AND. &
          opt%elimi(ieli)%part == 999 .AND. &
          opt%elimi(ieli)%mode == 999 ) THEN
        WRITE(lfnerr,'(/,A,2(/,18X,A),/)') ' *** SR ISTOBEEL: ' //         &
             'Parameter of type "' // TRIM(opt%elimi(ieli)%descr) // '" was',&
             'found in the input NEQs but there is an empty field',          &
             'in "Parameter Pre-Elimination" panel.'
        opt%elimi(ieli)%descr = ''
      ENDIF

      ! Define the result
      IF ( ipart /= 999 ) THEN
        isToBeEl = .TRUE.
      ELSE
        isToBeEl = opt%elimi(ieli)%mode == 999
      ENDIF
      EXIT ieli_loop

    ENDDO ieli_loop

  ENDIF

END FUNCTION isToBeEl

END MODULE
