MODULE s_NEQSTACK
  USE m_time,   ONLY: OPERATOR(+)
CONTAINS

! -------------------------------------------------------------------------
! Bernese GNSS Software Version 5.2
! -------------------------------------------------------------------------

SUBROUTINE neqstack(neq,neq_1,ifil)

! -------------------------------------------------------------------------
! Purpose:    This subroutine stacks two NEQ systems
!
! Author:     L. Mervart
!
! Created:    22-Nov-1997
!
! Changes:    25-Sep-2001 LM: correct time interval for crd
!             11-Oct-2001 HU: correct time interval for gcc
!             18-Oct-2001 RD: generate a parameter list for printing
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             19-Feb-2001 SS: Adjust time windows for parameter types
!                             8 and 12
!             27-May-2003 CU: New SR prparlst, remove parlst
!             07-Apr-2005 RD: Update parameter time interval for sap (type 25)
!             23-May-2005 HU: Correct stacking of parameters in same NEQ
!             28-Jul-2005 RD: Put the OPERATOR(+)-import on the top
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             12-Dec-2005 CU: Update npseu, npseuel
!             25-Jan-2008 RD: Add RAO/RAP parameters
!             28-May-2008 DT: Update time interval for locq(1)=2
!             15-Jun-2008 RD: Merge technique flags
!             23-Sep-2008 DT: Set locq if dynamic orbit parameters not stacked
!             04-May-2009 RD: Scaling of loading models added
!             10-May-2009 RD: Receiver clock offsets/biases implemented
!             10-Jun-2009 RD: Add epoch satellite/receiver clocks
!             13-Aug-2009 DT: Update time interval and locq for range biases
!             04-Jan-2010 SL: Update time interval for type 27 (HOI) added
!             24-Oct-2010 RD: Distinguish between piece-wise linear param.
!             25-Oct-2010 SL: integer to real conversion bug corrected
!             27-Oct-2010 SL: use m_bern with ONLY
!             30-Nov-2010 DT: Update time interval and par for Helmert
!             15-Dec-2010 MM: Update time interval for type 30 (GSP) added
!             20-Jul-2011 LP: Satellite-specific obstypes
!             08-Feb-2012 RD: Solve "out-of-bound" problem in "IF"-statement
!             08-Nov-2012 LP: Initialize indobst_1
!             16-Jul-2013 RD: Reset observation counter for clock parameter
!
! SR used:    neqckdim, updmisc, prparlst
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b
  USE m_time,   ONLY: t_timint
  USE d_par,    ONLY: merge_techn, &
                      isParTyp, parType_linear, parType_linearMiddlePoint
  USE d_neq,    ONLY: t_neq
  USE p_addneq, ONLY: opt

  USE f_ikf
  USE s_prparlst
  USE s_neqckdim
  USE f_tstequiv
  USE s_updmisc
  USE s_gobsdef, ONLY: tstobst
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_neq)   :: neq       ! Resulting NEQ-system
  TYPE(t_neq)   :: neq_1     ! Input NEQ-system
  INTEGER(i4b)  :: ifil

! Local Variables
! ---------------
  INTEGER(i4b),DIMENSION(neq_1%misc%npar) :: crsp
  INTEGER(i4b) :: npar_new, npar_old, ii, kk, i_1, k_1
  INTEGER(i4b) :: ipar, ipar_1, partyp, partyp_1
  INTEGER(i4b) :: maxisb
  INTEGER(i4b) :: indobst_1, indobst_new

  integer(i4b), SAVE :: norb

  CHARACTER(LEN=10)  :: helpstr

  TYPE(t_timint) :: timint
  TYPE(t_timint) :: timint_1
  TYPE(t_timint) :: timint_new

! Initialize number of parameters
! -------------------------------
  npar_old = neq%misc%npar
  npar_new = neq%misc%npar

  maxisb = 0
  DO ipar_1 = 1, neq_1%misc%npar
    IF (neq_1%par(ipar_1)%locq(1) == 2 .AND. neq_1%par(ipar_1)%locq(6) == 5) &
      maxisb = MAX(maxisb,neq_1%par(ipar_1)%locq(7))
  ENDDO
  DO ipar = 1, neq%misc%npar
    IF (neq%par(ipar)%locq(1) == 2 .AND. neq%par(ipar)%locq(6) == 5) &
      maxisb = MAX(maxisb,neq%par(ipar)%locq(7))
  ENDDO

  crsp = 0

  IF ( ifil==1 ) norb = 0

! Find the corresponding parameters
! ---------------------------------
  LOOP_ipar_1: DO ipar_1 = 1, neq_1%misc%npar

    IF ( neq_1%par(ipar_1)%locq(1) == 0 ) CYCLE LOOP_ipar_1

    LOOP_ipar: DO ipar = 1, neq%misc%npar

      partyp   = neq%par(ipar)%locq(1)
      parTyp_1 = neq_1%par(ipar_1)%locq(1)

      IF ( partyp /= partyp_1 ) CYCLE LOOP_ipar

      IF ( tstequiv(neq,ipar,neq_1,ipar_1) ) THEN
        crsp(ipar_1)  = ipar

        IF (partyp == 2 .AND. neq%par(ipar)%locq(6) == 5 .AND. &
            neq_1%par(ipar_1)%locq(6) == 5) THEN
          IF (neq_1%par(ipar_1)%locq(5) < 0 .AND. neq%par(ipar)%locq(5) > 0) &
            neq_1%par(ipar_1)%locq(5)=-neq_1%par(ipar_1)%locq(5)
          IF (neq_1%par(ipar_1)%locq(5) > 0 .AND. neq%par(ipar)%locq(5) < 0) &
            neq%par(ipar)%locq(5)=-neq%par(ipar)%locq(5)
        ENDIF

        CYCLE LOOP_ipar_1
      END IF

    END DO LOOP_ipar

! New parameter (will result in expansion of the system)
! ------------------------------------------------------
    npar_new     = npar_new + 1
    crsp(ipar_1) = npar_new


! Orbital parameters not stacked
! ------------------------------
    IF ( opt%splitDyn == 1 .AND. neq_1%par(ipar_1)%locq(1) == 3 ) THEN
      norb = norb + 1
      neq_1%par(ipar_1)%locq(6) = norb
    END IF


! Check and change the dimension
! ------------------------------
    CALL neqckdim(neq,npar_new)

    neq%par(npar_new) = neq_1%par(ipar_1)

    ! Number of observations per parameter
    IF (neq%par(npar_new)%locq(1) == 23 .OR. &
        neq%par(npar_new)%locq(1) == 24) THEN
      neq%par(npar_new)%locq(6) = 0
    ENDIF


! Update sat-specific obstype info
! --------------------------------
    helpstr = "          "
    indobst_1 = 0
    IF ((neq%par(npar_new)%locq(1)==8).OR.(neq%par(npar_new)%locq(1)==12)) &
      indobst_1 = neq_1%par(ipar_1)%locq(7)
    IF (neq%par(npar_new)%locq(1)==25) &
      READ(neq_1%par(ipar_1)%name,'(A10,I5)') helpstr,indobst_1

    indobst_new = 0
    IF (indobst_1.NE.0) THEN
      CALL tstobst(neq%misc,neq_1%misc%obst(indobst_1),indobst_new)
      IF ((neq%par(npar_new)%locq(1)==8).OR.(neq%par(npar_new)%locq(1)==12)) &
         neq%par(npar_new)%locq(7) = indobst_new
      IF (neq%par(npar_new)%locq(1)==25) &
         WRITE(neq%par(npar_new)%name,"(A10,I5)") helpstr,indobst_new
    ENDIF




    CALL prparlst(1,1,iFil,neq%par(iPar)%name,neq%par(iPar)%locq, &
                  neq%par(iPar)%time)

  END DO LOOP_ipar_1

! Update neq%aNor matrix, neq%bNor vector, and neq%par structure
! --------------------------------------------------------------
  DO i_1 = 1, neq_1%misc%npar
    ii = crsp(i_1)

    IF (ii == 0) CYCLE

    neq%bNor(ii) = neq%bNor(ii) + neq_1%bNor(i_1)

! Merge piece-wise linear identifiers
! -----------------------------------
    IF (isParTyp(neq%par(ii),parType_linear)    .AND. &
        isParTyp(neq_1%par(i_1),parType_linear) .AND. &
        neq%par(ii)%type /= neq_1%par(i_1)%type) THEN
      neq%par(ii)%type = parType_linearMiddlePoint
    ENDIF

! Update Time Interval
! --------------------
    IF (neq%par(ii)%locq(1) ==  1 .OR. &
       (neq%par(ii)%locq(1) ==  2 .AND. neq%par(ii)%locq(6) /= 5).OR. &
        neq%par(ii)%locq(1) ==  5 .OR. &
        neq%par(ii)%locq(1) ==  8 .OR. &
        neq%par(ii)%locq(1) == 12 .OR. &
        neq%par(ii)%locq(1) == 16 .OR. &
        neq%par(ii)%locq(1) == 18 .OR. &
        neq%par(ii)%locq(1) == 22 .OR. &
        neq%par(ii)%locq(1) == 25 .OR. &
        neq%par(ii)%locq(1) == 26 .OR. &
        neq%par(ii)%locq(1) == 27 .OR. &
        neq%par(ii)%locq(1) == 28 .OR. &
        neq%par(ii)%locq(1) == 30) THEN
      timint%t(1) = neq%par(ii)%time%mean - neq%par(ii)%time%half
      timint%t(2) = neq%par(ii)%time%mean + neq%par(ii)%time%half
      timint_1%t(1) = neq_1%par(i_1)%time%mean - neq_1%par(i_1)%time%half
      timint_1%t(2) = neq_1%par(i_1)%time%mean + neq_1%par(i_1)%time%half
      timint_new = timint + timint_1
      neq%par(ii)%time%mean = (timint_new%t(2) + timint_new%t(1)) / 2.0
      neq%par(ii)%time%half = (timint_new%t(2) - timint_new%t(1)) / 2.0
    END IF

    ! Keep the mixed information for receiver clock biases
    IF (neq%par(ii)%locq(1) == 2 .AND. &
        neq%par(ii)%locq(7) /= neq_1%par(i_1)%locq(7)) THEN
      neq%par(ii)%locq(7) = 1
    ENDIF

    ! Update number of observations per parameter
    IF (neq%par(ii)%locq(1) == 23 .OR. &
        neq%par(ii)%locq(1) == 24) THEN
      neq%par(ii)%locq(6) = neq%par(ii)%locq(6) + neq_1%par(i_1)%locq(6)
    ENDIF

    DO k_1 = i_1, neq_1%misc%npar
      kk = crsp(k_1)
      IF (kk == 0 ) CYCLE
      neq%aNor(ikf(ii,kk)) = neq%aNor(ikf(ii,kk)) + neq_1%aNor(ikf(i_1,k_1))
      IF (ii == kk .AND. i_1 /= k_1) &
        neq%aNor(ikf(ii,kk)) = neq%aNor(ikf(ii,kk)) + neq_1%aNor(ikf(i_1,k_1))
    END DO

    CALL merge_techn(neq%par(ii),neq_1%par(i_1))
    neq%par(ii)%obsTim = neq%par(ii)%obsTim + neq_1%par(i_1)%obsTim

    ! Update par for Helmert parameters if stacking all
    ! -------------------------------------------------
    IF ( neq%par(ii)%locq(1) == 28 ) THEN
      IF ( opt%helmert(neq%par(ii)%locq(2))%stack == 2 ) THEN
        neq%par(ipar)%name = 'ALL'
      ENDIF
    END IF

  END DO

  neq%misc%nparms  = neq%misc%nparms   + (npar_new - npar_old) + &
                     neq_1%misc%nparms - neq_1%misc%npar

  neq%misc%npseu   = neq%misc%npseu   + neq_1%misc%npseu
  neq%misc%npseuel = neq%misc%npseuel + neq_1%misc%npseuel

  CALL updmisc(neq,neq_1,ifil)

  DO ipar = 1, neq%misc%npar
    IF (neq%par(ipar)%locq(1) == 2 .AND. neq%par(ipar)%locq(6) == 5) &
      neq%par(ipar)%locq(7) = maxisb
  ENDDO

! Update locq for orbits if not stacked
! -------------------------------------
  IF ( opt%splitDyn == 1 .AND. ifil == SIZE(opt%neqFileName) ) THEN
    DO ipar=1, neq%misc%npar
      IF ( neq%par(ipar)%locq(1) /= 3 ) CYCLE
      neq%par(ipar)%locq(5) = norb
    END DO
  END IF

! Update locq for range biases if stacking
! ----------------------------------------
  IF ( opt%stackRGB == 3 ) THEN
    DO ipar=1, neq%misc%npar
      IF ( neq%par(ipar)%locq(1) /= 26 ) CYCLE
      neq%par(ipar)%locq(5) = 1000
    END DO
  END IF

  IF ( opt%stackRGB == 2 ) THEN
    DO ipar=1, neq%misc%npar
      IF ( neq%par(ipar)%locq(1) /= 26 ) CYCLE
      IF ( neq%par(ipar)%locq(5) < 0 ) CYCLE
      neq%par(ipar)%locq(5) = -(1 + NINT(neq%par(ipar)%locq(5) / 100d0))
    END DO
  END IF

END SUBROUTINE neqstack

END MODULE
