MODULE s_NEQELIM
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE neqElim(neq,numFil)

! -------------------------------------------------------------------------
! Purpose:    Prepare the deletion (elimination) of parameter by
!             setting locq(1) to zero
!
! Author:     L. Mervart
!
! Created:    08-Aug-2001
!
! Changes:    18-Oct-2001 RD: generate parameter list for printing
!             19-Oct-2001 RD: bugfix delete not only the last requ.
!             19-Oct-2001 RD: check also the name of the parameter
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             03-Oct-2002 CU: Decrease nparms if parameters eliminated
!             27-May-2003 CU: New SR prparlst, remove parlst
!             10-May-2005 RD: No not delete "sta-crux" parameters
!             22-Sep-2005 RD: Use new modules D_NEQ.f90 and D_PAR.f90
!             06-May-2009 RD: New options for satellite antennas
!             10-May-2009 RD: Receiver clock offsets/biases implemented
!             10-Jun-2009 RD: Add epoch satellite/receiver clocks
!             14-Aug-2009 SL: isStaCrx removed
!             05-May-2011 RD/LP: remove pulses for GALILEO due to few obs.
!             06-Oct-2011 RD: Delete pulses within an interval
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b
  USE m_time,   ONLY: OPERATOR(.isIn.),t_timint
  USE m_global, ONLY: g_syssvn,g_svnsys
  USE m_maxdim, ONLY: maxbad
  USE d_par,    ONLY: maxLcq
  USE d_neq,    ONLY: t_neq
  USE d_satcrx, ONLY: gtsatd
  USE p_addneq, ONLY: opt

  USE s_prparlst
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_neq)                           :: neq    ! NEQ structure (see P_ADDNEQ)
  INTEGER(i4b)                          :: numFil ! File number involved

! Local Parameters
! ----------------
  CHARACTER(LEN=7), PARAMETER           :: srName = 'neqelim'

! Local Variables
! ---------------
  LOGICAL, SAVE :: first = .TRUE.

  INTEGER(i4b),                      SAVE :: nDel
  INTEGER(i4b),   DIMENSION(maxbad), SAVE :: plsdel
  TYPE(t_timint), DIMENSION(maxbad), SAVE :: timdel

  INTEGER(i4b) :: ipar,jpar
  INTEGER(i4b) :: idel
  INTEGER(i4b) :: ieli
  INTEGER(i4b) :: ilcq
  LOGICAL      :: flag


  IF (first) THEN
    CALL gtsatd(maxbad,opt%satcrux,ndel,plsdel,timdel)
    first = .FALSE.
  ENDIF

  DO ipar = 1, neq%misc%npar

    ! Delete pulses for GALILEO due to few obs. in GGSP and OVF campaigns
    IF ((neq%par(ipar)%locq(1) == 11).AND.  &
        (INT(neq%par(ipar)%locq(3)/100) == 2)) THEN
      IF (numFil > 0) &
        CALL prparlst(1,4,numFil,neq%par(iPar)%name,neq%par(iPar)%locq, &
                      neq%par(iPar)%time)
      neq%par(ipar)%locq(1) = 0
      neq%misc%nparms       = neq%misc%nparms - 1
      CYCLE
    ENDIF

    ! Delete GPS satellites for satellite specific receiver clock biases
    IF ( opt%rcoRmGPS + opt%rcoToFrq /= 0 .AND.   &
         neq%par(ipar)%locq(1) == 2         .AND. &
        (( ABS(neq%par(ipar)%locq(6)) == 1  .AND. &
           neq%par(ipar)%locq(7) == 0 )     .OR.  &
         ( ABS(neq%par(ipar)%locq(6)) == 2  .AND. &
           neq%par(ipar)%locq(7) < 100 )) )   THEN
      IF (numFil > 0) &
        CALL prparlst(1,4,numFil,neq%par(iPar)%name,neq%par(iPar)%locq, &
                      neq%par(iPar)%time)
      neq%par(ipar)%locq(1) = 0
      neq%misc%nparms       = neq%misc%nparms - 1
      CYCLE
    ENDIF


    ! Delete satellite antenna offsets on special request
    IF (neq%par(ipar)%locq(1) == 12) THEN
      IF ( ( opt%saoOpt(1) == -1       .AND. &
             neq%par(ipar)%locq(4) == 0 .AND. &
             g_syssvn(INT(neq%par(ipar)%locq(5)/100)) == 'G') .OR. &
           ( opt%saoOpt(2) == -1       .AND. &
             neq%par(ipar)%locq(4) == 0 .AND. &
             g_syssvn(INT(neq%par(ipar)%locq(5)/100)) == 'R') .OR. &
           ( opt%saoOpt(1) == -1       .AND. &
             neq%par(ipar)%locq(4) /= 0 .AND. &
             g_svnsys(INT(neq%par(ipar)%locq(5)/100)) == 'G') .OR. &
           ( opt%saoOpt(2) == -1       .AND. &
             neq%par(ipar)%locq(4) /= 0 .AND. &
             g_svnsys(INT(neq%par(ipar)%locq(5)/100)) == 'R') ) THEN
        IF (numFil > 0) &
          CALL prparlst(1,4,numFil,neq%par(iPar)%name,neq%par(iPar)%locq, &
                        neq%par(iPar)%time)
        neq%par(ipar)%locq(1) = 0
        neq%misc%nparms       = neq%misc%nparms - 1
        CYCLE
      ENDIF
    ENDIF

    ! Delete satellite antenna pattern on special request
    IF (neq%par(ipar)%locq(1) == 25) THEN
      IF ( ( opt%sapOpt(1) == -1       .AND. &
             neq%par(ipar)%locq(2) == 0 .AND. &
             g_syssvn(INT(neq%par(ipar)%locq(3)/100)) == 'G') .OR. &
           ( opt%sapOpt(2) == -1       .AND. &
             neq%par(ipar)%locq(2) == 0 .AND. &
             g_syssvn(INT(neq%par(ipar)%locq(3)/100)) == 'R') .OR. &
           ( opt%sapOpt(1) == -1       .AND. &
             neq%par(ipar)%locq(2) /= 0 .AND. &
             g_svnsys(INT(neq%par(ipar)%locq(3)/100)) == 'G') .OR. &
           ( opt%sapOpt(2) == -1       .AND. &
             neq%par(ipar)%locq(2) /= 0 .AND. &
             g_svnsys(INT(neq%par(ipar)%locq(3)/100)) == 'R') ) THEN
        IF (numFil > 0) &
          CALL prparlst(1,4,numFil,neq%par(iPar)%name,neq%par(iPar)%locq, &
                        neq%par(iPar)%time)
        neq%par(ipar)%locq(1) = 0
        neq%misc%nparms       = neq%misc%nparms - 1
        CYCLE
      ENDIF
    ENDIF

    ! Delete pulses on request
    IF ( neq%par(ipar)%locq(1) == 11 ) THEN
      DO iDel = 1,nDel
        IF ( (neq%par(iPar)%locq(3) == plsDel(iDel))         .AND. &
             (neq%par(iPar)%time%mean .isIn. timDel(iDel)) ) THEN
          IF (numFil > 0) &
            CALL prparlst(1,4,numFil,neq%par(iPar)%name,neq%par(iPar)%locq, &
                          neq%par(iPar)%time)
          neq%par(ipar)%locq(1) = 0
          neq%misc%nparms       = neq%misc%nparms - 1
          EXIT
        ENDIF
      ENDDO
      IF (neq%par(ipar)%locq(1) == 0) CYCLE
    ENDIF

    ! Delete parameters following the generic specification
    DO ieli = 1, SIZE(opt%elimi)
      IF (opt%elimi(ieli)%mode == -1) THEN
        flag = .TRUE.
        DO ilcq = 1, maxLcq
          IF ( neq%par(ipar)%locq(ilcq)   /= opt%elimi(ieli)%locq(ilcq) .AND. &
               opt%elimi(ieli)%locq(ilcq) /= 0 ) flag = .FALSE.
        END DO
        IF ( neq%par(iPar)%name   /= opt%elimi(ieli)%name .AND. &
             opt%elimi(ieli)%name /= '' )        flag = .FALSE.

        ! If a clock is deleted, no constraints for any clock of
        ! this epoch are allowed
        IF (flag .AND. &
            (neq%par(ipar)%locq(1) == 23 .OR. neq%par(ipar)%locq(1) == 24)) THEN
          DO jpar=1, neq%misc%npar
            IF (neq%par(ipar)%time%mean /= neq%par(jpar)%time%mean) CYCLE
            IF (neq%par(jpar)%locq(1) /= 23 .AND. &
                neq%par(jpar)%locq(1) /= 24) CYCLE
            IF (neq%par(jpar)%locq(4) ==  1) EXIT ! Epoch already flagged
            neq%par(jpar)%locq(4) = 1
          ENDDO
        ENDIF

        IF (flag) THEN
          IF (numFil > 0) &
            CALL prparlst(1,4,numFil,neq%par(iPar)%name,neq%par(iPar)%locq, &
                          neq%par(iPar)%time)
          neq%par(ipar)%locq(1) = 0
          neq%misc%nparms       = neq%misc%nparms - 1

          EXIT
        ENDIF
      END IF
    END DO

  END DO

END SUBROUTINE neqElim


END MODULE
