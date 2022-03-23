MODULE s_NEQDELNO
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE neqDelNo(neq,numFil,orb)

! -------------------------------------------------------------------------
! Purpose:    Prepare the deletion (elimination) of parameter with no/less
!             observations by setting locq(1) to zero
!
! Author:     C. Urschl
!
! Created:    08-Oct-2002
!
! Changes:    26-Nov-2002 MM: Do not delete velocities
!             27-May-2003 CU: New SR prparlst, remove parlst
!             19-Sep-2003 MM: Do not delete orbit parameters
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             16-Apr-2009 DT: Remove velocity if coordinate is singular
!             06-May-2009 RD: Do not delete PCV parameters
!             03-Aug-2009 DT: Do not remove stoch. pulses (exc. manoever)
!             24-Oct-2010 RD: Keep the parameter type flag
!             27-Oct-2010 SL: use m_bern with ONLY
!             16-Feb-2012 RD: Solve "Out-of-bounds" problem in IF statement
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b
  USE m_global, ONLY: g_syssvn
  USE d_par,    ONLY: ispartyp, parType_linear
  USE d_neq,    ONLY: t_neq
  USE p_addneq, ONLY: t_orb, t_man, maxman, comstat

  USE f_ikf
  USE f_tstequiv
  USE s_prparlst
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_neq)                           :: neq    ! NEQ structure (see P_ADDNEQ)
  INTEGER(i4b)                          :: numFil ! File number involved
  TYPE(t_orb)                           :: orb    ! Orbit description structure

! Local Variables
! ---------------
  TYPE(t_man), DIMENSION(maxman)        :: man    ! List of manoevers

  INTEGER(i4b)                          :: ipar, ipar2
  INTEGER(i4b)                          :: nman, iman
  INTEGER(i4b)                          :: iPWL


! Create list of relevant manoevers
! ---------------------------------
  nman = 0
  DO iman = 1, orb%nman

    IF ( orb%man(iman)%timman < comstat%taecml(1,1,numFil) .OR. &
         orb%man(iman)%timman > comstat%taecml(2,1,numFil)     )  CYCLE

    nman = nman + 1
    man(nman)%satman = orb%man(iman)%satman
    man(nman)%timman = orb%man(iman)%timman

  END DO

  Loop_ipar: DO ipar = 1, neq%misc%npar

    ! Do not remove velocity parameter
    IF (neq%par(iPar)%locq(1) == 1 .AND. &
        neq%par(iPar)%locq(4) == 2) CYCLE Loop_ipar

    ! Do not remove orbit parameters
    IF (neq%par(iPar)%locq(1) ==  3) CYCLE Loop_ipar

    ! Do not remove PCV parameters (problems in PCVSTORE)
    IF (neq%par(iPar)%locq(1) == 18) CYCLE Loop_ipar

    IF (neq%par(iPar)%locq(1) == 25) THEN
      IF (g_syssvn(INT(neq%par(ipar)%locq(3)/100)) /= ' ' .AND. &
          neq%par(ipar)%locq(3) /= 0 ) CYCLE Loop_ipar
    ENDIF

    ! Do not remove stochastic orbit parameters
    IF (neq%par(iPar)%locq(1) == 11) THEN
      DO iman = 1, nman

        IF ( neq%par(iPar)%locq(3) /= man(iman)%satman    .AND. &
             neq%par(iPar)%locq(3) /= man(iman)%satman+50      )  CYCLE

        IF ( (neq%par(iPar)%time%mean > man(iman)%timman  .AND.       &
              neq%par(iPar)%locq(3) == man(iman)%satman+50     ) .OR. &
             (neq%par(iPar)%time%mean < man(iman)%timman  .AND.       &
              neq%par(iPar)%locq(3) == man(iman)%satman        )     ) CYCLE Loop_ipar

      END DO
    END IF


    IF (neq%aNor(ikf(ipar,ipar)) .lt. 1.d-16) THEN

      CALL prparlst(1,5,numFil,neq%par(iPar)%name,neq%par(iPar)%locq, &
                    neq%par(iPar)%time)

    ! Remove also velocity parameter if coordinate is singular
      IF ( neq%par(iPar)%locq(1) == 1 .AND. &
           neq%par(iPar)%locq(4) == 1) THEN

         DO ipar2=neq%misc%npar,ipar+1,-1
           IF ( tstequiv(neq,ipar,neq,ipar2) ) THEN

             CALL prparlst(1,5,numFil,neq%par(ipar2)%name,neq%par(ipar2)%locq, &
                           neq%par(ipar2)%time)

             neq%par(ipar2)%locq(1) = 0
             neq%misc%nparms        = neq%misc%nparms - 1

           END IF
         END DO

      END IF

      IF (ispartyp(neq%par(ipar),parType_linear)) THEN
        DO ipar2=1,neq%misc%npar
          IF ( .NOT. ispartyp(neq%par(ipar2),parType_linear)   ) CYCLE
          IF ( neq%par(ipar)%locq(1) /= neq%par(ipar2)%locq(1) ) CYCLE
          IF ( neq%par(ipar)%name    /= neq%par(ipar2)%name    ) CYCLE
          neq%par(ipar2)%type = parType_linear
        ENDDO
      ENDIF

      neq%par(ipar)%locq(1) = 0
      neq%misc%nparms       = neq%misc%nparms - 1

    ENDIF

  ENDDO Loop_ipar

END SUBROUTINE neqDelNo


END MODULE
