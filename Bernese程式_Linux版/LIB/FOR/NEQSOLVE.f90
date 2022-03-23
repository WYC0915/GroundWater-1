MODULE s_NEQSOLVE
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE neqsolve(neq,ipart,ifil,statist,hlmOut)

! -------------------------------------------------------------------------
! Purpose:    This subroutine computes the solution vector and a posteriori
!             rms error of unit weight.
!
! Author:     L. Mervart
!
! Created:    22-Nov-1997
! Last mod.:  02-Dec-2010
!
! Changes:    26-Jun-2001 RD: Use alcerr for allocation
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             24-Jan-2003 CU: Use new SR SYMINVG instead of SYMINV
!             04-Feb-2003 SS: Perform some checks concerning
!                             computation of rms of unit weight
!             27-May-2003 CU: New SR prparlst
!             04-Jun-2003 MM: Set aNor, bNor, xxx to 0.d0 for singular
!                             parameters
!             26-Jun-2003 MM: aNor now correctly set to 0.d0
!             23-May-2005 SS: Set rms error of unit weight to a priori
!                             value specified, if necessary
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             12-Dec-2005 CU: Print statistics for individual solutions,
!                             add ifil, npseu
!             27-Feb-2006 CU: Do not print statistics for indiv. sol.,
!                             save it in variable "statist"
!             08-Feb-2007 RD: misc%nObs/nParms i4b->r8b
!             18-May-2010 MF: Deallocate neq%xxx before reallocation
!             09-Aug-2010 RD: New syminvg used
!             02-Dec-2010 DT: Prepare output for WGT file (hlmOut)
!             14-Dec-2010 RD: Indicate part=3 in NEQSOLVE for repeatability
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_neq,    ONLY: t_neq
  USE d_datum,  ONLY: datum
  USE p_addneq, ONLY: comstat, opt, t_hlmFil, hlmFil

  USE f_ikf
  USE s_solve
  USE s_prparlst
  USE s_syminvg
  USE s_alcerr
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_neq)                            :: neq
  TYPE(t_hlmFil)                         :: hlmOut
  INTEGER(i4b), DIMENSION(neq%misc%npar) :: parlst
  INTEGER(i4b)                           :: ipart
  INTEGER(i4b)                           :: ifil
  REAL(r8b),    DIMENSION(9)             :: statist

! Local Parameters
! ----------------
  CHARACTER(LEN=7),PARAMETER             :: srName = 'neqsolve'

! Local Variables
! ---------------
  INTEGER(i4b)                       :: nsing
  INTEGER(i4b)                       :: iac
  INTEGER(i4b)                       :: ipar
  INTEGER(i4b)                       :: ityp
  INTEGER(i4b)                       :: jfil

  REAL(r8b)                          :: dof
  REAL(r8b)                          :: vTPv

! Initialization
! --------------
  IF ( ifil == 0 .AND. ipart == 2 ) THEN
    NULLIFY(hlmOut%ihelm)
    NULLIFY(hlmOut%rhelm)
    NULLIFY(hlmOut%indgrp)
    NULLIFY(hlmOut%grpnam)
    NULLIFY(hlmOut%fact)
  END IF


  IF ( ASSOCIATED(neq%xxx) ) DEALLOCATE( neq%xxx, stat=iac )
  ALLOCATE(neq%xxx(neq%misc%npar), stat=iac)
  CALL alcerr(iac, 'neq%xxx', (/neq%misc%npar/), 'neqsolve')

  CALL syminvg(neq%misc%npar,neq%aNor(:),0,nsing,parlst)
  CALL solve(neq%misc%npar,neq%aNor(:),neq%bNor(:),neq%xxx(:))

  IF (ipart /= 0 .AND. nsing > 0) THEN
    DO ipar = 1,neq%misc%npar
      IF (parlst(ipar) == 1) THEN
        CALL prparlst(1,8,0,neq%par(iPar)%name,neq%par(iPar)%locq, &
                      neq%par(iPar)%time)
        neq%aNor(ikf(iPar,iPar)) = 0.d0
        neq%bNor(iPar)           = 0.d0
        neq%xxx(iPar)            = 0.d0
      ENDIF
    ENDDO
  ENDIF

  vTPv = neq%misc%lTPl - DOT_PRODUCT(neq%xxx(1:neq%misc%npar), &
                                     neq%bNor(1:neq%misc%npar))

  dof = neq%misc%nobs + DBLE(neq%misc%npseu) - neq%misc%nparms

  IF (vTPv >= 0d0 .AND. dof > 0d0) THEN
    comstat%rms = SQRT( vTPv / dof )
  ELSE
    comstat%rms = opt%sigma0

    WRITE(lfnerr,"(/,' ### SR NEQSOLVE: ', &
      & 'A posteriori RMS of unit weight set to ',F8.4, &
      & /,18X,'vTPv / DOF: ',E12.4,' / ',ES12.4,/)") opt%sigma0, vTPv, dof
  ENDIF

! Save statistics for individual solutions
! ----------------------------------------
  IF (ipart == 3 .AND. ifil > 0) THEN
    statist(1) = ifil
    statist(2) = comStat%rms
    statist(3) = NINT(neq%misc%nObs - neq%misc%nParms) + neq%misc%nPseu
    statist(4) = (comStat%rms/opt%sigma0)**2
    statist(5) = NINT(neq%misc%nObs)
    statist(6) = neq%misc%nPseu
    statist(7) = neq%misc%nPar
    statist(8) = NINT(neq%misc%nParms) - neq%misc%nPar
    statist(9) = nsing
  ENDIF

! Collect information for WGT output (Var.rescaling, Helmert)
! -----------------------------------------------------------
  IF ( opt%wgtout == '' ) RETURN

  hlmOut%nNeq = SIZE(opt%neqFileName)

! Part 2: Combined solution
! -------------------------
  IF ( ifil == 0 .AND. ipart == 2 ) THEN

  ! Initialization
  ! --------------
    ALLOCATE(hlmOut%filNam(hlmOut%nNeq),stat=iac)
    CALL alcerr(iac, 'hlmOut%filNam', (/hlmOut%nNeq/), srName)
    hlmOut%filNam = opt%neqFileName

    ALLOCATE(hlmOut%indgrp(hlmOut%nNeq),stat=iac)
    CALL alcerr(iac, 'hlmOut%indgrp', (/hlmOut%nNeq/), srname)
    hlmOut%indgrp = hlmFil%indgrp

    ALLOCATE(hlmOut%grpnam(hlmOut%nNeq),stat=iac)
    CALL alcerr(iac, 'hlmOut%grpnam', (/hlmOut%nNeq/), srName)
    hlmOut%grpnam = hlmFil%grpnam

    ALLOCATE(hlmOut%ihelm(7,hlmOut%nNeq),stat=iac)
    CALL alcerr(iac, 'hlmOut%ihelm', (/7,hlmOut%nNeq/), srName)
    hlmOut%ihelm(:,:) = 0

    ALLOCATE(hlmOut%rhelm(7,hlmOut%nNeq),stat=iac)
    CALL alcerr(iac, 'hlmOut%rhelm', (/7,hlmOut%nNeq/), srName)
    hlmOut%rhelm(1:6,:) = 0d0
    hlmOut%rhelm(7,:)   = 1d0

    ALLOCATE(hlmOut%fact(hlmOut%nNeq),stat=iac)
    CALL alcerr(iac, 'hlmOut%fact', (/hlmOut%nNeq/), srName)
    hlmOut%fact(:) = 1.d0

    ! Collect Helmert parameters
    ! --------------------------
    DO jfil = 1, hlmOut%nNeq

      IF ( MAXVAL(hlmFil%ihelm(:,jfil)) <= 1 ) CYCLE

      DO ipar = 1, neq%misc%npar
        IF ( neq%par(ipar)%locq(1) /= 28 ) CYCLE

        IF ( neq%par(ipar)%name(1:3) /= 'ALL' .AND.  &
             neq%par(ipar)%name(1:3) /= hlmOut%grpnam(jfil) ) CYCLE

        ityp = neq%par(ipar)%locq(2)

        hlmOut%ihelm(ityp,jfil) = 2
        hlmOut%rhelm(ityp,jfil) = neq%xxx(iPar) + neq%par(iPar)%x0

        IF ( ityp > 3 .AND. ityp <= 6 )  &
          hlmOut%rhelm(ityp,jfil) = hlmOut%rhelm(ityp,jfil) / datum%aell

        IF ( ityp == 7 )  &
          hlmOut%rhelm(ityp,jfil) = 1d0 + hlmOut%rhelm(ityp,jfil) / datum%aell

      END DO
    END DO

! Comparison part: Get variance rescaling factors
! -----------------------------------------------
  ELSEIF ( ipart == 3 ) THEN
      hlmOut%fact(ifil) = 1d0 / statist(4)

  END IF


END SUBROUTINE neqsolve


END MODULE
