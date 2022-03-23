MODULE s_ORBTRANS
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE orbtrans(neq,ifil,orb)

! -------------------------------------------------------------------------
! Purpose:    This subroutine represents an interface between the program
!             ADDNEQ2 and the subroutine MODNEQ, which performs the
!             transformation of the orbital parameters.
!
! Author:     L. Mervart
!
! Created:    22-Nov-1997
!
! Changes:    26-Jun-2001 RD: Use alcerr for allocation
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             30-Jan-2002 CU: Increase sclsto: 3 -> 6
!             28-Jan-2003 HU: Error output to lfnerr
!             27-May-2003 CU: Update neq%misc%nparms
!             26-Jan-2004 HU: Handle case ifil > maxstd
!             18-Apr-2005 RD: Consider maneuvers for stc at neq boundaries
!             22-Sep-2005 RD: Use new modules D_NEQ.f90 and D_PAR.f90
!             08-Feb-2007 RD: misc%nObs/nParms i4b->r8b
!             16-Jul-2008 RD: Remove p_addold from modneq
!             28-Oct-2008 DT: Use maxVar from M_MAXDIM
!             14-Jul-2009 RD: Special handling for parameter 22 for longarc
!             14-Jul-2009 RD: Special handling for parameter  2 for longarc
!             03-Dec-2010 HB: Add parameter for SR PRTDER
!             21-Jan-2011 MM/SS: Special handling for parameter 30
!                             IMPORTANT NOTE: This is necessary because
!                                             par%name is not considered!
!             31-Jan-2011 MM: Reset locq(7) to 0 for parameter 30
!             14-Jun-2012 RD: Handle also empty NEQs (SIZE(neq%par) undefined)
!             14-Jun-2012 RD: Use m_bern with only
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnerr
  USE d_par,    ONLY: t_par,maxLcq, &
                      parType_constant,parType_epochSpecific
  USE d_neq,    ONLY: t_neq,maxSat
  USE m_maxdim, ONLY: maxVar
  USE p_addneq, ONLY: t_orb,opt,comstat, &
                      maxArc,maxStc,maxDyn,maxStd,maxCoe,maxStp

  USE s_alcerr
  USE s_prtder
  USE f_modsvn
  USE s_getorf
  USE s_modneq
  USE s_getarc
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_neq), INTENT(INOUT) :: neq
  INTEGER(i4b),INTENT(IN)    :: ifil
  TYPE(t_orb), INTENT(INOUT) :: orb

! Local Variables
! ---------------
  INTEGER(i4b),PARAMETER                            :: iant  = 0
  INTEGER(i4b),PARAMETER                            :: ider  = 1
  INTEGER(i4b),PARAMETER                            :: istop = 2
  INTEGER(i4b),PARAMETER                            :: iarc  = 1
  INTEGER(i4b)                                      :: numstc
  INTEGER(i4b)                                      :: narc
  INTEGER(i4b),DIMENSION(maxArc)                    :: nsaArc
  INTEGER(i4b),DIMENSION(maxSat,maxArc)             :: satArc
  INTEGER(i4b)                                      :: icrarc
  INTEGER(i4b)                                      :: iorsys
  INTEGER(i4b),DIMENSION(maxStc*maxStp),SAVE        :: stotyp
  INTEGER(i4b),DIMENSION(maxStc*maxStp),SAVE        :: svnsto
  INTEGER(i4b),DIMENSION(maxStc*maxStp),SAVE        :: stobnd
  INTEGER(i4b),DIMENSION(2,maxCoe)                  :: indcoe
  INTEGER(i4b),DIMENSION(:,:),ALLOCATABLE           :: locq
  INTEGER(i4b)                                      :: isat
  INTEGER(i4b)                                      :: irpr
  INTEGER(i4b)                                      :: iman
  INTEGER(i4b)                                      :: ircode
  INTEGER(i4b)                                      :: nvar
  INTEGER(i4b)                                      :: nrad
  INTEGER(i4b)                                      :: nrpr
  INTEGER(i4b)                                      :: ipar
  INTEGER(i4b)                                      :: ii
  INTEGER(i4b)                                      :: istoch
  INTEGER(i4b)                                      :: npar_old
  INTEGER(i4b)                                      :: ipar_old
  INTEGER(i4b)                                      :: ilcq
  INTEGER(i4b)                                      :: iac
  INTEGER(i4b)                                      :: nObs
  TYPE(t_par),DIMENSION(:),ALLOCATABLE              :: par_old

  REAL(r8b),DIMENSION(2,maxArc)                     :: tarc
  REAL(r8b)                                         :: ttt
  REAL(r8b),DIMENSION(3*(ider+1))                   :: xxx
  REAL(r8b),DIMENSION(:),ALLOCATABLE                :: aNew
  REAL(r8b),DIMENSION(:),ALLOCATABLE                :: bNew
  REAL(r8b),DIMENSION(maxCoe)                       :: coetra
  REAL(r8b),DIMENSION(7,maxSat)                     :: elefil
  REAL(r8b),DIMENSION(maxDyn,maxSat)                :: rprfil
  REAL(r8b),DIMENSION(7,maxSat)                     :: elesat
  REAL(r8b),DIMENSION(maxVar)                       :: sclorb
  REAL(r8b),DIMENSION(6)                            :: sclsto

  CHARACTER(LEN=1),DIMENSION(10,maxArc)             :: source
  CHARACTER(LEN=8)                                  :: anltyp

! Commons for old subroutines
! ---------------------------
  CHARACTER*6  mxnsat,mxnpar,mxnlcq,mxnstc,mxndyn,mxnstd, &
               mxnstp,mxnarc,mxncoe,mxnvar

  INTEGER(i4b) mxcsat,mxcpar,mxclcq,mxcstc,mxcdyn,mxcstd, &
               mxcstp,mxcarc,mxccoe,mxcvar

  COMMON/mcmsat/mxcsat,mxnsat
  COMMON/mcmpar/mxcpar,mxnpar
  COMMON/mcmlcq/mxclcq,mxnlcq
  COMMON/mcmstc/mxcstc,mxnstc
  COMMON/mcmstp/mxcstp,mxnstp
  COMMON/mcmdyn/mxcdyn,mxndyn
  COMMON/mcmstd/mxcstd,mxnstd
  COMMON/mcmarc/mxcarc,mxnarc
  COMMON/mcmcoe/mxccoe,mxncoe
  COMMON/mcmvar/mxcvar,mxnvar

  mxcsat = maxSat                          ; mxnsat = 'MAXSAT'
  IF ( neq%misc%npar == 0 ) THEN
    mxcpar = 1                             ; mxnpar = 'MAXPAR'
  ELSE
    mxcpar = SIZE(neq%par) - comstat%elimi ; mxnpar = 'MAXPAR'
  ENDIF
  mxclcq = maxlcq                          ; mxnlcq = 'MAXLCQ'
  mxcstc = maxStc                          ; mxnstc = 'MAXSTC'
  mxcdyn = maxDyn                          ; mxndyn = 'MAXDYN'
  mxcstd = maxStd                          ; mxnstd = 'MAXSTD'
  mxcstp = maxStp                          ; mxnstp = 'MAXSTP'
  mxcarc = maxArc                          ; mxnarc = 'MAXARC'
  mxccoe = maxCoe                          ; mxncoe = 'MAXCOE'
  mxcvar = maxVar                          ; mxnvar = 'MAXVAR'

  IF ( (opt%orbFil(1,ifil) == ' ') .OR. ( opt%trafo == 0 ) ) RETURN

!!!!!  ALLOCATE(aNew(mxcpar*mxcpar), stat=iac)
!!!!!  CALL alcerr(iac, 'aNew', (/mxcpar*mxcpar/), 'orbtrans')
  ALLOCATE(aNew(mxcpar*(mxcpar+1)/2), stat=iac)
  CALL alcerr(iac, 'aNew', (/mxcpar*(mxcpar+1)/2/), 'orbtrans')
  ALLOCATE(bNew(mxcpar), stat=iac)
  CALL alcerr(iac, 'bNew', (/mxcpar/), 'orbtrans')
  ALLOCATE(par_old(mxcpar), stat=iac)
  CALL alcerr(iac, 'par_old', (/mxcpar/), 'orbtrans')
  ALLOCATE(locq(mxclcq,mxcpar), stat=iac)
  CALL alcerr(iac, 'locq', (/mxclcq,mxcpar/), 'orbtrans')

! Find the scale factors, and number of rad. press. parameters
! ------------------------------------------------------------

  sclorb = 0.0
  sclsto = 1.d6

  DO ipar = 1,neq%misc%npar
    IF ( neq%par(ipar)%locq(1) == 3) THEN
      nrpr = neq%par(ipar)%locq(5) - 6
      sclorb( neq%par(ipar)%locq(4) ) = neq%par(ipar)%scale
      IF ( MINVAL(sclorb) /= 0.d0 ) EXIT
    END IF
  END DO

  DO ipar = 1,neq%misc%npar
    IF ( neq%par(ipar)%locq(1) == 11) THEN
      sclsto(neq%par(iPar)%locq(5)) = neq%par(ipar)%scale
    END IF
  END DO

! Read the information about the orbit arcs in current file
! ---------------------------------------------------------
  CALL getarc(opt%orbFil(1,ifil),narc,nsaarc,satarc, &
              tarc,source,iorsys)

  IF ( narc /= 1) THEN
    WRITE(lfnerr,*) ' *** SR ORBTRANS: MORE THAN ONE ARC'
    CALL EXITRC(2)
  END IF

  IF ( ifil > maxstd) THEN
    WRITE(lfnerr,*) ' *** SR ORBTRANS: TOO MANY FILES, MAXSTD=',maxstd
    CALL EXITRC(2)
  END IF

! Loop over all satellites, read the a priori orbit information
! -------------------------------------------------------------
  DO isat = 1, nsaarc(iarc)
    ttt = tarc(1,1)
    CALL getorf(opt%orbFil(1,ifil),satarc(isat,iarc),  &
                iant,ider,istop,ttt,icrarc,iorsys,xxx, &
                orb%tosc(ifil),elefil(1,isat),ircode)
    ttt = orb%tosc(ifil)
    CALL getorf(opt%orbFil(1,ifil),satarc(isat,iarc),  &
                iant,ider,istop,ttt,icrarc,iorsys,xxx, &
                orb%tosc(ifil),elefil(1,isat),ircode)

    DO irpr = 1,nrpr
      CALL prtder(opt%orbFil(2,ifil),satarc(isat,iarc),irpr,ider, &
                  istop,ttt,1,icrarc,iorsys,nvar,nrad,xxx,elesat, &
                  rprfil(1,isat),anltyp,ircode)
    END DO

  END DO

! Update orb%nstoch, stotyp, orb%tstoch, svnsto, stobnd
! -----------------------------------------------------
  LOOP_ipar: DO ipar = 1,neq%misc%npar
    IF ( neq%par(ipar)%locq(1) == 11) THEN

      DO istoch = 1, orb%nstoch
        IF ( stotyp(istoch)     == neq%par(ipar)%locq(5)   .AND. &
             orb%tstoch(istoch) == neq%par(ipar)%time%mean .AND. &
             svnsto(istoch)     == neq%par(ipar)%locq(3) ) THEN
          CYCLE LOOP_ipar
        END IF
      END DO

      orb%nstoch            = orb%nstoch + 1
      neq%par(ipar)%locq(4) = orb%nstoch

      IF ( orb%nstoch > maxStc*maxStp ) THEN
        WRITE(lfnerr,*) ' *** SR ORBTRANS: maxStc*maxStp TOO SMALL'
        CALL EXITRC(2)
      END IF

      stotyp(orb%nstoch)     = neq%par(ipar)%locq(5)
      orb%tstoch(orb%nstoch) = neq%par(ipar)%time%mean
      svnsto(orb%nstoch)     = neq%par(ipar)%locq(3)
      stobnd(orb%nstoch)     = -1
    END IF
  END DO LOOP_ipar

! Stochastic parameters at the boundary
! -------------------------------------
  IF ( opt%numstc == -1 ) THEN
    numstc        = nsaarc(iarc)
    opt%svnstc(:) = satarc(:,iarc)

    numstc = 0

    stcLoop: DO iSat = 1,nsaarc(iarc)
      DO iMan = 1,orb%nMan

        IF (orb%man(iMan)%satman /= modsvn(satarc(iSat,iArc)) .OR. &
            DABS(orb%man(iMan)%timman - orb%tosc(iFil)) >= opt%dtstc) CYCLE

        IF (orb%man(iMan)%satman == satarc(iSat,iArc) .AND. &
            orb%man(iMan)%timman - orb%tosc(iFil) < opt%dtstc) CYCLE stcLoop

        IF (modsvn(satarc(iSat,iArc)) /= satarc(iSat,iArc) .AND. &
            orb%man(iMan)%satman == modsvn(satarc(iSat,iArc)) .AND. &
            orb%tosc(iFil) - orb%man(iMan)%timman < opt%dtstc) CYCLE stcLoop

      ENDDO

      numstc = numstc+1
      opt%svnstc(numstc) = satarc(isat,iarc)

    ENDDO stcLoop
  ELSE
    numstc        = opt%numstc
  END IF

! Copy the locq, save the old parameter description
! -------------------------------------------------
  DO ipar = 1,neq%misc%npar
    IF (neq%par(ipar)%locq(1) ==  2) neq%par(ipar)%locq(2) = ipar
    IF (neq%par(ipar)%locq(1) == 22) neq%par(ipar)%locq(7) = ipar
    IF (neq%par(ipar)%locq(1) == 30) neq%par(ipar)%locq(7) = ipar
    locq(:,ipar) = neq%par(ipar)%locq(:)
    par_old(ipar) = neq%par(ipar)
  END DO
  npar_old   = neq%misc%npar

! Perform the transformation
! --------------------------
  nObs = NINT(neq%misc%nobs)

  CALL modneq(opt%maxpar,    &
              ifil,          opt%orbFil(:,:), nsaarc(iarc),   satarc(:,iarc), &
              opt%nstcep,    opt%rsw,         opt%sigma0,     opt%sigrsw,     &
              numstc,        opt%svnstc,      opt%numdyn,     opt%svndyn,     &
              opt%numgap,    opt%svngap,      opt%numnac,     opt%newarc,     &
              elefil,        rprfil,                                          &
              orb%nseff,     orb%svneff,      orb%arcnum,     orb%manovr,     &
              neq%misc%npar, nobs,            locq,           neq%aNor(:),    &
              neq%bNor(:),   neq%misc%lTPl,   orb%tosc,       orb%ele,        &
              orb%rpress,    orb%nstoch,      orb%tstoch,     stotyp,         &
              svnsto,        stobnd,          sclorb,         sclsto,         &
              orb%kmat,      orb%lmat,        orb%mmat,       orb%drho,       &
              orb%drhot,     orb%drpr,        anew,           bnew,           &
              coetra,        indcoe)

  IF ( neq%misc%npar > mxcpar ) THEN
    WRITE(lfnerr,*) '*** SR ORBTRANS: UNEXPECTED ERROR 1'
    CALL EXITRC(2)
  END IF

! Update nparms
! -------------
  neq%misc%nobs = DBLE(nObs)
  neq%misc%nparms = neq%misc%nparms + DBLE(neq%misc%npar-npar_old)

! Update parameter description structure
! --------------------------------------
  DO ipar = 1,neq%misc%npar

! Orbit parameters - just copy all the information

    IF ( locq(1,ipar) == 3 ) THEN
      neq%par(ipar)%locq(:)   = locq(:,ipar)
      neq%par(ipar)%time%mean = orb%tosc(ifil)
      neq%par(ipar)%time%half = 0.d0
      neq%par(ipar)%scale     = sclorb(locq(4,ipar))
      neq%par(ipar)%name      = ''
      neq%par(ipar)%x0        = 0.d0
      neq%par(ipar)%type      = parType_constant
    ELSE IF ( locq(1,ipar) == 11 ) THEN
      neq%par(ipar)%locq(:)   = locq(:,ipar)
      neq%par(ipar)%time%mean = orb%tstoch(locq(4,ipar))
      neq%par(ipar)%time%half = 0.d0
      neq%par(ipar)%scale     = sclsto(locq(5,ipar))
      neq%par(ipar)%name      = ''
      neq%par(ipar)%x0        = 0.d0
      neq%par(ipar)%type      = parType_epochSpecific
    ELSE

! Other parameters

      LOOP_ii: DO ii = 1, npar_old
        ipar_old = ii

        DO ilcq = 1, maxlcq
          IF ( par_old(ii)%locq(ilcq) /= locq(ilcq,ipar) ) THEN
            ipar_old = 0
            CYCLE LOOP_ii
          END IF
        END DO

        IF (ipar_old /= 0) EXIT
      END DO LOOP_ii

      IF (ipar_old /= 0) THEN
        neq%par(ipar)         = par_old(ipar_old)
        neq%par(ipar)%locq(:) = locq(:,ipar)
        IF (neq%par(ipar)%locq(1) == 22) neq%par(ipar)%locq(7) = 0
        IF (neq%par(ipar)%locq(1) == 30) neq%par(ipar)%locq(7) = 0
      ELSE
        WRITE(lfnerr,*) ' *** SR ORBTRANS: UNEXPECTED ERROR 2'
        CALL EXITRC(2)
      END IF
    END IF
  END DO

  DEALLOCATE(locq, stat=iac)
  DEALLOCATE(par_old, stat=iac)
  DEALLOCATE(bNew, stat=iac)
  DEALLOCATE(aNew, stat=iac)

END SUBROUTINE orbtrans


END MODULE
