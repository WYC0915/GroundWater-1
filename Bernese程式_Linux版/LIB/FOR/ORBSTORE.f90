MODULE s_ORBSTORE
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE orbstore(neq,orb)

! -------------------------------------------------------------------------
! Purpose:    This subroutine represents an interface between the program
!             ADDNEQ2 and the routine ELESAV, which stores the orbital
!             elements into a file
!
! Author:     L. Mervart
!
! Created:    22-NOV-1997
!
! Changes:    31-JUL-2000 LM: re-initialize orb structure before ARCDEF
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             30-Jan-2002 CU: Increase sclsto: 3 -> 6
!             12-Aug-2003 HU: Write errors to error output
!             15-Apr-2005 RD: New call of SR ARCDEF
!             22-Sep-2005 RD: Use new modules D_NEQ.f90 and D_PAR.f90
!             16-Jan-2008 HB: Add LEO acceleration parameters
!             28-Oct-2008 DT: Use maxVar from M_MAXDIM
!             03-Dec-2010 HB: Add parameter for SR PRTDER
!             27-Mar-2012 RD: Remove unused variables from elesav
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnerr
  USE d_par,    ONLY: maxlcq
  USE d_neq,    ONLY: t_neq,maxSat
  USE m_maxdim, ONLY: maxVar
  USE p_addneq, ONLY: t_orb,opt,comstat,maxArc,maxStc,maxDyn,maxStp

  USE s_prtder
  USE s_elesav
  USE s_getorf
  USE s_getarc
  USE s_exitrc
  USE s_arcdef
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_neq) :: neq
  TYPE(t_orb) :: orb  ! Orbit description structure (see P_ADDNEQ)

! Local Variables
! ---------------
  INTEGER(i4b)                                      :: nfil
  INTEGER(i4b), DIMENSION(maxlcq,neq%misc%npar)     :: locq
  INTEGER(i4b)                                      :: ipar
  INTEGER(i4b)                                      :: ihlp

  REAL(r8b)                                         :: hint
  REAL(r8b),DIMENSION(maxVar)                       :: sclorb
  REAL(r8b),DIMENSION(6)                            :: sclsto

  INTEGER(i4b),PARAMETER                            :: iant  = 0
  INTEGER(i4b),PARAMETER                            :: ider  = 1
  INTEGER(i4b),PARAMETER                            :: istop = 2
  INTEGER(i4b),PARAMETER                            :: iarc  = 1
  INTEGER(i4b)                                      :: narc
  INTEGER(i4b),DIMENSION(maxArc)                    :: nsaArc
  INTEGER(i4b),DIMENSION(maxSat,maxArc)             :: satArc
  INTEGER(i4b)                                      :: icrarc
  INTEGER(i4b)                                      :: iorsys
  INTEGER(i4b)                                      :: isat
  INTEGER(i4b)                                      :: irpr
  INTEGER(i4b)                                      :: ircode
  INTEGER(i4b)                                      :: nvar
  INTEGER(i4b)                                      :: nrad
  INTEGER(i4b)                                      :: nrpr
  INTEGER(i4b)                                      :: ifil
  INTEGER(i4b)                                      :: istoch
  INTEGER(i4b)                                      :: norbpar
  INTEGER(i4b)                                      :: iVar

  REAL(r8b),DIMENSION(2,maxArc)                     :: tarc
  REAL(r8b)                                         :: ttt
  REAL(r8b),DIMENSION(3*(ider+1))                   :: xxx
  REAL(r8b),DIMENSION(7,maxSat)                     :: elefil
  REAL(r8b),DIMENSION(maxDyn,maxSat)                :: rprfil
  REAL(r8b),DIMENSION(7,maxSat)                     :: elesat
  INTEGER(i4b),DIMENSION(maxStc*maxStp),SAVE        :: svnsto
  INTEGER(i4b),DIMENSION(maxStc*maxStp),SAVE        :: stotyp

  CHARACTER(LEN=1),DIMENSION(10,maxArc)             :: source
  CHARACTER(LEN=8)                                  :: anltyp


  IF (opt%orbFil(1,1) == '') RETURN

  sclorb = 0.0
  sclsto = 0.0

  norbpar = 0

  DO ipar = 1,neq%misc%npar
    IF ( neq%par(ipar)%locq(1) == 3) THEN
      norbpar = norbpar + 1
      nrpr = neq%par(ipar)%locq(5) - 6
      sclorb( neq%par(ipar)%locq(4) ) = neq%par(ipar)%scale
      IF ( MINVAL(sclorb) /= 0.d0 ) EXIT
    END IF
  END DO

  DO iVar = 1, maxVar
    IF ( sclorb(iVar) == 0.0) sclorb(iVar) = 1.0
  END DO

  IF (norbpar == 0) RETURN

  DO ipar = 1,neq%misc%npar
    IF ( neq%par(ipar)%locq(1) == 11) THEN
      IF (neq%par(iPar)%locq(5) > 10) THEN
        ihlp = neq%par(iPar)%locq(5)-10
      ELSE
        ihlp = neq%par(iPar)%locq(5)
      ENDIF
      sclsto(ihlp) = neq%par(ipar)%scale
    END IF
  END DO

! Copy the locq
! -------------
  DO ipar = 1,neq%misc%npar
    locq(:,ipar) = neq%par(ipar)%locq(:)
  END DO

  hint = 1.0d0

  orb%title = opt%title

! All the information already prepared by orbtrans and modneq
! -----------------------------------------------------------
  IF ( opt%trafo /= 0 ) THEN
    nfil = SIZE(opt%neqFileName)
    CALL elesav(orb%title,    nfil,          opt%orbFil(:,:), orb%nseff,   &
                orb%svneff,   orb%ele,       orb%rpress,                   &
                orb%tosc,     neq%misc%npar, locq,            neq%xxx(:),  &
                sclorb,       sclsto,        comstat%rms,     neq%anor(:), &
                orb%nstoch,   orb%tstoch,    orb%kmat,        orb%lmat,    &
                orb%mmat,     orb%drho,      orb%drhot,       opt%numdyn,  &
                opt%svndyn,   opt%nstcep,    hint,            orb%arcnum,  &
                orb%manovr)

  ELSE

! It is necessary to prepare the information now
! ----------------------------------------------
    DO ifil = 1, SIZE(opt%neqFileName)
      CALL getarc(opt%orbFil(1,ifil),narc,nsaarc,satarc, &
                  tarc,source,iorsys)
      IF ( narc /= 1) THEN
        WRITE(lfnerr,*) ' *** SR ORBSTORE: MORE THAN ONE ARC'
        CALL EXITRC(2)
      END IF

! Loop over all satellites, read the a priori orbit information
! -------------------------------------------------------------
      DO isat = 1, nsaarc(iarc)
        ttt = tarc(1,1)
        CALL getorf(opt%orbFil(1,ifil),satarc(isat,iarc),  &
                    iant,ider,istop,ttt,icrarc,iorsys,xxx, &
                    orb%tosc(1),elefil(1,isat),ircode)
        ttt = orb%tosc(1)
        CALL getorf(opt%orbFil(1,ifil),satarc(isat,iarc),  &
                    iant,ider,istop,ttt,icrarc,iorsys,xxx, &
                    orb%tosc(1),elefil(1,isat),ircode)

        DO irpr = 1,nrpr
          CALL prtder(opt%orbFil(2,ifil),satarc(isat,iarc),irpr,ider, &
                      istop,ttt,1,icrarc,iorsys,nvar,nrad,xxx,elesat, &
                      rprfil(1,isat),anltyp,ircode)
        END DO
      END DO

! Update orb%nstoch, stotyp, orb%tstoch, svnsto
! ---------------------------------------------
      orb%nstoch = 0
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
          locq(4,ipar)          = orb%nstoch

          IF ( orb%nstoch > maxStc*maxStp ) THEN
            WRITE(lfnerr,*) ' *** SR ORBSTORE: maxStc*maxStp TOO SMALL'
            CALL EXITRC(2)
          END IF

          stotyp(orb%nstoch)     = neq%par(ipar)%locq(5)
          orb%tstoch(orb%nstoch) = neq%par(ipar)%time%mean
          svnsto(orb%nstoch)     = neq%par(ipar)%locq(3)
        END IF
      END DO LOOP_ipar

      orb%nseff         = 0
      orb%svneff(:)     = 0
      orb%arcnum(:,:)   = 0
      orb%manovr(:)     = 0
      orb%ele(:,:,:)    = 0.0
      orb%rpress(:,:,:) = 0.0

      CALL arcdef(neq%misc%npar  , locq          , 1         , nsaarc(iarc), &
                  satarc(:,iarc) , 0             , (/ 1 /)   , opt%numgap  , &
                  opt%svngap     , elefil        , rprfil    , orb%tosc(1) , &
                  opt%numnac     , opt%newarc    , orb%nseff , orb%svneff  , &
                  orb%arcnum     , orb%manovr    , orb%ele(1,1,1) ,          &
                  orb%rpress(1,1,1))

      DO ipar = 1,neq%misc%npar
        IF ( locq(1,ipar) == 3 .AND. locq(4,ipar) > 6) locq(7,ipar) = 1
      END DO

      CALL elesav(orb%title,    1,             opt%orbFil(:,ifil), orb%nseff, &
                  orb%svneff,   orb%ele,       orb%rpress,                    &
                  orb%tosc,     neq%misc%npar, locq,             neq%xxx(:),  &
                  sclorb,       sclsto,        comstat%rms,      neq%anor(:), &
                  orb%nstoch,   orb%tstoch,    orb%kmat,         orb%lmat,    &
                  orb%mmat,     orb%drho,      orb%drhot,        opt%numdyn,  &
                  opt%svndyn,   opt%nstcep,    hint,             orb%arcnum,  &
                  orb%manovr)
    END DO
  END IF

END SUBROUTINE orbstore


END MODULE
