MODULE s_AOPTORB
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE aoptorb(opt, parOrb)

! -------------------------------------------------------------------------
! Purpose:    Reads input options for ADDNEQ2:
!             options and constraining for orbit parameters
!
! Author:     M. Meindl
!
! Created:    10-Jul-2001
!
! Changes:    30-Aug-2001 RD: NUMSTC=-1 for pulses on day boundaries
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             09-Jan-2002 CU: Use priorp: print orbit parameter input opt.
!             23-Jan-2002 CU: DEALLOCATE local variables
!             30-Jan-2002 CU: Make the introduction of stoch. pulses
!                             (system:S,Y,X) available
!             05-Feb-2002 CU: Error if same STD for different NEQs used
!             06-Feb-2002 CU: Take only introduced force types of stoch.
!                             pulses for new stoch. pulses on the NEQ bound.
!             08-Feb-2002 MM: Use t_sigma for local sigmas
!                             Enable relative sigmas
!             04-Jun-2002 CU: Correct format statement for priorp
!             28-Jun-2002 RD: Correct format statement for priorp
!             30-Jul-2002 HU: Use interface for priorp
!             29-Oct-2002 MR: Correct format (1x)
!             10-Dec-2002 CU: Use new SR prisig instead of priorp
!                             for printing of a priori sigma
!             19-Mar-2003 RD: Correct SIZE statement (because of IFC)
!             23-Apr-2003 CU: Nullify local pointers
!             22-May-2003 RD: Use maxsat from m_maxdim (instead of p_addneq)
!             27-May-2003 CU: Print long-arc request
!             19-Feb-2004 HU: Activate longarc and std only if orbits in NEQs
!             18-Apr-2005 RD: Consider maneuvers for stc at neq boundaries
!             05-May-2005 HU: Warning if longarc but no NEQ ordering
!             28-Feb-2007 AG: USE 206265... from DEFCON
!             09-Jan-2008 HB: maxvar 15 => 21
!             16-Jan-2008 HB: Add accelerations for LEOs
!             26-Feb-2008 RD: Modify satcrux entries in d_satcrx
!             18-Sep-2008 DT: New keyword SPLTDYN -> opt%splitDyn
!             28-Oct-2008 DT: Use maxVar from M_MAXDIM
!             13-Nov-2008 DT: Get orbit model description (SR gtorbmd)
!             12-Mar-2012 HB: Layout unification for stochastic orbit parameter
!             19-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnerr, lfnprt, &
                      keyValueLength
  USE d_const,  ONLY: ars
  USE d_satcrx, ONLY: crx_addneq
  USE m_maxdim, ONLY: maxsat, maxVar
  USE p_addneq, ONLY: t_opt, t_parOrb, t_sigma
  USE p_orbgen, ONLY: orbdsc
  USE s_ckoptr
  USE s_ckoptt
  USE s_alcerr
  USE s_prisig
  USE s_readkeys
  USE s_exitrc
  USE s_gtorbmd
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  TYPE(t_parOrb)               :: parOrb         ! Orbit param. inp. options

! output:
  TYPE(t_opt)                  :: opt            ! Options for ADDNEQ2

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=7), PARAMETER                           :: srName = 'aoptorb'
  CHARACTER(LEN=6),  DIMENSION(MAXVAR), PARAMETER       :: orbKeyw = &
  (/'ORB_A ','ORB_E ','ORB_I ','ORB_OM','ORB_P ','ORB_U ', &
    'ORB_D0','ORB_Y0','ORB_X0','ORB_DP','ORB_YP','ORB_XP', &
    'ORB_DP','ORB_YP','ORB_XP','ORB_DP','ORB_YP','ORB_XP', &
    'ORB_DP','ORB_YP','ORB_XP'/)
  CHARACTER(LEN=5),  DIMENSION(6), PARAMETER            :: stcKeyw = &
  (/'STC_R','STC_A','STC_O','STC_S','STC_Y','STC_X'/)
  CHARACTER(LEN=12),  DIMENSION(6), PARAMETER           :: frcTyp  = &
  (/'radial      ','along-track ','out-of-plane','dir. to Sun ','y-direction ','x-direction '/)

  INTEGER(i4b), PARAMETER                               :: orbSigTyp = 18

! Local variables
! ---------------
  TYPE(t_sigma),DIMENSION(:),ALLOCATABLE                :: locSig

  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER  :: keyValue

  INTEGER(i4b)                                          :: maxSigTyp
  INTEGER(i4b)                                          :: oldSigTyp
  INTEGER(i4b)                                          :: nSig, iSig
  INTEGER(i4b)                                          :: iOrb, jOrb
  INTEGER(i4b)                                          :: iStc, jStc
  INTEGER(i4b)                                          :: iFil,jFil,nFil
  INTEGER(i4b)                                          :: irc, iac, irCode
  INTEGER(i4b)                                          :: ityp
  INTEGER(i4b),      DIMENSION(3)                       :: hlptyp

  REAL(r8b),         DIMENSION(maxVar)                  :: sigOrb
  REAL(r8b),         DIMENSION(3)                       :: sigStc

! (sorry for the common blocks, used by SR priorp)
  CHARACTER(LEN=6) :: MXNSAT,MXNSTC,MXNVAR
  INTEGER(i4b)     :: MXCSAT,MXCSTC,MXCVAR
  COMMON/MCMSAT/MXCSAT,MXNSAT
  COMMON/MCMSTC/MXCSTC,MXNSTC
  COMMON/MCMVAR/MXCVAR,MXNVAR

  MXNSAT='MAXSAT'
  MXNSTC='MAXSTC'
  MXNVAR='MAXVAR'

  MXCSAT=maxSat
  MXCSTC=1
!  IF (MXCSTC == 0) MXCSTC=1
  MXCVAR=maxVar

  NULLIFY(keyValue)
  irCode = 0

! Get other sigmas from opt%sigma
! -------------------------------
  oldSigTyp = SIZE(opt%sigma)
  maxSigTyp = oldSigTyp + orbSigTyp

  ALLOCATE(locSig(maxSigTyp),stat=iac)
  CALL alcerr(iac,'locSig',(/maxSigTyp/),'aoptorb')

! Initialize local sigma
  DO iSig = 1,maxSigTyp
    locSig(iSig)%locq(:) = 0
  ENDDO
  sigOrb(:)           = 0.D0
  locSig(:)%value     = 0.D0
  locSig(:)%name      = ''
  locSig(:)%typFlg    = 'A'
  locSig(1:oldSigTyp) = opt%sigma(:)


! Read options
! ------------
  opt%numstc = 0
  opt%trafo  = 0
  opt%splitDyn = 0

  CALL readkeys('NUMSTC', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1' .AND. parOrb%nOrb > 0) opt%numstc = -1
  CALL readkeys('TRAFO', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1' .AND. parOrb%nOrb > 0) opt%trafo = 1
  IF (opt%trafo == 1 .AND. opt%chrono == 0) THEN
    WRITE(lfnerr,'(/,A,/,A,/,A,/,A)')                                       &
         ' ### SR AOPTORB: Long-arc computation enabled but chronological',   &
         '                 ordering of input NEQs not enabled. If the input', &
         '                 NEQs are not ordered this may cause unpredictable',&
         '                 for the output orbits!'
  ENDIF

  IF ( opt%trafo == 1 ) THEN
    CALL readkeys('SPLTDYN', keyValue, irc)
    IF ( irc == 0 .AND. keyValue(1) == '1' ) opt%splitDyn = 1
  END IF

  opt%dtstc = 0d0
  IF (opt%numstc == -1) THEN
    CALL readKeys('DTSTC', keyValue, irc)

    CALL ckoptt(1,'DTSTC', keyValue, srName, &
                'Stoch. pulses for maneuvers', irc, irCode, &
                empty = 0d0, ge = 0d0, maxVal = 1, result1 = opt%dtstc)
    opt%dtstc = opt%dtstc/24d0
  ENDIF


! Adopt the satecux entries concerning maneuver and arc split events
! ------------------------------------------------------------------
  CALL crx_addneq(parOrb%epoOrb)


! Constraining of orbit parameters
! --------------------------------
  nSig   = 0

! Get orbital parameter sigmas
! ----------------------------
  DO iOrb = 1, maxVar
    DO jOrb = 1,parOrb%nOrb
      IF (parOrb%seqOrb(jOrb) == iOrb) THEN

        CALL readkeys(orbKeyw(iOrb), keyValue, irc)

        CALL ckoptr(1, orbKeyw(iOrb), keyValue, srName,              &
                   'Orbit parameter constraints', irc, irCode,       &
                   empty=0d0, ge=0d0, maxVal=1, error=99.9999d0,     &
                   result1=sigOrb(jOrb))

        IF (sigOrb(jOrb) /= 0d0) THEN
          nSig = nSig + 1
          isig = nsig + oldSigTyp
          locSig(iSig)%locq(1) = 3
          locSig(iSig)%locq(4) = iOrb
          IF (iOrb > 2 .AND. iOrb < 7) THEN
            locSig(iSig)%value = sigOrb(jOrb) / ars
          ELSE
            locSig(iSig)%value = sigOrb(jOrb)
          ENDIF
        ENDIF

        EXIT
      ENDIF
    ENDDO

  ENDDO

! Get stochastic orbit parameter sigmas
! -------------------------------------
  IF (opt%numstc==-1 .AND. SIZE(opt%neqFileName)>1 .AND. opt%nStcep==0) THEN
    opt%nStcep = 3
    opt%rsw(1) = 1
    opt%rsw(2) = 2
    opt%rsw(3) = 3
  ENDIF

  IF (opt%nStcep > 0) THEN
    sigStc(:) = 0d0

    DO iStc = 1, 6
      DO jStc = 1, opt%nstcep
        IF(opt%rsw(jstc) > 10) THEN
          hlptyp(jstc)=opt%rsw(jstc)-10
        ELSE
          hlptyp(jstc)=opt%rsw(jstc)
        ENDIF
        IF (iStc == hlptyp(jStc)) THEN
          CALL readkeys(stcKeyw(iStc), keyValue, irc)

          CALL ckoptr(1, stcKeyw(iStc), keyValue, srName,              &
                     'Stochastic orbit parameter constraints', irc,    &
                     irCode, empty=0d0, ge=0d0, maxVal=1,              &
                     error=99.9999d0, result1=sigStc(jStc))

          IF (sigStc(jStc) /= 0d0) THEN
            nSig = nSig + 1
            isig = nsig + oldSigTyp
            locSig(iSig)%locq(1) = 11
            locSig(iSig)%locq(5) = iStc
            locSig(iSig)%value   = sigStc(jStc)
          ENDIF

          EXIT
        ENDIF
      ENDDO
    ENDDO

  ENDIF

! Get orbit model description (taken from first NEQ!)
! ---------------------------------------------------
  if (parOrb%nOrb > 0 ) THEN
    CALL gtorbmd(opt%orbfil(1,1), orbdsc)
  end if

! Print options of orbital elements
! ---------------------------------
! print a priori information
  IF (parOrb%nOrb > 0)  THEN
    WRITE(lfnprt,'(2(A,/))') ' Orbital elements: ',' ----------------'

    IF (opt%trafo == 1) WRITE(lfnprt,'(A,/)') ' Long-arc combination requested'

    IF (opt%splitDyn == 1) THEN
       WRITE(lfnprt,'(A,/)') ' Dynamic orbit parameters: Keep sub-arcs from NEQs'
    ELSE
       WRITE(lfnprt,'(A,/)') ' Dynamic orbit parameters: Stacking'
    ENDIF

    WRITE(lfnprt,'(A,/,A)')                                                 &
      ' File  Standard orbits                  Radiation pressure '      // &
      'coefficients  Orbital elements',                                     &
      ' ----------------------------------------------------------------'// &
      '-------------------------------------------------------------------'

    DO iFil = 1, SIZE(opt%neqFileName)
      WRITE(lfnprt,'(I4,2X,3(1X,A))') iFil,opt%orbfil(1:2,iFil)(1:32),  &
                                      opt%orbfil(4,iFil)(1:32)
    ENDDO
    WRITE(lfnprt,'(/)')

! print a priori sigma
    CALL prisig(3,  sigOrb, parOrb%nOrb, parOrb%seqorb)
  ENDIF

! Print options of stochastic orbit parameters
! --------------------------------------------
! print a priori information
  IF (opt%nstcep > 0) THEN
    WRITE(lfnprt,'(2(A,/))')                                                &
      ' Stochastic orbit parameters:',                                      &
      ' ---------------------------'
    IF (opt%numStc == -1 .AND. SIZE(opt%neqFileName) > 1 ) THEN
      WRITE(lfnprt,'(A,3(A,2X),/)')                                         &
        ' Stochastic pulses are added at the NEQ boundaries: ',             &
        (frctyp(mod(opt%rsw(ityp),10)),ityp=1,opt%nstcep)
    ENDIF

! print a priori sigma
    DO ityp=1,opt%nstcep
      hlptyp(ityp)=mod(opt%rsw(ityp),10)
    ENDDO
    CALL prisig(11, sigStc, opt%nstcep, hlptyp)
  ENDIF

! Store sigmas in opt%sigma
! -------------------------
  DEALLOCATE(opt%sigma,stat=iac)
  ALLOCATE(opt%sigma(nSig+oldSigTyp),stat=iac)
  CALL alcerr(iac,'opt%sigma',(/nSig+oldSigTyp/),'aoptorb')

  nSig = 0
  DO iSig = 1,maxSigTyp
    IF (locSig(iSig)%value /= 0d0) THEN
      nSig = nSig+1
      opt%sigma(nSig) = locSig(iSig)
    ENDIF
  ENDDO

! Some other orbit parameters
! ---------------------------
  opt%numdyn =  0
  opt%numgap =  0
  opt%numnac =  0

! Error if same STD for different NEQs used
  IF (opt%trafo == 1) THEN
    nFil = SIZE(opt%neqFileName)
    DO iFil = 1,nFil-1
      DO jFil = iFil+1,nFil
        IF (opt%orbFil(1,iFil) == opt%orbFil(1,jFil)) THEN
          WRITE(lfnerr,'(/,A,/,A,A,/,A,/,A)')                               &
           ' *** SR AOPTORB: The use of the same standard orbit file:',     &
           '                 ',(opt%orbFil(1,iFil)),                        &
           '                 for different normal equation files is ',      &
           '                 not allowed for long-arc combination.'
          CALL EXITRC(2)
        ENDIF
      ENDDO
    ENDDO
  ENDIF

! Clean up
! --------
  DEALLOCATE (parOrb%epoOrb,stat=iac)
  DEALLOCATE (locSig)
  DEALLOCATE (keyValue,stat=iac)

  RETURN
END SUBROUTINE aoptorb

END MODULE
