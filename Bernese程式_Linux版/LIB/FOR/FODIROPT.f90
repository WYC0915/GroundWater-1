MODULE s_FODIROPT
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE fodiropt(opt,sCore)

! -------------------------------------------------------------------------
! Purpose:    Read input panel options (FODITS.INP)
!
! Author:     Luca Ostini
!
! Created:    14-Aug-2008
!
! Changes:    14-Aug-2008 LO: Created this file
!             02-Oct-2008 LO: First revision
!             09-Oct-2008 LO: Third revision
!             05-Dec-2008 LO: Fourth revisio: velocity changes allowed
!             07-Jan-2009 LO: SignifLevel adapted for the 32-bits compiler
!             09-Jan-2009 LO: VARIANCE_ONLY to STD_DEV_ONLY changed in panel
!             11-Feb-2009 LO: Fifth revision: major changes
!             11-Mar-2009 LO: Fourier Series implemented
!             20-Aug-2009 LO: Addition of outliers due to too few observations
!             25-Sep-2009 LO: Changes for F90 consistency
!             18-Nov-2009 LO: Major changes for consistency
!             21-Dec-2009 LO: FFT removed and several changes apported
!             01-Feb-2010 LO: Additional criteria addded for velo and peri
!             02-Mar-2010 LO: Major changes do to elimination of FODISUBR
!             07-Apr-2010 LO: Major changes do to algorithm and output file
!             16-Jun-2010 LO: Time series diveded by component
!             26-Aug-2010 LO: Architectural changes
!             03-Jan-2010 LO: INTENT(INOUT) removed and bug fixed
!             24-May-2011 LO: New update of ADDNEQ2 meta-data
!             05-Jul-2011 LO: m0 set to 0.001 meters
!             19-Jul-2011 LO: Test datum defintion added
!             19-Sep-2012 RD: Use P_FODITS with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Used Modules
! ------------
! structures, variables:
  USE m_bern,    ONLY: i4b, r8b, lfnerr, shortLineLength, keyValueLength, &
                       staNameLength
  USE d_const,   ONLY: pi
  USE p_fodits,  ONLY: t_opt, t_sCore, &
                       inpres, defaultdval, inpcrd

! operator, methods:
!  USE d_debug,   ONLY: debug_entry, debug_exit
!  USE m_epoch,   ONLY: OPERATOR(.epochToReal.)

! subroutines, functions:
  USE f_djul
  USE s_exitrc
  USE s_alcerr
  USE s_ckoptb
  USE s_ckopti
  USE s_ckoptr
  USE s_ckoptu
  USE s_gtflna
  USE s_readkeys

! no implicit
  IMPLICIT NONE

! subroutine name
  CHARACTER(LEN=shortLineLength), PARAMETER    :: srName = 'fodiropt'


! List of Arguments
! -----------------
! input:
  TYPE(t_opt)                    :: opt          ! Option structure
  TYPE(t_sCore)                  :: sCore        ! General TS struct

! input/output:

! output:


! Local Types
! -----------


! Local Parameters
! ----------------


! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), &
    DIMENSION(:,:),POINTER       :: hlpStr
  CHARACTER(LEN=keyValueLength), &
    DIMENSION(:), POINTER        :: keyValue

  INTEGER(i4b)                   :: irc
  INTEGER(i4b)                   :: iac
  INTEGER(i4b)                   :: ios
  INTEGER(i4b)                   :: irCode
  INTEGER(i4b)                   :: ircSav
  INTEGER(i4b)                   :: iCrd
  INTEGER(i4b)                   :: iSta
  INTEGER(i4b)                   :: iPer
  INTEGER(i4b)                   :: year, month

  REAL(r8b)                      :: day
  REAL(r8b)                      :: kappa0
  REAL(r8b)                      :: kappa2


! Call debug routine
! ------------------
!  CALL debug_entry(srName)

! Initialization of all variables
! -------------------------------
  NULLIFY(keyValue)
  NULLIFY(hlpStr)
  irc = 0
  iac = 0
  irCode = 0

  ! Read Panel 0 of the FODITS program
  ! -------------------------------------
  CALL readkeys('TITLE', keyValue, irc)
  opt%title = keyValue(1)
  ! MENUAUX
  opt%rdOnlyStaNames = 0

  ! Read Panel 1 of the FODITS program
  ! -------------------------------------
  ! INPUT FILES
  opt%inPltFileVciEna = 0
  CALL readkeys('SEL_INP_TS_TYPE', keyValue, irc)
  ! CRD+VEL+PLT as input TS
  opt%inPltFileViM0 = 0.001D0
  IF ( irc == 0 .AND. keyValue(1) == 'RESIDUALS' )THEN
     opt%selInpTsType = inpRes
     CALL gtflna(0, 'IN_PLT_FILE',  opt%inPltFile , irc)
     CALL readkeys('IN_PLT_FILE_VCI_ENA', keyValue, irc)
     IF ( irc == 0 .AND. keyValue(1) == 'NONE' )THEN
        opt%inPltFileVciEna = 0
     ELSE IF( irc == 0 .AND. keyValue(1) == 'STDDEV_ONLY' )THEN
        opt%inPltFileVciEna = 1
     ELSE
        opt%inPltFileVciEna = 2
     END IF
     CALL readkeys('IN_PLT_FILE_VI_M0', keyValue, irc)
     CALL ckoptr(1, 'IN_PLT_FILE_VI_M0', keyValue, srName, &
          'with a priori sigma of unit weight', irc, irCode, &
          empty=defaultDVal, ge=0.0D0, result1=opt%inPltFileViM0)
     CALL gtflna(0, 'IN_CRD_FILE',  opt%inCrdFile , irc)
     CALL gtflna(0, 'IN_VEL_FILE',  opt%inVelFile , irc)
     opt%nCrds = 0
  ! CRDs as input TS
  ELSE IF ( irc == 0 .AND. keyValue(1) == 'COORDINATES' )THEN
     opt%selInpTsType = inpCrd
     CALL readkeys('IN_CRD_FILES', keyValue, irc)
     opt%nCrds = SIZE(keyValue)
     ALLOCATE(opt%crdFileName(opt%nCrds), stat=iac)
     CALL alcerr(iac, 'opt%crdFileName', (/opt%nCrds/), srName)
     IF ( irc == 0 )THEN
        DO iCrd = 1, opt%nCrds
           opt%crdFileName(iCrd) = keyValue(iCrd)
        END DO
     ELSE
        WRITE(lfnerr,'(/,A,/)') &
             ' *** SR FODIROPT: Problem with filenames of the input CRDs.'
        CALL exitrc(2)
     ENDIF
  END IF
  IF( opt%inPltFileVciEna == 0 )THEN
     opt%inPltFileVciEna = 0
  END IF

  ! PREDEFINED EVENTS
  CALL gtflna(0, 'IN_EVL_FILE', opt%inEvlFile, irc)
  CALL gtflna(0, 'IN_STA_FILE', opt%inStaFile, irc)
  CALL gtflna(0, 'IN_ERQ_FILE', opt%inErqFile, irc)

  ! Read Panel 1.1 of the FODITS program
  ! -------------------------------------
  ! GENERAL INPUT FILES
  CALL gtflna(1, 'CONST', opt%const, irc)
  CALL gtflna(1, 'DATUM', opt%datum, irc)

  ! Read Panel 1.2 of the FODITS program
  ! -------------------------------------
  ! VARIABLES FOR A PRIORI EVENTS
  CALL readkeys('GVAR_ERQ_FACT_A', keyValue, irc)
  CALL ckoptr(1,'GVAR_ERQ_FACT_A', keyValue, srName, &
       'Earthquake factor A', irc, irCode, &
       empty=-5.60D0, result1=opt%gvarErqFactA)
  CALL readkeys('GVAR_ERQ_FACT_B', keyValue, irc)
  CALL ckoptr(1,'GVAR_ERQ_FACT_B', keyValue, srName, &
       'Earthquake factor B', irc, irCode, &
       empty=+2.17D0, result1=opt%gvarErqFactB)
  CALL readkeys('GVAR_ERQ_MIN_DIST', keyValue, irc)
  CALL ckoptr(1,'GVAR_ERQ_MIN_DIST', keyValue, srName, &
       'Minimal distance between quakes (days)', irc, irCode, &
       empty=3.0D0, result1=opt%gvarErqMinDist)
  ! RESIDULAS TIME SERIES
  CALL ckoptb(1, (/'IN_CRD_FILE_ADD_PLT'/), srName, &
       'Apply coordinates to the residuals', irCode, &
       result1=opt%inCrdFileAddPlt )
  CALL ckoptb(1, (/'IN_VEL_FILE_ADD_PLT'/), srName, &
       'Add velocities to the residuals', irCode, &
       result1=opt%inVelFileAddPlt )
  ! ALGORITHM VARIABLES
  CALL readkeys('GVAR_MAX_NR_ITER', keyValue, irc)
  CALL ckopti(1,'GVAR_MAX_NR_ITER',keyValue, srName,&
       'Maximal number of iteration steps',irc,irCode,empty=50, &
       ge=0,result1=opt%gvarMaxNrIterStep)
  ALLOCATE(sCore%ctr%modNumIter(opt%gvarMaxNrIterStep),stat=iac)
  CALL alcerr(iac,'sCore%ctr%modNumIter',(/opt%gvarMaxNrIterStep/),srName)
  ALLOCATE(sCore%ctr%modNumIterOutl(opt%gvarMaxNrIterStep),stat=iac)
  CALL alcerr(iac,'sCore%ctr%modNumIterOutl',(/opt%gvarMaxNrIterStep/),srName)
  CALL readkeys('GVAR_MAX_NR_SCRN', keyValue, irc)
  CALL ckopti(1,'GVAR_MAX_NR_SCRN',keyValue, srName,&
       'Maximal number of screening steps',irc,irCode,empty=50, &
       ge=0,result1=opt%gvarMaxNrScrnStep)
  ALLOCATE(sCore%ctr%modNumScrn(opt%gvarMaxNrScrnStep),stat=iac)
  CALL alcerr(iac,'sCore%ctr%modNumScrn',(/opt%gvarMaxNrScrnStep/),srName)
  ALLOCATE(sCore%ctr%modNumScrnOutl(opt%gvarMaxNrScrnStep),stat=iac)
  CALL alcerr(iac,'sCore%ctr%modNumScrnOutl',(/opt%gvarMaxNrScrnStep/),srName)
  ! NEW IDENTIFIED ELEMENTS
  CALL readkeys('MOD_N_NEW_OUTL', keyValue, irc)
  CALL ckopti(1,'MOD_N_NEW_OUTL',keyValue, srName,&
       'Number of outliers to propose',irc,irCode,empty=0, &
       ge=0,le=9999,result1=opt%modNNewOutl)
  CALL readkeys('MOD_N_OUTL_RMS', keyValue, irc)
  CALL ckoptr(1,'MOD_N_OUTL_RMS', keyValue, srName, &
       'Times RMS to consider observations outliers', irc, irCode, &
       empty=100.0D0, result1=opt%modNOutlRms)

  ! Read Panel 2 of the FODITS program
  ! -------------------------------------
  ! RESULT FILES
  CALL gtflna(0, 'OUT_EVL_FILE',  opt%outEvlFile , irc)
  CALL gtflna(0, 'OUT_STA_FILE_FOR_ADDNEQ2', opt%outStaFileForAddneq2, irc)
  ! OUTPUT FILES
  CALL gtflna(0, 'OUT_PLT_FILE',  opt%outPltFile , irc)
  ! GENERAL OUTPUT FILES
  CALL ckoptb(1, (/'OUT_FILE_VERBOSE'/), srName, &
       'Verbose mode', irCode, &
       result1=opt%outFileVerbose )
  CALL ckoptb(1, (/'OUT_FILE_VERBOSET'/), srName, &
       'Verbose mode Extra - iter', irCode, &
       result1=opt%outFileVerboseT )
  CALL ckoptb(1, (/'OUT_FILE_VERBOSETT'/), srName, &
       'Verbose mode Extra - scrn', irCode, &
       result1=opt%outFileVerboseTT )

  ! Read Panel 3 of the FODITS program
  ! -------------------------------------
  ! INPUT STATION NAMES
  CALL readkeys("IN_TRUNC_STA_NAME",keyValue,irc)
  IF (keyValue(1)=="NO") THEN
    opt%inTruncStaName = staNameLength
  ELSE IF (keyValue(1)=="YES") THEN
    opt%inTruncStaName = 14
  ELSE
    CALL ckopti(1,'IN_TRUNC_STA_NAME',keyValue,'sr aoptfil',&
                'Truncate station names',irc,irCode,empty=staNameLength, &
                ge=0,le=staNameLength,result1=opt%inTruncStaName)
  ENDIF
  CALL readkeys('IN_MAN_STA', keyValue, irc)
  opt%nStaSel = SIZE(keyValue)
  IF( keyValue(1) == '' ) opt%nStaSel = 0
  ALLOCATE(opt%inStaSelection(opt%nStaSel), stat=iac)
  CALL alcerr(iac,'opt%inStaSelection',(/opt%nStaSel/),srName)
  IF ( irc == 0 )THEN
     DO iSta = 1,opt%nStaSel
        opt%inStaSelection(iSta) = keyValue(iSta)
     END DO
  ELSE
     WRITE(lfnerr,'(/,A,/)') &
          ' *** SR FODIROPT: Problem reading manaul selection of station.'
     CALL exitrc(2)
  ENDIF
  CALL readkeys('IN_MIN_NR_OBS', keyValue, irc)
  CALL ckopti(1,'IN_MIN_NR_OBS',keyValue, srName,&
       'Min number of observations',irc,irCode,empty=1, &
       ge=0,le=999,result1=opt%inMinNrObs)

  ! TEST EVENTS READ IN INPUT FILES
  CALL readkeys('TST_EVL_TST', keyValue, irc)
  IF ( irc == 0 .AND. keyValue(1) == 'NONE' )THEN
     opt%tstEvlTst = 0
  ELSE IF( irc == 0 .AND. keyValue(1) == 'TEST' )THEN
     opt%tstEvlTst = 1
  ELSE
     opt%tstEvlTst = 2
  END IF
  CALL readkeys('TST_STA_RENAMINGS', keyValue, irc)
  IF ( irc == 0 .AND. keyValue(1) == 'NONE' )THEN
     opt%tstStaRenamings = 0
  ELSE IF( irc == 0 .AND. keyValue(1) == 'TEST' )THEN
     opt%tstStaRenamings = 1
  ELSE
     opt%tstStaRenamings = 2
  END IF
  CALL readkeys('TST_REC_TYPE_CHANGES', keyValue, irc)
  IF ( irc == 0 .AND. keyValue(1) == 'NONE' )THEN
     opt%tstRecTypeChanges = 0
  ELSE IF( irc == 0 .AND. keyValue(1) == 'TEST' )THEN
     opt%tstRecTypeChanges = 1
  ELSE
     opt%tstRecTypeChanges = 2
  END IF
  CALL readkeys('TST_REC_NUMBER_CHANGES', keyValue, irc)
  IF ( irc == 0 .AND. keyValue(1) == 'NONE' )THEN
     opt%tstRecNumberChanges = 0
  ELSE IF( irc == 0 .AND. keyValue(1) == 'TEST' )THEN
     opt%tstRecNumberChanges = 1
  ELSE
     opt%tstRecNumberChanges = 2
  END IF
  CALL readkeys('TST_ANT_NAME_CHANGES', keyValue, irc)
  IF ( irc == 0 .AND. keyValue(1) == 'NONE' )THEN
     opt%tstAntTypeChanges = 0
  ELSE IF( irc == 0 .AND. keyValue(1) == 'TEST' )THEN
     opt%tstAntTypeChanges = 1
  ELSE
     opt%tstAntTypeChanges = 2
  END IF
  CALL readkeys('TST_ANT_TYPE_CHANGES', keyValue, irc)
  IF ( irc == 0 .AND. keyValue(1) == 'NONE' )THEN
     opt%tstAntNumberChanges = 0
  ELSE IF( irc == 0 .AND. keyValue(1) == 'TEST' )THEN
     opt%tstAntNumberChanges = 1
  ELSE
     opt%tstAntNumberChanges = 2
  END IF
  CALL readkeys('TST_ECC_CHANGES', keyValue, irc)
  IF ( irc == 0 .AND. keyValue(1) == 'NONE' )THEN
     opt%tstEccChanges = 0
  ELSE IF( irc == 0 .AND. keyValue(1) == 'TEST' )THEN
     opt%tstEccChanges = 1
  ELSE
     opt%tstEccChanges = 2
  END IF

  ! Read Panel 4 of the FODITS program
  ! -------------------------------------
  ! PRE-ANALYSIS STEP
  CALL ckoptb(1, (/'MOD_PRE_DAT_TEST'/), srName, &
       'Test datum definition', irCode, &
       result1=opt%modPreDatTest )
  ! VELOCITY CHANGES
  CALL readkeys('MIN_TIME_INT_VEL', keyValue, irc)
  CALL ckoptr(1, 'MIN_TIME_INT_VEL', keyValue, srName, &
       'Interval length for velocities estimation', irc, irCode, &
       empty=0.0D0, ge=0.0D0, result1=opt%minIntervForVel)
  opt%minIntervForVel = opt%minIntervForVel * 365.25D0

  ! PERIODIC FUNCTIONS
  CALL ckoptb(1, (/'MOD_PER_ANN'/), srName, &
       'Propose seasonal signals', irCode, &
       result1=opt%modPerAnn )
  CALL ckoptb(1, (/'MOD_ADD_ENA'/), srName, &
       'Enable additional periods', irCode, &
       result1=opt%modAddEna )
  IF( opt%modAddEna == 1 )THEN
     CALL readKeys('MOD_ADD_PER_PARAM',keyValue,irc)
     ALLOCATE(hlpStr(1,SIZE(keyValue)),stat=irc)
     CALL alcerr(irc,'hlpStr',(/1,SIZE(keyValue)/),srName)
     ircSav = irCode
     CALL ckoptu(1,'MOD_ADD_PER_PARAM',keyValue,srName, &
          'Set additional periods',irc,irCode,1, &
          maxVal=SIZE(hlpStr(1,:)),result2=hlpStr)
     ircSav = irCode-ircSav
     ALLOCATE(opt%modAddPerParam(SIZE(keyValue)),stat=irc)
     CALL alcerr(irc,'opt%modAddPerParam',(/SIZE(keyValue)/),srName)
     opt%nAddPerParam = SIZE(keyValue)

     CALL ckoptr(1,'MOD_ADD_PER_PARAM',hlpStr(1,:),srName, &
          'Set additional periods',ircSav,irCode, &
          colTit='Period in days',maxVal=SIZE(hlpStr(1,:)), &
          gt=0d0,result2=opt%modAddPerParam(:))
     ircSav = irCode-ircSav
     DEALLOCATE(hlpStr,stat=irc)
     DO iPer = 1,opt%nAddPerParam
        opt%modAddPerParam(iPer) = 2.0D0*pi/opt%modAddPerParam(iPer)
     END DO
  ELSE
     opt%nAddPerParam = 0
  END IF

  ! Read Panel 4.1 of the FODITS program
  ! -------------------------------------
  ! REFERENCE DATUM
  CALL gtflna(0, 'IN_DATUM_CRD_FILE',  opt%inDatumCrdFile , irc)
  CALL gtflna(0, 'IN_DATUM_VEL_FILE',  opt%inDatumVelFile , irc)
  CALL gtflna(0, 'IN_DATUM_FIX_FILE',  opt%inDatumFixFile , irc)
  ! HELMERT TRANSFORMATION
  CALL ckoptb(1, (/'HLM_1'/), srName, &
       'Parameters to be computed: shift 1', irCode, &
       result1=opt%datHlmTx )
  CALL ckoptb(1, (/'HLM_2'/), srName, &
       'Parameters to be computed: shift 2', irCode, &
       result1=opt%datHlmTy )
  CALL ckoptb(1, (/'HLM_3'/), srName, &
       'Parameters to be computed: shift 3', irCode, &
       result1=opt%datHlmTz )
  CALL ckoptb(1, (/'HLM_4'/), srName, &
       'Parameters to be computed: rot 1', irCode, &
       result1=opt%datHlmRx )
  CALL ckoptb(1, (/'HLM_5'/), srName, &
       'Parameters to be computed: rot 2', irCode, &
       result1=opt%datHlmRy )
  CALL ckoptb(1, (/'HLM_6'/), srName, &
       'Parameters to be computed: rot 3', irCode, &
       result1=opt%datHlmRz )
  CALL ckoptb(1, (/'HLM_7'/), srName, &
       'Parameters to be computed: scale', irCode, &
       result1=opt%datHlmSc )
  ! OUTLIER REJECTION FOR STATION COORDINATES
  CALL ckoptb(1, (/'DAT_REJECT_CRD'/), srName, &
       'Enable outlier rejection', irCode, &
       result1=opt%datRejectCrd )
  CALL readkeys('DAT_NLIMIT_CRD', keyValue, irc)
  CALL ckoptr(1, 'DAT_NLIMIT_CRD', keyValue, srName, &
       'Outlier criteria north component', irc, irCode, &
       empty=0.0D0, ge=0.0D0, result1=opt%datNLimitCrd)
  opt%datNLimitCrd = opt%datNLimitCrd / 1.0D3
  CALL readkeys('DAT_ELIMIT_CRD', keyValue, irc)
  CALL ckoptr(1, 'DAT_ELIMIT_CRD', keyValue, srName, &
       'Outlier criteria east component', irc, irCode, &
       empty=0.0D0, ge=0.0D0, result1=opt%datELimitCrd)
  opt%datELimitCrd = opt%datELimitCrd / 1.0D3
  CALL readkeys('DAT_ULIMIT_CRD', keyValue, irc)
  CALL ckoptr(1, 'DAT_ULIMIT_CRD', keyValue, srName, &
       'Outlier criteria up component', irc, irCode, &
       empty=0.0D0, ge=0.0D0, result1=opt%datULimitCrd)
  opt%datULimitCrd = opt%datULimitCrd / 1.0D3
  CALL readkeys('DAT_REJECT_OUTL_CRD', keyValue, irc)
  IF ( irc == 0 .AND. keyValue(1) == 'NONE' )THEN
     opt%datRejectOutlCrd = 0
  ELSE IF( irc == 0 .AND. keyValue(1) == 'TEST' )THEN
     opt%datRejectOutlCrd = 1
  ELSE
     opt%datRejectOutlCrd = 2
  END IF

  ! Read Panel 5 of the FODITS program
  ! -------------------------------------
  ! STATISTICAL TESTS
  CALL readkeys('MOD_TEST_VALUE', keyValue, irc)
  CALL ckoptr(1, 'MOD_TEST_VALUE', keyValue, srName, &
       'Abort criterion', irc, irCode, &
       empty=0.0D0, ge=0.0D0, result1=opt%modTestValue)
  CALL readkeys('MOD_OUTL_TEST_VALUE', keyValue, irc)
  CALL ckoptr(1, 'MOD_OUTL_TEST_VALUE', keyValue, srName, &
       'Test value for outliers', irc, irCode, &
       empty=0.0D0, ge=0.0D0, result1=opt%modOutlTestValue)

  ! MINIMAL ADDITIONAL CRITERIA TO TEST PARAMETERS
  CALL readkeys('MOD_JUMP_SPAC', keyValue, irc)
  CALL ckoptr(1, 'MOD_JUMP_SPAC', keyValue, srName, &
       'Size for space component', irc, irCode, &
       empty=0.0D0, ge=0.0D0, result1=opt%modJumpSpac)
  CALL readkeys('MOD_JUMP_SING', keyValue, irc)
  CALL ckoptr(1, 'MOD_JUMP_SING', keyValue, srName, &
       'Size for singe component', irc, irCode, &
       empty=0.0D0, ge=0.0D0, result1=opt%modJumpSing)
  CALL readkeys('MOD_JUMP_HORI', keyValue, irc)
  CALL ckoptr(1, 'MOD_JUMP_HORI', keyValue, srName, &
       'Size for horizonatal component', irc, irCode, &
       empty=0.0D0, ge=0.0D0, result1=opt%modJumpHori)
  CALL readkeys('MOD_JUMP_VERT', keyValue, irc)
  CALL ckoptr(1, 'MOD_JUMP_VERT', keyValue, srName, &
       'Size for vertical component', irc, irCode, &
       empty=0.0D0, ge=0.0D0, result1=opt%modJumpVert)

  CALL readkeys('MOD_VELO_SPAC', keyValue, irc)
  CALL ckoptr(1, 'MOD_VELO_SPAC', keyValue, srName, &
       'Size for space component', irc, irCode, &
       empty=0.0D0, ge=0.0D0, result1=opt%modVeloSpac)
  opt%modVeloSpac = opt%modVeloSpac / 365.25D0
  CALL readkeys('MOD_VELO_SING', keyValue, irc)
  CALL ckoptr(1, 'MOD_VELO_SING', keyValue, srName, &
       'Size for singe component', irc, irCode, &
       empty=0.0D0, ge=0.0D0, result1=opt%modVeloSing)
  opt%modVeloSing = opt%modVeloSing / 365.25D0
  CALL readkeys('MOD_VELO_HORI', keyValue, irc)
  CALL ckoptr(1, 'MOD_VELO_HORI', keyValue, srName, &
       'Size for horizonatal component', irc, irCode, &
       empty=0.0D0, ge=0.0D0, result1=opt%modVeloHori)
  opt%modVeloHori = opt%modVeloHori / 365.25D0
  CALL readkeys('MOD_VELO_VERT', keyValue, irc)
  CALL ckoptr(1, 'MOD_VELO_VERT', keyValue, srName, &
       'Size for vertical component', irc, irCode, &
       empty=0.0D0, ge=0.0D0, result1=opt%modVeloVert)
  opt%modVeloVert = opt%modVeloVert / 365.25D0

  CALL readkeys('MOD_OUTL_SPAC', keyValue, irc)
  CALL ckoptr(1, 'MOD_OUTL_SPAC', keyValue, srName, &
       'Size for space component', irc, irCode, &
       empty=0.0D0, ge=0.0D0, result1=opt%modOutlSpac)
  CALL readkeys('MOD_OUTL_SING', keyValue, irc)
  CALL ckoptr(1, 'MOD_OUTL_SING', keyValue, srName, &
       'Size for singe component', irc, irCode, &
       empty=0.0D0, ge=0.0D0, result1=opt%modOutlSing)
  CALL readkeys('MOD_OUTL_HORI', keyValue, irc)
  CALL ckoptr(1, 'MOD_OUTL_HORI', keyValue, srName, &
       'Size for horizonatal component', irc, irCode, &
       empty=0.0D0, ge=0.0D0, result1=opt%modOutlHori)
  CALL readkeys('MOD_OUTL_VERT', keyValue, irc)
  CALL ckoptr(1, 'MOD_OUTL_VERT', keyValue, srName, &
       'Size for vertical component', irc, irCode, &
       empty=0.0D0, ge=0.0D0, result1=opt%modOutlVert)

  CALL readkeys('MOD_PERI_SPAC', keyValue, irc)
  CALL ckoptr(1, 'MOD_PERI_SPAC', keyValue, srName, &
       'Size for space component', irc, irCode, &
       empty=0.0D0, ge=0.0D0, result1=opt%modPeriSpac)
  CALL readkeys('MOD_PERI_SING', keyValue, irc)
  CALL ckoptr(1, 'MOD_PERI_SING', keyValue, srName, &
       'Size for singe component', irc, irCode, &
       empty=0.0D0, ge=0.0D0, result1=opt%modPeriSing)
  CALL readkeys('MOD_PERI_HORI', keyValue, irc)
  CALL ckoptr(1, 'MOD_PERI_HORI', keyValue, srName, &
       'Size for horizonatal component', irc, irCode, &
       empty=0.0D0, ge=0.0D0, result1=opt%modPeriHori)
  CALL readkeys('MOD_PERI_VERT', keyValue, irc)
  CALL ckoptr(1, 'MOD_PERI_VERT', keyValue, srName, &
       'Size for vertical component', irc, irCode, &
       empty=0.0D0, ge=0.0D0, result1=opt%modPeriVert)

  ! Read Panel 6 of the FODITS program
  ! -------------------------------------
  ! IDENTIFICATION STEP
  CALL ckoptb(1, (/'MOD_NEW_JUMP_IDENTIF'/), srName, &
       'Identify new jump', irCode, &
       result1=opt%modNewJumpIdentify )
  CALL ckoptb(1, (/'MOD_NEW_VELO_IDENTIF'/), srName, &
       'Identify new velo', irCode, &
       result1=opt%modNewVeloIdentify )
  CALL ckoptb(1, (/'MOD_NEW_OUTL_IDENTIF'/), srName, &
       'Identify new outl', irCode, &
       result1=opt%modNewOutlIdentify )
  CALL ckoptb(1, (/'MOD_NEW_PERI_IDENTIF'/), srName, &
       'Identify new peri', irCode, &
       result1=opt%modNewPeriIdentify )
  ! NEW IDENTIFIED ELEMENTS
  CALL ckoptb(1, (/'MOD_ADD_VELO_A_J'/), srName, &
       'A velocity change after new jumps', irCode, &
       result1=opt%modAddVeloAJ )
  ! SEARCH FOR PERIODIC FUNCTIONS
  CALL readkeys('SAN_NLINES', keyValue, irc)
  CALL ckopti(1,'SAN_NLINES',keyValue, srName,&
       'Number of spectral lines',irc,irCode,empty=0, &
       ge=0,le=9999,result1=opt%sanNLines)
  CALL readkeys('SAN_MIN_PER', keyValue, irc)
  CALL ckoptr(1,'SAN_MIN_PER', keyValue, srName, &
       'Minimal period', irc, irCode, &
       empty=1.0D0,   ge=0.0D0, result1=opt%sanMinPeriod)
  CALL readkeys('SAN_MAX_PER', keyValue, irc)
  CALL ckoptr(1,'SAN_MAX_PER', keyValue, srName, &
       'Maximal period', irc, irCode, &
       empty=400.0D0, ge=0.0D0, result1=opt%sanMaxPeriod)
  ! Allocate
  sCore%san%nPer = opt%sanNLines
  ALLOCATE(sCore%san%per(sCore%san%nPer),stat=iac)
  CALL alcerr(iac,'per',(/sCore%san%nPer/),srName)
  ! Assign periods
  kappa0 = 2*pi/opt%sanMaxPeriod
  IF( sCore%san%nPer == 1 )THEN
     kappa2 = 2*pi*(1.0D0/opt%sanMinPeriod-1.0D0/opt%sanMaxPeriod)
  ELSE
     kappa2 = 2*pi*(1.0D0/opt%sanMinPeriod-1.0D0/opt%sanMaxPeriod) / &
              (sCore%san%nPer-1)
  END IF
  DO iPer = 1,sCore%san%nPer
     sCore%san%per(iPer) = kappa0 + kappa2*(iPer-1)
  END DO

  ! Read Panel 7 of the FODITS program
  ! -------------------------------------
  ! GENERAL OPTIONS
  opt%outTimRef = 0.0D0
  CALL readkeys('OUT_TIMREF', keyValue, irc)
  IF (TRIM(keyValue(1)) /= '' ) THEN
    IF (irc == 0) READ(keyValue(1), *, iostat=ios) year, month, day
    IF (year < 0    .OR. month < 0   .OR. month > 12 .OR.                  &
         day < 0.d0 .OR. day > 31.d0 .OR. irc /= 0   .OR. ios /= 0) THEN
     WRITE(lfnerr,'(/,A,/)') &
          ' *** SR FODIROPT: Wrong entry for reference epoch.'
     CALL exitrc(2)
    ELSE
       opt%outTimRef = djul(year, month, day)
    ENDIF
  ENDIF
  ! REFERENCE SITES
  CALL readkeys('MIN_TIME_INT_FIX', keyValue, irc)
  CALL ckoptr(1,'MIN_TIME_INT_FIX', keyValue, srName, &
       'Minimal interval length', irc, irCode, &
       empty=1.0D0, ge=0.0D0, result1=opt%minTimeIntLength)
  opt%minTimeIntLength = opt%minTimeIntLength * 365.25D0
  CALL readkeys('MIN_PERC_DATA_FIX', keyValue, irc)
  CALL ckoptr(1,'MIN_PERC_DATA_FIX', keyValue, srName, &
       '', irc, irCode, &
       empty=50.0D0, ge=0.0D0, result1=opt%minPercDataFix)
  ! UPDATE ADDNEQ2 INPUT FILES
  CALL gtflna(0, 'IN_CRD_FILE_FOR_ADDNEQ2',  opt%inCrdFileForAddneq2, irc)
  CALL gtflna(0, 'IN_VEL_FILE_FOR_ADDNEQ2',  opt%inVelFileForAddneq2, irc)
  CALL gtflna(0, 'IN_FIX_FILE_FOR_ADDNEQ2',  opt%inFixFileForAddneq2, irc)
  CALL gtflna(0, 'IN_SIG_FILE_FOR_ADDNEQ2',  opt%inSigFileForAddneq2, irc)
  CALL gtflna(0, 'OUT_CRD_FILE_FOR_ADDNEQ2', opt%outCrdFileForAddneq2, irc)
  CALL gtflna(0, 'OUT_VEL_FILE_FOR_ADDNEQ2', opt%outVelFileForAddneq2, irc)
  CALL gtflna(0, 'OUT_FIX_FILE_FOR_ADDNEQ2', opt%outFixFileForAddneq2, irc)
  CALL gtflna(0, 'OUT_SIG_FILE_FOR_ADDNEQ2', opt%outSigFileForAddneq2, irc)

  ! UPDATE STATION INFORMATION
  CALL readkeys('UPD_STA_NDAYS', keyValue, irc)
  IF ( irc == 0 .AND. keyValue(1) == '3' )THEN
     opt%updStaNDays = 3
  ELSE IF( irc == 0 .AND. keyValue(1) == '7' )THEN
     opt%updStaNDays = 7
  ELSE
     opt%updStaNDays = 1
  END IF
  opt%gvarCstrCrd = 99.9999D0
  opt%gvarCstrVel = 0.00001D0

  ! Check whether the reading proc. has been executed correctly
  ! -----------------------------------------------------------
  IF( irCode/=0 )THEN
     WRITE(lfnerr,'(/,A,/)') &
          ' *** SR FODIROPT: reading procedure finished wrongly.'
     CALL exitrc(2)
  END IF

!  CALL debug_exit(srName)
  RETURN

END SUBROUTINE fodiropt

END MODULE s_FODIROPT

