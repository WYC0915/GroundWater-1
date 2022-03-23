MODULE s_AOPTPRE
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE aoptpre(opt,namList)

! -------------------------------------------------------------------------
! Purpose:    Reads input options for ADDNEQ2:
!             preelimination of parameters
!
! Remark:     It is assumed that this subroutine is the FIRST one
!             reading values for "opt%elimi"
!
! Author:     M. Meindl
!
! Created:    10-Jul-2001
!
! Changes:    19-Oct-2001 RD: Enable pre-elimination of dcbs, trp-gradients
!             19-Oct-2001 RD: Use ckopt-subroutines
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             12-Feb-2002 MM: Allow file exceptions for BEFORE_STACKING
!                               and EXCEPT_OF_BOUNDARIES
!                             dT for boundaries removed (decommented)
!             13-Feb-2002 MM: Enable preelimi. for GIM, sat and rec DCBs
!             03-Apr-2002 RD: 'F' and 'L' for file exceptions
!             27-Nov-2002 DT: increase maxEliTyp 16 -> 20 for biases:
!                             range bias L1, range bias L2, time bias L1,
!                             time bias L2
!             23-Apr-2003 CU: Nullify local pointers
!             14-Aug-2003 HU: Read spv preelimination options
!             26-Aug-2003 RD: Correct pointer handling in opt%elimi
!             11-Dec-2003 MM: New earth orientation parameter handling
!                             except_for_boundaries instead of _of_
!             05-Jan-2004 RD: Allow 'f' and 'l' too
!             26-Feb-2004 RD: Preeliminate all parameters outside a time window
!             22-Sep-2005 RD: Use new modules D_NEQ.f90 and D_PAR.f90
!             25-Jan-2008 RD: add RAO/RAP parameters
!             21-Jul-2008 PS: Format corrected
!             10-Jun-2009 RD: Add epoch satellite/receiver clocks
!             21-Jul-2009 DT: Add keywords UT_ELIM, LOD_ELIM
!             27-Nov-2009 RD: Station exceptions for pre-elimination
!             14-Dec-2009 RD: Correct transfer bug for station exception
!             04-Jan-2010 SL: pre-elimination of HOI scaling factors added
!             22-Jul-2010 RD: seperate handling of SAO X/Y components
!             30-Nov-2010 MM: GNSS-specific parameters
!             01-Dec-2010 DT: Add Helmert parameters
!             27-Apr-2012 RD: Nullify all pointers, use m_bern with only
!             13-Jun-2012 MM: Keyword PRE_ELI removed
!             26-Jun-2012 RD: Check availability of parameters
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, keyValueLength, staNameLength
  USE d_par,    ONLY: maxLcq
  USE d_neq,    ONLY: maxStaSin
  USE p_addneq, ONLY: t_opt, t_elimi, t_namLst
  USE s_alcerr
  USE s_splarg
  USE s_exitrc
  USE s_readkeys
  USE s_ckoptc
  USE s_ckopti
  USE s_ckoptb
  USE s_gtStaNum
  USE s_gttimwin
  IMPLICIT NONE

! List of parameters
! ------------------
! input:

! output:
  TYPE(t_opt)                  :: opt            ! Options for ADDNEQ2
  TYPE(t_namLst), DIMENSION(:) :: namList        ! List of parameter names per
                                                 !   parameter type

! List of functions
! -----------------

! Local types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=8),PARAMETER                            :: srName = 'aoptpre'

  INTEGER(i4b),    PARAMETER                            :: maxEliTyp = 45

  CHARACTER(LEN=10), DIMENSION(maxEliTyp,7), PARAMETER  :: elimKeys =    &
  reshape ( source =                                                     &
  (/ 'CRD_ELIM  ','CLK_ELIM  ','CLK_ELIM  ','CLK_ELIM  ','CLK_ELIM  ',   &
     'ISB_ELIM  ',             'ORB_ELIM  ','RAO_ELIM  ','TRP_ELIM  ',   &
     'GRD_ELIM  ','GRD_ELIM  ','DCBS_ELIM ','DCBR_ELIM ','ERP_ELIM  ',   &
     'ERP_ELIM  ','LOD_ELIM  ','UT_ELIM   ','NUT_ELIM  ','NUT_ELIM  ',   &
     'STO_ELIM  ','ANTX_ELIM ','ANTX_ELIM ','ANT_ELIM  ','MSS_ELIM  ',   &
     'RAP_ELIM  ','GIM_ELIM  ','ALD_ELIM  ','OLD_ELIM  ','HLD_ELIM  ',   &
     'CLKR_ELIM ','CLKS_ELIM ','SPV_ELIM  ','RNG_ELIM  ','HOI_ELIM  ',   &
     'GTRA_ELIM ','GTRA_ELIM ','GTRA_ELIM ','GTRP_ELIM ',                &
     'TRANS_ELIM','TRANS_ELIM','TRANS_ELIM','ROT_ELIM  ','ROT_ELIM  ',   &
     'ROT_ELIM  ','SCALE_ELIM',                                          &

! Field for file exceptions
     'CRD_EXCP  ','CLK_EXCP  ','CLK_EXCP  ','CLK_EXCP  ','CLK_EXCP  ',   &
     'ISB_EXCP  ',             'ORB_EXCP  ','RAO_EXCP  ','TRP_EXCP  ',   &
     'GRD_EXCP  ','GRD_EXCP  ','DCBS_EXCP ','DCBR_EXCP ','ERP_EXCP  ',   &
     'ERP_EXCP  ','LOD_EXCP  ','UT_EXCP   ','NUT_EXCP  ','NUT_EXCP  ',   &
     'STO_EXCP  ','ANTX_EXCP ','ANTX_EXCP ','ANT_EXCP  ','MSS_EXCP  ',   &
     'RAP_EXCP  ','GIM_EXCP  ','ALD_EXCP  ','OLD_EXCP  ','HLD_EXCP  ',   &
     'CLKR_EXCP ','CLKS_EXCP ','SPV_EXCP  ','RNG_EXCP  ','HOI_EXCP  ',   &
     'GTRA_EXCP ','GTRA_EXCP ','GTRA_EXCP ','GTRP_EXCP ',                &
     'TRANS_EXCP','TRANS_EXCP','TRANS_EXCP','ROT_EXCP  ','ROT_EXCP  ',   &
     'ROT_EXCP  ','SCALE_EXCP',                                          &

! Enable station exceptions
     'CRD_EXST  ','CLK_EXST  ','CLK_EXST  ','CLK_EXST  ','CLK_EXST  ',   &
     'ISB_EXST  ',             '          ','          ','TRP_EXST  ',   &
     'GRD_EXST  ','GRD_EXST  ','          ','DCBR_EXST ','          ',   &
     '          ','          ','          ','          ','          ',   &
     '          ','          ','          ','          ','          ',   &
     '          ','          ','ALD_EXST  ','OLD_EXST  ','HLD_EXST  ',   &
     '          ','          ','          ','          ','          ',   &
     'GTRA_EXST ','GTRA_EXST ','GTRA_EXST ','GTRP_EXST ',                &
     '          ','          ','          ','          ','          ',   &
     '          ','          ',                                          &

! Station exceptions: selection
     'CRD_EXSEL ','CLK_EXSEL ','CLK_EXSEL ','CLK_EXSEL ','CLK_EXSEL ',   &
     'ISB_EXSEL ',             '          ','          ','TRP_EXSEL ',   &
     'GRD_EXSEL ','GRD_EXSEL ','          ','DCBR_EXSEL','          ',   &
     '          ','          ','          ','          ','          ',   &
     '          ','          ','          ','          ','          ',   &
     '          ','          ','ALD_EXSEL ','OLD_EXSEL ','HLD_EXSEL ',   &
     '          ','          ','          ','          ','          ',   &
     'GTRA_EXSEL','GTRA_EXSEL','GTRA_EXSEL','GTRP_EXSEL',                &
     '          ','          ','          ','          ','          ',   &
     '          ','          ',                                          &

! Station exceptions: manual
     'CRD_EXMAN ','CLK_EXMAN ','CLK_EXMAN ','CLK_EXMAN ','CLK_EXMAN ',   &
     'ISB_EXMAN ',             '          ','          ','TRP_EXMAN ',   &
     'GRD_EXMAN ','GRD_EXMAN ','          ','DCBR_EXMAN','          ',   &
     '          ','          ','          ','          ','          ',   &
     '          ','          ','          ','          ','          ',   &
     '          ','          ','ALD_EXMAN ','OLD_EXMAN ','HLD_EXMAN ',   &
     '          ','          ','          ','          ','          ',   &
     'GTRA_EXMAN','GTRA_EXMAN','GTRA_EXMAN','GTRP_EXMAN',                &
     '          ','          ','          ','          ','          ',   &
     '          ','          ',                                          &

! Station exceptions: files
     'CRD_EXFIL ','CLK_EXFIL ','CLK_EXFIL ','CLK_EXFIL ','CLK_EXFIL ',   &
     'ISB_EXFIL ',             '          ','          ','TRP_EXFIL ',   &
     'GRD_EXFIL ','GRD_EXFIL ','          ','DCBR_EXFIL','          ',   &
     '          ','          ','          ','          ','          ',   &
     '          ','          ','          ','          ','          ',   &
     '          ','          ','ALD_EXFIL ','OLD_EXFIL ','HLD_EXFIL ',   &
     '          ','          ','          ','          ','          ',   &
     'GTRA_EXFIL','GTRA_EXFIL','GTRA_EXFIL','GTRP_EXFIL',                &
     '          ','          ','          ','          ','          ',   &
     '          ','          ',                                          &

! Station exceptions: flags
     'CRD_EXFLG ','CLK_EXFLG ','CLK_EXFLG ','CLK_EXFLG ','CLK_EXFLG ',   &
     'ISB_EXFLG ',             '          ','          ','TRP_EXFLG ',   &
     'GRD_EXFLG ','GRD_EXFLG ','          ','DCBR_EXFLG','          ',   &
     '          ','          ','          ','          ','          ',   &
     '          ','          ','          ','          ','          ',   &
     '          ','          ','ALD_EXFLG ','OLD_EXFLG ','HLD_EXFLG ',   &
     '          ','          ','          ','          ','          ',   &
     'GTRA_EXFLG','GTRA_EXFLG','GTRA_EXFLG','GTRP_EXFLG',                &
     '          ','          ','          ','          ','          ',   &
     '          ','          ' /), &
  shape = (/maxEliTyp,7/) )

  INTEGER(i4b), DIMENSION(maxEliTyp,maxLcq),PARAMETER   :: locq  =        &
  reshape ( source =                                                      &
  (/ 1, 2, 2, 2, 2, 2, 3, 5, 6, 6, 6, 8, 8,10,10,10,10,10,10,11,12,12,12, &
    16,18,19,22,22,22,23,24,25,26,27,30,30,30,30,28,28,28,28,28,28,28,    &
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
     0, 0, 0, 1, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7,    &
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, &
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 0, 0, 0, 0, 0, 0, 0,    &
     0, 0, 0, 0, 0, 0, 0, 0, 3, 1, 2, 0, 0, 1, 2, 3, 3, 4, 5, 0, 0, 0, 0, &
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    &
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 1, 0, 0, 0, 0, 0, 0, &
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    &
     0, 1, 2, 3, 4, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    &
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/),&
  shape = (/maxEliTyp,maxLcq/) )

! Local variables
! ---------------
  TYPE(t_elimi), DIMENSION(:), ALLOCATABLE              :: elimLoc

  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER  :: keyValue
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER  :: argv
  CHARACTER(LEN=staNameLength),  DIMENSION(:), POINTER  :: staNam

  REAL(r8b),     DIMENSION(:,:), POINTER                :: dummy

  INTEGER(i4b)                                          :: mode
  INTEGER(i4b)                                          :: nEli, iElim
  INTEGER(i4b)                                          :: iSta, jSta
  INTEGER(i4b)                                          :: nSta
  INTEGER(i4b),  DIMENSION(:),   POINTER                :: staNum
  INTEGER(i4b)                                          :: ii
  INTEGER(i4b)                                          :: irCode
  INTEGER(i4b)                                          :: irc, iac


  NULLIFY(keyValue)
  NULLIFY(staNum)
  NULLIFY(staNam)
  NULLIFY(dummy)
  NULLIFY(argv)

  irCode = 0


! Count the number of requests
! ----------------------------
  nEli = maxEliTyp
  DO iElim = 1,maxEliTyp
    CALL readKeys(elimKeys(iElim,1),keyvalue,irc)
    IF (irc /= 0 .OR. keyValue(1) == 'NO' .OR. LEN_TRIM(keyValue(1)) == 0) CYCLE
    IF (LEN_TRIM(elimKeys(iElim,3)) == 0) THEN
      nEli = nEli+1
    ELSE
      CALL readKeys(elimKeys(iElim,3),keyvalue,irc)
      IF (irc == 0 .AND. keyValue(1) == '1') THEN
        nEli = nEli+maxStaSin
      ELSE
        nEli = nEli+1
      ENDIF
    ENDIF
  ENDDO

! Allocate the memory
! -------------------
  ALLOCATE(elimLoc(nEli), stat=iac)
  CALL alcerr(iac, 'elimLoc', (/nEli/), srName)

  nEli = 0

! Loop over all parameters
! ------------------------
  DO iElim = 1, maxEliTyp

    CALL readKeys(elimKeys(iElim,1),keyvalue,irc)

    CALL ckoptc(1,elimKeys(iElim,1),keyvalue,                         &
                (/ '                     ','NO                   ',   &
                   'BEFORE_STACKING      ','AFTER_STACKING       ',   &
                   'EXCEPT_FOR_BOUNDARIES','OUTSIDE_FROM_WINDOW  ',   &
                   'DELETE               ' /),                        &
                srName,'Preelimination action',irc, irCode,           &
                maxVal=1,valList=(/999,0,1,2,3,6,-1/),error=0,result1=mode)

    ! Parameter statistics
    nEli = nEli+1

    elimLoc(nEli)%locq(1:maxLcq)  = locq(iElim,1:maxLcq)
    elimLoc(nEli)%name            = ''
    elimLoc(nEli)%part            = 999
    elimLoc(nEli)%mode            = mode
    elimLoc(nEli)%deltaT          = 60.0D0
    elimLoc(nEli)%eliwin%t        = (/ 0d0, 1d20 /)
    elimLoc(nEli)%wParam          = .FALSE.
    NULLIFY(elimLoc(nEli)%excp)

    CALL readKeys("MSG_" // elimKeys(iElim,1),keyvalue,irc)
    elimLoc(nEli)%descr = keyValue(1)

    ! Nothing to pre-eliminate
    IF (mode == 0 .OR. mode == 999) CYCLE

    ! Allocate the resulting station lists
    ALLOCATE(staNam(namList(locq(iElim,1))%nSta),stat=iac)
    CALL alcerr(iac,'staNam',(/namList(locq(iElim,1))%nSta/),srName)
    ALLOCATE(staNum(namList(locq(iElim,1))%nSta),stat=iac)
    CALL alcerr(iac,'staNum',(/namList(locq(iElim,1))%nSta/),srName)

    ! Check for station exceptions
    IF (LEN_TRIM(elimKeys(iElim,3)) == 0) THEN
      nSta = 0
    ELSE
      CALL ckoptb(1,(/elimKeys(iElim,3)/),srname,'Station exception',irCode, &
                  result1 = nSta)

      ! Read the standard station selection dialog
      IF (nSta == 1) THEN
        CALL gtStaNum(namList(locq(iElim,1))%nSta,         &
                      namList(locq(iElim,1))%num,          &
                      namList(locq(iElim,1))%nam,          &
                      elimKeys(iElim,4),elimKeys(iElim,5), &
                      elimKeys(iElim,6),elimKeys(iElim,7), &
                      nSta,staNum,staNam,0,dummy)
      ENDIF
    ENDIF

    ! Loop all stations for this parameter
    iStaLoop: DO iSta = 1,namList(locq(iElim,1))%nSta+1

      ! Cycle if the station is the exception list
      DO jSta = 1,nSta
        IF (namList(locq(iElim,1))%nSta == 0) EXIT
        IF (namList(locq(iElim,1))%nam(iSta) == staNam(jSta)) CYCLE iStaLoop
      ENDDO

      nEli = nEli+1

      ! No exception: parameter name is blank
      IF (nSta == 0 .OR. namList(locq(iElim,1))%nSta == 0) THEN
        elimLoc(nEli)%name            = ''

      ! Exception: put the non-exception names into the parameter name
      ELSE
        elimLoc(nEli)%name            = namList(locq(iElim,1))%nam(iSta)
      ENDIF

      ! Define the remaining fields
      elimLoc(nEli)%locq(1:maxLcq)      = locq(iElim,1:maxLcq)
      elimLoc(nEli)%mode                = mode
      elimLoc(nEli)%part                = 1

      IF (mode == 2) elimLoc(nEli)%part = 2
      NULLIFY(elimLoc(nEli)%excp)

      ! Check for exceptions for files
      IF (mode == 1 .OR. mode == 3) THEN
        CALL readKeys(elimKeys(iElim,2),keyvalue,irc)

        ! File exception field is not empty
        IF (LEN_TRIM(keyValue(1)) /= 0) THEN
          CALL splarg(keyValue(1),argv)

          ! First/last short-cuts
          DO ii = 1,SIZE(argv)
            IF (argv(ii) == 'F' .OR. &            ! except first file in list
                argv(ii) == 'f') argv(ii) = '1'
            IF (argv(ii) == 'L' .OR. &            ! except last  file in list
                argv(ii) == 'l') THEN
              argv(ii) = ' '
              WRITE(argv(ii),*) SIZE(opt%neqFileName)
            ENDIF
          ENDDO

          ! Extract the exception file numbers
          ALLOCATE(elimLoc(nEli)%excp(SIZE(argv)), stat=iac)
          CALL alcerr(iac, 'elimLoc(nEli)%excp', (/SIZE(argv)/), srName)

          CALL ckopti(1,elimKeys(iElim,2),argv,srName,                      &
                      'File exceptions for preelimination',irc,irCode,      &
                      maxVal=SIZE(argv),error=0,result2=elimLoc(nEli)%excp)

          ! New mode if file exceptions requested
          IF (mode == 1) elimLoc(nEli)%mode = 4
          IF (mode == 3) elimLoc(nEli)%mode = 5
          DEALLOCATE(argv)

        ENDIF ! end of file exceptions

!!        ! Read limit for "except of boundary" preelimination
!!        IF (mode == 4) THEN
!!          CALL ckoptr(1,elimKeys(iElim,2),keyValue,srName,                  &
!!                      'Limits for "except of boundary" preelim.',irc,irCode,&
!!                      empty = 60d0, error=60d0, result1=elimLoc(nEli)%deltaT)
!!        END IF

        elimLoc(nEli)%deltaT = 60.0D0
      ENDIF

      ! Read the time window for "OUTSIDE_TIME_WINDOW"
      IF (elimLoc(nEli)%mode == 6) THEN
        CALL gttimwin(' ',(/'RADIO_1','RADIO_2'/),               &
                      (/'SESSION_YEAR','SESSION_STRG'/),         &
                      (/'STADAT','STATIM','ENDDAT','ENDTIM'/),   &
                      elimLoc(nEli)%eliwin%t)
      ENDIF

      IF (nSta == 0) EXIT
      IF (iSta == namList(locq(iElim,1))%nSta) EXIT
    ENDDO iStaLoop ! Next station in loop

    ! Deallocate the lists of selected stations
    DEALLOCATE(staNam)
    DEALLOCATE(staNum)
  ENDDO ! Loop all preelimination lines

! Stop in case of an error
! ------------------------
  IF (irCode /= 0) CALL exitrc(2)

! Copy the final list into the option record
! ------------------------------------------
  ALLOCATE(opt%elimi(nEli), stat=iac)
  CALL alcerr(iac, 'opt%elimi', (/nEli/), srName)

  DO iElim = 1,nEli
    opt%elimi(iElim) = elimLoc(iElim)
    IF (elimLoc(iElim)%mode == 4 .OR. elimLoc(iElim)%mode == 5) THEN
      ALLOCATE(opt%elimi(iElim)%excp(SIZE(elimLoc(iElim)%excp)), stat=iac)
      CALL alcerr(iac, 'opt%elimi(iElim)%excp', (/SIZE(elimLoc(iElim)%excp)/), srName)
      DO ii = 1,SIZE(elimLoc(iElim)%excp)
        opt%elimi(iElim)%excp(ii) = elimLoc(iElim)%excp(ii)
      ENDDO
      DEALLOCATE(elimLoc(iElim)%excp,stat=iac)
    ENDIF

  ENDDO

  DEALLOCATE(keyValue,stat=iac)
  DEALLOCATE(elimLoc,stat=iac)

END SUBROUTINE aoptpre


END MODULE
