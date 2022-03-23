MODULE s_AOPTCLK
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.0
! -------------------------------------------------------------------------

  SUBROUTINE aoptclk(opt, clkList, clkhed)

! -------------------------------------------------------------------------
! Purpose:    Read input options for epochwise clock estimation
!
! Author:     R. Dach
!
! Created:    10-Jun-2009
!
! Changes:    29-Nov-2010 MF: Nullify pointer, add call to init_ref
!             19-Jan-2011 RD: USe GETSTA
!             28-Mar-2012 RD: Use SVN2CHR as module now
!             19-Sep-2012 RD: Correctly deallocate arrays
!             16-Jul-2013 RD: Clock RINEX records only if output file is given
!
! Copyright:  Astronomical Institute
!             University of Berne
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnprt, program_name, &
                      keyValueLength, staNameLength, fileNameLength
  USE d_const,  ONLY: C,date,time
  USE d_clkrnx, ONLY: t_clkHead, init_ref
  USE d_datum,  ONLY: datum
  USE p_addneq, ONLY: t_opt,t_sigma,t_elimi

  USE s_alcerr
  USE s_chr2svn
  USE s_ckoptb
  USE s_ckoptc
  USE s_ckoptl
  USE s_ckoptr
  USE s_exitrc
  USE s_getsta
  USE s_gtstanum
  USE s_prisig
  USE s_readKeys
  USE s_svn2chr
  USE s_gtflna
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*), DIMENSION(:,:), POINTER :: clkList ! List of clocks

! output:
  TYPE(t_opt)                             :: opt    ! ADDNEQ input options
  TYPE(t_clkhead)                         :: clkHed


! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER            :: srName = 'aoptclk'

! Local Variables
! ---------------
  TYPE(t_sigma), DIMENSION(:),POINTER    :: hlpSig
  TYPE(t_elimi), DIMENSION(:),POINTER    :: hlpEli

  CHARACTER(LEN=keyValueLength),          &
                DIMENSION(:), POINTER    :: keyValue
  CHARACTER(LEN=fileNameLength)          :: clkFil
  CHARACTER(LEN=staNameLength),           &
                DIMENSION(:),ALLOCATABLE :: clkSta
  CHARACTER(LEN=staNameLength),           &
                DIMENSION(:), POINTER    :: refNam
  CHARACTER(LEN=staNameLength),           &
                DIMENSION(:),ALLOCATABLE :: clkSat
  CHARACTER(LEN=staNameLength),           &
                DIMENSION(:), POINTER    :: refSat
  CHARACTER(LEN=staNameLength)           :: hlpStr
  CHARACTER(LEN=1)                       :: satchr

  INTEGER(i4b)                           :: nClk1,nClk2
  INTEGER(i4b)                           :: iClk
  INTEGER(i4b)                           :: iRef
  INTEGER(i4b), DIMENSION(:),ALLOCATABLE :: hlpNum
  INTEGER(i4b), DIMENSION(:),ALLOCATABLE :: refNum
  INTEGER(i4b), DIMENSION(:),ALLOCATABLE :: svnNum
  INTEGER(i4b), DIMENSION(:),ALLOCATABLE :: refSvn
  INTEGER(i4b)                           :: nRefSta
  INTEGER(i4b)                           :: nRefSat
  INTEGER(i4b)                           :: satNum
  INTEGER(i4b)                           :: rckmod
  INTEGER(i4b)                           :: rckfix
  INTEGER(i4b)                           :: nElimi
  INTEGER(i4b)                           :: nSigma
  INTEGER(i4b)                           :: nCentr
  INTEGER(i4b), DIMENSION(:),ALLOCATABLE :: iCentr
  INTEGER(i4b)                           :: irCode
  INTEGER(i4b)                           :: irc,iac,ios

  REAL(r8b)                              :: clksig
  REAL(r8b),  DIMENSION(:,:),ALLOCATABLE :: xStat
  REAL(r8b),  DIMENSION(:,:),ALLOCATABLE :: xStell
  REAL(r8b),  DIMENSION(:,:),ALLOCATABLE :: xStecc
  REAL(r8b),  DIMENSION(:,:),POINTER     :: dummy

! Init error counter
! ------------------
  irCode = 0

  NULLIFY(keyValue)
  NULLIFY(dummy)
  NULLIFY(hlpSig)
  NULLIFY(hlpEli)
  NULLIFY(refNam)
  NULLIFY(refSat)

! Generate the list of all stations
! ---------------------------------
  nClk1 = 0
  DO iClk = 1,SIZE(clkList,1)
    IF (LEN_TRIM(clkList(iClk,1)) > 0) nClk1 = nClk1 + 1
  ENDDO

  ALLOCATE(clkSta(nClk1),stat=iac)
  CALL alcerr(iac,'clkSta',(/nClk1/),srName)

  nClk1 = 0
  DO iClk = 1,SIZE(clkList,1)
    IF (LEN_TRIM(clkList(iClk,1)) > 0) THEN
      nClk1 = nClk1 + 1
      clkSta(nClk1) = clkList(iClk,1)
    ENDIF
  ENDDO


  ALLOCATE(hlpNum(nClk1),stat=iac)
  CALL alcerr(iac,'hlpNum',(/nClk1/),srName)

  hlpNum = (/ (iClk,iClk=1,nClk1) /)

! Generate the list of all satellites
! -----------------------------------
  nClk2 = 0
  DO iClk = 1,SIZE(clkList,1)
    IF (LEN_TRIM(clkList(iClk,2)) > 0) nClk2 = nClk2 + 1
  ENDDO

  ALLOCATE(clkSat(2*nClk2),stat=iac)
  CALL alcerr(iac,'clkSat',(/2*nClk2/),srName)

  ALLOCATE(svnNum(2*nClk2),stat=iac)
  CALL alcerr(iac,'svnNum',(/2*nClk2/),srName)

  nClk2 = 0
  DO iClk = 1,SIZE(clkList,1)
    IF (LEN_TRIM(clkList(iClk,2)) > 0) THEN
      nClk2 = nClk2 + 1
      clkSat(nClk2) = clkList(iClk,2)

      READ(clkList(iClk,2),'(A1,I2.2)',iostat=ios) satChr,satNum
      IF  (ios /= 0) THEN
        nClk2 = nClk2 - 1
        CYCLE
      ENDIF
      CALL chr2svn(satNum,satChr,svnNum(nClk2))

      nClk2 = nClk2 + 1
      svnNum(nClk2) = svnNum(nClk2-1)
      clkSat(nClk2)=' '
      WRITE(clkSat(nClk2),*) svnNum(nClk2)
      clkSat(nClk2) = ADJUSTL(clkSat(nClk2))
    ENDIF
  ENDDO

  nClk2=nClk2/2

! No clocks, nothing to do
! ------------------------
  IF (nCLk1 == 0 .AND. nClk2 == 0) RETURN

! Get the reference clock selection
! ---------------------------------
  CALL readKeys('REFCLOCK',keyValue,irc)

  CALL ckoptc(1,'REFCLOCK',keyValue,                                 &
              (/'FIRST_STATION ','LAST_STATION  ','ALL_STATIONS  ',  &
                'ALL_SATELLITES','MANUAL        ','FROM_FILE     '/),&
              srName,'Select reference clock',irc,irCode,            &
              maxVal=1,valList=(/1,1,1,2,3,3/),result1=rckMod)

! Get all reference clocks
! ------------------------

  ! Station clocks
  nRefSta = 0
  IF (rckMod == 1 .OR. rckMod == 3) THEN

    ALLOCATE(refNam(nClk1),stat=irc)
    CALL alcerr(irc,'refNam',(/nClk1/),srName)

    ALLOCATE(refNum(nClk1),stat=irc)
    CALL alcerr(irc,'refNum',(/nClk1/),srName)

    ! Get the list of station reference clocks
    CALL gtStaNum(nClk1, hlpNum, clkSta,                   &
                  'REFCLOCK','CLKSTASEL','CLKSTAFIL', ' ', &
                  nRefSta, refNum, refNam, 0, dummy)
  ENDIF

  ! Satellite clocks
  nRefSat = 0
  IF (rckMod == 3) THEN

    ALLOCATE(refSat(nClk2),stat=irc)
    CALL alcerr(irc,'refSat',(/nClk2/),srName)

    ALLOCATE(refsvn(nCLk2),stat=irc)
    CALL alcerr(irc,'refsvn',(/nClk2/),srName)

    ! Get the list of satellite reference clocks
    CALL gtStaNum(2*nClk2, svnNum, clkSat,                 &
                 'REFCLOCK','CLKSATSEL','CLKSATFIL', ' ',  &
                  nRefSat, refSvn, refSat, 0, dummy)

  ELSE IF (rckMod == 2) THEN

    ALLOCATE(refSat(nClk2),stat=irc)
    CALL alcerr(irc,'refSat',(/nClk2/),srName)

    ALLOCATE(refsvn(nClk2),stat=irc)
    CALL alcerr(irc,'refsvn',(/nClk2/),srName)

    nRefSat = nClk2
    DO iClk = 1,nClk2
      refSat(iClk) = clkSat(iClk*2-1)
      refSvn(iClk) = svnNum(iClk*2-1)
    ENDDO

  ENDIF


! Fixed or constrained reference clock
! ------------------------------------
  IF (nRefSta > 0 .OR. nRefSat > 0) THEN

    CALL ckoptb(1,(/'CLKFIX'/),srName,                     &
               'Fix reference clocks',irCode,result1=rckFix)

    IF (rckFix == 1) THEN
      IF (ASSOCIATED(opt%elimi)) THEN

        nElimi = SIZE(opt%elimi)

        ALLOCATE(hlpEli(nElimi),stat=iac)
        CALL alcerr(iac,'hlpEli',(/nElimi/),srName)

        hlpEli = opt%elimi

        DEALLOCATE(opt%elimi,stat=iac)
        ALLOCATE(opt%elimi(nElimi+nRefSta+nRefSat),stat=iac)
        CALL alcerr(iac,'opt%elimi',(/nElimi+nRefSta+nRefSat/),srName)

        opt%elimi(1:nElimi) = hlpEli

        DEALLOCATE(hlpEli,stat=iac)

      ELSE

        nElimi = 0
        ALLOCATE(opt%elimi(nRefSta+nRefSat),stat=iac)
        CALL alcerr(iac,'opt%elimi',(/nRefSta+nRefSat/),srName)

      ENDIF

      DO iClk = 1,nRefSta
        nElimi = nElimi+1
        opt%elimi(nElimi)%name = refNam(iClk)
        opt%elimi(nElimi)%locq = (/ 23, 0, 0, 0, 0, 0, 0 /)
        opt%elimi(nElimi)%mode = -1
        opt%elimi(nElimi)%part = 1
      ENDDO


      DO iClk = 1,nRefSat
        nElimi = nElimi+1
        opt%elimi(nElimi)%name = ' '
        CALL svn2chr(refSvn(iClk),satnum,satchr)
        WRITE(opt%elimi(nElimi)%name,'(A1,I2.2)') satchr,satnum
        opt%elimi(nElimi)%locq = (/ 24, 0, 0, 0, 0, 0, 0 /)
        opt%elimi(nElimi)%mode = -1
        opt%elimi(nElimi)%part = 1
      ENDDO

      clkSig = 0d0

    ELSE

      CALL readKeys('CLKSIG',keyValue,irc)

      CALL ckoptr(1,'CLKSIG',keyValue,srName, &
                  'A priori sigma for reference clock',irc,irCode, &
                  maxVal = 1,ge=0d0,empty = 0d0, result1 = clksig)

      IF (clkSig /= 0d0) THEN

        IF (ASSOCIATED(opt%sigma)) THEN
          nSigma = SIZE(opt%sigma)

          ALLOCATE(hlpSig(nSigma),stat=iac)
          CALL alcerr(iac,'hlpSig',(/nSigma/),srName)

          hlpSig = opt%sigma

          DEALLOCATE(opt%sigma,stat=iac)
          ALLOCATE(opt%sigma(nSigma+nRefSta+nRefSat),stat=iac)
          CALL alcerr(iac,'opt%sigma',(/nSigma+nRefSta+nRefSat/),srName)

          opt%sigma(1:nSigma) = hlpSig

          DEALLOCATE(hlpSig,stat=iac)
        ELSE

          nSigma = 0
          ALLOCATE(opt%sigma(nRefSta+nRefSat),stat=iac)
          CALL alcerr(iac,'opt%sigma',(/nRefSta+nRefSat/),srName)

        ENDIF

        DO iClk = 1,nRefSta
          nSigma = nSigma+1
          opt%sigma(nSigma)%name   = refNam(iClk)
          opt%sigma(nSigma)%locq   = (/ 23, 0, 0, 0, 0, 0, 0 /)
          opt%sigma(nSigma)%value  = clksig*C/1d9
          opt%sigma(nSigma)%typFlg = 'A'
        ENDDO


        DO iClk = 1,nRefSat
          nSigma = nSigma+1
          hlpstr = ' '
          CALL svn2chr(refSvn(iClk),satnum,satchr)
          WRITE(hlpstr,'(A1,I2.2)') satchr,satnum
          opt%sigma(nSigma)%name = hlpstr
          opt%sigma(nSigma)%locq   = (/ 24, 0, 0, 0, 0, 0, 0 /)
          opt%sigma(nSigma)%value  = clksig*C/1d9
          opt%sigma(nSigma)%typFlg = 'A'
        ENDDO

      ENDIF

    ENDIF
  ENDIF

! Put clock information into a clock RINEX header (for output)
! ------------------------------------------------------------
  ! Data types
  IF (nClk1 > 0 .AND. nClk2 > 0) THEN
    clkHed%numTyp = 2
    ALLOCATE(clkHed%datTyp(2),stat=irc)
    CALL alcerr(irc,'clkHed%datTyp',(/2/),srName)
    clkHed%datTyp = (/ 'AR', 'AS' /)
  ELSE IF (nClk1 > 0) THEN
    clkHed%numTyp = 1
    ALLOCATE(clkHed%datTyp(1),stat=irc)
    CALL alcerr(irc,'clkHed%datTyp',(/1/),srName)
    clkHed%datTyp = (/ 'AR' /)
  ELSE IF (nClk2 > 0) THEN
    clkHed%numTyp = 1
    ALLOCATE(clkHed%datTyp(1),stat=irc)
    CALL alcerr(irc,'clkHed%datTyp',(/1/),srName)
    clkHed%datTyp = (/ 'AS' /)
  ELSE
    clkHed%numTyp = 0
  ENDIF

  ! Number of clocks, clock names
  clkHed%nSta = nClk1
  clkHed%nSat = nClk2

  ALLOCATE(clkHed%clkName(nCLk1+nClk2),stat=irc)
  CALL alcerr(irc,'clkHed%clkName',(/nCLk1+nClk2/),srName)

  IF (nClk1 > 0) clkHed%clkName(1:nClk1) = clkSta(1:nCLk1)
  DO iClk = 1,nClk2
    clkHed%clkName(nClk1+iClk) = clkSat(2*iClk-1)
  ENDDO

  ! Station coordinates
  IF (nClk1 > 0) THEN
    ALLOCATE(xStat (3,nClk1), stat=irc)
    CALL alcerr(irc,'xStat' ,(/3,nClk1/), srName)

    ALLOCATE(xStell(3,nClk1), stat=irc)
    CALL alcerr(irc,'xStell',(/3,nClk1/), srName)

    ALLOCATE(xStecc(3,nClk1), stat=irc)
    CALL alcerr(irc,'xStecc',(/3,nClk1/), srName)

    ALLOCATE(iCentr(nClk1),   stat=irc)
    CALL alcerr(irc,'iCentr',(/nClk1/),   srName)

    CALL getSta(nClk1, clkSta, hlpNum, nCentr, iCentr,           &
              xStat, xStell, xStecc,  datum%name, datum%aEll, datum%bEll, &
              datum%dxEll, datum%drEll, datum%scEll)

    ALLOCATE(clkHed%StaCoord(3,nClk1),stat=irc)
    CALL alcerr(irc,'clkHed%StaCoord',(/3,nClk1/),srName)

    clkHed%StaCoord = xStat

    DEALLOCATE(xStat,stat=irc)
    DEALLOCATE(xStell,stat=irc)
    DEALLOCATE(xStecc,stat=irc)
    DEALLOCATE(iCentr,stat=irc)
  ENDIF

  ! Reference clocks
  IF (nClk1 > 0 .OR. nClk2 > 0) THEN
    clkHed%numRef = 1
    ALLOCATE(clkHed%ref(1),stat=irc)
    CALL alcerr(irc,'clkHed%ref',(/1/),srName)
    CALL init_ref(clkHed%ref(1))
    clkHed%ref(1)%nRef = nRefSta+nRefSat
    clkHed%ref(1)%refWin%t = 0d0
    IF (nRefSta+nRefSat > 0) THEN
      ALLOCATE(clkHed%ref(1)%clk(clkHed%ref(1)%nRef),stat=irc)
      CALL alcerr(irc,'clkHed%ref(1)%clk',(/clkHed%ref(1)%nRef/),srName)
      iRef = 0
      DO iClk = 1,nRefSta
        iRef = iRef+1
        clkHed%ref(1)%clk(iRef)%name = refNam(iClk)
      ENDDO
      DO iClk = 1,nRefSat
        iRef = iRef+1
        clkHed%ref(1)%clk(iRef)%name = refSat(iClk)
      ENDDO
      clkHed%ref(1)%clk(:)%sigma = clkSig
    ENDIF
  ENDIF

  ! Other values
  clkhed%LeapSec = 0.0D0
  clkhed%CrDate  = date//' '//time
  clkhed%TRFName = datum%name
  clkhed%prognam = program_name

  CALL gtflna(0,'CLKRNX',clkFil,irc)
  if ( irc == 0 .AND. LEN_TRIM(clkFil) > 0 ) THEN

    ! Some more information from panel
    CALL readKeys('RUNBY',keyValue,irc)
    IF (irc == 0) clkhed%RunBy = keyValue(1)(1:20)

    CALL readKeys('AC',keyValue,irc)
    IF (irc == 0) clkhed%AC = keyValue(1)(1:3)

    CALL readKeys('ACNAME',keyValue,irc)
    IF (irc == 0) clkhed%ACName = keyValue(1)(1:55)

    CALL readkeys('TIMESYS', keyValue, irc)
    CALL ckoptl(1,'TIMESYS', keyValue, srName,            &
                'Clock RINEX: Time system', irc, irCode,  &
                maxLength=LEN(ClkHed%timsys), maxval=1,   &
                result1=ClkHed%timsys)

    CALL readkeys('DCBLINE', keyValue, irc)
    CALL ckoptl(1,'DCBLINE', keyValue, srName,            &
                'Clock RINEX: DCB line', irc, irCode,     &
                maxLength=LEN(ClkHed%dcbStr), empty=' ',  &
                maxval=1, result1=ClkHed%dcbStr)

    clkhed%nComment = 0
    CALL readKeys('COMMENT',keyValue,irc)

    clkhed%nComment = SIZE(keyValue)
    ALLOCATE(clkhed%Comment(clkhed%nComment),stat=ios)
    CALL alcerr(ios, 'clkhed%Comment', (/clkhed%nComment/), srName)

    CALL ckoptl(0,'COMMENT',keyValue,srName,        &
                'Clock RINEX comment',irc,irCode,   &
                empty=' ',maxVal=clkhed%nComment,result2=clkhed%Comment)
  ENDIF

! Deallocate local arrays
! -----------------------
  DEALLOCATE(keyValue,stat=irc)
  DEALLOCATE(hlpNum,stat=irc)
  DEALLOCATE(clkSta,stat=irc)
  DEALLOCATE(clkSat,stat=irc)
  DEALLOCATE(svnNum,stat=irc)

  IF (rckMod == 1 .OR. rckMod == 3) THEN
    DEALLOCATE(refNam,stat=irc)
    DEALLOCATE(refNum,stat=irc)
  ENDIF
  IF (rckMod == 2 .OR. rckMod == 3) THEN
    DEALLOCATE(refSat,stat=irc)
    DEALLOCATE(refSvn,stat=irc)
  ENDIF

! Delete only clocks with values from input file
! ----------------------------------------------
  IF (ASSOCIATED(opt%elimi)) THEN
    DO nElimi = 1,SIZE(opt%elimi)
      IF (LEN_TRIM(opt%elimi(nElimi)%name) > 0) CYCLE
      IF (opt%elimi(nElimi)%mode /= -1) CYCLE
      IF (opt%elimi(nElimi)%locq(1) == 23 .OR. &
          opt%elimi(nElimi)%locq(1) == 24) THEN
        CALL ckoptb(1,(/'CLKDELINP'/),srName,                         &
                   'Delete only clocks with apriori values',irCode,   &
                   result1=opt%elimi(nElimi)%locq(7))
      ENDIF
    ENDDO
  ENDIF

! Exit if entries are not correct
! -------------------------------
  IF (irCode /= 0) CALL exitrc(2)

! Write the input options into the program output
! -----------------------------------------------
  IF (nClk1 > 0 .OR. nClk2 > 0) THEN
    WRITE(lfnprt,"( //,' '                              &
                  & ,/,' EPOCH-WISE CLOCK ESTIMATION:'  &
                  & ,/,' ---------------------------'   &
                  & ,/,' ')")

    ! Report reference clocks (stations only)
    IF (nRefSat == 0 .AND. nRefSta > 0) THEN
      WRITE(lfnprt,"(A,2X,5(A,7X),/,40(' ',19X,5(A,7X),/))")    &
                ' Reference clocks:',                           &
                clkHed%ref(1)%clk(1:nRefSta)%name(1:16)
      IF(MOD(nRefSta,5) /= 0) WRITE(lfnprt,'( )')

    ! Report reference clocks (satellites only)
    ELSE IF (nRefSat > 0 .AND. nRefSta == 0) THEN
      WRITE(lfnprt,"(A,2X,16(A,4X),/,40(' ',19X,16(A,4X),/))")  &
            ' Reference clocks:',                               &
            clkHed%ref(1)%clk(1:nRefSat)%name(1:3)
      IF(MOD(nRefSat,16) /= 0) WRITE(lfnprt,'( )')

    ! Report reference clocks (stations and satellites)
    ELSE IF (nRefSat > 0 .AND. nRefSta > 0) THEN
      WRITE(lfnprt,"(A,2X,5(A,7X),/,40(' ',19X,5(A,7X),/))")    &
                ' Reference clocks:',                           &
                clkHed%ref(1)%clk(1:nRefSta)%name(1:16)
      WRITE(lfnprt,"(A,2X,16(A,4X),/,40(' ',19X,16(A,4X),/))")  &
            '                  ',                               &
            clkHed%ref(1)%clk(1:nRefSat)%name(1:3)
      IF(MOD(nRefSat,16) /= 0) WRITE(lfnprt,'( )')
    ENDIF

    ! Reference clocks are fixed or constraints
    IF (rckFix == 1) THEN
      WRITE(lfnprt,'(20X,A,//)') &
            'The reference clocks are fixed on their apriori values'
    ELSE
      IF (nRefSta > 0) THEN
        CALL prisig(23,(/clkSig/),1,(/1/),131)
      ELSE IF (nRefSat > 0) THEN
        CALL prisig(24,(/clkSig/),1,(/1/),131)
      ENDIF
    ENDIF

    ! Number of clocks to be estimated
    IF (rckFix == 1) THEN
      WRITE(lfnprt,'(1X,A,2(I6,A),/)')                        &
        'Number of clocks:',nClk1-nrefSta,' Stations and ', &
        nCLk2-nRefSat,' satellites have to be estimated per epoch'
    ELSE
      WRITE(lfnprt,'(1X,A,2(I6,A),/)')                        &
        'Number of clocks:',nCLk1,' Stations and ',         &
        nClk2,' satellites have to be estimated per epoch'
    ENDIF
  ENDIF

  RETURN
END SUBROUTINE aoptclk

END MODULE
