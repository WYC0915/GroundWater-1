MODULE s_CCLKIN
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE cclkin(CCopt, CCFil, ClkHead, inClkHead, irCode)

! -------------------------------------------------------------------------
! Purpose:    Reads input file for CCRNXC
!
! Author:     R.Dach
!
! Created:    18-Aug-2000
!
! Changes:    10-Oct-2000 RD: New min. number of clocks for mean
!             13-Feb-2001 RD: Flexible selection of the reference clock
!                                error msg. moved into "readKeys"
!             14-Feb-2001 RD: Use ALCERR now
!             18-Feb-2001 RD: Selection of reference clock
!             27-Feb-2001 RD: Inter/extrapolation
!             18-Jun-2001 RD: Select only one ref-clock
!             03-Aug-2001 RD: Modified program outupt
!             27-Aug-2001 RD: Min. number of data points for extrapolation
!             28-Aug-2001 RD: Use SR readstsg for reading clock selection file
!             25-Sep-2001 RD: Use new SRs to extract values from input file
!             02-Oct-2001 RD: Modified handling of srName
!             01-Feb-2002 RD: Use SR gtStaNum to get ref-clk candidates
!             25-Sep-2002 HU: Remove i_astlib
!             08-Oct-2002 RD: New call of SR gtStaNum
!             03-Feb-2003 RD: New call of GTSTANUM (weight is pointer now)
!             23-Apr-2003 CU: Nullify local pointers
!             15-May-2003 AJ: Initialize structure
!             19-May-2003 RD: Use SR gttimwin instead of SR readsess
!             07-Jul-2003 RD: Apply init_clkhead for inClkHead
!             19-Aug-2003 RD: Correct index bug
!             25-Nov-2003 RD: Some POINTER->ALLOCATABLE
!             28-Nov-2003 RD: Stop with error, if no ref clock in input file
!             04-Dec-2003 HB: Call of SR gtStaNum with hlpNam1 and hlpNam2
!             15-Jan-2004 RD/HB: Selection of satellites from file/list works
!             21-Jan-2004 RD: Adapt to new input panel
!             12-Mar-2004 RD: Write output files even if no ref-clock avail.
!             12-Mar-2004 RD: Extract only satellite clocks
!             02-Aug-2004 RD: Correct output RINEX header for empty input fields
!             07-Feb-2005 HB: Adopt for ifc-Compiler, Version 8.1
!             22-Jul-2005 HB: Factor of 2 for sat-clock arrays
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             09-Feb-2007 AG: Check given PCV models
!             12-Aug-2009 RD: Warning if PCV models differ (no error)
!             21-Sep-2009 RD: Remove epochs with eclipse flags
!             20-Jul-2011 RD: Extract RXCBV3 function to pgm RNXCLK
!             14-Nov-2011 SL: m_bern w/ ONLY, no PRITIT call (moved to program)
!             13-Jan-2012 RD: ALL_SATELLITES as candidates for reference clocks
!             28-Mar-2012 RD: Use SVN2CHR as module now
!             22-Apr-2012 RD: Do not use free format to write strings
!             19-Sep-2012 RD: Correctly deallocate arrays
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, &
                      KeyNameLength, keyValueLength, fileNameLength, &
                      timStrgLength, linelength, staNameLength, &
                      lfnPrt, lfnErr, lfnLoc
  USE p_ccrnxc, ONLY: t_ccrnxc_opt, t_ccrnxc_fil, &
                      prtinp, prtcmb, prtref, prtjmp, prtext, prtres
  USE d_clkrnx, ONLY: t_clkhead,init_clkHead
  USE d_stalst, ONLY: t_staList,init_stalist
  USE s_gtfile2
  USE s_ckoptr
  USE s_ckoptt
  USE s_alcerr
  USE s_opnfil
  USE s_ckoptu
  USE s_prflna
  USE s_svn2chr
  USE s_gtstanum
  USE s_opnerr
  USE s_gttimwin
  USE s_prfile
  USE s_timst2
  USE s_readkeys
  USE s_pricrh
  USE s_rdcrxh
  USE s_exitrc
  USE s_ckoptb
  USE s_ckoptc
  USE s_ckoptd
  USE s_gtflna
  USE s_ckopti
  USE s_readstsg
  USE s_ckoptl
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:

! output:
  TYPE(t_ccrnxc_opt)             :: CCopt      ! program input options
  TYPE(t_ccrnxc_fil)             :: CCfil      ! clock rinex file names
  TYPE(t_clkhead)                :: ClkHead    ! New output header
  TYPE(t_clkhead),                &
        DIMENSION(:),   POINTER  :: InClkHead  ! Input clock headers
  INTEGER(i4b)                   :: irCode     ! Check the input options

! Local Parameter
! ---------------

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER    :: srName = 'cclkin'

! Local Variables
! ---------------
  CHARACTER(LEN=KeyNameLength)   :: keyWord
  CHARACTER(LEN=keyValueLength),  &
    DIMENSION(:), POINTER        :: keyValue

  TYPE(t_staList)                :: clkList, clkList1, clkList2

  CHARACTER(LEN=fileNameLength),               &
    DIMENSION(:,:), POINTER      :: hlpFil     ! Help list for file names
  CHARACTER(LEN=fileNameLength)  :: fixfil     ! Name of ref-clock file
  CHARACTER(LEN=keyValueLength),               &
    DIMENSION(:,:), ALLOCATABLE  :: hlpStr     ! String to read an uniline
  CHARACTER(LEN=timStrgLength)   :: epost1, epost2
  CHARACTER(LEN=linelength)      :: Line
  CHARACTER(LEN=staNameLength),                &
    DIMENSION(:),POINTER         :: hlpNam1,hlpNam2
  CHARACTER(LEN=40)              :: pcvmod     ! PCV model
  CHARACTER(LEN=1)               :: satChr     ! 'G' or 'R' for satellite number

  REAL(r8b),      DIMENSION(:),   &
                  ALLOCATABLE    :: kSico      ! period. terms from spec. requ.
  REAL(r8b),      DIMENSION(:,:), &
                  ALLOCATABLE    :: funcWin    ! time win. for function comp.
  REAL(r8b),      DIMENSION(:,:), &
                  POINTER        :: dummy

  INTEGER(i4b),   DIMENSION(:),   &
                  ALLOCATABLE    :: kPoly      ! poly. degr. from spec. requ.
  INTEGER(i4b)                   :: iRefTyp
  INTEGER(i4b),   DIMENSION(2)   :: iSel
  INTEGER(i4b)                   :: iFil,jFil  ! Counts the file in list
  INTEGER(i4b)                   :: nFil       ! Number of files in list
  INTEGER(i4b)                   :: iClk, jClk ! Counter for clocks
  INTEGER(i4b)                   :: iClk0      ! Save counter
  INTEGER(i4b)                   :: iSta, iSat
  INTEGER(i4b)                   :: svnNum,satNum ! Satellite numbers
  INTEGER(i4b)                   :: hlpSat
  INTEGER(i4b)                   :: maxPoly    ! inter/extrapol. model (poly)
  INTEGER(i4b)                   :: maxSico    ! inter/extrapol. model (period)
  INTEGER(i4b)                   :: iErr
  INTEGER(i4b)                   :: ios, iac   ! IO status
  INTEGER(i4b),   DIMENSION(:),   &
                  ALLOCATABLE    :: numAll, numRef
  INTEGER(i4b)                   :: ircSave
  INTEGER(i4b)                   :: ii, jj     ! Counter for nothing
  INTEGER(i4b)                   :: i1, i2     ! Counter for nothing
  INTEGER(i4b)                   :: irc, iNum

! Initialization
! --------------
  irCode = 0

  CALL init_stalist(clkList)
  CALL init_stalist(clkList1)
  CALL init_stalist(clkList2)
  NULLIFY(keyValue)
  NULLIFY(hlpFil)
  NULLIFY(dummy)
  NULLIFY(hlpNam1)
  NULLIFY(hlpNam2)

! Title line
! ----------
  CALL readKeys('TITLE',keyValue,irc)

  CALL ckoptl(0,'TITLE',keyValue,srName,                                    &
              'Printing options: title line',irc,irCode,                    &
              maxVal=1,empty=' ',result1=CCopt%title)

! Start writing the protocol
! --------------------------
  CALL prflna(130)
  CALL prfile('RCLKINP', ' ', 1, 130)

! OUTPUT FILE NAMES
! =================
!
! Reads output file names
! -----------------------
  CALL gtflna (0,'RCLKOUT',CCfil%OutFilNam,irc)

  CALL gtflna (0,'SCLKOUT',CCfil%SatFilNam,irc)

!
! LIST OF INPUT FILES
! ===================
!
! Take reference clock from a special file?
! -----------------------------------------
  CALL ckoptb(1,(/'RADIO_F','RADIO_N'/),srName,                       &
              'Selection of strategy for reference clock', irCode,    &
              error=0,result1=iRefTyp)

  CCopt%refFil = (iRefTyp==1)

! Get the number of refernce file (0 or 1)
! -------------------------------
  iFil = 0
  fixFil = ' '
  IF (CCopt%refFil) THEN
    CALL gtflna (0,'RCLKREF',fixFil,irc)
    IF (irc == 0 .AND. LEN_TRIM(fixFil) > 0) iFil=1
  ENDIF

! Read the names of all other files
! ---------------------------------
  CALL gtfile2 ('RCLKINP',1,nFil,hlpFil)


! No input files selected
! -----------------------
  IF (nFil+iFil == 0) THEN
    WRITE(lfnerr,'(/,A,/)')                               &
         ' ### CCLKIN : No input rinex clock files found in input file'
    CALL exitrc(0)
  ENDIF


! Is the reference file in the input file list?
! ---------------------------------------------
  IF (iFil == 1) THEN
    DO jFil = 1, nFil
      IF (fixFil == hlpFil(1,jFil)) THEN
        iFil = 0
        EXIT
      ENDIF
    ENDDO
  ENDIF


! Allocate the memory for the input file names
! --------------------------------------------
  ALLOCATE (CCfil%ClkFilNam(nFil+iFil),stat=ios)
  CALL alcerr(ios,'CCfil%ClkFilNam',(/nFil+iFil/),'cclkin')


! Reads the input files (reference file first)
! --------------------------------------------
  nFil = 1
  IF (LEN_TRIM(fixfil) > 0) THEN
    CALL gtflna (0,'RCLKREF',CCfil%ClkFilNam(1),irc)
  ELSE
    CCfil%ClkFilNam(1) = hlpFil(1,1)
  ENDIF


! Generate the list of input files (take care on the ref. file)
! -------------------------------------------------------------
  DO iFil = 1,SIZE(hlpFil,2)
    IF (CCfil%ClkFilNam(1) /= hlpFil(1,iFil)) THEN
      nFil = nFil + 1
      CCfil%ClkFilNam(nFil) = hlpFil(1,iFil)
    ENDIF
  ENDDO

  DEALLOCATE(hlpFil, stat=irc)


!
! GENERATE A LIST OF ALL CLOCKS IN THE INPUT FILES
! ================================================
!
  ALLOCATE (InClkHead(nFil),stat=ios)
  CALL alcerr(ios,'InClkHead',(/nFil/),'ccrnxc')


! Loop all input files, read the header
! -------------------------------------
  clkList1%nSta = 0
  clkList2%nSta = 0
  DO iFil = 1, nFil

    CALL init_ClkHead(inClkHead(iFil))
    InClkHead(iFil)%TFirst=0D0

    CALL opnfil(lfnloc,CCfil%ClkFilNam(iFil),                      &
                'OLD','FORMATTED','READONLY',' ',irc)
    CALL opnerr(lfnerr,lfnloc,irc,CCfil%ClkFilNam(iFil),srName)
    IF (irc == 0) CALL rdcrxh(lfnloc,lfnerr,InClkHead(iFil),irc)
    IF (irc /= 0) irCode = irCode+1
    CLOSE(lfnloc)

! check PCV model given in header
    IF (iFil == 1) THEN
      pcvmod = InClkHead(iFil)%pcvstr
    ELSEIF(pcvmod /= InClkHead(iFil)%pcvstr) THEN
        WRITE(lfnerr,'(/,A,2(/,16X,A,A40),3(/,16X,A),/)') &
                   ' ### SR CCLKIN: Different PCV models in file headers', &
                                  'Model 1: ',pcvmod,                      &
                                  'Model 2: ',InClkHead(iFil)%pcvstr,      &
                                  'Different antenna phase center models', &
                                  'may cause consistency problems of ' //  &
                                  'the clock values ',                     &
                                  'you are going to combine.'
    ENDIF
! Update the list of station clocks
    iClkLoop1: DO iClk = 1,inClkHead(iFil)%nSta

      DO iSta = 1,clkList1%nSta
        IF (clkList1%staNam(iSta) == inClkHead(iFil)%clkName(iClk)) &
          CYCLE iCLkLoop1
      ENDDO

      IF (clkList1%nSta == 0) THEN
!!!!!!!!!!!!!!!! Problems with ifc81 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#ifdef CMP_IFC8
        iNum = clkList1%nSta+inClkHead(iFil)%nSta
        ALLOCATE(clkList1%staNam(iNum), stat=irc)
        CALL alcerr(irc,'clkList1%staNam',(/iNum/),srName)
        ALLOCATE(hlpNam1(iNum), stat=irc)
        CALL alcerr(irc,'hlpNam1',(/iNum/),srName)
#else
        ALLOCATE(clkList1%staNam(clkList1%nSta+inClkHead(iFil)%nSta), stat=irc)
        CALL alcerr(irc,'clkList1%staNam', &
                   (/clkList1%nSta+inClkHead(iFil)%nSta/),srName)
        ALLOCATE(hlpNam1(clkList1%nSta+inClkHead(iFil)%nSta), stat=irc)
        CALL alcerr(irc,'hlpNam1', &
                   (/clkList1%nSta+inClkHead(iFil)%nSta/),srName)
#endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        clkList1%staNam = ' '
        hlpNam1 = ' '

      ELSE IF (clkList1%nSta == SIZE(clkList1%staNam)) THEN

        ALLOCATE(clkList%staNam(clkList1%nSta), stat=irc)
        CALL alcerr(irc,'clkList%staNam',(/clkList1%nSta/),srName)
        clkList%staNam = clkList1%staNam
        DEALLOCATE(clkList1%staNam, stat=irc)

!!!!!!!!!!!!!!!! Problems with ifc81 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#ifdef CMP_IFC8
        iNum = clkList1%nSta+inClkHead(iFil)%nSta
        ALLOCATE(clkList1%staNam(iNum), stat=irc)
        CALL alcerr(irc,'clkList1%staNam',(/iNum/),srName)
        ALLOCATE(hlpNam1(iNum), stat=irc)
        CALL alcerr(irc,'hlpNam1',(/iNum/),srName)
#else
        ALLOCATE(clkList1%staNam(clkList1%nSta+inClkHead(iFil)%nSta), stat=irc)
        CALL alcerr(irc,'clkList1%staNam', &
                   (/clkList1%nSta+inClkHead(iFil)%nSta/),srName)
        ALLOCATE(hlpNam1(clkList1%nSta+inClkHead(iFil)%nSta), stat=irc)
        CALL alcerr(irc,'hlpNam1', &
                   (/clkList1%nSta+inClkHead(iFil)%nSta/),srName)
#endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        clkList1%staNam = ' '
        hlpNam1 = ' '

        clkList1%staNam(1:clkList1%nSta) = clkList%staNam(1:clkList1%nSta)
        DEALLOCATE(clkList%staNam, stat=irc)
      ENDIF

      clkList1%nSta = clkList1%nSta+1
      clkList1%staNam(clkList1%nSta) = inClkHead(iFil)%clkName(iClk)
    ENDDO iClkLoop1

! Update the list of satellite clocks
    iClkLoop2: DO iClk = inClkHead(iFil)%nSta+1, &
                         inClkHead(iFil)%nSta+inClkHead(iFil)%nSat
      DO iSat = 1,clkList2%nSta
        IF (clkList2%staNam(iSat) == inClkHead(iFil)%clkName(iClk)) &
          CYCLE iCLkLoop2
      ENDDO

      IF (clkList2%nSta == 0) THEN

!!!!!!!!!!!!!!!! Problems with ifc81 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#ifdef CMP_IFC8
         iNum = clkList2%nSta+inClkHead(iFil)%nSat
         ALLOCATE(clkList2%staNam(iNum), stat=irc)
         CALL alcerr(irc,'clkList2%staNam',(/iNum/),srName)
         ALLOCATE(hlpNam2(iNum), stat=irc)
         CALL alcerr(irc,'hlpNam2',(/iNum/),srName)
#else
        ALLOCATE(clkList2%staNam(clkList2%nSta+inClkHead(iFil)%nSat), stat=irc)
        CALL alcerr(irc,'clkList2%staNam', &
                    (/clkList2%nSta+inClkHead(iFil)%nSat/),srName)
        ALLOCATE(hlpNam2(clkList2%nSta+inClkHead(iFil)%nSat), stat=irc)
        CALL alcerr(irc,'hlpNam2', &
                    (/clkList2%nSta+inClkHead(iFil)%nSat/),srName)
#endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        clkList2%staNam = ' '
        hlpNam2 = ' '

      ELSE IF(clkList2%nSta == SIZE(clkList2%staNam)) THEN

        ALLOCATE(clkList%staNam(clkList2%nSta), stat=irc)
        CALL alcerr(irc,'clkList%staNam',(/clkList2%nSta/),srName)
        clkList%staNam = clkList2%staNam
        DEALLOCATE(clkList2%staNam, stat=irc)

!!!!!!!!!!!!!!!! Problems with ifc81 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#ifdef CMP_IFC8
        iNum = clkList2%nSta+inClkHead(iFil)%nSat
        ALLOCATE(clkList2%staNam(iNum), stat=irc)
        CALL alcerr(irc,'clkList2%staNam',(/iNum/),srName)
        ALLOCATE(hlpNam2(iNum), stat=irc)
        CALL alcerr(irc,'hlpNam2',(/iNum/),srName)
#else
        ALLOCATE(clkList2%staNam(clkList2%nSta+inClkHead(iFil)%nSat), stat=irc)
        CALL alcerr(irc,'clkList2%staNam', &
             (/clkList2%nSta+inClkHead(iFil)%nSat/),srName)
        ALLOCATE(hlpNam2(clkList2%nSta+inClkHead(iFil)%nSat), stat=irc)
        CALL alcerr(irc,'hlpNam2', &
                    (/clkList2%nSta+inClkHead(iFil)%nSat/),srName)
#endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        clkList2%staNam = ' '
        hlpNam2 = ' '

        clkList2%staNam(1:clkList2%nSta) = clkList%staNam(1:clkList2%nSta)
        DEALLOCATE(clkList%staNam, stat=irc)
      ENDIF

      clkList2%nSta = clkList2%nSta+1
      clkList2%staNam(clkList2%nSta) = inClkHead(iFil)%clkName(iClk)
    ENDDO iClkLoop2

  ENDDO ! next input file


! Put both lists together
! -----------------------
  clkHead%nSta = clkList1%nSta
  clkHead%nSat = clkList2%nSta

  ALLOCATE(clkhead%clkName(clkHead%nSta+2*clkHead%nSat),stat=irc)
  CALL alcerr(irc,'clkhead%clkName',(/clkHead%nSta+2*clkHead%nSat/),srName)

  clkHead%clkName(1:clkHead%nSta) = clkList1%staNam(1:clkHead%nSta)
  clkHead%clkName(clkHead%nSta+1:clkHead%nSta+clkHead%nSat) = &
                                    clkList2%staNam(1:clkHead%nSat)
  DO iClk = 1,clkHead%nSat
    READ(clkList2%staNam(iClk)(2:3),*,iostat=ios) iSat
    IF (clkList2%staNam(iClk)(1:1) == 'R') iSat = iSat+100
    WRITE(clkList2%staNam(iClk),*) iSat
    clkHead%clkName(clkHead%nSta+clkHead%nSat+iClk) = &
            ADJUSTL(clkList2%staNam(iClk))
  ENDDO

!
! LIST OF POSSIBLE REFERENCE CLOCKS
! =================================
!
! Get the refernce clock from "FIX" files
! ---------------------------------------
  IF (irefTyp == 2) THEN

    ALLOCATE(numAll(clkHead%nSta+2*clkHead%nSat),stat=irc)
    CALL alcerr(irc,'numAll',(/clkHead%nSta+2*clkHead%nSat/),srName)

    ALLOCATE(numRef(clkHead%nSta+clkHead%nSat),stat=irc)
    CALL alcerr(irc,'numRef',(/clkHead%nSta+clkHead%nSat/),srName)

    numAll = (/ (ii,ii=1,clkHead%nSta+2*clkHead%nSat) /)

! Get the list of station reference clocks
! ----------------------------------------
    CALL readKeys('REFCLOCK',keyValue,irc)
    IF (irc == 0) THEN
      IF (keyValue(1) /= 'ALL_SATELLITES') THEN
        CALL gtStaNum(clkHead%nSta, numAll,                       &
                      clkHead%clkName(1:clkHead%nSta),            &
                      'REFCLOCK','STAMAN','STAFIL',' ',           &
                      clkList1%nSta, numRef, hlpNam1, 0, dummy)
        clkList1%staNam(1:clkList1%nSta) = hlpNam1(1:clkList1%nSta)
      ENDIF
    ENDIF

! Get the list of satellite reference clocks
! ------------------------------------------
    clkList2%nSta = 0
    IF (irc == 0) THEN
      IF (keyValue(1) == 'MANUAL' .OR. keyValue(1) == 'FROM_FILE' .OR.         &
          keyValue(1) == 'ALL_SATELLITES') THEN
        hlpSat = 2*clkHead%nSat
        IF (keyValue(1) == 'ALL_SATELLITES') hlpSat = clkHead%nSat
        CALL gtStaNum(hlpSat, numAll,                                  &
                  clkHead%clkName(clkHead%nSta+1:clkHead%nSta+hlpSat), &
                  'REFCLOCK','SATMAN','SATFIL',' ',                            &
                  clkList2%nSta, numRef, hlpNam2, 0, dummy)
        clkList2%staNam(1:clkList2%nSta) = hlpNam2(1:clkList2%nSta)
      ENDIF
    ENDIF

! Generate the list of reference clock candidates
! -----------------------------------------------
    CCopt%nRef = clkList1%nSta + clkList2%nSta

    ALLOCATE(CCopt%refClk(CCopt%nRef), stat=ios)
    CALL alcerr(ios,'CCopt%refClk',(/CCopt%nRef/),srName)

    IF (clkList1%nSta > 0) &
      CCopt%refClk(1:clkList1%nSta) = clkList1%stanam(1:clkList1%nSta)

    IF (clkList2%nSta > 0) THEN
      DO iClk = 1,clkList2%nSta
        IF (clkList2%staNam(iClk)(1:1) == 'G' .OR. &
            clkList2%staNam(iClk)(1:1) == 'R') THEN
          CCopt%refClk(clkList1%nSta+iClk) = clkList2%stanam(iClk)
        ELSE
          READ(clkList2%staNam(iClk),*,iostat=ios) iSat
          WRITE(CCopt%refClk(clkList1%nSta+iClk),'(I3.3)') iSat
          IF (iSat < 100) THEN
            CCopt%refClk(clkList1%nSta+iClk)(1:1) = 'G'
          ELSE IF (iSat < 200) THEN
            CCopt%refClk(clkList1%nSta+iClk)(1:1) = 'R'
          ENDIF
        ENDIF
      ENDDO
    ENDIF

    DEALLOCATE(numAll, stat=irc)
    DEALLOCATE(numRef, stat=irc)
  ENDIF

  DEALLOCATE(clkList1%staNam, stat=irc)
  DEALLOCATE(clkList2%staNam, stat=irc)

  DEALLOCATE(clkHead%clkName, stat=irc)


! Stop if no ref. clock or ref. file was spec.
! --------------------------------------------
  IF (CCopt%nref == 0 .AND. .NOT. CCopt%refFil) THEN
    write(lfnerr,'(/,A,/)')                               &
          ' *** SR CCLKIN : No reference clock for the output file was given.'
    irCode=irCode+1
  ENDIF

!
! REFERENCE CLOCK SELECTION
! =========================
!
  IF (.NOT. CCOpt%refFil) THEN

! Polynom for reference clock alignment
! -------------------------------------
    CALL readKeys('NALIGN',keyValue,irc)

    CALL ckopti(1,'NALIGN',keyValue,srName,                                 &
                'Degree of polynom for reference clock align.',irc,irCode,  &
                maxVal=1,ge=0,le=20,error=0,result1=CCopt%nAlig)


! Polynom for reference clock alignment
! -------------------------------------
    CALL readKeys('MAXREF',keyValue,irc)

    CALL ckoptr(1,'MAXREF',keyValue,srName,                                 &
                'Max. rms for reference clock alignment',irc,irCode,        &
                maxVal=1,gt=0d0,empty=0d0,error=99d0,result1=CCopt%maxRef)


! Select only one reference clock
! -------------------------------
    CALL ckoptb(1,(/'ONEREF'/),srName,'Select only one reference clock',    &
                irCode,resultL=CCopt%oneRef)

  ENDIF

!
! CLOCK LIST TO BE PROCESSED
! ==========================
!
! List of clocks to be included: get field size
! ---------------------------------------------
  iClk=2

  CALL readKeys('SELSTA',keyValue,irc)

  CALL ckoptc(1,'SELSTA',keyValue,                                        &
             (/'NONE     ', 'FROM_FILE', 'FROM_LIST',                     &
             'ALL      ' /),                                              &
             srName,'List of station clocks to be processed',irc,irCode,  &
             maxVal=1,result1=iSel(1))


  CALL readKeys('SELSAT',keyValue,irc)

  CALL ckoptc(1,'SELSAT',keyValue,                                        &
             (/'NONE     ', 'FROM_FILE', 'FROM_LIST', 'ALL      ' /),     &
             srName,'List of satellite clocks to be processed',irc,irCode,&
             maxVal=1,result1=iSel(2))


  keyWord='STA'
  DO ii=1,2
    IF (iSel(ii) == 2) THEN
      CALL gtflna(0,'SFIL'//keyWord,fixfil,irc)
      IF (LEN_TRIM(fixfil) > 0 .AND. irc == 0) THEN
        CALL readstsg(fixfil,0,clkList)
        iClk = iClk-1+clkList%nSta
        DEALLOCATE(clkList%staNam, stat=irc)
      ENDIF
    ENDIF

    IF (iSel(ii) == 3) THEN
      CALL readKeys('SLST'//keyWord,keyValue,irc)
      IF (irc == 0) iClk = iClk-1+SIZE(keyValue)
    ENDIF
    keyWord='SAT'
  ENDDO


! Allocate the the list of clocks
! -------------------------------
  ALLOCATE(CCopt%clklst(iClk),stat=ios)
  CALL alcerr(ios,'CCopt%clklst',(/iClk/),'cclkin')

! Read the list of clocks
! -----------------------
!  CCopt%clklst(1)='STA:ALL'
!  CCopt%clklst(2)='SAT:ALL'
  iClk = 0
  keyWord = 'STA'
  DO ii = 1,2
    IF (iSel(ii) == 1) THEN           ! include no clocks
      iClk = iClk + 1
      CCopt%clklst(iClk) = TRIM(keyWord)//':NONE'
      iclk0=iclk ! do not translate satnum->satnam

    ELSE IF (iSel(ii) == 2) THEN           ! read from file
      iclk0=iclk ! translate satnum->satnam
      CALL gtflna(1,'SFIL'//keyWord,fixfil,irc)
      IF (LEN_TRIM(fixfil) > 0 .AND. irc == 0) THEN
        CALL readstsg(fixfil,0,clkList)
        CCopt%clklst(iClk+1:iClk+clkList%nSta) = &
             clklist%stanam(1:clkList%nSta)
        iClk = iClk+clkList%nSta
        DEALLOCATE(clkList%staNam, stat=irc)
      ENDIF

    ELSE IF (iSel(ii) == 3) THEN           ! read from list
      iclk0=iclk ! translate satnum->satnam
      CALL readKeys('SLST'//keyWord,keyValue,irc)

      CALL ckoptl(0,'SLST'//keyWord,keyValue,srName,                      &
           'Define a list of clocks to be processed',irc,irCode,   &
           nResult=jClk,                                           &
           result2=CCopt%clkLst(iClk+1:iClk+SIZE(keyValue)))
      iClk = iClk+jClk

    ELSE IF (iSel(ii) == 4 .OR. iSel(ii) == -1) THEN      ! use all clocks
      iClk=iClk+1
      CCopt%clklst(iClk) = TRIM(keyWord)//':ALL'
      iclk0=iclk ! do not translate satnum->satnam
    ENDIF

! translate sat names
    IF (keyWord=='SAT' .AND. iclk0/=iclk) then
      DO jclk=iclk0+1,iclk
        READ(CCopt%clkLst(jclk),*,iostat=ios) svnNum
        IF (ios /= 0) CYCLE
        CALL svn2chr(svnNum,satNum,satChr)
        CCopt%clkLst(jclk) = ' '
        WRITE(CCopt%clkLst(jclk),'(A,I2.2)') satChr,satNum
      ENDDO
    ENDIF
    keyWord='SAT'
  ENDDO


!
! OFFSET COMPUTATION
! ==================
!
! Offset computation: only reference stations
! -------------------------------------------
  CALL ckoptb(1,(/'ONLYREF'/),srName,                                       &
              'Offset computation: use only ref. clocks', irCode,           &
              resultL=CCopt%onlyRef)


! Offset computation: only station clocks
! ---------------------------------------
  CALL ckoptb(1,(/'USESTA'/),srName,                                        &
              'Offset computation: use station clocks', irCode,             &
              resultL=CCopt%useSta)


! Offset computation: only satellite clocks
! -----------------------------------------
  CALL ckoptb(1,(/'USESAT'/),srName,                                        &
              'Offset computation: use satellite clocks', irCode,           &
              resultL=CCopt%useSat)


! Remove satellite clocks with eclipse flag
! -----------------------------------------
  CALL readKeys('DELECLIPS',keyValue,irc)

  CALL ckoptc(1,'DELECLIPS',keyValue,                                       &
             (/'0                 ','1                 ',                   &
               'KEEP              ','REMOVE_FLAG_ONLY  ',                   &
               'REMOVE_ALL_RECORDS' /),srName,                              &
             'Remove satellite clocks with eclipse flag',irc,irCode,        &
             maxVal=1,valList=(/0,2,0,1,2/),error=0,result1=CCopt%delEcl)


! Offset computation: unit of wgt for offset comp.
! ------------------------------------------------
  CALL readKeys('SIGMA0',keyValue,irc)

  CALL ckoptr(1,'SIGMA0',keyValue,srName,                                   &
              'Offset computation: apriori unit of weight',irc,irCode,      &
              maxVal=1,gt=0d0,error=99d0,result1=CCopt%Sigma0)


! Offset computation: Max. resi allowed
! -------------------------------------
  CALL readKeys('MAXRESI',keyValue,irc)

  CALL ckoptr(1,'MAXRESI',keyValue,srName,                                  &
              'Offset computation: max. residual allowed',irc,irCode,       &
              maxVal=1,empty=0d0,ge=0d0,error=99d0,result1=CCopt%maxResi)

!
! COMBINING THE FILES
! ===================
!
! Options for combining the files
! -------------------------------
  CALL readKeys('COMBI',keyValue,irc)

  CALL ckoptc(1,'COMBI',keyValue,                                           &
             (/'UNWEIGHTED ', 'COMBINATION', 'INPUT_FILES' /),srName,       &
             'Comb. clock values: weighting strategy',irc,irCode,           &
             maxVal=1,valList=(/0,1,2/),error=0,result1=CCopt%Combi)


! Combining options: max. dev. from mean allowed
! ----------------------------------------------
  CALL readKeys('MAXMEAN',keyValue,irc)

  CALL ckoptr(1,'MAXMEAN',keyValue,srName,                                  &
              'Comb. clock values: residual allowed',irc,irCode,            &
              maxVal=1,empty=0d0,ge=0d0,error=99d0,result1=CCopt%maxMean)


! Combining options: min. # for station clocks
! --------------------------------------------
  CALL readKeys('MINSTA',keyValue,irc)

  CALL ckopti(1,'MINSTA',keyValue,srName,                                   &
              'Comb. clock values: min. # of station clocks',irc,irCode,    &
              maxVal=1,ge=1,error=1,result1=CCopt%minClkSta)



! Combining options: min. # for satellite clocks
! ----------------------------------------------
  CALL readKeys('MINSAT',keyValue,irc)

  CALL ckopti(1,'MINSAT',keyValue,srName,                                   &
              'Comb. clock values: min # of satellite clocks',irc,irCode,   &
              maxVal=1,ge=1,error=1,result1=CCopt%minClkSat)


! Combining options: Sigma for output
! -----------------------------------
  CALL readKeys('OUTSIG',keyValue,irc)

  CALL ckoptc(1,'OUTSIG',keyValue,(/'COMBINATION', 'INPUT_FILES' /),srName, &
             'Comb. clock values: sigma strategy for ouput',irc,irCode,     &
             maxVal=1,error=1,result1=CCopt%outSigma)


!
! JUMP DETECTION
! ==============
!
! Jump detection: confidence interval
! -----------------------------------
  CALL ckoptb(1,(/'DOJUMP'/),srName,'Enable jump detection', irCode,        &
              error=0,result1=CCopt%nJmpSig)

  IF (CCopt%nJmpSig /= 0) CCopt%nJmpSig = -1


! Jump detection: contidence interval
! -----------------------------------
  IF (CCopt%nJmpSig == -1) THEN
    CALL readKeys('NJMPSIG',keyValue,irc)

    CALL ckopti(1,'NJMPSIG',keyValue,srName,                                &
                'Jump detection: confidence interval',irc,irCode,           &
                maxVal=1,ge=1,error=0,result1=CCopt%nJmpSig)


! Jump detection: minimum RMS
! ---------------------------
    CALL readKeys('MJMPRMS',keyValue,irc)

    CALL ckoptr(1,'MJMPRMS',keyValue,srName,                                &
                'Jump detection: min. rms used for jump det.',irc,irCode,   &
                maxVal=1,ge=0d0,error=99d0,result1=CCopt%jumpRMS)


! Jump detection: min. # epochs between two jumps
! -----------------------------------------------
    CALL readKeys('NJMPEPO',keyValue,irc)

    CALL ckopti(1,'MJMPEPO',keyValue,srName,                                &
                'Jump detection: max. epochs between two jumps',irc,irCode, &
                maxVal=1,ge=0,empty=0,error=0,result1=CCopt%njmpEpo)


! Jump detection: delete outliers from output
! -------------------------------------------
    CALL ckoptb(1,(/'DJMPSTA'/),srName,                                     &
                'Jump detection: del. outliers for sta. clocks', irCode,    &
                resultL=CCopt%jmpdelsta)


    CALL ckoptb(1,(/'DJMPSAT'/),srName,                                     &
                'Jump detection: del. outliers for sat. clocks', irCode,    &
                resultL=CCopt%jmpdelsat)


! Jump detection: polynom for jump estimation
! -------------------------------------------
    CALL ckoptb(1,(/'DJMPEST'/),srName,                                     &
                'Jump detection: enable size estim.',irCode,                &
                result1 = CCopt%nJmpPoly)
    IF (CCopt%nJmpPoly == 1) THEN
      CALL readKeys('NJMPPOL',keyValue,irc)

      CALL ckopti(1,'MJMPPOL',keyValue,srName,                                &
                  'Jump detection: deg. of polynom for jump est.',irc,irCode, &
                   maxVal=1,ge=0,error=0,result1=CCopt%nJmpPoly)
    ELSE
      CCopt%nJmpPoly = -1
    ENDIF

  ENDIF ! only if jump detection is on

!
! CLK RINEX HEADER INFO
! =====================
!
! Information for the new clk. rinex header
! -----------------------------------------
  IF (LEN_TRIM(CCfil%OutFilNam) > 0) THEN

    CALL readKeys('RUNBY',keyValue,irc)

    CALL ckoptl(0,'RUNBY',keyValue,srName,                                  &
               'Clock rinex output: "runby"',irc,irCode,                    &
               nError=iErr,maxVal=1,result1=clkHead%runBy)
    IF (iErr /= 0) clkHead%runBy = ' '


    CALL readKeys('AC',keyValue,irc)

    CALL ckoptl(0,'AC',keyValue,srName,                                     &
               'Clock rinex output: "AC-ID"',irc,irCode,                    &
               nError=iErr,maxVal=1,result1=clkHead%ac)
    IF (iErr /= 0) clkHead%ac = ' '


    CALL readKeys('ACNAME',keyValue,irc)

    CALL ckoptl(0,'ACNAME',keyValue,srName,                                 &
               'Clock rinex output: "AC name"',irc,irCode,                  &
               nError=iErr,maxVal=1,result1=clkHead%acName)
    IF (iErr /= 0) clkHead%acName = ' '



    CALL readKeys('COMMENT',keyValue,irc)

    ClkHead%nComment = 0
    IF (irc==0) THEN

      ClkHead%nComment = SIZE(keyvalue)

      IF (ClkHead%nComment == 1 .AND. LEN_TRIM(keyValue(1)) == 0) THEN
        ClkHead%nComment = 0

      ELSE
        ALLOCATE(ClkHead%Comment(ClkHead%nComment),stat=ios)
        CALL alcerr(ios,'ClkHead%Comment',(/ClkHead%nComment/),'cclkin')

        CALL ckoptl(0,'COMMENT',keyValue,srName,                              &
                    'Clock rinex output: comment lines',irc,irCode,           &
                    maxVal=ClkHead%nComment,result2=clkHead%Comment)
      ENDIF
    ENDIF


  ENDIF

! Read infomation for the time window
! -----------------------------------
  CALL gttimwin('USEWIN',(/'RADIO_1','RADIO_2'/),        &
                (/'SESSION_YEAR','SESSION_STRG'/),       &
                (/'STADAT','STATIM','ENDDAT','ENDTIM'/), &
                CCopt%Timewin(:))
!
! New Sampling rate
! -----------------
  CALL readKeys('DELTAT',keyValue,irc)

  CALL ckopti(1,'DELTAT',keyValue,srName,                                   &
              'Sampling interval for output file',irc,irCode,               &
              maxVal=1,ge=0,empty=0,error=0,result1=CCopt%DeltaT)


!
! INTERPOLATION/EXTRAPOLATION
! ===========================
!
! Do an interpolation
! -------------------
  CALL ckoptb(1,(/'DOINTER'/),srName,'Enable interpolation', irCode,        &
              resultL=CCopt%doInter)

! Do an extrapolation
! -------------------
  CALL ckoptb(1,(/'DOEXTRA'/),srName,'Enable extrapolation', irCode,        &
              resultL=CCopt%doExtra)


! Polynom for interpolation/extrapolation
! ---------------------------------------
  IF (CCopt%doInter .OR. CCopt%doExtra) THEN
    CALL readKeys('POLYNOM',keyValue,irc)

    CALL ckopti(1,'POLYNOM',keyValue,srName,                                &
                'Inter/Extrapolation: degree of polnom',irc,irCode,         &
                maxVal=1,ge=0,le=20,error=0,result1=CCopt%iePoly)


! Periodic for interpolation/extrapolation
! ----------------------------------------
    CALL readKeys('PERIOD',keyValue,irc)

    CALL ckopti(1,'PERIOD',keyValue,srName,                                 &
                'Inter/Extrapolation: periodic terms',irc,irCode,           &
                maxVal=1,ge=0,empty=0,error=0,result1=CCopt%ieSiCo)


! Max. RMS for polynomial fit for interpolation/extrapolation
! -----------------------------------------------------------
    CALL readKeys('POLYRMS',keyValue,irc)

    CALL ckoptr(1,'POLYRMS',keyValue,srName,                                  &
                'Inter/Extrapolation: max rms to use the model',irc,irCode,   &
                maxVal=1,ge=0d0,empty=0d0,error=0d0,result1=CCopt%iemRMS)


! Min. # of obs. for polynomial fit for interpolation/extrapolation
! -----------------------------------------------------------------
    CALL readKeys('POLYOBS',keyValue,irc)

    CALL ckopti(1,'POLYOBS',keyValue,srName,                                &
                'Inter/Extrapolation: min data points',irc,irCode,          &
                maxVal=1,ge=0,empty=0,error=0,result1=CCopt%ieMobs)


! More parameters for inter/extrapolation function
! ------------------------------------------------
    CALL ckoptb(1,(/'MOREINEX'/),srName,                                    &
                'Inter/Extrapolation: advanced setup', irCode,              &
                error=0,result1=ii)
    IF (ii == 1) THEN

! Read the additional definition records
! --------------------------------------
      CALL readKeys('FUNCSTR',keyValue,irc)

      ALLOCATE(hlpStr(6,SIZE(keyValue)), stat=iac)
      CALL alcerr(iac, 'hlpStr', (/6,SIZE(keyValue)/), 'cclkin')

      ALLOCATE(kPoly(SIZE(keyValue)), stat=iac)
      CALL alcerr(iac, 'kPoly', (/SIZE(keyValue)/), 'cclkin')

      ALLOCATE(kSico(SIZE(keyValue)), stat=iac)
      CALL alcerr(iac, 'kSico', (/SIZE(keyValue)/), 'cclkin')

      ALLOCATE(funcWin(4,SIZE(keyValue)), stat=iac)
      CALL alcerr(iac, 'funcWin', (/4,SIZE(keyValue)/), 'cclkin')


! Extract the array-size necessary for the add. requests
! ------------------------------------------------------
      ircSave = irCode
      CALL ckoptu(1,'FUNCSTR',keyValue,srName,                              &
                  'Inter/Extrapolation: advanced model setup',irc,irCode,6, &
                  maxVal=SIZE(hlpStr,2),result2=hlpStr)
      ircSave = irCode - ircSave

      CALL ckopti(1,'FUNCSTR',hlpStr(1,:),srName,                           &
                  'Inter/Extrapolation: advanced model setup',              &
                  ircSave,irCode,colTit='polyn. degree',                    &
                  maxVal=SIZE(kPoly),empty=-1,error=-1,ge=0,result2=kPoly)

      CALL ckoptr(1,'FUNCSTR',hlpStr(2,:),srName,                           &
                  'Inter/Extrapolation: advanced model setup',              &
                  ircSave,irCode,colTit='period. terms',                    &
                  maxVal=SIZE(kSico),empty=-1d0,error=-1d0,                 &
                  gt=0d0,result2=kSiCo)

      CALL ckoptd(1,'FUNCSTR',hlpStr(3,:),srName,                           &
                  'Inter/Extrapolation: advanced model setup',              &
                  ircSave,irCode,colTit='start date for function comp.',    &
                  maxVal=SIZE(funcWin,2),error=-1d0,                        &
                  result2=funcWin(1,:))

      CALL ckoptt(1,'FUNCSTR',hlpStr(4,:),srName,                           &
                  'Inter/Extrapolation: advanced model setup',              &
                  ircSave,irCode,colTit='start time for function comp.',    &
                  maxVal=SIZE(funcWin,2),error=-1d0,                        &
                  ge=0d0,lt=24d0,result2=funcWin(2,:))

      CALL ckoptd(1,'FUNCSTR',hlpStr(5,:),srName,                           &
                  'Inter/Extrapolation: advanced model setup',              &
                  ircSave,irCode,colTit='end date for function comp.',      &
                  maxVal=SIZE(funcWin,2),error=-1d0,                        &
                  result2=funcWin(3,:))

      CALL ckoptt(1,'FUNCSTR',hlpStr(6,:),srName,                           &
                  'Inter/Extrapolation: advanced model setup',              &
                  ircSave,irCode,colTit='end time for function comp.',      &
                  maxVal=SIZE(funcWin,2),error=-1d0,                        &
                  ge=0d0,lt=48d0,result2=funcWin(4,:))


! Remove lines with invalid time window
! -------------------------------------
      DO ii = 1, SIZE(funcWin,2)
        DO jj = 1, SIZE(funcWin,1)
          IF (funcWin(jj,ii) == -1d0) THEN
            kPoly(ii) = -1
            kSico(ii) = -1d0
            EXIT
          ENDIF
        ENDDO
      ENDDO

! Get the number of function components
! -------------------------------------
      maxPoly = CCopt%iePoly
      maxSico = CCopt%ieSico

      DO ii = 1, SIZE(hlpStr,2)
        IF (kPoly(ii) > maxPoly) maxPoly = kPoly(ii)
        IF (kSico(ii) /= -1d0) maxSico = maxSico + 1
      ENDDO

! Allocate and init polynomial time record
! ----------------------------------------
      ALLOCATE(CCopt%ieTpoly(maxPoly+1), stat=ios)
      CALL alcerr(ios,'CCopt%ieTpoly',(/maxPoly/),'cclkin')

      DO ii = 1, CCopt%IEpoly+1
        CCopt%ieTpoly(ii)%t(1:2) = (/ 0d0, 99D99 /)
        DO jj = 1, SIZE(kPoly)
          IF (kPoly(jj)+1 == ii) THEN
            CCopt%ieTpoly(ii)%t(1) = funcWin(1,jj) + funcWin(2,jj)/24d0
            CCopt%ieTpoly(ii)%t(2) = funcWin(3,jj) + funcWin(4,jj)/24d0
            EXIT
          ENDIF
        ENDDO
      ENDDO

      DO ii = CCopt%iePoly+2,maxPoly+1
        CCopt%ieTpoly(ii)%t(1:2) = (/ 0d0, 0D0 /)
        DO jj = 1, SIZE(kPoly)
          IF (kPoly(jj)+1 == ii) THEN
            CCopt%ieTpoly(ii)%t(1) = funcWin(1,jj) + funcWin(2,jj)/24d0
            CCopt%ieTpoly(ii)%t(2) = funcWin(3,jj) + funcWin(4,jj)/24d0
            EXIT
          ENDIF
        ENDDO
      ENDDO
      CCopt%IEpoly = maxPoly

! Allocate and init periodic time record
! --------------------------------------
      ALLOCATE(CCopt%ieTsico(maxSico), stat=ios)
      CALL alcerr(ios,'CCopt%ieTsico',(/maxSico/),'cclkin')

      ALLOCATE(CCopt%iePsico(maxSico), stat=ios)
      CALL alcerr(ios,'CCopt%iePsico',(/maxSico/),'cclkin')

      DO ii = 1, CCopt%IEsico
        CCopt%iePsico(ii)        = DBLE(ii)
        CCopt%ieTsico(ii)%t(1:2) = (/ 0d0, 99D99 /)
        DO jj = 1, SIZE(kSico)
          IF (kSico(jj) == CCopt%iePsico(ii)) THEN
            CCopt%ieTsico(ii)%t(1) = funcWin(1,jj) + funcWin(2,jj)/24d0
            CCopt%ieTsico(ii)%t(2) = funcWin(3,jj) + funcWin(4,jj)/24d0
            EXIT
          ENDIF
        ENDDO
      ENDDO

      iiLoop: DO ii = 1, SIZE(kSico)
        IF (kSico(ii) == -1d0) CYCLE iiLoop
        DO jj = 1, CCopt%IEsico
          IF (CCopt%iePsico(jj) == kSiCo(ii)) CYCLE iiLoop
        ENDDO
        CCopt%IEsico = CCopt%IEsico + 1
        CCopt%iePsico(CCopt%IEsico)      = kSico(ii)
        CCopt%ieTsico(CCopt%IEsico)%t(1) = funcWin(1,ii) + funcWin(2,ii)/24d0
        CCopt%ieTsico(CCopt%IEsico)%t(2) = funcWin(3,ii) + funcWin(4,ii)/24d0
      ENDDO iiLoop

! Deallocate records for extracting uniline
! -----------------------------------------
      DEALLOCATE(kPoly,stat=iac)
      DEALLOCATE(kSico,stat=iac)
      DEALLOCATE(funcWin,stat=iac)
      DEALLOCATE(hlpStr,stat=irc)

! no advanced setup was used
! --------------------------
    ELSE

      ALLOCATE(CCopt%ieTpoly(CCopt%iePoly+1), stat=ios)
      CALL alcerr(ios,'CCopt%ieTpoly',(/CCopt%iePoly/),'cclkin')

      DO ii = 1, CCopt%IEpoly+1
        CCopt%ieTpoly(ii)%t(1:2) = (/ 0d0, 99D99 /)
      ENDDO

      ALLOCATE(CCopt%ieTsico(CCopt%IEsico), stat=ios)
      CALL alcerr(ios,'CCopt%ieTsico',(/CCopt%IEsico/),'cclkin')
      ALLOCATE(CCopt%iePsico(CCopt%IEsico), stat=ios)
      CALL alcerr(ios,'CCopt%iePsico',(/CCopt%IEsico/),'cclkin')

      DO ii = 1, CCopt%IEsico
        CCopt%iePsico(ii)        = DBLE(ii)
        CCopt%ieTsico(ii)%t(1:2) = (/ 0d0, 99D99 /)
      ENDDO

    ENDIF
  ENDIF ! do inter/extrapolation


! Get Print options
! -----------------
  CALL ckoptb(1,(/'PRTINP'/),srName,                                        &
              'Print details on input clock files',irCode,                  &
              result1=CCopt%prtDetail(prtinp))

  CALL ckoptb(1,(/'PRTCMB'/),srName,                                        &
              'Print details on input clock files',irCode,                  &
              result1=CCopt%prtDetail(prtcmb))

  IF (.NOT. CCopt%refFil)                                                   &
    CALL ckoptb(1,(/'PRTREF'/),srName,                                      &
                'Print details on input clock files',irCode,                &
                result1=CCopt%prtDetail(prtref))

  IF (CCopt%nJmpSig /= 0)                                                   &
    CALL ckoptb(1,(/'PRTJMP'/),srName,                                      &
                'Print details on input clock files',irCode,                &
                result1=CCopt%prtDetail(prtjmp))

  IF (CCopt%doInter .OR. CCopt%doExtra)                                     &
    CALL ckoptb(1,(/'PRTEXT'/),srName,                                      &
                'Print details on input clock files',irCode,                &
                result1=CCopt%prtDetail(prtext))

  CALL ckoptb(1,(/'PRTRES'/),srName,                                        &
              'Print details on input clock files',irCode,                  &
              result1=CCopt%prtDetail(prtres))



! Get sort order for clock statistic in output
! --------------------------------------------
  IF (CCopt%prtDetail(prtres) /= 0) THEN
    CALL readKeys('CLKSORT',keyValue,irc)

    CALL ckoptc(1,'CLKSORT',keyValue,                                         &
                (/ 'NONE ','ALPHA','SIGMA' /),srName,                         &
                'Printing options: sort order in pgm output',irc,irCode,      &
                maxVal=1,valList=(/1,2,3/),error=1,                           &
                result1=CCopt%PrtDetail(prtres))
  ENDIF

! Maximum coordinate differences allowed
! --------------------------------------
  CCopt%maxDcrd=0.5 ! meter


! Write input options into the protocol
! -------------------------------------
  WRITE(lfnprt,'(/,2(A,/))')                                                   &
       ' INPUT OPTIONS FOR THE CLOCK COMBINATION',                             &
       ' ---------------------------------------'

  IF (CCopt%useSta) THEN
    WRITE(lfnprt,'(A)') ' Use station clocks for combination'
  ELSE
    WRITE(lfnprt,'(A)') ' Do not use station clocks for combination'
  ENDIF

  IF (CCopt%useSat) THEN
    WRITE(lfnprt,'(A)') ' Use satellite clocks for combination'
  ELSE
    WRITE(lfnprt,'(A)') ' Do not use satellite clocks for combination'
  ENDIF

  IF (CCopt%onlyRef) &
    WRITE(lfnprt,'(A)') ' Use only reference clocks for combination'

  IF (CCopt%delEcl == 1) &
    WRITE(lfnprt,'(A)') ' Remove eclipse flags from output clock RINEX'

  IF (CCopt%delEcl == 2) &
    WRITE(lfnprt,'(A)') ' Remove satellite clocks with eclipse flag'

  IF (CCopt%maxresi == 0d0) THEN
    WRITE(lfnprt,'(/,A,F18.3,2(A,/))')                                         &
         ' Apriori unit of weight:      ',CCopt%sigma0 ,' ns',                 &
         ' Limit for outlier detection:           disabled'
  ELSE
    WRITE(lfnprt,'(2(/,A,F18.3,A),/)')                                         &
         ' Apriori unit of weight:      ',CCopt%sigma0 ,' ns',                 &
         ' Limit for outlier detection: ',CCopt%maxresi,' ns'
  ENDIF


  IF (CCopt%maxmean == 0d0) THEN
    WRITE(lfnprt,'(A,2(/,A,I18,A),/)')                                         &
       ' Max. allowed dev. from mean:           disabled',                     &
       ' Min. number of valid clocks: ',CCopt%MinClkSta,' for station clocks', &
       ' Min. number of valid clocks: ',CCopt%MinClkSat,' for satellite clocks'
  ELSE
    WRITE(lfnprt,'(A,F18.3,A,2(/,A,I18,A),/)')                                 &
       ' Max. allowed dev. from mean: ',CCopt%maxmean,' ns',                   &
       ' Min. number of valid clocks: ',CCopt%MinClkSta,' for station clocks', &
       ' Min. number of valid clocks: ',CCopt%MinClkSat,' for satellite clocks'
  ENDIF

  IF (CCopt%Combi == 0) THEN
    WRITE(lfnprt,'(A)')                                                        &
         ' Combination of the rinex clock files by unweighted mean'
  ELSE
    WRITE(lfnprt,'(A)')                                                        &
         ' Combination of the rinex clock files by weighted mean'
    IF (CCopt%Combi == 1) THEN
      WRITE(lfnprt,'(A)')                                                      &
           ' Weights were taken from the combination'
    ELSE IF (CCopt%Combi == 2) THEN
      WRITE(lfnprt,'(A)')                                                      &
           ' Weights were taken from the sigma in the input files'
    ENDIF
  ENDIF

  IF (CCopt%OutSigma == 1) THEN
    WRITE(lfnprt,'(A)')                                                        &
         ' Sigmas for the output file were taken from the combination'
  ELSE IF (CCopt%OutSigma == 2) THEN
    WRITE(lfnprt,'(2A)')                                                       &
         ' Sigmas for the output file come from error propagation ',           &
         'of the input files'
  ENDIF

  IF (CCopt%refFil) THEN
    WRITE(lfnprt,'(//,2(/,A),//,2A)')                                          &
         ' REFERENCE CLOCK SELECTION',                                         &
         ' -------------------------',                                         &
         ' Reference clock from file   :  ',TRIM(CCfil%ClkFilNam(1))
  ELSE
    WRITE(lfnprt,'(//,2(/,A),//,A,I14)')                                       &
         ' ALIGNMENT TO THE NEW REFERENCE CLOCK',                              &
         ' ------------------------------------',                               &
         ' Degree of polynom for alignment: ',CCopt%nAlig
    IF (CCopt%maxRef > 0d0) &
      WRITE(lfnprt,'(A,F14.3,A)')                                              &
           ' Max. rms in the alignment:       ',CCopt%maxRef,' ns'
    IF (CCopt%oneRef) &
      WRITE(lfnprt,'(A)')                                                      &
           ' Select only one reference station'

    DO ii=1,CCopt%nRef
      IF (MOD(ii-1,4) == 0) THEN
        IF (ii == 1) THEN
          Line = 'List of possible ref. clocks:  '
        ELSE
          Line = ' '
        ENDIF
      ENDIF
      i1 = 34+MOD(ii-1,4)*24
      i2 = i1+20
      READ(CCopt%refClk(ii), *, iostat=ios) jj
      IF (ios == 0 .AND. 0 < jj .AND. 100 > jj) THEN
        WRITE(Line(I1:I2),'(A)') 'G'//TRIM(CCopt%refClk(ii))
      ELSE IF (ios == 0 .AND. 100 < jj .AND. 200 > jj) THEN
        WRITE(Line(I1:I2),'(A)') 'R'//TRIM(CCopt%refClk(ii))
      ELSE
        WRITE(Line(I1:I2),'(A)') CCopt%refClk(ii)
      ENDIF
      IF (ii == CCopt%nRef .OR. MOD(ii-1,4) == 3) WRITE(lfnprt,'(1X,A)') TRIM(Line)
    ENDDO
  ENDIF

  WRITE(lfnprt,'(//,2(/,A),/)')                                                &
       ' JUMP DETECTION FOR COMBINED CLOCKS',                                  &
       ' ----------------------------------'
  IF (CCopt%nJmpSig == 0) THEN
    WRITE(lfnprt,'(A)') ' Jump detection is disabled'
  ELSE
    WRITE(lfnprt,'(A,F13.3,A,/,A,I13)')                                        &
         ' Minimum noise of a clock:         ',CCopt%jumpRMS,' ns / 300s',     &
         ' Confidence interval for jump det.:',CCopt%nJmpSig
    IF (CCopt%nJmpEpo > 0) THEN
      WRITE(lfnprt,'(A,I13,A,3(/,A))')                                         &
           ' Outlier or two jumps?:            ',CCopt%nJmpEpo,' epochs',      &
           '   If two jumps for one clock are located with in this limits',    &
           '   an outlier instead of two jumps is possible.',                  &
           '   (The size of these jumps must be equal with opposite sign.)'
      IF (CCopt%jmpDelSta) &
        WRITE(lfnprt,'(A)') ' Remove station clock outliers'
      IF (CCopt%jmpDelSat) &
        WRITE(lfnprt,'(A)') ' Remove satellite clock outliers'
    ENDIF
    IF (CCopt%nJmpPoly >= 0) THEN
      WRITE(lfnprt,'(A,I13)')                                                  &
         ' Polynomial deg. for est. the size:',CCopt%nJmpPoly
    ELSE
      WRITE(lfnprt,'(A)')                                                      &
         ' Polynomial deg. for est. the size:     disabled'
    ENDIF
  ENDIF

  IF (.NOT. CCopt%refFil) THEN
    IF (CCopt%doInter .OR. CCopt%doExtra) THEN
      IF (CCopt%doInter) THEN
        WRITE(lfnprt,'(//,2(/,1X,A),/)')                                       &
             'INTERPOLATION/EXTRAPOLATION',                                    &
             '---------------------------'
      ELSE
        WRITE(lfnprt,'(//,2(/,1X,A),/)')                                       &
             'CLOCK EXTRAPOLATION',                                            &
             '-------------------'
      ENDIF
      IF (CCopt%doInter) &
        WRITE(lfnprt,'(A)') ' Interpolation allowed'
      IF (CCopt%doExtra) &
        WRITE(lfnprt,'(A)') ' Extrapolation allowed'
      IF (CCopt%IEmRMS /= 0d0)                                                 &
        WRITE(lfnprt,'(A,F10.3,A)')                                            &
             ' Max. rms allowed for polynomial fit: ',   CCopt%IEmRMS,' ns'
      IF (CCopt%IEmObs /= 0 )                                                  &
        WRITE(lfnprt,'(A,I10)')                                                &
             ' Min. # of data points for poly. fit: ',   CCopt%IEmObs
      WRITE(lfnprt,'(/,1X,A,/,1X,A)')                                          &
      'Model function components            used from               to',       &
      '-----------------------------------------------------------------------'
      DO ii = 1, CCopt%iePoly+1
        epost1 = '         ---       '
        epost2 = '         ---       '
        IF (CCOpt%ieTpoly(ii)%t(1) /= 0d0) &
          CALL timst2(1,1,CCOpt%ieTpoly(ii)%t(1),epost1)
        IF (CCOpt%ieTpoly(ii)%t(2) /= 0d0 .AND. CCOpt%ieTpoly(ii)%t(2) /= 99d99) &
          CALL timst2(1,1,CCOpt%ieTpoly(ii)%t(2),epost2)
        IF (CCOpt%ieTpoly(ii)%t(1) /= 0d0 .OR. CCOpt%ieTpoly(ii)%t(2) /= 0d0) &
          WRITE(lfnprt,'(1X,A,I3,12X,A19,3X,A19)')                            &
          'Polynom degree ',ii-1, epost1, epost2
      ENDDO
      DO ii = 1, CCopt%ieSico
        epost1 = '         ---       '
        epost2 = '         ---       '
        IF (CCOpt%ieTsico(ii)%t(1) /= 0d0) &
          CALL timst2(1,1,CCOpt%ieTsico(ii)%t(1),epost1)
        IF (CCOpt%ieTsico(ii)%t(2) /= 0d0 .AND. CCOpt%ieTsico(ii)%t(2) /= 99d99) &
          CALL timst2(1,1,CCOpt%ieTsico(ii)%t(2),epost2)
        IF (CCOpt%ieTsico(ii)%t(1) /= 0d0 .OR. CCOpt%ieTsico(ii)%t(2) /= 0d0) &
          WRITE(lfnprt,'(1X,A,F5.2,A,8X,A19,3X,A19)') &
          'Periodic ',CCopt%iePsico(ii),' per day', epost1, epost2
      ENDDO
      WRITE(lfnprt,'(1X,A)')                                                   &
      '-----------------------------------------------------------------------'
    ENDIF
  ENDIF

  WRITE(lfnprt,*)

  IF (CCopt%PrtDetail(prtinp) == 1)          &
    CALL pricrh(nFil, CCfil%ClkFilNam)

  DEALLOCATE(keyValue,stat=irc)
  DEALLOCATE(dummy,stat=irc)
  DEALLOCATE(hlpNam1,stat=irc)
  DEALLOCATE(hlpNam2,stat=irc)

! Stop if an error in the input options found
! -------------------------------------------
  IF (irCode /= 0) CALL exitrc(2)

  RETURN
  END SUBROUTINE

END MODULE
