MODULE s_RSMTIN
CONTAINS


! -------------------------------------------------------------------------
! Bernese GNSS Software Version 5.2
! -------------------------------------------------------------------------

  SUBROUTINE rsmtin(title ,maxrec,smpint,obswin,gaparc,gapl4 ,rmsl5 ,&
                    rmsl4 ,rmsl3 ,slpmin,slpout,defevt,cycevt,maxevt,&
                    minobs,minl4 ,ifxslp,iflcod,iflphs,iprflg,iplflg,&
                    iepflg,rmston,ic2use,icbest,iglfrq,usegeos,gobsdef)

! -------------------------------------------------------------------------
! Purpose:    Read option input file for program RNXSMT
!
! Author:     M. Meindl
!
! Created:    16-Nov-2000
!
! Changes:    25-Sep-2002 HU: Remove i_astlib
!             19-Mar-2003 RD: Formatted output of the input parameters
!             23-Apr-2003 RD: Nullify local pointers
!             12-Jan-2004 RD: Add observation window
!             19-Jan-2004 RD: Add options for clock events
!             17-Mar-2004 SS: Allow blank field wrt RMSL4
!             29-Apr-2004 RD: Copy phase data if no screening request
!             17-May-2004 RD: Unit-correction after program output
!             28-Sep-2005 RD: Input option for event flag handling
!             20-Nov-2006 RD: Remove observ. if S1&S2-obs. are zero
!             12-Oct-2007 RD: Get MAXREC as an input option
!             29-Jul-2009 SS: Direct estimation of DCB values
!             29-Jul-2009 SS: Use C2 if P2 unavailable
!             26-Jan-2011 LP: Sat-specific obstype selection (GEOS-file)
!             18-May-2011 RD: Add Estimation of GLONASS frequency as an option
!             03-Oct-2011 LP: Direct estimation of P1-P2 DCBs, too
!             29-Dec-2011 RD: Change keyword SMPINT -> SAMPL (unification)
!             25-May-2012 LP: Introduction of RECEIVER OBSSEL file
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_rinex3, ONLY: t_gobsdef

  USE s_ckoptb
  USE s_ckoptc
  USE s_ckoptr
  USE s_readkeys
  USE s_prflna
  USE s_exitrc
  USE s_priwin
  USE s_gttimwin
  USE s_gobsdef,ONLY: init_geos, readgeos
  USE f_lincount
  USE s_gtflna
  IMPLICIT NONE
!
! List of Parameters
! ------------------
  CHARACTER(LEN=shortLineLength) :: title  ! title of processing run
  INTEGER(i4b)                   :: maxrec ! max. number of epochs per file
  REAL(r8b)                      :: smpint ! sampling interval (second)
  REAL(r8b), DIMENSION(2)        :: obswin ! Observation window (MJD)
                                           ! (1:start, 2:end)
  REAL(r8b)                      :: gaparc ! maximum gap length before starting
                                           ! a new arc (hours)
  REAL(r8b)                      :: gapl4  ! maximum gap length in l4. used for
                                           ! cycle slip correction
  REAL(r8b)                      :: rmsl5  ! melwub rms. if value is exceeded
                                           ! there might be a cycle slip
                                           ! (in l5 cycles)
  REAL(r8b)                      :: rmsl4  ! l4 rms. used for cycle slip search
                                           ! (in meters) used as rms and diff.
  REAL(r8b)                      :: rmsl3  ! l3-p3 rms. if value is exceeded
                                           ! there might be a cycle slip
                                           ! (in meters)
  REAL(r8b)                      :: slpmin ! minimal size of detectable cycle
                                           ! slips  (in l5 cycles)
  REAL(r8b)                      :: slpout ! minumal size of detectable outliers
                                           ! (in l5 cycles)
  REAL(r8b)                      :: defevt ! define the a clock event:
                                           ! (P3(n+1)-P3(n))-(L3(n+1)-L3(n))>defevt
                                           ! unit: seconds
  REAL(r8b)                      :: cycevt ! Tolerance for a ms-jump (in sec.)
  INTEGER(i4b)                   :: maxevt ! Maximum number of clock events
                                           ! allowed per file
  INTEGER(i4b)                   :: minobs ! minumum number of observation in
                                           ! an arc
  INTEGER(i4b)                   :: minl4  ! number of l4 observations for fit
  INTEGER(i4b)                   :: ifxslp ! repair cycle slips in code: 0= no
                                           !                             1= yes
                                           ! cycle slips never repaired in phase!
  INTEGER(i4b)                   :: iflcod ! use raw or smooth code : 0= raw
                                           !                          1= smooth
  INTEGER(i4b)                   :: iflphs ! preprocess phase data  : 0= no
                                           !                          1= yes
  INTEGER(i4b)                   :: iprflg ! print flag 0=summary only
                                           !            1=detailed output
  INTEGER(i4b)                   :: iplflg ! print flag 0=melwub and l4 only
  INTEGER(i4b)                   :: iepflg ! What to do in case of event flags
                                           ! 1: Ignore lines, warning
                                           ! 2: Warning, continue with next file
                                           ! 3: Warning, stop with error
  INTEGER(i4b)                   :: rmston ! If S1&S2-obs are zero
                                           ! 1: remove all observations
  INTEGER(i4b)                   :: ic2use ! Use C2 if P2 unavailable
                                           ! =0: No
                                           ! =1: Yes
  INTEGER(i4b)                   :: icbest ! Direct estimation of DCBs
                                           ! =0: No
                                           ! =1: P1-C1,P2-C2
                                           ! =2: P1-P2
  INTEGER(i4b)                   :: usegeos! Use Gal. Ext. Obs. Sel. (GEOS)
                                           ! = 0: No
                                           ! = 1: Yes, from OBSSEL file
                                           !      (depending on
                                           !      receiver, priority, and number
                                           !      of available observations
                                           ! = 2: Yes, from GEOS-file
  TYPE(t_gobsdef)                :: gobsdef! Galileo obstype info
  INTEGER(i4b)                   :: iglfrq ! Estim. GLONASS frequency
                                           ! =0/1: No/Yes


! List of functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER :: srName = 'rsmtin'

  CHARACTER(LEN=16), DIMENSION(7), PARAMETER :: outStr = &
  (/ 'disabled        ','enabled         ', &
     'raw code        ','smooth code     ', &
     'ignore, but warn','skip file       ','stop with error ' /)

! Local variables
! ---------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue
  CHARACTER(LEN=fileNameLength)                        :: geosFil
  CHARACTER(LEN=fileNameLength)                        :: sipFil
  CHARACTER(LEN=fileNameLength)                        :: obsselFil
  INTEGER(i4b)                        :: ios            ! IO status
  INTEGER(i4b)                        :: irc            ! return code
  INTEGER(i4b)                        :: irCode         ! return code
  INTEGER(i4b)                        :: ircgeos        ! return code geos file
  INTEGER(i4b)                        :: ircsip         ! return code sip file
  INTEGER(i4b)                        :: ircobssel      ! return code obssel file
  INTEGER(i4b)                        :: usegeos1       ! ok for use of gobsdef
  INTEGER(i4b)                        :: nLin           ! # of lines in geos file

! Init local variables
! --------------------
  irCode=0

  NULLIFY(keyValue)


! Direct estimation of DCB values
! -------------------------------
  CALL readKeys('ESTDCB',keyValue,irc)

  CALL ckoptc(1,'ESTDCB',keyValue,                          &
              (/'NO   ','P1-C1','P1-P2'/),srName,           &
              'Direct estimation of DCB values',irc,irCode, &
              maxVal=1,valList=(/0,1,2/),result1=icbest)


! If estimation of P1-P2 DCBs: Read TECU values
! ---------------------------------------------
  nLin = 0
  IF (icbest.eq.2) THEN
    CALL gtflna(0,'SIPFILE',sipFil,ircsip)
    IF ((ircsip == 0).AND.(LEN_TRIM(sipFil) > 0)) THEN
      nLin = linCount(sipFil,0)
    ENDIF
    IF (nLin == 0) THEN
      WRITE(lfnerr,'(/,A,/,A,/)')           &
            ' *** SR RSMTIN: Ionosphere info required for ',&
            '                estimation of P1-P2 DCBs.'
      CALL EXITRC(2)
    ENDIF
  ENDIF

! Read the title string
! ---------------------
  title=''
  CALL readkeys('TITLE',keyValue,irc)
  irCode=irCode+irc
  IF (irc == 0) title=keyValue(1)

!
! Maximum Gap Before Starting a New Arc
! -------------------------------------
  CALL readkeys('SAMPL',keyValue,irc)
  READ(keyValue(1),*,iostat=ios) smpint
  IF (ios /= 0) WRITE(LFNERR,'(A)')                       &
    ' *** SR RSMTIN: WRONG ENTRY DETECTED FOR KEYWORD: SMPINT'
  irCode=irCode+irc+ios

!
! Maximum Gap Before Starting a New Arc
! -------------------------------------
  CALL readkeys('MAXREC',keyValue,irc)
  READ(keyValue(1),*,iostat=ios) maxrec
  IF (ios /= 0) WRITE(LFNERR,'(A)')                       &
    ' *** SR RSMTIN: WRONG ENTRY DETECTED FOR KEYWORD: MAXREC'
  irCode=irCode+irc+ios

!
! Observation window
! ------------------
  CALL gttimwin('USEWIN',(/'RADIO_1','RADIO_2'/),        &
                (/'SESSION_YEAR','SESSION_STRG'/),       &
                (/'STADAT','STATIM','ENDDAT','ENDTIM'/), &
                obswin)
!
! Sampling Interval for RINEX Data
! --------------------------------
  CALL readkeys('GAPARC',keyValue,irc)
  READ(keyValue(1),*,iostat=ios) gaparc
  IF (ios /= 0) WRITE(LFNERR,'(A)')                       &
    ' *** SR RSMTIN: WRONG ENTRY DETECTED FOR KEYWORD: GAPARC'
  irCode=irCode+irc+ios

!
! Maximum Gap for Cycle Slip Correction
! -------------------------------------
  CALL readkeys('GAPL4',keyValue,irc)
  READ(keyValue(1),*,iostat=ios) gapl4
  IF (ios /= 0) WRITE(LFNERR,'(A)')                       &
    ' *** SR RSMTIN: WRONG ENTRY DETECTED FOR KEYWORD: GAPL4'
  irCode=irCode+irc+ios

!
! RMS of Melbourne Wubbena
! ------------------------
  CALL readkeys('RMSL5',keyValue,irc)
  READ(keyValue(1),*,iostat=ios) rmsl5
  IF (ios /= 0) WRITE(LFNERR,'(A)')                       &
    ' *** SR RSMTIN: WRONG ENTRY DETECTED FOR KEYWORD: RMSL5'
  irCode=irCode+irc+ios

!
! RMS of L4 for Fit and Cycle Slip Corr.
! --------------------------------------
  CALL readkeys('RMSL4',keyValue,irc)
  CALL ckoptr(1,'RMSL4',keyValue,'RSMTIN','RMS of L4', &
              irc,irCode,empty=0.d0,ge=0.d0,result1=rmsl4)

!
! RMS of L3-P3
! ------------
  CALL readkeys('RMSL3',keyValue,irc)
  READ(keyValue(1),*,iostat=ios) rmsl3
  IF (ios /= 0) WRITE(LFNERR,'(A)')                       &
    ' *** SR RSMTIN: WRONG ENTRY DETECTED FOR KEYWORD: RMSL3'
  irCode=irCode+irc+ios

!
! Size of Detectable Cycle Slips
! ------------------------------
  CALL readkeys('SLPMIN',keyValue,irc)
  READ(keyValue(1),*,iostat=ios) slpmin
  IF (ios /= 0) WRITE(LFNERR,'(A)')                       &
    ' *** SR RSMTIN: WRONG ENTRY DETECTED FOR KEYWORD: SLPMIN'
  irCode=irCode+irc+ios

!
! Size of Detectable Outliers
! --------------------------------
  CALL readkeys('SLPOUT',keyValue,irc)
  READ(keyValue(1),*,iostat=ios) slpout
  IF (ios /= 0) WRITE(LFNERR,'(A)')                       &
    ' *** SR RSMTIN: WRONG ENTRY DETECTED FOR KEYWORD: SLPOUT'
  irCode=irCode+irc+ios

!
! Define a clock event
! --------------------------------
  defevt = 0d0
  CALL readkeys('CLKEVT',keyValue,irc)
  IF (LEN_TRIM(keyValue(1)) > 0) THEN
    READ(keyValue(1),*,iostat=ios) defevt
    IF (ios /= 0) WRITE(LFNERR,'(A)')                       &
      ' *** SR RSMTIN: WRONG ENTRY DETECTED FOR KEYWORD: CLKEVT'
    IF (ios == 0) defevt = defevt*1d-9
  ENDIF
  irCode=irCode+irc+ios

!
! Tolerance for a ms-jump
! --------------------------------
  cycevt = 0d0
  IF (defevt > 0d0) THEN
    CALL readkeys('CYCEVT',keyValue,irc)
    IF (LEN_TRIM(keyValue(1)) > 0) THEN
      READ(keyValue(1),*,iostat=ios) cycevt
      IF (ios /= 0) WRITE(LFNERR,'(A)')                       &
        ' *** SR RSMTIN: WRONG ENTRY DETECTED FOR KEYWORD: CYCEVT'
      IF (ios == 0) cycevt = cycevt*1d-3
    ENDIF
    irCode=irCode+irc+ios
  ENDIF

!
! Tolerance for a ms-jump
! --------------------------------
  maxevt = 0d0
  IF (defevt > 0d0) THEN
    CALL readkeys('MAXEVT',keyValue,irc)
    IF (LEN_TRIM(keyValue(1)) > 0) THEN
      READ(keyValue(1),*,iostat=ios) maxevt
      IF (ios /= 0) WRITE(LFNERR,'(A)')                       &
        ' *** SR RSMTIN: WRONG ENTRY DETECTED FOR KEYWORD: MAXEVT'
    ENDIF
    irCode=irCode+irc+ios
  ENDIF

!
! Minimum Number of Observations in an Arc
! ----------------------------------------
  CALL readkeys('MINOBS',keyValue,irc)
  READ(keyValue(1),*,iostat=ios) minobs
  IF (ios /= 0) WRITE(LFNERR,'(A)')                       &
    ' *** SR RSMTIN: WRONG ENTRY DETECTED FOR KEYWORD: MINOBS'
  irCode=irCode+irc+ios

!
! Number of L4 Observations for Fit
! --------------------------------
  CALL readkeys('MINL4',keyValue,irc)
  READ(keyValue(1),*,iostat=ios) minl4
  IF (ios /= 0) WRITE(LFNERR,'(A)')                       &
    ' *** SR RSMTIN: WRONG ENTRY DETECTED FOR KEYWORD: MINL4'
  irCode=irCode+irc+ios

!
! Fixs Cycle Slips in Code Observations
! -------------------------------------
  CALL readkeys('FIXSLP',keyValue,irc)
  READ(keyValue(1),*,iostat=ios) ifxslp
  IF (ios /= 0 .OR. ifxslp < 0 .OR. ifxslp > 1) THEN
    WRITE(LFNERR,'(A)')                       &
    ' *** SR RSMTIN: WRONG ENTRY DETECTED FOR KEYWORD: FIXSLP'
    ios=1
  ENDIF
  irCode=irCode+irc+ios

!
! Use Smoothed Code (else use raw code)
! -------------------------------------
  CALL readkeys('FLGCOD',keyValue,irc)
  READ(keyValue(1),*,iostat=ios) iflcod
  IF (ios /= 0 .OR. iflcod < 0 .OR. iflcod > 1) THEN
    WRITE(LFNERR,'(A)')                       &
    ' *** SR RSMTIN: WRONG ENTRY DETECTED FOR KEYWORD: FLGCOD'
    ios=1
  ENDIF
  irCode=irCode+irc+ios

!
! Use Smoothed Code (else use raw code)
! -------------------------------------
  CALL readkeys('RMSTON',keyValue,irc)
  READ(keyValue(1),*,iostat=ios) rmston
  IF (ios /= 0 .OR. rmston < 0 .OR. rmston > 1) THEN
    WRITE(LFNERR,'(A)')                       &
    ' *** SR RSMTIN: WRONG ENTRY DETECTED FOR KEYWORD: RMSTON'
    ios=1
  ENDIF
  irCode=irCode+irc+ios

!
! Use C2 if P2 unavailable
! ------------------------
  CALL readkeys('USEC2',keyValue,irc)
  CALL ckoptb(1,(/'USEC2'/),'RSMTIN',            &
              'Use C2 if P2 unavailable',irCode, &
              result1=ic2use)

!
! Flag Bad Phase Observations
! ---------------------------
  CALL readkeys('FLGPHS',keyValue,irc)
  READ(keyValue(1),*,iostat=ios) iflphs
  IF (ios /= 0 .OR. iflphs < 0 .OR. iflphs > 1) THEN
    WRITE(LFNERR,'(A)')                       &
    ' *** SR RSMTIN: WRONG ENTRY DETECTED FOR KEYWORD: FLGPHS'
    ios=1
  ENDIF
  irCode=irCode+irc+ios

! Handling of event flags in the RINEX files
! ------------------------------------------
  iepflg = 0

  CALL readKeys('EPOFLAG',keyValue,irc)

  CALL ckoptc(1,'EPOFLAG',keyValue,                                  &
              (/'WARNING ','SKIP    ','ERROR   '/),srName,           &
              'Handling of event flags in RINEX',irc,irCode,         &
              maxVal=1,valList=(/1,2,3/),result1=iepflg)


! New observation type selection using receiver-specific priority
! ---------------------------------------------------------------
  usegeos = 0
  CALL gtflna(0,'OBSSEL',obsselFil,ircobssel)
  IF ((ircobssel == 0).AND.(LEN_TRIM(obsselFil) > 0)) THEN
    IF (linCount(obsselFil,8) > 0) usegeos=1
  ENDIF


! Read satellite-specific obstype selection file for GIOVE, Galileo, SBAS
! -----------------------------------------------------------------------
  nLin = 0
  CALL gtflna(0,'GEOSFILE',geosFil,ircgeos)
  IF ((ircgeos == 0).AND.(LEN_TRIM(geosFil) > 0)) THEN
    nLin = linCount(geosFil,6)
    IF ((nLin > 0).AND.(icbest.ne.1)) THEN
      CALL init_geos(nLin,gobsdef)
      CALL readgeos(geosFil,gobsdef,usegeos1)
      IF (usegeos1==1) THEN
        usegeos=2
      ELSE
        WRITE(LFNERR,'(/,A,/,A,/,A,/)')                                     &
        ' *** SR RSMTIN: No usable information in satellite-specific ',   &
        '                observation-type selection file found: ',geosFil
        CALL exitrc(2)
      ENDIF
    ENDIF
  ENDIF

! Deactivate obstype selection for P1-C1 and P2-C2 bias estimation
! in preliminary phase
  IF (icbest.eq.1) usegeos = 0

!
! Print Option
! ------------
  CALL readkeys('PRTFLG',keyValue,irc)
  IF (keyValue(1) == 'SUM' ) THEN
    iprflg=0
  ELSE IF (keyValue(1) == 'ALL' ) THEN
    iprflg=1
  ELSE
    WRITE(LFNERR,'(A)')                       &
      ' *** SR RSMTIN: WRONG ENTRY DETECTED FOR KEYWORD: PRTFLG'
    ios=1
  ENDIF
  irCode=irCode+irc+ios

!
! Plot File
! ---------
  CALL readkeys('PLTFLG',keyValue,irc)
  READ(keyValue(1),*,iostat=ios) iplflg
  IF (ios /= 0 .OR. iplflg < 0 .OR. iplflg > 1) THEN
    WRITE(LFNERR,'(A)')                       &
    ' *** SR RSMTIN: WRONG ENTRY DETECTED FOR KEYWORD: PLTFLG'
    ios=1
  ENDIF
  irCode=irCode+irc+ios

! Estimation of GLONASS frequency numbers
! ---------------------------------------
  CALL ckoptb(1,(/'GLOFRQ'/),'RSMTIN',                    &
              'Estimate GLONASS frequency number',irCode, &
              result1=iglfrq)

! Stop if the input file was corrupt
! ----------------------------------
  IF (irCode /= 0) CALL exitrc(2)

! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=irc)
!
  CALL prflna

  WRITE(lfnprt,*)

  IF (icbest > 0) THEN

! Direct estimation of DCB values
! -------------------------------
  WRITE(lfnprt,'(/,2(A,/),/)')       &
  ' DIRECT ESTIMATION OF DIFFERENTIAL CODE BIAS VALUES:',                  &
  ' --------------------------------------------------'
  ELSE

! Standard application of RNXSMT
! ------------------------------
  WRITE(lfnprt,'(/,2(A,/),A,I7,/,A,F8.0,A,/,A,F8.0,A,/,A,I7,/,A,A)')       &
  ' OBSERVATION ARC DEFINITION:',                                          &
  ' --------------------------',                                           &
  '   Maximum number of epochs in the RINEX data ',maxrec,                 &
  '   Sampling interval for RINEX data           ',smpint,' seconds',      &
  '   Maximum gap before starting a new arc      ',gaparc,' seconds',      &
  '   Minimum number of observations in an arc   ',minobs,                 &
  '   Handling of event flags in RINEX files     ',TRIM(outstr(iepflg+4))
  IF (rmston.eq.1) THEN
  WRITE(lfnprt,'(A,A)') &
  '   If S1&S2 observations are both zero        ','skip all observations'
  ELSE
  WRITE(lfnprt,'(A,A)') &
  '   If S1&S2 observations are both zero        ','use all observations'
  ENDIF
  IF (ic2use.eq.1) THEN
  WRITE(lfnprt,'(A,A,/)') &
  '   If P2 unavailable                          ','use C2'
  ELSE
  WRITE(lfnprt,'(A,A,/)') &
  '   If P2 unavailable                          ','do not use C2'
  ENDIF

  WRITE(lfnprt,'(/,2(A,/),2(A,F8.3,A,/),A,I8,/)')                          &
  ' DETECT CLOCK EVENTS:',                                                 &
  ' -------------------',                                                  &
  '   Minimum size of a clock event              ',defevt*1d9,' nanoseconds',&
  '   Tolerance for ms-jump detection            ',cycevt*1d3,' milliseconds',&
  '   Maximum number of events allowed per file  ',maxevt

  WRITE(lfnprt,'(/,2(A,/),3(A,F8.3,A,/))')                                 &
  ' MELBOURNE-WUEBBENA LINEAR COMBINATION: screening, cycle slip detection',&
  ' -------------------------------------',                                &
  '   RMS of Melbourne Wubbena                   ',rmsl5, ' L5 cycle',     &
  '   Size of detectable cycle slips             ',slpmin,' L5 cycles',    &
  '   Size of detectable outliers                ',slpout,' L5 cycles'

  WRITE(lfnprt,'(/,2(A,/),A,F8.0,A,/,A,I8,/,A,F8.3,A,/,A,A,/)')            &
  ' GEOMETRY-FREE LINEAR COMBINATION: cycle slip correction',              &
  ' --------------------------------',                                     &
  '   Maximum gap for cycle slip correction      ',gapl4, ' seconds',      &
  '   Number of L4 observations for fit          ',minl4,                  &
  '   RMS of L4 for fit and cycle slip corr.     ',rmsl4, ' meters',       &
  '   Fix cycle slips in code observations       ',TRIM(outStr(ifxslp+1))

  WRITE(lfnprt,'(/,2(A,/),A,F8.3,A,/)')                                    &
  ' IONOSPHERE-FREE LINEAR COMBINATION: outlier detection',                &
  ' ----------------------------------',                                   &
  '   RMS of L3-P3                               ',rmsl3, ' meters'

  IF (iglfrq == 1) THEN
    WRITE(lfnprt,'(/,3(A,/),/)')                                           &
    ' ADDITIONAL OPTIONS:',                                                &
    ' ------------------',                                                 &
    '   Estimate GLONASS frequency number          yes'
  ENDIF

  WRITE(lfnprt,'(/,2(A,/),2(A,A,/),/)')                                    &
  ' OUTPUT OPTIONS:',                                                      &
  ' --------------',                                                       &
  '   Preprocess phase observations              ',TRIM(outStr(iflphs+1)), &
  '   Code observations in output file           ',TRIM(outStr(iflcod+3))

  CALL priwin(1,obswin)

  gaparc=gaparc/3.6d3
  gapl4 =gapl4 /3.6d3

  ENDIF

  RETURN
END SUBROUTINE rsmtin

END MODULE
