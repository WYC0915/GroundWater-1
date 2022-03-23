MODULE s_EEINPT
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE eeinpt(maxTyp,titnew,modnam,npar,parnam,partim,parsig,sigma0,   &
                  wgttyp,iustyp,iussig,sumper,nwin  ,wintim,winSig,isampl, &
                  iadepo,nFilIn,iData,filErp)

! -------------------------------------------------------------------------
! Purpose:   Read Input File for Program ERPEST (new F90-version of EEINPT)
!
! Author:    H.Bock
!
! Created:   24-oct-2001
!
! Changes:   28-May-2003 PS: Removed LFNUM.inc
!                            Ignore Header of parameter list (4 lines)
!            30-May-2003 PS: Initialize winSig, not implemented yet!!!!
!            25-Jul-2003 MR: Add relative Sagnac frequency
!            13-Jan-2009 RD: Use '' as EOF-identifier in NEXTLINE
!            12-May-2009 PS/SL: Correct close statement
!            02-Jun-2009 PS/SL: lfn001 => lfnloc
!            21-Sep-2010 RD: ST2TIM can be used as a module now
!            27-Apr-2012 RD: Nullify all pointers, use m_bern with only
!
! Copyright: Astronomical Institute
!            University of Bern
!            Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnerr, lfnloc, &
                      fileNamelength, keyValueLength, lineLength
  USE d_const,  ONLY: PI
!
  USE s_ckoptr
  USE s_alcerr
  USE s_opnfil
  USE f_nextline
  USE s_opnerr
  USE s_gttimwin
  USE s_readkeys
  USE s_st2tim
  USE s_exitrc
  USE s_ckoptb
  USE s_ckoptc
  USE s_gtflna
  USE s_ckopti
  USE s_ckoptl
  IMPLICIT NONE

! List of Parameters
! ------------------
! IN:
  INTEGER(i4b)                   :: maxTyp ! Maximum Number of Types

! OUT:
  CHARACTER(LEN=80)              :: titNew ! Title
  CHARACTER(LEN=16)              :: modNam ! Name of Resulting Model
  INTEGER(i4b)                   :: nPar   ! Total Number of Parameters
  CHARACTER(LEN=20),DIMENSION(:),POINTER     :: parNam ! Parameter Names
  REAL(r8b),DIMENSION(:,:),POINTER           :: parTim ! Parameter Windows from to in MJD
  REAL(r8b),DIMENSION(:),POINTER             :: parSig ! A Priori Sigmas for Parameters
  REAL(r8b)                      :: sigma0 ! A Priori Sigma (mas)
  REAL(r8b),DIMENSION(2,*)       :: wgtTyp ! Weights of Individual Observation Types
  INTEGER(i4b),DIMENSION(2,*)    :: iusTyp ! Flag Whether to use a Specific
                                           ! Observation Type
  INTEGER(i4b)                   :: iusSig ! Use ERP sigmas in Estimation
  REAL(r8b)                      :: sumPer ! Maximum Period for Statistics
  INTEGER(i4b)                   :: nWin   ! Number of Data Windows
  REAL(r8b),DIMENSION(2,*)       :: winTim ! Data Windows from to in MJD
  REAL(r8b),DIMENSION(*)         :: winSig ! Sigma Factor for Each Window
  INTEGER(i4b),DIMENSION(3)      :: iSampl ! Sampling of the Data
  INTEGER(i4b)                   :: iadEpo ! Adjust Observation Epochs to the
                                           ! Nearest Full "iadEpo" Minutes
  INTEGER(i4b)                   :: nFilIn ! Number of Input Files
  INTEGER(i4b),DIMENSION(*)      :: iData  ! Data Type of Input Data
  CHARACTER(LEN=fileNameLength),DIMENSION(*) :: filErp ! File Names of Input Files

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength),DIMENSION(:),POINTER :: keyValue
  CHARACTER(LEN=45)                                  :: srname
  CHARACTER(LEN=fileNameLength)                      :: filEst
  CHARACTER(LEN=fileNameLength)                      :: filEr1
  CHARACTER(LEN=40)                                  :: tStrng
  CHARACTER(LEN=20)                                  :: parna1
  CHARACTER(LEN=lineLength)                          :: line

  REAL(r8b),DIMENSION(2) :: parti1
  REAL(r8b) :: parsi1
  REAL(r8b) :: peria
  REAL(r8b) :: perib
  REAL(r8b) :: peri0
  REAL(r8b) :: peri1
  REAL(r8b) :: peri2
  REAL(r8b) :: omega0
  REAL(r8b) :: period
  REAL(r8b) :: dtIntr

  INTEGER(i4b) :: irCode
  INTEGER(i4b) :: irc
  INTEGER(i4b) :: iac
  INTEGER(i4b) :: ioStat
  INTEGER(i4b) :: iEpo
  INTEGER(i4b) :: nEpo
  INTEGER(i4b) :: iFrq
  INTEGER(i4b) :: iPar
  INTEGER(i4b) :: ircEr1
  INTEGER(i4b) :: ircEr2
  INTEGER(i4b) :: ircEr3
  INTEGER(i4b) :: ircEr4
  INTEGER(i4b) :: ircEr5
  INTEGER(i4b) :: ircEr6
  INTEGER(i4b) :: ircEr7
  INTEGER(i4b) :: ircEr8
  INTEGER(i4b) :: i

  LOGICAL :: hlp

! Initialization
! --------------
  irCode = 0
  srName = 'EEINPT'
  NULLIFY(keyValue)

! Read input file names
! ---------------------

! GET DATA INPUT FILE NAME OR FILE NAMES, SET DATA TYPE
! -----------------------------------------------------
  nFilIn=0
  CALL GTFLNA(0,'NUTDAT ',filEr1,ircEr1)
  IF (ircEr1 == 0) THEN
    nFilIn=nFilIn+1
    iData(nFilIn)=1
    filErp(nFilIn)=filEr1
  ENDIF
  CALL GTFLNA(0,'RESDAT ',filEr1,ircEr2)
  IF (ircEr2 == 0) THEN
    nFilIn=nFilIn+1
    iData(nFilIn)=2
    filErp(nFilIn)=filEr1
  ENDIF
  CALL GTFLNA(0,'000DAT ',filEr1,ircEr3)
  IF (ircEr3 == 0) THEN
    nFilIn=nFilIn+1
    iData(nFilIn)=3
    filErp(nFilIn)=filEr1
  ENDIF
  CALL GTFLNA(0,'ERPDAT ',filEr1,ircEr4)
  IF (ircEr4 == 0) THEN
    nFilIn=nFilIn+1
    iData(nFilIn)=4
    filErp(nFilIn)=filEr1
  ENDIF
  CALL GTFLNA(0,'SUBDAT ',filEr1,ircEr5)
  IF (ircEr5 == 0) THEN
    nFilIn=nFilIn+1
    iData(nFilIn)=5
    filErp(nFilIn)=filEr1
  ENDIF
  CALL GTFLNA(0,'VLBIDAT ',filEr1,ircEr6)
  IF (ircEr6 == 0) THEN
    nFilIn=nFilIn+1
    iData(nFilIn)=6
    filErp(nFilIn)=filEr1
  ENDIF
  CALL GTFLNA(0,'G1_DAT  ',filEr1,ircEr7)
  IF (ircEr7 == 0) THEN
    nFilIn=nFilIn+1
    iData(nFilIn)=7
    filErp(nFilIn)=filEr1
  ENDIF
  CALL GTFLNA(0,'UG_DAT  ',filEr1,ircEr8)
  IF (ircEr8 == 0) THEN
    nFilIn=nFilIn+1
    iData(nFilIn)=8
    filErp(nFilIn)=filEr1
  ENDIF

  IF (nFilIn == 0) THEN
    nFilIn=1
    iData(nFilIn)=0
  ENDIF

! Read Title
! ----------
  CALL readKeys('TITNEW',keyValue,irc)
  CALL ckoptl(0,'TITNEW',keyValue,srName,                               &
              'Title Line.',irc,irCode,empty=' ',                       &
              maxLength=80,maxVal=1,result1=titNew)

! Read Model Name
! ---------------
  CALL readKeys('MODNAM',keyValue,irc)
  CALL ckoptl(0,'MODNAM',keyValue,srName,                               &
              'Name of Resulting Model.',irc,irCode,empty=' ',          &
              maxLength=16,maxVal=1,result1=modNam)

! Read A Priori Sigma
! -------------------
  CALL readKeys('SIGMA0', keyValue, irc)
  CALL ckoptr(1,'SIGMA0',keyValue,srName,                               &
              'A Priori Sigma of Observation.',irc,irCode,              &
              maxVal=1,error=0.D0,result1=sigma0)

! Activation of Observations and Relative Weights
! -----------------------------------------------
  iusTyp(1:2,1:maxTyp)=0
  wgtTyp(1:2,1:maxTyp)=0.D0
  CALL ckoptb(1,(/'XPOLG'/),srName,'Activation of Obs.: x-Pole.',       &
       irCode,resultL=hlp)
  IF (hlp) THEN
    iusTyp(1,1)=1
    CALL readKeys('XPOLWG', keyValue, irc)
    CALL ckoptr(1,'XPOLWG',keyValue,srName,                             &
         'Relative Weight: x-Pole.',irc,irCode,                         &
         maxVal=1,error=0.D0,result1=wgtTyp(1,1))
  ENDIF

  CALL ckoptb(1,(/'YPOLG'/),srName,'Activation of Obs.: y-Pole.',       &
       irCode,resultL=hlp)
  IF (hlp) THEN
    iusTyp(1,2)=1
    CALL readKeys('YPOLWG', keyValue, irc)
    CALL ckoptr(1,'YPOLWG',keyValue,srName,                             &
         'Relative Weight: y-Pole.',irc,irCode,                         &
         maxVal=1,error=0.D0,result1=wgtTyp(1,2))
  ENDIF

  CALL ckoptb(1,(/'UT1CG'/),srName,'Activation of Obs.: UT1-UTC.',      &
       irCode,resultL=hlp)
  IF (hlp) THEN
    iusTyp(1,3)=1
    CALL readKeys('UT1CWG', keyValue, irc)
    CALL ckoptr(1,'UT1CWG',keyValue,srName,                             &
         'Relative Weight: UT1-UTC.',irc,irCode,                        &
         maxVal=1,error=0.D0,result1=wgtTyp(1,3))
  ENDIF

  CALL ckoptb(1,(/'DEPSG'/),srName,'Activation of Obs.: deps.',         &
       irCode,resultL=hlp)
  IF (hlp) THEN
    iusTyp(1,4)=1
    CALL readKeys('DEPSWG', keyValue, irc)
    CALL ckoptr(1,'DEPSWG',keyValue,srName,                             &
         'Relative Weight: deps.',irc,irCode,                           &
         maxVal=1,error=0.D0,result1=wgtTyp(1,4))
  ENDIF

  CALL ckoptb(1,(/'DPSIG'/),srName,'Activation of Obs.: dpsi.',         &
       irCode,resultL=hlp)
  IF (hlp) THEN
    iusTyp(1,5)=1
    CALL readKeys('DPSIWG', keyValue, irc)
    CALL ckoptr(1,'DPSIWG',keyValue,srName,                             &
         'Relative Weight: dpsi.',irc,irCode,                           &
         maxVal=1,error=0.D0,result1=wgtTyp(1,5))
  ENDIF

  CALL ckoptb(1,(/'XPOLRG'/),srName,'Activation of Obs.: x-Pole Rate.', &
       irCode,resultL=hlp)
  IF (hlp) THEN
    iusTyp(1,6)=1
    CALL readKeys('XPOLRWG', keyValue, irc)
    CALL ckoptr(1,'XPOLRWG',keyValue,srName,                            &
         'Relative Weight: x-Pole Rate.',irc,irCode,                    &
         maxVal=1,error=0.D0,result1=wgtTyp(1,6))
  ENDIF

  CALL ckoptb(1,(/'YPOLRG'/),srName,'Activation of Obs.: y-Pole Rate.', &
       irCode,resultL=hlp)
  IF (hlp) THEN
    iusTyp(1,7)=1
    CALL readKeys('YPOLRWG', keyValue, irc)
    CALL ckoptr(1,'YPOLRWG',keyValue,srName,                            &
         'Relative Weight: y-Pole Rate.',irc,irCode,                    &
         maxVal=1,error=0.D0,result1=wgtTyp(1,7))
  ENDIF

  CALL ckoptb(1,(/'UT1CRG'/),srName,'Activation of Obs.: UT1-UTC Rate.',&
       irCode,resultL=hlp)
  IF (hlp) THEN
    iusTyp(1,8)=1
    CALL readKeys('UT1CRWG', keyValue, irc)
    CALL ckoptr(1,'UT1CRWG',keyValue,srName,                            &
         'Relative Weight: UT1-UTC Rate.',irc,irCode,                   &
         maxVal=1,error=0.D0,result1=wgtTyp(1,8))
  ENDIF

  CALL ckoptb(1,(/'DEPSRG'/),srName,'Activation of Obs.: deps Rate.',   &
       irCode,resultL=hlp)
  IF (hlp) THEN
    iusTyp(1,9)=1
    CALL readKeys('DEPSRWG', keyValue, irc)
    CALL ckoptr(1,'DEPSRWG',keyValue,srName,                            &
         'Relative Weight: deps Rate.',irc,irCode,                      &
         maxVal=1,error=0.D0,result1=wgtTyp(1,9))
  ENDIF

  CALL ckoptb(1,(/'DPSIRG'/),srName,'Activation of Obs.: dpsi Rate.',   &
       irCode,resultL=hlp)
  IF (hlp) THEN
    iusTyp(1,10)=1
    CALL readKeys('DPSIRWG', keyValue, irc)
    CALL ckoptr(1,'DPSIRWG',keyValue,srName,                            &
         'Relative Weight: dpsi Rate.',irc,irCode,                      &
         maxVal=1,error=0.D0,result1=wgtTyp(1,10))
  ENDIF

  CALL ckoptb(1,(/'SAGNFG'/),srName,'Activation of Obs.: rel.Sagnac',   &
       irCode,resultL=hlp)
  IF (hlp) THEN
    iusTyp(1,11)=1
    CALL readKeys('SAGNFWG', keyValue, irc)
    CALL ckoptr(1,'SAGNFWG',keyValue,srName,                            &
         'Relative Weight: rel.Sagnac',irc,irCode,                      &
         maxVal=1,error=0.D0,result1=wgtTyp(1,11))
  ENDIF

! Read VLBI-options, if File is available
! ---------------------------------------
  IF (ircEr6==0) THEN
    CALL ckoptb(1,(/'XPOLV'/),srName,'Activation of Obs.: x-Pole.',       &
         irCode,resultL=hlp)
    IF (hlp) THEN
      iusTyp(2,1)=1
      CALL readKeys('XPOLWV', keyValue, irc)
      CALL ckoptr(1,'XPOLWV',keyValue,srName,                             &
           'Relative Weight: x-Pole.',irc,irCode,                         &
           maxVal=1,error=0.D0,result1=wgtTyp(2,1))
    ENDIF

    CALL ckoptb(1,(/'YPOLV'/),srName,'Activation of Obs.: y-Pole.',       &
         irCode,resultL=hlp)
    IF (hlp) THEN
      iusTyp(2,2)=1
      CALL readKeys('YPOLWV', keyValue, irc)
      CALL ckoptr(1,'YPOLWV',keyValue,srName,                             &
           'Relative Weight: y-Pole.',irc,irCode,                         &
           maxVal=1,error=0.D0,result1=wgtTyp(2,2))
    ENDIF

    CALL ckoptb(1,(/'UT1CV'/),srName,'Activation of Obs.: UT1-UTC.',      &
         irCode,resultL=hlp)
    IF (hlp) THEN
      iusTyp(2,3)=1
      CALL readKeys('UT1CWV', keyValue, irc)
      CALL ckoptr(1,'UT1CWV',keyValue,srName,                             &
           'Relative Weight: UT1-UTC.',irc,irCode,                        &
           maxVal=1,error=0.D0,result1=wgtTyp(2,3))
    ENDIF

    CALL ckoptb(1,(/'DEPSV'/),srName,'Activation of Obs.: deps.',         &
         irCode,resultL=hlp)
    IF (hlp) THEN
      iusTyp(2,4)=1
      CALL readKeys('DEPSWV', keyValue, irc)
      CALL ckoptr(1,'DEPSWV',keyValue,srName,                             &
           'Relative Weight: deps.',irc,irCode,                           &
           maxVal=1,error=0.D0,result1=wgtTyp(2,4))
    ENDIF

    CALL ckoptb(1,(/'DPSIV'/),srName,'Activation of Obs.: dpsi.',         &
         irCode,resultL=hlp)
    IF (hlp) THEN
      iusTyp(2,5)=1
      CALL readKeys('DPSIWV', keyValue, irc)
      CALL ckoptr(1,'DPSIWV',keyValue,srName,                             &
           'Relative Weight: dpsi.',irc,irCode,                           &
           maxVal=1,error=0.D0,result1=wgtTyp(2,5))
    ENDIF

    CALL ckoptb(1,(/'XPOLRV'/),srName,'Activation of Obs.: x-Pole Rate.', &
         irCode,resultL=hlp)
    IF (hlp) THEN
      iusTyp(2,6)=1
      CALL readKeys('XPOLRWV', keyValue, irc)
      CALL ckoptr(1,'XPOLRWV',keyValue,srName,                            &
           'Relative Weight: x-Pole Rate.',irc,irCode,                    &
           maxVal=1,error=0.D0,result1=wgtTyp(2,6))
    ENDIF

    CALL ckoptb(1,(/'YPOLRV'/),srName,'Activation of Obs.: y-Pole Rate.', &
         irCode,resultL=hlp)
    IF (hlp) THEN
      iusTyp(2,7)=1
      CALL readKeys('YPOLRWV', keyValue, irc)
      CALL ckoptr(1,'YPOLRWV',keyValue,srName,                            &
           'Relative Weight: y-Pole Rate.',irc,irCode,                    &
           maxVal=1,error=0.D0,result1=wgtTyp(2,7))
    ENDIF

    CALL ckoptb(1,(/'UT1CRV'/),srName,'Activation of Obs.: UT1-UTC Rate.',&
         irCode,resultL=hlp)
    IF (hlp) THEN
      iusTyp(2,8)=1
      CALL readKeys('UT1CRWV', keyValue, irc)
      CALL ckoptr(1,'UT1CRWV',keyValue,srName,                            &
           'Relative Weight: UT1-UTC Rate.',irc,irCode,                   &
           maxVal=1,error=0.D0,result1=wgtTyp(2,8))
    ENDIF

    CALL ckoptb(1,(/'DEPSRV'/),srName,'Activation of Obs.: deps Rate.',   &
         irCode,resultL=hlp)
    IF (hlp) THEN
      iusTyp(2,9)=1
      CALL readKeys('DEPSRWV', keyValue, irc)
      CALL ckoptr(1,'DEPSRWV',keyValue,srName,                            &
           'Relative Weight: deps Rate.',irc,irCode,                      &
           maxVal=1,error=0.D0,result1=wgtTyp(2,9))
    ENDIF

    CALL ckoptb(1,(/'DPSIRV'/),srName,'Activation of Obs.: dpsi Rate.',   &
         irCode,resultL=hlp)
    IF (hlp) THEN
      iusTyp(2,10)=1
      CALL readKeys('DPSIRWV', keyValue, irc)
      CALL ckoptr(1,'DPSIRWV',keyValue,srName,                            &
           'Relative Weight: dpsi Rate.',irc,irCode,                      &
           maxVal=1,error=0.D0,result1=wgtTyp(2,10))
    ENDIF

    iusTyp(2,11)=0
    wgtTyp(2,11)=0.D0

  ENDIF

! Read use of Sigmas
! ------------------
  CALL readKeys('IUSSIG', keyValue, irc)
  CALL ckoptc(1,'IUSSIG',keyValue,                                      &
       (/ 'NO ','YES' /),srName,'Use A Priori Sigmas',                  &
       irc,irCode,maxVal=1,valList=(/0,1/),result1=iusSig)

! Read Sampling Options
! ---------------------
  CALL readKeys('ISAMPL', keyValue, irc)
  CALL ckopti(1,'ISAMPL',keyValue,srName,                               &
       'Sampling of the Input Data.',irc,irCode,                        &
       maxVal=1,empty=1,error=1,result1=iSampl(1))

  CALL readKeys('ISTART', keyValue, irc)
  CALL ckopti(1,'ISTART',keyValue,srName,                               &
       'Start With Input Data Record Number.',irc,irCode,               &
       maxVal=1,empty=1,error=1,result1=iSampl(2))

  CALL readKeys('SPLSIM', keyValue, irc)
  CALL ckopti(1,'SPLSIM',keyValue,srName,                               &
       'Sampling Interval for Simulation Data.',irc,irCode,             &
       maxVal=1,empty=1,error=1,result1=iSampl(3))

! Read Adjustment of Epochs
! -------------------------
  CALL readKeys('IADEPO', keyValue, irc)
  CALL ckopti(1,'IADEPO',keyValue,srName,                               &
       'Adustement of Epochs.',irc,irCode,                              &
       maxVal=1,error=1,result1=iadEpo)

! Read Periods to be Sumed
! ------------------------
  CALL readKeys('SUMPER', keyValue, irc)
  CALL ckoptr(1,'SUMPER',keyValue,srName,                               &
              'Sum up Differences up to Periods of n Days',irc,irCode,  &
              maxVal=1,error=0.D0,result1=sumPer)

! Time Window
! -----------
! Read infomation for the time window
! -----------------------------------
  CALL gtTimWin(' ',(/'RADIO_1','RADIO_2'/),                 &
                (/'SESSION_YEAR','SESSION_STRG'/),           &
                (/'STADAT', 'STATIM', 'ENDDAT', 'ENDTIM' /),winTim(:,1))
  nWin = 1

! Read Parameters to be Estimated
! ------------------------------
  CALL gtflna(1,'FILEST',filEst,irc)

!!!!!!!!! nPar = linCount(filEst,4)
  nPar=1000

! Allocate Memory
! ---------------
  ALLOCATE(parNam(nPar),stat=iac)
  CALL alcerr(iac,'parNam',(/nPar/),'eeinpt')
  ALLOCATE(parTim(2,nPar),stat=iac)
  CALL alcerr(iac,'parTim',(/2,nPar/),'eeinpt')
  ALLOCATE(parSig(nPar),stat=iac)
  CALL alcerr(iac,'parSig',(/nPar/),'eeinpt')

! Open File
! ---------
  CALL opnFil(lfnloc,filEst,'OLD','FORMATTED',&
       'READONLY',' ',ioStat)
  CALL opnErr(lfnErr,lfnloc,ioStat,filEst,'EEINPT')

! Ignore Header
! -------------
  header: DO i=1,4
    line = nextLine(lfnloc,0)
  END DO header


  iPar = 0
  LineLoop: DO
    line = nextLine(lfnloc,0)
    IF (line=='') EXIT LineLoop
    READ(line,'(7X,A20,2X,A40,F8.4)') PARNA1,TSTRNG,PARSI1
    IF (PARNA1 == ' ') EXIT LineLoop

! EXPANSION OF FOURIER PARAMETER INFORMATION
! ------------------------------------------
    IF (PARNA1(4:4) == 'F') THEN
      READ(PARNA1(5:20),'(F6.4,2F5.2)') PERI0,PERI1,PERI2
      OMEGA0=2*PI/PERI0
      PERIA=DMIN1(PERI1,PERI2)
      PERIB=DMAX1(PERI1,PERI2)
      CALL ST2TIM(1,2,TSTRNG,PARTI1)
      DO IFRQ=1,100000
        PERIOD=2*PI/(IFRQ*OMEGA0)

        IF (PERIOD >= PERIA .AND. PERIOD <= PERIB) THEN
          IPAR=IPAR+1
          WRITE(PARNAM(IPAR),'(A3,A3,F14.5)')&
               PARNA1(1:3),'SIN',PERIOD
          PARSIG(IPAR)=PARSI1
          PARTIM(1,IPAR)=PARTI1(1)
          PARTIM(2,IPAR)=PARTI1(2)

          IPAR=IPAR+1
          WRITE(PARNAM(IPAR),'(A3,A3,F14.5)')&
               PARNA1(1:3),'COS',PERIOD
          PARSIG(IPAR)=PARSI1
          PARTIM(1,IPAR)=PARTI1(1)
          PARTIM(2,IPAR)=PARTI1(2)
        ENDIF
      ENDDO
    ELSEIF (PARNA1(4:9) == 'LINEAR') THEN
      READ(PARNA1(10:20),'(F11.3)') DTINTR
      DTINTR=DTINTR/24.D0
      IF (TSTRNG == ' ') THEN
        WRITE(LFNERR,'(/,A,A,/,16X,A,A,/)')&
             ' *** SR EEINPT: NO PARAMETER TIME WINDOW GIVEN',&
             ' FOR "LINEAR" PARAMETER','PARAMETER NAME: ',PARNA1
        CALL exitrc(2)
      ENDIF

      CALL ST2TIM(1,2,TSTRNG,PARTI1)
      NEPO=IDNINT((PARTI1(2)-PARTI1(1))/DTINTR)+1
!      write(lfnprt,*)'NEPO',NEPO
!      write(lfnprt,*)'DTINTR,PARTI1',DTINTR,PARTI1(1),PARTI1(2)
      DO IEPO=1,NEPO
        IPAR=IPAR+1
        WRITE(PARNAM(IPAR),'(A9)') PARNA1(1:9)
        PARSIG(IPAR)=PARSI1
        PARTIM(1,IPAR)=PARTI1(1)+(IEPO-2)*DTINTR
        PARTIM(2,IPAR)=PARTIM(1,IPAR)+2*DTINTR
!        write(lfnprt,*)'IEPO,IPAR,PARNAM',IEPO,IPAR,PARNAM(IPAR)
!        write(lfnprt,*)'PARTIM',PARTIM(1,IPAR),PARTIM(2,IPAR)
      ENDDO
    ELSE
      IPAR=IPAR+1
      PARNAM(IPAR)=PARNA1
      PARSIG(IPAR)=PARSI1
      CALL ST2TIM(1,2,TSTRNG,PARTIM(1:2,IPAR))
    ENDIF
  ENDDO LineLoop
!
  nPar=iPar

! Data Windows, not implemented yet !!!!!!!!!!!!!!!!!!!!!!!!!!!!
! at the moment you can only use one window
  winSig(1)=1.d0


! Close File
! ----------------
  CLOSE(UNIT=lfnloc)

! Stop if an error in the input options found
! -------------------------------------------
  IF (irCode /= 0) CALL exitrc(2)

  RETURN
END SUBROUTINE eeinpt

END MODULE
