MODULE s_RDCLKN
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE rdclkn(clkopt)
!--------------------------------------------------------------------------
!
! NAME       :  RDCLKN
!
! PURPOSE    :  READ OPTIONS FOR PROGRAM CLKEST
!               (CLOCK ESTIMATION)
!
! PARAMETERS :
!        OUT :  NSAMPL : SAMPLING OF EPOCHS (SEC)              I*4
!               LINCOM : LINEAR COMBINATION FOR PROCESSING     I*4
!               IRFSEL : REFERENCE CLOCK SELECTION             I*$
!                         = 1: FILE NR
!                         = 2: SVN NR
!                         = 3: SUMMATION CONDITION ON SATELLITS
!                         = 4: TAKE STATION WITHOUT BREAKS IN
!                              PHASE FROM LIST
!               IRFCLK : REFERENCE CLOCK                       I*4
!                        FILE NR OR SVN NR
!               IEXTRA : USE TROPOSPHERE MODEL (IEXTRA=1)      I*4
!                        OR BERNESE TROPO-FILE (IEXTRA=2)
!               ITROPO : TROPOSPHERE MODEL                     I*4
!               MINSTA : MINIMUM NUMBER OF STATIONS AT WHICH   I*4
!                        A SATELLITE MUST BE AVAILABLE
!               ZENMAX : MAXIMUM ZENITH DISTANCE               R*8
!               OMCMXC : MAX. SIZE OF TERM ``O-C'' FOR CODE    R*8
!                        CLOCK ESTIMATION
!               APRSIC : A PRIORI SIGMA FOR ZENITH CODE OBSERV.R*8
!               NOCODE : =1: DO NOT USE CODE                   I*4
!               REMOVE : =1: REMOVE NON-FIXED DATA PIECES      I*4
!               RESMXC : MAXIMUM TOLERATED RESIDUAL (CODE)     R*8
!               RMSMXC : MAXIMUM TOLERATED RMS FOR CODE PRC    R*8
!               MAXITC : MAXIMUM NUMBER OF ITERATIONS FOR CODE I*4
!                        OBSERVATIONS
!               OMCMAX : MAX. SIZE OF TERM ``O-C'' FOR PHASE   R*8
!                        CLOCK ESTIMATION
!               APRSIP : A PRIORI SIGMA FOR PHASE DIFF. OBSERV.R*8
!               RESMXP : MAXIMUM TOLERATED RESIDUAL (PHASE)    R*8
!               RMSMXP : MAXIMUM TOLERATED RMS FOR PHASE PRC   R*8
!               MAXITP : MAXIMUM NUMBER OF ITERATIONS FOR CODE I*4
!                        OBSERVATIONS
!               EPOWIN : OBSERVATION WINDOW (MJD)              R*8(2)
!               TITCLK : TITLE OF BERNESE CLOCK FILE           CHR*64
!               CLKWRT : WRITE STATION CLOCKS TO RINEX         I*4
!                         =1: ALL STATION CLOCKS
!                         =2: STATION CLOCKS AT FIX EPOCHS
!                         =3: NO STATION CLOCKS (EXCEPT REFERENCE)
!               CPYRNX : =1: COPY CLOCKS FROM RINEX FILE       I*4
!                            (ONLY FOR IFIX=1)
!               RUNBY  : CLK RNX HEADER, AGENCY RUNNING PGM    CHR*20
!               AC     : CLK RNX HEADER, AGENCY ACRONYM        CHR*3
!               ACNAME : CLK RNX HEADER, AGENCY DESCRIPTION    CHR*55
!               NCOM   : NUMBER OF COMMENT LINES
!               COMENT : CLK RNX HEADER, COMMENT LINES         CHR*60(*)
!
! SR CALLED  :  readKeys, alcerr
!
! REMARKS    :  ---
!
! AUTHOR     :  H.BOCK
!
! VERSION    :  5.0
!
! CREATED    :  03-Apr-2001
! Last mod.  :  10-Jan-2012
!
! CHANGES    :  14-Jul-2001 HU: Read observation window
!               18-Jul-2001 HU: IRFSEL and NOCODE added
!               19-Jul-2001 HU: Read clock file header info
!               28-Jul-2001 HU: Call modified
!               06-Sep-2001 HU: REMOVE added
!               21-Dec-2001 HU: Use d_const
!               18-Jul-2002 HU: Only opt in parameter list, use check routines
!               23-Jul-2002 HU: Read cmbsel
!               10-Aug-2002 HU: Do not read iextra, use gtflna instead
!               30-Aug-2002 HU: Get CMBINFO filename
!               23-Apr-2003 RD: Nullify local pointers
!               19-May-2003 RD: Use SR gttimwin instead of SR readsess
!               27-Jun-2003 HB: Additional radio button for reference clock
!                               selection (reference clock from Clock RINEX)
!               08-Sep-2003 HU: Read new options
!               07-Jun-2004 HB: Small bug fixed (opt%clkwrt == 3 for writing)
!               13-Mar-2006 HB: Bug fixed: troposphere file was not used
!               24-Aug-2006 AG: GMF implemented
!               24-Nov-2006 AG: TIMSYS and DCBLINE implemented,
!                               ACRONYM and AGENCY renamed
!               24-Jun-2007 AG: Call init_stalist for reflist
!               01-Nov-2007 HB: Add parameter secIpl
!               30-Jun-2008 RD: VMF added
!               04-May-2009 RD: change opt%epowin to t_timint
!               19-Jul-2010 SL: tab characters removed
!               30-Nov-2010 DT: Add obstype to call of SR trpopt
!               12-Apr-2011 CR: Switch for Periodic Relativistic J2-Correction
!               06-May-2011 HB: Add rdstdh to initialize model names
!               10-Jan-2012 HB: Typo corrected REFSTA => REFSAT
!
! COPYRIGHT  :  ASTRONOMICAL INSTITUTE
!      2001      UNIVERSITY OF BERN
!                    SWITZERLAND
!
!--------------------------------------------------------------------------

  USE m_bern
  USE d_const,  ONLY: pi
  USE d_stalst, ONLY: t_staList,init_staList
  USE d_stdorb, ONLY: t_stdhead, init_stdHead
  USE p_clkest, ONLY: t_clkopt
  USE s_ckoptr
  USE s_alcerr
  USE s_gttimwin
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  USE s_ckoptc
  USE s_ckopti
  USE s_gtflna
  USE s_ckoptl
  USE s_readstsg
  USE s_trpopt
  USE s_rdstdh
  IMPLICIT NONE

! Dummy arguments
! ---------------
  TYPE(t_clkopt) :: clkopt

! Local variables
! ---------------
  TYPE(t_staList)                :: refList
  TYPE(t_stdhead)                :: stdHead   ! Structure of std header info
  INTEGER(i4b)                   :: iRef,jRef
  INTEGER(i4b)                   :: ios,usecode,irCode=0
  INTEGER(i4b)                   :: itrmap
  INTEGER(i4b), DIMENSION(2)     :: itrgrd
  CHARACTER(LEN=fileNameLength)  :: filnam

! Declaration for readKeys
! ------------------------
  CHARACTER(LEN=keyValueLength),DIMENSION(:),POINTER :: keyValue
  INTEGER(i4b) :: irc

! Init variables
! --------------
  NULLIFY(keyValue)
  CALL init_stdHead(stdHead)
  CALL init_staList(refList)

! GENERAL PROCESSING CHARACTERISTICS
! ----------------------------------
  CALL readKeys('SAMPLING', keyValue, irc)
  CALL ckopti(1,'SAMPLING', keyValue, 'sr rdclkn', 'Sampling rate (sec)', &
              irc, irCode, ge=0, maxVal=1, result1=clkopt%nsampl)

  CALL readKeys('DTSIM', keyValue, irc)
  CALL ckoptr(1,'DTSIM', keyValue, 'sr rdclkn', 'Diff. of equal epochs', &
              irc, irCode, gt=0D0, maxVal=1, result1=clkopt%dtsim)
  clkopt%dtsim=clkopt%dtsim/86400D0

  CALL readKeys('SECIPL', keyValue, irc)
  CALL ckoptr(1,'SECIPL', keyValue, 'sr rdclkn', 'Interpolation interval (sec)', &
              irc, irCode, ge=0.D0, maxVal=1, result1=clkopt%secIpl)

  CALL readKeys('LINCOMB', keyValue, irc)
  CALL ckoptc(1,'LINCOMB', keyValue,(/'L1','L2','L3'/), 'sr rdclkn', &
              'Frequency', irc, irCode, maxVal=1,result1=clkopt%lincom)

  CALL ckoptb(1, (/'RADIO_STA','RADIO_SAT','RADIO_SUM'/),     &
              'sr rdclkn', 'Reference clock for synchronization', irCode, &
              result1 = clkopt%irfsel)

  CALL readKeys('REFSTA', keyValue, irc)
  CALL ckopti(1,'REFSTA', keyValue, 'sr rdclkn', 'Reference station',  &
              irc, irCode, maxVal=1, result1=clkopt%irfclk)
  clkopt%irfclk=ABS(clkopt%irfclk)

  SELECT CASE (clkopt%irfsel)
!   CASE (1)
!     CALL readKeys('REFSTA', keyValue, irc)
!     CALL ckopti(1,'REFSTA', keyValue, 'sr rdclkn', 'Reference station',  &
!                 irc, irCode, maxVal=1, result1=clkopt%irfclk)
!     clkopt%irfclk=ABS(clkopt%irfclk)
    CASE (2)
      CALL readKeys('REFSAT', keyValue, irc)
      CALL ckopti(1,'REFSAT', keyValue, 'sr rdclkn', 'Reference satellite',  &
                  irc, irCode, maxVal=1, result1=clkopt%irfclk)
      clkopt%irfclk=ABS(clkopt%irfclk)
    CASE (3)
      clkopt%irfclk=0
  END SELECT

  CALL readKeys('MINSTAT', keyValue, irc)
  CALL ckopti(1,'MINSTAT', keyValue, 'sr rdclkn','Minimum number of stations',&
              irc, irCode, ge=1, maxVal=1, result1=clkopt%minsta)

! Periodic Relativistic J2-Correction
! -----------------------------------
  CALL ckoptb(1, (/'PRELJ2'/),'sr rdclkn',                   &
              'Periodic Relativistic J2-Correction', irCode, &
              result1 = clkopt%irelJ2)

! Tropospheric Model
! ------------------
  CALL trpopt(1, clkopt%itropo, clkopt%iextra, itrmap, itrgrd)

  IF (clkopt%iextra==2) THEN
    CALL ckoptb(1, (/'CHKTROP'/),'sr rdclkn', 'Check for missint trp values', &
                irCode, result1 = clkopt%chktrp)
  ELSE
    clkopt%chktrp=0
  ENDIF

  CALL readKeys('CMBSEL', keyValue, irc)
  CALL ckoptc(1,'CMBSEL', keyValue,(/'TRIDIAG      ','SUM_CONDITION'/), &
              'sr rdclkn', 'Combination algorithm', irc, irCode, &
              result1 = clkopt%cmbsel)

  IF (clkopt%cmbsel == 1) THEN
    CALL ckoptb(1, (/'IFIX'/),'sr rdclkn', 'Fix clocks to a priori', &
                irCode, result1 = clkopt%ifix)
  ELSE
    clkopt%ifix=0
  ENDIF

  CALL readKeys('DTFIX', keyValue, irc)
  CALL ckoptr(1,'DTFIX', keyValue, 'sr rdclkn','Remove clocks > dt',&
              irc, irCode, maxVal=1, result1=clkopt%dtfix)
  clkopt%dtfix=clkopt%dtfix/1440D0

  CALL ckoptb(1, (/'ALLIGN'/),'sr rdclkn', 'Allign with a priori clocks', &
              irCode, result1 = clkopt%allign)

  CALL readKeys('MAXZEN', keyValue, irc)
  CALL ckoptr(1,'MAXZEN', keyValue, 'sr rdclkn','Maximum zenith distance',&
              irc, irCode, maxVal=1, result1=clkopt%maxzen)
  clkopt%maxzen = clkopt%maxzen*PI/180.D0

! CHARACTERIZATION OF CODE CLOCK PROCESSING
! -----------------------------------------
  CALL readKeys('OMCMXC', keyValue, irc)
  CALL ckoptr(1,'OMCMXC', keyValue, 'sr rdclkn','Maximum O-C for code',&
              irc, irCode, maxVal=1, result1=clkopt%omcmxc)

  CALL ckoptb(1, (/'USECODE'/),'sr rdclkn', 'Use code', &
              irCode, result1 = usecode)
  IF (usecode == 0) THEN
    clkopt%nocode=1
  ELSE
    clkopt%nocode=0
  ENDIF

  IF (clkopt%ifix==1) THEN
    CALL ckoptb(1, (/'REMOVE'/),'sr rdclkn', 'Remove non-fixed data pieces', &
                irCode, result1 = clkopt%remove)
  ELSE
    clkopt%remove=0
  ENDIF

  CALL readKeys('APRSIC', keyValue, irc)
  CALL ckoptr(1,'APRSIC', keyValue, 'sr rdclkn','A priori sigma for code',&
              irc, irCode, maxVal=1, result1=clkopt%aprsic)
  IF (clkopt%aprsic==0D0) THEN
    clkopt%aprsic=1d0
    clkopt%nocode=1
  ENDIF

  CALL readKeys('RESMXC', keyValue, irc)
  CALL ckoptr(1,'RESMXC', keyValue, 'sr rdclkn','Maximum residual for code',&
              irc, irCode, maxVal=1, result1=clkopt%resmxc)

  CALL readKeys('RMSMXC', keyValue, irc)
  CALL ckoptr(1,'RMSMXC', keyValue, 'sr rdclkn','Maximum rms for code',&
              irc, irCode, maxVal=1, result1=clkopt%rmsmxc)

  CALL readKeys('MAXITC', keyValue, irc)
  CALL ckopti(1,'MAXITC', keyValue, 'sr rdclkn','Maximum number of iterations',&
              irc, irCode, ge=1, maxVal=1, result1=clkopt%maxitc)

! CHARACTERIZATION OF PHASE CLOCK PROCESSING
! ------------------------------------------
  CALL readKeys('OMCMAX', keyValue, irc)
  CALL ckoptr(1,'OMCMAX', keyValue, 'sr rdclkn','Maximum O-C for phase',&
              irc, irCode, maxVal=1, result1=clkopt%omcmax)

  CALL readKeys('APRSIP', keyValue, irc)
  CALL ckoptr(1,'APRSIP', keyValue, 'sr rdclkn','A priori sigma for phase',&
              irc, irCode, maxVal=1, result1=clkopt%aprsip)

  CALL readKeys('RESMXP', keyValue, irc)
  CALL ckoptr(1,'RESMXP', keyValue, 'sr rdclkn','Maximum residual for phase',&
              irc, irCode, maxVal=1, result1=clkopt%resmxp)

  CALL readKeys('RMSMXP', keyValue, irc)
  CALL ckoptr(1,'RMSMXP', keyValue, 'sr rdclkn','Maximum rms for phase',&
              irc, irCode, maxVal=1, result1=clkopt%rmsmxp)

  CALL readKeys('MAXITP', keyValue, irc)
  CALL ckopti(1,'MAXITP', keyValue, 'sr rdclkn','Maximum number of iterations',&
              irc, irCode, ge=1, maxVal=1, result1=clkopt%maxitp)

! USE OBSERVATION WINDOW
! ----------------------
  CALL gttimwin('USEWIN',(/'RADIO_1','RADIO_2'/),        &
                (/'SESSION_YEAR','SESSION_STRG'/),       &
                (/'STADAT','STATIM','ENDDAT','ENDTIM'/), &
                clkopt%epowin%t)

! CLOCK FILE HEADER INFO
! ----------------------
  CALL readKeys('TITCLK', keyValue, irc)
  CALL ckoptl(0,'TITCLK', keyValue, 'sr rdclkn', 'Title of rnx clock file', &
              irc, irCode, maxLength=80, maxVal=1, result1=clkopt%titclk)

  CALL readKeys('RUNBY', keyValue, irc)
  CALL ckoptl(0,'RUNBY', keyValue, 'sr rdclkn', 'Run by', &
              irc, irCode, maxLength=20, maxVal=1, result1=clkopt%runby)

  CALL readKeys('AC', keyValue, irc)
  CALL ckoptl(0,'AC', keyValue, 'sr rdclkn', 'Acronym of agency', &
              irc, irCode, maxLength=3, maxVal=1, result1=clkopt%ac)

  CALL readKeys('ACNAME', keyValue, irc)
  CALL ckoptl(0,'ACNAME', keyValue, 'sr rdclkn', 'Agency name', &
              irc, irCode, maxLength=55, maxVal=1, result1=clkopt%acname)

    CALL readkeys('TIMESYS', keyValue, irc)
    IF (irc==0) &
         CALL ckoptl(1,'TIMESYS', keyValue, 'sr rdclkn', 'Time system', irc,   &
              irCode, maxLength=LEN(Clkopt%timsys), maxval=1, result1=Clkopt%timsys)

    CALL readkeys('DCBLINE', keyValue, irc)
    IF (irc==0) &
         CALL ckoptl(0,'DCBLINE', keyValue, 'sr rdclkn', 'DCB line', irc, irCode, &
            maxLength=LEN(Clkopt%dcbStr), empty=' ', maxval=1, result1=Clkopt%dcbStr)

  CALL readKeys('COMMENT', keyValue, irc)
  clkopt%ncom=SIZE(keyValue)
  IF (clkopt%ncom==1 .AND. keyValue(1) == " ") clkopt%ncom=0
  IF (clkopt%ncom > 0) THEN
    ALLOCATE (clkopt%coment(clkopt%ncom),STAT=ios)
    CALL alcerr(ios,'clkopt%coment',(/clkopt%ncom/),'RDCLKN')
    CALL ckoptl(0,'COMMENT', keyValue, 'sr rdclkn', 'Comment lines', &
              irc, irCode, maxLength=60, maxVal=clkopt%ncom, &
              result2=clkopt%coment)
  ENDIF

! WRITE OUTPUT
! ------------
  CALL ckoptb(1, (/'RADIO_ALL','RADIO_FIX','RADIO_NOT','RADIO_SPC'/), &
              'sr rdclkn', 'Write clocks to rinex file', irCode,      &
              result1 = clkopt%clkwrt)

  IF (clkopt%clkwrt == 4) THEN
    CALL readKeys('STALIST', keyValue, irc)
    clkopt%nrxsta=SIZE(keyValue)
    IF (clkopt%nrxsta==1 .AND. keyValue(1) == " ") clkopt%nrxsta=0
    IF (clkopt%nrxsta > 0) THEN
      ALLOCATE (clkopt%rnxsta(clkopt%nrxsta),STAT=ios)
      CALL alcerr(IOS,'clkopt%rnxsta',(/clkopt%nrxsta/),'RDCLKN')
      CALL ckoptl(1, 'STALIST', keyValue, 'sr rdclkn', &
                  'Station list for rinex file', irc, irCode, maxLength=4, &
                  maxVal=clkopt%nrxsta, result2=clkopt%rnxsta)
    ENDIF
  ENDIF

  IF (clkopt%ifix==1) THEN
    CALL ckoptb(1, (/'COPYRNX'/),'sr rdclkn', 'Copy values from RNX file', &
              irCode, result1 = clkopt%cpyrnx)
  ELSE
    clkopt%cpyrnx=0
  ENDIF

  IF (clkopt%cpyrnx==1) THEN
    CALL ckoptb(1, (/'MISSRNX'/),'sr rdclkn', 'Missing clocks in RNX file', &
              irCode, result1 = clkopt%delrnx)
  ELSE
    clkopt%delrnx=0
  ENDIF

! REFERENCE CLOCK
! ---------------
  CALL ckoptb(1, (/'RADIO_RSTA','RADIO_RLST','RADIO_AUTO','RADIO_RXC ','RADIO_RFIL'/),  &
       'sr rdclkn','Reference clock selection', irCode,                    &
       result1 = clkopt%clksel)

  IF (clkopt%clksel==1) THEN
    CALL readKeys('REFCLK', keyValue, irc)
    CALL ckoptl(1,'REFCLK', keyValue, 'sr rdclkn', 'Reference clock', &
                irc, irCode, maxLength=4, maxVal=1, result1=clkopt%refclk)
  ENDIF

  clkopt%nrfsta=0
  IF (clkopt%clksel == 2) THEN
    CALL readKeys('REFLIST', keyValue, irc)
    clkopt%nrfsta=SIZE(keyValue)
    IF (clkopt%nrfsta==1 .AND. keyValue(1) == " ") clkopt%nrfsta=0
    IF (clkopt%nrfsta > 0) THEN
      ALLOCATE (clkopt%refsta(clkopt%nrfsta),STAT=ios)
      CALL alcerr(IOS,'clkopt%refsta',(/clkopt%nrfsta/),'RDCLKN')
      CALL ckoptl(1, 'STALIST', keyValue, 'sr rdclkn', &
                  'List of reference clocks', irc, irCode, maxLength=4, &
                  maxVal=clkopt%nrfsta, result2=clkopt%refsta)
    ENDIF
  ELSEIF (clkopt%clksel == 5) THEN
    clkopt%clksel = 2
    CALL gtflna(1,'REFSIG',filnam,irc)
    IF (irc == 0 .AND. LEN_TRIM(filnam) > 0) THEN
      CALL readstsg(filnam,1,refList)
      IF (refList%nSta > 0) THEN
        ALLOCATE (clkopt%refsta(refList%nSta),STAT=ios)
        CALL alcerr(IOS,'clkopt%refsta',(/refList%nSta/),'RDCLKN')
        clkopt%nrfsta=0
        DO WHILE(clkopt%nrfsta<refList%nSta)
          iRef = 0
          DO jRef = 1,refList%nSta
            IF (refList%staNam(jRef) == ' ') CYCLE
            IF (LEN_TRIM(refList%staNam(jRef)) == 3.AND. &
                (refList%staNam(jRef)(1:1) == 'G' .OR. &
                 refList%staNam(jRef)(1:1) == 'R')) THEN
              READ(refList%staNam(jRef)(2:3),'(i2)',IOSTAT=ios) irc
              IF (ios == 0) THEN
                refList%staNam(jRef) = ' '
                CYCLE
              ENDIF
            ENDIF
            IF (iRef == 0) THEN
              iRef = jRef
            ELSE IF (refList%sigma(1,iRef) > refList%sigma(1,jRef)) THEN
              iRef = jRef
            ENDIF
          ENDDO
          IF (iRef == 0) EXIT
          clkopt%nrfsta=clkopt%nrfsta+1
          clkopt%refsta(clkopt%nrfsta)=refList%staNam(iRef)
          refList%staNam(iRef) = ' '
        ENDDO
        DEALLOCATE(refList%stanam,stat=ios)
        DEALLOCATE(refList%sigma, stat=ios)
      ENDIF
    ENDIF
  ENDIF

! ERROR READING KEYWORD
! ---------------------
  IF (irCode /= 0) THEN
    WRITE(lfnerr,"(/,' *** SR RDCLKN: Error reading keyword'/)")
    CALL exitrc(2)
  ENDIF

  CALL gtflna(0,'CMBINFO', filnam, irc)
  IF (irc == 1) THEN
    clkopt%debug=0
  ELSE
    clkopt%debug=1
  ENDIF

! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=irc)

! WRITE OPTIONS
! -------------
  WRITE(lfnprt,"(' OPTIONS', &
             & /,' -------',//,1X,79('-'))")
  IF (clkopt%nocode==1) THEN
    WRITE(lfnprt,"(' Data for combination        : Use no code, only phase')")
  ELSE
    WRITE(lfnprt,"(' Data for combination        : Use code and phase')")
  ENDIF

  WRITE(lfnprt,"(' Linear combination          :      L',I1)") clkopt%lincom
  WRITE(lfnprt,"(' Sampling of data            :',I8)") clkopt%nsampl
  WRITE(lfnprt,"(' Maximum zenith distance     :',F10.1,' deg')") &
                                                    clkopt%maxzen*180D0/pi
  WRITE(lfnprt,"(' Minimum nbr of stations     :',I8)") clkopt%minsta

  IF (clkopt%iextra==1) THEN
    WRITE(lfnprt,"(' Troposphere                 : ', &
                                   & 'Extrapolate using Saastamoinen model')")
  ELSE
    WRITE(lfnprt,"(' Troposphere                 : Use values from file')")
  ENDIF

  IF (clkopt%irfsel==1) THEN
    WRITE(lfnprt,"(' Synchronization             : File',I6)") clkopt%irfclk
  ELSEIF (clkopt%irfsel==2) THEN
    WRITE(lfnprt,"(' Synchronization             : Satellite',I6)") &
                                                               clkopt%irfclk
  ELSEIF (clkopt%irfsel==3) THEN
    WRITE(lfnprt,"(' Synchronization             : ', &
                                     & 'Summation condition on satellites')")
  ELSEIF (clkopt%irfsel==4) THEN
    WRITE(lfnprt,"(' Synchronization             : Station from list')")
  ENDIF

  WRITE(lfnprt,"(' Code : A priori sigma       :',F12.3)") clkopt%aprsic
  WRITE(lfnprt,"('        Maximum o-c          :',F12.3)") clkopt%omcmxc
  WRITE(lfnprt,"('        Maximum residuum     :',F12.3)") clkopt%resmxc
  WRITE(lfnprt,"('        Maximum rms          :',F12.3)") clkopt%rmsmxc
  WRITE(lfnprt,"('        Maximum iterations   :',I8)")    clkopt%maxitc

  WRITE(lfnprt,"(' Phase: A priori sigma       :',F12.3)") clkopt%aprsip
  WRITE(lfnprt,"('        Maximum o-c          :',F12.3)") clkopt%omcmax
  WRITE(lfnprt,"('        Maximum residuum     :',F12.3)") clkopt%resmxp
  WRITE(lfnprt,"('        Maximum rms          :',F12.3)") clkopt%rmsmxp
  WRITE(lfnprt,"('        Maximum iterations   :',I8)")    clkopt%maxitp

  IF (clkopt%cmbsel==1) THEN
    WRITE(lfnprt,"(' Combination algorithm       : Tridiag')")
  ELSEIF (clkopt%cmbsel==2) THEN
    WRITE(lfnprt,"(' Combination algorithm       : Summation condition')")
  ENDIF
  IF (clkopt%ifix==1) THEN
    WRITE(lfnprt,"(' Fix on input RNX values     : Yes')")
    IF (clkopt%remove==1 .OR. clkopt%nocode==1) THEN
      WRITE(lfnprt,"(' Remove unfixed data pieces  : Yes')")
      WRITE(lfnprt,"(' Remove data away from fix by:',F10.1,' min')") &
                                                     clkopt%dtfix*1440D0
    ENDIF
  ENDIF
  IF (clkopt%allign==1) THEN
    WRITE(lfnprt,"(' Allign clocks to a priori   : Yes')")
  ENDIF

  IF (clkopt%clkwrt==1) THEN
    WRITE(lfnprt,"(' Write station clocks to rnx : All')")
  ELSEIF (clkopt%clkwrt==2) THEN
    WRITE(lfnprt,"(' Write station clocks to rnx : At fixing epochs')")
  ELSEIF (clkopt%clkwrt==3) THEN
    WRITE(lfnprt,"(' Write station clocks to rnx : Only reference')")
  ENDIF

  IF (clkopt%cpyrnx==1) THEN
    WRITE(lfnprt,"(' Copy clocks from input rnx  : Yes')")
  ENDIF

  WRITE(LFNPRT,"(1X,79('-'),//)")

! Get model information from standard orbit file header
! -----------------------------------------------------
  filNam = ' '
  CALL rdstdh(filNam,stdHead,irc)

  RETURN
END SUBROUTINE rdclkn

END MODULE
