MODULE s_SATMIN
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE satmin(opt)

! -------------------------------------------------------------------------
!  Purpose    :  Read option input file for program "satmrk"
!
!  Author     :  H. Bock
!
!
!  Created    :  17-Sep-2001
!  Last mod.  :  19-Apr-2010
!
!  Changes    :  25-Sep-2001  HB: Add synchronizing observations
!                07-Jan-2002  RD: Increase length of "mrkHlp" (3->6)
!                                 Do not read a string from an empty string
!                14-Feb-2002  RD: "BOTH" switches are not active for sync.
!                                 Deallocate local arrays
!                26-Feb-2002  RD: Use ckopt subroutines
!                29-Jan-2003  RD: Use DELETE instead ELIMINATE
!                06-Mar-2003  MM: New option: re-initialize ambiguities
!                23-Apr-2003  RD: Nullify local pointers
!                21-May-2003  RD: Init time window (/ 0d0,1d20 /)
!                05-Dec-2007  LP: Process also SLR data; use SR OBSFILLST
!                30-Apr-2008  DT: Add keyWord list to OBSFILLST;
!                                 dimension of fileNumber 1->5
!                30-Apr-2008  DT: Change order of files to PZ-CZ-PS-CS-RZ
!                                 init opt/nfil
!                19-Apr-2010  RD: Re-init. ambiguities only for one GNSS
!
!  SR called  :  djul, gtflna, opnerr
!
!  Copyright  :  Astronomical Institute
!                University of Bern
!                Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_global, ONLY: g_strsys
  USE p_satmrk, ONLY: t_satmrk_opt
  USE s_gtfile2
  USE s_ckoptt
  USE s_alcerr
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  USE s_ckoptc
  USE s_ckoptd
  USE s_gtflna
  USE s_ckopti
  USE s_obsfillst
  IMPLICIT NONE

! List of parameters
! ------------------
! input:

! output:
  TYPE(t_satmrk_opt) :: opt

! List of functions
! -----------------

! Local types
! -----------

! Local parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER :: srName = 'satmin'

! Local Variables
! ---------------
  CHARACTER(LEN=fileNameLength),DIMENSION(:,:),POINTER :: dummy
  CHARACTER(LEN=keyValueLength),     &
            DIMENSION(:),   POINTER :: keyValue

  INTEGER(i4b)   :: iTime
  INTEGER(i4b)   :: iac
  INTEGER(i4b)   :: irc, irCode
  INTEGER(i4b)   :: obstype         ! 1: GNSS/2: Range obs
  INTEGER(i4b)   :: nflcol          ! columns of the obsfilelist
  INTEGER(i4b)   :: ii

  INTEGER(i4b),DIMENSION(5) :: filenumber

  REAL(r8b)                         :: hTime


! Init variables
! --------------
  irCode = 0
  nflcol=0
  filenumber(:)=0
  NULLIFY(keyValue)
  NULLIFY(dummy)
  opt%nFil = 0

! Mark or Synchronize Observations
! --------------------------------
  CALL readKeys('WHATTODO', keyValue, irc)

  CALL ckoptc(1,'WHATTODO', keyValue,                                   &
        (/ 'SYNCHRONIZE','MARK_MANUAL','EDIT_FILE  ','INITIALIZE ' /),  &
        srName, 'Desired task',irc, irCode, maxVal=1,result1=opt%syc)

! Observation type
! ----------------
  CALL readKeys('OBSTYPE', keyValue, irc)

  CALL ckoptc(1,'OBSTYPE', keyValue,                                   &
        (/ 'GNSS ','Range' /),  &
        srName, 'Observation type',irc, irCode, maxVal=1,result1=obstype)

  IF ((obstype==2).AND.((opt%syc==1).OR.(opt%syc==4))) THEN
    write(lfnErr,'(A,/,A)')&
         '*** SR SATMIN: Synchronization and ambiguity reinitialization',&
         '               not possible for range data.'
    CALL exitrc(2)
  ENDIF


! Re-initialize ambiguities
! -------------------------
  IF (opt%syc==4 .AND. obstype==1) THEN
    CALL readkeys('RESAMB',keyValue,irc)
    CALL ckoptc(1,'RESAMB',keyValue,                                     &
          (/'L1   ','L2   ','L1&L2','L5   ','ALL  '/),srName,            &
          'Re-initialize ambiguities, frequency',irc,irCode,maxVal=1,    &
          result1=opt%resAmb)

    CALL readkeys('AMBSYS',keyValue,irc)
    CALL ckoptc(1,'AMBSYS',keyValue,g_strsys,srName,                     &
          'Re-initialize ambiguities, system',irc,irCode,maxVal=1,       &
          other=11,result1=opt%ambSys)
    opt%ambSys = opt%ambSys-1


! get list of files
    CALL OBSFILLST('OBSTYPE',(/'GNSS ','Range'/),(/'PZHFIL  ','CZHFIL  ','BOTHZERO'/), &
                   (/'PSHFIL  ','CSHFIL  ','BOTHSING'/),'RZHFIL',opt%syc,2,0,          &
                   nflcol,filenumber,opt%filNam,dummy)

    DO ii=1,5
      opt%nFil = opt%nFil + filenumber(ii)
    ENDDO

    DEALLOCATE(dummy,stat=irc)

    RETURN
  ELSE
    opt%resAmb = -1
  END IF


! Get edit file name
! ------------------
  opt%edtFil=''
  IF (opt%syc == 3) CALL gtflna(1,'EDITFILE',opt%edtFil,irc)

  IF (opt%syc > 1) opt%syc = 0

! Get the list of observation files
! ---------------------------------
  IF (opt%syc == 1 .AND. obstype==1) THEN
    CALL OBSFILLST('OBSTYPE',(/'GNSS ','Range'/),(/'PZHFIL  ','CZHFIL  ','BOTHZERO'/), &
         (/'PSHFIL  ','CSHFIL  ','BOTHSING'/),'RZHFIL',opt%syc,2,0, &
          nflcol,filenumber,opt%filNam,opt%filSyc)
  ELSE
    CALL OBSFILLST('OBSTYPE',(/'GNSS ','Range'/),(/'PZHFIL  ','CZHFIL  ','BOTHZERO'/), &
         (/'PSHFIL  ','CSHFIL  ','BOTHSING'/),'RZHFIL',opt%syc,2,0, &
          nflcol,filenumber,opt%filNam,dummy)
    DEALLOCATE(dummy,stat=irc)
  ENDIF

  DO ii=1,5
    opt%nFil = opt%nFil + filenumber(ii)
  ENDDO

! Read other options for manual
! -----------------------------
  IF (opt%syc /= 1 .AND. LEN_TRIM(opt%edtFil) == 0) THEN

! Read type of change
! -------------------
    CALL readKeys('TYPCHG', keyValue, irc)

    CALL ckoptc(1, 'TYPCHG', keyValue,                                  &
                (/ 'MARK     ','RESET    ','DELETE   ','ELIMINATE' /),  &
                srName,'Type of manual change',irc,irCode,              &
                maxVal=1,valList=(/1,-1,2,2/),result1=opt%mrkTyp)

! Frequency to change
! -------------------
    CALL readKeys('FREQ', keyValue, irc)

    CALL ckoptc(1,'FREQ',keyValue,(/ 'L1   ','L2   ','L1&L2' /), &
                srName,'Frequency for manual change',irc,irCode, &
                maxVal=1,result1=opt%mrkFrq)

! Satellites to change
! --------------------
    CALL readKeys('SATNUM', keyValue, irc)

    ALLOCATE(opt%mrkSat(size(keyValue)),stat=iac)
    CALL alcerr(iac,'opt%mrkSat',(/size(keyValue)/),srName)

    IF (irc == 0 .AND. SIZE(keyValue) == 1 .AND. keyValue(1) == 'ALL') THEN
      opt%mrkSat = 0
    ELSE
      CALL ckopti(1,'SATNUM',keyValue,srName,                   &
                  'Satellite numbers to change',irc,irCode,     &
                  empty=0,ge=1,le=999,maxVal=SIZE(opt%mrkSat),  &
                  result2=opt%mrkSat)
    ENDIF

! Get type of time interval
! -------------------------
    CALL ckoptb(1, (/ 'RADIO_21','RADIO_22' /),srName,          &
                'Time/Epoch window specification',irCode,       &
                result1 = iTime)

! Time interval selected by epochs
! --------------------------------
    IF (iTime == 1) THEN

      opt%mrkTim=0.D0

      CALL readKeys('NUMFROM', keyValue, irc)

      CALL ckopti(1,'NUMFROM',keyValue,srName,                 &
                  'First epoch to change',irc,irCode,          &
                  empty=1,ge=1,maxVal=1,result1=opt%mrkEpo(1))

      CALL readKeys('NUMTO', keyValue, irc)

      CALL ckopti(1,'NUMTO',keyValue,srName,                   &
                  'Last epoch to change',irc,irCode,           &
                  empty=HUGE(i4b),ge=opt%mrkEpo(1),maxVal=1,   &
                  result1=opt%mrkEpo(2))
    ENDIF

! Time interval selected by date
! ------------------------------
    IF (iTime == 2) THEN

      opt%mrkEpo = 0

      CALL readKeys('STADAT', keyValue, irc)

      CALL ckoptd(1,'STADAT',keyValue,srName,                  &
                  'Date for first epoch to change',irc,irCode, &
                  empty=0d0,maxVal=1,result1=opt%mrkTim(1))

      CALL readKeys('ENDDAT', keyValue, irc)

      CALL ckoptd(1,'ENDDAT',keyValue,srName,                  &
                  'Date for last epoch to change',irc,irCode,  &
                  empty=1d20,ge=DNINT(opt%mrkTim(1)),maxVal=1, &
                  result1=opt%mrkTim(2))

      IF (opt%mrkTim(1) /= 0d0) THEN
        CALL readKeys('STATIM', keyValue, irc)

        CALL ckoptt(1,'STATIM',keyValue,srName,                  &
                    'Time for first epoch to change',irc,irCode, &
                    maxVal=1,ge=0d0,result1=htime)

        opt%mrkTim(1) = opt%mrkTim(1) + hTime/24d0
      ENDIF

      IF (opt%mrkTim(2) /= 1d20) THEN
        CALL readKeys('ENDTIM', keyValue, irc)

        CALL ckoptt(1,'ENDTIM',keyValue,srName,                  &
                    'Time for last epoch to change',irc,irCode,  &
                    ge=(opt%mrkTim(1)-DNINT(opt%mrkTim(2)))*24d0,&
                    maxVal=1,result1=htime)

        opt%mrkTim(2) = opt%mrkTim(2) + hTime/24d0
      ENDIF
    ENDIF
  ENDIF ! Read options for manual changes

! An error in input options
! -------------------------
  IF (irCode /= 0) CALL exitrc(2)

! Init some settings (comming from edit file)
! -------------------------------------------
  opt%minAmb = 0
  opt%iSampl = 0

! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=irc)

  RETURN
END SUBROUTINE satmin

END MODULE
