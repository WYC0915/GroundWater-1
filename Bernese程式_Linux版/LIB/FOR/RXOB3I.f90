MODULE s_RXOB3I
CONTAINS

! -------------------------------------------------------------------------
! Bernese GNSS Software Version 5.2
! -------------------------------------------------------------------------

SUBROUTINE rxob3i(campgn ,title  ,isasys ,rnxSta ,iabupd ,session, &
                  isintv ,isoffs ,minsig ,iacpt0 ,icsflg ,nol2c  , &
                  staInfo,flgStr, rnxInfo,erropt,usegeos,gobsdef,iType)

! -------------------------------------------------------------------------
!
! Purpose:    This is a new version of the old subroutine RXOB3I.f that
!             reads the input options of the program RXOBV3
!
! Author:     L. Mervart
!
! Created:    11-Apr-2000
!
! Changes:    06-Aug-2000 HU: iacpt0 added as parameter
!             14-Dec-2000 RD: read the full title
!             14-Dec-2000 RD: add campaign
!             18-Dec-2000 RD: switch: old/new translation tables
!             18-Jun-2001 HB: add interface for sr readCrux
!             26-Jun-2001 RD: Use alcerr for allocation
!             23-Oct-2001 RD: use the new tables only
!             21-Dec-2001 HU: Use m_bern, other modules with ONLY
!             19-Jul-2002 SS: Satellite system selection
!             19-Mar-2003 RD: Update abbreviation table
!             20-Mar-2003 RD: Minumum number of epoch request
!             23-Apr-2003 RD: Nullify local pointers
!             24-Apr-2003 RD: General RXOBV3 update
!             05-Jun-2003 RD: Format cosmetics
!             24-Jun-2003 HB: Handle Type 005 from station info file
!             09-Jul-2003 RD: Dummy keyword FLG004 is not necessary
!             08-Sep-2003 HU: Read radome flag
!             18-Sep-2003 RD: "UPDATE+"->"UPDATE" for windows
!             16-Oct-2003 MM: Check PCV file even w/o STAINFO file
!             19-Dec-2003 SS: Correct position of radome code if necessary
!             15-Jan-2004 HU: Read options to check marker type
!             21-Jan-2004 RD: Adapt crux-file if no radome code requested
!             21-Jan-2004 CU: Correct output line
!             29-Mar-2004 CU: Option to verify frequency added (verfrq)
!             28-Sep-2005 RD: Input option for event flag handling
!             08-Feb-2006 RD: (Hardwired) option to skip L2C-data
!             08-May-2007 AG: Galileo system implemented
!             27-Oct-2010 SL: use m_bern with ONLY
!             26-Jan-2011 LP: Satellite-specific observation types possible
!             01-Nov-2011 LP: Give iType to the main program
!             29-Dec-2011 RD: Change keyword SINTERV -> SAMPL (unification)
!             02-Feb-2012 RD: Shift keyword REQEPO to RXOB3F
!             26-Apr-2012 LP: Deactivate obstype selection via sat-specific
!                             geos file (select obstypes in RNXSMT only)
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, lfnPrt, &
                      keyValueLength, fileNameLength, fileExtLength, &
                      lineLength
  USE d_stacrx, ONLY: t_stacrux
  USE p_rxobv3, ONLY: t_rxobv3_err
  USE d_rinex3, ONLY: t_gobsdef

  USE s_gtstaflg
  USE s_readcrux
  USE s_readkeys
  USE s_fparse
  USE s_exitrc
  USE s_ckoptb
  USE s_ckoptc
  USE s_rdstax
  USE s_gtflna
  USE s_ckopti
  USE s_ckoptl
  USE s_gobsdef,ONLY: init_geos, readgeos
  USE f_lincount

  IMPLICIT NONE

! List of Parameters
! ------------------
! input:

! output:
  CHARACTER(LEN=*)           :: campgn  ! campaign characterization
  CHARACTER(LEN=*)           :: title   ! general title line
  INTEGER(i4b)               :: isasys  ! satellite system to be considered
                                        ! = 0: ALL
                                        ! = 1: GPS
                                        ! = 2: GLONASS
  INTEGER(i4b)               :: rnxSta  ! Old station name from RINEX
                                        ! = 0: File name (4ID)
                                        ! = 1: Marker name
                                        ! = 2: Marker number
                                        ! = 3: Marker name+number (IGS like)
  INTEGER(i4b)               :: iabupd  ! Station not in abbreviation table:
                                        ! -1: stop with error
                                        !  0: write warning
                                        !  1: update (big letters only)
                                        !  2: update+ (big/small letters)

  CHARACTER(LEN=*)           :: session ! Session ID
  INTEGER(i4b)               :: isintv  ! sampling interval (sec, 0: take all)
  INTEGER(i4b)               :: isoffs  ! sampling offset   (sec)
  INTEGER(i4b)               :: minsig  ! minimum signal strength
  INTEGER(i4b)               :: iacpt0  ! accept signal strength 0
  INTEGER(i4b)               :: icsflg  ! accept rinex cycle slip flags
  INTEGER(i4b)               :: nol2c   ! Skip l2c data
  INTEGER(i4b)               :: usegeos ! Use Giove Ext. Obs. Sel. file
  TYPE(t_gobsdef)            :: gobsdef ! Giove Ext. Obs. Sel. info
  TYPE(t_staCrux)            :: staInfo ! staInfo: entries for Bernese
  CHARACTER(LEN=*),DIMENSION(5):: flgStr ! Strings with flags used in staInfo
  TYPE(t_staCrux)            :: rnxInfo ! staCrux: entries in RINEX files
  TYPE(t_rxobv3_err)         :: erropt  ! error handling options


! List of functions
! -----------------

! Local Types
! -----------

! Local parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER :: srName = 'rxob3i'

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue
  CHARACTER(LEN=lineLength)                            :: line
  CHARACTER(LEN=fileNameLength)                        :: staFil
  CHARACTER(LEN=fileNameLength)                        :: crxFil
  CHARACTER(LEN=fileNameLength)                        :: antFil
  CHARACTER(LEN=fileNameLength)                        :: frqFil
  CHARACTER(LEN=fileNameLength)                        :: geosFil
  CHARACTER(LEN=fileNameLength)          :: node    ! used by FPARSE
  CHARACTER(LEN=fileNameLength)          :: device  ! used by FPARSE
  CHARACTER(LEN=fileNameLength)          :: dir     ! used by FPARSE
  CHARACTER(LEN=fileNameLength)          :: name    ! used by FPARSE
  CHARACTER(LEN=fileExtLength)           :: ext     ! used by FPARSE
  CHARACTER(LEN=fileExtLength)           :: ver     ! used by FPARSE

  INTEGER(i4b)                           :: iCrx
  INTEGER(i4b)                           :: irc     ! return code froms SRs
  INTEGER(i4b)                           :: iac     ! allocation status
  INTEGER(i4b)                           :: irCode  ! Counting errors
  INTEGER(i4b)                           :: ircgeos ! return code geos file
  INTEGER(i4b)                           :: usegeos1! ok for use of gobsdef
  INTEGER(i4b)                           :: nLin    ! # of lines in geos file
  INTEGER(i4b)                           :: iType   ! 1: RINEX; 2: SMT

! Init variables
! --------------
  irCode=0

  NULLIFY(keyValue)

  nol2c = 1

! Get campaign name
! -----------------
  CALL gtflna(1,'CAMPAIGN', campgn, irc)
  CALL fparse(0,campgn,node,device,dir,name,ext,ver,irc)
  campgn=name

! Get title line
! --------------
  CALL readkeys('TITLE', keyValue, irc)

  CALL ckoptl(0,'TITLE',keyValue,srName,                       &
              'Title line',irc,irCode,                         &
              empty=' ',maxVal=1,result1=title)

! Get satellite system
! --------------------
  CALL readKeys('SATSYS', keyValue, irc)

  CALL ckoptc(1,'SATSYS', keyValue,                            &
              (/'GPS    ','GLONASS','GALILEO','GPS/GAL',       &
                'GPS/GLO','GAL/GLO','ALL    '/),               &
              srName, 'Satellite System', irc, irCode,         &
              valList=(/1,2,3,4,5,6,0/), result1=isasys)

! Station name from RINEX header
! ------------------------------
  CALL readKeys('STANAM', keyValue, irc)

  CALL ckoptc(1,'STANAM', keyValue,                            &
              (/'MARKER_NAME  ','MARKER_NUMBER',               &
                'MARKER_DOMES ','FILE_NAME    '/),             &
              srName, 'Gather station name', irc, irCode,      &
              valList=(/1,2,3,0/), result1=rnxsta)

! Action if station not in abbreviation list
! ------------------------------------------
  CALL readKeys('ABBUPD',keyValue,irc)

  CALL ckoptc(1,'ABBUPD',keyValue,                                     &
              (/'ERROR  ','WARNING','UPDATE ','UPDATE+'/),srName,      &
              'Action if station not in abbreviation list',irc,irCode, &
              maxVal=1,valList=(/-1,0,1,2/),result1=iabupd)

#ifdef OS_WIN32
  if (iabupd == 2) iabupd = 1    ! Windows is not case sensitive
#endif

! Read the Session
! ----------------
  CALL readKeys('SESSID', keyValue, irc)

  CALL ckoptl(1,'SESSID', keyValue, srName,                        &
              'Session identifier for Bernese files', irc ,irCode, &
              empty=' ',maxLength=4,maxVal=1,result1=session)

! Sampling interval
! -----------------
  CALL readkeys('SAMPL' , keyValue, irc)

  CALL ckopti(1,'SAMPL' , keyValue, srName,                        &
              'Sampling interval in Bernese files',irc,irCode,     &
              maxVal=1,ge=0,empty=0,result1=isintv)

! Sampling offset
! ---------------
  CALL readkeys('SOFFSET' , keyValue, irc)

  CALL ckopti(1,'SOFFSET' , keyValue, srName,                      &
              'Sampling offset in Bernese files',irc,irCode,       &
              maxVal=1,ge=0,empty=0,result1=isoffs)

! Minimum signal strength
! -----------------------
  CALL readkeys('MINSIG'  , keyValue, irc)

  CALL ckopti(1,'MINSIG'  , keyValue, srName,                      &
              'Minimum signal strength',irc,irCode,                &
              maxVal=1,ge=0,result1=minsig)

! Accept signal strength=0
! ------------------------
  CALL ckoptb(1,(/'ACCEPT0'/),srName,                              &
              'Accept signal strength=0',irCode,                   &
              result1=iacpt0)

! Accept cycle slip flags from RINEX
! ----------------------------------
  CALL ckoptb(1,(/'ACCEPTCS'/),srName,                             &
              'Accept cycle slip flags from RINEX',irCode,         &
              result1=icsflg)

! Consider radom code
! -------------------
  CALL ckoptb(1,(/'RADOME'/),srName,                                  &
              'Consider radom code',irCode,                           &
              result1=erropt%Radome)

! Correct position of radome code
! -------------------------------
  CALL ckoptb(1,(/'CORRAD'/),srName,                                  &
              'Correct position of radome code',irCode,                           &
              result1=erropt%corRad)

! Get station info
! ----------------
  CALL gtflna(0,'STAINFO',staFil,irc)

  IF (irc == 0 .AND. LEN_TRIM(staFil) > 0) THEN

    CALL readCrux(staFil, staInfo)

    DEALLOCATE(staInfo%coovel,stat=iac)
    staInfo%ncoovel  = 0

  ELSE

    staFil = ' '

    staInfo%nrenam   = 0
    staInfo%ninfo    = 0
    staInfo%nprob    = 0
    staInfo%ncoovel  = 0
    staInfo%nstatype = 0

    erropt%tStanam = 0
    erropt%tAnttyp = 0
    erropt%tAntnum = 0
    erropt%tAntpos = 0
    erropt%verSta  = 0
    erropt%verFil  = .FALSE.
    erropt%verAnt  = 0
    erropt%verFrq  = 0

  ENDIF

! Reduce station info record to the list of flags
! -----------------------------------------------
  CALL gtstaflg('STAFLG',(/'FLG001','FLG002','FLG003','      ','FLG005'/),&
       staInfo,flgStr)


! Verify receiver/antenna name using PHASECC file
! -----------------------------------------------
    CALL gtflna(0,'PHASECC',antFil,irc)

    erropt%verAnt = 0

    IF (irc == 0 .AND. LEN_TRIM(antFil) > 0) THEN
      CALL ckoptb(1,(/'DOANTVER'/),srName,                                 &
                   'Check phase center file for receiver/antenna', irCode, &
                   result1=erropt%verAnt)

      IF (erropt%verAnt == 1) THEN

        CALL readKeys('ANTVER',keyValue,irc)

        CALL ckoptc(1,'ANTVER',keyValue,                                   &
                    (/'WARNING ','SKIP    ','ERROR   '/),srName,           &
                    'Error handling for antenna verification',irc,irCode,  &
                    maxVal=1,valList=(/1,2,3/),result1=erropt%verAnt)
      ENDIF
    ENDIF


! Handling of event flags in the RINEX files
! ------------------------------------------
    erropt%epoFlag = 0

    CALL readKeys('EPOFLAG',keyValue,irc)

    CALL ckoptc(1,'EPOFLAG',keyValue,                                  &
                (/'WARNING ','SKIP    ','ERROR   '/),srName,           &
                'Handling of event flags in RINEX',irc,irCode,         &
                maxVal=1,valList=(/1,2,3/),result1=erropt%epoFlag)


! Verify frequency information using frequency information file
! -------------------------------------------------------------
    CALL gtflna(0,'FRQINFO',frqFil,irc)

    erropt%verFrq = 0

    IF (irc == 0 .AND. LEN_TRIM(frqFil) > 0) THEN
      CALL ckoptb(1,(/'DOFRQVER'/),srName,                                   &
                   'Check frequency info. file for frequency', irCode, &
                   result1=erropt%verFrq)

      IF (erropt%verFrq == 1) THEN

        CALL readKeys('FRQVER',keyValue,irc)

        CALL ckoptc(1,'FRQVER',keyValue,                                   &
                    (/'WARNING ','SKIP    ','ERROR   ','UPDATE  '/),srName,&
                    'Error handling for frequency verification',irc,irCode,&
                    maxVal=1,valList=(/1,2,3,4/),result1=erropt%verFrq)
      ENDIF
    ENDIF


! Check station name
! ------------------
  IF (LEN_TRIM(staFil) > 0) THEN
    CALL readKeys('TSTANAM',keyValue,irc)

    CALL ckoptc(1,'TSTANAM',keyValue,                                   &
                (/'NO_CHECK','WARNING ','SKIP    ','ERROR   '/),srName, &
                'Error handling for station names',irc,irCode,          &
                maxVal=1,valList=(/0,1,2,3/),result1=erropt%tStanam)

! Try also RINEX file name
! ------------------------
    erropt%abbStn = .FALSE.
    IF (erropt%tStanam > 0 .AND. rnxsta > 0) THEN

      CALL ckoptb(1,(/'TRYFILE'/),srName,                               &
                 'Try RINEX file name as station name', irCode,         &
                 resultL=erropt%abbStn)

    ENDIF

! Check antenna name
! ------------------
    CALL readKeys('TANTTYP',keyValue,irc)

    CALL ckoptc(1,'TANTTYP',keyValue,                                   &
                (/'NO_CHECK','WARNING ','SKIP    ','ERROR   '/),srName, &
                'Error handling for antenna types',irc,irCode,          &
                maxVal=1,valList=(/0,1,2,3/),result1=erropt%tAnttyp)

! Consider radom code
! -------------------
    erropt%Trnchr=20

! Check antenna number
! --------------------
    CALL readKeys('TANTNUM',keyValue,irc)

    CALL ckoptc(1,'TANTNUM',keyValue,                                   &
                (/'NO_CHECK','WARNING ','SKIP    ','ERROR   '/),srName, &
                'Error handling for antenna number',irc,irCode,         &
                maxVal=1,valList=(/0,1,2,3/),result1=erropt%tAntnum)

! Check antenna position
! ----------------------
    CALL readKeys('TANTPOS',keyValue,irc)

    CALL ckoptc(1,'TANTPOS',keyValue,                                   &
                (/'NO_CHECK','WARNING ','SKIP    ','ERROR   '/),srName, &
                'Error hanlding for antenna position',irc,irCode,       &
                maxVal=1,valList=(/0,1,2,3/),result1=erropt%tAntpos)

! Check marker type
! -----------------
    CALL readKeys('TMRKTYP',keyValue,irc)

    CALL ckoptc(1,'TMRKTYP',keyValue,                                   &
                (/'NO_CHECK','WARNING ','SKIP    ','ERROR   '/),srName, &
                'Error hanlding for marker type',irc,irCode,            &
                maxVal=1,valList=(/0,1,2,3/),result1=erropt%tMrkTyp)


! Verify station name
! -------------------
    CALL ckoptb(1,(/'DOSTAVER'/),srName,                                &
                'Verify station name',irCode,                           &
                result1=erropt%verSta)

    IF (erropt%verSta /= 0) THEN
      CALL readKeys('STAVER', keyValue, irc)

      CALL ckoptc(1,'STAVER', keyValue,                                 &
                  (/'MARKER_NAME  ','MARKER_NUMBER','MARKER_DOMES '/),  &
                  srName, 'Verify station name', irc, irCode,           &
                  valList=(/1,2,3/), result1=erropt%verSta)
    ENDIF


! Verify station name using RINEX file name
! -----------------------------------------
    CALL ckoptb(1,(/'RNXVER'/),srName,                                  &
                 'Verify station name using RINEX name', irCode,        &
                 resultL=erropt%verFil)


! RINEX crux
! ----------
    CALL gtflna(0,'RNXINFO',crxFIl,irc)

    IF (irc == 0 .AND. LEN_TRIM(crxFil) > 0) THEN
      CALL rdstax(crxFIl,rnxInfo)
      IF (erropt%Radome == 0) THEN
        DO iCrx = 1,rnxInfo%nInfo
          rnxInfo%staInfo(iCrx)%antnam(17:20)='    '
        ENDDO
      ENDIF
    ENDIF

  ENDIF ! Station info file was selected


! Read signal type selection (GEOS) file for GIOVE, Galileo, SBAS
! ---------------------------------------------------------------
  usegeos = 0
  gobsdef%norec=0

! Check for original or SMT RINEX Files
! -------------------------------------
  CALL ckoptb(1,(/ 'RADIO_O','RADIO_S' /), srName,              &
              'Selection for type of RINEX input files',irCode, &
              result1=iType)

!  CALL gtflna(0,'GEOSFILE',geosFil,ircgeos)
!  IF ((ircgeos==0).AND.(LEN_TRIM(geosFil)>0).AND.(iType==1)) THEN
!     nLin = linCount(geosFil,6)
!     IF (nLin > 0) THEN
!         CALL init_geos(nLin,gobsdef)
!         CALL readgeos(geosFil,gobsdef,usegeos1)
!         IF (usegeos1==1) usegeos=1
!     ENDIF
!  ENDIF

  IF (irCode /= 0) CALL exitrc(2)

! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=iac)

! Report input options
! --------------------
  WRITE(lfnprt,'(2(A,/))') &
        ' PROGRAM INPUT OPTIONS:',' ---------------------'

  ! Satellite system selection
  line = ' Satellite system to be considered             : no selection'
  IF (isasys == 1) WRITE(line(49:80),'(A)') ' GPS only'
  IF (isasys == 2) WRITE(line(49:80),'(A)') ' GLONASS only'
  IF (isasys == 3) WRITE(line(49:80),'(A)') ' GALILEO only'
  IF (isasys == 4) WRITE(line(49:80),'(A)') ' GPS/GAL only'
  IF (isasys == 5) WRITE(line(49:80),'(A)') ' GPS/GLO only'
  IF (isasys == 6) WRITE(line(49:80),'(A)') ' GLO/GAL only'
  WRITE(lfnprt,'(A)') TRIM(line)

  line = ' Handling of L2C data                          : ignored'
  IF (nol2c /= 1) THEN
    WRITE(line(49:80),'(A)') ' used if no P2 available'
    WRITE(lfnprt,'(A)') TRIM(line)
    line = ''
    WRITE(line(49:80),'(A)') ' DANGEROUS because P2-C2 <<<'
  ENDIF
  WRITE(lfnprt,'(A,/)') TRIM(line)


  ! Get station name
  line = ' Gather station names from                     : RINEX file name (4-ID)'
  IF (rnxsta == 1) WRITE(line(49:80),'(A)') ' Marker name in RINEX file'
  IF (rnxsta == 2) WRITE(line(49:80),'(A)') ' Marker number in RINEX file'
  IF (rnxsta == 3) WRITE(line(49:80),'(A)') ' Marker name+number'
  WRITE(lfnprt,'(A)') TRIM(line)

  ! Update abbreviation table
  line = ' Update of abbreviation list for new stations  : not allowed'
  IF (iabupd == 1) WRITE(line(49:80),'(A)') ' update (big letters only)'
  IF (iabupd == 2) WRITE(line(49:80),'(A)') ' update (upper and lower case)'
  WRITE(lfnprt,'(A,/)') TRIM(line)


  ! Session ID
  line = ' Session ID used for Bernese observation files : automatically generated'
  IF (LEN_TRIM(session) > 0) WRITE(line(49:80),'(1X,A)') TRIM(session)
  WRITE(lfnprt,'(A,/)') TRIM(line)


  ! Data sampling interval
  line = ' Data sampling interval                        : same as in RINEX'
  IF (isintv /= 0) WRITE(line(49:80),'(I9,A)') isintv,'  second'
  WRITE(lfnprt,'(A)') TRIM(line)

  ! Data sampling offset
  line = ' Data sampling offset to full minute           : no offset'
  IF (isoffs /= 0) WRITE(line(49:80),'(I9,A)') isoffs,'  second'
  WRITE(lfnprt,'(A,/)') TRIM(line)


  ! Minimum signal strength
  line = ' Minimum signal strength required              : disabled'
  IF (minsig >= 0) WRITE(line(49:80),'(I9)') minsig
  WRITE(lfnprt,'(A)') TRIM(line)

  ! Accept signal strength zero
  line = ' Accept signal strength = 0                    : disabled'
  IF (iacpt0 == 1) WRITE(line(49:80),'(A)') ' enabled'
  WRITE(lfnprt,'(A)') TRIM(line)

  ! Accept cycle slips
  line = ' Accept cycle slip flags from RINEX            : disabled'
  IF (icsflg == 1) WRITE(line(49:80),'(A)') ' enabled'
  WRITE(lfnprt,'(A,/)') TRIM(line)


  ! Flags of station info file
  ! --------------------------
  IF (LEN_TRIM(staFil) > 0) THEN
    line = ' Flags to be considered in station info file   : ' // TRIM(staFil)
    WRITE(lfnprt,'(A)') TRIM(line)

    ! Flags for station renaming
    line = '      Type 001: Renaming of stations           : not considered'
    IF (flgStr(1) == '999') THEN
      WRITE(line(49:80),'(A)') ' all entries'
    ELSE IF (LEN_TRIM(flgStr(1)) > 0) THEN
      WRITE(line(49:80),'(1X,A)') TRIM(flgStr(1))
    ENDIF
    WRITE(lfnprt,'(A)') TRIM(line)

    ! Station information
    line = '      Type 002: Station information            : not considered'
    IF (flgStr(2) == '999') THEN
      WRITE(line(49:80),'(A)') ' all entries'
    ELSE IF (LEN_TRIM(flgStr(2)) > 0) THEN
      WRITE(line(49:80),'(1X,A)') TRIM(flgStr(2))
    ENDIF
    WRITE(lfnprt,'(A)') TRIM(line)

    ! Station problems
    line = '      Type 003: Station problems               : not considered'
    IF (flgStr(3) == '999') THEN
      WRITE(line(49:80),'(A)') ' all entries'
    ELSE IF (LEN_TRIM(flgStr(3)) > 0) THEN
      WRITE(line(49:80),'(1X,A)') TRIM(flgStr(3))
    ENDIF
    WRITE(lfnprt,'(A)') TRIM(line)

    ! Marker types
    line = '      Type 005: Marker types                   : not considered'
    IF (flgStr(5) == '999') THEN
      WRITE(line(49:80),'(A)') ' all entries'
    ELSE IF (LEN_TRIM(flgStr(5)) > 0) THEN
      WRITE(line(49:80),'(1X,A)') TRIM(flgStr(5))
    ENDIF
    WRITE(lfnprt,'(A,/)') TRIM(line)
  ENDIF

  ! Consistency checks
  ! ------------------
  IF (LEN_TRIM(staFil) + LEN_TRIM(antFil) + LEN_TRIM(frqFil) > 0) THEN

    line = ' Actions in case of inconsistencies'
    WRITE(lfnprt,'(A)') TRIM(line)

  ENDIF

  IF (LEN_TRIM(staFil) > 0) THEN
    ! Check of station names
    line = '      Station name                             : no check'
    IF (erropt%tStanam == 1) WRITE(line(49:80),'(A)') ' warning'
    IF (erropt%tStanam == 2) WRITE(line(49:80),'(A)') ' skip file'
    IF (erropt%tStanam == 3) WRITE(line(49:80),'(A)') ' stop with error'
    WRITE(lfnprt,'(A)') TRIM(line)

    ! Try also station name
    IF (rnxsta /= 0) THEN
      line = '      Alternative station name from RINEX file : disabled'
      IF (erropt%abbStn) WRITE(line(49:80),'(A)') ' enabled'
      WRITE(lfnprt,'(A)') TRIM(line)
    ENDIF

    ! Check of receiver/antenna name
    line = '      Receiver/antenna type                    : no check'
    IF (erropt%tAnttyp == 1) WRITE(line(49:80),'(A)') ' warning'
    IF (erropt%tAnttyp == 2) WRITE(line(49:80),'(A)') ' skip file'
    IF (erropt%tAnttyp == 3) WRITE(line(49:80),'(A)') ' stop with error'
    WRITE(lfnprt,'(A)') TRIM(line)

    line = '      Radome code                              : considered'
    IF (erropt%Radome==0) line(49:80)=' ignored'
    WRITE(lfnprt,'(A)') TRIM(line)

    line = '      Position of radome code                  : corrected'
    IF (erropt%corRad==0) line(49:80)=' not corrected'
    WRITE(lfnprt,'(A)') TRIM(line)


    ! Check of receiver/antenna number
    line = '      Receiver/antenna number                  : no check'
    IF (erropt%tAntnum == 1) WRITE(line(49:80),'(A)') ' warning'
    IF (erropt%tAntnum == 2) WRITE(line(49:80),'(A)') ' skip file'
    IF (erropt%tAntnum == 3) WRITE(line(49:80),'(A)') ' stop with error'
    WRITE(lfnprt,'(A)') TRIM(line)

    ! Check of antenna position
    line = '      Antenna position                         : no check'
    IF (erropt%tAntpos == 1) WRITE(line(49:80),'(A)') ' warning'
    IF (erropt%tAntpos == 2) WRITE(line(49:80),'(A)') ' skip file'
    IF (erropt%tAntpos == 3) WRITE(line(49:80),'(A)') ' stop with error'
    WRITE(lfnprt,'(A)') TRIM(line)

    ! Check for marker type
    line = '      Marker type                              : no check'
    IF (erropt%tmrkTyp == 1) WRITE(line(49:80),'(A)') ' warning'
    IF (erropt%tmrkTyp == 2) WRITE(line(49:80),'(A)') ' skip file'
    IF (erropt%tmrkTyp == 3) WRITE(line(49:80),'(A)') ' stop with error'
    WRITE(lfnprt,'(A,/)') TRIM(line)
  ENDIF

  ! Additional verifications
  ! ------------------------
  IF (LEN_TRIM(staFil) > 0) THEN

    ! Verify station name from RINEX header
    line = ' Verify station name/number using              : disabled'
    IF (erropt%versta == 1) WRITE(line(49:80),'(A)') ' Marker name in RINEX file'
    IF (erropt%versta == 2) WRITE(line(49:80),'(A)') ' Marker number in RINEX file'
    IF (erropt%versta == 3) WRITE(line(49:80),'(A)') ' Marker name+number'
    WRITE(lfnprt,'(A)') TRIM(line)

    ! Verify station name from RINEX file name
    line = ' Verify station name using RINEX file name     : disabled'
    IF (erropt%verFil) WRITE(line(49:80),'(A)') ' enabled'
    WRITE(lfnprt,'(A,/)') TRIM(line)
  ENDIF

  ! Check receiver/antenna in PHASECC file
  IF (LEN_TRIM(antFil) > 0) THEN
    line = ' Check phase center for receiver/antenna       : no check'
    IF (erropt%verAnt  == 1) WRITE(line(49:80),'(A)') ' warning'
    IF (erropt%verAnt  == 2) WRITE(line(49:80),'(A)') ' skip file'
    IF (erropt%verAnt  == 3) WRITE(line(49:80),'(A)') ' stop with error'
    WRITE(lfnprt,'(A,/)') TRIM(line)
  ENDIF

  ! Check frequency in frequency information file
  IF (LEN_TRIM(frqFil) > 0) THEN
    line = ' Check frequency information file for frequency: no check'
    IF (erropt%verFrq  == 1) WRITE(line(49:80),'(A)') ' warning'
    IF (erropt%verFrq  == 2) WRITE(line(49:80),'(A)') ' skip file'
    IF (erropt%verFrq  == 3) WRITE(line(49:80),'(A)') ' stop with error'
    IF (erropt%verFrq  == 4) WRITE(line(49:80),'(A)') ' update'
    WRITE(lfnprt,'(A,/)') TRIM(line)
  ENDIF

  ! Handling of event flags in RINEX
  line = ' Handling of event flags in RINEX files        : ignore'
  IF (erropt%epoFlag == 1) WRITE(line(49:80),'(A)') ' ignore, but warning'
  IF (erropt%epoFlag == 2) WRITE(line(49:80),'(A)') ' skip file'
  IF (erropt%epoFlag == 3) WRITE(line(49:80),'(A)') ' stop with error'
  WRITE(lfnprt,'(A,/)') TRIM(line)

  RETURN
END SUBROUTINE rxob3i

END MODULE
