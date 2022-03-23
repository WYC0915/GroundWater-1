MODULE s_RDIRCO
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdirco(nAllSta, allStaNum, allStaName, globalWindow, clksys, &
                  nclreq, istclk, isaclk, nclk, ibias, clkwgt, clfrto,  &
                  nRec, isbTim)

! -------------------------------------------------------------------------
! Purpose:    Reads the receiver clock offset input options for GPSEST
!
! Author:     R. Dach
!
! Created:    22-Jun-2001
!
! Changes:    04-Jul-2001 RD: new call of gtStaNum (for ADDNEQ2)
!             30-Jul-2001 RD: "Time window" is a special option
!             05-Sep-2001 HU: Interface for alcerr added
!             28-Aug-2001 RD: New call of SR gtStaNum
!             08-Oct-2002 RD: New call of SR gtStaNum
!             03-Feb-2003 RD: New call of GTSTANUM (weight is pointer now)
!             27-Mar-2003 RD: New parameter time window definition
!             19-May-2003 RD: Use SR gttimwin instead of SR readsess
!             04-Nov-2003 HB: Declare allStaName with (:)
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             09-May-2009 RD: Sat/frq-specific receiver clock biases
!             08-Sep-2010 RD: Merge SLR-time bias option
!             15-Feb-2012 RD: Seperate RCO and IFB/ISB by radiobuttons
!             27-Apr-2012 RD: Nullify all pointers
!             11-Jun-2012 MM: New GPSEST panel/logic
!             26-Oct-2012 RD: Move ISBWIN into the MOREOPT area
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_time,   ONLY: t_timint
  USE d_const,  ONLY: C
  USE p_gpsest, ONLY: t_isbTime
  USE s_ckoptu
  USE s_ckoptb
  USE s_ckoptl
  USE s_ckopti
  USE s_ckoptt
  USE s_ckoptc
  USE s_ckoptr
  USE s_alcerr
  USE s_gtstanum
  USE s_gttimwin
  USE s_timst2
  USE s_alcerr
  USE s_readkeys
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                    :: nAllSta      ! number of all stations
  INTEGER(i4b), DIMENSION(*)      :: allStaNum    ! station numbers
  CHARACTER(LEN=staNameLength),   &
                     DIMENSION(:) :: allStaName   ! station names
  TYPE(t_timint)                  :: globalWindow ! window to be processed
                                                  ! (from - to, MJD)
  INTEGER(i4b)                    :: clksys       ! 1: One rec.clk for
                                                  !    each sat.sys

! output:
  INTEGER(i4b)                    :: nclreq       ! # clock error requests
  INTEGER(i4b), DIMENSION(:)      :: istclk       ! station numbers for clock
                                                  ! requests
  INTEGER(i4b), DIMENSION(:)      :: isaclk       ! satellite numbers for clock
                                                  ! requests (SLR time bias)
  INTEGER(i4b), DIMENSION(*)      :: nclk         ! # clock parameters to be
                                                  ! estimated
                                                  ! nclk(i)=1: offset estimated
                                                  !            for station i
  INTEGER(i4b), DIMENSION(*)      :: iBias        ! 0: station specific
                                                  ! 1: frequency specific
                                                  ! 2: satellite specific
                                                  ! 3: sat. specific for non-GPS
                                                  ! 4: frequency specific with polynom
  REAL(r8b), DIMENSION(2,*)       :: clkwgt       ! a priori weight for clocks
                                                  ! (in sec)
  REAL(r8b), DIMENSION(2,*)       :: clfrto       ! time interval for clock
                                                  ! errors (MJD for request k)
                                                  ! clfrto(1,k): start
                                                  ! clfrto(2,k): end
  INTEGER(i4b)                    :: nRec         ! Number of isb-requests
  TYPE(t_isbTime),                 &
             DIMENSION(:), POINTER:: isbTim       ! Time-dep. inter-syst. biases

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER :: srName = 'rdirco'

! Expands boundaries for receiver clock offset to prevent rounding problems
! (was done in GPSEST_P):
  REAL(r8b), PARAMETER :: dtRco = 1d0   ! (sec)


! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength),      &
             DIMENSION(:),   POINTER :: keyValue
  CHARACTER (LEN=staNam2Length),      &
             DIMENSION(:,:), POINTER :: hlpStr
  CHARACTER(LEN=40)                  :: epost1, epost2
  CHARACTER(LEN=staNameLength),       &
             DIMENSION(:),   POINTER :: staNam    ! Station names,
                                                  ! dummy for GPSEST

  INTEGER(i4b)                       :: iHlp
  INTEGER(i4b)                       :: irCode
  INTEGER(i4b)                       :: irc,iac

  REAL(r8b), DIMENSION(2)            :: localWindow
  REAL(r8b), DIMENSION(:,:), POINTER :: r3Hlp
  REAL(r8b)                          :: rHlp

  LOGICAL                            :: moreOpt
  LOGICAL                            :: isRcv, isTim

! Init the variables
! ------------------
  irCode = 0

  NULLIFY(KeyValue)
  NULLIFY(hlpStr)
  NULLIFY(staNam)
  NULLIFY(r3Hlp)


! Station specific offsets or biases
! ----------------------------------
  CALL ckoptb(1,(/'RCVCLOCK'/),srName,'GLONASS clock bias', &
              irCode, resultL= isRcv)
  CALL ckoptb(1,(/'MOREOPT'/),srName,'More options', &
              irCode, resultL= moreOpt)
  IF (moreOpt .AND. isRcv) THEN
    iHlp = 0 ! iHlp == 0 for the traditional "STATION-SPECIFIC"
  ELSE
    iHlp = 1
  ENDIF

! Type of GPS-GLONASS Inter-frequency/system bias
! -----------------------------------------------
  IF (iHlp == 1) THEN
    CALL readKeys('RCVCLKTYP', keyValue, irc)

    CALL ckoptc(1,'RCVCLKTYP', keyValue, &
                (/'NONE              ','FREQUENCY_SPECIFIC',&
                  'FREQ_SPEC_POLYNOM ','SATELLITE_SPECIFIC',&
                  'NON-GPS_SATELLITES'/), srName, &
                'Parameter setup for GPS-GLONASS biases', irc, irCode, &
                maxVal=1,valList=(/-1,1,4,2,3/),result1=iHlp)
  ENDIF

! Biases: for all stations with a condition of sum in NORINI
! ----------------------------------------------------------
  IF (iHlp == -1) THEN
    nclreq = 0

  ELSE IF (iHlp /= 0) THEN

    nclreq = nAllSta

    istclk(1:nclreq)   = allStaNum(1:nAllSta)

    iBias(1:nclreq)    = iHlp
    nclk(1:nclreq)     = 1
    IF (iHlp == 4) nclk(1:nclreq) = 4
    clkwgt(1,1:nclreq) = 0d0

! Receiver clock offsets
! ----------------------
  ELSE
    ALLOCATE(staNam(nAllSta), stat=irc)
    CALL alcerr(irc, 'staNam', (/nAllSta/), 'rdirco')

! Get the station selection
! -------------------------
    CALL gtStaNum(nAllSta, allStaNum, allStaName,           &
                 'RCVCLKLST', 'STATION5', 'STAFILE5', ' ',  &
                  nclreq, istclk, stanam, 0, r3Hlp)

    DEALLOCATE(stanam, stat=irc)

! No stations selected
! --------------------
    IF (nclreq == 0) RETURN

! Compute the offset only
! -----------------------
    nclk(1:nclreq) = 1
    iBias(1:nclreq) = 0

! Satellite-specific time bias for SLR
! (to be implemented)
! ------------------------------------
    isaclk(1:nclreq) = 0

! Read the apriori sigma
! ----------------------
    CALL readKeys('SIGMA5', keyValue, irc)

    CALL ckoptr(1,'SIGMA5', keyValue, srName, &
                'Sigma for receiver clock offsets',irc, irCode, &
                maxVal=1,empty=0d0,ge=0d0,result1=rHlp)

    clkwgt(1,1:nclreq) = rHlp / 1000.0 * C
  ENDIF

! A special time window was set (only for receiver clock offsets)
! ---------------------------------------------------------------
  IF (nclreq > 0 .AND. iHlp == 0) THEN
    CALL gttimwin(' ',(/'RCOTIM  ','RADIO5_1','RADIO5_2'/),    &
                  (/'SESSION_YEAR5','SESSION_STRG5'/),         &
                  (/'STADAT5','STATIM5','ENDDAT5','ENDTIM5'/), &
                  localWindow)

! The time window is outside the obs. window
! ------------------------------------------
    IF (localWindow(1) > globalWindow%t(2)+dtRco/86400d0 .OR. &
        localWindow(2) < globalWindow%t(1)-dtRco/86400d0) THEN
      CALL timst2(1,2, localWindow,    epost1)
      CALL timst2(1,2, globalWindow%t, epost2)
      WRITE(lfnerr,'(/,A,4(/,16X,A),/)')                                  &
        ' ### SR RDIRCO: The time window for the receiver clock setup',   &
             'is outside of the time window for the observations!',       &
             'Receiver clocks:     '//epost1,                             &
             'Observations:        '//epost2,                             &
             'No receiver clock offsets were setup.'
      nclreq = 0

! Use the time window definition
    ELSE

      IF (localWindow(1) == 0d0) THEN
        clfrto(1,1:nclreq) = globalWindow%t(1) - dtRco/86400d0
      ELSE
        clfrto(1,1:nclreq) = localWindow(1) - dtRco/86400d0
      ENDIF

      IF (localWindow(2) == 1d20) THEN
        clfrto(2,1:nclreq) = globalWindow%t(2) + dtRco/86400d0
      ELSE
        clfrto(2,1:nclreq) = localWindow(2) + dtRco/86400d0
      ENDIF

    ENDIF
  ELSEIF (nclreq > 0 .AND. iHlp > 0) THEN
    clfrto(1,1:nclreq) = globalWindow%t(1) - dtRco/86400d0
    clfrto(2,1:nclreq) = globalWindow%t(2) + dtRco/86400d0
  ENDIF

! Read the receiver type specific time-dep. inter-system biases
! -------------------------------------------------------------
  nRec = 0
  isTim = .FALSE.

! Time-dependency activated?
  IF ( moreOpt ) THEN
    CALL ckoptb(1,(/'IFBWIN'/),srName,'Time-dependent GLO biases', &
                irCode, resultL= isTim)
  ENDIF

  IF (clkSys /= 1 .AND. isTim) THEN

    CALL readKeys('RCOSTR',keyValue,irc)
    IF (irc == 0) nRec = SIZE(keyValue)

    ALLOCATE(isbTim(nRec),stat=iac)
    CALL alcerr(iac,'isbTim',(/nRec/),srName)

    ALLOCATE(hlpStr(4,nRec),stat=iac)
    CALL alcerr(iac,'hlpStr',(/4,nRec/),srName)

    CALL ckoptu(1,'RCOSTR',keyValue,srName,                            &
                'Time dependent inter-system biases', irc,irCode,      &
                numCol=4,maxVal=SIZE(hlpStr,2),result2=hlpStr)

    CALL ckoptl(1,'RCOSTR',hlpStr(1,:),srName,                         &
                  'Time dependent inter-system biases', irc, irCode,   &
                  colTit='Receiver name',empty='',                     &
                  maxVal=nRec,result2=isbTim(1:nRec)%recnam)

    CALL ckopti(1,'RCOSTR',hlpStr(2,:),srName,                         &
                  'Time dependent inter-system biases', irc, irCode,   &
                  colTit='Receiver number, from',empty=0,              &
                  maxVal=nRec,result2=isbTim(1:nRec)%recnum(1))

    CALL ckopti(1,'RCOSTR',hlpStr(3,:),srName,                         &
                  'Time dependent inter-system biases', irc, irCode,   &
                  colTit='Receiver number, to',empty=999999,           &
                  maxVal=nRec,result2=isbTim(1:nRec)%recnum(2))

    CALL ckoptt(1,'RCOSTR',hlpStr(4,:),srName,                         &
                  'Time dependent inter-system biases', irc, irCode,   &
                  colTit='Intervals for the biases',empty=1d20,        &
                  maxVal=nRec,result2=isbTim(1:nRec)%isbInt)

    DEALLOCATE(hlpStr,stat=iac)
  ENDIF

  DEALLOCATE(keyValue,stat=irc)

  IF (irCode /= 0) CALL exitrc(2)

  RETURN
END SUBROUTINE rdirco

END MODULE
