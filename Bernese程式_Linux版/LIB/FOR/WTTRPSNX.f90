MODULE s_WTTRPSNX
CONTAINS

! --------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! --------------------------------------------------------------------------

SUBROUTINE wttrpsnx(iPgm,filNam,trpEst,aIdx,aNor,xxx,locq,rms,nPar,        &
                    nSmpNq,nStat,nCentr,xStat,iCentr,staNam,datum,irCode,grdTim)

! --------------------------------------------------------------------------
! Purpose:    Write a troposphere SINEX file (for piecwise linear tropo)
!
! Author:     M. Meindl
!
! Created:    14-May-2003
! Last mod.:  01-Sep-2011
!
! Changes:    24-Jun-2003 HU: Interface to RDPWIN inserted
!             10-Mar-2004 HU: Writing of SINEX.-file content modified
!             17-Aug-2004 HU: Nullify keyValue
!             28-Apr-2005 SS: Translation of ITRF2000 datum identifier
!             24-Aug-2006 AG: GMF implemented
!             30-Jun-2008 RD: VMF added
!             21-May-2010 MF: Nullify trpWin
!             01-Sep-2011 LP: Write tropo gradients
!
! SR used:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! --------------------------------------------------------------------------


! Modules
! -------
  USE m_bern
  USE m_time,   ONLY: t_timint
  USE d_trpest, ONLY: t_trpest, dtMax

  USE f_ikf
  USE s_ckoptt
  USE s_opnfil
  USE s_sinini
  USE s_parint
  USE s_clocks
  USE f_djul
  USE f_parfac
  USE s_rdpwin
  USE s_opnerr
  USE s_readkeys
  USE s_sindat
  USE s_exitrc
  USE f_iyear4
  USE s_gtflna
  IMPLICIT NONE


! List of parameters
! ------------------
! input
  INTEGER(i4b)                  :: iPgm    ! Calling program
                                           !  =1: GPSEST
                                           !  =2: ADDNEQ2
  CHARACTER(LEN=fileNameLength) :: filNam  ! file name, blank: use GTFLNA
                                           ! mechanism with keyword TROPSNX
  TYPE(t_trpest)                :: trpEst  ! Structure of troposphere file
  INTEGER(i4b),DIMENSION(:,:)   :: aIdx    ! Parameter index for aNor
  REAL(r8b),DIMENSION(:)        :: aNor    ! Normal equation
  REAL(r8b),DIMENSION(:)        :: xxx     ! Solution vector (i=1,...)
  INTEGER(i4b),DIMENSION(:,:)   :: locq    ! Parameter characterization
  REAL(r8b)                     :: rms     ! rms of one phase observation
  INTEGER(i4b)                  :: nPar    ! Total number of parameters
  INTEGER(i4b)                  :: nSmpNq  ! Data sampling rate
  INTEGER(i4b)                  :: nStat   ! Number of stations
  INTEGER(i4b)                  :: nCentr  ! Number of center stations
  REAL(r8b),DIMENSION(:,:)      :: xStat   ! Geocentric sta coordinates
                                           !  (i=1,..3;j=1,..,nStat)
  INTEGER(i4b),DIMENSION(:)     :: iCentr  ! Index of center station
                                           ! for station i  (i=1,..,nStat)
  CHARACTER(LEN=16),DIMENSION(:):: staNam  ! Station names  (i=1,..,nStat)
  CHARACTER(LEN=16)             :: datum   ! Local geodetic datum
  REAL(r8b),DIMENSION(:,:)      :: grdTim  ! Time for tropo gradients

! output
  INTEGER(i4b)                  :: irCode  ! Return code of this SR
                                           ! = 1: No filename specified,
                                           !      file not written


! Local Variables
! ---------------
! general
  CHARACTER(LEN=keyValueLength),                                           &
                  DIMENSION(:), POINTER :: keyValue
  CHARACTER(LEN=fileNameLength)         :: filSnx
  CHARACTER(LEN=80)                     :: line, linhed
  INTEGER(i4b), DIMENSION(8)            :: actTim
  REAL(r8b)                             :: tHead, actDay

! time windows
  REAL(r8b)                             :: hlpTim
  TYPE(t_timint)                        :: maxWin
  TYPE(t_timint), DIMENSION(:), POINTER :: trpWin

! SINEX variables
  CHARACTER(LEN=1)                      :: obsCod
  CHARACTER(LEN=3)                      :: sinExt, solCon
  CHARACTER(LEN=3)                      :: agency, datOwn
  CHARACTER(LEN=12)                     :: timStrAct, timStrMin, timStrMax
  CHARACTER(LEN=16)                     :: sysTxt
  CHARACTER(LEN=22),DIMENSION(8)        :: mapStr
  REAL(r8b)                             :: sinVer
  INTEGER(i4b)                          :: iSampl

! indices and loop variables
  INTEGER(i4b)                          :: iSta, jSta
  INTEGER(i4b)                          :: iTrp, iTrp1
  INTEGER(i4b)                          :: idx, iInt, iPar
  INTEGER(i4b)                          :: nTimInt, iWrite

! interpolation of parameters
  REAL(r8b), DIMENSION(3)               :: xPart
  REAL(r8b)                             :: xFact
  REAL(r8b)                             :: t_0, dt_0
  REAL(r8b)                             :: t1, t2, trpEpo
  REAL(r8b)                             :: corr1, corr2
  REAL(r8b)                             :: x11, x12, x22
  REAL(r8b)                             :: trpPar, trpRms, trpRes
  REAL(r8b)                             :: grdn1,grdn2,grdn,grde1,grde2,grde
  REAL(r8b)                             :: rmsgrdn,rmsgrde
  REAL(r8b)                             :: grdFact
  INTEGER(i4b),DIMENSION(2)             :: grdInd
! error codes
  INTEGER(i4b)                          :: irc, ioStat

! Some initializations
! --------------------
  irCode = 0
  irc    = 0
  NULLIFY(keyValue)
  NULLIFY(trpWin)

! Open file
! ----------
  IF (LEN_TRIM(filNam)==0) CALL gtflna(0,'TROPSNX',filNam,irc)
  IF (LEN_TRIM(filNam)==0 .OR. irc/=0) THEN
    irCode = 1
    RETURN
  END IF

  CALL opnfil(lfnres,filNam,'UNKNOWN',' ', ' ',' ',ioStat)
  CALL opnerr(lfnerr,lfnres,ioStat,filNam,'sr wttrpsnx')


! Get some settings from SINEX input file
! ---------------------------------------
  CALL sinini(agency,datOwn)
  mapStr = (/"1/COS(Z)              ","HOPFIELD              ",            &
             "DRY NIELL             ","WET NIELL             ",            &
             "DRY GMF               ","WET GMF               ",            &
             "DRY VMF               ","WET VMF               "/)
  sysTxt = datum(1:6)
  IF (datum(1:6) == 'ITRF20') sysTxt = 'ITRF'//datum(7:8)
  sinExt = 'TRO'
  sinVer = 0.01
  obsCod = 'P'
  solCon = 'MIX'


! Get actual time
! ---------------
  CALL clocks(actTim)
  actTim(1) = iYear4(actTim(1))
  actDay    = actTim(3)+actTim(5)/24.d0+actTim(6)/1440.d0+actTim(7)/86400.d0
  tHead     = djul(actTim(1),actTim(2),actDay)
  CALL sindat(0,tHead,timStrAct)


! Setup time intervals
! --------------------
! get the parameter time window definition
  CALL rdpwin('TRPOFF',(/' ',' '/),t_0,dt_0)
  CALL readKeys('TRPRES',keyValue,irc)
  CALL ckoptt(1,'TRPRES',keyValue,'sr wttrpsnx',                           &
              'Time resolution for troposphere SINEX file',irc,irCode,     &
              gt=0.d0,maxVal=1,result1=trpRes)
  dt_0 = dt_0-trpRes/2.d0

! get maximum window
  maxWin%t(1) = 1.d20
  maxWin%t(2) = 0.d0
  DO iSta=1,trpEst%nSta
    DO iTrp=1,trpEst%sta(iSta)%nTrp
      hlpTim = trpEst%sta(iSta)%trp(iTrp)%timInt(1)
      IF (hlpTim==0.d0) CYCLE
      IF (hlpTim<maxWin%t(1)) maxWin%t(1) = hlpTim
      IF (hlpTim>maxWin%t(2)) maxWin%t(2) = hlpTim
    END DO
  END DO

! get intervals
  nTimInt = 0
  CALL parint(maxWin,dtMax,t_0,dt_0,trpRes,                                &
              'Time resolution for trop SINEX',nTimInt,trpWin)

! get max and min time
  CALL sindat(0,maxWin%t(1),timStrMin)
  CALL sindat(0,maxWin%t(2),timStrMax)

! compute sampling rate (seconds)
  iSampl = nint(trpRes*3600.d0)


! Write header line
! -----------------
  write(lfnres,"(A2,A3,1X,F4.2,1X,A3,1X,A12,1X,A3,2(1X,A12),1X,A1,1X,A4)") &
               "%=",sinExt,sinVer,agency,timStrAct,datOwn,                 &
               timStrMin,timStrMax,obsCod,solCon


! Write reference block
! ---------------------
  CALL gtflna(1,'SINEXIN',filSnx,irc)
  CALL opnfil(lfnloc,filSnx,'OLD','FORMATTED','READONLY',' ',ioStat)
  CALL opnerr(lfnerr,lfnloc,ioStat,filSnx,'sr wttrpsnx')
  iWrite=0
  IniLoop: DO
    READ(lfnloc,'(A)',iostat=ioStat) line
    IF (ioStat /= 0) EXIT IniLoop
    IF (line == '+FILE/REFERENCE') THEN
      iWrite=1
      linhed=line
      READ(lfnloc,'(A)',iostat=ioStat) line
      IF (ioStat /= 0) EXIT IniLoop
      IF (line(1:1) == '-') THEN
        iWrite=0
      ELSE
        WRITE(lfnres,'("*",79("-"))')
        WRITE(lfnres,'(A)') linhed
      ENDIF
    ENDIF
    IF (iWrite == 1) THEN
      WRITE(lfnres,'(A)') line
      IF (line(1:1) == '-') iWrite=0
    ENDIF
  END DO IniLoop
  CLOSE(lfnloc)
  IF (iWrite == 1) THEN
    WRITE(lfnerr,"(/,' *** SR WTTRPSNX: Error reading SINEX input file', &
                &  /,'                  File: ',A,/)") TRIM(filSnx)
    CALL exitrc(2)
  ENDIF

! Write troposphere description block
! -----------------------------------
  write(lfnres,"('*',79('-'))")
  write(lfnres,"(A17)") "+TROP/DESCRIPTION"
  write(lfnres,"(A40,A40)") "*_________KEYWORD_____________ __VALUE(S",    &
                          ")_______________________________________"
  write(lfnres,"(A30,1X,I22)")                                             &
               " ELEVATION CUTOFF ANGLE       ",trpEst%head%iElvnq
  write(lfnres,"(A30,1X,I22)")                                             &
               " SAMPLING INTERVAL            ",nSmpNq
  write(lfnres,"(A30,1X,I22)")                                             &
               " SAMPLING TROP                ",iSampl
  write(lfnres,"(A30,1X,A22)")                                             &
               " TROP MAPPING FUNCTION        ",mapStr(trpEst%head%iTrMap)
  IF (trpEst%head%iTrGrd(1) /= 0) THEN
    write(lfnres,"(A30,6(1X,A6))")                                           &
               " SOLUTION_FIELDS_1            ","TROTOT","STDDEV","TGNTOT",  &
               "STDDEV","TGETOT","STDDEV"
  ELSE
    write(lfnres,"(A30,2(1X,A6))")                                           &
               " SOLUTION_FIELDS_1            ","TROTOT","STDDEV"
  ENDIF
  write(lfnres,"(A17)") "-TROP/DESCRIPTION"


! Write station information block
! -------------------------------
  write(lfnres,"('*',79('-'))")
  write(lfnres,"(A21)") "+TROP/STA_COORDINATES"
  write(lfnres,"(A42,A25)") "*SITE PT SOLN T __STA_X_____ __STA_Y_____ ",  &
                            "__STA_Z_____ SYSTEM REMRK"

! loop over all coordinates
  DO iSta=1,nStat-nCentr
    DO jSta=1,iSta-1
      IF (staNam(iSta)(1:4)==staNam(jSta)(1:4)) THEN
        write(lfnerr,"(/,A,/,2(A,A16,/))")                                 &
             " ### sr wttrpsnx: Same 4-char abbreviation for two stations",&
             "                  Station 1: ",staNam(iSta),                 &
             "                  Station 2: ",staNam(jSta)
      END IF
    END DO

    IF (iPgm==1) THEN
      DO iPar=1,nPar
        IF (locq(1,iPar)==1 .AND. locq(3,iPar)==1 .AND.                    &
                             iCentr(iSta)==locq(2,iPar)) THEN
          xPart = xStat(:,iSta)+xxx(iPar:iPar+2)
        END IF
      END DO
    ELSE IF (iPgm==2) THEN
      xPart = xStat(:,iSta)
    END IF
    write(lfnres,"(1X,A4,1X,A9,3(1X,F12.3),1X,A6,1X,A3)")                  &
         staNam(iSta)(1:4)," A    1 P",xPart,sysTxt,agency
  END DO
  write(lfnres,"(A21)") "-TROP/STA_COORDINATES"


! Write TROP/SOLUTION block
! -------------------------
  write(lfnres,"('*',79('-'))")
  write(lfnres,"(A14)") "+TROP/SOLUTION"
  IF (trpEst%head%iTrGrd(1) /= 0) THEN
    write(lfnres,"(A62)") "*SITE ____EPOCH___ TROTOT STDDEV  TGNTOT STDDEV  TGETOT STDDEV"
  ELSE
    write(lfnres,"(A32)") "*SITE ____EPOCH___ TROTOT STDDEV"
  ENDIF
  idx = 0
  DO iSta=1,trpEst%nSta
    DO iInt=1,nTimInt

     trpEpo = (trpWin(iInt)%t(1)+trpWin(iInt)%t(2))/2.d0

! loop over all troposphere parameters
      DO iTrp=1,trpEst%sta(iSta)%nTrp
        iTrp1 = iTrp+1
        IF (iTrp==trpEst%sta(iSta)%nTrp) iTrp1 = iTrp     ! last parameter
        t1    = trpEst%sta(iSta)%trp(iTrp)%timInt(1)
        t2    = trpEst%sta(iSta)%trp(iTrp1)%timInt(1)
        IF (t1==0.d0 .OR. t2==0.d0) CYCLE
        xFact = parfac(trpEpo,t1,trpEst%head%iTab,dtMax)

! parameter spacing too big, trpEpo out of window, or xFact=0
        IF (t2-t1>trpEst%head%iTab+dTmax) CYCLE
        IF (trpEpo<t1-dtMax) CYCLE
        IF (xFact/=1.d0 .AND. iTrp==iTrp1) CYCLE
        IF (xFact==0.d0) CYCLE

! compute parameter and rms
        corr1  = trpEst%sta(iSta)%trp(iTrp)%total
        corr2  = trpEst%sta(iSta)%trp(iTrp1)%total
        x11    = aNor(ikf(aIdx(idx+iTrp,5),aIdx(idx+iTrp,5)))
        x12    = aNor(ikf(aIdx(idx+iTrp,5),aIdx(idx+iTrp1,5)))
        x22    = aNor(ikf(aIdx(idx+iTrp1,5),aIdx(idx+iTrp1,5)))
        trpPar = (corr1*xFact+corr2*(1.d0-xFact))*1.d3
        trpRms = xFact**2*x11+2.d0*x12*xFact*(1.d0-xFact)+x22*(1-xFact)**2
        trpRms = rms*dsqrt(trpRms)*1.d3

        IF (trpEst%head%iTrGrd(1) /= 0) THEN
          grdn1  = trpEst%sta(iSta)%trp(iTrp)%corr(1)
          grdn2  = trpEst%sta(iSta)%trp(iTrp1)%corr(1)
          grdn   = (grdn1*xFact+grdn2*(1.d0-xFact))*1.d3
          grde1  = trpEst%sta(iSta)%trp(iTrp)%corr(2)
          grde2  = trpEst%sta(iSta)%trp(iTrp1)%corr(2)
          grde   = (grde1*xFact+grde2*(1.d0-xFact))*1.d3

          grdInd = 0
          IF (trpEpo < grdTim(idx+iTrp1,1)) THEN
            grdInd(1) = idx+iTrp
          ELSE
            grdInd(1) = idx+iTrp1
          ENDIF
          IF (trpEpo < grdTim(idx+iTrp,2)) THEN
            grdInd(2) = idx+iTrp
          ELSE
            grdInd(2) = idx+iTrp1
          ENDIF

          grdFact = parfac(trpEpo,grdTim(grdInd(1),1), &
                    trpEst%head%iTab*trpEst%head%iTrGrd(2),dtMax)

          x11    = aNor(ikf(aIdx(grdInd(1),1),aIdx(grdInd(1),1)))
          x12    = aNor(ikf(aIdx(grdInd(1),1),aIdx(grdInd(2),3)))
          x22    = aNor(ikf(aIdx(grdInd(2),3),aIdx(grdInd(2),3)))
          rmsgrdn = grdFact**2*x11+2.d0*x12*grdFact*(1.d0-grdFact)+x22*(1-grdFact)**2
          rmsgrdn = rms*dsqrt(rmsgrdn)*1.d3

          x11    = aNor(ikf(aIdx(grdInd(1),2),aIdx(grdInd(1),2)))
          x12    = aNor(ikf(aIdx(grdInd(1),2),aIdx(grdInd(2),4)))
          x22    = aNor(ikf(aIdx(grdInd(2),4),aIdx(grdInd(2),4)))
          rmsgrde = grdFact**2*x11+2.d0*x12*grdFact*(1.d0-grdFact)+x22*(1-grdFact)**2
          rmsgrde = rms*dsqrt(rmsgrde)*1.d3
        ENDIF

! write output line
        CALL sindat(0,trpEpo,timStrAct)
        IF (trpEst%head%iTrGrd(1) == 0) THEN
          write(lfnres,"(1X,A4,1X,A12,2(1X,F6.1))")                        &
              trpEst%sta(iSta)%staNam(1:4),timStrAct,trpPar,trpRms
        ELSE
          WRITE(lfnres,"(1X,A4,1X,A12,2(1X,F6.1),2(1X,F7.3,1X,F6.3))")     &
              trpEst%sta(iSta)%staNam(1:4),timStrAct,trpPar,trpRms,grdn,   &
              rmsgrdn,grde,rmsgrde
        ENDIF
        EXIT
      END DO
    END DO
    idx = idx+trpEst%sta(iSta)%nTrp
  END DO
  write(lfnres,"(A14)") "-TROP/SOLUTION"
  write(lfnres,"(A8)") "%=ENDTRO"


! Close file
! ----------
    CLOSE (lfnres)


! Subroutine ends here
! --------------------
  RETURN
END SUBROUTINE wttrpsnx

END MODULE
