MODULE s_WTBIASSNX
CONTAINS

! --------------------------------------------------------------------------
! Bernese GNSS Software
! --------------------------------------------------------------------------

SUBROUTINE wtbiassnx(dcbFil,nSmpNq)

! --------------------------------------------------------------------------
! Purpose:    Write a BIAS SINEX file (for DCB, ISB, IFB); ICD by Tim Springer
!
! Author:     L. Prange
!
! Created:    07-Jul-2011
!
! Changes:    21-Jul-2011 LP: Add obstypes to dcbFil%dcbLst; introduce b_dcbtyp2
!             06-Jun-2012 LP: Use svn2chr as module now
!
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! --------------------------------------------------------------------------


! Modules
! -------
  USE m_bern
  USE m_time,   ONLY: t_timint
  USE d_dcbFil, ONLY: t_dcbFil,b_dcbtyp2

  USE s_opnfil
  USE s_sinini
  USE s_clocks
  USE f_djul
  USE s_opnerr
  USE s_sindat
  USE s_exitrc
  USE f_iyear4
  USE s_gtflna
  USE s_svn2chr
  IMPLICIT NONE


! List of parameters
! ------------------
  TYPE(t_dcbFil)                :: dcbFil
  INTEGER(i4b)                  :: nSmpNq  ! Data sampling rate
  CHARACTER(LEN=fileNameLength) :: filNam  ! file name, blank: use GTFLNA

! output
  INTEGER(i4b)                  :: irCode  ! Return code of this SR
                                           ! = 1: No filename specified,
                                           !      file not written
! Local Variables
! ---------------
  CHARACTER(LEN=fileNameLength)         :: filSnx
  CHARACTER(LEN=80)                     :: line, linhed
  INTEGER(i4b), DIMENSION(8)            :: actTim
  REAL(r8b)                             :: tHead, actDay

  CHARACTER(LEN=1)                      :: obsCod, constraintCode
  CHARACTER(LEN=3)                      :: sinExt, solCon
  CHARACTER(LEN=3)                      :: agency, datOwn
  CHARACTER(LEN=12)                     :: timStrAct, timStrMin, timStrMax
  REAL(r8b)                             :: sinVer
  INTEGER(i4b)                          :: iSampl

! error codes
  INTEGER(i4b)                          :: irc, ioStat
  REAL(r8b)                             :: biasRes
  INTEGER(i4b)                          :: iPar
  CHARACTER(LEN=4)                      :: biasType
  CHARACTER(LEN=4)                      :: SVN
  CHARACTER(LEN=3)                      :: PRN
  INTEGER(i4b)                          :: satnum
  CHARACTER(LEN=4)                      :: obst1, obst2, unit
  INTEGER(i4b)                          :: iWrite
  TYPE(t_timint)                        :: maxWin

! Some initializations
! --------------------
  irCode = 0
  irc    = 0
  filNam = ""

! Open file
! ----------
  CALL gtflna(0,'BIASSNX',filNam,irc)
  IF (LEN_TRIM(filNam)==0 .OR. irc/=0) THEN
    irCode = 1
    RETURN
  END IF

  CALL opnfil(lfnres,filNam,'UNKNOWN',' ', ' ',' ',ioStat)
  CALL opnerr(lfnerr,lfnres,ioStat,filNam,'sr wtbiassnx')


! Get some settings from SINEX input file
! ---------------------------------------
  CALL sinini(agency,datOwn)
  sinExt = 'BIA'
  sinVer = 0.01
  obsCod = 'P'
  constraintCode ='2'
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
!  CALL rdpwin('TRPOFF',(/' ',' '/),t_0,dt_0)
!  CALL readKeys('TRPRES',keyValue,irc)
!  CALL ckoptt(1,'TRPRES',keyValue,'sr wttrpsnx',                           &
!              'Time resolution for troposphere SINEX file',irc,irCode,     &
!              gt=0.d0,maxVal=1,result1=trpRes)
!  dt_0 = dt_0-trpRes/2.d0

! get maximum window
!  maxWin%t(1) = 1.d20
!  maxWin%t(2) = 0.d0
!  DO iSta=1,trpEst%nSta
!    DO iTrp=1,trpEst%sta(iSta)%nTrp
!      hlpTim = trpEst%sta(iSta)%trp(iTrp)%timInt(1)
!      IF (hlpTim==0.d0) CYCLE
!      IF (hlpTim<maxWin%t(1)) maxWin%t(1) = hlpTim
!      IF (hlpTim>maxWin%t(2)) maxWin%t(2) = hlpTim
!    END DO
!  END DO

! get intervals
!  nTimInt = 0
!  CALL parint(maxWin,dtMax,t_0,dt_0,trpRes,                                &
!              'Time resolution for trop SINEX',nTimInt,trpWin)

! Preliminary: As long as biases are estimated on a daily basis
!  helpDay   = actTim(3)+actTim(5)/24.d0+actTim(6)/1440.d0+actTim(7)/86400.d0
!  helptmin  = djul(actTim(1),actTim(2),actDay)
!  helpDay   = actTim(3)+actTim(5)/24.d0+actTim(6)/1440.d0+actTim(7)/86400.d0
!  helptmin  = djul(actTim(1),actTim(2),actDay)

! get max and min time
!  CALL sindat(0,maxWin%t(1),timStrMin)
!  CALL sindat(0,maxWin%t(2),timStrMax)
  DO ipar = 1,dcbFil%nVal
     maxWin%t(1) = 1.d20
     maxWin%t(2) = 0.d0
     IF (dcbFil%dcbLst(ipar)%tim%t(1)==0.d0) CYCLE
     IF (dcbFil%dcbLst(ipar)%tim%t(1)<maxWin%t(1)) maxWin%t(1) = dcbFil%dcbLst(ipar)%tim%t(1)
     IF (dcbFil%dcbLst(ipar)%tim%t(2)>maxWin%t(2)) maxWin%t(2) = dcbFil%dcbLst(ipar)%tim%t(2)
  ENDDO

  CALL sindat(0,maxWin%t(1),timStrMin)
  CALL sindat(0,maxWin%t(2),timStrMax)

! compute parameter sampling rate (seconds)
  biasRes = 24.d0
  iSampl = nint(biasRes*3600.d0)


! Write header line
! -----------------
  write(lfnres,"(A2,A3,1X,F4.2,1X,A3,1X,A12,1X,A3,2(1X,A12),1X,A1,1X,I5.5,1X,A1,1X,A4)") &
               "%=",sinExt,sinVer,agency,timStrAct,datOwn,                 &
               timStrMin,timStrMax,obsCod,dcbFil%nVal,constraintCode,solCon


! Write reference block
! ---------------------
  CALL gtflna(1,'SINEXIN',filSnx,irc)
  CALL opnfil(lfnloc,filSnx,'OLD','FORMATTED','READONLY',' ',ioStat)
  CALL opnerr(lfnerr,lfnloc,ioStat,filSnx,'sr wtbiassnx')
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
    WRITE(lfnerr,"(/,' *** SR WTBIASSNX: Error reading SINEX input file', &
                &  /,'                  File: ',A,/)") TRIM(filSnx)
    CALL exitrc(2)
  ENDIF

! Write bias description block
! ----------------------------
  write(lfnres,"('*',79('-'))")
  write(lfnres,"(A17)") "+BIAS/DESCRIPTION"
  write(lfnres,"(A40,A40)") "*_________KEYWORD_____________ __VALUE(S",    &
                          ")_______________________________________"
  write(lfnres,"(A30,1X,I22)")                                             &
               " DAT SAMPLING INTERVAL (sec):",nSmpNq
  write(lfnres,"(A30,1X,I5.5)")                                             &
               " PAR SAMPLING INTERVAL (sec): ",iSampl
  write(lfnres,"(A30,1X,A5)")                                             &
               " TBD 1:                       ","TBD 1"
  write(lfnres,"(A30,1X,A5)")                                             &
               " TBD 2:                       ","TBD 2"
  write(lfnres,"(A30,1X,A5)")                                           &
               " TBD 3:                       ","TBD 3"
  write(lfnres,"(A17)") "-BIAS/DESCRIPTION"


! Write BIAS/SOLUTION block
! -------------------------
  write(lfnres,"('*',79('-'))")
  write(lfnres,"(A14)") "+BIAS/SOLUTION"
  write(lfnres,"(A104)") "*BIAS SVN_ PRN SITE DOMES____ OBS1 OBS2 BIAS_START__ BIAS_END____ UNIT __ESTIMATED_VALUE____ _STD_DEV___"

  DO ipar = 1,dcbFil%nVal

!     biasType = "    "
     biasType(1:3) = b_dcbtyp2(dcbFil%dcbLst(ipar)%typ)
     biasType(4:4) = " "
     obst1(1:3)    = dcbFil%dcbLst(ipar)%obstyp(1)
     obst2(1:3)    = dcbFil%dcbLst(ipar)%obstyp(2)
     obst1(4:4)    = " "
     obst2(4:4)    = " "


!     obst1    = "    "
!     obst2    = "    "
!     if (dcbFil%dcbLst(ipar)%typ.eq.1) then
!         biasType = "P1P2"
!         biasType = "DCB "b_dcbtyp2
!         obst1 = "C1P "
!         obst2 = "C2P "
!     elseif (dcbFil%dcbLst(ipar)%typ.eq.2) then
!         biasType = "P1C1"
!         biasType = "DCB "
!         obst1 = "C1P "
!         obst2 = "C1C "
!     elseif (dcbFil%dcbLst(ipar)%typ.eq.3) then
!         biasType = "ISB "
!     elseif (dcbFil%dcbLst(ipar)%typ.eq.4) then
!         biasType = "P2C2"
!         biasType = "DCB "
!         obst1 = "C2P "
!         obst2 = "C2C "
!     elseif (dcbFil%dcbLst(ipar)%typ.eq.5) then
!         biasType = "IFB "
!         obst1 = "L1P "
!         obst2 = "L2P "
!     endif

     SVN(1:1) = dcbFil%dcbLst(ipar)%sys
     PRN(1:1) = dcbFil%dcbLst(ipar)%sys
     if (dcbFil%dcbLst(ipar)%svn.eq.0) then
        SVN(2:4)= "   "
        PRN(2:3)= "  "
     else
        WRITE(SVN(2:4),'(I3.3)') dcbFil%dcbLst(ipar)%svn
        CALL SVN2CHR(dcbFil%dcbLst(ipar)%svn,satnum,PRN(1:1))
        WRITE(PRN(2:3),'(I2.2)') satnum
        SVN(1:1) = PRN(1:1)
     endif

     unit = "ns  "

     WRITE(lfnres,"(1X,A4,1X,A4,1X,A3,1X,A14,2(1X,A4),2(1X,A12),1X,A4,1X,E21.15,1X,E11.6)") &
     biasType,SVN,PRN,dcbFil%dcbLst(ipar)%staNam,obst1,obst2,timStrMin, &
     timStrMax,unit,dcbFil%dcbLst(ipar)%dcbVal,dcbFil%dcbLst(ipar)%dcbRms
  ENDDO


  write(lfnres,"(A14)") "-BIAS/SOLUTION"
  write(lfnres,"(A8)") "%=ENDBIA"


! Close file
! ----------
    CLOSE (lfnres)


! Subroutine ends here
! --------------------
  RETURN
END SUBROUTINE wtbiassnx

END MODULE
