
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

PROGRAM snx2sta

! ----------------------------------------------------------------------------
! Purpose:    This program converts station info of selected stations from
!             SINEX file into a STACRUX file
!             The selection text file contains only the 4-character-IDs of the
!             selected stations
!
! Author:     D.Lenhart
!
! Created:    19-Feb-2003
!
! Changes:    02-May-2003 PS: Program output, call to SRs selectsta and selrenam
!             19-May-2003 DL: Conditional call to SR selsta
!             28-May-2003 PS: Corrected format if no selection file used
!             30-Aug-2003 HU: Use GTFLNA, call defcon, use filtitle
!             19-Nov-2003 RD: Read INP-filename in READINPF
!             21-Jun-2005 MM: LFNUM.inc removed (LFNUMs in m_bern)
!             09-Aug-2005 SS: Information reduction
!             27-Feb-2007 AG: Call DEFCON with parameter
!             22-Mar-2007 AG: Initialization of stacrux and stacrux2
!             20-Nov-2008 DT: Technique-specific Type 002 (TYP2TECH)
!             23-Sep-2010 RD: Enable CPU counter
!             04-Oct-2010 SL: use m_bern with ONLY, pgName, STAINFO vers. 1.01
!                             get technique from obs.type in SINEX
!             06-Oct-2010 RD: Exitrc added at the end
!             01-Dec-2011 SL: some cosmetics
!             07-Dec-2011 SL: New title string for pritit
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! ----------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, keyValueLength, fileNameLength
  USE m_cpu,    ONLY: cpu_start
  USE d_inpkey, ONLY: inpKey, init_inpkey
  USE d_const,  ONLY: filtitle
  USE d_stacrx, ONLY: t_stacrux, init_stacrux
  USE s_selrenam
  USE s_prflna
  USE s_selsta
  USE s_pritit
  USE s_readinpf
  USE s_readkeys
  USE s_rigssnx
  USE s_writcrux
  USE s_defcon
  USE s_opnsys
  USE s_gtflna
  USE s_ckoptl
  USE s_ckoptb
  USE s_exitrc
  IMPLICIT NONE

! Parameters
! ----------
  CHARACTER(LEN=7), PARAMETER    :: pgName = 'SNX2STA'

! Local variables
! ---------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER  :: keyValue
  CHARACTER(LEN=fileNameLength)  ::  snxfile
  CHARACTER(LEN=fileNameLength)  ::  selfile
  CHARACTER(LEN=fileNameLength)  ::  stafile
  TYPE(t_stacrux)                ::  stacrux , stacrux2

  INTEGER(i4b)                   ::  irc, irCode
  CHARACTER(LEN=3)               ::  flag1, flag2
  INTEGER(i4b)                   ::  ircsel
  INTEGER(i4b), DIMENSION(3)     ::  redInf

  NULLIFY(keyValue)
  CALL init_stacrux(stacrux)
  CALL init_stacrux(stacrux2)

! Start CPU Counter
! -----------------
  CALL cpu_start(.TRUE.)

! Get the name of the input file
! ------------------------------
  CALL init_inpkey(inpKey)
  CALL readinpf(' ',inpKey)

! Define system files
! -------------------
  CALL opnsys
  CALL defcon(1)
  CALL pritit(pgName,'Extract station information from SINEX file')
  CALL prflna

! Read Input Options
! ------------------
  irCode = 0
  CALL gtflna(1,'SNXFILE',snxfile,irc)
  CALL gtflna(0,'SELFILE',selfile,ircsel)
  CALL gtflna(1,'STAFILE',stafile,irc)

  CALL readkeys('FLG1',keyValue,irc)
  CALL ckoptl(1,'FLG1',keyValue,pgName,'Flag 001',irc,irCode, &
              empty='000',maxLength=3,result1=flag1)

  CALL readkeys('FLG2',keyValue,irc)
  CALL ckoptl(1,'FLG2',keyValue,pgName,'Flag 002',irc,irCode, &
              empty='000',maxLength=3,result1=flag2)

  CALL ckoptb(1,(/'REDRSN'/),pgName,'Disregard receiver serial numbers', &
              irCode,result1=redInf(1))
  IF(irCode /= 0) CALL exitrc(2)

  CALL ckoptb(1,(/'REDASN'/),pgName,'Disregard antenna serial numbers', &
              irCode,result1=redInf(2))
  IF(irCode /= 0) CALL exitrc(2)

  CALL ckoptb(1,(/'REDARC'/),pgName,'Disregard antenna radome codes', &
              irCode,result1=redInf(3))
  IF(irCode /= 0) CALL exitrc(2)

! Read SINEX File
! ---------------
  CALL rigssnx(snxfile,stacrux,redInf)

! Select stations from file
! -------------------------
  IF(ircsel == 0) THEN
    CALL selsta(selfile,stacrux,stacrux2,flag2)
  ELSE
    stacrux2 = stacrux
    stacrux2%stainfo(:)%flg = flag2
  END IF
  CALL selrenam(stacrux2,flag1)

! Write Stacrux File
! ------------------
  CALL writcrux(stafile,stacrux2,filtitle)

! Exit program
! ------------
  CALL exitrc(0)

END PROGRAM snx2sta
