
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

PROGRAM sta2sta

! ----------------------------------------------------------------------------
! Purpose:    Conversion of different STAINFO file formats.
!
! Author:     S.Lutz
!
! Created:    05-Oct-2010
!
! Changes:    26-Oct-2010 SL: truncation bug corrected
!             02-Dec-2011 SL: new title string for pritit, IMPLICIT NONE
!             01-Feb-2012 SL: enable CPU counter
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! ----------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, keyValueLength, fileNameLength80
  USE m_cpu,    ONLY: cpu_start
  USE d_stacrx, ONLY: t_stacrux, init_stacrux
  USE d_inpkey, ONLY: inpKey, init_inpkey
  USE d_const,  ONLY: filtitle
  USE s_readinpf
  USE s_opnsys
  USE s_pritit
  USE s_prflna
  Use s_ckoptb
  USE s_gtflna
  USE s_readcrux
  USE s_rdstax
  USE s_readkeys
  USE s_writcrux
  USE s_writcrux000
  USE s_wtstax
  USE s_wtstax000
  USE s_exitrc
  IMPLICIT NONE

! Parameters
! ----------
  CHARACTER(LEN=7), PARAMETER    :: pgName = 'STA2STA'

! Variables
! ---------
  CHARACTER(LEN=keyValueLength), &
    DIMENSION(:), POINTER        :: keyValue
  TYPE(t_stacrux)                :: stacrux
  CHARACTER(LEN=fileNameLength80):: stafile1, stafile2
  INTEGER(i4b)                   :: irc
  INTEGER(i4b)                   :: iType
  CHARACTER(LEN=3)               :: fid

! Initializations
! ---------------
  NULLIFY(keyValue)
  CALL init_stacrux(stacrux)
  CALL init_inpkey(inpKey)
  iType = 0
  fid = ''

! Start CPU counter
! -----------------
  CALL cpu_start(.TRUE.)

! Get the name of the input file
! ------------------------------
  CALL readinpf('',inpKey)

! Define system files
! -------------------
  CALL opnsys

! Write title and file list
! -------------------------
  CALL pritit(pgName,'Convert station information files')
  CALL prflna

! Read Stacrux File
! -----------------
  CALL ckoptb(1,(/'RADIO_S','RADIO_C'/),pgName, &
              'Selection for type of RINEX input files',irc, &
              result1=iType)
  IF(iType == 1) THEN
    CALL gtflna(0,'STAFILE1',stafile1,irc)
    CALL readcrux(stafile1,stacrux)
    fid = 'STA'
  ELSEIF(iType == 2) THEN
    CALL gtflna(0,'CRXFILE1',stafile1,irc)
    CALL rdstax(stafile1,stacrux)
    fid = 'CRX'
  ENDIF
  IF(fid == '') CALL exitrc(2)

! TYPE 002 technique
! ------------------
  CALL readkeys('TYP2TECH',keyValue,irc)
  IF(irc == 0) stacrux%technique = TRIM(keyValue(1))

! Write Stacrux File
! ------------------
  IF(fid == 'STA') CALL gtflna(1,'STAFILE2',stafile2,irc)
  IF(fid == 'CRX') CALL gtflna(1,'CRXFILE2',stafile2,irc)
  IF(irc /= 0) CALL exitrc(2)

  CALL readkeys('OUTFORM',keyValue,irc)
  IF(irc == 0) THEN
    IF(keyValue(1) == '0.00') THEN
      IF(fid == 'STA') CALL writcrux000(stafile2,stacrux,filtitle)
      IF(fid == 'CRX') CALL wtstax000(stafile2,stacrux,filtitle)
    ELSE
      IF(fid == 'STA') CALL writcrux(stafile2,stacrux,filtitle)
      IF(fid == 'CRX') CALL wtstax(stafile2,stacrux,filtitle)
    ENDIF
  ENDIF

! Exit program
! ------------
  CALL exitrc(0)

END PROGRAM sta2sta
