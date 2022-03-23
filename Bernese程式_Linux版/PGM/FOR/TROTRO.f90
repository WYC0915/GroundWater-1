! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

PROGRAM trotro

! -------------------------------------------------------------------------
! Purpose:    Remove stations marked in stacrux from troposphere sinex file
!
! Author:     U.Hugentobler
!
! Created:    18-Sep-2001
!
! Changes:    16-Dec-2001 HU: Use implicit none
!             25-Sep-2002 HU: Remove i_astlib
!             15-May-2003 AJ: Initialize structure
!             16-Sep-2003 RD: STACRUX->STAINFO
!             23-Sep-2003 RD: Error if input and output file name are identical
!                             Ignore flags in station information file
!             19-Nov-2003 RD: Read INP-filename in READINPF
!             21-Jun-2005 MM: LFNUM.inc removed (LFNUMs in m_bern)
!             27-Feb-2007 AG: Call DEFCON with parameter
!             23-May-2007 SS: Check whether stanam(5:5) blank
!             23-Sep-2010 RD: Enable CPU counter
!             26-Oct-2010 SL: two trunc. bugs corrected, use m_bern with ONLY
!             01-Dec-2011 SL: new title string for pritit
!             12-Mar-2012 RD: Use SPLSTR as module now
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnErr, lfn001, lfn002, lfnPrt, &
                      fileNameLength
  USE m_cpu,    ONLY: cpu_start
  USE d_inpkey, ONLY: inpKey, init_inpkey
  USE d_stacrx, ONLY: t_stacrux,init_stacrux
  USE s_opnfil
  USE s_prflna
  USE s_readcrux
  USE s_pritit
  USE s_splstr
  USE s_readinpf
  USE s_opnerr
  USE s_sindat
  USE s_defcon
  USE s_exitrc
  USE s_opnsys
  USE s_priwin
  USE s_gtflna

  IMPLICIT NONE
!
! DECLARATIONS
! ------------
  INTEGER(i4b),PARAMETER                :: maxstr=12
  INTEGER(i4b)                          :: irc,iostat,icrx
  INTEGER(i4b)                          :: nline,ipart,iwrite
  INTEGER(i4b)                          :: nout,nsta,ntro

  REAL(r8b)                             :: epoch
  REAL(r8b)                             :: dtend = 1D0   ! add 1min at end
                                                         ! of interval
  REAL(r8b),DIMENSION(2)                :: window

  CHARACTER(LEN=4)                      :: abbr
  CHARACTER(LEN=80)                     :: line
  CHARACTER(LEN=80),DIMENSION(maxstr)   :: substr
  CHARACTER(LEN=fileNameLength)         :: filinp,filnam

  TYPE(t_stacrux)                       :: stacrux


! Start CPU Counter
! -----------------
  CALL cpu_start(.TRUE.)

! Get the name of the input file
! ------------------------------
  CALL init_inpkey(inpKey)
  CALL readinpf(' ',inpKey)

  CALL init_stacrux(stacrux)

! Define system files
! -------------------
  CALL opnsys

! Define constants
! ----------------
  CALL defcon(1)
  dtend=dtend/1440D0

! Program header
! --------------
  CALL pritit ('TROTRO','Manipulate troposphere SINEX files')
  CALL prflna

! Read stacrux file
! -----------------
  CALL gtflna(1,'STAINFO',filnam,irc)
  CALL readcrux(filnam, stacrux)

! Check input/output file name
! ----------------------------
  CALL gtflna(1,'SNXINP',filinp,irc)
  CALL gtflna(1,'SNXOUT',filnam,irc)

  IF (filinp == filnam) THEN
    WRITE(lfnerr,'(/,A,/,16X,A,/)')                                   &
    ' *** PG TROTRO: The names of the input and output troposphere ', &
                    'SINEX files have to be different.'
    CALL exitrc(2)
  ENDIF

! Open files
! ----------
  CALL opnfil(lfn001,filinp,'OLD',' ', ' ',' ',iostat)
  CALL opnerr(lfnerr,lfn001,iostat,filinp,'TROTRO')

  CALL opnfil(lfn002,filnam,'UNKNOWN',' ', ' ',' ',iostat)
  CALL opnerr(lfnerr,lfn002,iostat,filnam,'TROTRO')

! LOOP OVER ENTRIES IN FILE
! -------------------------
  nline=0
  ipart=0
  nsta =0
  ntro =0

  ReadLine: DO
    nline = nline+1

! Read line
! ---------
    READ(lfn001,"(A)",IOSTAT=iostat) line
! error
    IF (iostat /= 0) THEN
      WRITE(lfnerr,"(/,' *** PG TROTRO: Reading error', &
                   & /,'                Line:',I6, &
                   & /,'                File: ',A,/)") nline, TRIM(filinp)
      CALL exitrc(2)
    ENDIF

! end of file
    IF (line == '%=ENDTRO') EXIT ReadLine

! Header Line
! -----------
    IF (nline==1) THEN
      IF (line(1:5) /= '%=TRO') THEN
        WRITE(lfnerr,"(/,' *** PG TROTRO: Input file is not ', &
                                       & 'a troposphere Sinex file', &
                     & /,'                File: ',A,/)") TRIM(filinp)
        CALL exitrc(2)
      ENDIF

! Get time window
      CALL splstr(line,maxstr,' ',nout,substr,irc)
      CALL SINDAT(1,window(1),substr(6))
      CALL SINDAT(2,window(2),substr(7))

      CALL priwin(1,window)

      WRITE(lfnprt,"(' REMOVE STATIONS', &
                 & /,' ---------------',/)")
    ENDIF

! Different parts
! ---------------
    IF (line == '+TROP/DESCRIPTION')     ipart=1
    IF (line == '+TROP/STA_COORDINATES') ipart=2
    IF (line == '+TROP/SOLUTION')        ipart=3
    IF (line(1:1) == '-')                ipart=0
    iwrite=1

! Station Coordinates
! -------------------
    IF (line(1:1) /= '*' .AND. line(1:1) /= '+' .AND. ipart == 2) THEN
      CALL splstr(line,maxstr,' ',nout,substr,irc)
      abbr=substr(1)(1:LEN(abbr))

! Check for station in stacrux
      DO icrx=1,stacrux%nprob
        IF ( stacrux%staprob(icrx)%stanam(1:4)       == abbr      .AND. &
             stacrux%staprob(icrx)%stanam(5:5)       == ' '       .AND. &
             stacrux%staprob(icrx)%timint%t(1)       <= window(1) .AND. &
             stacrux%staprob(icrx)%timint%t(2)+dtend >= window(2)) iwrite=0
      ENDDO

      IF (iwrite==0) nsta=nsta+1
      IF (iwrite==0) WRITE(lfnprt,"(1X,A)") abbr
    ENDIF

! Troposphere Values
! ------------------
    IF (line(1:1) /= '*' .AND. line(1:1) /= '+' .AND. ipart == 3) THEN
      CALL splstr(line,maxstr,' ',nout,substr,irc)
      abbr=substr(1)(1:LEN(abbr))
      CALL SINDAT(1,epoch,substr(2))

! Check for station in stacrux
      DO icrx=1,stacrux%nprob
        IF ( stacrux%staprob(icrx)%stanam(1:4)       == abbr  .AND. &
             stacrux%staprob(icrx)%stanam(5:5)       == ' '   .AND. &
             stacrux%staprob(icrx)%timint%t(1)       <= epoch .AND. &
             stacrux%staprob(icrx)%timint%t(2)+dtend >= epoch) iwrite=0
      ENDDO

      IF (iwrite==0) ntro=ntro+1
    ENDIF

! Write Line
! ----------
    IF (iwrite == 1) WRITE(lfn002,"(A)") TRIM(line)

  ENDDO ReadLine

! Last line
  WRITE(lfn002,"(A)") TRIM(line)

  WRITE(lfnprt,"(//,' NUMBER OF REMOVED ENTRIES', &
               &  /,' -------------------------', &
               & //,' Number of entries in station list:',I6, &
               &  /,' Number of troposphere entries    :',I6,/)") nsta,ntro

! Close files
! -----------
  CLOSE(lfn001)
  CLOSE(lfn002)

  CALL exitrc(0)
END PROGRAM trotro

