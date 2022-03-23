! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  PROGRAM getkey

! -------------------------------------------------------------------------
! Purpose:    Gives the contents of a keyword in a new input file
!
! Syntax:     "echo PGMNAM.INP KEYWORD [FILNAM|FILEXT] | getkey"
!             the result will be given into the stdout
!             the facultative 3rd parameter can be used for file names
!                FILNAM : the file name without path and withour extension
!                FILEXT : the file name without path but with extension
!                else the full entry is given back
!
! Author:     R. Dach
!
! Created:    09-Nov-2000
! Last mod.:  23-Sep-2010
!
! Changes:    18-Dec-2001 HU: Use implicit none
!             16-Aug-2002 LM: Use Mygetarg
!             23-Apr-2003 HU: Nullify local pointers
!             29-Apr-2003 RD: Replace environment variables
!             02-Jun-2003 RD: More correct error handling
!             27-Jun-2003 PF: preproc for win32/lf95
!             07-Jul-2003 PF: special handling for win32/lf95 empty string
!             18-Aug-2003 RD: Set "program_Name" variable
!             22-Sep-2003 RD: Finish program with EXITRC(0)
!             29-Oct-2003 RD: Preprocessor flag IO_BLANK
!             19-Nov-2003 RD: Read INP-filename in READINPF
!             21-Jun-2005 MM: LFNUM.inc removed (LFNUMs in m_bern)
!             19-Jul-2010 SL: tab characters removed
!             23-Sep-2010 RD: Enable CPU counter
!
! SR used:    readkeys, findwd, tstkey, readinpf
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE m_cpu,    ONLY: cpu_start
  USE d_inpkey, ONLY: inpKey, init_inpkey
  USE s_mygetarg
  USE s_findwd
  USE s_readinpf
  USE s_readkeys
  USE s_rplenvar
  USE s_fparse
  USE s_exitrc
  USE f_tstkey

  IMPLICIT NONE

  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue
  CHARACTER(LEN=lineLength)                 :: inpLine  ! input from STDIN
  CHARACTER(LEN=lineLength)                 :: inpFilNam ! Name of the input
                                                         ! file
  CHARACTER(LEN=keyNameLength)              :: KeyWord  ! Keyword string
  CHARACTER(LEN=6)                          :: inParam  ! additional input
                                                        ! param.
  CHARACTER(LEN=fileNameLength)             :: hlpNode
  CHARACTER(LEN=fileNameLength)             :: hlpDevice
  CHARACTER(LEN=fileNameLength)             :: hlpDir
  CHARACTER(LEN=fileNameLength)             :: hlpName
  CHARACTER(LEN=fileNameLength)             :: hlpExt
  CHARACTER(LEN=fileNameLength)             :: hlpVer

  INTEGER(i4b)                              :: i1,i2    ! Counter
  INTEGER(i4b)                              :: iVal     ! Counter
  INTEGER(i4b)                              :: iError   ! Error code
  INTEGER(i4b)                              :: irc      ! Return code
!
! Start CPU Counter
! -----------------
  CALL cpu_start(.FALSE.)

! Nullify pointers
! ----------------
  NULLIFY(keyValue)
  CALL init_inpkey(inpKey)

  program_Name = 'GETKEY'

  iError=0
  LFNERR=6

  inpFilNam = ' '
  keyword   = ' '
  inParam   = ' '
!
! Read the input line
! -------------------
  CALL mygetarg(inpLine)
!
! Get the name of the input file (1st parameter)
! ----------------------------------------------
  CALL findwd (inpLine,1,i1,i2)
  IF (i1 > 0) inpFilNam = inpLine(i1:i2)

!
! Get the keyword (2nd parameter)
! -------------------------------
  CALL findwd (inpLine,2,i1,i2)
  IF (i1 > 0) keyword = inpLine(i1:i2)
!
! Get the keyword (2nd parameter)
! -------------------------------
  CALL findwd (inpLine,3,i1,i2)
  IF (i1 > 0) inParam = inpLine(i1:i2)

  IF (LEN_TRIM(inpFilNam) <= 1 .OR. LEN_TRIM(keyword) <= 1) iError = 1

  IF (iError == 0) THEN
    CALL readinpf(TRIM(inpFilNam),inpKey)
    CALL readKeys(TRIM(keyword), keyValue, irc)

    IF (irc /= 0) iError = 2
  ENDIF

  IF (iError == 0) THEN
    IF (LEN_TRIM(inParam)>0) THEN
      IF (inParam(1:3) == 'FIL') THEN
        DO iVal=1,SIZE(keyValue)
          CALL fparse(0,keyValue(iVal),hlpNode,hlpDevice,hlpDir,hlpName,hlpExt,hlpVer,irc)
          IF (irc == 0) THEN
            IF (inParam == 'FILNAM') keyValue(ival)=TRIM(hlpName)
            IF (inParam == 'FILEXT') keyValue(ival)=TRIM(hlpName)//'.'//TRIM(hlpExt)
          ENDIF
        ENDDO
      ENDIF

    ENDIF

    ! Try to replace environment variables
    IF (TSTKEY('ENVIRONMENT')) THEN
      DO iVal=1,SIZE(keyValue)
        CALL rplenvar(0,keyValue(ival))
      ENDDO
    ENDIF

  ENDIF

!
! Give back error code or write out the result
! --------------------------------------------
  IF (iError /= 0) THEN
!
    IF (iError == 1) THEN
      WRITE(*,*) 'ERROR in syntax.'
      WRITE(*,*) '      Usage: "echo PGMNAM.INP KEYWORD [FILNAM|FILEXT] | getkey" '
      WRITE(*,*) '             the result will be given into the stdout'
      WRITE(*,*) '             the facultative 3rd parameter can be used for file names'
      WRITE(*,*) '                FILNAM : the file name without path and withour extension'
      WRITE(*,*) '                FILEXT : the file name without path but with extension'
      WRITE(*,*) '                else the full entry is given back'
    ENDIF
!
    CALL exitrc(2)
  ELSE
    DO iVal=1,SIZE(keyValue)

#ifdef IO_BLANK
      IF (LEN_TRIM(keyValue(iVal))==0) THEN
        WRITE(*,'(A)') TRIM(keyValue(iVal))
      ELSE
        WRITE(*,'(1X,A)') TRIM(keyValue(iVal))
      ENDIF
#else
      WRITE(*,'(A)') TRIM(keyValue(iVal))
#endif

    ENDDO
  ENDIF

  CALL exitrc(0)

  END PROGRAM getkey
