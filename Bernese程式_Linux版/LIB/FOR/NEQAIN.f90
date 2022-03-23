MODULE s_NEQAIN
CONTAINS

! --------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! --------------------------------------------------------------------------

SUBROUTINE neqain (nfiles, dir, files)

! --------------------------------------------------------------------------
! Purpose: Read option input file for program NEQ2ASC
!
! PARAMETERS :
!        OUT : NFILES : NUMBER OF INPUT FILES          INTEGER
!              DIR    : DIRECTION OF CONVERSION        INTEGER
!                         1 ...   BIN --> ASCII
!                         2 ... ASCII --> BIN
!              FILES  : ARRAY OF INPUT/OUTPUT FILES    CHARAKTER
!
! SR CALLED  : readkeys, exitrc, prflna
!
! Author     : M. Meindl
!
! Created    : 15-Feb-2001
!
! Changes    : 26-Jun-2001 RD: Use alcerr for allocation
!              22-Nov-2001 HU: Call prfile
!              21-Dec-2001 HU: Use m_bern
!              25-Sep-2002 HU: Remove i_astlib
!              23-Apr-2003 AJ: Nullify local pointers
!
! Copyright  : Astronomical Institute
!              University of Bern
!              Switzerland
!---------------------------------------------------------------------------

  USE m_bern

  USE s_alcerr
  USE s_prflna
  USE s_gtfile
  USE s_prfile
  USE s_readkeys
  USE s_exitrc
  IMPLICIT NONE

!
! List of Parameters
! ------------------
  INTEGER (i4b)                                            :: nfiles
  INTEGER (i4b)                                            :: dir
  CHARACTER (LEN=fileNameLength), DIMENSION (:,:), POINTER :: files

!
! Local Parameters
! ----------------
  CHARACTER (LEN=keyValueLength), DIMENSION (:), POINTER :: keyValue
  INTEGER (i4b)                             :: irc      ! return code
  INTEGER (i4b)                             :: iac      ! allocation status
  INTEGER (i4b)                             :: nfil     ! # of files
  INTEGER (i4b)                             :: irCode=0

  NULLIFY(keyValue)

!
! Direction of Conversion
! -----------------------
  CALL readkeys ('RADIO_1', keyValue, irc)
  irCode=irCode+irc
  IF (keyValue(1)=='1') THEN
    dir=1
  ELSE
    dir=2
  ENDIF

!
! Get Input/Output Files
! ----------------------
    IF (dir==1) THEN
      CALL readkeys('INPN2A',keyValue,irc)
      IF (irc == 0) THEN
        nfil=SIZE(keyValue)
        ALLOCATE(files(2,nfil), stat=iac)
        CALL alcerr(iac, 'files', (/2,nfil/), 'neqain')
        CALL GTFILE ('INPN2A', 2, nfil, nfiles, files)
      ELSE
        nfiles=0
      ENDIF
    ELSE
      CALL readkeys('INPA2N',keyValue,irc)
      IF (irc == 0) THEN
        nfil=SIZE(keyValue)
        ALLOCATE(files(2,nfil), stat=iac)
        CALL alcerr(iac, 'files', (/2,nfil/), 'neqain')
        CALL GTFILE ('INPA2N', 2, nfil, nfiles, files)
      ELSE
        nfiles=0
      ENDIF
    ENDIF

    IF (nfiles==0) THEN
      WRITE (lfnerr, '(/,A)') ' *** SR NEQAIN: NO INPUT FILES SELECTED '
      CALL exitrc (2)
    ENDIF

!
! Stop if Error Occured
! ---------------------
  IF (irCode/=0) CALL exitrc (2)

!
! Print Used Files
! ----------------
  CALL prflna

!
! Print Processed Files in Output
! -------------------------------

  IF (dir==1) THEN
    CALL prfile('INPN2A',' ',2)
  ELSE
    CALL prfile('INPA2N',' ',2)
  ENDIF

  IF (dir==1) THEN
    WRITE (lfnprt,"(I5,' file(s) converted from BIN to ASCII.',/)") nfiles
  ELSE
    WRITE (lfnprt,"(I5,' file(s) converted from ASCII to BIN.',/)") nfiles
  ENDIF

!
! The End
! -------
  DEALLOCATE(keyValue,stat=iac)

  RETURN

END SUBROUTINE neqain

END MODULE
