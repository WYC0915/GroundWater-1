MODULE s_BTSTIN
CONTAINS

! --------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! --------------------------------------------------------------------------

SUBROUTINE btstin (filbrd, nfil, nfilout, mfil, radiob, maxfil)

! --------------------------------------------------------------------------
! Purpose: Read option input file and compose filenames for program BRDTST
!
! PARAMETERS :
!        OUT : FILBRD  : INPUT FILENAME(S)       CHARACTER(LEN=fileNameLength)
!              NFIL    : NUMBER OF INPUT FILES   INTEGER
!              NFILOUT : OUTPUT FILENAME         CHARACTER(LEN=fileNameLength)
!              MFIL    : NUMBER OF OUTPUT FILES  INTEGER
!              RADIOB  : STATUS OF RADIOBUTTON   LOGICAL
!              MAXFIL  : MAXIMUM NUMBER OF FILES INTEGER
!
! SR CALLED  : readkeys, exitrc
!
! Author     : M. Meindl
!
! Created    : 13-Feb-2001
! Last mod.  : 23-Apr-2003
!
! Changes    : 25-Sep-2002 HU: Remove i_astlib
!              23-Apr-2003 CU: Nullify local pointers
!
! Copyright  : Astronomical Institute
!              University of Bern
!              Switzerland
!---------------------------------------------------------------------------

  USE m_bern

  USE s_prflna
  USE s_readkeys
  USE s_exitrc
  IMPLICIT NONE

!
! List of Parameters
! ------------------
  INTEGER :: nfil, mfil, maxfil
  CHARACTER(LEN=fileNameLength) filbrd(2,maxfil),nfilout(maxfil)
  LOGICAL :: radiob

!
! Local Parameters
! ----------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue
  INTEGER(i4b)                        :: irc            ! return code
  INTEGER(i4b)                        :: irCode=0       ! return code


  NULLIFY(keyValue)

!
! Read Status of Radiobutton
! --------------------------
  CALL readkeys ('RADIO_2', keyValue, irc)
  irCode=irCode+irc
  IF (irc==0) radiob=(keyvalue(1) == '1')

!
! Compose Filename-Matrix FILBRD
! ------------------------------
  IF (.NOT. radiob) THEN
    filbrd (2, :)=' '
  ELSE IF (mfil==0) THEN
    filbrd (2, :)=filbrd (1, :)
  ELSE IF (mfil==nfil) THEN
    filbrd (2, :)=nfilout (:)
  ELSE IF (mfil/=nfil) THEN
    WRITE (lfnerr, '(/,A,A,2(/,16X,A,I5),/)')                  &
           ' *** PG BRDTST: DIFFERENT NUMBER OF INPUT AND ',   &
                           'OUTPUT FILES SELECTED',            &
                           'NUMBER OF INPUT FILES:  ',NFIL,    &
                           'NUMBER OF OUTPUT FILES: ',MFIL
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
! The End
! -------
  DEALLOCATE(keyValue,stat=irc)

  RETURN
END SUBROUTINE btstin

END MODULE
