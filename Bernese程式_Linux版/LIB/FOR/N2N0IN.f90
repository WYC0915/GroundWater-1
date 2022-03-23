MODULE s_N2N0IN
CONTAINS

! --------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! --------------------------------------------------------------------------

SUBROUTINE n2n0in (nfiles, files)

! --------------------------------------------------------------------------
! Purpose: Read option input file for program NEQ2NQ0
!
! PARAMETERS :
!        OUT : NFILES : NUMBER OF INPUT FILES          INTEGER
!              FILES  : ARRAY OF INPUT/OUTPUT FILES    CHARAKTER
!
! SR CALLED  : readkeys, exitrc, prflna
!
! Author     : M. Meindl
!
! Created    : 16-Feb-2001
!
! Changes    : 26-Jun-2001 RD: Use alcerr for allocation
!              21-Dec-2001 HU: Use m_bern
!              25-Sep-2002 HU: Remove i_astlib
!              23-Apr-2003 AJ: Nullify local pointers
!              25-Aug-2003 CU: Change format strings for output
!
! Copyright  : Astronomical Institute
!              University of Bern
!              Switzerland
!---------------------------------------------------------------------------

  USE m_bern

  USE s_alcerr
  USE s_prflna
  USE s_gtfile
  USE s_readkeys
  USE s_exitrc
  IMPLICIT NONE

!
! List of Parameters
! ------------------
  INTEGER (i4b)                                             :: nfiles
  CHARACTER (LEN=fileNameLength), DIMENSION (:,:), POINTER  :: files

!
! Local Parameters
! ----------------
  CHARACTER (LEN=keyValueLength), DIMENSION (:), POINTER :: keyValue
  INTEGER (i4b)                             :: nfil     ! # of files
  INTEGER (i4b)                             :: iFil     ! file counter
  INTEGER (i4b)                             :: irc      ! return code
  INTEGER (i4b)                             :: iac      ! allocation status
  INTEGER (i4b)                             :: irCode=0

  NULLIFY(keyValue)

!
! Get Input/Output Files
! ----------------------
  CALL readkeys ('INPFIL', keyValue, irc)
    IF (irc==0) THEN
      nfil=SIZE (keyValue)
      ALLOCATE (files (2, nfil), STAT=iac)
      CALL alcerr(iac, 'files', (/2, nfil/), 'n2n0in')
      CALL GTFILE ('INPFIL', 2, nfil, nfiles, files)
    ELSE
      nfiles=0
    ENDIF

    IF (nfiles==0) THEN
      WRITE (lfnerr, '(/,A)') ' *** SR N2N0IN: NO INPUT FILES SELECTED '
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
  WRITE(LFNPRT,'(1X,79("-"),/,A,/,1X,79("-"))')          &
    ' File  Input files                        Output files'

  DO iFil=1, nfiles
    WRITE (LFNPRT, '(1X,I4,2X,A32,3X,A32)') iFil,files(1,iFil),files(2,iFil)
  ENDDO

  WRITE (LFNPRT,'(1X,79("-"),//,1X,I4,A)')   &
    nfiles,' file(s) converted from NEQ to NQ0.'


!
! The End
! -------
  DEALLOCATE(keyValue,stat=iac)

  RETURN

END SUBROUTINE n2n0in

END MODULE
