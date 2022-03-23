MODULE s_READABB
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE readAbb(abbFil,abbrev)

! -------------------------------------------------------------------------
! Purpose:    Read the content of station abbreviation file
!
! Author:     R. Dach
!
! Created:    14-Mar-2003
! Last mod.:  16-May-2003
!
! Changes:    16-May-2003 HU: Do not nullify abbrev
!
! SR called:  alcerr, gtflna, exitrc, opnfil, opnerr, linCount
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_abbrev, ONLY: t_abbrev

  USE s_exitrc
  USE s_alcerr
  USE s_opnfil
  USE f_lincount
  USE s_gtflna
  USE s_opnerr
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)              :: abbFil              ! Name of the file
                                                       ! if empty use ABBREV

! output:
  TYPE(t_abbrev)                :: abbrev               ! Abbreviations

! Local Parameters
! ----------------
  CHARACTER(LEN=7),PARAMETER    :: srName = 'readabb'

! Local Variables
! ---------------
  INTEGER(i4b)                  :: nAbb
  INTEGER(i4b)                  :: ios,iac,irc


! Init the data records
! ---------------------
  abbrev%title      = ' '
  abbrev%nAbb       = 0

! Get the filename (if not given)
! -------------------------------
  IF (LEN_TRIM(abbfil) == 0) THEN
    CALL gtflna(0,'ABBREV',abbfil,irc)
    IF (irc /= 0) abbfil = ' '
  ENDIF

! Nothing to do
! -------------
  IF (LEN_TRIM(abbfil) == 0) RETURN

! Allocate the data record
! ------------------------
  nAbb = linCount(abbfil,5)

  DEALLOCATE(abbrev%abb,stat=iac)

  ALLOCATE(abbrev%abb(nAbb),stat=iac)
  CALL alcerr(iac,'abbrev%abb',(/nAbb/),srName)

  abbrev%nAbb = nAbb

  abbrev%abb(:)%staNam = ' '
  abbrev%abb(:)%staAb4 = ' '
  abbrev%abb(:)%staAb2 = ' '
  abbrev%abb(:)%remark = ' '

! Open the file for reading
! -------------------------
  CALL opnfil(lfnloc,abbfil,'OLD','FORMATTED','READONLY',' ',ios)
  CALL opnerr(lfnerr,lfnloc,ios,abbfil,srName)

! Read the header of the file
! ---------------------------
  READ(lfnloc,'(A,////)',iostat=ios) abbrev%title

  IF (ios /= 0) THEN
    WRITE(lfnerr,'(/,A,/,17X,A,/)')                                   &
    ' *** SR READABB: Error reading header of the abbreviation file', &
                     'File name:  ' // TRIM(abbfil)
    CALL exitrc(2)
  ENDIF

! Read the abbreviation records
! -----------------------------
  nAbb = 0
  DO WHILE (nAbb < abbrev%nAbb)

    nAbb = nAbb+1
    READ(lfnloc,'(A16,4X,5X,A4,5X,A2,5X,A)',iostat=ios)        &
            abbrev%abb(nAbb)%staNam, abbrev%abb(nAbb)%staAb4,  &
            abbrev%abb(nAbb)%staAb2, abbrev%abb(nAbb)%remark

    IF (ios /= 0) THEN
      WRITE(lfnerr,'(/,A,/,17X,A,/,17X,A,I6,/)')                         &
      ' *** SR READABB: Error reading records of the abbreviation file', &
                       'File name:  ' // TRIM(abbfil),                   &
                       'Line number:' ,  nAbb
      CALL exitrc(2)
    ENDIF

  ENDDO

  CLOSE(lfnloc)

  RETURN
END SUBROUTINE readabb

END MODULE
