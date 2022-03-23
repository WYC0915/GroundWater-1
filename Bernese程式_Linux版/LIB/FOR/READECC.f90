MODULE s_READECC
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE readEcc(eccFil,eccent)

! -------------------------------------------------------------------------
! Purpose:    Read the content of Bernese Eccentricity file
!
! Author:     R. Dach
!
! Created:    07-May-2002
! Last mod.:  30-Jul-2002
!
! Changes:    30-Jul-2002 HU: Use interface for alcerr
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
  USE d_eccent, ONLY: t_eccent

  USE s_alcerr
  USE s_opnfil
  USE f_lincount
  USE s_opnerr
  USE s_exitrc
  USE s_gtflna
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)              :: eccFil              ! Name of the file
                                                       ! if empty use ECCENT

! output:
  TYPE(t_eccent)                :: eccent               ! Eccentricities

! Local Parameters
! ----------------
  CHARACTER(LEN=7),PARAMETER    :: srName = 'readecc'

! Local Variables
! ---------------
  CHARACTER(LEN=1)              :: chrTyp

  INTEGER(i4b)                  :: nEcc
  INTEGER(i4b)                  :: ii
  INTEGER(i4b)                  :: ios,iac,irc


! Init the data records
! ---------------------
  eccent%title      = ' '
  eccent%datum%name = ' '
  eccent%nEcc       = 0

! Get the filename (if not given)
! -------------------------------
  IF (LEN_TRIM(eccfil) == 0) THEN
    CALL gtflna(0,'ECCENT',eccfil,irc)
    IF (irc /= 0) eccfil = ' '
  ENDIF

! Nothing to do
! -------------
  IF (LEN_TRIM(eccfil) == 0) RETURN

! Allocate the data record
! ------------------------
  nEcc = linCount(eccfil,6)

  DEALLOCATE(eccent%ecc,stat=iac)

  ALLOCATE(eccent%ecc(nEcc),stat=iac)
  CALL alcerr(iac,'eccent%ecc',(/nEcc/),srName)

  eccent%nEcc = nEcc

  eccent%ecc(:)%staNum = 0
  eccent%ecc(:)%staNam = ' '
  eccent%ecc(:)%cenNam = ' '
  DO ii = 1,3
    eccent%ecc(:)%xEccen(ii) = 0d0
  ENDDO

! Open the file for reading
! -------------------------
  CALL opnfil(lfnloc,eccfil,'OLD','FORMATTED','READONLY',' ',ios)
  CALL opnerr(lfnerr,lfnloc,ios,eccfil,srName)

! Read the header of the file
! ---------------------------
  READ(lfnloc,'(A,//,22X,A16,12X,A1,///)',iostat=ios) &
          eccent%title,eccent%datum%name, chrTyp

  IF (ios /= 0) THEN
    WRITE(lfnerr,'(/,A,/,17X,A,/)')                                   &
    ' *** SR READECC: Error reading header of the eccentricity file', &
                     'File name:  ' // TRIM(eccfil)
    CALL exitrc(2)
  ENDIF

! Get the eccent. type
! --------------------
  eccent%eccTyp = 2
  IF (chrTyp /= 'G' .AND. chrTyp /= 'g') eccent%eccTyp = 1

  nEcc = 0
  DO WHILE (nEcc < eccent%nEcc)

    nEcc = nEcc+1
    READ(lfnloc,'(I3,2X,A16,2X,A16,3F11.4)',iostat=ios)             &
         eccent%ecc(nEcc)%staNum, eccent%ecc(nEcc)%staNam,          &
         eccent%ecc(nEcc)%cenNam,(eccent%ecc(nEcc)%xEccen(ii),ii=1,3)

    IF (ios /= 0) THEN
      WRITE(lfnerr,'(/,A,/,17X,A,/,17X,A,I6,/)')                         &
      ' *** SR READECC: Error reading records of the eccentricity file', &
                       'File name:  ' // TRIM(eccfil),                   &
                       'Line number:' ,  nEcc
      CALL exitrc(2)
    ENDIF

    IF(eccent%ecc(nEcc)%staNam == eccent%ecc(nEcc)%cenNam) THEN
      WRITE(lfnerr,'(/,A,2(/,17X,2A),/)')                                &
      ' *** SR READECC: Eccenter and center may not have the same name', &
                       'File name:    ',TRIM(eccfil),                    &
                       'Station name: ',TRIM(eccent%ecc(nEcc)%cenNam)
      CALL exitrc(2)
    ENDIF
  ENDDO

  CLOSE(lfnloc)

  RETURN
END SUBROUTINE readecc

END MODULE
