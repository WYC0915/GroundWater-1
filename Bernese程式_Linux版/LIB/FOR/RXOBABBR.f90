MODULE s_RXOBABBR
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rxobAbbr(iabupd, session, stanam, filcod, filpha, csess)

! -------------------------------------------------------------------------
!
! Purpose:    Define the Names of Zero-difference Files using the
!             Table of 4-character Abbreviations
!
! Author:     L. Mervart
!
! Created:    04-Aug-2000
! Last mod.:  03-Nov-2003
!
! Changes:    15-Aug-2000  HU: Take MAXSTA from include file
!             13-Dec-2000  RD: Use GTSTAB instead of GTSABN to read abb-table
!             15-Dec-2000  RD: Cosmetics for the error messages
!             30-Aug-2001  HU: MAXSTA not from include file, set to 500
!             05-Sep-2001  HU: Dynamic allocation of arrays
!             30-Sep-2001  HU: Interface of gtstab moved to I_ASTLIB
!             06-Oct-2001  HU: Interface of gtstab moved back to I_GPSLIB
!             26-Oct-2001  RD: Not all meatyps are available
!             17-Feb-2003  HU: Comment modified
!             19-Mar-2003  RD: Update abbreviation table
!             07-May-2003  RD: Handle also the session string
!             16-May-2003  HU: Initialize structure
!             03-Nov-2003  RD: Call SR gtabbv instead of SR getabb
!
! SR called:  gtflna, readabb, gtAbbv, writAbb, exitrc, fparse
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_const,  ONLY: filTitle
  USE d_abbrev, ONLY: t_abbrev, init_abbrev
  USE s_gtabbv
  USE s_readabb
  USE s_inquire
  USE s_writabb
  USE s_fparse
  USE s_exitrc
  USE s_gtflna
  IMPLICIT NONE

! INTEGER(i4b) :: maxsta

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                   :: iabupd  ! Station not in abbreviation table:
                                            ! -1: stop with error
                                            !  0: write warning
                                            !  1: update (big letters only)
                                            !  2: update+ (big/small letters)
  CHARACTER(LEN=4)               :: session ! Session string
  CHARACTER(LEN=*)               :: stanam  ! station name

! input/output:
  CHARACTER(LEN=*), DIMENSION(2) :: filcod  ! code file names
  CHARACTER(LEN=*), DIMENSION(2) :: filpha  ! phase file names

! output:
  CHARACTER(LEN=4), DIMENSION(2) :: csess   ! Session ID in obs. files


! List of functions
! -----------------

! Local Types
! -----------

! Local parameters
! ----------------

! Local Variables
! ---------------
  TYPE(t_abbrev),                       SAVE  :: abbrev

  CHARACTER(LEN=fileNameLength),        SAVE  :: filAbbr
  CHARACTER(LEN=fileNameLength)               :: node    ! used by FPARSE
  CHARACTER(LEN=fileNameLength)               :: device  ! used by FPARSE
  CHARACTER(LEN=fileNameLength)               :: dir     ! used by FPARSE
  CHARACTER(LEN=fileNameLength)               :: name    ! used by FPARSE
  CHARACTER(LEN=fileNameLength)               :: ext     ! used by FPARSE
  CHARACTER(LEN=fileNameLength)               :: ver     ! used by FPARSE

  INTEGER(i4b),    DIMENSION(:), POINTER      :: abbIdx
  INTEGER(i4b)                                :: nAbb0
  INTEGER(i4b)                                :: nAbb
  INTEGER(i4b)                                :: ii
  INTEGER(i4b)                                :: irc

  LOGICAL                                     :: yes
  LOGICAL, SAVE                               :: first = .TRUE.

! Nothing to do
! -------------
  IF (LEN_TRIM(filcod(1))+LEN_TRIM(filpha(1)) == 0) RETURN


! Read Station Name Abbreviations
! -------------------------------
  IF (first) THEN
    first = .FALSE.
    CALL gtflna(1,'ABBREV ', filAbbr, irc)
    CALL init_abbrev(abbrev)
    CALL inquire(file=filAbbr,exist=yes)
    IF (yes) THEN
      CALL readAbb(filAbbr,abbrev)
    ELSE
      abbrev%title = filTitle
    ENDIF
  END IF

! Init variables
! --------------
  nAbb = 0
  NULLIFY(abbIdx)

  nAbb0 = abbrev%nAbb

! Get the abbrevation
! -------------------
  IF (iabupd == -1 .OR. iabupd == 0) THEN

    CALL gtAbbv(0,stanam,1,filAbbr,abbrev,nAbb,abbIdx)

  ELSE IF (iabupd == 1 .OR. iabupd == 2) THEN

    CALL gtAbbv(1+iabupd,stanam,1,filAbbr,abbrev,nAbb,abbIdx)

    ! Write the updated abbrevation list (if necessary)
    IF (nAbb0 /= abbrev%nAbb) CALL writabb(filAbbr, abbrev)

  ! Unknown update mode
  ELSE

    RETURN

  ENDIF

! Error handling
! --------------
  IF (nAbb == 0 .AND. iabupd == -1) THEN
    WRITE(lfnerr,'(/,A,2(/,18X,A,A),/)')                               &
          ' *** SR RXOBABBR: Station not found in abbreviation table', &
                            'Station name:       ', TRIM(stanam),      &
                            'Abbreviation table: ', TRIM(filAbbr)
    CALL exitrc(2)
  ELSE IF (nAbb == 0) THEN
    WRITE(lfnerr,'(/,A,2(/,18X,A,A),/)')                                &
          ' ### SR RXOBABBR: Station not found in abbreviation table',  &
                            'Station name:       ', TRIM(stanam),       &
                            'Abbreviation table: ', TRIM(filAbbr)
  ELSE IF (nAbb > 1) THEN
    WRITE(lfnerr,'(/,A,2(/,18X,A,A),/)')                                &
          ' ### SR RXOBABBR: More than one station abbreviation found', &
                            'Station name:       ', TRIM(stanam),       &
                            'Abbreviation table: ', TRIM(filAbbr)
  ENDIF

! Update the name of the output files
! -----------------------------------
  DO ii = 1, 2

    IF (LEN_TRIM(filcod(ii)) > 0) THEN

      CALL fparse(1,filcod(ii),node,device,dir,name,ext,ver,irc)

      IF (irc == 0) THEN

        IF (nAbb /= 0 .AND. LEN_TRIM(session) > 0) THEN
          name = abbrev%abb(abbIdx(1))%staab4 // session // '.'

        ELSE IF (nAbb /= 0 .AND. LEN_TRIM(session) == 0) THEN
          name(1:4) = abbrev%abb(abbIdx(1))%staab4

        ELSE IF (nAbb == 0 .AND. LEN_TRIM(session) > 0) THEN
          name(5:8) = session // '.'
        ENDIF

        filcod(ii) = TRIM(node) // TRIM(device) // TRIM(dir) // &
                     TRIM(name) // TRIM(ext)    // TRIM(ver)
      ENDIF

      ! Set session IDs
      csess(1) = name(5:8)
      csess(2) = name(8:8)

    ENDIF

    IF (LEN_TRIM(filpha(ii)) > 0) THEN

      CALL fparse(1,filpha(ii),node,device,dir,name,ext,ver,irc)

      IF (irc == 0) THEN

        IF (nAbb /= 0 .AND. LEN_TRIM(session) > 0) THEN
          name = abbrev%abb(abbIdx(1))%staab4 // session // '.'

        ELSE IF (nAbb /= 0 .AND. LEN_TRIM(session) == 0) THEN
          name(1:4) = abbrev%abb(abbIdx(1))%staab4

        ELSE IF (nAbb == 0 .AND. LEN_TRIM(session) > 0) THEN
          name(5:8) = session // '.'
        ENDIF

        filpha(ii) = TRIM(node) // TRIM(device) // TRIM(dir) // &
                     TRIM(name) // TRIM(ext)    // TRIM(ver)
      ENDIF

      ! Set session IDs
      csess(1) = name(5:8)
      csess(2) = name(8:8)

    ENDIF
  ENDDO

  DEALLOCATE(abbIdx,stat=irc)

  RETURN
END SUBROUTINE rxobAbbr

END MODULE
