MODULE f_lincount
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

FUNCTION linCount(keyWord, nulLine, nComment)

! -------------------------------------------------------------------------
! Purpose:    keyWord has to refer a formatted file name.
!             The function gives the number of lines in this file back.
!
!             "numLine" header lines are red before the counting starts.
!             The counter stops if the first empty line was found.
!
! Author:     R. Dach
!
! Created:    06-Sep-2001
! Last mod.:  02-Feb-2011
!
! Changes:    17-Sep-2001 RD: keyWord may also be a file name
!             25-Sep-2002 HU: Remove i_astlib
!             02-Feb-2011 RD: Counting footer lines added
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE s_opnfil
  USE s_inquire
  USE s_opnerr
  USE s_gtflna
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)           :: keyWord      ! keyWord for file name
  INTEGER(i4b)               :: nulLine      ! number of header lines

! output:
  INTEGER(i4b)               :: linCount     ! number of lines in file
  INTEGER(i4b), OPTIONAL     :: nComment     ! Number of comment lines behind
                                             ! the last data line

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength) :: filnam
  CHARACTER(LEN=LineLength)     :: line

  INTEGER(i4b)                  :: lineNum
  INTEGER(i4b)                  :: iLine
  INTEGER(i4b)                  :: iData
  INTEGER(i4b)                  :: irc, ios

  LOGICAL                       :: yes

! Init the variables
! ------------------
  ios = 0
  lineNum = 0
  IF (PRESENT(nComment)) nComment = 0

! Open the file
! -------------
  CALL gtflna(0,keyWord,filnam,irc)
  IF (irc /= 0) THEN

! is keyWord a file name?
! -----------------------
    CALL INQUIRE(FILE=keyWord , EXIST=yes)

    IF (yes) THEN
      filnam = keyWord

! String keyWord is unknown
! -------------------------
    ELSE
      linCount = 0
      RETURN
    ENDIF

  ENDIF

  CALL opnfil(lfnloc,filnam,'OLD','FORMATTED','READONLY',' ',irc)
  CALL opnerr(lfnerr, lfnloc,irc,filnam,'linCount')

! Read the header lines
! ---------------------
  DO iLine = 1, nulLine
    READ(lfnloc,'(A)',iostat=ios) line
  ENDDO

! Get the number of lines in the file
! -----------------------------------
  iData = 1
  DO WHILE (ios == 0)
    READ(lfnloc,'(A)',iostat=ios) Line

    IF (LEN_TRIM(line) == 0) iData = 0    ! stop reading at the first empty line

    IF (ios == 0) THEN
      IF ( iData == 1 ) THEN              ! count number of data lines
        lineNum = lineNum + 1
      ELSEIF (PRESENT(nComment)) THEN     ! count number of comment lines
        nComment = nComment + 1
      ELSE                                ! no more data (no counting of
        EXIT                              ! comment lines)
      ENDIF
    ENDIF
  ENDDO

! Close the file
! --------------
  CLOSE(lfnloc)

! Give the result
! ---------------
  linCount = lineNum

  RETURN
END FUNCTION linCount

END MODULE
