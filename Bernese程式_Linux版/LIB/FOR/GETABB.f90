MODULE s_GETABB
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE getabb(staStr,strTyp,abbrev,nAbb,abbIdx)

! -------------------------------------------------------------------------
! Purpose:    Find the corresponding abbreviation record to a string
!             which represents a station name, 4-ID, or 2-ID
!
! Author:     R. Dach
!
! Created:    14-Mar-2003
! Last mod.:  03-Nov-2003
!
! Changes:    02-Jun-2003 RD: Update also an empty abbrev record
!             03-Nov-2003 RD: No update in this SR anymore
!
! SR called:  gtflna,readabb,updabb
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_abbrev, ONLY: t_abbrev

  USE s_alcerr
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)              :: staStr       ! Station string which is either
                                                ! a station name or an abbrev.
  INTEGER(i4b)                  :: strTyp       ! Interpretation of staStr:
                                                ! 1: station name
                                                ! 2: 4-ID of a station
                                                ! 3: 2-ID of a station
  TYPE(t_abbrev)                :: abbrev       ! Abbreviation record

! output:
  INTEGER(i4b)                  :: nAbb         ! Number of records for staStr
  INTEGER(i4b),                  &
       DIMENSION(:),    POINTER :: abbIdx       ! Index in abbreviation record

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6),PARAMETER    :: srName = 'getabb'

! Local Variables
! ---------------
  INTEGER(i4b)                  :: iAbb,jAbb
  INTEGER(i4b)                  :: irc


! Is there a record for this station string?
! ------------------------------------------
  nAbb = 0
  DO iAbb = 1,abbrev%nAbb

    IF ((strTyp == 1 .AND. staStr == abbrev%abb(iAbb)%staNam) .OR. &
        (strTyp == 2 .AND. staStr == abbrev%abb(iAbb)%staab4) .OR. &
        (strTyp == 3 .AND. staStr == abbrev%abb(iAbb)%staab2)) THEN

      nAbb = nAbb + 1

    ENDIF

  ENDDO

! Nothing was found, sorry
! ------------------------
  IF (nAbb == 0) RETURN


! Allocate the output record
! --------------------------
  DEALLOCATE(abbIdx,stat=irc)

  ALLOCATE(abbIdx(nAbb),stat=irc)
  CALL alcerr(irc,'abbIdx',(/nAbb/),srName)

  abbIdx(:) = 0

! Copy the result records
! -----------------------
  jAbb = 0
  DO iAbb = 1,abbrev%nAbb

    IF ((strTyp == 1 .AND. staStr == abbrev%abb(iAbb)%staNam) .OR. &
        (strTyp == 2 .AND. staStr == abbrev%abb(iAbb)%staab4) .OR. &
        (strTyp == 3 .AND. staStr == abbrev%abb(iAbb)%staab2)) THEN

      jAbb = jAbb + 1
      abbIdx(jAbb) = iAbb

    ENDIF

  ENDDO


  RETURN
END SUBROUTINE getabb

END MODULE
