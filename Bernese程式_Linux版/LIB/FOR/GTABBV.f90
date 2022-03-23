MODULE s_GTABBV
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE gtabbv(abbopt,staStr,strTyp,abbFil,abbrev,nAbb,abbIdx)

! -------------------------------------------------------------------------
! Purpose:    Find the corresponding abbreviation record to a string
!             which represents a station name, 4-ID, or 2-ID
!             Update the list, if it is requested...
!
! Author:     R. Dach
!
! Created:    14-Mar-2003
! Last mod.:  03-Nov-2003
!
! Changes:    02-Jun-2003 RD: Update also an empty abbrev record
!             03-Nov-2003 RD: Call getabb separately
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

  USE s_updabb
  USE s_getabb
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                  :: abbopt       ! Abbreviation update options
                                   ! 0: No update allowed
                                   ! 1: Use only first chr for abbrev
                                   ! 2: Use all chr of the station name
                                   ! 3: Use also small letters
  CHARACTER(LEN=*)              :: staStr       ! Station string which is either
                                                ! a station name or an abbrev.
  INTEGER(i4b)                  :: strTyp       ! Interpretation of staStr:
                                                ! 1: station name
                                                ! 2: 4-ID of a station
                                                ! 3: 2-ID of a station
  CHARACTER(LEN=*)              :: abbFil       ! Name of abbreviation file

! input/output:
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
  CHARACTER(LEN=6),PARAMETER    :: srName = 'gtabbv'

! Local Variables
! ---------------
  INTEGER(i4b)                  :: irc


! Update abbreviation list (if necessary)
! ---------------------------------------
  IF (strTyp == 1 .AND. abbopt > 0) &
    CALL updAbb(abbopt,staStr,abbFil,abbrev,irc)

! Get all records for this station string?
! ----------------------------------------
  CALL getabb(staStr,strTyp,abbrev,nAbb,abbIdx)


  RETURN
END SUBROUTINE gtabbv

END MODULE
