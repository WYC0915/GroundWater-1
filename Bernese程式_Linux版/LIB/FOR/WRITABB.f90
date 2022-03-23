MODULE s_WRITABB
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE writAbb(abbFil,abbrev)

! -------------------------------------------------------------------------
! Purpose:    Write the content of station abbreviation file
!
! Author:     R. Dach
!
! Created:    14-Mar-2003
! Last mod.:  14-Mar-2003
!
! Changes:    __-___-____ __:
!
! SR called:  opnfil, opnerr
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_abbrev, ONLY: t_abbrev

  USE s_opnfil
  USE s_opnerr
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)              :: abbFil               ! Name of the file
  TYPE(t_abbrev)                :: abbrev               ! Abbreviations

! output:

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=7),PARAMETER    :: srName = 'writabb'

! Local Variables
! ---------------
  INTEGER(i4b)                  :: iAbb
  INTEGER(i4b)                  :: ios


! Nothing to do
! -------------
  IF (LEN_TRIM(abbfil) == 0) RETURN

! Open the file for writing
! -------------------------
  CALL opnfil(lfnloc,abbfil,'UNKNOWN','FORMATTED',' ',' ',ios)
  CALL opnerr(lfnerr,lfnloc,ios,abbfil,srName)

! Write the header of the file
! ----------------------------
  WRITE(lfnloc,'(2(A,/),2(/,A))')                         &
          TRIM(abbrev%title),                             &
          '----------------------------------------' //   &
          '----------------------------------------',     &
          'Station name             4-ID    2-ID   ' //   &
          ' Remark                                 ',     &
          '****************         ****     **    ' //   &
          ' ***************************************'

! Write the abbreviation records
! ------------------------------
  DO iAbb = 1,abbrev%nAbb

    WRITE(lfnloc,'(A16,4X,5X,A4,5X,A2,5X,A)')                  &
            abbrev%abb(iAbb)%staNam, abbrev%abb(iAbb)%staAb4,  &
            abbrev%abb(iAbb)%staAb2, abbrev%abb(iAbb)%remark

  ENDDO

  CLOSE(lfnloc)

  RETURN
END SUBROUTINE writabb

END MODULE
