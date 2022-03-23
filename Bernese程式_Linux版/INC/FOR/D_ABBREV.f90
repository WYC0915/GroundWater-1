
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE d_abbrev

! -------------------------------------------------------------------------
! Purpose:    This module defines structures and global variables for
!             station abbreviations (4- and 2-characters)
!
! Author:     R. Dach
!
! Created:    14-Mar-2003
! Last mod.:  13-May-2003
!
! Changes:    13-May-2003 CU: Nullify pointers
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern

! Type for a station record
! -------------------------
  TYPE t_abbsta
    CHARACTER(LEN=staNameLength)         :: stanam  ! Station name
    CHARACTER(LEN=4)                     :: staab4  ! 4-chr abbreviation
    CHARACTER(LEN=2)                     :: staab2  ! 2-chr abbreviation
    CHARACTER(LEN=39)                    :: remark  ! Remark
  END TYPE t_abbsta

! Type for abbreviation data
! --------------------------
  TYPE t_abbrev
    CHARACTER(LEN=lineLength)            :: title   ! Abbreviation file title
    INTEGER(i4b)                         :: nAbb    ! Number of records
    TYPE(t_abbsta), DIMENSION(:),POINTER :: abb     ! Abbreviation records
  END TYPE t_abbrev

! Initialize structure
! --------------------
CONTAINS

  SUBROUTINE init_abbrev(abbrev)
    TYPE(t_abbrev) :: abbrev

    NULLIFY(abbrev%abb)
    abbrev%nAbb = 0
  END SUBROUTINE init_abbrev

END MODULE d_abbrev
