
! ------------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! ------------------------------------------------------------------------------

MODULE d_stalst

! ------------------------------------------------------------------------------
! Purpose:    This module defines structures for station list/sigma files
!
! Author:     R. Dach
!
! Created:    23-Aug-2001
! Last mod.:  13-May-2003
!
! Changes:    13-May-2003 CU: Nullify pointers
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! ------------------------------------------------------------------------------

  USE m_bern

! Structures
! ----------
TYPE t_staList
  CHARACTER(LEN=shortlinelength) :: title    ! Title line
  INTEGER(i4b)                   :: nSta     ! Number of stations in list
  CHARACTER(LEN=staNameLength),   &
      DIMENSION(:),   POINTER    :: staNam   ! List of the stations
  REAL(r8b),                      &
      DIMENSION(:,:), POINTER    :: sigma    ! Sigmas for the stations
END TYPE t_staList

! Initialize structure
! --------------------
CONTAINS

  SUBROUTINE init_stalist(stalist)
    TYPE(t_staList)  :: staList

    NULLIFY(staList%staNam)
    NULLIFY(staList%sigma)
    staList%nSta = 0
  END SUBROUTINE init_stalist

END MODULE d_stalst
