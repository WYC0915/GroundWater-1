
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE d_stdorb

! -------------------------------------------------------------------------
! Purpose:    This module defines structures and global variables for
!             the standard orbit files.
!
! Author:     U. Hugentobler
!
! Created:    23-Jun-2001
! Last mod.:  13-May-2003
!
! Changes:    13-May-2003 CU: Nullify pointers
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern

! Type for standard orbit header
! ------------------------------
  TYPE t_stdhead
    INTEGER(i4b)                        :: ifrmt  ! Format number
    INTEGER(i4b)                        :: narc   ! Number of arcs
    INTEGER(i4b)                        :: iorsys ! Orbit system,
                                                  !  1: B1950, 2: J2000
    TYPE(t_stdarc),DIMENSION(:),POINTER :: arc    ! Arc information
  END TYPE t_stdhead

! Type for arc information
! ------------------------
  TYPE t_stdarc
    INTEGER(i4b)                        :: nsat   ! Number of satellites
    INTEGER(i4b)                        :: nint   ! Number of intervals
    INTEGER(i4b)                        :: degr   ! Polynomial degree
    REAL(r8b)                           :: tosc   ! Ssculation epoch
    REAL(r8b),DIMENSION(2)              :: tbound ! First/last epoch
    CHARACTER(LEN=10)                   :: source ! Source info
    TYPE(t_stdsat),DIMENSION(:),POINTER :: sat    ! Satellite information
  END TYPE t_stdarc

! Type for satellite information
! ------------------------------
  TYPE t_stdsat
    INTEGER(i4b)                        :: svn    ! Satellite number
  END TYPE t_stdsat

! Initialize structure
! --------------------
CONTAINS

  SUBROUTINE init_stdhead(stdhead)
    TYPE(t_stdhead) :: stdhead

    NULLIFY(stdhead%arc)
    stdhead%narc = 0
  END SUBROUTINE init_stdhead

END MODULE d_stdorb
