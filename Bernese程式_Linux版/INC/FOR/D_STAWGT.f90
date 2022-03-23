
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE d_stawgt

! -------------------------------------------------------------------------
! Purpose:    This module defines station weight records
!
! Author:     R. Dach
!
! Created:    10-Jun-2002
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
  USE m_time, ONLY: t_timint

! Station weight data record
! --------------------------
  TYPE t_wgt
    CHARACTER(LEN=staNameLength)    :: staNam  ! Station name
    INTEGER(i4b)                    :: meaTyp  ! Measurement type
                                               ! (1,2,3, see meaStr)
    REAL(r8b)                       :: weight  ! Weight relative to 1d0
    TYPE(t_timint)                  :: timwin  ! Validity
  END TYPE t_wgt

! Station weight structure
! ------------------------
  TYPE t_stawgt
    CHARACTER(LEN=shortLineLength)  :: title   ! File title
    INTEGER(i4b)                    :: nWgt    ! Number of records
    TYPE(t_wgt),DIMENSION(:),POINTER:: wgt     ! List of records
  END TYPE t_stawgt

! Initialize structure
! --------------------
CONTAINS

  SUBROUTINE init_stawgt(stawgt)
    TYPE(t_stawgt)  :: stawgt

    NULLIFY(stawgt%wgt)
    stawgt%nWgt = 0
  END SUBROUTINE init_stawgt

END MODULE d_stawgt

