
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE d_sess

! -------------------------------------------------------------------------
! Purpose:    This module defines structures and global variables for
!             Bernese session table
!
! Author:     R. Dach
!
! Created:    11-Apr-2003
! Last mod.:  11-Apr-2003
!
! Changes:    __-___-____ __:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_time,  ONLY: t_timint

! Type for a station record
! -------------------------
  TYPE t_sess
    CHARACTER(LEN=4)                     :: sessID  ! Session ID
    TYPE(t_timint)                       :: timint  ! Session time window
  END TYPE t_sess

! Type for abbreviation data
! --------------------------
  TYPE t_sesLst
    LOGICAL                              :: fix     ! Abbreviation file title
    INTEGER(i4b)                         :: nSess   ! Number of records
    TYPE(t_sess),   DIMENSION(:),POINTER :: sess    ! Abbreviation records
  END TYPE t_sesLst

! Initialize structure
! --------------------
CONTAINS

  SUBROUTINE init_sesLst(sesLst)
    TYPE(t_sesLst)   :: sesLst

    NULLIFY(sesLst%sess)
    sesLst%nSess = 0
  END SUBROUTINE init_sesLst

END MODULE d_sess
