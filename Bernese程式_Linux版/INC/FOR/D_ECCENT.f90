! ------------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! ------------------------------------------------------------------------------

MODULE d_eccent

! ------------------------------------------------------------------------------
! Purpose:    This module defines structures for the Eccentricity File
!
! Author:     R. Dach
!
! Created:    07-May-2002
! Last mod.:  13-May-2003
!
! Changes:    27-Feb-2003 HU: DATUM from D_DATUM
!             13-May-2003 CU: Nullify pointers
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! ------------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_datum, ONLY: t_datum

! Parameter
! ---------
  CHARACTER(LEN=10),DIMENSION(2),PARAMETER :: eccTyp =  &
                    (/ 'LOCAL     ','GEOCENTRIC' /)

! Structures
! ----------
!
! Structure for Eccent. file record
! ---------------------------------
  TYPE t_ecc
    INTEGER(i4b)                 :: stanum
    CHARACTER(LEN=staNameLength) :: staNam
    CHARACTER(LEN=staNameLength) :: cenNam
    REAL(r8b), DIMENSION(3)      :: xEccen
  END TYPE t_ecc

! Structure for Eccentricity files
! --------------------------------
  TYPE t_eccent
    CHARACTER(LEN=80)                   :: title  ! Title for Edit Info File

    INTEGER(i4b)                        :: eccTyp ! Type (see parameter)

    TYPE(t_datum)                       :: datum  ! Datum for eccent.

    INTEGER(i4b)                        :: nEcc   ! Number of entries
    TYPE(t_ecc), DIMENSION(:),POINTER   :: ecc    ! Eccent records
  END TYPE t_eccent

! Initialize structure
! --------------------
CONTAINS

  SUBROUTINE init_eccent(eccent)
    TYPE(t_eccent)   :: eccent

    NULLIFY(eccent%ecc)
    eccent%nEcc = 0
  END SUBROUTINE init_eccent

END MODULE d_eccent
