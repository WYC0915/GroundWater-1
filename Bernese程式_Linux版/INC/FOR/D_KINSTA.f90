! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  MODULE d_kinSta

! -------------------------------------------------------------------------
! Purpose:    This module defines the data types for the kinematic station
!             coordinates
!
! Author:     R. Dach
!
! Created:    24-Jun-2003
! Last mod.:  24-Jun-2003
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
  USE d_datum, ONLY: t_datum

! Type for kinematic data set
! ---------------------------
  TYPE t_kin
    REAL(r8b)                                :: xyzEpo  ! Epoch
    REAL(r8b),DIMENSION(3)                   :: xyz     ! Position
    CHARACTER(LEN=staFlagLength)             :: kinFlg  ! Flag
  END TYPE t_kin

! Type for kinematic stations
! ---------------------------
  TYPE t_kSta
    CHARACTER(staNameLength)                 :: staNam  ! Station name
    INTEGER(i4b)                             :: nEpo    ! Number of epochs
    TYPE(t_kin), DIMENSION(:),POINTER        :: kin     ! Kin data
  END TYPE t_kSta

! Station list with kinematic records
! -----------------------------------
  TYPE t_kinSta
    CHARACTER(LEN=80)                        :: title   ! file title
    TYPE(t_datum)                            :: datum   ! Geodetic datum
    REAL(r8b)                                :: refEpo  ! reference epoch
    INTEGER(i4b)                             :: nSta    ! Number of stations
    TYPE(t_kSta), DIMENSION(:),POINTER       :: sta     ! List of stations
  END TYPE t_kinSta

! Initialize structure
! --------------------
  CONTAINS

  SUBROUTINE init_kinSta(kinSta)
    TYPE(t_kinSta)  :: kinSta

    kinSta%nSta=0
    NULLIFY(kinSta%sta)
  END SUBROUTINE init_kinSta

  SUBROUTINE init_kSta(kSta)
    TYPE(t_kSta)    :: kSta

    kSta%nEpo = 0
    NULLIFY(kSta%kin)
  END SUBROUTINE init_kSta

END MODULE d_kinSta


