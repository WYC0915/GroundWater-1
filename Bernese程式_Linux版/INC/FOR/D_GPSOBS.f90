! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  MODULE d_gpsobs

! -------------------------------------------------------------------------
! Purpose:    This module defines the data types for the GPS-observations
!
! Author:     L. Mervart
!
! Created:    07-Mar-2000
! Last mod.:  26-Jan-2011
!
! Changes:    28-AUG-2000 HB: rename m_rdhead into d_gpsobs and
!                             t_head into t_obshead, remove lenFil because
!                             fileNameLength is already PARAMETER in m_bern
!             02-SEP-2000 HU: t_obsrec added
!             03-SEP-2000 HU: elements with dimensions maxsat, maxamb, maxfrq
!                             declared as pointers and allocated outside.
!                             t_obsepo added
!             10-Jul-2002 HB: define new t_sta, t_sat => new t_obsHead
!                             modify t_obsRec and t_obsEpo
!             24-Feb-2003 RD: DELTAT in t_obsRec must be a real
!             13-May-2003 CU: Nullify pointers
!             08-Sep-2003 HU: recnam, antnam, oprnam chr16 -> chr20
!             26-Jan-2011 LP: Satellite-specific observation types (in t_sat)
!             24-Apr-2012 LP: dimension of obstyp in t_sat changed 4->8
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_time,   ONLY : t_timInt
  USE d_ambigu, ONLY : t_ambigu

! Type for stations
! -----------------
  TYPE t_sta
    CHARACTER(LEN=staNameLength)             :: staNam  ! Station Names
    CHARACTER(LEN=20)                        :: oprNam  ! Operator Name
    CHARACTER(LEN=20)                        :: recTyp  ! Receiver Types
    INTEGER(i4b)                             :: irUnit  ! Rec. Unit Numbers
    CHARACTER(LEN=20)                        :: antTyp  ! Antenna  Types
    INTEGER(i4b)                             :: iAnten  ! Rec. Antenna Numbers
    REAL(r8b),DIMENSION(3)                   :: posEcc  ! Eccentricities
    INTEGER(i4b)                             :: iClock  ! Type of Clock Par.
  END TYPE t_sta

! Type for satellites
! -------------------
  TYPE t_sat
    INTEGER(i4b)                             :: numSat  ! Satellite Number
    INTEGER(i4b),DIMENSION(2)                :: numObs  ! Number of Obs. Used
    INTEGER(i4b),DIMENSION(2)                :: numMrk  ! Number of Obs. Marked
    CHARACTER(LEN=3),DIMENSION(8)            :: obstyp  ! Observation types
  END TYPE t_sat

! Whole observation header
! ------------------------
  TYPE t_obsHead
    CHARACTER(LEN=16)                        :: campgn  ! Campaign Name
    CHARACTER(LEN=53)                        :: title   ! File Title
    INTEGER(i4b)                             :: irMark  ! Remark Number
    INTEGER(i4b)                             :: iFrmat  ! File Format Number

    CHARACTER(LEN=4) ,DIMENSION(2)           :: cSess   ! Session Definition
                                                        ! (1): Session
                                                        !      identifier
                                                        ! (2): File ident. of
                                                        !      the same session
    CHARACTER(LEN=9) ,DIMENSION(2)           :: crDate  ! (1): Creation Date
                                                        ! (2): Modification Date
    CHARACTER(LEN=5) ,DIMENSION(2)           :: crTime  ! (1): Creation Time
                                                        ! (2): Modification Time
    REAL(r8b)                                :: timRef  ! Reference Epoch
    INTEGER(i4b)                             :: iDeltt  ! Observation Interval
    INTEGER(i4b)                             :: meaTyp  ! Measurement Type
                                                        ! 1: Phase Observations
                                                        ! 2: Code Observations
                                                        ! 3: Range observations
    INTEGER(i4b)                             :: nDiff   ! Number of Differences
                                                        ! 0: Zero-Differences
                                                        ! 1: Single Differences
    INTEGER(i4b)                             :: nFreq   ! Number of Frequencies
                                                        ! 1: L1 Observations
                                                        ! 2: L1 and L2
                                                        !    Observations
    INTEGER(i4b)                             :: nEpoch  ! Number of Observation
    INTEGER(i4b)                             :: numAmb  ! Number of Ambiguities
    INTEGER(i4b)                             :: nSatel  ! Number of Satellites
    INTEGER(i4b)                             :: nEpFlg  ! Num. of Flag. Epochs

    TYPE(t_sta),DIMENSION(2)                 :: sta
    TYPE(t_sat),DIMENSION(:),POINTER         :: sat
    TYPE(t_ambigu),DIMENSION(:),POINTER      :: ambigu
  END TYPE t_obshead


! Additional Header Information Generated in Programs
! ---------------------------------------------------
  TYPE t_obsInfo
    TYPE(t_timInt)                 :: window  ! data window
    INTEGER(i4b)                   :: filNum  ! file number
    INTEGER(i4b)                   :: nFrFil  ! nbr of requested frq
    INTEGER(i4b),DIMENSION(2)      :: iCarr   ! requested frequencies
  END TYPE t_obsInfo

! Observation record
! ------------------
  TYPE t_obsRec
    INTEGER(i4b)                   :: numSat  ! Satellite number (PRN)
    CHARACTER(LEN=1),DIMENSION(2)  :: obsFlg  ! Observation flags
    REAL(r8b),DIMENSION(2)         :: observ  ! Observations
  END TYPE t_obsRec

! Observation Records for one Epoch
! ---------------------------------
  TYPE t_obsEpo
    REAL(r8b)                           :: obsTim   ! Observation time
    CHARACTER(LEN=1)                    :: epoFlg   ! Epoch flag
    REAL(r8b),DIMENSION(2)              :: deltat   ! Receiver clock correction
    INTEGER(i4b)                        :: nSat     ! Number of satellites in epoch
    TYPE(t_obsRec),DIMENSION(:),POINTER :: obsRec   ! Observation record
  END TYPE t_obsEpo

! Initialize structure
! --------------------
CONTAINS

  SUBROUTINE init_obshead(obsHead)
    TYPE(t_obsHead)  :: obsHead

    NULLIFY(obsHead%sat)
    NULLIFY(obsHead%ambigu)
  END SUBROUTINE init_obshead

  SUBROUTINE init_obsepo(obsEpo)
    TYPE(t_obsEpo)    :: obsEpo

    NULLIFY(obsEpo%obsRec)
  END SUBROUTINE init_obsepo

END MODULE d_gpsobs


