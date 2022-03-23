
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE d_trpest

! -------------------------------------------------------------------------
! Purpose:    This module defines structures and global variables for
!             file TRPEST containting estimated troposphere parameters
!
! Author:     U. Hugentobler
!
! Created:    10-Jun-2001
! Last mod.:  08-Oct-2010
!
! Changes:    15-May-2003 MM: init_trpest added
!             30-Jun-2008 RD: VMF added
!             08-Oct-2010 RD: Parameter undef_Trp added
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern

  INTEGER(i4b), PARAMETER        :: undef_Trp = -999999


! Type for tropest header
! -----------------------
  TYPE t_trphead
    CHARACTER(LEN=80)            :: title  ! Title
    INTEGER(i4b)                 :: iFrmt  ! Format number
    INTEGER(i4b)                 :: iTrpMd ! Tropopshere model
    INTEGER(i4b)                 :: iTrMap ! Mapping function
                                           !  =1:   1/cos(z)
                                           !  =2:   Hopfield
                                           !  =3,4: dry,wet Niell
                                           !  =5,6: dry,wet GMF
                                           !  =7,8: dry,wet VMF
    INTEGER(i4b),DIMENSION(2)    :: iTrGrd ! Tropospheric gradient model
                                           ! (1) =0: no estimation
                                           !     =1: tilting
                                           !     =2: linear
                                           ! (2) ratio ZPD/gradient
    INTEGER(i4b)                 :: iElvnq ! Minimum elevation angle in degrees
    REAL(r8b)                    :: iTab   ! Tabular interval (days)
  END TYPE t_trphead

! Type for tropest record
! -----------------------
  TYPE t_trprec
    REAL(r8b),DIMENSION(2)       :: timInt ! Interval (start,end) (MJD)
    REAL(r8b)                    :: model  ! Model value (Up)
    REAL(r8b)                    :: total  ! Total value (Up)
    REAL(r8b),DIMENSION(3)       :: corr   ! Estimated correction (N,E,U)
    REAL(r8b),DIMENSION(3)       :: sigma  ! Sigma value (N,E,U)
  END TYPE t_trprec

! Type for station records
! ------------------------
  TYPE t_trpstat
    INTEGER(i4b)                          :: nTrp   ! Number of records
    CHARACTER(LEN=staNameLength)          :: staNam ! Station name
    CHARACTER(LEN=1)                      :: staFlg ! Station flag
    TYPE(t_trprec),DIMENSION(:),POINTER   :: trp    ! List of tropo records
  END TYPE t_trpstat

! Type for entire troposphere file
! --------------------------------
  TYPE t_trpest
    INTEGER(i4b)                         :: nRec   ! Total number of records
    INTEGER(i4b)                         :: nSta   ! number of stations
    TYPE(t_trphead)                      :: head   ! Header info of file
    TYPE(t_trpstat),DIMENSION(:),POINTER :: sta    ! List of station records
  END TYPE t_trpest

! Maximum delta T (for boundary problem)
! --------------------------------------
  Real(r8b),PARAMETER                 :: dTmax = 1/86400.d0 ! (days)


! Initialize structure
! --------------------
CONTAINS
  SUBROUTINE init_trpest(trpEst)
    TYPE(t_trpEst)   :: trpEst
    NULLIFY(trpEst%sta)
    trpEst%nRec = 0
    trpEst%nSta = 0
  END SUBROUTINE init_trpEst



END MODULE d_trpest
