
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  MODULE p_mkclus


! -------------------------------------------------------------------------
! Purpose:    Program module for pgm mkclus
!
! Author:     R. Dach
!
!
! Created:    13-Jun-2002
! Last mod.:  15-Dec-2005
!
! Changes:    15-Dec-2005  RD: A few new options added
!
! SR called:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  IMPLICIT NONE

! Program options
! ---------------
  TYPE t_mkclus_opt
    ! General startegy:
    INTEGER(i4b)                       :: cluStrat  ! 1: global cluster (ZD)
                                                    ! 2: regional cluster (ZD)
                                                    ! 3: regional cluster (SD)
    ! Cluster definition
    INTEGER(i4b)                       :: numClu    ! Number of clusters
    INTEGER(i4b)                       :: maxSta    ! Max. number of stations
                                                    !    per cluster (glob.)
    INTEGER(i4b)                       :: staStrat  ! Criteria for maxSta
                                                    ! 1: numObs
                                                    ! 2: clock RMS
                                                    ! 3: density
    INTEGER(i4b)                       :: satObs    ! Redundancy per satellite
    INTEGER(i4b)                       :: staObs    ! Redundancy for each obs.
                                                    !    of a station
    INTEGER(i4b)                       :: nSolve    ! Number of clusters meet
                                                    !    satObs/staObs condition
    ! Station (baseline) exclusion
    INTEGER(i4b)                       :: maxAmb    ! Max. number of
                                                    !    ambiguities in file
    INTEGER(i4b)                       :: minObs    ! Min. number of
                                                    !    observations in file
    REAL(r8b)                          :: maxRMS    ! Receiver clock quality
                                                    !    from .SIG-file--only ZD
    INTEGER(i4b)                       :: codDel    ! 0: delete phase files
                                                    ! 1: delete code+phase files
    ! Observation selection
    INTEGER(i4b)                       :: isasys    ! Satellite system
                                                    ! 0: ALL
                                                    ! 1: GPS
                                                    ! 2: GLONASS
    INTEGER(i4b)                       :: nsampl    ! Sampling to be checked
    REAL(r8b)                          :: obsSat    ! Redundancy not required
                                                    !    for sat. with a low
                                                    !    num. of obs. (%/100)

  END TYPE t_mkclus_opt

! File information record
! -----------------------
  TYPE t_staFil
    INTEGER(i4b),DIMENSION(2)          :: staIdx    ! Index in station list
    INTEGER(i4b)                       :: nDiff     ! Difference level of file
    REAL(r8b)                          :: length    ! Length of a baseline
!
    INTEGER(i4b)                       :: numObs    ! Number of obs. per file
    INTEGER(i4b)                       :: numAmb    ! Number of amb. per file
!
    INTEGER(i4b)                       :: cluFlg    ! Use file or not...
  END TYPE t_staFil

! Station record
! --------------
  TYPE t_station
    CHARACTER(LEN=staNameLength)       :: staNam    ! Station name
    REAL(r8b),    DIMENSION(3)         :: lb        ! Station coord
                                                    ! (lat,lon,radius)
    REAL(r8b)                          :: clkRMS    ! RMS of clock fit
    INTEGER(i4b), DIMENSION(:),POINTER :: nxtSta    ! List of neighbouring
                                                    !   stations
  END TYPE t_station

! Cluster listing
! ---------------
  TYPE t_cluster
    INTEGER(i4b)                       :: filIdx    ! Index in file list
    INTEGER(i4b)                       :: cluster   ! Cluster number
    INTEGER(i4b)                       :: delFlg    ! Station may be deleted
                                                    !   to get "maxSta"/cluster
  END TYPE t_cluster

END MODULE p_mkclus
