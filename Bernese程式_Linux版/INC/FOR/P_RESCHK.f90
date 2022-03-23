
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE p_reschk

! -------------------------------------------------------------------------
! Purpose:    This module defines structures for program RESCHK
!
! Author:     R. Dach
!
! Created:    04-Dec-2000
! Last mod.:  18-Aug-2010
!
! Changes:    24-Apr-2001 RD: satellites with no observations
!             05-Sep-2001 HU: include MAXSTA removed
!             28-NOV-2001 RD: only 1 sat may be flaged as bad
!             24-Apr-2002 MM: add number of obs, short summary and RMS before
!             12-Mar-2003 RD: Remove name of stacrx file
!             30-Aug-2005 MM: Minimum number of observations added
!             18-Aug-2010 RD: RMS ratio for satellites and stations
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
!
! station information array
! -------------------------
TYPE t_reschk_opt
  CHARACTER(LEN=fileNameLength)  :: SATCRUX ! Satellite Problems
  CHARACTER(LEN=fileNameLength)  :: SESFILE ! Session table
  CHARACTER(LEN=fileNameLength)  :: ABBPAN  ! Station name abb. table
  CHARACTER(LEN=fileNameLength)  :: ASPLIT  ! ARCSPLIT SUMMARY FILE
  CHARACTER(LEN=fileNameLength)  :: CODXTR  ! CODSPP SUMMARY FILE
  CHARACTER(LEN=fileNameLength)  :: RESRMS  ! RESRMS SUMMARY FILE
  CHARACTER(LEN=fileNameLength)  :: DELSTA  ! List of stations to be deleted
  CHARACTER(LEN=fileNameLength)  :: SUMOUT  ! Short summary file

  CHARACTER(LEN=shortLineLength) :: TITLE   ! Title
  INTEGER(i4b) :: SUMMARY ! Output: Summary/Details
  INTEGER(i4b) :: iyear4  ! Year of the (actual) day

  LOGICAL      :: BADSTA  ! Search for bad stations
  INTEGER(i4b) :: nDiff   ! Zero difference case
  INTEGER(i4b) :: MAXSTA0 ! Maximum number of stations to delete
  REAL(r8b)    :: MAXRMS0 ! Maximum total RMS allowed
  REAL(r8b)    :: BADSOL0 ! RMS limit when a bad solution assumed
  INTEGER(i4b) :: MAXSTA1 ! Maximum number of stations to delete
  REAL(r8b)    :: MAXRMS1 ! Maximum total RMS allowed

  CHARACTER(LEN=lineLength)      :: DIR_CZH ! directory for DELSTA
  CHARACTER(LEN=lineLength)      :: DIR_CZO ! directory for DELSTA
  CHARACTER(LEN=lineLength)      :: DIR_PZH ! directory for DELSTA
  CHARACTER(LEN=lineLength)      :: DIR_PZO ! directory for DELSTA
  CHARACTER(LEN=fileExtLength)   :: EXT_CZH ! extension for DELSTA
  CHARACTER(LEN=fileExtLength)   :: EXT_CZO ! extension for DELSTA
  CHARACTER(LEN=fileExtLength)   :: EXT_PZH ! extension for DELSTA
  CHARACTER(LEN=fileExtLength)   :: EXT_PZO ! extension for DELSTA

  LOGICAL      :: MANSAT  ! Detect maneuver satellites
  REAL(r8b)    :: MAXRRMS ! Maximum ratio of Sat-RMS to total RMS allowed
  REAL(r8b)    :: MAXDEL  ! Maximal percentage of data can be deleted from
                          ! RESRMS
  REAL(r8b)    :: MAXARMS ! Maximum RMS allowed for a satellite (otherwise
                          ! maneuver)
  REAL(r8b)    :: MAXORB  ! Maximum RMS in arc split allowed    (otherwise
                          ! maneuver)
  REAL(r8b)    :: MAXMRMS ! Maximum ratio of Sat-RMS to total RMS allowed, when
                          ! man.
  REAL(r8b)    :: MINOBSG ! Minimum number of observations (GPS)
  REAL(r8b)    :: MINOBSR ! Minimum number of observations (GLONASS)
  LOGICAL      :: DEL1SAT ! Only one satellite is assumed to be bad

  LOGICAL      :: TSTMODE ! 1st iteration will be analized
END TYPE t_reschk_opt

!
! station information array
! -------------------------
TYPE t_rmssta
  CHARACTER(LEN=20),             DIMENSION(2) :: STANAM ! Station IDs
  CHARACTER(LEN=fileNameLength), DIMENSION(2) :: FilNam ! Session ID
  REAL(r8b)                                   :: RMS    ! Baseline/Station total
                                                        ! RMS
END TYPE t_rmssta

!
! bad station array
! -----------------
TYPE t_badsta
  CHARACTER(LEN=20)                 :: STANAM   ! Station IDs
  CHARACTER(LEN=fileNameLength)     :: FILNAM   ! file name
  INTEGER(i4b)                      :: nSta     ! # of occurance
  REAL(r8b)                         :: RMSSUM   ! Sum of all RMSs
  REAL(r8b)                         :: rmsRatio ! rms(sta)/rms(tot)
END TYPE t_badsta

!
! bad satellite array
! -------------------
TYPE t_badsat
  INTEGER(i4b)                      :: iSat     ! Index of satellite
  INTEGER(i4b)                      :: SatNum   ! Satellite number
  INTEGER(i4b)                      :: Flag     ! Flag
  REAL(r8b)                         :: rmsRatio ! rms(sat)/rms(tot)
END TYPE t_badsat

!
! satellite information array
! ---------------------------
TYPE t_rmssat
  INTEGER(i4b)                      :: SATNUM   ! Satellite number
  REAL(r8b)       , DIMENSION(2)    :: RMS      ! Total satellite RMS
  REAL(r8b)       , DIMENSION(2)    :: OBS      ! Observation statistic
  INTEGER(i4b)    , DIMENSION(2)    :: NUMOBS   ! Number of observations
  REAL(r8b)       , DIMENSION(2)    :: SATARC   ! Boundaries of arcs
END TYPE t_rmssat

!
! arc split information array
! ---------------------------
TYPE t_asplit
  INTEGER(i4b)                      :: SATNUM   ! Satellite number
  REAL(r8b)                         :: RMS      ! Total satellite RMS
END TYPE t_asplit

!
! satcrux information array
! -------------------------
TYPE t_satcrx
  INTEGER(i4b)                      :: SATNUM   ! Satellite number
  INTEGER(i4b)                      :: iLine    ! line number in file
  INTEGER(i4b)                      :: iobbad   ! bad obs. type in SATCRUX file
  INTEGER(i4b)                      :: iacbad   ! type of action in SATCRUX file
  REAL(r8b),    DIMENSION(2)        :: timbad   ! start/end in SATCRUX file
END TYPE t_satcrx

END MODULE p_reschk
