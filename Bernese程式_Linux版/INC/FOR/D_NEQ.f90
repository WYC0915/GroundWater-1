! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE d_neq

! -------------------------------------------------------------------------
! Purpose:    Normal equation
!
! Remarks:    In case of a new neq version check the subroutines
!             NEQASCII, NEQINIT, NEQSTORE, NEQWRITE, NQRDHEAD
!
! Author:     R.Dach
!
! Created:    22-Sep-2005
!
! Changes:    30-Nov-2005 CU: Add npseuel
!             08-Feb-2007 RD: misc%nObs/nParms i4b->r8b
!             09-Feb-2007 AG: maxStaSin increased 400 -> 500
!             26-Jun-2008 RD: System-specific PCV in SINEX record
!             27-Apr-2009 LM/SL: neqCurrentVersion added and set to 5
!             04-May-2009 RD: Scaling of loading models added
!             06-May-2009 RD: increase maxoff (multi-year solutions), old maxsat
!             31-Aug-2010 LO: maxStaSin 1000 -> 2000 due to FODITS
!             15-Nov-2010 RD: New NEQ version: D_PAR omega is written anytime
!             13-Jul-2011 LP: Sat-spec. obstypes; neq%version set to 8
!             13-Dec-2011 SL: m_bern with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  ! Modules
  ! -------
  USE m_bern,   ONLY: i4b, r8b, fileNameLength
  USE m_time,   ONLY: t_timint
  USE m_maxdim, ONLY: maxsat
  USE m_global, ONLY: maxsys
  USE d_grid,   ONLY: typLen
  USE d_par,    ONLY: t_par
  IMPLICIT NONE

  ! Current NEQ Format Version
  ! --------------------------
  INTEGER(i4b), PARAMETER :: neqCurrentVersion = 8

  ! Dimensions
  ! ----------
  INTEGER(i4b),PARAMETER, PRIVATE :: neqRecl   =  10000
  INTEGER(i4b),PARAMETER  :: maxfrq    =      2
  INTEGER(i4b),PARAMETER  :: maxOff    = 2*maxSat
  INTEGER(i4b),PARAMETER  :: maxStaSin =   2000
  INTEGER(i4b),PARAMETER  :: maxObst   =   30000

  ! NEQ-system structure
  ! --------------------
  TYPE t_sinex_v1
    TYPE(t_timint)                     :: timint
    CHARACTER(LEN=fileNameLength)      :: phasecc
    CHARACTER(LEN=16)                  :: stname
    REAL(r8b)   , DIMENSION(3,maxFrq)  :: antphs
    INTEGER(i4b)                       :: antfrq
    REAL(r8b)   , DIMENSION(3)         :: antecc
    INTEGER(i4b)                       :: antnum
    CHARACTER(LEN=16)                  :: antsta
    CHARACTER(LEN=16)                  :: antrec
  END TYPE t_sinex_v1

  TYPE t_sinex_v2
    TYPE(t_timint)                     :: timint
    CHARACTER(LEN=fileNameLength)      :: phasecc
    CHARACTER(LEN=16)                  :: stname
    REAL(r8b)   , DIMENSION(3,maxFrq)  :: antphs
    INTEGER(i4b)                       :: antfrq
    REAL(r8b)   , DIMENSION(3)         :: antecc
    INTEGER(i4b)                       :: antnum
    CHARACTER(LEN=20)                  :: antsta
    CHARACTER(LEN=20)                  :: antrec
  END TYPE t_sinex_v2

  TYPE t_sinex_pcv
    INTEGER(i4b)                       :: nFrq
    REAL(r8b)   , DIMENSION(3,maxFrq)  :: antphs
    INTEGER(i4b)                       :: adopted
    INTEGER(i4b)                       :: individ
    CHARACTER(LEN=10)                  :: atxStr
  END TYPE t_sinex_pcv

  TYPE t_sinex
    TYPE(t_timint)                     :: timint
    CHARACTER(LEN=16)                  :: stname
    REAL(r8b)        ,DIMENSION(3)     :: antecc
    TYPE(t_sinex_pcv),DIMENSION(0:maxsys-1):: antpcv
    INTEGER(i4b)                       :: antnum
    CHARACTER(LEN=20)                  :: antsta
    CHARACTER(LEN=20)                  :: antrec
  END TYPE t_sinex

  TYPE t_obst
    CHARACTER(LEN=3),DIMENSION(4)      :: obstyp  ! Code+phase obs used
  END TYPE t_obst

  TYPE t_misc
    CHARACTER(LEN=132),DIMENSION(2)    :: title   ! Two title lines
    CHARACTER(LEN=16)                  :: datum   ! Datum string
    CHARACTER(LEN=16)                  :: nutmod  ! Nutation model
    CHARACTER(LEN=16)                  :: submod  ! Subdaily model
    CHARACTER(LEN=32), DIMENSION(2)    :: orbFil  ! STD and RPR - file names
    CHARACTER(LEN=32)                  :: gravFil ! a priori gravity file
    REAL(r8b)                          :: nobs    ! number of observations
    INTEGER(i4b)                       :: npar    ! Number (withou pre-el.)
    REAL(r8b)                          :: lTPl    ! (obs-cmp)T * P * (obs-cmp)
    REAL(r8b)                          :: nparms  ! Number of all parameters
    INTEGER(i4b)                       :: npseu   ! Number of pseudo-observations
    INTEGER(i4b)                       :: npseuel ! Number of preelim. pseudo-obs.
    INTEGER(i4b)                       :: nftot   ! Total number of files
    INTEGER(i4b)                       :: nsmpnq  ! Sampling rate (sec)
    INTEGER(i4b)                       :: ielvnq  ! Minimum el. angle (degree)
    INTEGER(i4b)                       :: itropo  ! Tropospheric model
    INTEGER(i4b)                       :: iextra  ! Which trop. values
    INTEGER(i4b)                       :: itrmap  ! Mapping function
    INTEGER(i4b)                       :: itrgrd  ! Which gradient model
    INTEGER(i4b)                       :: nanoff  ! # sat. ant. offset groups
    INTEGER(i4b),DIMENSION(maxOff)     :: nsaoff  ! # sat. belonging to group
    INTEGER(i4b),DIMENSION(maxSat,maxOff) :: satoff ! satellite numbers
    INTEGER(i4b)                       :: nstat_sinex
    TYPE(t_sinex),DIMENSION(maxStaSin) :: sinex   ! Information for SINEX
    CHARACTER(LEN=typLen), DIMENSION(3):: grdNeq  ! Keywords of Vienna grids
    INTEGER(i4b)                       :: nobst   ! Number of observation types
    TYPE(t_obst),DIMENSION(maxObst)    :: obst    ! Sat-spec. obs. types
  END TYPE t_misc

  TYPE t_neq
    INTEGER(i4b)                       :: version ! Version of the NEQ-file
    TYPE(t_misc)                       :: misc    ! Miscelaneous information
    TYPE(t_par)  ,DIMENSION(:),POINTER :: par     ! Parameter information
    REAL(r8b)    ,DIMENSION(:),POINTER :: aNor    ! NEQ matrix (upper tr.)
    REAL(r8b)    ,DIMENSION(:),POINTER :: bNor    ! Right-hand side of NEQ
    REAL(r8b)    ,DIMENSION(:),POINTER :: xxx     ! Solution vector
  END TYPE t_neq

CONTAINS

  ! ---------------------------------------------------------------------------
  ! Read a binary record
  ! ---------------------------------------------------------------------------
  SUBROUTINE readRec(lfn,dim,rec)
    INTEGER(i4b)            :: lfn
    INTEGER(i4b)            :: dim  ! Size of the record
    REAL(r8b), DIMENSION(:) :: rec  ! record to be written

    ! Local variables
    INTEGER(i4b)            :: ii
    INTEGER(i4b)            :: myRecl

    READ(lfn) myRecl
    DO ii = 1, dim, myRecl
      IF (ii + myRecl < dim) THEN
        READ(lfn) rec(ii:ii+myRecl-1)
      ELSE
        READ(lfn) rec(ii:dim)
      ENDIF
    END DO
  END SUBROUTINE readRec

  ! ---------------------------------------------------------------------------
  ! Write a binary record
  ! ---------------------------------------------------------------------------
  SUBROUTINE writeRec(lfn,dim,rec)
    INTEGER(i4b)            :: lfn
    INTEGER(i4b)            :: dim  ! Size of the record
    REAL(r8b), DIMENSION(:) :: rec  ! record to be written

    ! Local variables
    INTEGER(i4b)            :: ii

    WRITE(lfn) neqRecl
    DO ii = 1, dim, neqRecl
      IF (ii + neqRecl < dim) THEN
        WRITE(lfn) rec(ii:ii+neqRecl-1)
      ELSE
        WRITE(lfn) rec(ii:dim)
      ENDIF
    END DO
  END SUBROUTINE writeRec

END MODULE d_neq


