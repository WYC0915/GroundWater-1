
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE p_gravdet

! -------------------------------------------------------------------------
! Purpose:    Maximum dimensions and structures for program GRAVDET
!
! Author:     L. Mervart
!
! Created:    ??-???-????
!
! Changes:    09-Jun-2009 AJ: structure t_covdat added
!             15-Jun-2009 AJ: maxepo increased from 18000 to 110000 (GOCE)
!             20-Nov-2009 GB: allow for single/double difference positions (m_poszsd, m_prtzsd, m_maxzsd)
!             04-May-2012 RD: Use m_bern with only
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern,    ONLY: i4b, r8b, fileNameLength
  USE m_epoch,   ONLY: t_epoch
  USE m_maxdim,  ONLY: maxSat

  IMPLICIT NONE

  CHARACTER(LEN=7), PARAMETER :: pgName = 'GRAVDET'

  ! Dimensions
  ! ----------
  INTEGER(i4b),     PARAMETER :: maxDim =     7 ! for m_coe and STCHDL: Orbit and 6 partials w.r.t. ini ele
  INTEGER(i4b),     PARAMETER :: maxQ   =    20 ! Max. Order for numerical integration of eqns. of motion
  INTEGER(i4b),     PARAMETER :: maxInt =  9000 ! Max. number of subintervals in arc
  INTEGER(i4b),     PARAMETER :: maxStc =  3000 ! Max. number of stochastic parameters (???epochs)(3000 allows pulses every 2 min)
  INTEGER(i4b),     PARAMETER :: maxArc =    20 ! Max. number of arcs (only for simulation)
  INTEGER(i4b),     PARAMETER :: maxPar = 16000 ! Maximum total number of parameters
  INTEGER(i4b),     PARAMETER :: MAXEPO = 110000 ! Max. number of epochs in observation file
  INTEGER(i4b),     PARAMETER :: MAXLCQ =     7 ! LOCQ Dimension
  INTEGER(i4b),     PARAMETER :: MAXSHD =   500 ! Max. number of shadow periods

  ! Input Options
  ! -------------
  INTEGER(i4b)                :: m_indeo   ! flag for simulation (1) or parameter estimation (2)
  INTEGER(i4b), DIMENSION(30) :: m_prcOpt  ! processing options
  INTEGER(i4b), DIMENSION(40) :: m_estOpt  ! OPTIONS FOR ORBIT ESTIMATION
  INTEGER(i4b)                :: m_iorSys  ! orbit system (2 ... J2000.0)
  INTEGER(i4b)                :: m_modEop  ! EOP flag
  INTEGER(i4b)                :: m_nitint  ! number of iteration steps
  INTEGER(i4b)                :: m_simul   ! generate standard orbit files consistent with *.tab
  INTEGER(i4b)                :: m_constst ! use constant step size
  INTEGER(i4b)                :: m_intstep ! use constant step size
  INTEGER(i4b)                :: m_redprct ! use constant step size
  INTEGER(i4b)                :: m_poszsd  ! difference level of observable
  INTEGER(i4b)                :: m_prtzsd  ! difference level of observable
  REAL(r8b)                   :: m_maxzsd  ! maximum "o-c" for single/double differences
  REAL(r8b)                   :: m_tolxv   ! maximum error in velocity
  CHARACTER(LEN=fileNameLength) :: m_updStdIn
  LOGICAL                     :: m_use_estpar

  ! Arc-specific information
  ! ------------------------
  INTEGER(i4b)                :: m_iarc    ! current arc number
  INTEGER(i4b)                :: m_numSat  ! current satellite number (PRN)
  REAL(r8b),    DIMENSION(2)  :: m_fromTo  ! current arc boundaries
  REAL(r8b)                   :: m_beta    ! Angle between orbital plane and direction to sun
  REAL(r8b),    DIMENSION(6)  :: m_eleSat  ! set of osculating elements at left arc boundary

  ! Orbit Description (within current arc)
  ! --------------------------------------
  INTEGER(i4b)                                :: m_ninter ! number of subintervals for orbit
  INTEGER(i4b)                                :: m_q      ! order of numerical integration
  REAL(r8b), DIMENSION(maxInt)                :: m_hsave  ! lengths of subintervals
  REAL(r8b), DIMENSION(maxInt)                :: m_t0save ! time origins of subintervals
  REAL(r8b), DIMENSION(2,maxInt)              :: m_ftsave ! subinterval boundaries
  REAL(r8b), DIMENSION(3*maxDim*maxQ, maxInt) :: m_coe    ! polynomial coefficients (subintervals)

  ! Parameter Description
  ! ---------------------
  INTEGER(i4b), DIMENSION(maxLcq,maxPar,maxArc) :: m_locq   ! parameter description
  INTEGER(i4b)                                  :: m_nvar   ! number of variational eqns. (dynamical parms.)

  ! Miscellaneous
  ! -------------
  INTEGER(i4b)                                  :: m_idim   ! 3 (simulation) or 3*7 (par. estimation)
  INTEGER(i4b)                                  :: m_iter   ! current iteration step (orbit improvement)
  INTEGER(i4b)                                  :: m_prn    ! satnum for the solve_deq call in GRAVDET

! Type for kinematic stations
! ---------------------------
  TYPE t_covDat
    TYPE(t_epoch)                            :: firstEpoch  ! first epcoh
    REAL(r8b)                                :: rms         ! rms
    INTEGER(i4b)                             :: iStep       ! time steps
    INTEGER(i4b), DIMENSION(:),POINTER       :: ind1        ! First index
    INTEGER(i4b), DIMENSION(:),POINTER       :: ind2        ! Second index
    REAL(r8b),    DIMENSION(:),POINTER       :: cov         ! Covariance
  END TYPE t_covDat

! Initialize structure
! --------------------
  CONTAINS

  SUBROUTINE init_covDat(covDat)
    TYPE(t_covDat)    :: covDat

    NULLIFY(covDat%ind1)
    NULLIFY(covDat%ind2)
    NULLIFY(covDat%cov)
  END SUBROUTINE init_covDat


END MODULE p_gravdet
