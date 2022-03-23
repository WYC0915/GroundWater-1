! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE s_stdOrbit_t

! -------------------------------------------------------------------------
! Description: Defines structures for standard orbits
!
! Author:     L. Mervart
!
! Created:    19-Sep-2007
!
! Changes:    02-Oct-2007 AJ: estStc added to t_stdOrb
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

USE m_bern
USE m_epoch
USE m_time
USE d_par, ONLY: t_par, maxlcq

IMPLICIT NONE

PRIVATE
PUBLIC :: t_stoch, t_orbInt, t_orbArc, t_stdOrb, t_satPos, t_estPar
PUBLIC :: stdOrb_version, ASSIGNMENT(=)

! -------------------------------------------------------------------------
! Structures for Standard Orbits
! -------------------------------------------------------------------------

! Stochastic Parameter
! --------------------
TYPE t_stoch
  CHARACTER(LEN=1) :: typ  ! 'A' ... acceleration, 'V' ... sudden vel. change
  CHARACTER(LEN=1) :: comp ! component ('R', 'S', 'W')
  REAL(r8b)               :: left  = 0.0 ! start in sec. rel. to t_orbArc
  REAL(r8b)               :: right = 0.0 ! for typ == 'V' right == left
  REAL(r8b)               :: value = 0.0 ! parameter value (m/s^2 or m/s)
  REAL(r8b), DIMENSION(6) :: dEds  = 0.0 ! partial derivative of initial osc
                                         ! elements on stochastic parameter
  REAL(r8b), DIMENSION(6) :: dE    = 0.0 ! Accumulated Impact of previous
                                         ! stoch. parameters of particular
                                         ! type up to 'left' on osc. elements
END TYPE

! Integration Interval
! --------------------
TYPE t_orbInt
  REAL(r8b)    :: left  = 0.0 ! start in seconds rel. to t_orbArc%tWin%t(1)
  REAL(r8b)    :: right = 0.0 ! end in seconds
  LOGICAL      :: shadow ! shadow flag
  REAL(r8b)    :: hh     ! scale factor
  REAL(r8b)    :: t0     ! origin of the polynomial development (seconds)
  INTEGER(i4b) :: qq     ! polynomial degree

  ! Equation of Motion
  REAL(r8b), DIMENSION(:,:),   POINTER :: rCoe    => NULL() ! (3,qq+1)

  ! Transition Matrix
  REAL(r8b), DIMENSION(:,:,:), POINTER :: drdeCoe => NULL() ! (3,6,qq+1)

  ! Sensitivity matrix (constant accelerations in RSW directions)
  REAL(r8b), DIMENSION(:,:,:), POINTER :: drdsCoe => NULL() ! (3,3,qq+1)
END TYPE

! Orbit Arc
! ---------
TYPE t_orbArc
  CHARACTER(LEN=1) :: sys = 'G' ! system ('G' = GPS, 'R' = GloNaSS)
  INTEGER(i4b)     :: svn = 0   ! satellite number (PRN for GPS)
  INTEGER(i4b)     :: man = 0   ! maneuver number
  TYPE(t_timWin)   :: tWin      ! time window
  TYPE(t_key)   , DIMENSION(:), POINTER :: modDesc => NULL() ! model descr.
  TYPE(t_orbInt), DIMENSION(:), POINTER :: orbInt  => NULL() ! integration int.
  TYPE(t_stoch),  DIMENSION(:), POINTER :: stoch   => NULL() ! stochastic par.
END TYPE

TYPE t_estPar
  INTEGER(i4b),DIMENSION(maxlcq)     :: locq  = 0    ! locq according to GPSEST
  CHARACTER(LEN=16)                  :: name  = ''   ! (usually) station name
  TYPE(t_time)                       :: time         ! validity interval (MJD)
  REAL(r8b)                          :: x0    = 0.0  ! a priori value
  REAL(r8b)                          :: dx    = 0.0  ! solution
  REAL(r8b)                          :: scale = 1.0  ! Scale factor
END TYPE

! Standard Orbit
! --------------
INTEGER(i4b), PARAMETER :: stdOrb_version = 2

TYPE t_stdOrb
  INTEGER(i4b)              :: version = stdOrb_version
  CHARACTER(LEN=lineLength) :: title   = ''
  LOGICAL                   :: useOldGetOrb = .FALSE.
  TYPE(t_key)   , DIMENSION(:), POINTER :: modDesc => NULL() ! model descr.
  TYPE(t_orbArc), DIMENSION(:), POINTER :: orbArc  => NULL() ! orbit arcs
  TYPE(t_par)  ,  DIMENSION(:), POINTER :: par     => NULL() ! parameter info
  TYPE(t_estPar), DIMENSION(:), POINTER :: estPar  => NULL() ! estimated parameters
  TYPE(t_stoch),  DIMENSION(:), POINTER :: estStc  => NULL() ! estimated stochastic
  TYPE(t_orbArc), DIMENSION(:), POINTER :: orbArcPart => NULL() ! special orbit arcs for drdeCoe
END TYPE

INTERFACE ASSIGNMENT (=)
  MODULE PROCEDURE assign_par_to_estPar
END INTERFACE

! Satellite Position
! ------------------
TYPE t_satPos
  CHARACTER(LEN=1)                    :: sys  = 'G'
  INTEGER(i4b)                        :: svn  = 0
  TYPE(t_epoch)                       :: epoch
  REAL(r8b), DIMENSION(:,:),  POINTER :: xv      => NULL() ! state vector
  REAL(r8b), DIMENSION(:,:,:),POINTER :: dxvdele => NULL() ! transition matrix
  REAL(r8b), DIMENSION(:,:),  POINTER :: dxvdp   => NULL() ! sensitivity matrix
END TYPE

CONTAINS

SUBROUTINE assign_par_to_estPar(estPar, par)
  USE d_par, ONLY: t_par
  IMPLICIT NONE
  TYPE(t_estPar), INTENT(OUT) :: estPar
  TYPE(t_par),    INTENT(IN)  :: par
  estPar%locq  = par%locq
  estPar%name  = par%name
  estPar%time  = par%time
  estPar%x0    = par%x0
  estPar%scale = par%scale
  estPar%dx    = 0.0
END SUBROUTINE

END MODULE
