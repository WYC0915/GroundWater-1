MODULE s_ERR3DM
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE err3dm(ellCrd, aell, bell, neuVarCovInfoI, neuVarCovInfoO)

! -------------------------------------------------------------------------
! Purpose:    Transform the variance-covariance information
!                  ( dRad*dRad dRad*dRad dRad*m )    ( m*m m*m m*m )
!             from ( dRad*dRad dRad*dRad dRad*m ) to ( m*m m*m m*m )
!                  ( dRad*m    dRad*m    m*m    )    ( m*m m*m m*m )
!                  \-------------1--------------/    \------2------/
!
!             Inputs  : - ellCrd(1:3) = (North,East,Up) in [rad,rad,m]
!                       - aell = semi-major axis of the datum definition
!                       - bell = semi-minor axis of the datum definition
!                       - neuVarCovInfoI = 3x3 Matrix (1)
!             Outputs : - neuVarCovInfoO = 3x3 Matrix (2)
!
! Author:     Luca Ostini
!
! Created:    17-Jun-2008
! Last mod.:  17-Jun-2008
!
! Changes:    17-Jun-2008 LO: Created this file
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern

  IMPLICIT NONE

! Subroutine Arguments
! ---------------------
  REAL(r8b),DIMENSION(3)        :: ellCrd
  REAL(r8b)                     :: aell
  REAL(r8b)                     :: bell
  REAL(r8b),DIMENSION(3,3)      :: neuVarCovInfoI
  REAL(r8b),DIMENSION(3,3)      :: neuVarCovInfoO

! Local Parameters
! ----------------
  CHARACTER(LEN=shortLineLength) , PARAMETER   :: srN = 'err3dm'

! Local Variables
! ---------------

! Subroutine
! ----------

  neuVarCovInfoO(1,1) = &
       TAN(neuVarCovInfoI(1,1))   * bell                * bell
  neuVarCovInfoO(1,2) = &
       TAN(neuVarCovInfoI(1,2))   * aell*COS(ellCrd(1)) * bell
  neuVarCovInfoO(1,3) = &
       neuVarCovInfoI(1,3)        * 1.0D0               * bell

  neuVarCovInfoO(2,1) = &
       TAN(neuVarCovInfoI(2,1))   * bell                * aell*COS(ellCrd(1))
  neuVarCovInfoO(2,2) = &
       TAN(neuVarCovInfoI(2,2))   * aell*COS(ellCrd(1)) * aell*COS(ellCrd(1))
  neuVarCovInfoO(2,3) = &
       neuVarCovInfoI(2,3)        * 1.0D0               * aell*COS(ellCrd(1))

  neuVarCovInfoO(3,1) = &
       TAN(neuVarCovInfoI(3,1))   * bell                * 1.0D0
  neuVarCovInfoO(3,2) = &
       TAN(neuVarCovInfoI(3,2))   * aell*COS(ellCrd(1)) * 1.0D0
  neuVarCovInfoO(3,3) = &
       neuVarCovInfoI(3,3)        * 1.0D0               * 1.0D0


 RETURN

END SUBROUTINE err3dm

END MODULE s_ERR3DM
