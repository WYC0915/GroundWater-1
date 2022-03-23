MODULE s_NUTN20
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE nutn20(xtdb,nut)

! -------------------------------------------------------------------------
! Purpose:    Computation of nutation matrix for epoch xtdb (jul. date in
!             barycentric dynamical time.
!             -> old variable xmod is possible too, because diff. < 1 min
!             causes differences < 0.1 mas
!
! Author:     C. Urschl
!
! Created:    12-Dec-2002
! Last mod.:  21-May-2010
!
! Changes:    26-Aug-2003 HU: Parameter list of SR NUTCOR adapted
!             29-Jan-2007 AG: Extrapolation tolerance for litpol added
!             21-May-2010 MF: Call sr init_nutat
!             04-May-2012 RD: Use m_bern with only
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnerr, &
                      fileNameLength
  USE d_const,  ONLY: pi, const_def
  USE d_nutmod, ONLY: t_nutat, init_nutat

  USE s_rdnutm
  USE s_exitrc
  USE s_litpol
  USE s_gtflna
  USE s_nutcor
  USE s_getcpo
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  REAL(r8b)                  :: xtdb  ! Epoch in barycentric dynamical time

! input/output:

! output:
  REAL(r8b), DIMENSION(3,3)  :: nut   ! Nutation matrix

! List of functions
! -----------------

! Local types
! -----------
  TYPE(t_nutat),SAVE         :: nutat           ! Nutation model parameters

! Local parameters
! ----------------

! Local variables
! ---------------
  INTEGER(i4b)                  :: ios
  INTEGER(i4b),SAVE             :: ifirst=1

  REAL(r8b)                     :: tu    ! Time interval between J2000.0 and xtdb
  REAL(r8b)                     :: dPsi  ! Longitude
  REAL(r8b)                     :: dEps  ! Obliquity
  REAL(r8b)                     :: eps, epstru
  REAL(r8b), DIMENSION(1)       :: ddeps, ddpsi
  REAL(r8b), DIMENSION(2)       :: tt, tteps, ttpsi
  REAL(r8b)                     :: sindp, cosdp
  REAL(r8b)                     :: sinem, cosem
  REAL(r8b)                     :: sinet, coset
  REAL(r8b)                     :: dtmoon

  CHARACTER(LEN=fileNameLength) :: filnam

! Pi defined?
! -----------
  IF (const_def /= 1) THEN
    WRITE(lfnerr,'(A)') 'SR NUTCOR: Dear programmer, ' // &
    'constants are not defined - call SR DEFCON!'
    CALL exitrc(2)
  ENDIF

! Get nutation model filename (on first call)
! ---------------------------
  IF (ifirst == 1) THEN
    CALL gtflna(1, 'NUTMOD', filnam, ios)

! Read nutation model file
! ------------------------
    CALL init_nutat(nutat)
    CALL rdnutm(filnam,nutat)
    ifirst = 0
  ENDIF

! Compute model corrections dPsi, dEps
! ------------------------------------
  CALL nutcor(xtdb,nutat,tu,dPsi,dEps,dtmoon)

! Celestial pole offsets in radians
! ---------------------------------
  CALL getcpo(xtdb,tt,tteps,ttpsi)
  CALL litpol(2,1,tt,tteps,xtdb,0d0,ddeps)
  CALL litpol(2,1,tt,ttpsi,xtdb,0d0,ddpsi)
  deps = deps + ddeps(1)
  dpsi = dpsi + ddpsi(1)

! Eps - zero epoch J2000 (rad)
! ----------------------------
  eps = 84381.448d0- 46.815d0*tu - 0.00059d0*tu**2 + 0.001813d0*tu**3
  eps = eps * pi/648000d0

! True obliquity epstru
! ---------------------
  epstru = eps + deps

! Sine and cosine
! ---------------
  sindp = DSIN(dpsi)
  cosdp = DCOS(dpsi)
  sinem = DSIN(eps)
  cosem = DCOS(eps)
  sinet = DSIN(epstru)
  coset = DCOS(epstru)

! Rotation matrix
! ---------------
  nut(1,1) =  cosdp
  nut(2,1) =  coset*sindp
  nut(3,1) =  sinet*sindp
  nut(1,2) = -cosem*sindp
  nut(2,2) =  cosem*coset*cosdp + sinem*sinet
  nut(3,2) =  cosem*sinet*cosdp - sinem*coset
  nut(1,3) = -sinem*sindp
  nut(2,3) =  sinem*coset*cosdp - cosem*sinet
  nut(3,3) =  sinem*sinet*cosdp + cosem*coset

  RETURN

END SUBROUTINE nutn20


END MODULE
