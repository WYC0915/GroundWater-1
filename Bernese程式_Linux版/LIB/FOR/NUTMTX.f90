MODULE s_NUTMTX
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE nutmtx(icpo,xtdb,nut,nutnam,eqequi,bias)

! -------------------------------------------------------------------------
! Purpose:    Computation of nutation matrix for epoch xtdb (jul. date in
!             barycentric dynamical time.
!             -> old variable xmod is possible too, because diff. < 1 min
!             causes differences < 0.1 mas
!
! Author:     C. Urschl
!
! Created:    12-Dec-2002
! Last mod.:  06-May-2011
!
! Changes:    20-Dec-2002 PS: Correct Error Message due to Pi
!             07-Jan-2003 PS: Added nutnam
!             15-Jan-2003 PS: Renamed nutn20 -> nutmtx
!             16-May-2003 MM: Initialize structure
!             11-Jun-2003 HU: Equation of equinox (IERS Conv 2000)
!             16-Jun-2003 HU: Linear term of eps corrected
!             04-Jul-2005 HU/HB: Add frame bias matrix, reviewed computation
!                             of equation of equinoxes (IERS Conv 2003)
!                             Moved precession rates from NUTCOR, but no
!                             longer necessary due to IERS Conv 20003 used
!             21-Jan-2006 HU: parameter icpo
!             29-Jan-2007 AG: Extrapolation tolerance for litpol added
!             23-Feb-2007 HB/AG: Use ars from d_const/DEFCON
!             05-Sep-2007 HU: Convert dPsi,dEps to dX,dY
!             06-May-2011 HB: Use SR obliqu, get model keys through d_model,
!                             reactivate V50
!
! SR used:    opnsys, defcon, gtflna, rdnutm, nutcor, getcpo, litpol
!             init_nutat
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_const,  ONLY: ars, const_def, pi
  USE d_nutmod, ONLY: t_nutat, init_nutat
  USE d_model,  ONLY: getModKey, mod_orb_nutMod,mod_orb_prcMod, &
                      chrValLength

  USE s_rdnutm
  USE s_exitrc
  USE s_litpol
  USE s_gtflna
  USE s_obliqu
  USE s_nutcor
  USE s_getcpo
  USE s_poldef2
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  INTEGER(i4b)               :: icpo   ! Interpolation of CPO
                                       ! =0: no application of CPO
                                       ! =1: linear interpolation
                                       ! =2: Lagrange interpolation
  REAL(r8b)                  :: xtdb   ! Epoch in barycentric dynamical time

! output:
  REAL(r8b), DIMENSION(3,3)  :: nut    ! Nutation matrix
  CHARACTER(LEN=16)          :: nutnam ! Nutation Model Name
  REAL(r8b)                  :: eqequi ! Equation of equinox
  REAL(r8b), DIMENSION(3,3)  :: bias   ! Frame bias matrix

! List of functions
! -----------------

! Local types
! -----------
  TYPE(t_nutat),SAVE         :: nutat           ! Nutation model parameters

! Local parameters
! ----------------

! Local variables
! ---------------
  INTEGER(i4b)                  :: ios, ii
  INTEGER(i4b),SAVE             :: ifirst=1

  REAL(r8b)                     :: tu    ! Time interval between J2000.0 and xtdb
  REAL(r8b)                     :: dPsi  ! Longitude
  REAL(r8b)                     :: dEps  ! Obliquity
  REAL(r8b)                     :: epstru, eps0, epsA
  REAL(r8b), DIMENSION(1)       :: ddeps, ddpsi
  REAL(r8b), DIMENSION(2)       :: tt, tteps, ttpsi
  REAL(r8b)                     :: sindp, cosdp,dra0,sindra
  REAL(r8b)                     :: pEps,pPsi,dXcpo,dYcpo,dEcpo,dPcpo
  REAL(r8b)                     :: sinem, cosem
  REAL(r8b)                     :: sinet, coset
  REAL(r8b)                     :: dtmoon ! Correction to GMST
  REAL(r8b)                     :: xpol,ypol,ut1utc,gpsutc

  CHARACTER(LEN=fileNameLength) :: filnam

  CHARACTER(LEN=chrValLength), SAVE :: nutMod, prcMod
  CHARACTER(LEN=8) :: srNget
  REAL(r8b)        :: numVal

! Pi defined?
! -----------
  IF (const_def /= 1) THEN
    WRITE(lfnerr,'(A)') 'SR NUTMTX: Dear programmer, ' // &
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

! Get nutMod- and prcMod-string
! -----------------------------
    CALL getModKey(mod_orb_nutMod,nutMod,srNget,numVal)
    CALL getModKey(mod_orb_prcMod,prcMod,srNget,numVal)

    ifirst = 0
  ENDIF

  nutnam=nutat%nutnam

! SR was only called to get name of nutation model
! ------------------------------------------------
  IF (xtdb.EQ.-999) THEN
     RETURN
  ENDIF

! Eps - zero epoch J2000 (rad)
! ----------------------------
  CALL OBLIQU(xtdb,eps0,epsA)

! Compute model corrections dPsi, dEps
! ------------------------------------
  CALL nutcor(xtdb,nutat,tu,dPsi,dEps,dtmoon)

! Celestial pole offsets in radians
! ---------------------------------
  IF (icpo == 2) THEN
    CALL poldef2(xtdb,0,2,xpol,ypol,ut1utc,gpsutc,dEcpo,dPcpo)
    dYcpo=dEcpo
    dXcpo=dPcpo*DSIN(epsA)
  ELSEIF (icpo == 1) THEN
    CALL getcpo(xtdb,tt,tteps,ttpsi)

    CALL litpol(2,1,tt,tteps,xtdb,0d0,ddeps)
    CALL litpol(2,1,tt,ttpsi,xtdb,0d0,ddpsi)
    dYcpo=ddeps(1)
    dXcpo=ddpsi(1)*DSIN(epsA)
  ELSE
    dXcpo=0D0
    dYcpo=0D0
  ENDIF

! Equation of equinox
! -------------------
  eqequi = dpsi*DCOS(epsA) + dtmoon

! Frame bias matrix
! -----------------
  dra0   = -0.01460D0/ars
  pPsi   = nutat%nutpre(1)*DSIN(eps0)/ars +dXcpo
  pEps   = nutat%nutpre(2)/ars +dYcpo

  sindra = DSIN(dra0)
  sindp  = DSIN(pPsi)
  sinet  = DSIN(pEps)

  bias(1,1) = 1.D0
  bias(2,1) = -sindra
  bias(3,1) = +sindp
  bias(1,2) = +sindra
  bias(2,2) = 1.D0
  bias(3,2) = +sinet
  bias(1,3) = -sindp
  bias(2,3) = -sinet
  bias(3,3) = 1.D0

! Precession rate (not required for IERS2003 and IERS2010 precession)
! -------------------------------------------------------------------
!!!!!! Activation for V50 !!!!!!!!!!!!!!!!!!!!!!!!!
  IF (nutMod(1:10) /= 'IAU2000R06' .AND. nutMod(1:7) /= 'IAU2006' .AND.&
      prcMod(1:4) == 'V50 ') THEN
    dPsi = dPsi + (nutat%nutpre(1)+nutat%nutpre(3)*tu )*pi/648000d0
    dEps = dEps + (nutat%nutpre(2)+nutat%nutpre(4)*tu )*pi/648000d0

! Bias matrix => Unit matrix
    bias(1:3,1:3)=0D0
    DO ii=1,3
      bias(ii,ii)=1D0
    ENDDO
  ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! True obliquity epstru
! ---------------------
  epstru = epsA + deps

! Sine and cosine
! ---------------
  sindp = DSIN(dpsi)
  cosdp = DCOS(dpsi)
  sinem = DSIN(epsA)
  cosem = DCOS(epsA)
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Equation of equinox for V50
! ---------------------------
  IF (nutMod(1:10) /= 'IAU2000R06' .AND. nutMod(1:7) /= 'IAU2006' .AND.&
   &  prcMod(1:4) == 'V50 ') THEN
    eqequi = nut(2,1) + dtmoon
  ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  RETURN

END SUBROUTINE nutmtx


END MODULE
