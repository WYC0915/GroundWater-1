MODULE s_ionosp2
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE ionosp2(xstat ,zen   ,meatyp, azi   ,pole , xhgt ,    &
                   svn   ,esum  ,icarr , alfxmf,xsl  , flag , tobs , &
                   hoi   ,xstell)

! -------------------------------------------------------------------------
! Purpose:    This SR computes the second and third order ionospheric
!             distance correction using global ionosphere models.
!             The formalism is based on:
!               "An improved model for the dual frequency ionospheric
!                correction of GPS observations"
!               Fritz K. Brunner and Min Gu
!               manuscripta geodaetica (1991) 16:205-214
!             and
!               "Higher-order ionospheric effects on global positioning
!                system observables and means of modeling them"
!               Sassan Bassiri and George A. Hajj;
!               manuscripta geodaetica (1993) 18:280-289
!
! Authors:    Christoph Knoefel, Mathias Fritsche
!
! Created:    20-Dec-2004
! Last mod.:  19-Nov-2010
!
! Changes:    25-Sep-2005 MF: Merge to f_MODULE
!             16-Oct-2007 MF: Bug fix magnetic field direction
!             04-Jan-2010 SL: FUNCTION->SUBROUTINE, return HOI
!             24-Jan-2010 SL: IGRF10 instead of Dipol
!             02-Feb-2010 SL: use IGRF11SYN
!             04-Mar-2010 SL: change in last comment
!             22-Sep-2010 SL/SS: ray bending term added (wrt IERS Conv. 02-Jul)
!             19-Nov-2010 SL: unused types/variables removed
!
! SR called:  ddreh, igrf11syn, jmt
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b,r8b
  USE d_const,  ONLY: c,conre,pi,rho
  USE d_satfrq, ONLY: faclin,wlgt,frq
  USE s_ddreh
  USE s_igrf11syn
  USE s_jmt

  IMPLICIT NONE

! List of Parameters
! ------------------
  REAL(r8b), DIMENSION(3) :: hoi     ! higher-order ionosphere parameters [m]
                                     ! (1): second order
                                     ! (2): third order
                                     ! (3): ray bending

! input:
  REAL(r8b), DIMENSION(3) :: xstat   ! Station coordinates (geographic frame)
  REAL(r8b), DIMENSION(3) :: xstell  ! Station coordinates
  REAL(r8b), DIMENSION(2) :: pole    ! (1): Latitude of geomagnetic pole
                                     ! (2): East longitude of geomagnetic pole
  REAL(r8b)               :: zen     ! Zenith distance [rad]
  REAL(r8b)               :: azi     ! Azimuth angle [rad]
                                     ! (from north to east in geographic frame)
  INTEGER(i4b)            :: meatyp  ! Measurement type
                                     ! =1: phase observations
                                     ! =2: code observations
                                     ! =3: range observations
  REAL(r8b)               :: xhgt    ! Single-layer height in m
  INTEGER(i4b)            :: svn     ! Satellite number
  REAL(r8b)               :: esum    ! Vertical TEC in TECU
  INTEGER(i4b)            :: icarr   ! Observed carrier
                                     ! =1: L1
                                     ! =2: L2
                                     ! =3: L3 (ionosphere-free lc)
                                     ! =4: L4 (geometry-free lc)
                                     ! =5: L5 (wide-lane lc)
  REAL(r8b)               :: alfxmf  ! Mapping function factor
  REAL(r8b), DIMENSION(3) :: xsl     ! Geocentric coordinates of intersection
                                     ! point (frame of flag)
  INTEGER(i4b)            :: flag    ! Flag for reference frame
                                     ! =1: geographic
                                     ! =2: geomagnetic
  REAL(r8b)               :: tobs    ! Observation time in Julian Date

! Local parameters
! ----------------
  ! corresponding values for the electron density and the
  ! total electron content; used for a linear interpolation
  ! (K.Brunner & M.Gu page 209)
  REAL(r8b), DIMENSION(2), PARAMETER :: Tpar = (/1.38d18, 4.55d18/)
  REAL(r8b), DIMENSION(2), PARAMETER :: Npar = (/6.00d12, 2.00d13/)

  ! amplitude of the magnetic field at the earth's surface
  ! at the magnetic equator
  ! (S.Bassiri & A.Hajj page 285)
!!!  REAL(r8b)              , PARAMETER :: Bg   = 3.12d-5

  ! factors of the linearisation of the ionospheric effect
  ! (S.Bassiri & A.Hajj page 283)
  REAL(r8b)              , PARAMETER :: par1 = 7527d0
  REAL(r8b)              , PARAMETER :: par2 = 2437d0

  ! shape parameter
  ! (S.Bassiri & A.Hajj page 286)
  REAL(r8b)              , PARAMETER :: shp  = 6.6d-1

! Local Variables
! ---------------
  REAL(r8b), DIMENSION(3,3)       :: Rmat
  REAL(r8b), DIMENSION(3,3)       :: Rgcc
  REAL(r8b), DIMENSION(3,3), SAVE :: Rpol   ! geographic -> geomagnetic
  REAL(r8b), DIMENSION(2),   SAVE :: pol    ! saved pole coordinates
  REAL(r8b), DIMENSION(3)         :: xslm   ! intersection point (geomagnetic)
  REAL(r8b), DIMENSION(2)         :: arg2   ! second order correction term
  REAL(r8b), DIMENSION(2)         :: arg3   ! third order correction term
  REAL(r8b), DIMENSION(2)         :: argrb  ! ray bending correction term
  REAL(r8b), DIMENSION(3)         :: xk     ! satellite to station link
  REAL(r8b), DIMENSION(3)         :: xB     ! magnetic field vector
  REAL(r8b), DIMENSION(4)         :: xBi    ! magnetic field vector (IGRF)
  REAL(r8b), DIMENSION(3)         :: Zm     ! topocentric unit vector (zenith)
  REAL(r8b), DIMENSION(3)         :: Ym     ! topocentric unit vector (north)
  REAL(r8b)                       :: lonppt ! geomagnetic lon of ionospheric pierce point
  REAL(r8b)                       :: latppt ! geomagnetic lat of ionospheric pierce point
  REAL(r8b)                       :: r3D
  REAL(r8b)                       :: rXY
  REAL(r8b)                       :: cosp
  REAL(r8b)                       :: sinp
  REAL(r8b)                       :: cosl
  REAL(r8b)                       :: sinl
  REAL(r8b)                       :: scol
  REAL(r8b)                       :: ccol
  REAL(r8b)                       :: colat  ! geomagnetic colatitude of the
                                            ! ionosphere intersection point
  REAL(r8b)                       :: fac2
  REAL(r8b)                       :: fac3
  REAL(r8b)                       :: facrb
  REAL(r8b)                       :: Tec    ! esum*xmf*TECU [1 TECU=1d16]
  REAL(r8b)                       :: Nmax   ! maximal electron density
  REAL(r8b)                       :: dpB,dpBi

  INTEGER(i4b)  :: i
  INTEGER(i4b)  :: year,month
  REAL(r8b)     :: dom,year2,month2

  LOGICAL, SAVE :: first = .TRUE.

! Initialization
! --------------
  hoi(:) = 0d0

! Return if carrier gt 5
! ----------------------
  IF(icarr.GT.5) RETURN

! Transformation: geocentric (geographic) to geocentric (geomagnetic)
! -------------------------------------------------------------------
  IF(first .OR. pol(1)/=pole(1) .OR. pol(2)/=pole(2) )THEN
    first = .FALSE.
    pol = pole
    CALL ddreh(3,pol(2),Rmat)
    Rpol = Rmat
    CALL ddreh(2,pi/2d0-pol(1),Rmat)
    Rpol = MATMUL(Rmat,Rpol)
  ENDIF

! Link from satellite to receiver in topocentric geographic frame
! ---------------------------------------------------------------
  xk(1) = -1d0*DCOS(azi)*DSIN(zen)   ! North
  xk(2) = -1d0*DSIN(azi)*DSIN(zen)   ! East
  xk(3) = -1d0          *DCOS(zen)   ! Up

! Link from satellite to receiver in geocentric geographic frame
! --------------------------------------------------------------
  r3D  = DSQRT(DOT_PRODUCT(xstat,xstat))
  rXY  = DSQRT(xstat(1)**2+xstat(2)**2)
  cosp = rXY      / r3D
  sinp = xstat(3) / r3D
  cosl = xstat(1) / rXY
  sinl = xstat(2) / rXY
  Rmat = reshape(source =               &
         (/ -sinp*cosl, -sinp*sinl, cosp, &
                 -sinl,       cosl, 0.d0, &
             cosp*cosl,  cosp*sinl, sinp  /),shape = (/3,3/))
  xk = MATMUL(Rmat,xk)

! Link from satellite to receiver in geocentric geomagnetic frame
! ---------------------------------------------------------------
  xk = MATMUL(Rpol,xk)

! Colatitude of intersection point
! --------------------------------
  IF(flag == 1)THEN         ! vector xsl is related to geographic frame
    xslm = MATMUL(Rpol,xsl)
  ELSE                      ! vector xsl is related to geomagnetic frame
    xslm = xsl
  END IF
  colat = pi/2d0 - DATAN2(xslm(3),DSQRT(xslm(1)**2+xslm(2)**2))

! Setup topocentric (geomagnetic) unit vectors:
! Y-north, X-east, Z-up
! --------------------------------------
  ! Zm to geomagnetic zenith direction
  Zm(:) = 0d0
  Zm(3) = 1d0

  ! Ym to geomagnetic north direction
  Ym(:) = 0d0
  Ym(2) = 1d0

! Transformation from topocentric (geomagnetic) to geocentric (geomagnetic)
! directions with respect to ionospheric pierce point
! -------------------------------------------------------------------------
  lonppt = DATAN2(xslm(2),xslm(1))
  latppt = pi/2d0-colat
  CALL ddreh(1,-(pi/2d0-latppt),Rmat)
  Rgcc = Rmat
  CALL ddreh(3,-(pi/2d0+lonppt),Rmat)
  Rgcc = MATMUL(Rmat,Rgcc)

  Zm = MATMUL(Rgcc,Zm)
  Ym = MATMUL(Rgcc,Ym)

! Direction of magnetic field (dipol approximation)
! -------------------------------------------------
  ! (S.Bassiri & A.Hajj, formula 16)
  scol = DSIN(colat)
  ccol = DCOS(colat)
  DO i = 1,3
    xB(i) = scol*Ym(i)-2d0*ccol*Zm(i)
  END DO
  dpB = DOT_PRODUCT(xB,xk)

! Direction of magnetic field (IGRF implementation)
! -------------------------------------------------
  CALL JMT(tobs,year,month,dom)
  month2 = month/12d0
  year2  = year+month2+dom/30.6001/12d0
  IF(xstell(2).LT.0d0)THEN
    CALL IGRF11SYN(0,year2,1,xstell(3)/1000d0,90d0-xstell(1)*rho, &
                   360d0+xstell(2)*rho,xBi(1),xBi(2),xBi(3),xBi(4))
  ELSE
    CALL IGRF11SYN(0,year2,1,xstell(3)/1000d0,90d0-xstell(1)*rho, &
                   xstell(2)*rho,xBi(1),xBi(2),xBi(3),xBi(4))
  ENDIF
  xBi = 1d-9*xBi
  xBi(1:3) = MATMUL(Rgcc,(/xBi(2),xBi(1),-xBi(3)/))
  dpBi = DOT_PRODUCT(xBi(1:3),xk)

! Compute frequency dependend distance correction for carriers L1 and L2
! ----------------------------------------------------------------------

  ! Linear interpolation of maximum electron density corresponding to Tec*1d16
  Tec  = esum*alfxmf*1d16
  Nmax = (Npar(2)-Npar(1))/(Tpar(2)-Tpar(1))*(Tec-Tpar(2))+Npar(2)

  ! (S.Bassiri & A.Hajj)
  ! 2nd order: formulas (11)+(16)+(20-a)
  ! 3rd order: formulas (12)+(26)
  fac2 = par1/c**2*(conre/(conre+xhgt))**3*dpBi*Tec
!!!  fac2 = par1/c**2*Bg*(conre/(conre+xhgt))**3*dpB*Tec
  fac3 = par2/c**4*shp*Nmax*Tec

  ! (IERS Conventions 02-Jul-2010)
  ! ray bending: formula (9.37), units = mm*(MHz)**4 -> m*Hz**4
  facrb = 1d21*2.495d8*((1d0-0.8592d0*COS(pi/2d0-zen)**2)**(-0.5d0)-1d0)* &
          (esum*alfxmf)**2

  DO i = 1,2
    arg2(i) = fac2 * wlgt(i,svn)**3
    arg3(i) = fac3 * wlgt(i,svn)**4
    argrb(i) = facrb / frq(i,svn)**4
  END DO

  ! Code or Phase measurement observation?
  IF(meatyp == 1)THEN
    DO i = 1,2
      arg2(i) = arg2(i)/2d0
      arg3(i) = arg3(i)/3d0
      argrb(i) = argrb(i)/3d0
    END DO
  END IF

! Perform linear combination and compute total effect
! ---------------------------------------------------
  hoi(1) = faclin(icarr,1,svn)*arg2(1) + faclin(icarr,2,svn)*arg2(2)
  hoi(2) = faclin(icarr,1,svn)*arg3(1) + faclin(icarr,2,svn)*arg3(2)
  hoi(3) = faclin(icarr,1,svn)*argrb(1) + faclin(icarr,2,svn)*argrb(2)

! Switch sign due to SR PRANGE: meatyp=1 (phase) DIST=DIST+DR
!                               meatyp=2 (code)  DIST=DIST-DR
! -----------------------------------------------------------
  hoi = -1d0*hoi

END SUBROUTINE ionosp2

END MODULE
