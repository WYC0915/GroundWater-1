MODULE s_ACCALB
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 4.3
! -------------------------------------------------------------------------

  SUBROUTINE accalb(albedo,nlon,nlat,xsat,xsun,rsw, &
                    rprfac,leonum,modtyp,falbed)

! -------------------------------------------------------------------------
!!
!! NAME       : ACCALB
!!
!! PURPOSE    : ACCELERATION DUE TO EARTH ALBEDO
!!
!!              The part of the Earth visible by the satellite is divided
!!              into segments. For each segment (l,m) the direction and
!!              magnitude of the acceleration is computed assuming Lambertian
!!              scattering properties of the surface element using
!!
!!                acc(l,m) = albedo * rprfac * area(l,m) *
!!                           * dist(l,m)**2 / dist(Sun)**2 *
!!                           * cos(normal(l,m)-Sun) * cos(normal(l,m)-Sat)
!!
!!              where
!!
!!                rprfac = 1/2 * S * C * Q/m
!!
!!              The accelerations for the (illuminated) surface elements
!!              are summed up to generate the total acceleration due to
!!              Earth albedo.
!!              Currently only a black ball satellite model is implemented.
!!              A spherical Earth is assumed.
!!
!! PARAMETERS :
!!         IN : ALBEDO  : Albedo factor                                R*8
!!              NLON    : Number of segments in longitude              I*4
!!              NLAT    : Number of segments in latitude               I*4
!!              XSAT    : Satellite position in J2000                  R*8(3)
!!              XSUN    : Sun position in J2000                        R*8(3)
!!              RSW     : Transformation matrix from RSW to J2000      R*8(3,3)
!!                        i.e., (er|,es|,ew|)
!!              RPRFAC  : Radiation pressure factor 1/2*S*C*Q/m        R*8
!!              LEONUM  : Number of LEO, currently unused              I*4
!!              MODTYP   : Satellite model                             I*4
!!                        =1: Black ball model
!!        OUT : FALBED  : Acceleration due to Earth albedo in J2000    R*8(3)
!!
!! AUTHOR     :  U. Hugentobler
!!
!! VERSION    :  4.3
!!
!! CREATED    :  27-Oct-2000
!!
!! CHANGES    :  19-Sep-2012 RD: Save allocate of the arrays
!!               19-Sep-2012 RD: use M_BERN with ONLY
!!               24-SEP-2012 RD: TAKE AU FROM D_CONST
!!
!! COPYRIGHT  :  ASTRONOMICAL INSTITUTE
!!      2000      UNIVERSITY OF BERN
!!                    SWITZERLAND
!!-------------------------------------------------------------------------

  USE m_bern,  ONLY: i4b, r8b
  USE d_const, ONLY: pi, ae, au

  USE s_dmlmtv
  USE s_alcerr
  USE s_dmlmav
  IMPLICIT NONE
!
! Parameters
! ----------
  REAL(r8b)                :: albedo                ! Earth albedo
  INTEGER(i4b)             :: nlon                  ! number of segments in lon
  INTEGER(i4b)             :: nlat                  ! number of segments in lat
  REAL(r8b),DIMENSION(*)   :: xsat                  ! satellite position
  REAL(r8b),DIMENSION(4)   :: xsun                  ! Sun direction and distance
  REAL(r8b),DIMENSION(3,3) :: rsw                   ! transformation matrix
  REAL(r8b)                :: rprfac                ! radiation pressure factor
  INTEGER(i4b)             :: leonum                ! number of the LEO
  INTEGER(i4b)             :: modtyp                ! satellite model type
  REAL(r8b),DIMENSION(3)   :: falbed                ! total acceleration

! Local variables
! ---------------
  INTEGER(i4b)             :: l,m                   ! indices of segments
  INTEGER(i4b)             :: iac
  REAL(r8b)                :: rsat,dsat,cospsi      ! scalars
  REAL(r8b)                :: cospl,sinpl,areal     !
  REAL(r8b)                :: cossun,cossat,sunfac  !
  REAL(r8b)                :: psi,rho,lon           ! angles
  REAL(r8b)                :: phi,phi1,phi2         !
  REAL(r8b)                :: dh                    !
  REAL(r8b),DIMENSION(3)   :: ssun                  ! sun direction in RSW
  REAL(r8b),DIMENSION(3)   :: nvec                  ! surface element normal
  REAL(r8b),DIMENSION(3)   :: tsat                  ! topocentr. satellite pos

  LOGICAL,SAVE                            :: first= .TRUE.
  INTEGER(i4b),SAVE                       :: nlon0
  REAL(r8b),SAVE                          :: dlon
  REAL(r8b),DIMENSION(:),ALLOCATABLE,SAVE :: coslm  ! cosine of lambda(m)
  REAL(r8b),DIMENSION(:),ALLOCATABLE,SAVE :: sinlm  ! sine of lambda(m)

 CHARACTER(LEN=6), PARAMETER :: srname = 'ACCALB'     ! subroutine name


! Vectors in RSW, Scalars, Angles
! -------------------------------
! ..Sun direction in RSW
  CALL dmlmtv(xsun(1:3),rsw,ssun)
  ssun   = ssun/xsun(4)

! ..distance of satellite in Earth radii
  rsat   = SQRT(DOT_PRODUCT(xsat(1:3),xsat(1:3)))/ae

! ..geocentric angle betwen direction to Sun and Satellite
  cospsi = ssun(1)
  psi    = ACOS(cospsi)

! Sun radiation scale factor
! --------------------------
!  sunfac = (1.49597870D+11/xsun(4))**2
  sunfac = (AU/xsun(4))**2

! Geocentric angle to satellite horizon
! -------------------------------------
  rho    = ACOS(1/rsat)

! Satellite horizon completely on dark side of terminator
! -------------------------------------------------------
  IF (psi >= rho + pi/2d0) THEN
    falbed = 0d0

! Compute Albedo Acceleration
! ***************************
  ELSE
    falbed = 0d0

! Allocate arrays and compute constants
! -------------------------------------
    IF (first .OR. nlon /= nlon0) THEN

! deallocate if nlat or nlon changed
      IF (.NOT.first) THEN
        IF ( ALLOCATED(coslm) ) DEALLOCATE(coslm)
        IF ( ALLOCATED(sinlm) ) DEALLOCATE(sinlm)
      ENDIF

! allocate
      ALLOCATE(coslm(nlon),stat=iac)
      CALL alcerr(iac,'coslm',(/nlon/),srname)
      ALLOCATE(sinlm(nlon),stat=iac)
      CALL alcerr(iac,'sinlm',(/nlon/),srname)

! compute cos and sin of lambda(l)
      dlon = 2*pi/nlon
      DO m = 1,nlon
        lon = m * dlon
        coslm(m) = COS(lon)
        sinlm(m) = SIN(lon)
      ENDDO

! save dimensions
      first=.FALSE.
      nlon0=nlon
    ENDIF

! compute cos and sin of phi(l) and area(l,m)
    dh   = SIN(rho)/(nlat+0.5d0)
    phi1 = ASIN(dh*0.5d0)

! loop over sectors l
    DO l = 1,nlat

      IF (l == 1) THEN
        cospl = 1d0
        sinpl = 0d0
        areal =     pi*(dh/2)**2
      ELSE
        phi2  = ASIN(dh*(l-0.5d0))
        phi   = (phi2+phi1)/2d0
        cospl = COS(phi)
        sinpl = SIN(phi)
        areal = (phi2-phi1)*sinpl*dlon
        phi1  = phi2
      ENDIF

! loop over sectors m
      DO m = 1,nlon

! ..special case for l=1: circular area in nadir direction of satellite
        IF (l == 1) THEN
          IF (m /= 1) EXIT
          cossun = ssun(1)
          cossat = 1d0
          tsat(1) = 1d0
          tsat(2) = 0d0
          tsat(3) = 0d0
          dsat    = rsat-1

! ..cases l>1
        ELSE
          nvec(1) = cospl
          nvec(2) = sinpl*coslm(m)
          nvec(3) = sinpl*sinlm(m)

! area illuminated by Sun?
          cossun = DOT_PRODUCT(nvec,ssun)

          IF (cossun > 0d0) THEN
            tsat   = (/rsat,0d0,0d0/) - nvec
            dsat   = SQRT(DOT_PRODUCT(tsat,tsat))
            tsat   = tsat/dsat
            cossat = DOT_PRODUCT(tsat,nvec)
          ENDIF
        ENDIF

! ..surface area illuminated
        IF (cossun > 0d0) THEN
          falbed = falbed + &
                   albedo*rprfac*sunfac*areal*cossun*cossat*tsat/dsat**2
        ENDIF

      ENDDO
    ENDDO

! transform to J2000
! ------------------
    CALL dmlmav(falbed,rsw,falbed)

  ENDIF

  RETURN

  END SUBROUTINE accalb

END MODULE
