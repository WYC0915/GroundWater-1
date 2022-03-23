MODULE s_ECLMOON
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE eclmoon(xsat,xmoon,xsun,frac,fecl,distp,distu)

! ------------------------------------------------------------------------
! Purpose:    Compute fraction of sunlight due (penumbral) eclipse by Moon
!
! Author:     U. Hugentobler
!
! Created:    14-Dec-2001
! Last mod.:  19-May-2011
!
! Changes:    29-Dec-2001 HU: Use d_const
!             29-Dec-2001 HU: Compute distp and distu
!             28-Jan-2011 SL: use m_bern with ONLY
!             19-May-2011 HB: Declarations with (:) instead of (*)
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: r8b
  USE d_const,  ONLY: pi
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  REAL(r8b),DIMENSION(:):: xsat     ! Position of satellite (i=1,...,3)
  REAL(r8b),DIMENSION(:):: xmoon    ! Position of Moon (i=1,...,3) and
  REAL(r8b),DIMENSION(:):: xsun     ! Position of Sun (i=1,...,3) and
                                    ! All vectors are assumed to be in the
                                    ! same system, all units in meter
! output:
  REAL(r8b)             :: frac     ! Fraction of sunlight
                                    ! (1: no eclipse, 0: umbral eclipse)
  REAL(r8b)             :: fecl     ! Magnitude of eclipse (fraction of
                                    ! Sun's diameter covered by the Moon)
  REAL(r8b)             :: distp    ! distance to penumbra phase (rad)
  REAL(r8b)             :: distu    ! distance to umbra or annular phase (rad)

! Local variables
! ---------------
  REAL(r8b)             :: radmon = 1738.D3  ! Radius of Moon in meters
  REAL(r8b)             :: radsun =  696.D6  ! Radius of Sun in meters

  REAL(r8b)             :: rho
  REAL(r8b)             :: dmoon,dsun,dmoon2,dsun2,xm,xs
  REAL(r8b)             :: angl,aream,areas
  REAL(r8b),DIMENSION(3):: rmoon,rsun


! Angular distance of Sun and Moon
! --------------------------------
  rmoon(1:3) = xmoon(1:3)-xsat(1:3)
  dmoon      = SQRT(DOT_PRODUCT(rmoon,rmoon))

  rsun(1:3)  = xsun(1:3) -xsat(1:3)
  dsun       = SQRT(DOT_PRODUCT(rsun,rsun))

  rho = ACOS(DOT_PRODUCT(rmoon,rsun)/dmoon/dsun)

! Angular diameter of Sun and Moon
! --------------------------------
  dmoon = radmon/dmoon
  dsun  = radsun/dsun

! Distance to shadow phase
! ------------------------
  distp = rho - (dmoon+dsun)
  distu = rho - ABS(dmoon-dsun)

! No eclipse
! ----------
  IF (distp >= 0D0) THEN
    frac=1D0
    fecl=0D0
  ELSE
    dmoon2 = dmoon**2
    dsun2  = dsun**2

! Eclipse
! -------
! central
    IF (distu <= 0D0) THEN
      frac = MAX(0D0,1-dmoon2/dsun2)
      fecl = MIN(1D0,dmoon/dsun)
    ELSE

! non-central
      xm = (rho-(dsun2-dmoon2)/rho)/2D0
      xm = MIN(xm,+dmoon)
      xm = MAX(xm,-dmoon)

      xs = rho-xm
      xs = MIN(xs,+dsun)
      xs = MAX(xs,-dsun)

      angl  = ACOS(xm/dmoon)
      aream = dmoon2*angl-xm*SQRT(dmoon2-xm**2)
      angl  = ACOS(xs/dsun)
      areas = dsun2*angl-xs*SQRT(dsun2-xs**2)

      frac  = 1-(aream+areas)/pi/dsun**2
      fecl  = (dmoon+dsun-rho)/dsun/2D0
    ENDIF
  ENDIF

  RETURN

  END SUBROUTINE eclmoon

END MODULE
