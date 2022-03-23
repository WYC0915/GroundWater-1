MODULE s_SHADOW1
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE shadow1(xsat,xsun,ishad,dist)

! -------------------------------------------------------------------------
! Purpose:    Decide whether or not a space vehicle is
!             in sun light or in earth shadow
!
! Remark:     Flattening of the Earth is considered
!
! Author:     U. Hugentobler
!
! Created:    29-Dec-2001
! Last mod.:  __-___-____
!
! Changes:    __-___-____ __:
!
! SR used:    ---
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern

  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  REAL(r8b),DIMENSION(*)     :: xsat       ! Geocentric coordinates of the
                                           ! satellite, meter, i=1,2,3.
  REAL(r8b),DIMENSION(*)     :: xsun       ! Geocentric coordinates of the
                                           ! Sun, meter, i=1,2,3.
! output:
  INTEGER(i4b)               :: ishad      ! =0: satellite is in sunlight
                                           ! =1: satellite is in Earth-shadow
  REAL(r8b)                  :: dist       ! Distance to shadow cylinder
                                           ! (meter, <0: within shadow)

! Local Variables
! ---------------
  REAL(r8b),PARAMETER        :: ae = 6378136.D0  ! Aequatorial radius of Earth
! REAL(r8b),PARAMETER        :: ae = 6367395.1D0 ! Average radius of Earth
  REAL(r8b),PARAMETER        :: flat = 298.257D0 ! Flattening of Earth

  REAL(r8b),DIMENSION(3)     :: zsat,zsun
  REAL(r8b)                  :: rsun,rsat2,rcos,rsin,fac

! Apply flattening
  zsat(1:3)=xsat(1:3)
  zsun(1:3)=xsun(1:3)
  fac=1+1D0/flat
  zsat(3)=zsat(3)*fac
  zsun(3)=zsun(3)*fac

  rsun  = SQRT(DOT_PRODUCT(zsun,zsun))
  rsat2 = DOT_PRODUCT(zsat,zsat)

  rcos  = DOT_PRODUCT(zsat,zsun)/rsun
  rsin  = SQRT(rsat2-rcos**2)

  ishad= 0

! Behind terminator
  IF (rcos < 0D0) THEN
    dist = rsin-ae
    IF (dist < 0D0) ishad=1
  ELSE
    dist = SQRT(rsat2)-ae
  ENDIF

  RETURN
END SUBROUTINE shadow1

END MODULE
