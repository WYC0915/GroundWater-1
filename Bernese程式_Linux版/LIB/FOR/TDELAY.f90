MODULE s_tdelay
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE TDELAY(TMET,ZEN,XSTELL,ITROPO,metex,WL,TEMP,PRESS,HUM,DR)

! -------------------------------------------------------------------------
! Purpose:    Compute tropospheric slant delay
!
! Remarks:
!
! Author:     A. GAEDE
!
! Created:    22-Aug-2006
!
! Changes:    30-Jun-2008 RD: VMF added
!             04-Jun-2010 PS: Comments added
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_const, ONLY: HREF, HUMREF, PREF, TREF
  USE d_grid,  ONLY: getGrid,grd_vmf1_zh,grd_vmf1_zw
  USE s_tropos
  USE s_gpt
  USE s_trpmap
  USE s_metex1
  USE f_vmf1ell

  IMPLICIT NONE
!
! Variables in parameter list
! ---------------------------
! IN:
  REAL(r8b)               :: TMET      ! Epoch in MJD
  REAL(r8b)               :: ZEN       ! Zenith distance in RAD
  REAL(r8b), DIMENSION(3) :: XSTELL    ! Ellipsoidal station coordinates
                                       ! 1: Latitude in RAD
                                       ! 2: Longitude in RAD
                                       ! 3: Height in M
  INTEGER(i4b)            :: ITROPO    ! Troposphere model
  INTEGER(i4b)            :: metex     ! Extrapolation of meteo data (==1)
  REAL(r8b)               :: WL        ! Wave length factor for Marini-Murray
! IN/OUT:
  REAL(r8b)               :: TEMP      ! Temperature in CELSIUS
  REAL(r8b)               :: PRESS     ! Pressure in MBAR
  REAL(r8b)               :: HUM       ! Humidity in %
! OUT:
  REAL(r8b)               :: DR        ! Tropospheric slant delay in M

! Local Variables
! ---------------
  INTEGER(i4b)                    :: ITR0
  REAL(r8b)                       :: ZEN0
  REAL(r8b)                       :: DRall
  REAL(r8b)                       :: undu
  REAL(r8b)                       :: zh, zw
  REAL(r8b),DIMENSION(2)          :: MAPFUN
  REAL(r8b),DIMENSION(2)          :: MAPDRY,MAPWET
  INTEGER(i4b)                    :: ITRMP0



  IF (metex == 1) CALL METEX1(TREF,PREF,HUMREF,XSTELL(3)-HREF, &
                              TEMP,PRESS,HUM)
!
  IF (ITROPO.EQ.5 .OR. ITROPO.EQ.15) THEN
! SAASTAMOINEN DELAY, BUT NIELL DRY MAPPING
    ZEN0=0.D0
    ITR0=ITROPO-4
    CALL TROPOS(ZEN0,XSTELL,TEMP,PRESS,HUM,ITR0,WL,DR)
    ITRMP0=3
    CALL TRPMAP(ITRMP0,TMET,XSTELL,ZEN,MAPFUN)
    DR = DR*MAPFUN(1)
!
  ELSEIF (ITROPO == 6 .OR. ITROPO ==16) THEN
! SAASTAMOINEN DELAY, BUT GMF MAPPING (DRY or DRY + WET)
    IF (metex == 1) CALL GPT(TMET,XSTELL(1),XSTELL(2), &
                             XSTELL(3),PRESS,TEMP,undu)
    ZEN0=0D0
    ITR0=ITROPO-5
    CALL TROPOS(ZEN0,XSTELL,TEMP,PRESS,0D0,ITR0,WL,DR)
    ITRMP0=5
    CALL TRPMAP(ITRMP0,TMET,XSTELL,ZEN,MAPDRY)
    IF (ITROPO == 16) THEN
      DR = DR*MAPDRY(1)
    ELSE
      CALL TROPOS(ZEN0,XSTELL,TEMP,PRESS,HUM,ITR0,WL,DRall)
      ITRMP0=6
      CALL TRPMAP(ITRMP0,TMET,XSTELL(1),ZEN,MAPWET)
      DR = DR*MAPDRY(1)+(DRall-DR)*MAPWET(1)
    ENDIF

! VMF1
  ELSEIF (ITROPO == 7 .OR. ITROPO ==17) THEN
! VMF1 from grid
    zh = vmf1ell('ZH', getGrid (grd_vmf1_zh, tmet, xstell), xstell)

    ITRMP0=7
    CALL TRPMAP(ITRMP0,TMET,XSTELL,ZEN,MAPDRY)
    IF (ITROPO == 17) THEN
      DR = ZH*MAPDRY(1)
    ELSE
      zw = vmf1ell('ZW', getGrid (grd_vmf1_zw, tmet, xstell), xstell)

      ITRMP0=8
      CALL TRPMAP(ITRMP0,TMET,XSTELL,ZEN,MAPWET)

      DR = ZH*MAPDRY(1)+ZW*MAPWET(1)
    ENDIF
!
  ELSE
! DELAY defined in TROPOS
    CALL TROPOS(ZEN,XSTELL,TEMP,PRESS,HUM,ITROPO,WL,DR)
  ENDIF

  RETURN
END SUBROUTINE TDELAY
END MODULE s_tdelay
