  MODULE s_leosky
  CONTAINS

! ------------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! ------------------------------------------------------------------------------

  SUBROUTINE LEOSKY(sensor,epo,ityp,irel,xleoE,xsat,sz,dt,xpol,ypol,meatyp, &
             &      xsky,dist,zen,azi,nad,azisat,svn,iorsys,azisok)

! ------------------------------------------------------------------------------
!
!  NAME       :  LEOSKY
!
!  PURPOSE    :  For a satellite given in true system of date and sensor onboard
!                LEO in Earth fixed system returns zenith distance, azimuth and
!                distance in true system as seen from sensor fixed system or
!                sensor local ellipsoidal coordinate system.
!                Additionally returns leocentric vector in true system.
!                Additionally returns nadir angle and azimuth at the GPS
!                satellite (LEO seen from the GPS satellite!).
!
!
!  SR CALLED  :  ECCELL, LEOROT, TRUERTH
!
!
!  REMARKS    :  - In the case that attitude information is not available for
!                  specific epoch, velocity vector should be in xleo(4:6)
!                - Measurement type is dummy parameter.
!                - The same parameter for linear eccentricity FELL is used in
!                  TOPSTA.f (approximation)
!                - !!! in order to avoid frequent transformation of the
!                    LEO velocity xleoE contains velocity in true system
!                -  LEO coordinates taken at TOBS+DT in PRCEPO
!                -  GPS satellite coordinates taken at TOBS in PRCEPO
!
!
!  AUTHOR     :  D. Svehla
!
!
!  CREATED    :  22-Mar-2001
!  LAST.MOD.  :  01-Oct-2010
!
!  CHANGES    :  20-Nov-2001 DS: Add line xLeo(4:9) = xleoE(4:9)
!                15-Dec-2001 HU: Add d_const, use implicit none
!                07-Jan-2003 DS: Shapiro effect (space) for
!                                LEO and GPS satellites
!                11-Aug-2003 RS: Add calculation of nad and azisat
!                24-Nov-2003 HU: Nadir angle test at 15.0 -> 17.5
!                10-May-2004 DS: Correction of LEO position due to clock correction
!                10-May-2004 DS: Add GPS antenna azimuth computation for GPS antenna azimuth/PCV
!                                add TOBS, SVN, IORSYS and calls of SUNEFF,VPROD, and COOTRA
!                                add computation of EX,EY and AZ, return AZISOK
!                29-Jun-2006 HB: scal outside IF for irel
!                17-Aug-2006 HU: Converted to module, small modif.
!                14-Nov-2006 HB: Activate Shapiro correction
!                01-Oct-2010 CR: New call of SUNEFF
!
!  COPYRIGHT  :  ASTRONOMICAL INSTITUTE
!                 UNIVERSITY OF BERN
!                     SWITZERLAND
! ------------------------------------------------------------------------------

  USE m_bern
  USE d_const, ONLY: ae,c,pi,GM
  USE s_leorot
  USE s_suneff
  USE s_vprod
  USE s_cootra
  USE s_eccell
  USE s_truearth

  IMPLICIT NONE

!
! Parameters
! ==========
!
! IN :
! ----
  CHARACTER(LEN=staNameLength)    :: sensor ! Sensor name
  REAL(r8b)                       :: epo    ! Epoch in MJD
  INTEGER(i4b)                    :: ityp   ! Type of coordinate system
                                            ! =0:fixed sensor coordinate system
                                            ! =1:sensor local ellipsoidal
                                            !    coordinate system
  INTEGER(i4b)                    :: irel   ! Apply relativistic correction
                                            ! =0:Yes
                                            ! =1:No
  REAL(r8b),DIMENSION(9)          :: xleoE  ! LEO position and velocities in
                                            !   Earth fixed system, see remark
                                            !   velocity in true system,
                                            !   see remark
  REAL(r8b),DIMENSION(9)          :: xsat   ! Position of a satellite in Earth
                                            !   fixed system,
  REAL(r8b)                       :: sz     ! True siderial time
  REAL(r8b)                       :: dt     ! Clock error (s)
  REAL(r8b)                       :: xpol   ! Pole coordinates
  REAL(r8b)                       :: ypol   !   in (rad)

  INTEGER(i4b)                    :: meatyp ! Measurement type (dummy)
                                            ! =1:phase observations
                                            ! =2:code observations
  INTEGER(i4b)                    :: SVN    ! Satellite number
  INTEGER(i4b)                    :: IORSYS ! Orbit system
                                            !  =1: B1950.0
                                            !  =2: J2000.0
!
! OUT:
! ----
  REAL(r8b),DIMENSION(3)          :: xsky   ! Leocentric (sensor) vector
                                            !   in true system
  REAL(r8b)                       :: dist   ! Distance                 (m)
  REAL(r8b)                       :: zen    ! Zenith distance         (rad)
  REAL(r8b)                       :: azi    ! Azimuth                 (rad)
  REAL(r8b)                       :: nad    ! Nadir angle (LEO seen from GPS
                                            ! satellite)              (rad)
  REAL(r8b)                       :: azisat ! Azimuth (LEO seen from GPS
                                            ! satellite)              (rad)
  REAL(r8b)                       :: AZISOK ! Corrected azimuth angle
                                            ! (LEO seen from GPS satellite);
                                            ! clockwise from the Y-axis
                                            ! when looking towards the Z-axis;
                                            !                         (rad)


!
! Local variables
! ---------------
  REAL(r8b),PARAMETER  :: FELL=1.D0/298.2572236D0     !Parameter from TOPSTA.f

  REAL(r8b),DIMENSION(9)          :: xleo
  REAL(r8b),DIMENSION(4)          :: xsun
  REAL(r8b),DIMENSION(3,3)        :: rotmat
  REAL(r8b),DIMENSION(3)          :: xskys,xell,dum3
  REAL(r8b),DIMENSION(3)          :: EX,EY,EZ,xsatcorr
  REAL(r8b)                       :: sz1,scal,perrel,vsat,baxsis
  REAL(r8b)                       :: ec2ecc,e2ecc,xyproj,theta,phi1,phi2
  REAL(r8b)                       :: relativ,rgps,rleo
  REAL(r8b)                       :: scal3,csnad
  REAL(r8b)                       :: REY,sz2,xpol2,ypol2,ut1gps2,prod1,prod2
  REAL(r8b)                       :: TDT
  INTEGER(i4b)                    :: ii,k
!
! Sidereal time
! -------------
  sz1=sz+dt/86400.D0*2.D0*PI*366.25D0/365.25D0
!
! LEO(sensor) coordinates in the true system of epoch
! ---------------------------------------------------
  CALL TRUEARTH(xleoE,xleo,1,0,sz1,xpol,ypol)
  xleo(4:9)=xleoE(4:9)
!
! Coordinate differences and distance in true system
! --------------------------------------------------
  dist=0.D0
  xsatcorr(1:3)=0.D0
  scal=0.D0
  scal3=0.D0
  perrel=0.D0
  vsat=0.D0
  rgps=0.D0
  rleo=0.D0
  DO ii=1,3
    xsatcorr(ii)=xsat(ii)+xsat(ii+3)*dt+xsat(ii+6)/2.D0*dt**2
    xsky(ii)=xsatcorr(ii)-xleo(ii)
    rgps=rgps+xsatcorr(ii)**2
    rleo=rleo+xleo(ii)**2

    dist=dist+xsky(ii)**2
    scal=scal+xsky(ii)*(xsat(ii+3)+xsat(ii+6)*dt)
    IF (irel==0) THEN
      vsat=xsat(ii+3)+xsat(ii+6)*dt
      perrel=perrel+2.D0*xsatcorr(ii)*vsat/C
    END IF
    scal3=scal3+xsky(ii)*(xsat(ii)+xsat(ii+3)*dt+xsat(ii+6)/2.D0*dt**2)
  END DO

  dist=DSQRT(dist)

!
! Shapiro relativistic correction between LEO and GPS satellites
! --------------------------------------------------------------
  rgps=DSQRT(rgps)
  rleo=DSQRT(rleo)
  relativ=(2.D0*GM/c/c)*DLOG((rgps+rleo+dist)/(rgps+rleo-dist))

!  IF (irel==0) THEN
!    dist=dist*(1.D0-scal/dist/C)+perrel
!  END IF

! Shapiro correction now also active
! ----------------------------------
 IF (irel==0) THEN
   dist=dist*(1.D0-scal/dist/C)+perrel+relativ
 END IF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Coordinates in sensor fixed system
! ===================================
  SELECT CASE(ityp)
  CASE(0)
!
! Get rotation matrix (from true system of epoch to sensor fixed system)
! ----------------------------------------------------------------------
    CALL LEOROT(sensor,epo,0,1,xleo,rotmat)
    xskys=matmul(rotmat,xsky)
!
! Cartesian coordinates in sensor local ellipsoidal coordinate system
! ===================================================================
  CASE(1)

! Conversion into geodetic coordinates
! ------------------------------------
    xyproj=DSQRT(xleo(1)**2+xleo(2)**2)
    IF (xyproj.NE.0.D0) THEN
      baxsis=AE*(1.D0-FELL)
      ec2ecc=(AE**2-baxsis**2)/baxsis**2
      e2ecc=(AE**2-baxsis**2)/AE**2
      theta=DATAN(xleo(3)*AE/(xyproj*baxsis))
      phi1=xleo(3)+ec2ecc*baxsis*DSIN(theta)**3
      phi2=xyproj-e2ecc*AE*DCOS(theta)**3

      xell(1)=DATAN(phi1/phi2)
      xell(2)=DATAN2(xleo(2),xleo(1))
    ELSE
      xell(1)=PI/2.D0
      xell(2)=0.D0
    END IF
    xell(3)=0.D0
    CALL ECCELL(xell,xsky,xskys)
  END SELECT
!
! Zenith distance
! ---------------
  xyproj=DSQRT(xskys(1)**2+xskys(2)**2)
  IF (xyproj.NE.0.D0 .OR. xskys(3).NE.0.D0) THEN
    zen=DATAN2(xyproj,xskys(3))
  ELSE
    zen=0.D0
  END IF
!
! Azimuth angle (GPS satellite seen from LEO and vice versa)
! ----------------------------------------------------------
  IF (xskys(1).NE.0.D0 .OR. xskys(2).NE.0.D0) THEN
    azi=DATAN2(xskys(2),xskys(1))
    azisat=DATAN2(-xskys(2),-xskys(1))
  ELSE
    azi=0.D0
    azisat=0.D0
  END IF
  IF (azi.LT.0.D0) azi=azi+2.D0*PI
  IF (azisat.LT.0.D0) azisat=azisat+2.D0*PI
!
! Nadir angle (LEO seen from GPS satellite)
! -----------------------------------------
  csnad=scal3/RGPS/dist
  IF (csnad.GT.1.D0) THEN
    csnad=1.D0
  ENDIF
  nad=DACOS(csnad)
! The following maximum value (17.5 degree) is provisional!!!
  IF (nad.GT.17.5D0/180.D0*PI) THEN
    WRITE(LFNERR,'(/,A,/,A,F7.3,A,/,A,F12.3,A,/,A,F12.3,A,/)')  &
          ' ### SR LEOSKY: NADIR ANGLE GREATER THAN 17.5 DEG',  &
          '                NADIR ANGLE: ',nad/PI*180.D0,' DEG', &
          '                RLEO:        ',RLEO/1.D3,' KM',      &
          '                RGPS:        ',RGPS/1.D3,' KM'
  ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
! Computation of unit vectors EX, EY and EZ in true system
! This part is based on SR TOPSTA
! --------------------------------------------------------
!
! Unit vector EZ
      DO k=1,3
        EZ(k)=-xsatcorr(k)/RGPS
      END DO
!
! Unit vector EY
      TDT=epo+(19.D0+32.184D0)/86400.D0
      CALL SUNEFF(IORSYS,2.D0,TDT,XSUN,DUM3)
      CALL COOTRA(IORSYS,0,epo,XSUN,sz2,xpol2,ypol2,ut1gps2)
      CALL VPROD(EZ,XSUN,EY)
      REY=DSQRT(EY(1)**2+EY(2)**2+EY(3)**2)
      DO k=1,3
        EY(k)=EY(k)/REY
      END DO
!
! Unit vector EX
      CALL VPROD(EY,EZ,EX)
!
! Computation of corrected azimuth AZISOK
! ---------------------------------------
!
! Projection of -xsky into the satellite system
      PROD1=0.D0
      PROD2=0.D0
      DO k=1,3
        PROD1=PROD1+EY(k)*(-1.D0)*xsky(k)
        PROD2=PROD2+EX(k)*(-1.D0)*xsky(k)
      END DO
!
      AZISOK=DATAN2(PROD2,PROD1)
50    CONTINUE
      IF (AZISOK.LT.0.D0) THEN
        AZISOK=AZISOK+2.D0*PI
        GOTO 50
      ENDIF
      IF (AZISOK.GE.2.D0*PI) THEN
        AZISOK=AZISOK-2.D0*PI
        GOTO 50
      ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  RETURN

  END SUBROUTINE LEOSKY

  END MODULE
