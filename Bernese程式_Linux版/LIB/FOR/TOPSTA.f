      MODULE s_TOPSTA
      CONTAINS

C*
      SUBROUTINE TOPSTA(XSTAT ,XVSAT ,SZ    ,DT    ,XPOL  ,YPOL  ,
     1                  XTOPO ,DIST  ,ZEN   ,AZI   ,MEATYP,NAD   ,
     2                  AZISAT,TOBS  ,SVN   ,IORSYS,AZISOK,IREL2)
CC
CC NAME       :  TOPSTA
CC
CC PURPOSE    :  THIS SR CALCULATES THE TOPOCENTRIC POSITION OF A
CC               SATELLITE AS SEEN FROM STATION NR. I . ZENITH
CC               DISTANCE, NADIR ANGLE AND AZIMUTH ARE RETURNED, TOO.
CC
CC PARAMETERS :
CC         IN :  XSTAT  : MATRIX CONTAINING COORDINATES OF    R*8(3)
CC                        GROUND STATIONS (EARTH-FIXED
CC               XVSAT  : VECTOR CONTAINING GEOC.POSITION,    R*8(9)
CC                        VELOCITY, AND ACCEL. OF SATELLITE
CC                        (TRUE EQUATORIAL SYSTEM OF DATE)
CC               SZ     : TRUE SIDERIAL TIME (GREENWICH)      R*8
CC               DT     : CLOCK ERROR (SECONDS)               R*8
CC               XPOL   : X-POLE-COORDINATE (RADIAN)          R*8
CC               YPOL   : Y-POLE-COORDINATE (RADIAN)          R*8
CC               MEATYP : MEASUREMENT TYPE                    I*4
CC                        =1: PHASE OBSERVATIONS
CC                        =2: CODE OBSERVATIONS
CC                        =3: RANGE OBSERVATIONS
CC               TOBS   : OBSERVATION TIME                    R*8
CC               SVN    : SATELLITE NUMBER                    I*4
CC               IORSYS : ORBIT SYSTEM                        I*4
CC                        =1: B1950.0
CC                        =2: J2000.0
CC               IREL2  : FLAG FOR PERIODIC RELATIVISTIC J2   I*4
CC                        CORRECTION
CC        OUT :  XTOPO  : VECTOR CONTAINING TOPOC. VECTOR     R*8(3)
CC                        (SAME SYSTEM AS XVSAT)
CC               DIST   : TOPOC. DISTANCE                     R*8
CC               ZEN    : ZENITH-DISTANCE OF SATELLITE (RAD)  R*8
CC               AZI    : AZIMUTH ANGLE (FROM NORTH TO EAST,  R*8
CC                        IN RADIAN)
CC               NAD    : NADIR ANGLE (RECEIVER SEEN FROM THE R*8
CC                        SATELLITE; RAD)
CC               AZISAT : AZIMUTH ANGLE (RECEIVER SEEN FROM   R*8
CC                        THE SATELLITE; RAD)
CC               AZISOK : CORRECTED AZIMUTH ANGLE (RECEIVER   R*8
CC                        SEEN FROM THE SATELLITE; ANGLE
CC                        COUNTS CLOCKWISE FROM THE Y-AXIS
CC                        WHEN LOOKING TOWARDS THE Z-AXIS;
CC                        RAD)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/03 11:55
CC
CC CHANGES    :  06-APR-92 : ??: PROBLEM WITH "ZEN" IF SATELLITE IN ZENITH
CC               01-OCT-92 : ??: IMPROVED COMPUTATION OF "ZEN" (USING
CC                               GEOGRAPHICAL COORDINATES)
CC               08-APR-94 : MR: COMPUTE AND RETURN AZIMUTH ANGLE
CC               26-MAR-96 : TS: CHANGES FOR ZERO DIFFERENCES AND SLR
CC               14-NOV-02 : RS: ADD CALCULATION OF NADIR ANGLE
CC               29-JAN-03 : DS: RELATIVISTIC RANGE CORRECTION IMPLEMENTED
CC               07-MAR-03 : DS: NO RELATIVITY PROPAGATION TERM BECAUSE OF
CC                               SCALE
CC               31-MAR-03 : DS: DO NOT CHECK MAX NADIR ANGLE IF MEATYP=3
CC               17-APR-03 : MR: WRITE ERROR MESSAGE TO LFNERR
CC               28-MAY-03 : SS: CHECK FOR NADIR ANGLE OF 14.5 DEG
CC               24-NOV-03 : HU: CHECK FOR NADIR ANGLE OF 14.7 DEG
CC               05-MAR-03 : HU: NO TRANSFORMATION TCG TO TT
CC               07-APR-04 : RS: ADD TOBS, SVN AND IORSYS, ADD CALLS OF
CC                               SUNEFF, VPROD AND COOTRA, ADD COMPUTATION
CC                               OF EX, EY AND EZ, RETURN AZISOK
CC               14-JUL-04 : HU: RELATIVISTIC RANGE CORRECTION NOT APPLIED
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               09-NOV-06 : AG: RELATIVISTIC RANGE CORRECTION APPLIED
CC               01-OCT-10 : CR: NEW CALL OF SUNEFF
CC               25-OCT-10 : CR: PERIODIC RELATIVISTIC J2-CORRECTION (IREL2)
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE d_const, ONLY: C, GM, OMEGA, PI, AE, J2
      USE s_eccell
      USE s_suneff
      USE s_vprod
      USE s_cootra
      USE s_eccell
      USE s_xyzele
      USE s_arglat
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 K      , IREL2
C
      REAL*8    AZI    , CS     , DIST   , DT     , FELL   , PERREL ,
     1          RELATIV, RSAT   , RSTA   , SCAL   , SCAL2  , SS     ,
     2          SZ     , SZ1    , TOP    , VH1    , VH2    , VSAT   ,
     3          XH1    , XH2    , XH3    , XL12   , XLAT   , XPOL   ,
     4          YPOL   , ZEN
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (FELL=1.D0/298.2572236D0)
C
      INTEGER*4  MEATYP,SVN,IORSYS
C
      REAL*8     XSTAT(*),XVSAT(*),XTOPO(*),Y(3),XELL(3),XLOC(3),V(3),LG
      REAL*8     SCAL3,CSNAD,NAD,AZISAT,AZISOK,TOBS,TDT,rot(3,3)
      REAL*8     EX(3),EY(3),EZ(3),XSUN(4),XSAT(3),ELE(6),DUM3(3)
      REAL*8     REY,SZ2,XPOL2,YPOL2,UT1GPS2,PROD1,PROD2
      REAL*8     USAT,DT2REL
C
      LG=6.969290134D-10
C
C SIDERAL TIME
C ------------
      SZ1=SZ+DT/86400*2*PI*366.25D0/365.25D0
      SS=DSIN(-SZ1)
      CS=DCOS(-SZ1)
C
C RECEIVER COORDINATES IN THE SYSTEM OF THE TRUE
C EQUATOR OF DATE
C ----------------------------------------------
      XH1=     XSTAT(1)              -XPOL*XSTAT(3)
      XH2=                   XSTAT(2)+YPOL*XSTAT(3)
      XH3=XPOL*XSTAT(1)-YPOL*XSTAT(2)     +XSTAT(3)
      Y(1)= CS*XH1+SS*XH2
      Y(2)=-SS*XH1+CS*XH2
      Y(3)=    XH3
C
C RECEIVER VELOCITY
C
      VH1=-OMEGA*XSTAT(2)
      VH2= OMEGA*XSTAT(1)
      V(3)=XPOL*VH1-YPOL*VH2
      V(1)=  CS*VH1+  SS*VH2
      V(2)= -SS*VH1+  CS*VH2
C
C TOPOCENTRIC SATELLITE POSITION
C ------------------------------
      TOP  =0.D0
      RSAT =0.D0
      RSTA =0.D0
      SCAL =0.D0
      SCAL2=0.D0
      SCAL3=0.D0
      PERREL=0.D0

      DO 1 K=1,3
        XSAT(K)=XVSAT(K)+XVSAT(3+K)*DT+XVSAT(6+K)/2*DT**2
        VSAT=XVSAT(3+K)+XVSAT(6+K)*DT
        XTOPO(K)=XSAT(K)-Y(K)
        TOP=TOP+XTOPO(K)**2
        RSAT=RSAT+(XSAT(K))**2
        RSTA=RSTA+(Y(K))**2
        IF (MEATYP.EQ.3) THEN
          SCAL=SCAL+XTOPO(K)*VSAT
          SCAL2=SCAL2+XTOPO(K)*(VSAT-2*V(K))
        ELSE
          SCAL=SCAL+XTOPO(K)*VSAT
        ENDIF
        PERREL=PERREL+2*XSAT(K)*VSAT/C
        SCAL3=SCAL3+XTOPO(K)*(XVSAT(K)+XVSAT(3+K)*DT+XVSAT(6+K)/2*DT**2)
1     CONTINUE
C
C RELATIVISTIC CORRECTION (IERS STANDARD FOR SLR, SHAPIRO EFFECT)
C ---------------------------------------------------------------
      DIST=DSQRT(TOP)
      RSAT=DSQRT(RSAT)
      RSTA=DSQRT(RSTA)
      RELATIV=(2*GM/C/C)*DLOG((RSAT+RSTA+DIST)/(RSAT+RSTA-DIST))
C
C No transformation of time system from TCG to TT in ITRF2000
C     DIST=DIST*(1-LG)
C
C PERIODIC RELATIVISTIC J2-CORRECTION
C -----------------------------------
C Reference: Kouba J (2004) Improved relativistic transformations in GPS.
C            GPS Solutions 8:170-180
      IF (IREL2.EQ.1) THEN
        CALL XYZELE(GM,0D0,XVSAT(1:3),XVSAT(4:6),SVN,
     1              ELE(1),ELE(2),ELE(3),ELE(4),ELE(5),ELE(6))
        CALL ARGLAT(XVSAT,USAT)
        DT2REL=-(J2/(2D0*C))*((AE/ELE(1))**2)*
     1          (3D0*DSQRT(GM*ELE(1))*(DSIN(ELE(3))**2)*DSIN(2D0*USAT))
C     Drift of clock due to J2 not included
C     2          -7D0*(GM/ELE(1))*(1D0-(3D0/2D0)*(DSIN(ELE(3))**2))*TINT)
        PERREL = PERREL-DT2REL
      ENDIF
C
C TOPOCENTRIC DISTANCE OF SATELLITE
C correct as well for light travel time (SCAL, SCAL2)
C ---------------------------------------------------
      IF (MEATYP.EQ.3) THEN
        DIST=DIST/2*((1.D0-SCAL/DIST/C)+(1.D0-SCAL2/DIST/C))+RELATIV
      ELSE
C
C Relativistic range correction is applied in IGS
        DIST=DIST*(1.D0-SCAL/DIST/C)+PERREL+RELATIV
C        DIST=DIST*(1.D0-SCAL/DIST/C)+PERREL
      ENDIF
C
C GEOGRAPHIC LATITUDE AND LONGITUDE
C ---------------------------------
      XLAT    =DATAN2(Y(3),DSQRT(Y(1)**2+Y(2)**2))
      XELL(1) =DATAN(1.D0/(1.D0-FELL)**2*DTAN(XLAT))
      XELL(2) =DATAN2(Y(2),Y(1))
      XELL(3) =0.D0
C
C APPROXIMATE ZENITH DISTANCE, NADIR ANGLE AND AZIMUTH ANGLE
C ----------------------------------------------------------
      CALL ECCELL(XELL,XTOPO,XLOC)
      XL12=DSQRT(XLOC(1)**2+XLOC(2)**2)
      IF (XL12.NE.0.D0 .OR. XLOC(3).NE.0.D0) THEN
        ZEN=DATAN2(XL12,XLOC(3))
      ELSE
        ZEN=0.D0
      ENDIF
      CSNAD=SCAL3/RSAT/DIST
      IF (CSNAD.GT.1.D0) THEN
        CSNAD=1.D0
      ENDIF
      NAD=DACOS(CSNAD)
C
C CHECK WHETHER NADIR ANGLE IS GREATER THAN THE THEORETICAL MAXIMUM
      IF (MEATYP.NE.3 .AND. NAD.GT.14.7D0/180.D0*PI) THEN
        WRITE(LFNERR,10) NAD/PI*180.D0,RSTA/1.D3,RSAT/1.D3
10      FORMAT(/,' ### SR TOPSTA: NADIR ANGLE GREATER THAN 14.7 DEG',
     1         /,'                NADIR ANGLE: ',F7.3,' DEG',
     2         /,'                RSTA:        ',F12.3,' KM',
     3         /,'                RSAT:        ',F12.3,' KM',/)
      ENDIF
C
      IF (XLOC(1).NE.0.D0 .OR. XLOC(2).NE.0.D0) THEN
        AZI=DATAN2(XLOC(2),XLOC(1))
        AZISAT=DATAN2(-XLOC(2),-XLOC(1))
      ELSE
        AZI=0.D0
        AZISAT=0.D0
      ENDIF
      IF (AZI.LT.0.D0) AZI=AZI+2*PI
      IF (AZISAT.LT.0.D0) AZISAT=AZISAT+2*PI
C
C
C COMPUTATION OF UNIT VECTORS EX, EY AND EZ IN TRUE SYSTEM
C --------------------------------------------------------
C
C UNIT VECTOR EZ
      DO 20 K=1,3
        EZ(K)=-XSAT(K)/RSAT
20    CONTINUE
C
C UNIT VECTOR EY
      TDT=TOBS+(19.D0+32.184D0)/86400.D0
      CALL SUNEFF(IORSYS,2.D0,TDT,XSUN,DUM3)
      CALL COOTRA(IORSYS,0,TOBS,XSUN,SZ2,XPOL2,YPOL2,UT1GPS2)
      CALL VPROD(EZ,XSUN,EY)
      REY=DSQRT(EY(1)**2+EY(2)**2+EY(3)**2)
      DO 30 K=1,3
        EY(K)=EY(K)/REY
30    CONTINUE
C
C UNIT VECTOR EX
      CALL VPROD(EY,EZ,EX)
C
C COMPUTATION OF CORRECTED AZIMUTH AZISOK
C ---------------------------------------
C
C PROJECTION OF -XTOPO INTO THE SATELLITE COORDINATE SYSTEM
      PROD1=0.D0
      PROD2=0.D0
      DO 40 K=1,3
        PROD1=PROD1+EY(K)*(-1.D0)*XTOPO(K)
        PROD2=PROD2+EX(K)*(-1.D0)*XTOPO(K)
40    CONTINUE
C
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
C
      RETURN
      END SUBROUTINE

      END MODULE
