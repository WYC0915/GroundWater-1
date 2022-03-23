      MODULE s_STEP2DIU
      CONTAINS

C*
      SUBROUTINE STEP2DIU(XSTA,FHR,T,XCORSTA)
CC
CC PURPOSE    :  THESE ARE THE SUBROUTINES FOR THE STEP2 OF THE TIDAL
CC               CORRECTIONS. THEY ARE CALLED TO ACCOUNT FOR THE FREQUENCY
CC               DEPENDENCE OF THE LOVE NUMBERS.
CC
CC               CONSISTENT WITH IERS CONVENTIONS 2000
CC
CC PARAMETERS :
CC         IN :  XSTA,FHR,T
CC     IN/OUT :  XCORSTA
CC
CC AUTHOR     :  V. DEHANT, S. MATHEWS AND J. GIPSON
CC
CC VERSION    :  4.1
CC
CC CREATED    :  17-MAY-00
CC
CC CHANGES    :  02-JUN-03 : CU: ADDAPTED FOR THE BERNESE SOFTWARE V5.0
CC               10-JUN-03 : HU: D0 ADDED
CC               06-JUL-04 : HU: DATDI UPDATED TO OFFICIAL VERSION
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               04-MAY-12 : RD: USE DMOD FROM MODULE
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      2003     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE l_basfun, ONLY: dmod
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , J
C
      REAL*8    COSLA , COSPHI, DE    , DN    , DR    , FHR   , H     ,
     1          P     , PR    , PS    , RSTA  , S     , SINLA , SINPHI,
     2          T     , TAU   , THETAF, ZLA   , ZNS
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      REAL*8 XSTA(3),XCORSTA(3),DATDI(9,31)
      REAL*8 DEG2RAD
C
      DATA DEG2RAD/0.0174532925d0/
      DATA ((DATDI(i,j),i=1,9),j=1,31)/
     1 -3.d0, 0.d0, 2.d0, 0.d0, 0.d0,-0.01d0, 0.00d0, 0.0d0 , 0.0d0,
     2 -3.d0, 2.d0, 0.d0, 0.d0, 0.d0,-0.01d0, 0.00d0, 0.0d0 , 0.0d0,
     3 -2.d0, 0.d0, 1.d0,-1.d0, 0.d0,-0.02d0, 0.00d0, 0.0d0 , 0.0d0,
     4 -2.d0, 0.d0, 1.d0, 0.d0, 0.d0,-0.08d0, 0.00d0,-0.01d0, 0.01d0,
     5 -2.d0, 2.d0,-1.d0, 0.d0, 0.d0,-0.02d0, 0.00d0, 0.0d0 , 0.0d0,
     6 -1.d0, 0.d0, 0.d0,-1.d0, 0.d0,-0.10d0, 0.00d0, 0.0d0 , 0.00d0,
     7 -1.d0, 0.d0, 0.d0, 0.d0, 0.d0,-0.51d0, 0.00d0,-0.02d0, 0.03d0,
     8 -1.d0, 2.d0, 0.d0, 0.d0, 0.d0, 0.01d0, 0.00d0, 0.0d0 , 0.0d0,
     9  0.d0,-2.d0, 1.d0, 0.d0, 0.d0, 0.01d0, 0.00d0, 0.0d0 , 0.0d0,
     1  0.d0, 0.d0,-1.d0, 0.d0, 0.d0, 0.02d0, 0.00d0, 0.d0  , 0.d0 ,
     1  0.d0, 0.d0, 1.d0, 0.d0, 0.d0, 0.06d0, 0.00d0, 0.00d0, 0.00d0,
     2  0.d0, 0.d0, 1.d0, 1.d0, 0.d0, 0.01d0, 0.00d0, 0.d0  ,  0.d0,
     3  0.d0, 2.d0,-1.d0, 0.d0, 0.d0, 0.01d0, 0.00d0, 0.0d0 , 0.0d0,
     4  1.d0,-3.d0, 0.d0, 0.d0, 1.d0,-0.06d0, 0.00d0, 0.0d0 , 0.0d0,
     5  1.d0,-2.d0, 0.d0,-1.d0, 0.d0, 0.01d0, 0.00d0, 0.0d0 , 0.0d0,
     6  1.d0,-2.d0, 0.d0, 0.d0, 0.d0,-1.23d0,-0.07d0, 0.06d0, 0.01d0,
     7  1.d0,-1.d0, 0.d0, 0.d0,-1.d0, 0.02d0, 0.00d0, 0.0d0 , 0.0d0,
     8  1.d0,-1.d0, 0.d0, 0.d0, 1.d0, 0.04d0, 0.00d0, 0.0d0 , 0.0d0,
     9  1.d0, 0.d0, 0.d0,-1.d0, 0.d0,-0.22d0, 0.01d0, 0.01d0, 0.0d0,
     1  1.d0, 0.d0, 0.d0, 0.d0, 0.d0,12.00d0,-0.80d0,-0.67d0,-0.03d0,
     1  1.d0, 0.d0, 0.d0, 1.d0, 0.d0, 1.73d0,-0.12d0,-0.10d0, 0.00d0,
     2  1.d0, 0.d0, 0.d0, 2.d0, 0.d0,-0.04d0, 0.00d0, 0.0d0 , 0.0d0,
     3  1.d0, 1.d0, 0.d0, 0.d0,-1.d0,-0.50d0,-0.01d0, 0.03d0, 0.0d0,
     4  1.d0, 1.d0, 0.d0, 0.d0, 1.d0, 0.01d0, 0.00d0, 0.0d0 , 0.0d0,
     5  0.d0, 1.d0, 0.d0, 1.d0,-1.d0,-0.01d0, 0.00d0, 0.0d0 , 0.0d0,
     6  1.d0, 2.d0,-2.d0, 0.d0, 0.d0,-0.01d0, 0.00d0, 0.0d0 , 0.0d0,
     7  1.d0, 2.d0, 0.d0, 0.d0, 0.d0,-0.11d0, 0.01d0, 0.01d0, 0.0d0,
     8  2.d0,-2.d0, 1.d0, 0.d0, 0.d0,-0.01d0, 0.00d0, 0.0d0 , 0.0d0,
     9  2.d0, 0.d0,-1.d0, 0.d0, 0.d0,-0.02d0, 0.00d0, 0.0d0 , 0.00d0,
     1  3.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.0d0 , 0.00d0, 0.0d0 , 0.00d0,
     1  3.d0, 0.d0, 0.d0, 1.d0, 0.d0, 0.0d0 , 0.00d0, 0.0d0 , 0.0d0/
C
      S   = 218.31664563D0+481267.88194D0*T-0.0014663889D0*T**2
     1      +0.00000185139D0*T**3
      TAU = fhr*15.D0+280.4606184D0+36000.7700536D0*T+0.00038793D0*T**2
     1      -0.0000000258D0*T**3-S
      PR  = 1.396971278D0*T+0.000308889D0*T**2+0.000000021D0*T**3
     1      +0.000000007D0*T**4
      S   = S+PR
      H   = 280.46645D0+36000.7697489D0*T+0.00030322222D0*T**2
     1      +0.000000020D0*T**3-0.00000000654D0*T**4
      P   = 83.35324312D0+4069.01363525D0*T-0.01032172222D0*T**2
     1      -0.0000124991D0*T**3+0.00000005263D0*T**4
      ZNS = 234.95544499D0 +1934.13626197D0*T-0.00207561111D0*T**2
     1      -0.00000213944D0*T**3+0.00000001650D0*T**4
      PS  = 282.93734098D0+1.71945766667D0*T+0.00045688889D0*T**2
     1     -0.00000001778D0*T**3-0.00000000334D0*T**4
C
C REDUCE ANGLES TO BETWEEN 0 AND 360
C ----------------------------------
      S   = DMOD(S,360.D0)
      TAU = DMOD(TAU,360.D0)
      H   = DMOD(H,360.D0)
      P   = DMOD(P,360.D0)
      ZNS = DMOD(ZNS,360.D0)
      PS  = DMOD(PS,360.D0)
C
      RSTA   = DSQRT(XSTA(1)**2+XSTA(2)**2+XSTA(3)**2)
      SINPHI = XSTA(3)/RSTA
      COSPHI = DSQRT(XSTA(1)**2+XSTA(2)**2)/RSTA
C
      COSLA = XSTA(1)/COSPHI/RSTA
      SINLA = XSTA(2)/COSPHI/RSTA
      ZLA   = DATAN2(XSTA(2),XSTA(1))
C
      DO 99 I=1,3
        XCORSTA(I) = 0.D0
99    CONTINUE
C
      DO 98 J=1,31
        THETAF = (TAU+DATDI(1,J)*S+DATDI(2,J)*H+DATDI(3,J)*P+
     1           DATDI(4,J)*ZNS+DATDI(5,J)*PS)*DEG2RAD
        DR = DATDI(6,J)*2.D0*SINPHI*COSPHI*SIN(THETAF+ZLA)+
     1       DATDI(7,J)*2.D0*SINPHI*COSPHI*COS(THETAF+ZLA)
        DN = DATDI(8,J)*(COSPHI**2-SINPHI**2)*SIN(THETAF+ZLA)+
     1       DATDI(9,J)*(COSPHI**2-SINPHI**2)*COS(THETAF+ZLA)
        DE = DATDI(8,J)*SINPHI*COS(THETAF+ZLA)+
     1       DATDI(9,J)*SINPHI*SIN(THETAF+ZLA)
        XCORSTA(1) = XCORSTA(1)+DR*COSLA*COSPHI-DE*SINLA
     1               -DN*SINPHI*COSLA
        XCORSTA(2) = XCORSTA(2)+DR*SINLA*COSPHI+DE*COSLA
     1               -DN*SINPHI*SINLA
        XCORSTA(3) = XCORSTA(3)+DR*SINPHI+DN*COSPHI
98    CONTINUE
C
      DO 97 I=1,3
        XCORSTA(I) = XCORSTA(I)/1000.D0
97    CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
