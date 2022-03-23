      MODULE s_EPHEM
      CONTAINS

C*
        SUBROUTINE EPHEM(GM,A,E,I,KNOT,PERI,T0,T,X,XP)
CC
CC NAME       :  EPHEM
CC
CC PURPOSE    :  COMPUTE POSITION X AND VELOCITY XP OF A CELESTIAL
CC               BODY WHOSE OSCULATING ELEMENTS ARE GIVEN
CC               (NO PARABOLAS, HYPERBOLAS)
CC
CC PARAMETERS :
CC         IN :  GM     : GRAVITY - CONSTANT                  R*8
CC               A      : SEMIMAJOR AXIS                      R*8
CC               E      : NUMERICAL EXCENTRICITY              R*8
CC               I      : INCLINATION (RADIAN)                R*8
CC               KNOT   : RIGHT ASCENSION OF ASCENDING NODE   R*8
CC                        (RADIAN)
CC               PERI   : ARGUMENT OF PERICENTRE (RADIAN)     R*8
CC               T0     : PERICENTRE-PASSING-TIME             R*8
CC               T      : EPOCH OF EPHEMERIS COMPUTATION      R*8
CC               X      : POSITION OF SATELLITE               R*8
CC               XP     : VELOCITY OF SATELLITE               R*8
CC               X(K), XP(K),K=1,2,3: POSITION AND VELOCITY   R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/30 08:11
CC
CC MODIFIED   :  13-AUG-03 : GB: NO ENDLESS LOOP BY KEPLER'S EQUATION
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 II
C
      REAL*8    A   , BETA, CI  , CK  , CO  , E   , EX  , EX1 , GM  ,
     1          P   , P1  , P2  , P3  , PERI, Q1  , Q2  , Q3  , R   ,
     2          SI  , SK  , SO  , T   , T0  , V   , X1  , X2  , XP1 ,
     3          XP2
C
CCC         IMPLICIT REAL*8(A-H,O-Z)
        REAL*8 I,KNOT,M,N
        REAL*8 X(3),XP(3)
C
C P=PARAMETER OF CONIC SECTION
        P=A*(1.D0-E**2)
C
C N = MEAN MOTION, M = MEAN ANOMALY
        N=DSQRT(GM/A**3)
        M=N*(T-T0)
C
C SOLVE KEPLER'S EQUATION
C EX, EX1: ECCENTRIC ANOMALY
        EX1=M
        DO ii = 1, 10
          EX=EX1+(M+E*DSIN(EX1)-EX1)/(1-E*DCOS(EX1))
          IF(DABS(EX-EX1).LT.1.D-12) EXIT
          EX1=EX
        END DO
C
C V = TRUE ANOMALY
        V=2*DATAN(DSQRT((1.D0+E)/(1.D0-E))*(DSIN(EX/2)/DCOS(EX/2)))
        R=A*(1.D0-E*DCOS(EX))
        BETA=DSQRT(GM/P)
        X1=R*DCOS(V)
        X2=R*DSIN(V)
        XP1=-BETA*DSIN(V)
        XP2= BETA*(E+DCOS(V))
C
C SINES AND COSINES OF INCLINATION I, NODE K, PERIGEE O
        CK=DCOS(KNOT)
        SK=DSIN(KNOT)
        CI=DCOS(I)
        SI=DSIN(I)
        CO=DCOS(PERI)
        SO=DSIN(PERI)
C
C VECTORS P AND Q
        P1=CK*CO-SK*CI*SO
        P2=SK*CO+CK*CI*SO
        P3=SI*SO
C
        Q1=-CK*SO-SK*CI*CO
        Q2=-SK*SO+CK*CI*CO
        Q3= SI*CO
C
C COMPUTE POSITION AND VELOCITY
        X(1)=P1*X1+Q1*X2
        X(2)=P2*X1+Q2*X2
        X(3)=P3*X1+Q3*X2
C
        XP(1)=P1*XP1+Q1*XP2
        XP(2)=P2*XP1+Q2*XP2
        XP(3)=P3*XP1+Q3*XP2
        RETURN
        END SUBROUTINE

      END MODULE
