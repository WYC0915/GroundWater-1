      MODULE s_ELEPRT
      CONTAINS

C*
      SUBROUTINE ELEPRT(GM,A,E,XI,XKN,PER,TPER,T0,T,RSW,ELEPAR)
CC
CC NAME       :  ELEPRT
CC
CC PURPOSE    :  COMPUTE PARTIAL DERIVATIVES OF ORBITAL ELEMENTS
CC               A, E, I, NODE, PER, SIGMA(T0), TPER, U0(T0)
CC               WITH RESPECT TO A FORCE PARAMETER P OF A FORCE
CC               OF TYPE P * RSW, RSW BEING A UNIT VECTOR IN THE
CC               SYSTEM "RADIAL,NORMAL TO RAD IN ORB PLANE, OUT OF
CC               PLANE"
CC               8 INSTEAD OF SIX DERIVATIVES ARE COMPUTED BECAAUSE
CC               (A) THE FIRST SIX ARE STANDARD, TPER IS USED IN THE
CC               ORBIT GENERATION, U0 IN THE PROGRAM GPSEST OF THE
CC               BERNESE GPS SOFTWARE
CC               THEORY : G. BEUTLER, HIMMELSMECHANIK II, MITT. DER
CC                        SATELLITENBEOBACHTUNGSSTATION ZIMMERWALD,
CC                        NO 28, EQNS. (6.61A).
CC
CC PARAMETERS :
CC        IN  :  GM     : G*M (EARTH)                           R*8
CC               A      : SEMI MAJOR AXIS                       R*8
CC               E      : NUMERICAL EXCENTRICITY                R*8
CC               XI     : INCLINATION                           R*8
CC               XKN    : RA OF ASCENDING NODE                  R*8
CC               PER    : ARGUMENT OF PERIGEE                   R*8
CC               TPER   : PERIGEE PASSING TIME                  R*8
CC               T0     : REFERENCE TIME FOR ELEMENTS SIGMA, U0 R*8
CC               T      : TIME OF PERTURBATION                  R*8
CC               RSW    : COMPONENTS R, S, W OF PERTURBING      R*8(*)
CC                        FORCE
CC        OUT :  ELEPAR : DERIVATIVES OF ELEMENTS A,E,I,NODE,   R*8(*)
CC                        PER,SIGMA,TPER,U0
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G. BEUTLER
CC
CC VERSION    :  3.4 (JAN 93)
CC
CC CREATED    :  92/12/22
CC
CC CHANGES    :  21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1992     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 K
C
      REAL*8    A   , CE  , CV  , E   , EE  , EX  , GM  , P   , PDM ,
     1          PER , R   , REE , SE  , SV  , T   , T0  , TPER, V   ,
     2          XI  , XKN , XM  , XN
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      REAL*8 RSW(*),ELEPAR(*)
C
C
C AUXILIARY QUANTITIES
C --------------------
      XN=DSQRT(GM/A**3)
      P=A*(1.D0-E**2)
      PDM=DSQRT(P/GM)
      EE=1-E**2
      REE=DSQRT(EE)
C
      XM=XN*(T-TPER)
      EX=XM
      DO 10 K=1,15
        EX=XM+E*DSIN(EX)
10    CONTINUE
      V=2*DATAN(DSQRT((1+E)/(1-E))*TAN(EX/2))
      SV=DSIN(V)
      CV=DCOS(V)
      R=P/(1+E*CV)
      SE=DSIN(EX)
      CE=DCOS(EX)
C
C SEMIMAJOR AXIS A
      ELEPAR(1)=PDM*2.D0*A/EE*(E*SV*RSW(1)+P/R*RSW(2))
C
C ECCENTRICITY E
      ELEPAR(2)=PDM*(SV*RSW(1)+(CV+CE)*RSW(2))
C
C INCLINATION
      ELEPAR(3)=R*DCOS(PER+V)/(XN*A**2*REE)*RSW(3)
C
C NODE
      ELEPAR(4)=R*DSIN(PER+V)/(XN*A**2*REE*DSIN(XI))*RSW(3)
C
C PERIGEE
      ELEPAR(5)=PDM/E*(-CV*RSW(1)+(1+R/P)*SV*RSW(2))-DCOS(XI)*ELEPAR(4)
C
C SIGMA (MEAN ANOMALY AT TIME T0)
      ELEPAR(6)=EE/(XN*A*E)*((CV-2*E*R/P)*RSW(1)-(1+R/P)*SV*RSW(2)) +
     1          1.5D0*XN/A*(T-T0)*ELEPAR(1)
C
C PARTIAL DERIVATIVES OF U0 AND T0 WITH RESPECT
C THE PARAMETERS U0 AND T0 USING THE FORMULAE BY HUGENTOBLER, MAY 1994
C --------------------------------------------------------------------
      XM=XN*(T0-TPER)
      EX=XM
      DO 20 K=1,15
        EX=XM+E*DSIN(EX)
20    CONTINUE
      V=2*DATAN(DSQRT((1+E)/(1-E))*DTAN(EX/2))
      SV=DSIN(V)
      CV=DCOS(V)
      R=P/(1+E*CV)
      CE=DCOS(EX)
      SE=DSIN(EX)
C
      ELEPAR(7)=-1.5D0*(T0-TPER)/A*ELEPAR(1)-ELEPAR(6)/XN
C
      ELEPAR(8)=1/REE*SE/(1-E*CE)*(1+EE/(1-E*CE))*ELEPAR(2)+
     1      ELEPAR(5)+REE/(1-E*CE)**2*ELEPAR(6)
999   RETURN
      END SUBROUTINE

      END MODULE
