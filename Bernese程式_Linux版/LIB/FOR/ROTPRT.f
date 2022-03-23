      MODULE s_ROTPRT
      CONTAINS

C*
      SUBROUTINE ROTPRT(T,TOSC,A,XN,U0,PER0,XI,GM,J2,AE,ABC,D,D2D,E1,
     1                  D2E1,F,D2F,DPER,ABCP,DP,E1P,FP,DNDELE,DPDELE)
CC
CC NAME       :  ROTPRT
CC
CC PURPOSE    : COMPUTE PERTURBATIONS DUE TO J2 IN ROTATION MATRICES
CC              AND PARTIALS OF NODE RESP. PERIGEE AT TIME T WITH
CC              RESPECT TO ELEMENTS AT TIME T0
CC
CC PARAMETERS :
CC        IN  : T       : CURRENT TIME (SEC)                        R*8
CC              TOSC    : OSCULATION TIME (SEC)                     R*8
CC              A       : SEMIMAJOR AXIS                            R*8
CC              XN      : MEAN MOTION  AT OSCULATION TIME           R*8
CC              U0      : ARGUMENT OF LATITUDE AT TIME TOSC         R*8
CC              PER0    : ARGUMENT OF PERIGEE AT TIME TOSC          R*8
CC              XI      : INCLINATION                               R*8
CC              GM      : G * M(EARTH)                              R*8
CC              J2      : TERM J20 OF EARTH POTENTIAL               R*8
CC              AE      : EQUATORIAL RADIUS                         R*8
CC              ABC     : R3(-NODE)*R1(-I)*R3(PER) AT TIME T0       R*8(*,*)
CC              D       : D(ABC)/D(I) AT TIME T0                    R*8(*,*)
CC              D2D     : D(D)/D(NODE), D(D)/D(PER)                 R*8(*,*,*)
CC                        D2D(*,*,1) NODE, D2D(*,*,2) PERIGEE
CC              E1      : D(ABC)/D(NODE) AT TIME T0                 R*8(*,*)
CC              D2E1    : D(E1)/D(NODE), D(E1)/D(PER)               R*8(*,*,*)
CC                        D2E1(*,*,1) NODE, D2E1(*,*,2) PERIGEE
CC              F       : D(ABC)/D(PER) AT TIME T0                  R*8(*,*)
CC              D2F     : D(F)/D(NODE), D(F)/D(PER)                 R*8(*,*,*)
CC                        D2F(*,*,1) NODE, D2F(*,*,2) PERIGEE
CC        OUT : DPER    : PERTURBATION OF THE PERIGEE               R*8(*,*)
CC              ABCP    : R3(-NODE)*R1(-I)*R3(PER) AT TIME T        R*8(*,*)
CC              DP      : D(ABC)/D(I) AT TIME T                     R*8(*,*)
CC              E1P     : D(ABC)/D(NODE) AT TIME T                  R*8(*,*)
CC              FP      : D(ABC)/D(PER) AT TIME T                   R*8(*,*)
CC              DNDELE  : D(NODE(T))/D(A,E,I,NODE,PER,U0, AT T0)    R*8(*)
CC              DPDELE  : D(PER(T))/D(A,E,I,NODE,PER,U0, AT T0)     R*8(*)
CC
CC REMARKS    :
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.5  (MAY 94)
CC
CC CREATED    :  94/05/11
CC
CC CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1994     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IFIRST, K
C
      REAL*8    A     , A72   , A92   , AE    , CI    , DNODE , DPER  ,
     1          G     , G1    , GG7   , GG9   , GM    , PER0  , S2    ,
     2          S32   , SI    , T     , TOSC  , U0    , XI    , XM1   ,
     3          XN
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
      REAL*8   J2,ABC(3,*),D(3,*),D2D(3,2,*),E1(3,*),D2E1(3,2,*),
     1         F(3,*),D2F(3,2,*),ABCP(3,*),DP(3,*),E1P(3,*),FP(3,*),
     2         DNDELE(*),DPDELE(*)
      DATA IFIRST/1/
C
C INITIALIZE
C ----------
      IF(IFIRST.EQ.1)THEN
        IFIRST=0
        G =1.5D0*AE**2*J2
        G1=DSQRT(GM)*AE**2*J2
      END IF
C
C TRIG. FUNCTIONS OF I :
      SI=DSIN(XI)
      CI=DCOS(XI)
      S2=SI**2
      S32=1-3.D0/2.D0*S2
C
C MEAN ANOMALY AT TIMES T0 AND T
C ------------------------------
      XM1=XN*(T-TOSC)
C
C PERTURBATIONS IN NODE AND PERIGEE
C ---------------------------------
      DNODE=-G/(A**2)*CI*XM1
      DPER = G/(A**2)*(3-4*S2)*XM1
     1       +1.5D0*G*S2/(A**2)*DCOS(2*U0)*XM1
C
C COMPUTE NEW ROTATION MATRICES
C -----------------------------
      DO 20 K=1,3
        DO 20 I=1,2
          ABCP(K,I)=ABC(K,I) + D(K,I)*DNODE+F(K,I)*DPER
          DP(K,I)  =D(K,I)+D2D (K,I,1)*DNODE+D2D (K,I,2)*DPER
          E1P(K,I) =E1(K,I)+D2E1(K,I,1)*DNODE+D2E1(K,I,2)*DPER
          FP(K,I)  =F(K,I)+D2F (K,I,1)*DNODE+D2F (K,I,2)*DPER
20    CONTINUE
C
C PARTIAL DERIVATIVES OF NODE AND PERIGEE AT TIME T WITH RESPECT TO
C THE ELEMENTS AT TOSC
C -----------------------------------------------------------------
      A72=DSQRT(A**7)
      A92=DSQRT(A**9)
      GG7=G1/A72
      GG9=G1/A92
      DNDELE(1)=21.D0/4.D0*GG9*CI*(T-TOSC)
      DNDELE(2)=0.D0
      DNDELE(3)=1.5D0*GG7*SI*(T-TOSC)
      DNDELE(4)=1.D0
      DNDELE(5)=0.D0
      DNDELE(6)=0.D0
C
      DPDELE(1)=-21.D0/4.D0*GG9*
     1          (3.D0-4*S2+1.5D0*S2*DCOS(2*U0))*(T-TOSC)
      DPDELE(2)=0.D0
      DPDELE(3)=1.5D0*GG7*DSIN(2*XI)*
     1          (-4D0+1.5D0*DCOS(2*U0))*(T-TOSC)
      DPDELE(4)=0.D0
      DPDELE(5)=1.D0
      DPDELE(6)=-4.5*GG7*S2*DSIN(2*U0)*(T-TOSC)
C
999   CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
