      MODULE s_RVPDER
      CONTAINS

C*
      SUBROUTINE RVPDER(IARC,SVN,GM,T,TOSC,A,E,I,KN,PER,T0,
     1                  DRDELE,DVDELE)
CC
CC NAME       :  RVPDER
CC
CC PURPOSE    :  COMPUTE PARTIAL DERIVATIVES OF POSITION AND VELOCITY
CC               OF A KEPLERIAN ORBIT WITH RESPECT TO THE OSCULATING
CC               ELEMENTS AT TIME TOSC. THE FOLLOWING SET OF OSCULATING
CC               ELEMENTS IS USED
CC
CC               THIS SUBROUTINE WAS CHECKED USING DIFFERENCE COEFFICIENTS
CC               FOR POSITIONS AND VELOCITIES FOR ALL SIX ELEMENTS.
CC               THE AGREEMENT WAS GOOD TO ABOUT 6 DIGITS OR MORE.
CC
CC               NUMBER   ELEMENT   EXPLANATION
CC               ----------------------------------------------------
CC                 1        A       SEMI MAJOR AXIS
CC                 2        E       EXCENTRICITY
CC                 3        I       INCLINATION
CC                 4        KN      R.A. OF ASCENDING NODE
CC                 5        PER     ARGUMENT OF PERIGEE
CC                 6        U0      ARGUMENT OF LATITUDE AT TIME TOSC
CC               ----------------------------------------------------
CC               THE PERIGEE PASSING TIME T0 ENTERING INTO THIS SUBROUTINE
CC               IS NOT USED AS AN ARGUMENT FOR COMPUTING THE PARTIALS
CC
CC               MANY PARTS OF THE COMPUTATION HAVE TO BE
CC               PERFORMED ONLY AT THE FIRST TIME A SPECIAL ARC IS
CC               ACTUALLY USED. THEREFORE THESE ITEMS ARE ONLY
CC               RECOMPUTED IF THE ARC IDENTIFICATION IS CHANGING.
CC               THE FORMULAE ARE BASED ON AN INTERNAL PAPER
CC               BY U. HUGENTOBLER :
CC
CC PARAMETERS :
CC         IN :  IARC   : ARC-NUMBER                          I*4
CC                        INITIALIZE IF IARC NEQ ARCOLD OR
CC                        IF IARC=0. IF IARC=0 IS USED, NO
CC                        INITIALIZATION TAKES PLACE WHEN
CC                        IARC CHANGES.
CC               SVN    : SPACE VEHICLE NUMBER                I*4
CC               GM     : GRAVITY CONSTANT                    R*8
CC               T      : TIME                                R*8
CC               TOSC   : OSCULATION EPOCH                    R*8
CC               A      : SEMI MAJOR AXIS                     R*8
CC               E      : NUMERICAL ECCENTRICITY              R*8
CC               I      : INCLINATION                         R*8
CC               KN     : R.A. OF ASCENDING NODE              R*8
CC               PER    : PERIGEE                             R*8
CC               T0     : PERIGEE PASSING TIME                R*8
CC        OUT :  DRDELE : ARRAY CONTAINING PARTIALS OF R WITH R*8(3,6)
CC                        RESPECT TO ELEMENTS A,E,I,KN,PER,U0
CC               DVDELE : ARRAY CONTAINING PARTIALS OF V WITH R*8(3,6)
CC                        RESPECT TO ELEMENTS A,E,I,KN,PER,U0
CC
CC REMARKS    :  NOT MORE THAN 25 SATELLITES PER ARC
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/03 10:37
CC
CC CHANGES    :  17-JUN-96 : MR: USE EXITRC, NOT STOP
CC               29-APR-97 : HH: INCLUDE FILE FOR MAXSAT
CC               28-AUG-00 : HB: MODIFIED USE OF IARC
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_maxdim, ONLY: MAXSAT
      USE s_exitrc
      USE s_maxtst
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IARC  , IFIRST, IRC   , ISVN  , ITER  , K     , MXCSAT,
     1          NSVN
C
      REAL*8    A     , CEX   , CI    , CK    , CP    , CV    , DE    ,
     1          DE0DE , DE0DU0, DEDA  , DEDE  , DEDU0 , DXDT0 , DYDT0 ,
     2          E     , EX    , GM    , HE1   , HE2   , PER   , RR    ,
     3          SEX   , SI    , SK    , SP    , SV    , T     , T0    ,
     4          TOSC  , V     , V0    , V1    , V2    , X1    , X2    ,
     5          Y1    , Y2    , Z1    , Z2
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C
      REAL*8 I,KN,M
      REAL*8 DRDELE(3,6),DVDELE(3,6)
      REAL*8 ABC(3,2,MAXSAT),D(3,2,MAXSAT),E1(3,2,MAXSAT),F(3,2,MAXSAT)
      REAL*8 N(MAXSAT),EW(MAXSAT),COEVEL(MAXSAT)
      REAL*8 DT0DA(MAXSAT),DT0DE(MAXSAT),DT0DU0(MAXSAT)
      INTEGER*4 SVNARC(MAXSAT),ARCOLD,SVN
      CHARACTER*6 MXNSAT
      COMMON/MCMSAT/MXCSAT,MXNSAT
      DATA IFIRST/0/,ARCOLD/-1/
C
C CHECK LOCAL MAXIMUM DIMENSIONS
      IF(IFIRST.EQ.0) THEN
        CALL MAXTST(1,'RVPDER',MXNSAT,MAXSAT,MXCSAT,IRC)
        IF(IRC.NE.0) CALL EXITRC(2)
        IFIRST=1
      ENDIF
C
C INITIALIZE NEW ARC
      IF(ARCOLD.NE.-999.AND.(IARC.NE.ARCOLD.AND.IARC.NE.0))THEN
        NSVN=0
        ARCOLD=IARC
      ELSE IF(IARC.EQ.0)THEN
        NSVN=0
        ARCOLD=-999
      END IF
C
C LOOK FOR NEW SVN-NUMBER
      DO 10 ISVN=1,NSVN
        IF(SVN.EQ.SVNARC(ISVN))GO TO 100
10    CONTINUE
      NSVN=NSVN+1
      SVNARC(NSVN)=SVN
      ISVN=NSVN
C
C INITIALIZE NEW SATELLITE
C ------------------------
C
C 1. COMPUTE ROTATION MATRICES
      CK=DCOS(KN)
      SK=DSIN(KN)
      CI=DCOS(I)
      SI=DSIN(I)
      CP=DCOS(PER)
      SP=DSIN(PER)
C
C R3(-KN)*R1(-I)*R3(-PER) (FIRST TWO COLUMNS ONLY)
      ABC(1,1,NSVN)= CK*CP-SK*CI*SP
      ABC(2,1,NSVN)= SK*CP+CK*CI*SP
      ABC(3,1,NSVN)= SI*SP
      ABC(1,2,NSVN)=-CK*SP-SK*CI*CP
      ABC(2,2,NSVN)=-SK*SP+CK*CI*CP
      ABC(3,2,NSVN)= SI*CP
C
C R3(-KN)*D(R1(-I))/DI*R3(-PER)
      D(1,1,NSVN)=-SK*CP-CK*CI*SP
      D(2,1,NSVN)= CK*CP-SK*CI*SP
      D(3,1,NSVN)= 0
      D(1,2,NSVN)= SK*SP-CK*CI*CP
      D(2,2,NSVN)=-CK*SP-SK*CI*CP
      D(3,2,NSVN)= 0
C
C D(R3(-KN))/DI*R1(-I)*R3(-PER)
      E1(1,1,NSVN)= SK*SI*SP
      E1(2,1,NSVN)=-CK*SI*SP
      E1(3,1,NSVN)= CI*SP
      E1(1,2,NSVN)= SK*SI*CP
      E1(2,2,NSVN)=-CK*SI*CP
      E1(3,2,NSVN)= CI*CP
C
C R3(-KN)*R1(-I)*D(R3(-PER))/DPER
      F(1,1,NSVN)=-CK*SP-SK*CI*CP
      F(2,1,NSVN)=-SK*SP+CK*CI*CP
      F(3,1,NSVN)= SI*CP
      F(1,2,NSVN)=-CK*CP+SK*CI*SP
      F(2,2,NSVN)=-SK*CP-CK*CI*SP
      F(3,2,NSVN)=-SI*SP
C
C 2. COMPUTE MEAN MOTION
      N(NSVN)=DSQRT(GM/A**3)
C
C 3. SQRT(1-E**2)
      EW(NSVN)=DSQRT(1.D0-E**2)
C
C 4. COMPUTE E0, V0
      M=N(NSVN)*(TOSC-T0)
      EX=M+E*DSIN(M)
      DO 20 ITER=1,3
        DE=(M-EX+E*DSIN(EX))/(1.D0-E*DCOS(EX))
        EX=EX+DE
20    CONTINUE
      CEX=DCOS(EX)
      SEX=DSIN(EX)
      V0 =2*DATAN(DSQRT((1+E)/(1-E))*DTAN(EX/2))
C
C 5. COMPUTE PARTIAL OF T0 WITH RESPECT TO A
      DT0DA(ISVN)=-1.5D0*(EX-E*SEX)/(N(ISVN)*A)
C
C 6. COMPUTE PARTIAL OF T0 WITH RESPECT TO E
      DE0DE=-2/(1-E**2)*DTAN(EX/2)/(1.D0+DTAN(EX/2)**2)
      DT0DE(ISVN)=-(DE0DE*(1-E*CEX)-SEX)/N(ISVN)
C
C 7. COMPUTE PARTIAL OF T0 WITH RESPECT TO U0
      HE1=1/DSQRT(1+E)
      HE2=1/DSQRT(1-E)
      DE0DU0=HE1/HE2*(1+(DTAN(V0/2))**2)/(1+(DTAN(EX/2))**2)
      DT0DU0(ISVN)=-(1.D0-E*CEX)/N(ISVN)*DE0DU0
C
C 8. COEFFICIENT OF VELOCITY VECTOR
C    ------------------------------
      COEVEL(NSVN)=DSQRT(GM/(A*(1-E**2)))
C
C COMPUTE PARTIALS FOR TIME T (NORMAL REQUEST)
C --------------------------------------------
C
C 1. KEPLERIAN POSITION AND VELOCITY
100   M=N(ISVN)*(T-T0)
      EX=M+E*DSIN(M)
      DO 101 ITER=1,3
        DE=(M-EX+E*DSIN(EX))/(1.D0-E*DCOS(EX))
        EX=EX+DE
101   CONTINUE
      SEX=DSIN(EX)
      CEX=DCOS(EX)
      V=2*DATAN(DSQRT((1+E)/(1-E))*TAN(EX/2))
      SV=DSIN(V)
      CV=DCOS(V)
      X1=A*(CEX-E)
      X2=A*EW(ISVN)*SEX
      RR = DSQRT(X1**2+X2**2)
      V1=-COEVEL(ISVN)*SV
      V2= COEVEL(ISVN)*(E+CV)
C
C 2. PARTIALS WITH RESPECT TO THE THREE EULERIAN ANGLES
C    (FIRST PART FOR PERIGEE ONLY)
      DO 110 K=1,3
        DRDELE(K,3)=E1(K,1,ISVN)*X1+E1(K,2,ISVN)*X2
        DRDELE(K,4)=D(K,1,ISVN)*X1+D(K,2,ISVN)*X2
        DRDELE(K,5)=F(K,1,ISVN)*X1+F(K,2,ISVN)*X2
        DVDELE(K,3)=E1(K,1,ISVN)*V1+E1(K,2,ISVN)*V2
        DVDELE(K,4)=D(K,1,ISVN)*V1+D(K,2,ISVN)*V2
        DVDELE(K,5)=F(K,1,ISVN)*V1+F(K,2,ISVN)*V2
110   CONTINUE
C
C 3. PARTIAL WITH RESPECT TO A
      DEDA=N(ISVN)*(-1.5/A*(T-T0)-DT0DA(ISVN))/RR*A
      Y1=X1/A-A*SEX*DEDA
      Y2=X2/A+A*EW(ISVN)*CEX*DEDA
      Z1=-V1/(2*A)+3*GM/(2*RR**3)*X1/A*(T-TOSC)
      Z2=-V2/(2*A)+3*GM/(2*RR**3)*X2/A*(T-TOSC)
      DO 120 K=1,3
        DRDELE(K,1)=ABC(K,1,ISVN)*Y1+ABC(K,2,ISVN)*Y2
120   CONTINUE
      DO 121 K=1,3
        DVDELE(K,1)=ABC(K,1,ISVN)*Z1+ABC(K,2,ISVN)*Z2
121   CONTINUE
C
C 4. PARTIAL WITH RESPECT TO ECCENTRICITY
      DEDE= (SEX-N(ISVN)*DT0DE(ISVN))/RR*A
      Y1=-A*(1.D0+SEX*DEDE)
      Y2= A*(-E/EW(ISVN)*SEX+EW(ISVN)*CEX*DEDE)
      DXDT0=GM*X1/RR**3
      DYDT0=GM*X2/RR**3
      Z1=-A*N(ISVN)*SV/EW(ISVN)**3*(E+CV+CV*(1+E*CV))
     1    +DT0DE(ISVN)*DXDT0
      Z2=-A*N(ISVN)/EW(ISVN)**3*(1-CV**2*(2+E*CV))
     1    +DT0DE(ISVN)*DYDT0
      DO 130 K=1,3
        DRDELE(K,2)=ABC(K,1,ISVN)*Y1+ABC(K,2,ISVN)*Y2
130   CONTINUE
      DO 131 K=1,3
        DVDELE(K,2)=ABC(K,1,ISVN)*Z1+ABC(K,2,ISVN)*Z2
131   CONTINUE
C
C 5. SECOND PART OF PARTIAL WITH RESPECT TO PER, PARTIAL WITH
C    RESPECT TO U0
      DEDU0=-N(ISVN)*A/RR*DT0DU0(ISVN)
      Y1=-A*SEX*DEDU0
      Y2=A*EW(ISVN)*CEX*DEDU0
      Z1=DT0DU0(ISVN)*DXDT0
      Z2=DT0DU0(ISVN)*DYDT0
      DO 140 K=1,3
      DRDELE(K,6)=ABC(K,1,ISVN)*Y1+ABC(K,2,ISVN)*Y2
      DVDELE(K,6)=ABC(K,1,ISVN)*Z1+ABC(K,2,ISVN)*Z2
      DRDELE(K,5)=DRDELE(K,5)-(ABC(K,1,ISVN)*Y1+ABC(K,2,ISVN)*Y2)
      DVDELE(K,5)=DVDELE(K,5)-(ABC(K,1,ISVN)*Z1+ABC(K,2,ISVN)*Z2)
140   CONTINUE
      RETURN
      END SUBROUTINE


      END MODULE
