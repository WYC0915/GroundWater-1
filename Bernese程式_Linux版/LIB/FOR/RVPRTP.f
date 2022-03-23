      MODULE s_RVPRTP
      CONTAINS

C*
      SUBROUTINE RVPRTP(IARC,SVN,GM,T,TOSC,A,E,I,KN,PER,T0,
     1                  DRDELE,DVDELE)
CC
CC NAME       :  RVPRTP
CC
CC PURPOSE    :  COMPUTE PARTIAL DERIVATIVES OF AN ORBIT CHARACTERIZED
CC               BY OSCULATING ELEMENTS AT THE OSCULATION EPOCH TOSC (SEC)
CC               THE SUBROUTINE COMPUTES THE PARTIALS OF THE POSITION AND
CC               THE VELOCITY VECTOR.
CC               **********************************************************
CC               *** THE DOMINANT PERTURBATIONS DUE TO J20 ARE INCLUDED ***
CC               *** THE SUBROUTINE CALL IS IDENTICAL WITH THAT OF THE  ***
CC               *** SUBROUTINE RPARTN. RVPRTP SHOULD BE AT LEAST ONE   ***
CC               *** ORDER OF MAGNITUDE MORE ACCURATE THAN RPARTN       ***
CC               **********************************************************
CC
CC               NUMBER   ELEMENT   EXPLANATION
CC
CC                 1        A       SEMI MAJOR AXIS
CC                 2        E       EXCENTRICITY
CC                 3        I       INCLINATION
CC                 4        KN      R.A. OF ASCENDING NODE
CC                 5        PER     ARGUMENT OF PERIGEE
CC                 6        U0      ARGUMENT OF LATITUDE AT TIME TOSC
CC
CC PARAMETERS :
CC         IN :  IARC   : ARC-NUMBER                          I*4
CC               SVN    : SPACE VEHICLE NUMBER                I*4
CC               GM     : GRAVITY CONSTANT                    R*8
CC               T      : TIME (SEC)                          R*8
CC               TOSC   : OSCULATION EPOCH (SEC)              R*8
CC               A      : SEMI MAJOR AXIS         AT TOSC     R*8
CC               E      : NUMERICAL ECCENTRICITY  ""  ""      R*8
CC               I      : INCLINATION             ""  ""      R*8
CC               KN     : R.A. OF ASCENDING NODE  ""  ""      R*8
CC               PER    : PERIGEE                 ""  ""      R*8
CC               T0     : PERIGEE PASSING TIME    ""  ""      R*8
CC        OUT :  DRDELE : ARRAY CONTAINING PARTIALS WITH      R*8(3,6)
CC                        RESPECT TO ELEMENTS A,E,I,KN,PER,U0
CC
CC REMARKS    :  MAX NUMBER OF SATELLITES DEFINED IN I:MAXSAT
CC
CC AUTHOR     :  G.BEUTLER
CC
CC CREATED    :  87/11/03 10:37
CC
CC CHANGES    :  07-MAY-96 : MR: REMOVE "DRDNOD","DRDPER"
CC               22-SEP-97 : DI: USE MAXSAT.inc
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               23-AUG-07 : AG: USE AE from CONST. file
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_maxdim, ONLY: MAXSAT
      USE d_const, ONLY: AE
      USE s_rotprt
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IARC  , ISVN  , ITER  , J     , K     , L     , NSVN
C
      REAL*8    A     , CEX   , CEX0  , CI    , CK    , COE   ,
     1          CP    , DE    , DE0DE , DEDA  , DEDE  , DEDU0 , DPER  ,
     2          DV1DA , DV1DE , DV1DU0, DV2DA , DV2DE , DV2DU0, DXI1DA,
     3          DXI1DE, DXI1DU, DXI2DA, DXI2DE, DXI2DU, E     , EX    ,
     4          EX0   , GM    , PER   , RR    , SEX   , SEX0  , SI    ,
     5          SK    , SP    , T     , T0    , TOSC  , V0    , V1    ,
     6          V2    , XI1   , XI2
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 I,KN,M,J2
      REAL*8 DRDELE(3,6),DVDELE(3,6)
      REAL*8 ABC(3,2,MAXSAT),D(3,2,MAXSAT),E1(3,2,MAXSAT),F(3,2,MAXSAT)
      REAL*8 D2D(3,2,2,MAXSAT),D2E1(3,2,2,MAXSAT),D2F(3,2,2,MAXSAT)
      REAL*8 N(MAXSAT),EW(MAXSAT),U0(MAXSAT),DM0DE(MAXSAT),
     1       DM0DU0(MAXSAT)
      REAL*8 ABCP(3,2),DP(3,2),E1P(3,2),FP(3,2)
      REAL*8 DNDELE(6),DPDELE(6)
      REAL*8 DRDELH(3,6),DVDELH(3,6)
      INTEGER*4 SVNARC(MAXSAT),ARCOLD,SVN
      DATA ARCOLD/-1/
      DATA J2/1082.6267D-6/
C
C INITIALIZE NEW ARC
      IF(IARC.NE.ARCOLD)THEN
        NSVN=0
        ARCOLD=IARC
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
C ------------------------------------------------
C
C D = D(R3(-KN))/DKN*R1(-I)*R3(-PER)
      D(1,1,NSVN)=-SK*CP-CK*CI*SP
      D(2,1,NSVN)= CK*CP-SK*CI*SP
      D(3,1,NSVN)= 0
      D(1,2,NSVN)= SK*SP-CK*CI*CP
      D(2,2,NSVN)=-CK*SP-SK*CI*CP
      D(3,2,NSVN)= 0
C
C D(D)/D(NODE)
      D2D(1,1,1,NSVN)=-CK*CP+SK*CI*SP
      D2D(2,1,1,NSVN)=-SK*CP-CK*CI*SP
      D2D(3,1,1,NSVN)= 0
      D2D(1,2,1,NSVN)= CK*SP+SK*CI*CP
      D2D(2,2,1,NSVN)= SK*SP-CK*CI*CP
      D2D(3,2,1,NSVN)= 0
C
C D(D)/D(PER)
      D2D(1,1,2,NSVN)= SK*SP-CK*CI*CP
      D2D(2,1,2,NSVN)=-CK*SP-SK*CI*CP
      D2D(3,1,2,NSVN)= 0
      D2D(1,2,2,NSVN)= SK*CP+CK*CI*SP
      D2D(2,2,2,NSVN)=-CK*CP+SK*CI*SP
      D2D(3,2,2,NSVN)= 0
C ------------------------------------------------
C
C E1 = R3(-KN))*D(R1(-I))/DI*R3(-PER)
      E1(1,1,NSVN)= SK*SI*SP
      E1(2,1,NSVN)=-CK*SI*SP
      E1(3,1,NSVN)= CI*SP
      E1(1,2,NSVN)= SK*SI*CP
      E1(2,2,NSVN)=-CK*SI*CP
      E1(3,2,NSVN)= CI*CP
C
C D(E1)/D(NODE)
      D2E1(1,1,1,NSVN)= CK*SI*SP
      D2E1(2,1,1,NSVN)= SK*SI*SP
      D2E1(3,1,1,NSVN)= 0
      D2E1(1,2,1,NSVN)= CK*SI*CP
      D2E1(2,2,1,NSVN)= SK*SI*CP
      D2E1(3,2,1,NSVN)= 0
C
C D(E1)/D(PER)
      D2E1(1,1,2,NSVN)= SK*SI*CP
      D2E1(2,1,2,NSVN)=-CK*SI*CP
      D2E1(3,1,2,NSVN)= CI*CP
      D2E1(1,2,2,NSVN)=-SK*SI*SP
      D2E1(2,2,2,NSVN)= CK*SI*SP
      D2E1(3,2,2,NSVN)=-CI*SP
C ------------------------------------------------
C
C F = R3(-KN)*R1(-I)*D(R3(-PER))/DPER
      F(1,1,NSVN)=-CK*SP-SK*CI*CP
      F(2,1,NSVN)=-SK*SP+CK*CI*CP
      F(3,1,NSVN)= SI*CP
      F(1,2,NSVN)=-CK*CP+SK*CI*SP
      F(2,2,NSVN)=-SK*CP-CK*CI*SP
      F(3,2,NSVN)=-SI*SP
C
C D(F)/D(NODE)
      D2F(1,1,1,NSVN)= SK*SP-CK*CI*CP
      D2F(2,1,1,NSVN)=-CK*SP-SK*CI*CP
      D2F(3,1,1,NSVN)= 0
      D2F(1,2,1,NSVN)= SK*CP+CK*CI*SP
      D2F(2,2,1,NSVN)=-CK*CP+SK*CI*SP
      D2F(3,2,1,NSVN)= 0
C
C D(F)/D(PER)
      D2F(1,1,2,NSVN)=-CK*CP+SK*CI*SP
      D2F(2,1,2,NSVN)=-SK*CP-CK*CI*SP
      D2F(3,1,2,NSVN)=-SI*SP
      D2F(1,2,2,NSVN)= CK*SP+SK*CI*CP
      D2F(2,2,2,NSVN)= SK*SP-CK*CI*CP
      D2F(3,2,2,NSVN)=-SI*CP
C ------------------------------------------------
C
C COMPUTE MEAN MOTION
      N(NSVN)=DSQRT(GM/A**3)
C
C SQRT(1-E**2)
      EW(NSVN)=DSQRT(1.D0-E**2)
C
C COMPUTE E0, V0
      M=N(NSVN)*(TOSC-T0)
      EX=M+E*DSIN(M)
      DO 20 ITER=1,3
        DE=(M-EX+E*DSIN(EX))/(1.D0-E*DCOS(EX))
        EX=EX+DE
20    CONTINUE
      CEX=DCOS(EX)
      SEX=DSIN(EX)
      V0 =2*DATAN(DSQRT((1+E)/(1-E))*DTAN(EX/2))
      U0(NSVN)=V0+PER
C
C COMPUTE EXCENTRIC ANOMALY AT TIME TOSC AND PARTIALS OF M0 WITH RESPECT
C TO EXCENTRICITY AND U0
      EX0=2*DATAN(DSQRT((1-E)/(1+E))*DTAN(V0/2))
      SEX0=DSIN(EX0)
      CEX0=DCOS(EX0)
      DE0DE=-(1+CEX0)/((1+E)*EW(NSVN))*DTAN(V0/2)
      DM0DE(NSVN) =(1-E*CEX0)*DE0DE-SEX0
      DM0DU0(NSVN)=(1-E*CEX0)*DSQRT((1-E)/(1+E))*(1+CEX0)/(1+DCOS(V0))
C --------------------------------------------------------------------
C
C COMPUTE ROTATION MATRICES (PLUS FIRST DERIVATIVES) FOR CURRENT TIME
C -------------------------------------------------------------------
100   CONTINUE
      CALL ROTPRT(T,TOSC,A,N(ISVN),U0(ISVN),PER,I,GM,J2,AE,
     1            ABC(1,1,ISVN),D(1,1,ISVN),D2D(1,1,1,ISVN),
     2            E1(1,1,ISVN),D2E1(1,1,1,ISVN),F(1,1,ISVN),
     3            D2F(1,1,1,ISVN),DPER,ABCP,DP,E1P,FP,DNDELE,DPDELE)
C
C 1. KEPLERIAN POSITION AT TIME T
C    ----------------------------
      M=N(ISVN)*(T-T0)
      EX=M+E*DSIN(M)
      DO 101 ITER=1,3
        DE=(M-EX+E*DSIN(EX))/(1.D0-E*DCOS(EX))
        EX=EX+DE
101   CONTINUE
      SEX=DSIN(EX)
      CEX=DCOS(EX)
      XI1=A*(CEX-E)
      XI2=A*EW(ISVN)*SEX
      RR = DSQRT(XI1**2+XI2**2)
      V1 =-A*N(ISVN)*SEX/(1-E*CEX)
      V2 = A*N(ISVN)*EW(ISVN)*CEX/(1-E*CEX)
C
C 2. PARTIALS WITH RESPECT TO THE THREE EULERIAN ANGLES
C    (FIRST PART FOR PERIGEE ONLY)
C    --------------------------------------------------
      DO 110 K=1,3
        DRDELH(K,3)=E1P(K,1)*XI1+E1P(K,2)*XI2
        DRDELH(K,4)=DP(K,1)*XI1+DP(K,2)*XI2
        DRDELH(K,5)=FP(K,1)*XI1+FP(K,2)*XI2
        DVDELH(K,3)=E1P(K,1)*V1+E1P(K,2)*V2
        DVDELH(K,4)=DP(K,1)*V1+DP(K,2)*V2
        DVDELH(K,5)=FP(K,1)*V1+FP(K,2)*V2
110   CONTINUE
C
C 3. PARTIAL WITH RESPECT TO A
C    -------------------------
      DEDA=-1.5D0*N(ISVN)*(T-TOSC)/RR
      DXI1DA=CEX-E-A*SEX*DEDA
      DXI2DA=EW(ISVN)*SEX+A*EW(ISVN)*CEX*DEDA
      DV1DA= .5D0*N(ISVN)*SEX/(1-E*CEX)-
     1        A*N(ISVN)/(1-E*CEX)**2*(CEX-E)*DEDA
      DV2DA=-.5D0*N(ISVN)*EW(ISVN)*CEX/(1-E*CEX)-
     1        A*N(ISVN)*EW(ISVN)*SEX/(1-E*CEX)**2*DEDA
      DO 120 K=1,3
        DRDELH(K,1)=ABCP(K,1)*DXI1DA+ABCP(K,2)*DXI2DA
        DVDELH(K,1)=ABCP(K,1)*DV1DA+ABCP(K,2)*DV2DA
120   CONTINUE
C
C 4. PARTIAL WITH RESPECT TO ECCENTRICITY
C    ------------------------------------
      DEDE=1/(1-E*CEX)*(DM0DE(ISVN)+SEX)
      DXI1DE=-A*(1+SEX*DEDE)
      DXI2DE=A*(-E/EW(ISVN)*SEX+EW(ISVN)*CEX*DEDE)
      DV1DE=-A*N(ISVN)/(1-E*CEX)**2*(CEX-E)*DEDE
      DV1DE=DV1DE-A*N(ISVN)*SEX*CEX/(1-E*CEX)**2
      DV2DE=-A*N(ISVN)*EW(ISVN)*SEX/(1-E*CEX)**2*DEDE
      DV2DE=DV2DE-E/EW(ISVN)*A*N(ISVN)*CEX/(1-E*CEX)+
     1      A*N(ISVN)*EW(ISVN)*CEX**2/(1-E*CEX)**2
      DO 130 K=1,3
        DRDELH(K,2)=ABCP(K,1)*DXI1DE+ABCP(K,2)*DXI2DE
        DVDELH(K,2)=ABCP(K,1)*DV1DE+ABCP(K,2)*DV2DE
130   CONTINUE
C
C 5. SECOND PART OF PARTIAL WITH RESPECT TO PER,
C    PARTIAL WITH RESPECT TO U0
C    -------------------------------------------
      DEDU0=1/(1-E*CEX)*DM0DU0(ISVN)
      DXI1DU=-A*SEX*DEDU0
      DXI2DU= A*EW(ISVN)*CEX*DEDU0
      DV1DU0=-A*N(ISVN)/(1-E*CEX)**2*(CEX-E)*DEDU0
      DV2DU0=-A*N(ISVN)*EW(ISVN)*SEX/(1-E*CEX)**2*DEDU0
      DO 140 K=1,3
        DRDELH(K,6)=ABCP(K,1)*DXI1DU+ABCP(K,2)*DXI2DU
        DRDELH(K,5)=DRDELH(K,5)-(ABCP(K,1)*DXI1DU+ABCP(K,2)*DXI2DU)
        DVDELH(K,6)=ABCP(K,1)*DV1DU0+ABCP(K,2)*DV2DU0
        DVDELH(K,5)=DVDELH(K,5)-(ABCP(K,1)*DV1DU0+ABCP(K,2)*DV2DU0)
140   CONTINUE
C
C INCLUDE PERTURBATIONS
C ---------------------
      DO 200 J=1,6
        DO 150 K=1,3
          DRDELE(K,J)=0.D0
          DVDELE(K,J)=0.D0
150     CONTINUE
        DO 190 L=1,6
          IF(L.LT.4.OR.L.EQ.6)THEN
            IF(J.NE.L)GO TO 190
            COE=1
          ELSE
            IF(L.EQ.4)THEN
              COE=DNDELE(J)
            ELSE
              COE=DPDELE(J)
            END IF
          END IF
          DO 180 K=1,3
            DRDELE(K,J)=DRDELE(K,J)+COE*DRDELH(K,L)
            DVDELE(K,J)=DVDELE(K,J)+COE*DVDELH(K,L)
180       CONTINUE
190     CONTINUE
200   CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
