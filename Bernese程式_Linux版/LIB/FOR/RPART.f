      MODULE s_RPART
      CONTAINS

C*
        SUBROUTINE RPART(GM,T,R,A,E,I,KN,PER,T0,DRDELE,D2RDT0)
CC
CC NAME       :  RPART
CC
CC PURPOSE    :  COMPUTE PARTIAL DERIVATIVES OF POSITION
CC               VECTOR R(K),K=1,2,3 WITH RESPECT TO
CC               ORBITAL E;LEMENTS A, E, I, KN, PER, T0
CC
CC PARAMETERS :
CC         IN :  GM     : GRAVITY CONST. * MASS OF EARTH      R*8
CC               T      : TIME                                R*8
CC               R(K),K=1,2,3: POSITION VECTOR                R*8
CC               A, E, I, KN, PER, T0: KEPLERIAN ELEMENTS     R*8
CC                        (SEE E.G. SR EPHEM)
CC        OUT :  DRDELE(K,I),K=1,2,3,I=1,2,..,6               R*8
CC                        PARTIALS OF R WITH RESPECT TO
CC                        A, E, I, KN, PER, T0 (THIS SEQUENCE)
CC               D2RDT0(K),K=1,2,3: SECOND PARTIAL DERIVATIVE R*8
CC                        OF R WITH RESPECT TO T0
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/30 08:11
CC
CC CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
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
      INTEGER*4 I7    , K
C
      REAL*8    A     , CI    , CK    , CP    , D2EDT0, DEDA  , DEDE  ,
     1          DEDT0 , E     , EW    , EX    , GM    , H1    , H2    ,
     2          PER   , RR    , SI    , SK    , SP    , T     , T0    ,
     3          X1    , X2    , Y1    , Y2
C
CCC         IMPLICIT REAL*8 (A-H,O-Z)
        REAL*8 I,KN,N,M
        REAL*8 DRDELE(3,6),R(3),D2RDT0(3)
        REAL*8 ABC(3,2),D(3,2),E1(3,2),F(3,2)
        RR=DSQRT(R(1)**2+R(2)**2+R(3)**2)
        N=DSQRT(GM/A**3)
        M=N*(T-T0)
        EX=M
        DO 2 I7=1,5
        EX=EX+(M-EX+E*DSIN(EX))/(1-E*DCOS(EX))
2       CONTINUE
        H1=-A*DSIN(EX)
        EW=DSQRT(1-E**2)
        H2=A*EW*DCOS(EX)
        X1=A*(DCOS(EX)-E)
        X2=A*EW*DSIN(EX)
C
        CK=DCOS(KN)
        SK=DSIN(KN)
        CI=DCOS(I)
        SI=DSIN(I)
        CP=DCOS(PER)
        SP=DSIN(PER)
C
        ABC(1,1)= CK*CP-SK*CI*SP
        ABC(2,1)= SK*CP+CK*CI*SP
        ABC(3,1)= SI*SP
        ABC(1,2)=-CK*SP-SK*CI*CP
        ABC(2,2)=-SK*SP+CK*CI*CP
        ABC(3,2)= SI*CP
C
        D(1,1)=-SK*CP-CK*CI*SP
        D(2,1)= CK*CP-SK*CI*SP
        D(3,1)= 0
        D(1,2)= SK*SP-CK*CI*CP
        D(2,2)=-CK*SP-SK*CI*CP
        D(3,2)= 0
C
        E1(1,1)= SK*SI*SP
        E1(2,1)=-CK*SI*SP
        E1(3,1)= CI*SP
        E1(1,2)= SK*SI*CP
        E1(2,2)=-CK*SI*CP
        E1(3,2)= CI*CP
C
        F(1,1)=-CK*SP-SK*CI*CP
        F(2,1)=-SK*SP+CK*CI*CP
        F(3,1)= SI*CP
        F(1,2)=-CK*CP+SK*CI*SP
        F(2,2)=-SK*CP-CK*CI*SP
        F(3,2)=-SI*SP
C
        DO 3 K=1,3
        DRDELE(K,3)=E1(K,1)*X1+E1(K,2)*X2
        DRDELE(K,4)=D(K,1)*X1+D(K,2)*X2
3       DRDELE(K,5)=F(K,1)*X1+F(K,2)*X2
C
        DEDA=-1.5*N/RR*(T-T0)
        DEDE= A*DSIN(EX)/RR
        DEDT0=-N*A/RR
        D2EDT0=-A/RR*E*DSIN(EX)*DEDT0**2
        Y1=X1/A+H1*DEDA
        Y2=X2/A+H2*DEDA
        DO 4 K=1,3
4       DRDELE(K,1)=ABC(K,1)*Y1+ABC(K,2)*Y2
C
        Y1=-A+H1*DEDE
        Y2=-A*E/EW*DSIN(EX)+H2*DEDE
        DO 5 K=1,3
5       DRDELE(K,2)=ABC(K,1)*Y1+ABC(K,2)*Y2
        Y1=H1*DEDT0
        Y2=H2*DEDT0
        DO 6 K=1,3
6       DRDELE(K,6)=ABC(K,1)*Y1+ABC(K,2)*Y2
        Y1=-A*DCOS(EX)*DEDT0**2-H1*D2EDT0
        Y2=-A*EW*DSIN(EX)*DEDT0**2+H2*D2EDT0
        DO 7 K=1,3
7       D2RDT0(K)=ABC(K,1)*Y1+ABC(K,2)*Y2
        RETURN
        END SUBROUTINE

      END MODULE
