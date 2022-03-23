      MODULE s_VAREQN
      CONTAINS

C*
      SUBROUTINE VAREQN(NVAR,Q,T0,H,Z0,A0,DFDP,Z,MAT,DBDP,ZCOE)
CC
CC NAME       :  VAREQN
CC
CC PURPOSE    :  INTEGRATE VARIATIONAL EQUATIONS ASSOCIATED
CC               WITH PRIMARY EQNS.
CC
CC PARAMETERS :
CC         IN :  NVAR   : NUMBER OF VARIATIONAL EQN SYSTEMS   I*4
CC               H      : LENGTH OF PARTIAL INTERVAL          R*8
CC               Q      : ORDER OF INTEGRATION                I*4
CC               T0     : ORIGIN OF DEVELOPMENT               R*8
CC               H      : INTERVAL LENGTH                     R*0
CC               Z0(I),I=1,2,...: INITIAL CONDITIONS          R*8
CC               A0(L,K,I),L,K=1,2,3, I=1,2,..,Q-1            R*8
CC                        JACOBIAN OF FORCE VECTOR AT TIME TI
CC               DFDP(I),I=1,2,...,NVAR*3*(Q-1): PARTIALS     R*8
CC                        OF FORCE VECTOR WITH RESPECT TO
CC               MAT(I),I=1,2,..,(Q+1)*(Q+1): MATRIX USED FOR R*8
CC                        FINAL INTEGRATION
CC               Z(I),I=1,2,...:  AUX. ARRAY                  R*8
CC        OUT :  DBDP(I),I=1,2,...,NVAR*3*(Q+1): AUX. ARRAY   R*8
CC                        (SEE SR INTFST)
CC               ZCOE(I),I=1,2,... : COEFFICIENTS OF          R*8
CC                        EXPANSION FOR VARIATIONAL EQNS.
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  88/01/04 08:01
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC               25-JAN-96 : GB: NEW ORBIT MODEL IMPLEMENTED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-OCT-08 : DT: USE MAXVAR FROM M_MAXDIM
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE m_maxdim, ONLY: maxVar
      USE s_solint
      USE s_exitrc
      USE s_dmlmav
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I0    , I0P   , I1    , I2    , I3    , I33   ,
     1          I4    , I44   , IP    , IPT   , ITER  , IVAR  , K     ,
     2          N     , NVAR
C
      REAL*8    DT    , H     , T0    , TREL
C
CCC       IMPLICIT REAL*8    (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      INTEGER*4 Q
      REAL*8 A0(3,3,*),DFDP(*),DBDP(*),ZCOE(*),Z0(*),Z(*),MAT(*)
      REAL*8 Z0011(2*3*MAXVAR)
C
C CHECK NUMBER OF VARIATIONAL EQNS
      IF(NVAR.GT.MAXVAR)THEN
        WRITE(LFNERR,501)NVAR,MAXVAR
501     FORMAT(/,' *** SR VAREQN: TOO MANY VARIATIONAL EQUATIONS',/,
     1                       16X,'NUMBER OF VARIATIONAL EQNS.:',I4,/,
     2                       16X,'MAXIMUM NUMBER OF VAR.EQNS.:',I4,/)
        CALL EXITRC(2)
      END IF
C
C INITIALIZE LOCAL INITIAL CONDITIONS
      N=2*3*NVAR
      DO 1 I=1,N
        Z0011(I)=Z0(I)
1     CONTINUE
C
C MAIN LOOP
      DO 200 IPT=1,Q-3
C
C DEFINE INITIAL CONDITIONS:
        DO 10 I=1,2
          I0=3*NVAR*(I-1)
          DO 10 K=1,3*NVAR
            IF(IPT.EQ.1)DBDP(I0+K)=Z0011(I0+K)*H**(I-1)
            ZCOE(I0+K)=Z0011(I0+K)*H**(I-1)
10      CONTINUE
C
C SECOND DERIVATIVES AT POINT IPT
        I3=3*NVAR*(IPT-1)
        I0=3*2*NVAR+I3
        DO 20 IVAR=1,NVAR
          I1=3*(IVAR-1)
          I2=I0+I1
          CALL DMLMAV(Z0011(I1+1),A0(1,1,IPT),DBDP(I2+1))
          DO 20 K=1,3
            DBDP(I2+K)=H**2*(DBDP(I2+K)+DFDP(I1+I3+K))
            ZCOE(3*2*NVAR+I1+K)=DBDP(I2+K)/2
20      CONTINUE
C
C DEFINE SOLUTION UP TO TERMS OF ORDER 4
        DT=1.D0/(Q-2)
        DO 100 ITER=1,1
        DO 50 IP=2,3
          I0= 3*NVAR*(IPT-1+IP+1)
          I0P=3*NVAR*(IPT-1+IP-1)
          TREL=(IP-1)*DT
          DO 40 IVAR=1,NVAR
            I1=3*(IVAR-1)
            I2=I0+I1
            DO 30 K=1,3
              Z(K)=ZCOE(I1+K)+
     1                ZCOE(1*3*NVAR+I1+K)*TREL+
     2                ZCOE(2*3*NVAR+I1+K)*TREL**2
              IF(ITER.GT.1)THEN
                Z(K)=Z(K)+
     3                  ZCOE(3*3*NVAR+I1+K)*TREL**3+
     4                  ZCOE(4*3*NVAR+I1+K)*TREL**4
              END IF
30          CONTINUE
            CALL DMLMAV(Z,A0(1,1,IPT-1+IP),DBDP(I2+1))
            DO 35 K=1,3
              DBDP(I2+K)=H**2*(DBDP(I2+K)+DFDP(I0P+I1+K))
35          CONTINUE
40        CONTINUE
50      CONTINUE
        DO 60 K=1,3*NVAR
          I2=2*(3*NVAR)+3*(IPT-1)*NVAR
          I3=3*(3*NVAR)+3*(IPT-1)*NVAR
          I4=4*(3*NVAR)+3*(IPT-1)*NVAR
          I33=3*(3*NVAR)
          I44=4*(3*NVAR)
          ZCOE(I44+K)=(DBDP(I2+K)-2*DBDP(I3+K)+DBDP(I4+K))/(24*DT**2)
          ZCOE(I33+K)=(DBDP(I3+K)-DBDP(I2+K))/(6*DT)-2*DT*ZCOE(I44+K)
60      CONTINUE
100     CONTINUE
C
C COMPUTE NEW INITIAL CONDITIONS
        TREL=1.D0/(Q-2)
        DO 110 K=1,3*NVAR
              Z0011(K)= ZCOE(K)
     1                 +ZCOE(1*3*NVAR+K)*TREL
     2                 +ZCOE(2*3*NVAR+K)*TREL**2
     3                 +ZCOE(3*3*NVAR+K)*TREL**3
     4                 +ZCOE(4*3*NVAR+K)*TREL**4
              Z0011(3*NVAR+K)=    ZCOE(1*3*NVAR+K)
     1                        +2 *ZCOE(2*3*NVAR+K)*TREL
     2                        +3 *ZCOE(3*3*NVAR+K)*TREL**2
     3                        +4 *ZCOE(4*3*NVAR+K)*TREL**3
              Z0011(3*NVAR+K)=Z0011(3*NVAR+K)/H
110     CONTINUE
200   CONTINUE
201   CONTINUE
C
C PERFORM FIRST APPROXIMATE SOLUTION
      CALL SOLINT(Q,3*NVAR,MAT,DBDP,ZCOE)
      RETURN
      END SUBROUTINE

      END MODULE
