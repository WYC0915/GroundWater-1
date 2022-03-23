      MODULE s_INTFST
      CONTAINS

C*
      SUBROUTINE INTFST(ITYP  ,ITIM  ,TLEFT ,TRIGHT,XV    ,Q     ,NVAR,
     1                  LOCQ  ,Z0    ,ORBMOD,NUMSAT,T0    ,H     ,YCOE,
     2                  ZCOE  ,ERRMAX)
CC
CC NAME       :  INTFST
CC
CC PURPOSE    :  INTEGRATION OF EQUATIONS OF MOTION OF A SATELLITE
CC               IN THE INTERVAL I = (TLEFT,TRIGHT).
CC               LINEARIZED EQNS, FORMULATAION IN TRADITION OF
CC               PERTURBATION THEORY. IN ADDITION NVAR VARIATIONAL
CC               EQUATIONS ARE INTEGRATED.
CC
CC PARAMETERS :
CC         IN :  ITYP   : PROBLEM CHARACTERIZATION:             I*4
CC                   =0 : INITIAL VALUE PROBLEM
CC                   =1 : BOUNDARY VALUE PROBLEM
CC               ITIM   : 0,1 (1 = RPR SPRINGER MODEL)          I*4
CC               TLEFT, TRIGHT: LEFT/ RIGHT INTERVAL BOUNDA-    R*8
CC                        RIES
CC               FOR ITYP=0: TLEFT = INITIAL EPOCH
CC               FOR ITYP=1: TLEFT, TRIGHT = BOUNDARY EPOCHS
CC               XV(K,I),I=1,2: INITIAL CONDITIONS RESP.        R*8
CC                        BOUNDARY CONDITIONS FOR RESP. PROBLEM
CC                        TYPES.
CC               Q      : DEGREE OF APPROXIMATING POLYNOMIALS   I*4
CC               NVAR   : NUMBER OF VARIATIONAL EQUATIONS
CC                        (FOR ITYP=0 ONLY)
CC               LOCQ(K,I),K=1,2,..,6, I=1,2,...,NVAR: DEFI-    I*4
CC                        NITION OF PARAMETERS
CC               Z0(K),K=1,2,..,3*NVAR*2: INITIAL CONDITIONS    R*8
CC                        FOR VARIATIONAL EQUATIONS
CC               ORBMOD : ORBIT MODEL ARRAY                     I*4(*)
CC               NUMSAT : SATELLITE NUMBER                    I*4
CC        OUT :  T0     : ORIGIN OF DEVELOPMENT                 R*8
CC               H      : LENGTH OF INTERVAL (NORMALIZATION     R*8
CC                        FACTOR)
CC               YCOE(K,I),K=1,2,3,I=1,2,..,Q+1: POLYNOMIAL     R*8
CC                        COEFFICIENTS FOR SOLUTION
CC               ZCOE(K,I),K=1,2,3,..,3*NVAR, I=1,2,...,Q+1     R*8
CC                        POLYNOMIAL COEFFICIENTS FOR VAR. EQNS
CC               ERRMAX : MAXIMUM ERROR ESTIMATED               R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/12/28 11:03
CC
CC CHANGES    :  31-MAY-92 : ??: SMALLER BOUNDARY FOR "DELTA"
CC               22-JUN-92 : ??: OPTION P0-DRIFT
CC               10-AUG-94 : MR: CALL EXITRC
CC               23-OCT-94 : GB: MORE ACCURATE COMP. OF VARIATIONAL EQNS
CC                               (REPLACE "NULL=0" BY "NULL=1")
CC               08-MAR-95 : ??: DELETE ALL CHANGES REFERING TO E2, IE2, ...
CC               08-MAR-95 : ??: IMPROVE NUMERICAL INTEGRATION IN SR SETMAT,
CC                               SOLINT (NO CHANGES IN THIS SR !)
CC               25-JAN-96 : GB: NEW ORBIT MODEL IMPLEMENTED
CC               27-JUN-96 : TS: ADDED ORBIT MODEL ARRAY
CC               16-JUN-05 : MM: COMCONST.INC REPLACED BY D_CONST
CC               21-JUN-05 : MM: COMLFNUM.INC REMOVED, M_BERN ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               18-OCT-06 : PL: ITIM ADDED
CC               04-MAY-08 : RD: NUMSAT ADDED TO CALL OF SR XYZELE,DEQRHS
CC               28-OCT-08 : DT: USE MAXVAR FROM M_MAXDIM
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE d_const,  ONLY: GM
      USE m_maxdim, ONLY: MAXVAR
      USE s_deqrhs
      USE s_vareqn
      USE s_ypolrp
      USE s_ephem
      USE s_maxtst
      USE s_setmat
      USE s_solint
      USE s_ypol
      USE s_exitrc
      USE s_xyzele
      USE s_setdt
      USE s_dmlmav
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I0    , I1    , I2    , IFIRST, IITT  , IP    ,
     1          IRC   , ITER  , IT    , ITOLD , ITYP  , IVAR  , K     ,
     2          L     , MAXQ  , MXCVAR, NULL  , NVAR  , ITIM  , NUMSAT
C
      REAL*8    A     , DELMAX, DELTA , DT    , E     , ERRMAX, H     ,
     1          HREL  , PER   , RSAT  , RSAT2 , RSAT3 , T0    , T0SEC ,
     2          TEST  , TIMDAY, TL    , TLEFT , TR    , TRIGHT, TSEC  ,
     3          TSECO , XI    , XKN
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      PARAMETER (MAXQ=20)
C
      INTEGER*4 Q,QOLD,LOCQ(6,*),ORBMOD(*)
C
      REAL*8    XV(3,2),YCOE(3,*),ZCOE(*),Z0(*)
      REAL*8    TREL(MAXQ-1),MAT((MAXQ+1)**2),TIMMAT((MAXQ+1)*(MAXQ-1))
      REAL*8    XSAT(6),XSAVE(3),F(3,MAXQ-1),A0(3,3,MAXQ-1)
      REAL*8    B(3,MAXQ+1),FAK(MAXQ+1)
      REAL*8    DFDP(3*MAXVAR*(MAXQ-1)),DBDP(3*MAXVAR*(MAXQ+1))
      REAL*8    Z(2*3*MAXVAR),PERT2(3),PERT3(3),HELP(3,3)
      REAL*8    XVHLP(6)
C
      CHARACTER*6 MXNVAR
C
      COMMON/CINTFS/ TREL,MAT,TIMMAT,F,A0,B,FAK,DFDP,DBDP
      COMMON/MCMVAR/MXCVAR,MXNVAR
C
      DATA QOLD/-1/,ITOLD/-1/,IFIRST/1/
C
C CHECK MAXVAR
C ------------
      IF(IFIRST.EQ.1)THEN
        IFIRST=0
        CALL MAXTST(0,'INTFST',MXNVAR,MAXVAR,MXCVAR,IRC)
        IF(IRC.NE.0)THEN
          WRITE(LFNERR,5011)MAXVAR
5011      FORMAT(/,' *** SR INTFST : MAXVAR TOO SMALL:',I4,/)
          CALL EXITRC(2)
        END IF
      END IF
C
C COMPUTE FACTORIALS
C ------------------
      IF(QOLD.EQ.-1) THEN
        FAK(1)=1.D0
        DO 11 K=1,MAXQ
          FAK(1+K)=FAK(K)*K
11      CONTINUE
      END IF
C
C CHECK MAXIMUM DIMENSIONS
C ------------------------
      IF(Q.GT.MAXQ) THEN
        WRITE(LFNERR,1) Q,MAXQ
1       FORMAT(/,' *** SR INTFST: MAXIMUM POLYNOMIAL DEGREE EXCEEDED',/,
     1                       16X,'POLYNOMIAL DEGREE:',I4,/,
     2                       16X,'MAX. POLY. DEGREE:',I4,/)
        CALL EXITRC(2)
      END IF
C
C INITIAL VALUE PROBLEM (NORMAL INTEGRATION STEP)
C -----------------------------------------------
      IF(ITYP.EQ.0) THEN
C
C A. INITIAL VALUE PROBLEM
C ------------------------
        H=(TRIGHT-TLEFT)*86400.D0
        T0=TLEFT
        IF(ITYP.NE.ITOLD.OR.Q.NE.QOLD)THEN
          QOLD=Q
          ITOLD=ITYP
C
C A.1 INITIALIZE PROBLEM
C     ------------------
          TL=(TLEFT -T0)/(TRIGHT-TLEFT)
          TR=(TRIGHT-T0)/(TRIGHT-TLEFT)
          CALL SETDT(1,Q-1,TL,TR,HREL,TREL)
          CALL SETMAT(ITYP,Q,TL,TR,TREL,MAT,TIMMAT)
        END IF
        DO 2 K=1,3
          B(K,1)=XV(K,1)
          B(K,2)=XV(K,2)*H
2       CONTINUE
C
C A 2. DEFINE OSCULATING, THEN MEAN ELEMENTS AT TIME T0
C      ------------------------------------------------
        CALL XYZELE(GM,0.D0,XV,XV(1,2),NUMSAT,A,E,XI,XKN,PER,T0SEC)
        DO 100 I=1,Q-1
          TIMDAY=T0+H/86400.D0*TREL(I)
          TSEC=H*TREL(I)
          CALL EPHEM(GM,A,E,XI,XKN,PER,T0SEC,TSEC,XSAT,XSAT(4))
C
C APPLY THIRD ORDER PERTURBATION CORRECTION
          IF(I.NE.1)THEN
            DO 4 K=1,3
              XSAT(K)=XSAT(K)+.5*(TSEC-TSECO)**2*PERT2(K)
     1                +1.D0/6.D0*(TSEC-TSECO)**3*PERT3(K)
              XSAT(3+K)=XSAT(3+K)+(TSEC-TSECO)*PERT2(K)
     1                +1.D0/2.D0*(TSEC-TSECO)**2*PERT3(K)
4           CONTINUE
            CALL XYZELE(GM,TSEC,XSAT,XSAT(4),NUMSAT,
     1                  A,E,XI,XKN,PER,T0SEC)
          END IF
          I0=3*(I-1)*NVAR
          CALL DEQRHS(NUMSAT,ITIM,TIMDAY,XSAT,NVAR,LOCQ,ORBMOD,
     1                F(1,I),A0(1,1,I),DFDP(I0+1))
          CALL DMLMAV(XSAT,A0(1,1,I),B(1,I+2))

          DO 10 K=1,3
            IF(I.EQ.Q-1)XSAVE(K)=XSAT(K)
            B(K,I+2)=H**2*(B(K,I+2)+F(K,I))
10        CONTINUE
C
C COMPUTE THIRD ORDER PERTUBATION CORRECTION
C         IF(I.EQ.1)THEN
            RSAT=DSQRT(XSAT(1)**2+XSAT(2)**2+XSAT(3)**2)
            RSAT2=RSAT**2
            RSAT3=RSAT**3
            DO 5 K=1,3
              PERT2(K)=B(K,I+2)/H**2+GM*XSAT(K)/RSAT3
5           CONTINUE

            DO 6 L=1,3
            DO 6 K=1,3
              HELP(L,K)= GM/RSAT3*3*XSAT(L)*XSAT(K)/RSAT2
              IF(L.EQ.K)HELP(L,K)=HELP(L,K)-GM/RSAT3
                HELP(L,K)=A0(L,K,I)-HELP(L,K)
6           CONTINUE
            DO 7 L=1,3
              PERT3(L)=0.D0
              DO 7 K=1,3
                PERT3(L)=PERT3(L)+HELP(L,K)*XSAT(3+K)
7           CONTINUE
C         END IF
          TSECO=TSEC
100     CONTINUE
C
C FIRST STEP IN ITERATIVE SOLUTION OF INITIAL VALUE PROBLEM
        CALL SOLINT(Q,3,MAT,B,YCOE)
C
C ITERATIVE IMPROVEMENT OF THIS FIRST SOLUTION
        DO 200 ITER=1,10
          DO 130 IP=2,Q-1
            IT =(IP-1)*(Q+1)+1
            CALL YPOLRP(Q,3,TIMMAT(IT),YCOE,XSAT)
            CALL DMLMAV(XSAT,A0(1,1,IP),B(1,2+IP))
            DO 120 K=1,3
              B(K,2+IP)=H**2*(B(K,2+IP)+F(K,IP))
120         CONTINUE
            IF(IP.EQ.Q-1)THEN
              DELTA=0.D0
              DO 121 K=1,3
                DELTA=DELTA+(XSAT(K)-XSAVE(K))**2
                XSAVE(K)=XSAT(K)
121           CONTINUE
            END IF
130       CONTINUE
          CALL SOLINT(Q,3,MAT,B,YCOE)
          DELTA=DSQRT(DELTA)
          IF(DELTA.LT.0.01D0)GO TO 201
200     CONTINUE
201     CONTINUE
C
C SOLVE VARIATIONAL EQUATIONS
C ---------------------------
        IF(NVAR.EQ.0)GO TO 999

C
C INITIALIZE SOLUTION
        CALL VAREQN(NVAR,Q,T0,H,Z0,A0,DFDP,Z,MAT,DBDP,ZCOE)
C
C ITERATIVE IMPROVEMENT OF THIS SOLUTION (NOT ACTIVE)
        NULL=2
        DO 190 ITER=1,NULL
          DO 180 IP=2,Q-1
            IT=(IP-1)*(Q+1)+1
            CALL YPOLRP(Q,3*NVAR,TIMMAT(IT),ZCOE,Z)
            I0=3*NVAR*(IP+1)
            I2=3*NVAR*(IP-1)
            DO 170 IVAR=1,NVAR
              I1=3*(IVAR-1)
              CALL DMLMAV(Z(I1+1),A0(1,1,IP),DBDP(I0+I1+1))
              DO 170 K=1,3
                DBDP(I0+I1+K)=H**2*(DBDP(I0+I1+K)+DFDP(I2+I1+K))
170         CONTINUE
180       CONTINUE
          CALL SOLINT(Q,3*NVAR,MAT,DBDP,ZCOE)
          DO 181 IP=2,Q-1
            IT=(IP-1)*(Q+1)+1
            CALL YPOLRP(Q,3*NVAR,TIMMAT(IT),ZCOE,Z)
181       CONTINUE
182       FORMAT(I4,I3,9D10.2)
190     CONTINUE
C
      ELSE IF(ITYP.EQ.1)THEN
C
C B. BOUNDARY VALUE PROBLEM
C -------------------------
        QOLD=Q
        ITOLD=ITYP
C
C B.1 INITIALIZE PROBLEM
        H=(TRIGHT-TLEFT)*86400.D0
        T0=(TLEFT+TRIGHT)/2
        TL=(TLEFT -T0)/(TRIGHT-TLEFT)
        TR=(TRIGHT-T0)/(TRIGHT-TLEFT)
C
C INITIALIZE COEFFICIENTS
        DO 210 I=5,Q+1
          DO 210 K=1,3
            YCOE(K,I)=0.D0
210     CONTINUE
        do 211 k=1,3
          xvhlp(k)=xv(k,1)
          xvhlp(3+k)=(xv(k,2)-xv(k,1))/h
211     continue
c
        CALL DEQRHS(NUMSAT,ITIM,TLEFT ,XVhlp,0,LOCQ,ORBMOD,
     1              F(1,1)  ,A0(1,1,1),DFDP)
        do 212 k=1,3
          xvhlp(k)=xv(k,2)
212     continue
        CALL DEQRHS(NUMSAT,ITIM,TRIGHT,XVhlp,0,LOCQ,ORBMOD,
     1              F(1,Q-1),A0(1,1,Q-1),DFDP)
        CALL DMLMAV(XV(1,1),A0(1,1,1),B(1,3))
        CALL DMLMAV(XV(1,2),A0(1,1,Q-1),B(1,Q+1))
        DO 215 K=1,3
          B(K,3)=H**2*(B(K,3)+F(K,1))
          B(K,Q+1)=H**2*(B(K,Q+1)+F(K,Q-1))
215     CONTINUE
        DT=TR-TL
        DO 220 K=1,3
          YCOE(K,4)=1.D0/(6*DT)*(B(K,Q+1)-B(K,3))
          YCOE(K,3)=1.D0/(2*DT)*(TR*B(K,3)-TL*B(K,Q+1))
          YCOE(K,2)=(XV(K,2)-XV(K,1))/DT-(TL+TR)*YCOE(K,3)
     1             -(TR**3-TL**3)/DT*YCOE(K,4)
          YCOE(K,1)=(TR*XV(K,1)-TL*XV(K,2))/DT+TL*TR*YCOE(K,3)
     1             +TL*TR*(TL+TR)*YCOE(K,4)
220     CONTINUE
        CALL SETDT(1,Q-1,TL,TR,HREL,TREL)
        CALL SETMAT(ITYP,Q,TL,TR,TREL,MAT,TIMMAT)
C
        DO 230 K=1,3
          B(K,1)=XV(K,1)
          B(K,2)=XV(K,2)
230     CONTINUE
C
        DELMAX=500.D0
        DO 500 IITT=1,3
          DO 300 I=1,Q-1
            TSEC=H*TREL(I)
            TIMDAY=T0+H/86400.D0*TREL(I)
            CALL YPOL(1,Q,3,H,FAK,TSEC,YCOE,XSAT)
            CALL DEQRHS(NUMSAT,ITIM,TIMDAY,XSAT,0,LOCQ,ORBMOD,
     1                  F(1,I),A0(1,1,I),DFDP)
            CALL DMLMAV(XSAT,A0(1,1,I),B(1,I+2))
            DO 240 K=1,3
              IF(I.EQ.(Q-1)/2)XSAVE(K)=XSAT(K)
              B(K,I+2)=H**2*(B(K,I+2)+F(K,I))
240         CONTINUE
300       CONTINUE
C
C ITERATIVE IMPROVEMENT OF THIS FIRST SOLUTION
          DO 400 ITER=1,20
            DO 330 IP=1,Q-1
              IT=(IP-1)*(Q+1)+1
              CALL YPOLRP(Q,3,TIMMAT(IT),YCOE,XSAT)
              CALL DMLMAV(XSAT,A0(1,1,IP),B(1,2+IP))
              IF(IP.EQ.(Q-1)/2)DELTA=0.D0
              DO 320 K=1,3
                IF(IP.EQ.(Q-1)/2)THEN
                  DELTA=DELTA+(XSAT(K)-XSAVE(K))**2
                  XSAVE(K)=XSAT(K)
                END IF
                B(K,2+IP)=H**2*(B(K,2+IP)+F(K,IP))
320           CONTINUE
330         CONTINUE
            CALL SOLINT(Q,3,MAT,B,YCOE)
            DELTA=DSQRT(DELTA)
            IF(ITER.GT.1.AND.DELTA.LT.DELMAX)GO TO 401
400       CONTINUE
401       DELMAX=.10D0
500     CONTINUE
        GO TO 999
C
      ELSE
C
C INVALID INTEGRATION TYPE
C ------------------------
        WRITE(LFNERR,991) ITYP
991     FORMAT(/,' *** SR INTFST: ILLEGAL INTEGRATION PROBLEM TYPE',/,
     1                       16X,'PROBLEM TYPE: ',I3,/)
        CALL EXITRC(2)
      END IF
C
C END
C ---
999   CONTINUE
      ERRMAX=0.D0
      DO 1000 K=1,3
        TEST=DABS(YCOE(K,Q+1)*.6D0**Q)/(Q+1)
        IF(TEST.GT.ERRMAX)ERRMAX=TEST
1000  CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
