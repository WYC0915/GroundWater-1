      MODULE s_TIDPOT
      CONTAINS

C*
      SUBROUTINE TIDPOT(K2,XSAT,SUN,MOON,THETA,PRE,NUT,SID,ORBMOD,A)
CC
CC NAME       :  TIDPOT
CC
CC PURPOSE    :  PERTURBING ACCELERATION DUE TO FIXED-BODY TIDES
CC               MODEL EXACTLY CORRESPONDING TO IERS STANDARDS 1996
CC               ONLY STEP 1 CORRECTIONS INCLUDED!
CC
CC PARAMETERS :
CC        IN  :  K2      : LOVE CONSTANT                             R*8
CC               XSAT    : POSITION VECTOR OF SATELLITE              R*8
CC               SUN     : GEOCENTRIC POSITION VECTOR OF SUN         R*8
CC               MOON    : GEOCENTRIC POSITION VECTOR OF MOON        R*8
CC               THETA   : SIDEREAL TIME GREENWICH                   R*8
CC               PRE     : PRECESSION MATRIX                         R*8
CC               NUT     : NUTATION MATRIX                           R*8
CC               SID     : SIDEREAL TIME MATRIX                      R*8
CC               ORBMOD : ORBIT MODEL ARRAY                          I*4(*)
CC        OUT :  A       : RESULTING ACCELERATION (M/S)              R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  4.0  (JUNE 96)
CC
CC CREATED    :  94/05/11
CC
CC CHANGES    :  27-JUN-96 : TS: ADDED ORBIT MODEL ARRAY
CC               10-JUL-96 : TS: USE K2 TO REMOVE PERMANENT TIDE FROM C02
CC               03-MAY-00 : HB: USE OF NEW SR DUPRLB INSTEAD OF SR DENORM,
CC                               LPCOEF,LEGPOL, AND DGPRLB,
CC                               ADD THE FIRST THREE POTENTIAL TERMS
CC                               CPOT(I),SPOT(I)=0.D0,(I=1,3),REMOVE PNM
CC                               AND COEFLP
CC               30-JUL-02 : HU: USE INTERFACE FOR DUPRLB
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1994     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE d_const, ONLY: AE, GM, GMM, GMS
      USE s_duprlb
      USE s_dgpxyz
      USE s_dmlmav
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 ISM   , K     , M
C
      REAL*8    GSM   , P20   , P21   , P22   , R     , R3    , SINPHI,
     1          THETA , UPOT  , XLONG
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
CCC       IMPLICIT INTEGER*4 (I-N)
C
      INTEGER*4 ORBMOD(*)
C
      REAL*8    K2,XSAT(*),SUN(*),MOON(*),A(*),PRE(3,*),NUT(3,*)
      REAL*8    XSM(3),CPOT(6),SPOT(6),DU1RLB(3)
      REAL*8    DUMMY(3,3),XSUN(4),XMOON(4),SID(3,*)
C
C
C INITIALIZE TERMS OF GEOPOTENTIAL
C --------------------------------
      DO 1 M=1,6
        CPOT(M)=0.D0
        SPOT(M)=0.D0
1     CONTINUE
C
C TRANSFORMATION INTO EARTH-FIXED SYSTEM
C --------------------------------------
      CALL DMLMAV(SUN,PRE,XSUN)
      CALL DMLMAV(XSUN,NUT,XSUN)
      CALL DMLMAV(XSUN,SID,XSUN)
      CALL DMLMAV(MOON,PRE,XMOON)
      CALL DMLMAV(XMOON,NUT,XMOON)
      CALL DMLMAV(XMOON,SID,XMOON)
C
C LOOP OVER SUN AND MOON FOR STEP 1 CORRECTIONS:
C ---------------------------------------------
C
      DO 100 ISM=1,2
C
C SUN OR MOON?
        IF(ISM.EQ.1)THEN
          GSM=GMM
          DO 10 K=1,3
            XSM(K)=XMOON(K)
10        CONTINUE
        ELSE
          GSM=GMS
          DO 20 K=1,3
            XSM(K)=XSUN(K)
20        CONTINUE
        END IF
C
C CHANGE IN PEOPOTENTIAL TERMS
        R=DSQRT(XSM(1)**2+XSM(2)**2+XSM(3)**2)
        R3=R**3
        SINPHI=XSM(3)/R
        XLONG =DATAN2(XSM(2),XSM(1))
        P20=0.5D0*(3.D0*SINPHI**2-1.D0)
        P21=3.D0*SINPHI*DSQRT(1.D0-SINPHI**2)
        P22=3.d0*(1.D0-SINPHI**2)
        CPOT(4)=CPOT(4)+ 1.D0/DSQRT(5.D0)*K2*AE**3/GM*GSM/R3*P20
        CPOT(5)=CPOT(5)+DSQRT(1.D0/15.D0)*K2*AE**3/GM*GSM/R3*P21*
     1                  DCOS(XLONG)
        SPOT(5)=SPOT(5)+DSQRT(1.D0/15.D0)*K2*AE**3/GM*GSM/R3*P21*
     1                  DSIN(XLONG)
        CPOT(6)=CPOT(6)+DSQRT(1.D0/60.D0)*K2*AE**3/GM*GSM/R3*P22*
     1                  DCOS(2*XLONG)
        SPOT(6)=SPOT(6)+DSQRT(1.D0/60.D0)*K2*AE**3/GM*GSM/R3*P22*
     1                  DSIN(2*XLONG)
100   CONTINUE
C
C REMOVE PREMANENT TIDE FROM C02 (FOR JGM-3 NOT FOR GEM-T3)
C ---------------------------------------------------------
      IF (ORBMOD(2).EQ.1) CPOT(4)=CPOT(4)+1.39119D-8*K2
C
C STEP 2 CORRECTIONS (K1 CONSTITUENT ONLY)
C ----------------------------------------
      CPOT(5)=CPOT(5)-507.4D-12*DSIN(THETA)
      SPOT(5)=SPOT(5)-507.4D-12*DCOS(THETA)
C
C
C COMPUTE ACCELERATION
      CALL duprlb(ae,gm,2,cpot,spot,xsat,1,upot,du1rlb,dummy)
      CALL DGPXYZ(XSAT,1,DU1RLB,DUMMY,XSM,DUMMY)
      DO 110 K=1,3
        A(K)=A(K)+XSM(K)
110   CONTINUE
C
999   CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
