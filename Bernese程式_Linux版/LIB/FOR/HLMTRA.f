      MODULE s_HLMTRA
      CONTAINS

C*
      SUBROUTINE HLMTRA(NSTAT,XSTAT,YSTAT,ITYP,IP,IOPT,
     1                  AELL,BELL,DXELL,DRELL,SCELL,VRES,RMS,
     2                  NPACT,NB,Z0,SIGMA)
CC
CC NAME       :  HLMTRA
CC
CC PURPOSE    :  COMPUTE TRANSFORMATION PARAMETERS BETWEEN 2 COORD.
CC               SYSTEMS (XSTAT TO YSTAT)
CC
CC               SR HLMTRA IS A MODIFIED VERSION OF SR HELMTR
CC
CC               SR IS USED FOR PGM ADDNEQ: FOR FREE SOLUTIONS
CC               ALL TRANSFORMATION PARAMETERS ARE ZERO USING
CC               A LOCAL COORDINATE SYSTEM
CC
CC PARAMETERS :
CC         IN :  NSTAT  : TOTAL NUMBER OF SITES               I*4
CC               XSTAT(3,I),I=1,..,NSTAT: STATION COORDINATES R*8
CC                         IN FILE 1
CC               YSTAT(3,I),I=1,..,NSTAT: STATION COORDINATES R*8
CC                         IN FILE 2
CC               ITYP(I),I=1,..,NSTAT: TYPE OF PROCESSING FOR I*4
CC                         STATION I:
CC                         =0: USE SITE
CC                         =1: COMPUTE RESIDUALS ONLY
CC                         =2: DO NOT USE AT ALL
CC               IP(7)  : VECTOR CHARACTERIZING TRANSFORMATION I*4
CC                        PARAMETERS TO BE USED IN THE TRANSF.
CC                        IP(1)=1: TRANSLATION  IN X DIRECTION
CC                        IP(2)=1:                 Y
CC                        IP(3)=1:                 Z
CC                        IP(4)=1: ROTATION AROUND X AXIS
CC                        IP(5)=1:                 Y
CC                        IP(6)=1:                 Z
CC                        IP(7)=1: SCALE FACTOR
CC                        IF IP(1),IP(2),IP(3)=0: TRANSFORM X COORD
CC                        INTO BARICENTER OF Y COORD.
CC               IOPT   : TRANSFORMATION OPTION               I*4
CC                        =1: USE LOCAL SYSTEM (N,E,UP)
CC                        =2: USE EQUATORIAL SYSTEM
CC               AELL   : SEMI-MAJOR AXIS OF ELLIPSOID        R*8
CC               BELL   : SEMI-MINOR AXIS OF ELLIPSOID        R*8
CC               DXELL(I),I=1,2,3: SHIFTS TO WGS-84 (M)       R*8
CC               DRELL(I),I=1,2,3: ROTATIONAL TO WGS-84       R*8
CC               SCELL  : SCALE FACTOR BETWEEN LOCAL GEODETIC R*8
CC                        DATUM AND WGS-84
CC        OUT :  VRES(I,J): I=1,3;J=1,NSTAT: RESIDUALS IN M   R*8
CC                        ACCORDING TO OPTION IOPT
CC               RMS    : RMS OF TRANSFORMATION               R*8
CC               NPACT  : NUMBER OF PARAMETER                 I*4
CC               NB     : NUMBER OF COORDINATES               I*4
CC               Z0(I)  : I=1,7 HELMERT TRAFO PARAMETER       R*8
CC               SIGMA(I):I=1,7 RMS OF TRAFO PARAMTER         R*8
CC                        (1,2,3: [M], 4,5,6 [ARCSEC],
CC                         7 [1-MM/KM])
CC
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  E.BROCKMANN
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  88/02/03 17:22
CC
CC CHANGES    :  27-SEP-91 : ??: PRINT TRANS.PARAMETER WITH 3 DIGITS
CC               10-AUG-94 : MR: CALL EXITRC
CC               18-OCT-95 : EB: MAXITER = 10
CC               19-MAY-00 : RD: MAXSTA=350 (OLD 300)
CC               06-SEP-01 : RD: MAXSTA=500 (OLD 350)
CC               30-AUG-02 : MR: MAXSTA=700 (OLD 500)
CC               28-JUN-04 : RD: ALLOCATE INSTEAD OF MAXSTA
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-FEB-07 : AG: USE PI AND 206264... FROM DEFCON
CC               10-MAR-09 : SS: ENABLE NULL TRANSFORMATION
CC               14-FEB-11 : RD: REMOVE MAXSTA-COMMON (UNUSED)
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE d_const, ONLY: pi, ars
      USE s_dminv
      USE s_dmlmac
      USE s_dmabl
      USE s_dmlmam
      USE s_ddreh
      USE s_dmlmav
      USE s_xyzell
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I    , IBARI, IEND , IK   , IOPT , ITER , J    , K    ,
     1          L    , NB   , NP   , NPACT, NSTAT, NTYP0
C
      REAL*8    AELL , BELL , DET  , RMS  , SCELL, V    , VV
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER*4    IP(7),L0(7),M0(7),ITYP(*)
      REAL*8       DXELL(3),DRELL(3)
      REAL*8       XSTAT(3,*),YSTAT(3,*),XSCH(3),YSCH(3),YSCHEL(3)
      REAL*8       XCOORD(3,NSTAT),YCOORD(3,NSTAT)
      REAL*8       R1(3,3),R2(3,3),R3(3,3),R(3,3),RS(3,3)
      REAL*8       D3R2R1(3,3),R3D2R1(3,3),R3R2D1(3,3),X0ROT(3),X0(3)
      REAL*8       HVEC(3),HMAT(3,3),D1(3,3),D2(3,3),D3(3,3)
      REAL*8       A(3,7),B(7),Z(7),Z0(*),ABSGL(3),Q(49),GVEC(3)
      REAL*8       VRES(3,*),SIGMA(*)
C
C      COMMON/CHLMTR/XCOORD,YCOORD
C
C      PI=4*DATAN(1.D0)
C
C SHIFT INTO BARICENTER?
C ----------------------
      IBARI=1
      DO 15 I=1,3
        IF(IP(I).NE.0) IBARI=0
15    CONTINUE
C
C NUMBER OF PARAMETERS:
C --------------------
      NP=0
      DO 10 I=1,7
        IF(IP(I).EQ.1) NP=NP+1
10    CONTINUE
C
C NUMBER OF DIFFERENT STATION TYPES
C ---------------------------------
      NTYP0=0
      DO 25 I=1,NSTAT
        IF(ITYP(I).EQ.0) NTYP0=NTYP0+1
25    CONTINUE
C
C NUMBER OF "OBSERVATIONS"
C ------------------------
      NB=NTYP0*3
      IF(NB-NP.LT.0) THEN
        WRITE(LFNPRT,11) NP,NTYP0
11      FORMAT(/,' *** SR HLMTRA: TOO MANY PARAMETERS TO ESTIMATE',/,
     1                       16X,'NUMBER OF PARAMETERS:',I4,/,
     2                       16X,'NUMBER OF STATIONS  :',I4,/)
        RETURN
      END IF
C
C ENABLE NULL TRANSFORMATION
C --------------------------
      IF (NP.EQ.0) THEN
        DO I=1,NSTAT
          DO K=1,3
            VRES(K,I)=YSTAT(K,I)-XSTAT(K,I)
          ENDDO
        ENDDO
        RMS=0.D0
        NPACT=0
        DO I=1,7
          Z0(I)=0.D0
          SIGMA(I)=0.D0
        ENDDO
        RETURN
      ENDIF
C
C COPY X- AND Y-COORDINATES
C -------------------------
      DO 20 I=1,NSTAT
        DO 30 K=1,3
          YCOORD(K,I)=YSTAT(K,I)
          XCOORD(K,I)=XSTAT(K,I)
30      CONTINUE
20    CONTINUE
C
C BARYCENTER
C ----------
      DO 35 K=1,3
        XSCH(K)=0.D0
        YSCH(K)=0.D0
35    CONTINUE
      DO 50 I=1,NSTAT
        IF(ITYP(I).NE.0) GOTO 50
        DO 60 K=1,3
          XSCH(K)=XSCH(K)+XCOORD(K,I)
          YSCH(K)=YSCH(K)+YCOORD(K,I)
60      CONTINUE
50    CONTINUE
      DO 70 K=1,3
        XSCH(K)=XSCH(K)/NTYP0
        YSCH(K)=YSCH(K)/NTYP0
70    CONTINUE
C
C SHIFT COORDINATES INTO BARYCENTER
C ---------------------------------
      IF(IBARI.NE.0) THEN
        DO 80 I=1,NSTAT
          IF(ITYP(I).NE.2) THEN
            DO 90 K=1,3
              XCOORD(K,I)=XCOORD(K,I)-XSCH(K)
              YCOORD(K,I)=YCOORD(K,I)-YSCH(K)
90          CONTINUE
          END IF
80      CONTINUE
      END IF
C
C ROTATE COORDINATES INTO LOCAL AZIMUTH/ELEVATION SYSTEM, IF IOPT=1
C -----------------------------------------------------------------
      IF(IOPT.EQ.1) THEN
C
C TRANSFORM BARYCENTER  INTO ELL.COORD. (USER ELLIPSOID)
C ------------------------------------------------------
        CALL XYZELL(AELL,BELL,DXELL,DRELL,SCELL,YSCH,YSCHEL)
C
        CALL DDREH(3,YSCHEL(2)-PI,R3)
        CALL DDREH(2,YSCHEL(1)-0.5*PI,R2)
        CALL DMLMAM(R2,R3,R)
C
        DO 810 I=1,NSTAT
          IF(ITYP(I).NE.2) THEN
            DO 820 K=1,3
              GVEC(K)=XCOORD(K,I)-YSCH(K)
              HVEC(K)=YCOORD(K,I)-YSCH(K)
820         CONTINUE
            CALL DMLMAV(GVEC,R,GVEC)
            CALL DMLMAV(HVEC,R,HVEC)
            DO 830 K=1,3
              XCOORD(K,I)=GVEC(K)
              YCOORD(K,I)=HVEC(K)
830         CONTINUE
          END IF
810     CONTINUE
      END IF
C
C FIRST APPROXIMATIONS (DX,DY,DZ,R1,R2,R3,SCALE)
C ----------------------------------------------
      Z0(7)=1.D0
      DO 40 I=1,6
        Z0(I)=0.D0
40    CONTINUE
      ITER=0
      IEND=0
C
C ROTATION MATRICES
C -----------------
600   CALL DDREH(1,Z0(4),R1)
      CALL DDREH(2,Z0(5),R2)
      CALL DDREH(3,Z0(6),R3)
C  MAT R = R3 * R2 * R1
      CALL DMLMAM(R2,R1,HMAT)
      CALL DMLMAM(R3,HMAT,R)
C  MAT RS = (SCALE) * R
      CALL DMLMAC(R,Z0(7),RS)
C
C DERIVATIVES
C -----------
100   IF(IP(4).EQ.1) THEN
C  MAT D1 = D(R1)/D(Z0(4))
        CALL DMABL(1,Z0(4),D1)
C  MAT R3R2D1 = (SCALE) * R3 * R2 * D1
        CALL DMLMAM(R2,D1,HMAT)
        CALL DMLMAM(R3,HMAT,R3R2D1)
        CALL DMLMAC(R3R2D1,Z0(7),R3R2D1)
      END IF
C
      IF(IP(5).EQ.1) THEN
C
C  MAT D2 = D(R2)/D(Z0(5))
        CALL DMABL(2,Z0(5),D2)
C
C  MAT R3D2R1 = (SCALE) * R3 * D2 * R1
        CALL DMLMAM(D2,R1,HMAT)
        CALL DMLMAM(R3,HMAT,R3D2R1)
        CALL DMLMAC(R3D2R1,Z0(7),R3D2R1)
      END IF
C
      IF(IP(6).EQ.1) THEN
C
C  MAT D3 = D(R3)/D(Z0(6))
        CALL DMABL(3,Z0(6),D3)
C
C  MAT D3R2R1 = (SCALE) * D3 * R2 * R1
        CALL DMLMAM(R2,R1,HMAT)
        CALL DMLMAM(D3,HMAT,D3R2R1)
        CALL DMLMAC(D3R2R1,Z0(7),D3R2R1)
      END IF
C
      DO 110 I=1,49
        Q(I)=0.D0
110   CONTINUE
      DO 120 I=1,7
        B(I)=0.D0
120   CONTINUE
C
C LOOP OVER ALL STATIONS
C ----------------------
      DO 210 J=1,NSTAT
        IF(ITYP(J).EQ.0) THEN
C
C APPROX. TRANSLATION
C -------------------
          DO 220 L=1,3
            X0(L)=XCOORD(L,J)+Z0(L)
220       CONTINUE
C
C APPROX. TRANSFORMATION
C MAT X0ROT = (SCALE) * R3 * R2 * R1 * X0
C ---------------------------------------
          CALL DMLMAV(X0,RS,X0ROT)
C
C DIFFERENCE TERRESTRIAL - APPROXIMATIVE TRANSFORMATION
C -----------------------------------------------------
          DO 290 L=1,3
            ABSGL(L)=YCOORD(L,J)-X0ROT(L)
290       CONTINUE
C
C BUILD UP A-MATRIX (3 ROWS AT A TIME)
C ------------------------------------
C
C *** ORDER OF PARAMETERS (IF PRESENT) IN SOLUTION VECTOR:
C     DX,DY,DZ,R1,R2,R3,SCALE
C     K: CURRENT ROW OF A MATRIX
C
          K=1
C
C TRANSLATION DX,DY,DZ
C --------------------
          DO 270 I=1,3
            IF(IP(I).EQ.1) THEN
              DO 280 L=1,3
                A(L,K)=RS(L,I)
280           CONTINUE
              K=K+1
            END IF
270       CONTINUE
C
C ROTATION R1
C -----------
          IF(IP(4).EQ.1) THEN
            CALL DMLMAV(X0,R3R2D1,HVEC)
            DO 260 L=1,3
              A(L,K)=HVEC(L)
260         CONTINUE
            K=K+1
          END IF
C
C ROTATION R2
C -----------
          IF(IP(5).EQ.1) THEN
            CALL DMLMAV(X0,R3D2R1,HVEC)
            DO 250 L=1,3
              A(L,K)=HVEC(L)
250         CONTINUE
            K=K+1
          END IF
C
C ROTATION R3
C -----------
          IF(IP(6).EQ.1) THEN
            CALL DMLMAV(X0,D3R2R1,HVEC)
            DO 240 L=1,3
              A(L,K)=HVEC(L)
240         CONTINUE
            K=K+1
          END IF
C
C SCALE FACTOR
C ------------
          IF(IP(7).EQ.1) THEN
            CALL DMLMAV(X0,R,HVEC)
            DO 230 L=1,3
              A(L,K)=HVEC(L)
230         CONTINUE
            K=K+1
          END IF
C
          DO 310 L=1,3
C
C NORM. EQ.
C ---------
            DO 320 K=1,NP
               DO 330 I=1,NP
                 IK=(K-1)*NP+I
                 Q(IK)=Q(IK)+A(L,I)*A(L,K)
330            CONTINUE
C
C RIGHT HAND SIDE
               B(K)=B(K)+A(L,K)*ABSGL(L)
320         CONTINUE
310       CONTINUE
C
C END OF STATION LOOP
C -------------------
        END IF
210   CONTINUE
C
C SOLUTION VECTOR MAT Z = INV(Q) * B
C ----------------------------------
      CALL DMINV(Q,NP,DET,L0,M0)
      DO 370 I=1,NP
        Z(I)=0.D0
        DO 375 K=1,NP
          IK=(K-1)*NP+I
          Z(I)=Z(I)+Q(IK)*B(K)
375     CONTINUE
370   CONTINUE
C
C ADD CORRECTIONS TO APPROXIMATIVE VALUES
C ---------------------------------------
      K=1
      DO 410 I=1,7
        IF(IP(I).EQ.1) THEN
          Z0(I)=Z0(I)+Z(K)
          K=K+1
        END IF
410   CONTINUE
C
C NEW ROTATION MATRICES
C ---------------------
      CALL DDREH(1,Z0(4),R1)
      CALL DDREH(2,Z0(5),R2)
      CALL DDREH(3,Z0(6),R3)
C
C  MAT R = R3 * R2 * R1, MAT RS = (SCALE) * R
      CALL DMLMAM(R2,R1,HMAT)
      CALL DMLMAM(R3,HMAT,R)
      CALL DMLMAC(R,Z0(7),RS)
C
C COMPUTE NEW TRANSFORMED COORDINATES, DIFFERENCE TO TERR. SOLUTION
C -----------------------------------------------------------------
700   VV=0.D0
C
      DO 510 I=1,NSTAT
        IF(ITYP(I).NE.2) THEN
C
          DO 520 L=1,3
            X0(L)=XCOORD(L,I)+Z0(L)
520       CONTINUE
          CALL DMLMAV(X0,RS,X0ROT)
          DO 530 L=1,3
            V=YCOORD(L,I)-X0ROT(L)
            VRES(L,I)=V
            IF(ITYP(I).EQ.0) VV=VV+V*V
530       CONTINUE
        END IF
C
510   CONTINUE
C
      IF(IBARI.EQ.1) THEN
        NPACT=NP+3
      ELSE
        NPACT=NP
      END IF
      IF(NB-NPACT.GT.0) THEN
        RMS=DSQRT(VV/(NB-NPACT))
      ELSE
        RMS=0.D0
      END IF
C
C  SAVE IN LAST ITERATION ONLY
      IF(IEND.EQ.0) GOTO 620
      K=1
      DO 540 I=1,3
      IF(IP(I).EQ.1) THEN
        SIGMA(I)=RMS*DSQRT(Q((K-1)*NP+K))
        K=K+1
      END IF
540   CONTINUE
      DO 550 I=1,3
      IF(IP(I+3).EQ.1) THEN
        Z0(I+3)=Z0(I+3)*ars
        SIGMA(I+3)=RMS*DSQRT(Q((K-1)*NP+K))*ars
        K=K+1
      END IF
550   CONTINUE
      IF(IP(7).EQ.1) THEN
        Z0(7)=(Z0(7)-1.D0)*1.D6
        SIGMA(7)=RMS*DSQRT(Q((K-1)*NP+K))*1.D6
        K=K+1
      ELSE
        Z0(7)=0.D0
      END IF
C
C ANOTHER ITERATION NECESSARY?
C ----------------------------
620   IF(IEND.EQ.0) THEN
        ITER=ITER+1
        IF (ITER.GE.10) THEN
          IEND=1
          GOTO 700
        ENDIF
        DO 610 K=1,NP
          IF(Q((K-1)*NP+K).LT.0.D0) GOTO 999
          IF(DABS(Z(K)).GT.RMS*DSQRT(Q((K-1)*NP+K))) GOTO 600
610     CONTINUE
        IEND=1
        GOTO 700
      END IF
C
999   RETURN
      END SUBROUTINE

      END MODULE
