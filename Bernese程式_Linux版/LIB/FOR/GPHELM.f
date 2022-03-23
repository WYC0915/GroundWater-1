      MODULE s_GPHELM
      CONTAINS

C*
      SUBROUTINE GPHELM(MAXSTA,TITLES,IPART,IOPT,STNAME,XSTAT,NFIX,XXX,
     1                  NUST,LISTUS,STANUM,IP,AELL,BELL,DXELL,
     2                  DRELL,SCELL)
CC
CC NAME       :  GPHELM
CC
CC PURPOSE    :  COMPUTE TRANSFORMATION OF GPS-SOLUTION COORDINATES
CC               TO TERRESTRIAL SOLUTION (APPROX.COORDINATES)
CC
CC PARAMETERS :
CC         IN :  MAXSTA : MAXIMUM NUMBER OF STATIONS          I*4
CC               TITLES(I),I=1,2: TITLE LINES                 CH*132
CC               IOPT   : OPTION                              I*4
CC                   =1 : USE LOCAL SYSTEM ( NORTH, EAST, UP)
CC                   =2 : USE EQUATORIAL SYSTEM
CC               STNAME : STATION-NAMES                       CH*16(1)
CC               XSTAT  : COORDINATE LIST OF ALL STATIONS     R*8(3,1)
CC               NFIX   : NUMBER OF FIXED STATIONS            I*4
CC               XXX    : SOLUTION VECTOR                     R*8(1)
CC               NUST   : NUMBER OF UNKNOWN STATIONS          I*4
CC               LISTUS : LIST OF ALL STATIONS                I*4(2,1)
CC                        LISTUS(1,I): LOCATION IN XSTAT
CC                        LISTUS(2,I): LOCATION OF X-COORD IN XXX
CC                        (LISTUS(2,I) OF FIXED STAT. = 0)
CC               STANUM : EXTERNAL STATION NUMBERS            I*4(1)
CC               IP     : VECTOR CHARACTERIZING TRANSFORM.    I*4(7)
CC                        PARAMETERS TO BE USED IN THE TRANSFORMATION
CC                        IP(1)=1: TRANSLATION  IN X DIRECTION
CC                        IP(2)=1:                 Y
CC                        IP(3)=1:                 Z
CC                        IP(4)=1: ROTATION AROUND X AXIS
CC                        IP(5)=1:                 Y
CC                        IP(6)=1:                 Z
CC                        IP(7)=1: SCALE FACTOR
CC                        IF IP(1),IP(2),IP(3)=0: TRANSFORM GPS COORD
CC                        INTO BARICENTER OF TERR. COORD.
CC               AELL,BELL: SEMI-MAJOR/MINOR AXIS OF ELLIPS.  R*8
CC               DXELL(I),I=1,2,3: SHIFTS TO WGS-84           R*8
CC               DRELL(I),I=1,2,3: ROTATIONS TO WGS-84        R*8
CC               SCELL(I),I=1,2,3: SCALE FACTOR TO WGS-84     R*8
CC
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  W.GURTNER, M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/02 12:01
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC               18-SEP-95 : JJ: INCREASE MAXSTA TO 200
CC               22-DEC-01 : HU: H EDIT DESCRIPTOR REMOVED
CC               05-MAR-03 : CU: REMOVE USE OF SKELETON FILE, USE IPART
CC               28-JUN-04 : RD: USE MAXSTA FROM P_GPSEST
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               15-AUG-05 : RD: MAXSTA IS AN INPUT PARAMETER
CC               28-FEB-07 : AG: USE PI AND 206264... FROM DEFCON
CC               14-FEB-11 : RD: REMOVE MAXSTA-COMMON (NOT NEEDED)
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE d_const, ONLY: pi, ars
C
      USE s_dminv
      USE s_dmlmac
      USE s_dmabl
      USE s_dmlmam
      USE s_radgms
      USE s_ddreh
      USE s_dmlmav
      USE s_xyzell
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IBARI , IG    , IK    , IM    , IOPT  ,
     1          IS    , IX    , J     , JJJ   , K     , L     ,
     2          MAXNR , NB    , NFIX  , NP    , NPACT , NS    ,
     3          NUST  , MAXSTA
C
      REAL*8    AELL  , BELL  , DET   , RMS   , SCALE , SCELL ,
     1          SEC   , SIGMA , V     , VV
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*132 TITLES(2)
      CHARACTER*16  STNAME(*)
      CHARACTER*1   V1
      CHARACTER*1   XYZ(3)
C
      INTEGER*4     LISTUS(2,*),IP(7),L0(7),M0(7)
      INTEGER*4     NUMSTA(MAXSTA),INDSTA(MAXSTA)
      INTEGER*4     STANUM(*),IPART
C
      REAL*8    DXELL(3),DRELL(3)
      REAL*8    XSTAT(3,*),XXX(*),XSCHG(3),YSCHT(3),YSCHEL(3)
      REAL*8    XGPS(3,MAXSTA),YTERR(3,MAXSTA)
      REAL*8    R1(3,3),R2(3,3),R3(3,3),R(3,3),RS(3,3)
      REAL*8    D3R2R1(3,3),R3D2R1(3,3),R3R2D1(3,3),X0ROT(3),X0(3)
      REAL*8    HVEC(3),HMAT(3,3),D1(3,3),D2(3,3),D3(3,3)
      REAL*8    A(3,7),B(7),Z(7),Z0(7),ABSGL(3),Q(49),GVEC(3)
      REAL*8    VRES(3)
C
      DO 16 I=1,MAXSTA
        INDSTA(I)=0
16    CONTINUE
C      PI=4*DATAN(1.D0)
      XYZ(1)='X'
      XYZ(2)='Y'
      XYZ(3)='Z'
C
C  SHIFT INTO BARICENTER?
      IBARI=1
      DO 15 I=1,3
        IF(IP(I).NE.0) IBARI=0
15    CONTINUE
C
C  NUMBER OF PARAMETERS:
      NP=0
      DO 10 I=1,7
        IF(IP(I).EQ.1) NP=NP+1
10    CONTINUE
      IF(NP.EQ.0) RETURN
C
C  TOTAL NUMBER OF STATIONS:
      NS=NUST+NFIX
C
C  NUMBER OF "OBSERVATIONS"
      NB=NS*3
      IF(NB-NP-3.LT.0) THEN
        WRITE(LFNERR,11) NP,NS
11      FORMAT(/,' *** SR GPHELM: TOO MANY PARAMETERS',/,
     1                       16X,'PARAMETERS:',I3,/,
     2                       16X,'STATIONS  :',I3,/)
        RETURN
      END IF
C
C  REORGANIZE AND COPY GPS- AND TERRESTRIAL COORDINATES
      MAXNR=0
      DO 20 I=1,NS
C  INDEX OF STATION COORDINATE
        IS=LISTUS(1,I)
        NUMSTA(I)=IS
C  INDEX TO VECTOR NUMSTA
        INDSTA(IS)=I
        IF(IS.GT.MAXNR) MAXNR=IS
C  INDEX OF UNKNOWN IN GPS SOLUTION (0 IF HELD FIXED)
        IX=LISTUS(2,I)
        DO 30 K=1,3
          YTERR(K,I)=XSTAT(K,IS)
          XGPS(K,I) =XSTAT(K,IS)
          IF(IX.NE.0) XGPS(K,I)=XGPS(K,I)+XXX(IX+K-1)
30      CONTINUE
20    CONTINUE
C
C  BARYCENTER
C
      DO 35 K=1,3
        XSCHG(K)=0.D0
        YSCHT(K)=0.D0
35    CONTINUE
      DO 50 I=1,NS
        DO 60 K=1,3
          XSCHG(K)=XSCHG(K)+XGPS(K,I)
          YSCHT(K)=YSCHT(K)+YTERR(K,I)
60      CONTINUE
50    CONTINUE
      DO 70 K=1,3
        XSCHG(K)=XSCHG(K)/NS
        YSCHT(K)=YSCHT(K)/NS
70    CONTINUE
C
C  SHIFT COORDINATES INTO BARYCENTER
      IF(IBARI.NE.0) THEN
        DO 80 I=1,NS
          DO 90 K=1,3
            XGPS(K,I)=XGPS(K,I)-XSCHG(K)
            YTERR(K,I)=YTERR(K,I)-YSCHT(K)
90        CONTINUE
80      CONTINUE
      END IF
C
C  ROTATE COORDINATES INTO LOCAL AZIMUTH/ELEVATION SYSTEM, IF IOPT=1
      IF(IOPT.EQ.1) THEN
C
C  TRANSFORM BARYCENTER  INTO ELL.COORD. (USER ELLIPSOID)
        CALL XYZELL(AELL,BELL,DXELL,DRELL,SCELL,YSCHT,YSCHEL)
C
        CALL DDREH(3,YSCHEL(2)-PI,R3)
        CALL DDREH(2,YSCHEL(1)-0.5*PI,R2)
        CALL DMLMAM(R2,R3,R)
C
        DO 810 I=1,NS
          DO 820 K=1,3
            GVEC(K)=XGPS(K,I)
            HVEC(K)=YTERR(K,I)
820       CONTINUE
          CALL DMLMAV(GVEC,R,GVEC)
          CALL DMLMAV(HVEC,R,HVEC)
          DO 830 K=1,3
            XGPS(K,I)=GVEC(K)
            YTERR(K,I)=HVEC(K)
830       CONTINUE
810     CONTINUE
      END IF
C
C  FIRST APPROXIMATIONS (DX,DY,DZ,R1,R2,R3,SCALE)
      Z0(7)=1.D0
      DO 40 I=1,6
        Z0(I)=0.D0
40    CONTINUE
C
C  ROTATION MATRICES
C
      CALL DDREH(1,Z0(4),R1)
      CALL DDREH(2,Z0(5),R2)
      CALL DDREH(3,Z0(6),R3)
C  MAT R = R3 * R2 * R1
      CALL DMLMAM(R2,R1,HMAT)
      CALL DMLMAM(R3,HMAT,R)
C  MAT RS = (SCALE) * R
      CALL DMLMAC(R,Z0(7),RS)
C
C  DERIVATIVES
C
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
C  MAT D2 = D(R2)/D(Z0(5))
        CALL DMABL(2,Z0(5),D2)
C  MAT R3D2R1 = (SCALE) * R3 * D2 * R1
        CALL DMLMAM(D2,R1,HMAT)
        CALL DMLMAM(R3,HMAT,R3D2R1)
        CALL DMLMAC(R3D2R1,Z0(7),R3D2R1)
      END IF
C
      IF(IP(6).EQ.1) THEN
C  MAT D3 = D(R3)/D(Z0(6))
        CALL DMABL(3,Z0(6),D3)
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
C  LOOP OVER ALL STATIONS
C
      DO 210 J=1,NS
C
C  APPROX. TRANSLATION
        DO 220 L=1,3
          X0(L)=XGPS(L,J)+Z0(L)
220     CONTINUE
C
C  APPROX. TRANSFORMATION
C  MAT X0ROT = (SCALE) * R3 * R2 * R1 * X0
        CALL DMLMAV(X0,RS,X0ROT)
C
C  DIFFERENCE TERRESTRIAL - APPROXIMATIVE TRANSFORMATION
        DO 290 L=1,3
          ABSGL(L)=YTERR(L,J)-X0ROT(L)
290     CONTINUE
C
C  BUILD UP A-MATRIX (3 ROWS AT A TIME)
C
C  *** ORDER OF PARAMETERS (IF PRESENT) IN SOLUTION VECTOR:
C      DX,DY,DZ,R1,R2,R3,SCALE
C      K: CURRENT ROW OF A MATRIX
C
        K=1
C
C  TRANSLATION DX,DY,DZ
        DO 270 I=1,3
          IF(IP(I).EQ.1) THEN
            DO 280 L=1,3
              A(L,K)=RS(L,I)
280         CONTINUE
            K=K+1
          END IF
270     CONTINUE
C
C  ROTATION R1
        IF(IP(4).EQ.1) THEN
          CALL DMLMAV(X0,R3R2D1,HVEC)
          DO 260 L=1,3
            A(L,K)=HVEC(L)
260       CONTINUE
          K=K+1
        END IF
C
C  ROTATION R2
        IF(IP(5).EQ.1) THEN
          CALL DMLMAV(X0,R3D2R1,HVEC)
          DO 250 L=1,3
            A(L,K)=HVEC(L)
250       CONTINUE
          K=K+1
        END IF
C
C  ROTATION R3
        IF(IP(6).EQ.1) THEN
          CALL DMLMAV(X0,D3R2R1,HVEC)
          DO 240 L=1,3
            A(L,K)=HVEC(L)
240       CONTINUE
          K=K+1
        END IF
C
C  SCALE FACTOR
        IF(IP(7).EQ.1) THEN
          CALL DMLMAV(X0,R,HVEC)
          DO 230 L=1,3
            A(L,K)=HVEC(L)
230       CONTINUE
          K=K+1
        END IF
C
        DO 310 L=1,3
C
C  NORM. EQ.
          DO 320 K=1,NP
             DO 330 I=1,NP
               IK=(K-1)*NP+I
               Q(IK)=Q(IK)+A(L,I)*A(L,K)
330          CONTINUE
C
C  RIGHT HAND SIDE
             B(K)=B(K)+A(L,K)*ABSGL(L)
320       CONTINUE
310     CONTINUE
C
C  END OF STATION LOOP
210   CONTINUE
C
C
C  SOLUTION VECTOR MAT Z = INV(Q) * B
      CALL DMINV(Q,NP,DET,L0,M0)
      DO 370 I=1,NP
        Z(I)=0.D0
        DO 370 K=1,NP
          IK=(K-1)*NP+I
          Z(I)=Z(I)+Q(IK)*B(K)
370   CONTINUE
C
C  ADD CORRECTIONS TO APPROXIMATIVE VALUES
      K=1
      DO 410 I=1,7
        IF(IP(I).EQ.1) THEN
          Z0(I)=Z0(I)+Z(K)
          K=K+1
        END IF
410   CONTINUE
C
C  NEW ROTATION MATRICES
      CALL DDREH(1,Z0(4),R1)
      CALL DDREH(2,Z0(5),R2)
      CALL DDREH(3,Z0(6),R3)
C  MAT R = R3 * R2 * R1, MAT RS = (SCALE) * R
      CALL DMLMAM(R2,R1,HMAT)
      CALL DMLMAM(R3,HMAT,R)
      CALL DMLMAC(R,Z0(7),RS)
C
C WRITE TITLES
C ------------
      WRITE(LFNPRT,451) TITLES
451   FORMAT(//,A132,/,A132,/,' ',131('-'),//)
C
      WRITE(LFNPRT,"(
     1     ' HELMERT TRANSFORMATION (PART ',I1,'):'
     2  ,/,' -------------------------------'
     3  ,/,' (GPS-SOLUTION INTO A PRIORI COORDINATES)'
     4  ,/,1X)") IPART
C
      IF(IOPT.EQ.1) WRITE(LFNPRT,"(
     1  ' LOCAL COORDINATE SYSTEM (NORTH,EAST,UP)')")
      IF(IOPT.EQ.2) WRITE(LFNPRT,"(
     1  ' GLOBAL COORDINATE SYSTEM (X,Y,Z)')")
C
C WRITE RESIDUAL TITLE
C --------------------
      WRITE(LFNPRT,453)
453   FORMAT(//,1X,59('-'),/,
     1  ' | NUM        NAME                RESIDUALS IN METERS      |',
     2        /,1X,59('-'))
      WRITE(LFNPRT,4539)
4539  FORMAT(1X,'|',5X,'|',18X,'|',32X,'|')
C
C  COMPUTE NEW TRANSFORMED COORDINATES, DIFFERENCE TO TERR. SOLUTION
      VV=0.D0
C
      DO 510 I=1,MAXNR
        J=INDSTA(I)
        IF(J.EQ.0) GOTO 510
C
        DO 520 L=1,3
          X0(L)=XGPS(L,J)+Z0(L)
520     CONTINUE
        CALL DMLMAV(X0,RS,X0ROT)
        DO 530 L=1,3
          V=YTERR(L,J)-X0ROT(L)
          VRES(L)=V
          VV=VV+V*V
530     CONTINUE
        JJJ=NUMSTA(J)
        WRITE(LFNPRT,531) STANUM(NUMSTA(J)),STNAME(JJJ),(VRES(L),L=1,3)
531     FORMAT(' | ',I3,' | ',A16,' | ',3F10.4,' |')
C
510   CONTINUE
      WRITE(LFNPRT,4539)
      WRITE(LFNPRT,5311)
5311  FORMAT(1X,59('-'))
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
C WRITE NUMBER OF PARAMETERS,...
      WRITE(LFNPRT,533) NPACT,NB,RMS
533   FORMAT(//,' NUMBER OF PARAMETERS        :',10X,I6,
     1        /,' NUMBER OF COORDINATES       :',10X,I6,
     2        /,' RMS OF TRANSFORMATION       :',F16.4,'  M',/)
C
C TRANSFORMATIONS
      K=1
      DO 540 I=1,3
      IF(IP(I).EQ.1) THEN
        SIGMA=RMS*DSQRT(Q((K-1)*NP+K))
        WRITE(LFNPRT,541) XYZ(I),Z0(I),SIGMA
541     FORMAT(' TRANSLATION IN ',A1,'-COORDINATE :',
     1         F16.4,'  +-',F7.4,'  M')
        K=K+1
      END IF
540   CONTINUE
      IF(K.NE.1) WRITE(LFNPRT,542)
542     FORMAT(' ')
C
C ROTATIONS
      DO 550 I=1,3
      IF(IP(I+3).EQ.1) THEN
        SIGMA=RMS*DSQRT(Q((K-1)*NP+K))*ars
        CALL RADGMS(1,Z0(I+3),V1,IG,IM,SEC)
        WRITE(LFNPRT,551) XYZ(I),V1,IG,IM,SEC,SIGMA
551     FORMAT(' ROTATION AROUND ',A1,'-AXIS      :',
     1         2X,A1,2I3,F7.3,'  +-',F7.3,'  "')
        K=K+1
      END IF
550   CONTINUE
      IF(K.GT.4) WRITE(LFNPRT,542)
C
C SCALE FACTOR
      IF(IP(7).EQ.1) THEN
        SCALE=(Z0(7)-1.D0)*1.D6
        SIGMA=RMS*DSQRT(Q((K-1)*NP+K))*1.D6
        WRITE(LFNPRT,552) SCALE,SIGMA
552     FORMAT(' SCALE FACTOR',16X,':',F16.3,'  +-',F7.3,'  MM/KM')
        K=K+1
      END IF
C
      RETURN
      END SUBROUTINE

      END MODULE
