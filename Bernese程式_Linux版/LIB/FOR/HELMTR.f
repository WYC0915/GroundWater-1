      MODULE s_HELMTR
      CONTAINS

C*
      SUBROUTINE HELMTR(TITLE1,TITLE2,NSTAT,STNAME,STANUM,STAFLG,
     1                  XSTAT,YSTAT,VEL,ITYP,IP,IOPT,IUNIT,
     2                  DATUM,AELL,BELL,DXELL,DRELL,SCELL,
     3                  LFNUM,PRTLEV,IOUTL,RESMX,PAR,RMSPAR,RMSHLM,IDOF,
     4                  RMSS)
CC
CC NAME       :  HELMTR
CC
CC PURPOSE    :  COMPUTE TRANSFORMATION PARAMETERS BETWEEN 2 COORD.
CC               SYSTEMS (XSTAT TO YSTAT)
CC
CC PARAMETERS :
CC         IN :  TITLE1 : TITLE OF COORD. FILE 1              CH*80
CC               TITLE2 : TITLE OF COORD. FILE 2              CH*80
CC               NSTAT  : TOTAL NUMBER OF SITES               I*4
CC               STNAME(I),I=1,..,NSTAT: STATION NAMES        CH*16
CC               STANUM(I),I=1,..,NSTAT: STATION NUMBERS      I*4
CC               STAFLG(2,I),I=1,..,NSTAT: STATION FLAGS IN   I*4
CC                         FILE 1 AND 2
CC               XSTAT(3,I),I=1,..,NSTAT: STATION COORDINATES R*8
CC                         IN FILE 1
CC               YSTAT(3,I),I=1,..,NSTAT: STATION COORDINATES R*8
CC                         IN FILE 2
CC               VEL(3,I),I=1,..,NSTAT: VELOCITY FOR IOPT=3   R*8
CC     IN/OUT :  ITYP(I),I=1,..,NSTAT: TYPE OF PROCESSING FOR I*4
CC                         STATION I:
CC                         = 0: USE SITE
CC                         = 1: COMPUTE RESIDUALS ONLY
CC                         = 2: DO NOT USE AT ALL
CC                         =-1: OUTLIER (OUTPUT FROM SR)
CC         IN :  IP(7)  : VECTOR CHARACTERIZING TRANSFORMATION I*4
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
CC                        =3: USE RSW
CC               IUNIT  : =1: WRITE RESULTS IN METERS         I*4
CC                        =2: WRITE RESULTS IN MM
CC               DATUM  : LOCAL GEODETIC DATUM                CH*16
CC               AELL   : SEMI-MAJOR AXIS OF ELLIPSOID        R*8
CC               BELL   : SEMI-MINOR AXIS OF ELLIPSOID        R*8
CC               DXELL(I),I=1,2,3: SHIFTS TO WGS-84 (M)       R*8
CC               DRELL(I),I=1,2,3: ROTATIONAL TO WGS-84       R*8
CC               SCELL  : SCALE FACTOR BETWEEN LOCAL GEODETIC R*8
CC                        DATUM AND WGS-84
CC               PRTLEV : PRINTING LEVEL                      R*8
CC                        =0 : NOTHING PRINTED
CC                        =1 : ONLY SUMMARY
CC                        =2 : SUMMARY PLUS RESIDUALS
CC               IOUTL  : OUTLIER REJECTION                   I*4
CC                        =1 : YES
CC               RESMX  : MAXIMUM RESIDUUM IN N,E,U (METER)   R*8(3)
CC        OUT :  PAR    : PARAMETER OF TRANSFORMATION         R*8(*)
CC               RMSPAR : RMS OF PARAMETERS                   R*8(*)
CC               RMSHLM : RMS OF COORDINATE                   R*8
CC               IDOF   : DEGREE OF FREEDOM                   I*4
CC               RMSS   : RMS PER COMPONENT (METER)           R*8(3)
CC
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  W.GURTNER, M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  88/02/03 17:22
CC
CC CHANGES    :  27-SEP-91 : ??: PRINT TRANS.PARAMETER WITH 3 DIGITS
CC               17-JUL-92 : ??: UNITS IN METERS OR MILLIMETERS
CC               09-FEB-94 : ??: ONE DIGIT MORE IN ROTATION ANGLES
CC               10-AUG-94 : MR: CALL EXITRC
CC               15-NOV-04 : EB: COMPONENT SPECIFIC RMS COMPUTATION
CC                3-JAN-96 : EB: RESIDUALS IN GEOCENTRIX SYSTEM ARE
CC                               ARE TRANSFORMED IN A LOCAL SYSTEM
CC                               AND NEW CALL (+Z0)
CC                9-JAN-96 : EB: RESIDUALS OF THE TRANSFORMATION
CC                               IN THE LOCAL SYSTEM ARE NO REALLY
CC                               IN N-E-U (N-W-U IN FORMER TIMES).
CC                               THAT AFFECTS ALSO THE TRANSFORMATION
CC                               PARAMETERS (DIFFERENT SIGN TO Y-ROTATION)
CC               17-MAY-96 : MR: ONE MORE DIGIT FOR SCALE
CC                5-NOV-98 : TS: ADDITIONAL PARAMETER IN CALL FOR IGSCOMBO
CC                6-OCT-01 : HU: OUTPUT FORMAT MODIFIED, 1H REMOVED
CC               14-NOV_02 : HU: AVOID ENDLESS LOOP
CC               16-MAR-03 : HU: OUTLIER REJECTION
CC               03-APR-03 : HU: DYNAMIC MEMORY ALLOCATION
CC               04-APR-03 : HU: ERROR IN OUTLIER REJECTION LOOP REMOVED
CC               14-APR-03 : SS: COSMETIC CHANGE
CC               16-APR-03 : RS: USE INTERFACE TO ALCERR
CC               20-MAY-03 : HU: NUMBER OF DIGITS INCREASED
CC               20-AUG-03 : HU: RETURN RMS ALSO FOR NP=0
CC               20-NOV-03 : HU: WRITE BARYCENTER COORDS FOR NEU
CC               19-FEB-04 : HU: RETURN DEGREE OF FREEDOM
CC               15-JUN-05 : HU: ARGUMENT HMAT OF XYZLOC
CC               21-JUN-05 : MM: COMLFNUM.INC REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               15-FEB-06 : HU: SUPPORT RSW SYSTEM FOR ORBITS
CC               15-FEB-06 : PS: NUMBER OF DIGITS INCREASED
CC               28-FEB-07 : AG: USE 206264... FROM DEFCON
CC               21-JUL_09 : DT: RMSS ADDED TO PARAMETER LIST
CC               22-FEB-11 : SS: REDEFINED TITLE1/TITLE2
CC               20-MAR-12 : HB: ADD MEAN, MIN AND MAX (MAINLY FOR ORBCMP)
CC               18-SEP-12 : RD: ALLOCATE ON THE FLY
CC               18-SEP-12 : RD: USE M_BERN WITH ONLY
CC               24-SEP-12 : RD: DELETE LISTFIL IF THERE ARE NO RESULTS
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: r8b, lfnprt, lfnloc, lfnerr,
     1                    fileNameLength
      USE d_const,  ONLY: pi,filTitle,ars
      USE s_opnfil
      USE s_xyzloc
      USE s_opnerr
      USE s_dminv
      USE s_dmlmac
      USE s_dmabl
      USE s_dmlmam
      USE s_radgms
      USE s_gtflna
      USE s_ddreh
      USE s_dmlmav
      USE s_xyzell
      USE s_resorb
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IBARI , ICHR  , IDEG1 , IDEG2 , IDOF  ,
     1          IEND  , IG    , IK    , IM    , IMAX  , IMIN1 , IMIN2 ,
     2          IOSTAT, IOUTL , IRCLST, ISTA  , ITER  , IUNIT , IUNITL,
     3          J     , K     , L     , NB    , NFIX  , NOUTL , NP    ,
     4          NPACT , NRSTA , NSTAT , NTYP0
C
      REAL*8    AELL  , BELL  , DET   , FACT  , RMS   , S4    , SCALE ,
     1          SCELL , SEC1  , SEC2  , SIGMA , V     , VMAX  , VV
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*80 TITLE1,TITLE2
      CHARACTER*16 STNAME(*),DATUM,CHRHLP,CHRHLP2,CHRHLP3,CHRHLP4
      CHARACTER*1  STAFLG(2,*),V4,SIGN1,SIGN2
      CHARACTER    XYZ(3,2)*1,ASTRIX*1,UNITS(2)*3
      CHARACTER(LEN=fileNameLength)  :: filnam
C
      INTEGER*4    IP(7),L0(7),M0(7),ITYP(*),STANUM(*)
      INTEGER*4    IOPT,PRTLEV,LFNUM
C
      REAL*8       PAR(*),RMSPAR(*),RMSHLM
      REAL*8       DXELL(3),DRELL(3)
      REAL*8       XSTAT(3,*),YSTAT(3,*),XSCH(3),YSCH(3),YSCHEL(3)
      REAL*8       R1(3,3),R2(3,3),R3(3,3),R(3,3),RS(3,3)
      REAL*8       D3R2R1(3,3),R3D2R1(3,3),R3R2D1(3,3),X0ROT(3),X0(3)
      REAL*8       HVEC(3),HMAT(3,3),D1(3,3),D2(3,3),D3(3,3)
      REAL*8       A(3,7),B(7),Z(7),Z0(7),ABSGL(3),Q(49),GVEC(3)
      REAL*8       VRES(3),VVS(3),RMSS(3),XELL(3),VHLP(3)
      REAL*8       VMEAN(3),VMIN(3),VMX(3)
      REAL*8       RH1(3,3),RH2(3,3),ZLIMIT(7),RESMX(3),VEL(3,*)
C
      REAL(r8b),   DIMENSION(3,NSTAT)  :: RESID,XCOORD,YCOORD
C
C
      DATA UNITS/'M ','MM'/
      DATA ((RH1(I,J),I=1,3),J=1,3)/1,0,0,0,-1,0,0,0,1/
      DATA ZLIMIT/1D-6,1D-6,1D-6,1D-8,1D-8,1D-8,1D-15/
C
      IF(IUNIT.EQ.2) THEN
        IUNITL=2
        FACT=1000.D0
      ELSE
        IUNITL=1
        FACT=1.D0
      END IF
      IF (IOPT.LT.1.OR.IOPT.GT.2) THEN
        ICHR=2
      ELSE
        ICHR=IOPT
      ENDIF
C
      XYZ(1:3,1)=(/'N','E','U'/)
      XYZ(1:3,2)=(/'X','Y','Z'/)
      IMAX=0
      RMS=0D0
      RMSHLM=0D0
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
      IF (NP.EQ.0) IBARI=0
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
      IDOF=NB-NP
      IF(PRTLEV.GT.0.AND.NB-NP.LT.0) THEN
        WRITE(LFNUM,11) NP,NTYP0
11      FORMAT(/,' *** SR HELMTR: TOO MANY PARAMETERS TO ESTIMATE',/,
     1                       16X,'NUMBER OF PARAMETERS:',I7,/,
     2                       16X,'NUMBER OF STATIONS  :',I7,/)
        RETURN
      END IF
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
        CALL DMLMAM(R2,R3,RH2)
C
C  TRANSFORMATION RIGHT HAND SYSTEM TO LEFT HAND SYSTEM
        CALL DMLMAM(RH1,RH2,R)
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
      PAR(1:6)=0D0
      PAR(7)  =1.D0
C
      Z0(7)=1.D0
      DO 40 I=1,6
        Z0(I)=0.D0
40    CONTINUE
      ITER=0
      IEND=0
      IF(NP.EQ.0) IEND=1
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
      IF(IP(4).EQ.1) THEN
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
      NB=0
C
C LOOP OVER ALL STATIONS
C ----------------------
      DO 210 J=1,NSTAT
        IF(ITYP(J).EQ.0) THEN
          NB=NB+3
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
      IDOF=NB-NP
C
C SOLUTION VECTOR MAT Z = INV(Q) * B
C ----------------------------------
      IF (NP.GT.0) THEN
        CALL DMINV(Q,NP,DET,L0,M0)
        DO 370 I=1,NP
          Z(I)=0.D0
          DO 375 K=1,NP
            IK=(K-1)*NP+I
            Z(I)=Z(I)+Q(IK)*B(K)
375       CONTINUE
370     CONTINUE
C
C ADD CORRECTIONS TO APPROXIMATIVE VALUES
C ---------------------------------------
        K=1
        DO 410 I=1,7
          IF(IP(I).EQ.1) THEN
            Z0(I)=Z0(I)+Z(K)
            K=K+1
          END IF
410     CONTINUE
      ENDIF
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
C WRITE IN LAST ITERATION ONLY
C ----------------------------
      IF(IEND.EQ.0) GOTO 460
700   RMSHLM=RMS
C
C OUTLIER FOUND IN PREVIOUS ITERATION
      IF (IOUTL.EQ.1.AND.IMAX.GT.0) GOTO 460
C
C TITLE LINES OF FILE 1 AND 2
      IF(PRTLEV.GT.0)WRITE(LFNUM,451) TITLE1(1:71),TITLE2(1:71)
451   FORMAT(' FILE 1: ',A71,/,' FILE 2: ',A71,/)
      IF(IOPT.EQ.1.AND.PRTLEV.GT.1) WRITE(LFNUM,4511) DATUM
4511  FORMAT(' LOCAL GEODETIC DATUM: ',A16,/,
     1       ' RESIDUALS IN LOCAL SYSTEM (NORTH, EAST, UP)'/)
      IF(IOPT.EQ.2.AND.PRTLEV.GT.1) WRITE(LFNUM,4512)
4512  FORMAT(' TRANSFORMATION IN EQUATORIAL SYSTEM (X, Y, Z): ',/,
     1       ' RESIDUALS IN LOCAL SYSTEM (NORTH, EAST, UP)'/)
C
C TABLE HEAD
      IF(PRTLEV.GT.1) THEN
        IF(IUNITL.EQ.1) THEN
          WRITE(LFNUM,453)
453       FORMAT(//,1X,69('-'),/,
     1      ' | NUM |      NAME        | FLG |       ',
     2      'RESIDUALS IN METERS ',
     3      '     |   |',/,1X,69('-'))
        ELSE
          WRITE(LFNUM,454)
454       FORMAT(//,1X,69('-'),/,
     1      ' | NUM |      NAME        | FLG |     ',
     2      'RESIDUALS IN MILLIMETERS',
     3      '   |   |',/,1X,69('-'))
        ENDIF
        WRITE(LFNUM,4539)
4539    FORMAT(1X,'|',5X,'|',18X,'|',5X,'|',32X,'|','   |')
      ENDIF
C
C COMPUTE NEW TRANSFORMED COORDINATES, DIFFERENCE TO TERR. SOLUTION
C -----------------------------------------------------------------
460   VV=0.D0
      DO K=1,3
        VVS(K)=0.D0
        VMEAN(K)=0.D0
        VMIN(K)=0.D0
        VMX(K)=0.D0
      ENDDO
      VMAX=0D0
      IMAX=0
C
      DO 510 I=1,NSTAT
        IF(ITYP(I).NE.2) THEN
C
          DO 520 L=1,3
            X0(L)=XCOORD(L,I)+Z0(L)
520       CONTINUE
          CALL DMLMAV(X0,RS,X0ROT)

          DO L=1,3
            VRES(L)=YCOORD(L,I)-X0ROT(L)
          ENDDO
C
          IF (IOPT.EQ.2) THEN
            IF (YCOORD(1,I).EQ.0.D0.AND.YCOORD(2,I).EQ.0.D0.AND.
     1          YCOORD(1,I).EQ.0.D0) THEN
              DO K=1,3
                VRES(K)=0.D0
              ENDDO
            ELSE
              CALL XYZELL(AELL,BELL,DXELL,DRELL,SCELL,YSTAT(1,I),XELL)
cc            CALL XYZELL(AELL,BELL,DXELL,DRELL,SCELL,YCOORD(1,I),XELL)
              CALL XYZLOC(XELL(1),XELL(2),XELL(3),AELL,BELL,-1,VRES,
     1                    VHLP,HMAT)
              DO L=1,3
                VRES(L)=VHLP(L)
              ENDDO
            ENDIF
          ENDIF
C
          IF (IOPT.EQ.3) THEN
            IF (YCOORD(1,I).EQ.0.D0.AND.YCOORD(2,I).EQ.0.D0.AND.
     1          YCOORD(1,I).EQ.0.D0) THEN
              DO K=1,3
                VRES(K)=0.D0
              ENDDO
            ELSE
              CALL RESORB(VRES,XSTAT(1,I),VEL(1,I),VRES)
            ENDIF
          ENDIF
C
          DO 530 L=1,3
            V=VRES(L)
            IF(ITYP(I).EQ.0) VV=VV+V*V
            IF(ITYP(I).EQ.0) VVS(L)=VVS(L)+V*V
            IF(ITYP(I).EQ.0) THEN
              VMEAN(L)=VMEAN(L)+V
              IF(V.LT.VMIN(L))VMIN(L)=V
              IF(V.GT.VMX(L))VMX(L)=V
            END IF
530       CONTINUE
          IF(ITYP(I).EQ.0) THEN
            ASTRIX=' '
          ELSEIF(ITYP(I).EQ.-1) THEN
            ASTRIX='*'
          ELSE
            ASTRIX='M'
          END IF
          IF(STANUM(I).EQ.0) THEN
            NRSTA=I
          ELSE
            NRSTA=STANUM(I)
          END IF
          IF(PRTLEV.GT.1.AND.IEND.EQ.1) THEN
            IF(IUNITL.EQ.1) THEN
              WRITE(LFNUM,531) NRSTA,STNAME(I),
     1                     (STAFLG(J,I),J=1,2),
     2                     (VRES(L)*FACT,L=1,3),ASTRIX
531           FORMAT(' | ',I3,' | ',A16,' | ',A1,1X,A1,
     1               ' | ',3F10.4,' | ',A1,' |')
            ELSE
              WRITE(LFNUM,532) NRSTA,STNAME(I),
     1                     (STAFLG(J,I),J=1,2),
     2                     (VRES(L)*FACT,L=1,3),ASTRIX
532           FORMAT(' | ',I3,' | ',A16,' | ',A1,1X,A1,
     1               ' | ',3F10.2,' | ',A1,' |')
            END IF
          END IF
C
C REJECT OUTLIERS
C ---------------
          RESID(1:3,I)=VRES(1:3)
          IF (IOUTL.EQ.1.AND.ITYP(i).EQ.0.AND.
     1        (DABS(VRES(1)).GT.RESMX(1).OR.
     2         DABS(VRES(2)).GT.RESMX(2).OR.
     3         DABS(VRES(3)).GT.RESMX(3))) THEN
            V=DSQRT(DOT_PRODUCT(VRES,VRES))
            IF (V.GT.VMAX) THEN
              VMAX=V
              IMAX=I
            ENDIF
          ENDIF
        END IF
C
510   CONTINUE
C
C OUTLIER FOUND
      IF (IOUTL.EQ.1.AND.IMAX.GT.0) THEN
        ITYP(IMAX)=-1
        ITER=1
        IEND=0
        GOTO 600
      ENDIF
      IF(PRTLEV.GT.1.AND.IEND.EQ.1) THEN
        WRITE(LFNUM,4539)
        WRITE(LFNUM,5311)
5311    FORMAT(1X,69('-'))
      END IF
C
      IF(IBARI.EQ.1) THEN
        NPACT=NP+3
      ELSE
        NPACT=NP
      END IF
      IF(NB-NPACT.GT.0) THEN
        RMS=DSQRT(VV/(NB-NPACT))
        DO K=1,3
          RMSS(K)=DSQRT(VVS(K)/(NB/3.D0-1.D0))
          VMEAN(K)=VMEAN(K)/(NB/3.D0)
       ENDDO
      ELSE
        RMS=0.D0
        DO K=1,3
          RMSS(K)=0.D0
          VMEAN(K)=0.D0
          VMIN(K)=0.D0
          VMX(K)=0.D0
        ENDDO
      END IF
C
C WRITE IN LAST ITERATION ONLY
      IF(IEND.EQ.0) GOTO 620
C
C WRITE RMS PER COMPONENT
C
      IF(PRTLEV.GT.0)THEN
        CHRHLP='RMS / COMPONENT '
        CHRHLP2='MEAN            '
        CHRHLP3='MIN             '
        CHRHLP4='MAX             '
        IF(IUNITL.EQ.1) THEN
          WRITE(LFNUM,553) CHRHLP,(RMSS(L)*FACT,L=1,3)
          WRITE(LFNUM,553) CHRHLP2,(VMEAN(L)*FACT,L=1,3)
          WRITE(LFNUM,553) CHRHLP3,(VMIN(L)*FACT,L=1,3)
          WRITE(LFNUM,553) CHRHLP4,(VMX(L)*FACT,L=1,3)
553       FORMAT(' | ',3X,' | ',A16,' | ',3X,
     1           ' | ',3F10.4,' | ',1X,' |')
        ELSE
          WRITE(LFNUM,554) CHRHLP,(RMSS(L)*FACT,L=1,3)
          WRITE(LFNUM,554) CHRHLP2,(VMEAN(L)*FACT,L=1,3)
          WRITE(LFNUM,554) CHRHLP3,(VMIN(L)*FACT,L=1,3)
          WRITE(LFNUM,554) CHRHLP4,(VMX(L)*FACT,L=1,3)
554       FORMAT(' | ',3X,' | ',A16,' | ',3X,
     1           ' | ',3F10.2,' | ',1X,' |')
        END IF
        WRITE(LFNUM,5311)
C
        IF(IUNITL.EQ.1) THEN
          WRITE(LFNUM,533) NPACT,NB,RMS*FACT,UNITS(IUNITL)
533       FORMAT(//,' NUMBER OF PARAMETERS  :',I6,
     1            /,' NUMBER OF COORDINATES :',I6,
     2            /,' RMS OF TRANSFORMATION :',F11.4,1X,A)
        ELSE
          WRITE(LFNUM,534) NPACT,NB,RMS*FACT,UNITS(IUNITL)
534       FORMAT(//,' NUMBER OF PARAMETERS  :',I6,
     1            /,' NUMBER OF COORDINATES :',I6,
     2            /,' RMS OF TRANSFORMATION :',F8.2,1X,A)
        END IF

C
C WRITE COORDINATES OF BARYCENTER
        IF (IOPT.EQ.1) THEN
          CALL RADGMS(1,YSCHEL(1),SIGN1,IDEG1,IMIN1,SEC1)
          CALL RADGMS(1,YSCHEL(2),SIGN2,IDEG2,IMIN2,SEC2)
          WRITE(LFNPRT,525) SIGN1,IDEG1,IMIN1,SEC1,
     1                      SIGN2,IDEG2,IMIN2,SEC2,YSCHEL(3)/1D3
525       FORMAT(/,' BARYCENTER COORDINATES:',
     1          //,' LATITUDE              :  ',A1,2I3,F6.2,
     2           /,' LONGITUDE             :  ',A1,2I3,F6.2,
     3           /,' HEIGHT                :  ',F14.3,' KM')
        ENDIF
C
        IF(NP.GT.0) WRITE(LFNUM,536)
536     FORMAT(//,' PARAMETERS:',/)
        IF(NP.EQ.0) RMSHLM=RMS
      ENDIF
C
      K=1
      DO 540 I=1,3
        IF(IP(I).EQ.1) THEN
          PAR(I)=Z0(I)
          RMSPAR(I)=RMS*DSQRT(Q((K-1)*NP+K))
          SIGMA=RMSPAR(I)
          IF(PRTLEV.GT.0)THEN
            IF(IUNITL.EQ.1) THEN
              WRITE(LFNUM,541)XYZ(I,ICHR),Z0(I)*FACT,SIGMA*FACT,
     1                                               UNITS(IUNITL)
541           FORMAT(' TRANSLATION IN  ',A1,'     :',F17.4,
     1                 '   +-',F8.4,2X,A)
            ELSE
              WRITE(LFNUM,542)XYZ(I,ICHR),Z0(I)*FACT,SIGMA*FACT,
     1                                               UNITS(IUNITL)
542           FORMAT(' TRANSLATION IN  ',A1,'     :',F15.2,
     1               '     +-',F6.2,4X,A)
            END IF
          ENDIF
          K=K+1
        ELSE
          PAR(I)=0D0
          RMSPAR(I)=0D0
        END IF
540   CONTINUE
      DO 550 I=1,3
        IF(IP(I+3).EQ.1) THEN
          PAR(I+3)=Z0(I+3)
          RMSPAR(I+3)=RMS*DSQRT(Q((K-1)*NP+K))
          SIGMA=RMSPAR(I+3)*ars
          CALL RADGMS(1,Z0(I+3),V4,IG,IM,S4)
          IF(PRTLEV.GT.0)WRITE(LFNUM,551) XYZ(I,ICHR),V4,IG,IM,S4,SIGMA
551       FORMAT(' ROTATION AROUND ',A1,'-AXIS:',2X,A1,2I3,F10.6,' +-',
     1             F10.6,' "')
          K=K+1
        ELSE
          PAR(I+3)=0D0
          RMSPAR(I+3)=0D0
        END IF
550   CONTINUE
      IF(IP(7).EQ.1) THEN
        PAR(7)=Z0(7)
        RMSPAR(7)=RMS*DSQRT(Q((K-1)*NP+K))
        SCALE=(Z0(7)-1.D0)*1.D6
        SIGMA=RMSPAR(7)*1.D6
        IF(PRTLEV.GT.0)WRITE(LFNUM,552) SCALE,SIGMA
552     FORMAT(' SCALE FACTOR',10X,':',F18.5,'  +-',F9.5,' MM/KM')
        K=K+1
      END IF
620   IF(IEND.EQ.0) THEN
C
C ANOTHER ITERATION NECESSARY?
C ----------------------------
        ITER=ITER+1
        DO 610 K=1,NP
          IF (Q((K-1)*NP+K).LT.0.D0) THEN
            IEND=1
            GOTO 700
          ENDIF
          IF(DABS(Z(K)).GT.RMS*DSQRT(Q((K-1)*NP+K)/3D0).AND.
     1       DABS(Z(K)).GT.ZLIMIT(K)) GOTO 600
610     CONTINUE
        IEND=1
        GOTO 700
      END IF
      IF(PRTLEV.GT.0.AND.NP.GT.0) WRITE(LFNUM,611) ITER
611   FORMAT(//,' NUMBER OF ITERATIONS  :',I6,/)
C
C LIST OF OUTLIERS
C ----------------
      IF (IOUTL.EQ.1) THEN
        NOUTL=0
        NFIX =0
        DO ISTA=1,NSTAT
          IF (ITYP(ISTA).EQ.-1) NOUTL=NOUTL+1
          IF (ITYP(ISTA).EQ. 0) NFIX =NFIX +1
        ENDDO
        CALL GTFLNA(0,'LISTFIL',FILNAM,IRCLST)
        IF (NOUTL.GT.0.AND.IRCLST .EQ. 0) THEN
          CALL OPNFIL(LFNLOC,FILNAM,'UNKNOWN','FORMATTED',
     1                  ' ',' ',IOSTAT)
          CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILNAM,'HELMTR')
          WRITE(LFNLOC,"(A,/,80('-'))") filTitle
          WRITE(LFNUM,710) NFIX,NOUTL
          IF (IRCLST .EQ. 0) WRITE(LFNLOC,711) NFIX,NOUTL
710       FORMAT( /,' ACCEPTED STATIONS     :',I6,
     1            /,' REJECTED STATIONS     :',I6,
     1          ///,' LIST OF REJECTED STATIONS',
     1            /,' -------------------------',
     1           //,' STATION',24X,'RESIDUALS (MILLIMETERS)',
     1            /,'        ',24X,' N         E         U',
     1            /,' ')
711       FORMAT( /,' Number of accepted stations :',I8,
     1            /,' Number of rejected stations :',I8,
     1          ///,' List of rejected stations',
     1            /,' -------------------------',
     1           //,' Station',24X,'Residuals (millimeters)',
     1            /,'        ',24X,' N         E         U',
     1            /,' ')
          DO ISTA=1,NSTAT
            IF (ITYP(ISTA).EQ.-1) THEN
              WRITE(LFNUM,720)STNAME(ISTA),RESID(1:3,ISTA)*1D3
              IF (IRCLST .EQ. 0)
     1          WRITE(LFNLOC,720)STNAME(ISTA),RESID(1:3,ISTA)*1D3
720           FORMAT(1X,A16,4X,4X,3(F10.2))
            ENDIF
          ENDDO
C
          WRITE(LFNUM,"(1X)")
          IF (IRCLST .EQ. 0)
     1      WRITE(LFNLOC,"(/,' RMS of accepted stations',3F10.2,/,1X)")
     1                         (RMSS(L)*1D3,L=1,3)
          IF (IRCLST .EQ. 0) CLOSE(LFNLOC)
        ELSEIF (NOUTL.EQ.0)THEN
          WRITE(LFNUM,"(//,' NO OUTLIER DETECTED',/)")
          IF (IRCLST .EQ. 0) THEN
            CALL OPNFIL(LFNLOC,FILNAM,'UNKNOWN','FORMATTED',
     1                    ' ',' ',IOSTAT)
            CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILNAM,'HELMTR')
            CLOSE(UNIT=LFNLOC,STATUS='DELETE')
          ENDIF
        ENDIF
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
