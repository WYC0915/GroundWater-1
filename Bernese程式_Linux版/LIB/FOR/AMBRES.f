      MODULE s_AMBRES
      CONTAINS

C*
      SUBROUTINE AMBRES(TITLES,IPRAMB,STNAME,NFTOT ,NUMOBS,STFIL ,
     1                  ICENTR,STRAMB,SIGAMB,AR2MOD,AR2INF,NP    ,
     2                  NPARAR,NPN   ,NPARMS,PARLST,NREF  ,LOCQ  ,
     3                  LOCLOC,X     ,N11   ,B0    ,B1    ,NOBS  ,
     4                  RMS   ,SUMABS,RMSSES,D2    ,D3    ,PARFLG,
     5                  IAMB1 ,IAMB2 ,IAMB3 ,MATCH ,NUMAMB,AMBSAT,
     6                  AMBIEP,AMBWLF,AMBIGU,AMBCLS,OBSCLS,SIGAPR,
     7                  ICARR ,MELWUB,NDIFF ,RECTYP,TIMREF)
CC
CC NAME       :  AMBRES
CC
CC PURPOSE    :  RESOLVE AMBIGUITIES TO INTEGER NUMBERS (IF POSSIBLE)
CC               USING DIFFERENT STRATEGIES (STRAMB(1))
CC
CC PARAMETERS :
CC         IN :  TITLES(I),I=1,2: TITLE LINES                 CH*132
CC               IPRAMB : PRINT ITERATIONS OF AMBIGUITY       I*4
CC                        RESOLUTION (STRATEGY 3) (YES=1)
CC               STNAME(I),I=1,2,... : STATION NAMES          CH*16(*)
CC               NFTOT  : TOTAL NUMBER OF FILES               I*4
CC               NUMOBS : NUMBER OF OBS (PER FRQ AND FILE)    I*4(*,*)
CC               STFIL(K,I),K=1,2 : INDEX OF STATIONS 1,2     I*4
CC                       OF FILE I IN ARRAY STNAME
CC               ICENTR(K),K=1,2,.. : INDEX OF CENTER         I*4(*)
CC               STRAMB : (1): AMBIGUITY RESOLUTION STRATEGY  I*4(*)
CC                             =-1: AMBIGUITY PRE-ELIMINATION
CC                             = 0: NO AMBIGUITY RESOLUTION
CC                             = 1: ROUND-TO-NEAREST-INTEGER
CC                             = 2: GENERAL SEARCH
CC                             = 3: SIGMA-DEPENDENT
CC                             = 4: QUASI-IONOSPHERE-FREE
CC                             = 5: LAMBDA
CC                        (2): AMBIGUITY PRE-ELIMINATION
CC                             =-1: ONCE PER SESSION
CC                             = 0: EVERY EPOCH
CC                             = N: EVERY N SECONDS
CC                        (3): SELECTION OF GNSS
CC                             = 0: ALL
CC                             = 1: GPS
CC                             = 2: GLONASS
CC                             = 3: GALILEO
CC                        (4): IGNORED GPS QUARTER-CYCLE BIASES
CC                             = 0: NO
CC                             = 1: YES
CC               SIGAMB : OPTIONS FOR SIGMA-DEPENDENT OR      R*8(4)
CC                        QIF AMBIGUITY RESOLUTION STRATEGY
CC                   IF STRAMB(1)=3 (SIGMA)
CC                          SIGAMB(1): AT LEAST 1 INTEGER WITHIN
CC                                     SIGAMB(1)*SIGMA ALLOWED
CC                          SIGAMB(2): MAXIMUM  SIGMA ALLOWED FOR
CC                                     THE AMBIGUITY WHICH SHOULD
CC                                     BE RESOLVED
CC                          SIGAMB(3): MINIMAL SIGMA USED IN TEST
CC                          SIGAMB(4): MAX. NUMBER OF AMBIGUITIES
CC                             TO BE SOLVED IN ONE ITERATION STEP
CC                          SIGAMB(5): GLONASS AMBIGUITY RESOLUTION
CC                                     BETWEEN DIFFERENT FREQUENCY
CC                                     CHANNELS:
CC                                     = 0: NEVER
CC                                     = 1: SAME REVEIVER TYPE
CC                                     = 2: SAME REVEIVER MODEL
CC                                     = 3: SAME REVEIVER GROUP
CC                                     =-1: ALWAYS
CC                    IF STRAMB(1)=4 (QIF)
CC                          SIGAMB(1): SEARCH WIDTH IN WIDE-LANE
CC                                     CYCLES
CC                          SIGAMB(2): MAXIMAL ALLOWED RMS ERROR
CC                                     OF NARROW-LANE AMBIGUITY
CC                                     (BET13*X1+BET23*X2) IN
CC                                     NARROW-LANE CYCLES
CC                          SIGAMB(3): MAXIMAL ALLOWED DISTANCE IN
CC                                 L1&L2 SPACE FROM GRID POINT WHICH IS
CC                                 SUPPOSED TO BE THE CORRECT SOLUTION
CC                                 (IN NARROW-LANE CYCLES)
CC                          SIGAMB(4): MAX. NUMBER OF AMBIGUITIY PAIRS
CC                             TO BE SOLVED IN ONE ITERATION STEP
CC                    IF STRAMB(1)=5 (LAMBDA)
CC                          SIGAMB(1): MAXIMUM ALLOWED RMS RATIO
CC                                     CONCERNING FIXED TO FLOAT
CC                          SIGAMB(2): MAXIMUM ALLOWED RMS OF UNIT
CC                                     WEIGHT FOR FIXED SOLUTION
CC                          SIGAMB(3): RESOLUTION MODE CONCERNING
CC                                     INVOLVED GNSS
CC                                     =1: GNSS BY GNSS
CC                                     =2: MIXED-GNSS
CC                                     =3: SEPARATE-GNSS
CC               AR2MOD : MODE OF PROCESSING FOR AMBIGUITY    I*4
CC                        RESOLUTION, STRATEGY 2
CC                        =1 : BASELINE-WISE AMBIGUITY RESOLUTION
CC                        =0 : RESOLVE ALL PRESENT AMBIGUITIES
CC               AR2INF : INFORMATION FOR A.R., STRATEGY 2    R*8
CC                        (1) : SEARCH WIDTH IN UNITS OF STD DEV
CC                        (2) : MAX ALLOWED RMS(FIXED)/RMS(FLOAT)
CC                        (3) : MIN ALLOWED
CC                              RMS(2-ND AMB)/RMS(1-ST AMB)
CC                        (4) : MINIMUM SEARCH WIDTH FOR GEO-
CC                              METRY-FREE LC (IN L1 CYCLES)
CC               NP     : TOTAL NUMBER OF PARAMETERS          I*4
CC               NPARAR : TOTAL NUMBER OF PARAMETERS          I*4
CC                        INCLUDING RESOLVED AMBIGUITIES
CC               NPN    : NUMBER OF PARAMETERS WITHOUT AMBI-  I*4
CC                        GUITIES
CC               NPARMS : NUMBER OF PARAMETERS FOR RMS COMP.  I*4
CC               PARLST(I,K), I=1,..,5,K=1,..,MAXTYP: NUMBER  I*4
CC                        OF PARAMETERS:
CC                        I=1: #PARAMETERS OF TYPE I (NPARMS)
CC                        I=2: #SET-UP
CC                        I=3: #NO-OBS
CC                        I=4: #REF. PARAMETERS
CC                        I=5: #SINGULAR
CC               NREF   : NUMBER OF REFERENCE AMBIGUITIES     I*4
CC               LOCQ(K,I),K=1,..,MAXLCQ,I=1,..,NP: PARAMETER I*4
CC                        CHARACTERIZATION ARRAY
CC               LOCLOC : AUXILIARY ARRAY                     I*4
CC               X(I),I=1,..: SOLUTION VECTOR                 R*8
CC               N11:    PARTS OF THE ANOR-MATRIX             R*8
CC               B0,B1 : AUXILIARY MATRICES                   R*8
CC               NOBS   : NUMBER OF DOUBLE DIFFERENCE OBSERV. I*4
CC               RMS    : RMS-ERROR                           R*8
CC               SUMABS : WEIGHTED SUM OF TERMS (O-C)**2      R*8
CC               RMSSES(I),I=1,..,NFTOT: SESSION SPECIFIC RMS R*8
CC               D2(I),I=1,..: RIGHT HAND SIDE OF TOTAL       R*8
CC                        NORMAL EQUATION SYSTEM
CC               D3(I),I=1,..: NORMAL EQUATION MATRIX A       R*8
CC                        (UPPER TRIANGLE ONLY)
CC               HP1,HP2: AUXILIARY ARRAYS                    R*8
CC               LSM    : AUXILIARY ARRAY                     CH*1
CC               PARFLG(K),K=1,..NPAR: FLAG FOR SINGULAR PAR. I*4
CC                        =0 : PARAMETER NOT SINGULAR
CC                        =1 : PARAMETER SINGULAR
CC               IAMB1,IAMB2,IAMB3: AUXILIARY ARRAYS FOR      I*4
CC                        AMBIGUITIES
CC               MATCH(I),I=,..,NPAR: ARRAY TO SAVE L2 AMBIG. I*4
CC                        INDEX BELONGING TO L1 AMBIG. INDEX
CC               NUMAMB(I),I=1,..,NFTOT: NUMBER OF AMBIGU.    I*4
CC               AMBSAT(J,I),J=1,..,NUMAMB(I),I=1,..,NFTOT:   I*4
CC                        AMBIGUITY SATELLITE NUMBERS
CC               AMBIEP(J,I),J=1,..,NUMAMB(I),I=1,..,NFTOT:   I*4
CC                        STARTING EPOCH NRS FOR AMBIGUITIES
CC               AMBWLF(L,K,I),L=1,..,NUMAMB(I), K=1,2 ,      I*4
CC                        I=1,..,NFTOT: WAVELENGTH FACTORS
CC               AMBIGU(L,K,I),L=1,..,NUMAMB(I), K=1,2,3,     R*8
CC                        I=1,..,NFTOT: AMBIGUITIES
CC               AMBCLS(L,K,I),L=1,..,NUMAMB(I), K=1,2,3,     I*4
CC                        I=1,..,NFTOT: AMBIGUITY CLUSTERS
CC               OBSCLS(I),I=1,..,NAMB: CLUSTER NUMBER OF     I*4
CC                        AMBIGUITY PARAMETER
CC               SIGAPR : A PRIORI SIGMA OF OBSERVATIONS      R*8
CC               ICARR(K,IFIL), K=1..NFRFIL(I),I=1,..NFTOT    I*4
CC                        FREQUENCIES TO BE PROCESSED FOR
CC                        FILE I
CC               MELWUB : MELBOURNE-WUEBBENA LC               I*4
CC                        =0: NO
CC                        =1: YES
CC                        =2: DTEC LC
CC               NDIFF(I):I=1,..,NFTOT: DIFFERENCE TYPE       I*4
CC                        NDIFF=0: ZERO DIFFERENCE
CC                        NDIFF=1: SINGLE DIFFERENCE
CC               RECTYP(K,I), K=1,2, I=1,..,NFTOT: RECEIVER
CC                         TYPES                             CH*20
CC               TIMREF(I),I=... : REFERENCE TIME OF FILE     R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER, L.MERVART, S.SCHAER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/03 09:24
CC
CC CHANGES    :  27-MAY-91 : ??: DON'T PRINT TRAILING BLANKS
CC               31-JUL-92 : ??: FAST AMBIGUITY RESOLUTION
CC               05-AUG-92 : ??: ITERATIVE APROACH IN STRATEGY 3
CC                               ACCORDING TO RMS OF AMBIGUITIES
CC               10-AUG-92 : ??: NO ITERATION FOR STRATEGY 1
CC               10-NOV-92 : ??: AMB. TITLE WAS PRINTED AT THE WRONG
CC                               POSITION FOR STRATEGY 2.
CC                               PRINT RMS*DSQRT(2.D0)
CC               16-NOV-92 : ??: MXNFIL WAS NOT DECLARED
CC               18-JAN-93 : ??: ADD PARAMETERS "NREF","OBSCLS" TO PRINT
CC                               CORRECT REF. AMB. IN SR "PRIAMI"
CC               10-APR-94 : MR: BETTER AMBIGUITY PRINTING, CALL TO
CC                               ARSTR3 CHANGED ("ITRY" ADDED)
CC               13-APR-94 : SS,LM: AMBIGUITY STRATEGY 4
CC               21-APR-94 : MR: NEW PARAMETER "SIGAPR" FOR SR SIGMA1
CC               15-JUL-94 : SS: SQUARING ENABLED (QIF)
CC               25-JUL-94 : MR: PRE-ELIMINATION STATISTICS SIMPLIFIED
CC               28-JUL-94 : MR: SYMING INSTEAD OF SYMIN8, "PARFLG"
CC               10-AUG-94 : MR: CALL EXITRC
CC               29-NOV-95 : MR: STRATEGY 1 INCLUDED IN STRATEGY 3
CC               29-NOV-95 : SS: CALL SIGMA2 WITH "SIGAPR"
CC               09-OCT-97 : SS: PRINT POST-FIT SIGMA FOR ONE-WAY OBSERVABLE
CC               04-AUG-99 : MR: ADD PARAMETERS TO CALL ARSTR3 FOR GLONASS
CC               19-APR-00 : RD: NEW CALL OF SR PRIAMI
CC               23-JAN-02 : RD: REMOVE ZAMBIG
CC               09-FEB-02 : RD: WGT REAL*4->REAL*8
CC               16-SEP-02 : SS/MR: ENABLE NEQ SAVING IN CASE OF AMBRES
CC               05-MAR-03 : CU: REMOVE USE OF SKELETON FILE
CC               15-APR-03 : CU: BUG FIXED (FORMAT STATEMENTS)
CC               26-MAY-03 : RD: NEW CALL OF SR PRIAMI
CC               27-AUG-03 : HU: SHARED DO LABELS REMOVED
CC               21-OCT-04 : RD: REMOVE UNUSED ARGUMENTS: N12, N22
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               08-MAY-07 : SS: GLONASS AMBIGUITY RESOLUTION ENABLED
CC               16-OCT-08 : SL: TIMREF added, ARSTR{3,4} call changed
CC               31-OCT-08 : SS/RD: CONTROL DEVELOPMENT OF RMS FOR SIGMA
CC                               AND QIF STRATEGY
CC               12-AUG-09 : SS: RMS SCALED UP IF INDICATED
CC               07-DEC-10 : SS: ADDED NREF TO CALL OF ARSTR3/ARSTR4
CC               16-FEB-11 : SS: SIGAMB(5) FOR GLONASS AMBIGUITY RESOLUTION
CC               17-FEB-11 : SS: ISTRAT REPLACED BY STRAMB
CC               17-FEB-11 : SS: STRAMB(3) FOR SELECTION OF GNSS
CC               17-FEB-11 : SS: LAMBDA AMBIGUITY RESOLUTION STRATEGY
CC               18-FEB-11 : SS: IGNORED GPS QUARTER-CYCLE BIASES
CC               24-MAY-12 : RD: SOME MESSGES ONLY FOR "AIUB"
CC               24-MAY-12 : RD: USE M_BERN WITH ONLY, REMOVE UNUSED VARIABLES
CC               10-JUL-12 : RD: USE SYMINVG INSTEAD OF SYMING
CC               11-OCT-12 : RD: ADJUST RMSMIN FOR MELWUB OR OTHER OBSERV.
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,  ONLY: lfnprt, lfnerr
      USE f_ikf
      USE f_sigma2
      USE s_nglord
      USE s_syminvg
      USE s_solve
      USE s_arstr2
      USE s_arngbl
      USE s_exitrc
      USE s_arstr3
      USE s_arstr4
      USE s_priami
      USE f_lengt1
C
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I1    , IA1   , IAMB  , IAMBR2, ICAR  , ICLS  ,
     1          ICLSTM, ICLUST, IFIL  , IFRQ  , IND   , INDNEW,
     2          INDOK , INEW  , INPN  , IPAR  , IPRAMB, ISM   ,
     3          ISTFIL, ITRY  , K     , K1    , KAMB  , KPAR  ,
     4          MELWUB, MXCAMB, MXCFIL, MXCFRQ, MXCLCQ, NAMB  , NAMBBL,
     5          NAMTOT, NBLINE, NEWSTA, NFTOT , NOBS  , NOBSBL, NP    ,
     6          NPARAR, NPARMS, NPN   , NPNBL , NPNEW , NREF
C
      REAL*8    RMS   , SIGAPR, SUMABS, SUMRES, SUMABL, RMSTST, RMSMIN
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*132 TITLES(2)
      CHARACTER*20  RECTYP(2,*)
      CHARACTER*16  STNAME(*)
      CHARACTER*6   MXNLCQ,MXNAMB,MXNFRQ,MXNFIL
C
      REAL*8 N11(*),X(*),D2(*),D3(*),B0(*),B1(*)
      REAL*8 RMSSES(*)
      REAL*8 AMBIGU(MXCAMB,3,*),SIGAMB(*),AR2INF(*)
      REAL*8 TIMREF(*)
C
      INTEGER*4 IAMB1(*),IAMB2(*),LOCQ(MXCLCQ,*)
      INTEGER*4 NUMAMB(*),AMBSAT(MXCAMB,*)
      INTEGER*4 AMBIEP(MXCAMB,*),AMBCLS(MXCAMB,3,*)
      INTEGER*4 AMBWLF(MXCAMB,2,*)
      INTEGER*4 STRAMB(*),AR2MOD,STFIL(2,*),NUMOBS(MXCFRQ,*)
      INTEGER*4 LOCLOC(MXCLCQ,*),IAMB3(*),MATCH(*),ICENTR(*)
      INTEGER*4 OBSCLS(*)
      INTEGER*4 PARLST(5,*),PARFLG(*),ICARR(MXCAMB,*),NDIFF(*)
C
C
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
      COMMON/MCMAMB/MXCAMB,MXNAMB
      COMMON/MCMFRQ/MXCFRQ,MXNFRQ
      COMMON/MCMFIL/MXCFIL,MXNFIL
C
C PRINT TITLE LINES FOR AMBIGUITIES
C ---------------------------------
      WRITE(LFNPRT,5011) TITLES(1)(1:LENGT1(TITLES(1))),
     1                   TITLES(2)(1:LENGT1(TITLES(2)))
5011  FORMAT(//,A,/,A,/,' ',131('-'),//)
C
      WRITE(LFNPRT,"(
     1     ' AMBIGUITY RESOLUTION:'
     2  ,/,' --------------------'
     3  ,/,1X)")
C
      IF(STRAMB(1).EQ.1) WRITE(LFNPRT,"(
     1  ' STRATEGY :  AMBIGUITY RESOLUTION TO NEAREST INTEGER')")
      IF(STRAMB(1).EQ.2) WRITE(LFNPRT,"(
     1  ' STRATEGY :  AMBIGUITY RESOLUTION WITH A LIMITED SEARCH ',
     2  'ALGORITHM')")
      IF(STRAMB(1).EQ.3) WRITE(LFNPRT,"(
     1  ' STRATEGY :  SIGMA-DEPENDENT AMBIGUITY RESOLUTION')")
      IF(STRAMB(1).EQ.4) WRITE(LFNPRT,"(
     1  ' STRATEGY :  QUASI-IONOSPHERE-FREE AMBIGUITY RESOLUTION',
     2  ' (QIF)')")
      IF(STRAMB(1).EQ.5) WRITE(LFNPRT,"(
     1  ' STRATEGY :  LAMBDA AMBIGUITY RESOLUTION')")
C
C INITIALIZATION OF AMBIGUITY FLAGS
C ---------------------------------
      NAMB=NP-NPN
      DO 10 I=1,NP
        IAMB1(I)=1
        IAMB2(I)=1
10    CONTINUE
C
      SUMABL=SUMABS
C
C CHECK, IF L3,L4 RESOLVABLE (L5 INTRODUCED)
C ------------------------------------------
      IAMBR2=1
      DO 100 I=1,NAMB+NREF
        INPN=NPN+I
        ICAR=LOCQ(5,INPN)
        IF(ICAR.NE.3.AND.ICAR.NE.4) GOTO 100
        IF (LOCQ(6,INPN).LE.1) THEN
          IAMB2(INPN)=0
          IAMBR2=0
        ELSE
          IFIL=LOCQ(2,INPN)
          ICLS=LOCQ(3,INPN)
          IFRQ=LOCQ(4,INPN)
          DO 80 IA1=1,NUMAMB(IFIL)
            IF (AMBCLS(IA1,IFRQ,IFIL).EQ.ICLS) GOTO 90
80        CONTINUE
90        CONTINUE
          IAMB2(INPN)=AMBCLS(IA1,3,IFIL)
        ENDIF
100   CONTINUE
C
      NPNEW=NP
C
C AMBIGUITY RESOLUTION: STRATEGY 1
C --------------------------------
C USE MODIFIED STRATEGY 3 (SIGMA-DEPENDENT) WITH NO ITERATIONS
      IF (STRAMB(1).EQ.1) SIGAMB(4)=0.D0
C
C AMBIGUITY RESOLUTION: STRATEGY 2
C --------------------------------
      IF (STRAMB(1).EQ.2) THEN
C
C AMBIGUITY RESOLUTION NOT ALLOWED IF SOME AMBIGUITIES ARE UNRESOLVABLE
        DO 900 I=1,NAMB+NREF
          INPN=NPN+I
          ICAR=LOCQ(5,INPN)
          IF(ICAR.NE.3.AND.ICAR.NE.4) GOTO 900
          IF (IAMB2(INPN).NE.IAMB2(LOCQ(7,INPN))) IAMBR2=0
900     CONTINUE

        IF (IAMBR2.EQ.0) THEN
          WRITE(LFNERR,901)
901       FORMAT(/,' *** SR AMBRES: THE GENERAL SEARCH STRATEGY IS ',
     1             'NOT AS GENERAL AS YOU THINK:',
     2           /,16X,'THIS STRATEGY MAY NOT BE USED IF SOME',
     3             ' AMBIGUITIES ARE NOT',
     4           /,16X,'RESOLVABLE (WIDELANE NOT RESOLVED WHEN USING',
     5             ' L3 OR L4)',/)
          CALL EXITRC(2)
        ENDIF
C
        IF (AR2MOD.NE.1) THEN
C
C RESOLVE ALL AMBIGUITIES PRESENT
C -------------------------------
          IND=0
          DO 23 I=1,NP
            B0(I)=X(I)
            B1(I)=D2(I)
            DO K=1,I
              IND=IND+1
              D3(IND)=N11(IND)
            ENDDO
23        CONTINUE
          CALL ARSTR2(TITLES,AR2MOD,NPN,NAMB,NPARMS,NOBS,
     1                AR2INF,SIGAPR,D3,B1,SUMABS,LOCQ,INDOK,B0,
     2                PARFLG,RMS)
          IF(INDOK.EQ.1) THEN
C
C UPDATE NUMBER OF PARAMETERS FOR RMS COMPUTATION
            NPARMS=NPARMS-NAMB
            PARLST(1,4)=0
C
            DO 21 I=1,NAMB
              IAMB1(NPN+I)=0
21          CONTINUE
            NPNEW=NPN
            IND=0
            DO 22 I=1,NPNEW
              D2(I)=B1(I)
              DO K=1,I
                IND=IND+1
                N11(IND)=D3(IND)
              ENDDO
22          CONTINUE
            DO 24 I=1,NP
              X(I)=B0(I)
24          CONTINUE
          ELSE
            DO 25 I=1,NAMB
              IAMB1(NPN+I)=1
25          CONTINUE
            NPNEW=NP
            CALL SYMINVG(NPNEW,N11,1,ISM,PARFLG)
            IND=0
            DO 26 I=1,NPNEW
              DO K=1,I
                IND=IND+1
                D3(IND)=N11(IND)
              ENDDO
26          CONTINUE
          END IF
C
C BASELINE-WISE RESOLUTION OF AMBIGUITIES
C ---------------------------------------
        ELSE
          SUMRES=0.D0
          ISTFIL=1
          NAMTOT=0
1000      CALL ARNGBL(ISTFIL,NFTOT,STNAME,ICENTR,STFIL,NUMOBS,NP,N11,
     1                D2,X,RMSSES,LOCQ,D3,B1,B0,SUMABS,NOBSBL,NPNBL,
     2                NAMBBL,LOCLOC,NEWSTA,NBLINE)
C
          CALL ARSTR2(TITLES,AR2MOD,NPNBL,NAMBBL,NPARMS,
     1                NOBSBL,AR2INF,SIGAPR,D3,B1,SUMABS,LOCLOC,INDOK,
     2                B0,PARFLG,RMS)
          SUMRES=SUMRES+SUMABS
          IF(INDOK.EQ.0)NAMTOT=NAMTOT+NAMBBL
          DO 1030 IPAR=1,NPNBL+NAMBBL
            DO 1020 KPAR=1,NPN+NAMB
              DO 1010 K=1,MXCLCQ
                IF(LOCLOC(K,IPAR).NE.LOCQ(K,KPAR))GOTO 1020
1010          CONTINUE
              IF(LOCLOC(1,IPAR).NE.4)THEN
                IF(INDOK.EQ.1)THEN
                  D2(KPAR)=B1(IPAR)
                END IF
              ELSE
                IF(INDOK.EQ.0)THEN
                  IAMB1(KPAR)=1
                ELSE
                  IAMB1(KPAR)=0
                  X(KPAR)=B0(IPAR)
                END IF
              END IF
1020        CONTINUE
1030      CONTINUE
          IF(NEWSTA.LE.NFTOT)THEN
            ISTFIL=NEWSTA
            GO TO 1000
          END IF
C
C ALL BASELINES TREATED PROPERLY
C ------------------------------
          DO 1110 IPAR=1,NPN
            IAMB3(IPAR)=IPAR
1110      CONTINUE
          INDNEW=NPN
          DO 1200 IAMB=1,NAMB
            IF(IAMB1(NPN+IAMB).EQ.1)THEN
              INDNEW=INDNEW+1
              IAMB3(INDNEW)=NPN+IAMB
            END IF
1200      CONTINUE
          DO 1300 IAMB=1,NAMB
            IF(IAMB1(NPN+IAMB).EQ.0)THEN
              INDNEW=INDNEW+1
              IAMB3(INDNEW)=NPN+IAMB
            END IF
1300      CONTINUE
C
C RE-ARRANGE NEQ-SYSTEM
C ---------------------
          CALL NGLORD(NP,NP,IAMB3,LOCQ,N11,D2,X,IAMB1,
     1                LOCLOC,D3)
          NPNEW=NP-NAMB+NAMTOT
          NPARMS=NPARMS-NAMB+NAMTOT
          PARLST(1,4)=PARLST(1,4)-NAMB+NAMTOT
C
C SOLVE NEQ-SYSTEM WITHOUT RESOLVED AMBIGUITIES
C ---------------------------------------------
          CALL SYMINVG(NPNEW,N11,0,ISM,PARFLG)
          CALL SOLVE(NPNEW,N11,D2,X)
          IND=0
          DO 1400 IPAR=1,NPNEW
            SUMRES=SUMRES-D2(IPAR)*X(IPAR)
            DO KPAR=1,IPAR
              IND=IND+1
              D3(IND)=N11(IND)
            ENDDO
1400      CONTINUE
          RMS=DSQRT(SUMRES/(NOBS-NPARMS))
        END IF
C
      ELSE IF (STRAMB(1).EQ.1 .OR.
     1         STRAMB(1).EQ.3 .OR.
     2         STRAMB(1).EQ.4 .OR.
     3         STRAMB(1).EQ.5) THEN
C
C ITERATIVE AMBIGUITY RESOLUTION: STRATEGIES 3, 4, 5
C --------------------------------------------------
C
C INITIALIZE CLUSTERS
        ICLSTM=1
        DO 290 ICLUST=1,NAMB+NREF
          IAMB3(ICLUST)=0
290     CONTINUE
C
        IF (STRAMB(1).EQ.4) THEN
C
C FIND ALL L1 AMBIGUITIES AND CORRESPONDING L2 AMBIGUITIES
C --------------------------------------------------------
          DO 2020 IAMB=1,NAMB
            IF (LOCQ(5,NPN+IAMB).EQ.1) THEN
              DO 2010 KAMB=1,NAMB
                IF (LOCQ(5,NPN+KAMB).EQ.2 .AND.
     1              LOCQ(2,NPN+KAMB).EQ.LOCQ(2,NPN+IAMB).AND.
     2              LOCQ(3,NPN+KAMB).EQ.LOCQ(3,NPN+IAMB)) THEN
                  MATCH(IAMB)=KAMB
                END IF
2010          CONTINUE
            END IF
2020      CONTINUE
        ENDIF
C
        DO 295 ITRY=1,1000000
          RMSMIN=SIGAPR
          IF (MELWUB.EQ.1) RMSMIN=0.1D0*SIGAPR
          IF (RMS.LT.RMSMIN) THEN
#ifdef GRP_AIUB
            WRITE(LFNERR,903) ITRY,RMS,RMSMIN
903         FORMAT(/,' ### SR AMBRES: RMS OF UNIT WEIGHT SCALED UP ',
     1               'FOR TRY ',I4,
     2         /,16X,'CURRENT RMS: ',F9.5,' M',
     3         /,16X,'RMS USED:    ',F9.5,' M',/)
#endif
            RMSTST=RMSMIN
          ELSE
            RMSTST=RMS
          ENDIF
          IF (STRAMB(1).EQ.1 .OR.
     1        STRAMB(1).EQ.3) THEN
            CALL ARSTR3(STRAMB,ITRY,RMSTST,SIGAMB,NPN,NAMB,LOCQ,
     1                  NUMAMB,AMBSAT,AMBCLS,N11,D3,
     2                  D2,SUMABS,IAMB2,IAMB1,
     3                  OBSCLS,IAMB3,X,ICLSTM,INEW,
     4                  RECTYP,TIMREF,NREF)
          ELSEIF (STRAMB(1).EQ.4) THEN
            CALL ARSTR4(STRAMB,ITRY,RMSTST,SIGAMB,NPN,NAMB,LOCQ,AMBWLF,
     1                  NUMAMB,AMBSAT,AMBCLS,N11,D3,
     2                  D2,SUMABS,IAMB2,IAMB1,
     3                  OBSCLS,IAMB3,MATCH,X,ICLSTM,INEW,
     4                  RECTYP,TIMREF,NREF)
          ELSEIF (STRAMB(1).EQ.5) THEN
CC            CALL ARSTR5(STRAMB,ITRY,RMSTST,SIGAMB,NPN,NAMB,LOCQ,
CC     1                  NUMAMB,AMBSAT,AMBCLS,N11,D3,
CC     2                  D2,SUMABS,IAMB2,IAMB1,
CC     3                  OBSCLS,IAMB3,X,ICLSTM,INEW,
CC     4                  RECTYP,TIMREF,NREF)
          END IF
          IF (INEW.EQ.0) GO TO 296
C
C COMPUTE A NEW SOLUTION
          CALL SYMINVG(NP,D3,0,ISM,PARFLG)
          CALL SOLVE(NP,D3,D2,X)
C
          NPNEW=NPNEW-INEW
          NPARMS=NPARMS-INEW
          PARLST(1,4)=PARLST(1,4)-INEW
          RMS=SIGMA2(NP,NPARMS,NOBS,X,D2,SUMABS,IAMB1,NAMB,SIGAPR)
C
#ifdef GRP_AIUB
          IF (SUMABS.LT.SUMABL) THEN
            WRITE(LFNERR,902) ITRY,SUMABL,SUMABS
902         FORMAT(/,' ### SR AMBRES: PROBLEM CONCERNING RMS ',
     1               'COMPUTATION FOR ITERATION ',I4,
     2         /,16X,'SUMABL: ',E30.20,
     3         /,16X,'SUMABS: ',E30.20,/)
          ENDIF
#endif
          SUMABL=SUMABS
C
C PRINT AMBIGUITIES DURING ITERATIONS
          IF (IPRAMB.EQ.1) THEN
            WRITE(LFNPRT,1001) INEW,RMS
1001        FORMAT(/,' NUMBER OF AMBIGUITIES RESOLVED:',I6,4X,
     1               'NEW SIGMA OF OBSERVATIONS:',F9.4,' M')
            CALL PRIAMI(1,NAMB,NPN,NREF,LOCQ,X,RMS,D3,IAMB1,IAMB2,
     1                  NUMAMB,AMBSAT,AMBIEP,AMBWLF,AMBIGU,
     2                  AMBCLS,ICARR,NDIFF)
          ENDIF
C
295     CONTINUE
296     CONTINUE
C
      ENDIF
C
C PRINT FINAL AMBIGUITIES
C -----------------------
      IF (STRAMB(1).LT.3 .OR. IPRAMB.EQ.0) THEN
        CALL PRIAMI(1,NAMB,NPN,NREF,LOCQ,X,RMS,D3,IAMB1,IAMB2,
     1              NUMAMB,AMBSAT,AMBIEP,AMBWLF,AMBIGU,
     2              AMBCLS,ICARR,NDIFF)
      ENDIF
C
C SQUEEZE NORMAL EQUATION SYSTEM AND COFACTOR MATRIX
C --------------------------------------------------
      I1=0
      DO 520 I=1,NP
        IF(IAMB1(I).EQ.0) GOTO 520
        I1=I1+1
C
C KEEP FULL X VECTOR FOR AMBIGUITY SAVING
CC        X(I1)=X(I)
        B0(I1)=D2(I)
        K1=0
        DO 510 K=1,I
          IF(IAMB1(K).EQ.0) GOTO 510
          K1=K1+1
          D3(IKF(I1,K1))=D3(IKF(I,K))
          N11(IKF(I1,K1))=N11(IKF(I,K))
510     CONTINUE
520   CONTINUE
C
C UPDATED TOTAL NUMBER OF PARAMETERS
      NPARAR=NP
      NP=I1
C
      RETURN
      END SUBROUTINE

      END MODULE
