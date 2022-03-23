      MODULE s_ARSTR4
      CONTAINS

C*
      SUBROUTINE ARSTR4(STRAMB,ITER,RMS,SIGAMB,NPARN,NAMB,LOCQ,AMBWLF,
     1                  NUMAMB,AMBSAT,AMBCLS,NMAT,
     2                  QMAT,BMAT,SUMABS,IAMB2,IAMB1,
     3                  OBSCLS,CLUSTR,MATCH,X,ICLSTM,INEW,
     4                  RECTYP,TIMREF,NREF)
CC
CC NAME       :  ARSTR4
CC
CC PURPOSE    :  AMBIGUITY RESOLUTION USING THE "QUASI-IONOSPHERE-FREE"
CC               APPROACH (QIF). L1 AND L2 OBSERVATIONS HAVE TO BE
CC               PROCESSED IN THE SAME RUN, BECAUSE L1 AND L2 AMBIG.
CC               ARE RESOLVED IN PAIRS (L1,L2). AMBIGUITIES ARE SOLVED
CC               STARTING WITH THE SMALLEST RMS IN L3 AMBIGUITY.
CC               TO ELIMINATE THE IONOSPHERIC BIASES YOU HAVE TO
CC               ESTIMATE DIFF. IONOSPHERE PARAMETERS WHEN USING THIS
CC               RESOLUTION TECHNIQUE.
CC
CC PARAMETERS :
CC         IN :  STRAMB : (1): AMBIGUITY RESOLUTION STRATEGY  I*4(*)
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
CC                        (4): CONSIDERED GPS QUARTER-CYCLE BIASES
CC                             = 0: NEVER
CC                             = 1: IF INDICATED
CC                             = 2: ALWAYS
CC                             =-N: PRN NUMBER
CC               ITER   : AMB. RESOLUTION ITERATION NUMBER        I*4
CC               RMS    : RMS                                     R*8
CC               SIGAMB : OPTIONS FOR QUASI-IONOSPHERE-FREE       R*8(4)
CC                        AMBIGUITY RESOLUTION STRATEGY (QIF)
CC                          SIGAMB(1): SEARCH WIDTH IN WIDE-LANE
CC                                     CYCLES
CC                          SIGAMB(2): MAXIMAL ALLOWED RMS ERROR
CC                                     OF NARROW-LANE AMBIGUITY
CC                                     (BET13*X1+BET23*X2) IN
CC                                     NARROW-LANE CYCLES
CC                          SIGAMB(3): MAXIMAL ALLOWED DISTANCE IN
CC                            L1&L2 SPACE FROM GRID POINT WHICH IS
CC                            SUPPOSED TO BE THE CORRECT SOLUTION
CC                            (IN NARROW-LANE CYCLES)
CC                          SIGAMB(4): MAX. NUMBER OF AMBIGUITIY PAIRS
CC                            TO BE SOLVED IN ONE ITERATION STEP
CC               NPARN  : NUMBER OF NON AMB. PARAMETERS           I*4
CC               NAMB   : NUMBER OF AMBIGUITIES                   I*4
CC               LOCQ(K,I),K=1,..,MAXLCQ,I=1,..,NP: PARAMETER     I*4
CC                        CHARACTERIZATION ARRAY
CC               AMBWLF(I,J,IFIL),I=1,..,NUMAMB,J=1,2: WAVELENGTH I*4
CC                        FACTORS FOR BOTH FREQUENCIES
CC               RECTYP : RECEIVER TYPES                          CH*20(2)
CC               TIMREF(I),I=... : REFERENCE TIME OF FILE         R*8
CC               NREF   : NUMBER OF REFERENCE AMBIGUITIES         I*4
CC     IN/OUT :  NMAT(I): N.E.MATRIX BEFORE INVERSION (VECTORIZED)R*8
CC               QMAT(I): N.E.MATRIX AFTER INVERSION (VECTORIZED) R*8
CC               BMAT(I): RIGHT HAND SIGHT OF N.E. SYSTEM         R*8
CC               SUMABS : SUM OF SQUARED (OBS-COMP)               R*8
CC               IAMB2(I),I=1,...NPAR !!! FLAG FOR RESOVABLE      I*4
CC                        AMBIGUITIES  (=0: NOT RESOVABLE (E.G. L3))
CC               IAMB1(I),I=1,...NPAR !!! FLAG FOR RESOLVED       I*4
CC                        AMBIGUITIES
CC                          =0: ALREADY RESOLVED
CC                          =1: NOT YET RESOLVED
CC               OBSCLS(I),I=1,..NAMB : AMBIGUITY CLUSTERS        I*4
CC               CLUSTR(I): AUXILIARY ARRAY FOR AMB. CLUSTERS     I*4
CC               MATCH(I),I=,..,NPAR: ARRAY TO SAVE L2 AMBIGUITY  I*4
CC                        INDEX BELONGING TO L1 AMBIGUITY INDEX
CC               X(I)   : SOLUTION VECTOR                         R*8
CC               ICLUSTM: NUMBER OF NEXT CLUSTER                  I*4
CC        OUT :  INEW   : NUMBER OF AMBIGUITIES RESOLVED          I*4
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  L.MERVART, M.ROTHACHER, S.SCHAER
CC
CC VERSION    :  3.5
CC
CC CREATED    :  12-APR-94
CC
CC CHANGES    :  02-MAY-94 : LM: SEARCH WITHIN L1-L5 SPACE
CC               15-JUL-94 : SS: SQUARING ENABLED
CC               27-JUL-94 : SS: "INEW" INCREMENT 2 (INSTEAD OF 1)
CC               28-SEP-95 : JJ: DECLARE MXNLCQ AS C*6 INSTEAD OF I*4
CC               28-SEP-95 : JJ: DECLARE MXNAMB AS C*6 INSTEAD OF I*4
CC               09-MAR-04 : RD: DO NOT RESOLVE SINGLUAR AMBIGUITIES
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               08-MAY-07 : SS: GLONASS AMBIGUITY RESOLUTION ENABLED
CC               03-OCT-07 : SS: CHECK WAVELENGTH DIFFERENCE
CC               16-OCT-08 : SL: TIMREF added, BLOCK IIR-M SAT w/ timewindow
CC               07-AUG-09 : SS: MANAGE QUARTER-CYCLE BIAS ISSUE
CC               08-JUN-10 : SS: TREAT AMBIGUITIES SEPARATELY
CC               19-JUL-10 : SL: tab characters removed
CC               07-DEC-10 : SS: SINGLE-DIFFERENCE AMBIGUITIES UNRESOLVABLE
CC               17-FEB-11 : SS: STRAMB(3) FOR SELECTION OF GNSS
CC               18-FEB-11 : SS: IGNORED GPS QUARTER-CYCLE BIASES
CC               13-FEB-12 : SS: JAVAD AFFECTED BY GPS QUARTER-CYCLE BIASES
CC                               BUG CORRECTION WRT 1/4-CYCLE FOR
CC               15-FEB-12 : SS: REFINED QUARTER-CYCLE BIAS HANDLING
CC               16-FEB-12 : SS: NEGATIVE STRAMB(4) (PRN NUMBER)
CC               17-FEB-12 : SS: ADDED LIST OF QUARTER-CYCLE BIAS EXCEPTIONS
CC               29-MAR-12 : SS: SAFE HANDLING IN CASE OF QUARTER-CYCLE BIAS
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1994     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
C DECLARATIONS
C ------------
      USE m_bern
      USE d_const, ONLY: FREQ
      USE f_ikf
      USE s_satblk
      USE s_exitrc
C
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I1I1  , I1I2  , I1J1  , I1J2  , I2I2  , I2J2  ,
     1          IAM11 , IAM12 , IAM21 , IAM22 , IAMB  , IAMP11, IAMP12,
     2          IAMP21, IAMP22, IAMSTP, ICL1  , ICL2  , ICLNEW, ICLS1 ,
     3          ICLS2 , ICLSTM, IFIL  , IFIRST, IFRQ  , II    , IJ    ,
     4          INEW  , INTAMB, ITER  , IWLF1 , IWLF2 , IX1   ,
     5          IX1MIN, IX2   , IX2MIN, IX5   , J     , J1I2  , J1J1  ,
     6          J1J2  , J2J2  , JJ    , K     , KI    , KJ    , MINA11,
     7          MINA12, MINA21, MINA22, MXCAMB, MXCLCQ, N1LW  , N1UP  ,
     8          N5LW  , N5UP  , NAMB  , NAMSTP, NCLS1 , NCLS2 , NPAR  ,
     9          NPARN , IA1   , IA2   , ISVN1 , ISVN2 , IBLK1 , IBLK2 ,
     1          IFRQ1 , IFRQ2 , IGRP1 , IGRP2 , IREF2 , NREF  , ISAFE
C
      REAL*8    BET13 , BET13O, BET23 , BET23O, Q1    , Q12   , Q2    ,
     1          RMATIJ, RMS   , RMSMIN, RMSNEW, RMSOLD, SUMABS, TEST  ,
     2          TSTMIN, TSTOLD, X1    , X11   , X12   , X2    , X21   ,
     3          X22   , XL1DIF, XL2DIF, XL3DIF, XL5DIF
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      REAL*8    SIGAMB(*),X(*),NMAT(*),QMAT(*),BMAT(*),TIMREF(*)
C
      CHARACTER*20 RECTYP(2,*)
      CHARACTER*6 MXNLCQ,MXNAMB
C
      INTEGER*4 CLUSTR(*),IAMB1(*),IAMB2(*),OBSCLS(*)
      INTEGER*4 LOCQ(MXCLCQ,*),INTAM2(2),IA(2),JA(2),MATCH(*)
      INTEGER*4 AMBWLF(MXCAMB,2,*)
      INTEGER*4 NUMAMB(*),AMBSAT(MXCAMB,*)
      INTEGER*4 AMBCLS(MXCAMB,3,*)
      INTEGER*4 STRAMB(*)
C
      INCLUDE 'COMFREQ.inc'
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
      COMMON/MCMAMB/MXCAMB,MXNAMB
C
      DATA IFIRST/1/
C
C FACTORS FOR LINEAR COMBINATIONS AND DIFFERENTIAL IONOSPHERE MODEL
C -----------------------------------------------------------------
      IF (IFIRST.EQ.1) THEN
        BET13O  = FREQ(1) / (FREQ(1)-FREQ(2))
        BET23O  =-FREQ(2) / (FREQ(1)-FREQ(2))
        IFIRST=0
      ENDIF
C
C INITIALIZATION
C --------------
      NPAR=NPARN+NAMB
      INEW=0
      IF (SIGAMB(4).NE.0) THEN
        NAMSTP = IDNINT(SIGAMB(4))
      ELSE
        NAMSTP = NAMB
      END IF
C
C OPTIMAL SELECTION OF MAXIMAL "NAMSTP" AMBIGUITY PAIRS
C -----------------------------------------------------
      DO 50 IAMSTP=1,NAMSTP
C
C LOOP OVER ALL PAIRS
C -------------------
        RMSMIN=SIGAMB(2)
        RMSOLD=SIGAMB(2)
        TSTOLD=SIGAMB(3)
C
        DO 100 IAM11=1,NAMB
C
          IAMP11=IAM11+NPARN
          IAM12=MATCH(IAM11)
          IAMP12=IAM12+NPARN
          IF (LOCQ(5,IAMP11).NE.1) GOTO 100
C
          DO 200 IAM21=IAM11,NAMB
C
            IAMP21=IAM21+NPARN
            IAM22=MATCH(IAM21)
            IAMP22=IAM22+NPARN
            IF (LOCQ(5,IAMP21).NE.1) GOTO 200
C
C AMBIGUITIES ARE NOT FROM THE SAME FILE
            IF (LOCQ(2,IAMP11).NE.LOCQ(2,IAMP21)) GO TO 200
C
C AMBIGUITIES HAVEN'T THE SAME CLUSTER NUMBER
            IF (OBSCLS(IAM11).NE.OBSCLS(IAM21)) GO TO 200
C
C AMBIGUITY NOT RESOLVABLE OR ALREADY RESOLVED
            IF (IAMB2(IAMP11).EQ.0 .OR. IAMB2(IAMP21).EQ.0 .OR.
     1          IAMB1(IAMP11).EQ.0 .OR. IAMB1(IAMP21).EQ.0) GO TO 200
C
C SINGLE-DIFFERENCE AMBIGUITY UNRESOLVABLE
            IF (IAM11.EQ.IAM21 .AND. NREF.EQ.0) GO TO 200
C
C AMBIGUITIES ARE NOT FROM THE SAME SATELLITE SYSTEM
            IFIL=LOCQ(2,IAMP11)
            ICLS1=LOCQ(3,IAMP11)
            DO 101 IA1=1,NUMAMB(IFIL)
              IF (AMBCLS(IA1,1,IFIL).EQ.ICLS1) GOTO 102
101         CONTINUE
102         CONTINUE
            ISVN1=AMBSAT(IA1,IFIL)
C
            ICLS2=LOCQ(3,IAMP21)
            IF (LOCQ(7,IAMP11).EQ.0) THEN
              IREF2=0
            ELSE
              IREF2=LOCQ(3,LOCQ(7,IAMP11))
            ENDIF
            DO 201 IA2=1,NUMAMB(IFIL)
              IF (IAMP11.NE.IAMP21) THEN
                IF (AMBCLS(IA2,1,IFIL).EQ.ICLS2) GOTO 202
              ELSE
                IF (AMBCLS(IA2,1,IFIL).EQ.IREF2) GOTO 202
              ENDIF
201         CONTINUE
202         CONTINUE
            ISVN2=AMBSAT(IA2,IFIL)
C
            IF (ISVN1/100.NE.ISVN2/100) GOTO 200
C
C AMBIGUITIES ARE NOT FROM THE SELECTED GNSS
            IF (STRAMB(3).GT.0 .AND.
     1          ISVN1/100.NE.STRAMB(3)-1) GOTO 200
C
C AMBIGUITY WITH POTENTIAL QUARTER-CYCLE BIAS (ADD_GNSS_HERE)
            ISAFE=1
CC            IF (STRAMB(4).GT.0) THEN
            IF (STRAMB(4).GT.0 .AND. ISVN1/100.EQ.0) THEN
              CALL SATBLK(ISVN1,TIMREF(IFIL),IFRQ1,IBLK1)
              CALL SATBLK(ISVN2,TIMREF(IFIL),IFRQ2,IBLK2)
              IF     (IBLK1.LE.  6) THEN
                IGRP1=1
              ELSEIF (IBLK1.LE.  8) THEN
                IGRP1=2
                IF (ISAFE.EQ.1) IGRP1=-ISVN1
              ELSEIF (IBLK1.LT.100) THEN
                IGRP1=-100
                WRITE(LFNERR,"(/,' *** SR ARSTR4: BLOCK NUMBER ',
     1            'COULD NOT BE ASSIGNED TO A SATELLITE GROUP',
     2            /,16X,'BLOCK NUMBER: ',I3,/)") IBLK1
                CALL EXITRC(2)
              ELSE
                IGRP1=0
              ENDIF
              IF     (IBLK2.LE.  6) THEN
                IGRP2=1
              ELSEIF (IBLK2.LE.  8) THEN
                IGRP2=2
                IF (ISAFE.EQ.1) IGRP2=-ISVN2
              ELSEIF (IBLK2.LT.100) THEN
                IGRP2=-100
                WRITE(LFNERR,"(/,' *** SR ARSTR4: BLOCK NUMBER ',
     1            'COULD NOT BE ASSIGNED TO A SATELLITE GROUP',
     2            /,16X,'BLOCK NUMBER: ',I3,/)") IBLK2
                CALL EXITRC(2)
              ELSE
                IGRP2=0
              ENDIF
C PRN31/SVN52:
              IF (TIMREF(IFIL).GE.54010.D0 .AND.
     1            TIMREF(IFIL).LT.54019.D0) THEN
                IF (ISVN1.EQ. 31) IGRP1=-ISVN1
                IF (ISVN2.EQ. 31) IGRP2=-ISVN2
              ENDIF
C PRN15/SVN55:
              IF (TIMREF(IFIL).GE.54397.D0 .AND.
     1            TIMREF(IFIL).LT.54398.D0) THEN
                IF (ISVN1.EQ. 15) IGRP1=-ISVN1
                IF (ISVN2.EQ. 15) IGRP2=-ISVN2
              ENDIF
C PRN07/SVN48:
              IF (TIMREF(IFIL).GE.54544.D0 .AND.
     1            TIMREF(IFIL).LT.54545.D0) THEN
                IF (ISVN1.EQ.  7) IGRP1=-ISVN1
                IF (ISVN2.EQ.  7) IGRP2=-ISVN2
              ENDIF
C PRN01/SVN49:
              IF (TIMREF(IFIL).GE.54918.D0 .AND.
     1            TIMREF(IFIL).LT.54920.D0) THEN
                IF (ISVN1.EQ.  1) IGRP1=-ISVN1
                IF (ISVN2.EQ.  1) IGRP2=-ISVN2
              ENDIF
C PRN01/SVN49:
              IF (TIMREF(IFIL).GE.54951.D0 .AND.
     1            TIMREF(IFIL).LT.54970.D0) THEN
                IF (ISVN1.EQ.  1) IGRP1=1
                IF (ISVN2.EQ.  1) IGRP2=1
              ENDIF
C PRN25/SVN62:
              IF (TIMREF(IFIL).GE.55353.D0 .AND.
     1            TIMREF(IFIL).LT.55375.D0) THEN
                IF (ISVN1.EQ. 25) IGRP1=1
                IF (ISVN2.EQ. 25) IGRP2=1
              ENDIF
C PRN01/SVN49:
              IF (TIMREF(IFIL).GE.55677.D0 .AND.
     1            TIMREF(IFIL).LT.55688.D0) THEN
                IF (ISVN1.EQ.  1) IGRP1=1
                IF (ISVN2.EQ.  1) IGRP2=1
              ENDIF
C PRN24/SVN49:
              IF (TIMREF(IFIL).GE.55959.D0 .AND.
     1            TIMREF(IFIL).LT.56001.D0) THEN
                IF (ISVN1.EQ. 24) IGRP1=1
                IF (ISVN2.EQ. 24) IGRP2=1
              ENDIF
              IF (IGRP1.NE.IGRP2) THEN
                IF (STRAMB(4).EQ.2) THEN
                  GOTO 200
                ELSEIF (RECTYP(1,IFIL)(1:5).EQ.'JAVAD' .OR.
     1                  RECTYP(1,IFIL)(1:5).EQ.'LEICA' .OR.
     2                  RECTYP(1,IFIL)(1:3).EQ.'NOV'   .OR.
     3                  RECTYP(2,IFIL)(1:5).EQ.'JAVAD' .OR.
     4                  RECTYP(2,IFIL)(1:5).EQ.'LEICA' .OR.
     5                  RECTYP(2,IFIL)(1:3).EQ.'NOV'  ) THEN
                  GOTO 200
                ENDIF
              ENDIF
            ELSEIF (STRAMB(4).LT.0) THEN
              IF (ISVN1.EQ.-STRAMB(4) .OR.
     1            ISVN2.EQ.-STRAMB(4) ) GOTO 200
            ENDIF
C
C AMBIGUITIES DO NOT HAVE THE SAME WAVELENGTH
            IF (WLGT(1,ISVN1).NE.WLGT(1,ISVN2)) GOTO 200
C
C AMBIGUITIES LINEAR DEPENDENT (1. FREQUENCY)
            IF (IAM11.NE.IAM21) THEN
              ICL1 = CLUSTR(IAM11)
              ICL2 = CLUSTR(IAM21)
            ELSE
              ICL1 = CLUSTR(IAM11)
              ICL2 = CLUSTR(LOCQ(7,IAMP11)-NPARN)
            END IF
            IF (ICL1.NE.0 .AND. ICL1.EQ.ICL2) GO TO 200
C
C CURRENT WAVELENGTH FACTORS
            IWLF1=AMBWLF(1,1,LOCQ(2,IAMP11))
            BET13=BET13O/IWLF1
            IWLF2=AMBWLF(1,2,LOCQ(2,IAMP11))
            BET23=BET23O/IWLF2
C
C "SIGMA" TEST
            I1I1=IKF(IAMP11,IAMP11)
            I1J1=IKF(IAMP11,IAMP21)
            I1I2=IKF(IAMP11,IAMP12)
            I1J2=IKF(IAMP11,IAMP22)
            J1J1=IKF(IAMP21,IAMP21)
            J1I2=IKF(IAMP21,IAMP12)
            J1J2=IKF(IAMP21,IAMP22)
            I2I2=IKF(IAMP12,IAMP12)
            I2J2=IKF(IAMP12,IAMP22)
            J2J2=IKF(IAMP22,IAMP22)
C
C DO NOT RESOLVE SINGLUAR AMBIGUITIES (REFERENCE IF GLONASS INVOLVED)
            IF (QMAT(I1I1).EQ.0D0.OR.QMAT(I2I2).EQ.0D0.OR.
     1          QMAT(J1J1).EQ.0D0.OR.QMAT(J2J2).EQ.0D0) GOTO 200
C
            IF (IAMP11.EQ.IAMP21) THEN
              X1=X(IAMP11)
              X2=X(IAMP12)
              Q1=QMAT(I1I1)
              Q2=QMAT(I2I2)
              Q12=QMAT(I1I2)
            ELSE
              X1=X(IAMP11)-X(IAMP21)
              X2=X(IAMP12)-X(IAMP22)
              Q1=QMAT(I1I1)-2*QMAT(I1J1)+QMAT(J1J1)
              Q2=QMAT(I2I2)-2*QMAT(I2J2)+QMAT(J2J2)
              Q12=QMAT(I1I2)-QMAT(I1J2)-QMAT(J1I2)+QMAT(J1J2)
            END IF
C
            RMSNEW=RMS*DSQRT(BET13**2*Q1+2*BET13*BET23*Q12+BET23**2*Q2)
            IF (RMSNEW.LT.RMSMIN) THEN
              RMSMIN=RMSNEW
C
              N1UP = IDNINT(X1    - BET23*SIGAMB(1)-0.5)
              N1LW = IDNINT(X1    + BET23*SIGAMB(1)+0.5)
              N5UP = IDNINT(X1*IWLF2-X2*IWLF1 + SIGAMB(1)-0.5)
              N5LW = IDNINT(X1*IWLF2-X2*IWLF1 - SIGAMB(1)+0.5)
C
              TSTMIN=SIGAMB(3)
              DO 610 IX1 = N1LW,N1UP
                DO 620 IX5 = N5LW,N5UP
                  IX2=IX1-IX5
C
C "IONOSPHERE-FREE BAND" TEST
                  TEST = BET13*(X1-IX1) + BET23*(X2-IX2)
                  IF (DABS(TEST).LT.TSTMIN) THEN
                    TSTMIN = DABS(TEST)
                    TSTOLD = TSTMIN
                    XL3DIF = TEST
                    IX1MIN = IX1
                    IX2MIN = IX2
                    MINA11 = IAM11
                    MINA12 = IAM12
                    MINA21 = IAM21
                    MINA22 = IAM22
                  END IF
620             CONTINUE
610           CONTINUE
C
              IF (TSTMIN.EQ.SIGAMB(3)) THEN
                RMSMIN=RMSOLD
              ELSE
                RMSOLD=RMSMIN
              END IF
            END IF
C
200       CONTINUE
100     CONTINUE
C
C NO GOOD PAIR FOUND
        IF (TSTOLD.EQ.SIGAMB(3)) GO TO 51
C
        IF (IAMSTP.EQ.1) THEN
          WRITE (LFNPRT,1000) ITER
1000      FORMAT (/,' ', 131('-'),
     1            /,' AMBIGUITY RESOLUTION ITERATION:',I6,
     2            /,' ',131('-'),/,
     3            /,33X,'BEST INT.    CORRECTIONS IN CYCLES',
     4            /,' FILE AM1 CL1 #AM1 AM2 CL2 #AM2   L1   L2',
     5              '    L1     L2     L5      L3    RMS(L3)',
     6              '  SA1  SA2',
     7            /,' ',131('-'),/)
        ENDIF
C
C NEXT BEST FOUND :  UPDATE CLUSTERS
        INEW=INEW+2
C
        X11=X(NPARN+MINA11)
        X12=X(NPARN+MINA12)
        X21=X(NPARN+MINA21)
        X22=X(NPARN+MINA22)
C
        IF (MINA11.EQ.MINA21) THEN
          MINA21=LOCQ(7,MINA11+NPARN)-NPARN
          MINA22=LOCQ(7,MINA12+NPARN)-NPARN
          XL5DIF=(X11-X12)-(IX1MIN-IX2MIN)
          XL1DIF=X11-IX1MIN
          XL2DIF=X12-IX2MIN
        ELSE
          LOCQ(7,MINA11+NPARN)=MINA21+NPARN
          LOCQ(7,MINA12+NPARN)=MINA22+NPARN
          XL5DIF=((X11-X21)-(X12-X22))-(IX1MIN-IX2MIN)
          XL1DIF=X11-X21-IX1MIN
          XL2DIF=X12-X22-IX2MIN
        END IF
C
        IFIL =LOCQ(2,MINA11+NPARN)
        ICLS1=LOCQ(3,MINA11+NPARN)
        IF (CLUSTR(MINA11).NE.0) THEN
          NCLS1=0
          DO 210 IAMB=1,NAMB
            IF (LOCQ(5,IAMB+NPARN).EQ.2) GOTO 210
            IF (CLUSTR(IAMB).EQ.CLUSTR(MINA11)) NCLS1=NCLS1+1
210       CONTINUE
        ELSE
          NCLS1=1
        ENDIF
C
        ICLS2=LOCQ(3,MINA21+NPARN)
        IF (CLUSTR(MINA21).NE.0) THEN
          NCLS2=0
          DO 220 IAMB=1,NAMB
            IF (LOCQ(5,IAMB+NPARN).EQ.2) GOTO 220
            IF (CLUSTR(IAMB).EQ.CLUSTR(MINA21)) NCLS2=NCLS2+1
220       CONTINUE
        ELSE
          NCLS2=1
        ENDIF
C
        DO 211 IA1=1,NUMAMB(IFIL)
          IF (AMBCLS(IA1,1,IFIL).EQ.ICLS1) GOTO 212
211     CONTINUE
212     CONTINUE
        ISVN1=AMBSAT(IA1,IFIL)
C
        DO 221 IA2=1,NUMAMB(IFIL)
          IF (AMBCLS(IA2,1,IFIL).EQ.ICLS2) GOTO 222
221     CONTINUE
222     CONTINUE
        ISVN2=AMBSAT(IA2,IFIL)
C
        WRITE (LFNPRT,1001) IFIL,MINA11,ICLS1,NCLS1,
     1                           MINA21,ICLS2,NCLS2,
     2                      IX1MIN,IX2MIN,
     3                      XL1DIF,XL2DIF,XL5DIF,XL3DIF,RMSOLD,
     4                      ISVN1,ISVN2
1001    FORMAT(I5,3I4,1X,3I4,I6,I5,2F7.2,3F8.3,2I5)
C
        ICL1 = CLUSTR(MINA11)
        ICL2 = CLUSTR(MINA21)
        X(NPARN+MINA11)= IX1MIN
        IAMB1(NPARN+MINA11)  = 0
        X(NPARN+MINA12)= IX2MIN
        IAMB1(NPARN+MINA12)  = 0
C
        IF (ICL1 .NE. ICL2) THEN
          ICLNEW = AMAX0(ICL1,ICL2)
          CLUSTR(MINA11) = ICLNEW
          CLUSTR(MINA21) = ICLNEW
          DO 400 IAMB=1,NAMB
            IF ((CLUSTR(IAMB).EQ.ICL1.OR.CLUSTR(IAMB).EQ.ICL2) .AND.
     1          (CLUSTR(IAMB).NE.0)) THEN
               CLUSTR(IAMB) = ICLNEW
            END IF
400       CONTINUE
        ELSE
          IF (ICL1.EQ.0) THEN
            CLUSTR(MINA11) = ICLSTM
            CLUSTR(MINA21) = ICLSTM
            ICLSTM = ICLSTM + 1
          END IF
        END IF
C
C REDUCE THE NORMAL EQUATION SYSTEM (I ... REF. AMB., J ... SOLVED)
C -----------------------------------------------------------------
        IA(1)=NPARN+MINA21
        JA(1)=NPARN+MINA11
        IA(2)=NPARN+MINA22
        JA(2)=NPARN+MINA12
        INTAM2(1)=IX1MIN
        INTAM2(2)=IX2MIN
C
        DO 700 IFRQ=1,2
          I=IA(IFRQ)
          J=JA(IFRQ)
          INTAMB=INTAM2(IFRQ)
          II=IKF(I,I)
          IJ=IKF(I,J)
          JJ=IKF(J,J)
C
CCC        IF (I.LE.NPAR)
          SUMABS=SUMABS-2*INTAMB*BMAT(J)+INTAMB*INTAMB*NMAT(JJ)
C
          DO 300 K=1,NPAR
            KJ=IKF(K,J)
            BMAT(K)=BMAT(K)-NMAT(KJ)*INTAMB
300       CONTINUE
C
          IF (I.LE.NPAR) THEN
            BMAT(I)=BMAT(I)+BMAT(J)
C
            RMATIJ=NMAT(IJ)
            IF (I.LT.J) THEN
              DO 310 K=1,NPAR
                KI=IKF(K,I)
                KJ=IKF(K,J)
                NMAT(KI) = NMAT(KI)+NMAT(KJ)
310           CONTINUE
            ELSE
              DO 320 K=NPAR,1,-1
                KI=IKF(K,I)
                KJ=IKF(K,J)
                NMAT(KI) = NMAT(KI)+NMAT(KJ)
320           CONTINUE
            END IF
            NMAT(II) = NMAT(II)+RMATIJ+NMAT(JJ)
          END IF
C
          DO 360 K=1,NPAR
            KJ=IKF(K,J)
            NMAT(KJ)=0.D0
360       CONTINUE
C
          NMAT(JJ)=1.D0
          BMAT(J)=INTAMB
C
700     CONTINUE
C
50    CONTINUE
51    CONTINUE
C
      IF (INEW.GT.0) THEN
        DO 500 I=1,NPAR*(NPAR+1)/2
          QMAT(I)=NMAT(I)
500     CONTINUE
      END IF
C
      RETURN
      END SUBROUTINE

      END MODULE
