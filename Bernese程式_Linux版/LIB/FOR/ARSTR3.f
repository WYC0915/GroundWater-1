      MODULE s_ARSTR3
      CONTAINS

C*
      SUBROUTINE ARSTR3(STRAMB,ITER,RMS,SIGAMB,NPARN,NAMB,LOCQ,
     1                  NUMAMB,AMBSAT,AMBCLS,NMAT,
     2                  QMAT,BMAT,SUMABS,IAMB2,IAMB1,
     3                  OBSCLS,CLUSTR,X,ICLSTM,INEW,
     4                  RECTYP,TIMREF,NREF)
CC
CC NAME       :  ARSTR3
CC
CC PURPOSE    :  SIGMA-DEPENDENT AMBIGUITY RESOLUTION ALGORITHM
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
CC                             =-N: PRN NUMBER N
CC               ITER   : AMB. RESOLUTION ITERATION NUMBER    I*4
CC               RMS    : RMS                                 R*8
CC               SIGAMB : CRITICAL SIGMAS FOR SIGMA-DEPENDENT R*8(4)
CC                        AMBIGUITY RESOLUTION
CC                          SIGAMB(1): EXACTLY 1 INTEGER WITHIN
CC                                     SIGAMB(1)*SIGMA
CC                          SIGAMB(2): MAX. SIGMA ALLOWED
CC                          SIGAMB(3): MINIMAL SIGMA USED
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
CC               NPARN  : NUMBER OF NON AMB. PARAMETERS           I*4
CC               NAMB   : NUMBER OF AMBIGUITIES                   I*4
CC               LOCQ(K,I),K=1,..,MAXLCQ,I=1,..,NP: PARAMETER     I*4
CC                        CHARACTERIZATION ARRAY
CC               NUMAMB(I),I=1,..,NFTOT: NUMBER OF AMBIGU.        I*4
CC               AMBSAT(J,I),J=1,..,NUMAMB(I),I=1,..,NFTOT:       I*4
CC                        AMBIGUITY SATELLITE NUMBERS
CC               AMBCLS(L,K,I),L=1,..,NUMAMB(I), K=1,2,3,         I*4
CC                        I=1,..,NFTOT: AMBIGUITY CLUSTERS
CC               RECTYP : RECEIVER TYPES                          CH*20(2)
CC               TIMREF(I),I=... : REFERENCE TIME OF FILE         R*8
CC               NREF   : NUMBER OF REFERENCE AMBIGUITIES         I*4
CC     IN/OUT :  NMAT(I): N.E.MATRIX BEFORE INVERSION (VECTORIZED)R*8
CC               QMAT(I): N.E.MATRIX AFTER INVERSION (VECTORIZED) R*8
CC               BMAT(I): RIGHT HAND SIGHT OF N.E. SYSTEM         R*8
CC               SUMABS : SUM OF SQUARED (OBS-COMP)               R*8
CC               IAMB2(I),I=1,...NPAR !!! FLAG FOR RESOLVABLE     I*4
CC                     AMBIGUITIES  (=0: NOT RESOLVABLE (E.G. L3))
CC               IAMB1(I),I=1,...NPAR !!! FLAG FOR RESOLVED       I*4
CC                              AMBIGUITIES
CC                          =0: ALREADY RESOLVED
CC                          =1: NOT YET RESOLVED
CC               OBSCLS(I),I=1,..NAMB : AMBIGUITY CLUSTERS        I*4
CC               CLUSTR(I)    : AUXILIARY ARRAY FOR AMB. CLUSTERS I*4
CC               X(I) : SOLUTION VECTOR                           R*8
CC               ICLUSTM : NUMBER OF NEXT CLUSTER                 I*4
CC        OUT :  INEW : NUMBER OF AMBIGUITIES RESOLVED            I*4
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  L.MERVART, M.ROTHACHER, S.SCHAER
CC
CC VERSION    :  3.4
CC
CC CREATED    :  23-AUG-92
CC
CC CHANGES    :  10-APR-94 : MR: BETTER AMBIG. PRINTING, NEW PARAMETER
CC                               "ITER" IN CALL
CC               29-NOV-95 : SS: STRATEGY 1 INCLUDED IN STRATEGY 3
CC               15-JAN-97 : MR: PRINT FREQUENCY IN EACH ITERATION
CC               04-AUG-99 : MR: GLONASS IMPLEMENTATION
CC               10-DEC-03 : MR: CHECK FOR IDENTICAL FREQUENCY
CC               09-MAR-04 : RD: DO NOT RESOLVE SINGLUAR AMBIGUITIES
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               08-MAY-07 : SS: GLONASS AMBIGUITY RESOLUTION ENABLED
CC               29-AUG-07 : SS: PRINT CORRECT FREQUENCY
CC               03-OCT-07 : SS: CHECK WAVELENGTH DIFFERENCE
CC               16-OCT-08 : SL: TIMREF added, BLOCK IIR-M SAT w/ timewindow
CC               07-AUG-09 : SS: MANAGE QUARTER-CYCLE BIAS ISSUE
CC               08-JUN-10 : SS: TREAT AMBIGUITIES SEPARATELY
CC               07-DEC-10 : SS: SINGLE-DIFFERENCE AMBIGUITIES UNRESOLVABLE
CC               16-FEB-11 : SS: SIGAMB(5) FOR GLONASS AMBIGUITY RESOLUTION
CC               17-FEB-11 : SS: REFINED DEFINITION OF RECEIVER GROUPS
CC               17-FEB-11 : SS: ISTRAT REPLACED BY STRAMB
CC               17-Feb-11 : SS: STRAMB(3) FOR SELECTION OF GNSS
CC               18-FEB-11 : SS: IGNORED GPS QUARTER-CYCLE BIASES
CC               03-MAR-11 : SS: UPDATED RECEIVER GROUPS
CC               13-FEB-12 : SS: JAVAD AFFECTED BY GPS QUARTER-CYCLE BIASES
CC                               BUG CORRECTION WRT 1/4-CYCLE FOR
CC               15-FEB-12 : SS: REFINED QUARTER-CYCLE BIAS HANDLING
CC               16-FEB-12 : SS: NEGATIVE STRAMB(4) (PRN NUMBER)
CC               17-FEB-12 : SS: ADDED LIST OF QUARTER-CYCLE BIAS EXCEPTIONS
CC               29-MAR-12 : SS: SAFE HANDLING IN CASE OF QUARTER-CYCLE BIAS
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1992     UNIVERSITY OF BERN
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
      INTEGER*4 I     , IAM1  , IAM2  , IAMB  , IAMP1 , IAMP2 , IAMSTP,
     1          ICAR  , ICL1  , ICL2  , ICLNEW, ICLS1 , ICLS2 , ICLSTM,
     2          IFIL  , IFREQ , IFRQ  , II    , IJ    , INEW  ,
     3          INP1  , INP2  , INTAMB, ITER  , IUP   , J     ,
     4          JJ    , K     , KI    , KJ    , LOW   , MINAM1, MINAM2,
     5          MXCAMB, MXCLCQ, NAMB  , NAMSTP, NCLS1 , NCLS2 , NPAR  ,
     6          NPARN , IA1   , IA2   , ISVN1 , ISVN2 , IBLK1 , IBLK2 ,
     7          IFRQ1 , IFRQ2 , IGRP1 , IGRP2 , IREF2 , NREF  , IAMGLO,
     8          ISAFE
C
      REAL*8    RMATIJ, RMS   , RMSIJ , RMSMIN, RMSTST, SUMABS, UPPER ,
     1          XDIF  , XLOW
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*20 RECTYP(2,*)
      CHARACTER*6 MXNLCQ,MXNAMB
      CHARACTER*4 RECSTR(2)
      INTEGER*4   CLUSTR(*),IAMB1(*),IAMB2(*),OBSCLS(*)
      INTEGER*4   LOCQ(MXCLCQ,*)
      INTEGER*4   NUMAMB(*),AMBSAT(MXCAMB,*)
      INTEGER*4   AMBCLS(MXCAMB,3,*)
      INTEGER*4   STRAMB(*)

      REAL*8      SIGAMB(*),X(*),NMAT(*),QMAT(*),BMAT(*),TIMREF(*)
C
      INCLUDE 'COMFREQ.inc'
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
      COMMON/MCMAMB/MXCAMB,MXNAMB
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
      IAMGLO = IDNINT(SIGAMB(5))
C
C OPTIMAL SELECTION OF MAXIMAL "NAMSTP" AMBIGUITY PAIRS
C -----------------------------------------------------
      DO 50 IAMSTP=1,NAMSTP
C
C LOOP OVER ALL PAIRS
        RMSMIN=1.D20
ccccc        DFRMIN=1.D20
C
        DO 100 IAM1=1,NAMB
          IAMP1=IAM1+NPARN
          DO 200 IAM2=IAM1,NAMB
            IAMP2=IAM2+NPARN
C
C AMBIGUITIES ARE NOT FROM THE SAME FILE
            IF (LOCQ(2,IAMP1).NE.LOCQ(2,IAMP2)) GO TO 200
C
C AMBIGUITIES ARE NOT FROM THE SAME FREQUENCY
            IF (LOCQ(4,IAMP1).NE.LOCQ(4,IAMP2)) GO TO 200
C
C AMBIGUITIES HAVEN'T THE SAME CLUSTER NUMBER
            IF (OBSCLS(IAM1).NE.OBSCLS(IAM2)) GO TO 200
C
C AMBIGUITY NOT RESOLVABLE OR ALREADY RESOLVED
            IF (IAMB2(IAMP1).EQ.0 .OR. IAMB2(IAMP2).EQ.0 .OR.
     1          IAMB1(IAMP1).EQ.0 .OR. IAMB1(IAMP2).EQ.0) GO TO 200
C
C SINGLE-DIFFERENCE AMBIGUITY UNRESOLVABLE
            IF (IAM1.EQ.IAM2 .AND. NREF.EQ.0) GO TO 200
C
C AMBIGUITIES ARE NOT FROM THE SAME SATELLITE SYSTEM
            IFIL=LOCQ(2,IAMP1)
            ICLS1=LOCQ(3,IAMP1)
            ICAR=LOCQ(5,IAMP1)
            IF (ICAR.EQ.2) THEN
              IFREQ=2
            ELSEIF (ICAR.EQ.5) THEN
              IFREQ=3
            ELSE
              IFREQ=1
            ENDIF
            DO 101 IA1=1,NUMAMB(IFIL)
              IF (AMBCLS(IA1,IFREQ,IFIL).EQ.ICLS1) GOTO 102
101         CONTINUE
102         CONTINUE
            ISVN1=AMBSAT(IA1,IFIL)
C
            ICLS2=LOCQ(3,IAMP2)
            IF (LOCQ(7,IAMP1).EQ.0) THEN
              IREF2=0
            ELSE
              IREF2=LOCQ(3,LOCQ(7,IAMP1))
            ENDIF
            DO 201 IA2=1,NUMAMB(IFIL)
              IF (IAMP1.NE.IAMP2) THEN
                IF (AMBCLS(IA2,IFREQ,IFIL).EQ.ICLS2) GOTO 202
              ELSE
                IF (AMBCLS(IA2,IFREQ,IFIL).EQ.IREF2) GOTO 202
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
                WRITE(LFNERR,"(/,' *** SR ARSTR3: BLOCK NUMBER ',
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
                WRITE(LFNERR,"(/,' *** SR ARSTR3: BLOCK NUMBER ',
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
            IF (WLGT(ICAR,ISVN1).NE.WLGT(ICAR,ISVN2) .AND.
     1          IAMGLO.NE.-1) THEN
              DO I=1,2
                RECSTR(I)=RECTYP(I,IFIL)(1:3)//' '
              ENDDO
              IF (IAMGLO.EQ.0) THEN
                GOTO 200
              ELSEIF (IAMGLO.EQ.1) THEN
                IF (RECTYP(1,IFIL).NE.RECTYP(2,IFIL)) GOTO 200
              ELSEIF (IAMGLO.EQ.2) THEN
                IF (RECSTR(1).NE.RECSTR(2)) GOTO 200
              ELSEIF (IAMGLO.EQ.3) THEN
                DO I=1,2
C GENERAL DEFINITION OF VARIOUS RECEIVER GROUPS:
                  IF (RECSTR(I)(1:3).EQ.'ASH') RECSTR(I)='@TRI'
                  IF (RECSTR(I)(1:3).EQ.'JAV') RECSTR(I)='@JNS'
                  IF (RECSTR(I)(1:3).EQ.'JPS') RECSTR(I)='@JNS'
                  IF (RECSTR(I)(1:3).EQ.'LEI') RECSTR(I)='@LEI'
                  IF (RECSTR(I)(1:3).EQ.'NOV') RECSTR(I)='@LEI'
                  IF (RECSTR(I)(1:3).EQ.'SEP') RECSTR(I)='@SEP'
                  IF (RECSTR(I)(1:3).EQ.'TPS') RECSTR(I)='@JNS'
                  IF (RECSTR(I)(1:3).EQ.'TRI') RECSTR(I)='@TRI'
C EXCEPTIONS CONCRNING SPECIFIC RECEIVER TYPES:
CC                  IF (RECTYP(I,IFIL)(1:11).EQ.'ASHTECH Z18')
CC     1              RECSTR(I)='@ASH'
C INCLUDED IN DEFINITION LIST ABOVE?
                  IF (RECSTR(I)(1:1).NE.'@') THEN
                    WRITE(LFNERR,"(/,' *** SR ARSTR3: RECEIVER TYPE ',
     1                'COULD NOT BE ASSIGNED TO A RECEIVER GROUP',
     2                /,16X,'RECEIVER TYPE: ',A,/)") RECTYP(I,IFIL)
                    CALL EXITRC(2)
                  ENDIF
                ENDDO
                IF (RECSTR(1).NE.RECSTR(2)) GOTO 200
              ENDIF
            ENDIF
C
C IF WIDELANE INTRODUCED, ONLY AMBIGUITIES IN THE SAME L5 CLUSTER MAY
C BE RESOLVED
            IFRQ=LOCQ(4,IAMP1)
            ICAR=LOCQ(5,IAMP1)
            IF ((ICAR.EQ.3 .OR. ICAR.EQ.4) .AND. IFRQ.EQ.1) THEN
              IF (IAM1.EQ.IAM2) THEN
                IF (IAMB2(IAMP1).NE.IAMB2(LOCQ(7,IAMP1))) GOTO 200
              ELSE
                IF (IAMB2(IAMP1).NE.IAMB2(IAMP2)) GOTO 200
              ENDIF
            ENDIF
C
C AMBIGUITIES LINEAR DEPENDENT
            IF (IAM1.NE.IAM2) THEN
              ICL1 = CLUSTR(IAM1)
              ICL2 = CLUSTR(IAM2)
            ELSE
              ICL1 = CLUSTR(IAM1)
              ICL2 = CLUSTR(LOCQ(7,IAMP1)-NPARN)
              IF (LOCQ(7,IAMP1).EQ.0) GOTO 200
            END IF
            IF (ICL1.NE.0 .AND. ICL1.EQ.ICL2) GO TO 200
C
C "SIGMA" TEST
            INP1=NPARN+IAM1
            INP2=NPARN+IAM2
            II=IKF(INP1,INP1)
            JJ=IKF(INP2,INP2)
            IJ=IKF(INP1,INP2)
C
C DO NOT RESOLVE SINGLUAR AMBIGUITIES (REFERENCE IF GLONASS INVOLVED)
            IF (QMAT(II).EQ.0D0.OR.QMAT(JJ).EQ.0D0) GOTO 200
C
            IF (IAM1.EQ.IAM2) THEN
              RMSIJ=RMS*DSQRT(QMAT(II))
            ELSE
              RMSIJ=RMS*DSQRT(QMAT(II)-2*QMAT(IJ)+QMAT(JJ))
            END IF
C
C SIGMA TESTS FOR STRATEGY 3 ONLY
            IF (STRAMB(1).EQ.1) THEN
              IF (IAM1.EQ.IAM2) THEN
                LOW=IDNINT(X(INP1))
              ELSE
                LOW=IDNINT(X(INP1)-X(INP2))
              END IF
            ELSE
              IF (RMSIJ.GT.SIGAMB(2)) GO TO 200
              RMSTST=RMSIJ
C
              IF (RMSTST.LT.SIGAMB(3)) RMSTST=SIGAMB(3)
              IF (IAM1.EQ.IAM2) THEN
                UPPER=X(INP1)+SIGAMB(1)*RMSTST
                XLOW =X(INP1)-SIGAMB(1)*RMSTST
              ELSE
                UPPER=X(INP1)-X(INP2)+SIGAMB(1)*RMSTST
                XLOW =X(INP1)-X(INP2)-SIGAMB(1)*RMSTST
              END IF
              IUP=IDNINT(UPPER-.5D0)
              LOW=IDNINT(XLOW+.5D0)
              IF(IUP.NE.LOW) GOTO 200
            END IF
C
            IF (RMSMIN.GT.RMSIJ) THEN
              RMSMIN=RMSIJ
              MINAM1=IAM1
              MINAM2=IAM2
              INTAMB=LOW
            END IF
C
ccc         DFRTST=DABS(WLGT(IFRQ,ISVN(IAM1))-WLGT(IFRQ,ISVN(IAM2)))    GLONASS
ccc         IF (DFRMIN.GT.DFRTST) THEN                                  GLONASS
ccc           DFRMIN=DFRTST                                             GLONASS
ccc           RMSMIN=RMSIJ                                              GLONASS
ccc           MINAM1=IAM1                                               GLONASS
ccc           MINAM2=IAM2                                               GLONASS
ccc           INTAMB=LOW                                                GLONASS
ccc         ELSE IF (DFRMIN.EQ.DFRTST) THEN                             GLONASS
ccc           IF (RMSMIN.GT.RMSIJ) THEN                                 GLONASS
ccc             RMSMIN=RMSIJ                                            GLONASS
ccc             MINAM1=IAM1                                             GLONASS
ccc             MINAM2=IAM2                                             GLONASS
ccc             INTAMB=LOW                                              GLONASS
ccc           END IF                                                    GLONASS
ccc         ENDIF                                                       GLONASS
C
200       CONTINUE
100     CONTINUE
C
C NO GOOD PAIR FOUND
        IF (RMSMIN.EQ.1.D20) GO TO 51
C
        IF (IAMSTP.EQ.1)
     1    WRITE (LFNPRT,1000) ITER
1000      FORMAT (/,' ', 131('-'),
     1            /,' AMBIGUITY RESOLUTION ITERATION:',I6,
     2            /,' ',131('-'),/,
     3            /,' FILE  FRQ  AMB1 CLU1 #AMB1  AMB2 CLU2 #AMB2',
     4              '      AMBIGUITY    RMS   SAT1 SAT2',
     5            /,' ',131('-'),/)
C
C NEXT BEST FOUND :  PRINT INFORMATION
        INEW=INEW+1
C
        IF (MINAM1.EQ.MINAM2) THEN
          MINAM2=LOCQ(7,MINAM1+NPARN)-NPARN
          XDIF=X(NPARN+MINAM1)
        ELSE
          LOCQ(7,MINAM1+NPARN)=MINAM2+NPARN
          XDIF=X(NPARN+MINAM1)-X(NPARN+MINAM2)
        ENDIF
C
        IFIL =LOCQ(2,MINAM1+NPARN)
        ICLS1=LOCQ(3,MINAM1+NPARN)
        IFRQ =LOCQ(4,MINAM1+NPARN)
        IFREQ=LOCQ(5,MINAM1+NPARN)
        IF (CLUSTR(MINAM1).NE.0) THEN
          NCLS1=0
          DO 210 IAMB=1,NAMB
            IF (CLUSTR(IAMB).EQ.CLUSTR(MINAM1)) NCLS1=NCLS1+1
210       CONTINUE
        ELSE
          NCLS1=1
        ENDIF
C
        ICLS2=LOCQ(3,MINAM2+NPARN)
        IF (CLUSTR(MINAM2).NE.0) THEN
          NCLS2=0
          DO 220 IAMB=1,NAMB
            IF (CLUSTR(IAMB).EQ.CLUSTR(MINAM2)) NCLS2=NCLS2+1
220       CONTINUE
        ELSE
          NCLS2=1
        ENDIF
C
        ICAR=LOCQ(5,IAMP1)
        IF (ICAR.EQ.2) THEN
          IFREQ=2
        ELSEIF (ICAR.EQ.5) THEN
          IFREQ=3
        ELSE
          IFREQ=1
        ENDIF
        DO 211 IA1=1,NUMAMB(IFIL)
          IF (AMBCLS(IA1,IFREQ,IFIL).EQ.ICLS1) GOTO 212
211     CONTINUE
212     CONTINUE
        ISVN1=AMBSAT(IA1,IFIL)
C
        DO 221 IA2=1,NUMAMB(IFIL)
          IF (AMBCLS(IA2,IFREQ,IFIL).EQ.ICLS2) GOTO 222
221     CONTINUE
222     CONTINUE
        ISVN2=AMBSAT(IA2,IFIL)
C
        WRITE (LFNPRT,1001) IFIL,IFRQ,MINAM1,ICLS1,NCLS1,
     1                                MINAM2,ICLS2,NCLS2,
     2                                XDIF,RMSMIN,ISVN1,ISVN2
1001    FORMAT(I5,I4,I7,2I5,I7,2I5,F16.3,F8.3,2I5)
ccc        DLL=(WLGT(IFRQ,ISVN(MINAM1))-WLGT(IFRQ,ISVN(MINAM2)))/          GLONASS
ccc     1                                 WLGT(IFRQ,ISVN(MINAM1))          GLONASS
C
ccc        WRITE (LFNPRT,1001) IFIL,IFRQ,MINAM1,ICLS1,NCLS1,               GLONASS
ccc     1                                MINAM2,ICLS2,NCLS2,
ccc     2                                XDIF,RMSMIN
ccc        WRITE(LFNPRT,1002) ISVN(MINAM1),ISVN(MINAM2),DLL                GLONASS
ccc1001    FORMAT(I5,I4,I7,2I5,I7,2I5,F16.3,F8.3)
ccc1002    FORMAT(6X,'SVN1 ',I5,7X,'SVN2 ',I5,10X,'DL/L',F14.10)           GLONASS
C
C UPDATE CLUSTERS
        ICL1 = CLUSTR(MINAM1)
        ICL2 = CLUSTR(MINAM2)
        X(NPARN+MINAM1)= INTAMB
        IAMB1(NPARN+MINAM1)  = 0
C
        IF (ICL1 .NE. ICL2) THEN
          ICLNEW = AMAX0(ICL1,ICL2)
          CLUSTR(MINAM1) = ICLNEW
          CLUSTR(MINAM2) = ICLNEW
          DO 400 IAMB=1,NAMB
            IF ((CLUSTR(IAMB).EQ.ICL1.OR.CLUSTR(IAMB).EQ.ICL2) .AND.
     1          (CLUSTR(IAMB).NE.0)) THEN
               CLUSTR(IAMB) = ICLNEW
            END IF
400       CONTINUE
        ELSE
          IF (ICL1.EQ.0) THEN
            CLUSTR(MINAM1) = ICLSTM
            CLUSTR(MINAM2) = ICLSTM
            ICLSTM = ICLSTM + 1
          END IF
        END IF
C
C REDUCE THE NORMAL EQUATION SYSTEM (I ... REF. AMB., J ... SOLVED)
C -----------------------------------------------------------------
        I=NPARN+MINAM2
        J=NPARN+MINAM1
        II=IKF(I,I)
        IJ=IKF(I,J)
        JJ=IKF(J,J)
C
        SUMABS=SUMABS-2*INTAMB*BMAT(J)+INTAMB*INTAMB*NMAT(JJ)
C
        DO 300 K=1,NPAR
          KJ=IKF(K,J)
          BMAT(K)=BMAT(K)-NMAT(KJ)*INTAMB
300     CONTINUE
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
310         CONTINUE
          ELSE
            DO 320 K=NPAR,1,-1
              KI=IKF(K,I)
              KJ=IKF(K,J)
              NMAT(KI) = NMAT(KI)+NMAT(KJ)
320         CONTINUE
          END IF
          NMAT(II) = NMAT(II)+RMATIJ+NMAT(JJ)
        END IF
C
        DO 360 K=1,NPAR
          KJ=IKF(K,J)
          NMAT(KJ)=0.D0
360     CONTINUE
C
        NMAT(JJ)=1.D0
        BMAT(J)=INTAMB
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
