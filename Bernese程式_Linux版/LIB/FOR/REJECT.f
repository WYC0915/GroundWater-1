      MODULE s_REJECT
      CONTAINS

C*
      SUBROUTINE REJECT(IPRNT2,ITITL2,IRJECT,MXOGAP,MXIOND,NSAT  ,
     1                  SVN   ,INDFRQ,FRQAUX,NFREQ ,MRK1O2,
     2                  MINCYC,ABSACT,ABSLST,RHS   ,MRKDEL,IDELTT,
     3                  IEPOCH,TIMLST,FRSTOB,NSLIP1,NSLIP ,LSTSLP,
     4                  SLPLST,SLPXXX,NDEL  ,LSTDEL,NMAXI ,LSTMXI,
     5                  IAMNEW,NNEWAM,LSTAMB,AMSFLG,IRETC )
CC
CC NAME       :  REJECT
CC
CC PURPOSE    :  REJECTION OF OBSERVATIONS
CC
CC PARAMETERS :
CC         IN :  IPRNT2 : PRINT LEVEL FOR CYCLE SLIP DETECT.   I*4
CC                        =0: NO MESSAGES PRINTED
CC                        =1: PRINT SUMMARY
CC                        =2: ALL MESSAGES PRINTED
CC               ITITL2 : TITLE PRINT FLAG                     I*4
CC                        =0: TITLE PRINTED ALREADY
CC                        =1: TITLE NOT PRINTED YET
CC               IRJECT : OUTLIER REJECTION (YES=1,NO=0)       I*4
CC               MXOGAP : MAXIMUM OBSERVATION GAP ALLOWED IN   I*4
CC                        OUTLIER REJECTION (SEC)
CC               MXIOND : MAXIMUM IONOSPHERE CHANGE BETWEEN    I*4
CC                        EPOCHS (IN % OF L1 CYCLES) FOR OUT-
CC                        LIER REJECTION
CC               NSAT   : NUMBER OF SATELLITE IN FILE         I*4
CC               SVN(I),I=1,..,NSAT: SV-NUMBERS               I*4
CC               INDFRQ : INDEX OF FREQUENCY TO BE PROCESSED  I*4
CC                        IN ARRAY "FRQAUX"
CC               FRQAUX(I),I=1,2,3: FREQUENCIES               I*4
CC               NFREQ  : NUMBER OF FREQUENCIES IN FILE       I*4
CC               MRK1O2 : FLAG TO MARK UNPAIRED L1/L2 OBSER-  I*4
CC                        VATIONS
CC                         =0 : NO MARKING DONE
CC                         =1 : L1 WITHOUT L2, OR L2 WITHOUT L1
CC                              OBSERVATIONS ARE MARKED
CC               MINCYC : ACCEPT SLIPS > "MINCYC" CYCLES      I*4
CC               ABSACT(K,ISAT),K=1,2,3, ISAT=1,2,..,NSAT     I*4
CC                        TERMS "OBS-COMPUTED"
CC               ABSLST(K,ISAT),K=1,2,3, ISAT=1,2,..,NSAT     I*4
CC                        TERMS "OBS-COMPUTED" FROM PREVIOUS
CC                        EPOCHS
CC               RHS(K,ISAT),K=1,2,3     ISAT=1,2,..,NSAT:    R*8
CC                        RIGHT HAND SIDES OF COND. EQNS IN FREQ. K
CC               MRKDEL(ISAT),ISAT=1,2,..,NSAT: MARKS FOR     CH*1
CC                        SATELLITES
CC                        =O: TRIPLE DIFF. OK
CC                        =N: NOT USED
CC                        =Y: TRIPLE DIFFERENCE MARKED FOR
CC                            ELIMINATION
CC                        =R: REFERENCE SATELLITE
CC               IDELTT : TIME INTERVAL BETWEEN EPOCHS (SEC)  I*4
CC               IEPOCH : EPOCH NUMBER
CC               TIMLST(K,ISAT),K=1,2,3, ISAT=1,2,..,NSAT:    I*4
CC                        LAST OBS. EPOCH
CC               FRSTOB(K,ISAT),K=1,2,3, ISAT=1,2,..,NSAT:    CH*1
CC                        =Y: DIFF. IS FORMED WITH FIRST OBS.
CC               NSLIP1 : INDEX OF FIRST SLIP TO BE CHECKED   I*4
CC     IN/OUT :  NSLIP  : NUMBER OF SLIPS                     I*4
CC               LSTSLP(K,IS),K=1,..,6, IS=1,2,..,NSLIP:      I*4
CC                        SLIP DESCRIPTION
CC                      K=1: EPOCH NUMBER
CC                      K=2: SV NUMBER
CC                      K=3: FREQUENCY NUMBER
CC                      K=4: WAVELENGTH FACTOR
CC                      K=5: WAVELENGTH FACTOR OF L5
CC                           IF AN L1 AND L2 SLIP OCCURS AT
CC                           THE SAME EPOCH FOR THE SAME
CC                           SATELLITE:
CC                           =3 FOR L1, INDICATING THAT
CC                              AN L2 SLIP OCCURED TOO
CC                           =WAVELENGTH OF L5 FOR L2
CC                      K=6: DETECTED BY
CC                           =1: SINGLE FREQ. SLIP DETECTION
CC                           =2: DUAL   FREQ. SLIP DETECTION
CC                           =3: CLOCK
CC                           =4: USER
CC                           =5: MILLI-SEC JUMP
CC                           ALL CYCLE SLIPS DETECTED IN THE
CC                           LATEST RUN HAVE A NEGATIVE SIGN
CC               SLPLST(IS),IS=1,2,..,NSLIP: SIZE OF SLIP IS  R*8
CC               SLPXXX(2,I),I=1,..,NSLIP: REAL VALUED        R*8
CC                        ESTIMATES FOR SLIP I - INTEGER SLIP
CC                        SINGLE FREQ.: L1/L2 ESTIMATES
CC                        DUAL   FREQ.: L1/L2 AND L5 ESTIMATES
CC               NDEL   : NUMBER OF DELETIONS                 I*4
CC               LSTDEL(K,I),K=1,..,5, I=1,2,..,NDEL          I*4
CC                        DEFINITION OF MARK REQUEST NUMBER I
CC                        (1,I): SV-NUMBER
CC                        (2,I): FIRST EPOCH OF MARKED AREA I
CC                        (3,I): LAST  EPOCH OF MARKED AREA I
CC                        (4,I): FREQUENCY (1=L1, 2=L2)
CC                        (5,I): MARKED BY
CC                               =1: SINGLE FREQ. REJECTION
CC                               =2: DUAL   FREQ. REJECTION
CC                               =3: UNPAIRED L1/L2 OBSERVATIONS
CC                               =4: USER
CC                               =5: SMALL ELEVATION
CC                               =6: SMALL PIECES
CC                               =7: BAD OBSERVED-COMPUTED
CC                               ALL AREAS MARKED OR CHANGED IN THE
CC                               LATEST RUN HAVE A NEGATIVE SIGN
CC               NMAXI  : NUMBER OF "MAX. INTERVAL EXCEEDED"   I*4
CC               LSTMXI(K,I),K=1,..,5, I=1,2,..,NMAXI          I*4
CC                        DEFINITION OF "MAX.INT.EXC" NUMBER I
CC                        (1,I): EPOCH
CC                        (2,I): SATELLITE
CC                        (3,I): FREQUENCY (1=L1, 2=L2, 3=BOTH)
CC                        (4,I): BREAK IN NUMBER OF EPOCHS
CC                        (5,I): DETECTED BY
CC                               =1: SINGLE FREQ. REJECTION
CC                               =2: DUAL   FREQ. REJECTION
CC               IRETC  : RETURN CODE                         I*4
CC                        =0  : NO REJECTIONS
CC                        =1  : REJECTION(S), RECHECK EPOCH
CC               IAMNEW(I),I=1,2,3: SETTING OF NEW AMBIGUITIES I*4
CC                        I=1 : USE CYCLE SLIP FLAG (0/1)
CC                        I=2 : IF PROBLEM IN SLIP-FIXING (0/1)
CC                        I=3 : AFTER GAP LARGER THAN (SEC)
CC                        I=4 : USE AMBIGUITIES FROM FILE (0/1)
CC                        I=5 : MIN. TIME OF OBS. PER AMB. (SEC)
CC               AMSFLG : INDICATER OF MAXAMS EXCEEDINGS      CH*1
CC    IN/OUT :   NNEWAM(ISATEL) : NUMBER OF AMBIGUITIES        I*4
CC               LSTAMB(I,ISATEL,IAMB)  LIST OF AMBIGUITIES    I*4
CC                        I=1 : THE FIRST EPOCH
CC                        I=2 : TYPE: 1 ... FILE
CC                                    2 ... CYCLE SLIP
CC                                    3 ... USER
CC                                    4 ... GAP
CC                                    5 ... PREPROCESSING PROBLEM
CC                                    6 ... CLOCK EVENT
CC                        I=3 : THE LAST EPOCH WITH OBERVATIONS
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER, L.MERVART
CC
CC VERSION    :  3.4  (JAN 94)
CC
CC CREATED    :  88/12/14 10:33
CC
CC CHANGES    :  05-JUN-92 : ??: CHANGES FOR THE NEW MAUPRP VERSION
CC               23-DEC-92 : ??: DECLARATION OF "MXNSAT"
CC               10-AUG-94 : MR: CALL EXITRC
CC               24-APR-95 : MR: UPDATE DESCRIPTION OF LSTDEL
CC                5-MAR-96 : TS: HANDLING OF "MAXAMS" EXCEEDINGS
CC               13-JAN-97 : MR: ADD CHECK IF OBS. MARKED --> FEWER
CC                               AMBIGUITIES SET UP.
CC               20-JAN-97 : MR: UPDATE ABSLST AND ABSACT FOR BOTH
CC                               FREQUENCIES IN THE MODE "BOTH"
CC               14-OCT-98 : HH: MODIFICATIONS FOR GLONASS
CC               16-JAN-02 : MR: MINCYC=-1
CC               07-OCT-02 : RD: SVNREF REMOVED FROM PARAMETER LIST (NOT USED)
CC               06-DEC-02 : RD: UPDATE OF LSTAMB
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               07-DEC-06 : RD: BUGFIX FOR SMALL CYCLE SLIP CORR. IN BOTH MODE
CC               29-FEB-12 : RD: CORRECT ARRAY DIMENSIONS OF IWLF0 AND XSLIP0
CC               26-MAR-12 : RD: USE WTMSGS AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnerr
      USE s_mrkobs
      USE s_updamb
      USE s_wtmsgs
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IDELF , IDELTT, IEPO  , IEPOCH, IFREQ , IFRMSG, IFRQ  ,
     1          IFRQ1 , IFRQ2 , IFRQ3 , IMAXI , IMSG  , INDFR2, INDFRQ,
     2          INTEPO, IPRNT2, IRC   , IRETC , IRJECT, IRSAT0, IS    ,
     3          ISAT  , ISATUP, ISLIP , ISLPNW, ISVN  , ITITL2,
     4          K     , MINCYC, MRK1O2, MXCDEL, MXCMXI, MXCSAT, MXIOND,
     5          MXOGAP, NDEL  , NFREQ , NMAXI , NREJ  , NSAT  , NSLIP ,
     6          NSLIP1
C
      REAL*8    FRACT , FRCMAX
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      CHARACTER*1 FRSTOB(3,*),MRKDEL(*),AMSFLG
      CHARACTER*6 MXNSAT,MXNDEL,MXNMXI
C
      INTEGER*4   SVN(*),LSTSLP(6,*),LSTDEL(5,*),TIMLST(3,*),FRQAUX(*)
      INTEGER*4   SATMXI,LSTMXI(5,*),IAMNEW(*), IWLF0(3)
      INTEGER*4   LSTAMB(3,MXCSAT,*),NNEWAM(MXCSAT)
C
      REAL*8      ABSACT(3,*),ABSLST(3,*),SLPLST(*),SLPXXX(2,*)
      REAL*8      RHS(3,*),SLIP0(3),XSLIP0(3)
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMDEL/MXCDEL,MXNDEL
      COMMON/MCMMXI/MXCMXI,MXNMXI
C
C
      DATA IFRQ3/3/,IRSAT0/0/
C
C INIT VARIABLES
C --------------
      IWLF0 =0
      SLIP0 =0.D0
      XSLIP0=0.D0
C
C SINGLE FREQUENCY REJECTION
C --------------------------
      IF(INDFRQ.LE.2) THEN
        IFREQ=FRQAUX(INDFRQ)
        IF (INDFRQ.EQ.1) INDFR2=2
        IF (INDFRQ.EQ.2) INDFR2=1
C
        IRETC = 0
        NREJ=0

        FRCMAX=0.D0
        DO 11 ISLIP=NSLIP1,NSLIP
          FRACT=DABS(SLPXXX(1,ISLIP))
          IF(FRACT.GT.MXIOND/100.D0 .OR.
     1       DABS(SLPLST(ISLIP)).LE.MINCYC.OR.MINCYC.EQ.-1) THEN
            IF (NINT(SLPLST(ISLIP)).NE.0) NREJ=NREJ+1
          ENDIF
11      CONTINUE
C
C NEW AMBIGUITIES FOR ALL SATELLITES (UNDEFINED CLOCK) ?
C ------------------------------------------------------
        DO 35 ISLIP=NSLIP1,NSLIP
          IF (LSTSLP(6,ISLIP).EQ.-100) THEN
            DO 12 ISAT=1,NSAT
              CALL UPDAMB(ISAT,IEPOCH,IEPOCH,5,NNEWAM,
     1                    LSTAMB,NSAT,SVN,IPRNT2,AMSFLG,IRC)
              ABSLST(INDFRQ,ISAT)=1.D20
              IF(MRK1O2.EQ.1.AND.NFREQ.EQ.2)
     1                            ABSLST(INDFR2,ISAT)=1.D20
12          CONTINUE
            IRETC=1
            RETURN
          ENDIF
35      CONTINUE
C
C NO OUTLIERS FOUND:
        IF(NREJ.EQ.0) THEN
C
C RETAIN SLIPS OF SIZE > MINCYC
          ISLPNW=NSLIP1-1
          DO 30 ISLIP=NSLIP1,NSLIP
            IF(DABS(SLPLST(ISLIP)).GT.MINCYC.AND.MINCYC.NE.-1) THEN
C
              ISLPNW=ISLPNW+1
              SLPLST(ISLPNW)=SLPLST(ISLIP)
              SLPXXX(1,ISLPNW)=SLPXXX(1,ISLIP)
              DO 20 K=1,6
                LSTSLP(K,ISLPNW)=LSTSLP(K,ISLIP)
20            CONTINUE
            ENDIF
30        CONTINUE
C
          NSLIP=ISLPNW
          RETURN
        ENDIF
C
        DO 10 ISLIP=NSLIP1,NSLIP
C
C INDEX OF SATELLITE IN ARRAY ABSACT:
          DO 2 IS=1,NSAT
            IF(LSTSLP(2,ISLIP).EQ.SVN(IS)) ISAT=IS
2         CONTINUE
C
          INTEPO=(IEPOCH-TIMLST(INDFRQ,ISAT))*IDELTT
          SATMXI=SVN(ISAT)
          ISATUP = ISAT
C
          IF (INTEPO .GT. IAMNEW(3)) THEN
            CALL UPDAMB(ISATUP,TIMLST(INDFRQ,ISATUP)+1,IEPOCH,4,
     1                  NNEWAM,LSTAMB,NSAT,SVN,IPRNT2,AMSFLG,IRC)
            ABSLST(INDFRQ,ISATUP)=1.D20
            IF(MRK1O2.EQ.1.AND.NFREQ.EQ.2) ABSLST(INDFR2,ISATUP)=1.D20
            IRETC = 1
            GO TO 10
          END IF
C
          FRACT=DABS(SLPXXX(1,ISLIP))
          IF(FRACT.LE.MXIOND/100.D0.AND.SLPLST(ISLIP).EQ.0D0) THEN
            GO TO 10
          ELSE IF(FRACT.LE.MXIOND/100.D0.AND.
     1       DABS(SLPLST(ISLIP)).GT.MINCYC.AND.MINCYC.NE.-1) THEN
            GO TO 10
          ELSE
            IF(IPRNT2.GE.2) THEN
              IMSG=3
              CALL WTMSGS(IMSG,ITITL2,IEPOCH,LSTSLP(2,ISLIP),0,
     1                    IFREQ,IWLF0,SLIP0,SLPXXX(1,ISLIP))
            ENDIF
          ENDIF
C
C MAXIMUM INTERVAL EXCEEDED
          IF(INTEPO.GT.MXOGAP) THEN
            IF(IPRNT2.GE.2) THEN
              IMSG=6
              CALL WTMSGS(IMSG,ITITL2,IEPOCH,LSTSLP(2,ISLIP),0,
     1                    IFREQ,IWLF0,SLIP0,SLPXXX(1,ISLIP))
            ENDIF
            DO 15 IMAXI=NMAXI,1,-1
              IF(IEPOCH.EQ.LSTMXI(1,IMAXI).AND.
     1           SATMXI.EQ.LSTMXI(2,IMAXI).AND.
     2           IFREQ .EQ.LSTMXI(3,IMAXI)) GOTO 18
15          CONTINUE
C
            NMAXI=NMAXI+1
            IF(NMAXI.GT.MXCMXI) THEN
              WRITE(LFNERR,2000) NMAXI,MXCMXI
2000          FORMAT(/,' *** SR REJECT: MAX. NUMBER OF "MAX.INT.EXC." ',
     1                               'EXCEEDED',
     2                         /,16X,'NUMBER OF "MAX.INT." >=',I6,/,
     3                           16X,'MAX. NUMBER ALLOWED  :',I6,/)
              CALL EXITRC(2)
            ENDIF
            LSTMXI(1,NMAXI)=IEPOCH
            LSTMXI(2,NMAXI)=SATMXI
            LSTMXI(3,NMAXI)=IFREQ
            LSTMXI(4,NMAXI)=INTEPO/IDELTT
            LSTMXI(5,NMAXI)=1
18          CONTINUE
          ENDIF
C
C OUTLIER(S) FOUND
          IDELF=0
          IF(FRSTOB(INDFRQ,ISAT).EQ.'Y') THEN
            ISVN=SVN(ISAT)
            IEPO=TIMLST(INDFRQ,ISAT)
            ABSLST(INDFRQ,ISAT)=1.D20
            IF(MRK1O2.EQ.1.AND.NFREQ.EQ.2) ABSLST(INDFR2,ISAT)=1.D20
            IDELF = 1
            IRETC = 1
          ELSE
            IF(((IRJECT.EQ.1).AND.(INTEPO.GT.MXOGAP)) .OR.
     1         ((IRJECT.EQ.0).AND.(IAMNEW(2).EQ.1)))  THEN
C
C SET A NEW AMBIGUITY
              CALL UPDAMB(ISAT,IEPOCH,IEPOCH,5,NNEWAM,
     1                    LSTAMB,NSAT,SVN,IPRNT2,AMSFLG,IRC)
              ABSLST(INDFRQ,ISAT)=1.D20
              IF(MRK1O2.EQ.1.AND.NFREQ.EQ.2) ABSLST(INDFR2,ISAT)=1.D20
              IRETC = 1
            ELSE
              IF(IRJECT.EQ.1) THEN
                ISVN=SVN(ISAT)
                IEPO=IEPOCH
                ABSACT(INDFRQ,ISAT)=1.D20
                IF(MRK1O2.EQ.1.AND.NFREQ.EQ.2) ABSACT(INDFR2,ISAT)=1.D20
                IF(MRK1O2.EQ.1.AND.NFREQ.EQ.2) RHS(INDFR2,ISAT)=1.D20
                IDELF = 1
                IRETC = 1
              ENDIF
            ENDIF
          ENDIF
C
C MARK ONE OR BOTH FREQUENCIES
          IF (IDELF .EQ. 1) THEN
            IF(MRK1O2.EQ.1.AND.NFREQ.EQ.2) THEN
              IFRQ1=1
              IFRQ2=2
              IFRMSG=3
            ELSE
              IFRQ1=IFREQ
              IFRQ2=IFREQ
              IFRMSG=IFREQ
            ENDIF
C
            DO 50 IFRQ=IFRQ1,IFRQ2
              CALL MRKOBS(1,ISVN,IEPO,IFRQ,NSAT,SVN,NDEL,LSTDEL)
50          CONTINUE
C
            IF(IPRNT2.GE.2) THEN
              IMSG=4
              CALL WTMSGS(IMSG,ITITL2,IEPO,ISVN,IRSAT0,
     1                    IFRMSG,IWLF0,SLIP0,XSLIP0)
            ENDIF
            IRETC=1
          END IF
10      CONTINUE
C
      ELSE
C
C DUAL FREQUENCY REJECTION
C ------------------------
C
C NEW AMIBGUITY FOR ALL SATELLITES (UNDEFINED CLOCK)?
C ---------------------------------------------------
        DO 40 ISLIP=NSLIP1,NSLIP
          IF(LSTSLP(6,ISLIP).EQ.-100)THEN
            DO 41 ISAT=1,NSAT
              ISATUP = ISAT
              CALL UPDAMB(ISATUP,TIMLST(INDFRQ,ISATUP)+1,IEPOCH,5,
     1                    NNEWAM,LSTAMB,NSAT,SVN,IPRNT2,AMSFLG,IRC)
              ABSLST(1,ISATUP)=1.D20
              ABSLST(2,ISATUP)=1.D20
              ABSLST(3,ISATUP)=1.D20
              IRETC = 1
41          CONTINUE
            RETURN
          END IF
40      CONTINUE
C
C INDEX OF REFERENCE SATELLITE IN ARRAY ABSACT:
C AND NUMBER OF REJECTIONS
        NREJ=0
        IRETC = 0
C
C COUNT BAD OBSERVATIONS
        DO 502 ISAT=1,NSAT
          IF(MRKDEL(ISAT).EQ.'Y') NREJ=NREJ+1
502     CONTINUE
        IF(NREJ.EQ.0) RETURN
C
        DO 1000 ISAT=1,NSAT
          IF(ABSACT(3,ISAT).EQ.1.D20) GOTO 1000
          IDELF = 0
C
C TIME INTERVAL OF TRIPLE DIFFERENCE OBSERVATION CONSIDERED
          INTEPO=(IEPOCH-TIMLST(INDFRQ,ISAT))*IDELTT
          SATMXI=SVN(ISAT)
          ISATUP = ISAT
          IF (INTEPO .GT. IAMNEW(3)) THEN
            CALL UPDAMB(ISATUP,TIMLST(INDFRQ,ISATUP)+1,IEPOCH,4,
     1                  NNEWAM,LSTAMB,NSAT,SVN,IPRNT2,AMSFLG,IRC)
            ABSLST(1,ISATUP)=1.D20
            ABSLST(2,ISATUP)=1.D20
            ABSLST(3,ISATUP)=1.D20
            IRETC = 1
            GO TO 1000
          END IF
          IF(MRKDEL(ISAT).NE.'Y') GOTO 1000
C
C MAX. INTERVAL EXC.
          IF(INTEPO.GT.MXOGAP) THEN
            IF(IPRNT2.GE.2) THEN
              IMSG=7
              CALL WTMSGS(IMSG,ITITL2,IEPOCH,SVN(ISAT),IRSAT0,
     1                    IFRQ3,IWLF0,SLIP0,XSLIP0)
            ENDIF
            DO 55 IFRQ=1,2
              DO 45 IMAXI=NMAXI,1,-1
                IF(IEPOCH.EQ.LSTMXI(1,IMAXI).AND.
     1             SATMXI.EQ.LSTMXI(2,IMAXI).AND.
     2             IFRQ  .EQ.LSTMXI(3,IMAXI)) GOTO 55
45            CONTINUE
              NMAXI=NMAXI+1
              IF(NMAXI.GT.MXCMXI) THEN
                WRITE(LFNERR,2000) NMAXI,MXCMXI
                CALL EXITRC(2)
              ENDIF
              LSTMXI(1,NMAXI)=IEPOCH
              LSTMXI(2,NMAXI)=SATMXI
              LSTMXI(3,NMAXI)=IFRQ
              LSTMXI(4,NMAXI)=INTEPO/IDELTT
              LSTMXI(5,NMAXI)=2
55          CONTINUE
          ENDIF
C
C OUTLIER FOUND:
          IF (FRSTOB(INDFRQ,ISAT).EQ.'Y') THEN
            ISVN=SVN(ISAT)
            IEPO=TIMLST(INDFRQ,ISAT)
            ABSLST(1,ISAT)=1.D20
            ABSLST(2,ISAT)=1.D20
            ABSLST(3,ISAT)=1.D20
            IDELF = 1
            IRETC = 1
C
C MOVE AMBIGUTIY IF AT TIMLST
            CALL UPDAMB(ISAT,IEPO,IEPO,0,NNEWAM,
     1                  LSTAMB,NSAT,SVN,IPRNT2,AMSFLG,IRC)
            IF (IRC.EQ.1) THEN
              CALL UPDAMB(ISAT,IEPOCH,IEPOCH,4,NNEWAM,
     1                    LSTAMB,NSAT,SVN,IPRNT2,AMSFLG,IRC)
            ENDIF
          ELSE
            IF(((IRJECT.EQ.1).AND.(INTEPO.GT.MXOGAP)) .OR.
     1         ((IRJECT.EQ.0).AND.(IAMNEW(2).EQ.1)))  THEN
C
C SET A NEW AMBIGUITY
              CALL UPDAMB(ISAT,IEPOCH,IEPOCH,5,NNEWAM,
     1                    LSTAMB,NSAT,SVN,IPRNT2,AMSFLG,IRC)
              ABSLST(1,ISAT)=1.D20
              ABSLST(2,ISAT)=1.D20
              ABSLST(3,ISAT)=1.D20
              IRETC = 1
            ELSE
              IF(IRJECT.EQ.1) THEN
                ISVN=SVN(ISAT)
                IEPO=IEPOCH
                ABSACT(1,ISAT)=1.D20
                ABSACT(2,ISAT)=1.D20
                ABSACT(3,ISAT)=1.D20
                IDELF = 1
                IRETC = 1
              ENDIF
            ENDIF
          ENDIF
C
C MARK BOTH FREQUENCIES
          IF(IDELF.EQ.1) THEN
            DO 70 IFRQ=1,2
              CALL MRKOBS(2,ISVN,IEPO,IFRQ,NSAT,SVN,NDEL,LSTDEL)
70          CONTINUE
C
            IF(IPRNT2.GE.2) THEN
              IMSG=5
              CALL WTMSGS(IMSG,ITITL2,IEPO,ISVN,IRSAT0,
     1                    IFRQ3,IWLF0,SLIP0,XSLIP0)
            ENDIF
          ENDIF
1000    CONTINUE
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
