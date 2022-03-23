      MODULE s_DETSLP
      CONTAINS

C*
      SUBROUTINE DETSLP(LFNOBS,IPRNT2,RMSMAX,JMPOPT,TOLJMP,IRESID,
     1                  IREFIL,NFREQ ,NFRAUX,FRQAUX,NSAT  ,SVN   ,
     2                  NEP   ,LASTFL,PAR   ,SIGL12,IWLSCR,TIMREF,
     3                  IDELTT,NEPOCH,SWIDTH,MRK1O2,IRJECT,MXOGAP,
     4                  MXIOND,MINCYC,STANAM,NSLIP ,LSTSLP,SLPLST,
     5                  SLPXXX,IONO  ,NDEL  ,LSTDEL,NMAXI ,LSTMXI,
     6                  IAMNEW,NNEWAM,LSTAMB,NTONLY,L5CLEA,AMSFLG,
     7                  LTRIP ,SIGWGS,KINSTA,NDIFF ,MEATYP,TIMLST,
     8                  CLKFLG,NCLKEV,LSTCLK)
CC
CC NAME       :  DETSLP
CC
CC PURPOSE    :  DETERMINE ALL CYCLE SLIPS USING SINGLE
CC               OR DUAL FREQUENCY OBSERVATIONS
CC
CC PARAMETERS :
CC         IN :  LFNOBS : UNIT NUMBER WITH OBS-EQNS           I*4
CC               IPRNT2 : PRINT LEVEL FOR CYCLE SLIP DETECT.  I*4
CC                        =0: NO MESSAGES PRINTED
CC                        =1: PRINT SUMMARY
CC                        =2: ALL MESSAGES PRINTED
CC               RMSMAX : MAX RMS ALLOWED, OTHERWISE SOL.FLAG R*8
CC               JMPOPT : CLOCK EVENT OPTIONS                  I*4(6)
CC                         (1): 0/1 ALLOW MS-JUMP CYCLE SLIPS
CC                         (2): MIN. SIZE OF A CLOCK EVENT (NS)
CC                         (3): MARK EPOCHS WITH CLOCK EVENTS
CC                              UP TO (IN S)
CC                         (4): 0/1 AMBIGUITIES FOR ALL SATELLITES
CC                         (5): 0/1 FLAG IF MS-JUMP IN FILE
CC                         (6): 0/1 FLAG IF A CLOCK EVENT IN FILE
CC               TOLJMP : TOLERANCE TO DETECT A MS-JUMP       R*8
CC               IRESID : SAVE TRIPLE DIFFERENCE RESIDUALS    I*4
CC                        ON FILE (YES=1)
CC               IREFIL : FILE NUMBER IN RESIDUAL FILE        I*4
CC               NFREQ  : NUMBER OF FREQUENCIES IN FILE       I*4
CC               NFRAUX : NUMBER OF FREQUENCIES ON AUX.-FILE  I*4
CC               FRQAUX(I),I=1,2,..,NFRAUX: FREQUENCIES       I*4
CC               NSAT   : NUMBER OF SATELLITES                I*4
CC               SVN(I),I=1,2,..,NSAT: SV NUMBERS             I*4
CC               NEP    : TOTAL NUMBER OF EPOCHS              I*4
CC               PAR(K),K=1,2,3: TRIPLE DIFF COORD. SOLUTION  R*8
CC               SIGL12(L),L=1,2: MEAN ERROR OF OBSERVATION   R*8
CC                        IN CARRIER L
CC               IWLSCR(L),L=1,2: WAVELENGTH FACTOR           I*4
CC               TIMREF : REFERENCE EPOCH OF THE FILE         R*8
CC               IDELTT : TIME INTERVAL BETWEEN SUBSEQUENT    I*4
CC                        OBSERVATION EPOCHS
CC               NEPOCH : NUMBER OF EPOCHS IN  FILE            I*4
CC               SWIDTH(K),K=1,2: NUMBER OF NEAREST INTEGERS   I*4
CC                        TO BE TESTED
CC                         K=1 : IN L1/L2
CC                         K=2 : IN L5
CC               MRK1O2 : FLAG TO MARK UNPAIRED L1/L2 OBSERVA- I*4
CC                        TIONS
CC                         =0 : NO MARKING DONE
CC                         =1 : L1 WITHOUT L2, OR L2 WITHOUT L1
CC                              OBSERVATIONS ARE MARKED
CC               IRJECT : OUTLIER REJECTION (YES=1,NO=0)       I*4
CC               MXOGAP : MAXIMUM OBSERVATION GAP ALLOWED IN   I*4
CC                        OUTLIER REJECTION (SEC)
CC               MXIOND : MAXIMUM IONOSPHERE CHANGE BETWEEN    I*4
CC                        EPOCHS (IN % OF L1 CYCLES) FOR OUT-
CC                        LIER REJECTION
CC               MINCYC : ACCEPT CYCLE SLIPS > "MINCYC" CYCLES I*4
CC               IAMNEW(I),I=1,2,3: SETTING OF NEW AMBIGUITIES I*4
CC                        I=1 : USE CYCLE SLIP FLAG (0/1)
CC                        I=2 : IF PROBLEM IN SLIP-FIXING (0/1)
CC                        I=3 : AFTER GAP LARGER THAN (SEC)
CC                        I=4 : USE AMBIGUITIES FROM FILE (0/1)
CC                        I=5 : MIN. TIME OF OBS. PER AMB. (SEC)
CC               NTONLY : TEST OBS. WITH CYCLE SLIP FLAG       I*4
CC                          ONLY (0/1)
CC               L5CLEA : L5 IS CLEAN (EXCEPT FLAGGED          I*4
CC                          EPOCHS)  (0/1)
CC               LASTFL(IFRQ,ISAT) : ARRAY FOR SUBROUTINE      I*4
CC                    CYCEPO FROM SUBROUTINE PREPHA
CC               LTRIP  : CARRIER TO BE USED FOR TRIPLE        I*4
CC                        DIFFERENCE SOLUTION
CC               SIGWGS(I),I=1,2,3: A PRIORI WEIGHTS FOR       R*8
CC                        COORDINATES
CC               KINSTA : KINEMATIC COORDINATE ESTIMATION       I*4
CC               NDIFF:   0: ZERO/ 1: SIGNLE DIFFERENCE FILES   I*4
CC               MEATYP : MEASUREMENT TYPE                     I*4
CC                        =1: PHASE OBSERVATIONS
CC                        =2: CODE OBSERVATIONS
CC                        =3: RANGE OBSERVATIONS
CC               TIMLST(K,ISAT),K=1,2,3, ISAT=1,2,..,NSAT:     I*4
CC                        LAST OBS. EPOCH
CC               STANAM : STATION NAME                        CH*16
CC        OUT :  NSLIP  : NUMBER OF SLIPS DETECTED            I*4
CC               LSTSLP(K,IS),K=1,..,6, IS=1,2,..,NSLIP:      I*4
CC                        SLIP DESCRIPTION
CC                        K=1: EPOCH NUMBER
CC                        K=2: SV NUMBER
CC                        K=3: FREQUENCY NUMBER
CC                        K=4: WAVELENGTH FACTOR
CC                        K=5: WAVELENGTH FACTOR OF L5
CC                             IF AN L1 AND L2 SLIP OCCURS AT
CC                             THE SAME EPOCH FOR THE SAME
CC                             SATELLITE:
CC                              =3 FOR L1, INDICATING THAT
CC                                 AN L2 SLIP OCCURED TOO
CC                              =WAVELENGTH OF L5 FOR L2
CC                        K=6: DETECTED BY
CC                             =1: SINGLE FREQ. SLIP DETECTION
CC                             =2: DUAL FREQ. SLIP DETECTION
CC                             =3: CLOCK
CC                             =4: USER
CC                             ALL CYCLE SLIPS DETECTED IN THE
CC                             LATEST RUN HAVE A NEGATIVE SIGN
CC               SLPLST(IS),IS=1,2,..,NSLIP: SIZE OF SLIP IS  R*8
CC               SLPXXX(2,I),I=1,..,NSLIP: REAL VALUED        R*8
CC                        ESTIMATES FOR SLIPS - INTEGER SLIP
CC                        SINGLE FREQ.: L1/L2 ESTIMATES
CC                        DUAL   FREQ.: L1/L2 AND L5 ESTIMATES
CC               IONO(K,I),K=1,2,3, I=1,2,..,NSLIP            R*8
CC                        K=1: IONOSPHERE MEAN FROM L1, L2
CC                        K=2: IONOSPHERE DIFF. FROM L1, L2
CC                        K=3: RESIDUAL IN L3
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
CC                               =6: SMALL PIECE
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
CC    IN/OUT :   NNEWAM(ISATEL) : NUMBER OF AMBIGUITIES        I*4
CC               LSTAMB(I,ISATEL,IAMB)  LIST OF AMBIGUITIES    I*4
CC                        I=1 : THE FIRST EPOCH
CC                        I=2 : TYPE: 1 ... FILE
CC                                    2 ... CYCLE SLIP
CC                                    3 ... USER
CC                                    4 ... GAP
CC                                    5 ... PREPROCESSING PROBLEM
CC                                    6 ... CLOCK EVENT
CC                        I=3 : THE LAST EPOCH WITH OBSERVATIONS
CC               AMSFLG : INDICATER OF MAXAMS EXCEEDINGS      CH*1
CC               CLKFLG : CLOCK FLAG                          CH*1
CC                        'G': OK, NO EVENT
CC                        'J': MS-JUMP, REPAIRED AS CYCLE SLIP
CC                        'B': OTHER BIG CLOCK VALUE
CC               NCLKEV : NUMBER OF CLOCK EVENTS IN LIST      I*4
CC               LSTCLK : (1,I): EPOCH OF THE CLOCK EVENT
CC                        (2,I): MAGNITUDE OF THE CLOCK EVENT
CC                               IN NANOSECONDS
CC                        (3,I): CLOCK ESTIMATE FROM CODSPP
CC                               IN NANOSECONDS
CC                        (4,I): ACTION (0:NONE,1:MARK,2:AMB)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER, L.MERVART
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  88/04/30 11:09
CC
CC CHANGES    :  05-JUN-92 : ??: CHANGES FOR THE NEW MAUPRP VERSION
CC               23-DEC-92 : ??: DECLARATION OF "MXNSAT"
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               10-AUG-94 : MR: CALL EXITRC
CC               24-APR-95 : MR: UPDATE DESCRIPTION OF LSTDEL
CC                5-MAR-96 : TS: HANDLING OF "MAXAMS" EXCEEDINGS
CC               20-JAN-97 : MR: ADD "RHS" TO THE CALL OF REJECT
CC               21-JAN-97 : MR: REMOVE "NSAT","SVN","RHS" FROM CALL
CC                               OF RSTCLK
CC               13-FEB-97 : JJ: REMOVE LEFT OVER DEBUGGING SCHROTT FROM MR
CC               23-SEP-97 : DI: USE MAXSAT.inc
CC               14-OCT-98 : HH: MODIFICATIONS GLONASS
CC               16-JAN-02 : DS: ZERO DIFFERENCE PROCESSING
CC               18-JUN-02 : RD: CLOCK EST. FOR ZD ONLY
CC               19-JUN-02 : RD: NEW CALL FOR SR INITRP
CC               26-JUN_02 : RD: ADD PROGRAM OUTPUT FOR EPOCH SOLUTIONS
CC               09-AUG-02 : RD: ADD STANAM TO UPDTRP FOR OUTPUT
CC               14-AUG-02 : RD: APRIORI VALUE FOR EPOCH CLOCK
CC                               DELETE COMPLETE EPOCH IF TOO FEW OBS.
CC               07-OCT-02 : RD: PREVENT INDEX==0 FOR SINGE DIFF CASE
CC                               ISVREF REMOVED FROM CALL REJECT (NOT USED)
CC               04-NOV-02 : RD: MESSAGE IF LAST OBSERVATION REMOVED
CC               13-NOV-02 : RD: MAKE KINEMATIC BASELINES WORK
CC               18-NOV-02 : RD: SLIGHTLY MODIFIED "MARK LAST OBS." PROCEDURE
CC               04-DEC-02 : RD: ADD CLOCK EVENT FLAG AND LIST
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               29-FEB-08 : RD: CHECK FOR EPOCH-BY-EPOCH "CYCLE SLIP"
CC               01-OCT-08 : HB: SINGLE-FREQUENCY PRE-PROCESSING ADAPTED
CC               02-JUL-10 : RD: NCLKEV MAY BE EXCEED MXCCLK
CC               19-JUL-10 : SL: TAB CHARACTERS REMOVED
CC               29-FEB-12 : RD: CORRECT ARRAY DIMENSIONS OF IWLF0 AND XSLIP0
CC               26-MAR-12 : RD: USE WTMSGS AS MODULE NOW
CC               26-MAR-12 : RD: REMOVE UNUSED VARIABLES FROM DUASLP
CC               10-JUL-12 : RD: REMOVE UNUSED VARIABLES FROM SNGSOL
CC               17-SEP-12 : HB: AVOID CALL OF SNGSOL IF NO OBSERVATIONS ARE
CC                               AVAILABLE
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnprt, lfnerr
      USE m_maxdim, ONLY: MAXSAT
      USE d_const, ONLY: C
      USE s_reject
      USE s_sngsol
      USE s_mrkobs
      USE s_cycepo
      USE s_wtmsgs
      USE s_lincom
      USE s_rstclk
      USE s_mpprep
      USE s_initrp
      USE s_ambupd
      USE s_sngslp
      USE s_dspcyc
      USE s_dspmxi
      USE s_dspdel
      USE s_nxtepo
      USE s_updsng
      USE s_exitrc
      USE s_dspclk
      USE s_duaslp
      USE s_clkevt
      USE s_chkslp
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IAMB  , ICYFRQ, ICYSVN, ICYTYP, IDEFRQ, IDEL  ,
     1          IDELTT, IDESVN, IDETYP, IEP   , IEP0  , IEPO  , IEPOCH,
     2          IFRQ  , IFRQ3 , II    , IK    , IMSG  , INDFRQ, INEW  ,
     3          IPRNT2, IRC   , IREF  , IREFIL, IRESID, IRESSV, IRETC ,
     4          IRJECT, IRSAT0, ISAT  , ISAT11, ISUMRY, ISVN  , ITITL2,
     5          ITITLE, IZERO , K     , KINSTA, L5CLEA, LFNOBS,
     6          LTRIP , MEATYP, MINCYC, MINDOF, MRK1O2, MXCSAT, MXIOND,
     7          MXOGAP, NCLKEV, NCNSTR, NCOBS , NDEL  , NDIFF , NDUAEP,
     8          NEP   , NEPOCH, NFRAUX, NFREQ , NITER , NMARK , NMAXI ,
     9          NMAXI1, NPAR  , NSAT  , NSATEP, NSLIP , NSLIP1, NSNG  ,
     1          NSTART, NSTEP , NTONLY, ISLIP , MSLIP , MXCCLK
C
      REAL*8    CYCMAX, EPCLK0, EPOCLK, RESCHK, RESMAX, RMSMAX, RMSSUM,
     1          SNGPOS, STRP1 , STRP2 , STRP3 , TIMREF, TOLJMP
C
CCC       IMPLICIT REAL*8    (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
C
      CHARACTER*1  FRSTOB(3,MAXSAT),MRKDEL(MAXSAT),FILFLG(3,MAXSAT)
      CHARACTER*1  AMSFLG,MRKSVN(MAXSAT)
      CHARACTER*1  FLGCOM,FLGLC,SNGFLG,CLKFLG
      CHARACTER*6  MXNSAT,MXNCLK
      CHARACTER*16 STANAM(2)
      CHARACTER*80 LINE,LINE1,LINE2
C
      INTEGER*4    SVN(*),LSTSLP(6,*),SVNEP(MAXSAT),INDSAT(MAXSAT)
      INTEGER*4    FRQAUX(*),IWLSCR(*),IAMNEW(5),JMPOPT(6)
      INTEGER*4    LSTDEL(5,*),LSTMXI(5,*),SWIDTH(2),LSTCLK(4,*)
      INTEGER*4    TIMLST(3,*),NONZER(3),LASTFL(2,*)
      INTEGER*4    LSTAMB(3,MXCSAT,*),NNEWAM(MXCSAT)
      INTEGER*4    NSOL(4),IEPMAX(4),IWLF0(3)
C
      REAL*8       PAR(*),SLPLST(*),SLPXXX(2,*),IONO(3,*)
      REAL*8       ABSLST(3,MAXSAT),ABSACT(3,MAXSAT),SIGL12(*)
      REAL*8       RHS(3,MAXSAT),CLKOLD(3),CLOCK(3)
      REAL*8       AOBS(3,MAXSAT),AOBSVAL(4)
      REAL*8       ANOR(10),BNOR(4),SIGWGS(3),RMSSNG,RHSLC
      REAL*8       PARSNG(4),SIGPAR(4)
      REAL*8       DELTAT(2,2)
      REAL*8       SUMMAX(6),SLIP0(3),XSLIP0(3)
C
      INCLUDE 'COMFREQ.inc'
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMCLK/MXCCLK,MXNCLK
C
      DATA IFRQ3/3/,IRSAT0/0/
C
C INIT VARIABLES
C --------------
      IWLF0 =0
      SLIP0 =0.D0
      XSLIP0=0.D0
C
C CHECK NUMBER OF SATELLITES
C --------------------------
      IF(NSAT.GT.MAXSAT)THEN
        WRITE(LFNERR,5) NSAT,MAXSAT
5       FORMAT(/,' *** SR DETSLP: TOO MANY SATELLITES',/,
     1                       16X,'NUMBER OF SATELLITES:',I3,/,
     2                       16X,'MAX. NUMBER ALLOWED :',I3,/)
        CALL EXITRC(2)
      END IF
C
C REWIND UNIT WITH OBS EQNS
C -------------------------
      REWIND LFNOBS
C
C NUMBER OF CONSTRAINTS
C ---------------------
      NCNSTR=0
      IF (KINSTA.EQ.1) THEN
        DO I=1,3
          IF (SIGWGS(I).NE.0.D0) NCNSTR=NCNSTR+1
        END DO
      ENDIF
C
C INIT CLOCK FLAG
C ---------------
      CLKFLG='G'
C
C INITIALIZE TITLE FLAG
C ---------------------
      IF(IPRNT2.GE.2) ITITL2=1
C
C INITIALIZE L3 SIGMA (SEE DUASLP)
C -------------------------------
      IF (NDIFF.EQ.1) THEN
        STRP1=2*DSQRT(2.D0)*SIGL12(1)
        STRP2=2*DSQRT(2.D0)*SIGL12(2)
      ELSE
        STRP1=2*SIGL12(1)
        STRP2=2*SIGL12(2)
      END IF

      STRP3=DSQRT((FACLIN(3,1,SVN(1))*STRP1)**2+
     1            (FACLIN(3,2,SVN(1))*STRP2)**2)
C
C INITIALIZE LAST ARRAY CONTAINING THE LAST RESIDUAL
C AND TIMLST
C --------------------------------------------------
      DO IFRQ=1,3
        CLOCK(IFRQ)=0.D0
        DO ISAT=1,NSAT
          ABSLST(IFRQ,ISAT)=1.D20
          TIMLST(IFRQ,ISAT)=-1
        ENDDO
      ENDDO
C
C INITIALIZE ARRAY WITH CYCLE SLIP FLAGS
      DO 94 ISAT=1,NSAT
        DO 95 IFRQ=1,NFRAUX
          FILFLG(IFRQ,ISAT) = 'O'
95      CONTINUE
94    CONTINUE
C
C STARTING NUMBER FOR NEW CYCLE SLIPS OF CURRENT ITERATION
C --------------------------------------------------------
      NSTART=NSLIP+1
C
C INIT THE SUMMARY STATISTIC
C --------------------------
      NSOL(1:4)=0
      RMSSUM=0D0
      IEPMAX(1:3)=0
      SUMMAX(1:6)=0D0
      MINDOF=0
      EPOCLK=0D0
C
C READ OBSERVATIONS OF FIRST EPOCH, DEFINE ACTUAL CLOCK
C -----------------------------------------------------
      DO 20 IEP0=1,NEP
        CALL NXTEPO(LFNOBS,NSAT  ,SVN   ,PAR   ,NFRAUX,FRQAUX,
     1              NSTART,NSLIP ,LSTSLP,SLPLST,IAMNEW,IDELTT,
     2              IPRNT2,IEPOCH,NSATEP,SVNEP ,DELTAT,INDSAT,
     3              ABSACT,NDEL  ,LSTDEL,NNEWAM,LSTAMB,ABSLST,
     4              TIMLST,FILFLG,AMSFLG,AOBS  ,NFREQ)
C
C MOVE THE CYCLE SLIP FLAGS INTO THE NEXT EPOCH IF NECESSARY
        DO 21 IDEL=1,NDEL
          IF ((LSTDEL(2,IDEL) .LE. IEPOCH) .AND.
     1        (LSTDEL(3,IDEL) .GE. IEPOCH)) THEN
            DO 22 ISAT=1,NSAT
              IF (LSTDEL(1,IDEL) .EQ. SVN(ISAT)) THEN
                DO 23 IFRQ=1,NFRAUX
                  IF (((LSTDEL(4,IDEL).EQ.IFRQ).OR.(IFRQ.GT.2)) .AND.
     1                (FILFLG(IFRQ,ISAT).EQ.'C')) FILFLG(IFRQ,ISAT)='M'
23              CONTINUE
              END IF
22          CONTINUE
          END IF
21      CONTINUE
C
        IF(NSATEP.GT.1) GOTO 30
C
20    CONTINUE
30    IEP0=IEP0+1
C
C UPDATE ABSLST
C -------------
      DO 40 IFRQ=1,NFRAUX
        NONZER(IFRQ)=NSATEP
40    CONTINUE
      CALL UPDSNG(IEPOCH,NSAT,NFRAUX,NONZER,NDIFF,CLOCK,ABSACT,
     1            ABSLST,TIMLST,FRSTOB)
C
C LOOP OVER REMAINING NEP-1 EPOCHS
C --------------------------------
      DO IEP=IEP0,NEP
C
C STARTING NUMBER FOR NEW CYCLE SLIPS OF CURRENT EPOCH
        NSTEP=NSLIP+1
        CLKOLD(1)=CLOCK(1)
        CLKOLD(2)=CLOCK(2)
        CLKOLD(3)=CLOCK(3)
C
C DEFINE ABSOLUTE VALUES AND CLOCK OF CURRENT EPOCH
        CALL NXTEPO(LFNOBS,NSAT  ,SVN   ,PAR   ,NFRAUX,FRQAUX,NSTART,
     1              NSLIP ,LSTSLP,SLPLST,IAMNEW,IDELTT,IPRNT2,IEPOCH,
     2              NSATEP,SVNEP ,DELTAT,INDSAT,ABSACT,NDEL  ,LSTDEL,
     3              NNEWAM,LSTAMB,ABSLST,TIMLST,FILFLG,AMSFLG,AOBS  ,
     4              NFREQ)
        IF(NSATEP.LE.1) GO TO 499
C
C SINGLE FREQUENCY MODE: CYCLE SLIP AND OUTLIER DETECTION
C ------------------------------------------------------
        IF(NFRAUX.LE.2) THEN
C
C
C START OF ITERATION
          LINE1=' '
          LINE2=' '
          NITER=0
          NDUAEP=0
50        CONTINUE
          SNGFLG='B'
          MRKSVN(:)='G'
          NMARK =0
          EPOCLK=0D0
          EPCLK0=0D0
450       CONTINUE
C LOOP OVER FREQUENCY(IES)
          DO 200 IFRQ=1,NFRAUX
C
C INDEX FOR FIRST CYCLE SLIP BEING DETECTED
            NSLIP1=NSLIP+1
            NMAXI1=NMAXI+1
            NITER=NITER+1
C KIN NEED CLOCKS IN ANY CASE...
            IF (KINSTA.EQ.1) THEN
              NPAR=4
C
C ZD: NEEDS ONLY CLOCKS
            ELSE IF (NDIFF .EQ.0) THEN
              NPAR=1
            ELSE
C
C NO EPOCH SOLUTION NECESSARY
              NPAR = 0
            ENDIF
C
            IF (NPAR.GT.0) THEN
              CALL INITRP(NPAR  ,LTRIP ,NDIFF ,SIGL12,SIGWGS,
     1                    NSNG  ,RMSSNG,ANOR  ,BNOR  )
              FLGLC=CHAR(0)
            ENDIF
C
C COMPUTE SINGLE DIFFERENCE RESIDUALS
            NONZER(IFRQ)=0
            DO ISAT=1,NSAT
              RHS(IFRQ,ISAT)=1.D20
              IF(ABSACT(IFRQ,ISAT).NE.1.D20) THEN
                NONZER(IFRQ)=NONZER(IFRQ)+1
                IF(ABSLST(IFRQ,ISAT).NE.1.D20) THEN
                  RHS(IFRQ,ISAT)=ABSACT(IFRQ,ISAT)-ABSLST(IFRQ,ISAT)
                  RHS(IFRQ,ISAT)=RHS(IFRQ,ISAT)+EPCLK0*C/1D9
                ENDIF
              END IF
C FORM LINEAR COMBINATION FOR SINGLE DIFFERENCE SOLUTION
C ------------------------------------------------------
              IF (NPAR.GT.0.AND.
     1          RHS(IFRQ,ISAT).NE.1.D20.AND. (MRKSVN(ISAT).EQ.'G')) THEN
                RHSLC=RHS(IFRQ,ISAT)
C
C IN ORDER TO HAVE "O-C" CHANGE THE SIGN OF RESIDUAL
                IF (NDIFF.EQ.0.AND.MEATYP.EQ.1) THEN
                  RHSLC=-RHSLC
                ENDIF
C
C SINGLE DIFFERENCE PARTIALS
C --------------------------
                IF (KINSTA.EQ.1) THEN
                  AOBSVAL(1:3)=AOBS(1:3,ISAT)
                  AOBSVAL(4)=C/1.D9
                ELSE
                  AOBSVAL(1)=C/1.D9
                END IF
C
C UPDATE SINGLE NORMAL EQUATIONS
C ------------------------------
                NSNG=NSNG+1
                RMSSNG=RMSSNG+RHSLC**2
                DO I=1,NPAR
                  BNOR(I)=BNOR(I)+AOBSVAL(I)*RHSLC
                  DO K=1,I
                    IK=K+(I-1)*I/2
                    ANOR(IK)=ANOR(IK)+AOBSVAL(I)*AOBSVAL(K)
                  END DO
                END DO
C
C KEEP SATELLITE INDEX FOR LATER USE (MARK ONE OBS. ONLY)
                ISAT11=ISAT
              ELSE
C              RHS(1:NFRAUX,ISAT)=1.D20
              END IF
            ENDDO
200       CONTINUE

C NOT ENOUGH OBSERVATIONS
C -----------------------
          IF(NONZER(NFRAUX).LE.NDIFF) GOTO 499
C
C MAKE AN EPOCH SOLUTION
C ----------------------
          IF (NPAR.GT.0 .AND.
     1        (NSNG.GE.NPAR .OR. NCNSTR.GT.0) .AND. NSNG.NE.0) THEN
C
C SOLVE NORMAL EQUATION
C ---------------------
C
C CASE WITH CONSTRAINTS
C ---------------------
            IF (NSNG.LT.NPAR) THEN
              NCOBS=NSNG+NCNSTR
            ELSE
              NCOBS=NSNG
            END IF
            CALL SNGSOL(STANAM,RMSMAX,LTRIP,NSNG,NPAR,ANOR,BNOR,
     1                  IEPOCH,RMSSNG,SNGFLG,PARSNG,SIGPAR,LINE1)
            IF (IPRNT2.GE.2.AND.LEN_TRIM(LINE1).GT.0)
     1        WRITE(LFNERR,'(A)') TRIM(LINE1)
C
C Check est. clock value, repeat with other apriori clock
C -------------------------------------------------------
            LINE2=' '
            IF (SNGFLG.EQ.'G' .OR. SNGFLG.EQ.'R'
     1          .OR. (SNGFLG.EQ.'B'.AND. NSNG.GE.NPAR)) THEN
              IF (NDIFF.EQ.0.AND.DABS(PARSNG(NPAR))>1D4) THEN
                EPCLK0=EPCLK0+PARSNG(NPAR)
C
C GENERATE A MESSAGE LINE
                LINE=' ### SR DETSLP: EPOCH,UPDATED APRIORI CLOCK: '
                WRITE(LINE(46:80),'(I6,F16.3,1X,A)')
     1                IEPOCH,EPCLK0,'(' // STANAM(1)(1:4) // ')'
                IF (DABS(EPCLK0).GE.1D9) THEN
                  WRITE(LINE(52:67),'(E16.6)') EPCLK0
                ENDIF
                IF (LEN_TRIM(STANAM(2)).GT.0) THEN
                  WRITE(LINE(74:80),'(A)') '-' // STANAM(2)(1:4) // ')'
                ENDIF
                IF (IPRNT2.GE.2) WRITE(LFNERR,'(A)') TRIM(LINE)
                IF (DABS(EPCLK0)>1D4) LINE2=LINE
                GOTO 450
              ENDIF
            ENDIF
C
C CORRECT "O-C" FOR SNG ESTIMATES
C -------------------------------
            IF (SNGFLG.EQ.'G' .OR. SNGFLG.EQ.'R'
     1          .OR. (SNGFLG.EQ.'B'.AND. NSNG.EQ.NPAR)) THEN
              RESCHK=0.D0
              DO ISAT=1,NSAT
                DO IFRQ=1,NFRAUX
                  IF(RHS(IFRQ,ISAT).NE.1.D20) THEN
                    IF (KINSTA.EQ.1) THEN
                      AOBSVAL(1:3)=AOBS(1:3,ISAT)
                      AOBSVAL(4)=C/1.D9
                    ELSE
                      AOBSVAL(1)=C/1.D9
                    END IF
                    IF (NDIFF.EQ.0.AND.MEATYP.EQ.1) THEN
                      RHS(IFRQ,ISAT)=RHS(IFRQ,ISAT)+
     1                  DOT_PRODUCT(PARSNG(1:NPAR),AOBSVAL(1:NPAR))
                    ELSE
                      RHS(IFRQ,ISAT)=RHS(IFRQ,ISAT)-
     1                  DOT_PRODUCT(PARSNG(1:NPAR),AOBSVAL(1:NPAR))
                    ENDIF
                  ENDIF

                  IF(RHS(IFRQ,ISAT).NE.1.D20.AND.
     1                        MRKSVN(ISAT).EQ.'G') THEN
                    RHSLC=-RHS(IFRQ,ISAT)
                    IF (DABS(RHSLC).GT.RESCHK) THEN
                      RESCHK=DABS(RHSLC)
                      IRESSV=ISAT
                    END IF
                  END IF
                END DO
              END DO
C
C MARK RESIDUAL
C -------------
              RESMAX=5.D0*STRP1
              IF (RESCHK.GE.RESMAX) THEN
                MRKSVN(IRESSV)='B'
C
                IF (IPRNT2.GE.2) THEN
                  CALL MPPREP(IPRNT2,ITITL2,IEPOCH,NITER ,
     1                        NSNG  ,NMARK ,NDUAEP,EPCLK0,RMSSNG,
     2                        NPAR  ,PARSNG,SIGPAR,SNGFLG)
                ENDIF
C
                NMARK=NMARK+1
                GOTO 450
              END IF
C
C SAVE BACK THE RESULTING EPOCH CLOCK ESTIMATE
C --------------------------------------------
              EPOCLK = parsng(NPAR)+epclk0
              DO ISAT=1,NSAT
                DO IFRQ=1,NFRAUX
                  IF(RHS(IFRQ,ISAT).NE.1.D20) THEN
                    IF (NDIFF.EQ.0.AND.MEATYP.EQ.1) THEN
                      RHS(IFRQ,ISAT)=RHS(IFRQ,ISAT)-EPOCLK*C/1D9
                    ELSE
                      RHS(IFRQ,ISAT)=RHS(IFRQ,ISAT)+EPOCLK*C/1D9
                    ENDIF
                  ENDIF
                ENDDO
              ENDDO
            END IF
C
C WRITE A PROGRAM OUTPUT LINE
C ---------------------------
            IF (IPRNT2.GE.2) THEN
              CALL MPPREP(IPRNT2,ITITL2,IEPOCH,NITER ,
     1                    NSNG  ,NMARK ,NDUAEP,EPCLK0,RMSSNG,
     2                    NPAR  ,PARSNG,SIGPAR,SNGFLG)
            ENDIF
          ENDIF
C
C SAVE THE RESULTING EPOCH CLOCK ESTIMATE
C --------------------------------------
          IF (NDIFF.EQ.0 .AND. SNGFLG.EQ.'G') THEN
            EPOCLK = parsng(NPAR)+epclk0
          ELSE
            EPOCLK = 0D0
          ENDIF
C CYCLE SLIP DETECTION
          DO IFRQ=1,NFRAUX
            IREF=0
            IZERO=1
            CALL SNGSLP(IEPOCH,NSAT,SVN,IFRQ,FRQAUX,IREF,IWLSCR,RHS,
     1                  IZERO,IPRNT2,ITITL2,MINCYC,
     2                  NSLIP,LSTSLP,SLPLST,SLPXXX,CLOCK,FILFLG,
     3                  NTONLY,L5CLEA,SIGL12)
C
C OUTLIER REJECTION
            CALL REJECT(IPRNT2,ITITL2,IRJECT,MXOGAP,MXIOND,NSAT  ,
     1                  SVN   ,IFRQ  ,FRQAUX,NFREQ ,MRK1O2,
     2                  MINCYC,ABSACT,ABSLST,RHS   ,MRKDEL,IDELTT,
     3                  IEPOCH,TIMLST,FRSTOB,NSLIP1,NSLIP ,LSTSLP,
     4                  SLPLST,SLPXXX,NDEL  ,LSTDEL,NMAXI ,LSTMXI,
     5                  IAMNEW,NNEWAM,LSTAMB,AMSFLG,IRETC )
C
C OUTLIER(S) REJECTED: RESTART ITERATION
            IF(IRETC.NE.0)THEN
              NSLIP=NSLIP1-1
              NMAXI=NMAXI1-1
              CLOCK(IFRQ)=CLKOLD(IFRQ)
              NDUAEP=NDUAEP+1
              GOTO 50
            END IF
          ENDDO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C Mark a single Observations
          IF (NPAR.GT.0.AND.NSNG+NCNSTR.LE.NPAR) THEN
            IF (IPRNT2.GE.2) THEN
              WRITE(LFNPRT,'(A,/,17X,A,I5,A,/)') ' EPOCH SOLUTION: '//
     1              'NO SUITABLE EPOCH SOLUTION AVAILABLE.',
     2              'ALL OBSERVATIONS OF THE EPOCH ',IEPOCH,' MARKED'
            ENDIF
C
            DO ISAT=1,NSAT
              DO 601 IFRQ=1,2
                IF (IFRQ.GT.NFRAUX) EXIT
                IF (RHS(IFRQ,ISAT).EQ.1.D20) CYCLE
C
                ISVN=SVN(ISAT)
                IEPO=IEPOCH
                ABSACT(1,ISAT)=1.D20
                ABSACT(2,ISAT)=1.D20
                ABSACT(3,ISAT)=1.D20
C
                CALL MRKOBS(2,ISVN,IEPO,IFRQ,NSAT,SVN,NDEL,LSTDEL)
601           CONTINUE
C
              IF(IPRNT2.GE.2) THEN
                IMSG=4
                CALL WTMSGS(IMSG,ITITL2,IEPO,ISVN,IRSAT0,
     1                      IFRQ3,IWLF0,SLIP0,XSLIP0)
              ENDIF
            ENDDO
          ENDIF
C
C WRITE WARNINGS FROM PROCESSING OF THIS EPOCH SOLUTION
C -----------------------------------------------------
          IF (IPRNT2.LT.2 .AND. LEN_TRIM(LINE1).GT.0)
     1      WRITE(LFNERR,'(A)') TRIM(LINE1)
          IF (IPRNT2.LT.2 .AND. LEN_TRIM(LINE2).GT.0)
     1      WRITE(LFNERR,'(A)') TRIM(LINE2)
C
C UPDATE SUMMARY STATISTIC
C ------------------------
          IF (SNGFLG.EQ.'G') THEN
            NSOL(1)=NSOL(1)+1
          ELSE IF (SNGFLG.EQ.'R') THEN
            NSOL(2)=NSOL(2)+1
          ELSE IF (SNGFLG.EQ.'B' .AND. NSNG.LE.NPAR) THEN
            NSOL(3)=NSOL(3)+1
          ELSE IF (SNGFLG.EQ.'B') THEN
            NSOL(4)=NSOL(4)+1
          ENDIF
C A GOOD OR RMS SOLUTION FOUND:
          IF (SNGFLG.EQ.'G') THEN
            RMSSUM=RMSSUM+RMSSNG
C CHECK MAX. RMS:
            IF (IEPMAX(1).EQ.0.OR.SUMMAX(1).LT.RMSSNG) THEN
              IEPMAX(1)=IEPOCH
              SUMMAX(1)=RMSSNG
            ENDIF
C CHECK MAX. CLOCK CORRECTION:
            IF (NDIFF.EQ.0.OR.KINSTA.EQ.1) THEN
              IF (IEPMAX(2).EQ.0.OR.
     1            DABS(SUMMAX(2)).LT.DABS(EPCLK0+PARSNG(NPAR))) THEN
                IEPMAX(2)=IEPOCH
                SUMMAX(2)=EPCLK0+PARSNG(NPAR)
              ENDIF
            ENDIF
C CHECK MAX. POSITION CORRECTION:
            IF (KINSTA.EQ.1) THEN
              SNGPOS=0D0
              DO II=1,3
                SNGPOS=SNGPOS+PARSNG(II)**2
              ENDDO
              IF (IEPMAX(3).EQ.0.OR.SUMMAX(3).LT.SNGPOS) THEN
                IEPMAX(3)=IEPOCH
                SUMMAX(3)=SNGPOS
                SUMMAX(4:6)=PARSNG(1:3)
              ENDIF
            ENDIF
C CHECK DEGREE OF FREEDOM
            IF (IEPMAX(4).EQ.0.OR.MINDOF.GT.NSNG-NPAR) THEN
              IEPMAX(4)=IEPOCH
              MINDOF=NSNG-NPAR
            ENDIF
          ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

C
C DUAL FREQUENCY MODE: CYCLE SLIP AND OUTLIER DETECTION
C -----------------------------------------------------
        ELSE
C
C INITIALIZE INTERNAL MARKING OF SATELLITES AND ITER. COUNTER
C ------------------------------------------------------------
          LINE1=' '
          LINE2=' '
          NITER =0
          NDUAEP=0
C
C INIT EPOCH CLOCK (USED FOR ZD-EPOCH CLOCK ESTIMATION ONLY)
C ----------------------------------------------------------
C
C START ITERATION DUE TO OUTLIERS
349       CONTINUE
          SNGFLG='B'
          MRKSVN(:)='G'
          NMARK =0
          EPOCLK=0D0
          EPCLK0=0D0
350       CONTINUE
C
C COMPUTE SINGLE DIFFERENCE RESIDUALS
          DO 385 IFRQ=1,NFRAUX
            NONZER(IFRQ)=0
385       CONTINUE
C
C INITIALIZE SINGLE DIFFERENCE SOLUTION
C -------------------------------------
          NITER=NITER+1
C
C KIN NEED CLOCKS IN ANY CASE...
          IF (KINSTA.EQ.1) THEN
            NPAR=4
C
C ZD: NEEDS ONLY CLOCKS
          ELSE IF (NDIFF .EQ.0) THEN
            NPAR=1
          ELSE
C
C NO EPOCH SOLUTION NECESSARY
            NPAR = 0
          ENDIF
C
          IF (NPAR.GT.0) THEN
            CALL INITRP(NPAR  ,LTRIP ,NDIFF ,SIGL12,SIGWGS,
     1                  NSNG  ,RMSSNG,ANOR  ,BNOR  )
            FLGLC=CHAR(0)
          ENDIF
C
C COMPUTE SINGLE DIFFERENCES IN TIME
C ----------------------------------
          DO 390 ISAT=1,NSAT
            DO IFRQ=1,NFRAUX
              RHS(IFRQ,ISAT)=1.D20
              IF(ABSACT(IFRQ,ISAT).NE.1.D20) THEN
                NONZER(IFRQ)=NONZER(IFRQ)+1
                IF(ABSLST(IFRQ,ISAT).NE.1.D20) THEN
                  RHS(IFRQ,ISAT)=ABSACT(IFRQ,ISAT)-ABSLST(IFRQ,ISAT)
                  RHS(IFRQ,ISAT)=RHS(IFRQ,ISAT)+EPCLK0*C/1D9
                ENDIF
              ENDIF
            END DO
C
C FORM LINEAR COMBINATION FOR SINGLE DIFFERENCE SOLUTION
C ------------------------------------------------------
            IF (NPAR.GT.0.AND.
     1          RHS(1,ISAT).NE.1.D20 .AND. RHS(2,ISAT).NE.1.D20
     2            .AND. (MRKSVN(ISAT).EQ.'G')) THEN
              CALL LINCOM(LTRIP,SVN(ISAT),
     1                    RHS(1,ISAT),RHS(2,ISAT),FLGLC,FLGLC,
     2                    RHSLC,FLGCOM)
C
C IN ORDER TO HAVE "O-C" CHANGE THE SIGN OF RESIDUAL
              IF (NDIFF.EQ.0.AND.MEATYP.EQ.1) THEN
                RHSLC=-RHSLC
              ENDIF
C
C SINGLE DIFFERENCE PARTIALS
C --------------------------
              IF (KINSTA.EQ.1) THEN
                AOBSVAL(1:3)=AOBS(1:3,ISAT)
                AOBSVAL(4)=C/1.D9
              ELSE
                AOBSVAL(1)=C/1.D9
              END IF
C
C UPDATE SINGLE NORMAL EQUATIONS
C ------------------------------
              NSNG=NSNG+1
              RMSSNG=RMSSNG+RHSLC**2
              DO I=1,NPAR
                BNOR(I)=BNOR(I)+AOBSVAL(I)*RHSLC
                DO K=1,I
                  IK=K+(I-1)*I/2
                  ANOR(IK)=ANOR(IK)+AOBSVAL(I)*AOBSVAL(K)
                END DO
              END DO
C
C KEEP SATELLITE INDEX FOR LATER USE (MARK ONE OBS. ONLY)
              ISAT11=ISAT
            ELSE
C              RHS(1:NFRAUX,ISAT)=1.D20
            END IF
390       CONTINUE
C
C NOT ENOUGH OBSERVATIONS
C -----------------------
          IF(NONZER(NFRAUX).LE.NDIFF) GOTO 499
C
C MAKE AN EPOCH SOLUTION
C ----------------------
          IF (NPAR.GT.0 .AND.
     1        (NSNG.GE.NPAR .OR. NCNSTR.GT.0) .AND. NSNG.NE.0) THEN
C
C SOLVE NORMAL EQUATION
C ---------------------
C
C CASE WITH CONSTRAINTS
C ---------------------
            IF (NSNG.LT.NPAR) THEN
              NCOBS=NSNG+NCNSTR
            ELSE
              NCOBS=NSNG
            END IF
            CALL SNGSOL(STANAM,RMSMAX,LTRIP,NSNG,NPAR,ANOR,BNOR,
     1                  IEPOCH,RMSSNG,SNGFLG,PARSNG,SIGPAR,LINE1)
            IF (IPRNT2.GE.2.AND.LEN_TRIM(LINE1).GT.0)
     1        WRITE(LFNERR,'(A)') TRIM(LINE1)
C
C Check est. clock value, repeat with other apriori clock
C -------------------------------------------------------
            LINE2=' '
            IF (SNGFLG.EQ.'G' .OR. SNGFLG.EQ.'R'
     1          .OR. (SNGFLG.EQ.'B'.AND. NSNG.GE.NPAR)) THEN
              IF (NDIFF.EQ.0.AND.DABS(PARSNG(NPAR))>1D4) THEN
                EPCLK0=EPCLK0+PARSNG(NPAR)
C
C GENERATE A MESSAGE LINE
                LINE=' ### SR DETSLP: EPOCH,UPDATED APRIORI CLOCK: '
                WRITE(LINE(46:80),'(I6,F16.3,1X,A)')
     1                IEPOCH,EPCLK0,'(' // STANAM(1)(1:4) // ')'
                IF (DABS(EPCLK0).GE.1D9) THEN
                  WRITE(LINE(52:67),'(E16.6)') EPCLK0
                ENDIF
                IF (LEN_TRIM(STANAM(2)).GT.0) THEN
                  WRITE(LINE(74:80),'(A)') '-' // STANAM(2)(1:4) // ')'
                ENDIF
                IF (IPRNT2.GE.2) WRITE(LFNERR,'(A)') TRIM(LINE)
                IF (DABS(EPCLK0)>1D4) LINE2=LINE
                GOTO 350
              ENDIF
            ENDIF
C
C CORRECT "O-C" FOR SNG ESTIMATES
C -------------------------------
            IF (SNGFLG.EQ.'G' .OR. SNGFLG.EQ.'R'
     1          .OR. (SNGFLG.EQ.'B'.AND. NSNG.EQ.NPAR)) THEN
              RESCHK=0.D0
              DO ISAT=1,NSAT
                DO IFRQ=1,NFRAUX
                  IF(RHS(IFRQ,ISAT).NE.1.D20) THEN
                    IF (KINSTA.EQ.1) THEN
                      AOBSVAL(1:3)=AOBS(1:3,ISAT)
                      AOBSVAL(4)=C/1.D9
                    ELSE
                      AOBSVAL(1)=C/1.D9
                    END IF
                    IF (NDIFF.EQ.0.AND.MEATYP.EQ.1) THEN
                      RHS(IFRQ,ISAT)=RHS(IFRQ,ISAT)+
     1                  DOT_PRODUCT(PARSNG(1:NPAR),AOBSVAL(1:NPAR))
                    ELSE
                      RHS(IFRQ,ISAT)=RHS(IFRQ,ISAT)-
     1                  DOT_PRODUCT(PARSNG(1:NPAR),AOBSVAL(1:NPAR))
                    ENDIF
                  ENDIF
                END DO

                IF(RHS(1,ISAT).NE.1.D20.AND.RHS(2,ISAT).NE.1.D20
     2            .AND. MRKSVN(ISAT).EQ.'G') THEN
                  CALL LINCOM(LTRIP,SVN(ISAT),
     1                        RHS(1,ISAT),RHS(2,ISAT),FLGLC,FLGLC,
     2                        RHSLC,FLGCOM)
                  RHSLC=-RHSLC
                  IF (DABS(RHSLC).GT.RESCHK) THEN
                    RESCHK=DABS(RHSLC)
                    IRESSV=ISAT
                  END IF
                END IF
              END DO
C
C MARK RESIDUAL
C -------------
              RESMAX=5.D0*STRP3
              IF (RESCHK.GE.RESMAX) THEN
                MRKSVN(IRESSV)='B'
C
                IF (IPRNT2.GE.2) THEN
                  CALL MPPREP(IPRNT2,ITITL2,IEPOCH,NITER ,
     1                        NSNG  ,NMARK ,NDUAEP,EPCLK0,RMSSNG,
     2                        NPAR  ,PARSNG,SIGPAR,SNGFLG)
                ENDIF
C
                NMARK=NMARK+1
                GOTO 350
              END IF
C
C SAVE BACK THE RESULTING EPOCH CLOCK ESTIMATE
C --------------------------------------------
              EPOCLK = parsng(NPAR)+epclk0
              DO ISAT=1,NSAT
                DO IFRQ=1,NFRAUX
                  IF(RHS(IFRQ,ISAT).NE.1.D20) THEN
                    IF (NDIFF.EQ.0.AND.MEATYP.EQ.1) THEN
                      RHS(IFRQ,ISAT)=RHS(IFRQ,ISAT)-EPOCLK*C/1D9
                    ELSE
                      RHS(IFRQ,ISAT)=RHS(IFRQ,ISAT)+EPOCLK*C/1D9
                    ENDIF
                  ENDIF
                ENDDO
              ENDDO
            END IF
C
C WRITE A PROGRAM OUTPUT LINE
C ---------------------------
            IF (IPRNT2.GE.2) THEN
              CALL MPPREP(IPRNT2,ITITL2,IEPOCH,NITER ,
     1                    NSNG  ,NMARK ,NDUAEP,EPCLK0,RMSSNG,
     2                    NPAR  ,PARSNG,SIGPAR,SNGFLG)
            ENDIF
          ENDIF
C
C SAVE THE RESULTING EPOCH CLOCK ESTIMATE
C --------------------------------------
          IF (NDIFF.EQ.0 .AND. SNGFLG.EQ.'G') THEN
            EPOCLK = parsng(NPAR)+epclk0
          ELSE
            EPOCLK = 0D0
          ENDIF
C
C CYCLE SLIP DETECTION
          CALL DUASLP(IEPOCH,NSAT,SVN,IWLSCR,RHS,SIGL12,SWIDTH,
     1                IRJECT,MXIOND,IPRNT2,ITITL2,MINCYC,NSLIP,LSTSLP,
     2                SLPLST,SLPXXX,IONO,CLOCK,IREF,MRKDEL,FILFLG,
     3                NTONLY,L5CLEA)
C
C OUTLIER DETECTION
          INDFRQ=3
          CALL REJECT(IPRNT2,ITITL2,IRJECT,MXOGAP,MXIOND,NSAT  ,SVN   ,
     1                INDFRQ,FRQAUX,NFREQ ,MRK1O2,MINCYC,ABSACT,
     2                ABSLST,RHS   ,MRKDEL,IDELTT,IEPOCH,TIMLST,FRSTOB,
     3                NSTEP ,NSLIP ,LSTSLP,SLPLST,SLPXXX,NDEL  ,LSTDEL,
     4                NMAXI ,LSTMXI,IAMNEW,NNEWAM,LSTAMB,AMSFLG,IRETC )
C
C OUTLIER(S) REJECTED: RESTART ITERATION
          IF(IRETC.NE.0)THEN
            NSLIP=NSTEP-1
            CLOCK(1)=CLKOLD(1)
            CLOCK(2)=CLKOLD(2)
            CLOCK(3)=CLKOLD(3)
            NDUAEP=NDUAEP+1
            GOTO 349
          END IF
C
C Mark a single Observations
          IF (NPAR.GT.0.AND.NSNG+NCNSTR.LE.NPAR) THEN
            IF (IPRNT2.GE.2) THEN
              WRITE(LFNPRT,'(A,/,17X,A,I5,A,/)') ' EPOCH SOLUTION: '//
     1              'NO SUITABLE EPOCH SOLUTION AVAILABLE.',
     2              'ALL OBSERVATIONS OF THE EPOCH ',IEPOCH,' MARKED'
            ENDIF
C
            DO ISAT=1,NSAT
              IF (RHS(1,ISAT).EQ.1.D20) CYCLE
              IF (RHS(2,ISAT).EQ.1.D20) CYCLE
C
              ISVN=SVN(ISAT)
              IEPO=IEPOCH
              ABSACT(1,ISAT)=1.D20
              ABSACT(2,ISAT)=1.D20
              ABSACT(3,ISAT)=1.D20
C
              DO 701 IFRQ=1,2
                IF (IFRQ.GT.NFRAUX) EXIT
                CALL MRKOBS(2,ISVN,IEPO,IFRQ,NSAT,SVN,NDEL,LSTDEL)
701           CONTINUE
C
              IF(IPRNT2.GE.2) THEN
                IMSG=5
                CALL WTMSGS(IMSG,ITITL2,IEPO,ISVN,IRSAT0,
     1                      IFRQ3,IWLF0,SLIP0,XSLIP0)
              ENDIF
            ENDDO
          ENDIF
C
C WRITE WARNINGS FROM PROCESSING OF THIS EPOCH SOLUTION
C -----------------------------------------------------
          IF (IPRNT2.LT.2 .AND. LEN_TRIM(LINE1).GT.0)
     1      WRITE(LFNERR,'(A)') TRIM(LINE1)
          IF (IPRNT2.LT.2 .AND. LEN_TRIM(LINE2).GT.0)
     1      WRITE(LFNERR,'(A)') TRIM(LINE2)
C
C UPDATE SUMMARY STATISTIC
C ------------------------
          IF (SNGFLG.EQ.'G') THEN
            NSOL(1)=NSOL(1)+1
          ELSE IF (SNGFLG.EQ.'R') THEN
            NSOL(2)=NSOL(2)+1
          ELSE IF (SNGFLG.EQ.'B' .AND. NSNG.LE.NPAR) THEN
            NSOL(3)=NSOL(3)+1
          ELSE IF (SNGFLG.EQ.'B') THEN
            NSOL(4)=NSOL(4)+1
          ENDIF
C A GOOD OR RMS SOLUTION FOUND:
          IF (SNGFLG.EQ.'G') THEN
            RMSSUM=RMSSUM+RMSSNG
C CHECK MAX. RMS:
            IF (IEPMAX(1).EQ.0.OR.SUMMAX(1).LT.RMSSNG) THEN
              IEPMAX(1)=IEPOCH
              SUMMAX(1)=RMSSNG
            ENDIF
C CHECK MAX. CLOCK CORRECTION:
            IF (NDIFF.EQ.0.OR.KINSTA.EQ.1) THEN
              IF (IEPMAX(2).EQ.0.OR.
     1            DABS(SUMMAX(2)).LT.DABS(EPCLK0+PARSNG(NPAR))) THEN
                IEPMAX(2)=IEPOCH
                SUMMAX(2)=EPCLK0+PARSNG(NPAR)
              ENDIF
            ENDIF
C CHECK MAX. POSITION CORRECTION:
            IF (KINSTA.EQ.1) THEN
              SNGPOS=0D0
              DO II=1,3
                SNGPOS=SNGPOS+PARSNG(II)**2
              ENDDO
              IF (IEPMAX(3).EQ.0.OR.SUMMAX(3).LT.SNGPOS) THEN
                IEPMAX(3)=IEPOCH
                SUMMAX(3)=SNGPOS
                SUMMAX(4:6)=PARSNG(1:3)
              ENDIF
            ENDIF
C CHECK DEGREE OF FREEDOM
            IF (IEPMAX(4).EQ.0.OR.MINDOF.GT.NSNG-NPAR) THEN
              IEPMAX(4)=IEPOCH
              MINDOF=NSNG-NPAR
            ENDIF
          ENDIF
C END OF SINGLE FRQ./COMBINED MODE
        END IF
C
C UPDATE ABSACT, RHS, CLOCK, WRITE TRIPLE DIFF. RESIDUALS
C -------------------------------------------------------
        CALL RSTCLK(IPRNT2,ITITL2,JMPOPT,TOLJMP,IEPOCH,NFRAUX,FRQAUX,
     1              IWLSCR,NSAT  ,SVN   ,CLKOLD,NDIFF ,EPOCLK,STANAM(1),
     2              TIMREF,IDELTT,DELTAT,NSLIP ,LSTSLP,SLPLST,CLOCK ,
     3              CLKFLG,NCLKEV,LSTCLK)
        IF (NCLKEV.GT.MXCCLK) EXIT
C
        CALL CYCEPO(IRESID,IREFIL,IEPOCH,
     1              NSTEP,NSLIP,LSTSLP,SLPLST,
     2              NSAT,SVN,NFRAUX,FRQAUX,
     3              NNEWAM,LSTAMB,LASTFL,ABSACT,RHS)
C
C UPDATE LAST OBSERVATION PER AMBIGUITY
        DO ISAT=1,NSAT
          IF (ABSACT(1,ISAT).EQ.1D20) CYCLE
          IF (ABSACT(2,ISAT).EQ.1D20.AND.NFRAUX.GT.1) CYCLE
          DO IAMB=NNEWAM(ISAT),1,-1
            IF(IEPOCH.GE.LSTAMB(1,ISAT,IAMB)) THEN
              LSTAMB(3,ISAT,IAMB)=IEPOCH
              EXIT
            ENDIF
          ENDDO
        ENDDO
C
C UPDATE ABSLST
        CALL UPDSNG(IEPOCH,NSAT,NFRAUX,NONZER,NDIFF,CLOCK,ABSACT,
     1              ABSLST,TIMLST,FRSTOB)
C
C MOVE THE CYCLE SLIP FLAGS INTO THE NEXT EPOCH IF NECESSARY
C ----------------------------------------------------------
499     DO 510 IDEL=1,NDEL
          IF ((LSTDEL(2,IDEL) .LE. IEPOCH) .AND.
     1        (LSTDEL(3,IDEL) .GE. IEPOCH)) THEN
            DO 511 ISAT=1,NSAT
              IF (LSTDEL(1,IDEL) .EQ. SVN(ISAT)) THEN
                DO 512 IFRQ=1,NFRAUX
                  IF (((LSTDEL(4,IDEL).EQ.IFRQ).OR.(IFRQ.GT.2)) .AND.
     1                (FILFLG(IFRQ,ISAT).EQ.'C')) FILFLG(IFRQ,ISAT)='M'
512             CONTINUE
              END IF
511         CONTINUE
          END IF
510     CONTINUE
C
      ENDDO
C
C PRINT FINAL LINE FOR MESSAGES
C -----------------------------
      IF(IPRNT2.GE.2.AND.ITITL2.EQ.0) WRITE(LFNPRT,502)
C
C WRITE THE SUMMARY IN THE LAST ITERATION
C ---------------------------------------
      IF (IPRNT2.GE.1.AND.NPAR.GT.0) THEN
        WRITE(LFNPRT,'(//,1X,72("-"),
     1        /," SUMMARY OF EPOCH SOLUTIONS",
     2        /,1X,72("-"),/)')
        WRITE(LFNPRT,'(4(A,I10,/),/,A,I10,/)')
     1  ' NUMBER OF EPOCH SOLUTIONS WITHOUT PROBLEMS: ',NSOL(1),
     2  ' NUMBER OF EPOCH SOLUTIONS WITH A BIG RMS:   ',NSOL(2),
     3  ' NUMBER OF EPOCH SOLUTIONS WITH TOO FEW OBS.:',NSOL(3),
     4  ' NUMBER OF EPOCH SOLUTIONS WITH "BAD" FLAG:  ',NSOL(4),
     5  ' TOTAL NUMBER OF EPOCH SOLUTIONS:            ',
     6                           NSOL(1)+NSOL(2)+NSOL(3)+NSOL(4)
        WRITE(LFNPRT,'(A,F12.4,A,/)')
     1  ' MEAN RMS OF THE EPOCH SOLUTIONS:          ',
     2                             RMSSUM/(NSOL(1)+NSOL(2)),' M'
        IF (IEPMAX(1).NE.0)
     1    WRITE(LFNPRT,'(A,F12.4,A,I6)')
     2    ' MAXIMUM RMS FOR AN EPOCH SOLUTION:        ',
     3                       SUMMAX(1),' M    EPOCH:',IEPMAX(1)
        IF (IEPMAX(2).NE.0) THEN
          IF (DABS(SUMMAX(2)).LT.1D6) THEN
            WRITE(LFNPRT,'(A,F12.4,A,I6)')
     1      ' MAX. CLOCK CORR. FOR AN EPOCH SOLUTION:   ',
     2                         SUMMAX(2),' NS   EPOCH:',IEPMAX(2)
          ELSE
            WRITE(LFNPRT,'(A,E12.4,A,I6)')
     1      ' MAX. CLOCK CORR. FOR AN EPOCH SOLUTION:   ',
     2                         SUMMAX(2),' NS   EPOCH:',IEPMAX(2)
          ENDIF
        ENDIF
        IF (IEPMAX(3).NE.0)
     1    WRITE(LFNPRT,'(A,F10.4,A,I6,2(/,43X,A,F10.4,A))')
     2    ' MAX. POS. CORR. FOR AN EPOCH SOLUTION:    ' //
     3                  'X:',SUMMAX(4),' M    EPOCH:',IEPMAX(3),
     4                  'Y:',SUMMAX(5),' M',
     5                  'Z:',SUMMAX(6),' M'
        IF (IEPMAX(4).NE.0)
     1    WRITE(LFNPRT,'(A,I12,A,I6)')
     2    ' MIN. DEG OF FREEDOM FOR AN EPOCH SOLUTION:',
     3                       MINDOF,'      EPOCH:',IEPMAX(4)
        WRITE(LFNPRT,502)
      ENDIF
C
C HANDLE CLOCK EVENTS
C -------------------
      IF (NDIFF.EQ.0.AND.NCLKEV.LE.MXCCLK) THEN
        CALL CLKEVT(IPRNT2,ITITL2,JMPOPT,NSAT  ,SVN   ,NFRAUX,
     1              IDELTT,NEPOCH,NCLKEV,LSTCLK,NDEL  ,LSTDEL,
     2              NNEWAM,LSTAMB,AMSFLG)
      ENDIF
C
C CHECK FOR EPOCH-BY-EPOCH "CYCLE SLIP" EVENTS
C --------------------------------------------
      CALL CHKSLP(MXCSAT,IPRNT2,ITITL2,NSAT  ,SVN   ,NFRAUX,
     1            NSLIP ,LSTSLP,NDEL  ,LSTDEL,NNEWAM,LSTAMB,AMSFLG)
C
C MERGE LSTDEL AND LSTAMB TO REMOVE AMBIGUITIES WITH NO/FEW OBSERV.
C -----------------------------------------------------------------
      CALL AMBUPD(IPRNT2,ITITL2,IAMNEW,NSAT  ,SVN   ,NFRAUX,
     1            IDELTT,NNEWAM,LSTAMB,AMSFLG,NDEL  ,LSTDEL)
C
C PRINT CYCLE SLIPS SUMMARY (AND NEW SLIPS)
C -----------------------------------------
      ITITLE=0
      INEW  =1
C
C SHORT SUMMARY OF CYCLE SLIPS
      WRITE(LFNPRT,501)
501   FORMAT(//,1X,72('-'),
     1        /,' CYCLE SLIPS ACCEPTED IN THIS RUN',
     2        /,1X,72('-'),/)
      ISUMRY=1
      ICYTYP=0
      ICYFRQ=0
      ICYSVN=0
      CYCMAX=1.D20
      CALL DSPCYC(ITITLE,ISUMRY,INEW,ICYTYP,ICYFRQ,ICYSVN,CYCMAX,
     1            NFREQ,NSAT,SVN,
     2            NSLIP,LSTSLP,SLPLST,SLPXXX,IONO,IRC)
C
C LIST OF NEW CYCLE SLIPS
      IF(IPRNT2.GE.1) THEN
        ISUMRY=0
        CALL DSPCYC(ITITLE,ISUMRY,INEW,ICYTYP,ICYFRQ,ICYSVN,CYCMAX,
     1              NFREQ,NSAT,SVN,
     2              NSLIP,LSTSLP,SLPLST,SLPXXX,IONO,IRC)
      ELSE
        WRITE(LFNPRT,502)
502     FORMAT(/,1X,72('-'))
      ENDIF
C
C PRINT MARKED AREA SUMMARY (AND NEW OR CHANGED AREAS)
C ----------------------------------------------------
C
C SUMMARY OF MARKED AREAS
      WRITE(LFNPRT,503)
503   FORMAT(//,1X,72('-'),
     1        /,' NEW OR MODIFIED MARKED AREAS IN THIS RUN',
     2        /,1X,72('-'),/)
      ISUMRY=1
      IDETYP=0
      IDEFRQ=0
      IDESVN=0
      CALL DSPDEL(ITITLE,ISUMRY,INEW,IDETYP,IDEFRQ,IDESVN,
     1            NFREQ,NSAT,SVN,NDEL,LSTDEL,IRC)
C
C LIST OF NEW CYCLE SLIPS
      IF(IPRNT2.GE.1) THEN
        ISUMRY=0
        CALL DSPDEL(ITITLE,ISUMRY,INEW,IDETYP,IDEFRQ,IDESVN,
     1              NFREQ,NSAT,SVN,NDEL,LSTDEL,IRC)
      ELSE
        WRITE(LFNPRT,502)
      ENDIF
C
C PRINT MAX.INTERVAL EXCEEDING SUMMARY
C ------------------------------------
C
C SUMMARY OF MAX.INTERVAL EXCEEDINGS
      WRITE(LFNPRT,504)
504   FORMAT(//,1X,72('-'),
     1        /,' MAXIMUM INTERVAL EXCEEDINGS IN THIS RUN',
     2        /,1X,72('-'),/)
      ISUMRY=1
      CALL DSPMXI(ITITLE,ISUMRY,NFREQ,NMAXI,LSTMXI,IRC)
C
C LIST OF MAX.INTERVAL EXCEEDINGS
      IF(IPRNT2.GE.1.AND.NMAXI.GT.0) THEN
        ISUMRY=0
        CALL DSPMXI(ITITLE,ISUMRY,NFREQ,NMAXI,LSTMXI,IRC)
      ELSE
        WRITE(LFNPRT,502)
      ENDIF
C
C PRINT CLOCK EVENTS
C ------------------
      IF (NDIFF.EQ.0) THEN
        WRITE(LFNPRT,505)
505     FORMAT(//,1X,72('-'),
     1          /,' CLOCK EVENT SUMMARY FOR ZERO-DIFFERENCE FILES',
     2          /,1X,72('-'),/)
        ISUMRY=1
        CALL DSPCLK(ITITLE,ISUMRY,NCLKEV,LSTCLK,IRC)
C
C LIST OF THE CLOCK EVENTS
        IF(IPRNT2.GE.1.AND.NCLKEV.GT.0) THEN
          ISUMRY=0
          CALL DSPCLK(ITITLE,ISUMRY,NCLKEV,LSTCLK,IRC)
        ELSE
          WRITE(LFNPRT,502)
        ENDIF
      ENDIF
C
C Update slip list by removing epoch-by-epoch-events
      mSlip = 0
      DO iSlip = 1,nSlip
        IF (IABS(lstslp(6,iSlip)) > 1000) CYCLE
        mSlip = mSlip + 1
        IF (mSLip == iSlip) CYCLE
        lstSlp(:,mSlip) = lstSlp(:,iSlip)
        slpLst(mSlip)   = slpLst(iSlip)
        slpXXX(:,mSlip) = slpXXX(:,iSlip)
        iono(:,mSlip)   = iono(:,iSlip)
      ENDDO
      nSlip = mSlip
C
      RETURN
      END SUBROUTINE

      END MODULE
