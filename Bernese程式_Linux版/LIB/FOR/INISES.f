      MODULE s_INISES
      CONTAINS

C*
      SUBROUTINE INISES(SESSID ,STRAMB ,CORSTR ,NPAR   ,NPARN  ,MAXPAR ,
     1                  MAXAMP ,MAXFRS ,IELEV  ,TPRINT ,IEXTRA ,NFTOT  ,
     2                  OBSFIL ,CSESS  ,LOCQ   ,ANOR   ,BNOR   ,AELL   ,
     3                  BELL   ,DXELL  ,DRELL  ,SCELL  ,TITLES ,POSECC ,
     4                  MELWUB ,IZEROD ,MEATYP ,NFREQ  ,ARCINT ,TBOUND ,
     5                  NUMSAT ,STFIL  ,STNAME ,XSTAT  ,XSTELL ,NFRFIL ,
     6                  ICARR  ,TIMREF ,NEPOCH ,IDELTT ,NSATEL ,SATNUM ,
     7                  OPTDIP ,OPTELI ,NSASES ,NFLSES ,NFRSES ,FRQSES ,
     8                  STASES ,SATSES ,FILNUM ,IFDONE ,NPASES ,NAMSES ,
     9                  IELE0  ,SYNCM  ,ISYNC  ,WGSSES ,ELLSES ,IAMB1  ,
     1                  INDARC ,XMAXCL ,TLAST  ,RMSSES ,POLARS ,NDIFF  ,
     2                  ELEVMM ,NOBELV ,ARCINT2,TBOUND2,LEOARC ,NAMAX  ,
     3                  NADIGN ,NOBNAD ,NADIMM ,OBSEPO ,NOBAZIS,ICLU   ,
     4                  USEGEOS,GOBSDEF)
CC
CC NAME       :  INISES
CC
CC PURPOSE    :  INITIALIZE SESSION-WISE PROCESSING IN
CC               BERNESE GPS SOFTWARE VERSION 3
CC
CC PARAMETERS :
CC         IN :  SESSID : IDENTIFIER OF SESSION TO BE         CH*4
CC                        INITIALIZED
CC               STRAMB : STRATEGY TO HANDLE AMBIGUITIES      I*4(*)
CC               CORSTR : STRATEGY TO HANDLE CORRELATIONS     I*4
CC               NPAR   : NUMBER OF PARAMETERS
CC               NPARN  : NUMBER OF PARAMETERS WITHOUT AMBIG. I*4
CC               MAXPAR : MAXIMUM NUMBER OF PARAMETERS        I*4
CC               MAXAMP : MAXIMUM NUMBER OF AMBIGUITIES       I*4
CC               MAXFRS : MAXIMUM NUMBER OF FREQUENCIES PER   I*4
CC                        SESSION
CC               IELEV  : INDEX FOR WRITING A TABLE OF ELEVA- I*4
CC                        TIONS
CC               TPRINT ; LAST EPOCH PRINTED                  R*8
CC               IEXTRA : USE MODEL VALUES FOR TROPOSPHERE?   I*4
CC               NFTOT  : TOTAL NUMBER OF FILES               I*4
CC               OBSFIL(I),I=1,2,..,NFTOT: OBS FILE NAMES     CH*(*)
CC               CSESS(K,I),K=1,2, I=1,2,..,NFTOT: SESSION    CH*4
CC                        IDENTIFIERS FOR INDIVIDUAL FILES
CC               LOCQ(K,I),K=1,..,MAXLCQ, I=1,2,..,NPAR:      I*4
CC                        PARAMETER DEFINITION
CC               ANOR   : NORMAL EQUATION MATRIX              R*8
CC               BNOR   : RIGHT HAND SIDE OF NORMAL EQUATION  R*8
CC                        SYSTEM
CC               AELL,BELL: SEMI-MAJOR AND -MINOR AXIS OF     R*8
CC                        ELIPSOID
CC               DXELL(I),I=1,2,3: SHIFT TO WGS-84            R*8
CC               DRELL(I),I=1,2,3: ROTATIONS TO WGS-84        R*8
CC               SCELL  : SCALE TO WGS-84                     R*8
CC               TITLES(I),I=1,2: TITLE LINES                 CH*132
CC               POSECC(I,K,L),I=1,2,3,K=1,2,L=1,..,NFTOT:    R*8
CC                        POSITIONING ECCENTRICITIES
CC                        I: COORDINATE
CC                        K: STATION
CC                        L: FILE
CC               MELWUB : MELBOURNE-WUEBBENA LC               I*4
CC                        =0: NO
CC                        =1: YES
CC                        =2: DTEC LC
CC               ARCINT(I),I=1,..,NFTOT: ARC NUMBER FOR FILE I I*4
CC               TBOUND(K,I),K=1,2, I=1,2,..,NARC: INTERVAL   R*8
CC                        BOUNDARIES FOR ARC I (MJD)
CC               NUMSAT(I),I=1,..,NARC: NUMBER OF SATEL-      I*4
CC                        LITES IN ARC NUMBER I
CC               STFIL(K,I),K=1,2, I=1,..,NFTOT: NUMBERS OF   I*4
CC                        OBSERVING STATIONS IN COORDINATE FILE
CC               STNAME(I),I=1,2,..,NSTAT: STATION NAMES      CH*16
CC               XSTAT(K,I),K=1,2,3, I=1,..,NSTAT: WGS COORD  R*8
CC               XSTELL(K,I),... : ELLIPSOIDAL COORDINATES
CC               NFRFIL(I),I=1,..,NFTOT: NUMBER OF OBS. FREQ. I*4
CC               ICARR(K,I),K=1,.,I=1,..,NFTOT: CARRIERS      I*4
CC               TIMREF(I),I=1,..,NFTOT: FILE REFERENCE TIMES R*8
CC               NEPOCH(I),I=1,..,NFTOT: NUMBER OF EPOCHS     I*4
CC               IDELTT(I),I=1,..,NFTOT: OBSERVATION INTER-   I*4
CC                        VALS (SEC)
CC               NSATEL(I),I=1,..,NFTOT: NUMBER OF SATELLITES I*4
CC               SATNUM(K,I),K=1,..,NSATEL(I),I=1,..,NFTOT:   I*4
CC                        SATELLITE NUMBERS IN FILE I
CC               OPTDIP : OPTIONS FOR DIFF. ION. PARAMETERS   I*4(3)
CC                        (1): =0: NO DIFF. ION. PARAMETERS
CC                             =1: ONE PAR. PER EPOCH AND SAT.
CC                             =2: PARAMETERS EPOCH-WISE PRE-
CC                                 ELIMINATED
CC                        (2): ELIMINATION OF REF. ION. PAR.
CC                        (3): ELEV.-DEP. PAR. CONSTRAINING
CC               OPTELI(I),I=1,..,MAXTYP: OPTION FOR PRE-     I*4
CC                        ELIMINATION OF PARAMETER TYPES:
CC                        =0 : NOT PRE-ELIMINATED
CC                        =1 : PRE-ELIMINATED BEFORE INVERSION
CC                        =2 : PRE-ELIMINATED AFTER  INVERSION
CC                        =3 : PRE-ELIMINATED EPOCH-WISE
CC               NDIFF(I):I=1,..,NFTOT: DIFFERENCE TYPE       I*4(*)
CC                        NDIFF=0: ZERO DIFFERENCE
CC                        NDIFF=1: SINGLE DIFFERENCE
CC               ARCINT2(I),I=1,..,NFTOT: LEO ARC NUMBER      I*4
CC                        FOR FILE I
CC               TBOUND2(K,I),K=1,2, I=1,2,..,NARC: INTERVAL  R*8
CC                        BOUNDARIES FOR LEO ARC I (MJD)
CC        OUT :  NSASES : NUMBER OF SATELLITE IN SESSION      I*4
CC               NFLSES : NUMBER OF FILE IN SESSION           I*4
CC               NFRSES : NUMBER OF FREQUENCIES IN SESSION    I*4
CC               FRQSES(I),I=1,..,NFRSES: FREQUENCIES         I*4
CC               STASES(I),I=1,..,NSTSES: STATION NAMES       CH*16
CC               SATSES(I),I=1,..,NSASES: SATELLITE NUMBERS   I*4
CC               FILNUM(I),I=1,..,NFLSES: OBS FILE NUMBERS IN I*4
CC                        SESSION
CC               IFDONE(I),I=1,..,NFTOT : =0 FILE NOT YET     I*4
CC                        PROCESSED,      =1 ALREADY PROCESSED
CC               NPASES : NUMBER OF PARAMETERS FOR ONE SES-   I*4
CC                        SION: NUMBER OF PARAMETERS WITHOUT
CC                        AMBIGUITIES + AMBIGUITIES OF THE
CC                        SESSION
CC               IELE0  : STARTING POINT (-1) IN ARRAY ...    I*4
CC               SYNCM(I),I=1,..,NFLSES: SYNCH. ERROR         R*8
CC               ISYNC(K,I),K=1,2,...,MAXSAT, I=1,2,..,NFLSES I*4
CC                        SYNCH. INIT. FOR SATELLITE I
CC               WGSSES(K,J,L,I),K=1,2,3,J=1,2,L=1,..,MAXFRQ, R*8
CC                        I=1,..,NFLSES: WGS COORDINATES X,Y,Z
CC                        (INCL. ECCENTRICITIES)
CC               ELLSES(K,J,L,I),K=1,2,3,J=1,2,L=1,..,MAXFRQ, R*8
CC                        I=1,..,NFLSES: ELLIPSOIDAL COORD.
CC                        (INCL. ECCENTRICITIES)
CC               IAMB1(M,K,I),M=1,..,MAXAMB, K=1,..,NFRFIL,  CH*1
CC                        I=1,..,NFLSES:
CC                        FLAGS FOR AMBIGUITY DEFINITION
CC               INDARC : ARC NUMBER FOR THE SESSION
CC               XMAXCL(I),I=1,..,NFTOT: MAXIMUM CLOCK ERRORS R*8
CC               TLAST  : LAST OBS TIME                       R*8
CC               RMSSES : SESSION SPECIFIC RMS ERROR          R*8
CC               POLARS(L,K,I),L=1,..,NUMAMB,K=1,..,NFRFIL,   R*8
CC                        I=1,..,NFLSES: POLARIZATION SAVE ARRAY
CC               ELEVMM(2,I),I=1,..,NFTOT: MIN. AND MAX.      R*8
CC                        ELEVATION ANGLE
CC               NOBELV(I,J,K),I=1,2,J=1,..,18,K=1,..,NFTOT   I*4
CC                        5-DEG BIN OBSERVATION STATISTICS
CC                        (ELEVATION)
CC               LEOARC: ARC NUMBER FOR THE LEO               I*4
CC               NAMAX  : ACTUAL MAXIMUM NADIR ANGLE          R*8
CC               NADIGN : NUMBER OF OBSERVATIONS IGNORED DUE  I*4
CC                        TO A TOO HIGH NADIR ANGLE
CC               NOBNAD(I,J,K),I=1,2,J=1,..,30,K=1,..,NFTOT   I*4
CC                        0.5-DEG BIN OBSERVATION STATISTICS
CC                        (NADIR ANGLE)
CC               NADIMM(2,I),I=1,..,NFTOT: MIN. AND MAX.      R*8
CC                        NADIR ANGLE
CC               OBSEPO(I,J),I=1..MAXSAT,J=1,NFREQ,K=1,NFLSES I*4(*,*,*)
CC                        SAT OBSEVRED IN THE PREV. EPOCH
CC               NOBAZIS(L,K,J,I),L=1,2,K=1,..,36,            I*4
CC                        J=1,..,NSATEL(I),I=1,..,NFTOT
CC                        OBSERVATION STATISTICS FOR THE
CC                        AZIMUTH ANGLE AT THE SATELLITE
CC               ICLU(I,K),I=1,MXCAMB,K=1,NFLSES              I*4
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER, G.BEUTLER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/10/15 11:46
CC
CC CHANGES    :  10-MAY-91 : ??: OPEN REPLACED BY SUBROUTINE "OPNFIL" TO
CC                               AVOID CONFLICTS WHEN RUNNING SEVERAL JOBS.
CC               18-JUL-92 : ??: CORRECT SETTING OF "NPASES"
CC               24-JUL-92 : ??: CHECK CONSISTENCY OF "INDARC" WITHIN ONE
CC                               SESSION
CC               31-JUL-92 : ??: SESSION SPECIFIC RMS AS PARAMETER
CC               25-FEB-93 : ??: NEW PARAMETERS IN CALL GPHECC (ELEV. DEP.)
CC               26-APR-93 : ??: NEW PARAMETER IFDONE
CC               06-APR-94 : MR: PRINT OF ELEVATIONS FOR 31 SAT.
CC               08-APR-94 : MR: ADD PARAM. "AZI" TO CALL OF GPHECC
CC               29-MAY-94 : MR: ADD SATELLITE NUMBER AND AZIMUTH
CC                               TO CALL OF SR METEO
CC               10-AUG-94 : MR: CALL EXITRC
CC               12-AUG-94 : MR: FORMAT 4: SESSION AS CHARACTER*4
CC               23-SEP-94 : MR: CLOSING OF OBSERVATION FILES REMOVED,
CC                               NOW DEALT WITH IN SR CLOSES
CC               14-AUG-95 : LM: MEMORY COMPACT VERSION
CC               31-AUG-95 : SS/MR: CHECK COLLISION OF LF-NUMBERS
CC               05-SEP-95 : ??: NO INITIALIZATION OF THE AMBIGUITIES
CC               26-MAR-96 : MR: ADD "CSESS" TO CALL GPHECC
CC               26-MAR-96 : TS: ADDED NDIFF FOR ZERO/SINGLE DIFFERENCES
CC               06-MAY-96 : TS: REMOVED OLD SATELLITE CLOCK STUFF
CC               27-JAN-97 : MR: MINUMUM AND MAXIMUM ELEV. ANGLES
CC               29-JAN-97 : SS: ELEVATION-DEPENDENT OBS. WEIGHTING
CC               05-AUG-97 : SS: ELEV.-DEP. SIP CONSTRAINING
CC               22-SEP-97 : SS: 5-DEG BIN OBSERVATION STATISTICS
CC               30-MAR-98 : TS: SIMULTANEOUS CODE AND PHASE ZD PROCESSING
CC               29-APR-98 : SS: DTEC LC
CC               04-MAY-98 : SS: SET GLONASS FREQUENCIES
CC               18-MAY-01 : DS: NEW PARAMETERS, FOR LEO ARC: LEOARC, ARCINT
CC               18-MAY-01 : DS: CHECK CONSISTENCY OF "LEOARC"
CC                               WITHIN ONE SESSION
CC               15-NOV-02 : RS: NAMAX, NADIGN, NOBNAD, NADIMM ADDED
CC               05-MAR-03 : CU: REMOVE USE OF SKELETON FILE
CC               28-MAY-03 : RD: NEW CALL OF SR GPHECC
CC               11-AUG-03 : RS: RECTYP=' ' IN CALL OF GPHECC
CC               08-SEP-03 : HU: ANTNAM, RECNAM CHR16 -> CHR20,
CC                               FILENAMES CHR(*)
CC               13-SEP-03 : HU: INTERFACE FOR DEFREQ
CC               29-MAR-04 : CU: ADD DUMMY VARIABLE TO CALL OF SR METEO
CC               13-APR-04 : RS: ADD NOBAZIS
CC               05-JUL-04 : HU: SAT. NUMBERS IN HEADER FOR ELEVATION TABLE
CC               04-JAN-05 : RD: INDARC/LEOARC CHECK ONLY FOR ORBIT ESTIM.
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               08-Aug-05 : HB: Use new SR TIMST2 (module)
CC               11-JAN-06 : RD: STATISTICS ON PHASE-CONNECTED EPOCHS
CC               05-DEC-06 : HB: ICLU AS PARAMETER FOR INITIALIZATION
CC               12-Jun-07 : AG: GPHECC removed
CC               16-MAY-08 : DT: SLR SATELLITES TREATED AS GNSS NOT AS LEO
CC               20-NOV-09 : RD: AMBIGUITY INTERVALS PER GNSS
CC               26-JAN-11 : LP: Sat.-specific obstypes in DEFREQ call
CC               17-FEB-11 : SS: STRAMB(3) FOR SELECTION OF GNSS
CC               29-FEB-12 : RD: CORRECT ARRAY DIMENSIONS OF DUMMY ARGUMENTS
CC               29-FEB-12 : RD: REMOVE UNUSED VARIABLES
CC               29-Feb-12 : RD: USE METEO AS MODULE
CC               28-MAR-12 : RD: USE SVN2CHR AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnprt, lfnerr, lfn001,lfn002, lfneph
      USE m_global, ONLY: MAXSYS
      USE d_rinex3, ONLY: t_gobsdef
      USE s_opnfil
      USE s_svn2chr
      USE s_opnerr
      USE s_timst2
      USE s_defreq
      USE s_exitrc
      USE s_ellecc
      USE s_meteo
      USE s_xyzell
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IAMB  , IARC  , IBIN  , IBIN1 , IELE0 ,
     1          IELEV , IEXTRA, IF    , IFIL  , IFIL1 , IFILE , IFIRST,
     2          IFRPR1, IFRPRI, IFRQ  , IFRSES, IK    , IND   , INDARC,
     3          IOSTAT, IPAR  , ISATEL, IST   , ISTA  , ISVMOD,
     4          ISVN  , IZEROD, K     , KFIL  , KSAT  , KST   , LEOARC,
     5          LFNOBS, LFNTOP, LFNTST, MAXAMP, MAXFRS, MAXPAR, MELWUB,
     6          MXCAMB, MXCFLS, MXCFRQ, MXCLCQ, MXCSAT, NAMSES, NFLSES,
     7          NFRSES, NFTOT , NPAR  , NPARN , NPASES, NSASES, NSTSES,
     8          MXCSAS, IBIN2
C
      REAL*8    AELL  , BELL  , DTBNDS, RMSSES, SCELL ,
     1          TIMEND, TLAST , TPRINT
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      INTEGER*4 STRAMB(*),CORSTR,LOCQ(MXCLCQ,*)
      INTEGER*4 ARCINT(*),NUMSAT(*),SATSES(*),ISYNC(MXCSAT,*)
      INTEGER*4 ARCINT2(*)
      INTEGER*4 STFIL(2,*),NFRFIL(*),MEATYP(*),NFREQ(*)
      INTEGER*4 ICARR(MXCFRQ,*),NEPOCH(*),IDELTT(*)
      INTEGER*4 FILNUM(*),IFDONE(*),FRQSES(*)
      INTEGER*4 NSATEL(*),SATNUM(MXCSAT,*),OPTDIP(*),OPTELI(*),NDIFF(*)
      INTEGER*4 NOBELV(2,18,*),NOBNAD(2,30,*),NADIGN
      INTEGER*4 OBSEPO(MXCSAT,MXCFRQ,*)
      INTEGER*4 NOBAZIS(2,36,MXCSAT,*)
      INTEGER*4 ICLU(0:MAXSYS,MXCAMB,*),USEGEOS
C
      REAL*8 ANOR(*),BNOR(*),POSECC(3,2,*),SYNCM(*),DXELL(3),DRELL(3)
      REAL*8 WGSSES(3,2,MXCFRQ,*),ELLSES(3,2,MXCFRQ,*)
      REAL*8 XSTAT(3,*),XSTELL(3,*),EXC(3),TIMREF(*),XMAXCL(*)
      REAL*8 TSESS(2),EPOFRQ(2)
      REAL*8 POLARS(MXCAMB,MXCFRQ,*),ELEVMM(2,*),NAMAX,NADIMM(2,*)
      REAL*8 TBOUND(2,*),TBOUND2(2,*),DUMMY(1),DUMMY31(3,1)
C
      CHARACTER*132 TITLES(2)
      CHARACTER*(*) OBSFIL(*)
      CHARACTER*40  SESSTR
      CHARACTER*16  STASES(*),STNAME(*)
      CHARACTER*6   MXNSAS,MXNSAT,MXNFRQ,MXNAMB,MXNFLS,MXNLCQ
      CHARACTER*4   SESSID,CSESS(2,*)
      CHARACTER*3   NAVPRT(MXCSAT)
      CHARACTER*1   IAMB1(MXCAMB,MXCFRQ,*),SVNCHR
C
      TYPE(t_gobsdef) :: GOBSDEF ! Giove External Obs. Selection info
C
      COMMON/MCMSAS/MXCSAS,MXNSAS
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMFRQ/MXCFRQ,MXNFRQ
      COMMON/MCMAMB/MXCAMB,MXNAMB
      COMMON/MCMFLS/MXCFLS,MXNFLS
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
C
      DATA IFIRST/1/,IFRPRI/1/,IFRPR1/1/
C
C INITIALIZE SESSION SPECIFIC ERRORS
C ----------------------------------
      RMSSES = 0.D0
C
C INITIALIZE MAXIMUM SYNCHRONIZATION ERRORS AND ELEVATION AND
C -----------------------------------------------------------
C NADIR ANGLE STATISTICS
C ----------------------
      IF(IFIRST.EQ.1) THEN
        IFIRST=0
        NAMAX = 0.0
        NADIGN = 0
        DO IF=1,NFTOT
          XMAXCL(IF)=0.D0
          ELEVMM(1,IF)=90.D0
          ELEVMM(2,IF)=-90.D0
          NADIMM(1,IF)=90.D0
          NADIMM(2,IF)=0.D0
          DO ISTA=1,2
            DO IBIN=1,18
              NOBELV(ISTA,IBIN,IF)=0
            ENDDO
            DO IBIN1=1,30
              NOBNAD(ISTA,IBIN1,IF)=0
            ENDDO
            DO IBIN2=1,36
              DO ISATEL=1,NSATEL(IF)
                NOBAZIS(ISTA,IBIN2,ISATEL,IF)=0
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C
C DEFINE NUMBER OF FILES, AND CORRESPONDING FILE NUMBERS IN SESSION
C -----------------------------------------------------------------
      IF(CORSTR.EQ.1)THEN
        NFLSES=1
        READ(SESSID,'(I4)') IFILE
        FILNUM(1)=IFILE
        IFDONE(IFILE)=1
C
C MELBOURNE-WUEBBENA: LOOK FOR CORRESPONDING CODE FILE
        IF (MELWUB.EQ.1) THEN
          DO 1010 KFIL=1,NFTOT
            IF(MEATYP(KFIL).EQ.2.AND.NFREQ(KFIL).EQ.2.AND.
     1         STFIL(1,IFILE).EQ.STFIL(1,KFIL).AND.
     2         STFIL(2,IFILE).EQ.STFIL(2,KFIL).AND.
     3         CSESS(1,IFILE).EQ.CSESS(1,KFIL).AND.
     4         CSESS(2,IFILE).EQ.CSESS(2,KFIL))       THEN
              NFLSES=2
              FILNUM(2)=KFIL
              IFDONE(KFIL)=1
              GO TO 1020
            END IF
1010      CONTINUE
C
C NO CORRESPONDING CODE FILE FOUND
          WRITE(LFNERR,1011) IFILE
1011      FORMAT(/,' *** SR INISES: NO DUAL FRQ CODE FILE FOUND FOR',/,
     1                       16X,'PHASE FILE NUMBER: ',I4,/)
          CALL EXITRC(2)
1020      CONTINUE
C
C FOR ZERO-DIFF CHECK FOR CORRESPONDING CODE/PHASE FILE
C -----------------------------------------------------
c         IF (IZEROD.EQ.1) THEN
c           DO 1030 JF=IF,NFTOT
c             IF(CSESS(1,JF).EQ.SESSID .AND.
c    1           STFIL(1,IF).EQ.STFIL(1,JF) .AND.
c    2           MEATYP(IF).NE.MEATYP(JF)   .AND.
c    3           CSESS(1,IF).EQ.CSESS(1,JF)) THEN
c               NFLSES=2
c               FILNUM(2)=JF
c               IFDONE(JF)=1
c             END IF
c1030        CONTINUE
c          ENDIF
        ENDIF
      ELSE
        NFLSES=0
        DO 15 IF=1,NFTOT
          IF (CSESS(1,IF).EQ.SESSID) THEN
            NFLSES=NFLSES+1
            FILNUM(NFLSES)=IF
            IFDONE(IF)=1
          END IF
15      CONTINUE
      END IF
C
      IF (NFLSES.GT.MXCFLS) THEN
        WRITE(LFNERR,16) NFLSES,MXCFLS,SESSID
16      FORMAT(/,' *** SR INISES: TOO MANY FILES IN SESSION',/,
     1                       16X,'NUMBER OF FILES IN SESSION: ',I4,/,
     2                       16X,'MAXIMUM NUMBER OF FILES   : ',I4,/,
     3                       16X,'SESSION IDENTIFIER        : ',A4,/)
        CALL EXITRC(2)
      ENDIF
C
C INITIALIZATION
C --------------
      NSTSES=0
      NSASES=0
      NAMSES=0
      NFRSES=0
      TSESS(1)=1.D20
      TSESS(2)=0.D0
      DO 100 IFIL=1,NFLSES
        IF=FILNUM(IFIL)
ccc        INDARC=ARCINT(IF)
ccc        LEOARC=ARCINT2(IF)
cccC
cccC CHECK CONSISTENCY OF "INDARC" & "LEOARC" WITHIN ONE SESSION
cccC -----------------------------------------------------------
ccc        IF (IFIL.GT.1) THEN
ccc          IF (INDARC.NE.INDOLD) THEN
ccc            WRITE(LFNERR,18) IF, FILNUM(IFIL-1),SESSID
ccc18          FORMAT(/,' *** SR INISES: INCONSISTENT SATELLITE ARCS',
ccc     1               ' FOR FILES OF THE SAME SESSION',/,
ccc     2               16X,'INCONSISTENT FILE NUMBERS: ',2I4,/,
ccc     3               16X,'SESSION IDENTIFIER       : ',A4,/)
ccc            CALL EXITRC(2)
ccc          ENDIF
ccc          IF (LEOARC.NE.INDOLD2) THEN
ccc            WRITE(LFNERR,19) IF, FILNUM(IFIL-1),SESSID
ccc19          FORMAT(/,' *** SR INISES: INCONSISTENT LEO ARCS',
ccc     1               ' FOR FILES OF THE SAME SESSION',/,
ccc     2               16X,'INCONSISTENT FILE NUMBERS: ',2I4,/,
ccc     3               16X,'SESSION IDENTIFIER       : ',A4,/)
ccc            CALL EXITRC(2)
ccc          ENDIF
ccc        ENDIF
ccc        INDOLD=INDARC
ccc        INDOLD2=LEOARC
cccC
cccC IELE0: INDEX FOR SATELLITE NUMBER ARRAY OF STANDARD ORBIT
cccC ---------------------------------------------------------
ccc        IELE0=0
ccc        DO 20 I=1,INDARC-1
ccc          IELE0=IELE0+NUMSAT(I)
ccc20      CONTINUE
C
C SATELLITE NUMBERS, NUMBER OF SATELLITES
C ---------------------------------------
        DO 50 ISATEL=1,NSATEL(IF)
          DO 40 KSAT=1,NSASES
            IF(SATNUM(ISATEL,IF).EQ.SATSES(KSAT))GO TO 50
40        CONTINUE
          NSASES=NSASES+1
C
C CHECK MAXIMUM NUMBER OF SATELLITES IN SESSION
          IF(NSASES.GT.MXCSAT) THEN
            WRITE(LFNERR,49) NSASES,MXCSAT,SESSID
49          FORMAT(/,' *** SR INISES: TOO MANY SATELLITES IN SESSION',/,
     1                        16X,'NUMBER OF SATEL. IN SESSION > ',I4,/,
     2                        16X,'MAXIMUM NUMBER OF SATELLITES: ',I4,/,
     3                        16X,'SESSION IDENTIFIER          : ',A4,/)
            CALL EXITRC(2)
          ENDIF
          SATSES(NSASES)=SATNUM(ISATEL,IF)
50      CONTINUE
C
C STATION NAMES, NUMBER OF STATIONS, COORDINATES
C ----------------------------------------------
        DO 70 IST=1,NDIFF(IF)+1
          IND=STFIL(IST,IF)
          DO 60 KST=1,NSTSES
            IF(STNAME(IND).EQ.STASES(KST)) GOTO 63
60        CONTINUE
          NSTSES=NSTSES+1
          STASES(NSTSES)=STNAME(IND)
63        CALL ELLECC(XSTELL(1,IND),POSECC(1,IST,IF),EXC)
          DO 66 IFRQ=1,NFRFIL(IF)
            DO 65 K=1,3
              WGSSES(K,IST,IFRQ,IFIL)=XSTAT(K,IND)+EXC(K)
65          CONTINUE
            CALL XYZELL(AELL,BELL,DXELL,DRELL,SCELL,
     1                  WGSSES(1,IST,IFRQ,IFIL),ELLSES(1,IST,IFRQ,IFIL))
66        CONTINUE
70      CONTINUE
C
C FIRST AND LAST OBSERVATION EPOCH IN SESSION
C -------------------------------------------
        TIMEND=TIMREF(IF)+(NEPOCH(IF)-1)*IDELTT(IF)/86400.D0
        IF(TIMREF(IF).LT.TSESS(1)) TSESS(1)=TIMREF(IF)
        IF(TIMEND.GT.TSESS(2)) TSESS(2)=TIMEND
C
C NUMBER OF AMBIGUITY PER SESSION
C -------------------------------
        DO 80 IPAR=NPAR,1,-1
          IF(LOCQ(1,IPAR).NE.4)  EXIT
          IF(LOCQ(2,IPAR).NE.IF) CYCLE
          NAMSES=NAMSES+1
80      CONTINUE
C
C NUMBER OF FREQUENCIES IN SESSION AND FREQUENCIES
C ------------------------------------------------
        DO 90 IFRQ=1,NFRFIL(IF)
          DO 85 IFRSES=1,NFRSES
            IF(ICARR(IFRQ,IF).EQ.FRQSES(IFRSES)) GOTO 90
85        CONTINUE
          NFRSES=NFRSES+1
          FRQSES(NFRSES)=ICARR(IFRQ,IF)
          IF(NFRSES.GT.MAXFRS) THEN
            WRITE(LFNERR,89) NFRSES,MAXFRS,SESSID
89          FORMAT(/,' *** SR INISES: TOO MANY FREQUENCIES REQUESTED',/,
     1                           16X,'NUMBER OF FREQUENCIES  : ',I4,/,
     2                           16X,'MAXIMUM NUMBER OF FREQ.: ',I4,/,
     3                           16X,'SESSION IDENTIFIER     : ',A4,/)
            CALL EXITRC(2)
          ENDIF
90      CONTINUE
100   CONTINUE
C
C SET GPS AND GLONASS FREQUENCIES
C -------------------------------
C     SHOULD FRQ REALLY BE SET AT THIS PLACE ?!?
      EPOFRQ(1)=TSESS(1)
      EPOFRQ(2)=TSESS(2)
      CALL DEFREQ(EPOFRQ,NSASES,SATSES,
     1            USEGEOS=USEGEOS,GOBSDEF=GOBSDEF,MEATYPC='U')

C
C GET THE ARC NUMBER FOR ORBIT IMPROVEMENT
C ----------------------------------------
      DTBNDS=1.D0/86400.D0
      INDARC=0
      LEOARC=0
      DO IPAR=1,NPAR
        IF (LOCQ(1,IPAR).NE.3) CYCLE
        IF (LOCQ(6,IPAR).NE.1) CYCLE
        IARC=LOCQ(2,IPAR)
        ISVN=LOCQ(3,IPAR)
C LAGEOS is not treated as LEO satellite
        IF (ISVN.GT.900 .AND. ISVN.LT.951) THEN
          IF (LEOARC.EQ.0) LEOARC=-1
          IF (TBOUND2(1,IARC)-DTBNDS.LE.TSESS(1).AND.
     1        TBOUND2(2,IARC)+DTBNDS.GE.TSESS(2)) THEN
            LEOARC=IARC
          ENDIF
        ELSE
          IF (INDARC.EQ.0) INDARC=-1
          IF (TBOUND(1,IARC)-DTBNDS.LE.TSESS(1).AND.
     1        TBOUND(2,IARC)+DTBNDS.GE.TSESS(2)) THEN
            INDARC=IARC
          ENDIF
        ENDIF
      ENDDO
C
C SESSION OVER SEVERAL ARCS IS NOT ALLOWED FOR ORBIT IMPROVEMENT
      IF (INDARC.EQ.-1) THEN
        CALL TIMST2(1,2,TSESS,SESSTR)
        WRITE(LFNERR,'(/,A,/,16X,A,2(/,16X,A,A),/)')
     1  ' *** SR INISES: The satellite arcs do not coincide ' //
     2  'with the sessions.',
     3  'No orbit improvement is possible in that case.',
     4  'Session identifier       : ',TRIM(SESSID),
     5  'Time interval            : ',TRIM(SESSTR)
      ENDIF
C
      IF (LEOARC.EQ.-1) THEN
        CALL TIMST2(1,2,TSESS,SESSTR)
        WRITE(LFNERR,'(/,A,/,16X,A,2(/,16X,A,A),/)')
     1  ' *** SR INISES: The LEO arcs do not coincide ' //
     2  'with the sessions.',
     3  'No orbit improvement is possible in that case.',
     4  'Session identifier       : ',TRIM(SESSID),
     5  'Time interval            : ',TRIM(SESSTR)
      ENDIF
C
      IF (INDARC.EQ.-1.OR.LEOARC.EQ.-1) CALL EXITRC(2)
C
C NO ORBIT ESTIMATION
      IF (INDARC.EQ.0) INDARC=ARCINT (FILNUM(1))
      IF (LEOARC.EQ.0) LEOARC=ARCINT2(FILNUM(1))
C
C IELE0: INDEX FOR SATELLITE NUMBER ARRAY OF STANDARD ORBIT
C ---------------------------------------------------------
      IELE0=0
      DO 20 I=1,INDARC-1
        IELE0=IELE0+NUMSAT(I)
20    CONTINUE
C
C SET FREQUENCIES FOR MELBOURNE-WUEBBENA CODE
C -------------------------------------------
      IF (MELWUB.EQ.1) THEN
        NFRFIL(FILNUM(2))=2
        ICARR(1,FILNUM(2))=1
        ICARR(2,FILNUM(2))=2
      ENDIF
C
C AMBIGUITY FLAGS, POLARIZATION EFFECT
C ------------------------------------
      DO 30 IFIL=1,NFLSES
      DO 30 IFRQ=1,MXCFRQ
      DO 30 IAMB=1,MXCAMB
        IAMB1(IAMB,IFRQ,IFIL)='U'
        POLARS(IAMB,IFRQ,IFIL)=0.D0
30    CONTINUE
C
C OPEN CORRECT METEO FILES
C ------------------------
      CALL METEO(1,1,IEXTRA,NSTSES,STASES,0,
     1           0.D0,DUMMY31,DUMMY,DUMMY,0.D0,DUMMY,DUMMY,DUMMY,DUMMY)
C
C INITIALIZE LAST OBS. EPOCH
C --------------------------
      TLAST=0.D0
C
C CHECK COLLISION OF LOGICAL FILE NUMBERS
C ---------------------------------------
      LFNTST=LFN001+(NFLSES-1)
      IF (IEXTRA.EQ.0) THEN
        LFNTOP=LFN002
      ELSE
        LFNTOP=LFNEPH
      END IF
      IF (LFNTST.GE.LFNTOP) THEN
        WRITE(LFNERR,901) SESSID,NFLSES,LFN001,LFNTOP-1
901     FORMAT(/,' *** SR INISES: TOO MANY OBSERVATION FILES ',
     1           'IN SESSION TO BE OPEN SIMULTANEOUSLY',/,
     2       16X,'COLLISION OF LOGICAL FILE NUMBERS ',
     3           '(SEE INCLUDE FILE "LFNUM")',/,
     4       16X,'SESSION IDENTIFIER        : ',A4,/,
     5       16X,'NUMBER OF FILES IN SESSION: ',I4,/,
     6       16X,'STARTING LF-NUMBER        : ',I4,/,
     7       16X,'MAXIMUM LF-NUMBER ALLOWED : ',I4,/)
        CALL EXITRC(2)
      END IF
C
C OPEN OBSERVATION FILES
C ----------------------
      DO 110 IFIL=1,NFLSES
        IF=FILNUM(IFIL)
        LFNOBS=LFN001+(IFIL-1)
        CALL OPNFIL(LFNOBS,OBSFIL(IF),'OLD','UNFORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNOBS,IOSTAT,OBSFIL(IF),'INISES')
110   CONTINUE
C
C IF AMBIGUITIES ARE PRE-ELIMINATED
C ------------------------------------------------------------
      IF (STRAMB(1).EQ.-1) THEN
        IF (OPTDIP(1).EQ.2 .OR. OPTELI(21).EQ.3 .OR.
     1      OPTELI(23).EQ.3.OR. OPTELI(24).EQ.3) THEN
C
C CHECK MAXIMUM NUMBER OF AMBIGUITIES AND PARAMETERS
          IF(NAMSES.GT.MAXAMP) THEN
            WRITE(LFNERR,113) NAMSES,MAXAMP
113         FORMAT(/,' *** SR INISES: TOO MANY AMBIGUITY PARAMETERS',/,
     1                           16X,'NUMBER OF AMBIGUITIES :',I4,/,
     2                           16X,'MAXIMUM NUMBER OF AMB.:',I4,/)
            CALL EXITRC(2)
          ENDIF
C
          NPASES=NPARN+NAMSES
C
          IF(NPASES.GT.MAXPAR) THEN
            WRITE(LFNERR,114) NPASES,MAXPAR
114         FORMAT(/,' *** SR INISES: TOO MANY PARAMETERS',/,
     1                           16X,'NUMBER OF PARAMETERS:',I4,/,
     2                           16X,'MAXIMUM NUMBER      :',I4,/)
            CALL EXITRC(2)
          ENDIF
C
          DO 115 K=1,NAMSES
            BNOR(NPARN+K)=0.D0
            DO 115 I=1,K+NPARN
              IK=I+(K+NPARN-1)*(K+NPARN)/2
              ANOR(IK)=0.D0
115       CONTINUE
C
        ELSE
          NPASES=NPARN
        END IF
      END IF
C
C INITIALIZE A PRIORI SYNCHRONIZATION ERROR
C -----------------------------------------
      DO 130 IFIL=1,NFLSES
        SYNCM(IFIL)=0.D0
        DO 120 I=1,NSASES
          ISYNC(I,IFIL)=0
120     CONTINUE
130   CONTINUE
C
C WRITE TITLE FOR ELEVATIONS (AS SEEN FROM STATION 1)
C ---------------------------------------------------
      IF(IFRPRI.EQ.1) THEN
        IFRPRI=0
        WRITE(LFNPRT,141) TITLES
141     FORMAT(//,A132,/,A132,/,' ',131('-'),//)
C
        WRITE(LFNPRT,"(
     1       ' 12. TEST OUTPUT'
     2    ,/,' ---------------')")
C
        IF(IELEV.EQ.1) THEN
          WRITE(LFNPRT,143)
143       FORMAT(/,' SATELLITE ELEVATIONS:',/,' ',20('-'))
        ENDIF
      ENDIF
      IF(IELEV.EQ.1) THEN
        TPRINT=0.D0
        IF(IFRPR1.EQ.1) THEN
          IFRPR1=0
        ELSE
          WRITE(LFNPRT,146)
146       FORMAT(' ')
        ENDIF
        IFIL1=FILNUM(1)
        DO K=1,NSASES
          CALL SVN2CHR(SATSES(K),ISVMOD,SVNCHR)
          WRITE(NAVPRT(K),"(A1,I2.2)") SVNCHR,ISVMOD
        ENDDO
        WRITE(LFNPRT,144) SESSID,IFIL1,STNAME(STFIL(1,IFIL1)),
     1                    (NAVPRT(K),K=1,NSASES)
144     FORMAT(/,' SESSION : ',A4,/,
     1           ' FILE    : ',I4,/,
     2           ' STATION : ',A16,//,
     3           ' IOBS',84(1X,A3))
        WRITE(LFNPRT,145)
145     FORMAT(' ',131('-'),/)
      ENDIF
C
C INIT STATISTICS ON PHASE-CONNECTED EPOCHS
C -----------------------------------------
      DO IFIL=1,NFLSES
        DO I=0,MAXSYS
          ICLU(I,1:MXCAMB,IFIL)=0
        ENDDO
        DO I=1,NSASES
          OBSEPO(I,1:NFRSES,IFIL)=0
        ENDDO
      ENDDO
C
      RETURN
      END SUBROUTINE

      END MODULE

