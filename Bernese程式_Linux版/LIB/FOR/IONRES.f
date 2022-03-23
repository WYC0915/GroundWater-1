      MODULE s_IONRES
      CONTAINS

C*
      SUBROUTINE IONRES(NFIL,INDFIL,HEDFIS,OBSFIS,MTYPS,NEPOS,NSATS,
     1                  IDELTS,TIMRES,NUMSAS,XSTAS,XSTELS,PRTLEV,
     2                  ICARR,IQ,INTMAX,RMSOBS,NIONP,MINELV,IDEVLA,
     3                  IDEVHA,IDEVMI,HION,LOCQ,IRSOUT,IFRMAT,
     4                  XX,FLAG,OBSFLG,NRSAT,IFRQS,AMBIEP,NUMAMB,OFFS,
     5                  OFFSET,TIMOBS,DPHI,OBSERV,IONDEV,RESIDU,AMBIE1,
     6                  L4SUM,C4,MODNR,NUMOBS)
CC
CC NAME       :  IONRES
CC
CC PURPOSE    :  COMPUTES RESIDUALS FOR THE SINGLE LAYER MODEL
CC
CC PARAMETERS :
CC         IN :  NFIL   : NUMBER OF FILES OF CURRENT SESSION  I*4
CC               INDFIL : FILE INDEX                          I*4
CC               HEDFIS : HEADER FILE NAMES
CC                        HEDFIS(I),I=1,NFIL                 CH*32(*)
CC               OBSFIS : OBSERVATION FILE NAMES
CC                        OBSFIS(I),I=1,NFIL                 CH*32(*)
CC               MTYPS  : MEASUREMENT TYPE
CC                        MTYPS(I),I=1,NFIL                   I*4(*)
CC               NEPOS  : NUMBER OF EPOCHS
CC                        NEPOS(I),I=1,NFIL                   I*4(*)
CC               NSATS  : NUMBER OF SATELLITES
CC                        NSATS(I),I=1,NFIL                   I*4(*)
CC               IDELTS : SAMPLING RATE
CC                        IDELTS(I),I=1,NFIL                  I*4(*)
CC               TIMRES : REFERENCE EPOCH
CC                        TIMRES(I),I=1,NFIL                  I*4(*)
CC               NUMSAS : SATELLITE NUMBERS
CC                        NUMSAS(I,J),I=1,MAXSAT,J=1,NFIL     I*4(*,*)
CC               XSTAS  : STATION COORDINATES
CC                        XSTATS(I,J,K),I=1,3,J=1,2,K=1,NNFIL R*8(*,*,*)
CC               XSTELS : STATION COORDINATES (ELLIPSOIDAL)
CC                        XSTELS(I,J,K),I=1,3,J=1,2,K=1,NNFIL R*8(*,*,*)
CC               PRTLEV : PRINT LEVEL FOR PREPROCESSING
CC                        (0:NO MESSAGES PRINTED,1: PRINT)    I*4
CC               ICARR  : CARRIER TO BE USED IN PREPROCESSING I*4
CC               IQ     : POLYNOMIAL DEGREE FOR PREPROCESSING I*4
CC               INTMAX : MAXIMUN INTERVAL (MINUTES)          I*4
CC               RMSOBS : RMS OF A SINGLE OBSERVATION         R*8
CC               NIONP  : NUMBER OF IONOSPHERE PARAMETERS     I*4
CC               MINELV : MINIMAL ELEVATION                   I*4
CC               IDEVLA : DEGREE OF DEVELOPMENT IN LATITUDE   I*4
CC               IDEVHA :    "          "          HOUR ANGLE I*4
CC               IDEVMI :    "          "          MIXED      I*4
CC               HION   : HEIGHT OF IONOSPHERIC LAYER         R*8
CC               LOCQ   : PARAMETER DEFINITION
CC                        LOCQ(I,J),I=1,MAXLCQ, J=1,MAXPAR    I*4(*,*)
CC               IRSOUT : OUTPUT FLAG FOR RESIDUALS
CC                        IRSOUT=1: OUTPUT ON SCREEN
CC                              =2: ON FILE I*4
CC               IFRMAT : FILE FORMAT NUMBER
CC                        IFRMAT(I),I=1,NFIL                  I*4(*)
CC        OUT :  XX     : ESTIMATED PARAMETERS
CC                        XX(I),I=1,MAXPAR                    R*8(*)
CC               RMS    : RMS OF UNIT WEIGHT                  R*8
CC               FLAG   : OBSERVATION FLAG
CC                        FLAG(I,J,K),I=1,MAXEPO,J=1,MAXSAT,
CC                                    K=1,2                  CH*1(*,*,*)
CC               OBSFLG : OBSERVATION FLAG
CC                        OBSFLAG(I,J),I=1,MAXSAT,J=1,MAXFRQ CH*1(*,*)
CC               NRSAT  : SATELLITE NUMBERS (ONE EPOCH)
CC                        NRSAT(I),I=1,MAXSAT                 I*4(*)
CC               IFRQS  : FREQUENCIES
CC                        IFRQS(I),I=1,MAXFRQ                 I*4(*)
CC               AMBIEP : NEW AMBIGUTIY AT EPOCH "AMBIEP"
CC                        AMBIEP(I,J),I=1,MAXAMP,J=1,MAXSAT   I*4(*,*)
CC               NUMAMB : NUMBER OF AMBIGUITIES PER SATELLITE
CC                        NUMAMB(I),I=1,MAXSAT                I*4(*)
CC               OFFS   : PHASE OBSERVATION OFFSET
CC                        OFFS(I),I=1,MAXSAT                  R*8(*)
CC               OFFSET : PHASE OBSERVATION OFFSET
CC                        OFFSET(I,J),I=1,MAXAMP,J=1,MAXSAT   R*8(*,*)
CC               TIMOBS : OBSERVATION EPOCHS
CC                        TIMOBS(I),I=1,MAXEPO                R*8(*)
CC               DPHI   : OBSERVATIONS (PHASE OR CODE)
CC                        DPHI(I,J,2),I=1,MAXEPO,J=1,MAXSAT   R*8(*,*,*)
CC               OBSERV : OBSERVATIONS OF ONE EPOCH
CC                        OBSERV(I,J),I=1,MAXSAT,J=1,MAXFRQ   R*8(*,*)
CC               IONDEV : ION. MODEL DESCRIPTION
CC                        IONDEV(8,I),I=1,MAXMOD              R*8(8,*)
CC               RESIDU : RESIDUALS OF ONE EPOCH
CC                        RESIDU(I),I=1,MAXSAT                R*8(*)
CC               AMBIE1 : AUX. ARRAY                          I*4(*,*)
CC               L4SUM  : AUX. ARRAY                          R*8(*,*)
CC               C4     : AUX. ARRAY                          R*8(*,*)
CC               MODNR  : IONOSPHERE MODEL NUMBER             I*4
CC               NUMOBS : AUX. ARRAY                          I*4(*,*)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  U. WILD
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  89/08/04 14:29
CC
CC CHANGES    :  04-JUN-92 : ??: NEW PARAMETERS IN "GETORB" AND "COOTRA"
CC                               OPTION SYSTEM J2000.0
CC               12-JUN-92 : ??: WRITE A FLAG INTO THE RESIDUAL FILE
CC               11-JAN-93 : ??: USE "PRTLV1" TO SUPPRESS PRINTING
CC               15-JUN-93 : ??: TEST FOR TIMOBS(IOBS)=0.D0
CC               03-NOV-93 : ??: HANDLE SATELLITES MISSING IN STD.ORBIT
CC                               IDER=2, XSAT(9) NECESSARY FOR SR TOPSTA
CC               08-APR-94 : MR: ADD PARAM. "azi" TO CALLS OF TOPSTA
CC               14-JAN-97 : TS: CALL OF TOPSTA CORRECTED
CC               27-AUG-02 : RD: TIMS ELEV/AZI TRICK FOR RESIDUAL FILES
CC               04-DEC-02 : RS: "NAD" AND "AZISAT" TO CALL OF TOPSTA
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               25-Oct-10 : CR: New call of TOPSTA
CC               04-MAY-12 : RD: USE DMOD FROM MODULE, USE M_BERN WITH ONLY
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1989     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnprt, lfn001
      USE d_const,  ONLY: PI
      USE l_basfun, ONLY: dmod
      USE f_tstflg
      USE s_cootra
      USE s_getorb
      USE s_ionbsf
      USE s_ionpar
      USE s_ionobs
      USE s_ionprp
      USE s_topsta
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IAMB  , ICARR , ICRARC, IDEG  , IDER  , IDEVHA, IDEVLA,
     1          IDEVMI, IEPOCH, IFIL  , IFLAG , IFLOLD, IFRQ  , II    ,
     2          ILOOP , IND1  , IND2  , INDEX , INTMAX, IOBS  , IORSYS,
     3          IPAR  , IQ    , IRC   , IRSOUT, ISAT  , JAMB  , JCARR ,
     4          JFIL  , JSAT  , KDEG  , LFNRSD, MINELV, MODNR , MODTYP,
     5          MXCAMP, MXCEPO, MXCFIL, MXCFRQ, MXCLCQ, MXCMOD, MXCPAR,
     6          MXCSAT, NAMBP , NFIL  , NFILS , NIONP , NOBS  , NSATEP
C
      REAL*8    AZI   , B0    , D     , DB    , DELTAT, DR    , DRZEN ,
     1          DS    , DT    , ELEV  , ELNUM , FACTOR, HION  , OFFS0 ,
     2          RMSOBS, S0    , SZ    , TIMEND, TOSC  , UT1GPS, XPOL  ,
     3          YPOL  , ZD    , ZEN   , ZP
C
CCC       IMPLICIT REAL*8(A-H,O-Z)
C
C DECLARATIONS
C ------------
      REAL*8        IONDEV(8,MXCMOD),OBSERV(MXCSAT,MXCFRQ)
      REAL*8        DPHI(MXCEPO,MXCSAT,2),TIMOBS(MXCEPO),DX(3)
      REAL*8        XSTAS(3,2,MXCFIL),XSTELS(3,2,MXCFIL),XSAT(9),ELE(7)
      REAL*8        XX(MXCPAR),OFFSET(MXCAMP,MXCSAT)
      REAL*8        OFFS(MXCSAT),TIMRES(MXCFIL)
      REAL*8        RESIDU(MXCSAT)
      REAL*8        L4SUM(MXCAMP,MXCSAT),C4(MXCAMP,MXCSAT)
      REAL*8        NAD,AZISAT,AZISOK
C
      INTEGER*4     NUMAMB(MXCSAT),AMBIEP(MXCAMP,MXCSAT)
      INTEGER*4     AMBIE1(2*MXCAMP+1,MXCSAT)
      INTEGER*4     IFRQS(MXCFRQ),NRSAT(MXCSAT)
      INTEGER*4     MTYPS(MXCFIL),NEPOS(MXCFIL),NSATS(MXCFIL)
      INTEGER*4     IDELTS(MXCFIL)
      INTEGER*4     NUMSAS(MXCSAT,MXCFIL),LOCQ(MXCLCQ,MXCPAR)
      INTEGER*4     PRTLEV,NOBSVN(100),INDSAT(100),INDFIL(MXCFIL)
      INTEGER*4     FILFRQ,IFRMAT(MXCFIL),NUMOBS(MXCAMP,MXCSAT)
      INTEGER*4     PRTLV1
C
      CHARACTER*132 TEXT
      CHARACTER*32  HEDFIS(MXCFIL),OBSFIS(MXCFIL)
      CHARACTER*6   BLANK
      CHARACTER*6   MXNSAT,MXNFIL,MXNLCQ,MXNPAR,MXNMOD,MXNFRQ
      CHARACTER*6   MXNEPO,MXNAMP
      CHARACTER*1   OBSFLG(MXCSAT,MXCFRQ),FLAG(MXCEPO,MXCSAT,2)
      CHARACTER*1   RESFLG
C
C COMMONS
C -------
      COMMON/CIONRS/NOBSVN,INDSAT
      COMMON/MCMFIL/MXCFIL,MXNFIL
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
      COMMON/MCMPAR/MXCPAR,MXNPAR
      COMMON/MCMMOD/MXCMOD,MXNMOD
      COMMON/MCMFRQ/MXCFRQ,MXNFRQ
      COMMON/MCMEPO/MXCEPO,MXNEPO
      COMMON/MCMAMP/MXCAMP,MXNAMP
C
C LOGICAL FILE NUMBERS, CONSTANT
C ------------------------------
C
      DATA  MODTYP/1/,BLANK/'      '/
      DATA  FILFRQ/4/,NFILS/0/,RESFLG/' '/
C
C INITIALIZATION
C --------------
      IF (ICARR .EQ. 3) THEN
        IFRQ = 2
      ELSE IF (ICARR .EQ. 4) THEN
        IFRQ = 1
      ENDIF
C
C INITIALIZE IONOSPHERE PARAMETERS / COMPUTE START/END OF MODEL
C APPLICABILITY / COMPUTE DEVELOPMENT ORIGIN
C -------------------------------------------------------------
      MODNR = MODNR - 1
      CALL IONPAR(NFIL,TIMRES,NEPOS,IDELTS,XSTELS,IDEVLA,IDEVHA,
     1            IDEVMI,IONDEV,LOCQ,NIONP,MODNR)
C
C LOOP OVER ALL INPUT FILES
C -------------------------
      DO 1000 IFIL = 1,NFIL
C
C COMPUTE ORIGINAL FILE NUMBER
C ----------------------------
        JFIL = NFILS + IFIL
        IFLOLD = INDFIL(JFIL)
C
C INITIALIZATIONS (NORMAL EQUATION SYSTEM ETC.)
C ---------------------------------------------
        ILOOP = 1
        IDER = 2
        IEPOCH = 0
        DO 1010 ISAT = 1,MXCSAT
          OFFS(ISAT) = 0.D0
1010    CONTINUE
C
C READ OBSERVATIONS OF ONE FILE
C -----------------------------
        CALL IONOBS(HEDFIS(IFIL),OBSFIS(IFIL),MTYPS(IFIL),TIMRES(IFIL),
     1              ICARR,IDELTS(IFIL),NSATS(IFIL),NUMSAS(1,IFIL),
     2              IFRMAT(IFIL),NOBS,DPHI,FLAG,OBSFLG,NRSAT,IFRQS,
     3              OBSERV,TIMOBS,NOBSVN,TIMEND)
C
C PREPROCESSING
C -------------
CC SUPPRESS MESSAGES
CC        PRTLEV = 0
        PRTLV1=0
        CALL IONPRP(MODTYP,IFIL,MTYPS(IFIL),NOBS,PRTLV1,ICARR,
     1              NSATS(IFIL),NUMSAS(1,IFIL),DPHI,FLAG,IDELTS(IFIL),
     2              TIMRES(IFIL),TIMOBS,IQ,RMSOBS,INTMAX,
     3              LOCQ,NIONP,NUMAMB,AMBIEP,NAMBP,OFFSET)
C
        DO 1030 ISAT = 1,NSATS(IFIL)
          JAMB = 0
          DO 1030 IAMB = 1,NUMAMB(ISAT)
            IF (MTYPS(IFIL) .EQ. 1) THEN
              AMBIE1(JAMB+1,ISAT) = AMBIEP(IAMB,ISAT)
              IF (AMBIEP(IAMB+1,ISAT) .NE. 0) THEN
                AMBIE1(JAMB+2,ISAT) = AMBIEP(IAMB+1,ISAT) - 1
              ELSE
                AMBIE1(JAMB+2,ISAT) = 99999
              ENDIF
              JAMB = JAMB + 2
            ENDIF
C
            C4(IAMB,ISAT) = 0.D0
            L4SUM(IAMB,ISAT) = 0.D0
            NUMOBS(IAMB,ISAT) = 0
C
1030    CONTINUE
C
C COMPUTE ELEMENTS OF FIRST DESIGN MATRIX
C ---------------------------------------
C
        B0=IONDEV(1,MODNR)
        S0=DMOD(IONDEV(2,MODNR),1.D0)*2.D0*PI+IONDEV(3,MODNR)-PI
        S0=DMOD(S0+10.D0*PI,2.D0*PI)
C
C LOOP OVER ALL OBSERVATIONS
C --------------------------
        DO 300 IOBS=1,NOBS
C
          IF (TIMOBS(IOBS).EQ.0.D0) GOTO 300
C
          DELTAT = TIMOBS(IOBS) - TIMRES(IFIL)
          IEPOCH = IDNINT((DELTAT*86400.D0)/IDELTS(IFIL))+1
C
C LOOP OVER ALL SATELLITES
C ------------------------
          DO 200 ISAT=1,NSATS(IFIL)
C
            IF (MTYPS(IFIL) .EQ. 1) THEN
              DO 120 IAMB = 1,NUMAMB(ISAT)
                IF (IEPOCH .EQ. AMBIEP(IAMB,ISAT)) THEN
                  OFFS(ISAT) = OFFSET(IAMB,ISAT)
                ENDIF
120           CONTINUE
            ENDIF
C
C OBSERVATION IN TIME INTERVAL?
C -----------------------------
            IF(TIMOBS(IOBS).LT.IONDEV(4,MODNR).OR.
     1         TIMOBS(IOBS).GT.IONDEV(5,MODNR)) GOTO 200
C
C OBSERVATION EQUAL ZERO OR FLAGED?
C ---------------------------------
            IF (DPHI(IOBS,ISAT,IFRQ) .EQ. 0.D0 .OR.
     1          TSTFLG(FLAG(IOBS,ISAT,IFRQ),0)) GOTO 200
C
C GET SATELLITE COORDINATES
C -------------------------
            DO 130 II = 1,9
              XSAT(II) = 0.D0
130         CONTINUE
            CALL GETORB(NUMSAS(ISAT,IFIL),1,IDER,2,TIMOBS(IOBS),
     1                  ICRARC,IORSYS,XSAT,TOSC,ELE,IRC)
            IF (XSAT(1) .EQ. 0.D0 .OR. IRC.NE.0) GOTO 200
            CALL COOTRA(IORSYS,IDER,TIMOBS(IOBS),XSAT,SZ,XPOL,YPOL,
     1                  UT1GPS)
C
C COMPUTE ZENITH DISTANCE
C -----------------------
            DT = 0.D0
            CALL TOPSTA(XSTAS(1,1,IFIL),XSAT,SZ,DT,XPOL,YPOL,DX,D,
     1                  ZEN,AZI,2,NAD,AZISAT,TIMOBS(IOBS),
     2                  NUMSAS(ISAT,IFIL),IORSYS,AZISOK,0)
            ZD = ZEN*(180.D0/PI)
            ELEV = 90.D0 - ZD
            IF (IDINT(ELEV) .LE. MINELV) GOTO 200
C
C COMPUTE INTERSECTION POINT REC/SAT <--> IONOSPHERE LAYER
C --------------------------------------------------------
            JCARR = 4
            CALL IONBSF(XSTAS(1,1,IFIL),XSAT(1),TIMOBS(IOBS),SZ,ZEN,
     1                  HION,JCARR,B0,S0,IONDEV(6,1),IONDEV(7,1),
     2                  IONDEV(8,1),FACTOR,DB,DS,ZP)
C
C COMPUTE ELECTRON DENSITY IN DIRECTION TO THE SATELLITE
C ------------------------------------------------------
            ELNUM=0.D0
            DO 135 IPAR = 1,NIONP
              KDEG=LOCQ(3,IPAR)
              IDEG=LOCQ(4,IPAR)
              ELNUM = ELNUM + XX(IPAR) * DB**KDEG * DS**IDEG
135         CONTINUE
            DRZEN=FACTOR*ELNUM
            DR=DRZEN/DCOS(ZP)
C
C CORRECT OBSERVATION
C -------------------
            DPHI(IOBS,ISAT,IFRQ) = DPHI(IOBS,ISAT,IFRQ) - OFFS(ISAT)
            IF (MTYPS(IFIL) .EQ. 1) THEN
              DPHI(IOBS,ISAT,IFRQ) = DPHI(IOBS,ISAT,IFRQ) - DR
            ELSE
              DPHI(IOBS,ISAT,IFRQ) = DPHI(IOBS,ISAT,IFRQ) + DR
            ENDIF
C
C COMPUTE AMBIGUITY PARAMETERS (FOR PHASE OBS. ONLY)
C --------------------------------------------------
            IF (MTYPS(IFIL) .EQ. 1) THEN
              JAMB = 0
              DO 220 IAMB = 1,NUMAMB(ISAT)
                IF (IEPOCH .GE. AMBIE1(JAMB+1,ISAT)  .AND.
     1              IEPOCH .LE. AMBIE1(JAMB+2,ISAT)) THEN
                  L4SUM(IAMB,ISAT)=L4SUM(IAMB,ISAT)+DPHI(IOBS,ISAT,IFRQ)
                  NUMOBS(IAMB,ISAT) = NUMOBS(IAMB,ISAT) + 1
                  GOTO 200
                ENDIF
                JAMB = JAMB + 2
220           CONTINUE
            ENDIF
C
C NEXT SATELLITE
C --------------
200       CONTINUE
C
C NEXT EPOCH
C ----------
300     CONTINUE
C
C AMBIGUITY PARAMETERS
C --------------------
        IF (MTYPS(IFIL) .EQ. 1) THEN
          DO 700 ISAT = 1,NSATS (IFIL)
            DO 720 IAMB = 1,NUMAMB(ISAT)
              IF (NUMOBS(IAMB,ISAT) .NE. 0) THEN
                C4(IAMB,ISAT) = L4SUM(IAMB,ISAT)/NUMOBS(IAMB,ISAT)
              ELSE
                C4(IAMB,ISAT) = 0.D0
              ENDIF
720         CONTINUE
700       CONTINUE
        ENDIF
C
C RESIDUALS
C ---------
        DO 800 IOBS=1,NOBS
C
          IF (TIMOBS(IOBS).EQ.0.D0) GOTO 800
C
          DELTAT = TIMOBS(IOBS) - TIMRES(IFIL)
          IEPOCH = IDNINT((DELTAT*86400.D0)/IDELTS(IFIL))+1
C
          NSATEP = 0
C
C LOOP OVER ALL SATELLITES
C ------------------------
          DO 900 ISAT=1,NSATS(IFIL)
C
C OBSERVATION IN TIME INTERVAL?
C -----------------------------
            IF(TIMOBS(IOBS).LT.IONDEV(4,MODNR).OR.
     1         TIMOBS(IOBS).GT.IONDEV(5,MODNR)) GOTO 900
C
C OBSERVATION EQUAL ZERO OR FLAGED?
C ---------------------------------
            IF ((DPHI(IOBS,ISAT,IFRQ) .EQ. 0.D0 .AND.
     1          OFFS(ISAT) .NE. DPHI(IOBS,ISAT,IFRQ)) .OR.
     2          TSTFLG(FLAG(IOBS,ISAT,IFRQ),0)) GOTO 900
C
C GET SATELLITE COORDINATES
C -------------------------
            DO 930 II = 1,9
              XSAT(II) = 0.D0
930         CONTINUE
            CALL GETORB(NUMSAS(ISAT,IFIL),1,IDER,2,TIMOBS(IOBS),
     1                  ICRARC,IORSYS,XSAT,TOSC,ELE,IRC)
            IF (XSAT(1) .EQ. 0.D0 .OR. IRC.NE.0) GOTO 900
            CALL COOTRA(IORSYS,IDER,TIMOBS(IOBS),XSAT,SZ,XPOL,YPOL,
     1                  UT1GPS)
C
C COMPUTE ZENITH DISTANCE
C -----------------------
            DT = 0.D0
            CALL TOPSTA(XSTAS(1,1,IFIL),XSAT,SZ,DT,XPOL,YPOL,DX,D,
     1                  ZEN,AZI,2,NAD,AZISAT,TIMOBS(IOBS),
     2                  NUMSAS(ISAT,IFIL),IORSYS,AZISOK,0)
            ZD = ZEN*(180.D0/PI)
            ELEV = 90.D0 - ZD
            IF (IDINT(ELEV) .LE. MINELV) GOTO 900
C
C STATISTICS
C ----------
            NSATEP = NSATEP + 1
            INDSAT(NSATEP) = ISAT
C
            OFFS0 = 0.D0
            IF (MTYPS(IFIL) .EQ. 1) THEN
              JAMB = 0
              DO 820 IAMB = 1,NUMAMB(ISAT)
                IF (IEPOCH .GE. AMBIE1(JAMB+1,ISAT) .AND.
     1              IEPOCH .LE. AMBIE1(JAMB+2,ISAT)    ) THEN
                  OFFS0 = C4(IAMB,ISAT)
                  GOTO 825
                ENDIF
                JAMB = JAMB + 2
820           CONTINUE
            ENDIF
C
825         RESIDU(ISAT) = OFFS0 - DPHI(IOBS,ISAT,IFRQ)
C
C NEXT SATELLITE
C --------------
900       CONTINUE
C
C OUTPUT OF RESIDUALS
C -------------------
          IF (ILOOP .EQ. 1 .AND. IRSOUT .NE. 2) THEN
            WRITE(LFNPRT,690) IFLOLD,
     1                        (NUMSAS(ISAT,IFIL),ISAT=1,NSATS(IFIL))
690         FORMAT(//,1X,'FILE: ',I5,/,1X,11('-'),/,1X,
     1             'EPOCH    ',20(2X,I2,2X))
            WRITE(LFNPRT,691)
691         FORMAT(1X,131('-'))
            ILOOP = 0
          ENDIF
C
          IF (NSATEP .NE. 0) THEN
            IF (IRSOUT .NE. 2) THEN
              TEXT(1:132) = ' '
C
              WRITE(TEXT(1:9),701) IEPOCH
701           FORMAT(I5)
C
              DO 710 JSAT = 1,NSATS(IFIL)
                IFLAG = 0
                IND1  = 10 + 6*(JSAT-1)
                IND2  = IND1 + 6
                DO 715 INDEX = 1,NSATEP
                  IF (JSAT .EQ. INDSAT(INDEX)) THEN
                    WRITE(TEXT(IND1:IND2),711) RESIDU(JSAT)
711                 FORMAT(F6.2)
                    IFLAG = 1
                  ENDIF
715             CONTINUE
                IF (IFLAG .NE. 1) THEN
                  WRITE(TEXT(IND1:IND2),712) BLANK
712               FORMAT(A6)
                ENDIF
710           CONTINUE
C
              WRITE(LFNPRT,713) TEXT
713           FORMAT(A132)
C
            ENDIF
C
C WRITE RESIDUAL FILE
            IF (IRSOUT .NE. 1) THEN
              LFNRSD = LFN001+10
              DO 750 JSAT = 1,NSATS(IFIL)
                DO 760 INDEX = 1,NSATEP
                  IF (JSAT .EQ. INDSAT(INDEX)) THEN
                    WRITE(LFNRSD) IFLOLD,IEPOCH,FILFRQ,NUMSAS(JSAT,
     1                            IFIL),0,RESIDU(JSAT),RESFLG
                  ENDIF
760             CONTINUE
750           CONTINUE
            ENDIF
C
          ENDIF
C
C NEXT EPOCH
C ----------
800     CONTINUE
C
C NEXT INPUT FILE
C ---------------
1000  CONTINUE
C
C SAVE NUMBER OF FILES
C --------------------
      NFILS = NFILS + IFIL - 1
C
      RETURN
      END SUBROUTINE

      END MODULE
