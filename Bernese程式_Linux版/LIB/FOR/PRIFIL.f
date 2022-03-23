      MODULE s_PRIFIL
      CONTAINS

C*
      SUBROUTINE PRIFIL(NFTOT ,ICAMPN,ICAMP ,MEATYP,NFREQ ,NEPOCH,
     1                  NSATEL,CSESS ,ICLOCK,IDELTT,TIMREF,IRMARK,
     2                  NEPFLG,AMBDEF,AMBSAV,NUMAMB,AMBCLS,NFRFIL,
     3                  ICARR ,STNAME,STFIL ,ARCINT,NDIFF ,HEADER,
     4                  OBSFIL,AMBCLU,RECTYP,ANTTYP)
CC
CC NAME       :  PRIFIL
CC
CC PURPOSE    :  PRINT MAIN CHARACTERISTICS OF OBSERVATION FILES
CC
CC PARAMETERS :
CC         IN :  NFTOT  : TOTAL NUMBER OF FILES               I*4
CC               ICAMPN(I),I=1,..,NFTOT: CAMPAIGN NUMBER OF   I*4
CC                        FILE I
CC               ICAMP  : CAMPAIGN NUMBER TO BE PRINTED       I*4
CC               MEATYP(I),I=1,..,NFTOT: MEASUREMENT TYPES    I*4
CC               NFREQ(I),I=1,..,NFTOT: NUMBER OF FREQ. IN    I*4
CC                        IN FILE I
CC               NEPOCH(I),I=1,..,NFTOT: NUMBER OF EPOCHS     I*4
CC               NSATEL(I),I=1,..,NFTOT: NUMBER OF SATELLITES I*4
CC                        IN FILE I
CC               CSESS(2,I),I=1,..,NFTOT: SESSION IDENTIFIERS CH*4
CC               ICLOCK(J,I),J=1,2,I=1,..,NFTOT: TYPE OF      I*4
CC                        CLOCK MODELLING IN FILE I
CC               IDELTT(I),I=1,..,NFTOT: OBSERVATION INTERVAL I*4
CC               TIMREF(I),I=1,..,NFTOT: REFERENCE TIME       R*8
CC               IRMARK(I),I=1,..,NFTOT: REMARK NUMBER        I*4
CC               NEPFLG(I),I=1,..,NFTOT: # EPOCHS FLAGGED     I*4
CC               AMBDEF(I),I=1,..,NFTOT: AMBIGUITY INITIALI-  I*4
CC                        ZATION TYPE
CC               AMBSAV(I),I=1,..,NFTOT: SAVE AMBIGUITIES     I*4
CC                        (SAVE=1, NO SAVE=0)
CC               NUMAMB(I) : I=1,..,NFTOT:                    I*4
CC                        NUMBER OF AMBIGUITIES
CC               AMBCLS : AMBIGUITY CLUSTERS                  I*4(*,3,*)
CC                          AMBCLS(I,K,J): AMBIGUITY NUMBER I
CC                                         FREQUENCY NUMBER K
CC                                         FILE             J
CC                            K=1: L1 AMBIGUITIES
CC                            K=2: L2 AMBIGUITIES
CC                            K=3: L5 AMBIGUITIES (WIDELANE)
CC               NFRFIL(I),I=1,..,NFTOT: NUMBER OF REQUESTED  I*4
CC                        CARRIERS
CC               ICARR(J,I),J=1,..,NFRFIL(I),I=1,..,NFTOT:    I*4
CC                        CARRIERS
CC               STNAME(I),I=1,..,NSTAT: STATION NAMES        CH*16
CC               STFIL(2,I),I=1,..,NFTOT: STATION NUMBERS OF  I*4
CC                        FILE I
CC               ARCINT(I),I=1,..,NFTOT: ARC NUMBER FOR       I*4
CC                        FILE I
CC               NDIFF(I):I=1,..,NFTOT: DIFFERENCE TYPE       I*4
CC                        NDIFF=0: ZERO DIFFERENCE
CC                        NDIFF=1: SINGLE DIFFERENCE
CC               HEADER(I),I=1,..,NFTOT: HEADER FILE NAMES    CH*(*)
CC               OBSFIL(I),I=1,..,NFTOT: OBSERV. FILE NAMES   CH*(*)
CC               RECTYP(K,I),K=1,2,I=...: RECEIVER TYPES      CH*20
CC                        TYPES
CC               ANTTYP(K,I),K=1,2,I=1,..,NFTOT: ANTENNA      CH*20
CC                        TYPES
CC      LOCAL :  AMBCLU(I),I=1,..,NUMAMB: ARRAY TO SAVE TEMP- I*4(*)
CC                        ORARILY THE CLUSTER NUMBERS
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/18 14:49
CC
CC CHANGES    :  27-MAY-91 : ??: DON'T PRINT TRAILING BLANKS
CC               21-DEC-93 : MR: PRINT AMBIGUITY INFO DIFFERENTLY
CC               12-AUG-94 : MR: FORMAT 4: SESSION AS CHARACTER*4
CC               06-MAY-96 : TS: CORRECT PRINTING ZERO-DIFFERENCE FILES
CC               14-JAN-97 : MR: PRINT HEADER AND OBS. FILE NAMES
CC               06-AUG-01 : DS: NEW PARAMETER: RECTYP
CC               05-MAR-03 : CU: REMOVE USE OF SKELETON FILE
CC               15-APR-03 : CU: BUG FIXED (FORMAT STATEMENTS)
CC               17-APR-03 : MR: ADD RECEIVER1/2 TO HEADLINE
CC               11-AUG-03 : RS: NEW PARAMETER ANTTYP, PRINT ANTTYP
CC                               INSTEAD OF RECTYP
CC               08-SEP-03 : HU: RECNAM CHR16 -> CHR20, FILENAMES CHR(*)
CC               10-SEP-03 : HU: PRINT RECTYP, NOT ANTTYP
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               01-DEC-05 : CU: PRINT R1,R2 FOR RANGE MEASUREMENTS
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE s_jmt
      USE s_radgms
      USE f_lengt1
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IAM3  , IAMB  , IAML12, IAMWDL, ICAMP , ICLU  ,
     1          IDAY  , IF    , IFR   , IHOUR , IMIN  , IMONTH, ISEC  ,
     2          ISTAT , IYEAR , MXCAMB, MXCFRQ, MXCSAT, NAMB  ,
     3          NFTOT
C
      REAL*8    DAY   , SEC
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*132 TEXT
      CHARACTER*(*) HEADER(*),OBSFIL(*)
      CHARACTER*16  STNAME(*)
      CHARACTER*20  RECTYP(2,*),ANTTYP(2,*)
      CHARACTER*6   MXNSAT,MXNAMB,MXNFRQ
      CHARACTER*4   CSESS(2,*)
      CHARACTER*1   MTYP(3),VORZ
      CHARACTER*1   NOYES(2)
      CHARACTER*1   FREQCHAR
C
      REAL*8        TIMREF(*)
C
      INTEGER*4     ICAMPN(*),MEATYP(*),NFREQ(*),NEPOCH(*),NEPFLG(*)
      INTEGER*4     NSATEL(*),ICLOCK(2,*),IDELTT(*)
      INTEGER*4     IRMARK(*),NUMAMB(*),AMBCLS(MXCAMB,3,*)
      INTEGER*4     NFRFIL(*),ICARR(MXCFRQ,*),STFIL(2,*),ARCINT(*)
      INTEGER*4     AMBDEF(*),AMBSAV(*),AMBCLU(*),NCLUST(3),NDIFF(*)
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMAMB/MXCAMB,MXNAMB
      COMMON/MCMFRQ/MXCFRQ,MXNFRQ
C
      DATA MTYP/'P','C','R'/
      DATA NOYES/'N','Y'/
C
C PRINT TITLE LINES
C -----------------
      WRITE(LFNPRT,"(
     1     ' MAIN CHARACTERISTICS:'
     2  ,/,' --------------------'
     3  ,/,' '
     4  ,/,' FILE  OBSERVATION FILE HEADER          OBSERVATION FILE  '
     4    ,'                SESS     RECEIVER 1            RECEIVER 2'
     5  ,/,1X,131('-')
     6  ,/,1X)")
C
C PRINT LIST OF HEADER AND OBSERVATION FILE NAMES
C -----------------------------------------------
      DO 150 IF=1,NFTOT
        IF(ICAMPN(IF).NE.ICAMP) GOTO 150
        WRITE(LFNPRT,151) IF,HEADER(IF)(1:32),OBSFIL(IF)(1:32),
     1        CSESS(1,IF),(RECTYP(I,IF),I=1,NDIFF(IF)+1)
151     FORMAT(' ',I4,2X,A32,1X,A32,2X,A4,5X,A20,2X,A20)
150   CONTINUE
C
C PRINT HEADER FOR CHARACTERISTICS
C --------------------------------
      WRITE(LFNPRT,"(
     1     ' '
     2  ,/,100X,'   AMB.I.+S.      #CLUSTERS'
     3  ,/,' FILE TYP FREQ.  STATION 1        STATION 2        SESS '
     3    ,' FIRST OBSERV.TIME  #EPO  DT #EF #CLK ARC #SAT  W 12    '
     3    ,'#AMB  L1  L2  L5  RM'
     4  ,/,1X,131('-')
     5  ,/,1X)")
C
C PRINT MAIN CHARACTERISTICS OF OBSERVATION FILES
C -----------------------------------------------
      DO 200 IF=1,NFTOT
        IF(ICAMPN(IF).NE.ICAMP) GOTO 200
C
C REFERENCE EPOCH
        CALL JMT(TIMREF(IF),IYEAR,IMONTH,DAY)
        IYEAR=MOD(IYEAR,100)
        IDAY=DAY
        CALL RADGMS(3,DAY-IDAY,VORZ,IHOUR,IMIN,SEC)
        ISEC=IDNINT(SEC)
C
C AMBIGUITY INITIALIZATION
        IF (AMBDEF(IF).EQ.1) THEN
          IAMWDL=0
          IAML12=0
        ELSE IF (AMBDEF(IF).EQ.2) THEN
          IAMWDL=1
          IAML12=0
        ELSE IF (AMBDEF(IF).EQ.3) THEN
          IAMWDL=1
          IAML12=1
        ELSE
          IAMWDL=0
          IAML12=1
        ENDIF
C
C NUMBER OF AMBIGUITY CLUSTERS
        IF (NFREQ(IF).EQ.1) THEN
          NAMB=1
        ELSE
          NAMB=3
        ENDIF
        DO 20 IAM3=1,3
          NCLUST(IAM3)=0
20      CONTINUE
        DO 100 IAM3=1,NAMB
          DO 90 IAMB=1,NUMAMB(IF)
            DO 80 ICLU=1,NCLUST(IAM3)
              IF (AMBCLU(ICLU).EQ.AMBCLS(IAMB,IAM3,IF)) GOTO 90
80          CONTINUE
            NCLUST(IAM3)=NCLUST(IAM3)+1
            AMBCLU(NCLUST(IAM3))=AMBCLS(IAMB,IAM3,IF)
90        CONTINUE
100     CONTINUE
C
C WRITE OUTPUT LINE
        IF (NDIFF(IF).EQ.0) THEN
          FREQCHAR = 'L'
          IF (MEATYP(IF) == 3) FREQCHAR = 'R'
          WRITE(TEXT,2,IOSTAT=ISTAT)
     1                IF,MTYP(MEATYP(IF)),
     1                FREQCHAR,(ICARR(1,IF)),
     1                FREQCHAR,(ICARR(2,IF)),
     1                STNAME(STFIL(1,IF)),CSESS(1,IF),
     2                IYEAR,IMONTH,IDAY,IHOUR,IMIN,ISEC,
     3                NEPOCH(IF),IDELTT(IF),NEPFLG(IF),
     4                ICLOCK(1,IF),
     5                ARCINT(IF),NSATEL(IF),
     6                NOYES(IAMWDL+1),NOYES(IAML12+1),
     7                NOYES(AMBSAV(IF)+1),NUMAMB(IF),
     8                (NCLUST(IFR),IFR=1,3),IRMARK(IF)
2         FORMAT(' ',I4,2X,A1,2X,A1,I1,',',A1,I1,2X,A16,5X,'-',12X,A4,
     1           I4,2('-',I2),I3,2(':',I2),I6,2I4,I3,' -',I3,I5,1X,
     2           3(2X,A1),I5,4I4)
          IF(ICLOCK(1,IF).EQ.999) TEXT(89:91)='  E'
        ELSE
          WRITE(TEXT,12,IOSTAT=ISTAT)
     1                IF,MTYP(MEATYP(IF)),(ICARR(I,IF),I=1,2),
     1                (STNAME(STFIL(I,IF)),I=1,2),CSESS(1,IF),
     2                IYEAR,IMONTH,IDAY,IHOUR,IMIN,ISEC,
     3                NEPOCH(IF),IDELTT(IF),NEPFLG(IF),
     4                (ICLOCK(I,IF),I=1,2),
     5                ARCINT(IF),NSATEL(IF),
     6                NOYES(IAMWDL+1),NOYES(IAML12+1),
     7                NOYES(AMBSAV(IF)+1),NUMAMB(IF),
     8                (NCLUST(IFR),IFR=1,3),IRMARK(IF)
12        FORMAT(' ',I4,2X,A1,2X,'L',I1,',L',I1,2X,A16,1X,A16,1X,A4,
     1           I4,2('-',I2),I3,2(':',I2),I6,2I4,I3,I2,I3,I5,1X,
     2           3(2X,A1),I5,4I4)
          IF(ICLOCK(1,IF).EQ.999) TEXT(89:91)='  E'
          IF(ICLOCK(2,IF).EQ.999) TEXT(92:93)=' E'
        ENDIF
C
        IF(TEXT(61:61).EQ.' ') TEXT(61:61)='0'
        IF(TEXT(64:64).EQ.' ') TEXT(64:64)='0'
        IF(TEXT(70:70).EQ.' ') TEXT(70:70)='0'
        IF(TEXT(73:73).EQ.' ') TEXT(73:73)='0'
C
        IF(NFRFIL(IF).EQ.1) TEXT(13:15)=' '
        IF(MEATYP(IF).EQ.1) THEN
          IF(NFREQ(IF).EQ.1) TEXT(121:128)='   -   -'
        ELSE
          TEXT(112:128)='    -   -   -   -'
        ENDIF
        WRITE(LFNPRT,1) TEXT(1:LENGT1(TEXT))
1       FORMAT(A)
C
200   CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
