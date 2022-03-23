      MODULE s_RXWTOR
      CONTAINS

C*
      SUBROUTINE RXWTOR(LFNOBS,LFNERR,MAXSAT,
     1                  NUMTYP,ISNMIN,ISNTHR,ISNMAX,
     2                  EPOCH,IFLAG,NSATEP,SATEPO,
     3                  OBSEPO,SIGNAL,LLI,IRCODE)
CC
CC NAME       :  RXWTOR
CC
CC PURPOSE    :  WRITE OBSERVATION RECORDS OF A
CC               RINEX OBSERVATION FILE
CC
CC PARAMETERS :
CC         IN :  LFNOBS : LOGICAL FILE NUMBER                  I*4
CC               LFNERR : LFN FOR ERROR MESSAGES               I*4
CC               MAXSAT : MAXIMUM NUMBER OF SATELLITES         I*4
CC                        -> CORRESPONDS TO ROW DECLARATIONS OF
CC                           ARRAYS OBSEPO,SIGNAL,LLI
CC               NUMTYP : NUMBER OF DIFFERENT OBS.TYPES        I*4
CC               ISNMIN : MINIMUM S/N RATIO FOR ALL OBS.TYPES  I*4(*)
CC               ISNTHR : THRESHOLD FOR GOOD S/N RATIO         I*4(*)
CC               ISNMAX : MAXIMUM S/N RATIO FOR ALL OBS.TYPES  I*4(*)
CC               EPOCH  : OBSERVATION EPOCH (RECEIVER TIME)    R*8
CC               IFLAG  : FLAG FOR CURRENT RECORD              I*4
CC               NSATEP : NUMBER OF SATELLITES                 I*4
CC               SATEPO : LIST OF SATELLITE NUMBERS            I*4(*)
CC               OBSEPO : LIST OF OBSERVATIONS                 R*8(*,*)
CC                        OBSEPO(I,J), I: SATELLITE, J: OBS.TYPE
CC               SIGNAL : S/N RATIOS                           R*8(*,*)
CC                        SIGNAL(I,J), I: SATELLITE, J: OBS.TYPE
CC               LLI    : LOSS OF LOCK INDICATORS              I*4(*,*)
CC                        LLI   (I,J), I: SATELLITE, J: OBS.TYPE
CC        OUT :  IRCODE : RETURN CODE (0=OK)                   I*4
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  W. GURTNER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  89/04/05 10:00
CC
CC CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               04-MAY-12 : RD: USE DMOD FROM MODULE
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1989     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE l_basfun, ONLY: dmod
      USE s_jmt
      USE s_radgms
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IDAY  , IFLAG , IHOUR , IRCODE, IREC  , ISN   ,
     1          ITYP  , IYEAR , J     , K     , L     , LFNERR, LFNOBS,
     2          MAXSAT, MINUTE, MONTH , NSATEP, NUMREC, NUMTYP
C
      REAL*8    DAY   , EPOCH , OBS   , SEC
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C GLOBAL DECLARATIONS
C -------------------
      INTEGER*4    SATEPO(*),LLI(MAXSAT,*)
      INTEGER*4    ISNMIN(*),ISNTHR(*),ISNMAX(*)
      REAL*8       SIGNAL(MAXSAT,*),OBSEPO(MAXSAT,*)
C
C  LOCAL DECLARATIONS
C  ------------------
      CHARACTER    STRING(5)*16,CHR*1
C
      DATA ITYP/3/
C
      IF(NSATEP.LE.0) GOTO 900
C
C RECORD 1        EPOCH/SAT
      CALL JMT(EPOCH,IYEAR,MONTH,DAY)
      CALL RADGMS(ITYP,DAY,CHR,IHOUR,MINUTE,SEC)
      SEC=DNINT(SEC*1.D6)/1.D6
      IDAY=IDINT(DAY)
      IYEAR=MOD(IYEAR,100)
      WRITE(LFNOBS,1) IYEAR,MONTH,IDAY,IHOUR,MINUTE,SEC,IFLAG,
     1               NSATEP,(SATEPO(K),K=1,NSATEP)
1     FORMAT(5I3,F11.7,19I3)
C
C RECORD 2 FF     OBSERVATIONS
C
C LOOP OVER ALL SATELLITES
      DO 100 I=1,NSATEP
C
C LOOP OVER NECESSARY NUMBER OF RECORDS
        NUMREC=INT(NUMTYP-1)/5+1
        K=0
        DO 120 IREC=1,NUMREC
C
C LOOP OVER OBSERVATION TYPES WITHIN ONE RECORD
          DO 110 L=1,5
            K=K+1
            IF(K.GT.NUMTYP) GOTO 130
            IF(OBSEPO(I,K).NE.0.D0) THEN
C KEEP OBS IN INTERVAL (-1.D9,+1.D9)
              OBS=DMOD(OBSEPO(I,K),1.D9)
C PROJECT S/N RATIO INTO INTERVAL (1,9)
C (5 = THRESHOLD)
              IF(SIGNAL(I,K).NE.0.D0.AND.
     1             ISNMIN(K).NE.ISNTHR(K).AND.
     2             ISNTHR(K).NE.ISNMAX(K)) THEN
                IF(SIGNAL(I,K).LE.ISNTHR(K)) THEN
                  ISN=IDNINT((SIGNAL(I,K)-ISNTHR(K))/
     1                       (ISNMIN( K )-ISNTHR(K))*(1-5)+5)
                ELSE
                  ISN=IDNINT((SIGNAL(I,K)-ISNTHR(K))/
     1                       (ISNMAX( K )-ISNTHR(K))*(9-5)+5)
                END IF
                IF(ISN.LT.1) ISN=1
                IF(ISN.GT.9) ISN=9
              ELSE
                ISN=0
              END IF
              WRITE(STRING(L),111) OBS,LLI(I,K),ISN
111           FORMAT(F14.3,2I1)
              IF(STRING(L)(15:15).EQ.'0') STRING(L)(15:15)=' '
              IF(STRING(L)(16:16).EQ.'0') STRING(L)(16:16)=' '
            ELSE
              STRING(L)=' '
            END IF
110       CONTINUE
C
C  ACTUAL NUMBER OF OBS.TYPES IN CURRENT RECORD
130       L=L-1
          WRITE(LFNOBS,112) (STRING(J),J=1,L)
112       FORMAT(5A16)
120     CONTINUE
100   CONTINUE
C
900   IRCODE=0
      RETURN
      END SUBROUTINE

      END MODULE
