      MODULE s_R2WTNR
      CONTAINS

C*
      SUBROUTINE R2WTNR(LFNNAV,LFNERR,IRXVRS,ISVN,EPHDAT,IRCODE)
CC
CC NAME       :  R2WTNR
CC
CC PURPOSE    :  WRITE OBSERVATION RECORDS OF A
CC               RINEX NAVIGATION MESSAGE FILE (VERSION 1 OR 2)
CC
CC PARAMETERS :
CC         IN :  LFNNAV : LOGICAL FILE NUMBER                  I*4
CC               LFNERR : LFN FOR ERROR MESSAGES               I*4
CC               IRXVRS : RINEX VERSION NUMBER                 I*4
CC               ISVN   : PRN OF SATELLITE                     I*4
CC               EPHDAT : VECTOR WITH MESSAGE DATA             R*8(*)
CC                        EPHDAT(1): TOC
CC                          (2)-(4): A0,A1,A2
CC                              (5): AODE
CC                              (6): CRS
CC                                 .
CC                             (13): TOE
CC                                 .
CC                             (28): AODC
CC                             (29): TRANSM.TIME OF MESSAGE
CC                                 .
CC                             (32): SPARE
CC                                   (29-32: VERSION 2 ONLY)
CC        OUT :  IRCODE : RETURN CODE (0=OK)                   I*4
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  W. GURTNER
CC
CC VERSION    :  3.4  (JAN 94)
CC
CC CREATED    :  30-JUN-92 17:50
CC
CC CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1992     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE s_jmt
      USE s_radgms
      USE f_gpsmjd
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I2    , IDAY  , IHOUR , IRCODE, IRXVRS, ISVN  ,
     1          ITYP  , IYEAR , K     , LFNERR, LFNNAV, MINUTE, MONTH ,
     2          NWEEK
C
      REAL*8    DAY   , SEC   , TOC
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C GLOBAL DECLARATIONS
C -------------------
      REAL*8       EPHDAT(*)
C
C  LOCAL DECLARATIONS
C  ------------------
      CHARACTER    CHR*1
C
      DATA ITYP/3/
C
C  ASSUME: GPS WEEK BELONGS TO TOE
      NWEEK=IDNINT(EPHDAT(23))
C
C  CORRECT GPS WEEK IF TOC IS IN OTHER WEEK
      IF(EPHDAT(1)-EPHDAT(13).GT.+302400.D0) NWEEK=NWEEK-1
      IF(EPHDAT(1)-EPHDAT(13).LT.-302400.D0) NWEEK=NWEEK+1
C
C  TOC IN MOD.JUL.DATE
      TOC=GPSMJD(EPHDAT(1),NWEEK)
      CALL JMT(TOC,IYEAR,MONTH,DAY)
      CALL RADGMS(ITYP,DAY,CHR,IHOUR,MINUTE,SEC)
      IDAY=IDINT(DAY)
      IYEAR=MOD(IYEAR,100)
C
C RECORD 1
      WRITE(LFNNAV,1) ISVN,IYEAR,MONTH,IDAY,IHOUR,MINUTE,SEC,
     1                (EPHDAT(K),K=2,4)
1     FORMAT(I2,5I3,F5.1,3D19.12)
C
C RECORDS 2-7 (8)
      IF(IRXVRS.EQ.1) THEN
        I2=25
      ELSE
        I2=29
      END IF
      DO 20 I=5,I2,4
        WRITE(LFNNAV,2) (EPHDAT(K),K=I,I+3)
2       FORMAT(3X,4D19.12)
20    CONTINUE
C
900   IRCODE=0
      RETURN
      END SUBROUTINE

      END MODULE
