      MODULE s_RXWTMR
      CONTAINS

C*
      SUBROUTINE RXWTMR(LFNMET,LFNERR,NUMTYP,EPOCH,DATMET,IRCODE)
CC
CC NAME       :  RXWTMR
CC
CC PURPOSE    :  WRITE OBSERVATION RECORDS OF A
CC               RINEX METEOROLOGICAL DATA FILE
CC
CC PARAMETERS :
CC         IN :  LFNMET : LOGICAL FILE NUMBER                  I*4
CC               LFNERR : LFN FOR ERROR MESSAGES               I*4
CC               NUMTYP : NUMBER OF DIFFERENT MET.TYPES        I*4
CC               EPOCH  : OBSERVATION EPOCH (RECEIVER TIME)    R*8
CC               DATMET : LIST OF MET.VALUES                   R*8(*)
CC                        DATMET(J), J: OBS.TYPE
CC        OUT :  IRCODE : RETURN CODE (0=OK)                   I*4
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  W. GURTNER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  89/04/05 12:05
CC
CC CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               29-Feb-12 : RD: USE RXWTMR AS MODULE
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1989     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE s_jmt
      USE s_radgms
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IDAY  , IHOUR , IRCODE, ISEC  , ITYP  , IYEAR , K     ,
     1          LFNERR, LFNMET, MINUTE, MONTH , NUMTYP
C
      REAL*8    DAY   , EPOCH , SEC
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C GLOBAL DECLARATIONS
C -------------------
      REAL*8       DATMET(*)
C
C  LOCAL DECLARATIONS
C  ------------------
      CHARACTER    CHR*1
C
      DATA ITYP/3/
C
C RECORD 1        EPOCH/MET
      CALL JMT(EPOCH,IYEAR,MONTH,DAY)
      CALL RADGMS(ITYP,DAY,CHR,IHOUR,MINUTE,SEC)
      ISEC=IDNINT(SEC)
      IDAY=IDINT(DAY)
      IYEAR=MOD(IYEAR,100)
      WRITE(LFNMET,1) IYEAR,MONTH,IDAY,IHOUR,MINUTE,ISEC,
     1                (DATMET(K),K=1,NUMTYP)
1     FORMAT(6I3,8F7.1)
C
900   IRCODE=0
      RETURN
      END SUBROUTINE

      END MODULE
