      MODULE s_R2WTGR
      CONTAINS

C*
      SUBROUTINE R2WTGR(LFNNAV,LFNERR,IRXVRS,ISVN,EPHDAT,IRCODE)
CC
CC NAME       :  R2WTGR
CC
CC PURPOSE    :  WRITE OBSERVATION RECORDS OF A
CC               RINEX GLONASS NAVIGATION MESSAGE FILE
CC
CC PARAMETERS :
CC         IN :  LFNNAV : LOGICAL FILE NUMBER                  I*4
CC               LFNERR : LFN FOR ERROR MESSAGES               I*4
CC               IRXVRS : RINEX VERSION NUMBER                 I*4
CC               ISVN   : PRN OF SATELLITE                     I*4
CC               EPHDAT : VECTOR WITH MESSAGE DATA             R*8(*)
CC                        EPHDAT( 1)=TB UTC
CC                        EPHDAT( 2)=TAU N
CC                        EPHDAT( 3)=GAMMA N
CC                        EPHDAT( 4)=TK (SECONDS INTO CURRENT UTC DAY)
CC                        EPHDAT( 5)=X0
CC                        EPHDAT( 6)=X1
CC                        EPHDAT( 7)=X2
CC                        EPHDAT( 8)=BN (C/A CODE ONLY)
CC                        EPHDAT( 9)=Y0
CC                        EPHDAT(10)=Y1
CC                        EPHDAT(11)=Y2
CC                        EPHDAT(12)=FREQUENCY NUMBER
CC                        EPHDAT(13)=Z0
CC                        EPHDAT(14)=Z1
CC                        EPHDAT(15)=Z2
CC                        EPHDAT(16)=E (AGE OF OPER. INFORMATION)
CC        OUT :  IRCODE : RETURN CODE (0=OK)                   I*4
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  W. GURTNER
CC
CC VERSION    :  3.4
CC
CC CREATED    :  14-MAR-93
CC
CC CHANGES    :  30-DEC-97 : WG: INCLUDE VERSION 2
CC               21-JAN-98 : WG: ALLOW FOR "FLOATING" VERSION NUMBER
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1993     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE s_jmt
      USE s_radgms
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I2    , IDAY  , IHOUR , IRCODE, IRXVRS, ISVN  ,
     1          ITYP  , IYEAR , K     , LFNERR, LFNNAV, MINUTE, MONTH
C
      REAL*8    DAY   , FLTVRS, SEC
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
      IF(IRXVRS.GE.100) THEN
        FLTVRS=0.01*IRXVRS
      ELSE
        FLTVRS=IRXVRS
      END IF
C
C  TB
      CALL JMT(EPHDAT(1),IYEAR,MONTH,DAY)
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
      I2=13
      DO 20 I=5,I2,4
        WRITE(LFNNAV,2) (EPHDAT(K),K=I,I+3)
2       FORMAT(3X,4D19.12)
20    CONTINUE
C
900   IRCODE=0
      RETURN
      END SUBROUTINE

      END MODULE
