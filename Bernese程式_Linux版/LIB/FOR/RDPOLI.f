      MODULE s_RDPOLI
      CONTAINS

C*
      SUBROUTINE RDPOLI(LFN,POLTIM,POLCOO,GPSUTC,REM,RMSPOL,IFORM,IEND)
CC
CC NAME       :  RDPOLI
CC
CC PURPOSE    :  READ ONE OBSERVATION OF THE POLE FILE
CC
CC PARAMETER  :
CC        IN  : LFN     : LOGICAL FILE NUMBER                 I*4
CC       OUT  : POLTIM  : TIME OF PARAMETER SET (MJD)         R*8
CC              POLCOO(I): (I=1,5); POLE COORDINATES          R*8(5)
CC                        I=1 :VALUE OF X-POLE (ARC SECONDS)
CC                        I=2 :VALUE OF Y-POLE (ARC SECONDS)
CC                        I=3 :VALUE OF UT1-UTC (SECONDS)
CC                        I=4 :VALUE OF EPS    (ARC SECONDS)
CC                        I=5 :VALUE OF PSI    (ARC SECONDS)
CC              GPSUTC  : GPS-UTC DIFFERENC GIVEN IN SECONDS  R*8
CC              REM     : REMARK                             CH*3
CC              RMSPOL(I): (I=1,5) RMS OF POLE COORDINATES    R*8(5)
CC                        I=1 :RMS OF X-POLE (ARC SECONDS)
CC                        I=2 :RMS OF Y-POLE (ARC SECONDS)
CC                        I=3 :RMS OF UT1-UTC (SECONDS)
CC                        I=4 :RMS OF EPS    (ARC SECONDS)
CC                        I=5 :RMS OF PSI    (ARC SECONDS)
CC              IFORM   : VERSION :0=UNKNOWN 1=NEW 2=OLD      I*4
CC                                 3=SPECIAL FORMAT (NO RMS..)
CC                                 4=NEW WITH DEPS, DPSI
CC              IEND    : =1 : END OF FILE REACHED            I*4
CC                        =2 : ERROR READING POLE RECORD
CC                        =0 : ELSE
CC
CC REMARKS    :
CC
CC AUTHOR     :  S.FANKHAUSER
CC
CC VERSION    :  3.3
CC
CC CREATED    :  91/11/19 12:00
CC
CC CHANGES    :  07-JUL-92 : ??: TEST FOR BLANK LINE AS END OF DATA
CC               02-AUG-92 : ??: ERROR WHILE READING ==> IEND=2
CC               12-FEB-93 : ??: INCLUDE I:COMLFNUM
CC               19-APR-94 : SF: INCLUDE EPSILON AND PSI
CC               13-OCT-94 : MR: REPLACE "X" BY "1X" IN FORMAT 11 & 12
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE f_djul
      USE f_lengt0
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I1    , I2    , IDAY  , IEND  , IFORM , IHOUR ,
     1          IYEAR , LFN   , LL    , MIN   , MONTH
C
      REAL*8    GPSUTC, POLTIM, T
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C GLOBAL DECLARATIONS
C -------------------
      CHARACTER*3 REM
      REAL*8      POLCOO(5),RMSPOL(5)
C
C LOCAL DECLARATIONS
C ------------------
      CHARACTER*132 POLINE
C
C
C INITIALIZATION OF THE VARIABLES
C -------------------------------
      IEND=0
      IYEAR=0
      MONTH=0
      IDAY=0
      IHOUR=0
      MIN=0
      GPSUTC=0.0
      POLTIM=0.0
      REM=' '
      IFORM=0
      DO 10 I1=1,5
        POLCOO(I1)=0.0
        RMSPOL(I1)=0.0
10    CONTINUE
C
C READ INFORMATION FROM FILE
C --------------------------
      READ(LFN,'(A)',END=900) POLINE
      IF (POLINE.EQ.' ') GOTO 900
      LL=LENGT0(POLINE)
      IF (POLINE(5:5).EQ.' '.AND.POLINE(8:8).EQ.' ') THEN
        IF (LL.LT.80) THEN
          IFORM=1
          READ(POLINE,11,ERR=910) IYEAR,MONTH,IDAY,IHOUR,MIN,
     1         (POLCOO(I),I=1,3),GPSUTC,REM,(RMSPOL(I),I=1,3)
11        FORMAT(I4,4(1X,I2),2(1X,F8.5),1X,F9.6,2X,F3.0,
     1           1X,A3,1X,2F8.5,F9.6)
        ELSE
          IFORM=4
          READ(POLINE,12,ERR=910) IYEAR,MONTH,IDAY,IHOUR,MIN,
     1         (POLCOO(I),I=1,3),GPSUTC,REM,(RMSPOL(I),I=1,3),
     2         (POLCOO(I),I=4,5),(RMSPOL(I),I=4,5)
12        FORMAT(I4,4(1X,I2),2(1X,F8.5),1X,F9.6,2X,F3.0,
     1           1X,A3,1X,2F8.5,F9.6,2(1X,F8.5),2F8.5)
        ENDIF
        T=IDAY*1.0d0+IHOUR/24.0d0+MIN/1440.0d0
        POLTIM=DJUL(IYEAR,MONTH,T)
      ELSE IF(POLINE(6:6).EQ.'.') THEN
        IF (POLINE(7:7).EQ.' ') THEN
          IFORM=2
          READ(POLINE,13,ERR=910) POLTIM,(POLCOO(I2),I2=1,3),GPSUTC,REM
13        FORMAT(F6.0,F8.4,F9.4,F10.5,F8.0,16X,A3)
        ELSE
          IFORM=3
          READ(POLINE,*,ERR=910) POLTIM,(POLCOO(I2),I2=1,3),GPSUTC
        END IF
      END IF
      GOTO 999
C
C END OF FILE REACHED
C -------------------
900   CONTINUE
      IEND=1
      GOTO 999
C
C ERROR WHILE READING
C -------------------
910   CONTINUE
      WRITE(LFNERR,901)
901   FORMAT(/,' *** SR RDPOLI: ERROR READING POLE FILE RECORD',/)
      IEND=2
C
999   RETURN
      END SUBROUTINE

      END MODULE
