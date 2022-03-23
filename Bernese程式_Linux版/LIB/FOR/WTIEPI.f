      MODULE s_WTIEPI
      CONTAINS

C*
      SUBROUTINE WTIEPI(LFN   ,POLTIM,POLCOO,POLRAT,RMSCOO,
     1                  RMSRAT,POLCOR,NRFPOL)
CC
CC NAME       :  WTIEPI
CC
CC PURPOSE    :  WRITE ONE SET OF POLE COORDINATES (IERS FORMAT)
CC
CC PARAMETERS :
CC         IN :  LFN     :  LOGICAL FILE NUMBER                  I*4
CC               POLTIM  :  TIME OF PARAMETER SET (MJD)          R*8
CC               POLCOO(I): (I=1,5);  POLE COORDINATES           R*8(5)
CC                          I=1 :VALUE OF X-POLE (ARC SECONDS)
CC                          I=2 :VALUE OF Y-POLE (ARC SECONDS)
CC                          I=3 :VALUE OF UT1-UTC (SECONDS)
CC                          I=4 :DELTA EPSILON (ARC SECONDS)
CC                          I=5 :DELTA PSI (ARC SECONDS)
CC               POLRAT(I): (I=1,5);  POLE RATES                 R*8(5)
CC                          I=1 :RATE VALUE OF X-POLE (ARC SEC/D)
CC                          I=2 :RATE VALUE OF Y-POLE (ARC SEC/D)
CC                          I=3 :RATE VALUE OF UT1-UTC (SEC/D)
CC                          I=4 :RATE DELTA EPSILON (ARC SEC/D)
CC                               NOT YET IMPLEMENTED (=0)
CC                          I=5 :RATE DELTA PSI (ARC SEC/D)
CC                               NOT YET IMPLEMENTED (=0)
CC               RMSCOO(I): (I=1,5);  RMS OF POLE COORDINATES    R*8(5)
CC                          I=1 :RMS OF X-POLE (ARC SECONDS)
CC                          I=2 :RMS OF Y-POLE (ARC SECONDS)
CC                          I=3 :RMS OF UT1-UTC (SECONDS)
CC                          I=4 :RMS OF DELTA EPS (ARC SEC)
CC                          I=5 :RMS OF DELTA PSI (ARC SEC)
CC               RMSRAT(I): (I=1,5);  RMS OF POLE RATES          R*8(5)
CC                          I=1 :RMS OF X-POLE RATE (ARC SEC/D)
CC                          I=2 :RMS OF Y-POLE RATE (ARC SEC/D)
CC                          I=3 :RMS OF UT1-UTC RATE (SEC/D)
CC                          I=4 :RMS OF DELTA EPSILON RATE (ARC SEC/D)
CC                               NOT YET IMPLEMENTED (=0)
CC                          I=5 :RMS OF DELTA PSI RATE (ARC SEC/D)
CC                               NOT YET IMPLEMENTED (=0)
CC               POLCOR(I): (I=1,10); CORRELATION VALUES         R*8(10)
CC                          I=1 : X-Y CORRELATION
CC                          I=2 : X-UT1 CORRELATION
CC                          I=3 : NOT YET USED (=0)
CC                          I=4 : NOT YET USED (=0)
CC                          I=5 : Y-UT1 CORRELATION
CC                          I=6 : NOT YET USED (=0)
CC                          I=7 : NOT YET USED (=0)
CC                          I=8 : NOT YET USED (=0)
CC                          I=9 : NOT YET USED (=0)
CC                          I=10: NOT YET USED (=0)
CC               NRFPOL(I): (I=1,3);  NO. OF USED RECEIVERS/SAT. I*4(3)
CC                          I=1 : NO. OF USED RECEIVERS
CC                          I=2 : NO. OF RECEIVERS WITH 'FIXED'
CC                                COORINATES
CC                          I=3 : NUMBER OF USED SATELLITES
CC
CC REMARKS    :  IERS POLE FORMAT VERSION 2
CC
CC AUTHOR     :  D.INEICHEN
CC
CC VERSION    :  4.1
CC
CC CREATED    :  16-JUL-98
CC
CC CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1998     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I1    , LFN
C
      REAL*8    POLTIM
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C
C DECLARATIONS
C ------------
      INTEGER*4    NRFPOL(3),IPOLCO(5),IRMSCO(5),IPOLRA(5)
      INTEGER*4    IRMSRA(5),ICORRE(10)
C
      REAL*8       POLCOO(5),POLRAT(5),RMSCOO(5),RMSRAT(5)
      REAL*8       POLCOR(10)
C
C CHANGE UNITS
C ------------
      DO I=1,5
        IF (I.NE.3) THEN
          IPOLCO(I)=IDNINT(POLCOO(I)*1000000.D0)
          IRMSCO(I)=IDNINT(RMSCOO(I)*1000000.D0)
          IF (I.LE.2) THEN
            IPOLRA(I)=IDNINT(POLRAT(I)*1000000.D0)
            IRMSRA(I)=IDNINT(RMSRAT(I)*1000000.D0)
          ENDIF
        ELSE
          IPOLCO(I)=IDNINT(POLCOO(I)*10000000.D0)
          IRMSCO(I)=IDNINT(RMSCOO(I)*10000000.D0)
          IPOLRA(I)=-IDNINT(POLRAT(I)*10000000.D0)
          IRMSRA(I)=IDNINT(RMSRAT(I)*10000000.D0)
        ENDIF
      ENDDO
      DO I=1,10
        ICORRE(I)=IDNINT(POLCOR(I)*100.D0)
      ENDDO
C
C WRITE ONE SET OF POLE COORDINATES TO THE FILE
C ---------------------------------------------
C      TIMSPAN=(TAECMP(2,1)-TAECMP(1,1))*24.D0
C      WRITE(LFN,3) TSAV,(POLCOR(I1),I1=1,3),(ERR(I1),I1=1,3),RMS*6.D0,
C     1             CORREL(1),CORREL(2),CORREL(5),
C     2             NSTA,NSAT,0,72.0
C3     FORMAT(F11.5,2F9.5,F10.6,2F8.5,F9.6,F7.4,2F9.5,F10.6,3I6,
C     1       F9.1)
C
C       WRITE(LFN,2) TSAV,(IPOLCO(I1),I1=1,3),IRATE(3),(IERR(I1),I1=1,3),
C     1              IRASIG(3),NSTA,NFIX,NSAT,(IRATE(I1),I1=1,2),
C     2              (IRASIG(I1),I1=1,2),(CORREL(I1),I1=1,2),CORREL(5),
C     3              IPOLCO(5),IPOLCO(4),IERR(5),IERR(4)
C2      FORMAT(F8.2,3I8,I6,4I5,3I4,2I6,2I5,3F6.2,2I6,2I5)
C
       WRITE(LFN,5) POLTIM,(IPOLCO(I1),I1=1,3),IPOLRA(3),
     1              (IRMSCO(I1),I1=1,3),IRMSRA(3),(NRFPOL(I1),I1=1,3),
     2              (IPOLRA(I1),I1=1,2),(IRMSRA(I1),I1=1,2),
     3              (ICORRE(I1),I1=1,2),ICORRE(5),IPOLCO(5),IPOLCO(4),
     4              IRMSCO(5),IRMSCO(4)
5      FORMAT(F8.2,3I9,I7,4I6,3I4,2I7,2I6,3I5,2I7,2I6)
C
999    RETURN
       END SUBROUTINE

      END MODULE
