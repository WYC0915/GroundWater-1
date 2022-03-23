      MODULE s_RDIEPI
      CONTAINS

C*
      SUBROUTINE RDIEPI(LFN   ,IFORM ,ICOL  ,POLTIM,POLCOO,POLRAT,
     1                  RMSCOO,RMSRAT,POLCOR,NRFPOL,IRCODE)
CC
CC TITLE      :  READ POLE FILES IN IERS FORMAT
CC
CC PURPOSE    :  READ THE INFORMATION OF POLE FILES IN IERS FORMAT.
CC               3 DIFFERENT IERS POLE FORMATS ARE KNOWN (SEE IFORM).
CC               ALWAYS ONE OBSERVATION WILL BE READ (= INFORMATION
CC               OF ONE LINE OF THE POLE FILE)
CC
CC PARAMETERS :
CC         IN :  LFN     : LOGICAL FILE NUMBER                   I*4
CC               IFORM   : FORMAT OF POLE FILE                   I*4
CC                          0 = 'OLD' IGS/IERS POLE FORMAT
CC                          1 = 'NEW' IGS/IERS POLE FORMAT
CC                          2 =  IGS/IERS POLE FORMAT VERSION 2
CC               ICOL  : COLUMN INDICES FOR OPTIONAL COLUNMS     I*4(11)
CC        OUT :  POLTIM  :  TIME OF PARAMETER SET (MJD)          R*8
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
CC               NRFPOL(I): (I=1,3); NO. OF USED RECEIVERS/SAT   I*4(3)
CC                          I=1 : NO. OF USED RECEIVERS
CC                          I=2 : NO. OF RECEIVERS WITH 'FIXED'
CC                                COORDINATES
CC                          I=3 : NUMBER OF USED SATELLITES
CC               IRCODE:   =1 : END OF FILE REACHED              I*4
CC                         =0 : ELSE
CC
CC REMARKS    :  -
CC
CC AUTHOR     :  D.INEICHEN
CC
CC VERSION    :  4.1
CC
CC CREATED    :  16-JUL-98
CC
CC CHANGES    :  05-OCT-02 : HU: CONFORMING TO VERSION 2 FORMAT
CC                               PARAMETER ICOL ADDED FOR OPTIONAL COLUMNS
CC                               READING OF OLD IGS FORMATS POSSIBLE
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               04-NOV-05 : HB: INPCR8 IS INTEGER*4 AND NOT REAL*8
CC               28-MAR-12 : RD: USE INPCR8 AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1998     UNIVERSITY OF BERN
CC               SWITZERLAND
C*
      USE m_bern,  ONLY: r8b, lfnerr
      USE s_exitrc
      USE f_inpcr8
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , ICOR  , IFORM , IRCINP, IRCODE,
     1          ISTA  , ITMP  , K     , LFN
C
      REAL*8    POLTIM, RMS   , TIMSPN
C
CCC       IMPLICIT INTEGER*4 (I-N)
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C DECLARATIONS
C ------------
      CHARACTER*80  CDUMMY
      CHARACTER*150 LINE
C
      INTEGER*4     NRFPOL(3),IPOLCO(5),IPOLRA(5),IRMSCO(5)
      INTEGER*4     IRMSRA(5),ICORRE(10),ICOL(11), IDUMMY(1)
C
      REAL*8        POLCOO(5),POLRAT(5),RMSCOO(5),RMSRAT(5)
      REAL*8        POLCOR(10)
      REAL(r8b),DIMENSION(0:23) :: VAL
C
C
C INITIALIZATION OF THE VARIABLES
C -------------------------------
      POLTIM=0.D0
      IRCODE=0
      DO I=1,3
        NRFPOL(I)=0
      ENDDO
      DO I=1,5
       POLCOO(I)=0.D0
       POLRAT(I)=0.D0
       RMSCOO(I)=0.D0
       RMSRAT(I)=0.D0
      ENDDO
      DO I=1,10
       POLCOR(I)=0.D0
      ENDDO
C
C READ ONE LINE OF THE POLE FILE
C ------------------------------
      READ(LFN,'(A)',END=950) LINE
      IF (LINE.EQ.' ') GOTO 950
C
C OLD IERS/IGS FORMAT
      IF (IFORM.EQ.0) THEN
        READ(LINE,100,ERR=900) POLTIM,(POLCOO(K),K=1,3),
     1      (RMSCOO(K),K=1,3),RMS,(POLCOR(K),K=1,2),POLCOR(5),
     2      NRFPOL(1),NRFPOL(3),NRFPOL(2),TIMSPN
C
100     FORMAT(F11.5,2F9.5,F10.6,2F8.5,F9.6,F7.4,2F9.5,F10.6,
     1         3I6,F9.1)
C
C NEW IERS/IGS FORMAT
      ELSE IF (IFORM.EQ.1) THEN
        READ(LINE,110,ERR=900) POLTIM,(IPOLCO(K),K=1,3),
     1       IPOLRA(3),(IRMSCO(K),K=1,3),IRMSRA(3),
     2       (NRFPOL(K),K=1,3),(IPOLRA(K),K=1,2),
     3       (IRMSRA(K),K=1,2),(POLCOR(K),K=1,2),
     4       POLCOR(5),IPOLCO(5),IPOLCO(4),IRMSCO(5),
     5       IRMSCO(4)
110     FORMAT(F8.2,3I8,I6,4I5,3I4,2I6,2I5,3F6.2,2I6,2I5)
        DO I=1,5
          IF (I.NE.3) THEN
            POLCOO(I)=IPOLCO(I)/100000.D0
            RMSCOO(I)=IRMSCO(I)/100000.D0
            IF (I.LE.2) THEN
              POLRAT(I)=IPOLRA(I)/100000.D0
              RMSRAT(I)=IRMSRA(I)/100000.D0
            ENDIF
          ELSE
            POLCOO(I)=IPOLCO(I)/1000000.D0
            RMSCOO(I)=IRMSCO(I)/1000000.D0
            POLRAT(I)=-IPOLRA(I)/1000000.D0
            RMSRAT(I)=IRMSRA(I)/1000000.D0
          ENDIF
        ENDDO
C
C IERS/IGS FORMAT 'VERSION 2'
      ELSE IF (IFORM.EQ.2) THEN
        ITMP=INDEX(LINE,'.')
        ICOR=INDEX(LINE(ITMP+1:150),'.')

C
C       INITIALIZE NOT FOUND VALUES
        DO I=0,23
          VAL(I)=0D0
        ENDDO
C
        ISTA = INPCR8(-150,23,0,LINE,VAL(1),CDUMMY,IDUMMY,IRCINP)
C
        IF(IRCINP.GE.1 .AND. IRCINP.LE.7) GOTO 905
C
        POLTIM   =VAL(1)
        IPOLCO(1)=NINT(VAL(2))
        IPOLCO(2)=NINT(VAL(3))
        IPOLCO(3)=NINT(VAL(4))
        IPOLRA(3)=NINT(VAL(5))
        IRMSCO(1)=NINT(VAL(6))
        IRMSCO(2)=NINT(VAL(7))
        IRMSCO(3)=NINT(VAL(8))
        IRMSRA(3)=NINT(VAL(9))
        NRFPOL(1)=NINT(VAL(10))
        NRFPOL(2)=NINT(VAL(11))
        NRFPOL(3)=NINT(VAL(12))
        IPOLRA(1)=NINT(VAL(ICOL(1)))
        IPOLRA(2)=NINT(VAL(ICOL(2)))
        IRMSRA(1)=NINT(VAL(ICOL(3)))
        IRMSRA(2)=NINT(VAL(ICOL(4)))
        IF (ICOR.EQ.0) THEN
          ICORRE(1)=NINT(VAL(ICOL(5)))
          ICORRE(2)=NINT(VAL(ICOL(6)))
          ICORRE(5)=NINT(VAL(ICOL(7)))
        ELSE
          ICORRE(1)=NINT(VAL(ICOL(5))*1000D0)
          ICORRE(2)=NINT(VAL(ICOL(6))*100D0)
          ICORRE(5)=NINT(VAL(ICOL(7))*100D0)
        ENDIF
        IPOLCO(5)=NINT(VAL(ICOL(8)))
        IPOLCO(4)=NINT(VAL(ICOL(9)))
        IRMSCO(5)=NINT(VAL(ICOL(10)))
        IRMSCO(4)=NINT(VAL(ICOL(11)))
C
        DO I=1,5
          IF (I.NE.3) THEN
            POLCOO(I)=IPOLCO(I)/1000000.D0
            RMSCOO(I)=IRMSCO(I)/1000000.D0
            IF (I.LE.2) THEN
              POLRAT(I)=IPOLRA(I)/1000000.D0
              RMSRAT(I)=IRMSRA(I)/1000000.D0
            ENDIF
          ELSE
            POLCOO(I)=IPOLCO(I)/10000000.D0
            RMSCOO(I)=IRMSCO(I)/10000000.D0
            POLRAT(I)=-IPOLRA(I)/10000000.D0
            RMSRAT(I)=IRMSRA(I)/10000000.D0
          ENDIF
        ENDDO
        DO I=1,10
          POLCOR(I)=ICORRE(I)/100.D0
        ENDDO
      ELSE
        GOTO 910
      ENDIF
C
      GOTO 999
C
C ERROR READING POLE FILE
C -----------------------
900   WRITE(LFNERR,9001) IFORM, LFN, LINE
9001  FORMAT(/,' *** SR RDIEPI : ERROR READING POLE FILE'
     1       ,/,17X,'POLE FORMAT : ',I6,/,
     2          17X,'FILE UNIT   : ',I6,/,
     3          17X,'STRING      : ',A150,/)
      CALL EXITRC(2)
C
905   WRITE(LFNERR,9005) IFORM, LFN, LINE, IRCINP
9005  FORMAT(/,' *** SR RDIEPI : ERROR READING POLE FILE'
     1       ,/,17X,'POLE FORMAT : ',I6,/,
     2          17X,'FILE UNIT   : ',I6,/,
     3          17X,'STRING      : ',A150,/,
     4          17X,'IRCODE      : IRCINP=',I2,/)
      CALL EXITRC(2)
C
C ERROR: NOT IMPLEMENTED POLE FORMAT
C ----------------------------------
910   WRITE(LFNERR,9010) IFORM
9010  FORMAT(/,' *** SR RDIEPI : POLE FORMAT IFORM=',I2,' IS NOT ',
     1       'SUPPORTED',/)
      CALL EXITRC(2)
C
C END OF FILE REACHED
C -------------------
950   CONTINUE
      IRCODE=1
C
C END OF SUBROUTINE
C -----------------
999   RETURN
      END SUBROUTINE
C

      END MODULE
