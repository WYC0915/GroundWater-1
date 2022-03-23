      MODULE s_PRIPOL
      CONTAINS

C*
      SUBROUTINE PRIPOL(TITLES,NCAMP ,TAECMP,POLPAR,NPOL  ,
     1                  TPOL  ,SIGPOL,ISGPOL,ISGNUT)
CC
CC NAME       :  PRIPOL
CC
CC PURPOSE    :  PRINT POLE A PRIORI INFORMATION FOR EACH CAMPAIGN
CC               AND ERP PARAMETER ESTIMATION
CC
CC PARAMETERS :
CC         IN :  TITLES(I),I=1,2: TITLE LINES                 CH*132
CC               NCAMP  : NUMBER OF CAMPAIGNS                 I*4
CC               TAECMP(2,I),I=1,..,NCAMP: OBSERVATION INTER- R*8
CC                        VAL FOR CAMPAIGN I
CC               POLPAR : PARAMETERS TO BE ESTIMATED          I*4(5)
CC                        (1)=XP, (2)=YP, (3)=DT, (4)=EPS, (5)=PSI
CC                        1 := ESTIMATED 0 := NOT ESTIMATED
CC               NPOL   : NUMBER OF POLE PARAMETER SETS       I*4
CC               TPOL   : START AND END TIME OF INTERVAL FOR  R*8(2,*)
CC                        ONE SET OF PARAMETERS
CC                        1,2 :=BEGIN,END TIME, *:= 1..MAXPOL
CC               SIGPOL : A PRIORI SIGMA OF POLE PARAMETERS   R*8(5,*)
CC                        1-5 := XP,YP,DT,DE,DP *:= 1..MAXPOL
CC                        1,2,4,5 GIVEN IN MAS, 3 IN MSEC
CC               ISGPOL : EARTH ROTATION PARAMETER SIGMAS :   I*4(*)
CC                        =0 : APPLY FOR RELEVANT PARAMETER
CC                             ONLY THE ABSOLUTE CONSTRAINTS
CC                             GIVEN IN INPUT OPTION FILE
CC                        =1 : ENSURE CONTINUITY WITH RESPECT
CC                             TO PREVIOUS POLYNOMIAL (IN ADD.
CC                             TO ABSOLUTE CONSTRAINTS)
CC                        =4 : CONSTRAIN DRIFTS TO ZERO
CC                        =5 : ENSURE CONTINUITY WITH RESPECT
CC                             TO PREVIOUS POLYNOMIAL AND
CC                             CONSTRAIN DRIFTS TO ZERO
CC               ISGNUT : EARTH ORIENTATION PARAMETER SIGMAS :   I*4(*)
CC                        =0 : APPLY FOR RELEVANT PARAMETER
CC                             ONLY THE ABSOLUTE CONSTRAINTS
CC                             GIVEN IN INPUT OPTION FILE
CC                        =1 : ENSURE CONTINUITY WITH RESPECT
CC                             TO PREVIOUS POLYNOMIAL (IN ADD.
CC                             TO ABSOLUTE CONSTRAINTS)
CC                        =4 : CONSTRAIN DRIFTS TO ZERO
CC                        =5 : ENSURE CONTINUITY WITH RESPECT
CC                             TO PREVIOUS POLYNOMIAL AND
CC                             CONSTRAIN DRIFTS TO ZERO
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S.FANKHAUSER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/11 14:36
CC
CC CHANGES    :  27-MAY-91 : ??: DON'T PRINT TRAILING BLANKS
CC               31-MAY-92 : ??: PRINT HEADER LINES ONLY IF PARAMETERS
CC                               ARE ESTIMATED
CC               04-JUL-92 : ??: PRINT APRIORI SIGMA IF(POLPAR.GE.1),
CC                               CHANGE DUE TO POLYNOM ERP EST. MODEL.
CC                               PRINT NUMBER OF PARAMS PER REQUEST
CC               02-AUG-92 : ??: ADD CHECK FOR "IEND" FROM SR RDPOLI
CC               22-MAR-93 : ??: ADD CONTINUITY INFO FOR ERP ESTIMATION
CC               19-APR-94 : RW: CPO-MODEL INCLUDED
CC               25-APR-94 : MR: CONSTRAINTS FOR DRIFTS
CC               13-OCT-94 : MR : REPLACE "X" BY "1X" IN FORMAT
CC               10-SEP-95 : LM: REORDER DATA STATEMENT
CC               04-JUN-96 : TS: ADDED SUBDAILY POLE MODEL
CC               05-DEC-02 : PS: CALL RDPOLH WITH NUTNAM AND SUBNAM
CC                               WRITE SUBNAM TO ERP FILE
CC               08-JAN-03 : PS: WRITE NUTNAM TO ERP FILE
CC               04-FEB-03 : PS: CALL TO SR GETPOL CHANGED
CC               05-MAR-03 : CU: REMOVE USE OF SKELETON FILE
CC               15-APR-03 : CU: BUG FIXED (FORMAT STATEMENTS)
CC               05-NOV-03 : HU: ADDITIONAL ARGUMENTS FOR GETPOL
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               26-MAR-12 : RD: USE TIMSTR AS MODULE NOW
CC               26-MAR-12 : RD: REMOVE UNUSED VARIABLES
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,  ONLY: LFNPRT, LFNERR, LFNLOC
      USE s_opnfil
      USE s_opnerr
      USE s_getpol
      USE s_rdpolh
      USE s_timstr
      USE s_rdpoli
      USE f_lengt1
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I1    , I2    , IEND  , IFORM , IOSTAT, IRC   ,
     1          NCAMP , NPOL
C
      REAL*8    T1    , T2
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C GLOBAL DECLARATIONS
C -------------------
      CHARACTER*132 TITLES(2)
C
      REAL*8        TAECMP(2,*),TPOL(2,*),SIGPOL(5,*)
C
      INTEGER*4     POLPAR(*),ISGPOL(*),ISGNUT(*)

C LOCAL DECLARATIONS
C ------------------
      CHARACTER*132 TEXT
      CHARACTER*80  DUMMYC
      CHARACTER*36  TSBIG
      CHARACTER*32  FILPOL
      CHARACTER*17  TSTRNG
C      CHARACTER*10  NUTCH(3),POLCH(2)
      CHARACTER*13  REM,CNTTXT(6)
      CHARACTER*16  NUTNAM
      CHARACTER*16  SUBNAM
C
      REAL*8        T,GPSUTC,POLCOO(5),RMSPOL(5)
      REAL*8        RD1(2),RD2(2),RD3(2),RD4(2),RD5(2),RD6(2),THELP(2)
C
      INTEGER*4     POLTYP(2)
C
C
      DATA CNTTXT/'NO           ','YES          ','             ',
     1            '             ','NO , DRIFTS=0','YES, DRIFTS=0'/
C      DATA NUTCH/'NO MODEL  ','OBSERVED  ','HERRING   '/
C      DATA POLCH/'NO MODEL  ','RAY       '/

C
C PRINT TITLE LINES
C -----------------
      WRITE(LFNPRT,2) TITLES(1)(1:LENGT1(TITLES(1))),
     1                TITLES(2)(1:LENGT1(TITLES(2)))
2     FORMAT(//,A,/,A,/,' ',131('-'),//)
C
      WRITE(LFNPRT,"(
     1     ' 8. POLE COORDINATES AND TIME INFORMATION'
     2  ,/,' ----------------------------------------'
     3  ,/,' '
     4  ,/,' A PRIORI POLE AND TIME INFORMATION FROM THE POLE FILE:'
     5  ,/,' -----------------------------------------------------'
     6  ,/,' '
     7  ,/,' DATUM       TIME       X-POLE ("")  Y-POLE ("")  UT1-UTC '
     7    ,'(S) GPS-UTC (S)  RMS XP ("")  RMS YP ("")  RMS DT (S)'
     8  ,/,'                        EP-CPO ("")  PS-CPO ("")          '
     8    ,'                 RMS EP ("")  RMS PS ("")'
     9  ,/,1X,131('-')
     .  ,/,1X)")
C
C PRINT A PRIORI INFORMATION FROM POLE FILE
C -----------------------------------------
      DO 20 I=1,NCAMP
C
C GET LARGEST TIME SMALLER THAN TAECMP(1) AND
C SMALLEST TIME GREATER THAN TAECMP(2)
        CALL GETPOL(TAECMP(1,I),1,THELP,RD1,RD2,RD3,RD4,RD5,RD6,POLTYP)
        IF (THELP(2).EQ.TAECMP(1,I)) THEN
          T1=THELP(2)
        ELSE
          T1=THELP(1)
        END IF
        CALL GETPOL(TAECMP(2,I),1,THELP,RD1,RD2,RD3,RD4,RD5,RD6,POLTYP)
        IF (THELP(1).EQ.TAECMP(2,I)) THEN
          T2=THELP(1)
        ELSE
          T2=THELP(2)
        END IF
C
C OPEN POLE FILE
        CALL GTFLNA(1,'POLE   ',FILPOL,IRC)
        CALL OPNFIL(LFNLOC,FILPOL,'OLD',' ','READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILPOL,'PRIPOL')
C
C READ HEADER OF POLE FILE
        CALL RDPOLH(LFNLOC,1,DUMMYC,POLTYP,IFORM,IEND,NUTNAM,SUBNAM)

C
C FIND,READ AND PRINT THE INFORMATION
        DO 30 I2=1,10000
          CALL RDPOLI(LFNLOC,T,POLCOO,GPSUTC,REM,RMSPOL,IFORM,IEND)
          IF (IEND.GT.0) GOTO 40
          IF (T.GE.T1.AND.T.LE.T2) THEN
            CALL TIMSTR(1,(/T/),TSTRNG)
            WRITE(LFNPRT,3) TSTRNG,(POLCOO(I1),I1=1,3),GPSUTC,
     1                      (RMSPOL(I1),I1=1,3),
     2                      (POLCOO(I1),I1=4,5),(RMSPOL(I1),I1=4,5)
3           FORMAT(' ',A,2F12.5,F13.6,F10.0,F15.5,F12.5,F13.6/
     1             18X,2F12.5,26X,2F12.5)
          END IF
          IF (T.GT.T2) GOTO 40
30      CONTINUE
40      CONTINUE
        WRITE(LFNPRT,101) NUTNAM
101     FORMAT(/,1X,'NUTATION MODEL:           ',A16)
        WRITE(LFNPRT,102) SUBNAM
102     FORMAT(1X,'SUBDAILY POLE MODEL:      ',A16,/)
        CLOSE(LFNLOC)
20    CONTINUE
C
C PRINT A PRIORI INFORMATION STEMMING FROM INPUT OPTION FILE
C ----------------------------------------------------------
      IF (NPOL.NE.0) THEN
C
        WRITE(LFNPRT,"(
     1       ' '
     2    ,/,' A PRIORI EARTH ROTATION PARAMETER INFORMATION:'
     3    ,/,' ---------------------------------------------'
     4    ,/,' PARAM SET ',77X,'A PRIORI SIGMAS              CONTINUITY'
     5    ,/,'    NR     FROM               TO                 # PAR X '
     5      ,' # PAR Y  # PAR DT    XP (MAS)   YP (MAS)     DT (MSEC)  '
     5      ,'   BETWEEN SETS'
     6    ,/,49X,'# PAR EP # PAR PS             EP (MAS)   PS (MAS)'
     7    ,/,1X,131('-')
     8    ,/,1X)")
C
        DO 70 I2=1,NPOL
          TEXT=' '
          CALL TIMSTR(2,TPOL(1,I2),TSBIG)
          WRITE(TEXT(2:6),4) I2
4         FORMAT(I5)
          WRITE(TEXT(12:47),1) TSBIG
1         FORMAT(A)
          IF (POLPAR(1).GE.1) THEN
            WRITE(TEXT(52:53),8) POLPAR(1)
            WRITE(TEXT(76:86),5) SIGPOL(1,I2)
5           FORMAT(F11.5)
          END IF
          IF (POLPAR(2).GE.1) THEN
            WRITE(TEXT(61:62),8) POLPAR(2)
            WRITE(TEXT(88:98),6) SIGPOL(2,I2)
6           FORMAT(F11.5)
          END IF
          IF (POLPAR(3).GE.1) THEN
            WRITE(TEXT(70:71),8) POLPAR(3)
            WRITE(TEXT(100:112),7) SIGPOL(3,I2)
7           FORMAT(F13.7)
8           FORMAT(I2)
          END IF
          WRITE(TEXT(118:130),9) CNTTXT(ISGPOL(I2)+1)
9         FORMAT(A13)
          WRITE(LFNPRT,1) TEXT(1:LENGT1(TEXT))
C  CELESTIAL-OFFSETS
          TEXT=' '
          CALL TIMSTR(2,TPOL(1,I2),TSBIG)
C          TEXT(2:6)='     '
          WRITE(TEXT(12:47),1) TSBIG
          IF (POLPAR(4).GE.1) THEN
            WRITE(TEXT(52:53),8) POLPAR(4)
            WRITE(TEXT(76:86),5) SIGPOL(4,I2)
          END IF
          IF (POLPAR(5).GE.1) THEN
            WRITE(TEXT(61:62),8) POLPAR(5)
            WRITE(TEXT(88:98),6) SIGPOL(5,I2)
          END IF
          WRITE(TEXT(118:130),9) CNTTXT(ISGNUT(I2)+1)
          WRITE(LFNPRT,1) TEXT(1:LENGT1(TEXT))
70      CONTINUE
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
