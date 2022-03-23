      MODULE s_SMCLKI
      CONTAINS

C*
      SUBROUTINE SMCLKI(NSTAT,STANAM,NCLOCK,CLOCK)
CC
CC NAME       :  SMCLKI
CC
CC PURPOSE    :  READ SIMULATED CLOCK ERRORS FOR A GIVEN LIST OF
CC               STATIONS
CC
CC PARAMETERS :
CC         IN :  NSTAT  : NUMBER OF STATIONS IN CATALOG       I*4
CC               STANAM(I),I=1,2,..,NSTAT                     CH*16
CC        OUT :  NCLOCK(I),I=1,2,..,NSTAT: NUMBER OF          I*4
CC                        RECEIVER CLOCK PARAMETERS FOR
CC                        RECEIVER I
CC               CLOCK(K,I),K=1,2,..,NCLOCK(I),I=1,..,NSTAT:  R*8
CC                        CLOCK POLYNOMIAL COEFFICIENTS FOR
CC                        STATION I
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  88/02/23 18:31
CC
CC CHANGES    :  11-JAN-93 : ??: USE OF SR "OPNFIL" TO OPEN FILES
CC               10-AUG-94 : MR: CALL EXITRC
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
C
      USE s_opnfil
      USE s_opnerr
      USE s_maxtst
      USE s_exitrc
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , ICOE  , IOSTAT, IRC   , IST   , K     , MAXCLK,
     1          MXCCLK, NNN   , NSTAT
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MAXCLK=10)
C
      INTEGER*4       NCLOCK(*)
      REAL*8          CLOCK(MXCCLK,*),COEFF(MAXCLK)
      CHARACTER*6     MXNCLK
      CHARACTER*16    STANAM(*),STATIO
      CHARACTER*32    FILNAM
C
      COMMON/MCMCLK/MXCCLK,MXNCLK
C
C CHECK LOCAL DIMENSION
C ---------------------
      CALL MAXTST(1,'SMCLKI',MXNCLK,MAXCLK,MXCCLK,IRC)
      IF(IRC.NE.0) CALL EXITRC(2)
C
C INITIALIZE CLOCK MATRIX
C -----------------------
      DO 10 I=1,NSTAT
        NCLOCK(I)=999
        DO 10 K=1,MXCCLK
          CLOCK(I,K)=0.D0
10    CONTINUE
C
C GET RECEIVER CLOCK FILE NAME
C ----------------------------
      CALL GTFLNA(0,'RECCLK ',FILNAM,IRC)
C
C RECEIVER CLOCK FILE AVAILABLE: OPEN FILE
C ----------------------------------------
      IF (IRC.EQ.0) THEN
        CALL OPNFIL(LFNLOC,FILNAM,'OLD','FORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILNAM,'SMCLKI')
C
C READ ALL INPUT RECORDS
C ----------------------
        DO 100 I=1,1000
          IF(I.EQ.1)THEN
            READ(LFNLOC,20)STATIO,NNN,(COEFF(K),K=1,NNN)
20          FORMAT(///,A16,I1,1X,10F9.6)
          ELSE
            READ(LFNLOC,21,END=110)STATIO,NNN,(COEFF(K),K=1,NNN)
21          FORMAT(A16,I1,1X,10F9.6)
          END IF
          IF(STATIO.EQ.'   ')GO TO 110
C
C CHECK MAXIMAL NUMBER OF CLOCK PARAMETERS
          IF(NNN.GT.MXCCLK) THEN
            WRITE(LFNERR,902) NNN,MXCCLK
902         FORMAT(/,' *** SR SMCLKI: TOO MANY CLOCK PARAMETERS',/,
     1                           16X,'NUMBER OF CLOCK PARAMETERS:',I3,/,
     2                           16X,'MAX. NUMBER OF CLOCK PARA.:',I3,/)
            CALL EXITRC(2)
          ENDIF
C
C IDENTIFY STATION
C ----------------
          DO 30 IST=1,NSTAT
            IF(STANAM(IST).EQ.STATIO) GO TO 40
30        CONTINUE
          GO TO 100
C
C SAVE COEFFICIENT
C ----------------
40        NCLOCK(IST)=NNN
          DO 50 ICOE=1,NCLOCK(IST)
            CLOCK(ICOE,IST)=COEFF(ICOE)
50        CONTINUE
100     CONTINUE
110     CONTINUE
C
C CHECK, IF CLOCKS AVAILABLE FOR ALL STATIONS
C -------------------------------------------
        DO 60 IST=1,NSTAT
          IF(NCLOCK(IST).EQ.999) THEN
            WRITE(LFNERR,55) STANAM(IST)
55          FORMAT(/,' *** SR SMCLKI: NO CLOCK PARAMETERS FOUND IN ',
     1            'CLOCK FILE',/,16X,'STATION: ',A16,/)
            NCLOCK(IST)=0
          ENDIF
60      CONTINUE
C
C NO RECEIVER CLOCK FILE: CLOCKS = 0.D0
C -------------------------------------
      ELSE
        DO 70 IST=1,NSTAT
          NCLOCK(IST)=1
          CLOCK(1,IST)=0.D0
70      CONTINUE
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
