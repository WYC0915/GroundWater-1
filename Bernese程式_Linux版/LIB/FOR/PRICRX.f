      MODULE s_PRICRX
      CONTAINS

C*
      SUBROUTINE PRICRX(NWGT,SATWGT,TIMWGT,WGTWGT,SIGAPR,NCAMP,TAECMP)
CC
CC NAME       :  PRICRX
CC
CC PURPOSE    :  PRINT SATELLITE SPECIFIC SIGMAS AND SATELLITE
CC               PROBLEMS (SATELLITE PROBLEM FILE)
CC
CC PARAMETERS :
CC         IN :  NWGT   : NUMBER OF INTERVALS WITH WEIGHTED     I*4
CC                        SATELLITES
CC               SATWGT(I),I=1,..,NWGT: NUMBERS OF WEIGHTED     I*4
CC                        SATELLITES
CC               TIMWGT(K,I),K=1,2,I=1,..,NWGT: START AND END   R*8
CC                        OF TIME INTERVAL WITH WEIGHTED
CC                        SATELLITES IN MODIFIED JULIAN DATE
CC               WGTWGT(I),I=1,..,NWGT: SATELLITE SPECIFIC      R*8
CC                        SIGMA
CC               SIGAPR : APRIORI SIGMA OF A L1 ZERO-DIFF.      R*8
CC                        PHASE OBSERVATION
CC               NCAMP  : NUMBER OF CAMPAIGNS                   I*4
CC               TAECMP(2,I),I=1,..,NCAMP: START AND END TIME   R*8
CC                        OF CAMPAIGNS
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.4
CC
CC CREATED    :  28-JUL-92
CC
CC CHANGES    :  17-AUG-94 : MR: MAXBAD=200, MAXMAN=100
CC               23-AUG-96 : TS: MAXBAD IN INCLUDE FILE
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               05-MAR-03 : CU: REMOVE USE OF SKELETON FILE
CC               15-APR-03 : CU: BUG FIXED (FORMAT STATEMENTS)
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               26-FEB-08 : RD: USE GTSATM/GTSATB FROM D_SATCRX
CC               26-MAR-12 : RD: USE TIMSTR AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1992     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: LFNPRT
      USE m_maxdim, ONLY: MAXBAD
      USE d_satcrx, ONLY: gtsatm, gtsatb
      USE s_timstr
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IBAD  , ICAMP , IMAN  , IPRINT, IRC   , IWGT  , MAXMAN,
     1          NBAD  , NCAMP , NMAN  , NWGT
C
      REAL*8    SIGAPR, SIGSAT
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MAXMAN=100)
C
      CHARACTER*36  TSTRNG
      CHARACTER*14  TYPTXT(3)
      CHARACTER*12  ACTTXT(2)
      CHARACTER*32  FILNAM
C
      REAL*8        TIMWGT(2,*),WGTWGT(*),TIMMAN(MAXMAN)
      REAL*8        TIMBAD(2,MAXBAD),TAECMP(2,*)
C
      INTEGER*4     SATWGT(*),SATMAN(MAXMAN)
      INTEGER*4     SATBAD(MAXBAD),IOBBAD(MAXBAD),IACBAD(MAXBAD)
C
C
      DATA TYPTXT/'BAD PHASE     ','BAD CODE      ','BAD PHASE+CODE'/
      DATA ACTTXT/'OBS. MARKED ','OBS. REMOVED'/
C
C SATELLITE SPECIFIC SIGMAS
C -------------------------
      IF (NWGT.GT.0) THEN
C
C WRITE TITLE LINES
C -----------------
        WRITE(LFNPRT,"(
     1       ' '
     2    ,/,' '
     3    ,/,' SATELLITE SPECIFIC SIGMAS:'
     4    ,/,' -------------------------'
     5    ,/,' '
     6    ,/,' SAT  FROM               TO                SIGMA (M)'
     7    ,/,1X,131('-')
     8    ,/,1X)")
C
C SATELLITE SPECIFIC SIGMAS
C -------------------------
        DO 30 IWGT=1,NWGT
          CALL TIMSTR(2,TIMWGT(1,IWGT),TSTRNG)
          SIGSAT=SIGAPR/DSQRT(WGTWGT(IWGT))
          WRITE(LFNPRT,2) SATWGT(IWGT),TSTRNG,SIGSAT
2         FORMAT(I4,2X,A36,F12.4)
30      CONTINUE
      ENDIF
C
C SATELLITE MANOEUVRES AND PROBLEMS
C ---------------------------------
      CALL GTSATM(MAXMAN,NMAN,SATMAN,TIMMAN)
      CALL GTFLNA(0,'SATCRUX',FILNAM,IRC)
      CALL GTSATB(MAXBAD,FILNAM,NBAD,SATBAD,IOBBAD,IACBAD,TIMBAD)
C
      IPRINT=0
C
      DO 60 IMAN=1,NMAN
        DO 40 ICAMP=1,NCAMP
          IF (TIMMAN(IMAN).LT.TAECMP(2,ICAMP).AND.
     1        TIMMAN(IMAN).GE.TAECMP(1,ICAMP)) GOTO 45
40      CONTINUE
        GOTO 60
45      IF (IPRINT.EQ.0) THEN
          WRITE(LFNPRT,"(
     1         ' '
     2      ,/,' '
     3      ,/,' SATELLITE PROBLEMS:'
     4      ,/,' ------------------'
     5      ,/,' '
     6      ,/,' SAT  PROBLEM TYPE     ACTION         FROM     '
     6        ,'          TO'
     7      ,/,1X,131('-')
     8      ,/,1X)")
          IPRINT=1
        ENDIF
        CALL TIMSTR(1,TIMMAN(IMAN:IMAN),TSTRNG)
        WRITE(LFNPRT,3) SATMAN(IMAN),SATMAN(IMAN)+50,TSTRNG
3       FORMAT(I4,2X,'MANOEUVRE',8X,'NEW SAT.',I3, 4X,A)
60    CONTINUE
C
      DO 90 IBAD=1,NBAD
        DO 70 ICAMP=1,NCAMP
          IF (TIMBAD(1,IBAD).LT.TAECMP(2,ICAMP).AND.
     1        TIMBAD(2,IBAD).GE.TAECMP(1,ICAMP)) GOTO 75
70      CONTINUE
        GOTO 90
75      IF (IPRINT.EQ.0) THEN
          WRITE(LFNPRT,"(
     1         ' '
     2      ,/,' '
     3      ,/,' SATELLITE PROBLEMS:'
     4      ,/,' ------------------'
     5      ,/,' '
     6      ,/,' SAT  PROBLEM TYPE     ACTION         FROM     '
     6        ,'          TO'
     7      ,/,1X,131('-')
     8      ,/,1X)")
          IPRINT=1
        ENDIF
        CALL TIMSTR(2,TIMBAD(1,IBAD),TSTRNG)
        WRITE(LFNPRT,4) SATBAD(IBAD),TYPTXT(IOBBAD(IBAD)),
     1                  ACTTXT(IACBAD(IBAD)),TSTRNG
4       FORMAT(I4,2X,A14,3X,A12,3X,A)
90    CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
