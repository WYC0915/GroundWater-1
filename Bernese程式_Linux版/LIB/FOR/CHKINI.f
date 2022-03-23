      MODULE s_CHKINI
      CONTAINS

C*
      SUBROUTINE CHKINI(MEATYP,Q,NEXT,NOBS,SIGMA,MAXINT,ARG,OBSERV,
     1                  OBSFLG,NBAD,NSLIPS,INDLOC,NFIRST,NLAST,IRETRN)
CC
CC NAME       :  CHKINI
CC
CC PURPOSE    :  INITIALIZE SCREENING PROCESS FOR CODE AND PHASE
CC               OBSERVATIONS
CC
CC PARAMETERS :
CC         IN :  MEATYP : MEASUREMENT TYPE                    I*4
CC                    =1: PHASE
CC                    =2: CODE
CC               Q      : DEGREE OF SCREENING POLYNOMIAL      I*4
CC               NEXT   : INDEX OF OBSERVATION TO START       I*4
CC                        INITIALIZATION
CC               NOBS   : TOTAL NUMBER OF DATA POINTS         I*4
CC               SIGMA  : RMS OF OBSERVATION                  R*8
CC               MAXINT : MAX. INTERVAL LENGTH FOR
CC               ARG(I),I=1,2,..,NOBS: INDEPENDENT ARGUMENTS  R*8
CC               OBSERV(I),I=1,2,..,NOBS: OBSERVATIONS        R*8
CC     IN/OUT :  OBSFLG(I),I=1,2,..,NOBS: FLAGS               CH*1
CC                        BIT 0 = 1: NBAD OBSERVATION
CC                        BIT 0 = 0: GOOD OBSERVATION
CC                        BIT 1 = 1: CYCLE SLIP DETECTED
CC                        BIT 1 = 0: GOOD OBSERVATION
CC               NBAD   : NUMBER OF OBSERVATIONS NBAD         I*4
CC               NSLIPS : NUMBER OF OBSERVATIONS WITH CICLE   I*4
CC                        MARKED
CC        OUT :  INDLOC(I),I=1,2,..,Q+2: INDEX OF USED OBSER- I*4
CC                        VATIONS
CC               NFIRST : FIRST UNMARKED OBSERVATION FOUND    I*4
CC                        DURING INITIALIZATION
CC               NLAST  : LAST UNMARKED OBSERVATION FOUND     I*4
CC                        DURING INITIALIZATION
CC               IRETRN : RETURN CODE                         I*4
CC                        = 0 : INITIALIZATION SUCCESSFUL     I*4
CC                        = 1 : SCREENING PROCESS TERMINATED
CC                              DUE TO LACK OF OBSERVATIONS
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER, T.SCHILDKNECHT
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  89/06/28 16:42
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1989     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE s_setflg
      USE s_exitrc
      USE s_nxtobs
      USE s_chkpol
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IDIR  , INITOK, INXT  , IOBS  , IRC   , IRETRN, MAXINT,
     1          MAXQ  , MEATYP, NACT  , NBAD  , NEXT  , NFIRST, NLAST ,
     2          NOBS  , NSLIPS, NSTART
C
      REAL*8    RATIO , SIGMA , TIMTST
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      PARAMETER(MAXQ=10)
C
      INTEGER*4   Q,INDLOC(*)
      REAL*8      ARG(*),OBSERV(*),ARGLOC(MAXQ+2),OBSLOC(MAXQ+2)
      CHARACTER*1 OBSFLG(*),ANSWER
C
C
C CHECK MAXIMAL POLYNOMIAL DEGREE
C -------------------------------
      IF(Q.GT.MAXQ) THEN
        WRITE(LFNERR,901) Q,MAXQ
901     FORMAT(/,' *** SR CHKINI: POLYNOMIAL DEGREE TOO LARGE',/,
     1                       16X,'POLYNOMIAL DEGREE:',I3,/,
     2                       16X,'MAX. POLY. DEGREE:',I3,/)
        CALL EXITRC(2)
      ENDIF
C
C SAVE STARTING INDEX IN ARRAY "OBSERV"
      NSTART=NEXT
      NFIRST=NEXT
      NLAST =NEXT
C
C LOOK FOR FIRST Q+2 GOOD OBSERVATIONS (CONSECUTIVE)
C --------------------------------------------------
      IDIR=1
      NACT=NEXT
      DO 100 INXT=1,30000
C
C GET NEXT Q+2 ACTUAL OBSERVATIONS
C --------------------------------
        CALL NXTOBS(Q+2,IDIR,NACT,NOBS,ARG,OBSERV,OBSFLG,
     1              ARGLOC,OBSLOC,INDLOC,IRC)
C
C SET FIRST AND LAST UNMARKED OBSERVATION
        IF(INXT.EQ.1.AND.IRC.NE.-1) NFIRST=INDLOC(1)
        IF(IRC.EQ.0) NLAST=INDLOC(Q+2)
        IF(IRC.GT.0) NLAST=INDLOC(IRC)
C
C STOP INITIALIZATION, IF IRC > 0
        IF(IRC.NE.0) GOTO 900
C
C INTERVAL SPANNED BY THE SELECTED Q+2 OBSERVATIONS
        TIMTST=DABS(ARGLOC(Q+2)-ARGLOC(1))
C
C CHECK SELECTED Q+2 OBSERVATIONS FOR CONSISTENCY
C -----------------------------------------------
        CALL CHKPOL(Q,ARGLOC,OBSLOC,SIGMA,ANSWER,RATIO)
C
C TEST MAXIMUM INTERVAL ALLOWED AND RETURN CODE OF "CHKPOL"
C ---------------------------------------------------------
        IF(TIMTST.LE.MAXINT.AND.ANSWER.EQ.'Y') GOTO 110
C
C OBSERVATION GROUP NOT OK: MARK FIRST OBSERVATION AS BAD FOR PHASE
        IF(MEATYP.EQ.1) THEN
          NBAD=NBAD+1
          CALL SETFLG(OBSFLG(INDLOC(1)),0)
        END IF
C
C TRY NEXT GROUP OF OBS. STARTING AT "INDLOC(2)"
        NACT=INDLOC(2)
100   CONTINUE
C
C INITIALIZATION SUCCESSFUL:
C -------------------------
110   IF(MEATYP.EQ.1) THEN
C
C FIRST OBS. = SLIP FOR PHASE
        NSLIPS=NSLIPS+1
        CALL SETFLG(OBSFLG(INDLOC(1)),1)
      ELSE
C
C CODE: CHECK OBS. BACKWARD IN TIME BACK TO THE START INDEX OF INIT.
C ------------------------------------------------------------------
        INITOK=INDLOC(1)
        IDIR=-1
        NACT=INDLOC(Q+1)
        DO 200 INXT=1,30000
C
C GET NEXT Q+2 ACTUAL OBSERVATIONS (BACKWARD)
C -------------------------------------------
          CALL NXTOBS(Q+2,IDIR,NACT,NSTART,ARG,OBSERV,OBSFLG,
     1                ARGLOC,OBSLOC,INDLOC,IRC)
          IF(IRC.NE.0) GOTO 210
C
C INTERVAL SPANNED BY THE SELECTED Q+2 OBSERVATIONS
          TIMTST=DABS(ARGLOC(Q+2)-ARGLOC(1))
C
C CHECK SELECTED Q+2 OBSERVATIONS FOR CONSISTENCY
C -----------------------------------------------
          CALL CHKPOL(Q,ARGLOC,OBSLOC,SIGMA,ANSWER,RATIO)
C
C TEST MAXIMUM INTERVAL ALLOWED AND RETURN CODE OF "CHKPOL"
C ---------------------------------------------------------
          IF(TIMTST.LE.MAXINT.AND.ANSWER.EQ.'Y') THEN
            NACT=INDLOC(2)
          ELSE
            NBAD=NBAD+1
            CALL SETFLG(OBSFLG(INDLOC(Q+2)),0)
            NACT=INDLOC(1)
          ENDIF
200     CONTINUE
C
C STARTING INDEX OF INITIALIZATION REACHED: RECOVER "INDLOC" OF
C SUCCESSFUL INITIALIZATION
C -------------------------------------------------------------
210     IDIR=1
        CALL NXTOBS(Q+2,IDIR,INITOK,NOBS,ARG,OBSERV,OBSFLG,
     1                ARGLOC,OBSLOC,INDLOC,IRC)
      ENDIF
C
C SET RETURN CODE: INITIALIZATION SUCCESSFUL
      IRETRN=0
      GOTO 999
C
C END OF FILE REACHED: MARK REMAINING OBS. FOR PHASE, MARK ALL OBS.
C SINCE START OF INITIALIZATION FOR CODE
C -----------------------------------------------------------------
900   IF(MEATYP.EQ.2) THEN
        IDIR=1
        NACT=NSTART
        DO 920 INXT=1,30000
          CALL NXTOBS(Q+2,IDIR,NACT,NOBS,ARG,OBSERV,OBSFLG,
     1                ARGLOC,OBSLOC,INDLOC,IRC)
          IF(IRC.NE.0) GOTO 930
          DO 910 IOBS=1,Q+2
            NBAD=NBAD+1
            CALL SETFLG(OBSFLG(INDLOC(IOBS)),0)
910       CONTINUE
          NACT=INDLOC(Q+2)
920     CONTINUE
      ENDIF
C
930   DO 940 IOBS=1,IRC
        NBAD=NBAD+1
        CALL SETFLG(OBSFLG(INDLOC(IOBS)),0)
940   CONTINUE
C
C SET RETURN CODE: INITIALIZATION NOT SUCCESSFUL
      IRETRN=1
C
C RETURN
999   RETURN
      END SUBROUTINE

      END MODULE
