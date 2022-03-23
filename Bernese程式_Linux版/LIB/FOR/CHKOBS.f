      MODULE s_CHKOBS
      CONTAINS

C*
      SUBROUTINE CHKOBS(MEATYP,PRTLEV,Q,MAXINT,NOBS,TIMREF,IDELTT,
     1                  SIGMA,ARG,OBSERV,OBSFLG,NBAD,NSLIPS)
CC
CC NAME       :  CHKOBS
CC
CC PURPOSE    :  CHECK CODE- OR PHASE-OBSERVATIONS (ZERO/SINGLE/DOUBLE)
CC               IF CODE DATA ARE CHECKED: OBSERVATIONS ARE SCREENED
CC                                         FOR OUTLIERS
CC               IF PHASE DATA ARE CHECKED:OBSERVATIONS ARE SCREENED
CC                                         FOR CYCLE SLIPS
CC                                         (DISCONTINUITIES)
CC
CC PARAMETERS :
CC         IN :  MEATYP : MEASUREMENT TYPE                    I*4
CC                        =1: PHASE DATA
CC                        =2: CODE DATA
CC               PRTLEV : PRINT LEVEL                         I*4
CC                        =0 : NO MESSAGES
CC                        =1 : PRINT MESSAGES
CC               Q      : DEGREE OF SCREENING POLYNOMIAL      I*4
CC               MAXINT : MAXIMUM INTERVAL LENGTH FOR         I*4
CC                        POLYNOMIAL FIT (MINUTES)
CC               NOBS   : NUMBER OF OBSERVATIONS              I*4
CC               TIMREF : REFERENCE EPOCH (FIRST OBS. TIME)   R*8
CC               IDELTT : OBSERVATION INTERVAL (SEC)          I*4
CC               SIGMA  : RMS OF SINGLE OBSERVATION IN ARRAY  R*8
CC                        OBSERV
CC               ARG(I),I=1,2,..,NOBS: OBS ARGS               R*8
CC               OBSERV(I),I=1,2,..,NOBS: OBSERVATIONS        R*8
CC     IN/OUT :  OBSFLG(I),I=1,2,..,NOBS: OBS. FLAGS          CH*1
CC        OUT :  NBAD   : NUMBER OF BAD OBSERVATIONS          I*4
CC               NSLIPS : NUMBER OF SLIPS FOUND               I*4
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER, T.SCHILDKNECHT
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  88/02/27 14:51
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC               30-AUG-95 : MR: CORRECT ERROR MESSAGE
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE s_chkini
      USE s_setflg
      USE s_exitrc
      USE s_nxtobs
      USE s_chkpol
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IDELTT, IDIR  , IEP   , IEPE  , IEPLST, IEPS  , INXT  ,
     1          IRC   , MAXINT, MAXQ  , MEATYP, NBAD  , NBDNEW, NBDOLD,
     2          NEXT  , NFIRS1, NFIRST, NLAST , NOBINT, NOBS  , NSLIPS
C
      REAL*8    RATIO , SIGMA , TIMREF, TIMTST
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      PARAMETER(MAXQ=10)
C
      INTEGER*4 Q,INDLOC(MAXQ+2),PRTLEV
C
      REAL*8 ARG(*),OBSERV(*)
      REAL*8 ARGLOC(MAXQ+2),OBSLOC(MAXQ+2)
C
      CHARACTER*1 OBSFLG(*),ANSWER
C
C
C CHECK MAXIMUM POLYNOMIAL DEGREE
      IF(Q.GT.MAXQ)THEN
        WRITE(LFNERR,10)Q,MAXQ
10      FORMAT(/,' *** SR CHKOBS: POLYNOMIAL DEGREE TOO LARGE',/,
     1                       16X,'POLYNOMIAL DEGREE:',I3,/,
     2                       16X,'MAX. POLY. DEGREE:',I3,/)
        CALL EXITRC(2)
      END IF
C
C CHECK IF AT LEAST Q+2 OBSERVATIONS IN INTERVAL "MAXINT"
C -------------------------------------------------------
      NOBINT=IDINT(MAXINT*60.D0/IDELTT)+1
      IF(NOBINT.LT.Q+2) THEN
        WRITE(LFNERR,8) MAXINT,IDELTT,Q,Q+2
8       FORMAT(/,' *** SR CHKOBS: NEVER ENOUGH OBSERVATIONS WITHIN THE',
     1                           ' MAXIMUM INTERVAL LENGTH',/,
     2                       16X,'OF POLYNOMIAL FIT',/,
     3                       16X,'MAXIMUM INTERVAL (MIN)    :',I5,/,
     4                       16X,'OBSERVATION INTERVAL (SEC):',I5,/,
     5                       16X,'POLYNOMIAL DEGREE         :',I5,/,
     6                       16X,'# OBSERVAT. NEEDED FOR FIT:',I5,/)
        CALL EXITRC(2)
      ENDIF
C
C INITIALIZATION
C --------------
      NBAD  =0
      NSLIPS=0
      NEXT=1
      CALL CHKINI(MEATYP,Q,NEXT,NOBS,SIGMA,MAXINT,ARG,OBSERV,
     1            OBSFLG,NBAD,NSLIPS,INDLOC,NFIRST,NLAST,IRC)
C
C PRINT INITIALIZATION RESULT
C ---------------------------
      IF(PRTLEV.GT.0) THEN
        IF(MEATYP.EQ.1) THEN
          WRITE(LFNPRT,9)
        ELSE
          WRITE(LFNPRT,11)
        END IF
        IF(NBAD.GT.0) THEN
          IEPS=IDNINT((ARG(NFIRST)-TIMREF)*86400.D0/IDELTT)+1
          IF(IRC.EQ.0) THEN
            IEPE=IDNINT((ARG(INDLOC(1)-1)-TIMREF)*86400.D0/IDELTT)+1
          ELSE
            IEPE=IDNINT((ARG(NLAST)-TIMREF)*86400.D0/IDELTT)+1
          ENDIF
          WRITE(LFNPRT,17) IEPS,IEPE,NBAD
        ENDIF
        IF(IRC.EQ.0) THEN
          IEP=IDNINT((ARG(INDLOC(1))-TIMREF)*86400.D0/IDELTT)+1
          IF(MEATYP.EQ.1) THEN
            WRITE(LFNPRT,16) IEP,1
          ELSE
            WRITE(LFNPRT,19) IEP
          ENDIF
        ELSE
          IEP=IDNINT((ARG(NLAST)-TIMREF)*86400.D0/IDELTT)+1
          WRITE(LFNPRT,15) IEP
        ENDIF
      ENDIF
C
C RETURN IF NO VALID OBSERVATIONS ARE FOUND
      IF(IRC.NE.0) GOTO 900
C
C SCREEN ALL OBSERVATIONS
C -----------------------
      IDIR=1
      NEXT=INDLOC(2)
      DO 100 INXT=1,30000
C
C GET NEXT Q+2 OBSERVATIONS STARTING AT INDEX "NEXT"
        CALL NXTOBS(Q+2,IDIR,NEXT,NOBS,ARG,OBSERV,OBSFLG,
     1              ARGLOC,OBSLOC,INDLOC,IRC)
        IF(IRC.EQ.0) NLAST=INDLOC(Q+2)
        IF(IRC.GT.0) NLAST=INDLOC(IRC)
        IF(IRC.NE.0) GOTO 900
C
C TIME INTERVAL SPANNED BY THE Q+2 OBSERVATIONS
        TIMTST=DABS(ARGLOC(Q+2)-ARGLOC(1))
C
C CHECK VALIDITY OF ASSUMPTION THAT FUNCTION IS A POLYNOMIAL
C OF DEGREE Q:
        CALL CHKPOL(Q,ARGLOC,OBSLOC,SIGMA,ANSWER,RATIO)
C
C EPOCH NUMBER OF THE LATEST OBSERVATION
        IEPLST=IDNINT((ARG(INDLOC(Q+2))-TIMREF)*86400.D0/IDELTT)+1
C
C PHASE:
C -----
        IF(MEATYP.EQ.1) THEN
C
C CHECK MAXIMUM INTERVAL LENGTH AND RETURN CODE OF "CHKPOL"
          IF(TIMTST.GT.MAXINT.OR.ANSWER.NE.'Y') THEN
C
C PRINT MESSAGES
            IF(PRTLEV.GT.0) THEN
              IF(TIMTST.GT.MAXINT) THEN
                WRITE(LFNPRT,14) IEPLST,MAXINT,IDNINT(TIMTST)
              ELSE
                WRITE(LFNPRT,13) IEPLST,RATIO
              ENDIF
            ENDIF
C
C SAVE NUMBER OF MARKED OBS. BEFORE NEW INITIALIZATION
            NBDOLD=NBAD
C
C NEW INITIALIZATION
            NEXT=INDLOC(Q+2)
            CALL CHKINI(MEATYP,Q,NEXT,NOBS,SIGMA,MAXINT,ARG,OBSERV,
     1                  OBSFLG,NBAD,NSLIPS,INDLOC,NFIRS1,NLAST,IRC)
C
C PRINT INITIALIZATION RESULT
            IF(PRTLEV.GT.0) THEN
              NBDNEW=NBAD-NBDOLD
              IF(NBDNEW.GT.0) THEN
                IEPS=IDNINT((ARG(NEXT)-TIMREF)*86400.D0/IDELTT)+1
                IF(IRC.EQ.0) THEN
                  IEPE=IDNINT((ARG(INDLOC(1)-1)-TIMREF)*86400.D0
     1                        /IDELTT)+1
                ELSE
                  IEPE=IDNINT((ARG(NLAST)-TIMREF)*86400.D0/IDELTT)+1
                ENDIF
                WRITE(LFNPRT,17) IEPS,IEPE,NBDNEW
              ENDIF
              IF(IRC.EQ.0) THEN
                IEP=IDNINT((ARG(INDLOC(1))-TIMREF)*86400.D0/IDELTT)+1
                WRITE(LFNPRT,16) IEP,1
              ELSE
                IEP=IDNINT((ARG(NLAST)-TIMREF)*86400.D0/IDELTT)+1
                WRITE(LFNPRT,15) IEP
              ENDIF
            ENDIF
C
C RETURN IF NO MORE OBSERVATIONS ARE FOUND
            IF(IRC.NE.0) GOTO 900
          ENDIF
C
C NEXT TRIAL
          NEXT=INDLOC(2)
C
C CODE:
C ----
        ELSE
C
C CHECK MAXIMUM INTERVAL LENGTH AND RETURN CODE OF "CHKPOL"
          IF(TIMTST.GT.MAXINT.OR.ANSWER.NE.'Y') THEN
C
C PRINT MESSAGES
            IF(PRTLEV.GT.0) THEN
              IF(TIMTST.GT.MAXINT) THEN
                WRITE(LFNPRT,14) IEPLST,MAXINT,IDNINT(TIMTST)
              ELSE
                WRITE(LFNPRT,12) IEPLST,1,RATIO
              ENDIF
            ENDIF
C
C NEW INITIALIZATION:
C FOR EXCEEDED INTERVAL START AT INDLOC(Q+2)
C FOR OUTLIER MARK LAST OBS. AND START AT INDLOC(2)
            IF(TIMTST.GT.MAXINT) THEN
              NEXT=INDLOC(Q+2)
            ELSE
              NBAD=NBAD+1
              CALL SETFLG(OBSFLG(INDLOC(Q+2)),0)
              NEXT=INDLOC(2)
            ENDIF
C
C SAVE NUMBER OF MARKED OBS. BEFORE NEW INITIALIZATION
            NBDOLD=NBAD
            CALL CHKINI(MEATYP,Q,NEXT,NOBS,SIGMA,MAXINT,ARG,OBSERV,
     1                  OBSFLG,NBAD,NSLIPS,INDLOC,NFIRS1,NLAST,IRC)
C
C PRINT INITIALIZATION RESULT
            IF(PRTLEV.GT.0) THEN
              NBDNEW=NBAD-NBDOLD
              IF(NBDNEW.GT.0) THEN
                IEPS=IDNINT((ARG(NEXT)-TIMREF)*86400.D0/IDELTT)+1
                IF(IRC.EQ.0) THEN
                  IEPE=IDNINT((ARG(INDLOC(1)-1)-TIMREF)*86400.D0
     1                        /IDELTT)+1
                ELSE
                  IEPE=IDNINT((ARG(NLAST)-TIMREF)*86400.D0/IDELTT)+1
                ENDIF
                WRITE(LFNPRT,17) IEPS,IEPE,NBDNEW
              ENDIF
              IF(IRC.EQ.0) THEN
                IEP=IDNINT((ARG(INDLOC(1))-TIMREF)*86400.D0/IDELTT)+1
                WRITE(LFNPRT,19) IEP
              ELSE
                IEP=IDNINT((ARG(NLAST)-TIMREF)*86400.D0/IDELTT)+1
                WRITE(LFNPRT,15) IEP
              ENDIF
            ENDIF
C
C RETURN IF NO MORE OBSERVATIONS ARE FOUND
            IF(IRC.NE.0) GOTO 900
          ENDIF
C
C NEXT TRIAL
          NEXT=INDLOC(2)
C
        ENDIF
100   CONTINUE
C
C PRINT SUMMARY
900   IF(PRTLEV.GT.0) THEN
        IEPS=IDNINT((ARG(NFIRST)-TIMREF)*86400.D0/IDELTT)+1
        IEPE=IDNINT((ARG(NLAST)-TIMREF)*86400.D0/IDELTT)+1
        IF(MEATYP.EQ.1) THEN
          WRITE(LFNPRT,901) IEPS,IEPE,NBAD,NSLIPS
        ELSE
          WRITE(LFNPRT,903) IEPS,IEPE,NBAD
        ENDIF
      ENDIF
      GOTO 999
C
C RETURN
999   RETURN
C
C FORMAT
9     FORMAT(/' EPOCHS         MARK SLIP  COMMENT'/1X,72('-')/)
11    FORMAT(/' EPOCHS         MARK       COMMENT'/1X,72('-')/)
12    FORMAT(I6,8X,I5,8X,'OUTLIER (',1PG8.2,' * 3 SIGMA): NEW INIT.')
13    FORMAT(I6,21X,'PROBLEM (',1PG8.2,' * 3 SIGMA): NEW INIT.    ')
14    FORMAT(I6,21X,'MAX. INTERVAL (',I3,' MIN) EXCEEDED: NEW INIT.',
     1         /27X,'(INTERVAL SPANNED:',I5,' MIN)')
15    FORMAT(I6,21X,'EOF REACHED DURING INITIALIZATION')
16    FORMAT(I6,13X,I5,3X,'INITIALIZATION SUCCESSFUL')
19    FORMAT(I6,21X,'INITIALIZATION SUCCESSFUL')
17    FORMAT(I6,' -',I6,I5,8X,
     1       'OBS. MARKED DURING INITIALIZATION')
901   FORMAT(/1X,72('-')/
     1        I6,' -',I6,2I5,3X,'SUMMARY'/1X,72('-'))
903   FORMAT(/1X,72('-')/
     1        I6,' -',I6,I5,8X,'SUMMARY'/1X,72('-'))
C
      END SUBROUTINE

      END MODULE
