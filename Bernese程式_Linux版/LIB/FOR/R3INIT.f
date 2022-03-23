      MODULE s_R3INIT
      CONTAINS

C*
      SUBROUTINE R3INIT(FILCOD,FILPHA,SCROBS,ICSFLG,TIMREF,TLAST ,
     1                  NEPFLG,IOK1  ,NSMEA ,ANZCOD,ANZPHA,NUMMRK,
     2                  PHA001,IOBPRT,ISAPRT,IAMPRT)
CC
CC NAME       :  R3INIT
CC
CC PURPOSE    :  INITIALIZE PROCESSING OF ONE OUTPUT FILE IN
CC               TRANSFER PROGRAMS
CC
CC PARAMETERS :
CC         IN :  FILCOD:  CODE FILE NAMES                     CH*32
CC                          FILCOD(1): HEADER FILE NAME
CC               FILPHA:  PHASE FILE NAMES                    CH*32
CC                          FILPHA(1): HEADER FILE NAME
CC               SCROBS : SCRATCH FILES FOR THE OBSERVATIONS  CH*32(2)
CC               ICSFLG : ACCEPT RINEX CYCLE SLIP FLAGS        I*4
CC                        IF ICSFLG.EQ.1 PHA001 WILL BE 0.D0
CC         OUT : TIMREF(2): FIRST OBSERVATION EPOCH FOR CODE  R*8
CC                            AND PHASE OBSERVATIONS
CC               TLAST(2):  LAST OBSERVATION EPOCH FOR CODE   R*8
CC                            AND PHASE OBSERVATIONS
CC               NEPFLG(2): NUMBER OF FLAGGED EPOCHS FOR CODE I*4
CC                            AND PHASE OBSERVATIONS
CC               IOK1(2): NUMBER OF GOOD OBSERVATION EPOCHS   I*4
CC                         IOK1(1): CODE EPOCHS
CC                         IOK1(2): PHASE EPOCHS
CC               NSMEA(I),I=1,2: NUMBER OF SATELLITES         I*4
CC               ANZCOD(I,K),I=1,..,MXCSAT,K=1,2: NUMBER OF   I*4
CC                        CODE OBSERVATIONS SAT.I FREQ.K
CC               ANZPHA(I,K),I=1,..,MXCSAT,K=1,2: NUMBER OF   I*4
CC                        PHASE OBSERVATIONS SAT.I FREQ.K
CC               NUMMRK(I,K),I=1,..,MXCSAT,K=1,2: NUMBER OF   I*4
CC                        MARKED OBSERVATIONS
CC               PHA001(I,K),I=1,..,MXCSAT,K=1,2: INITIAL     R*8
CC                        VALUE FOR PHASE OBSERVATIONS
CC               IOBPRT(1:4): NUM. OF OBS. IN BERNESE FILES   I*4
CC                        1: P1/C1 2: P2 3:L1 4:L2
CC               ISAPRT(1:4): NUM. OF SAT. IN BERNESE FILES   I*4
CC                        1: GPS-CODE    2: GLONASS CODE
CC                        3: GPS-PHASE   4: GLONASS PHASE
CC               IAMPRT:  NUM. OF AMBIG. IN BERNESE FILES     I*4
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  W. GURTNER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  89/04/15 13:54
CC
CC CHANGES    :  11-JAN-93 : ??: USE OF SR "OPNFIL" TO OPEN FILES
CC               07-OCT-97 : TS: ADDED ICSFLG TO SET PHA001 TO ZERO.
CC                               PHASE IS THEN NOT INITIALIZED TO ZERO.
CC               14-DEC-00 : RD: OPEN A SCRATCH FILE FOR THE OBSERVATIONS
CC               13-MAY-03 : RD: INIT VARIABLES FOR PROGRAM OUTPUT
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1989     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE s_opnfil
      USE s_opnerr
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IAMPRT, ICSFLG, II    , IOSTAT, K     , MXCSAT
C
      REAL*8    PHA0
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*32 FILCOD,FILPHA,SCROBS(2)
      CHARACTER*6  MXNSAT
      INTEGER*4    NSMEA(2),ANZCOD(MXCSAT,*),ANZPHA(MXCSAT,*)
      INTEGER*4    NUMMRK(MXCSAT,*),IOK1(2),NEPFLG(2)
      INTEGER*4    IOBPRT(4),ISAPRT(4)
      REAL*8       TIMREF(2),PHA001(MXCSAT,*)
      REAL*8       TLAST(2)
      COMMON/MCMSAT/MXCSAT,MXNSAT
C
C INITIALIZE OBSERVATION TIMES AND INTERVAL
C -----------------------------------------
      TIMREF(1)=1.D20
      TIMREF(2)=1.D20
      TLAST (1)=0.D0
      TLAST (2)=0.D0
C
C INITIALIZE ARRAYS FOR CODES AND PHASES
C --------------------------------------
      NEPFLG(1)=0
      NEPFLG(2)=0
      IOK1(1)=0
      IOK1(2)=0
      NSMEA(1)=0
      NSMEA(2)=0
C
C SET PHA001 WHICH IS USED IN "SR SAVMEA" TO INITIALIZE THE PHASE
C ---------------------------------------------------------------
      IF (ICSFLG.EQ.1) THEN
        PHA0 = 0.00D0
      ELSE
        PHA0 = 0.01D0
      ENDIF
C
      DO 40 I=1,MXCSAT
        DO 30 K=1,2
          ANZCOD(I,K)=0
          ANZPHA(I,K)=0
          NUMMRK(I,K)=0
          PHA001(I,K)=PHA0
30      CONTINUE
40    CONTINUE
C
C INIT VARIABLES FOR PROGRAM OUTPUT STATISTIC
C -------------------------------------------
       DO II=1,4
         IOBPRT(II)=0
         ISAPRT(II)=0
       ENDDO
       IAMPRT=0
C
C OPEN OBS. OUTPUT FILES
C ----------------------
      IF(FILCOD.NE.' ') THEN
        CALL OPNFIL(LFN001,SCROBS(1),'UNKNOWN','UNFORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFN001,IOSTAT,SCROBS(1),'R3INIT')
      ENDIF
      IF(FILPHA.NE.' ') THEN
        CALL OPNFIL(LFN002,SCROBS(2),'UNKNOWN','UNFORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFN002,IOSTAT,SCROBS(2),'R3INIT')
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
