      MODULE s_UPDTRP
      CONTAINS

C*
      SUBROUTINE UPDTRP(NEP,OBTIME,IDELTT,TIMREF,SVN,SVNREF,STANAM,
     1                  ABSVAL,FLGSAT,FLGREF,AOBS,OMCMAX,NFREQ,
     2                  NSATEL,NUMSAT,NDEL,LSTDEL,NTRP,ANOR,BNOR,RMS)
CC
CC NAME       :  UPDTRP
CC
CC PURPOSE    :  UPDATE TRIPLE DIFFERENCE SOLUTION WITH ALL
CC               OBSERVATIONS OF A SPECIAL SATELLITE PAIR
CC
CC PARAMETERS :
CC         IN :  NEP    : NUMBER OF EPOCHS                    I*4
CC               OBTIME(I),I=1,2,..,NEP: OBS. TIMES           R*8
CC               IDELTT : OBSERVATION INTERVAL IN SEC         I*4
CC               TIMREF : REFERENCE TIME OF FILE              R*8
CC               SVN    : SATELLITE NUMBER                    I*4
CC               SVNREF : REFERENCE SATELLITE                 I*4
CC               STANAM(I),I=1,2: STATION NAMES               CH*16
CC               ABSVAL(I),I=1,2,..,NEP: DOUBLE DIFFERENCES   R*8
CC               FLGSAT(I),I=1,2,..,NEP: OBS-FLAGS OF         CH*1
CC                        SATELLITE CONSIDERED
CC               FLGREF(I),I=1,2,..,NEP: OBS-FLAGS OF         CH*1
CC                        REFERENCE SATELLITE
CC               AOBS(K,I),K=1,2,3, I=1,2,..,NEP: FIRST       R*8
CC                        DESIGN MATRIX
CC               OMCMAX : MAXIMUM ALLOWD O-C VALUE            R*8
CC               NFREQ  : NUMBER OF FREQUENCIES IN FILE       I*4
CC               NSATEL : TOTAL NUMBER OF SATELLITES          I*4
CC               NUMSAT(I),I=1,..,NSATEL: SATELLITE NUMBERS   I*4
CC               NDEL   : NUMBER OF DELETIONS                 I*4
CC               LSTDEL(K,I),K=1,..,5, I=1,2,..,NDEL          I*4
CC                        DEFINITION OF MARK REQUEST NUMBER I
CC                        (1,I): SV-NUMBER
CC                        (2,I): FIRST EPOCH OF MARKED AREA I
CC                        (3,I): LAST  EPOCH OF MARKED AREA I
CC                        (4,I): FREQUENCY (1=L1, 2=L2)
CC                        (5,I): MARKED BY
CC                               =1: SINGLE FREQ. REJECTION
CC                               =2: DUAL   FREQ. REJECTION
CC                               =3: UNPAIRED L1/L2 OBSERVATIONS
CC                               =4: USER
CC                               =5: SMALL ELEVATION
CC                               =6: SMALL PIECE
CC                               =7: BAD OBSERVED-COMPUTED
CC                               ALL AREAS MARKED OR CHANGED IN THE
CC                               LATEST RUN HAVE A NEGATIVE SIGN
CC     IN/OUT :  NTRP   : NUMBER OF TRIPLE DIFFERENCES USED   R*8
CC               ANOR(IK),IK=1,2,..,6: UPPER TRIANGULAR PART  R*8
CC                        OF NORMA EQN SYSTEM
CC               BNOR(I),I=1,2,3: RIGHT HAND SIDE OF NEQ SYST.R*8
CC               RMS    : SUM OF TERMS (OBS-COMP)**2          R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  88/05/04 10:40
CC
CC CHANGES    :  03-DEC-91 : ??: ADD PARAMETERS "OBTIME","IDELTT",
CC                               "TIMREF","SVN","SVNREF" TO PRINT
CC                               CORRECT EPOCH NUMBER AND SATELLITES
CC               24-APR-95 : MR: MAXIMUM ALLOWED O-C VALUE
CC               27-SEP-95 : CR: ADD PROBLEM FLAG
CC               05-OCT-95 : MR: REMOVE PROBLEM FLAG
CC               20-MAR-97 : MR: WARNING INSTEAD OF ERROR
CC               24-JAN-00 : RD: ABSLST IS SET TO 0 FOR EVERY CALL
CC                               O-C CHECK ONLY IF ABSLST.NE.0
CC               25-JUN-02 : RD: OMCMAX.EQ.0D0 TO SUPRESS THE CHECK
CC               09-AUG-02 : RD: ADD BASELINE NAME TO THE WARNING MSG.
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               23-JUN-07 : AG: I3 -> I4 FOR SVN NUMBERS
CC               31-JAN-12 : RD: CORRECT FORMAT FOR O-C MESSAGE (BASELINES)
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE f_tstflg
      USE s_mrkobs
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IDELTT, IEPO  , IEPOCH, IFRQ  , IK    , K     ,
     1          NDEL  , NEP   , NFREQ , NSATEL, NTRP
C
      REAL*8    ABSLST, ABSTRP, OMCMAX, RMS   , TIMREF
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      CHARACTER*85 LINE
      CHARACTER*16 STANAM(2)
      CHARACTER*1  FLGSAT(*),FLGREF(*)
C
      REAL*8      OBTIME(*),ABSVAL(*),AOBS(3,*),ANOR(6),BNOR(3)
      REAL*8      ATRIPL(3),ALAST(3)
C
      INTEGER*4   SVN,SVNREF,NUMSAT(*),LSTDEL(5,*)
C
C LOOP OVER ALL EPOCHS
C --------------------
      ABSLST=0.D0
      DO 40 IEPO=1,NEP
C
C SKIP EPOCH, IF "OBS-COMP"=0 OR IF OBSFLAG OF ONE OF THE TWO
C SATELLITES INVOLVED IS SET
         IF(TSTFLG(FLGSAT(IEPO),0).OR.
     1      TSTFLG(FLGREF(IEPO),0).OR.
     2      ABSVAL(IEPO).EQ.0.D0)GO TO 35
C
C FORM A TRIPLE DIFFERENCE OBSERVATION, IF BOTH SATELLITES
C INVOLVED ARE NOT MARKED TO HAVE SUFFERED A CYCLE SLIP
          IF(.NOT.TSTFLG(FLGSAT(IEPO),1).AND.
     1       .NOT.TSTFLG(FLGREF(IEPO),1).AND.(ABSLST.NE.0.D0))THEN
            ABSTRP=ABSVAL(IEPO)-ABSLST
            IF (OMCMAX.NE.0D0.AND.DABS(ABSTRP).GT.OMCMAX) THEN
              IEPOCH=IDNINT((OBTIME(IEPO)-TIMREF)*86400.D0/IDELTT)+1
C
C GENERATE A MESSAGE LINE
              LINE=' ### SR UPDTRP: EPOCH,SVN,SVNREF,O-C TOO BIG:'
              WRITE(LINE(46:80),'(I6,2I4,F10.3,1X,A)') IEPOCH,
     1              SVN,SVNREF,ABSTRP,'(' // STANAM(1)(1:4) // ')'
              IF (DABS(ABSTRP).GE.1D3) THEN
                WRITE(LINE(58:67),'(E10.3)') ABSTRP
              ENDIF
              IF (LEN_TRIM(STANAM(2)).GT.0) THEN
                WRITE(LINE(76:85),'(A)')  '-' // STANAM(2)(1:4) // ')'
              ENDIF
              WRITE(LFNERR,'(A)') TRIM(LINE)
C
              DO 90 IFRQ=1,NFREQ
                CALL MRKOBS(7,SVN,IEPOCH,IFRQ,NSATEL,NUMSAT,
     1                      NDEL,LSTDEL)
90            CONTINUE
            ELSE
C
C UPDATE NUMBER OF OBS, RMS, FORM "OBS-COMP"
              NTRP=NTRP+1
              RMS=RMS+ABSTRP**2
C
C FIRST DESIGN MATRIX
              DO 10 K=1,3
                ATRIPL(K)=AOBS(K,IEPO)-ALAST(K)
10            CONTINUE
C
C UPDATE NEQ-SYSTEM
              DO 20 I=1,3
                BNOR(I)=BNOR(I)+ATRIPL(I)*ABSTRP
                DO 20 K=1,I
                  IK=K+(I-1)*I/2
                  ANOR(IK)=ANOR(IK)+ATRIPL(I)*ATRIPL(K)
20            CONTINUE
            ENDIF
          ENDIF
C
C SAVE CURRENT OBSERVATION AS PRECEEDING OBSERVATION
21        ABSLST=ABSVAL(IEPO)
          DO 30 K=1,3
            ALAST(K)=AOBS(K,IEPO)
30        CONTINUE
          GO TO 40
35        CONTINUE
40      CONTINUE
        RETURN
        END SUBROUTINE

      END MODULE

