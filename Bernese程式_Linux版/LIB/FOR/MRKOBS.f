      MODULE s_MRKOBS
      CONTAINS

C*
      SUBROUTINE MRKOBS(MRKTYP,SVN,IEPOCH,IFRQ,NSATEL,NUMSAT,
     1                  NDEL,LSTDEL)
CC
CC NAME       :  MRKOBS
CC
CC PURPOSE    :  - MARK OBSERVATIONS
CC
CC PARAMETERS :
CC         IN :  MRKTYP : TYP OF MARKING
CC                        = 3 ... L1 WITHOUT L2
CC                        = 5 ... SMALL ELEVATION
CC                        = 6 ... SMALL PIECES
CC               SVN    : SATELLITE NUMBER (IN EPOCH)
CC               IEPOCH : EPOCH NUMBER
CC               IFRQ   : FREQUENCE
CC               NSATEL : NUMBER OF SATELLITES (IN HEADER)    I*4
CC               NUMSAT(I),I=1,..,NSATEL: SATELLITE NUMBERS   I*4
CC     IN/OUT :  NDEL   : NUMBER OF DELETIONS                 I*4
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
CC                               =6: SMALL PIECES
CC                               =7: BAD OBSERVED-COMPUTED
CC                               =8: GPS SATELLITE CLOCK IS MISSING
CC                               ALL AREAS MARKED OR CHANGED IN THE
CC                               LATEST RUN HAVE A NEGATIVE SIGN
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER, L.MERVART
CC
CC VERSION    :  3.4
CC
CC CREATED    :  05-JUN-92
CC
CC CHANGES    :  23-DEC-92 : DECLARATION OF "MXNDEL"
CC               10-AUG-94 : MR: CALL EXITRC
CC               20-JAN-95 : MR: ERROR MESSAGE CORRECTED
CC               24-APR-95 : MR: UPDATE DESCRIPTION OF LSTDEL
CC               21-AUG-95 : MR: OMIT DOUBLE MARKING
CC               10-JAN-02 : DS: MARKING WHEN GPS SAT CLOCK IS MISSING
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               20-MAY-09 : DT: IFRQ=0 if screen frequ. as available
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IDEL  , IEPOCH, IFRQ  , MRKTYP, MXCDEL, NDEL  , NSATEL
C
CCC       IMPLICIT     REAL*8 (A-H,O-Z)
CCC       IMPLICIT     INTEGER*4 (I-N)
C
      CHARACTER*6  MXNDEL
C
      INTEGER*4    SVN,NUMSAT(*),LSTDEL(5,*)
C
C COMMON BLOCKS
C -------------
      COMMON/MCMDEL/MXCDEL,MXNDEL
C
C APPEND TO EXISTING MARKED AREA
C ------------------------------
      DO 32 IDEL=NDEL,1,-1
        IF(LSTDEL(1,IDEL).EQ.SVN      .AND.
     1     LSTDEL(3,IDEL).EQ.IEPOCH-1 .AND.
     2     (LSTDEL(4,IDEL).EQ.IFRQ .AND. IFRQ>0) .AND.
     3     IABS(LSTDEL(5,IDEL)).EQ.MRKTYP) THEN
             LSTDEL(3,IDEL)=IEPOCH
             LSTDEL(5,IDEL)=-MRKTYP
             GOTO 34
        ELSEIF(LSTDEL(1,IDEL).EQ.SVN    .AND.
     1         LSTDEL(2,IDEL).LE.IEPOCH .AND.
     2         LSTDEL(3,IDEL).GE.IEPOCH .AND.
     3         (LSTDEL(4,IDEL).EQ.IFRQ .AND. IFRQ>0)) THEN
          GOTO 34
        ENDIF
32    CONTINUE
C
C DEFINE NEW MARKED AREA
C ----------------------
      NDEL=NDEL+1
      IF(NDEL.GT.MXCDEL) THEN
        WRITE(LFNERR,33) NDEL,MXCDEL
33      FORMAT(/,' *** SR MRKOBS: MAX. NUMBER OF ',
     1                      'DELETIONS EXCEEDED',
     2       /,16X,'NUMBER OF DELETIONS >=',I6,/,
     3         16X,'MAX. NUMBER OF DEL.  :',I6,/)
        CALL EXITRC(2)
      ENDIF
      LSTDEL(1,NDEL)=SVN
      LSTDEL(2,NDEL)=IEPOCH
      LSTDEL(3,NDEL)=IEPOCH
      LSTDEL(4,NDEL)=IFRQ
      LSTDEL(5,NDEL)=-MRKTYP
C
34    RETURN
      END SUBROUTINE

      END MODULE
