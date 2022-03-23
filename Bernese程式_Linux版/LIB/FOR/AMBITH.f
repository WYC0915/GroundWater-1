      MODULE s_AMBITH
      CONTAINS

C*
      SUBROUTINE AMBITH(ITH,MAXSET,MAXVAL,NAMB,NVAL,AMBVAL,
     1                  AMBCOR,MATCH,AMBL1,AMB)
CC
CC NAME       :  AMBITH
CC
CC PURPOSE    :  DEFINE THE I-TH SET OF AMBIGUITY VALUES
CC               AMB(I),I=1,2,..,NAMB.
CC               AMB(I) MAY BE ONE OF THE VALUES
CC               VAL(1),VAL(2),..,VAL(NVAL(I))
CC               THE INDEX OF THE LAST AMBIGUITY IS
CC               FASTEST VARYING
CC
CC PARAMETERS :
CC         IN :  ITH    : NUMBER OF AMBIGUITY SET             I*4
CC               MAXSET : NUMBER OF VALUES FOR SPECIAL SET    I*4
CC               MAXVAL : MAX NUMBER OF AMB. VALUES           I*4
CC               NAMB   : NUMBER OF AMBIGUITIES               I*4
CC               NVAL(I),I=1,2,..,NAMB: NUMBER OF VALUES      I*4
CC                        FOR AMBIGUITY I
CC               AMBVAL(I,K),I=1,2,..,NVAL(I),K=1,..,NAMB:    I*4
CC                        AMBIGUITY VALUES
CC               AMBCOR(I,K),I=1,2,..,NVAL(I),K=1,..,NAMB:    I*4
CC                        CORRESPONDING L2 VALUES
CC               MATCH(I,K),I=1,2, K=1,2,...,NAMB             I*4
CC                        AMBIGUITY INDICES IN ORIGINAL ARRAY
CC        OUT :  AMBL1(I),I=1,2,..,NAMB: I-TH SET OF AMBIGUITY I*4
CC                        VALUES (L1 ONLY)
CC               AMB(I),I=1,2,..,NAMB: I-TH SET OF AMBIGUITY  I*4
CC                        VALUES (L1&L2)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G. BEUTLER
CC
CC VERSION    :  3.4  (JAN 94)
CC
CC CREATED    :  89/06/30 09:15
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               04-MAY-12 : RD: USE DMOD FROM MODULE, USE M_BERN WITH ONLY
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1989     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnerr
      USE l_basfun, ONLY: dmod
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I1    , I2    , IAMB  , IND   , MAXVAL, NAMB
C
      REAL*8    HELP  , XTEST
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      INTEGER*4 NVAL(*),AMBVAL(MAXVAL,*),AMB(*),AMBL1(*),MATCH(2,*)
      INTEGER*4 AMBCOR(MAXVAL,*)
C
      REAL*8    ITH,MAXSET,NENNER
C
C
C CHECK VALIDITY OF REQUEST
      IF(ITH.GT.MAXSET)THEN
        WRITE(LFNERR,20) ITH,MAXSET
20      FORMAT(/,' *** SR AMBITH: TOO MANY AMBIGUITY SETS',/,
     1                       16X,'AMBIGUITY SET NO   :',F20.0,/,
     2                       16X,'MAX. NUMBER OF SETS:',F20.0,/)
        CALL EXITRC(2)
      END IF
C
C DEFINE I-TH AMBIGUITY SET
      DO 100 IAMB=NAMB,1,-1
        NENNER=1
        DO 30 I=NAMB,IAMB+1,-1
          NENNER=NENNER*NVAL(I)
30      CONTINUE
        XTEST=(ITH-1.D0)/NENNER
        HELP=NVAL(IAMB)
        IND=DMOD(XTEST,HELP)+1.D0
        AMBL1(IAMB)=AMBVAL(IND,IAMB)
        I1=MATCH(1,IAMB)
        AMB(I1)=AMBL1(IAMB)
        IF(MATCH(2,IAMB).NE.0)THEN
          I2=MATCH(2,IAMB)
          AMB(I2)=AMBCOR(IND,IAMB)
        END IF
100   CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
