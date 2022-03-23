      MODULE s_NXTIND
      CONTAINS

C*
      SUBROUTINE NXTIND(NAMB,NVAL,ITH,IAMB,CONDLC,INEXT)
CC
CC NAME       :  NXTIND
CC
CC PURPOSE    :  SR USED FOR AMBIGUITY RESOLUTION, STRATEGY 2.
CC               LET:  IAMB(I),I=1,2,..,NAMB CURRENT SET
CC                             OF AMBIGUITIES
CC                     CONDLC(K,I),K=1,2,I=1,2,..,NAMB*(NAMB-1)/2
CC                             CONDITIONS FOR (LINE-WISE LINEARIZED)
CC                             DIFFERENCES BETWEEN AMBIGUITIES
CC                     XCOND(K,I): CONDITIONS FOR L1/L2 PAIRS
CC                     DECIDE WHETHER CURRENT LC IS ALLOWED.
CC                     IF NOT: COMPUTE NEXT INDEX
CC
CC PARAMETERS :
CC         IN :  NAMB   : NUMBER OF AMBIGUITIES               I*4
CC               NVAL(I),I=1,2,..,NAMB: NUMBER OF AMBIGUITY   I*4
CC                        VALUES FOR EACH AMBIGUITY
CC               ITH    : INDEX OF CURRENT AMBIGUITY SET      R*8
CC               IAMB(I),I=1,2,..,NAMB: CURRENT SET           I*4
CC               CONDLC(K,I),K=1,2,I=1,2,..: CONDITION        I*4
CC                        FOR AMBIGUITIES 1-2,1-3,..,1-NAMB,
CC                        2-3,2-4,..,...,
CC        OUT :  INEXT  : INDEX OF NEXT COMBINATION TO BE     R*8
CC                        TESTED
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.4  (JAN 94)
CC
CC CREATED    :  89/07/15 08:44
CC
CC CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1989     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I    , IA   , IA0  , IA1  , IND  , ITEST, KA   , KA1  ,
     1          NAMB
C
      REAL*8    XINCR
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER*4 NVAL(*),IAMB(*),CONDLC(2,*)
C
      REAL*8 ITH,INEXT
C
C INITIALIZE INEXT
      INEXT=ITH
C
C CHECK ALL CONDITION EQUATIONS
C -----------------------------
      DO 200 KA=1,NAMB-1
        KA1=IAMB(KA+1)
        IA0=0
        DO 100 IA=1,KA
          IA1=IAMB(IA)
          ITEST=IA1-KA1
          IND=IA0+KA-(IA-1)
          IA0=IA0+NAMB-IA
          IF(ITEST.LT.CONDLC(1,IND).OR.
     1       ITEST.GT.CONDLC(2,IND))THEN
            XINCR=1.D0
            DO 10 I=KA+2,NAMB
              XINCR=XINCR*NVAL(I)
10          CONTINUE
            INEXT=ITH+XINCR
            GO TO 999
          END IF
100     CONTINUE
200   CONTINUE
C
999   RETURN
      END SUBROUTINE

      END MODULE
