      MODULE s_WILDCD
      CONTAINS

C*
      SUBROUTINE WILDCD(WILD,STRING,IFOUND)
CC
CC NAME       :  WILDCD
CC
CC PURPOSE    :  TEST STRING FOR WILD CARD
CC
CC PARAMETERS :
CC         IN :  WILD   : WILD CARD CHARACTERIZATION          CH*(*)
CC               STRING : STRING TO TEST                      CH*(*)
CC        OUT :  IFOUND : 1: FOUND   0: NOT FOUND              I*4
CC
CC REMARKS    :  WILDCARD STRING < 128 CHARACTERS
CC
CC AUTHOR     :  W. GURTNER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  24-NOV-92
CC
CC CHANGES    :  31-JAN-93 : ??: ALLOW '?', TOO
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1992     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE f_lengt0
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IEND  , IFOUND, ILSTAR, IRSTAR, ISTAR , IW1   ,
     1          IW2   , J     , K     , KS1   , KS2   , L     ,
     2          LS    , LW
C
      CHARACTER WILD*(*),STRING*(*)
      CHARACTER W*127
C
      IF(WILD.EQ.STRING.OR.WILD.EQ.'*') THEN
        IFOUND=1
        GOTO 900
      END IF
      IFOUND=0
      W=WILD
      LW=LENGT0(W)
      LS=LENGT0(STRING)
      IF(LS.EQ.0.OR.LW.EQ.0) GOTO 900
C
C  LOOK FOR MULTIPLE ASTERISKS, DELETE AND SHIFT
230   ISTAR=0
      DO 210 I=1,LW
        IF(W(I:I).EQ.'*') THEN
          IF(ISTAR.EQ.1) THEN
            DO 220 K=I,LW
              W(K-1:K-1)=W(K:K)
220         CONTINUE
            LW=LW-1
            GOTO 230
          ELSE
            ISTAR=1
          END IF
        ELSE
          ISTAR=0
        END IF
210   CONTINUE
C
      IF(W(1:1).EQ.'*') THEN
        ILSTAR=1
        IW1=2
      ELSE
        ILSTAR=0
        IW1=1
      END IF
C
      KS1=1
C
C  LOOK FOR NEXT SUBSTRING WITHIN WILDCARD STRING
C
      IEND=0
10    IRSTAR=0
      DO 20 I=IW1,LW
        IF(W(I:I).EQ.'*') THEN
          IRSTAR=1
          IF(I.EQ.LW) IEND=1
          GOTO 30
        END IF
20    CONTINUE
      IEND=1
30    IW2=I-1
C
C  LOOK FOR SUBSTRING W(IW1:IW2) IN STRING(KS1:KS2)
      IF(ILSTAR.EQ.1) THEN
        KS2=LS
      ELSE
        KS2=KS1+(IW2-IW1)
      END IF
C
      DO 110 K=KS1,KS2
        DO 120 J=1,IW2-IW1+1
          I=IW1+J-1
          L=K+J-1
          IF(L.GT.LS.OR.L.GT.KS2) GOTO 900
          IF(W(I:I).EQ.'%'.OR.W(I:I).EQ.'?') GOTO 120
          IF(W(I:I).NE.STRING(L:L)) GOTO 110
120     CONTINUE
C
C  FOUND
        IF(IEND.EQ.1) THEN
          IF(IRSTAR.EQ.1.OR.L.EQ.LS) IFOUND=1
          GOTO 900
        ELSE
C  LOOK FOR NEXT SUBSTRING
          KS1=L+1
          ILSTAR=IRSTAR
          IW1=IW2+2
          GOTO 10
        END IF
C
110   CONTINUE
C
C  NOT FOUND
      GOTO 900
C
900   RETURN
      END SUBROUTINE

      END MODULE
