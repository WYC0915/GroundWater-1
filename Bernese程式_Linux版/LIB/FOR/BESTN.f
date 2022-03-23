      MODULE s_BESTN
      CONTAINS

C*
      SUBROUTINE BESTN(INDEX,N,VALUE,NACT,SMALL,INDSML)
CC
CC NAME       :  BESTN
CC
CC PURPOSE    :  BUILD UP (AT MAXIMUM) THE N SMALLEST VALUES
CC               "VALUE" AND THE CORRESPONDING INDEX "INDEX"
CC               IN THE ARRAYS SMALL(I), INDSML(I), I=1,2,..,NACT
CC
CC PARAMETERS :
CC         IN :  INDEX  : INDEX OF CURRENT ELEMENT            R*8
CC               N      : NUMBER OF VALUES TO BE RETAINED     I*4
CC               VALUE  : VALUE OF CURRENT ELEMENT            R*8
CC     IN/OUT :  NACT   : ACTUAL NUMBER OF ELEMENTS IN ARRAYS I*4
CC                        SMALL AND INDSML
CC               SMALL(I),I=1,2,..,NACT: SMALLEST ELEMENTS    R*8
CC               INDSML(I),I=1,2,..,NACT: CORRESP. INDICES    R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.4  (JAN 94)
CC
CC CREATED    :  89/07/01 16:48
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
      INTEGER*4 I    , N    , NACT
C
      REAL*8    HELP , VALUE, XHELP
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      REAL*8 INDSML(*)
      REAL*8 INDEX,SMALL(*)
C
C UPDATE NACT ?
C -------------
      IF (NACT.LT.N) THEN
        IF (NACT.EQ.0.OR.VALUE.NE.SMALL(NACT)) THEN
          NACT=NACT+1
          SMALL(NACT)=VALUE
          INDSML(NACT)=INDEX
          DO 10 I=NACT-1,1,-1
            IF (SMALL(I+1).LT.SMALL(I)) THEN
              HELP=SMALL(I+1)
              SMALL(I+1)=SMALL(I)
              SMALL(I)=HELP
              XHELP=INDSML(I+1)
              INDSML(I+1)=INDSML(I)
              INDSML(I)=XHELP
            END IF
10        CONTINUE
        END IF
        RETURN
      END IF
C
C UPDATE COMPLETE ARRAY ?
C -----------------------
      IF (VALUE.LT.SMALL(N)) THEN
        SMALL(N)=VALUE
        INDSML(N)=INDEX
        DO 11 I=N-1,1,-1
          IF (SMALL(I+1).LT.SMALL(I)) THEN
            HELP=SMALL(I+1)
            SMALL(I+1)=SMALL(I)
            SMALL(I)=HELP
            XHELP=INDSML(I+1)
            INDSML(I+1)=INDSML(I)
            INDSML(I)=XHELP
          END IF
11      CONTINUE
      END IF
C
      RETURN
      END SUBROUTINE

      END MODULE
