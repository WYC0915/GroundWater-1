      MODULE s_DMATRD
      CONTAINS

C*
      SUBROUTINE DMATRD(RCS,N,M,IDEL,MAT,NNEW,MNEW)
CC
CC NAME       :  DMATRD
CC
CC PURPOSE    :  (A) RCS = 'R'
CC                   MAT IS A COLUMNWISE LINEARIZED
CC                   DOUBLE PRECISION MATRIX
CC                   WITH N ROWS AND M COLUMNS.
CC                   IF IDEL(I)=1 , ROW I IS DELETED
CC                   IF IDEL(I)=0 , THIS ROW IS TAKEN INTO THE
CC                                  RESULTING MATRIX
CC
CC               (B) RCS = 'C'
CC                   MAT IS A COLUMNWISE LINEARIZED
CC                   DOUBLE PRECISION MATRIX
CC                   WITH N ROWS AND M COLUMNS.
CC                   IF IDEL(I)=1 , COLUMN I IS DELETED
CC                   IF IDEL(I)=0 , THIS COLUMN IS TAKEN INTO THE
CC                                  RESULTING MATRIX
CC
CC               (C) RCS = 'S'
CC                   MAT IS THE UPPER TRIANGULAR,
CC                   COLUMNWISE LINEARIZED PART OF A
CC                   DOUBLE PRECISION N*N MATRIX
CC                   IF IDEL(I)=1, COLUMN AND ROW I ARE DELETED.
CC
CC               NNEW, MNEW ARE THE DIMENSIONS OF THE RESULTING
CC               MATRIX.
CC
CC PARAMETERS :
CC         IN :  RCS    : OPTION SELECTION (SEE ABOVE)        I*4
CC               N      : NUMBER OF ROWS                      I*4
CC               M      : NUMBER OF COLUMNS                   I*4
CC               IDEL(I),I=1,2,...,N OR M : (SEE ABOVE)       I*4(..)
CC     IN/OUT :  MAT(I),I=1,2,...,N*M OR N*(N+1)/2            R*8(..)
CC        OUT :  NNEW   : NUMBER OF ROWS FOR RESULTING MATRIX I*4
CC               MNEW   : NUMBER OF COL. FOR RESULTING MATRIX I*4
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/10/30 17:32
CC
CC CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , INDNEW, K     , M     , MNEW  , N     , NNEW
C
      CHARACTER*1 RCS
      INTEGER IDEL(*)
      REAL*8 MAT(*)
C
C (A) RCS = R, DELETE ROWS
C ------------------------
      IF(RCS.EQ.'R')THEN
        MNEW=M
        NNEW=N
        INDNEW=0
        DO 20 K=1,M
          DO 10 I=1,N
            IF(K.EQ.1.AND.IDEL(I).EQ.1)NNEW=NNEW-1
            IF(IDEL(I).EQ.0)THEN
              INDNEW=INDNEW+1
              MAT(INDNEW)=MAT(I+(K-1)*N)
            END IF
10        CONTINUE
20      CONTINUE
        GO TO 999
      END IF
C
C (B) RCS = C, DELETE COLUMNS
C ---------------------------
      IF(RCS.EQ.'C')THEN
        MNEW=M
        NNEW=N
        INDNEW=0
        DO 40 K=1,M
          IF(IDEL(K).EQ.1)THEN
            MNEW=MNEW-1
            GO TO 40
          END IF
          DO 30 I=1,N
            INDNEW=INDNEW+1
            MAT(INDNEW)=MAT(I+(K-1)*N)
30        CONTINUE
40      CONTINUE
        GO TO 999
      END IF
C
C (C) RCS = S, SYMMETRIC MATRIX, DELETE ROWS AND COLUMNS
C ------------------------------------------------------
      IF(RCS.EQ.'S')THEN
        NNEW=N
        INDNEW=0
        DO 60 K=1,N
          IF(IDEL(K).EQ.1)THEN
            NNEW=NNEW-1
            GO TO 60
          END IF
          DO 50 I=1,K
            IF(IDEL(I).EQ.1)GO TO 50
            INDNEW=INDNEW+1
            MAT(INDNEW)=MAT(I+(K-1)*K/2)
50        CONTINUE
60      CONTINUE
        MNEW=NNEW
      END IF
999   RETURN
      END SUBROUTINE

      END MODULE
