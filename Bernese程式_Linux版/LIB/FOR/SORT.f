      MODULE s_SORT
      CONTAINS

C*
      SUBROUTINE SORT(N,RA)
CC
CC NAME       :  SORT
CC
CC PURPOSE    :  SORTS AN ARRAY RA OF LENGTH N INTO ASCENDING NUMERICAL ORDER
CC               USING THE HEAPSORT ALGORITHM. N IS INPUT RA IS REPLACED ON
CC               OUTPUT BY ITS SORTED REARRANGEMENT
CC
CC PARAMETERS :
CC         IN :  N      : NUMBER OF INPUT VALUES              I*4
CC               RA(N)  : INPUT VALUES                        R*8(*)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  T.A. SPRINGER
CC
CC VERSION    :  4.1  (JUN 96)
CC
CC CREATED    :  25-JUN-96
CC
CC CHANGES    :  25-JUN-96 : TS: CREATED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1996     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I  , IR , J  , L  , N
C
      REAL*8    RRA
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      REAL*8 RA(*)
C
      L  = N/2+1
      IR = N
10    CONTINUE
        IF (L.GT.1) THEN
          L   = L-1
          RRA = RA(L)
        ELSE
          RRA    = RA(IR)
          RA(IR) = RA(1)
          IR     = IR-1
          IF (IR.EQ.1) THEN
            RA(1) = RRA
            RETURN
          ENDIF
        ENDIF
        I = L
        J = L+L
20      IF (J.LE.IR) THEN
          IF (J.LT.IR) THEN
            IF (RA(J).LT.RA(J+1)) J = J+1
          ENDIF
          IF (RRA.LT.RA(J)) THEN
            RA(I) = RA(J)
            I = J
            J = J+J
          ELSE
            J = IR+1
          ENDIF
        GOTO 20
        ENDIF
        RA(I) = RRA
      GOTO 10
C
      END SUBROUTINE

      END MODULE
