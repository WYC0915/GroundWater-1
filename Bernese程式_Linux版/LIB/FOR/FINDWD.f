      MODULE s_FINDWD
      CONTAINS

C*
      SUBROUTINE FINDWD(STRING,N,IST,IEND)
CC
CC NAME       :  FINDWD
CC
CC PURPOSE    :  FIND THE NTH WORD IN STRING
CC               (BPE)
CC
CC PARAMETERS :
CC                                                             T
CC         IN :
CC      IN/OUT:
CC        OUT :
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  J. JOHNSON
CC
CC VERSION    :  3.6  (AUG 94)
CC
CC CREATED    :  17-AUG-94
CC
CC CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               19-JUL-10 : SL: TAB CHARACTERS REMOVED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1991     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I    , IEND , IST  , IWORD, L    , N
C
      CHARACTER*(*) STRING
C
      LOGICAL DELIM
C
      L = LEN(STRING)
      I = 1
      IWORD = 0
      DELIM = .TRUE.
      IST = 0
      IEND = 0
100   CONTINUE
         IF (I .GT. L) GOTO 200
         IF (DELIM) THEN
           IF (STRING(I:I) .NE. ' ') THEN
             DELIM = .FALSE.
             IWORD = IWORD + 1
             IF (IWORD .EQ. N) IST = I
           ENDIF
         ELSE IF (STRING(I:I) .EQ. ' ') THEN
           DELIM = .TRUE.
           IF (IWORD .EQ. N) THEN
             IEND = I-1
             GOTO 200
           ENDIF
         ENDIF
         I=I+1
         GOTO 100
C
200   CONTINUE
      IF ((IST.EQ.0).OR.(IEND.EQ.0)) THEN
        IST=0
        IEND=0
      ENDIF
      RETURN
      END SUBROUTINE













      END MODULE
