      MODULE f_lengt1
      CONTAINS

C*
        FUNCTION LENGT1(STRING)
CC
CC NAME       :  LENGT1
CC
CC PURPOSE    : DETERMINE LENGTH OF A GIVEN STRING
CC              BUT WILL RETURN 1 IF THE LENGTH IS ZERO
CC              FOR USE WIT SUB STRING EXPRESSIONS LIKE:
CC                  STR(1:LENGT1(STR))
CC              SINCE STR(1:0) IS NOT ALLOWED.
CC              (TRAILING BLANKS OR ASCII NUL EXCLUDED)
CC
CC PARAMETERS :
CC         IN :  STRING: GIVEN STRING                             CH*(*)
CC        OUT :  LENGTH: LENGTH OF STRING, OR 1 IF EMPTY STRING    I*4
CC
CC REMARKS     :
CC
CC RESTRICTION : NONE
CC
CC AUTHOR      : W. GURTNER
CC               ASTRONOMICAL INSTITUTE, UNIVERSITY OF BERN
CC               SWITZERLAND
CC
CC CREATED     : 29-SEP-95
CC
CC CHANGES     : 23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS
CC               19-JUL-10 : SL: TAB CHARACTERS REMOVED
CC
C*
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , LENGT1, LENSTR
C
        CHARACTER STRING*(*)
        CHARACTER NUL
        NUL=CHAR(0)
C
        LENSTR=LEN(STRING)
        DO 10 I=LENSTR,1,-1
          IF(STRING(I:I).NE.' '.AND.STRING(I:I).NE.NUL) GOTO 20
10      CONTINUE
20      CONTINUE
        IF (I.GT.0) THEN
          LENGT1=I
        ELSE
          LENGT1=1
        ENDIF
        RETURN
        END FUNCTION

      END MODULE
