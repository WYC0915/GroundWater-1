      MODULE s_INLIST
      CONTAINS

C*
      SUBROUTINE INLIST(SVN,NSAT,NUMSAT,IELE)
CC
CC NAME       :  INLIST
CC
CC PURPOSE    : CHECK WHETHER SATELLITE WITH NUMBER SVN IS IN
CC              LIST "NUMSAT".
CC
CC PARAMETERS :
CC        IN  : SVN     : SATELLITE NUMBER TO BE CHECKED            I*4
CC              NSAT    : NUMBER OF SATELLITES IN LIST              I*4
CC              NUMSAT  : SATELLITE NUMBERS                         I*4(*)
CC        OUT : IELE    : POSITION IN ARRAY; =0 IF "SVN" NOT        I*4
CC                        IN ARRAY
CC
CC REMARKS    :
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.5  (MAY 94)
CC
CC CREATED    :  94/06/19
CC
CC CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1994     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IELE, ISAT, NSAT
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
      INTEGER*4 SVN,NUMSAT(*)
C
      IELE=0
      DO 10 ISAT=1,NSAT
        IF(SVN.EQ.NUMSAT(ISAT))THEN
          IELE=ISAT
          GO TO 999
        END IF
10    CONTINUE
999   CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
