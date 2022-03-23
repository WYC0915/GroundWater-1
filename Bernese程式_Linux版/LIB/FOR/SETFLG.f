      MODULE s_SETFLG
      CONTAINS

C*
      SUBROUTINE SETFLG(FLAG,IBIT)
CC
CC NAME       :  SETFLG
CC
CC PURPOSE    :  SET BIT NUMBER IBIT IN CHARACTER*1 FLAG
CC
CC PARAMETERS :
CC         IN :  FLAG   : CHARACTER FLAG                     CH*1
CC               IBIT   : BIT NUMBER (0<=IBIT<=7)             I*4
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  W. GURTNER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/26  10:34
CC
CC CHANGES    :  07-APR-95 : SF: AVIOD BIT SETTING INTRINSIC FUNCTION
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               11-MAR-10 : SL: ONLY ADDED TO USE M_BERN
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,    ONLY: lfnErr

      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IBIT
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4(I-N)
C
C GLOBAL DECLARATIONS
C -------------------
      CHARACTER*1 FLAG
C
C INCLUDE FILES
C -------------
C
C SET FLAG
C --------
      IF(IBIT.GE.0.AND.IBIT.LE.7) THEN
        IF (MOD((ICHAR(FLAG)/2**IBIT),2).EQ.0) THEN
          FLAG=CHAR(ICHAR(FLAG)+2**IBIT)
        ENDIF
      ELSE
        WRITE(LFNERR,1) IBIT
1       FORMAT(/,' *** SR SETFLG: ILLEGAL BIT NUMBER',/,
     1                       16X,'BIT NUMBER:',I3,/)
      END IF
C
C RETURN CODES
C ------------
      RETURN
      END SUBROUTINE

      END MODULE
