      MODULE s_CHKBR3
      CONTAINS

C*
      SUBROUTINE CHKBR3(EPH,STATUS)
CC
CC NAME       :  CHKBR3
CC
CC PURPOSE    :  CHECK ONE EPHEMERIS MESSAGE FOR GARBAGE. SET STATUS
CC               ACCORDING TO THE RESULT OF THE CHECK
CC
CC PARAMETERS :
CC         IN :    EPH  : EPHEMERIDES INFORMATION              R*8(16)
CC                        EPH(I):
CC                          I: EPHEMERIDE ELEMENT
CC                          EPH(1) : MJD
CC                          EPH(2) : NOT USED
CC                          EPH(3) : A
CC                          EPH(4) : E
CC                          EPH(5) : I
CC                          EPH(6) : R.A. OF ASCENDING NODE
CC                          EPH(7) : PERIGEE
CC                          EPH(8) : T0
CC                          EPH(9) : NOT USED
CC                          EPH(10): NOT USED
CC                          EPH(11): NOT USED
CC                          EPH(12): NOT USED
CC                          EPH(13): NOT USED
CC                          EPH(14): NOT USED
CC                          EPH(15): NOT USED
CC                          EPH(16): NOT USED
CC                          EPH(17): NOT USED
CC                          EPH(18): NOT USED
CC                          EPH(19): NOT USED
CC                          EPH(20): NOT USED
CC
CC        OUT :  STATUS : STATUS OF THE MESSAGE (RESULT OF     CH*8
CC                          THE CHECKS PERFORMED)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER, H.HABRICH
CC
CC VERSION    :  4.1  (MAR 97)
CC
CC CREATED    :  20-MAR-97
CC
CC CHANGES    :  16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE d_const, ONLY: PI
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      REAL*8    A  , E  , PER, T0E, XI , XKN
C
CCC       IMPLICIT REAL*8(A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
      REAL*8      EPH(20)
      CHARACTER*8 STATUS
C
C
C INITIALIZE STATUS
C -----------------
      STATUS=' '
C
C SET EPHEMERIS ELEMENTS
C ----------------------
      T0E   =EPH(1)
      A     =EPH(3)
      E     =EPH(4)
      XI    =EPH(5)*180/PI
      XKN   =EPH(6)*180/PI
      PER   =EPH(7)*180/PI
C
C CHECK T0E
C ---------
      IF(T0E.LT.5.D4.OR.T0E.GT.6.D4) THEN
        STATUS='BAD T0E'
        GOTO 100
      ENDIF
C
C CHECK A
C -------
      IF(A.LT.25.0D6.OR.A.GT.26.0D6) THEN
        STATUS='BAD A'
        GOTO 100
      ENDIF
C
C CHECK E
C -------
      IF(E.LT.0.D0.OR.E.GT.0.1D0) THEN
        STATUS='BAD E'
        GOTO 100
      ENDIF
C
C CHECK I
C -------
      IF(XI.GT.67.D0.OR.XI.LT.63.D0) THEN
        STATUS='BAD I'
        GOTO 100
      ENDIF
C
C CHECK NODE
C ----------
      IF(DABS(XKN).LT.1.D-6) THEN
        STATUS='BAD NODE'
        GOTO 100
      ENDIF
C
C CHECK PERIGEE
C -------------
      IF(DABS(PER).LT.1.D-6) THEN
        STATUS='BAD PERI'
        GOTO 100
      ENDIF
C
C END
C ---
100   CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
