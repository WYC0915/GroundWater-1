      MODULE s_CHKBR1
      CONTAINS

C*
      SUBROUTINE CHKBR1(EPH,STATUS)
CC
CC NAME       :  CHKBR1
CC
CC PURPOSE    :  CHECK ONE EPHEMERIS MESSAGE FOR GARBAGE. SET STATUS
CC               ACCORDING TO THE RESULT OF THE CHECK
CC
CC PARAMETERS :
CC         IN :  EPH    : EPHEMERIDES INFORMATION              R*8(20)
CC                        EPH(I):
CC                          I: EPHEMERIDE ELEMENT
CC                          EPH(1) : GPS-WEEK
CC                          EPH(2) : T0E
CC                          EPH(3) : A
CC                          EPH(4) : E
CC                          EPH(5) : I
CC                          EPH(6) : R.A. OF ASCENDING NODE
CC                          EPH(7) : PERIGEE
CC                          EPH(8) : MEAN ANOMALY (T0E)
CC                          EPH(9) : DN (CORRECTION TO MEAN MOTION)
CC                          EPH(10): RATE OF NODE
CC                          EPH(11): CUS
CC                          EPH(12): CUC
CC                          EPH(13): CRS
CC                          EPH(14): CRC
CC                          EPH(15): CIS
CC                          EPH(16): CIC
CC                          EPH(17): AODE
CC                          EPH(18): IDOT
CC                          EPH(19): NOT USED
CC                          EPH(20): NOT USED
CC                          EPH(21): GPS WEEK OF THE NEXT EPHEMERIDE
CC                              :        :
CC        OUT :  STATUS : STATUS OF THE MESSAGE (RESULT OF    CH*8
CC                          THE CHECKS PERFORMED)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  88/08/02 09:08
CC
CC CHANGES    :  31-MAR-94 : MR: CHANGE RANGE FOR SEMI MAJOR AXIS
CC               27-APR-95 : MR: ADD TEST FOR T0E MODULO 100
CC               22-APR-00 : HU: INCLINATION LIMIT DECREAED TO 52 DEG
CC                               (SATELLITE 11)
CC               25-MAY-00 : DI: CHANGE MODULO TESTS FOR T0E
CC               04-APR-04 : HU: LIMITS FOR I: 52 -> 50, 57 -> 58
CC               07-APR-04 : HU: LIMIT DN 0.6E-8 -> 0.7E-8
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               04-MAY-12 : RD: USE DMOD FROM MODULE
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE d_const, ONLY: PI
      USE l_basfun, ONLY: dmod
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IWEEK
C
      REAL*8    A    , DN   , E    , ODOT , PER  , T0E  , XI   , XM0  ,
     1          XNODE
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
      IWEEK=IDNINT(EPH(1))
      T0E  =EPH(2)
      A    =EPH(3)
      E    =EPH(4)
      XI   =EPH(5)*180/PI
      XNODE=EPH(6)*180/PI
      PER  =EPH(7)*180/PI
      XM0  =EPH(8)*180/PI
      DN   =EPH(9)
      ODOT =EPH(10)*180/PI
C
C CHECK GPS WEEK
C --------------
      IF(IWEEK.LE.0.OR.IWEEK.GT.2000) THEN
        STATUS='BAD WEEK'
        GOTO 100
      ENDIF
C
C CHECK T0E
C ---------
      IF(T0E.LT.0.D0 .OR. T0E.GT.7*86400.D0 .OR.
     1   (DABS(DMOD(T0E,100.D0)-3.D0).GT.1.D-5 .AND.
     2   (DMOD(T0E+1.D-5,100.D0)).GT.2.D-5)) THEN
        STATUS='BAD T0E'
        GOTO 100
      ENDIF
C
C CHECK A
C -------
      IF(A.LT.26.0D6.OR.A.GT.27.0D6) THEN
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
      IF((XI.GT.65.D0.OR.XI.LT.60.D0).AND.(XI.GT.58.OR.XI.LT.50)) THEN
        STATUS='BAD I'
        GOTO 100
      ENDIF
C
C CHECK NODE
C ----------
      IF(DABS(XNODE).LT.1.D-6) THEN
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
C CHECK MEAN ANOMALY
C ------------------
      IF(DABS(XM0).LT.1.D-6) THEN
        STATUS='BAD M0'
        GOTO 100
      ENDIF
C
C CHECK CORRECTION TO MEAN MOTION
C -------------------------------
      IF(DN.LT.0.01D-8.OR.DN.GT.0.70D-8) THEN
        STATUS='BAD DN'
        GOTO 100
      ENDIF
C
C CHECK RATE OF NODE
C ------------------
      IF(ODOT.LT.-0.60D-6.OR.ODOT.GT.-0.25D-6) THEN
        STATUS='BAD ODOT'
        GOTO 100
      ENDIF
C
C END
C ---
100   CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
