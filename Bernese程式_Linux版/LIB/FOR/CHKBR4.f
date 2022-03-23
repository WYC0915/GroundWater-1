      MODULE s_CHKBR4
      CONTAINS

C*
      SUBROUTINE CHKBR4(EPH1,EPH2,STATUS)
CC
CC NAME       :  CHKBR4
CC
CC PURPOSE    :  CHECK TWO GLONASS BROADCAST MESSAGES FOR CONSISTENCY
CC
CC PARAMETERS :
CC         IN :  EPH1   : EPHEMERIDES INFORMATION (FIRST SET)  R*8(20)
CC                        EPH1(I):
CC                          I: EPHEMERIDE ELEMENT
CC                          EPH1(1) : MJD
CC                          EPH1(2) : NOT USED
CC                          EPH1(3) : A
CC                          EPH1(4) : E
CC                          EPH1(5) : I
CC                          EPH1(6) : R.A. OF ASCENDING NODE
CC                          EPH1(7) : PERIGEE
CC                          EPH1(8) : T0
CC                          EPH1(9) : NOT USED
CC                          EPH1(10): NOT USED
CC                          EPH1(11): NOT USED
CC                          EPH1(12): NOT USED
CC                          EPH1(13): NOT USED
CC                          EPH1(14): NOT USED
CC                          EPH1(15): NOT USED
CC                          EPH1(16): NOT USED
CC                          EPH1(17): NOT USED
CC                          EPH1(18): NOT USED
CC                          EPH1(19): NOT USED
CC                          EPH1(20): NOT USED
CC                          EPH1(21): MJD OF THE NEXT EPHEMERIDE
CC                              :        :
CC               EPH2   : EPHEMERIDES INFORMATION (SECOND SET) R*8(20)
CC                          (SEE ARRAY EPH1)
CC        OUT :  STATUS : RESULT OF THE CONSISTENCY CHECK     CH*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER, H.HABRICH
CC
CC VERSION    :  4.1  (MAR 97)
CC
CC CREATED    :  25-MAR-97
CC
CC CHANGES    :  21-JUN-99 : TS: ACTIVATE DNODE TEST
CC               21-AUG-01 : DI: CHANGE CRITERIA FOR E TEST
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
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
      REAL*8    A1    , A2    , DA    , DE    , DI    , DNODE , DT    ,
     1          E1    , E2    , HOURS , PER1  , PER2  , T01   , T02   ,
     2          T0E1  , T0E2  , XI1   , XI2   , XNOD21, XNODE1, XNODE2
C
CCC       IMPLICIT REAL*8(A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
      REAL*8      EPH1(20),EPH2(20)
      CHARACTER*8 STATUS
C
C
C INITIALIZE STATUS
C -----------------
      STATUS=' '
C
C FIRST SET OF ELEMENTS
C ---------------------
      T0E1  =EPH1(1)
      A1    =EPH1(3)
      E1    =EPH1(4)
      XI1   =EPH1(5)*180.D0/PI
      XNODE1=EPH1(6)*180.D0/PI
      PER1  =EPH1(7)*180.D0/PI
      IF(PER1.LT.-180.D0) PER1=PER1+360.D0
      IF(PER1.GT.+180.D0) PER1=PER1-360.D0
      T01   =EPH1(8)
C
C SECOND SET OF ELEMENTS
C ----------------------
      T0E2  =EPH2(1)
      A2    =EPH2(3)
      E2    =EPH2(4)
      XI2   =EPH2(5)*180.D0/PI
      XNODE2=EPH2(6)*180.D0/PI
      PER2  =EPH2(7)*180.D0/PI
      IF(PER2.LT.-180.D0) PER2=PER2+360.D0
      IF(PER2.GT.+180.D0) PER2=PER2-360.D0
      T02   =EPH2(8)
C
C COMPUTE CHANGE IN FIRST ELEMENTS IN THE TIME INTERVAL (T0E1,T0E2)
C -----------------------------------------------------------------
      DT=T0E2-T0E1
      HOURS=DT*24.D0
ccc   XI1=XI1+XIDOT1*DT*180.D0/PI
ccc   XNODE1=XNODE1+XNDOT1*DT*180.D0/PI
ccc   XN=DSQRT(GM/A1**3)+DN1
ccc   XM01=XM01+XN*DT*180.D0/PI
ccc   IF(XM01.LE.-180.D0) XM01=XM01+360.D0
ccc   IF(XM01.GT.+180.D0) XM01=XM01-360.D0
C
C CHECK DIFFERENCE IN A
C ---------------------
      DA=5000.D0+100.D0*HOURS
      IF(DABS(A2-A1).GT.DA) THEN
        STATUS='BAD DA'
        GOTO 100
      ENDIF
C
C CHECK DIFFERENCE IN E
C ---------------------
ccc   DE=1.D-4+0.5D-5*HOURS
      DE=2.D-4+0.25D-4*HOURS
      IF(DABS(E2-E1).GT.DE) THEN
        STATUS='BAD DE'
        GOTO 100
      ENDIF
C
C CHECK DIFFERENCE IN I
C ---------------------
      DI=0.005D0+0.003D0*HOURS
      IF(DABS(XI2-XI1).GT.DI) THEN
        STATUS='BAD DI'
        GOTO 100
      ENDIF
C
C CHECK DIFFERENCE IN NODE
C ------------------------
      XNOD21=DABS(XNODE2-XNODE1)
      IF (XNOD21.GT.100.D0) XNOD21=DABS(XNOD21-360.D0)
      DNODE=0.002D0+0.003D0*HOURS
      IF(XNOD21.GT.DNODE) THEN
        STATUS='BAD DNOD'
        GOTO 100
      ENDIF
C
C CHECK DIFFERENCE IN PERIGEE
C ---------------------------
ccc   PER21=DABS(PER2-PER1)
ccc   IF (PER21.GT.100.D0) PER21=DABS(PER21-360.D0)
ccc   DPER=7.0D1+1.0D0*HOURS
ccc   IF (EPH1(4).LT.0.001) DPER=60.0D0+0.01D0*HOURS
ccc   IF(PER21.GT.DPER) THEN
ccc     STATUS='BAD DPER'
ccc     GOTO 100
ccc   ENDIF
C
C CHECK DIFFERENCE IN MEAN ANOMALY
C --------------------------------
ccc   XM021=DABS(XM02-XM01)
ccc   IF(XM021.GT.100.D0) XM021=DABS(XM021-360.D0)
ccc   DM0=2.00D0+0.08D0*HOURS
ccc   IF (EPH1(4).LT.0.001) DM0=10.0D0+0.01D0*HOURS
ccc   IF(XM021.GT.DM0) THEN
ccc     STATUS='BAD DM0'
ccc     GOTO 100
ccc   ENDIF
C
C END
C ---
100   CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
