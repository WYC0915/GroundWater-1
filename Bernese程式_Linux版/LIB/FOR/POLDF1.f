      MODULE s_POLDF1
      CONTAINS

C*
      SUBROUTINE POLDF1(FILNAM,TIME,ISUBFL,XR,YR,DUTFRD,DUTGPS)
CC
CC NAME       :  POLDF1
CC
CC PURPOSE    :  DEFINITION OF POLAR COORDINATES XR,YR (RADIAN),
CC               UT1-UTC (DAYFRACTION), GPS-UTC(DAYFRACTION)
CC               USING AN EXTERNAL FILE. BASED ON LATEST (1996)
CC               VERSION OF POLDEF
CC               ONLY DIFFERENCE BETWEEN POLDEF AND POLDF1:
CC               EXTERNAL FILE NAME
CC
CC PARAMETERS :
CC         IN :  FILNAM : NAME OF POLE FILE                   CH*32
CC               TIME   : TIME OF REQUEST IN MJD (IN UTC)     R*8
CC               ISUBFL : SUBDAILY MODEL INDICATION FLAG      I*4
CC                        =0 DO NOT APPLY SUBDAILY MODEL
CC                        =1 DO APPLY SUBDAILY MODEL
CC        OUT :  XR     : POLE X-COORDINATE (RADIAN)          R*8
CC               YR     : POLE Y-COORDINATE (RADIAN)          R*8
CC               DUTFRD : UT1-UTC FOR REQUESTED TIME (DAYS)   R*8
CC               DUTGPS : GPS-UTC (DAYS)                      R*8
CC
CC REMARKS    :  23-JUN-92 : ACTUALLY NO OPPOLZER CORRECTION
CC               11-JUN-93 : SR IS A SLIGHT MODIFICATION OF SR POLDF1
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER, S.FANKHAUSER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/10/29 09:53
CC
CC CHANGES    :  11-FEB-92 : SF: CHANGES DUE TO ERP-ESTIMATION (UT1RED)
CC               18-JUN-92 : SF: CORRECTED FOR OPPOLZER NUTATION
CC               23-JUN-92 : SF: UT1-UTC REQUIRED IN POLE FILE
CC               24-NOV-92 : SF: PREPARED OPPOLZER REMOVED
CC               11-JUN-93 : ??: FILNAME FROM SR PARAMETER LIST
CC               25-JUL-94 : MR: REMOVE 2 BLANKS FROM DATA FOR "FILOLD"
CC               25-APR-94 : MR: CHANGE TEST FROM "GT" TO "GE"
CC               26-APR-95 : LM: CORRECT DIMENSION FILNAM
CC               04-JUN-96 : TS: SUBDAILY POLE MODEL ADDED
CC               22-AUG-00 : HB: ADD ISUBFL IN CALL OF SR
CC               28-NOV-02 : PS: USE MODIFIED SR SUBPOL
CC               16-DEC-02 : PS: CORRECT SUBNAM
CC               14-FEB-03 : PS: CALL TO SR GETPL1 CHANGED
CC               08-MAR-03 : HU: INTERFACE FOR SUBPOL ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               29-JAN-07 : AG: EXTRAPOLATION TOLERANCE FOR LITPOL ADDED
CC               28-FEB-07 : AG: USE 206264... FROM DEFCON
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE d_const, ONLY: ars
      USE s_ut1red
      USE s_subpol
      USE s_litpol
      USE s_getpl1
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I1    , ISUBFL
C
      REAL*8    DELTA , DUTFRD, DUTGPS, TIME  , XR    , YR
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*32 FILNAM, FILOLD
C
      REAL*8    T(2),XPOLE(2),YPOLE(2),UT1UTC(2),GPSUTC(2)
C
      REAL*8 xr_hlp(1), yr_hlp(1), dutfrd_hlp(1), dutgps_hlp(1)
      INTEGER*4 POLTYP(2)
C
      DATA T/200000.D0,0.D0/,FILOLD/'                '/

C Variables for SUBPOL
      REAL*8       ERPSUB(3),ERPSUR(3)
      CHARACTER*16 SUBNAM

C
      IF(TIME.LT.T(1).OR.TIME.GE.T(2).OR.FILNAM.NE.FILOLD)THEN
        CALL GETPL1(FILNAM,TIME,T,XPOLE,YPOLE,UT1UTC,GPSUTC,POLTYP,
     1               ISUBFL)
        FILOLD=FILNAM

C
C REDUCE UT1-UTC TO UT1R-UTC FOR INTERPOLATION
        DO 10 I1=1,2
          CALL UT1RED(T(I1),DELTA)
          UT1UTC(I1)=UT1UTC(I1)-DELTA/86400000.D0
10      CONTINUE
C
      END IF
C
C INTERPOLATION OF POLE COORDINATES
C ---------------------------------
      CALL LITPOL(2,1,T,XPOLE,TIME,0d0,XR_hlp)
      xr = xr_hlp(1)
      CALL LITPOL(2,1,T,YPOLE,TIME,0d0,YR_hlp)
      yr = yr_hlp(1)
      CALL LITPOL(2,1,T,UT1UTC,TIME,0d0,DUTFRD_hlp)
      dutfrd = dutfrd_hlp(1)
      CALL LITPOL(2,1,T,GPSUTC,TIME,0d0,DUTGPS_hlp)
      dutgps = dutgps_hlp(1)
C
C ADD SUBDAILY POLE MODEL TO POLE VALUES
C --------------------------------------
      IF (ISUBFL.EQ.1) THEN
        CALL SUBPOL(TIME,SUBNAM,ERPSUB,ERPSUR)
        XR=XR+ERPSUB(1)/ars
        YR=YR+ERPSUB(2)/ars
        DUTFRD=DUTFRD+ERPSUB(3)/86400.D0
      ENDIF
C
C CORRECT INTERPOLATED UT1R-UTC TO UT1-UTC
C ----------------------------------------
      CALL UT1RED(TIME,DELTA)
      DUTFRD=DUTFRD+DELTA/86400000.D0
C
      RETURN
      END SUBROUTINE

      END MODULE
