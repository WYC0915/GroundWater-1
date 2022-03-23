      MODULE s_COOTRA
      CONTAINS

C*
      SUBROUTINE COOTRA(IORSYS,INDDER,TOBS,XV,SZ,XPOL,YPOL,UT1GPS)
CC
CC NAME       :  COOTRA
CC
CC PURPOSE    :  TRANSFORM COORDINATES, VELOCITIES, ACCELERATION, ...
CC               FROM SYSTEM 1950.0 INTO SYSTEM OF DATE.
CC               IN ADDITION TRUE SIDERAL TIME, POLE COORDINATES
CC               ARE RETURNED.
CC
CC PARAMETERS :
CC         IN :  IORSYS : ORBIT SYSTEM                        I*4
CC                        =1: B1950.0
CC                        =2: J2000.0
CC               INDDER : HIGHEST DERIVATIVE TO BE COMPUTED   I*4
CC                    =0: POSITION ONLY
CC                    =1: POSITION AND VELOCITY
CC                    =2:     ..    ..    ..     AND ACCELERATION
CC               TOBS   : TIME (GPS-TIME, MJD)                R*8
CC     IN/OUT :  XV(K),K=1,2,3,4,...: POSITION, VEL., ETC     R*8
CC                        BEFORE AND AFTER TRANSFORMATION
CC        OUT :  SZ     : TRUE SIDERAL TIME                   R*8
CC               XPOL   : POLE COORDINATES                    R*8
CC               YPOL   :    (RADIANS)                        R*8
CC               UT1GPS : UT1-GPS  (DAYS)                     R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/10/16 08:00
CC
CC CHANGES    :  05-JUN-92 : ??: NEW PARAMETER "IORSYS" FOR J2000.0
CC                               NEW CALLS: "PRCEFF", "NUTEFF",
CC               05-JUN-96 : TS: CALL POLDEF CHANGED DUE TO SUBDAILY POLE
CC               11-JUN-03 : HU: USE GSTIME
CC               14-JUN-03 : HU: DESACTIVATE USE OF GMST2000
CC               06-AUG-03 : HU: NEW CALL FOR GSTIME
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               07-JUL-05 : HB: USE T_EPOCH FOR GSTIME
CC                               ADD BIAS TO PARAMETER LIST OF SR NUTEFF
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_epoch, ONLY : t_epoch, OPERATOR(.realToEpoch.)
      USE s_nuteff
      USE f_gstime
      USE s_poldef
      USE s_prceff
      USE s_dmlmav
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 INDDER, IORSYS, L     , L0
C
      REAL*8    EQEQUI, GPSUTC, SZ    , SZSAVE, TDT   , TOBS  ,
     1          TOLD  , UT1GPS, UT1GSV, UT1UTC, XPOL  , XPOLSV, YPOL  ,
     2          YPOLSV
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      REAL*8 PRECES(3,3),NUT(3,3),BIAS(3,3),XV(*)
C
      TYPE(t_epoch) :: TTOBS,TTDT
C
      DATA TOLD/0.D0/
C
C COMPUTE NEW ROTATION MATRICES, SIDERAL TIME FOR NEW TOBS
C --------------------------------------------------------
      IF(TOBS.NE.TOLD) THEN
        TOLD=TOBS
        CALL POLDEF(TOBS,1,XPOLSV,YPOLSV,UT1UTC,GPSUTC)
C
C PRECESSION AND NUTATION (TIME ARGUMENT: TDB, APPROX. AS TDT)
        TDT=TOBS+(19.D0+32.184D0)/86400.D0
        CALL PRCEFF(IORSYS,5.D0,TDT,PRECES)
        CALL NUTEFF(IORSYS,1.D0,TDT,NUT,EQEQUI,BIAS)
        PRECES = matmul(PRECES,BIAS)
        UT1GSV=UT1UTC-GPSUTC
        TTOBS=.realToEpoch.(TOBS+UT1GSV)
        TTDT =.realToEpoch.TDT
        SZSAVE=GSTIME(0,TTOBS,TTDT,NUT(2,1),EQEQUI)
      END IF
C
C SET VALUES FOR POLE COORDINATES, SIDERAL TIME, ...
C --------------------------------------------------
      XPOL=XPOLSV
      YPOL=YPOLSV
      UT1GPS=UT1GSV
      SZ=SZSAVE
C
C TRANSFORMATION INTO SYSTEM OF EPOCH
C -----------------------------------
      DO 20 L=1,INDDER+1
        L0=3*(L-1)
        CALL DMLMAV(XV(L0+1),PRECES,XV(L0+1))
        CALL DMLMAV(XV(L0+1),NUT,XV(L0+1))
20    CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
