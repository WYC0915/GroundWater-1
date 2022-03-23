      MODULE s_TRPMAP
      CONTAINS

C*
      SUBROUTINE TRPMAP(ITRMAP,TOBS,XSTELL,ZEN,MAPFUN)
CC
CC NAME       :  TRPMAP
CC
CC PURPOSE    :  ROUTINE TO COMPUTE VARIOUS TROPOSPHERIC MAPPING
CC               FUNCTIONS AND THEIR DERIVATIVES WITH RESPECT TO
CC               THE ZENITH DISTANCE.
CC
CC PARAMETERS :
CC         IN :  ITRMAP : MAPPING FUNCTION FOR TROPOSP.EST.   I*4
CC                        =1: 1/COS(Z)
CC                        =2: HOPFIELD
CC                        =3: DRY NIELL
CC                        =4: WET NIELL
CC                        =5: DRY GMF
CC                        =6: WET GMF
CC                        =7: DRY VMF
CC                        =8: WET VMF
CC               TOBS   : OBSERVATION EPOCH IN MJD            R*8
CC               XSTELL(I),I=1..3: ELLIPSOIDAL STATION        R*8(3)
CC                        COORDINATES (LAT/LON IN RAD, HEIGHT
CC                        IN M)
CC               ZEN    : ZENITH DISTANCE IN RAD              R*8
CC        OUT :  MAPFUN : TROPOSPHERIC MAPPING FUNCTION       R*8(2)
CC                        (1): MAPPING FACTOR
CC                        (2): DERIVATIVE WITH RESPECT TO
CC                             ZENITH DISTANCE (IN 1/RAD)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S.SCHAER, M.ROTHACHER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  29-MAR-97
CC
CC CHANGES    :  15-APR-97 : SS: DO NOT CHECK ELEVATION ANGLE
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               22-AUG-06 : HU: DRY AND WET GMF IMPLEMENTED
CC               30-JUN-08 : RD: VMF ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE d_const, ONLY: PI
      USE d_grid,  ONLY: getGrid,grd_vmf1_ah,grd_vmf1_aw
      USE s_nmfwet
      USE s_nmfdry
      USE s_gmf
      USE s_vmf1_ht
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 ITRMAP
C
      REAL*8    TOBS  , XELE  , XELE1 , ZEN   , ZEN1
      REAL*8    AH    , AW
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      REAL*8 XSTELL(3),MAPFUN(2),FUN(2)
C
C
C 1/COS(Z)
C --------
      IF (ITRMAP.EQ.1) THEN
        MAPFUN(1)=1.D0/DCOS(ZEN)
        MAPFUN(2)=DTAN(ZEN)/DCOS(ZEN)
C
C HOPFIELD
C --------
      ELSEIF (ITRMAP.EQ.2) THEN
        XELE=PI/2.D0-ZEN
        XELE1=DSQRT(XELE**2+0.0006853891945D0)
        ZEN1=PI/2.D0-XELE1
        MAPFUN(1)=1.D0/DCOS(ZEN1)
        MAPFUN(2)=DTAN(ZEN1)/DCOS(ZEN1)*XELE/XELE1
C
C DRY NIELL
C ---------
      ELSEIF (ITRMAP.EQ.3) THEN
        CALL NMFDRY(TOBS,XSTELL(1),XSTELL(3),ZEN,MAPFUN)
C
C WET NIELL
C ---------
      ELSEIF (ITRMAP.EQ.4) THEN
        CALL NMFWET(XSTELL(1),ZEN,MAPFUN)
C
C DRY GMF
C -------
      ELSEIF (ITRMAP.EQ.5) THEN
        CALL GMF (TOBS,XSTELL(1),XSTELL(2),XSTELL(3),ZEN,MAPFUN,FUN)
C
C WET GMF
C -------
      ELSEIF (ITRMAP.EQ.6) THEN
        CALL GMF (TOBS,XSTELL(1),XSTELL(2),XSTELL(3),ZEN,FUN,MAPFUN)
C
C DRY VMF
C -------
      ELSEIF (ITRMAP.EQ.7) THEN
        ah = getGrid (grd_vmf1_ah, tobs, xstell)
        CALL VMF1_HT (AH,0D0,TOBS,XSTELL(1),XSTELL(3),ZEN,MAPFUN,FUN)
C
C WET VMF
C -------
      ELSEIF (ITRMAP.EQ.8) THEN
        aw = getGrid (grd_vmf1_aw, tobs, xstell)
        CALL VMF1_HT (0D0,AW,TOBS,XSTELL(1),XSTELL(3),ZEN,FUN,MAPFUN)
C
C STOP, IF MAPPING FUNCTION NOT FOUND
C -----------------------------------
      ELSE
        WRITE(LFNERR,910) ITRMAP
910     FORMAT(/,' *** SR TRPMAP: INVALID TROPOSPHERIC MAPPING ',
     1    'FUNCTION',/,
     2    16X,'MAPPING FUNCTION:',I3,/)
        CALL EXITRC(2)
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
