      MODULE s_PDANT
      CONTAINS

C*
      SUBROUTINE PDANT(IORSYS,ICOR,TOBS,TUV,XVSAT,NDIFF,DERANT)
CC
CC NAME       :  PDANT
CC
CC PURPOSE    :  COMPUTE PARTIALS WITH RESPECT TO SATELLITE ANTENNA
CC               OFFSET PARAMETERS
CC
CC PARAMETERS :
CC         IN :  IORSYS : ORBIT SYSTEM                          I*4
CC                        =1: B1950.0
CC                        =2: J2000.0
CC               ICOR   : COORDINATE (X=1,Y=2,Z=3)              I*4
CC               TOBS   : OBSERVATION TIME                      R*8
CC               TUV(I,J),I=1,2,3,J=1,2: UNIT VECTORS STATION I R*8
CC                        TO SATELLITE
CC               XVSAT(I),I=1,2,3: POSITION OF SATELLITE        R*8
CC                        IN SYSTEM OF EPOCH
CC               NDIFF  : DIFFERENCE TYPE                       I*4
CC                        NDIFF=0: ZERO DIFFERENCE
CC                        NDIFF=1: SINGLE DIFFERENCE
CC       OUT  :  DERANT : RESULTING PARTIAL DERIVATIVE          R*8
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  93/04/03
CC
CC CHANGES    :  06-JUN-96 : MR: REMOVED UNUSED VARIABLES
CC               26-NOV-98 : SS: CONSIDER "NDIFF"
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               30-MAY-07 : AG: USE s_suneff
CC               01-OCT-10 : CR: NEW CALL OF SUNEFF
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1993     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE s_cootra
      USE s_vprod
      USE s_suneff
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 ICOR  , IDERIV, IORSYS, K     , NDIFF
C
      REAL*8    DERANT, HELP  , REY   , RSAT  , SZ    , TDT   , TOBS  ,
     1          TOLD  , UT1GPS, XPOL  , YPOL
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      REAL*8 TUV(3,2),XVSAT(*),XSUN(4),EDER(3),EY(3),EZ(3),DUM3(3)
C
      DATA TOLD/0.D0/
C
      RSAT=DSQRT(XVSAT(1)**2+XVSAT(2)**2+XVSAT(3)**2)
      DO 10 K=1,3
        EZ(K)=-XVSAT(K)/RSAT
10    CONTINUE
C
      IF (ICOR.LE.2 .AND. TOBS.NE.TOLD) THEN
C
C SUN (TIME ARGUMENT: TDB, APPROX. AS TDT)
        TDT=TOBS+(19.D0+32.184D0)/86400.D0
        CALL SUNEFF(IORSYS,2.D0,TDT,XSUN,DUM3)
        IDERIV=0
        CALL COOTRA(IORSYS,IDERIV,TOBS,XSUN,SZ,XPOL,YPOL,UT1GPS)
        TOLD=TOBS
      ENDIF
C
C UNIT VECTORS
C ------------
      IF (ICOR.EQ.1) THEN
        CALL VPROD(EZ,XSUN,EY)
        REY=DSQRT(EY(1)**2+EY(2)**2+EY(3)**2)
        DO 20 K=1,3
          EY(K)=EY(K)/REY
20      CONTINUE
        CALL VPROD(EY,EZ,EDER)
      ELSEIF (ICOR.EQ.2) THEN
        CALL VPROD(EZ,XSUN,EY)
        REY=DSQRT(EY(1)**2+EY(2)**2+EY(3)**2)
        DO 30 K=1,3
          EDER(K)=EY(K)/REY
30      CONTINUE
      ELSE
        DO 40 K=1,3
          EDER(K)=EZ(K)
40      CONTINUE
      ENDIF
C
C PARTIAL DERIVATIVE
C ------------------
      HELP=0.D0
      DO 50 K=1,3
        HELP=HELP+TUV(K,1)*EDER(K)
        IF (NDIFF.EQ.1) HELP=HELP-TUV(K,2)*EDER(K)
50    CONTINUE
      DERANT=HELP
C
      RETURN
      END SUBROUTINE

      END MODULE
