      MODULE s_POLARLEO
      CONTAINS

C*
      SUBROUTINE POLARLEO(TOBS,IPOLAR,SENSOR,XVLEO,XVSAT,DPOLAR)
CC
CC NAME       :  POLARI
CC
CC PURPOSE    :  COMPUTE THE EFFECT OF ANTENNA ORIENTATION
CC               (RIGHT CIRCULARLY POLARIZED WAVE)
CC
CC
CC PARAMETERS :
CC         IN :  TOBS     : TIME OF OBSERVATION (MJD)         R*8
CC               IPOLAR   : POLARIZATION EFFECT               I*4
CC                          =0: NONE
CC                          =1: ONLY GEOMETRICAL PART
CC                          =2: FULL, I.E. INCL GNSS ATTITUDE
CC               XVLEO(K) : SAT. POSITION, VELOCITY           R*8
CC                          (TRUE SYSTEM OF EPOCH)
CC               XVSAT(K),K=1,..,6: POSITION, VELOCITY,..     R*8
CC                          OF SATELLITE  (TRUE SYSTEM)
CC        OUT :  DPOLAR : RESULTING EFFECT IN PHASE (CYCLES)  R*8
CC
CC REMARKS    :  FLATTENING OF THE EARTH ELLIPSOID IS NEGLECTED
CC
CC AUTHOR     :  L.MERVART
CC
CC VERSION    :  3.4
CC
CC CREATED    :  14-APR-93
CC
CC CHANGES    :  24-JUN-96 : MR: NUMERICAL PROBLEM WITH ARCCOS> 1.0
CC               06-OCT-97 : TS: DACOS BETWEEN +1.0 AND -1.0
CC               16-FEB-99 : MR: VECTOR "XSUN" WAS NOT DECLARED !
CC               16-SEP-03 : HU: POLARIZATION DOES NO LONGER CONTAIN
CC                               SATELLITE ATTITUDE
CC               23-JUL-05 : HU: USE MODULES, IMPLICIT NONE
CC               09-NOV-06 : HU: IPOLAR FOR HANDLING OF ATTITUDE
CC               30-MAY-07 : AG: USE s_suneff
CC               01-OCT-10 : CR: NEW CALL OF SUNEFF
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1993     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE d_const, ONLY: pi
      USE s_vprod
      USE s_leoprn
      USE s_dmlmav
      USE s_sprod
      USE s_suneff
      USE s_attitude
      USE s_gtoffuni
      IMPLICIT NONE
C
C Declarations
      INTEGER*4    LEOSVN, I     , IPOLAR, IORSYS
      REAL*8       RSAT  , VZ    , VV    , XX    , REK    , EKX    ,
     1             EKEX  , CALPHA, EDD   , RED   , RD     , DPOLAR ,
     2             TOBS  , SIG   , DUMMY1, DUMMY2
ccc      IMPLICIT REAL*8 (A-H,O-Z)
ccc      IMPLICIT INTEGER*4 (I-N)
C
      REAL*8       XVSAT(*),XVLEO(3),XSUN(4),DUM3(3)
      REAL*8       EX(3),EY(3),EZ(3),XELL(3),LOCECC(3),X(3),Y(3),Z(3)
      REAL*8       EK(3),D(3),ED(3),EKY(3),EKEY(3),EDDVEC(3)
C
      CHARACTER(LEN=staNameLength)  :: sensor
      REAL(r8b),DIMENSION(3,3) :: attit
      REAL(r8b),DIMENSION(3)   :: azim,bore,antoff
C
      DPOLAR=0D0
      IF (IPOLAR.EQ.0) RETURN
C
C Get LEO number
C --------------
      CALL LEOPRN(sensor,tobs,leosvn)
C
C Get sensor offsets and sensor unit vector
C -----------------------------------------
      CALL GTOFFUNI(1,(/leosvn/),(/sensor/),tobs,antoff,bore,azim)
C
C Get attitude rotation matrix
C ----------------------------
      CALL ATTITUDE(leosvn,tobs,xvleo,0,attit)
C
C Antenna vectors in true system
C ------------------------------
      CALL DMLMAV(azim,attit,X)
      CALL DMLMAV(bore,attit,Z)
      CALL VPROD(Z,X,Y)
!!!         write(*,*)tobs,x,z
C
C COMPUTE THE SATELLITE SPECIFIC UNIT VECTORS IN TRUE SYSTEM
C ----------------------------------------------------------
C UNIT VECTOR EZ
      RSAT=DSQRT(XVSAT(1)**2+XVSAT(2)**2+XVSAT(3)**2)
      DO 100 I=1,3
        EZ(I)=-XVSAT(I)/RSAT
100   CONTINUE
C
C ..PHASE WINDUP WITHOUT GNSS SATELLITE ATTITUDE INCLUDED
      IF (IPOLAR.EQ.1) THEN
C
C UNIT VECTOR EX
        VZ=DOT_PRODUCT(XVSAT(4:6),EZ(1:3))
        VV=DOT_PRODUCT(XVSAT(4:6),XVSAT(4:6))
        XX=DSQRT(VV-VZ**2)
        EX(1:3)=(XVSAT(4:6)-VZ*EZ(1:3))/XX
C
C UNIT VECTOR EY
        CALL VPROD(EZ,EX,EY)
C
C ..PHASE WINDUP INCLUDING GNSS SATELLITE ATTITUDE
      ELSE
C
C UNIT VECTOR EY
        IORSYS=2
        CALL SUNEFF(IORSYS,2.D0,TOBS,XSUN,DUM3)
        CALL VPROD(EZ,XSUN,EY)
        REK=DSQRT(EY(1)**2+EY(2)**2+EY(3)**2)
        DO I=1,3
          EY(I)=EY(I)/REK
        ENDDO
C
C UNIT VECTOR EX
        CALL VPROD(EY,EZ,EX)
      ENDIF
C
C UNIT VECTOR EK (SATELLITE-->RECEIVER)
      REK=0.D0
      DO 300 I=1,3
        EK(I) = XVLEO(I) - XVSAT(I)
        REK   = REK + EK(I)**2
300   CONTINUE
      REK = DSQRT(REK)
      DO 400 I=1,3
        EK(I) = EK(I) / REK
400   CONTINUE
C
C COMPUTE THE EFFECTIVE DIPOLE OF THE RECEIVER ANTENNA
C ----------------------------------------------------
      CALL SPROD(EK,X,EKX,DUMMY1,DUMMY2)
      CALL VPROD(EK,Y,EKY)
      DO 500 I=1,3
        D(I) = X(I) - EK(I)*EKX + EKY(I)
500   CONTINUE
C
C COMPUTE THE EFFECTIVE DIPOLE OF THE SATELLITE ANTENNA
C -----------------------------------------------------
      CALL SPROD(EK,EX,EKEX,DUMMY1,DUMMY2)
      CALL VPROD(EK,EY,EKEY)
      DO 600 I=1,3
        ED(I) = EX(I) - EK(I)*EKEX - EKEY(I)
600   CONTINUE
C
C COMPUTE THE RESULTING EFFECT
C ----------------------------
      CALL SPROD(ED,D,EDD,RED,RD)
      CALL VPROD(ED,D,EDDVEC)
      CALL SPROD(EK,EDDVEC,SIG,DUMMY1,DUMMY2)
C
      CALPHA=EDD/RED/RD
      IF (CALPHA.GT.1.D0) CALPHA=1.D0
      IF (CALPHA.LT.-1.D0) CALPHA=-1.D0
      DPOLAR = DSIGN(1.D0,SIG)*DACOS(CALPHA)/2/PI
C
      RETURN
      END SUBROUTINE

      END MODULE
