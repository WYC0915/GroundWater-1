      MODULE s_CHKNTD
      CONTAINS

C*
      SUBROUTINE CHKNTD(TOBS,WGSEPO,IORSYS,IRCNTD)
CC
CC NAME       :  CHKNTD
CC
CC PURPOSE    :  CHECK WHETHER NIGHT-TIME DATA IS PROCESSED OR NOT
CC
CC PARAMETERS :
CC         IN :  TOBS   : OBSERVATION TIME IN MJD             R*8
CC               WGSEPO : STATION COORDINATES IN M            R*8(3)
CC               IORSYS : ORBIT SYSTEM                        I*4
CC                        =1: B1950.0
CC                        =2: J2000.0
CC        OUT :  IRCNTD : RETURN CODE                         I*4
CC                        =0: IF DAY  -TIME DATA IS PROCESSED
CC                        =1: IF NIGHT-TIME DATA IS PROCESSED
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S.SCHAER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  22-OCT-97
CC
CC CHANGES    :  24-OCT-97 : SS: CALL OF SR SUNEFF
CC               28-OCT-97 : SS: SKIP FIRST SEQUENCE IF POSSIBLE
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               30-MAY-07 : AG: USE s_suneff
CC               01-OCT-10 : CR: NEW CALL OF SUNEFF
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE d_const, ONLY: PI
      USE s_sprod
      USE s_cootra
      USE s_suneff
      USE s_ddreh
      USE s_dmlmav
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IORSYS, IRCNTD
C
      REAL*8    ALPHA , R1    , R2    , SCAL  , SZ    , TOBS  , TOBS0 ,
     1          UT1GPS, XPOL  , YPOL
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      REAL*8 WGSEPO(3),SUNPOS(4),ROTMAT(3,3),DUM3(3)
C
C
      DATA TOBS0/0.D0/
C
C COMPUTE EARTH-FIXED VECTOR OF THE SUN
C -------------------------------------
      IF (TOBS.NE.TOBS0) THEN
        CALL SUNEFF(IORSYS,2.D0,TOBS,SUNPOS,DUM3)
        CALL COOTRA(IORSYS,0,TOBS,SUNPOS,SZ,XPOL,YPOL,UT1GPS)
C
        CALL DDREH(3,SZ,ROTMAT)
        CALL DMLMAV(SUNPOS,ROTMAT,SUNPOS)
C
        TOBS0=TOBS
      ENDIF
C
C COMPUTE GEOCENTRIC ANGLE BETWEEN STATION AND THE SUN
C ----------------------------------------------------
      CALL SPROD(WGSEPO,SUNPOS,SCAL,R1,R2)
C
      SCAL=SCAL/(R1*R2)
      IF (DABS(SCAL).GT.1.D0) SCAL=SCAL/DABS(SCAL)
      ALPHA=DACOS(SCAL)
C
C SET RETURN CODE
C ---------------
      IF (ALPHA.GT.PI/2.D0) THEN
        IRCNTD=1
      ELSE
        IRCNTD=0
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
