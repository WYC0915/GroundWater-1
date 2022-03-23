      MODULE s_SUBVAL
      CONTAINS

C*
      SUBROUTINE SUBVAL(XTDB,SUBFAR,IARG,ARG,ARGR,PERIOD)
CC
CC NAME       :  SUBVAL
CC
CC PURPOSE    :  COMPUTATION OF THE SUBDAILY ARGUMENT FOR EPOCH "XTDB"
CC               AS THE SUM OF THE FUNDAMENTAL ARGUMENTS WITH THE
CC               MULTIPLIERS "IARG". IN ADDITION THE RATE OF CHANGE OF
CC               ARGUMENT AND THE EXACT PERIOD IS RETURNED.
CC               (JUL. DATE IN BARYCENTRIC DYNAMICAL TIME)
CC
CC PARAMETERS :
CC         IN :  XTDB   : EPOCH IN BARYCENTRIC DYNAMICAL TIME  R*8
CC                        MODIFIED JULIAN DATE
CC               SUBFAR(I,J),I=1,..,6,J=1,..,6: COEFFICIENTS    R*8
CC                        TO COMPUTE FUNDAMENTAL ARGUMENTS
CC                        I=1,..5: TERMS WITH DT**(I-1) IN
CC                                 ARCSEC PER CENTURY
CC                        I=6    : NUMBER OF FULL REVOLUTIONS
CC                                 CORRESPOINDING TO 1296000"
CC                        J= 1: L = MEAN ANOMALY OF THE MOON
CC                        J= 2: L'= MEAN ANOMALY OF THE SUN
CC                        J= 3: F = MEAN LONGITUDE OF THE MOON - O
CC                        J= 4: D = MEAN LONGITUDE OF THE MOON - MEAN
CC                                  LONGITUDE OF THE SUN
CC                        J= 5: O = MEAN LONGITUDE OF THE NODE OF THE
CC                                  MOON
CC                        J= 6: TP= GREENWICH MEAN SIDEREAL TIME + PI
CC               IARG(I),I=1,..,6: MULTIPLIERS OF THE          I*4
CC                        FUNDAMENTAL ARGUMENTS (L,L',F,D,O,TP)
CC        OUT :  ARG    : ARGUMENT AT TIME "XTDB"" (RADIAN)    R*8
CC               ARGR   : RATE OF ARGUMENT AT "XTDB" (RAD/DAY) R*8
CC               PERIOD : PERIOD (JULIAN DAYS)                 R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M. ROTHACHER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  26-FEB-98
CC
CC CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               04-JUL-05 : HU: MODIFIED COMPUTATION OF FUNDAMENTAL
CC                               ARGUMENTS DUE TO NUMERICAL REASONS
CC               28-FEB-07 : AG: USE PI FROM DEFCON
CC               04-MAY-12 : RD: USE DMOD FROM MODULE
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1998     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE d_const,  ONLY: pi
      USE l_basfun, ONLY: dmod
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IA    , IT
C
      REAL*8    ARG   , ARGR  , PERIOD, R     , ROH   , TU    ,
     1          XTDB
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      REAL*8    ARGFUN(6),ARGRAT(6),SUBFAR(6,6)
      REAL*8    TCENT(5)
C
      INTEGER*4 IARG(6)
C
C DATA
C ----
C      DATA PI/3.141592653589793D0/,R/1296000.D0/
      DATA R/1296000.D0/
C
      ROH = PI/648000.D0
C
C TIME INTERVAL (IN JUL. CENTURIES) BETWEEN XTDB AND J2000.0
C ----------------------------------------------------------
      TU =(XTDB-51544.5D0)/36525.D0
      TCENT(1)=1.D0
      TCENT(2)=TU
      TCENT(3)=TU*TU
      TCENT(4)=TU*TU*TU
      TCENT(5)=TU*TU*TU*TU
C
C FUNDAMENTAL ARGUMENTS (L,L',F,D,O,TP) IN RADIAN
C -----------------------------------------------
      DO IA=1,6
C
C ARGUMENT (ARCSEC)
        ARGFUN(IA)=0.D0
        DO IT=5,1,-1
          ARGFUN(IA)=ARGFUN(IA)*TU+SUBFAR(IT,IA)
        ENDDO
        ARGFUN(IA)=ARGFUN(IA)+SUBFAR(6,IA)*R*TU
C
C ARGUMENT RATE (ARCSEC PER CENTURY)
        ARGRAT(IA)=0.D0
        DO IT=5,2,-1
          ARGRAT(IA)=ARGRAT(IA)
     1               +SUBFAR(IT,IA)*(IT-1)*TCENT(IT-1)
        ENDDO
        ARGRAT(IA)=ARGRAT(IA)+SUBFAR(6,IA)*R
C
C CONVERSION TO RADIAN, RADIAN/DAY
        ARGFUN(IA)=DMOD(ARGFUN(IA)*ROH,2.D0*PI)
        ARGRAT(IA)=     ARGRAT(IA)*ROH/36525.D0
      ENDDO
C
C COMPUTE SUM OF ARGUMENTS WITH MULTIPLIERS "IARG"
C ------------------------------------------------
      ARG =0.D0
      ARGR=0.D0
      DO IA=1,6
        ARG =ARG +IARG(IA)*ARGFUN(IA)
        ARGR=ARGR+IARG(IA)*ARGRAT(IA)
      ENDDO
      ARG=DMOD(ARG,2.D0*PI)
C
C PERIOD IN TDB DAYS
C ------------------
      IF (ARGR.NE.0.D0) THEN
        PERIOD=DABS(2.D0*PI/ARGR)
      ELSE
        PERIOD=0.D0
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
