      MODULE s_nutval
      CONTAINS
C*
      SUBROUTINE NUTVAL(XTDB,NUTFAR,IARG,ARG,ARGR,PERIOD)
CC
CC NAME       :  NUTVAL
CC
CC PURPOSE    :  COMPUTATION OF THE NUTATION ARGUMENT FOR EPOCH "XTDB"
CC               AS THE SUM OF THE FUNDAMENTAL ARGUMENTS WITH THE
CC               MULTIPLIERS "IARG". IN ADDITION THE RATE OF CHANGE OF
CC               ARGUMENT AND THE EXACT PERIOD IS RETURNED.
CC               (JUL. DATE IN BARYCENTRIC DYNAMICAL TIME)
CC
CC PARAMETERS :
CC         IN :  XTDB   : EPOCH IN BARYCENTRIC DYNAMICAL TIME  R*8
CC                        MODIFIED JULIAN DATE
CC               NUTFAR(I,J),I=1,..,6,J=1,..,14: COEFFICIENTS    R*8
CC                        TO COMPUTE FUNDAMENTAL ARGUMENTS
!                         i=1,..5: terms with dt**(i-1) in
!                                  arcsec per century etc.
!                         i=6    : number of full revolutions
!                                  corresponding to 1296000"
!                         j= 1: l = mean anomaly of the moon
!                         j= 2: l'= mean anomaly of the sun
!                         j= 3: f = mean longitude of the
!                                   moon - o
!                         j= 4: d = mean longitude of the
!                                   moon - mean longitude
!                                   of the sun
!                         j= 5: o = mean longitude of
!                                   the node of the moon
!                         j= 6: lq= mean longitude of merkur
!                         j= 7: lv= mean longitude of venus
!                         j= 8: le= mean longitude of earth
!                         j= 9: lm= mean longitude of mars
!                         j=10: lj= mean longitude of jupiter
!                         j=11: ls= mean longitude of saturn
!                         j=12: lu= mean longitude of uranus
!                         j=13: ln= mean longitude of neptun
!                         j=14: pa= mean longitude of general
!                                   precession in longitude
CC               IARG(I),I=1,..,14: MULTIPLIERS OF THE         I*4
CC                        FUNDAMENTAL ARGUMENTS
CC
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
CC CREATED    :  10-NOV-97
CC
CC CHANGES    :  12-AUG-03 : PS: SWITCH TO NEW NUTATION MODEL FORMAT
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-FEB-07 : AG: USE PI FROM DEFCON
CC               21-OCT-08 : HB: MAKE NUTVAL A MODULE
CC               04-MAY-12 : RD: USE DMOD FROM MODULE
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
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
      REAL*8    ARGFUN(14),ARGRAT(14),NUTFAR(6,*)
      REAL*8    TCENT(5)
C
      INTEGER*4 IARG(*)
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
      TU =(XTDB-51544.5)/36525.D0
      TCENT(1)=1.D0
      TCENT(2)=TU
      TCENT(3)=TU*TU
      TCENT(4)=TU*TU*TU
      TCENT(5)=TU*TU*TU*TU
C
! FUNDAMENTAL ARGUMENTS (L,L',F,D,O,LV,LE,LM,LJ,LS,PA) IN RADIAN
C --------------------------------------------------------------
      DO IA=1,14
C
C ARGUMENT (ARCSEC)
        ARGFUN(IA)=0.D0
        DO IT=5,1,-1
          ARGFUN(IA)=ARGFUN(IA)+NUTFAR(IT,IA)*TCENT(IT)
        ENDDO
        ARGFUN(IA)=ARGFUN(IA)+NUTFAR(6,IA)*R*TU
C
C ARGUMENT RATE (ARCSEC PER CENTURY)
        ARGRAT(IA)=0.D0
        DO IT=5,2,-1
          ARGRAT(IA)=ARGRAT(IA)
     1               +NUTFAR(IT,IA)*(IT-1)*TCENT(IT-1)
        ENDDO
        ARGRAT(IA)=ARGRAT(IA)+NUTFAR(6,IA)*R
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
      DO IA=1,11
        ARG =ARG +IARG(IA)*ARGFUN(IA)
        ARGR=ARGR+IARG(IA)*ARGRAT(IA)
      ENDDO
      ARG=DMOD(ARG,2.D0*PI)
C
C PERIOD IN TDB DAYS
C ------------------
      IF (ARGR.NE.0.D0) THEN
        PERIOD=2.D0*PI/ARGR
      ELSE
        PERIOD=0.D0
      ENDIF
C
      RETURN
      END SUBROUTINE
      END MODULE
