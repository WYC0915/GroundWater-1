      MODULE s_NUTMOD
      CONTAINS

C*
      SUBROUTINE NUTMOD(XTDB  ,NUTPRE,NUTFAR,NNUT  ,NUTMLT,NUTCOE,
     1                  DEPS  ,DPSI  ,DEPSR ,DPSIR)
CC
CC NAME       :  NUTMOD
CC
CC PURPOSE    :  COMPUTE NUTATION IN LONGITUDE (DEPS) AND OBLIQUITY
CC               DPSI) USING THE COEFFICENTS "NUTCOE"WRITE NUTATION MODEL FILE
CC
CC PARAMETERS :
CC        IN  :  XTDB   : EPOCH IN MOD. JULIAN DATE (TDB)        R*8
CC               NUTPRE(I),I=1,2: CORRECTIONS TO PRECESSION      R*8
CC                        RATE IN ARCSEC PER CENTURY
CC                        I=1: LONGITUDE
CC                        I=2: OBLIQUITY
CC               NUTFAR(I,J),I=1,..,6,J=1,..,11: COEFFICIENTS    R*8
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
CC                        J= 6: LV= MEAN LONGITUDE OF VENUS
CC                        J= 7: LE= MEAN LONGITUDE OF EARTH
CC                        J= 8: LM= MEAN LONGITUDE OF MARS
CC                        J= 9: LJ= MEAN LONGITUDE OF JUPITER
CC                        J=10: LS= MEAN LONGITUDE OF SATURN
CC                        J=11: PA= MEAN LONGITUDE OF GENERAL
CC                                  PRECESSION IN LONGITUDE
CC               NNUT   : NUMBER OF NUTATION PERIODS             I*4
CC               NUTMLT(J,K),J=1,..,11,K=1,..,NNUT: MULTIPLIERS  I*4
CC                        OF THE FUNDAMENTAL ARGUMENTS FOR EACH
CC                        NUTATION TERM (PERIOD)
CC               NUTCOE(L,K),L=1,..,6,K=1,..,NNUT: COEFFICIENTS  R*8
CC                        OF NUTATION MODEL IN ARCSEC
CC                        L: AI,AI',BI,BI',AI'',BI'' ACCORDING
CC                           TO THE IERS DEFINITION (IERS CON-
CC                           VENTIONS 1996, P.26)
CC        OUT :  DEPS   : NUTATION IN OBLIQUITY (ARCSEC)
CC               DPSI   : NUTATION IN LONGITUDE (ARCSEC)
CC               DEPSR  : NUTATION RATE IN OBLIQUITY (ARCSEC/DAY)
CC               DPSIR  : NUTATION RATE IN LONGITUDE (ARCSEC/DAY)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  07-NOV-97
CC
CC CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-FEB-07 : AG: VARIABLE PI REMOVED
CC               21-OCT-08 : HB: ADD USE s_nutval
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE s_nutval
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 INUT  , MAXPER, NNUT
C
      REAL*8    ARG   , ARGR  , DEPS  , DEPSR , DPSI  , DPSIR , PERIOD,
     1          TU    , XTDB
C
CCC       IMPLICIT REAL*8(A-H,O-Z)
C
      PARAMETER (MAXPER=1000)
C
      REAL*8       NUTFAR(6,14),NUTCOE(11,*),NUTPRE(2)
C
      INTEGER*4    NUTMLT(11,MAXPER)
c
C INITIALIZATION
C --------------
      TU=(XTDB-51544.5)/36525.D0
C
      DEPS =0.D0
      DPSI =0.D0
      DEPSR=0.D0
      DPSIR=0.D0
C
C PRECESSION RATE CORRECTION
C --------------------------
      DEPS =DEPS +NUTPRE(2)*TU
      DPSI =DPSI +NUTPRE(1)*TU
C
      DEPSR=DEPSR+NUTPRE(2)/36525.D0
      DPSIR=DPSIR+NUTPRE(1)/36525.D0
C
C COMPUTE DEPS AND DPSI AND THEIR RATES
C -------------------------------------
      DO INUT=NNUT,1,-1
        CALL NUTVAL(XTDB,NUTFAR,NUTMLT(1,INUT),ARG,ARGR,PERIOD)
C
        DEPS = DEPS +(NUTCOE(3,INUT)+NUTCOE(4,INUT)*TU)*DCOS(ARG)
     1              +NUTCOE(6,INUT)*DSIN(ARG)
        DPSI = DPSI +(NUTCOE(1,INUT)+NUTCOE(2,INUT)*TU)*DSIN(ARG)
     1              +NUTCOE(5,INUT)*DCOS(ARG)
C
        DEPSR= DEPSR-(NUTCOE(3,INUT)+NUTCOE(4,INUT)*TU)*DSIN(ARG)*ARGR
     1              +NUTCOE(4,INUT)*DCOS(ARG)/36525.D0
     2              +NUTCOE(6,INUT)*DCOS(ARG)*ARGR
        DPSIR= DPSIR+(NUTCOE(1,INUT)+NUTCOE(2,INUT)*TU)*DCOS(ARG)*ARGR
     1              +NUTCOE(2,INUT)*DSIN(ARG)/36525.D0
     2              -NUTCOE(5,INUT)*DSIN(ARG)*ARGR
      ENDDO
C
      RETURN
      END SUBROUTINE

      END MODULE
