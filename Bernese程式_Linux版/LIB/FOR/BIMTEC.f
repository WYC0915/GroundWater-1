      MODULE s_BIMTEC
      CONTAINS

C*
      SUBROUTINE BIMTEC(BIMCOE,BIMARG,XTEC  ,INTD  )
CC
CC NAME       :  BIMTEC
CC
CC PURPOSE    :  COMPUTE TEC BASED ON BROADCAST IONOSPHERE MODEL (BIM)
CC               COEFFICIENTS
CC
CC PARAMETERS :
CC         IN :  BIMCOE(I=1,2,J=1,..,4): BIM COEFFICIENTS     R*8(2,4)
CC                        I=1: ION ALPHAS
CC                        I=2: ION BETAS
CC               BIMARG(I=1,2,3): ARGUMENTS                   R*(*)
CC                        I=1: FRACTIONAL PART OF DAY (DAYS)
CC                        I=2: LATITUDE (RAD)
CC                        I=3: LONGITUDE (RAD)
CC        OUT :  XTEC(I=1,2): RESULTS                         R*8(*)
CC                        I=1: TEC (TECU)
CC                        I=2: PERIOD (SEC)
CC               INTD   : NIGHT-TIME DATA WRT BIM             I*4
CC                        =0/1: NO/YES
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S.SCHAER
CC
CC VERSION    :  4.3
CC
CC CREATED    :  21-JUN-00
CC
CC CHANGES    :  23-JUN-00 : SS: "INTD" INTRODUCED
CC               05-JUL-00 : SS: REDUCE GEODETIC LATITUDE ARGUMENT
CC               12-JUL-00 : SS: "IFLG" FOR TEST PURPOSES
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               04-MAY-12 : RD: USE DMOD FROM MODULE, USE M_BERN WITH ONLY
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      2000     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnerr
      USE d_const,  ONLY: C, FACTEC, FREQ, PI
      USE l_basfun, ONLY: dmod
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IAMP , IFLG , INTD , IPER
C
      REAL*8    XAMP , XION , XLAT0, XLAT1, XLAT2, XLON1, XPER , XPER0,
     1          XTIM1, XTIM2, XTIM3
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      REAL*8        BIMCOE(2,4),BIMARG(*),XTEC(*)
C
C
      DATA IFLG/0/
C
C CHECK TIME ARGUMENT
C -------------------
      IF (BIMARG(1).LT.0.D0 .OR.
     1    BIMARG(1).GE.1.D0) THEN
        WRITE(LFNERR,910)
910     FORMAT(/,' *** SR BIMTEC: ILLEGAL TIME ARGUMENT',/)
        CALL EXITRC(2)
      ENDIF
C
C CONVERT ARGUMENTS INTO UNITS OF SECONDS AND SEMI-CIRCLES
C --------------------------------------------------------
      XTIM1=BIMARG(1)*86400.D0
      XLAT1=BIMARG(2)/PI
      XLON1=BIMARG(3)/PI
C
C REDUCE GEODETIC LATITUDE ARGUMENT
C ---------------------------------
      IF (IFLG.EQ.0) THEN
        XLAT0=0.416D0
        IF (XLAT1.GT. XLAT0) XLAT1= XLAT0
        IF (XLAT1.LT.-XLAT0) XLAT1=-XLAT0
      ENDIF
C
C GEOMAGNETIC LATITUDE IN SEMI-CIRCLES
C ------------------------------------
      XLAT2=XLAT1+0.064D0*DCOS(PI*(XLON1-1.617D0))
      IF (IFLG.EQ.1) THEN
        IF (XLAT2.GT. 0.5D0) THEN
          XLAT2= 1.D0-XLAT2
          XLON1=XLON1+1.D0
        ENDIF
        IF (XLAT2.LT.-0.5D0) THEN
          XLAT2=-1.D0-XLAT2
          XLON1=XLON1+1.D0
        ENDIF
      ENDIF
C
C PERIOD IN SECONDS
C -----------------
      XPER=0.D0
      DO IPER=1,4
        XPER=XPER+BIMCOE(2,IPER)*XLAT2**(IPER-1)
      ENDDO
      XPER0=72000.D0
      IF (XPER.LT.XPER0) XPER=XPER0
C
C AMPLITUDE IN SECONDS
C --------------------
      XAMP=0.D0
      DO IAMP=1,4
        XAMP=XAMP+BIMCOE(1,IAMP)*XLAT2**(IAMP-1)
      ENDDO
      IF (XAMP.LT.0.D0) XAMP=0.D0
C
C LOCAL TIME IN SECONDS
C ---------------------
      XTIM2=DMOD(43200.D0*XLON1+XTIM1+86400.D0,86400.D0)
      IF (XTIM2.LT.7200.D0) XTIM2=XTIM2+86400.D0
C
C REDUCED TIME ARGUMENT IN RADIANS
C --------------------------------
      XTIM3=2.D0*PI*(XTIM2-50400.D0)/XPER
C
C IONOSPHERIC PATH DELAY WRT FIRST FREQUENCY IN SECONDS
C -----------------------------------------------------
      XION=5.D-9
      INTD=1
      IF (DABS(XTIM3).LT.DSQRT(6.D0-DSQRT(12.D0)) .AND.
     1  XAMP.GT.0.D0) THEN
        XION=XION+XAMP*(1.D0-XTIM3**2/2.D0+XTIM3**4/24.D0)
        INTD=0
      ENDIF
C
C COMPUTE TEC IN TECU WHERE C*FREQ(1)**2/FACTEC=1.8463D9
C ------------------------------------------------------
      XTEC(1)=XION*C*FREQ(1)**2/FACTEC
C
      IF (XAMP.GT.0.D0) THEN
        XTEC(2)=XPER
      ELSE
        XTEC(2)=XPER0
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
