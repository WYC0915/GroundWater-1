      MODULE s_TIDE96
      CONTAINS

C*
      SUBROUTINE TIDE96(XSTA,XSUN,XMON,DMJD,XPOL,YPOL,
     1                  IRCOCN,OCNAMP,OCNPHS,DXTIDE)
CC
CC  PURPOSE   :  COMPUTATION OF TIDAL CORRECTIONS OF STATION DISPLACEMENTS
CC               CAUSED BY LUNAR AND SOLAR GRAVITATIONAL ATTRACTION AND
CC               POLAR MOTION (SEE IERS CONVENTIONS 1996)
CC               (pages refer to IERS TN 21)
CC
CC               STEP 1 (HERE GENERAL DEGREE 2 AND 3 CORRECTIONS +
CC                       CALL ST1DIU + CALL ST1SEM + CALL ST1L1)
CC               STEP 2 (CALL ST2DIU + CALL ST2LON)
CC               CORRECTION FOR POLAR MOTION
CC
CC               NO CORRECTION FOR PERMANENT TIDE
CC               THESE CORRECTIONS MUST BE REMOVED IN ORDER TO GET THE
CC               NON-TIDAL STATION POSITION.
CC
CC PARAMETERS :
CC         IN :  XSTA(I),I=1,2,3:  GEOCENTRIC POSITION OF     R*8
CC                        THE STATION
CC               XSUN(I),I=1,2,3: GEOC. POSITION OF THE SUN   R*8
CC               XMON(I),I=1,2,3: GEOC. POSITION OF THE MOON  R*8
CC               DMJD : MJD                                  R*8
CC               XPOL  : POLAR MOTION (X RAD)                 R*8
CC               YPOL  : POLAR MOTION (Y RAD)                 R*8
CC               IRCOCN: OCEAN LOADING FLAG (0: APPLY OL)     I*4
CC               OCNAMP: OCEAN LOADING AMPLITUDES (M)         R*8(3,MAXOCN)
CC               OCNPHS: OCEAN LOADING PHASES   (RAD)         R*8(3,MAXOCN)
CC        OUT :  DXTIDE(I),I=1,2,3: DISPLACEMENT VECTOR       R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  V. DEHANT, S. MATHEWS AND J. GIPSON
CC
CC VERSION    :  4.1
CC
CC CREATED    :  06-OCT-97
CC
CC CHANGES    :  06-OCT-97 : TS: ADDEPTED FOR THE BERNESE SOFTWARE
CC               10-MAR-98 : TS: ADDED OCEAN LOADING CORRECTIONS
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               16-JUL-04 : HU: FHR FOR STEP 2 IS IN HOURS
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-FEB-07 : AG: USE PI AND 206264... FROM DEFCON
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_maxdim, ONLY: MAXOCN
      USE d_const, ONLY: GM, GMM, GMS, PI, ARS
      USE s_dmlmtv
      USE s_st2diu
      USE s_sprod
      USE s_st1l1
      USE s_ocload
      USE s_st2lon
      USE s_st1sem
      USE s_st1diu
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IRCOCN
C
      REAL*8    COSLA , COSPHI, DE    , DMJD  , DN    , DR    , F2MON ,
     1          F2SUN , F3MON , F3SUN , FHR   , HELP1 , HELP2 , P2MON ,
     2          P2SUN , P3MON , P3SUN , RMON  , RSTA  , RSUN  , SCM   ,
     3          SCMON , SCS   , SCSUN , SINLA , SINPHI, T     , X2MON ,
     4          X2SUN , X3MON , X3SUN , XP    , XPOL  , XQUER , YP    ,
     5          YPOL  , YQUER
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C
      REAL*8 XSTA(*),XSUN(*),XMON(*),DXTIDE(*)
      REAL*8 H20,L20,H3,L3,H2,L2
      REAL*8 SS(3),R(3,3),XCOSTA(3),DOLOAD(3)
C
      REAL*8    OCNAMP(3,MAXOCN)
      REAL*8    OCNPHS(3,MAXOCN)
C
C
C NOMINAL SECOND DEGREE AND THIRD DEGREE LOVE NUMBERS AND SHIDA NUMBERS
C (H20, L20 ARE THE "ANELASTIC" VALUES)
C ---------------------------------------------------------------------
      DATA H20/0.6078D0/,L20/0.0847D0/,H3/0.292D0/,L3/0.015D0/
      DATA XQUER/0.033D0/,YQUER/0.331D0/
C
C SCALAR PRODUCT OF STATION VECTOR WITH SUN/MOON VECTOR
C -----------------------------------------------------
      CALL SPROD(XSTA,XSUN,SCS,RSTA,RSUN)
      CALL SPROD(XSTA,XMON,SCM,RSTA,RMON)
      SCSUN=SCS/RSTA/RSUN
      SCMON=SCM/RSTA/RMON
C
C COMPUTATION OF NEW H2 AND L2 (page 60)
C --------------------------------------
      COSPHI=DSQRT(XSTA(1)**2+XSTA(2)**2)/RSTA
      H2=H20-0.0006D0*(1.D0-3.D0/2.D0*COSPHI**2)
      L2=L20+0.0002D0*(1.D0-3.D0/2.D0*COSPHI**2)
C
C P2-TERM  (eqn 8 page 61)
C -------
      P2SUN=3.D0*(H2/2.D0-L2)*SCSUN**2-H2/2.D0
      P2MON=3.D0*(H2/2.D0-L2)*SCMON**2-H2/2.D0
C
C P3-TERM  (eqn 9 page 61)
C -------
      P3SUN=5.D0/2.D0*(H3-3.D0*L3)*SCSUN**3+3.D0/2.D0*(L3-H3)*SCSUN
      P3MON=5.D0/2.D0*(H3-3.D0*L3)*SCMON**3+3.D0/2.D0*(L3-H3)*SCMON
C
C TERM IN DIRECTION OF SUN/MOON VECTOR
C ------------------------------------
      X2SUN=3.D0*L2*SCSUN
      X2MON=3.D0*L2*SCMON
      X3SUN=3.D0*L3/2.D0*(5.D0*SCSUN**2-1.D0)
      X3MON=3.D0*L3/2.D0*(5.D0*SCMON**2-1.D0)
C
C FACTORS FOR SUN/MOON
C --------------------
      F2SUN=GMS/GM*RSTA*(RSTA/RSUN)**3
      F2MON=GMM/GM*RSTA*(RSTA/RMON)**3
      F3SUN=F2SUN*(RSTA/RSUN)
      F3MON=F2MON*(RSTA/RMON)
C
C TOTAL DISPLACEMENT
C ------------------
      DO 10 I=1,3
        DXTIDE(I)=F2SUN*( X2SUN*XSUN(I)/RSUN + P2SUN*XSTA(I)/RSTA ) +
     1            F2MON*( X2MON*XMON(I)/RMON + P2MON*XSTA(I)/RSTA ) +
     2            F3SUN*( X3SUN*XSUN(I)/RSUN + P3SUN*XSTA(I)/RSTA ) +
     3            F3MON*( X3MON*XMON(I)/RMON + P3MON*XSTA(I)/RSTA )
10    CONTINUE
C
      SINPHI=XSTA(3)/RSTA
      COSPHI=DSQRT(XSTA(1)**2+XSTA(2)**2)/RSTA
      COSLA=XSTA(1)/COSPHI/RSTA
      SINLA=XSTA(2)/COSPHI/RSTA
C
C CORRECTIONS FOR THE OUT-OF-PHASE PART OF LOVE NUMBERS (PART H_2^(0)I
C AND L_2^(0)I )
C
C FIRST, FOR THE DIURNAL BAND
C ---------------------------
      CALL ST1DIU(XSTA,XSUN,XMON,F2SUN,F2MON,XCOSTA)
      DO 11 I=1,3
        DXTIDE(I)=DXTIDE(I)+XCOSTA(I)
11    CONTINUE
C
C SECOND, FOR THE SEMI-DIURNAL BAND
C ---------------------------------
      CALL ST1SEM(XSTA,XSUN,XMON,F2SUN,F2MON,XCOSTA)
      DO 12 I=1,3
        DXTIDE(I)=DXTIDE(I)+XCOSTA(I)
12    CONTINUE
C
C CORRECTIONS FOR THE LATITUDE DEPENCE OF LOVE NUMBERS (PART L^(1) )
C ------------------------------------------------------------------
      CALL ST1L1(XSTA,XSUN,XMON,F2SUN,F2MON,XCOSTA)
      DO 13 I=1,3
        DXTIDE(I)=DXTIDE(I)+XCOSTA(I)
13    CONTINUE
C
C CONSIDER CORRECTIONS FOR STEP 2
C ===============================
C
C CORRECTIONS FOR THE DIURNAL BAND:
C
C  FIRST, WE NEED TO KNOW THE DATE CONVERTED IN JULIAN CENTURIES
C  -------------------------------------------------------------
      T=(DMJD-51544.5D0)/36525.D0
C
C  AND THE HOUR IN THE DAY
cc    FHR = DMJD-INT(DMJD)   ! Error corrected at CODE on June 14, 2004
      FHR = (DMJD-INT(DMJD))*24D0
C
C  2) CALL THE SUBROUTINE COMPUTING THE CORRECTION OF UTC TIME
C
C      CALL DUTC(IYR,IMONTH,IDAY,DTT)
C      fhr=fhr+dtt/3600.
C
C  SECOND, WE CAN CALL THE SUBROUTINE ST2DIU
C  -----------------------------------------
      CALL ST2DIU(XSTA,FHR,T,XCOSTA)
      DO 14 I=1,3
        DXTIDE(I)=DXTIDE(I)+XCOSTA(I)
14    CONTINUE
C
C CORRECTIONS FOR THE LONG-PERIOD BAND:
C -------------------------------------
      CALL ST2LON(XSTA,FHR,T,XCOSTA)
      DO 15 I=1,3
        DXTIDE(I)=DXTIDE(I)+XCOSTA(I)
15    CONTINUE
C
C UNCORRECT FOR THE PERMANENT TIDE
C --------------------------------
cc      DR=-DSQRT(5.D0/4.D0/PI)*H2*0.31460D0*(3.D0/2.D0*SINPHI**2-0.5D0)
cc      DN=-DSQRT(5.D0/4.D0/PI)*L2*0.31460D0*3.D0*COSPHI*SINPHI
cc      DXTIDE(1)=DXTIDE(1)-DR*COSLA*COSPHI+DN*COSLA*SINPHI
cc      DXTIDE(2)=DXTIDE(2)-DR*SINLA*COSPHI+DN*SINLA*SINPHI
cc      DXTIDE(3)=DXTIDE(3)-DR*SINPHI      -DN*COSPHI
C
C DISPLACEMENT IN STEP 3: ROTATIONAL DEFORMATION DUE TO POLAR MOTION
C (SLIGHTLY TIMEDEPENDENT)
C ------------------------------------------------------------------
      R(1,1)=SINPHI*COSLA
      R(1,2)=SINPHI*SINLA
      R(1,3)=-COSPHI
      R(2,1)=-SINLA
      R(2,2)=COSLA
      R(2,3)=0.D0
      R(3,1)=COSPHI*COSLA
      R(3,2)=COSPHI*SINLA
      R(3,3)=SINPHI
C
C RAD IN ARCSEC AND TRANSFORM MM IN M
      XP=XPOL*ARS-XQUER
      YP=YPOL*ARS-YQUER
      HELP1=(XP*COSLA-YP*SINLA)
      HELP2=(XP*SINLA+YP*COSLA)
C
C POLTIDES IN M ,
C DISPLACEMENT IN  1 .. CO-LATITUDE, 2 .. LONGITUDE, 3 .. RADIAL
C --------------------------------------------------------------
      SS(1) = -9.D0*(1-2*COSPHI**2)  *HELP1/1000.D0
      SS(2) =  9.D0*SINPHI           *HELP2/1000.D0
      SS(3) =-32.D0*(2*SINPHI*COSPHI)*HELP1/1000.D0
C
C sin(2*colat)=2*sinphi*cosphi
C cos(2*colat)=1-2*cosphi**2
C
C MULTIPLY TRANSPOSE OF R WITH DISPLACEMENTVECTOR
C -----------------------------------------------
      CALL DMLMTV(SS,R,SS)
C
C DISPLACEMENT OF POLTIDES (IN M)
C -------------------------------
      DXTIDE(1)=DXTIDE(1)+SS(1)
      DXTIDE(2)=DXTIDE(2)+SS(2)
      DXTIDE(3)=DXTIDE(3)+SS(3)
C
C ADD DISPLACEMENT DUE TO OCEAN TIDES
C -----------------------------------
      IF (IRCOCN.EQ.0) THEN
        CALL OCLOAD(DMJD,OCNAMP,OCNPHS,DOLOAD)
        DN=-DOLOAD(1)
        DE=-DOLOAD(2)
        DR= DOLOAD(3)
        XCOSTA(1)=DR*COSLA*COSPHI-DE*SINLA-DN*SINPHI*COSLA
        XCOSTA(2)=DR*SINLA*COSPHI+DE*COSLA-DN*SINPHI*SINLA
        XCOSTA(3)=DR*SINPHI+DN*COSPHI
        DO 20 I=1,3
          DXTIDE(I)=DXTIDE(I)+XCOSTA(I)
20      CONTINUE
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
