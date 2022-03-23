      MODULE s_TIDE2000
      CONTAINS

C*
      SUBROUTINE TIDE2000(STANAM,XSTA,XSUN,XMON,DMJD,XPOL,YPOL,MPOL,
     1                    IRCOCN,OCNAMP,OCNPHS,
     2                    IRCATM,ATMSIN,ATMCOS,DXTIDE)
CC
CC PURPOSE    :  COMPUTATION OF TIDAL CORRECTIONS OF STATION DISPLACEMENTS
CC               CAUSED BY LUNAR AND SOLAR GRAVITATIONAL ATTRACTION
CC               (SEE IERS STANDARDS 2000)
CC               STEP 1 (HERE GENERAL DEGREE 2 AND 3 CORRECTIONS +
CC                       CALL ST1DIU + CALL ST1SEM + CALL ST1L1)
CC               + STEP 2 (CALL STEP2DIU + CALL ST2LON)
CC                         CORRECTION FOR POLAR MOTION
CC               IT HAS BEEN DECIDED THAT THE STEP 3 NON-CORRECTION FOR
CC               PERMANENT TIDE WOULD NOT BE APPLIED IN ORDER TO AVOID JUMP
CC               IN THE REFERENCE FRAME (THIS STEP 3 MUST ADDED IN ORDER TO
CC               GET THE NON-TIDAL STATION POSITION AND TO BE CONFORMED WITH
CC               THE IAG RESOLUTION.)
CC
CC PARAMETERS :
CC         IN :  STANAM           : STATION NAME                        CH*16
CC               XSTA(I),I=1,2,3,4: GEOCENTRIC POSITION OF THE STATION  R*8
CC               XSUN(I),I=1,2,3,4: GEOC. POSITION OF THE SUN           R*8
CC               XMON(I),I=1,2,3,4: GEOC. POSITION OF THE MOON          R*8
CC               DMJD             : MJD                                 R*8
CC               XPOL             : POLAR MOTION (X RAD)                R*8
CC               YPOL             : POLAR MOTION (Y RAD)                R*8
CC               MPOL             : MEAN POLE                           I*4
CC                                  1: OLD DEF, 2: IERS2003
CC               IRCOCN           : OCEAN LOADING FLAG (0: APPLY OL)    I*4
CC               OCNAMP           : OCEAN LOADING AMPLITUDES (M)  R*8(3,MAXOCN)
CC               OCNPHS           : OCEAN LOADING PHASES   (RAD)  R*8(3,MAXOCN)
CC               IRCATM           : OCEAN LOADING FLAG (0: APPLY OL)    I*4
CC               ATMSIN           : ATM-TID LOADING SIN-TERM (M)  R*8(3,MAXATM)
CC               ATMCOS           : ATM-TID LOADING COS-TERM (M)  R*8(3,MAXATM)
CC        OUT :  DXTIDE(I),I=1,2,3: DISPLACEMENT VECTOR
CC
CC AUTHOR     :  V. DEHANT, S. MATHEWS AND J. GIPSON (IERS 1996)
CC                     (TEST BETWEEN TWO SUBROUTINES)
CC AUTHOR     :  V. DEHANT AND S. MATHEWS (IERS 2000)
CC                     (TEST IN THE BERNESE PROGRAM BY C. BRUYNINX)
CC
CC CREATED    :  23-MAR-96
CC
CC CHANGES    :  06-OCT-97 : TS: ADDAPTED FOR THE BERNESE SOFTWARE
CC               10-MAR-98 : TS: ADDED OCEAN LOADING CORRECTIONS
CC               01-FEB-01 : CB: IERS200
CC               02-JUN-03 : CU: ADDAPTED FOR THE BERNESE SOFTWARE V5.0
CC               10-JUN-03 : HU: MASS RATIOS FROM CONST.
CC               12-JUL-04 : HU: FHR FOR STEP 2 IS IN HOURS
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               11-OCT-05 : MM: GEOCENTER COORDINATES POSSIBLE
CC               21-JAN-06 : HU: CHECK COORDINATE COMPONENTS SEPARATELY
CC               21-SEP-06 : HU: MEAN POLE HANDLING
CC               09-NOV-06 : AG: SWITCH FROM SR OCLOAD TO SR HARLOAD
CC               28-FEB-07 : AG: USE 206264... FROM DEFCON
CC               29-JUN-09 : RD: ADD S1/S2 ATM-TIDAL LOADING
CC               07-JUN-11 : HB: SS(3), TYPO CORRECTED
CC                               => NOT YET ACTIVATED
CC               21-SEP-12 : RD/SS: ACTIVATE SS(3) TYPO CORRECTION
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      2003     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern, ONLY: lfnerr
      USE m_maxdim, ONLY: MAXOCN,MAXATM
      USE d_const, ONLY: AE, GM, GMM, GMS, ARS, PI
      USE s_dmlmtv
      USE s_sprod
      USE s_st1l1
      USE s_ocload
      USE s_harload
      USE s_st2lon
      USE s_step2diu
      USE s_st1sem
      USE s_st1diu
      USE s_meanpol
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I      , IRCOCN , MPOL   , IRCATM
C
      REAL*8    COSLA  , COSPHI , DE     , DMJD   , DN     , DR     ,
     1          FAC2MON, FAC2SUN, FAC3MON, FAC3SUN, FHR    , HELP1  ,
     2          HELP2  , P2MON  , P2SUN  , P3MON  , P3SUN  , RMON   ,
     3          RSTA   , RSUN   , SCM    , SCMON  , SCS    , SCSUN  ,
     4          SINLA  , SINPHI , T      , X2MON  , X2SUN  , X3MON  ,
     5          X3SUN  , XP     , XPOL   , XQUER  , YP     , YPOL   ,
     6          YQUER
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      REAL*8  XSTA(*),XSUN(*),XMON(*),DXTIDE(*)
      REAL*8  H20,L20,H3,L3,H2,L2
      REAL*8  SS(3),R(3,3),XCOSTA(3),DOLOAD(3)
C
      REAL*8  OCNAMP(3,MAXOCN),OCNPHS(3,MAXOCN)
      REAL*8  ATMSIN(3,MAXATM),ATMCOS(3,MAXATM)
C
      CHARACTER*16 STANAM, STAERR
C
C
C NOMINAL SECOND DEGREE AND THIRD DEGREE LOVE NUMBERS AND SHIDA NUMBERS
C ---------------------------------------------------------------------
      DATA H20/0.6078D0/,L20/0.0847D0/,H3/0.292D0/,L3/0.015D0/
C
C STATION IN GEOCENTER?
C ---------------------
      IF (XSTA(1).EQ.0D0 .AND. XSTA(2).EQ.0D0 .AND. XSTA(3).EQ.0D0) THEN
        DXTIDE(1:3) = 0.d0
        IF (STAERR .EQ. STANAM) RETURN
        WRITE(LFNERR,'(3(/,A),A16,/,A,F13.5,/)')
     1    ' ### SR TIDE2000: STATION COORDINATES ARE ZERO (GEOCENTER)',
     2    '                  NO TIDAL CORRECTIONS COMPUTED',
     3    '                  STATION NAME: ',STANAM
        STAERR = STANAM
        RETURN
      ENDIF
C
C MEAN POLE
C ---------
      CALL MEANPOL(MPOL,DMJD,XQUER,YQUER)
C
C SCALAR PRODUCT OF STATION VECTOR WITH SUN/MOON VECTOR
C -----------------------------------------------------
      CALL SPROD(XSTA,XSUN,SCS,RSTA,RSUN)
      CALL SPROD(XSTA,XMON,SCM,RSTA,RMON)
      SCSUN=SCS/RSTA/RSUN
      SCMON=SCM/RSTA/RMON
C
C COMPUTATION OF NEW H2 AND L2
C ----------------------------
      COSPHI = DSQRT(XSTA(1)**2+XSTA(2)**2)/RSTA
      H2     = H20-0.0006D0*(1.D0-3.D0/2.D0*COSPHI**2)
      L2     = L20+0.0002D0*(1.D0-3.D0/2.D0*COSPHI**2)
C
C P2-TERM
C -------
      P2SUN = 3.D0*(H2/2.D0-L2)*SCSUN**2-H2/2.D0
      P2MON = 3.D0*(H2/2.D0-L2)*SCMON**2-H2/2.D0
C
C P3-TERM
C -------
      P3SUN = 5.D0/2.D0*(H3-3.D0*L3)*SCSUN**3+3.D0/2.D0*(L3-H3)*SCSUN
      P3MON = 5.D0/2.D0*(H3-3.D0*L3)*SCMON**3+3.D0/2.D0*(L3-H3)*SCMON
C
C TERM IN DIRECTION OF SUN/MOON VECTOR
C ------------------------------------
      X2SUN = 3.D0*L2*SCSUN
      X2MON = 3.D0*L2*SCMON
      X3SUN = 3.D0*L3/2.D0*(5.D0*SCSUN**2-1.D0)
      X3MON = 3.D0*L3/2.D0*(5.D0*SCMON**2-1.D0)
C
C FACTORS FOR SUN/MOON
C --------------------
      FAC2SUN = GMS/GM*AE*(AE/RSUN)**3
      FAC2MON = GMM/GM*AE*(AE/RMON)**3
      FAC3SUN = FAC2SUN*(AE/RSUN)
      FAC3MON = FAC2MON*(AE/RMON)
C
C TOTAL DISPLACEMENT
C ------------------
      DO 10 I=1,3
        DXTIDE(I) = FAC2SUN*( X2SUN*XSUN(I)/RSUN + P2SUN*XSTA(I)/RSTA )+
     1              FAC2MON*( X2MON*XMON(I)/RMON + P2MON*XSTA(I)/RSTA )+
     2              FAC3SUN*( X3SUN*XSUN(I)/RSUN + P3SUN*XSTA(I)/RSTA )+
     3              FAC3MON*( X3MON*XMON(I)/RMON + P3MON*XSTA(I)/RSTA )
10    CONTINUE
C
C CORRECTIONS FOR THE OUT-OF-PHASE PART OF LOVE NUMBERS (PART H_2^(0)I
C            AND L_2^(0)I )
C FIRST, FOR THE DIURNAL BAND
C
      CALL ST1DIU(XSTA,XSUN,XMON,FAC2SUN,FAC2MON,XCOSTA)
      DO 11 I=1,3
        DXTIDE(I) = DXTIDE(I)+XCOSTA(I)
11    CONTINUE
C
C SECOND, FOR THE SEMI-DIURNAL BAND
C
      CALL ST1SEM(XSTA,XSUN,XMON,FAC2SUN,FAC2MON,XCOSTA)
      DO 12 I=1,3
        DXTIDE(I) = DXTIDE(I)+XCOSTA(I)
12    CONTINUE
C
C CORRECTIONS FOR THE LATITUDE DEPENDENCE OF LOVE NUMBERS (PART L^(1) )
C
      CALL ST1L1(XSTA,XSUN,XMON,FAC2SUN,FAC2MON,XCOSTA)
      DO 13 I=1,3
        DXTIDE(I) = DXTIDE(I)+XCOSTA(I)
13    CONTINUE
C
C CONSIDER CORRECTIONS FOR STEP 2
C ===============================
C
C CORRECTIONS FOR THE DIURNAL BAND:
C
C  FIRST, WE NEED TO KNOW THE DATE CONVERTED IN JULIAN CENTURIES
c  -------------------------------------------------------------
      T = (DMJD-51544.5D0)/36525.D0
C
C  AND THE HOUR IN THE DAY
cc    FHR = DMJD-INT(DMJD)   ! Error corrected at CODE on June 14, 2004
      FHR = (DMJD-INT(DMJD))*24D0
c
c   2) CALL THE SUBROUTINE COMPUTING THE CORRECTION OF UTC TIME
c
C      CALL DUTC(IYR,IMONTH,IDAY,DTT)
C      fhr=fhr+dtt/3600.
C
C  SECOND, WE CAN CALL THE SUBROUTINE STEP2DIU
C
      CALL STEP2DIU(XSTA,FHR,T,XCOSTA)
      DO 14 I=1,3
        DXTIDE(I) = DXTIDE(I)+XCOSTA(I)
14    CONTINUE
C
C CORRECTIONS FOR THE LONG-PERIOD BAND:
C
      CALL ST2LON(XSTA,FHR,T,XCOSTA)
      DO 15 I=1,3
        DXTIDE(I) = DXTIDE(I)+XCOSTA(I)
15    CONTINUE
C
C
C DISPLACEMENT IN STEP 3: ROTATIONAL DEFORMATION DUE TO POLAR MOTION
C (SLIGHTLY TIMEDEPENDENT)
C ------------------------------------------------------------------
      SINPHI = XSTA(3)/RSTA
      COSPHI = DSQRT(XSTA(1)**2+XSTA(2)**2)/RSTA
      SINLA  = XSTA(2)/COSPHI/RSTA
      COSLA  = XSTA(1)/COSPHI/RSTA
C
      R(1,1) = SINPHI*COSLA
      R(1,2) = SINPHI*SINLA
      R(1,3) = -COSPHI
      R(2,1) = -SINLA
      R(2,2) = COSLA
      R(2,3) = 0.D0
      R(3,1) = COSPHI*COSLA
      R(3,2) = COSPHI*SINLA
      R(3,3) = SINPHI
C
C RAD IN ARCSEC AND TRANSFORM MM IN M
      XP    = (XPOL-XQUER)*ARS
      YP    = (YPOL-YQUER)*ARS
      HELP1 = (XP*COSLA-YP*SINLA)
      HELP2 = (XP*SINLA+YP*COSLA)
C
C POLTDES IN M ,
C DISPLACEMENT IN  1 .. CO-LATITUDE, 2 .. LONGITUDE, 3 .. RADIAL
C --------------------------------------------------------------
      SS(1) =  -9.D0*(1-2*COSPHI**2)  *HELP1/1000.D0
      SS(2) =   9.D0*SINPHI           *HELP2/1000.D0
!!      SS(3) = -32.D0*(2*SINPHI*COSPHI)*HELP1/1000.D0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!! has to be activated when switching to IERS2010 conventions !!!!!!!!!!
! IERS2010 Conventions, typo corrected:
      SS(3) = -33.D0*(2*SINPHI*COSPHI)*HELP1/1000.D0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
C
C MULTIPLY TRANSPOSE OF R WITH DISPLACEMENTVECTOR
C -----------------------------------------------
      CALL DMLMTV(SS,R,SS)
C
C DISPLACEMENT OF POLTIDES (IN M)
C -------------------------------
      DXTIDE(1) = DXTIDE(1)+SS(1)
      DXTIDE(2) = DXTIDE(2)+SS(2)
      DXTIDE(3) = DXTIDE(3)+SS(3)
C
C ADD DISPLACEMENT DUE TO OCEAN TIDES
C -----------------------------------
      IF (IRCOCN.EQ.0) THEN
CC        CALL OCLOAD(DMJD,OCNAMP,OCNPHS,DOLOAD)
        CALL HARLOAD(DMJD,OCNAMP,OCNPHS,DOLOAD)
        DN = -DOLOAD(1)
        DE = -DOLOAD(2)
        DR =  DOLOAD(3)
        XCOSTA(1) = DR*COSLA*COSPHI-DE*SINLA-DN*SINPHI*COSLA
        XCOSTA(2) = DR*SINLA*COSPHI+DE*COSLA-DN*SINPHI*SINLA
        XCOSTA(3) = DR*SINPHI+DN*COSPHI
        DO 20 I=1,3
          DXTIDE(I) = DXTIDE(I)+XCOSTA(I)
20      CONTINUE
      ENDIF
C
C ADD DISPLACEMENT DUE TO ATMOSPHERIC TIDES
C -----------------------------------------
      IF (IRCATM.EQ.0) THEN
        T = DMJD - DINT(DMJD)
        DN = 0D0
        DE = 0D0
        DR = 0D0
        DO I = 1,MAXATM
          DN = DN + ATMCOS(1,I)*COS(T*DBLE(I)*2D0*PI) +
     1              ATMSIN(1,I)*SIN(T*DBLE(I)*2D0*PI)
          DE = DE + ATMCOS(2,I)*COS(T*DBLE(I)*2D0*PI) +
     1              ATMSIN(2,I)*SIN(T*DBLE(I)*2D0*PI)
          DR = DR + ATMCOS(3,I)*COS(T*DBLE(I)*2D0*PI) +
     1              ATMSIN(3,I)*SIN(T*DBLE(I)*2D0*PI)
        ENDDO
        XCOSTA(1) = DR*COSLA*COSPHI-DE*SINLA-DN*SINPHI*COSLA
        XCOSTA(2) = DR*SINLA*COSPHI+DE*COSLA-DN*SINPHI*SINLA
        XCOSTA(3) = DR*SINPHI+DN*COSPHI
        DO 30 I=1,3
          DXTIDE(I) = DXTIDE(I)+XCOSTA(I)
30      CONTINUE
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
