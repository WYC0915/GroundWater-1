      MODULE s_XYZTIM
      CONTAINS

C*
      SUBROUTINE XYZTIM(IORSYS,TOBS,XSTA,STANAM,XSTEPO,cmcyn)
CC
CC NAME       :  XYZTIM
CC
CC PURPOSE    :  CORRECT GEOCENTRIC COORDINATES FOR TIME-DEPENDENT
CC               EFFECTS: EARTH TIDES
CC
CC PARAMETERS :
CC         IN :  IORSYS : ORBIT SYSTEM                        I*4
CC                        =1: B1950.0
CC                        =2: J2000.0
CC               TOBS   : OBSERVATION TIME IN MJD             R*8
CC               XSTA(I),I=1,..,3: GEOCENTRIC COORDINATES IN  R*8
CC                        IN WGS SYSTEM
CC               STANAM : STATION NAME                       CH*16
CC        OUT :  XSTEPO(I),I=1,..,3: GEOCENTRIC COORDINATES   R*8
CC                        CORRECTED FOR EARTH TIDES
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  90/03/03 13:50
CC
CC CHANGES    :  05-JUN-92 : ??: NEW PARAMETER "IORSYS" FOR J2000.0
CC                               NEW CALLS: "SUNEFF", "MONEFF", "COOTRA"
CC               05-JUN-96 : TS: CALL POLDEF CHANGED DUE TO SUBDAILY POLE
CC               06-OCT-97 : TS: BUFFER FOR INTERPOLATION
CC               06-OCT-97 : TS: NEW TIDES MODEL (TIDE96)
CC               09-OCT-97 : TS: T(2) BUFFER TIME CORRECTED
CC               10-MAR-98 : TS: OCEAN LOADING ADDED
CC               01-MAY-01 : DS: DELTAT INCREASED 0.01D0->0.001D0
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               11-JUN-03 : HU: NEW TIDES MODEL (TIDE2000)
CC               24-NOV-03 : HU: ROUNDING PROBLEM CORRECTED
CC               28-JUN-04 : RD: USE MAXSTA FROM M_MAXDIM
CC               30-NOV-04 : SS: HARD-WIRED VELOCITY REDUCTION FOR AMUN
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               11-OCT-05 : MM: NEW TIDE2000 CALL
CC               17-DEC-05 : SS: AMU2 ADDED
CC               18-JUL-06 : AG: CMC IMPLEMENTED
CC               21-SEP-06 : HU: MEAN POLE HANDLING (DISABLED)
CC               09-NOV-06 : AG: MEAN POLE HANDLING ENABLED
CC               09-JAN-07 : AG: NO APPLYING OF CMC IF ICMC /= 0
CC               30-MAY-07 : AG: USE s_suneff
CC               29-JUN-09 : RD: ADD S1/S2 ATM-TIDAL LOADING
CC               01-OCT-10 : CR: NEW CALL OF SUNEFF
CC               02-DEC-10 : RD: CMC for ATL ADDED
CC               06-MAY-11 : HB: GET MPOL THROUGH D_MODEL
CC               04-MAY-12 : RD: USE DMOD FROM MODULE, USE M_BERN WITH ONLY
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1990     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
C REMARK: MAXSTA has to be taken either from M_MAXDIM (GPSSIM, MAUPRP) or
C         from P_GPSEST. The source with the biggest value has to be selected.
C ----------------------------------------------------------------------------
      USE m_bern,   ONLY: i4b, r8b, lfnerr
      USE m_maxdim, ONLY: MAXOCN,MAXATM,MAXSTA
      USE d_model,  ONLY: getModKey, chrValLength, mod_orb_meaPol
      USE l_basfun, ONLY: dmod
      USE s_cmc,    ONLY: getcmc
      USE s_poldef
      USE s_cootra
      USE s_moneff
      USE s_suneff
      USE s_tide2000
      USE s_gtocnl
      USE s_gtatml
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IDERIV, IORSYS, IRCOCN, L     , NSTA  , NTOT  ,
     1          IRCATM
C
      REAL*8    CS    , EPS   , GPSUTC, SS    , SZ    , TDT   , TOBS  ,
     1          UT1GPS, UT1UTC, XFAC  , XM1   , XM2   , XM3   , XPOL  ,
     2          XS1   , XS2   , XS3   , YPOL
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*16 STNAME(MAXSTA),STANAM
C
      INTEGER*4 IFLAG(2)
C
      REAL*8 XSTA(*),XSTEPO(*)
      REAL*8 XSUN(4),XMON(4),DXTIDE(3),DUM3(3)
      REAL*8 T(2,MAXSTA),DR(3,2,MAXSTA),DRABS(3)
      REAL*8 DELTAT,cmc(3)
      REAL*8 OCNAMP(3,MAXOCN,MAXSTA),OCNPHS(3,MAXOCN,MAXSTA)
      REAL*8 ATMSIN(3,MAXATM,MAXSTA),ATMCOS(3,MAXATM,MAXSTA)
C
      LOGICAL,  DIMENSION(2), OPTIONAL   :: cmcyn

      CHARACTER(LEN=chrValLength)          :: chrmPol
      CHARACTER(LEN=8)                     :: srNget
      REAL(r8b)                            :: numVal
      INTEGER(i4b)                 , SAVE  :: mPol
      LOGICAL                      , SAVE  :: mFirst = .TRUE.
C
      PARAMETER (DELTAT=0.001D0, EPS=1D-7)
C
C
      DATA NTOT/0/
C
C Get MEANPOL convention
C ----------------------
      IF (mFirst) THEN
        CALL getModKey(mod_orb_meaPol,chrmPol,srNget,numVal)
        mPol = IDNINT(numVal)
        mFirst = .FALSE.
      ENDIF

C CHECK IF STATION NAME IN LIST
C -----------------------------
      DO 10 NSTA=1,NTOT
        IF (STNAME(NSTA).EQ.STANAM) THEN
          GOTO 100
        END IF
10    CONTINUE
C
C STATION NOT FOUND --> ADD TO LIST
C ---------------------------------
      NTOT = NTOT + 1
      NSTA = NTOT
      STNAME(NSTA) = STANAM
      T(1,NSTA) = TOBS
      T(2,NSTA) = TOBS + DELTAT
      IFLAG(1) = 1
      IFLAG(2) = 1
C
C CHECK MAXSTA
C ------------
      IF (NTOT.GT.MAXSTA) THEN
        WRITE(LFNERR,901) NTOT, MAXSTA
901     FORMAT(/,' *** SR XYZTIM: TOO MANY STATION ',
     1                         /,16X,'MAX. NUMBER OF STATIONS : ',I5,
     2                         /,16X,'CURRENT NUMBER OF STAT. : ',I5,/)
        CALL EXITRC(2)
      ENDIF
C
C GET OCEAN LOADING TABLES FOR NEW STATION
C ----------------------------------------
      CALL GTOCNL(STNAME(NSTA),OCNAMP(1,1,NSTA),OCNPHS(1,1,NSTA),IRCOCN)
      CALL GTATML(STNAME(NSTA),ATMSIN(1,1,NSTA),ATMCOS(1,1,NSTA),IRCATM)
C
      GOTO 200
C
C FOR INTERPOLATION
C -----------------
100   IF (T(1,NSTA)-EPS.LE.TOBS .AND. TOBS.LE.T(2,NSTA)+EPS) THEN
        IFLAG(1) = 0
        IFLAG(2) = 0
      ELSE IF (T(2,NSTA).LE.TOBS.AND.TOBS.LE.(T(2,NSTA)+DELTAT)) THEN
        T(1,NSTA) = T(2,NSTA)
        T(2,NSTA) = T(2,NSTA) + DELTAT
        DR(1,1,NSTA) = DR(1,2,NSTA)
        DR(2,1,NSTA) = DR(2,2,NSTA)
        DR(3,1,NSTA) = DR(3,2,NSTA)
        IFLAG(1) = 0
        IFLAG(2) = 1
      ELSE IF ((T(1,NSTA)-DELTAT).LE.TOBS.AND.TOBS.LE.T(1,NSTA)) THEN
        T(2,NSTA) = T(1,NSTA)
        T(1,NSTA) = T(1,NSTA) - DELTAT
        DR(1,2,NSTA) = DR(1,1,NSTA)
        DR(2,2,NSTA) = DR(2,1,NSTA)
        DR(3,2,NSTA) = DR(3,1,NSTA)
        IFLAG(1) = 1
        IFLAG(2) = 0
      ELSE
        T(1,NSTA) = TOBS
        T(2,NSTA) = TOBS + DELTAT
        IFLAG(1) = 1
        IFLAG(2) = 1
      END IF
C
C CHECK IF T(2,NSTA) IS WITHIN THE SAME DAY AS TOBS (FOR POLE)
C ------------------------------------------------------------
200   CONTINUE
      IF (T(2,NSTA).GT.DINT(TOBS)+1D0) THEN
        T(2,NSTA)=DINT(TOBS)+1D0
        IFLAG(2) = 1
      ENDIF
C
C GET TWO VALUES FOR INTERPOLATION
C --------------------------------
      DO 50 L=1,2
        IF (IFLAG(L).EQ.1) THEN
C
C GET COORDINATES OF SUN AND MOON
C -------------------------------
C
C POLE AND UT1
C ------------
          CALL POLDEF(T(L,NSTA),1,XPOL,YPOL,UT1UTC,GPSUTC)
C
C SUN AND MOON (TIME ARGUMENT: TDB, APPROX. AS TDT)
C -------------------------------------------------
          TDT=T(L,NSTA)+(19.D0+32.184D0)/86400.D0
          CALL SUNEFF(IORSYS,2.D0,TDT,XSUN,DUM3)
          CALL MONEFF(IORSYS,1.D0,TDT,XMON)
C
C TRANSFORMATION FROM B1950.0 OR J2000.0 TO WGS
C ---------------------------------------------
          IDERIV=0
          CALL COOTRA(IORSYS,IDERIV,T(L,NSTA),XSUN,SZ,XPOL,YPOL,UT1GPS)
          CALL COOTRA(IORSYS,IDERIV,T(L,NSTA),XMON,SZ,XPOL,YPOL,UT1GPS)
C
          SS=DSIN(SZ)
          CS=DCOS(SZ)
C
          XS1= CS*XSUN(1)+SS*XSUN(2)
          XS2=-SS*XSUN(1)+CS*XSUN(2)
          XS3=    XSUN(3)
          XSUN(1)=      XS1         +XPOL*XS3
          XSUN(2)=               XS2-YPOL*XS3
          XSUN(3)=-XPOL*XS1+YPOL*XS2     +XS3
C
          XM1= CS*XMON(1)+SS*XMON(2)
          XM2=-SS*XMON(1)+CS*XMON(2)
          XM3=    XMON(3)
          XMON(1)=      XM1         +XPOL*XM3
          XMON(2)=               XM2-YPOL*XM3
          XMON(3)=-XPOL*XM1+YPOL*XM2     +XM3
C
C EARTH TIDE CORRECTION
C ---------------------
C Use mean pole: mpol=1: no, mpol=2: yes
CC          MPOL=1
CC          MPOL=2           => mPol is got from d_model
CC        CALL TIDALD(XSTA,XSUN,XMON,DXTIDE)
CC        CALL TIDNEW(XSTA,XSUN,XMON,T(L,NSTA),XPOL,YPOL,DXTIDE)
CC        CALL TIDE96(XSTA,XSUN,XMON,T(L,NSTA),XPOL,YPOL,
CC     1              IRCOCN,OCNAMP(1,1,NSTA),OCNPHS(1,1,NSTA),DXTIDE)
          CALL TIDE2000(STANAM,XSTA,XSUN,XMON,T(L,NSTA),XPOL,YPOL,MPOL,
     1                  IRCOCN,OCNAMP(1,1,NSTA),OCNPHS(1,1,NSTA),
     2                  IRCATM,ATMSIN(1,1,NSTA),ATMCOS(1,1,NSTA),DXTIDE)
          DO 40 I=1,3
            DR(I,L,NSTA)=DXTIDE(I)
40        CONTINUE
        END IF
50    CONTINUE
C
C CMC correction if desired
C -------------------------
      cmc = 0D0
      IF (PRESENT(cmcyn)) THEN
        CALL getcmc(cmcyn,TDT,cmc)
      ENDIF
C
C INTERPOLATION
C -------------
      DO 60 I=1,3
        DRABS(I) = ((DR(I,2,NSTA)-DR(I,1,NSTA))*(TOBS-T(1,NSTA)))
     1             /DELTAT + DR(I,1,NSTA)
        XSTEPO(I) = XSTA(I)+DRABS(I)+cmc(I)
60    CONTINUE
C
C HARD-WIRED VELOCITY REDUCTION FOR AMUNDSEN-SCOTT (AMUN) STATION
C ---------------------------------------------------------------
      IF (STANAM(1:14).EQ.'AMUN 66040M001' .OR.
     1    STANAM(1:14).EQ.'AMU2 66040M002') THEN
        XFAC=(DMOD(TOBS,1.D0)-0.5D0)/365.25D0
        XSTEPO(1)=XSTEPO(1)+7.6849D0*XFAC
        XSTEPO(2)=XSTEPO(2)-6.4056D0*XFAC
        XSTEPO(3)=XSTEPO(3)+0.1496D0*XFAC
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
