      MODULE s_PRANGE
      CONTAINS

C*
      SUBROUTINE PRANGE(ISTA  ,TOBS  ,DELTAT,SECIPL,STAT  ,WGS   ,
     1                  SVN   ,XVSAT ,SZ    ,XPOL  ,YPOL  ,ISATCO,
     2                  ITROPO,IEXTRA,XSTELL,MEATYP,ICARR ,IORSYS,
     3                  TOPPOS,ZENMAX,NADMAX,DIST  ,CLOCKC,ZEN   ,
     4                  AZI   ,ZENION,NDIFF ,RNXCLK,IRCCLK,IRCPCV,
     5                  NAD   ,AZISAT,AZISOK,ANTTYP,IANTEN,CSESS ,
     6                  IREL2)
CC
CC NAME       :  PRANGE
CC
CC PURPOSE    :  COMPUTE PSEUDORANGE OF ONE SATELLITE-RECEIVER PAIR
CC               IN BERNESE GPS SOFTWARE VERSION 3
CC
CC PARAMETERS :
CC         IN :  ISTA   : STATION INDEX (FIRST/SECOND)        I*4
CC               TOBS   : OBSERVATION TIME                    R*8
CC               DELTAT : CORRECTION FROM NOMINAL EPOCH       R*8
CC                        "TOBS" TO EPOCH IN GPS-TIME (SEC)
CC               SECIPL : MAX INTERVAL FOR CLK INTERPOLATION  R*8
CC               STAT   : STATION NAME                        CH*16
CC               WGS(K),K=1,2,3: WGS POSTION OF RECEIVER      R*8
CC               SVN    : SATELLITE NUMBER                    I*4
CC               XVSAT(K),K=1,2,3,..: POSITION, VELOCITY,..   R*8
CC                        OF SATELLITE
CC               SZ     : TRUE SIDERAL TIME                   R*8
CC               XPOL   : X-COORDINATE OF POLE                R*8
CC               YPOL   : Y-COORDINATE OF POLE                R*8
CC               ISATCO : ISATCO=0: NO SATELLITE CLOCK CORR.  I*4
CC                        ISATCO=1: SAT. CLOCK CORRECTIONS
CC                                  TO BE APPLIED
CC               ITROPO : TROPOSPHERE INDEX                   I*4
CC               IEXTRA : ATMOSPHERE MODEL INDEX              I*4
CC               XSTELL(J),J=1..3: ELLIPSOIDAL STATION        R*8
CC                         COORDINATES (LAT/LON IN RAD,
CC                         HEIGHT IN M)
CC               MEATYP : MEASUREMENT TYPE                    I*4
CC               ICARR  : CARRIER INDEX                       I*4
CC               IORSYS : ORBIT SYSTEM                        I*4
CC                        =1: B1950.0
CC                        =2: J2000.0
CC               NDIFF(I):I=1,..,NFTOT: DIFFERENCE TYPE       I*4
CC                        NDIFF=0: ZERO DIFFERENCE
CC                        NDIFF=1: SINGLE DIFFERENCE
CC               RNXCLK:  WHAT TO DO IF NO INPUT CLOCK RINEX  I*4
CC                        FOR SATELLITE CLOCKS:
CC                         -1: IGNORE CLOCK RINEX FILE
CC                          0: TRY ALSO SAT CLK FILE
CC                          1: USE OBS. (INTERPOL. CLK RNX)
CC                          2: SKIP OBS.
CC                          3: USE OBS. (SAT CLK = ZERO)
CC               ZENMAX : MAXIMUM ZENITH DISTANCE (RAD)       R*8
CC               NADMAX : MAXIMUM NADIR  ANGLE    (RAD)       R*8
CC               ANTTYP : RECEIVER ANTENNA NAME               CH*20
CC               IANTEN : RECEIVER ANTENNA NUMBER             I*4
CC               CSESS  : SESSION ID                          CH*4
CC               IREL2  : FLAG FOR PERIODIC RELATIVISTIC J2   I*4
CC                        CORRECTION
CC        OUT :  TOPPOS(K),K=1,2,3: TOPOC. POSITION OF SAT.   R*8
CC               DIST   : RESULTING DISTANCE                  R*8
CC               CLOCKC : APRIORI SATELLITE CLOCK CORRECTION  R*8
CC               ZEN    : ZENITH DISTANCE (RADIAN)            R*8
CC               AZI    : AZIMUTH ANGLE (FROM NORTH TO EAST,  R*8
CC                        IN RADIAN)
CC               IRCCLK : RETURN CODE SAT. CLOCK CORRECTION   I*4
CC                        =0: OK
CC               IRCPCV : RETURN CODE PCV CORRECTION          I*4
CC                        =0: OK
CC               NAD    : NADIR ANGLE (RECEIVER SEEN FROM     R*8
CC                        THE SATELLITE; RADIAN)
CC               AZISAT : AZIMUTH ANGLE (RECEIVER SEEN FROM   R*8
CC                        THE SATELLITE; RADIAN)
CC               AZISOK : CORRECTED AZIMUTH ANGLE (RECEIVER   R*8
CC                        SEEN FROM THE SATELLITE; ANGLE
CC                        COUNTS CLOCKWISE FROM THE Y-AXIS
CC                        WHEN LOOKING TOWARDS THE Z-AXIS;
CC                        RAD)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/10/16 09:09
CC
CC CHANGES    :  18-FEB-93 : ??: USE OF DUMMY1,DUMMY2,DUMMY3
CC               08-APR-94 : MR: ADD AZIMUTH ANGLE TO PARAMETER LIST;
CC                               CHANGE CALL OF TOPSTA
CC               29-JUL-94 : MR: ADD SAT.NUMBER TO PARAMETER LIST
CC               05-DEC-95 : SS: NEW IONOSPHERE MODEL (TYPE 2)
CC               05-DEC-95 : SS: "IONO" REMOVED (SEE SR CHKION)
CC               05-DEC-95 : SS: CALL OF SR IONOSP WITH "IORSYS"
CC               26-MAR-96 : TS: ADDED NDIFF FOR ZERO/SINGLE DIFFERENCES
CC               08-APR-97 : SS: NIELL MAPPING, TROPOSPHERE GRADIENTS
CC               14-AUG-97 : SS: GEOMETRY-FREE LC OF ZERO DIFFERENCES
CC               25-NOV-97 : SS: RETURN "IRCCLK"
CC               26-JAN-98 : SS: STATION-SPECIFIC GIMS
CC               04-MAY-98 : SS: PASS "SVN"
CC               05-AUG-99 : SS: PASS "ZENMAX" TO SR IONOSP
CC               27-JAN-00 : TS: "DIRTY" TRICK FOR MARINNI MURRAY WL
CC               02-FEB-00 : RD: APRIORI VALUES FOR REC-CLOCK ESTIMATION
CC                               IN DELTAT(2,*) ARE HANDELD CORRECT
CC               23-FEB-01 : DS: SR:STAFLG - HANDLE STATION TYPES
CC               23-FEB-01 : DS: SR:TOPLEO
CC               14-NOV-02 : RS: ADD NADIR ANGLE
CC               23-JUN-03 : HB: INTERFACE FOR SR STAFLG
CC               11-AUG-03 : RS: ADD NAD AND AZISAT TO CALL OF LEOSKY
CC               29-MAR-04 : CU: GET WAVELENGTH FOR RANGE MEASUR. (SR GETWAV),
CC                               ADD WL TO CALL OF SR METEO
CC               07-APR-04 : RS: ADD TOBS, SVN, IORSYS AND AZISOK TO
CC                               CALL OF TOPSTA, RETURN AZISOK
CC               10-MAY-04 : DS: PASS SVN NUMBER, IORSYS and AZISOK TO LEOSKY
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               13-DEC-05 : CU: ADAPT CALL OF SR TROPOS
CC               29-JUN-06 : HB: ADD A PRIORI SATELLITE CLOCK CORRECTION TO
CC                               PARAMETER LIST
CC               17-AUG-06 : HU: USE FOR S_LEOSKY
CC               12-JUN-07 : AG: APPLY PCV SHIFTED INTO PRANGE
CC               01-NOV-07 : HB: ADD PARAMETER SECIPL
CC               05-DEC-07 : HB: ADD PARAMETERS NADMAX AND IRCPCV
CC                               MODIFICATIONS FOR LEO PROCESSING
CC               31-JUL-08 : DT: REMOVE UNUSED VARIABLES (IDUM)
CC               01-OCT-08 : HB: ADD PARAMETER ZENION
CC               11-NOV-08 : DT: CALL GETWAV FOR TROP.MODEL MENDES-PAVLIS
CC               29-MAY-09 : RD: INPUT CLOCKS ALSO FROM INPUT CLK RNX FILE
CC               13-AUG-09 : DT: APPLY APRIORI RANGE BIASES
CC               28-SEP-09 : DT: COM FOR SLR FROM GETRGB INSTEAD OF SLR_PCV
CC               04-JAN-10 : SL: HOI PARAMETER ADDED TO IONOSP CALL
CC               26-OCT-10 : CR: ADD SWITCH (IREL2) FOR PERIODIC RELATIVISTIC
CC                               J2-CORRECTION
CC               17-NOV-11 : HB: ALLOW FOR SLR PCVS
CC               24-NOV-11 : RD: REMOVE UNUSED VARIABLE "TIME"
CC               29-FEB-12 : RD: USE METEO AS MODULE
CC               20-AUG-12 : SL: SHAPE OF METEO PARAMETER CHANGED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE m_epoch,  ONLY: t_epoch, OPERATOR(.RealToEpoch.)
      USE d_stacrx, ONLY: MTypeSPACE
      USE d_const,  ONLY: C
      USE d_satfil, ONLY: typeMWTR, typeSLR
      USE d_phaecc, ONLY: sta_pcv, sat_pcv
      USE d_rgbfil, ONLY: t_rgb

      USE s_getwav
      USE s_tropos
      USE s_ionosp
      USE s_leosky
      USE s_topsta
      USE s_meteo
      USE s_gtsclk
      USE s_staflg
      USE s_getrgb

      USE f_dgpsut

      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 ICARR , IEXTRA, IFLAG , IORSYS, IRCCLK, ISATCO, ISTA  ,
     1          ITROPO, MEATYP, IRCPCV, RNXCLK
C
      REAL*8    AZI   , CLOCKC, COSZZZ, DELTA , DELTA1, DELTA2, DELTAT,
     1          DH    , DIST  , DL    , DRMET , DRZEN1, DRZEN2, E1    ,
     2          E2    , FACZEN, SECIPL, SINZZZ, SZ    , TOBS  , XPOL  ,
     3          YPOL  , ZEN   , ZENMAX, ZENMEA, ZZZ   , NADMAX
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      CHARACTER*16 STAT,MET_STAT(1)
      CHARACTER*20 MARTYP
      CHARACTER*20 ANTTYP
      CHARACTER*4  CSESS
      CHARACTER*2  FRQID

      CHARACTER(LEN=staNam2Length) :: STAT2
C
      INTEGER*4    SVN,NDIFF,IANTEN,IREL2
      INTEGER*4    corFound, rgbWL
C
      REAL*8       WGS(*),XVSAT(*),TOPPOS(*)
      REAL*8       ZENRES(2),HGTRES(2),PRES(2),TRES(2),HUMRES(2)
      REAL*8       DUMMY1(2),DUMMY2(2),DUMMY3(2),XSTELL(3)
      REAL*8       NAD,AZISAT,AZISOK,DUMMY,DUM3(3)
      REAL*8       TA1,TA2,NM1,NM2
      REAL*8       WL
      REAL*8       ELVCOR,ZENION
      REAL*8       HOI(3)
      REAL*8       MET_XSTELL(3,1),MET_ZEN(1),MET_AZI(1),MET_DRMET(1)
C
      TYPE(t_rgb)   :: rgbReq
      TYPE(t_epoch) :: epoch
C
C INIT
C ----
      WL = 0.D0
      ELVCOR = 0.d0
      DELTA  = 0D0
      HOI(:) = 0D0
      STAT2  = ''
C
C HANDLING STATION TYPES
C -----------------------
      CALL STAFLG(STAT,TOBS,IFLAG,MARTYP)
C
C COMPUTE TOPO(LEO)-CENTRIC DISTANCE
C ----------------------------------
      IF (MARTYP.EQ.MTypeSPACE) THEN
        CALL LEOSKY(STAT,TOBS,0,0,WGS,XVSAT,SZ,DELTAT,XPOL,YPOL,MEATYP,
     1              TOPPOS,DIST,ZEN,AZI,NAD,AZISAT,SVN,IORSYS,AZISOK)
        CALL LEOSKY(STAT,TOBS,1,0,WGS,XVSAT,SZ,DELTAT,XPOL,YPOL,MEATYP,
     1              DUM3,DUMMY,ZENION,DUMMY,DUMMY,DUMMY,SVN,IORSYS,
     2              DUMMY)
      ELSE
        CALL TOPSTA(WGS,XVSAT,SZ,DELTAT,XPOL,YPOL,TOPPOS,DIST,ZEN,AZI,
     1              MEATYP,NAD,AZISAT,TOBS,SVN,IORSYS,AZISOK,IREL2)
        ZENION=ZEN
      END IF
      IF (ICARR.EQ.4) DIST=0.D0
C
C APPLY IONOSPHERIC REFRACTION CORRECTION
C ---------------------------------------
      CALL IONOSP(WGS   ,XVSAT ,TOBS  ,SZ    ,ZENION,ICARR ,
     1            IORSYS,STAT  ,SVN   ,ZENMAX,DELTA ,
     2            MEATYP,AZI   ,HOI   ,XSTELL)
C
      IF (MEATYP.EQ.1) THEN
        DIST=DIST+DELTA
      ELSE IF (MEATYP.EQ.2) THEN
        DIST=DIST-DELTA
      END IF
C
C ELEVATION-DEPENDENT RECEIVER ANTENNA PHASE CENTER CORRECTION
      IRCPCV=0
      IF (ZEN <= ZENMAX) THEN
        CALL STA_PCV(ANTTYP,IANTEN,STAT,SVN,ICARR,CSESS,ZEN,AZI,ELVCOR)
        DIST=DIST + ELVCOR
      ELSE
        IRCPCV=1
      ENDIF
C
C ELEVATION-DEPENDENT SATELLITE ANTENNA PHASE CENTER CORRECTION
      IF (NAD <= NADMAX) THEN
        IF (MEATYP == 3) THEN
          CALL SAT_PCV(SVN,TOBS,typeSLR,ICARR,NAD,AZISAT,ELVCOR)
        ELSE
          CALL SAT_PCV(SVN,TOBS,typeMWTR,ICARR,NAD,AZISAT,ELVCOR)
        ENDIF
        DIST=DIST + ELVCOR
      ELSE
        IRCPCV=1
      ENDIF
C
C RETURN IF GEOMETRY-FREE LC
C --------------------------
      IF (ICARR.EQ.4) RETURN
C
C APPLY TROPOSPHERIC REFRACTION CORRECTION
C ----------------------------------------
      IF (MARTYP.EQ.MTypeSPACE) GOTO 50
C
C (A) HOPFIELD OR SAASTAMOINEN
      IF (ITROPO.NE.3) THEN
C
C       MARINI-MURRAY or MENDES-PAVLIS
        IF (ITROPO.EQ.4 .OR. ITROPO.EQ.8) THEN
          IF (MEATYP.EQ.1) FRQID(1:1) = 'L'
          IF (MEATYP.EQ.2) FRQID(1:1) = 'P'
          IF (MEATYP.EQ.3) FRQID(1:1) = 'R'
          WRITE(FRQID(2:2),'(I1)') ICARR
          CALL GETWAV(MEATYP,STAT,FRQID,TOBS,WL)
        ENDIF
C
        MET_STAT(1)       = STAT
        MET_XSTELL(1:3,1) = XSTELL(1:3)
        MET_ZEN(1)        = ZEN
        MET_AZI(1)        = AZI
        CALL METEO(0,ITROPO,IEXTRA,1,MET_STAT,SVN,
     1             TOBS,MET_XSTELL,MET_ZEN,MET_AZI,WL,
     2             MET_DRMET,DUMMY1,DUMMY2,DUMMY3)
        DRMET=MET_DRMET(1)
        DIST=DIST+DRMET
C
      ELSE
C
C (B) ESSEN-FROOME DIFFERENTIAL MODELING
        ZENRES(ISTA)=ZEN
        HGTRES(ISTA)=XSTELL(3)

        MET_STAT(1)       = STAT
        MET_XSTELL(1:3,1) = XSTELL(1:3)
        MET_ZEN(1)        = ZEN
        MET_AZI(1)        = AZI
        CALL METEO(0,1,IEXTRA,1,MET_STAT,SVN,
     1             TOBS,MET_XSTELL,MET_ZEN,MET_AZI,0.D0,MET_DRMET,
     2             PRES(ISTA),TRES(ISTA),HUMRES(ISTA))
        DRMET=MET_DRMET(1)
C
C ESSEN-FROOME IF ONLY TROPOSPHERIC ZENITH CORRECTIONS AVAILABLE
        IF(PRES(ISTA).EQ.0)THEN
          IF(ISTA.EQ.1)THEN
            DRZEN1=DRMET*DCOS(ZEN)
            GO TO 20
          ELSE
C
C TAKE INTO ACCOUNT LAYERS ABOVE SECOND STATION
            DH=HGTRES(2)-HGTRES(1)
            FACZEN=6378000.D0/(6378000.D0+DH)
            SINZZZ=FACZEN*DSIN(ZENRES(1))
            COSZZZ=DSQRT(1.D0-SINZZZ**2)
            ZZZ=DATAN(SINZZZ/COSZZZ)
            ZENMEA=(ZENRES(1)+ZZZ)/2
            DRZEN2=DRMET*DCOS(ZEN)
            DRMET=-((DRZEN1-DRZEN2)/DCOS(ZENMEA)+
     1              DRZEN2/DCOS(ZZZ)-DRZEN2/DCOS(ZEN))
            DIST=DIST+DRMET
            GO TO 20
          END IF
        END IF
C
C ESSEN-FROOME IF TEMP, PRESS AND HUMIDITY ARE AVAILABLE
C ------------------------------------------------------
        DRMET=0.D0
        IF(ISTA.EQ.2)THEN
C
C PATH LENGTH IN LAYER BETWEEN THE TWO STATIONS
          DL=(HGTRES(2)-HGTRES(1))/DCOS(ZENRES(1))
          TA1=TRES(1)+273.15D0
          TA2=TRES(2)+273.15D0
C
C WATER VAPOR PRESSURE
          E1=HUMRES(1)/100.D0*
     1       EXP(-37.2465D0+0.213166D0*TA1-0.256908D-3*TA1*TA1)
          E2=HUMRES(2)/100.D0*
     1       EXP(-37.2465D0+0.213166D0*TA2-0.256908D-3*TA2*TA2)
C
C FORMULA BY ESSEN AND FROOME
          NM1=(77.64D0*PRES(1)/TA1-
     1         12.96D0*E1/TA1+371780.D0*E1/TA1**2)/1.D6
          NM2=(77.64D0*PRES(2)/TA2-
     1         12.96D0*E2/TA2+371780.D0*E2/TA2**2)/1.D6
C
C DIFFERENTIAL CORRECTION IN DISTANCE DR
          DRMET=(NM1+NM2)/2*DL
C
C TAKE INTO ACCOUNT LAYERS ABOVE SECOND STATION
          DH=HGTRES(2)-HGTRES(1)
          FACZEN=6378000.D0/(6378000.D0+DH)
          SINZZZ=FACZEN*DSIN(ZENRES(1))
          COSZZZ=DSQRT(1.D0-SINZZZ**2)
          ZZZ=DATAN(SINZZZ/COSZZZ)
          CALL TROPOS(ZZZ,(/0d0,0d0,HGTRES(2)/),TRES(2),PRES(2),
     1                HUMRES(2),1,0.D0,DELTA1)
          CALL TROPOS(ZENRES(2),(/0d0,0d0,HGTRES(2)/),TRES(2),PRES(2),
     1                HUMRES(2),1,0.D0,DELTA2)
          DRMET=-(DRMET+DELTA1-DELTA2)
          DIST=DIST+DRMET
        END IF
      END IF
20    CONTINUE
50    CONTINUE
C
C SATELLITE CLOCK CORRECTION
C --------------------------
      IF(ISATCO.EQ.1 .AND. MEATYP.NE.3) THEN
        CALL GTSCLK(RNXCLK,TOBS,SVN,SECIPL,2,CLOCKC,IRCCLK)
        DIST=DIST-CLOCKC*C
      ELSE
        IRCCLK=0
      ENDIF
C
C SLR data corrections: Range bias and CoM
C ----------------------------------------
      IF (MEATYP == 3) THEN

        rgbWL = 1
        epoch = .realToEpoch.TOBS

        STAT2(1:16) = STAT

C Range bias correction
        CALL GETRGB('RGB', STAT2, SVN, rgbWL, epoch, rgbReq, corFound)

        IF ( corFound==0 ) THEN
          DIST = DIST + rgbReq%value
          write(lfnprt,*) 'apriori RGB: ',rgbReq%staNam, rgbReq%satNum,
     1                                    rgbReq%value
        END IF

C Center-of-Mass for spherical satellites
        CALL GETRGB('COM', STAT2, SVN, rgbWL, epoch, rgbReq, corFound)

        IF ( corFound==0 ) THEN
          DIST = DIST - rgbReq%value
        END IF

      ENDIF
C
C IT MIGHT BE MORE CORRECT TO USE: TOBS=TOBS+(DELTAT-DIST/C)/86400D0
C WHEN CALLING GTSCLK ?!
C
C RECEIVER CLOCK CORRECTION IN CASE OF ZERO DIFFERENCE
C ----------------------------------------------------
CCC THIS IS DONE IN GOBSEP NOW...
CCC      IF(NDIFF.EQ.0 .AND. MEATYP.NE.3) THEN
CCC        DIST=DIST-DELTAT*C
CCC      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
