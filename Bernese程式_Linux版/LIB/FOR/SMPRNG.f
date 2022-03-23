      MODULE s_SMPRNG
      CONTAINS

C*
      SUBROUTINE SMPRNG(STATIO,XSTAT0,PLH,SECIPL,NSAT,SVN,
     1                  ITROPO,IEXTRA,NCLOCK,CLOCK,IONO,XSTION,OBSWIN,
     2                  RECTYP,ANTTYP,IANTEN,CSESS,INTER,IELEV,MXOSVN,
     3                  IREL2,NEPO,PRANG,ZENDIS,NOBSVN,IX,
     4                  NFRQPH,XSTELL)
CC
CC NAME       :  SMPRNG
CC
CC PURPOSE    :  SIMULATE PSEUDODIFFERENCES FOR ONE STATION
CC               AND ONE SESSION. SEPARATE VALUES FOR PHASE AND
CC               CODE, AND FOR L1 AND L2 ARE GENERATED
CC
CC PARAMETERS :
CC         IN :  STATIO : STATION NAME                        CH*16
CC               XSTAT0(K,L),K=1,2,3,L=1,2: EARTH FIXED R*8
CC                        STATION COORD., FREQUENCY L
CC               PLH(I,L),I=1,3,L=1,2: PHI, LAMBDA, H OF      R*8
CC                        STATION, FREQUENCY L
CC               SECIPL : MAX ALLOWED CLOCK INTERPOL INTERVAL R*8
CC               NSAT   : ACTUAL NUMBER OF SATELLITES         I*4
CC               SVN(I),I=1,2,..,NSAT: SVN-NUMBERS            I*4
CC               ITROPO  : MODEL INDEX                        I*4
CC                     = 0: NO CORRECTION
CC                     = 1: SAASTAMOINEN
CC                     = 2: HOPFIELD
CC                     = 3: ESSEN AND FROOME
CC                     = 4: MARINI MURRAY (SLR)
CC                     = 5: SAASTAMOINEN WITH NIELL DRY MAPPING
CC                     = 6: GMF
CC                     = 7: VMF
CC                     =11: SAASTAMOINEN DRY DELAY ONLY
CC                     =12: HOPFIELD DRY DELAY ONLY
CC                     =15: SAASTAMOINEN DRY WITH NIELL DRY MAPPING
CC                     =16: GMF DRY DELAY ONLY
CC                     =17: VMF DRY DELAY ONLY
CC               IEXTRA  : METEO DATA SOURCE INDICATOR        I*4
CC                     =0: USE REAL DATA FROM EXTERNAL FILES
CC                     =1: USE VALUES FROM ATMOSPHERIC MODEL
CC                     =2: USE ESTIMATED VALUES IN BERNESE
CC                         TRP-FORMAT (FROM GPSEST OR ADDNEQ)
CC               NCLOCK : NUMBER OF CLOCK TERMS               I*4
CC               CLOCK(I),I=1,2,.: CLOCK PARAMTERS            R*8
CC               IONO   : IONOSPHERE PARAMETERS               I*4
CC               XSTION(K),K=1,2,3 COORDINATES FOR IONOSPHERE R*8
CC                        REFERENCE STATION
CC               OBSWIN : FIRST/LAST OBSERVATION TIME (MJD)   t_timint
CC               RECTYP : RECEIVER TYPE                      CH*20
CC               ANTTYP : ANTENNA TYPE                       CH*20
CC               IANTEN : ANTENNA NUMBER                      I*4
CC               CSESS(I),I=1,2: SESSION IDENTIFICATIONS     CH*4
CC               INTER  : INTERVAL BETWEEN SUBSEQUENT OBSER-  I*4
CC                        VATIONS (SEC)
CC               IELEV  : MINIMUM ELEVATION (DEGREES)         I*4
CC               MXOSVN : MAXIMUM NUMBER OF OBSERVED SAT.     I*4
CC               IREL2  : FLAG FOR PERIODIC RELATIVISTIC J2   I*4
CC                        CORRECTION
CC        OUT :  NEPO   : NUMBER OF OBSERVATION EPOCHS        I*4
CC               PRANG(I,L,K,CP),I=1,2,..,MAXEPO,L=1,2,       R*8
CC                        K=1,2,..,MAXSAT, CP=1,2:
CC                        OBSERVED PSEUDORANGES FOR CARRIER L
CC                        AND CP=1(CODE), CP=2(PHASE)
CC               ZENDIS(I,J),I=1,..,MAXEPO,J=1,..,MAXSAT:     R*8(*,*)
CC                        ZENITH DISTANCES IN RAD
CC               NOBSVN(I),I=1,2,...,NSAT: NUMBER OF OBSERVED I*4
CC                        PSEUDORANGES
CC      IN/OUT:  IX     : RANDOM NUMBER                       I*4
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  88/02/23 18:49
CC
CC CHANGES    :  04-JUN-92 : ??: NEW PARAMETERS IN "GETORB","COOTRA",
CC                               "XYZTIM" AND "SNDIFF" FOR OPTION
CC                               SYSTEM J2000.0
CC               03-MAR-93 : ??: NEW PARAMETERS "RECTYP","ANTTYP","IANTEN"
CC               02-NOV-93 : ??: RETURN CODE IN CALL TO SR BRCEPH
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               08-APR-94 : MR: ADD AZIMUTH DEPENDENT PHASE CORR.
CC               10-AUG-94 : MR: CALL EXITRC
CC               29-SEP-95 : JJ: CALL TO OPNERR FIXED
CC               26-MAR-96 : MR: ADD "CSESS" AS PARAMETER, ALSO TO
CC                               CALL GPHECC
CC               14-JAN-97 : TS: CALL OF TOPSTA CORRECTED
CC               11-FEB-97 : MR: USE "GTSCLK" TO GET SAT. CLOCKS,
CC                               REMOVE CALL TO "BRCEPH"
CC               13-MAY-97 : MR: REMOVE UNUSED VARIABLE "DUMMY"
CC               23-SEP-97 : DI: USE MAXSAT.inc
CC               23-OCT-97 : SS: ELEVATION-DEPENDENT SIGMAS
CC               05-AUG-99 : SS: USE SAME CORE MODELING MODULE (SR
CC                               PRANGE) AS USED BY GPSEST
CC               07-AUG-99 : MR: ADD GLONASS FREQUENCIES
CC               13-APR-00 : SS: ESTIMATE (P1-C1) CODE BIASES
CC               22-JUN-00 : RD: INCREASED MAXEPO (OLD=4000)
CC               22-JUN-00 : RD: NO ARC FOUND FOR A SATELLITE ->
CC                               WARNING NOT ERROR
CC               13-SEP-00 : RD: INCREASED MAXEPO (OLD=5000)
CC               30-JAN-01 : DS/MR: LEO SIMULATION
CC               05-DEC-01 : DS: ADDED KIN ORBIT AS LEO ORBIT
CC               15-JUN-01 : HB: rename d_stacrux in d_stacrx
CC               23-FEB-02 : HU: LIMIT NUMBER OF OBSERVED SATELLITES
CC               07-MAY-02 : SS: DCB UPDATE
CC               27-JUN-02 : RD/DS: MERGING VERSION BE AND MUNICH
CC               30-JUN-02 : DS: COMPUTE CORRECT NUMBER OF EPOCHS
CC               13-SEP-02 : MR: REMOVE METEO FILE SIMULATION
CC               10-OCT-02 : MR: ADJUST COMPUTATION OF NEPO
CC               04-DEC-02 : RS: CALL OF PRANGE MODIFIED
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               09-MAR-03 : DS: REMOVE LFNKIN, LFNVEL
CC               28-MAY-03 : RD: NEW CALL OF SR GPHECC
CC               29-MAY-03 : DS: VELOCITY TRANSFORMATION w.r.t. LEOSKY
CC               23-JUN-03 : HB: INTERFACE FOR SR STAFLG
CC               11-AUG-03 : RS: ADD CALL OF GTSATA, APPLY SATELLITE ANTENNA
CC                               PHASE CENTER VARIATIONS, CHANGE CALL OF
CC                               LEOSKY, USE M_BERN, RECTYP=' ' IN 3 CALLS
CC                               OF GPHECC
CC               08-SEP-03 : HU: ANTNAM, RECNAM CHR16 -> CHR20
CC               24-NOV-03 : HU: CHECK IONO ON/OFF FLAG
CC               15-JAN-04 : HU: HANDLING OF IONOSPHERE IMPROVED
CC               08-APR-04 : RS: ADD AZISOK TO CALL OF PRANGE
CC               10-MAY-04 : DS: PASS SVN NUMBER AND IORSYS TO LEOSKY
CC               18-JUN-04 : DS: CALL GPHECC WITH ICARR
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-JUN-05 : MM: UNUSED VARIABLES REMOVED
CC               08-Aug-05 : HB: Use new SR TIMST2 (module)
CC               26-Aug-05 : HB: Apply polarization effect
CC               09-NOV-05 : AG: SENNUM for GTSATA & GPHECC CALLs ADDED
CC               29-JUN-06 : HB: SATCLK AS PARAMETER FOR SR PRANGE
CC               18-JUL-06 : AG: CMC ADDED
CC               17-Aug-06 : HU: USE FOR S_LEOSKY
CC               29-AUG-06 : HB: Test if maximum nadir angle exceeded
CC               14-NOV-06 : HU: New parameter (ipolar) for POLARI
CC                               and POLARLEO
CC               12-Jun-07 : AG: USE D_PHAECC INSTEAD OF GPHECC
CC               01-NOV-07 : HB: New parameter secIpl added for GTSCLK
CC               05-DEC-07 : HB: New parameter nadmax,ircpcv for PRANGE
CC               03-MAR-08 : HB: Bug corrected (reading CRD-file)
CC               01-OCT-08 : HB: Add ZENION as parameter to SR PRANGE
CC               04-MAY-09 : RD: CONSIDER VIENNA GRID FILES IN XYZTIM
CC               04-MAY-09 : RD: CHANGE OPT%TFIRST/TLAST TO OPT%OBSWIN
CC               09-MAY-09 : RD: NEW CALL OF DCBCOR
CC               29-MAY-09 : RD: NEW CALL OF PRANGE
CC               30-SEP-09 : RD: CHANGE VARIABLE NAME ERROR->RCLK
CC               09-MAR-10 : SL: BUG FIX WRT GRID FILE CORRECTION
CC               03-DEC-10 : RD: CMC FOR ATL ADDED
CC               21-DEC-10 : LP: CMC CORRECTION FOR LEOS INTRODUCED (SR CMC_LEO)
CC               03-FEB-11 : CR: ADD SWITCH (IREL2) FOR PERIODIC RELATIVISTIC
CC                               J2-CORRECTION
CC               21-SEP-11 : RD: USE THE CORRECT COORDINATE ARRAY
CC               11-NOV-11 : MM: TAKE SATELLITE PCOS FROM PCV FILE
CC               05-MAR-12 : RD: USE LISTI4 AS MODULE NOW
CC               05-MAR-12 : RD: REMOVE UNUSED VARIABLES
CC               17-JAN-13 : RD: CORRECT COMPUTATION OF "NEPO"
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnerr, lfnloc
      USE m_time,   ONLY: t_timint
      USE d_stacrx, ONLY: MTypeSPACE
      USE d_satfil, ONLY: typeMWTR
      USE m_maxdim, ONLY: MAXSAT
      USE d_const,  ONLY: C, PI
      USE d_phaecc, ONLY: sta_off,sat_off
      USE d_grid,   ONLY: getGrid,getGridKeyw,grdNeq
C
      USE s_ellecc
      USE s_opnfil
      USE s_cootra
      USE s_prange
      USE s_leoante
      USE s_randu
      USE s_getorb
      USE s_truearth
      USE s_opnerr
      USE s_dcbcor
      USE s_geotop
      USE s_maxtst
      USE s_staflg
      USE s_timst2
      USE s_readvel
      USE s_xyztim
      USE s_getdat
      USE s_readkin
      USE s_ionos1
      USE s_exitrc
      USE s_gtleoco
      USE f_listi4
      USE s_leosky
      USE s_gtflna
      USE s_xyzell
      USE s_polari
      USE s_polarleo
      USE s_cmc_leo
      USE s_gnssatti
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I      , IANTEN , ICARR  , ICP    , ICRARC , IELEV  ,
     1          IEPO   , IEXTRA , IFRQ   , II     , ILST   , IRCPCV ,
     2          INIT   , INTER  , IORSYS , IOSTAT , IRC    , IRC1   ,
     3          IRC2   , IRCCLK , IRCEPO2, IRCKIN , IRCODE , NFRQPH ,
     4          IRCSTD , IRCVEL , IRCVEL2, ISATCO , ISAT   , JJ     ,
     5          ISTA   , ITROPO , IX     , IY     , MAXEPO ,
     6          MEATYP , MXCEPO , MXCSAT , MXOSVN , NCLOCK , NDIFF  ,
     7          NEPO   , NSAT   , NSVNEW , NSVOLD , NPOLAR , IREL2  ,
     8          K      , IRCEPO
C
      REAL*8    AZI    , COSAZ  , DAY    , DELTA  , DELTAT , RHLP   ,
     1          DION   , ERRDFR , RCLK   , FACION , FACRMS ,
     2          PLREFF , SECIPL , SINAZ  , T      ,
     3          TOSC   , TREL   , UT1GPS , ZEN    , ZENION , ZENMAX ,
     4          DPOLAR , NADMAX
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
C LOCAL MAXIMUM DIMENSIONS
C
      PARAMETER (MAXEPO=21000)
C
C DECLARATIONS
      TYPE(t_timint) :: OBSWIN
C
      REAL*8       XSTAT(9,2),PRANG(MXCEPO,2,MXCSAT,2),CLOCK(*),PLH(3,2)
      REAL*8       XSAT(9),ELE(7),XSTEPO(9,2),XSTAT0(3,2),XSTSAT(9,2)
      REAL*8       XTOP(3),XTOP2(3),XSTION(3),EXC(3)
      REAL*8       DXTOP(3),POLAR(3)
      REAL*8       NIGHT,ELEIRR(2,MAXEPO),DE(2),POLARS(MAXSAT)
      REAL*8       ZENDIS(MXCEPO,*),ANTECC(3),TRUOFF(3)
      REAL*8       XLEO(9),SZ,XPOL,YPOL
      REAL*8       AELL,BELL,DXELL(3),DRELL(3),SCELL,ZEN2,AZI2
      REAL*8       XSTAT2(9,2),XSTELL(3),gridld(3),grdxyz(3)
      REAL*8       XKIN(3),XVEL(3),XTMP(9),XKV(6),XDUMMY(3)
      REAL*8       NAD,AZISAT,NAD2,AZISA2,DUMMY,AZISOK,AZISO2
      REAL*8       SATCLK(MAXSAT)
      REAL*8       XSATEP(9,2),OFFSET(3),EX(3),EY(3),EZ(3)
C
      INTEGER*4    SVN(*),NOBSVN(*),IONO(4),SVNOLD(MAXSAT)
      INTEGER*4    IFLAG
C
      CHARACTER*32 FILNAM,FILKIN,FILVEL
      CHARACTER*32 COOFIL
      CHARACTER*20 MARTYP
      CHARACTER*20 RECTYP,ANTTYP
      CHARACTER*16 STATIO,DATUM
      CHARACTER*6  MXNEPO,MXNSAT
      CHARACTER*4  CSESS(2)
      CHARACTER*1  CDUMMY
C
      LOGICAL      cmcyn(2), cmcyn_LEO(2)
C
      COMMON/CSMPRN/ELEIRR
      INCLUDE 'COMFREQ.inc'
      COMMON/MCMEPO/MXCEPO,MXNEPO
      COMMON/MCMSAT/MXCSAT,MXNSAT
      DATA INIT/1/,NSVOLD/0/
C
C CHECK MAXIMUM DIMENSION
C -----------------------
      CALL MAXTST(1,'SMPRNG',MXNSAT,MAXSAT,MXCSAT,IRC1)
      CALL MAXTST(1,'SMPRNG',MXNEPO,MAXEPO,MXCEPO,IRC2)
      IF (IRC1+IRC2.NE.0) CALL EXITRC(2)
C
C GET LEO ORBIT INFORMATION
C -------------------------
      CALL GTFLNA(0,'LEOSTD ',FILNAM,IRCSTD)
      CALL GTFLNA(0,'KININP ',FILKIN,IRCKIN)
      CALL GTFLNA(0,'KINVEL ',FILVEL,IRCVEL)
C
C INITIALIZE POSITION VECTOR
C --------------------------
      DO IFRQ=1,2
        XSTAT(1:3,IFRQ)=XSTAT0(:,IFRQ)
      END DO
C
C HANDLING STATION TYPES
C ---------------------------
      CALL STAFLG(STATIO,OBSWIN%T(1),IFLAG,MARTYP)
C
C SET LEO DATA
C ------------
      IF (MARTYP.EQ.MTypeSPACE) THEN
        IF (IRCSTD.NE.0 .AND. IRCKIN.NE.0) THEN
          WRITE(LFNERR,10)
10        FORMAT(/,' *** SR SMPRNG: LEO ORBIT NOT AVAILABLE',/)
          CALL EXITRC(2)
        END IF
C
C READ LOCAL GEODETIC DATUM FROM COORDINATE FILE
C ----------------------------------------------
        CALL GTFLNA(1,'COORD  ',COOFIL,IRC)
        CALL OPNFIL(LFNLOC,COOFIL,'OLD','FORMATTED',
     1           'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNLOC,IOSTAT,COOFIL,'SMPRNG')

        READ(LFNLOC,1) DATUM,CDUMMY
1       FORMAT(//,22X,A16,12X,A1,///)
        CLOSE(UNIT=LFNLOC)
C
C GET LOCAL GEODETIC DATUM PARAMETERS
C -----------------------------------
        CALL GETDAT(DATUM,AELL,BELL,DXELL,DRELL,SCELL)
      END IF
C
C COMPUTE MAXIMUM NUMBER OF EPOCHS
C --------------------------------
      NEPO=IDNINT(86400.D0*(OBSWIN%T(2)-OBSWIN%T(1)))/INTER+1
      IF(NEPO.GT.MXCEPO)THEN
        WRITE(LFNERR,11) NEPO,MXCEPO
11      FORMAT(/,' *** SR SMPRNG: TOO MANY OBSERVATION EPOCHS',/,
     1                       16X,'NUMBER OF EPOCHS     :',I5,/,
     2                       16X,'MAX. NUMBER OF EPOCHS:',I5,/)
        CALL EXITRC(2)
      END IF
C
C SIMULATE IRREGULAR IONOSPHERE ERRORS
C ------------------------------------
      IF (INIT.EQ.1) THEN
        FACRMS=DSQRT(INTER/60.D0)/10.D0
        NIGHT=IONO(1)
        DAY=IONO(2)
        DE(1)=0.D0
        DE(2)=0.D0
        IF (IONO(3).NE.0) THEN
          DO IEPO=1,NEPO
            DO I=1,2
              CALL RANDU(IX,IY,DELTA)
              IX=IY
              DE(I)=DE(I)+(DELTA-0.5)*IONO(3)*FACRMS
              ELEIRR(I,IEPO)=DE(I)
            END DO
          END DO
        ENDIF
      END IF

C -----------------------------------------------------
C INITIALIZE OBSERVATION NUMBERS AND POLARIZATION ARRAY
C -----------------------------------------------------
      DO ISAT=1,NSAT
        NOBSVN(ISAT)=0
        POLARS(ISAT)=0.D0
      END DO
C
C GET ORBIT SYSTEM
C ----------------
      CALL GETORB(SVN(1),1,0,1,OBSWIN%T(1),ICRARC,IORSYS,
     1            XSAT,TOSC,ELE,IRC,cmcyn)
C --------------------------------
C
C LOOP OVER ALL OBSERVATION EPOCHS
C
C --------------------------------
      DO 500 IEPO=1,NEPO
        T=OBSWIN%T(1)+(IEPO-1)*INTER/86400.D0
        TREL=T-OBSWIN%T(1)
C
C COMPUTE POLYNOMIAL RECEIVER CLOCK ERROR:
C ---------------------------------------
        IF (NCLOCK.EQ.0) THEN
          RCLK=0.D0
        ELSE
          RCLK=CLOCK(NCLOCK)
          DO I=NCLOCK-1,1,-1
            RCLK=CLOCK(I)+RCLK*TREL
          END DO
        END IF
        ERRDFR=RCLK/86400.D0
C
C GET LEO POSITION IN TRUE SYSTEM OF EPOCH
C ----------------------------------------
        IF (MARTYP.EQ.MTypeSPACE) THEN
C
C 2.1) GET LEO COORDINATES FROM THE FILE "KININP"
C -----------------------------------------------
          IRCEPO2=1
          IRCVEL2=1
          IF (IRCKIN==0) THEN
            CALL  ReadKin(FILKIN,STATIO,
     1                    T-ERRDFR,1,0,XKIN,IRCEPO2)
          END IF
          IF (IRCVEL==0) THEN
            CALL  ReadVel(FILVEL,STATIO,
     1                    T-ERRDFR,1,0,XVEL,IRCVEL2)
          END IF
C
C TRANSFORMATION INTO TRUE SYSTEM OF EPOCH
C ----------------------------------------
          IF (IRCEPO2==0) THEN
            XDUMMY(1:3)=0.0D0
            CALL COOTRA(IORSYS,0,T-ERRDFR,XDUMMY,
     1                  SZ,XPOL,YPOL,UT1GPS)
            IF (IRCVEL2.NE.0) THEN
              CALL TRUEARTH(XKIN,XLEO(1:3),1,0,
     1                      SZ,XPOL,YPOL)
              XLEO(4:9)=0.D0
            ELSE
              XKV(1:3)=XKIN(1:3)
              XKV(4:6)=XVEL(1:3)
              CALL TRUEARTH(XKV,XLEO(1:6),1,1,
     1                      SZ,XPOL,YPOL)
              XLEO(7:9)=0.D0
            END IF
          ELSE
            IF (IRCVEL2.EQ.0 .AND. IRCSTD.EQ.0 ) THEN
              CALL GTLEOCO(STATIO,T-ERRDFR,2,0,
     1                     XKV,SZ,XPOL,YPOL,IRCODE,cmcyn_leo)
              IF (IRCODE.EQ.0) THEN
                XKV(4:6)=XVEL(1:3)
                CALL TRUEARTH(XKV,XLEO(1:6),1,1,
     1                        SZ,XPOL,YPOL)
                XLEO(7:9)=0.D0
              END IF
            END IF
          END IF
C
C 2.2) GET LEO COORDINATES FROM THE ORBIT
C ---------------------------------------
          IF (IRCSTD==0) THEN
            CALL GTLEOCO(STATIO,T-ERRDFR,1,2,
     1                   XTMP(1:9),SZ,XPOL,YPOL,IRCODE,cmcyn_LEO)
            IF (IRCODE==0) THEN
              IF (IRCEPO2.NE.0) XLEO(1:3)=XTMP(1:3)
              IF (IRCVEL2.NE.0) XLEO(4:6)=XTMP(4:6)
              XLEO(7:9)=XTMP(7:9)
            END IF
          END IF
C
          IF ((IRCEPO2.NE.0 .AND. IRCODE.NE.0) .OR.
     1        (IRCVEL2.NE.0 .AND. IRCODE.NE.0 )) THEN
            WRITE(LFNERR,15)T-ERRDFR
15                FORMAT(/,' *** SR SMPRNG: ',
     1            'NO LEO A PRIORI POSITION/VELOCITY FOR THE EPOCH: ',
     2            F15.6/)
            DO ISAT=1,NSAT
              PRANG(IEPO,1,ISAT,1)=0.D0
              PRANG(IEPO,2,ISAT,1)=0.D0
              PRANG(IEPO,1,ISAT,2)=0.D0
              PRANG(IEPO,2,ISAT,2)=0.D0
              ZENDIS(IEPO,ISAT)=0.D0
            END DO
            GOTO 500
          END IF
        END IF
C
C COMPUTE IONOSPHERE CORRECTION FACTOR
C ------------------------------------
        IF (IONO(3).NE.0.AND.MARTYP.NE.MTypeSPACE) THEN
          CALL GEOTOP(PLH(1,1),PLH(2,1),XSTION,XSTAT(1,1),DXTOP,POLAR)
          FACION=POLAR(3)/200000.D0
          IF (FACION.GT.1.D0) FACION=1
          COSAZ=DCOS(POLAR(1))
          SINAZ=DSIN(POLAR(1))
        END IF
C
C CORRECTION FOR SOLID EARTH TIDES
C --------------------------------
        IF (MARTYP.NE.MTypeSPACE) THEN
          CALL XYZTIM(IORSYS,T,XSTAT(1:3,1),STATIO,XSTEPO(1:3,1),
     1                cmcyn)
C
C IN CASE OF KINEMATIC STATIONS
C -----------------------------
          IF (IRCKIN.EQ.0) THEN
            CALL READKIN(FILKIN,STATIO,T,1,0,XKIN,IRCEPO)
            IF (IRCEPO.EQ.0) THEN
              DO K=1,3
                XSTEPO(K,1)=XSTEPO(K,1)+(XKIN(K)-XSTAT(K,1))
              ENDDO
            ENDIF
          ENDIF
C
C
C CONSIDER CORRECTIONS FROM VIENNA GRID FILES
C -------------------------------------------
          GRIDLD(:) = 0D0
          DO II = 1, SIZE(GRDNEQ)-1
            DO JJ = 1,3
              GRIDLD(JJ) = GRIDLD(JJ) +
     1          GETGRID(GETGRIDKEYW(GRDNEQ(II),JJ),T,PLH(1:3,1))
            ENDDO
C REORDER FROM "UNE" TO "NEU"
            RHLP = GRIDLD(1)
            GRIDLD(1:2) = GRIDLD(2:3)
            GRIDLD(3) = RHLP
C
            CALL ELLECC(XSTELL,GRIDLD,GRDXYZ)
            DO JJ = 1,3
              XSTEPO(JJ,1)=XSTEPO(JJ,1)+GRDXYZ(JJ)
            ENDDO
          ENDDO
C
C SHOULD BE FASTER THAN A SECOND CALL OF XYZTIM ...
          DO II=1,3
            XSTEPO(II,2)=XSTEPO(II,1)+(XSTAT(II,2)-XSTAT(II,1))
          ENDDO
C
C          CALL XYZTIM(IORSYS,T,XSTAT(1:3,2),STATIO,XSTEPO(1:3,2),
C     1                cmcyn)
        END IF
C
C LOOP OVER ALL SATELLITES OF EPOCH IEPO
C --------------------------------------
        DO 100 ISAT=1,NSAT
C
C GET SATELLITE POSITION IN SYSTEM B1950.0 OR J2000.0
          CALL GETORB(SVN(ISAT),1,2,2,T-ERRDFR,ICRARC,IORSYS,
     1                  XSAT,TOSC,ELE,IRC)
C
C NO ARC FOUND OR THIS SATELLITE
          IF (IRC .NE. 0) THEN
            PRANG(IEPO,1,ISAT,1)=0.D0
            PRANG(IEPO,2,ISAT,1)=0.D0
            PRANG(IEPO,1,ISAT,2)=0.D0
            PRANG(IEPO,2,ISAT,2)=0.D0
            ZENDIS(IEPO,ISAT)=0.D0
            GOTO 100
          END IF
C
C TRANSFORM INTO SYSTEM OF EPOCH
C--------------------------------
          CALL COOTRA(IORSYS,2,T-ERRDFR,XSAT,SZ,XPOL,YPOL,UT1GPS)
C
C ATTITUDE OF SATELLITE
C ---------------------
          CALL GNSSATTI(IORSYS,SVN(ISAT),T-ERRDFR,1,XSAT,
     1                  EX_SAT=EX,EY_SAT=EY,EZ_SAT=EZ)
C
C APPLY SENSOR OFFSET AND PHASE CENTER OFFSET
C --------------------------------------------
          DO IFRQ=1,NFRQPH
C
C FOR GNSS SATELLITES
            CALL SAT_OFF(SVN(ISAT),T-ERRDFR,typeMWTR,IFRQ,OFFSET)
            DO K = 1,3
              XSATEP(K,IFRQ)=XSAT(K)+EX(K)*OFFSET(1)
     1                              +EY(K)*OFFSET(2)
     2                              +EZ(K)*OFFSET(3)
            ENDDO
            XSATEP(4:,IFRQ)=XSAT(4:)
C
C FOR LEO
            IF (MARTYP.EQ.MTypeSPACE) THEN
              CALL STA_OFF(ANTTYP,IANTEN,STATIO,SVN(ISAT),IFRQ,
     1                                         CSESS(IFRQ),ANTECC)
              CALL LEOANTE(STATIO,T-ERRDFR,ANTECC,0,XLEO,TRUOFF)
              XSTAT2(1:3,IFRQ)=XLEO(1:3)+TRUOFF(:)
              XSTAT2(4:9,IFRQ)=XLEO(4:9)
C            TRANSFORMATION INTO EARTH FIXED SYSTEM
              CALL TRUEARTH(XSTAT2(:,IFRQ),XSTAT(:,IFRQ),0,0,
     1                    SZ,XPOL,YPOL)
              XSTAT(4:6,IFRQ)=XSTAT2(4:6,IFRQ)
              CALL XYZELL(AELL,BELL,DXELL,DRELL,SCELL,XSTAT(1,IFRQ),
     1                    PLH(1,IFRQ))
              XSTSAT(:,IFRQ)=XSTAT(:,IFRQ)
C
              IF (IRCEPO2==0)cmcyn_leo=.FALSE.
              CALL CMC_LEO(XLEO(1:3),XDUMMY(1:3),
     1                 XSTAT(1:3,IFRQ),XSTSAT(1:3,IFRQ),
     2                 T-ERRDFR,cmcyn,cmcyn_leo)
            ELSE
C
C FOR OTHERS
              CALL STA_OFF(ANTTYP,IANTEN,STATIO,SVN(ISAT),IFRQ,
     1                                         CSESS(IFRQ),ANTECC)
              CALL ELLECC(XSTELL,ANTECC,EXC)
              DO I=1,3
                XSTSAT(I,IFRQ)=XSTEPO(I,IFRQ)+EXC(I)
              END DO
            ENDIF
          ENDDO
C
C COMPUTE PSEUDORANGES
C --------------------
          NDIFF=0
          ISTA=1
          ISATCO=1
          DELTAT=0.D0
          ZENMAX=PI/180.D0*(90-IELEV)
          NADMAX=PI/180.D0*15.D0
C
          DO ICARR=1,2
            DO MEATYP=1,2
              ICP=MOD(MEATYP,2)+1
              CALL PRANGE(ISTA,T,DELTAT,SECIPL,STATIO,XSTSAT(1,ICARR),
     1          SVN(ISAT),XSATEP(:,ICARR),SZ,XPOL,YPOL,ISATCO,ITROPO,
     2          IEXTRA,PLH(1,ICARR),MEATYP,ICARR,IORSYS,XTOP,ZENMAX,
     3          NADMAX,PRANG(IEPO,ICARR,ISAT,ICP),SATCLK(ISAT),ZEN,AZI,
     4          ZENION,NDIFF,-1,IRCCLK,IRCPCV,NAD,AZISAT,AZISOK,ANTTYP,
     5          IANTEN,CSESS(1),IREL2)
C
C COMPUTE ZENITH DISTANCE AND AZIMUTH OF A SATELLITE AS SEEN FROM SENSOR SYSTEM
C -----------------------------------------------------------------------------
              IF (MARTYP.EQ.MTypeSPACE) THEN
                CALL LEOSKY(STATIO,T,0,0,XSTSAT(:,ICARR),
     1                      XSATEP(:,ICARR),SZ,DELTAT,XPOL,YPOL,MEATYP,
     2                      XTOP2,DUMMY,ZEN2,AZI2,NAD2,AZISA2,SVN(ISAT),
     3                      IORSYS,AZISO2)
              ELSE
                ZEN2=ZEN
                AZI2=AZI
                NAD2=NAD
                AZISA2=AZISAT
                AZISO2=AZISOK
                XTOP2=XTOP
              END IF
C
C CHECK WHETHER SATELLITE CLOCK AVAILABLE AND ELEVATION CUT-OFF
              IF (IRCCLK.NE.0 .OR. IRCPCV.NE.0)THEN
                PRANG(IEPO,1,ISAT,1)=0.D0
                PRANG(IEPO,2,ISAT,1)=0.D0
                PRANG(IEPO,1,ISAT,2)=0.D0
                PRANG(IEPO,2,ISAT,2)=0.D0
                ZENDIS(IEPO,ISAT)=0.D0
                POLARS(iSat) = 0.D0
                GOTO 100
              END IF
C
C POLARIZATION EFFECT OF DIPOLE ANTENNAS (RECEIVER AND SATELLITE)
C ---------------------------------------------------------------
              IF (MEATYP == 1) THEN
                IF (MARTYP.NE.MTypeSPACE) THEN
                  CALL POLARI(T,2,XTOP2,XSATEP(:,ICARR),DPOLAR)
                ELSE
                  CALL POLARLEO(T,2,STATIO,XSTAT2(1,ICARR),
     1                          XSATEP(:,ICARR),DPOLAR)
                END IF
                NPOLAR=NINT(POLARS(ISAT)-DPOLAR)
                POLARS(ISAT)=NPOLAR+DPOLAR
                PLREFF=POLARS(ISAT)*WLGT(ICARR,SVN(ISAT))
                PRANG(IEPO,ICARR,ISAT,ICP)=
     1                PRANG(IEPO,ICARR,ISAT,ICP)+PLREFF
              ELSE
                PLREFF=0.D0
              END IF
C
C DIFFERENTIAL CODE BIAS CORRECTION
C----------------------------------
              CALL DCBCOR(MEATYP,ICARR,SVN(ISAT),STATIO,RECTYP,0,0,T,
     1                    PRANG(IEPO,ICARR,ISAT,ICP))
C
C COMPUTE IONOSPHERIC REFRACTION (REGULAR PART)
C----------------------------------------------
              IF (MARTYP.NE.MTypeSPACE) THEN
                IF (IONO(3).NE.0) THEN
                  NIGHT=IONO(1)+(COSAZ*ELEIRR(1,IEPO)
     1                          +SINAZ*ELEIRR(2,IEPO))*FACION
                ENDIF
                CALL IONOS1(FRQ(ICARR,SVN(ISAT)),NIGHT,DAY,
     1                      XSTSAT(1,ICARR),XSATEP(:,ICARR),SZ,T,ZEN,
     2                      DION)
              ELSE
                DION=0D0
              ENDIF
C
C APPLY RECEIVER CLOCK ERROR AND IONOSPHERIC REFRACTION
              PRANG(IEPO,ICARR,ISAT,ICP)=
     1              PRANG(IEPO,ICARR,ISAT,ICP)+RCLK*C+(-1)**MEATYP*DION
C
C NUMBER OF OBSERVATIONS FOR SATELLITE ISAT
              IF (ICARR.EQ.2 .AND. MEATYP.EQ.2) THEN
                  NOBSVN(ISAT)=NOBSVN(ISAT)+1
C
C STORE ZENITH DISTANCE
                ZENDIS(IEPO,ISAT)=ZEN2
              END IF
            END DO
          END DO
100     CONTINUE
C
C USE ONLY MXOSVN SATELLITES
C --------------------------
C CHECK FOR SATELLITES THAT DISAPPEARED SINCE THE PREVIOUS EPOCH
        DO ILST=1,NSVOLD
          ISAT=LISTI4(0,NSAT,SVN,SVNOLD(ILST),NSAT)
          IF (ISAT.NE.0) THEN
            IF (PRANG(IEPO,1,ISAT,1).EQ.0D0) ISAT = 0
          ENDIF
          IF (ISAT.EQ.0) SVNOLD(ILST)=0
        ENDDO
C SQUEEZE LIST OF PREVIOUSLY OBSERVED SATELLITES
        NSVNEW=NSVOLD
        DO ILST=NSVOLD,1,-1
          IF (SVNOLD(ILST).EQ.0) THEN
            DO II=ILST,NSVOLD-1
              SVNOLD(II)=SVNOLD(II+1)
            ENDDO
            NSVNEW=NSVNEW-1
          ENDIF
        ENDDO
        NSVOLD=NSVNEW
C ADD NEW SATELLITES
        DO ISAT=1,NSAT
          IF (PRANG(IEPO,1,ISAT,1).NE.0D0) THEN
            ILST=LISTI4(0,NSVOLD,SVNOLD,SVN(ISAT),NSVOLD)
            IF (ILST.EQ.0) THEN
              IF (NSVOLD.LT.MXOSVN) THEN
                NSVOLD=NSVOLD+1
                SVNOLD(NSVOLD)=SVN(ISAT)
              ELSE
                PRANG(IEPO,1,ISAT,1)=0.D0
                PRANG(IEPO,2,ISAT,1)=0.D0
                PRANG(IEPO,1,ISAT,2)=0.D0
                PRANG(IEPO,2,ISAT,2)=0.D0
                ZENDIS(IEPO,ISAT)=0.D0
              ENDIF
            ENDIF
          ENDIF
        ENDDO
500   CONTINUE

C
C DEALLOCATE KINEMATIC ARRAYS
C ---------------------------
      IF (IRCKIN.EQ.0) THEN
        CALL  ReadKin(FILKIN,STATIO,T-ERRDFR,1,1,XKIN,IRCEPO2)
      END IF
      IF (IRCVEL.EQ.0) THEN
        CALL  ReadVel(FILVEL,STATIO,
     1                T-ERRDFR,1,1,XKIN,IRCEPO2)
      END IF
C
      RETURN
      END SUBROUTINE

      END MODULE
