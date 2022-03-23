      MODULE s_EEPRNT
      CONTAINS
C*
      SUBROUTINE EEPRNT(MAXTYP,TITNEW,MODNAM,SIGMA0,WGTTYP,IUSSIG,
     1                  SUMPER,NWIN,WINTIM,WINSIG,ISAMPL,IADEPO,
     2                  REFEPO,NOBS,NUMOBS,NPAR,PARNAM,PARTIM,RMS,
     3                  RMSOBS,XXXAPR,XXX,ANOR)
CC
CC NAME       :  EEPRNT
CC
CC PURPOSE    :  PRINT RESULTS OF ERP ESTIMATION
CC
CC PARAMETERS :
CC        IN  :  MAXTYP : MAXIMUM NUMBER OF OBSERVATION TYPES    I*4
CC               TITNEW : GENERAL TITLE OF PROGRAM RUN          CH*80
CC               MODNAM : NAME OF THE RESULTING MODEL OF        CH*16
CC                          NUTATION OR SUB-DAILY ERPS
CC               SIGMA0 : A PRIORI SIGMA OF OBS. OF UNIT WEIGHT  R*8
CC                        (MAS)
CC               WGTTYP(2,ITYP): WEIGHT OF OBSERVATION           R*8
CC               IUSSIG : USE ERP SIGMAS IN ESTIMATION           I*4
CC                        =0: NO, =1: YES
CC               SUMPER : MAXIMUM PERIOD FOR STATISTICS          R*8
CC               NWIN   : NUMBER OF DATA WINDOWS                 I*4
CC               WINTIM(2,I),I=1,..,NWIN: DATA WINDOWS           R*8
CC                          FROM,TO IN MJD
CC               WINSIG(I),I=1,..,NWIN: SIGMA FACTOR FOR EACH    R*8
CC                          WINDOW
CC               ISAMPL(3): SAMPLING OF THE DATA                 I*4
CC                        (1): SAMPLING (EVERY NTH OBSERVATION)
CC                        (2): START DATA RECORD (FIRST=1)
CC                        (3): SAMPLING INTERVAL (MIN) FOR
CC                               SIMULATION OF SERIES
CC               IADEPO : ADJUST OBSERVATION EPOCHS TO THE       I*4
CC                        NEAREST FULL "IADEPO" MINUTES
CC                        (E.G. IADEPO=60: ROUNDING TO THE
CC                        NEAREST HOUR)
CC               REFEPO(I),I=1,2: REFERENCE EPOCHS               R*8
CC                        (1): REFERENCE EPOCH FOR DRIFT
CC                             PARAMETERS, FIRST EPOCH PROCESSED
CC                        (2): LAST EPOCH PROCESSED
CC               NOBS   : NUMBER OF OBSERVATIONS                 I*4
CC               NUMOBS(I),I=1,..,MAXTYP: NUMBER OF OBSERVATIONS I*4
CC                        FOR EACH OBSERVATION TYPE
CC               NPAR   : MAXIMUM NUMBER OF PARAMETERS ALLOWED   I*4
CC               PARNAM(I),I=1,..,NPAR: PARAMETER NAMES         CH*20
CC               PARTIM(2,I),I=1,..,NPAR: PARAMETER WINDOWS      R*8
CC                          FROM,TO IN MJD
CC               RMS    : SUM OF O-C**2                          R*8
CC               RMSOBS(I),I=1,..,MAXTYP: RMS PER OBS. TYPE      R*8
CC               XXXAPR(I),I=1,..,NPAR: A PRIORI VALUES OF PARA. R*8
CC               XXX(I),I=1,..,NPAR: SOLUTION VECTOR             R*8
CC               ANOR(I),I=1,..,NPAR*(NPAR+1)/2: NORMAL EQUATION R*8
CC                        MATRIX
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC CREATED    :  07-NOV-97
CC
CC CHANGES    :  24-FEB-98 : MR: ADD SAMPLING OPTIONS
CC               26-MAR-98 : MR: ADD STATISTICS FOR SUBDAILY
CC               25-SEP-02 : HU: USE INTERFACE TO PRFLNA
CC               02-OCT-02 : HU: USE V50 PRFLNA
CC               28-MAY-03 : PS: USE SR PRITIT
CC               24-JUN-03 : MR: ADD PRINTING OF VLBI WEIGHTS
CC               12-AUG-03 : MR: ADD RELATIVE SAGNAC FREQUENCY
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: COMLFNUM.INC REMOVED, M_BERN ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               08-AUG-05 : HB: USE NEW SR TIMST2 (MODULE)
CC               14-NOV-2011 SL: M_BERN W/ ONLY, NO PRITIT CALL
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnPrt
      USE f_ikf
      USE s_prflna
      USE s_maxtst
      USE s_timst2
      USE s_eeppro
      USE s_eepsub
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IADEPO, IDEG  , II    , IPAR  , IRC   , ITYP  ,
     1          IUSSIG, IWIN  , MAXTYP, MXLTYP, NOBS  , NPAR  , NWIN
C
      REAL*8    RMS   , SCAL  , SIGMA0, SUMPER, XRMS
C
CCC       IMPLICIT REAL*8(A-H,O-Z)
C
      PARAMETER (MXLTYP=11)
C
      CHARACTER*80 TITNEW
      CHARACTER*40 TSTRNG
      CHARACTER*20 PARNAM(*)
      CHARACTER*16 MODNAM
      CHARACTER*12 OBSTYP(MXLTYP)
      CHARACTER*8  UNIT
      CHARACTER*5  OBSUNT(MXLTYP)
      CHARACTER*3  NOYES(2)
C
      REAL*8       PARTIM(2,*),XXXAPR(*),XXX(*),ANOR(*),REFEPO(2)
      REAL*8       WINTIM(2,*),RMSOBS(MAXTYP),WGTTYP(2,MAXTYP),WINDOW(2)
      REAL*8       WINSIG(*)
C
      INTEGER*4    NUMOBS(MAXTYP),ISAMPL(3)
C

C
      DATA NOYES/'NO ','YES'/
      DATA OBSTYP/'X-POLE      ','Y-POLE      ','UT1-UTC     ',
     1            'DEPS        ','DPSI        ','X-POLE RATE ',
     2            'Y_POLE RATE ','UT1-UTC RATE','DEPS RATE   ',
     3            'DPSI RATE   ','REL. SAGNAC '/
      DATA OBSUNT/'MAS  ','MAS  ','MS   ',
     1            'MAS  ','MAS  ','MAS/D',
     2            'MAS/D','MS/D ','MAS/D',
     3            'MAS/D','     '/
C
C CHECK MAXIMUM DIMENSION
C -----------------------
      CALL MAXTST(0,'EEPRNT','MAXTYP',MXLTYP,MAXTYP,IRC)
      IF (IRC.NE.0) CALL EXITRC(2)

! Print File Names
! ----------------
      CALL prflna
C
C PRINT ALL PARAMETER VALUES
C --------------------------
      WRITE(LFNPRT,1001) TITNEW
1001  FORMAT(1X,A80,/,1X,80('-'),/)

C
C OPTIONS
C -------
      WRITE(LFNPRT,1002) NOYES(IUSSIG+1)
1002  FORMAT(1X,'OPTIONS:',/,1X,7('-'),//,
     1       1X,'WEIGHTING OF THE OBSERVATIONS: ',A,//)
C
C EPOCH ADJUSTMENT:
C ----------------
      WRITE(LFNPRT,1021) IADEPO
1021  FORMAT(1X,'EPOCH ADJUSTMENT:',/,1X,16('-'),//,
     1       1X,'ADJUST TO NEAREST N MINUTES:',I9,//)
C
C SAMPLING OPTIONS
C ----------------
      WRITE(LFNPRT,1022) (ISAMPL(II),II=1,3)
1022  FORMAT(1X,'SAMPLING OPTIONS:',/,1X,16('-'),//,
     1       1X,'EVERY N-TH OBSERVATION USED:',I9,/,
     2       1X,'STARTING OBSERVATION NUMBER:',I9,/,
     3       1X,'SAMPLING INTERVAL (MIN)    :',I9,//)
C
C OBSERVATION WINDOWS
C -------------------
      WRITE(LFNPRT,1003)
1003  FORMAT(1X,'ACTUAL OBSERVATION WINDOWS:',/,1X,26('-'),//,
     1       1X,'YYYY MM DD HH MM SS  YYYY MM DD HH MM SS',
     2          '  SIG.FAC.',/)
      IF (NWIN.NE.0) THEN
        DO IWIN=1,NWIN
          WINDOW(1)=DMAX1(WINTIM(1,IWIN),REFEPO(1))
          WINDOW(2)=DMIN1(WINTIM(2,IWIN),REFEPO(2))
          CALL TIMST2(1,2,WINDOW,TSTRNG)
          WRITE(LFNPRT,1004) TSTRNG,WINSIG(IWIN)
1004      FORMAT(1X,A40,F9.3)
        ENDDO
      ELSE
        CALL TIMST2(1,2,REFEPO,TSTRNG)
        WRITE(LFNPRT,1004) TSTRNG,1.D0
      ENDIF
      WRITE(LFNPRT,'( )')
C
C REFERENCE EPOCH FOR DRIFT PARAMETERS
C ------------------------------------
      CALL TIMST2(1,1,REFEPO(1),TSTRNG)
      WRITE(LFNPRT,1041) TSTRNG
1041  FORMAT(1X,'REFERENCE EPOCH: ',A,//)
C
C NOBS,NPAR,RMS
C -------------
      WRITE(LFNPRT,1005) MODNAM,NOBS,NPAR,RMS,SIGMA0
1005  FORMAT(1X,'RESULTS:',/,1X,8('-'),//,
     1       1X,'MODEL NAME:  ',A,//,
     2       1X,'NUMBER OF OBSERVATIONS  : ',I9,/,
     3       1X,'NUMBER OF PARAMETERS    : ',I9,/,
     4       1X,'RMS OF UNIT WEIGHT      : ',F9.5,/,
     5       1X,'A PRIORI RMS            : ',F9.5,//)
C
C OBSERVATION STATISTICS
C ----------------------
      WRITE(LFNPRT,1051)
1051  FORMAT(1X,'OBSERVATION STATISTICS:',/,1X,23('-'),//,
     1       1X,'TYPE  OBSERVATION   WEIGHT  WEIGHT2    #OBS.',
     2          '       WRMS',
     3          '     UNITS',/)
C
      DO ITYP=1,MAXTYP
        IF (NUMOBS(ITYP).NE.0) THEN
          WRITE(LFNPRT,1052) ITYP,OBSTYP(ITYP),WGTTYP(1,ITYP),
     1                       WGTTYP(2,ITYP),NUMOBS(ITYP),
     2                       DSQRT(RMSOBS(ITYP)/(NUMOBS(ITYP))),
     3                       OBSUNT(ITYP)
1052      FORMAT(I5,2X,A12,2F8.4,I10,F13.5,4X,A)
        ENDIF
      ENDDO
      WRITE(LFNPRT,'(/)')
C
C PRINT PARAMETERS
C ----------------
      WRITE(LFNPRT,1006)
1006  FORMAT(1X,'AUXILIARY PARAMETERS:',/,1X,20('-'),//,
     1       1X,'PAR.  PAR. NAME             UNITS     TIME INTERVAL',
     2       29X,'A PRIORI VALUE    EST. VALUE        NEW-OLD        ',
     3          '   RMS',/)
C
      DO IPAR=1,NPAR
C
C ONLY NON-AMPLITUDE PARAMETERS
        IF (PARNAM(IPAR)(1:2).NE.'ES' .AND.
     1      PARNAM(IPAR)(1:2).NE.'EC' .AND.
     2      PARNAM(IPAR)(1:2).NE.'PS' .AND.
     3      PARNAM(IPAR)(1:2).NE.'PC') THEN
C
C UNITS
          UNIT='MAS  '
          IF (PARNAM(IPAR)(1:2).EQ.'UC' .OR.
     1        PARNAM(IPAR)(1:2).EQ.'US')           UNIT='MS'
          IF (PARNAM(IPAR)(1:2).EQ.'U_')           UNIT='MS'
          IF (PARNAM(IPAR)(1:1).EQ.'D')            UNIT='MAS/D'
          IF (PARNAM(IPAR)(1:2).EQ.'DU')           UNIT='MS/D'
          IF (PARNAM(IPAR)(3:8).EQ.'DRIFT')        UNIT='MAS/Y'
          IF (PARNAM(IPAR)(1:8).EQ.'U_ DRIFT')     UNIT='MS/Y'
          IF (PARNAM(IPAR)(3:6).EQ.' DEG')
     1         UNIT='MAS/Y**'//PARNAM(IPAR)(8:8)
          IF (PARNAM(IPAR)(1:6).EQ.'U_ DEG')
     1         UNIT='MS/Y**'//PARNAM(IPAR)(8:8)
C
          IF (PARNAM(IPAR)(3:8).EQ.' DRIFT') THEN
            SCAL=365.25D0
          ELSEIF (PARNAM(IPAR)(3:6).EQ.' DEG') THEN
            READ(PARNAM(IPAR)(8:8),'(I1)') IDEG
            SCAL=365.25**IDEG
          ELSE
            SCAL=1.D0
          ENDIF
C
          WINDOW(1)=DMAX1(PARTIM(1,IPAR),REFEPO(1))
          WINDOW(2)=DMIN1(PARTIM(2,IPAR),REFEPO(2))
          CALL TIMST2(1,2,WINDOW,TSTRNG)
          XRMS=RMS*DSQRT(ANOR(IKF(IPAR,IPAR)))
C
          WRITE(LFNPRT,1007) IPAR,PARNAM(IPAR),UNIT,TSTRNG,
     1                       XXXAPR(IPAR)*SCAL,
     2                       (XXX(IPAR)+XXXAPR(IPAR))*SCAL,
     3                       XXX(IPAR)*SCAL,XRMS*SCAL
1007      FORMAT(I5,2X,A20,2X,A8,2X,A40,4F15.5)
        ENDIF
      ENDDO
      WRITE(LFNPRT,'(/)')
C
C WRITE RMS FOR SUB-DAILY AMPLITUDE DIFFERENCES
C ---------------------------------------------
      CALL EEPSUB(REFEPO,SUMPER,NPAR,PARNAM,PARTIM,RMS,XXXAPR,XXX,ANOR)
C
C WRITE PROGRADE/RETROGRADE TERMS AND THEIR FORMAL ERRORS
C -------------------------------------------------------
      CALL EEPPRO(REFEPO,SUMPER,
     1            NPAR,PARNAM,PARTIM,RMS,XXXAPR,XXX,ANOR)
C
C END
C ---
      RETURN
      END SUBROUTINE

      END MODULE
