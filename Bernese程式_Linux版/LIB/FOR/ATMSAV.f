      MODULE s_ATMSAV
      CONTAINS

C*
      SUBROUTINE ATMSAV(TITLE ,NPAR  ,RMS   ,LOCQ  ,SOL   ,QMAT  ,
     1                  OPTGIM,POLGIM,NAMGIM,EPOGIM,SCAGIM,NSTEFF,
     2                  NSAEFF,ZENMAX,NSAMPL,INFGIM,IORSYS)
CC
CC NAME       :  ATMSAV
CC
CC PURPOSE    :  SAVE COEFFICIENTS OF LOCAL OR GLOBAL IONOSPHERE
CC               MODELS COMPUTED BY PROGRAM GPSEST
CC
CC PARAMETERS :
CC         IN :  TITLE  : GENERAL TITLE                       CH*80
CC               NPAR   : NUMBER OF PARAMETERS ESTIMATED      I*4
CC               RMS    : ESTIMATED MEAN ERROR OF UNIT WEIGHT R*8
CC               LOCQ(K,I),K=1,.,MAXLCQ,I=1,..,NPAR:          I*4
CC                        DESCRIPTION OF PARAMETER NUMBER I
CC               SOL(I),I=1,...,NPAR: SOLUTION VECTOR         R*8
CC               QMAT(I),I=1,..: LINEARIZED NORMAL EQUATION   R*8
CC                        MATRIX
CC               OPTGIM : OPTIONS FOR GLOBAL IONOSPHERE MODEL I*4(*)
CC                        (1): MAXIMUM DEGREE
CC                        (2): MAXIMUM ORDER
CC                        (3): FLAG FOR REFERENCE FRAME
CC                             =1: GEOGRAPHICAL
CC                             =2: GEOMAGNETIC
CC                        (4): FLAG FOR POSITION OF THE SUN
CC                             =1: MEAN
CC                             =2: TRUE
CC                        (5): ESTIMATION OF LAYER HEIGHT
CC                             =0: NO
CC                             =1: ONE PARAMETER IN ALL
CC                             =2: ONE PARAMETER PER MODEL
CC                        (6): MODE OF TEMPORAL MODELING
CC                             =1: STATIC MODEL
CC                             =2: DYNAMIC MODEL
CC                        (7): TOTAL NUMBER OF MODELS
CC                        (8): MAPPING FUNCTION
CC                             =0: NONE
CC                             =1: 1/COS
CC                        (9): STATION-SPECIFIC MODELS
CC                        (10): COMPONENT TO BE ESTIMATED
CC                              =1: DETERMINISTIC
CC                              =2: STOCHASTIC
CC               POLGIM(I,J),I=1,2,3,J=1,..,OPTGIM(7):        R*8(3,*)
CC                        I=1: HEIGHT OF SINGLE LAYER (M)
CC                        I=2: LAT. OF NORTH GEOMAGNETIC POLE
CC                        I=3: EAST LONGITUDE
CC               NAMGIM(I),I=1,..,OPTGIM(7): MODEL NUMBERS    CH*16(*)
CC               EPOGIM(I,J),I=1,2,J=1,..,OPTGIM(7): PERIODS  R*8(2,*)
CC                        OF VALIDITY / REF EPOCHS (MJD)
CC               SCAGIM : SCALING FACTOR FOR                  R*8(*)
CC                        (1): ION. COEFFICIENTS
CC                        (2): SINGLE-LAYER HEIGHT
CC                        (3): DTEC PARAMETERS
CC               NSTEFF : NUMBER OF CONTRIBUTING STATIONS     I*4
CC               NSAEFF : NUMBER OF SATELLITES                I*4
CC               ZENMAX : MAXIMUM ZENITH DISTANCE IN RAD      R*8
CC               NSAMPL : SAMPLING RATE (SEC)                 I*4
CC               INFGIM(I,J),I=1,2,J=1,..,OPTGIM(7):          R*8(2,*)
CC                        I=1: MAXIMUM TEC (TECU)
CC                        I=2: RMS ERROR (TECU)
CC               IORSYS : ORBIT SYSTEM                        I*4
CC                        =1: B1950.0
CC                        =2: J2000.0
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, S.SCHAER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/10/29 17:35
CC
CC CHANGES    :  24-DEC-92 : ??: USE OF SR "OPNFIL" TO OPEN FILES
CC               05-DEC-95 : SS: NEW IONOSPHERE MODEL (TYPE 2)
CC               05-DEC-95 : SS: INSERT REMARK LINE
CC               05-DEC-95 : SS: APPLY "SCAGIM"
CC               20-DEC-95 : SS: DECLARE "NUMGIM" AS CH*7
CC               20-DEC-95 : SS: "IONTXT" IN CALL OF SR GETGIM
CC               28-DEC-95 : SS: SAVE TID INDICATOR
CC               19-FEB-96 : SS: SAVE "NSTEFF"
CC               07-MAR-96 : SS: SAVE IONOSPHERE MAPS IN IONEX FORMAT
CC               06-JUN-96 : SS: CALL OF SR "INXSAV" MODIFIED
CC               07-APR-97 : SS: HANDLE CASE OF NEGATIVE VARIANCES
CC               17-APR-97 : SS: WRITE ELEVATION CUT-OFF ANGLE
CC               23-SEP-97 : SS: IONEX VERSION 1.0
CC               13-NOV-97 : SS: "FILAPR" IN CALL OF SR GETGIM
CC               14-NOV-97 : SS: CHECK "NMODEL"
CC               17-NOV-97 : SS: "IONTIT" IN CALL OF SR GETGIM
CC               20-JAN-98 : SS: USE SR SAVGIM
CC               20-JAN-98 : SS: "IONINF" ADDED
CC               26-JAN-98 : SS: STATION-SPECIFIC GIMS
CC               29-APR-98 : SS: DTEC LC
CC               26-MAY-98 : SS: "INFGIM" ADDED
CC               15-AUG-99 : JJ: RM UNUSED VAR VORZ
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               26-MAR-12 : RD: USE GETGIM AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: LFNERR, LFNLOC
      USE m_maxdim, ONLY: MAXGIT, MAXGIM
      USE d_const,  ONLY: PI
      USE f_ikf
      USE s_inxsav
      USE s_dimtst
      USE s_opnfil
      USE s_savgim
      USE s_getgim
      USE s_opnerr
      USE s_ionosi
      USE s_exitrc
      USE s_jmt
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , ID0   , ID1   , ID2   , IDEG  , ILAT  ,
     1          IMOD  , IMODEL, IONTYP, IORD  , IORSYS, IOSTAT, IPAR  ,
     2          IRC   , IRCAPR, IRCINX, IRCION, IREQ  , ITERM , ITIME ,
     3          ITRM  , ITYP  , J0    , J1    , J2    , K     , M0    ,
     4          M1    , M2    , MIN1  , MXCLCQ, NDEG  , NH    , NMOD  ,
     5          NMODEL, NORD  , NPAR  , NSAEFF, NSAMPL, NSTEFF, NTRM
C
      REAL*8    COE   , D0    , D1    , D2    , H0    , H1    , H2    ,
     1          RMS   , SIG   , XMJD0 , XMJD1 , XMJD2 , XRMS  , XVAL  ,
     2          XVAR  , ZENMAX
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C
      CHARACTER    TITLE*80,TEXT(16)*48,UPD(MAXGIM)*1
      CHARACTER    NAMGIM(*)*16,IONTXT(MAXGIM)*16,IONTIT*80
      CHARACTER*32 FILION,FILINX,FILAPR
      CHARACTER*6  MXNLCQ
C
      REAL*8       SOL(*),QMAT(*)
      REAL*8       IONDEV(10,MAXGIM)
      REAL*8       IONCOE(MAXGIT,MAXGIM),IONSIG(MAXGIT,MAXGIM)
      REAL*8       POLGIM(3,*),EPOGIM(2,*),SCAGIM(*),INFGIM(2,*)
C
      INTEGER*4    LOCQ(MXCLCQ,*)
      INTEGER*4    IONREQ(6,MAXGIM),NTERM(MAXGIM),NM(MAXGIT,2,MAXGIM)
      INTEGER*4    OPTGIM(*),IONIND(MAXGIT,MAXGIM),IONINF(4)
C
      COMMON/CATMSV/IONDEV,IONCOE,IONSIG,IONREQ,NTERM,NM,IONTXT,IONIND
C
C
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
C
C GET IONOSPHERE RESULT FILE NAMES
C --------------------------------
      CALL GTFLNA(0,'IONOSRS',FILION,IRCION)
      CALL GTFLNA(0,'IONEXRS',FILINX,IRCINX)
C
C RETURN, IF NO OUTPUT FILE NAME SPECIFIED
C ----------------------------------------
      IF (IRCION.NE.0.AND.
     1    IRCINX.NE.0) RETURN
C
C GET TYPE CODE OF IONOSPHERE MODEL PARAMETERS (1=LOCAL,2=GLOBAL)
C ---------------------------------------------------------------
      IONTYP=0
C
      DO 310 IPAR=1,NPAR
        IF (LOCQ(1,IPAR).EQ. 7) IONTYP=1
        IF (LOCQ(1,IPAR).EQ.19) IONTYP=2
310   CONTINUE
C
      IF (IONTYP.EQ.0) THEN
        WRITE(LFNERR,320)
320     FORMAT(/,' ### SR ATMSAV: NO IONOSPHERE MODEL PARAMETERS ',
     1         'FOUND',/)
        RETURN
      END IF
C
      IF (IONTYP.EQ.1) THEN
        IF (IRCINX.EQ.0) THEN
          WRITE(LFNERR,330)
330       FORMAT(/,' ### SR ATMSAV: IONEX OUTPUT FILE NOT CREATED',/)
C
          IRCINX=1
        END IF
        IF (IRCION.NE.0) RETURN
      END IF
C
      IF (IONTYP.EQ.1) THEN
C
C PART 1: SAVE INFORMATION CONCERNING LOCAL IONOSPHERE MODELS
C -----------------------------------------------------------
C READ OLD IONOSPHERE INFORMATION
        CALL IONOSI(1,NMODEL,IONREQ,IONDEV,
     1              NTERM,NM,IONCOE,IONSIG)
        DO 1 I=1,MAXGIM
          UPD(I)='N'
1       CONTINUE
C
C UPDATE ALL IONOSPHERE MODELS
        DO 100 IPAR=1,NPAR
          ITYP=LOCQ(1,IPAR)
          IF(ITYP.NE.7)GO TO 100
          NMOD=LOCQ(2,IPAR)
          ILAT=LOCQ(3,IPAR)
          ITIME=LOCQ(4,IPAR)
          COE=SOL(IPAR)
          SIG=RMS*DSQRT(QMAT(IPAR*(IPAR+1)/2))
C
C FIND A PRIORI VALUE FOR COEFFICIENT
          DO 50 ITERM=1,NTERM(NMOD)
            IF(NM(ITERM,1,NMOD).EQ.ILAT.AND.
     1         NM(ITERM,2,NMOD).EQ.ITIME)THEN
               UPD(NMOD)='Y'
               IONCOE(ITERM,NMOD)=IONCOE(ITERM,NMOD)+COE
               IONSIG(ITERM,NMOD)=SIG
               IF(NM(ITERM,1,NMOD).GT.IONREQ(1,NMOD))THEN
                 IONREQ(1,NMOD)=NM(ITERM,1,NMOD)
               END IF
               IF(NM(ITERM,2,NMOD).GT.IONREQ(2,NMOD))THEN
                 IONREQ(2,NMOD)=NM(ITERM,2,NMOD)
               END IF
               IF(NM(ITERM,1,NMOD)+NM(ITERM,2,NMOD).GT.
     1         IONREQ(3,NMOD))THEN
                 IONREQ(3,NMOD)=NM(ITERM,1,NMOD)+NM(ITERM,2,NMOD)
               END IF
               GO TO 60
            END IF
50        CONTINUE
C
C NO A PRIORI VALUE AVAILABLE
          UPD(NMOD)='Y'
          NTERM(NMOD)=NTERM(NMOD)+1
          NH=NTERM(NMOD)
          IONCOE(NH,NMOD)=SOL(IPAR)
          IONSIG(NH,NMOD)=SIG
          NM(NH,1,NMOD)=ILAT
          NM(NH,2,NMOD)=ITIME
          IF(NM(NH,1,NMOD).GT.IONREQ(1,NMOD))THEN
            IONREQ(1,NMOD)=NM(NH,1,NMOD)
          END IF
          IF(NM(NH,2,NMOD).GT.IONREQ(2,NMOD))THEN
            IONREQ(2,NMOD)=NM(NH,2,NMOD)
          END IF
          IF(NM(NH,1,NMOD)+NM(NH,2,NMOD).GT.
     1      IONREQ(3,NMOD))THEN
            IONREQ(3,NMOD)=NM(NH,1,NMOD)+NM(NH,2,NMOD)
          END IF
60        CONTINUE
100     CONTINUE
C
C OPEN OUTPUT IONOSPHERE FILES
        CALL OPNFIL(LFNLOC,FILION,'UNKNOWN','FORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILION,'ATMSAV')
C
C WRITE TITLE TO OUTPUT FILE
        WRITE(LFNLOC,110)TITLE
110     FORMAT(A80,/,80('-'))
C
C DEFINE TEXT FOR OUTPUT FILE
        TEXT(1)='IONOSPHERE MODEL NUMBER                        :'
        TEXT(2)='UPDATED IN LAST PROGRAM RUN ?                  :'
        TEXT(3)='ORIGIN OF DELEVOPMENT: LOCAL TIME (Y M D H)    :'
        TEXT(4)='                       LATITUDE   (DEGREES)    :'
        TEXT(5)='                       LONGITUDE  (DEGREES)    :'
        TEXT(6)='                       HEIGHT OF LAYER (KM)    :'
        TEXT(7)='DEGREE OF DEVELOPMENT: TIME                    :'
        TEXT(8)='                       LATITUDE                :'
        TEXT(9)='                       MIXED                   :'
        TEXT(10)='NORMALIZATION FACTORS: LATITUDE (DEGREES)      :'
        TEXT(11)='                       TIME (HOURS)            :'
        TEXT(12)='                       ELECTRON CONTENT        :'
        TEXT(13)='APPLICABILITY          FROM EPOCH              :'
        TEXT(14)='                       TO EPOCH                :'
        TEXT(15)='COEFFICIENTS:                                   '
        TEXT(16)='DEG. LAT   DEG. TIME  COEFFICIENT        RMS    '
C
C LOOP OVER ALL MODELS
        DO 200 IMODEL=1,NMODEL
          XMJD0=IONDEV(2,IMODEL)
          XMJD1=IONDEV(4,IMODEL)
          XMJD2=IONDEV(5,IMODEL)
          CALL JMT(XMJD0,J0,M0,D0)
          CALL JMT(XMJD1,J1,M1,D1)
          CALL JMT(XMJD2,J2,M2,D2)
          ID0=D0
          ID1=D1
          ID2=D2
          H0=24*(D0-ID0)
          H1=24*(D1-ID1)
          H2=24*(D2-ID2)
          IONDEV(1,IMODEL)=IONDEV(1,IMODEL)*180/PI
          IONDEV(3,IMODEL)=IONDEV(3,IMODEL)*180/PI
C
C WRITE HEADER FOR MODEL IMODEL
          WRITE(LFNLOC,115)TEXT(1),IMODEL,TEXT(2),UPD(IMODEL),
     1                TEXT(3),J0,M0,ID0,H0,
     2                TEXT(4),IONDEV(1,IMODEL),
     3                TEXT(5),IONDEV(3,IMODEL),
     4                TEXT(6),IONREQ(4,IMODEL),
     5                TEXT(7),IONREQ(2,IMODEL),
     6                TEXT(8),IONREQ(1,IMODEL),
     7                TEXT(9),IONREQ(3,IMODEL),
     8                TEXT(10),IONDEV(7,IMODEL),
     9                TEXT(11),IONDEV(8,IMODEL),
     *                TEXT(12),IONDEV(6,IMODEL),
     A                TEXT(13),J1,M1,ID1,H1,
     B                TEXT(14),J2,M2,ID2,H2,
     D                TEXT(15),TEXT(16)
115          FORMAT(A48,I5,/,A48,4X,A1,/,A48,I5,2I3,F5.1,/,
     1            2(A48,F8.2,/),4(A48,I5,/),2(A48,F7.2,/),
     2              A48,D15.2,/,2(A48,I5,2I3,F5.1,/),A48,/,A48)
C
C WRITE TERMS AND ASSOCIATED RMS-ERRORS
          DO 130 ITERM=1,NTERM(IMODEL)
            WRITE(LFNLOC,120)(NM(ITERM,K,IMODEL),K=1,2),
     1                  IONCOE(ITERM,IMODEL),IONSIG(ITERM,IMODEL)
120         FORMAT(I5,I10,5X,2E17.8)
130       CONTINUE
          MIN1=-1
          WRITE(LFNLOC,120)MIN1
200     CONTINUE
C
C CLOSE OUTPUT IONOSPHERE FILE
        CLOSE(UNIT=LFNLOC)
C
      ELSE IF (IONTYP.EQ.2) THEN
C
C PART 2: SAVE INFORMATION CONCERNING GLOBAL IONOSPHERE MODELS
C ------------------------------------------------------------
        IF (OPTGIM(5).GT.0) THEN
C
C READ A PRIORI MODELS
          CALL GTFLNA(1,'IONOS  ',FILAPR,IRCAPR)
C
          CALL GETGIM(FILAPR,1     ,NMODEL,IONREQ,IONDEV,NTERM ,
     1                NM    ,IONCOE,IONSIG,IONTXT,IONTIT,IONINF,
     2                IONTYP)
        ELSE
C
C COPY HEADER INFORMATION
          NDEG=OPTGIM(1)
          NORD=OPTGIM(2)
          NTRM=(NDEG+1)**2-(NDEG-NORD)*(NDEG-NORD+1)
C
          CALL DIMTST(1,1,2,'ATMSAV','MAXGIT',
     1      'TERMS PER GLOBAL IONOSPHERE MODEL','INCLUDE FILE USED',
     2      NTRM,MAXGIT,IRC)
C
          NMODEL=OPTGIM(7)
          CALL DIMTST(1,1,2,'ATMSAV','MAXGIM',
     1      'GLOBAL IONOSPHERE MODELS','INCLUDE FILE USED',
     2      NMODEL,MAXGIM,IRC)
C
          DO 510 IMOD=1,NMODEL
            IONTXT(IMOD)=NAMGIM(IMOD)
            IONREQ(1,IMOD)=0
            IONREQ(2,IMOD)=NDEG
            IONREQ(3,IMOD)=NORD
            IONREQ(4,IMOD)=OPTGIM(3)
            IONREQ(5,IMOD)=OPTGIM(4)
            IONREQ(6,IMOD)=OPTGIM(8)
            IONDEV(1,IMOD)=POLGIM(1,IMOD)
            IONDEV(2,IMOD)=0.D0
            IONDEV(3,IMOD)=POLGIM(2,IMOD)
            IONDEV(4,IMOD)=POLGIM(3,IMOD)
            IONDEV(5,IMOD)=EPOGIM(1,IMOD)
            IONDEV(6,IMOD)=EPOGIM(2,IMOD)
            IONDEV(9,IMOD)=INFGIM(1,IMOD)
            IONDEV(10,IMOD)=INFGIM(2,IMOD)
            NTERM(IMOD)=NTRM
510       CONTINUE
        END IF
C
C UPDATE PARAMETER INFORMATION
        DO 520 IMOD=1,NMODEL
          ITRM=0
          DO 530 IPAR=1,NPAR
            IF (LOCQ(1,IPAR).EQ.19) THEN
              IREQ=LOCQ(2,IPAR)
              XVAL=SOL(IPAR)/SCAGIM(IREQ)
              XRMS=RMS*DSQRT(QMAT(IKF(IPAR,IPAR)))/SCAGIM(IREQ)
C
C HANDLE CASE OF NEGATIVE VARIANCES
              XVAR=QMAT(IKF(IPAR,IPAR))
              IF (XVAR.GE.0.D0) THEN
                XRMS=RMS*DSQRT(XVAR)/SCAGIM(IREQ)
              ELSE
                XRMS=0.D0
                WRITE(LFNERR,940)
940             FORMAT(/,' ### SR ATMSAV: RMS ERROR SET TO ZERO ',
     1            'DUE TO NEGATIVE VARIANCE',/)
              ENDIF
C
C IONOSPHERIC COEFFICIENTS
              IF (IREQ.NE.2.AND.
     1            IMOD.EQ.LOCQ(3,IPAR)) THEN
                ITRM=ITRM+1
                IDEG=LOCQ(4,IPAR)
                IORD=LOCQ(5,IPAR)
                IF (OPTGIM(5).GT.0) THEN
                  IF (IDEG.NE.NM(ITRM,1,IMOD).OR.
     1                IORD.NE.NM(ITRM,2,IMOD)) THEN
                    WRITE(LFNERR,920)
920                 FORMAT(/,' *** SR ATMSAV: UNEXPECTED ERROR',/)
                    CALL EXITRC(2)
                  END IF
C
                  IONCOE(ITRM,IMOD)=IONCOE(ITRM,IMOD)+XVAL
                ELSE
                  NM(ITRM,1,IMOD)=IDEG
                  NM(ITRM,2,IMOD)=IORD
                  IONCOE(ITRM,IMOD)=XVAL
                END IF
C
                IONSIG(ITRM,IMOD)=XRMS
C
C SAVE PARAMETER INDEX
                IONIND(ITRM,IMOD)=IPAR
C
C INFORMATION CONCERNING LATITUDE BAND COVERED
                IF (IDEG.EQ.0) THEN
                  IONDEV(7,IMOD)=PI/648000.D0*LOCQ(6,IPAR)
                  IONDEV(8,IMOD)=PI/648000.D0*LOCQ(7,IPAR)
                END IF
C
C SINGLE-LAYER HEIGHT
              ELSE IF (IREQ.EQ.2) THEN
                IF (OPTGIM(5).EQ.1.OR.
     1              OPTGIM(5).EQ.2.AND.IMOD.EQ.LOCQ(3,IPAR)) THEN
                  IONDEV(1,IMOD)=POLGIM(1,IMOD)+XVAL
                  IONDEV(2,IMOD)=XRMS
                END IF
              END IF
            END IF
530       CONTINUE
C
          IF (ITRM.EQ.NTERM(IMOD)) THEN
            UPD(IMOD)='Y'
          ELSE
            UPD(IMOD)='N'
C
            WRITE(LFNERR,930) IONTXT(IMOD)
930         FORMAT(/,' ### SR ATMSAV: GLOBAL IONOSPHERE MODEL ',A16,
     1             ' NOT SAVED',/,
     2             16X,'BECAUSE OF INCOMPLETE PARAMETER SEQUENCE',/)
          END IF
C
520     CONTINUE
C
C WRITE IONOSPHERE OUTPUT FILE
C ----------------------------
        IF (IRCION.EQ.0) THEN
C
C ADDITIONAL INFORMATION
          IONINF(1)=NSTEFF
          IONINF(2)=NSAEFF
          IONINF(3)=IDNINT(90.D0-180.D0/PI*ZENMAX)
          IF (OPTGIM(10).EQ.2) THEN
            IONINF(4)=NSAMPL
          ELSE
            IONINF(4)=0
          ENDIF
C
          IF (OPTGIM(9).EQ.1) THEN
            IONTYP=3
            IONINF(1)=0
          ENDIF
C
          CALL SAVGIM(FILION,IONTYP,NMODEL,IONREQ,IONDEV,NTERM ,
     1                NM    ,IONCOE,IONSIG,IONTXT,TITLE ,IONINF)
        ENDIF
C
C WRITE IONEX OUTPUT FILE
C -----------------------
        IF (IRCINX.EQ.0) THEN
          IF (OPTGIM(9).EQ.1) THEN
            WRITE(LFNERR,950)
950         FORMAT(/,' ### SR ATMSAV: IONEX OUTPUT FILE NOT CREATED',/)
            RETURN
          ENDIF
C
          CALL INXSAV(NMODEL,IONREQ,IONDEV,NTERM ,NM    ,IONCOE,
     1                TITLE ,NSTEFF,NSAEFF,ZENMAX,RMS   ,QMAT  ,
     2                IONIND,SCAGIM,IORSYS)
        ENDIF
      END IF
C
      RETURN
      END SUBROUTINE

      END MODULE
