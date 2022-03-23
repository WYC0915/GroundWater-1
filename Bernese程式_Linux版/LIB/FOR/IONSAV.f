      MODULE s_IONSAV
      CONTAINS

C*
      SUBROUTINE IONSAV(MODTYP,TITLE,NPAR,RMS,LOCQ,SOL,QMAT,IONDEV,
     1                  IONREQ,NTERM,NM,IONCOE,IONSIG,HION)
CC
CC NAME       :  IONSAV
CC
CC PURPOSE    :  SAVE COEFFICIENTS OF IONOSPHERE MODELS
CC               COMPUTED BY PROGRAM IONEST
CC
CC PARAMETERS :
CC         IN :  MODTYP : MODEL TYPE (1: SINGLE LAYER)        I*4
CC               TITLE  : GENERAL TITLE                       CH*80
CC               NPAR   : NUMBER OF PARAMETERS ESTIMATED      I*4
CC               RMS    : ESTIMATED MEAN ERROR OF UNIT WEIGHT R*8
CC               LOCQ(K,I),K=1,.,MAXLCQ,I=1,..,NPAR: DESCRIP- I*4
CC                        TION OF PARAMETER NUMBER I
CC               SOL(I),I=1,2,...,NPAR: SOLUTION VECTOR       R*8
CC               QMAT(I),I=1,2,... : LINEARIZED NORMAL EQN.-  R*8
CC                        MATRIX
CC               IONREQ(K,I),K=1,2,3,4,I=1,2,...,NMODEL       I*2
CC                     (1,I): DEGREE OF DEVELOPMENT IN LATITUDE
CC                     (2,I): DEGREE OF DEVELOPMENT IN TIME
CC                     (3,I): MAXIMUM DEGREE FOR MIXED COEFFICIENTS
CC                     (4,I): HEIGHT OF SINGLE LAYER (KM)
CC               IONDEV(K,I),K=1,2,3,4,I=1,2,...,NMODEL       R*8
CC                     (1,I): LATITUDE ORIGIN OF DEVELOPMENT
CC                     (2,I): TIME ORIGIN OF DEVELOPMENT
CC                     (3,I): LONGITUDE ORIGIN OF DEVELOPMENT
CC                     (4,I): MINIMUM TIME OF VALIDITY FOR MODEL I
CC                     (5,I): MAXIMUM TIME OF VALIDITY FOR MODEL I
CC                     (6,I): NORMALIZATION FACTOR FOR ELECTRON NUMBER
CC                     (7,I): NORMALIZATION FACTOR FOR LATITUDE (DEG)
CC                     (8,I): NORMALIZATION FACTOR FOR TIME (HOURS)
CC               NTERM(I),I=1,2,...,NMODEL: NUMBER OF TERMS GIVEN
CC                            A PRIORI FOR MODEL I                I*2
CC               NM(K,J,I),K=1,2,...,NTERM,J=1,2,I=1,2,...,NMODEL I*2
CC                 (K,1,I): LATITUDE DEGREE OF TERM K OF MODEL I
CC                 (K,2,I): TIME DEGREE OF TERM K OF MODEL I
CC               IONCOE(K,I),K=1,2,...,NTERM,I=1,2,...,NMODEL:    R*8
CC                          COEFFICIENT K
CC               IONSIG(K,I),K=1,2,...,NTERM,I=1,2,...,NMODEL     R*8
CC                          A PRIORI SIGMA
CC               HION  : HEIGHT OF IONOSPHERIC LAYER (M)          R*8
CC
CC REMARKS    :  ADAPTED (WITH SMALL CHANGES) FROM SR ATMSAV
CC
CC AUTHOR     :  U.WILD
CC
CC VERSION    :  1.0
CC
CC CREATED    :  88/12/27 16:00
CC
CC CHANGES    :  28-DEC-92 : ??: USE OF SR "OPNFIL" TO OPEN FILES
CC               28-JAN-03 : RS: IONCOE,IONSIG REAL*4 -> REAL*8
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE d_const, ONLY: PI
      USE s_opnfil
      USE s_opnerr
      USE s_jmt
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 ID0   , ID1   , ID2   , IFIRST, IHION , ILAT  , IOSTAT,
     1          IPAR  , IRC   , ITERM , ITIME , ITYP  , J0    , J1    ,
     2          J2    , K     , LFNION, M0    , M1    , M2    , MIN1  ,
     3          MODTYP, MXCFIL, MXCLCQ, MXCMOD, MXCPAR, MXCSAT, MXCTRM,
     4          NH    , NMOD  , NPAR
C
      REAL*8    D0    , D1    , D2    , H0    , H1    , H2    , HION  ,
     1          RMS   , SIG   , XMJD0 , XMJD1 , XMJD2
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER*4 LOCQ(MXCLCQ,MXCPAR)
C
      INTEGER*4 IONREQ(4,MXCMOD),NTERM(MXCMOD),NM(MXCTRM,2,MXCMOD)
C
      REAL*8 IONCOE(MXCTRM,MXCMOD),IONSIG(MXCTRM,MXCMOD)
C
      REAL*8 IONDEV(8,MXCMOD)
      REAL*8 SOL(*),QMAT(*)
C
      CHARACTER*32 FILION
      CHARACTER*6  MXNLCQ,MXNMOD,MXNTRM,MXNPAR,MXNFIL,MXNSAT
      CHARACTER TITLE*80,TEXT(16)*48
C
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
      COMMON/MCMMOD/MXCMOD,MXNMOD
      COMMON/MCMTRM/MXCTRM,MXNTRM
      COMMON/MCMPAR/MXCPAR,MXNPAR
      COMMON/MCMFIL/MXCFIL,MXNFIL
      COMMON/MCMSAT/MXCSAT,MXNSAT
C
C
      DATA IFIRST/1/
C
      LFNION = LFNRES
C
C OPEN IONOSPHERE OUTPUT FILE
C ---------------------------
      IF (IFIRST .EQ. 1) THEN
        CALL GTFLNA(0,'IONOSRS',FILION,IRC)
        IF(IRC.NE.0) GOTO 999
        CALL OPNFIL(LFNION,FILION,'UNKNOWN','FORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNION,IOSTAT,FILION,'IONSAV')
C
C WRITE TITLE TO OUTPUT FILE
C --------------------------
        WRITE(LFNION,1) TITLE
1       FORMAT(A80,/,80('-'))
C
        IFIRST = 0
      ENDIF
C
      IF (MODTYP .EQ. 1) THEN
C
C UPDATE IONOSPHERE MODEL (SINGLE LAYER MODEL)
C --------------------------------------------
        DO 10 IPAR=1,NPAR
          ITYP=LOCQ(1,IPAR)
          IF(ITYP.NE.7)GO TO 10
          NMOD=LOCQ(2,IPAR)
          ILAT=LOCQ(3,IPAR)
          ITIME=LOCQ(4,IPAR)
          SIG=RMS*DSQRT(QMAT(IPAR*(IPAR+1)/2))
C
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
     1       IONREQ(3,NMOD))THEN
             IONREQ(3,NMOD)=NM(NH,1,NMOD)+NM(NH,2,NMOD)
          END IF
10      CONTINUE
C
C DEFINE TEXT FOR OUTPUT FILE
C ---------------------------
        TEXT(1)='IONOSPHERE MODEL NUMBER                        :'
        TEXT(2)='TYPE OF IONOSPHERE MODEL                       :'
        TEXT(3)='ORIGIN OF DELEVOPMENT: TIME (UT)  (Y M D H)    :'
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
        XMJD0=IONDEV(2,NMOD)
        XMJD1=IONDEV(4,NMOD)
        XMJD2=IONDEV(5,NMOD)
        CALL JMT(XMJD0,J0,M0,D0)
        CALL JMT(XMJD1,J1,M1,D1)
        CALL JMT(XMJD2,J2,M2,D2)
        ID0=D0
        ID1=D1
        ID2=D2
        H0=24*(D0-ID0)
        H1=24*(D1-ID1)
        H2=24*(D2-ID2)
        IONDEV(1,NMOD)=IONDEV(1,NMOD)*180/PI
        IONDEV(3,NMOD)=IONDEV(3,NMOD)*180/PI
        IHION = IDINT(HION/1.D3)
C
C WRITE HEADER FOR MODEL NMOD
C ---------------------------
        WRITE(LFNION,15) TEXT(1),NMOD,TEXT(2),MODTYP,
     1                    TEXT(3),J0,M0,ID0,H0,
     2                    TEXT(4),IONDEV(1,NMOD),
     3                    TEXT(5),IONDEV(3,NMOD),
     4                    TEXT(6),IHION,
     5                    TEXT(7),IONREQ(2,NMOD),
     6                    TEXT(8),IONREQ(1,NMOD),
     7                    TEXT(9),IONREQ(3,NMOD),
     8                    TEXT(10),IONDEV(7,NMOD),
     9                    TEXT(11),IONDEV(8,NMOD),
     *                    TEXT(12),IONDEV(6,NMOD),
     A                    TEXT(13),J1,M1,ID1,H1,
     B                    TEXT(14),J2,M2,ID2,H2,
     D                    TEXT(15),TEXT(16)
15      FORMAT(A48,I5,/,A48,I5/,A48,I5,2I3,F5.1,/,
     1         2(A48,F10.4,/),4(A48,I5,/),2(A48,F7.2,/),
     2         A48,D15.2,/,2(A48,I5,2I3,F5.1,/),A48,/,A48)
C
C WRITE TERMS AND ASSOCIATED RMS-ERRORS
C -------------------------------------
        DO 20 ITERM=1,NTERM(NMOD)
          WRITE(LFNION,21) (NM(ITERM,K,NMOD),K=1,2),
     1                     IONCOE(ITERM,NMOD),IONSIG(ITERM,NMOD)
21        FORMAT(I5,I10,5X,2E17.8)
20      CONTINUE
        MIN1=-1
        WRITE(LFNION,21)MIN1
C
      ENDIF
C
999   RETURN
      END SUBROUTINE

      END MODULE
