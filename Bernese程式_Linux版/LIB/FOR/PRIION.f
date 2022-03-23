      MODULE s_PRIION
      CONTAINS

C*
      SUBROUTINE PRIION(NIOREQ,IONMOD,IONREQ,TFRLST,OPTDIP,SIGDIP,
     1                  OPTGIM,POLGIM,SIGGIM,NAMGIM,EPOGIM,PRIOPT,
     2                  OPTDCB,SIGDCB)
CC
CC NAME       :  PRIION
CC
CC PURPOSE    :  PRINT IONOSPHERE MODELS AND MODEL PARAMETERS
CC
CC PARAMETERS :
CC         IN :  NIOREQ : # IONOSPHERE REQUESTS               I*4
CC               IONMOD : NUMBER  OF IONOSPHERE MODEL TO BE   I*4(1)
CC                        IMPROVED FOR REQUEST I
CC               IONREQ : IONREQ(1,I): DEGREE OF DEVELOPMENT  I*4(3,1)
CC                                     IN LATITUDE
CC                        IONREQ(2,I): DEGREE OF DEVELOPMENT
CC                                     IN HOUR ANGLE
CC                        IONREQ(3,I): MAX. DEGREE OF MIXED
CC                                     COEFFICIENTS
CC                        TFRLST(2): FIRST AND LAST OBSERV.   R*8
CC                                EPOCH OF ALL FILES
CC               OPTDIP : OPTIONS FOR DIFF. ION. PARAMETERS   I*4(3)
CC                        (1): =0: NO DIFF. ION. PARAMETERS
CC                             =1: ONE PAR. PER EPOCH AND SAT.
CC                             =2: PARAMETERS EPOCH-WISE PRE-
CC                                 ELIMINATED
CC                        (2): ELIMINATION OF REF. ION. PAR.
CC                        (3): ELEV.-DEP. PAR. CONSTRAINING
CC               SIGDIP : A PRIORI SIGMAS FOR DIFF. ION. PAR. R*8(2)
CC                        (1): ABSOLUTE SIGMA
CC                        (2): RELATIVE SIGMA IN M/MIN**1/2
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
CC                             =1: COSZ
CC                             =2: MSLM
CC                             =3: ESM
CC                        (9): STATION-SPECIFIC MODELS
CC                        (10): COMPONENT TO BE ESTIMATED
CC                              =1: DETERMINISTIC
CC                              =2: STOCHASTIC
CC               POLGIM(I,J),I=1,2,3,J=1,..,OPTGIM(7):        R*8(3,*)
CC                        I=1: HEIGHT OF SINGLE LAYER (M)
CC                        I=2: LAT. OF NORTH GEOMAGNETIC POLE
CC                        I=3: EAST LONGITUDE
CC               SIGGIM : ABSOLUTE SIGMA FOR                  R*8(*)
CC                        (1): ION. COEFFICIENTS (TECU)
CC                        (2): SINGLE-LAYER HEIGHT (M)
CC                        RELATIVE SIGMA FOR
CC                        (3): ION. COEFFICIENTS (TECU)
CC                        (4): SINGLE-LAYER HEIGHT (M)
CC               NAMGIM(I),I=1,..,OPTGIM(7): MODEL NUMBERS    CH*16(*)
CC               EPOGIM(I,J),I=1,2,J=1,..,OPTGIM(7): PERIODS  R*8(2,*)
CC                        OF VALIDITY / REF EPOCHS (MJD)
CC               PRIOPT:  PRINT OPTION                        I*4
CC               OPTDCB : OPTIONS FOR ESTIMATION OF           I*4(*)
CC                        DIFFERENTIAL CODE BIASES
CC                        (1): ESTIMATE DCBS FOR SATELLITES
CC                             =0: NO
CC                             =1: P1-P2
CC                             =2: P1-C1
CC                        (2): ESTIMATE DSBS FOR RECEIVERS
CC                             =0: NO
CC                             =1: P1-P2
CC                             =2: P1-C1
CC                        (3): REFERENCE SATELLITE NUMBER
CC                             = 0: CONSTRAIN ALL SAT
CC                             =-1: CONSTRAIN SUM OF ALL SAT
CC                        (4): PROCESS NIGHT-TIME DATA ONLY
CC               SIGDCB : A PRIORI SIGMA FOR DCBS (IN NS)     R*8(*)
CC                        (1): REFERENCE SATELLITE BIASES
CC                        (2): RECEIVER BIASES
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER, S.SCHAER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/18 18:32
CC
CC CHANGES    :  27-MAY-91 : ??: DON'T PRINT TRAILING BLANKS
CC               23-SEP-91 : ??: INCLUDE IONOS.MODEL FILENAME INTO OUTPUT
CC               14-APR-94 : SS: DIFFERENTIAL IONOSPHERE PARAMETERS
CC               06-JUN-95 : SS: GLOBAL IONOSPHERE MODEL PARAMETERS
CC               04-SEP-95 : SS: "SIGDIP(1)" GIVEN ON SINGLE DIFF. LEVEL
CC               05-DEC-95 : SS: NEW IONOSPHERE MODEL (TYPE 2)
CC               05-DEC-95 : SS: "IONO" REMOVED (SEE SR CHKION)
CC               05-DEC-95 : SS: PRINT "CONRE"
CC               20-DEC-95 : SS: DECLARE "NUMGIM" AS CH*7
CC               20-DEC-95 : SS: "IONTXT" IN CALL OF SR GETGIM
CC               20-DEC-95 : SS: DO NOT PRINT MEAN TEC / ZERO-DEGREE
CC                               COEFFICIENT FOR REGIONAL MODELS
CC               28-DEC-95 : SS: TID INDICATOR INTRODUCED
CC               05-AUG-97 : SS: ELEV.-DEP. SIP CONSTRAINING
CC               14-AUG-97 : SS: DIFFERENTIAL CODE BIASES
CC               22-OCT-97 : SS: CODE BIAS I/O FILES
CC               13-NOV-97 : SS: "IONFIL" IN CALL OF SR GETGIM
CC               17-NOV-97 : SS: "IONTIT" IN CALL OF SR GETGIM
CC               20-JAN-98 : SS: "IONINF" ADDED
CC               26-JAN-98 : SS: STATION-SPECIFIC GIMS
CC               26-JAN-98 : SS: RELATIVE SIGMA FOR GIMS
CC               24-MAR-98 : SS: FORMAT STATEMENT CORRECTED
CC               29-APR-98 : SS: DTEC LC
CC               26-MAY-98 : SS: "IONDEV" REDIMENSIONED
CC               13-APR-00 : SS: ESTIMATE (P1-C1) CODE BIASES
CC               19-SEP-01 : SS: NEW TEC MAPPING FUNCTIONS
CC               07-MAY-02 : SS: DCB UPDATE
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               05-MAR-03 : CU: REMOVE USE OF SKELETON FILE
CC               15-APR-03 : CU: BUG FIXED (FORMAT STATEMENTS)
CC               24-APR-03 : RD: CORRECTED FORMAT STATEMENT
CC               19-JAN-03 : SS/MM: REVISION OF GPSEST INPUT PANELS
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               26-MAR-12 : RD: USE TIMSTR AS MODULE NOW
CC               26-MAR-12 : RD: USE GETGIM AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: LFNPRT, LFNERR
      USE m_maxdim, ONLY: MAXGIM, MAXGIT
      USE d_const, ONLY: AE, CONRE, PI
      USE s_chkion
      USE s_getgim
      USE s_timstr
      USE s_ionosi
      USE s_exitrc
      USE s_jmt
      USE f_lengt1
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , ICBTYP, ID1   , ID2   , ID3   , II    , IM1   ,
     1          IM2   , IM3   , IMOD  , IONTYP, IPRTCO, IRC   , IREQ  ,
     2          ITERM , ITRM  , IY1   , IY2   , IY3   , NCOE  ,
     3          NDEG  , NHGT  , NIOREQ, NMOD  , NMODEL, NORD
C
      REAL*8    DAY1  , DAY2  , DAY3  , HOUR1 , HOUR2 , HOUR3 , XAE   ,
     1          XDEV1 , XDEV3 , XDEV4 , XDEV7 , XDEV8 , XHGT  , XLAT  ,
     2          XLON  , XRE   , XSIG1 , XSIG2 , XSIG3 , XSIG4
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*80  IONTIT
      CHARACTER*36  EPOSTR
      CHARACTER*32  IONFIL
      CHARACTER*32  FILNAM
      CHARACTER*16  STRPOL,NAMGIM(*),IONTXT(MAXGIM),IONLBL(3)
      CHARACTER*13  ESTCOM(2)
      CHARACTER*5   DCBTYP(3)
      CHARACTER*4   MAPFUN(4)
      REAL*8        IONDEV(10,MAXGIM),TFRLST(2),SIGDIP(*),SIGDCB(*)
      REAL*8        POLGIM(3,*),SIGGIM(*),EPOGIM(2,*)
      REAL*8        IONCOE(MAXGIT,MAXGIM),IONSIG(MAXGIT,MAXGIM)
      INTEGER*4     IONMOD(*),IONREQ(3,*),OPTDIP(*),IONINF(4)
      INTEGER*4     IONRQ1(6,MAXGIM),NTERM(MAXGIM),NM(MAXGIT,2,MAXGIM)
      INTEGER*4     OPTGIM(*),PRIOPT,OPTDCB(*)
C
      COMMON/CPRION/IONDEV,IONCOE,IONSIG,IONRQ1,NM,IONTXT
C
C
      DATA MAPFUN/'NONE','COSZ','MSLM','ESM'/
      DATA ESTCOM/'DETERMINISTIC','STOCHASTIC'/
      DATA IONLBL/'LOCAL','GLOBAL','STATION-SPECIFIC'/
      DATA DCBTYP/'P1-P2','P1-C1','LC'/
C
C GET TYPE CODE OF IONOSPHERE MODELS
C ----------------------------------
      CALL CHKION(IONTYP)
C
C WRITE TITLE LINES
C -----------------
      CALL GTFLNA(0,'IONOS  ',FILNAM,IRC)
      WRITE(LFNPRT,"(
     1     ' '
     2  ,/,' '
     3  ,/,' IONOSPHERE MODELS: ',24X,A
     4  ,/,' -----------------'
     5  ,/,1X)") FILNAM(1:LENGT1(FILNAM))
C
C PRINT INFORMATION CONCERNING A PRIORI IONOSPHERE MODELS
C -------------------------------------------------------
      IF (IONTYP.EQ.0) THEN
C
C NO IONOSPHERE MODELS AVAILABLE
        WRITE(LFNPRT,1) ' NO IONOSPHERE MODELS APPLIED'
1       FORMAT(A)
C
      ELSE IF (IONTYP.EQ.1) THEN
C
C PART 1: LOCAL IONOSPHERE MODELS
C -------------------------------
        XAE=AE/1000.D0
C
        WRITE(LFNPRT,911) IONLBL(IONTYP),XAE
911     FORMAT(' TYPE OF IONOSPHERE MODELS :  ',A,
     1       /,' RADIUS OF THE EARTH       :',F9.2,' KM',/)
C
        CALL IONOSI(1,NMODEL,IONRQ1,IONDEV,
     1              NTERM,NM,IONCOE,IONSIG)
C
        WRITE(LFNPRT,"(
     1       '        DEG. OF DEVELOP.                            '
     1      ,'              ORIGIN OF DEVELOPMENT          HEIGHT '
     1      ,'     NORMAIZATION FACTORS'
     2    ,/,' MODEL  TIME LAT. MIXED   VALIDITY START   VALIDITY '
     2      ,'END      LOCAL TIME        LAT.(D) LONG.(D)   (KM)  '
     2      ,'   TIME(H) LAT.(D) ELE.CONT.'
     3    ,/,1X,131('-')
     4    ,/,1X)")
C
C IONOSPHERE MODELS
C -----------------
        DO 40 IMOD=1,NMODEL
          IF(IONDEV(4,IMOD).GT.TFRLST(2).OR.IONDEV(5,IMOD).LT.TFRLST(1))
     1      GOTO 40
          CALL JMT(IONDEV(4,IMOD),IY1,IM1,DAY1)
          CALL JMT(IONDEV(5,IMOD),IY2,IM2,DAY2)
          CALL JMT(IONDEV(2,IMOD),IY3,IM3,DAY3)
          ID1=DAY1
          ID2=DAY2
          ID3=DAY3
          HOUR1=(DAY1-ID1)*24.D0
          HOUR2=(DAY2-ID2)*24.D0
          HOUR3=(DAY3-ID3)*24.D0
          WRITE(LFNPRT,2) IMOD,IONRQ1(2,IMOD),IONRQ1(1,IMOD),
     1                    IONRQ1(3,IMOD),IY1,IM1,ID1,HOUR1,
     1                    IY2,IM2,ID2,HOUR2,IY3,IM3,ID3,HOUR3,
     2                    IONDEV(1,IMOD)*180/PI,IONDEV(3,IMOD)*180/PI,
     3                    IONRQ1(4,IMOD),
     4                    IONDEV(8,IMOD),IONDEV(7,IMOD),IONDEV(6,IMOD)
2         FORMAT(I5,1X,3I5,3X,2(I6,2I3,F5.1),3X,I4,2I3,F5.1,F8.2,F9.2,
     1           I9,2X,2F8.2,D11.2)
40      CONTINUE
C
C IONOSPHERE MODEL COEFFICIENTS
C -----------------------------
        IPRTCO=1
C
        DO 70 IMOD=1,NMODEL
          IF(IONDEV(4,IMOD).GT.TFRLST(2).OR.IONDEV(5,IMOD).LT.TFRLST(1))
     1      GOTO 70
          IF(IPRTCO.EQ.1.AND.NTERM(IMOD).GT.0) THEN
            WRITE(LFNPRT,"(
     1           ' '
     2        ,/,'                POL. DEGREE IN'
     3        ,/,' MODEL  TERM    TIME    LATIT.     COEFFICIENT     '
     3          ,' SIGMA'
     4        ,/,1X,131('-')
     5        ,/,1X)")
            IPRTCO=0
          ENDIF
          DO 60 ITERM=1,NTERM(IMOD)
            IF(ITERM.EQ.1) THEN
              WRITE(LFNPRT,3) IMOD,ITERM,NM(ITERM,2,IMOD),
     1                        NM(ITERM,1,IMOD),IONCOE(ITERM,IMOD),
     2                        IONSIG(ITERM,IMOD)
3             FORMAT(I5,I6,I8,I9,4X,2E15.6)
            ELSE
              WRITE(LFNPRT,4)      ITERM,NM(ITERM,2,IMOD),
     1                        NM(ITERM,1,IMOD),IONCOE(ITERM,IMOD),
     2                        IONSIG(ITERM,IMOD)
4             FORMAT(5X,I6,I8,I9,4X,2E15.6)
            ENDIF
60        CONTINUE
          WRITE(LFNPRT,5)
5         FORMAT(' ')
70      CONTINUE
C
      ELSE IF (IONTYP.GE.2) THEN
C
C PART 2: GLOBAL IONOSPHERE MODELS
C --------------------------------
        XRE=CONRE/1000.D0
C
        WRITE(LFNPRT,912) IONLBL(IONTYP),XRE
912     FORMAT(' TYPE OF IONOSPHERE MODELS :  ',A,
     1       /,' MEAN RADIUS OF THE EARTH  :',F9.2,' KM')
C
        CALL GTFLNA(1,'IONOS  ',IONFIL,IRC)
C
        CALL GETGIM(IONFIL,1     ,NMODEL,IONRQ1,IONDEV,NTERM ,
     1              NM    ,IONCOE,IONSIG,IONTXT,IONTIT,IONINF,
     2              IONTYP)
C
C DO NOT ACCEPT DTEC INFORMATION
        IF (IONINF(4).GT.0) THEN
          WRITE(LFNERR,913)
913       FORMAT(/,' *** SR PRIION: DTEC INFORMATION FOUND',/)
          CALL EXITRC(2)
        ENDIF
C
C PRINT HEADER INFORMATION
        WRITE(LFNPRT,300)
300     FORMAT(/,' MODEL /            VALIDITY START /   VALIDITY END',
     1           '       MAX DEG/ORD  #COE  FLAGS     HGT (KM)',
     2           '  GEOMAGNETIC POLE  LATITUDE BAND',
     3         /,' STATION            REFERENCE EPOCH                ',
     4           '                          F1 F2 F3          ',
     5           '  LAT     LON       MIN/MAX LAT',
     6    /,1X,131('-'),/)
C
        DO 310 IMOD=1,NMODEL
          IF (IONDEV(6,1).NE.0.D0) THEN
            CALL TIMSTR(2,IONDEV(5,IMOD),EPOSTR)
          ELSE
            CALL TIMSTR(1,IONDEV(5,IMOD),EPOSTR)
          END IF
C
          XDEV1=IONDEV(1,IMOD)/1000.D0
          XDEV3=180.D0/PI*IONDEV(3,IMOD)
          XDEV4=180.D0/PI*IONDEV(4,IMOD)
          IF (IONRQ1(4,IMOD).EQ.1) THEN
            STRPOL='    ---     --- '
          ELSE
            WRITE(STRPOL,'(2F8.2)') XDEV3,XDEV4
          ENDIF
          XDEV7=180.D0/PI*IONDEV(7,IMOD)
          XDEV8=180.D0/PI*IONDEV(8,IMOD)
C
          WRITE(LFNPRT,301) IONTXT(IMOD),EPOSTR,IONRQ1(2,IMOD),
     1      IONRQ1(3,IMOD),NTERM(IMOD),(IONRQ1(I,IMOD),I=4,6),
     2      XDEV1,STRPOL,XDEV7,XDEV8
301       FORMAT(1X,A16,3X,A36,I5,I4,I9,2X,3I3,F9.2,2X,A16,2X,2F8.2)
310     CONTINUE
C
C PRINT COEFFICIENTS
        IF (PRIOPT.EQ.1) THEN
          WRITE(LFNPRT,302)
302       FORMAT(//,' MODEL / STATION     DEG  ORD   VALUE (TECU)',
     1      '   RMS (TECU)',
     2      /,1X,131('-'))
C
          DO 320 IMOD=1,NMODEL
            DO 321 ITRM=1,NTERM(IMOD)
              IF (ITRM.EQ.1) THEN
                WRITE(LFNPRT,303) IONTXT(IMOD),(NM(1,I,IMOD),I=1,2),
     1            IONCOE(ITRM,IMOD),IONSIG(ITRM,IMOD)
303             FORMAT(/,1X,A16,2X,2I5,F16.8,F11.4)
              ELSE
                WRITE(LFNPRT,304) (NM(ITRM,I,IMOD),I=1,2),
     1            IONCOE(ITRM,IMOD),IONSIG(ITRM,IMOD)
304             FORMAT(19X,2I5,F16.8,F11.4)
              END IF
321         CONTINUE
320       CONTINUE
        END IF
      END IF
C
C LOCAL IONOSPHERE MODEL PARAMETERS
C ---------------------------------
      IF(NIOREQ.NE.0) THEN
C
        WRITE(LFNPRT,"(
     1       ' '
     2    ,/,' '
     3    ,/,' LOCAL IONOSPHERE MODEL PARAMETERS:'
     4    ,/,' ---------------------------------'
     5    ,/,' '
     6    ,/,'        DEGREE OF DEVELOPMENT IN'
     7    ,/,' MODEL    TIME   LATIT.  MIXED'
     8    ,/,1X,131('-')
     9    ,/,1X)")
C
        DO 100 IREQ=1,NIOREQ
          WRITE(LFNPRT,6) IONMOD(IREQ),IONREQ(2,IREQ),IONREQ(1,IREQ),
     1                    IONREQ(3,IREQ)
6         FORMAT(I5,3I8)
100     CONTINUE
      ENDIF
C
C GLOBAL IONOSPHERE MODEL PARAMETERS
C ----------------------------------
      NMOD=OPTGIM(7)
C
      IF (NMOD.GT.0) THEN
        NDEG=OPTGIM(1)
        NORD=OPTGIM(2)
        NCOE=(NDEG+1)**2-(NDEG-NORD)*(NDEG-NORD+1)
C
        WRITE(LFNPRT,210) (OPTGIM(II),II=1,2),NCOE,NMOD
210     FORMAT(//,' GLOBAL IONOSPHERE MODEL PARAMETERS:',
     1    /,1X,34('-'),
     2         //,' MAXIMUM DEGREE OF SPHERICAL HARMONICS     :',I6,
     3          /,' MAXIMUM ORDER                             :',I6,
     4          /,' NUMBER OF COEFFICIENTS PER MODEL          :',I6,
     5          /,' NUMBER OF MODELS                          :',I6)
C
        WRITE(LFNPRT,1) ' DEVELOPMENT WITH RESPECT TO'
        IF (OPTGIM(3).EQ.1) THEN
          WRITE(LFNPRT,1) '   GEOGRAPHICAL REFERENCE FRAME'
        ELSE
          WRITE(LFNPRT,1) '   GEOMAGNETIC REFERENCE FRAME'
        END IF
        IF (OPTGIM(4).EQ.1) THEN
          WRITE(LFNPRT,1) '   MEAN POSITION OF THE SUN'
        ELSE
          WRITE(LFNPRT,1) '   TRUE POSITION OF THE SUN'
        END IF
C
        WRITE(LFNPRT,220) MAPFUN(OPTGIM(8)+1)
220     FORMAT(' MAPPING FUNCTION                          :  ',A)
C
        WRITE(LFNPRT,225) ESTCOM(OPTGIM(10))
225     FORMAT(' ESTIMATED COMPONENT                       :  ',A)
C
        IF (OPTGIM(5).EQ.0) THEN
          WRITE(LFNPRT,211)
211       FORMAT(' NO ESTIMATION OF SINGLE-LAYER HEIGHT',/)
        ELSE
          IF (OPTGIM(5).EQ.1) THEN
            NHGT=1
          ELSE
            NHGT=NMOD
          END IF
          WRITE(LFNPRT,212) NHGT
212       FORMAT(' ESTIMATION OF SINGLE-LAYER HEIGHT',
     1         /,' TOTAL NUMBER OF HEIGHT PARAMETERS         :',I6,/)
        END IF
C
        XRE=CONRE/1000.D0
C
        XHGT=0.D0
        DO 219 IMOD=1,NMOD
          XHGT=XHGT+POLGIM(1,IMOD)
219     CONTINUE
        XHGT=XHGT/NMOD/1000.D0
C
        WRITE(LFNPRT,213) XRE,XHGT
213     FORMAT(' MEAN RADIUS OF THE EARTH                  :',F9.2,
     1    ' KM',
     2       /,' A PRIORI HEIGHT OF SINGLE LAYER           :',F9.2,
     3    ' KM')
C
        IF (OPTGIM(3).EQ.2.AND.OPTGIM(5).EQ.0) THEN
          XLAT=180.D0/PI*POLGIM(2,1)
          XLON=180.D0/PI*POLGIM(3,1)
          WRITE(LFNPRT,214) XLAT,XLON
214       FORMAT(' COORDINATES OF EARTH-CENTERED DIPOLE AXIS',
     1         /,'   LATITUDE OF NORTH GEOMAGENTIC POLE      :',F9.2,
     2      ' DEGREES',
     3         /,'   EAST LONGITUDE                          :',F9.2,
     4      ' DEGREES')
        END IF
C
        XSIG1=SIGGIM(1)
        XSIG2=SIGGIM(2)/1000.D0
        XSIG3=SIGGIM(3)
        XSIG4=SIGGIM(4)/1000.D0
        WRITE(LFNPRT,215) XSIG1,XSIG2,XSIG3,XSIG4
215     FORMAT(' ABSOLUTE A PRIORI PRIORI SIGMA FOR',
     1    /,'   IONOSPHERIC COEFFICIENTS                :',F9.2,' TECU',
     2    /,'   SINGLE-LAYER HEIGHT                     :',F9.2,' KM',
     3    /,' RELATIVE A PRIORI PRIORI SIGMA FOR',
     4    /,'   IONOSPHERIC COEFFICIENTS                :',F9.2,' TECU',
     5    /,'   SINGLE-LAYER HEIGHT                     :',F9.2,' KM',/)
C
        IF (OPTGIM(6).EQ.1) THEN
          IF (OPTGIM(9).EQ.0) THEN
            WRITE(LFNPRT,216)
216         FORMAT(' MODEL NUMBER         VALIDITY START',
     1        '     VALIDITY END',
     2        /,1X,131('-'),/)
          ELSE
            WRITE(LFNPRT,221)
221         FORMAT(' STATION NAME         VALIDITY START',
     1        '     VALIDITY END',
     2        /,1X,131('-'),/)
          ENDIF
          DO 230 IMOD=1,NMOD
            CALL TIMSTR(2,EPOGIM(1,IMOD),EPOSTR)
            WRITE(LFNPRT,217) NAMGIM(IMOD),EPOSTR
217         FORMAT(1X,A16,5X,A36)
230       CONTINUE
        ELSE
          IF (OPTGIM(9).EQ.0) THEN
            WRITE(LFNPRT,218)
218         FORMAT(' MODEL NUMBER         REFERENCE EPOCH',
     1        /,1X,131('-'),/)
          ELSE
            WRITE(LFNPRT,222)
222         FORMAT(' STATION NAME         REFERENCE EPOCH',
     1        /,1X,131('-'),/)
          ENDIF
          DO 231 IMOD=1,NMOD
            CALL TIMSTR(1,EPOGIM(1,IMOD),EPOSTR)
            WRITE(LFNPRT,217) NAMGIM(IMOD),EPOSTR
231       CONTINUE
        END IF
      END IF
C
C DIFFERENTIAL IONOSPHERE PARAMETERS
C ----------------------------------
      IF (OPTDIP(1).NE.0) THEN
C
        WRITE(LFNPRT,"(
     1       ' '
     2    ,/,' '
     3    ,/,' STOCHASTIC IONOSPHERE PARAMETERS:'
     4    ,/,' --------------------------------'
     5    ,/,1X)")
C
        IF(OPTDIP(1).EQ.1) WRITE(LFNPRT,"(
     1    ' ONE PARAMETER PER EPOCH AND SATELLITE ESTIMATED')")
        IF(OPTDIP(1).EQ.2) WRITE(LFNPRT,"(
     1    ' ONE PARAMETER PER EPOCH AND SATELLITE ESTIMATED ',
     1    '(EPOCH-WISE PRE-ELIMINATED)')")
C
        IF(OPTDIP(2).EQ.0) WRITE(LFNPRT,"(
     1    ' NO ELIMINATION OF REFERENCE IONOSPHERE PARAMETERS')")
        IF(OPTDIP(2).EQ.1) WRITE(LFNPRT,"(
     1    ' ELIMINATION OF REFERENCE IONOSPHERE PARAMETERS')")
C
        IF(OPTDIP(3).EQ.0) WRITE(LFNPRT,"(
     1    ' ELEVATION-INDEPENDENT PARAMETER CONSTRAINING ',
     1    '(LINE-OF-SIGHT PARAMETERS ESTIMATED)')")
        IF(OPTDIP(3).EQ.1) WRITE(LFNPRT,"(
     1    ' ELEVATION-DEPENDENT PARAMETER CONSTRAINING ',
     1    '(ZENITH PARAMETERS ESTIMATED)')")
C
        WRITE(LFNPRT,141) (SIGDIP(I),I=1,2)
141     FORMAT(/,' ABSOLUTE A PRIORI SIGMA ON SINGLE DIFFERENCE LEVEL:',
     1         F8.4,' M',
     2         /,' RELATIVE A PRIORI SIGMA OF IONOSPHERIC RANDOM WALK:',
     3         F8.4,' M/MIN**1/2')
      ENDIF
C
C DIFFERENTIAL CODE BIASES
C ------------------------
      ICBTYP=OPTDCB(1)
      IF (ICBTYP.EQ.0) ICBTYP=IABS(OPTDCB(2))
      IF (ICBTYP.GT.0) THEN
        WRITE(LFNPRT,400)
400     FORMAT(//,' DIFFERENTIAL CODE BIASES:',/,1X,24('-'),/)
C
        WRITE(LFNPRT,407) DCBTYP(ICBTYP)
407     FORMAT(' TYPE OF BIASES                            :  ',A5,/)
C
        IF (OPTDCB(1).GT.0) THEN
          WRITE(LFNPRT,401)
401       FORMAT(' SATELLITE BIASES ESTIMATED')
        ENDIF
        IF (OPTDCB(2).GT.0) THEN
          WRITE(LFNPRT,402)
402       FORMAT(' RECEIVER  BIASES ESTIMATED')
        ELSEIF (OPTDCB(2).LT.0) THEN
          WRITE(LFNPRT,410)
410       FORMAT(' RECEIVER-SPECIFIC MULTIPLIERS ESTIMATED')
        ENDIF
C
        IF (OPTDCB(1).GT.0) THEN
          IF (OPTDCB(3).GT.0) THEN
            WRITE(LFNPRT,403) OPTDCB(3)
403         FORMAT(/,' REFERENCE SATELLITE NUMBER                :',
     1        I6.2)
          ELSEIF (OPTDCB(3).EQ.0) THEN
            WRITE(LFNPRT,404)
404         FORMAT(/,' ALL SATELLITE BIASES CONSTRAINED')
          ELSE
            WRITE(LFNPRT,405)
405         FORMAT(/,' SUM OF ALL SATELLITE BIASES CONSTRAINED')
          ENDIF
        ENDIF
C
        IF (OPTDCB(1).GT.0) THEN
          WRITE(LFNPRT,408) SIGDCB(1)
408       FORMAT(/,' A PRIORI SIGMA FOR REFERENCE SATELLITES   :',
     1      F10.3,' NANOSEC')
        ENDIF
        IF (OPTDCB(2).GT.0) THEN
          WRITE(LFNPRT,409) SIGDCB(2)
409       FORMAT(/,' A PRIORI SIGMA FOR RECEIVERS              :',
     1      F10.3,' NANOSEC')
        ENDIF
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
