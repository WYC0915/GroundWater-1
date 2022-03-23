C*
      PROGRAM STDPRE
CC
CC NAME       :  STDPRE
CC
CC PURPOSE    :  GENERATE A PRECISE ORBIT FILE (REMONDI FORMAT) FROM
CC               A STANDARD ORBIT FILE
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER, G.BEUTLER, L.MERVART
CC
CC CREATED    :  86/08/21 10:23
CC
CC CHANGES    :  23-APR-92 : ??: NEW INTERNAL FILE NAME "PREOUT"
CC               30-APR-92 : ??: NEW NGS FORMAT, RDPREI CALL CHANGED
CC               04-JUN-92 : ??: J2000.0; OPNFIL IMPLEMENTED;
CC                               NEW CALLS GETORB AND ORBINF
CC               10-JUN-92 : ??: POSSIBILITY TO APPEND THE INFORMATION
CC                               ABOUT ACCURACY
CC               20-JUN-92 : ??: CHANGE THE NEW FORMAT ACCORDING TO
CC                               CLYDE GOAD
CC               23-JUL-92 : ??: CORRECT SATELLITE LIST DUE TO MANOEUVRES
CC               09-DEC-92 : ??: CORRECT ORDERING OF SATELLITES IN
CC                               ASCENDING ORDER
CC               05-MAY-93 : ??: USE SATELLITE CLOCK FILE IF EXISTS
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               22-DEC-93 : RW: PRINT GENERAL FILE NAMES: PRFLNA
CC               28-MAR-94 : MR: PROBLEM IF ONLY MANOEUVRE SATELLITE
CC               10-AUG-94 : MR: CALL EXITRC
CC               06-SEP-94 : MR: GTSCLK WITH RETURN CODE
CC               14-DEC-94 : LM: ARCINT INTEGER*4
CC               05-APR-95 : MR: USE IDINT TO COMPUTE NUMBER OF EPOCHS
CC               15-APR-96 : TS: HANDLE SATELLITES MANOEUVRES CORRECTLY
CC               05-JUN-96 : TS: CALL POLDEF CHANGED DUE TO SUBDAILY POLE
CC               06-JUN-96 : MR: CORRECT TFIRST, TLAST FOR SAT. CHECK
CC               22-OCT-96 : TS: ADDED "SPV" OUTPUT FORMAT (SP3 + VEL)
CC               16-JUL-97 : MR: SR "RDPREI" RENAMED TO "BPINPT"
CC               24-SEP-97 : DI: USE MAXSAT.inc
CC               04-JUN-98 : TS: CHANGES FOR GLONASS (MODSVN)
CC               01-DEC-98 : DI: DTSATC/DTSATD CHANGED (IN CASE OF NO CLOCKS)
CC               06-JUN-00 : DI: CORRECT MODSVN CALL, INTRODUCE ISYS
CC               01-NOV-00 : CU: SWITCH TO THE NEW MENU SYSTEM
CC               18-DEC-00 : HU: USE INTERFACE FOR PRFLNA
CC               18-OCT-01 : MM: NEW OPTION (EXCLUDE SATELLITES WHICH ARE
CC                               IN SATCRUX FILE)
CC               25-SEP-02 : HU: REMOVE I_ASTLIB
CC               13-NOV-02 : HU: SP3C IMPLEMENTED
CC               02-FEB-03 : HU: EXLUDE SAT THAT ARE BAD AT END OF FIT INTERVAL
CC               06-FEB-03 : SS: "USECRX" PARAMETER REMOVED
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               20-MAY-03 : RD: INIT TIME WINDOW TO (/ 0D0, 1D20 /)
CC               11-JUN-03 : HU: USE GSTIME
CC               14-JUN-03 : HU: DESACTIVATE USE OF GMST2000
CC               04-AUG-03 : HU: NEW OPTION TO EXCLUDE SAT. FOR PREDICTION
CC               05-AUG-03 : HU: ERROR CONCERNING REMOVAL OF SAT CORRECTED
CC               06-AUG-03 : HU: NEW CALL GSTIME
CC               28-AUG-03 : HU: PREDICTION FOR BAD SATELLITES CORRECTED
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               04-DEC-03 : HB: REMOVE ONLY FOR M_BERN,
CC                               COMMENT INCLUDE 'COMLFNUM.inc'
CC               07-JAN-04 : HU: WRITE MANEUVER FLAG
CC                               CHECK MANEUVERS FOR PREDICTION
CC               29-JUN-04 : HU/RD/SS: COMPUTE POLE INFO ONLY ONCE PER EPOCH;
CC                               GAIN: FACTOR OF 15 FOR 39 SATELLITES
CC               04-NOV-04 : HU: DO NOT WRITE MANEUVRED SATELLITES FOR PREDI
CC               06-NOV-04 : RD: CONSIDER ONLY MAN. AFTER TFIRST
CC               16-NOV-04 : HU: CONSIDER MAN. AFTER TPRED
CC               21-MAY-05 : HU: NUTEFF TIME STEP 2h -> 1h
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: LFNUM.inc REMOVED (LFNUMs IN M_BERN)
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               07-JUL-05 : HB: USE T_EPOCH FOR GSTIME
CC                               ADD BIAS TO PARAMETER LIST OF SR NUTEFF
CC               01-AUG-05 : HU: EPOCH AS STRUCTURE
CC               08-Aug-05 : HB: USE NEW SR TIMST2 (MODULE)
CC               18-JUL-06 : AG: CMC IMPLEMENTED
CC               09-AUG-06 : AG: ATL INFO IN TITLE LINE ADDED
CC               15-AUG-06 : AG: CALL FOR GETCMC MOVED
CC               21-AUG-06 : JD/HU: READ MORE THAN JUST A SINGLE ARC
CC               24-NOV-06 : AG: FORMAT AND ORDER CHANGED IN FOURTH COMMENT LINE
CC               27-FEB-07 : AG: CALL DEFCON WITH PARAMETER
CC               27-MAY-07 : AG: GALILEO SYSTEM IMPLEMENTED
CC               01-NOV-07 : HB: ADD PARAMETER SECIPL FOR GTSCLK
CC               26-FEB-08 : RD: USE GTSATM/GTSATB FROM D_SATCRX
CC               08-SEP-08 : DT: ADD ORBIT OUTPUT FOR ILRS
CC               14-NOV-08 : DT: ADD TIMSTD TO ORBINF
CC               03-DEC-08 : DT: ALLOW TIME SCALES UTC,TAI,GAL,GLO
CC               29-MAY-09 : RD: INPUT CLOCKS ALSO FROM INPUT CLK RNX FILE
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               03-DEC-10 : RD: CMC FOR ATL ADDED
CC               10-MAY-11 : HB: TIME ARGUMENT FOR V50-STD IS DIFFERENT, GET
CC                               MODEL INFORMATION THROUGH D_MODEL
CC               01-DEC-11 : SL: NEW TITLE STRING FOR PRITIT, M_BERN WITH ONLY
CC               05-MAR-12 : RD: USE LISTI4 AS MODULE NOW
CC               28-MAR-12 : RD: USE LISTI4 AS MODULE NOW
CC               04-MAY-12 : RD: USE DMOD FROM MODULE
CC               10-OCT-12 : SL/SS: GET DATUM STRING FROM CRD FILE (OPTIONAL)
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: r8b, fileNameLength, lfnOrb, lfnOr1, lfnErr,
     1                    lfnloc
      USE m_cpu,    ONLY: cpu_start
      USE m_maxdim, ONLY: maxsat, maxbad, maxocn
      USE m_epoch,  ONLY: t_epoch, OPERATOR(.epochToReal.)
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE d_const,  ONLY: omega, pi
      USE d_satcrx, ONLY: gtsatm, gtsatb
      USE d_model,  ONLY: getModKey, chrValLength, mod_orb_prcMod
      USE l_basfun, ONLY: dmod
      USE s_cmc,    ONLY: getcmc
      USE s_nuteff
      USE s_iordup
      USE s_prflna
      USE s_poldef
      USE s_mjdgps
      USE s_bpinpt
      USE s_pritit
      USE s_sidmat
      USE s_getorb
      USE s_readinpf
      USE s_timst2
      USE s_wtpreh
      USE s_wtprei
      USE s_orbinf
      USE s_defcon
      USE s_exitrc
      USE s_opnsys
      USE s_jmt
      USE s_radgms
      USE s_rmsele
      USE s_gtflna
      USE s_prceff
      USE s_gtsclk
      USE s_dmlmav
      USE f_modsvn
      USE f_gstime
      USE s_gtsensor
      USE s_opnfil
      USE s_opnerr
      USE s_ckoptb
      USE f_listi4
      USE s_svn2chr
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I      , IARC   , IARC1  , IBAD   , ICRARC , IDAY   ,
     1          IEPO   , IFRMAT , IHOUR  , IMAN   , IMIN   ,
     2          IMONTH , IORSYS , IPOS   , IRC    , IRCCLK , IRCCRX ,
     3          IRCODE , IRCORB , IREMOVE, ISAT   , ISAT0  , ISTOP  ,
     4          ISVN   , ISYS   , IUPD   , IYEAR  , MAXARC , MAXMAN ,
     5          MXCARC , MXCSAT , NARC   , NBAD   , NEPO   , NSVN1  ,
     6          NMAN   , NOPRED , NRSVN  , NSVN   , NWEEK  , MAXSP3 ,
     7          IARC2  , IUPDA  , ISAT1  , IRCCRD
C
      REAL*8    BASCLK , BASPOS , CSZ    , DAY    , DOPRED , DTTAB  ,
     1          DU     , EQEQUI , FITCLK , FITORB , FRAC   , FRST   ,
     2          GPSUTC , SEC    , SECOND , SECIPL , SSZ    , SZ     ,
     3          DUMAX  , TDT    , TEND   , TEPO   , TFIRST , TFIT   ,
     4          TLAST  , TOSC   , TPOL   , TPRED  , TUT1   , UT1UTC ,
     5          XLOG2  , XPOLE  , YPOLE  , numVal
C
      CHARACTER(LEN=chrValLength) :: prcMod
      CHARACTER(LEN=8)            :: srnGet
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C MAXIMAL DIMENSIONS
C ------------------
      PARAMETER (MAXARC=20,MAXMAN=100,MAXSP3=85)
C
C MAXSAT: MAXIMUM NUMBER OF SATELLITES
C MAXARC: MAXIMUM NUMBER OF ARCS
C MAXBAD: MAXIMUM NUMBER OF BAD SATELLITES IN SATCRUX FILE
C
C DECLARATIONS
C ------------
      REAL*8    XV(6),X(3),V(3),X1(3),V1(3),TT1(1),ELE(7)
      REAL*8    PRE(3,3),NUT(3,3),SID(3,3),SID1(3,3),BIAS(3,3)
      REAL*8    TBOUND(2,MAXARC),DTSATC(MAXSAT),DTSATD(MAXSAT)
      REAL*8    POS(3,MAXSAT),VEL(3,MAXSAT)
      REAL*8    TIMBAD(2,MAXBAD)
      REAL*8    SDEVP(4,MAXSAT),SDEVV(4,MAXSAT)
      REAL*8    CORRP(6,MAXSAT),CORRV(6,MAXSAT)
      REAL*8    TIMMAN(MAXMAN),cmc(3)
C
      INTEGER*4 ARCINT(MAXARC)
      INTEGER*4 NUMSAT(MAXARC),NAVNUM(MAXSAT*MAXARC),SVNNUM(MAXSP3)
      INTEGER*4 SVNNM1(MAXSP3)
      INTEGER*4 ACCURA(MAXSP3),SATNR(MAXSP3),INDEX(MAXSP3)
      INTEGER*4 ACCHLP(MAXSAT),ACCHP2(MAXSAT),ISADEL(MAXSAT)
      INTEGER*4 IOBBAD(MAXBAD),IACBAD(MAXBAD),SATBAD(MAXBAD)
      INTEGER*4 ACCPOS(4,MAXSAT),ACCVEL(4,MAXSAT),IWRTE(2)
      INTEGER*4 SATMAN(MAXMAN)
C
      CHARACTER*57 TITLE(4)
      CHARACTER*(fileNameLength) FILNAM,FILCLK,FILCRX,FILCRD
      CHARACTER*19 EPOSTR
      CHARACTER*10 pcvmod,atlmod,ocnmod
      CHARACTER*6  MXNARC,MXNSAT
      CHARACTER*5  COOSYS,DATDES
      CHARACTER*4  AGENCY
      CHARACTER*3  ORBTYP,TIMSYS,timSTD,clkinf,inforb
      CHARACTER*1  VORZ,SOURCE(10,MAXARC)
      CHARACTER*1  EVTFLG(4,MAXSAT),SVNCHR,cmcchr,cmccha
C
      TYPE(t_epoch) :: TTUT1,TTDT,tmjd
C
      CHARACTER(LEN=16),DIMENSION(2)       :: cmcmod  ! name CMC model
      LOGICAL,DIMENSION(2)                 :: cmcyn
      CHARACTER(LEN=16)                    :: datum
      CHARACTER(LEN=1)                     :: cdummy
      LOGICAL                              :: titopt
C
C ILRS orbit output
      integer*4                  lfnor2, iostat, k
      integer*4                  orbILRS
      character*(fileNameLength) filnam2
      real(r8b)                  mjd_epoch
C
C COMMON BLOCKS
C -------------
      COMMON/MCMARC/MXCARC,MXNARC
      COMMON/MCMSAT/MXCSAT,MXNSAT
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C
C INITIALIZE COMMON BLOCKS FOR MAXIMAL DIMENSIONS
C -----------------------------------------------
      MXCARC=MAXARC
      MXNARC='MAXARC'
      MXCSAT=MAXSAT
      MXNSAT='MAXSAT'
C
C GET THE NAME OF THE INPUT FILE
C ------------------------------
      CALL init_inpkey(inpKey)
      CALL readinpf(' ',inpKey)
C
C DEFINE SYSTEM FILES
C -------------------
      CALL OPNSYS
C
C DEFINE CONSTANTS
C ----------------
      CALL DEFCON(1)
C
C PRINT GENERAL TITLE
C -------------------
      CALL pritit('STDPRE','Convert standard to precise orbits')
C
C PRINT GENERAL FILE NAMES
C ------------------------
      CALL prflna
C
C READ ALL INPUT FROM INPUT FILE
C ------------------------------
      CALL bpinpt(title,datdes,tfirst,tlast,dttab,ifrmat,iwrte,isys,
     1            coosys,timsys,orbtyp,agency,fitorb,fitclk,dopred,
     2            baspos,basclk,titopt,clkinf)
C
C GET GEODETIC DATUM FROM COORDINATE FILE
C ---------------------------------------
      CALL GTFLNA(0,'CSYSTEM_CRD',FILCRD,IRCCRD)
      IF (IRCCRD.EQ.0) THEN
        CALL OPNFIL(LFNLOC,FILCRD,'OLD','FORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILCRD,'STDPRE')
C
        READ(LFNLOC,1) DATUM,CDUMMY
1       FORMAT(//,22X,A16,12X,A1///)
        CLOSE(UNIT=LFNLOC)
C
        COOSYS = DATUM(1:5)
        IF (DATUM(6:6) .NE. ' ') WRITE(LFNERR,"(/,A,A,/)")
     1    ' ### PG STDPRE: TRUNCATED DATUM: ',DATUM
      ENDIF
C
C READ ARC INFORMATION
C --------------------
      CALL ORBINF(0,TT1,NARC,ARCINT,NUMSAT,SOURCE,TBOUND,
     1            NAVNUM,IORSYS,cmcyn,timSTD)
C
C READ SATCRUX FILE IF REQUIRED
C -----------------------------
      CALL GTFLNA(0,'SATCRUX',FILCRX,IRCCRX)
      IF (IRCCRX.EQ.0) THEN
        CALL GTSATB(MAXBAD,FILCRX,NBAD,SATBAD,IOBBAD,IACBAD,TIMBAD)
        CALL GTSATM(MAXMAN,NMAN,SATMAN,TIMMAN)
      ENDIF
C
C SET TFIRST AND TLAST IF NOT DEFINED BY THE USER
C -----------------------------------------------
      IF (TFIRST.EQ.0D0) THEN
        DO IARC=1,NARC
          IF (TFIRST.EQ.0D0.OR.TFIRST.GT.TBOUND(1,IARC))
     1      TFIRST=TBOUND(1,IARC)
        ENDDO
      ENDIF
C
      IF (TLAST.EQ.1D20) THEN
        DO IARC=1,NARC
          IF (TLAST.EQ.1D20 .OR. TLAST.LT.TBOUND(2,IARC))
     1       TLAST =TBOUND(2,IARC)
        ENDDO
      ENDIF
C
C END OF FIT INTERVAL
      TFIT  =  TFIRST+FITORB/24D0
      TEND  =  DMIN1(TLAST,TFIT)
      TPRED =  TFIT-DOPRED/24D0
C
C FIND MATCHING ARCS
C ------------------
      IARC1=0
      IARC2=0
      DO IARC=1,NARC
        IF(TFIRST.GE.TBOUND(1,IARC).AND.TFIRST.LE.TBOUND(2,IARC))
     1                                                     IARC1=IARC
        IF(TLAST .GE.TBOUND(1,IARC).AND.TLAST .LE.TBOUND(2,IARC))
     1                                                     IARC2=IARC
      ENDDO
C
C NO ARC FOUND FOR THE ENTIRE INTERVAL (TFIRST,TLAST)
C ---------------------------------------------------
      IF (IARC1.EQ.0.OR.IARC2.EQ.0) THEN
        WRITE(LFNERR,11) TFIRST,TLAST
11      FORMAT(/,' NO ARCS FOUND CONTAINING THE ENTIRE INTERVAL',/,
     1       '    FROM T1 =',F12.5,/,
     2       '    TO   T2 =',F12.5,//,
     3       ' ARC INTERVALS IN STANDARD ORBIT FILE:',/,1X ,36('-'),
     4    //,' ARC           TSTART              TEND',/)
        DO IARC=1,NARC
          WRITE(LFNERR,12) IARC,TBOUND(1,IARC),TBOUND(2,IARC)
12        FORMAT(1X,I2,2F20.5)
        ENDDO
        WRITE(LFNERR,13)
13      FORMAT(/)
        CALL EXITRC(2)
      ENDIF
C
C COMPUTE STARTING TIME AND NUMBER OF EPOCHS
C ------------------------------------------
      CALL JMT(TFIRST,IYEAR,IMONTH,DAY)
      CALL MJDGPS(TFIRST,SECOND,NWEEK)
      IDAY=IDINT(DAY)
      FRAC=DMOD(DAY,1.D0)
      CALL RADGMS(3,FRAC,VORZ,IHOUR,IMIN,SEC)
      SEC=DNINT(SEC)
      TMJD%DAY =IDINT(TFIRST)
      TMJD%FRAC=DMOD(TFIRST,1.D0)
C
      NEPO=IDINT((TLAST-TFIRST)/DTTAB*86400.D0)+1
C
C COMPILE ARRAY CONTAINING ALL SATELLITE NUMBERS FOR ARCS IARC1 TO IARC2
C ----------------------------------------------------------------------
      NSVN1=0
      ISAT0=0
      DO IARC=1,IARC1-1
        ISAT0=ISAT0+NUMSAT(IARC)
      ENDDO
      ISAT1=ISAT0
      DO IARC=IARC1,IARC2
        DO ISVN=1,NUMSAT(IARC)
          NRSVN=NAVNUM(ISAT1+ISVN)
          IPOS=LISTI4(1,MAXSP3,SVNNM1,NRSVN,NSVN1)
        ENDDO
        ISAT1=ISAT1+NUMSAT(IARC)
      ENDDO
C
      DO ISAT=1,NSVN1
        ISADEL(ISAT)=0
      ENDDO
C
C READ THE ACCURACY OF SEMI-MAJOR AXIS, TAKE MAXIMUM VALUE FOR ALL ARCS
C ---------------------------------------------------------------------
      IF (IFRMAT .GT. 1) THEN
        XLOG2 = LOG(2.D0)
        DO ISAT=1,NSVN1
          DUMAX=0D0
          IUPDA=-1
          ISAT1=ISAT0
          DO IARC=IARC1,IARC2
            IPOS=LISTI4(0,MAXSAT,NAVNUM(ISAT1+1),
     1                    SVNNM1(ISAT),NUMSAT(IARC))
            IF (IPOS.NE.0) THEN
              CALL RMSELE(IARC,SVNNM1(ISAT),IUPD,DU)
              IF (IUPD.EQ.1.AND.DU.GT.DUMAX) DUMAX=DU
              IF (IUPD.EQ.0) IUPDA=0
            ENDIF
            ISAT1=ISAT1+NUMSAT(IARC)
          ENDDO
          IF (DUMAX.GT.0) THEN
            ACCHLP(ISAT)=NINT(LOG(2.65D10*PI/180*DUMAX)/XLOG2)
            IF (ACCHLP(ISAT).GT.99) ACCHLP(ISAT)=99
          ELSE
            ACCHLP(ISAT)=0
          ENDIF
C
C FLAG SATELLITES THAT HAVE NOT BEEN IMPROVED
          IF (IUPDA.EQ.0) THEN
            ISADEL(ISAT)=1
          ENDIF
        ENDDO
      END IF
C
C EXCLUDE SATELLITES ACCORDING TO SATCRUX FILE
C --------------------------------------------
      IF (IRCCRX.EQ.0) THEN
        DO ISAT=1,NSVN1
          DO IBAD=1,NBAD
            IF (SVNNM1(ISAT).EQ.SATBAD(IBAD)) THEN
C ..EXCLUSION INTERVAL COVERING WITH REQUESTED SP3-INTERVAL
              IF (IOBBAD(IBAD).EQ.3        .AND.
     1            IACBAD(IBAD).EQ.2        .AND.
     2            TFIRST.GE.TIMBAD(1,IBAD) .AND.
     3            TEND  .LE.TIMBAD(2,IBAD))      THEN
                ISADEL(ISAT)=1
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDIF
C
C REMOVE SATELLITES, ONLY ONE SAT. NUMBER AND ACCURACY FOR MANOEUVRE
C ------------------------------------------------------------------
      NSVN=0
      DO ISAT=1,NSVN1
        IF (ISADEL(ISAT).EQ.1) THEN
          WRITE(LFNERR,901) SVNNM1(ISAT)
901       FORMAT(/,' ### PG STDPRE: SATELLITE ',I3,' NOT INCLUDED',
     1             ' IN PRECISE ORBIT FILE',/)
          CYCLE
        ENDIF
        NRSVN=MODSVN(SVNNM1(ISAT))
C
C ISYS: 0=ALL, 1=GPS, 2=GLONASS, 3=GALILEO
C ----------------------------------------
        CALL SVN2CHR(NRSVN,I,SVNCHR)
        IF (ISYS.EQ.1 .AND. SVNCHR.NE.'G') THEN
          CYCLE
        ELSEIF (ISYS.EQ.2 .AND. SVNCHR.NE.'R') THEN
          CYCLE
        ELSEIF (ISYS.EQ.3 .AND. SVNCHR.NE.'E') THEN
          CYCLE
        ENDIF
C
C COMPILE LIST WITH SATELLITES TO BE WRITTEN TO SP3 FILE
C ------------------------------------------------------
        IPOS=LISTI4(1,MAXSP3,SVNNUM,NRSVN,NSVN)
        ACCHP2(IPOS)=ACCHLP(ISAT)
      ENDDO
C
C CHECK AVAILABILITY OF SATELLITES FOR FIRST AND LAST EPOCH
C ---------------------------------------------------------
cc      ISTOP=2
cc      ISVN =NSVN
cc      DO ISAT=1,ISVN
cc        TEPO=TFIRST
cc        CALL GETORB(SVNNUM(ISAT),0,1,ISTOP,TEPO,ICRARC,
cc     1              IORSYS,XV,TOSC,ELE,IRCF)
cc        TEPO=TFIRST+(NEPO-1)*DTTAB/86400.D0
cc        CALL GETORB(SVNNUM(ISAT),0,1,ISTOP,TEPO,ICRARC,
cc     1              IORSYS,XV,TOSC,ELE,IRCL)
cc        IF (IRCF.EQ.2 .OR. IRCL.EQ.2) THEN
cc          SVNNUM(ISAT)=SVNNUM(NSVN)
cc          NSVN=NSVN-1
cc        ENDIF
cc      ENDDO
C
C PUT ALL SATELLITES IN ASCENDING ORDER
C -------------------------------------
      CALL IORDUP(SVNNUM,NSVN,INDEX)
      DO ISAT=1,NSVN
        SATNR(ISAT)=SVNNUM(INDEX(ISAT))
        ACCURA(ISAT)=ACCHP2(INDEX(ISAT))
      ENDDO
C
      DO ISAT=NSVN+1,MAXSP3
        SATNR(ISAT)=0
        ACCURA(ISAT)=0
      ENDDO
C
      DO ISAT=1,NSVN
        ISADEL(ISAT)=0
      ENDDO
C
C GET SATELLITE CLOCK FILE NAME
C -----------------------------
      CALL GTFLNA(0,'SATCLK ',FILCLK,IRCCLK)
C
C GET FILENAME OF PRECISE ORBIT FILE
C ----------------------------------
      CALL GTFLNA(1,'PREOUT ',FILNAM,IRC)
C
C Hardwired switch for orbit info
C -------------------------------
      inforb = 'CoN'
C      inforb = 'CoM'
C
C System of orbit? CoM or CoN
C ---------------------------
      If (.NOT. cmcyn(1)) inforb = 'CoN'
C
C FILL INFORMATION INTO FOURTH COMMENT LINE
C -----------------------------------------
      IF (titopt) THEN
        pcvmod=' '
        CALL GTSENSOR(pcvmod=pcvmod)
        CALL getcmc(cmcyn,cmcmod=cmcmod)
        cmcchr='N'
        ocnmod='NONE    '
        IF (cmcyn(1)) THEN
          cmcchr='Y'
          ocnmod=cmcmod(1)(1:8)
        ENDIF
        cmccha='N'
        atlmod='NONE    '
        IF (cmcyn(2)) THEN
          cmccha='Y'
          atlmod=cmcmod(2)(1:8)
        ENDIF
        WRITE(TITLE(4),"('PCV:',A10,' OL/AL:',A8,1X,A8,1X,2A1,
     1                        ' ORB:',A3,' CLK:',A3)")
     2                 pcvmod(1:10),ocnmod,atlmod,cmcchr,cmccha,
     3                       inforb,clkinf
      ENDIF
C
C WRITE HEADER OF PRECISE ORBIT
C -----------------------------
      CALL WTPREH(FILNAM,LFNOR1,IFRMAT,NSVN,SATNR,ACCURA,TFIRST,
     1            NEPO,DTTAB,TITLE,DATDES,COOSYS,ORBTYP,AGENCY,
     2            TIMSYS,BASPOS,BASCLK)
C
C Orbit output for ILRS
C ---------------------
      CALL ckoptb(1,(/'BENCH'/),'STDPRE','ILRS orbit',irCode,
     1            result1=orbILRS)
C
      IF (orbILRS.EQ.1) THEN

        CALL gtflna(1,'ORBILRS',filnam2,irc)
C
        CALL opnfil(lfnor2,filnam2,'UNKNOWN','FORMATTED',' ',' ',iostat)
        CALL opnerr(lfnerr,lfnor2,iostat,filnam2,'STDPRE')

      END IF
C
C LOOP OVER ALL EPOCHS
C --------------------
      TTUT1%day = INT(TFIRST)
      TTDT%day = INT(TFIRST)
      TMJD%DAY =INT(TFIRST)
      FRST=TFIRST-TMJD%DAY
      CALL POLDEF(TFIRST,0,XPOLE,YPOLE,UT1UTC,GPSUTC)
      DO IEPO=1,NEPO
        TMJD%FRAC=FRST+(IEPO-1)*DTTAB/86400.D0
        TEPO=.epochToReal.TMJD
C
C POLE-INFORMATION (TIME ARGUMENT: UTC; V50: GPS)
C -----------------------------------------------
        CALL getModKey(mod_orb_prcMod,prcMod,srnGet,numVal)
        IF (prcMod(1:3) == 'V50') THEN
          tPol = tEpo
        ELSE
          TPOL = TEPO - GPSUTC
        ENDIF
        CALL POLDEF(TPOL,1,XPOLE,YPOLE,UT1UTC,GPSUTC)
CC
C PRECESSION AND NUTATION (TIME ARGUMENT: TDB, APPROX. AS TDT)
C ------------------------------------------------------------
        TDT=TEPO+(19.D0+32.184D0)/86400.D0
        CALL PRCEFF(IORSYS,5.D0,TDT,PRE)
        CALL NUTEFF(IORSYS,0.1D0,TDT,NUT,EQEQUI,BIAS)
        pre = matmul(pre,bias)
C
C TRUE SIDERIAL TIME (TIME ARGUMENT: UT1)
C ---------------------------------------
        TUT1=TEPO-GPSUTC+UT1UTC
        TTUT1%frac=TMJD%frac-GPSUTC+UT1UTC
        TTDT%frac =TMJD%frac +(19.D0+32.184D0)/86400.D0
        SZ=GSTIME(0,TTUT1,TTDT,NUT(2,1),EQEQUI)
C
C PRODUCT OF TRANSPOSE OF POLAR MOTION MATRIX AND THE SIDERAL TIME
C MATRIX
C ----------------------------------------------------------------
        CALL sidmat(tdt,xpole,ypole,sz,sid)
C
C PRODUCT OF TRANSPOSE OF POLAR MOTION MATRIX AND THE TIME DERIVATIVE
C OF THE SIDERAL TIME MATRIX
C -------------------------------------------------------------------
        SSZ=DSIN(SZ)
        CSZ=DCOS(SZ)
        SID1(1,1)=-SSZ
        SID1(1,2)= CSZ
        SID1(1,3)= 0.D0
        SID1(2,1)=-CSZ
        SID1(2,2)=-SSZ
        SID1(2,3)= 0.D0
        SID1(3,1)= SSZ*XPOLE-CSZ*YPOLE
        SID1(3,2)=-CSZ*XPOLE-SSZ*YPOLE
        SID1(3,3)= 0.D0
C
C CMC correction if desired
C -------------------------
        cmc = 0D0
        IF (inforb == 'CoN') CALL getcmc(cmcyn,TDT,cmc)
C
C LOOP OVER ALL SATELLITES OF THE ARC "IARC"
C ------------------------------------------
        DO ISAT=1,NSVN
C
C EXCLUDE SATELLITES ACCORDING TO SATCRUX FILE
C --------------------------------------------
          IREMOVE=0
          NOPRED=0
          IF (IRCCRX.EQ.0) THEN
            DO IBAD=1,NBAD
              IF (MODSVN(SATNR(ISAT)).EQ.SATBAD(IBAD)) THEN
                IF (IOBBAD(IBAD).EQ.3        .AND.
     1              IACBAD(IBAD).EQ.2) THEN
C ..SATELLITE EXCLUDED FOR TEPO
                  IF (TEPO.GT.TIMBAD(1,IBAD)  .AND.
     1                TEPO.LE.TIMBAD(2,IBAD)) THEN
                    IREMOVE=1
                  ENDIF
C ..DO NOT PREDICT SATELLITE (BAD TIME INTERVAL)
                  IF (TIMBAD(2,IBAD).GT.TPRED .AND.
     1                TIMBAD(2,IBAD).LE.TEND) THEN
                    NOPRED=1
                  ENDIF
                ENDIF
              ENDIF
            ENDDO
C
C ..DO NOT PREDICT SATELLITE (MANOEUVER)
            DO IMAN=1,NMAN
              IF (MODSVN(SATNR(ISAT)).EQ.SATMAN(IMAN)) THEN
                IF (TIMMAN(IMAN).GT.TPRED .AND.
     1              TIMMAN(IMAN).LE.TEND) THEN
                  NOPRED=1
                ENDIF
              ENDIF
            ENDDO
C
C DO NOT INCLUDE REMOVED SATELLITE AGAIN FOR PREDICTED INTERVAL
            IF (ISADEL(ISAT)==1.AND.IREMOVE==0.AND.TEPO>TFIT) THEN
              IREMOVE=1
            ELSEIF (TEPO>TFIT.AND.NOPRED==1) THEN
              IREMOVE=1
              IF (ISADEL(ISAT)==0) THEN
                CALL TIMST2(1,1,TEPO,EPOSTR)
                WRITE(LFNERR,"(/,' ### PG STDPRE: SATELLITE ',I3,
     1                           ' NOT INCLUDED IN PREDICTION INTERVAL',
     2                      /,16X,'START EPOCH: ',A,/)")
     3                        SATNR(ISAT),EPOSTR
              ENDIF
              ISADEL(ISAT)=IREMOVE
            ELSE
              IF (ISADEL(ISAT)==0.AND.IREMOVE==1) THEN
                CALL TIMST2(1,1,TEPO,EPOSTR)
                WRITE(LFNERR,"(/,' ### PG STDPRE: SATELLITE ',I3,
     1                           ' NOT INCLUDED IN PRECISE ORBIT FILE',
     2                      /,16X,'START EPOCH: ',A,/)")
     3                        SATNR(ISAT),EPOSTR
              ENDIF
              ISADEL(ISAT)=IREMOVE
            ENDIF
          ENDIF
          IF (IREMOVE==1) THEN
            POS(1:3,ISAT)=0D0
            VEL(1:3,ISAT)=0D0
            DTSATC(ISAT)=999999.999999D0
            DTSATD(ISAT)=999999.999999D0
          ELSE
C
C COMPUTE SATELLITE POSITIONS AND VELOCITYS FOR EPOCH "TEPO"
C --------------------------------------------------------
            ISTOP=2
            CALL GETORB(SATNR(ISAT),0,1,ISTOP,TEPO,ICRARC,
     1                  IORSYS,XV,TOSC,ELE,IRCORB)
C
C COMPUTE SATELLITE CLOCK CORRECTION FOR ONE EPOCH AND ONE SATELLITE
C FROM THE SATELLITE CLOCK FILE
C ------------------------------------------------------------------
            IF (IRCCLK.EQ.0) THEN
              SECIPL=0.D0
              CALL GTSCLK(-1,TEPO,SATNR(ISAT),SECIPL,2,DTSATC(ISAT),IRC)
              IF (IRC.GT.0) DTSATC(ISAT)=999999.999999D0
            ELSE
              DTSATC(ISAT)=999999.999999D0
            ENDIF
            DTSATD(ISAT)=999999.999999D0
C
C TRANSFORMATION FROM SYSTEM 1950.0 TO WGS-84
C OR FROM 2000.0 TO IERS-85
C -------------------------------------------
            IF (IRCORB.EQ.0) THEN
              DO I=1,3
                X1(I)=XV(I)/1.0D0
                V(I) =XV(3+I)/1.0D0
              ENDDO
              CALL DMLMAV(X1,PRE,X1)
              CALL DMLMAV(X1,NUT,X1)
              CALL DMLMAV(X1,SID,X)
              CALL DMLMAV(X1,SID1,V1)
              CALL DMLMAV(V,PRE,V)
              CALL DMLMAV(V,NUT,V)
              CALL DMLMAV(V,SID,V)
C
              DO I=1,3
                POS(I,ISAT)=X(I)-cmc(I)
                VEL(I,ISAT)=V(I)+V1(I)*OMEGA
              ENDDO
            ELSE
              POS(1:3,ISAT)=0D0
              VEL(1:3,ISAT)=0D0
            ENDIF
          ENDIF
C
C ACCURACY CODES
C --------------
          ACCPOS(1:4,ISAT)=0
          ACCVEL(1:4,ISAT)=0
          SDEVP(1:4,ISAT) =0D0
          SDEVV(1:4,ISAT) =0D0
          CORRP(1:6,ISAT) =0D0
          CORRV(1:6,ISAT) =0D0
C
C EVENT FLAGS
C -----------
          EVTFLG(1:4,ISAT)=' '
          IF (TEPO .GT. TFIRST+FITORB/24D0) EVTFLG(4,ISAT)='P'
          IF (TEPO .GT. TFIRST+FITCLK/24D0) EVTFLG(2,ISAT)='P'
C
C MANOEUVER
C ---------
          DO IMAN=1,NMAN
            IF (SATNR(ISAT).EQ.SATMAN(IMAN).AND.
     1          TIMMAN(IMAN).GE.TFIRST .AND.
     2          TIMMAN(IMAN).LE.TEPO) THEN
              IF (TIMMAN(IMAN).GT.TEPO-DTTAB/86400D0) THEN
                EVTFLG(3,ISAT)='M'
              ENDIF
C DO NOT WRITE PREDICTIONS AFTER MANEUVER
              IF (TIMMAN(IMAN).GT.TPRED.AND.
     1            TEPO.GT.TFIT) THEN
                POS(1:3,ISAT)=0D0
                VEL(1:3,ISAT)=0D0
                DTSATC(ISAT)=999999.999999D0
                DTSATD(ISAT)=999999.999999D0
              ENDIF
            ENDIF
          ENDDO
C
C NEXT SATELLITE
C --------------
        ENDDO
C
C Correct from GPS-time to other time scale if necesary
C -----------------------------------------------------
        IF ( timSTD /= 'GPS' .AND. timSTD /= 'GAL' ) THEN
          WRITE(LFNERR,"(/,' ### PG STDPRE: Your standard orbit is',
     1                     ' not in GPS time scale!',
     2               /,16X,'Time scale: ',A,/)")
     3                     timSTD
        END IF

        IF ( timsys=='UTC' .OR. timsys=='GLO' ) THEN
          tmjd%frac = tmjd%frac - GPSUTC

        ELSEIF ( timsys=='TAI' ) THEN
          tmjd%frac = tmjd%frac + 19.0/86400.0

        END IF
C
C WRITE EPOCH OF PRECISE ORBIT
C ----------------------------
        CALL WTPREI(LFNOR1,IFRMAT,IWRTE,NSVN,SATNR,TMJD,POS,VEL,
     1              DTSATC,DTSATD,ACCPOS,ACCVEL,EVTFLG,SDEVP,SDEVV,
     2              CORRP,CORRV,IRCODE)
C
C ILRS orbit output (for first satellite!)
C ----------------------------------------
        IF (orbILRS.EQ.1) THEN
          mjd_epoch = tmjd%day + tmjd%frac
          write(lfnor2,1010) mjd_epoch,
     1                       (POS(k,1)*1d3, k=1,3),
     2                       (VEL(k,1)*1d-1,k=1,3)
1010      FORMAT(2X,F14.8,1X,3(1X,F17.6),3(1X,F17.10))

        END IF
C
C NEXT EPOCH
C ----------
      ENDDO
C
C WRITE END OF FILE "EOF"
C -----------------------
      WRITE(LFNOR1,1009)
1009  FORMAT('EOF')
C
C CLOSE INPUT AND OUTPUT FILE
C ---------------------------
      CLOSE(UNIT=LFNORB)
      CLOSE(UNIT=LFNOR1)
C
      IF (orbILRS.EQ.1)  CLOSE(UNIT=lfnor2)
C
      CALL EXITRC(0)
      END
