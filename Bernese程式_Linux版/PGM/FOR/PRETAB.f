C*
      PROGRAM PRETAB
CC
CC NAME       :  PRETAB
CC
CC PURPOSE    :  GENERATE A TABULAR FILE FROM PRECISE EPHEMERIDES
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER, L.MERVART
CC
CC CREATED    :  86/08/22 10:34
CC
CC CHANGES    :  11-FEB-92 : ??: WRITE EPOCH NUMBER WITH 5 DIGITS
CC               29-APR-92 : ??: NEW NGS FORMAT
CC               05-JUN-92 : ??: OPNFIL IMPLEMENTED; OPTION J2000.0;
CC                               NEW INPUT SR "PTINPT";
CC                               NEW CALLS: "PRCEFF","NUTEFF"
CC               12-JUN-92 : ??: READ MANOEUVRES AND CHANGE SATELLITE
CC                               NUMBER FROM SVN TO SVN+50 AFTER SHIFT
CC               20-JUN-92 : ??: POSSIBILITY TO READ THE NEW FORMAT
CC                               CHANGED ACCORDING TO CLYDE GOAD
CC               02-JUN-93 : ??: COMPUTE COEFFICIENTS FOR SATELLITE CLOCK
CC                               CORRECTIONS IF THEY ARE AVAILABLE IN
CC                               PRECISE EPHEMERIDES FILE;
CC                               NEW INPUTS IN SR "PTINPT"
CC               22-JUL-93 : ??: CORRECT TEST IF SAT.CLOCKS AVAILABLE
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               15-MAR-94 : RW: PRINT GENERAL FILE NAMES: PRFLNA
CC               10-AUG-94 : MR: CALL EXITRC
CC               17-AUG-94 : MR: CLEAN PROGRAM. CORRECT INDENTION ...
CC               06-SEP-94 : MR: WARNING IF NO SATELLITE CLOCKS FOUND
CC               11-OCT-94 : MR: WARNING ONLY, IF CLOCK FILE GIVEN
CC               10-JUL-95 : TS: HANDLE REQUEST SMIN=0 AND DELTAT=0 TO GET
CC                               THE REAL CLOCK VALUES FROM THE SP3 FILE
CC                               WITHOUT POLYNOMIAL FIT
CC               06-DEC-95 : MR: VARIABLE "USERHR" CHANGED TO "IUSRHR",
CC                               COMMA ADDED IN FORMAT 380,
CC                               MAXCKR CHANGED FROM 50 TO 100
CC               01-APR-96 : MR: ADD "D0" TO 999999.999999 (3 TIMES)
CC               26-APR-96 : TS: CHECK CLOCK POLYNOMIAL QUALITY WITH
CC                               "OUTLIER REJECTION"
CC               05-JUN-96 : TS: CALL POLDEF CHANGED DUE TO SUBDAILY POLE
CC               29-APR-97 : HH: ADD GLONASS (FORMATS, ALLOW 0.0000 POS.)
CC               27-AUG-98 : MR: USE FUNCTION "MODSVN"
CC               11-JAN-99 : MR: ALLOW MANOEUVRE SATELLITES IN PRE-FILE
CC               22-JUN-00 : TS: CHANGED MAXCLK TO MAXSAC AND USE INCLUDE
CC               02-NOV-00 : CU: SWITCH TO THE NEW MENU SYSTEM
CC               18-DEC-00 : HU: USE INTERFACE FOR PRFLNA
CC               10-JAN-01 : RD/HU: MORE THAN ONE TABFIL MAY BE SPECIFIED
CC               15-FEB-01 : DS: SR TYP2SVN: SV TYPE AND SP3 NUMBER
CC                               CONVERSION
CC               05-NOV-01 : MM: NEW OPTION (EXCLUDE SATELLITES FROM TAB-
CC                               FILE ACCORDING TO THEIR ACCURACY CODES)
CC               15-NOV-01 : MM: ERROR IF ALL SAT. ARE EXCLUDED
CC               22-JUN-02 : DS: WRITE NUMBER OF EPOCH WITH I7
CC               17-JUL-02 : MM: NBAD INITIALIZED
CC               25-SEP-02 : HU: REMOVE I_ASTLIB
CC               26-OCT-02 : HU: REMOVE BAD SATELLITES ON REQUEST
CC               02-NOV-02 : HU: SP3C IMPLEMENTED,
CC                               USE RDPREH, RDPREI, WTSATH, WTSATI
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               15-MAY-03 : HU: MAXCKR INCREASED FROM 2880 TO 8640
CC               11-JUN-03 : HU: USE GSTIME
CC               14-JUN-03 : HU: DESACTIVATE USE OF GMST2000
CC               06-AUG-03 : HU: WRITE NUTNAM AND SUBNAM TO TAB FILE
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               10-MAR-04 : HB: CHANGE ORDER OF MODULES
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: (COM)LFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               07-JUL-05 : HB: USE T_EPOCH FOR GSTIME
CC                               ADD BIAS TO PARAMETER LIST OF SR NUTEFF
CC               01-AUG-05 : HU: EPOCH AS STRUCTURE
CC               09-FEB-06 : HB: GET POLE AGAIN WITH GPS TIME (CODE ERP
CC                               IS IN GPS TIME)
CC               18-JUL-06 : AG: CMC IMPLEMENTED
CC               28-FEB-07 : AG: USE 206264... FROM DEFCON
CC               23-MAR-07 : HB/AG: TUT1 AGAIN INITIALIZED (SR GETCMC)
CC               28-MAR-07 : HB/HU: ADD 'BIAS' AS CHARACTERIZATION OF
CC                                  IERS2003 STANDARDS IN TAB-FILE
CC               28-MAY-07 : AG: ADD AN ENTER AT END OF SATELLITE CLOCK FILE
CC               26-FEB-08 : RD: USE GTSATM/GTSATB FROM D_SATCRX
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               03-DEC-10 : RD: CMC FOR ATL ADDED
CC               05-MAY-11 : HB: CALL RDNUTSUB + SETTING MODEL 'BIAS'
CC                               EARLIER, SET MODEL KEYS
CC               19-MAY-11 : HB: INITIALIZE NUMVAL = 0.D0
CC               30-NOV-11 : SL: NEW TITLE STRING FOR PRITIT, M_BERN WITH ONLY
CC               05-MAR-12 : RD: USE LISTI4 AS MODULE
CC               04-MAY-12 : RD: USE DMOD FROM MODULE
CC               04-MAY-12 : RD: REMOVE UNUSED MODULES AND VARIABLES
CC               22-AUG-12 : RD: INIT FLAG ARRAY "L"
CC               22-AUG-12 : RD: SOLVE ROUNDING PROBLEM WITH IFC FOR CLOCK-POLY.
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnPrt, lfnErr, lfnRes, lfnOrb, lfnOr1
      USE m_cpu,    ONLY: cpu_start
      USE m_maxdim, ONLY: maxsat, maxsac, maxbad, maxocn
      USE m_epoch,  ONLY: t_epoch, OPERATOR(.epochToReal.)
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE d_const,  ONLY: FILTITLE, ars
      USE d_satcrx, ONLY: gtsatm, gtsatb
      USE d_model,  ONLY: setModKey, chrValLength, mod_orb_prcMod
      USE l_basfun, ONLY: dmod
      USE s_cmc,    ONLY: getcmc
      USE s_nuteff
      USE s_sidmat
      USE s_dmlmtv
      USE s_opnfil
      USE s_prflna
      USE s_poldef
      USE s_gtfile
      USE s_pritit
      USE s_readinpf
      USE s_opnerr
      USE s_rdpreh
      USE s_rdprei
      USE s_wtsath
      USE s_wtsati
      USE s_polyap
      USE s_defcon
      USE s_exitrc
      USE s_opnsys
      USE s_jmt
      USE s_radgms
      USE s_rdnutsub
      USE s_ptinpt
      USE s_gtflna
      USE s_prceff
      USE s_clrflg
      USE f_djul
      USE f_listi4
      USE f_gstime
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IBAD  , ICLKSV, IDAY1 , IDAY2 , IDEG  , IEPO  ,
     1          IFIL  , IFRMAT, IHOUR1, IHOUR2, IMAN  , IMIN1 , IMIN2 ,
     2          IMONT1, IMONT2, IORSYS, IOSTAT, IPOS  , IRC   , IRCCLK,
     3          IREADE, IREC  , IREFP , ISAT  , ISATNW, IUSRHR, IYDAY ,
     4          IYEAR1, IYEAR2, JBAD  , MAXCKR, MAXFIL, J     ,
     5          MAXMAN, MFIL  , NBAD1 , NCOEF , NEPO  , NFIL  , NFLCOL,
     6          NMAN  , NSAT  , NSATNW, NSBAD
C
      REAL*8    BASCLK, BASPOS, CLK   , DAY1  , DAY2  , DELT  ,
     1          DTTAB , EPOHR , EQEQUI, GPSUTC, RMS   , SAMPRT,
     2          SEC1  , SEC2  , SMIN  , SZ    , TCLK  , TCLKA , TCLKB ,
     3          TDT   , TEND  , TGPS  , TSTART, TUT1  , UT1C1 ,
     4          UT1C2 , UT1UTC, XHOUR1, XHOUR2, XMIN1 , XMIN2 , XMJD0 ,
     5          XPOLE , XPOLE1, XPOLE2, YPOLE , YPOLE1, YPOLE2, numVal
C
      CHARACTER(LEN=8), PARAMETER :: pgName = 'PRETAB  '

CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C MAXIMAL DIMENSIONS
C ------------------
      PARAMETER (MAXFIL=100, MAXMAN=100, MAXCKR=8640)
C
C
C MAXFIL: MAXIMUM NUMBER OF FILES TO BE PROCESSED IN ONE RUN
C MAXMAN: MAXIMUM NUMBER OF MANOEUVRES
C MAXSAC: MAXIMUM NUMBER SATELLITE CLOCK COEFFICIENTS
C MAXCKR: MAXIMUM NUMBER OF CLOCK RECORDS FOR ONE POLYNOMIAL FIT
C MAXSAT: MAXIMUM NUMBER OF SATELLITES
C
C DECLARATIONS
C ------------
      REAL*8       POS(3)
      REAL*8       SID(3,3),NUT(3,3),PRE(3,3),BIAS(3,3)
      REAL*8       CLKCOR(MAXCKR,MAXSAT),CLKTIM(MAXCKR,MAXSAT)
      REAL*8       COEF(MAXSAC),RES(MAXCKR),CRMS(MAXSAC)
      REAL*8       TIMMAN(MAXMAN),POSMAN(3,MAXMAN),TIMACT(MAXMAN)
      REAL*8       TIMBAD(2,MAXBAD)
      REAL*8       POSPRE(3,MAXSAT),VELPRE(3,MAXSAT)
      REAL*8       DTSATC(MAXSAT),DDTSAT(MAXSAT)
      REAL*8       SDEVP(4,MAXSAT),SDEVV(4,MAXSAT)
      REAL*8       CORRP(6,MAXSAT),CORRV(6,MAXSAT)
      REAL*8       cmc(3)              ! CMC offset
C
      INTEGER*4    SVNNUM(85),SATMAN(MAXMAN),NCLKEP(MAXSAT)
      INTEGER*4    IFLCLK(MAXFIL),PRTERR(MAXSAT)
      INTEGER*4    IOBBAD(MAXBAD),IACBAD(MAXBAD),SATBAD(MAXBAD)
      INTEGER*4    ACCPOS(4,MAXSAT),ACCVEL(4,MAXSAT),IEREC(2)
C
      CHARACTER(LEN=chrValLength) :: chrVal
      CHARACTER*64 TITLE
      CHARACTER*57 TITLES(4)
      CHARACTER*32 FILNAM(2,MAXFIL),FLNCLK,FILCRX
      CHARACTER*7  ORBSYS(2)
      CHARACTER*1  VORZ,L(MAXCKR)
      CHARACTER*32 tabfil(maxfil)
      CHARACTER*16 cmcmod(2)
      CHARACTER*20 harstr
      CHARACTER*1  cmcchr(2)
C
      CHARACTER*5  COOSYS,DATDES
      CHARACTER*4  AGENCY,BSNAM
      CHARACTER*3  ORBTYP,TIMSYS
      CHARACTER*2  FILTYP
      CHARACTER*1  EVTFLG(4,MAXSAT),EVTMAN(2,MAXMAN),FLAG
      CHARACTER*16 NUTNAM,SUBNAM
C
      LOGICAL   USEAC, RMBAD, HELPAC, CMCYN(2)
      INTEGER*4 MINAC, MAXAC
C
      INTEGER*4 ACCODE(85), BADSAT(MAXSAT), BADAC(MAXSAT), NBAD
C
      TYPE(t_epoch) :: TTUT1,TTDT, TMJD
C
      DATA ORBSYS/'B1950.0','J2000.0'/
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
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
C WRITE TITLE
C -----------
      CALL pritit('PRETAB','Create tabular orbits')
C
C INIT VARIABLES
C --------------
      DO I=1,MAXCKR
        DO J = 0,7
          CALL CLRFLG(L(I),J)
        ENDDO
      ENDDO
C
C READ FILE NAMES
C ---------------
      NFLCOL = 2
      CALL GTFILE('PREFIL ',NFLCOL,MAXFIL,NFIL,FILNAM)
C
      CALL GTFILE('TABFIL',1,MAXFIL,MFIL,TABFIL)
      IF (MFIL .GT. 0) THEN
        IF (NFIL .NE. MFIL) THEN
          WRITE(LFNERR,'(/,2A,2(/,16X,A,I5),/)')
     1          ' *** PG PRETAB: NUMBER OF INPUT AND ',
     1                          'OUTPUT FILES IS DIFFERENT',
     2                          '# OF PRECISE FILES :',NFIL,
     3                          '# OF TABULAR FILES :',MFIL
          CALL EXITRC(2)
        ENDIF
C
        DO iFil = 1, mFil
          filnam(2,iFil) = tabfil(iFil)
        END DO
      END IF
C
C READ INPUT OPTION
C -----------------
      CALL ptinpt(MAXSAC,TITLE,IORSYS,SMIN,IDEG,
     1            RMBAD,HELPAC,MINAC,MAXAC,CMCYN)
      DELT=SMIN/1440.D0
      NCOEF=IDEG+1
C
C WRITE TITLE
C -----------
      WRITE(LFNPRT,"(1X,'System: ',A,/)") ORBSYS(IORSYS)
C
C PRINT GENERAL FILE NAMES
C ------------------------
      CALL prflna
C
      WRITE(LFNPRT,57)
57    FORMAT(//,' FILE  PRECISE ORBIT FILENAME         '
     1          ,         '   TABULAR ORBIT FILENAME '
     2         /,' ',4('-'),'  ',32('-'),'  ',32('-'),/)
C
C GET SATELLITE MANOEUVRES
C ------------------------
      CALL GTSATM(MAXMAN,NMAN,SATMAN,TIMMAN)
C
C GET BAD SATELLITE TIME INTERVALS
C ------------------------
      CALL GTFLNA(0,'SATCRUX',FILCRX,IRC)
      CALL GTSATB(MAXBAD,FILCRX,NSBAD,SATBAD,IOBBAD,IACBAD,TIMBAD)
C
C SEARCH FOR A FILE WITH INTERNAL NAME "SATCLRS" IN INPUT FILE
C "PRETABN.INP", SET ITS EXTERNAL NAME (VARIABLE FLNCLK IF EXISTS)
C WHERE WILL BE WRITTEN POLYNOMIAL SATELLITE CLOCK COEFFICIENTS
C ----------------------------------------------------------------
      CALL GTFLNA(0,'SATCLRS',FLNCLK,IRCCLK)
      ICLKSV=0
C
C LOOP OVER ALL FILES
C -------------------
      DO 1000 IFIL=1,NFIL
C
C INITIALISATION OF NUMBER OF CLOCK VALUES PER SATELLITE
C ------------------------------------------------------
        IFLCLK(IFIL)=0
        DO 11 ISAT=1,MAXSAT
          NCLKEP(ISAT)=0
          PRTERR(ISAT)=0
11      CONTINUE
        DO 12 IREC=1,MAXCKR
          L(IREC)=CHAR(0)
12      CONTINUE
C
C WRITE FILENAMES
C ---------------
        IF (FILNAM(2,IFIL).EQ.' ') THEN
          FILNAM(2,IFIL)= '  ---  '
        ENDIF
        WRITE(LFNPRT,56) IFIL,FILNAM(1,IFIL),FILNAM(2,IFIL)
56      FORMAT(I5,2X,A32,2X,A32/)
C
C NO PROCESS IF OUTPUT FILENAME DOES NOT EXIST
C --------------------------------------------
        IF (FILNAM(2,IFIL).EQ.'  ---  ') GOTO 1000
C
C READ HEADER OF PRECISE FILE
C ---------------------------
        CALL RDPREH(FILNAM(1,IFIL),LFNOR1,IFRMAT,NSAT,SVNNUM,ACCODE,
     1              TSTART,NEPO,DTTAB,TITLES,DATDES,COOSYS,ORBTYP,
     2              AGENCY,FILTYP,TIMSYS,BASPOS,BASCLK)
C
C TIME SYSTEM UTC CURRENTLY NOT SUPPORTED
C ---------------------------------------
        IF (TIMSYS.NE.'   '.AND.TIMSYS.NE.'GPS') THEN
          WRITE(LFNERR,"(/,' *** PG PRETAB: TIME SYSTEM ',A3,
     1                           ' CURRENTLY NOT SUPPORTED',/)") TIMSYS
          CALL EXITRC(2)
        ENDIF
C
C CHECK FORMAT IF ACCURACY CODES SHOULD BE USED
C ---------------------------------------------
        USEAC = HELPAC
        IF (USEAC.AND.IFRMAT.LT.2) THEN
          WRITE(LFNERR,401) IFRMAT+1,FILNAM(1,IFIL)
401       FORMAT(/,' ### PG PRETAB: USE OF ACCURACY CODES NOT ',
     1             'SUPPORTED FOR SP',I1,' FORMAT',/,
     2                         16X,'FILE NAME: ',A,/,
     3                         16X,'ACCURACY CODES ARE NOT USED',/)
          USEAC = .FALSE.
        ENDIF
C
C END OF TIME INTERVAL
C --------------------
        TEND=TSTART+(NEPO-1)*DTTAB/86400.D0
C
C CHECK FOR BAD SATELLITES BASED ON SATCRUX OR ACCURACY CODES
C -----------------------------------------------------------
        NBAD = 0
        DO IBAD=1,NSAT
C ACCURACY CODE IS BAD
          IF (USEAC .AND.
     1        (ACCODE(IBAD).LT.MINAC .OR. ACCODE(IBAD).GT.MAXAC)) THEN
            IPOS=LISTI4(1,MAXSAT,BADSAT,SVNNUM(IBAD),NBAD)
            BADAC(IPOS)  = ACCODE(IBAD)
            CYCLE
          ENDIF
C SATELLITE IS MARKED AS BAD IN SATCRUX
          IF (RMBAD) THEN
            DO JBAD=1,NSBAD
              IF (SATBAD(JBAD).EQ.SVNNUM(IBAD) .AND.
     1            IOBBAD(JBAD).NE.0            .AND.
     2            IACBAD(JBAD).NE.0            .AND.
     3            TIMBAD(1,JBAD).LT.TEND       .AND.
     4            TIMBAD(2,JBAD).GT.TSTART) THEN
                IPOS=LISTI4(1,MAXSAT,BADSAT,SVNNUM(IBAD),NBAD)
                BADAC(IPOS)  = -999
                EXIT
              ENDIF
            ENDDO
          ENDIF
        ENDDO
C
C ERROR IF ALL SATELLITES ARE MARKED AS "BAD"
        IF (NBAD.EQ.NSAT) THEN
          WRITE(LFNERR,366) FILNAM(1,IFIL),MAXAC
366       FORMAT(/,' *** PG PRETAB: ALL SATELLITES ARE EXCLUDED',
     1             ' DUE TO ACCURACY CODES OR SATCRUX.',/,
     2             16X,'FILE NAME            : ',A,/,
     3             16X,'MAXIMUM ACCURACY CODE: ',I4,/)
          CALL exitrc(2)
        ENDIF
C
C GENERATION OF T-FILES
C ---------------------
! Get/Set model names for system transformation
! ---------------------------------------------
        CALL RDNUTSUB(NUTNAM,SUBNAM)
! -----------------------------------------------
! IERS2003/IERS2010 system transformation applied
! -----------------------------------------------
        IF (iFil == 1) THEN
          BSNAM='BIAS'
          chrVal= ' '
          chrVal(1:4)=bsNam
          numVal=0.D0
          CALL setModKey(mod_orb_prcMod,chrVal,pgName,numVal)
        ENDIF
! --------------------------------------
C
C 1. READ RELEVANT POLE-INFORMATION
C ---------------------------------
        CALL POLDEF(TSTART,1,XPOLE1,YPOLE1,UT1C1,GPSUTC)
        CALL POLDEF(TEND  ,1,XPOLE2,YPOLE2,UT1C2,GPSUTC)
C
C 2. SET POLAR WOBBLE MATRIX
C --------------------------
!        DO 220 I=1,3
!          DO 210 K=1,3
!            POLE(I,K)=0.D0
!            IF(I.EQ.K)POLE(I,K)=1.D0
!210       CONTINUE
!220     CONTINUE
C
C OPEN OUTPUT FILE (TABULAR ORBITS)
C ---------------------------------
        CALL OPNFIL(LFNORB,FILNAM(2,IFIL),'UNKNOWN','FORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNORB,IOSTAT,FILNAM(2,IFIL),'PRETAB')
C
C WRITE HEADER FOR T-FILES ("OTTAWA-FORMAT")
C ------------------------------------------
        CALL JMT(TSTART,IYEAR1,IMONT1,DAY1)
        CALL RADGMS(3,DMOD(TSTART,1.D0),VORZ,IHOUR1,IMIN1,SEC1)
        IDAY1=INT(DAY1)
        XHOUR1=IHOUR1
        XMIN1=IMIN1
        CALL JMT(TEND,IYEAR2,IMONT2,DAY2)
        CALL RADGMS(3,DMOD(TEND,1.D0),VORZ,IHOUR2,IMIN2,SEC2)
        IDAY2=INT(DAY2)
        XHOUR2=IHOUR2
        XMIN2=IMIN2
        XMJD0=DJUL(IYEAR1-1,12,31.D0)
        IYDAY=INT(TSTART-XMJD0+.002D0)
C
        WRITE(LFNORB,230)IYEAR1,IYDAY,ORBSYS(IORSYS)
230     FORMAT('TABULAR EPHEMERIS DERIVED FROM PREC. EPH:  PR',
     1          I4,'.',I3,'  SYSTEM ',A)
! --------------------------------------
! IERS2003 system transformation applied
! --------------------------------------
        BSNAM='BIAS'
        WRITE(LFNORB,'(A,A16,A4,5X,A,A16)')
     1         'CELESTIAL POLE OFFSET: ',NUTNAM,BSNAM,
     2         'SUBDAILY POLE MODEL: ',SUBNAM
! --------------------------------------
! CMC for OTL and ATL
! --------------------------------------
        IF (cmcyn(1) .OR. cmcyn(2)) THEN
          CALL getcmc(cmcyn,cmcmod=cmcmod,harstr=harstr)
          cmcchr='N'
          IF (cmcyn(1)) THEN
            cmcchr(1)='Y'
          ELSE
            cmcmod(1)='NONE'
            harstr='----'
          ENDIF
          IF (cmcyn(2)) THEN
            cmcchr(2)='Y'
          ELSE
            cmcmod(2)='NONE'
          ENDIF
          WRITE(LFNORB,'(A,A16,A,A20,A,A1,5X,A,A16,A,A1)')
     1           'LOADING CMC - OTLOAD: ',cmcmod(1),' / ',harstr,
     2           ' - ',cmcchr(1),'ATLOAD: ',cmcmod(2),' - ',cmcchr(2)
        ENDIF
        WRITE(LFNORB,242)IMONT1,IDAY1,IYEAR1,XHOUR1,XMIN1,SEC1,
     2               IMONT2,IDAY2,IYEAR2,XHOUR2,XMIN2,SEC2
242     FORMAT(I4,2I5,3D20.12,/,I4,2I5,3D20.12)
C
C NEW SATELLITE NUMBERS DUE TO MANOEUVRES
        NSATNW=NSAT
        DO 130 IMAN=1,NMAN
          IF (TIMMAN(IMAN).GT.TSTART.AND.TIMMAN(IMAN).LE.TEND) THEN
C
C CHECK IF MANOEUVRE SATELLITE (SVN+50) ALREADY EXISTS IN PRECISE FILE
            DO 105 ISAT=1,NSAT
              IF (SVNNUM(ISAT).EQ.SATMAN(IMAN)+50) GOTO 130
105         CONTINUE
C
C CHECK IF ORIGINAL SATELLITE (SVN) EXISTS IN PRECISE FILE
            DO 110 ISAT=1,NSAT
              IF (SVNNUM(ISAT).EQ.SATMAN(IMAN)) GOTO 120
110         CONTINUE
            GOTO 130
C
C MANOEUVRE WITHIN TIME INTERVAL OF TABULAR FILE
120         NSATNW=NSATNW+1
            SVNNUM(NSATNW)=SATMAN(IMAN)+50
            TIMACT(NSATNW-NSAT)=TIMMAN(IMAN)
C
C IF SAT IS "BAD" MANOEUVRE SAT IS ALSO "BAD"
            NBAD1=NBAD
            DO IBAD=1,NBAD1
              IF (BADSAT(IBAD).EQ.SATMAN(IMAN)) THEN
                IPOS=LISTI4(1,MAXSAT,BADSAT,SATMAN(IMAN)+50,NBAD)
                BADAC(IPOS)  = BADAC(IBAD)
              ENDIF
            ENDDO
C
          ENDIF
130     CONTINUE
C
C CHECK MAXIMUM NUMBER OF SATELLITES
        IF (NSATNW.GT.MAXSAT) THEN
          WRITE(LFNERR,901) NSATNW,MAXSAT
901       FORMAT(/,' *** PG PRETAB: TOO MANY SATELLITES',/,
     1                         16X,'NUMBER OF SATELLITES:',I4,/,
     2                         16X,'MAX. NUMBER ALLOWED :',I4,/)
          CALL EXITRC(2)
        ENDIF
C
C WRITE SATELLITE NUMBERS...
        WRITE(LFNORB,250)DTTAB,NEPO,UT1C1*86400,XPOLE1*ARS,
     1               YPOLE1*ARS,NSATNW-NBAD
250     FORMAT(D19.12,I7,/,D24.12,2D25.12////,I5)
        DO 255 ISAT=1,NSATNW
C
C ONLY IF SAT NOT "BAD"
          DO IBAD=1,NBAD
            IF (BADSAT(IBAD).EQ.SVNNUM(ISAT)) GO TO 255
          ENDDO
C
          WRITE(LFNORB,256) SVNNUM(ISAT)
256       FORMAT('SVN',I8,/)
255     CONTINUE
C
C GENERATION OF T-FILE
C --------------------
        IREADE=0     ! skip optional EP and EV records
        IREC=0
        TCLKA=0.D0
        DO 300 IEPO=1,NEPO
C
C READ EPOCH RECORDS
          CALL RDPREI(LFNOR1,IFRMAT,IREADE,NSAT,SVNNUM,TMJD,
     1                POSPRE,VELPRE,DTSATC,DDTSAT,ACCPOS,ACCVEL,
     2                EVTFLG,IEREC,SDEVP,SDEVV,CORRP,CORRV,IRC)
          TTUT1 = tmjd
          TTDT  = tmjd
          TGPS=.epochToReal.TMJD
C
          IF (TCLKA.EQ.0.D0) TCLKA=TGPS
          TCLKB=TGPS
C
C POLE-INFORMATION
          CALL POLDEF(TGPS,1,XPOLE,YPOLE,UT1UTC,GPSUTC)
          TUT1=TGPS-GPSUTC+UT1UTC
C
C PRECESSION AND NUTATION (TIME ARGUMENT: TDB, APPROX. AS TDT)
          TDT=TGPS+(19.D0+32.184D0)/86400.D0
          CALL PRCEFF(IORSYS,5.D0,TDT,PRE)
          CALL NUTEFF(IORSYS,0.1D0,TDT,NUT,EQEQUI,BIAS)
          PRE = matmul(PRE,BIAS)
C
C SIDERIAL TIME (TIME ARGUMENT: UT1)
          TTUT1%frac=ttut1%frac-GPSUTC+UT1UTC
          TTDT%frac =ttdt%frac +(19.D0+32.184D0)/86400.D0
          SZ=GSTIME(0,TTUT1,TTDT,NUT(2,1),EQEQUI)
          CALL sidmat(tdt,xpole,ypole,sz,sid)
C
C INITIALIZE MANOEUVRE SATELLITES
C -------------------------------
          DO 270 ISAT=1,NSATNW-NSAT
            POSMAN(1,ISAT)=0.D0
270       CONTINUE
C
C CALCULATE CMC IF DESIRED
C ------------------------
          CALL getcmc(cmcyn,TUT1,cmc)
C
C
C POSITION AT TGPS FOR EACH SATELLITE
C -----------------------------------
          DO 290 ISAT=1,NSATNW
C
C SKIP "BAD" SATELLITES
C ---------------------
            DO IBAD=1,NBAD
              IF (BADSAT(IBAD).EQ.SVNNUM(ISAT)) GO TO 290
            ENDDO
C
            IREC=IREC+1
C
C COPY READINGS FOR SATELLITE ISAT
            IF (ISAT .LE. NSAT) THEN
              POS(1:3)=POSPRE(1:3,ISAT)
              CLK=DTSATC(ISAT)
C
C IS THE FILE WITH INTERNAL NAME "SATCLRS" IN INPUT FILE "PRETABN.INP"
C AVAILABLE ? (IF IT IS TRUE ==> IRCCLK=0 !)
C --------------------------------------------------------------------
              IF (IRCCLK.EQ.0 .AND. IFRMAT.GE.2) THEN
C
C CONVERT MINIMUM TIME INTERVAL (DTTAB) FOR POLYNOMIAL FITTING TO PARTS
C OF A DAY FOR COMPARISON WITH TIME INTERVAL FOR POLYNOMIAL FITTING GI-
C VEN BY USER IN "PRETABNI.INP"
C ---------------------------------------------------------------------
                SAMPRT=(IDEG-1)*DTTAB/86400.D0
C
C TEST OF CONVENIENCE OF TIME INTERVAL CHOICE FOR POLYNOMIAL FITTING
C WITH RESPECT TO SAMPLING RATE IN A PRECISE EPHEMERIDES FILE ( SP3
C FORMAT ONLY !). IF CHOICE IS NOT CONVENIENT ==> WARNING !
C ------------------------------------------------------------------
                IF (DELT.LE.SAMPRT) GOTO 377
C
C NUMBER OF CLOCK RECORDS PER SATELLITE
C -------------------------------------
                IF (CLK.NE.999999.999999D0 .AND.
     1              CLK.NE. 99999.999900D0 .AND.
     2              CLK.NE. 99999.999999D0) THEN
                  NCLKEP(ISAT)=NCLKEP(ISAT)+1
                  IF (NCLKEP(ISAT).GT.MAXCKR) GOTO 1010
                  CLKCOR(NCLKEP(ISAT),ISAT)=CLK
                  CLKTIM(NCLKEP(ISAT),ISAT)=TGPS
                ENDIF
              ENDIF
C
C
C SKIP POSITIONS WITH 0.0000
              IF (POS(1).EQ.0.D0 .AND.
     1            POS(2).EQ.0.D0 .AND.
     2            POS(3).EQ.0.D0)       GOTO 290
C
C APPLY CMC IF DESIRED
              DO I=1,3
                POS(I)=POS(I)+cmc(I)
              ENDDO
C
C TRANSFORM EARTH-FIXED INTO B1950.0 OR J2000.0 SYSTEM
              CALL DMLMTV(POS,SID,POS)
              CALL DMLMTV(POS,NUT,POS)
              CALL DMLMTV(POS,PRE,POS)
C
              POS(1:3)=POS(1:3)/1D3
C
C MANOEUVRE SATELLITE ?
              DO 410 ISATNW=NSAT+1,NSATNW
                IF (SVNNUM(ISAT)+50.EQ.SVNNUM(ISATNW)) GOTO 420
410           CONTINUE
C
              WRITE(LFNORB,280) IREC,POS(1:3),EVTFLG(3:4,ISAT)
280           FORMAT(I5,3D25.15,1X,2A1)
              GOTO 290
C
C THIS EPOCH BEFORE OR AFTER MANOEUVRE ?
420           IF (TGPS.LT.TIMACT(ISATNW-NSAT)) THEN
                WRITE(LFNORB,280) IREC,POS(1:3),EVTFLG(3:4,ISAT)
              ELSE
                POSMAN(1:3,ISATNW-NSAT)=POS(1:3)
                EVTMAN(1:2,ISATNW-NSAT)=EVTFLG(3:4,ISAT)
              ENDIF
C
C WRITE SATELLITE POSITIONS AFTER MANOEUVRE
            ELSEIF (POSMAN(1,ISAT-NSAT).NE.0.D0) THEN
              WRITE(LFNORB,280) IREC,POSMAN(1:3,ISAT-NSAT),
     1                               EVTMAN(1:2,ISAT-NSAT)
            ENDIF
290       CONTINUE
C
C TEST IF TIME INTERVAL IS BIG ENOUGH FOR POLYNOMIAL FIT OF CLOCKS
C ----------------------------------------------------------------
          IF (IRCCLK.NE.0 .OR. IFRMAT.LT.2) GOTO 300
C
C REMARK: DIFFERENCE MUST BE COMPUTED IN A SERATE LINE OTHERWISE
C         ROUNDING PROBLEMS WITH IFC MAY APPEAR (RD, 22-AUG-2012)
          TCLK=TCLKB-TCLKA
          IF (TCLK+DTTAB/86400.D0 .GT. DELT .OR.
     1        IEPO.EQ.NEPO) THEN
            TCLKA=TCLKB
C
C COMPUTE POLYNOMIAL COEFICIENTS OF SATELLITE CLOCK CORRECTIONS, RMS OF
C POLYNOMIAL COEFICIENTS, VECTOR WITH RESIDUALS, RMS, INDEX OF REFERENCE
C TIME FOR POLYNOMIAL COEFICIENTS
C ----------------------------------------------------------------------
            DO 295 ISAT=1,NSAT
              FLAG=' '
C
C SKIP "BAD" SATELLITES
C ---------------------
              DO IBAD=1,NBAD
                IF (BADSAT(IBAD).EQ.SVNNUM(ISAT)) GO TO 295
              ENDDO
C
              IF (NCLKEP(ISAT).GE.NCOEF) THEN
C
C SPECIAL HANDLING OF REQUEST SMIN=0, IDEG=0
C ------------------------------------------
                IF (DELT.EQ.0.0.AND.NCOEF.EQ.1) THEN
                  COEF(1)=CLKCOR(1,ISAT)
                  TCLK=TGPS
C EVENT AND PREDICTION FLAGS
                  FLAG=EVTFLG(2,ISAT)
                  IF (EVTFLG(1,ISAT).NE.' ') FLAG=EVTFLG(1,ISAT)
                ELSE
                  CALL POLYAP(CLKTIM(1,ISAT),CLKCOR(1,ISAT),L,
     1                        NCLKEP(ISAT),NCOEF,COEF,CRMS,RES,RMS,
     2                        IREFP)
C
C CONVERSION OF POLYNOMIAL COEFICIENTS ( BEGINNING WITH THE 2ND COEFF. -
C A1 ) OF SATELLITE CLOCK CORRECTIONS TO THE CORRECT UNITS
C ----------------------------------------------------------------------
                  WRITE(LFNPRT,3073)SVNNUM(ISAT),CRMS(1)*1D9
3073              FORMAT(1X,'PRN=',I3,1X,' CLK RMS:',F10.3,' NS')
                  DO 373 I=2,NCOEF
                    COEF(I)=COEF(I)/(86400.**(I-1))
                    WRITE(LFNPRT,3074)CRMS(I)*1.D9/864.D2,I-1
3074                FORMAT(9X,' CLK RMS:',F10.3,' NS/DAY**',I1)
373               CONTINUE
C
                  TCLK=CLKTIM(IREFP,ISAT)
                ENDIF
C
C OPEN CORRESPONDING CLOCK FILE (IF EXIST - IRCCLK=0), WRITE ITS HEADER
C AT THE FIRST PASS OF LOOP
C ---------------------------------------------------------------------
                IF (ICLKSV.EQ.0) THEN
                  CALL WTSATH(FLNCLK,LFNRES,filTitle)
                  ICLKSV=1
                ENDIF
C
C WRITE A FILE WITH RELEVANT INFORMATIONS FOR SINGLE POINT POSITIONING -
C CODSPP (SAT. NUMBER, GPS WEEK, SECONDS OF GPS WEEK, NUMBER OF POLYNO-
C         MIAL COEFFICIENTS, POLYNOMIAL COEFFICIENTS)
C ----------------------------------------------------------------------
                IF (CRMS(1)*1D9.LT.1000.0) THEN
                  CALL WTSATI(LFNRES,SVNNUM(ISAT),TCLK,
     1                               NCOEF,COEF,IRC)
                ELSE
                  WRITE(LFNERR,903) SVNNUM(ISAT),FILNAM(1,IFIL),
     1                              FLNCLK,CRMS(1)*1D9
903               FORMAT(/,' ### PG PRETAB: SATELLITE CLOCK VAL',
     1                     'UES NOT WRITTEN',/,
     2                   16X,'SATELLITE  :',I5,/,
     3                   16X,'FILE NAME  : ',A,/,
     4                   16X,'CLOCK FILE : ',A,/,
     4                   16X,'COEFF RMS  : ',F10.3,/)
                ENDIF
                IFLCLK(IFIL)=1
C
C NUMBER OF COEFFICIENTS
C ----------------------
                IF (DMOD(DELT,DTTAB/86400.D0).LT.1.D-7 .AND.
     1              CLKTIM(NCLKEP(ISAT),ISAT).EQ.TCLKB) THEN
                  CLKTIM(1,ISAT)=CLKTIM(NCLKEP(ISAT),ISAT)
                  CLKCOR(1,ISAT)=CLKCOR(NCLKEP(ISAT),ISAT)
                  NCLKEP(ISAT)=1
                ELSE
                  NCLKEP(ISAT)=0
                ENDIF
C
C SPECIAL HANDLING OF REQUEST SMIN=0, IDEG=0
C ------------------------------------------
                IF (DELT.EQ.0.0.AND.NCOEF.EQ.1) THEN
                  NCLKEP(ISAT)=0
                ENDIF
              ELSE
C
C NOT ENOUGH CLOCK VALUES FOUND IN INTERVAL
                IF (PRTERR(ISAT).EQ.0) THEN
                  WRITE(LFNERR,902) SVNNUM(ISAT),IFIL,FILNAM(1,IFIL)
902               FORMAT(/,' ### PG PRETAB: SATELLITE CLOCK VAL',
     1                     'UES MISSING',/,
     2                   16X,'SATELLITE  :',I5,/,
     3                   16X,'FILE NUMBER:',I5,/,
     4                   16X,'FILE NAME  : ',A,/)
                  PRTERR(ISAT)=1
                ENDIF
                NCLKEP(ISAT)=0
              ENDIF
C
295         CONTINUE
C
          ENDIF
C
300     CONTINUE
C
C CLOSE INPUT AND OUTPUT FILE
C ---------------------------
        CLOSE(UNIT=LFNOR1)
        CLOSE(UNIT=LFNORB)
C
C WRITE A MESSAGE IN OUTPUT FILE IF NO SATELLITE CLOCK CORRECTIONS
C ARE AVAILABLE IN PRECISE EPH. FILE
C ----------------------------------------------------------------
        IF(IFLCLK(IFIL).EQ.0.AND.IRCCLK.EQ.0.AND.IFRMAT.LE.1) THEN
          WRITE(LFNPRT,375) IFRMAT+1,IFIL,FILNAM(1,IFIL)
375       FORMAT(/,' ### PG PRETAB: NO SATELLITE CLOCK VALUES ',
     1           'AVAILABLE IN SP',I1,' FORMAT',/,
     2           16X,'FILE NUMBER: ',I4,/,
     3           16X,'FILE NAME  : ',A,/)
        ENDIF
C
C FOR SP3 FORMAT
C --------------
        IF(IFLCLK(IFIL).EQ.0.AND.IRCCLK.EQ.0.AND.IFRMAT.GE.2) THEN
          WRITE(LFNPRT,374) IFIL,FILNAM(1,IFIL)
374       FORMAT(/,' ### PG PRETAB: NO SATELLITE CLOCK VALUES FOUND IN',
     1             ' SP3 FORMAT FILE',/,
     2             16X,'FILE NUMBER: ',I4,/,
     3             16X,'FILE NAME  : ',A,/)
        ENDIF
C
C WRITE MESSAGE IF ACCURACY CODES ARE USED
C ----------------------------------------
        DO IBAD=1,NBAD
          IF (BADAC(IBAD).EQ.-999) THEN
            WRITE(LFNPRT,476) FILNAM(1,IFIL),BADSAT(IBAD),
     1                        TRIM(FILCRX)
          ELSEIF (BADAC(IBAD).LT.MINAC) THEN
            WRITE(LFNPRT,477) FILNAM(1,IFIL),BADSAT(IBAD),BADAC(IBAD)
          ELSE
            WRITE(LFNPRT,478) FILNAM(1,IFIL),BADSAT(IBAD),BADAC(IBAD),
     1                          MAXAC
          ENDIF
        ENDDO
C
476     FORMAT(/,' ### PG PRETAB: SATELLITE EXCLUDED DUE TO',
     1             ' ENTRY IN SATCRUX',/,
     2             16X,'FILE NAME       : ',A,/,
     3             16X,'SATELLITE NUMBER: ',I4,/,
     4             16X,'SATCRUX FILE    : ',A,/)
C
477     FORMAT(/,' ### PG PRETAB: SATELLITE EXCLUDED DUE TO',
     1             ' ACCURACY CODE',/,
     2             16X,'FILE NAME       : ',A,/,
     3             16X,'SATELLITE NUMBER: ',I4,/,
     4             16X,'ACCURACY CODE   : ',I4,/)
C
478     FORMAT(/,' ### PG PRETAB: SATELLITE EXCLUDED DUE TO',
     1             ' ACCURACY CODE',/,
     2             16X,'FILE NAME        : ',A,/,
     3             16X,'SATELLITE NUMBER : ',I4,/,
     4             16X,'ACCURACY CODE    : ',I4,/,
     5             16X,'MAX ACCURACY CODE: ',I4,/)
C
C
        GOTO 1000
C
1010    EPOHR=3600.D0/DTTAB
        IUSRHR=IDNINT((SMIN/60.D0)*EPOHR)+1
C
C WRITE A MESSAGE IN OUTPUT FILE, WHEN TIME INTERVAL FOR POLYNOMIAL
C FITTING OF SATELLITE CLOCK CORRECTIONS SPECIFIED BY USER IS TOO
C LARGE
C -----------------------------------------------------------------
        WRITE(LFNERR,380) IFIL,FILNAM(1,IFIL),IUSRHR,MAXCKR
380     FORMAT(/,' *** PG PRETAB: TOO MANY CLOCK VALUES FOR POLYNOMIAL',
     1           ' FIT',/,
     2           16X,'INCREASE PARAMETER MAXCKR IN PG PRETAB',/,
     3           16X,'FILE NUMBER                        :',I5,/,
     4           16X,'FILE NAME                          : ',A,/,
     5           16X,'NUMBER OF CLOCK VALUES PER INTERVAL:',I5,/,
     6           16X,'MAX. NUMBER OF CLOCK VALUES ALLOWED:',I5,/)
        CALL EXITRC(2)
C
C WRITE A MESSAGE IN OUTPUT FILE, WHEN TIME INTERVAL FOR POLYNOMIAL
C FITTING OF SATELLITE CLOCK CORRECTIONS SPECIFIED BY USER IS LESS
C THAN SAMPLING RATE GIVEN IN A PRECISE EPHEMERIS FILE
C -----------------------------------------------------------------
C
377     WRITE(LFNERR,378) SMIN,DTTAB,IFIL,FILNAM(1,IFIL)
378     FORMAT(/,' *** PG PRETAB: NOT ENOUGH CLOCK VALUES IN USER-',
     1           'SPECIFIED INTERVAL',/,
     2           16X,'TO BE FITTED BY A POLYNOMIAL OF DEGREE',I2,/,
     3           16X,'TIME INTERVAL (MIN)  :',F8.2,/,
     4           16X,'SAMPLING RATE IN FILE:',F8.2,/,
     5           16X,'FILE NUMBER          :',I8,/,
     6           16X,'FILE NAME            : ',A,/)
        CALL EXITRC(2)
C
C NEXT FILE
C
1000  CONTINUE
C
C CLOSE SATELLITE CLOCK FILE
C --------------------------
      IF (ICLKSV.NE.0.AND.IRCCLK.EQ.0) THEN
        WRITE(LFNRES,'(/)')
        CLOSE (UNIT=LFNRES)
      ENDIF
C
      WRITE(LFNPRT,'(/)')
C
      CALL EXITRC(0)
      END
