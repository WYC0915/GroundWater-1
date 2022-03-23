C*
      PROGRAM RXOBV3
CC
CC NAME       :  RXOBV3
CC
CC PURPOSE    :  TRANSFORM A RINEX OBSERVATION FILE (VERSION 1 OR 2)
CC               INTO BERNESE FILES VERSION 3.5.
CC               THE FOLLOWING FILES ARE CREATED:
CC                 - FILE WITH CA/P-CODES
CC                 - FILE WITH CARRIER PHASES
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  W.GURTNER / M.ROTHACHER
CC
CC CREATED    :  89/04/15 12:20
CC
CC CHANGES    :  04-JUL-91 : ??: COMPUTE OBSERVATION INTERVAL FROM DATA.
CC                               REJECT OBSERV. EPOCHS NOT ORDERED IN TIME.
CC               06-JUL-91 : ??: MEASUREMENT TYPE "C2" WAS NOT ACCEPTED.
CC               10-SEP-91 : ??: INCLUDE SAMPLING RATE/OFFSET
CC                9-APR-92 : ??: ANTENNA HEIGHT TRANSLATION TABLE
CC               23-APR-92 : ??: IRCOBS=3: CONTINUE WITH NEXT FILE
CC               31-JUL-92 : ??: DO NOT WRITE HEADER FILE, IF NO OBS.
CC               06-AUG-92 : ??: MAXGAP=300
CC               07-AUG-92 : LM: HIGHER PRIORITY FOR P-CODE (L1)
CC               07-AUG-92 : LM: CHECK IF -9999.000 IN RINEX FILE
CC                               APPEARS
CC               19-AUG-92 : EB: IF RINEX-PROBLEMS, TAKE NEXT FILE
CC               20-OCT-92 : WG: READ ERROR IN R2RDOR: CLOSE FILE,
CC                               BUT CONTINUE NORMALLY
CC               22-OCT-92 : MR: NUMBER OF EPOCHS FOR CODE AND PHASE
CC                               WERE REVERSED FOR WRITE OF HEADER
CC               23-DEC-92 : MR: USE OF SR "OPNFIL" TO OPEN FILES
CC               11-JAN-93 : MR: ADD "TIMSTR" IN SR CALLED
CC                2-JUN-93 : WG: CHANGE OF WAVELENGTH FACTORS ALLOWED
CC                               CHANGE OF OBSERVATION TYPES  ALLOWED
CC                               (EVENT FLAG 4)
CC               06-AUG-93 : LM: NEW FORMAT, VERSION 3.5
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               04-JAN-94 : MR: SITE-SPECIFIC ANT.-REC.TRANSLATION
CC               10-AUG-94 : MR: CALL EXITRC
CC               14-AUG-94 : MR: FORMAT 4: SESSION AS CHARACTER*4
CC               20-SEP-94 : MR: ADD PARAMETER STOP PARAMETER TO GTSTNA
CC                               AND GTANTH
CC                8-SEP-95 : WG: ELIMINATE 2ND OBS WHERE P1=P2 OR L1=L2
CC               27-SEP-95 : WG: INCLUDE SLR RANGES (R1,R2)
CC               06-NOV-95 : MR: CHECK FOR SAMPLING BELOW 1 SEC: WARNING
CC               25-JAN-96 : MR: CORRECT FACTOR OF TEN WHEN COMPUTING
CC                               "IDTSUB"
CC               07-MAY-96 : TS: ADDED TIME WINDOW
CC               10-MAY-96 : TS: SMALL TIME WINDOW CORRECTION
CC               13-MAY-96 : TS: INTERPRET NEW HEADER IN FILE SIMILAR TO
CC                               EVENT FLAG 4
CC               12-JUL-96 : TS: CHANGED CALL OF GTSTNA
CC               13-JAN-97 : TS: MAX.SIGNAL ALSO FOR CODE ACTIVATED
CC               23-JAN-97 : TS: SET UP AMBIGUITIES IN ZD-FILES HEADER
CC               24-JAN-97 : TS: FLAG FOR ACCEPTING CYCLE FLAGS OR NOT
CC               21-AUG-97 : TS: ALLOW FILE TO BE WRITTEN WITH ONLY 1 EPOCH
CC                               (FOR SLR)
CC               24-SEP-97 : DI: USE MAXSAT.inc
CC               07-OCT-97 : TS: CORRECT EPOCHS FOR AMBIGUITIES IN HEADER
CC                               NO INITIALIZATION OF PHASE IF ICSFLG.EQ.1
CC               14-OCT-97 : TS: BUG CORRECTED WITH AMBIEP
CC               22-OCT-97 : HH: ADD GLONASS
CC               10-NOV-97 : TS: CHECK IANTEN.LE.999999
CC               23-FEB-98 : MR: CORRECT GPSUTC TO SECONDS
CC               04-MAY-98 : SS: SR DEFREQ MODIFIED
CC               17-NOV-98 : MR: SET "TIMREF" TO "TFIRST" IF NO DATA
CC               25-MAR-99 : DI: WRITE NO CODE HEADER FILE IF NEPOCH.LT.2
CC               12-JUL-99 : SS: "STASTR" IN CALL OF SR GTSTNA
CC               08-OCT-99 : TS/SS: "OBSTYP" IN CALL OF SR R2RDOR
CC               23-MAY-00 : SS: "MAXAMB" FROM 600 TO 800
CC               21-JUL-00 : HU: DO NOT ALLOW HORIZONTAL ANTENNA OFFSETS
CC               04-AUG-00 : LM: APPLY ABBREVIATION TABLE TO FILENAMES
CC               06-AUG-00 : HU: 'IACPT0' ADDED AS PARAMETER OF RXOB3I
CC               16-DEC-00 : RD: SWITCH TO THE NEW MENU SYSTEM
CC               18-DEC-00 : RD: USE STACRUX FILES
CC               26-FEB-01 : RD: A BETTER HANDLING OF BAD STATIONS
CC               07-MAR-01 : LM: INCLUDE MAXAMB
CC               06-JUN-01 : RD: AMB. FROM RINEX FILE WITH A SAMPLING RATE
CC               23-OCT-01 : RD: USE ONLY THE NEW TABLES
CC               01-NOV-01 : RD: AMB. FROM RINEX FILE WITH A TIME WINDOW
CC               16-DEC-01 : HU: USE D_CONST
CC               18-DEC-01 : RD: IDELTT IS AN ARRAY
CC                               DEFINE SAMPLING RATE TO 1 SEC FOR RANGES
CC               21-Dec-01 : HU: USE M_BERN, ONLY FOR MODULES
CC               15-JAN-02 : MM: DIMTST (MAXTYP)
CC               18-APR-02 : RD: OUTPUT FILE NAMES NEED IFLINP INSTEAD OF IFLOUT
CC               19-JUL-02 : SS: SATELLITE SYSTEM SELECTION
CC               25-SEP-02 : HU: REMOVE I_ASTLIB, USE INTERFACE TO RNXNEW
CC               06-NOV-02 : RD: COMPARE SAMPLING FROM INPUT AND FROM RINEX DATA
CC               15-NOV-02 : HU: CHECK SAMPLING TO DTEST=0.5 SEC
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               19-MAR-03 : RD: UPDATE ABBREVIATION TABLE
CC               20-MAR-03 : RD: MINUMUM NUMBER OF EPOCH REQUEST
CC               21-Mar-03 : RD: CHECK FOR DOUBLE OUTPUT FILE NAMES
CC               15-Apr-03 : RD: INIT TIME WINDOW TO (/0d0,1d20/)
CC               24-APR-03 : RD: GENERAL RXOBV3 UPDATE
CC               18-MAY-03 : HU: INITIALIZE STRUCTURE
CC               23-JUN-03 : HB: CHANGE DIMENSION OF POSECC AND ANTNEW FROM
CC                               (3) to (3,3), NEW SR RXOTYP FOR CHECKING
CC                               MARKER TYPE
CC               08-SEP-03 : HU: ANTNAM, RECNAM IS CHR20 FOR WTHEAD
CC               11-SEP-03 : CU: FOR PRINTING OF SUMMARY: CHECK IF OBSERVATIONS
CC                               FOUND
CC               12-SEP-03 : CU: CLOSE LFNOBS IF NO OBSERVATIONS FOUND
CC               13-SEP-03 : HU: INTERFACE FOR DEFREQ
CC               04-NOV-03 : RD: CORRECT MESSAGE IN NO DATA IN TIME WINDOW
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               12-JAN-04 : RD: SKIP SAT. W/O FREQ IN DEFREQ
CC               29-MAR-04 : CU: CHECK FREQUENCY FOR RANGE MEASUREMENTS USING
CC                               FRQ INFO FILE, READ EPOCH(2) USING
CC                               -NUMTYP(R2RDOR), ADD  RNGOBS TO CALL OF
CC                               SR SAVMEA
CC               24-MAY-04 : HU: OBSERV. HEADER FORMAT 5
CC               17-JAN-05 : HB: UPDATE COORDINATES ONLY FOR STATIONS WHICH
CC                               ARE OK
CC               03-JUN-05 : HU/RD: CHECK IF NUMAMB > MAXAMB
CC               21-JUN-05 : MM: LFNUM.inc REMOVED (LFNUMs IN M_BERN)
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-JUN-05 : MM: UNUSED VARIABLES REMOVED
CC               08-AUG-05 : HB: USE NEW SR TIMST2 (MODULE)
CC               11-AUG-05 : HU: CALL TIMST2 WITH ARRAY
CC               28-SEP-05 : RD: INPUT OPTION FOR EPOCH FLAG HANDLING
CC               08-FEB-06 : RD: OPTION TO SKIP L2C-DATA
CC               16-AUG-06 : HU: MAXTYP 10 -> 12
CC               27-FEB-07 : AG: CALL DEFCON WITH PARAMETER
CC               17-MAY-07 : AG: IRXVRS ADAPTED FOR GALILEO
CC               11-DEC-07 : HB: ADD MEATYP AS PARAMETER TO SR RXOANT
CC               31-JAN-08 : RD: CORRECT FORMAT FOR NONZERO HORIZ. ANTENNA ECC.
CC               01-FEB-08 : HB: BUG FIX: EARLIER DEFINITION OF MEATYP
CC               03-JUN-09 : SL: FLGSTR(3) -> FLGSTR IN RXOCRX CALL
CC               23-JUL-09 : DT: NO ERROR FOR NONZERO HORIZ. ANTENNA ECC.
CC                               IF RANGE OBS
CC               24-JUL-09 : DT: ADD IMEA TO CALL OF RXOB3F; NO WARNING FOR
CC                               OBSERVATIONS NOT ORDERED IN TIME IF RANGE
CC                               OBSERVATIONS
CC               02-JUN-10 : SS: AVOID SATELLITE-SPECIFC WL FACTORS OF 2 FOR L1
CC               08-JUL-10 : EO: HANDLING OF STATIONS PROBLEMS
CC               24-AUG-10 : RD: SHIFT "IRQEPO"-REQUEST INTO A SUBROUTINE
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               05-OCT-10 : SL: USE M_BERN WITH ONLY, STAINFO VERSION 1.01
CC               23-NOV-10 : RD: 4 DIGIT YEARS IN WARNING MESSAGE
CC               05-JAN-11 : RD: ADD RINEX NAME TO SAVMEA
CC               18-JAN-11 : SL: MAXTYP 12->26
CC               20-JAN-11 : LP,SL: MAXTYP DEFINED IN D_RINEX3
CC               26-JAN-11 : LP: Sat-specific obs types; DEFREQ changes;
CC               17-FEB-11 : RD: REPLACE UPSTAT BY WTSTAT
CC               14-JUN-11 : SL: COPY OF OBSTY4 TO OBSTYP
CC               13-SEP-11 : LP: maxcom 10->30
CC               04-NOV-11 : LP: Re-initialization of gobsdef for every file
CC               14-NOV-11 : SL: PRITIT CALL CHANGED
CC               15-DEC-11 : SL: WRITE FORMAT * CHANGED TO A
CC               21-JAN-12 : RD: CORRECT CONDITION FOR REMOVED GNSS
CC               02-FEB-12 : RD: DEFINE REQUIREMENTS ON A BERNESE OBS. FILE
CC               09-FEB-12 : LP: MAXCOM 30->60
CC               14-FEB-12 : RD: HANDLE INCONSISTENT SAMPL. IN PANEL AND FILE
CC               05-MAR-12 : RD: USE WTHEAD AS MODULE NOW
CC               26-APR-12 : LP: MAXCOM 60->150; deactivate obstype selection
CC                               via sat-specific geos file (select obstypes in
CC                               RNXSMT only)
CC               27-APR-12 : RD: NULLIFY POINTERS
CC               04-MAY-12 : RD: USE DMOD FROM MODULE
CC               13-JUN-12 : DT: ALLOW FILES WITH 1 OBS. IF REQEPO=0
CC               31-JAN-13 : LP: BUGFIX FOR SETTING OF IRXVRS
CC               07-MAR-13 : SS: MAXCOM FROM 150 TO 300 (DUE TO BISK/POUS)
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1989     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: i4b,r8b,
     1                    keyValueLength, fileNameLength, staNameLength,
     2                    lfn001, lfn002, lfnPrt, lfnErr
      USE m_cpu,    ONLY: cpu_start
      USE m_time,   ONLY: t_timint
      USE m_maxdim, ONLY: maxsat, maxamb
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE p_rxobv3, ONLY: t_rxobv3_err, t_rxobv3_req
      USE p_gpsest, ONLY: maxamb2 => maxamb
      USE d_stacrx, ONLY: t_staCrux,init_staCrux
      USE d_const,  ONLY: date,time
      USE d_rinex3, ONLY: maxtyp, t_gobsdef
      USE l_basfun, ONLY: dmod
      USE s_dimtst
      USE s_opnfil
      USE s_prflna
      USE s_mjdgps
      USE s_r3init
      USE s_r2rdoh
      USE s_pritit
      USE s_savmea
      USE s_rxosta
      USE s_readinpf
      USE s_opnerr
      USE s_r2rdor
      USE s_rxofrq
      USE s_rnxses
      USE s_timst2
      USE s_setflg
      USE s_timst2
      USE s_gtflna
      USE s_inquire
      USE s_getco3
      USE s_wtstat
      USE s_updmea
      USE s_rxob3f
      USE s_rnxamb
      USE s_rxob3i
      USE s_rxoant
      USE s_defreq
      USE s_defcon
      USE s_exitrc
      USE s_rxotyp
      USE s_opnsys
      USE s_rxobabbr
      USE s_wthead
      USE s_rxocrx
      USE s_stripdir
      USE s_fewobs
      USE f_dgpsut
      USE f_tstflg
      USE s_gobsdef,ONLY: init_geos
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IABUPD, IACPT0, IANTE4, IANTEN, IANTNW, ICLOCK, ICSFLG,
     1          IDTNEW, IDTRN4, IDTRNX, IDTSBN, IDTSUB, IEPBAD, IFIL  ,
     2          IFIL1 , IFLGEP, IFLINP, IFLOUT, IFREQ , IFRMAT, IFRQ  ,
     3          INDLLI, IOBGAP, IOFSET, IOSTAT, IRC   , IRCHE4, IRCHED,
     4          IRCOBS, IRCODE, IREC  , IRECNW, IRMARK,
     5          IRUNI4, IRUNIT, IRXSEL, IRXVR4, IRXVRS, ISA4  , ISASYS,
     6          ISAT  , ISATEP, ISIGNL, ISINTV, ISOFFS, ITYP  , IUSWIN,
     7          JFIL  , JFIL1 , JSAT  , K     , KSAT  , LFNOBS, MAXCOM,
     8          MAXFIL, MAXGAP, MAXREC, MEATYP, MEPOCH, MINEPO,
     9          MINGAP, MINSIG, MRKTEX, MXCAMB, MXCCOM, MXCFIL, MXCSAT,
     1          MXCTYP, NCOM  , NCOM4 , NDIFF , NFLINP, NFLOUT,
     2          NFRSUM, NOBGAP, NOFRQ , NSASUM, NSATE4, NSATEL, NSATEP,
     3          NUMAM0, NUMAMB, NUMLIN, NUMTY4, NUMTYP, NWEEK , NWLSA4,
     4          NWLSAT, ILINE , NOL2C , usealternative, igeos , indgeos,
     5          NSTAT , NDUMMY
C
      REAL*8    DELTT2, DTEST , EPOCH , GPSUTC, SECOND, TEST  ,
     1          TFIRS4, TFIRST, TIMOLD, TLAST4
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C MAXIMAL DIMENSIONS
C ------------------
      PARAMETER (MAXFIL=1000,MAXCOM=300,MAXGAP=300)
C
C
C MAXSAT: MAXIMUM NUMBER OF SATELLITES
C MAXFIL: MAXIMUM NUMBER OF PHASE OR CODE FILES TO BE CREATED
C MAXTYP: MAXIMUM NUMBER OF RINEX OBSERVATION TYPES
C MAXCOM: MAXIMUM NUMBER OF COMMENT LINES
C MAXGAP: MAXIMUM NUMBER OF SEPARATE INTERVALS OF OBSERVATIONS
C MAXAMB: MAXIMUM NUMBER OF AMBIGUITIES
C
C DECLARATIONS
C ------------
C     error handling options
      TYPE(t_rxobv3_err)     :: erropt
C     define requirements on an obs. file
      TYPE(t_rxobv3_req)     :: reqObs
C     stacrux: old entries in RINEX files
      TYPE(t_staCrux)        :: rnxInfo
C     stacrux: new entries for Bernese
      TYPE(t_staCrux)        :: staInfo
      TYPE(t_timint)         :: timint
      TYPE(t_gobsdef)        :: GOBSDEF, GOBSDEFINIT ! sat-specific
c                                         observation type selection info
C
      CHARACTER*150  LINE(MAXFIL,3)
      CHARACTER(keyValueLength) flgStr(5)
      CHARACTER*80   STRING
      CHARACTER*53   TITLE
      CHARACTER*40   TSTRNG,EPONOF
      CHARACTER*(fileNameLength)   FILPRT(4), FILCRD
      CHARACTER*20   CAMPGN,RECTNW,ANTTNW
      CHARACTER*9    CRDATE(2)
      CHARACTER*6    MXNFIL,MXNSAT,MXNAMB,MXNCOM,MXNTYP
      CHARACTER*5    CRTIME(2)
      CHARACTER*4    SESSION
      CHARACTER*3    TIMSYS
      CHARACTER*1    CODFLG(MAXSAT,2),PHAFLG(MAXSAT,2),EPOFLG(2)
      CHARACTER*1    RNGOBS,OBST,STAFLG(MAXFIL)
      CHARACTER*(fileNameLength)   DUMMY1
      CHARACTER*16   DUMMY2,STANMS(MAXFIL),DATUM
      CHARACTER(LEN=staNameLength),POINTER,DIMENSION(:) :: cDummy
C
      INTEGER*4 NSMEA(2),SVNMEA(MAXSAT,2),IOK1(2),IDELTT(2)
      INTEGER*4 NFREQ(2),NEPOCH(2),NEPFLG(2)
      INTEGER*4 ANZCOD(MAXSAT,2),ANZPHA(MAXSAT,2),NUMMRK(MAXSAT,2)
      INTEGER*4 ANZOBS(MAXSAT,2,2)
      INTEGER*4 AMBSAT(MAXAMB),AMBIEP(MAXAMB),AMBWLF(MAXAMB,2)
      INTEGER*4 AMBCLS(MAXAMB,3)
      INTEGER*4 NEPGAP(MAXGAP),INDINP(MAXFIL),IOKPRT(2,MAXFIL)
      INTEGER*4 RNXSTA,NRXOBS(MAXGAP,6)
      INTEGER*4 IOBPRT(4,MAXFIL),ISAPRT(4,MAXFIL),IAMPRT(MAXFIL)
      INTEGER*4 numfreq,usegeos
      INTEGER*4 SATNOF(MAXSAT),LSTNOF(MAXSAT),ITYPE
c      INTEGER*4 IG, JG, IREPL
C
      REAL*8    TIMREF(2),TLAST(2),TIMGAP(2,MAXGAP),EPOBAD(2)
      REAL*8    PHAL12(MAXSAT,2),CODL12(MAXSAT,2),PHA001(MAXSAT,2)
      REAL*8    AMBIGU(MAXAMB,3),WINDOW(2,MAXFIL),EPOFRQ(2)
      REAL*8    TIMEPO(3),TIMNOF(2,MAXSAT)
      REAL*8    RXFREQ,XSTAT(3,MAXFIL),TIMCRD
C
      LOGICAL*4 SAVEPO, MRKTYP_EX
      LOGICAL   CHKFREQ, COPYBACK
      LOGICAL   YES
C
C  RXOB3F
      CHARACTER*(fileNameLength)   FILPHA(2,MAXFIL),FILCOD(2,MAXFIL)
      CHARACTER*(fileNameLength)   RXFILE(MAXFIL), SCROBS(2)
      CHARACTER*16   STNAME(MAXFIL)
      CHARACTER*4    CSESS(2,MAXFIL)
      INTEGER*4      iMea
C
C  R2RDOH
      CHARACTER*20 PRGNAM,RUNBY,RCVERS,OPRNAM
      CHARACTER*40 ANTTYP,RECTYP
      CHARACTER*40 AGENCY
      CHARACTER    CRDATX*9,CRTIMX*5,COMENT(MAXCOM)*60,OBSTYP(MAXTYP)*2
      CHARACTER    STANAM*60,STANUM*40
      REAL*8       POSXYZ(3),POSANT(3,3),ANTNEW(3,3)
      INTEGER*4    IWLRNX(2),NUMSAT(MAXSAT),NUMOBS(MAXSAT,MAXTYP)
      INTEGER*4    IWLSAT(3,MAXSAT)
C
      CHARACTER*20 PRGNA4,RUNBY4,RCVER4,ANTTY4,RECTY4,OPRNA4
      CHARACTER*40 AGENC4
      CHARACTER    CRDAT4*9,CRTIM4*5,COMEN4(MAXCOM)*60,OBSTY4(MAXTYP)*2
      CHARACTER    STANA4*60,STANU4*20
      REAL*8       POSXY4(3),POSAN4(3)
      INTEGER*4    IWLRN4(2),NUMSA4(MAXSAT),NUMOB4(MAXSAT,MAXTYP)
      INTEGER*4    IWLSA4(3,MAXSAT),norec,icom
C
C  R2RDOR
      INTEGER*4    SATEP(MAXSAT),LLI(MAXSAT,MAXTYP),ISIGN(MAXSAT,MAXTYP)
      REAL*8       OBSEPO(MAXSAT,MAXTYP)
      REAL*8       RXEPOCH(2)
C
C  RNXAMB
      REAL*8       AMBNEP(MAXAMB)
C
C  RDOBSI
c      CHARACTER*1  EPFLAG,    OBSFLG(MAXSAT,2)
c      INTEGER*4    IFRQS(2),  NRSAT (MAXSAT)
c      REAL*8       DELTAT(2), OBSERV(MAXSAT,2)
C
C  WTHEAD
      CHARACTER(LEN=16),DIMENSION(2)      :: hlp_staNam
      CHARACTER(LEN=20),DIMENSION(2)      :: hlp_recTyp
      CHARACTER(LEN=20),DIMENSION(2)      :: hlp_antTyp
      INTEGER(i4b),DIMENSION(2)           :: hlp_irUnit
      INTEGER(i4b),DIMENSION(2)           :: hlp_iAnten
      CHARACTER(LEN=20),DIMENSION(2)      :: hlp_oprNam
      REAL(r8b),DIMENSION(3,2)            :: hlp_posecc
      INTEGER(i4b),DIMENSION(2)           :: hlp_iClock
C
C COMMON BLOCKS
C -------------
      COMMON/LARGE/ FILPHA,FILCOD,RXFILE,STNAME,CSESS,LLI,ISIGN,OBSEPO,
     1              AMBSAT,AMBIEP,AMBWLF,AMBCLS,AMBIGU
      INCLUDE 'COMFREQ.inc'
      COMMON/MCMFIL/MXCFIL,MXNFIL
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMAMB/MXCAMB,MXNAMB
      COMMON/MCMCOM/MXCCOM,MXNCOM
      COMMON/MCMTYP/MXCTYP,MXNTYP
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C DEFINE LOGICAL FILE NUMBERS
C ---------------------------
      LFNOBS=LFN001+1
      NULLIFY(CDUMMY)
C
C INITIALIZE COMMON BLOCKS FOR MAXIMAL DIMENSIONS
C -----------------------------------------------
      MXCAMB=MAXAMB
      MXCSAT=MAXSAT
      MXCFIL=MAXFIL
      MXNAMB='MAXAMB'
      MXNSAT='MAXSAT'
      MXNFIL='MAXFIL'
      MXNCOM='MAXCOM'
      MXNTYP='MAXTYP'
C
      MAXREC=1000000
C      IFRQS(1)=1
C      IFRQS(2)=2
      ERROPT%STPERR=.FALSE.
      CALL init_staCrux(staInfo)
      CALL init_staCrux(rnxInfo)
C
      LINE(1:MAXFIL,1:3)=""
      CHKFREQ = .FALSE.
C
C TEST INTERVAL FOR EQUAL EPOCHS (SEC)
C ------------------------------------
      DTEST=0.5D0
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
C Print title section
C -------------------
      CALL PRITIT('RXOBV3',
     1     'Transfer RINEX observation files to Bernese format',132)
      CALL PRFLNA(132)
C
C CREATION AND MODIFICATION DATE AND TIME
C ---------------------------------------
      CRDATE(1)=DATE
      CRTIME(1)=TIME
      CRDATE(2)=DATE
      CRTIME(2)=TIME
C
C ADDITIONAL HEADER INFORMATION
C -----------------------------
      IFRMAT=5
      NDIFF= 0
      IRMARK=0
      ICLOCK=0
      IOFSET=0
      DELTT2=0.D0
      NSTAT=0
C
C READ OPTION INPUT FILE
C ----------------------
      USEGEOS=0
      GOBSDEFINIT%NOREC=0
      CALL RXOB3I(CAMPGN ,TITLE  ,ISASYS ,RNXSTA ,IABUPD ,SESSION,
     1            ISINTV ,ISOFFS ,MINSIG ,IACPT0 ,ICSFLG ,
     2            NOL2C  ,STAINFO,FLGSTR, RNXINFO,ERROPT,USEGEOS,
     3            GOBSDEFINIT,ITYPE)
C
C READ OBSERVATION INPUT AND OUTPUT FILE NAMES
C --------------------------------------------
      CALL RXOB3F(ISASYS,ICSFLG,NFLINP,RXFILE,NFLOUT,FILCOD,
     1            FILPHA,SCROBS,WINDOW,IMEA  ,REQOBS)
C
C WRITE TITLE LINES FOR SUMMARY
C -----------------------------
      WRITE(lfnprt,'(//,2(A,/),/,2(A,/))')
     1     ' SUMMARY OF RINEX DATA TRANSFERRED TO BERNESE FILES:',
     2     ' --------------------------------------------------',
     3     ' Num  Rinex file name      Fr  Sa Epoch  Start time' //
     4     '           End time                  #C1    #C2    ' //
     5     ' #P1    #P2     #L1    #L2',
     6     ' --------------------------------------------------' //
     7     '---------------------------------------------------' //
     8     '-------------------------------'

C
      MINGAP=5
C
C LOOP OVER ALL INPUT FILES
C -------------------------
      IFLOUT=0
      IRCODE=0
      DO 2000 IFLINP=1,NFLINP
C
C
C INITIALIZE GOBSDEF STRUCTURE
C ----------------------------
        IF (USEGEOS==2) USEGEOS=0
        GOBSDEF%NOREC=0
c        IF (USEGEOS==1) THEN
c          CALL init_geos(GOBSDEFINIT%NOREC,GOBSDEF)
c          DO IG=1,GOBSDEFINIT%NOREC
c            GOBSDEF%SAT(IG)%SATNAME = GOBSDEFINIT%SAT(IG)%SATNAME
c            GOBSDEF%SAT(IG)%SATNUM  = GOBSDEFINIT%SAT(IG)%SATNUM
c            GOBSDEF%SAT(IG)%SYSCHAR = GOBSDEFINIT%SAT(IG)%SYSCHAR
c            GOBSDEF%SAT(IG)%SYSNUM  = GOBSDEFINIT%SAT(IG)%SYSNUM
c            DO JG=1,4
c             GOBSDEF%SAT(IG)%OBSTYP(JG) = GOBSDEFINIT%SAT(IG)%OBSTYP(JG)
c            ENDDO
c          ENDDO
c        ENDIF
c
        RNGOBS='N'
        AMBSAT=0
C
C OPEN RINEX FILE
        CALL OPNFIL(LFNOBS,RXFILE(IFLINP),'OLD','FORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNOBS,IOSTAT,RXFILE(IFLINP),'RXOBV3')
C
C GET NEW RINEX HEADER INFORMATION
C --------------------------------
5000    NUMLIN=0
        OBSTYP(:)='  '
        NUMSAT(:)=0
        NUMOBS(:,:)=0
        CALL R2RDOH(LFNOBS,LFNERR,MAXSAT,MAXCOM,NUMLIN,
     1              PRGNAM,RUNBY,CRDATX,CRTIMX,NCOM,COMENT,
     2              STANAM,STANUM,OPRNAM,AGENCY,
     3              IRUNIT,RECTYP,RCVERS,
     4              IANTEN,ANTTYP,POSXYZ,POSANT,
     5              IWLRNX,IWLSAT,NWLSAT,
     6              NUMTYP,OBSTYP,IDTRNX,TFIRST,TLAST(1),
     7              NSATEL,NUMSAT,NUMOBS,IRXVRS,IRCHED,USEGEOS=USEGEOS,
     8              GOBSDEF=GOBSDEF)
c
C
C IN CASE OF INPUT FROM SMT-FILE,TAKE GEOS-INFO FROM SMT HEADER
C -------------------------------------------------------------
        IF ((ITYPE==2).AND.(USEGEOS==0)) THEN
         norec = 0
         DO ICOM=1,NCOM
          IF (COMENT(ICOM)(1:4).EQ.'GEOS') THEN
           norec=norec+1
          ENDIF
         ENDDO
C
         IF (norec>0) THEN
           USEGEOS=2
           CALL init_geos(norec,gobsdef)
           norec=0
           DO ICOM=1,NCOM
            IF (COMENT(ICOM)(1:5).EQ.'GEOS:') THEN
             norec=norec+1
             READ(COMENT(ICOM),22) gobsdef%sat(norec)%syschar,
     1            gobsdef%sat(norec)%sysnum,gobsdef%sat(norec)%satnum,
     2            gobsdef%sat(norec)%obstyp(1),
     3            gobsdef%sat(norec)%obstyp(2),
     4            gobsdef%sat(norec)%obstyp(3),
     5            gobsdef%sat(norec)%obstyp(4)
22           FORMAT(9X,A1,I1,I2,1X,A3,1X,A3,1X,A3,1X,A3)
            ENDIF
           ENDDO
         ENDIF
        ENDIF
C
C CHECK FOR SLR/GNSS
C ------------------
        IF ( (IMEA.EQ.1.AND.RECTYP(1:20).EQ.'SLR') .OR.
     1       (IMEA.EQ.2.AND.RECTYP(1:20).NE.'SLR') ) THEN
          STRING=RXFILE(IFLINP)(1:32)
          CALL STRIPDIR(STRING)
          IF (IMEA.EQ.1.AND.RECTYP(1:20).EQ.'SLR') THEN
            WRITE(LFNPRT,'(I4,2X,A20,1X,A)')
     1         IFLINP,STRING(1:20),
     2         'File contains SLR data but GNSS expected'
          ELSE
            WRITE(LFNPRT,'(I4,2X,A20,1X,A)')
     1         IFLINP,STRING(1:20),
     2         'File contains GNSS data but SLR expected'
          ENDIF
          CLOSE(LFNOBS)
          CYCLE
        ENDIF
C
C CHECK DIMENSION OF NUMTYP
C -------------------------
        CALL DIMTST(1,2,2,'RXOBV3','MAXTYP',
     1       'NUMBER OF OBSERVATION TYPES',' ',NUMTYP,MAXTYP,IRC)


C
C CHECK TIME SYSTEM FOR OBSERVATION
C ---------------------------------
C TFIRST > 0 : GPS-TIME
C TFIRST < 0 : UTC-TIME
C
        IF(TFIRST.LT.0.D0)THEN
          TFIRST=-TFIRST
          TIMSYS='UTC'
          GPSUTC=DGPSUT(TFIRST)
          TFIRST=TFIRST+GPSUTC/86400.D0
          WRITE(LFNPRT,19) GPSUTC
19        FORMAT(/,' *** PG RXOBV3: OBSERVATION EPOCH TRANSFORMED',/,
     1                         16X,'FROM UTC TO GPS-TIME',/,
     2                         16X,'GPS-UTC (SEC) : ',F4.0,/)
        ELSE
          TIMSYS='GPS'
        ENDIF
C
        IF(IRCHED.EQ.9) GOTO 5010
C
C TAKE NEXT FILE
C --------------
        IF(IRCHED.NE.0) GOTO 5010
        NFREQ(1)=1
        NFREQ(2)=1
        DO ITYP=1,NUMTYP
          IF(OBSTYP(ITYP).EQ.'P2') NFREQ(1)=2
          IF(OBSTYP(ITYP).EQ.'C2') NFREQ(1)=2
          IF(OBSTYP(ITYP).EQ.'R2') NFREQ(1)=2
          IF(OBSTYP(ITYP).EQ.'L2') NFREQ(2)=2
        ENDDO
C Using satellite-specific obs. selection for GIOVE:
        IF ((USEGEOS==1).AND.(gobsdef%norec>0)) THEN
          IF (gobsdef%nfreqc==2) NFREQ(1)=2
          IF (gobsdef%nfreqp==2) NFREQ(2)=2
        ENDIF
C
        IFLOUT=IFLOUT+1
        IF(IFLOUT.GT.NFLOUT) THEN
          WRITE(LFNERR,21) NFLOUT,NFLOUT
21        FORMAT(/,' *** PG RXOBV3: NOT ENOUGH OUTPUT FILES DEFINED',/,
     1                         16X,'# OF OUTPUT FILES DEFINED:',I4,/,
     2                         16X,'ACTUAL # OF OUTPUT FILES >',I4,/)
          IRCODE=2
          GOTO 9999
        END IF
C
C INITIALIZE OUTPUT VARIABLES FOR CURRENT OUTPUT FILES
C ----------------------------------------------------
        CALL R3INIT(FILCOD(1,IFLINP),FILPHA(1,IFLINP),SCROBS,
     1              ICSFLG,TIMREF,TLAST,NEPFLG,IOK1,
     2              NSMEA,ANZCOD,ANZPHA,NUMMRK,PHA001,
     3              IOBPRT(1:4,IFLINP),ISAPRT(1:4,IFLINP),
     4              IAMPRT(IFLINP))
        NOBGAP=0
        TIMOLD=0.D0
        IEPBAD=0
        IDTSUB=999999
        IDELTT(1)=999999
        IDELTT(2)=999999
        NUMAMB=0
        INDLLI=0
        ERROPT%WRTOBS=.TRUE.
C
C INIT LIST OF SATELLITES W/O FREQ IN DEFREQ
C ------------------------------------------
        DO ISAT=1,MAXSAT
          LSTNOF(ISAT)=0
        ENDDO
C
C DEFINE SAMPLING RATE TO 1 SEC FOR RANGES
C ----------------------------------------
        DO ITYP=1,NUMTYP
          IF (OBSTYP(ITYP)(1:1).EQ.'R') THEN
            IDELTT(1)=1
            IDELTT(2)=1
            EXIT
          ENDIF
        ENDDO
C
C "EPOCH STATISTICS":
C   MEPOCH: NUMBER OF EPOCHS
C   TIMEPO(1): FIRST EPOCH FOUND IN THE DATA
C   TIMEPO(2): (ARITH.) MEAN EPOCH FOUND IN THE DATA
C   TIMEPO(3): LAST EPOCH FOUND IN THE DATA
C THE MEAN EPOCH CAN LATER BE COMPARED WITH THE MIDDLE EPOCH:
C   (TIMEPO(1)+TIMEPO(3))/2
C ---------------------------------------------------
        MEPOCH      = 0
        TIMEPO(1:3) = 0D0
C
C SATELLITE SYSTEM SELECTION
C --------------------------
        IF (ISASYS.EQ.1) THEN
          IRXSEL=MOD(IRXVRS,100)
        ELSEIF (ISASYS.EQ.2) THEN
          IRXSEL=MOD(IRXVRS,100)+100
        ELSEIF (ISASYS.EQ.3) THEN
          IRXSEL=MOD(IRXVRS,100)+200
        ELSEIF (ISASYS.EQ.4) THEN
          IRXSEL=MOD(IRXVRS,100)+600
        ELSEIF (ISASYS.EQ.5) THEN
          IRXSEL=MOD(IRXVRS,100)+700
        ELSEIF (ISASYS.EQ.6) THEN
          IRXSEL=MOD(IRXVRS,100)+800
        ELSE
          IRXSEL=IRXVRS
        ENDIF
c       ADD_GNSS_HERE
C
        IF (IRXSEL.NE.IRXVRS .AND.
     1       (IRXVRS/100 .GT. 5 .OR. IRXSEL.LT.500)) THEN
          WRITE(LFNERR,1994) RXFILE(IFLINP)
1994      FORMAT(/,' ### PG RXOBV3: OBSERVATION DATA FROM OTHER ',
     1                             'SATELLITE SYSTEM REJECTED',
     2                       /,16X,'RINEX FILE NAME: ',A,/)
        ENDIF
        IRXVRS=IRXSEL
C
C PROCESS ALL OBSERVATION FOR THE CURRENT OUTPUT FILE
C ---------------------------------------------------
        DO 1000 IREC=1,MAXREC
          SAVEPO      = .TRUE.
          OBSEPO(:,:) = 0.D0
          ISIGN(:,:)  = 0
          LLI(:,:)    = 0
          SATEP(:)    = 0
C
C GET NEXT RINEX RECORD
          CALL R2RDOR(LFNOBS,LFNERR,MAXSAT,IRXVRS,
     1                -NUMTYP,OBSTYP,RXEPOCH,IFLGEP,NSATEP,SATEP,
     2                OBSEPO,ISIGN,LLI,IRCOBS,USEGEOS=USEGEOS,
     3                GOBSDEF=GOBSDEF)
          IF(IRCOBS.EQ.8) GOTO 5010
          IF(IRCOBS.EQ.3.OR.IRCOBS.EQ.4.OR.
     1       IRCOBS.EQ.9) GOTO 1010
          IF(IRCOBS.EQ.5) THEN
            IFLGEP=4
            NSATEP=0
          ENDIF

          EPOCH  = RXEPOCH(1)
          DELTT2 = RXEPOCH(2)
C
C CORRECT OBSERVATION EPOCH TO GPS TIME
          IF (TIMSYS.EQ.'UTC') EPOCH=EPOCH+GPSUTC/86400.D0
C
C EVENT FLAG 4
C ------------
          IF(IFLGEP.EQ.4) THEN
C
C READ INSERTED HEADER RECORDS (INITIALIZE ALL VARIABLES)
            NUMLIN=-NSATEP
            OBSTY4(:)   = '  '
            NUMSA4(:)   = 0
            NUMOB4(:,:) = 0
C
            CALL R2RDOH(LFNOBS,LFNERR,MAXSAT,MAXCOM,NUMLIN,
     1              PRGNA4,RUNBY4,CRDAT4,CRTIM4,NCOM4,COMEN4,
     2              STANA4,STANU4,OPRNA4,AGENC4,
     3              IRUNI4,RECTY4,RCVER4,
     4              IANTE4,ANTTY4,POSXY4,POSAN4,
     5              IWLRN4,IWLSA4,NWLSA4,
     6              NUMTY4,OBSTY4,IDTRN4,TFIRS4,TLAST4,
     7              NSATE4,NUMSA4,NUMOB4,IRXVR4,IRCHE4,USEGEOS=USEGEOS,
     8              GOBSDEF=GOBSDEF)
            IF(IRCHE4.EQ.9) GOTO 5010
            IF(IRCHE4.NE.0) GOTO 5010
C
            IF(STANA4.NE.' '.AND.STANA4.NE.STANAM) THEN
              WRITE(LFNERR,1996) STANA4,RXFILE(IFLINP)
1996          FORMAT(/,' *** PG RXOBV3: NEW SITE FOUND IN THE FILE:',
     1                    A20,/,16X,'PROCESSING STOPPED FOR FILE!',
     3                         /,16X,'RINEX FILE NAME: ',A,/)
              GOTO 1010
            END IF
C
C  NEW WAVELENGTH FACTORS
            IF(IWLRN4(1).EQ.2) IWLRNX(1)=2
            IF(IWLRN4(2).EQ.2) IWLRNX(2)=2
C
C  SATELLITE-DEPENDENT
            DO 510 ISA4=1,NWLSA4
              DO 520 ISAT=1,NWLSAT
                IF(IWLSAT(3,ISAT).EQ.IWLSA4(3,ISA4)) THEN
                  IF(IWLSA4(1,ISA4).EQ.2) IWLSAT(1,ISAT)=2
                  IF(IWLSA4(2,ISA4).EQ.2) IWLSAT(2,ISAT)=2
                  GOTO 510
                END IF
520           CONTINUE
C
C  UPDATE LIST
              NWLSAT=NWLSAT+1
              IWLSAT(3,NWLSAT)=IWLSA4(3,ISA4)
              IF(IWLSA4(1,ISA4).NE.0) THEN
                IWLSAT(1,NWLSAT)=IWLSA4(1,ISA4)
              ELSE
                IWLSAT(1,NWLSAT)=IWLRNX(1)
              END IF
              IF(IWLSA4(2,ISA4).NE.0) THEN
                IWLSAT(2,NWLSAT)=IWLSA4(2,ISA4)
              ELSE
                IWLSAT(2,NWLSAT)=IWLRNX(2)
              END IF
510         CONTINUE
C
C  NEW OBSERVATION TYPES
            IF(NUMTY4.NE.0) THEN
              NUMTYP=NUMTY4
              OBSTYP=OBSTY4
              DO 530 ITYP=1,NUMTYP
                IF(OBSTYP(ITYP).EQ.'P2') NFREQ(1)=2
                IF(OBSTYP(ITYP).EQ.'C2') NFREQ(1)=2
                IF(OBSTYP(ITYP).EQ.'L2') NFREQ(2)=2
                IF(OBSTYP(ITYP).EQ.'R2') NFREQ(2)=2
530           CONTINUE
            END IF
C Using satellite-specific obs. selection for GIOVE:
            IF ((USEGEOS==1).AND.(gobsdef%norec>0)) THEN
             IF (gobsdef%nfreqc==2) NFREQ(1)=2
             IF (gobsdef%nfreqp==2) NFREQ(2)=2
            ENDIF
C
            GOTO 1000
          END IF
C
C OTHER EVENT FLAGS
          IF(IFLGEP.GT.1) THEN
            IF (ERROPT%EPOFLAG.EQ.1) THEN
              WRITE(LFNERR,'(/A,I3,2(/,16X,A),/)')
     1          ' ### PG RXOBV3: EVENT FLAG NOT HANDLED:',IFLGEP,
     2                      'PROCESSING CONTINUED IGNORING THIS FLAG!',
     3                      'RINEX FILE NAME: ' // TRIM(RXFILE(IFLINP))
              DO ILINE = 1,NSATEP
                READ(LFNOBS,'(A)') STRING
              ENDDO
              GOTO 1000
C
            ELSE IF (ERROPT%EPOFLAG.EQ.2.OR.
     1               ERROPT%EPOFLAG.EQ.3) THEN
              WRITE(LFNERR,'(/A,I3,2(/,16X,A),/)')
     1          ' ### PG RXOBV3: EVENT FLAG NOT HANDLED:',IFLGEP,
     2                      'PROCESSING STOPPED FOR THIS FILE!',
     3                      'RINEX FILE NAME: ' // TRIM(RXFILE(IFLINP))
              IF(FILCOD(1,IFLINP).NE.' ') CLOSE(UNIT=LFN001)
              IF(FILPHA(1,IFLINP).NE.' ') CLOSE(UNIT=LFN002)
              FILCOD(1,IFLINP)=' '
              FILPHA(1,IFLINP)=' '
C
              ERROPT%STPERR = ERROPT%STPERR .OR. (ERROPT%EPOFLAG.EQ.3)

              GOTO 5010
            ENDIF

cc            WRITE(LFNERR,1998) IFLGEP,RXFILE(IFLINP)
cc1998        FORMAT(/,' *** PG RXOBV3: EPOCH FLAG NOT YET HANDLED:',I3,
cc     1                         /,16X,'PROCESSING STOPPED FOR FILE!',
cc     3                         /,16X,'RINEX FILE NAME: ',A,/)
cc            GOTO 1010
          END IF
C
C USE THE TIME WINDOW ONLY, IF IT IS A REAL MJD
C (NO OPEN WINDOW USED)
C ----------------------------------------------
          MEPOCH=MEPOCH+1
          TIMEPO(2)=TIMEPO(2)+(EPOCH-TFIRST)
          IF (TIMEPO(1).EQ.0D0 .OR. TIMEPO(1).GT.EPOCH)
     1        TIMEPO(1)=EPOCH
          IF (TIMEPO(3).EQ.0D0 .OR. TIMEPO(3).LT.EPOCH)
     1        TIMEPO(3)=EPOCH
C
          IF ((WINDOW(1,IFLINP).NE. 0D0 .AND.
     1         EPOCH.LT.WINDOW(1,IFLINP) )  .OR.
     2        (WINDOW(2,IFLINP).NE.1D20 .AND.
     3         EPOCH.GT.WINDOW(2,IFLINP) ))  SAVEPO = .FALSE.
C
C CHECK EPOCH ORDER
          IF (EPOCH.LE.TIMOLD) THEN
            IEPBAD=IEPBAD+1
            IF (IEPBAD.EQ.1) EPOBAD(1)=EPOCH
            EPOBAD(2)=EPOCH
            GOTO 1000
          ELSE IF (IEPBAD.GT.0) THEN
            CALL TIMST2(1,2,EPOBAD,TSTRNG)
            WRITE(LFNERR,1997) IEPBAD,TSTRNG,RXFILE(IFLINP)
1997        FORMAT(/,' ### PG RXOBV3: OBS.EPOCHS NOT ORDERED IN TIME',
     1                         /,16X,'NUMBER OF EPOCHS REJECTED:',I5,
     2                         /,16X,'FROM - TO : ',A40,
     3                         /,16X,'RINEX FILE: ',A,/)
            IEPBAD=0
            GOTO 1000
          ENDIF
C
C  EPOCH CONSISTENT WITH SAMPLING RATE/OFFSET?
          CALL MJDGPS(EPOCH,SECOND,NWEEK)
          IF(ISINTV.NE.0) THEN
            TEST=DABS(DMOD(SECOND-ISOFFS,DBLE(ISINTV)))
            IF(TEST.GT.DTEST.AND.DBLE(ISINTV)-TEST.GT.DTEST)
     1        SAVEPO=.FALSE.
          END IF
C
C FIND CORRECT OBSERVATION INTERVAL
          IF (TIMOLD.NE.0.D0 .AND. SAVEPO) THEN
            IDTNEW=IDNINT((EPOCH-TIMOLD)*86400.D0)
            IF (IDTNEW.LT.IDELTT(1)) IDELTT(1)=IDTNEW
            IF (IDTNEW.LT.IDELTT(2)) IDELTT(2)=IDTNEW
            IDTSBN=IDNINT((EPOCH-TIMOLD)*864000.D0)
            IF (IDTSBN.LT.IDTSUB) IDTSUB=IDTSBN
          ENDIF
C
C UPDATE TIME GAP STATISTICS
C (LIMIT IS 5MIN OR TWICE THE SAMPLING RATE GIVEN BY THE USER)
          IF (SAVEPO) THEN
            IF (EPOCH.LT.TIMOLD+MINGAP/1440.D0 .OR.
     1          EPOCH-TIMOLD.LT.2D0*DBLE(ISINTV)/86400D0) THEN
              NEPGAP(NOBGAP)=NEPGAP(NOBGAP)+1
              TIMGAP(2,NOBGAP)=EPOCH
            ELSE
              NOBGAP=NOBGAP+1
              IF (NOBGAP.GT.MAXGAP) THEN
                WRITE(LFNERR,1999) NOBGAP,MAXGAP,RXFILE(IFLINP)
1999            FORMAT(/,' *** PG RXOBV3: TOO MANY GAPS IN THE DATA',/,
     1                    16X,'NUMBER OF SEPARATE INTERVALS >=',I5,/,
     2                    16X,'MAX. NUMBER OF SEP.INTERVALS  :',I5,/,
     3                    16X,'RINEX FILE NAME: ',A,/)
                CALL EXITRC(2)
              ENDIF
              NEPGAP(NOBGAP)=1
              NRXOBS(NOBGAP,1:6)=0
              TIMGAP(1,NOBGAP)=EPOCH
              TIMGAP(2,NOBGAP)=EPOCH
            ENDIF
            TIMOLD=EPOCH
          ENDIF
C
C  COPY EPOCH FLAG
          IF(IFLGEP.EQ.0) THEN
            EPOFLG(1)=CHAR(0)
            EPOFLG(2)=CHAR(0)
          ELSE
            EPOFLG(1)=CHAR(1)
            EPOFLG(2)=CHAR(1)
          ENDIF
C
C  CHECK IF SATELLITES ARE SET ACTIVE IN SATELLITE FILE AND SET
C  THE CORRESPONDING FREQUENCIES (CF.'I:COMFREQ')
C  ------------------------------------------------------------
C
          EPOFRQ(1)=EPOCH
          EPOFRQ(2)=EPOCH
          OBST='U'
          IF (usegeos==2) THEN
            CALL DEFREQ(EPOFRQ,NSATEP,SATEP,NOFRQ,SATNOF,
     1                USEGEOS=1,GOBSDEF=GOBSDEF,MEATYPC=OBST)
          ELSE
            CALL DEFREQ(EPOFRQ,NSATEP,SATEP,NOFRQ,SATNOF,
     1                USEGEOS=USEGEOS,GOBSDEF=GOBSDEF,MEATYPC=OBST)
          ENDIF
C
C  COPY OBSERVATIONS TO BERNESE FORMAT, PUT S/N INTO OBSERV.FLAGS
C  --------------------------------------------------------------
          DO 30 ISATEP=1,NSATEP
            DO 40 IFREQ=1,2
              CODL12(ISATEP,IFREQ)=0.D0
              CODFLG(ISATEP,IFREQ)=CHAR(0)
              PHAL12(ISATEP,IFREQ)=0.D0
              PHAFLG(ISATEP,IFREQ)=CHAR(0)
40          CONTINUE
C
C HAS THIS SATELLITE A FREQUENCY?
C -------------------------------
            DO 45 ISAT=1,NOFRQ
              IF (SATEP(ISATEP).EQ.SATNOF(ISAT)) THEN
                DO JSAT=1,MAXSAT
                  IF (LSTNOF(JSAT).EQ.SATEP(ISATEP)) THEN
                    TIMNOF(2,JSAT)=EPOCH
                    GOTO 30
                  ENDIF
                  IF (LSTNOF(JSAT).EQ.0) THEN
                    LSTNOF(JSAT)=SATEP(ISATEP)
                    TIMNOF(1,JSAT)=EPOCH
                    TIMNOF(2,JSAT)=EPOCH
                    GOTO 30
                  ENDIF
                ENDDO
              ENDIF
45          CONTINUE

c  Different loops for "normal" satellites and those with individual signal sel.
            usealternative=0
            indgeos=0
            IF ((USEGEOS==1).AND.(gobsdef%norec>0)) THEN
             DO igeos=1,gobsdef%norec
              IF (ISATEP==gobsdef%sat(igeos)%eposatind) THEN
                 usealternative=1
                 gobsdef%sat(igeos)%eposatind=0
                 indgeos=igeos
                 EXIT
              ENDIF
             ENDDO
            ENDIF
C
C  WHICH FREQUENCY?
            IF (usealternative==0) THEN
             DO 50 ITYP=1,NUMTYP
C
C  COUNT NUMBER OF OBSERVATION PER OBSTYP
              IF (SAVEPO.AND.OBSEPO(ISATEP,ITYP).NE.0.D0) THEN
                IF (OBSTYP(ITYP).EQ.'C1') THEN
                  NRXOBS(NOBGAP,1)=NRXOBS(NOBGAP,1)+1
                ELSEIF (OBSTYP(ITYP).EQ.'C2') THEN
                  NRXOBS(NOBGAP,2)=NRXOBS(NOBGAP,2)+1
                  IF (NOL2C.EQ.1) GOTO 50
                ELSEIF (OBSTYP(ITYP).EQ.'P1') THEN
                  NRXOBS(NOBGAP,3)=NRXOBS(NOBGAP,3)+1
                ELSEIF (OBSTYP(ITYP).EQ.'P2') THEN
                  NRXOBS(NOBGAP,4)=NRXOBS(NOBGAP,4)+1
                ELSEIF (OBSTYP(ITYP).EQ.'L1') THEN
                  NRXOBS(NOBGAP,5)=NRXOBS(NOBGAP,5)+1
                ELSEIF (OBSTYP(ITYP).EQ.'L2') THEN
                  NRXOBS(NOBGAP,6)=NRXOBS(NOBGAP,6)+1
                ENDIF
              ENDIF
C
C  GET THE FREQUENCY INDEX
              IF(OBSTYP(ITYP)(2:2).EQ.'1') THEN
                IFREQ=1
              ELSEIF(OBSTYP(ITYP)(2:2).EQ.'2') THEN
                IFREQ=2
              ELSE
                GOTO 50
              END IF
              ISIGNL=ISIGN(ISATEP,ITYP)
C
C  CODE
              IF((OBSTYP(ITYP)(1:1).EQ.'C'.AND.
     1            CODL12(ISATEP,IFREQ).EQ.0.D0)
     2                       .OR.
     3           (OBSTYP(ITYP)(1:1).EQ.'P'.AND.
     4            OBSEPO(ISATEP,ITYP).NE.0.D0)) THEN
C
C MINIMUM SIGNAL STRENGTH
                IF((ISIGNL.EQ.0.AND.IACPT0.EQ.1).OR.
     1             (ISIGNL.GE.MINSIG)) THEN
C
C THE CASE WHICH APPEARS IN RINEX FILE FROM MATERA DAY 92/214
                  IF (OBSEPO(ISATEP,ITYP).NE.-9999.D0) THEN
                    CODL12(ISATEP,IFREQ)= OBSEPO(ISATEP,ITYP)
                    CODFLG(ISATEP,IFREQ)= CHAR(ISIGNL*16)
                  END IF
                END IF
C
C  PHASE
              ELSEIF(OBSTYP(ITYP)(1:1).EQ.'L') THEN
C
C  CHANGE OF WAVELENGTH FACTOR?
C  IF SWITCH TO 2 AND DEFAULT=1: SET DEFAULT TO 2 (FOR L2 ONLY)
                IF(TSTFLG(CHAR(LLI(ISATEP,ITYP)),1).AND.IFREQ.EQ.2) THEN
                  DO 60 KSAT=1,NWLSAT
                    IF(SATEP(ISATEP).EQ.IWLSAT(3,KSAT)) THEN
                      IF(IWLSAT(IFREQ,KSAT).EQ.1) IWLSAT(IFREQ,KSAT)=2
                      GOTO 65
                    END IF
60                CONTINUE
                  IF(IWLRNX(IFREQ).EQ.1) THEN
                    NWLSAT=NWLSAT+1
                    IWLSAT(3,NWLSAT)=SATEP(ISATEP)
                    IWLSAT(1,NWLSAT)=IWLRNX(1)
                    IF(NFREQ(2).EQ.2) IWLSAT(2,NWLSAT)=IWLRNX(2)
                    IWLSAT(IFREQ,NWLSAT)=2
                  END IF
                END IF
C
C MINIMUM SIGNAL STRENGTH
65              IF((ISIGNL.EQ.0.AND.IACPT0.EQ.1).OR.
     1             (ISIGNL.GE.MINSIG)) THEN
                  PHAL12(ISATEP,IFREQ)=-OBSEPO(ISATEP,ITYP)
     1                                    *WLGT(IFREQ,SATEP(ISATEP))
                  PHAFLG(ISATEP,IFREQ)= CHAR(ISIGNL*16)
                  IF (LLI(ISATEP,ITYP).EQ.1) THEN
                    CALL SETFLG(PHAFLG(ISATEP,IFREQ),1)
                    INDLLI=1
                  END IF
                END IF
C
C  RANGE
              ELSEIF(OBSTYP(ITYP)(1:1).EQ.'R') THEN
                CODL12(ISATEP,IFREQ)= OBSEPO(ISATEP,ITYP)
                CODFLG(ISATEP,IFREQ)= CHAR(ISIGNL*16)
                RNGOBS='Y'
              ELSE
                GOTO 50
              END IF
50           CONTINUE
            END IF
c
c  Alternative: satellite-specific signal selection

            IF ((usealternative==1).AND.(indgeos.NE.0)) THEN
             DO 51 ITYP=1,4
C
C  COUNT NUMBER OF OBSERVATIONS PER OBSTYP
              IF (SAVEPO.AND.OBSEPO(ISATEP,ITYP).NE.0.D0) THEN
                IF (gobsdef%sat(indgeos)%obstyp2(ITYP).EQ.'P1') THEN
                  NRXOBS(NOBGAP,3)=NRXOBS(NOBGAP,3)+1
                ELSEIF (gobsdef%sat(indgeos)%obstyp2(ITYP).EQ.'P2') THEN
                  NRXOBS(NOBGAP,4)=NRXOBS(NOBGAP,4)+1
                ELSEIF (gobsdef%sat(indgeos)%obstyp2(ITYP).EQ.'L1') THEN
                  NRXOBS(NOBGAP,5)=NRXOBS(NOBGAP,5)+1
                ELSEIF (gobsdef%sat(indgeos)%obstyp2(ITYP).EQ.'L2') THEN
                  NRXOBS(NOBGAP,6)=NRXOBS(NOBGAP,6)+1
                ENDIF
              ENDIF
C
C  GET THE FREQUENCY INDEX
              IF (gobsdef%sat(indgeos)%obstyp2(ITYP)(2:2).EQ.'1') THEN
                IFREQ=1
              ELSEIF (gobsdef%sat(indgeos)%obstyp2(ITYP)(2:2).EQ.'2')
     1          THEN
                IFREQ=2
              ELSE
                GOTO 51
              END IF
              ISIGNL=ISIGN(ISATEP,ITYP)
C
C  CODE
              IF(gobsdef%sat(indgeos)%obstyp2(ITYP)(1:1).EQ.'P'.AND.
     1            OBSEPO(ISATEP,ITYP).NE.0.D0) THEN
C
C MINIMUM SIGNAL STRENGTH
                IF((ISIGNL.EQ.0.AND.IACPT0.EQ.1).OR.
     1             (ISIGNL.GE.MINSIG)) THEN
C
C THE CASE WHICH APPEARS IN RINEX FILE FROM MATERA DAY 92/214
                  IF (OBSEPO(ISATEP,ITYP).NE.-9999.D0) THEN
                    CODL12(ISATEP,IFREQ)= OBSEPO(ISATEP,ITYP)
                    CODFLG(ISATEP,IFREQ)= CHAR(ISIGNL*16)
                  END IF
                END IF
C
C  PHASE
              ELSEIF(gobsdef%sat(indgeos)%obstyp2(ITYP)(1:1).EQ.'L')
     1          THEN
C
C  CHANGE OF WAVELENGTH FACTOR?
C  IF SWITCH TO 2 AND DEFAULT=1: SET DEFAULT TO 2 (FOR L2 ONLY)
                IF(TSTFLG(CHAR(LLI(ISATEP,ITYP)),1).AND.IFREQ.EQ.2) THEN
                  DO 61 KSAT=1,NWLSAT
                    IF(SATEP(ISATEP).EQ.IWLSAT(3,KSAT)) THEN
                      IF(IWLSAT(IFREQ,KSAT).EQ.1) IWLSAT(IFREQ,KSAT)=2
                      GOTO 66
                    END IF
61                CONTINUE
                  IF(IWLRNX(IFREQ).EQ.1) THEN
                    NWLSAT=NWLSAT+1
                    IWLSAT(3,NWLSAT)=SATEP(ISATEP)
                    IWLSAT(1,NWLSAT)=IWLRNX(1)
                    IF(NFREQ(2).EQ.2) IWLSAT(2,NWLSAT)=IWLRNX(2)
                    IWLSAT(IFREQ,NWLSAT)=2
                  END IF
                END IF
C
C MINIMUM SIGNAL STRENGTH
66              IF((ISIGNL.EQ.0.AND.IACPT0.EQ.1).OR.
     1             (ISIGNL.GE.MINSIG)) THEN
                  PHAL12(ISATEP,IFREQ)=-OBSEPO(ISATEP,ITYP)
     1                                    *WLGT(IFREQ,SATEP(ISATEP))
                  PHAFLG(ISATEP,IFREQ)= CHAR(ISIGNL*16)
                  IF (LLI(ISATEP,ITYP).EQ.1) THEN
                    CALL SETFLG(PHAFLG(ISATEP,IFREQ),1)
                    INDLLI=1
                  END IF
                END IF
              ELSE
                GOTO 51
              END IF
51           CONTINUE
            END IF
C
C LOSS OF LOCK ==> NEW AMBIGUITY (check maxamb)
C (ABUSE "AMBIGU" TO STORE EPOCH)
C -------------------------------
            IF (INDLLI.NE.0 .AND. ICSFLG.EQ.1) THEN
              NUMAMB=NUMAMB+1
              IF (NUMAMB.LE.MAXAMB) THEN
                AMBSAT(NUMAMB)=SATEP(ISATEP)
                AMBIEP(NUMAMB)=IOK1(2)+1
                AMBIGU(NUMAMB,1)=EPOCH
              ENDIF
              INDLLI=0
            ENDIF
C
C  ELIMINATE P2 OR L2 IF 1ST AND 2ND FREQ OBS ARE IDENTICAL
C
CC            IF(DABS(CODL12(ISATEP,2)-CODL12(ISATEP,1)).LT.0.001D0) THEN
CC              CODL12(ISATEP,2)=0.D0
CC            END IF
            IF(DABS(PHAL12(ISATEP,2)-PHAL12(ISATEP,1)).LT.0.001D0) THEN
              PHAL12(ISATEP,2)=0.D0
            END IF
C
30        CONTINUE

C
C SAVE MEASUREMENTS, UPDATE HEADER INFORMATION
C --------------------------------------------
          IF (SAVEPO)
     .      CALL SAVMEA(RXFILE(IFLINP),FILCOD(1,IFLINP),
     1                FILPHA(1,IFLINP),IFRMAT,NFREQ,EPOCH,DELTT2,EPOFLG,
     2                IOFSET,NSATEP,SATEP,TIMREF,TLAST,IOK1,NEPFLG,
     3                NSMEA,SVNMEA,ANZCOD,ANZPHA,CODL12,PHAL12,
     4                CODFLG,PHAFLG,PHA001,RNGOBS)
1000    CONTINUE
C
C CLOSE OBS. OUTPUT FILES
C -----------------------
1010    IF(FILCOD(1,IFLINP).NE.' ') CLOSE(UNIT=LFN001)
        IF(FILPHA(1,IFLINP).NE.' ') CLOSE(UNIT=LFN002)
C
C No epochs found: skip
C ---------------------
        IF (MEPOCH.EQ.0) THEN
          FILCOD(1,IFLINP) = ' '
          FILPHA(1,IFLINP) = ' '
          CLOSE(UNIT=LFNOBS)
          GOTO 2000
        ENDIF
C
C SET MEATYP, MINEPO
C ------------------
        IF(RNGOBS.NE.'Y') THEN
          MEATYP=2
          MINEPO=2
        ELSE
          MEATYP=3
          MINEPO=1
        END IF
C
C GET SESSION AUTOMATICALLY FROM FOR THE RINEX DATA
C -------------------------------------------------
        TIMEPO(2)=TIMEPO(2)/MEPOCH+TFIRST
C
        IF (LEN_TRIM(SESSION) == 0) THEN
          CALL RNXSES(RXFILE(IFLINP), TIMEPO, IUSWIN,
     1                WINDOW(1,IFLINP), SESSION)
        ENDIF
C
C WRITE MESSAGE FOR BAD EPOCHS
C ----------------------------
        IF (IEPBAD.GT.0) THEN
          CALL TIMST2(1,2,EPOBAD,TSTRNG)
          WRITE(LFNERR,1997) IEPBAD,TSTRNG,RXFILE(IFLINP)
          IEPBAD=0
        ENDIF
C
C REPORT SATELLITES REMOVED BECAUSE NO FREQUENCY AVAILABLE IN DEFREQ
C ------------------------------------------------------------------
        DO ISAT=1,MAXSAT
          IF (LSTNOF(ISAT).EQ.0) EXIT
          CALL TIMST2(1,2,TIMNOF(1:2,ISAT),EPONOF)
          WRITE(LFNERR,1993) LSTNOF(ISAT),EPONOF,TRIM(RXFILE(IFLINP))
1993      FORMAT(/,' ### SR RXOBV3: OBSERVATIONS REMOVED BECAUSE NOT ',
     1                             'BOTH FREQUENCIES ARE DEFINED FOR',/,
     2                         16X,'SATELLITE NUMBER : ',I3,/,
     3                         16X,'EPOCHS           : ',A,/,
     4                         16X,'RINEX FILE       : ',A,/)
        ENDDO
C
C SAVE INPUT FILE INDEX AND "IOK1" FOR PRINTING
C ---------------------------------------------
        INDINP(IFLOUT)=IFLINP
        IOKPRT(1,IFLOUT)=IOK1(1)
        IOKPRT(2,IFLOUT)=IOK1(2)
C
C CHECK MINIMUM NUMBERS OF EPOCHS
C -------------------------------
        CALL FEWOBS(IOK1,0,REQOBS,NSMEA,SVNMEA,NFREQ(1),IOK1(1),ANZCOD,
     1              NFREQ(2),IOK1(2),ANZPHA,RXFILE(IFLINP),
     2              FILCOD(1:2,IFLINP),FILPHA(1:2,IFLINP),IFRMAT,SCROBS)
C
C NO DATA FOUND IN THE TIME WINDOW
C --------------------------------
        IF (IDTSUB.EQ.999999 .AND. REQOBS%REQEPO.NE.0) THEN
          WRITE(LFNERR,'(/,A,2(/,16X,A),/)')
     1    ' ### PG RXOBV3: THERE ARE NO DATA WITHIN THE TIME WINDOW ' //
     2                                              'IN THE RINEX FILE',
     3                    'RINEX FILE NAME:  ' // TRIM(RXFILE(IFLINP)),
     4                    'NO BERNESE OBS. FILES WILL BE WRITTEN'

          FILCOD(1:2,IFLINP) = ' '
          FILPHA(1:2,IFLINP) = ' '
C
C CHECK SAMPLING IN FILE WITH RESPECT TO INPUT
C --------------------------------------------
        ELSE IF (ISINTV.NE.0.AND.
     1           ABS(ISINTV*10-IDTSUB).GT.INT(DTEST*10)) THEN
          WRITE(LFNERR,'(/,A,/,16X,A,16X,A,2(/,16X,A,I6,A),/,16X,A,/)')
     1    ' ### PG RXOBV3: DATA RATE PROBLEM IN THE RINEX FILE',
     2    'RINEX FILE NAME:  ',                TRIM(RXFILE(IFLINP)),
     3    '(NOMINAL) DATA RATE IN RINEX:     ',IDTRNX,' SECONDS',
     4    'USER SPECIFIED SAMPLING INTERVAL: ',ISINTV,' SECONDS',
     5    'CHECK CONTENTS OF BERNESE OBS. FILES'

!          FILCOD(1:2,IFLINP) = ' '
!          FILPHA(1:2,IFLINP) = ' '
          IDELTT(:) = ISINTV
C
C THEORETICAL NUMBER OF EPOCHS (CHECK IF DATA RATE BELOW 1 SEC)
C -------------------------------------------------------------
        ELSE IF (IDTSUB.LT.10) THEN
          WRITE(LFNERR,1995) IDTSUB/10.D0,RXFILE(IFLINP)
1995      FORMAT(/,' ### PG RXOBV3: SAMPLING RATE PROBABLY BELOW 1 SEC',
     1                       /,16X,'SAMPLING RATE (SEC):',F4.1,
     2                       /,16X,'RINEX FILE NAME    : ',A,
     3                       /,16X,'BERNESE IS NOT SUPPORTING THIS.',
     4                       /,16X,'USE THE SAMPLING OPTION TO REDUCE',
     5                       /,16X,'THE DATA',/)
          IDELTT(1)=1
          IDELTT(2)=1
        ENDIF
        DO 4030 K=1,2
          IF (IOK1(K).LT.2) THEN
            NEPOCH(K)=IOK1(K)
            IDELTT(k)=1
          ELSE
            NEPOCH(K)=IDNINT(86400.D0*(TLAST(K)-TIMREF(K))/IDELTT(K))+1
          ENDIF
4030    CONTINUE
C
C USE THE NEW TABLES
C ------------------
        TIMINT%T(1) = TIMEPO(1)
        TIMINT%T(2) = TIMEPO(3)
C
C STATION NAME: CHANGE, CHECK, VERIFY
C -----------------------------------
        IF (LEN_TRIM(FILCOD(1,IFLINP)) +
     1      LEN_TRIM(FILPHA(1,IFLINP)) > 0) THEN
          CALL RXOSTA(RNXSTA,ERROPT,STAINFO,FLGSTR(1),TIMINT,
     1                RXFILE(IFLINP),STANAM,STANUM,STNAME(IFLOUT),
     2                LINE(IFLOUT,1),IRC)
          WRITE(LINE(IFLOUT,1)(1:3),'(I3)') IFLOUT
C
C ERROR HANDLING
          IF (IRC.GE.2) THEN
            FILCOD(:,IFLINP) = ' '
            FILPHA(:,IFLINP) = ' '
          ENDIF
C
          ERROPT%STPERR = ERROPT%STPERR .OR. (IRC.EQ.3)
C
        ENDIF
C
C SKIP STATIONS WITH PROBLEMS IN STATION INFO FILE
C ------------------------------------------------
        IF (LEN_TRIM(FILCOD(1,IFLINP)) +
     1      LEN_TRIM(FILPHA(1,IFLINP)) > 0) THEN
          CALL RXOCRX(STAINFO,FLGSTR,TIMINT,RXFILE(IFLINP),
     1                STNAME(IFLOUT),IRC)
C
C ERROR HANDLING
          IF (IRC.EQ.2) THEN
            FILCOD(:,IFLINP) = ' '
            FILPHA(:,IFLINP) = ' '
          ENDIF
C
        ENDIF
C
C STATION NAME: CHANGE, CHECK, VERIFY
C -----------------------------------
        IF (LEN_TRIM(FILCOD(1,IFLINP)) +
     1      LEN_TRIM(FILPHA(1,IFLINP)) > 0) THEN
C
          CALL RXOANT(ERROPT, RNXINFO,STAINFO,FLGSTR(2),TIMINT,
     1                RXFILE(IFLINP), STNAME(IFLOUT), RECTYP,
     2                ANTTYP, POSANT, MEATYP, RECTNW,
     3                ANTTNW, IRECNW, IANTNW, ANTNEW, LINE(IFLOUT,2),
     4                IRC)
C
C COPY NEW RECEIVER/ANTENNA NUMBERS BACK
          IRUNIT = IRECNW
          IANTEN = IANTNW
C
C ERROR HANDLING
          IF (IRC.GE.2) THEN
            FILCOD(:,IFLINP) = ' '
            FILPHA(:,IFLINP) = ' '
          ENDIF
C
          ERROPT%STPERR = ERROPT%STPERR .OR. (IRC.EQ.3)
C
        ENDIF
C
C MARKER TYPE: CHECK
C ------------------
        IF (LEN_TRIM(FILCOD(1,IFLINP)) +
     1      LEN_TRIM(FILPHA(1,IFLINP)) > 0) THEN
C
          CALL RXOTYP(ERROPT,FLGSTR(5),TIMINT, RXFILE(IFLINP),
     1                STNAME(IFLOUT), STANUM, LINE(IFLOUT,3),
     2                MRKTEX,IRC)
          IF (MRKTEX == 1) MRKTYP_EX = .TRUE.
C ERROR HANDLING
          IF (IRC.GE.2) THEN
            FILCOD(:,IFLINP) = ' '
            FILPHA(:,IFLINP) = ' '
          ENDIF
C
          ERROPT%STPERR = ERROPT%STPERR .OR. (IRC.EQ.3)
C
        ENDIF
C
C HORIZONTAL ANTENNA OFFSETS
C --------------------------
        IF (LEN_TRIM(FILCOD(1,IFLINP)) +
     1      LEN_TRIM(FILPHA(1,IFLINP)) > 0 .AND.
     2      MEATYP .NE. 3                       ) THEN
          IF(ANTNEW(1,1).NE.0D0.OR.ANTNEW(2,1).NE.0D0)THEN
            IF (LEN_TRIM(FILCOD(1,IFLINP)).GT.0) THEN
              WRITE(LFNERR,4014) FILCOD(1,IFLINP),ANTNEW(1,1),
     1                    ANTNEW(2,1)
4014          FORMAT(/,' ### PG RXOBV3: NONZERO HORIZONTAL ',
     1                                  'ANTENNA OFFSETS',/,
     2                16X,'BERNESE HEADER FILE NAME: ',A,/,
     3                16X,'OFFSETS IN N AND E      : ',2F11.4,/)
            ELSE
              WRITE(LFNERR,4014) FILPHA(1,IFLINP),ANTNEW(1,1),
     1                    ANTNEW(2,1)
            ENDIF
          ENDIF
        ENDIF
C
        NUMAM0=0
C
C FREQUENCY: CHECK
C ----------------
        IF (LEN_TRIM(FILCOD(1,IFLINP)) +
     1      LEN_TRIM(FILPHA(1,IFLINP)) > 0) THEN
C IF RANGE MEASUREMENTS (SLR)
          IF (MEATYP.EQ.3 .AND. ERROPT%VERFRQ.GT.0) THEN
            DO ITYP = 1, NUMTYP
              IF (OBSTYP(ITYP)(2:2) == '1') NUMFREQ = 1
              IF (OBSTYP(ITYP)(2:2) == '2') NUMFREQ = 2
              RXFREQ = IWLRNX(NUMFREQ)/10D0
              CALL RXOFRQ(ERROPT,RXFILE(IFLINP),MEATYP,STNAME(IFLOUT),
     1          OBSTYP(ITYP),RXFREQ,TIMEPO(1),IRC)
              CHKFREQ = .TRUE.
C ERROR HANDLING
              IF (IRC.EQ.2) THEN       !SKIP FILE
                FILCOD(:,IFLINP) = ' '
                FILPHA(:,IFLINP) = ' '
              ENDIF
            ENDDO
          ENDIF
C
        ENDIF
C
C UPDATE THE OBSERVATION FILE NAMES AND SESSION ID
C ------------------------------------------------
        IF (LEN_TRIM(FILCOD(1,IFLINP)) +
     1      LEN_TRIM(FILPHA(1,IFLINP)) > 0) THEN
          CALL RXOBABBR(IABUPD,SESSION,STNAME(IFLOUT),
     1         FILCOD(1,IFLINP),FILPHA(1,IFLINP),CSESS(1:2,IFLINP))
        ENDIF
C
C COPY OBSERVATIONS FROM SCRATCH INTO OBS. FILES
C ----------------------------------------------
        COPYBACK = .TRUE.
        DO WHILE (COPYBACK)
          CALL UPDMEA(IFRMAT,FILCOD(2,IFLINP),FILPHA(2,IFLINP),
     1         SCROBS,NFREQ, WINDOW(1,IFLINP),STAINFO,STNAME(IFLOUT),
     2         AMBSAT,AMBIGU,IDELTT,nsmea,    svnmea, NEPOCH,
     3         TIMREF,ANZOBS,NEPFLG,IOKPRT(1,IFLOUT), AMBNEP)
          IF (nsmea(1) == 0) FILCOD(1,IFLINP) = ' '
          IF (nsmea(2) == 0) FILPHA(1,IFLINP) = ' '
          COPYBACK = .FALSE.
C
C SET UP ALL AMBIGUITIES AND CHECK MAXIMUM
C ----------------------------------------
          IF (ICSFLG.EQ.0) NUMAMB=NSMEA(2)
          IF (NUMAMB.GE.MAXAMB) THEN
            WRITE(LFNERR,'(/,A,2(/,16X,A,I5),/,16X,A,A,/)')
     1      ' *** PG RXOBV3: TOO MANY AMBIGUITIES IN FILE',
     2                      'NUMBER OF AMBIGUITIES: ',NUMAMB,
     3                      'MAXIMUM NUMBER       : ',MAXAMB,
     4                      'RINEX FILE: ',TRIM(RXFILE(IFLINP))

            CALL EXITRC(2)
          ENDIF
C
          CALL RNXAMB(ICSFLG,NSMEA(2),SVNMEA,TIMREF(2),IDELTT(2),
     1         NEPOCH(2),NFREQ(2),NWLSAT,IWLSAT,IWLRNX,AMBNEP,
     2         NUMAMB,AMBSAT,AMBIEP,AMBIGU,AMBCLS,AMBWLF)
C
C CHECK MINIMUM NUMBERS OF EPOCHS
C -------------------------------
          CALL FEWOBS(IOKPRT(1:2,IFLOUT),NUMAMB,REQOBS,NSMEA,SVNMEA,
     1                NFREQ(1),NEPOCH(1),ANZOBS(:,:,1),
     2                NFREQ(2),NEPOCH(2),ANZOBS(:,:,2),
     3                RXFILE(IFLINP),FILCOD(1:2,IFLINP),
     4                FILPHA(1:2,IFLINP),IFRMAT,SCROBS,COPYBACK)
        ENDDO
C
C WRITE CODE AND PHASE HEADERS
C ----------------------------
        IF (NEPOCH(1).LT.MINEPO.AND.FILCOD(1,IFLINP).NE.' ') THEN
          WRITE(LFNERR,4011) NEPOCH(1),FILCOD(1,IFLINP)
4011      FORMAT(/,' *** PG RXOBV3: NOT ENOUGH OBS. EPOCHS FOUND',/,
     1              16X,'HEADER FILE NOT WRITTEN',/,
     2              16X,'NUMBER OF OBSERV. EPOCHS:',I3,/,
     3              16X,'BERNESE HEADER FILE NAME: ',A,/)
        ELSE
          hlp_staNam(1)    =STNAME(IFLOUT); hlp_staNam(2) =''
          hlp_recTyp(1)    =RECTNW;         hlp_recTyp(2) =''
          hlp_antTyp(1)    =ANTTNW;         hlp_antTyp(2) =''
          hlp_irUnit(1)    =IRUNIT;         hlp_irUnit(2) = 0
          hlp_iAnten(1)    =IANTEN;         hlp_iAnten(2) = 0
          hlp_oprNam(1)    =OPRNAM;         hlp_oprNam(2) =''
          hlp_posecc(1:3,1)=ANTNEW(:,1);    hlp_posecc(1:3,2)=0d0
          hlp_iClock(1)    =ICLOCK;         hlp_iClock(2) = 0
C
          IF (usegeos==2) THEN
           CALL WTHEAD(FILCOD(1,IFLINP),
     1                MEATYP,NDIFF,NFREQ(1),NEPOCH(1),NSMEA(1),
     2                CSESS(:,IFLINP),IDELTT(1),TIMREF(1),CAMPGN(1:16),
     3                TITLE,CRDATE,CRTIME,IRMARK,NEPFLG(1),IFRMAT,
     4                HLP_STANAM,HLP_RECTYP,HLP_ANTTYP,
     5                HLP_IRUNIT,HLP_IANTEN,HLP_OPRNAM,HLP_POSECC,
     6                HLP_ICLOCK,SVNMEA(1,1),ANZOBS(1,1,1),NUMMRK,
     7                NUMAM0,AMBSAT,AMBIEP,AMBWLF,AMBIGU,AMBCLS,1,
     8                GOBSDEF)
          ELSE
           CALL WTHEAD(FILCOD(1,IFLINP),
     1                MEATYP,NDIFF,NFREQ(1),NEPOCH(1),NSMEA(1),
     2                CSESS(:,IFLINP),IDELTT(1),TIMREF(1),CAMPGN(1:16),
     3                TITLE,CRDATE,CRTIME,IRMARK,NEPFLG(1),IFRMAT,
     4                HLP_STANAM,HLP_RECTYP,HLP_ANTTYP,
     5                HLP_IRUNIT,HLP_IANTEN,HLP_OPRNAM,HLP_POSECC,
     6                HLP_ICLOCK,SVNMEA(1,1),ANZOBS(1,1,1),NUMMRK,
     7                NUMAM0,AMBSAT,AMBIEP,AMBWLF,AMBIGU,AMBCLS,USEGEOS,
     8                GOBSDEF)
          ENDIF
C
C EXTRACT INFORMATION FOR PROGRAM OUTPUT
C --------------------------------------
          IF (FILCOD(1,IFLINP).NE.' ') THEN
            DO ISAT = 1,NSMEA(1)
              IF (SVNMEA(ISAT,1).LT.100) THEN
                ISAPRT(1,IFLINP)=ISAPRT(1,IFLINP)+1
              ELSEIF (SVNMEA(ISAT,1).LT.200) THEN
                ISAPRT(2,IFLINP)=ISAPRT(2,IFLINP)+1
              ENDIF
              DO IFRQ = 1,NFREQ(1)
                IOBPRT(IFRQ,IFLINP)=IOBPRT(IFRQ,IFLINP)+
     1                              ANZOBS(ISAT,IFRQ,1)
              ENDDO
            ENDDO
          ENDIF
        ENDIF
C
        IF (NEPOCH(2).LT.2.AND.FILPHA(1,IFLINP).NE.' ') THEN
          WRITE(LFNERR,4011) NEPOCH(2),FILPHA(1,IFLINP)
        ELSE
          hlp_staNam(1)    =STNAME(IFLOUT); hlp_staNam(2) =''
          hlp_recTyp(1)    =RECTNW;         hlp_recTyp(2) =''
          hlp_antTyp(1)    =ANTTNW;         hlp_antTyp(2) =''
          hlp_irUnit(1)    =IRUNIT;         hlp_irUnit(2) = 0
          hlp_iAnten(1)    =IANTEN;         hlp_iAnten(2) = 0
          hlp_oprNam(1)    =OPRNAM;         hlp_oprNam(2) =''
          hlp_posecc(1:3,1)=ANTNEW(1:3,1);  hlp_posecc(1:3,2)=0d0
          hlp_iClock(1)    =ICLOCK;         hlp_iClock(2) = 0
C
          MEATYP=1
          IF (usegeos==2) THEN
           CALL WTHEAD(FILPHA(1,IFLINP),
     1                MEATYP,NDIFF,NFREQ(2),NEPOCH(2),NSMEA(2),
     2                CSESS(:,IFLINP),IDELTT(2),TIMREF(2),CAMPGN(1:16),
     3                TITLE,CRDATE,CRTIME,IRMARK,NEPFLG(2),IFRMAT,
     4                hlp_staNam,hlp_recTyp,hlp_antTyp,
     5                hlp_irUnit,hlp_iAnten,hlp_oprNam,hlp_posecc,
     6                hlp_iClock,SVNMEA(1,2),ANZOBS(1,1,2),NUMMRK,
     7                NUMAMB,AMBSAT,AMBIEP,AMBWLF,AMBIGU,AMBCLS,1,
     8                GOBSDEF)
          ELSE
           CALL WTHEAD(FILPHA(1,IFLINP),
     1                MEATYP,NDIFF,NFREQ(2),NEPOCH(2),NSMEA(2),
     2                CSESS(:,IFLINP),IDELTT(2),TIMREF(2),CAMPGN(1:16),
     3                TITLE,CRDATE,CRTIME,IRMARK,NEPFLG(2),IFRMAT,
     4                hlp_staNam,hlp_recTyp,hlp_antTyp,
     5                hlp_irUnit,hlp_iAnten,hlp_oprNam,hlp_posecc,
     6                hlp_iClock,SVNMEA(1,2),ANZOBS(1,1,2),NUMMRK,
     7                NUMAMB,AMBSAT,AMBIEP,AMBWLF,AMBIGU,AMBCLS,USEGEOS,
     8                GOBSDEF)
          ENDIF
C
C COMPARE NUMAMB WITH MAXAMB IN GPSEST
C ------------------------------------
          IF (NUMAMB.GT.MAXAMB2) THEN
            WRITE(LFNERR,'(/,A,2(/,16X,A),/,16X,A,A,2(/,16X,A,I5),/)')
     1      ' ### PG RXOBV3: The number of ambiguities in the zero',
     2      'difference file exceeds the maximum number',
     3      'of ambiguities in PG GPSEST.',
     4      'File name:     ',TRIM(FILPHA(1,IFLINP)),
     5      'Number of ambiguities in file:' ,numamb,
     6      'Maximum num. allowed in GPSEST:',maxamb2
          ENDIF
C
C EXTRACT INFORMATION FOR PROGRAM OUTPUT
C --------------------------------------
          IF (FILPHA(1,IFLINP).NE.' ') THEN
            DO ISAT = 1,NSMEA(2)
              IF (SVNMEA(ISAT,2).LT.100) THEN
                ISAPRT(3,IFLINP)=ISAPRT(3,IFLINP)+1
              ELSEIF (SVNMEA(ISAT,2).LT.200) THEN
                ISAPRT(4,IFLINP)=ISAPRT(4,IFLINP)+1
              ENDIF
              DO IFRQ = 1,NFREQ(2)
                IOBPRT(2+IFRQ,IFLINP)=IOBPRT(2+IFRQ,IFLINP)+
     1                                ANZOBS(ISAT,IFRQ,2)
              ENDDO
            ENDDO
            IAMPRT(IFLINP)=NUMAMB
          ENDIF
        ENDIF
C
C WRITE SUMMARY FILE
C ------------------
        DO 4500 IOBGAP=1,NOBGAP
          CALL TIMST2(1,2,TIMGAP(1:2,IOBGAP),TSTRNG)
          IF (IOBGAP.EQ.1) THEN
            NFRSUM=MAX(NFREQ(1),NFREQ(2))
            NSASUM=MAX(NSMEA(1),NSMEA(2))
            STRING=RXFILE(IFLINP)(1:32)
            CALL STRIPDIR(STRING)
            WRITE(LFNPRT,1002) IFLINP,STRING(1:20),
     1                         NFRSUM,NSASUM,
     2                         NEPGAP(IOBGAP),
     3                         TSTRNG,NRXOBS(IOBGAP,1:6)
1002        FORMAT(I4,2X,A20,1X,I2,I4,I6,2X,A40,2X,3(I8,I7))
          ELSE
            WRITE(LFNPRT,1003) NEPGAP(IOBGAP),
     1                         TSTRNG,NRXOBS(IOBGAP,1:6)
1003        FORMAT(33X,I6,2X,A40,2X,3(I8,I7))
          ENDIF
4500    CONTINUE
C
C UPDATE COORDINATES FILE (IF PRESENT IN FILENAME LIST)
C -----------------------------------------------------
        IF (LEN_TRIM(FILCOD(1,IFLINP)) +
     1      LEN_TRIM(FILPHA(1,IFLINP)) > 0) THEN
          NSTAT=NSTAT+1
          STANMS(NSTAT)=STNAME(IFLOUT)
          XSTAT(1:3,NSTAT) = POSXYZ(1:3)
          STAFLG(NSTAT) = 'R'
          TIMCRD=TIMCRD+TIMEPO(2)
        ENDIF
C
        IF(IRCOBS.EQ.5) GOTO  5000
C
C END OF RINEX INPUT FILE
C -----------------------
5010    CLOSE(UNIT=LFNOBS)
c
C
C RESET USEGEOS FOR SMT FILES
C ---------------------------
        IF (USEGEOS==2) USEGEOS=0
C
2000  CONTINUE
C
C
C DEALLOCATE ARRAYS USED FOR FREQUENCY INFORMATION
C ------------------------------------------------
      IF (CHKFREQ) THEN
        DUMMY1 = ''
        DUMMY2 = ''
        CALL RXOFRQ(ERROPT,DUMMY1,0,DUMMY2,'R1',0D0,0D0,IRC)
      ENDIF
C
C FINAL LINE FOR SUMMARY FILE
      WRITE(LFNPRT,'(A)') ''
C
      IF(IFLOUT.NE.NFLOUT) THEN
        WRITE(LFNERR,2001) NFLOUT,IFLOUT
2001    FORMAT(/,' *** PG RXOBV3: TOO MANY OUTPUT FILES DEFINED',/,
     1                       16X,'# OF OUTPUT FILES DEFINED:',I4,/,
     2                       16X,'# OF OUTPUT FILES FOUND  :',I4,/)
        NFLOUT=IFLOUT
      END IF
C
C WRITE STATION RENAMING TRANSLATION SUMMARY
C ------------------------------------------
      WRITE(LFNPRT,'(//,2(1X,A,/),/,1X,A,/,1X,132("-"))')
     1            'STATION NAMING SUMMARY:',
     2            '----------------------',
     3            'Num  Rinex file name                     ' //
     4            'Station name        Rinex marker name     ' //
     5            'Rinex marker number Rinex marker type'
      DO IFLOUT=1,NFLOUT
        IF (LEN_TRIM(LINE(IFLOUT,1)).EQ.0) CYCLE
        WRITE(LFNPRT,'(1X,A)') TRIM(LINE(IFLOUT,1))
      ENDDO
      WRITE(LFNPRT,'(A)') ''
C
C
C WRITE RECEIVER/ANTENNA TRANSLATION SUMMARY
C ------------------------------------------
      IF (MEATYP /= 3) THEN
        WRITE(LFNPRT,'(//,2(1X,A,/),/,2(1X,A,/),1X,132("-"))')
     1      'STATION INFORMATION SUMMARY:',
     2      '---------------------------',
     3      '                   Station information used in Bernes' //
     4      'e observation files         RINEX header information ' //
     5      'if it is different',
     6      'Station name      Receiver name        Antenna name  ' //
     7      '      Antenna height    Receiver name        Antenna ' //
     8      'name        Antenna height'
        DO IFLOUT=1,NFLOUT
          IF (LEN_TRIM(LINE(IFLOUT,2)).EQ.0) CYCLE
          WRITE(LFNPRT,'(1X,A)') TRIM(LINE(IFLOUT,2))
        ENDDO
        WRITE(LFNPRT,'(A)') ''
      ENDIF
C
C WRITE MARKER TYPE SUMMARY IF NECESSARY
C --------------------------------------
      IF (MRKTYP_EX.AND.MEATYP /= 3) THEN
        WRITE(LFNPRT,'(//,2(1X,A,/),/,1X,A,/,1X,132("-"))')
     1              'STATION MARKER TYPE SUMMARY:',
     2              '---------------------------',
     3              'Station name      Marker type               ' //
     4              'Rinex marker type if it is different'
        DO IFLOUT=1,NFLOUT
          IF (LEN_TRIM(LINE(IFLOUT,3)).EQ.0) CYCLE
          WRITE(LFNPRT,'(1X,A)') TRIM(LINE(IFLOUT,3))
        ENDDO
        WRITE(LFNPRT,'(A)') ''
      ENDIF
C
C WRITE INPUT/OUPUT FILE NAMES TO OUTPUT FILE
C -------------------------------------------
      WRITE(LFNPRT,12)
12    FORMAT(//,' TABLE OF INPUT AND OUTPUT FILE NAMES:',
     2       /, ' ------------------------------------',
     3       /,78X,   '   #observations    #satell.',
     4       /,' Num  Rinex file name',19X,
     5                'Bernese code  header  file name   #epo',
     6                '   #C1/P1    #P2    GPS  GLO',
     7       /,40X,   'Bernese code  observ. file name       ',
     8       /,40X,   'Bernese phase header  file name   #epo',
     9                '      #L1    #L2    GPS  GLO   #amb',
     .       /,40X,   'Bernese phase observ. file name       ',
     1       /,1X,132('-'),/)
C
C WRITE FILE INPUT AND OUTPUT FILE NAMES AND NUMBER OF EPOCHS
C -----------------------------------------------------------
      DO 6000 IFLOUT=1,NFLOUT
C
C check if INDINP(IFLOUT) equals 0 (happens, when no epochs found in
c observation file for specified sat. system)
        IF(INDINP(IFLOUT).EQ.0) CYCLE
        IFLINP=INDINP(IFLOUT)
C
        IF(FILCOD(1,IFLINP).NE.' ') THEN
          FILPRT(1)=FILCOD(1,IFLINP)
          FILPRT(2)=FILCOD(2,IFLINP)
        ELSE
          FILPRT(1)='  ---'
          FILPRT(2)='  ---'
        ENDIF
C
        IF(FILPHA(1,IFLINP).NE.' ') THEN
          FILPRT(3)=FILPHA(1,IFLINP)
          FILPRT(4)=FILPHA(2,IFLINP)
        ELSE
          FILPRT(3)='  ---'
          FILPRT(4)='  ---'
        ENDIF
C
        WRITE(LFNPRT,1012) IFLINP,RXFILE(IFLINP)(1:32),FILPRT(1)(1:32),
     1        IOKPRT(1,IFLOUT),IOBPRT(1:2,IFLINP),ISAPRT(1:2,IFLINP),
     2        FILPRT(2)(1:32),FILPRT(3)(1:32),IOKPRT(2,IFLOUT),
     3        IOBPRT(3:4,IFLINP),ISAPRT(3:4,IFLINP),IAMPRT(IFLINP),
     4        FILPRT(4)(1:32)
1012    FORMAT(I4,2X,A32,2X,A32,I6,2X,2I7,2X,2I5,/,40X,A32,/,
     1         40X,A32,I6,2X,2I7,2X,2I5,2X,I5/,40X,A32,/)
6000  CONTINUE
C
C ADD NEW STATIONS TO THE COORDINATE FILE
C ---------------------------------------
      CALL GTFLNA(0,'COORD',filCrd,irc)
      IF (LEN_TRIM(filCrd) > 0) THEN
        CALL inquire(FILE=filCrd,EXIST=yes)
        IF (yes) THEN
          CALL GETCO3(FILCRD,1,(/'@'/),NDUMMY,CDUMMY,
     1                TITLE=STRING,DATUM=DATUM,TIMCRD=TIMCRD)
        ELSE
          DATUM ='WGS - 84'
          STRING=TITLE
          TIMCRD=TIMCRD/NSTAT
        ENDIF
        CALL WTSTAT(2,FILCRD,STRING,DATUM,NSTAT,STANMS,
     1              XSTAT,STAFLG=STAFLG,TIMCRD=TIMCRD)
      ENDIF
C
C CHECK FOR DOUBLE FILENAMES
C --------------------------
      DO IFIL = 1,NFLOUT-1
C check if INDINP(IFIL) equals 0 (happens, when no epochs found in
C observation file for specified sat. system)
        IF(INDINP(IFIL).EQ.0) CYCLE
c
        IFIL1=INDINP(IFIL)
        IF (LEN_TRIM(FILCOD(1,IFIL1)).EQ.0 .AND.
     1      LEN_TRIM(FILPHA(1,IFIL1)).EQ.0) CYCLE
C
        DO JFIL = IFIL+1,NFLOUT
C check if INDINP(IFIL) equals 0 (happens, when no epochs found in
C observation file for specified sat. system)
          IF(INDINP(JFIL).EQ.0) CYCLE
          JFIL1=INDINP(JFIL)
          IF (LEN_TRIM(FILCOD(1,JFIL1)).EQ.0 .AND.
     1        LEN_TRIM(FILPHA(1,JFIL1)).EQ.0) CYCLE
C
          IF ((FILCOD(1,IFIL1).EQ.FILCOD(1,JFIL1) .AND.
     1        LEN_TRIM(FILCOD(1,IFIL1)).GT.0)        .OR.
     2        (FILPHA(1,IFIL1).EQ.FILPHA(1,JFIL1) .AND.
     3        LEN_TRIM(FILPHA(1,IFIL1)).GT.0))      THEN
            WRITE(LFNERR,'(/,A,/,16X,A,2(/,16X,A,A),/)')
     1        ' ### PG RXOBV3: A filename appears more than once.',
     2                'This may result in data loss.',
     3                'Code  file name: ',TRIM(filcod(1,ifil1)),
     4                'Phase file name: ',TRIM(filpha(1,ifil1))
          ENDIF
        ENDDO
      ENDDO
C
C Stop with error, if requested (new tables only)
      IF (erropt%stpErr) CALL exitrc(2)

9999  CALL EXITRC(IRCODE)
      END

