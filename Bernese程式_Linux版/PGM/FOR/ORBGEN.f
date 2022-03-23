C*
      PROGRAM ORBGEN
CC
CC NAME       :  ORBGEN
CC
CC PURPOSE    :  GENERATE STANDARD ORBITS FROM TABULAR EPHEMERIDES
CC               IN ADDITION GENERATE FILE WITH PARTIALS OF ORBIT
CC               WITH RESPECT TO SOLAR RADIATION PRESSURE PARAMETERS
CC               THIS PROGRAM USES A VERY EFFICIENT INTEGRATION
CC               ROUTINE (INTFST)
CC               IF GENUPD=3, THE PROGRAM IS USED AS AN ORBIT UPDATE
CC               PROGRAM, USING THE ELE-FILE INFORMATION.
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC CREATED    :  87/11/21 11:42
CC
CC CHANGES    :  24-JUN-91 : ??: PROBLEM WITH ARC DEFINITION
CC               23-APR-92 : ??: NEW INTERNAL FILE NAMES "STDOUT" AND
CC                               "RPROUT"
CC               05-MAY-92 : ??: PROBLEM IF START OF ARC EXACTLY EQUALS
CC                               ONE OF THE TABULAR EPOCHS (TOBS.LE.0.D0)
CC               31-MAY-92 : ??: ROCK-4/42 MODEL IMPLEMENTED
CC                               LIGHT/SHADOW TRANSITS MODELED CORRECTLY
CC               04-JUN-92 : ??: OPTION J2000.0, COMMON/ORBSYS. USE "ZERO"
CC                               AS INDICATOR OF REF.SYSTEM:
CC                               B1950.0 : ZERO=0.D0; J2000.0 : ZERO=2.D0.
CC                               OPNFIL IMPLEMENTED; SHDCHG WITH "IORSYS".
CC               15-JUN-92 : LM: THE POSSIBILITY TO WRITE THE RESIDUALS
CC                               INTO THE RESIDUAL FILE (TIME INTERVAL
CC                               BETWEEN TWO EPOCHS IS STILL 1000 S)
CC               20-JUN-92 : ??: USE NEW SATELLITE INFORMATION FILE
CC               22-JUN-92 : ??: ADD OPTION TO ESTIMATE P0-DRIFT
CC               11-JUN-92 : ??: CHANGE OUTPUT. NEW SUBROUTINE "DSPRIF"
CC               13-JUL-92 : ??: COMMON FOR "MAXFRQ" DUE TO RESIDUAL FILE
CC               25-JUL-92 : ??: ADD "IF...ENDIF" FOR WRITE TO LFNSCR ;
CC                               MAXARN=150
CC               30-JUL-92 : LM: BETTER SATELLITE STATISTICS
CC               20-AUG-92 : ??: SATELLITE OFFSET CORRECTIONS
CC               10-SEP-92 : ??: PRINT: END OF ARC BEFORE END OF ECLIPSE
CC               13-OCT-92 : ??: OPTION "IPRRES" FOR RESIDUAL PRINTING,
CC                               OPTIONS FOR PLOTTING REMOVED. WRITING OF
CC                               PLOT FILE DEPENDING ON GTFLNA RETURN CODE
CC               16-NOV-92 : ??: CORRECT LIGHT <--> SHADOW TRANSITS FOR
CC                               VARIATIONAL EQUATIONS
CC               24-DEC-92 : ??: ADD "MAXSAA" FOR ARRAYS OF SR "GTSATA",
CC                               USE MODULO 50 DUE TO MANOEUVRES,
CC                               CORRECT INDEX IN COMPUTATION OF RMS OF
CC                               PARAMETERS
CC               07-JAN-93 : ??: ADD COMMON FOR "MAXFIL"
CC               28-APR-93 : ??: BACKSPACE STATEMENT REPLACED BY SAVING
CC                               RECORD INFORMATION (PROBLEM DEC ALPHA)
CC               21-AUG-93 : ??: SUBROUTINE SHDHDL FOR LIGHT/SHADOW HANDLING
CC                               SUBROUTINE SNGHDL FOR HANDLING OF "ALMOST"
CC                               SINGULARITIES (Y-BIAS, ROCK IV ETC) WHEN
CC                               SUN IS IN ORBITAL PLANE
CC               24-AUG-93 : LM: MANOEUVRES
CC               22-SEP-93 : ??: PRINTING OF ECLIPSE TIMES CHANGED
CC               29-SEP-93 : ??: SUBROUTINE "SHDPRI" TO PRINT ECLIPSE TIMES
CC                               TEST "TOBS.LT.-1.D-14" NOT "TOBS.LT.0.D0"
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               11-JAN-94 : MR: PRINT REFERENCE SYSTEM
CC               10-AUG-94 : MR: CALL EXITRC
CC               12-AUG-94 : MR: FORMAT 4: SESSION AS CHARACTER*4
CC               17-AUG-94 : MR: MAXMAN=100 (OLD: MAXMAN=10)
CC               18-AUG-94 : LM: DON'T SET MANOEUVER FOR THE SECOND TIME
CC               23-OCT-94 : GB: INTRODUCE SR MOSUPN, TO AVOID REPEATED
CC                               COMPUTATION OF POS. OF SUN, MOON, ETC.
CC               14-MAR-95 : MR: CORRECT DECLARATIONS FOR "WTRESH"
CC                3-OCT-95 : WG: DECLARATION OF ANTOFF CHANGED
CC               02-NOV-95 : MR: CALL MOSUPN WITH WRONG # OF ARGUMENTS
CC               27-DEC-95 : GB: IMPLEMENT NEW ORBIT REPRESENTATION
CC                               NEW "ZERO"= OLD "ZERO" + 10
CC                               NEW SR RPROUT
CC                               INTEGRATE PARTIALS FOR ORBITAL ELEMENTS
CC                               SEPARATE INTEGRATION ORDER/SUBDIVISION
CC                               AND REPRESENTATION FOR RPR-FILE.
CC               05-JAN-96 : GB: USE THIS PROGRAM FOR ORBIT UPDATES
CC                               THROUGH *.ELE FILES, TOO. (REPLACE
CC                               OLD PROGRAM UPDSTD, RESP. UNIFY
CC                               DEFSTD AND UPDSTD)
CC               27-JUN-96 : TS: ORBIT MODEL INDICATOR FLAG
CC               11-JUL-96 : TS: APPLY C02 DRIFT FOR JGM3 (GETPOT)
CC               02-OCT-96 : TS: NEW ORBIT MODELS (MODEL "B")
CC               14-NOV-96 : TS: MAXINT INCREASED FOR LONG ARCS (21-DAYS)
CC               29-APR-97 : HH: ADD GLONASS (MAXSAT=48, FORMATS)
CC               18-SEP-97 : MR: MAXSAA IN INCLUDE MAXSAT
CC               06-OCT-97 : MR: MAXECL CHANGED FROM 10 TO 20
CC               22-JUL-98 : SF: INCLUDE TIME DEPENDENT SAT. ANT. OFF'S
CC               27-AUG-98 : MR: USE FUNCTION "MODSVN"
CC               10-MAY-99 : MR: WRITE SUMMARY FILE
CC               21-SEP-99 : TS: CHECK FOR ORBMOD(1).EQ.99
CC               03-MAY-00 : HB: INCLUDE I:MAXPOT, CHANGE INDEXING OF
CC                               CPOT AND SPOT (SR GETPOT), REMOVE COEFLP
CC                               FROM COMMON POTCOE, CHANGE CALL OF SR
CC                               POTPRT (INDICES OF CPOT,SPOT)
CC               11-AUG-00 : DI: OPNSYS BEFORE FIRST ERROR MESSAGE
CC               13-NOV-00 : RD: CALL OF DSPRIF FOR PRE FILES
CC               15-NOV-00 : RD: WRITE FILE NAMES FOR ORBIT UPDATE
CC               18-DEC-00 : HU: USE INTERFACE FOR PRFLNA
CC               29-JAN-01 : RD: SMALL CHANGE IN STORING THE RESIDUALS
CC               13-JAN-02 : HU: CONSIDER SHADOW OF MOON
CC               06-JUL-01 : DS: "D" MODEL
CC               21-FEB-02 : DS: TEG4 AND EIGEN1S GRAVITY MODELS
CC                               "E" AND "F" MODEL
CC               21-FEB-02 : DS:  USE MODFLG FOR MODEL
CC               25-FEB-02 : DS: MAXMOD: 10->20
CC               28-JUN-02 : RD/DS: MERGING VERSIONS BE AND MUNICH
CC               27-Aug-02 : RD: Handle new formatted residual files
CC               25-SEP-02 : HU: REMOVE I_ASTLIB
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               03-MAR-03 : SC: ADD TITLE SECTION
CC               17-APR-03 : RD: CORRECT SETTING OF ARRAYS
CC               07-MAR-03 : HU: MAXINT MOVED TO M_MAXDIM
CC               08-MAR-03 : HU: TITPOT CHANGED TO CHARACTER 80
CC               16-MAY-03 : HB: INITIALIZE STRUCTURES
CC               11-JUN-03 : HU: NEW CALL FOR MOSUPN
CC               06-AUG-03 : HU: NEW STD FORMAT
CC               11-AUG-03 : RS: USE M_BERN, CHANGE CALL OF GTSATA
CC               30-AUG-03 : HU: READ LIST OF BAD SATELLITES
CC               23-OCT-03 : HU: NEW PARAMETER FOR SHMHDL
CC               24-OCT-03 : SS: T0ARC=DINT(TB1) REPLACED BY T0ARC=TB1
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               12-DEC-03 : AJ: HANDLE STOCHASTIC ACCELERATIONS
CC               06-JAN-04 : HU: COMMON POTCOE, CALL FOR TIDPT2 MODIFIED
CC               15-NOV-04 : HU: MAXECL = MAXSAT
CC               14-DEC-04 : HB: ADD INTERFACE FOR SR GETPOT
CC               31-JAN-05 : HU: ORBIT MODEL DESCRIPTION
CC               03-FEB-05 : HU: MAXOMD REMOVED FROM CALL TO OSCALL
CC               04-APR-05 : AJ: WRITE STOCH. PARAMETERS TO RPR-FILE
CC               14-APR-05 : HU: ROUNDING PROBLEM SOLVED
CC               11-MAY-04 : DS: DO NOT CHECK NAME OF THE GRAVITY MODEL
CC               21-MAY-05 : HU: WRITE RESIDUALS ONLY IN LAST ITERATION
CC               04-JUN-05 : HU: ERROR IN ERROR MESSAGE CORRECTED
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: LFNUM.inc, COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-JUN-05 : MM: UNUSED VARIABLES REMOVED
CC               08-Aug-05 : HB: Use new SR TIMST2 (module)
CC               09-NOV-05 : AG: SENNUM for GTSATA CALL ADDED
CC               22-JAN-06 : HU: DTABD 1.0 -> 0.5  MIN
CC               09-FEB-06 : HB: NEW PARAMETER "WINDOW" FOR RDORBI
CC                               DON'T USE LAST POSITION IF AN ARC FOLLOWS
CC               18-JUL-06 : AG: CMC IMPLEMENTED
CC               21-SEP-06 : HU: MEAN POLE HANDLING
CC               25-OCT-06 : MP: RPR MODEL ESTIMATION SWITCH
CC               16-FEB-07 : HB: ADD TPONAM AS PARAMETER TO SR OTIDNAM AND
CC                               ORBMDNAM
CC               27-FEB-07 : AG: CALL DEFCON WITH PARAMETER
CC               10-SEP-07 : GB: MAJOR REVIEW: MODULE STCOGN.F90 ADDED
CC               10-SEP-07 : GB: SR GTSATM REPLACED BY GTSATP
CC               26-FEB-08 : RD: USE GTSATM/GTSATB FROM D_SATCRX
CC               04-MAY-08 : RD: NUMSAT ADDED TO A FEW SR'S
CC               22-JUL-08 : DT: Store PLT file for residuals (RESPLOT)
CC               29-JUL-08 : DT: CHANGE DTABD TO 1 SEC (FROM 1 MIN)
CC               05-AUG-08 : DT: ADD OTDMIN TO RDORBI; REMOVE ORBDSC FROM
CC                               CALL TO ORBMDNAM, ORBMDCHK
CC               15-SEP-08 : DT: CORRECT SCALING FOR PRINTING RPR PARAMETERS
CC               29-SEP-08 : RD: PRE- AND TAB- FILES ARE SORTED IN RDORBI
CC               28-OCT-08 : DT: USE MAXVAR FROM M_MAXDIM
CC               14-NOV-08 : DT: ADD INDTIM TO SR STXYZ2 AND ORBMDNAM
CC               03-JUL-09 : SL: WRITE STATEMENT CORRECTED (COMMA ADDED),
CC                               MOSUPN CALL CORRECTED
CC               21-MAY-10 : MF: CALL SR INIT_FILHEAD
CC               10-AUG-10 : RD: USE TIMST2 INSTEAD OF TIMSTR
CC               26-AUG-10 : DT: USE MAXINT FROM P_ORBGEN
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               03-OCT-10 : CR: NEW CALL OF ARGSUN
CC               03-DEC-10 : RD: CMC FOR ATL ADDED
CC               03-DEC-10 : KS: ADD OTDDEG (MAX DEGREE OF OCEAN TIDES MODEL)
CC               28-JAN-11 : SL: USE M_BERN,D_RESFIL WITH ONLY, GET MOON ECL FLG
CC               24-FEB-11 : RD: REVISE PULSE SETUP DURING MANEUVERS
CC               17-MAR-11 : CR: CALL S_MOSUPN AS MODULE
CC               05-MAY-11 : HB: USE D_MODEL FOR SETTING MODEL KEYS
CC               19-MAY-11 : HB: USE VARIABLES AND NOT 0.D0 FOR CALL ECLMOON
CC               26-MAY-11 : HB: ARRAY DRDHLP HAS TO BE DECLARED WITH MAXVAR
CC               15-NOV-11 : SL: ONE MORE DIGIT FOR MJD IN LFNPL2
CC               30-NOV-11 : SL: NEW TITLE STRING FOR PRITIT
CC               28-MAR-12 : RD: USE TIMSTR AS MODULE NOW
CC               12-FEB-13 : SL: USE XDUM AND YDUM IN ECLMOON CALL
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC               UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: staNam2Length,lfnErr, lfnPrt, lfnOrb, lfn001,
     1                    lfn002, lfnPlt, lfnRes
      USE m_cpu,    ONLY: cpu_start
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE d_shadow, ONLY: t_mooecl, init_mooecl
      USE m_maxdim, ONLY: maxsat, maxsaa, maxbad, maxpot,
     1                    maxocn, maxVar
C
      USE p_orbgen, ONLY: t_orbmodel, orbdsc, maxint
      USE d_resfil, ONLY: t_reshead, init_reshead, init_filHead,
     1                    resHed_ORBGEN
      USE d_const,  ONLY: AE, GM, P0
      USE d_satcrx, ONLY: gtsatm, gtsatb
      USE d_model,  ONLY: setModKey, getModKey, chrValLength,
     1                    mod_orb_meaPol, mod_orb_ethPot, mod_orb_subMod
      USE s_cmc,    ONLY: chkcmc
      USE s_iordup
      USE s_wtskel
      USE s_orbmdnam
      USE s_opnfil
      USE s_alcerr
      USE s_prflna
      USE s_mosupn
      USE s_dsbprb
      USE s_argsun
      USE s_intfst
      USE s_shmhdl
      USE s_shadow
      USE s_eclmoon
      USE s_pritit
      USE s_snghdl
      USE s_rvpder
      USE s_deflcq
      USE s_vprod
      USE s_ephem
      USE s_dsprif
      USE s_readinpf
      USE s_opnerr
      USE s_shdpri
      USE s_aplstc
      USE s_oscall
      USE s_timst2
      USE s_timstr
      USE s_rpartn
      USE s_eletra
      USE s_setxyz
      USE s_rprout
      USE s_getpot
      USE s_shdhdl
      USE s_prtint
      USE s_ypol
      USE s_otidnam
      USE s_defcon
      USE s_exitrc
      USE s_orbmdchk
      USE s_rdorbi
      USE s_opnsys
      USE s_jmt
      USE s_dsbnds
      USE s_resorb
      USE s_wtresh2
      USE s_rdnutsub
      USE s_gtsata
      USE s_gtflna
      USE s_stxyz2
      USE s_ogsumf
      USE f_djul
      USE f_modsvn
      USE s_gttabhed
      USE s_stcogn, ONLY: stcogn_init, stcogn_update, stcogn_beta,
     1                    stcogn_solve, stcogn_inipar,
     2                    stcogn_applyPos,stcogn_applyVel
      USE s_stcogn, ONLY: m_rpress, m_rpress_rms, m_rpress_ini,
     1                    m_rms_tot, m_curpar, m_inipar, m_nobs_tot,
     2                    m_stoch, m_npar_tot, m_nstcepo, m_rmspar,
     3                    m_stcepo, m_indsat, t_stcopt, m_nstcmax,
     4                    m_pulse, m_pulse_rms
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      CHARACTER(LEN=8) :: pgName = 'ORBGEN'
      CHARACTER(LEN=8) :: srnGet

      INTEGER*4 I     , I1    , I2    , I3    , IANTOF, IARC  , IARCNW,
     1          ICOR  , IEIE  , IELE  , IEOF  , IEP   , IEPOCH, IFIL  ,
     2          IFMT  , IFRQ  , II    , III   , IK    , IKL   , IMAN  ,
     3          IMONTH, IND   , IND1  , IND2  , IND3  , INDSUN, INDTIM,
     4          INT   , IORSYS, IOS   , IOSTAT, IPAR  , IPOTNM, IPRCHS,
     5          IPRRES, IRC   , IRCORB, IRCPLT, IRCRES, IRCRPR, IRCTOT,
     6          IREC  , IRPR  , IS2   , ISAANT, ISAT  , ISATEL, ISAVEX,
     7          ISHUSE, ISTART, ITER  , IUPD  , IYEAR , IZTID , JSAT  ,
     8          K     , KIL   , KK    , KRPR  , L     , LFNSCR, MAXARC,
     9          MAXARN, MAXCHG, MAXECL, MAXELE, MAXFIL, MAXFLD, MAXFRQ,
     1          MAXMAN, MAXMOD, MAXPAR, MAXQ1 , MODEL , OTDDEG,
     2          MXCARC, MXCCHG, MXCECL, MXCELE, MXCFIL, MXCFRQ, MXCINT,
     3          MXCQ1 , MXCSAT, MXCVAR, MXRECL, NARC  , NBAD  , NEW   ,
     4          NFIL  , NFLCOL, NINT  , NINT1 , NITER , NMAN  , NNN   ,
     5          NPOINT, NPOTMX, NSAANT, NSACHG, NSAFIL, NSAT  , NTERM ,
     6          NUMSVN, NUNB  , NVAR  , MPOL  , IEPO  , IIKK  , INTMOM,
     7          NSTMOM, LMIN  , ircpl2, iosta2, ANTTHR, ERPMOD
C
      REAL*8    DAY   , DET   , DTABD , DTPI  , DTPIV , DUM   ,
     1          DUMMY , ERRMAX, GPSUTC, H     , OTDMIN, REY   , RPRMOM,
     2          RSAT  , SCAL  , SCALPA, SVNSAV, T0    , T0ARC , T0YEAR,
     3          TA    , TB    , TB1   , TB2   , TLEFT , TOBS  , TOBSAV,
     4          TOSC  , TOSCC , TPRINT, TRIGHT, TSCOSC, TSEC  , TTT   ,
     5          UT1UTC, XDUM  , XYZRES, YDUM  , ZERO,   BTMPO,  TMID  ,
     6          TMIN  , TEST  , TSTA  , TEND  , INDMON, rmPol,  numVal
C
      LOGICAL   CMCYN(2)
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C MAXIMAL DIMENSIONS
C ------------------
      PARAMETER (MAXPAR=20,MAXARC=300,MAXECL= MAXSAT,
     1           MAXQ1 =20,MAXFIL=50,MAXELE= 15,MAXARN=500,MAXCHG=100,
     2           MAXFRQ= 3,MAXFLD= 1,MAXMAN=100,MAXMOD=20)
C
      PARAMETER (MXRECL=10000)
C
C
C MAXSAT: MAXIMUM NUMBER OF SATELLITES
C MAXPAR: MAXIMUM NUMBER OF PARAMETER TO BE ESTIMATED PER SATELLITE
C MAXINT: MAXIMUM NUMBER OF INTEGRATION INTERVALS
C MAXARC: MAXIMUM NUMBER OF ARCS
C MAXQ1 : MAXIMUM NUMBER FOR POLYNOMIAL DEGREE + 1 (NUMERICAL INTEGR.)
C MAXFIL: MAXIMUM NUMBER OF TABULAR ORBIT FILES
C MAXELE: MAXIMUM NUMBER OF ORBITAL ELEMENTS
C MAXARN: MAXIMUM NUMBER OF NEW ARCS
C MAXECL: MAXIMUM NUMBER OF ECLIPSING SATELLITES
C MAXCHG: MAXIMUM NUMBER OF LIGHT/SHADOW CHANGES PER SATELLITE
C MAXFRQ: MAXIMUM NUMBER OF "FREQUENCIES" (COMP.) FOR RESIDUAL FILE
C MAXFLD: MAXIMUM NUMBER OF FIELDS IN PLOT SKELETON
C MAXSAA: MAXIMUM NUMBER OF SATELLITES IN SATELLITE INFO FILE (GTSATA)
C MAXMAN: MAXIMUM NUMBER OF MANOEUVRES
C MAXMOD: MAXIMUM NUMBER OF DIFFERENT ORBIT MODEL SETTINGS (SEE RDORBI)
C
C DECLARATIONS
C ------------
      TYPE(t_stcopt) :: stcopt

      REAL*8 X(3),SCALPRT(20),DRV(6)
      REAL*8 XSAT(18),YCOE(3,MAXQ1,MAXSAT),FAC(21),XTF(3),XTFSAV(3)
      REAL*8 DANOR(MAXPAR*(MAXPAR+1)/2),DBNOR(MAXPAR),
     1                             AAA_COR,BBB_COR,DRMS
      REAL*8 DRDELE(3,MAXPAR),DVDELE(3,MAXPAR),DRDHLP(3,MAXVAR)
      REAL*8 RMS(MAXSAT),RMSRAO(3,MAXSAT)
      REAL*8 RESMAX(3,MAXSAT),RMSTOT(MAXSAT)
      REAL*8 SOL(MAXPAR,MAXSAT)
      REAL*8 TB12(MAXINT+1),TB12S(MAXINT+1)
      REAL*8 Z01(3*MAXVAR*2,MAXSAT),ZCOE(3*MAXVAR*MAXQ1,MAXSAT)
      REAL*8 ZSAT(3*MAXVAR*2)
      REAL*8 CPOT((MAXPOT+1)*(MAXPOT+2)/2),SPOT((MAXPOT+1)*(MAXPOT+2)/2)
      REAL*8 XSUN(4),XMOON(4),GURTNR(3),ANTEST(MAXSAT)
      REAL*8 XVSAVE(3,2,MAXSAT)
      REAL*8 RPRESS(MAXVAR,MAXSAT),SIGRPR(MAXVAR,MAXSAT),TFL(2,MAXARC)
      REAL*8 RPRESS2(MAXSAT,9),SIGRPR2(MAXSAT,9)
      REAL*8 BTMP0(MAXSAT),UTMP0(MAXSAT)
      REAL*8 A(MAXSAT),E(MAXSAT),XI(MAXSAT),XKN(MAXSAT),PER(MAXSAT)
      REAL*8 TPER(MAXSAT),U0(MAXSAT),TBNDS(2,MAXSAT),XBNDS(3,2,MAXSAT)
      REAL*8 ARCINT(2,MAXARN),ARCELE(MAXELE,MAXARN)
      REAL*8 ARCSIG(MAXELE,MAXARN),ARCPSD(MAXELE,MAXARN)
      REAL*8 ARCT00(MAXARN)
      REAL*8 CHGTIM(2,MAXECL,MAXCHG)
      REAL*8 ANTOFF(6,MAXSAA),EX(3),EY(3),EZ(3),TIMINT(2,MAXSAA)
      REAL*8 TIMMAN(MAXMAN)
      REAL*8 ELE(7,MAXSAT)
      REAL*8 TIMSTC(MAXINT,MAXSAT),PARSTC(3,MAXINT,MAXSAT)
      REAL*8 RMSSUM(MAXARC,MAXFIL,MAXSAT)
      REAL*8 TIMBAD(2,MAXBAD)
      REAL*8 PSCMOM(3,MAXINT)
      REAL*8 TSTMOM(MAXINT)
      REAL*8 WINDOW(2), NSAMPL(2)
      REAL*8 DUMPRE(3,3),DUMNUT(3,3),DUMSUN(4),DUMMON(4),DUMVEL(3)
C
      INTEGER*4 Q,QVAR,QP1,L1(MAXPAR),L2(MAXPAR),NAVNUM(MAXSAT)
      INTEGER*4 ADDVAR,RPRFRM
      INTEGER*4 SVNFIL,ARCFIL(MAXARC),NUMOBS(MAXSAT)
      INTEGER*4 LOCQ(6,MAXPAR),LOCINT(6,MAXVAR),INDLOC(MAXPAR)
      INTEGER*4 NAVFIL(MAXSAT),NSATEL(MAXARC)
      INTEGER*4 NUMSAT(MAXSAT,MAXARC)
      INTEGER*4 SATCHG(MAXECL),IDXCHG(MAXECL),NUMCHG(MAXECL)
      INTEGER*4 IPRCHG(MAXECL),IDXSAT(MAXSAT)
      INTEGER*4 SATANT(MAXSAA),SATBLK(MAXSAA),IPLFLG(MAXFLD)
      INTEGER*4 SATMAN(MAXMAN),SENNUM(MAXSAA)
      INTEGER*4 GENUPD,ORBMOD(MAXMOD)
      INTEGER*4 NSTC(MAXSAT),NSTCEP(MAXINT,MAXSAT),
     1          INTSTC(MAXINT,MAXSAT),FRCTYP(3,MAXINT,MAXSAT)
      INTEGER*4 NUMSUM(MAXARC,MAXFIL,MAXSAT),IECLIP(MAXARC,MAXSAT)
      INTEGER*4 IDASUM(MAXARC,MAXFIL),IDXFIL(MAXFIL)
      INTEGER*4 SATBAD(MAXBAD),IOBBAD(MAXBAD),IACBAD(MAXBAD)
      INTEGER*4 ISTMOM(MAXINT),NSCMOM(MAXINT),FCTMOM(3,MAXINT)
      INTEGER*4 LFNSAT,lfnpl2
      INTEGER*4 ITIM
C
      CHARACTER*80 FIELDS(MAXFLD),OUTARR,TITPOT,FILNAM2
c     CHARACTER*80 TITLE
      CHARACTER*32 FILNAM,TABFIL(MAXFIL),PREFIL(2,MAXFIL),FILSKL,FILPLT
      CHARACTER*20 STRING
      CHARACTER*19 TSTRNG
      CHARACTER*16 ARCSAT(MAXARN)
      CHARACTER*16 NUTNAM,SUBNAM,TPONAM,OTIDNM,POTNAM,CMCMOD(2)
      CHARACTER*7  SYSTXT(2)
      CHARACTER*6  MXNFIL,MXNINT,MXNSAT,MXNQ1,MXNELE,MXNARC,MXNFRQ
      CHARACTER*6  MXNECL,MXNCHG,MXNVAR
      CHARACTER*6  FSTATUS,FPOSITION
      CHARACTER*4  JSATSTR
c     CHARACTER*2  ELETYP
      CHARACTER*1  YN(2),SOURCE(10),RESFLG
      CHARACTER*1  ARCFGT(MAXARN),ARCSYS(MAXARN),ARCFLG(MAXELE,MAXARN)
      CHARACTER*1  MODFLG
      CHARACTER(LEN=staNam2Length),DIMENSION(2,MAXSAA) :: SATNAM
      CHARACTER(LEN=chrValLength) :: chrVal, subMod

      LOGICAL FEXIST
C
      TYPE(t_mooecl)   MOOECL
      TYPE(t_reshead)  reshed
      TYPE(t_orbmodel) orbdsci

C Function
C --------
      INTEGER*4 listi4

C COMMON BLOCKS
C -------------
      COMMON/LARGE/ YCOE,SOL,TB12,TB12S,Z01,ZCOE,
     1              ARCSAT,ARCELE,ARCSIG,ARCPSD,XBNDS
      COMMON/POTCOE/CPOT,SPOT,NTERM,IPOTNM,IZTID
      COMMON/RPRESS/RPRMOM(21),SCALPA(21),TOSC,NUMSVN,ISHUSE
      COMMON/ALBMDF/ANTTHR,ERPMOD
      COMMON/STCACC/INTMOM,NSTMOM,ISTMOM,NSCMOM,
     1              PSCMOM,TSTMOM,FCTMOM
      COMMON/TORIGO/T0ARC
      COMMON/ORBSYS/IORSYS
      COMMON/MCMFIL/MXCFIL,MXNFIL
      COMMON/MCMINT/MXCINT,MXNINT
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMQ1/MXCQ1,MXNQ1
      COMMON/MCMELE/MXCELE,MXNELE
      COMMON/MCMARC/MXCARC,MXNARC
      COMMON/MCMFRQ/MXCFRQ,MXNFRQ
      COMMON/MCMECL/MXCECL,MXNECL
      COMMON/MCMCHG/MXCCHG,MXNCHG
      COMMON/MCMVAR/MXCVAR,MXNVAR
C
C DATA STATEMENT
C --------------
      DATA YN/'Y','N'/, RESFLG/' '/
      DATA SYSTXT/'B1950.0','J2000.0'/
      DATA IFMT/2/
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C INITIALIZE COMMON BLOCKS FOR MAXIMAL DIMENSIONS
C -----------------------------------------------
      MXCFIL=MAXFIL
      MXNFIL='MAXFIL'
      MXCQ1=MAXQ1
      MXNQ1='MAXQ1 '
      MXCINT=MAXINT
      MXNINT='MAXINT'
      MXCSAT=MAXSAT
      MXNSAT='MAXSAT'
      MXCELE=MAXELE
      MXNELE='MAXELE'
      MXCARC=MAXARN
      MXNARC='MAXARN'
      MXCFRQ=MAXFRQ
      MXNFRQ='MAXFRQ'
      MXCECL=MAXECL
      MXNECL='MAXECL'
      MXCCHG=MAXCHG
      MXNCHG='MAXCHG'
      MXCVAR=MAXVAR
      MXNVAR='MAXVAR'
C
C NULLIFY POINTERS
C ----------------
      CALL INIT_RESHEAD(resHed)
      CALL INIT_MOOECL(mooEcl)
      CALL INIT_INPKEY(inpKey)
C
C GET THE NAME OF THE INPUT FILE
C ------------------------------
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
C PRINT TITLE
C -----------
      CALL pritit('ORBGEN','Create/update standard orbits')
C
C MAXPAR MUST BE GE MAXVAR
C ------------------------
      IF(MAXPAR.LT.(MAXVAR-6))THEN
        WRITE(LFNERR,5010)
5010    FORMAT(//,' *** PGM ORBGEN: MAXPAR MUST BE GE MAXVAR !',//)
        CALL EXITRC(2)
      END IF
C
C DEFINE SCALING OF PARAMETERS
C ----------------------------
c !!!!!!! scalpa set identically to "1".
c !!!!!!! should be removed from sr shmhdl.
c !!!!!!! should also be removed from sr rprout.
c !!!!!!! scaling factor should be set to "1" there
c !!!!!!! check, however, that there is no effect in GPSEST !!!!!
c !!!!!!! danger is zero, if the sr getrpr is returning the unscaled partials
      SCALPA(:)=1.D0
c
c newly introduced scale purely for printing purposes (to get correct units)
      SCALPRT(:)=1.d7
      SCALPRT(1)=1
C
C READ ALL INPUT FROM INPUT FILE
C ------------------------------
      CALL rdorbi(NARC  ,NUNB  ,LOCQ  ,MODEL ,NPOTMX,IPRRES,IANTOF,
     1            DTPI  ,NITER ,Q     ,ARCFIL,TFL   ,WINDOW,INDTIM,
     2            DTPIV ,QVAR  ,ADDVAR,RPRFRM,GENUPD,IUPD  ,ORBMOD,
     3            MODFLG,CMCYN ,ITIM  ,NFIL  ,TABFIL,PREFIL,STCOPT,
     4            OTDMIN,OTDDEG,ANTTHR,ERPMOD )
C
C Re-define Scaling for along-track
C ---------------------------------
      IF(ORBMOD(6).EQ.2)  SCALPRT(8)=1.D10
C
C READ ALL TABULAR ORBIT FILENAMES
C --------------------------------
      IF (GENUPD.EQ.1) THEN
        DO IFIL=1,NFIL
          CALL GTTABHED(TABFIL(IFIL),cmcmod=cmcmod,cmcyn=cmcyn)
          CALL chkcmc(cmcyn,cmcmod)
        ENDDO
      ELSEIF (GENUPD.EQ.2) THEN
        CONTINUE
      ELSE
        cmcyn=.FALSE.
        NFIL=0
      ENDIF
C
C GET NAMES OF NUTATION AND SUBDAILY MODEL
C ----------------------------------------
      CALL RDNUTSUB(NUTNAM,SUBNAM)
C
C DEFINE ARRAY LOCINT FOR NUMERICAL INTEGRATION
C ---------------------------------------------
      CALL DEFLCQ(MODEL,NUNB,LOCQ,ADDVAR,NVAR,LOCINT,INDLOC)
C
C GET SATELLITE ANTENNA OFFSETS
C -----------------------------
      IF ((GENUPD.EQ.1.OR.GENUPD.EQ.2).AND.IANTOF.EQ.1) THEN
        CALL GTSATA(MAXSAA,NSAANT,SATANT,ANTOFF,TIMINT,SATNAM,SATBLK,
     1                                                         SENNUM)
      ENDIF
C
C READ CNM,SNM
C ------------
      CALL getModKey(mod_orb_subMod,subMod,srnGet,numVal)
      chrVal = ' '
! IERS2010 conventions
      IF (subMod(1:8) == 'IERS2010') THEN
        chrVal(1:8)='IERS2010'
        orbMod(4)=4
        mPol = 3
! IERS2000/IERS2003 conventions (Version 5.1)
      ELSEIF (subMod(1:8) == 'IERS2000') THEN
        chrVal(1:8)='IERS2003'
        orbMod(4)=3
        mPol = 2
! IERS1996 conventions (Version 5.0)
      ELSE
        chrVal(1:2)='NO'
        orbMod(4)=2
        mPol = 1
      ENDIF
      CALL setModKey(mod_orb_meaPol,chrVal,pgName,mPol*1.D0)

      CALL GETPOT(NPOTMX,TFL(1,1),MPOL,NTERM,TITPOT,GM,AE,CPOT,SPOT,
     1            IPOTNM,IZTID)
      POTNAM=TITPOT(3:18)
      chrVal=' '
      chrVal(1:16)=potnam
      CALL setModKey(mod_orb_ethPot,chrVal,pgName,npotmx*1.D0)
C
C CHECK IF CORRECT GRAVITY FIELD IS USED
C --------------------------------------
C CHECKED IN ORBMDNAM
      IF (ORBMOD(2).EQ.0) THEN
        IF (TITPOT(3:3).NE.'G') THEN
          WRITE(LFNERR,902)'GEM-T3N',TITPOT(3:9),ORBMOD(2)
902       FORMAT(/,' *** PG ORBGEN: WRONG EARTH GRAVITY FIELD USED',/,
     1                         16X,'SHOULD USE GRAVITY FIELD: ',A7,/,
     2                         16X,'GRAVITY FIELD FOUND     : ',A7,/,
     3                         16X,'ORBMOD(2) : ',I4,/)
          CALL EXITRC(2)
        ENDIF
      ELSEIF (ORBMOD(2).EQ.1 .AND. ORBMOD(1).LT.99) THEN
        IF (TITPOT(3:3).NE.'J') THEN
          WRITE(LFNERR,902)'JGM-3  ',TITPOT(3:9),ORBMOD(2)
          CALL EXITRC(2)
        ENDIF
      ELSEIF (ORBMOD(2).EQ.2 .AND. ORBMOD(1).LT.99) THEN
        ORBMOD(1)=2
        IF (TITPOT(3:3).NE.'E') THEN
          WRITE(LFNERR,902)'EGM96  ',TITPOT(3:9),ORBMOD(2)
          CALL EXITRC(2)
        ENDIF
      ELSEIF (ORBMOD(2).EQ.3 .AND. ORBMOD(1).LT.99) THEN
        ORBMOD(1)=2
        ORBMOD(2)=1
        IF (TITPOT(3:3).NE.'T') THEN
          WRITE(LFNERR,902)'TEG4   ',TITPOT(3:9),ORBMOD(2)
          CALL EXITRC(2)
        ENDIF
      ELSEIF (ORBMOD(2).EQ.4 .AND. ORBMOD(1).LT.99) THEN
        ORBMOD(1)=2
        ORBMOD(2)=1
        IF (TITPOT(3:3).NE.'E'.AND.TITPOT(3:3).NE.'e') THEN
          WRITE(LFNERR,902)'EIGEN1S',TITPOT(3:9),ORBMOD(2)
          CALL EXITRC(2)
        ENDIF
      ENDIF
C
C CHECK WHETHER ROCK4/42 MODELS ARE USED
C --------------------------------------
      CALL GTFLNA(0,'SATELL ',FILNAM,IRC)
      IF (IRC.EQ.0) P0=0.D0
C
C WRITE FILE NAMES AND OPTIONS TO OUTPUT FILE
C -------------------------------------------
      IF (GENUPD .EQ. 1)
     1  CALL DSPRIF(GENUPD,NFIL,TABFIL,NARC,NUNB,NPOTMX,DTPI,
     2            NITER,Q,ARCFIL,TFL,INDTIM,IANTOF,ORBMOD(1))
      IF (GENUPD .EQ. 2)
     1  CALL DSPRIF(GENUPD,NFIL,PREFIL,NARC,NUNB,NPOTMX,DTPI,
     2            NITER,Q,ARCFIL,TFL,INDTIM,IANTOF,ORBMOD(1))
      IF (GENUPD .EQ. 3)
     1  CALL prflna
C
C ORBIT DESCRIPTION
C -----------------
      CALL otidnam(tponam,otidnm)
      CALL orbmdnam(orbmod,dtpi,q,dtpiv,qvar,nutnam,subnam,
     1              potnam,npotmx,tponam,otidnm,otdmin,otddeg,
     2              cmcyn,indtim,antthr,erpmod)
      WRITE(lfnprt,"(/,1X,79('-'),
     1               /,' ORBIT MODEL DESCRIPTION',/,1X,79('-'))")
      DO i=1,orbdsc%nlin
        WRITE(lfnprt,"(1X,A)") TRIM(orbdsc%orbmod(i))
      ENDDO
      WRITE(lfnprt,"(1X,79('-'),/)")
C
C OPEN STANDARD ORBIT OUTPUT FILE (FILENAME = BLANK: NO SAVE)
C -----------------------------------------------------------
      CALL GTFLNA(0,'STDOUT ',FILNAM,IRCORB)
      IF (IRCORB.EQ.0) THEN
        CALL OPNFIL(LFNORB,FILNAM,'UNKNOWN','UNFORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNORB,IOSTAT,FILNAM,'ORBGEN')
      ENDIF
C
C Open residual plot file
C -----------------------
      CALL GTFLNA(0,'RESPLOT',FILPLT,ircpl2)
      IF (ircpl2.EQ.0) THEN
        lfnpl2=LFN002+2
        CALL OPNFIL(lfnpl2,FILPLT,'UNKNOWN','FORMATTED',
     1              ' ',' ',IOSTA2)
        CALL OPNERR(LFNERR,lfnpl2,IOSTA2,FILPLT,'ORBGEN')
      ENDIF
C
C old version (no panel options anymore???)
      IF(GENUPD.EQ.1.OR.GENUPD.EQ.2)THEN
        CALL GTFLNA(0,'PLOTRS ',FILNAM,IRCPLT)

        IF (IRCPLT.EQ.0) THEN
          CALL OPNFIL(LFNPLT,FILNAM,'UNKNOWN','FORMATTED',
     1                ' ',' ',IOSTAT)
          CALL OPNERR(LFNERR,LFNPLT,IOSTAT,FILNAM,'ORBGEN')
C
C WRITE HEADER FOR PLOT FILE (SKELETON KEY: A)
C --------------------------------------------
          CALL GTFLNA(1,'PLOTSKL',FILSKL,IRC)

          CALL WTSKEL('A  ',FILSKL,FIELDS,IPLFLG,LFNPLT)
        ENDIF
C
C OPEN THE RESIDUAL FILE AND THE AUXILIARY FILE
C ---------------------------------------------
        CALL GTFLNA(0,'RESIDRS',FILNAM,IRCRES)
        IF (IRCRES.EQ.0) THEN
          CALL OPNFIL(LFNRES,FILNAM,'UNKNOWN','UNFORMATTED',
     1                ' ',' ',IOSTAT)
          CALL OPNERR(LFNERR,LFNRES,IOSTAT,FILNAM,'ORBGEN')
C
          LFNSCR = LFN002+1
          CALL GTFLNA(1,'AUXFIL ',FILNAM,IRC)
          CALL OPNFIL(LFNSCR,FILNAM,'UNKNOWN','UNFORMATTED',
     1                ' ',' ',IOSTAT)
          CALL OPNERR(LFNERR,LFNSCR,IOSTAT,FILNAM,'ORBGEN')
        END IF
C
C INITIALIZE NUMBER OF "NEW" ARCS
        IARCNW=0
C
C GET BAD SATELLITES
C ------------------
        CALL GTFLNA(1,'SATCRUX',FILNAM,IRC)
        CALL GTSATB(MAXBAD,FILNAM,NBAD,SATBAD,IOBBAD,IACBAD,TIMBAD)
C
C GET SATELLITE MANOEUVRES
C ------------------------
        CALL GTSATM(MAXMAN,NMAN,SATMAN,TIMMAN)
      END IF

C
C MAIN LOOP: COMPUTE ALL THE ARCS REQUIRED:
C =========================================
C
      DO 1000 IARC=1,NARC
C
C INITIALIZE NUMBER OF ECLIPSING SATELLITES
        NSACHG=0
        IPRCHS=1
        mooecl%nsat=0
C
C INITIALIZE ECLIPSE FLAG AND DAY OF YEAR ARRAY
        DO ISAT=1,MAXSAT
          IECLIP(IARC,ISAT)=0
        ENDDO
C
        DO IFIL=1,MAXFIL
          IDASUM(IARC,IFIL)=0
        ENDDO
C
C CHECK MAXIMUM POL. DEGREE
C -------------------------
        IF (Q+1.GT.MAXQ1) Q=MAXQ1-1
        QP1=Q+1
C
C FIRST AND LAST EPOCH
C --------------------
        TB1=TFL(1,IARC)
        TB2=TFL(2,IARC)
C
C PREPARE TABLE FOR SUN, MOON, PRE, NUT, XPOL, YPOL, UT1UTC, GPSUTC
C -----------------------------------------------------------------
        CALL MOSUPN(0.D0,0,TB1,TB2,0.5D0,XDUM,YDUM,UT1UTC,GPSUTC,
     1              DUMPRE,DUMNUT,DUMSUN,DUMMON,DUMMY,DUMVEL)
C
C DEFINE RELATIVE TIME SCALE
C --------------------------
        T0ARC=TB1
        TB1=TB1-T0ARC
        TB2=TB2-T0ARC
C
C READ ALL ORBIT POSITIONS
C ------------------------
        TSTA=TB1+T0ARC
        IF (iArc == nArc) THEN
          IF (TB2+T0ARC > WINDOW(2)) THEN
            TEND = WINDOW(2)
          ELSE
            TEND = TB2+T0ARC
          ENDIF
        ELSE
          TEND=TB2+T0ARC-0.1D0/86400.D0
        ENDIF
        NSAMPL=0d0
        IF(GENUPD.EQ.1.OR.GENUPD.EQ.2)THEN
          IF(GENUPD.EQ.1)THEN
            CALL SETXYZ(NFIL,TABFIL,ARCFIL(IARC),INDTIM,TSTA,TEND,
     1                  DTABD,NSAFIL,NAVFIL,IRCTOT,SOURCE,NSAMPL)
          ELSEIF(GENUPD.EQ.2)THEN
            CALL STXYZ2(NFIL,PREFIL,ARCFIL(IARC),TSTA,TEND,
     1                  NMAN,SATMAN,TIMMAN,NBAD,SATBAD,TIMBAD,NSAFIL,
     2                  NAVFIL,IRCTOT,SOURCE,CMCYN,INDTIM,NSAMPL)
          END IF
C
C MANOEUVERS
C ----------
          DO 800 IMAN=1,NMAN
            DO 850 ISAT=1,NSAFIL
              IF (SATMAN(IMAN).EQ.NAVFIL(ISAT) .AND.
     1            TIMMAN(IMAN).GT.TFL(1,IARC)  .AND.
     2            TIMMAN(IMAN).LT.TFL(2,IARC)) THEN
C DO NOT SET MANOEUVRE MORE THAN ONCE
                DO 860 IS2=1,NSAFIL
                  IF (NAVFIL(IS2).EQ.SATMAN(IMAN)+50) GOTO 861
860             CONTINUE
                NSAFIL=NSAFIL+1
                NAVFIL(NSAFIL)=NAVFIL(ISAT)+50
861             CONTINUE
              END IF
850         CONTINUE
800       CONTINUE
        END IF
C
C PARTITION OF INTEGRATION INTERVAL
C ---------------------------------
        DTABD=1.D0/86400.D0
        CALL PRTINT(DTPI,TB1,TB2,DTABD,NINT,TB12,DUMMY)
        TOSC=TB12(1)+T0ARC
C
C FIND BOUNDARY VALUE FOR BOUNDARY VALUE PROBLEM FOR ALL SATELLITES
C -----------------------------------------------------------------
        IF(GENUPD.EQ.1.OR.GENUPD.EQ.2)THEN
          ISHUSE=0
          CALL DSBNDS(NSAFIL,NAVFIL,IRCTOT,DTPI,
     1                MAXMAN,NMAN,SATMAN,TIMMAN,
     2                NSAT,NAVNUM,TBNDS,XBNDS)
        ELSE
C
C SATELLITE INFORMATION FROM ELE-FILE
C -----------------------------------
          CALL OSCALL(IARC,IUPD,NINT,T0ARC,TB12,NSAT,NAVNUM,
     1                TOSCC,A,E,XI,XKN,PER,U0,TPER,RPRESS,NSTC,
     2                NSTCEP,FRCTYP,INTSTC,TIMSTC,PARSTC,
     3                SOURCE,IORSYS,ORBDSCI)
C
C CHECK MODEL CONSISTENCY
C -----------------------
          CALL orbmdchk(1,orbdsci,irc)
        END IF
C
C WRITE ORBIT MODEL DESCRIPTION
C -----------------------------
        IF (IRCORB .EQ. 0 .AND. IARC .EQ. 1)THEN
          WRITE(LFNORB) -1
          WRITE(LFNORB) IFMT,NARC
          WRITE(LFNORB) orbdsc%nlin
          DO I=1,orbdsc%nlin
            WRITE(LFNORB) orbdsc%orbmod(I)
          ENDDO
        ENDIF
C
C WRITE TITLE
C -----------
        WRITE(LFNPRT,1001) IARC,SOURCE,SYSTXT(IORSYS)
1001    FORMAT(//,1X,79('*'),/,' PROCESSING ARC NUMBER',I4,
     1            7X,'SOURCE:  ',10A1,7X,'SYSTEM:  ',A,
     2         /,1X,79('*'),//)
C
C SAVE INFORMATION FOR RESIDUAL FILE HEADER
C -----------------------------------------
        NSATEL(IARC) = NSAT
        DO 701 ISATEL=1,NSAT
          NUMSAT(ISATEL,IARC) = NAVNUM(ISATEL)
701     CONTINUE
C
C WRITE CONTROL INFORMATION TO OUTPUT-FILE
C ----------------------------------------
        NINT1=NINT+1
        IF (IRCORB.EQ.0) THEN
          WRITE(LFNORB) NSAT,NINT,Q,(NAVNUM(I),I=1,NSAT),
     1                  (SOURCE(I),I=1,10)
          IF (IORSYS.EQ.1) THEN
            ZERO=0.D0
          ELSE
            ZERO=2.D0
          ENDIF
          WRITE(LFNORB) TB12(1)+T0ARC,ZERO
          DO 171 I=1,NINT1
            WRITE(LFNORB) TB12(I) + T0ARC
171       CONTINUE
        ENDIF
C
C INITIALIZE PROCESSING FOR EACH SATELLITE
C ----------------------------------------
C
C A.) Initialize estimation of stochastic parameters
        CALL stcogn_init(stcopt,nsat,navnum,t0arc,nint,tb12,nunb,
     1                   nman,satman,timman,nsampl(2))
C
C
C B.) ANTENNA OFFSETS
        DO 60 ISAT=1,NSAT
          NUMSVN=NAVNUM(ISAT)
          ANTEST(ISAT)=0.D0
C
C C.) RADIATION PRESSURE COEFFICIENTS (estimated parameters)
          IF(GENUPD.EQ.1.OR.GENUPD.EQ.2)THEN
            RPRESS(1,ISAT)=P0
            DO 53 K=2,MAXVAR
              RPRESS(K,ISAT)=0.D0
53          CONTINUE
            DO 54 K=1,MAXVAR
              RPRMOM(K)=RPRESS(K,ISAT)
54          CONTINUE
C
C D.) SOLVE BOUNDARY VALUE PROBLEM TO GET INITIAL CONDITIONS AT START
C     OF ARC AND OSCULATING ELEMENTS
            CALL DSBPRB(TBNDS(1,ISAT),XBNDS(1,1,ISAT),TB12(1),DTPI,
     1                  DTABD,Q,ORBMOD,NUMSVN,FAC,YCOE(1,1,ISAT),
     2                  XVSAVE(1,1,ISAT),A(ISAT),E(ISAT),XI(ISAT),
     3                  XKN(ISAT),PER(ISAT),TPER(ISAT),TB12S)
            CALL ELETRA(1,A(ISAT),E(ISAT),PER(ISAT),0.D0,TPER(ISAT),
     1                  U0(ISAT))
          END IF
          ELE(1,ISAT)=A(ISAT)
          ELE(2,ISAT)=E(ISAT)
          ELE(3,ISAT)=XI(ISAT)
          ELE(4,ISAT)=XKN(ISAT)
          ELE(5,ISAT)=PER(ISAT)
          ELE(6,ISAT)=U0(ISAT)
          ELE(7,ISAT)=TPER(ISAT)
60      CONTINUE
C
C ORBIT DEFINITION IN NITER STEPS
C -------------------------------
        DO 400 ITER=1,NITER
C
C REWIND FILE WITH SAT. POSITIONS
          IF(GENUPD.EQ.1.OR.GENUPD.EQ.2)REWIND LFN001
          ISAVEX=0
C
C INITIALIZE RESIDUAL (O-C) STATISTICS
          DO IFIL=1,NFIL
            DO ISAT=1,NSAT
              NUMSUM(IARC,IFIL,ISAT)=0
              RMSSUM(IARC,IFIL,ISAT)=0.D0
            ENDDO
          ENDDO
C
C COMPUTE NEW INITIAL CONDITIONS AT STARTING TIME
C INITIALIZE NORMAL EQUATION SYSTEM
C WRITE A TITLE ON PLOT-FILE
C -----------------------------------------------
          DO 70 ISAT=1,NSAT
            CALL stcogn_inipar(iter,maxvar,isat,locq,ele(:,isat),
     1                         rpress(:,isat))
            NUMSVN=NAVNUM(ISAT)
C
C NORMAL EQUATION SYSTEMS
            IF(GENUPD.EQ.1.OR.GENUPD.EQ.2)THEN
              RMS(ISAT)=0.D0
C
C INITIAL VALUES FOR RPR PARTIALS, RMS(R.A.O.) AND MAX.RESIDUALS
              DO 561 K=1,3
                RMSRAO(K,ISAT)=0.D0
                RESMAX(K,ISAT)=0.D0
561           CONTINUE
            END IF
C
            CALL RVPDER(ITER,NUMSVN,GM,0.D0,0.D0,A(ISAT),E(ISAT),
     1                  XI(ISAT),XKN(ISAT),PER(ISAT),TPER(ISAT),
     2                  DRDELE,DVDELE)
C
            DO 57 I=1,NVAR
              DO 57 L=1,2
              DO 57 K=1,3
                  KIL=K+(I-1)*3+(L-1)*3*NVAR
                  IF(MODEL.NE.3.OR.I.GT.6)THEN
                    Z01(KIL,ISAT)=0.D0
                  ELSE
                    IF(L.EQ.1)THEN
                      Z01(KIL,ISAT)=DRDELE(K,I)/SCALPA(I)
                    ELSE
                      Z01(KIL,ISAT)=DVDELE(K,I)/SCALPA(I)
                    END IF
                  END IF
57          CONTINUE
C
C INITIAL VALUES FOR PRIMARY EQUATIONS
            CALL EPHEM(GM,A(ISAT),E(ISAT),XI(ISAT),XKN(ISAT),
     1                 PER(ISAT),TPER(ISAT),
     2                 0.D0,XVSAVE(1,1,ISAT),XVSAVE(1,2,ISAT))
70        CONTINUE
C
C WRITE TITLE OF RESIDUAL TABLE
C -----------------------------
          IF(GENUPD.EQ.1.OR.GENUPD.EQ.2)THEN
            IF (IPRRES.EQ.-1 .OR. IPRRES.EQ.ITER) THEN
              WRITE(LFNPRT,1262) IARC,ITER
1262          FORMAT(' ',79('-'),/,' RESIDUALS (M)',20X,'ARC NUMBER:',
     1               I3,19X,'ITERATION:',I3,/,' ',79('-'),//,
     2               ' DATE AND TIME        SVN     RADIAL    A. TRACK',
     3               '   OUT PLA.     INT.ERROR   SUN',/,' ',19('-'),2X,
     4               3('-'),4X,3(8('-'),3X),4X,6('-'),5X,'-',/)
            ENDIF
          END IF
C
C LOOP OVER ALL SUB-INTERVALS
C ---------------------------
          NEW=1
          IF(GENUPD.EQ.1.OR.GENUPD.EQ.2)THEN
            IEOF=0
          ELSE
            IEOF=1
          END IF
C
          DO 300 INT=1,NINT
C
C SOLVE INITIAL VALUE PROBLEMS (ALL SATELLITE, INCL. RPR. COEFF.)
C ---------------------------------------------------------------
            DO 80 ISAT=1,NSAT
              NUMSVN=NAVNUM(ISAT)
C
C DETECT WHETHER OR NOT SATELLITE IS IN SUNLIGHT AT START OF INTERVAL
C -------------------------------------------------------------------
              TTT=TB12(INT)+T0ARC
              CALL MOSUPN(TTT,2,DUM,DUM,DUM,XDUM,YDUM,DUM,DUM,
     1                    DUMPRE,DUMNUT,XSUN,DUMMON,DUMMY,DUMVEL)
              CALL SHADOW(XVSAVE(1,1,ISAT),XSUN,ISHUSE)
C
C NUMERICAL INTEGRATION FOR CURRENT SUBINTERVAL
C ---------------------------------------------
              DO 75 K=1,MAXVAR
                RPRMOM(K)=RPRESS(K,ISAT)
75            CONTINUE
C
C STOCH. ACC. INFO FOR CURRENT SUBINTERVAL
C ----------------------------------------
              INTMOM=INT
              IF(GENUPD.EQ.3)THEN
                INTMOM=INT
                NSTMOM=NSTC(ISAT)
                DO 78 K=1,NSTMOM
                  ISTMOM(K)=INTSTC(K,ISAT)
                  TMIN=1000000.D0
                  DO 76 L=1,NINT+1
                    TEST=DABS(TIMSTC(K,ISAT)-(TB12(L)+T0ARC))
                    IF (TEST.LT.TMIN) THEN
                      LMIN=L
                      TMIN=TEST
                    END IF
76                CONTINUE
                  TSTMOM(K)=TB12(LMIN)+T0ARC
                  NSCMOM(K)=NSTCEP(K,ISAT)
                  DO 77 I=1,3
                    PSCMOM(I,K)=PARSTC(I,K,ISAT)
                    FCTMOM(I,K)=FRCTYP(I,K,ISAT)
77                CONTINUE
78              CONTINUE
              ELSE
                DO 79 K=1,MAXINT
                  DO 79 I=1,3
                    FCTMOM(I,K)=0
79              CONTINUE
              END IF
C
C ADD STOCHASTIC PULSES IN UPDATE-MODE:
C ------------------------------------
              IF(GENUPD.EQ.3)THEN
                CALL APLSTC(NSTC(ISAT),INT,INTSTC(1,ISAT),
     1                      NSTCEP(1,ISAT),PARSTC(1,1,ISAT),
     2                      FRCTYP(1,1,ISAT),XSUN,XVSAVE(1,1,ISAT),
     3                      XVSAVE(1,2,ISAT))
C
C ADD STOCHASTIC PULSES FROM ORBGEN:
C ---------------------------------
              ELSE
                CALL stcogn_applyVel(isat,tb12(int),xvsave)
              END IF
C
C INTEGRATION IN CURRENT INTERVAL:
C -------------------------------
              CALL INTFST(0,ITIM,TB12(INT),TB12(INT+1),XVSAVE(1,1,ISAT),
     1                    Q,NVAR,LOCINT,Z01(1,ISAT),ORBMOD,NUMSVN,T0,H,
     2                    YCOE(1,1,ISAT),ZCOE(1,ISAT),ERRMAX)
C
C IN LAST ITERATION STEP : COMPUTE AND STORE OSCULATING ELEMENTS
C --------------------------------------------------------------
              IF (NEW.EQ.1.AND.IRCORB.EQ.0.AND.ITER.EQ.NITER) THEN
                WRITE(LFNORB) A(ISAT),E(ISAT),XI(ISAT),
     1                        XKN(ISAT),PER(ISAT),U0(ISAT),TPER(ISAT)
              END IF
C
C HANDLE LIGHT/SHADOW TRANSITS PROPERLY
C -------------------------------------
              CALL SHDHDL(ITER,NITER,IORSYS,T0ARC,TB12,T0,H,Q,NVAR,FAC,
     1                    LOCINT,ISHUSE,MAXCHG,INT,NUMSVN,ORBMOD,NSACHG,
     2                    SATCHG,NUMCHG,CHGTIM,IPRCHS,IPRCHG,
     3                    YCOE(1,1,ISAT),ZCOE(1,ISAT))
C
C HANDLE TRANSITS THROUGH SHADOW OF MOON
C --------------------------------------
              npoint=100
              CALL shmhdl(iter,niter,iorsys,t0arc,tb12,t0,h,q,nvar,fac,
     1                    locint,int,numsvn,orbmod,rprmom,scalpa,npoint,
     2                    ycoe(1,1,isat),zcoe(1,isat),mooecl)
C
C HANDLE SINGULARITIES DUE TO RAPID ROTATION OF SATELLITE
C -------------------------------------------------------
              CALL SNGHDL(IORSYS,T0ARC,TB12,T0,H,Q,NVAR,FAC,
     1                    LOCINT,INT,XVSAVE(1,1,ISAT),Z01(1,ISAT),
     2                    ORBMOD,NUMSVN,YCOE(1,1,ISAT),ZCOE(1,ISAT))
80          CONTINUE
C
            NEW=0
C
C LOOP OVER ALL OBSERVATION-EPOCHS OF SUB-INTERVAL
C ------------------------------------------------
            IF (IEOF.EQ.1) GOTO 201
            DO 200 IEP=1,IRCTOT
C
              IF (ISAVEX.EQ.1) THEN
                SVNFIL=SVNSAV
                TOBS  =TOBSAV
                DO 205 ICOR=1,3
                  XTF(ICOR)=XTFSAV(ICOR)
205             CONTINUE
                ISAVEX=0
              ELSE
                READ(LFN001,END=134) IFIL,SVNFIL,TOBS,(XTF(K),K=1,3)
                DO 25 IMAN=1,NMAN
                  IF (SVNFIL.EQ.SATMAN(IMAN)     .AND.
     1                TOBS+T0ARC.GE.TIMMAN(IMAN) .AND.
     2                T0ARC.LT.TIMMAN(IMAN))     SVNFIL=SVNFIL+50
25              CONTINUE
C
                SVNSAV=SVNFIL
                TOBSAV=TOBS
                DO 208 ICOR=1,3
                  XTFSAV(ICOR)=XTF(ICOR)
208             CONTINUE
              ENDIF
C
              DO 132 ISAT=1,NSAT
                IF (SVNFIL.EQ.NAVNUM(ISAT)) GOTO 133
132           CONTINUE
              GOTO 200
133           CONTINUE
C R*16 PROBLEM   IF (TOBS.LT.-1.D-14) GOTO 134
              IF (TOBS.LT.-1.D-11) GOTO 134
              IF (TOBS.GT.TB12(INT+1)) THEN
                ISAVEX=1
                GOTO 201
              END IF
              GOTO 135
134           IEOF=1
              GOTO 201
135           CONTINUE
C
C TOBS : TIME ARGUMENT IN MJD - T0ARC
C TSEC : TIME ARGUMENT IN SEC FOR NUMERICAL INTEGRATION
C TSCOSC : TIME ARGUMENT IN SEC FOR PARTIAL DERIVATIVES (SR RPART)
C TPRINT:TIME ARGUMENT FOR PRINT IN MINUTES
              TSEC=(TOBS-T0)*86400
              TSCOSC=(TOBS-TB12(1))*86400
              TPRINT=(TOBS-TB12(1))*1440
C
C COMPUTE POSITION AND VELOCITY OF SATELLITE AT TIME TSEC
C -------------------------------------------------------
              CALL YPOL(1,Q,3,H,FAC,TSEC,YCOE(1,1,ISAT),XSAT)
              RSAT=DSQRT(XSAT(1)**2+XSAT(2)**2+XSAT(3)**2)
C
C DETECT WHETHER OR NOT SATELLITE IS IN SUNLIGHT (TIME: TDB, HERE TDT)
              TTT=TOBS+T0ARC
              CALL MOSUPN(TTT,2,DUM,DUM,DUM,XDUM,YDUM,DUM,DUM,
     1                    DUMPRE,DUMNUT,XSUN,DUMMON,DUMMY,DUMVEL)
              CALL SHADOW(XSAT,XSUN,INDSUN)
              CALL MOSUPN(TTT,3,DUM,DUM,DUM,DUM,DUM,DUM,DUM,
     1                    DUMPRE,DUMNUT,DUMSUN,XMOON,DUMMY,DUMVEL)
              CALL ECLMOON(XSAT,XMOON,XSUN,INDMON,DUM,XDUM,YDUM)
C
C SATELLITE ANTENNA OFFSET CORRECTIONS
C ------------------------------------
              IF (IANTOF.EQ.1) THEN
                DO 162 ISAANT=1,NSAANT
                  IF (SATANT(ISAANT).EQ. MODSVN(SVNFIL).AND.
     1               TOBS+T0ARC.GE.TIMINT(1,ISAANT) .AND.
     2               TOBS+T0ARC.LE.TIMINT(2,ISAANT)) GOTO 163
162             CONTINUE
C
C SATELLITE ANTENNA OFFSET NOT FOUND
                CALL TIMST2(1,1,TOBS+T0ARC,STRING)
                WRITE(LFNERR,1006) MODSVN(SVNFIL),STRING
1006            FORMAT(/,' *** PG ORBGEN: SATELLITE ANTENNA OFFSET',
     1                   ' NOT FOUND',/,
     2                 16X,'IN SATELLITE INFORMATION FILE',/,
     3                 16X,'SATELLITE NUMBER:',I4,/,
     4                 16X,'EPOCH           : ',A20,/)

                CALL EXITRC(2)
163             CONTINUE
C
C UNIT VECTOR EZ
                DO 164 K=1,3
                  EZ(K)=-XSAT(K)/RSAT
164             CONTINUE
C
C UNIT VECTOR EY
                CALL VPROD(EZ,XSUN,EY)
                REY=DSQRT(EY(1)**2+EY(2)**2+EY(3)**2)
                DO 165 K=1,3
                  EY(K)=EY(K)/REY
165             CONTINUE
C
C UNIT VECTOR EX
                CALL VPROD(EY,EZ,EX)
C
C ANTENNA OFFSET CORRECTION
                DO 161 K=1,3
                  XTF(K)=XTF(K)-EX(K)*ANTOFF(1,ISAANT)
     1                         -EY(K)*ANTOFF(2,ISAANT)
     2                         -EZ(K)*ANTOFF(3,ISAANT)
161             CONTINUE
              ENDIF
C
C RESIDUALS
C ---------
              DO 140 K=1,3
                X(K)=XSAT(K)-XTF(K)*(1-ANTEST(ISAT)/RSAT)
140           CONTINUE
C
C COMPUTE PARTIALS
C 1. WITH RESPECT TO ORBITAL ELEMENTS
C -----------------------------------
              ISTART=1
              IF(MODEL.NE.3)THEN
                CALL RPARTN(ITER,NAVNUM(ISAT),GM,TSCOSC,0.D0,
     1                      A(ISAT),E(ISAT),XI(ISAT),XKN(ISAT),
     2                      PER(ISAT),TPER(ISAT),DRDHLP)
                ISTART=7
              END IF
C
C 2. WITH RESPECT TO RADIATION PRESSURE PARAMETERS
C ------------------------------------------------
              IF (NUNB.GE.7.OR.MODEL.EQ.3) THEN
C
C COMPUTE PARTIAL OF ORBIT WITH RESPECT TO RADIATION PRESSURE PARAMETERS
                CALL YPOL(0,Q,3*NVAR,H,FAC,TSEC,ZCOE(1,ISAT),
     1               DRDHLP(1,ISTART))
              END IF
C
C REDUCE TO ACTUALLY ESTIMATED SUBSET OF UNKNOWNS
              DO 1441 I=1,NUNB
                DO 1441 K=1,3
                  IF(I.LE.6)THEN
                    DRDELE(K,I)=DRDHLP(K,I)
                  ELSE
                    IF(MODEL.LT.3)THEN
                      IND=6+INDLOC(I)
                    ELSE
                      IND=INDLOC(I)
                    END IF
                    DRDELE(K,I)=DRDHLP(K,IND)
                  END IF
1441          CONTINUE
C
C Apply corrections due to stochastic parameters
C ----------------------------------------------
              IF(iter > 1)THEN
                call stcogn_applyPos(isat,tobs,drdele, drv)
                x(1:3) = x(1:3) + drv(1:3)
              ENDIF
C
              CALL RESORB(X,XSAT(1),XSAT(4),GURTNR)
C
C PRINT "COMPUTED-OBSERVED" VALUES
C --------------------------------
              IF (IPRRES.EQ.-1 .OR. IPRRES.EQ.ITER) THEN
                CALL TIMST2(1,1,TOBS+T0ARC,TSTRNG)
                WRITE(LFNPRT,145) TSTRNG,NAVNUM(ISAT),
     1                            (GURTNR(K),K=1,3),ERRMAX,YN(INDSUN+1)
145             FORMAT(1X,A19,I5,1X,3F11.5,F13.6,5X,A1)
cc145             FORMAT(1X,A19,I5,1X,3F11.3,F13.4,5X,A1)
              ENDIF
C
C SUM STATISTICS FOR ALL FILES AND SATELLITES
C -------------------------------------------
              DO ICOR=1,3
                NUMSUM(IARC,IFIL,ISAT)=NUMSUM(IARC,IFIL,ISAT)+1
                RMSSUM(IARC,IFIL,ISAT)=RMSSUM(IARC,IFIL,ISAT)+
     1                                 GURTNR(ICOR)**2
              ENDDO

              IF(IECLIP(IARC,ISAT).NE.3) THEN
                IF(INDSUN.EQ.1) THEN
                  IF(IECLIP(IARC,ISAT).EQ.2) THEN
                    IECLIP(IARC,ISAT) = 3
                  ELSE
                    IECLIP(IARC,ISAT) = 1
                  ENDIF
                ENDIF
                IF(INDMON.NE.1d0) THEN
                  IF(IECLIP(IARC,ISAT).EQ.1) THEN
                    IECLIP(IARC,ISAT) = 3
                  ELSE
                    IECLIP(IARC,ISAT) = 2
                  ENDIF
                ENDIF
              ENDIF

              IF(IDASUM(IARC,IFIL).EQ.0) THEN
                CALL JMT(TOBS+T0ARC,IYEAR,IMONTH,DAY)
                T0YEAR=DJUL(IYEAR,1,1.D0)
                IDASUM(IARC,IFIL)=IDINT(TOBS+T0ARC-T0YEAR+1.D0)
              ENDIF
C
C SAVE TIME PLUS RESIDUALS IF REQUIRED (PLOT FILE)
C ------------------------------------------------
              IF (IRCPLT.EQ.0) THEN
                DO 1452 K=1,3
                  WRITE(LFNPLT,1451) IARC,ITER,SVNFIL,K,TPRINT,
     1                               GURTNR(K)
1451              FORMAT(I4,I3,I3,I2,F12.2,F12.4)
1452            CONTINUE
              ENDIF
C
C WRITE THE RESIDUALS INTO THE SCRATCH FILE
C -----------------------------------------
              IF (IRCRES.EQ.0.AND.ITER.EQ.NITER) THEN
                IEPOCH = IDNINT(TSCOSC/1000.D0+1)
                DO 703 IFRQ=1,3
                  WRITE(LFNSCR) IARC,IEPOCH,IFRQ,
     1                          NAVNUM(ISAT),GURTNR(IFRQ),RESFLG
703             CONTINUE
              ENDIF
C
C Write orbit residuals in PLT file
C ---------------------------------
              IF (ircpl2.EQ.0.AND.ITER.EQ.NITER) THEN
                CALL TIMSTR(1,(/TOBS+T0ARC/),TSTRNG)
                WRITE(lfnpl2,1453) ITER,TOBS+T0ARC,TSTRNG,NAVNUM(ISAT),
     1                             (GURTNR(K),K=1,3)
1453            FORMAT(1X,I3,1X,F13.5,3X,A17,1X,I5,3(1X,F10.4))
              ENDIF

C COMPLETE NORMAL-EQUATION SYSTEM
              DANOR=0D0
              DBNOR=0D0
              DRMS=0D0
              DO L=1,3
                RMS(ISAT)=RMS(ISAT)+X(L)**2
                RMSRAO(L,ISAT)=RMSRAO(L,ISAT)+GURTNR(L)**2
                DRMS=DRMS+X(L)**2
                IF(DABS(GURTNR(L)).GT.RESMAX(L,ISAT))
     1            RESMAX(L,ISAT)=DABS(GURTNR(L))
                DO II=1,NUNB
                  BBB_cor=X(L)*DRDELE(L,II)
                  DBNOR(II)=DBNOR(II)-BBB_cor
                  DO K=II,NUNB
                    IK=II+(K-1)*NUNB
                    IIKK=II+(K-1)*K/2
                    AAA_cor=DRDELE(L,II)*DRDELE(L,K)
                    DANOR(IIKK)=DANOR(IIKK)+AAA_cor
                  ENDDO
                ENDDO
              ENDDO
C
C Store NEQ for stochastic parameters
              CALL stcogn_update(isat,tobs,drms,danor,dbnor)
C
200         CONTINUE
C
C END OF LOOP OVER ALL EPOCHS
C ---------------------------
201         CONTINUE
            IF((GENUPD.EQ.1.OR.GENUPD.EQ.2).AND.
     1          ITER.NE.NITER.AND.IEOF.EQ.1)GO TO 301
C
C IN LAST ITERATION STEP SAVE POLYNOMIAL COEFFICIENTS
C (IF REQUIRED)
C ---------------------------------------------------
            IF (ITER.EQ.NITER) THEN
C
C WRITE ORBIT COEFFICIENTS
              IF (IRCORB.EQ.0) THEN
                WRITE(LFNORB) T0+T0ARC,H
                DO 210 I=1,QP1
                  WRITE(LFNORB) ((YCOE(K,I,ISAT),K=1,3),ISAT=1,NSAT)
210             CONTINUE
              ENDIF
C
C WRITE RPR COEFFICIENTS AND RELATED INFORMATION ON RPR FILE
C ----------------------------------------------------------
              CALL GTFLNA(0,'RPROUT ',FILNAM,IRCRPR)
              IF (IRCRPR.EQ.0) THEN
                TA=TB12(1)+T0ARC
                TB=TB12(NINT1)+T0ARC
                TLEFT=TB12(INT)+T0ARC
                TRIGHT=TB12(INT+1)+T0ARC
                IF(GENUPD.EQ.1.OR.GENUPD.EQ.2) THEN
                  DO 230 ISAT=1,MAXSAT
                    NSTC(ISAT)=0
230               CONTINUE
                ENDIF
                CALL RPROUT(NARC,IARC,QVAR,DTPIV,TA,TB,NSAT,NAVNUM,
     1                      SOURCE,IORSYS,NVAR,LOCINT,ELE,RPRESS,
     2                      SCALPA,TLEFT,TRIGHT,Q,T0+T0ARC,H,ZCOE,
     3                      FAC,ZSAT,ORBMOD,MODFLG,RPRFRM,NSTC,
     4                      FRCTYP,NSTCEP,INTSTC,TIMSTC,PARSTC)
              ENDIF
            ENDIF
C
            IF (INT.EQ.NINT) GOTO 291
C
C INITIAL CONDITIONS FOR NEXT SUBINTERVAL
C (ALL SATELLITES, RPR COEFFICIENTS INCLUDED)
C -------------------------------------------
            TSEC=(TB12(INT+1)-T0)*86400.D0
            DO 290 ISAT=1,NSAT
              CALL YPOL(1,Q,3,H,FAC,TSEC,YCOE(1,1,ISAT),XSAT)
              DO 260 K=1,3
                XVSAVE(K,1,ISAT)=XSAT(K)
                XVSAVE(K,2,ISAT)=XSAT(3+K)
260           CONTINUE
              CALL YPOL(1,Q,3*NVAR,H,FAC,TSEC,ZCOE(1,ISAT),ZSAT)
              DO 270 I=1,3
                DO 270 K=1,NVAR
                  IKL=I+(K-1)*3
                  Z01(IKL,ISAT)=ZSAT((K-1)*3+I)
                  L=2
                  IKL=I+(K-1)*3+(L-1)*3*NVAR
                  Z01(IKL,ISAT)=ZSAT(3*NVAR+(K-1)*3+I)
270           CONTINUE
290         CONTINUE
291       CONTINUE
c
c Update stochastic pulses
c ------------------------
          DO isat=1,nsat
            call stcogn_beta(maxq1,navnum(isat),tb12(int),tb12(int+1),
     1                       q,h,nvar,fac,ycoe,zcoe)
          ENDDO
300       CONTINUE
301       CONTINUE
C
C END OF LOOP OVER ALL SUBINTERVALS
C ---------------------------------
          IF(GENUPD.EQ.3)THEN
C
C WRITE LIGHT/SHADOW PASSAGES FOR UPDATE-STEP
C -------------------------------------------
            CALL SHDPRI(NSACHG,SATCHG,NUMCHG,CHGTIM,
     1                  IPRCHS,IPRCHG,IARC,TFL(1,IARC),IDXCHG,mooecl)
            GO TO 999
          END IF
C
          IF (IPRRES.EQ.-1 .OR. IPRRES.EQ.ITER) THEN
            WRITE(LFNPRT,1011)
1011        FORMAT(/,' ',79('-'),//)
          ENDIF
C
C Alternative solution of neq-system
C ----------------------------------
          CALL stcogn_solve(iter, locq, ele, rpress)
C
C ORDER SATELLITES
C ----------------
          CALL IORDUP(NAVNUM,NSAT,IDXSAT)
C
C SOLVE NORMAL EQUATION SYSTEMS FOR ALL SATELLITES
C ------------------------------------------------
          TMID=(TFL(1,IARC)+TFL(2,IARC))/2.0D0
          DO 390 ISAT=1,NSAT
            JSAT=IDXSAT(ISAT)
C
C From here onward use numobs, sol, rms from module stcogn
C --------------------------------------------------------
            numobs(jsat) = m_nobs_tot(jsat)
            sol(1:nunb,jsat) = m_curpar(1:nunb,jsat)
C
            IF (RMS(JSAT).GE.0.D0.AND.NUMOBS(JSAT).GT.0) THEN
              RMSTOT(JSAT)=DSQRT(RMS(JSAT)/DBLE(NUMOBS(JSAT)))
            ELSE
              RMSTOT(JSAT)=0.D0
            ENDIF
C
            RMS(JSAT)=m_rms_tot(jsat)
C
            DO 311 I=1,3
              IF(RMSRAO(I,JSAT).GE.0.D0.AND.NUMOBS(JSAT).GT.0) THEN
                RMSRAO(I,JSAT)=
     1                 DSQRT(RMSRAO(I,JSAT)*3.D0/DBLE(NUMOBS(JSAT)))
              ELSE
                RMSRAO(I,JSAT)=0.D0
              END IF
311         CONTINUE
            A(JSAT)   = m_curpar(1,JSAT)
            E(JSAT)   = m_curpar(2,JSAT)
            XI(JSAT)  = m_curpar(3,JSAT)
            XKN(JSAT) = m_curpar(4,JSAT)
            PER(JSAT) = m_curpar(5,JSAT)
            U0(JSAT)  = m_curpar(6,JSAT)
            CALL ELETRA(2,A(JSAT),E(JSAT),PER(JSAT),0.D0,TPER(JSAT),
     1                  U0(JSAT))
            ELE(1,JSAT)=A(JSAT)
            ELE(2,JSAT)=E(JSAT)
            ELE(3,JSAT)=XI(JSAT)
            ELE(4,JSAT)=XKN(JSAT)
            ELE(5,JSAT)=PER(JSAT)
            ELE(6,JSAT)=U0(JSAT)
            ELE(7,JSAT)=TPER(JSAT)
C
C PRINT RADIATION PRESSURE PARAMETERS
C -----------------------------------
            DO 320 KK=7,NUNB
              IF(MODEL.NE.3)THEN
                KRPR=INDLOC(KK)
              ELSE
                KRPR=INDLOC(KK)-6
              END IF
              RPRESS(KRPR,JSAT) = m_rpress(krpr,jsat)
              SIGRPR(KRPR,JSAT) = m_rpress_rms(krpr,jsat)
320         CONTINUE
C
C WRITE B0,U0 IN OUTPUT
C ---------------------
            IF (ITIM.EQ.1) THEN
               DUMVEL(1:3)=0D0
               CALL ARGSUN(TMID,XSUN,DUMVEL,XVSAVE(1,1,JSAT),
     1                     NAVNUM(JSAT),BTMP0(JSAT),DUMMY,
     2                                  UTMP0(JSAT),DUMMY)
            ENDIF
C
C SAVE PARAMETERS FOR ORBIT INFO FILE
C -----------------------------------
            IF (ITER.EQ.NITER) THEN
              IARCNW=IARCNW+1
C
              IF (IARCNW.GT.MAXARN) THEN
                WRITE(LFNERR,901) IARCNW,MAXARN,IARC,NAVNUM(JSAT)
901             FORMAT(/,' *** PG ORBGEN: TOO MANY "NEW" SAT. ARCS',/,
     1                               16X,'NUMBER OF "NEW" ARCS >=',I4,/,
     2                               16X,'MAX. NUMBER OF ARCS   :',I4,/,
     3                               16X,'OLD ARC NUMBER        :',I4,/,
     4                               16X,'SATELLITE NUMBER      :',I4,/)
                CALL EXITRC(2)
              ENDIF
C
              WRITE(ARCSAT(IARCNW),315) NAVNUM(JSAT)
315           FORMAT('GPS',I3)
              ARCT00(IARCNW)  = TB12(1)+T0ARC
              ARCINT(1,IARCNW)= TB12(1)+T0ARC
              ARCINT(2,IARCNW)= TB12(NINT+1)+T0ARC
C
              ARCFGT(IARCNW)='D'
              ARCSYS(IARCNW)='I'
C
C SUBTRACT LAST SOLUTION TO GET ELEMENTS CORRESP. TO SAVED STD.ORBIT
C ------------------------------------------------------------------
              ARCELE(1,IARCNW)=m_inipar(1,jsat)
              ARCELE(2,IARCNW)=m_inipar(2,jsat)
              ARCELE(3,IARCNW)=m_inipar(3,jsat)
              ARCELE(4,IARCNW)=m_inipar(4,jsat)
              ARCELE(5,IARCNW)=m_inipar(5,jsat)
              ARCELE(6,IARCNW)=m_inipar(6,jsat)
C
              DO 370 III=7,NUNB
                IF(MODEL.NE.3)THEN
                  IRPR=INDLOC(III)
                ELSE
                  IRPR=INDLOC(III)-6
                END IF
                ARCELE(IRPR+6,IARCNW)=m_RPRESS_INI(IRPR,JSAT)
370           CONTINUE
              DO 380 IELE=1,MAXELE
                IF(IELE.LE.NUNB) THEN
                  ARCFLG(IELE,IARCNW)='D'
                  IEIE=IELE+(IELE-1)*NUNB
                  arcsig(iele,iarcnw) = m_rmspar(iele,jsat)
                ELSE
                  ARCFLG(IELE,IARCNW)=' '
                  ARCSIG(IELE,IARCNW)=0.D0
                ENDIF
                ARCPSD(IELE,IARCNW)=0.D0
380           CONTINUE
            ENDIF
C
390       CONTINUE
C
C WRITE RMS TABLE
C ---------------
          WRITE(LFNPRT,1012) IARC,ITER
1012      FORMAT(' ',79('-'),/,' RMS ERRORS AND MAX. RESIDUALS   ',
     1           'ARC NUMBER:',I3,20X,'ITERATION:',I3,/,' ',79('-'),//,
     2           27X,'QUADRATIC MEAN OF O-C (M)',
     3            9X,'MAX. RESIDUALS (M)',/,
     3       ' SAT   #POS   RMS (M)     TOTAL   RADIAL  ALONG   OUT',
     4       '       RADIAL  ALONG   OUT',/,
     5       ' ---   ----   -------     -----------------------------',
     6       '     --------------------',/)
          DO 391 ISAT=1,NSAT
            JSAT=IDXSAT(ISAT)
            WRITE(LFNPRT,306,IOSTAT=IOS) NAVNUM(JSAT),NUMOBS(JSAT)/3,
     1           RMS(JSAT),RMSTOT(JSAT),
     2           (RMSRAO(K,JSAT),K=1,3),(RESMAX(K,JSAT),K=1,3)
306         FORMAT(I4,I7,F9.3,F11.3,2X,3(F7.3),5X,3(F7.3))
CCC306         FORMAT(I4,I7,F9.2,F11.2,2X,3(F7.2),5X,3(F7.2))
391       CONTINUE
C
C WRITE END LINE OF RMS TABLE
          WRITE(LFNPRT,1011)
C
C WRITE RPR PARAMETERS
C --------------------
C
C Copy info from module stcogn
          RPRESS(1:maxvar,JSAT) = m_rpress(1:maxvar,JSAT)
          SIGRPR(1:maxvar,JSAT) = m_rpress_rms(1:maxvar,JSAT)
C
          IF (NUNB.GT.6) THEN
            I1=0
            I2=0
            I3=0
            NNN=0
            DO 6010 IPAR=7,NUNB
              IF(LOCQ(1,IPAR).EQ.3.AND.LOCQ(4,IPAR).EQ.7)THEN
                I1=1
                IND1=IPAR
                NNN=NNN+1
              END IF
              IF(LOCQ(1,IPAR).EQ.3.AND.LOCQ(4,IPAR).EQ.8)THEN
                I2=2
                IND2=IPAR
                NNN=NNN+1
              END IF
              IF(LOCQ(1,IPAR).EQ.3.AND.LOCQ(4,IPAR).EQ.9)THEN
                I3=3
                IND3=IPAR
                NNN=NNN+1
              END IF
6010        CONTINUE
            IF(NNN.NE.0)THEN
              IF(ORBMOD(6).EQ.0) THEN
                WRITE(LFNPRT,1016) IARC,ITER
1016            FORMAT(' ',79('-'),/,
     1             ' RPR PARAMETERS AND RMS ERRORS   ',
     1             'ARC NUMBER:',I3,20X,'ITERATION:',I3,/,' ',79('-'),/,
     2               ' SAT        D0       RMS        Y0       RMS  ',
     3               '       X0       RMS',/,
     4               '           (M/S**2*1.D-7)      (M/S**2*1.D-7) ',
     5               '      (M/S**2/D*1.D-7) ',/,
     6               ' ---     ------------------   ------------------',
     7               '   ------------------',/)
              ELSE IF(ORBMOD(6).EQ.1) THEN
                WRITE(LFNPRT,1017) IARC,ITER
1017            FORMAT(' ',79('-'),/,
     1             ' RPR PARAMETERS AND RMS ERRORS   ',
     1             'ARC NUMBER:',I3,20X,'ITERATION:',I3,/,' ',79('-'),/,
     2               ' SAT        R0       RMS        S0       RMS  ',
     3               '       W0       RMS',/,
     4               '           (M/S**2*1.D-7)      (M/S**2*1.D-7) ',
     5               '      (M/S**2/D*1.D-7) ',/,
     6               ' ---     ------------------   ------------------',
     7               '   ------------------',/)
              ELSE IF(ORBMOD(6).EQ.2) THEN
                WRITE(LFNPRT,1018) IARC,ITER
1018            FORMAT(' ',79('-'),/,
     1             ' RPR PARAMETERS AND RMS ERRORS   ',
     1             'ARC NUMBER:',I3,20X,'ITERATION:',I3,/,' ',79('-'),/,
     2               ' SAT        D0       RMS        S0       RMS  ',
     3               '       W0       RMS',/,
     4               '           (M/S**2*1.D-7)      (M/S**2*1.D-10)',
     5               '      (M/S**2/D*1.D-7) ',/,
     6               ' ---     ------------------   ------------------',
     7               '   ------------------',/)
              END IF
CC
              DO 392 ISAT=1,NSAT
                OUTARR=' '
                JSAT=IDXSAT(ISAT)
                IF(I1.NE.0)THEN
                  IND=INDLOC(IND1)
                  IF(MODEL.EQ.3)IND=IND-6
                  SCAL=SCALPRT(I1+6)
                  WRITE(OUTARR(2:4),3071)NAVNUM(JSAT)
                  WRITE(OUTARR(8:27),3072)RPRESS(IND,JSAT)*SCAL,
     1                                    SIGRPR(IND,JSAT)*SCAL
                  RPRESS2(JSAT,1)=RPRESS(IND,JSAT)*SCAL
                  SIGRPR2(JSAT,1)=SIGRPR(IND,JSAT)*SCAL
                END IF
                IF(I2.NE.0)THEN
                  IND=INDLOC(IND2)
                  IF(MODEL.EQ.3)IND=IND-6
                  SCAL=SCALPRT(I2+6)
                  WRITE(OUTARR(2:4),3071)NAVNUM(JSAT)
                  WRITE(OUTARR(29:48),3072)RPRESS(IND,JSAT)*SCAL,
     1                                     SIGRPR(IND,JSAT)*SCAL
                  RPRESS2(JSAT,2)=RPRESS(IND,JSAT)*SCAL
                  SIGRPR2(JSAT,2)=SIGRPR(IND,JSAT)*SCAL
                END IF
                IF(I3.NE.0)THEN
                  IND=INDLOC(IND3)
                  IF(MODEL.EQ.3)IND=IND-6
                  SCAL=SCALPRT(I3+6)
                  WRITE(OUTARR(2:4),3071)NAVNUM(JSAT)
                  WRITE(OUTARR(50:69),3072)RPRESS(IND,JSAT)*SCAL,
     1                                     SIGRPR(IND,JSAT)*SCAL
                  RPRESS2(JSAT,3)=RPRESS(IND,JSAT)*SCAL
                  SIGRPR2(JSAT,3)=SIGRPR(IND,JSAT)*SCAL
                 END IF
3071            FORMAT(I3)
3072            FORMAT(2F10.5)
                WRITE(LFNPRT,308)OUTARR
308             FORMAT(A80)
392           CONTINUE
C
C WRITE END LINE OF PARAMETER TABLE
C ---------------------------------
              WRITE(LFNPRT,1011)
            END IF

            I1=0
            I2=0
            I3=0
            NNN=0
            DO 6011 IPAR=7,NUNB
              IF(LOCQ(1,IPAR).EQ.3.AND.LOCQ(4,IPAR).EQ.10)THEN
                I1=4
                IND1=IPAR
                NNN=NNN+1
              END IF
              IF(LOCQ(1,IPAR).EQ.3.AND.LOCQ(4,IPAR).EQ.11)THEN
                I2=5
                IND2=IPAR
                NNN=NNN+1
              END IF
              IF(LOCQ(1,IPAR).EQ.3.AND.LOCQ(4,IPAR).EQ.12)THEN
                I3=6
                IND3=IPAR
                NNN=NNN+1
              END IF
6011        CONTINUE
            IF(NNN.NE.0)THEN
              IF(ORBMOD(6).EQ.0) THEN
                WRITE(LFNPRT,1019) IARC,ITER
1019            FORMAT(' ',79('-'),/,
     1             ' RPR PARAMETERS AND RMS ERRORS   ',
     1             'ARC NUMBER:',I3,20X,'ITERATION:',I3,/,' ',79('-'),/,
     2               ' SAT        D_COS    RMS        Y_COS    RMS  ',
     3               '       X_COS    RMS',/,
     4               '           (M/S**2*1.D-7)      (M/S**2*1.D-7) ',
     5               '      (M/S**2/D*1.D-7) ',/,
     6               ' ---     ------------------   ------------------',
     7               '   ------------------',/)
              ELSE IF(ORBMOD(6).EQ.1) THEN
                WRITE(LFNPRT,1020) IARC,ITER
1020            FORMAT(' ',79('-'),/,
     1             ' RPR PARAMETERS AND RMS ERRORS   ',
     1             'ARC NUMBER:',I3,20X,'ITERATION:',I3,/,' ',79('-'),/,
     2               ' SAT        R_COS    RMS        S_COS    RMS  ',
     3               '       W_COS    RMS',/,
     4               '           (M/S**2*1.D-7)      (M/S**2*1.D-7) ',
     5               '      (M/S**2/D*1.D-7) ',/,
     6               ' ---     ------------------   ------------------',
     7               '   ------------------',/)
              ELSE IF(ORBMOD(6).EQ.2) THEN
                WRITE(LFNPRT,1021) IARC,ITER
1021            FORMAT(' ',79('-'),/,
     1             ' RPR PARAMETERS AND RMS ERRORS   ',
     1             'ARC NUMBER:',I3,20X,'ITERATION:',I3,/,' ',79('-'),/,
     2               ' SAT        R_COS    RMS        S_COS    RMS  ',
     3               '       W_COS    RMS',/,
     4               '           (M/S**2*1.D-7)      (M/S**2*1.D-7) ',
     5               '      (M/S**2/D*1.D-7) ',/,
     6               ' ---     ------------------   ------------------',
     7               '   ------------------',/)
              END IF
C
              DO 393 ISAT=1,NSAT
                OUTARR=' '
                JSAT=IDXSAT(ISAT)
                IF(I1.NE.0)THEN
                  IND=INDLOC(IND1)
                  IF(MODEL.EQ.3)IND=IND-6
                  SCAL=SCALPRT(I1+6)
                  WRITE(OUTARR(2:4),3071)NAVNUM(JSAT)
                  WRITE(OUTARR(8:27),3072)RPRESS(IND,JSAT)*SCAL,
     1                                    SIGRPR(IND,JSAT)*SCAL
                  RPRESS2(JSAT,4)=RPRESS(IND,JSAT)*SCAL
                  SIGRPR2(JSAT,4)=SIGRPR(IND,JSAT)*SCAL
            END IF
                IF(I2.NE.0)THEN
                  IND=INDLOC(IND2)
                  IF(MODEL.EQ.3)IND=IND-6
                  SCAL=SCALPRT(I2+6)
                  WRITE(OUTARR(2:4),3071)NAVNUM(JSAT)
                  WRITE(OUTARR(29:48),3072)RPRESS(IND,JSAT)*SCAL,
     1                                     SIGRPR(IND,JSAT)*SCAL
                  RPRESS2(JSAT,5)=RPRESS(IND,JSAT)*SCAL
                  SIGRPR2(JSAT,5)=SIGRPR(IND,JSAT)*SCAL
                END IF
                IF(I3.NE.0)THEN
                  IND=INDLOC(IND3)
                  IF(MODEL.EQ.3)IND=IND-6
                  SCAL=SCALPRT(I3+6)
                  WRITE(OUTARR(2:4),3071)NAVNUM(JSAT)
                  WRITE(OUTARR(50:69),3072)RPRESS(IND,JSAT)*SCAL,
     1                                     SIGRPR(IND,JSAT)*SCAL
                  RPRESS2(JSAT,6)=RPRESS(IND,JSAT)*SCAL
                  SIGRPR2(JSAT,6)=SIGRPR(IND,JSAT)*SCAL
                END IF
                WRITE(LFNPRT,308)OUTARR
393           CONTINUE
C
C WRITE END LINE OF PARAMETER TABLE
C ---------------------------------
              WRITE(LFNPRT,1011)
            END IF
            I1=0
            I2=0
            I3=0
            NNN=0
            DO 6012 IPAR=7,NUNB
              IF(LOCQ(1,IPAR).EQ.3.AND.LOCQ(4,IPAR).EQ.13)THEN
                I1=7
                IND1=IPAR
                NNN=NNN+1
              END IF
              IF(LOCQ(1,IPAR).EQ.3.AND.LOCQ(4,IPAR).EQ.14)THEN
                I2=8
                IND2=IPAR
                NNN=NNN+1
              END IF
              IF(LOCQ(1,IPAR).EQ.3.AND.LOCQ(4,IPAR).EQ.15)THEN
                I3=9
                IND3=IPAR
                NNN=NNN+1
              END IF
6012        CONTINUE
            IF(NNN.NE.0)THEN
              IF(ORBMOD(6).EQ.0) THEN
                WRITE(LFNPRT,1022) IARC,ITER
1022            FORMAT(' ',79('-'),/,
     1             ' RPR PARAMETERS AND RMS ERRORS   ',
     1             'ARC NUMBER:',I3,20X,'ITERATION:',I3,/,' ',79('-'),/,
     2               ' SAT        D_SIN    RMS        Y_SIN    RMS  ',
     3               '       X_SIN    RMS',/,
     4               '           (M/S**2*1.D-7)      (M/S**2*1.D-7) ',
     5               '      (M/S**2/D*1.D-7) ',/,
     6               ' ---     ------------------   ------------------',
     7               '   ------------------',/)
              ELSE IF(ORBMOD(6).EQ.1) THEN
                WRITE(LFNPRT,1023) IARC,ITER
1023            FORMAT(' ',79('-'),/,
     1             ' RPR PARAMETERS AND RMS ERRORS   ',
     1             'ARC NUMBER:',I3,20X,'ITERATION:',I3,/,' ',79('-'),/,
     2               ' SAT        R_SIN    RMS        S_SIN    RMS  ',
     3               '       W_SIN    RMS',/,
     4               '           (M/S**2*1.D-7)      (M/S**2*1.D-7) ',
     5               '      (M/S**2/D*1.D-7) ',/,
     6               ' ---     ------------------   ------------------',
     7               '   ------------------',/)
              ELSE IF(ORBMOD(6).EQ.2) THEN
                WRITE(LFNPRT,1024) IARC,ITER
1024            FORMAT(' ',79('-'),/,
     1             ' RPR PARAMETERS AND RMS ERRORS   ',
     1             'ARC NUMBER:',I3,20X,'ITERATION:',I3,/,' ',79('-'),/,
     2               ' SAT        R_SIN    RMS        S_SIN    RMS  ',
     3               '       W_SIN    RMS',/,
     4               '           (M/S**2*1.D-7)      (M/S**2*1.D-7) ',
     5               '      (M/S**2/D*1.D-7) ',/,
     6               ' ---     ------------------   ------------------',
     7               '   ------------------',/)
              END IF
C
              DO 394 ISAT=1,NSAT
                OUTARR=' '
                JSAT=IDXSAT(ISAT)
                IF(I1.NE.0)THEN
                  IND=INDLOC(IND1)
                  IF(MODEL.EQ.3)IND=IND-6
                  SCAL=SCALPRT(I1+6)
                  WRITE(OUTARR(2:4),3071)NAVNUM(JSAT)
                  WRITE(OUTARR(8:27),3072)RPRESS(IND,JSAT)*SCAL,
     1                                    SIGRPR(IND,JSAT)*SCAL
                  RPRESS2(JSAT,7)=RPRESS(IND,JSAT)*SCAL
                  SIGRPR2(JSAT,7)=SIGRPR(IND,JSAT)*SCAL
                END IF
                IF(I2.NE.0)THEN
                  IND=INDLOC(IND2)
                  IF(MODEL.EQ.3)IND=IND-6
                  SCAL=SCALPRT(I2+6)
                  WRITE(OUTARR(2:4),3071)NAVNUM(JSAT)
                  WRITE(OUTARR(29:48),3072)RPRESS(IND,JSAT)*SCAL,
     1                                     SIGRPR(IND,JSAT)*SCAL
                  RPRESS2(JSAT,8)=RPRESS(IND,JSAT)*SCAL
                  SIGRPR2(JSAT,8)=SIGRPR(IND,JSAT)*SCAL
                END IF
                IF(I3.NE.0)THEN
                  IND=INDLOC(IND3)
                  IF(MODEL.EQ.3)IND=IND-6
                  SCAL=SCALPRT(I3+6)
                  WRITE(OUTARR(2:4),3071)NAVNUM(JSAT)
                  WRITE(OUTARR(50:69),3072)RPRESS(IND,JSAT)*SCAL,
     1                                     SIGRPR(IND,JSAT)*SCAL
                  RPRESS2(JSAT,9)=RPRESS(IND,JSAT)*SCAL
                  SIGRPR2(JSAT,9)=SIGRPR(IND,JSAT)*SCAL
                END IF
                WRITE(LFNPRT,308)OUTARR
394           CONTINUE
C
C WRITE END LINE OF PARAMETER TABLE
              WRITE(LFNPRT,1011)
            END IF
          ENDIF
C
C Write stochastic parameters
C ---------------------------
          IF(m_stoch > 0)THEN
              WRITE(LFNPRT,2018) IARC,ITER
2018          FORMAT(' ',79('-'),/,' STC PARAMETERS AND RMS ERRORS   ',
     1       'ARC NUMBER:',I3,20X,'ITERATION:',I3,/,' ',79('-'),/,
     2       ' SAT         EPOCH   ',
     3              '        DR       RMS      DS       RMS  ',
     3       '    DW       RMS',/,10x,
     4       '                   (M/S*1.D-5)       (M/S*1.D-5)    ',
     5       '   (M/S*1.D-5)            ',/,
     6       ' ---  -------------------',
     7       '  ---------------  ----------------',
     7       '  ----------------',/)
            DO ISAT=1,NSAT
              JSAT=IDXSAT(ISAT)
              IF(m_indsat(jsat) > 0)THEN
                DO iepo=1,m_nstcepo(m_indsat(jsat))
                  CALL TIMST2(1,1,
     1                 t0arc+m_stcepo(iepo,m_indsat(jsat)),TSTRNG)
                  WRITE(lfnprt,2019)navnum(jsat),TSTRNG,
     1                 (m_pulse(kk,iepo,m_indsat(jsat))*1.d5,
     2                  m_pulse_rms(kk,iepo,m_indsat(jsat))*1.d5,kk=1,3)
2019              FORMAT(' ',i3,2X,A,3(f9.3,f8.3,1x))
                ENDDO
              ENDIF
            ENDDO
C
C WRITE END LINE OF STOCHASTICS TABLE
            WRITE(LFNPRT,1011)

          ENDIF
C
C WRITE LIGHT/SHADOW PASSAGES
C ---------------------------
          IF (ITER.EQ.NITER) THEN
            CALL SHDPRI(NSACHG,SATCHG,NUMCHG,CHGTIM,
     1                  IPRCHS,IPRCHG,IARC,TFL(1,IARC),IDXCHG,mooecl)
          ENDIF
C
400     CONTINUE

C
C write RPR Values to satellite specific file
C -------------------------------------------
        IF (ITIM.EQ.1) THEN
           DO ISAT=1,NSAT
              JSAT=IDXSAT(ISAT)
              WRITE(JSATSTR,'(I4.4)')NAVNUM(JSAT)
              FILNAM2='/home/aiub/aiub_u_camp/RPRTIM/OUT/RPR_T2'
     1             //JSATSTR//'.TXT'
              INQUIRE(FILE=FILNAM2, EXIST=FEXIST)
              FSTATUS='NEW'
              FPOSITION='ASIS'
              IF (FEXIST)THEN
                 FSTATUS='OLD'
                 FPOSITION='APPEND'
              ENDIF
              OPEN(UNIT=LFN001,FILE=FILNAM2,STATUS=FSTATUS,
     1             POSITION=FPOSITION,FORM='FORMATTED',
     2             IOSTAT=IOSTAT)
              WRITE(LFN001,99990)NAVNUM(JSAT),TMID,
     1             BTMP0(JSAT),UTMP0(JSAT),
     2             RPRESS2(JSAT,1),RPRESS2(JSAT,2),
     3             RPRESS2(JSAT,4),RPRESS2(JSAT,6),
     4             RPRESS2(JSAT,7),RPRESS2(JSAT,9)
99990         FORMAT(I5,F11.4,2F11.6,6F10.5)
              CLOSE(LFN001)
           ENDDO
        ENDIF
1000  CONTINUE
C
C END OF MAIN LOOP (ALL ARCS)
C
C WRITE THE HEADER OF THE RESIDUAL FILE
C -------------------------------------
      IF (IRCRES .EQ. 0) THEN
C
C INIT RESIDUAL HEADER RECORD
C ---------------------------
        RESHED%TITLE    = 'RESIDUALS FROM PROGRAM ORBGEN'
        RESHED%DSC      = resHed_ORBGEN
        RESHED%DSC%NPAR = NUNB
        RESHED%NFIL     = NARC
        ALLOCATE(RESHED%FILHEAD(NARC),STAT=irc)
        CALL ALCERR(IRC,'RESHED%FILHEAD',(/NARC/),'ORBGEN')
        DO IARC=1,NARC
          CALL init_filHead(RESHED%FILHEAD(IARC))
          RESHED%FILHEAD(IARC)%MEATYP = 2
          RESHED%FILHEAD(IARC)%NFRFIL = 3
          RESHED%FILHEAD(IARC)%ICARR(1:3) = (/ 1,2,3 /)
          RESHED%FILHEAD(IARC)%STANAM(1) = 'ARC'
          WRITE(RESHED%FILHEAD(IARC)%STANAM(1)(4:6),'(I3)') IARC
          RESHED%FILHEAD(IARC)%CSESS(:) = ' '
          RESHED%FILHEAD(IARC)%IDELTT = IDNINT(DTPI*3600.D0)
          RESHED%FILHEAD(IARC)%TIMREF = TFL(1,IARC)
          RESHED%FILHEAD(IARC)%NSATEL = NSATEL(IARC)
          ALLOCATE(RESHED%FILHEAD(IARC)%NUMSAT(NSATEL(IARC)),STAT=IRC)
          CALL ALCERR(IRC,'RESHED%FILHEAD(IARC)%NUMSAT',
     1               (/NSATEL(IARC)/),'ORBGEN')
          RESHED%FILHEAD(IARC)%NUMSAT(1:NSATEL(IARC)) =
     1                                     NUMSAT(1:NSATEL(IARC),IARC)
        ENDDO
C
C WRITE THE RESIDUAL FILE HEADER
C ------------------------------
        CALL WTRESH2(LFNRES,RESHED)
C
        DO IARC=1,NARC
          DEALLOCATE(RESHED%FILHEAD(IARC)%NUMSAT,STAT=IRC)
        ENDDO
        DEALLOCATE(RESHED%FILHEAD,STAT=irc)
C
C COPY RESIDUAL FILE FROM LFNSCR TO LFNRES
C ----------------------------------------
        REWIND(UNIT=LFNSCR)
        DO 700 IREC = 1,MXRECL*MAXARC*MAXSAT
          READ(LFNSCR,END=9999) IARC,IEPOCH,IFRQ,ISATEL,XYZRES,RESFLG
          WRITE(LFNRES)IARC,IEPOCH,IFRQ,ISATEL,0,XYZRES,RESFLG
700     CONTINUE
        CLOSE(UNIT=LFNRES)
        CLOSE(UNIT=LFNSCR,STATUS='DELETE')
C
9999    CONTINUE
      END IF
C
C COMPLETE PLOT-FILE (SKELETON PART B)
C ------------------------------------
      IF (IRCPLT.EQ.0) THEN
        CALL WTSKEL('B  ',FILSKL,FIELDS,IPLFLG,LFNPLT)
        CLOSE(UNIT=LFNPLT)
      ENDIF
C
C Residual PLOT-FILE
C -------------------
      IF (ircpl2.EQ.0) THEN
        CLOSE(UNIT=lfnpl2)
      ENDIF
C
C WRITE SUMMARY OUTPUT FILE
C -------------------------
      IF (GENUPD.EQ.1.OR.GENUPD.EQ.2) THEN
        CALL OGSUMF(MAXARC,MAXFIL,NARC  ,TFL   ,NFIL  ,NSATEL,
     1              NUMSAT,NUMSUM,RMSSUM,IECLIP,IDASUM)
      ENDIF
C
C GET NAME OF NEW ORBIT INFO FILE
C -------------------------------
c      CALL GTFLNA(0,'ORBELE ',FILNAM,IRCELE)
cC
cC PREPARE GENERAL INFO TO WRITE NEW ORBITAL ELEMENT FILE
cC ------------------------------------------------------
c      IF (IRCELE .EQ. 0) THEN
c        ELETYP='TP'
c        TITLE='ORBITAL ELEMENT FROM PROGRAM "ORBGEN"'
c        TITLE(66:74)=DATE
c        TITLE(76:80)=TIME
c        NARCNW=IARCNW
cC
c routine WTOELE does not support new radiation pressure model (14-sep-00/hu)
c (routine removed from cvs)
c        CALL WTOELE(FILNAM,ELETYP,TITLE,NARCNW,ARCSAT,ARCINT,ARCFGT,
c     1              ARCSYS,ARCT00,ARCELE,ARCSIG,ARCFLG,ARCPSD)
c      ENDIF
C
C END
C ---
999   CONTINUE
      CALL EXITRC(0)
      END
