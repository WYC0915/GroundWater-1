C*
      PROGRAM CODSPP
CC
CC NAME       :  CODSPP
CC
CC PURPOSE    :  CODSPP PROCESSES PSEUDO-RANGES
CC                 - SINGLE POINT SOLUTION
CC                 - PARAMETERS: - RECEIVER COORDINATES
CC                               - RECEIVER CLOCK PARAMETERS (OFFSET,
CC                                 DRIFT ... UP TO 6 POLYN. COEFF.)
CC                               - ONE RECEIVER CLOCK OFFSET PER EPOCH
CC                 - PROCESSING OBSERVATIONS FROM L1,L2, OR COMBINATION
CC                   L1,L2 ("IONOSPHERE-FREE") IS POSSIBLE
CC
CC AUTHOR     :  T.SCHILDKNECHT
CC
CC CREATED    :  87/10/19 08:41
CC
CC CHANGES    :  06-JUL-91 : ??: CHANGE PROCESSING FREQUENCY FROM L3 TO L1
CC                               FOR FILES WITH ONLY L1 CODE
CC               17-JUL-91 : ??: ALLOW STANDARD ORBITS AND SAT. CLOCK FILE
CC                               CHANGE MAXEPO FROM 4000 TO 6000.
CC                               ADD PARAMETER "ISESS" TO CALL OF "CDRESU"
CC               23-DEC-91 : ??: VERSION NUMBER CHANGED TO 3.3
CC               23-APR-92 : ??: NEW INTERNAL FILE NAME "RESIDRS"
CC               23-MAY-92 : LM: SATELLITE DEPENDENT ACCURACY
CC                               STATISTICS
CC               13-JUL-92 : ??: COMMON FOR "MAXFRQ" DUE TO RESIDUAL FILE
CC               07-AUG-92 : ??: RETURN CODE FROM "CDCOMP"
CC               11-JAN-93 : ??: VERSION NUMBER CHANGED TO 3.4
CC               03-AUG-93 : LM: NEW FORMAT, VERSION 3.5
CC               23-NOV-93 : MR: OUTLIER DETECTION AND PRINTING OF
CC                               MARKED AREAS
CC               08-MAR-94 : MR: ORDER SATELLITES FOR OUTPUT
CC               25-JUL-94 : MR: ADD COMMA AS FIELD SEPARATOR IN FORMAT
CC               12-AUG-94 : MR: CALL EXITRC
CC               12-AUG-94 : MR: FORMAT 4: SESSION AS CHARACTER*4
CC               30-MAR-95 : RW: PRINT GENERAL FILE NAMES
CC               07-APR-95 : MR: CHANGE PROCESSING FREQUENCY IF
CC                               NOT ENOUGH L2 OBSERVATIONS
CC                               ADD NUMOBT,NUMMRT TO CALL HEDINF
CC               24-APR-95 : MR: CHECK IF NO L1 OBSERVATIONS
CC               28-AUG-95 : TS: ALLOW SAMPLING WITH EPOCH WISE CLOCK EST.
CC               28-SEP-95 : TS: ALLOW MEASUREMENT TYPE 3=RANGES
CC               17-SEP-95 : JJ: INCREASE MAXSTA TO 200
CC               07-OCT-95 : MR: CHANGE VERSION TO 4.0
CC               16-OCT-95 : TS: INCREASE MAXDEL 2000 --> 5000
CC               13-DEC-95 : MR: NEW OPTION "MAX. NUMBER OF ITER."
CC               26-MAR-96 : MR: ADD "CSESS" TO CALL CDCOMP
CC               23-APR-96 : MR: NEW VERSION 4.1
CC               24-JUN-96 : TS: ALLOW ESTIMATED TROPOSPHERE INPUT
CC               06-OCT-97 : HH: ADD GLONASS
CC               30-OCT-97 : DI: CALL DEFREQ WITH NSATEL AND SATNUM
CC               19-NOV-97 : SS: INCLUDE "PGMVER"
CC               04-MAY-98 : SS: SR DEFREQ MODIFIED
CC               13-MAY-98 : MR: REPLACE "DFLOAT" BY "DBLE"
CC               19-MAY-98 : SS: "MAXSVN=124" REMOVED
CC               23-JUN-98 : MR: ADD "MIXED" TO CALL HEDINF
CC               25-AUG-98 : MR: SAVE SATELLITE CLOCK (CORRECTED)
CC               04-AUG-99 : PF: ADD "TIMCRD" TO CALL CDSCOR
CC               14-JAN-00 : RD: NEW OPTIION: SAVE CLK ESTIM. INTO OBS FILES
CC               22-AUG-00 : RD: SAVE CLK ESTIM.: ANOTHER REALIZATION
CC               02-NOV-00 : CU: SWITCH TO THE NEW MENU SYSTEM
CC               18-DEC-00 : HU: USE INTERFACE FOR PRFLNA
CC               08-JAN-01 : RD: IORBFL IS SET IN SR CDINPT
CC               07-MAR-01 : LM: INCLUDE MAXAMB
CC               10-MAY-01 : RD: WRITE CLOCK RINEX FILE
CC               06-JUN-01 : MR: REMOVE SUPERFLUOUS PARAMETERS FROM
CC                               "CDCOMP" AND "CDRESU"
CC               27-JUL-01 : DS: HANDLE KINEMATIC COORDINATES
CC               27-JUL-01 : DS: LEO ORBIT
CC               05-SEP-01 : HU: INTERFACES FOR WTCRXH, WTCRXR ADDED
CC               16-DEC-01 : HU: USE D_CONST
CC               16-DEC-01 : HU: USE M_BERN, FOR MODULES USE ONLY
CC               26-JUN-02 : RD: OUTPUT FLAG FOR CDSCOR IS A PARAM.
CC               27-AUG-02 : RD: HANDLE NEW FORMATTED RESIDUAL FILES
CC               28-AUG-02 : DS: FLAG FOR ESTIMATION OF KINEMATIC COORDINATES
CC               28-AUG-02 : DS: ESTIMATION OF KINEMATIC COORDINATES
CC               25-SEP-02 : HU: REMOVE I_ASTLIB
CC               16-DEC-02 : RD: USE MARK FLAG FROM OBSERV. FILE IS AN OPTION
CC               21-JAN-03 : RD: INCREASE MAXDEL 5000->15000
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               28-FEB-03 : SC: ADD TITLE SECTION
CC               15-MAY-03 : HU: INITIALIZE STRUCTURES
CC               19-MAY-03 : RD: INIT TIME WINDOW TO (/0d0,1d20/)
CC               31-JUL-03 : RD: OPEN/CLOSE KINOUT OUTSIDE THE NFTOT-LOOP
CC               22-AUG-03 : AJ: NEW CALL FOR SR CDCOMP
CC               08-SEP-03 : HU: ANTNAM, RECNAM, OPRNAM CHR16 -> CHR20
CC               13-SEP-03 : HU: INTERFACE FOR DEFREQ
CC               15-OCT-03 : SS: WRITE DCB OUTPUT FILE IN CASE OF GNSS
CC               29-OCT-03 : HB: CORRECT WRITING ERROR
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               01-DEC-03 : HB: INITIALIZE CLK-SIGMAS WITH
CC                               UNDEF = 999999.999999D0
CC               05-DEC-03 : RD: RESCALE CLK RINEX VALUES WITH 1D6 (SEE WTCRXR)
CC               16-Mar-04 : RD: NEW CALL OF HEDINF
CC               17-MAR-04 : PF: IDUMMY IN CALL TO HEDINF, INSTEAD OF 0
CC               28-JUN-04 : RD: USE MAXSTA FROM M_MAXDIM
CC               14-JUL-04 : HU: SATELLITE CLOCK FILE REQUIRED
CC               07-FEB-05 : HB: ADOPT FOR ifc-COMPILER, VERSION 8.1
CC               21-JUN-05 : MM: LFNUM.inc REMOVED (LFNUMs IN M_BERN)
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               08-Aug-05 : HB: Use new SR TIMST2 (module)
CC               11-AUG-05 : RD: BUGFIX OBSERVATION WINDOW
CC               20-DEC-05 : RD: COMPUTE EPOCH IN CLOCK RINEX CORRECT WHEN SAMPL
CC               01-MAR-06 : RD: ADAPT LAYOUT OF THE CLK-RINEX TO GPSEST
CC               12-JUN-06 : HB: USE NEW SR OSAMPL FOR FINDING CORRECT
CC                               SAMPLING EPOCH
CC               05-JUL-06 : RD: ONLY SAT-CLK IN CLK RINEX IF NO STA CLOCKS
CC                               FOR THIS EPOCH
CC               24-NOV-06 : AG: LEAP SECOND, PGMNAM, PCVSTR SET IN CLKHED
CC               27-Feb-07 : AG: CALL DEFCON WITH PARAMETER
CC               27-May-07 : AG: "E" IMPLEMENTED FOR CLOCK RINEX OUTPUT
CC               12-JUN-07 : AG: INITIALIZE ANTENNA BUFFER
CC               12-OCT-07 : RD: ALLOCATE MAXEPO-ARRAYS
CC               01-NOV-07 : HB: ADD SECIPL
CC               04-MAY-09 : RD: CONSIDER VIENNA GRID FILES FOR VMF
CC               09-MAY-09 : RD: INCLUDE FREQ CODE BIASES INTO THE DCB FILES
CC               29-MAY-09 : RD: INPUT CLOCKS ALSO FROM INPUT CLK RNX FILE
CC               21-SEP-09 : RD: ECLIPSING FLAG ADDED
CC               21-MAY-10 : MF: CALL SR INIT_REF & INIT_FILHEAD
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               25-OCT-10 : SL: TWO INTEGER TO REAL CONVERSION BUGS CORRECTED
CC               27-OCT-10 : SL: USE M_BERN WITH ONLY
CC               19-JAN-11 : RD: USE GETSTA
CC               26-JAN-11 : LP: Sat-specific obs types; DEFREQ changes
CC               17-FEB-11 : RD: COMMON MCMSTA NOT NEEDED ANYMORE
CC               24-NOV-11 : SL: NEW TITLE STRING FOR PRITIT
CC               05-MAR-12 : RD: REMOVE UNUSED ARGUMENTS FROM SR CDCOMP
CC               18-JUN-12 : RD: REMOVE UNUSED ARGUMENTS FROM SR CDINPT
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: i4b, r8b, pgmVer,
     1                    lfnPrt, lfnErr, lfnRes, lfn001, lfn002,
     2                    staNameLength
      USE m_cpu,    ONLY: cpu_start
      USE m_time,   ONLY: t_timint
      USE m_maxdim, ONLY: maxsat, maxamb
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE d_const,  ONLY: date,time
      USE d_clkrnx, ONLY: t_clkhead, t_clkrec, init_clkhead,
     1                    init_clkrec,unDef, init_ref
      USE d_resfil, ONLY: t_reshead, reshed_CODSPP, init_reshead,
     1                    init_filhead
      USE d_phaecc, ONLY: init_buf
      USE d_grid,   ONLY: initGridBuffer,prtGridInfo
      USE d_rinex3, ONLY: t_gobsdef
      USE f_dgpsut
      USE f_lincount
      USE s_opnfil
      USE s_wtcrxr
      USE s_cdresu
      USE s_cdsclk
      USE s_cdoinf
      USE s_pritit
      USE s_cdcomp
      USE s_timst2
      USE s_defreq
      USE s_defcon
      USE s_opnsys
      USE s_gtflna
      USE s_cdinpt
      USE s_alcerr
      USE s_prflna
      USE s_readinpf
      USE s_opnerr
      USE s_hedinf
      USE s_getsta
      USE s_wtcbfl
      USE s_exitrc
      USE s_wtresh2
      USE s_upsclk
      USE s_gtsclk
      USE s_gtsensor
      USE s_wtcrxh
      USE s_cdscor
      USE s_cdresi
      USE s_osampl
      USE s_clrflg
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 ICBTYP, ICLK  , ICMP  , ICORFL, ICORSV, IDUMMY, IEPO  ,
     1          IFIL  , IMINTT, IONO  , IORBFL, IOS   , IOSTAT, IOUTLR,
     2          IRC   , IRCCMP, IRCKOU, IRCODE, IRETC , IRSSAV, ISAT  ,
     3          ISTA  , ITROPO, KINEST, L2TOL1, LFNKOU, MAXARC, MAXCLK,
     4          MAXCMP, MAXCOR, MAXDEL, MAXEPH, MAXEPO, MAXFIL, MAXFRQ,
     5          MINDOF, MIXED , MXCAMB, MXCARC, MXCCLK, MXCCMP, MXCCOR,
     6          MXCDEL, MXCEPH, MXCEPO, MXCFIL, MXCFRQ, MXCSAT,
     7          MXCSVN, NCAMP , NCENTR, NDEL  , NFTOT , NITER , NORDIM,
     8          NSAMPL, NSTAT , NUMREC, NUMSAT, NUMIFB, II    , NCLK  ,
     9          MAXSTA, IAC
C
      REAL*8    CONFID, DIFMAX, SIGOFF, TIMCRD, TIMOFF, USRSIG, ZENMAX,
     1          SAMPL , secIpl, TSAMPL, TPREV , TNEXT
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C  MAXIMAL DIMENSIONS
C  ------------------
      PARAMETER (MAXEPH= 70,MAXCLK=10)
      PARAMETER (MAXFIL= 200,MAXCMP= 6,MAXCOR=3)
      PARAMETER (MAXARC=20,MAXFRQ= 1,MAXDEL=15000)
      INCLUDE 'COMFREQ.inc'
C
C  MAXSTA: MAXIMAL NUMBER OF STATIONS
C  MAXSAT: MAXIMAL NUMBER OF SATELLITES
C  MAXSVN: MAXIMAL SPACE VEHICLE NUMBER
C  MAXEPH: MAXIMAL NUMBER OF BROADCAST EPHEMERIS DATA SETS
C  MAXCLK: MAXIMAL NUMBER OF RECEIVER CLOCK MODEL PARAMETERS
C  MAXFIL: MAXIMAL NUMBER OF OBSERVATION FILE PER RUN
C  MAXCMP: MAXIMAL NUMBER OF CAMPAINGS
C  MAXAMB: MAXIMAL NUMBER OF AMBIGUITIES IN A FILE
C  MAXCOR: NUMBER OF COORDINATE PARAMETERS
C  MAXEPO: MAXIMAL NUMBER OF EPOCHS PER FILE
C  MAXARC: MAXMUM NUMBER OF ARCS IN STANDARD ORBIT FILE
C  MAXFRQ: MAXIMUM NUMBER OF FREQUENCIES FOR RESIDUAL FILE
C  MAXDEL: MAXIMUM NUMBER OF DELETED AREAS (ALL FILES)
C
C  DECLARATIONS
C  ------------
      TYPE(t_clkhead) :: clkhed
      TYPE(t_clkrec)  :: clkrec
      TYPE(t_timint)  :: allwin
      TYPE(t_reshead) :: reshed
      TYPE(t_gobsdef) :: gobsdef(MAXFIL) ! Giove External Obs. Selection info
C
      REAL(r8b), DIMENSION(:), ALLOCATABLE :: OFFS,DOFFS                (MAXEPO)
      REAL*8 OBSWIN(2),FILWIN(2,MAXFIL)
      REAL*8 TIMREF(MAXFIL),POSECC(3,2,MAXFIL)
      REAL*8 OBSERV(MAXSAT,2)
      REAL*8 AMBIGU(MAXAMB,3,MAXFIL),OFFS0(MAXFIL)
      REAL(r8b), DIMENSION(:,:), ALLOCATABLE :: XSTAT
      REAL(r8b), DIMENSION(:,:), ALLOCATABLE :: XSTELL
      REAL(r8b), DIMENSION(:,:), ALLOCATABLE :: XSTECC
      REAL*8 AELL,BELL,DXELL(3),DRELL(3),SCELL
      REAL*8 EPH(20*MAXEPH,MAXSAT),CLOCK(20*MAXEPH,MAXSAT)
      REAL*8 SIGMA0(MAXFIL),RMSSAT(MAXSAT,MAXFIL)
      REAL*8 DPAR(MAXCOR+MAXCLK)
      REAL*8 MCOR(MAXCOR,MAXFIL),MELL(MAXCOR,MAXFIL)
      REAL*8 MCLOCK(MAXCLK,MAXFIL)
      REAL*8 XSTANW(3,MAXFIL),XSTNEL(3,MAXFIL),CLKMOD(MAXCLK,MAXFIL)
      REAL*8 EPOFRQ(2)
      REAL*8 DCBVA1(2,MAXSAT),DCBVA2(2,MAXFIL),DCBVA3(4,1)
C
      INTEGER*4 PRIOPT(5),NSCAMP(MAXCMP),NDIFF(MAXFIL)
      INTEGER*4 STFIL(2,MAXFIL),MEATYP(MAXFIL)
      INTEGER*4 NSATEL(MAXFIL)
      INTEGER*4 IDELTT(MAXFIL),ICAMPN(MAXFIL)
      INTEGER*4 NFREQ(MAXFIL),NEPOCH(MAXFIL),IRMARK(MAXFIL)
      INTEGER*4 IRUNIT(2,MAXFIL),NUMOB1(MAXSAT,2),NUMMR1(MAXSAT,2)
      INTEGER*4 NUMOBT(2,MAXFIL),NUMMRT(2,MAXFIL)
      INTEGER*4 ICLOCK(2,MAXFIL),SATNUM(MAXSAT,MAXFIL)
      INTEGER*4 NUMAMB(MAXFIL),AMBIEP(MAXAMB,MAXFIL)
      INTEGER*4 AMBSAT(MAXAMB,MAXFIL),AMBWLF(MAXAMB,2,MAXFIL)
      INTEGER*4 AMBCLS(MAXAMB,3,MAXFIL)
      INTEGER(i4b), DIMENSION(:),   ALLOCATABLE :: STANUM
      INTEGER(i4b), DIMENSION(:),   ALLOCATABLE :: ICENTR
      INTEGER(i4b), DIMENSION(:,:), ALLOCATABLE :: STCAMP
      INTEGER*4 INDSVN(MAXSVN),OBSSAT(2,MAXSAT,MAXFIL)
      INTEGER*4 ICLPOL(MAXFIL),ICLKSV(MAXFIL),FILFRQ(MAXFIL)
      INTEGER*4 IFRMAT(MAXFIL),IANTEN(2,MAXFIL),NEPFLG(MAXFIL)
      INTEGER*4 NRSAT(MAXSAT),IMFIL(MAXFIL)
      INTEGER*4 IPHSAV(MAXFIL)
      INTEGER*4 LSTDEL(5,MAXDEL),INDEX(MAXSAT),NITERF(MAXFIL)
      INTEGER*4 IOUTSV(MAXFIL),USEMRK
      INTEGER*4 DCBID1(MAXSAT),DCBIN1(MAXSAT),DCBIN2(MAXFIL)
      INTEGER*4 DCBIN3(MAXSAT*MAXFIL)
      INTEGER*4 NUMREF,usegeos(MAXFIL)
C
      CHARACTER(LEN=8), PARAMETER :: pgName = 'CODSPP'
      CHARACTER*80 TITLE
      CHARACTER*64 STITLE
      CHARACTER*32 CODHED(MAXFIL),CODOBS(MAXFIL),PHAHED(MAXFIL)
      CHARACTER*32 PHAOBS(MAXFIL),RESFIL,CORFIL,CRXFIL,FILKOU,DCBFIL
      CHARACTER*32 FILSAC
      CHARACTER*19 TSTRNG
      CHARACTER*11 PGMNAM
      CHARACTER*16 CAMPGN(MAXCMP),DATUM,DCBID2(MAXFIL)
      CHARACTER(LEN=staNameLength), DIMENSION(:), ALLOCATABLE :: stname
      CHARACTER*16 DCBID3(1)
      CHARACTER*20 RECTYP(2,MAXFIL),ANTTYP(2,MAXFIL)
      CHARACTER*6  MXNSAT,MXNSVN,MXNEPO,MXNEPH,MXNCLK,MXNFIL
      CHARACTER*6  MXNCMP,MXNAMB,MXNCOR,MXNARC,MXNFRQ,MXNDEL
      CHARACTER*4  CSESS(2,MAXFIL)
      CHARACTER*3  SATNAM
      CHARACTER*1  OBSFLG(MAXSAT,2),DCBSYS(MAXFIL),OBST
C
C  COMMON FOR CONSTANTS AND MAXIMAL DIMENSIONS
C  -------------------------------------------
      COMMON/LARGE/TIMREF,POSECC,OBSERV,AMBIGU,OFFS0,
     1             EPH,CLOCK,SIGMA0,
     2             MCLOCK,XSTANW,XSTNEL,CLKMOD,
     3             SATNUM,NUMAMB,AMBSAT,AMBIEP,AMBWLF,AMBCLS,
     4             OBSSAT,CODHED,CODOBS,PHAHED,PHAOBS,ANTTYP
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMSVN/MXCSVN,MXNSVN
      COMMON/MCMEPH/MXCEPH,MXNEPH
      COMMON/MCMCLK/MXCCLK,MXNCLK
      COMMON/MCMCOR/MXCCOR,MXNCOR
      COMMON/MCMFIL/MXCFIL,MXNFIL
      COMMON/MCMCMP/MXCCMP,MXNCMP
      COMMON/MCMAMB/MXCAMB,MXNAMB
      COMMON/MCMEPO/MXCEPO,MXNEPO
      COMMON/MCMARC/MXCARC,MXNARC
      COMMON/MCMFRQ/MXCFRQ,MXNFRQ
      COMMON/MCMDEL/MXCDEL,MXNDEL
C
      DATA   IRSSAV/0/,ICORSV/0/
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C  INITIALIZE COMMON BLOCKS FOR MAXIMAL DIMENSIONS
C  -----------------------------------------------
      MXCSAT=MAXSAT
      MXNSAT='MAXSAT'
      MXCSVN=MAXSVN
      MXNSVN='MAXSVN'
      MXCEPH=MAXEPH
      MXNEPH='MAXEPH'
      MXCCLK=MAXCLK
      MXNCLK='MAXCLK'
      MXCCOR=MAXCOR
      MXNCOR='MAXCOR'
      MXCFIL=MAXFIL
      MXNFIL='MAXFIL'
      MXCCMP=MAXCMP
      MXNCMP='MAXCMP'
      MXCAMB=MAXAMB
      MXNAMB='MAXAMB'
      MXCEPO=MAXEPO
      MXNEPO='MAXEPO'
      MXCARC=MAXARC
      MXNARC='MAXARC'
      MXCFRQ=MAXFRQ
      MXNFRQ='MAXFRQ'
      MXCDEL=MAXDEL
      MXNDEL='MAXDEL'
C
C INITIALIZE STRUCTURES
C ---------------------
      CALL init_clkhead(clkhed)
      CALL init_clkrec(clkrec)
      CALL init_reshead(reshed)
      CALL init_inpkey(inpKey)
C
C GET THE NAME OF THE INPUT FILE
C ------------------------------
      CALL readinpf(' ',inpKey)
C
C DEFINE SYSTEM FILES
C -------------------
      CALL OPNSYS
C
C PRINT TITLE
C -----------
      CALL  pritit('CODSPP','Code-based clock synchronization')
C
C  DEFINE CONSTANTS
C  ----------------
      CALL DEFCON(1)
      PGMNAM='CODSPP V'//PGMVER
      IRCODE=0
C
C PRINT GENERAL TITLE
C -------------------
      WRITE(LFNPRT,1) DATE,TIME
1     FORMAT(/,1X,79('*'),
     1       /,' COMPUTATION OF SINGLE POINT POSITION ',
     2         9X,A9,1X,A5,/,1X,79('*'),/)
C
C PRINT GENERAL FILE NAMES AND OUTPUT HEADER
C ------------------------
C
      CALL prflna
C
C  READ ALL INPUT INFORMATION FROM INPUT FILE
C  ------------------------------------------
      CALL CDINPT(STITLE,PRIOPT,USEMRK,
     1            NSAMPL,SECIPL,ZENMAX,OBSWIN,ITROPO,IONO,
     2            ICORFL,IOUTLR,DIFMAX,CONFID,MINDOF,USRSIG,
     3            NITER,IORBFL,CRXFIL,CLKHED,KINEST,IRETC)
      IRCODE=IRETC
      IF(IRETC.NE.0) GOTO 9000
C
C SET TITLE FOR OUTPUT FILES
C --------------------------
      TITLE(1:64)  = STITLE
      TITLE(65:65) = ' '
      TITLE(66:74) = DATE
      TITLE(75:75) = ' '
      TITLE(76:80) = TIME
C
C NO CLOCK FILE SPECIFIED
C -----------------------
      IF (IORBFL.EQ.2) THEN
        CALL GTFLNA(1,'SATCLK ',FILSAC,IRETC)
      ENDIF
C
C  READ INFORMATION CONCERNING OBSERVATION FILES
C  ---------------------------------------------
      CALL CDOINF(MAXFIL,KINEST, NFTOT,CODHED,CODOBS,PHAHED,
     1            PHAOBS,FILFRQ,ICLPOL,ICLKSV,IOUTSV,IRETC)
      IRCODE=IRETC
      IF(IRETC.NE.0) GOTO 9000
C
C ALLOCATE STATINO RELATED ARRAYS
C -------------------------------
      MAXSTA=NFTOT + LINCOUNT('ECCENT',6)
C
      ALLOCATE(STNAME(MAXSTA),STAT=IAC)
      CALL alcerr(iac,'STNAME',(/MAXSTA/),pgName)
      STNAME=''
C
      ALLOCATE(XSTAT(3,MAXSTA),STAT=IAC)
      CALL alcerr(iac,'XSTAT',(/3,MAXSTA/),pgName)
      XSTAT=0d0
C
      ALLOCATE(XSTELL(3,MAXSTA),STAT=IAC)
      CALL alcerr(iac,'XSTELL',(/3,MAXSTA/),pgName)
      XSTELL=0d0
C
      ALLOCATE(XSTECC(3,MAXSTA),STAT=IAC)
      CALL alcerr(iac,'XSTECC',(/3,MAXSTA/),pgName)
      XSTECC=0d0
C
      ALLOCATE(STANUM(MAXSTA),STAT=IAC)
      CALL alcerr(iac,'STANUM',(/MAXSTA/),pgName)
      STANUM=0
C
      ALLOCATE(ICENTR(MAXSTA),STAT=IAC)
      CALL alcerr(iac,'ICENTR',(/MAXSTA/),pgName)
      ICENTR=0
C
      ALLOCATE(STCAMP(NFTOT,MAXCMP),STAT=IAC)
      CALL alcerr(iac,'STCAMP',(/NFTOT,MAXCMP/),pgName)
      ICENTR=0
C
C  READ HADER OF OBSERVATION FILES, SAVE INFORMATION
C  --------------------------------------------------
      IDUMMY=0
      CALL HEDINF(NFTOT,CODHED,MAXCMP,NCAMP,CAMPGN,NDIFF,
     1            NSTAT,STNAME,STFIL,NSCAMP,STCAMP,MEATYP,NSATEL,
     2            CSESS,IDELTT,TIMREF,ICAMPN,
     3            NFREQ,NEPOCH,IRMARK,IRUNIT,POSECC,NUMOB1,NUMMR1,
     4            NUMOBT,NUMMRT,
     5            ICLOCK,RECTYP,ANTTYP,IANTEN,IFRMAT,NEPFLG,
     6            SATNUM,NUMAMB,AMBSAT,AMBIEP,AMBWLF,AMBIGU,AMBCLS,
     7            IDUMMY,MIXED,USEGEOS,GOBSDEF)
C
C INIT CLOCK RINEX OUTPUT FILE
C ----------------------------
      ClkHed%NumTyp = 0
      IF (LEN_TRIM(crxfil) > 0) THEN
        ClkHed%ProgNam       = PGMNAM
        ClkHed%CrDate        = DATE
        ClkHed%CrDate(11:16) = TIME
!
        ClkHed%NumTyp        = 2
        ALLOCATE(ClkHed%DatTyp(ClkHed%NumTyp),stat=ios)
        CALL alcerr(ios,'ClkHed%DatTyp',(/ClkHed%NumTyp/),pgName)
        ClkHed%DatTyp=(/'AR','AS'/)
!
        ClkHed%nSta          = NSTAT
        ClkHed%nSat          = 0
        ALLOCATE(ClkHed%ClkName(ClkHed%nSta+maxsat),stat=ios)
        CALL alcerr(ios,'ClkHed%ClkName',
     1                  (/ClkHed%nSta+maxsat/),pgName)
        ALLOCATE(ClkHed%StaCoord(3,ClkHed%nSta),stat=ios)
        CALL alcerr(ios,'ClkHed%StaCoord',
     1                  (/3,ClkHed%nSta/),pgName)
        ClkHed%ClkName(1:NSTAT)=STNAME(1:NSTAT)
      ENDIF
C
C
C  COMPUTE OBS.WINDOW FOR EVERY FILE
C  ---------------------------------
      allwin%t(1) =  99d99
      allwin%t(2) = -99d99
      iMINTT      =  HUGE(i4b)
      MAXEPO      = 0
      DO IFIL=1,NFTOT
        IF (OBSWIN(1).EQ.0D0 .OR. OBSWIN(1).LT.TIMREF(IFIL)) THEN
          FILWIN(1,IFIL) = TIMREF(IFIL)
        ELSE
          FILWIN(1,IFIL) = OBSWIN(1)
        ENDIF
        IF (OBSWIN(2).EQ.1D20 .OR. OBSWIN(2).GT.
     1      TIMREF(IFIL)+(NEPOCH(IFIL)*IDELTT(IFIL))/86400D0) THEN
          FILWIN(2,IFIL) = TIMREF(IFIL)+
     1                     (NEPOCH(IFIL)*IDELTT(IFIL))/86400D0
        ELSE
          FILWIN(2,IFIL) = OBSWIN(2)
        ENDIF
C
C Make some statistic
C -------------------
        IF (allwin%t(1) > FILWIN(1,IFIL)) allwin%t(1) = FILWIN(1,IFIL)
        IF (allwin%t(2) < FILWIN(2,IFIL)) allwin%t(2) = FILWIN(2,IFIL)
        IF (iMINTT > IDELTT(IFIL)) iMINTT = IDELTT(IFIL)
C
C Get a list of all satellites for clock rinex output
C ---------------------------------------------------
        IF (LEN_TRIM(crxfil) > 0) THEN
          DO iSat = 1,NSATEL(IFIL)
            satnam=''
            IF (SATNUM(iSat,IFIL) < 100) THEN
              WRITE(satnam(1:3),'(A,I2.2)') 'G',SATNUM(iSat,IFIL)
            ELSEIF (SATNUM(iSat,IFIL) < 200) THEN
              WRITE(satnam(1:3),'(A,I2.2)') 'R',SATNUM(iSat,IFIL)-100
            ELSEIF (SATNUM(iSat,IFIL) < 300) THEN
              WRITE(satnam(1:3),'(A,I2.2)') 'E',SATNUM(iSat,IFIL)-200
            ENDIF
            DO iClk = 1, ClkHed%nSat
              IF (ClkHed%ClkName(ClkHed%nSta+iClk) == satnam)
     1           satnam = ''
            ENDDO
            IF (LEN_TRIM(satnam) > 0) THEN
              ClkHed%nSat = ClkHed%nSat+1
              ClkHed%ClkName(ClkHed%nSta+ClkHed%nSat) = satnam
            ENDIF
          ENDDO
        ENDIF
        IF (MAXEPO.EQ.0.OR.NEPOCH(IFIL).GT.MAXEPO) MAXEPO=NEPOCH(IFIL)
      ENDDO
C
C ALLOCATE OFFS-ARRAYS USING MAXEPO
C ---------------------------------
      MXCEPO=MAXEPO
C
      ALLOCATE(OFFS(MAXEPO),STAT=IOS)
      CALL ALCERR(IOS,'OFFS',(/MAXEPO/),pgName)
      ALLOCATE(DOFFS(MAXEPO),STAT=IOS)
      CALL ALCERR(IOS,'DOFFS',(/MAXEPO/),pgName)
C
C  CHECK FILES AND OPTIONS
C  -----------------------
C
      DO 100 IFIL=1,NFTOT
C
        IMFIL(IFIL) = 0
        NITERF(IFIL)= 0
C
C  CHECK MAXIMAL NUMBER OF EPOCHS
        IF (NEPOCH(IFIL) .GT. MAXEPO) THEN
          WRITE(LFNERR,11) CODOBS(IFIL),NEPOCH(IFIL),MAXEPO
11        FORMAT(/' *** PG CODSPP: TOO MANY EPOCHS ON FILE'/,
     1                        16X,'FILE NAME        : ',A32/,
     2                        16X,'NUMBER OF EPOCHS : ',I7/,
     3                        16X,'MAXEPO           : ',I7/)
          IMFIL(IFIL) = 1
        ENDIF
C
C  CHECK COMPATIBILITY OF OPTIONS  (SAMPLING/WINDOW/OFFS. PER EPOCH)
        IF (ICLPOL(IFIL).EQ.-1 .AND. ICLKSV(IFIL).NE.0) THEN
          IF (NSAMPL .GT. 1) THEN
          WRITE(LFNERR,12) CODOBS(IFIL)
12        FORMAT(/' ### PG CODSPP: WARNING: SAMPLING USED WITH'/,
     1                        16X,'ESTIMATION OF ONE OFFSET PER EPOCH'/,
     2                        16X,'OBSERVATION FILES WILL NOT CONTAIN'/,
     3                        16X,'CLOCK CORRECTIONS FOR ALL EPOCHS'/,
     3                        16X,'FILE NAME: ',A32/)
          ENDIF
C
cc        IF (ICLPOL(IFIL) .EQ. -1 .AND. OBSWIN(1) .GT. 0.D0 .AND.
cc        IF (OBSWIN(1) .GT. 0.D0 .OR.
cc     1      OBSWIN(2) .LT. 1.D20) THEN
cc          WRITE(LFNERR,13) CODOBS(IFIL)
cc13        FORMAT(/' *** PG CODSPP: NO WINDOW ALLOWED FOR'/,
cc     1                        16X,'ESTIMATION OF ONE OFFSET PER EPOCH'/,
cc     2                        16X,'FILE NAME: ',A32/)
cc          IMFIL(IFIL) = 1
cc        ENDIF
        ENDIF
C
C  CHECK MEASUREMENT TYPE
        IF(MEATYP(IFIL).NE.2 .AND. MEATYP(IFIL).NE.3) THEN
          WRITE(LFNERR,21) CODOBS(IFIL),MEATYP
21        FORMAT(/' *** PG CODSPP: MEASUREMENT TYPE NOT ALLOWED'/,
     1                        16X,'FILE NAME       : ',A32/,
     2                        16X,'MEASUREMENT TYPE: ',I3/)
          IMFIL(IFIL) = 1
        ENDIF
C
C  CHECK DIFFERENCE LEVEL (ZERO DIFF.)
        IF(NDIFF(IFIL).NE.0) THEN
          WRITE(LFNERR,22) CODOBS(IFIL),NDIFF(IFIL)
22        FORMAT(/' *** PG CODSPP: ONLY ZERO DIFF. FILES ALLOWED'/,
     1                        16X,'FILE NAME       : ',A32/,
     2                        16X,'DIFF. LEVEL     : ',I3/)
          IMFIL(IFIL) = 1
        ENDIF
C
C  CHECK FREQUENCY
        IF(FILFRQ(IFIL).GT.1.AND.NFREQ(IFIL).EQ.1) THEN
          WRITE(LFNERR,23) CODOBS(IFIL),FILFRQ(IFIL)
23        FORMAT(/,' *** PG CODSPP: FREQUENCY NOT ALLOWED. ONLY L1 ',
     1            'AVAILABLE',/,
     2                        16X,'FILE NAME       : ',A32,/,
     3                        16X,'FREQUENCY       : L',I1,/)
          IF (FILFRQ(IFIL).EQ.3) THEN
            WRITE(LFNERR,24)
24          FORMAT(16X,'FREQUENCY CHANGED TO L1',/)
            FILFRQ(IFIL)=1
          ELSE
            WRITE(LFNERR,'( )')
            IMFIL(IFIL)=1
          ENDIF
        ENDIF
C
C CHECK PERCENTAGE OF L2 COMPARED TO L1 OBSERVATIONS
        IF(FILFRQ(IFIL).EQ.3) THEN
          IF (NUMOBT(1,IFIL).GT.0) THEN
            L2TOL1=IDNINT(DBLE(NUMOBT(2,IFIL))/
     1                    DBLE(NUMOBT(1,IFIL))*100.D0)

            IF (L2TOL1.LT.50) THEN
              WRITE(LFNERR,25) CODOBS(IFIL),FILFRQ(IFIL),L2TOL1
25            FORMAT(/,' ### PG CODSPP: NOT ENOUGH L2 OBSERVATIONS ',
     1                'AVAILABLE',/,
     2                            16X,'FILE NAME       : ',A32,/,
     3                            16X,'FREQUENCY       : L',I1,/,
     4                            16X,'PERCENTAGE L2/L1: ',I2,/,
     5                            16X,'FREQUENCY CHANGED TO L1',/)
              FILFRQ(IFIL)=1
            ENDIF
          ENDIF
        ENDIF
C
100   CONTINUE
C
C  WRITE HEADER OF RESIDUAL FILE
C  -----------------------------
      CALL GTFLNA(0,'RESIDRS',RESFIL,IRC)
      IF (RESFIL .NE. ' ') THEN
C
C INIT RESIDUAL HEADER RECORD
C ---------------------------
        RESHED%TITLE = TITLE
        RESHED%DSC   = resHed_CODSPP
        RESHED%NFIL  = NFTOT
        ALLOCATE(RESHED%FILHEAD(NFTOT),STAT=irc)
        CALL ALCERR(IRC,'RESHED%FILHEAD',(/NFTOT/),pgName)
        DO IFIL=1,NFTOT
          CALL init_filhead(RESHED%FILHEAD(IFIL))
          RESHED%FILHEAD(IFIL)%MEATYP    = MEATYP(IFIL)
          RESHED%FILHEAD(IFIL)%NFRFIL    = 1
          RESHED%FILHEAD(IFIL)%ICARR(1)  = FILFRQ(IFIL)
          RESHED%FILHEAD(IFIL)%STANAM(1) = STNAME(STFIL(1,IFIL))
          RESHED%FILHEAD(IFIL)%CSESS(:)  = CSESS(:,IFIL)
          RESHED%FILHEAD(IFIL)%IDELTT    = IDELTT(IFIL)
          RESHED%FILHEAD(IFIL)%TIMREF    = TIMREF(IFIL)
          RESHED%FILHEAD(IFIL)%NSATEL    = NSATEL(IFIL)
          ALLOCATE(RESHED%FILHEAD(IFIL)%NUMSAT(NSATEL(IFIL)),STAT=IRC)
          CALL ALCERR(IRC,'RESHED%FILHEAD(IFIL)%NUMSAT',
     1               (/NSATEL(IFIL)/),pgName)
          RESHED%FILHEAD(IFIL)%NUMSAT(:) = SATNUM(1:NSATEL(IFIL),IFIL)
        ENDDO
C
C WRITE THE RESIDUAL FILE HEADER
C ------------------------------
        CALL WTRESH2(LFNRES,RESHED)
C
        DO IFIL=1,NFTOT
          DEALLOCATE(RESHED%FILHEAD(IFIL)%NUMSAT,STAT=IRC)
        ENDDO
        DEALLOCATE(RESHED%FILHEAD,STAT=irc)
C
        IRSSAV = 1
      ENDIF
C
C  GET STATION COORDINATES AND ECCENTRICITY INFORMATION
C  ----------------------------------------------------
      CALL GETSTA(NSTAT,STNAME,STANUM,NCENTR,ICENTR,
     1            XSTAT,XSTELL,XSTECC,
     2            DATUM,AELL,BELL,DXELL,DRELL,SCELL)
C
C PREPARE CLOCK RINEX DATA RECORD
C -------------------------------
      IF (LEN_TRIM(crxfil) > 0) THEN
C set the coordinates
        ClkHed%TRFName = DATUM
        clkHed%stacoord(:,:)=xstat(:,1:nstat)
C
C allocate and fill the reference clock array
        clkHed%numRef  = 1
        ALLOCATE(ClkHed%Ref(clkHed%numRef),stat=ios)
        CALL alcerr(ios,'ClkHed%Ref',(/clkHed%numRef/),pgName)
        CALL init_ref(ClkHed%Ref(1))
        ClkHed%ref(1)%refWin%t=0d0
        IF (mixed == 1) THEN
          ClkHed%Ref(1)%nRef = 0
          DO iSat=1,ClkHed%nSat
            IF (ClkHed%ClkName(ClkHed%nSta+iSat)(1:1) == 'G')
     1        ClkHed%Ref(1)%nRef = ClkHed%Ref(1)%nRef + 1
          ENDDO
        ELSE
          ClkHed%Ref(1)%nRef = ClkHed%nSat
        ENDIF
!!!!!!!!!! Problems with ifc81 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#ifdef CMP_IFC8
        numRef = ClkHed%ref(1)%nRef
        ALLOCATE(ClkHed%ref(1)%clk(numRef),stat=ios)
        CALL alcerr(ios,'ClkHed%ref(1)%clk',
     1                       (/numRef/),pgName)
#else
        ALLOCATE(ClkHed%ref(1)%clk(ClkHed%ref(1)%nRef),stat=ios)
        CALL alcerr(ios,'ClkHed%ref(1)%clk',
     1                       (/ClkHed%ref(1)%nRef/),pgName)
#endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        IF (mixed == 1) THEN
          ClkHed%Ref(1)%nRef = 0
          DO iSat=1,ClkHed%nSat
            IF (ClkHed%ClkName(ClkHed%nSta+iSat)(1:1) == 'G') THEN
              ClkHed%Ref(1)%nRef = ClkHed%Ref(1)%nRef + 1
              ClkHed%ref(1)%clk(ClkHed%Ref(1)%nRef)%name =
     1                             ClkHed%ClkName(ClkHed%nSta+iSat)
            ENDIF
          ENDDO
        ELSE
          ClkHed%ref(1)%clk(1:ClkHed%nSat)%name =
     1             ClkHed%ClkName(ClkHed%nSta+1:ClkHed%nSta+ClkHed%nSat)
        ENDIF
        ClkHed%ref(1)%clk(:)%sigma = 0d0
C
C Allocate and init the data record for clock rinex
        SAMPL=iMinTT*nsampl
        CALL osampl(allwin%t(1),SAMPL,1D0,tsampl,tprev,tnext)
        IF (tsampl.NE.0D0) THEN
          ClkHed%TFirst = tsampl
        ELSE
          ClkHed%TFirst = tnext
        ENDIF
        ClkRec%nEpo=NINT((allwin%t(2)-allwin%t(1))*
     1                    86400d0/(iMinTT*NSAMPL)+1d0)
        nClk=ClkHed%nSta+ClkHed%nSat
        ALLOCATE(ClkRec%Epoch(ClkRec%nEpo),stat=ios)
        CALL alcerr(ios,'ClkRec%Epoch',(/ClkRec%nEpo/),pgName)
        ALLOCATE(ClkRec%Clock(nClk,ClkRec%nEpo),stat=ios)
        CALL alcerr(ios,'ClkRec%Clock',(/nClk,ClkRec%nEpo/),pgName)
        ALLOCATE(ClkRec%Sigma(nClk,ClkRec%nEpo),stat=ios)
        CALL alcerr(ios,'ClkRec%Sigma',(/nClk,ClkRec%nEpo/),pgName)
        ALLOCATE(ClkRec%clkFlg(nClk,ClkRec%nEpo),stat=ios)
        CALL alcerr(ios,'ClkRec%clkFlg',(/nClk,ClkRec%nEpo/),pgName)
C
        ClkRec%Epoch(1)=0D0
        DO iEpo=2,ClkRec%nEpo
          tsampl=ClkHed%TFirst+ClkRec%Epoch(iEpo-1)/86400D0
          CALL osampl(tsampl,SAMPL,1D0,tsampl,tprev,tnext)
          ClkRec%Epoch(iEpo)=DNINT((tnext-ClkHed%TFirst)*86400D0)
        ENDDO
        ClkRec%Clock=unDef
        ClkRec%Sigma=0d0
        DO iEpo = 1,clkRec%nEpo
          DO iClk = 1,nClk
            DO ii = 0,7
              CALL clrflg(ClkRec%clkFlg(iClk,iEpo),ii)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C
C OPEN KINEMATIC OUTPUT FILE IF NECESSARY
C ---------------------------------------
      LFNKOU=0
      IF (KINEST.EQ.1) THEN
        CALL GTFLNA(0,'KINOUT ',FILKOU,IRCKOU)
        IF (IRCKOU.EQ.0) THEN
          LFNKOU=LFN002+3
          CALL OPNFIL(LFNKOU,FILKOU,'NEW','FORMATTED',
     1            ' ',' ',IOSTAT)
          CALL OPNERR(LFNERR,LFNKOU,IOSTAT,FILKOU,pgName)
C
          CALL TIMST2(1,1,TIMREF(1),TSTRNG)
          WRITE(LFNKOU,10) TITLE,DATUM,TSTRNG
10        FORMAT(A80,/,80('-'),/,
     1             'LOCAL GEODETIC DATUM: ',A16,2X,'EPOCH: ',A19,//,
     2             ' STATION NAME     WEEK  SECONDS',8X,
     3             'X (M)',10X,'Y (M)',10X,'Z (M)',4X,'F',/)
        ENDIF
      ENDIF
C
C INITIALIZE NUMBER OF DELETIONS
C ------------------------------
      NDEL=0
C
C Initialize antenna buffer for one station and max n satellites
C
      CALL init_buf(bufsize=(/1,MAXVAL(NSATEL)/))
C
C INIT THE BUFFER FOR VIENNA GRID FILES
C -------------------------------------
      CALL initGridBuffer(bufferSize=1)
      CALL prtGridInfo
C
C  LOOP OVER FILES
C  ---------------
C
      DO 500 IFIL=1,NFTOT
C
C  CHECK IF SATELLITES ARE SET ACTIVE IN SATELLITE FILE AND SET
C  THE CORRESPONDING FREQUENCIES (CF.'I:COMFREQ')
C  ------------------------------------------------------------
        EPOFRQ(1)=TIMREF(IFIL)
        EPOFRQ(2)=TIMREF(IFIL)+(NEPOCH(IFIL)-1)*IDELTT(IFIL)/86400.D0
        OBST='C'
        CALL DEFREQ(EPOFRQ,NSATEL(IFIL),SATNUM(1,IFIL),
     1         USEGEOS=USEGEOS(IFIL),GOBSDEF=GOBSDEF(IFIL),MEATYPC=OBST)
c
        IF (ASSOCIATED(GOBSDEF(IFIL)%SAT)) THEN
            DEALLOCATE(GOBSDEF(IFIL)%SAT,STAT=IRC)
        ENDIF
c
        SAMPL=IDELTT(IFIL)*nsampl
C
C INITIALIZE SATELLITE DEPENDED ACCURACY
        DO 501 ISAT=1,MAXSAT
          RMSSAT(ISAT,IFIL) = 0
501     CONTINUE
C
C CHECK FILE STATUS
        IF (IMFIL(IFIL).NE.0) GOTO 500
C
C EPOCH FOR COORDINATES
        TIMCRD=(EPOFRQ(1)+EPOFRQ(2))/2.D0
C
C  SAVE FLAG FOR PHASE FILES
        IPHSAV(IFIL) = 0
C
C  INTERNAL STATION NUMBER, INTERNAL CAMPAIGN NUMBER
        ISTA=STFIL(1,IFIL)
        ICMP=ICAMPN(IFIL)
C
C  PARAMETER ESTIMATION FOR ONE FILE
C  ---------------------------------
        CALL CDCOMP(PRIOPT,NSAMPL,ZENMAX,SECIPL,FILWIN(1,IFIL),ITROPO,
     1            IONO,ICORFL,ICLPOL(IFIL),USEMRK,CODOBS(IFIL),
     2            FILFRQ(IFIL),NSATEL(IFIL),IDELTT(IFIL),TIMREF(IFIL),
     3            STNAME(IFIL),POSECC(1,1,IFIL),RECTYP(1,IFIL),
     4            ANTTYP(1,IFIL),IANTEN(1,IFIL),CSESS(1,IFIL),
     5            SATNUM(1,IFIL),XSTAT(1,ISTA),XSTELL(1,ISTA),AELL,BELL,
     6            DXELL,DRELL,SCELL,EPH,CLOCK,IFRMAT(IFIL),IORBFL,
     7            IOUTLR,DIFMAX,CONFID,MINDOF,USRSIG,IFIL,MEATYP(IFIL),
     8            NITER,INDSVN,OBSSAT(1,1,IFIL),XSTANW(1,IFIL),
     9            XSTNEL(1,IFIL),CLKMOD(1,IFIL),OFFS,DOFFS,SIGMA0(IFIL),
     1            NORDIM,DPAR,MCOR(1,IFIL),MELL(1,IFIL),MCLOCK(1,IFIL),
     2            NDEL,LSTDEL,NITERF(IFIL),KINEST,LFNKOU,IRCCMP)
C
        IF (IRCCMP.GT.0) THEN
          IMFIL(IFIL)=1
          GOTO 500
        ENDIF
C
C SAVE OFFSET OF FIRST EPOCH
        IF (ICLPOL(IFIL) .EQ. -1) THEN
          OFFS0(IFIL) = OFFS(1)
        ENDIF
C
C  COMPUTE/WRITE ELEVATIONS/RESIDUALS IF REQUESTED
C  -----------------------------------------------
        CALL CDRESI(IFIL,PGMNAM,CAMPGN(ICMP),STITLE,
     1              ICLPOL(IFIL),PRIOPT,IRSSAV,CODOBS(IFIL),
     2              FILFRQ(IFIL),NORDIM,NSATEL(IFIL),SATNUM(1,IFIL),
     3              INDSVN,DPAR,TIMREF(IFIL),IDELTT(IFIL),OFFS,DOFFS,
     4              KINEST,CLKHED,CLKREC,RMSSAT)
C
C WRITE CLOCK DATA INTO PHASE FILE HEADER(S) (L1,L2), OR NOT
C ----------------------------------------------------------
        IF (ICLKSV(IFIL).NE.0 .OR. LEN_TRIM(crxfil) > 0) THEN
          CALL CDSCLK(ICLKSV(IFIL),CODHED(IFIL),CODOBS(IFIL),
     1              PHAHED(IFIL),PHAOBS(IFIL),ICLPOL(IFIL),
     2              CLKMOD(1,IFIL),MCLOCK(1,IFIL),OFFS,
     3              SATNUM(1,IFIL),NUMOB1,NUMMR1,NUMAMB(IFIL),
     4              AMBSAT(1,IFIL),AMBIEP(1,IFIL),AMBWLF(1,1,IFIL),
     5              AMBIGU(1,1,IFIL),AMBCLS(1,1,IFIL),NRSAT,OBSFLG,
     6              OBSERV,IFIL,IOUTSV(IFIL),NDEL,LSTDEL,
     7              ifil,clkhed,ClkRec,SAMPL,IPHSAV(IFIL))
        END IF
C
C NEXT FILE
C ---------
500   CONTINUE
      IF (IRSSAV.EQ.1) CLOSE(LFNRES)
C
C CLOSE KINEMATIC OUTPUT FILE
C ---------------------------
      IF (LFNKOU.NE.0) CLOSE(LFNKOU)
C
C PRINT ALL RESULTS
C -----------------
      CALL CDRESU(PGMNAM,CAMPGN,STITLE,NSAMPL,ZENMAX,OBSWIN,FILWIN,
     1            ITROPO,IONO,ICLPOL,ICLKSV,IOUTSV,NFTOT,
     2            CODOBS,FILFRQ,ICAMPN,
     3            STNAME,STFIL,
     4            XSTAT,XSTELL,DATUM,
     5            IRUNIT,NEPOCH,IDELTT,TIMREF,NSATEL,SATNUM,
     6            OBSSAT,XSTANW,MCOR,XSTNEL,MELL,CLKMOD,MCLOCK,
     7            SIGMA0,OFFS0,IMFIL,CSESS,RMSSAT,
     8            IOUTLR,DIFMAX,CONFID,MINDOF,USRSIG,NDEL,LSTDEL,
     9            NITERF,TIMOFF,SIGOFF,INDEX,KINEST)
C
C SAVE SATELLITE CLOCKS CORRECTED FOR GLONASS/GPS TIME OFFSET
C -----------------------------------------------------------
      CALL UPSCLK(TITLE,MIXED,TIMOFF,SIGOFF)
C
C Write clock rinex file
C ----------------------
      IF (LEN_TRIM(crxfil) > 0) THEN
C
C add satellite clocks
        DO iEpo=1,clkRec%nEpo
          DO iSta=1,ClkHed%nSta
            IF (clkrec%clock(ista,iepo) /= undef) THEN
              DO iSat=ClkHed%nSta+1, ClkHed%nSta+ClkHed%nSat
                READ(ClkHed%ClkName(iSat)(2:3),*) numsat
                IF (ClkHed%ClkName(ISat)(1:1) == 'R') numsat=numSat+100
                CALL gtsclk(-1,clkHed%TFirst+clkrec%epoch(iepo)/86400d0,
     1                      numsat,secIpl,0,clkrec%clock(isat,iepo),irc)
                IF (clkrec%clock(isat,iepo) == 0d0 .OR. irc .NE.0 ) THEN
                  clkrec%clock(isat,iepo) = undef
                  clkrec%sigma(isat,iepo) = undef
                  DO ii = 0,7
                    CALL clrflg(clkrec%clkflg(isat,iepo),ii)
                  ENDDO
                ELSE IF (MIXED == 1 .AND.
     1                   ClkHed%ClkName(ISat)(1:1) == 'R') THEN
                  clkrec%clock(isat,iepo) =
     1                        (clkrec%clock(isat,iepo)-TIMOFF)
                  clkrec%sigma(isat,iepo) = SIGOFF
                ELSE
                  clkrec%sigma(isat,iepo) = undef
                ENDIF
              ENDDO
              EXIT
            ENDIF
          ENDDO
        ENDDO
        DO iEpo=1,clkRec%nEpo
          DO iClk=1, ClkHed%nSta+ClkHed%nSat
            IF ( ClkRec%Clock(iClk,iEpo) .NE. undef )
     1        ClkRec%Clock(iClk,iEpo)=ClkRec%Clock(iClk,iEpo)*1D6
            IF ( ClkRec%Sigma(iClk,iEpo) .NE. undef )
     1        ClkRec%Sigma(iClk,iEpo)=ClkRec%Sigma(iClk,iEpo)*1D6
          ENDDO
        ENDDO
        CALL opnfil(lfn001,crxfil,'UNKNOWN','FORMATTED',' ',' ',irCode)
        CALL opnerr(lfnerr,lfn001,irCode,crxfil,pgName)
        ClkHed%pcvstr=''
        CALL gtsensor(pcvmod=ClkHed%pcvstr(1:10))
        ClkHed%LeapSec=NINT(DGPSUT(clkHed%TFirst))
        ClkHed%pgmnam = 'CODSPP V'//PGMVER//'      '
        CALL wtcrxh(lfn001, lfnerr, ClkHed, irCode)
        CALL wtcrxr(lfn001,lfnerr,ClkHed,ClkRec,irCode)
        CLOSE(lfn001)
      ENDIF

C
C SAVE COORDINATES IN STATION COORDINATE FILE
C -------------------------------------------
      CALL GTFLNA(0,'COORDRS',CORFIL,IRC)
      IF (CORFIL .NE. ' ') ICORSV = 1
      CALL CDSCOR('C',STITLE,ICORSV,ICORFL,NSTAT,STNAME,XSTAT,XSTECC,
     1            ICENTR,NFTOT,STFIL,XSTANW,TIMCRD)
C
C SAVE RECEIVER-SPECIFIC DCB DIFFERENCES BETWEEN GLONASS AND GPS
C --------------------------------------------------------------
      CALL GTFLNA(0,'DCBOUT ',DCBFIL,IRC)
      IF (DCBFIL.NE.' ') THEN
        NUMREC=0
        DO IFIL=1,NFTOT
          IF (FILFRQ(IFIL).EQ.3 .AND.
     1        ICLPOL(IFIL).EQ.-1 .AND.
     2        CLKMOD(1,IFIL).NE.0.D0 .AND.
     3        MCLOCK(1,IFIL).NE.0.D0) THEN
            NUMREC=NUMREC+1
            DCBID2(NUMREC)=STNAME(STFIL(1,IFIL))
            DCBVA2(1,NUMREC)=CLKMOD(1,IFIL)*1.D9
            DCBVA2(2,NUMREC)=MCLOCK(1,IFIL)*1.D9
            DCBSYS(NUMREC)='R'
C           ADD_GNSS_HERE ?
          ENDIF
        ENDDO
        IF (NUMREC.GT.0) THEN
          NUMSAT=0
          NUMIFB=0
          ICBTYP=3
          CALL WTCBFL(DCBFIL,TITLE ,NUMSAT,NUMREC,NUMIFB,ICBTYP,DCBID1,
     1                DCBVA1,DCBID2,DCBVA2,DCBSYS,DCBID3,DCBVA3,DCBIN1,
     2                DCBIN2,DCBIN3)
        ELSE
          WRITE(LFNERR,901)
901       FORMAT(/,' ### PG CODSPP: NO SYSTEM TIME OFFSETS COMPUTED',/)
        ENDIF
      ENDIF
C
9000  CONTINUE
      CALL EXITRC(IRCODE)
C
      END

