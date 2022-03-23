C*
      PROGRAM CLKEST
CC
CC NAME       :  CLKEST
CC
CC PURPOSE    :  ESTIMATION OF RECEIVER AND SATELLITE CLOCKS
CC               USING CODE AND/OR PHASE OBSERVATIONS
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER
CC
CC CREATED    :  99/05/14
CC
CC CHANGES    :  05-APR-01 : HB: SWITCH TO NEW MENU SYSTEM
CC               25-JUL-01 : HU: ANTENNA PHASE CENTER IMPLEMENTED
CC               11-AUG-01 : HU: TIDES IMPLEMENTED
CC               16-DEC-01 : HU: D_CONST ADDED
CC               22-DEC-01 : HU: FORMAT STATEMENT CORRECTED
CC               03-JAN-01 : HU: DIVERSE CHANGES
CC               07-MAY-02 : SS: DCB UPDATE
CC               05-JUL-02 : HB: CALL DEFREQ TO DEFINE FREQUENCIES
CC               18-JUL-02 : HU: OPT STRUCTURE AND SAMPLING MODIFIED
CC               24-JUL-02 : HU: CHECK AVAILABILITY OF TRP PARAMETERS
CC               11-AUG-02 : HU: SELECTION OF REFERENCE CLOCK REVISED
CC               12-AUG-02 : HU: CALL OF CEALLIGN MODIFIED
CC               31-AUG-02 : HU: SELECTION OF REFERENCE CLOCK IMPROVED
CC               25-SEP-02 : HU: REMOVE I_ASTLIB, USE INTERFACE TO RDHEAD2
CC               04-DEC-02 : RS: CALL OF PRANGE MODIFIED
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               23-APR-03 : HU: NULLIFY LOCAL POINTERS
CC               15-MAY-03 : HU: INITIALIZE STRUCTURES
CC               19-MAY-03 : RD: INIT TIME WINDOW TO (/0d0,1d20/)
CC               28-MAY-03 : RD: NEW CALL OF SR GPHECC
CC               07-JUL-03 : MM: NEW GETTRP CALL
CC               11-AUG-03 : RS: ADD CALL OF GTSATA, APPLY SATELLITE
CC                               ANTENNA PHASE CENTER VARIATIONS,
CC                               RECTYP=' ' IN 3 CALLS OF GPHECC
CC               08-SEP-03 : HU: DELETE UNDEFINED CLOCKS AT FIXING EPOCHS
CC                               USE NEW ROUTINES TO WRITE CLK RINEX
CC               09-SEP-03 : HU: ARRAY EPOC REMOVED
CC               13-SEP-03 : HU: INTERFACE FOR DEFREQ, WTCRXH, WTCRXR
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               01-DEC-03 : HB: INITIALIZE CLK-SIGMAS TO 999999.999999D0
CC                               INDSIG AND INDSVN AS REAL ARRAYS
CC               26-JAN-04 : HB: TEST FOR ZENMAX ALREADY FOR SR GPHECC
CC               27-JAN-04 : HU: CALL OF GOBSEP CHANGED
CC               30-JAN-04 : HB: MODIFICATION OF MISSING EPOCH OUTPUT
CC               16-MAR-04 : HB: MODIFICATION OF MISSING EPOCH OUTPUT (2)
CC               23-MAR-04 : HB: MOVE SUMMARY TABLES IN "EPOCH"-FILE
CC               08-APR-04 : RS: ADD AZISOK, CHANGE CALLS OF PRANGE AND GPHECC
CC               16-APR-04 : HB: CORRECT WRONG ERROR HANDLING
CC               30-Apr-04 : HB: CORRECT HANDLING OF FIXED EPOCHS => cancelled
CC               04-May-04 : HB: CORRECT HANDLING OF FIXED EPOCHS
CC               07-Jun-04 : HB: CORRECT WRITING OF STATION INFORMATION IN
CC                               CLOCK RINEX OUTPUT FILE
CC               20-Oct-04 : RD: NEW CALL OF GFRQEP
CC               20-Jan-05 : HB: WGTCLD=0.D0 FOR CODE-DIFFERENCE IF OPT%NOCODE=1
CC               21-JUN-05 : MM: LFNUM.inc REMOVED (LFNUMs IN M_BERN)
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               08-Aug-05 : HB: Use new SR TIMST2 (module)
CC               04-NOV-05 : HB: ALLIGNMENT ONLY IF STATION CLOCK IS REFERENCE
CC                               AT THE END
CC               09-NOV-05 : AG: SENNUM for GTSATA & GPHECC CALLs ADDED
CC               24-NOV-05 : RD: CONSIDER IRC FROM GTSCLM FOR SYNCHRONIZATION
CC                               CORRECT FOR SATCLK WHEN CHECKING OMCMXC
CC                               CORRECT THE INDEX IN DTFIL FOR CODE OBS.
CC               07-FEB-06 : RD: REPORT MISSING INTERVALS (INSTEAD OF EPOCHS)
CC               08-Feb-06 : HB: USE NEW SR GTSCLK INSTEAD OF GTSCLM
CC               24-Mar-06 : RD: CORRECT INDEX IN STNAME FOR "NO TROPO" MSG.
CC               12-JUN-06 : HB/RD: INDEX CORRECTION FOR GETTRP
CC               29-JUN-06 : HB: ADD PARAMETER SATC FOR SR PRANGE
CC               18-Jul-06 : AG: CMC added
CC               17-Aug-06 : HU: USE FOR S_GOBSEP, S_GFREQP
CC               12-Aug-06 : RD: hlpirc added
CC               29-Aug-06 : RD/AG: CORRECTION IF LFIL.EQ.0
CC               04-Oct-06 : AG: CORRECTION IF KFIL.EQ.0
CC               24-NOV-06 : AG: LEAP SECOND, PGMNAM, PCVSTR SET IN CLKHED
CC               29-NOV-06 : AG: PGMNAM, PCVSTR and DCBSTR from input CLK-RINEX
CC               27-Feb-07 : AG: CALL DEFCON WITH PARAMETER
CC               12-Jun-07 : AG: USE D_PHAECC INSTEAD OF GPHECC
CC               08-Aug-07 : AG: INDICES CORRECTED
CC               01-NOV-07 : HB: ADD PARAMETER SECIPL FOR GTSCLK
CC               11-Dec-07 : HB: Check activated, whether reference clock is
CC                               present in observation data
CC               28-Mar-08 : HB: Allocate arrays from MAXFIL and MAXEPO
CC               04-Aug-08 : HB: Modify formats for writing problem statistic
CC               08-Sep-08 : HB: Improve layout of PRC-file
CC               01-Oct-08 : HB: Add parameter ZENION to SR PRANGE
CC               02-Feb-09 : HB/RD/AS: A priori clock from first code
CC                               iteration applied (GALILEO satellites)
CC               04-May-09 : RD: CONSIDER CORRECTIONS FROM VIENNA GRID FILES
CC               09-MAY-09 : RD: NEW CALL OF DCBCOR
CC               29-MAY-09 : RD: "T" FLAG IS RESETED  FOR ALL ITERATIONS
CC               29-May-09 : RD: New call of GOBSEP
CC               29-MAY-09 : RD: INPUT CLOCKS ALSO FROM INPUT CLK RNX FILE
CC               22-JUN-09 : HB: SET STCLK AND MEPOF ARRAYS TO ZERO
CC               21-SEP-09 : RD: Eclipsing flag for clock results added
CC               25-MAY-10 : MF: Init ClkHead, ClkHead%ref, ClkRec
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               01-OCT-10 : CR: NEW CALL OF SUNEFF
CC               03-DEC-10 : RD: CMC FOR ATL ADDED
CC               19-JAN-11 : RD: Use GETSTA
CC               26-JAN-11 : LP: Sat-specific obs types; DEFREQ changes
CC               17-FEB-11 : RD: COMMON MCMSTA NOT NEEDED ANYMORE
CC               12-APR-11 : CR: SWITCH FOR PERIODIC RELATIVISTIC J2-CORRECTION
CC               11-NOV-11 : MM: TAKE SATELLITE PCOs FROM PCV FILE
CC               24-NOV-11 : SL: new title string for pritit, m_bern with ONLY
CC               05-MAR-12 : RD: USE LISTI4 AS MODULE NOW
CC               28-MAR-12 : RD: USE SVN2CHR AS MODULE NOW
CC               28-MAR-12 : RD: REMOVE UNUSED VARIABLES FROM CESREF
CC               10-JUL-12 : RD: REMOVE UNUSED VARIABLES FROM ESTCLD, ESTCLK
CC               19-SEP-12 : RD: REMOVE UNUSED VARIABLES FROM CECOMB
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1999     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: i4b, r8b, staNameLength, filenamelength,
     1                    timstrglength2,
     1                    lfnPrt, lfnErr, lfnOr1, lfnPlt, lfnRes,
     1                    lfn001, lfn002, lfnRp1, pgmVer
      USE m_cpu,    ONLY: cpu_start
      USE m_time,   ONLY: t_timint
      USE m_maxdim, ONLY: maxamb, maxsat, maxshd
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE d_const,  ONLY: c,date,time,pi
      USE d_clkrnx, ONLY: t_clkhead,t_clkrec,init_clkhead,init_clkrec,
     1                    unDef,init_ref
      USE d_gpsobs, ONLY: t_obsHead,init_obsHead
      USE d_phaecc, ONLY: init_buf,sta_off,sat_off
      USE d_satfil, ONLY: typeMWTR
      USE d_grid,   ONLY: initGridBuffer,prtGridInfo
      USE d_rinex3, ONLY: t_gobsdef
      USE p_clkest, ONLY: t_clkopt

      USE s_gnssatti
      USE s_gtfile2
      USE s_gtsensor
      USE f_dgpsut
      USE s_iordup
      USE s_alcerr
      USE s_opnfil
      USE s_prflna
      USE s_wtcrxr
      USE s_stamp
      USE s_cootra
      USE s_prange
      USE s_cecomb
      USE s_pritit
      USE s_gettrp
      USE s_svn2chr
      USE s_getorb
      USE s_readinpf
      USE s_opnerr
      USE s_dcbcor
      USE s_prfile
      USE s_timst2
      USE s_wtsath
      USE s_wtsati
      USE s_rdhead2
      USE s_gobsep
      USE s_xyztim
      USE s_estcld
      USE s_rdclkn
      USE s_getsta
      USE s_defreq
      USE s_estclk
      USE s_rdcrxh
      USE s_cesref
      USE s_defcon
      USE s_exitrc
      USE s_gfrqep
      USE s_cetref
      USE s_opnsys
      USE s_ellecc
      USE s_priwin
      USE s_rdcrxr
      USE s_gtflna
      USE s_gtsclk
      USE s_cewckd
      USE s_ceallign
      USE s_wtcrxh
      USE s_setflg
      USE s_clrflg
      USE s_dimtst
      USE s_shadow1
      USE s_suneff
      USE s_gobsdef,ONLY: init_geos, setgeos
      USE f_listi4
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 ICLK   , ICLUS  , ICLUS2 , ICRARC , IEP    , IEPO   ,
     1          IFFIL  , IFIL   , IFOUND , II     , ILST   ,
     2          IND    , INDFIL , INDSAT , INDSTA , INIPHA ,
     3          INUM   , IORSYS , IOS    , IOSTAT , IP     , IPF    ,
     4          IPOS   , IRC    , IRCCLK , IRCPCV , IRCCLKO,
     5          IRCRNX , IRCRNXI, IREC   , IREF   , IRETC  , IRETRN ,
     6          IRFCLK , IRFCLT , IRFCRX , IRFOUT , IRFRNX , IRFSAC ,
     7          IRFSAP , IRFSAT , IRFSIT , ISATCO , ISASYS ,
     8          ISAT   , ISATCL , ISATPIE, ISLCLK , ISTA   , ISTACL ,
     9          ISTAPIE, ISTAREF, IT     , ITER   , ITRGRD , ITRMAP ,
     1          ITRPMD , ITYP   , IZEROD , JCLK   , JCLUS  , JFIL   ,
     2          JREF   , JRFCLK , JSAT   , JSTA   , K      , KFIL   ,
     3          KOBS   , KSAT   , LFIL   , LOBS   , LSAT   ,
     4          MAXFRQ , MEA    , MOBS   , MXCAMB ,
     5          MXCFRQ , MXCSAT , NCENTR , NCLK   , NCLUS  ,
     6          NDEL   , NEPO   , NEPO1  , NFCOLD ,
     7          NFILPH , NFLCOL , NFTOT  , NITER  , NMARC  , NMARK  ,
     8          NMESS  , NOBSCL , NOCLK  , NOREF  , NOTALL , NOTRP  ,
     9          NSACLK , NSATPH , NSCOLD , NSUM   ,
     1          IFRQ   , ICOM   , NSHD   , ISHD   , ISHAD
C
      REAL*8    AELL   , APRWGP , AZI    , BELL   , BRDEPO , BRDOFF ,
     1          BRDRFT , CLKLIN , DIST   , DTCOD  , DTFCOD , DTSATC ,
     2          EPORNX , GPSSEC , REFCLK , RESRES , RMSCOD , RMSPHA ,
     3          SATPIE , SCELL  , STAPIE , SUM    , SUMSAT , RDUMMY ,
     4          SZ     , TFRAC  , TIM0   , TLASTC , TLASTP ,
     5          TOBS   , TOBSP  , TOSC   , TPREV  , UT1GPS , XPOL   ,
     6          YPOL   , ZEN    , SATC   , nadMax , ZENION
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C MAXIMAL DIMENSIONS
C ------------------
ccc      PARAMETER (MAXFRQ=4,MAXEPO=2882,MAXFIL=300)
      PARAMETER (MAXFRQ=4)
!!      PARAMETER (MAXFRQ=4,MAXEPO=17300,MAXFIL=300)
CC ONESEC      PARAMETER (MAXFRQ=4,MAXEPO=86401,MAXFIL=106)
      INCLUDE 'COMFREQ.inc'
C
C MAXSAT: MAXIMUM NUMBER OF SATELLITES
C MAXFIL: MAXIMUM NUMBER OF FILES TO BE PROCESSED
C MAXEPO: MAXIMUM NUMBER OF EPOCHS
C
C DECLARATIONS
C ------------
C
      INTEGER*4    WRTSTA,SVNRF,SVNMOD,SVNNUM,SVNCMB
      INTEGER*4    SVNLST(MAXSAT),SVNFI1(MAXSAT,2),SVNEP(MAXSAT,2)
      INTEGER*4    NSATEP(2),NFILEP(2)
      INTEGER*4    SVNPHA(MAXSAT),PROTOT(9),PROSAT(9,MAXSAT)
      INTEGER*4    SVCOLD(MAXSAT),NOBSPS(MAXSAT),NOBSCS(MAXSAT)
      INTEGER*4    IOBSCL(MAXSAT),IEPOLD(MAXSAT),INDX(MAXSAT)
      INTEGER*4    IPAR(MAXSAT),SVNC(MAXSAT),SVNORD(MAXSAT)
      INTEGER*4    NDFSAT(MAXSAT),INDS(MAXSAT),ISATRX(MAXSAT)
      INTEGER*4    SCKIRC(MAXSAT),HLPIRC(MAXSAT)
      INTEGER*4    SATSHD(MAXSHD),FLGSHD(MAXSHD),obs,norec,usegeos

      INTEGER(i4b),DIMENSION(:,:,:),ALLOCATABLE :: NUMSAT,SVNFIL,SAFLEP

      INTEGER(i4b),DIMENSION(:,:),ALLOCATABLE   :: NSATEL,STANUM,NSATFL
      INTEGER(i4b),DIMENSION(:,:),ALLOCATABLE   :: OBSNUM,FILEP,NSFLEP
      INTEGER(i4b),DIMENSION(:,:),ALLOCATABLE   :: ICARR,SAFLPH,PROTYP
      INTEGER(i4b),DIMENSION(:,:),ALLOCATABLE   :: CLUST,MEPOF,MEPO

      INTEGER(i4b),DIMENSION(:),ALLOCATABLE     :: ICENTR,FILNUM,FILPHA
      INTEGER(i4b),DIMENSION(:),ALLOCATABLE     :: NSFLPH,NDFSIT,INDC
      INTEGER(i4b),DIMENSION(:),ALLOCATABLE     :: IFILRX,IEFOLD,IPARF
      INTEGER(i4b),DIMENSION(:),ALLOCATABLE     :: FILOLD
      INTEGER(i4b),DIMENSION(:),ALLOCATABLE     :: MCLK
      INTEGER(i4b),DIMENSION(:,:),ALLOCATABLE   :: NUMCLK,NUMCLD
      INTEGER(i4b),DIMENSION(2,1)               :: dummy2
C
      REAL(r8b),DIMENSION(:,:,:,:),ALLOCATABLE  :: OBSERV

      REAL(r8b),DIMENSION(:,:,:),ALLOCATABLE    :: DTFIL,OBFLEP

      REAL(r8b),DIMENSION(:,:),ALLOCATABLE      :: XSTAT,XSTA1,XSTA2
      REAL(r8b),DIMENSION(:,:),ALLOCATABLE      :: XSTA3,XSTELL,XSTECC
      REAL(r8b),DIMENSION(:,:),ALLOCATABLE      :: WINDOW,DELTAT,PRNGTL
      REAL(r8b),DIMENSION(:,:),ALLOCATABLE      :: ZENDST, RESIDU,ZENPHA
      REAL(r8b),DIMENSION(:,:),ALLOCATABLE      :: STACLR,STASIG,STCLK
      REAL(r8b),DIMENSION(:,:),ALLOCATABLE      :: DSTCLK,WGTSTA,WGTSTD
      REAL(r8b),DIMENSION(:,:),ALLOCATABLE      :: CLK,WGTCLK,DCLK
      REAL(r8b),DIMENSION(:,:),ALLOCATABLE      :: WGTCLD,SATCLR,SATSIG

      REAL(r8b),DIMENSION(:),ALLOCATABLE        :: RMSSIP,SITCLK,RMSSIT
      REAL(r8b),DIMENSION(:),ALLOCATABLE        :: SITCLP,APRWGT,RESFIL
      REAL(r8b),DIMENSION(:),ALLOCATABLE        :: FILSIG,CLFOLD,CLFOL2
      REAL(r8b),DIMENSION(:),ALLOCATABLE        :: INDSIG,EPOCH

      CHARACTER(LEN=1),DIMENSION(:,:,:,:),ALLOCATABLE :: OBSFLG
      CHARACTER(LEN=1),DIMENSION(:,:),ALLOCATABLE     :: FILACT
      CHARACTER(LEN=1),DIMENSION(:,:),ALLOCATABLE     :: MRKOBS,MRKPHA
      CHARACTER(LEN=staNameLength), DIMENSION(1)      :: dummy1
      CHARACTER(LEN=staNameLength),DIMENSION(:,:),ALLOCATABLE :: STNAME

      REAL*8       DXELL(3),DRELL(3),PHAECC(3),DUM3(3)
      REAL*8       OBSER1(MAXSAT,MAXFRQ,2),ELESAT(7)
      REAL*8       XVA(9,MAXSAT),XVAEP(9),TOPPOS(3),SATCLK(MAXSAT)
      REAL*8       DSACLK(MAXSAT)
      REAL*8       SATCLP(MAXSAT),RMSSAC(MAXSAT),RMSSAP(MAXSAT)
      REAL*8       EXCEN(3),CLKOLD(MAXSAT),CLKOL2(MAXSAT)
      REAL*8       RESSVN(MAXSAT),SVNSIG(MAXSAT),INDSVN(MAXSAT)
      REAL*8       EPOFRQ(2),DRTROP(3),TIMSHD(2,MAXSHD),SUN(4)
      REAL*8       DELTT,NAD,AZISAT,AZISOK
      REAL*8       OFFSET(3),EX(3),EY(3),EZ(3)
C
      CHARACTER*1  SVNCHR,OBSFL1(MAXSAT,MAXFRQ,2)
      CHARACTER*6  MXNSAT,MXNAMB,MXNFRQ
      CHARACTER(LEN=fileNameLength) :: FILCLK,FILCLR,FILCLM,FILAUX,
     1                                 FILRNX,FILTRP
      CHARACTER(LEN=fileNameLength),DIMENSION(:,:),POINTER :: FILNAM
      CHARACTER*16 DATUM
      CHARACTER(LEN=staNameLength) :: STAREF,CLKREF,CLKNAM
      CHARACTER*80 TITCLK
      CHARACTER(LEN=timStrgLength2) :: TSTRNG
C
      LOGICAL   cmcyn(2), doit
C
C Type Declarations
      TYPE(t_clkopt)      :: opt
      TYPE(t_clkhead)     :: InClkHead, ClkHead
      TYPE(t_clkrec)      :: InClkRec,  ClkRec
      TYPE(t_obsHead),DIMENSION(:,:),ALLOCATABLE  :: obshead
      TYPE(t_timint)      :: obsWin
      TYPE(t_timint), DIMENSION(:),  ALLOCATABLE  :: misepo
      TYPE(t_gobsdef)                             :: gobsdef
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMAMB/MXCAMB,MXNAMB
      COMMON/MCMFRQ/MXCFRQ,MXNFRQ
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C INITIALIZE COMMON BLOCKS FOR MAXIMAL DIMENSIONS
C -----------------------------------------------
      MXCSAT=MAXSAT
      MXNSAT='MAXSAT'
      MXCAMB=MAXAMB
      MXNAMB='MAXAMB'
      MXCFRQ=MAXFRQ
      MXNFRQ='MAXFRQ'
C
C NULLIFY POINTERS
C ----------------
      NULLIFY(FILNAM)
      NULLIFY(opt%coment)
      NULLIFY(opt%refsta)
      NULLIFY(opt%rnxsta)
      CALL init_clkhead(InClkHead)
      CALL init_clkhead(  ClkHead)
      CALL init_clkrec(InClkRec)
      CALL init_clkrec(  ClkRec)
      CALL init_inpkey(inpKey)
      nShd = 0
C
C GET THE NAME OF THE INPUT FILE
C ------------------------------
      CALL readinpf(' ',inpKey)
C
C DEFINE SYSTEM FILES
C -------------------
      CALL OPNSYS
      CALL stamp(lfnprt,'Start program')
C
C DEFINE CONSTANTS
C ----------------
      CALL DEFCON(1)
C
C WRITE TITLE
C -----------
      CALL PRITIT('CLKEST','Epoch-wise clock interpolation')
C
C WRITE FILE NAMES
C ----------------
      CALL PRFLNA
      CALL PRFILE('OBSFIL',' ',4,138)
C
C READ ALL INPUT INFORMATION FROM INPUT FILE
C ------------------------------------------
      CALL RDCLKN(opt)
      nadMax=PI/180.D0*17.D0
C
C GET LIST OF FILES TO BE PROCESSED
C ---------------------------------
      NFLCOL=4
      CALL GTFILE2('OBSFIL',NFLCOL,NFTOT,FILNAM)
C
C ALLOCATE FILE ARRAYS
C --------------------
      ALLOCATE (obsHead(nftot,2),STAT=ios)
      CALL alcerr(ios,'obsHead',(/nftot,2/),'CLKEST')
      DO ifil=1,nftot
        DO ii=1,2
          CALL init_obshead(obsHead(ifil,ii))
        ENDDO
      ENDDO

      ALLOCATE (numsat(maxsat,nftot,2),STAT=ios)
      CALL alcerr(ios,'numsat',(/maxsat,nftot,2/),'CLKEST')
      ALLOCATE (svnfil(maxsat,nftot,2),STAT=ios)
      CALL alcerr(ios,'svnfil',(/maxsat,nftot,2/),'CLKEST')
      ALLOCATE (saflep(maxsat,nftot,2),STAT=ios)
      CALL alcerr(ios,'saflep',(/maxsat,nftot,2/),'CLKEST')

      ALLOCATE (nsatel(nftot,2),STAT=ios)
      CALL alcerr(ios,'nsatel',(/nftot,2/),'CLKEST')
      ALLOCATE (nsatfl(nftot,2),STAT=ios)
      CALL alcerr(ios,'nsatfl',(/nftot,2/),'CLKEST')
      ALLOCATE (stanum(nftot,2),STAT=ios)
      CALL alcerr(ios,'stanum',(/nftot,2/),'CLKEST')
      ALLOCATE (obsnum(nftot,2),STAT=ios)
      CALL alcerr(ios,'obsnum',(/nftot,2/),'CLKEST')
      ALLOCATE (filep(nftot,2),STAT=ios)
      CALL alcerr(ios,'filep',(/nftot,2/),'CLKEST')
      ALLOCATE (nsflep(nftot,2),STAT=ios)
      CALL alcerr(ios,'nsflep',(/nftot,2/),'CLKEST')
      ALLOCATE (icarr(maxfrq,nftot),STAT=ios)
      CALL alcerr(ios,'icarr',(/maxfrq,nftot/),'CLKEST')
      ALLOCATE (saflph(maxsat,nftot),STAT=ios)
      CALL alcerr(ios,'saflph',(/maxsat,nftot/),'CLKEST')
      ALLOCATE (protyp(10,nftot),STAT=ios)
      CALL alcerr(ios,'protyp',(/10,nftot/),'CLKEST')
      ALLOCATE (clust(3,maxsat*nftot),STAT=ios)
      CALL alcerr(ios,'clust',(/3,maxsat*nftot/),'CLKEST')

      ALLOCATE (icentr(nftot),STAT=ios)
      CALL alcerr(ios,'icentr',(/nftot/),'CLKEST')
      ALLOCATE (filnum(nftot),STAT=ios)
      CALL alcerr(ios,'filnum',(/nftot/),'CLKEST')
      ALLOCATE (filpha(nftot),STAT=ios)
      CALL alcerr(ios,'filpha',(/nftot/),'CLKEST')
      ALLOCATE (nsflph(nftot),STAT=ios)
      CALL alcerr(ios,'nsflph',(/nftot/),'CLKEST')
      ALLOCATE (ndfsit(nftot),STAT=ios)
      CALL alcerr(ios,'ndfsit',(/nftot/),'CLKEST')
      ALLOCATE (indc(nftot),STAT=ios)
      CALL alcerr(ios,'indc',(/nftot/),'CLKEST')
      ALLOCATE (ifilrx(nftot),STAT=ios)
      CALL alcerr(ios,'ifilrx',(/nftot/),'CLKEST')
      ALLOCATE (iefold(nftot),STAT=ios)
      CALL alcerr(ios,'iefold',(/nftot/),'CLKEST')
      ALLOCATE (iparf(nftot),STAT=ios)
      CALL alcerr(ios,'iparf',(/nftot/),'CLKEST')
      ALLOCATE (filold(nftot),STAT=ios)
      CALL alcerr(ios,'filold',(/nftot/),'CLKEST')

      ALLOCATE (observ(maxsat,maxfrq,nftot,2),STAT=ios)
      CALL alcerr(ios,'observ',(/nftot/),'CLKEST')

      ALLOCATE (dtfil(2,nftot,2),STAT=ios)
      CALL alcerr(ios,'dtfil',(/2,nftot,2/),'CLKEST')
      ALLOCATE (obflep(maxsat,nftot,2),STAT=ios)
      CALL alcerr(ios,'obflep',(/maxsat,nftot,2/),'CLKEST')

      ALLOCATE (xstat(3,nftot),STAT=ios)
      CALL alcerr(ios,'xstat',(/3,nftot/),'CLKEST')
      ALLOCATE (xsta1(3,nftot),STAT=ios)
      CALL alcerr(ios,'xsta1',(/3,nftot/),'CLKEST')
      ALLOCATE (xsta2(3,nftot),STAT=ios)
      CALL alcerr(ios,'xsta2',(/3,nftot/),'CLKEST')
      ALLOCATE (xsta3(3,nftot),STAT=ios)
      CALL alcerr(ios,'xsta3',(/3,nftot/),'CLKEST')
      ALLOCATE (xstell(3,nftot),STAT=ios)
      CALL alcerr(ios,'xstell',(/3,nftot/),'CLKEST')
      ALLOCATE (xstecc(3,nftot),STAT=ios)
      CALL alcerr(ios,'xstecc',(/3,nftot/),'CLKEST')
      ALLOCATE (window(2,nftot),STAT=ios)
      CALL alcerr(ios,'window',(/2,nftot/),'CLKEST')
      ALLOCATE (deltat(nftot,2),STAT=ios)
      CALL alcerr(ios,'deltat',(/nftot,2/),'CLKEST')
      ALLOCATE (zendst(maxsat,nftot),STAT=ios)
      CALL alcerr(ios,'zendst',(/maxsat,nftot/),'CLKEST')
      ALLOCATE (residu(maxsat,nftot),STAT=ios)
      CALL alcerr(ios,'residu',(/maxsat,nftot/),'CLKEST')
      ALLOCATE (zenpha(maxsat,nftot),STAT=ios)
      CALL alcerr(ios,'zenpha',(/maxsat,nftot/),'CLKEST')
      ALLOCATE (prngtl(maxsat,nftot),STAT=ios)
      CALL alcerr(ios,'prngtl',(/maxsat,nftot/),'CLKEST')

      ALLOCATE (rmssip(nftot),STAT=ios)
      CALL alcerr(ios,'rmssip',(/nftot/),'CLKEST')
      ALLOCATE (rmssit(nftot),STAT=ios)
      CALL alcerr(ios,'rmssit',(/nftot/),'CLKEST')
      ALLOCATE (sitclk(nftot),STAT=ios)
      CALL alcerr(ios,'sitclk',(/nftot/),'CLKEST')
      ALLOCATE (sitclp(nftot),STAT=ios)
      CALL alcerr(ios,'sitclp',(/nftot/),'CLKEST')
      ALLOCATE (aprwgt(nftot),STAT=ios)
      CALL alcerr(ios,'aprwgt',(/nftot/),'CLKEST')
      ALLOCATE (resfil(nftot),STAT=ios)
      CALL alcerr(ios,'resfil',(/nftot/),'CLKEST')
      ALLOCATE (filsig(nftot),STAT=ios)
      CALL alcerr(ios,'filsig',(/nftot/),'CLKEST')
      ALLOCATE (clfold(nftot),STAT=ios)
      CALL alcerr(ios,'clfold',(/nftot/),'CLKEST')
      ALLOCATE (clfol2(nftot),STAT=ios)
      CALL alcerr(ios,'clfol2',(/nftot/),'CLKEST')
      ALLOCATE (indsig(nftot),STAT=ios)
      CALL alcerr(ios,'indsig',(/nftot/),'CLKEST')

      ALLOCATE(obsflg(maxsat,maxfrq,nftot,2),STAT=ios)
      CALL alcerr(ios,'obsflg',(/maxsat,maxfrq,nftot,2/),'CLKEST')
      ALLOCATE (filact(nftot,2),STAT=ios)
      CALL alcerr(ios,'filact',(/nftot,2/),'CLKEST')
      ALLOCATE (mrkobs(maxsat,nftot),STAT=ios)
      CALL alcerr(ios,'mrkobs',(/maxsat,nftot/),'CLKEST')
      ALLOCATE (mrkpha(maxsat,nftot),STAT=ios)
      CALL alcerr(ios,'mrkpha',(/maxsat,nftot/),'CLKEST')
      ALLOCATE (stname(nftot,2),STAT=ios)
      CALL alcerr(ios,'stname',(/nftot,2/),'CLKEST')
C
C GET NECESSARY INFORMATION OF CODE AND PHASE HEADER FILES,
C OPEN OBSERVATION FILES
C --------------------------------------------------------
      NEPO1=0
      TIM0 =9D9
      MRKOBS=' '
      SVNNUM=0
      DO IFIL=1,NFTOT
C
C CODE HEADER
C -----------
        CALL rdHead2(filnam(1,ifil),obsHead(ifil,1))

        NSATEL(IFIL,1)=obsHead(ifil,1)%nSatel
        NUMSAT(1:nsatel(ifil,1),IFIL,1)=obsHead(ifil,1)%sat(:)%numSat
C
        TIM0= MIN(TIM0,obsHead(ifil,1)%timRef)
        WINDOW(1,IFIL)=obsHead(ifil,1)%timRef
        WINDOW(2,IFIL)=obsHead(ifil,1)%timRef
     1                +(obsHead(ifil,1)%nEpoch-1)
     2                *obsHead(ifil,1)%iDeltt/86400.D0
C
        STNAME(IFIL,1)=obsHead(ifil,1)%sta(1)%staNam
C
C PHASE HEADER
C ------------
        CALL rdHead2(filnam(3,ifil),obsHead(ifil,2))

        NSATEL(IFIL,2)=obsHead(ifil,2)%nSatel
        NUMSAT(1:nsatel(ifil,2),IFIL,2)=obsHead(ifil,2)%sat(:)%numSat
C
        STNAME(IFIL,2)=obsHead(ifil,2)%sta(1)%staNam
        ICARR(1,IFIL)=1
        ICARR(2,IFIL)=2
        ICARR(3,IFIL)=3
        ICARR(4,IFIL)=4
        NEPO1= MAX(NEPO1,obsHead(ifil,1)%nepoch)
        obsHead(ifil,1:2)%nfreq=4
        FILNUM(IFIL)=IFIL
C
C LIST OF SATELLITE NUMBERS
        DO II=1,2
          DO ISAT=1,NSATEL(IFIL,II)
            IPOS=LISTI4(1,MAXSAT,SVNLST,NUMSAT(ISAT,IFIL,II),SVNNUM)
          ENDDO
        ENDDO
C
C OPEN CODE OBSERVATION FILES
C ---------------------------
        CALL OPNFIL(LFN001+IFIL-1,FILNAM(2,IFIL),'OLD','UNFORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFN001+IFIL-1,IOSTAT,FILNAM(2,IFIL),'CLKEST')
C
C OPEN PHASE OBSERVATION FILES
C ----------------------------
        CALL OPNFIL(LFN002+IFIL-1,FILNAM(4,IFIL),'OLD','UNFORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFN002+IFIL-1,IOSTAT,FILNAM(4,IFIL),
     2              'CLKEST')
      ENDDO

! Allocate arrays for epochs
! --------------------------
      ALLOCATE(mepof(nepo1,nftot),STAT=ios)
      CALL alcerr(ios,'mepof',(/nepo1,nftot/),'CLKEST')
      ALLOCATE(mepo(nepo1,maxsat),STAT=ios)
      CALL alcerr(ios,'mepo',(/nepo1,maxsat/),'CLKEST')

      ALLOCATE(staclr(nftot,nepo1),STAT=ios)
      CALL alcerr(ios,'staclr',(/nftot,nepo1/),'CLKEST')
      ALLOCATE(stasig(nftot,nepo1),STAT=ios)
      CALL alcerr(ios,'stasig',(/nftot,nepo1/),'CLKEST')
      ALLOCATE(stclk(nepo1,nftot),STAT=ios)
      CALL alcerr(ios,'stclk',(/nepo1,nftot/),'CLKEST')
      ALLOCATE(wgtsta(nepo1,nftot),STAT=ios)
      CALL alcerr(ios,'wgtsta',(/nepo1,nftot/),'CLKEST')
      ALLOCATE(dstclk(nepo1,nftot),STAT=ios)
      CALL alcerr(ios,'dstclk',(/nepo1,nftot/),'CLKEST')
      ALLOCATE(wgtstd(nepo1,nftot),STAT=ios)
      CALL alcerr(ios,'wgtstd',(/nepo1,nftot/),'CLKEST')
      ALLOCATE(clk(nepo1,maxsat),STAT=ios)
      CALL alcerr(ios,'clk',(/nepo1,maxsat/),'CLKEST')
      ALLOCATE(wgtclk(nepo1,maxsat),STAT=ios)
      CALL alcerr(ios,'wgtclk',(/nepo1,maxsat/),'CLKEST')
      ALLOCATE(numclk(nepo1,maxsat),STAT=ios)
      CALL alcerr(ios,'numclk',(/nepo1,maxsat/),'CLKEST')
      ALLOCATE(dclk(nepo1,maxsat),STAT=ios)
      CALL alcerr(ios,'dclk',(/nepo1,maxsat/),'CLKEST')
      ALLOCATE(wgtcld(nepo1,maxsat),STAT=ios)
      CALL alcerr(ios,'wgtcld',(/nepo1,maxsat/),'CLKEST')
      ALLOCATE(numcld(nepo1,maxsat),STAT=ios)
      CALL alcerr(ios,'numcld',(/nepo1,maxsat/),'CLKEST')
      ALLOCATE(satclr(maxsat,nepo1),STAT=ios)
      CALL alcerr(ios,'satclr',(/maxsat,nepo1/),'CLKEST')
      ALLOCATE(satsig(maxsat,nepo1),STAT=ios)
      CALL alcerr(ios,'satsig',(/maxsat,nepo1/),'CLKEST')

      ALLOCATE(epoch(nepo1),STAT=ios)
      CALL alcerr(ios,'epoch',(/nepo1/),'CLKEST')
      epoch = 0d0
C
C Initialize antenna buffer
C -------------------------
      CALL init_buf(bufsize=(/NFTOT,SVNNUM/))
C
C INITIALIZE SVNSIG AND FILSIG
C ----------------------------
      INDSVN=0.D0
      SVNSIG=0.D0
      INDSIG=0.D0
      FILSIG=0.D0
      DSACLK=0.D0
C
C OBSERVATION WINDOW
C ------------------
      obsWin%t(1)=1d20
      obsWin%t(2)=0d0
      DO IFIL=1,NFTOT
        IF(WINDOW(1,IFIL).LT.opt%epowin%t(1))
     1     WINDOW(1,IFIL)=opt%epowin%t(1)
        IF(WINDOW(2,IFIL).GT.opt%epowin%t(2))
     1     WINDOW(2,IFIL)=opt%epowin%t(2)
        IF (obsWin%t(1) > WINDOW(1,IFIL)) obsWin%t(1) = WINDOW(1,IFIL)
        IF (obsWin%t(2) < WINDOW(2,IFIL)) obsWin%t(2) = WINDOW(2,IFIL)
      ENDDO

C PRINT WINDOW
      CALL PRIWIN(1,opt%epowin%t)
C
C GET STATION COORDINATES AND ECCENTRICITY INFORMATION
C ----------------------------------------------------
      CALL GETSTA(NFTOT,STNAME(:,1),STANUM(:,1),NCENTR,ICENTR,
     1            XSTAT,XSTELL,XSTECC,
     2            DATUM,AELL,BELL,DXELL,DRELL,SCELL)
C
C APPLY POSITIONAL ECCENTRICITIES
C -------------------------------
      DO IFIL=1,NFTOT
        CALL ELLECC(XSTELL(1,IFIL),obsHead(ifil,1)%sta(1)%posEcc(:),
     1              EXCEN)
        XSTA1(1:3,IFIL)=XSTAT(1:3,IFIL)
        XSTAT(1:3,IFIL)=XSTAT(1:3,IFIL)+EXCEN(1:3)
      ENDDO
C
C INIT THE BUFFER FOR VIENNA GRID FILES
C -------------------------------------
      CALL initGridBuffer(xstell=XSTELL,TIMINT=obsWin)
      CALL prtGridInfo
C
C READ CLOCK RINEX INFORMATION
C ----------------------------
      InClkHead%pgmnam=' '
      InClkHead%nSta=0
      InClkHead%nSat=0
      CALL GTFLNA(0,'CLKRNX',FILRNX,IRCRNXI)
      IF (IRCRNXI.EQ.1.AND.opt%ifix.EQ.1) THEN
        WRITE(LFNERR,"(/,' *** PG CLKEST: Specify a CLKRNX file ',
     1                                   'to fix clocks')")
        CALL EXITRC(2)
      ENDIF
      IF (IRCRNXI.EQ.0) THEN
        CALL OPNFIL(LFNOR1,FILRNX,'OLD','FORMATTED','READONLY',' ',IRC)
        CALL OPNERR(LFNERR,LFNOR1,IRC,FILRNX,'CLKEST')
C
C READ HEADER
        CALL RDCRXH(LFNOR1,LFNERR,InClkHead,IRC)
        IF (IRC.NE.0) THEN
          WRITE(LFNERR,"(/,' *** PG CLKEST: Error reading clock ',
     1                     'rinex file header, irc=',I6)") IRC
          CALL EXITRC(2)
        ENDIF
        NCLK=0
C
C READ RECORDS
        IF (opt%ifix.EQ.1) THEN
          CALL RDCRXR(LFNOR1,LFNERR,opt%epowin%t,
     1                              InClkHead,InClkRec,IRC)
          IF (IRC.NE.0) THEN
            WRITE(LFNERR,"(/,' *** PG CLKEST: Error reading clock ',
     1                       'rinex file records, irc=',I6)") IRC
            CALL EXITRC(2)
          ENDIF
          NCLK=InClkRec%nEpo
C
C RINEX REFERENCE CLOCK
          DO ISTA=1,InClkHead%nSta+InClkHead%nSat
            IF(InClkHead%ClkName(ISTA).EQ.InClkHead%ref(1)%clk(1)%Name)
     1                                             IRFRNX=ISTA
          ENDDO
          InClkHead%ref(1)%clk(1)%Idx=IRFRNX
        ENDIF
        CLOSE(LFNOR1)
C PROTOCOL OUTPUT
        WRITE(LFNPRT,"(' CLOCK RINEX FILE',
     1               /,' ----------------',
     2              //,' Reference Clock       : ',A,
     3               /,' Number of Stations    :',I6,
     4               /,' Number of Satellites  :',I6,
     4               /,' Number of Clock Values:',I6,//)")
     5                 InClkHead%ref(1)%clk(1)%Name,
     6                 InClkHead%nSta,InClkHead%nSat,NCLK
      ENDIF
C
C CHECK WHETHER SELECTED REFERENCE CLOCK IS AVAILABLE
C ---------------------------------------------------
      IF(opt%clksel==4)opt%refClk=InClkHead%ref(1)%clk(1)%Name
      IRFOUT=0
C Station clock
      DO IFIL=1,NFTOT
        IF (STNAME(IFIL,1)(1:4).EQ.opt%refclk) IRFOUT=IFIL
      ENDDO
      IF (IRFOUT.NE.0) THEN
        staref=STNAME(IRFOUT,1)
      ELSE
C Satellite clock
        DO ISAT=1,SVNNUM
          CALL SVN2CHR(SVNLST(ISAT),SVNMOD,SVNCHR)
          WRITE(CLKNAM,"(A1,I2.2)") SVNCHR,SVNMOD
          IF (CLKNAM.EQ.opt%refclk) IRFOUT=ISAT
        ENDDO
        IF (IRFOUT.NE.0) staref=CLKNAM
      ENDIF
      IF (opt%clksel.EQ.1.OR.(opt%clksel==4.AND.opt%ifix==1)) THEN
        IF (IRFOUT.EQ.0) THEN
          WRITE(LFNERR,"(/,' *** PG CLKEST: Requested output ',
     1                     'reference clock not found in input files.',
     2                   /,'                Specified clock: ',A,/)")
     3                     opt%refclk
          CALL EXITRC(2)
        ENDIF
C INDEX IN CLK RINEX FILE FOR FIXING AND COPYING OF RINEX VALUES
      ELSEIF (opt%ifix==1) THEN
        IRFCRX=0
        DO ISTA=1,InClkHead%nSta+InClkHead%nSat
          IF (InClkHead%ClkName(ISTA).EQ.staref) IRFCRX=ISTA
        ENDDO
        IF (IRFCRX.EQ.0) THEN
          WRITE(LFNERR,"(/,' *** PG CLKEST: Requested output ',
     1                'reference clock not found in input rinex file',
     2                /,'                for options fix and copy',
     2                /,'                Specified clock: ',A,/)")
     3                TRIM(staref)
          CALL EXITRC(2)
        ENDIF
      ELSEIF (opt%clksel.LT.1.OR.opt%clksel.GT.4) THEN
        WRITE(LFNERR,"(/,' *** PG CLKEST: Reference clock selection ',
     1              'not allowed:',I6)")opt%clksel
        CALL EXITRC(2)
      ENDIF
C
C LOOP OVER ALL EPOCHS
C --------------------
c      TLASTC=-1.D10
c      TLASTP=-1.D10
      TLASTC=0D0
      TLASTP=0D0
      IZEROD=1
      INIPHA=1
      isasys=0
C
C WRITE FILE INFORMATION
C ----------------------
      WRITE(LFNPRT,5001)
5001  FORMAT(' FILE INFORMATION',
     1     /,' ----------------'
     2    //,1X,79('-'),
     3     /,' Station',15X,'Code Header File',17X,'Reference Epoch',
     4           '   # Sat',
     5     /,1X,79('-'))
      DO IFIL=1,NFTOT
        WRITE(LFNPRT,5002) STNAME(IFIL,1),FILNAM(1,IFIL),
     1                     obsHead(ifil,1)%timRef,
     1                     obsHead(ifil,1)%nsatel

5002    FORMAT(1X , A16,6X,A32,F14.6,I8)
      ENDDO
      WRITE(LFNPRT,5003)NFTOT
5003  FORMAT(1X,79('-'),/,' Number of Stations:',I6,/,1X,79('-'),//)
C
C DEFINE FREQUENCIES FOR SATELLITES
C ---------------------------------
      DO IFIL=1,NFTOT
        EPOFRQ(1)=WINDOW(1,IFIL)
        EPOFRQ(2)=WINDOW(2,IFIL)
C
C       Fill GOBSDEF structure
        norec = 0
        usegeos=0
        IF (obsHead(ifil,1)%iFrmat > 5) THEN
          DO iSat = 1,obsHead(ifil,1)%nsatel
            DO obs = 1,4
             IF (obsHead(ifil,1)%sat(iSat)%obstyp(obs).NE.'   ') THEN
                norec=norec+1
                EXIT
             ENDIF
            ENDDO
          ENDDO
        ENDIF
C       Initialize GOBSDEF and set frequencies
        IF ((obsHead(ifil,1)%iFrmat > 5).AND.(norec > 0)) THEN
          CALL init_geos(norec,gobsdef)
          CALL setgeos(obsHead(ifil,1),gobsdef,usegeos)
          CALL DEFREQ(EPOFRQ,obsHead(ifil,1)%nsatel,NUMSAT(1,IFIL,1),
     1                USEGEOS=USEGEOS,GOBSDEF=GOBSDEF,MEATYPC='C')
c
          IF (ASSOCIATED(GOBSDEF%SAT)) THEN
             DEALLOCATE(GOBSDEF%SAT,STAT=IRC)
          ENDIF
        ELSE
          CALL DEFREQ(EPOFRQ,obsHead(ifil,1)%nsatel,NUMSAT(1,IFIL,1))
        ENDIF
      ENDDO
C
C INITIALIZE ARRAY WITH PROBLEM TYPE
C ----------------------------------
      APRWGT=0.D0
      PROTYP=0
      PROSAT=0
C
C INITIALIZE NUMBER OF "OLD" CODE-CLOCKS
      NSCOLD=0
C INITIALIZE VARIABLES FOR CLOCK COMBINATION
      SVNCMB=0
      IEPOLD=1
      IEFOLD=1
C INITIALIZE NUMBER OF AVAILABLE DIFFERENCES PER CLOCK
      NDFSAT=0
      NDFSIT=0
C INITIALIZE NUMBER OF
      STAPIE=0
      SATPIE=0
C INITIALIZE COUNTERS FOR NUMBER OF CLOCK AND CLOCK DIFFERENCE SOLUTIONS
C FOR SATELLITES AND RECEIVERS
      IPAR =0
      IPARF=0
C
      WRITE(LFNPRT,5004)
5004  FORMAT(//,' EPOCH        DEL(C) DEL(P)',7X,
     1            'RMS(CODE)    RMS(PHASE)',/)
C
C *****************************************************************
      CALL stamp(lfnprt,'Start epoch processing')
C
C PROCESS EACH EPOCH
C ------------------
      NEPO=0
      REFCLK=0D0
      EpochLoop: DO IEP=1,NEPO1
C
C PROCESSING CODE OBSERVATIONS
C ****************************
        MEA=2
C
C GET ALL CODE OBSERVATIONS OF ONE EPOCH
        DO
          CALL GOBSEP(1,NFTOT,FILNUM,TLASTC,WINDOW,opt%dtsim ,
     1                obsHead(:,1)%timRef,obsHead(:,1)%iDeltt,
     2                obsHead(:,1)%iFrmat,obsHead(:,1)%meatyp,
     3                obsHead(:,1)%nsatel,NUMSAT(1,1,1),
     4                obsHead(:,1)%nfreq,ICARR ,0  ,SATSHD,
     4                TIMSHD,FLGSHD,TOBS  ,OBSNUM(1,1),
     5                NSATFL(1,1),SVNFIL(1,1,1),OBSFLG(1,1,1,1),
     6                OBSERV(1,1,1,1),SVNFI1(1,1),OBSFL1(1,1,1),
     7                OBSER1(1,1,1),DTFIL(1,1,1) ,FILACT(1,1),
     8                obsHead(:,1)%ndiff,-1,0d0,dummy1,dummy2,IRETC)
          IF (IRETC.EQ.1) EXIT EpochLoop
          IF(opt%nsampl == 0) THEN
            EXIT
          ELSE
            GPSSEC=(TOBS-DINT(TOBS))*86400D0
            TFRAC=GPSSEC-DNINT(GPSSEC/opt%nsampl)*opt%nsampl
            IF(DABS(TFRAC).LE.opt%dtsim*86400D0) EXIT
          ENDIF
        ENDDO
        NEPO=NEPO+1
C
C CLEAN THE MARKING FLAGS
        DO IFIL=1,NFTOT
          DO IFRQ=1,obsHead(ifil,1)%nfreq
            DO ISAT=1,NSATFL(IFIL,1)
              CALL CLRFLG(OBSFLG(ISAT,IFRQ,IFIL,1),0)
            ENDDO
          ENDDO
        ENDDO
C
C GET ALL ``GOOD'' OBSERVATIONS OF REQUIRED LC
        CALL GFRQEP(opt%lincom,NFTOT,FILACT,NSATFL(1,1),
     1              SVNFIL(1,1,1),OBSFLG(1,1,1,1),OBSERV(1,1,1,1),
     1              obsHead(:,1)%nfreq,ICARR,FILNUM,
     2              obsHead(:,1)%meatyp,MEA,IZEROD,MAXSAT,isasys,
     3              NSATEP(1),NFILEP(1),SVNEP(1,1),FILEP(1,1),
     4              NSFLEP(1,1),SAFLEP(1,1,1),OBFLEP(1,1,1))
C
C COMPUTE AND TRANSFORM SATELLITE POSITIONS
        DO ISAT=1,NSATEP(1)
          CALL GETORB(SVNEP(ISAT,1),1,2,0,TOBS,ICRARC,IORSYS,
     1                XVA(:,ISAT),TOSC,ELESAT,IRC,cmcyn)
          IF(IRC.EQ.2) THEN
            SVNEP(ISAT,1)=99
            CYCLE
          ENDIF
          CALL COOTRA(IORSYS,2,TOBS,XVA(:,ISAT),SZ,XPOL,YPOL,UT1GPS)
C
C Shadow condition
          CALL suneff(iorsys,0.5D0,tobs,sun,dum3)
          CALL shadow1(xva(:,isat),sun,ishad,rDummy)
          IF (ishad == 1) THEN
            iShd=0
            IF (IEP > 1) THEN
              DO ii = 1,nShd
                IF (satshd(ii) == svnep(isat,1) .AND.
     1              timshd(2,ii) == epoch(iep-1)) THEN
                  iShd = ii
                  timshd(2,ii) = tobs
                  EXIT
                ENDIF
              ENDDO
            ENDIF
            IF (iShd == 0) THEN
              nshd=nshd+1
              CALL dimtst(1,1,2,'CLKEST','SATSHD',
     1                   'Number of shadow transits',
     2                   ' ',nshd,maxshd,irc)
              satshd(nshd)=svnep(isat,1)
              CALL dimtst(1,1,2,'CLKEST','TIMSHD',
     1                   'Number of shadow transits',
     2                   ' ',nshd,maxshd,irc)
              timshd(1:2,nshd)=TOBS
              CALL dimtst(1,1,2,'CLKEST','FLGSHD',
     1                   'Number of shadow transits',
     2                   ' ',nshd,maxshd,irc)
              flgshd(nshd)=0
            ENDIF
          ENDIF
        ENDDO
C
C APPLY TIME DEPENDENT TERMS TO STATION POSITIONS
C -----------------------------------------------
        DO IFIL=1,NFTOT
          CALL XYZTIM(IORSYS,TOBS,XSTAT(1,IFIL),
     1                STNAME(IFIL,1),XSTA2(1,IFIL),cmcyn)
ccc       write(*,*)'no tides'
ccc       xsta2(1:3,ifil)=xstat(1:3,ifil)
        ENDDO
C
C LOOP OVER ALL FILES AND ALL FILES PER SATELLITE TO ELIMINATE
C ``GEOMETRY TERM''
C ------------------------------------------------------------
        IRFSAC=0
        ISATCO=0
C
C INITIALIZE STATION CLOCK ERROR
C ------------------------------
        DELTAT(:,1)=0D0
C
C PROCEED IN TWO ITERATION STEPS TO SYNCHRONIZE STATION CLOCKS
C ------------------------------------------------------------
        NITER=2
        IterationLoop: DO ITER=1,NITER
          NOBSCL=0
          NCLUS =0
          CodeFileLoop: DO IFIL=1,NFILEP(1)
            INDSTA=FILEP(IFIL,1)
C
C CHECK AVAILABILITY OF TROPOSPHERE PARAMETERS
C --------------------------------------------
            IF (opt%chktrp==1) THEN
              FILTRP=' '
              CALL GETTRP(FILTRP,TOBS,STNAME(INDSTA,1),0,0,
     1                    ITRPMD,ITRMAP,ITRGRD,DRTROP,IRC)
              IF (IRC.NE.0) THEN
                MRKOBS(1:NSFLEP(IFIL,1),IFIL)='T'
                IF (ITER==1) PROTYP(10,INDSTA)=PROTYP(10,INDSTA)+1
                CYCLE CodeFileLoop
              ENDIF
            ENDIF
            CodeSatLoop: DO ISAT=1,NSFLEP(IFIL,1)
              KSAT=LISTI4(0,MAXSAT,SVNEP(1,1),SAFLEP(ISAT,IFIL,1),
     1                                                  NSATEP(1))
              IF(KSAT.EQ.0)CYCLE CodeSatLoop
              INDSAT=LISTI4(0,MAXSAT,SVNLST,SAFLEP(ISAT,IFIL,1),SVNNUM)
C
C APPLY ANTENNA PHASE CENTER OFFSETS FOR STATIONS
              CALL STA_OFF(obsHead(indsta,1)%sta(1)%antTyp,
     1                     obsHead(indsta,1)%sta(1)%iAnten,
     2                     obsHead(indsta,1)%sta(1)%staNam,
     3                     SAFLEP(ISAT,IFIL,1),opt%lincom,
     4                     obshead(indsta,1)%csess(1),PHAECC)
              CALL ELLECC(XSTELL(1,INDSTA),PHAECC,EXCEN)
              XSTA3(1:3,INDSTA)=XSTA2(1:3,INDSTA)+EXCEN(1:3)
C
              DELTT=DELTAT(INDSTA,1)
              DELTT=DELTT+REFCLK
C
C APPLY ANTENNA PHASE CENTER OFFSETS FOR SATELLITES
              CALL SAT_OFF(SAFLEP(ISAT,IFIL,1),TOBS,typeMWTR,
     1                     ICARR(3,INDSTA),OFFSET)
              CALL GNSSATTI(IORSYS,SAFLEP(ISAT,IFIL,1),TOBS,1,
     1                      XVA(:,KSAT),EX_SAT=EX,EY_SAT=EY,EZ_SAT=EZ)
              XVAEP = XVA(:,KSAT)
              DO K = 1,3
                XVAEP(K)=XVAEP(K)+EX(K)*OFFSET(1)
     1                           +EY(K)*OFFSET(2)
     2                           +EZ(K)*OFFSET(3)
              ENDDO
C
              CALL PRANGE(1,TOBS,DELTT,opt%SECIPL,STNAME(INDSTA,1),
     1                    XSTA3(1,INDSTA),SAFLEP(ISAT,IFIL,1),
     2                    XVAEP,SZ,XPOL,YPOL,ISATCO,opt%itropo,
     3                    opt%iextra,XSTELL(1,INDSTA),
     4                    obsHead(indsta,1)%meatyp,ICARR(3,INDSTA),
     5                    IORSYS,TOPPOS,opt%maxzen,nadMax,DIST,SATC,ZEN,
     6                    AZI,ZENION,obsHead(indsta,1)%ndiff,-1,IRCCLK,
     7                    IRCPCV,NAD,AZISAT,AZISOK,
     8                    obsHead(indsta,1)%sta(1)%antTyp,
     8                    obsHead(indsta,1)%sta(1)%iAnten,
     9                    obshead(indsta,1)%csess(1),opt%irelJ2)
C DIFFERENTIAL CODE BIAS CORRECTION
              CALL DCBCOR(obsHead(indsta,1)%meatyp,ICARR(3,INDSTA),
     1                    SAFLEP(ISAT,IFIL,1),STNAME(INDSTA,1),
     2                    obsHead(indsta,1)%sta(1)%recTyp,
     3                    0,0,TOBS,DIST)
C GET A PRIORI SATELLITE CLOCKS
              IF (iter==1) THEN
                CALL GTSCLK(-1,TOBS,SAFLEP(ISAT,IFIL,1),opt%SECIPL,2,
     1                      DTSATC,SCKIRC(KSAT))
                satclk(ksat)=dtsatc
              ELSE
                dtsatc=satclk(ksat)
              ENDIF
C CORRECT DIST WITH SATELLITE AND STATION CLOCK OFFSETS
              DIST=DIST-C*DTSATC-C*DELTAT(INDSTA,1)+C*DTFIL(1,INDSTA,1)
C
C CORRECT PSEUDORANGE TO ELIMINATE GEOMETRY TERM
C ----------------------------------------------
              PRNGTL(ISAT,IFIL)=OBFLEP(ISAT,IFIL,1)-DIST
              IF(ITER.EQ.NITER)THEN
                PROTYP(7,INDSTA)=PROTYP(7,INDSTA)+1
                PROSAT(7,INDSAT)=PROSAT(7,INDSAT)+1
              ENDIF
              ZENDST(ISAT,IFIL)=ZEN
!!              IF(ZEN.LE.opt%maxzen)THEN
              IF(IRCPCV == 0)THEN
                IF((ITER.EQ.1.OR.MRKOBS(ISAT,IFIL).NE.'C').AND.
     1                           MRKOBS(ISAT,IFIL).NE.'T')THEN
                  MRKOBS(ISAT,IFIL)=' '
                ENDIF
              ELSE
                MRKOBS(ISAT,IFIL)='Z'
                IF(ITER.EQ.1)THEN
                  PROTYP(1,INDSTA)=PROTYP(1,INDSTA)+1
                  PROSAT(1,INDSAT)=PROSAT(1,INDSAT)+1
                ENDIF
              ENDIF
              IF(ITER.EQ.NITER.AND.
     1      DABS(PRNGTL(ISAT,IFIL))+0d0*C*DSACLK(KSAT).GT.opt%omcmxc)
     2                                                           THEN
                MRKOBS(ISAT,IFIL)='O'
                PROTYP(9,INDSTA)=PROTYP(9,INDSTA)+1
                PROSAT(9,INDSAT)=PROSAT(9,INDSAT)+1
              ENDIF
C
C LOOKING FOR DIFFERENT CLUSTERS
C ------------------------------
              IF(MRKOBS(ISAT,IFIL).EQ.' ') THEN
                ISATCL=0
                ISTACL=0
                DO KOBS=1,NOBSCL
                  IF(CLUST(1,KOBS).EQ.IFIL)ISTACL=CLUST(3,KOBS)
                  IF(CLUST(2,KOBS).EQ.SAFLEP(ISAT,IFIL,1))
     1                        ISATCL=CLUST(3,KOBS)
                ENDDO
                IF (ISATCL.EQ.0.AND.ISTACL.EQ.0) THEN
                  NCLUS=NCLUS+1
                  ICLUS=NCLUS
                ELSEIF(ISATCL.EQ.0.AND.ISTACL.NE.0) THEN
                  ICLUS=ISTACL
                ELSEIF(ISATCL.NE.0.AND.ISTACL.EQ.0) THEN
                  ICLUS=ISATCL
                ELSEIF(ISATCL.EQ.ISTACL) THEN
                  ICLUS=ISATCL
                ELSE
                  ICLUS=MIN(ISTACL,ISATCL)
                  ICLUS2=MAX(ISTACL,ISATCL)
                  NCLUS=NCLUS-1
                  DO LOBS=1,NOBSCL
                    IF(CLUST(3,LOBS).EQ.ICLUS2)CLUST(3,LOBS)=ICLUS
                    IF(CLUST(3,LOBS).GT.ICLUS2)
     1                          CLUST(3,LOBS)=CLUST(3,LOBS)-1
                  ENDDO
                ENDIF
                NOBSCL=NOBSCL+1
                IF(NOBSCL.GT.NFTOT*MAXSAT)THEN
                  WRITE(LFNERR,93)
93                FORMAT(//,' *** PG CLKEST: NOBSCL TOO LARGE ',//)
                  CALL EXITRC(2)
                ENDIF
                CLUST(1,NOBSCL)=IFIL
                CLUST(2,NOBSCL)=SAFLEP(ISAT,IFIL,1)
                CLUST(3,NOBSCL)=ICLUS
              ENDIF
            ENDDO CodeSatLoop
          ENDDO CodeFileLoop
          DO JCLUS=1,NCLUS
            IOBSCL(JCLUS)=0
            DO MOBS=1,NOBSCL
              IF(CLUST(3,MOBS).EQ.JCLUS)IOBSCL(JCLUS)=IOBSCL(JCLUS)+1
              IF(CLUST(1,MOBS).EQ.opt%irfclk)IRFCLT=CLUST(3,MOBS)
            ENDDO
            IF(NCLUS.GT.1)THEN
              WRITE(*,*)'IEP,CLU,IOBSCL= ',IEP,JCLUS,IOBSCL(JCLUS)
            ENDIF
          ENDDO
C
C SOLVE FOR SATELLITE AND RECEIVER CLOCK ERRORS
C ---------------------------------------------
          CALL ESTCLK(IEP,ITER,NITER,NFTOT,NSATEP(1),NFILEP(1),
     1                FILEP(1,1),SVNNUM,SVNLST,SVNEP(1,1),NSFLEP(1,1),
     2                SAFLEP(1,1,1),SCKIRC,opt%minsta,opt%rmsmxc,
     3                opt%resmxc,PRNGTL,MRKOBS,ZENDST,opt%omcmxc,
     4                opt%aprsic,opt%maxitc,IRFSAC,DSACLK,SITCLK,RMSCOD,
     5                RMSSAC,RMSSIT,RESIDU,NMARK,PROTYP,PROSAT,NOBSCS,
     6                INDC,RESFIL,INDS,RESSVN,IRETRN)
          IF(ITER.EQ.1) NMARC=NMARK
          IF(ITER.NE.1) NMARC=NMARC+NMARK
C
C REFERENCE CLOCK FOR RECEIVER SYNCHRONIZATION
C --------------------------------------------
          NOREF=1
C FILE NR
          IF (opt%irfsel.EQ.1) THEN
            KFIL=LISTI4(0,NFTOT,FILEP,opt%irfclk,NFILEP(1))
            IF (KFIL.NE.0) THEN
              IF (RMSSIT(KFIL).NE.0D0) THEN
                REFCLK=SITCLK(KFIL)-DELTAT(FILEP(KFIL,1),1)
                NOREF=0
              ENDIF
            ENDIF
C SATELLITE
          ELSEIF (opt%irfsel.EQ.2) THEN
            KSAT=LISTI4(0,MAXSAT,SVNEP,opt%irfclk,NSATEP(1))
            IF (KSAT.NE.0) THEN
              IF (SCKIRC(KSAT).NE.1.AND.SCKIRC(KSAT).NE.2.AND.
     1            RMSSAC(KSAT).NE.0D0) THEN
                REFCLK=DSACLK(KSAT)
                NOREF=0
              ENDIF
            ENDIF
C STATION FROM LIST
          ELSEIF (opt%irfsel.EQ.4) THEN
            StalstLoop: DO IREF=1,opt%nrfsta
              DO IFIL=1,NFTOT
                IF (STNAME(IFIL,1)(1:4) .EQ. opt%refsta(IREF)) THEN
                  KFIL=LISTI4(0,NFTOT,FILEP,IFIL,NFILEP(1))
                  IF (KFIL.NE.0) THEN
                    IF (RMSSIT(KFIL).NE.0D0) THEN
                      REFCLK=SITCLK(KFIL)-DELTAT(FILEP(KFIL,1),1)
                      NOREF=0
                      EXIT StalstLoop
                    ENDIF
                  ENDIF
                ENDIF
              ENDDO
            ENDDO StalstLoop
          ENDIF
C SUM CONDITION ON SATELLITES (DEFAULT)
          IF (opt%irfsel.EQ.3 .OR. NOREF.EQ.1) THEN
            NSUM=0
            SUM =0D0
            DO ISAT=1,NSATEP(1)
              IF(SVNEP(ISAT,1).EQ.99)CYCLE
              IF (SCKIRC(ISAT).NE.1.AND.SCKIRC(ISAT).NE.2.AND.
     1            RMSSAC(ISAT).NE.0D0) THEN
                NSUM=NSUM+1
                SUM =SUM+DSACLK(ISAT)
              ENDIF
            ENDDO
            IF(NSUM>0)THEN
              REFCLK=SUM/NSUM
            ELSE
       write(*,*)'no satellite clocks',iep,tobs
              REFCLK=0D0
            ENDIF
            NOREF=0
          ENDIF

C
C APPLY SATELLITE CLOCK CORRECTIONS BASED ON CODE
C -----------------------------------------------
          DO ISAT=1,NSATEP(1)
            SATCLK(ISAT)=SATCLK(ISAT)+DSACLK(ISAT)
          ENDDO
C
C APPLY RECEIVER CLOCK CORRECTIONS BASED ON CODE
C ----------------------------------------------
          DO IFIL=1,NFILEP(1)
            IF (RMSSIT(IFIL).GT.0D0) THEN
              INDFIL=FILEP(IFIL,1)
              DELTAT(INDFIL,1)=DELTAT(INDFIL,1)-SITCLK(IFIL)
            ENDIF
          ENDDO
        ENDDO IterationLoop
C
C PHASE CLOCK DIFFERENCE PROCESSING
C *********************************
        MEA=1
C GET ALL PHASE OBSERVATIONS OF ONE EPOCH
        DO
          CALL GOBSEP(2,NFTOT,FILNUM,TLASTP ,WINDOW,opt%dtsim ,
     1                obsHead(:,2)%timRef,obsHead(:,2)%iDeltt,
     2                obsHead(:,2)%iFrmat,obsHead(:,2)%meaTyp,
     3                NSATEL(1,2),NUMSAT(1,1,2),obsHead(:,2)%nFreq,
     4                ICARR ,0,SATSHD,TIMSHD,FLGSHD,
     4                TOBSP ,OBSNUM(1,2),NSATFL(1,2),
     3                SVNFIL(1,1,2),OBSFLG(1,1,1,2),OBSERV(1,1,1,2),
     3                SVNFI1(1,2),OBSFL1(1,1,2),OBSER1(1,1,2),
     4                DTFIL(1,1,2) ,FILACT(1,2),obsHead(:,2)%nDiff,
     5                -1,0d0,dummy1,dummy2,IRETC)
          IF (IRETC.EQ.1) EXIT EpochLoop
          IF(opt%nsampl == 0) THEN
            EXIT
          ELSE
            GPSSEC=(TOBSP-DINT(TOBSP))*86400D0
            TFRAC=GPSSEC-DNINT(GPSSEC/opt%nsampl)*opt%nsampl
            IF(DABS(TFRAC).LE.opt%dtsim*86400D0) EXIT
          ENDIF
        ENDDO
C
C CLEAN THE MARKING FLAGS
        DO IFIL=1,NFTOT
          DO IFRQ=1,obsHead(ifil,2)%nfreq
            DO ISAT=1,NSATFL(IFIL,2)
              CALL CLRFLG(OBSFLG(ISAT,IFRQ,IFIL,2),0)
            ENDDO
          ENDDO
        ENDDO
C
C GET ALL ``GOOD'' OBSERVATIONS OF REQUIRED LC
        CALL GFRQEP(opt%lincom,NFTOT,FILACT,NSATFL(1,2),
     1              SVNFIL(1,1,2),OBSFLG(1,1,1,2),OBSERV(1,1,1,2),
     1              obsHead(:,2)%nFreq,ICARR,FILNUM,
     2              obsHead(:,2)%meaTyp,MEA,IZEROD,
     3              MAXSAT,isasys,NSATEP(2),NFILEP(2),SVNEP(1,2),
     4              FILEP(1,2),NSFLEP(1,2),SAFLEP(1,1,2),
     5              OBFLEP(1,1,2))
C
C IF CODE CLOCK ESTIMATION WAS NOT OKAY, GO TO NEXT EPOCH,
C THIS TEST IS NECESSARY HERE BECAUSE IN THE PHASE OBSERVATION FILES,
C THE EPOCH HAVE TO BE CHOSEN
        IF(IRETRN.NE.0)THEN
          DO IREC=1,NFTOT
            DO INUM=1,NFILEP(1)
              STASIG(IREC,IEP)=0.D0
              STACLR(IREC,IEP)=UNDEF
            ENDDO
          ENDDO
          CYCLE EpochLoop
        ENDIF
C SAVE INDS AND RESSVN OF EACH SATELLITE
        DO ISAT=1,SVNNUM
          INDSVN(ISAT)=INDSVN(ISAT)+INDS(ISAT)*1.D0
          SVNSIG(ISAT)=SVNSIG(ISAT)+RESSVN(ISAT)
        ENDDO
C SAVE INDC AND RESFIL OF EACH FILE
        DO IFIL=1,NFTOT
          INDSIG(IFIL)=INDSIG(IFIL)+INDC(IFIL)*1.D0
          FILSIG(IFIL)=FILSIG(IFIL)+RESFIL(IFIL)
        ENDDO
C
C COMPUTE PHASE PSEUDORANGE
        PhaseFileLoop: DO IFIL=1,NFILEP(2)
          INDSTA=FILEP(IFIL,2)
C
C CORRESPONDING FILE OF CODE
          KFIL=LISTI4(0,NFTOT,FILEP(1,1),INDSTA,NFILEP(1))
C
C NO TROPO AVAILABLE
          IF (KFIL.NE. 0) THEN
            IF (opt%chktrp==1) THEN
              FILTRP=' '
              CALL GETTRP(FILTRP,TOBS,STNAME(INDSTA,2),0,0,
     1                    ITRPMD,ITRMAP,ITRGRD,DRTROP,IRC)
              IF (IRC.NE.0) THEN
                MRKPHA(1:NSFLEP(IFIL,2),IFIL)='T'
                CYCLE PhaseFileLoop
              ENDIF
            ENDIF
          ENDIF
C
C LOOP OVER ALL SATELLITES OF FILE
C --------------------------------
          PhaseSatLoop: DO ISAT=1,NSFLEP(IFIL,2)
            ZENPHA(ISAT,IFIL)=0.D0
            INDSAT=LISTI4(0,MAXSAT,SVNLST,SAFLEP(ISAT,IFIL,2),SVNNUM)
            KSAT=LISTI4(0,MAXSAT,SVNEP(1,2),SAFLEP(ISAT,IFIL,2),
     1                                                NSATEP(2))
C
C LOOK FOR SATELLITE IN ALL CODE OBSERVATIONS OF EPOCH
            LSAT=LISTI4(0,MAXSAT,SVNEP(1,1),SAFLEP(ISAT,IFIL,2),
     1                                                NSATEP(1))
            IF (LSAT.EQ.0) THEN
              MRKPHA(ISAT,IFIL)='M'
              PROTYP(5,INDSTA)=PROTYP(5,INDSTA)+1
              PROSAT(5,INDSAT)=PROSAT(5,INDSAT)+1
              CYCLE PhaseSatLoop
            ENDIF
C
C LOOK FOR SATELLITE IN CORRESPONDING CODE FILE
            IF (kFil /= 0) THEN
              JSAT=LISTI4(0,MAXSAT,SAFLEP(1,KFIL,1),SAFLEP(ISAT,IFIL,2),
     1                    NSFLEP(KFIL,1))
            ELSE
              JSAT = 0
            ENDIF
            IF (JSAT.EQ.0) THEN
              MRKPHA(ISAT,IFIL)='M'
              PROTYP(5,INDSTA)=PROTYP(5,INDSTA)+1
              PROSAT(5,INDSAT)=PROSAT(5,INDSAT)+1
              CYCLE PhaseSatLoop
            ENDIF
C
C APPLY ANTENNA PHASE CENTER OFFSETS FOR STATIONS
            CALL STA_OFF(obsHead(indsta,1)%sta(1)%antTyp,
     1                   obsHead(indsta,1)%sta(1)%iAnten,
     2                   obsHead(indsta,1)%sta(1)%staNam,
     3                   SAFLEP(ISAT,IFIL,2),opt%lincom,
     4                   obshead(indsta,1)%csess(1),PHAECC)
            CALL ELLECC(XSTELL(1,INDSTA),PHAECC,EXCEN)
            XSTA3(1:3,INDSTA)=XSTA2(1:3,INDSTA)+EXCEN(1:3)
C
C APPLY ANTENNA PHASE CENTER OFFSETS FOR SATELLITES
              CALL SAT_OFF(SAFLEP(ISAT,IFIL,2),TOBSP,typeMWTR,
     1                     ICARR(3,INDSTA),OFFSET)
              CALL GNSSATTI(IORSYS,SAFLEP(ISAT,IFIL,2),TOBSP,1,
     1                      XVA(:,LSAT),EX_SAT=EX,EY_SAT=EY,EZ_SAT=EZ)
              XVAEP = XVA(:,LSAT)
              DO K = 1,3
                XVAEP(K)=XVAEP(K)+EX(K)*OFFSET(1)
     1                           +EY(K)*OFFSET(2)
     2                           +EZ(K)*OFFSET(3)
              ENDDO
C
C SET INITIAL FLAGS FOR PHASE OBSERVATIONS OF EPOCH
            RMSSAP(ISAT)=0.D0
            MRKPHA(ISAT,IFIL)=MRKOBS(JSAT,KFIL)
            DELTT=DELTAT(INDSTA,1)
            DELTT=DELTT+REFCLK
            CALL PRANGE(1,TOBSP,DELTT,opt%SECIPL,STNAME(INDSTA,2),
     1                  XSTA3(1,INDSTA),SAFLEP(ISAT,IFIL,2),
     2                  XVAEP,SZ,XPOL,YPOL,ISATCO,opt%itropo,
     3                  opt%iextra,XSTELL(1,INDSTA),
     4                  obsHead(indsta,2)%meaTyp,ICARR(3,INDSTA),
     5                  IORSYS,TOPPOS,opt%maxzen,nadMax,DIST,SATC,ZEN,
     6                  AZI,zenion,obsHead(indsta,2)%nDiff,-1,IRCCLK,
     7                  IRCPCV,NAD,AZISAT,AZISOK,
     8                  obsHead(indsta,1)%sta(1)%antTyp,
     8                  obsHead(indsta,1)%sta(1)%iAnten,
     9                  obshead(indsta,1)%csess(1),opt%irelJ2)
            CALL GTSCLK(-1,TOBS,SAFLEP(ISAT,IFIL,2),opt%SECIPL,2,DTSATC,
     1                  IRC)
            dtsatc=satclk(LSAT)
C
C CORRECT DIST WITH CLOCK OFFSETS (ALSO RECEIVER CLOCK, BECAUSE THIS
C IS NOT ANY LONGER SUPPORTED IN SR PRANGE)
            DIST=DIST-C*DTSATC-C*DELTAT(INDSTA,1)+C*DTFIL(1,INDSTA,2)
C
C CORRECT PSEUDORANGE TO ELIMINATE GEOMETRY TERM
C ----------------------------------------------
            PRNGTL(ISAT,IFIL)=OBFLEP(ISAT,IFIL,2)-DIST                     +C*SATCLK(LSAT)
            PROTYP(8,INDSTA)=PROTYP(8,INDSTA)+1
            PROSAT(8,INDSAT)=PROSAT(8,INDSAT)+1
            ZENPHA(ISAT,IFIL)=ZEN
          ENDDO PhaseSatLoop
        ENDDO PhaseFileLoop
C
C COMPUTE CLOCK DIFFERENCE W.R.T. PREVIOUS EPOCH
C ----------------------------------------------
        CALL ESTCLD(IEP,INIPHA,NFTOT,NSATEP(2),NFILEP(2),FILEP(1,2),
     1              SVNNUM,SVNLST,SVNEP(1,2),NSFLEP(1,2),SAFLEP(1,1,2),
     2              opt%resmxp,opt%rmsmxp,opt%maxitp,opt%omcmax,
     3              opt%minsta,PRNGTL,MRKPHA,ZENPHA,opt%maxzen,
     4              opt%aprsip,IRFSAP,NSATPH,NFILPH,SVNPHA,FILPHA,
     5              NSFLPH,SAFLPH,NOBSPS,SATCLP,SITCLP,RMSPHA,RMSSAP,
     6              RMSSIP,RESIDU,NDEL,PROTYP,PROSAT,IRETRN)
C
C WRITE CLOCK-INFORMATION
C -----------------------
        ind=2
C
        ind=0
        DO isat=1,nsatep(1)
          IF (svnep(isat,1)==216) ind=isat
        enddo
!GIOVE        ind=0
!GIOVE        DO isat=1,nsatep(1)
!GIOVE          IF (svnep(isat,1)==216) ind=isat
!GIOVE        enddo
        WRITE(LFNPRT,190)TOBS,NMARC,NDEL,RMSCOD,RMSPHA,
     1           SVNEP(IND,1),SATCLK(ind)*1000.D0,deltat(1,1)*1000.D0,
     2           SITCLK(2)*1000.D0,SATCLP(ind)*1000.D0,SITCLP(2)*1000.D0
190     FORMAT(F14.6,I3,I7,2X,F13.3,F13.6,I3,5D15.6)
C
C SET CLOCK RMS FOR REFERENCE SATELLITE TO AVERAGE OF OTHER SATELLITES
C --------------------------------------------------------------------
        SUM=0D0
        DO ISAT=1,NSATEP(1)
          IF(SVNEP(ISAT,1).EQ.99)CYCLE
          IF (SVNEP(ISAT,1).EQ.IRFSAC) THEN
            KSAT=ISAT
          ELSE
            SUM=SUM+RMSSAC(ISAT)
          ENDIF
        ENDDO
        RMSSAC(KSAT)=SUM/(NSATEP(1)-1)
C
        SUM=0D0
        DO ISAT=1,NSATEP(2)
          IF(SVNEP(ISAT,1).EQ.99)CYCLE
          IF (SVNEP(ISAT,2).EQ.IRFSAP) THEN
            KSAT=ISAT
          ELSE
            SUM=SUM+RMSSAP(ISAT)
          ENDIF
        ENDDO
        RMSSAP(KSAT)=SUM/(NSATEP(2)-1)
C
C WRITE LIST OF PROBLEMS FOR CURRENT EPOCH
C ----------------------------------------
        NMESS=0
        DO IFIL=1,NFILEP(2)
          KFIL=LISTI4(0,NFTOT,FILPHA,FILEP(IFIL,2),NFILPH)
          DO ISAT=1,NSFLEP(IFIL,2)
            IF (KFIL.NE.0) THEN
              KSAT=LISTI4(0,MAXSAT,SAFLPH(1,KFIL),
     1                      SAFLEP(ISAT,IFIL,2),NSFLPH(KFIL))
              IF (KSAT.NE.0) THEN
                RESRES=RESIDU(KSAT,KFIL)
              ELSE
                RESRES=999.D0
              ENDIF
            ELSE
              RESRES=999.D0
            ENDIF
            IF(MRKPHA(ISAT,IFIL).NE.' '.AND.
     1         MRKPHA(ISAT,IFIL).NE.'Z'.AND.
     2         MRKPHA(ISAT,IFIL).NE.'X')THEN
              NMESS=NMESS+1
            ENDIF
          ENDDO
        ENDDO
C SAVE EPOCH
        EPOCH(IEP)=TOBS
C REFERENCE SATELLITE FOR EPOCH IEP AND INDEX OF THIS SATELLITE
cc        SVNRFC(IEP)=IRFSAC
cc        SVNRFP(IEP)=IRFSAP
C
C SAVE SATELLITE CLOCK CORRECTIONS
C --------------------------------
        SaveSatLoop: DO ISAT=1,NSATEP(1)
          IF(SVNEP(ISAT,1).EQ.99)CYCLE SaveSatLoop
C STORE CODE CLOCK CORRECTIONS FOR COMBINATION
C LOOKING FOR SATELLITE NUMBER IN LIST FOR COMBINATION
          IF(RMSSAC(ISAT).NE.0.D0)THEN
C ADD THE SATELLITE TO LIST FOR COMBINATION
            JSAT=LISTI4(1,MAXSAT,SVNC,SVNEP(ISAT,1),SVNCMB)
C INCREMENT NUMBER OF PARAMETERS FOR SATELLITE
            IPAR(JSAT)=IPAR(JSAT)+1
C SAVE CODE CLOCK VALUES
            CLK(IPAR(JSAT),JSAT)=SATCLK(ISAT)
            HLPIRC(JSAT)=SCKIRC(ISAT)
            WGTCLK(IPAR(JSAT),JSAT)=1/RMSSAC(ISAT)**2
            NUMCLK(IPAR(JSAT),JSAT)=NOBSCS(ISAT)
            MEPO(IPAR(JSAT),JSAT)=IEP
ccc        write(lfnerr,*)tobs,jsat,clk(ipar(jsat),jsat)
C SAVE CODE CLOCKS OF SATELLITE FOR NEXT EPOCH
            CLKOL2(ISAT)=CLK(IPAR(JSAT),JSAT)
C
C FIND PREVIOUS CLOCK VALUE
            IF(NSCOLD.EQ.0)THEN
              DCLK(IPAR(JSAT),JSAT)=0.D0
              WGTCLD(IPAR(JSAT),JSAT)=0.D0
              NUMCLD(IPAR(JSAT),JSAT)=0
            ELSE
C INITIALIZE CLOCK DIFFERENCES AND WEIGHTS FOR THE CLOCK COMBINATION
              DCLK(IPAR(JSAT),JSAT)=0.D0
              WGTCLD(IPAR(JSAT),JSAT)=0.D0
              NUMCLD(IPAR(JSAT),JSAT)=0
C
C FIND PREVIOUS CODE CLOCK VALUE
              KSAT=LISTI4(0,MAXSAT,SVCOLD,SVNEP(ISAT,1),NSCOLD)
              IF (KSAT.EQ.0) CYCLE SaveSatLoop
              DTCOD=CLK(IPAR(JSAT),JSAT)-CLKOLD(KSAT)
C
C FIND CORRESPONDING PHASE CLOCK
              LSAT=LISTI4(0,MAXSAT,SVNPHA,SVNEP(ISAT,1),NSATPH)
              IF(IEP-IEPOLD(JSAT).NE.1)THEN
                IEPOLD(JSAT)=IEP
                CYCLE SaveSatLoop
              ENDIF
              doIt = LSAT.EQ.0
              IF (LSAT.GT.0) THEN
                doIt = LSAT.GT.NSATEP(2).OR.RMSSAP(LSAT).EQ.0.D0.OR.
     1                 RMSSAC(ISAT).EQ.0.D0
              ENDIF
              IF(doIt)THEN
C STORE CODE DIFFERENCE FOR CLOCK COMBINATION
                if (lsat.eq.0) then
                  isatpie=isatpie+1
                else if(rmssap(lsat).eq.0.D0) then
                  isatpie=isatpie+1
                endif
                IF(IPAR(JSAT).NE.1)THEN
                  DCLK(IPAR(JSAT)-1,JSAT)=DTCOD
                  IF(RMSSAC(ISAT).NE.0.D0)THEN
                    WGTCLD(IPAR(JSAT)-1,JSAT)=1/(2*RMSSAC(ISAT))**2
                    NUMCLD(IPAR(JSAT)-1,JSAT)=-1
                  ENDIF
                  IF (opt%nocode==1)WGTCLD(IPAR(JSAT)-1,JSAT)=0.D0
                ENDIF
              ELSE
C STORE PHASE DIFFERENCE FOR CLOCK COMBINATION
                IEPOLD(JSAT)=IEP
                IF(IPAR(JSAT).NE.1)THEN
                  DCLK(IPAR(JSAT)-1,JSAT)=DTCOD+SATCLP(LSAT)
                  WGTCLD(IPAR(JSAT)-1,JSAT)=1/RMSSAP(LSAT)**2
                  NUMCLD(IPAR(JSAT)-1,JSAT)=NOBSPS(LSAT)
                  NDFSAT(JSAT)=NDFSAT(JSAT)+1
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDDO SaveSatLoop
C
C SAVE SATELLITE NUMBERS (CODE) AND CODE CLOCKS OF CURRENT EPOCH
        NSCOLD=NSATEP(1)
        TPREV=TOBS
        DO ISAT=1,NSCOLD
          IF(RMSSAC(ISAT).NE.0.D0)THEN
            CLKOLD(ISAT)=CLKOL2(ISAT)
            SVCOLD(ISAT)=SVNEP(ISAT,1)
          ELSE
            SVCOLD(ISAT)=0
          ENDIF
        ENDDO
C
C SAVE RECEIVER CLOCK CORRECTIONS
C -------------------------------
        SaveStaLoop: DO IREC=1,NFTOT
C FIND RECEIVER INDEX FOR EPOCH IEP
          INUM=LISTI4(0,NFTOT,FILEP(1,1),IREC,NFILEP(1))
          IF(INUM.NE.0) THEN
            IF(RMSSIT(INUM).NE.0.D0)THEN
C INCREMENT COUNTER
              IPARF(IREC)=IPARF(IREC)+1
C SAVE CODE RESULTS
              MEPOF(IPARF(IREC),IREC)=IEP
              STCLK(IPARF(IREC),IREC)=-DELTAT(FILEP(INUM,1),1)
              WGTSTA(IPARF(IREC),IREC)=1/RMSSIT(INUM)**2
ccc        write(lfnerr,*)tobs,irec+50,stclk(iparf(irec),irec)
C SAVE CODE CLOCKS OF FILE FOR NEXT EPOCH
              CLFOL2(INUM)=STCLK(IPARF(IREC),IREC)
C
C FIND PREVIOUS CLOCK VALUE
              IF(NFCOLD.EQ.0)THEN
                DSTCLK(IPARF(IREC),IREC)=0.D0
                WGTSTD(IPARF(IREC),IREC)=0.D0
              ELSE
C INITIALIZE CLOCK DIFFERENCES AND WEIGHTS FOR THE CLOCK COMBINATION
                DSTCLK(IPARF(IREC),IREC)=0.D0
                WGTSTD(IPARF(IREC),IREC)=0.D0
C
C FIND PREVIOUS CODE CLOCK VALUE
                KFIL=LISTI4(0,NFTOT,FILOLD,FILEP(INUM,1),NFCOLD)
                IF (KFIL.EQ.0) CYCLE SaveStaLoop
                DTFCOD=STCLK(IPARF(IREC),IREC)-CLFOLD(KFIL)
C
C FIND CORRESPONDING PHASE CLOCK
                LFIL=LISTI4(0,NFTOT,FILPHA,FILEP(INUM,1),NFILPH)
                IF(IEP-IEFOLD(IREC).NE.1)THEN
                  IEFOLD(IREC)=IEP
                  CYCLE SaveStaLoop
                ENDIF
                IF(LFIL.EQ.0) THEN
                  ISTAPIE=ISTAPIE+1
                  IF(IPARF(IREC).NE.1)THEN
                    DSTCLK(IPARF(IREC)-1,IREC)=DTFCOD
                    IF(RMSSIT(INUM).NE.0.D0)THEN
!                      WGTSTD(IPARF(IREC)-1,IREC)=1/(2*RMSSIT(INUM))**2
                      WGTSTD(IPARF(IREC)-1,IREC)=0D0
                    ENDIF
                  ENDIF
                ELSE IF(LFIL.GT.NFILEP(2).OR.
     1                 RMSSIP(LFIL).EQ.0.D0.OR.RMSSIT(INUM).EQ.0.D0)THEN
C STORE CODE DIFFERENCE FOR CLOCK COMBINATION
                  IF (lfil/=0) THEN
                    if(rmssip(lfil).eq.0.d0)istapie=istapie+1
                  ENDIF
                  IF(IPARF(IREC).NE.1)THEN
                    DSTCLK(IPARF(IREC)-1,IREC)=DTFCOD
                    IF(RMSSIT(INUM).NE.0.D0)THEN
!                      WGTSTD(IPARF(IREC)-1,IREC)=1/(2*RMSSIT(INUM))**2
                      WGTSTD(IPARF(IREC)-1,IREC)=0D0
                    ENDIF
                  ENDIF
                ELSE
C STORE PHASE DIFFERENCE FOR CLOCK COMBINATION
                  IEFOLD(IREC)=IEP
                  IF(IPARF(IREC).NE.1)THEN
                    DSTCLK(IPARF(IREC)-1,IREC)=DTFCOD+SITCLP(LFIL)
                    WGTSTD(IPARF(IREC)-1,IREC)=1/RMSSIP(LFIL)**2
                    NDFSIT(IREC)=NDFSIT(IREC)+1
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDDO SaveStaLoop
C
C SAVE FILE NUMBERS (CODE) AND CODE CLOCKS OF CURRENT EPOCH
        NFCOLD=NFILEP(1)
!       TPREV=TOBS
        DO IFFIL=1,NFCOLD
          IF(RMSSIT(IFFIL).NE.0.D0)THEN
            CLFOLD(IFFIL)=CLFOL2(IFFIL)
            FILOLD(IFFIL)=FILEP(IFFIL,1)
          ELSE
            FILOLD(IFFIL)=0
          ENDIF
        ENDDO
ccc      write(lfnerr,*)
C
C END OF EPOCH LOOP
      ENDDO EpochLoop
      WRITE(lfnprt,*)
      CALL stamp(lfnprt,'End epoch processing')
C
C CLOSE ALL CODE OBSERVATION FILES
C --------------------------------
      DO IFIL=1,NFTOT
        CLOSE(UNIT=LFN001-1+IFIL)
        CLOSE(UNIT=LFN002-1+IFIL)
      ENDDO
C
C NO EPOCH FOUND
C --------------
      IF (NEPO.EQ.0) THEN
        WRITE(LFNERR,910)
910     FORMAT(/,' *** PG CLKEST: No epoch found in window',/)
        CALL EXITRC(2)
      ENDIF
      WRITE(LFNPRT,"(/)")
C
C ****************************
C END OF PROCESSING EACH EPOCH
C ****************************
C
C TRANSFORM CLOCKS AND CLOCK DIFFERENCES TO COMMON REFERENCE
C ----------------------------------------------------------
      WRITE(LFNPRT,"(/,' MISSING CLOCK DIFFERENCES PER STATION',
     1                 '    Number of epochs:',I8,
     2               /,' -------------------------------------')")
     3                                                         nepo
      DO IFIL=1,NFTOT
        IF (nepo-ndfsit(ifil)-1>0)THEN
          WRITE(lfnprt,"(1X,I4,2X,A4,I8,2I6)")
     1           ifil,stname(ifil,1)(1:4),ndfsit(ifil)+1,
     1           nepo-ndfsit(ifil)-1
        ENDIF
      ENDDO
      WRITE(lfnprt,*)
C
C SEARCH CLOCK WITH NO MISSING DIFFERENCES
      CALL CESREF(3,2,opt%ifix,2,opt,InClkHead,InClkRec,nepo,epoch,
     1            svncmb,svnc,ipar,ndfsat,mepo,nftot,stname(:,1),
     2            iparf,ndfsit,mepof,irfsat,irfsit,irfrnx,islclk,
     3            clkref,svnrf,irc)
      IF (irc /= 0) THEN
        WRITE(LFNERR,"(/,' *** PG CLKEST: ',
     1                   'No internal reference clock found',/)")
        CALL EXITRC(2)
      ENDIF
C
C TRANSFORM ALL CLOCK AND CLOCK DIFFERENCES TO NEW REFERENCE
      CALL CETREF(IRFSAT,IRFSIT,NEPO,
     1            SVNCMB,IPAR ,MEPO ,CLK  ,WGTCLK,DCLK  ,WGTCLD,
     2            NFTOT ,IPARF,MEPOF,STCLK,WGTSTA,DSTCLK,WGTSTD)
C
C WRITE CKD-FILE
C --------------
      CALL cewckd(1,irfsat,irfsit,epoch,
     1            svncmb,svnc,ipar,mepo,clk,wgtclk,numclk,
     1            dclk ,wgtcld,numcld,
     2            nftot ,stname(:,1),iparf,mepof,stclk,wgtsta,dstclk,
     3            wgtstd)
C
C CLOCK COMBINATION FOR SATELLITES
C --------------------------------
      IF (opt%debug==1) THEN
        CALL gtflna(0,'CMBINFO',FILAUX, IRC)
        CALL OPNFIL(LFNPLT,FILAUX,'UNKNOWN','FORMATTED',' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNPLT,IOSTAT,FILAUX,'CLKEST')
      ENDIF
C
      call stamp(lfnprt,'Start clock combination')
      DO JSAT=1,SVNCMB
        IF (JSAT.EQ.IRFSAT) CYCLE
        CALL SVN2CHR(SVNC(JSAT),SVNMOD,SVNCHR)
        WRITE(CLKNAM,"(A1,I2.2)") SVNCHR,SVNMOD
C
C FIX ON CLKRNX CLOCKS
        IF (opt%ifix .EQ. 1) THEN
C GET RNX CLOCK INDEX
          IPOS=0
          DO ISAT=1,InClkHead%nSta+InClkHead%nSat
            IF (InClkHead%ClkName(ISAT)==CLKNAM) IPOS=ISAT
          ENDDO
        ENDIF
C
C COMBINE
        IF(opt%debug==1)WRITE(lfnplt,*)'satellite',jsat,'  ',clknam
        CALL CECOMB(1,opt,ipar(jsat),clk(:,jsat),wgtclk(:,jsat),
     1              dclk(:,jsat),wgtcld(:,jsat),epoch,mepo(:,jsat),
     2              InClkHead,InClkRec,ipos,irfrnx,irc)
      ENDDO
C
C CLOCK COMBINATION FOR STATIONS
C ------------------------------
      DO JFIL=1,NFTOT
        IF(IPARF(JFIL).EQ.0.OR.JFIL.EQ.IRFSIT)CYCLE
        IF(IPARF(JFIL).EQ.0)CYCLE
        CLKNAM=STNAME(JFIL,1)
C
C FIX ON CLKRNX CLOCKS
        IF (opt%ifix .EQ. 1) THEN
C GET RNX CLOCK INDEX
          IPOS=0
          DO ISTA=1,InClkHead%nSta+InClkHead%nSat
            IF (InClkHead%ClkName(ISTA)==CLKNAM) IPOS=ISTA
          ENDDO
        ENDIF
C
C COMBINE
        IF(opt%debug==1)write(lfnplt,*)'station',jfil,'  ',clknam(1:4)
        CALL CECOMB(2,opt,iparf(jfil),stclk(:,jfil),wgtsta(:,jfil),
     1              dstclk(:,jfil),wgtstd(:,jfil),epoch,mepof(:,jfil),
     2              InClkHead,InClkRec,ipos,irfrnx,irc)
      ENDDO
      IF(opt%debug==1)CLOSE (LFNPLT)
C
C REMOVE UNOBSERVED SATELLITES AND STATIONS
C -----------------------------------------
      ISAT=0
      DO JSAT=1,SVNCMB
        IF (IPAR(JSAT).GT.0) THEN
          ISAT=ISAT+1
          IF (ISAT.NE.JSAT) THEN
            IPAR(ISAT)=IPAR(JSAT)
            SVNC(ISAT)=SVNC(JSAT)
            HLPIRC(ISAT)=HLPIRC(JSAT)
            DO IP=1,IPAR(JSAT)
              CLK(IP,ISAT) =CLK(IP,JSAT)
              MEPO(IP,ISAT)=MEPO(IP,JSAT)
            ENDDO
          ENDIF
        ENDIF
      ENDDO
      SVNCMB=ISAT
C
      IFIL=0
      DO JFIL=1,NFTOT
        IF (IPARF(JFIL).GT.0) THEN
          IFIL=IFIL+1
          IF (IFIL.NE.JFIL) THEN
            IPARF(IFIL)=IPARF(JFIL)
            STNAME(IFIL,1)=STNAME(JFIL,1)
            XSTA1(1:3,IFIL)=XSTA1(1:3,JFIL)
            DO IP=1,IPARF(JFIL)
              STCLK(IP,IFIL)=STCLK(IP,JFIL)
              MEPOF(IP,IFIL)=MEPOF(IP,JFIL)
            ENDDO
            DO IP=IPARF(JFIL)+1,nepo1
              STCLK(IP,IFIL)=0.D0
              MEPOF(IP,IFIL)=0
            ENDDO
          ENDIF
        ENDIF
      ENDDO
      NFTOT=IFIL
      stclk(:,nftot+1:)=0.D0
      mepof(:,nftot+1:)=0
C
C TRANSFORM TO OTHER REFERENCE
C ----------------------------
C
C SEARCH REFERENCE CLOCK
      CALL CESREF(opt%clksel,3,opt%ifix,1,opt,InClkHead,InClkRec,
     1            nepo,epoch,svncmb,svnc,ipar,ndfsat,mepo,nftot,
     2            stname(:,1),iparf,ndfsit,mepof,irfsat,irfsit,
     3            irfrnx,islclk,staref,svnrf,irc)
      IF (irc /= 0) THEN
        WRITE(LFNERR,"(/,' *** PG CLKEST: ',
     1                   'No external reference clock found',/)")
        CALL EXITRC(2)
      ENDIF
C
! OPEN FILE FOR MISSING EPOCHS AND SUMMARY
      CALL GTFLNA(1,'EPOCHS ',FILCLM,IRC)
      CALL OPNFIL(LFNRES,FILCLM,'UNKNOWN','FORMATTED',' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNRES,IOSTAT,FILCLM,'CLKEST')
C
      WRITE(LFNRES,'(2(/,A),//,2(A,/),2(A,A,/),//,A,/)')
     1'HIGH-RATE CLOCK SOLUTION',
     2'------------------------',
     3' REFERENCE CLOCK',
     1' ---------------',
     2' Internal Reference for Combination :  ',TRIM(clkref),
     3' Reference for Output Clock Files   :  ',TRIM(staref),
     1'FOR THE FOLLOWING EPOCHS AND SATELLITES NO CLOCKS ARE AVAILABLE:'
      WRITE(LFNRES,'(A,/)')
     1   '    SAT     INTERVAL                                   NUMBER'
C
      IRFCLK=MAX(irfsat,irfsit)
      IF (IRFRNX>0) IRFCRX=IRFRNX
C
C TRANSFORM CLOCK TO NEW REFERENCE
      CALL CETREF(IRFSAT,IRFSIT,NEPO,
     1            SVNCMB,IPAR ,MEPO ,CLK  ,WGTCLK,DCLK  ,WGTCLD,
     2            NFTOT ,IPARF,MEPOF,STCLK,WGTSTA,DSTCLK,WGTSTD)
C
C GET ALLIGNMENT
C --------------
      CALL ceallign(opt,InClkHead,InClkRec,nepo,epoch,svncmb,ipar,
     1              mepo,clk,hlpirc,irfrnx,brdepo,brdoff,brdrft)
C
C ORDER SATELLITES
C ----------------
      CALL IORDUP(SVNC,SVNCMB,INDX)
C
C OUTPUT FILES
C ------------
      CALL GTFLNA(0,'CLKINF ',FILCLK,IRCRNX)
      CALL GTFLNA(0,'CLKRES ',FILCLR,IRCCLKO)
C
C MISSING EPOCHS OF CLOCKS
C ------------------------
C
      ALLOCATE(MISEPO(SVNCMB),STAT=IRC)
      CALL ALCERR(IRC,'MISEPO,',(/SVNCMB/),'CLKEST')
      MISEPO(1:SVNCMB)%T(1) = 0d0

      ALLOCATE(MCLK(SVNCMB),STAT=IRC)
      CALL ALCERR(IRC,'MCLK,',(/SVNCMB/),'CLKEST')
      MCLK(1:SVNCMB)=0
C
      NOCLK=0
      DO IT=1,NEPO
        DO LSAT=1,SVNCMB
          JSAT=INDX(LSAT)
          SATCLR(LSAT,IT)=UNDEF
          SVNORD(LSAT)=SVNC(JSAT)
          SATSIG(JSAT,IT)=0.D0
          DO IP=1,IPAR(JSAT)
            IF(MEPO(IP,JSAT).EQ.IT)THEN
!!              CALL GTSCLK(EPOCH(IT),SVNC(JSAT),opt%SECIPL,2,DTSATC,IRC)
!!              CLK(IP,JSAT)=CLK(IP,JSAT)+DTSATC
              SATCLR(LSAT,IT)=CLK(IP,JSAT)*1000000.D0
              EXIT
            ENDIF
          ENDDO
C LOOK FOR MISSING EPOCHS FOR A SATELLITE
C ---------------------------------------
          IF(SATCLR(LSAT,IT).GE.UNDEF)THEN
            NOCLK=NOCLK+1
            MCLK(LSAT)=MCLK(LSAT)+1
            IF (MISEPO(LSAT)%T(1).EQ.0D0) MISEPO(LSAT)%T(1)=EPOCH(IT)
            MISEPO(LSAT)%T(2)=EPOCH(IT)
C
          ELSE IF (MISEPO(LSAT)%T(1).NE.0D0) THEN
            CALL TIMST2(1,2,MISEPO(LSAT)%T,TSTRNG)
            WRITE(LFNRES,'(I7,5X,A40,2X,I7)')
     1                  SVNORD(LSAT),TSTRNG,MCLK(LSAT)
            MISEPO(LSAT)%T=0D0
            MCLK(LSAT)=0
          ENDIF
        ENDDO
C
C PEPARE STATION CLOCKS FOR RINEX
        IF (IRCRNX.EQ.0) THEN
          DO JFIL=1,NFTOT
            STACLR(JFIL,IT)=UNDEF
            STASIG(JFIL,IT)=0.D0
            DO IPF=1,IPARF(JFIL)
              IF(MEPOF(IPF,JFIL).EQ.IT)THEN
                STACLR(JFIL,IT)=STCLK(IPF,JFIL)*1000000.D0
              ENDIF
            ENDDO
          ENDDO
        ENDIF
      ENDDO
C
C REPORT THE REMAINING "MISSING" INTERVALS
      DO LSAT=1,SVNCMB
        IF (MISEPO(LSAT)%T(1).NE.0D0) THEN
          SVNORD(LSAT)=SVNC(INDX(LSAT))
          CALL TIMST2(1,2,MISEPO(LSAT)%T,TSTRNG)
          WRITE(LFNRES,'(I7,5X,A40,2X,I7)')
     1                SVNORD(LSAT),TSTRNG,MCLK(LSAT)
          MISEPO(LSAT)%T=0D0
          MCLK(LSAT)=0
        ENDIF
      ENDDO
      DEALLOCATE(MISEPO,STAT=IRC)
      DEALLOCATE(MCLK  ,STAT=IRC)
!      CLOSE(LFNRES)
      WRITE(LFNRES,"(//,' TOTAL NUMBER OF MISSING SATELLITE CLOCKS:',
     1               11X,I8,//)") NOCLK
      WRITE(LFNPRT,"(' NUMBER OF MISSING SATELLITE CLOCKS:',I6,//)")
     1                                                        NOCLK
C
C GET SATELLITE REFERENCE CLOCK IN ORDERED LIST
      JRFCLK=1
      IF (ISLCLK.EQ.1) THEN
        DO LSAT=1,SVNCMB
          IF (INDX(LSAT).EQ.IRFCLK) JRFCLK=LSAT
        ENDDO
      ENDIF
C REFERENCE EXISTING FOR EACH EPOCH?
      NOTALL=0
      DO IT=1,NEPO
        IF((ISLCLK.EQ.1.AND.SATCLR(JRFCLK,IT).EQ.UNDEF).OR.
     1     (ISLCLK.EQ.2.AND.STACLR(IRFCLK,IT).EQ.UNDEF))THEN
           NOTALL=NOTALL+1
C SET ALL CLOCKS TO UNDEFINED
          DO ICLK=1,NFTOT
            STACLR(ICLK,IT)=UNDEF
          ENDDO
          DO ICLK=1,SVNCMB
            SATCLR(ICLK,IT)=UNDEF
          ENDDO
        ENDIF
      ENDDO
      IF (NOTALL.NE.0) THEN
        WRITE(LFNPRT,"(' Number of missing clocks           :',I6)")
     1                                                         NOTALL
        WRITE(LFNERR,"(/,' ### PG CLKEST: Reference not available ',
     1                                     'for each epoch',
     1                        /,16X,'Epochs not written',
     2                        /,16X,'Reference: ',A,
     3                        /,16X,'Number of missing epochs:',I6)")
     4                        STAREF,NOTALL
      ENDIF
      WRITE(LFNPRT,*)
C
C DO NOT WRITE ALL STATION CLOCKS
C -------------------------------
      IF (opt%clkwrt.GT.1.OR.opt%cpyrnx.EQ.1) THEN
C
C FIND FILE INDICES IN RINEX FILE
        IF (opt%ifix.EQ.1) THEN
          IFILRX=0
          DO IFIL=1,NFTOT
            DO JSTA=1,InClkHead%nSta
              IF(InClkHead%ClkName(JSTA).EQ.STNAME(IFIL,1))THEN
                IFILRX(IFIL)=JSTA
                EXIT
              ENDIF
            ENDDO
          ENDDO
C
C FIND SATELLITE INDICES IN RINEX FILE
          ISATRX=0
          DO ISAT=1,SVNCMB
            CALL SVN2CHR(SVNC(ISAT),SVNMOD,SVNCHR)
            WRITE(CLKNAM,"(A1,I2.2)") SVNCHR,SVNMOD
            DO JSAT=InClkHead%nSta+1,InClkHead%nSta+InClkHead%nSat
              IF(InClkHead%ClkName(JSAT).EQ.CLKNAM)THEN
                ISATRX(ISAT)=JSAT
                EXIT
              ENDIF
            ENDDO
          ENDDO
        ENDIF
C
        ICLK=1
        DO IT=1,NEPO
C HANDLE CLOCK VALUES AT FIXING EPOCH
          IF (opt%clkwrt.EQ.2.OR.opt%cpyrnx.EQ.1) THEN
            IFOUND=0
            DO JCLK=ICLK,NCLK
              EPORNX=InClkHead%Tfirst+InClkRec%Epoch(JCLK)/86400D0
              IF (ABS(EPOCH(IT)-EPORNX).LT.1D-8) THEN
                IFOUND=1
                ICLK  =JCLK
                EXIT
              ELSEIF (EPORNX.GT.EPOCH(IT)) THEN
                EXIT
              ENDIF
            ENDDO
C WRITE CLOCKS ONLY AT FIXING EPOCHS
            IF (IFOUND.EQ.0.AND.opt%clkwrt.EQ.2)THEN
              DO JSTA=1,NFTOT
                IF (JSTA.NE.IRFCLK) STACLR(JSTA,IT)=UNDEF
              ENDDO
            ENDIF
          ENDIF
C COPY CLOCKS AT FIXING EPOCHS
          IF (IFOUND.EQ.1.AND.opt%cpyrnx.EQ.1.AND.
     1                        opt%ifix.EQ.1) THEN
            DO JSTA=1,NFTOT
              IF (IFILRX(JSTA).NE.0) THEN
                IF (InClkRec%Clock(IFILRX(JSTA),JCLK).NE.
     1                                             UNDEF.AND.
     2                      InClkRec%Clock(IRFCRX,JCLK).NE.UNDEF) THEN
                  IF (ISLCLK.EQ.2) THEN
                    STACLR(JSTA,IT)=InClkRec%Clock(IFILRX(JSTA),JCLK)-
     1                          InClkRec%Clock(IRFCRX,JCLK)
                  ELSE
                    STACLR(JSTA,IT)=InClkRec%Clock(IFILRX(JSTA),JCLK)
                  ENDIF
                  STASIG(JSTA,IT)=InClkRec%Sigma(IFILRX(JSTA),JCLK)
                ELSEIF(opt%delrnx==1)THEN
                  STACLR(JSTA,IT)=UNDEF
                  STASIG(JSTA,IT)=UNDEF
                ENDIF
              ENDIF
            ENDDO
            DO ISAT=1,SVNCMB
              JSAT=INDX(ISAT)
              IF (ISATRX(JSAT).NE.0) THEN
                IF (InClkRec%Clock(ISATRX(JSAT),JCLK).NE.
     1                                             UNDEF.AND.
     2              InClkRec%Clock(IRFCRX,JCLK).NE.UNDEF) THEN
                  IF (ISLCLK.EQ.2) THEN
                    SATCLR(ISAT,IT)=InClkRec%Clock(ISATRX(JSAT),JCLK)-
     1                          InClkRec%Clock(IRFCRX,JCLK)
                  ELSE
                    SATCLR(ISAT,IT)=InClkRec%Clock(ISATRX(JSAT),JCLK)
                  ENDIF
                  SATSIG(ISAT,IT)=InClkRec%Sigma(ISATRX(JSAT),JCLK)
                ELSEIF(opt%delrnx==1)THEN
                  SATCLR(ISAT,IT)=UNDEF
                  SATSIG(ISAT,IT)=UNDEF
                ENDIF
              ENDIF
            ENDDO
          ENDIF
C WRITE CLOCKS FROM LIST
          IF (opt%clkwrt.EQ.4) THEN
            DO JSTA=1,NFTOT
              WRTSTA=0
              IF (JSTA.EQ.IRFCLK) WRTSTA=1
              DO ILST=1,opt%nrxsta
                IF (STNAME(JSTA,1)(1:4).EQ.opt%rnxsta(ILST)) WRTSTA=1
              ENDDO
              IF (WRTSTA.EQ.0) STACLR(JSTA,IT)=UNDEF
            ENDDO
          ENDIF
C WRITE ONLY REFERENCE CLOCK
          IF (opt%clkwrt.EQ.3.OR.(opt%clkwrt.EQ.2.AND.
     1         opt%ifix.EQ.0)) THEN
cc            IF (IRFCLK.NE.1) STACLR(1,IT)=STACLR(IRFCLK,IT)
cc            NFTOT=1
            DO JSTA=1,NFTOT
              WRTSTA=0
              IF (JSTA.EQ.IRFCLK.AND.ISLCLK.EQ.2) WRTSTA=1
             IF (WRTSTA.EQ.0) STACLR(JSTA,IT)=UNDEF
            ENDDO
          ENDIF
        ENDDO
      ENDIF
C
C ADD LINEAR CLOCK AS REFERENCE
C -----------------------------
      IF (opt%allign.EQ.1.AND.islclk == 2)THEN
        DO IT=1,NEPO
          CLKLIN=BRDOFF+BRDRFT*(EPOCH(IT)-BRDEPO)
          DO JSAT=1,SVNCMB
            IF (SATCLR(JSAT,IT).NE.UNDEF)THEN
              SATCLR(JSAT,IT)=SATCLR(JSAT,IT)+CLKLIN*1D6
            ENDIF
          ENDDO
          DO JSTA=1,NFTOT
            IF (STACLR(JSTA,IT).NE.UNDEF)THEN
              STACLR(JSTA,IT)=STACLR(JSTA,IT)+CLKLIN*1D6
            ENDIF
          ENDDO
        ENDDO
      ENDIF
C
C DEALLOCATE CLKRINEX STRUCTURE
C -----------------------------
      IF (IRCRNXI.EQ.0) THEN
        DEALLOCATE (InClkHead%Comment,stat=irc)
        DEALLOCATE (InClkHead%DatTyp,stat=irc)
        DO jRef=1,InClkHead%numRef
          DEALLOCATE (InClkHead%Ref(jRef)%clk,stat=irc)
        ENDDO
        DEALLOCATE (InClkHead%Ref,stat=irc)
        DEALLOCATE (InClkHead%ClkName,stat=irc)
        DEALLOCATE (InClkHead%StaCoord,stat=irc)
        IF (opt%ifix==1) THEN
          DEALLOCATE(InClkRec%Epoch,stat=irc)
          DEALLOCATE(InClkRec%Clock,stat=irc)
          DEALLOCATE(InClkRec%Sigma,stat=irc)
          DEALLOCATE(InClkRec%clkFlg,stat=irc)
        ENDIF
      ENDIF
C
C WRITE RINEX CLOCK FILE
C ----------------------
      call stamp(lfnprt,'Start writing of output files')
      IF (IRCRNX.EQ.0) THEN
        CALL OPNFIL(LFNPLT,FILCLK,'UNKNOWN','FORMATTED',' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNPLT,IOSTAT,FILCLK,'CLKEST')
C
C WRITE RINEX CLOCK FILE HEADER
C -----------------------------
        NCLK=NFTOT+SVNCMB
        ClkHead%ProgNam  = 'CLKEST V'//PGMVER
        ClkHead%RunBy    = opt%runby
        ClkHead%CrDate   = DATE//' '//TIME
        ClkHead%nComment = opt%ncom
        ALLOCATE (ClkHead%Comment(opt%ncom),STAT=ios)
        CALL alcerr(ios,'ClkHead%Comment',(/opt%ncom/),'CLKEST')
        DO iCom = 1,opt%ncom
          ClkHead%Comment(iCom)  = opt%coment(iCom)
        ENDDO
        ClkHead%LeapSec=DGPSUT(epoch(1))
        IF (opt%clkwrt.EQ.3.AND.islClk.EQ.1) THEN
          iTyp = 1
        ELSE
          iTyp = 2
        ENDIF
        ClkHead%NumTyp   = iTyp
        ALLOCATE (ClkHead%DatTyp(iTyp),STAT=ios)
        CALL alcerr(ios,'ClkHead%DatTyp',(/iTyp/),'CLKEST')
        IF (opt%clkwrt.EQ.3.AND.islClk.EQ.1.AND.iTyp.EQ.1) THEN
          ClkHead%DatTyp   = (/'AS'/)
        ELSE
          ClkHead%DatTyp   = (/'AR','AS'/)
        ENDIF
        ClkHead%AC       = opt%ac
        ClkHead%ACName   = opt%acname
        ClkHead%timsys = opt%timsys
        IF (InClkHead%pgmnam == ' ') THEN
          ClkHead%pgmnam = 'CLKEST V'//PGMVER
          ClkHead%dcbStr = opt%dcbStr
          ClkHead%pcvstr = ''
          CALL gtsensor(pcvmod=ClkHead%pcvstr(1:10))
        ELSE
          ClkHead%pgmnam = InClkHead%pgmnam
          ClkHead%dcbStr = InClkHead%dcbstr
          ClkHead%pcvstr = InClkHead%pcvstr
        ENDIF
        ClkHead%numRef   = 1
        ALLOCATE (ClkHead%ref(1),STAT=ios)
        CALL alcerr(ios,'ClkHead%ref',(/1/),'CLKEST')
        CALL init_ref(ClkHead%ref(1))
        ALLOCATE (ClkHead%ref(1)%clk(1),STAT=ios)
        CALL alcerr(ios,'ClkHead%ref(1)%clk',(/1/),'CLKEST')
        ClkHead%ref(1)%nRef         = 1
        ClkHead%ref(1)%clk(1)%Name  = STAREF
        ClkHead%ref(1)%clk(1)%Sigma = 0.D0
        IF(epoch(1)/=0.D0) THEN
          ClkHead%TFirst = DNINT(EPOCH(1))
        ELSE
          ClkHead%TFirst = DNINT(EPOCH(2))
        END IF
        ClkHead%ref(1)%refWin%t = (/0D0,0D0/)
        ClkHead%TRFName         = DATUM
        IF (opt%clkwrt.EQ.3.AND.islClk.EQ.1) THEN
           ClkHead%nSta            = 0
           nClk = nClk - nfTot
           nfTot = 0
        ELSEIF (opt%clkwrt.EQ.3.AND.islClk.EQ.2) THEN
           ClkHead%nSta            = 1
           nClk = nClk - nfTot +1
        ELSE
           ClkHead%nSta            = NFTOT
        ENDIF
        ClkHead%nSat            = SVNCMB
        ALLOCATE (ClkHead%ClkName(NCLK),STAT=ios)
        CALL alcerr(ios,'ClkHead%ClkName',(/NCLK/),'CLKEST')
        IF (nfTot.NE.0) THEN
          ALLOCATE (ClkHead%StaCoord(3,NFTOT),STAT=ios)
          CALL alcerr(ios,'ClkHead%StaCoord',(/3,NFTOT/),'CLKEST')
        ENDIF
        DO IFIL=1,NFTOT
          IF (opt%clkwrt.EQ.3.AND.islClk.EQ.2) THEN
            IF (STNAME(IFIL,1).EQ.STAREF) THEN
              ClkHead%ClkName(1)      = STNAME(IFIL,1)
              ClkHead%StaCoord(1:3,1) = XSTA1(1:3,IFIL)
              iStaRef = iFil
              nfTot = 1
              EXIT
            ENDIF
          ELSE
            ClkHead%ClkName(IFIL)      = STNAME(IFIL,1)
            ClkHead%StaCoord(1:3,IFIL) = XSTA1(1:3,IFIL)
          ENDIF
        ENDDO
        DO ISAT=1,SVNCMB
          CALL svn2chr(svnord(ISAT),svnmod,svnchr)
          WRITE(ClkHead%ClkName(NFTOT+ISAT),"(A1,I2.2)") svnchr,svnmod
        ENDDO
C
C WRITE CLOCK RINEX HEADER
        CALL WTCRXH(LFNPLT,LFNERR,ClkHead,IRCCLK)
C
C WRITE RECORDS
        ClkRec%nEpo = NEPO
        ALLOCATE (ClkRec%Epoch(NEPO),STAT=ios)
        CALL alcerr(ios,'ClkRec%Epoch',(/NEPO/),'CLKEST')
        ALLOCATE (ClkRec%Clock(NCLK,NEPO),STAT=ios)
        CALL alcerr(ios,'ClkRec%Clock',(/NCLK,NEPO/),'CLKEST')
        ClkRec%Clock(:,:) = unDef
        ALLOCATE (ClkRec%Sigma(NCLK,NEPO),STAT=ios)
        CALL alcerr(ios,'ClkRec%Sigma',(/NCLK,NEPO/),'CLKEST')
        ClkRec%Sigma(:,:) = unDef
        ALLOCATE (ClkRec%clkFlg(NCLK,NEPO),STAT=ios)
        CALL alcerr(ios,'ClkRec%clkFlg',(/NCLK,NEPO/),'CLKEST')
        DO iEpo = 1,nEpo
          DO iClk = 1,nClk
            DO ii = 0,7
              CALL clrflg(clkRec%clkFlg(iClk,iEpo),ii)
            ENDDO
          ENDDO
        ENDDO
        DO IEPO=1,NEPO
          ClkRec%Epoch(IEPO) = (EPOCH(IEPO)-ClkHead%TFirst)*86400d0
          IF (opt%clkwrt.EQ.3.AND.islClk.EQ.2) THEN
            ClkRec%Clock(1,IEPO) = STACLR(iStaRef,IEPO)
            IF (staSig(iStaRef,iEpo) /= 0.D0) THEN
              ClkRec%Sigma(1,IEPO) = STASIG(iStaRef,IEPO)
            ENDIF
          ELSE
            DO ISTA=1,NFTOT
              ClkRec%Clock(ISTA,IEPO) = STACLR(ISTA,IEPO)
              IF (staSig(iSta,iEpo) /= 0.D0) THEN
                ClkRec%Sigma(ISTA,IEPO) = STASIG(ISTA,IEPO)
              ENDIF
            ENDDO
          ENDIF
          DO ISAT=1,SVNCMB
            ClkRec%Clock(NFTOT+ISAT,IEPO) = SATCLR(ISAT,IEPO)
            IF (satSig(iSat,iEpo) /= 0.D0) THEN
              ClkRec%Sigma(NFTOT+ISAT,IEPO) = SATSIG(ISAT,IEPO)
            ENDIF
            DO ISHD=1,NSHD
              IF (TIMSHD(1,ISHD) .LE. EPOCH(IEPO) .AND.
     1            TIMSHD(2,ISHD) .GE. EPOCH(IEPO) .AND.
     2            SATSHD(ISHD) .EQ. svnord(isat) ) THEN
                CALL setflg(ClkRec%clkflg(NFTOT+ISAT,IEPO),0)
              ENDIF
            ENDDO
          ENDDO
        ENDDO
        CALL WTCRXR(LFNPLT,LFNERR,ClkHead,ClkRec,IRCCLK)
        CLOSE(LFNPLT)
      ENDIF
C
C WRITE BERNESE CLOCK RESULTS
C ---------------------------
      IF (IRCCLKO.EQ.0) THEN
C
C WRITE HEADER FOR BERNESE CLOCK RESULT FILE
        TITCLK = ''
        TITCLK(1:64) = opt%titclk
        TITCLK(66:74)= DATE
        TITCLK(76:80)= TIME
        CALL WTSATH(FILCLR,LFNRP1,TITCLK)
        NSACLK=1
C
        DO IT=1,NEPO
          DO LSAT=1,SVNCMB
            IF (SATCLR(LSAT,IT).EQ.UNDEF) CYCLE
            SATCLR(LSAT,IT)=SATCLR(LSAT,IT)*1D-6
            CALL WTSATI(LFNRP1,SVNORD(LSAT),EPOCH(IT),NSACLK,
     1                                      SATCLR(LSAT,IT),IRC)
          ENDDO
        ENDDO
        CLOSE(LFNRP1)
      ENDIF

      IF (opt%chktrp==1) THEN
        WRITE(lfnprt,
     1        "(/,' Number of Observations with no Troposphere:',/)")
        notrp=0
        DO ifil=1,nftot
          IF (protyp(10,ifil)>0) THEN
            WRITE(lfnprt,"(1X,A,I10)") stname(ifil,1),protyp(10,ifil)
            notrp=notrp+1
          ENDIF
        ENDDO
        IF (notrp==0) WRITE(lfnprt,"('   none')")
      ENDIF
      WRITE(lfnprt,*)
C
C COMPUTE SIGMA(CODE) FOR EACH SATELLITE AND EACH FILE
C ----------------------------------------------------
      DO ISAT=1,SVNNUM
        IF(INDSVN(ISAT).NE.0)
     1              SVNSIG(ISAT)=DSQRT(SVNSIG(ISAT)/INDSVN(ISAT))
      ENDDO
      DO IFIL=1,NFTOT
        IF(INDSIG(IFIL).NE.0)
     1              FILSIG(IFIL)=DSQRT(FILSIG(IFIL)/INDSIG(IFIL))
      ENDDO
C
C PRINT PROBLEMS STATISTICS FOR SATELLITES
C ----------------------------------------
!      WRITE(LFNPRT,"(1X,79('-'),
      WRITE(LFNRES,"(/,1X,88('-'),
     1             /,' NSAT SAT       PROBLEM TYPE       ',
     1               '                               #OBSERVATIONS',
     2             /,'                Z      C      F      P',
     2           '      M      X      O       CODE    PHASE     SIGMA',
     3             /,1X,88('-'))")
      DO ISAT=1,SVNNUM
        WRITE(LFNRES,"(1X,I4,1X,I3,2X,I8,6I7,2I9,F10.4)")
     1        ISAT,SVNLST(ISAT),PROSAT(1:6,ISAT),
     2        PROSAT(9,ISAT),PROSAT(7:8,ISAT),SVNSIG(ISAT)
      ENDDO
      WRITE(LFNRES,"(1X,79('-'))")
      DO ITYP=1,9
        PROTOT(ITYP)=0
        DO K=1,SVNNUM
          PROTOT(ITYP)=PROTOT(ITYP)+PROSAT(ITYP,K)
        ENDDO
      ENDDO
      WRITE(LFNRES,"(' TOTAL     ',I8,6I7,2I9)")
     1               PROTOT(1:6),PROTOT(9),PROTOT(7:8)
      WRITE(LFNRES,"(1X,79('-'),//)")
C
C PRINT PROBLEMS STATISTICS FOR STATIONS
C --------------------------------------
      WRITE(LFNRES,"(1X,88('-'),
     1             /,' NFIL SITE      PROBLEM TYPE       ',
     1               '                               #OBSERVATIONS',
     2             /,'                Z      C      F      P',
     2         '      M      X      O       CODE    PHASE     SIGMA',
     3             /,1X,88('-'))")
      DO IFIL=1,NFTOT
        WRITE(LFNRES,"(1X,I4,1X,A4,1X,I8,6I7,2I9,F10.4)")
     1        IFIL,STNAME(IFIL,1)(1:4),PROTYP(1:6,IFIL),
     2        PROTYP(9,IFIL),PROTYP(7:8,IFIL),FILSIG(IFIL)
      ENDDO
      WRITE(LFNRES,"(1X,79('-'))")
      DO ITYP=1,9
        PROTOT(ITYP)=0
        DO K=1,NFTOT
          PROTOT(ITYP)=PROTOT(ITYP)+PROTYP(ITYP,K)
        ENDDO
      ENDDO
      WRITE(LFNRES,"(' TOTAL     ',I8,6I7,2I9)")
     1               PROTOT(1:6),PROTOT(9),PROTOT(7:8)
      WRITE(LFNRES,"(1X,79('-'))")
C
      write(lfnRES,'(1X,A,I5)')'Stations:  ',istapie
      write(lfnRES,'(1X,A,I5)')'Satellites:',isatpie
      WRITE(LFNRES,"(1X,79('-'),//)")
      write(lfnRes,'(1X,A8)')'Legend: '
      write(lfnRes,'(1X,A)')'Z: Marked due to zenith distance'
      write(lfnRes,'(1X,A)')'C: Refused due to code quality'
      write(lfnRes,'(1X,A)')'F: New ambiguity'
      write(lfnRes,'(1X,A)')'P: Phase marked due to O-C'
      write(lfnRes,'(1X,A)')'M: Phase, but no code observations'
      write(lfnRes,'(1X,A)')
     1           'X: Satellite not observed by enough stations or'
      write(lfnRes,'(1X,A)')
     1           '   receiver clock not observed by enough observations'
      write(lfnRes,'(1X,A)')'O: Marked due to O-C of code observation'

      CLOSE(lfnRes)
      call stamp(lfnprt,'End of program')

      CALL EXITRC(0)
      END

