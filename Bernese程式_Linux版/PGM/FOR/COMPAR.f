C*
      PROGRAM COMPAR
CC
CC NAME       :  COMPAR
CC
CC PURPOSE    :  THIS PROGRAM COMPARES THE BASELINE LENGTH, LATITUTES,
CC               LONGITUDES AND HEIGHTS OF DIFFERENT COORDINATE
CC               FILES (SOLUTIONS). IT GENERATES A PLOT FILE
CC
CC               SPECIAL VERSION: IT TAKES INTO ACCOUNT THE VARIANCE
CC               COVARIANCE MATRIX OF THE COORDINATES,
CC               IF THERE ARE SOME MISSING COVARIANCES A WEIGHT OF 1 CM
CC               IS INTRODUCED
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER, E.BROCKMANN
CC
CC CREATED    :  89/10/21 13:54
CC
CC CHANGES    :  01-MAR-93 : ??: SPECIAL VERSION WITH VARIANCE COVARIANCE
CC                               MATRIX
CC               13-NOV-93 : MR: "BASRPR" --> "BASRPP"
CC               25-JAN-94 : ??: FIXSTATION FLAG CRITERIUM CHANGED
CC               07-APR-94 : ??: BASELINES IN X,Y,Z
CC               10-AUG-94 : MR: CALL EXITRC
CC               22-SEP-94 : EB: CORRECT FIXING CHECK
CC               11-OCT-94 : EB: CORRECT WEIGHTED GROUP RMS
CC               12-OCT-94 : EB: OPTION FOR COVARIANCE SAVE
CC               06-FEB-95 : RW: CHANGE MAXFIL TO 740
CC               24-MAR-95 : MR: BASELINE DEFINITION FILE
CC               20-JUN-95 : TS: ADD WEEKLY SUMMARY FILE
CC               28-JUN-95 : MR: CUT STRING IF TOO LONG FOR "SSTR"
CC               31-AUG-95 : EB: NEW CALL COVSA2 (+FILCOV)
CC               08-SEP-95 : EB: NEW CALL WTSTA2 (+FILCOR)
CC               25-SEP-95 : CR: MORE MACHINE - READABLE OUTPUT
CC               30-SEP-95 : MR: ADD TITLE LINE TO NEW OUTPUT
CC               24-OCT-95 : EB: RESIDUALS IN MM INSTEAD OF M
CC               04-JAN-96 : EB: (MORE) CORRECT N-E-U COMPUTATION
CC               19-APR-96 : TS: SUMMARY LINE IN WEEKLY SUMMARY OUTPUT
CC               15-JUL-96 : EB: OPPOSITE SIGN OF BASELINE RESIDUALS !!!
CC               04-AUG-99 : PF: REPLACE WTSTA2 AND GETCO2 BY NEW SR'S
CC               16-AUG-99 : RD: DIMENSIONS (SMALL/MEDIUM/LARGE)
CC               15-JUN-00 : SS: WRITING OF 'FIXED' DEACTIVATED
CC               10-OCT-00 : CU: REPLACE COV(IND)=0.D0 -> COV(IND)=1.D0
CC               10-NOV-00 : CU: REMOVED LAST CHANGE: COV(IND)=1.D0 -->
CC                                                    COV(IND)=0.D0
CC               14-SEP-01 : RD: SWITCH TO NEW MENU
CC               14-SEP-01 : RD: IF "ALL FLAGS" NO COV INFO IS USED!
CC               04-OCT-01 : SS: 0.1-MM RESOLUTION FOR WEEKLY SUMMARY
CC               10-OCT-01 : SS: HEADER FOR WEEKLY SUMMARY MODIFIED
CC               25-SEP-02 : HU: REMOVE I_ASTLIB
CC               18-FEB-03 : HU: USE PREPROCESSOR COMMANDS
CC               11-MAR-03 : SS: WRITE "STRING(1:7)"
CC               23-APR-03 : HU: NULLIFY LOCAL POINTERS
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               24-NOV-03 : PS: 0.01-MM RESOLUTION
CC               23-FEB-04 : RD: TRIM FOR STRING IN SYSOUT
CC               04-MAY-04 : PS: NEW CALL FOR SR XYZLOC
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: LFNUM.inc, COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               01-JUN-06 : SS: WRITE MJD IN PLT FILE
CC               27-FEB-07 : AG: CALL DEFCON WITH PARAMETER
CC               19-JUL-10 : SL: TAB CHARACTERS REMOVED
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               19-JAN-11 : RD: ALLOCATE STATION/FILE ARRAYS
CC               07-FEB-11 : RD: SPECIFY AN EPOCH FOR COMPARISON
CC               17-FEB-11 : RD: COMMON MCMSTA NOT NEEDED ANYMORE
CC               24-FEB-11 : RD: USE AE FROM D_CONST
CC               24-NOV-11 : SL: NEW TITLE STRING FOR PRITIT, M_BERN WITH ONLY
CC               28-NOV-12 : RD: SIMPLIFY COMPUTATION OF VVXYZ2
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1993     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: i4b, r8b, shortLineLength, timStrgLength,
     1                    fileNameLength, staFlagLength, staNameLength,
     1                    lfnPrt, lfnErr, lfnLoc, lfn001, lfnPlt
      USE m_cpu,    ONLY: cpu_start
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE d_const,  ONLY: DATE, TIME, AE
C
      USE s_wtstat
      USE s_compri
      USE s_dimtst
      USE s_getcov
      USE s_wtskel
      USE s_syminvg
      USE s_opnfil
      USE s_alcerr
      USE s_prflna
      USE s_xyzloc
      USE s_exitrc
      USE s_defcon
      USE s_pritit
      USE s_covsa2
      USE s_opnsys
      USE s_gtflna
      USE s_opnerr
      USE s_readinpf
      USE s_xyzell
      USE s_crpmat
      USE s_getco3
      USE s_cordup
      USE s_cksizec1
      USE s_getdat
      USE s_gtvelo
      USE s_crdvel
      USE s_timst2
      USE f_ikf
      USE f_lengt0
      USE f_lengt1
      USE f_lincount
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I      , IB     , IBASDF , IBASE  , IBLK   , ICH1   ,
     1          ICH2   , ICHR   , ICO    , IFIL   , IFIL1  , IFIL2  ,
     2          IFLST1 , IFLST2 , IFLSTA , IFXALL , II     , IAC    ,
     3          IMAX   , IND    , INDI   , IOPBAS , IOPCOV , IOSTAT ,
     4          IPAR   , IRCAPR , IRCBSL , IRCPLT , IRCSKL ,
     5          IRCSUM , IRST   , ISING  , ISSTR  , ISTA   , ISTA1  ,
     6          ISTA2  , ISTAR2 , ISTAT  , ISTAT1 , ISTAT2 , ISTR   ,
     7          J      , K      , KPAR   , MAXBSL , JSTA   , MAXCOV ,
     8          MAXFLD , MAXSTA , MXCFIL , IRCVEL ,
     9          NBASDF , NBASE  , NBLK1  , NBLK2  , NBLMAX , NBLOCK ,
     1          NCHAR  , NCOL   , NDIM   , NFIL   , NFILPR , NFLAG  ,
     2          NOBS   , NOBS1  , NOBS2  , NOCOV  , NOPRT  , NPAOLD ,
     3          NPAR   , NREP   , NRST   , NSTAT  , NSTEST , NUNKNO ,
     4          NUNKNO1, NUNKNO2, NEWSTA
C
      REAL*8    AELL   , BASDIF , BASME1 , BASME2 , BASPPM , BASRM2 ,
     1          BASRMS , BASRP2 , BASRPP , BASSUM , BELL   , DIFMAX ,
     2          DIST2  , EPETOT , OBCOOB , OMEGA1 , OMEGA2 , OMEGAO ,
     3          R1     , RDIFF  , RMS    , RMS1   , RMS2   , SCELL  ,
     4          SIGMA2 , VHLP   , VVTOT  , XOLD   , REFEPO
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C MAXIMAL DIMENSIONS
C ------------------
      PARAMETER (MAXFLD=  1)
C
C MAXFIL: MAXIMUM NUMBER OF COORDINATE FILES
C MAXSTA: MAXIMUM NUMBER OF STATIONS IN COORDINATE FILE
C MAXLIN: MAXIMUM NUMBER OF LINES WITH COORD. DIFFERENCES PER STATION
C MAXFLD: MAXIMUM NUMBER OF FIELDS IN PLOT SKELETON
C MAXBSL: MAXIMUM NUMBER OF BASELINES IN BASELINE DEFINITION FILE
C
C DECLARATIONS
C ------------
      CHARACTER(LEN=  6),                PARAMETER         :: pgName =
     1                                                        'COMPAR'
      CHARACTER(LEN=132)                                   :: STRING
      CHARACTER(LEN=132),   ALLOCATABLE, DIMENSION(:,:)    :: STR
      CHARACTER(LEN=shortLineLength)                       :: TITLE
      CHARACTER(LEN=shortLineLength)                       :: TITSAV
      CHARACTER(LEN=shortLineLength),    DIMENSION(MAXFLD) :: FIELDS
      CHARACTER(LEN=shortLineLength),
     1                      ALLOCATABLE, DIMENSION(:)      :: SSTR
      CHARACTER(LEN=55)                                    :: TITGEN
      CHARACTER(LEN=timStrgLength)                         :: EPOSTR
      CHARACTER(LEN=fileNameLength)                        :: FILPLT
      CHARACTER(LEN=fileNameLength)                        :: FILSKL
      CHARACTER(LEN=fileNameLength)                        :: FILBSL
      CHARACTER(LEN=fileNameLength)                        :: FILSUM
      CHARACTER(LEN=fileNameLength)                        :: FILCOV
      CHARACTER(LEN=staNameLength),
     1                      ALLOCATABLE, DIMENSION(:)      :: STANAM
      CHARACTER(LEN=staNameLength),
     1                      POINTER,     DIMENSION(:)      :: STNAME
      CHARACTER(LEN=staNameLength),
     1                      POINTER,     DIMENSION(:)      :: STALST
      CHARACTER(LEN=staNameLength),
     1                      ALLOCATABLE, DIMENSION(:)      :: STAEST
      CHARACTER(LEN=staNameLength),
     1                      ALLOCATABLE, DIMENSION(:,:)    :: STABSL
      CHARACTER(LEN=16)                                    :: DATUM
      CHARACTER(LEN= 7),                 DIMENSION(2)      :: INTNAM
      CHARACTER(LEN= 6)                                    :: MXNFIL
      CHARACTER(LEN= 6),    ALLOCATABLE, DIMENSION(:)      :: YYDDDS
      CHARACTER(LEN= 4)                                    :: LCODE
      CHARACTER(LEN=staFlagLength),
     1                      ALLOCATABLE, DIMENSION(:,:)    :: STAFLG
      CHARACTER(LEN=staFlagLength),
     1                      POINTER    , DIMENSION(:)      :: HLPFLG
      CHARACTER(LEN=staFlagLength),
     1                      ALLOCATABLE, DIMENSION(:)      :: FLGXYZ
      CHARACTER(LEN=staFlagLength),
     1                      ALLOCATABLE, DIMENSION(:)      :: STAFLA
      CHARACTER(LEN=staFlagLength),
     1                      ALLOCATABLE, DIMENSION(:)      :: STFLAG
      CHARACTER(LEN=fileNameLength),
     1                      POINTER    , DIMENSION(:,:)    :: FILNAM
      CHARACTER(LEN=staFlagLength),
     1                      POINTER    , DIMENSION(:)      :: FLAGS
      CHARACTER(LEN=staFlagLength),
     1                      ALLOCATABLE, DIMENSION(:)      :: VELFLG
      CHARACTER(LEN= 1),    ALLOCATABLE, DIMENSION(:)      :: FOUND
      CHARACTER(LEN= 1),                 DIMENSION(3)      :: CTXT =
     1                                   (/'N','E','U'/)
C
      INTEGER(i4b),         ALLOCATABLE, DIMENSION(:)      :: NSTA
      INTEGER(i4b),         POINTER    , DIMENSION(:)      :: STNUMB
      INTEGER(i4b),         ALLOCATABLE, DIMENSION(:)      :: STANUM
      INTEGER(i4b),         ALLOCATABLE, DIMENSION(:)      :: NFLSTA
      INTEGER(i4b),         ALLOCATABLE, DIMENSION(:)      :: BASFIL
      INTEGER(i4b),                      DIMENSION(MAXFLD) :: IPLFLG
      INTEGER(i4b),         ALLOCATABLE, DIMENSION(:,:)    :: INDFIL
      INTEGER(i4b),         ALLOCATABLE, DIMENSION(:,:)    :: INDSTA
      INTEGER(i4b),         ALLOCATABLE, DIMENSION(:)      :: NOBSAP
      INTEGER(i4b),         ALLOCATABLE, DIMENSION(:)      :: NUNKAP
      INTEGER(i4b),         ALLOCATABLE, DIMENSION(:)      :: INDPRT
C
      REAL(r8b),            ALLOCATABLE, DIMENSION(:,:,:)  :: XSTAT
      REAL(r8b),            ALLOCATABLE, DIMENSION(:,:,:)  :: XSTELL
      REAL(r8b),            POINTER    , DIMENSION(:,:)    :: HLPXYZ
      REAL(r8b),                         DIMENSION(3)      :: DXELL
      REAL(r8b),                         DIMENSION(3)      :: DRELL
      REAL(r8b),            ALLOCATABLE, DIMENSION(:)      :: BASLEN
      REAL(r8b),            ALLOCATABLE, DIMENSION(:,:)    :: POSDIF
      REAL(r8b),                         DIMENSION(3)      :: POSME1
      REAL(r8b),                         DIMENSION(3)      :: POSSIG
      REAL(r8b),                         DIMENSION(3)      :: VELL
      REAL(r8b),                         DIMENSION(3)      :: VVELL
      REAL(r8b),                         DIMENSION(3)      :: RESHP1
      REAL(r8b),                         DIMENSION(3)      :: VXYZ
      REAL(r8b),            ALLOCATABLE, DIMENSION(:,:)    :: POSXYZ
      REAL(r8b),            ALLOCATABLE, DIMENSION(:,:)    :: VVXYZ
      REAL(r8b),            ALLOCATABLE, DIMENSION(:,:)    :: POSMEA
      REAL(r8b),                         DIMENSION(3)      :: REPEAT
      REAL(r8b),            ALLOCATABLE, DIMENSION(:)      :: COVHLP
      REAL(r8b),            ALLOCATABLE, DIMENSION(:,:)    :: VVXYZ2
      REAL(r8b),            ALLOCATABLE, DIMENSION(:)      :: COV
      REAL(r8b),            ALLOCATABLE, DIMENSION(:)      :: COVSUM
      REAL(r8b),            ALLOCATABLE, DIMENSION(:,:)    :: COVOBS
      REAL(r8b),            ALLOCATABLE, DIMENSION(:,:)    :: COVOBSI
      REAL(r8b),            ALLOCATABLE, DIMENSION(:,:)    :: OBS
      REAL(r8b),            ALLOCATABLE, DIMENSION(:,:)    :: XSTAT0
      REAL(r8b),            ALLOCATABLE, DIMENSION(:)      :: OMEG
      REAL(r8b),            ALLOCATABLE, DIMENSION(:)      :: OBCOOBI
      REAL(r8b),            ALLOCATABLE, DIMENSION(:)      :: COVADD
      REAL(r8b),            ALLOCATABLE, DIMENSION(:)      :: RHLP
      REAL(r8b),            ALLOCATABLE, DIMENSION(:)      :: SIGMA
      REAL(r8b),            ALLOCATABLE, DIMENSION(:)      :: TIMFIL
      REAL(r8b),            ALLOCATABLE, DIMENSION(:,:)    :: XVEL
      REAL(r8b),            ALLOCATABLE, DIMENSION(:,:)    :: POSLBH
      REAL(r8b),                         DIMENSION(3)      :: POSME2
      REAL(r8b),                         DIMENSION(3)      :: POSSI2
      REAL(r8b),                         DIMENSION(3,3)    :: HMAT
C
      LOGICAL,              ALLOCATABLE, DIMENSION(:)      :: VFOUND
C
C COMMON BLOCKS
C -------------
      COMMON/MCMFIL/MXCFIL,MXNFIL
      DATA DIFMAX/.0001D0/
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C NULLIFY POINTERS
C ----------------
      NULLIFY(HLPFLG)
      NULLIFY(STNUMB)
      NULLIFY(STALST)
      NULLIFY(STNAME)
      NULLIFY(FILNAM)
      NULLIFY(FLAGS)
      NULLIFY(HLPXYZ)
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
C WRITE THE TITLE
C ---------------
      CALL pritit('COMPAR','Coordinate comparison',131)
C
C PRINT THE LIST OF FILES
C -----------------------
      CALL prflna(131)
C
C GET INPUT OPTIONS
C -----------------
      CALL COMPRI(NFIL,FILNAM,NFLAG,FLAGS,TITGEN,IOPBAS,IOPCOV,REFEPO)
C
C COUNT THE NUMBER OF STATIONS
C ----------------------------
      maxcov = 0
      iFil   = 1
      CALL getco3(filnam(1,ifil),nflag,flags,maxSta,staLst)
      DO IFIL=2,NFIL
        CALL getco3(filnam(1,ifil),nflag,flags,nStat,stName)
        ALLOCATE(FOUND(nStat),stat=iac)
        CALL alcerr(iac,'FOUND',(/nStat/),pgName)
        newSta = 0
        DO iSta = 1,nStat
          found(iSta) = 'N'
          DO jSta = 1,MAXSTA
            IF (stName(iSta) == staLst(jSta)) THEN
              FOUND(iSta) = 'Y'
              EXIT
            ENDIF
          ENDDO
          IF (found(iSta)=='N') newSta = newSta+1
        ENDDO
        IF (newSta > 0) CALL CKSIZEC1(staLst,maxsta+newSta,newSta)
        DO iSta = 1,nStat
          IF (found(iSta)=='N') THEN
            maxSta=maxSta+1
            staLst(maxSta) = stName(iSta)
          ENDIF
        ENDDO
        DEALLOCATE(found,stat=iac)
        DEALLOCATE(stName,stat=iac)
        NULLIFY(stname)
        IF (LEN_TRIM(filnam(2,ifil))>0) THEN
          MAXCOV=MAX(MAXCOV,LINCOUNT(FILNAM(2,IFIL),17)/3)
        ENDIF
      ENDDO
      DEALLOCATE(staLst,stat=iac)
C
      MXCFIL=NFIL
      MXNFIL='MAXFIL'
C
C ALLOCATE THE STATION RELATED ARRAYS
C -----------------------------------
      ALLOCATE(SSTR(MAXSTA),STAT=IAC)
      CALL ALCERR(IAC,'SSTR',(/MAXSTA/),PGNAME)
      SSTR=''
C
      ALLOCATE(STANAM(MAXSTA),STAT=IAC)
      CALL ALCERR(IAC,'STANAM',(/MAXSTA/),PGNAME)
      STANAM=''
C
      ALLOCATE(STAEST(MAXCOV),STAT=IAC)
      CALL ALCERR(IAC,'STAEST',(/MAXCOV/),PGNAME)
      STANAM=''
C
      ALLOCATE(STAFLG(MAXSTA,NFIL),STAT=IAC)
      CALL ALCERR(IAC,'STAFLG',(/MAXSTA,NFIL/),PGNAME)
      STAFLG=''
C
      ALLOCATE(YYDDDS(NFIL),STAT=IAC)
      CALL ALCERR(IAC,'YYDDDS',(/NFIL/),PGNAME)
      YYDDDS=''
C
      ALLOCATE(FLGXYZ(MAXSTA),STAT=IAC)
      CALL ALCERR(IAC,'FLGXYZ',(/MAXSTA/),PGNAME)
      FLGXYZ=''
C
      ALLOCATE(STFLAG(MAXSTA),STAT=IAC)
      CALL ALCERR(IAC,'STFLAG',(/MAXSTA/),PGNAME)
      STAFLG=''
C
      ALLOCATE(STAFLA(MAXCOV),STAT=IAC)
      CALL ALCERR(IAC,'STAFLA',(/MAXCOV/),PGNAME)
      STAFLA=''
C
      ALLOCATE(NSTA(NFIL),STAT=IAC)
      CALL ALCERR(IAC,'NSTA',(/NFIL/),PGNAME)
      NSTA=0
C
      ALLOCATE(STANUM(MAXSTA),STAT=IAC)
      CALL ALCERR(IAC,'STANUM',(/MAXSTA/),PGNAME)
      STANUM=0
C
      ALLOCATE(NFLSTA(MAXSTA),STAT=IAC)
      CALL ALCERR(IAC,'NFLSTA',(/MAXSTA/),PGNAME)
      NFLSTA=0
C
      ALLOCATE(BASFIL(NFIL),STAT=IAC)
      CALL ALCERR(IAC,'BASFIL',(/NFIL/),PGNAME)
      BASFIL=0
C
      ALLOCATE(INDFIL(MAXSTA,NFIL),STAT=IAC)
      CALL ALCERR(IAC,'INDFIL',(/MAXSTA,NFIL/),PGNAME)
      INDFIL=0
C
      ALLOCATE(INDSTA(MAXSTA,NFIL),STAT=IAC)
      CALL ALCERR(IAC,'INDSTA',(/MAXSTA,NFIL/),PGNAME)
      INDSTA=0
C
      ALLOCATE(NOBSAP(NFIL),STAT=IAC)
      CALL ALCERR(IAC,'NOBSAP',(/NFIL/),PGNAME)
      NOBSAP=0
C
      ALLOCATE(NUNKAP(NFIL),STAT=IAC)
      CALL ALCERR(IAC,'NUNKAP',(/NFIL/),PGNAME)
      NUNKAP=0
C
      ALLOCATE(INDPRT(MAXSTA),STAT=IAC)
      CALL ALCERR(IAC,'INDPRT',(/MAXSTA/),PGNAME)
      INDPRT=0
C
      ALLOCATE(XSTAT(3,MAXSTA,NFIL),STAT=IAC)
      CALL ALCERR(IAC,'XSTAT',(/3,MAXSTA,NFIL/),PGNAME)
      XSTAT=0d0
C
      ALLOCATE(XSTELL(3,MAXSTA,NFIL),STAT=IAC)
      CALL ALCERR(IAC,'XSTELL',(/3,MAXSTA,NFIL/),PGNAME)
      XSTELL=0d0
C
      ALLOCATE(BASLEN(NFIL),STAT=IAC)
      CALL ALCERR(IAC,'BASLEN',(/NFIL/),PGNAME)
      BASLEN=0d0
C
      ALLOCATE(POSDIF(3,NFIL),STAT=IAC)
      CALL ALCERR(IAC,'POSDIF',(/3,NFIL/),PGNAME)
      POSDIF=0d0
C
      ALLOCATE(POSXYZ(3,MAXSTA),STAT=IAC)
      CALL ALCERR(IAC,'POSXYZ',(/3,MAXSTA/),PGNAME)
      POSXYZ=0d0
C
      ALLOCATE(VVXYZ(3,MAXSTA),STAT=IAC)
      CALL ALCERR(IAC,'VVXYZ',(/3,MAXSTA/),PGNAME)
      VVXYZ=0d0
C
      ALLOCATE(POSMEA(3,MAXSTA),STAT=IAC)
      CALL ALCERR(IAC,'POSMEA',(/3,MAXSTA/),PGNAME)
      POSMEA=0d0
C
      ALLOCATE(COVHLP(IKF(3*MAXCOV,3*MAXCOV)),STAT=IAC)
      CALL ALCERR(IAC,'COVHLP',(/IKF(3*MAXCOV,3*MAXCOV)/),PGNAME)
      COVHLP=0d0
C
      ALLOCATE(VVXYZ2(3,MAXSTA),STAT=IAC)
      CALL ALCERR(IAC,'VVXYZ2',(/3,MAXSTA/),PGNAME)
      VVXYZ2=0d0
C
      ALLOCATE(COV(IKF(3*MAXSTA,3*MAXSTA)),STAT=IAC)
      CALL ALCERR(IAC,'COV',(/IKF(3*MAXSTA,3*MAXSTA)/),PGNAME)
      COV=0d0
C
      ALLOCATE(COVSUM(IKF(3*MAXSTA,3*MAXSTA)),STAT=IAC)
      CALL ALCERR(IAC,'COVSUM',(/IKF(3*MAXSTA,3*MAXSTA)/),PGNAME)
      COVSUM=0d0
C
      ALLOCATE(COVOBS(NFIL,3*MAXSTA),STAT=IAC)
      CALL ALCERR(IAC,'COVOBS',(/NFIL,3*MAXSTA/),PGNAME)
      COVOBS=0d0
C
      ALLOCATE(COVOBSI(NFIL,3*MAXSTA),STAT=IAC)
      CALL ALCERR(IAC,'COVOBSI',(/NFIL,3*MAXSTA/),PGNAME)
      COVOBSI=0d0
C
      ALLOCATE(OBS(3*MAXSTA,NFIL),STAT=IAC)
      CALL ALCERR(IAC,'OBS',(/3*MAXSTA,NFIL/),PGNAME)
      OBS=0d0
C
      ALLOCATE(XSTAT0(3,MAXSTA),STAT=IAC)
      CALL ALCERR(IAC,'XSTAT0',(/3,MAXSTA/),PGNAME)
      XSTAT0=0d0
C
      ALLOCATE(OMEG(3*MAXSTA),STAT=IAC)
      CALL ALCERR(IAC,'OMEG',(/3*MAXSTA/),PGNAME)
      OMEG=0d0
C
      ALLOCATE(OBCOOBI(3*MAXSTA),STAT=IAC)
      CALL ALCERR(IAC,'OBCOOBI',(/3*MAXSTA/),PGNAME)
      OBCOOBI=0d0
C
      ALLOCATE(RHLP(3*MAXSTA),STAT=IAC)
      CALL ALCERR(IAC,'RHLP',(/3*MAXSTA/),PGNAME)
      RHLP=0d0
C
      ALLOCATE(COVADD(3*MAXSTA),STAT=IAC)
      CALL ALCERR(IAC,'COVADD',(/3*MAXSTA/),PGNAME)
      COVADD=0d0
C
      ALLOCATE(SIGMA(NFIL),STAT=IAC)
      CALL ALCERR(IAC,'SIGMA',(/NFIL/),PGNAME)
      SIGMA=0d0
C
      ALLOCATE(TIMFIL(NFIL),STAT=IAC)
      CALL ALCERR(IAC,'TIMFIL',(/NFIL/),PGNAME)
      TIMFIL=0d0
C
      ALLOCATE(POSLBH(3,MAXSTA),STAT=IAC)
      CALL ALCERR(IAC,'POSLBH',(/3,MAXSTA/),PGNAME)
      POSLBH=0d0
C
C PRINT GENERAL FILE NAMES
C ------------------------
      NOPRT=2
      INTNAM(1)='INPUT  '
      INTNAM(2)='COOFIL '
      NCOL=131
C
C      CALL PRFLNA(NOPRT,INTNAM,NCOL)
C
C OPEN PLOT-FILE
C --------------
      CALL GTFLNA(0,'PLOTRS ',FILPLT,IRCPLT)
      IF (IRCPLT.EQ.0) THEN
        CALL OPNFIL(LFNPLT,FILPLT,'UNKNOWN','FORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNPLT,IOSTAT,FILPLT,'COMPAR')
C
C WRITE HEADER FOR PLOT FILE (SKELETON KEY: A)
C --------------------------------------------
        CALL GTFLNA(0,'PLOTSKL',FILSKL,IRCSKL)
        IF (IRCSKL.EQ.0) THEN
          IPLFLG(1)=1
          WRITE(FIELDS(1),801) TITGEN
801       FORMAT(A55)
          CALL WTSKEL('A  ',FILSKL,FIELDS,IPLFLG,LFNPLT)
        ENDIF
      ENDIF
C
C TEST, IF THERE ARE ANY COVARIANCE INFORMATION AVAILABLE
C -------------------------------------------------------
      NOCOV=0
      DO 109 IFIL=1,NFIL
        IF (FILNAM(2,IFIL).EQ.' ') THEN
          NOCOV=NOCOV+1
        ENDIF
109   CONTINUE
C
C COMPUTE THE RMS OF GPS OBSERVABLES
      IRCAPR=0
      IF (NOCOV.EQ.NFIL) IRCAPR=1
C
C WRITE GENERAL TITLE AND FILE LIST TITLE
C ---------------------------------------
      WRITE(LFNPRT,11) NFIL
11    FORMAT(1X,131('-'),
     4       /,' LIST OF COORDINATE FILES',
     5       /,1X,131('-'),/,
     6       /,' NUMBER OF COORDINATE FILES:',I6,/,
     7       /,' FILE  FILE NAME',25X,'  #STA  TITLE',
     8       /,1X,131('-'),/)
C
C INITIALIZE TOTAL ARRAYS
C -----------------------
      NSTAT=0
      NPAOLD=0
C
C LOOP OVER ALL COORDINATE FILES
C ------------------------------
      DO 100 IFIL=1,NFIL
C
C READ ALL STATION COORDINATES WITH SPECIFIED FLAGS
        CALL GETCO3(FILNAM(1,IFIL),NFLAG,FLAGS,NSTA(IFIL),STNAME,
     1              STANUM=STNUMB,STAFLG=HLPFLG,XSTAT=HLPXYZ,
     2              DATUM=DATUM,TITLE=TITLE,TIMCRD=TIMFIL(IFIL))
C
        STAFLG(1:NSTA(IFIL),IFIL)=HLPFLG(1:NSTA(IFIL))
        DEALLOCATE(HLPFLG,STAT=IAC)
        NULLIFY(HLPFLG)
C
        XSTAT(1:3,1:NSTA(IFIL),IFIL)=HLPXYZ(1:3,1:NSTA(IFIL))
        DEALLOCATE(HLPXYZ,STAT=IAC)
        NULLIFY(HLPXYZ)
C
        CALL GETDAT(DATUM,AELL,BELL,DXELL,DRELL,SCELL)
        DO ISTA=1,NSTA(IFIL)
          CALL XYZELL(AELL,BELL,DXELL,DRELL,SCELL,
     1                XSTAT(1:3,ISTA,IFIL),XSTELL(1:3,ISTA,IFIL))
        ENDDO
C
C ADOPT THE EPOCH OF THE COORDINATES
C ----------------------------------
        IF (REFEPO.NE.0D0) THEN
          ALLOCATE(XVEL(3,NSTA(IFIL)),STAT=IAC)
          CALL ALCERR(IAC,'XVEL',(/3,NSTA(IFIL)/),'COMPAR')
          ALLOCATE(VELFLG(NSTA(IFIL)),STAT=IAC)
          CALL ALCERR(IAC,'VELFLG',(/NSTA(IFIL)/),'COMPAR')
          ALLOCATE(VFOUND(NSTA(IFIL)),STAT=IAC)
          CALL ALCERR(IAC,'VFOUND',(/NSTA(IFIL)/),'COMPAR')
C
          CALL GTVELO('VELOS   ',0,NFLAG,FLAGS,NSTA(IFIL),
     1                XSTAT(1:3,1:NSTA(IFIL),IFIL),STNAME,
     2                XVEL,VELFLG,VFOUND,IRCVEL)
C
          IF (IRCVEL.EQ.0) THEN
            CALL CRDVEL(NSTA(IFIL),XSTAT(1:3,1:NSTA(IFIL),IFIL),
     1                  XVEL,VFOUND,REFEPO,TIMFIL(IFIL))
            TIMFIL(IFIL) = REFEPO
C
          ENDIF
          DEALLOCATE(XVEL,STAT=IAC)
          DEALLOCATE(VELFLG,STAT=IAC)
          DEALLOCATE(VFOUND,STAT=IAC)
        ENDIF
C
C DETERMINE YEAR AND SESSION ASSUMING A FILENAME AS I.E. I_953350.CRD
C
        NCHAR=LENGT0(FILNAM(1,IFIL))
C
C STEP BACK AND FIND "." - IF NO "." FOUND YYDDDS(IFIL)='YYDDDS'
C
        DO ICHR=1,5
          ICH1=NCHAR-ICHR-6
          ICH2=NCHAR-ICHR
          IF (ICH1.LE.0) THEN
            YYDDDS(IFIL)='YYDDDS'
            GOTO 111
          ELSE
            IF(FILNAM(1,IFIL)(ICH2:ICH2).EQ.'.') THEN
              YYDDDS(IFIL)=FILNAM(1,IFIL)(ICH1:ICH2-1)
              GOTO 111
            ENDIF
          ENDIF
        ENDDO
        YYDDDS(IFIL)='YYDDDS'
111     CONTINUE
C
C WRITE NEXT LINE OF FILE LIST
        WRITE(LFNPRT,2) IFIL,FILNAM(1,IFIL),NSTA(IFIL),TITLE
2       FORMAT(I4,3X,A32,I6,4X,A80)
C
C LOOP OVER ALL STATIONS OF THE CURRENT FILE
        DO 50 ISTA=1,NSTA(IFIL)
C
C NEW STATION ?
          DO 10 ISTAT=1,NSTAT
            IF(STANAM(ISTAT).EQ.STNAME(ISTA)) THEN
              NPAOLD=NPAOLD+3
              GOTO 20
            ENDIF
10        CONTINUE
          NSTAT=NSTAT+1
          IF (NSTAT.GT.MAXSTA) THEN
            WRITE(LFNERR,902) NSTAT,MAXSTA
902          FORMAT(/,' *** PG COMPAR: TOO MANY STATIONS',/,
     1                           16X,'NUMBER OF STATIONS     :',I3,/,
     2                           16X,'MAX. NUMBER OF STATIONS:',I3,/)
            CALL EXITRC(2)
          ENDIF
          NFLSTA(NSTAT)=0
          STANAM(NSTAT)=STNAME(ISTA)
          STANUM(NSTAT)=STNUMB(ISTA)
          ISTAT=NSTAT
C
C APPROXIMATION OF COORDINATES (EQUAL FIRST STATION)
C --------------------------------------------------
          DO 25 K=1,3
            XSTAT0(K,NSTAT)=XSTAT(K,ISTA,IFIL)
25        CONTINUE
C
C SAVE STATION INDEX
20        NFLSTA(ISTAT)=NFLSTA(ISTAT)+1
          INDFIL(ISTAT,NFLSTA(ISTAT))=IFIL
          INDSTA(ISTAT,NFLSTA(ISTAT))=ISTA
C
C CHECK FIXSTATIONS
C -----------------
          DO 26 K=1,3
            RDIFF=DABS(XSTAT(K,ISTA,IFIL)-XSTAT0(K,ISTAT))
            IF (STAFLG(ISTA,IFIL).EQ.'F'.AND.RDIFF.GT.DIFMAX.AND.
     1          NOCOV.EQ.0)THEN
              WRITE(LFNERR,903) STANAM(ISTAT),K,RDIFF
903           FORMAT(/,' *** PG COMPAR: WARNING:',/,
     1          16X,'FIXSTATION FIXED ON DIFFERENT COORDINATES! ',/,
     2          16X,'STATION: ',A16,/,
     2          16X,'COORDINATE: ',I1,/,
     3          16X,'DIFFERENCE:  ',F10.5,/,
     4          16X,'RESULTS / RMS ARE STATISTICALLY NOT O.K.!!!!')
            ENDIF
26        CONTINUE
C
50      CONTINUE
C
        DEALLOCATE(STNAME,STAT=IAC)
        NULLIFY(STNAME)
        DEALLOCATE(STNUMB,STAT=IAC)
        NULLIFY(STNUMB)
100   CONTINUE
101   FORMAT(/,1X,131('-'))
C
C INITIALISE WEIGHT MATRIX
C ------------------------
      IMAX=3*NSTAT*(3*NSTAT+1)/2
      DO 30 I=1,IMAX
        COVSUM(I) = 0.D0
30    CONTINUE
      IMAX=3*NSTAT
      DO 31 I=1,IMAX
        OBCOOBI(I)=0.D0
        COVADD(I)=0.D0
        OMEG(I)=0.D0
31    CONTINUE
      OMEGA1=0.D0
      OMEGA2=0.D0
      OBCOOB =0.D0
      EPETOT=0.D0
C
C WRITE COVARIANCE FILE LIST TITLE
C --------------------------------
      WRITE(LFNPRT,12) NFIL-NOCOV
12    FORMAT(1X,131('-'),
     4       /,' LIST OF COVARIANCE FILES',
     5       /,1X,131('-'),/,
     6       /,' NUMBER OF COVARIANCE FILES:',I6,/,
     7       /,' FILE  FILE NAME',25X,'RMS       #STA  TITLE',
     8       /,1X,131('-'),/)
C
C LOOP OVER ALL VARIANCE COVARIANCE FILES
C ---------------------------------------
      DO 110 IFIL=1,NFIL
C
C CHECK COORDINATE EPOCHS
C -----------------------
        IF (TIMFIL(IFIL).NE.TIMFIL(1)) THEN
          WRITE(LFNERR,904)
904       FORMAT(/,' *** PG COMPAR: WARNING:',/,
     1         16X,'COORDINATE EPOCH DIFFERENT TO FIRST FILE! ',/)
        ENDIF
C
C SEARCH FOR FLAGS AND SETUP OBS MATRIX
C -------------------------------------
        DO 70 ISTAT=1,NSTAT
          STFLAG(ISTAT)=' '
          DO 71 K=1,3
            OBS(3*(ISTAT-1)+K,IFIL)=0.D0
71        CONTINUE
          DO 80 IFLSTA=1,NFLSTA(ISTAT)
            IF (IFIL.EQ.INDFIL(ISTAT,IFLSTA)) THEN
              ISTA=INDSTA(ISTAT,IFLSTA)
              STFLAG(ISTAT)=STAFLG(ISTA,IFIL)
              IF (STAFLG(ISTA,IFIL).NE.' ' .OR. FLAGS(1).EQ.'@') THEN
                DO 81 K=1,3
CC                  OBS(3*(ISTAT-1)+K,IFIL)=XSTAT(K,ISTA,IFIL)
                  OBS(3*(ISTAT-1)+K,IFIL)=
     1              XSTAT(K,ISTA,IFIL)-XSTAT0(K,ISTAT)
81              CONTINUE
                GOTO 70
              ENDIF
            ENDIF
80        CONTINUE
70      CONTINUE
C
C COMPUTE WEIGHT MATRIX
C ---------------------
C
C IF NO COVARIANCE AVAILABLE
C
        IF (FILNAM(2,IFIL).EQ.' ') THEN
          DO 75 IPAR=1,3*NSTAT
            ISTAT=(IPAR-MOD(IPAR-1,3))/3+1
            DO 76 KPAR=1,IPAR
              IND=IKF(IPAR,KPAR)
              IF (IPAR.EQ.KPAR) THEN
                IF (STFLAG(ISTAT).NE.' ' .OR. FLAGS(1).EQ.'@') THEN
C                 CASE A: NO OTHER STATIONS HAS COVARIANCE INFORMATION:
C                         UNIT WEIGHT
C                 CASE B: ONLY SOME MISSING COVARIANCE INFORMATION:
C                         WEIGHT=1CM
                  IF (NOCOV.EQ.NFIL)THEN
                    IF (STFLAG(ISTAT).EQ.'F') THEN
C                     FAKTOR THE SAME AS WITHOUT FIXING
                      COV(IND)=1.D0
                    ELSE
                      COV(IND)=1.D0
                    ENDIF
                  ELSE
                    IF (STFLAG(ISTAT).EQ.'F') THEN
C                     0.01 MM A-PRIORI ACCURACCY FOR FIXED STATIONS
CC                      COV(IND)=1/(0.00001D0**2)/1.0D12
                      COV(IND)=1/(0.00001D0**2)
                    ELSE
C                     1 CM A-PRIORI ACCURACCY FOR NONE FIXED STATIONS
CC                      COV(IND)=1/(0.01D0**2)/1.0D12
                      COV(IND)=1/(0.01D0**2)
                    ENDIF
                  ENDIF
                ELSE
                  COV(IND)=0.D0
                ENDIF
              ELSE
                COV(IND)=0.D0
              ENDIF
76          CONTINUE
75        CONTINUE
        ELSE
C
C READ ALL COVARIANCES OF THE (PROCESSED) STATION COORDINATES
C
          CALL GETCOV(FILNAM(2,IFIL),TITLE,NSTEST,STAEST,STAFLA,
     1                COVHLP,SIGMA(IFIL),NOBSAP(IFIL),NUNKAP(IFIL))
C
C IF YOU WOULD LIKE TO COMBINE -OLD- SOLUTIONS (WITHOUT NOBSAP AND
C NUNKAP): COMPUTE RMS IN A-POSTERIORI LEAST SQUARED ADJUSTMENT
C ----------------------------------------------------------------
          IF (NOBSAP(IFIL).EQ.0.OR.NUNKAP(IFIL).EQ.0) IRCAPR=1
C
C
C CREATE WEIGHT MATRIX "COV" FOR EACH COORDINATE SET AND
C SORT ACCORDING TO STANAM, IF FIXED STATION (FLAG=F) INTRODUCE
C AN APRIORI WEIGHT TO MAKE INVERSION POSSIBLE, OBS IS ALREADY
C INITIALISED
C
C IF CORRECT COMPUTATION OF THE RMS: A PRIORI COORDINATES ARE NECASSARY
C (NO A PRIORI COORDINATES GIVEN: ONLY A-POSTERIORI LEAST SQUARED
C SOLUTION)
C ---------------------------------------------------------------------
          CALL CRPMAT(COVHLP,SIGMA(IFIL),NSTEST,STAEST,STAFLA,NSTAT,
     1                STANAM,STFLAG,NOBSAP(IFIL),
     2                NUNKAP(IFIL),XSTAT(1,1,IFIL),COV)
        ENDIF
C
C UPDATE EPE (CORRECT RMS COMPUTATION)
C ------------------------------------
        IF (IRCAPR.EQ.0) THEN
          EPETOT=EPETOT+SIGMA(IFIL)**2*(NOBSAP(IFIL)-NUNKAP(IFIL))
        ENDIF
C
C UPDATE WEIGHT MATRIX COVSUM = SUM (COV)
C ---------------------------------------
        IMAX=3*NSTAT*(3*NSTAT+1)/2
        DO 90 I=1,IMAX
          COVSUM(I)=COVSUM(I)+COV(I)
90      CONTINUE
C
C UPDATE WEIGHT MATRIX COVOBS = SUM (COV * OBS)
C AND SAVE             COVOBSI=      COVI* OBSI (WEIGHTED OBSERVATION)
C                                               FOR GROUP RMS COMPUTATION
C --------------------------------------------------------------------
        DO 91 IPAR=1,3*NSTAT
          COVOBS(IFIL,IPAR)=0.D0
          INDI=IKF(IPAR,IPAR)
          COVOBSI(IFIL,IPAR)=COV(INDI)*OBS(IPAR,IFIL)
C       IF (IPAR.EQ.10)WRITE(*,*)'COVOBSI 10:',COV(INDI),OBS(IPAR,IFIL),
C     1     COVOBSI(IFIL,IPAR)
          DO 93 KPAR=1,3*NSTAT
            IND=IKF(IPAR,KPAR)
            COVOBS(IFIL,IPAR)=COVOBS(IFIL,IPAR)+
     1                        COV(IND)*OBS(KPAR,IFIL)
93        CONTINUE
91      CONTINUE
C                          T
C UPDATE OBCOOB = SUM (OBS   * COV * OBS)
C AND (FOR WEIGHTED GROUP RMS)
C                           T
C SAVE   OBCOOBI = SUM (OBSI  * COVI * OBSI)
C                  FOR GROUP RMS COMPUTATION
C AND
C UPDATE COVADD = SUM (COVI)
C -------------------------------------------
        DO 95 IPAR=1,3*NSTAT
          IND=IKF(IPAR,IPAR)
          OBCOOBI(IPAR)=OBCOOBI(IPAR)+COVOBSI(IFIL,IPAR)*OBS(IPAR,IFIL)
CC       IF (IPAR.EQ.10)WRITE(*,*)'OBCOOBI 10:',OBCOOBI(IPAR),
CC     1     OBS(IPAR,IFIL),COVOBSI(IFIL,IPAR)
          OBCOOB = OBCOOB + COVOBS(IFIL,IPAR)*OBS(IPAR,IFIL)
          COVADD(IPAR) = COVADD(IPAR) + COV(IND)
95      CONTINUE
C
C WRITE NEXT LINE OF FILE LIST
        IF (FILNAM(2,IFIL).NE.' ') THEN
          WRITE(LFNPRT,3) IFIL,FILNAM(2,IFIL),SIGMA(IFIL)*DSQRT(2.D0),
     1                    NSTEST,TITLE
3       FORMAT(I4,3X,A32,F8.5,I6,4X,A80)
        ENDIF
C
C END OF LOOP OVER ALL COVARIANCE FILES
110   CONTINUE
C
C
C WRITE STATION LIST TITLE
C ------------------------
      WRITE(LFNPRT,102) NSTAT
102   FORMAT('1',//,1X,131('-'),
     1        /,' LIST OF STATIONS',
     2        /,1X,131('-'),/,
     3        /,' TOTAL NUMBER OF STATIONS:',I6,/)
C
      NBLMAX=104
      NRST =MOD(NFIL,NBLMAX)
      IF (NRST.EQ.0) NRST=NBLMAX
      NBLOCK=(NFIL-NRST)/NBLMAX+1
C
      DO 205 IBLK=1,NBLOCK
        IF (IBLK.EQ.NBLOCK) THEN
          NFILPR=NRST
        ELSE
          NFILPR=NBLMAX
        ENDIF
        NBLK1=(IBLK-1)*NBLMAX+1
        NBLK2=NBLK1-1+NFILPR
C
        WRITE(LFNPRT,1021) (II/100       ,II=NBLK1,NBLK2)
        WRITE(LFNPRT,1021) (MOD(II/10,10),II=NBLK1,NBLK2)
1021    FORMAT(28X,104I1)
        WRITE(LFNPRT,1022) (MOD(II,10)   ,II=NBLK1,NBLK2)
1022    FORMAT(' NUM  STATION         #FIL  ',104I1)
        WRITE(LFNPRT,103)
103     FORMAT(1X,131('-'),/)
C
C LOOP OVER ALL STATIONS
C ----------------------
        DO 200 ISTAT=1,NSTAT
          STRING=' '
          DO 150 IFLSTA=1,NFLSTA(ISTAT)
            IFIL=INDFIL(ISTAT,IFLSTA)
            IF (IFIL.GE.NBLK1.AND.IFIL.LE.NBLK2) THEN
              ISTA=INDSTA(ISTAT,IFLSTA)
              ISTR=IFIL-NBLK1+1
              STRING(ISTR:ISTR)=STAFLG(ISTA,IFIL)
            ENDIF
150       CONTINUE
C
C WRITE NEXT LINE OF STATION LIST
          WRITE(LFNPRT,151) STANUM(ISTAT),STANAM(ISTAT),NFLSTA(ISTAT),
     1                      TRIM(STRING)
C
          WRITE(SSTR(ISTAT),152)STANAM(ISTAT),NFLSTA(ISTAT),STRING(1:7)
151       FORMAT(I4,2X,A16,I4,2X,A)
152       FORMAT(1X,A16,I4,2X,A)
C
200     CONTINUE
        WRITE(LFNPRT,101)
205   CONTINUE
C
C WRITE COORDINATE COMPARISON TITLE
C ---------------------------------
      NBLMAX=16
      NRST =MOD(NFIL,NBLMAX)
      IF (NRST.EQ.0) NRST=NBLMAX
      NBLOCK=(NFIL-NRST)/NBLMAX+1
C
      ALLOCATE(STR(3,NBLOCK),STAT=IAC)
      CALL alcerr(iac,'STR',(/3,NBLOCK/),PGNAME)
      STR = ''
C
      DO 210 IBLK=1,NBLOCK
        IF (IBLK.EQ.NBLOCK) THEN
          NFILPR=NRST
        ELSE
          NFILPR=NBLMAX
        ENDIF
        NBLK1=(IBLK-1)*NBLMAX+1
        NBLK2=NBLK1-1+NFILPR
C
        IF (IBLK.EQ.1) THEN
          CALL TIMST2(1,1,REFEPO,EPOSTR)
          IF (REFEPO == 0d0) EPOSTR = 'AS IN COORDINATE FILES'
          WRITE(LFNPRT,201) TRIM(EPOSTR),(II,II=NBLK1,NBLK2)
201       FORMAT('1',//,1X,131('-'),
     1        /,' COMPARISON OF COORDINATES (IN NORTH, EAST,',
     2          ' AND HEIGHT COMPONENT)',
     3        /,' EPOCH FOR COMPARISON: ',A,
     3        /,' RMS: UNWEIGHTED RMS OF THE ESTIMATION OF ',
     4          'ONE COORDINATE COMPONENT IN MM',
     5        /,1X,131('-'),/,
     6        /,' NUM  STATION         #FIL C   RMS',16I6)
        ELSE
          WRITE(LFNPRT,202) (II,II=NBLK1,NBLK2)
202       FORMAT(34X,16I6)
        ENDIF
C
210   CONTINUE
      WRITE(LFNPRT,103)
C
C COMPUTE THE WEIGHTED MEAN OF ALL COORDINATES (IN X,Y,Z)
C -------------------------------------------------------
C
C COMPUTE WEIGHTED MEAN COORDINATE SET INCLUDING TO WHOLE
C INFORMATION OF THE VARIANCE COVARIANCE MATRIX
C -------------------------------------------------------
C
      NDIM=NSTAT*3
      CALL SYMINVG(NDIM,COVSUM,0,ISING)
      IF(ISING.NE.0)THEN
        WRITE(LFNERR,1001)
1001    FORMAT(/,' *** PG COMPAR: SUM OF WEIGHT MATRIX SINGULAR',/)
        CALL EXITRC(2)
      END IF
C
C COMPUTE MEAN COORDINATES
C ------------------------
      DO 720 IPAR=1,3*NSTAT
        ISTAT=(IPAR-MOD(IPAR-1,3))/3+1
        ICO=MOD(IPAR-1,3)+1
        POSXYZ(ICO,ISTAT)=0.D0
        POSMEA(ICO,ISTAT)=0.D0
        DO 710 KPAR=1,3*NSTAT
          IND=IKF(IPAR,KPAR)
          DO 715 IFIL=1,NFIL
            IF (KPAR.EQ.1)POSMEA(ICO,ISTAT)=POSMEA(ICO,ISTAT)
     1                        +COVOBSI(IFIL,IPAR)
            POSXYZ(ICO,ISTAT)=POSXYZ(ICO,ISTAT)
     1                        +COVSUM(IND)*COVOBS(IFIL,KPAR)
715       CONTINUE
710     CONTINUE
        POSMEA(ICO,ISTAT)=POSMEA(ICO,ISTAT)/COVADD(IPAR)
720   CONTINUE
C                          T
C UPDATE OBCOOB = SUM (OBS   * COV * BETADACH)
C --------------------------------------------
CC      DO 98 IPAR=1,3*NSTAT
CC        OBCOOB = OBCOOB + COVOBS(IFIL,IPAR)*OBS(IPAR,IFIL)
CC98    CONTINUE
C
C COMPUTE SUM OF (WEIGHTED) SQUARED RESIDUALS
C FORMULA MAY BE NUMERICALLY PROBLEMATIC (LARGE WEIGHTS ON FREE
C NET SOLUTIONS)!!: LATER THE RESIDUAL FORMULA IS COMPUTED
C --------------------------------------------------------------
        DO 730 IPAR=1,3*NSTAT
          OMEG(IPAR)=0.D0
          ISTAT=(IPAR-MOD(IPAR-1,3))/3+1
          ICO=MOD(IPAR-1,3)+1
          DO 735 IFIL=1,NFIL
            OMEG(IPAR)=OMEG(IPAR)+COVOBSI(IFIL,IPAR)*POSMEA(ICO,ISTAT)
            OMEGA1=OMEGA1+COVOBS(IFIL,IPAR)*POSXYZ(ICO,ISTAT)
735       CONTINUE
C GROUP OMEGA
CC          WRITE(LFNERR,*)'OBCOOBI ',IPAR,OBCOOBI(IPAR),OMEG(IPAR)
          OMEG(IPAR)=OBCOOBI(IPAR)-OMEG(IPAR)
730     CONTINUE
C TOTAL OMEGA
CC          WRITE(*,*)'OBCOOB ',OBCOOB,OMEGA1
        OMEGA1=OBCOOB-OMEGA1
        OMEGAO=OMEGA1
CC        RR=0.D0
CC        DO 333 I=1,3*NSTAT
CC         RR=RR+OMEG(I)
CC333     CONTINUE
CC        WRITE(*,*)'OMEG:',RR
C                                              T   T
C COMPUTE RMS OF A POSTERIORI COMBINATION (B - B )  (X P X) (B - B )
C ---------------------------------------       I                 I
        OMEGA2=0.D0
        IF (IRCAPR.EQ.0)OMEGA1=0.D0
CC        WRITE(*,*)'IRCAPR',IRCAPR
        DO 180 IFIL=1,NFIL
          IF (IRCAPR.EQ.1) GOTO 180
          CALL GETCOV(FILNAM(2,IFIL),TITLE,NSTEST,STAEST,STAFLA,
     1                COVHLP,SIGMA(IFIL),NOBSAP(IFIL),NUNKAP(IFIL))
C
          CALL CRPMAT(COVHLP,SIGMA(IFIL),NSTEST,STAEST,STAFLA,NSTAT,
     1                STANAM,STFLAG,NOBSAP(IFIL),
     2                NUNKAP(IFIL),XSTAT(1,1,IFIL),COV)
          DO 198 IPAR=1,3*NSTAT
            RHLP(IPAR)=0.D0
            DO 199 KPAR=1,3*NSTAT
              IND=IKF(IPAR,KPAR)
              ISTAT=(KPAR-MOD(KPAR-1,3))/3+1
              ICO=MOD(KPAR-1,3)+1
              RHLP(IPAR)=RHLP(IPAR)+COV(IND)*(OBS(KPAR,IFIL)
     1                                   -POSXYZ(ICO,ISTAT))
199         CONTINUE
198       CONTINUE
          DO 170 IPAR=1,3*NSTAT
            ISTAT=(IPAR-MOD(IPAR-1,3))/3+1
            ICO=MOD(IPAR-1,3)+1
CC            WRITE(*,*)(RHLP(IPAR)*(OBS(IPAR,IFIL)-POSXYZ(ICO,ISTAT))),
CC     1      STANAM(ISTAT)
            OMEGA1=OMEGA1+RHLP(IPAR)*(OBS(IPAR,IFIL)-POSXYZ(ICO,ISTAT))
170       CONTINUE
180     CONTINUE
CC        WRITE(*,*)'OMEGA12 ',EPETOT,OMEGA1,OMEGAO
        OMEGA2=EPETOT+OMEGA1
C
C ADD APRIORI VALUE TO THE ESTIMATION
C -----------------------------------
      DO 760 IPAR=1,3*NSTAT
        ISTAT=(IPAR-MOD(IPAR-1,3))/3+1
        ICO=MOD(IPAR-1,3)+1
        POSXYZ(ICO,ISTAT)=XSTAT0(ICO,ISTAT)+POSXYZ(ICO,ISTAT)
760   CONTINUE
C
C SUM OF ALL OBSERVATIONS
C -----------------------
      NOBS1=0
      NUNKNO1=0
      NOBS2=0
      NUNKNO2=0
C
C COORDINATE COMBINATION RMS
C --------------------------
      DO 740 ISTAT=1,NSTAT
        NOBS1=NOBS1+NFLSTA(ISTAT)
740   CONTINUE
      NOBS1=3*NOBS1
      NUNKNO1=3*NSTAT
      IF (OMEGA1.GT.0.AND.(NOBS1-NUNKNO1).GT.0)THEN
        RMS1=DSQRT(OMEGA1/(NOBS1-NUNKNO1))
      ENDIF
C
C CORRECT ESTIMATED RMS OF GPSEST
C -------------------------------
      IF (IRCAPR.EQ.0) THEN
        DO 741 IFIL=1,NFIL
          NOBS2=NOBS2+NOBSAP(IFIL)
          NUNKNO2=NUNKNO2+NUNKAP(IFIL)
741     CONTINUE
        NUNKNO2=NUNKNO2-NPAOLD
        IF (OMEGA2.GT.0.AND.(NOBS2-NUNKNO2).GT.0)THEN
          RMS2=DSQRT(OMEGA2/(NOBS2-NUNKNO2))
        ENDIF
CC      WRITE(*,*)'NUM',NOBS1,NOBS2,NUNKNO1,NUNKNO2
      ENDIF
      IF (IRCAPR.EQ.0) THEN
        RMS=RMS2
        NOBS=NOBS2
        NUNKNO=NUNKNO2
      ELSE
        RMS=RMS1
        NOBS=NOBS1
        NUNKNO=NUNKNO1
      ENDIF
C
C LOOP OVER ALL STATIONS
C ----------------------
      CALL GETDAT(DATUM,AELL,BELL,DXELL,DRELL,SCELL)
      NREP = 0
      REPEAT(1)=0.D0
      REPEAT(2)=0.D0
      REPEAT(3)=0.D0
      DO 410 ISTAT=1,NSTAT
C
C TRANSFORM MEAN COORDINATES TO ELLIPTICAL COORDINATES
C ----------------------------------------------------
        CALL XYZELL(AELL,BELL,DXELL,DRELL,SCELL,POSXYZ(1,ISTAT),
     1              POSLBH(1,ISTAT))
C
        DO 470 K=1,3
          VVELL(K)=0.D0
          VVXYZ(K,ISTAT)=0.D0
          DO 465 IBLK=1,NBLOCK
            STR(K,IBLK)=' '
465       CONTINUE
470     CONTINUE
C
        WRITE(STR(1,1)(1:25),451) STANUM(ISTAT),STANAM(ISTAT),
     1                            NFLSTA(ISTAT)
451     FORMAT(I4,2X,A16,I3)
        DO 480 IFLSTA=1,NFLSTA(ISTAT)
          IFIL=INDFIL(ISTAT,IFLSTA)
          ISTA=INDSTA(ISTAT,IFLSTA)
          IRST =MOD(IFIL,NBLMAX)
          IF (IRST.EQ.0) IRST=NBLMAX
          IBLK=(IFIL-IRST)/NBLMAX+1
          ISTR=37+(IRST-1)*6
C
          DO I=1,3
            RESHP1(I)=XSTAT(I,ISTA,IFIL)-POSXYZ(I,ISTAT)
          ENDDO
          CALL XYZLOC(POSLBH(1,ISTAT),POSLBH(2,ISTAT),
     1       POSLBH(3,ISTAT),AELL,BELL,-1,RESHP1,VELL,HMAT)
C
          DO 490 K=1,3
            VXYZ(K)=XSTAT(K,ISTA,IFIL)-POSXYZ(K,ISTAT)
            VVXYZ(K,ISTAT)=VVXYZ(K,ISTAT)+VXYZ(K)**2
            VVELL(K)=VVELL(K)+VELL(K)**2
            VHLP=VELL(K)*1000.D0
            WRITE(STR(K,IBLK)(ISTR:ISTR+5),'(F6.2)',IOSTAT=IOSTAT)
     1                       VHLP
C
C WRITE PLOT FILE
            IF (IRCPLT.EQ.0) THEN
              WRITE(LFNPLT,802) STANAM(ISTAT),IFIL,K,VELL(K),
     1                          TIMFIL(IFIL)
802           FORMAT(A16,I5,I2,F10.5,F14.5)
            ENDIF
C
490       CONTINUE
480     CONTINUE
C
C GROUP RMS FOR EACH COORDINATE: CORRECT COMPUTATION IN XYZ, ONLY UNWEIGHTED
C                                RMS OF ONE COORDINATE ESTIMATION IN BLH
C --------------------------------------------------------------------------
        DO 520 K=1,3
          IF (NFLSTA(ISTAT).LT.2) THEN
            VVELL(K)=0.D0
            VVXYZ(K,ISTAT)=0.D0
          ELSE
C RMS OF ONE COORDINATE ESTIMATION (BLH) - ONLY UNWEIGHTED
            VVELL(K)=DSQRT(VVELL(K)/(NFLSTA(ISTAT)-1))
C
C RMS OF THE MEAN OF THE COORDINATE ESTIMATIONS (XYZ)
C VVXYZ: WEIGHTED RMS OF MEAN OF THE COORDINATE COMPONENT
C
CC            VVXYZ(K,ISTAT)=DSQRT(VVXYZ(K,ISTAT)/NFLSTA(ISTAT)/
CC     1                           (NFLSTA(ISTAT)-1))
C
              XOLD=VVXYZ(K,ISTAT)
              IND=IKF(3*(ISTAT-1)+K,3*(ISTAT-1)+K)
              IF (COVADD(3*(ISTAT-1)+K).EQ.0) THEN
                R1=0.D0
              ELSE
                R1=(OMEG(3*(ISTAT-1)+K)/
     1         (NFLSTA(ISTAT)-1)/COVADD(3*(ISTAT-1)+K))
              ENDIF
              IF (R1.GE.0) THEN
                VVXYZ(K,ISTAT)=DSQRT(R1)
              ELSE
                VVXYZ(K,ISTAT)=0.D0
              ENDIF
CC              WRITE(LFNERR,*)'VV-',STANAM(ISTAT),VVXYZ(K,ISTAT),
CC     1                  DSQRT(XOLD/NFLSTA(ISTAT)/
CC     1                           (NFLSTA(ISTAT)-1)),
CC     1                 COVADD(3*(ISTAT-1)+K),OMEG(3*(ISTAT-1)+K),R1
          ENDIF
520     CONTINUE
           DO 830 J=1,3
             VHLP=VVELL(J)*1000.D0
             WRITE(STR(J,1)(28:35),'(A1,F7.2)',IOSTAT=IOSTAT)
     1                                  CTXT(J),VHLP
             IF ((J.EQ.-3).AND.(IDNINT(VVELL(J)*1.D3).EQ.0)) THEN
               WRITE(SSTR(ISTAT)(31:36),'(A6)') '     -'
               WRITE(SSTR(ISTAT)(37:42),'(A6)') '     -'
               WRITE(SSTR(ISTAT)(43:48),'(A6)') '     -'
               IF (NFLSTA(ISTAT).GT.1) THEN
                 WRITE(SSTR(ISTAT)(52:56),'(A5)') 'FIXED'
               ENDIF
             ELSE
               ISSTR=33+(J-1)*6
               WRITE(SSTR(ISTAT)(ISSTR:ISSTR+5),'(F6.2)') VVELL(J)*1.D3
               IF (J.EQ.3) NREP=NREP+NFLSTA(ISTAT)
               REPEAT(J)=REPEAT(J)+(VVELL(J)*1D3)**2*(NFLSTA(ISTAT)-1)
             ENDIF
830        CONTINUE
C
        DO 540 IBLK=1,NBLOCK
          DO 530 K=1,3
            IF (STR(K,IBLK).NE.' ') THEN
              WRITE(LFNPRT,'(A)',IOSTAT=IOSTAT) TRIM(STR(K,IBLK))
            ENDIF
530       CONTINUE
          WRITE(LFNPRT,*)
540     CONTINUE
C
410   CONTINUE
      WRITE(LFNPRT,104)
104   FORMAT(1X,131('-'))
C
      DEALLOCATE(STR)
C
C WRITE SUMMARY OUTPUT FOR IGS WEEKLY PROTOCOL ON REQUEST
C -------------------------------------------------------
      CALL GTFLNA(0,'WKSSUM ',FILSUM,IRCSUM)
      IF (IRCSUM.EQ.0) THEN
        CALL OPNFIL(LFN001,FILSUM,'UNKNOWN','FORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFN001,IOSTAT,FILSUM,'COMPAR')
        WRITE(LFN001,105) TITGEN,NSTAT
105     FORMAT(/,1X,A55,/,1X,79('-'),/,
     1         ' Total number of stations:',I6,
     2         /,1X,79('-'))
        IF (NFIL.GT.7) THEN
          WRITE(LFNERR,908)NFIL,7
908       FORMAT(/,' ### PG COMPAR: MORE THEN 7 FILES FOR WRITING',
     1                      /,16X,'THE WEEKLY SUMMARY FILE',
     2                      /,16X,'USING ONLY THE FIRST 7 FLAGS',
     3                      /,16X,'NUMBER OF FILES     :',I3,
     4                      /,16X,'MAX. NUMBER OF FILES:',I3,/)
        ENDIF
        WRITE(LFN001,1023)
1023    FORMAT(23X,'Weekday  Repeatability (mm)',/,
     1         ' Station        #Days  0123456     N     E     U',
     2         /,1X,79('-'))
        CALL CORDUP(SSTR,NSTAT,2,5,INDPRT)
        DO 800 I=1,NSTAT
          WRITE(LFN001,'(A)',IOSTAT=IOSTAT)
     1    SSTR(INDPRT(I))(1:LENGT1(SSTR(INDPRT(I))))
800     CONTINUE
        WRITE(LFN001,106)
106     FORMAT(1X,79('-'))
        DO I=1,3
          REPEAT(I)=DSQRT(REPEAT(I)/(NREP-1))
        ENDDO
        WRITE(LFN001,107)NREP,(REPEAT(I),I=1,3)
107     FORMAT(' # Coordinate estimates:',I6,2X,3F6.2)
        CLOSE(UNIT=LFN001)
      ENDIF
C
C COMPUTE THE RIGHT RMS OF THE MEAN
C ---------------------------------
      DO 750 IPAR=1,3*NSTAT
        ISTAT=(IPAR-MOD(IPAR-1,3))/3+1
        ICO=MOD(IPAR-1,3)+1
        IND=IKF(IPAR,IPAR)
        IF (COVSUM(IND).GE.0.D0) THEN
          VVXYZ2(ICO,ISTAT)=RMS*DSQRT(COVSUM(IND))
        ELSE
          VVXYZ2(ICO,ISTAT)=9.9999D0
        ENDIF
750   CONTINUE
C
C PRINT RMS
C ---------
      IF (IRCAPR.EQ.0) THEN
        WRITE(LFNPRT,405)RMS2*DSQRT(2.D0),RMS1
405     FORMAT('1',//,1X,131('-')
     1         /,' RMS OF UNIT WEIGHT FOR GPS SINGLE ',
     2           'DIFF. OBSERVABLES: ',F8.5,
     3         /,' (RMS OF UNIT WEIGHT FOR COORDINATE ',
     4           'COMPARISON :',F8.5,')',
     5         /,1X,131('-'),//)
      ELSE
        WRITE(LFNPRT,406)RMS1
406     FORMAT('1',//,1X,131('-'),
     1         /,' RMS OF UNIT WEIGHT FOR COORDINATE ',
     2           'COMPARISON :',F8.5,
     3         /,1X,131('-'),//)
      ENDIF
C
C WRITE MEAN COORDINATE X,Y,Z - VALUES
C ------------------------------------
      IF (IRCAPR.EQ.0) THEN
        WRITE(LFNPRT,404)
404     FORMAT(//,1X,131('-'),
     1         /,' MEAN VALUES OF GEOCENTRIC X,Y,Z - COORDINATES',
     2         /,' RMS1: RMS OF WEIGHTED AVERAGE OF EACH COORDINATE'
     3          ,' COMPONENT'
     4         /,' RMS2: FORMAL ACCURACY OF EACH COORDINATE COMPONENT'
     5          ,' FROM COMBINED SOLUTION'
     6         /,1X,131('-'),/,
     7         /,' NUM  STATION         #FIL FLG         X (M)  ',
     8           '    RMS1   RMS2 ',
     9           '         Y (M)      RMS1   RMS2        Z (M)',
     .           '     RMS1    RMS2 ',
     1         /,1X,131('-'),/)
      ELSE
        WRITE(LFNPRT,407)
407     FORMAT(//,1X,131('-'),
     1         /,' MEAN VALUES OF GEOCENTRIC X,Y,Z - COORDINATES',
     2         /,' RMS1: RMS OF UNWEIGHTED AVERAGE OF EACH COORDINATE'
     3          ,' COMPONENT'
     4         /,' RMS2: FORMAL ACCURACY OF EACH COORDINATE COMPONENT'
     5          ,' FROM COMBINED SOLUTION USING EQUAL WEIGHTS'
     6         /,1X,131('-'),/,
     7         /,' NUM  STATION         #FIL FLG         X (M)  ',
     8           '    RMS1   RMS2 ',
     9           '         Y (M)      RMS1   RMS2        Z (M)',
     .           '     RMS1    RMS2 ',
     1         /,1X,131('-'),/)
      ENDIF
C
      DO 600 ISTAT=1,NSTAT
C
C SET STATION FLAG
        IFXALL=1
        DO 610 IFLSTA=1,NFLSTA(ISTAT)
          IFIL=INDFIL(ISTAT,IFLSTA)
          ISTA=INDSTA(ISTAT,IFLSTA)
C          IF (STAFLG(ISTA,IFIL).EQ.'F') IFXALL=0
          IF (STAFLG(ISTA,IFIL).NE.'F') IFXALL=0
610     CONTINUE
        VVTOT=VVXYZ(1,ISTAT)+VVXYZ(2,ISTAT)+VVXYZ(3,ISTAT)
        IF (IFXALL.EQ.1.AND.VVTOT.LT.1.D-7) THEN
C        IF (IFXALL.EQ.0.OR.VVTOT.LT.1.D-5) THEN
          FLGXYZ(ISTAT)='F'
        ELSE
          FLGXYZ(ISTAT)='M'
        ENDIF
C
        WRITE(LFNPRT,403) STANUM(ISTAT),STANAM(ISTAT),NFLSTA(ISTAT),
     1                    FLGXYZ(ISTAT),
     2                    (POSXYZ(K,ISTAT),VVXYZ(K,ISTAT),
     3                     VVXYZ2(K,ISTAT),K=1,3)
403     FORMAT(I4,2X,A16,I4,2X,A1,2X,3(F15.5,F8.5,F8.5))
600   CONTINUE
      WRITE(LFNPRT,101)
C
C SAVE MEAN COORDINATES IN COORDINATE FILE
C ----------------------------------------
      TITSAV=TITGEN
      TITSAV(66:74)=DATE
      TITSAV(76:80)=TIME
C
C DATUM FROM LAST COORDINATE FILE
      FILCOV=' '
      CALL WTSTAT(1,FILCOV,TITSAV,DATUM,NSTAT,STANAM,POSXYZ,
     1            STANUM,FLGXYZ,TIMFIL(1))
C
C SAVE NEW VARIANCE COVARIANCE MATRIX
C -----------------------------------
      NPAR=3*NSTAT
      FILCOV=' '
      IF (IOPCOV.EQ.0) THEN
        CALL COVSA2(FILCOV,TITSAV,RMS,NOBS,NUNKNO,NPAR,COVSUM,STANAM)
      ELSE
        CALL COVSA2(FILCOV,TITSAV,RMS1,NOBS1,NUNKNO1,NPAR,COVSUM,STANAM)
      ENDIF
C
      IF (IOPBAS.EQ.0) GOTO 9999
C
C GET BASELINE DEFINITION FILE NAME
C ---------------------------------
      CALL GTFLNA(0,'BASDEF ',FILBSL,IRCBSL)
      IF (IRCBSL.EQ.0) THEN
C
        MAXBSL=LINCOUNT(FILBSL,0)
        ALLOCATE(STABSL(2,MAXBSL),STAT=IAC)
        CALL ALCERR(IAC,'STABSL',(/2,MAXBSL/),PGNAME)
C
        CALL OPNFIL(LFNLOC,FILBSL,'OLD','FORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILBSL,'COMPAR')
C
C READ BASELINES TO BE PRINTED
        DO 500 IBASDF=1,MAXBSL
          READ(LFNLOC,'(A16,1X,A)',END=510,ERR=910)
     1                        (STABSL(IB,IBASDF),IB=1,2)
          IF (STABSL(1,IBASDF).EQ.' ') GOTO 510
500     CONTINUE
C
        WRITE(LFNERR,906) MAXBSL,FILBSL
906     FORMAT(/,' *** PG COMPAR: TOO MANY BASELINES IN BASELINE',
     1           ' DEFINITION FILE',
     2         /,16X,'MAX. NUMBER OF BASELINE ALLOWED:',I5,
     3         /,16X,'BASELINE DEFINITION FILE       : ',A,
     4         /,16X,'INCREASE PARAMETER "MAXBSL" IN PG COMPAR',/)
        CALL EXITRC(2)
C
910     WRITE(LFNERR,907) FILBSL,IBASDF
907     FORMAT(/,' *** PG COMPAR: ERROR READING BASELINE DEFINI',
     1           'TION FILE',
     2         /,16X,'BASELINE DEF. FILE: ',A,
     3         /,16X,'LINE NUMBER       :',I5,/)
        CALL EXITRC(2)
C
510     CONTINUE
        CLOSE(UNIT=LFNLOC)
        NBASDF=IBASDF-1
      ENDIF
C
C WRITE BASELINE LENGTH TITLE
C ---------------------------
      WRITE(LFNPRT,430)
430   FORMAT('1',//,1X,131('-'),
     1       /,' COMPARISON OF BASELINE LENGTHS ',
     2         '(REFERENCE: FIRST STATION)',
     3       /,1X,131('-'),/)
C
C LOOP OVER ALL COMBINATIONS OF BASELINES
C ---------------------------------------
      DO 400 ISTAT1=1,NSTAT
C
C SET VARIABLE OF SECOND STATION LOOP
        IF (IRCBSL.EQ.0) THEN
          ISTAR2=1
        ELSE
          ISTAR2=ISTAT1+1
        ENDIF
C
        DO 390 ISTAT2=ISTAR2,NSTAT
C
C CHECK BASELINE DEF.FILE
          IF (IRCBSL.EQ.0) THEN
            IF (ISTAT1.EQ.ISTAT2) GOTO 390
            DO 700 IBASDF=1,NBASDF
              IF (STABSL(1,IBASDF).EQ.STANAM(ISTAT1) .AND.
     1            STABSL(2,IBASDF).EQ.STANAM(ISTAT2)) GOTO 705
700         CONTINUE
            GOTO 390
705         CONTINUE
          ENDIF
C
          NBASE=0
          BASSUM=0.D0
          DO 300 IFLST1=1,NFLSTA(ISTAT1)
            IFIL1=INDFIL(ISTAT1,IFLST1)
            DO 290 IFLST2=1,NFLSTA(ISTAT2)
              IFIL2=INDFIL(ISTAT2,IFLST2)
              IF(IFIL1.EQ.IFIL2) THEN
                NBASE=NBASE+1
                BASFIL(NBASE)=IFIL1
                ISTA1=INDSTA(ISTAT1,IFLST1)
                ISTA2=INDSTA(ISTAT2,IFLST2)
                DIST2=0.D0
                DO 250 I=1,3
                  IF (IOPBAS.EQ.1) THEN
                    POSDIF(I,NBASE)=XSTELL(I,ISTA2,IFIL2)-
     1                              XSTELL(I,ISTA1,IFIL1)
                    IF(I.EQ.1)THEN
                      POSDIF(I,NBASE)=POSDIF(I,NBASE)*AE
                    ELSE IF(I.EQ.2)THEN
                      POSDIF(I,NBASE)=POSDIF(I,NBASE)*AE*
     1                                DCOS(XSTELL(1,ISTA1,IFIL1))
                    END IF
                  ELSE
                    POSDIF(I,NBASE)=XSTAT(I,ISTA2,IFIL2)-
     1                              XSTAT(I,ISTA1,IFIL1)
                  ENDIF
                  DIST2=DIST2+(XSTAT(I,ISTA2,IFIL2)-
     1                         XSTAT(I,ISTA1,IFIL1))**2
250             CONTINUE
                BASLEN(NBASE)=DSQRT(DIST2)
                BASSUM=BASSUM+BASLEN(NBASE)
                GOTO 300
              ENDIF
290         CONTINUE
300       CONTINUE
C
C CHECK NUMBER OF BASELINE LENGTH OBTAINED
          IF(NBASE.LT.2) GOTO 390
C
C COMPUTE MEAN AND RMS FOR BASELINE LENGTH, LAT, LONG, AND HEIGHT
C
C BASELINE LENGHT FROM COMBINED SOLUTION
          BASME1=BASSUM/NBASE
          BASME2=0.D0
          DO 303 I=1,3
            BASME2=BASME2+(POSXYZ(I,ISTAT2)-POSXYZ(I,ISTAT1))**2
303       CONTINUE
          BASME2=DSQRT(BASME2)
C
C L,B,H / X,Y,Z - DIFFERENCE OF COMBINED SOLUTION
          DO 305 I=1,3
            POSME1(I)=0.D0
            DO 304 K=1,NBASE
              POSME1(I)=POSME1(I)+POSDIF(I,K)
304         CONTINUE
            POSME1(I)=POSME1(I)/NBASE
C
            IF (IOPBAS.EQ.1) THEN
              POSME2(I)=POSLBH(I,ISTAT2)-POSLBH(I,ISTAT1)
              IF(I.EQ.1)THEN
                POSME2(I)=POSME2(I)*AE
              ELSE IF(I.EQ.2)THEN
                POSME2(I)=POSME2(I)*AE*
     1                          DCOS(POSLBH(1,ISTAT1))
              ENDIF
            ELSE
              POSME2(I)=POSXYZ(I,ISTAT2)-POSXYZ(I,ISTAT1)
              POSSI2(I)=DSQRT(VVXYZ(I,ISTAT1)**2+VVXYZ(I,ISTAT2)**2)
            ENDIF
305       CONTINUE
C
          SIGMA2=0.D0
          DO 310 IBASE=1,NBASE
            SIGMA2=SIGMA2+(BASLEN(IBASE)-BASME1)**2
310       CONTINUE
          DO 315 I=1,3
            POSSIG(I)=0.D0
            DO 314 K=1,NBASE
              POSSIG(I)=POSSIG(I)+(POSDIF(I,K)-POSME1(I))**2
314         CONTINUE
            IF (NBASE.GT.1) THEN
              POSSIG(I)=DSQRT(POSSIG(I)/(NBASE-1))
            ELSE
              POSSIG(I)=0.D0
            ENDIF
315       CONTINUE
          IF (NBASE.GT.1) THEN
            BASRMS=DSQRT(SIGMA2/(NBASE-1))
            BASRM2=DSQRT(((POSXYZ(1,ISTAT1)*VVXYZ(1,ISTAT1))**2+
     1                    (POSXYZ(1,ISTAT2)*VVXYZ(1,ISTAT2))**2+
     2                    (POSXYZ(2,ISTAT1)*VVXYZ(2,ISTAT1))**2+
     3                    (POSXYZ(2,ISTAT2)*VVXYZ(2,ISTAT2))**2+
     4                    (POSXYZ(3,ISTAT1)*VVXYZ(3,ISTAT1))**2+
     5                    (POSXYZ(3,ISTAT2)*VVXYZ(3,ISTAT1))**2)
     6                    /BASME2**2)
          ELSE
            BASRMS=0.D0
            BASRM2=0.D0
          ENDIF
C
C PRINT BASELINE LENGTH AND DEVIATION FROM MEAN LENGTH
          DO 320 IBASE=1,NBASE
            BASDIF=BASLEN(IBASE)-BASME1
            IF (BASLEN(IBASE).NE.0.D0) THEN
              BASPPM=BASDIF/BASLEN(IBASE)*1.D6
            ELSE
              BASPPM=0.D0
            ENDIF
            IF (IOPBAS.EQ.1) THEN
              IF(IBASE.EQ.1) THEN
                WRITE(LFNPRT,311)STANAM(ISTAT1),STANAM(ISTAT2)
C
C START CHANGE CR: 25-SEP-95
C
C311            FORMAT(1X,'BASELINE : ',A16,'  TO  ',A16,/,
C    1          1X,131(1H-),/,'      FILE   BASE.LENGTH    D(LAT)  ',
C    2               '  D(LON)    D(HGT)    D(LGT)   D(LGT)(PPM)'/)
C             END IF
C             WRITE(LFNPRT,312,IOSTAT=IOSTAT) BASFIL(IBASE),
C    1                        BASLEN(IBASE),
C    2                        POSDIF(1,IBASE)-POSME1(1),
C    3                        POSDIF(2,IBASE)-POSME1(2),
C    4                        POSDIF(3,IBASE)-POSME1(3),
C    5                        BASDIF,BASPPM
C312          FORMAT(1X,I7,F16.4,4F10.4,F9.3)
C           ELSE
C             IF(IBASE.EQ.1) THEN
C               WRITE(LFNPRT,411)STANAM(ISTAT1),STANAM(ISTAT2)
C411            FORMAT(1X,'BASELINE : ',A16,'  TO  ',A16,/,
C    1          1X,131(1H-),/,'      FILE',
C    2               '       DX(M)      DDX(M)  ',
C    3               '       DY(M)      DDY(M)  ',
C    4               '       DZ(M)      DDZ(M)  ',
C    5               '   BASE.LENGTH    DDL(M)    DDL(PPM)',/)
C             END IF
C             WRITE(LFNPRT,412,IOSTAT=IOSTAT) BASFIL(IBASE),
C    1                       POSDIF(1,IBASE),POSDIF(1,IBASE)-POSME1(1),
C    2                       POSDIF(2,IBASE),POSDIF(2,IBASE)-POSME1(2),
C    3                       POSDIF(3,IBASE),POSDIF(3,IBASE)-POSME1(3),
C    4                       BASLEN(IBASE),BASDIF,BASPPM
C412          FORMAT(1X,I7,4(F16.4,F10.4),F10.4)
C           ENDIF
C320      CONTINUE
CC
CC PRINT MEAN BASELINE LENGTH AND RMS
C         IF (BASME1.NE.0.D0) THEN
C           BASRPP=BASRMS/BASME1*1.D6
C         ELSE
C           BASRPP=0.D0
C         ENDIF
C         IF (BASME2.NE.0.D0) THEN
C           BASRP2=BASRM2/BASME2*1.D6
C         ELSE
C           BASRP2=0.D0
C         ENDIF
C         IF (IOPBAS.EQ.1) THEN
C           WRITE(LFNPRT,321,IOSTAT=IOSTAT)NBASE,BASME1,
C    1                (POSSIG(K),K=1,3),BASRMS,BASRPP
C321        FORMAT(1X,131('-'),/,1X,I7,F16.4,4F10.4,F9.3,/)
C         ELSE
C           WRITE(LFNPRT,421,IOSTAT=IOSTAT)NBASE,
C    1                        (POSME1(K),POSSIG(K),K=1,3),
C    2                        BASME1,BASRMS,BASRPP
CC           WRITE(LFNPRT,421,IOSTAT=IOSTAT)NBASE,
CC    1                        (POSME2(K),POSSI2(K),K=1,3),
CC    2                        BASME2,BASRM2,BASRP2
C421      FORMAT(1X,131('-'),/,1X,I7,4(F16.4,F10.4),F10.4,/)
C         ENDIF
C
 311            FORMAT(1X,'BASELINE : ',A16,'  TO  ',A16,/,
     1            1X,131('-'),/,' FILE    BASE.LENGTH    D(LAT)  ',
     2            '  D(LON)    D(HGT)    D(LGT) D(LGT)(PPM) CODE ',
     3            '  YYDDDS   STATION 1        STATION 2',/)
              END IF
              LCODE='_BL_'
              IF( YYDDDS(BASFIL(IBASE)).EQ.'YYDDDS') LCODE='_XX_'
              WRITE(LFNPRT,312,IOSTAT=IOSTAT) BASFIL(IBASE),
     1                        BASLEN(IBASE),
     2                        POSDIF(1,IBASE)-POSME1(1),
     3                        POSDIF(2,IBASE)-POSME1(2),
     4                        POSDIF(3,IBASE)-POSME1(3),
     5                        BASDIF,BASPPM,
     6                        LCODE,YYDDDS(BASFIL(IBASE)),
     7                        STANAM(ISTAT1),STANAM(ISTAT2)
 312          FORMAT(1X,I4,F15.5,4F10.5,F9.3,
     1               4X,A4,3X,A6,3X,A16,1X,A16)
            ELSE
              IF(IBASE.EQ.1) THEN
                WRITE(LFNPRT,411)STANAM(ISTAT1),STANAM(ISTAT2)
411             FORMAT(1X,'BASELINE : ',A16,'  TO  ',A16,/,
     1          1X,131('-'),/,' FILE  ',
     2               '       DX(M)      DDX(M)  ',
     3               '       DY(M)      DDY(M)  ',
     4               '       DZ(M)      DDZ(M)  ',
     5               '   BASE.LENGTH    DDL(M)   DDL(PPM)   CODE   ',
     6               'YYDDDS   STATION 1        STATION 2',/)
              END IF
              LCODE='_BL_'
              IF( YYDDDS(BASFIL(IBASE)).EQ.'YYDDDS') LCODE='_XX_'
              WRITE(LFNPRT,412,IOSTAT=IOSTAT) BASFIL(IBASE),
     1                       POSDIF(1,IBASE),POSDIF(1,IBASE)-POSME1(1),
     2                       POSDIF(2,IBASE),POSDIF(2,IBASE)-POSME1(2),
     3                       POSDIF(3,IBASE),POSDIF(3,IBASE)-POSME1(3),
     4                       BASLEN(IBASE),BASDIF,BASPPM,
     5                       LCODE,YYDDDS(BASFIL(IBASE)),
     6                       STANAM(ISTAT1),STANAM(ISTAT2)
412           FORMAT(1X,I4,4(F16.5,F10.5),F10.5,
     1               4X,A4,3X,A6,3X,A16,1X,A16)
            ENDIF
320       CONTINUE
C
C PRINT MEAN BASELINE LENGTH AND RMS
          IF (BASME1.NE.0.D0) THEN
            BASRPP=BASRMS/BASME1*1.D6
          ELSE
            BASRPP=0.D0
          ENDIF
          IF (BASME2.NE.0.D0) THEN
            BASRP2=BASRM2/BASME2*1.D6
          ELSE
            BASRP2=0.D0
          ENDIF
          IF (IOPBAS.EQ.1) THEN
            WRITE(LFNPRT,321,IOSTAT=IOSTAT)NBASE,BASME1,
     1                (POSSIG(K),K=1,3),BASRMS,BASRPP,
     2                 STANAM(ISTAT1),STANAM(ISTAT2)
321         FORMAT(1X,131('-'),/,1X,I4,F15.5,4F10.5,F9.3,
     1             '    _TT_            ', A16,1X,A16,/)
          ELSE
            WRITE(LFNPRT,421,IOSTAT=IOSTAT)NBASE,
     1                        (POSME1(K),POSSIG(K),K=1,3),
     2                        BASME1,BASRMS,BASRPP,
     3                        STANAM(ISTAT1),STANAM(ISTAT2)
421         FORMAT(1X,131('-'),/,1X,I4,4(F16.5,F10.5),F10.5,
     1             '    _TT_            ', A16,1X,A16,/)
          ENDIF
C
C NEXT BASELINE
390     CONTINUE
400   CONTINUE
C
C PRINT FINAL LINE
C ----------------
      WRITE(LFNPRT,401)
401   FORMAT(/,1X,131('-'),//)
C
C WRITE REMAINING PART OF PLOT FILE (SKELETON KEY: B)
C ---------------------------------------------------
      IF (IRCPLT.EQ.0.AND.IRCSKL.EQ.0) THEN
        CALL WTSKEL('B  ',FILSKL,FIELDS,IPLFLG,LFNPLT)
        CLOSE(UNIT=LFNPLT)
      ENDIF
C
C END
C ---
9999  CALL EXITRC(0)
      END
