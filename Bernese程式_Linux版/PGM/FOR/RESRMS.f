C*
      PROGRAM RESRMS
CC
CC NAME       :  RESRMS
CC
CC PURPOSE    :  DETERMINE THE RMS BASED ON THE RESIDUALS OF
CC               A FILE GENERATED BY
CC               GPSEST, MAUPRP, CODSPP, IONEST, OR RNXCYC
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  T.A.SPRINGER, M.ROTHACHER
CC
CC CREATED    :  93/01/06
CC
CC CHANGES    :  09-NOV-93 : MR: CHANGE NAME OF SUBROUTINE "DSPRES"
CC               02-APR-94 : RW: MAXSAT=30
CC               12-AUG-94 : MR: CALL EXITRC
CC               14-AUG-94 : MR: FORMAT 4: SESSION AS CHARACTER*4
CC               13-JAN-95 : MR: CHECK FOR NO OBS. WHEN COMP. RMS
CC               20-JAN-95 : MR: CHECK MAXIMUM DIMENSION "MAXEDT"
CC               30-APR-95 : MR: FREQUENCY AS INPUT PARAMETER
CC               13-SEP-95 : LM: REORDER DATA STATEMENT
CC               25-SEP-95 : MR: WRITE STATION NAME ABBREVIATIONS
CC               24-SEP-95 : JJ: RM UNUSED FILOU2, FILRES AND TSTRNG
CC               30-SEP-95 : MR: REMOVE "ICARR=3"
CC               04-APR-96 : MR: CHANGE FORMAT "<>"
CC               17-JUN-96 : MR: COSMETIC CHANGE , USE FORMAT
CC               15-FEB-97 : MR: MAXDEL=5000 (OLD:1500)
CC               12-JUN-97 : TS: USE "NDSTA" IN "IST" LOOP
CC               24-SEP-97 : DI: USE MAXSAT.inc
CC               14-OCT-97 : TS: USE "RESMAX" FOR "*" IN SUM-FILE
CC               25-MAR-98 : MR: HANDLE L4 AND L5 RES FILES
CC               28-JUL-98 : MR: ADD "DEFREQ"
CC               17-AUG-99 : SS: PRINT PERCENTAGE OF OBS PER SATELLITE
CC               28-JAN-00 : RD: ADD MINIMUM #OBS. PER AMBIGUITY
CC                               WRITE RESRMS OPTIONS INTO THE .EDT FILE
CC               29-MAR-00 : RD: CALL FOR SR WTEDIT HAS BEEN CHANGED
CC               14-APR-00 : TS: NEW FORMAT FOR TOTAL RMSB IF GE 100
CC               16-MAY-00 : RD: MAXEDT=15000 (OLD:10000)
CC               09-NOV-00 : CU: SWITCH TO NEW MENU SYSTEM
CC               05-SEP-01 : HU: DYNAMIC ALLOCATION OF STABBR ARRAYS
CC               30-SEP-01 : HU: INTERFACE OF GTSTAB MOVED TO I_ASTLIB
CC               06-OCT-01 : HU: INTERFACE OF GTSTAB MOVED TO I_GPSLIB
CC               16-DEC-01 : HU: USE D_CONST
CC               25-Feb-02 : HB: USE D_EDIT AND NEW SR WTEDIT2, DEALLOCATE
CC               25-JAN-02 : MM: NEW OUTPUT LINE ADDED (TOT OBS (1En): ...)
CC               24-APR-02 : MM: REMOVE "TOT-OBS"-BUG REGARDING WRONG FREQ
CC               28-AUG-02 : RD: HANDLE NEW FORMATTED RESIDUAL FILES
CC               02-OCT-02 : RD: INDIVIDUAL RESMAX FOR EACH MEATYP
CC                               OUTPUT FILE IS OPTIONAL
CC               15-JAN-03 : RD: ADD STATION OBSERVATION SIGMA FACTORS
CC               03-FEB-03 : RD: WARNING AND STOP IF NO RESI FILES SELECTED
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               18-MAR-03 : RD: USE STRUCTURE T_ABBREV FOR ABBREVIATION TABLE
CC               23-APR-03 : RD: CLOSE ONLY OPENED FILES
CC               23-APR-03 : HU: NULLIFY LOCAL POINTERS
CC               16-MAX-03 : HB: INITIALIZE STRUCTURES
CC               13-SEP-03 : HU: INTERFACE FOR DEFREQ
CC               03-NOV-03 : RD: CALL SR GTABBV INSTEAD OF SR GETABB
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               17-DEC-03 : RD: CORRECT NUM.SAT IN PROGRAM OUTPUT
CC               02-APR-04 : HU: WRITE NUMBER OF EDIT REQUESTS TO PROTOCOL
CC               21-JUN-05 : MM: LFNUM.inc REMOVED (LFNUMs IN M_BERN)
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               20-SEP-06 : RD: INCREASE LINELENGTH FOR SUMMARY FILE
CC               26-SEP-06 : RD: USE LEN INSTEAD OF LENGT1
CC               05-FEB-07 : AG: ALLOW INFINITY NUMBER OF SATELLITES IN
CC                               HEADER AND FOOTER LINES
CC               27-FEB-07 : AG: CALL DEFCON WITH PARAMETER
CC               12-OCT-07 : RD: REPLACE MAXDEL BY MAXEDT FROM D_EDIT
CC               20-JUL-10 : RD: MORE DIGITS IN OUTPUT TABLES
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               16-MAY-11 : HB/SL: TMPRMS INCLUDED DUE TO PROBLEMS WITH PGF90
CC               01-DEC-11 : SL: NEW TITLE STRING FOR PRITIT, M_BERN WITH ONLY
CC               19-DEC-11 : SL: FMTSTR FOR WRITE LFNLST,*, USE INT FOR IEXP
CC               30-JUL-12 : RD: SR STATIS WITH OPTIONAL ARGUMENTS
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1993     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: i4b, r8b, longLineLength, fileNameLength,
     1                    lfnPrt, lfnRes, lfnErr, lfn001, lfn002
      USE m_cpu,    ONLY: cpu_start
      USE m_global, ONLY: g_meaStr
      USE m_maxdim, ONLY: maxsat
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE d_abbrev, ONLY: t_abbrev,init_abbrev
      USE d_edit,   ONLY: maxEdt, t_edit, init_edit
      USE d_resFil, ONLY: t_resHead, init_resHead
      USE d_stawgt, ONLY: t_stawgt, init_staWgt
      USE f_lengt1
      USE f_prtRms
      USE s_gtabbv
      USE s_opnfil
      USE s_pritit
      USE s_inquire
      USE s_defreq
      USE s_statis
      USE s_defcon
      USE s_opnsys
      USE s_gtflna
      USE s_gtfile2
      USE s_iordup
      USE s_alcerr
      USE s_prflna
      USE s_estwgt
      USE s_readabb
      USE s_rdresh2
      USE s_sumres
      USE s_readinpf
      USE s_opnerr
      USE s_prfile
      USE s_writstwg
      USE s_fparse
      USE s_wtedit2
      USE s_exitrc
      USE s_rrinpt
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I1     , IAC    , ICARR  , ICHR1  , ICLS   ,
     1          ICYCLE , IDEL   , IEDT   , IEXP   , IFIL   , IFTOT  ,
     2          IJ     , ILOOP  , IOBS   , IOSTAT , IRC    , IRCLST ,
     3          IRCSUM , ISAT   , ISATE  , ISATOT , ISIGMA , IST    ,
     4          ISTR1  , ISTR2  , ISTR3  , ITITLE , IUNIT  , IWEIGHT,
     5          IWGT   , IWLFAC , JWGT   , K      , LFNLST ,
     6          MCLS   , MXCDEL , NDEL   , NDSTA  , NEPMED ,
     7          NFFIL  , NOBS   , NP     , NSATOT , NSTABB , NTOT   ,
     8          NUMCLS , NUMWGT
C
      REAL*8    HUNIT  , RESMAX , RMSB   , RMST   , RMSTOT , XMEDIA ,
     1          XSIGMA , ZMAX
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
C MAXIMAL DIMENSIONS
C ------------------
C MAXSAT: MAXIMUM NUMBER OF SATELLITES
C MAXEDT: MAXIMUM NUMBER OF BAD DATA AREAS PER FILE (SEE D_EDIT)
C
! TYPES
! -----
      TYPE(t_edit)    :: edt
      TYPE(t_stawgt)  :: stawgt
      TYPE(t_resHead) :: resHed
      TYPE(t_abbrev)  :: abbrev

C DECLARATIONS
C ------------
      CHARACTER(LEN=longLineLength) :: OUTSTR,STRING,STRIN1,STRIN2
      CHARACTER(LEN=fileNameLength)                        :: FILABR
      CHARACTER(LEN=fileNameLength)                        :: SOSFIL
      CHARACTER(LEN=fileNameLength)                        :: FILOUT
      CHARACTER(LEN=fileNameLength)                        :: FILLST
      CHARACTER(LEN=fileNameLength),DIMENSION(:,:),POINTER :: FFILES
      CHARACTER(LEN=4),DIMENSION(2)                        :: ABBR4
      CHARACTER(LEN=7),DIMENSION(:),ALLOCATABLE            :: HLPSTR
      CHARACTER(LEN=fileNameLength) :: NODE,DEVICE,DIR,NAME,EXT,VER
      CHARACTER*6   MXNDEL
      CHARACTER(LEN=5)                                     :: tmpRms
      CHARACTER(LEN=longLineLength)                        :: fmtStr
C
      INTEGER(i4b), DIMENSION(:), POINTER                  :: ABBIDX
      INTEGER(i4b), DIMENSION(:), ALLOCATABLE              :: NHISTO
      INTEGER*4     FROMTO(2)
      INTEGER*4     NPST(MAXSAT),LSTDEL(5,MAXEDT)
      INTEGER*4     NUMTOT(MAXSAT),IDXSAT(MAXSAT)
C
      REAL(r8b),    DIMENSION(:,:), POINTER     :: WGTCLS
      REAL(r8b),    DIMENSION(:),   POINTER     :: RMSMED
      REAL(r8b),    DIMENSION(:),   ALLOCATABLE :: WEIGHT
      REAL*8        RMS(MAXSAT),RMSS(MAXSAT)
      REAL*8        RMSST(MAXSAT),OBSS(MAXSAT)
      REAL*8        XPOINT(MAXSAT)
      REAL*8        EPOFRQ(2)
C
      LOGICAL       OPENED
C
C COMMON BLOCKS
C -------------
C      COMMON/LARGE/NFRFIL,IFRFIL,MEATYP,NSATEL,NUMSAT,CSESS,IDELTT,
C     1             STANAM
      COMMON/MCMDEL/MXCDEL,MXNDEL
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C NULLIFY POINTERS
C ----------------
      NULLIFY(FFILES)
      NULLIFY(ABBIDX)
      NULLIFY(WGTCLS)
      NULLIFY(RMSMED)
      CALL INIT_ABBREV(abbrev)
      CALL INIT_EDIT(edt)
      CALL INIT_STAWGT(staWgt)
      CALL INIT_RESHEAD(resHed)
      CALL INIT_INPKEY(inpKey)
C
C GET THE NAME OF THE INPUT FILE
C ------------------------------
      CALL readinpf(' ',inpKey)
C
C INITIALIZE COMMON BLOCKS FOR MAXIMAL DIMENSIONS
C -----------------------------------------------
      MXCDEL=MAXEDT
      MXNDEL='MAXDEL'
C
C DEFINE SYSTEM FILES
C -------------------
      CALL OPNSYS
C
C DEFINE CONSTANTS
C ----------------
      CALL DEFCON(1)
C
C START WRITING A PROGRAM OUTPUT
C ------------------------------
      CALL pritit('RESRMS','Create residual statistics')
      CALL prflna
C
C READ FILE WITH LIST OF RESIDUAL FILES
C -------------------------------------
      CALL GTFILE2('RESFIL',1,NFFIL,FFILES)
      CALL PRFILE ('RESFIL','LIST OF RESIDUAL FILES',1)
C
C READ INPUT OPTIONS
C ------------------
      CALL RRINPT(EDT,ICARR,NUMCLS,HUNIT,IWEIGHT,WGTCLS)
C
C NO RESIDUAL FILES SELECTED
C --------------------------
      IF (NFFIL.EQ.0) THEN
        WRITE(LFNERR,'(/,A,/)')
     1        ' ### PG RESRMS: NO RESIDUAL FILES SELECTED'
        CALL exitrc(0)
      ENDIF
C
C ALLOCATE HISTOGRAM ARRAY
C ------------------------
      ALLOCATE(NHISTO(2*NUMCLS+1),STAT=IRC)
      CALL ALCERR(IRC,'NHISTO',(/2*NUMCLS+1/),'RESRMS')
C
      ALLOCATE(HLPSTR(2*NUMCLS+1),STAT=IRC)
      CALL ALCERR(IRC,'HLPSTR',(/2*NUMCLS+1/),'RESRMS')
C
C INIT TITLE VARIABLE FOR OUTPUT FILES
C ------------------------------------
      ITITLE=0
C
C OPEN OUTPUT FILE FOR RESIDUAL STATISTICS
C ----------------------------------------
      CALL GTFLNA(0,'OUTPUT',FILOUT,IRCSUM)
      IF (IRCSUM.EQ.0) THEN
        CALL OPNFIL(LFN001,FILOUT,'UNKNOWN','FORMATTED',
     1                ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFN001,IOSTAT,FILOUT,'RESRMS')
        ITITLE=1
      ENDIF
C
C OPEN HISTOGRAM FILE IF REQUESTED
C --------------------------------
      LFNLST=LFN001+1
      CALL GTFLNA(0,'HISTOGRAM',FILLST,IRCLST)
      IF (IRCLST.EQ.0) THEN
        CALL OPNFIL(LFNLST,FILLST,'UNKNOWN','FORMATTED',
     1                ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNLST,IOSTAT,FILLST,'RESRMS')
        ITITLE=1
      ENDIF
C
C GET STATION NAME ABBREV. TABLE
C ------------------------------
      CALL gtflna(1,'ABBREV ', filAbr, irc)
      CALL readAbb(filAbr,abbrev)
c      FILA80 = ' '
c      WRITE(FILA80,'(A)') FILABR
c      CALL gtstab(FILA80, nstabb, stnabb, abbre4, abbre2, irc)
c      IF (irc == 2) CALL exitrc(2)
C
c      ALLOCATE(ABBR4(nstabb),STAT=iac)
c      CALL alcerr(iac, 'ABBR4', (/ nstabb /), 'RESRMS')
C
C READ ALL OCCURING SATELLITES
C ----------------------------
      NSATOT=0
      IFTOT =0
!
! Loop over residual file headers to count number of files
! --------------------------------------------------------
      DO ILOOP=1,NFFIL
        CALL OPNFIL(LFNRES,FFILES(1,ILOOP),'OLD','UNFORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNRES,IOSTAT,FFILES(1,ILOOP),'RESRMS')
C
C READ HEADER OF RESIDUAL FILE
C ----------------------------
        CALL rdresh2(lfnres,resHed)
C
        iftot=iftot+resHed%nFil
C
        CLOSE(lfnRes)
        DO iFil = 1,resHed%nFil
          DEALLOCATE(resHed%filHead(iFil)%numSat,stat=irc)
        ENDDO
        DEALLOCATE(resHed%filHead,stat=irc)
      ENDDO
!
! Allocate header array for EDIT info file
! ----------------------------------------
      ALLOCATE(edt%head(iftot),stat=iac)
      CALL alcerr(iac, 'edt%head', (/iftot/), 'resrms')

! Allocate header array for station weight file
! ---------------------------------------------
      ALLOCATE(weight(iftot),stat=iac)
      CALL alcerr(iac, 'weight', (/iftot/), 'resrms')
      iftot=0

! Loop over residual file headers to get information
! --------------------------------------------------
      DO ILOOP=1,NFFIL
        CALL OPNFIL(LFNRES,FFILES(1,ILOOP),'OLD','UNFORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNRES,IOSTAT,FFILES(1,ILOOP),'RESRMS')
C
C READ HEADER OF RESIDUAL FILE
C ----------------------------
        CALL rdresh2(lfnres,resHed)
        ndsta = resHed%dsc%nResta+1
C
        DO iFil=1,resHed%nFil
          IFTOT=IFTOT+1
C
          DO ISAT=1,resHed%filHead(iFil)%nSatel
            DO ISATE=1,NSATOT
              IF (NUMTOT(ISATE).EQ.resHed%filHead(iFil)%numSat(iSat))
     1          GOTO 10
            ENDDO
            NSATOT=NSATOT+1
C
C CHECK MAXIMUM NUMBER OF SATELLITES
            IF (NSATOT.GT.MAXSAT) THEN
              WRITE(LFNERR,901) MAXSAT
901           FORMAT(/,' *** PG RESRMS: TOO MANY SATELLITES',
     1               /,16X,'MAXIMUM NUMBER:',I4,/,
     2               /,16X,'INCREASE "MAXSAT" IN PG RESRMS',/)
              CALL EXITRC(2)
            ENDIF
            NUMTOT(NSATOT)=resHed%filHead(iFil)%numSat(iSat)
10          CONTINUE
          ENDDO
C
C FILL INFORMATION FOR EDITING FILE INTO ARRAYS
          edt%head(iftot)%cseEdt(:)=resHed%filHead(iFil)%csess(1:2)
          edt%head(iftot)%staEdt(:)=resHed%filHead(iFil)%stanam(1:2)
          IF (NDSTA.EQ.1) edt%head(iftot)%staEdt(2)=' '
          edt%head(iftot)%timEdt=resHed%filHead(iFil)%timref
          edt%head(iftot)%idtEdt=resHed%filHead(iFil)%ideltt
          edt%head(iftot)%meaEdt=resHed%filHead(iFil)%meatyp
        ENDDO

        DO iFil = 1,resHed%nFil
          DEALLOCATE(resHed%filHead(iFil)%numSat,stat=irc)
        ENDDO
        DEALLOCATE(resHed%filHead,stat=irc)

        CLOSE(LFNRES)
      ENDDO
C
C      WRITE(LFNPRT,'(/,1X,131("-"),//)')
C
C WRITE THE HEADER OF THE PROGRAM OUTPUT
C --------------------------------------
      WRITE(LFNPRT,'(2(A,/),/,2(A,/))')
     1      ' FILE INFORMATION AND STATISTIC:',
     2      ' ------------------------------',
     3      ' Num   Station 1        Station 2         Total RMS ' //
     4      '  med.Resi      Sigma  numObs  nSat  nDel  ObsTyp  S' //
     5      'ession    fileIdx  File name',
     6      ' ---------------------------------------------------' //
     7      '----------------------------------------------------' //
     8      '----------------------------'
C
      edt%nEdFil=iftot
C
C ORDER SATELLITES
C ----------------
      CALL IORDUP(NUMTOT,NSATOT,IDXSAT)
C
C INITIALIZATION
C --------------
      IFTOT =0
      IEDT=0
      NTOT=0
      RMSTOT=0.0
      DO ISATOT=1,NSATOT
        NPST(ISATOT)=0
        RMSST(ISATOT)=0.0
      ENDDO

! Allocate record arrays of edt-structure
! ---------------------------------------
      ALLOCATE(edt%rec(maxEdt),stat=iac)
      CALL alcerr(iac, 'edt%rec', (/maxEdt/), 'resrms')
C
C START LOOP OVER ALL FILES IN FILELIST FILE
C ------------------------------------------
      DO ILOOP=1,NFFIL
        CALL OPNFIL(LFNRES,FFILES(1,ILOOP),'OLD','UNFORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNRES,IOSTAT,FFILES(1,ILOOP),'RESRMS')
C
C READ HEADER OF RESIDUAL FILE
C ----------------------------
        CALL rdresh2(lfnres,resHed)
        ndsta = resHed%dsc%nResta+1
        IUNIT=2
        IWLFAC=1
C
C READ RESIDUALS
C -----------------
        FROMTO(1)=0
        FROMTO(2)=0
C
        DO IFIL=1,resHed%nFil
          IFTOT=IFTOT+1
C
C SET GPS AND GLONASS FREQUENCIES
          EPOFRQ(1)=resHed%filHead(iFil)%timref
          EPOFRQ(2)=resHed%filHead(iFil)%timref
          CALL DEFREQ(EPOFRQ,resHed%filHead(iFil)%nSatel,
     1                resHed%filHead(iFil)%numSat)
C
          NDEL=0
          RESMAX=edt%resMax(resHed%filHead(iFil)%meaTyp)
          NHISTO=0
C
          CALL SUMRES(IFIL,FROMTO,RESMAX,ICARR,IWLFAC,ICYCLE,
     1                IUNIT,NSATOT,NUMTOT,edt%nSampl,edt%minInt,
     2                NUMCLS,HUNIT,NHISTO,XPOINT,RMS,
     3                NEPMED,RMSMED,NDEL,LSTDEL,IRC)
          if (irc /= 0) CYCLE
C
C CALCULATE SATELLITE SPECIFIC RMSS
C ---------------------------------
          DO K=1,NSATOT
            IF (IDNINT(XPOINT(K)).GT.0) THEN
              RMSS(K)=DSQRT(RMS(K)/XPOINT(K))
            ELSE
              RMSS(K)=0.D0
            ENDIF
          ENDDO
C
C CALCULATE RMS FOR THIS BASELINE
C -------------------------------
          NP=0
          RMST=0.
          DO K=1,NSATOT
            IF (IDNINT(XPOINT(K)).GT.0) THEN
              NP=NP+IDNINT(XPOINT(K))
              RMST=RMST+RMS(K)
            ENDIF
          ENDDO
          IF (NP.NE.0) THEN
            RMSB=DSQRT(RMST/NP)
          ELSE
            RMSB=0.D0
          ENDIF
C
C THE STATION WEIGHT IS THE MEAN MEDIAN OF THE SQUARED EPOCH RESIDUALS
C --------------------------------------------------------------------
          CALL STATIS(NEPMED,RMSMED,xMed=XMEDIA)
C
          DEALLOCATE(RMSMED,STAT=IRC)
C
C UPDATE TOTALS OF POINTS AND RMS
C -------------------------------
          DO K=1,NSATOT
            NPST(K)=NPST(K)+IDNINT(XPOINT(K))
            RMSST(K)=RMSST(K)+RMS(K)
          ENDDO
          NTOT=NTOT+NP
          RMSTOT=RMSTOT+RMST
C
C WRITE RMS INFORMATION TO OUTPUT FILE
C ------------------------------------
          IF (ITITLE.EQ.1) THEN
            IF (IRCSUM.EQ.0) THEN
              OUTSTR =''
              OUTSTR(1:15)=' BASELINE  SESS'
              DO ISAT=1,NSATOT
                ISTR1=16+(ISAT-1)*6
                ISTR2=ISTR1+5
                WRITE(OUTSTR(ISTR1:ISTR2),10014) NUMTOT(IDXSAT(ISAT))
              ENDDO
              ICHR1=16+NSATOT*6
              OUTSTR(ICHR1:ICHR1+5)='   TOT'
C
C CHECK LENGTH OF THE STRING
              IF (LEN(OUTSTR).LT.ICHR1+6) THEN
                WRITE(LFNERR,'(/,A,3(/,16X,A),/)')
     1          ' ### PG RESRMS: The summary file will be ' //
     2          'incomplete because of ',
     3          'too many satellites.',
     4          'Enlarge the length of the line!',
     5          '(parameter "longLineLength" in M_BERN.f)'
              ENDIF
              WRITE(LFN001,'(A)') OUTSTR(1:LENGT1(OUTSTR))
              OUTSTR=' '
              DO I1=2,6*NSATOT+21
                OUTSTR(I1:I1)='-'
              ENDDO
              WRITE(LFN001,'(A)') OUTSTR(1:LENGT1(OUTSTR))
            ENDIF
            IF (IRCLST.EQ.0) THEN
              DO ICLS=1,2*NUMCLS+1
                WRITE(HLPSTR(ICLS),'(F7.1)') DBLE(ICLS-NUMCLS-1)*HUNIT
              ENDDO
CC              WRITE(LFNLST,*)
CC     1        'BASELINE  SESS   SIG ',HLPSTR(:)
CC              WRITE(LFNLST,*) '---------------------',
CC     1        ('-------',ICLS=-NUMCLS,NUMCLS)
              WRITE(fmtStr,'(A,I4,A)') '(1X,A,',SIZE(HLPSTR),'A)'
              WRITE(LFNLST,fmtStr) 'BASELINE  SESS   SIG ',HLPSTR(:)
              WRITE(fmtStr,'(A,I4,A)')
     1                     '(1X,A,',LEN(HLPSTR)*SIZE(HLPSTR),'("-"))'
              WRITE(LFNLST,fmtStr) '---------------------'
            ENDIF
            ITITLE=0
          ENDIF
C
C GET STATION NAME ABBREVIATION
C -----------------------------
          DO IST=1,NDSTA
            CALL gtAbbv(0,resHed%filHead(iFil)%stanam(ist),1,filabr,
     1                  abbrev,nstabb,abbIdx)
            IF (NSTABB.EQ.0) THEN
              WRITE(LFNERR,903) resHed%filHead(iFil)%stanam(ist),FILABR
903           FORMAT(/,' *** PG RESRMS: STATION NAME ABBREVIATIONS ',
     1                 'NOT FOUND',
     2               /,16X,'STATION NAME          : ',A,
     3               /,16X,'ABBREVIATION FILE NAME: ',A,/)
              CALL EXITRC(2)
            ELSEIF (NSTABB.GT.1) THEN
              WRITE(LFNERR,904) resHed%filHead(iFil)%stanam(ist),FILABR
904           FORMAT(/,' *** PG RESRMS: MORE THAN ONE ABBREVIATIONS ',
     1                 'FOUND FOR STATION',
     2               /,16X,'STATION NAME          : ',A,
     3               /,16X,'ABBREVIATION FILE NAME: ',A,/)
              ABBR4(IST)=ABBREV%ABB(ABBIDX(1))%STAAB4
            ELSE
              ABBR4(IST)=ABBREV%ABB(ABBIDX(1))%STAAB4
            ENDIF
          ENDDO
          IF (NDSTA.EQ.1) THEN
            WRITE(STRING,10009)ABBR4(1),resHed%filHead(iFil)%csess(1)
          ELSE
            WRITE(STRING,10010)(ABBR4(K),K=1,2),
     1            resHed%filHead(iFil)%csess(1)
          ENDIF
C
          RESMAX=edt%resMax(resHed%filHead(iFil)%meaTyp)*1D3
          DO ISAT=1,NSATOT
            IF (RMSS(IDXSAT(ISAT)).NE.0.D0) THEN
              ISTR1=15+(ISAT-1)*6
              ISTR2=ISTR1+5
              WRITE(STRING(ISTR1:ISTR2),10011) RMSS(IDXSAT(ISAT))
              IF (RESMAX.NE.0.AND.RMSS(IDXSAT(ISAT)).GT.RESMAX.AND.
     1            RMSS(IDXSAT(ISAT)).LT.100.0) THEN
                WRITE(STRING(ISTR1:ISTR1),10012)
              ENDIF
            ENDIF
          ENDDO
          ISTR1=15+NSATOT*6
          ISTR2=ISTR1+6
          tmpRms=prtRms(RMSB)
          WRITE(STRING(ISTR1:ISTR2),10111) tmpRms
          IF (RESMAX.NE.0.AND.RMSB.GT.RESMAX) THEN
            ISTR3=ISTR2+1
            WRITE(STRING(ISTR3:ISTR3),10012)
          ENDIF
10009     FORMAT(A4,5X    ,1X,A4)
10010     FORMAT(A4,'-',A4,1X,A4)
10011     FORMAT(F6.1)
10111     FORMAT(1X,A)
10012     FORMAT('*')
10013     FORMAT(' ',A)
10014     FORMAT(I6)
          IF (IRCSUM.EQ.0) THEN
            WRITE(LFN001,10013) STRING(1:LENGT1(STRING))
          ENDIF
C
C GET A MEASUREMENT SIGMA FROM THE HISTOGRAM
C ------------------------------------------
          NOBS=0
C
C COUNT NUMBER OF ALL OBSERVATIONS AND
C FIND BIN WITH BIGGEST NUMBER OF OBSERV.
          MCLS=NUMCLS+1
          DO ICLS=1,2*NUMCLS+1
            NOBS=NOBS+NHISTO(ICLS)
            IF (ICLS.GT.1.AND.ICLS.LT.2*NUMCLS+1.AND.
     1          NHISTO(ICLS).GT.NHISTO(MCLS)) MCLS=ICLS
          ENDDO
C
C COMPUTE THE SIGMA FROM HISTGRAM
          IOBS=NHISTO(MCLS)
          IF (IOBS.EQ.0) THEN
            ISIGMA=NUMCLS+1
          ELSE IF (NHISTO(1)+NHISTO(2*NUMCLS+1).GE.IOBS*3) THEN
            ISIGMA=NUMCLS+1
          ELSE
            ISIGMA=0
            DO WHILE (ISIGMA.LT.NUMCLS.AND.MCLS-ISIGMA.GT.2)
              ISIGMA=ISIGMA+1
              IF(2*NOBS/IOBS.LT.3) EXIT
              IF (MCLS-ISIGMA.LT.1.OR.MCLS+ISIGMA.GT.2*NUMCLS+1) THEN
                ISIGMA=NUMCLS+1
                EXIT
              ENDIF
              IOBS=IOBS+NHISTO(MCLS-ISIGMA)+NHISTO(MCLS+ISIGMA)
            ENDDO
          ENDIF
          XSIGMA=DBLE(ISIGMA-1)*HUNIT
          IF (IRCLST.EQ.0) THEN
            DO ICLS=1,2*NUMCLS+1
              WRITE(HLPSTR(ICLS),'(I7)') NHISTO(ICLS)
            ENDDO
            NAME=' '
            WRITE(NAME,'(F6.1,A)') XSIGMA,':'
CC            WRITE(LFNLST,*) STRING(1:14),NAME(1:7),HLPSTR(:)
            WRITE(fmtStr,'(A,I4,A)') '(1X,A14,A7,',SIZE(HLPSTR),'A)'
            WRITE(LFNLST,fmtStr) STRING(1:14),NAME(1:7),HLPSTR(:)
           ENDIF
C
C WRITE FILE INFORMATION TO OUTPUT FILE
          STRING=' '
          CALL FPARSE(0,FFILES(1,ILOOP),
     1                NODE,DEVICE,DIR,NAME,EXT,VER,IRC)
          IF (IRC.NE.0) NAME=' '
          WRITE(STRING,13) IFTOT,resHed%filHead(iFil)%stanam(1),
     1                     RMSB,XMEDIA,
     2                     XSIGMA,NP,resHed%filHead(iFil)%nSatel,NDEL,
     2                     G_MEASTR(resHed%filHead(iFil)%meaTyp),
     3                     resHed%filHead(iFil)%csess(1:2),
     4                     IFIL,ILOOP,TRIM(NAME)
13        FORMAT(1X,I3,3X,A16,1X,16X,3(2X,F9.1),I8,2I6,
     1           3X,A5,3X,A4,1X,A1,3X,2I4,2X,A)
          IF (NDSTA.EQ.2)
     1      WRITE(STRING(25:40),'(A)') resHed%filHead(iFil)%stanam(2)
C
          WRITE(LFNPRT,'(A)') TRIM(STRING)
C
C GET THE WEIGHT
C --------------
          weight(iftot)=0d0
          IF (IWEIGHT.EQ.1) THEN
            weight(iftot)=XMEDIA
          ELSE IF (IWEIGHT.EQ.2) THEN
            weight(iftot)=XSIGMA
          ENDIF
C
C SAVE OUTLIER AREAS IN ARRAY
C ---------------------------
          DO IDEL=1,NDEL
            IEDT=IEDT+1
C
C CHECK MAXIMUM DIMENSIONS
            IF(IEDT.GT.MAXEDT) THEN
              WRITE(LFNERR,902) IEDT,MAXEDT
902           FORMAT(/,' *** PG RESRMS: MAX. NUMBER OF ',
     1                      'EDITING REQUESTS EXCEEDED',
     2               /,16X,'NUMBER OF REQUESTS        >=',I6,/,
     3                 16X,'MAX. NUMBER OF REQ. ALLOWED:',I6,/)
              CALL EXITRC(2)
            ENDIF
C
            DO IJ=1,3
              edt%rec(iEdt)%LSTEDT(IJ)=LSTDEL(IJ,IDEL)
            ENDDO
            IF (LSTDEL(4,IDEL).LT.3) THEN
              edt%rec(iEdt)%LSTEDT(4)=LSTDEL(4,IDEL)
            ELSE
              edt%rec(iEdt)%LSTEDT(4)=3
            ENDIF
            edt%rec(iEdt)%LSTEDT(5)=1
            edt%rec(iEdt)%LSTEDT(6)=IFTOT
            edt%rec(iEdt)%LSTEDT(7)=IABS(LSTDEL(5,IDEL))
          ENDDO
        ENDDO
C
C END OF DISPLAY: CLOSE FILES
C ---------------------------
        CLOSE(UNIT=LFNRES)
        DO iFil=1,resHed%nFil
          DEALLOCATE(resHed%filHead(iFil)%numSat,stat=irc)
        ENDDO
        DEALLOCATE(resHed%filHead,stat=irc)
      ENDDO
C
C FINISH THE PROGRAM OUTPUT
C -------------------------
      WRITE(LFNPRT,'(/,1X,131("-"),//)')
C
C WRITE EDITING INFO FILE
C -----------------------
      edt%nEdt=IEDT
      edt%filNam='TAKE FROM FILE'
      CALL wtedit2(edt)
C
C ADD STA WEIGHT INFORMATION
C --------------------------
      IF (IWEIGHT.NE.0) THEN
C
        CALL ESTWGT(EDT,WEIGHT,WGTCLS,STAWGT)
C
        CALL GTFLNA(0,'STAWGTRS',SOSFIL,IRC)

        IF (STAWGT%NWGT > 0.AND.IRC==0) THEN
          STAWGT%TITLE=EDT%TITLE
          CALL WRITSTWG(SOSFIL,STAWGT)
C
C PRINT THE WEIGHT STATISTIC INTO THE PROGRAM OUTPUT
C --------------------------------------------------
          WRITE(LFNPRT,'(A,7X,A,/,A,//,A,/,A,/)')
     1    ' STATION OBSERVATION SIGMA FACTOR SUMMARY:',TRIM(SOSFIL),
     2    ' ----------------------------------------',
     3    ' Sigma factor                            ' //
     4    '                   Number of stations',
     5    ' ----------------------------------------' //
     6    '---------------------------------------'
          DO IWGT=1,SIZE(WGTCLS,2)
            NUMWGT=0
            DO JWGT=1,STAWGT%NWGT
              IF (STAWGT%WGT(JWGT)%WEIGHT.EQ.WGTCLS(1,IWGT)) THEN
                NUMWGT=NUMWGT+1
              ENDIF
            ENDDO
            STRING=' '
            WRITE(STRING,'(2X,F8.3,3X,A,F11.4,A,4X,I4,4X,F6.1,A)')
     1           WGTCLS(1,IWGT),'(measurement noise larger than ',
     2           WGTCLS(2,IWGT),' m)',NUMWGT,
     3           DBLE(NUMWGT)/DBLE(STAWGT%NWGT)*100D0,' %'
            IF (IWGT.EQ.1) STRING(13:58)=' '
            WRITE(LFNPRT,'(A)') TRIM(STRING)
          ENDDO
          WRITE(LFNPRT,'(/,1X,79("-"),//)')
        ENDIF
        DEALLOCATE(stawgt%wgt,stat=irc)
      ENDIF
C
C CALCULATE SATELLITE SPECIFIC TOTAL RMSS
C ---------------------------------------
      DO K=1,NSATOT
        IF (NPST(K).GT.0) THEN
          RMSS(K)=DSQRT(RMSST(K)/NPST(K))
          OBSS(K)=1.D2*NPST(K)/NTOT
        ELSE
          RMSS(K)=0.D0
          OBSS(K)=0.D0
        ENDIF
      ENDDO
C
C CALCULATE TOTAL OBSERVATIONS AND EXPONENT
C -----------------------------------------
      ZMAX=MAXVAL(NPST(1:NSATOT),1)
      IF (ZMAX.EQ.0.D0) ZMAX=1.D0
      IEXP=INT(MAX(LOG10(ZMAX)-4.D0,0.D0))
      NPST(:)=NPST(:)/10**IEXP
C
C CALCULATE TOTAL RMS
C -------------------
      IF (NTOT.NE.0) THEN
        RMSTOT=DSQRT(RMSTOT/NTOT)
      ELSE
        RMSTOT=0.D0
      ENDIF
      IF (IRCSUM.EQ.0) THEN
        STRING=' '
        STRIN1=' '
        STRIN2=' '
        WRITE(LFN001,'(A)')OUTSTR(1:LENGT1(OUTSTR))
        STRING(1:15) = ' TOTAL RMS:    '
        STRIN1(1:15) = ' TOTAL OBS:    '
        WRITE(STRIN2(1:15),10022)IEXP
10022   FORMAT(' TOT OBS (1E',I1,'):')
        DO ISAT=1,NSATOT
          ISTR1=16+(ISAT-1)*6
          ISTR2=ISTR1+5
          tmpRms=prtRms(RMSS(IDXSAT(ISAT)))
          WRITE(STRING(ISTR1:ISTR2),10111) tmpRms
          WRITE(STRIN1(ISTR1:ISTR2),10011) OBSS(IDXSAT(ISAT))
          WRITE(STRIN2(ISTR1:ISTR2),10014) NPST(IDXSAT(ISAT))
        ENDDO
        ICHR1=16+NSATOT*6
        tmpRms=prtRms(RMSTOT)
        WRITE(STRING(ICHR1:ICHR1+5),10111) tmpRms
        WRITE(LFN001,'(A)') TRIM(STRING)
        WRITE(LFN001,'(A)') TRIM(OUTSTR)
        WRITE(LFN001,'(A)') TRIM(STRIN1)
        WRITE(LFN001,'(A)') TRIM(OUTSTR)
        WRITE(LFN001,'(A)') TRIM(STRIN2)
        WRITE(LFN001,'(A/)')TRIM(OUTSTR)
      ENDIF
C
C WRITE NUMBER OF EDIT REQUESTS TO PROTOCOL
C -----------------------------------------
      IF (edt%nEdt == 0) THEN
        WRITE(LFNPRT,"(' NO EDIT REQUESTS',/)")
      ELSE
        WRITE(LFNPRT,"(' NUMBER OF EDIT REQUESTS:',I6,/)") edt%nEdt
      ENDIF
C
C CLOSE FILES
C -----------
      IF (IRCSUM.EQ.0) CLOSE(UNIT=LFN001)
      IF (IRCLST.EQ.0) CLOSE(UNIT=LFNLST)
      CALL INQUIRE(UNIT=LFN002,OPENED=OPENED)
      IF (OPENED)      CLOSE(UNIT=LFN002)

! Deallocate memory
! -----------------
c      DEALLOCATE(ABBR4,stat=iac)
      DEALLOCATE(edt%head,stat=iac)
      DEALLOCATE(edt%rec,stat=iac)
      DEALLOCATE(weight,stat=iac)
C
C END
C ---
      CALL EXITRC(0)

      END PROGRAM RESRMS
