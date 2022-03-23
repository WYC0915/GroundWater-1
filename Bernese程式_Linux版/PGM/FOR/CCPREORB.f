C*
      PROGRAM CCPREORB
CC
CC NAME         : CCPREORB
CC
CC PURPOSE      : CONCATENATION OF PRECISE ORBIT FILES INTO
CC                1 PRECISE ORBIT FILE AND MERGING OF TWO
CC                PRECISE FILES (E.G. GPS AND GLONASS FILE)
CC
CC REMARKS      : SHOULD WORK ALSO FOR SP1, SP2 FILES
CC                SP1/SP2 CAN NOT BE COMBINDED WITH SP3
CC                COMBINATION OF SP3(POS) AND SP3(POS+VEL) IS POSSIBLE
CC
CC AUTHOR       : C. ROCKEN
CC
CC CREATED      : 25-AUG-94
CC
CC CHANGES      : 18-SEP-94 : CR: CALL RDPHE2 WITHOUT INDEX
CC                24-MAR-95 : MR: WARNING ONLY IF DIFF. SYSTEMS
CC                17-JUL-97 : DI: USE SR RDPREH, WTPREH, RDPREI,WTPREI
CC                23-JUL-97 : DI: USE COMLFNUM
CC                23-JUL-97 : DI: WRITE ALL FOUND SAT. INTO NEW PRECISE FILE
CC                25-AUG-98 : DI: ALLOW MERGING OF TWO ORBIT FILES INTO ONE
CC                11-FEB-99 : MR: 4 COMMENT LINES
CC                01-JUL-99 : PF: CALL IYEAR4 FOR CONVERSION YY->YYYY
CC                03-AUG-99 : MR: REMOVE ARRAYS WITH SATELLITE INDEX
CC                12-AUG-99 : JJ: C_LANG_F77/90
CC                15-AUG-99 : JJ: RM UNUSED VARS IDA, IHA, IYA, SECC, IMIA
CC                                NWEEK, SECOND, FROMTO, IMOA
CC                07-JUL-01 : DS: USE END=212 IN READ (NEAR LINE 192)
CC                09-JUL-01 : HB: INITIALIZE IUSECNT
CC                10-OCT-02 : HU: NEW OPTION: SET ACCURACY CODES TO ZERO
CC                12-NOV-02 : HU: SP3C IMPLEMENTED
CC                14-NOV-02 : HU: WRITE ALWAYS LOWEST INPUT FORMAT VERSION
CC                02-FEB-03 : HU: ADAPTED TO V50 STANDARD, NEW OPTIONS
CC                17-FEB-03 : LM: USE M_MAXDIM
CC                28-APR-03 : HU: ERROR CORRECTED FOR IFRMAT=5
CC                16-MAY-03 : RD: INIT TIME WINDOW TO (/0D0,1D20/)
CC                11-SEP-03 : HU: FIND OUT TIME WINDOW IF NOT SPECIFIED
CC                                OPTION "USE ACCURACY CODES" IMPLEMENTED
CC                19-NOV-03 : RD: READ INP-FILENAME IN READINPF (BERNESE)
CC                30-JUN-04 : HU: DO NOT SORT IF MERGING TWO FILES
CC                21-JUN-05 : MM: LFNUM.inc REMOVED (LFNUMs IN M_BERN)
CC                23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC                01-AUG-05 : HU: EPOCH AS STRUCTURE
CC                08-Aug-05 : HB: USE NEW SR TIMST2 (MODULE)
CC                30-JAN-06 : HB: BUGFIX: WRITE ALSO VELOCITIES IN OUTPUT FILE
CC                04-APR-06 : HB: ADD SAMPLING RATE SELECTION
CC                24-APR-06 : HB: ADD SELECTION IF VELOCITY SHOULD BE
CC                                WRITTEN OR NOT
CC                25-APR-06 : HB: BUGFIX: SAMPLING RATE
CC                27-FEB-07 : AG: CALL DEFCON
CC                03-Oct-08 : DT: ADD ORBIT OUTPUT FOR ILRS BENCHMARK
CC                23-SEP-10 : RD: ENABLE CPU COUNTER
CC                08-NOV-11 : RD: ADD REFERENCE EPOCH FOR OUTPUT FILE NAMING
CC                               (BLANK MEANS AUTO)
CC                24-NOV-11 : SL: NEW TITLE STRING FOR PRITIT, M_BERN WITH ONLY
CC                26-JAN-12 : RD/SL: ADD KEYWORD SHOWILRS
CC                04-MAY-12 : RD: USE DMOD FROM MODULE
CC                04-MAY-12 : RD: REMOVE UNUSED VARIABLES, LABELS AND MODULES
CC
CC COPYRIGHT    : ASTRONOMICAL INSTITUTE
CC  (1990)        UNIVERSITY OF BERN
CC                SWITZERLAND
CC
C*
      USE m_bern,   ONLY: r8b, fileNameLength, fileExtLength,
     1                    lfnErr, lfnOrb, lfnOr1, lfnPrt, lfnRes, lfnEph
      USE m_cpu,    ONLY: cpu_start
      USE m_maxdim, ONLY: maxsat
      USE m_epoch,  ONLY: t_epoch, OPERATOR(.epochToReal.)
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE l_basfun, ONLY: dmod
      USE s_opnfil
      USE s_defcon
      USE s_clocks
      USE s_pritit
      USE s_rdpreh
      USE s_rdprei
      USE s_timst2
      USE s_opnsys
      USE s_priwin
      USE s_dimtst
      USE s_dordup
      USE s_prflna
      USE s_readinpf
      USE s_opnerr
      USE s_prfile
      USE s_ccprin
      USE s_wtpreh
      USE s_wtprei
      USE s_exitrc
      USE s_jmt
      USE f_djul
      USE f_lengt1
      USE f_tstkey

      USE s_ckoptb
      USE s_gtflna


      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I      , I4     , IACC   , ICOM   , IDOY   , IEOF2  ,
     1          IEPO_F , IEPO_SC, IEX    , IFLFLG , IFMT   , IFRMA2 ,
     2          IFRMT2 , II     , III    , ILINE  , IMERGE ,
     3          IMONTH1, IOSTAT , IPOS   , IPRED  , IRC    , IRCODE ,
     4          IREADE , IU     , IUSECNT, IYEAR1 , IZERO  ,
     5          J      , K      , LFNTMP , MAXCOM ,
     6          MAXFIL , MAXUSE , NCOM   , NCOM1  , NFIL   ,
     7          NFRMT  , NLOOP  , NOKSV  , NUSE
C
      REAL*8    DOY    , DTMP   , FRAC   , SAMPL  , TBEG   , TCOLL1 ,
     1          TCOLL2 , TEND   , TFIRST , TGPS   , TGPSM  , TLAST  ,
     2          TYF    , XDAY1  , REFEPO , FILEPO
C
CCC       IMPLICIT INTEGER*4 (I-N)
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C
C     MAXIMAL DIMENSIONS
C     ------------------
      PARAMETER(MAXFIL=100,MAXUSE=10,MAXCOM=4)
C
C
      CHARACTER(LEN=fileNameLength) ::  FILSCR
      CHARACTER(LEN=fileNameLength) ::  CCPREOUT,DEVOUT
      CHARACTER*4  CHAR4I
      CHARACTER    COMENT(MAXCOM)*57,COMOUT(MAXCOM)*57
      CHARACTER    CRDATE*9,CRTIME*5,MONTH(12)*3
      CHARACTER    SDOY*3,SSESS*1
      CHARACTER(LEN=fileExtLength)  ::  CHAR3X
C
      CHARACTER(LEN=fileNameLength) ::  filnam
      INTEGER*4    lfnor2
      INTEGER*4    orbILRS
      real(r8b)    mjd_epoch
C
      INTEGER*4    M(8)
C
C     FOR PRECISE ORBIT FILE HEADER:
C     ------------------------------
      INTEGER*4    SVNNUM(MAXSAT,MAXUSE),SATWGT(MAXSAT,MAXUSE)
      INTEGER*4    SVOK(MAXSAT),SWEOK(MAXSAT),SWEORI(MAXSAT)
      INTEGER*4    IFRMAT(MAXUSE),NEPO(MAXUSE)
      INTEGER*4    INDEX(MAXFIL),NSAT(MAXUSE),IWRTE(2),IWRTEM(2)
      INTEGER*4    ACCPOS(4,MAXSAT),ACCVEL(4,MAXSAT)
      INTEGER*4    ACCPO2(4,MAXSAT),ACCVE2(4,MAXSAT)
      INTEGER*4    ACCPOM(4,MAXSAT),ACCVEM(4,MAXSAT)
C
      REAL*8       T_BEG(MAXFIL),T_END(MAXFIL),POS(3,MAXSAT)
      REAL*8       VEL(3,MAXSAT),DTSATC(MAXSAT),DDTSAT(MAXSAT)
      REAL*8       POS2(3,MAXSAT),VEL2(3,MAXSAT),DTSAT2(MAXSAT)
      REAL*8       DDTSA2(MAXSAT),DTTAB(MAXUSE),DDTSAM(MAXSAT)
      REAL*8       POSM(3,MAXSAT),VELM(3,MAXSAT),DTSATM(MAXSAT)
      REAL*8       BASPOS(MAXUSE),BASCLK(MAXUSE)
      REAL*8       SDEVP(4,MAXSAT),SDEVV(4,MAXSAT)
      REAL*8       SDEVP2(4,MAXSAT),SDEVV2(4,MAXSAT)
      REAL*8       SDEVPM(4,MAXSAT),SDEVVM(4,MAXSAT)
      REAL*8       CORRP(6,MAXSAT),CORRV(6,MAXSAT)
      REAL*8       CORRP2(6,MAXSAT),CORRV2(6,MAXSAT)
      REAL*8       CORRPM(6,MAXSAT),CORRVM(6,MAXSAT)
C
      CHARACTER*120 STRING
      CHARACTER*57  TITLE(MAXCOM,MAXUSE)
      CHARACTER*40  TSTRING
      CHARACTER*19  EPOST1,EPOST2
      CHARACTER(LEN=fileNameLength),DIMENSION(maxfil) :: FILPRE
      CHARACTER(LEN=fileNameLength),DIMENSION(maxfil) :: FILPR2
      CHARACTER*5   COOSYS(MAXUSE),DATDES(MAXUSE)
      CHARACTER*4   AGENCY(MAXUSE)
      CHARACTER*3   ORBTYP(MAXUSE),TIMSYS(MAXUSE)
      CHARACTER*2   FILTYP(MAXUSE)
      CHARACTER*1   EVTFLG(4,MAXSAT),EVTFL2(4,MAXSAT),EVTFLM(4,MAXSAT)
C
      TYPE(t_epoch) TMJD,TMJDM
C
      LOGICAL       LUSE(MAXFIL)
C
C DATA BLOCKS
C -------------
      DATA MONTH/'JAN','FEB','MAR','APR','MAY','JUN',
     1           'JUL','AUG','SEP','OCT','NOV','DEC'/
      DATA CHAR4I/' '/
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
C OPEN FILES AND DEFINE CONSTANTS
C -------------------------------
      CALL OPNSYS
      CALL DEFCON(0)
C
C  SOME INITIALIZATION
C  --------------------
      IEX =0
      IFMT=2
      NFRMT=0
C
C  HARDWIRED SCRATCH FILE NAME
C  ---------------------------
      FILSCR='CCPREORB.SCR'
C
C  GET OPTIONS, DEFAULT VALUES ETC.
C  --------------------------------
      CALL CCPRIN(MAXFIL,MAXCOM,NCOM,COMENT,IFLFLG,IMERGE,NFIL,
     1            FILPRE,DEVOUT,CHAR4I,SSESS,CHAR3X,TCOLL1,TCOLL2,
     2            SAMPL,NFRMT,FILSCR,IFMT,IPRED,IACC,IZERO,REFEPO)
C
C  PRINT-FILE
C  ----------
      IF(IMERGE.EQ.1) THEN
        CALL PRITIT ('CCPREORB','Merge precise orbit files')
        CALL PRFLNA
      ELSE
        CALL PRITIT ('CCPREORB','Concatenate precise orbit files')
        CALL PRFLNA
        CALL PRFILE ('CONCAT','PRECISE FILES',1)
      END IF
C
C LOOP OVER FILES AND READ HEADERS
C --------------------------------
      DO I = 1,NFIL
        LFNTMP=-1
        CALL RDPREH(FILPRE(I),LFNTMP,IFRMAT(1),NSAT(1),SVNNUM(1,1),
     1              SATWGT(1,1),T_BEG(I),NEPO(1),DTTAB(1),TITLE(1,1),
     2              DATDES(1),COOSYS(1),ORBTYP(1),AGENCY(1),
     3              FILTYP(1),TIMSYS(1),BASPOS(1),BASCLK(1))
C
        T_END(I)=T_BEG(I)+(NEPO(1)-1)*DTTAB(1)/86400.D0
        LUSE(I)=.FALSE.
      ENDDO
C
C SORT ACCORDING TO START TIMES
C -----------------------------
      IF (IMERGE.EQ.1) THEN
        DO I=1,NFIL
          INDEX(I)=I
        ENDDO
      ELSE
        CALL DORDUP(T_BEG,NFIL,INDEX)
      ENDIF
C
C DECIDE WHICH FILES CAN CONTRIBUTE TO TIME WINDOW
C -------------------------------------------------
      TBEG=1D20
      TEND=0D0
      IUSECNT=0
      DO I = 1,NFIL
        IF(T_END(I).GE.TCOLL1.AND.T_BEG(I).LE.TCOLL2) THEN
          LUSE(I)=.TRUE.
          IUSECNT=IUSECNT+1
          IF (T_BEG(I).LT.TBEG) TBEG=T_BEG(I)
          IF (T_END(I).GT.TEND) TEND=T_END(I)
        ENDIF
      ENDDO
      NUSE=IUSECNT
C
      IF (TCOLL1.EQ.0D00) TCOLL1=TBEG
      IF (TCOLL2.EQ.1D20) TCOLL2=TEND
      CALL PRIWIN(1,(/TCOLL1,TCOLL2/))
C
C ERROR CHECKING AND INFORMATION ABOUT TIME WINDOW
C ------------------------------------------------
      IF(NUSE.GT.MAXUSE) THEN
        CALL TIMST2(1,2,(/TCOLL1,TCOLL2/),TSTRING)
        WRITE(LFNERR,
     1  "(/,' *** PG CCPREORB: Too many precise files',
     2    /,'                  Maximum number allowed          :',I6,
     3    /,'                  Number requested for time window:',I6,
     4    /,'                  Requested start time            :',1X,A,
     5    /,'                  Requested end   time            :',1X,A,
     6    /,'                  Change maxuse in ccpreorb!',/)")
     7     MAXUSE,NUSE,TSTRING(1:19),TSTRING(22:40)
        CALL EXITRC(2)
      ENDIF
      IF(NUSE.EQ.0) THEN
        CALL TIMST2(1,2,(/TCOLL1,TCOLL2/),     TSTRING)
        CALL TIMST2(1,1,T_BEG(INDEX(1)),   EPOST1)
        CALL TIMST2(1,1,T_END(INDEX(NFIL)),EPOST2)
        WRITE(LFNERR,
     1  "(/,' *** PG CCPREORB:  No data found in requested window :'
     2    /,'                   Requested start time   :',1X,A,
     3    /,'                   Earliest available time:',1X,A,
     4    /,'                   Requested end   time   :',1X,A,
     5    /,'                   Latest available time  :',1X,A,
     6    /,'                   Add precise orbit files ',
     7                        ' or adjust time window',/)")
     8      TSTRING(1:19),EPOST1,TSTRING(22:40),EPOST2
        CALL EXITRC(2)
      ENDIF
      IF(IMERGE.EQ.1. AND. NUSE.NE.2) THEN
        WRITE(LFNERR,
     1  "(/,' *** PG CCPREORB: If the option ''merge common epochs'' ',
     2                        '(2 files) is activated',
     3    /,'                  exactly two valid precise orbit must ',
     4                        'be selected',
     5    /,'                  Number of selected files:',I6,/)") NUSE
        CALL EXITRC(2)
      ENDIF
      IF((TCOLL1.NE.0D00.AND.T_BEG(INDEX(1))   .GT.TCOLL1).OR.
     1   (TCOLL2.NE.1D20.AND.T_END(INDEX(NFIL)).LT.
     2                             TCOLL2-DTTAB(1)/86400D0)) THEN
        CALL TIMST2(1,2,(/TCOLL1,TCOLL2/),     TSTRING)
        CALL TIMST2(1,1,T_BEG(INDEX(1)),   EPOST1)
        CALL TIMST2(1,1,T_END(INDEX(NFIL)),EPOST2)
        WRITE(LFNERR,
     1  "(/,' ### PG CCPREORB: Data not available for entire window :'
     2    /,'                  Requested start time   :',1X,A,
     3    /,'                  Earliest available time:',1X,A,
     4    /,'                  Requested end   time   :',1X,A,
     5    /,'                  Latest available time  :',1X,A,
     6    /,'                  Processing continues',/)")
     8      TSTRING(1:19),EPOST1,TSTRING(22:40),EPOST2
      ENDIF
C
C LOOP OVER FILES THAT WILL BE USED - READ HEADERS AGAIN
C TO COLLECT INFORMATION FOR THE FILE NEW HEADER
C ------------------------------------------------------
      IU=0
      IFRMA2=99
      DO II=1,NFIL
        I=INDEX(II)
        IF(LUSE(I)) THEN
          IU=IU+1
          CALL RDPREH(FILPRE(I),LFNTMP,IFRMAT(IU),NSAT(IU),SVNNUM(1,IU),
     1                SATWGT(1,IU),T_BEG(IU),NEPO(IU),DTTAB(IU),
     2                TITLE(1,IU),DATDES(IU),COOSYS(IU),ORBTYP(IU),
     3                AGENCY(IU),FILTYP(IU),TIMSYS(IU),BASPOS(IU),
     4                BASCLK(IU))
          FILPR2(IU)=FILPRE(I)
C CONVERT TO SP3C
          IF(IFMT.EQ.2.AND.(IFRMAT(IU).EQ.2.OR.IFRMAT(IU).EQ.3)) THEN
            IFRMAT(IU)=IFRMAT(IU)+2
            IF (TIMSYS(IU).EQ.'   ') TIMSYS(IU)='GPS'
            BASPOS(IU)=1.25D0
            BASCLK(IU)=1.025D0
          ENDIF
          IF(IFRMAT(IU).LT.IFRMA2) IFRMA2=IFRMAT(IU)
          IF(IFMT.EQ.1)IFRMA2=2
          IF (NFRMT == 1 .AND. (IFRMA2 == 3.OR.IFRMA2 == 5)) THEN
            IFRMA2=IFRMA2-1
          ELSEIF (NFRMT == 2 .AND. (IFRMA2 == 2.OR.IFRMA2 == 4)) THEN
            WRITE(lfnErr,'(A,/,A)')
     1        ' ### PG CCPREORB: Writing of velocities is not ',
     2        '                  possible! '
          ENDIF
        ENDIF
      ENDDO
      NUSE=IU
C
C MERGE: SET ACCURACY CODES FOR SATELLITES IN SECOND FILE TO ZERO
C ---------------------------------------------------------------
      IF (IMERGE.EQ.1.AND.IZERO.EQ.1) THEN
        DO I=1,MAXSAT
          SATWGT(I,2)=0
        ENDDO
      ENDIF
      SWEORI(:)=999
C
C DETERMINE WHICH SVS's WILL BE IN NEW PRECISE FILE
C (ALL FOUND SATELLITES)
C AND COMPUTE WEIGHTS TO USE IN COMBINED FILE
C     -------------------------------------------------
      NOKSV=0
      DO J = 1, NUSE
        DO K = 1, NSAT(J)
          DO II=1,NOKSV
            IF (SVOK(II).EQ.SVNNUM(K,J)) THEN
C ..MERGE AND CONSIDER ACC: GET SMALLEST ACC
              IF (IMERGE.EQ.1.AND.IACC.EQ.1.AND.SATWGT(K,J).NE.0.AND.
     1            SWEORI(II).GT.SATWGT(K,J)) SWEOK(II)=SATWGT(K,J)
C ..NO MERGE: USE LARGEST ACC
              IF (IMERGE.NE.1 .AND. SATWGT(K,J).GT.SWEOK(II))
     1          SWEOK(II)=SATWGT(K,J)
              GOTO 10
            ENDIF
          ENDDO
          NOKSV=NOKSV+1
C
C CHECK MAXIMUM NUMBER OF SATELLITES
          CALL DIMTST(1,2,1,'CCPREORB','MAXSAT','SATELLITES',
     1              'INCLUDE FILE USED',NOKSV,MAXSAT,IRC)
          IPOS=NOKSV
          DO II=1,NOKSV-1
            IF (SVNNUM(K,J).LT.SVOK(II)) THEN
              IPOS=II
              EXIT
            ENDIF
          ENDDO
          DO II=NOKSV-1,IPOS,-1
            SVOK(II+1)  =SVOK(II)
            SWEOK(II+1) =SWEOK(II)
            SWEORI(II+1)=SWEORI(II)
          ENDDO
          SVOK(IPOS)=SVNNUM(K,J)
          SWEOK(IPOS)=SATWGT(K,J)
          IF (IMERGE.EQ.1.AND.J.EQ.1.AND.
     1        SATWGT(K,J).NE.0) SWEORI(IPOS)=SATWGT(K,J)
10        CONTINUE
        ENDDO
      ENDDO
C
C DETERMINE IF OTHER IMPORTANT ITEMS MATCH
C ----------------------------------------
      DO  J = 1, NUSE
        DO K = 1 , J-1
          IF(DTTAB(K).NE.DTTAB(J)) THEN
            WRITE(LFNERR,
     1        "(/,' *** PG CCPREORB: Cannot combine orbit files :',
     2          /,'                  Intervall is different :',2F10.3,
     3          /,'                  File 1 : ',A,
     4          /,'                  File 2 : ',A)")
     5               DTTAB(K),DTTAB(J),TRIM(FILPR2(K)),TRIM(FILPR2(J))
            CALL EXITRC(2)
          ENDIF
          IF(IMERGE.EQ.0) THEN
            IF(COOSYS(K).NE.COOSYS(J)) THEN
              WRITE(LFNERR,
     1        "(/,' ### PG CCPREORB: Cannot combine Orbit Files :',
     2          /,'                  Coordinate system is different : ',
     3          /,'                  Sysems : ',A,'   ',A,
     4          /,'                  File 1 : ',A,
     5          /,'                  File 2 : ',A)")
     6             COOSYS(K), COOSYS(J),TRIM(FILPR2(K)),TRIM(FILPR2(J))
            ENDIF
          ENDIF
          IF(IFRMAT(K).NE.IFRMAT(J)) THEN
            IF(IFRMAT(K).LT.2. OR . IFRMAT(J).LT.2) THEN
              WRITE(LFNERR,
     1          "(/,' *** PG CCPREORB: Cannot combine SP3- with SP1-',
     2                               ' or SP2-format',
     3            /,'                  File 1 : ',A,
     4            /,'                  File 2 : ',A)")
     5               TRIM(FILPR2(K)),TRIM(FILPR2(J))
              CALL EXITRC(2)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C orbit output for ILRS Benchmark
C -------------------------------
      orbILRS=1
      IF (tstkey('SHOWILRS')) THEN
        CALL ckoptb(1,(/'SHOWILRS'/),'CCPREORB','ILRS orbit',irCode,
     1              result1=orbILRS)
      ENDIF
C
      IF (orbILRS.EQ.1) THEN
        CALL ckoptb(1,(/'BENCH'/),'CCPREORB','ILRS orbit',irCode,
     1              result1=orbILRS)
C
        IF (orbILRS.EQ.1) THEN

          CALL gtflna(1,'ORBILRS',filnam,irc)
C
          CALL opnfil(lfnor2,filnam,'UNKNOWN','FORMATTED',' ',' ',
     1                iostat)
          CALL opnerr(lfnerr,lfnor2,iostat,filnam,'CCPREORB')
        END IF
      END IF
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C LOOP OVER USABLE FILES READ OBSERVATIONS AND COPY TO SCRATCH ORB FILE
C IF THE SV IS OK ..
C ---------------------------------------------------------------------
      CALL OPNFIL(LFNRES,FILSCR,'UNKNOWN',' ',' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNRES,IOSTAT,FILSCR,'CCPREORB')
      IEPO_SC=0
      TLAST=0
      IEOF2=0
      TGPSM=0.D0
      DTMP=1D-10
C
      IF(IMERGE.EQ.1) THEN
        NLOOP=1
      ELSE
        NLOOP=NUSE
      ENDIF
C
      DO I =1,NLOOP
C
C       READ HEADER
C       -----------
        CALL RDPREH(FILPR2(I),LFNORB,IFRMAT(I),NSAT(I),SVNNUM(1,I),
     1              SATWGT(1,I),T_BEG(I),NEPO(I),DTTAB(I),TITLE(1,I),
     2              DATDES(I),COOSYS(I),ORBTYP(I),AGENCY(I),
     3              FILTYP(I),TIMSYS(I),BASPOS(I),BASCLK(I))
C CONVERT TO SP3C
        IF(IFMT.EQ.2.AND.(IFRMAT(I).EQ.2.OR.IFRMAT(I).EQ.3)) THEN
          IFRMAT(I)=IFRMAT(I)+2
          IF (TIMSYS(I).EQ.'   ') TIMSYS(I)='GPS'
          BASPOS(I)=1.25D0
          BASCLK(I)=1.025D0
        ENDIF
C
C       READ HEADER OF SECOND FILE IF OPTION 'MERGE' IS REQUESTED
C       ---------------------------------------------------------
        IF(IMERGE.EQ.1) THEN
C
          CALL RDPREH(FILPR2(2),LFNOR1,IFRMAT(2),NSAT(2),SVNNUM(1,2),
     1                SATWGT(1,2),T_BEG(2),NEPO(2),DTTAB(2),TITLE(1,2),
     2                DATDES(2),COOSYS(2),ORBTYP(2),AGENCY(2),
     3                FILTYP(2),TIMSYS(2),BASPOS(2),BASCLK(2))
C CONVERT TO SP3C
          IF(IFMT.EQ.2.AND.(IFRMAT(2).EQ.2.OR.IFRMAT(2).EQ.3)) THEN
            IFRMAT(2)=IFRMAT(2)+2
            IF (TIMSYS(2).EQ.'   ') TIMSYS(2)='GPS'
            BASPOS(2)=1.25D0
            BASCLK(2)=1.025D0
          ENDIF
        ENDIF
C
C       OBSERVATION LOOP
C       ----------------
        IEPO_F=1
600     CONTINUE
        CALL RDPREI(LFNORB,IFRMAT(I),IREADE,NSAT(I),SVNNUM(1,I),TMJD,
     1              POS,VEL,DTSATC,DDTSAT,ACCPOS,ACCVEL,EVTFLG,
     2              IWRTE,SDEVP,SDEVV,CORRP,CORRV,IRC)
        TGPS=.epochToReal.TMJD
        IF(IRC.NE.0) GOTO 700
C
C       ECHO OBSERVATION IF TIME IS OK
C       ------------------------------
        IEPO_F=IEPO_F+1
        IF(TGPS.LE.TLAST) GOTO 600
        IF(TCOLL1.NE.0.D00.AND.TGPS.LT.TCOLL1) GOTO 600
        IF(TCOLL1.NE.1.D20.AND.TGPS.GT.TCOLL2) GOTO 800
        IF (sampl/=0.D0) THEN
          frac=tMjd%frac*86400.D0
          IF (DMOD(frac,sampl)/sampl>=0.01D0.AND.
     1                DMOD(frac,sampl)/sampl<=0.99D0) GOTO 600
        ENDIF

        IEPO_SC=IEPO_SC+1
C
C       USE ALL FOUND SATELLITES. WHEN SATELLITE NOT FOUND IN ONE
C       OF THE PRECISE FILES: SET 0.0 FOR POSITION AND VELOCITY ,
C       999999.999999 FOR CLOCK ERROR VALUES
C       ----------------------------------------------------------
        DO II=1,MAXSAT
          DO I4=1,3
            POS2(I4,II)=0D0
            VEL2(I4,II)=0D0
          ENDDO
          DTSAT2(II)=999999.999999D0
          DDTSAT(II)=999999.999999D0
C
          ACCPO2(1:4,II)=0
          ACCVE2(1:4,II)=0
          EVTFL2(1:4,II)=' '
        ENDDO
C
        DO II=1,NOKSV
          DO III=1,NSAT(I)
            IF (SVOK(II).EQ.SVNNUM(III,I)) THEN
              DO I4=1,3
                POS2(I4,II)=POS(I4,III)
                VEL2(I4,II)=VEL(I4,III)
              ENDDO
              DTSAT2(II)=DTSATC(III)
              DDTSA2(II)=DDTSAT(III)
C
              ACCPO2(1:4,II)=ACCPOS(1:4,III)
              ACCVE2(1:4,II)=ACCVEL(1:4,III)
              EVTFL2(1:4,II)=EVTFLG(1:4,III)
              SDEVP2(1:4,II)=SDEVP(1:4,III)
              SDEVV2(1:4,II)=SDEVV(1:4,III)
              CORRP2(1:6,II)=CORRP(1:6,III)
              CORRV2(1:6,II)=CORRV(1:6,III)
            ENDIF
          ENDDO
        ENDDO
C
C       IF OPTION 'MERGE' ORBIT FILES IS ACTIVATED:
C       READ SECOND FILE AND USE THE SATELLITES OF THE SECOND
C       ORBIT FILE WHICH ARE NOT CONTAINED IN THE FIRST ORBIT FILE
C       -----------------------------------------------------------
        IF(IMERGE.EQ.1) THEN
610       CONTINUE
          IF(IEOF2.EQ.0) THEN
            IF(TGPSM.LT.TGPS-DTMP) THEN
              CALL RDPREI(LFNOR1,IFRMAT(2),IREADE,NSAT(2),SVNNUM(1,2),
     1                    TMJDM,POSM,VELM,DTSATM,DDTSAM,ACCPOM,
     2                    ACCVEM,EVTFLM,IWRTEM,SDEVPM,SDEVVM,CORRPM,
     3                    CORRVM,IRC)
              TGPSM=.epochToReal.TMJDM
C
              IF(IRC.NE.0) IEOF2=1
              GOTO 610
            ELSEIF(TGPSM.LT.TGPS+DTMP) THEN
C
              DO II=1,NOKSV
                IF(POS2(1,II).EQ.0.D0.OR.IACC.EQ.1.OR.
     1             (IPRED.EQ.1.AND.EVTFL2(4,II).EQ.'P'))THEN
                  DO III=1,NSAT(2)
                    IF (SVOK(II).EQ.SVNNUM(III,2)) THEN
                      IF (POS2(1,II).EQ.0D0.OR.
     1                    (IACC.EQ.1.AND.SATWGT(III,2).LT.SWEORI(II)
     2                              .AND.SATWGT(III,2).NE.0).OR.
     3                    (IPRED.EQ.1.AND.EVTFLM(4,III).NE.'P'
     4                               .AND.EVTFL2(4,II).EQ.'P'))THEN
                        DO I4=1,3
                          POS2(I4,II)=POSM(I4,III)
                          VEL2(I4,II)=VELM(I4,III)
                        ENDDO
C
                        ACCPO2(1:3,II)=ACCPOM(1:3,III)
                        ACCVE2(1:3,II)=ACCVEM(1:3,III)
                        EVTFL2(3:4,II)=EVTFLM(3:4,III)
                        SDEVP2(1:3,II)=SDEVPM(1:3,III)
                        SDEVV2(1:3,II)=SDEVVM(1:3,III)
                        CORRP2(1:6,II)=CORRPM(1:6,III)
                        CORRV2(1:6,II)=CORRVM(1:6,III)
                      ENDIF
                    ENDIF
                  ENDDO
                ENDIF
C CLOCKS
                IF(DTSAT2(II).EQ.999999.999999D0.OR.IACC.EQ.1.OR.
     1                    (IPRED.EQ.1.AND.EVTFL2(2,II).EQ.'P'))THEN
                  DO III=1,NSAT(2)
                    IF (SVOK(II).EQ.SVNNUM(III,2)) THEN
                      IF (DTSAT2(II).EQ.999999.999999D0.OR.
     1                    (IACC.EQ.1.AND.SATWGT(III,2).LT.SWEORI(II)
     2                              .AND.SATWGT(III,2).NE.0).OR.
     3                    (IPRED.EQ.1.AND.EVTFLM(2,III).NE.'P'
     4                               .AND.EVTFL2(2,II).EQ.'P'))THEN
                        DTSAT2(II)=DTSATM(III)
                        DDTSA2(II)=DDTSAM(III)
                        EVTFL2(1:2,II)=EVTFLM(1:2,III)
                        SDEVP2(4,II)  =SDEVPM(4,III)
                        SDEVV2(4,II)  =SDEVVM(4,III)
                      ENDIF
                    ENDIF
                  ENDDO
                ENDIF
              ENDDO
              IWRTE(1)=MIN(IWRTE(1),IWRTEM(1))
              IWRTE(2)=MIN(IWRTE(2),IWRTEM(2))
C
            ENDIF
          ENDIF
        ENDIF
C
C       ECHO OBSERVATION TO SCRATCH FILE
C       --------------------------------
        IFRMT2=IFRMA2
C
        CALL WTPREI(LFNRES,IFRMT2,IWRTE,NOKSV,SVOK,TMJD,POS2,VEL2,
     1              DTSAT2,DDTSA2,ACCPO2,ACCVE2,EVTFL2,SDEVP2,SDEVV2,
     2              CORRP2,CORRV2,IRCODE)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       ILRS Benchmark orbit output
C       ---------------------------
        IF (orbILRS.EQ.1) THEN
          mjd_epoch = tmjd%day + tmjd%frac
          write(lfnor2,1010) mjd_epoch,
     1                       (POS2(k,1)*1d3, k=1,3),
     2                       (VEL2(k,1)*1d-1,k=1,3)
1010      FORMAT(2X,F14.8,1X,3(1X,F17.6),3(1X,F17.10))
        END IF
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       DATE AND TIME OF FIRST USED OBSERVATION
C       ---------------------------------------
        IF(IEPO_SC.EQ.1) THEN
          TFIRST=TGPS
        ENDIF
        TLAST=TGPS
        GOTO 600
C
C NEXT FILE
C ---------
700     CONTINUE
        CLOSE (LFNORB)
      ENDDO
C
C FINISH WRITING THE FILE
C -----------------------
800   CONTINUE
      CLOSE (LFNRES)
C
C COMPUTE STARTING TIME AND NUMBER OF EPOCHS
C ------------------------------------------
      FILEPO=TFIRST
      IF (REFEPO.GT.0d0) FILEPO=REFEPO
C
      CALL JMT(FILEPO+1D-10,IYEAR1,IMONTH1,XDAY1)
      TYF   =DJUL(IYEAR1,1,0.D0)
      DOY   =FILEPO-TYF
      IDOY  =IDINT(DOY)
      WRITE(SDOY,'(I3.3)') IDOY
C
C OUTPUT FILE NAME GENERATION
C ---------------------------
      CCPREOUT=TRIM(DEVOUT)//TRIM(CHAR4I)//
     1         TRIM(SDOY)//TRIM(SSESS)//'.'//TRIM(CHAR3X)
      WRITE(LFNPRT,"(' Output file: ',A,/)") TRIM(CCPREOUT)
C
C GET SYSTEM DATE/TIME TO BE STORED IN THE FIRST OUTPUT RECORD
C ------------------------------------------------------------
      CALL CLOCKS(M)
      WRITE(CRDATE,71) M(3),MONTH(M(2)),MOD(M(1),100)
71    FORMAT(I2.2,'-',A3,'-',I2)
      WRITE(CRTIME,72) M(5),M(6)
72    FORMAT(I2.2,':',I2.2)
C
C NOTICE IN HEADER
C ----------------
      IF (COMENT(1)==' ') THEN
        IF(IMERGE.EQ.1) THEN
          TITLE(1,1)=
     1      'ORBIT MERGED BY CCPREORB    : '//CRDATE//' AT '//CRTIME
        ELSE
          TITLE(1,1)=
     1      'ORBIT GENERATED BY CCPREORB : '//CRDATE//' AT '//CRTIME
        ENDIF
      ENDIF
C
      DO ICOM=1,MAXCOM
        COMOUT(ICOM)=' '
      ENDDO
C
      NCOM1=0
      DO ICOM=1,NCOM
        IF (COMENT(ICOM).NE.' ') NCOM1=ICOM
        COMOUT(ICOM)=COMENT(ICOM)
      ENDDO
C
      DO ICOM=1,NCOM
        IF (COMENT(ICOM)(1:5).EQ.'LINE ') THEN
          READ(COMENT(ICOM)(6:6),'(I1)',IOSTAT=IOSTAT) ILINE
          IF (IOSTAT.EQ.0 .AND. ILINE.GE.1 .AND. ILINE.LE.4) THEN
            COMOUT(ICOM)=TITLE(ILINE,1)
          ENDIF
        ENDIF
      ENDDO
      IF(NCOM1.NE.0) THEN
        WRITE(LFNPRT,"(' Comment:',/)")
        DO ICOM=1,NCOM1
          WRITE(LFNPRT,"(1X,A)") COMOUT(ICOM)
        ENDDO
        WRITE(LFNPRT,*)
      END IF
C
C WRITE HEADER INTO OUTPUT FILE
C -----------------------------
      IF (sampl /= 0.D0) dtTab(1)=sampl
      CALL WTPREH(CCPREOUT,LFNEPH,IFRMT2,NOKSV,SVOK,SWEOK,TFIRST,
     1            IEPO_SC,DTTAB(1),COMOUT,DATDES(1),COOSYS(1),
     2            ORBTYP(1),AGENCY(1),TIMSYS(1),BASPOS(1),BASCLK(1))
      CALL OPNFIL(LFNRES,FILSCR,'OLD',' ',' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNRES,IOSTAT,FILSCR,'CCPREORB')
C
C ECHO SCRATCH FILE INTO OUTPUT FILE
C ----------------------------------
      DO
        READ(LFNRES,'(A80)',IOSTAT=IOSTAT) STRING
        IF (IOSTAT.LT.0) EXIT
        WRITE(LFNEPH,'(A)') STRING(1:LENGT1(STRING))
      ENDDO
      CLOSE (LFNRES,STATUS='DELETE')
C
C WRITE END OF FILE "EOF"
C -----------------------
      WRITE(LFNEPH,"('EOF')")
      CLOSE (LFNEPH)
      IEX=0
      GOTO 999
C
C THAT'S ALL
C ----------
999   CALL EXITRC(IEX)
      END
