C*
      PROGRAM RNXSMT
CC
CC NAME        : RNXSMT
CC
CC PURPOSE     : CLEAN RINEX CODE AND PHASE OBSERVATIONS.
CC                 - DETECT OUTLIERS AND CYCLE SLIPS
CC                 - FLAG OUTLIERS
CC                 - REPAIR CYCLE-SLIPS
CC                 - SMOOTH THE CODE OBSERVATIONS
CC
CC REMARKS     : OBSTYP: 1 : L1
CC                       2 : L2
CC                       3 : P1 or C1
CC                       4 : P2 or C2
CC
CC AUTHOR      : T.A.SPRINGER
CC
CC CREATED     : 22-JUL-96
CC
CC CHANGES     : 15-MAY-97 : MR: CORRECT CHANGE OF DO LOOP VARIABLE
CC               24-SEP-97 : DI: USE MAXSAT.inc
CC               20-MAY-98 : TS: ADDED PHASE FLAG OPTION (IFLPHS)
CC               31-AUG-98 : DI: USE DEFREQ (FOR GLONASS SATELLITES)
CC                5-JUL-00 : TS: DO NOT WRITE RINEX FILE IF NTOTO=0
CC               27-JUL-00 : TS: WRITE NTARC NOT NTSLP ON SUMMARY LINE
CC               20-DEC-00 : TS: ADDED CODE FLAG OPTION (IFLCOD)
CC               09-FEB-01 : MM: SWITCH TO THE NEW MENU
CC               15-AUG-02 : SS: UPDATE RINEX INTERVAL VALUE
CC               17-AUG-02 : SS: DO NOT WRITE RINEX FILE IF NTOTO<=0
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               18-FEB-03 : SS: CONSIDER FIRST FREQUENCY TYPE
CC               03-MAR-03 : SC: ADD TITLE SECTION
CC               12-AUG-03 : PS: USE SR DEFREQ1 INSTEAD OF DEFREQ
CC               13-SEP-03 : HU: INTERFACE FOR DEFREQ
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               12-JAN-04 : RD: ADD OBSERVATION WINDOW AS PARAMETER
CC               19-JAN-04 : RD: HANDLE CLOCK EVENTS (SR CHKEVT)
CC               29-APR-04 : RD: NEW CALL OF UPDRNX AND WRTRNX
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: LFNUM.inc, COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-SEP-05 : RD: INPUT OPTION FOR EVENT FLAG HANDLING
CC               20-NOV-06 : RD: REMOVE OBSERV. IF S1&S2-OBS. ARE ZERO
CC               27-FEB-07 : AG: CALL DEFCON WITH PARAMETER
CC               12-OCT-07 : RD: MAXREC IS AN INPUT PARAMETER NOW
CC               02-DEC-08 : RD: SKIP FILE IM CASE OF MAXDIM ERROR IN UPDARC
CC               29-JUL-09 : SS: DIRECT ESTIMATION OF DCB VALUES
CC               29-JUL-09 : SS: USE C2 IF P2 UNAVAILABLE
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               26-JAN-11 : LP: Sat-specific obs types; DEFREQ changes
CC               18-MAY-11 : RD: ADD GLONASS FREQUENCY ESTIMATION AS AN OPTION
CC               03-OCT-11 : LP: Direct estimation of P1-P2 DCBs, too
CC               28-OCT-11 : LP: Re-initialization of gobsdef for every file
CC               01-DEC-11 : SS: REFINED EXTRACTION/COMBINATION OF DCB RESULTS
CC               01-DEC-11 : SL: USE M_BERN WITH ONLY, MAXFIL 200->500
CC               26-JAN-12 : LP: Parameter ICBEST added to calls of SR DCBEST
CC               28-MAR-12 : RD: USE SVN2CHR AS MODULE NOW
CC               24-APR-12 : LP: LOOP OVER GOBSDEF%OBSTYP CHANGED (4->8)
CC               28-FEB-13 : RD: CORRECT RESULT LINES FOR GLONASS FREQ-ESTIM.
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1996     UNIVERSITY OF BERN
CC               SWITZERLAND
C*
      USE m_bern,   ONLY: r8b, lfnPrt, lfnErr, staNameLength
      USE m_cpu,    ONLY: cpu_start
      USE m_maxdim, ONLY: MAXSAT
      USE d_const,  ONLY: C
      USE d_inpkey, ONLY: INPKEY, INIT_INPKEY
      USE d_rinex3, ONLY: t_gobsdef
      USE s_lincmb
      USE s_alcerr
      USE s_gtfile
      USE s_getrnx
      USE s_pritit
      USE s_chkl4p
      USE s_chkoff
      USE s_readinpf
      USE s_updrnx
      USE s_chkevt
      USE s_chknoi
      USE s_chkwub
      USE s_defreq
      USE s_rsmtin
      USE s_defcon
      USE s_exitrc
      USE s_wrtrnx
      USE s_opnsys
      USE s_gtflna
      USE s_updarc
      USE s_gobsdef,ONLY: init_geos
      USE s_dimtst
      USE s_rnxdcb
      USE s_rnxstat
      USE s_svn2chr
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IAC   , IARC  , ICOR  , IFL   , IFLCOD, IFLPHS, IFXSLP,
     1          ILIN  , ILIN1 , IPLFLG, IPOLY , IPRFLG, IRCPLT, IS    ,
     2          ISAT  , ITYP  , MAXARC, MAXCMB, MAXEVT, MAXFIL, MAXREC,
     3          MAXSLP, MINL4 , MINOBS, NARC  , NEPOCH, NEVT  , NFLCOL,
     4          NFLINP, NOBS  , NRSAT , NTARC , NTOT  , NTOTB , NTOTO ,
     5          NTSLP , IEPFLG, IRC   , RMSTON, IC2USE, ICBEST, IGLFRQ,
     6          ISVN  , IEPOCH, JFRQ  , JTST  , FRQNUM0,KFRQ  , KFRQ0 ,
     7          KFRQ1 , ICOMB , IOBS  , IG    , JG     ,NFLSIP
C
      REAL*8    CYCEVT, DEFEVT, DL1   , DL2   , DL4   , DTSIM , GAPARC,
     1          GAPL4 , RMSL3 , RMSL4 , RMSL5 , SLPMIN, SLPOUT, SMPINT
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER(MAXFIL=500,MAXARC=500,MAXCMB=4,MAXSLP=30)
      PARAMETER(DTSIM=0.1d0/86400d0)
C
      INCLUDE 'COMFREQ.inc'
C
C MAXSAT: MAXIMUM NUMBER OF SATELLITES
C MAXREC: MAXIMUM NUMBER OF OBS. PER SATELLITE IN FILE (24 HOURS WITH 30SEC SAMPLING)
C MAXFIL: MAXIMUM NUMBER OF OBS.FILES IN LIST
C MAXARC: MAXIMUM NUMBER OF ARCS PER SATELLITE
C MAXCMB: MAXIMUM NUMBER OF OBSERVATION TYPES USED
C MAXSLP: MAXIMUM NUMBER OF CYCLE SLIPS PER ARC
C DTSIM:  TOLERANCE TO IDENTIFY ONE EPOCH
C
      CHARACTER*1  OBST
      CHARACTER*4  HLP4
      CHARACTER*80 TITLE
      CHARACTER*32 FILRNX(3,MAXFIL),DEVICE,DCBFIL
      CHARACTER*32 SIPFIL(MAXFIL)
      CHARACTER*20 RECTYP
      CHARACTER*2  CODTYP
      CHARACTER*1  SVNCHR
      CHARACTER(LEN=staNameLength)                    :: staNam
      CHARACTER(LEN=1), DIMENSION(:,:,:), ALLOCATABLE :: OBSFLG
      CHARACTER(LEN=1), DIMENSION(:),     ALLOCATABLE :: LINFLG
C
      INTEGER*4    NUMSAT(MAXSAT),SVN,USEGEOS,USEGEOSINIT
      INTEGER*4    ARCTIM(MAXARC,2),NOBA(MAXARC),NBAD(MAXARC)
      INTEGER*4    SLPTIM(MAXSLP,MAXARC),NSLIP(MAXARC)
      INTEGER*4    FRQRNG(2),ITST(2),IFRQ(2),OBSOK(50)
C
      REAL(r8b),        DIMENSION(:),     ALLOCATABLE :: OBSTIM
      REAL(r8b),        DIMENSION(:,:,:), ALLOCATABLE :: OBSREC,OBSAUX
      REAL(r8b),        DIMENSION(:),     ALLOCATABLE :: LINTIM
      REAL(r8b),        DIMENSION(:),     ALLOCATABLE :: LINOBS
      REAL*8       ARCOFF(MAXARC,5),ARCRMS(MAXARC,5),SATRMS(2)
      REAL*8       SLPCYC(MAXSLP,MAXARC,5),EPOFRQ(2),OBSWIN(2)
C
      CHARACTER(LEN=1), DIMENSION(:)  , ALLOCATABLE :: SYSEVT
      REAL(r8b),        DIMENSION(:),   ALLOCATABLE :: TIMEVT
      REAL(r8b),        DIMENSION(:),   ALLOCATABLE :: SIZEVT
      REAl(r8b),        DIMENSION(:,:), ALLOCATABLE :: OBSSAV            !(MAXREC,2)
C
      DATA FRQRNG /-7,24/
C
      TYPE(t_gobsdef)  :: GOBSDEF, GOBSDEFINIT ! Giove External
c                         Obs. Selection info
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
C PRINT TITLE
C -----------
      CALL  pritit('RNXSMT','CLEAN/SMOOTH OBSERVATION FILES')
C
C DEFINE CONSTANTS
C ----------------
      CALL DEFCON(1)
C
C READ INPUT OPTIONS
C ------------------
      CALL RSMTIN(TITLE ,MAXREC,SMPINT,OBSWIN,GAPARC,GAPL4 ,RMSL5 ,
     1     RMSL4 ,RMSL3 ,SLPMIN,SLPOUT,DEFEVT,CYCEVT,MAXEVT,MINOBS,
     2     MINL4 ,IFXSLP,IFLCOD,IFLPHS,IPRFLG,IPLFLG,IEPFLG,RMSTON,
     3     IC2USE,ICBEST,IGLFRQ,USEGEOSINIT,GOBSDEFINIT)
C
C ALLOCATE MAXREC ARRAYS
C ----------------------
      ALLOCATE(OBSFLG(MAXREC,MAXCMB,MAXSAT),STAT=IAC)
      CALL alcerr(iac,'obsflg',(/maxrec,maxcmb,maxsat/),'RNXSMT')
      ALLOCATE(LINFLG(MAXREC),STAT=IAC)
      CALL alcerr(iac,'linflg',(/maxrec/),'RNXSMT')
      ALLOCATE(OBSTIM(MAXREC),STAT=IAC)
      CALL alcerr(iac,'obstim',(/maxrec/),'RNXSMT')
      ALLOCATE(OBSREC(MAXREC,MAXCMB,MAXSAT),STAT=IAC)
      CALL alcerr(iac,'obsrec',(/maxrec,maxcmb,maxsat/),'RNXSMT')
      ALLOCATE(OBSAUX(MAXREC,2,MAXSAT),STAT=IAC)
      CALL alcerr(iac,'obsaux',(/maxrec,2,maxsat/),'RNXSMT')
      ALLOCATE(LINTIM(MAXREC),STAT=IAC)
      CALL alcerr(iac,'lintim',(/maxrec/),'RNXSMT')
      ALLOCATE(LINOBS(MAXREC),STAT=IAC)
      CALL alcerr(iac,'linobs',(/maxrec/),'RNXSMT')
      IF (IGLFRQ.EQ.1) THEN
        ALLOCATE(OBSSAV(MAXREC,2),STAT=IAC)
        CALL alcerr(iac,'obssav',(/maxrec,2/),'RNXSMT')
      ENDIF
C
C ALLOCATE CLOCK EVENT ARRAYS
C ---------------------------
      ALLOCATE(TIMEVT(MAXEVT),STAT=IAC)
      CALL alcerr(iac,'timevt',(/maxevt/),'RNXSMT')
      ALLOCATE(SIZEVT(MAXEVT),STAT=IAC)
      CALL alcerr(iac,'sizevt',(/maxevt/),'RNXSMT')
      ALLOCATE(SYSEVT(MAXEVT),STAT=IAC)
      CALL alcerr(iac,'sysevt',(/maxevt/),'RNXSMT')
C
C CHECK IF PLOTS ARE REQUESTED
C ----------------------------
      CALL GTFLNA(0,'DEVICE ',DEVICE,IRCPLT)
C
C FILE WITH INPUT FILENAMES
C -------------------------
      NFLCOL=3
      CALL GTFILE('RNXFIL ',NFLCOL,MAXFIL,NFLINP,FILRNX)
C
C GET SIP FILENAMES
C -----------------
      IF (icbest == 2) THEN
        NFLCOL=1
        CALL GTFILE('SIPFILE',NFLCOL,MAXFIL,NFLSIP,SIPFIL)
        IF (NFLINP.NE.NFLSIP) THEN
          WRITE(lfnerr,001)
001       FORMAT(/,'*** RNXSMT: There must be one file per station',/,
     1                     12X,'containing slant ionosphere',/,
     2                     12X,'estimates for P1-P2 DCB estimation.',/)
          CALL exitrc(2)
        ENDIF
      ENDIF
C
C LOOP OVER ALL INPUT RINEX FILES
C -------------------------------
      DO 10 IFL=1,NFLINP
C
        USEGEOS=0
C
C INITIALIZE GOBSDEF STRUCTURE FOR CURRENT FILE
C (sat-specific; RINEX3 only)
C ---------------------------------------------
        IF (USEGEOSINIT==2) THEN
          USEGEOS=1
          CALL init_geos(GOBSDEFINIT%NOREC,GOBSDEF)
          DO IG=1,GOBSDEFINIT%NOREC
            GOBSDEF%SAT(IG)%SATNAME = GOBSDEFINIT%SAT(IG)%SATNAME
            GOBSDEF%SAT(IG)%SATNUM  = GOBSDEFINIT%SAT(IG)%SATNUM
            GOBSDEF%SAT(IG)%SYSCHAR = GOBSDEFINIT%SAT(IG)%SYSCHAR
            GOBSDEF%SAT(IG)%SYSNUM  = GOBSDEFINIT%SAT(IG)%SYSNUM
            DO JG=1,4
             GOBSDEF%SAT(IG)%OBSTYP(JG) = GOBSDEFINIT%SAT(IG)%OBSTYP(JG)
            ENDDO
          ENDDO
        ENDIF
C
C INITIALIZE GOBSDEF STRUCTURE FOR CURRENT FILE
C (considering receiver, number of observations, and priority;
C  mandatory for RINEX2.12, possible for RINEX2; RINEX3: either
C  this obstype priority or the above handled sat-specific obstype
C  selection must be used)
C -----------------------------------------------------------------
        IF (USEGEOSINIT==1) THEN
          CALL RNXSTAT(FILRNX(1,IFL),USEGEOS,GOBSDEF)
          IF (USEGEOS==0) CYCLE
        ENDIF
C
C GET OBSERVATIONS FROM RINEX FILE
C --------------------------------
        IF (USEGEOS==1) THEN
          CALL GETRNX(MAXREC,MAXCMB,FILRNX(1,IFL),FILRNX(3,IFL),
     1              SMPINT,OBSWIN,IEPFLG,RMSTON,IC2USE,ICBEST,
     2              NRSAT ,NUMSAT,NEPOCH,OBSTIM,OBSREC,OBSFLG,
     3              CODTYP,IRC,USEGEOS=USEGEOS,GOBSDEF=GOBSDEF,
     4              SIPFIL=SIPFIL(IFL))
        ELSE
C
          CALL GETRNX(MAXREC,MAXCMB,FILRNX(1,IFL),FILRNX(3,IFL),
     1              SMPINT,OBSWIN,IEPFLG,RMSTON,IC2USE,ICBEST,
     2              NRSAT ,NUMSAT,NEPOCH,OBSTIM,OBSREC,OBSFLG,
     3              CODTYP,IRC,SIPFIL=SIPFIL(IFL))
        ENDIF
C
        IF (ICBEST > 0) GOTO 10
        IF (IRC.EQ.0) THEN
          WRITE(LFNPRT,11)FILRNX(1,IFL)
11        FORMAT(//,1X,'PROCESSING FILE: ',A32,/,
     1           1X,60('*'),/)
        ELSE
          WRITE(LFNPRT,15)FILRNX(1,IFL)
15        FORMAT(//,1X,'SKIPPED PROCESSING FILE: ',A32,/,
     1           1X,60('*'),/)
          GOTO 10
        ENDIF
C
C CHECK FOR CLOCK EVENTS
C ----------------------
        CALL CHKEVT(DEFEVT,CYCEVT,IPRFLG,RMSL3 ,MAXEVT,NEPOCH,MAXCMB,
     1              NRSAT ,NUMSAT,OBSTIM,OBSREC,OBSFLG,NEVT  ,TIMEVT,
     2              SIZEVT,SYSEVT)
C
C SKIP FILE IF TOO MANY CLOCK EVENTS DETECTED
C -------------------------------------------
        IF (NEVT.GT.MAXEVT) THEN
          WRITE(LFNERR,'(/,A,/,16X,A,A,/,2(16X,A,I8,/))')
     1    ' ### PG RNXSMT: ' //
     2         'TOO MANY CLOCK EVENTS, NO FURTHER PROCESSING',
     3         'FILENAME           : ',TRIM(FILRNX(1,IFL)),
     4         'NUMBER OF EVENTS   : ',NEVT,
     5         'MAX. NUMBER ALLOWED: ',MAXEVT
          GOTO 10
        ENDIF
C
C INITIALIZE SOME FILE SPECIFIC VARIABLES
C ---------------------------------------
        NTOTO=0
        NTOTB=0
        NTSLP=0
        NTARC=0
C
C  CHECK IF SATELLITES ARE SET ACTIVE IN SATELLITE FILE AND SET
C  THE CORRESPONDING FREQUENCIES (CF.'I:COMFREQ')
C  ------------------------------------------------------------
        EPOFRQ(1)=OBSTIM(1)
        EPOFRQ(2)=OBSTIM(NEPOCH)
        OBST='U'
        CALL DEFREQ(EPOFRQ,NRSAT,NUMSAT,
     1           USEGEOS=USEGEOS,GOBSDEF=GOBSDEF,MEATYPC=OBST)
C
C CLEAN RINEX DATA SATELLITE BY SATELLITE
C ---------------------------------------
        DO 1000 ISAT=1,NRSAT
          SVN=NUMSAT(ISAT)
C
C INIT GLONASS FREQUENCY ESTIMATION (OR EVEN NOT)
C -----------------------------------------------
          FRQNUM0 = FREQNM(SVN)
          ITST(1) = 2
          ITST(2) = 2
C
C RECOVER THE ORIGINAL PHASE OBSERVATIONS FOR FREQUENCY ESTIMATION
C ----------------------------------------------------------------
          CALL SVN2CHR(SVN,ISVN,SVNCHR)
          IF (IGLFRQ.EQ.1.AND.SVNCHR.EQ.'R') THEN
            DO IEPOCH=1,NEPOCH
              OBSSAV(IEPOCH,1:2) = 0D0
              IF (OBSREC(IEPOCH,1,ISAT).NE.0D0)
     1          OBSSAV(IEPOCH,1)=OBSREC(IEPOCH,1,ISAT)/WLGT(1,SVN)
              IF (OBSREC(IEPOCH,2,ISAT).NE.0D0)
     1          OBSSAV(IEPOCH,2)=OBSREC(IEPOCH,2,ISAT)/WLGT(2,SVN)
              IF (OBSREC(IEPOCH,1,ISAT).NE.0D0 .AND.
     1            OBSREC(IEPOCH,2,ISAT).NE.0D0 .AND.
     2            OBSREC(IEPOCH,3,ISAT).NE.0D0 .AND.
     3            OBSREC(IEPOCH,4,ISAT).NE.0D0 ) ITST(1)=1
            ENDDO
          ENDIF
C
C RUN TWO TIMES THROUGH:
C     JTST = 1: FREQUENCY ESTIMATION (ONLY IF NECESSARY)
C     JTST = 2: REAL DATA SCREENING WITH NOMINAL FREQUENCY
C --------------------------------------------------------
          DO JTST = ITST(1),ITST(2)
            IF (JTST.EQ.1) THEN
              IFRQ(1) = FRQRNG(1)
              IFRQ(2) = FRQRNG(2)
              CALL DIMTST(1,2,1,'RNXSMT','OBSOK',
     1                    'OBSERVATION STATISTICS',' ',
     2                    IFRQ(2)-IFRQ(1)+1,SIZE(OBSOK),IRC)
            ELSE
              IFRQ(1) = FRQNUM0
              IFRQ(2) = FRQNUM0
            ENDIF
C
C LOOP THE FREQUENCIES
C --------------------
            DO JFRQ = IFRQ(1),IFRQ(2)
              IF (ITST(1).EQ.1) THEN
                FRQ(1,SVN) = FREQG(1)+JFRQ*DFREQG(1)
                FRQ(2,SVN) = FREQG(2)+JFRQ*DFREQG(2)
C
                WLGT(1,SVN) = C/(FREQG(1)+JFRQ*DFREQG(1))
                WLGT(2,SVN) = C/(FREQG(2)+JFRQ*DFREQG(2))
                WLGT(3,SVN) = WLGT(1,SVN)*WLGT(2,SVN)/(WLGT(1,SVN)+
     1                                                 WLGT(2,SVN))
                WLGT(4,SVN) = WLGT(1,SVN)-WLGT(2,SVN)
                WLGT(5,SVN) = WLGT(1,SVN)*WLGT(2,SVN)/(WLGT(2,SVN)-
     1                                                 WLGT(1,SVN))
C
                FACLIN(3,1,SVN) = FRQ(1,SVN)**2/(FRQ(1,SVN)**2-
     1                                                   FRQ(2,SVN)**2)
                FACLIN(3,2,SVN) =-FRQ(2,SVN)**2/(FRQ(1,SVN)**2-
     1                                                   FRQ(2,SVN)**2)
                FACLIN(5,1,SVN) = FRQ(1,SVN)/(FRQ(1,SVN)-FRQ(2,SVN))
                FACLIN(5,2,SVN) =-FRQ(2,SVN)/(FRQ(1,SVN)-FRQ(2,SVN))
C
C PREPARE THE OBSERVATIONS
C ------------------------
                DO IEPOCH=1,NEPOCH
                  IF (OBSSAV(IEPOCH,1).NE.0D0)
     1              OBSREC(IEPOCH,1,ISAT)=OBSSAV(IEPOCH,1)*WLGT(1,SVN)
                  IF (OBSSAV(IEPOCH,2).NE.0D0)
     1              OBSREC(IEPOCH,2,ISAT)=OBSSAV(IEPOCH,2)*WLGT(2,SVN)
                ENDDO
C
C HEADER FOR FREQUENCY TEST OUTPUT
C --------------------------------
                IF (JTST.EQ.1.AND.JFRQ.EQ.IFRQ(1).AND.IPRFLG.EQ.1) THEN
                  WRITE(LFNPRT,16) SVN
16                FORMAT(/,1X,78('-'),/,
     1                   1X,'FREQUENCY ESTIMATION FOR PRN:',I3,/,
     2                   1X,78('-'))
                ENDIF
C
C FREQUENCY TEST: REPORT DIFFERENCE BETWEEN ESTIMATED AND EXPECTED
C ----------------------------------------------------------------
                IF (JTST.EQ.2) THEN
                  KFRQ0=-999999
                  DO KFRQ=FRQRNG(1),FRQRNG(2)
                    KFRQ1=KFRQ-FRQRNG(1)+1
                    IF (OBSOK(KFRQ1).EQ.0) CYCLE
                    IF (KFRQ0.EQ.-999999) THEN
                      KFRQ0=KFRQ1
                    ELSEIF (OBSOK(KFRQ1).GT.OBSOK(KFRQ0)) THEN
                      KFRQ0=KFRQ1
                    ENDIF
                  ENDDO

                  IF (IPRFLG.EQ.1.OR.
     1                (KFRQ0+FRQRNG(1)-1.NE.FRQNUM0.AND.
     2                 KFRQ0.NE.-999999)) THEN
                    IF ( KFRQ0.EQ.-999999 ) THEN
                      HLP4='  --'
                    ELSE
                      WRITE(HLP4,'(I4)')KFRQ0+FRQRNG(1)-1
                    ENDIF
C
                    WRITE(LFNPRT,17) SVN,HLP4,
     1                               FRQNUM0,TRIM(FILRNX(1,IFL))
17                  FORMAT(1X,78('-'),/,
     1                     1X,'PRN:',I3,2X,'FREQ-NUM: EST',A,2X,
     2                              'EXP',I4,2X,'FILE: ',A,/,
     3                     1X,78('-'),/)
                  ENDIF
                ENDIF
C
              ENDIF
C
C 1) CREATE MELBOURNE WUBBENA COMBINATION
C =======================================
              ITYP=10
              CALL LINCMB(MAXREC,MAXCMB,MAXSAT,NEPOCH,OBSTIM,OBSREC,
     1                    ISAT  ,SVN   ,ITYP  ,NOBS  ,LINTIM,LINOBS,
     2                    LINFLG)
c
c              IF ((SVN==201).OR.(SVN==216)) THEN
c                DO IOBS=1,NOBS
c                  write(*,111) SVN, LINTIM(IOBS), LINOBS(IOBS)
c111               FORMAT(I3,2X,F8.5,2X,F14.3)
c                ENDDO
c              ENDIF
C
C SET-UP ARC INFORMATION
C ----------------------
              CALL UPDARC(MAXSLP,MAXARC,DTSIM ,GAPARC,MINOBS,NOBS  ,
     1                    LINTIM,LINOBS,LINFLG,NEVT  ,TIMEVT,SIZEVT,
     2                    SYSEVT,SVN   ,FILRNX(1,IFL),NARC  ,NOBA  ,
     3                    NBAD  ,ARCTIM,NSLIP ,SLPTIM,SLPCYC,IRC)
              IF (IRC.NE.0) GOTO 10
C
C CHECK EACH MELWUB ARC FOR CYCLE SLIPS AND OUTLIERS
C -----------------------------------------------------
              SATRMS(1)=0D0
              DO 100 IARC=1,NARC
                IF (NOBA(IARC)-NBAD(IARC).GE.MINOBS) THEN
                  IPOLY=1
                  IS=ARCTIM(IARC,1)
                  CALL CHKWUB(FILRNX(1,IFL),OBSTIM(1),
     1                        MAXSLP,RMSL5,MINOBS,SLPMIN,SLPOUT,IPOLY,
     2                        NOBA(IARC),SVN,LINTIM(IS),LINOBS(IS),
     3                        LINFLG(IS),NBAD(IARC),ARCOFF(IARC,5),
     3                        ARCRMS(IARC,5),NSLIP(IARC),SLPTIM(1,IARC),
     4                        SLPCYC(1,IARC,5))
                  SATRMS(1)=SATRMS(1)+
     1                      ARCRMS(IARC,5)*(NOBA(IARC)-NBAD(IARC))
                ENDIF
100           CONTINUE
C
C DETERMINE THE MELWUB RMS
C ------------------------
              NTOT=0
              DO 120 IARC=1,NARC
                NTOT=NTOT+NOBA(IARC)-NBAD(IARC)
120           CONTINUE
              IF (NTOT.GT.1) THEN
                SATRMS(1)=SATRMS(1)/NTOT
              ELSE
                SATRMS(1)=0D0
              ENDIF
C
C PLOT CLEAN MELWUB ARCS IF REQUESTED
C -----------------------------------
CC              IF (IRCPLT.EQ.0 .AND. NTOT.GT.1) THEN
CC                CALL LINPLT(MAXARC,DEVICE,SVN   ,NOBS  ,
CC     1                      LINTIM,LINOBS,LINFLG,NARC  ,ARCTIM)
CC              ENDIF
C
C 2) CREATE L4 COMBINATION
C ========================
              ITYP=4
              CALL LINCMB(MAXREC,MAXCMB,MAXSAT,NEPOCH,OBSTIM,OBSREC,
     1                    ISAT  ,SVN   ,ITYP  ,NOBS  ,LINTIM,LINOBS,
     2                    LINFLG)
C
C TRY TO DETERMINE THE SIZE OF THE DETECTED CYCLE SLIPS IN L4
C -----------------------------------------------------------
              DO 200 IARC=1,NARC
                IF ((NSLIP(IARC).GT.0) .AND.
     1              (NOBA(IARC)-NBAD(IARC).GE.MINOBS)) THEN
                  IS=ARCTIM(IARC,1)
                  IPOLY=2
                  CALL CHKL4P(RMSL4,GAPL4 ,MINL4 ,IFXSLP,IPOLY,
     1                        NOBA(IARC),SVN,LINTIM(IS),LINOBS(IS),
     2                        LINFLG(IS),NBAD(IARC),NSLIP(IARC),
     3                        SLPTIM(1,IARC),  SLPCYC(1,IARC,1),
     4                        SLPCYC(1,IARC,2),SLPCYC(1,IARC,3),
     5                        SLPCYC(1,IARC,4),SLPCYC(1,IARC,5))
                ENDIF
C
C NEXT L4 ARC OF THIS SATELLITE
C -----------------------------
200           CONTINUE
C
C UPDATE ARC INFORMATION
C ----------------------
              CALL UPDARC(MAXSLP,MAXARC,DTSIM ,GAPARC,MINOBS,NOBS  ,
     1                    LINTIM,LINOBS,LINFLG,NEVT  ,TIMEVT,SIZEVT,
     2                    SYSEVT,SVN   ,FILRNX(1,IFL),NARC  ,NOBA  ,
     3                    NBAD  ,ARCTIM,NSLIP ,SLPTIM,SLPCYC,IRC)
              IF (IRC.NE.0) GOTO 10
C
C UPDATE FILE INFO FOR SUMMARY
C ----------------------------
              NTOT=0
              DO 220 IARC=1,NARC
                NTOT=NTOT+NOBA(IARC)-NBAD(IARC)
220           CONTINUE
C
C PLOT CLEAN L4 IF REQUESTED
C --------------------------
CC              IF (IRCPLT.EQ.0 .AND. NTOT.GT.1) THEN
CC                CALL LINPLT(MAXARC,DEVICE,SVN   ,NOBS  ,
CC     1                      LINTIM,LINOBS,LINFLG,NARC  ,ARCTIM)
CC              ENDIF
C
C 3) NOW AS LAST CHECK THE NOISE IN L3-P3 (FOR CODE BLUNDERS)
C ===========================================================
              ICOR=1
              ILIN=3
              ITYP=8
              CALL LINCMB(MAXREC,MAXCMB,MAXSAT,NEPOCH,OBSTIM,OBSREC,
     1                    ISAT  ,SVN   ,ITYP  ,NOBS  ,LINTIM,LINOBS,
     2                    LINFLG)
C
              SATRMS(2)=0D0
              DO 300 IARC=1,NARC
                IF (NOBA(IARC)-NBAD(IARC).GE.MINOBS) THEN
                  IS=ARCTIM(IARC,1)
                  CALL CHKNOI(RMSL3,MINOBS,NOBA(IARC),NBAD(IARC),
     1                        LINTIM(IS),LINOBS(IS),LINFLG(IS),
     2                        ARCOFF(IARC,ILIN),ARCRMS(IARC,ILIN),
     3                        NSLIP(IARC) ,SLPTIM(1,IARC),
     4                        SLPCYC(1,IARC,ILIN),WLGT(ILIN,SVN),ICOR)
                  SATRMS(2)=SATRMS(2)+
     1                      ARCRMS(IARC,ILIN)*(NOBA(IARC)-NBAD(IARC))
                ENDIF
300           CONTINUE
C
C UPDATE ARC INFORMATION
C ----------------------
              CALL UPDARC(MAXSLP,MAXARC,DTSIM ,GAPARC,MINOBS,NOBS  ,
     1                    LINTIM,LINOBS,LINFLG,NEVT  ,TIMEVT,SIZEVT,
     2                    SYSEVT,SVN   ,FILRNX(1,IFL),NARC  ,NOBA  ,
     3                    NBAD  ,ARCTIM,NSLIP ,SLPTIM,SLPCYC,IRC)
              IF (IRC.NE.0) GOTO 10
C
C UPDATE FILE INFO FOR SUMMARY AND DETERMINE THE L3-P3 RMS
C --------------------------------------------------------
              NTOT=0
              IF (JTST.EQ.2) THEN
                DO 310 IARC=1,NARC
                  NTOT=NTOT+NOBA(IARC)-NBAD(IARC)
                  NTOTO=NTOTO+NOBA(IARC)-NBAD(IARC)
                  NTOTB=NTOTB+NBAD(IARC)
                  NTSLP=NTSLP+NSLIP(IARC)
                  IF (NOBA(IARC).GE.NBAD(IARC)) NTARC=NTARC+1
310             CONTINUE
                IF (NTOT.GT.1) THEN
                  SATRMS(2)=SATRMS(2)/NTOT
                ELSE
                  SATRMS(2)=0D0
                ENDIF
              ENDIF
C
C PLOT CLEAN L3-P3 ARCS IF REQUESTED
C -----------------------------------
CC              IF (IRCPLT.EQ.0 .AND. NTOT.GT.1) THEN
CC                CALL LINPLT(MAXARC,DEVICE,SVN   ,NOBS  ,
CC     1                      LINTIM,LINOBS,LINFLG,NARC  ,ARCTIM)
CC              ENDIF
C
C CREATE L1-P1, L2-P2 and P4 COMBINATION
C --------------------------------------
              DO 400 ILIN=1,3
                ILIN1=ILIN
                IF (ILIN1.EQ.3) THEN
                  ILIN1=4
                  ITYP=5
                  ICOR=0
                ELSE
                  ITYP=5+ILIN1
                  ICOR=1
                ENDIF
                CALL LINCMB(MAXREC,MAXCMB,MAXSAT,NEPOCH,OBSTIM,OBSREC,
     1                      ISAT  ,SVN   ,ITYP  ,NOBS  ,LINTIM,LINOBS,
     2                      LINFLG)
C
C 4) GET L1-P1, L2-P2 and P4 OFFSETS
C ==================================
                DO 410 IARC=1,NARC
                  IF (NOBA(IARC)-NBAD(IARC).GE.MINOBS) THEN
                    IS=ARCTIM(IARC,1)
                    CALL CHKOFF(NOBA(IARC),LINTIM(IS),LINOBS(IS),
     1                          LINFLG(IS),ARCOFF(IARC,ILIN1),
     2                          ARCRMS(IARC,ILIN1),NSLIP(IARC) ,
     3                          SLPTIM(1,IARC),SLPCYC(1,IARC,ILIN1),
     4                          WLGT(ILIN1,SVN),ICOR)
                  ENDIF
410             CONTINUE
C
C PLOT CLEAN DATA IF REQUESTED
C ----------------------------
CC                IF (IRCPLT.EQ.0 .AND. NTOT.GT.1 .AND. IPLFLG.EQ.1) THEN
CC                  CALL LINPLT(MAXARC,DEVICE,SVN   ,NOBS  ,
CC     1                        LINTIM,LINOBS,LINFLG,NARC  ,ARCTIM)
CC                ENDIF
400           CONTINUE
C
C COMPUTE THE REAL L1-P1 AND L2-P2 OFFSETS USING THE P4 DIFFERENCE
C ----------------------------------------------------------------
              DO 420 IARC=1,NARC
                IF (NOBA(IARC)-NBAD(IARC).GE.MINOBS) THEN
                  DL1=ARCOFF(IARC,1)
                  DL2=ARCOFF(IARC,2)
                  DL4=ARCOFF(IARC,4)
                  ARCOFF(IARC,1)=-DL1-2D0*DL4*FACLIN(3,2,SVN)
                  ARCOFF(IARC,2)=-DL2+2D0*DL4*FACLIN(3,1,SVN)
                ENDIF
420           CONTINUE
C
C UPDATE STATISTICS FOR GLONASS FREQUENCY ESTIMATION
C --------------------------------------------------
              IF (JTST.EQ.1) THEN
                KFRQ=JFRQ-FRQRNG(1)+1
                OBSOK(KFRQ)=NOBS
                DO IARC=1,NARC
                  OBSOK(KFRQ)=OBSOK(KFRQ)-NBAD(IARC)
                ENDDO
C
                IF (IPRFLG.EQ.1) THEN
                  WRITE(LFNPRT,21)SVN,NOBS,NOBS-OBSOK(KFRQ),
     1                            JFRQ,FRQNUM0,NARC
21                FORMAT(1X,'PRN:',I3,2X,'NOBS: ',I4,2X,'NBAD: ',I4,
     1                   2X,'FRQ: ',I2,2X,'FRQ0:',I2,2X,'NARC: ',I2)
                ENDIF
              ENDIF
C
C UPDATE OBSERVATION ARRAYS AND WRITE SATELLITE SUMMARY
C -----------------------------------------------------
              IF (JTST.EQ.2) THEN
                CALL UPDRNX(MAXREC,MAXCMB,MAXSAT,MAXARC,MAXSLP,IPRFLG,
     1                      IFLCOD,IFLPHS,DTSIM ,ISAT  ,SVN   ,NEPOCH,
     2                      OBSTIM,NEVT  ,TIMEVT,SIZEVT,SYSEVT,OBSREC,
     3                      OBSFLG,NOBS  ,LINTIM,LINFLG,NARC  ,ARCTIM,
     4                      ARCOFF,SATRMS,NSLIP ,NOBA  ,NBAD  ,SLPTIM,
     5                      SLPCYC)
              ENDIF
C
C END OF FREQUENCY ESTIMATION LOOPS
            ENDDO
          ENDDO
C
C NEXT SATELLITE TO CLEAN
C -----------------------
1000    CONTINUE
C
C WRITE RINEX FILE
C ----------------
        IF (NTOTO.LE.0) THEN
          WRITE(LFNERR,901)FILRNX(1,IFL)
901       FORMAT(/,' ### PG RNXSMT: NO VALID OBSERVATIONS',
     1                       /,16X,'FILE NAME: ',A32,/)
        ELSE
          CALL WRTRNX(MAXREC,MAXCMB,FILRNX(1,IFL),FILRNX(2,IFL),IFXSLP,
     1                IFLCOD,IFLPHS,NRSAT ,NUMSAT,NEPOCH,OBSTIM,OBSREC,
     2                OBSFLG,SMPINT,CODTYP,USEGEOS,GOBSDEF)
        ENDIF
C
C WRITE SUMMARY OF CLEANING
C -------------------------
        WRITE(LFNPRT,13)FILRNX(1,IFL)
13      FORMAT(1X,78('-'),/,1X,'PROCESSING SUMMARY OF FILE: ',A32,/,
     1         1X,78('-'))
C       WRITE(LFNPRT,14)NRSAT,NTOTO,NTOTB,NTSLP,FILRNX(1,IFL)
        WRITE(LFNPRT,14)NRSAT,NTOTO,NTOTB,NTARC,FILRNX(1,IFL)
14      FORMAT(2X,'#SAT: ',I2,1X,'#OBS: ',I5,1X,'#BAD: ',I5,
     1         1X,'#ARC: ',I4,1X,A32,/,1X,78('-'),/)
C
C NEXT RINEX FILE
C ---------------
10    CONTINUE
C
C COMBINATION OF P1-C1 AND P2-C2 DCB RESULTS
C ------------------------------------------
      staNam = ''
      IF(ICBEST>0) THEN
        DCBFIL=FILRNX(3,NFLINP)
        RECTYP=' '
        ICOMB=1
        CALL RNXDCB(MAXREC,MAXCMB,OBSREC,OBSAUX,NEPOCH,NRSAT,NUMSAT,
     1              OBSTIM,DCBFIL,staNam,ICBEST,RECTYP,ICOMB)
C
        DCBFIL=FILRNX(3,NFLINP)
        DCBFIL=DCBFIL(1:LEN_TRIM(DCBFIL)-3)//'AOA'
        RECTYP='AOA'
        ICOMB=1
        CALL RNXDCB(MAXREC,MAXCMB,OBSREC,OBSAUX,NEPOCH,NRSAT,NUMSAT,
     1              OBSTIM,DCBFIL,staNam,ICBEST,RECTYP,ICOMB)
C
        DCBFIL=FILRNX(3,NFLINP)
        DCBFIL=DCBFIL(1:LEN_TRIM(DCBFIL)-3)//'ASH'
        RECTYP='ASHTECH'
        ICOMB=1
        CALL RNXDCB(MAXREC,MAXCMB,OBSREC,OBSAUX,NEPOCH,NRSAT,NUMSAT,
     1              OBSTIM,DCBFIL,staNam,ICBEST,RECTYP,ICOMB)
C
        DCBFIL=FILRNX(3,NFLINP)
        DCBFIL=DCBFIL(1:LEN_TRIM(DCBFIL)-3)//'BLA'
        RECTYP='BLACKJACK'
        ICOMB=1
        CALL RNXDCB(MAXREC,MAXCMB,OBSREC,OBSAUX,NEPOCH,NRSAT,NUMSAT,
     1              OBSTIM,DCBFIL,staNam,ICBEST,RECTYP,ICOMB)
C
        DCBFIL=FILRNX(3,NFLINP)
        DCBFIL=DCBFIL(1:LEN_TRIM(DCBFIL)-3)//'JAV'
        RECTYP='JAVAD'
        ICOMB=1
        CALL RNXDCB(MAXREC,MAXCMB,OBSREC,OBSAUX,NEPOCH,NRSAT,NUMSAT,
     1              OBSTIM,DCBFIL,staNam,ICBEST,RECTYP,ICOMB)
C
        DCBFIL=FILRNX(3,NFLINP)
        DCBFIL=DCBFIL(1:LEN_TRIM(DCBFIL)-3)//'JPS'
        RECTYP='JPS'
        ICOMB=1
        CALL RNXDCB(MAXREC,MAXCMB,OBSREC,OBSAUX,NEPOCH,NRSAT,NUMSAT,
     1              OBSTIM,DCBFIL,staNam,ICBEST,RECTYP,ICOMB)
C
        DCBFIL=FILRNX(3,NFLINP)
        DCBFIL=DCBFIL(1:LEN_TRIM(DCBFIL)-3)//'LEI'
        RECTYP='LEICA'
        ICOMB=1
        CALL RNXDCB(MAXREC,MAXCMB,OBSREC,OBSAUX,NEPOCH,NRSAT,NUMSAT,
     1              OBSTIM,DCBFIL,staNam,ICBEST,RECTYP,ICOMB)
C
        DCBFIL=FILRNX(3,NFLINP)
        DCBFIL=DCBFIL(1:LEN_TRIM(DCBFIL)-3)//'NOV'
        RECTYP='NOV'
        ICOMB=1
        CALL RNXDCB(MAXREC,MAXCMB,OBSREC,OBSAUX,NEPOCH,NRSAT,NUMSAT,
     1              OBSTIM,DCBFIL,staNam,ICBEST,RECTYP,ICOMB)
C
        DCBFIL=FILRNX(3,NFLINP)
        DCBFIL=DCBFIL(1:LEN_TRIM(DCBFIL)-3)//'SEP'
        RECTYP='SEPT'
        ICOMB=1
        CALL RNXDCB(MAXREC,MAXCMB,OBSREC,OBSAUX,NEPOCH,NRSAT,NUMSAT,
     1              OBSTIM,DCBFIL,staNam,ICBEST,RECTYP,ICOMB)
C
        DCBFIL=FILRNX(3,NFLINP)
        DCBFIL=DCBFIL(1:LEN_TRIM(DCBFIL)-3)//'TPS'
        RECTYP='TPS'
        ICOMB=1
        CALL RNXDCB(MAXREC,MAXCMB,OBSREC,OBSAUX,NEPOCH,NRSAT,NUMSAT,
     1              OBSTIM,DCBFIL,staNam,ICBEST,RECTYP,ICOMB)
C
        DCBFIL=FILRNX(3,NFLINP)
        DCBFIL=DCBFIL(1:LEN_TRIM(DCBFIL)-3)//'TRI'
        RECTYP='TRIMBLE'
        ICOMB=1
        CALL RNXDCB(MAXREC,MAXCMB,OBSREC,OBSAUX,NEPOCH,NRSAT,NUMSAT,
     1              OBSTIM,DCBFIL,staNam,ICBEST,RECTYP,ICOMB)
C
        DCBFIL=FILRNX(3,NFLINP)
        DCBFIL=DCBFIL(1:LEN_TRIM(DCBFIL)-3)//'TR5'
        RECTYP='TRIMBLE NETR5'
        ICOMB=1
        CALL RNXDCB(MAXREC,MAXCMB,OBSREC,OBSAUX,NEPOCH,NRSAT,NUMSAT,
     1              OBSTIM,DCBFIL,staNam,ICBEST,RECTYP,ICOMB)
C
        DCBFIL=FILRNX(3,NFLINP)
        DCBFIL=DCBFIL(1:LEN_TRIM(DCBFIL)-3)//'TR8'
        RECTYP='TRIMBLE NETR8'
        ICOMB=1
        CALL RNXDCB(MAXREC,MAXCMB,OBSREC,OBSAUX,NEPOCH,NRSAT,NUMSAT,
     1              OBSTIM,DCBFIL,staNam,ICBEST,RECTYP,ICOMB)
C
        DCBFIL=FILRNX(3,NFLINP)
        DCBFIL=DCBFIL(1:LEN_TRIM(DCBFIL)-3)//'TR9'
        RECTYP='TRIMBLE NETR9'
        ICOMB=1
        CALL RNXDCB(MAXREC,MAXCMB,OBSREC,OBSAUX,NEPOCH,NRSAT,NUMSAT,
     1              OBSTIM,DCBFIL,staNam,ICBEST,RECTYP,ICOMB)
C
        DCBFIL=FILRNX(3,NFLINP)
        DCBFIL=DCBFIL(1:LEN_TRIM(DCBFIL)-3)//'TRS'
        RECTYP='TRIMBLE NETRS'
        ICOMB=1
        CALL RNXDCB(MAXREC,MAXCMB,OBSREC,OBSAUX,NEPOCH,NRSAT,NUMSAT,
     1              OBSTIM,DCBFIL,staNam,ICBEST,RECTYP,ICOMB)
      ENDIF
C
      CALL EXITRC(0)
C
      END
