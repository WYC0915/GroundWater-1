C*
      PROGRAM MAUPRP
CC
CC NAME       :  MAUPRP
CC
CC PURPOSE    :  PREPROCESSING PROGRAM FOR SINGLE DIFFERENCE FILES
CC               FOREGROUND AND BACKGROUND VERSION
CC
CC               OUTLINE OF PROCEDURE:
CC                - SINGLE DIFFERENCES ARE SCREENED USING A
CC                  SIMPLE NON-PARAMETRIC METHOD
CC                - DOUBLE DIFFERENCES ARE SCREENED USING A
CC                  SIMPLE NON-PARAMETRIC METHOD
CC                - WITH THESE SCREENED OBSERVATIONS A TRIPLE
CC                  DIFFERENCE SOLUTION IS MADE.
CC                - THE TRIPLE DIFFERENCE RESIDUALS ARE USED TO
CC                  DETECT AND ELIMINATE CYCLE SLIPS.
CC               REMARKS:
CC                - THE PROBLEM IS SOLVED IN BLOCKS OF MAXEPO
CC                  NUMBER OF EPOCHS. (MAXEPO IS OF THE ORDER
CC                  OF 100-1000).
CC                - THE SINGLE DIFFERENCE OBSERVATION EQUATIONS
CC                  ARE WRITTEN TO A DISK FILE FOR COMPUTATION OF
CC                  OF RESIDUALS.
CC                - SINGLE OR DUAL FREQUENCY OBSERVATIONS MAY BE
CC                  PROCESSED.
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER, G.BEUTLER, L.MERVART
CC
CC VERSION    :  3.5  (AUG 93)
CC
CC CREATED    :  88/04/20
CC
CC CHANGES    :  05-JUN-92 : ??: CHANGES FOR THE NEW MAUPRP VERSION
CC               13-JUL-92 : ??: COMMON FOR "MAXFRQ" DUE TO RESIDUAL FILE
CC               22-DEC-92 : ??: USE OF SR "OPNFIL" TO OPEN FILES
CC               03-MAR-93 : ??: ELEVATION DEPENDENT PHASE ECCENTRICITIES
CC                               NEW PARAMETERS "ICORR","ZEN" IN GPHECC
CC                               NEW PARAMETERS "RECTYP","ANTTYP","IANTEN"
CC                               IN PREPHA
CC               15-MAR-93 : ??: MAXSTA INCREASED FROM 50 TO 70
CC               19-MAR-93 : ??: MAXEPO REDUCED TO 300, ELSE MANY OBSERV.
CC                               LOST DUE TO REFERENCE SATELLITE PER BLOCK
CC               09-AUG-93 : ??: VERSION 3.5, NEW FORMAT
CC               28-OCT-93 : ??: MARK SATELLITES ACCORDING TO SATCRUX-FILE
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               08-FEB-94 : MR: CHECK MAXIMUM NUMBER OF AMBIGUITIES
CC               08-APR-94 : MR: ADD PARAM. "AZI" TO CALL OF GPHECC
CC               10-AUG-94 : MR: CALL EXITRC
CC               14-AUG-94 : MR: FORMAT 4: SESSION AS CHARACTER*4
CC               17-AUG-94 : MR: MAXBAD=200 (OLD: MAXBAD=20)
CC               08-DEC-94 : RW: INCLUDE INPUT/OUTPUT FILE TABLE
CC               08-DEC-94 : RW: WRITE OBS-FILE NAME TO OUTPUT
CC               25-APR-95 : MR: ADD OPTION FOR MAXIMUM O-C
CC               30-AUG-95 : MR: ADJUST MAX. INTERVAL "MXINTR"
CC               17-SEP-95 : JJ: INCREASE MAXFIL TO 200
CC                               INCREASE MAXSTA TO 200
CC                               INCREASE MAXDEL TO 20000
CC               05-OCT-95 : MR: TRACK AND PRINT PROBLEMS, REMOVE
CC                               DELETION OF FILES
CC               30-OCT-95 : MR: WARNING MESSAGE IF FILE NOT SAVED
CC               05-MAR-96 : TS: HANDLING OF "MAXAMS" EXCEEDINGS
CC               26-MAR-96 : MR: ADD "CSESS" TO CALL GPHECC, PREPHA
CC               06-MAY-96 : TS: REMOVED OLD SAT.CLOCK STUFF
CC               23-AUG-96 : TS: "MAXBAD" IN INCLUDED FILE
CC               17-JAN-97 : MR: MAXMXI=500 (OLD: 100)
CC               24-SEP-97 : DI: USE MAXSAT.inc
CC               04-MAY-98 : SS: SET GLONASS FREQUENCIES
CC               14-OCT-98 : HH: MODIFICATIONS FOR GLONASS
CC               18-NOV-98 : MR: MAXAMS=500 (OLD: 50)
CC               23-DEC-98 : MR: REMOVE OBSERVATIONS, IF MAXAMB EXCEEDED
CC               04-AUG-99 : PF: ADD "TIMCRD" TO CALL COORUP
CC               07-MAR-01 : LM: INCLUDE MAXAMB
CC               16-JUL-01 : HU: MAXIMUM NUMBER OF AMBIGUITIES FROM I-FILE
CC               13-AUG-01 : DS: SR:STAFLG - HANDLE STATION TYPES
CC               13-AUG-01 : DS: HANDLE LEO
CC               05-SEP-01 : HU: INTERFACE FOR PRFLNA ADDED
CC               23-SEP-01 : DI: VERSION 4.3 AND 5.0 MERGED FOR NEW MENU
CC               25-SEP-01 : DI: WARNING IF MXAMBS GT MAXAMB
CC               27-SEP-01 : DI: IMPROVED OUTPUT (NEW SR PROUTP)
CC               02-JAN-02 : DS/MR: ZERO-DIFFERENCE PRE-PROCESSING
CC               27-Aug-02 : RD: Handle new formatted residual files
CC               18-SEP-02 : DS: KINEMATICS FOR:LEO,AIRPLANE,
CC                               SHIP,GROUND,STATIC GROUND
CC               25-SEP-02 : HU: REMOVE I_ASTLIB
CC               04-DEC-02 : RD: ADD CLOCK EVENT FLAG AND LIST
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               02-APR-03 : RD: INCREASE MAXCLK 100->500
CC               23-APR-03 : HU: NULLIFY LOCAL POINTERS
CC               16-MAY-03 : HB: INITIALIZE STRUCTURE
CC               22-MAY-03 : RD: COMPARE NUMAMB IN SNG-DIFF WITH MAXAMB(GPSEST)
CC               28-MAY-03 : RD: NEW CALL OF SR GPHECC
CC               23-JUN-03 : HB: INTERFACE FOR SR STAFLG
CC               11-AUG-03 : RS: RECTYP=' ' IN CALL OF GPHECC
CC               08-SEP-03 : HU: ANTNAM, RECNAM, OPRNAM CHR16 -> CHR20
CC               13-SEP-03 : HU: INTERFACE FOR DEFREQ
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               02-FEB-04 : RD: PREVENT I*4 OVERFLOW FOR 1-SEC DATA
CC               28-JUN-04 : RD: USE MAXSTA FROM M_MAXDIM
CC               03-JUN-05 : HU/RD: CHECK MAXAMB
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: LFNUM.inc, COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-JUN-05 : MM: UNUSED VARIABLES REMOVED
CC               27-FEB-07 : AG: CALL DEFCON WITH PARAMETER
CC               12-JUN-07 : AG: USE STA_OFF INSTEAD OF GPHECC
CC               08-SEP-07 : RD: ADVANCED MXIOND (DEP. FROM BSL-LENGTH)
CC               08-SEP-07 : RD: AUTOMATIC SWITCH BETWEEN COMBINED/BOTH
CC               01-NOV-07 : HB: ADD PARAMETER SECIPL FOR GTSCLK
CC               26-FEB-08 : RD: USE GTSATB FROM D_SATCRX
CC               04-MAY-09 : RD: INIT VIENNA GRID FILES FOR VMF1
CC               25-MAY-10 : MF: CALL SR init_filhead
CC               02-JUL-10 : RD: NCLKEV MAY BE EXCEED MAXCLK
CC               12-AUG-10 : DT: FORMAT STATEMENTS CORRECTED
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               25-OCT-10 : SL: REAL TO INTEGER CONVERSION BUG CORRECTED
CC               27-OCT-10 : SL: USE M_BERN WITH ONLY, REMOVAL OF UNUSED MOD
CC               26-JAN-11 : LP: Sat.-specific obstypes
CC               08-FEB-11 : RD: GETSTA IS USED AS A MODULE NOW
CC               17-FEB-11 : RD: COMMON MCMSTA NOT NEEDED ANYMORE
CC               30-NOV-11 : SL: NEW TITLE STRING FOR PRITIT
CC               31-JAN-12 : RD: "AUTO" FOR ZERO-DIFFERENCE FILES
CC               05-MAR-12 : RD: USE WTHEAD AS MODULE NOW
CC               27-MAR-12 : RD: USE PROMP1 AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnErr, lfn001, lfn002, lfnRes, lfnPrt,
     1                    lfnKbd, lfnRp1, lfnOrb, fileNameLength
      USE m_cpu,    ONLY: cpu_start
      USE m_maxdim, ONLY: maxsat, maxamb, maxbad, maxsta, maxsaa
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE d_phaecc, ONLY: init_buf
      USE p_gpsest, ONLY: maxamb2 => maxamb
      USE d_resfil, ONLY: t_reshead, reshed_MAUPRP0, reshed_MAUPRP1,
     1                    init_resHead, init_filhead
      USE d_const,  ONLY: DATE, TIME
      USE d_satcrx, ONLY: gtsatb
      USE d_grid,   ONLY: initGridBuffer,prtGridInfo
      USE d_rinex3, ONLY: t_gobsdef
C
      USE s_prtopt
      USE s_proutp
      USE s_prepha
      USE s_iordup
      USE s_rdobsi
      USE s_alcerr
      USE s_opnfil
      USE s_prflna
      USE s_dspamb
      USE s_mrkobs
      USE s_wtobsi
      USE s_pritit
      USE s_inmapr
      USE s_readinpf
      USE s_opnerr
      USE s_prfile
      USE s_staflg
      USE s_setflg
      USE s_promp1
      USE s_cycmrk
      USE s_rdhead
      USE s_getsta
      USE s_defreq
      USE s_prinpt
      USE s_exitrc
      USE s_defcon
      USE s_slfile
      USE s_hedin2
      USE s_ellecc
      USE s_opnsys
      USE s_jmt
      USE s_wthead
      USE s_upperc
      USE s_wtresh2
      USE s_gtflna
      USE s_xyzell
      USE s_coorup
      USE s_cdscor
      USE f_lengt0
      USE f_djul
      USE f_tstflg

      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IAMB  , IAMS  , IAMSAT, IAUTO , IBAD  , ICH1  ,
     1          ICH2  , IDAY  , IDELTT, IEP   , IEPLST, IEPO  ,
     2          IEPOC1, IEPOC2, IEPOCH, IEXTRA, IFIL  , IFLAG , IFREQ ,
     3          IFRMAT, IFRQ  , IIONO , ILEOS , IOSTAT, IPRNT1, IPRNT2,
     4          IRC   , IRCCRD, IRCRES, IREFIL, IRESID, IRETC ,
     5          IRETRN, IRJECT, IRMARK, ISAT  , ISATEL, ISATEP, ISAVE ,
     6          ISETOP, IST1  , IST2  , ISTA  , ISTFIL, ITROPO, IUSFLG,
     7          IYEAR , IZEROD, KINSTA, L     , L5CLEA, LFIL  ,
     8          LFNHLP, LFNOBS, LTRIP , LTRIP0, MAXAMS, MAXARC, MAXCLK,
     9          MAXDEL, MAXEPO, MAXFIL, MAXFRQ, MAXMXI, MAXSLP, MAXZEN,
     1          MEATYP, MINCYC, MNCONT, MONTH , MRK1O2, MXAMBS, MXCAMB,
     2          MXCAMS, MXCARC, MXCCLK, MXCDEL, MXCEPO, MXCFIL, MXCFRQ,
     3          MXCMXI, MXCSAT, MXCSLP, MXHOLE, MXINTF, MXINTR,
     4          MXIONU, MXOGAP, MXZLEO, NBAD  , NCENTR, NCLKEV, NCOL  ,
     5          NDEL  , NDIFF , NEPFLG, NEPOCH, NEWAMB, NFIL  , NFIL1 ,
     6          NFIL2 , NFRCH0, NFRCHK, NFREQ , NFRQ  , NMAXI , NOBINT,
     7          NOPRT , NSATEL, NSATEP, NSLIP , NSTAT , NTONLY, NUMAMB,
     8          USEGEOS
C
      REAL*8    AELL  , BELL  , BLGTH , DAY   , OBSTIM, MINLEN,
     1          OMCMAX, RMSMAX, SCELL , SECIPL, TIMCRD, TIMEND, TIMREF,
     2          TOLJMP
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
C MAXIMUM DIMENSIONS
C ------------------
      PARAMETER(MAXEPO=300,MAXAMS=  500,
     1          MAXFIL=  200,MAXARC= 20,MAXSLP=20000,
     2          MAXDEL=20000,MAXCLK=500,MAXMXI=500,MAXFRQ=  2)
C
C
C MAXSAT : MAXIMUM NUMBER OF SATELLITES (PER FILE)
C MAXEPO : MAXIMUM NUMBER OF EPOCHS
C MAXAMB : MAXIMUM NUMBER OF AMBIGUITIES
C MAXAMS : MAXIMUM NUMBER OF AMBIGUITIES PER SATELLITE
C MAXARC : MAXIMUM NUMBER OF SATELLITE ARCS
C MAXSTA : MAXIMUM NUMBER OF STATIONS
C MAXSLP : MAXIMUM NUMBER OF CYCLE SLIPS
C MAXDEL : MAXIMUM NUMBER OF MARK REQUESTS
C MAXCLK : MAXIMUM NUMBER OF CLOCK EVENTS
C MAXMXI : MAXIMUM NUMBER OF REGISTRATED "MAX. INTERVAL EXCEEDED"
C MAXFIL : MAXIMUM NUMBER OF FILES
C MAXFRQ : MAXIMUM NUMBER OF FREQUENCIES FOR RESIDUAL FILE
C MAXBAD : MAXIMUM NUMBER OF BAD SATELLITES (SATCRUX-FILE)
C
C DECLARATIONS
C ------------
      TYPE(t_resHead) reshed
C
      TYPE(t_gobsdef) :: gobsdef ! Giove External Obs. Selection info
C
      CHARACTER*1  TRPFLG(MAXFIL),AMSFLG,CLKFLG,OBST
      CHARACTER*1  OBSFLG(MAXSAT,2),EPOFLG,CHGOPT,SAVE
      CHARACTER*4  CSESS(2),SESFIL(2,MAXFIL),PROBLM
      CHARACTER*5  CRTIME(2)
      CHARACTER*6  MXNSAT,MXNEPO,MXNAMB,MXNAMS,MXNARC
      CHARACTER*6  MXNSLP,MXNDEL,MXNMXI,MXNFIL,MXNFRQ,MXNCLK
      CHARACTER*7  INTNAM
      CHARACTER*8  FILSTR
      CHARACTER*9  CRDATE(2)
      CHARACTER*16 CAMPGN,DATUM,STANAM(2)
      CHARACTER*16 STAFIL(2,MAXFIL),STNAME(MAXSTA),STAFIX
      CHARACTER*20 RECTYP(2),OPRNAM(2),ANTTYP(2)
      CHARACTER*20 MARTYP
      CHARACTER*32 FILHED(MAXFIL),FILRES,FILCRX, FILCRD
      CHARACTER*53 TITLE
      CHARACTER*80 TITRES,STR(2)
      CHARACTER(LEN=fileNameLength), DIMENSION(:,:), POINTER  :: FILNAM

C
      REAL*8       POSECC(3,2),AMBIGU(MAXAMB,3)
      REAL*8       OBSERV(MAXSAT,2),TIMFIL(MAXFIL),DELTAT(2)
      REAL*8       XSTAT(3,MAXSTA),XSTELL(3,MAXSTA),XSTECC(3,MAXSTA)
      REAL*8       DXELL(3),DRELL(3),XWGS(3,2),XELL(3,2)
      REAL*8       DISCLV(2),SLPXXX(2,MAXSLP),IONO(3,MAXSLP),SIGL12(2)
      REAL*8       SIGWGS(3),SLPLST(MAXSLP)
      REAL*8       EXC(3),DXFIL(3,MAXFIL),TIMBAD(2,MAXBAD)
      REAL*8       EPOFRQ(2),XSTANW(3,MAXFIL),TIMDEL(2)
C
      INTEGER*4    QMAX,Q(2),IRUNIT(2),IANTEN(2),ICLOCK(2)
      INTEGER*4    NUMSAT(MAXSAT),NUMOBS(MAXSAT,2),L12(2)
      INTEGER*4    NUMMRK(MAXSAT,2),AMBIEP(MAXAMB)
      INTEGER*4    AMBSAT(MAXAMB),AMBWLF(MAXAMB,2),AMBCLS(MAXAMB,3)
      INTEGER*4    LSTSLP(6,MAXSLP),LSTDEL(5,MAXDEL),LSTMXI(5,MAXMXI)
      INTEGER*4    LSTCLK(4,MAXCLK),JMPOPT(6)
      INTEGER*4    STANUM(MAXSTA),ICENTR(MAXSTA)
      INTEGER*4    SVNEP(MAXSAT),IWLSCR(2),IAMNEW(5),IPPROC(2)
      INTEGER*4    STFIL(2,MAXFIL),SWIDTH(2)
      INTEGER*4    NFRRES(MAXFIL),FRQRES(MAXFRQ,MAXFIL)
      INTEGER*4    NFRFIL(MAXFIL),NEPFIL(MAXFIL),NSAFIL(MAXFIL)
      INTEGER*4    SVNFIL(MAXSAT,MAXFIL),MEAFIL(MAXFIL),IDTFIL(MAXFIL)
      INTEGER*4    LSTAMB(3,MAXSAT,MAXAMS),NNEWAM(MAXSAT)
      INTEGER*4    LASTCS(MAXSAT),DELAMB(MAXAMS)
      INTEGER*4    IOBBAD(MAXBAD),IACBAD(MAXBAD),SATBAD(MAXBAD)
      INTEGER*4    INDEXS(MAXSAT),MXIOND(3)
C
C COMMON BLOCKS
C -------------
      COMMON/LARGE1/STAFIL,STNAME,FILHED,FILNAM
      COMMON/LARGE2/AMBIGU,OBSERV,TIMFIL,XSTAT,XSTELL,XSTECC,SLPXXX,
     1              IONO,SLPLST,DXFIL
      COMMON/LARGE3/LSTDEL,LSTSLP,LSTMXI,STFIL,NFRRES,FRQRES,NFRFIL,
     1              NEPFIL,NSAFIL,SESFIL,SVNFIL,MEAFIL,IDTFIL,
     2              AMBSAT,AMBIEP,AMBCLS
C
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMEPO/MXCEPO,MXNEPO
      COMMON/MCMAMB/MXCAMB,MXNAMB
      COMMON/MCMAMS/MXCAMS,MXNAMS
      COMMON/MCMARC/MXCARC,MXNARC
      COMMON/MCMSLP/MXCSLP,MXNSLP
      COMMON/MCMDEL/MXCDEL,MXNDEL
      COMMON/MCMCLK/MXCCLK,MXNCLK
      COMMON/MCMMXI/MXCMXI,MXNMXI
      COMMON/MCMFIL/MXCFIL,MXNFIL
      COMMON/MCMFRQ/MXCFRQ,MXNFRQ
      DATA L12/1,2/
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C INITIALIZE COMMON BLOCKS FOR MAXIMAL DIMENSIONS
C -----------------------------------------------
      MXCSAT=MAXSAT
      MXNSAT='MAXSAT'
      MXCEPO=MAXEPO
      MXNEPO='MAXEPO'
      MXCAMB=MAXAMB
      MXNAMB='MAXAMB'
      MXCAMS=MAXAMS
      MXNAMS='MAXAMS'
      MXCARC=MAXARC
      MXNARC='MAXARC'
      MXCSLP=MAXSLP
      MXNSLP='MAXSLP'
      MXCDEL=MAXDEL
      MXNDEL='MAXDEL'
      MXCCLK=MAXCLK
      MXNCLK='MAXCLK'
      MXCMXI=MAXMXI
      MXNMXI='MAXMXI'
      MXCFIL=MAXFIL
      MXNFIL='MAXFIL'
      MXCFRQ=MAXFRQ
      MXNFRQ='MAXFRQ'
C
      LFNOBS=LFN001+2
      LFNHLP=LFNRES
C
C NULLIFY POINTERS
C ----------------
      NULLIFY(FILNAM)
      CALL INIT_RESHEAD(resHed)
      CALL INIT_INPKEY(inpKey)
      STR(1)=' '
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
C READ OPTION INPUT FILE
C ----------------------
      CALL PRINPT(IAUTO ,ISETOP,NFRCH0,MINLEN,ISAVE ,IIONO ,IUSFLG,
     1            MRK1O2,MAXZEN,SECIPL,MXHOLE,MNCONT,IPRNT1,MXINTR,
     2            IPPROC,Q     ,DISCLV,LTRIP0,SIGWGS,STAFIX,IPRNT2,
     3            SIGL12,IWLSCR,SWIDTH,MINCYC,IRJECT,MXOGAP,MXIOND,
     4            IAMNEW,NTONLY,L5CLEA,OMCMAX,MXAMBS,KINSTA,MXZLEO,
     5            ITROPO,IEXTRA,IZEROD,ILEOS ,RMSMAX,JMPOPT,TOLJMP,
     6            NFIL  ,FILNAM)
C
      IF (MXAMBS.GT.MAXAMB) THEN
        WRITE(LFNERR,905) MXAMBS,MAXAMB
905     FORMAT(/,' ### PG MAUPRP: MXAMBS GREATER THAN MAXAMB',/,
     1             16X,'MXAMBS: ',I5,/,
     2             16X,'MAXAMB: ',I5,/,
     3             16X,'MAXIMUM NUMBER OF AMBIGUITIES PER BASELINE',/,
     4             16X,'(MXAMBS) SET TO MAXAMB',/)
        MXAMBS=MAXAMB
      ENDIF
C
C PRINT GENERAL TITLE
C -------------------
      IF (IZEROD.EQ.1) THEN
        CALL pritit('MAUPRP',
     1            'Zero-difference phase preprocessing')
      ELSE
        CALL pritit('MAUPRP',
     1            'Single-difference phase preprocessing')
      END IF
C
C PRINT GENERAL FILE NAMES
C ------------------------
      NOPRT=1
      INTNAM='SYSERR '
      NCOL=79
C
      CALL PRFLNA(NCOL)
C
      IF (IZEROD.EQ.1) THEN
        CALL PRFILE('PZFILES','',2)
      ELSE
        CALL PRFILE('OBSFIL','',2)
      END IF
C
C PRINT INPUT OPTIONS TO OUTPUT FILE
C ----------------------------------
      CALL PROUTP(IAUTO ,ISETOP,NFRCH0,MINLEN,ISAVE ,IIONO ,IUSFLG,
     1            MRK1O2,MAXZEN,MXHOLE,MNCONT,IPRNT1,MXINTR,IPPROC,
     2            Q     ,DISCLV,LTRIP0,SIGWGS,STAFIX,IPRNT2,SIGL12,
     3            IWLSCR,SWIDTH,MINCYC,IRJECT,MXOGAP,MXIOND,IAMNEW,
     4            NTONLY,L5CLEA,OMCMAX,MXAMBS,IZEROD,ILEOS ,ITROPO,
     5            IEXTRA,KINSTA,MXZLEO,RMSMAX,JMPOPT,TOLJMP)
C
C SAVE RESIDUALS ?
C ----------------
      CALL GTFLNA(0,'RESIDRS',FILRES,IRCRES)
      IF (IRCRES.EQ.0) THEN
        IRESID=1
      ELSE
        IRESID=0
      ENDIF
C
C GET HEADER AND OBS. FILE NAME
C -----------------------------
      DO 25 IFIL=1,NFIL
        TRPFLG(IFIL)='B'
        FILHED(IFIL)=FILNAM(1,IFIL)
25    CONTINUE
C
C READ ALL HEADERS TO GET HEADER INFO FOR FILE SELECTION LIST
C -----------------------------------------------------------
      CALL HEDIN2(NFIL,FILHED,MEAFIL,NFRFIL,NEPFIL,NSAFIL,SESFIL,
     1            IDTFIL,TIMFIL,STAFIL,SVNFIL,
     2            TITLE,NSTAT,STNAME,STFIL,NUMOBS,NUMMRK,
     3            NUMAMB,AMBSAT,AMBIEP,AMBWLF,AMBIGU,AMBCLS)
C
C GET STATION COORDINATES
C -----------------------
      CALL GETSTA(NSTAT,STNAME,STANUM,NCENTR,ICENTR,
     1            XSTAT,XSTELL,XSTECC,
     2            DATUM,AELL,BELL,DXELL,DRELL,SCELL)
C
C TITLE FOR OUTPUT FILES
C ----------------------
      TITRES=TITLE
      TITRES(66:74)=DATE
      TITRES(76:80)=TIME
C
C GET BAD SATELLITE INTERVALS FROM SATELLITE PROBLEM FILE
C -------------------------------------------------------
      CALL GTFLNA(0,'SATCRUX',FILCRX,IRC)
      CALL GTSATB(MAXBAD,FILCRX,NBAD,SATBAD,IOBBAD,IACBAD,TIMBAD)
C
C FILE SELECTION
C --------------
2000  IF(IAUTO.EQ.0) THEN
        CALL SLFILE(NFIL,STAFIL,SESFIL,NFRFIL,IDTFIL,
     1              NSAFIL,NEPFIL,TIMFIL,IFIL)
        IF(IFIL.EQ.0) GOTO 999
        NFIL1=IFIL
        NFIL2=IFIL
      ELSE
        NFIL1=1
        NFIL2=NFIL
C
C PRINT OPTIONS (AUTOMATICAL MODE ONLY)
C
        CALL PRTOPT
C
C WRITE HEADER OF RESIDUAL FILE
        IF(IRESID.EQ.1) THEN
C
C INIT RESIDUAL HEADER RECORD
C ---------------------------
          RESHED%TITLE=TITRES
          IF (IZEROD.EQ.1) THEN
            RESHED%DSC  = resHed_MAUPRP0
          ELSE
            RESHED%DSC  = resHed_MAUPRP1
          ENDIF
          RESHED%NFIL = NFIL
          ALLOCATE(RESHED%FILHEAD(NFIL),STAT=irc)
          CALL ALCERR(IRC,'RESHED%FILHEAD',(/NFIL/),'MAUPRP')
          DO IFIL=1,NFIL
            CALL init_filhead(RESHED%FILHEAD(IFIL))
            RESHED%FILHEAD(IFIL)%MEATYP = MEAFIL(IFIL)
            IF(NFRCH0.LT.3) THEN
              RESHED%DSC%ITYP = 1
              RESHED%FILHEAD(IFIL)%NFRFIL = 1
              RESHED%FILHEAD(IFIL)%ICARR(1) = NFRCH0
            ELSE
              RESHED%DSC%ITYP = 2
              RESHED%FILHEAD(IFIL)%NFRFIL = 2
              RESHED%FILHEAD(IFIL)%ICARR(1:2) = (/ 1,2 /)
            ENDIF
            RESHED%FILHEAD(IFIL)%STANAM(1) = STNAME(STFIL(1,IFIL))
            IF (IZEROD.EQ.1) THEN
              RESHED%FILHEAD(IFIL)%STANAM(2) = ' '
            ELSE
              RESHED%FILHEAD(IFIL)%STANAM(2) = STNAME(STFIL(2,IFIL))
            ENDIF
            RESHED%FILHEAD(IFIL)%CSESS(:) = SESFIL(:,IFIL)
            RESHED%FILHEAD(IFIL)%IDELTT = IDTFIL(IFIL)
            RESHED%FILHEAD(IFIL)%TIMREF = TIMFIL(IFIL)
            RESHED%FILHEAD(IFIL)%NSATEL = NSAFIL(IFIL)
            ALLOCATE(RESHED%FILHEAD(IFIL)%NUMSAT(NSAFIL(IFIL)),STAT=IRC)
            CALL ALCERR(IRC,'RESHED%FILHEAD(IFIL)%NUMSAT',
     1               (/NSAFIL(IFIL)/),'MAUPRP')
            RESHED%FILHEAD(IFIL)%NUMSAT(:) = SVNFIL(1:NSAFIL(IFIL),IFIL)
          ENDDO
C
C WRITE THE RESIDUAL FILE HEADER
C ------------------------------
          CALL WTRESH2(LFNHLP,RESHED)
C
          DO IFIL=1,NFIL
            DEALLOCATE(RESHED%FILHEAD(IFIL)%NUMSAT,STAT=IRC)
          ENDDO
          DEALLOCATE(RESHED%FILHEAD,STAT=irc)
        ENDIF
      ENDIF
C
C Initialize antenna buffer
C
      CALL init_buf(bufsize=(/2,maxsaa/))
C
C INIT THE BUFFER FOR VIENNA GRID FILES
C -------------------------------------
      CALL initGridBuffer(bufferSize=2)
      CALL prtGridInfo
C
C LOOP OVER ALL FILES (ONE FILE FOR MANUAL PREPROC.)
C --------------------------------------------------
      TIMCRD=0D0
C
      DO 1500 IFIL=NFIL1,NFIL2
        PROBLM='OK  '
        AMSFLG='G'
C
C OPEN OBS-FILE
C -------------
        CALL OPNFIL(LFNOBS,FILNAM(2,IFIL),'OLD','UNFORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNOBS,IOSTAT,FILNAM(2,IFIL),'MAUPRP')
C
C READ HEADER
C -----------
        USEGEOS=0
        GOBSDEF%NOREC=0
        CALL RDHEAD(FILNAM(1,IFIL),
     1              MEATYP,NDIFF,NFREQ,NEPOCH,NSATEL,
     2              CSESS,IDELTT,TIMREF,CAMPGN,TITLE,CRDATE,
     3              CRTIME,IRMARK,NEPFLG,IFRMAT,
     4              STANAM,RECTYP,ANTTYP,IRUNIT,IANTEN,
     5              OPRNAM,POSECC,ICLOCK,NUMSAT,NUMOBS,NUMMRK,
     6              NUMAMB,AMBSAT,AMBIEP,AMBWLF,AMBIGU,AMBCLS,
     7              1,USEGEOS=USEGEOS,GOBSDEF=GOBSDEF)
C
C INIT THE 2ND STRINGS FOR ZD FILES
C ---------------------------------
        IF (NDIFF.EQ.0) THEN
          STANAM(2)=' '
          RECTYP(2)=' '
          ANTTYP(2)=' '
          OPRNAM(2)=' '
        ENDIF
C
        TIMEND=TIMREF+(NEPOCH-1)*IDELTT/86400.D0
C
C COMPUTE THE MEAN EPOCH FOR STORING THE COORDINATES
C --------------------------------------------------
        TIMCRD=TIMCRD+(TIMREF+TIMEND)/2D0/DBLE(NFIL)
C
C SET GPS AND GLONASS FREQUENCIES
C -------------------------------
        EPOFRQ(1)=TIMREF
        EPOFRQ(2)=TIMEND
        OBST='P'
        CALL DEFREQ(EPOFRQ,NSATEL,NUMSAT,
     1              USEGEOS=USEGEOS,GOBSDEF=GOBSDEF,MEATYPC=OBST)
C
C EPOCH FOR COORDINATES
C ---------------------
        TIMCRD=(TIMREF+TIMEND)/2.D0
C
C INITIALIZE THE LIST OF AMBIGUITIES
C ----------------------------------
        IF (IAMNEW(4) .EQ. 1) THEN
          DO 600 ISATEL = 1, NSATEL
            NNEWAM(ISATEL)=0
            DO 650 IAMB=1,NUMAMB
              IF (AMBSAT(IAMB).EQ.NUMSAT(ISATEL)) THEN
                NNEWAM(ISATEL)=NNEWAM(ISATEL)+1
                LSTAMB(1,ISATEL,NNEWAM(ISATEL)) = AMBIEP(IAMB)
                LSTAMB(2,ISATEL,NNEWAM(ISATEL)) = 1
                LSTAMB(3,ISATEL,NNEWAM(ISATEL)) = AMBIEP(IAMB)
              END IF
650         CONTINUE
            IF (NNEWAM(ISATEL).LE.MAXAMS) THEN
              DO 660 IAMS=NNEWAM(ISATEL)+1,MAXAMS
                LSTAMB(1,ISATEL,IAMS) = 0
                LSTAMB(2,ISATEL,IAMS) = 0
                LSTAMB(3,ISATEL,IAMS) = 0
660           CONTINUE
            ELSE
              WRITE(LFNERR,900) NUMSAT(ISATEL),NNEWAM(ISATEL),
     1                          MAXAMS,IFIL,FILNAM(1,IFIL)
900           FORMAT(/,' *** PG MAUPRP: TOO MANY AMB. PER SATELLITE',/,
     1                         16X,'SATELLITE  :',I4,/,
     2                         16X,'# AMBIG.   :',I4,/,
     3                         16X,'MAX. NUMBER:',I4,/,
     4                         16X,'FILE NUMBER:',I4,/,
     5                         16X,'FILE NAME  : ',A32,/)
              PROBLM='MXAM'
              GOTO 1499
            END IF
600       CONTINUE
        ELSE
          DO 680 ISATEL = 1,NSATEL
            NNEWAM(ISATEL) = 1
            LSTAMB(1,ISATEL,1) = 1
            LSTAMB(2,ISATEL,1) = 1
            LSTAMB(3,ISATEL,1) = 1
            DO 670 IAMS = 2,MAXAMS
              LSTAMB(1,ISATEL,IAMS) = 0
              LSTAMB(2,ISATEL,IAMS) = 0
              LSTAMB(3,ISATEL,IAMS) = 0
670         CONTINUE
680       CONTINUE
        END IF
C
C CHECK WHETHER A PHASE ZERO/SINGLE DIFFERENCE FILE IS PROCESSED
C ---------------------------------------------------------
        IF(MEATYP.NE.1) THEN
          WRITE(LFNERR,902) IFIL,FILNAM(1,IFIL)
902       FORMAT(/,' *** PG MAUPRP: FILE IS NOT A PHASE FILE',/,
     1                         16X,'FILE NUMBER:',I4,/,
     2                         16X,'FILE NAME  : ',A32,/)
          PROBLM='NOPH'
          GOTO 1499
        ENDIF
C
C APPROXIMATE BASELINE LENGTH
C ---------------------------
        BLGTH=0.D0
        IF (NDIFF.NE.0) THEN
          IST1=STFIL(1,IFIL)
          IST2=STFIL(2,IFIL)
          DO 35 I=1,3
            BLGTH=BLGTH+(XSTAT(I,IST1)-XSTAT(I,IST2))**2
35        CONTINUE
          BLGTH=DSQRT(BLGTH)
        ENDIF
C
C YEAR, DAY OF THE YEAR
        CALL JMT(TIMREF,IYEAR,MONTH,DAY)
        IDAY=NINT(TIMREF-DJUL(IYEAR,1,1.D0)+1.D0)
C
C WRITE TITLE FOR PROTOCOL
C ------------------------
        IF (NDIFF.EQ.0) THEN
          WRITE(LFNPRT,36) CAMPGN,DATE,TIME,STANAM(1),IYEAR,CSESS(1),
     1                     IDAY ,CSESS(2)(1:1),
     2                     FILNAM(1,IFIL)
36        FORMAT(//,1X,72('*'),
     1            /,' DATA SCREENING: ',A16,25X,A9,1X,A5,/,1X,72('*'),/,
     2            /,' STATION  : ',A16,8X,'YEAR:',I6,12X,
     3              'SESSION:  ',A4,
     4            /,'            ',16X,8X,'DAY :',I6,12X,
     5              'FILE   :',5X,A1,
     6            //,' OBSERVAT. FILE NAME : ',A,/)
        ELSE
          WRITE(LFNPRT,37) CAMPGN,DATE,TIME,STANAM(1),IYEAR,CSESS(1),
     1                     STANAM(2),IDAY ,CSESS(2)(1:1),BLGTH,
     2                     FILNAM(1,IFIL)
37        FORMAT(//,1X,72('*'),
     1            /,' DATA SCREENING: ',A16,25X,A9,1X,A5,/,1X,72('*'),/,
     2            /,' STATION 1: ',A16,8X,'YEAR:',I6,12X,
     3              'SESSION:  ',A4,
     4            /,' STATION 2: ',A16,8X,'DAY :',I6,12X,
     5              'FILE   :',5X,A1,
     6           //,' BASELINE LENGTH (M) :',F14.3,
     7            /,' OBSERVAT. FILE NAME : ',A,/)
        END IF
C
C INITIALIZE MATRICES
C -------------------
        CALL INMAPR(LSTSLP,LSTDEL,LSTMXI,SLPLST,SLPXXX,IONO)
C
C ADJUST OPTIONS ACCORDING TO FILE HEADER INFO
C --------------------------------------------
        IF (NFRCH0.EQ.0) THEN
          IF (NFREQ.EQ.1) THEN
            NFRCHK=1
          ELSEIF(NDIFF.NE.0.AND.MINLEN.GE.BLGTH/1000)THEN
            NFRCHK=4
          ELSE
            NFRCHK=3
          ENDIF
        ELSE
          NFRCHK=NFRCH0
        ENDIF
        LTRIP =LTRIP0
        IF(ISETOP.EQ.1) THEN
          IF(NFREQ.EQ.1) THEN
            NFRCHK=1
            LTRIP =1
          ENDIF
          DO 40 IFREQ=1,NFREQ
            IWLSCR(IFREQ)=1
            DO 45 IAMB=1,NUMAMB
              IWLSCR(IFREQ)=MAX0(IWLSCR(IFREQ),AMBWLF(IAMB,IFREQ))
45          CONTINUE
40        CONTINUE
        ENDIF
C
C CHANGE PROCESSING OPTIONS ?
C ---------------------------
        IF(IAUTO.EQ.0) THEN
          WRITE(STR(2),6)
6         FORMAT('CHANGE OR DISPLAY PROCESSING OPTIONS (Y/N,DF:N) ?')
          CALL PROMP1(2,STR)
          READ(LFNKBD,1014) CHGOPT
          CALL UPPERC(CHGOPT)
          IF(CHGOPT.EQ.'Y') THEN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C change text in MODOPT SINGLE-ZERO BY USING NDIFF (new parameter to SR NDIFF)
C what is the purpose of IOPTIO
C should be modiffied for printing MXZLEO
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC            CALL MODOPT(ISETOP,NFRCHK,ISAVE,IIONO,IUSFLG,
CCC     1                  MRK1O2,MAXZEN,MXHOLE,MNCONT,IPRNT1,
CCC     2                  MXINTR,IPPROC,Q,DISCLV,LTRIP,SIGWGS,
CCC     3                  STAFIX,IPRNT2,SIGL12,IWLSCR,SWIDTH,
CCC     4                  MINCYC,IRJECT,MXOGAP,MXIOND,IAMNEW,
CCC     5                  NTONLY,L5CLEA,OMCMAX)
          ENDIF
        ENDIF
C
C CHECK WHETHER MAXIMUM INTERVAL FOR POLYNOMIAL FIT IS OK
C -------------------------------------------------------
        MXINTF=MXINTR
        IF (IPPROC(1).EQ.1 .AND. IPPROC(2).EQ.1) THEN
          QMAX=MAX(Q(1),Q(2))
        ELSE IF (IPPROC(1).EQ.1) THEN
          QMAX=Q(1)
        ELSE IF (IPPROC(2).EQ.1) THEN
          QMAX=Q(2)
        ENDIF
        NOBINT=IDINT(MXINTR*60.D0/IDELTT)+1
        IF(NOBINT.LT.QMAX+2) THEN
          MXINTF=IDINT((QMAX+1)*IDELTT/60.D0)+1
          WRITE(LFNERR,904) MXINTR,QMAX,IDELTT,QMAX+2,MXINTF,
     1                      IFIL,FILNAM(1,IFIL)
904       FORMAT(/,' ### PG MAUPRP: NEVER ENOUGH OBSERVATIONS WITHIN ',
     1                        'THE MAXIMUM INTERVAL LENGTH',/,
     2                         16X,'OF POLYNOMIAL FIT',/,
     3                         16X,'MAXIMUM INTERVAL (MIN)     :',I5,/,
     4                         16X,'POLYNOMIAL DEGREE          :',I5,/,
     5                         16X,'OBSERVATION INTERVAL (SEC) :',I5,/,
     6                         16X,'# OBSERVATIONS USED FOR FIT:',I5,/,
     7                         16X,'MAX. INTERVAL SET TO (MIN) :',I5,/,
     8                         16X,'FILE NUMBER                :',I5,/,
     9                         16X,'FILE NAME                  : ',A,/)
        ENDIF
C
C SET MARKINGS FROM SATCRUX-FILE
C ------------------------------
        NDEL=0
        DO 10 IBAD=1,NBAD
          IF (IOBBAD(IBAD).EQ.1 .OR.
     1        IOBBAD(IBAD).EQ.3) THEN
            IF (TIMBAD(1,IBAD).LE.TIMEND .AND.
     1          TIMBAD(2,IBAD).GE.TIMREF) THEN
C
C This may become bigger than max(i*4)...
C              IEPOC1=IDNINT((TIMBAD(1,IBAD)-TIMREF)*86400.D0/IDELTT)+1
C              IEPOC2=IDNINT((TIMBAD(2,IBAD)-TIMREF)*86400.D0/IDELTT)+1
C              IF (IEPOC1.LT.1) IEPOC1=1
C              IF (IEPOC2.GT.NEPOCH) IEPOC2=NEPOCH
cccc
              TIMDEL(1:2)=TIMBAD(1:2,IBAD)
              IF (TIMDEL(1).LT. TIMREF) TIMDEL(1)=TIMREF
              IF (TIMDEL(2).GT. TIMEND) TIMDEL(2)=TIMEND
              IEPOC1=IDNINT((TIMDEL(1)-TIMREF)*86400.D0/IDELTT)+1
              IEPOC2=IDNINT((TIMDEL(2)-TIMREF)*86400.D0/IDELTT)+1
cccc
              DO 15 IFREQ=1,NFREQ
                NDEL=NDEL+1
                IF(NDEL.GT.MXCDEL) THEN
                  WRITE(LFNERR,33) NDEL,MXCDEL,FILNAM(1,IFIL)
33                FORMAT(' *** PG MAUPRP: MAX. NUMBER OF ',
     1                                   'DELETIONS EXCEEDED',
     2                   /,16X,'NUMBER OF DELETIONS >=',I6,/,
     3                     16X,'MAX. NUMBER OF DEL.  :',I6,/,
     4                     16X,'FILE NAME            : ',A,/)
                  PROBLM='MXDE'
                  GOTO 1499
                ENDIF
                LSTDEL(1,NDEL)=SATBAD(IBAD)
                LSTDEL(2,NDEL)=IEPOC1
                LSTDEL(3,NDEL)=IEPOC2
                LSTDEL(4,NDEL)=IFREQ
                LSTDEL(5,NDEL)=-4
15            CONTINUE
            ENDIF
          ENDIF
10      CONTINUE
C
C INITIALIZE NUMBER OF SLIPS, MARKINGS, AND NEW AMBIGUITIES
C ---------------------------------------------------------
        NSLIP=0
        NEWAMB=0
        NCLKEV=0
C
C PROCESS FILE
C ------------
1010    CONTINUE
        NMAXI=0
C
C ADJUST OPTIONS ACCORDING TO FILE HEADER INFO
C --------------------------------------------
        IF(ISETOP.EQ.1) THEN
          IF(NFREQ.EQ.1) THEN
            NFRCHK=1
            LTRIP =1
          ENDIF
          DO 1020 IFREQ=1,NFREQ
            IWLSCR(IFREQ)=1
            DO 1045 IAMB=1,NUMAMB
              IWLSCR(IFREQ)=MAX0(IWLSCR(IFREQ),AMBWLF(IAMB,IFREQ))
1045        CONTINUE
1020      CONTINUE
        ENDIF
C
        IF(NFREQ.EQ.1.AND.NFRCHK.NE.1) THEN
          NFRCHK=1
          WRITE(LFNPRT,1011)
1011      FORMAT(/,' ONLY 1 FREQ. AVAILABLE ==>',
     1             ' FREQ. TO BE CHECKED = 1',/)
        ENDIF
        IF(NFREQ.EQ.1.AND.LTRIP.NE.1) THEN
          LTRIP=1
          IF (NDIFF.EQ.0) THEN
            WRITE(LFNPRT,2012)
2012        FORMAT(/,' ONLY 1 FREQ. AVAILABLE ==>',
     2             ' FREQ. FOR ZERO DIFF. SOLUTION = 1',/)
          ELSE
            WRITE(LFNPRT,2013)
2013        FORMAT(/,' ONLY 1 FREQ. AVAILABLE ==>',
     2             ' FREQ. FOR TRIPLE DIFF. SOLUTION = 1',/)
          END IF
        ENDIF
C
C ADD POSITIONING ECCENTRICITIES TO COORDINATES
C (EXACT FOR CARRIER "LTRIP")
C ---------------------------------------------
        DO 70 ISTA=1,NDIFF+1
C
C HANDLE STATION TYPES
C --------------------
          ISTFIL=STFIL(ISTA,IFIL)
          CALL STAFLG(STNAME(ISTFIL),TIMREF,IFLAG,MARTYP)
          IF (MARTYP.EQ.' ') THEN
            CALL ELLECC(XSTELL(1,ISTFIL),POSECC(1,ISTA),EXC)
            DO 60 I=1,3
              XWGS(I,ISTA)=XSTAT(I,ISTFIL)+EXC(I)
60          CONTINUE
          ELSE
            XWGS(1:3,ISTA)=XSTAT(1:3,ISTFIL)
          END IF
            CALL XYZELL(AELL,BELL,DXELL,DRELL,SCELL,XWGS(1,ISTA),
     1                  XELL(1,ISTA))
70      CONTINUE
C
C WRITE HEADER OF FILE CONTAINING THE RESIDUALS
C ---------------------------------------------
        IF(IRESID.EQ.1) THEN
          IF(IAUTO.EQ.0) THEN
            IREFIL=1
C
C INIT RESIDUAL HEADER RECORD
C ---------------------------
            RESHED%TITLE=TITRES
            IF (IZEROD.EQ.1) THEN
              RESHED%DSC  = resHed_MAUPRP0
            ELSE
              RESHED%DSC  = resHed_MAUPRP1
            ENDIF
            RESHED%NFIL = 1
            ALLOCATE(RESHED%FILHEAD(1),STAT=irc)
            CALL ALCERR(IRC,'RESHED%FILHEAD',(/1/),'MAUPRP')
            CALL init_filhead(RESHED%FILHEAD(1))
C
            RESHED%FILHEAD(1)%MEATYP = MEATYP
            IF(NFRCHK.LT.3) THEN
              RESHED%DSC%ITYP            = 1
              RESHED%FILHEAD(1)%NFRFIL   = 1
              RESHED%FILHEAD(1)%ICARR(1) = NFRCHK
            ELSE
              RESHED%FILHEAD(1)%NFRFIL   = 2
              RESHED%FILHEAD(1)%ICARR(1:2) = (/ 1,2 /)
            ENDIF
            RESHED%FILHEAD(1)%STANAM(1) = STNAME(1)
            IF (IZEROD.EQ.1) THEN
              RESHED%FILHEAD(1)%STANAM(2) = ' '
            ELSE
              RESHED%FILHEAD(1)%STANAM(2) = STNAME(2)
            ENDIF
            RESHED%FILHEAD(1)%CSESS(:)  = CSESS
            RESHED%FILHEAD(1)%IDELTT    = IDELTT
            RESHED%FILHEAD(1)%TIMREF    = TIMREF
            RESHED%FILHEAD(1)%NSATEL    = NSATEL
            ALLOCATE(RESHED%FILHEAD(1)%NUMSAT(NSATEL),STAT=IRC)
            CALL ALCERR(IRC,'RESHED%FILHEAD(1)%NUMSAT',
     1                 (/NSATEL/),'MAUPRP')
            RESHED%FILHEAD(1)%NUMSAT(:) = NUMSAT
C
C WRITE THE RESIDUAL FILE HEADER
C ------------------------------
            CALL WTRESH2(LFNHLP,RESHED)
C
            DEALLOCATE(RESHED%FILHEAD(1)%NUMSAT,STAT=IRC)
            DEALLOCATE(RESHED%FILHEAD,STAT=irc)
          ELSE
            IREFIL=IFIL
          ENDIF
        ENDIF
C
C ADJUST MXIOND ACCORDING TO THE BASELINE LENGTH
C ----------------------------------------------
        IF (NFRCHK.EQ.3) THEN
          IF (NDIFF.EQ.0.OR.MXIOND(3).EQ.0.OR.
     1        MXIOND(3).LE.NINT(BLGTH/1000d0)) THEN
            MXIONU = MXIOND(2)
          ELSE
            MXIONU = MXIOND(1) + NINT(DBLE(MXIOND(2)-MXIOND(1))/
     1               DBLE(MXIOND(3))*BLGTH/1000)
          ENDIF
        ELSE
          MXIONU = MXIOND(1)
        ENDIF
        IF ( NDIFF .EQ. 0 ) THEN
          WRITE(LFNPRT,1015) 'STATION',NFRCHK,MXIONU
        ELSE
          WRITE(LFNPRT,1015) 'BASELINE',NFRCHK,MXIONU
        ENDIF
1015    FORMAT(
     1  ' ',A,' DEPENDENT OPTIONS:',/,
     2  ' --------------------------',//,
     3  ' CHECK FREQUENCIES (L1=1, L2=2, L1&L2=3, L1,L2=4) --> :',I7,/,
     4  ' MAX. IONOS.DIFF. BETW. EPOCHS (o/o OF L1 CYCLES) --> :',I7,//)
C
C PREPROCESSING OF CURRENT FILE
C -----------------------------
        CALL PREPHA(IUSFLG,DISCLV,Q     ,MXINTF,IPPROC,LTRIP ,
     1              RMSMAX,JMPOPT,TOLJMP,STANAM,XWGS  ,XELL  ,
     2              SIGWGS,NSATEL,NUMSAT,NEPOCH,NFRCHK,NFREQ ,
     3              IFRMAT,TIMREF,IDELTT,RECTYP,ANTTYP,IANTEN,
     4              CSESS ,SWIDTH,MRK1O2,IRJECT,MXOGAP,MXIONU,
     5              DELAMB,IWLSCR,SIGL12,IPRNT1,IPRNT2,MINCYC,
     6              IIONO ,IREFIL,NDEL  ,LSTDEL,NMAXI ,LSTMXI,
     7              MAXZEN,SECIPL,IRESID,NSLIP ,LSTSLP,SLPLST,
     8              SLPXXX,IONO  ,TRPFLG(IFIL) ,DXFIL(1,IFIL),
     9              MXHOLE,MNCONT,IAMNEW,       NNEWAM,LSTAMB,
     .              LASTCS,NTONLY,L5CLEA,OMCMAX,AMSFLG,NDIFF ,
     1              KINSTA,MXZLEO,ITROPO,IEXTRA,MEATYP,POSECC,
     2              AELL  ,BELL  ,DXELL ,DRELL ,SCELL ,CLKFLG,
     3              NCLKEV,LSTCLK)
        IF (TRPFLG(IFIL).EQ.'B') THEN
          PROBLM='TRPL'
          GOTO 1499
        ENDIF
        IF (AMSFLG.EQ.'B') THEN
          PROBLM='MAMS'
          GOTO 1499
        ENDIF
        IF (JMPOPT(5).EQ.1.AND.CLKFLG.EQ.'J') THEN
          PROBLM='JUMP'
          GOTO 1499
        ENDIF
        IF (JMPOPT(6).EQ.1.AND.CLKFLG.EQ.'B') THEN
          PROBLM='CLKP'
          GOTO 1499
        ENDIF
        IF (NCLKEV.GT.MAXCLK) THEN
          PROBLM='CLKE'
          GOTO 1499
        ENDIF
        REWIND LFNRP1
        REWIND LFNOBS
C
C DISPLAY AMBIGUITY
C
        CALL DSPAMB(NSATEL,NUMSAT,NNEWAM,LSTAMB,IRC)
C
C INTERACTION PART
C ----------------
        IF(IAUTO.EQ.0) THEN
          IRETC=0
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C should be modiffied for printing MXZLEO
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC          CALL INTACT(ISETOP,NFRCHK,MRK1O2,IIONO ,IPRNT1,MXINTR,IUSFLG,
CCC     1                IPPROC,Q     ,DISCLV,LTRIP ,MAXZEN,SIGWGS,IPRNT2,
CCC     2                MINCYC,SIGL12,IWLSCR,SWIDTH,IRJECT,MXOGAP,MXIOND,
CCC     3                IRESID,NFREQ ,NSATEL,NUMSAT,NSLIP ,LSTSLP,SLPLST,
CCC     4           SLPXXX,IONO  ,NDEL  ,LSTDEL,NMAXI ,LSTMXI(1,1),NNEWAM,
CCC     5                LSTAMB,IRETC ,MXHOLE,MNCONT,IAMNEW,NTONLY,L5CLEA,
CCC     6                OMCMAX)
          IF(IRETC.NE.0) GOTO 1010
        ENDIF
C
C SAVE FILE ?
C -----------
        DO 108 ISATEL = 1, NSATEL
          DO 109 IAMS = 1, NNEWAM(ISATEL)
            IF (LSTAMB(2,ISATEL,IAMS) .LT. 0) NEWAMB=1
109       CONTINUE
108     CONTINUE
        IF(NDEL.EQ.0.AND.NSLIP.EQ.0.AND.NEWAMB.EQ.0.AND.
     1     IUSFLG.EQ.1) THEN
          SAVE='-'
        ELSE IF(IAUTO.EQ.0) THEN
110       WRITE(STR(2),1013)
1013      FORMAT('DO YOU WANT TO SAVE CHANGES (Y/N) ?')
          CALL PROMP1(2,STR)
          READ(LFNKBD,1014) SAVE
1014      FORMAT(A1)
          CALL UPPERC(SAVE)
          IF(SAVE.NE.'Y'.AND.SAVE.NE.'N') GOTO 110
        ELSE IF(ISAVE.EQ.1) THEN
          SAVE='Y'
        ELSE
          SAVE='N'
        ENDIF
C
C WRITE CORRECTED OBS-FILES
C -------------------------
        IF(SAVE.EQ.'Y') THEN
C
C CHECK WHETHER THE TOTAL NUMBER OF AMBIGUITIES IS TOO LARGE
C ----------------------------------------------------------
          NUMAMB=0
          DO 690 ISATEL=1,NSATEL
            NUMAMB=NUMAMB+NNEWAM(ISATEL)
690       CONTINUE
          IF (MXAMBS.EQ.0.AND.NUMAMB.GT.MAXAMB) THEN
            WRITE(LFNERR,901) NUMAMB,MAXAMB
901         FORMAT(/,' *** PG MAUPRP: TOO MANY AMBIGUITIES',/,
     1                         16X,'INCREASE MAXAMB IN M_MAXDIM',/
     2                         16X,'NUMAMB:',I6,/,
     3                         16X,'MAXAMB:',I6,/)
            CALL EXITRC(2)
          ENDIF
C
C TOO MANY AMBIGUTIES: REMOVE OBSERVATIONS OF WORST SATELLITE FIRST
          IF (NUMAMB.GT.MXAMBS) THEN
            CALL IORDUP(NNEWAM,NSATEL,INDEXS)
C
            DO ISATEL=NSATEL,1,-1
              ISAT=INDEXS(ISATEL)
              DO IAMSAT=NNEWAM(ISAT),1,-1
                IF (NUMAMB.LE.MXAMBS) GOTO 692
                NUMAMB=NUMAMB-1
                IEPLST=LSTAMB(1,ISAT,IAMSAT)
                LSTAMB(1,ISAT,IAMSAT)=0
                LSTAMB(2,ISAT,IAMSAT)=0
              ENDDO
692           CONTINUE
              NNEWAM(ISAT)=IAMSAT
C
              DO IFRQ=1,NFREQ
                DO IEPO=IEPLST,NEPOCH
                  CALL MRKOBS(6,NUMSAT(ISAT),IEPO,IFRQ,NSATEL,NUMSAT,
     1                        NDEL,LSTDEL)
                ENDDO
              ENDDO
C
              WRITE(LFNERR,903) NUMSAT(ISAT),IEPLST,NEPOCH,
     1                          FILNAM(1,IFIL)
903           FORMAT(/,' ### PG MAUPRP: TOO MANY AMBIGUITIES SET UP',
     1               /,16X,'OBSERVATIONS REMOVED TO REDUCE # OF AMBIG.',
     2               /,16X,'SATELLITE:',I6,
     3               /,16X,'EPOCHS   :',I6,' -',I6,
     4               /,16X,'FILENAME : ',A,/)
              IF (NUMAMB.LE.MXAMBS) GOTO 693
            ENDDO
693         CONTINUE
          ENDIF
C
C INITIALIZE NUMBER OF GOOD AND BAD OBSERVATIONS
          DO 310 ISATEL=1,NSATEL
            DO 300 IFREQ=1,NFREQ
              NUMMRK(ISATEL,IFREQ)=0
              NUMOBS(ISATEL,IFREQ)=0
300         CONTINUE
310       CONTINUE
C
C LOOP OVER ALL EPOCHS
          DO 400 IEP=1,NEPOCH
C
C READ OBSERVATIONS OF ONE EPOCH FROM OBS FILE COPY
            CALL RDOBSI(LFNRP1,IFRMAT,NFREQ,L12,OBSTIM,DELTAT,EPOFLG,
     1                  NSATEP,SVNEP,OBSFLG,OBSERV,IRETRN)
            IF(IRETRN.NE.0) GOTO 401
C
C COMPUTE EPOCH NUMBER
            IEPOCH=IDNINT((OBSTIM-TIMREF)*86400.D0/IDELTT)+1
C
C APPLY CYCLE SLIPS/MARK OBSERVATIONS
            CALL CYCMRK(NSATEP,NFREQ,SVNEP,IEPOCH,NSLIP,LSTSLP,
     1                  SLPLST,NDEL,LSTDEL,OBSERV,OBSFLG)
C
C CHANGE SIGN OF PHASE ZERO DIFFERENCES BACK
C AS IT IS IN OBSERVATION FILE (SEE SR DSRDBL)
C --------------------------------------------
            IF (NDIFF.EQ.0 .AND. MEATYP.EQ.1) THEN
              DO ISATEP=1,NSATEP
                DO L=1,NFREQ
                  OBSERV(ISATEP,L)=-OBSERV(ISATEP,L)
                END DO
              END DO
            END IF
C
C UPDATE GOOD AND BAD OBSERVATION COUNTERS,
C SAVE CYCLE SLIP FLAGS
            DO 360 IFREQ=1,NFREQ
              DO 350 ISAT=1,NSATEP
                DO 320 ISATEL=1,NSATEL
                  IF(NUMSAT(ISATEL).EQ.SVNEP(ISAT)) GOTO 330
320             CONTINUE
330             IF(OBSERV(ISAT,IFREQ).NE.0.D0) THEN
                  IF(TSTFLG(OBSFLG(ISAT,IFREQ),0)) THEN
                    NUMMRK(ISATEL,IFREQ)=NUMMRK(ISATEL,IFREQ)+1
                  ELSE
                    NUMOBS(ISATEL,IFREQ)=NUMOBS(ISATEL,IFREQ)+1
                  ENDIF
                  DO 370 IAMS=1,NNEWAM(ISATEL)
                    IF (LSTAMB(1,ISATEL,IAMS) .EQ. IEP)
     1                CALL SETFLG(OBSFLG(ISAT,IFREQ),1)
370               CONTINUE
                ENDIF
350           CONTINUE
360         CONTINUE
C
C WRITE OBSERVATIONS OF EPOCH
            CALL WTOBSI(LFNOBS,IFRMAT,NFREQ,OBSTIM,DELTAT,EPOFLG,
     1                  NSATEP,SVNEP,OBSFLG,OBSERV)
400       CONTINUE
C
C RESET AMBIGUITIES IF SLIPS HAVE BEEN CORRECTED
401       IF(NFREQ.EQ.1) THEN
            NFRQ=1
          ELSE
            NFRQ=3
          ENDIF
C
          IF (NSLIP.NE.0) THEN
            DO 450 IAMB=1,NUMAMB
              DO 430 IFRQ=1,NFRQ
                AMBIGU(IAMB,IFRQ)=0.D0
                AMBCLS(IAMB,IFRQ)=IAMB
430           CONTINUE
450         CONTINUE
          END IF
C
C MODIFICATION DATE AND TIME
          CRDATE(2)=DATE
          CRTIME(2)=TIME
C
C UPDATE AMBIGUITIES
C ------------------
          NUMAMB=0
          DO 700 ISATEL=1,NSATEL
            DO 730 IAMS=1,NNEWAM(ISATEL)
              NUMAMB=NUMAMB+1
              AMBSAT(NUMAMB) = NUMSAT(ISATEL)
              AMBIEP(NUMAMB) = LSTAMB(1,ISATEL,IAMS)
              DO 760 IFRQ = 1,NFRQ
                AMBIGU(NUMAMB,IFRQ) = 0.D0
                AMBCLS(NUMAMB,IFRQ) = NUMAMB
760           CONTINUE
              DO 770 IFREQ = 1,NFREQ
                AMBWLF(NUMAMB,IFREQ) = IWLSCR(IFREQ)
770           CONTINUE
730         CONTINUE
700       CONTINUE
C
C WRITE UPDATED HEADER
          CALL WTHEAD(FILNAM(1,IFIL),
     1                MEATYP,NDIFF,NFREQ,NEPOCH,NSATEL,
     2                CSESS,IDELTT,TIMREF,CAMPGN,TITLE,CRDATE,
     3                CRTIME,IRMARK,NEPFLG,IFRMAT,
     4                STANAM,RECTYP,ANTTYP,IRUNIT,IANTEN,
     5                OPRNAM,POSECC,ICLOCK,NUMSAT,NUMOBS,NUMMRK,
     6                NUMAMB,AMBSAT,AMBIEP,AMBWLF,AMBIGU,AMBCLS,
     7                USEGEOS,GOBSDEF)
C
C COMPARE NUMAMB WITH MAXAMB IN GPSEST
C ------------------------------------
          IF (NUMAMB.GT.MAXAMB2) THEN
            WRITE(LFNERR,'(/,A,2(/,16X,A),/,16X,A,A,2(/,16X,A,I5),/)')
     1      ' ### PG MAUPRP: The number of ambiguities in the Bernese',
     2      'observation file exceeds the maximum number',
     3      'of ambiguities in PG GPSEST.',
     4      'File name:     ',TRIM(FILNAM(1,IFIL)),
     5      'Number of ambiguities in file:' ,numamb,
     6      'Maximum num. allowed in GPSEST:',maxamb2
          ENDIF
C
C PRINT MESSAGE
          WRITE(LFNPRT,451)
451       FORMAT(/,' FILE SAVED',/)
        ELSE IF(SAVE.EQ.'-') THEN
          WRITE(LFNPRT,452)
452       FORMAT(/,' FILE  N O T  SAVED:',
     1           ' NO OBS. MARKED, NO SLIPS FOUND, NO AMBIG. CHANGED',/)
        ELSE
          WRITE(LFNPRT,453)
453       FORMAT(/,' FILE  N O T  SAVED',/)
          WRITE(LFNERR,454) FILNAM(1,IFIL)
454       FORMAT(/,' ### PG MAUPRP: CLEANED PHASE FILE HAS NOT BEEN ',
     1             'SAVED !',
     2           /,16X,'PHASE FILE NAME: ',A,/)
        ENDIF
C
C WRITE PROBLEM SUMMARY LINE
C --------------------------
1499    CONTINUE
        LFIL=LENGT0(FILNAM(2,IFIL))
        ICH1=MAX0(LFIL-11,1)
        ICH2=MAX0(LFIL-3,1)
        FILSTR=FILNAM(2,IFIL)(ICH1:ICH2)
        IF (NDIFF.EQ.0) THEN
          WRITE(LFNPRT,471) PROBLM,FILSTR,STANAM(1)
 471      FORMAT(/,' #@# ',A4,1X,A8,1X,A16)
        ELSE
          WRITE(LFNPRT,472) PROBLM,FILSTR,STANAM(1),STANAM(2),BLGTH
 472      FORMAT(/,' #@# ',A4,1X,A8,1X,A16,1X,A16,F14.3)
        END IF
C
C CLOSE OBS. FILES
        CLOSE(UNIT=LFNOBS)
        CLOSE(UNIT=LFNRP1)
C
        IF (ASSOCIATED(GOBSDEF%SAT)) THEN
            DEALLOCATE(GOBSDEF%SAT,STAT=IRC)
        ENDIF
C
C END OF FILE LOOP
C ----------------
1500  CONTINUE
C
C CLOSE AUXILIARY FILE AND RESIDUAL FILE
      CLOSE(UNIT=LFN002)
      IF(IRESID.EQ.1) CLOSE(UNIT=LFNHLP)
C
C PROCESS NEXT FILE ? (MANUAL PREPROC.)
      IF(IAUTO.EQ.0.AND.NFIL.GT.1) GOTO 2000
C
C SAVE TRIPLE-DIFFERENCE SOLUTION COORDINATES
C -------------------------------------------
      CALL GTFLNA(0,'COORDRS',FILCRD,IRCCRD)
      IF (IRCCRD.EQ.0.AND.KINSTA.EQ.0) THEN
        IF (NDIFF.NE.0) THEN
          CALL COORUP(IAUTO,TITRES,NSTAT,STNAME,STAFIX,XSTAT,
     1                NFIL,STFIL,TRPFLG,DXFIL,TIMCRD)
        ELSE
          WRITE(LFNPRT,'(/,A,/)')
     1          ' EPOCH DIFFERENCE SOLUTION COORDINATES SAVED'
          DO IFIL=1,NFIL
            XSTANW(1:3,IFIL) = XSTAT(1:3,STFIL(1,IFIL))+DXFIL(1:3,IFIL)
          ENDDO
C
          CALL CDSCOR('U',TITRES,1,1,NSTAT,STNAME,XSTAT,XSTECC,ICENTR,
     1                NFIL,STFIL,XSTANW,TIMCRD)
        ENDIF
      ENDIF
C
999   CLOSE(UNIT=LFNORB)
      CALL EXITRC(0)
      END

