C*
      PROGRAM SNGDIF
CC
CC NAME       :  SNGDIF
CC
CC PURPOSE    :  FORM SINGLE DIFFERENCE FILES FROM ZERO DIFFERENCE FILES
CC               (PHASE OR CODE)
CC
CC REMARKS    :  SPECIAL RECEIVER CLOCK SETTINGS FOR MINIMAC
CC
CC AUTHOR     :  M.ROTHACHER, E. BROCKMANN, L. MERVART
CC
CC CREATED    :  87/10/01 17:28
CC
CC CHANGES    :  22-JUL-91 : ??: WARNING INSTEAD OF ERROR IF REFERENCE
CC                               SATELLITE HAS MORE THAN ONE AMBIGUITY
CC               28-MAR-92 : ??: OPTION TO SET UP NEW AMBIG. IF SLIP FLAG
CC               03-MAY-92 : ??: OBS.FILE NOT WRITTEN IF FILENAME BLANK
CC               12-MAY-92 : EB: NEW OPTION TO MAX. SUM OF NUMBER OF
CC                               SNGDIF-OBSERVATIONS
CC               30-JUN-92 : ??: ONLY 1 EPOCH: SET IDELTT=1
CC               02-JUL-92 : LM: NEW OPTIMAL SELECTION OF BASELINES
CC               21-JUL-92 : LM: NEW OPTION "MINOBS"
CC               21-SEP-92 : MR: MORE ACCURATE TEST FOR SIMULTANEITY
CC               24-NOV-92 : MR: CLOCK CORRECTION FOR MINIMAC SET TO
CC                               ZERO AT THE RIGHT PLACE NOW. BEFORE
CC                               ONLY THE FIRST SATELLITE WAS CORRECTED
CC               24-DEC-92 : MR: USE OF SR "OPNFIL" TO OPEN FILES
CC               02-APR-93 : LM: REDUNDANT BASELINES
CC               06-APR-93 : LM: FLAGS FOR THE REDUNDANT BASELINES
CC               14-APR-93 : LM: CORRECT DIMENSIONS
CC               08-AUG-93 : ??: NEW FORMAT, VERSION 3.5
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               07-APR-94 : LM: NEW PROTOCOL FORMAT
CC               06-MAY-94 : RW: CHANGE OF OUTPUT FORMAT (MORE THAN 100 FILES)
CC               10-AUG-94 : MR: CALL EXITRC
CC               14-AUG-94 : MR: FORMAT 4: SESSION AS CHARACTER*4
CC               25-OCT-94 : EB: WRITE BASELINE DEFINITIONS AS OUTPUT
CC               08-APR-95 : MR: 3 DIGITS FOR FILE NUMBERS
CC               20-APR-95 : LM: WRITE THE CLUSTERS
CC               05-MAY-95 : LM: INTERN NAME CLUINP INSTEAD OF BASCLU
CC               17-SEP-95 : JJ: INCREASE MAXFIL TO 200
CC               29-SEP-95 : JJ: EXPLICITLY USE L*4 INSTEAD OF L
CC               09-OCT-95 : SF: FAST BASELINE OPTIMATION (NUMOBS)
CC               21-DEC-95 : MR: FORMAT FOR BASELINE CRITERION I3 --> I4
CC               28-MAY-96 : RS: CLUSTER DEFINITIONS NOW VALID FOR ALL STRAT.
CC               30-JAN-97 : JJ: ADD IMOST TO DISTBS CALL
CC               30-JAN-97 : JJ: NO FAST BASELINE OPT FOR MANUAL MODE (DONNOT
CC                               CALL NUMOBS IN 430 LOOP)
CC               04-FEB-97 : JJ: TAKE IMOST OUT OF CALL TO DISTBS
CC               13-NOV-97 : HH: ADD GLONASS
CC               13-NOV-97 : DI: USE CHANGED SUBROUTINE "DEFREQ"
CC               04-MAY-98 : SS: SR DEFREQ MODIFIED
CC               16-AUG-99 : RD: DIMENSIONS (SMALL/MEDIUM/LARGE)
CC               22-AUG-01 : MR: MERGE ZERO DIFF AMBIGUITIES
CC               07-MAR-01 : LM: INCLUDE MAXAMB
CC               20-NOV-01 : MM: USE OF PREDEFINED BL FOR OBSMAX NOW HANDLED
CC                               IN SNGDIF
CC               28-JUN-02 : RD/DS: MERGING VERSIONS BE AND MUNICH
CC               25-SEP-02 : HU: REMOVE I_ASTLIB
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               24-MAR-03 : RD: COMPARE NUMAMB IN SNG-DIFF WITH MAXAMB(GPSEST)
CC               08-SEP-03 : HU: ANTNAM, RECNAM, OPRNAM CHR16 -> CHR20
CC               13-SEP-03 : HU: INTERFACE FOR DEFREQ
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               17-MAY-04 : RD: DTAMBI=0d0 FOR MERGING AMBIGUITIES
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: (COM)LFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               14-DEC-06 : SS: BUGFIX: NUMOBS CORRECTED (FOR FAST COUNTING)
CC               27-FEB-07 : AG: CALL DEFCON WITH PARAMETER
CC               19-JUL-07 : SS: ADD BONUS DEPENDING ON BASELINE LENGTH
CC               09-JUL-08 : RD: ALLOCATE MAXFIL-ARRAYS
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               07-OCT-10 : RD: DO NOT CLOSE THE LFNPRT FILE
CC               27-OCT-10 : SL: USE M_BERN WITH ONLY
CC               26-JAN-11 : LP: Satellite-specific observation types possible
CC               25-FEB-11 : RD: HANDLE MARGINALLY OBSERV. SATELLITES
CC               21-JUL-11 : LP: Bugfix in consistency check for satspec obstypes
CC               29-JUL-11 : SS: RESET SUSPECT GLONASS-ONLY OBSERVATIONS
CC               02-DEC-11 : SL: NEW TITLE STRING FOR PRITIT
CC               16-FEB-12 : RD: MESSAGE IF STATION WAS LOST
CC               05-MAR-12 : RD: USE WTHEAD AS MODULE NOW
CC               24-APR-12 : LP: Loop over gobsdef%obstyp changed (4->8)
CC               01-JUN-12 : LP: New gobsdef consistency check (same FRQ, not signal)
CC               02-JUL-12 : LP: Skip satellites with less than 2 common FRQs on both stations
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: i4b, lfn001, lfnErr, lfnPrt, lfnLoc,
     1                    fileNameLength, staNameLength
      USE m_cpu,    ONLY: cpu_start
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE m_maxdim, ONLY: maxsat, maxamb
      USE p_gpsest, ONLY: maxamb2 => maxamb
      USE d_const,  ONLY: C, DATE, TIME
      USE d_rinex3, ONLY: t_gobsdef
      USE s_sdffil
      USE s_rdobsi
      USE s_opnfil
      USE s_prflna
      USE s_wtobsi
      USE s_pritit
      USE s_distbs
      USE s_sdfinp
      USE s_readinpf
      USE s_opnerr
      USE s_setflg
      USE s_rdhead
      USE s_sortbs
      USE s_defreq
      USE s_sdambf
      USE s_defcon
      USE s_exitrc
      USE s_opnsys
      USE s_wthead
      USE s_gtflna
      USE s_alcerr
      USE f_bsllen
      USE f_tstflg
      USE f_numkeys
      USE s_gobsdef,ONLY: init_geos
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I1    , IAM1  , IAMB  , IAMB0 , IAMEPO, IAMSLP,
     1          IDELT1, IDELT2, IDELTT, IEPO  , IEPOCH, IFAST , IFH   ,
     2          IFIL  , IFIL1 , IFIL2 , IFILX1, IFILX2, IFREQ , IFRMAT,
     3          IFRMT1, IFRMT2, IFRQ  , IFRQX , IMLT  , IMOST , INDEX1,
     4          INDEX2, INDNEW, IOK   , IOSTAT, IRC   , IREC  , IRETRN,
     5          IRMARK, IRMRK1, IRMRK2, ISAT  , ISAT1 , ISAT2 , ISATEL,
     6          ISATX , ISECOD, ISGNA1, ISGNA2, ISGNAL, JMPEPO, LFNOB1,
     7          LFNOB2, LFNOB3, MAXFIL, MEATY1, MEATY2, MEATYP, MINOBS,
     8          MXCAMB, MXCFIL, MXCSAT, NAMNEW, NDIFF , NDIFF1, NDIFF2,
     9          NEPFLG, NEPOC1, NEPOC2, NEPOCH, NFIL  , NFREQ , NFREQ1,
     1          NFREQ2, NFRQ  , NFRQS , NSAT  , NSAT1 , NSAT2 , NSATEL,
     2          NSATL1, NSATL2, NUMAM1, NUMAMB, MAXFL2, IAC   ,usegeos1,
     3          usegeos2,usegeos, satnum1, satnum2    , CRTBAS, samesat,
     4          JSAT  , KSAT  , NUMBAD, IBSL  , IADD,   CODPHAS, help ,
     5          ISAT3 , NOFRQ
C
      REAL*8    DISMIN, DISWIN, DTAMBI, DTDIFF, DTSIMU, EPOCH1, CRTSAT,
     1          EPOCH2, OBSTI1, OBSTI2, OBSTIM, TIMEN1, TIMEN2, TIMREF,
     2          TIMRF1, TIMRF2, BONMAX
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C MAXIMAL DIMENSIONS
C ------------------
      CHARACTER(LEN=6), PARAMETER :: pgName = 'SNGDIF'
C      PARAMETER (MAXFIL=200)
C
C MAXSAT: MAXIMUM NUMBER OF SATELLITES
C MAXFIL: MAXIMUM NUMBER OF ZERO DIFFERENCE FILES
C MAXAMB: MAXIMUM NUMBER OF AMBIGUITIES
C
C DECLARATIONS
C ------------
      CHARACTER*53 TITLE,TITLE1,TITLE2
      CHARACTER*36 CHRORD
C      CHARACTER*32 FILINP(2,MAXFIL),FILOUT(2,MAXFIL*(MAXFIL-1)/2)
      CHARACTER(LEN=filenameLength),DIMENSION(:,:),ALLOCATABLE :: FILINP
      CHARACTER(LEN=filenameLength),DIMENSION(:,:),ALLOCATABLE :: FILOUT
      CHARACTER*32 BASPRT,CLUINP
      CHARACTER*16 CAMPGN,STANAM(2)
C      CHARACTER*16 STANAX(2,MAXFIL*(MAXFIL-1)/2)
      CHARACTER(LEN=stanameLength), DIMENSION(:,:),ALLOCATABLE :: STANAX
      CHARACTER*20 RECTYP(3),OPRNAM(3),ANTTYP(3)
      CHARACTER*9  CRDATE(2)
      CHARACTER*6  MXNSAT,MXNFIL,MXNAMB
      CHARACTER*5  CRTIME(2),STATUS(3)
      CHARACTER*4  CSESS(2),CSESS1(2),CSESS2(2)
      CHARACTER*1  OBSFLG(MAXSAT,2),OBSFL1(MAXSAT,2),OBSFL2(MAXSAT,2)
      CHARACTER*1  EPOFLG,EPOFL1,EPOFL2,OBST
C
      REAL*8       POSECC(3,3),AMBIGU(MAXAMB,3)
      REAL*8       AMBIG1(MAXAMB,3)
      REAL*8       OBSERV(MAXSAT,2),OBSER1(MAXSAT,2),OBSER2(MAXSAT,2)
      REAL*8       JUMPS(MAXSAT,2),JUMP1,LSTTIM,EPOFRQ(2)
      REAL*8       TLAST(MAXSAT),DELTA1(2),DELTA2(2),DELTAT(2)
      REAL*8       BONLEN(3)
C
      INTEGER*4    NRSAT(MAXSAT),NRSAT1(MAXSAT),NRSAT2(MAXSAT)
C      INTEGER*4    ICOMBI(2,MAXFIL*(MAXFIL-1)/2)
C      INTEGER*4    IOKFIL(MAXFIL*(MAXFIL-1)/2)
      INTEGER(i4b), DIMENSION(:,:),ALLOCATABLE :: ICOMBI
      INTEGER(i4b), DIMENSION(:),  ALLOCATABLE :: IOKFIL
      INTEGER*4    IAMFLG(MAXSAT,2),IFRQS(2)
      INTEGER*4    IRUNIT(3),IANTEN(3),ICLOCK(3)
C      INTEGER*4    CRITER(MAXFIL*(MAXFIL-1)/2)
C      INTEGER*4    COMSAT(MAXFIL*(MAXFIL-1)/2)
      INTEGER(i4b), DIMENSION(:),  ALLOCATABLE :: CRITER
      INTEGER(i4b), DIMENSION(:),  ALLOCATABLE :: COMSAT
      INTEGER*4    NUMSAT(MAXSAT),NUMSA1(MAXSAT),NUMSA2(MAXSAT)
      INTEGER*4    NUMOBS(MAXSAT,2),NUMMRK(MAXSAT,2),NUMHLP(MAXSAT,2)
      INTEGER*4    AMBSAT(MAXAMB),AMBWLF(MAXAMB,2)
      INTEGER*4    AMBSA1(MAXAMB),AMBWL1(MAXAMB,2)
      INTEGER*4    AMBCLS(MAXAMB,3),AMBIEP(MAXAMB)
      INTEGER*4    AMBCL1(MAXAMB,3),AMBIE1(MAXAMB)
      INTEGER*4    AMBFLG(MAXAMB),SATNOF(MAXSAT)
      INTEGER*4    NPREDEF, IPRE, grec, jrec, obs
      INTEGER*4    NSATFL, SATFIL(2,MAXSAT), SKIPSAT
      INTEGER(i4b), DIMENSION(:),  ALLOCATABLE :: SATCRT
C
C      LOGICAL*4    FLAGBS(MAXFIL*(MAXFIL-1)/2)
C      LOGICAL*4    FLMLBS(MAXFIL*(MAXFIL-1)/2)
      LOGICAL,      DIMENSION(:),  ALLOCATABLE :: FLAGBS
      LOGICAL,      DIMENSION(:),  ALLOCATABLE :: FLMLBS
C
      TYPE(t_gobsdef) :: gobsdef1, gobsdef2, gobsdef ! Obstype info
C
CC      COMMON/LARGE/FILINP,FILOUT,ICOMBI,IOKFIL,AMBIGU,AMBCLS,
CC     1             AMBSAT,AMBIEP,AMBWLF,AMBSA1,AMBWL1
C
C COMMON FOR MAXIMAL DIMENSIONS AND CONSTANTS
C -------------------------------------------
      INCLUDE 'COMFREQ.inc'
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMAMB/MXCAMB,MXNAMB
      COMMON/MCMFIL/MXCFIL,MXNFIL
C
      DATA STATUS/'ERROR','WARNG','  OK '/
      DATA CHRORD/'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
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
C INITIALIZE MAXFIL ARRAYS
C ------------------------
      MAXFIL = MAX(NUMKEYS('PHASEZERO'),NUMKEYS('CODEZERO'))
      IF(MAXFIL.LE.2) MAXFIL = 3
      MAXFL2 = MAXFIL*(MAXFIL-1)/2
C
      ALLOCATE(FILINP(2,MAXFIL),STAT=IAC)
      CALL ALCERR(IAC,'FILINP',(/2,MAXFIL/),PGNAME)
      FILINP = ''
C
      ALLOCATE(FILOUT(2,MAXFL2),STAT=IAC)
      CALL ALCERR(IAC,'FILOUT',(/2,MAXFL2/),PGNAME)
      FILOUT=''
C
      ALLOCATE(STANAX(2,MAXFL2),STAT=IAC)
      CALL ALCERR(IAC,'STANAX',(/2,MAXFL2/),PGNAME)
      STANAX=''
C
      ALLOCATE(ICOMBI(2,MAXFL2),STAT=IAC)
      CALL ALCERR(IAC,'ICOMBI',(/2,MAXFL2/),PGNAME)
      ICOMBI=0
C
      ALLOCATE(IOKFIL(MAXFL2),STAT=IAC)
      CALL ALCERR(IAC,'IOKFIL',(/MAXFL2/),PGNAME)
      IOKFIL=0
C
      ALLOCATE(CRITER(MAXFL2),STAT=IAC)
      CALL ALCERR(IAC,'CRITER',(/MAXFL2/),PGNAME)
      CRITER=0
C
      ALLOCATE(SATCRT(MAXFL2),STAT=IAC)
      CALL ALCERR(IAC,'SATCRT',(/MAXFL2/),PGNAME)
      SATCRT=0
C
      ALLOCATE(COMSAT(MAXFL2),STAT=IAC)
      CALL ALCERR(IAC,'COMSAT',(/MAXFL2/),PGNAME)
      COMSAT=0
C
      ALLOCATE(FLAGBS(MAXFL2),STAT=IAC)
      CALL ALCERR(IAC,'FLAGBS',(/MAXFL2/),PGNAME)
C
      ALLOCATE(FLMLBS(MAXFL2),STAT=IAC)
      CALL ALCERR(IAC,'FLMLBS',(/MAXFL2/),PGNAME)
C
C INITIALIZE COMMON BLOCKS FOR MAXIMAL DIMENSIONS
C -----------------------------------------------
      MXCSAT=MAXSAT
      MXCAMB=MAXAMB
      MXCFIL=MAXFIL
      MXNSAT='MAXSAT'
      MXNAMB='MAXAMB'
      MXNFIL='MAXFIL'
C
C DEFINE LOGICAL FILE NUMBERS
C ---------------------------
      LFNOB1=LFN001
      LFNOB2=LFN001+1
      LFNOB3=LFN001+2
C
C DEFINE SYSTEM FILES
C -------------------
      CALL OPNSYS
C
C DEFINE CONSTANTS
C ----------------
      CALL DEFCON(1)
C
C PRINT TITLE AND FILES
C ---------------------
      CALL PRITIT ('SNGDIF','Create single-difference/baseline files')
      CALL PRFLNA
C
C READ PROCESSING OPTIONS
C -----------------------
      CALL SDFINP(DTSIMU,DTAMBI,IAMSLP,IMOST,MINOBS,IFAST,
     1            IMLT,DISMIN,DISWIN,BONMAX,BONLEN,CRTSAT,CRTBAS)
C
C READ FILE NAMES
C ---------------
      CALL SDFFIL(NFIL,FILINP,FILOUT,ICOMBI,NPREDEF,
     1            CRTSAT, NSATFL, SATFIL)
C
C WRITE TITLE LINES
C -----------------
C      WRITE(LFNPRT,11)
C11    FORMAT(//,1X,79('*'),
C     1        /,' GENERATION OF SINGLE DIFFERENCE FILES',
C     2        /,1X,79('*'))
C
C LOOP OVER ALL FILES
C -------------------
      ISECOD=0
22    DO 1000 IFIL=1,NFIL
        IF (ASSOCIATED(GOBSDEF1%SAT)) THEN
            DEALLOCATE(GOBSDEF1%SAT,STAT=IRC)
        ENDIF
        IF (ASSOCIATED(GOBSDEF2%SAT)) THEN
            DEALLOCATE(GOBSDEF2%SAT,STAT=IRC)
        ENDIF
        IF (ASSOCIATED(GOBSDEF%SAT)) THEN
            DEALLOCATE(GOBSDEF%SAT,STAT=IRC)
        ENDIF
C
        IF((ISECOD.EQ.1) .AND. (.NOT.FLAGBS(IFIL))) GO TO 1000
C
        IOKFIL(IFIL)=2
C
C READ THE HEADERS OF THE TWO ZERO-DIFFERENCE FILES
C -------------------------------------------------
        IFIL2=ICOMBI(2,IFIL)
        USEGEOS1=0
        GOBSDEF1%NOREC=0
        CALL RDHEAD(FILINP(1,IFIL2),
     1              MEATY2,NDIFF2,NFREQ2,NEPOC2,NSATL2,
     2              CSESS2,IDELT2,TIMRF2,CAMPGN,TITLE1,CRDATE,
     3              CRTIME,IRMRK2,NEPFLG,IFRMT2,
     4              STANAM(2),RECTYP(2),ANTTYP(2),IRUNIT(2),IANTEN(2),
     5              OPRNAM(2),POSECC(1,2),ICLOCK(2),
     6              NUMSA2,NUMHLP,NUMMRK,
     7              NUMAMB,AMBSAT,AMBIEP,AMBWLF,AMBIGU,AMBCLS,
     8              1,USEGEOS1,GOBSDEF1)
C
        IFIL1=ICOMBI(1,IFIL)
        USEGEOS2=0
        GOBSDEF2%NOREC=0
        CALL RDHEAD(FILINP(1,IFIL1),
     1              MEATY1,NDIFF1,NFREQ1,NEPOC1,NSATL1,
     2              CSESS1,IDELT1,TIMRF1,CAMPGN,TITLE2,CRDATE,
     3              CRTIME,IRMRK1,NEPFLG,IFRMT1,
     4              STANAM(1),RECTYP(1),ANTTYP(1),IRUNIT(1),IANTEN(1),
     5              OPRNAM(1),POSECC(1,1),ICLOCK(1),
     6              NUMSA1,NUMOBS,NUMMRK,
     7              NUMAM1,AMBSA1,AMBIE1,AMBWL1,AMBIG1,AMBCL1,
     8              1,USEGEOS2,GOBSDEF2)
        STANAX(1,IFIL) = STANAM(1)
        STANAX(2,IFIL) = STANAM(2)
C
C CHECK FILE COMPATIBILITY
C ------------------------
C MEASUREMENT TYPE
        IF(MEATY1.NE.MEATY2) THEN
          WRITE(LFNERR,1) MEATY1,FILINP(1,IFIL1),MEATY2,FILINP(1,IFIL2)
1         FORMAT(/,' *** PG SNGDIF: NOT THE SAME MEASUREMENT TYPE IN',/,
     1                         16X,'THE FILES TO BE COMBINED',/,
     2                         16X,'MEA.TYP.1:',I2,2X,'FILE 1: ',A32,/,
     3                         16X,'MEA.TYP.2:',I2,2X,'FILE 2: ',A32,/,
     4                         16X,'CONTINUING WITH THE NEXT FILE',/)
          IOKFIL(IFIL)=0
          CRITER(IFIL)=0
          GOTO 1000
        ENDIF
        MEATYP=MEATY1
cC
cC CHECK IF EXT. SIGNAL TYPE INFO IN BOTH FILES IS IDENTICAL AND CYCLE IF NOT
cC --------------------------------------------------------------------------
c        IF ((USEGEOS1==1).AND.(USEGEOS2==1)) THEN
cc        CHECK FOR IDENTICAL SATELLITES
c         DO grec = 1,GOBSDEF1%NOREC
c          satnum1 = GOBSDEF1%sat(grec)%sysnum*100
c     1                 + GOBSDEF1%sat(grec)%satnum
c          DO jrec = 1,GOBSDEF2%NOREC
c           satnum2 = GOBSDEF2%sat(jrec)%sysnum*100
c     1                  + GOBSDEF2%sat(jrec)%satnum
c           IF (satnum1.EQ.satnum2) THEN
cc             CHECK FOR IDENTICAL OBSERVATION TYPES IN IDENTICAL ORDER
c              DO obs = 1,4
c                IF (gobsdef1%sat(grec)%obstyp(obs).NE.
c     1              gobsdef2%sat(jrec)%obstyp(obs)) THEN
c                 WRITE(LFNERR,12) FILINP(1,IFIL1),FILINP(1,IFIL2)
c12               FORMAT(/,' ### PG SNGDIF: NOT THE SAME SAT-SPECIFIC',/,
c     1                         16X,'OBSERVATION TYPES IN',/,
c     2                         16X,'THE FILES TO BE COMBINED',/,
c     3                         16X,'FILE 1: ',A32,/,
c     4                         16X,'FILE 2: ',A32,/,
c     5                         16X,'CONTINUING WITH THE NEXT FILE',/)
c                    GOTO 1000
c                ELSE
c                    gobsdef1%sat(grec)%obstyp(obs+4)=
c     1                              gobsdef2%sat(jrec)%obstyp(obs)
c                ENDIF
c              ENDDO
c              EXIT
c           ENDIF
c          ENDDO
c         ENDDO
c        ENDIF
c
C CHECK FOR EACH SATELLITE IF OBSTYPE INFO IS AVAILABLE IN BOTH FILES AND FILL
C UP THE COMMON GOBSDEF STRUCTURE IF SIGNALS WITH SAME FREQUENCY ARE AVAILABLE
C (TO BE ACTIVATED AFTER GPSEST IS PREPARED TO HANDLE THIS)
C ----------------------------------------------------------------------------
        USEGEOS=0
        GOBSDEF%NOREC=0
        if (MEATYP==2) then
c         code observation
          CODPHAS=0
        elseif (MEATYP==1)then
c         phase observation
          CODPHAS=2
        else
          WRITE(LFNERR,*) ' ### PGM SNGDIF: Measurement type not
     1                                         handled: ',MEATYP
          GOTO 1000
        endif
c
c       COUNT NUMBER OF SATELLITES
        if ((USEGEOS1==1).and.(USEGEOS2==1)) then
          do grec = 1,GOBSDEF1%NOREC
            satnum1 = GOBSDEF1%sat(grec)%sysnum*100
     1                 + GOBSDEF1%sat(grec)%satnum
            do jrec = 1,GOBSDEF2%NOREC
              satnum2 = GOBSDEF2%sat(jrec)%sysnum*100
     1                  + GOBSDEF2%sat(jrec)%satnum
c             Same satellite available on both stations (ZD files)?
              samesat = 0
              if (satnum1.ne.satnum2) cycle
              do obs = 1,2
c              Check whether signal type info is available for both stations
               if ((gobsdef1%sat(grec)%obstyp(obs+CODPHAS).eq.'   ').or.
     1             (gobsdef2%sat(jrec)%obstyp(obs+CODPHAS).eq.'   '))
c     2              cycle
     2              exit
c              ... with same frequency
               if ((gobsdef1%sat(grec)%obstyp(obs+CODPHAS)(2:2)).ne.
     1             (gobsdef2%sat(jrec)%obstyp(obs+CODPHAS)(2:2)))
c     2             cycle
     2             exit
c              Update sat counter
               if (samesat==0) then
                  GOBSDEF%NOREC=GOBSDEF%NOREC+1
                  samesat=1
               endif
               if (samesat==1) exit
              enddo
              if (samesat==1) exit
            enddo
          enddo
C
          if (GOBSDEF%NOREC>0) then
            USEGEOS=1
          else
            WRITE(LFNERR,12) FILINP(1,IFIL1),FILINP(1,IFIL2)
12          FORMAT(/,' ### PG SNGDIF: NO CONPATIBLE SAT-SPECIFIC',/,
     1                         16X,'OBSERVATION INFO FOUND IN',/,
     2                         16X,'THE FILES TO BE COMBINED',/,
     3                         16X,'FILE 1: ',A32,/,
     4                         16X,'FILE 2: ',A32,/,
     5                         16X,'CONTINUING WITH THE NEXT FILE',/)
            GOTO 1000
          endif
        endif
c
        if (USEGEOS==1) then
c         Initialize common GOBSDEF structure
          help = GOBSDEF%NOREC
          CALL init_geos(help,GOBSDEF)
          irec = 0
          do grec = 1,GOBSDEF1%NOREC
            satnum1 = GOBSDEF1%sat(grec)%sysnum*100
     1              + GOBSDEF1%sat(grec)%satnum
            do jrec = 1,GOBSDEF2%NOREC
              satnum2 = GOBSDEF2%sat(jrec)%sysnum*100
     1                + GOBSDEF2%sat(jrec)%satnum
              samesat = 0
              if (satnum1.ne.satnum2) cycle
c
              skipsat = 0
              do obs = 1,2
c              Check whether signal type info is available for both stations
               if ((gobsdef1%sat(grec)%obstyp(obs+CODPHAS).eq.'   ').or.
     1           (gobsdef2%sat(jrec)%obstyp(obs+CODPHAS).eq.'   ')) then
                    skipsat = 1
                    exit
               endif
c              Check also for same frequency
               if ((gobsdef1%sat(grec)%obstyp(obs+CODPHAS)(2:2)).ne.
     1             (gobsdef2%sat(jrec)%obstyp(obs+CODPHAS)(2:2))) then
                    skipsat = 1
                    exit
               endif
              enddo
              if (skipsat==1) exit
c
c             Fill up obstype structure of SD file
              if (samesat==0) then
                irec=irec+1
                if (irec.gt.GOBSDEF%NOREC) then
                   WRITE(LFNERR,13) irec,GOBSDEF%NOREC,FILINP(1,IFIL1),
     1                              FILINP(1,IFIL2)
13                 FORMAT(/,' ### PG SNGDIF: More obstype records than
     1                                            expected.',/,
     2                         16X,'Records expected: ',I3.3,/,
     3                         16X,'Records found:    ',I3.3,/,
     4                         16X,'FILE 1: ',A32,/,
     5                         16X,'FILE 2: ',A32,/,
     6                         16X,'CONTINUING WITH THE NEXT FILE',/)
                   GOTO 1000
                endif
                samesat=1
                GOBSDEF%sat(irec)%sysnum=
     1               GOBSDEF1%sat(grec)%sysnum
                GOBSDEF%sat(irec)%syschar=
     1               GOBSDEF1%sat(grec)%syschar
                GOBSDEF%sat(irec)%satnum=
     1               GOBSDEF1%sat(grec)%satnum
              endif
              do obs = 1,2
                GOBSDEF%sat(irec)%obstyp(obs+CODPHAS)=
     1                 gobsdef1%sat(grec)%obstyp(obs+CODPHAS)
                GOBSDEF%sat(irec)%obstyp(obs+CODPHAS+4)=
     1                 gobsdef2%sat(jrec)%obstyp(obs+CODPHAS)
              enddo
              if (samesat==1) exit
            enddo
          enddo
        endif
C
C ARE THE SATELLITES IN THE SPECIAL LIST?
C ---------------------------------------
        DO ISAT = 1,NSATFL
          IF (SATCRT(IFIL) /= 0) CYCLE
          DO JSAT = 1,NSATL1
            IF (SATFIL(1,ISAT) == NUMSA1(JSAT) .AND.
     1          NUMOBS(JSAT,1) > 0 .AND. NUMOBS(JSAT,NFREQ1) > 0) THEN
              DO KSAT = 1,NSATL2
                IF (SATFIL(1,ISAT) == NUMSA2(KSAT) .AND.
     1            NUMHLP(KSAT,1) > 0 .AND. NUMHLP(KSAT,NFREQ2) > 0) THEN
                  SATCRT(IFIL)   = ISAT
                  SATFIL(2,ISAT) = SATFIL(2,ISAT) + 1
                ENDIF
              ENDDO
            ENDIF
          ENDDO
        ENDDO
C
C DEFINE FREQUENCIES
C ------------------
        TIMEN1=TIMRF1+(NEPOC1-1)*IDELT1/86400.D0
        TIMEN2=TIMRF2+(NEPOC2-1)*IDELT2/86400.D0
C
        EPOFRQ(1)=DMAX1(TIMRF1,TIMRF2)
        EPOFRQ(2)=DMIN1(TIMEN1,TIMEN2)

        IF (MEATYP==1) THEN
            OBST='P'
        ELSE
            OBST='C'
        ENDIF
c
        CALL DEFREQ(EPOFRQ,NSATL1,NUMSA1,NOFRQ,SATNOF ,
     1              USEGEOS=USEGEOS,GOBSDEF=GOBSDEF,MEATYPC=OBST)
C
C SKIP FORMING BASELINES IF SATELLITES WITH UNDEFINED FREQUENCIES OCCURED
C (might, e.g., be the case if different FREQs are tracked for a satellite
C  on the two stations)
        IF (NOFRQ.GT.0) THEN
         DO ISAT = 1,NOFRQ
          WRITE(LFNERR,905) SATNOF(ISAT),TRIM(STANAX(1,IFIL)),
     1                      TRIM(STANAX(2,IFIL))
905       FORMAT(/,' ### PG SNGDIF: NOT SAME/BOTH FREQUENCIES',/,
     1                15X,'FOR SATELLITE NUMBER : ',I3,/,
     2                15X,'ON STATIONS ',A,' AND ',A,'.',/)
         ENDDO
C         GOTO 1000
        ENDIF
C
C SESSION NUMBER
        IF(CSESS1(1).NE.CSESS2(1)) THEN
          WRITE(LFNERR,2) CSESS1(1),FILINP(1,IFIL1),
     1                    CSESS2(1),FILINP(1,IFIL2)
2         FORMAT(/,' *** PG SNGDIF: NOT THE SAME SESSION IN THE FILES',
     1                             ' TO BE COMBINED',/,
     2                         16X,'SESSION 1: ',A4,2X,'FILE 1: ',A32,/,
     3                         16X,'SESSION 2: ',A4,2X,'FILE 2: ',A32,/,
     4                         16X,'CONTINUING WITH THE NEXT FILE',/)
          IOKFIL(IFIL)=0
          CRITER(IFIL)=0
          GOTO 1000
        ENDIF
        CSESS(1)=CSESS1(1)
        INDEX1=INDEX(CHRORD,CSESS1(2)(1:1))
        INDEX2=INDEX(CHRORD,CSESS2(2)(1:1))
        INDNEW=MAX0(INDEX1,INDEX2)
        CSESS(2)=CHRORD(INDNEW:INDNEW)
C
C NUMBER OF DIFFERENCES
        IF(NDIFF1.NE.0.OR.NDIFF2.NE.0) THEN
          WRITE(LFNERR,3) NDIFF1,FILINP(1,IFIL1),NDIFF2,FILINP(1,IFIL2)
3         FORMAT(/,' *** PG SNGDIF: ILLEGAL NUMBER OF DIFFERENCES IN',/,
     1                         16X,'ONE OF THE FILES TO BE COMBINED',/,
     2                         16X,'# DIFF. 1:',I2,2X,'FILE 1: ',A32,/,
     3                         16X,'# DIFF. 2:',I2,2X,'FILE 2: ',A32,/,
     4                         16X,'CONTINUING WITH THE NEXT FILE',/)
          IOKFIL(IFIL)=0
          CRITER(IFIL)=0
          GOTO 1000
        ENDIF
        NDIFF=1
C
C NUMBER OF FREQUENCIES, REMARK, CREATION DATE
C ---------------------------------------------------------------
        IFRMAT=4
        NFREQ=MIN0(NFREQ1,NFREQ2)
        IF (NFREQ.EQ.2) THEN
          NFRQ=3
        ELSE
          NFRQ=1
        END IF
        IRMARK=MAX0(IRMRK1,IRMRK2)
        DO 10 I=1,2
          CRDATE(I)=DATE
          CRTIME(I)=TIME
10      CONTINUE
        TITLE=TITLE1
C
C LOOK FOR NUMBER OF COMMON SATELLITES
C ------------------------------------
CC old
c        NSATEL=0
c        DO 30 ISAT1=1,NSATL1
c          DO 20 ISAT2=1,NSATL2
c            IF(NUMSA1(ISAT1).EQ.NUMSA2(ISAT2)) THEN
cc             IF ((USEGEOS==1).AND.(GOBSDEF%NOREC>0)) THEN
cc              DO ISAT3=1,GOBSDEF%NOREC
cc               satnum1 = GOBSDEF%sat(ISAT3)%sysnum*100
cc     1                 + GOBSDEF%sat(ISAT3)%satnum
cc               IF (satnum1.EQ.NUMSA1(ISAT1)) THEN
c                NSATEL=NSATEL+1
c                IF (IMOST.NE.0) THEN
c                 DO 430 I1=1,NFREQ
c                  NUMOBS(NSATEL,I1)=
c     1              MIN0(NUMOBS(ISAT1,I1),NUMHLP(ISAT2,I1))
c430              CONTINUE
c                ENDIF
cc               ENDIF
cc              ENDDO
cc             ENDIF
c             GOTO 30
c            ENDIF
c20        CONTINUE
c30      CONTINUE
CC
        NSATEL=0
        DO 30 ISAT1=1,NSATL1
C
C SKIP SATELLITES WITH NO FREQUENCIES
          SKIPSAT=0
          IF (NOFRQ.GT.0) THEN
            DO ISAT = 1,NOFRQ
              IF (SATNOF(ISAT).EQ.NUMSA1(ISAT1)) SKIPSAT=1
            ENDDO
          ENDIF
          IF (SKIPSAT==1) CYCLE
C
          DO 20 ISAT2=1,NSATL2
            samesat = 0
            IF(NUMSA1(ISAT1).NE.NUMSA2(ISAT2)) CYCLE

            IF ((USEGEOS==1).AND.(GOBSDEF%NOREC>0)) THEN
              DO ISAT3=1,GOBSDEF%NOREC
                satnum1 = GOBSDEF%sat(ISAT3)%sysnum*100
     1                  + GOBSDEF%sat(ISAT3)%satnum
                IF (satnum1.EQ.NUMSA1(ISAT1)) THEN
                  samesat = 1
                  EXIT
                ENDIF
              ENDDO
            ELSE
              samesat = 1
            ENDIF
            IF (samesat.eq.1) THEN
              NSATEL=NSATEL+1
              IF (IMOST.NE.0) THEN
                DO 430 I1=1,NFREQ
                  NUMOBS(NSATEL,I1)=
     1            MIN0(NUMOBS(ISAT1,I1),NUMHLP(ISAT2,I1))
430             CONTINUE
              ENDIF
              GOTO 30
            ENDIF
20        CONTINUE
30      CONTINUE
CC
C
C NUMBER OF COMMON SATELLITES < 2 ?
C ---------------------------------
        IF(NSATEL.LT.2) THEN
          WRITE(LFNERR,31) NSATEL,FILINP(1,IFIL1),FILINP(1,IFIL2)
31        FORMAT(/,' *** PG SNGDIF: ONLY',I2,' COMMON SATELLITES ',/,
     1                         16X,'FILE 1: ',A32,/,
     2                         16X,'FILE 2: ',A32,/,
     3                         16X,'CONTINUING WITH THE NEXT FILE',/)
          IOKFIL(IFIL)=0
          CRITER(IFIL)=0
          GOTO 1000
        ENDIF
C
C SETUP NEW AMBIGUITIES IN SNGDIF
C -------------------------------
        IF (DTAMBI.NE.0D0) THEN
          NUMAMB=0
        ELSE
C
C MERGING OF AMBIGUITIES FROM TWO ZERO-DIFFERENCE FILES
C -----------------------------------------------------
          DO IAM1=1,NUMAM1
            EPOCH1=TIMRF1+(AMBIE1(IAM1)-1)*IDELT1/86400.D0
            IAMEPO=IDNINT((EPOCH1-TIMRF2)/IDELT2*86400.D0)+1
            IAMB0=NUMAMB+1
            DO IAMB=NUMAMB,1,-1
              IF (AMBSAT(IAMB).EQ.AMBSA1(IAM1) .AND.
     1            AMBIEP(IAMB).GE.IAMEPO) THEN
                IAMB0=IAMB
                IF (AMBIEP(IAMB).EQ.IAMEPO) GOTO 45
              ENDIF
            ENDDO
C
C ADD NEW AMBIGUITY
            NUMAMB=NUMAMB+1
            IF (NUMAMB.GT.MAXAMB) THEN
              WRITE(LFNERR,151) NUMAMB,MAXAMB,FILOUT(1,IFIL)
              IOKFIL(IFIL)=0
              CRITER(IFIL)=0
              GOTO 1000
            ENDIF
C
            DO IAMB=NUMAMB,IAMB0+1,-1
              AMBSAT(IAMB)=AMBSAT(IAMB-1)
              AMBIEP(IAMB)=AMBIEP(IAMB-1)
              AMBWLF(IAMB,:)=AMBWLF(IAMB-1,:)
              AMBIGU(IAMB,:)=AMBIGU(IAMB-1,:)
              AMBCLS(IAMB,:)=AMBCLS(IAMB-1,:)
            ENDDO
C
            AMBSAT(IAMB0)=AMBSA1(IAM1)
            AMBIEP(IAMB0)=IAMEPO
            AMBWLF(IAMB0,:)=AMBWL1(IAM1,:)
            AMBIGU(IAMB0,:)=AMBIG1(IAM1,:)
            AMBCLS(IAMB0,:)=AMBCL1(IAM1,:)
C
45          CONTINUE
C
          ENDDO
C
          DO IAMB=1,NUMAMB
            AMBFLG(IAMB)=0
          ENDDO
        ENDIF
C
C FAST STRATEGY OF OBSERVATION COUNTING AND BASELINE SELECTION
C ------------------------------------------------------------
        IF (IMOST.NE.0) THEN
          IF (BSLLEN(STANAM(1),STANAM(2)).LT.IFAST*1.D3 .AND.
     1        IMOST .EQ. 1 .AND. ISECOD .EQ. 0 ) THEN
            IDELTT=MAX0(IDELT1,IDELT2)
            GOTO 700
          ENDIF
        ENDIF
C
C
C INITIALIZE NUMBER OF SINGLE-DIFFERENCE OBSERVATIONS, NUMBER OF
C CYCLES TO BE SUBTRACTED FROM EVERY OBSERVATION, AND NUMBER OF AMB.
C ------------------------------------------------------------------
        DO 50 ISAT1=1,NSATL1
          DO 40 IFRQ=1,NFREQ
            NUMOBS(ISAT1,IFRQ)=0
            NUMMRK(ISAT1,IFRQ)=0
            IAMFLG(ISAT1,IFRQ)=1
            JUMPS(ISAT1,IFRQ)=0.D0
40        CONTINUE
          TLAST(ISAT1)=-1.D20
50      CONTINUE
C
C INITIALIZE OBSERVATION INTERVAL, NUMBER OF COMMON EPOCHS, TOTAL
C NUMBER OF SINGLE-DIFF. SATELLITES, LAST OBSERVATION TIME, AND
C CONSTANT TIME OFFSET BETWEEN THE TWO FILES
C ---------------------------------------------------------------------
        IDELTT=100000
        IEPO=0
        NSATEL=0
        NEPFLG=0
        NUMBAD=0
C
C OPEN OBSERVATION FILES
C ----------------------
        CALL OPNFIL(LFNOB1,FILINP(2,IFIL1),'OLD','UNFORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNOB1,IOSTAT,FILINP(2,IFIL1),'SNGDIF')
        CALL OPNFIL(LFNOB2,FILINP(2,IFIL2),'OLD','UNFORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNOB2,IOSTAT,FILINP(2,IFIL2),'SNGDIF')
        IF ((IMOST.EQ.0) .OR. (ISECOD.EQ.1 .AND. FLAGBS(IFIL))) THEN
          CALL OPNFIL(LFNOB3,FILOUT(2,IFIL),'UNKNOWN','UNFORMATTED',
     1                ' ',' ',IOSTAT)
          CALL OPNERR(LFNERR,LFNOB3,IOSTAT,FILOUT(2,IFIL),'SNGDIF')
        ENDIF
C
C INITIALIZE AUXILIARY AMBIGUITY ARRAY "AMBWL1"
C ---------------------------------------------
        IF (MEATYP.EQ.1) THEN
          DO 400 IAM1=1,NUMAM1
            DO 410 IAMB=1,NUMAMB
              IF (AMBSAT(IAMB).EQ.AMBSA1(IAM1)) THEN
                DO 420 IFREQ=1,NFREQ
                  AMBWL1(IAM1,IFREQ)=MAX0(AMBWLF(IAMB,IFREQ),
     1                                    AMBWL1(IAM1,IFREQ))
420             CONTINUE
                GO TO 400
              END IF
410         CONTINUE
400       CONTINUE
        END IF
C
C FREQUENCIES TO BE REQUESTED
C ---------------------------
        NFRQS=NFREQ
        IFRQS(1)=1
        IFRQS(2)=2
C
C LOOP OVER ALL EPOCHS
C --------------------
C
        DO 500 IREC=1,100000
          CALL RDOBSI(LFNOB1,IFRMT1,NFRQS,IFRQS,OBSTI1,DELTA1,EPOFL1,
     1                NSAT1,NRSAT1,OBSFL1,OBSER1,IRETRN)
          IF(IRETRN.EQ.1) GOTO 510
          CALL SDAMBF(NFREQ,NSAT1,NRSAT1,OBSFL1,NSATEL,NUMSAT,IAMFLG)
          CALL RDOBSI(LFNOB2,IFRMT2,NFRQS,IFRQS,OBSTI2,DELTA2,EPOFL2,
     1                NSAT2,NRSAT2,OBSFL2,OBSER2,IRETRN)
          IF(IRETRN.EQ.1) GOTO 510
          CALL SDAMBF(NFREQ,NSAT2,NRSAT2,OBSFL2,NSATEL,NUMSAT,IAMFLG)
C
C CHECK FOR COMMON EPOCH
C ----------------------
100       DTDIFF=OBSTI1+(DELTA1(1)+DELTA1(2))/86400.D0 -
     1           OBSTI2-(DELTA2(1)+DELTA2(2))/86400.D0
          IF(DTDIFF.LT.-DTSIMU) THEN
            CALL RDOBSI(LFNOB1,IFRMT1,NFRQS,IFRQS,OBSTI1,DELTA1,EPOFL1,
     1                  NSAT1,NRSAT1,OBSFL1,OBSER1,IRETRN)
            IF(IRETRN.EQ.1) GOTO 510
C
C KEEP TRACK OF CYCLE SLIP FLAGS
            CALL SDAMBF(NFREQ,NSAT1,NRSAT1,OBSFL1,NSATEL,NUMSAT,IAMFLG)
            GOTO 100
          ENDIF
          IF(DTDIFF.GE.DTSIMU) THEN
            CALL RDOBSI(LFNOB2,IFRMT2,NFRQS,IFRQS,OBSTI2,DELTA2,EPOFL2,
     1                  NSAT2,NRSAT2,OBSFL2,OBSER2,IRETRN)
            IF(IRETRN.EQ.1) GOTO 510
C
C KEEP TRACK OF CYCLE SLIP FLAGS
            CALL SDAMBF(NFREQ,NSAT2,NRSAT2,OBSFL2,NSATEL,NUMSAT,IAMFLG)
            GOTO 100
          ENDIF
C
C COMMON EPOCH FOUND: INITIALIZE NUMBER OF COMMON SATELLITES FOR THIS
C EPOCH
C -------------------------------------------------------------------
          ISAT=0
C
C LOOP OVER ALL FILE 1 - SATELLITES OBSERVED AT THIS EPOCH
C --------------------------------------------------------
          DO 200 ISAT1=1,NSAT1
C
C SKIP SATELLITES WITH NO FREQUENCIES
            SKIPSAT = 0
            IF (NOFRQ.GT.0) THEN
              DO ISAT = 1,NOFRQ
                IF (SATNOF(ISAT).EQ.NUMSA1(ISAT1)) SKIPSAT=1
              ENDDO
            ENDIF
            IF (SKIPSAT==1) CYCLE
C
C FIND THE SAME SATELLITE IN FILE 2 OBSERVATIONS
            DO 110 ISAT2=1,NSAT2
              samesat = 0
              IF(NRSAT2(ISAT2).NE.NRSAT1(ISAT1)) CYCLE

              IF ((USEGEOS==1).AND.(GOBSDEF%NOREC>0)) THEN
                DO ISAT3=1,GOBSDEF%NOREC
                  satnum1 = GOBSDEF%sat(ISAT3)%sysnum*100
     1                    + GOBSDEF%sat(ISAT3)%satnum
                  IF (satnum1.EQ.NRSAT1(ISAT1)) THEN
                    samesat = 1
                    EXIT
                  ENDIF
                ENDDO
              ELSE
                samesat = 1
              ENDIF
C
              IF (samesat.eq.1) GOTO 120
110         CONTINUE
            GOTO 200
C
C AT LEAST ONE OBSERVATION OK IN BOTH FILES ?
120         IOK=0
            DO 130 IFRQ=1,NFREQ
              IF(OBSER1(ISAT1,IFRQ).NE.0.D0.AND.
     1           OBSER2(ISAT2,IFRQ).NE.0.D0.AND.
     2           (.NOT.TSTFLG(OBSFL1(ISAT1,IFRQ),0)).AND.
     3           (.NOT.TSTFLG(OBSFL2(ISAT2,IFRQ),0))) IOK=1
130         CONTINUE
            IF(IOK.EQ.0) GOTO 200
C
C OBSERVATION OK FOR THIS EPOCH AND SATELLITE:
C SATELLITE ALREADY REGISTRATED IN ARRAY "NUMSAT" (ENTIRE FILE) ?
            DO 140 ISATEL=1,NSATEL
              IF(NRSAT1(ISAT1).EQ.NUMSAT(ISATEL)) GOTO 150
140         CONTINUE
C
C NEW SATELLITE
            NSATEL=NSATEL+1
            ISATEL=NSATEL
            NUMSAT(ISATEL)=NRSAT1(ISAT1)
C
C UPDATE SATELLITE AND OBSERVATION ARRAY FOR THIS EPOCH
150         ISAT=ISAT+1
            NRSAT(ISAT)=NRSAT1(ISAT1)
            DO 160 IFRQ=1,NFREQ
              OBSFLG(ISAT,IFRQ)=CHAR(0)
C
C RESET SUSPECT GLONASS-ONLY OBSERVATIONS
              IF (NRSAT(1).GT.100 .AND. NRSAT(1).LT.200) THEN
                OBSER1(ISAT1,IFRQ)=0.D0
                NUMBAD=NUMBAD+1
              ENDIF
C             ADD_GNSS_HERE ?
              IF(OBSER1(ISAT1,IFRQ).EQ.0.D0.OR.
     1           OBSER2(ISAT2,IFRQ).EQ.0.D0.OR.
     2           TSTFLG(OBSFL1(ISAT1,IFRQ),0).OR.
     3           TSTFLG(OBSFL2(ISAT2,IFRQ),0)) THEN
                OBSERV(ISAT,IFRQ)=0.D0
              ELSE
                NUMOBS(ISATEL,IFRQ)=NUMOBS(ISATEL,IFRQ)+1
C
C CYCLE SLIP FLAG
                IF(TSTFLG(OBSFL1(ISAT1,IFRQ),1).OR.
     1             TSTFLG(OBSFL2(ISAT2,IFRQ),1))
     2            CALL SETFLG(OBSFLG(ISAT,IFRQ),1)
                IF(IAMFLG(ISATEL,IFRQ).EQ.1) THEN
                  CALL SETFLG(OBSFLG(ISAT,IFRQ),1)
                  IAMFLG(ISATEL,IFRQ)=0
                ENDIF
C
C SIGNAL/NOISE
                ISGNA1=ICHAR(OBSFL1(ISAT1,IFRQ))/16
                ISGNA2=ICHAR(OBSFL2(ISAT2,IFRQ))/16
                IF(ISGNA1.NE.0.AND.ISGNA2.NE.0) THEN
                  ISGNAL=(ISGNA1+ISGNA2)/2
                  OBSFLG(ISAT,IFRQ)=CHAR(ICHAR(OBSFLG(ISAT,IFRQ))+
     1                                   ISGNAL*16)
                ENDIF
C
C OBSERVATIONS
C ------------
C
C PHASE
                IF(MEATYP.EQ.1) THEN
C
C CORRECTION FOR C*(RECEIVER CLOCK OFFSET)
                  OBSER1(ISAT1,IFRQ)=OBSER1(ISAT1,IFRQ)-C*DELTA1(2)
                  OBSER2(ISAT2,IFRQ)=OBSER2(ISAT2,IFRQ)-C*DELTA2(2)
C
                  IF(RECTYP(1)(1:6).NE.'SIMULA'.OR.
     1               RECTYP(2)(1:6).NE.'SIMULA')   THEN
C
C PHASE (NO INITIALIZATION FOR CODE AND SIMULATED OBSERVATIONS)
                    OBSERV(ISAT,IFRQ)= OBSER2(ISAT2,IFRQ)
     1                                -OBSER1(ISAT1,IFRQ)
     2                       +JUMPS(ISATEL,IFRQ)*WLGT(IFRQ,NRSAT(ISAT))
C
C FIRST OBSERVATION OF A SATELLITE --> DEFINE "JUMPS"
                    IF(NUMOBS(ISATEL,IFRQ).EQ.1) THEN
                      JUMPS(ISATEL,IFRQ)=-DNINT(OBSERV(ISAT,IFRQ)/
     1                                  WLGT(IFRQ,NRSAT(ISAT)))
                      OBSERV(ISAT,IFRQ)=OBSERV(ISAT,IFRQ)
     1                      +JUMPS(ISATEL,IFRQ)*WLGT(IFRQ,NRSAT(ISAT))
                    ENDIF
                    IF(OBSERV(ISAT,IFRQ).GE.1.D9.OR.
     1                 OBSERV(ISAT,IFRQ).LE.-1.D8) THEN
                      JUMP1=-DNINT(OBSERV(ISAT,IFRQ)/
     1                              WLGT(IFRQ,NRSAT(ISAT)))
                      JUMPS(ISATEL,IFRQ)=JUMPS(ISATEL,IFRQ)+JUMP1
                      OBSERV(ISAT,IFRQ)= OBSERV(ISAT,IFRQ)
     1                                   +JUMP1*WLGT(IFRQ,NRSAT(ISAT))
                      CALL SETFLG(OBSFLG(ISAT,IFRQ),1)
                      JMPEPO=IDNINT((OBSTI1-TIMREF)/IDELTT*86400.D0)+1
                      WRITE(LFNERR,159) IFRQ,NRSAT(ISAT),JMPEPO,JUMP1,
     1                                  FILOUT(1,IFIL)
159                   FORMAT(/,' ### PG SNGDIF: JUMP INTRODUCED TO ',
     1                                        'AVOID FORMAT OVERFLOW',/,
     2                                    16X,'FREQUENCY    :',I5,/,
     3                                    16X,'SATELLITE    :',I5,/,
     4                                    16X,'EPOCH NUMBER :',I5,/,
     5                                    16X,'JUMP (CYCLES):',F15.0,/,
     6                                    16X,'FILE         : ',A32,/)
                      IOKFIL(IFIL)=1
                    ENDIF
                  ELSE
C
C SIMULATED PHASE OBSERVATIONS
                    OBSERV(ISAT,IFRQ)= OBSER2(ISAT2,IFRQ)
     1                                -OBSER1(ISAT1,IFRQ)
                  ENDIF
C
C SET UP A NEW AMBIGUITY FOR THE FIRST EPOCH, AFTER A LONG BREAK OR
C IF CYCLE SLIP FLAG SET
                  IF(IFRQ.EQ.NFREQ.AND.DTAMBI.NE.0d0) THEN
                    IF ((TLAST(ISATEL).EQ.-1.D20)             .OR.
     1                  ((OBSTI1-TLAST(ISATEL).GE.DTAMBI)   .OR.
     2                   (IAMSLP.EQ.1                    .AND.
     3                    (TSTFLG(OBSFLG(ISAT,IFRQ),1) .OR.
     4                     TSTFLG(OBSFLG(ISAT,1),1)))))       THEN
                      NUMAMB=NUMAMB+1
                      IF(NUMAMB.GT.MAXAMB) THEN
                        WRITE(LFNERR,151) NUMAMB,MAXAMB,FILOUT(1,IFIL)
151                     FORMAT(/,' *** PG SNGDIF: TOO MANY AMBIGU',
     1                           'ITIES',/,
     2                       16X,'NUMBER OF AMBIGUITIES :',I3,/,
     3                       16X,'MAXIMUM NUMBER OF AMB.:',I3,/,
     4                       16X,'FILE NAME             : ',A32,/,
     5                       16X,'CONTINUING WITH NEXT FILE',/)
                        IOKFIL(IFIL)=0
                        CLOSE(UNIT=LFNOB1)
                        CLOSE(UNIT=LFNOB2)
                        IF ((IMOST.EQ.0) .OR. (ISECOD.EQ.1 .AND.
     1                          FLAGBS(IFIL))) CLOSE(UNIT=LFNOB3)
                        CRITER(IFIL)=0
                        GOTO 1000
                      ENDIF
C
                      AMBFLG(NUMAMB)=1
                      AMBSAT(NUMAMB)=NUMSAT(ISATEL)
                      IF (TLAST(ISATEL).EQ.-1.D20) THEN
                        AMBIEP(NUMAMB)=1
                      ELSE
                        AMBIEP(NUMAMB)=
     1                    IDNINT((OBSTI1-TIMREF)/IDELTT*86400.D0)+1
                      END IF
C
                      DO 154 IAM1=1,NUMAM1
                        IF (AMBSAT(NUMAMB).EQ.AMBSA1(IAM1)) GO TO 155
154                   CONTINUE
155                   CONTINUE
                      DO 156 IFREQ=1,NFREQ
                        AMBWLF(NUMAMB,IFREQ)=AMBWL1(IAM1,IFREQ)
156                   CONTINUE
C
                      DO 157 IFH=1,NFRQ
                        IF(RECTYP(1)(1:6).EQ.'SIMULA' .AND.
     1                     RECTYP(2)(1:6).EQ.'SIMULA') THEN
                          AMBCLS(NUMAMB,IFH)=1
                        ELSE
                          AMBCLS(NUMAMB,IFH)=NUMAMB
                        END IF
                        AMBIGU(NUMAMB,IFH)=0.D0
157                   CONTINUE
                    END IF
                    TLAST(ISATEL)=OBSTI1
                  END IF
C
C FLAG AMBIGUITIES WITH OBSERVATIONS
C ----------------------------------
                  IEPOCH=IDNINT((OBSTI1-TIMRF2)/IDELT2*86400.D0)+1
                  DO IAMB=NUMAMB,1,-1
                    IF(AMBSAT(IAMB).EQ.NUMSAT(ISATEL) .AND.
     1                 AMBIEP(IAMB).LE.IEPOCH) THEN
                      GOTO 411
                    END IF
                  ENDDO
                  IAMB=1
411               CONTINUE
                  AMBFLG(IAMB)=1
C
C CODE
                ELSE
C
C CORRECTION FOR C*(RECEIVER CLOCK OFFSET)
                  OBSER1(ISAT1,IFRQ)=OBSER1(ISAT1,IFRQ)+C*DELTA1(2)
                  OBSER2(ISAT2,IFRQ)=OBSER2(ISAT2,IFRQ)+C*DELTA2(2)
C
                  OBSERV(ISAT,IFRQ)=-( OBSER2(ISAT2,IFRQ)
     1                                -OBSER1(ISAT1,IFRQ) )
                ENDIF

              ENDIF
160         CONTINUE
200       CONTINUE
C
C NO COMMON SATELLITE WITH OBSERVATIONS OK
C ----------------------------------------
          NSAT=ISAT
          IF(NSAT.EQ.0) GOTO 500
C
C EPOCH FLAG, NUMBER OF EPOCH FLAGS
C ---------------------------------
          EPOFLG=CHAR(IOR(ICHAR(EPOFL1),ICHAR(EPOFL2)))
          IF(TSTFLG(EPOFLG,0)) NEPFLG=NEPFLG+1
C
C OBSERVATION TIME AND TIME CORRECTIONS
C -------------------------------------
C
C SPECIAL CASE MINIMAC: RECEIVER CLOCK CORRECTION ALREADY IN PHASE
          IF(RECTYP(1)(1:7).EQ.'MINIMAC') DELTA1(2)=0.D0
          IF(RECTYP(2)(1:7).EQ.'MINIMAC') DELTA2(2)=0.D0
C
          OBSTIM=OBSTI1
          DELTAT(1)=DELTA1(1)+DELTA1(2)
          DELTAT(2)=DELTA2(1)+DELTA2(2)
     1                +DNINT((OBSTI2-OBSTI1)*86400.D0)
C
C WRITE SINGLE-DIFF. OBSERVATIONS FOR THIS EPOCH
C ----------------------------------------------
          IF ((IMOST.EQ.0) .OR. (ISECOD.EQ.1 .AND. FLAGBS(IFIL)))
     1      CALL WTOBSI(LFNOB3,IFRMAT,NFREQ,OBSTIM,DELTAT,EPOFLG,
     2                  NSAT,NRSAT,OBSFLG,OBSERV)
C
C REFERENCE TIME
C --------------
          IEPO=IEPO+1
          IF(IEPO.EQ.1) TIMREF=OBSTIM
C
C OBSERVATION INTERVAL
C --------------------
          IF(IEPO.NE.1) THEN
            IDELT1=IDNINT((OBSTIM-LSTTIM)*86400.D0)
            IF(IDELT1.LT.IDELTT) IDELTT=IDELT1
          ENDIF
          LSTTIM=OBSTIM
C
C NEXT EPOCH
500     CONTINUE
C
C END OF ONE FILE REACHED: CLOSE FILES
C ------------------------------------
510     CLOSE(UNIT=LFNOB1)
        CLOSE(UNIT=LFNOB2)
        IF ((IMOST.EQ.0) .OR. (ISECOD.EQ.1 .AND. FLAGBS(IFIL)))
     1                                      CLOSE(UNIT=LFNOB3)
C
C NO COMMON EPOCH FOUND
C ---------------------
        IF(IEPO.EQ.0) THEN
          IF (IMOST.NE.1) THEN
            WRITE(LFNERR,511) FILINP(1,IFIL1),FILINP(1,IFIL2)
511         FORMAT(/,' *** PG SNGDIF: NO COMMON EPOCH FOUND',/,
     1                        16X,'FILE 1: ',A32,/,
     2                        16X,'FILE 2: ',A32,/,
     3                        16X,'CONTINUING WITH THE NEXT FILES',/)
          ENDIF
          IOKFIL(IFIL)=0
          CRITER(IFIL)=0
          GOTO 1000
        ENDIF
C
C NUMBER OF EPOCHS
C ----------------
        NEPOCH=IDNINT((OBSTIM-TIMREF)/IDELTT*86400.D0)+1
        IF (NEPOCH.EQ.1) IDELTT=1
C
C  MINIMAC: RECEIVER CLOCK CORRECTION ALREADY APPLIED --> ICLOCK=0
C  ---------------------------------------------------------------
        IF(MEATYP.EQ.1) THEN
          IF(RECTYP(1)(1:7).EQ.'MINIMAC') ICLOCK(1)=0
          IF(RECTYP(2)(1:7).EQ.'MINIMAC') ICLOCK(2)=0
        ENDIF
C
C MODFIY AMBIGUITY EPOCH NUMBERS TO NEW "TIMREF" AND "IDELTT"
C -----------------------------------------------------------
        IF (DTAMBI.EQ.0d0) THEN
          NAMNEW=0
          DO IAMB=1,NUMAMB
            EPOCH2=TIMRF2+(AMBIEP(IAMB)-1)*IDELT2/86400.D0
            IAMEPO=IDNINT((EPOCH2-TIMREF)/IDELTT*86400.D0)+1
            IF (IAMEPO.LT.1) IAMEPO=1
            IF (IAMEPO.LE.NEPOCH .AND. AMBFLG(IAMB).EQ.1) THEN
              NAMNEW=NAMNEW+1
              AMBSAT(NAMNEW)=AMBSAT(IAMB)
              AMBIEP(NAMNEW)=IAMEPO
              AMBWLF(NAMNEW,:)=AMBWLF(IAMB,:)
              AMBIGU(NAMNEW,:)=AMBIGU(IAMB,:)
              IF(RECTYP(1)(1:6).EQ.'SIMULA' .AND.
     1           RECTYP(2)(1:6).EQ.'SIMULA') THEN
                AMBCLS(NAMNEW,:)=1
                AMBIGU(NAMNEW,:)=0.D0
              ELSE
                AMBCLS(NAMNEW,:)=NAMNEW
              ENDIF
            ENDIF
          ENDDO
          NUMAMB=NAMNEW
        ENDIF
C
C WRITE SINGLE DIFFERENCE HEADER FILE
C -----------------------------------
700     IF ((IMOST.EQ.0) .OR. (ISECOD.EQ.1 .AND. FLAGBS(IFIL))) THEN
          CALL WTHEAD(FILOUT(1,IFIL),
     1                MEATYP,NDIFF,NFREQ,NEPOCH,NSATEL,
     2                CSESS,IDELTT,TIMREF,CAMPGN,TITLE,CRDATE,
     3                CRTIME,IRMARK,NEPFLG,IFRMAT,
     4                STANAM,RECTYP,ANTTYP,IRUNIT,IANTEN,
     5                OPRNAM,POSECC,ICLOCK,NUMSAT,NUMOBS,NUMMRK,
     6                NUMAMB,AMBSAT,AMBIEP,AMBWLF,AMBIGU,AMBCLS,
     7                USEGEOS,GOBSDEF)
c     7                USEGEOS1,GOBSDEF1)
C
C COMPARE NUMAMB WITH MAXAMB IN GPSEST
C ------------------------------------
          IF (NUMAMB.GT.MAXAMB2) THEN
            WRITE(LFNERR,'(/,A,2(/,16X,A),/,16X,A,A,2(/,16X,A,I5),/)')
     1      ' ### SR SNGDIF: The number of ambiguities in the single',
     2      'difference file exceeds the maximum number',
     3      'of ambiguities in PG GPSEST.',
     4      'File name:     ',TRIM(FILOUT(1,IFIL)),
     5      'Number of ambiguities in file:' ,numamb,
     6      'Maximum num. allowed in GPSEST:',maxamb2
          ENDIF
        ELSE IF (ISECOD .EQ. 0) THEN
          CRITER(IFIL)=0
          DO 530 ISATX=1,NSATEL
            DO 531 IFRQX=1,NFREQ
              CRITER(IFIL) = CRITER(IFIL)+NUMOBS(ISATX,IFRQX)
531         CONTINUE
530       CONTINUE
          CRITER(IFIL) = IDELTT*CRITER(IFIL)/60/NFREQ
          COMSAT(IFIL) = NSATEL
        END IF
C
        IF (ASSOCIATED(GOBSDEF1%SAT)) THEN
            DEALLOCATE(GOBSDEF1%SAT,STAT=IRC)
        ENDIF
        IF (ASSOCIATED(GOBSDEF2%SAT)) THEN
            DEALLOCATE(GOBSDEF2%SAT,STAT=IRC)
        ENDIF
        IF (ASSOCIATED(GOBSDEF%SAT)) THEN
            DEALLOCATE(GOBSDEF%SAT,STAT=IRC)
        ENDIF
C
        IF (NUMBAD.GT.0) THEN
          WRITE(LFNERR,'(/,A,I6,A,2(/,16X,A,A),/)')
     1      ' ### SR SNGDIF: ',NUMBAD,
     2      ' suspect GLONASS-only observations found',
     3      'File 1: ',FILINP(1,IFIL1),
     4      'File 2: ',FILINP(1,IFIL2)
        ENDIF
C             ADD_GNSS_HERE ?

C
C NEXT FILE
1000  CONTINUE
C
C USE PREDEFINED BASELINES IF OBSMAX IS SELECTED
C ----------------------------------------------
      IF (IMOST.EQ.1.AND.NPREDEF.NE.0) THEN
        DO IPRE=1,NPREDEF
          IF (CRITER(IPRE).GT.MINOBS) CRITER(IPRE)=99999
        ENDDO
      ENDIF
C
C LOOP, IF OPTION MOST IS SELECTED
C --------------------------------
      IF (IMOST.EQ.1.AND.ISECOD.EQ.0) THEN
        CALL SORTBS(MAXFIL,NFIL,STANAX,ICOMBI,CRITER,MINOBS,
     1              IMLT,DISMIN,DISWIN,BONMAX,BONLEN,CRTBAS,
     2              NSATFL,SATFIL,SATCRT,FLAGBS,FLMLBS)
        ISECOD=1
        GOTO 22
      ENDIF
C
C IF STRATEGY NOT `OBSMAX' SET BASELINE = `OK'
C --------------------------------------------
      IF (IMOST.EQ.0) THEN
        DO 1021 IFIL=1,NFIL
          FLAGBS(IFIL)=.TRUE.
1021    CONTINUE
      ENDIF
C
C DISTRIBUTE THE BASELINES ACCORDING TO CLUSTERS
C ----------------------------------------------
      CALL GTFLNA(0,'CLUINP ',CLUINP,IRC)
      IF (IRC.EQ.0)
     1  CALL DISTBS(CLUINP,NFIL,FLAGBS,FLMLBS,STANAX,FILOUT)
C
C WRITE SUMMARY
C -------------
      WRITE(LFNPRT,1032)
1032  FORMAT(  'SNGDIF: INPUT AND OUTPUT OBSERVATION FILE NAMES',
     1       /,'-----------------------------------------------',
     2       /,'0-DIF. HEADER FILE NAMES (INPUT)',1X,
     3         '0-DIF. OBS. FILE NAMES (INPUT)   NUM',
     4       /,32('*'),1X,32('*'),1X,3('*'),/)
C
      IFILX1 = -1
      IFILX2 = 0
      DO 1010 IFIL=1,NFIL
        IF (IMOST.EQ.0 .OR. FLAGBS(IFIL)) THEN
          IFILX1 = IFILX1 + 2
          IFILX2 = IFILX2 + 2
          IFIL1=ICOMBI(1,IFIL)
          IFIL2=ICOMBI(2,IFIL)
          IF(FILOUT(2,IFIL)(1:4).EQ.'    ') FILOUT(2,IFIL)=' ---'
          WRITE(LFNPRT,1002) FILINP(1,IFIL1),FILINP(2,IFIL1),IFILX1
          WRITE(LFNPRT,1002) FILINP(1,IFIL2),FILINP(2,IFIL2),IFILX2
1002      FORMAT(A32,1X,A32,1X,I3)
        END IF
1010  CONTINUE
C
      WRITE(LFNPRT,1033)
1033  FORMAT(//,'1-DIF. HEADER FILE NAMES (OUT)',3X,
     1          '1-DIF. OBS. FILE NAMES (OUT)     NR1 NR2 STAT.',
     2        /,2(32('*'),1X),2(3('*'),1X),5('*'),/)
C
      IFILX1 = -1
      IFILX2 = 0
      DO 1030 IFIL=1,NFIL
        IF (IMOST.EQ.0 .OR. FLAGBS(IFIL)) THEN
          IFILX1 = IFILX1 + 2
          IFILX2 = IFILX2 + 2
          IFIL1=ICOMBI(1,IFIL)
          IFIL2=ICOMBI(2,IFIL)
          IF(FILOUT(2,IFIL)(1:4).EQ.'    ') FILOUT(2,IFIL)=' ---'
          WRITE(LFNPRT,1031) FILOUT(1,IFIL),FILOUT(2,IFIL),
     1                       IFILX1,IFILX2,STATUS(IOKFIL(IFIL)+1)
1031      FORMAT(2(A32,1X),2(I3,1X),A5)
        END IF
1030  CONTINUE
      WRITE(LFNPRT,'(//)')
C
      IF (IMOST.EQ.1) THEN
        IBSL = 0
        IADD = 0
        DO 1020 IFIL=1,NFIL
          IF (FLAGBS(IFIL)) THEN
            IF (FLMLBS(IFIL)) THEN
              WRITE(LFNPRT,1003) IFIL,STANAX(1,IFIL),STANAX(2,IFIL),
     1                           COMSAT(IFIL),CRITER(IFIL)
1003          FORMAT(1X,I4,1X,A16,' - ',A16,' #SAT: ',I3,'  CRIT.: ',I7,
     1               ' MLT')
              IADD=IADD+1
            ELSE
              WRITE(LFNPRT,1004) IFIL,STANAX(1,IFIL),STANAX(2,IFIL),
     1                           COMSAT(IFIL),CRITER(IFIL)
1004          FORMAT(1X,I4,1X,A16,' - ',A16,' #SAT: ',I3,'  CRIT.: ',I7,
     1               '  OK')
              IBSL=IBSL+1
            END IF
          ELSE
            WRITE(LFNPRT,1005) IFIL,STANAX(1,IFIL),STANAX(2,IFIL),
     1                         COMSAT(IFIL),CRITER(IFIL)
1005        FORMAT(1X,I4,1X,A16,' - ',A16,' #SAT: ',I3,'  CRIT.: ',I7)
          END IF
1020    CONTINUE
C
C REPORT IF STATIONS HAVE BEEN LOST (DUE TO "MINOBS"-CONDITIONS)
        IF (IBSL+1.NE.MAXFIL) THEN
          WRITE(LFNERR,'(/,A,/,16X,A,3(/,16X,A,I6),/,16X,A,/)')
     1    ' ### PG SNGDIF: A STATION MIGHT GET LOST LIKELY DUE TO THE',
     2         '"MINIMUM NUMBER OF OBSERVATION" REQUIREMENT',
     2         'NUMBER OF INPUT OBSERVATION. FILES:',MAXFIL,
     3         'NUMBER OF RESULTING BASELINE FILES:',IBSL,
     4         'NUMBER OF REDUNDANT BASELINE FILES:',IADD,
     5         'CHECK THE RESULTING BASELINES FILES.'
        ENDIF
      END IF
C      CLOSE(UNIT=LFNPRT)
C
C WRITE BASELINE DEFINITION FILE
C ------------------------------
      CALL GTFLNA(0,'BASPRT ',BASPRT,IRC)
      IF(IRC.EQ.0) THEN
        CALL OPNFIL(LFNLOC,BASPRT,'UNKNOWN','FORMATTED',' ',' ',IOSTAT)
        DO 1120 IFIL=1,NFIL
          IF (IMOST.EQ.0 .OR. (FLAGBS(IFIL).AND.IMOST.EQ.1)) THEN
              WRITE(LFNLOC,1006) STANAX(1,IFIL),STANAX(2,IFIL)
1006          FORMAT(A16,1X,A16)
          END IF
1120    CONTINUE
        CLOSE(UNIT=LFNLOC)
      END IF
C
C
C      WRITE(LFNPRT,1001)
C1001  FORMAT(/,' FILE  ZERO-DIFF. HEADER  FILE 1',9X,
C     1                'SINGLE-DIFF. HEADER  FILE',9X,'STATS',
C     2       /,'       ZERO-DIFF. OBSERV. FILE 1',9X,
C     3                'SINGLE-DIFF. OBSERV. FILE',
C     4       /,'       ZERO-DIFF. HEADER  FILE 2',
C     5       /,'       ZERO-DIFF. OBSERV. FILE 2',
C     6       /,' ----',2X,32('-'),2X,32('-'),2X,5('-'),/)
C      IFILX = 0
C      DO 1010 IFIL=1,NFIL
C        IF (IMOST.EQ.0 .OR. FLAGBS(IFIL)) THEN
C          IFILX=IFILX+1
C          IFIL1=ICOMBI(1,IFIL)
C          IFIL2=ICOMBI(2,IFIL)
C          IF(FILOUT(2,IFIL)(1:4).EQ.'    ') FILOUT(2,IFIL)=' ---'
C          WRITE(LFNPRT,1002) IFILX,FILINP(1,IFIL1),FILOUT(1,IFIL),
C     1                          STATUS(IOKFIL(IFIL)+1),
C     2                          FILINP(2,IFIL1),FILOUT(2,IFIL),
C     3                          FILINP(1,IFIL2),FILINP(2,IFIL2)
C1002      FORMAT(I5,2X,A32,2X,A32,2X,A5,/,
C     1              7X,A32,2X,A32,      /,
C     2            2(7X,A32,             /))
C        END IF
C1010  CONTINUE
C
      CALL EXITRC(0)
      END
