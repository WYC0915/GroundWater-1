C*
      PROGRAM SATGRA
CC
CC NAME       :  SATGRA
CC
CC PURPOSE    :  CREATE GRAPHIC TABLE OF SATELLITE OBSERVATIONS
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  U. WILD
CC
CC CREATED    :  88/08/15 13:59
CC
CC CHANGES    :  24-DEC-92 : ??: USE OF SR "OPNFIL" TO OPEN FILES
CC               31-JUL-93 : ??: NEW FORMAT
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               10-AUG-94 : MR: CALL EXITRC
CC               14-AUG-94 : MR: FORMAT 4: SESSION AS CHARACTER*4
CC               06-JUN-96 : MR: REMOVED UNUSED VARIABLES
CC               24-SEP-97 : DI: USE MAXSAT.inc
CC               22-OCT-97 : HH: FORMAT FOR GLONASS (1X,I2) --> (I3)
CC               13-MAY-98 : MR: REMOVE "DFLOAT"
CC               07-AUG-00 : RD: CALL "DEFREQ" FOR SAT-FREQUENCIES
CC               03-JAN-01 : HB: MAXFIL TO 200 FROM 50 (SYNCHRONIZATION
CC                               WITH SR SATGRS)
CC               20-SEP-01 : HB: SWITCH TO NEW MENU
CC               02-FEB-02 : DS: PLOT AMBIGUITIES FOR PHASE FILES
CC               25-SEP-02 : HU: REMOVE I_ASTLIB
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               23-APR-03 : HU: NULLIFY LOCAL POINTERS
CC               10-JUL-03 : RD: PRINT AMBIGUITIES FOR MORE THAN ONE FILE
CC               08-SEP-03 : HU: ANTNAM, RECNAM, OPRNAM CHR16 -> CHR20
CC               13-SEP-03 : HU: INTERFACE FOR DEFREQ
CC               14-OCT-03 : RD: START AND END OF WINDOW INDEPENDENT
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               19-JAN-05 : RD: ADD MANEUVER SUMMARY
CC                               INDICATE RESOLVED AMBIGUITIES WITH 'a'
CC               21-JUN-05 : MM: LFNUM.inc REMOVED (LFNUMs IN M_BERN)
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               08-AUG-05 : HB: USE NEW SR TIMST2 (MODULE)
CC               11-AUG-05 : HU: CALL TIMST2 WITH ARRAY
CC               27-FEB-07 : AG: CALL DEFCON WITH PARAMETER
CC               26-FEB-08 : RD: USE GTSATM FROM D_SATCRX
CC               29-MAY-08 : DT: ADD RANGE AS OBSTYPE
CC               30-MAY-08 : DT: NOBSEX=1 for RANGE
CC               31-MAY-09 : RD: MAXFIL 200->300
CC               03-NOV-09 : RD: ALLOCATE ALL MAXFIL ARRAYS
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               26-JAN-11 : LP: CALL TO RDHEAD CHANGED
CC               02-DEC-11 : SL: NEW TITLE STRING FOR PRITIT, M_BERN WITH ONLY
CC               28-MAR-12 : RD: USE SVN2CHR AS MODULE NOW
CC               04-MAY-12 : RD: USE DMOD FROM MODULE
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: i4b, r8b, fileNameLength, StaNameLength,
     1                    lfnPrt, lfnLoc, lfnPrf, lfnErr, lfn001
      USE m_cpu,    ONLY: cpu_start
      USE m_maxdim, ONLY: maxsat
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE d_satcrx, ONLY: gtsatm
      USE l_basfun, ONLY: dmod
      USE s_rdobsi
      USE s_opnfil
      USE s_mrkepo
      USE s_sginp
      USE s_pritit
      USE s_svn2chr
      USE s_timst2
      USE s_defreq
      USE s_defcon
      USE s_opnsys
      USE s_gtflna
      USE s_alcerr
      USE s_prflna
      USE s_readinpf
      USE s_opnerr
      USE s_rdhead
      USE s_stripdir
      USE s_exitrc
      USE s_jmt
      USE s_radgms
      USE f_lincount
      USE f_tstflg
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I       , I1      , I2      , IAC     , IAMB    ,
     1          IC1     , IC2     , ICHMAN  , ICHR    , IDOBS   ,
     2          IDTMAN  , IDXMAN  , IFIL    , IFIL1   , IFIL2   ,
     3          IFREQ   , IFRQ    , II      , IMAN    , INDEX   ,
     4          IOSTAT  , IRC     , IRCSUM  , IRETRN  , IRMARK  ,
     5          ISAT    , ISATEL  , ISAV    , ISAVID  , ISEC    ,
     6          ISVN    , J       , JAMB    , JCHR    , JFIL    ,
     7          JJ      , JMARK   , KK      , LEPMRK  ,
     8          MAXAMB  , MAXCHR  , MAXFIL  , MAXMAN  , MAXOBS  ,
     9          MFREQ   , MINNUM  , MINSAT  , MXCAMB  , MXCCHR  ,
     1          MXCFIL  , MXCSAT  , NCHAR   , NEPFLG  , NEPOCH  ,
     2          NFLBAD  , NFREQ   , NFRQS   , NIDX    , NMAN    ,
     3          NOBS1   , NOBS2   , NOBS3   , NOBTOT  , NSAT    ,
     4          NUMMAN  , READGEOS
C
      REAL*8    AMBTIM  , DAY     , DELTAT  , DEPO1   , DEPO2   ,
     1          DT      , DTIM1   , DTIM2   , OBSTIM  , T1      ,
     2          T2      , TDISP   , TFIRST  , TIMREF  , TLAST
C
CCC       IMPLICIT  REAL*8  (A-H,O-Z)
C
      PARAMETER (MAXFIL=300,MAXCHR=255,MAXAMB=600)
C
C
C MAXFIL: MAXIMUM NUMBER OF INPUT FILES
C MAXSAT: MAXIMUM NUMBER OF SATELLITES
C
C DECLARATIONS
C -------------
C
      CHARACTER*255  EPOSTR,FILSTR
      CHARACTER*32   SUMFIL
      CHARACTER*16   CAMPGN,STANAM(2)
      CHARACTER*20   ANTTYP(2),RECTYP(2),OPRNAM(2)
      CHARACTER*9    CRDATE(2)
      CHARACTER*7    GRSTR1
      CHARACTER*6    MXNFIL,MXNSAT,MXNAMB,MXNCHR
      CHARACTER*5    CRTIME(2),GRSTR3
      CHARACTER*4    CSESS(2)
      CHARACTER*1    SVNCHR
      CHARACTER*1    STRING(2,MAXSAT,MAXCHR),GRSTR2(MAXCHR)
      CHARACTER*1    EPODSP(MAXCHR)
      CHARACTER*1    OBSFLG(MAXSAT,2),EPOFLG
      CHARACTER      OBSTYP*43,OBS1*5,OBS2*18,VORZ*1
      CHARACTER(LEN=53),DIMENSION(:),ALLOCATABLE :: TITLE
      CHARACTER(LEN=fileNameLength),
     1            DIMENSION(:,:), POINTER :: filNam
      CHARACTER(LEN=StaNameLength),
     1            DIMENSION(:,:), ALLOCATABLE :: STATNA
      CHARACTER(LEN=6),PARAMETER :: PGNAME = 'SATGRA'
C
      REAL*8     POSECC(3,2),AMBIGU(MAXAMB,3)
      REAL*8     WINDOW(2),OBSWIN(2),SEC
      REAL*8     OBSERV(MAXSAT,2)
      REAL*8     DELTA1(2)
      REAL(r8b),DIMENSION(:),  ALLOCATABLE :: TIMMAN
      REAL(r8b),DIMENSION(:,:),ALLOCATABLE :: WINMAN
      REAL(r8b),DIMENSION(:),  ALLOCATABLE :: TF
      REAL(r8b),DIMENSION(:),  ALLOCATABLE :: TL
C
      INTEGER*4  NFIL,IANTEN(2)
      INTEGER*4  IRUNIT(2),ICLOCK(2)
      INTEGER*4  AMBWLF(MAXAMB,2),AMBCLS(MAXAMB,3),PLTAMB
      INTEGER*4  IFRQS(2),NRSAT(MAXSAT),EPOMRK(MAXCHR)
      INTEGER*4  NOBS(2,MAXSAT,MAXCHR),NOBSM(2,MAXSAT,MAXCHR)
      INTEGER*4  NOBSEX(MAXCHR)
      INTEGER*4  IYEAR,IMONTH,IDAY,IHOUR,IMIN
      INTEGER(i4b),DIMENSION(:),      ALLOCATABLE :: NDIFF
      INTEGER(i4b),DIMENSION(:),      ALLOCATABLE :: MEATYP
      INTEGER(i4b),DIMENSION(:),      ALLOCATABLE :: SATMAN
      INTEGER(i4b),DIMENSION(:,:,:,:),ALLOCATABLE :: OBSMAN
      INTEGER(i4b),DIMENSION(:,:),    ALLOCATABLE :: TOTMAN
      INTEGER(i4b),DIMENSION(:,:),    ALLOCATABLE :: CHRFIL
      INTEGER(i4b),DIMENSION(:),      ALLOCATABLE :: IDXFIL
      INTEGER(i4b),DIMENSION(:,:),    ALLOCATABLE :: AMBSOL
      INTEGER(i4b),DIMENSION(:),      ALLOCATABLE :: INDEX0
      INTEGER(i4b),DIMENSION(:),      ALLOCATABLE :: IFRMAT
      INTEGER(i4b),DIMENSION(:),      ALLOCATABLE :: FILBAD
      INTEGER(i4b),DIMENSION(:,:),    ALLOCATABLE :: NUMSAT
      INTEGER(i4b),DIMENSION(:),      ALLOCATABLE :: IDELTT
      INTEGER(i4b),DIMENSION(:,:,:),  ALLOCATABLE :: NUMOBS
      INTEGER(i4b),DIMENSION(:,:,:),  ALLOCATABLE :: NUMMRK
      INTEGER(i4b),DIMENSION(:),      ALLOCATABLE :: NUMAMB
      INTEGER(i4b),DIMENSION(:,:),    ALLOCATABLE :: AMBIEP
      INTEGER(i4b),DIMENSION(:,:),    ALLOCATABLE :: AMBSAT
      INTEGER(i4b),DIMENSION(:,:),    ALLOCATABLE :: SATNUM
      INTEGER(i4b),DIMENSION(:),      ALLOCATABLE :: NSATEL
      INTEGER(i4b),DIMENSION(:),      ALLOCATABLE :: NUMEPO
      INTEGER(i4b),DIMENSION(:),      ALLOCATABLE :: IEP1
      INTEGER(i4b),DIMENSION(:),      ALLOCATABLE :: IEP2
      INTEGER(i4b),DIMENSION(:),      ALLOCATABLE :: INDEX1
C
      LOGICAL*4  MARKED,SORTED
C
C COMMONS
C -------
      COMMON /MCMFIL/ MXCFIL,MXNFIL
      COMMON /MCMSAT/ MXCSAT,MXNSAT
      COMMON /MCMCHR/ MXCCHR,MXNCHR
      COMMON/MCMAMB/MXCAMB,MXNAMB
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C INITIALIZE COMMON BLOCKS FOR MAXIMAL DIMENSIONS
C -----------------------------------------------
      MXCSAT=MAXSAT
      MXCFIL=MAXFIL
      MXCCHR=MAXCHR
      MXCAMB=MAXAMB
      MXNSAT='MAXSAT'
      MXNFIL='MAXFIL'
      MXNCHR='MAXCHR'
      MXNAMB='MAXAMB'
C
C NULLIFY POINTERS
C ----------------
      NULLIFY(filNam)
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

! Write title and file list
! -------------------------
      CALL pritit('SATGRA','Create pseudo-graphics of the observations')
      CALL prflna
C
C READ INPUT OPTIONS
C ------------------
      CALL SGINP (MFREQ,NCHAR,WINDOW,FILNAM,NFIL,PLTAMB)
      minsat=0

! Allocate memory
! ---------------
      ALLOCATE(title(nFil), stat=iac)
      CALL alcerr(iac, 'title', (/nFil/), 'satgra')
      ALLOCATE(meatyp(nFil), stat=iac)
      CALL alcerr(iac, 'meatyp', (/nFil/), 'satgra')
      ALLOCATE(ndiff(nFil), stat=iac)
      CALL alcerr(iac, 'ndiff', (/nFil/), 'satgra')
      ALLOCATE(AMBSOL(maxamb,nFil), stat=iac)
      CALL alcerr(iac, 'ambsol', (/maxamb,nFil/), 'satgra')
      ALLOCATE(tf(nFil), stat=iac)
      CALL alcerr(iac, 'tf', (/nFil/), 'satgra')
      ALLOCATE(tl(nFil), stat=iac)
      CALL alcerr(iac, 'tl', (/nFil/), 'satgra')
      ALLOCATE(index0(nFil), stat=iac)
      CALL alcerr(iac, 'index0', (/nFil/), 'satgra')
      ALLOCATE(ifrmat(nFil), stat=iac)
      CALL alcerr(iac, 'ifrmat', (/nFil/), 'satgra')
      ALLOCATE(filbad(nFil), stat=iac)
      CALL alcerr(iac, 'filbad', (/nFil/), 'satgra')
      ALLOCATE(numsat(maxSat,nFil), stat=iac)
      CALL alcerr(iac, 'numsat', (/maxSat,nFil/), 'satgra')
      ALLOCATE(ideltt(nFil), stat=iac)
      CALL alcerr(iac, 'ideltt', (/nFil/), 'satgra')
      ALLOCATE(numobs(maxsat,2,nFil), stat=iac)
      CALL alcerr(iac, 'numobs', (/maxsat,2,nFil/), 'satgra')
      ALLOCATE(nummrk(maxsat,2,nFil), stat=iac)
      CALL alcerr(iac, 'nummrk', (/maxsat,2,nFil/), 'satgra')
      ALLOCATE(numamb(nFil), stat=iac)
      CALL alcerr(iac, 'numamb', (/nFil/), 'satgra')
      ALLOCATE(ambiep(maxamb,nFil), stat=iac)
      CALL alcerr(iac, 'ambiep', (/maxamb,nFil/), 'satgra')
      ALLOCATE(ambsat(maxamb,nFil), stat=iac)
      CALL alcerr(iac, 'ambsat', (/maxamb,nFil/), 'satgra')
      ALLOCATE(satnum(maxsat,nFil), stat=iac)
      CALL alcerr(iac, 'satnum', (/maxsat,nFil/), 'satgra')
      ALLOCATE(nsatel(nFil), stat=iac)
      CALL alcerr(iac, 'nsatel', (/nFil/), 'satgra')
      ALLOCATE(numepo(nFil), stat=iac)
      CALL alcerr(iac, 'numepo', (/nFil/), 'satgra')
      ALLOCATE(iep1(nFil), stat=iac)
      CALL alcerr(iac, 'iep1', (/nFil/), 'satgra')
      ALLOCATE(iep2(nFil), stat=iac)
      CALL alcerr(iac, 'iep2', (/nFil/), 'satgra')
      ALLOCATE(index1(nFil), stat=iac)
      CALL alcerr(iac, 'index1', (/nFil/), 'satgra')
      ALLOCATE(statna(2,nFil), stat=iac)
      CALL alcerr(iac, 'statna', (/2,nFil/), 'satgra')
C
C LOOP OVER ALL INPUTFILES
C ------------------------
      TFIRST = 1.D20
      TLAST  = -1.D20
      NFLBAD = 0
      READGEOS=0
C
      DO 1000 I = 1,NFIL
C
C INITIALIZE STATION NAMES
        DO 100 KK = 1,2
          STANAM(KK) = ' '
100     CONTINUE
C
        CALL RDHEAD(FILNAM(1,I),MEATYP(I),NDIFF(I),NFREQ,NEPOCH,
     1              NSATEL(I),CSESS,IDELTT(I),TIMREF,CAMPGN,
     2              TITLE(I),CRDATE,CRTIME,IRMARK,NEPFLG,IFRMAT(I),
     3              STANAM,RECTYP,ANTTYP,IRUNIT,IANTEN,
     4              OPRNAM,POSECC,ICLOCK,NUMSAT(1,I),NUMOBS(1,1,I),
     5              NUMMRK(1,1,I),NUMAMB(I),AMBSAT(1,I),AMBIEP(1,I),
     6              AMBWLF,AMBIGU,AMBCLS,READGEOS)
C
        DO 1100 ISAT = 1,NSATEL(I)
          NUMOBS(ISAT,1,I) = 0
          NUMOBS(ISAT,2,I) = 0
1100    CONTINUE
C
        NUMEPO(I) = NEPOCH
        TF(I) = TIMREF
        DT = ((NEPOCH-1)*IDELTT(I))/86400.D0
        TL(I) = TIMREF + DT
        IF (WINDOW(1).NE.0.D0) THEN
          IF (TL(I).LT.WINDOW(1)) THEN
            NFLBAD = NFLBAD + 1
            FILBAD(NFLBAD) = I
            GOTO 1000
          ENDIF
        ENDIF
        IF (WINDOW(2).NE.1.D20) THEN
          IF (TF(I).GT.WINDOW(2)) THEN
            NFLBAD = NFLBAD + 1
            FILBAD(NFLBAD) = I
            GOTO 1000
          ENDIF
        ENDIF
C
        IF (TF(I) .LT. TFIRST) TFIRST = TF(I)
        IF (TL(I) .GT. TLAST)  TLAST  = TL(I)
C
        DO 1010 JJ = 1,NSATEL(I)
          MINNUM = NUMSAT(JJ,I)
          ISAV = 0
          DO 1011 II = 1,NSATEL(I)
            IF (NUMSAT(II,I) .LT. MINNUM) THEN
              MINNUM = NUMSAT(II,I)
              ISAV = II
            ENDIF
1011      CONTINUE
          IF (ISAV .NE. 0) THEN
            NUMSAT(ISAV,I) = 1000
          ELSE
            NUMSAT(JJ,I) = 1000
          ENDIF
          SATNUM(JJ,I) = MINNUM
1010    CONTINUE
C
        STATNA (1,I) = STANAM(1)
        STATNA (2,I) = STANAM(2)
C
        DO IAMB=1,NUMAMB(I)
          AMBSOL(IAMB,I)=0
          IF (NDIFF(I).EQ.0) CYCLE
          DO JAMB=1,NUMAMB(I)
            IF (IAMB.EQ.JAMB) CYCLE
            IF (AMBCLS(IAMB,1).EQ.AMBCLS(JAMB,1))
     1          AMBSOL(IAMB,I)=AMBSOL(IAMB,I)+1
            EXIT
          ENDDO
          DO JAMB=1,NUMAMB(I)
            IF (IAMB.EQ.JAMB) CYCLE
            IF (AMBCLS(IAMB,2).EQ.AMBCLS(JAMB,2))
     1          AMBSOL(IAMB,I)=AMBSOL(IAMB,I)+2
            EXIT
          ENDDO
          DO JAMB=1,NUMAMB(I)
            IF (IAMB.EQ.JAMB) CYCLE
            IF (AMBCLS(IAMB,3).EQ.AMBCLS(JAMB,3))
     1          AMBSOL(IAMB,I)=AMBSOL(IAMB,I)+4
            EXIT
          ENDDO
        ENDDO
C
1000  CONTINUE
C
C SET WINDOW
C ----------
      IF (WINDOW(1) .NE. 0.D00 ) TFIRST = WINDOW(1)
      IF (WINDOW(2) .NE. 1.D20 ) TLAST  = WINDOW(2)
C
      DELTAT = (TLAST - TFIRST)*86400.D0
C
C IS THERE A MANEUVER FOR ONE OF THE SATELLITES?
C ----------------------------------------------
C READ THE SATCRUX FILE
      MAXMAN=linCount('SATCRUX',6)
C
      ALLOCATE(SATMAN(MAXMAN),STAT=IRC)
      CALL ALCERR(IRC,'SATMAN',(/MAXMAN/),PGNAME)
C
      ALLOCATE(TIMMAN(MAXMAN),STAT=IRC)
      CALL ALCERR(IRC,'TIMMAN',(/MAXMAN/),PGNAME)
C
      CALL GTSATM(MAXMAN,NMAN,SATMAN,TIMMAN)
C
C COUNT THE MANEUVERS THAT ARE OBSERVED WITH THE SELECTED FILES
      NUMMAN=0
      DO 400 IMAN=1,NMAN
        IF (TIMMAN(IMAN).GE.TFIRST.AND.
     1      TIMMAN(IMAN).LE.TLAST ) THEN
          DO IFIL=1,NFIL
            DO ISATEL=1,NSATEL(IFIL)
              IF (SATMAN(IMAN).EQ.SATNUM(ISATEL,IFIL)) THEN
                NUMMAN=NUMMAN+1
                IF (IMAN.GT.NUMMAN) THEN
                  SATMAN(NUMMAN) = SATMAN(IMAN)
                  TIMMAN(NUMMAN) = TIMMAN(IMAN)
                ENDIF
                GOTO 400
              ENDIF
            ENDDO
          ENDDO
        ENDIF
400   CONTINUE
C
C PREPARE THE MANEUVER STATISTIC ARRAYS
      NFRQS=1
      IF (MFREQ.EQ.4) NFRQS=2
      ALLOCATE(OBSMAN(NCHAR,NFRQS,NFIL,NUMMAN),STAT=IRC)
      CALL ALCERR(IRC,'OBSMAN',(/NCHAR,NFRQS,NFIL,NUMMAN/),PGNAME)
      OBSMAN=0
C
      ALLOCATE(WINMAN(2,NUMMAN),STAT=IRC)
      CALL ALCERR(IRC,'WINMAN',(/2,NUMMAN/),PGNAME)
C
C FOR MANEUVERS: ONE EPOCH PER CHARACTER WITH THE MANEUVER IN THE MIDDLE
      ICHMAN=NCHAR/2-1
      IDTMAN=0
      DO IMAN=1,NUMMAN
        WINMAN(1:2,IMAN)=TIMMAN(IMAN)
        DO IFIL=1,NFIL
          DO ISATEL=1,NSATEL(IFIL)
            IF (SATMAN(IMAN).EQ.SATNUM(ISATEL,IFIL)) THEN
C
              T1=TIMMAN(IMAN)-ICHMAN*IDELTT(IFIL)/86400d0
              T2=TIMMAN(IMAN)+ICHMAN*IDELTT(IFIL)/86400d0
C
              IF (T1.LT.WINMAN(1,IMAN)) THEN
                WINMAN(1,IMAN)=T1
                IDTMAN=IDELTT(IFIL)
              ENDIF
C
              IF (T2.GT.WINMAN(2,IMAN)) THEN
                WINMAN(2,IMAN)=T2
                IDTMAN=IDELTT(IFIL)
              ENDIF
C
              EXIT
            ENDIF
          ENDDO
        ENDDO
C
C ADAAPT THE LEFT BOUNDARY OF THE MANEUVER WINDOW IF NECESSARY
        IF (WINMAN(1,IMAN).LT.TFIRST) THEN
          WINMAN(2,IMAN)=WINMAN(2,IMAN)+TFIRST-WINMAN(1,IMAN)
          WINMAN(1,IMAN)=TFIRST
          IF (WINMAN(2,IMAN).GT.TLAST) WINMAN(2,IMAN)=TLAST
        ENDIF
C
C ADAAPT THE RIGHT BOUNDARY OF THE MANEUVER WINDOW IF NECESSARY
        IF (WINMAN(2,IMAN).GT.TLAST) THEN
          WINMAN(1,IMAN)=WINMAN(1,IMAN)+TLAST-WINMAN(2,IMAN)
          WINMAN(2,IMAN)=TLAST
          IF (WINMAN(1,IMAN).LT.TFIRST) WINMAN(1,IMAN)=TFIRST
        ENDIF
      ENDDO
C
C NUMBER OF MINUTES DISPLAYED BY ONE GRAPHIC SYMBOL
C -------------------------------------------------
      TDISP = (IDINT((DELTAT/60.D0)/((NCHAR-15)/2.D0))+1)/2.D0
C
C COMPUTE FIRST AND LAST EPOCH NUMBER
C -----------------------------------
      DO 500 I=1,NFIL
        DEPO1 = TFIRST - TF(I)
        IF (DEPO1 .GT. 0.D0) THEN
          IEP1(I) = (IDINT(DEPO1*86400.D0)/IDELTT(I))+1
        ELSE
          IEP1(I) = 1
        ENDIF
        DEPO2 = TLAST - TL(I)
        IF (DEPO2 .LT. 0.D0) THEN
          IEP2(I) = NUMEPO(I) - (IDINT(DABS(DEPO2)*86400.D0)/IDELTT(I))
        ELSE
          IEP2(I) = NUMEPO(I)
        ENDIF
        DTIM1 = (TF(I) - TFIRST)*86400.D0
        DTIM2 = (TL(I) - TLAST) *86400.D0
        IF (DTIM1 .GT. 0) THEN
          INDEX0(I) = IDNINT(DTIM1/(TDISP*60.D0))
        ELSE
          INDEX0(I) = 0
        ENDIF
        IF (DTIM2 .GT. 0.D0) THEN
          INDEX1(I) = 0
        ELSE
          INDEX1(I) = IDNINT(DABS(DTIM2)/(TDISP*60.D0))
        ENDIF
500   CONTINUE
C
C LOOP OVER ALL INPUT FILES
C -------------------------
      DO 2000 IFIL = 1,NFIL
C
C CHECK FILE NUMBER (FILE OUT OF WINDOW)
C --------------------------------------
        DO 2010 JJ = 1,NFLBAD
          IF (FILBAD(JJ) .EQ. IFIL) THEN
            WRITE(LFNPRT,2011) FILNAM(2,IFIL)
2011        FORMAT(/,' *** PGM SATGRA: START TIME OUT OF WINDOW',/,
     1             17X,'FILE: ',A32,/)
            GOTO 2000
          ENDIF
2010    CONTINUE
C
C DEFINE FREQUENCIES FOR THE SATELLITES OF THE FILE
C -------------------------------------------------
        OBSWIN(1)=TFIRST
        OBSWIN(2)=TLAST
        CALL DEFREQ(OBSWIN,NSATEL(IFIL),SATNUM(1,IFIL))
C
C OPEN OBSERVATION FILE
C ---------------------
        CALL OPNFIL(LFN001,FILNAM(2,IFIL),'OLD','UNFORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFN001,IOSTAT,FILNAM(2,IFIL),'SATGRA')
C
C INITIALIZE NUMBER OF OBSERVATION FOR EACH CHARACTER
C ---------------------------------------------------
        DO 2100 ICHR = 1,NCHAR-15
          NOBSEX(ICHR) = 0
          DO 2110 ISAT = 1,NSATEL(IFIL)
            DO 2120 IFREQ = 1,2
              NOBS(IFREQ,ISAT,ICHR) = 0
              NOBSM(IFREQ,ISAT,ICHR) = 0
2120        CONTINUE
2110      CONTINUE
2100    CONTINUE
C
C NUMBER OF EXPECTED OBSERVATIONS FOR ONE CHARACTER
C -------------------------------------------------
        INDEX = INDEX0(IFIL)+1
        ISAVID = INDEX0(IFIL)+1
        JJ = 0
        DO 2500 J = IEP1(IFIL),IEP2(IFIL)
          JJ = JJ + 1
          ISEC = (JJ - 1)*IDELTT(IFIL)
          INDEX = INDEX0(IFIL) + ISEC/(IDNINT(TDISP*60.D0))+1
C
C Range observations only 1 observation expected
          IF (MEATYP(IFIL).EQ.3) THEN
            NOBSEX(INDEX) = 1
C GNSS observations
          ELSE
            NOBSEX(INDEX) = NOBSEX(INDEX) + 1
          END IF

          IF (INDEX .NE. ISAVID) THEN
            EPOMRK(INDEX-1) = J
            ISAVID = INDEX
          ENDIF
2500    CONTINUE
C
C LOOP OVER ALL OBSERVATION EPOCHS
C ---------------------------------
        IF (MFREQ .EQ. 1) THEN
          NFRQS = 1
          IFRQS(1) = 1
        ELSE IF (MFREQ .EQ. 2) THEN
          NFRQS = 1
          IFRQS(1) = 2
        ELSE IF (MFREQ .EQ. 3) THEN
          NFRQS = 1
          IFRQS(1) = 3
        ELSE IF (MFREQ .EQ. 4) THEN
          NFRQS = 2
          IFRQS(1) = 1
          IFRQS(2) = 2
        ENDIF
C
        II = 0
        DO 3000 J = IEP1(IFIL),IEP2(IFIL)
C
3500      CALL RDOBSI(LFN001,IFRMAT(IFIL),NFRQS,IFRQS,OBSTIM,DELTA1,
     1                EPOFLG,NSAT,NRSAT,OBSFLG,OBSERV,IRETRN)
          IF (IRETRN .EQ. 1) GOTO 3010
C
          IF (OBSTIM .LT. TFIRST .OR. OBSTIM .GT. TLAST) THEN
            GOTO 3500
          ENDIF
C
          IF (WINDOW(1) .GT. 0D0 .AND. INDEX0(IFIL) .EQ. 0) THEN
            ISEC = IDNINT((OBSTIM-TFIRST)*86400.D0)
          ELSE
            ISEC = IDNINT((OBSTIM-TF(IFIL))*86400.D0)
          ENDIF
          INDEX = INDEX0(IFIL) + ISEC/(IDNINT(TDISP*60.D0))+1
          IF (INDEX .EQ. 0 .OR. INDEX .GT. MAXCHR) THEN
            WRITE(LFNERR,3501) INDEX,MAXCHR
3501        FORMAT(/,'*** PGM SATGRA: UNALLOWED CHARACTER INDEX',/,
     1               16X,'INDEX  : ',I6,/,
     2               16X,'MAXCHR : ',I6)
            GOTO 2020
          ENDIF
C
C LOOP OVER ALL SATELLITES
C ------------------------
          DO 4000 ISAT = 1,NSAT
C
C FIND INDEX IN ARRAY "SATNUM" FOR SATELLITE "ISAT"
            DO 4010 ISATEL=1,NSATEL(IFIL)
              IF(NRSAT(ISAT).EQ.SATNUM(ISATEL,IFIL)) GOTO 4020
4010        CONTINUE
C
4020        DO 4050 IFREQ = 1,NFRQS
              MARKED = TSTFLG(OBSFLG(ISAT,IFREQ),0)
              IF (OBSERV(ISAT,IFREQ).NE.0.D0.AND.
     1            .NOT.MARKED.AND.NSAT.GE.MINSAT) THEN
                NOBS(IFREQ,ISATEL,INDEX) = NOBS(IFREQ,ISATEL,INDEX) + 1
              ELSE IF (OBSERV(ISAT,IFREQ).NE.0.D0.AND.
     1                 (MARKED.OR.NSAT.LT.MINSAT)) THEN
                NOBSM(IFREQ,ISATEL,INDEX)=NOBSM(IFREQ,ISATEL,INDEX) + 1
              ENDIF
4050        CONTINUE
C
C IS THERE A MANEUVER FOR THIS SATELLITE?
            DO IMAN=1,NUMMAN
              IF (NRSAT(ISAT).EQ.SATMAN(IMAN).AND.
     1            OBSTIM.GE.WINMAN(1,IMAN).AND.
     2            OBSTIM.LE.WINMAN(2,IMAN)) THEN
                IDXMAN=NINT((OBSTIM-WINMAN(1,IMAN))*86400d0/IDTMAN)+1
                DO IFREQ = 1,NFRQS
                  MARKED = TSTFLG(OBSFLG(ISAT,IFREQ),0)
                  IF (OBSERV(ISAT,IFREQ).NE.0.D0.AND.MARKED) THEN
                    OBSMAN(IDXMAN,IFREQ,IFIL,IMAN)=1
                  ELSE IF (OBSERV(ISAT,IFREQ).NE.0.D0.AND..NOT.MARKED
     1                 .AND.OBSMAN(IDXMAN,IFREQ,IFIL,IMAN).EQ.0) THEN
                    OBSMAN(IDXMAN,IFREQ,IFIL,IMAN)=2
                  ENDIF
                ENDDO
              ENDIF
            ENDDO
C
4000      CONTINUE
C
3000    CONTINUE
C
C LAST INDEX
C ----------
3010    JMARK = INDEX + INDEX1(IFIL)
        LEPMRK = INDEX
C
C INITIALIZE GRAPHIC STRINGS
C --------------------------
        DO 4520 ICHR=1,MAXCHR
          GRSTR2(ICHR) = ' '
          EPOSTR(ICHR:ICHR) = ' '
          EPODSP(ICHR) = ' '
          DO 4510 ISAT=1,NSATEL(IFIL)
            DO 4500 IFREQ=1,NFRQS
              STRING(IFREQ,ISAT,ICHR) = ' '
4500        CONTINUE
4510      CONTINUE
4520    CONTINUE
C
        GRSTR1 = '       '
C
        IDOBS = EPOMRK(INDEX0(IFIL)+2) - EPOMRK(INDEX0(IFIL)+1)
C
C LOOP OVER ALL GRAPHIC CHARACTERS
C --------------------------------
        DO 5000 ICHR = 1,JMARK
          DO 5010 ISATEL = 1,NSATEL(IFIL)
            DO 5020 IFREQ = 1,NFRQS
              IF (NOBSEX(ICHR) .EQ. 0) THEN
                STRING(IFREQ,ISATEL,ICHR) = ' '
                GOTO 5020
              ENDIF
              NOBS1 = NOBS(IFREQ,ISATEL,ICHR)
              NUMOBS(ISATEL,IFREQ,IFIL) = NUMOBS(ISATEL,IFREQ,IFIL)
     1                                    + NOBS1
              NOBS2 = NOBSM(IFREQ,ISATEL,ICHR)
              NOBS3 = NOBSEX(ICHR) - NOBS1 - NOBS2
              MAXOBS = MAX0(NOBS1,NOBS2,NOBS3)
              IF (MAXOBS .EQ. NOBS1) THEN
                STRING(IFREQ,ISATEL,ICHR) = '*'
                IF (ICHR .GT. INDEX0(IFIL)+1 .AND.
     1              EPOMRK(ICHR) .NE. 0      .AND.
     2              TDISP*60.D0 .GT. IDELTT(IFIL) ) THEN
                  CALL MRKEPO (NOBS,NOBSM,IDOBS,STRING,IFREQ,ISATEL,
     1                         ICHR,'*',EPOMRK,EPOSTR)
                ENDIF
              ELSE IF (MAXOBS .EQ. NOBS2) THEN
                STRING(IFREQ,ISATEL,ICHR) = '-'
                IF (ICHR .GT. INDEX0(IFIL)+1 .AND.
     1              EPOMRK(ICHR) .NE. 0      .AND.
     2              TDISP*60.D0 .GT. IDELTT(IFIL) ) THEN
                  CALL MRKEPO (NOBS,NOBSM,IDOBS,STRING,IFREQ,ISATEL,
     1                         ICHR,'-',EPOMRK,EPOSTR)
                ENDIF
              ELSE IF (MAXOBS .EQ. NOBS3) THEN
                STRING(IFREQ,ISATEL,ICHR) = ' '
                IF (ICHR .GT. INDEX0(IFIL)+1 .AND.
     1              EPOMRK(ICHR) .NE. 0      .AND.
     2              TDISP*60.D0 .GT. IDELTT(IFIL) ) THEN
                  CALL MRKEPO (NOBS,NOBSM,IDOBS,STRING,IFREQ,ISATEL,
     1                         ICHR,' ',EPOMRK,EPOSTR)
                ENDIF
              ENDIF
5020        CONTINUE
5010      CONTINUE
5000    CONTINUE
C
C ADD AMBIGUITY CHARACTERS
C ------------------------
        IF (PLTAMB.EQ.1) THEN
          DO IAMB=1,NUMAMB(IFIL)
            OBSTIM=TF(IFIL)+(AMBIEP(IAMB,IFIL)-1)*IDELTT(IFIL)/86400.D0
            IF (OBSTIM .GE. TFIRST .AND. OBSTIM .LE. TLAST) THEN
              IF (WINDOW(1) .GT. 0 .AND. INDEX0(IFIL) .EQ. 0) THEN
                ISEC = IDNINT((OBSTIM-TFIRST)*86400.D0)
              ELSE
                ISEC = IDNINT((OBSTIM-TF(IFIL))*86400.D0)
              ENDIF
              INDEX = INDEX0(IFIL) + ISEC/(IDNINT(TDISP*60.D0))+1
              IF (INDEX .EQ. 0 .OR. INDEX .GT. MAXCHR) THEN
                WRITE(LFNERR,3501) INDEX,MAXCHR
                GOTO 2020
              ENDIF
              DO ISATEL=1,NSATEL(IFIL)
                IF(SATNUM(ISATEL,IFIL).EQ.AMBSAT(IAMB,IFIL)) EXIT
              END DO
              IF (AMBSOL(IAMB,IFIL).EQ.0) THEN
                STRING(1:NFRQS,ISATEL,INDEX)='A'
              ELSE
                STRING(1:NFRQS,ISATEL,INDEX)='a'
              ENDIF
            ENDIF
          END DO
        ENDIF
C
C CLOSE INPUT FILE
C ----------------
        CLOSE (UNIT = LFN001)
C
C DISPLAY
C -------
C
C WRITE HEADER FOR GRAPHIC DISPLAY
        WRITE (LFNPRT,6021) TITLE(IFIL)
6021    FORMAT (1X,A53,/,1X,53('-'))
C
        IF (NDIFF(IFIL) .EQ. 0) THEN
          WRITE(LFNPRT,6022) CAMPGN,STATNA(1,IFIL)
6022      FORMAT(1X,'CAMPAIGN: ',A16,'STATION: ',A16)
        ELSE
          WRITE(LFNPRT,6023) CAMPGN,(STATNA(J,IFIL),J=1,2)
6023      FORMAT(1X,'CAMPAIGN: ',A16,'STATION 1: ',A16,
     1           'STATION 2: ',A16)
        ENDIF
C
        IF (MEATYP(IFIL) .EQ. 1) THEN
          OBS1 = 'PHASE'
        ELSE IF (MEATYP(IFIL) .EQ. 2) THEN
          OBS1 = 'CODE '
        ELSE
          OBS1 = 'RANGE'
        ENDIF
C
        IF (NDIFF(IFIL) .EQ. 0) THEN
          OBS2 = ' ZERO DIFFERENCE  '
        ELSE IF (NDIFF(IFIL) .EQ. 1) THEN
          OBS2 = ' SINGLE DIFFERENCE'
        ENDIF
C
        OBSTYP = 'TYP OF OBSERVATION: '//OBS1//OBS2
        WRITE(LFNPRT,6024) OBSTYP
6024    FORMAT (1X,A43)
C
        CALL JMT(TFIRST,IYEAR,IMONTH,DAY)
        IDAY = IDINT(DAY)
        DAY = DMOD(TFIRST,1.D0)
        CALL RADGMS(3,DAY,VORZ,IHOUR,IMIN,SEC)
        WRITE(LFNPRT,6025) IYEAR,IMONTH,IDAY,IHOUR,IMIN,SEC,TDISP
6025    FORMAT (1X,'REFERENCE EPOCH : ',I4,'-',I2,'-',I2,2X,I2,':',I2,
     1          ':',F5.2,2X,' ONE CHARACTER = ',F5.2,' MINUTES')
C
        NOBTOT = 0
        DO ISAT = 1,NSATEL(IFIL)
          DO IFRQ = 1,NFRQS
            NOBTOT = NOBTOT + NUMOBS(ISAT,IFRQ,IFIL)
          END DO
        END DO
        WRITE(LFNPRT,6501) NOBTOT
6501    FORMAT(1X,'TOTAL NUMBER OF OBSERVATIONS: ',I7,/)
C
        WRITE(LFNPRT,6026)
6026    FORMAT (1X,'SVN FRQ #OBS')
C
C DISPLAY GRAPHIC TABLE
        DO 6000 ISATEL = 1,NSATEL(IFIL)
          DO 6010 IFREQ = 1,NFRQS
            WRITE (GRSTR1,'(I3,1X,A1,I1,1X)') SATNUM(ISATEL,IFIL),
     1                                        'L',IFRQS(IFREQ)
C
            DO 6020 ICHR = 1,JMARK
              GRSTR2(ICHR+1) = STRING(IFREQ,ISATEL,ICHR)
6020        CONTINUE
C
            GRSTR2(1)       = '|'
            GRSTR2(JMARK+2) = '|'
C
            WRITE(GRSTR3,'(1X,I4)') NUMOBS(ISATEL,IFREQ,IFIL)
C
            WRITE(LFNPRT,6011) GRSTR1,GRSTR3,
     1                         (GRSTR2(ICHR),ICHR=1,JMARK+2)
6011        FORMAT(1X,A7,A5,255A)
C
6010      CONTINUE
6000    CONTINUE
C
C DISPLAY EPOCH NUMBERS
C ---------------------
C FIRST AND LAST EPOCH
      IF (IEP1(IFIL) .LT. 10) THEN
        WRITE (EPOSTR(INDEX0(IFIL)+1:INDEX0(IFIL)+1),'(I1)') IEP1(IFIL)
      ELSE IF (IEP1(IFIL) .LT. 100) THEN
        WRITE (EPOSTR(INDEX0(IFIL)+1:INDEX0(IFIL)+2),'(I2)') IEP1(IFIL)
      ELSE IF (IEP1(IFIL) .LT. 1000) THEN
        WRITE (EPOSTR(INDEX0(IFIL)+1:INDEX0(IFIL)+3),'(I3)') IEP1(IFIL)
      ELSE IF (IEP1(IFIL) .LT. 10000) THEN
        WRITE (EPOSTR(INDEX0(IFIL)+1:INDEX0(IFIL)+4),'(I4)') IEP1(IFIL)
      ENDIF
C
      IF (IEP2(IFIL) .LT. 10) THEN
        WRITE (EPOSTR(LEPMRK+1:LEPMRK+1),'(I1)') IEP2(IFIL)
      ELSE IF (IEP2(IFIL) .LT. 100) THEN
        WRITE (EPOSTR(LEPMRK:LEPMRK+1),'(I2)') IEP2(IFIL)
      ELSE IF (IEP2(IFIL) .LT. 1000) THEN
        WRITE (EPOSTR(LEPMRK-1:LEPMRK+1),'(I3)') IEP2(IFIL)
      ELSE IF (IEP2(IFIL) .LT. 10000) THEN
        WRITE (EPOSTR(LEPMRK-2:LEPMRK+1),'(I4)') IEP2(IFIL)
      ENDIF
C
      DO 2001 JCHR=1,JMARK+2
        EPODSP(JCHR+1) = EPOSTR(JCHR:JCHR)
2001  CONTINUE
      WRITE (LFNPRT,'(13X,255A)') (EPODSP(JCHR),JCHR=1,JMARK+2)
C
      WRITE (LFNPRT,'(//)')
C
2000  CONTINUE
C
C ADD MANEUVER SUMMARY
C --------------------
      CALL GTFLNA(0,'REPOSSUM',SUMFIL,IRCSUM)
      IF (IRCSUM.EQ.0) THEN
        CALL OPNFIL(LFNLOC,SUMFIL,'UNKNOWN','FORMATTED',' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNLOC,IOSTAT,SUMFIL,PGNAME)
      ENDIF
C
      IF (NUMMAN.GT.0) THEN
        WRITE(LFNPRT,'(2(1X,A,/))')
     1  'REPOSITIONING SUMMARY', '---------------------'

        IF (IRCSUM.EQ.0) WRITE(LFNLOC,'(2(1X,A,/))')
     1  'REPOSITIONING SUMMARY', '---------------------'
      ENDIF
C
      ALLOCATE(TOTMAN(NCHAR,4),STAT=IRC)
      CALL ALCERR(IRC,'TOTMAN',(/NCHAR,4/),PGNAME)
C
      ALLOCATE(CHRFIL(2,NFIL),STAT=IRC)
      CALL ALCERR(IRC,'IDXFIL',(/2,NFIL/),PGNAME)
C
      ALLOCATE(IDXFIL(NFIL),STAT=IRC)
      CALL ALCERR(IRC,'IDXFIL',(/NFIL/),PGNAME)
C
      DO IMAN=1,NUMMAN
C
C WRITE A HEADER FOR THE MANEUVER
        CALL TIMST2(1,1,TIMMAN(IMAN),EPOSTR)
        CALL SVN2CHR(SATMAN(IMAN),ISVN,SVNCHR)
        WRITE(LFNPRT,'(1X,2A,I2.2,2A)')
     1    'REPOSITIONING EVENT FOR SATELLITE: ',SVNCHR,ISVN,
     2    '   AT EPOCH ',TRIM(EPOSTR)
        IF (IRCSUM.EQ.0) WRITE(LFNLOC,'(1X,2A,I2.2,2A)')
     1    'REPOSITIONING EVENT FOR SATELLITE: ',SVNCHR,ISVN,
     2    '   AT EPOCH ',TRIM(EPOSTR)
C
        CALL TIMST2(1,2,WINMAN(1:2,IMAN),EPOSTR)
        WRITE(LFNPRT,'(1X,3A,F5.2,A,/)')
     1    'DISPLAYING EPOCHS ',TRIM(EPOSTR),
     2    '   ONE CHARACTER = ',DBLE(IDTMAN)/60d0,' MINUTES'
        IF (IRCSUM.EQ.0) WRITE(LFNLOC,'(1X,3A,F5.2,A,/)')
     1    'DISPLAYING EPOCHS ',TRIM(EPOSTR),
     2    '   ONE CHARACTER = ',DBLE(IDTMAN)/60d0,' MINUTES'
C
        WRITE(LFNPRT,'(1X,A)') 'FILENAME      FRQ'
        IF (IRCSUM.EQ.0) WRITE(LFNLOC,'(1X,A)') 'FILENAME      FRQ'
C
C FIND FIRST AND LAST CHARACTER FOR EACH FILE
        CHRFIL=0
C
        IC1=NCHAR
        IC2=0
C
        DO IFIL=1,NFIL
          DO IFRQ=1,NFRQS
            DO ICHR=1,NCHAR
              IF (OBSMAN(ICHR,IFRQ,IFIL,IMAN).NE.0) THEN
                IF (CHRFIL(1,IFIL).EQ.0) THEN
                  CHRFIL(1,IFIL)=ICHR
                ELSEIF (CHRFIL(1,IFIL).GT.ICHR) THEN
                  CHRFIL(1,IFIL)=ICHR
                ENDIF
                EXIT
              ENDIF
            ENDDO
C
            IF (CHRFIL(1,IFIL).EQ.0) CYCLE
            DO ICHR=NCHAR,1,-1
              IF (OBSMAN(ICHR,IFRQ,IFIL,IMAN).NE.0) THEN
                IF (CHRFIL(2,IFIL).EQ.0) THEN
                  CHRFIL(2,IFIL)=ICHR
                ELSEIF (CHRFIL(2,IFIL).GT.ICHR) THEN
                  CHRFIL(2,IFIL)=ICHR
                ENDIF
                EXIT
              ENDIF
            ENDDO
          ENDDO
C
          IF (CHRFIL(1,IFIL).EQ.0) CYCLE
          IF (CHRFIL(1,IFIL).LT.IC1) IC1=CHRFIL(1,IFIL)
          IF (CHRFIL(2,IFIL).GT.IC2) IC2=CHRFIL(2,IFIL)
        ENDDO
C
C REORDER THE FILES FOR PLOTTING
        IDXFIL(1:NFIL)=(/ (II,II=1,NFIL) /)
C
        SORTED=.FALSE.
        DO WHILE (.NOT. SORTED)
          SORTED=.TRUE.
          DO JFIL=1,NFIL-1
            IFIL1=IDXFIL(JFIL)
            IFIL2=IDXFIL(JFIL+1)
            IF (CHRFIL(2,IFIL1).GT.CHRFIL(2,IFIL2)) THEN
              IDXFIL(JFIL)=IFIL2
              IDXFIL(JFIL+1)=IFIL1
              SORTED=.FALSE.
            ENDIF
          ENDDO
        ENDDO
C
        SORTED=.FALSE.
        DO WHILE (.NOT. SORTED)
          SORTED=.TRUE.
          DO JFIL=1,NFIL-1
            IFIL1=IDXFIL(JFIL)
            IFIL2=IDXFIL(JFIL+1)
            IF (CHRFIL(1,IFIL1).GT.CHRFIL(1,IFIL2)) THEN
              IDXFIL(JFIL)=IFIL2
              IDXFIL(JFIL+1)=IFIL1
              SORTED=.FALSE.
            ENDIF
          ENDDO
        ENDDO
C
        DO JFIL=1,NFIL
          IFIL=IDXFIL(JFIL)
          IF (CHRFIL(1,IFIL).EQ.0) IDXFIL(JFIL)=0
        ENDDO
C
C WRITE ONE LINE PER FILE
        TOTMAN=0
        DO JFIL=1,NFIL
          IFIL=IDXFIL(JFIL)
          IF (IFIL.EQ.0) CYCLE
C
          FILSTR=FILNAM(1,IFIL)
          CALL STRIPDIR(FILSTR)
          DO IFRQ=1,NFRQS
C
C PREPARE THE STRING INDICATING THE OBSERVATIONS
            EPOSTR=''
            DO ICHR=1,NCHAR
              IF (OBSMAN(ICHR,IFRQ,IFIL,IMAN).EQ.1) THEN
                EPOSTR(ICHR:ICHR)='-'
                TOTMAN(ICHR,2)=TOTMAN(ICHR,2)+1
              ELSEIF (OBSMAN(ICHR,IFRQ,IFIL,IMAN).EQ.2) THEN
                EPOSTR(ICHR:ICHR)='*'
                TOTMAN(ICHR,1)=TOTMAN(ICHR,1)+1
              ENDIF
            ENDDO
C
C ADD AMBIGUTY FLAGS
            DO IAMB=1,NUMAMB(IFIL)
              IF (AMBSAT(IAMB,IFIL).NE.SATMAN(IMAN)) CYCLE
              AMBTIM=TF(IFIL)+(AMBIEP(IAMB,IFIL)-1)*IDELTT(IFIL)/86400D0
              IF (AMBTIM.LT.WINMAN(1,IMAN)) CYCLE
              IF (AMBTIM.GT.WINMAN(2,IMAN)) CYCLE
              ICHR=NINT((AMBTIM-WINMAN(1,IMAN))*86400d0/IDTMAN)+1
              TOTMAN(ICHR,3)=TOTMAN(ICHR,3)+1
              IF (PLTAMB.EQ.1) THEN
                IF (AMBSOL(IAMB,IFIL).EQ.0) THEN
                  EPOSTR(ICHR:ICHR)='A'
                ELSE
                  EPOSTR(ICHR:ICHR)='a'
                  TOTMAN(ICHR,4)=TOTMAN(ICHR,4)+1
                ENDIF
              ENDIF
            ENDDO
C
            WRITE(LFNPRT,'(1X,A,2X,A,I1,3A)')
     1            FILSTR(1:12),'L',IFRQS(IFRQ),' |',EPOSTR(IC1:IC2),'|'
            IF (IRCSUM.EQ.0) WRITE(LFNLOC,'(1X,A,2X,A,I1,3A)')
     1            FILSTR(1:12),'L',IFRQS(IFRQ),' |',EPOSTR(IC1:IC2),'|'
          ENDDO
        ENDDO
C
        EPOSTR=''
        WRITE(LFNPRT,'(18X,3A)') '|',EPOSTR(IC1:IC2),'|'
        IF (IRCSUM.EQ.0) WRITE(LFNLOC,'(18X,3A)')'|',EPOSTR(IC1:IC2),'|'
C
C TOTAL NUMBER OF GOOD/BAD OBSERVATIONS
        DO II=1,3
          EPOSTR=''
          DO ICHR=1,NCHAR
            IF (TOTMAN(ICHR,II).GE.1)
     1        WRITE(EPOSTR(ICHR:ICHR),'(I1)') TOTMAN(ICHR,II)
          ENDDO
          IF (II.EQ.1) FILSTR='TOTAL OBSERV. OK |'
          IF (II.EQ.2) FILSTR='TOTAL OBSERV. BAD|'
          IF (II.EQ.3) FILSTR='TOTAL #AMBIG.    |'
          IF (II.EQ.4) FILSTR='TOTAL #FIXED  AMB|'
          WRITE(LFNPRT,'(1X,3A)')
     1          TRIM(FILSTR),EPOSTR(IC1:IC2),'|'
          IF (IRCSUM.EQ.0) WRITE(LFNLOC,'(1X,3A)')
     1          TRIM(FILSTR),EPOSTR(IC1:IC2),'|'
        ENDDO
C
C ADD TIME LABELS
        EPOSTR=''
C
        IDXMAN=NINT((TIMMAN(IMAN)-WINMAN(1,IMAN))*86400d0/IDTMAN)+1
        EPOSTR(IDXMAN:IDXMAN)='M'
C
        NIDX=10
        DO WHILE(IDXMAN-NIDX-2.GT.0 .AND. IDXMAN+NIDX+1.LT.NCHAR)
          I1=IDXMAN+NIDX-2
          I2=IDXMAN+NIDX+1
          WRITE(EPOSTR(I1:I2),'(I4)')  NIDX
          IF (NIDX.LT.100) I1=I1+1
          IF (NIDX.LT.10)  I1=I1+1
          EPOSTR(I1:I1)='+'
          I1=IDXMAN-NIDX-2
          I2=IDXMAN-NIDX+1
          WRITE(EPOSTR(I1:I2),'(I4)') -NIDX
          NIDX=NIDX+10
        ENDDO
C
        WRITE(LFNPRT,'(/,1X,3A,//)')
     1        'REL. EPOCH NUM.  |',EPOSTR(IC1:IC2),'|'
        IF (IRCSUM.EQ.0) WRITE(LFNLOC,'(/,1X,3A,//)')
     1        'REL. EPOCH NUM.  |',EPOSTR(IC1:IC2),'|'
C
      ENDDO
      DEALLOCATE(TOTMAN,STAT=IRC)
      DEALLOCATE(CHRFIL,STAT=IRC)
      DEALLOCATE(IDXFIL,STAT=IRC)
C
      IF (NUMMAN.EQ.0) THEN
        CLOSE(LFNLOC,STATUS='DELETE')
      ELSE
        CLOSE(LFNLOC)
      ENDIF
C
2020  IF(LFNPRT.EQ.LFNPRF) THEN
        CLOSE(UNIT=LFNPRT)
      END IF

! Deallocate memory
! -----------------
      DEALLOCATE(title,stat=iac)
      DEALLOCATE(meatyp,stat=iac)
      DEALLOCATE(ndiff,stat=iac)
      DEALLOCATE(ambsol,stat=iac)
C
      CALL EXITRC(0)
      END
