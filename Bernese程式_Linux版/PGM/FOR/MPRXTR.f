C*
      PROGRAM MPRXTR
CC
CC NAME       :  MPRXTR
CC
CC PURPOSE    :  EXTRACT THE MOST IMPORTANT INFORMATIONS FROM THE
CC               MAUPRP OUTPUT FILE
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  L.MERVART
CC
CC CREATED    :  30-JUN-92
CC
CC CHANGES    :  12-OCT-92 : ??: DETECT "NOT ENOUGH OBS."
CC               01-DEC-92 : EB: MORE THAN 1 MAUPRP OUTPUT FILE
CC               14-APR-93 : ??: WRITE THE FILE NUMBER
CC                               NOT TO CHECK 'CLK' SLIPS
CC                               WRITE THE MAXIMUM L3 RESIDUM
CC               25-MAY-94 : ??: CHECK FOR MAXAMB ERROR AND CHECK FOR
CC                               BAD OMC COMPUTATIONS
CC               07-APR-95 : MR: 4-CHARACTER SESSIONS
CC               29-MAY-95 : TS: CHECK FOR MAXAMB PER SATELLITE
CC                               (SR UPDAMB)
CC               09-JUN-95 : TS: CORRECT CHECK FOR MAXAMB PER SATELLITE
CC                               (SR UPDAMB)
CC               17-SEP-95 : JJ: MAXFIL AND MAXSTA TO 200
CC               25-SEP-95 : CR: ARRAYS TO DETERMINE WHICH FILES
CC                               TO DELETE AND BASELINES TO FORM
CC               06-OCT-95 : MR: OPTION TO DELTE SINGLE DIFF. ONLY
CC               10-OCT-95 : JJ: RM EXTRA DECL OF FILDEL
CC               08-NOV-95 : MR: CHANGE TITLE LINE AND MINSLP SETTING
CC               11-DEC-95 : MR: CHECK ALL LINES FOR '#@#',
CC                               CORRECT CASE WITH "NO MULTIPLE AMB."
CC               29-MAR-96 : TS: ADDED OVERALL SUMMARY LINE
CC               04-FEB-97 : JJ: MR: PASS "NUMOBS" AS ARRAY TO CHKBAS
CC               24-JAN-97 : SS: MAXFIL AND MAXSTA TO 500
CC               27-SEP-01 : DI: PREVENT CRASH IF NO BASELINE FOUND
CC               27-SEP-01 : DI: USE NEW SR GTSTAB
CC               27-SEP-01 : DI: SWITCH TO NEW MENU SYSTEM
CC               30-SEP-01 : HU: INTERFACE OF GTSTAB MOVED TO I_ASTLIB
CC               06-OCT-01 : HU: INTERFACE OF GTSTAB MOVED BACK TO I_GPSLIB
CC               26-JUN-02 : RD: ADAPT NEW OUTPUT STRING FROM MAUPRP
CC               03-JUL-02 : RD: NO ABBREV. FOR 2ND STATION FOR ZD PROCESSING
CC               14-AUG-02 : RD: REPORT CORRECT NUMBER OF SLIPS (ALSO FOR CLK)
CC               25-SEP-02 : HU: REMOVE I_ASTLIB
CC               16-OCT-02 : MM: ADMIT OF NON INTEGER SESSION CHARACTERS
CC               09-DEC-02 : HU: ERROR WRITING INPUT FILE LIST CORRECTED
CC               23-JAN-03 : RD: WRITE FULL PATH INTO DELETION FILE
CC               19-FEB-03 : RD: WARNING IF RPLENVAR FAILED
CC               17-MAR-03 : RD: USE T_ABBREV FOR STATION ABBREVIATIONS
CC               23-APR-03 : HU: NULLIFY LOCAL POINTERS
CC               16-MAY-03 : HB: INITIALIZE STRUCTURE
CC               03-NOV-03 : RD: CALL SR GTABBV INSTEAD OF SR GETABB
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               28-JUN-04 : RD: USE MAXCRD FROM M_MAXDIM FOR MAXSTA
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: LFNUM.inc, COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               07-FEB-07 : AG: SUMMARY OUTPUT MODIFIED
CC               27-FEB-07 : AG: CALL DEFCON WITH PARAMETER
CC               21-MAR-07 : EB/SS: READ ISLP WITH I9 (INSTEAD OF I12)
CC               01-Apr-08 : RD: TRIM FILENAME IN DELETION FILE
CC               19-JUL-10 : SL: TAB CHARACTERS REMOVED
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               17-FEB-11 : RD: COMMON MCMSTA NOT NEEDED ANYMORE
CC               30-NOV-11 : SL: NEW TITLE STRING FOR PRITIT, M_BERN WITH ONLY
CC               10-FEB-12 : SL: KEYWORD ABBPAN CHANGED TO ABBREV
CC               11-JUN-12 : RD: MORE DIGITS FOR NUMBER OF CYCLE SLIPS
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC    1990       UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: i4b, keyValueLength,
     1                    lfnPrt, lfnErr, lfnOrb, lfn001, lfn002
      USE m_cpu,    ONLY: cpu_start
      USE m_maxdim, ONLY: maxsta => maxcrd
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE d_abbrev, ONLY: t_abbrev, init_abbrev
C
      USE s_gtabbv
      USE s_opnfil
      USE s_prflna
      USE s_readabb
      USE s_gtfile
      USE s_pritit
      USE s_sjustl
      USE s_readinpf
      USE s_opnerr
      USE s_mxinpt
      USE s_prfile
      USE s_readkeys
      USE s_rplenvar
      USE s_defcon
      USE s_exitrc
      USE s_chkbas
      USE s_opnsys
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I1    , I2    , IDEL  , IF    , IFIL  , INEW  , IOSTAT,
     1          IRC   , IRCBSL, IRCDEL, IRCODE, IREC  , ISLP  , ISNGDF,
     2          IST   , ITOLEN, ITOT  ,
     3          IZEROD, JTOT  , LEN   , MAXFIL, MINSLP, MXCFIL,
     4          NABB  , NAMB  , NCOL  , NDEL  , NDEL1 , NDEL2 , NFIL  ,
     5          NFLCOL, NFLDEL, NFLNEW, NSLP  , NSLP1 , NSLP2 , NSLPCL,
     6          NUMOBS, MAXOBS, MAXSLP, MAXAMB, MAXDEL, MMINSLP,MAXL3 ,
     7          MAXDX , MAXDY , MAXDZ , NUMOBSa(1)    , NSLPPR, NDELPR,
     8          NAMBPR
C
      REAL*8    DX    , DY    , DZ    , RESL3 , RESL3M, RMS   ,
     1          TOTLEN, MAXRMS, MAXD
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C MAXIMAL DIMENSIONS
C ------------------
      PARAMETER (MAXFIL=500)
C
C MAXFIL: MAXIMUM NUMBER OF FILES ONE OUTPUT FILE
C
C DECLARATIONS
C ------------
      TYPE(t_abbrev) abbrev
      CHARACTER*80  LINE1,LINE2,LINE3,LINE4,LINE5,FILABR,FILTMP
      CHARACTER*32  FILSUM,FILDEL,FILPAN,SNGFIL
      CHARACTER*32  BSLFIL
      CHARACTER*32  FILNAM(MAXFIL)
      CHARACTER*16  STANAM(2,MAXFIL)
      CHARACTER*8   TYPDEL(MAXFIL),DELFIL(MAXFIL),FILSTR(MAXFIL)
      CHARACTER*6   MXNFIL
      CHARACTER*4   CSESS(MAXFIL)
      CHARACTER*4   STANA4(2,MAXFIL),PROBLM(MAXFIL)
      CHARACTER*4   AB4NEW(2,MAXFIL)
      CHARACTER*2   STANA2(2,MAXFIL)
      CHARACTER*2   AB2NEW(2,MAXFIL)
      CHARACTER*6   CFILE
      REAL*8        BLGTH(MAXFIL)
      INTEGER*4     JMPMS(MAXFIL)
      INTEGER(i4b), DIMENSION(:),POINTER :: abbIdx
C
      CHARACTER(LEN=keyValueLength),SAVE                   :: DPZH
      CHARACTER(LEN=keyValueLength),SAVE                   :: DPZO
      CHARACTER(LEN=keyValueLength),SAVE                   :: DPSH
      CHARACTER(LEN=keyValueLength),SAVE                   :: DPSO
      CHARACTER(LEN=keyValueLength),SAVE                   :: EPZH
      CHARACTER(LEN=keyValueLength),SAVE                   :: EPZO
      CHARACTER(LEN=keyValueLength),SAVE                   :: EPSH
      CHARACTER(LEN=keyValueLength),SAVE                   :: EPSO
C
      CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue
C
C COMMON BLOCKS
C -------------
      COMMON/MCMFIL/MXCFIL,MXNFIL
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C INITIALIZE COMMON BLOCKS FOR MAXIMAL DIMENSIONS
C -----------------------------------------------
      MXCFIL=MAXFIL
      MXNFIL='MAXFIL'
C
      IRCODE=0
C
C DEFINE LOGICAL FILE NUMBERS
C ---------------------------
C
C NULLIFY POINTERS
C ----------------
      NULLIFY(keyValue)
      CALL INIT_ABBREV(abbrev)
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
C PRINT GENERAL TITLE
C -------------------
      CALL pritit('MPRXTR',
     1            'Extract MAUPRP program output')
C
C PRINT GENERAL FILE NAMES
C ------------------------
      NCOL=79
      CALL PRFLNA(NCOL)
C
C READ NAMES OF INPUT FILES (F-FILE) : OUTPUT FILES OF MAUPRP
C -----------------------------------------------------------
      NFLCOL=1
      CALL readkeys ('RADIO_1',keyValue,irc)
      IF (keyValue(1) .EQ. '1') THEN
        CALL GTFILE('JOBOUT',NFLCOL,MAXFIL,NFIL,FILNAM)
        CALL PRFILE('JOBOUT','',1)
      ELSE IF (keyValue(1) .EQ. '0') THEN
        CALL GTFILE('OUTPUT',NFLCOL,MAXFIL,NFIL,FILNAM)
        CALL PRFILE('OUTPUT','',1)
      END IF
C
C
C READ INPUT OPTION
C -----------------
      CALL MXINPT(ISNGDF)
C
C GET THE NAME OF THE OUTPUT FILE (SUMMARY FILE)
C ----------------------------------------------
      CALL GTFLNA(1,'SUMMARY',FILSUM,IRC)
C
C OPEN MPRXTR SUMMARY FILE (OUTPUT)
C ---------------------------------
      CALL OPNFIL(LFN001,FILSUM,'NEW','FORMATTED',' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFN001,IOSTAT,FILSUM,'MPRXTR')
C
C WRITE THE HEADER OF THE SUMMARY FILE
C ------------------------------------
      WRITE(LFN001,2000)
2000  FORMAT(1X,'SUMMARY OF THE MAUPRP OUTPUT FILE ',
     1 /,1X,33('*'),/,
     2 /,' SESS FIL OK?  ST1  ST2 L(KM)   #OBS.    RMS    DX     DY',
     3   '     DZ    #SL   #DL   #MA  MAXL3      MIN. SLIP',
     4 /,1X,104('-'))
C
C GET STATION NAME ABBREV.TABLE
C ------------------------------------
      CALL GTFLNA(0,'ABBREV ',FILPAN,IRC)
      FILABR=FILPAN
      CALL readAbb(filabr,abbrev)
      NULLIFY(abbIdx)
C
C INITIALIZE SOME VALUES
C ----------------------
      ITOT   = 0
      TOTLEN = 0.D0
      MAXOBS = 0
      MAXRMS = 0.D0
      MAXSLP = 0
      MAXDEL = 0
      MAXAMB = 0
      MAXL3  = 0
      MMINSLP= huge(i4b)
      MAXD   = 0.d0
C
C LOOP OVER ALL F-FILES
C ---------------------
      DO 1000 IFIL=1,NFIL
C
C OPEN MAUPRP OUTPUT FILE (INPUT)
C -------------------------------
        CALL OPNFIL(LFNORB,FILNAM(IFIL),'OLD','FORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNORB,IOSTAT,FILNAM(IFIL),'MPRXTR')
C
C LOOP OVER ALL FILES IN THE MAUPRP-OUTPUTFILE
C --------------------------------------------
        DO 100 IF=1,MAXFIL
C
C INITIALIZATIONS
C ---------------
          LEN=0.D0
          NUMOBS=0
          RMS=0.D0
          DX=0.D0
          DY=0.D0
          DZ=0.D0
          NSLP=0
          NDEL=0
          NAMB=0
          RESL3M=0.D0
          MINSLP=0
          JMPMS(IFIL)=0
C
C READ THE NAMES OF THE STATIONS, SESSION, FILE NUMBER, LENGTH AND FILE NAME
C --------------------------------------------------------------------------
          IZEROD=0
          DO 110 IREC=1,100000
            READ(LFNORB,1001,END=900) LINE1
1001        FORMAT(A)
            IF (LINE1(2: 4) .EQ. '#@# ') GOTO 700
            IF (LINE1(2:11) .EQ. 'STATION 1:') GOTO 112
            IF (LINE1(2:11) .EQ. 'STATION  :') GOTO 111
110       CONTINUE
C
111       IZEROD=1
112       READ(LFNORB,1001,END=900) LINE2
          READ(LFNORB,1001,END=900) LINE3
          READ(LFNORB,1001,END=900) LINE4
          READ(LFNORB,1001,END=900) LINE5
C
          ITOT=ITOT+1
C
C CHECK MAXIMUM DIMENSION MAXFIL
          IF (ITOT.GT.MAXFIL) THEN
            WRITE(LFNERR,901) ITOT,MAXFIL
901         FORMAT(/,' *** PG MPRXTR: TOO MANY SINGLE DIFF FILES IN ',
     1             /,16X,'THE MAUPRP OUTPUT FILE(S)',
     2             /,16X,'NUMBER OF FILES   >=',I4,
     3             /,16X,'MAX. NUMBER ALLOWED:',I4,/)
            CALL EXITRC(2)
          ENDIF
C
          READ(LINE1,1011) STANAM(1,ITOT)
          READ(LINE2,1011) STANAM(2,ITOT)
1011      FORMAT (12X,A16)
          READ(LINE1,1012) CSESS(ITOT)
1012      FORMAT(69X,A4)
          READ(LINE2,1014) CFILE
1014      FORMAT (67X,A6)
          IF (IZEROD.EQ.0) THEN
            READ (LINE4,1013) BLGTH(ITOT)
1013        FORMAT (22X,F14.3)
            LEN = DNINT(BLGTH(ITOT)/1000.)
            READ (LINE5,1015) SNGFIL
1015        FORMAT (23X,A)
          ELSE
            LEN = 0D0
            READ (LINE4,1015) SNGFIL
          ENDIF
C
C GET STATION ABBREVIATION FOR THE TWO SITES
C ------------------------------------------
          DO 510 IST=1,2
            IF (IST.EQ.2.AND.LEN_TRIM(STANAM(2,ITOT)).EQ.0) THEN
              STANA4(IST,ITOT)=' '
              STANA2(IST,ITOT)=' '
              CYCLE
            ENDIF
            CALL GTABBV(0,STANAM(IST,ITOT),1,FILABR,ABBREV,NABB,ABBIDX)
C
            IF (NABB.EQ.0) THEN
              WRITE(LFNERR,902) STANAM(IST,ITOT),TRIM(FILPAN)
902           FORMAT(/,' ### PG MPRXTR: STATION ABBREVIATIONS ',
     .                                 'NOT FOUND',
     1             /,16X,'STATION NAME     : ',A,
     2             /,16X,'ABBREVIATION FILE: ',A,/,
     3             /,16X,'FIRST 4 CHARACTERS OF STATION USED',/)
              STANA4(IST,ITOT)=STANAM(IST,ITOT)(1:4)
              STANA2(IST,ITOT)=STANAM(IST,ITOT)(1:2)
              IRCODE=1
            ELSE IF (NABB.GT.1) THEN
              WRITE(LFNERR,904) STANAM(IST,ITOT),TRIM(FILPAN)
904           FORMAT(/,' ### PG MPRXTR: MORE THAN ONE STATION ',
     .                                 'ABBREVIATIONS FOUND',
     1             /,16X,'STATION NAME     : ',A,
     2             /,16X,'ABBREVIATION FILE: ',A,/)
              STANA4(IST,ITOT)=ABBREV%ABB(ABBIDX(1))%STAAB4
              STANA2(IST,ITOT)=ABBREV%ABB(ABBIDX(1))%STAAB2
            ELSE
              STANA4(IST,ITOT)=ABBREV%ABB(ABBIDX(1))%STAAB4
              STANA2(IST,ITOT)=ABBREV%ABB(ABBIDX(1))%STAAB2
            ENDIF
510       CONTINUE
C
C READ THE RESULTS OF THE TRIPLE DIFFERENCE SOLUTION
C --------------------------------------------------
          DO 120 IREC=1,100000
            READ(LFNORB,1001,END=900) LINE1
            IF (LINE1(2: 4) .EQ. '#@# ') GOTO 700
            IF (LINE1(2:17) .EQ. 'NOT ENOUGH OBS. ') GOTO 125
            IF (LINE1(2:22) .EQ. '#OBS. USED FOR EPOCH ') GOTO 121
120       CONTINUE
121       READ(LFNORB,1001,END=900) LINE2
          READ(LFNORB,1001,END=900) LINE3
          READ(LFNORB,1001,END=900) LINE3
          READ(LFNORB,1001,END=900) LINE4
          READ(LFNORB,1001,END=900) LINE5
          READ(LINE1,1021) NUMOBS
1021      FORMAT(33X,I12)
          READ(LINE2,1022) RMS
          READ(LINE3,1022) DX
          READ(LINE4,1022) DY
          READ(LINE5,1022) DZ
1022      FORMAT(33X,F12.3)
C
125       CONTINUE
C
C READ THE NUMBER OF CYCLE SLIPS
C ------------------------------
          DO 130
            READ(LFNORB,1001,END=900) LINE1
            IF (LINE1(2: 4) .EQ. '#@# ') GOTO 700
            IF (LINE1(2:22) .EQ. 'NUMBER OF SLIPS IN L1') GO TO 131
            IF (LINE1(2:21) .EQ. '*** SR UPDAMB: TOO M') GOTO 90
130       CONTINUE
131       READ(LFNORB,1001,END=900) LINE2
          READ(LINE1,1031) NSLP1
          READ(LINE2,1031) NSLP2
1031      FORMAT(23X,I7)
          NSLP = AMAX0(NSLP1,NSLP2)
C
          DO 135 IREC=1,100000
            READ(LFNORB,1001,END=900) LINE1
            IF (LINE1(2: 4) .EQ. '#@# ') GOTO 700
            IF (LINE1(8:10) .EQ. '---') GO TO 136
135       CONTINUE
136       CONTINUE
C
          NSLPCL = 0
          RESL3M = 0.D0
          MINSLP = 99999999
          DO 138 IREC=1,100000
            READ(LFNORB,1001,END=900) LINE1
            IF (LINE1(2: 4) .EQ. '#@# ') GOTO 700
            IF (LINE1(8:10) .EQ. '---') GO TO 139
            IF (LINE1(8:10) .EQ. 'CLK') THEN
              NSLPCL = NSLPCL + 1
            ELSE IF (LINE1(8:10) .EQ. 'JMP') THEN
              NSLPCL = NSLPCL + 1
              JMPMS(ITOT) = JMPMS(ITOT)+1
            ELSE IF (LINE1(8:10) .EQ. 'DUA') THEN
              READ (LINE1,'(55X,F6.3)') RESL3
              RESL3M = DMAX1(DABS(RESL3),RESL3M)
C READ ISLP WITH I9 (TO KEEP ASSIGNED VALUE BELOW 2**31)
              READ (LINE1,'(33X,I9)') ISLP
              IF (IABS(ISLP).NE.0 .AND. IABS(ISLP).LT.MINSLP)
     1          MINSLP=IABS(ISLP)
            END IF
138       CONTINUE
139       CONTINUE
C This line is nonsense because for CLK events ALL satellites are counted
C for NSLP. It means the correct line is NSLP=NSLP-NSLPCL/2*NSAT but NSAT is
C not available in the MAUPRP in any case...
C          NSLP = NSLP - NSLPCL/2
          IF (MINSLP.EQ.99999999) MINSLP=0
C
C READ THE NUMBER OF MARKED AREAS
C -------------------------------
          DO 140 IREC=1,100000
            READ(LFNORB,1001,END=900) LINE1
            IF (LINE1(2: 4) .EQ. '#@# ') GOTO 700
            IF (LINE1(2:23) .EQ. 'NUMBER OF MARKED AREAS') GO TO 141
140       CONTINUE
141       READ(LFNORB,1001,END=900) LINE2
          READ(LINE1,1041) NDEL1
          READ(LINE2,1041) NDEL2
1041      FORMAT(30X,I5)
          NDEL = AMAX0(NDEL1,NDEL2)
C
C READ THE NUMBER OF MULTIPLE AMPIGUITIES
C ---------------------------------------
          DO 150 IREC=1,100000
            READ(LFNORB,1001,END=900) LINE1
            IF (LINE1(2: 4) .EQ. '#@# ') GOTO 700
            IF (LINE1(2:21) .EQ. 'MULTIPLE AMBIGUITIES') GO TO 151
150       CONTINUE
151       READ(LFNORB,'(/)')
          READ(LFNORB,1001,END=900) LINE1
          IF (LINE1(2: 4) .EQ. '#@# ') GOTO 700
          IF (LINE1(2:21) .EQ. 'NO MULTIPLE AMBIGUIT') GO TO 90
          READ(LFNORB,'(/)')
          DO 155 NAMB=1,100000
            READ(LFNORB,1001,END=900) LINE1
            IF (LINE1(2: 4) .EQ. '#@# ') GOTO 700
            IF (LINE1(2:10) .EQ. '         ') GO TO 156
155       CONTINUE
156       NAMB = NAMB - 1
C
C READ STATUS LINE "#@#"
C ----------------------
90        CONTINUE
C
          DO 160 IREC=1,100000
            READ(LFNORB,1001,END=900) LINE1
            IF (LINE1(2:4) .EQ. '#@# ') GOTO 700
160       CONTINUE
C
C READ STATUS LINE
700       READ(LINE1,701) PROBLM(ITOT),FILSTR(ITOT)
701       FORMAT(5X,A4,1X,A8)
C
C WRITE STATUS LINE TO MPRXTR OUTPUT FILE
C ---------------------------------------
          WRITE(LFNPRT,822) PROBLM(ITOT),STANA2(1,ITOT),STANA2(2,ITOT),
     1          STANA4(1,ITOT),STANA4(2,ITOT),FILSTR(ITOT),CSESS(ITOT)
822       FORMAT(' #@#',1X,A4,1X,A2,1X,A2,1X,A4,1X,A4,1X,A8,1X,A4)
C
C WRITE ONE LINE OF THE SUMMARY FILE
C ----------------------------------
          NSLPPR=NSLP
          NDELPR=NDEL
          NAMBPR=NAMB
          IF (NSLPPR.GT.99999) NSLPPR = 99999
          IF (NDELPR.GT.99999) NDELPR = 99999
          IF (NAMBPR.GT.99999) NAMBPR = 99999
          WRITE(LFN001,2001,IOSTAT=IOSTAT) CSESS(ITOT),ITOT,
     1          PROBLM(ITOT),STANA4(1,ITOT),STANA4(2,ITOT),LEN,
     2          NUMOBS,NINT(RMS*1d3),NINT(DX*1d3),NINT(DY*1d3),
     3          NINT(DZ*1d3),NSLPPR,NDELPR,NAMBPR,NINT(RESL3M*1d3),
     4          MINSLP
2001      FORMAT(1X,A4,I4,1X,A4,2(1X,A4),I5,I8,4I7,3I6,I7,I15)
C
C UPDATE "MAXS"
C ---------------
          TOTLEN = TOTLEN+LEN*1.D0
          IF (MAXOBS < NUMOBS) MAXOBS = NUMOBS
          IF (MAXRMS < RMS)    MAXRMS = RMS
          IF (MAXD   < DSQRT(DX**2+DY**2+DZ**2)) THEN
            MAXD  = DSQRT(DX**2+DY**2+DZ**2)
            MAXDX = NINT(DX*1d3)
            MAXDY = NINT(DY*1d3)
            MAXDZ = NINT(DZ*1d3)
          ENDIF
          IF (MAXSLP < NSLP)   MAXSLP = NSLP
          IF (MAXDEL < NDEL)   MAXDEL = NDEL
          IF (MAXAMB < NAMB)   MAXAMB = NAMB
          IF (MAXL3  < NINT(RESL3M*1d3)) MAXL3  = NINT(RESL3M*1d3)
          IF (MMINSLP> MINSLP) MMINSLP= MINSLP
C
C SEARCH FOR NEXT SINGLE DIFF. FILE IN MAUPRP OUTPUT
C --------------------------------------------------
100     CONTINUE
C
C CLOSE MAUPRP OUTPUT FILE
C ------------------------
900     CONTINUE
        CLOSE (LFNORB)
C
C NEXT MAUPRP OUTPUT FILE
C -----------------------
1000  CONTINUE
C
C WRITE OVERALL SUMMARY LINE
C --------------------------
      IF (ITOT.GT.0) THEN
        ITOLEN = IDNINT(TOTLEN/ITOT)
        NSLPPR=MAXSLP
        NDELPR=MAXDEL
        NAMBPR=MAXAMB
        IF (NSLPPR.GT.99999) NSLPPR = 99999
        IF (NDELPR.GT.99999) NDELPR = 99999
        IF (NAMBPR.GT.99999) NAMBPR = 99999
        WRITE(LFN001,2002,IOSTAT=IOSTAT) ITOT,ITOLEN,MAXOBS,
     1              NINT(MAXRMS*1d3),MAXDX,MAXDY,MAXDZ,NSLPPR,NDELPR,
     2              NAMBPR,MAXL3,MMINSLP
2002    FORMAT(1X,104('-'),/,
     1       1X,'Tot:',I4,14X,I6,I8,4I7,3I6,I7,I15)
C
C REPORT MS-JUMPS
C ---------------
        LINE1=' MS-JUMPS FOUND FOR STATIONS:       (NUMBER OF EVENTS)'
        DO JTOT=1,ITOT
          IF (JMPMS(JTOT).EQ.0) CYCLE
          IF (LINE1(2:9).EQ.'MS-JUMPS') THEN
            WRITE(LFN001,'(/,A,/,1X,29("-"),/)') LINE1
            LINE1=' '
            I1=1
          ENDIF
          I2=I1+23
          WRITE(LINE1(I1:I2),2003) STANAM(1,JTOT),' (',JMPMS(JTOT)/2,')'
2003      FORMAT(1X,2A,I4,A)
          I1=LEN_TRIM(LINE1)+3
          IF (I1+23.GT.80) THEN
            WRITE(LFN001,'(A)') LINE1
            LINE1=' '
            I1=1
          ENDIF
        ENDDO
        IF (LINE1(2:9).NE.'MS-JUMPS'.AND.LEN_TRIM(LINE1).GT.0) THEN
          WRITE(LFN001,'(A)') LINE1
        ENDIF
      ELSE
        WRITE(LFNERR,903)
 903    FORMAT(/,' *** PG MPRXTR: NO BASELINE FOUND',/)
        CALL EXITRC(2)
      ENDIF
      CLOSE (LFN001)
C
C CHECK BASELINES TO DETERMINE WHAT STATIONS TO DELETE
C ----------------------------------------------------
      CALL CHKBAS(ISNGDF,ITOT,STANAM,STANA4,STANA2,CSESS,PROBLM,
     1            BLGTH,NUMOBSa,NFLDEL,TYPDEL,DELFIL,
     2            NFLNEW,AB4NEW,AB2NEW,IRC)
      IF (IRCODE.LT.IRC) IRCODE=IRC
C
C WRITE FILES TO BE DELETED
C -------------------------
      CALL GTFLNA(0,'DELFIL ',FILDEL,IRCDEL)
      IF (IRCDEL.EQ.0) THEN
        CALL OPNFIL(LFN002,FILDEL,'UNKNOWN','FORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFN002,IOSTAT,FILDEL,'MPRXTR')
C
C       GET PATH AND EXTENSIONS
        CALL readkeys ('DPZH',keyValue,irc)
        IF (irc == 0) THEN
          DPZH = keyValue(1)
          CALL SJUSTL(DPZH)
          CALL RPLENVAR(1,DPZH)
        ELSE
          WRITE(lfnerr,'(A,/)')
     1      ' *** PG MPRXTR: Error reading keyword "DPZH"'
          CALL exitrc(2)
        ENDIF

        CALL readkeys ('EPZH',keyValue,irc)
        IF (irc == 0) THEN
          EPZH = keyValue(1)
        ELSE
          WRITE(lfnerr,'(A,/)')
     1      ' *** PG MPRXTR: Error reading keyword "EPZH"'
          CALL exitrc(2)
        ENDIF

        CALL readkeys ('DPZO',keyValue,irc)
        IF (irc == 0) THEN
          DPZO = keyValue(1)
          CALL SJUSTL(DPZO)
          CALL RPLENVAR(1,DPZO)
        ELSE
          WRITE(lfnerr,'(A,/)')
     1      ' *** PG MPRXTR: Error reading keyword "DPZO"'
          CALL exitrc(2)
        ENDIF

        CALL readkeys ('EPZO',keyValue,irc)
        IF (irc == 0) THEN
          EPZO = keyValue(1)
        ELSE
          WRITE(lfnerr,'(A,/)')
     1      ' *** PG MPRXTR: Error reading keyword "EPZO"'
          CALL exitrc(2)
        ENDIF

        CALL readkeys ('DPSH',keyValue,irc)
        IF (irc == 0) THEN
          DPSH = keyValue(1)
          CALL SJUSTL(DPSH)
          CALL RPLENVAR(1,DPSH)
        ELSE
          WRITE(lfnerr,'(A,/)')
     1      ' *** PG MPRXTR: Error reading keyword "DPSH"'
          CALL exitrc(2)
        ENDIF

        CALL readkeys ('EPSH',keyValue,irc)
        IF (irc == 0) THEN
          EPSH = keyValue(1)
        ELSE
          WRITE(lfnerr,'(A,/)')
     1      ' *** PG MPRXTR: Error reading keyword "EPSH"'
          CALL exitrc(2)
        ENDIF

        CALL readkeys ('DPSO',keyValue,irc)
        IF (irc == 0) THEN
          DPSO = keyValue(1)
          CALL SJUSTL(DPSO)
          CALL RPLENVAR(1,DPSO)
        ELSE
          WRITE(lfnerr,'(A,/)')
     1      ' *** PG MPRXTR: Error reading keyword "DPSO"'
          CALL exitrc(2)
        ENDIF

        CALL readkeys ('EPSO',keyValue,irc)
        IF (irc == 0) THEN
          EPSO = keyValue(1)
        ELSE
          WRITE(lfnerr,'(A,/)')
     1      ' *** PG MPRXTR: Error reading keyword "EPSO"'
          CALL exitrc(2)
        ENDIF
C
        DO IDEL=1,NFLDEL
          IF (TYPDEL(IDEL)=='PZERO') THEN
            FILTMP=TRIM(DPZH)//TRIM(DELFIL(IDEL))//'.'//TRIM(EPZH)
            WRITE(LFN002,'(A)') TRIM(FILTMP)
            FILTMP=TRIM(DPZO)//TRIM(DELFIL(IDEL))//'.'//TRIM(EPZO)
            WRITE(LFN002,'(A)') TRIM(FILTMP)
          ELSEIF (TYPDEL(IDEL)=='PSING') THEN
            FILTMP=TRIM(DPSH)//TRIM(DELFIL(IDEL))//'.'//TRIM(EPSH)
            WRITE(LFN002,'(A)') TRIM(FILTMP)
            FILTMP=TRIM(DPSO)//TRIM(DELFIL(IDEL))//'.'//TRIM(EPSO)
            WRITE(LFN002,'(A)') TRIM(FILTMP)
          ENDIF
        ENDDO
C
        IF (NFLDEL.GT.0) THEN
          CLOSE(UNIT=LFN002)
        ELSE
          CLOSE(UNIT=LFN002,STATUS='DELETE')
        ENDIF
      ENDIF
C
      CALL GTFLNA(0,'BSLFIL ',BSLFIL,IRCBSL)
      IF (IRCBSL.EQ.0) THEN
        CALL OPNFIL(LFN002,BSLFIL,'UNKNOWN','FORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFN002,IOSTAT,BSLFIL,'MPRXTR')
C
        DO 2020 INEW=1,NFLNEW
          WRITE(LFN002,'(A4,2X,A4,2X,A2,2X,A2)') AB4NEW(1,INEW),
     1             AB4NEW(2,INEW),AB2NEW(1,INEW),AB2NEW(2,INEW)
2020    CONTINUE
C
        IF (NFLNEW.GT.0) THEN
          CLOSE(UNIT=LFN002)
        ELSE
          CLOSE(UNIT=LFN002,STATUS='DELETE')
        ENDIF
      ENDIF
C
      CALL EXITRC(IRCODE)
      END
