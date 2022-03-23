C*
      PROGRAM SETDAY
CC
CC NAME         : SETDAY
CC
CC PURPOSE      : PREPARE COMMAND FILE TO SET THE DAY OF THE YEAR
CC                AND THE YEAR INTO SYMBOLS
CC
CC REMARKS      : NUMBER OF DAYS TO ADD TO DAY OF THE YEAR BEFORE STORING
CC                ARE READ FROm THE INPUT FILE.
CC                MINUSDAY < 0: MINUSDAY=MINUSDAY
CC                MINUSDAY > 0: MINUSDAY=DAY OF YEAR
CC
CC                FILE 'SETDAY.COM' WILL CONTAIN COMMAND TO SET THE
CC                SYMBOLS
CC
CC                $ DAYWEEK == d
CC                $ GPSWEEK == dddd
CC                $ DAYYEAR == ddd
CC                $ DAYYM   == ddd-1
CC                $ DAYYM2  == ddd-2
CC                $ DAYYM3  == ddd-3
CC                $ DAYYM4  == ddd-4
CC                $ DAYYM5  == ddd-5
CC                $ DAYYM6  == ddd-6
CC                $ DAYYM30 == ddd-30
CC                $ DAYYP   == ddd+1
CC                $ DAYYP2  == ddd+2
CC                $ DAYYP3  == ddd+3
CC                $ DAYYP6  == ddd+6
CC                $ YEAR    == yy
CC                $ YEAR4   == yyyy
CC                $ YEARM   == yy (DAYYM)
CC                $ YEARM2  == yy (DAYYM2)
CC                $ YEARM3  == yy (DAYYM3)
CC                $ YEARM4  == yy (DAYYM4)
CC                $ YEARM5  == yy (DAYYM5)
CC                $ YEARM6  == yy (DAYYM6)
CC                $ YEARP   == yy (DAYYP)
CC                $ YEARP2  == yy (DAYYP2)
CC                $ YEARP3  == yy (DAYYP2)
CC                $ YEARP6  == yy (DAYYP6)
CC                $ DAY     == dd
CC                $ DAYM    == dd-1
CC                $ DAYM2   == dd-2
CC                $ DAYM3   == dd-3
CC                $ DAYM4   == dd-4
CC                $ DAYM5   == dd-5
CC                $ DAYM6   == dd-6
CC                $ DAYM30  == dd-30
CC                $ DAYP    == dd+1
CC                $ DAYP2   == dd+2
CC                $ DAYP3   == dd+3
CC                $ DAYP6   == dd+6
CC                $ MONTH   == mm
CC                $ MONTHM  == mm (DAYYM)
CC                $ MONTHM2 == mm (DAYYM2)
CC                $ MONTHM3 == mm (DAYYM3)
CC                $ MONTHM4 == mm (DAYYM4)
CC                $ MONTHM5 == mm (DAYYM5)
CC                $ MONTHM6 == mm (DAYYM6)
CC                $ MONTHM30== mm (DAYYM30)
CC                $ MONTHP  == mm (DAYYP)
CC                $ MONTHP2 == mm (DAYYP2)
CC                $ MONTHP3 == mm (DAYYP2)
CC                $ MONTHP6 == mm (DAYYP6)
CC                $ GPSUTC  == nn
CC                $ MJD     == ddddd
CC
CC RESTRICTIONS : ***  VAX/VMS AND UNIX VERSION, DOS NOT CHECKED ***
CC
CC AUTHOR       : W. GURTNER
CC
CC CREATED      : 08-OCT-90
CC
CC CHANGES      : 11-MAY-92 : ??: DAY OF THE WEEK, GPSWEEK ADDED
CC                18-JUN-92 : ??: DAY AFTER ADDED
CC                04-JAN-93 : ??: MINUSDAY < 0: MINUSDAY=MINUSDAY
CC                                MINUSDAY > 0: MINUSDAY=DAY OF YEAR
CC                22-NOV-93 : ??: DAYP6 ADDED
CC                12-AUG-94 : MR: CALL EXITRC
CC                05-JAN-95 : EB: POSSIBILITY TO FORCE THE YEAR IN
CC                                THE OPT FILE
CC                27-APR-95 : RW: ADD DAY-3,DAY-4,DAY-5,DAY-6
CC                02-OCT-95 : WG: ADD 4-CHARACTER YEAR
CC                04-OCT-95 : WG: GET GPS-UTC FROM POLE FILE
CC                03-JAN-96 : TS: ADDED DAY+2 VARIABLES
CC                10-MAY-96 : TS: ADDED MJD VARIABLE
CC                05-JUN-96 : LM: UNIX + VMS
CC                17-JUN-96 : MR: USE EXITRC, NOT STOP
CC                18-JUN-96 : SF: END STATEMENT MOVED TO EOF.
CC                03-JAN-97 : TS: ADDED DAY+3 VARIABLES
CC                27-JAN-99 : TS: UNIX AND VMS SAME (UNIFIED FM AND FI VERSION)
CC                27-APR-99 : DI: ADDED DAY-30 VARIABLES
CC                17-JUN-99 : TS: ADDED C-SHELL OUTPUT FILE (SETDAY.CSH)
CC                06-JUL-99 : PF: CALL IYEAR4 FOR CONVERSION YY->YYYY
CC                13-AUG-99 : MR: ADD LAHEY PART (MIGHT NOT BE WORKING !)
CC                24-SEP-99 : RD: ADD A PERL-SCRIPT OUTPUT (SETDAY.PL)
CC                13-OCT-99 : RD: PERFORM ALL SCRIPT TYPES IN ONE LOOP
CC                                USING SR WRTCMD
CC                28-DEC-99 : SS: POSSIBLE Y2K PROBLEM DEFUSED AND
CC                                READING OF SETDAY.OPT FILE REFINED
CC                07-NOV-00 : RD: 4 DIGIT YEAR FOR ALL DAYS
CC                02-NOV-02 : HU: ADAPTED FROM SETDAY_P TO NEW MENU
CC                18-FEB-03 : HU: USE PREPROCESSOR COMMANDS
CC                23-APR-03 : HU: NULLIFY LOCAL POINTERS
CC                06-NOV-03 : HU: DO NOT USE OPNSYS
CC                19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC                21-JUN-05 : MM: LFNUM.inc REMOVED (LFNUMs IN M_BERN)
CC                23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC                27-FEB-07 : AG: CALL DEFCON
CC                23-SEP-10 : RD: ENABLE CPU COUNTER
CC                29-OCT-12 : RD: REMOVE #ifdef OS_VMS
CC
CC COPYRIGHT    : SIO-IGPP / AIUB
CC      1990
CC
C*
      USE m_bern,   ONLY: lfnloc, lfnerr,
     1                    keyValueLength, fileNameLength80
      USE m_cpu,    ONLY: cpu_start
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE s_opnfil
      USE s_defcon
      USE s_mjdgps
      USE s_clocks
      USE s_readinpf
      USE s_opnerr
      USE s_readkeys
      USE s_wrtcmd
      USE s_exitrc
      USE s_jmt
      USE s_gtflna
      USE s_ckopti
      USE f_iyear4
      USE f_djul
      USE f_dgpsut

      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IDAY   , IDAYC  , IDAYM  , IDAYM2 , IDAYM3 , IDAYM30,
     1          IDAYM4 , IDAYM5 , IDAYM6 , IDAYP  , IDAYP2 , IDAYP3 ,
     2          IDAYP6 , IDAYY  , IDAYYM , IDAYYP , IDYYM2 , IDYYM3 ,
     3          IDYYM30, IDYYM4 , IDYYM5 , IDYYM6 , IDYYP2 , IDYYP3 ,
     4          IDYYP6 , IOSTAT , IRC    , IRCSUM , ITYP   , IWDAY  ,
     5          IYEAR  , IYEARC , IYEARM , IYEARP , IYEARP2,
     6          IYEARP3, IYRM2  , IYRM3  , IYRM30 , IYRM4  , IYRM5  ,
     7          IYRM6  , IYRP6  , JAN    , MNTHM2 , MNTHM3 , MNTHM30,
     8          MNTHM4 , MNTHM5 , MNTHM6 , MNTHP2 , MNTHP3 , MNTHP6 ,
     9          MONTH  , MONTHM , MONTHP , NWEEK
C
      REAL*8    DAY    , DAYM   , DAYM2  , DAYM3  , DAYM30 , DAYM4  ,
     1          DAYM5  , DAYM6  , DAYP   , DAYP2  , DAYP3  , DAYP6  ,
     2          FIRST  , GPSUTC , SECOND , TMIN2  ,
     3          TMIN3  , TMIN30 , TMIN4  , TMIN5  , TMIN6  , TMINUS ,
     4          TPLUS  , TPLUS2 , TPLUS3 , TPLUS6 , TSTART
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue
      CHARACTER(LEN=fileNameLength80) CMDFIL(5),FILUTC
      INTEGER*4    M(8)
      LOGICAL      DOIT,DOITI(5)
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.FALSE.)
C
C NULLIFY POINTERS
C ----------------
      NULLIFY(keyValue)
C
C GET THE NAME OF THE INPUT FILE
C ------------------------------
      CALL init_inpkey(inpKey)
      CALL readinpf(' ',inpKey)
C
C DEFINE SYSTEM FILES AND CONSTANTS
C ---------------------------------
C     CALL OPNSYS
      CALL DEFCON(0)
C
C FILE NAMES
C ----------
      CALL GTFLNA(1,'GPSUTC',FILUTC,IRC)
C
C OUTPUT
      DOITI(1:5)=.TRUE.
      CALL GTFLNA(0,'MSDOS',   CMDFIL(1),IRC)
      IF (IRC.NE.0) DOITI(1)=.FALSE.
      CALL GTFLNA(0,'VAX',     CMDFIL(2),IRC)
      IF (IRC.NE.0) DOITI(1)=.FALSE.
      CALL GTFLNA(0,'UNIX_SH', CMDFIL(3),IRC)
      IF (IRC.NE.0) DOITI(1)=.FALSE.
      CALL GTFLNA(0,'UNIX_CSH',CMDFIL(4),IRC)
      IF (IRC.NE.0) DOITI(1)=.FALSE.
      CALL GTFLNA(0,'PERL',    CMDFIL(5),IRC)
      IF (IRC.NE.0) DOITI(5)=.FALSE.
C
C OPTIONS
C -------
      IRCSUM=0
      CALL readkeys('DOY', keyValue, IRC)
      CALL ckopti(1,'DOY', keyValue, 'pg setday', 'doy',
     1            IRC,IRCSUM, empty=0, maxVal=1, result1=IDAYC)
      CALL readkeys('YEAR', keyValue, IRC)
      CALL ckopti(1,'YEAR', keyValue, 'pg setday', 'year',
     1            IRC,IRCSUM, empty=-1, maxVal=1, result1=IYEARC)
      IF (IRCSUM.NE.0) CALL EXITRC(2)
C
C GET SYSTEM DATE/TIME
C --------------------
      CALL CLOCKS(M)
C
C JULIAN DATE AT START OF PROGRAM
C -------------------------------
      DAY=M(3)+M(5)/24.D0+M(6)/1440.D0+M(7)/86400.D0
      M(1) = IYEAR4(M(1))
      TSTART=DJUL(M(1),M(2),DAY)
C
C MINUSDAY INTERPRETATION
C -----------------------
      IF (IDAYC.LE.0) THEN
        TSTART=TSTART+IDAYC
      ELSE
        JAN=1
        FIRST=1.D0
        IF (IYEARC.EQ.-1) THEN
          CALL JMT(TSTART,IYEAR,MONTH,DAY)
          IDAYY=TSTART-DJUL(IYEAR ,JAN,FIRST)+1
C
C DOY OF THE DAY TO BE PROCESSED MUST BE LOWER/EQUAL THAN THE CURRENT DAY
C -----------------------------------------------------------------------
          IF (IDAYY.LT.IDAYC) IYEAR=IYEAR-1
          TSTART=DJUL(IYEAR,JAN,FIRST)-1+IDAYC
        ELSE
          IYEARC=IYEAR4(IYEARC)
          TSTART=DJUL(IYEARC,JAN,FIRST)-1+IDAYC
        ENDIF
      ENDIF
C
C MINUS DAYS
C ----------
      TMINUS=TSTART-1.D0
      TMIN2=TSTART-2.D0
      TMIN3=TSTART-3.D0
      TMIN4=TSTART-4.D0
      TMIN5=TSTART-5.D0
      TMIN6=TSTART-6.D0
      TMIN30=TSTART-30.D0
C
C PLUS DAYS
C ---------
      TPLUS=TSTART+1.D0
      TPLUS2=TSTART+2.D0
      TPLUS3=TSTART+3.D0
      TPLUS6=TSTART+6.D0
C
C CONVERT MJD'S TO OTHER VALUES
C -----------------------------
      JAN=1
      FIRST=1.D0
C
      CALL JMT(TSTART,IYEAR,MONTH,DAY)
      IDAY=DAY
      IDAYY=TSTART-DJUL(IYEAR ,JAN,FIRST)+1
C
      CALL MJDGPS(TSTART,SECOND,NWEEK)
      IWDAY=DINT(SECOND/86400.D0)
C
      CALL JMT(TMINUS,IYEARM,MONTHM,DAYM)
      IDAYM=DAYM
      IDAYYM=TMINUS-DJUL(IYEARM,JAN,FIRST)+1
C
      CALL JMT(TMIN2,IYRM2,MNTHM2,DAYM2)
      IDAYM2=DAYM2
      IDYYM2=TMIN2-DJUL(IYRM2,JAN,FIRST)+1
C
      CALL JMT(TMIN3,IYRM3,MNTHM3,DAYM3)
      IDAYM3=DAYM3
      IDYYM3=TMIN3-DJUL(IYRM3,JAN,FIRST)+1
C
      CALL JMT(TMIN4,IYRM4,MNTHM4,DAYM4)
      IDAYM4=DAYM4
      IDYYM4=TMIN4-DJUL(IYRM4,JAN,FIRST)+1
C
      CALL JMT(TMIN5,IYRM5,MNTHM5,DAYM5)
      IDAYM5=DAYM5
      IDYYM5=TMIN5-DJUL(IYRM5,JAN,FIRST)+1
C
      CALL JMT(TMIN6,IYRM6,MNTHM6,DAYM6)
      IDAYM6=DAYM6
      IDYYM6=TMIN6-DJUL(IYRM6,JAN,FIRST)+1
C
      CALL JMT(TMIN30,IYRM30,MNTHM30,DAYM30)
      IDAYM30=DAYM30
      IDYYM30=TMIN30-DJUL(IYRM30,JAN,FIRST)+1
C
      CALL JMT(TPLUS,IYEARP,MONTHP,DAYP)
      IDAYP=DAYP
      IDAYYP=TPLUS -DJUL(IYEARP,JAN,FIRST)+1
C
      CALL JMT(TPLUS2,IYEARP2,MNTHP2,DAYP2)
      IDAYP2=DAYP2
      IDYYP2=TPLUS2-DJUL(IYEARP2,JAN,FIRST)+1
C
      CALL JMT(TPLUS3,IYEARP3,MNTHP3,DAYP3)
      IDAYP3=DAYP3
      IDYYP3=TPLUS3-DJUL(IYEARP3,JAN,FIRST)+1
C
      CALL JMT(TPLUS6,IYRP6,MNTHP6,DAYP6)
      IDAYP6=DAYP6
      IDYYP6=TPLUS6 -DJUL(IYRP6,JAN,FIRST)+1
C
      CALL GTFLNA(-1,'GPSUTC ',FILUTC,IRC)
      GPSUTC=DGPSUT(TSTART)
C
C
      DO ITYP=1,5
C
        DOIT=DOITI(ITYP)
#ifdef OS_WIN32
        IF (ITYP.EQ.2) DOIT=.FALSE.
        IF (ITYP.EQ.3) DOIT=.FALSE.
        IF (ITYP.EQ.4) DOIT=.FALSE.
#endif
#ifdef OS_UNIX
        IF (ITYP.EQ.1) DOIT=.FALSE.
        IF (ITYP.EQ.2) DOIT=.FALSE.
#endif
        IF (DOIT) THEN
          CALL OPNFIL(LFNLOC,CMDFIL(ITYP),'UNKNOWN','FORMATTED',
     &                ' ',' ',IOSTAT)
          CALL OPNERR(LFNERR,LFNLOC,IOSTAT,CMDFIL(ITYP),'SETDAY')

          CALL WRTCMD(LFNLOC,'DAYWEEK',IWDAY,1,ITYP)
          CALL WRTCMD(LFNLOC,'GPSWEEK',NWEEK,4,ITYP)

          CALL WRTCMD(LFNLOC,'DAYYEAR',IDAYY,3,ITYP)
          CALL WRTCMD(LFNLOC,'DAYYM',IDAYYM,3,ITYP)
          CALL WRTCMD(LFNLOC,'DAYYM2',IDYYM2,3,ITYP)
          CALL WRTCMD(LFNLOC,'DAYYM3',IDYYM3,3,ITYP)
          CALL WRTCMD(LFNLOC,'DAYYM4',IDYYM4,3,ITYP)
          CALL WRTCMD(LFNLOC,'DAYYM5',IDYYM5,3,ITYP)
          CALL WRTCMD(LFNLOC,'DAYYM6',IDYYM6,3,ITYP)
          CALL WRTCMD(LFNLOC,'DAYYM30',IDYYM30,3,ITYP)
          CALL WRTCMD(LFNLOC,'DAYYP',IDAYYP,3,ITYP)
          CALL WRTCMD(LFNLOC,'DAYYP2',IDYYP2,3,ITYP)
          CALL WRTCMD(LFNLOC,'DAYYP3',IDYYP3,3,ITYP)
          CALL WRTCMD(LFNLOC,'DAYYP6',IDYYP6,3,ITYP)

          CALL WRTCMD(LFNLOC,'YEAR4',IYEAR,4,ITYP)

          CALL WRTCMD(LFNLOC,'YR_4',IYEAR ,2,ITYP)
          CALL WRTCMD(LFNLOC,'YR_4M',IYEARM,2,ITYP)
          CALL WRTCMD(LFNLOC,'YR_4M2',IYRM2,2,ITYP)
          CALL WRTCMD(LFNLOC,'YR_4M3',IYRM3,2,ITYP)
          CALL WRTCMD(LFNLOC,'YR_4M4',IYRM4,2,ITYP)
          CALL WRTCMD(LFNLOC,'YR_4M5',IYRM5,2,ITYP)
          CALL WRTCMD(LFNLOC,'YR_4M6',IYRM6,2,ITYP)
          CALL WRTCMD(LFNLOC,'YR_4M30',IYRM30,2,ITYP)
          CALL WRTCMD(LFNLOC,'YR_4P',IYEARP,2,ITYP)
          CALL WRTCMD(LFNLOC,'YR_4P2',IYEARP2,2,ITYP)
          CALL WRTCMD(LFNLOC,'YR_4P3',IYEARP3,2,ITYP)
          CALL WRTCMD(LFNLOC,'YR_4P6',IYRP6,2,ITYP)

          CALL WRTCMD(LFNLOC,'YEAR',MOD(IYEAR ,100),2,ITYP)
          CALL WRTCMD(LFNLOC,'YEARM',MOD(IYEARM,100),2,ITYP)
          CALL WRTCMD(LFNLOC,'YEARM2',MOD(IYRM2,100),2,ITYP)
          CALL WRTCMD(LFNLOC,'YEARM3',MOD(IYRM3,100),2,ITYP)
          CALL WRTCMD(LFNLOC,'YEARM4',MOD(IYRM4,100),2,ITYP)
          CALL WRTCMD(LFNLOC,'YEARM5',MOD(IYRM5,100),2,ITYP)
          CALL WRTCMD(LFNLOC,'YEARM6',MOD(IYRM6,100),2,ITYP)
          CALL WRTCMD(LFNLOC,'YEARM30',MOD(IYRM30,100),2,ITYP)
          CALL WRTCMD(LFNLOC,'YEARP',MOD(IYEARP,100),2,ITYP)
          CALL WRTCMD(LFNLOC,'YEARP2',MOD(IYEARP2,100),2,ITYP)
          CALL WRTCMD(LFNLOC,'YEARP3',MOD(IYEARP3,100),2,ITYP)
          CALL WRTCMD(LFNLOC,'YEARP6',MOD(IYRP6,100),2,ITYP)

          CALL WRTCMD(LFNLOC,'DAY',IDAY,2,ITYP)
          CALL WRTCMD(LFNLOC,'DAYM',IDAYM,2,ITYP)
          CALL WRTCMD(LFNLOC,'DAYM2',IDAYM2,2,ITYP)
          CALL WRTCMD(LFNLOC,'DAYM3',IDAYM3,2,ITYP)
          CALL WRTCMD(LFNLOC,'DAYM4',IDAYM4,2,ITYP)
          CALL WRTCMD(LFNLOC,'DAYM5',IDAYM5,2,ITYP)
          CALL WRTCMD(LFNLOC,'DAYM6',IDAYM6,2,ITYP)
          CALL WRTCMD(LFNLOC,'DAYM30',IDAYM30,2,ITYP)
          CALL WRTCMD(LFNLOC,'DAYP',IDAYP,2,ITYP)
          CALL WRTCMD(LFNLOC,'DAYP2',IDAYP2,2,ITYP)
          CALL WRTCMD(LFNLOC,'DAYP3',IDAYP3,2,ITYP)
          CALL WRTCMD(LFNLOC,'DAYP6',IDAYP6,2,ITYP)

          CALL WRTCMD(LFNLOC,'MONTH',MONTH,2,ITYP)
          CALL WRTCMD(LFNLOC,'MONTHM',MONTHM,2,ITYP)
          CALL WRTCMD(LFNLOC,'MONTHM2',MNTHM2,2,ITYP)
          CALL WRTCMD(LFNLOC,'MONTHM3',MNTHM3,2,ITYP)
          CALL WRTCMD(LFNLOC,'MONTHM4',MNTHM4,2,ITYP)
          CALL WRTCMD(LFNLOC,'MONTHM5',MNTHM5,2,ITYP)
          CALL WRTCMD(LFNLOC,'MONTHM6',MNTHM6,2,ITYP)
          CALL WRTCMD(LFNLOC,'MONTHM30',MNTHM30,2,ITYP)
          CALL WRTCMD(LFNLOC,'MONTHP',MONTHP,2,ITYP)
          CALL WRTCMD(LFNLOC,'MONTHP2',MNTHP2,2,ITYP)
          CALL WRTCMD(LFNLOC,'MONTHP3',MNTHP3,2,ITYP)
          CALL WRTCMD(LFNLOC,'MONTHP6',MNTHP6,2,ITYP)

          CALL WRTCMD(LFNLOC,'GPSUTC',IDNINT(GPSUTC),2,ITYP)

          CALL WRTCMD(LFNLOC,'MJD',IDNINT(TSTART),5,ITYP)
          CLOSE(UNIT=LFNLOC)
        ENDIF
C
      ENDDO
C
      CALL EXITRC(0)
      END
