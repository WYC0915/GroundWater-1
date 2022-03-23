C*
      PROGRAM SETWEEK
CC
CC NAME         : SETWEEK
CC
CC PURPOSE      : PREPARE COMMAND FILE TO SET THE DAY OF THE YEAR
CC                AND THE YEAR INTO SYMBOLS
CC
CC REMARKS      : COMPUTE WEEK VARIABLES BASED ON INPUT GPS WEEK
CC                GPSWEEK=0: CURRENT GPS WEEK
CC                GPSWEEK<0: GPS WEEK RELATIVE TO CURRENT WEEK
CC
CC                FILE 'SETWEEK.COM' WILL CONTAIN COMMAND TO SET THE
CC                SYMBOLS
CC
CC                $ GPSWEEK  == wwww
CC                $ DAYYEAR0 == dd
CC                $ DAYYEAR1 == dd
CC                $ DAYYEAR2 == dd
CC                $ DAYYEAR3 == dd
CC                $ DAYYEAR4 == dd
CC                $ DAYYEAR5 == dd
CC                $ DAYYEAR6 == dd
CC                $ YEAR0    == yy
CC                $ YEAR1    == yy
CC                $ YEAR2    == yy
CC                $ YEAR3    == yy
CC                $ YEAR4    == yy
CC                $ YEAR5    == yy
CC                $ YEAR6    == yy
CC                $ GPSWEEKP1== wwww+1
CC                $ GPSWEEKM1== wwww-1
CC                $ GPSWEEKM2== wwww-2
CC                $ GPSWEEKM3== wwww-3
CC                $ GPSWEEKM4== wwww-4
CC
CC RESTRICTIONS : ***  VAX VMS and UNIX VERSION, DOS NOT CHECKED ***
CC
CC AUTHOR       :  M. ROTHACHER
CC
CC CREATED      :  19-JUL-93
CC
CC CHANGES      :  12-AUG-94 : MR: CALL EXITRC
CC                 15-NOV-95 : EB: ALLOW MINUS WEEK
CC                 27-JAN-99 : TS: ADOPTED FOR UNIX
CC                 06-JUL-99 : PF: CALL IYEAR4 FOR CONVERSION YY->YYYY
CC                 13-AUG-99 : MR: ADD LAHEY PART (NOT CHECKED !)
CC                 13-OCT-99 : RD: PERFORM ALL SCRIPT TYPES IN ONE LOOP
CC                                 USING SR WRTCMD
CC                 02-NOV-02 : HU: ADAPTED FROM SETDAY_P TO NEW MENU
CC                 18-FEB-03 : HU: USE PREPROCESSOR COMMANDS
CC                 23-APR-03 : HU: NULLIFY LOCAL POINTERS
CC                 06-NOV-03 : HU: DO NOT USE OPNSYS
CC                 19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC                 21-JUN-05 : MM: LFNUM.inc REMOVED (LFNUMs IN M_BERN)
CC                 23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC                 27-FEB-07 : AG: CALL DEFCON
CC                 23-SEP-10 : RD: ENABLE CPU COUNTER
CC                 29-OCT-12 : RD: REMOVE #ifdef OS_VMS
CC
CC COPYRIGHT    :  AIUB
CC  (1993)
CC
C
      USE m_bern,   ONLY: lfnloc, lfnerr,
     1                    keyValueLength, fileNameLength80
      USE m_cpu,    ONLY: cpu_start
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE s_opnfil
      USE s_clocks
      USE s_readkeys
      USE s_wrtcmd
      USE s_opnsys
      USE s_defcon
      USE s_gtflna
      USE s_mjdgps
      USE s_readinpf
      USE s_opnerr
      USE s_exitrc
      USE s_jmt
      USE s_ckopti
      USE f_gpsmjd
      USE f_djul
      USE f_iyear4

      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IDAYWK, IMONTH, IOSTAT, IRC   , IRCSUM, ITYP  , IWEEK
C
      REAL*8    DAY   , SECOND, TMJD  , TMJD0 , TSTART
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue
      CHARACTER(LEN=fileNameLength80) CMDFIL(5)
      INTEGER*4    M(8),IYEAR(7),IDOY(7)
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
      CALL OPNSYS
      CALL DEFCON(0)
C
C FILE NAMES
C ----------
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
      CALL readkeys('WEEK', keyValue, IRC)
      CALL ckopti(1,'WEEK', keyValue, 'pg setday', 'week',
     1            IRC,IRCSUM, empty=0, maxVal=1, result1=IWEEK)
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
C JULIAN DATE AT START OF GPS WEEK "IWEEK"
C
C MINUSDAY INTERPRETATION
C -----------------------
      IF (IWEEK.LE.0) THEN
        TSTART=TSTART+IWEEK*7.D0
        CALL MJDGPS(TSTART,SECOND,IWEEK)
      ELSE
        TSTART=GPSMJD(0.D0,IWEEK)
      ENDIF
C
C LOOP OVER DAYS OF WEEK
C ----------------------
      DO 100 IDAYWK=0,6
        TMJD=TSTART+IDAYWK
        CALL JMT(TMJD,IYEAR(IDAYWK+1),IMONTH,DAY)
        TMJD0=DJUL(IYEAR(IDAYWK+1),1,1.D0)
        IDOY(IDAYWK+1)=IDNINT(TMJD-TMJD0)+1
100   CONTINUE
C
C WRITE DAY OF YEAR AND YEAR FOR EACH DAY OF THE WEEK INTO
C
C LOOP OVER ALL SCRIPT TYPES
C --------------------------
      DO ITYP=1,5
        DOIT=DOITI(5)
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
C
C OPEN COMMAND FILE (OUTPUT)
C ----------------------------------------
          CALL OPNFIL(LFNLOC,CMDFIL(ITYP),'UNKNOWN','FORMATTED',
     &                ' ',' ',IOSTAT)
          CALL OPNERR(LFNERR,LFNLOC,IOSTAT,CMDFIL(ITYP),'SETWEEK')
C
C WRITE GPS WEEK INTO COMMAND FILE
C --------------------------------
          CALL WRTCMD(LFNLOC,'GPSWEEK',IWEEK,4,ITYP)
C
C COMMAND FILE
C --------------------------------------------------------
          CALL WRTCMD(LFNLOC,'DAYYEAR0',IDOY(1),3,ITYP)
          CALL WRTCMD(LFNLOC,'YEAR0',MOD(IYEAR(1),100),2,ITYP)
          CALL WRTCMD(LFNLOC,'YEAR40',IYEAR(1),4,ITYP)
C
          CALL WRTCMD(LFNLOC,'DAYYEAR1',IDOY(2),3,ITYP)
          CALL WRTCMD(LFNLOC,'YEAR1',MOD(IYEAR(2),100),2,ITYP)
          CALL WRTCMD(LFNLOC,'YEAR41',IYEAR(2),4,ITYP)
C
          CALL WRTCMD(LFNLOC,'DAYYEAR2',IDOY(3),3,ITYP)
          CALL WRTCMD(LFNLOC,'YEAR2',MOD(IYEAR(3),100),2,ITYP)
          CALL WRTCMD(LFNLOC,'YEAR42',IYEAR(3),4,ITYP)
C
          CALL WRTCMD(LFNLOC,'DAYYEAR3',IDOY(4),3,ITYP)
          CALL WRTCMD(LFNLOC,'YEAR3',MOD(IYEAR(4),100),2,ITYP)
          CALL WRTCMD(LFNLOC,'YEAR43',IYEAR(4),4,ITYP)
C
          CALL WRTCMD(LFNLOC,'DAYYEAR4',IDOY(5),3,ITYP)
          CALL WRTCMD(LFNLOC,'YEAR4',MOD(IYEAR(5),100),2,ITYP)
          CALL WRTCMD(LFNLOC,'YEAR44',IYEAR(5),4,ITYP)
C
          CALL WRTCMD(LFNLOC,'DAYYEAR5',IDOY(6),3,ITYP)
          CALL WRTCMD(LFNLOC,'YEAR5',MOD(IYEAR(6),100),2,ITYP)
          CALL WRTCMD(LFNLOC,'YEAR45',IYEAR(6),4,ITYP)
C
          CALL WRTCMD(LFNLOC,'DAYYEAR6',IDOY(7),3,ITYP)
          CALL WRTCMD(LFNLOC,'YEAR6',MOD(IYEAR(7),100),2,ITYP)
          CALL WRTCMD(LFNLOC,'YEAR46',IYEAR(7),4,ITYP)
C
          CALL WRTCMD(LFNLOC,'GPSWEEKP1',IWEEK+1,4,ITYP)
          CALL WRTCMD(LFNLOC,'GPSWEEKM1',IWEEK-1,4,ITYP)
          CALL WRTCMD(LFNLOC,'GPSWEEKM2',IWEEK-2,4,ITYP)
          CALL WRTCMD(LFNLOC,'GPSWEEKM3',IWEEK-3,4,ITYP)
          CALL WRTCMD(LFNLOC,'GPSWEEKM4',IWEEK-4,4,ITYP)
C
C CLOSE OUTPUT FILE
C -----------------
          CLOSE(UNIT=LFNLOC)
C
        ENDIF
      ENDDO
C
      CALL EXITRC(0)
      END
