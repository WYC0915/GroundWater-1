C*
      PROGRAM BRDTST
CC
CC NAME       :  BRDTST
CC
CC PURPOSE    :  CHECK BROADCAST MESSAGES: - REMOVE BAD MESSAGE SETS
CC                                         - DETECT SHIFTED SATELLITES
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC CREATED    :  88/07/29 10:55
CC
CC CHANGES    :  12-JUN-92 : MANOEUVRES: CHANGE SATELLITE NUMBER ISVN TO
CC                           ISVN+50 IF A SATELLITE WAS SHIFTED
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               20-DEC-93 : MR: PRINT ELEMENT WHERE SHIFT DETECTED
CC               10-AUG-94 : MR: CALL EXITRC
CC               24-SEP-97 : DI: USE MAXSAT.inc
CC               12-FEB-01 : MM: SWITCH TO NEW MENU
CC               15-DEC-01 : HU: USE D_CONST
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               28-FEB-03 : SC: ADD TITLE SECTION
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               21-JUN-05 : MM: LFNUM.inc REMOVED (LFNUMs IN M_BERN)
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               27-FEB-07 : AG: CALL DEFCON WITH PARAMETER
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               24-NOV-11 : SL: NEW TITLE STRING FOR PRITIT, M_BERN WITH ONLY
CC               05-Mar-12 : RD: SWITCH FROM TIMSTR TO TIMST2
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: fileNameLength, lfnPrt, lfnErr
      USE m_cpu,    ONLY: cpu_start
      USE m_maxdim, ONLY: maxsat
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE d_const,  ONLY: pi
      USE s_wtbrdc
      USE s_pritit
      USE s_gtbrdc
      USE s_btstin
      USE s_defcon
      USE s_chkbr1
      USE s_chkbr2
      USE s_opnsys
      USE s_gtfile
      USE s_readinpf
      USE s_timst2
      USE s_exitrc
      USE f_gpsmjd

      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I0NEW , I0OLD , IBAD  , IEPH  , IEPH0 , IEPH01,
     1          IEPH1 , IEPH10, IEPH2 , IEPH20, IEPHNW, IEPPRE, IFIL  ,
     2          IFIRST, IJUMP , INDEX1, INDEX2, ISANEW, ISAT  , ISATNW,
     3          IWEEK , IWEEK1, IWEEK2, IWEEKN, J     , JUMP  , MAXEPH,
     4          MAXFIL, MAXJMP, MFIL  , MXBAD , MXCEPH, MXCSAT, MXEPH ,
     5          NBAD  , NEPOLD, NFIL  , NFLCOL, NJUMP , NOK   , NSANEW,
     6          NSAT
C
      REAL*8    T0E   , T0E1  , T0E2  , XMJD
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C MAXIMAL DIMENSIONS
C ------------------
      PARAMETER (MAXEPH=100,MAXJMP=20,MAXFIL=50)
C
C MAXSAT: MAXIMUM NUMBER OF SATELLITES
C MAXEPH: MAXIMUM NUMBER OF DIFFERENT NAVIGATION MESSAGES PER SATELLITE
C MAXJMP: MAXIMUM NUMBER OF JUMPS IN ALL THE MESSAGES
C MAXFIL: MAXIMUM NUMBER OF FILES
C
C DECLARATIONS
C ------------
      CHARACTER*53 TITLE
      CHARACTER(LEN=fileNameLength) FILBRD(2,MAXFIL),NFILOUT(MAXFIL),
     1                              FILOUT
      CHARACTER*19 TSTRNG(2)
      CHARACTER*8  STATUS(MAXEPH),STSBAD(MAXEPH),STATS1
      CHARACTER*6  MXNSAT,MXNEPH
      INTEGER*4    NRSAT(MAXSAT),NEPH(MAXSAT),INDEX0(MAXEPH)
      INTEGER*4    NRSNEW(MAXSAT),NEPNEW(MAXSAT)
      INTEGER*4    NJUMPS(MAXSAT),NMARKD(MAXSAT),INDBAD(MAXEPH)
      INTEGER*4    JMPSAT(MAXJMP),JMPWEK(2,MAXJMP),JMPEPH(MAXJMP)
      REAL*8       JMPT0E(2,MAXJMP)
      REAL*8       EPH(20*MAXEPH,MAXSAT),CLOCK(20*MAXEPH,MAXSAT)
      REAL*8       EPHNEW(20*MAXEPH,MAXSAT),CLKNEW(20*MAXEPH,MAXSAT)
      LOGICAL      RADIOB
C
C COMMON BLOCKS
C -------------
      COMMON/LARGE/EPH,CLOCK,EPHNEW,CLKNEW
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMEPH/MXCEPH,MXNEPH
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C INITIALIZE COMMON BLOCKS FOR MAXIMAL DIMENSIONS
C -----------------------------------------------
      MXCSAT=MAXSAT
      MXCEPH=MAXEPH
      MXNSAT='MAXSAT'
      MXNEPH='MAXEPH'
C
C DEFINE LOGICAL FILE NUMBERS
C ---------------------------
C
C GET THE NAME OF THE INPUT FILE
C ------------------------------
      CALL init_inpkey(inpKey)
      CALL readinpf(' ',inpKey)
C
C DEFINE SYSTEM FILES
C -------------------
      CALL OPNSYS

C PRINT HEADER FOR OUTPUT FILE
C ----------------------------
      CALL PRITIT('BRDTST','Check broadcast orbits')
C
C DEFINE CONSTANTS
C ----------------
      CALL DEFCON(1)
C
C NUMBER OF BAD MESSAGES BEFORE NEW INITIALIZATION (AT LEAST 2)
      MXBAD=6

C
C READ BROADCAST INPUT AND OUTPUT FILES
C -------------------------------------
      NFLCOL=1
      CALL GTFILE('BRDFIL',NFLCOL,MAXFIL,NFIL,FILBRD(1,:))
      CALL GTFILE('OUTFIL',NFLCOL,MAXFIL,MFIL,NFILOUT)

C
C CHECK STATUS OF RADIOBUTTTON
C ----------------------------
      CALL BTSTIN (FILBRD, NFIL, NFILOUT, MFIL, RADIOB, MAXFIL)

C
C LOOP OVER ALL BROADCAST FILES TO BE TESTED
C ------------------------------------------
      DO 1000 IFIL=1,NFIL
C
C WRITE FILE NAMES
C ----------------
        IF(FILBRD(2,IFIL).EQ.' ') THEN
          FILOUT='  ---'
        ELSE
          FILOUT=FILBRD(2,IFIL)
        ENDIF
        WRITE(LFNPRT,1001) IFIL,FILBRD(1,IFIL),FILOUT
1001    FORMAT(///,' ',72('*'),
     1           /,' TEST BROADCAST MESSAGES:  FILE',I4,
     2           /,' ',72('*'),
     3          //,' BROADCAST INPUT  FILE NAME: ',A32,
     4           /,' BROADCAST OUTPUT FILE NAME: ',A32)

C
C READ BROADCAST MESSAGES
C -----------------------
        CALL GTBRDC(FILBRD(1,IFIL),TITLE,NSAT,NRSAT,NEPH,EPH,CLOCK)

C
C LOOP OVER ALL SATELLITES
C ------------------------
        ISATNW=1
        NJUMP=0
        DO 200 ISAT=1,NSAT
C
C ORDER BROADCAST MESSAGES IN TIME
C --------------------------------
          DO 30 IEPH1=1,NEPH(ISAT)
            IEPH10=(IEPH1-1)*20
            IWEEK1=IDNINT(EPH(IEPH10+1,ISAT))
            T0E1  =EPH(IEPH10+2,ISAT)
            DO 10 IEPH2=IEPH1-1,1,-1
              IEPH20=(INDEX0(IEPH2)-1)*20
              IWEEK2=IDNINT(EPH(IEPH20+1,ISAT))
              T0E2  =EPH(IEPH20+2,ISAT)
              IF((IWEEK2.LT.IWEEK1).OR.
     1           (IWEEK2.EQ.IWEEK1.AND.T0E2.LT.T0E1)) GOTO 20
              INDEX0(IEPH2+1)=INDEX0(IEPH2)
10          CONTINUE
20          INDEX0(IEPH2+1)=IEPH1
30        CONTINUE
C
C CHECK BROADCAST ELEMENTS: ONE BY ONE
C ------------------------------------
          DO 40 IEPH=1,NEPH(ISAT)
            IEPH01=(INDEX0(IEPH)-1)*20+1
            CALL CHKBR1(EPH(IEPH01,ISAT),STATUS(INDEX0(IEPH)))
40        CONTINUE
C
C CHECK ELEMENT DIFFERENCES IN TIME
C ---------------------------------
          IFIRST=1
          JUMP=0
          DO 100 IEPH1=1,NEPH(ISAT)-1
            IF(STATUS(INDEX0(IEPH1)).NE.' ') GOTO 100
            INDEX1=(INDEX0(IEPH1)-1)*20+1
            MXEPH=MIN0(IEPH1+MXBAD,NEPH(ISAT))
            NBAD=0
            DO 60 IEPH2=IEPH1+1,MXEPH
              IF(STATUS(INDEX0(IEPH2)).NE.' ') GOTO 60
              INDEX2=(INDEX0(IEPH2)-1)*20+1
              CALL CHKBR2(EPH(INDEX1,ISAT),CLOCK(INDEX1,ISAT),
     1                    EPH(INDEX2,ISAT),CLOCK(INDEX2,ISAT),STATS1)
              IF(STATS1.EQ.' ') THEN
C
C STATUS OF DIFFERENCE CHECK OK
                IFIRST=0
                IF(JUMP.EQ.1) THEN
                  STATUS(INDEX0(IEPH1))(1:4)='JUMP'
                  STATUS(INDEX0(IEPH1))(5:8)=STSBAD(1)(5:8)
                  JUMP=0
                ELSE
                  DO 50 IBAD=1,NBAD
                    STATUS(INDEX0(INDBAD(IBAD)))=STSBAD(IBAD)
50                CONTINUE
                ENDIF
                GOTO 100
              ELSE
C
C STATUS BAD
                NBAD=NBAD+1
                INDBAD(NBAD)=IEPH2
                STSBAD(NBAD)=STATS1
              ENDIF
60          CONTINUE
C
C ALL CHECKS BAD
C IF FIRST MESSAGE: SET FIRST MESSAGE BAD
            IF(IFIRST.EQ.1) THEN
              STATUS(INDEX0(IEPH1))='BAD ???'
            ELSE
              JUMP=1
              IFIRST=1
            ENDIF
100       CONTINUE
C
C LAST MESSAGE(S) BAD
C -------------------
          IF(JUMP.EQ.1) THEN
            DO 105 IBAD=1,NBAD
              STATUS(INDEX0(INDBAD(IBAD)))=STSBAD(IBAD)
105         CONTINUE
          ENDIF
C
C PRINT ALL MESSAGES OF ONE SATELLITE
C -----------------------------------
C TITLE FOR EPHEMERIS PARAMETERS
          WRITE(LFNPRT,1) NRSAT(ISAT)
1         FORMAT(//' EPHEMERIS PARAMETERS FOR SATELLITE',I3,/,
     1             ' -------------------------------------'//,
     2             ' NUM  STATUS   WEEK   T0E       A            E',
     3             '          I        NODE     PER       M0',
     4             '         DN           ODOT')
C
C INITIALIZE NUMBER OF BAD MESSAGES AND JUMPS
          NJUMPS(ISAT)=0
          NMARKD(ISAT)=0
          IWEEKN=0
C
          DO 110 IEPH=1,NEPH(ISAT)
            IEPH0=(INDEX0(IEPH)-1)*20
            IWEEK=IDNINT(EPH(IEPH0+1,ISAT))
            STATS1=STATUS(INDEX0(IEPH))
C
C WRITE JUMP DETECTED
            IF(STATS1(1:4).EQ.'JUMP') THEN
              NJUMPS(ISAT)=NJUMPS(ISAT)+1
              NJUMP=NJUMP+1
C
C CHECK MAXIMUM NUMBER OF JUMPS ALLOWED
              IF(NJUMP.GT.MAXJMP) THEN
                WRITE(LFNERR,901) NJUMP,MAXJMP
901             FORMAT(/,' *** PG BRDTST: TOO MANY SATELLITE JUMPS',/,
     1                               16X,'NUMBER OF JUMPS: >=',I4,/,
     2                               16X,'MAXIMUM NUMBER :   ',I4,/)
                CALL EXITRC(2)
              ENDIF
              JMPSAT(NJUMP)=NRSAT(ISAT)
              JMPEPH(NJUMP)=IEPH
              IEPPRE=(INDEX0(IEPH-1)-1)*20
              JMPWEK(1,NJUMP)=IDNINT(EPH(IEPPRE+1,ISAT))
              JMPT0E(1,NJUMP)=EPH(IEPPRE+2,ISAT)
              JMPWEK(2,NJUMP)=IWEEK
              JMPT0E(2,NJUMP)=EPH(IEPH0+2,ISAT)
              WRITE(LFNPRT,2) STATS1(6:8)
2             FORMAT(/,'      SATELLITE SHIFTED: LARGE CHANGE IN ',A,/)
              STATS1=' '
            ELSE IF(STATS1(1:3).EQ.'BAD') THEN
              NMARKD(ISAT)=NMARKD(ISAT)+1
            ENDIF
C
C NEW WEEK: BLANK LINE
            IF(IWEEK.NE.IWEEKN) THEN
              IWEEKN=IWEEK
              WRITE(LFNPRT,'( )')
            ENDIF
            WRITE(LFNPRT,3) IEPH,STATS1,IWEEK,
     1                      (EPH(IEPH0+J,ISAT),       J=2,4),
     2                      (EPH(IEPH0+J,ISAT)*180/PI,J=5,8),
     3                      EPH(IEPH0+9,ISAT),EPH(IEPH0+10,ISAT)*180/PI
3           FORMAT(I4,1X,A8,I5,F9.0,F11.1,F12.8,
     1             F11.6,F10.5,2F9.4,2D13.5)
110       CONTINUE
C
C TITLE FOR CLOCK PARAMETERS
          WRITE(LFNPRT,4) NRSAT(ISAT)
4         FORMAT(//' CLOCK PARAMETERS FOR SATELLITE',I3,/,
     1             ' ---------------------------------'//,
     2             ' NUM  STATUS   WEEK   TOE      TOC         A0',
     3            '            A1            A2')
          IWEEKN=0
C
          DO 120 IEPH=1,NEPH(ISAT)
            IEPH0=(INDEX0(IEPH)-1)*20
            IWEEK=IDNINT(EPH(IEPH0+1,ISAT))
            STATS1=STATUS(INDEX0(IEPH))
C
C WRITE JUMP DETECTED
            IF(STATS1(1:4).EQ.'JUMP') THEN
              WRITE(LFNPRT,2) STATS1(6:8)
              STATS1=' '
            ENDIF
C
C NEW WEEK: BLANK LINE
            IF(IWEEK.NE.IWEEKN) THEN
              IWEEKN=IWEEK
              WRITE(LFNPRT,'( )')
            ENDIF
            WRITE(LFNPRT,5) IEPH,STATS1,IWEEK,
     1                      EPH(IEPH0+2,ISAT),CLOCK(IEPH0+11,ISAT),
     2                      (CLOCK(IEPH0+J,ISAT),J=14,12,-1)
5           FORMAT(I4,1X,A8,I5,F9.0,F9.0,3D14.6)
120       CONTINUE
C
C UPDATE ARRAYS "EPH" AND "CLOCK"
C -------------------------------
          IEPHNW=0
          DO 140 IEPH=1,NEPH(ISAT)
            I0OLD=20*(INDEX0(IEPH)-1)
            IF(STATUS(INDEX0(IEPH))(1:3).EQ.'BAD') GOTO 140
            IEPHNW=IEPHNW+1
            I0NEW=20*(IEPHNW-1)
            DO 130 I=1,20
              EPHNEW(I0NEW+I,ISATNW)=EPH(I0OLD+I,ISAT)
              CLKNEW(I0NEW+I,ISATNW)=CLOCK(I0OLD+I,ISAT)
130         CONTINUE
140       CONTINUE
          IF(IEPHNW.NE.0) THEN
            NEPNEW(ISATNW)=IEPHNW
            NRSNEW(ISATNW)=NRSAT(ISAT)
            ISATNW=ISATNW+1
C
C WRITE WARNING, IF ALL MESSAGES OF ONE SATELLITE DELETED
          ELSE
            WRITE(LFNPRT,6) NRSAT(ISAT)
6           FORMAT(//,' WARNING: ALL MESSAGES OF SATELLITE',I3,
     1                ' DELETED')
          ENDIF
200     CONTINUE
        NSANEW=ISATNW-1
C
C WRITE PROCESSING SUMMARY
C ------------------------
        WRITE(LFNPRT,7)
7       FORMAT(//,' SUMMARY:',/,' -------',
     1         //,' SAT.  #MSG  #OK   #BAD  #SHIFTS',/)
        ISANEW=1
        DO 210 ISAT=1,NSAT
          IF(NRSAT(ISAT).NE.NRSNEW(ISANEW)) THEN
            NOK=0
          ELSE
            NOK=NEPNEW(ISANEW)
            ISANEW=ISANEW+1
          ENDIF
          WRITE(LFNPRT,8) NRSAT(ISAT),NEPH(ISAT),NOK,NMARKD(ISAT),
     1                    NJUMPS(ISAT)
8         FORMAT(I4,3I6,I7)
210     CONTINUE
C
C WRITE JUMPS
C -----------
        IF(NJUMP.EQ.0) THEN
          WRITE(LFNPRT,9)
9         FORMAT(/,' NO SHIFTS DETECTED',/)
        ELSE
          WRITE(LFNPRT,11)
11        FORMAT(/,' SHIFTS:',/,' ------',
     1           /,'                LAST MESSAGE BEFORE SHIFT ',
     2             '        FIRST MESSAGE AFTER SHIFT',
     3           /,' NUM  SAT      DATUM      TIME    WEEK   T0E',
     4             '       DATUM      TIME    WEEK   T0E',/)
          DO 220 IJUMP=1,NJUMP
            XMJD=GPSMJD(JMPT0E(1,IJUMP),JMPWEK(1,IJUMP))
            CALL TIMST2(1,1,XMJD,TSTRNG(1))
            XMJD=GPSMJD(JMPT0E(2,IJUMP),JMPWEK(2,IJUMP))
            CALL TIMST2(1,1,XMJD,TSTRNG(2))
            WRITE(LFNPRT,12) IJUMP,JMPSAT(IJUMP),(TSTRNG(I),
     1                       JMPWEK(I,IJUMP),JMPT0E(I,IJUMP),I=1,2)
12          FORMAT(I4,I5,2X,2(2X,A19,I6,F9.0))
220       CONTINUE
          WRITE(LFNPRT,'( )')
        ENDIF
C
C CHANGE SATELLITE NUMBER FROM ISVN TO ISVN+50 AFTER MANOEUVRE
C ------------------------------------------------------------
        DO 300 IJUMP=1,NJUMP
          DO 230 ISAT=1,NSANEW
            IF (NRSNEW(ISAT).EQ.JMPSAT(IJUMP)) GOTO 240
230       CONTINUE
C
C NEW SATELLITE
240       NSANEW=NSANEW+1
          IF(NSANEW.GT.MAXSAT) THEN
            WRITE(LFNERR,902) MAXSAT
902         FORMAT(/,' *** PG BRDTST: TOO MANY SATELLITES DUE',
     1               ' TO ADDITIONAL',/,
     2               16X,'"MANOEUVRE"-SATELLITE',/,
     3               16X,'MAXIMUM NUMBER OF SAT. ALLOWED:',I3,/)
            CALL EXITRC(2)
          ENDIF
          NRSNEW(NSANEW)=NRSNEW(ISAT)+50
          NEPNEW(NSANEW)=0
          DO 260 IEPH=1,NEPNEW(ISAT)
            IEPH0=(IEPH-1)*20
            IWEEK=IDNINT(EPHNEW(IEPH0+1,ISAT))
            T0E=EPHNEW(IEPH0+2,ISAT)
            IF((IWEEK.GT.JMPWEK(1,IJUMP)).OR.
     1         (IWEEK.EQ.JMPWEK(1,IJUMP).AND.T0E.GT.JMPT0E(1,IJUMP)))
     2        THEN
              NEPNEW(NSANEW)=NEPNEW(NSANEW)+1
              IF (NEPNEW(NSANEW).EQ.1) NEPOLD=IEPH-1
              I0NEW=(NEPNEW(NSANEW)-1)*20
              DO 250 I=1,20
                EPHNEW(I0NEW+I,NSANEW)=EPHNEW(IEPH0+I,ISAT)
                CLKNEW(I0NEW+I,NSANEW)=CLKNEW(IEPH0+I,ISAT)
250           CONTINUE
            ENDIF
260       CONTINUE
          IF (NEPNEW(NSANEW).EQ.0) THEN
            NSANEW=NSANEW-1
          ELSE
            NEPNEW(ISAT)=NEPOLD
          ENDIF
300     CONTINUE
C
C SAVE NEW EPHEMERIS SET
C ----------------------
        CALL WTBRDC(FILBRD(2,IFIL),TITLE,NSANEW,NRSNEW,NEPNEW,
     1              EPHNEW,CLKNEW)
C
C END OF FILE LOOP
C ----------------
1000  CONTINUE
C
      CALL EXITRC(0)
      END
