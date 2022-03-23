      MODULE s_BRDTS1
      CONTAINS

C*
      SUBROUTINE BRDTS1(FILNAM,NSAT,NRSAT,NEPH,EPH,CLOCK,
     1                  NSANEW,NRSNEW,NEPNEW,EPHNEW,CLKNEW)
CC
CC NAME       :  BRDTS1
CC
CC PURPOSE    :  CHECK BROADCAST MESSAGES: - REMOVE BAD MESSAGE SETS
CC                                         - DETECT SHIFTED SATELLITES
CC
CC
CC PARAMETERS :
CC         IN :  FILNAM : FILE NAME FOR OUTPUT                 CH*
CC               NSAT   : NUMBER OF SATELLITES                 I*4
CC               NRSAT  : SATELLITE NUMBERS                    I*4(*)
CC                        NRSAT(I):
CC                          I: SATELLITE
CC               NEPH   : NUMBER OF EPHEMERIDES AVAILABLE      I*4(*)
CC                        NEPH(I):
CC                          I: SATELLITE
CC               EPH    : EPHEMERIDES INFORMATION              R*8(*,*)
CC                        EPH(I,K):
CC                          I: EPHEMERIDE ELEMENT
CC                          K: SATELLITE
CC               CLOCK  : SATELLITE CLOCK INFORMATION          R*8(*,*)
CC                        CLOCK(I,K):
CC                          I: CLOCK ELEMENT
CC                          K: SATELLITE
CC        OUT :  NSANEW : NEW NUMBER OF SATELLITES             I*4
CC               NRSNEW : NEW SATELLITE NUMBERS                I*4(*)
CC                        NRSNEW(I):
CC                          I: SATELLITE
CC               NEPNEW : NEW NUMBER OF EPHEMERIDES AVAILABLE  I*4(*)
CC                        NEPNEW(I):
CC                          I: SATELLITE
CC               EPHNEW : NEW EPHEMERIDES INFORMATION          R*8(*,*)
CC                        EPHNEW(I,K):
CC                          I: EPHEMERIDE ELEMENT
CC                          K: SATELLITE
CC               CLKNEW : NEW SATELLITE CLOCK INFORMATION      R*8(*,*)
CC                        CLKNEW(I,K)
CC                          I: CLOCK ELEMENT
CC                          K: SATELLITE
CC
CC REMARKS    :  SUBROUTINE VERSION OF BRDTST
CC
CC AUTHOR     :  M.ROTHACHER, H. HABRICH
CC
CC VERSION    :  4.1  (MAR 97)
CC
CC CREATED    :  13-MAR-1997
CC
CC CHANGES    :  02-MAR-98 : MR: MAXEPH FROM 100 TO 500
CC               10-SEP-98 : MR: FORMAT I4 FOR GLONASS
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               09-AUG-10 : SL/RD: FILNAM added, MAXEPH 500->700
CC               29-FEB-12 : RD: CORRECT ARRAY DIMENSIONS OF XMJD
CC               26-MAR-12 : RD: USE TIMSTR AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnprt, lfnerr
      USE m_maxdim, ONLY: MAXSAT
      USE d_const,  ONLY: PI
      USE s_maxtst
      USE s_timstr
      USE s_exitrc
      USE s_chkbr1
      USE s_chkbr2
      USE f_gpsmjd
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I0NEW , I0OLD , IBAD  , IEPH  , IEPH0 , IEPH01,
     1          IEPH1 , IEPH10, IEPH2 , IEPH20, IEPHNW, IEPPRE, IFIRST,
     2          IJUMP , INDEX1, INDEX2, IRC1  , IRC2  , ISANEW, ISAT  ,
     3          ISATNW, IWEEK , IWEEK1, IWEEK2, IWEEKN, J     , JUMP  ,
     4          MAXEPH, MAXJMP, MXBAD , MXCEPH, MXCSAT, MXEPH , NBAD  ,
     5          NEPOLD, NJUMP , NOK   , NSANEW, NSAT
C
      REAL*8    T0E   , T0E1  , T0E2
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MAXJMP=20,MAXEPH=700)
C
C
C DECLARATIONS
C ------------
      CHARACTER*32 FILNAM
      CHARACTER*17 TSTRNG(2)
      CHARACTER*8  STATUS(MAXEPH),STSBAD(MAXEPH),STATS1
      CHARACTER*6  MXNSAT,MXNEPH
      INTEGER*4    NRSAT(MXCSAT),NEPH(MXCSAT),INDEX(MAXEPH)
      INTEGER*4    NRSNEW(MXCSAT),NEPNEW(MXCSAT)
      INTEGER*4    NJUMPS(MAXSAT),NMARKD(MAXSAT),INDBAD(MAXEPH)
      INTEGER*4    JMPSAT(MAXJMP),JMPWEK(2,MAXJMP),JMPEPH(MAXJMP)
      REAL*8       JMPT0E(2,MAXJMP),XMJD(1)
      REAL*8       EPH(20*MXCEPH,MXCSAT),CLOCK(20*MXCEPH,MXCSAT)
      REAL*8       EPHNEW(20*MXCEPH,MXCSAT),CLKNEW(20*MXCEPH,MXCSAT)
C
C COMMON BLOCKS
C -------------
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMEPH/MXCEPH,MXNEPH
C
C CHECK MAXIMUM DIMENSIONS
      CALL MAXTST(1,'BRDTS1',MXNSAT,MAXSAT,MXCSAT,IRC1)
      CALL MAXTST(1,'BRDTS1',MXNEPH,MAXEPH,MXCEPH,IRC2)
      IF (IRC1+IRC2.NE.0) CALL EXITRC(2)
C
C NUMBER OF BAD MESSAGES BEFORE NEW INITIALIZATION (AT LEAST 2)
      MXBAD=6
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
              IEPH20=(INDEX(IEPH2)-1)*20
              IWEEK2=IDNINT(EPH(IEPH20+1,ISAT))
              T0E2  =EPH(IEPH20+2,ISAT)
              IF((IWEEK2.LT.IWEEK1).OR.
     1           (IWEEK2.EQ.IWEEK1.AND.T0E2.LT.T0E1)) GOTO 20
              INDEX(IEPH2+1)=INDEX(IEPH2)
10          CONTINUE
20          INDEX(IEPH2+1)=IEPH1
30        CONTINUE
C
C CHECK BROADCAST ELEMENTS: ONE BY ONE
C ------------------------------------
          DO 40 IEPH=1,NEPH(ISAT)
            IEPH01=(INDEX(IEPH)-1)*20+1
CC            to be activated: use only healthy GPS satellites
CC            IF (CLOCK(4,ISAT).NE.0) THEN
CC              write(*,*)'unhealthy satellite:',NRSAT(ISAT),
CC     .                  CLOCK(4,ISAT)
CC              STATUS(INDEX(IEPH))='BAD UNH'
CC            ELSE
            CALL CHKBR1(EPH(IEPH01,ISAT),STATUS(INDEX(IEPH)))
CC            ENDIF
40        CONTINUE
C
C CHECK ELEMENT DIFFERENCES IN TIME
C ---------------------------------
          IFIRST=1
          JUMP=0
          DO 100 IEPH1=1,NEPH(ISAT)-1
            IF(STATUS(INDEX(IEPH1)).NE.' ') GOTO 100
            INDEX1=(INDEX(IEPH1)-1)*20+1
            MXEPH=MIN0(IEPH1+MXBAD,NEPH(ISAT))
            NBAD=0
            DO 60 IEPH2=IEPH1+1,MXEPH
              IF(STATUS(INDEX(IEPH2)).NE.' ') GOTO 60
              INDEX2=(INDEX(IEPH2)-1)*20+1
              CALL CHKBR2(EPH(INDEX1,ISAT),CLOCK(INDEX1,ISAT),
     1                    EPH(INDEX2,ISAT),CLOCK(INDEX2,ISAT),STATS1)
              IF(STATS1.EQ.' ') THEN
C
C STATUS OF DIFFERENCE CHECK OK
                IFIRST=0
                IF(JUMP.EQ.1) THEN
                  STATUS(INDEX(IEPH1))(1:4)='JUMP'
                  STATUS(INDEX(IEPH1))(5:8)=STSBAD(1)(5:8)
                  JUMP=0
                ELSE
                  DO 50 IBAD=1,NBAD
                    STATUS(INDEX(INDBAD(IBAD)))=STSBAD(IBAD)
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
              STATUS(INDEX(IEPH1))='BAD ???'
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
              STATUS(INDEX(INDBAD(IBAD)))=STSBAD(IBAD)
105         CONTINUE
          ENDIF
C
C PRINT ALL MESSAGES OF ONE SATELLITE
C -----------------------------------
C TITLE FOR EPHEMERIS PARAMETERS
          WRITE(LFNPRT,1) NRSAT(ISAT),TRIM(FILNAM)
1         FORMAT(//' EPHEMERIS PARAMETERS FOR SATELLITE',I4,
     1             '     (FROM FILE: ',A,')',/,
     2             ' --------------------------------------'//,
     3             ' NUM  STATUS   WEEK   T0E       A            E',
     4             '          I        NODE     PER       M0',
     5             '         DN           ODOT')
C
C INITIALIZE NUMBER OF BAD MESSAGES AND JUMPS
          NJUMPS(ISAT)=0
          NMARKD(ISAT)=0
          IWEEKN=0
C
          DO 110 IEPH=1,NEPH(ISAT)
            IEPH0=(INDEX(IEPH)-1)*20
            IWEEK=IDNINT(EPH(IEPH0+1,ISAT))
            STATS1=STATUS(INDEX(IEPH))
C
C WRITE JUMP DETECTED
            IF(STATS1(1:4).EQ.'JUMP') THEN
              NJUMPS(ISAT)=NJUMPS(ISAT)+1
              NJUMP=NJUMP+1
C
C CHECK MAXIMUM NUMBER OF JUMPS ALLOWED
              IF(NJUMP.GT.MAXJMP) THEN
                WRITE(LFNERR,901) NJUMP,MAXJMP
901             FORMAT(/,' *** SR BRDTS1: TOO MANY SATELLITE JUMPS',/,
     1                               16X,'NUMBER OF JUMPS: >=',I4,/,
     2                               16X,'MAXIMUM NUMBER :   ',I4,/)
                CALL EXITRC(2)
              ENDIF
              JMPSAT(NJUMP)=NRSAT(ISAT)
              JMPEPH(NJUMP)=IEPH
              IEPPRE=(INDEX(IEPH-1)-1)*20
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
          WRITE(LFNPRT,4) NRSAT(ISAT),TRIM(FILNAM)
4         FORMAT(//' CLOCK PARAMETERS FOR SATELLITE',I4,
     1             '     (FROM FILE: ',A,')',/,
     2             ' ----------------------------------'//,
     3             ' NUM  STATUS   WEEK   TOE      TOC         A0',
     4            '            A1            A2')
          IWEEKN=0
C
          DO 120 IEPH=1,NEPH(ISAT)
            IEPH0=(INDEX(IEPH)-1)*20
            IWEEK=IDNINT(EPH(IEPH0+1,ISAT))
            STATS1=STATUS(INDEX(IEPH))
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
            I0OLD=20*(INDEX(IEPH)-1)
            IF(STATUS(INDEX(IEPH))(1:3).EQ.'BAD') GOTO 140
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
6           FORMAT(//,' WARNING: ALL MESSAGES OF SATELLITE',I4,
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
     3           /,' NUM  SAT     DATUM     TIME    WEEK   T0E',
     4             '      DATUM     TIME    WEEK   T0E',/)
          DO 220 IJUMP=1,NJUMP
            XMJD(1)=GPSMJD(JMPT0E(1,IJUMP),JMPWEK(1,IJUMP))
            CALL TIMSTR(1,XMJD,TSTRNG(1))
            XMJD(1)=GPSMJD(JMPT0E(2,IJUMP),JMPWEK(2,IJUMP))
            CALL TIMSTR(1,XMJD,TSTRNG(2))
            WRITE(LFNPRT,12) IJUMP,JMPSAT(IJUMP),(TSTRNG(I),
     1                       JMPWEK(I,IJUMP),JMPT0E(I,IJUMP),I=1,2)
12          FORMAT(I4,I5,2X,2(2X,A17,I6,F9.0))
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
          IF(NSANEW.GT.MXCSAT) THEN
            WRITE(LFNERR,902) MXCSAT
902         FORMAT(/,' *** SR BRDTS1: TOO MANY SATELLITES DUE',
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
      RETURN
      END SUBROUTINE

      END MODULE
