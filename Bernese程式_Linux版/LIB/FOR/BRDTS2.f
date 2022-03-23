      MODULE s_BRDTS2
      CONTAINS

C*
      SUBROUTINE BRDTS2(FILNAM,NGSAT,NRGSAT,NGEPH,GEPH,IEXSHI,
     1                  NGSANW,NRGSAN,NGEPHN,GEPHNW)
CC
CC NAME       :  BRDTS2
CC
CC PURPOSE    :  CHECK BROADCAST MESSAGES FOR GLONASS SATELLITES:
CC                                         - REMOVE BAD MESSAGE SETS
CC                                         - DETECT SHIFTED SATELLITES
CC
CC PARAMETERS :
CC         IN :  FILNAM : FILE NAME FOR OUTPUT                 CH*
CC               NGSAT  : NUMBER OF SATELLITES                 I*4
CC               NRGSAT : SATELLITE NUMBERS                    I*4(*)
CC                        NRGSAT(I):
CC                          I: SATELLITE
CC               NGEPH  : NUMBER OF EPHEMERIDES AVAILABLE      I*4(*)
CC                        NGEPH(I):
CC                          I: SATELLITE
CC               GEPH   : EPHEMERIDES INFORMATION              R*8(*,*)
CC                        GEPH(I,K):
CC                          I: EPHEMERIDE ELEMENT
CC                          K: SATELLITE
CC               IEXSHI : EXCLUDE SHIFTED GLONASS SATELLITES   I*4
CC                          0: NO
CC                          1: YES
CC        OUT :  NGSANW : NEW NUMBER OF SATELLITES             I*4
CC               NRGSAN : NEW SATELLITE NUMBERS                I*4(*)
CC                        NRGSAN(I):
CC                          I: SATELLITE
CC               NGEPHN : NEW NUMBER OF EPHEMERIDES AVAILABLE  I*4(*)
CC                        NGEPHN(I):
CC                          I: SATELLITE
CC               GEPHNW : NEW EPHEMERIDES INFORMATION          R*8(*,*)
CC                        GEPHNW(I,K):
CC                          I: EPHEMERIDE ELEMENT
CC                          K: SATELLITE
CC
CC REMARKS    :  TRANSFORM GEPH INTO ARRAY "EPH" FOR TEST PURPOSE
CC
CC               ARRAY "EPH"
CC               EPH(1)   : MJD
CC               EPH(2)   : NOT USED
CC               EPH(3)   : A
CC               EPH(4)   : E
CC               EPH(5)   : I
CC               EPH(6)   : R.A. OF ASCENDING NODE
CC               EPH(7)   : PERIGEE
CC               EPH(8)   : T0
CC               EPH(9)   : NOT USED
CC               ..
CC               EPH(20)  : NOT USED
CC               EPH(21)  : NEXT MESSAGE
CC               ..
CC
CC AUTHOR     :  H. HABRICH
CC
CC VERSION    :  4.1  (MAR 97)
CC
CC CREATED    :  20-MAR-97
CC
CC CHANGES    :  13-MAY-97 : MR: REMOVE UNUSED VARIABLES, DECLARE
CC                               "EPH" WITH MAXEPH, NOT MXCEPH
CC               06-OCT-97 : HH: REPLACE BRDTS1 AND BRDTST BY BRDTS2
CC               27-FEB-98 : MR: MAXEPH FROM 100 TO 500
CC               17-AUG-98 : TS: ADDED POSITION AND VELOCITY TEST
CC               10-SEP-98 : MR: FORMAT I4 FOR GLONASS
CC               10-FEB-99 : MR: CHECK FOR SATELLITE NUMBERS < 1
CC               26-MAR-99 : DI: CHECK D3POS AND D3VEL
CC               12-APR-99 : MR: CHECK CLOCK OFFSET EQUAL ZERO
CC               21-JUN-99 : TS: CORRECT KEPLER ELEMENTS (THETAN)
CC               11-APR-00 : TS: REMOVE SATS WITH NGEPH.LE.2
CC               03-APR-01 : DI: CHECK THE HEALTH FLAG
CC               16-FEB-02 : DI: ADD IEXSHI OPTION (EXCL. SHIFTED GLONASS SATs)
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               23-APR-03 : SS: DO NOT REJECT UNHEALTHY SATELLITES
CC               11-JUN-03 : HU: USE GSTIME
CC               15-JUN-03 : HU: DESACTIVATE USE OF GMST2000
CC               06-AUG-03 : HU: NEW CALL FOR GSTIME
CC               01-FEB-04 : SS: DO NOT REJECT SATELLITES W/O CLOCK OFFSETS
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               07-JUL-05 : HB: USE T_EPOCH FOR GSTIME
CC               04-MAY-08 : RD: NUMSAT ADDED TO CALL OF SR XYZELE
CC               09-AUG-10 : SL/RD: FILNAM ADDED, MAXEPH 500->700
CC               26-MAR-12 : RD: USE TIMSTR AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnerr,lfnprt
      USE m_maxdim, ONLY: MAXSAT
      USE m_epoch,  ONLY: t_epoch, OPERATOR(.realToEpoch.)
      USE d_const,  ONLY: GM, OMEGA, PI
      USE s_dmlmtv
      USE s_maxtst
      USE s_timstr
      USE f_gstime
      USE s_exitrc
      USE s_chkbr3
      USE s_chkbr4
      USE s_xyzele
      USE s_ddreh
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I0NEW , I0OLD , IBAD  , IEPH  , IEPH0 , IEPH01,
     1          IEPH02, IEPH1 , IEPH2 , IEPHNW, IEPPRE, IEPTST, IEXSHI,
     2          IFIRST, II    , IJUMP , INDEX1, INDEX2, IRC1  , IRC2  ,
     3          ISANEW, ISAT  , ISATNW, IWEEKN, J     , JMPOCC, JUMP  ,
     4          L10   , L20   , MAXEPH, MAXJMP, MXBAD , MXCEPH, MXCSAT,
     5          MXEPH , NBAD  , NEPOLD, NGSANW, NGSAT , NJUMP , NOK
C
      REAL*8    A     , D3POS , D3VEL , E     , PER   , POSSQR,
     1          SZ    , T0    , T0E   , T0E1  , T0E2  , T0ESEC, VELSQR,
     2          XI    , XNODE
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
      INTEGER*4    NRGSAT(MXCSAT),NGEPH(MXCSAT),INDEX(MAXEPH)
      INTEGER*4    NRGSAN(MXCSAT),NGEPHN(MXCSAT)
      INTEGER*4    NJUMPS(MAXSAT),NMARKD(MAXSAT),INDBAD(MAXEPH)
      INTEGER*4    JMPSAT(MAXJMP),JMPEPH(MAXJMP)
      REAL*8       JMPT0E(2,MAXJMP),POS(3),VEL(3)
      REAL*8       THET(3,3)
      REAL*8       GEPH(16*MXCEPH,MXCSAT),GEPHNW(16*MXCEPH,MXCSAT)
      REAL*8       EPH(20*MAXEPH)
C
      TYPE(t_epoch) :: TTIM
C
C COMMON BLOCKS
C -------------
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMEPH/MXCEPH,MXNEPH
C
C CHECK MAXIMUM DIMENSIONS
C ------------------------
      CALL MAXTST(1,'BRDTS2',MXNSAT,MAXSAT,MXCSAT,IRC1)
      CALL MAXTST(1,'BRDTS2',MXNEPH,MAXEPH,MXCEPH,IRC2)
      IF (IRC1+IRC2.NE.0) CALL EXITRC(2)
C
C NUMBER OF BAD MESSAGES BEFORE NEW INITIALIZATION (AT LEAST 2)
C -------------------------------------------------------------
      MXBAD=3
C
C LOOP OVER ALL SATELLITES
C ------------------------
      ISATNW=1
      NJUMP=0
      DO 200 ISAT=1,NGSAT
C
C ORDER BROADCAST MESSAGES IN TIME
C --------------------------------
        DO 30 IEPH1=1,NGEPH(ISAT)
          L10=(IEPH1-1)*16
          T0E1=GEPH(L10+1,ISAT)
          DO 10 IEPH2=IEPH1-1,1,-1
            L20=(INDEX(IEPH2)-1)*16
            T0E2=GEPH(L20+1,ISAT)
            IF(T0E2.LT.T0E1) GOTO 20
            INDEX(IEPH2+1)=INDEX(IEPH2)
10        CONTINUE
20        INDEX(IEPH2+1)=IEPH1
30      CONTINUE
C
C CALCULATE ARRAY "EPH"
C --------------------
        DO 35 IEPH=1,NGEPH(ISAT)
          IEPH01=(INDEX(IEPH)-1)*16
          IEPH02=(INDEX(IEPH)-1)*20
          T0ESEC=GEPH(IEPH01+1,ISAT)*86400
          POS(1)=GEPH(IEPH01+5,ISAT)
          POS(2)=GEPH(IEPH01+9,ISAT)
          POS(3)=GEPH(IEPH01+13,ISAT)
          VEL(1)=GEPH(IEPH01+6,ISAT)-OMEGA*POS(2)
          VEL(2)=GEPH(IEPH01+10,ISAT)+OMEGA*POS(1)
          VEL(3)=GEPH(IEPH01+14,ISAT)
C
C CONVERT POS AND VEL TO QUASI INTERTIAL
C --------------------------------------
          TTIM = .realToEpoch.GEPH(IEPH01+1,ISAT)
          SZ=GSTIME(2,TTIM,TTIM,0D0,0D0)
          CALL DDREH(3,SZ,THET)
          CALL DMLMTV(POS,THET,POS)
          CALL DMLMTV(VEL,THET,VEL)
C
C INITIALIZE STATUS AND ARRAY "EPH" FOR PRINTING
C ----------------------------------------------
          STATUS(INDEX(IEPH))=' '
          EPH(IEPH02+1)=GEPH(IEPH01+1,ISAT)
          DO II=3,8
            EPH(IEPH02+II)=0.D0
          ENDDO
C
C CHECK SATELLITE NUMBER
C ----------------------
          IF (NRGSAT(ISAT).LE.100) THEN
            STATUS(INDEX(IEPH))='BAD SAT'
            GOTO 35
          ENDIF
C
C CHECK POSITION
C --------------
          POSSQR=POS(1)**2+POS(2)**2+POS(3)**2
          D3POS=DSQRT(POSSQR)
          IF (DABS(POS(1)).GT.26.D6.OR.
     1        DABS(POS(2)).GT.26.D6.OR.
     2        DABS(POS(3)).GT.26.D6.OR.
     3        D3POS.LT.25.D6 .OR. D3POS.GT.26.D6) THEN
            STATUS(INDEX(IEPH))='BAD POS'
            GOTO 35
          ENDIF
C
C CHECK VELOCITY
C --------------
          VELSQR=VEL(1)**2+VEL(2)**2+VEL(3)**2
          D3VEL=DSQRT(VELSQR)
          IF (DABS(VEL(1)).GT.5.D3.OR.
     1        DABS(VEL(2)).GT.5.D3.OR.
     2        DABS(VEL(3)).GT.5.D3.OR.
     3        D3VEL.LT.3.9D3 .OR. D3VEL.GT.4.0D3) THEN
             STATUS(INDEX(IEPH))='BAD VEL'
             GOTO 35
          ENDIF
C
          CALL XYZELE(GM,T0ESEC,POS,VEL,NRGSAT(ISAT),
     1                A,E,XI,XNODE,PER,T0)
C
          T0=T0/86400.D0
          EPH(IEPH02+1)=GEPH(IEPH01+1,ISAT)
          EPH(IEPH02+3)=A
          EPH(IEPH02+4)=E
          EPH(IEPH02+5)=XI
          EPH(IEPH02+6)=XNODE
          EPH(IEPH02+7)=PER
          EPH(IEPH02+8)=T0
C
C CHECK ZERO SATELLITE CLOCK OFFSET
C ---------------------------------
CC          IF (GEPH(IEPH01+2,ISAT).EQ.0.D0) THEN
CC            STATUS(INDEX(IEPH))='BAD CLK'
CC            GOTO 35
CC          ENDIF
C
C CHECK HEALTH FLAG
C -----------------
CC          IF (GEPH(IEPH01+8,ISAT).NE.0.D0) THEN
CC            STATUS(INDEX(IEPH))='BAD UNH'
CC            GOTO 35
CC          ENDIF
35      CONTINUE
C
C CHECK BROADCAST ELEMENTS: ONE BY ONE
C ------------------------------------
        DO 40 IEPH=1,NGEPH(ISAT)
          IEPH01=(INDEX(IEPH)-1)*20+1
          IF (STATUS(INDEX(IEPH)).NE.' ') GOTO 40
          CALL CHKBR3(EPH(IEPH01),STATUS(INDEX(IEPH)))
40      CONTINUE
C
C CHECK ELEMENT DIFFERENCES IN TIME
C ---------------------------------
        IFIRST=1
        JUMP=0
        JMPOCC=0
        DO 100 IEPH1=1,NGEPH(ISAT)-1
          IF(STATUS(INDEX(IEPH1)).NE.' ') GOTO 100
          INDEX1=(INDEX(IEPH1)-1)*20+1
CCCC          MXEPH=MIN0(IEPH1+MXBAD,NGEPH(ISAT))
          MXEPH=NGEPH(ISAT)
          NBAD=0
          IEPTST=0
          DO 60 IEPH2=IEPH1+1,MXEPH
            IF (STATUS(INDEX(IEPH2)).NE.' ') GOTO 60
            IEPTST=IEPTST+1
            IF (IEPTST.GT.MXBAD) GOTO 65
            INDEX2=(INDEX(IEPH2)-1)*20+1
            CALL CHKBR4(EPH(INDEX1),EPH(INDEX2),STATS1)
C
C STATUS OF DIFFERENCE CHECK OK
C -----------------------------
            IF (STATS1.EQ.' ') THEN
              IFIRST=0
              IF (JUMP.EQ.1) THEN
                STATUS(INDEX(IEPH1))(1:4)='JUMP'
                STATUS(INDEX(IEPH1))(5:8)=STSBAD(1)(5:8)
                JUMP=0
                JMPOCC=1
              ELSE
                DO 50 IBAD=1,NBAD
                  STATUS(INDEX(INDBAD(IBAD)))=STSBAD(IBAD)
50              CONTINUE
              ENDIF
              GOTO 100
C
C STATUS BAD
C ----------
            ELSE
              NBAD=NBAD+1
              INDBAD(NBAD)=IEPH2
              STSBAD(NBAD)=STATS1
            ENDIF
60        CONTINUE
C
C ALL CHECKS BAD
C IF FIRST MESSAGE: SET FIRST MESSAGE BAD
C ---------------------------------------
65        IF (IFIRST.EQ.1) THEN
            STATUS(INDEX(IEPH1))='BAD ???'
          ELSE
            JUMP=1
            IFIRST=1
          ENDIF
100     CONTINUE
C
C LAST MESSAGE(S) BAD
C -------------------
        IF (JUMP.EQ.1) THEN
          DO 105 IBAD=1,NBAD
            STATUS(INDEX(INDBAD(IBAD)))=STSBAD(IBAD)
105       CONTINUE
        ENDIF
C
C CHECK IF MORE THAN 2 MESSAGEs REMAIN
C ------------------------------------
        IF ((NGEPH(ISAT)-NBAD).LE.2) THEN
          NBAD=NGEPH(ISAT)
          DO IEPH=1,NGEPH(ISAT)
            IF (STATUS(INDEX(IEPH)).EQ.' ') THEN
              STATUS(INDEX(IEPH))='BAD ???'
            ENDIF
          ENDDO
        ENDIF
C
C EXCLUDE SATELLITES WITH SHIFTS?
C ------------------------------
        IF (JMPOCC.EQ.1 .AND. IEXSHI.EQ.1) THEN
          DO IEPH=1,NGEPH(ISAT)
            IF (STATUS(INDEX(IEPH)).EQ.' ') THEN
              STATUS(INDEX(IEPH))='BAD SHI'
            ENDIF
          ENDDO
C
          WRITE(LFNERR,903) NRGSAT(ISAT)
 903      FORMAT(/,' ### SR BRDTS2: SATELLITE EXCLUDED DUE TO SHIFTS',/,
     1                         16X,'GLONASS SATELLITE NUMBER :   ',I4,/)
        ENDIF
C
C PRINT ALL MESSAGES OF ONE SATELLITE
C TITLE FOR EPHEMERIS PARAMETERS
C -----------------------------------
        WRITE(LFNPRT,1) NRGSAT(ISAT),TRIM(FILNAM)
1       FORMAT(//' EPHEMERIS PARAMETERS FOR SATELLITE',I4,
     1             '     (FROM FILE: ',A,')',/,
     2           ' --------------------------------------'//,
     3           ' NUM  STATUS        MJD         A            E',
     4           '          I        NODE     PER       T0')
C
C INITIALIZE NUMBER OF BAD MESSAGES AND JUMPS
C -------------------------------------------
        NJUMPS(ISAT)=0
        NMARKD(ISAT)=0
        IWEEKN=0
C
        DO 110 IEPH=1,NGEPH(ISAT)
          IEPH0=(INDEX(IEPH)-1)*16
          IEPH01=(INDEX(IEPH)-1)*20
          STATS1=STATUS(INDEX(IEPH))
C
C WRITE JUMP DETECTED
C -------------------
          IF (STATS1(1:4).EQ.'JUMP') THEN
            NJUMPS(ISAT)=NJUMPS(ISAT)+1
            NJUMP=NJUMP+1
C
C CHECK MAXIMUM NUMBER OF JUMPS ALLOWED
C -------------------------------------
            IF (NJUMP.GT.MAXJMP) THEN
              WRITE(LFNERR,901) NJUMP,MAXJMP
901           FORMAT(/,' *** SR BRDTS2: TOO MANY SATELLITE JUMPS',/,
     1                             16X,'NUMBER OF JUMPS: >=',I4,/,
     2                             16X,'MAXIMUM NUMBER :   ',I4,/)
              CALL EXITRC(2)
            ENDIF
            JMPSAT(NJUMP)=NRGSAT(ISAT)
            JMPEPH(NJUMP)=IEPH
            IEPPRE=(INDEX(IEPH-1)-1)*16
            JMPT0E(1,NJUMP)=GEPH(IEPPRE+1,ISAT)
            JMPT0E(2,NJUMP)=GEPH(IEPH0+1,ISAT)
            WRITE(LFNPRT,2) STATS1(6:8)
2           FORMAT(/,'      SATELLITE SHIFTED: LARGE CHANGE IN ',A,/)
            IF (IEXSHI.EQ.1) THEN
              STATS1='BAD SHI'
              NMARKD(ISAT)=NMARKD(ISAT)+1
            ELSE
              STATS1=' '
            ENDIF
          ELSE IF (STATS1(1:3).EQ.'BAD') THEN
            NMARKD(ISAT)=NMARKD(ISAT)+1
          ENDIF
C
          WRITE(LFNPRT,3) IEPH,STATS1,EPH(IEPH01+1),
     1                    EPH(IEPH01+3),EPH(IEPH01+4),
     2                   (EPH(IEPH01+J)*180/PI,J=5,7),
     3                    EPH(IEPH01+8)
3         FORMAT(I4,1X,A8,F11.5,F11.1,F12.8,
     1           F11.6,2F11.5,F12.5)
110     CONTINUE
C
C TITLE FOR CLOCK PARAMETERS
C --------------------------
        WRITE(LFNPRT,4) NRGSAT(ISAT),TRIM(FILNAM)
4       FORMAT(//' CLOCK PARAMETERS FOR SATELLITE',I4,
     1             '     (FROM FILE: ',A,')',/,
     2           ' ----------------------------------'//,
     3           ' NUM  STATUS       MJD       A0',
     4           '            A1      ')
C
        DO 120 IEPH=1,NGEPH(ISAT)
          IEPH0=(INDEX(IEPH)-1)*16
          STATS1=STATUS(INDEX(IEPH))
C
C WRITE JUMP DETECTED
C -------------------
          IF (STATS1(1:4).EQ.'JUMP') THEN
            WRITE(LFNPRT,2) STATS1(6:8)
              IF (IEXSHI.EQ.1) THEN
                STATS1='BAD SHI'
              ELSE
                STATS1=' '
              ENDIF
          ENDIF
          WRITE(LFNPRT,5) IEPH,STATS1,GEPH(IEPH0+1,ISAT),
     1                    GEPH(IEPH0+2,ISAT),GEPH(IEPH0+3,ISAT)
5         FORMAT(I4,1X,A8,1X,F11.5,1X,D14.6,1X,D14.6)
120     CONTINUE
C
C UPDATE ARRAYS "EPH" AND "CLOCK"
C -------------------------------
        IEPHNW=0
        DO 140 IEPH=1,NGEPH(ISAT)
          I0OLD=16*(INDEX(IEPH)-1)
          STATS1=STATUS(INDEX(IEPH))
          IF (STATUS(INDEX(IEPH))(1:3).EQ.'BAD') GOTO 140
          IF (STATUS(INDEX(IEPH))(1:4).EQ.'JUMP' .AND. IEXSHI.EQ.1)
     1                                           GOTO 140
          IEPHNW=IEPHNW+1
          I0NEW=16*(IEPHNW-1)
          DO 130 I=1,16
            GEPHNW(I0NEW+I,ISATNW)=GEPH(I0OLD+I,ISAT)
130       CONTINUE
140     CONTINUE
        IF (IEPHNW.NE.0) THEN
          NGEPHN(ISATNW)=IEPHNW
          NRGSAN(ISATNW)=NRGSAT(ISAT)
          ISATNW=ISATNW+1
C
C WRITE WARNING, IF ALL MESSAGES OF ONE SATELLITE DELETED
C -------------------------------------------------------
        ELSE
          WRITE(LFNPRT,6) NRGSAT(ISAT)
6         FORMAT(//,' WARNING: ALL MESSAGES OF SATELLITE',I4,
     1              ' DELETED')
        ENDIF
C
C NEXT SATELLITE
C --------------
200   CONTINUE
      NGSANW=ISATNW-1
C
C WRITE PROCESSING SUMMARY
C ------------------------
      WRITE(LFNPRT,7)
7     FORMAT(//,' SUMMARY:',/,' -------',
     1       //,' SAT.  #MSG  #OK   #BAD  #SHIFTS',/)
      ISANEW=1
      DO 210 ISAT=1,NGSAT
        IF (NRGSAT(ISAT).NE.NRGSAN(ISANEW)) THEN
          NOK=0
        ELSE
          NOK=NGEPHN(ISANEW)
          ISANEW=ISANEW+1
        ENDIF
        WRITE(LFNPRT,8) NRGSAT(ISAT),NGEPH(ISAT),NOK,NMARKD(ISAT),
     1                  NJUMPS(ISAT)
8       FORMAT(I4,3I6,I7)
210   CONTINUE
C
C WRITE JUMPS
C -----------
      IF (NJUMP.EQ.0) THEN
        WRITE(LFNPRT,9)
9       FORMAT(/,' NO SHIFTS DETECTED',/)
      ELSE
        WRITE(LFNPRT,11)
11      FORMAT(/,' SHIFTS:',/,' ------',
     1         /,'                LAST MESSAGE BEFORE SHIFT ',
     2           '        FIRST MESSAGE AFTER SHIFT',
     3         /,' NUM  SAT     DATUM     TIME           T0E',
     4           '      DATUM     TIME           T0E',/)
        DO 220 IJUMP=1,NJUMP
          CALL TIMSTR(1,JMPT0E(1,IJUMP),TSTRNG(1))
          CALL TIMSTR(1,JMPT0E(2,IJUMP),TSTRNG(2))
          WRITE(LFNPRT,12) IJUMP,JMPSAT(IJUMP),(TSTRNG(I),
     1                           JMPT0E(I,IJUMP),I=1,2)
12        FORMAT(I4,I5,2X,2(2X,A17,6X,F9.0))
220     CONTINUE
        WRITE(LFNPRT,'( )')
      ENDIF
C
C CHANGE SATELLITE NUMBER FROM ISVN TO ISVN+50 AFTER MANOEUVRE
C ------------------------------------------------------------
      DO 300 IJUMP=1,NJUMP
        DO 230 ISAT=1,NGSANW
          IF (NRGSAN(ISAT).EQ.JMPSAT(IJUMP)) GOTO 240
230     CONTINUE
C
C NEW SATELLITE
C -------------
240     NGSANW=NGSANW+1
        IF (NGSANW.GT.MXCSAT) THEN
          WRITE(LFNERR,902) MXCSAT
902       FORMAT(/,' *** SR BRDTS2: TOO MANY SATELLITES DUE',
     1             ' TO ADDITIONAL',/,
     2         16X,'"MANOEUVRE"-SATELLITE',/,
     3         16X,'MAXIMUM NUMBER OF SAT. ALLOWED:',I3,/)
          CALL EXITRC(2)
        ENDIF
        NRGSAN(NGSANW)=NRGSAN(ISAT)+50
        NGEPHN(NGSANW)=0
        DO 260 IEPH=1,NGEPHN(ISAT)
          IEPH0=(IEPH-1)*16
          T0E=GEPHNW(IEPH0+1,ISAT)
          IF (T0E.GT.JMPT0E(1,IJUMP)) THEN
            NGEPHN(NGSANW)=NGEPHN(NGSANW)+1
            IF (NGEPHN(NGSANW).EQ.1) NEPOLD=IEPH-1
            I0NEW=(NGEPHN(NGSANW)-1)*16
            DO 250 I=1,16
              GEPHNW(I0NEW+I,NGSANW)=GEPHNW(IEPH0+I,ISAT)
250         CONTINUE
          ENDIF
260     CONTINUE
        IF (NGEPHN(NGSANW).EQ.0) THEN
          NGSANW=NGSANW-1
        ELSE
          NGEPHN(ISAT)=NEPOLD
        ENDIF
300   CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
