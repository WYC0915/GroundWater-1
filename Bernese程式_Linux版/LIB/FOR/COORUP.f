      MODULE s_COORUP
      CONTAINS

C*
      SUBROUTINE COORUP(IAUTO,TITLE,NSTAT,STNAME,STAFIX,XSTAT,
     1                  NFIL,STFIL,TRPFLG,DXFIL,TIMCRD)
CC
CC NAME       :  COORUP
CC
CC PURPOSE    :  CORRECT THE A PRIORI STATION COORDINATES BY A
CC               VECTOR GIVEN IN THE MAUPRP PROGRAM.
CC               AT LEAST ONE STATION IS KEPT FIXED
CC
CC PARAMETERS :
CC         IN :  IAUTO  : FLAG: MANUAL PREPROCESSING IAUTO=0 I*4
CC                              AUTO PREPROCESSING   IAUTO=1
CC               TITLE  : TITLE OF THE NEW COORDINATE FILE    CH*80
CC               NSTAT  : NUMBER OF STATIONS THAT ARE IN
CC                        THE LIST OF FILES                    I*4
CC               STNAME(K),K=1..NSTAT: LIST OF STATION NAMES  CH*16
CC               STAFIX : SELECTED STATION TO BE FIXED        CH*16
CC               XSTAT(J,K),J=1,..,3,K=1,..NSTAT : COORDINATES
CC                        OF THE STATIONS INVOLVED             R*8
CC               NFIL   : NUMBER OF FILES TO BE PROCESSED      I*4
CC               STFIL(J,K),J=1,2,K=1,..,NFIL : VECTOR WITH
CC                        THE STATION NAMES INVOLVED IN THE
CC                        FILE NUMBER K                        I*4
CC               TRPFLG(K),K=1,..,NFIL: TRIPLE DIFFERENCE     CH*1
CC                        SOLUTION FLAG
CC                        ='B': BAD OR NO SOLUTION
CC                        ='G': GOOD SOLUTION
CC               DXFIL(J,K)  : CORRECTION VECTOR OF THE FILE
CC                        NUMBER K                             R*8
CC               TIMCRD : EPOCH OF COORDINATES (MJD)           R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S.FANKHAUSER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  90/02/02 14:24
CC
CC CHANGES    :  30-SEP-93 : ??: CALL TO SR WTSTAT CHANGED (STANUM,DATUM)
CC               10-AUG-94 : MR: CALL EXITRC
CC               18-SEP-95 : JJ: INCREASE MAXSTA TO 200
CC                               INCREASE MAXFIL TO 200
CC               09-FEB-96 : MR: ERROR MESSAGE IMPROVED
CC               04-AUG-99 : PF: PARAMETER "TIMCRD" FOR SR WTSTAT
CC               28-JUN-04 : RD: USE MAXSTA FROM M_MAXDIM
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               11-FEB-11 : RD: NEW CALL OF WTSTAT
CC               14-FEB-11 : RD: REMOVE MAXSTA-COMMON (NOT NEEDED)
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1990     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
C
C MAXFIL: MAXIMAL NUMBER OF BASELINES (FILES)
C MAXSTA: MAXIMAL NUMBER OF STATIONS
C
      USE s_wtstat
      USE s_exitrc
      USE s_gtflna
      USE s_maxtst
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IAUTO , ICHNFL, IFIL  , IFIX  , IND   , IRC   ,
     1          IRC1  , ISTAOC, J     , K     , L     , M     ,
     2          MAXFIL, MXCFIL, NFIL  , NFIX  , NSTAT
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C DECLARATION OF THE PARAMETERS
C -----------------------------
      PARAMETER(MAXFIL=200)
C
C DECLARATION OF THE VARIABLES
C ----------------------------
      CHARACTER*80    TITLE
      CHARACTER*32    FILCOR,FILECC
      CHARACTER*16    STNAME(*),STAFIX,DATUM
      CHARACTER*6     MXNFIL
      CHARACTER*1     STAFLG(NSTAT), FSTA1,FSTA2,TRPFLG(*)
      REAL*8          XSTAT(3,*),DXFIL(3,*)
      REAL*8          DXHELP(3,NSTAT),XSTARS(3,NSTAT)
      REAL*8          TIMCRD
      INTEGER*4       STFIL(2,*),STAOC(NSTAT),FIXSTA,STA1,STA2
      INTEGER*4       STANUM(NSTAT)
      LOGICAL*4       END
C
      COMMON/MCMFIL/MXCFIL,MXNFIL
C
C CHECK MAXIMUM LOCAL DIMENSION
C -----------------------------
      CALL MAXTST(1,'COORUP',MXNFIL,MAXFIL,MXCFIL,IRC1)
      IF(IRC1.NE.0) CALL EXITRC(2)
C
C NO COORDINATES SAVED, IF OUTPUT FILE NOT AVAILABLE
C --------------------------------------------------
      CALL GTFLNA(0,'COORDRS',FILCOR,IRC)
      IF(IRC.NE.0) RETURN
C
      CALL GTFLNA(0,'ECCENT ',FILECC,IRC)
      IF(IRC.EQ.0) THEN
        WRITE(LFNERR,901)
901     FORMAT(/,' *** SR COORUP: COORDINATES NOT SAVED IF ECCENT',
     1                           'RICITY FILE GIVEN',/)
        RETURN
      ENDIF
C
C INITIALISATION OF THE VARIABLES
C -------------------------------
      END=.TRUE.
      FIXSTA=0
      DO 100 I=1,NSTAT
        STAOC(I)=0
        STAFLG(I)=' '
        DO 106 J=1,3
          XSTARS(J,I)=XSTAT(J,I)
          DXHELP(J,I)=0.D0
106     CONTINUE
100   CONTINUE
C
C COUNTS THE OCCURRENCE OF THE SEVERAL STATIONS IN THE NET
C --------------------------------------------------------
      IFIL=0
      DO 101 I=1,NFIL
        IF (TRPFLG(I).EQ.'G') THEN
          DO 102 J=1,2
            IND=STFIL(J,I)
            STAOC(IND)=STAOC(IND)+1
102       CONTINUE
        ELSE
          IFIL=IFIL+1
        END IF
101   CONTINUE
C
C MANUAL SELECTION OF A FIXED STATION
C -----------------------------------
      IF (IAUTO.EQ.0) THEN
        DO 119 J=1,10000
          NFIX=0
          WRITE(LFNPRT,16)
16        FORMAT(' NUMBER    STATION NAME      #OCCURRENCES',/,
     1           ' ======    ================  ============',//)
          DO 118 I=1,NSTAT
            IF (STAOC(I).GT.0) THEN
              WRITE(LFNPRT,12) I,STNAME(I),STAOC(I)
12            FORMAT (I7,4X,A16,2X,I12)
            END IF
118       CONTINUE
          WRITE(LFNPRT,13)
13        FORMAT(/,' ENTER NUMBER OF FIXED STATION (AUTOMATIC=0,',
     1             ' REDISP=-1):')
          READ (LFNKBD,*,ERR=121) NFIX
          IF (NFIX.LE.-1) GOTO 119
          IF (NFIX.EQ.0) THEN
            FIXSTA=0
            GOTO 120
          ELSE
            IF ((NFIX.LE.NSTAT).AND.(STAOC(NFIX).GT.0)) THEN
              FIXSTA=NFIX
              GOTO 120
            END IF
          END IF
121       WRITE(LFNPRT,15)
15        FORMAT(' THIS SELECTION IS NOT VALID  ',/)
119     CONTINUE
      END IF
C
C LOOP OVER ALL CONNECTED PARTS OF A NET ==> WHOLE NET IS CORRECTED
C -----------------------------------------------------------------
120   DO 112 K=1,10000
C
C CHECKS IF THAT STATION CAN BE FIXED
C -----------------------------------
        IF ((STAFIX.NE.' ').AND.(IAUTO.EQ.1)) THEN
          DO 116 I=1,NSTAT
            IF (STAFIX.EQ.STNAME(I)) THEN
              IF (STAOC(I).GT.0) THEN
                FIXSTA=I
                STAFIX=' '
              END IF
              GOTO 117
            END IF
116       CONTINUE
117       IF (FIXSTA.EQ.0) THEN
            WRITE(LFNPRT,11) STAFIX
11          FORMAT(/,' *** SR COORUP : SELECTED STATION ',A16,
     1             /,'                 IS NOT MEMBER OF THE ACTUAL NET'
     2             /,'                 AUTO-SELECTION ACTIVATED')
          END IF
        END IF
C
C IF NO FIXED STATION SELECTED, MOST OCCURED STATION WILL BE FIXED
C ----------------------------------------------------------------
        IF (FIXSTA.EQ.0) THEN
          ISTAOC=0
          DO 103 I=1,NSTAT
            IF ((STAOC(I).GT.ISTAOC).AND.(STAFLG(I).EQ.' ')) THEN
              FIXSTA=I
              ISTAOC=STAOC(I)
            END IF
103       CONTINUE
        END IF
C
C SET THE FLAG 'F' FOR THE FIXED STATION
C --------------------------------------
        STAFLG(FIXSTA)='F'
        STAOC(FIXSTA)=0
C
C LOOP: A CONNECTED NET WILL BE CORRECTED
C ---------------------------------------
        DO 113 L=1,10000
C
C CORRECT A PART OF A CONNECTED NET
C ---------------------------------
          ICHNFL=0
          DO 105 I=1,NFIL
            IF (TRPFLG(I).EQ.'G') THEN
              STA1=STFIL(1,I)
              STA2=STFIL(2,I)
              FSTA1=STAFLG(STA1)
              FSTA2=STAFLG(STA2)
              IF(((FSTA1.EQ.' ').AND.(FSTA2.NE.' ')).OR.
     1          ((FSTA1.NE.' ').AND.(FSTA2.EQ.' '))) THEN
                IF (FSTA1.EQ.' ') THEN
                  DO 107 J=1,3
                    DXHELP(J,STA1)=DXHELP(J,STA2)-DXFIL(J,I)
                    XSTARS(J,STA1)=XSTAT(J,STA1)+DXHELP(J,STA1)
107               CONTINUE
                  STAFLG(STA1)='T'
                  STAOC(STA1)=0
                ELSE
                  DO 108 J=1,3
                    DXHELP(J,STA2)=DXHELP(J,STA1)+DXFIL(J,I)
                    XSTARS(J,STA2)=XSTAT(J,STA2)+DXHELP(J,STA2)
108               CONTINUE
                  STAFLG(STA2)='T'
                  STAOC(STA2)=0
                END IF
                ICHNFL=ICHNFL+1
              END IF
            END IF
105       CONTINUE
C
C THE CORRECTION IS PROBABLY FINISHED
C -----------------------------------
          IF (ICHNFL.EQ.0) GOTO 111
C
113     CONTINUE
C
C IF ALL CORRECTIONS ARE MADE THEN THE LOOP HAS TO BE ENDED
C ---------------------------------------------------------
111     DO 125 M=1,NSTAT
          IF(STAOC(M).GE.1) END=.FALSE.
125     CONTINUE
        IF (END) GOTO 115
        END=.TRUE.
        FIXSTA=0
C
112   CONTINUE
C
C SAVE UPDATED COORDINATES
C ------------------------
115   DATUM=' '
      CALL WTSTAT(1,FILCOR,TITLE,DATUM,NSTAT,STNAME,XSTARS,
     1            STANUM,STAFLG,TIMCRD)
C
C PRINT PROTOCOLL
C ---------------
      WRITE(LFNPRT,17)
17    FORMAT(//,' TRIPLE DIFFERENCE SOLUTION COORDINATES SAVED',/)
      IFIX=0
      DO 122 I=1,NSTAT
        IF (STAFLG(I).EQ.'F') THEN
          IF (IFIX.EQ.0) THEN
            IFIX=1
            WRITE(LFNPRT,18) STNAME(I)
18          FORMAT(' FIXED STATION(S):  ',A)
          ELSE
            WRITE(LFNPRT,19) STNAME(I)
19          FORMAT('                    ',A)
          END IF
        END IF
122   CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
