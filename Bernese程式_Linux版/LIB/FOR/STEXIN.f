      MODULE s_STEXIN
      CONTAINS

C*
      SUBROUTINE STEXIN(NSTAT ,STNAME,allStaName   ,NCENTR,ICENTR,
     1                  NFIX  ,NKIN  ,NCLREQ,NSTWGT,NTRSTA,STFIX ,
     2                  STKIN ,ISTCLK,ISTWGT,STATRP,NCLKST,CLKSTA,
     3                  NRGB  ,STARGB)
CC
CC NAME       :  STEXIN
CC
CC PURPOSE    :  TRANSFORM EXTERNAL STATION NUMBERS INTO INTERNAL ONES
CC               CHECK VALIDITY OF REQUESTS
CC
CC PARAMETERS :
CC         IN :  NSTAT  : NUMBER OF STATIONS                  I*4
CC               STNAME(I),I=1,..,NSTAT: EXTERNAL STATION     CH*16(*)
CC                        NAMES
CC               allStaName(I),I=1,..,NSTAT: SORTED LIST OF   CH*16(*)
CC                        STATION NAMES
CC               NCENTR : NUMBER OF NOT-DIRECTLY OBSERVED     I*4
CC                        CENTER STATIONS
CC               ICENTR(I),I=1,..,NSTAT: INDEX OF CENTER      I*4
CC                        STATION BELONGING TO STATION NUM. I
CC               NFIX   : NUMBER OF FIXED STATIONS            I*4
CC               NKIN   : # OF STATIONS EST. IN KIN. MODUS    I*4
CC               NCLREQ : NUMBER OF CLOCK REQUESTS            I*4
CC               NSTWGT : NUMBER OF STATIONS WITH A PRIORI    I*4
CC                        WEIGHTS
CC               NTRSTA : NUMBER OF TROPOSPHERE REQUESTS FOR  I*4
CC                        INDIVIDUAL STATIONS
CC               NCLKST : NUMBER OF EPOCH WISE STATION CLOCKS I*4
CC               CLKSTA(I),I=1,..,MAXSTA: STATION NUMBERS FOR I*4
CC                        CLOCK ESTIMATION
CC               NRGB   : NUMBER OF SLR RANGE BIAS REQUESTS   I*4
CC               STARGB(I), I=1,...NRGB: Station numbers for  I*4
CC                        Range Bias requests
CC     IN/OUT :  STFIX(I),I=1,..,NFIX: FIXED STATION NUMBERS  I*4(*)
CC               STKIN(I),I=1,..,NKIN: KIN. STATION NUMBERS   I*4(*)
CC               ISTCLK(I),I=1,..,NCLREQ: STATIONS FOR CLOCK  I*4(*)
CC                        REQUESTS
CC               ISTWGT(I),I=1,..,NSTWGT: STATIONS WITH A     I*4(*)
CC                        PRIORI WEIGHTS
CC               STATRP(I),I=1,..,NTRSTA: STATIONS WITH TRO-  I*4(*)
CC                        POSPHERE PARAMETER
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/07 15:06
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC               08-JUN-95 : LM: KINEMATIC COORDINATES
CC               31-AUG-95 : MR,SS: REMOVE INVALID REQUESTS
CC               27-MAR-96 : TS: EPOCH WISE STATION CLOCKS
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               16-JAN-11 : RD: USE STNAME INSTEAD OF STANUM
CC               16-JAN-11 : RD: ADD RANGE BIASES
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 ICLK  , ICLR  , IFIX  , IKIN  , ISTAT , ISTW  , ITRP  ,
     1          NCENTR, NCLKNW, NCLKST, NCLREQ, NFIX  , NFIXNW, NKIN  ,
     2          NKINNW, NSTAT , NSTWGT, NTRSTA, IRGB  , NRGB  , NRGBNW
C
CCC       IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*16 STNAME(*),allStaName(*)
      INTEGER*4  ICENTR(*),STFIX(*) ,ISTCLK(*),ISTWGT(*)
      INTEGER*4  STATRP(*),STKIN(*) ,CLKSTA(*),STARGB(*)
C
C
C FIXED STATIONS
C --------------
      DO 20 IFIX=1,NFIX
        DO 10 ISTAT=1,NSTAT
          IF(STNAME(ISTAT).EQ.allStaName(STFIX(IFIX))) THEN
            IF(ICENTR(ISTAT).EQ.ISTAT) THEN
              STFIX(IFIX)=ISTAT
              GOTO 20
            ENDIF
            WRITE(LFNERR,1) STFIX(IFIX)
1           FORMAT(/,' *** SR STEXIN: INVALID FIXED STATION REQUEST',/,
     1                      16X,'ONLY CENTER STATIONS MAY BE FIXED',/,
     2                      16X,'STATION NUMBER: ',I3,/)
            CALL EXITRC(2)
          ENDIF
10      CONTINUE
        WRITE(LFNERR,2) STFIX(IFIX)
2       FORMAT(/,' ### SR STEXIN: INVALID FIXED STATION REQUEST',/,
     1                       16X,'STATION DOES NOT OCCUR ',
     2                           'AND WAS REMOVED FROM LIST',/,
     3                       16X,'STATION NUMBER: ',I3,/)
        STFIX(IFIX)=-1
20    CONTINUE
C
C REMOVE INVALID FIXED STATIONS
C -----------------------------
      NFIXNW=0
      DO 110 IFIX=1,NFIX
        IF (STFIX(IFIX).NE.-1) THEN
          NFIXNW=NFIXNW+1
          STFIX(NFIXNW)=STFIX(IFIX)
        END IF
110   CONTINUE
      NFIX=NFIXNW
C
C KINEMATIC COORDINATES
C ---------------------
      DO 15 IKIN=1,NKIN
        DO 16 ISTAT=1,NSTAT
          IF(STNAME(ISTAT).EQ.allStaName(STKIN(IKIN))) THEN
            IF(ICENTR(ISTAT).EQ.ISTAT) THEN
              STKIN(IKIN)=ISTAT
              GOTO 15
            ENDIF
            WRITE(LFNERR,301) STKIN(IKIN)
301         FORMAT(/,' *** SR STEXIN: INVALID KIN. STATION REQUEST',/,
     1                      16X,'ONLY CENTER STATIONS MAY BE KIN.',/,
     2                      16X,'STATION NUMBER: ',I3,/)
            CALL EXITRC(2)
          ENDIF
16      CONTINUE
        WRITE(LFNERR,302) STKIN(IKIN)
302     FORMAT(/,' ### SR STEXIN: INVALID KIN. STATION REQUEST',/,
     1                       16X,'STATION DOES NOT OCCUR ',
     2                           'AND WAS REMOVED FROM LIST',/,
     3                       16X,'STATION NUMBER: ',I3,/)
        STKIN(IKIN)=-1
15    CONTINUE
C
C REMOVE INVALID KIN. STATIONS
C ----------------------------
      NKINNW=0
      DO 120 IKIN=1,NKIN
        IF (STKIN(IKIN).NE.-1) THEN
          NKINNW=NKINNW+1
          STKIN(NKINNW)=STKIN(IKIN)
        END IF
120   CONTINUE
      NKIN=NKINNW
C
C CLOCK REQUESTS
C --------------
      DO 40 ICLR=1,NCLREQ
        DO 30 ISTAT=1,NSTAT-NCENTR
          IF(STNAME(ISTAT).EQ.allStaName(ISTCLK(ICLR))) THEN
            ISTCLK(ICLR)=ISTAT
            GOTO 40
          ENDIF
30      CONTINUE
        WRITE(LFNERR,3) ISTCLK(ICLR)
3       FORMAT(/,' *** SR STEXIN: INVALID CLOCK REQUEST',/,
     1                       16X,'STATION DOES NOT OCCUR',/,
     2                       16X,'STATION NUMBER: ',I3,/)
        CALL EXITRC(2)
40    CONTINUE
C
C A PRIORI STATION WEIGHTS
C ------------------------
      DO 60 ISTW=1,NSTWGT
        DO 50 ISTAT=1,NSTAT
          IF(STNAME(ISTAT).EQ.allStaName(ISTWGT(ISTW))) THEN
            IF(ICENTR(ISTAT).EQ.ISTAT) THEN
              ISTWGT(ISTW)=ISTAT
              GOTO 60
            ENDIF
            WRITE(LFNERR,4) ISTWGT(ISTW)
4           FORMAT(/,' *** SR STEXIN: A PRIORI SIGMAS NOT ALLOWED',/,
     1                       16X,'ALLOWED ONLY FOR CENTER STATIONS',/,
     2                       16X,'STATION NUMBER: ',I3,/)
            CALL EXITRC(2)
          ENDIF
50      CONTINUE
        WRITE(LFNERR,5) ISTWGT(ISTW)
5       FORMAT(/,' *** SR STEXIN: A PRIORI SIGMAS NOT ALLOWED',/,
     1                       16X,'STATION DOES NOT OCCUR',/,
     2                       16X,'STATION NUMBER: ',I3,/)
        CALL EXITRC(2)
60    CONTINUE
C
C TROPOSPHERE REQUESTS
C --------------------
      DO 80 ITRP=1,NTRSTA
        DO 70 ISTAT=1,NSTAT-NCENTR
          IF(STNAME(ISTAT).EQ.allStaName(STATRP(ITRP))) THEN
            STATRP(ITRP)=ISTAT
            GOTO 80
          ENDIF
70      CONTINUE
        WRITE(LFNERR,6) STATRP(ITRP)
6       FORMAT(/,' *** SR STEXIN: INVALID TROPOSPHERE REQUEST',/,
     1                       16X,'STATION DOES NOT OCCUR',/,
     2                       16X,'STATION NUMBER: ',I3,/)
        CALL EXITRC(2)
80    CONTINUE
C
C RECEIVER CLOCK REQUESTS (EPOCH WISE)
C ------------------------------------
      DO 100 ICLK=1,NCLKST
        DO 90 ISTAT=1,NSTAT-NCENTR
          IF(STNAME(ISTAT).EQ.allStaName(CLKSTA(ICLK))) THEN
CC            IF(ICENTR(ISTAT).EQ.ISTAT) THEN
              CLKSTA(ICLK)=ISTAT
              GOTO 100
CC            ENDIF
          ENDIF
90      CONTINUE
        WRITE(LFNERR,7) CLKSTA(ICLK)
7       FORMAT(/,' ### SR STEXIN: INVALID RECEIVER CLOCK REQUEST',/,
     1                       16X,'STATION DOES NOT OCCUR',/,
     2                       16X,'STATION NUMBER: ',I3,/)
        CLKSTA(ICLK)=-1
100   CONTINUE
C
C REMOVE INVALID STATIONS CLOCKS
C ------------------------------
      NCLKNW=0
      DO 130 ICLK=1,NCLKST
        IF (CLKSTA(ICLK).NE.-1) THEN
          NCLKNW=NCLKNW+1
          CLKSTA(NCLKNW)=CLKSTA(ICLK)
        END IF
130   CONTINUE
      NCLKST=NCLKNW
C
C RANGE BIAS PARAMETERS
C ---------------------
      DO 150 IRGB=1,NRGB
        DO 160 ISTAT=1,NSTAT-NCENTR
          IF(STNAME(ISTAT).EQ.allStaName(STARGB(IRGB))) THEN
CC            IF(ICENTR(ISTAT).EQ.ISTAT) THEN
              STARGB(IRGB)=ISTAT
              GOTO 150
CC            ENDIF
          ENDIF
160      CONTINUE
        WRITE(LFNERR,8) STARGB(IRGB)
8       FORMAT(/,' ### SR STEXIN: INVALID RANGE BIAS REQUEST',/,
     1                       16X,'STATION DOES NOT OCCUR',/,
     2                       16X,'STATION NUMBER: ',I3,/)
        STARGB(IRGB)=-1
150   CONTINUE
C
C REMOVE INVALID RANGE BIAS
C -------------------------
      NRGBNW=0
      DO 170 IRGB=1,NRGB
        IF (STARGB(IRGB).NE.-1) THEN
          NRGBNW=NRGBNW+1
          STARGB(NRGBNW)=STARGB(IRGB)
        END IF
170   CONTINUE
      NRGB=NRGBNW
C
      RETURN
      END SUBROUTINE

      END MODULE
