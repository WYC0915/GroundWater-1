      MODULE s_CORSAV
      CONTAINS

C*
      SUBROUTINE CORSAV(TITLE,NSTAT,STNAME,ICENTR,STAFLG,
     1                  XSTAT,XSTECC,NFIX,STFIX,NPAR,LOCQ,XXX,
     2                  TIMCRD,CLKHED,NOBSPA)
CC
CC NAME       :  CORSAV
CC
CC PURPOSE    :  WRITE NEW STATION COORDINATES (ESTIMATED BY GPSEST)
CC               ON AN OUTPUT FILE. ALL COORDINATES ARE WRITTEN
CC               (WHETHER IT WAS A STATION USED, NOT USED OR HELD FIXED)
CC
CC PARAMETERS :
CC         IN :  TITLE  : TITLE FOR FILE                      CH*80
CC               NSTAT  : NUMBER OF STATIONS                  I*4
CC               STNAME(I),I=1,..,NSTAT: STATION NAMES        CH*16
CC               ICENTR(I),I=1,..,NSTAT: INDEX OF CENTER      I*4
CC                        STATION FOR STATION I
CC               XSTAT(K,I),K=1,2,3, I=1,2,..,NSTAT           R*8
CC                        A PRIORI VALUES FOR STATION COORDINATES
CC               XSTECC(K,I),K=1,2,3, I=1,..,NSTAT: ECCENTR.  R*8
CC                        VECTOR FOR STATION I TO ITS CENTER
CC               NFIX   : NUMBER OF FIXED STATIONS            I*4
CC               STFIX(I),I=1,2,...,NFIX: NUMBERS OF THE      I*4(1)
CC                        STATIONS HELD FIXED
CC               NPAR   : TOTAL NUMBER OF PARAMETERS          I*4
CC               LOCQ(K,I),K=1,..,MAXLCQ,I=1,2,...            I*4
CC                        CHARACTERIZATION OF PARAMETERS
CC               XXX(I),I=1,2,....                            R*8
CC                        SOLUTION VECTOR
CC               TIMCRD : EPOCH OF COORDINATES (MJD)          R*8
CC               CLKHED : CLOCK HEADER INFORMATION            T_CLKHEAD
CC                          %NUMREF=0: FIX REF-CLOCKS
CC                          %NUMREF=2: SUM FOR REF-CLOCKS
CC               NOBSPA : NUM.OF OBSERV PER PARAMETER         I*4(*,*)
CC                        NOBSPA(MAXMEA*ISYS+IMEA,IPAR)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/10/30 15:13
CC
CC CHANGES    :  30-SEP-93 : ??: CALL TO SR WTSTAT CHANGED (STANUM,DATUM)
CC               10-AUG-94 : MR: CALL EXITRC
CC               16-MAY-97 : LM: DATUM*16
CC               04-AUG-99 : PF: CALL WTSTAT ADAPTED (FILCOR, TIMCRD
CC                               ADDED)
CC               28-JUN-04 : RD: USE MAXSTA FROM P_GPSEST
CC               16-JUL-04 : RD: UPDATE COORDINATES IN CLKHED RECORD
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               15-AUG-05 : RD: ALLOCATE MAXSTA DYNAMICALLY
CC               15-APR-10 : DT: ADD NOBSPA; NO FLAG IF NOBSPA=0
CC               11-FEB-11 : RD: NEW CALL OF WTSTAT
CC               14-FEB-11 : RD: REMOVE MAXSTA-COMMON (UNUSED)
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE d_clkrnx, ONLY: t_clkhead
      USE m_global, ONLY: MAXSYS
      USE p_gpsest, ONLY: MAXMEA
C
      USE s_wtstat
      USE s_exitrc
      USE s_maxtst
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , ICOR  , IFIX  , IPAR  , IRC   , ISTA  , ISTAT ,
     1          MXCLCQ, NFIX  , NPAR  , NSTAT
C
      REAL*8    TIMCRD
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C GLOBAL DIMENSIONS
      TYPE(t_clkhead) CLKHED
      CHARACTER*16 STNAME(*)
      CHARACTER*80 TITLE
      CHARACTER*1  STAFLG(*)
      REAL*8       XSTAT(3,*),XSTECC(3,*),XXX(*)
      INTEGER*4    LOCQ(MXCLCQ,*),STFIX(*),ICENTR(*)
      INTEGER*4    NOBSPA(:,:)
C
C LOCAL DIMENSIONS
      CHARACTER*6  MXNLCQ
      CHARACTER*16 DATUM
      CHARACTER*32 FILCOR
      REAL*8       XSTNEW(3,NSTAT)
      INTEGER*4    STANUM(NSTAT)
      INTEGER*4    SUMOBS
C
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
C
C COMPUTE NEW STATION COORDINATES FOR ALL STATIONS
C ------------------------------------------------
      DO 100 ISTAT=1,NSTAT
        DO 10 I=1,3
          XSTNEW(I,ISTAT)=XSTAT(I,ISTAT)
10      CONTINUE
        STAFLG(ISTAT)=' '
C
C FIND OUT WHETHER STATION HAS BEEN FIXED
C ---------------------------------------
        DO 20 IFIX=1,NFIX
          IF(STFIX(IFIX).EQ.ICENTR(ISTAT)) THEN
            STAFLG(ISTAT)='F'
            GOTO 30
          ENDIF
20      CONTINUE
30      CONTINUE
C
C FIND WHETHER STATION-COORDINATES HAVE BEEN ESTIMATED
C ----------------------------------------------------
        DO 60 IPAR=1,NPAR
          IF(LOCQ(1,IPAR).NE.1) GOTO 70
          IF(LOCQ(2,IPAR).EQ.ICENTR(ISTAT)) THEN
            ICOR=LOCQ(3,IPAR)
C
C SET FLAG
            IF(ICENTR(ISTAT).EQ.ISTAT) THEN
              STAFLG(ISTAT)='P'
            ELSE
              STAFLG(ISTAT)='E'
            ENDIF
C
C GET SUM OF ALL OBSERVATIONS FOR THE ACTUAL STATION
C --------------------------------------------------
            SUMOBS=0
            DO I=1,MAXMEA*MAXSYS
              SUMOBS = SUMOBS + NOBSPA(I,IPAR)
            END DO
            IF (SUMOBS.EQ.0) STAFLG(ISTAT)=' '
C
C SET NEW COORDINATES
            XSTNEW(ICOR,ISTAT)=XSTAT(ICOR,ICENTR(ISTAT))
     1                           +XSTECC(ICOR,ISTAT)+XXX(IPAR)
          END IF
60      CONTINUE
70      CONTINUE
C
C UPDATE COORDINATES FOR THE CLOCK RINEX FILES
        DO ISTA=1,CLKHED%NSTA
          IF (CLKHED%CLKNAME(ISTA).NE.STNAME(ISTAT)) CYCLE
          CLKHED%STACOORD(1:3,ISTA)=XSTNEW(1:3,ISTAT)
          EXIT
        ENDDO
100   CONTINUE
C
C SAVE COORDINATES ON FILE
      DATUM=' '
      FILCOR=' '
      CALL WTSTAT(1,FILCOR,TITLE,DATUM,NSTAT,STNAME,XSTNEW,
     1            STANUM,STAFLG,TIMCRD)
C
      RETURN
      END SUBROUTINE

      END MODULE
