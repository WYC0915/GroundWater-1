      MODULE s_COVSAV
      CONTAINS

C*
      SUBROUTINE COVSAV(FILCOV,TITLE,SIGMA,NOBS,NPARMS,NPAR,COV,LOCQ,
     1                  STNAME)
CC
CC NAME       :  COVSAV
CC
CC PURPOSE    :  PRINT COVARIANCE MATRIX OF STATION COORDINATES
CC
CC PARAMETERS :
CC         IN :  FILCOV : FILENAME                            CH*32
CC               TITLE  : GENERAL TITLE                       CH*80
CC               SIGMA  : MEAN ERROR OF UNIT WEIGHT           R*8
CC               NOBS   : NUMBER OF OBSERVATIONS              R*8
CC               NPARMS : NUMBER OF PARAMETERS FOR RMS COMP.  R*8
CC               NPAR   : NUMBER OF PARAMETERS                I*4
CC               COV    : UPPER TRIANGULAR PART OF COVARI-    R*8
CC                        ANCE MATRIX
CC               LOCQ(K,I),K=1,.,MAXLCQ,I=1,NPAR: PARAMETER   I*4
CC                        TION
CC               STNAME(I),I=1,..: STATION NAMES              CH*16
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/02 17:38
CC
CC CHANGES    :  28-DEC-92 : ??: USE OF SR "OPNFIL" TO OPEN FILES
CC               09-JUN-93 : EB: PRINT NUMBER OF OBERVATIONS AND NUMBER
CC                               OF UNKNOWNS
CC               03-AUG-93 : ??: STATION COORDINATES NOT ORDERD IN LOCQ IS
CC                               POSSIBLE
CC               15-APR-94 : ??: INCLUDE VARIANCE COVARIANCE OF VELOCITIES
CC               08-SEP-95 : ??: FILENAME POSSIBLE
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               08-FEB-07 : RD: NOBS/NPARMS -> I*4->R*8
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE s_opnfil
      USE s_gtflna
      USE s_opnerr
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IC    , IND   , IND0  , IOSTAT, IPAR  , IRC   , IST   ,
     1          ITYP  , KC    , KPAR  , KST   , MXCLCQ, NPAR
C
      REAL*8    NOBS  , NPARMS,SIGMA
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER*4    LOCQ(MXCLCQ,*)
      REAL*8       COV(*)
      CHARACTER*1  COOXYZ(3),FLG1,FLG2
      CHARACTER*6  MXNLCQ
      CHARACTER*16 STNAME(*)
      CHARACTER*32 FILCOV
      CHARACTER*80 TITLE
C
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
      DATA COOXYZ/'X','Y','Z'/
C
C GET COVARIANCE OUTPUT FILE NAME ( = BLANK : NO SAVE)
C ----------------------------------------------------
      IF (FILCOV.EQ.' ') THEN
        CALL GTFLNA(0,'COVARRS',FILCOV,IRC)
        IF(IRC.NE.0) RETURN
      ENDIF
      IF (FILCOV.EQ.' ') RETURN
C
C OPEN OUTPUT FILE
C ----------------
      CALL OPNFIL(LFNLOC,FILCOV,'UNKNOWN','FORMATTED',
     1            ' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILCOV,'COVSAV')
C
C WRITE TITLE
C -----------
      IF (NPARMS.GT.99999999d0) THEN
        WRITE(LFNLOC,10) TITLE,SIGMA,NOBS,NPARMS
10      FORMAT(A80,/,80('-'),//,
     1         'UPPER TRIANGULAR PART OF VARIANCE-COVARIANCE MATRIX ',
     2         'FOR COORDINATES/VELOCITIES:',/,78('-'),//,
     3         'RMS OF UNIT WEIGHT:',F8.4,2X,'# OBS:',E11.4,2X,
     4         '# UNKNOWNS:',E9.4,///,
     5         'STATION 1        XYZ    STATION 2        XYZ',
     6         ' FLG    MATRIX ELEMENT',/)
      ELSE
        WRITE(LFNLOC,11) TITLE,SIGMA,NINT(NOBS),NINT(NPARMS)
11      FORMAT(A80,/,80('-'),//,
     1         'UPPER TRIANGULAR PART OF VARIANCE-COVARIANCE MATRIX ',
     2         'FOR COORDINATES/VELOCITIES:',/,78('-'),//,
     3         'RMS OF UNIT WEIGHT:',F8.4,2X,'# OBS:',I11,2X,
     4         '# UNKNOWNS:',I9,///,
     5         'STATION 1        XYZ    STATION 2        XYZ',
     6         ' FLG    MATRIX ELEMENT',/)
      ENDIF
C
C LOOP OVER ALL PARAMETERS
C ------------------------
      DO 100 IPAR=1,NPAR
        ITYP=LOCQ(1,IPAR)
        IF(ITYP.NE.1.AND.ITYP.NE.20) GOTO 100
        IST=LOCQ(2,IPAR)
        IC=LOCQ(3,IPAR)
        IF (ITYP.EQ.1) THEN
          FLG1=' '
        ELSE
          FLG1='V'
        ENDIF
C
C LOOP OVER ALL LINES OF UPPER TRIANGULAR PART OF MATRIX
C ------------------------------------------------------
        IND0=IPAR*(IPAR-1)/2
        DO 50 KPAR=1,IPAR
          IF (LOCQ(1,KPAR).NE.1.AND.LOCQ(1,KPAR).NE.20) GOTO 50
          KST=LOCQ(2,KPAR)
          KC=LOCQ(3,KPAR)
          IF (LOCQ(1,KPAR).EQ.1) THEN
            FLG2=' '
          ELSE
            FLG2='V'
          ENDIF
          IND=IND0+KPAR
          WRITE(LFNLOC,30) STNAME(IST),COOXYZ(IC),
     1                     STNAME(KST),COOXYZ(KC),FLG1,FLG2,COV(IND)
C30        FORMAT(2(A16,2X,A1,5X),D20.10)
30        FORMAT(A16,2X,A1,5X,A16,2X,A1,1X,A1,1X,A1,1X,D20.10)
50      CONTINUE
        WRITE(LFNLOC,60)
60      FORMAT('   ')
100   CONTINUE
C
C CLOSE FILE
C ----------
      CLOSE(UNIT=LFNLOC)
C
      RETURN
      END SUBROUTINE

      END MODULE
