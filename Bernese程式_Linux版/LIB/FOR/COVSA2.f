      MODULE s_COVSA2
      CONTAINS

C*
      SUBROUTINE COVSA2(FILNAM,TITLE,SIGMA,NOBS,NPARMS,NPAR,COV,STNAME)
CC
CC NAME       :  COVSA2
CC
CC PURPOSE    :  PRINT COVARIANCE MATRIX OF STATION COORDINATES
CC
CC PARAMETERS :
CC         IN :  FILNAM : FILNAME OF COVARIANCE FILE          CH*32
CC               TITLE  : GENERAL TITLE                       CH*80
CC               SIGMA  : MEAN ERROR OF UNIT WEIGHT           R*8
CC               NOBS   : NUMBER OF OBSERVATIONS              I*4
CC               NPARMS : NUMBER OF PARAMETERS FOR RMS COMP.  I*4
CC               NPAR   : NUMBER OF PARAMETERS                I*4
CC               COV    : UPPER TRIANGULAR PART OF COVARI-    R*8
CC                        ANCE MATRIX
CC               STNAME(I),I=1,..: STATION NAMES              CH*16
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  E.BROCKMANN
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  5-MAR-93
CC
CC CHANGES    :  31-AUG-95 : EB: NEW CALL (+FILNAM)
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1993     UNIVERSITY OF BERN
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
      INTEGER*4 ICO   , IND   , IND0  , IOSTAT, IPAR  , IRC   , ISTAT ,
     1          KCO   , KPAR  , KSTAT , NOBS  , NPAR  , NPARMS
C
      REAL*8    SIGMA
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      REAL*8       COV(*)
      CHARACTER*1  COOXYZ(3)
      CHARACTER*16 STNAME(*)
      CHARACTER*32 FILCOV,FILNAM
      CHARACTER*80 TITLE
C
      DATA COOXYZ/'X','Y','Z'/
C
C GET COVARIANCE OUTPUT FILE NAME ( = BLANK : NO SAVE)
C ----------------------------------------------------
      IF (FILNAM.EQ.' ') THEN
        CALL GTFLNA(0,'COVARRS',FILCOV,IRC)
        IF(IRC.NE.0) RETURN
      ELSE
        FILCOV=FILNAM
      ENDIF
C
C OPEN OUTPUT FILE
C ----------------
      CALL OPNFIL(LFNLOC,FILCOV,'UNKNOWN','FORMATTED',
     1            ' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILCOV,'COVSAV')
C
C WRITE TITLE
C -----------
      WRITE(LFNLOC,10) TITLE,SIGMA,NOBS,NPARMS
10    FORMAT(A80,/,80('-'),//,
     1       'UPPER TRIANGULAR PART OF VARIANCE-COVARIANCE MATRIX ',
     2       'FOR COORDINATES:',/,67('-'),//,
     3       'RMS OF UNIT WEIGHT:',F8.4,2X,'# OBS:',I11,2X,
     4       '# UNKNOWNS:',I9,///,
     5       'STATION 1        XYZ    STATION 2        XYZ',
     6       '        MATRIX ELEMENT',/)
C
C LOOP OVER ALL PARAMETERS
C ------------------------
      DO 100 IPAR=1,NPAR
C
C LOOP OVER ALL LINES OF UPPER TRIANGULAR PART OF MATRIX
C ------------------------------------------------------
        IND0=IPAR*(IPAR-1)/2
        ISTAT=(IPAR-MOD(IPAR-1,3))/3+1
        ICO=MOD(IPAR-1,3)+1
        DO 50 KPAR=1,IPAR
          KSTAT=(KPAR-MOD(KPAR-1,3))/3+1
          KCO=MOD(KPAR-1,3)+1
          IND=IND0+KPAR
          WRITE(LFNLOC,30) STNAME(ISTAT),COOXYZ(ICO),
     1                     STNAME(KSTAT),COOXYZ(KCO),COV(IND)
30        FORMAT(2(A16,2X,A1,5X),D20.10)
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
