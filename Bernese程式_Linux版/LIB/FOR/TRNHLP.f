      MODULE s_TRNHLP
      CONTAINS

C*
      SUBROUTINE TRNHLP(NLINES,NCOL,MAXLIN,MAXCOL,INDCOE,NLPCOL,IDXCOE)
CC
CC NAME       :  TRNHLP
CC
CC PURPOSE    :
CC
CC PARAMETERS :
CC        IN  : NLINES  : NUMBER OF LINES OF TRANSFORMATION MATRIX  I*4
CC              NCOL    : NUMBER OF COLUMNS OF TRANSFORMATION MATRIX I*4
CC              MAXLIN  : NUMBER OF NON-ZERO ELEMENTS IN COLUMN K   I*4
CC                        OF TRANSFORMATION MATRIX
CC              MAXCOL  : MAXIMUM NUMBER OF COLUMNS                 I*4
CC              INDCOE  : IPAR, KPAR FOR NON-ZERO COEFFICIENTS      I*4
CC        OUT : NLPCOL  : NUMBER OF NON-ZERO ELEMENTS IN LINE K     I*2
CC              IDXCOE :  INDEX FOR COEFFICIENTS OF LINE K          I*2
CC
CC REMARKS    : SR TO SPEED UP SR TRAFO1 AND TRAFO3
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.6  (MAY 95)
CC
CC CREATED    :  01-MAY-95
CC
CC CHANGES    :  21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1995     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IELE  , IPAR  , KPAR  , MAXCOL, MAXLIN, NCOL  ,
     1          NLINES
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
      INTEGER*4 INDCOE(2,*)
      INTEGER*4 NLPCOL(*),IDXCOE(2,MAXLIN,*)
C
C CHECK MAX NUMBER OF COLUMNS
C ---------------------------
      IF(NCOL.GT.MAXCOL)THEN
        WRITE(LFNERR,1)NCOL,MAXCOL
1       FORMAT(//,' ** SR TRNHLP : NUMBER OF PARMS',I5,
     1            ' GT DIM MAXCOL :',I5,//)
        CALL EXITRC(2)
      END IF
C
C INITIALIZE NUMBER OF LINES PER COLUMN
      DO 10 I=1,NCOL
        NLPCOL(I)=0
10    CONTINUE
      DO 100 IELE=1,1000000
        IF(INDCOE(1,IELE).EQ.-1)GO TO 110
        IPAR=INDCOE(1,IELE)
        KPAR=INDCOE(2,IELE)
        NLPCOL(KPAR)=NLPCOL(KPAR)+1
        IF(NLPCOL(KPAR).GT.MAXLIN)THEN
          WRITE(LFNERR,20)MAXLIN
20        FORMAT(//,' ** SR TRNHLP : DIMENSION MAXLIN TOO SMALL : ',
     1           I5,//)
          CALL EXITRC(2)
        END IF
        IDXCOE(1,NLPCOL(KPAR),KPAR)=IPAR
        IDXCOE(2,NLPCOL(KPAR),KPAR)=IELE
100   CONTINUE
110   CONTINUE
999   CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
