      MODULE s_QITPOL
      CONTAINS

C*
      SUBROUTINE QITPOL(NEP,NFCT,TTAB,TAB,T,FCT)
CC
CC NAME       :  QITPOL
CC
CC PURPOSE    :  QUADRAT. INTERPOLATE IN A TABLE WITH NFCT FUNCTIONS
CC               AND NEP TABULAR EPOCHS.
CC
CC PARAMETERS :
CC         IN :  NEP    : NUMBER OF EPOCHS                    I*4
CC               NFCT   : NUMBER OF FUNCTIONS TO BE INTERPOL. I*4
CC               TTAB(I),I=1,2,...,NEP: TABULAR EPOCHS        R*8
CC               TAB(I),I=1,2,...,NEP*NFCT: TABULAR VALUES    R*8
CC                        THE FIRST NFCT VALUES REFER TO
CC                        EPOCH TTAB(1), THE SECOND NFCT
CC                        VALUES TO TTAB(2) AND SO ON.
CC               T      : INTERPOLATION TIME                  R*8
CC        OUT :  FCT(I),I=1,..,NFCT: INTERPOLATED FUNCTION    R*8
CC                        VALUES
CC
CC REMARKS    :  THE TABULAR INTERVAL MAY BE POSITIVE OR NEGATIVE
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/12/11 12:02
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               31-OCT-12 : SS: KLUDGE FOR EXCEEDANCE OF TIME WINDOW
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
      INTEGER*4 I0  , I1  , I2  , IFCT, INT , NEP , NFCT , ISTOP
C
      REAL*8    A2  , T   , TREL
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 TTAB(*),TAB(*),FCT(*)
C
      ISTOP=1
C
      IF(NEP.LT.3) THEN
        WRITE(LFNERR,5) NEP
5       FORMAT(/,' *** SR QITPOL: NOT ENOUGH VALUES FOR QUADR. INTER',
     1                            'POLATION',/,
     2                       16X,'NUMBER OF VALUES:',I3,/)
        CALL EXITRC(2)
      END IF
      IF(TTAB(1).GT.TTAB(NEP))GO TO 100
      IF(T.GE.TTAB(1).AND.T.LE.TTAB(NEP))GO TO 20
      WRITE(LFNERR,10) T,TTAB(1),TTAB(NEP)
10    FORMAT(/,' *** SR QITPOL: ARGUMENT OUT OF RANGE',/,
     1                     16X,'ARGUMENT       :',F12.3,/,
     2                     16X,'RANGE START    :',F12.3,/,
     3                     16X,'RANGE END      :',F12.3,/)
      IF (ISTOP.EQ.1) CALL EXITRC(2)
      IF (ISTOP.EQ.0) RETURN
20    CONTINUE
      DO 30 INT=2,NEP-1
      IF(T.LE.TTAB(INT+1))GO TO 200
30    CONTINUE
100   IF(T.LE.TTAB(1).AND.T.GE.TTAB(NEP))GO TO 110
      WRITE(LFNERR,10)T,TTAB(1),TTAB(NEP)
      IF (ISTOP.EQ.1) CALL EXITRC(2)
      IF (ISTOP.EQ.0) RETURN
110   CONTINUE
      DO 120 INT=2,NEP-1
      IF(T.GE.TTAB(INT+1))GO TO 200
120   CONTINUE
200   TREL=(T-TTAB(INT-1))/(TTAB(INT)-TTAB(INT-1))
      DO 210 IFCT=1,NFCT
      I0=(INT-2)*NFCT
      I1=I0+NFCT
      I2=I1+NFCT
      A2=(TAB(I2+IFCT)-2*TAB(I1+IFCT)+TAB(I0+IFCT))/2
      FCT(IFCT)=TAB(I0+IFCT)+TREL*(TAB(I1+IFCT)-TAB(I0+IFCT)-A2)
     1          +A2*TREL*TREL
210   CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
