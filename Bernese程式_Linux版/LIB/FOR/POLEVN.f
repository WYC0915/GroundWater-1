      MODULE s_POLEVN
      CONTAINS

C*
      SUBROUTINE POLEVN(N,Q,DIM,DT,H,COE,XV)
CC
CC NAME       :  POLEVN
CC
CC PURPOSE    : BERECHNUNG VON 0-TER ABLEITUNG
CC              BIS (N-1)-TER ABLEITUNG DES POLYNOMS
CC
CC PARAMETERS :
CC        IN  : N       : Maximale Ableitung + 1                    I*4
CC              Q       : Polynomgrad                               I*4
CC              DT      : Relatives Zeitargument                    R*8
CC              H       : Normierungskonstante                      R*8
CC              COE     : Koeffizienten                             R*8
CC        OUT : XV      : 0-te bis n-te Ableitungen                 R*8
CC                        zur (relativen) Zeit DT
CC
CC REMARKS    :
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  4.1  (MAY 94)
CC
CC CREATED    :  96/10/06
CC
CC CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1994     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE M_BERN
      USE d_const
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IABL  , IFIRST, ITERM , K     , MAXTRM, N
C
      REAL*8    DT    , DTREL , H
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
      PARAMETER (MAXTRM=40)
      INTEGER*4 Q,DIM
      REAL*8   COE(DIM,*),XV(DIM,*)
      REAL*8   TIMPOT(MAXTRM),myFAKULT(MAXTRM)
C      INCLUDE 'I:COMLFNUM'
      DATA IFIRST/1/
C
C Initialisieren
C --------------
      IF(IFIRST.EQ.1)THEN
        IFIRST=0
C
C Polynomgrad pruefen
        IF(Q+1.GT.MAXTRM)THEN
          WRITE(LFNERR,1)Q
1         FORMAT(//,' MAXIMUM POLYNOMIAL DEGREE Q=',I3,' TOO HIGH',//)
          CALL EXITRC(2)
        END IF
C
C Fakultaeten berechnen
        MYFAKULT(1)=1.D0
        DO 10 I=1,MAXTRM-1
          MYFAKULT(I+1)=MYFAKULT(I)*I
10      CONTINUE
      END IF
C
C Potenzen des Zeitarguments
C --------------------------
      DTREL=DT/H
      TIMPOT(1)=1.D0
      DO 20 I=1,Q
        TIMPOT(I+1)=TIMPOT(I)*DTREL
20    CONTINUE
C
C Initialisieren des Resultats
C ----------------------------
C
C Auswerten der Taylorreihe ("von hinten")
C fuer jede Ableitung
C ----------------------------------------
      DO 100 IABL=1,N
        I=IABL-1
C
C Initialisieren des Resultates
        DO 30 K=1,DIM
          XV(K,IABL)=0.D0
30      CONTINUE
C
C Summieren der Terme
        DO 90 ITERM=Q+1,IABL,-1
          DO 80 K=1,DIM
            XV(K,IABL)=XV(K,IABL)+TIMPOT(ITERM-I)*
     1                 MYFAKULT(ITERM)/MYFAKULT(ITERM-I)*
     2                 COE(K,ITERM)
80        CONTINUE
90      CONTINUE
        DO 95 K=1,DIM
          XV(K,IABL)=XV(K,IABL)/H**I
95      CONTINUE
100   CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
