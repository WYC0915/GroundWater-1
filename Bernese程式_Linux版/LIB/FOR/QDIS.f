      MODULE s_QDIS
      CONTAINS

C*
      SUBROUTINE QDIS(XSTAT,ISTFIX,NFIX,XXX,QXX,NSTAT,LISTUS,
     1                DISOLD,DISNEW,QDD)
CC
CC NAME       :  QDIS
CC
CC PURPOSE    :  COMPUTE SLOPE DISTANCES BETWEEN ALL PARTICIPATING
CC               STATIONS
CC               COMPUTE VARIANCES OF ALL DISTANCES
CC
CC PARAMETERS :
CC         IN :  XSTAT  : COORDINATE LIST OF ALL STATIONS     R*8(3,1)
CC               ISTFIX : LIST OF FIXED STATIONS              I*4(1)
CC                        ISTFIX(I):   LOCATION IN XSTAT
CC               NFIX   : NUMBER OF FIXED STATIONS            I*4
CC               XXX    : SOLUTION VECTOR                     R*8(1)
CC               QXX    : VAR/COV MATRIX OF UNKNOWNS          R*8(1)
CC                          (UPPER TRIANGLE, COLUMNWISE LIN.)
CC               NSTAT  : NUMBER OF UNKNOWN STATIONS          I*4
CC               LISTUS : LIST OF UNKNOWN STATIONS            I*4(2,1)
CC                        LISTUS(1,I): LOCATION IN XSTAT
CC                        LISTUS(2,I): LOCATION OF X-COORD IN LOCQ
CC                        FIXED STATIONS ARE ADDED IN THIS SUBROUTINE
CC                        (LISTUS(2,I) OF FIXED STAT. = 0)
CC        OUT :  DISOLD : LINEARIZED MATRIX OF ALL POSSIBLE   R*8(1)
CC                        SLOPE DISTANCES (APPROX. VALUES)
CC                        UPPER TRIANGLE ONLY
CC               DISNEW : SAME, BUT ADJUSTED COORD. USED      R*8(1)
CC               QDD    : LINEARIZED MATRIX OF COV. OF ABOVE  R*8(1)
CC                        DISTANCES (UPPER TRIANGLE)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  W.GURTNER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/03 08:41
CC
CC CHANGES    :  20-DEC-93 : MR: CORRECT ERROR IN THE DERIVATIVES
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE f_ikf
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I    , IK   , IS   , IX   , J    , K    , KS   ,
     1          KX   , L    , NFIX , NSTAT, NTOT
C
      REAL*8    S    , XI   , XK   , YI   , YK   , ZI   , ZK
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER*4 LISTUS(2,*),ISTFIX(*)
      REAL*8    XSTAT(3,*),DISOLD(*),DISNEW(*),QDD(*),QXX(*)
      REAL*8    XXX(*),QSUB(6,6),DD(6)
C
C  ADD FIXED STATIONS TO STATION LIST OF UNKNOWN STATIONS
C
      DO 10 I=1,NFIX
C
C  KEEP INTERNAL STATION NUMBER
        LISTUS(1,NSTAT+I)=ISTFIX(I)
C  SET PLACE OF X-COORDINATE IN PARAMETER LIST TO ZERO
        LISTUS(2,NSTAT+I)=0
C
10    CONTINUE
C
C  TOTAL NUMBER OF STATIONS:
      NTOT=NSTAT+NFIX
C
      DO 20 J=1,NTOT*(NTOT+1)/2
        QDD(J)=0.D0
        DISOLD(J)=0.D0
        DISNEW(J)=0.D0
20    CONTINUE
C
      DO 30 K=1,NTOT
C
        DO 40 I=1,K-1
C
C
C  COMPUTE SLOPE DISTANCES BETWEEN STATION I AND K
C
          IS=LISTUS(1,I)
          KS=LISTUS(1,K)
          IX=LISTUS(2,I)
          KX=LISTUS(2,K)
C
C  USE APPROX. COORDINATES
          XI=XSTAT(1,IS)
          YI=XSTAT(2,IS)
          ZI=XSTAT(3,IS)
          XK=XSTAT(1,KS)
          YK=XSTAT(2,KS)
          ZK=XSTAT(3,KS)
          IK=K*(K-1)/2+I
          DISOLD(IK)=DSQRT((XI-XK)**2+(YI-YK)**2+(ZI-ZK)**2)
C
C  USE ADJUSTED COORDINATES
          IF(IX.NE.0) THEN
            XI=XI+XXX(IX)
            YI=YI+XXX(IX+1)
            ZI=ZI+XXX(IX+2)
          END IF
          IF(KX.NE.0) THEN
            XK=XK+XXX(KX)
            YK=YK+XXX(KX+1)
            ZK=ZK+XXX(KX+2)
          END IF
          DISNEW(IK)=DSQRT((XI-XK)**2+(YI-YK)**2+(ZI-ZK)**2)
C
C  IF BOTH STATIONS ARE HELD FIXED: QDD(IK)=0 --> NEXT COMBINATION
          IF(IX.EQ.0.AND.KX.EQ.0) GOTO 40
C
          DO 50 J=1,6
            DO 60 L=1,6
              QSUB(J,L)=0.D0
60          CONTINUE
            DD(J)=0.D0
50        CONTINUE
C
C  COMPUTE DERIVATIVES TO XI,YI,ZI,XK,YK,ZK
          DD(1)=(XI-XK)/DISNEW(IK)
          DD(4)=-DD(1)
          DD(2)=(YI-YK)/DISNEW(IK)
          DD(5)=-DD(2)
          DD(3)=(ZI-ZK)/DISNEW(IK)
          DD(6)=-DD(3)
C
C  FORM SUBMATRIX OUT OF QXX
          DO 80 J=1,3
            DO 90 L=1,3
C
              IF(IX.EQ.0) GOTO 100
              QSUB(J  ,L  )=QXX(IKF(IX+L-1,IX+J-1))
              IF(KX.EQ.0) GOTO 90
              QSUB(J  ,L+3)=QXX(IKF(KX+L-1,IX+J-1))
              QSUB(J+3,L  )=QXX(IKF(IX+L-1,KX+J-1))
100           QSUB(J+3,L+3)=QXX(IKF(KX+L-1,KX+J-1))
C
90          CONTINUE
80        CONTINUE
C
C  VARIANCE OF DISTANCE I,K = DD *QSUB * DD-T
          DO 110 L=1,6
            S=0.D0
            DO 120 J=1,6
              S=S+QSUB(J,L)*DD(J)
120         CONTINUE
            QDD(IK)=QDD(IK)+S*DD(L)
110       CONTINUE
C
40      CONTINUE
C
30    CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
