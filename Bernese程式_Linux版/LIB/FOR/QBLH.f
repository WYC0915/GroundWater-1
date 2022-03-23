      MODULE s_QBLH
      CONTAINS

C*
      SUBROUTINE QBLH(MAXSTA,LOCQ,NPAR,XSTAT,QXX,QB,QL,QH,NSTAT,LISTUS)
CC
CC NAME       :  QBLH
CC
CC PURPOSE    :  COMPUTE COV.MATRICES WITH RESPECT TO LAT,LONG,HEIGHT
CC               COMPLETE FOR ALL UNKNOWN STATIONS
CC               (NO CROSS CORRELATIONS B-L,B-H,L-H)
CC
CC PARAMETERS :
CC         IN :  MAXSTA : MAXIMUM NUMBER OF STATIONS          I*4
CC               LOCQ   : PARAMETER CHARACTERISATION LIST     I*4(*,1)
CC               NPAR   : TOTAL NUMBER OF PARAMETERS          I*4
CC               XSTAT  : COORDINATE LIST OF ALL STATIONS     R*8(3,1)
CC               QXX    : VAR/COV MATRIX OF UNKNOWNS          R*8(1)
CC                        (UPPER TRIANGLE,COLUMNWISE LIN.)
CC         OUT:  QB,QL,QH: COV.MATRICES OF LAT,LONG,HEIGHT    R*8(1)
CC                        (UPPER TRIANGLE, LINEARIZED)
CC               NSTAT  : NUMBER OF UNKNOWN STATIONS          I*4
CC               LISTUS : LIST OF UNKNOWN STATIONS            I*4(2,1)
CC                        LISTUS(1,I): LOCATION IN XSTAT
CC                        LISTUS(2,I): LOCATION OF X-COORD IN LOCQ
CC
CC REMARKS    :  MATRICES DB,DL,DH HAVE TO BE DECLARED
CC               ACCORDING TO MAXIMUM NUMBER OF UNKNOWN STATIONS
CC
CC AUTHOR     :  W. GURTNER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/03 08:36
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC               18-SEP-95 : JJ: INCREASE MAXSTA TO 200
CC               28-JUN-04 : RD: USE MAXSTA FROM P_GPSEST
CC               08-JUL-04 : RD: MAXSTA IS AN INPUT PARAMETER
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-JUN-05 : MM: UNUSED VARIABLES REMOVED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
C
      USE f_ikf
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IKBLH , IKQX  , IQXJ  , J     , K     ,
     1          KQXL  , L     , MAXSTA, MXCLCQ, NPAR  , NSTAT
C
      REAL*8    CB    , CH    , CL    , D     , D2    , R     , R2    ,
     1          X     , Y     , Z
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER*4 LOCQ(MXCLCQ,*),LISTUS(2,*)
      REAL*8    XSTAT(3,*),DB(3,MAXSTA),DL(3,MAXSTA),DH(3,MAXSTA)
      REAL*8    QB(*),QL(*),QH(*),QXX(*)
      CHARACTER*6 MXNLCQ
c      COMMON/CQBLH/DB,DL,DH
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
C
C  COMPILE INTERNAL STATION LIST OF UNKNOWN STATIONS
C
      NSTAT=0
C
      DO 10 I=1,NPAR
C
C  IF NOT UNKNOWN STATION COORDINATE: NEXT ELEMENT
      IF(LOCQ(1,I).NE.1) GOTO 10
C  IF NOT X-COORDINATE:               NEXT ELEMENT
      IF(LOCQ(3,I).NE.1) GOTO 10
C  KEEP NUMBER OF UNKNOWN STATIONS
      NSTAT=NSTAT+1
C  KEEP INTERNAL STATION NUMBER
      LISTUS(1,NSTAT)=LOCQ(2,I)
C  KEEP PLACE OF X-COORDINATE IN PARAMETER LIST (AND Q MATRIX)
      LISTUS(2,NSTAT)=I
C
10    CONTINUE
C
C  COMPUTE DIFFERENTIALS DB,DL,DH/DX,DY,DZ
C
C    DB,DL,DH: DIFFERENTIALS
C              E.G. DB(1,I)=D-LAT / D-X  OF I-TH STATION
C                   DB(2,I)=D-LAT / D-Y
C                   DB(3,I)=D-LAT / D-Z
C                   DB: LATITUDE, DL: LONGITUDE, DH: HEIGHT
C
      DO 20 I=1,NSTAT
C
C  PLACE OF I-TH UNKNOWN STATION IN LIST OF ALL STATIONS
      K=LISTUS(1,I)
C
C  ITS COORDINATES
      X=XSTAT(1,K)
      Y=XSTAT(2,K)
      Z=XSTAT(3,K)
C
      D2=X*X+Y*Y
      D =DSQRT(D2)
      R2=X*X+Y*Y+Z*Z
      R =DSQRT(R2)
C
C  DIFFERENTIALS OF LATITUDE WITH RESPECT TO X,Y,Z
C    (SAME UNITS AS X,Y,Z: DB HAS BEEN MULTIPLIED BY R)
      DB(1,I)=-X*Z/(D*R)
      DB(2,I)=-Y*Z/(D*R)
      DB(3,I)= D/R
C
C  DIFFERENTIALS OF LONGITUDE
C    (SAME UNITS AS X,Y,Z: DL HAS BEEN MULTIPLIED BY R*COSB=D)
      DL(1,I)=-Y/D
      DL(2,I)= X/D
      DL(3,I)= 0.D0
C
C  DIFFERENTIALS OF HEIGHT
      DH(1,I)=X/R
      DH(2,I)=Y/R
      DH(3,I)=Z/R
C
20    CONTINUE
C
C  FORM COVARIANCE MATRIX WITH RESPECT TO LAT, LONG, HEIGHT
C  (NO CROSS-CORRELATIONS B-L,B-H,L-H COMPUTED)
C  QB = DB * QXX * DB-T
C  QL = DL * QXX * DL-T
C  QH = DH * QXX * DH-T
C
      DO 30 K=1,NSTAT
C
        DO 40 I=1,K
C
C  ELEMENT NUMBER OF LINEARISED QB,QL,QH
          IKBLH=(K-1)*K/2+I
          QB(IKBLH)=0.D0
          QL(IKBLH)=0.D0
          QH(IKBLH)=0.D0
C
          DO 50 L=1,3
C
            CB=0.D0
            CL=0.D0
            CH=0.D0
C
C  INDEX OF L-TH COORDINATE OF STATION K IN QXX-MATRIX
            KQXL=LISTUS(2,K)+L-1
C
            DO 60 J=1,3
C
C  INDEX OF J-TH COORDINATE OF STATION I IN QXX-MATRIX
              IQXJ=LISTUS(2,I)+J-1
C
C  INDEX OF CORRESPONDING I-K ELEMENT IN LINEARISED QXX-MATRIX
              IKQX=IKF(IQXJ,KQXL)
C
              CB=CB+DB(J,I)*QXX(IKQX)
              CL=CL+DL(J,I)*QXX(IKQX)
              CH=CH+DH(J,I)*QXX(IKQX)
C
60          CONTINUE
C
            QB(IKBLH)=QB(IKBLH)+CB*DB(L,K)
            QL(IKBLH)=QL(IKBLH)+CL*DL(L,K)
            QH(IKBLH)=QH(IKBLH)+CH*DH(L,K)
C
50        CONTINUE
C
40      CONTINUE
C
30    CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
