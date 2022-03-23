      MODULE s_ITPOLE
      CONTAINS

C*
      SUBROUTINE ITPOLE(Q,DIM,H,F,COE,IFLAG)
CC
CC NAME       :  ITPOLE
CC
CC PURPOSE    : INTERPOLATE DIM SERIES OF EQUIDISTANT FUNCTION VALUES
CC              OF LENGTH Q+1 BY A POLYNOMIAL DEGREE Q. RETURN POLYNOMIAL
CC              COEFFICIENTS
CC
CC PARAMETERS :
CC        IN  : Q       : ORDNUNG DER TAYLORREIHE                   I*4
CC              DIM     : DIMENSION OF SYSTEM                       I*4
CC              H       : NORMALIZATION FACTOR                      R*8
CC              T       : ARRAY OF TABULAR EPOCHS                   R*8
CC              F       : DIM-DIMENSIONAL ARRAY TO BE INTERPOLATED  R*8
CC        OUT : COE     : KOEFFIZIENTEN DER TAYLORREIHE             R*8
CC              IFLAG   : =0 : O.K.                                 I*4
CC                        =2 : Q EXCEEDS MAX. Q
CC
CC REMARKS    :
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  5.0  (MAR 2003)
CC
CC CREATED    :  03/03/03
CC
CC CHANGES    :  21-JUN-05 : MM: COMLFNUM.INC REMOVED, M_BERN ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-JUN-05 : MM: UNUSED VARIABLES REMOVED
CC               19-JUL-10 : SL: TAB CHARACTERS REMOVED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      2003     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IC    , IDIFF , IDIM  , IFIRST, IFLAG , K
C
      REAL*8    H
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
      INTEGER*4   Q,DIM,QLOC
      PARAMETER   (IDIM=500,QLOC=50)
      REAL*8      COE(DIM,*)
C
C Interne Deklarationen
C ---------------------
      REAL*8    COEMAT(20,20),F(DIM,*)
      REAL*8    FH(IDIM,QLOC+1)
      DATA IFIRST/1/
C
C Beim ersten Aufruf: Lesen der Koeffizientenmatrix
C und Test einiger Dimensionen etc.
C -------------------------------------------------
      IF(IFIRST.EQ.1)THEN
C
C DIMENSION OK ?
        IF(DIM.GT.IDIM)THEN
          WRITE(LFNERR,1)DIM,IDIM
1         FORMAT(//,' ** SR COLLON: DIMENSION',I3,' GT DIMLOC',I3,//)
          CALL EXITRC(2)
        ENDIF
C
C INTEGRATIONSORDNUNG OK ?
C ----------------------
        IF(Q.GT.15)THEN
          WRITE(LFNERR,3)Q
3         FORMAT(//,' ** SR COLLON: Q=',I3,' NOT ALLOWED(QMAX=15)',//)
          IFLAG=3
          Q=15
        ENDIF
C
        IF(Q.LT.0)THEN
          IFLAG=2
          Q=0
        END IF
C
C Definition der Koeffizienten
C ----------------------------
        IFIRST=0
        COEMAT( 2, 2)=                  1.D0 /                  1.D0
        COEMAT( 2, 3)=                 -1.D0 /                  2.D0
        COEMAT( 2, 4)=                  1.D0 /                  3.D0
        COEMAT( 2, 5)=                 -1.D0 /                  4.D0
        COEMAT( 2, 6)=                  1.D0 /                  5.D0
        COEMAT( 2, 7)=                 -1.D0 /                  6.D0
        COEMAT( 2, 8)=                  1.D0 /                  7.D0
        COEMAT( 2, 9)=                 -1.D0 /                  8.D0
        COEMAT( 2,10)=                  1.D0 /                  9.D0
        COEMAT( 2,11)=                 -1.D0 /                 10.D0
        COEMAT( 2,12)=                  1.D0 /                 11.D0
        COEMAT( 2,13)=                 -1.D0 /                 12.D0
        COEMAT( 2,14)=                  1.D0 /                 13.D0
        COEMAT( 2,15)=                 -1.D0 /                 14.D0
        COEMAT( 3, 3)=                  1.D0 /                  2.D0
        COEMAT( 3, 4)=                 -1.D0 /                  2.D0
        COEMAT( 3, 5)=                 11.D0 /                 24.D0
        COEMAT( 3, 6)=                 -5.D0 /                 12.D0
        COEMAT( 3, 7)=                137.D0 /                360.D0
        COEMAT( 3, 8)=                 -7.D0 /                 20.D0
        COEMAT( 3, 9)=                363.D0 /               1120.D0
        COEMAT( 3,10)=               -761.D0 /               2520.D0
        COEMAT( 3,11)=               7129.D0 /              25200.D0
        COEMAT( 3,12)=               -671.D0 /               2520.D0
        COEMAT( 3,13)=              83711.D0 /             332640.D0
        COEMAT( 3,14)=              -6617.D0 /              27720.D0
        COEMAT( 3,15)=            1145993.D0 /            5045040.D0
        COEMAT( 4, 4)=                  1.D0 /                  6.D0
        COEMAT( 4, 5)=                 -1.D0 /                  4.D0
        COEMAT( 4, 6)=                  7.D0 /                 24.D0
        COEMAT( 4, 7)=                 -5.D0 /                 16.D0
        COEMAT( 4, 8)=                 29.D0 /                 90.D0
        COEMAT( 4, 9)=               -469.D0 /               1440.D0
        COEMAT( 4,10)=              29531.D0 /              90720.D0
        COEMAT( 4,11)=              -1303.D0 /               4032.D0
        COEMAT( 4,12)=              16103.D0 /              50400.D0
        COEMAT( 4,13)=            -190553.D0 /             604800.D0
        COEMAT( 4,14)=             128977.D0 /             415800.D0
        COEMAT( 4,15)=              -9061.D0 /              29700.D0
        COEMAT( 5, 5)=                  1.D0 /                 24.D0
        COEMAT( 5, 6)=                 -1.D0 /                 12.D0
        COEMAT( 5, 7)=                 17.D0 /                144.D0
        COEMAT( 5, 8)=                 -7.D0 /                 48.D0
        COEMAT( 5, 9)=                967.D0 /               5760.D0
        COEMAT( 5,10)=                -89.D0 /                480.D0
        COEMAT( 5,11)=               4523.D0 /              22680.D0
        COEMAT( 5,12)=              -7645.D0 /              36288.D0
        COEMAT( 5,13)=             341747.D0 /            1555200.D0
        COEMAT( 5,14)=            -412009.D0 /            1814400.D0
        COEMAT( 5,15)=            9301169.D0 /           39916800.D0
        COEMAT( 6, 6)=                  1.D0 /                120.D0
        COEMAT( 6, 7)=                 -1.D0 /                 48.D0
        COEMAT( 6, 8)=                  5.D0 /                144.D0
        COEMAT( 6, 9)=                 -7.D0 /                144.D0
        COEMAT( 6,10)=               1069.D0 /              17280.D0
        COEMAT( 6,11)=                -19.D0 /                256.D0
        COEMAT( 6,12)=              31063.D0 /             362880.D0
        COEMAT( 6,13)=            -139381.D0 /            1451520.D0
        COEMAT( 6,14)=            1148963.D0 /           10886400.D0
        COEMAT( 6,15)=            -355277.D0 /            3110400.D0
        COEMAT( 7, 7)=                  1.D0 /                720.D0
        COEMAT( 7, 8)=                 -1.D0 /                240.D0
        COEMAT( 7, 9)=                 23.D0 /               2880.D0
        COEMAT( 7,10)=                 -1.D0 /                 80.D0
        COEMAT( 7,11)=               3013.D0 /             172800.D0
        COEMAT( 7,12)=               -781.D0 /              34560.D0
        COEMAT( 7,13)=             242537.D0 /            8709120.D0
        COEMAT( 7,14)=              -9607.D0 /             290304.D0
        COEMAT( 7,15)=            1666393.D0 /           43545600.D0
        COEMAT( 8, 8)=                  1.D0 /               5040.D0
        COEMAT( 8, 9)=                 -1.D0 /               1440.D0
        COEMAT( 8,10)=                 13.D0 /               8640.D0
        COEMAT( 8,11)=                 -1.D0 /                384.D0
        COEMAT( 8,12)=                683.D0 /             172800.D0
        COEMAT( 8,13)=              -1903.D0 /             345600.D0
        COEMAT( 8,14)=             314617.D0 /           43545600.D0
        COEMAT( 8,15)=            -112879.D0 /           12441600.D0
        COEMAT( 9, 9)=                  1.D0 /              40320.D0
        COEMAT( 9,10)=                 -1.D0 /              10080.D0
        COEMAT( 9,11)=                 29.D0 /             120960.D0
        COEMAT( 9,12)=                -11.D0 /              24192.D0
        COEMAT( 9,13)=              10831.D0 /           14515200.D0
        COEMAT( 9,14)=               -299.D0 /             268800.D0
        COEMAT( 9,15)=             944311.D0 /          609638400.D0
        COEMAT(10,10)=                  1.D0 /             362880.D0
        COEMAT(10,11)=                 -1.D0 /              80640.D0
        COEMAT(10,12)=                  1.D0 /              30240.D0
        COEMAT(10,13)=                -11.D0 /             161280.D0
        COEMAT(10,14)=               1747.D0 /           14515200.D0
        COEMAT(10,15)=               -793.D0 /            4147200.D0
        COEMAT(11,11)=                  1.D0 /            3628800.D0
        COEMAT(11,12)=                 -1.D0 /             725760.D0
        COEMAT(11,13)=                  1.D0 /             248832.D0
        COEMAT(11,14)=                -13.D0 /            1451520.D0
        COEMAT(11,15)=                491.D0 /           29030400.D0
        COEMAT(12,12)=                  1.D0 /           39916800.D0
        COEMAT(12,13)=                 -1.D0 /            7257600.D0
        COEMAT(12,14)=                 19.D0 /           43545600.D0
        COEMAT(12,15)=                -13.D0 /           12441600.D0
        COEMAT(13,13)=                  1.D0 /          479001600.D0
        COEMAT(13,14)=                 -1.D0 /           79833600.D0
        COEMAT(13,15)=                 41.D0 /          958003200.D0
        COEMAT(14,14)=                  1.D0 /         6227020800.D0
        COEMAT(14,15)=                 -1.D0 /          958003200.D0
        COEMAT(15,15)=                  1.D0 /        87178291200.D0
      END IF
C
C First Term
C ----------
      DO K=1,DIM
        COE(K,1)=F(K,1)
      ENDDO
C
C Auxiliary Array
C ---------------
      DO I=1,Q+1
        DO K=1,DIM
          FH(K,I)=F(K,I)
        ENDDO
      ENDDO
C
C Differenzenbildung
C ------------------
        DO 150 IDIFF=1,Q
          DO 140 I=Q+1,IDIFF+1,-1
            DO 130 K=1,DIM
              FH(K,I)=FH(K,I)-FH(K,I-1)
130         CONTINUE
140       CONTINUE
150     CONTINUE
C
C Berechnung der Koeffizienten CI :
C -------------------------------
        DO 180 IC=2,Q+1
          DO 170 K=1,DIM
            COE(K,IC)=0.D0
            DO 160 I=Q+1,IC,-1
              COE(K,IC)=COE(K,IC)+COEMAT(IC,I)*FH(K,I)
160         CONTINUE
C
            COE(K,IC)=COE(K,IC)/(H/Q)**(IC-1)
170       CONTINUE
180     CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
