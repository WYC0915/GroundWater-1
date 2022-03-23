      MODULE s_DMINV
      CONTAINS

C*
      SUBROUTINE DMINV(A,N,D,L,M)
CC
CC NAME       :  DMINV
CC
CC PURPOSE    :  INVERSION OF A MATRIX
CC               DIESE SR WURDE DEM IBM-SSP ENTNOMMEN.(P.118)
CC
CC PARAMETERS :
CC         IN :  A      : MATRIX TO BE INVERTED (LINEARIZED)  R*8(N*N)
CC               N      : DIMENSION OF A (# OF ROWS OR COL)   I*4
CC        OUT :  D      : DETERMINANT                         R*8
CC               L,M    : TWO AUXILIARY ARRAYS                I*4(N)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/10/30 17:37
CC
CC CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I , IJ, IK, IZ, J , JI, JK, JP, JQ, JR, K , KI, KJ, KK,
     1          L , M , N , NK
C
      DIMENSION A(*),L(*),M(*)
      DOUBLE PRECISION A,D,BIGA,HOLD
C
      D=1
      NK=-N
      DO 80 K=1,N
        NK=NK+N
        L(K)=K
        M(K)=K
        KK=NK+K
        BIGA=A(KK)
        DO 20 J=K,N
          IZ=N*(J-1)
          DO 20 I=K,N
            IJ=IZ+I
10          IF(DABS(BIGA)-DABS(A(IJ)))15,20,20
15          BIGA=A(IJ)
            L(K)=I
            M(K)=J
20      CONTINUE
        J=L(K)
        IF(J-K)35,35,25
25      KI=K-N
        DO 30 I=1,N
          KI=KI+N
          HOLD=-A(KI)
          JI=KI-K+J
          A(KI)=A(JI)
          A(JI)=HOLD
30      CONTINUE
35      I=M(K)
        IF(I-K)45,45,38
38      JP=N*(I-1)
        DO 40 J=1,N
          JK=NK+J
          JI=JP+J
          HOLD=-A(JK)
          A(JK)=A(JI)
          A(JI)=HOLD
40      CONTINUE
45      IF(BIGA)48,46,48
46      D=0
        RETURN
48      DO 55 I=1,N
          IF(I-K)50,55,50
50        IK=NK+I
          A(IK)=A(IK)/(-BIGA)
55      CONTINUE
        DO 65 I=1,N
          IK=NK+I
          HOLD=A(IK)
          IJ=I-N
          DO 65 J=1,N
            IJ=IJ+N
            IF(I-K)60,65,60
60          IF(J-K)62,65,62
62          KJ=IJ-I+K
            A(IJ)=HOLD*A(KJ)+A(IJ)
65      CONTINUE
        KJ=K-N
        DO 75 J=1,N
          KJ=KJ+N
          IF(J-K)70,75,70
70        A(KJ)=A(KJ)/BIGA
75      CONTINUE
C       D=D*BIGA
        A(KK)=1/BIGA
80    CONTINUE
      K=N
100   K=K-1
      IF(K)150,150,105
105   I=L(K)
      IF(I-K)120,120,108
108   JQ=N*(K-1)
      JR=N*(I-1)
      DO 110 J=1,N
        JK=JQ+J
        HOLD=A(JK)
        JI=JR+J
        A(JK)=-A(JI)
        A(JI)=HOLD
110   CONTINUE
120   J=M(K)
      IF(J-K)100,100,125
125   KI=K-N
      DO 130 I=1,N
        KI=KI+N
        HOLD=A(KI)
        JI=KI-K+J
        A(KI)=-A(JI)
        A(JI)=HOLD
130   CONTINUE
      GO TO 100
150   RETURN
      END SUBROUTINE

      END MODULE
