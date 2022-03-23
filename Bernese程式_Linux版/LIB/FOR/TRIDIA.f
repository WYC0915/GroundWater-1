      MODULE s_TRIDIA
      CONTAINS

C*
      SUBROUTINE TRIDIA(NP,N,A,D,E)
CC
CC NAME       :  TRIDIA
CC
CC PURPOSE    :  HOUSEHOLDER REDUCTION OF A REAL, SYMMETRIC MATRIX
CC               THE RESULT IS A SO CALLED TRIDIAGONAL MATRIX: THE ONLY
CC               NONZERO ELEMENTS ARE THE DIAGONAL ELEMENTS AND THE
CC               ELEMENTS JUST ABOVE AND BELOW THE DIAGONAL.
CC               THIS ROUTINE IS USED AS A PRESTEP TO THE ROUTINE
CC               "EIGENV" THAT RETURNS THE EIGENVALUES AND EIGENVECTORS.
CC
CC               REFERENCE: WILLIAM H. PRESS ET AL. "NUMERICAL RECIPES",
CC                          CAMBRIDGE UNIVERSITY PPESS, 1986.
CC
CC PARAMETERS :
CC         IN :  NP     : MAXIMAL DIMENSION OF MATRIX A,D,E   I*4
CC               N      : ACTUAL DIMENSION OF MATRIX A,D,E    I*4
CC     IN/OUT :  A      : INPUT: REAL, SYMMETRIC, N*N-MATRIX  R*8
CC                          TO BE REDUCED TO A TRIDIAGONAL
CC                          MATRIX
CC                        OUTPUT: ORTHOGONAL MATRIX "Q"
CC                          EFFECTING THE TRANSFORMATION
CC        OUT :  D      : VECTOR OF N ELEMENTS CONTAINING THE R*8
CC                        DIAGONAL ELEMENTS OF THE TRIDIAGONAL
CC                        MATRIX
CC               E      : VECTOR CONTAINING THE OFF-DIAGONAL  R*8
CC                        ELEMENTS OF THE TRIDIAGONAL MATRIX
CC                        WITH E(1)=0.D0.
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  W.H.PRESS ET AL., M.ROTHACHER
CC
CC VERSION    :  3.2
CC
CC CREATED    :  90/07/12 14:18
CC
CC CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1990     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I    , J    , K    , L    , N    , NP
C
      REAL*8    F    , G    , H    , HH   , SCALE
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      REAL*8 A(NP,NP),D(NP),E(NP)
C
      IF (N.GT.1) THEN
        DO 18 I=N,2,-1
          L=I-1
          H=0.D0
          SCALE=0.D0
          IF (L.GT.1) THEN
            DO 11 K=1,L
              SCALE=SCALE+DABS(A(I,K))
11          CONTINUE
            IF (SCALE.EQ.0.D0) THEN
              E(I)=A(I,L)
            ELSE
              DO 12 K=1,L
                A(I,K)=A(I,K)/SCALE
                H=H+A(I,K)**2
12            CONTINUE
              F=A(I,L)
              G=-DSIGN(DSQRT(H),F)
              E(I)=SCALE*G
              H=H-F*G
              A(I,L)=F-G
              F=0.D0
              DO 15 J=1,L
                A(J,I)=A(I,J)/H
                G=0.D0
                DO 13 K=1,J
                  G=G+A(J,K)*A(I,K)
13              CONTINUE
CC                IF (L.GT.J) THEN
                  DO 14 K=J+1,L
                    G=G+A(K,J)*A(I,K)
14                CONTINUE
CC                ENDIF
                E(J)=G/H
                F=F+E(J)*A(I,J)
15            CONTINUE
              HH=F/(H+H)
              DO 17 J=1,L
                F=A(I,J)
                G=E(J)-HH*F
                E(J)=G
                DO 16 K=1,J
                  A(J,K)=A(J,K)-F*E(K)-G*A(I,K)
16              CONTINUE
17            CONTINUE
            ENDIF
          ELSE
            E(I)=A(I,L)
          ENDIF
          D(I)=H
18      CONTINUE
      ENDIF
      D(1)=0.D0
      E(1)=0.D0
      DO 23 I=1,N
        L=I-1
        IF (D(I).NE.0.D0) THEN
          DO 21 J=1,L
            G=0.D0
            DO 19 K=1,L
              G=G+A(I,K)*A(K,J)
19          CONTINUE
            DO 20 K=1,L
              A(K,J)=A(K,J)-G*A(K,I)
20          CONTINUE
21        CONTINUE
        ENDIF
        D(I)=A(I,I)
        A(I,I)=1.D0
CC        IF (L.GE.1) THEN
          DO 22 J=1,L
            A(I,J)=0.D0
            A(J,I)=0.D0
22        CONTINUE
CC        ENDIF
23    CONTINUE
      RETURN
      END SUBROUTINE


      END MODULE
