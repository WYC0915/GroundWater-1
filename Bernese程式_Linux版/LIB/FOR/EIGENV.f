      MODULE s_EIGENV
      CONTAINS

C*
      SUBROUTINE EIGENV(NP,N,D,E,Z)
CC
CC NAME       :  EIGENV
CC
CC PURPOSE    :  QL ALGORITHM WITH IMPLICIT SHIFT, TO DETERMINE THE
CC               EIGENVALUES AND EIGENVECTORS OF A REAL, SYMMETRIC,
CC               TRIDIAGONAL MATRIX, OR A REAL, SYMMETRIC MATRIX
CC               PREVIOUSLY REDUCED BY SUBROUTINE "TRIDIA".
CC
CC               REFERENCE: WILLIAM H. PRESS ET AL. "NUMERICAL RECIPES"
CC                          CAMBRIDGE UNIVERSITY PPESS, 1986.
CC
CC PARAMETERS :
CC         IN :  NP     : MAXIMAL DIMENSION OF MATRIX D,E,Z   I*4
CC               N      : ACTUAL DIMENSION OF MATRIX D,E,Z    I*4
CC     IN/OUT :  D      : INPUT: VECTOR OF N ELEMENTS         R*8
CC                          CONTAINING THE DIAGONAL ELEMENTS
CC                          OF THE TRIDIAGONAL MATRIX
CC                        OUTPUT: VECTOR CONTAINING THE N
CC                          EIGENVALUES
CC               E      : INPUT: VECTOR CONTAINING THE        R*8
CC                          OFF-DIAGONAL ELEMENTS OF THE
CC                          TRIDIAGONAL MATRIX WITH E(1)
CC                          ARBITRARY
CC                        OUTPUT: E IS DESTROYED.
CC               Z      : N*N-MATRIX
CC                        INPUT: IF THE EIGENVECTORS OF A     R*8
CC                          TRIDIAGONAL MATRIX ARE DESIRED,
CC                          THE MATRIX Z IS INPUT AS THE
CC                          IDENTITY MATRIX. IF THE EIGEN-
CC                          VECTORS OF A MATRIX THAT HAS BEEN
CC                          REDUCED BY "TRIDIA" ARE REQUIRED,
CC                          THEN Z IS INPUT AS THE MATRIX
CC                          OUTPUT BY "TRIDIA".
CC                        OUTPUT: IN EITHER CASE, THE K-TH
CC                          COLUMN OF Z RETURNS THE NORMALIZED
CC                          EIGENVECTOR CORRESPONDING TO D(K).
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  W.H.PRESS ET AL., M.ROTHACHER
CC
CC VERSION    :  3.2
CC
CC CREATED    :  90/07/12 14:18
CC
CC CHANGES    :  29-OCT-93 : SF: REMOVE LINE NUMBERS ON COL 72 TO 80
CC               10-AUG-94 : MR: CALL EXITRC
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1990     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I   , ITER, K   , L   , M   , N   , NP
C
      REAL*8    B   , C   , DD  , F   , G   , P   , R   , S
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      REAL*8 D(NP),E(NP),Z(NP,NP)
C
      IF (N.GT.1) THEN
        DO 11 I=2,N
          E(I-1)=E(I)
11      CONTINUE
        E(N)=0.D0
        DO 15 L=1,N
          ITER=0
1         DO 12 M=L,N-1
            DD=DABS(D(M))+DABS(D(M+1))
            IF (DABS(E(M))+DD.EQ.DD) GOTO 2
12        CONTINUE
          M=N
2         IF (M.NE.L) THEN
            IF (ITER.EQ.30) THEN
              WRITE(*,901) ITER
901           FORMAT(/,' *** SR EIGENV: TOO MANY ITERATIONS',/,
     1                             16X,'# ITERATIONS >=',I3,/)
              CALL EXITRC(2)
            ENDIF
            ITER=ITER+1
            G=(D(L+1)-D(L))/(2.D0*E(L))
            R=DSQRT(G**2+1.D0)
            G=D(M)-D(L)+E(L)/(G+DSIGN(R,G))
            S=1.D0
            C=1.D0
            P=0.D0
            DO 14 I=M-1,L,-1
              F=S*E(I)
              B=C*E(I)
              IF(DABS(F).GE.DABS(G))THEN
                C=G/F
                R=DSQRT(C**2+1.D0)
                E(I+1)=F*R
                S=1.D0/R
                C=C*S
              ELSE
                S=F/G
                R=DSQRT(S**2+1.D0)
                E(I+1)=G*R
                C=1.D0/R
                S=S*C
              ENDIF
              G=D(I+1)-P
              R=(D(I)-G)*S+2.D0*C*B
              P=S*R
              D(I+1)=G+P
              G=C*R-B
              DO 13 K=1,N
                F=Z(K,I+1)
                Z(K,I+1)=S*Z(K,I)+C*F
                Z(K,I)=C*Z(K,I)-S*F
13            CONTINUE
14          CONTINUE
            D(L)=D(L)-P
            E(L)=G
            E(M)=0.D0
            GOTO 1
          ENDIF
15      CONTINUE
      ENDIF
      RETURN
      END SUBROUTINE


      END MODULE
