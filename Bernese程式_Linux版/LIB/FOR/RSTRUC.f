      MODULE s_RSTRUC
      CONTAINS

C*
      SUBROUTINE RSTRUC(ISTRUC,NP,NN,A,B,Q,Q1,Q2,B0,B1)
CC
CC NAME       :  RSTRUC
CC
CC PURPOSE    :  ISTRUC=0 : NORMAL EQUATION SYSTEM AND RIGHT HAND SIDE
CC                          ARE COPIED TO AUXILIARY ARRAYS
CC
CC               ISTRUC=1 : REARRANGE NORMAL EQUATION SYSTEM USED
CC                          IN FIRST PART OF PROGRAM GPSEST FOR USE
CC                          IN SECOND PART
CC
CC                          PART 1 :  A*X = B
CC
CC                          PART 2 :   |  Q  ,  Q1  |          | BO |
CC                                     |   T        | * X|  =  |    |
CC                                     | Q1  ,  Q2  |          | B1 |
CC
CC PARAMETERS :
CC         IN :  ISTRUC : FLAG FOR RESTRUCTURING/COPY OF      I*4
CC                        NORMAL EQUATION SYSTEM (SEE ABOVE)
CC               NP     : DIMENSION OF OLD SYSTEM             I*4
CC               NN     : DIMENSION OF MATRIX Q               I*4
CC                        (NP-NN  DIMENSION OF MATRIX Q2)
CC               A      : OLD NORMAL EQUATION MATRIX (UPPER   R*8
CC                        TRIANGLE, COLUMNWISE LINEARISED
CC               B      : OLD RIGHT HAND SIDE                 R*8
CC        OUT :  Q,Q1,Q2,B0,B1 : MATRICES AS EXPLAINED ABOVE, R*8
CC                        BUT Q AND Q2 STAND FOR THE UPPER
CC                        TRIANGLE, COLUMNWISE LINEARISED.
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/03 10:44
CC
CC CHANGES    :  31-JUL-92 : ??: NO REARRANGEMENT FOR ISTR=2 ("FARA")
CC               15-JUL-93 : ??: COPY ALSO IF NORMAL EQUATION FILE IS
CC                               SPECIFIED
CC               28-JUL-94 : MR: FLAG "ISTRUC" DECIDES ON STRUCTURING,
CC                               NOT THE AMBIGUITY STRATEGY "ISTR"
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
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
      INTEGER*4 I     , IK    , IK0   , ISTRUC, K     , NA    , NN    ,
     1          NP
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      REAL*8  A(*),B(*),Q(*),Q1(*),Q2(*),B0(*),B1(*)
C
C COPY NORMAL EQUATION SYSTEM
C ---------------------------
      IF (ISTRUC.EQ.0) THEN
        DO 70 I=1,NP*(NP+1)/2
          Q(I)=A(I)
70      CONTINUE
        DO 60 I=1,NP
            B0(I)=B(I)
60      CONTINUE
C
C REARRANGING NORMAL EQUATION SYSTEM
C ----------------------------------
      ELSE
        NA=NP-NN
        DO 30 I=1,NN
          B0(I)=B(I)
          DO 10 K=1,I
            IK=I*(I-1)/2+K
            Q(IK)=A(IK)
10        CONTINUE
          DO 20 K=1,NA
            IK0=(NN+K)*(NN+K-1)/2+I
            IK=I+(K-1)*NN
            Q1(IK)=A(IK0)
20        CONTINUE
30      CONTINUE
C
        DO 50 I=1,NA
          B1(I)=B(NN+I)
          DO 40 K=1,I
            IK0=(NN+I)*(NN+I-1)/2+NN+K
            IK=I*(I-1)/2+K
            Q2(IK)=A(IK0)
40        CONTINUE
50      CONTINUE
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
