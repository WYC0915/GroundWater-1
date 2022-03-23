      MODULE s_YPOLRP
      CONTAINS

C*
      SUBROUTINE YPOLRP(Q,D,TIME,YCOE,RES)
CC
CC NAME       :  YPOLRP
CC
CC PURPOSE    :  RES = RES+ (TIME(I)*YCOE(K,I),K=1,2,..,D) ;
CC                          K=1,2,..,Q+1
CC
CC PARAMETERS :
CC         IN :  Q      : DIMENSION OF TIME - 1               I*4
CC               D      : NUMBER OF LINES OF MATRICES YCOE    I*4
CC                        AND RES
CC               TIME(I),I=1,2,..,Q+1: ARRAY TIME             R*8
CC               YCOE(K),K=1,2,..,(Q+1)*D INPUT ARRAY         R*8
CC        OUT :  RES(K),K=1,2,..,(Q+1)*D  OUTPUT ARRAY        R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/12/31 13:10
CC
CC CHANGES    :  08-MAR-95 : GB: REVERSED ORDER FOR LOOP "DO 100 ..."
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
      INTEGER*4 I , L , LI
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
      INTEGER*4 Q,D
      REAL*8 TIME(*),YCOE(*),RES(*)
C
C MATRIX MULTIPLICATION
      DO 100 I=1,D
        RES(I)=0.D0
        DO 100 L=Q+1,1,-1
          LI=(L-1)*D+I
          RES(I)=RES(I) + TIME(L)*YCOE(LI)
100   CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
