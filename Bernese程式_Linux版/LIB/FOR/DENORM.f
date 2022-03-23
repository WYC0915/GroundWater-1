      MODULE s_DENORM
      CONTAINS

C*
      SUBROUTINE DENORM(NTERM,CPOT,SPOT)
CC
CC NAME       :  DENORM
CC
CC PURPOSE    :  DENORMALISATION OF POTENTIAL COEFFICIENTS
CC
CC PARAMETERS :  NTERM  : MAXIMUM ORDER OF POTENTIAL          I*4
CC               CPOT(I): ZONAL COEFFICIENTS    C(N,M)        R*8
CC               SPOT(I): TESSERAL COEFFICIENTS S(N,M)        R*8
CC                        I=(N*(N+1)/2-2+M
CC               THE NORMALIZED COEFFICIENTS ARE REPLACED BY
CC               THE DENORMALIZED ONES.
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/30 08:11
CC
CC MODIFIED   :  30-AUG-03 : HU: SHARED DO LABELS REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE f_fakult
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IND   , M     , M1    , N     , N1    , NTERM
C
      REAL*8    CPOT  , RNM   , SPOT
C
CCC         IMPLICIT REAL*8 (A-H,O-Z)
        DIMENSION CPOT(*),SPOT(*)
        IND=0
        DO 10 N=2,NTERM
          N1=N+1
          DO M1=1,N1
            M=M1-1
            IND=IND+1
            IF(M.NE.0) RNM=DSQRT(2.D0*FAKULT(N-M)/FAKULT(N+M)*(2*N+1))
            IF(M.EQ.0) RNM=DSQRT(2*N+1.D0)
            CPOT(IND)=CPOT(IND)*RNM
            SPOT(IND)=SPOT(IND)*RNM
          ENDDO
10      CONTINUE
        RETURN
        END SUBROUTINE

      END MODULE
