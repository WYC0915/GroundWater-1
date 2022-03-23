      MODULE s_YPOL
      CONTAINS

C*
      SUBROUTINE YPOL(LMAX,Q,D,HNORM,FAK,T,YKOE,Y)
CC
CC NAME       :  YPOL
CC
CC PURPOSE    :  COMPUTATION OF THE SOLUTION OF A DIFFERENTIAL
CC               EQUATION SYSTEM (AND ITS DERIVATIVES UP TO ORDER LMAX)
CC               WHICH WAS COMPUTED WITH THE SUBROUTINES OF INTLIB
CC
CC PARAMETERS :
CC         IN :  LMAX   : MAXIMUM DERIVATIVE TO BE RETURNED   I*4
CC               Q      : DEGREE OF THE POLYNOMIAL            I*4
CC               D      : DIMENSION OF THE DIFF.EQUAT.SYSTEM  I*4
CC               HNORM  : NORMALISATION FACTOR                R*8
CC               FAK(I),I=1,..: FACTORIALS                    R*8
CC               T      : INDEPENDENT ARGUMENT FOR INTERPOL-  R*8
CC                        ATION
CC               YKOE(I),I=1,..: POLYNOMIAL COEFFICIENTS      R*8
CC        OUT :  Y(I),I=1,..: VALUE OF THE FUNCTION AND ITS   R*8
CC                        DERIVATIVES
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/10/29 10:37
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC               08-MAR-95 : GB: REVERSED ORDER FOR LOOP "DO 10 ..."
CC               22-JAN-03 : HU: PREVENT OVERFLOW IN T**Q
CC                               DO NOT MODIFY LMAX
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I00   , IFIRST, J     , L     , L00   , L1    ,
     1          LLL   , LMAX  , LMAX1 , MAXQ
C
      REAL*8    HNORM , T     , T1
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(MAXQ=40)
      REAL*8 YKOE(*),Y(*),FAK(*)
      REAL*8 TPOWER(MAXQ+1)
      INTEGER*4 Q,D
      DATA IFIRST/1/
C
      IF(Q.GT.MAXQ) THEN
        WRITE(LFNERR,1) Q,MAXQ
1       FORMAT(/,' *** SR YPOL: POLYNOMIAL DEGREE TOO LARGE',/,
     1                     14X,'POLYNOMIAL DEGREE:',I3,/,
     2                     14X,'MAX. POLY. DEGREE:',I3,/)
        CALL EXITRC(2)
      END IF
      IF(IFIRST.EQ.1)THEN
        IFIRST=0
        FAK(1)=1.D0
        DO 2 I=2,Q+1
          FAK(I)=(I-1)*FAK(I-1)
2       CONTINUE
      END IF
      LMAX1=LMAX
      IF(LMAX1.GT.Q)LMAX1=Q
      T1=T/HNORM
      LMAX1=LMAX1+1
      TPOWER(1)=1.D0
      DO 100 I=2,Q+1
        IF (ABS(TPOWER(I-1)) < TINY(T1)*ABS(T1)) T1=0D0
        TPOWER(I)=TPOWER(I-1)*T1
100   CONTINUE
      DO 20 LLL=1,LMAX1
        L=LLL-1
        L1=L+1
        L00=L*D
        DO 16 J=1,D
          Y(L00+J)=0.D0
          IF(L.EQ.Q)GO TO 15
          DO 10 I=Q,L1,-1
            I00=I*D
            Y(L00+J)=Y(L00+J)+FAK(I+1)/FAK(I+1-L)*YKOE(I00+J)*
     1               TPOWER(I-L+1)
10        CONTINUE
15        Y(L00+J)=(Y(L00+J)+FAK(L+1)*YKOE(L00+J))/HNORM**L
16      CONTINUE
20    CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
