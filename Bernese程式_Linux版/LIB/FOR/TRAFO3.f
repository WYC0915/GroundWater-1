      MODULE s_TRAFO3
      CONTAINS

C*
      SUBROUTINE TRAFO3(NPAR,INDCOE,COETRA,DELTA,
     1                  ANOR,BNOR,RMS,XMAT,BAUX)
CC
CC NAME       :  TRAFO3
CC
CC PURPOSE    : GIVEN AN NEQ SYSTEM OF TYPE
CC
CC                N * X = B
CC
CC              PERFORM A VARIABLE TRANSFORMATION
CC
CC                X = C * Y + DELTA
CC
CC              AND RETURN THE NEW NEQ SYSTEM
CC
CC                M * Y = B'
CC
CC              WHERE :
CC                     T               T        T
CC                M = C * N * C, B' = C  * B - C * N * DELTA
CC
CC                                      T           T
CC                RMS' = RMS - 2 * DELTA * B + DELTA * N * DELTA
CC
CC
CC PARAMETERS :
CC        IN  : NPAR    : NUMBER OF PARAMETERS                      I*4
CC              INDCOE  : INDEX ARRAY FOR MATRIX COETRA             I*4(*,*)
CC              COETRA  : MATRIX OF COEFFICIENTS                    R*8(*)
CC              DELTA   : OFFSET                                    R*8(*)
CC     IN/OUT : ANOR    : RESULTING NEQ MATRIX                      R*8(*)
CC              BNOR    : RESULTING RHS OF NEQS                     R*8(*)
CC              RMS     : RESULTING SUM OF RES. SQUARES             R*8
CC              XMAT    : AUX. MATRIX                               R*8(*)
CC              BAUX    : AUX. MATRIX                               R*8(*)
CC
CC
CC REMARKS    :
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.5  (MAY 94)
CC
CC CREATED    :  94/05/11
CC
CC CHANGES    :  01-MAY-95 : GB: TURBO VERSION
CC               03-MAY-95 : MR: MAXPAR=1300 (OLD:1000)
CC               25-JUL-95 : TS: INCREASED MAXPAR FROM 1300 --> 1600
CC               20-FEB-96 : TS: INCREASED MAXPAR FROM 1600 --> 1700
CC               18-MAR-96 : EB: INCREASED MAXPAR/MAXLOC FROM 1700 --> 1800
CC               12-AUG-97 : GB: REDUCE ANEW TO UPPER TRIANGULAR MATRIX
CC                               "ABUSE" ARRAY BAUX FOR COMPUTATION OF
CC                               TRN(C)*N*C
CC               16-AUG-99 : RD: DIMENSIONS (SMALL/MEDIUM/LARGE) USING I:ADDNEQ
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               15-MAR-03 : HU: REMOVE UNUSED MAXxxx
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               16-JUL-08 : RD: USE P_ADDOLD->P_ADDNEQ
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1994     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE p_addneq, ONLY: MAXLIN

      USE f_ikf
      USE s_trnhlp
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I    , IK   , IL   , INDEX, IP   , IPAR , KPAR ,
     1          L    , LPAR , NPAR
C
      REAL*8    DRMS , RMS
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
C MAXLIN : MAXIMUM NUMBER OF NON-ZERO ELEMENTS PER COL OF MATRIX COETRA
C MAXPAR : MAXIMUM NUMBER OF PARAMETERS
C
      INTEGER*4   INDCOE(2,*)
      REAL*8      ANOR(*),BNOR(*),COETRA(*),DELTA(*)
      REAL*8      XMAT(*),BAUX(*)
      INTEGER*4    NLPCOL(NPAR),IDXCOE(2,MAXLIN,NPAR)
C
C DEFINE AUX. ARRAYS NLPCOL, IDXCOE
C ---------------------------------
      CALL TRNHLP(NPAR,NPAR,MAXLIN,NPAR,INDCOE,NLPCOL,IDXCOE)
C
C COPY MATRIX ANOR INTO XMAT
C --------------------------
      DO 10 IK=1,NPAR*(NPAR+1)/2
        XMAT(IK)=ANOR(IK)
10    CONTINUE
C
C TRANSFORM ANOR USING AUXILIARY ARRAY BAUX
C -----------------------------------------
C
C LOOP OVER ALL COLUMNS OF RESULTING MATRIX
      DO 200 KPAR=1,NPAR
C
C AUXILIARY ARRAY BAUX = XMAT * C
        DO 120 IP=1,NPAR
          BAUX(IP)=0.D0
          DO 110 L=1,NLPCOL(KPAR)
            LPAR =IDXCOE(1,L,KPAR)
            INDEX=IDXCOE(2,L,KPAR)
            IL=IKF(IP,LPAR)
            BAUX(IP)=BAUX(IP)+XMAT(IL)*COETRA(INDEX)
110       CONTINUE
120     CONTINUE
C
C ANEW = TRN(C) * BAUX
C
C LOOP OVER ALL LINES OF NEW MATRIX
        DO 190 IPAR=1,KPAR
          IK=IKF(IPAR,KPAR)
          ANOR(IK)=0.D0
          DO 180 L=1,NLPCOL(IPAR)
            LPAR =IDXCOE(1,L,IPAR)
            INDEX=IDXCOE(2,L,IPAR)
            ANOR(IK)=ANOR(IK)+COETRA(INDEX)*BAUX(LPAR)
180       CONTINUE
190     CONTINUE
200   CONTINUE
C
C 1. MODIFY THE RMS OF THE NEQ SYSTEM OF ARC I
C    -----------------------------------------
      DO 40 I=1,NPAR
        BAUX(I)=0.D0
        DO 30 L=1,NPAR
          IL=IKF(I,L)
          BAUX(I)=BAUX(I)+XMAT(IL)*DELTA(L)
30      CONTINUE
40    CONTINUE
C
C 3. MODIFICATION OF SUM OF RES SQUARES
C    ----------------------------------
      DRMS=0.D0
      DO 50 I=1,NPAR
        DRMS=DRMS+DELTA(I)*(-2*BNOR(I)+BAUX(I))
50    CONTINUE
      RMS=RMS+DRMS
C
C 4. RIGHT HAND SIDE OF NEQ SYSTEM
C    -----------------------------
      DO 55 I=1,NPAR
        BAUX(I)=BNOR(I)-BAUX(I)
55    CONTINUE
C
      DO 70 IPAR=1,NPAR
        BNOR(IPAR)=0.D0
        DO 60 L=1,NLPCOL(IPAR)
          LPAR =IDXCOE(1,L,IPAR)
          INDEX=IDXCOE(2,L,IPAR)
          BNOR(IPAR)=BNOR(IPAR)+COETRA(INDEX)*BAUX(LPAR)
60      CONTINUE
70    CONTINUE
C
999   CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
