      MODULE s_POLYAP
      CONTAINS

C*
      SUBROUTINE POLYAP(X,Y,L,N,IP,C,CRMS,V,RMS,IFOK)
CC
CC NAME       :  POLYAP
CC
CC PURPOSE    :  APPROXIMATION OF A SET OF DISCRETE FUNCTIONAL VALUES
CC               BY A POLYNOMIAL OF DEGREE IP-1 (IP<15)
CC               DETERMINING THE INTEGRAL OF THE POLYNOMIAL
CC               USING DIFFERENCES OF CONSECUTIVE VALUES
CC
CC PARAMETERS :
CC         IN :  X      : VECTOR WITH INDEPENDENT ARGUMENTS   R*8(N)
CC               Y      : VECTOR WITH FUNCTIONAL VALUES       R*8(N)
CC               L      : BIT 2=1: VALUE # I IS NOT USED      CH*1(N)
CC                                 FOR APPROXIMATION
CC                        BIT 0=1: VALUE # I IS NOT USED AT ALL
CC               N      : # OF VALUES (=DIMENSION OF X,Y,V)   I*4
CC               IP     : # OF POLY. COEFFICIENTS             I*4
CC        OUT :  C      : VECTOR WITH COEFFIC. OF POLYNOM.    R*8(IP)
CC               CRMS   : VECTOR WITH RMS OF COEFF. OF POLY.  R*8(IP)
CC               V      : VECTOR WITH RESIDUALS               R*8(N)
CC                        (DIFF APPROX-Y)
CC               IFOK   : INDEX OF REFERENCE X-VALUE FOR      I*4
CC                        POLYNOMIAL
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  07-JUL-92
CC
CC CHANGES    :  12-FEB-93 : ??: TEST "...TSTFLG(L(J),2)) RMS=..." WAS WRONG
CC               01-JUN-93 : ??: POLYNOM DEGREE WRONG IN ERROR MESSAGE
CC               10-AUG-94 : MR: CALL EXITRC
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               10-JUL-12 : RD: USE SYMINVG INSTEAD OF SYMIN8
CC               10-JUL-12 : RD: USE M_BERN WITH ONLY
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1992     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,  ONLY: lfnerr
      USE f_ikf
      USE f_tstflg
      USE s_syminvg
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I    , IFOK , IK   , IP   , ISING, J    , K    ,
     1          MAXIP, N    , NEFF
C
      REAL*8    RMS  , XHLP , XMAX , XREDJ, YS
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (MAXIP=10)
C
      REAL*8      X(*),Y(*),C(*),CRMS(*),V(*)
      REAL*8      Q(MAXIP*(MAXIP+1)/2),F(MAXIP),A(MAXIP)
      CHARACTER*1 L(*)
C
      IF(IP.GT.MAXIP) THEN
        WRITE(LFNERR,1) IP,MAXIP
1       FORMAT(/,' *** SR POLYAP: TOO MANY POLY.COEFFICIENTS',/,
     1                       16X,'NUMBER OF COEFFICIENTS:',I3,/,
     2                       16X,'MAX. NUMBER ALLOWED   :',I3,/)
        CALL EXITRC(2)
      END IF
C
C  NORMALISATION OF INDEPENDENT ARGUMENT
C  -------------------------------------
      XMAX=-1.D37
      IFOK=0
      DO 10 I=1,N
        IF(TSTFLG(L(I),0).OR.TSTFLG(L(I),2)) GOTO 10
        IF (IFOK.EQ.0) IFOK=I
        IF(DABS(X(I)-X(IFOK)).GT.XMAX) XMAX=DABS(X(I)-X(IFOK))
10    CONTINUE
      IF (XMAX.EQ.0.D0) XMAX=1.D0
C
      DO 20 I=1,IP*IP
        Q(I)=0.D0
20    CONTINUE
      DO 30 I=1,IP
        F(I)=0.D0
30    CONTINUE
C
C  LOOP OVER N
C
      NEFF=0
      DO 70 J=1,N
C
C  MARKED VALUES
        IF(TSTFLG(L(J),0).OR.TSTFLG(L(J),2)) GOTO 70
        XREDJ=(X(J)-X(IFOK))/XMAX
        NEFF=NEFF+1
C
C  ROW # J OF A-MATRIX
        A(1)=1.D0
        DO 40 K=2,IP
          A(K)=A(K-1)*XREDJ
40      CONTINUE
C
C  CONTRIBUTION TO Q-MATRIX (UPPER TRIANGLE)
        DO 55 I=1,IP
          DO 50 K=I,IP
            IK=IKF(I,K)
            Q(IK)=Q(IK)+A(I)*A(K)
50        CONTINUE
55      CONTINUE
C
C  CONTRIBUTION TO RIGHT HAND SIDE
        DO 60 I=1,IP
          F(I)=F(I)+A(I)*Y(J)
60      CONTINUE
C
70    CONTINUE
C
      IF(NEFF.LT.IP) THEN
        WRITE(LFNERR,2) NEFF,IP-1
2       FORMAT(/,' *** SR POLYAP: NOT ENOUGH OBSERVATIONS',/,
     1                       16X,'NUMBER OF OBSERVATIONS:',I5,/,
     2                       16X,'POLYNOMIAL DEGREE     :',I5,/)
        CALL EXITRC(2)
      END IF
C
C  INVERSION OF Q MATRIX
      CALL SYMINVG(IP,Q,0,ISING)
      IF (ISING.NE.0) THEN
        WRITE(LFNERR,3)
3       FORMAT(/,' *** SR POLYAP: MATRIX SINGULAR',/)
        CALL EXITRC(2)
      ENDIF
C
C  SOLUTION VECTOR (POLYNOMIAL COEFFICIENTS)
      DO 90 I=1,IP
        C(I)=0.D0
        DO 90 K=1,IP
          IK=IKF(I,K)
          C(I)=C(I)+Q(IK)*F(K)
90    CONTINUE
C
C RESIDUALS, RMS
C ---------------
      RMS=0.D0
      DO 110 J=1,N
        IF(TSTFLG(L(J),0)) GOTO 110
        XREDJ=(X(J)-X(IFOK))/XMAX
        YS=0.D0
        XHLP=1.D0
        DO 100 K=1,IP
          YS=YS+C(K)*XHLP
          XHLP=XHLP*XREDJ
100     CONTINUE
        V(J)=YS-Y(J)
C        WRITE(LFNPRT,*) XREDJ,YS,Y(J),V(J)
        IF(.NOT.TSTFLG(L(J),2)) RMS=RMS+V(J)*V(J)
110   CONTINUE
C
C RMS AND COEFFICIENTS
C --------------------
      IF(NEFF-IP.NE.0.AND.RMS.GT.0.D0) THEN
         RMS=DSQRT(RMS/(NEFF-IP))
      ELSE
         RMS=0.D0
      END IF
C
      DO 120 K=1,IP
        CRMS(K)=RMS*DSQRT(Q(IKF(K,K)))
120   CONTINUE
C
      XHLP=1.D0
      DO 130 K=1,IP
        C(K)=C(K)/XHLP
        CRMS(K)=CRMS(K)/XHLP
        XHLP=XHLP*XMAX
130   CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
