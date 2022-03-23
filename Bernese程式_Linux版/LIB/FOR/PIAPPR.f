      MODULE s_PIAPPR
      CONTAINS

C*
      SUBROUTINE PIAPPR(X,Y,L,N,IP,C,V,RMS,DX,FT)
CC
CC NAME       :  PIAPPR
CC
CC PURPOSE    :  APPROXIMATION OF A SET OF DISCRETE FUNCTIONAL VALUES
CC               BY A POLYNOMIAL OF DEGREE IP (IP<15)
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
CC               IP     : DEGREE OF POLYNOMIAL                I*4
CC        OUT :  C      : VECTOR WITH COEFFIC. OF POLYNOM.    R*8(IP+1)
CC               V      : VECTOR WITH RESIDUALS               R*8(N)
CC                        (DIFF APPROX-Y)
CC               DX,FT  : NORMALISATION OF IND. ARGUMENT      R*8
CC                        XNORM=(X-DX)*FT
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  W.GURTNER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/12/11 12:02
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE f_tstflg
      USE s_dminv
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I    , IK   , IP   , IPMAX, J    , K    , KI   , M    ,
     1          N    , NEFF
C
      REAL*8    DET  , DX   , FT   , RMS  , XMAX , XMIN , XREDJ, XREDM,
     1          YS
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (IPMAX=10)
C
      REAL*8      X(*),Y(*),C(*),V(*)
      REAL*8      Q(IPMAX*IPMAX),F(IPMAX),A(IPMAX)
      INTEGER*4   L0(IPMAX),M0(IPMAX)
      CHARACTER*1 L(*)
C
      IF(IP.GT.IPMAX) THEN
        WRITE(LFNERR,1) IP
1       FORMAT(/,' *** SR PIAPPR: POLYNOMIAL DEGREE TOO LARGE',/,
     1                       16X,'POLYNONIAL DEGREE:',I3,/)
        CALL EXITRC(2)
      END IF
C
C  NORMALISATION OF INDEPENDENT ARGUMENT
C  XNORM=(X-DX)*FT
      XMIN=1.D37
      XMAX=-1.D37
      DO 10 I=1,N
        IF(TSTFLG(L(I),0)) GOTO 10
        IF(X(I).GT.XMAX) XMAX=X(I)
        IF(X(I).LT.XMIN) XMIN=X(I)
10    CONTINUE
      DX=(XMAX+XMIN)/2.D0
      FT=2.D0/(XMAX-XMIN)
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
      DO 70 J=1,N-1
C
C  MARKED VALUES
        IF(TSTFLG(L(J),0)) GOTO 70
        XREDJ=(X(J)-DX)*FT
        DO 75 M=J+1,N
          IF(TSTFLG(L(M),0)) GOTO 75
          IF(TSTFLG(L(M),2)) GOTO 70
          GOTO 76
75      CONTINUE
        GOTO 77
76      XREDM=(X(M)-DX)*FT
        NEFF=NEFF+1
C
C  ROW # J OF A-MATRIX
        DO 40 K=1,IP
          A(K)=XREDM**K-XREDJ**K
40      CONTINUE
C
C  CONTRIBUTION TO Q-MATRIX (UPPER TRIANGLE)
45      DO 55 I=1,IP
          DO 50 K=I,IP
            IK=(K-1)*IP+I
            Q(IK)=Q(IK)+A(I)*A(K)
50        CONTINUE
55      CONTINUE
C
C  CONTRIBUTION TO RIGHT HAND SIDE
        DO 60 I=1,IP
          F(I)=F(I)+A(I)*(Y(M)-Y(J))
60      CONTINUE
C
70    CONTINUE
C
77    IF(NEFF.LT.IP) THEN
        WRITE(LFNERR,2) NEFF,IP
2       FORMAT(/,' *** SR PIAPPR: NOT ENOUGH OBSERVATIONS',/,
     1                       16X,'NUMBER OF OBSERVATIONS:',I5,/,
     2                       16X,'POLYNOMIAL DEGREE     :',I5,/)
        CALL EXITRC(2)
      END IF
C
C  LOWER TRIANGLE OF Q MATRIX
      DO 80 I=1,IP
        DO 80 K=1,I
          IK=(K-1)*IP+I
          KI=(I-1)*IP+K
          Q(IK)=Q(KI)
80    CONTINUE
C
C  INVERSION OF Q MATRIX
      CALL DMINV(Q,IP,DET,L0,M0)
C
C  SOLUTION VECTOR (POLYNOMIAL COEFFICIENTS)
      DO 90 I=1,IP
        C(I)=0.D0
        DO 90 K=1,IP
          IK=(K-1)*IP+I
          C(I)=C(I)+Q(IK)*F(K)
90    CONTINUE
C
C  RESIDUALS, RMS
      RMS=0.D0
      DO 110 J=1,N-1
        IF(TSTFLG(L(J),0)) GOTO 110
        XREDJ=(X(J)-DX)*FT
        DO 115 M=J+1,N
          IF(.NOT.TSTFLG(L(M),0)) GOTO 116
115     CONTINUE
        GOTO 117
116     YS=0.D0
        XREDM=(X(M)-DX)*FT
        DO 100 K=1,IP
          YS=YS+C(K)*(XREDM**K-XREDJ**K)
100     CONTINUE
120     V(M)=YS-(Y(M)-Y(J))
        IF(.NOT.TSTFLG(L(M),2)) RMS=RMS+V(M)*V(M)
110   CONTINUE
117   IF(NEFF-IP.NE.0.AND.RMS.GT.0.D0) THEN
         RMS=DSQRT(RMS/(NEFF-IP))
      ELSE
         RMS=0.D0
      END IF
C
      RETURN
      END SUBROUTINE

      END MODULE
