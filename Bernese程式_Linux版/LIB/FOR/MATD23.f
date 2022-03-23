      MODULE s_MATD23
      CONTAINS

C*
      SUBROUTINE MATD23(NPN,NAMB,N11,N12,N22,U1,U2,D1,D2,D3)
CC
CC NAME       :  MATD23
CC
CC PURPOSE    :  COMPUTE MATRICES D1, D2, D3 FOR AMBIGUITY
CC               RESOLUTION USING A SEARCH STRATEGY
CC               SEE TR 108, UNB, P.116
CC
CC PARAMETERS :
CC         IN :  NPN    : NUMBER OF NON-AMBIGUITY PARAMETERS  I*4
CC               NAMB   : NUMBER OF AMBIGUITY PARAMETERS      I*4
CC               N11(I),I=1,..,NPN*(NPN+1)/2: UPPER TRIANG.   R*8
CC                        (INVERTED)
CC                        PART OF NEQ SYSTEM (WITHOUT AMBS)
CC               N12(IK),IK=1,..,NPN*NAMB: PART OF NEQ SYSTEM R*8
CC               N22(I),I=1,..,NAMB*NAMB+1)/2: PART OF NEQ.   R*8
CC                        SYSTEM (SEE EQN. (9.11)
CC               U1(I),I=1,..,NPN: SEE EQN. (9.11)            R*8
CC               U2(I),I=1,..,NAMB:SEE EQN. (9.11)            R*8
CC     IN/OUT :  D1  : SEE EQN. (9.21)                        R*8
CC        OUT :  D2(I),I=1,..,NAMB: SEE EQN. (9.21)           R*8
CC               D3(IK),IK=1,..,NAMB*NAMB: SEE EQN. (9.21)    R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G. BEUTLER
CC
CC VERSION    :  3.4  (JAN 94)
CC
CC CREATED    :  89/07/01 11:11
CC
CC CHANGES    :  01-JUL-94 : SS: MAXPP1=300 (INSTEAD OF 10)
CC               25-JUL-94 : MR: MAXAMB=200 (INSTEAD OF 100)
CC               10-AUG-94 : MR: CALL EXITRC
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1989     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE f_ikf
      USE s_exitrc
      USE s_solve
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IK    , IL    , K     , KI    , L     ,
     1          LK    , MAXAMB, MAXPP1, NAMB  , NPN
C
      REAL*8    D1
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MAXPP1=300,MAXAMB=200)
C
      REAL*8 N11(*),N12(*),N22(*),U1(*),U2(*),D2(*),D3(*)
      REAL*8 X(MAXPP1),DH(MAXPP1*MAXAMB)
C
C
C CHECK MAXIMUM LOCAL DIMENSIONS
C ------------------------------
      IF(NPN.GT.MAXPP1)THEN
        WRITE(LFNERR,1) NPN,MAXPP1
1       FORMAT(/,' *** SR MATD23: TOO MANY NON-AMBIGUITY PARAMETERS',/,
     1            16X,'NUMBER OF NON-AMBIG. PARAMETERS :',I4,/,
     2            16X,'MAX. NUMBER OF NON-AMBIG. PARAM.:',I4,/)
        CALL EXITRC(2)
      END IF
      IF(NAMB.GT.MAXAMB)THEN
        WRITE(LFNERR,2) NAMB,MAXAMB
2       FORMAT(/,' *** SR MATD23: TOO MANY AMBIGUITY PARAMETERS',/,
     1            16X,'NUMBER OF AMBIG. PARAMETERS :',I4,/,
     2            16X,'MAX. NUMBER OF AMBIG. PARAM.:',I4,/)
        CALL EXITRC(2)
      END IF
C
C AMBIGUITY RESOLUTION, STRATEGY 2
C (SEE TECHNICAL REPORT 108, UNB, FREDERICTON)
C --------------------------------------------
      CALL SOLVE(NPN,N11,U1,X)
C
C COMPUTE MATRICES D2 AND D3 (SEE TR 108 OF UNB,(9.21))
C -----------------------------------------------------
      DO 90 I=1,NAMB
        D2(I)=-2*U2(I)
        DO 80 K=1,NPN
          KI=K+(I-1)*NPN
          D2(I)=D2(I)+2*X(K)*N12(KI)
80      CONTINUE
90    CONTINUE
C
      DO 100 I=1,NPN
        D1=D1-U1(I)*X(I)
        DO 100 K=1,NAMB
          IK=I+(K-1)*NPN
          DH(IK)=0.D0
          DO 100 L=1,NPN
            IL=IKF(I,L)
            LK=L+(K-1)*NPN
            DH(IK)=DH(IK)+N11(IL)*N12(LK)
100   CONTINUE
      DO 110 I=1,NAMB
        DO 110 K=1,NAMB
          IK=I+(K-1)*NAMB
          D3(IK)=N22(IKF(I,K))
          DO 110 L=1,NPN
            IL=L+(I-1)*NPN
            LK=L+(K-1)*NPN
            D3(IK)=D3(IK)-N12(IL)*DH(LK)
110   CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
