      MODULE f_aslefu
      CONTAINS

C*
      FUNCTION ASLEFU(XLAT,XLON,INN,IMM,IFLAG)
CC
CC NAME       :  ASLEFU
CC
CC PURPOSE    :  COMPUTE THE (NORMALIZED) ASSOCIATED LEGENDRE FUNCTION
CC               OF DEGREE N AND ORDER M EVALUATED AT X.
CC
CC PARAMETERS :
CC         IN :  XLAT   : LATITUDE                            R*8
CC               XLON   : LONGITUDE                           R*8
CC               INN    : DEGREE                              I*4
CC                        =+: XX=SIN(XLAT)
CC                        =-: XX=COS(XLAT)
CC               IMM    : ORDER                               I*4
CC                        =+: COS-TERM
CC                        =-: SIN-TERM
CC               IFLAG  : FLAG FOR NORMALIZATION              I*4
CC                        =0: NO
CC                        =1: YES
CC        OUT :  ASLEFU : (NORMALIZED) COEFFICIENT            R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S.SCHAER
CC
CC VERSION    :  3.6
CC
CC CREATED    :  23-FEB-95
CC
CC CHANGES    :  06-MAR-95 : SS: NORMALIZATION FACTOR INTRODUCED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1995     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE f_fakult
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IFLAG , II    , IMM   , INN   , KDELTA, LL    , MM    ,
     1          NN
C
      REAL*8    ASLEFU, COEF  , FACT  , FACTN , PLL   , PMM   ,
     1          PMMP1 , SOMX2 , XLAT  , XLON  , XX
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C
      NN=IABS(INN)
      MM=IABS(IMM)
C
C CHECK INPUT PARAMETERS
C ----------------------
      IF (MM.GT.NN) THEN
        WRITE(LFNERR,910) NN,MM
910     FORMAT(/,' *** SR ASLEFU: BAD ARGUMENTS',/,
     1                       16X,'N = ',I3,/,
     2                       16X,'M = ',I3,/)
        CALL EXITRC(2)
      END IF
C
C COMPUTE ARGUMENT X AND THE ASSOCIATED LEGENDRE POLYNOMIAL
C ---------------------------------------------------------
      IF (INN.GE.0) THEN
        XX=DSIN(XLAT)
      ELSE
        XX=DCOS(XLAT)
      END IF
C
      PMM=1.D0
      IF (MM.GT.0) THEN
        SOMX2=DSQRT((1.D0-XX)*(1.D0+XX))
        FACT=1.D0
        DO 110 II=1,MM
          PMM=PMM*FACT*SOMX2
          FACT=FACT+2.D0
110     CONTINUE
      END IF
      IF (NN.EQ.MM) THEN
        COEF=PMM
      ELSE
        PMMP1=XX*(2*MM+1)*PMM
        IF (NN.EQ.MM+1) THEN
          COEF=PMMP1
        ELSE
          DO 120 LL=MM+2,NN
            PLL=(XX*(2*LL-1)*PMMP1-(LL+MM-1)*PMM)/(LL-MM)
            PMM=PMMP1
            PMMP1=PLL
120       CONTINUE
          COEF=PLL
        END IF
      END IF
C
C COMPUTE NORMALIZATION FACTOR AND (NORMALIZED) COEFFICIENT
C ---------------------------------------------------------
      IF (IFLAG.EQ.1) THEN
        IF (MM.EQ.0) THEN
          KDELTA=1
        ELSE
          KDELTA=0
        END IF
        FACTN=DSQRT(2.D0*(2.D0*NN+1.D0)/(1.D0+KDELTA)*
     1    FAKULT(NN-MM)/FAKULT(NN+MM))
      ELSE
        FACTN=1.D0
      END IF
C
      IF (IMM.GE.0) THEN
        ASLEFU=FACTN*COEF*DCOS(MM*XLON)
      ELSE
        ASLEFU=FACTN*COEF*DSIN(MM*XLON)
      END IF
C
999   RETURN
      END FUNCTION

      END MODULE
