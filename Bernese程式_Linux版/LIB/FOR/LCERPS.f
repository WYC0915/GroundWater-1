      MODULE s_LCERPS
      CONTAINS

C*
      SUBROUTINE LCERPS(IINTER,NPAR,LOCQ,XXX,QNOR,
     1                  RMS,TPOL,TSAV,VAL,ERR,RATSIG,CORREL)
CC
CC NAME       :  LCERPS
CC
CC PURPOSE    :  COMPUTE VALUE OF EARTH ROTATION PARAMETERS AT TIME TSAV
CC               WITHIN INTERVAL IINTER, THE ASSOCIATED RMS ERRORS AND
CC               THE CORRELATIONS BETWEEN X,Y,UT,DE,DP
CC
CC PARAMETERS :
CC         IN :  IINTER : USE ERP-SET NR IINTER TO COMPUTE THE  I*4
CC                        CORRECTION
CC               NPAR   : NUMBER OF PARAMETERS IN PROCESSING    I*4
CC               LOCQ   : PARAMETER DESCRIPTION                 I*4(*,*)
CC               XXX    : SOLUTION VECTOR                       R*8(*)
CC               RMS    : MEAN ERROR OF WEIGHT UNIT             R*8
CC               QNOR   : INVERTED NEQ MATRIX                   R*8
CC               TSAV   : EPOCH FOR THE RESULTING CORRECTION    R*8
CC       OUT :   VAL    : VALUE OF ERPS AT TIME TSAV            R*8
CC               ERR    : CORRESPONDING RMS ERROR               R*8
CC               RATSIG : SIGMA OF RATES OF ERP                 R*8(*)
CC               CORREL : CORRELATION MATRIX BETWEEN            R*8(*)
CC                        X,Y,UT,DE,DP
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G. BEUTLER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  20-MAR-93
CC
CC CHANGES    :  19-APR-94 : RW: CPO-MODEL INCLUDED
CC               21-JUN-94 : SF: SIGMA OF DRIFTS (RATSIG) INCLUDED
CC               10-AUG-94 : MR: CALL EXITRC
CC               10-SEP-95 : LM: REORDER DATA STATEMENT
CC               04-JUN-97 : MR: CHECK FOR NEGATIVE DIAGONAL ELEMENT IN
CC                               INVERTED NORMAL EQUATION MATRIX
CC               05-JUN-97 : MR: USE ".GE." INSTEAD OF ".GT."
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1993     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE f_ikf
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I1    , ICOR  , ICOREL, IDEG  , IINTER, IK    ,
     1          IPAR  , ITERM , K     , KTERM , L     , LM    ,
     2          M     , MAXPTM, MXCLCQ, NEOP  , NPAR
C
      REAL*8    RMS   , TIMINT, TSAV
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER       (MAXPTM=200)
C
C GLOBAL DECLARATION
C ------------------
      CHARACTER*6  MXNLCQ
      REAL*8       XXX(*),QNOR(*),TPOL(2,*),VAL(*),ERR(*),CORREL(*)
      REAL*8       RATSIG(*)
      INTEGER*4    LOCQ(MXCLCQ,*)
C
C INTERNAL DECLARATION
C --------------------
      INTEGER*4    NTERM(5),IELE(MAXPTM,5),IHCORR(4)
      REAL*8       COE(MAXPTM,5)
C
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
C
C
      DATA IHCORR/0,4,7,9/
C
C DEFINE LINEAR COMBINATION, COMPUTE MODEL VALUES AT TIME TSAV,
C AND ASSOCIATED RMS ERRORS
C -------------------------------------------------------------
      DO 1000 ICOR=1,5
        VAL(ICOR)=0.D0
        ERR(ICOR)=0.D0
        NTERM(ICOR)=0
C
C POLYNOMIAL DEGREE > 0
        ITERM=0
        TIMINT=TSAV-TPOL(1,IINTER)
        DO 210 IPAR=1,NPAR
          IF(LOCQ(1,IPAR).EQ.10.AND.LOCQ(4,IPAR).EQ.ICOR.AND.
     1       LOCQ(3,IPAR).EQ.IINTER) THEN
            ITERM=ITERM+1
            IDEG=LOCQ(5,IPAR)
            NTERM(ICOR)=LOCQ(6,IPAR)
            IF(ITERM.GT.MAXPTM)THEN
              WRITE(LFNERR,205) ITERM,MAXPTM
205           FORMAT(/,' *** SR LCERPS: TOO MANY EARTH ROTATION',
     1                 ' PARAMETERS',/,
     2                             16X,'NUMBER OF PARAMETERS >=',I4,/,
     3                             16X,'MAXIMUM NUMBER ALLOWED:',I4,/,
     4                             16X,'INCREASE PARAMETER "MAXPTM"',/)
              CALL EXITRC(2)
            END IF
            IELE(ITERM,ICOR)=IPAR
            IF(IDEG.NE.1)THEN
              VAL(ICOR)=VAL(ICOR)+
     1                  XXX(IPAR)/1000.D0*TIMINT**(IDEG-1)
              COE(ITERM,ICOR) =TIMINT**(IDEG-1)/1000.D0
            ELSE
              VAL(ICOR)=VAL(ICOR)+
     1                  XXX(IPAR)/1000.D0
              COE(ITERM,ICOR) =1.D0/1000.D0
            END IF
          END IF
210     CONTINUE
ccc        IF(ITERM.NE.NTERM(ICOR))THEN
ccc          WRITE(LFNERR,240)
ccc240       FORMAT(/,' *** SR LCERPS: NUMBER OF TERMS NOT O.K.',/)
ccc        END IF
C
C COMPUTE MEAN ERROR OF TERMS
        DO 300 ITERM=1,NTERM(ICOR)
          I=IELE(ITERM,ICOR)
          DO 300 KTERM=1,NTERM(ICOR)
            K=IELE(KTERM,ICOR)
            IK=IKF(I,K)
            ERR(ICOR)=ERR(ICOR)+COE(ITERM,ICOR)*QNOR(IK)*
     1                COE(KTERM,ICOR)
300     CONTINUE
        IF (ERR(ICOR).GE.0.D0) THEN
          ERR(ICOR)=RMS*DSQRT(ERR(ICOR))
        ELSE
          ERR(ICOR)=9.9999D0
        ENDIF
        IF (NTERM(ICOR).GE.2) THEN
          I1=IELE(2,ICOR)
          IF (QNOR(IKF(I1,I1)).GE.0.D0) THEN
            RATSIG(ICOR)=RMS*DSQRT(QNOR(IKF(I1,I1)))/1000.D0
          ELSE
            RATSIG(ICOR)=9.9999D0
          ENDIF
        ELSE
          RATSIG(ICOR)=0.D0
        ENDIF
1000  CONTINUE
C
C COMPUTE CORRELATIONS BETWEEN X,Y,UT,EP,PSI
C ------------------------------------------
C NEOP ... MAX. NUMBER OF EOP-TYPES
      NEOP=5
      DO 2000 I=1,NEOP-1
       DO 2000 K=I+1,NEOP
C
C ICOREL:
C 1.. X-Y, 2.. X-UT, 3.. X-EP, 4.. X-PS,
C          5.. Y-UT, 6.. Y-EP, 7.. Y-PS,
C                    8..UT-EP, 9..UT-PS,
C                             10..EP-PS
        ICOREL=K-I+IHCORR(I)
        CORREL(ICOREL)=0.D0
        IF(NTERM(I).NE.0.AND.NTERM(K).NE.0)THEN
          DO 1900 L=1,NTERM(I)
            DO 1900 M=1,NTERM(K)
              LM=IKF(IELE(L,I),IELE(M,K))
              CORREL(ICOREL)=CORREL(ICOREL)+
     1                       COE(L,I)*QNOR(LM)*COE(M,K)
1900      CONTINUE
          if ( err(i)*err(k) .ne. 0.d0 ) then
            CORREL(ICOREL)=CORREL(ICOREL)/(ERR(I)*ERR(K))*RMS**2
          else
            CORREL(ICOREL)=0.D0
          end if
        ELSE
          CORREL(ICOREL)=0.D0
        END IF
2000  CONTINUE
999   RETURN
      END SUBROUTINE

      END MODULE
