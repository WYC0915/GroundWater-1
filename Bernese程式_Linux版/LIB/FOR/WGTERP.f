      MODULE s_WGTERP
      CONTAINS

C*
      SUBROUTINE WGTERP(ISGPOL,NPAR,LOCQ,TPOL,S0,SIGACT,ANOR)
CC
CC NAME       :  WGTERP
CC
CC PURPOSE    :  COMPUTE A PRIORI OCCUPATION OF NORMAL EQUATION MATRIX
CC               FOR ERP PARAMETERS
CC
CC PARAMETERS :
CC         IN :  ISGPOL : =0 : APPLY ONLY ABSOLUTE SIGMAS       I*4
CC                        =1 : ESTABLISH CONTINUITY WITH RESPECT
CC                             TO PREVIOUS SET OF ERPS.
CC                        =4 : CONSTRAIN DRIFTS TO ZERO
CC                        =5 : ENSURE CONTINUITY WITH RESPECT
CC                             TO PREVIOUS POLYNOMIAL AND
CC                             CONSTRAIN DRIFTS TO ZERO
CC               NPAR   : CURRENT PARAMETER NUMBER              I*4
CC               LOCQ   : PARAMETER DESCRIPTION                 I*4(*,*)
CC               TPOL   : INTERVAL BOUNDARIES FOR ERPS          R*8(2,*)
CC               S0     : WEIGHT UNIT                           R*8
CC               SIGACT : SIGMA OF CURRENT INTERVAL/COMPONENT   R*8
CC                        COMBINATION
CC       OUT :   ANOR   : UPDATED NEQ MATRIX                    R*8(*)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G. BEUTLER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  27-JAN-93
CC
CC CHANGES    :  25-APR-94 : MR: CONSTRAINTS FOR ERP DRIFTS
CC               10-AUG-94 : MR: CALL EXITRC
CC               08-JUL-97 : LM: ANOR += INSTEAD OF ANOR =
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
      INTEGER*4 I     , ICOR  , IDEG  , IK    , INTACT, INTER ,
     1          IPAR  , IPIP  , ISGPOL, ITERM , K     , KTERM , MAXPTM,
     2          MXCLCQ, NDEG  , NPAR  , NTERM
C
      REAL*8    S0    , SIGACT, SIGDRF, SIGREL, TIMINT, WEIGHT
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MAXPTM=200)
C
C GLOBAL DECLARATION
C ------------------
      CHARACTER*6  MXNLCQ
      REAL*8       ANOR(*),TPOL(2,*)
      INTEGER*4    LOCQ(MXCLCQ,*)
C
C INTERNAL DECLARATION
C --------------------
      INTEGER*4    IELE(MAXPTM)
      REAL*8       COE(MAXPTM)
C
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
C
C
C SIGMA TO ENSURE CONTINUITY, SIGMA TO CONSTRAIN DRIFT TO ZERO
C ------------------------------------------------------------
      SIGREL=1.D-6
      SIGDRF=1.D-6
C
C DEFINE CURRENT REQUEST
C ----------------------
      INTACT=LOCQ(3,NPAR)
      ICOR  =LOCQ(4,NPAR)
      IDEG  =LOCQ(5,NPAR)
      NDEG  =LOCQ(6,NPAR)
C
C RETURN, IF DEGREE OF COEFFICIENT IS NOT ZERO
C --------------------------------------------
      IF (IDEG.EQ.1) THEN
        IF (SIGACT.NE.0.D0) THEN
          IPIP=IKF(NPAR,NPAR)
          WEIGHT=(S0/SIGACT)**2
          ANOR(IPIP)=anor(ipip) + WEIGHT
        END IF
C
C DEFINE LINEAR COMBINATION, COMPUTE MODEL VALUES AT TIME TSAV,
C AND ASSOCIATED RMS ERRORS
C -------------------------------------------------------------
        IF ((ISGPOL.EQ.1.OR.ISGPOL.EQ.5).AND.INTACT.NE.1) THEN
          NTERM=NDEG+1
          ITERM=0
          INTER=INTACT-1
          TIMINT=TPOL(2,INTER)-TPOL(1,INTER)
          DO 210 IPAR=1,NPAR
            IF(LOCQ(1,IPAR).EQ.10.AND.LOCQ(4,IPAR).EQ.ICOR.AND.
     1         LOCQ(3,IPAR).EQ.INTER)THEN
              ITERM=ITERM+1
              IDEG=LOCQ(5,IPAR)
              IF(ITERM.GT.MAXPTM)THEN
                WRITE(LFNERR,205) ITERM,MAXPTM
205             FORMAT(/,' *** SR WGTERP: TOO MANY EARTH ROTATION',
     1                   ' PARAMETERS',/,
     2                             16X,'NUMBER OF PARAMETERS >=',I4,/,
     3                             16X,'MAXIMUM NUMBER ALLOWED:',I4,/,
     4                             16X,'INCREASE PARAMETER "MAXPTM"',/)
                CALL EXITRC(2)
              END IF
              IELE(ITERM)=IPAR
              IF (IDEG.NE.1) THEN
                COE(ITERM) =TIMINT**(IDEG-1)
              ELSE
                COE(ITERM) =1.D0
              END IF
            END IF
210       CONTINUE
C
C CURRENT TERM :
          ITERM=ITERM+1
          IF(ITERM.GT.MAXPTM)THEN
            WRITE(LFNERR,205) ITERM,MAXPTM
            CALL EXITRC(2)
          END IF
          COE(ITERM)=-1.D0
          IELE(ITERM)=NPAR
C
          IF(ITERM.NE.NTERM)THEN
            WRITE(LFNERR,240)
240         FORMAT(/,' *** SR WGTERP: NUMBER OF TERMS NOT O.K.',/)
          END IF
C
C DEFINE NON-ZERO ELEMENTS OF MATRIX A :
          DO 300 ITERM=1,NTERM
            I=IELE(ITERM)
            DO 300 KTERM=1,ITERM
              K=IELE(KTERM)
              IK=IKF(I,K)
              ANOR(IK)=ANOR(IK)+(S0/SIGREL)**2*COE(ITERM)*COE(KTERM)
300       CONTINUE
        ENDIF
C
C CONSTRAIN DRIFTS TO ZERO IF "ISGPOL=4"
C --------------------------------------
      ELSE IF (IDEG.EQ.2 .AND. ISGPOL.GE.4) THEN
        IPIP=IKF(NPAR,NPAR)
        WEIGHT=(S0/SIGDRF)**2
        ANOR(IPIP)=ANOR(IPIP)+WEIGHT
      END IF
C
      RETURN
      END SUBROUTINE

      END MODULE
