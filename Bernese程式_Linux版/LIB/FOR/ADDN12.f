      MODULE s_ADDN12
      CONTAINS

C*
      SUBROUTINE ADDN12(NEQ   ,NPAEPO,NPAR12,WGTGEN,P     ,AOBS  ,
     1                  INDA  ,ASNG12,IND12 ,INDI  ,HELP  ,MXCEQN,
     2                  ATMP12)
CC
CC NAME       :  ADDN12
CC
CC PURPOSE    :  UPDATE NORMAL EQUATION SYSTEM IN BERNESE GPS
CC               SOFTWARE N12 PART ONLY:
CC                                        T
CC               ATMP12=ATMP12 + WGTGEN*ASNG12 * P * AOBS
CC
CC PARAMETERS :
CC         IN :  NEQ    : NUMBER OF OBSERVATION EQUATIONS     I*4
CC               NPAEPO : NUMBER OF EPOCH PARAMETERS  IN N22  I*4
CC               NPAR12 : NUMBER OF COMMON PARAMETERS IN N11  I*4
CC               WGTGEN : GENERAL WEIGHT FACTOR               R*8
CC               P      : WEIGHT MATRIX, UPPER TRIANGULAR     R*8
CC                        PART, COLUMNWISE LINEARIZED
CC               AOBS   : FIST DESIGN MATRIX                  R*8
CC                        COLUMNWISE LINEARIZED
CC               INDA   : INDEX FOR MATRIX AOBS (SAME STRUC-  I*2
CC                        TURE) AS AOBS
CC               AOBS12 : FIST DESIGN MATRIX                  R*8
CC                        COLUMNWISE LINEARIZED
CC               INDA12 : INDEX FOR MATRIX AOBS12 (SAME STRU- I*2
CC                        CTURE) AS AOBS12
CC               INDI   : AUXILIARY 1-DIM ARRAY               I*2
CC               HELP   : AUXILIARY 1-DIM ARRAY               R*8
CC               MXCEQN : MAX. NUMBER OF EQNS PER EPOCH       I*4
CC     IN/OUT :  ATMP12 : NORMAL EQN MATRIX, UPPER TRIANGULAR R*8
CC                        PART, COLUMNWISE LINEARIZED
CC
CC REMARKS    :  ONLY NON-ZERO ELEMENTS ARE GIVEN IN AOBS.
CC               INDA CONTAINS INDICES OF THESE ELEMENTS IN
CC               "ORIGINAL" MATRIX
CC
CC AUTHOR     :  D.SVEHLA, M.ROTHACHER
CC
CC REMARK     :
CC
CC VERSION    :
CC
CC CREATED    :  01/11/24
CC
CC CHANGES    :  30-JUL-02 : DS: P matrix REAL*4->REAL*8
CC               28-JAN-03 : RS: COMMENT CORRECTED (R*4 -> R*8)
CC               07-MAR-03 : HU: USE M_MAXDIM AND PREPROCESSOR COMMANDS
CC               19-NOV-04 : RD: Interface for ADNHLP.f
CC               26-MAY-05 : GB/RD: DYN. ARRAYS->LOCALLY ALLOCATED ARRAYS
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC               UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern, ONLY: i4b
c      USE M_MAXDIM, ONLY: MAXFLS,MAXSAS
      USE f_ikf
      USE s_adnhlp
      USE s_alcerr
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IAC    , ICOL   , ICOL12 , IFIRST , IK     ,
     1          IP     , IPAR   , IPAR12 , KEQ    , KP     , L      ,
     2          LINE   , LINE12 , LK     , MXCEQN , MXCSNG , MXXEQN ,
     3          NEQ    , NPAEPO , NPAR12 , NPEFF  , NPEFF12
C
      REAL*8    HLPSKL , WGTGEN
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
c      PARAMETER (MAXEQN=MAXSAS*MAXFLS)
C
      CHARACTER(LEN=6), PARAMETER :: SRNAME= 'ADDN12'
      CHARACTER*6 MXNEQN,MXNSNG
C
      INTEGER*4 INDA(MXCEQN,*),IND12(MXCEQN,*),INDI(*)
C      INTEGER*4 NLIN(NPAEPO),IPAEXT(NPAEPO)
C      INTEGER*4 NLIN12(NPAR12),IPAEXT12(NPAR12)
C      INTEGER*4 LINNUM(NEQ,NPAEPO),COLNUM(NEQ,NPAEPO)
C      INTEGER*4 LINNUM12(NEQ,NPAR12),COLNUM12(NEQ,NPAR12)
      INTEGER(i4b), DIMENSION(:),   ALLOCATABLE :: NLIN                  ! (NPAEPO)
      INTEGER(i4b), DIMENSION(:),   ALLOCATABLE :: IPAEXT                ! (NPAEPO)
      INTEGER(i4b), DIMENSION(:),   ALLOCATABLE :: NLIN12                ! (NPAR12)
      INTEGER(i4b), DIMENSION(:),   ALLOCATABLE :: IPAEXT12              ! (NPAR12)
      INTEGER(i4b), DIMENSION(:,:), ALLOCATABLE :: LINNUM                ! (NEQ,NPAEPO)
      INTEGER(i4b), DIMENSION(:,:), ALLOCATABLE :: COLNUM                ! (NEQ,NPAEPO)
      INTEGER(i4b), DIMENSION(:,:), ALLOCATABLE :: LINNUM12              ! (NEQ,NPAR12)
      INTEGER(i4b), DIMENSION(:,:), ALLOCATABLE :: COLNUM12              ! (NEQ,NPAR12)
C
      REAL*8    AOBS(MXCEQN,*),ASNG12(MXCEQN,*),ATMP12(*),HELP(*)
C
      REAL*8    P(*)
C
      COMMON/MCMEQN/MXXEQN,MXNEQN
      COMMON/MCMSNG/MXCSNG,MXNSNG
C
      DATA IFIRST/1/
C
      IF(IFIRST.EQ.1)THEN
        IFIRST = 0
c        CALL MAXTST(1,'ADDN12',MXNEQN,MAXEQN,MXCEQN,IRC1)
c        IF(IRC1.NE.0) CALL EXITRC(2)
      END IF
C
C ALLOCATE LOCAL ARRAYS
      ALLOCATE(NLIN(NPAEPO),STAT=IAC)
      CALL ALCERR(IAC,'NLIN',(/NPAEPO/),SRNAME)
      NLIN=0
C
      ALLOCATE(IPAEXT(NPAEPO),STAT=IAC)
      CALL ALCERR(IAC,'IPAEXT',(/NPAEPO/),SRNAME)
      IPAEXT=0
C
      ALLOCATE(NLIN12(NPAR12),STAT=IAC)
      CALL ALCERR(IAC,'NLIN12',(/NPAR12/),SRNAME)
      NLIN12=0
C
      ALLOCATE(IPAEXT12(NPAR12),STAT=IAC)
      CALL ALCERR(IAC,'IPAEXT12',(/NPAR12/),SRNAME)
      IPAEXT12=0
C
      ALLOCATE(LINNUM(NEQ,NPAEPO),STAT=IAC)
      CALL ALCERR(IAC,'LINNUM',(/NEQ,NPAEPO/),SRNAME)
      LINNUM=0
C
      ALLOCATE(COLNUM(NEQ,NPAEPO),STAT=IAC)
      CALL ALCERR(IAC,'COLNUM',(/NEQ,NPAEPO/),SRNAME)
      COLNUM=0
C
      ALLOCATE(LINNUM12(NEQ,NPAR12),STAT=IAC)
      CALL ALCERR(IAC,'LINNUM12',(/NEQ,NPAR12/),SRNAME)
      LINNUM12=0
C
      ALLOCATE(COLNUM12(NEQ,NPAR12),STAT=IAC)
      CALL ALCERR(IAC,'COLNUM12',(/NEQ,NPAR12/),SRNAME)
      COLNUM12=0
C
C ORGANIZE PROCESSING
      CALL ADNHLP(NEQ,NPAEPO,INDA,NPAEPO,NEQ,MXCEQN,INDI,NPEFF,
     1            IPAEXT,NLIN,LINNUM,COLNUM)
      CALL ADNHLP(NEQ,NPAR12,IND12,NPAR12,NEQ,MXCEQN,INDI,NPEFF12,
     1            IPAEXT12,NLIN12,LINNUM12,COLNUM12)
C
C COMPUTE N12 (MATRIX ATMP12)
C ---------------------------
C
C HELP= ONE LINE OF MATRIX PRODUCT TRN(ASNG12)*P
      DO IP=1,NPEFF12
                             IPAR12=IPAEXT12(IP)
        DO KEQ=1,NEQ
          HELP(KEQ)=0.D0
          DO L=1,NLIN12(IP)
            LINE12=LINNUM12(L,IP)
            ICOL12=COLNUM12(L,IP)
            LK=IKF(LINE12,KEQ)
            HELP(KEQ)=HELP(KEQ)+ASNG12(LINE12,ICOL12)*P(LK)
          END DO
        END DO

        DO KP=1,NPEFF
                                IPAR=IPAEXT(KP)
           IK=NPAR12*(KP-1)+IP
          ATMP12(IK)=0.D0
          HLPSKL=0.D0
          DO L=1,NLIN(KP)
            LINE=LINNUM(L,KP)
            ICOL=COLNUM(L,KP)
            HLPSKL=HLPSKL+HELP(LINE)*AOBS(LINE,ICOL)
          END DO
            ATMP12(IK)=ATMP12(IK)+HLPSKL*WGTGEN
        END DO
      END DO
C
C DEALLOCATE LOCAL ARRAYS
      DEALLOCATE(NLIN)
      DEALLOCATE(IPAEXT)
      DEALLOCATE(NLIN12)
      DEALLOCATE(IPAEXT12)
      DEALLOCATE(LINNUM)
      DEALLOCATE(COLNUM)
      DEALLOCATE(LINNUM12)
      DEALLOCATE(COLNUM12)
C
      RETURN
      END SUBROUTINE

      END MODULE
