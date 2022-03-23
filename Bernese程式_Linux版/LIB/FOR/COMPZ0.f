      MODULE s_COMPZ0
      CONTAINS

C*
      SUBROUTINE COMPZ0(NSAT,SVNNUM,SVN,TACT,RPRFIL,IFIL,Z0)
CC
CC NAME       : COMPZ0
CC
CC PURPOSE    : COMPUTE PARTIALS WITH RESPECT TO INITIAL CONDITIONS
CC              FOR ALL SATELLITES OF A LIST SVNNUM(ISAT),I=1,2,..,NSAT
CC              AND RETURN PARTIALS FOR SATELLITE SVN.
CC
CC PARAMETERS :
CC        IN  : NSAT    : NUMBER OF SATS IN LIST                    I*4
CC              SVNNUM  : CORRESPONDING SATELLITE NUMBERS           I*4
CC              SVN     : SATELLITE NUMBER REQUESTED                I*4
CC              TACT    : TIME REQUESTED                            R*8
CC              RPRFIL  : FILE WITH RPR-COEFFICIENTS                CH*32
CC        OUT : Z0      : PARTIALS FOR SPECIAL SATELLITE            R*8
CC
CC REMARKS    :
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  4.0  (JAN 96)
CC
CC CREATED    :  96/02/01
CC
CC CHANGES    :  06-JUN-96 : TS: REMOVED UNUSED VARIABLES
CC               25-SEP-97 : DI: USE MAXSAT.inc
CC               16-AUG-99 : RD: DIMENSIONS (SMALL/MEDIUM/LARGE) USING I:ADDNEQ
CC               17-FEB-03 : LM: USE M_MAXDIM, P_ADDOLD
CC               08-MAR-03 : HU: UNUSED MAXxxx REMOVED
CC               30-AUG-03 : HU: SHARED DO LABELS REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               16-Jul-08 : RD: USE P_ADDOLD->P_ADDNEQ
CC               28-OCT-08 : DT: Use maxVar from M_MAXDIM
CC               03-DEC-10 : HB: ADD PARAMETER FOR SR PRTDER
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1996     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_maxdim, ONLY: MAXSAT,MAXSTC,MAXVAR
      USE p_addneq, ONLY: MAXSTD

      USE s_exitrc
      USE s_inlist
      USE s_prtder
      USE s_maxtst
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I1    , ICRARC, IFIL  , IFIRST, IORSYS, IPAR  ,
     1          IRC   , IRCC  , IRCS  , IRCT  , IRCV  , ISAT  , ITIM  ,
     2          K     , K1    , KPAR  , KSAT  , L     , MXCSAT, MXCSTC,
     3          MXCSTD, MXCVAR, NRAD  , NSAT  , NSATLC, NTIM  , NVAR
C
      REAL*8    TACT
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
C MAXIMAL DIMENSIONS
C ------------------
C
C
C
      CHARACTER*6  MXNSAT,MXNSTC,MXNVAR,MXNSTD
      CHARACTER*8  ANLTYP
      CHARACTER*32 RPRFIL
C
      INTEGER*4    SVNNUM(*),SVN,SVNINT(MAXSAT)
C
      REAL*8       Z0(6,6),Z0INT(6,6,MAXSAT,MAXSTC,MAXSTD)
      REAL*8       ELESAT(7),TIMINT(MAXSTC),RPRPAR(MAXVAR)
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMSTC/MXCSTC,MXNSTC
      COMMON/MCMVAR/MXCVAR,MXNVAR
C
      DATA         IFIRST/1/
C
C INITIALIZE
C ----------
      IF(IFIRST.EQ.1)THEN
        IFIRST=0
        NTIM=0
        NSATLC=0
        DO 10 I=1,MAXSAT
          DO K=1,MAXSTC
            DO L=1,MAXSTD
              DO I1=1,6
                DO K1=1,6
                  Z0INT(I1,K1,I,K,L)=0.D0
                ENDDO
              ENDDO
            ENDDO
          ENDDO
10      CONTINUE
        CALL MAXTST(1,'COMPZ0',MXNSTC,MAXSTC,MXCSTC,IRCC)
        CALL MAXTST(1,'COMPZ0',MXNSAT,MAXSAT,MXCSAT,IRCS)
        CALL MAXTST(1,'COMPZ0',MXNVAR,MAXVAR,MXCVAR,IRCV)
        CALL MAXTST(1,'COMPZ0',MXNSTD,MAXSTD,MXCSTD,IRCT)
        IF(IRCC.NE.0.OR.IRCS.NE.0.OR.
     1     IRCV.NE.0.OR.IRCT.NE.0)CALL EXITRC(2)
      END IF
C
C IS THE TIME TACT OLD OR NEW ?
C ---------------------------
      DO 20 ITIM=1,NTIM
        IF(TACT.EQ.TIMINT(ITIM))GO TO 30
20    CONTINUE
      NTIM=NTIM+1
      ITIM=NTIM
      TIMINT(NTIM)=TACT
30    CONTINUE
C
C ARE THE SATELLITES IN EXTERNAL LIST ALSO IN INTERNAL LIST ?
C IF NOT : UPDATE INTERNAL LIST
C -----------------------------------------------------------
      DO 50 ISAT=1,NSAT
        DO 40 KSAT=1,NSATLC
          IF(SVNNUM(ISAT).EQ.SVNINT(KSAT))GO TO 50
40      CONTINUE
        NSATLC=NSATLC+1
        SVNINT(NSATLC)=SVNNUM(ISAT)
50    CONTINUE
C
C COMPUTE PARTIALS FOR ALL SATELLITES IN INTERNAL LIST
C ----------------------------------------------------
      DO 100 ISAT=1,NSATLC
        IF(Z0INT(1,1,ISAT,ITIM,IFIL).EQ.0.D0)THEN
          DO 70 IPAR=1,6
            CALL PRTDER(RPRFIL,SVNINT(ISAT),IPAR,1,0,TACT,1,ICRARC,
     1                  IORSYS,NVAR,NRAD,Z0INT(1,IPAR,ISAT,ITIM,IFIL),
     2                  ELESAT,RPRPAR,ANLTYP,IRC)
            IF(NVAR.LT.15)THEN
              DO 60 I=1,6
                Z0INT(I,IPAR,ISAT,ITIM,IFIL)=0.D0
60            CONTINUE
              GO TO 110
            ELSE IF(IRC.EQ.2)THEN
              DO 65 I=1,6
                DO 65 KPAR=1,6
                  Z0INT(I,KPAR,ISAT,ITIM,IFIL)=-9.99D15
65            CONTINUE
              GO TO 100
            END IF
70        CONTINUE
        END IF
100   CONTINUE
110   CONTINUE
C
C SATELLITE INDEX OF REQUIRED SATELLITE IN INTERNAL LIST
C ------------------------------------------------------
      CALL INLIST(SVN,NSATLC,SVNINT,ISAT)
C
C RETURN REQUESTED Z0-MATRIX
C --------------------------
      DO 120 I=1,6
        DO 120 K=1,6
          IF(Z0INT(I,K,ISAT,ITIM,IFIL).EQ.-9.99D15)THEN
            Z0(I,K)=0.D0
          ELSE
            Z0(I,K)=Z0INT(I,K,ISAT,ITIM,IFIL)
          END IF
120   CONTINUE
C
999   CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
