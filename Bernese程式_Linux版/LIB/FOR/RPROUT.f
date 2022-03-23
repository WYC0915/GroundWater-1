      MODULE s_RPROUT
      CONTAINS
C*
      SUBROUTINE RPROUT(NARC  ,IARC  ,QVAR  ,DTPIV ,TA    ,TB    ,
     1                  NSAT  ,NAVNUM,SOURCE,IORSYS,NVAR  ,LOCQ  ,
     2                  ELE   ,RPRESS,SCALPA,TLEFT ,TRIGHT,Q     ,
     3                  T0    ,HINT  ,ZCOE  ,FAC   ,ZSAT  ,ORBMOD,
     4                  MODFLG,RPRFRM,NSTC  ,FRCTYP,NSTCEP,INTSTC,
     5                  TIMSTC,PARSTC)
CC
CC NAME       :  RPROUT
CC
CC PURPOSE    :  COMPILE AND WRITE INFORMATION CONCERNING VARIATIONAL
CC               EQUATIONS
CC
CC PARAMETERS :
CC        IN  :  NARC    : NUMBER OF ARCS                            I*4
CC               IARC    : CURRENT ARC NUMBER                        I*4
CC               QVAR    : DEGREE FOR POLYNOMIALS                    I*4
CC               DTPIV   : LENGTH OF PARTIAL INTERVAL (HOURS)        R*8
CC               TA, TB  : INTERVAL BOUNARIES (ENTIRE INTERVAL)      R*8
CC               NSAT    : NUMBER OF SATELLITES                      I*4
CC               NAVNUM  : SATELLITE NUMBERS                         I*4
CC               SOURCE  : CHARACTERIZATION OF ORBIT FILE           CH*1
CC               IORSYS  : ORBIT SYSTEM                              I*4
CC               NVAR    : NUMBER OF VARIATIONAL EQNS                I*4
CC               LOCQ    : PARAMETER DESCRIPTION                     I*4
CC               ELE     : OSCULATING ELEMENTS AT INITIAL EPOCH      R*8
CC               RPRESS  : RADIATION PRESSURE PARAMETERS             R*8
CC               SCALPA  : SCALING FACTORS FOR PARAMETERS            R*8
CC               TLEFT   : LEFT SI BOUNDARY FROM CURRENT NUM. INT.   R*8
CC               TRIGHT  : RIGHT SI BOUNDARY FROM CURRENT NUM. INT.  R*8
CC               Q       : INTEGRATION ORDER                         I*4
CC               T0      : ORIGIN OF DEVELOPPMENT (INTEGRATION)      R*8
CC               HINT    : INTERVAL LENGTH (INTEGRATION)             R*8
CC               ZCOE    : COEFFICIENT MATRIX FROM NUM. INT.         R*8
CC               FAC     : FACTORIALS                                R*8
CC               ZSAT    : AUX. ARRAY                                R*8
CC               ORBMOD : ORBIT MODEL ARRAY                          I*4(*)
CC               MODFLG  : MODEL FLAG: ' ','A','B','C','D','E','F'  CH*1
CC               RPRFRM  : RPR FORMAT IDENTIFICATION                 I*4
CC               NSTC    : NUMBER OF STOCH. EPOCHS                   I*4
CC               FRCTYP  : PARAMETER TYPES                           I*4
CC               NSTCEP  : NUMBER OF PARAMETERS PER EPOCH            I*4
CC               INTSTC  : INTERVAL NUMBERS OF STOCHASTIC            I*4
CC                         PERTURBATIONS
CC               TIMSTC  : EPOCHS WITH STOCHASTIC PERTURBAT.         R*8
CC               PARSTC  : PARAMETER VALUES                          R*8
CC
CC
CC REMARKS    :
CC
CC AUTHOR     :  G.BEUTLER
CC
CC CREATED    :  96/01/01
CC
CC CHANGES    :  02-OCT-96 : TS: NEW ORBIT MODEL FLAGS
CC               02-MAY-97 : TS: MAXSAT IN INCLUDE FILE
CC               13-MAY-98 : MR: REPLACE "DFLOAT" BY "DBLE"
CC               14-AUG-01 : DS: MAXINT 400->510
CC               21-FEB-02 : DS: MODELS "D","E","F"
CC               21-FEB-02 : DS: USE MODFLG INSTEAD OF (DATA ORBMFL)
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               07-MAR-03 : HU: MAXINT MOVED TO M_MAXDIM
CC               12-DEC-03 : AJ: MAXVAR:15->21
CC               04-APR-05 : AJ: WRITE STOCH. PARAMETERS TO RPR-FILE
CC               16-JUN-05 : MM: UNUSED COMCONST.INC REMOVED
CC               21-JUN-05 : MM: COMLFNUM.INC REMOVED, M_BERN ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-OCT-08 : DT: USE MAXVAR FROM M_MAXDIM
CC               02-MAY-11 : SL: M_BERN WITH ONLY, MAXINT FROM P_ORBGEN
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1996     UNIVERSITY OF BERN
CC               SWITZERLAND
C*
      USE m_bern,   ONLY: lfnErr, lfnRpr
      USE m_maxdim, ONLY: MAXSAT, maxVar
      USE p_orbgen, ONLY: maxInt
      USE s_opnfil
      USE s_rdanlt
      USE s_opnerr
      USE s_maxtst
      USE s_dminv
      USE s_ypol
      USE s_exitrc
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IARC  , IBEG  , ICOL  , ICOL0 , ICURR , IDEG  ,
     1          IEND  , IFIRST, IK    , IK1   , IK2   , IL    , IND   ,
     2          INTACT, IORSYS, IOSTAT, IPT   , IRC   , IRCRPR, ISAT  ,
     3          IVAR  , K     , L     , LK    , MAXQ1 , MXCQ1 ,
     4          MXCSAT, MXCVAR, NARC  , NINT  , NLINES, NPOINT, NSAT  ,
     5          NVAR  , ISTC  , IFRC
C
      REAL*8    ARG   , DET   , DTPIV , H     , HINT  , HSEC  , T0    ,
     1          TA    , TB    , TEST  , TLEFT , TREL  , TRIGHT, ZERO  ,
     2          ZERON
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      PARAMETER (MAXQ1=20)
C
C
      CHARACTER*32 FILNAM
      CHARACTER*6  MXNVAR,MXNQ1,MXNSAT
      CHARACTER*1  SOURCE(*)
      CHARACTER*1  MODFLG
C
      REAL*8       RPRESS(MXCVAR,*),FAC(*),ELE(7,*),SCALPA(*)
      REAL*8       ZCOE(3*MXCVAR*MXCQ1,*),ZSAT(*)
      REAL*8       TIMSTC(MAXINT,*),PARSTC(3,MAXINT,*)
C
      INTEGER*4    Q,QVAR,ARCOLD,NAVNUM(*),LOCQ(6,*),ORBMOD(*)
      INTEGER*4    FRCTYP(3,MAXINT,*),NSTC(*),NSTCEP(MAXINT,*)
      INTEGER*4    INTSTC(MAXINT,*),RPRFRM
C
C LOCAL DECLARATIONS
      CHARACTER*8  ANLTYP
C      CHARACTER*1  ORBMFL(4)
C
      INTEGER*4    L1(MAXQ1),L2(MAXQ1)
C
      REAL*8       TBOUND(2,MAXINT),TVAL(MAXQ1-2)
      REAL*8       MAT(MAXQ1**2),B(MAXQ1*3*MAXVAR*MAXSAT)
      REAL*8       ZCOEN(MAXQ1*3*MAXVAR*MAXSAT)
C
      CHARACTER(LEN=6), PARAMETER :: srName= 'RPROUT'
C
      COMMON/MCMQ1/MXCQ1,MXNQ1
      COMMON/MCMVAR/MXCVAR,MXNVAR
      COMMON/MCMSAT/MXCSAT,MXNSAT
C
      DATA IFIRST/1/,ARCOLD/0/
C      DATA ORBMFL/'A','B','C','D'/
C
C INITIALIZATION
C --------------
      IF(IFIRST.EQ.1)THEN
        IFIRST=0
C
C MAXIMUM DIMENSIONS
        CALL MAXTST(0,srName,MXNQ1,MAXQ1,MXCQ1,IRC)
        IF(IRC.NE.0)THEN
          WRITE(LFNERR,1)MAXQ1
1         FORMAT(//,' SR RPROUT : MAXQ1 NOT CORRECT: ',I4,//)
          CALL EXITRC(2)
        END IF
        CALL MAXTST(0,srName,MXNVAR,MAXVAR,MXCVAR,IRC)
        IF(IRC.NE.0)THEN
          WRITE(LFNERR,2)MAXVAR
2         FORMAT(/,' *** SR RPROUT : MAXVAR TOO SMALL:',I4,/)
          CALL EXITRC(2)
        END IF
        CALL MAXTST(0,srName,MXNSAT,MAXSAT,MXCSAT,IRC)
        IF(IRC.NE.0)THEN
          WRITE(LFNERR,3)MAXSAT
3         FORMAT(/,' *** SR RPROUT : MAXSAT TOO SMALL:',I4,/)
          CALL EXITRC(2)
        END IF
C
C SECURITY: ONLY STORE PORAMETERS, IF ALL 15 PARAMETERS OF NEW MODEL
C ARE AVAILABLE (NOT ALL MAY HAVE BEEN ESTIMATED)
        IF(NVAR.NE.15.AND.NVAR.NE.18.AND.NVAR.NE.21)THEN
          WRITE(LFNERR,6)
6         FORMAT(//,' ** SR RPROUT: NOT ALL 15 PARAMETERS OF NEW',
     1           ' MODEL WERE SET UP. CHANGE INPUT PARAMETER!',//)
          CALL EXITRC(2)
        END IF
C
C OPEN RADIATION-PRESSURE COEFF. OUTPUT FILE
        CALL GTFLNA(0,'RPROUT ',FILNAM,IRCRPR)
        CALL OPNFIL(LFNRPR,FILNAM,'UNKNOWN','UNFORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNRPR,IOSTAT,FILNAM,'DEFW95')
C
C DEFINE SATELLITE FILE-TYPE
        CALL RDANLT(ANLTYP)
C
C WRITE FORMAT DEFINITION
        IF(RPRFRM.EQ.1) WRITE(LFNRPR) '#P '
C
C DEFINE ORBIT MODEL TYPE (8. CHARACTER OF ANLTYP)
C ------------------------------------------------
        ANLTYP(8:8)=MODFLG
C
C WRITE NUMBER OF ARCS TO OUTPUT FILE
        WRITE(LFNRPR) NARC
C
C PARTITION OF INTERVAL (TA,TB)
        NINT=0
        DO 10 I=1,100000
          NINT=NINT+1
          IF(NINT.GT.MAXINT)THEN
            WRITE(LFNERR,4)MAXINT
4           FORMAT(//,' ** SR RPROUT : MAX NUMBER OF ',
     1                     'INTERVALS EXCEEDED, MAXINT=',I5)
            CALL EXITRC(2)
          END IF
          TBOUND(1,NINT)=TA+(I-1)*DTPIV/24.D0
          TBOUND(2,NINT)=TA+I*DTPIV/24.D0
          TEST=DABS(TBOUND(2,NINT)-TB)
          IF(TEST.LT.1.D-3.OR.TBOUND(2,NINT).GT.TB)THEN
            TBOUND(2,NINT)=TB
          END IF
          IF(TBOUND(2,NINT).EQ.TB)GO TO 20
10      CONTINUE
20      CONTINUE
C
C MAXIMUM VALUE FOR Q
        IF(QVAR+1.GT.MAXQ1)THEN
          WRITE(LFNERR,5)MAXQ1
5         FORMAT(//,' ** SR RPROUT : MAX POLYNOMIAL ',
     1              'DEGREE EXCEEDED, MAXQ1 = ',I3)
          CALL EXITRC(2)
        END IF
C
C DEFINITION OF MATRIX MAT
C ------------------------
        DO 29 I=1,QVAR+1
          IF(I.LE.2)THEN
            ARG=0.D0
          ELSE IF(I.GT.2.AND.I.LE.QVAR)THEN
            ARG=DBLE(I-2)/(QVAR-2)
          ELSE
            ARG=1.D0
          END IF
          IF(I.NE.2.AND.I.NE.QVAR+1)THEN
            MAT(I)=1.D0
            IBEG=2
          ELSE
            MAT(I)=0.D0
            MAT(I+QVAR+1)=1.D0
            IBEG=3
          END IF
          DO 28 K=IBEG,QVAR+1
            IK=I+(K-1)*(QVAR+1)
            IF(I.NE.2.AND.I.NE.QVAR+1)THEN
              MAT(IK)=ARG**(K-1)
            ELSE
              MAT(IK)=(K-1)*ARG**(K-2)
            END IF
28        CONTINUE
29      CONTINUE
C
C INVERT MATRIX
        CALL DMINV(MAT,QVAR+1,DET,L1,L2)
        NLINES=3*NVAR*NSAT
      END IF
C
C NEW ARC
C -------
      IF(IARC.NE.ARCOLD)THEN
        ARCOLD=IARC
C
C WRITE CONTROL INFORMATION FOR ARC IARC ON RADIATION PRESSURE FILE
        WRITE(LFNRPR) NSAT,NINT,QVAR,(NAVNUM(I),I=1,NSAT),
     1                (SOURCE(I),I=1,10)
        IF (IORSYS.EQ.1) THEN
          ZERO=0.D0
        ELSE
          ZERO=2.D0
        ENDIF
        ZERON=ZERO+10
C
        WRITE(LFNRPR) TA,TB,ZERON
        WRITE(LFNRPR) NVAR,ANLTYP
        DO 30 I=1,NINT
          WRITE(LFNRPR) TBOUND(1,I)
30      CONTINUE
        WRITE(LFNRPR) TBOUND(2,NINT)
C
        DO 40 ISAT=1,NSAT
          DO 35 K=1,NVAR
            IF(K.LE.6)THEN
              WRITE(LFNRPR) ELE(K,ISAT),SCALPA(K),(LOCQ(L,K),L=1,6)
            ELSE
              WRITE(LFNRPR) RPRESS(K-6,ISAT),SCALPA(K),
     1                      (LOCQ(L,K),L=1,6)
            END IF
35        CONTINUE
40      CONTINUE
C
C WRITE STOCH. INFORMATION FOR ARC IARC ON RADIATION PRESSURE FILE
        IF(RPRFRM.EQ.1) THEN
          DO 47 ISAT=1,NSAT
            WRITE(LFNRPR) NSTC(ISAT)
            DO 46 ISTC=1,NSTC(ISAT)
              DO 45 IFRC=1,NSTCEP(ISTC,ISAT)
                WRITE(LFNRPR) FRCTYP(IFRC,ISTC,ISAT),NSTCEP(ISTC,ISAT),
     1                        INTSTC(ISTC,ISAT),TIMSTC(ISTC,ISAT),
     2                        PARSTC(IFRC,ISTC,ISAT)
45            CONTINUE
46          CONTINUE
47        CONTINUE
        END IF
C
C CURRENT SUBINTERVAL, CURRENT NUMBER OF DATA POINTS
        INTACT=1
        NPOINT=QVAR-1
        ICURR=1
        H=(TBOUND(2,INTACT)-TBOUND(1,INTACT))
        HSEC=H*86400.D0
        DO 50 I=1,NPOINT
          TVAL(I)=TBOUND(1,INTACT)+(I-1)*H/(NPOINT-1)
50      CONTINUE
      END IF
C
C LOOP OVER ALL POINTS TVAL OF CURRENT SUBINTERVAL:
C SET UP CONDITION EQUATIONS
C ------------------------------------------------
      DO 100 IPT=ICURR,NPOINT
        IF(TVAL(IPT).GE.TLEFT-1.D-4.AND.
     1     TVAL(IPT).LT.TRIGHT+1.D-4)THEN
          TREL=(TVAL(IPT)-T0)*86400.D0
C
C COLUMN-NUMBER (PART 1)
          IF(IPT.EQ.1)THEN
            ICOL0=1
          ELSE
            ICOL0=IPT+1
          END IF
C
C NUMBER OF DERIVATIVES
          IEND=1
          IF(IPT.EQ.1.OR.IPT.EQ.NPOINT)IEND=2
C
C LOOP OVER ALL SATELLITES
          DO 60 ISAT=1,NSAT
            CALL YPOL(1,Q,3*NVAR,HINT,FAC,TREL,ZCOE(1,ISAT),ZSAT)
            DO 55 K=1,3
              DO 55 IVAR=1,NVAR
                DO 55 L=1,IEND
                  ICOL=ICOL0+(L-1)
                  IK1=K+3*(IVAR-1)+3*NVAR*(L-1)
                  IK2=(ICOL-1)*NLINES+K+3*(IVAR-1)+3*NVAR*(ISAT-1)
                  B(IK2)=ZSAT(IK1)*HSEC**(L-1)
55          CONTINUE
60        CONTINUE
        ELSE
          GO TO 110
        END IF
100   CONTINUE
110   ICURR=IPT
C
C COEFFICIENT MATRIX, STORE INFORMATION:
C -------------------------------------
      IF(ICURR.EQ.NPOINT+1)THEN
        ICURR=1
C
C MATRIX MULTIPLICATION (COEFFICIENT MATRIX)
        DO 115 I=1,QVAR+1
        DO 115 K=1,NLINES
          IK=K+(I-1)*NLINES
          ZCOEN(IK)=0.D0
          DO 115 L=QVAR+1,1,-1
            IL=I+(L-1)*(QVAR+1)
            LK=K+(L-1)*NLINES
            ZCOEN(IK)=ZCOEN(IK)+MAT(IL)*B(LK)
115     CONTINUE
C
C STORE INFORMATION
        WRITE(LFNRPR) TBOUND(1,INTACT),HSEC
        DO 120 ISAT=1,NSAT
          DO 120 IVAR=1,NVAR
            DO 120 IDEG=1,QVAR+1
              IND=NLINES*(IDEG-1)+3*NVAR*(ISAT-1)+3*(IVAR-1)
              WRITE(LFNRPR) (ZCOEN(IND+K),K=1,3)
120     CONTINUE
        INTACT=INTACT+1
        NPOINT=QVAR-1
C
C INITIALIZE NEW SUBINTERVAL
        IF(INTACT.LE.NINT)THEN
          H=(TBOUND(2,INTACT)-TBOUND(1,INTACT))
          HSEC=H*86400.D0
          DO 130 I=1,NPOINT
            TVAL(I)=TBOUND(1,INTACT)+(I-1)*H/(NPOINT-1)
130       CONTINUE
        END IF
      END IF
      RETURN
      END SUBROUTINE

      END MODULE
