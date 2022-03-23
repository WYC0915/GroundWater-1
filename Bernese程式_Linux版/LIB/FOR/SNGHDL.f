      MODULE s_SNGHDL
      CONTAINS

C*
      SUBROUTINE SNGHDL(IORSYS,T0ARC ,TB12  ,T0    ,H     ,Q     ,
     1                  NVAR  ,FAC   ,LOCQ  ,INT   ,XVSAV ,ZCOSAV,
     2                  ORBMOD,NUMSAT,YCOE  ,ZCOE  )
CC
CC NAME       :  SNGHDL
CC
CC PURPOSE    :  HANDLE LIGHT/SHADOW CHANGES CORRECTLY
CC
CC PARAMETERS :
CC         IN :  IORSYS : REFERENCE SYSTEM INDEX               I*4
CC               T0ARC  : REFERENCE TIME FOR ARC               R*8
CC               TB12   : INTERVAL BOUNDARIES                  R*8(*)
CC               T0     : REFERENCE TIME FOR ARC               R*8
CC               H      : STEP SIZE                            R*8
CC               Q      : INTEGRATION ORDER                    I*4
CC               NVAR   : NUMBER OF VAR. EQNS.                 I*4
CC               FAC    : FAKULTAETEN (?)                      R*8(*)
CC               LOCQ   : DEFINITION OF ORBIT PARMS            I*4(6,*)
CC               INT    : CURRENT INTERVAL                     I*4
CC               XVSAV  : POSITION AND VELOCITY AT START OF    R*8(*,*)
CC                        INTERVAL
CC               ZCOSAV : INITIAL CONDITIONS FOR VAREQNS       R*8(*)
CC               ORBMOD : ORBIT MODEL ARRAY                    I*4(*)
CC               NUMSAT : SATELLITE NUMBER                     I*4
CC        OUT :  YCOE   : MODIFIED COEFFICIENTS FOR SATS       R*8(*,*)
CC               ZCOE   : MODIFIED COEFF FOR VAR EQNS          R*8(*,*)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G. BEUTLER
CC
CC VERSION    :  3.5
CC
CC CREATED    :  93/08/19
CC
CC CHANGES    :  25-JAN-96 : GB: NEW ORBIT MODEL IMPLEMENTED
CC               27-JUN-96 : TS: ADDED ORBIT MODEL ARRAY
CC               05-FEB-97 : MR: ADD *'S TO ERROR OUTPUT
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               18-OCT-06 : MP: PARAMETER TO CALL OF INTFST ADDED
CC               04-MAY-08 : RD: NUMSAT ADDED TO CALL OF SR INTFST,SNGDET
CC               28-OCT-08 : DT: USE MAXVAR FROM M_MAXDIM
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1993     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE m_maxdim, ONLY: maxVar
      USE s_intfst
      USE s_maxtst
      USE s_ypol
      USE s_exitrc
      USE s_sngdet
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IFIRST, INT   , IORSYS, IRC   , K     , K3    ,
     1          K4    , MAXDIV, MAXQ1 , MXCECL, MXCQ1 , MXCVAR,
     2          NINT  , NPOINT, NVAR  , NUMSAT
C
      REAL*8    DSMAX , ERRHLP, H     , HH    , T0    , T0ARC , TSEC  ,
     1          TT00
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MAXQ1=20,MAXDIV=10)
C
C GLOBAL DECLARATIONS :
C -------------------
      COMMON/MCMECL/MXCECL,MXNECL
      COMMON/MCMQ1/MXCQ1,MXNQ1
      CHARACTER*6 MXNECL,MXNQ1,MXNVAR
C
      REAL*8      TB12(*),FAC(*),YCOE(3,*),ZCOE(*),
     1            XVSAV(6),ZCOSAV(*)
C
      INTEGER*4   Q,LOCQ(6,*),ORBMOD(*)
C
C LOCAL DECLARATIONS
C ------------------
      REAL*8      XVSAT(6),YCOLOC(3,MAXQ1),ZCOLOC(3*MAXVAR*MAXQ1),
     1            ZCOSAT(2*3*MAXVAR),ZCONEW(2*3*MAXVAR),TBND(2,MAXDIV),
     2            XVSOLD(6)
C
      COMMON/MCMVAR/MXCVAR,MXNVAR
C
      DATA        IFIRST/1/
C
C CHECK MAXIMUM DIMENSION
C -----------------------
      IF(IFIRST.EQ.1)THEN
        IFIRST=0
        CALL MAXTST(1,'SNGHDL',MXNQ1,MAXQ1,MXCQ1,IRC)
        IF(NVAR.GT.MAXVAR)THEN
          WRITE(LFNERR,5)
5         FORMAT(//,' *** SR SNGHDL : LOCAL MAXVAR TOO SMALL',//)
          CALL EXITRC(2)
        END IF
        IF(IRC.NE.0)THEN
          WRITE(LFNERR,10)MAXQ1
10        FORMAT(//,' *** SR SNGHDL : MAXQ1 TOO SMALL:',I4,//)
          CALL EXITRC(2)
        END IF
        CALL MAXTST(0,'SNGHDL',MXNVAR,MAXVAR,MXCVAR,IRC)
        IF(IRC.NE.0)THEN
          WRITE(LFNERR,11)MAXVAR
11        FORMAT(/,' *** SR SNGHDL : MAXVAR TOO SMALL:',I4,/)
          CALL EXITRC(2)
        END IF
      END IF
C
C WAS THERE AN EARTH SHADOW CROSSING ?
C ----------------------------------
      NPOINT=Q-1
      DSMAX=0.6D0
      CALL SNGDET(IORSYS,NUMSAT,TB12(INT)+T0ARC,TB12(INT+1)+T0ARC,
     1            T0ARC,NPOINT,T0,H,Q,FAC,YCOE,MAXDIV,DSMAX,NINT,TBND)
      IF(NINT.EQ.1)GO TO 999
C
C COPY INITIAL POSITION AND VELOCITY INTO A LOCAL ARRAY
C -----------------------------------------------------
      DO 20 I=1,6
        XVSAT(I)=XVSAV(I)
20    CONTINUE
      DO 21 I=1,2*3*NVAR
        ZCOSAT(I)=ZCOSAV(I)
21    CONTINUE
C
C PERFORM INTEGRATION IN NINT STEPS
C ---------------------------------
      DO 200 I=1,NINT
C
C INTEGRATION
        CALL INTFST(0,0,TBND(1,I)-T0ARC,TBND(2,I)-T0ARC,XVSAT,
     1              Q,NVAR,LOCQ,ZCOSAT,ORBMOD,NUMSAT,TT00,HH,
     2              YCOLOC,ZCOLOC,ERRHLP)
C
C NEW INITIAL CONDITIONS
        TSEC=(TBND(2,I)-T0ARC-TT00)*86400
        IF (DABS(TSEC).LT.1.D-4) TSEC=0.D0
        CALL YPOL(1,Q,3,HH,FAC,TSEC,YCOLOC,XVSAT)
        CALL YPOL(1,Q,3*NVAR,HH,FAC,TSEC,ZCOLOC,ZCOSAT)
200   CONTINUE
C
C CORRECT ORIGINAL SET OF COEFFICIENTS
C ------------------------------------
C
C NEW INITIAL CONDITIONS
      TSEC=(TB12(INT+1)-TT00)*86400
      IF (DABS(TSEC).LT.1.D-4) TSEC=0.D0
      CALL YPOL(1,Q,3,HH,FAC,TSEC,YCOLOC,XVSAT)
      CALL YPOL(1,Q,3*NVAR,HH,FAC,TSEC,ZCOLOC,ZCONEW)
C
C OLD INITIAL CONDITIONS
      TSEC=(TB12(INT+1)-T0)*86400
      IF (DABS(TSEC).LT.1.D-4) TSEC=0.D0
      CALL YPOL(1,Q,3,H,FAC,TSEC,YCOE,XVSOLD)
      CALL YPOL(1,Q,3*NVAR,H,FAC,TSEC,ZCOE,ZCOSAT)
C
C MODIFY COEFFICIENTS YCOE
      DO 81 K=1,3
        YCOE(K,3)=YCOE(K,3)
     1                 -(XVSAT(3+K)-XVSOLD(3+K))*H
     2                 +3*(XVSAT(K)-XVSOLD(K))
        YCOE(K,4)=YCOE(K,4)
     1                 +(XVSAT(3+K)-XVSOLD(3+K))*H
     2                 -2*(XVSAT(K)-XVSOLD(K))
81    CONTINUE
C
      K3=3*NVAR*2
      K4=3*NVAR*3
      DO 82 K=1,3*NVAR
        ZCOE(K+K3)     =ZCOE(K+K3)
     1                  -(ZCONEW(3*NVAR+K)-ZCOSAT(3*NVAR+K))*H
     2                  +3*(ZCONEW(K)-ZCOSAT(K))
        ZCOE(K+K4)     =ZCOE(K+K4)
     1                  +(ZCONEW(3*NVAR+K)-ZCOSAT(3*NVAR+K))*H
     2                  -2*(ZCONEW(K)-ZCOSAT(K))
82    CONTINUE
999   RETURN
      END SUBROUTINE

      END MODULE
