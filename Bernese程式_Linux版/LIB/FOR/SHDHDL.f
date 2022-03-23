      MODULE s_SHDHDL
      CONTAINS

C*
      SUBROUTINE SHDHDL(ITER  ,NITER ,IORSYS,T0ARC ,TB12  ,T0    ,
     1                  H     ,Q     ,NVAR  ,FAC   ,LOCQ  ,ISHUSE,
     2                  MAXCHG,INT   ,NUMSVN,ORBMOD,NSACHG,SATCHG,
     3                  NUMCHG,CHGTIM,IPRCHS,IPRCHG,YCOE  ,ZCOE  )
CC
CC NAME       :  SHDHDL
CC
CC PURPOSE    :  HANDLE LIGHT/SHADOW CHANGES CORRECTLY
CC
CC PARAMETERS :
CC         IN :  ITER   : CURRENT ITERATION STEP               I*4
CC               NITER  : TOTAL NUMER OF ITERATIONS            I*4
CC               IORSYS : REFERENCE SYSTEM INDEX               I*4
CC               T0ARC  : REFERENCE TIME FOR ARC               R*8
CC               TB12   : INTERVAL BOUNDARIES                  R*8(*)
CC               T0     : REFERENCE TIME FOR ARC               R*8
CC               H      : STEP SIZE                            R*8
CC               Q      : INTEGRATION ORDER                    I*4
CC               NVAR   : NUMBER OF VARIATIONAL EQNS           I*4
CC               FAC    : FAKULTAETEN (?)                      R*8(*)
CC               LOCQ   : DEFINITION OF ORBIT PARMS            I*4(6,*)
CC               ISHUSE : CURRENT STATUS OF SATELLITE          I*4
CC               MAXCHG : MAXIMUM NUMER OF ECL. SATS ALLOWED   I*4
CC               INT    : CURRENT INTERVAL                     I*4
CC               NUMSVN : SATELLITE NUMBER                     I*4
CC               ORBMOD : ORBIT MODEL ARRAY                     I*4(*)
CC        OUT :  NSACHG : ACTUAL NUMBER OF ECL SATS            I*4
CC               SATCHG : SATELLITE NUMBERS                    I*4(*)
CC               NUMCHG : NUMBER OF CHANGES FOR EACH SAT       I*4(*)
CC               CHGTIM : CHANGING TIMES                    R*8(*,*,*)
CC               IPRCHS : PRINT INDEX FOR ECL SATS             I*4
CC               IPRCHG : PRINT INDEX FOR ECL SATS             I*4(*)
CC               YCOE   : MODIFIED COEFFICIENTS FOR SATS    R*8(*,*,*)
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
CC CHANGES    :  22-SEP-93 : ??: CHANGE BOOK-KEEPING OF ECLIPSE TIMES
CC               29-SEP-93 : ??: ADD PARAMETER "IPRCHS"
CC               10-AUG-94 : MR: CALL EXITRC
CC               23-OCT-94 : GB: NPOINT=40 ==> NPOINT=100.
CC               25-JAN-96 : GB: NEW ORBIT MODEL IMPLEMENTED
CC               27-JUN-96 : TS: ADDED ORBIT MODEL ARRAY
CC               13-JAN-02 : HU: LAYOUT MODIFIED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               18-OCT-06 : MP: PARAMETER TO CALL OF INTFST ADDED
CC               04-MAY-08 : RD: NUMSVN ADDED TO CALL OF SR INTFST
CC               28-OCT-08 : DT: USE maxVar FROM M_MAXDIM
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
      USE s_shdchg
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IFIRST, INDCH2, INDCHG, INT   , IORSYS, IPRCHS, IRC   ,
     1          ISACHG, ISHUSE, ITER  , K     , K3    , K4    , MAXCHG,
     2          MAXQ1 , MXCECL, MXCQ1 , MXCVAR, NITER , NPOINT,
     3          NSACHG, NUMSVN, NVAR
C
      REAL*8    ERRHLP, H     , HH    , T0    , T0ARC , TCHAN2, TCHANG,
     1          TSEC  , TT00
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MAXQ1=20)
C
C GLOBAL DECLARATIONS :
C -------------------
      COMMON/MCMECL/MXCECL,MXNECL
      COMMON/MCMQ1/MXCQ1,MXNQ1
C
      CHARACTER*6 MXNECL,MXNQ1,MXNVAR
C
      REAL*8      TB12(*),FAC(*),CHGTIM(2,MXCECL,*),
     1            YCOE(3,*),ZCOE(*)
C
      INTEGER*4   Q,LOCQ(6,*),SATCHG(*),NUMCHG(*),IPRCHG(*),ORBMOD(*)
C
C LOCAL DECLARATIONS
C ------------------
      REAL*8      XSAT(3*2*MAXVAR),XVSOLD(6),YCOLOC(3,MAXQ1),
     1            ZCOLOC(3*MAXVAR*MAXQ1),ZCONEW(3*2*MAXVAR),
     2            ZCOOLD(3*2*MAXVAR)
C
      COMMON/MCMVAR/MXCVAR,MXNVAR
C
      DATA        IFIRST/1/
C
C CHECK MAXIMUM DIMENSION
C -----------------------
      IF(IFIRST.EQ.1)THEN
        IFIRST=0
        CALL MAXTST(1,'SHDHDL',MXNQ1,MAXQ1,MXCQ1,IRC)
        IF(IRC.NE.0)THEN
          WRITE(LFNERR,10)MAXQ1
10        FORMAT(/,' *** SR SHDHDL : MAXQ1 TOO SMALL:',I4,/)
          CALL EXITRC(2)
        END IF
        CALL MAXTST(0,'SHDHDL',MXNVAR,MAXVAR,MXCVAR,IRC)
        IF(IRC.NE.0)THEN
          WRITE(LFNERR,11)MAXVAR
11        FORMAT(/,' *** SR SHDHDL : MAXVAR TOO SMALL:',I4,/)
          CALL EXITRC(2)
        END IF
      END IF

C
C WAS THERE AN EARTH SHADOW CROSSING ?
C ----------------------------------
      NPOINT=100
      CALL SHDCHG(IORSYS,TB12(INT)+T0ARC,TB12(INT+1)+T0ARC,
     1            T0ARC,NPOINT,T0,H,Q,FAC,YCOE,
     2            ISHUSE,INDCHG,TCHANG)
C
C SPLIT UP INTEGRATION IF SHADOW TRANSIT TOOK PLACE
C -------------------------------------------------
      IF (INDCHG.NE.ISHUSE) THEN
        IF (ITER.EQ.NITER) THEN
          DO 76 ISACHG=1,NSACHG
            IF (SATCHG(ISACHG).EQ.NUMSVN) GOTO 78
76        CONTINUE
          NSACHG=NSACHG+1
          IF (NSACHG.GT.MXCECL) THEN
            IPRCHS=0
            GOTO 79
          ENDIF
          SATCHG(NSACHG)=NUMSVN
          IF (ISHUSE.EQ.1) THEN
            NUMCHG(NSACHG)=1
          ELSE
            NUMCHG(NSACHG)=0
          ENDIF
          CHGTIM(1,NSACHG,1)=0.D0
          IPRCHG(NSACHG)=1
          ISACHG=NSACHG
78        IF (ISHUSE.EQ.0) THEN
            NUMCHG(ISACHG)=NUMCHG(ISACHG)+1
            IF (NUMCHG(ISACHG).GT.MAXCHG) THEN
              IPRCHG(ISACHG)=0
            ELSE
              CHGTIM(2,ISACHG,NUMCHG(ISACHG))=0.D0
            ENDIF
          ENDIF
          CHGTIM(ISHUSE+1,ISACHG,NUMCHG(ISACHG))=TCHANG
        ENDIF
C
C OLD POSITION AND VELOCITY AT TIME TCHANG
C ----------------------------------------
79      TSEC=(TCHANG-T0ARC-T0)*86400
        IF (DABS(TSEC).LT.1.D-4) TSEC=0.D0
        CALL YPOL(1,Q,3,H,FAC,TSEC,YCOE,XVSOLD)
        CALL YPOL(1,Q,3*NVAR,H,FAC,TSEC,ZCOE,ZCOOLD)
        ISHUSE=INDCHG
        CALL INTFST(0,0,TCHANG-T0ARC,TB12(INT+1),XVSOLD,
     1              Q,NVAR,LOCQ,ZCOOLD,ORBMOD,NUMSVN,TT00,HH,
     2              YCOLOC,ZCOLOC,ERRHLP)
C
        CALL SHDCHG(IORSYS,TCHANG,TB12(INT+1)+T0ARC,T0ARC,
     1              NPOINT,TT00,HH,Q,FAC,YCOLOC,ISHUSE,
     2              INDCH2,TCHAN2)
C
        IF (INDCH2.NE.ISHUSE) THEN
          IF (ITER.EQ.NITER) THEN
            DO 86 ISACHG=1,NSACHG
              IF (SATCHG(ISACHG).EQ.NUMSVN) GOTO 88
86          CONTINUE
            NSACHG=NSACHG+1
            IF (NSACHG.GT.MXCECL) THEN
              IPRCHS=0
              GOTO 89
            ENDIF
            SATCHG(NSACHG)=NUMSVN
            IF (ISHUSE.EQ.1) THEN
              NUMCHG(NSACHG)=1
            ELSE
              NUMCHG(NSACHG)=0
            ENDIF
            CHGTIM(1,NSACHG,1)=0.D0
            IPRCHG(NSACHG)=1
            ISACHG=NSACHG
88          IF (ISHUSE.EQ.0) THEN
              NUMCHG(ISACHG)=NUMCHG(ISACHG)+1
              IF (NUMCHG(ISACHG).GT.MAXCHG) THEN
                IPRCHG(ISACHG)=0
              ELSE
                CHGTIM(2,ISACHG,NUMCHG(ISACHG))=0.D0
              ENDIF
            ENDIF
            CHGTIM(ISHUSE+1,ISACHG,NUMCHG(ISACHG))=TCHAN2
          ENDIF
89        TSEC=(TCHAN2-T0ARC-TT00)*86400
          IF (DABS(TSEC).LT.1.D-4) TSEC=0.D0
          CALL YPOL(1,Q,3,HH,FAC,TSEC,YCOLOC,XVSOLD)
          CALL YPOL(1,Q,3*NVAR,HH,FAC,TSEC,ZCOLOC,ZCOOLD)
          ISHUSE=INDCH2
          CALL INTFST(0,0,TCHAN2-T0ARC,TB12(INT+1),XVSOLD,
     1                Q,NVAR,LOCQ,ZCOOLD,ORBMOD,NUMSVN,TT00,HH,
     2                YCOLOC,ZCOLOC,ERRHLP)
        END IF
C
C NEW INITIAL CONDITIONS
        TSEC=(TB12(INT+1)-TT00)*86400
        IF (DABS(TSEC).LT.1.D-4) TSEC=0.D0
        CALL YPOL(1,Q,3,HH,FAC,TSEC,YCOLOC,XSAT)
        CALL YPOL(1,Q,3*NVAR,HH,FAC,TSEC,ZCOLOC,ZCONEW)
C
C OLD INITIAL CONDITIONS
        TSEC=(TB12(INT+1)-T0)*86400
        IF (DABS(TSEC).LT.1.D-4) TSEC=0.D0
        CALL YPOL(1,Q,3,H,FAC,TSEC,YCOE,XVSOLD)
        CALL YPOL(1,Q,3*NVAR,H,FAC,TSEC,ZCOE,ZCOOLD)
C
C MODIFY COEFFICIENTS YCOE
        DO 81 K=1,3
          YCOE(K,3)=YCOE(K,3)
     1                -(XSAT(3+K)-XVSOLD(3+K))*H
     2                +3*(XSAT(K)-XVSOLD(K))
          YCOE(K,4)=YCOE(K,4)
     1                +(XSAT(3+K)-XVSOLD(3+K))*H
     2                -2*(XSAT(K)-XVSOLD(K))
81      CONTINUE
        K3=3*NVAR*2
        K4=3*NVAR*3
        DO 82 K=1,3*NVAR
          ZCOE(K+K3)=ZCOE(K+K3)
     1                -(ZCONEW(3*NVAR+K)-ZCOOLD(3*NVAR+K))*H
     2                +3*(ZCONEW(K)-ZCOOLD(K))
          ZCOE(K+K4)=ZCOE(K+K4)
     1                +(ZCONEW(3*NVAR+K)-ZCOOLD(3*NVAR+K))*H
     2                -2*(ZCONEW(K)-ZCOOLD(K))
82      CONTINUE
      END IF
      RETURN
      END SUBROUTINE

      END MODULE
