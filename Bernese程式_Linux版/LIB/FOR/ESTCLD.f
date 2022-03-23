      MODULE s_ESTCLD
      CONTAINS

C*
      SUBROUTINE ESTCLD(IEP,INIPHA,NFTOT,NSATEP,NFILEP,FILEP,SVNNUM,
     1                  SVNLST,SVNEP,NSFLEP,SAFLEP,RESMXP,RMSMXP,
     2                  MAXITP,OMCMAX,MINSTA,PRNGTL,MRKOBS,ZENPHA,
     3                  ZENMAX,APRSIP,IRFSAP,NSATPH,NFILPH,
     4                  SVNPHA,FILPHA,NSFLPH,SAFLPH,NOBSPS,SATCLK,
     5                  SITCLK,RMSOBS,RMSSAT,RMSSIT,RESIDU,NDEL,
     6                  PROTYP,PROSAT,IRETRN)
CC
CC NAME       :  ESTCLD
CC
CC PURPOSE    :  DETERMINE SATELLITE AND STATION CLOCK CORRECTIONS
CC               RELATIVE TO PREVIOUS EPOCH USING PHASE OBSERVATIONS
CC               IT IS ASSUMED THAT CODE-CLOCK CORRECTIONS WERE ALREADY
CC               APPLIED
CC
CC PARAMETERS :
CC        IN  :  IEP    . EPOCH NUMBER                          I*4
CC    IN/OUT  :  INIPHA : =1: INITIALIZE PROCESSING             I*4
CC        IN  :  NFTOT  : TOTAL NUMBER OF FILES                 I*4
CC               NSATEP : NUMBER OF SATELLITES IN EPOCH         I*4
CC               NFILEP : NUMBER OF FILES IN EPOCH              I*4
CC               FILEP  : FILE NUMBERS IN EPOCH                 I*4
CC               SVNEP  : SATELLITE NUMBERS                     I*4
CC               NSFLEP : NUMBER OF SATELLITES IN FILE          I*4
CC               SAFLEP : SATELLITE NUMBERS IN FILE             I*4
CC               RESMXP : MAXIMUM ALLOWED RESIDUAL              R*8
CC               RMSMXP : MAXIMUM ALLOWED RMS OF OBSERVATION    R*8
CC               MAXITP : MAXIMUM NUMBER OF ITERATION STEPS     I*4
CC               OMCMAX : MAXIMUM ALLOWED O-C                   R*8
CC               MINSTA : MINIMUM NUMBER OF OBSERVATIONS PER    I*4
CC                        PER SATELLITE
CC               PRNGTL : P-RANGE CORRECTED FOR GEOMETRY        R*8
CC               MRKOBS : FLAGS FOR EACH OBSERVATION            CH*1
CC               ZENPHA : ZENITH DISTANCE OF OBSERVATION        R*8
CC               ZENMAX : MAXIMUM ALLOWED ZENITH DISTANCE       R*8
CC               APRSIP : APRIORI SIGMA OF PHASE OBSERVATIONS   R*8
CC        OUT :  IRFSAP : SVN NR OF REFERENCE SATELLITE         I*4
CC               NSATPH : NUMBER OF SATELLITES FOR WHICH CLOCK  I*4
CC                        DIFFERENCES WERE ESTIMATED
CC               NFILPH : NUMBER OF SITES FOR WHICH CLOCK       I*4
CC                        DIFFERENCES WERE ESTIMATED
CC               SVNPHA : SATELLITE NUMBERS                     I*4
CC               FILPHA : was ist das ????                      I*4
CC               NSFLPH : NUMBER OF SATELLITES PER FILE         I*4
CC               SAFLPH : SATELLITE NUMBERS IN EACH FILE        I*4
CC               NOBSPS : OBSERVATIONS PER SATELLITE            I*4
CC               SATCLK : SATELLITE CLOCK ERRORS                R*8
CC               SITCLK : SITE CLOCK ERROR                      R*8
CC               RMSOBS : RMS ERROR OF OBSERVATION              R*8
CC               RMSSAT : RMS ERROR OF SATELLITE CLOCK ERRORS   R*8
CC               RMSSIT : RMS ERROR OF SITES                    R*8
CC               RESIDU : RESIDUALS                             R*8
CC               NDEL   : NUMBER OF DELETIONS                   R*8
CC               PROTYP : ARRAY WITH PROBLEM TYPE               I*4
CC               IRETRN : RETURN CODE                           I*4
CC                        =0: EVERYTHING OK
CC                        =1: DETERMINATION FAILED
CC
CC REMARKS    :  THE REFERENCE CLOCK IS SELECTED AUTOMATICALLY
CC               (MAY BE TRANSFORMED TO ANY OTHER CLOCK OUTSIDE).
CC               THE CORRESPONDING CLOCK DIFFERENCE IS SET TO ZERO.
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  4.1  (MAY 99)
CC
CC CREATED    :  99/05/15
CC
CC CHANGES    :  04-MAY-01 : HB: Take RMSSAT and RMSSIT in m not in seconds
CC               15-JUL-01 : HU: Sum constraint on satellite clocks
CC               20-JUL-01 : HU: SYMIN8 replaced by SYMING
CC               20-JUL-01 : HU: Use LISTI4
CC               24-JUL-01 : HU: Automatic selection of reference satellite
CC               06-AUG-01 : HU: REFWGT=1000/APRSIP**2
CC               07-OCT-01 : HU: INDICES CHANGED FOR PROTYP;
CC                               NFTOT ADDED; MAXFLS DYNAMIC
CC               15-DEC-01 : HU: INCLUDE D_CONST
CC               03-JAN-02 : HU: Problem statistics and rms for sat
CC               11-AUG-02 : HU: Dimension of PROTYP increased to 10
CC               27-AUG-02 : HU: Numerical problem for ms-jumps eliminated
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-JUN-05 : MM: UNUSED VARIABLES REMOVED
CC               21-MAR-07 : HB: SAVE ALLOCATABLE ARRAYS
CC               02-FEB-09 : HB/RD/AS: NO O-C TEST FOR GALILEO SATELLITES
CC               25-MAY-09 : RD: ALLOCATE MAXPAR-MATRICES
CC               18-SEP-09 : RD: CORRECT DEALLOCATION OF THE ARRAYS
CC               18-MAY-10 : RD: OUTLIER DETECTION IMPROVED
CC               19-JUL-10 : SL: TAB CHARACTERS REMOVED
CC               05-MAR-12 : RD: USE LISTI4 AS MODULE NOW
CC               10-JUL-12 : RD: USE SYMINVG INSTEAD OF SYMING
CC               10-JUL-12 : RD: REMOVE UNUSED VARIABLES
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1999     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: i4b, r8b, lfnerr
      USE d_const,  ONLY: c
      USE m_maxdim, ONLY: maxsat
      USE f_ikf
      USE s_alcerr
      USE s_syminvg
      USE s_exitrc
      USE f_listi4
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I0I0  , I12   , IEPOLD, IFIL  , IFILC , II    ,
     1          II1   , II2   , IK    , IND1  , IND2  , INDSAT, IAC   ,
     2          INDSTA, IPAR  , IRC   , ISAT  , ISATC , ISYM  , J     ,
     3          JJ    , K     , KFIL  , KK    , KPAR  , KRFSAT, KSAT  ,
     4          LSAT  , MXCSAT, NDELP , NDELX , NFLOLD, NFTOT , JSAT  ,
     5          NMARK , NOBS  , NPAR  , NSING
C
      REAL*8    BOBS  , QQ    , REFWGT, RESMXL, RMSCHK,
     1          RMSOLD, RTEST , TEST
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C ARGUMENT LIST
C -------------
      INTEGER*4    IEP,INIPHA,NSATEP,NFILEP,MAXITP,MINSTA,IRFSAP
      INTEGER*4    NSATPH,NFILPH,NDEL,IRETRN
      INTEGER*4    FILEP(*),SVNEP(*),NSFLEP(*),SVNPHA(*),FILPHA(*)
      INTEGER*4    NSFLPH(*),NOBSPS(*)
      INTEGER*4    SVNNUM,SVNLST(*)
      INTEGER*4    SAFLEP(MAXSAT,*),SAFLPH(MAXSAT,*)
      INTEGER*4    PROTYP(10,*),PROSAT(9,*),MFIL(4,2)
C
      REAL*8       RESMXP,RMSMXP,OMCMAX,ZENMAX,APRSIP,RMSOBS
      REAL*8       SATCLK(*),SITCLK(*),RMSSAT(*),RMSSIT(*)
      REAL*8       PRNGTL(MAXSAT,*),ZENPHA(MAXSAT,*),RESIDU(MAXSAT,*)
C
      CHARACTER*1  MRKOBS(MAXSAT,*)
C
C LOCAL DECLARATIONS
C ------------------
      INTEGER(i4b), SAVE                                  :: IFIRST=1
      INTEGER(i4b), DIMENSION(:),   ALLOCATABLE, SAVE     :: NSFOLD
      INTEGER(i4b), DIMENSION(:),   ALLOCATABLE, SAVE     :: FILOLD
      INTEGER(i4b), DIMENSION(:,:), ALLOCATABLE, SAVE     :: SAFOLD
      INTEGER(i4b), DIMENSION(:),   ALLOCATABLE           :: PARFLG
      INTEGER(i4b), DIMENSION(:),   ALLOCATABLE           :: ISING
C
      REAL(r8b), DIMENSION(:),   ALLOCATABLE              :: HS1,HS2
      REAL(r8b), DIMENSION(:),   ALLOCATABLE              :: ANOR
      REAL(r8b), DIMENSION(:),   ALLOCATABLE              :: BNOR,SOL
      REAL(r8b), DIMENSION(:,:), ALLOCATABLE, SAVE        :: PRNACT
      REAL(r8b), DIMENSION(:,:), ALLOCATABLE, SAVE        :: WGTACT
      REAL(r8b), DIMENSION(:,:), ALLOCATABLE, SAVE        :: RESNOR
      REAL(r8b), DIMENSION(:,:), ALLOCATABLE, SAVE        :: PRNOLD
      REAL(r8b), DIMENSION(:,:), ALLOCATABLE, SAVE        :: ZENOLD
C
      CHARACTER(LEN=1), DIMENSION(:,:), ALLOCATABLE, SAVE :: MRKPHA
      CHARACTER(LEN=1), DIMENSION(:,:), ALLOCATABLE, SAVE :: MRKOLD
      CHARACTER(LEN=1), DIMENSION(:),   ALLOCATABLE       :: LS1
      CHARACTER*6  MXNSAT
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
C
C CHECK LOCAL MAXIMUM DIMENSIONS
C ------------------------------
      IF(IFIRST.EQ.1) THEN
        ALLOCATE (NSFOLD(NFTOT),STAT=IRC)
        CALL ALCERR(IRC,'NSFOLD',(/NFTOT/),'ESTCLD')
        ALLOCATE (FILOLD(NFTOT),STAT=IRC)
        CALL ALCERR(IRC,'FILOLD',(/NFTOT/),'ESTCLD')
        ALLOCATE (SAFOLD(MAXSAT,NFTOT),STAT=IRC)
        CALL ALCERR(IRC,'SAFOLD',(/MAXSAT,NFTOT/),'ESTCLD')
        NSFOLD=0
        FILOLD=0
        SAFOLD=0
C
        ALLOCATE (PRNACT(MAXSAT,NFTOT),STAT=IRC)
        CALL ALCERR(IRC,'PRNACT',(/MAXSAT,NFTOT/),'ESTCLD')
        ALLOCATE (WGTACT(MAXSAT,NFTOT),STAT=IRC)
        CALL ALCERR(IRC,'WGTACT',(/MAXSAT,NFTOT/),'ESTCLD')
        ALLOCATE (RESNOR(MAXSAT,NFTOT),STAT=IRC)
        CALL ALCERR(IRC,'RESNOR',(/MAXSAT,NFTOT/),'ESTCLD')
        ALLOCATE (PRNOLD(MAXSAT,NFTOT),STAT=IRC)
        CALL ALCERR(IRC,'PRNOLD',(/MAXSAT,NFTOT/),'ESTCLD')
        ALLOCATE (ZENOLD(MAXSAT,NFTOT),STAT=IRC)
        CALL ALCERR(IRC,'ZENOLD',(/MAXSAT,NFTOT/),'ESTCLD')
        PRNACT=0.D0
        WGTACT=0.D0
        RESNOR=0.D0
        PRNOLD=0.D0
        ZENOLD=0.D0
C
        ALLOCATE (MRKPHA(MAXSAT,NFTOT),STAT=IRC)
        CALL ALCERR(IRC,'MRKPHA',(/MAXSAT,NFTOT/),'ESTCLD')
        ALLOCATE (MRKOLD(MAXSAT,NFTOT),STAT=IRC)
        CALL ALCERR(IRC,'MRKOLD',(/MAXSAT,NFTOT/),'ESTCLD')
        MRKPHA=''
        MRKOLD=''
C
        IFIRST=0
      END IF
C
C INITIALIZE IRETRN
C -----------------
      IRETRN=0
C
      NPAR=NSATEP+NFILEP
      ALLOCATE(PARFLG(NPAR),stat=iac)
      CALL alcerr(iac,'PARFLG',(/NPAR/),'ESTCLD')
      PARFLG=0
      ALLOCATE(ISING(NPAR),stat=iac)
      CALL alcerr(iac,'ISING',(/NPAR/),'ESTCLD')
      ISING=0
C
      ALLOCATE(HS1(NPAR),stat=iac)
      CALL alcerr(iac,'HS1',(/NPAR/),'ESTCLD')
      HS1=0D0
      ALLOCATE(HS2(NPAR),stat=iac)
      CALL alcerr(iac,'HS2',(/NPAR/),'ESTCLD')
      HS2=0D0
      ALLOCATE(ANOR(NPAR*(NPAR+1)/2),stat=iac)
      CALL alcerr(iac,'ANOR',(/NPAR*(NPAR+1)/2/),'ESTCLD')
      ANOR=0D0
      ALLOCATE(BNOR(NPAR),stat=iac)
      CALL alcerr(iac,'BNOR',(/NPAR/),'ESTCLD')
      BNOR=0D0
      ALLOCATE(SOL(NPAR),stat=iac)
      CALL alcerr(iac,'SOL',(/NPAR/),'ESTCLD')
      SOL=0D0

      ALLOCATE(LS1(NPAR),stat=iac)
      CALL alcerr(iac,'LS1',(/NPAR/),'ESTCLD')
      LS1=' '
C
C INITIALIZE
C ----------
      IF(INIPHA.EQ.1.OR.IEP-IEPOLD.NE.1)THEN
        IEPOLD=IEP
        NFLOLD=NFILEP
        DO IFIL=1,NFILEP
          FILOLD(IFIL)=FILEP(IFIL)
          NSFOLD(IFIL)=NSFLEP(IFIL)
          DO KSAT=1,NSFLEP(IFIL)
            SAFOLD(KSAT,IFIL)=SAFLEP(KSAT,IFIL)
            PRNOLD(KSAT,IFIL)=PRNGTL(KSAT,IFIL)
            ZENOLD(KSAT,IFIL)=ZENPHA(KSAT,IFIL)
            MRKOBS(KSAT,IFIL)='F'
            MRKOLD(KSAT,IFIL)=' '
          ENDDO
        ENDDO
        IRETRN=1
        INIPHA=0
C
C CHECK NUMBER OF OBSERVATIONS PER SATELLITE
C ------------------------------------------
        DO ISAT=1,NSATEP
          NOBSPS(ISAT)=0
          SATCLK(ISAT)=0.D0
          RMSSAT(ISAT)=0.D0
          DO IFIL=1,NFILEP
            SITCLK(IFIL)=0.D0
            RMSSIT(IFIL)=0.D0
            DO KSAT=1,NSFLEP(IFIL)
              IF(SVNEP(ISAT).EQ.SAFLEP(KSAT,IFIL).AND.
     1                    MRKOBS(KSAT,IFIL).EQ.'F')THEN
                NOBSPS(ISAT)=NOBSPS(ISAT)+1
                EXIT
              ENDIF
            ENDDO
          ENDDO
C
          IF(NOBSPS(ISAT).LT.MINSTA)THEN
            DO IFIL=1,NFILEP
              DO KSAT=1,NSFLEP(IFIL)
                IF(SVNEP(ISAT).EQ.SAFLEP(KSAT,IFIL))THEN
                  MRKOLD(KSAT,IFIL)='X'
                  MRKOBS(KSAT,IFIL)='X'
                ENDIF
              ENDDO
            ENDDO
          ENDIF
        ENDDO
        GOTO 999
      ENDIF
C
C PREPARE CLOCK DIFFERENCE TERMS ``OBS-COMPUTED''
C AND MANAGE BOOK-KEEPING BETWEEN EPOCHS
C -----------------------------------------------
C
C INITIALIZE NUMBER OF SATELLITES, NUMBER OF FILES, NUMBER OF ITERATIONS
      IEPOLD=IEP
      NMARK =0
      DO JJ=1,MAXSAT
        NOBSPS(JJ)=0
      END DO
C ITERATIONS
      DO 9 KK=1,MAXITP
        NMARK =0
        NSATPH=0
        NFILPH=0
C INITIALIZE SATELLITE AND STATION CLOCK DIFFERENCES
        DO IFIL=1,NFILEP
          SITCLK(IFIL)=0.D0
          RMSSIT(IFIL)=0.D0
        ENDDO
        DO ISAT=1,NSATEP
          SATCLK(ISAT)=0.D0
          RMSSAT(ISAT)=0.D0
        ENDDO
        DO 600 IFIL=1,NFILEP
C
C FIND CORRESPONDING FILE IN PREV. EPOCH
          KFIL=LISTI4(0,NFTOT,FILOLD,FILEP(IFIL),NFLOLD)
C
C ALL SATELLITES OF CURRENT FILES HAVE TO HAVE FLAG 'F'
          IF (KFIL.EQ.0) THEN
            DO ISAT=1,NSFLEP(IFIL)
              MRKOBS(ISAT,IFIL)='F'
            ENDDO
            CYCLE    ! GOTO 600
          ENDIF
          NFILPH=NFILPH+1
          FILPHA(NFILPH)=FILEP(IFIL)
          NSFLPH(NFILPH)=0
C
C LOOP OVER SATELLITES OF CURRENT EPOCH
          DO 530 ISAT=1,NSFLEP(IFIL)
C
C FIND MATCHING SATELLITE IN PREVIOUS EPOCH (IN CORRESP. FILE)
            KSAT=LISTI4(0,MAXSAT,SAFOLD(1,KFIL),SAFLEP(ISAT,IFIL),
     1                                               NSFOLD(KFIL))
            IF (KSAT.EQ.0) THEN
              MRKOBS(ISAT,IFIL)='F'
              CYCLE    ! GOTO 530
            ENDIF
C
C UPDATE SATELLITE NUMBERS OF EPOCH
            LSAT=LISTI4(0,MAXSAT,SVNPHA,SAFLEP(ISAT,IFIL),NSATPH)
            IF (LSAT.EQ.0) THEN
              NSATPH=NSATPH+1
              IF(NSATPH.GT.MAXSAT)THEN
                WRITE(LFNERR,11)
11              FORMAT(/,' *** SR ESTCLD: MAXSAT TOO SMALL',/)
                CALL EXITRC(2)
              ENDIF
              SVNPHA(NSATPH)=SAFLEP(ISAT,IFIL)
              IF(MRKOBS(ISAT,IFIL).EQ.' ')THEN
                NOBSPS(NSATPH)=1
              ELSE
                NOBSPS(NSATPH)=0
              ENDIF
            ELSE
              IF(MRKOBS(ISAT,IFIL).EQ.' ')NOBSPS(LSAT)=NOBSPS(LSAT)+1
            ENDIF
C
C SET TERMS ``OBS-COMPUTED'' AND SAFLPH
            NSFLPH(NFILPH)=NSFLPH(NFILPH)+1
            SAFLPH(NSFLPH(NFILPH),NFILPH)=SAFLEP(ISAT,IFIL)
            TEST=PRNGTL(ISAT,IFIL) - PRNOLD(KSAT,KFIL)
c            RTEST=DNINT(TEST/(C/1000.D0))
c            TEST=TEST-RTEST*C/1000.D0
            PRNACT(NSFLPH(NFILPH),NFILPH)=TEST
C
C TAKE INTO ACCOUNT FLAGS FROM CODE PROCESSING OR
C PREVIOUS PHASE PROCESSING
            MRKPHA(NSFLPH(NFILPH),NFILPH)=MRKOBS(ISAT,IFIL)
C
C IS THE OLD OBSERVATION ACCEPTABLE?
C ---------------------------------
            IF(MRKOLD(KSAT,KFIL).NE.' '.AND.
     1                  MRKOBS(ISAT,IFIL).EQ.' ')THEN
              MRKPHA(NSFLPH(NFILPH),NFILPH)='F'
              MRKOBS(ISAT,IFIL)='F'
            ENDIF
            IF(ZENPHA(ISAT,IFIL).LT.ZENMAX.AND.
     1                  ZENOLD(KSAT,KFIL).LT.ZENMAX)THEN
C ZENITH DEPENDENT WEIGHTING OF OBSERVATIONS
C ------------------------------------------
              WGTACT(NSFLPH(NFILPH),NFILPH)=
     1                    1/(APRSIP**2*( 1/DCOS(ZENPHA(ISAT,IFIL))**2+
     2                    1/DCOS(ZENOLD(KSAT,KFIL))**2 ))
            ELSE
              MRKPHA(NSFLPH(NFILPH),NFILPH)='Z'
              MRKOBS(ISAT,IFIL)='Z'
            ENDIF
C
C CHECK MAXIMUM ALLOWED TERM O-C
            IF(DABS(PRNACT(NSFLPH(NFILPH),NFILPH)).GT.OMCMAX.AND.
     1                  MRKOBS(ISAT,IFIL).EQ.' '.AND.
     2         SAFLPH(NSFLPH(NFILPH),NFILPH).LT.200)THEN
              MRKPHA(NSFLPH(NFILPH),NFILPH)='P'
              MRKOBS(ISAT,IFIL)='P'
            ENDIF
530       CONTINUE
600     CONTINUE
C
C CHECK MAX. NUMBER OF PARAMETERS
C -------------------------------
        NPAR=NSATPH+NFILPH
C
C INITIALIZE NEQ-SYSTEM
C ---------------------
        DO I=1,NPAR
          ISING(I)=0
          BNOR(I)=0
          DO K=1,I
            IK=IKF(I,K)
            ANOR(IK)=0.D0
          ENDDO
        ENDDO
C
C CHECK WHETHER ALL SATELLITES WERE OBSERVED
C ------------------------------------------
        IRFSAP=0
        REFWGT=1000D0/APRSIP**2
cccc        REFWGT=1000000D0
        DO ISAT=1,NSATPH
          I0I0=IKF(ISAT,ISAT)
          IF(NOBSPS(ISAT).LT.MINSTA)THEN
C
C MARK OBSERVATIONS
            DO IFIL=1,NFILPH
              DO KSAT=1,NSFLPH(IFIL)
                IF(SVNPHA(ISAT).EQ.SAFLPH(KSAT,IFIL))THEN
                  IF(MRKPHA(KSAT,IFIL).EQ.' ')MRKPHA(KSAT,IFIL)='X'
                ENDIF
              ENDDO
            ENDDO
            DO IFIL=1,NFILEP
              DO KSAT=1,NSFLEP(IFIL)
                IF(SVNPHA(ISAT).EQ.SAFLEP(KSAT,IFIL))THEN
                  IF(MRKOBS(KSAT,IFIL).EQ.' ')MRKOBS(KSAT,IFIL)='X'
                ENDIF
              ENDDO
            ENDDO
          ENDIF
        ENDDO
C
C REFERENCE SATELLITE
C -------------------
        RefSelLoop: DO IFIL=1,NFILPH
          DO ISAT=1,NSFLPH(IFIL)
            KSAT=LISTI4(0,MAXSAT,SVNPHA,SAFLPH(ISAT,IFIL),NSATPH)
            IF(MRKPHA(ISAT,IFIL).EQ.' ')THEN
              IRFSAP=SVNPHA(KSAT)
              KRFSAT=KSAT
              EXIT RefSelLoop
            ENDIF
          ENDDO
        ENDDO RefSelLoop
C
C SET UP NORMAL EQUATION SYSTEM
C -----------------------------
C
C THE SATELLITE CLOCK ERRORS ARE FIRST, FOLLOWED BY THE
C RECEIVER CLOCK ERRORS IN THE NEQ SYSTEM
C -----------------------------------------------------
        NOBS=0
        DO IFIL=1,NFILPH
          IND2=NSATPH+IFIL
          DO ISAT=1,NSFLPH(IFIL)
            IND1=LISTI4(0,MAXSAT,SVNPHA,SAFLPH(ISAT,IFIL),NSATPH)
            IF(MRKPHA(ISAT,IFIL).EQ.' ')THEN
              NOBS=NOBS+1
              BOBS=PRNACT(ISAT,IFIL)*WGTACT(ISAT,IFIL)
              BNOR(IND1)=BNOR(IND1)-BOBS
              BNOR(IND2)=BNOR(IND2)+BOBS
              II1=IKF(IND1,IND1)
              ANOR(II1)=ANOR(II1)+1.D0*WGTACT(ISAT,IFIL)
              II2=IKF(IND2,IND2)
              ANOR(II2)=ANOR(II2)+1.D0*WGTACT(ISAT,IFIL)
              I12=IKF(IND1,IND2)
              ANOR(I12)=ANOR(I12)-1.D0*WGTACT(ISAT,IFIL)
            ENDIF
          ENDDO
        ENDDO
C REFERENCE CLOCK IS ALWAYS THE FIRST SATELLITE, CLOCK DIFFERENCE SET TO ZERO
        I0I0=IKF(KRFSAT,KRFSAT)
ccc       i0i0=ikf(nsatep+1,nsatep+1)
        ANOR(I0I0)=ANOR(I0I0)+REFWGT
C CHECK FOR ZERO DIAGONAL ELEMENTS OF ANOR=> REGULARIZE ANOR
        ISYM=0
        DO I=1,NPAR
          J=IKF(I,I)
          IF(ANOR(J).EQ.0.D0)THEN
            ANOR(J)=1.D0
            ISYM=ISYM+1
            ISING(I)=1
          ENDIF
        ENDDO
C
C SOLVE NEQ SYSTEM
C
        CALL SYMINVG(NPAR,ANOR,0,NSING,PARFLG)
        IF(NSING.NE.0)THEN
          WRITE(LFNERR,110)IEP
110       FORMAT(' ### SR ESTCLD: MATRIX SINGULAR, IEP=',I6)
cc        write(lfnerr,*) 'sats:',SVNPHA(1:NSATPH)
cc        write(lfnerr,*) npar,':',parflg(1:npar)
cc        IRETRN=2
cc        GOTO 999
        ENDIF
C
C COMPUTE SOLUTION
C ----------------
        DO IPAR=1,NPAR
          SOL(IPAR)=0.D0
          IF (ISING(IPAR).NE.1) THEN
            DO KPAR=1,NPAR
              IK=IKF(IPAR,KPAR)
              SOL(IPAR)=SOL(IPAR)+ANOR(IK)*BNOR(KPAR)
            ENDDO
          ENDIF
        ENDDO
C
C COMPUTE RESIDUALS AND RMS ERROR
C -------------------------------
C
C RESIDUALS
C ---------
        RMSOBS=0.D0
        RESMXL=0.D0
        DO IFIL=1,NFILPH
          IND2=NSATPH+IFIL
          DO ISAT=1,NSFLPH(IFIL)
            IND1=LISTI4(0,MAXSAT,SVNPHA,SAFLPH(ISAT,IFIL),NSATPH)
            RESIDU(ISAT,IFIL)=PRNACT(ISAT,IFIL)-(SOL(IND2)-SOL(IND1))
            IF(MRKPHA(ISAT,IFIL).EQ.' ')THEN
              RMSOBS=RMSOBS+WGTACT(ISAT,IFIL)*RESIDU(ISAT,IFIL)**2
            ENDIF
          ENDDO
        ENDDO
C
C RMS OF OBSERVATION
C ------------------
        IF(NOBS+ISYM.GT.NPAR)THEN
          RMSOBS=DSQRT(RMSOBS/(NOBS+ISYM-NPAR))
        ELSE
          RMSOBS=0.D0
        END IF
C
C COMPUTE RMS ERRORS OF ESTIMATES
C -------------------------------
        DO IPAR=1,NPAR
          IF (ISING(IPAR).NE.1) THEN
            II=IKF(IPAR,IPAR)
            QQ=DSQRT(ANOR(II))
            IF(IPAR.LE.NSATPH)THEN
              SATCLK(IPAR)=SOL(IPAR)/C
              RMSSAT(IPAR)=QQ
!             RMSSAT(IPAR)=QQ/C
!             RMSSAT(IPAR)=RMSOBS*QQ/C
              IF(NOBSPS(IPAR).LT.MINSTA)THEN
                SATCLK(IPAR)=0.D0
                RMSSAT(IPAR)=0.D0
              ENDIF
            ELSE
              SITCLK(IPAR-NSATPH)=SOL(IPAR)/C
              RMSSIT(IPAR-NSATPH)=QQ
!             RMSSIT(IPAR-NSATPH)=QQ/C
!             RMSSIT(IPAR-NSATPH)=RMSOBS*QQ/C
            ENDIF
          ENDIF
        ENDDO
C
C SCREENING PROCEDURE: MARK BIGGEST RESIDUAL
C ------------------------------------------
!      IF(RMSOBS.NE.0.01D0.AND.RESMXL.GT.RESMXP.AND.NMARK.LT.MAXITP)THEN
C RESIDUAL CHECK (4*sigma-METHOD)
C -------------------------------
        RMSCHK=4*RMSOBS
!        IF(RMSCHK.LT.RESMXP)RMSCHK=RESMXP
        DO IFIL=1,NFILPH
          DO ISAT=1,NSFLPH(IFIL)
!           KSAT=LISTI4(0,MAXSAT,SVNPHA,SAFLPH(ISAT,IFIL),NSATPH) ! necessary?
            IF(MRKPHA(ISAT,IFIL).EQ.' ')THEN
              RESNOR(ISAT,IFIL)=RESIDU(ISAT,IFIL)*
     1                    DSQRT(WGTACT(ISAT,IFIL))
ccc              IF(DABS(RESNOR(ISAT,IFIL)).GT.RMSCHK.OR.
ccc     1                            RMSOBS.GT.RMSMXP/APRSIP)THEN
              IF(DABS(RESNOR(ISAT,IFIL)).GT.RMSCHK)THEN
C
C FIND CORRECT FILE/SATELLITE COMBINATION IN ORIGINAL ARRAY MRKOBS
                IFILC=LISTI4(0,NFTOT,FILEP,FILPHA(IFIL),NFILEP)

                ISATC=LISTI4(0,MAXSAT,SAFLEP(1,IFILC),
     1                         SAFLPH(ISAT,IFIL),NSFLEP(IFILC))
!!!      write(*,*)iep,kk,ifil,isat,SAFLPH(ISAT,IFIL),RESNOR(ISAT,IFIL),'X'
c                MRKOBS(ISATC,IFILC)='P'
c                NMARK=NMARK+1
!      GO TO 9
      ELSE
!!!      write(*,*) iep,kk,ifil,isat,SAFLPH(ISAT,IFIL),RESNOR(ISAT,IFIL),'X'
              ENDIF
            ENDIF
          ENDDO
        ENDDO
C RESIDUAL CHECK (4*sigma-METHOD)
C -------------------------------
        RMSCHK=4*RMSOBS
        DO IFIL=1,NFILPH
          DO ISAT=1,NSFLPH(IFIL)
            RESNOR(ISAT,IFIL)=1D20
            IF(MRKPHA(ISAT,IFIL).EQ.' ')THEN
              RESNOR(ISAT,IFIL)=RESIDU(ISAT,IFIL)*
     1                    DSQRT(WGTACT(ISAT,IFIL))
            ENDIF
          ENDDO
        ENDDO
C
C LOOK FOR THE TWO BIGGEST RESIDUALS
        DO ISAT=1,NSATPH
          MFIL=0
          DO IFIL=1,NFILPH
            JSAT=LISTI4(0,MAXSAT,SAFLPH(1,IFIL),SVNPHA(ISAT),
     1                  NSFLPH(IFIL))
            IF (JSAT.EQ.0) CYCLE
            IF (RESNOR(JSAT,IFIL).EQ.1d20.OR.
     1          RESNOR(JSAT,IFIL).EQ.0d0) CYCLE
            IF (MFIL(1,1).EQ.0) THEN
              MFIL(1,1)=IFIL
              MFIL(1,2)=JSAT
              MFIL(3,1)=IFIL
              MFIL(3,2)=JSAT
            ELSEIF(RESNOR(JSAT,IFIL).GT.
     1             RESNOR(MFIL(1,2),MFIL(1,1)))THEN
              MFIL(2,:)=MFIL(1,:)
              MFIL(1,1)=IFIL
              MFIL(1,2)=JSAT
              IF (MFIL(4,1).EQ.0) THEN
                MFIL(4,1)=IFIL
                MFIL(4,2)=JSAT
              ENDIF
            ELSEIF(RESNOR(JSAT,IFIL).LT.
     1             RESNOR(MFIL(3,2),MFIL(3,1)))THEN
              MFIL(4,:)=MFIL(3,:)
              MFIL(3,1)=IFIL
              MFIL(3,2)=JSAT
              IF (MFIL(2,1).EQ.0) THEN
                MFIL(2,1)=IFIL
                MFIL(2,2)=JSAT
              ENDIF
            ELSEIF(MFIL(2,1).EQ.0) THEN
              MFIL(2,1)=IFIL
              MFIL(2,2)=JSAT
              MFIL(4,1)=IFIL
              MFIL(4,2)=JSAT
            ELSEIF(RESNOR(JSAT,IFIL).GT.
     1             RESNOR(MFIL(2,2),MFIL(2,1)))THEN
              MFIL(2,1)=IFIL
              MFIL(2,2)=JSAT
            ELSEIF(RESNOR(JSAT,IFIL).LT.
     1             RESNOR(MFIL(4,2),MFIL(4,1)))THEN
              MFIL(4,1)=IFIL
              MFIL(4,2)=JSAT
            ENDIF
          ENDDO
          IF(MFIL(1,1).EQ.0) CYCLE
          IF(DABS(RESNOR(MFIL(1,2),MFIL(1,1))).LT.RMSCHK.AND.
     1       DABS(RESNOR(MFIL(3,2),MFIL(3,1))).LT.RMSCHK) CYCLE
!!!          write(*,*) 'MFIL-0',1,RESNOR(MFIL(1,2),MFIL(1,1))
!!!          IF (mfil(2,1).ne.0) THEN
!!!            write(*,*) 'MFIL-0',2,RESNOR(MFIL(2,2),MFIL(2,1))
!!!          ELSE
!!!            write(*,*) 'MFIL-0',2,0d0
!!!          ENDIF
!!!          write(*,*) 'MFIL-0',3,RESNOR(MFIL(3,2),MFIL(3,1))
!!!          IF (mfil(4,1).ne.0) THEN
!!!            write(*,*) 'MFIL-0',4,RESNOR(MFIL(4,2),MFIL(4,1))
!!!          ELSE
!!!            write(*,*) 'MFIL-0',4,0d0
!!!          ENDIF

          IF (DABS(RESNOR(MFIL(3,2),MFIL(3,1))).GT.
     1        DABS(RESNOR(MFIL(1,2),MFIL(1,1)))) THEN
            MFIL(1:2,1:2) = MFIL(3:4,1:2)
          ENDIF
!!!          write(*,*) 'MFIL-1',1,RESNOR(MFIL(1,2),MFIL(1,1))
!!!          IF (mfil(2,1).ne.0) THEN
!!!            write(*,*) 'MFIL-1',2,RESNOR(MFIL(2,2),MFIL(2,1))
!!!          ELSE
!!!            write(*,*) 'MFIL-1',2,0d0
!!!          ENDIF
C
C MARK THE BIGGEST RESIDUAL
          IFILC=LISTI4(0,NFTOT,FILEP,FILPHA(MFIL(1,1)),NFILEP)
          ISATC=LISTI4(0,MAXSAT,SAFLEP(1,IFILC),
     1                         SVNPHA(ISAT),NSFLEP(IFILC))
          MRKOBS(ISATC,IFILC)='P'
          NMARK=NMARK+1
!!!      write(*,*)iep,kk,MFIL(1,1),MFIL(1,1),SVNPHA(ISAT),
!!!     1          RESNOR(MFIL(1,2),MFIL(1,1)),'PP'
C
C MARK NO MORE RESIDUALS IF THE BIGGEST IS MUSCH BIGGER THAN THE SECOND ONE
          IF (MFIL(2,1).EQ.0) CYCLE
          IF (DABS(RESNOR(MFIL(1,2),MFIL(1,1))).GT.100d0*RESMXP) CYCLE
          IF (DABS(RESNOR(MFIL(1,2),MFIL(1,1))-
     1             RESNOR(MFIL(2,2),MFIL(2,1)))/
     2        DABS(RESNOR(MFIL(1,2),MFIL(1,1))).GT.0.5d0) CYCLE
C
C MARK ALSO THE OTHER BIG RESIDUALS
          DO IFIL=1,NFILPH
            IF (IFIL.EQ.MFIL(1,1)) CYCLE
            JSAT=LISTI4(0,MAXSAT,SAFLPH(1,IFIL),SVNPHA(ISAT),
     1                  NSFLPH(IFIL))
            IF (JSAT.EQ.0) CYCLE
            IF (RESNOR(JSAT,IFIL).EQ.1d20.OR.
     1          RESNOR(JSAT,IFIL).EQ.0d0) CYCLE
            IFILC=LISTI4(0,NFTOT,FILEP,FILPHA(IFIL),NFILEP)
            ISATC=LISTI4(0,MAXSAT,SAFLEP(1,IFILC),
     1                           SVNPHA(ISAT),NSFLEP(IFILC))
            MRKOBS(ISATC,IFILC)='P'
            NMARK=NMARK+1
!!!      write(*,*)iep,kk,IFIL,ISAT,SVNPHA(ISAT),
!!!     1          RESNOR(JSAT,IFIL),'P'
          ENDDO
        ENDDO
        IF (RMSOBS.EQ.RMSOLD.AND.RMSOBS.NE.0.D0)EXIT
!       IF (KK.LT.MAXITP.AND.RMSOBS.LT.RMSMXP) EXIT
        RMSOLD=RMSOBS
9     CONTINUE
C
C BOOKKEEPING
C -----------
      IEPOLD=IEP
      NFLOLD=NFILEP
      NDELX=0
      NDELP=0
      NDEL=0
      DO IFIL=1,NFILEP
        FILOLD(IFIL)=FILEP(IFIL)
        NSFOLD(IFIL)=NSFLEP(IFIL)
        DO ISAT=1,NSFLEP(IFIL)
          INDSTA=FILEP(IFIL)
          INDSAT=LISTI4(0,MAXSAT,SVNLST,SAFLEP(ISAT,IFIL),SVNNUM)
          IF(INDSAT.EQ.0)CYCLE
          SAFOLD(ISAT,IFIL)=SAFLEP(ISAT,IFIL)
          PRNOLD(ISAT,IFIL)=PRNGTL(ISAT,IFIL)
          ZENOLD(ISAT,IFIL)=ZENPHA(ISAT,IFIL)
C
C TAKE OVER "NON EXISTING CODE OBS"-FLAG FROM PREVIOUS EPOCH
          IF(MRKOBS(ISAT,IFIL).EQ.'M')THEN
            MRKOLD(ISAT,IFIL)='M'
          ELSE IF(MRKOBS(ISAT,IFIL).EQ.'X')THEN
            MRKOLD(ISAT,IFIL)=' '
            PROTYP(6,INDSTA)=PROTYP(6,INDSTA)+1
            PROSAT(6,INDSAT)=PROSAT(6,INDSAT)+1
            NDELX=NDELX+1
          ELSE IF(MRKOBS(ISAT,IFIL).EQ.'F')THEN
            MRKOLD(ISAT,IFIL)=' '
            PROTYP(3,INDSTA)=PROTYP(3,INDSTA)+1
            PROSAT(3,INDSAT)=PROSAT(3,INDSAT)+1
          ELSE IF(MRKOBS(ISAT,IFIL).EQ.'P')THEN
            MRKOLD(ISAT,IFIL)=' '
            PROTYP(4,INDSTA)=PROTYP(4,INDSTA)+1
            PROSAT(4,INDSAT)=PROSAT(4,INDSAT)+1
            NDELP=NDELP+1
          ELSE
            MRKOLD(ISAT,IFIL)=' '
          END IF
        ENDDO
      ENDDO
      NDEL=NDELX+NDELP
C
999   CONTINUE
C
      DEALLOCATE(PARFLG)
      DEALLOCATE(ISING)
      DEALLOCATE(HS1)
      DEALLOCATE(HS2)
      DEALLOCATE(ANOR)
      DEALLOCATE(BNOR)
      DEALLOCATE(SOL)
      DEALLOCATE(LS1)
C
      RETURN
      END SUBROUTINE

      END MODULE
