      MODULE s_ESTCLK
      CONTAINS

C*
      SUBROUTINE ESTCLK(IEP,ITER,NITER,NFTOT,NSATEP,NFILEP,FILEP,
     1                  SVNNUM,SVNLST,SVNEP,NSFLEP,SAFLEP,SCKIRC,
     2                  MINSTA,RMSMAX,RESMAX,PRNGTL,MRKOBS,ZENDST,
     3                  OMCMXC,APRSIC,MAXITC,IRFSAT,SATCLK,
     4                  SITCLK,RMSOBS,RMSSAT,RMSSIT,RESIDU,NMARK,
     5                  PROTYP,PROSAT,NOBSCS,INDC,RESFIL,INDS,
     6                  RESSVN,IRETRN)
CC
CC NAME       :  ESTCLK
CC
CC PURPOSE    :  DETERMINE SATELLITE AND STATION CLOCK CORRECTIONS
CC               USING ALL CODE MEASUREMENTS OF ONE EPOCH.
CC               IT IS ASSUMED THAT THE COORDINATES OF ALL SITES
CC               ARE KNOWN.
CC
CC PARAMETERS :
CC        IN  :  IEP    : EPOCH NUMBER                          I*4
CC               NFTOT  : TOTAL NUMBER OF FILES                 I*4
CC               NSATEP : NUMBER OF SATELLITES IN EPOCH         I*4
CC               NFILEP : NUMBER OF FILES IN EPOCH              I*4
CC               SVNEP  : SATELLITE NUMBERS                     I*4
CC               NSFLEP : NUMBER OF SATELLITES IN FILE          I*4
CC               SAFLEP : SATELLITE NUMBERS IN FILE             I*4
CC               SCKIRC : RETURN CODES FROM GTSCLM FOR EACH SAT. I*4(*)
CC               MINSTA : MINIMUM NUMBER OF STATIONS OBSERVING  I*4
CC                        A SATELLITE
CC               RMSMAX : MAX. RMS OF OBSERVATION TOLERATED     R*8
CC               RESMAX : MAXIMUM RESIDUAL TOLERATED (ONLY      R*8
CC                        CHECKED, IF RMSMAX EXCEEDED)
CC               PRNGTL : P-RANGE CORRECTED FOR GEOMETRY        R*8
CC               MRKOBS : FLAGS FOR OBSERVATIONS                CH*1
CC               ZENDST : ZENITH DISTANCE OF OBSERVATION        R*8
CC               OMCMXC : MAX. O-C FOR CODE OBSERV.             R*8
CC               APRSIC : A PRIORI SIGMA FOR CODE OBS.          R*8
CC               MAXITC : MAXIMUM NUMBER OF ITERATIONS          I*4
CC        OUT :  IRFSAT : SNV NR OF REFERENCE SATELLITE         I*4
CC                        (AUTOMATICALLY SELECTED)
CC               SATCLK : SATELLITE CLOCK ERRORS (sec)          R*8
CC               SITCLK : SITE CLOCK ERROR (sec)                R*8
CC               RMSOBS : RMS ERROR OF OBSERVATION              R*8
CC               RMSSAT : RMS ERROR OF SATELLITE CLOCK ERRORS   R*8
CC               RMSSIT : RMS ERROR OF SITES                    R*8
CC               RESIDU : RESIDUALS                             R*8
CC               NMARK  : NUMBER OF MARKED OBSERVATIONS         I*4
CC               PROTYP : PROBLEM TYPE (MODIFIED HERE)          I*4
CC               IRETRN : RETURN CODE                           I*4
CC
CC REMARKS    :  THE REFERENCE CLOCK IS SELECTED AUTOMATICALLY
CC               (MAY BE TRANSFORMED TO ANY OTHER CLOCK OUTSIDE).
CC               THE CORRESPONDING CLOCK IS SET TO ZERO.
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  4.1  (MAY 99)
CC
CC CREATED    :  99/05/15
CC
CC CHANGES    :  04-MAY-01 : HB: TAKE RMSSAT AND RMSSIT IN M NOT IN SECONDS
CC               15-JUL-01 : HU: SUM CONDITION ON SATELLITES
CC               20-JUL-01 : HU: SYMIN8 REPLACED BY SYMING
CC               20-JUL-01 : HU: USE LISTI4
CC               24-JUL-01 : HU: AUTOMATIC SELECTION OF REFERENCE SATELLITE
CC               06-AUG-01 : HU: REFWGT=1000/APRSIC**2
CC               07-OCT-01 : HU: INDICES CHANGED FOR PROTYP, PROTYT;
CC                               NFTOT ADDED; MAXFLS DYNAMIC
CC               03-JAN-02 : HU: PROBLEM STATISTICS AND RMS FOR SATELLITES
CC               11-AUG-02 : HU: DIMENSION OF PROTYP INCREASED TO 10
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               02-JAN-05 : HU: SKIP SVN NUMBERS 99
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-JUN-05 : MM: UNUSED VARIABLES REMOVED
CC               24-NOV-05 : RD: DO NOT USE A SATELLITE W/O A PRIORI AS REF.
CC                               DOUBLE RESMAX FOR GLONASS
CC               21-MAR-07 : HB: SAVE ALLOCATABLE ARRAYS
CC               02-FEB-09 : HB/RD/AS: SPECIAL GALILEO RMSFAC
CC               25-MAY-09 : RD: ALLOCATE MAXPAR-MATRICES
CC               18-SEP-09 : RD: CORRECT DEALLOCATION OF THE ARRAYS
CC               19-JUL-10 : SL: TAB CHARACTERS REMOVED
CC               21-MAY-11 : RD: REMOVING HUGE OUTLIERS DOES NOT COUNT
CC                               AS AN ITERATION
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
      USE f_listi4
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I0I0  , I12   , IFIL  , II    , II1   , II2   ,
     1          IK    , IND1  , IND2  , IPAR  , IRC   , ISAT  , IAC   ,
     2          ISYM  , ITERL , J     , JJ    , K     , KFIL  , KPAR  ,
     3          KRFSAT, KSAT  , MXCSAT, NOBS  , NPAR  , NSING ,
     4          IMXSAT, IMXFIL, KMXSAT, KMXFIL
C
      REAL*8    BOBS  , QQ    , REFWGT, RMSCHK, RMSOLD, RMSFAC,
     1          RMXRES
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C ARGUMENT LIST
C -------------
      INTEGER*4    IEP,NSATEP,NFILEP,MINSTA,MAXITC,IRFSAT,NMARK,IRETRN
      INTEGER*4    ITER,NITER,NFTOT
      INTEGER*4    FILEP(*),SVNEP(*),NSFLEP(*),NOBSCS(*),INDC(*),INDS(*)
      INTEGER*4    SVNNUM,SVNLST(*),SCKIRC(*)
      INTEGER*4    SAFLEP(MAXSAT,*),PROTYP(10,*),PROSAT(9,*)

      REAL*8       RMSMAX,RESMAX,APRSIC,RMSOBS,OMCMXC
      REAL*8       PRNGTL(MAXSAT,*),ZENDST(MAXSAT,*)
      REAL*8       SATCLK(*),SITCLK(*),RMSSAT(*),RMSSIT(*),RESFIL(*)
      REAL*8       RESSVN(*),RESIDU(MAXSAT,*)

      CHARACTER*1  MRKOBS(MAXSAT,*)
C
C LOCAL DECLARATIONS
C ------------------
      INTEGER(i4b), SAVE :: ifirst=1
      INTEGER(i4b), DIMENSION(:), ALLOCATABLE :: PARFLG,ISING
C
      REAL(r8b), DIMENSION(:),   ALLOCATABLE         :: HS1,HS2
      REAL(r8b), DIMENSION(:),   ALLOCATABLE         :: ANOR
      REAL(r8b), DIMENSION(:),   ALLOCATABLE         :: BNOR,SOL
      REAL(r8b), DIMENSION(:,:), ALLOCATABLE,SAVE    :: RESNOR
      REAL(r8b), DIMENSION(:,:), ALLOCATABLE,SAVE    :: WGTPRN
C
      CHARACTER(LEN=1), DIMENSION(:), ALLOCATABLE    :: LS1
      CHARACTER*6  MXNSAT
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
C
C CHECK LOCAL MAXIMUM DIMENSIONS
C ------------------------------
      IF(IFIRST.EQ.1) THEN
        ALLOCATE (RESNOR(MAXSAT,NFTOT),STAT=IRC)
        CALL ALCERR(IRC,'RESNOR',(/MAXSAT,NFTOT/),'ESTCLK')
        ALLOCATE (WGTPRN(MAXSAT,NFTOT),STAT=IRC)
        CALL ALCERR(IRC,'WGTPRN',(/MAXSAT,NFTOT/),'ESTCLK')
        RESNOR=0.D0
        WGTPRN=0.D0
C
        IFIRST=0
      ENDIF
C
C INITIALIZE
C ----------
      IRETRN=0
      NMARK=0
      RMSOLD=0.D0
      DO JJ=1,NSATEP
        DO II=1,NFTOT
          WGTPRN(JJ,II)=0.D0
        ENDDO
        RMSSAT(JJ)=0.D0
      ENDDO
      DO JJ=1,NFILEP
        RMSSIT(JJ)=0.D0
      ENDDO
C
      NPAR=NSATEP+NFILEP
      ALLOCATE(PARFLG(NPAR),stat=iac)
      CALL alcerr(iac,'PARFLG',(/NPAR/),'ESTCLK')
      PARFLG=0
      ALLOCATE(ISING(NPAR),stat=iac)
      CALL alcerr(iac,'ISING',(/NPAR/),'ESTCLK')
      ISING=0
C
      ALLOCATE(HS1(NPAR),stat=iac)
      CALL alcerr(iac,'HS1',(/NPAR/),'ESTCLK')
      HS1=0D0
      ALLOCATE(HS2(NPAR),stat=iac)
      CALL alcerr(iac,'HS2',(/NPAR/),'ESTCLK')
      HS2=0D0
      ALLOCATE(ANOR(NPAR*(NPAR+1)/2),stat=iac)
      CALL alcerr(iac,'ANOR',(/NPAR*(NPAR+1)/2/),'ESTCLK')
      ANOR=0D0
      ALLOCATE(BNOR(NPAR),stat=iac)
      CALL alcerr(iac,'BNOR',(/NPAR/),'ESTCLK')
      BNOR=0D0
      ALLOCATE(SOL(NPAR),stat=iac)
      CALL alcerr(iac,'SOL',(/NPAR/),'ESTCLK')
      SOL=0D0

      ALLOCATE(LS1(NPAR),stat=iac)
      CALL alcerr(iac,'LS1',(/NPAR/),'ESTCLK')
      LS1=' '
C
C CHECK MAX. NUMBER OF PARAMETERS
C -------------------------------
      ITERL=0
      IterationLoop: DO WHILE (ITERL < MAXITC)
        ITERL=ITERL+1
        NPAR=NSATEP+NFILEP
        DO II=1,NFTOT
          INDC(II)=0
          RESFIL(II)=0.D0
        ENDDO
C
C CHECK NUMBER OF STATIONS OBSERVING ONE SATELLITE
C ------------------------------------------------
        IRFSAT=0
        REFWGT=1000D0/APRSIC**2
cccc        REFWGT=1000D0
        DO ISAT=1,NSATEP
          IF (SVNEP(ISAT).EQ.99) CYCLE
          NOBSCS(ISAT)=0
          DO IFIL=1,NFILEP
            DO KSAT=1,NSFLEP(IFIL)
              IF(SVNEP(ISAT).EQ.SAFLEP(KSAT,IFIL).AND.
     1                    MRKOBS(KSAT,IFIL).EQ.' ')THEN
                NOBSCS(ISAT)=NOBSCS(ISAT)+1
                EXIT
              ENDIF
            ENDDO
          ENDDO
C
          IF(NOBSCS(ISAT).LT.MINSTA)THEN
            DO IFIL=1,NFILEP
              DO KSAT=1,NSFLEP(IFIL)
                IF(SVNEP(ISAT).EQ.SAFLEP(KSAT,IFIL))THEN
                  MRKOBS(KSAT,IFIL)='X'
                ENDIF
              ENDDO
            ENDDO
          ENDIF
        ENDDO
C
C REFERENCE SATELLITE
C -------------------
        RefSelLoop: DO IFIL=1,NFILEP
          DO ISAT=1,NSFLEP(IFIL)
            KSAT=LISTI4(0,MAXSAT,SVNEP,SAFLEP(ISAT,IFIL),NSATEP)
            IF (KSAT==0) CYCLE
            IF(MRKOBS(ISAT,IFIL)==' '.AND.
     1         SCKIRC(KSAT).NE.1.AND.SCKIRC(KSAT).NE.2)THEN
              IRFSAT=SVNEP(ISAT)
              KRFSAT=KSAT
              EXIT RefSelLoop
            ENDIF
          ENDDO
        ENDDO RefSelLoop
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
C SET UP NORMAL EQUATION SYSTEM
C -----------------------------
C
C THE SATELLITE CLOCK ERRORS ARE FIRST, FOLLOWED BY THE
C RECEIVER CLOCK ERRORS IN THE NEQ SYSTEM
C -----------------------------------------------------
        NOBS=0
        DO IFIL=1,NFILEP
          IND2=NSATEP+IFIL
          DO ISAT=1,NSFLEP(IFIL)
            IND1=LISTI4(0,MAXSAT,SVNEP,SAFLEP(ISAT,IFIL),NSATEP)
            IF(IND1.EQ.0)CYCLE
            IF(MRKOBS(ISAT,IFIL).EQ.' ')THEN
              NOBS=NOBS+1
C ZENITH DEPENDENT WEIGHTING (COS Z)**2
              WGTPRN(ISAT,IFIL)=(DCOS(ZENDST(ISAT,IFIL))/APRSIC)**2
              BOBS=PRNGTL(ISAT,IFIL)*WGTPRN(ISAT,IFIL)
              BNOR(IND1)=BNOR(IND1)-BOBS
              BNOR(IND2)=BNOR(IND2)+BOBS
C ZENITH DEPENDENT WEIGHTING (COS Z)**2
              II1=IKF(IND1,IND1)
              ANOR(II1)=ANOR(II1)+1.D0*WGTPRN(ISAT,IFIL)
              II2=IKF(IND2,IND2)
              ANOR(II2)=ANOR(II2)+1.D0*WGTPRN(ISAT,IFIL)
              I12=IKF(IND1,IND2)
              ANOR(I12)=ANOR(I12)-1.D0*WGTPRN(ISAT,IFIL)
            ENDIF
          ENDDO
        ENDDO
C REFERENCE CLOCK IS ALWAYS THE FIRST SATELLITE, CLOCK SET TO ZERO
        I0I0=IKF(KRFSAT,KRFSAT)
ccc       i0i0=ikf(nsatep+1,nsatep+1)
        ANOR(I0I0)=ANOR(I0I0)+REFWGT
C TOO FEW PARAMETERS
        IF(NOBS.LT.NPAR)THEN
          IRETRN=1
          GO TO 999
        END IF
C REGULARIZE, IF NO OBSERVATIONS FOR PARAMETER I
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
C ----------------
        CALL SYMINVG(NPAR,ANOR,0,NSING,PARFLG)
        IF(NSING.NE.0)THEN
          WRITE(LFNERR,110)IEP
110       FORMAT(' ### SR ESTCLK: MATRIX SINGULAR, IEPO=',I6)
cc        write(lfnerr,*) 'sats:',SVNEP(1:NSATEP)
cc        write(lfnerr,*)npar,':',parflg(1:npar)
cc        IRETRN=2
cc        GO TO 999
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
C RESIDUALS
C ---------
        RMSOBS=0.D0
        DO IFIL=1,NFILEP
          IND2=NSATEP+IFIL
          DO ISAT=1,NSFLEP(IFIL)
            IND1=LISTI4(0,MAXSAT,SVNEP,SAFLEP(ISAT,IFIL),NSATEP)
            IF(IND1.EQ.0)CYCLE
            IF(MRKOBS(ISAT,IFIL).EQ.' ')THEN
              RESIDU(ISAT,IFIL)=
     1                    PRNGTL(ISAT,IFIL)-(SOL(IND2)-SOL(IND1))
              RMSOBS=RMSOBS+RESIDU(ISAT,IFIL)**2*WGTPRN(ISAT,IFIL)
            ENDIF
          ENDDO
        ENDDO
C
C RMS OF OBSERVATION
C ------------------
        IF(NOBS+ISYM.GT.NPAR)THEN
          RMSOBS=DSQRT(RMSOBS/(NOBS+ISYM-NPAR))
        ELSE
          RMSOBS=1.D0
        ENDIF
C
C CHECK 4*SIGMA
C -------------
        RMSCHK=4*RMSOBS
        IF(RMSCHK.LT.RESMAX)THEN
          RMSCHK=RESMAX
        ENDIF
        IMXSAT=0
        IMXFIL=0
        DO IFIL=1,NFILEP
          DO ISAT=1,NSFLEP(IFIL)
            KSAT=LISTI4(0,MAXSAT,SVNLST,SAFLEP(ISAT,IFIL),SVNNUM)
            IF(KSAT.EQ.0)CYCLE
            KFIL=FILEP(IFIL)
            IF(MRKOBS(ISAT,IFIL).EQ.' ')THEN
C NORMALIZE RESIDUAL AND LOOK IF IT IS .GT. 4*RMSOBS
              RESNOR(ISAT,IFIL)=RESIDU(ISAT,IFIL)*
     1                    DSQRT(WGTPRN(ISAT,IFIL))
C
C SPECIAL GLONASS (BECAUSE FREQ.-BIASES ARE NOT CONSIDERED):
              RMSFAC=1d0
              IF(SAFLEP(ISAT,IFIL).GE.100.AND.
     1           SAFLEP(ISAT,IFIL).LT.199) RMSFAC=2.0d0
C
C SPECIAL GALILEO (BECAUSE INTERSYSTEM BIAS IS NOT CONSIDERED):
              IF(SAFLEP(ISAT,IFIL).GE.200.AND.
     1           SAFLEP(ISAT,IFIL).LT.299) RMSFAC=5.0d0
C
C FIND THE BIGGEST RESIDUAL
              IF (IMXSAT.EQ.0.OR.DABS(RESNOR(ISAT,IFIL)).GT.RMXRES) THEN
                IMXSAT=ISAT
                IMXFIL=IFIL
                KMXSAT=KSAT
                KMXFIL=KFIL
                RMXRES=DABS(RESNOR(ISAT,IFIL))
              ENDIF
C
C IN THE CASE OF A HUGE RMS, MARK ONLY ONE OBSERVATION
              IF (RMSOBS.LE.OMCMXC .AND.
     1            DABS(RESNOR(ISAT,IFIL)).GT.RMSCHK*RMSFAC) THEN
!!!      write(*,*)iep,ITERL,ifil,isat,SAFLEP(ISAT,IFIL),
!!!     1          RESNOR(ISAT,IFIL),'C'
                MRKOBS(ISAT,IFIL)='C'
                NMARK=NMARK+1
                PROSAT(2,KSAT)=PROSAT(2,KSAT)+1
                PROTYP(2,KFIL)=PROTYP(2,KFIL)+1
!!!      else
!!!      write(*,*)iep,ITERL,ifil,isat,SAFLEP(ISAT,IFIL),
!!!     1          RESNOR(ISAT,IFIL),'c'
              ENDIF
            ENDIF
          ENDDO
        ENDDO
C
C REMOVE ONLY THE OBSERVATION WITH THE BIGGEST RESIDUAL
C -----------------------------------------------------
        IF (RMSOBS.GT.OMCMXC.AND.IMXSAT.GT.0) THEN
!!!      write(*,*)iep,ITERL,IMXFIL,IMXSAT,SAFLEP(IMXSAT,IMXFIL),
!!!     1          RESNOR(IMXSAT,IMXFIL),'CC'
          MRKOBS(IMXSAT,IMXFIL)='C'
          NMARK=NMARK+1
          PROSAT(2,KMXSAT)=PROSAT(2,KMXSAT)+1
          PROTYP(2,KMXFIL)=PROTYP(2,KMXFIL)+1
          IF (ITERL.GT.0) ITERL=ITERL-1
        ENDIF
C
C COMPUTE RMS ERRORS OF ESTIMATES
C -------------------------------
        DO IPAR=1,NPAR
          IF (ISING(IPAR).NE.1) THEN
            II=IKF(IPAR,IPAR)
            QQ=DSQRT(ANOR(II))
            IF(IPAR.LE.NSATEP)THEN
              SATCLK(IPAR)=SOL(IPAR)/C
              RMSSAT(IPAR)=QQ
!             RMSSAT(IPAR)=QQ/C
!             RMSSAT(IPAR)=RMSOBS*QQ/C
              IF(NOBSCS(IPAR).LT.MINSTA)THEN
                SATCLK(IPAR)=0.D0
                RMSSAT(IPAR)=0.D0
              ENDIF
            ELSE
              SITCLK(IPAR-NSATEP)=SOL(IPAR)/C
              RMSSIT(IPAR-NSATEP)=QQ
!             RMSSIT(IPAR-NSATEP)=QQ/C
!             RMSSIT(IPAR-NSATEP)=RMSOBS*QQ/C
            ENDIF
          ENDIF
        ENDDO
!       IF(ITERL.LT.MAXITC.AND.RMSOBS.LT.RMSMAX)GO TO 999
        IF (RMSOBS.EQ.RMSOLD) EXIT IterationLoop
        RMSOLD=RMSOBS
      ENDDO IterationLoop
C
C COMPUTE MEAN RMS
C ----------------
      IF(ITER.EQ.NITER)THEN
        DO IFIL=1,NFILEP
          DO ISAT=1,NSFLEP(IFIL)
            KSAT=LISTI4(0,MAXSAT,SVNLST,SAFLEP(ISAT,IFIL),SVNNUM)
            IF(KSAT.EQ.0)CYCLE
            KFIL=FILEP(IFIL)
cc          IF(MRKOBS(ISAT,IFIL).EQ.' '.OR.MRKOBS(ISAT,IFIL).EQ.'C')THEN
            IF(MRKOBS(ISAT,IFIL).EQ.' ')THEN
              RESSVN(KSAT)=RESSVN(KSAT)+RESNOR(ISAT,IFIL)**2
              INDS(KSAT)  =INDS(KSAT)+1
              RESFIL(KFIL)=RESFIL(KFIL)+RESNOR(ISAT,IFIL)**2
              INDC(KFIL)  =INDC(KFIL)+1
            ENDIF
          ENDDO
        ENDDO
      ENDIF
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
