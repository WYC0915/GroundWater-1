      MODULE s_MODNEQ
      CONTAINS

C*
      SUBROUTINE MODNEQ(MAXPAR,IFIL,ORBFIL,NSFIL,SVNFIL,NSTCEP,RSW,
     1                  SIGMA0,SIGRSW,NUMSTC,SVNSTC,NUMDYN,SVNDYN,
     2                  NUMGAP,SVNGAP,NUMNAC,NEWARC,ELEFIL,RPRFIL,
     3                  NSEFF,SVNEFF,ARCNUM,MANOVR,NPAR,NOBS,LOCQ,
     4                  ANOR,BNOR,RMS,TOSC,ELE,RPRESS,NSTOCH,TSTOCH,
     5                  STOTYP,SVNSTO,STOBND,SCLORB,SCLSTO,KMAT,
     6                  LMAT,MMAT,DRHO,DRHOT,DRPR,ANEW,BNEW,COETRA,
     7                  INDCOE)
CC
CC NAME       :  MODNEQ
CC
CC
CC PURPOSE    :  TRANSFORM THE NEQ-SYSTEM OF ARC IARC TO THE PARAMETERS
CC               USED IN THE PREVIOUS ARC(S), ADD STOCHASTIC PARAMETERS
CC               AT ARC BOUNDARIES.
CC
CC PARAMETERS :
CC        IN  :  IFIL   : CURRENT FILE NUMBER                       I*4
CC               ORBFIL : FILE NAMES                                CH*32(3,*)
CC               NSFIL  : NUMBER OF SATELLITES IN CURRENT FILE      I*4
CC               SVNFIL : SATELLITE NUMBERS IN CURRENT FILE         I*4(*)
CC               NSTCEP : NUMBER OF STOCHASTIC PARAMETERS TO        I*4
CC                        INTRODUCED AT ARC BOUNDARIES
CC               RSW    : PULSE DEFINITION                          I*4(*)
CC               SIGMA0 : MEAN ERROR OF UNIT WEIGHT (A PRIORI)      R*8
CC               SIGRSW : A PRIORI SIGMAS FOR PULSES                R*8(*)
CC               NUMSTC : NUMBER OF SATELLITES FOR WHICH STOCHASTIC I*4
CC                        PARAMETERS HAVE TO BE SET UP AT BOUNDARIES
CC               SVNSTC : CORRESPONDING SATELLITE LIST              I*4(*)
CC               NUMDYN : NUMBER OF SATELLITES FOR WHICH ARC SPEC.  I*4
CC                        DYNAMICAL PARAMETERS HAVE TO BE SET UP
CC               SVNDYN : CORRESPONDING SATELLITE LIST              I*4(*)
CC               NUMGAP : NUMBER OF SATELLITES WITH SPECIAL         I*4
CC                        GAP-HANDLING
CC               SVNGAP : CORRESPONDING SATELLITE NUMBERS           I*4(*)
CC               NUMNAC : NUMBER OF NEW ARCS TO BE SET UP           I*4
CC               NEWARC : DEFINITION OF NEW ARCS                    I*4(*,*)
CC                        NEWARC(1,I) = SVN
CC                        NEWARC(2,I) = FILE NUMBER
CC               ELEFIL : A PRIORI ELEMENTS FOR SATS OF CURR.FILE   R*8(*,*)
CC               RPRFIL : RPR PARMS FOR SATS OF CURRENT FILE        R*8(*,*)
CC    IN/OUT  :  NSEFF  : TOTAL NUMBER OF SATELLITES                I*4
CC               SVNEFF : CORRESPONDING SATELLITE NUMBERS           I*4(*)
CC               ARCNUM : ARRAY CONTAINING THE ARCNUMBERS FOR       I*4(*,*)
CC                        EACH FILE/SATELLITE
CC               MANOVR : INDICATES WHETHER THE SATELLITE "ISAT"    I*4(*)
CC                        HAD A MANOEUVRE
CC                        MANOVR(ISAT)=0 : NO MANEOUVRE SO FAR
CC                              (ISAT)=KFIL : THERE WAS A MANOEUVRE
CC                                            IN FILE KFIL.
CC               NPAR   : NUMBER OF PARAMETERS                      I*4
CC               NOBS   : NUMBER OF OBSERVATIONS                    I*4
CC               LOCQ   : PARAMETER DESCRIPTION                     I*4
CC               ANOR   : NEQ AMTRIX                                R*8(*)
CC               BNOR   : RHS OF NEQ SYSTEM                         R*8(*)
CC               RMS    : SUM OF RES. SQUARES                       R*8
CC               TOSC   : OSCULATION EPOCHS FOR EACH FILE           R*8(*)
CC               ELE    : OSCULATING ELEMENTS                       R*8(*,*,*)
CC               RPRESS : RADIATION PRESSURE PARAMETERS             R*8(*,*)
CC               NSTOCH : NUMBER OF STOCHASTIC PARAMETERS           I*4
CC               TSTOCH : EPOCHS FOR STOCHASTIC PULSES              R*8(*)
CC               STOTYP : TYPE OF STOCHASTIC PULSE                  I*4(*)
CC               SVNSTO : SAT. NUMBER FOR STOCH PULSE I             I*4(*)
CC               STOBND : INDEX WHETHER PULSE I WAS SET UP AT THE   I*4(*)
CC                        ARC BOUNDARY (STOBND(I)=IFIL) OR
CC                        INTERNALLY (STOBND(I)=0)
CC               SCLORB : SCALING FOR ORBIT PARAMETERS              R*8(*)
CC               SCLSTO : SCALING FOR STOCHASTIC ORBIT PARMAMETERS  R*8(*)
CC               KMAT   : K-TRANSFORMATION MATRIX                   R*8(*,*,*,*)
CC               LMAT   : L-TRANSFORMATION MATRIX                   R*8(*,*,*,*)
CC               MMAT   : M-TRANSFORMATION MATRIX                   R*8(*,*,*,*,*)
CC               DRHO   : DRHO TRANSFORMATION MARIX                 R*8(*,*,*)
CC               DRHOT  : DRHOT TRANSFORMATION MARIX                R*8(*,*,*)
CC               ANEW   : AUX. MATRIX (DIM=MAXPAR*MAXPAR)           R*8(*)
CC               BNEW   : AUX. MATRIX (DIM=MAXPAR)                  R*8(*)
CC               COETRA : AUX. MATRIX (DIM=MAXCOE)                  R*8(*)
CC               INDCOE : AUX. INDEX ARRAY FOR COETRA               I*4(*,*)
CC
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.5  (MAY 94)
CC
CC CREATED    :  94/05/11
CC
CC CHANGES    :  20-JUN-95 : MR: CORRECT ERROR MESSAGES
CC               25-JUL-95 : TS: INCREASED MAXPAR FROM 1400 --> 1600
CC               14-JAN-96 : GB: NEW ORBIT MODEL. NEW SR TRAFO4
CC               05-FEB-96 : GB: CORRECT ALGORITHM, IF A SATELLITE
CC                               DOES NOT OCCUR IN FILE IFIL
CC               20-FEB-96 : TS: INCREASED MAXPAR FROM 1600 --> 1700
CC               07-MAR-96 : EB: NEW CALL ARCDEF
CC               18-MAR-96 : EB: INCREASED MAXPAR/MAXLOC FROM 1700 --> 1800
CC               23-JUL-97 : LM: TEST TSTOCH(ISTOCH) ONLY IF ISTOCH /= 0
CC               25-SEP-97 : DI: REMOVE UNUSED PARAMTER 'MAXSAT'
CC               27-AUG-98 : MR: USE FUNCTION "MODSVN"
CC               16-AUG-99 : RD: DIMENSIONS (SMALL/MEDIUM/LARGE) USING "I:ADDNEQ"
CC               30-JAN-02 : CU: NO USE OF SIGRSW
CC               06-FEB-02 : CU: UNCOMMENT REGION: ADD A PRIORI WEIGHT
CC               17-FEB-03 : LM: USE M_ADDOLD
CC               15-MAR-03 : HU: REMOVE UNUSED MAXxxx
CC               27-MAY-03 : CU: UNCOMMENT REDUCTION OF NOBS
CC               10-JUN-03 : MM: DEBUG OUTPUT REMOVED
CC               26-JAN-04 : HU: NO ISAT=0 IN CURARC
CC               15-APR-05 : RD: MANEUVER OF STOCH. PULSES (SVNSTC)
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               16-Jul-08 : RD: USE P_ADDOLD->P_ADDNEQ
CC               24-SEP-08 : DT: ARC-SPECIFIC DYNAMIC ORBIT PARAMETERS (SVNDYN)
CC               06-OCT-11 : RD: DELETE PULSES WITHIN AN INTERVAL
CC               27-MAR-12 : RD: USE TRAFO4 AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1994     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: i4b,r8b,lfnerr
      USE m_maxdim, ONLY: maxbad
      USE m_time,   ONLY: OPERATOR(.isIn.),t_timint
      USE d_satcrx, ONLY: gtsatd
      USE p_addneq, ONLY: MAXLCQ,MAXSTD, opt

      USE s_ctrdef
      USE f_modsvn
      USE s_inlist
      USE s_curarc
      USE s_trafo1
      USE s_trafo2
      USE s_trafo3
      USE s_trafo4
      USE s_exitrc
      USE s_arcdef

      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IA    , IAAA  , IARC  , IELE  , IF    , IFF   , IFIL  ,
     1          IIFIL , IK    , IND   , INDEX , IOK   , IORB  , IPAR  ,
     2          IPAR0 , IPULSE, ISAT  , ISTC  , ISTOCH, ISTTYP, ITYP  ,
     3          IX    , K     , KA    , KORB  , KPAR  , KTYP  , L     ,
     4          MXCDYN, MXCLCQ, MXCPAR, MXCSAT, MXCSTC, MXCSTD, IDEL  ,
     5          MXCSTP, NEWOLD, NFL   , NOBS  , NPAR  , NPNEW , NPSAV ,
     6          NRPR  , NSEFF , NSFIL , NSTCEP, NSTOCH, NUMDYN, NUMGAP,
     7          NUMNAC, NUMSA2, NUMSAT, NUMSTC, NUMSTD, MAXPAR
C
      REAL*8    DD    , RMS   , RR    , SIGMA0, TBND  , XXX
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C COMMON AREAS WITH MAXIMUM DIMENSIONS
      CHARACTER*6  MXNSAT,MXNPAR,MXNLCQ,MXNSTC,MXNDYN,MXNSTD,MXNSTP
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMPAR/MXCPAR,MXNPAR
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
      COMMON/MCMSTC/MXCSTC,MXNSTC
      COMMON/MCMSTP/MXCSTP,MXNSTP
      COMMON/MCMDYN/MXCDYN,MXNDYN
      COMMON/MCMSTD/MXCSTD,MXNSTD
C
      CHARACTER*32 ORBFIL(4,*)
      INTEGER*4    SVNEFF(*),RSW(*),LOCQ(MXCLCQ,*),STOTYP(*),
     1             SVNSTO(*),STOBND(*),SVNSTC(*),SVNDYN(*),INDCOE(2,*),
     2             MANOVR(*),SVNGAP(*),NEWARC(2,*),SVNFIL(*),
     3             ARCNUM(MXCSAT,*)
      REAL*8       SIGRSW(*),ANOR(*),BNOR(*),ANEW(*),BNEW(*),TOSC(*),
     1             SCLORB(*),SCLSTO(*)
      REAL*8       ELE(7,MXCSAT,*),RPRESS(MXCDYN,MXCSAT,*),TSTOCH(*)
      REAL*8       ELEFIL(7,*),RPRFIL(MXCDYN,*)
      REAL*8       KMAT(6,6,MXCSAT,*),LMAT(6,MXCDYN,MXCSAT,*),
     1             MMAT(6,MXCDYN,MXCSAT,MXCSTD,*),DRHO(6,MXCSAT,*),
     2             DRHOT(6,MXCSAT,*)
      REAL*8       DRPR(MXCDYN,MXCSAT,*),COETRA(*)
C
C LOCAL DECLARATIONS
      INTEGER*4    LCQTRN(MAXLCQ,MAXPAR)
      INTEGER*4    INDIPA(MAXPAR),FILNUM(MAXSTD),FILACT(MAXSTD)
      REAL*8       DELTA(MAXPAR),ELEPAR(8)
C
      INTEGER(i4b)                      :: nDel
      INTEGER(i4b),   DIMENSION(maxbad) :: plsdel
      TYPE(t_timint), DIMENSION(maxbad) :: timdel
C
C READ SOME EVENTS FROM SATCRUX FIRST
C -----------------------------------
      CALL gtsatd(maxbad,opt%satcrux,ndel,plsdel,timdel)
C
C CHECK LOCAL DECLARATIONS
C ------------------------
      IF(MAXPAR.LT.MXCPAR)THEN
        WRITE(LFNERR,11)MAXPAR,MXCPAR
11      FORMAT(//,' ** SR MODNEQ : LOCAL # PAR TOO SMALL:',I5,I5//)
        CALL EXITRC(2)
      END IF
      IF(MAXLCQ.NE.MXCLCQ)THEN
        WRITE(LFNERR,12)MAXLCQ
12      FORMAT(//,' ** SR MODNEQ : LOCAL DIM. MAXLCQ WRONG :',I3,//)
        CALL EXITRC(2)
      END IF
      IF(MAXSTD.LT.MXCSTD)THEN
        WRITE(LFNERR,13)MAXSTD
13      FORMAT(//,' ** SR MODNEQ : LOCAL DIM. MAXSTD TOO SMALL :',I3,//)
        CALL EXITRC(2)
      END IF
C
C CREATE/UPDATE ARRAY ARCNUM, ELEFIL, RPRFIL
C ------------------------------------------
      IF(IFIL.EQ.1)NSEFF=0
      CALL ARCDEF(NPAR,LOCQ,IFIL,NSFIL,SVNFIL,NUMSTC,SVNSTC,
     1            NUMGAP,SVNGAP,ELEFIL,RPRFIL,TOSC(IFIL),NUMNAC,NEWARC,
     2            NSEFF,SVNEFF,ARCNUM,MANOVR,ELE(1,1,IFIL),
     3            RPRESS(1,1,IFIL))
C
C Arc-specific dynamical parameters
C ---------------------------------
      IF(opt%splitDyn.EQ.1) THEN
        NUMDYN = NSEFF
        DO 10 ISAT=1,NSEFF
          SVNDYN(ISAT) = SVNEFF(ISAT)
10      CONTINUE
      END IF
C
C UPDATE LIST OF STOCHASTIC PULSES WITH PULSES SET UP AT THE ARC BOUNDARY
C (SATELLITE HAS TO BE AVAILABLE IN FILE "IFIL", IT CANNOT BE THE START
C OF THE ARC)
C -----------------------------------------------------------------------
      DO 20 ISAT=1,NSEFF
        CALL INLIST(SVNEFF(ISAT),NUMSTC,SVNSTC,IELE)
        CALL CURARC(ISAT,IFIL,ARCNUM,IARC,NUMSTD,FILNUM)
        IF(IELE.NE.0.AND.NUMSTD.NE.0.AND.IFIL.NE.FILNUM(1))THEN
          DO IDEL=1,NDEL
            IF ( (SVNEFF(ISAT).EQ. PLSDEL(IDEL)) .AND.
     1           (TOSC(IFIL) .ISIN. TIMDEL(IDEL)) ) GOTO 20
          ENDDO
          DO 15 IPULSE=1,NSTCEP
            NSTOCH=NSTOCH+1
            IF (NSTOCH.GT.MXCSTC*MXCSTP) THEN
              WRITE(LFNERR,900) NSTOCH, MXCSTC*MXCSTP
900           FORMAT(/,' *** SR MODNEQ: ERROR:',/,
     1          16X,'NUMBER OF STOCHASTIC PARAMETERS TOO LARGE!',/
     2          16X,'NUMBER OF STOCHASTIC LARGER THAN: ',I4,/,
     2          16X,'MAX.NUMBER OF STOCH. PARAMETERS : ',I4,/,
     3          16X,'INCREASE PARAMETER MAXSTC OR/AND MAXSTP ',
     4              'IN MAIN PROGRAM',/)
              CALL EXITRC(2)
            ENDIF
            STOTYP(NSTOCH)=RSW(IPULSE)
            TSTOCH(NSTOCH)=TOSC(IFIL)
            SVNSTO(NSTOCH)=SVNEFF(ISAT)
            STOBND(NSTOCH)=IFIL
15        CONTINUE
        END IF
20    CONTINUE
C
C SET UP LOCQ FOR TRANSFORMED NEQ SYSTEM
C --------------------------------------
      NPNEW=0
      NRPR=0
      DO 100 IPAR=1,NPAR
        NPNEW=NPNEW+1
        IF(NPNEW.GT.MAXPAR)THEN
          WRITE(LFNERR,21)MAXPAR
21        FORMAT(//,' ** SR MODNEQ : NPNEW EXCEEDS MAXPAR=',I3,//)
          CALL EXITRC(2)
        END IF
        IF(NRPR.EQ.0.AND.LOCQ(1,IPAR).EQ.3.AND.LOCQ(4,IPAR).EQ.1)THEN
          NRPR=LOCQ(5,IPAR)-6
          IPAR0=NPNEW
        END IF
        DO 30 K=1,MXCLCQ
          LCQTRN(K,NPNEW)=LOCQ(K,IPAR)
30      CONTINUE
        NPSAV=NPNEW
C
C ADD DYNAMICAL PARAMETERS FROM PREVIOUS ARCS (FOR IELE NE 0 ONLY)
        CALL INLIST(LOCQ(3,IPAR),NSEFF,SVNEFF,ISAT)
        IF (ISAT.GT.0) THEN
          CALL CURARC(ISAT,IFIL,ARCNUM,IARC,NUMSTD,FILNUM)
        ENDIF
        IF(LOCQ(1,IPAR).EQ.3.AND.
     1     LOCQ(4,IPAR).GT.6.AND.
     2     LOCQ(2,IPAR).EQ.IARC)THEN
          CALL INLIST(LOCQ(3,IPAR),NUMDYN,SVNDYN,IELE)
          IF(IELE.NE.0)THEN
            DO 39 IFF=1,NUMSTD
              IF=FILNUM(IFF)
              IF(IF.LT.IFIL)THEN
                NPNEW=NPNEW+1
                IF(NPNEW.GT.MAXPAR)THEN
                  WRITE(LFNERR,21)MAXPAR
                  CALL EXITRC(2)
                END IF
                DO 31 K=1,MXCLCQ
                  LCQTRN(K,NPNEW)=LOCQ(K,IPAR)
31              CONTINUE
                LCQTRN(7,NPNEW)=IF
C
C Sub-Arcs: further increase locq(6)
C Asumption: identical number of RPR per STD orbit
                IF (opt%splitDyn.EQ.1) THEN
                  LCQTRN(6,NPNEW)=LOCQ(6,IPAR)+NRPR*(IF-1)
                END IF
C
              ELSE
                LCQTRN(7,NPSAV)=IFIL
                LOCQ(7,IPAR)=IFIL
C
C Sub-Arcs: further increase locq(6)
C Asumption: identical number of RPR per STD orbit
                IF (opt%splitDyn.EQ.1) THEN
                  LCQTRN(6,NPSAV)=LOCQ(6,IPAR)+NRPR*(IF-1)
                  LOCQ(6,IPAR)=LOCQ(6,IPAR)+NRPR*(IF-1)
C
CCC ??????????????????????????
CC Locq(2) noch hochzählen???
CC dann aber Probleme in ELETRN mit IF-Abfrage locq(2)==IARC !!!
CC                locq(2,ipar)=IFIL
CC                lcqtrn(2,NPSAV)=IFIL
C
                END IF
C
              END IF
39          CONTINUE
          ELSE
            LCQTRN(7,NPSAV)=FILNUM(1)
            LOCQ(7,IPAR)=FILNUM(1)
          END IF
        END IF
C
C ADD "OLD" STOCHASTIC PULSES
        IF(LOCQ(6,IPAR).NE.0)THEN
          IX=6
        ELSE
          IX=4
        END IF
CCC  change to 4 in all cases ????????????
CCC        IX = 4
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        IF(LOCQ(1,IPAR).EQ.3.AND.
     1     LOCQ(IX,IPAR).EQ.LOCQ(5,IPAR).AND.
     2     LOCQ(2,IPAR).EQ.IARC)THEN
          DO 40 ISTOCH=1,NSTOCH
            CALL INLIST(LOCQ(3,IPAR),NSEFF,SVNEFF,ISAT)
            CALL CURARC(ISAT,IFIL,ARCNUM,IARC,NUMSTD,FILNUM)
            IF(SVNSTO(ISTOCH).EQ.LOCQ(3,IPAR).AND.
     1        NUMSTD.NE.0.AND.
     2        STOBND(ISTOCH).NE.-1.AND.
     3        TSTOCH(ISTOCH).GE.TOSC(FILNUM(1))-1.D0/1440.D0)THEN
C
              NPNEW=NPNEW+1

              IF(NPNEW.GT.MAXPAR)THEN
                WRITE(LFNERR,21)MAXPAR
                CALL EXITRC(2)
              END IF
              LCQTRN(1,NPNEW)=11
              LCQTRN(2,NPNEW)=IARC
              LCQTRN(3,NPNEW)=SVNSTO(ISTOCH)
              LCQTRN(4,NPNEW)=ISTOCH
              LCQTRN(5,NPNEW)=STOTYP(ISTOCH)
              LCQTRN(6,NPNEW)=1
              LCQTRN(7,NPNEW)=STOBND(ISTOCH)
            END IF
40        CONTINUE
        END IF
100   CONTINUE
C
C DEFINE TRANSFORMATION MATRIX FROM "OLD" TO "EXPANDED" PARAMETER SET
C "EXPANDED" : ADD STOCHASTIC PARAMETERS NOT ALREADY IN CURRENT ARC
C -------------------------------------------------------------------
      INDEX=0
      DO 200 IPAR=1,NPAR
        DO 190 KPAR=1,NPNEW
          IND=IPAR+(KPAR-1)*NPAR
          IF((LOCQ(1,IPAR).EQ.3.AND.LOCQ(4,IPAR).LE.6).
     1       AND.(LCQTRN(1,KPAR).EQ.11.AND.
     2       LOCQ(3,IPAR).EQ.LCQTRN(3,KPAR).AND.
     3       LCQTRN(7,KPAR).NE.-1))THEN
            ISTOCH=LCQTRN(4,KPAR)
            ISTTYP=LCQTRN(5,KPAR)
            IORB  =LOCQ(4,IPAR)
C
            CALL INLIST(LOCQ(3,IPAR),NSEFF,SVNEFF,ISAT)
            IF(ISAT.EQ.0)THEN
              WRITE(LFNERR,55)LOCQ(3,IPAR)
55            FORMAT(//,' SR MODNEQ : SATELLITE ',I2,' NOT FOUND',//)
              CALL EXITRC(2)
            END IF
            CALL INLIST(MODSVN(SVNEFF(ISAT)),NSEFF,SVNEFF,ISAT)
C
            CALL CURARC(ISAT,IFIL,ARCNUM,IAAA,NFL,FILACT)
C
C COMPUTE PARTIALS W.R.T. SPECIFIED STOCHASTIC PULSE
C --------------------------------------------------
            IF(NFL.NE.0)THEN
              CALL TRAFO4(1,IFIL,ISTOCH,ISTTYP,TSTOCH(ISTOCH),
     1                    TOSC(IFIL),SVNEFF(ISAT),ELE(1,ISAT,IFIL),
     2                    ORBFIL,NFL,FILACT,NSEFF,SVNEFF,TOSC,
     3                    NEWOLD,ELEPAR)
C
C DEFINE TRANSFORMATION MATRIX
C ----------------------------
              XXX=ELEPAR(IORB)*SCLORB(IORB)/SCLSTO(ISTTYP)
              CALL CTRDEF(IPAR,KPAR,XXX,INDEX,INDCOE,INDIPA,COETRA)
            END IF
          ELSE
            IOK=1
            DO 110 K=1,MXCLCQ
              IF(LOCQ(K,IPAR).NE.LCQTRN(K,KPAR))IOK=0
110         CONTINUE
            XXX=IOK
            CALL CTRDEF(IPAR,KPAR,XXX,INDEX,INDCOE,INDIPA,COETRA)
          END IF
190     CONTINUE
200   CONTINUE
C
C TRANSFORMATION FROM OLD TO "EXPANDED" PARAMETER SET
C ---------------------------------------------------
      CALL TRAFO1(NPAR,NPNEW,INDCOE,COETRA,ANOR,BNOR,ANEW,BNEW)
C
C SET UP TRANSFORMATION FROM "EXPANDED" TO "NEW" PARAMETER SET
C "NEW" = TRANSFORMATION TO OSCULATING ELEMENTS OF REFERENCE ARC
C --------------------------------------------------------------
      TBND=TOSC(IFIL)
      CALL TRAFO2(NSEFF,SVNEFF,IFIL,ARCNUM,MANOVR,ORBFIL,NRPR,
     1            LCQTRN(1,IPAR0),SCLORB,SCLSTO,TBND,NSTCEP,RSW,
     2            KMAT,LMAT,MMAT,DRHO,DRHOT,DRPR)
C
C TRANSFORMATION MATRICES
C -----------------------
      INDEX=0
      DO 400 IPAR=1,NPNEW
        ITYP =LCQTRN(1,IPAR)
        IIFIL=LCQTRN(7,IPAR)
        IA=LCQTRN(2,IPAR)
        IF(ITYP.NE.3)THEN
          DELTA(IPAR)=0.D0
        ELSE
          CALL INLIST(LCQTRN(3,IPAR),NUMDYN,SVNDYN,IELE)
          NUMSAT=LCQTRN(3,IPAR)
          CALL INLIST(NUMSAT,NSEFF,SVNEFF,ISAT)
          IF(ISAT.EQ.0)THEN
            WRITE(LFNERR,315)NUMSAT
315         FORMAT(//,' ** SR MODNEQ : SATELLITE',I3,' NOT IN LIST',//)
            CALL EXITRC(2)
          END IF
C
          IF(LCQTRN(6,IPAR).NE.0)THEN
            IORB=LCQTRN(6,IPAR)
          ELSE
            IORB=LCQTRN(4,IPAR)
          END IF
C
          IF(LCQTRN(4,IPAR).LE.6)THEN
            IF(IELE.EQ.0)THEN
              DELTA(IPAR)=DRHO(IORB,ISAT,IFIL)
            ELSE
              DELTA(IPAR)=DRHOT(IORB,ISAT,IFIL)
            END IF
          ELSE
            IF(IELE.EQ.0)THEN
              DELTA(IPAR)=DRPR(IORB-6,ISAT,IFIL)
            ELSE
              DELTA(IPAR)=0
            END IF
          END IF
        END IF
C
        DO 390 KPAR=1,NPNEW
          IK=IPAR+(KPAR-1)*NPNEW
          IF(IPAR.NE.KPAR)THEN
            DD=0.D0
          ELSE
            DD=1.D0
          END IF
C
C PARAMETER NE ORBIT PARAMETER, SATELLITE NUMBER DO NOT MATCH
          KTYP=LCQTRN(1,KPAR)
          NUMSA2=LCQTRN(3,KPAR)
          KA=LCQTRN(2,KPAR)
          IF(ITYP.NE.3.OR.KTYP.NE.3)THEN
            CALL CTRDEF(IPAR,KPAR,DD,INDEX,INDCOE,INDIPA,COETRA)
            GO TO 390
          END IF
          IF(NUMSAT.NE.NUMSA2)THEN
            CALL CTRDEF(IPAR,KPAR,DD,INDEX,INDCOE,INDIPA,COETRA)
            GO TO 390
          END IF
C
C LINE CORRESPONDS TO A DYNAMICAL PARAMETER
          IF(LCQTRN(4,IPAR).GT.6)THEN
            CALL CTRDEF(IPAR,KPAR,DD,INDEX,INDCOE,INDIPA,COETRA)
            GO TO 390
          END IF
C
C NUMBERS MATCH, ARCS DO NOT
          IF(IA.NE.KA)THEN
            CALL CTRDEF(IPAR,KPAR,DD,INDEX,INDCOE,INDIPA,COETRA)
            GO TO 390
          END IF
C
C ORBIT PARAMETERS, NUMBERS MATCH
          IF(LCQTRN(6,KPAR).NE.0)THEN
            KORB=LCQTRN(6,KPAR)
C
C Sub-Arcs
            IF (opt%splitDyn.EQ.1 .AND. LCQTRN(4,KPAR).GT.6) THEN
              KORB=LCQTRN(6,KPAR)-NRPR*(LCQTRN(7,KPAR)-1)
            END IF
C
C "OLD" ORBIT MODEL
          ELSE
            KORB=LCQTRN(4,KPAR)
          END IF
C
          IF(LCQTRN(4,KPAR).LE.6)THEN
            XXX=KMAT(IORB,KORB,ISAT,IFIL)
            CALL CTRDEF(IPAR,KPAR,XXX,INDEX,INDCOE,INDIPA,COETRA)
          ELSE
            IF(IELE.EQ.0)THEN
              XXX=LMAT(IORB,KORB-6,ISAT,IFIL)
              CALL CTRDEF(IPAR,KPAR,XXX,INDEX,INDCOE,INDIPA,COETRA)
            ELSE
              IF=LCQTRN(7,KPAR)
              XXX=MMAT(IORB,KORB-6,ISAT,IF,IFIL)
              CALL CTRDEF(IPAR,KPAR,XXX,INDEX,INDCOE,INDIPA,COETRA)
            END IF
          END IF
390     CONTINUE
400   CONTINUE
C
C PERFORM TRANSFORMATION
C ----------------------
      CALL TRAFO3(NPNEW,INDCOE,COETRA,DELTA,ANOR,BNOR,
     1            RMS,ANEW,BNEW)
cu      NOBS=NOBS+(NPNEW-NPAR)
      NPAR=NPNEW
C
      RR=SIGMA0
C
C GENERATE NEW LOCQ :
C -----------------
      DO 500 IPAR=1,NPAR
        DO 490 L=1,MXCLCQ
          LOCQ(L,IPAR)=LCQTRN(L,IPAR)
490     CONTINUE
C
C ADD A PRIORI WEIGHT TO NEQ-MATRIX FOR NEW STOCHASTIC PULSES
ccccc        ISTOCH=LOCQ(4,IPAR)
ccccc        IF ( LOCQ(1,IPAR).EQ.11) THEN
ccccc          IF (TSTOCH(ISTOCH).EQ.TBND) THEN
ccccc            ISTO=LOCQ(5,IPAR)
ccccc            IF(SIGRSW(ISTO).GT.0.D0.AND.SCLSTO(ISTO).GT.0.D0)THEN
ccccc              II=IKF(IPAR,IPAR)
CCC              ANOR(II)=ANOR(II)+(SIGMA0/SIGRSW(ISTO)/SCLSTO(ISTO))**2
ccccc            END IF
ccccc          END IF
ccccc        END IF
500   CONTINUE
C
C REDEFINE "NEW" STOCHASTIC PULSES IN THE ARC INTERIOR AS "OLD" ONES
      DO 510 ISTC=1,NSTOCH
        IF(STOBND(ISTC).EQ.-1)THEN
          STOBND(ISTC)=0
        END IF
510   CONTINUE
C
C REDEFINE "NEW" STOCHASTIC PULSES IN THE ARC INTERIOR AS "OLD" ONES
C (IN LOCQ)
      DO 520 IPAR=1,NPAR
        IF(LOCQ(1,IPAR).EQ.11.AND.LOCQ(7,IPAR).EQ.-1)THEN
          LOCQ(7,IPAR)=0
        END IF
520   CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
