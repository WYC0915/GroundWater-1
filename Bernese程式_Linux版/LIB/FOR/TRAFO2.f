      MODULE s_TRAFO2
      CONTAINS

C*
      SUBROUTINE TRAFO2(NSEFF,SVNEFF,IFIL,ARCNUM,MANOVR,ORBFIL,NRPR,
     1                  LCQTRN,SCLORB,SCLSTO,TBND,NSTCEP,RSW,KI1,
     2                  LI1,MI1,DRI1,DRI1T,DRPR)
CC
CC NAME       : TRAFO2
CC
CC PURPOSE    : COMPUTE RELEVANT MATRICES FOR ORBIT COMBINATION
CC
CC PARAMETERS :
CC        IN  : NSEFF   : NUMBER OF SATELLITES TO BE PROCESSED      I*4
CC              SVNEFF  : SATELLITE NUMBERS
CC              IFIL    : CURRENT FILE NUMBER                       I*4
CC              ARCNUM  : ARRAY CONTAINING THE ARCNUMBERS FOR       I*4(*,*)
CC                        EACH FILE/SATELLITE
CC              MANOVR  : INDICATES WHETHER THE SATELLITE "ISAT"    I*4(*)
CC                        HAD A MANOEUVRE
CC                        MANOVR(ISAT)=0 : NO MANEOUVRE SO FAR
CC                              (ISAT)=KFIL : THERE WAS A MANOEUVRE
CC                                            IN FILE KFIL.
CC              ORBFIL  : FILE NAMES                                R*8
CC              NRPR    : NUMBER OF RPR PARAMETERS                  I*4
CC              LCQTRN  : PARAMETER DESCRIPTION                     I*4(*,*)
CC              SCLORB  : SCALING INFORMATION FOR ORBIT PARMS       R*8(*)
CC              SCLSTO  : SCALING INFORMATION FOR STOCH PARMS       R*8(*)
CC              TBND    : BOUNDARY FOR ARC COMBINATION              R*8
CC              NSTCEP  : NUMBER OF STOCHASTIC PULSES TO BE SET UP  I*4
CC                        AT ARC BOUNDARIES
CC              RSW     : PULSE DEFINITION                          I*4(*)
CC     IN/OUT   KI1     : TRANSFORMATION MATRIX K FOR TRAFO TO      R*8(*)
CC                        REFERENCE ELEMENTS
CC              LI1     : TRANSFORMATION MATRIX L                   R*8(*)
CC              MI1     : TRANSFORMATION MATRIX M                   R*8(*)
CC              DRI1    : TRANSFORMATION MATRIX DRI1                R*8(*)
CC              DRI1T   : TRANSFORMATION MATRIX DRI1 (ARC SPEC RPR)R*8(*)
CC              DRPR    : DIFFERENCE OF RPR PARAMETERS              R*8(*,*)
CC
CC REMARKS    :
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.5  (MAY 94)
CC
CC CREATED    :  94/05/11
CC
CC CHANGES    :  28-SEP-95 : JJ: DECLARE LHLP AS I*4 INSTEAD OF R*8
CC               28-SEP-95 : JJ: DECLARE MHLP AS I*4 INSTEAD OF R*8
CC               09-JAN-96 : GB: NEW ORBIT MODEL IMPLEMENTED.
CC                               REPLACE RVPRTP BY PRTDER
CC                               (FOR NEW ORBIT TYPE)
CC                               USE INFO IN LCQTRN
CC               06-JUN-96 : TS: REMOVED UNUSED VARIABLES
CC               29-JUL-97 : LM: CORRECT DEFINITION OF IFILP
CC               25-SEP-97 : DI: USE MAXSAT.inc;
CC                               USE MAXTST FOR 'MAXSAT','MAXDYN','MAXSTD'
CC               27-AUG-98 : MR: USE FUNCTION "MODSVN"
CC               16-AUG-99 : RD: DIMENSIONS (SMALL/MEDIUM/LARGE) USING I:ADDNEQ
CC               20-Aug-02 : JD: CORRECTION IN CALL OF PRTDER
CC               17-FEB-03 : LM: USE P_ADDOLD
CC               15-MAR-03 : HU: REMOVE UNUSED MAXxxx
CC               10-MAR-04 : HB: CHANGE ORDER OF MODULES
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               16-Jul-08 : RD: USE P_ADDOLD->P_ADDNEQ
CC               01-Oct-08 : DT: ARC-SPECIFIC DYN. PARAMETERS
CC               28-OCT-08 : DT: USE MAXVAR FROM M_MAXDIM
CC               03-DEC-10 : HB: ADD PARAMETER FOR SR PRTDER
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1994     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE p_addneq, ONLY: MAXDYN,MAXSTD, opt
      USE m_maxdim, ONLY: MAXSAT,MAXVAR
      USE d_const,  ONLY: GM
      USE s_prtder
      USE f_modsvn
      USE s_maxtst
      USE s_getorf
      USE s_curarc
      USE s_dminv
      USE s_exitrc
      USE s_rvprtp
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IARC  , ICR   , ICRARC, IF    , IFF   , IFIL  ,
     1          IFILP , II    , IORB  , IORS  , IORSYS, IPAR  , IPAR1 ,
     2          IRC   , IRC1  , IRC2  , IRC3  , IRCRPR, ISAT  , K     ,
     3          KK    , L     , MXCDYN, MXCLCQ, MXCSAT, MXCSTD,
     4          MXCVAR, NRAD1 , NRAD2 , NRPR  , NSEFF , NSTCEP, NUMSAT,
     5          NUMSTD, NVAR1 , NVAR2 , IDYN
C
      REAL*8    DET   , SCALF , T     , TBND  , TOSC  , TOSC0
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C
      CHARACTER*6   MXNSAT,MXNDYN,MXNLCQ,MXNSTD,MXNVAR
C
      CHARACTER*32 ORBFIL(4,*)
      CHARACTER*8  ANLTYP
C
      INTEGER*4    LCQTRN(MXCLCQ,*),SVNEFF(*),RSW(*),
     1             ARCNUM(MXCSAT,*),MANOVR(*)
      INTEGER*4    LHLP(6),MHLP(6)
      INTEGER*4    FILNUM(MAXSTD)
C
      REAL*8       KI1(6,6,MXCSAT,*),LI1(6,MXCDYN,MXCSAT,*),
     1             MI1(6,MXCDYN,MXCSAT,MXCSTD,*),DRI1(6,MXCSAT,*),
     2             DRI1T(6,MXCSAT,*),SCLORB(*),SCLSTO(*)
      REAL*8       DRPR(MXCDYN,MXCSAT,*)
      REAL*8       ELE(7),ELE0(7),ELESAT(7)
      REAL*8       DRDELE(3,MAXVAR),DVDELE(3,MAXVAR),HELP(6),HELP2(6),
     1             RPRPAR(MAXDYN,MAXSAT),RPRPA0(MAXDYN,MAXSAT)
      REAL*8       DRDEL0(3,10),DVDEL0(3,10)
      REAL*8       HI(6,6,MAXSAT),HI1(6,6,MAXSAT),
     1             QI(6,MAXDYN,MAXSAT),QI1(6,MAXDYN,MAXSAT)
      REAL*8       KI1I(6,6,MAXSAT),
     1             LI1I(6,MAXDYN,MAXSAT),DRI1I(6,MAXSAT),
     2             DRI1IT(6,MAXSAT)
      REAL*8       RHOI(6,MAXSAT),RHOI1(6,MAXSAT),HMAT(6,MAXDYN),
     1             HMAT2(6,MAXDYN)
      REAL*8       LI1IT(6,MAXDYN,MAXSAT),LI1I1T(6,MAXDYN,MAXSAT)
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMDYN/MXCDYN,MXNDYN
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
      COMMON/MCMSTD/MXCSTD,MXNSTD
      COMMON/MCMVAR/MXCVAR,MXNVAR
C
C
C CHECK LOCAL DECLARATIONS
C ------------------------
      CALL MAXTST(1,'TRAFO2',MXNSAT,MAXSAT,MXCSAT,IRC1)
      CALL MAXTST(1,'TRAFO2',MXNDYN,MAXDYN,MXCDYN,IRC2)
      CALL MAXTST(1,'TRAFO2',MXNSTD,MAXSTD,MXCSTD,IRC3)
C
      IF(IRC1.NE.0 .OR. IRC2.NE.0 .OR. IRC3.NE.0) CALL EXITRC(2)
C
C DEFINE MAXIMUM NUMBER OF VARIATIONAL EQNS
C -----------------------------------------
      MXCVAR=MAXVAR
      MXNVAR='MAXVAR'
C
C GET SATELLITE POSITIONS FOR CURRENT ARC
C ---------------------------------------
      DO 100 ISAT=1,NSEFF
        CALL CURARC(ISAT,IFIL,ARCNUM,IARC,NUMSTD,FILNUM)
        IF(NUMSTD.LE.1)GO TO 100
        IF(MANOVR(ISAT).EQ.IFIL)THEN
          NUMSAT=SVNEFF(ISAT)
        ELSE
          NUMSAT=MODSVN(SVNEFF(ISAT))
        END IF
        CALL GETORF(ORBFIL(1,IFIL),NUMSAT,0,1,1,TBND,ICRARC,IORSYS,
     1              RHOI1(1,ISAT),TOSC,ELE,IRC)
C
C PARTIAL DERIVATIVES WITH RESPECT TO OSCULATING ELEMENTS
C -------------------------------------------------------
        CALL PRTDER(ORBFIL(2,IFIL),NUMSAT,1,0,1,TBND,1,ICR,IORS,
     1               NVAR1,NRAD1,DRDELE,ELESAT,RPRPAR(1:MAXDYN,ISAT),
     2               ANLTYP,IRCRPR)
        IF(NVAR1.EQ.15)THEN
          DO 15 IORB=1,6
            CALL PRTDER(ORBFIL(2,IFIL),NUMSAT,IORB,1,1,TBND,1,ICR,IORS,
     1                  NVAR1,NRAD1,HELP,ELESAT,RPRPAR(1:MAXDYN,ISAT),
     2                  ANLTYP,IRCRPR)
            DO 14 K=1,3
              DRDELE(K,IORB)=HELP(K)
              DVDELE(K,IORB)=HELP(3+K)
14          CONTINUE
15        CONTINUE
        ELSE
          T=(TBND-TOSC)*86400.D0
          CALL RVPRTP(1,NUMSAT,GM,T,0.D0,ELE(1),ELE(2),ELE(3),
     1                ELE(4),ELE(5),ELE(7),DRDELE,DVDELE)
        END IF
        DO 20 I=1,6
          SCALF=SCLORB(I)
          DO 20 K=1,3
            HI1(K,I,ISAT)=DRDELE(K,I)/SCALF
            HI1(K+3,I,ISAT)=DVDELE(K,I)/SCALF
20      CONTINUE
100   CONTINUE
C
C GET SATELLITE POSITIONS FOR PREVIOUS ARC
C ----------------------------------------
      DO 200 ISAT=1,NSEFF
        CALL CURARC(ISAT,IFIL,ARCNUM,IARC,NUMSTD,FILNUM)
        IF(NUMSTD.LE.1)GO TO 200
        IFILP=FILNUM(NUMSTD-1)
        IF(MANOVR(ISAT).EQ.IFILP)THEN
          NUMSAT=SVNEFF(ISAT)
        ELSE
          NUMSAT=MODSVN(SVNEFF(ISAT))
        END IF
        CALL GETORF(ORBFIL(1,IFILP),NUMSAT,0,1,1,TBND,ICRARC,IORSYS,
     1              RHOI(1,ISAT),TOSC0,ELE0,IRC)
C
C PARTIAL DERIVATIVES WITH RESPECT TO OSCULATING ELEMENTS
C -------------------------------------------------------
        CALL PRTDER(ORBFIL(2,IFILP),NUMSAT,1,0,1,TBND,1,ICR,IORS,
     1               NVAR2,NRAD2,DRDELE,ELESAT,RPRPA0(1:MAXDYN,ISAT),
     2               ANLTYP,IRCRPR)
        IF(NVAR2.EQ.15)THEN
          DO 25 IORB=1,6
            CALL PRTDER(ORBFIL(2,IFILP),NUMSAT,IORB,1,1,TBND,1,ICR,
     1                  IORS,NVAR2,NRAD2,HELP,ELESAT,
     2                  RPRPA0(1:MAXDYN,ISAT),ANLTYP,IRCRPR)
            DO 24 K=1,3
              DRDEL0(K,IORB)=HELP(K)
              DVDEL0(K,IORB)=HELP(3+K)
24          CONTINUE
25        CONTINUE
        ELSE
          T=(TBND-TOSC0)*86400
          CALL RVPRTP(2,NUMSAT,GM,T,0.D0,ELE0(1),ELE0(2),ELE0(3),
     1                ELE0(4),ELE0(5),ELE0(7),DRDEL0,DVDEL0)
        END IF
        DO 120 I=1,6
          SCALF=SCLORB(I)
          DO 120 K=1,3
            HI(K,I,ISAT)=DRDEL0(K,I)/SCALF
            HI(K+3,I,ISAT)=DVDEL0(K,I)/SCALF
120      CONTINUE
200   CONTINUE
C
C PARTIAL DERIVATIVES WITH RESPECT TO RPR-PARAMETERS
C CURRENT ARC
C --------------------------------------------------
      DO 300 ISAT=1,NSEFF
        CALL CURARC(ISAT,IFIL,ARCNUM,IARC,NUMSTD,FILNUM)
        IF(NUMSTD.LE.1)GO TO 300
        IF(MANOVR(ISAT).EQ.IFIL)THEN
          NUMSAT=SVNEFF(ISAT)
        ELSE
          NUMSAT=MODSVN(SVNEFF(ISAT))
        END IF
        DO 290 IPAR=1,NRPR

          IF(opt%splitDyn.EQ.1) THEN
            IDYN=1+(IPAR-1)*NUMSTD
          ELSE
            IDYN=IPAR
          END IF

          IF(NVAR1.EQ.15)THEN
            IPAR1=LCQTRN(4,6+idyn)
            SCALF=SCLORB(IPAR1)
          ELSE
            IPAR1=LCQTRN(4,6+idyn)-6
            SCALF=SCLORB(6+IPAR1)
          END IF
C
          CALL PRTDER(ORBFIL(2,IFIL),NUMSAT,IPAR1,1,1,TBND,1,
     1                ICRARC,IORSYS,NVAR1,NRAD1,HELP,ELESAT,
     2                RPRPAR(1:MAXDYN,ISAT),ANLTYP,IRC)
          DO 280 K=1,3
            QI1(K,IPAR,ISAT)=HELP(K)/SCALF
            QI1(K+3,IPAR,ISAT)=HELP(K+3)/SCALF
280       CONTINUE
290     CONTINUE
300   CONTINUE
C
C PARTIAL DERIVATIVES WITH RESPECT TO RPR-PARAMETERS
C PREVIOUS ARC
C --------------------------------------------------
      DO 400 ISAT=1,NSEFF
        CALL CURARC(ISAT,IFIL,ARCNUM,IARC,NUMSTD,FILNUM)
        IF(NUMSTD.LE.1)GO TO 400
        IFILP=FILNUM(NUMSTD-1)
        IF(MANOVR(ISAT).EQ.IFILP)THEN
          NUMSAT=SVNEFF(ISAT)
        ELSE
          NUMSAT=MODSVN(SVNEFF(ISAT))
        END IF
        DO 390 IPAR=1,NRPR

          IF(opt%splitDyn.EQ.1) THEN
            IDYN=NUMSTD+(IPAR-1)*NUMSTD
          ELSE
            IDYN=IPAR
          END IF
          IF(NVAR2.EQ.15)THEN
            IPAR1=LCQTRN(4,6+idyn)
            SCALF=SCLORB(IPAR1)
          ELSE
            IPAR1=LCQTRN(4,6+idyn)-6
            SCALF=SCLORB(6+IPAR1)
          END IF
          CALL PRTDER(ORBFIL(2,IFILP),NUMSAT,IPAR1,1,1,TBND,1,
     1                ICRARC,IORSYS,NVAR2,NRAD2,HELP,ELESAT,
     2                RPRPA0(1:MAXDYN,ISAT),ANLTYP,IRC)
          DO 380 K=1,3
            QI(K,IPAR,ISAT)=HELP(K)/SCALF
            QI(K+3,IPAR,ISAT)=HELP(K+3)/SCALF
380       CONTINUE
390     CONTINUE
400   CONTINUE
C
C INVERT HI1 :
C ----------
      DO 500 ISAT=1,NSEFF
        CALL CURARC(ISAT,IFIL,ARCNUM,IARC,NUMSTD,FILNUM)
        IF(NUMSTD.GT.1)THEN
          CALL DMINV(HI1(1,1,ISAT),6,DET,LHLP,MHLP)
        END IF
500   CONTINUE
C
C MATRICES KI1I, LI1I, DRI1I
C --------------------------
      DO 600 ISAT=1,NSEFF
        CALL CURARC(ISAT,IFIL,ARCNUM,IARC,NUMSTD,FILNUM)
        IF(NUMSTD.LE.1)GO TO 600
        DO 520 I=1,6
          HELP(I)=RHOI(I,ISAT)-RHOI1(I,ISAT)
          HELP2(I)=RHOI(I,ISAT)-RHOI1(I,ISAT)
          DO 510 K=1,NRPR
            KK=LCQTRN(4,6+K)-6
            SCALF=SCLORB(6+KK)
            HELP(I)=HELP(I)+QI1(I,K,ISAT)*
     1                       (RPRPAR(KK,ISAT)-RPRPA0(KK,ISAT))*SCALF
510        CONTINUE
520     CONTINUE
C
        DO 540 I=1,6
          DRI1I(I,ISAT)=0.D0
          DRI1IT(I,ISAT)=0.D0
          DO 530 K=1,6
            DRI1I(I,ISAT)=DRI1I(I,ISAT)+HI1(I,K,ISAT)*HELP(K)
            DRI1IT(I,ISAT)=DRI1IT(I,ISAT)+HI1(I,K,ISAT)*HELP2(K)
530       CONTINUE
540     CONTINUE
C
        DO 560 I=1,6
          DO 560 K=1,6
            KI1I(I,K,ISAT)=0.D0
            DO 550 L=1,6
              KI1I(I,K,ISAT)=KI1I(I,K,ISAT)+HI1(I,L,ISAT)*HI(L,K,ISAT)
550         CONTINUE
560     CONTINUE
C
        DO 580 I=1,6
          DO 580 K=1,NRPR
            LI1I(I,K,ISAT)=0.D0
            LI1IT(I,K,ISAT)=0.D0
            LI1I1T(I,K,ISAT)=0.D0
            DO 570 L=1,6
              LI1I(I,K,ISAT)=LI1I(I,K,ISAT)+HI1(I,L,ISAT)*
     1                       (QI(L,K,ISAT)-QI1(L,K,ISAT))
              LI1IT(I,K,ISAT)=LI1IT(I,K,ISAT)+HI1(I,L,ISAT)*
     1                        QI(L,K,ISAT)
              LI1I1T(I,K,ISAT)=LI1I1T(I,K,ISAT)+HI1(I,L,ISAT)*
     1                         (-QI1(L,K,ISAT))
570         CONTINUE
580     CONTINUE
600   CONTINUE
C
C COMPUTE MATRICES KI1, LI1, DRI1
C -------------------------------
C
C (A) CASE OF FIRST ARC TO BE ADDED
C     -----------------------------
      DO 800 ISAT=1,NSEFF
        CALL CURARC(ISAT,IFIL,ARCNUM,IARC,NUMSTD,FILNUM)
        IF (NUMSTD.GT.1) IFILP=FILNUM(NUMSTD-1)
C First arc
        IF(NUMSTD.LE.1)THEN
          DO 631 I=1,6
            DRI1(I,ISAT,IFIL)=0.D0
            DRI1T(I,ISAT,IFIL)=0.D0
            DO 611 K=1,6
              IF(I.EQ.K)THEN
                KI1(I,K,ISAT,IFIL)=1.D0
              ELSE
                KI1(I,K,ISAT,IFIL)=0.D0
              END IF
611         CONTINUE
            DO 621 K=1,NRPR
              LI1(I,K,ISAT,IFIL)=0.D0
              MI1(I,K,ISAT,IFIL,IFIL)=0.D0
621         CONTINUE
631       CONTINUE
          DO 641 I=1,NRPR
            DRPR(I,ISAT,IFIL)=0.D0
641       CONTINUE
          GO TO 800
        END IF
C second arc
        IF(NUMSTD.EQ.2)THEN
          DO 630 I=1,6
            DRI1(I,ISAT,IFIL)=DRI1I(I,ISAT)
            DRI1T(I,ISAT,IFIL)=DRI1IT(I,ISAT)
            DO 610 K=1,6
              KI1(I,K,ISAT,IFIL)=KI1I(I,K,ISAT)
610         CONTINUE
            DO 620 K=1,NRPR
              LI1(I,K,ISAT,IFIL)=LI1I(I,K,ISAT)
              MI1(I,K,ISAT,IFIL,IFIL)=LI1I1T(I,K,ISAT)
              MI1(I,K,ISAT,IFILP,IFIL)=LI1IT(I,K,ISAT)
620         CONTINUE
630       CONTINUE
C
          DO 640 I=1,NRPR
            II=LCQTRN(4,6+I)-6
            SCALF=SCLORB(6+II)
            DRPR(I,ISAT,IFIL)=(RPRPA0(II,ISAT)-RPRPAR(II,ISAT))*SCALF
640       CONTINUE
        ELSE
C
C (B) CASE OF SUBSEQUENT ARCS TO BE ADDED
C     -----------------------------------
          DO 720 I=1,6
            HELP(I)=DRI1I(I,ISAT)
            HELP2(I)=DRI1IT(I,ISAT)
            DO 710 L=1,6
              HELP(I) =HELP(I)+ KI1I(I,L,ISAT)*DRI1(L,ISAT,IFILP)
              HELP2(I)=HELP2(I)+KI1I(I,L,ISAT)*DRI1T(L,ISAT,IFILP)
710         CONTINUE
            DO 715 L=1,NRPR
              HELP(I)=HELP(I)+LI1I(I,L,ISAT)*DRPR(L,ISAT,IFILP)
715         CONTINUE
720       CONTINUE
          DO 730 I=1,6
            DRI1(I,ISAT,IFIL)=HELP(I)
            DRI1T(I,ISAT,IFIL)=HELP2(I)
730       CONTINUE
C
          DO 740 I=1,NRPR
            II=LCQTRN(4,6+I)-6
            SCALF=SCLORB(6+II)
            DRPR(I,ISAT,IFIL)=DRPR(I,ISAT,IFILP)+
     1                        (RPRPA0(II,ISAT)-RPRPAR(II,ISAT))*SCALF
740       CONTINUE
C
          DO 760 I=1,6
            DO 760 K=1,6
              HMAT(I,K)=0.D0
              DO 750 L=1,6
                HMAT(I,K)=HMAT(I,K)+KI1I(I,L,ISAT)*KI1(L,K,ISAT,IFILP)
750           CONTINUE
760       CONTINUE
          DO 770 I=1,6
            DO 770 K=1,6
              KI1(I,K,ISAT,IFIL)=HMAT(I,K)
770       CONTINUE
C
          DO 790 I=1,6
            DO 790 K=1,NRPR
              HMAT(I,K)=LI1I(I,K,ISAT)
              MI1(I,K,ISAT,IFIL,IFIL)=LI1I1T(I,K,ISAT)
              HMAT2(I,K)=LI1IT(I,K,ISAT)
              DO 780 L=1,6
                HMAT(I,K)=HMAT(I,K)+KI1I(I,L,ISAT)*
     1                              LI1(L,K,ISAT,IFILP)
                HMAT2(I,K)=HMAT2(I,K)+KI1I(I,L,ISAT)*
     1                              MI1(L,K,ISAT,IFILP,IFILP)
780           CONTINUE
790       CONTINUE
          DO 791 I=1,6
            DO 791 K=1,NRPR
              LI1(I,K,ISAT,IFIL)=HMAT(I,K)
              MI1(I,K,ISAT,IFILP,IFIL)=HMAT2(I,K)
791       CONTINUE
C
          DO 799 IFF=1,NUMSTD-2
            IF=FILNUM(IFF)
            DO 793 I=1,6
              DO 793 K=1,NRPR
                HMAT2(I,K)=0.D0
                DO 792 L=1,6
                  HMAT2(I,K)=HMAT2(I,K)+KI1I(I,L,ISAT)*
     1                                MI1(L,K,ISAT,IF,IFILP)
792           CONTINUE
793         CONTINUE
            DO 794 I=1,6
              DO 794 K=1,NRPR
                MI1(I,K,ISAT,IF,IFIL)=HMAT2(I,K)
794         CONTINUE
799       CONTINUE
        END IF
800   CONTINUE
999   CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
