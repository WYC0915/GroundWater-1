      MODULE s_TRAFO4
      CONTAINS

c*
      SUBROUTINE TRAFO4(IWT,IFIL,ISTOCH,ISTTYP,TSTOCH,TACT,SVN,
     1                  ELEACT,ORBFIL,NFL,FILNUM,NSEFF,SVNEFF,
     2                  TOSC,NEWOLD,ELEPAR)
CC
CC NAME       :  TRAFO4
CC
CC PURPOSE    : FOR IWT=1: COMPUTE AND STORE PARTIAL DERIVATIVES OF
CC                         ORBITAL ELEMENTS W.R.T. STOCHASTIC PULSE
CC                         CHARACTERIZED BY ISTTYP
CC                         THE COMPUTATION IS DIFFERENT FOR DIFFERENT
CC                         RPR-MODELS (NEW ORBIT MODEL IS MORE ACCURATE)
CC              FOR IWT NEQ 1: RETRIEVE THE SAME VALUES STORED IN
CC                             INTERNAL ARRAY ELESTO.
CC                             IN THIS MODE ISTTYP, TSTOCH, TACT, SVN,
CC                             ELE, AND ORBFIL ARE NOT USED
CC
CC PARAMETERS :
CC        IN  : IWT     : MODE SELECTION (SEE ABOVE)                I*4
CC              IFIL    : FILE NUMBER                               I*4
CC              ISTOCH  : INDEX OF STOCHASTIC PULSE                 I*4
CC              ISTTYP  : TYPE OF STOCHASTIC PULSE                  I*4
CC              TSTOCH  : TIME OF STOCHASTIC PULSE                  R*8
CC              TACT    : TIME ARGUMENT FOR COMPUTATION OF PARTIAL  R*8
CC              SVN     : SATELLITE NUMBER                          I*4
CC              ELEACT  : ELEMENTS REFERING TO FILE IFIL            R*8
CC              ORBFIL  : FILE NAMES FOR ORBITS/RPRS ETC            CH*32
CC              NFIL    : NUMBER OF FILES (ARCS) INVOLVED           I*4
CC              FILNUM  : CORRESPONDING FILE NUMBERS                I*4
CC              NSEFF   : TOTAL NUMBER OF "ACTIVE" SATELLITES       I*4
CC              SVNEFF  : CORRESPONDING SV-NUMBERS                  I*4
CC        OUT : NEWOLD  : TYPE OF RPR-FILES                         R*8
CC              ELEPAR  : PARTIAL DERIVATIVES OF OSCULATING ELE-    R*8
CC                        MENTS AT TIME TACT W.R.T. STOCHASTIC
CC                        PULSE
CC
CC REMARKS    :
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  4.0  (JAN 96)
CC
CC CREATED    :  96/01/14
CC
CC CHANGES    :  05-JUN-96 : LM: DATA STATEMENT OUT OF ORDER
CC               16-AUG-99 : RD: DIMENSIONS (SMALL/MEDIUM/LARGE) USING I:ADDNEQ
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               15-MAR-03 : HU: REMOVE UNUSED MAXxxx
CC               10-MAR-04 : HB: CHANGE ORDER OF MODULES
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               02-DEC-05 : RD: BUGFIX IF NO PARTIALS FOR THE REQUEST
CC               30-MAY-07 : AG: USE s_suneff
CC               16-Jul-08 : RD: USE P_ADDOLD->P_ADDNEQ
CC               10-FEB-10 : HB: BUG FIXED FOR DYX-PULSES
CC               01-OCT-10 : CR: NEW CALL OF SUNEFF
CC               27-MAR-12 : RD: USE TRAFO4 AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1996     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE p_addneq, ONLY: MAXSTD,MAXSTP
      USE m_maxdim, ONLY: MAXSTC
      USE d_const, ONLY: GM
      USE s_compz0
      USE s_vprod
      USE s_maxtst
      USE s_getorf
      USE s_dminv
      USE s_suneff
      USE s_eleprt
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , ICRARC, IF    , IFIL  , IFIL0 , IFIRST, IFL   ,
     1          IFLOLD, IND   , IORSYS, IPAR  , IRC   , IRCC  , IRCF  ,
     2          IRCP  , ISTOCH, ISTTYP, IWT   , K     , MXCSTC, MXCSTD,
     3          MXCSTP, MXCVAR, NEWLOC, NEWOLD, NFL   , NSEFF , NSTOCH
C
      REAL*8    DET   , DEW   , DEY   , RSAT  , RSUN  , TACT  , TDT   ,
     1          TOSC1 , TSEC  , TSTOCH, TTT
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      INTEGER*4    SVN,FILNUM(*),SVNEFF(*)
      REAL*8       ELEACT(*),ELEPAR(*),TOSC(*)
      CHARACTER*6  MXNSTP,MXNSTC,MXNSTD,MXNVAR
      CHARACTER*32 ORBFIL(4,*)
C
C LOCAL DIMENSIONS
      INTEGER*4    L1(6),M1(6),FRCLOC(MAXSTC*MAXSTP,MAXSTD)
      INTEGER*4    SVNLOC(MAXSTC*MAXSTP,MAXSTD)
      REAL*8       ELESTO(6,MAXSTD,MAXSTC*MAXSTP),
     1             TSTCLC(MAXSTC*MAXSTP,MAXSTD)
      REAL*8       ELESAT(7),HELP(6),XVSAT(6),ELEP(8)
      REAL*8       XSUN(4),ESUN(3),ER(3),ES(3),EW(3),EX(3),EY(3),DUM3(3)
      REAL*8       Z0(6,6)
C
      COMMON/MCMSTC/MXCSTC,MXNSTC
      COMMON/MCMSTP/MXCSTP,MXNSTP
      COMMON/MCMSTD/MXCSTD,MXNSTD
      COMMON/MCMVAR/MXCVAR,MXNVAR
C
      DATA IFIRST/1/,IFLOLD/1/
C
C CHECK MAXIMUM DIMENSIONS
C ------------------------
      IF(IFIRST.EQ.1)THEN
        IFIRST=0
        CALL MAXTST(0,'TRAFO4',MXNSTP,MAXSTP,MXCSTP,IRCP)
        CALL MAXTST(0,'TRAFO4',MXNSTC,MAXSTC,MXCSTC,IRCC)
        CALL MAXTST(1,'TRAFO4',MXNSTD,MAXSTD,MXCSTD,IRCF)
        IF(IRCP.NE.0.OR.IRCC.NE.0.OR.IRCF.NE.0)CALL EXITRC(2)
        NSTOCH=MAXSTP*MAXSTC
        DO 1 IFL=1,MAXSTD
        DO 1 K=1,NSTOCH
          FRCLOC(K,IFL)=0
          SVNLOC(K,IFL)=0
          TSTCLC(K,IFL)=0.D0
1       CONTINUE
      END IF
C
C COMPUTE PARTIALS
C ----------------
      IF(IWT.EQ.1)THEN
        IF(SVNLOC(ISTOCH,IFIL).EQ.0)THEN
          IFLOLD=IFIL
C
C FIND OUT FILE IN WHICH THE STOCHASTIC PULSE OCCURED
C ---------------------------------------------------
CCC          IF(SVN.EQ.28)WRITE(*,7777)svn,ifil,TSTOCH,TACT,NFL,
CCC     1               (FILNUM(K),K=1,NFL)
CCC7777      FORMAT(' SVN,IFIL,TSTOCH,TACT=',2I3,2F15.3,' NFL,FILNUM=',
CCC     2           10I3)
          DO 10 IFL=1,NFL-1
            IF(TSTOCH.LT.TOSC(FILNUM(IFL+1))-1.D-4)THEN
              IFIL0=IFL
              GO TO 11
            END IF
10        CONTINUE
          IFIL0=NFL
11        CONTINUE
C
C FIND OUT FILE TYPE (NEW/OLD)
C ----------------------------
          IND=FILNUM(IFIL0)
          CALL COMPZ0(NSEFF,SVNEFF,SVN,TSTOCH,ORBFIL(2,IND),IND,Z0)
C
C FILE TYPE:
          IF(Z0(1,1).NE.0.D0)THEN
            NEWLOC=1
          ELSE
            NEWLOC=0
          END IF
C
C COMPUTATION FOR NEW ORBIT MODEL
C -------------------------------
          IF(NEWLOC.EQ.1)THEN
C
C COMPUTE INITIAL VALUE FOR SPECIFIED STOCHASTIC PARAMETER
C --------------------------------------------------------
            CALL GETORF(ORBFIL(1,IND),SVN,0,1,1,TSTOCH,ICRARC,
     1                  IORSYS,XVSAT,TOSC1,ELESAT,IRC)
C
C COMPUTE RELEVANT UNIT VECTORS (UVs):
C ER: UV IN RADIAL DIRECTION
C EW: UV NORMAL TO ORBITAL PLANE
C ES: UV IN S-DIRECTION (ES=EW*ER)
C ESUN: UV IN DIRECTION TO THE SUN
C EY: UV IN SPACE VEHICLE'S Y-DIRECTION
C EX: UV IN SPACE VEHICLE'S X-DIRECTION
C -------------------------------------
            IF(ISTTYP.GT.3)THEN
              TDT=TSTOCH+(19.D0+32.184D0)/86400.D0
              CALL SUNEFF(IORSYS,2.D0,TDT,XSUN,DUM3)
              RSUN=DSQRT((XVSAT(1)-XSUN(1))**2+(XVSAT(2)-XSUN(2))**2+
     1                   (XVSAT(3)-XSUN(3))**2)
            END IF
            RSAT=DSQRT(XVSAT(1)**2+XVSAT(2)**2+XVSAT(3)**2)
            DO 20 K=1,3
              ER(K)=XVSAT(K)/RSAT
              IF(ISTTYP.GT.3)ESUN(K)=(XSUN(K)-XVSAT(K))/RSUN
20          CONTINUE
            IF(ISTTYP.GT.3)THEN
              CALL VPROD(ER,ESUN,EY)
              DEY=DSQRT(EY(1)**2+EY(2)**2+EY(3)**2)
            END IF
            CALL VPROD(XVSAT,XVSAT(4),EW)
            DEW=DSQRT(EW(1)**2+EW(2)**2+EW(3)**2)
            DO 30 K=1,3
              IF(ISTTYP.GT.3)EY(K)=EY(K)/DEY
              EW(K)=EW(K)/DEW
30          CONTINUE
            CALL VPROD(EW,ER,ES)
            IF(ISTTYP.GT.3)CALL VPROD(ESUN,EY,EX)
            DO 40 K=1,3
              HELP(K)=0.D0
40          CONTINUE
C
            IF(ISTTYP.EQ.1)THEN
              DO 50 K=1,3
                HELP(K+3)=ER(K)
50            CONTINUE
            ELSE IF(ISTTYP.EQ.2)THEN
              DO 60 K=1,3
                HELP(K+3)=ES(K)
60            CONTINUE
            ELSE IF(ISTTYP.EQ.3)THEN
              DO 70 K=1,3
                HELP(K+3)=EW(K)
70            CONTINUE
            ELSE IF(ISTTYP.EQ.4)THEN
              DO 80 K=1,3
                HELP(K+3)=ESUN(K)
80            CONTINUE
            ELSE IF(ISTTYP.EQ.5)THEN
              DO 90 K=1,3
                HELP(K+3)=EY(K)
90            CONTINUE
            ELSE IF(ISTTYP.EQ.6)THEN
              DO 100 K=1,3
                HELP(K+3)=EX(K)
100           CONTINUE
            END IF
C
C GET PARTIAL DERIVATIVES AT TIME TSTOCH
C --------------------------------------
              CALL COMPZ0(NSEFF,SVNEFF,SVN,TSTOCH,ORBFIL(2,IND),IND,Z0)
C
C INVERT MATRIX Z0 CONTAINING VALUES OF PARTIALS W.R.T. INITIAL ORBIT
C ELEMENTS AT TIME TSTC
C -------------------------------------------------------------------
            CALL DMINV(Z0,6,DET,L1,M1)
C ------------------------------------------------------------
            DO 130 IPAR=1,6
              ELESTO(IPAR,IFIL,ISTOCH)=0.D0
              DO 120 K=1,6
                ELESTO(IPAR,IFIL,ISTOCH)=ELESTO(IPAR,IFIL,ISTOCH)+
     1                                       Z0(IPAR,K)*HELP(K)
120           CONTINUE
130         CONTINUE
C
C PROPAGATE TO END OF CURRENT FILE/BEGINNING OF NEXT FILE
C -------------------------------------------------------
            IF(IND.LT.IFIL)THEN
              TTT=TOSC(IND+1)
            ELSE
              TTT=TACT
            END IF
            CALL COMPZ0(NSEFF,SVNEFF,SVN,TTT,ORBFIL(2,IND),IND,Z0)
            DO 135 IPAR=1,6
              HELP(IPAR)=ELESTO(IPAR,IFIL,ISTOCH)
135         CONTINUE
            DO 142 IPAR=1,6
              ELESTO(IPAR,IFIL,ISTOCH)=0.D0
              DO 141 K=1,6
                ELESTO(IPAR,IFIL,ISTOCH)=ELESTO(IPAR,IFIL,ISTOCH)+
     1                                    Z0(IPAR,K)*HELP(K)
141           CONTINUE
142         CONTINUE
C
C PROPAGATE FORWARD TO CURRENT FILE
C ---------------------------------
            DO 180 IF=FILNUM(IFIL0)+1,FILNUM(NFL)-1
              CALL COMPZ0(NSEFF,SVNEFF,SVN,TOSC(IF),ORBFIL(2,IF),IF,Z0)
              CALL DMINV(Z0,6,DET,L1,M1)
              DO 144 IPAR=1,6
                HELP(IPAR)=ELESTO(IPAR,IFIL,ISTOCH)
144           CONTINUE
              DO 150 IPAR=1,6
                ELESTO(IPAR,IFIL,ISTOCH)=0.D0
                DO 145 K=1,6
                  ELESTO(IPAR,IFIL,ISTOCH)=ELESTO(IPAR,IFIL,ISTOCH)+
     1                                      Z0(IPAR,K)*HELP(K)
145             CONTINUE
150           CONTINUE
              CALL COMPZ0(NSEFF,SVNEFF,SVN,TOSC(IF+1),ORBFIL(2,IF),
     1                    IF,Z0)
              DO 155 IPAR=1,6
                HELP(IPAR)=ELESTO(IPAR,IFIL,ISTOCH)
155           CONTINUE
              DO 170 IPAR=1,6
                ELESTO(IPAR,IFIL,ISTOCH)=0.D0
                DO 165 K=1,6
                  ELESTO(IPAR,IFIL,ISTOCH)=ELESTO(IPAR,IFIL,ISTOCH)+
     1                                      Z0(IPAR,K)*HELP(K)
165             CONTINUE
170           CONTINUE
180         CONTINUE
C
C COMPUTE AT TSTOCH PARTIALS W.R.T. ORBITAL ELEMENTS
C --------------------------------------------------
            CALL COMPZ0(NSEFF,SVNEFF,SVN,TOSC(IFIL),ORBFIL(2,IFIL),
     1                  IFIL,Z0)
            CALL DMINV(Z0,6,DET,L1,M1)
            DO 181 IPAR=1,6
              HELP(IPAR)=ELESTO(IPAR,IFIL,ISTOCH)
181         CONTINUE
            DO 187 IPAR=1,6
              ELESTO(IPAR,IFIL,ISTOCH)=0.D0
              DO 186 K=1,6
                ELESTO(IPAR,IFIL,ISTOCH)=ELESTO(IPAR,IFIL,ISTOCH)+
     1                                    Z0(IPAR,K)*HELP(K)
186           CONTINUE
187         CONTINUE
          ELSE
C
C COMPUTATION OF PARTIALS USING OLD ORBIT MODEL (APPROXIMATION)
C -------------------------------------------------------------
            DO 190 I=1,3
              HELP(I)=0.D0
190         CONTINUE
            HELP(ISTTYP)=1
C
            TSEC=(TSTOCH-TACT)*86400.D0
            CALL ELEPRT(GM,ELEACT(1),ELEACT(2),ELEACT(3),ELEACT(4),
     1                  ELEACT(5),ELEACT(7),0.D0,TSEC,HELP,ELEP)
            DO 200 IPAR=1,5
              ELESTO(IPAR,IFIL,ISTOCH)=ELEP(IPAR)
200         CONTINUE
            ELESTO(6,IFIL,ISTOCH)=ELEP(8)
          END IF
        END IF
        FRCLOC(ISTOCH,IFIL)=ISTTYP
        SVNLOC(ISTOCH,IFIL)=SVN
        TSTCLC(ISTOCH,IFIL)=TSTOCH
      END IF
C
C RETURN PARTIALS
C ---------------
      IF(IWT.EQ.2)THEN
        DO 205 ISTOCH=1,NSTOCH
          IF(TSTOCH.EQ.TSTCLC(ISTOCH,IFIL).AND.
     1       ISTTYP.EQ.FRCLOC(ISTOCH,IFIL).AND.
     2       SVN.EQ.SVNLOC(ISTOCH,IFIL)) GO TO 206
205     CONTINUE
        ELEPAR(1:6)=0d0
        GOTO 999
      END IF
206   CONTINUE
      NEWOLD=NEWLOC
      DO 210 IPAR=1,6
        ELEPAR(IPAR)=ELESTO(IPAR,IFIL,ISTOCH)
210   CONTINUE
C
999   CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
