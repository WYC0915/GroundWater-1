      MODULE s_DUASLP
      CONTAINS

C*
      SUBROUTINE DUASLP(IEPOCH,NSAT,SVN,IWLSCR,RHS,
     1                  SIGL12,SWIDTH,
     2                  IRJECT,MXIOND,IPRNT2,ITITL2,MINCYC,
     3                  NSLIP,LSTSLP,SLPLST,
     4                  SLPXXX,IONO,CLOCK,IREF,MRKDEL,FILFLG,
     5                  NTONLY,L5CLEA)
CC
CC NAME       :  DUASLP
CC
CC PURPOSE    :  CHECK ALL SIMULTANEOUSLY OBSERVED OBSERVATIONS
CC               OF A DUAL FREQUENCY RECEIVER
CC
CC PARAMETERS :
CC         IN :  IEPOCH : EPOCH NUMBER                        I*4
CC               NSAT   : NOMINAL NUMBER OF SATELLITES        I*4
CC               SVN(I),I=1,2,..,NSAT: SV NUMBERS             I*4
CC               IWLSCR(L),L=1,2: WAVELENGTH FACTOR FOR       I*4
CC                        ORIGINAL CARRIER L
CC               RHS(K,ISAT),K=1,2,3     ISAT=1,2,..,NSAT:    R*8
CC                        RIGHT HAND SIDES OF COND. EQNS IN FREQ. K
CC               SIGL12(L), L=1,2: RMS OF CARRIER L           R*8
CC               SWIDTH(K),K=1,2: NUMBER OF NEAREST INTEGERS   I*4
CC                        TO BE TESTED
CC                         K=1 : IN L1/L2
CC                         K=2 : IN L5
CC               IRJECT : OUTLIER REJECTION (YES=1,NO=0)       I*4
CC               MXIOND : MAXIMUM IONOSPHERE CHANGE BETWEEN    I*4
CC                        EPOCHS (IN % OF L1 CYCLES) FOR OUT-
CC                        LIER REJECTION
CC               IPRNT2 : PRINT LEVEL FOR CYCLE SLIP DETECT.   I*4
CC                        =0: NO MESSAGES PRINTED
CC                        =1: PRINT SUMMARY
CC                        =2: ALL MESSAGES PRINTED
CC               ITITL2 : TITLE PRINT FLAG                     I*4
CC                        =0: TITLE PRINTED ALREADY
CC                        =1: TITLE NOT PRINTED YET
CC               FILFLG(IFRQ,ISAT),ISAT=1,2,..,NSAT: FLAGS    CH*1
CC                        FOR SATELLITES
CC                        =O: NO FLAG IN FILE
CC                        =C: CYCLE SLIP FLAG IN FILE
CC               MINCYC : ACCEPT CYCLE SLIPS > "MINCYC" CYCLES I*4
CC               NTONLY : TEST OBS. WITH CYCLE SLIP FLAG       I*4
CC                          ONLY (0/1)
CC               L5CLEA : L5 IS CLEAN (EXCEPT FLAGGED          I*4
CC                          EPOCHS)  (0/1)
CC        OUT :  NSLIP  : TOTAL NUMBER OF SLIPS DISCOVERED     I*4
CC               LSTSLP(K,I),K=1,..,6, I=1,2,..,NSLIP          I*4
CC                        SLIP DESCRIPTION:
CC                     (1,I): EPOCH NUMBER
CC                     (2,I): SV NUMBER
CC                     (3,I): FREQUENCY NUMBER
CC                     (4,I): WAVELENGTH FACTOR
CC                     (5,I): WAVELENGTH FACTOR OF L5
CC                            IF AN L1 AND L2 SLIP OCCURS AT
CC                            THE SAME EPOCH FOR THE SAME
CC                            SATELLITE:
CC                              (5,I)=3 FOR L1, INDICATING THAT
CC                                      AN L2 SLIP OCCURED TOO
CC                              (5,I)=WAVELENGTH OF L5 FOR L2
CC                     (6,I): DETECTED BY
CC                            =1: SINGLE FREQ. SLIP DETECTION
CC                            =2: DUAL   FREQ. SLIP DETECTION
CC                            =3: CLOCK
CC                            =4: USER
CC                            =5: MILLI-SEC JUMP
CC                            ALL CYCLE SLIPS DETECTED IN THE
CC                            LATEST RUN HAVE A NEGATIVE SIGN
CC               SLPLST(I),I=1,2,..,NSLIP: NUMBER OF CYCLES   R*8
CC                        (OR HALF CYCLES) OF SLIP I
CC               SLPXXX(2,I),I=1,..,NSLIP: REAL VALUED        R*8
CC                        ESTIMATES FOR SLIPS - INTEGER SLIP
CC                        SINGLE FREQ.: L1/L2 ESTIMATES
CC                        DUAL   FREQ.: L1/L2 AND L5 ESTIMATES
CC               IONO(K,I),K=1,2,3, I=1,2,..,NSLIP            R*8
CC                        K=1: MEAN VALUE FOR IONOSPHERE FROM
CC                             L1 AND L2 CARRIER
CC                        K=2: DIFFERENCE OF ESTIMATES FROM L1
CC                             AND L2
CC                        K=3: RESIDUAL IN L3
CC               CLOCK(L),L=1,2,..,5: BIASED CLOCK VALUES     R*8
CC               IREF   : INDEX OF REFERENCE SATELLITE        I*4
CC               MRKDEL(ISAT),ISAT=1,2,..,NSAT: MARKS FOR     CH*1
CC                        SATELLITES
CC                        =O: TRIPLE DIFF. OK
CC                        =N: NOT USED
CC                        =Y: TRIPLE DIFFERENCE MARKED FOR
CC                            ELIMINATION
CC                        =R: REFERENCE SATELLITE
CC
CC REMARKS    :  AS VERSION
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER, L.MERVART
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  88/05/11 12:01
CC
CC CHANGES    :  05-JUN-92 : ??: CHANGES FOR THE NEW MAUPRP VERSION
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               29-NOV-93 : MR: SELECT SMALLEST L3, IF SEVERAL POSSIBLE
CC               10-AUG-94 : MR: CALL EXITRC
CC               08-NOV-95 : MR: CYCLE SLIPS NOT ACCEPTED IF ANY OF THE
CC                               FREQ. HAS A SLIP SMALLER THAN "MINCYC"
CC               23-SEP-97 : DI: USE MAXSAT.inc
CC               14-OCT-98 : MR: GLONASS MODIFICATIONS
CC               04-DEC-98 : DI: COMPUTATION OF STRP3 CHANGED
CC               21-DEC-98 : MR: INITIALIZE "MRKDEL" IN ANY CASE
CC               15-AUG-99 : JJ: RM UNUSED VARS SLPLOC, LSTLOC, SLPXXL,
CC                               IFIRST
CC               09-AUG-01 : MR: USE "SVNSYS" INSTEAD OF "MIXSVN"
CC               16-JAN-02 : MR: MINCYC=-1
CC               19-SEP-02 : RD: CYCLE SLIP ONLY ON ONE FREQU. (L1 OR L2)
CC               07-OCT-02 : RD: PREVENT INDEX==0 FOR FACLIN (CLOCK(3)=..)
CC               31-OCT-02 : RD: NO CLOCK CORRECTION FROM ONE OBS PER EPOCH
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               30-APR-04 : RD: AT LEAST HALF OF THE OBS. CONTRIBUTE TO L3XXX
CC               13-DEC-04 : HB: COMMENT ERROR MESSAGE
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               26-MAR-12 : RD: USE WTMSGS AS MODULE NOW
CC               26-MAR-12 : RD: REMOVE UNUSED VARIABLES
CC               28-MAR-12 : RD: USE SVNSYS AS MODULE NOW
CC               13-JUN-12 : RD: AMB. TO ALL SAT. IF CONFAC EXCEEDS LIMIT
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: LFNPRT, LFNERR
      USE m_maxdim, ONLY: MAXSAT
      USE s_l12val
      USE s_neaint
      USE s_wtmsgs
      USE s_major
      USE f_svnsys
      USE s_exitrc
      USE s_l12sol
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IEPOCH, IFREQ , IFRQ  , IFRQ1 , IIL1  , IIL5  , IL1   ,
     1          IL5   , IMSG  , INDMIN, IOS   , IPRNT2, IREF  , IRJECT,
     2          ISAT  , ISATOK, ISWID2, ITEST , ITESTN, ITESTY, ITITL2,
     3          IWL5  , IWLSC5, L5CLEA, MAXTRY, MINCYC, MXCSLP, MXIOND,
     4          NGOOD , NMAX  , NOK   , NSAT  , NSLIP , NTONLY
C
      REAL*8    CONFAC, QUAL  , QUAMIN, RESL3 , STRP1 , STRP2 , STRP3 ,
     1          TEST  , XIMIN , XN1   , XN1INT, XN2   , XN2INT, XN5   ,
     2          XN5INT
C
CCC       IMPLICIT  REAL*8 (A-H,O-Z)
CCC       IMPLICIT  INTEGER*4 (I-N)
C
      PARAMETER (MAXTRY=10)
C
      CHARACTER*1 MRKDEL(*),OK,FILFLG(3,*)
      CHARACTER*6 MXNSLP
C
      INTEGER*4   SVN(*),IWLSCR(*),LSTSLP(6,*),SVNREF
      INTEGER*4   SWIDTH(2),IWLF3(3)
C
      REAL*8      RHS(3,*),SLPLST(*),SLPXXX(2,*),IONO(3,*)
      REAL*8      RHSLOC(MAXSAT),
     1            CLOCK(3),SIGL12(2),XIONO(3),SLIP3(3),XSLIP3(3),
     2            L3RHS(MAXSAT),L3XXX
      REAL*8      SEQL5(MAXTRY),SEQL1(MAXTRY),ARRAY(5,MAXTRY*MAXTRY)
C
      COMMON/CDUSLP/ARRAY
      INCLUDE 'COMFREQ.inc'
      COMMON/MCMSLP/MXCSLP,MXNSLP
C
C SET RMS OF N1
C -------------
      STRP1=2*DSQRT(2.D0)*SIGL12(1)
      STRP2=2*DSQRT(2.D0)*SIGL12(2)
C
C L5-WAVELENGTH FACTOR
C --------------------
      IF(IWLSCR(1).EQ.1.AND.IWLSCR(2).EQ.1)THEN
        IWL5=1
      ELSE
        IWL5=2
      END IF
C
C CHECK MAXIMUM NUMBER OF SATELLITES
C ----------------------------------
      IF(NSAT.GT.MAXSAT) THEN
        WRITE(LFNERR,901) NSAT,MAXSAT
901     FORMAT(/,' *** SR DUASLP: TOO MANY SATELLITES',/,
     1                       16X,'NUMBER OF SATELLITES:',I3,/,
     2                       16X,'MAX. NUMBER ALLOWED :',I3,/)
        CALL EXITRC(2)
      ENDIF
C
C CHECK MAXIMUM NUMBER OF TRIALS
C ------------------------------
      DO 10 IFRQ=1,2
        IF(SWIDTH(IFRQ).GT.MAXTRY) THEN
          IF(IFRQ.EQ.1) THEN
            IFREQ=1
          ELSE
            IFREQ=5
          ENDIF
          WRITE(LFNERR,902) SWIDTH(IFRQ),IFREQ,MAXTRY
902       FORMAT(/,' *** SR DUASLP: TOO MANY INTEGERS TO BE TESTED',/,
     1                         16X,'NUMBER OF TESTS    :',I3,/,
     2                         16X,'SEARCH FREQUENCY   :',I3,/,
     3                         16X,'MAX. NUMBER ALLOWED:',I3,/)
          CALL EXITRC(2)
        ENDIF
10    CONTINUE
C
C INITIALIZE MRKDEL
C -----------------
      DO ISAT=1,NSAT
        MRKDEL(ISAT)='N'
      ENDDO
C
C DEFINE CLOCK
C ------------
      DO 15 ISAT=1,NSAT
        IF(RHS(1,ISAT).EQ.1.D20.OR.RHS(2,ISAT).EQ.1.D20)THEN
          L3RHS(ISAT)=1.D20
        ELSE
          L3RHS(ISAT)=FACLIN(3,1,SVN(ISAT))*RHS(1,ISAT)+
     1                FACLIN(3,2,SVN(ISAT))*RHS(2,ISAT)
        ENDIF
15    CONTINUE
C
      STRP3=DSQRT((FACLIN(3,1,SVN(1))*STRP1)**2+
     1            (FACLIN(3,2,SVN(1))*STRP2)**2)
CC
CC FLEXIBLE EDITING LEVEL:
CC   AT LEAST THE HALF OF OBSERVATIONS SHOULD CONTRIBUTE TO L3XXX
CC      CALL MAJOR(NSAT,L3RHS,3*STRP3,NOK,NMAX,L3XXX,IREF)
      CONFAC=3d0
      NMAX=0
      NOK=NSAT
      DO WHILE (CONFAC.LT.3000D0 .AND.
     1          DBLE(NMAX)/DBLE(NOK).LT.0.5D0)
        CALL MAJOR(NSAT,L3RHS,CONFAC*STRP3,NOK,NMAX,L3XXX,IREF)
        IF (NOK.EQ.0) EXIT
        IF (DBLE(NMAX)/DBLE(NOK).LT.0.5D0) THEN
CC          IF (NMAX.GT.1) WRITE(LFNERR,'(A,F6.0,A,I2,A,I2,A)')
CC     1          ' ### SR DUASLP: INCR. CONFIDENCE FACTOR:',
CC     2          CONFAC,' (',NMAX,',',NOK,')'
          CONFAC=CONFAC*10d0
        ENDIF
      ENDDO
C
      IF (SVNSYS(1,NSAT,SVN)) THEN
C
C NO GOOD OBSERVATIONS: PREVENT ENDLESS LOOP
        IF (NOK.EQ.0) RETURN
C
        IF (NMAX.LT.2 .OR. L3XXX.GT.100.D0 .OR. CONFAC.GE.3000D0) THEN
C
C NEW AMBIGUITIES FOR ALL SATELLITES
          NSLIP=NSLIP+1
            IF(NSLIP.GT.MXCSLP) THEN
               WRITE(LFNERR,21) NSLIP,MXCSLP
               CALL EXITRC(2)
             ENDIF
          LSTSLP(6,NSLIP)=-100
          RETURN
        ENDIF
      ENDIF
C
      CLOCK(1)=0
      CLOCK(2)=0
      CLOCK(3)=0
      ISATOK=0
C
      DO 17 ISAT=1,NSAT
        IF (NMAX.LT.2) GOTO 17
        STRP3=DSQRT((FACLIN(3,1,SVN(ISAT))*STRP1)**2
     1             +(FACLIN(3,2,SVN(ISAT))*STRP2)**2)
        IF (RHS(3,ISAT).NE.1.D20 .AND.
     1      DABS(L3RHS(ISAT)-L3XXX).LE.CONFAC*STRP3) THEN
          CLOCK(1)=CLOCK(1)+RHS(1,ISAT)
          CLOCK(2)=CLOCK(2)+RHS(2,ISAT)
          ISATOK=ISATOK+1
        ENDIF
17    CONTINUE
      IF (ISATOK.GT.0) THEN
        CLOCK(1)=CLOCK(1)/ISATOK
        CLOCK(2)=CLOCK(2)/ISATOK
C
C       USE FACTOR FOR 1ST SVN (SEE SR RSTCLK)
        CLOCK(3)=FACLIN(5,1,SVN(1))*CLOCK(1)+
     1           FACLIN(5,2,SVN(1))*CLOCK(2)
      ENDIF
C
C FORM ALL CONDITION EQUATIONS WITH RESPECT TO "REF.SAT." (CLOCK)
C ---------------------------------------------------------------
      DO 100 ISAT=1,NSAT
        IF(RHS(3,ISAT).NE.1.D20) THEN
C
C ACTION ACCORDING TO OPTIONS "NTONLY","L5CLEA"
          ISWID2 = SWIDTH(2)
          IF (FILFLG(3,ISAT) .EQ. 'O') THEN
            IF (NTONLY .EQ. 1) GO TO 100
            IF (L5CLEA .EQ. 1) ISWID2 = 1
          END IF
C
          RHSLOC(1)=RHS(1,ISAT)-CLOCK(1)
          RHSLOC(2)=RHS(2,ISAT)-CLOCK(2)
C
C TEST HYPOTHESIS THAT NO CYCLE SLIP IS PRESENT
C ---------------------------------------------
          CALL L12VAL(SVN(ISAT),RHSLOC,0.D0,0.D0,IWLSCR,XIONO,RESL3)
          TEST=DABS((XIONO(1)+XIONO(2))/2)
          STRP3=DSQRT((FACLIN(3,1,SVN(ISAT))*STRP1)**2
     1               +(FACLIN(3,2,SVN(ISAT))*STRP2)**2)
          IF(DABS(RESL3).LT.3*STRP3
     1            .AND.TEST.LT.MXIOND*.19/100) GOTO 100
C
C RECOVER L5-SLIP AND CORRESPONDING REAL VALUED ESTIMATE
C ------------------------------------------------------
          XN5=(FACLIN(5,1,SVN(ISAT))*RHSLOC(1)+
     1         FACLIN(5,2,SVN(ISAT))*RHSLOC(2))/
     2        (WLGT(5,SVN(ISAT))/IWL5)
          XN5INT=DNINT(XN5)
C
C COMPUTE REAL VALUED ESTIMATE FOR N1 (AND N2)
C --------------------------------------------
          CALL L12SOL(SVN(ISAT),RHSLOC,XN5INT,IWLSCR,XN1INT,XN1)
          IF(IWLSCR(1).EQ.IWLSCR(2)) THEN
            XN2=XN1-XN5INT
            XN2INT=XN1INT-XN5INT
          ELSE
            XN2=2*XN1-XN5INT
            XN2INT=2*XN1INT-XN5INT
          ENDIF
C
C WRITE MESSAGE
C -------------
          IF(IPRNT2.GE.2) THEN
            IMSG=8
            SVNREF=0
            IFRQ1=1
            IWLF3(1)=IWLSCR(1)
            IWLF3(2)=IWLSCR(2)
            IWLF3(3)=MAX0(IWLSCR(1),IWLSCR(2))
            SLIP3(1)=XN1INT
            SLIP3(2)=XN2INT
            SLIP3(3)=XN5INT
            XSLIP3(1)=XN1-XN1INT
            XSLIP3(2)=XN2-XN2INT
            XSLIP3(3)=XN5-XN5INT
            CALL WTMSGS(IMSG,ITITL2,IEPOCH,SVN(ISAT),SVNREF,
     1                  IFRQ1,IWLF3,SLIP3,XSLIP3)
          ENDIF
C
C COMPUTE INTEGERS TO BE TESTED
C -----------------------------
          CALL NEAINT(ISWID2,XN5,SEQL5)
          CALL NEAINT(SWIDTH(1),XN1,SEQL1)
C
C SEARCH AROUND NEAREST WIDE-LANE/L1 INTEGERS
C -------------------------------------------
          ITEST=0
          NGOOD=0
          INDMIN=0
          XIMIN=1.D20
          QUAMIN=1.D20
C
          IF(IPRNT2.GE.2) THEN
            WRITE(LFNPRT,22)
22          FORMAT(8X,'TEST DN1 DN5       RES.L3       IONOS   ',
     1                'QUALITY  OK')
          ENDIF
C
          DO 55 IL5=1,ISWID2
            DO 50 IL1=1,SWIDTH(1)
              ITEST=ITEST+1
              CALL L12VAL(SVN(ISAT),RHSLOC,SEQL1(IL1),SEQL5(IL5),
     1                    IWLSCR,XIONO,RESL3)
              QUAL=DABS(RESL3/STRP3)
CCC           QUAL=DABS(RESL3*XIONO(3))/(STRP4*STRP3)*
CCC  1             (DABS(XIONO(1))+DABS(XIONO(2)))/2/STRP4
              IF(DABS(QUAL).LT.QUAMIN) THEN
                QUAMIN=DABS(QUAL)
                IIL1=IL1
                IIL5=IL5
                ITESTN=ITEST
              ENDIF
C
C CHECK WHETHER COMBINATION IS ACCEPTABLE
C ---------------------------------------
              OK='N'
              TEST=DABS((XIONO(1)+XIONO(2))/2)
              IF(DABS(RESL3).LT.3*STRP3.
     1            AND.TEST.LT.MXIOND*.19/100) THEN
                OK='Y'
                MRKDEL(ISAT)='O'
                NGOOD=NGOOD+1
                ARRAY(1,NGOOD)=SEQL1(IL1)
                ARRAY(2,NGOOD)=SEQL5(IL5)
                ARRAY(3,NGOOD)=(XIONO(1)+XIONO(2))/2
                ARRAY(4,NGOOD)=XIONO(3)
                ARRAY(5,NGOOD)=RESL3
CC                TEST=DABS(ARRAY(3,NGOOD))
CC SELECT THE ONE WITH BEST L3 RESIDUAL (INSTEAD OF SMALLEST IONOSPHERE)
                TEST=DABS(RESL3)
                IF(TEST.LT.XIMIN) THEN
                  INDMIN=NGOOD
                  XIMIN=TEST
                  ITESTY=ITEST
                ENDIF
              ENDIF
C
              IF(IPRNT2.GE.2) THEN
                WRITE(LFNPRT,23,IOSTAT=IOS) ITEST,
     1                                 IDNINT(SEQL1(IL1)-XN1INT),
     2                                 IDNINT(SEQL5(IL5)-XN5INT),
     3                                 RESL3,XIONO(1),QUAL,OK
23              FORMAT(8X,I3,2I4,6X,F8.3,4X,F8.3,F10.3,3X,A1)
              ENDIF
50          CONTINUE
55        CONTINUE
C
C CHOOSE COMBINATION WITH BEST QUALITY VECTOR IF NO ACCEPTABLE
C SOLUTION WAS FOUND
C ------------------------------------------------------------
          IF(INDMIN.EQ.0) THEN
            MRKDEL(ISAT)='Y'
C
C PRINTING MESSAGE
            IF(IPRNT2.GE.2) THEN
              IF(IRJECT.EQ.1) THEN
                WRITE(LFNPRT,24) ITESTN
24              FORMAT(8X,'BEST TEST =',I3,' NOT OK --> ',
     1                    'OUTLIER REJECTION INVOKED')
              ELSE
                WRITE(LFNPRT,241) ITESTN
241             FORMAT(8X,'BEST TEST =',I3,' NOT OK --> ',
     1                    'OUTLIER REJECT. N O T INVOKED')
              ENDIF
            ENDIF
C
            CALL L12VAL(SVN(ISAT),RHSLOC,SEQL1(IIL1),SEQL5(IIL5),
     1                  IWLSCR,XIONO,RESL3)
            INDMIN=1
            ARRAY(1,INDMIN)=SEQL1(IIL1)
            ARRAY(2,INDMIN)=SEQL5(IIL5)
            ARRAY(3,INDMIN)=(XIONO(1)+XIONO(2))/2
            ARRAY(4,INDMIN)=XIONO(3)
            ARRAY(5,INDMIN)=RESL3
          ELSE
            IF(IPRNT2.GE.2) THEN
              XN1INT=ARRAY(1,INDMIN)
              XN5INT=ARRAY(2,INDMIN)
              IF(IWLSCR(1).EQ.IWLSCR(2)) THEN
                XN2INT=XN1INT-XN5INT
              ELSE
                XN2INT=2*XN1INT-XN5INT
              ENDIF
              IF(MINCYC.NE.-1 .AND. DABS(XN5INT).GT.MINCYC .AND.
     1          ((DABS(XN1INT).GT.MINCYC.AND.DABS(XN2INT).GT.MINCYC).OR.
     2           (DABS(XN1INT).GT.MINCYC.AND.     XN2INT .EQ.0D0   ).OR.
     3           (     XN1INT .EQ.0D0   .AND.DABS(XN2INT).GT.MINCYC)))
     4                                                             THEN
                WRITE(LFNPRT,25) ITESTY
25              FORMAT(8X,'BEST TEST =',I3,' OK     --> ',
     1                    'SLIP(S) OK, ACCEPTED')
              ELSE
                MRKDEL(ISAT)='Y'
C
C PRINTING MESSAGE
                IF(IPRNT2.GE.2) THEN
                  IF(IRJECT.EQ.1) THEN
                    WRITE(LFNPRT,26) ITESTY
26                  FORMAT(8X,'BEST TEST =',I3,' OK     --> ',
     1                        'SLIP(S) OK, BUT TOO SMALL',/,
     1                    34X,'OUTLIER REJECTION INVOKED')
                  ELSE
                    WRITE(LFNPRT,261) ITESTN
261                 FORMAT(8X,'BEST TEST =',I3,' OK     --> ',
     1                        'SLIP(S) OK, BUT TOO SMALL',/,
     1                    34X,'OUTLIER REJECT. N O T INVOKED')
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
C
C SELECT BEST SOLUTION
C --------------------
          XN1INT=ARRAY(1,INDMIN)
          XN5INT=ARRAY(2,INDMIN)
          IF(IWLSCR(1).EQ.IWLSCR(2)) THEN
            XN2INT=XN1INT-XN5INT
          ELSE
            XN2INT=2*XN1INT-XN5INT
          ENDIF
          IWLSC5=MAX0(IWLSCR(1),IWLSCR(2))
C
C CYCLE SLIPS LARGER THAN "MINCYC"
          IF(MINCYC.NE.-1 .AND. DABS(XN5INT).GT.MINCYC .AND.
     1      ((DABS(XN1INT).GT.MINCYC.AND.DABS(XN2INT).GT.MINCYC) .OR.
     2       (DABS(XN1INT).GT.MINCYC.AND.     XN2INT .EQ.0D0   ) .OR.
     3       (     XN1INT .EQ.0D0   .AND.DABS(XN2INT).GT.MINCYC))) THEN
            IF(XN1INT.NE.0.D0) THEN
              NSLIP=NSLIP+1
              IF(NSLIP.GT.MXCSLP) THEN
                WRITE(LFNERR,21) NSLIP,MXCSLP
21              FORMAT(/,' *** SR DUASLP: TOO MANY CYCLE SLIPS',/,
     1                               16X,'NUMBER OF SLIPS: >=',I5,/,
     2                               16X,'MAXIMUM NUMBER :   ',I5,/)
                CALL EXITRC(2)
              ENDIF
              LSTSLP(1,NSLIP)=IEPOCH
              LSTSLP(2,NSLIP)=SVN(ISAT)
              LSTSLP(3,NSLIP)=1
              LSTSLP(4,NSLIP)=IWLSCR(1)
              IF(XN2INT.EQ.0) THEN
                LSTSLP(5,NSLIP)=IWLSC5
              ELSE
                LSTSLP(5,NSLIP)=3
              ENDIF
              LSTSLP(6,NSLIP)=-2
              SLPLST(NSLIP)=XN1INT
              SLPXXX(1,NSLIP)=XN1-XN1INT
              SLPXXX(2,NSLIP)=XN5-XN5INT
              IONO(1,NSLIP)=ARRAY(3,INDMIN)
              IONO(2,NSLIP)=ARRAY(4,INDMIN)
              IONO(3,NSLIP)=ARRAY(5,INDMIN)
            ENDIF
            IF(XN2INT.NE.0.D0) THEN
              NSLIP=NSLIP+1
              IF(NSLIP.GT.MXCSLP) THEN
                WRITE(LFNERR,21) NSLIP,MXCSLP
                CALL EXITRC(2)
              ENDIF
              LSTSLP(1,NSLIP)=IEPOCH
              LSTSLP(2,NSLIP)=SVN(ISAT)
              LSTSLP(3,NSLIP)=2
              LSTSLP(4,NSLIP)=IWLSCR(2)
              LSTSLP(5,NSLIP)=IWLSC5
              LSTSLP(6,NSLIP)=-2
              SLPLST(NSLIP)=XN2INT
              SLPXXX(1,NSLIP)=XN2-XN2INT
              SLPXXX(2,NSLIP)=XN5-XN5INT
              IONO(1,NSLIP)=ARRAY(3,INDMIN)
              IONO(2,NSLIP)=ARRAY(4,INDMIN)
              IONO(3,NSLIP)=ARRAY(5,INDMIN)
            ENDIF
          ELSE
            MRKDEL(ISAT)='Y'
          ENDIF
        ENDIF
100   CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
