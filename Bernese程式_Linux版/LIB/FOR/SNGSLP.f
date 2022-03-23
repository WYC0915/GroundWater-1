      MODULE s_SNGSLP
      CONTAINS

C*
      SUBROUTINE SNGSLP(IEPOCH,NSAT,SVN,INDFRQ,FRQAUX,IREF,IWLSCR,RHS,
     1                  IZERO,IPRNT2,ITITL2,MINCYC,
     2                  NSLIP,LSTSLP,SLPLST,SLPXXX,CLOCK,FILFLG,
     3                  NTONLY,L5CLEA,SIGL12)
CC
CC NAME       :  SNGSLP
CC
CC PURPOSE    :  CHECK ALL SIMULTANEOUSLY OBSERVED OBSERVATIONS
CC               OF A SINGLE FREQUENCY (L1 OR L2)
CC
CC PARAMETERS :
CC         IN :  IEPOCH : EPOCH NUMBER                        I*4
CC               NSAT   : NOMINAL NUMBER OF SATELLITES        I*4
CC               SVN(I),I=1,2,..,NSAT: SV NUMBERS             I*4
CC               INDFRQ : INDEX OF FREQUENCY TO BE CHECKED    I*4
CC               FRQAUX(I),I=1,2,..,NFREQ:                    I*4
CC                         FRQAUX(INDFRQ): FREQUENCY TO BE CHECKED
CC               IWLSCR(L),L=1,2: WAVELENGTH FACTOR FOR       I*4
CC                        ORIGINAL CARRIER L
CC               RHS(K,ISAT),K=1,2,3     ISAT=1,2,..,NSAT:    R*8
CC                        RIGHT HAND SIDES OF COND. EQNS IN FREQ. K
CC               IZERO  : REPORT ALSO SLIPS OF SIZE 0 ?       I*4
CC                        =0: NO
CC                        =1: YES
CC               IPRNT2 : PRINT LEVEL FOR CYCLE SLIP DETECT.  I*4
CC                        =0: NO MESSAGES PRINTED
CC                        =1: PRINT SUMMARY
CC                        =2: ALL MESSAGES PRINTED
CC               ITITL2 : TITLE PRINT FLAG                    I*4
CC                        =0: TITLE PRINTED ALREADY
CC                        =1: TITLE NOT PRINTED YET
CC               MINCYC : ACCEPT SLIPS > "MINCYC" CYCLES      I*4
CC               FILFLG(IFRQ,ISAT),ISAT=1,2,..,NSAT: FLAGS    CH*1
CC                        FOR SATELLITES
CC                        =O: NO FLAG IN FILE
CC                        =C: CYCLE SLIP FLAG IN FILE
CC               NTONLY : TEST OBS. WITH CYCLE SLIP FLAG       I*4
CC                          ONLY (0/1)
CC               L5CLEA : L5 IS CLEAN (EXCEPT FLAGGED          I*4
CC                          EPOCHS)  (0/1)
CC               SIGL12(L), L=1,2: RMS OF CARRIER L           R*8
CC     IN/OUT :  IREF   : REFERENCE SATELLITE TO BE           I*4
CC                        USED FOR ORIGINAL CARRIER L
CC               NSLIP  : TOTAL NUMBER OF SLIPS DISCOVERED    I*4
CC               LSTSLP(K,I),K=1,..,6, I=1,2,..,NSLIP         I*4
CC                        SLIP DESCRIPTION:
CC                        (1,I): EPOCH NUMBER
CC                        (2,I): SV NUMBER
CC                        (3,I): FREQUENCY NUMBER
CC                        (4,I): WAVELENGTH FACTOR
CC                        (5,I): WAVELENGTH FACTOR OF L5
CC                               IF AN L1 AND L2 SLIP OCCURS AT
CC                               THE SAME EPOCH FOR THE SAME
CC                               SATELLITE:
CC                                =3 FOR L1, INDICATING THAT
CC                                   AN L2 SLIP OCCURED TOO
CC                                =WAVELENGTH OF L5 FOR L2
CC                        (6,I): DETECTED BY
CC                               =1: SINGLE FREQ. SLIP DETECTION
CC                               =2: DUAL   FREQ. SLIP DETECTION
CC                               =3: CLOCK
CC                               =4: USER
CC                               =5: MILLI-SEC JUMP
CC                               ALL CYCLE SLIPS DETECTED IN THE
CC                               LATEST RUN HAVE A NEGATIVE SIGN
CC               SLPLST(I),I=1,2,..,NSLIP: NUMBER OF CYCLES   R*8
CC                        (OR HALF CYCLES) OF SLIP I
CC               SLPXXX(2,I),I=1,2,..,N: REAL VALUED ESTIMATE R*8
CC                         FOR SLIP I - INTEGER SLIP
CC                         SINGLE FREQ.: L1/L2 ESTIMATES
CC                         DUAL   FREQ.: L1/L2 AND L5 ESTIMATES
CC               CLOCK(L),L=1,2,..,5: BIASED CLOCK            R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER, L.MERVART
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  88/05/11 12:01
CC
CC CHANGES    :  05-JUN-92 : ??: CHANGES FOR THE NEW MAUPRP VERSION
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               10-AUG-94 : MR: CALL EXITRC
CC               23-SEP-97 : DI: USE MAXSAT.inc
CC               14-OCT-98 : HH: MODIFICATIONS FOR GLONASS
CC               09-AUG-01 : MR: USE "SVNSYS" INSTEAD OF "MIXSVN"
CC               16-JAN-02 : MR: MINCYC=-1
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               26-MAR-12 : RD: USE WTMSGS AS MODULE NOW
CC               28-MAR-12 : RD: USE SVNSYS AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: LFNERR
      USE m_maxdim, ONLY: MAXSAT
      USE s_wtmsgs
      USE s_major
      USE f_svnsys
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IEPOCH, IFRQ  , IMSG  , INDFRQ, IPRNT2, IREF  , ISAT  ,
     1          ISATOK, ITITL2, IZERO , L5CLEA, MINCYC, MXCSLP,
     2          NMAX  , NOK   , NSAT  , NSEFF , NSLIP , NTONLY
C
      REAL*8    CYCLE , RHSXXX, STRP
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
C
      INTEGER*4   SVN(*),IWLSCR(*),LSTSLP(6,*)
      INTEGER*4   IWL(3),FRQAUX(*),SVNREF
      REAL*8      RHS(3,*),SLPLST(*),SLPXXX(2,*),RHSLOC(MAXSAT),CLOCK(*)
      REAL*8      CYCINT(3),SIGL12(2)
      CHARACTER*6 MXNSLP
      CHARACTER*1 FILFLG(3,*)
C
      INCLUDE 'COMFREQ.inc'
      COMMON/MCMSLP/MXCSLP,MXNSLP
C
C SET RMS FOR TRIPLE DIFFERENCE
C -----------------------------
      IFRQ=FRQAUX(INDFRQ)
      STRP=2*DSQRT(2.D0)*SIGL12(IFRQ)
C
C CHECK MAXIMUM NUMBER OF SATELLITES
C ----------------------------------
      IF(NSAT.GT.MAXSAT)THEN
        WRITE(LFNERR,10) NSAT,MAXSAT
10      FORMAT(/,' *** SR SNGSLP: TOO MANY SATELLITES',/,
     1                       16X,'NUMBER OF SATELLITES:',I4,/,
     2                       16X,'MAX. NUMBER ALLOWED :',I4,/)
        CALL EXITRC(2)
      END IF
C
C GOOD OBSERVATIONS ?
C -------------------
      NSEFF=0
C
      DO 15 ISAT=1,NSAT
        IF(RHS(INDFRQ,ISAT).NE.1.D20)THEN
          NSEFF=NSEFF+1
          RHSLOC(ISAT)=RHS(INDFRQ,ISAT)
        ELSE
          RHSLOC(ISAT)=1.D20
        END IF
15    CONTINUE
      IF(NSEFF.EQ.0)RETURN
C
C DEFINE CLOCK
C ------------
      CALL MAJOR(NSAT,RHSLOC,3*STRP,NOK,NMAX,RHSXXX,IREF)
C
      IF (SVNSYS(1,NSAT,SVN)) THEN
        IF (NMAX.LT.2 .OR. RHSXXX.GT.100.D0) THEN
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
      CLOCK(INDFRQ)=0
      ISATOK=0
C
      DO 17 ISAT=1,NSAT
        IF (RHSLOC(ISAT).NE.1.D20 .AND.
     1      DABS(RHSLOC(ISAT)-RHSXXX).LE.3*STRP) THEN
          CLOCK(INDFRQ)=CLOCK(INDFRQ)+RHSLOC(ISAT)
          ISATOK=ISATOK+1
        ENDIF
17    CONTINUE
      IF (ISATOK.GT.0) THEN
        CLOCK(INDFRQ)=CLOCK(INDFRQ)/ISATOK
      ENDIF
C
C CYCLE SLIPS ON SINGLE DIFFERENCE
C --------------------------------
      IWL(1)=IWLSCR(IFRQ)
      DO 30 ISAT=1,NSAT
        IF(RHSLOC(ISAT).NE.1.D20)THEN
          CYCLE=((RHSLOC(ISAT)-CLOCK(INDFRQ))/WLGT(IFRQ,SVN(ISAT)))
     1           *IWL(1)
          CYCINT(1)=DNINT(CYCLE)
C ACTION ACCORDING TO OPTIONS "NTONLY","L5CLEA"
          IF (FILFLG(INDFRQ,ISAT) .EQ. 'O') THEN
            IF (NTONLY .EQ. 1) GO TO 30
          END IF
C
          IF(CYCINT(1).NE.0.D0.OR.IZERO.EQ.1)THEN
            NSLIP=NSLIP+1
            IF(NSLIP.GT.MXCSLP)THEN
              WRITE(LFNERR,21) NSLIP,MXCSLP
21            FORMAT(/,' *** SR SNGSLP: TOO MANY CYCLE SLIPS',/,
     1                             16X,'NUMBER OF SLIPS >= :',I6,/,
     2                             16X,'MAX. NUMBER ALLOWED:',I6,/)
              CALL EXITRC(2)
            END IF
            LSTSLP(1,NSLIP)=IEPOCH
            LSTSLP(2,NSLIP)=SVN(ISAT)
            LSTSLP(3,NSLIP)=IFRQ
            LSTSLP(4,NSLIP)=IWL(1)
            LSTSLP(5,NSLIP)=0
            LSTSLP(6,NSLIP)=-1
            SLPLST(NSLIP)=CYCINT(1)
            SLPXXX(1,NSLIP)=CYCLE-CYCINT(1)
C
            IF(CYCINT(1).NE.0.D0.AND.IPRNT2.GE.2) THEN
              IF(DABS(CYCINT(1)).GT.MINCYC .AND. MINCYC.NE.-1) THEN
                IMSG=1
              ELSE
                IMSG=9
              ENDIF
              SVNREF=0
              CALL WTMSGS(IMSG,ITITL2,IEPOCH,SVN(ISAT),SVNREF,
     1                      IFRQ,IWL,CYCINT,SLPXXX(1,NSLIP))
            ENDIF
          END IF
        END IF
30    CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
