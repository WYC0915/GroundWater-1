      MODULE s_DSPCYC
      CONTAINS

C*
      SUBROUTINE DSPCYC(ITITLE,ISUMRY,INEW,ICYTYP,ICYFRQ,ICYSVN,CYCMAX,
     1                  NFREQ,NSATEL,NUMSAT,
     2                  NSLIP,LSTSLP,SLPLST,SLPXXX,IONO,IRC)
CC
CC NAME       :  DSPCYC
CC
CC PURPOSE    :  DISPLAY CYCLE SLIPS AND ADDITIONAL INFORMATION
CC               ACCORDING TO THE OPTIONS "ICYTYP,ICYFRQ,ICYSVN,CYCMAX"
CC
CC PARAMETERS :
CC         IN :  ITITLE : DISPLAY GENERAL TITLE               I*4
CC                        =0: NO GENERAL TITLE
CC                        =1: GENERAL TITLE DISPLAYED
CC               ISUMRY : DISPLAY SUMMARY OF CYCLE SLIPS      I*4
CC                        =0: NO SUMMARY
CC                        =1: SHORT SUMMARY DISPLAYED
CC                        =2: LONG  SUMMARY DISPLAYED
CC               INEW   : DISPLAY CHANGES OF THE LAST RUN
CC                        =0: DISPLAY ALL CYCLE SLIPS
CC                        =1: DISPLAY NEW CYCLE SLIPS ONLY
CC               ICYTYP : TYPE OF CYCLE SLIP                  I*4
CC                        =0: ALL CYCLE SLIPS
CC                        =1: SLIPS OF SINGLE FREQ. DETECTION
CC                        =2: SLIPS OF DUAL   FREQ. DETECTION
CC                        =3: CLOCK CORRECTIONS
CC                        =4: USER DEFINED
CC               ICYFRQ : FREQUENCY OF CYCLE SLIPS            I*4
CC                        =0: ALL (BOTH) FREQUENCIES
CC                        =1: SLIPS OF FREQUENCY L1
CC                        =2: SLIPS OF FREQUENCY L2
CC               ICYSVN : SATELLITE(S) WITH CYCLE SLIPS       I*4
CC                        =0: ALL SATELLITES
CC                        =I: SATELLITE NUMBER I
CC               CYCMAX : MAXIMUM SIZE OF CYCLE SLIPS TO BE   R*8
CC                        DISPLAYED
CC               NFREQ  : NUMBER OF FREQUENCIES IN FILE       I*4
CC               NSATEL : TOTAL NUMBER OF SATELLITES          I*4
CC               NUMSAT(I),I=1,..,NSATEL: SATELLITE NUMBERS   I*4
CC               NSLIP  : NUMBER OF SLIPS DETECTED            I*4
CC               LSTSLP(K,IS),K=1,..,6, IS=1,2,..,NSLIP:      I*4
CC                        SLIP DESCRIPTION
CC                        K=1: EPOCH NUMBER
CC                        K=2: SV NUMBER
CC                        K=3: FREQUENCY NUMBER
CC                        K=4: WAVELENGTH FACTOR
CC                        K=5: WAVELENGTH FACTOR OF L5
CC                             IF AN L1 AND L2 SLIP OCCURS AT
CC                             THE SAME EPOCH FOR THE SAME
CC                             SATELLITE:
CC                              =3 FOR L1, INDICATING THAT
CC                                 AN L2 SLIP OCCURED TOO
CC                              =WAVELENGTH OF L5 FOR L2
CC                        K=6: CYCLE SLIP DETECTED BY
CC                             =1: SINGLE FREQ. SLIP DETECTION
CC                             =2: DUAL   FREQ. SLIP DETECTION
CC                             =3: CLOCK
CC                             =4: USER
CC                             =5: MILLI-SEC JUMP
CC                             ALL CYCLE SLIPS DETECTED IN THE
CC                             LATEST RUN HAVE A NEGATIVE SIGN
CC               SLPLST(IS),IS=1,2,..,NSLIP: SIZE OF SLIP IS  R*8
CC               SLPXXX(2,I),I=1,..,NSLIP: REAL VALUED        R*8
CC                        ESTIMATES FOR SLIP I - INTEGER SLIP
CC                        SINGLE FREQ.: L1/L2 ESTIMATES
CC                        DUAL   FREQ.: L1/L2 AND L5 ESTIMATES
CC               IONO(K,I),K=1,2,3, I=1,2,..,NSLIP            R*8
CC                        K=1: IONOSPHERE MEAN FROM L1, L2
CC                        K=2: IONOSPHERE DIFF. FROM L1, L2
CC                        K=3: RESIDUAL IN L3
CC               IRC    : RETURN CODE                         I*4
CC                        =0 : AT LEAST ONE CYCLE SLIP
CC                             DISPLAYED
CC                        =1 : NO CYCLE SLIP DISPLAYED WITH
CC                             ENTERED SPECIFICATIONS
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  89/08/10 16:09
CC
CC CHANGES    :  05-JUN-92 : ??: CHANGES FOR THE NEW MAUPRP VERSION
CC               16-JUN-93 : ??: ADD OPTION "ALL SATELLITES"=99
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               10-AUG-94 : MR: CALL EXITRC
CC               23-SEP-97 : DI: USE MAXSAT.inc
CC               01-AUG-02 : RD: HANDLE MILLI-SEC JUMPS FOR ZD PHASE
CC               06-DEC-02 : RD: ROUND SIZE OF MS-JUMPS
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               29-FEB-08 : RD: INDICATE CYCLE SLIP PROBLEMS
CC               11-JUN-12 : RD: MORE DIGITS FOR NUMBER OF CYCLE SLIPS
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1989     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE m_maxdim, ONLY: MAXSAT
      USE d_const, ONLY: C
      USE s_maxtst
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 ICYFRQ, ICYSVN, ICYTYP, IEPO  , IFIRST, IFLG12, IFRQ  ,
     1          IFRQ5 , INEW  , IOLTYP, IRC   , ISATEL, ISLP  , ISUMRY,
     2          ISVN  , ITITL1, ITITLE, ITYP  , ITYPA , IWL5  , IWLF  ,
     3          IWLF1 , IWLF2 , IWLF5 , MAXEVT, MSJUMP, MXCSAT, NFREQ ,
     4          NSAT1 , NSAT2 , NSATEL, NSLDSP, NSLIP
C
      REAL*8    CYCMAX, SLIP5
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MAXEVT=5)
C MAXEVT: MAXIMUM NUMBER OF DIFFERENT EVENT TYPES
C
C
      CHARACTER*6  MXNSAT
      CHARACTER*5  TSVN
      CHARACTER*3  CYCTXT(2*MAXEVT)
      CHARACTER*1  NEWTXT
C
      REAL*8       SLPLST(*),SLPXXX(2,*),IONO(3,*)
C
      INTEGER*4    NUMSAT(*),LSTSLP(6,*),NCYCLE(MAXEVT,2,MAXSAT)
      INTEGER*4    NCYCSA(2,MAXSAT),NCYCTT(MAXEVT,2),NCYCFR(2,2)
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
      INCLUDE 'COMFREQ.inc'
C
      DATA IFIRST/1/
      DATA CYCTXT/'SNG','DUA','CLK','USR','JMP',
     1            '*R*','*R*','*R*','*R*','*R*'/
C
C INITIALIZATION OF DEFAULTS
C --------------------------
      IF(IFIRST.EQ.1) THEN
        IFIRST=0
        CALL MAXTST(1,'DSPCYC',MXNSAT,MAXSAT,MXCSAT,IRC)
        IF(IRC.NE.0) CALL EXITRC(2)
      ENDIF
C
C DISPLAY SUMMARY OF CYCLE SLIPS
C ------------------------------
      IF(ISUMRY.GE.1) THEN
C
C WRITE GENERAL TITLE
        IF(ITITLE.EQ.1) THEN
          WRITE(LFNPRT,1)
1         FORMAT(//,1X,72('-'),
     1            /,' SUMMARY OF CYCLE SLIPS',
     2            /,1X,72('-'),/)
        ENDIF
C
C INITIALIZATION
        DO 30 IFRQ=1,NFREQ
          NCYCFR(IFRQ,:)=0
          DO 20 ITYP=1,MAXEVT
            NCYCTT(ITYP,IFRQ)=0
            DO 10 ISATEL=1,NSATEL
              NCYCLE(ITYP,IFRQ,ISATEL)=0
              NCYCSA(IFRQ,ISATEL)=0
10          CONTINUE
20        CONTINUE
30      CONTINUE
C
C SUMMARY
        NSLDSP=0
        DO 40 ISLP=1,NSLIP
          ISVN=LSTSLP(2,ISLP)
          IFRQ=LSTSLP(3,ISLP)
          ITYP=MOD(IABS(LSTSLP(6,ISLP)),1000)
          IF(ITYP.GT.MAXEVT) GOTO 40
          IF(INEW.EQ.1.AND.LSTSLP(6,ISLP).GT.0) GOTO 40
          IF (ISVN.NE.99) THEN
            DO 35 ISATEL=1,NSATEL
              IF(NUMSAT(ISATEL).EQ.ISVN) GOTO 38
35          CONTINUE
38          NSAT1=ISATEL
            NSAT2=ISATEL
          ELSE
            NSAT1=1
            NSAT2=NSATEL
          ENDIF
          DO 39 ISATEL=NSAT1,NSAT2
            IF(IABS(LSTSLP(6,ISLP)).LT.1000) THEN
              NSLDSP=NSLDSP+1
              NCYCLE(ITYP,IFRQ,ISATEL)=NCYCLE(ITYP,IFRQ,ISATEL)+1
              NCYCSA(IFRQ,ISATEL)=NCYCSA(IFRQ,ISATEL)+1
              NCYCTT(ITYP,IFRQ)=NCYCTT(ITYP,IFRQ)+1
              NCYCFR(IFRQ,1)=NCYCFR(IFRQ,1)+1
            ELSE
              NCYCFR(IFRQ,2)=NCYCFR(IFRQ,2)+1
            ENDIF
39        CONTINUE
40      CONTINUE
C
C DISPLAY SUMMARY
        DO 50 IFRQ=1,NFREQ
          IF (NCYCFR(IFRQ,2).EQ.0) THEN
            WRITE(LFNPRT,41) IFRQ,NCYCFR(IFRQ,1)
41          FORMAT(' NUMBER OF SLIPS IN L',I1,':',I7)
          ELSE
            WRITE(LFNPRT,42) IFRQ,NCYCFR(IFRQ,1),NCYCFR(IFRQ,2)
42          FORMAT(' NUMBER OF SLIPS IN L',I1,':',I7,5X,
     1             I7,' CYCLE SLIPS REMOVED FROM LIST')
          ENDIF
50      CONTINUE
        IF(ISUMRY.GE.2) THEN
          IF(NSLDSP.EQ.0) THEN
            IRC=1
            GOTO 999
          ENDIF
C
          IF(NFREQ.EQ.1 ) THEN
            WRITE(LFNPRT,51)
51          FORMAT(/,1X,'SATELLITE    SING.FRQ.  CLOCK     USER',
     1                  '  MS-JUMP      TOTAL',
     2             /,1X,79('-'),/)
            DO 60 ISATEL=1,NSATEL
              WRITE(LFNPRT,59) NUMSAT(ISATEL),
     1                         NCYCLE(1,1,ISATEL),NCYCLE(3,1,ISATEL),
     2                         NCYCLE(4,1,ISATEL),NCYCLE(5,1,ISATEL),
     3                         NCYCSA(1,ISATEL)
59            FORMAT(I6,4X,4I9,I11)
60          CONTINUE
            WRITE(LFNPRT,61) NCYCTT(1,1),NCYCTT(3,1),NCYCTT(4,1),
     1                       NCYCTT(5,1),NCYCFR(1,1)
61          FORMAT(/,1X,79('-'),
     1             /,1X,'  TOTAL   ',4I9,I11,
     2             /,1X,79('-'))
          ELSE
            WRITE(LFNPRT,62)
62          FORMAT(/,14X,'SING.FRQ.  DUAL FRQ.   CLOCK       USER',
     1                   '    MS-JUMP        TOTAL',
     2             /,1X ,'SATELLITE',4(4X,'#L1 #L2'),6X,'#L1 #L2',
     3             /,1X,79('-'),/)
            DO 70 ISATEL=1,NSATEL
              WRITE(LFNPRT,69) NUMSAT(ISATEL),
     1                       ((NCYCLE(ITYP,IFRQ,ISATEL),IFRQ=1,2),
     2                                                  ITYP=1,MAXEVT),
     3                        (NCYCSA(IFRQ,ISATEL),IFRQ=1,2)
69            FORMAT(I6,4X,5(I7,I4),I9,I4)
70          CONTINUE
            WRITE(LFNPRT,71)
     1           ((NCYCTT(ITYP,IFRQ),IFRQ=1,2),ITYP=1,MAXEVT),
     1            (NCYCFR(IFRQ,1),IFRQ=1,2)
71          FORMAT(/,1X,79('-'),
     1             /,1X,'  TOTAL  ',5(I7,I4),I9,I4,
     2             /,1X,79('-'))
          ENDIF
        ENDIF
        IRC=0
        GOTO 999
      ENDIF
C
C DISPLAY LIST OF CYCLE SLIP
C --------------------------
C
C WRITE GENERAL TITLE
      IF(ITITLE.EQ.1) THEN
        WRITE(LFNPRT,72)
72      FORMAT(//,1X,72('-'),
     1          /,' CYCLE SLIPS AND ADDITIONAL INFORMATION',
     2          /,1X,72('-'))
      ENDIF
C
      IF(NSLIP.EQ.0.AND.INEW.EQ.0) THEN
        WRITE(LFNPRT,73)
73      FORMAT(/,' NO CYCLE SLIPS FOUND',/)
        IRC=1
        GOTO 999
      ENDIF
C
      ITITL1=1
      IFLG12=0
      IOLTYP=-1
      IFRQ5 =5
C
C LOOP OVER ALL SLIPS
      DO 100 ISLP=1,NSLIP
        IEPO=LSTSLP(1,ISLP)
        ISVN=LSTSLP(2,ISLP)
        IFRQ=LSTSLP(3,ISLP)
        IWLF=LSTSLP(4,ISLP)
        IWL5=LSTSLP(5,ISLP)
        ITYPA=MOD(IABS(LSTSLP(6,ISLP)),1000)
        ITYP=ITYPA
        IF(ITYP.GT.MAXEVT) ITYP=ITYP-MAXEVT
C
        IF(INEW.EQ.1.AND.LSTSLP(6,ISLP).GT.0) GOTO 100
C
        IF(ICYTYP.NE.0.AND.ITYP.NE.ICYTYP)                GOTO 100
        IF(ICYFRQ.NE.0.AND.IFRQ.NE.ICYFRQ)                GOTO 100
        IF(ICYSVN.NE.0.AND.ISVN.NE.ICYSVN.AND.ISVN.NE.99) GOTO 100
        IF(CYCMAX.LT.DABS(SLPLST(ISLP)))                  GOTO 100
C
C TITLE
        IF(ITITL1.EQ.1) THEN
          ITITL1=0
          IF(NFREQ.EQ.1) THEN
            WRITE(LFNPRT,74)
74          FORMAT(/,1X,'NUMB  TYP N  EPOCH SAT FRQ WLF        SLIP   ',
     1                'FRAC',
     3          /,1X,72('-'))
          ELSE
            WRITE(LFNPRT,75)
75          FORMAT(/,1X,'NUMB  TYP N  EPOCH SAT FRQ WLF        SLIP   ',
     1                'FRAC     RES.L3     IONOS',
     2          /,55X,'  (M)       (M)',
     3          /,1X,72('-'))
          ENDIF
        ENDIF
C
C ALL SATELLITES OR SATELLITE NUMBER
        IF (ISVN.EQ.99) THEN
          TSVN='  ALL'
        ELSE
          WRITE(TSVN,'(I5)') ISVN
        ENDIF
C
C NEW CYCLE SLIP INDICATOR
        IF(IABS(LSTSLP(6,ISLP)).GT.1000) THEN
          NEWTXT='-'
        ELSE IF(LSTSLP(6,ISLP).GT.0) THEN
          NEWTXT=' '
        ELSE
          NEWTXT='*'
        ENDIF
C
C BLANK LINE, IF NEW TYPE
        IF(ITYP.NE.IOLTYP) WRITE(LFNPRT,'( )')
C
C SLIPS FROM SINGLE FREQUENCY PREPROCESSING
C -----------------------------------------
        IF(ITYP.EQ.1) THEN
          WRITE(LFNPRT,76) ISLP,CYCTXT(ITYPA),NEWTXT,IEPO,
     1                     TSVN,IFRQ,IWLF,SLPLST(ISLP),SLPXXX(1,ISLP)
76        FORMAT(I5,2X,A3,1X,A1,I6,A5,I3,I4,F13.0,F7.2,2X,3(F7.3))
          IFLG12=0
C
C SLIPS FROM DUAL FREQUENCY PREPROCESSING
C ---------------------------------------
        ELSE IF(ITYP.EQ.2) THEN
C
C L1 SLIP OF DUAL FREQ. PREPROCESSING
          IF(IFRQ.EQ.1) THEN
C
C WRITE L1 LINE
            WRITE(LFNPRT,77) ISLP,CYCTXT(ITYPA),NEWTXT,IEPO,
     1                       TSVN,IFRQ,IWLF,SLPLST(ISLP),SLPXXX(1,ISLP),
     2                       IONO(3,ISLP),IONO(1,ISLP)
77        FORMAT(I5,2X,A3,1X,A1,I6,A5,I3,I4,F13.0,F7.2,2X,2(F9.3))
C
            IFLG12=0
            IF(IWL5.EQ.3) THEN
              IF((ICYFRQ.EQ.0.OR.LSTSLP(3,ISLP+1).EQ.ICYFRQ).AND.
     1           (ICYSVN.EQ.0.OR.LSTSLP(2,ISLP+1).EQ.ICYSVN).AND.
     2           (CYCMAX.GE.DABS(SLPLST(ISLP+1)))) THEN
C
C L2 SLIP BELONGING TO L1 SLIP FOLLOWS
                IFLG12=1
              ENDIF
            ENDIF
            IF(IFLG12.EQ.0) THEN
C
C WRITE L5 INFO SINCE L2 SLIP OF THE SAME EPOCH IS NOT DISPLAYED
              IF(IWL5.EQ.3) THEN
                IWLF1=LSTSLP(4,ISLP)
                IWLF2=LSTSLP(4,ISLP+1)
                IF(IWLF1.EQ.IWLF2) THEN
                  SLIP5=SLPLST(ISLP)-SLPLST(ISLP+1)
                ELSE
                  SLIP5=2*SLPLST(ISLP)-SLPLST(ISLP+1)
                ENDIF
                IWLF5=LSTSLP(5,ISLP+1)
              ELSE
                SLIP5=SLPLST(ISLP)
                IWLF5=IWL5
              ENDIF
              WRITE(LFNPRT,81) IFRQ5,IWLF5,SLIP5,SLPXXX(2,ISLP)
81            FORMAT(23X,I3,I4,F13.0,F7.2)
            ENDIF
          ELSE
C
C L2 SLIP OF DUAL FREQUENCY PROCESSING
            IF(IFLG12.EQ.1) THEN
              WRITE(LFNPRT,86) ISLP,CYCTXT(ITYPA),NEWTXT,IFRQ,IWLF,
     1                         SLPLST(ISLP),SLPXXX(1,ISLP)
86            FORMAT(I5,2X,A3,1X,A1,11X,I3,I4,F13.0,F7.2)
            ELSE
              WRITE(LFNPRT,77) ISLP,CYCTXT(ITYPA),NEWTXT,IEPO,TSVN,
     1                         IFRQ,IWLF,SLPLST(ISLP),SLPXXX(1,ISLP),
     2                         IONO(3,ISLP),IONO(1,ISLP)
            ENDIF
            IWLF5=IWL5
            IF(ISLP.EQ.1) THEN
              SLIP5=-SLPLST(ISLP)
            ELSE IF(LSTSLP(5,ISLP-1).NE.3) THEN
              SLIP5=-SLPLST(ISLP)
            ELSE
              IWLF1=LSTSLP(4,ISLP-1)
              IWLF2=LSTSLP(4,ISLP)
              IF(IWLF1.EQ.IWLF2) THEN
                SLIP5=SLPLST(ISLP-1)-SLPLST(ISLP)
              ELSE
                SLIP5=2*SLPLST(ISLP-1)-SLPLST(ISLP)
              ENDIF
            ENDIF
            WRITE(LFNPRT,81) IFRQ5,IWLF5,SLIP5,SLPXXX(2,ISLP)
            IFLG12=0
          ENDIF
C
C MILLI-SEC. JUMP
C ---------------
        ELSE IF (ITYP.EQ.5) THEN
          MSJUMP=NINT(SLPLST(ISLP)*(WLGT(IFRQ,NUMSAT(1))/IWLF)/C*1D3)
          WRITE(LFNPRT,78) ISLP,CYCTXT(ITYPA),NEWTXT,IEPO,TSVN,
     1                     IFRQ,IWLF,SLPLST(ISLP),'(EQUIVALENT TO ',
     2                     DBLE(MSJUMP),' MS)'
78        FORMAT(I5,2X,A3,1X,A1,I6,A5,I3,I4,F16.3,3X,A,F5.0,A)
          IFLG12=0
C
C SLIPS DUE TO CLOCK OR USER DEFINED
C ----------------------------------
        ELSE
          WRITE(LFNPRT,76) ISLP,CYCTXT(ITYPA),NEWTXT,IEPO,TSVN,
     1                     IFRQ,IWLF,SLPLST(ISLP)
          IFLG12=0
        ENDIF
        IOLTYP=ITYP
100   CONTINUE
C
C NO MATCHES ?
      IF(ITITL1.EQ.1.AND.INEW.EQ.0) THEN
        WRITE(LFNPRT,103)
103     FORMAT(/,' NO SLIPS FOUND WITH ENTERED SPECIFICATIONS',/)
        IRC=1
        GOTO 999
      ENDIF
      WRITE(LFNPRT,104)
104   FORMAT(/,1X,72('-'))
      IRC=0
C
C RETURN
999   RETURN
      END SUBROUTINE

      END MODULE
