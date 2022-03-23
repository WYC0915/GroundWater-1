      MODULE s_RSTCLK
      CONTAINS

C*
      SUBROUTINE RSTCLK(IPRNT2,ITITL2,JMPOPT,TOLJMP,IEPOCH,NFRAUX,
     1                  FRQAUX,IWLSCR,NSAT  ,SVN   ,CLKOLD,NDIFF ,
     2                  EPOCLK,STANAM,TIMREF,IDELTT,DELTAT,NSLIP ,
     3                  LSTSLP,SLPLST,CLOCK ,CLKFLG,NCLKEV,LSTCLK)
CC
CC NAME       :  RSTCLK
CC
CC PURPOSE    :  IF THE CLOCK JUMP SINCE THE LAST EPOCH EXCEEDS
CC               100 M, USE CLOCK OF PREVIOUS EPOCH, APPLY CYCLE
CC               SLIPS
CC
CC PARAMETERS :
CC         IN :  IPRNT2 : PRINT LEVEL FOR CYCLE SLIP DETECT.  I*4
CC                        =0: NO MESSAGES PRINTED
CC                        =1: PRINT SUMMARY
CC                        =2: ALL MESSAGES PRINTED
CC               ITITL2 : TITLE PRINT FLAG                    I*4
CC                        =0: TITLE PRINTED ALREADY
CC                        =1: TITLE NOT PRINTED YET
CC               JMPOPT : CLOCK EVENT OPTIONS                  I*4(6)
CC                         (1): 0/1 ALLOW MS-JUMP CYCLE SLIPS
CC                         (2): MIN. SIZE OF A CLOCK EVENT (NS)
CC                         (3): MARK EPOCHS WITH CLOCK EVENTS
CC                              UP TO (IN S)
CC                         (4): 0/1 AMBIGUITIES FOR ALL SATELLITES
CC                         (5): 0/1 FLAG IF MS-JUMP IN FILE
CC                         (6): 0/1 FLAG IF A CLOCK EVENT IN FILE
CC               TOLJMP : TOLERANCE TO DETECT A MS-JUMP       R*8
CC               IEPOCH : EPOCH NUMBER                        I*4
CC               NFRAUX : NUMBER OF FREQUENCIES PROCESSED     I*4
CC               FRQAUX(I),I=1,..,NFRAUX: FREQUENCIES         I*4
CC               IWLSCR(L),L=1,2: WAVELENGTH FACTOR FOR       I*4
CC                        ORIGINAL CARRIER L
CC               NSAT   : NUMBER OF SATELLITES                I*4
CC               SVN(I),I=1,2,..,NSAT: SV NUMBERS             I*4
CC               CLKOLD(K),K=1,2,3: CLOCK OF PREVIOUS EPOCH   R*8
CC               NDIFF  : FILE DIFFERENCE LEVEL               I*4
CC               EPOCLK : CLOCK EPOCH DIFFERENCE              R*8
CC               STANAM : STATION NAME                        CH*16
CC               TIMREF : REFERENCE EPOCH OF THE FILE         R*8
CC               IDELTT : SAMPLING RATE OF THE FILE           I*4
CC               DELTAT(I,J): RCV CLOCK RECORD FROM OBS FILES R*8(2,2)
CC                        I=1 LAST EPOCH, I=2 ACTUAL EPOCH
CC                        J=1,2 CLOCK RECORD (1: RINEX, 2: CODSPP)
CC     IN/OUT :  NSLIP  : TOTAL NUMBER OF SLIPS DISCOVERED    I*4
CC               LSTSLP(K,I),K=1,..,6, I=1,2,..,NSLIP         I*4
CC                        SLIP DESCRIPTION:
CC                     (1,I): EPOCH NUMBER
CC                     (2,I): SV NUMBER
CC                     (3,I): FREQUENCY NUMBER
CC                     (4,I): WAVELENGTH FACTOR
CC                     (5,I): WAVELENGTH FACTOR OF L5
CC                            IF AN L1 AND L2 SLIP OCCURS AT
CC                            THE SAME EPOCH FOR THE SAME
CC                            SATELLITE:
CC                             =3 FOR L1, INDICATING THAT
CC                                AN L2 SLIP OCCURED TOO
CC                             =WAVELENGTH OF L5 FOR L2
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
CC               CLOCK(L),L=1,2,..,5: BIASED CLOCK            R*8
CC               CLKFLG : CLOCK FLAG                          CH*1
CC                        'G': OK, NO EVENT
CC                        'J': MS-JUMP, REPAIRED AS CYCLE SLIP
CC                        'B': OTHER BIG CLOCK VALUE
CC               NCLKEV : NUMBER OF CLOCK EVENTS IN LIST      I*4
CC               LSTCLK : (1,I): EPOCH OF THE CLOCK EVENT
CC                        (2,I): MAGNITUDE OF THE CLOCK EVENT
CC                               IN NANOSECONDS
CC                        (3,I): CLOCK ESTIMATE FROM CODSPP
CC                               IN NANOSECONDS
CC                        (4,I): ACTION (0:NONE,1:MARK,2:AMB,3:CYC)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  88/05/11 12:01
CC
CC CHANGES    :  16-JUN-93 : ??: ADD OPTION "ALL SATELLITES"=99
CC               10-AUG-94 : MR: CALL EXITRC
CC               29-NOV-95 : MR: CHANGE CLOCK RESET LEVEL FROM
CC                               10000 TO 100 METERS BECAUSE OF
CC                               PROBLEMS WITH AMB. INIT. IN GPSEST
CC               21-JAN-97 : MR: REMOVE "NSAT","SVN","RHS" PARAMETERS
CC               28-JUL-98 : HH: MODIFICATIONS FOR GLONASS
CC               19-JAN-02 : MR: 100->10000 CLOCK RESET LEVEL
CC               08-JUL-02 : RD: NO CLOCK CYCLE SLIP FOR ZD-CASE
CC               01-AUG-02 : RD: CORRECT MILLI-SEC JUMPS FOR ZD PHASE
CC               03-NOV-02 : RD: 10000->100 CLOCK RESET LEVEL
CC               04-DEC-02 : RD: ADD CLOCK EVENT FLAG AND LIST
CC               25-JUL-03 : RD: MS-JUMP BUG FIXED
CC               10-NOV-03 : RD: TOLJMP=0D0->NO MS-JUMPS ANYMORE
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               08-Aug-05 : HB: Use new SR TIMST2 (module)
CC               02-JUL-10 : RD: NCLKEV MAY BE EXCEED MXCCLK
CC               26-MAR-12 : RD: USE WTMSGS AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,  ONLY: LFNERR
      USE d_const, ONLY: C
      USE s_wtmsgs
      USE s_timst2
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 ICARR , IDELTT, IEPOCH, IFRQ  , IMSG  , IPRNT2, IRFSAT,
     1          ITITL2, MSGSVN, MSJUMP, MXCCLK, MXCSLP, NCLKEV,
     2          NDIFF , NFRAUX, NSAT  , NSLIP
C
      REAL*8    EPOCLK, TEST  , TIMREF, TOLJMP
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      REAL*8       SLPLST(*),CLOCK(*),CLKOLD(*)
      REAL*8       JUMP(3), XSLIP(3)
      REAL*8       DELTAT(2,2),DTPROT(2)
      INTEGER*4    IWLSCR(*),LSTSLP(6,*),FRQAUX(*),SVN(*),LSTCLK(4,*)
      INTEGER*4    JMPOPT(6),IWLF(3)
      CHARACTER*1  CLKFLG
      CHARACTER*6  MXNSLP,MXNCLK
      CHARACTER*16 STANAM
      CHARACTER*19 EPOSTR
C
      INCLUDE 'COMFREQ.inc'
      COMMON/MCMSLP/MXCSLP,MXNSLP
      COMMON/MCMCLK/MXCCLK,MXNCLK
C
C EPOCH CLOCK ESTIMATE IS A MILLI-SECOND JUMP
C -------------------------------------------
      IF (NDIFF.EQ.0.AND.EPOCLK.NE.0D0) THEN

C COMPUTE THE SIZE OF A MS-JUMP
        MSJUMP=NINT(EPOCLK*1D-6)
C
C MAKE SURE THAT IT IS REALLY N*1MS
        IF (TOLJMP.EQ.0D0.OR.
     1      DABS(EPOCLK*1D-6-DBLE(MSJUMP)).GT.TOLJMP) MSJUMP=0

        IF ((JMPOPT(1).EQ.1.OR.JMPOPT(5).EQ.1).AND.
     1       ABS(MSJUMP).GT.0) THEN
C
C SET THE CLOCK FLAG
C ------------------
          IF (CLKFLG.EQ.'G') CLKFLG='J'
C
C WRITE A WARNING MESSAGE
C -----------------------
          CALL TIMST2(2,1,TIMREF+(IEPOCH-1)*IDELTT/86400d0,EPOSTR)
          WRITE(lfnerr,'(/,A,4(/,16X,A),3(/,16X,A,F12.6,A),/,16X,A,/)')
     1    ' ### SR RSTCLK: ' //
     2    'CLOCK JUMP IN THE ZERO-DIFFERENCE PHASE DATA FOUND!',
     3    'THERE SEEMS TO BE AN INCONSISTENCE BETWEEN THE CLOCK',
     4    'FOR CODE AND PHASE DATA (ERROR IN RINEX FILE)',
     5    'STATION NAME:  ' // STANAM,
     6    'EPOCH:         ' // EPOSTR,
     7    'RINEX  CLOCK:  ',(DELTAT(2,1)-DELTAT(1,1))*1D3,' MS',
     8    'CODSPP CLOCK:  ',(DELTAT(2,1)-DELTAT(1,1)+
     9                       DELTAT(2,2)-DELTAT(1,2))*1D3,' MS',
     1    'MAUPRP CLOCK:  ',-EPOCLK*1D-6,' MS',
     2    'THE PHASE DATA ARE CORRECTED FOR THIS CLOCK JUMP'

C
C PUT CLOCK EVENT INTO THE LIST
C -----------------------------
          NCLKEV=NCLKEV+1
          IF(NCLKEV.GT.MXCCLK)THEN
            WRITE(LFNERR,22)NCLKEV,MXCCLK,STANAM
22          FORMAT(/,' *** SR RSTCLK: TOO MANY CLOCK EVENTS',/,
     1                           16X,'NUMBER OF EVENTS >= :',I6,/,
     2                           16X,'MAX.NUMBER OF EVENTS:',I6,/,
     3                           16X,'STOP PROCESSING THE STATION:',A,/)
C
          ELSE
            LSTCLK(1,NCLKEV)=IEPOCH
            LSTCLK(2,NCLKEV)=-NINT(EPOCLK)
            LSTCLK(3,NCLKEV)= NINT((DELTAT(2,1)-DELTAT(1,1)+
     1                              DELTAT(2,2)-DELTAT(1,2))*1D9)
            LSTCLK(4,NCLKEV)=3
          END IF
C
C ADD "CYCLE SLIPS" FOR ALL FREQUENCIES
          IF (JMPOPT(1).EQ.1) THEN
            DO IFRQ=1,NFRAUX
              IF (IFRQ.GT.2) EXIT

              ICARR=FRQAUX(IFRQ)
              IWLF(1)=IWLSCR(ICARR)

              JUMP(1)=-DBLE(MSJUMP)*1D-3/(WLGT(ICARR,SVN(1))/IWLF(1))*C
              CLOCK(ICARR)=CLOCK(ICARR)-JUMP(1)*WLGT(ICARR,SVN(1))/
     1                     IWLF(1)

              NSLIP=NSLIP+1
              IF(NSLIP.GT.MXCSLP)THEN
                WRITE(LFNERR,20)NSLIP,MXCSLP
20              FORMAT(/,' *** SR RSTCLK: TOO MANY CYCLE SLIPS',/,
     1                               16X,'NUMBER OF SLIPS >= :',I6,/,
     2                               16X,'MAX.NUMBER OF SLIPS:',I6,/)
                CALL EXITRC(2)
              END IF
              LSTSLP(1,NSLIP)=IEPOCH
              LSTSLP(2,NSLIP)=99
              LSTSLP(3,NSLIP)=ICARR
              LSTSLP(4,NSLIP)=IWLF(1)
              LSTSLP(5,NSLIP)=0
              LSTSLP(6,NSLIP)=-5
              SLPLST(NSLIP)=JUMP(1)
C
              IF(IPRNT2.GE.2) THEN
                IMSG=10
                IRFSAT=0
                XSLIP=0.D0
                MSGSVN=99
                DTPROT(1:2)=DELTAT(2,1:2)-DELTAT(1,1:2)
                CALL WTMSGS(IMSG,ITITL2,IEPOCH,MSGSVN,IRFSAT,
     1                      ICARR,IWLF,JUMP,XSLIP)
              ENDIF
            ENDDO
          ENDIF
C
C WRITE A MESSAGE (BIG CLOCK VALUE, BUT NO MS JUMP)
        ELSE IF (JMPOPT(2).GT.0.AND.
     1           NINT(DABS(EPOCLK)).GT.JMPOPT(2)) THEN
C
          CALL TIMST2(2,1,TIMREF+(IEPOCH-1)*IDELTT/86400d0,EPOSTR)
          WRITE(lfnerr,'(/,A,4(/,16X,A),3(/,16X,A,F12.6,A),/)')
     1      ' ### SR RSTCLK: ' //
     2      'CLOCK JUMP IN THE ZERO-DIFFERENCE PHASE DATA FOUND!',
     3      'THERE SEEMS TO BE AN INCONSISTENCE BETWEEN THE CLOCK',
     4      'FOR CODE AND PHASE DATA (ERROR IN RINEX FILE)',
     5      'STATION NAME:  ' // STANAM,
     6      'EPOCH:         ' // EPOSTR,
     7      'RINEX  CLOCK:  ',(DELTAT(2,1)-DELTAT(1,1))*1D3,' MS',
     8      'CODSPP CLOCK:  ',(DELTAT(2,1)-DELTAT(1,1)+
     9                         DELTAT(2,2)-DELTAT(1,2))*1D3,' MS',
     1      'MAUPRP CLOCK:  ',-EPOCLK*1D-6,' MS'
C
C PUT CLOCK EVENT INTO THE LIST
C -----------------------------
          NCLKEV=NCLKEV+1
          IF(NCLKEV.GT.MXCCLK)THEN
            WRITE(LFNERR,23)NCLKEV,MXCCLK,STANAM
23          FORMAT(/,' *** SR RSTCLK: TOO MANY CLOCK EVENTS',/,
     1                           16X,'NUMBER OF EVENTS >= :',I6,/,
     2                           16X,'MAX.NUMBER OF EVENTS:',I6,/,
     3                           16X,'STOP PROCESSING THE STATION:',A,/)
C
          ELSE
            LSTCLK(1,NCLKEV)=IEPOCH
            LSTCLK(2,NCLKEV)=-NINT(EPOCLK)
            LSTCLK(3,NCLKEV)= NINT((DELTAT(2,1)-DELTAT(1,1)+
     1                              DELTAT(2,2)-DELTAT(1,2))*1D9)
            LSTCLK(4,NCLKEV)=0
          ENDIF
C
C SET THE CLOCK FLAG
C ------------------
          CLKFLG='B'
C
        ENDIF
      ENDIF
C
C LOOP OVER FREQUENCIES BEING PROCESSED
C -------------------------------------
      DO 100 IFRQ=1,NFRAUX
        ICARR=FRQAUX(IFRQ)
C
C L1/L2 CARRIERS
C --------------
        IF(IFRQ.LE.2) THEN
          IWLF(1)=IWLSCR(ICARR)
          TEST=CLKOLD(ICARR)-CLOCK(ICARR)
C
C CLOCK INDUCED CYCLE SLIPS, SD-CASE ONLY
          IF(DABS(TEST).GT.100.D0.AND.NDIFF.EQ.1) THEN
            JUMP(1)=-DNINT(TEST/(WLGT(ICARR,SVN(1))/IWLF(1)))            GLONASS
            CLOCK(ICARR)=CLOCK(ICARR)-JUMP(1)*WLGT(ICARR,SVN(1))/IWLF(1) GLONASS
            NSLIP=NSLIP+1
            IF(NSLIP.GT.MXCSLP)THEN
              WRITE(LFNERR,21)NSLIP,MXCSLP
21            FORMAT(/,' *** SR RSTCLK: TOO MANY CYCLE SLIPS',/,
     1                             16X,'NUMBER OF SLIPS >= :',I6,/,
     2                             16X,'MAX.NUMBER OF SLIPS:',I6,/)
              CALL EXITRC(2)
            END IF
            LSTSLP(1,NSLIP)=IEPOCH
            LSTSLP(2,NSLIP)=99
            LSTSLP(3,NSLIP)=ICARR
            LSTSLP(4,NSLIP)=IWLF(1)
            LSTSLP(5,NSLIP)=0
            LSTSLP(6,NSLIP)=-3
            SLPLST(NSLIP)=JUMP(1)
C
            IF(IPRNT2.GE.2) THEN
              IMSG=2
              IRFSAT=0
              XSLIP=0.D0
              MSGSVN=99
              CALL WTMSGS(IMSG,ITITL2,IEPOCH,MSGSVN,IRFSAT,
     1                    ICARR,IWLF,JUMP,XSLIP)
            ENDIF
          ENDIF
C
C L5 CARRIER
C ----------
        ELSE
CC        CLOCK(3)=WLGTH(5)*(CLOCK(1)/(WLGTH(1)/IWLSCR(1))-
CC   1                       CLOCK(2)/(WLGTH(2)/IWLSCR(2)))
          CLOCK(3)=WLGT(5,SVN(1))*(CLOCK(1)/WLGT(1,SVN(1))-             GLONASS
     1                       CLOCK(2)/WLGT(2,SVN(1)))                   GLONASS

        END IF
100   CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
