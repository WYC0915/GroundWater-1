      MODULE s_PRIREC
      CONTAINS

C*
      SUBROUTINE PRIREC(TITLES, NCLREQ, ISTCLK, ISACLK, STNAME, NCLK  ,
     1                  IBIAS , CLKWGT, CLFRTO, CLKHED, NCLKST, NCLKSA,
     2                  CLKSYS, NOINCLK,SECIPL, NPAR  , LOCQ  , TIMISB,
     3                  NFTOT , NDIFF , STFIL , RECTYP, IRUNIT)
CC
CC NAME       :  PRIREC
CC
CC PURPOSE    :  PRINT RECEIVER CLOCK OFFSETS AND EPOCH CLOCK PARAMETERS
CC
CC PARAMETERS :
CC         IN :  TITLES(I),I=1,2: TITLE LINES                 CH*132
CC               NCLREQ : NUMBER OF CLOCK PARAMETER REQUESTS  I*4
CC               ISTCLK(I),I=1,..,NCLREQ: STATION NUMBERS     I*4
CC                        CLOCK CORRECTIONS
CC               ISACLK(I),I=1,..,NCLREQ: SATELLITE NUMBERS   I*4
CC                        FOR SLR TIME-BIAS
CC               STNAME(I),I=1,..,NSTAT: STATION NAMES FOR    CH*16
CC                        ALL STATIONS
CC               NCLK(I),I=1,..,NCLREQ: NUMBER OF PARAMETERS  I*4
CC                        FOR REQUEST I
CC               IBIAS(I),I=1,2,..,NCLREQ: TYPE OF CLOCK BIAS I*4
CC                        0: STATION SPECIFIC
CC                        1: FREQUENCY SPECIFIC
CC                        2: SATELLITE SPECIFIC
CC                        3: SAT. SPECIFIC FOR NON-GPS
CC                        4: FREQUENCY SPECIFIC WITH POLYNOM
CC               CLKWGT(J,I),I=1,..,NCLREQ,J=1,2: A PRIORI    R*8
CC                        WEIGHTS FOR CLOCK PARAMETERS
CC               CLFRTO(2,I),I=1,..,NCLREQ: VALIDITY INTERVAL R*8
CC                        OF REQUEST I
CC               CLKHED : CLOCK HEADER INFORMATION            T_CLKHEAD
CC                          %NUMREF=0: FIX REF-CLOCKS
CC                          %NUMREF=2: SUM FOR REF-CLOCKS
CC               NCLKST : NUMBER OF EPOCH WISE STATION CLOCKS I*4
CC               NCLKSA : NUMBER OF EPOCH WISE SAT. CLOCKS    I*4
CC               CLKSYS : =1: ONE REC.CLK FOR EACH SAT.SYS    I*4
CC               NOINCLK  WHAT TO DO IF NO INPUT CLOCK:       I*4(3)
CC                        NOINCLK(1): REC FROM CLK RNX
CC                         -1: IGNORE CLOCK RINEX FILE
CC                          0: USE OBS. (REC FROM OBS-FILE)
CC                          1: USE OBS. (INTERPOL. CLK RNX)
CC                          2: SKIP OBS.
CC                        NOINCLK(2): SAT FROM CLK RNX
CC                         -1: IGNORE CLOCK RINEX FILE
CC                          0: TRY ALSO SAT CLK FILE
CC                          1: USE OBS. (INTERPOL. CLK RNX)
CC                          2: SKIP OBS.
CC                          3: USE OBS. (SAT CLK = ZERO)
CC                        NOINCLK(3): SAT FROM SAT CLK FILE
CC                          2: SKIP OBS.
CC                          3: USE OBS. (SAT CLK = ZERO)
CC               SECIPL : MAX INTERVAL FOR CLK INTERPOLATION  R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/18 18:32
CC
CC CHANGES    :  27-MAY-91 : DON'T PRINT TRAILING BLANKS
CC               26-MAR-96 : MR: REMOVE RECEIVER HEIGHT PARAMETERS
CC               05-MAR-03 : CU: REMOVE USE OF SKELETON FILE
CC               28-MAR-03 : RD: REPORT CLOCK INPUT OPTIONS IN SR PRIREC
CC               15-APR-03 : CU: BUG FIXED (FORMAT STATEMENTS)
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               07-MAY-08 : DT: RECEIVER CLOCK OFFSETS AS RANGE BIASES (SLR)
CC               09-MAY-09 : RD: SAT/FRQ-SPECIFIC RECEIVER CLOCK BIASES
CC               09-MAY-09 : RD: SEPERATE RECEIVER CLOCKS FOR GPS/GLONASS
CC               29-MAY-09 : RD: INPUT CLOCKS ALSO FROM INPUT CLK RNX FILE
CC               08-SEP-10 : RD: MERGE SLR-TIME BIAS OPTION
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE d_const,  ONLY: C
      USE D_CLKRNX, ONLY: T_CLKHEAD
      USE s_jmt
      USE s_timst2
      USE f_lengt1
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 ID1    , ID2    , IM1    , IM2    , IOS    , IREF   ,
     1          IREQ   , IY1    , IY2    , NCLKSA , NCLKST , CLKSYS ,
     2          NCLREQ , NRFSAT , NRFSTA , NUMREF , NPAR   ,
     3          IPAR   , MXCLCQ , IFIL   , II
C
      REAL*8    DAY1   , DAY2   , HOUR1  , HOUR2  , SATNUM , SECIPL
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      TYPE(T_CLKHEAD) CLKHED
C
      CHARACTER*132 TITLES(2)
      CHARACTER*40  EPOSTR
      CHARACTER*20  RECTYP(2,*)
      CHARACTER*16  STNAME(*)
      CHARACTER*6   MXNLCQ
      CHARACTER(LEN=33),DIMENSION(6),PARAMETER :: biastr =
     1  (/ 'STATION-SPECIFIC                 ',
     2     'SLR TIME BIAS                    ',
     3     'FREQUENCY-SPECIFIC               ',
     4     'SATELLITE-SPECIFIC               ',
     5     'SATELLITE-SPECIFIC (ONLY NON-GPS)',
     6     'FREQUENCY-SPECIFIC (POLYNOMIAL)  ' /)
C
      REAL*8        CLKWGT(2,*),CLFRTO(2,*),TIMISB(3,*)
C
      INTEGER*4     ISTCLK(*),ISACLK(*),NCLK(*), IBIAS(*), NOINCLK(3)
      INTEGER*4     LOCQ(MXCLCQ,*)
      INTEGER*4     IRUNIT(2,*), NFTOT, NDIFF(*), STFIL(2,*)
C
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
C
C PRINT TITLE LINES
C -----------------
      IF(NCLREQ.NE.0 .OR. NCLKSA.NE.0 .OR. NCLKST.NE.0) THEN
1       FORMAT(A)
        WRITE(LFNPRT,2) TITLES(1)(1:LENGT1(TITLES(1))),
     1                  TITLES(2)(1:LENGT1(TITLES(2)))
2       FORMAT(//,A,/,A,/,' ',131('-'),//)
        WRITE(LFNPRT,"(
     1       ' 7. CLOCK PARAMETERS'
     2    ,/,' -------------------')")
      ENDIF
C
C RECEIVER CLOCK CORRECTION PARAMETERS / Range biases for SLR
C -----------------------------------------------------------
      IF(NCLREQ.NE.0) THEN
C
        WRITE(LFNPRT,"(
     1       ' '
     2    ,/,' RECEIVER CLOCK OFFSET / RANGE BIAS PARAMETERS:'
     3    ,/,' ---------------------------------------------'
     4    ,/,' '
     5    ,/,' PAR  STATION NAME       VALIDITY START      '
     5      ,' VALIDITY END           SIGMA (USEC)'
     6    ,/,1X,131('-')
     7    ,/,1X)")
C
        DO 50 IREQ=1,NCLREQ
          CALL TIMST2(1,2,CLFRTO(1:2,IREQ),EPOSTR)
          IF (IBIAS(IREQ).EQ.0.AND.ISACLK(IREQ).EQ.0) THEN
            WRITE(LFNPRT,3) IREQ,STNAME(ISTCLK(IREQ)),EPOSTR,
     1                      CLKWGT(1,IREQ)*1.D6/C,
     2                      TRIM(BIASTR(IBIAS(IREQ)+1))
3           FORMAT(I4,2X,A16,3X,A,F14.5,4X,A)
          ELSE IF (IBIAS(IREQ).EQ.0) THEN
            WRITE(LFNPRT,3) IREQ,STNAME(ISTCLK(IREQ)),EPOSTR,
     1                      CLKWGT(1,IREQ)*1.D6/C,
     2                      TRIM(BIASTR(IBIAS(IREQ)+2))
          ELSE
            WRITE(LFNPRT,4) IREQ,STNAME(ISTCLK(IREQ)),EPOSTR,
     1                      'COND. OF SUM',TRIM(BIASTR(IBIAS(IREQ)+2))
4           FORMAT(I4,2X,A16,3X,A,4X,A12,4X,A)
          ENDIF
50      CONTINUE
        WRITE(LFNPRT,5)
5       FORMAT(' ')
      ENDIF
C
C EPOCH WISE CLOCK ESTIMATION
C ---------------------------
      IF (NCLKST.NE.0 .OR. NCLKSA.NE.0) THEN
C
C GET THE NUMBER OF STATIONS WITHIN THE LIST OF REFERNECE CLOCKS
        NRFSTA=CLKHED%REF(1)%NREF
        NRFSAT=0
        NUMREF=CLKHED%REF(1)%NREF
        DO IREF=NUMREF,1,-1
          IF (LEN_TRIM(CLKHED%REF(1)%CLK(IREF)%NAME).NE.3) EXIT
          READ(CLKHED%REF(1)%CLK(IREF)%NAME(2:3),*,IOSTAT=IOS) SATNUM
          IF (IOS.NE.0) EXIT
          NRFSTA=NRFSTA-1
          NRFSAT=NRFSAT+1
        ENDDO
C
C WRITE THE TITLE
        WRITE(LFNPRT,"(
     1       ' '
     2    ,/,' EPOCH-WISE CLOCK ESTIMATION:'
     3    ,/,' ---------------------------'
     4    ,/,' ')")
C
C REPORT REFERENCE CLOCKS (STATIONS ONLY)
        IF (NRFSTA.EQ.NUMREF) THEN
          WRITE(LFNPRT,1001)
     1          CLKHED%REF(1)%CLK(1:NUMREF)%NAME(1:16)
1001      FORMAT(' REFERENCE CLOCKS:',2X,5(A,7X),/,
     1           40(' ',19X,5(A,7X),/))
          IF(MOD(NRFSTA,5).NE.0) WRITE(LFNPRT,'( )')
C
C REPORT REFERENCE CLOCKS (SATELLITES ONLY)
        ELSE IF (NRFSAT.EQ.NUMREF) THEN
          WRITE(LFNPRT,1002) ' REFERENCE CLOCKS:',
     1          CLKHED%REF(1)%CLK(1:NUMREF)%NAME(1:3)
1002      FORMAT(A,2X,16(A,4X),/,
     1           40(' ',19X,16(A,4X),/))
          IF(MOD(NRFSAT,16).NE.0) WRITE(LFNPRT,'( )')
C
C REPORT REFERENCE CLOCKS (STATIONS AND SATELLITES)
        ELSE
          WRITE(LFNPRT,1001)
     1          CLKHED%REF(1)%CLK(1:NRFSTA)%NAME(1:16)
          WRITE(LFNPRT,1002) '                  ',
     1          CLKHED%REF(1)%CLK(NRFSTA+1:NUMREF)%NAME(1:3)
          IF(MOD(NRFSAT,16).NE.0) WRITE(LFNPRT,'( )')
        ENDIF
C
C REFERENCE CLOCKS ARE FIXED OR CONDITION OF SUM
        IF (CLKHED%NUMREF.EQ.1) THEN
          WRITE(LFNPRT,'(20X,A,//)')
     1    'THE REFERENCE CLOCKS ARE FIXED ON THEIR APRIORI VALUES'
        ELSE IF (CLKHED%NUMREF.EQ.2) THEN
          WRITE(LFNPRT,'(20X,A,//)')
     1    'A CONDITION OF SUM IS SET FOR ALL REFERENCE CLOCK ESTIMATES'
        ENDIF
C
C NUMBER OF CLOCKS TO BE ESTIMATED
        WRITE(LFNPRT,'(1X,A,2(I6,A),/)')
     1    'NUMBER OF CLOCKS:',NCLKST,' STATIONS AND ',
     2    NCLKSA,' SATELLITES HAVE TO BE ESTIMATED PER EPOCH'
C
C RECEIVER CLOCKS FOR EACH SATELLITE SYSTEM
        IF (CLKSYS.EQ.1) THEN
          WRITE(LFNPRT,'(1X,A,/)') 'INDIVIDUAL ' //
     1    'RECEIVER CLOCKS ARE ESTIMATED FOR EACH SATELLITE SYSTEM'
        ENDIF
C
      ENDIF
C
C APRIORI RECEIVER CLOCKS:
C -----------------------
      IF(NCLREQ.NE.0 .OR. NCLKSA.NE.0 .OR. NCLKST.NE.0) THEN
        WRITE(lfnprt,'(1X,A)') 'APRIORI RECEIVER CLOCK:'
C
        IF (NOINCLK(1).EQ.-1) THEN
          WRITE(LFNPRT,'(5X,A)')
     1    '- ARE TAKEN FROM THE OBSERVATION FILES'
        ELSE
          WRITE(LFNPRT,'(5X,A,/,5X,A)')
     1    '- ARE PRIMARY TAKEN FROM THE INPUT CLOCK RINEX FILE',
     2    '- IF THERE IS NO CLOCK VALUE AVAILABLE FOR A STATION/EPOCH:'
        ENDIF
C
        IF (NOINCLK(1).EQ.0) THEN
          WRITE(LFNPRT,'(10X,A)')
     1    'THE CLOCK VALUES FROM OBSERVATION FILES ARE ALTERNATIVELY' //
     2    ' USED'
        ELSE IF(NOINCLK(1).EQ.1) THEN
          WRITE(LFNPRT,'(10X,A,/,5X,A,/,10X,A)')
     1    'THE VALUES IN THE CLOCK RINEX FILE ARE ALTERNATIVELY INTE' //
     2    'RPOLATED.',
     3    '- IF A STATION IS COMPLETELY MISSING IN THE INPUT CLOCK R' //
     4    'INEX FILE:',
     5    'IT IS NOT PROCESSED.'
        ELSE IF(NOINCLK(1).EQ.2) THEN
          WRITE(LFNPRT,'(10X,A)')
     1    'THE CORRESPONDING OBSERVATIONS ARE NOT USED FOR PROCESSING.'
        ENDIF
C
C APRIORI SATELLITE CLOCKS (CLOCK RINEX):
C --------------------------------------
        WRITE(lfnprt,'(/,1X,A)') 'APRIORI SALLITE CLOCK:'
C
        IF (NOINCLK(2).EQ.-1) THEN
          WRITE(LFNPRT,'(5X,A)')
     1    '- ARE TAKEN FROM THE GNSS SATELLITE CLOCK FILE'
        ELSE
          WRITE(LFNPRT,'(5X,A,/,5X,A)')
     1    '- ARE PRIMARY TAKEN FROM THE INPUT CLOCK RINEX FILE',
     2    '- IF THERE IS NO CLOCK VALUE AVAILABLE FOR A SATELLITE/EP' //
     3    'OCH:'
        ENDIF
C
        IF (NOINCLK(2).EQ.0) THEN
          WRITE(LFNPRT,'(10X,A)')
     1    'THE CLOCK VALUES FROM GNSS SATELLITE CLOCK FILE ARE ALTER' //
     2    'NATIVELY USED'
        ELSE IF(NOINCLK(2).EQ.1) THEN
          WRITE(LFNPRT,'(10X,A)')
     1    'THE VALUES IN THE CLOCK RINEX FILE ARE ALTERNATIVELY INTE' //
     2    'RPOLATED.'
        ELSE IF(NOINCLK(2).EQ.2) THEN
          WRITE(LFNPRT,'(10X,A)')
     1    'THE CORRESPONDING OBSERVATIONS ARE NOT USED FOR PROCESSING.'
        ELSE IF(NOINCLK(2).EQ.3) THEN
          WRITE(LFNPRT,'(10X,A)')
     1    'THE SATELLITE CLOCK VALUE IS INTRODUCED WITH ZERO.'
        ENDIF
C
C APRIORI SATELLITE CLOCKS (BERNESE SAT. CLOCK FILE):
C --------------------------------------------------
        IF (NOINCLK(2).EQ.-1.OR. NOINCLK(2).EQ.0) THEN
C
          WRITE(LFNPRT,'(5X,A)')
     1    '- IF THE GNSS SATELLITE CLOCK FILE CONTAINS NO CLOCK VALU' //
     2    'E FOR AN EPOCH/SATELLITE:'
C
          IF(NOINCLK(3).EQ.2) THEN
            WRITE(LFNPRT,'(10X,A)')
     1      'THE CORRESPONDING OBSERVATIONS ARE NOT USED FOR PROCESS' //
     2    'ING.'
          ELSE
            WRITE(LFNPRT,'(10X,A)')
     1      'THE SATELLITE CLOCK VALUE IS INTRODUCED WITH ZERO.'
          ENDIF
        ENDIF
C
C ADD THE INTERPLOATION INTERVAL:
C -------------------------------
        IF (SECIPL.GT.0D0) THEN
          WRITE(LFNPRT,'(5X,A,F5.1,A)')
     1    'INTERPOLATION INTERVAL FOR A PRIORI RECEIVER/SATELLITE CL' //
     2    'OCKS: ',SECIPL,' SECONDS'
        ENDIF
C
C TIME-DEPENDENT INTER-SYSTEM BIASES
C ----------------------------------
        DO IPAR=1,NPAR
          IF (LOCQ(1,IPAR).EQ.2.AND.LOCQ(6,IPAR).EQ.5) THEN
            IF (LOCQ(4,IPAR).EQ.1) THEN
              WRITE(LFNPRT,'(//,A,/,A,//,A,/,1X,131("-"),/)')
     1        ' TIME-DEPENDENT INTER-SYSTEM BIASES:',
     2        ' ----------------------------------',
     3        ' PARAM   NUM     STATION NAME         EPOCH        ' //
     4        '         RECEIVER TYPE          NUMBER'
            ENDIF
            CALL timst2(1,1,timisb(1,locq(4,ipar)),EPOSTR)
            DO IFIL=1,NFTOT
              DO II=1,1+NDIFF(IFIL)
                IF (STFIL(II,IFIL).EQ.LOCQ(2,IPAR)) THEN
                  WRITE(LFNPRT,'(I6,I6,5X,A16,5X,A,3X,A,I9)')
     1                locq(4,ipar),LOCQ(3,IPAR),STNAME(LOCQ(2,IPAR)),
     2                EPOSTR(1:19),RECTYP(II,IFIL),IRUNIT(II,IFIL)
                ENDIF
              ENDDO
            ENDDO
          ENDIF
        ENDDO
      ENDIF
      RETURN
      END SUBROUTINE

      END MODULE
