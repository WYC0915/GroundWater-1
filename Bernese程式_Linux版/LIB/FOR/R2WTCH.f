      MODULE s_R2WTCH
      CONTAINS

C*
      SUBROUTINE R2WTCH(LFNCLK,LFNERR,PRGNAM,RUNBY ,CRDATE,LEAPSC,
     1    NCOM  ,COMENT,NUMTYP,DATTYP,AC    ,ACNAME,TSTART,FROMTO,
     2    NREF  ,REFNAM,REFDOM,REFSIG,TRF   ,NRSTA ,STANAM,STADOM,
     3    STACRD,NRSAT ,SATNUM,IRCODE)
CC
CC NAME       :  R2WTCH
CC
CC PURPOSE    :  WRITE THE ENTIRE HEADER INFORMATION OF A
CC               RINEX CLOCK FILE
CC
CC PARAMETERS :
CC         IN :  LFNCLK : LOGICAL FILE NUMBER OF RINEX FILE            I*4
CC               LFNERR : LFN FOR ERROR MESSAGES                       I*4
CC               PRGNAM : PROGRAM NAME                                CH*20
CC               RUNBY  : NAME OF AGENCY CREATING RINEX FILE          CH*20
CC               CRDATE : CREATION DATE                               CH*20
CC               LEAPSC : NUMBER OF LEAPSECONDS (0=UNKNOWN)            I*4
CC               NCOM   : NUMBER OF COMMENT LINES                      I*4
CC               COMENT : COMENT LINES                                CH*60(*)
CC               NUMTYP : NUMBER OF DIFFERENT DATA TYPES               I*4
CC               DATTYP : LIST OF DIFFERENT DATA TYPES                CH*2(*)
CC               AC     : ANALYSIS CENTER 3-CHAR ID                   CH*3
CC               ACNAME : ANALYSIS CENTER FULL NAME                   CH*55
CC               TSTART : STARTING DAY OF THE VALUES (MJD)             R*8
CC               FROMTO : TIME WINDOW IN SECONDS SINCE TSTART (sec)    R*8(2)
CC               NREF   : NUMBER OF REFERENCE CLOCKS                   I*4
CC               REFNAM : REFERENCE STATION NAME                      CH*4(*)
CC               REFDOM : REFERENCE STATION IDENTIFIER                CH*20(*)
CC               REFSIG : REFERENCE STATION CLOCK CONSTRAINT           R*8(*)
CC               TRF    : TERRESTRIAL REFERENCE FRAME                 CH*50
CC               NRSTA  : NUMBER OF CLOCK STATIONS                     I*4
CC               STANAM : STATION NAME                                CH*4(*)
CC               STADOM : STATION IDENTIFIER (DOMES)                  CH*20(*)
CC               STACRD : STATION COORDINATES (M) cm-accurate          R*8(3,*)
CC               NRSAT  : NUMBER OF SATELLITES                         I*4
CC               SATNUM : SATELLITE PRN NUMBER                         I*4(*)
CC        OUT :  IRCODE : RETURN CODE                                  I*4
CC                        0: OK
CC                        (no further codes supported yet)
CC
CC REMARKS    :  SOME CLOCK RINEX FEATURES ARE NOT YET SUPPORTED.
CC                 CLKNAM : CLOCK NAME (FOR CR AND DR DATA)             CH*4
CC                 CLKDOM : STATION IDENTIFIER (DOMES)                  CH*20
CC                 CALNAM : CALIBRATION STATION IDENTIFIER              CH*60
CC
CC AUTHOR     :  T.A. SPRINGER
CC
CC VERSION    :  1.0
CC
CC CREATED    :  06-DEC-99
CC
CC CHANGES    :  06-DEC-99 : TS: CREATION
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-JUN-05 : MM: D0s ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1999     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE s_jmt
      USE s_radgms
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I1    , I1F   , I2    , I2F   , I3    , I3F   ,
     1          IDE   , IDS   , IHE   , IHEAD , IHS   , IME   , IMS   ,
     2          IPRN  , IRCODE, ISAT  , IYYYE , IYYYS , LEAPSC, LFNCLK,
     3          LFNERR, MME   , MMS   , NCOM  , NPRN  , NREF  , NRSAT ,
     4          NRSTA , NUMTYP
C
      REAL*8    DAY   , RXVERS, SECE  , SECS  , TFRAC , TSTART
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*60 COMENT(*)
      CHARACTER*55 ACNAME
      CHARACTER*50 TRF
      CHARACTER*20 PRGNAM,RUNBY,HEADRC(20)
      CHARACTER*20 CRDATE,REFDOM(*),STADOM(*)
      CHARACTER*4  REFNAM(*),STANAM(*)
      CHARACTER*3  AC,CHR3(15)
      CHARACTER*1  RXTYPE,SIGN
      CHARACTER*2  DATTYP(*)
C
      INTEGER*4    SATNUM(*)
C
      REAL*8       STACRD(3,*),FROMTO(2),REFSIG(*)
C
C RINEX FILE TYPE
C ---------------
      DATA RXTYPE/'C'/
C
C RINEX VERSION NUMBER
C --------------------
      DATA RXVERS/2.00d0/
C
C HEADER RECORDS
C --------------
      DATA HEADRC/'RINEX VERSION / TYPE',
     2            'PGM / RUN BY / DATE ',
     3            'COMMENT             ',
     4            'LEAP SECONDS        ',
     5            '# / TYPES OF DATA   ',
     6            'STATION NAME / NUM  ',
     7            'STATION CLK REF     ',
     8            'ANALYSIS CENTER     ',
     9            '# OF CLK REF        ',
     .            'ANALYSIS CLK REF    ',
     1            '# OF SOLN STA / TRF ',
     2            'SOLN STA NAME / NUM ',
     3            '# OF SOLN SATS      ',
     4            'PRN LIST            ',
     5            '                    ',
     6            '                    ',
     7            '                    ',
     8            '                    ',
     9            '                    ',
     .            'END OF HEADER       '/
C
C WRITE FIRST LINE
C ----------------
      IHEAD=1
      WRITE(LFNCLK,1) RXVERS,RXTYPE,HEADRC(IHEAD)
1     FORMAT(F9.2,11X,A1,39X,A20)
C
C PGM / RUN BY / DATE
C -------------------
      IHEAD=2
      WRITE(LFNCLK,2) PRGNAM,RUNBY,CRDATE,HEADRC(IHEAD)
2     FORMAT(A20,A20,A20,A20)
C
C COMMENT LINES
C -------------
      IHEAD=3
      DO I=1,NCOM
        WRITE(LFNCLK,3) COMENT(I),HEADRC(IHEAD)
3       FORMAT(A60,A20)
      ENDDO
C
C LEAP SECONDS
C ------------
      IF (LEAPSC.NE.0) THEN
        IHEAD=4
        WRITE(LFNCLK,4) LEAPSC,HEADRC(IHEAD)
4       FORMAT(I6,54X,A20)
      ENDIF
C
C # / TYPES OF DATA
C -----------------
      IHEAD=5
      WRITE(LFNCLK,5) NUMTYP,(DATTYP(I),I=1,5),HEADRC(IHEAD)
5     FORMAT(I6,5(4X,A2),24X,A20)
C
C STATION NAME / NUM (not supported yet)
C ------------------
C      IHEAD=6
C      WRITE(LFNCLK,6) CLKNAM,CLKDOM,HEADRC(IHEAD)
C6     FORMAT(A4,1X,A20,35X,A20)
C
C STATION CLK REF (not supported yet)
C ---------------
C      IHEAD=7
C      WRITE(LFNCLK,7) CLKREF,DESCR,HEADRC(IHEAD)
C7     FORMAT(A3,2X,A55,A20)
C
C ANALYSIS CENTER
C ---------------
      IHEAD=8
      WRITE(LFNCLK,8) AC,ACNAME,HEADRC(IHEAD)
8     FORMAT(A3,2X,A55,A20)
C
C # OF CLK REF
C ------------
      IF (NREF.GT.0) THEN
        IHEAD=9
        IF (FROMTO(1).EQ.FROMTO(2)) THEN
          WRITE(LFNCLK,91) NREF,HEADRC(IHEAD)
        ELSE
          CALL JMT(TSTART,IYYYS,MMS,DAY)
          IDS=IDNINT(DAY)
          IF (FROMTO(2).LT.86400D0) THEN
            CALL JMT(TSTART,IYYYE,MME,DAY)
            TFRAC=FROMTO(2)
          ELSE
            CALL JMT(TSTART+1D0,IYYYE,MME,DAY)
            TFRAC=FROMTO(2)-86400D0
          ENDIF
          IDE=IDNINT(DAY)
          CALL RADGMS(3,FROMTO(1)/86400D0,SIGN,IHS,IMS,SECS)
          CALL RADGMS(3,TFRAC/86400D0,SIGN,IHE,IME,SECE)
          WRITE(LFNCLK,92) NREF,IYYYS,MMS,IDS,IHS,IMS,SECS,
     1                         IYYYE,MME,IDE,IHE,IME,SECE,HEADRC(IHEAD)
        ENDIF
91      FORMAT(I6,54X,A20)
92      FORMAT(I6,1X,I4,4I3,F10.6,1X,I4,4I3,F10.6,A20)
C
C ANALYSIS CLK REF
C ----------------
        IHEAD=10
        DO I=1,NREF
          WRITE(LFNCLK,10)REFNAM(I),REFDOM(I),REFSIG(I),HEADRC(IHEAD)
10        FORMAT(A4,1X,A20,15X,E19.12,1X,A20)
        ENDDO
      ENDIF
C
C # OF SOLN STA / TRF
C -------------------
      IF (NRSTA.GT.0) THEN
        IHEAD=11
        WRITE(LFNCLK,11) NRSTA,TRF,HEADRC(IHEAD)
11      FORMAT(I6,4X,A50,A20)
C
C SOLN STA NAME / NUM (+ CRD (cm))
C --------------------------------
        IHEAD=12
        DO I=1,NRSTA
          I1=IDINT(STACRD(1,I))
          I2=IDINT(STACRD(2,I))
          I3=IDINT(STACRD(3,I))
          I1F=IDINT(DABS(STACRD(1,I)-I1)*1D3)
          I2F=IDINT(DABS(STACRD(2,I)-I2)*1D3)
          I3F=IDINT(DABS(STACRD(3,I)-I3)*1D3)
          WRITE(LFNCLK,12)STANAM(I),STADOM(I),I1,I1F,I2,I2F,I3,I3F,
     1                    HEADRC(IHEAD)
12        FORMAT(A4,1X,A20,I8,I3.3,I9,I3.3,I9,I3.3,A20)
        ENDDO
      ENDIF
C
C # OF SOLN SATS
C --------------
      IF (NRSAT.GT.0) THEN
        IHEAD=13
        WRITE(LFNCLK,13) NRSAT,HEADRC(IHEAD)
13      FORMAT(I6,54X,A20)
C
C PRN LIST
C --------
        IHEAD=14
        NPRN=IDINT((NRSAT-1)/15.D0)+1
        DO IPRN=1,NPRN
          ISAT=(IPRN-1)*15
          DO I=1,15
            IF (SATNUM(ISAT+I).GT.100) THEN
              WRITE(CHR3(I),'(A1,I2.2)')'R',SATNUM(ISAT+I)-100
            ELSE IF (ISAT+I.LE.NRSAT) THEN
              WRITE(CHR3(I),'(A1,I2.2)')'G',SATNUM(ISAT+I)
            ELSE
              CHR3(I)='   '
            ENDIF
          ENDDO
          WRITE(LFNCLK,14)(CHR3(I),I=1,15),HEADRC(IHEAD)
14        FORMAT(15(A3,1X),A20)
        ENDDO
      ENDIF
C
C END OF HEADER
C -------------
      IHEAD=20
      WRITE(LFNCLK,20) HEADRC(IHEAD)
20    FORMAT(60X,A20)
C
      IRCODE=0
C
999   RETURN
      END SUBROUTINE

      END MODULE
