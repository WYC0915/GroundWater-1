      MODULE s_FMTPOL
      CONTAINS

C*
      SUBROUTINE FMTPOL(LFN,IREWIN,IFORM,IVERS,ICOL,NUTNAM,SUBNAM)
CC
CC TITLE      :  GET FORMAT OF INPUT POLE FILE
CC
CC PURPOSE    :  GET THE FORMAT OF AN INPUT POLE FILE AND
CC               GOES TO THE FIRST POLE VALUE.
CC               THE FOLLOWING FORMATS CAN BE DISTINGUISHED:
CC               - BULLETIN B
CC               - BULLETIN A  (RAPID POLE FROM MCCARTHY)
CC               - C04 POLE
CC               - IGS STATIONS:
CC                   - CODE SOLUTION IGS FORMAT
CC                   - CODE SOLUTION OLD FORMAT
CC                   - CODE SOLUTION NEW FORMAT
CC                   - GFZ SOLUTION
CC                   - IGS SOLUTION
CC                   - JPL SOLUTION
CC                   - NGS SOLUTION
CC                   - SIO SOLUTION
CC                   - IGS 'NEW' FORMAT (CODE SOLUTION)
CC                   - IGS FORMAT 'VERSION 2' (CODE SOLUTION)
CC
CC PARAMETERS :
CC         IN :  LFN    : LOGICAL FILE NUMBER                 I*4
CC               IREWIN : 1= REWIND POLE FILE,                I*4
CC                        0= GOTO FIRST OBS EPOCH
CC        OUT :  IFORM  : FORMAT OF INPUT POLE FILE           I*4
CC                         0 = UNKNOWN FORMAT
CC                         1 = BULLETIN B
CC                         2 = BULLETIN A
CC                         3 = C04 (VALID UNTIL OCT. 2007)
CC                         4 = CODE SOLUTION IGS FORMAT
CC                         5 = CODE SOLUTION OLD FORMAT
CC                         6 = CODE SOLUTION NEW FORMAT
CC                         7 = GFZ SOLUTION
CC                         8 = IGS SOLUTION
CC                         9 = JPL SOLUTION
CC                        10 = NGS SOLUTION
CC                        11 = SIO SOLUTION
CC                        12 = CODE SOLUTION 'NEW' IGS FORMAT
CC                        13 = IGS/IERS FORMAT 'VERSION 1'
CC                        14 = GSI JPL FORMAT
CC                        15 = IGS/IERS FORMAT 'VERSION 2' (UT1-UTC, LOD)
CC                        16 = FORMAT 'VERSION 2' (UT1R-UTC, LODR)
CC                        17 = FORMAT 'VERSION 2' (UT1R-TAI, LODR)
CC                        18 = FORMAT 'VERSION 2' (UT1-TAI, LOD)
CC                        19 = SAME AS IFORM=2 BUT OTHER FORMAT
CC                        20 = A NEW C04 FORMAT
CC                IVERS : FORMAT VERSION                       I*4
CC                         1 = OLD IERS FORMAT
CC                         2 = IERS VERSION 2 FORMAT
CC                ICOL  : COLUMN INDICES FOR OPTIONAL COLUNMS  I*4(11)
CC               NUTNAM  : NUTATION MODEL NAME                 CH*16
CC               SUBNAM  : SUBDAILY POLE MODEL NAME            CH*16
CC
CC REMARKS    :  NUTNAM AND SUBNAM ARE BLANK IF NOT AVAILABLE
CC
CC AUTHOR     :  S.FANKHAUSER
CC
CC VERSION    :  3.5  (MAR 94)
CC
CC CREATED    :  18-MAR-94
CC
CC CHANGES    :  20-JUN-94 : SF: 12: CODE SOL. NEW IGS FORMAT
CC               05-FEB-95 : EB: 13: READ ALSO IGS ORBIT FORMAT
CC               16-JUL-98 : DI: 15: CODE SOL. IGS FORMAT 'VERSION 2'
CC               23-JUL-98 : MR: 15: ALL VERSION 2 FORMATS
CC               20-DEC-01 : HU: 16: VERSION 2 FORMAT UT1R-UTC, LODR
CC               20-DEC-01 : HU: 17: VERSION 2 FORMAT UT1R-TAI, LODR
CC               02-OCT-02 : HU: 18: VERSION 2 FORMAT UT1-TAI, LOD
CC                               FUNCTION CONVERTED TO SUBROUTINE AND
CC                               RETURNING IVERS, ICOL
CC               01-NOV-03 : HU: READ AND RETURN NUTATION AND SUBDAILY
CC                               MODELS FOR CODE FILES
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               23-JUN-07 : AG: ANOTHER POLE FORMAT IMPLEMENTED (19)
CC               25-OCT-07 : LO/RD: NEW C04 FORMAT (FORMAT 20)
CC               14-FEB-09 : HB: MODIFIED C04 FORMAT HEADER LINE
CC               23-MAR-11 : PS/DT: '08 C04' ADDED
CC               12-MAR-12 : RD: USE SPLSTR AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1994     UNIVERSITY OF BERN
CC               SWITZERLAND
C*
      USE s_splstr
      USE f_lengt0
      USE s_upperc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I1    , I2    , ILIN  , IRC   , IREWIN, IVERS ,
     1          LFN   , LL    , MAXCOL, NOUT
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(MAXCOL=23)
C
C GLOBAL DECLARATON
C -----------------
      INTEGER*4 IFORM,ICOL(11)
      CHARACTER*16  NUTNAM,SUBNAM
C
C LOCAL DECLARATIONS
C ------------------
      CHARACTER*132 LINE,HLPSTR
      CHARACTER*10  SUBSTR(MAXCOL)
C
C LOGICAL FILE NUMBERS
C --------------------
C
C GET THE FORMAT
C --------------
      NUTNAM=' '
      SUBNAM=' '
      ILIN=0
      IVERS=1
      IFORM=0
      DO 10 I1=1,100000
        READ(LFN,11,END=990) LINE
11      FORMAT(A)
        ILIN=ILIN+1
        LL=LENGT0(LINE)
        IF (LL.EQ.0) GOTO 10
C
C DETECT IGS/IERS FORMAT VERSION 2
        HLPSTR=LINE(1:9)
        CALL UPPERC(HLPSTR)
        IF (HLPSTR.EQ.'VERSION 2') THEN
          IVERS=2
          GOTO 10
        ENDIF
C
        IF (IVERS.EQ.1.AND.
     1      LINE(1:24).EQ.' Final Bulletin B values') THEN
          IFORM=1
          READ(LFN,11)
          GOTO 990
        ENDIF
        IF (IVERS.EQ.1.AND.LL.GE.90 .AND.
     1      LINE(13:13).EQ.'.'.AND.LINE(21:21)  .EQ.'.'.AND.
     2      LINE(30:30).EQ.'.'.AND.LINE(40:40)  .EQ.'.'.AND.
     3      LINE(49:49).EQ.'.'.AND.LINE(61:61)  .EQ.'.'.AND.
     4      LINE(71:71).EQ.'.'.AND.LINE(82:82)  .EQ.'.'.AND.
     5      LINE(89:89).EQ.'.'.AND.LINE(103:103).EQ.'.') THEN
          IFORM=19
          REWIND LFN
          DO 40 I2=1,ILIN-1
            READ(LFN,11)
40        CONTINUE
          GOTO 990
        ENDIF
        IF (IVERS.EQ.1.AND.LL.GE.80 .AND.
     1      LINE(9:9)  .EQ.'.'.AND.LINE(16:16).EQ.'.'.AND.
     2      LINE(24:24).EQ.'.'.AND.LINE(31:31).EQ.'.'.AND.
     3      LINE(39:39).EQ.'.'.AND.LINE(47:47).EQ.'.'.AND.
     4      LINE(56:56).EQ.'.'.AND.LINE(64:64).EQ.'.'.AND.
     5      LINE(72:72).EQ.'.'.AND.LINE(80:80).EQ.'.') THEN
          IFORM=2
          REWIND LFN
          DO 20 I2=1,ILIN-1
            READ(LFN,11)
20        CONTINUE
          GOTO 990
        ENDIF
        IF (IVERS.EQ.1.AND.LL.GT.11.AND.LINE(1:11).EQ.'  YEAR ==> ')THEN
          IFORM=3
          READ(LFN,11)
          GOTO 990
        ENDIF
        IF (IVERS.EQ.1.AND.(INDEX(LINE,'EOP (IERS) C04').NE.0.OR.
     1              INDEX(LINE,'EOP (IERS) 05 C04').NE.0.OR.
     2              INDEX(LINE,'EOP (IERS) 08 C04').NE.0))THEN
          DO I2=1,100000
            READ(LFN,11,END=990) LINE
            IF (LINE(1:13).EQ.'     (0h UTC)') THEN
              IFORM=20
              READ(LFN,11)
              GOTO 990
            ENDIF
          ENDDO
        ENDIF
        IF (IVERS.EQ.1.AND.LL.GT.80.AND.LINE(1:38).EQ.
     1      'TIME        X-POLE   Y-POLE   T=UT1UTC') THEN
          IFORM=4
          READ(LFN,11)
          GOTO 990
        ENDIF
        IF (IVERS.EQ.1.AND.LL.GT.50.AND.LINE(1:50).EQ.
     1      ' MJD    X-POLE   Y-POLE   UT1-UTC   GPS-UTC   DATE') THEN
          IFORM=5
          READ(LFN,11)
          GOTO 990
        ENDIF
        IF (IVERS.EQ.1.AND.LL.GT.50.AND.LINE(1:49).EQ.
     1      'YYYY MM DD HH MM    (")      (")      (S)     (S)') THEN
          IFORM=6
          READ(LFN,11)
          GOTO 990
        ENDIF
        IF (IVERS.EQ.1.AND.LL.GT.46.AND.LINE(1:46).EQ.
     1     ' MJD        XP       YP   UT1R-IAT for 0h SIGX') THEN
          IFORM=7
          READ(LFN,11)
          GOTO 990
        ENDIF
        IF (IVERS.EQ.1.AND.LL.GT.43.AND.LINE(1:43).EQ.
     1     'MJD        Xpole    Ypole  UT1-UTC    X-Err') THEN
          IFORM=8
          READ(LFN,11)
          READ(LFN,11)
          READ(LFN,11)
          GOTO 990
        ENDIF
        IF (IVERS.EQ.1.AND.LL.GT.50.AND.LINE(1:50).EQ.
     1     '  MJD       X        Y       UT1R-UTC XSIG    YSIG') THEN
          IFORM=9
          READ(LFN,11)
          GOTO 990
        ENDIF
        IF (IVERS.EQ.1.AND.LL.GE.71 .AND.
     1      LINE(7:7)  .EQ.'.'.AND.LINE(12:12).EQ.'.'.AND.
     2      LINE(21:21).EQ.'.'.AND.LINE(30:30).EQ.'.'.AND.
     3      LINE(38:38).EQ.'.'.AND.LINE(46:46).EQ.'.'.AND.
     4      LINE(54:54).EQ.'.'.AND.LINE(62:62).NE.'.'.AND.
     5      LINE(66:66).NE.'.'.AND.LINE(71:71).NE.'.') THEN
          IFORM=10
          REWIND LFN
          DO 30 I2=1,ILIN-1
            READ(LFN,11)
30        CONTINUE
          GOTO 990
        ENDIF
        IF (IVERS.EQ.1.AND.LL.GT.44.AND.LINE(1:44).EQ.
     1     '  MJD        X        Y      UT1-UTC    XSIG') THEN
          IFORM=11
          READ(LFN,11)
          GOTO 990
        ENDIF
        IF (IVERS.EQ.1.AND.LL.GT.80.AND.LINE(1:54).EQ.
     1  '  MJD        X-P     Y-P  UT1UTC   LOD  S-X  S-Y S-UT ') THEN
          IFORM=12
          READ(LFN,11)
          GOTO 990
        ENDIF
        IF (IVERS.EQ.1.AND.LINE(1:54).EQ.
     1  '  MJD      Xpole Ypole UT1-UTC   LOD Xsig Ysig  UTsig ') THEN
          IFORM=13
          READ(LFN,11)
          GOTO 990
        ENDIF
C JPL IGS RAPID
        IF (IVERS.EQ.1.AND.LINE(1:54).EQ.
     1  '     MJD  XPole  Ypole UT1R-UTC  LODR Xsig Ysig UTsig '
     2                .AND.LINE(55:108).EQ.
     3  'LODsig Nr Nf Nt  Xrt  Yrt Xrtsig Yrtsig  XYCor XUTCor ') THEN
          IFORM=14
          READ(LFN,11)
          GOTO 990
        ENDIF
C
C DETECT IGS/IERS FORMAT VERSION 2
C --------------------------------
        IF (IVERS.EQ.2) THEN
          CALL UPPERC(LINE)
C
          IF (LINE(1:23).EQ.'NUTATION MODEL       : ') THEN
            NUTNAM=LINE(24:39)
            SUBNAM=LINE(70:85)
          ENDIF
C
          IF (INDEX(LINE,'MJD') > 0) THEN
            CALL SPLSTR(LINE,MAXCOL,' ',NOUT,SUBSTR,IRC)
C
C UT1-UTC AND LOD
            IF ((SUBSTR(4).EQ.'UT1-UTC'.OR.SUBSTR(4).EQ.'UT1UTC').AND.
     1           SUBSTR(5).EQ.'LOD') THEN
               IFORM=15
C UT1R-UTC AND LODR
            ELSEIF ((SUBSTR(4).EQ.'UT1R-UTC'.OR.SUBSTR(4).EQ.'UT1RUTC')
     1               .AND. SUBSTR(5).EQ.'LODR') THEN
               IFORM=16
C UT1R-TAI AND LODR
            ELSEIF ((SUBSTR(4).EQ.'UT1R-TAI'.OR.SUBSTR(4).EQ.'UT1RTAI')
     1               .AND. SUBSTR(5).EQ.'LODR') THEN
               IFORM=17
C UT1-TAI AND LOD
            ELSEIF ((SUBSTR(4).EQ.'UT1-TAI'.AND. SUBSTR(5).EQ.'LOD')
     1             .OR.(SUBSTR(4).EQ.'UT1'.AND. SUBSTR(5).EQ.'-TAI'
     2                .AND.SUBSTR(6).EQ.'LOD')) THEN
               IFORM=18
            ENDIF
            IF (IFORM==0) CYCLE
C OPTIONAL COLUMNS
            ICOL=0
            DO I=13,MAXCOL
              IF (SUBSTR(I).EQ.'XRT' .OR.SUBSTR(I).EQ.'XPRT'.OR.
     1            SUBSTR(I).EQ.'XDOT'.OR.SUBSTR(I).EQ.'X-RT') THEN
                ICOL(1)=I
              ELSEIF (SUBSTR(I).EQ.'YRT' .OR.SUBSTR(I).EQ.'YPRT'.OR.
     1                SUBSTR(I).EQ.'YDOT'.OR.SUBSTR(I).EQ.'Y-RT') THEN
                ICOL(2)=I
              ELSEIF (SUBSTR(I).EQ.'XRTSIG' .OR.SUBSTR(I).EQ.'XRS'.OR.
     1                SUBSTR(I).EQ.'XDOTSIG'.OR.SUBSTR(I).EQ.'S-XR'.OR.
     2                SUBSTR(I).EQ.'XRTSG') THEN
                ICOL(3)=I
              ELSEIF (SUBSTR(I).EQ.'YRTSIG' .OR.SUBSTR(I).EQ.'YRS'.OR.
     1                SUBSTR(I).EQ.'YDOTSIG'.OR.SUBSTR(I).EQ.'S-YR'.OR.
     2                SUBSTR(I).EQ.'YRTSG') THEN
                ICOL(4)=I
              ELSEIF (SUBSTR(I).EQ.'XYCORR' .OR.SUBSTR(I).EQ.'XYCOR'.OR.
     1                SUBSTR(I).EQ.'C-XY'   .OR.SUBSTR(I).EQ.'XYC') THEN
                ICOL(5)=I
              ELSEIF (SUBSTR(I).EQ.'XUTCOR' .OR.SUBSTR(I).EQ.'XTC'.OR.
     1                SUBSTR(I).EQ.'C-XT') THEN
                ICOL(6)=I
              ELSEIF (SUBSTR(I).EQ.'YUTCOR' .OR.SUBSTR(I).EQ.'YTC'.OR.
     1                SUBSTR(I).EQ.'C-YT') THEN
                ICOL(7)=I
              ELSEIF (SUBSTR(I).EQ.'DPSI') THEN
                ICOL(8)=I
              ELSEIF (SUBSTR(I).EQ.'DEPS') THEN
                ICOL(9)=I
              ELSEIF (SUBSTR(I).EQ.'S-DP') THEN
                ICOL(10)=I
              ELSEIF (SUBSTR(I).EQ.'S-DE') THEN
                ICOL(11)=I
              ENDIF
            ENDDO
            READ(LFN,11)
            GOTO 990
          ENDIF
        ENDIF
10    CONTINUE
C
C REWIND THE FILE
C ---------------
990   IF (IREWIN.EQ.1) REWIND LFN
C
C RETURN CODE
C -----------
      RETURN
      END SUBROUTINE

      END MODULE
