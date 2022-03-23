      MODULE s_R2WTOH
      CONTAINS

C*
      SUBROUTINE R2WTOH(LFNOBS,LFNERR,MAXSAT,
     1                  PRGNAM,RUNBY,CRDATE,CRTIME,NCOM,COMENT,
     2                  STANAM,STANUM,OPRNAM,AGENCY,
     3                  IRUNIT,RECTYP,RCVERS,
     4                  IANTEN,ANTTYP,POSXYZ,POSECC,
     5                  IWLFAC,IWLSAT,NWLSAT,
     6                  NUMTYP,OBSTYP,IDELTT,TFIRST,TLAST,
     7                  NSATEL,NUMSAT,NUMOBS,IRXVRS,IRCODE)
CC
CC NAME       :  R2WTOH
CC
CC PURPOSE    :  WRITE THE ENTIRE HEADER INFORMATION TO A
CC               RINEX OBSERVATION FILE
CC
CC PARAMETERS :
CC         IN :  LFNOBS : LOGICAL FILE NUMBER OF RINEX FILE    I*4
CC               LFNERR : LFN FOR ERROR MESSAGES               I*4
CC               MAXSAT : MAXIMUM NUMBER OF SATELLITES         I*4
CC                        CORRESPONDS TO ROW DECLARATION OF
CC                        ARRAY NUMOBS
CC               PRGNAM : PROGRAM NAME                        CH*20
CC               RUNBY  : NAME OF AGENCY CREATING RINEX FILE  CH*20
CC               CRDATE : CREATION DATE                       CH*9
CC               CRTIME : CREATION TIME                       CH*5
CC               NCOM   : NUMBER OF COMMENT LINES              I*4
CC               COMENT : COMENT LINES                        CH*60(*)
CC               STANAM : STATION NAME                        CH*60
CC               STANUM : STATION NUMBER                      CH*20
CC               OPRNAM : OPERATOR NAME                       CH*20
CC               AGENCY : OPERATOR AGENCY                     CH*40
CC               IRUNIT : RECEIVER UNIT NUMBER                 I*4
CC               RECTYP : RECEIVER TYPE                       CH*(*)
CC               RCVERS : RECEIVER VERSION                    CH*20
CC               IANTEN : ANTENNA NUMBER                       I*4
CC               ANTTYP : ANTENNA  TYPE                       CH*(*)
CC               POSXYZ : APPROXIMATE MARKER POSITION (WGS 84) R*8(3)
CC               POSECC : POSITIONING ECCENTRICITIES IN LOCAL  R*8(3)
CC                          SYSTEM IN METERS
CC                          (ANTENNA BOTTOM PLANE - MARKER)
CC                          POSECC(1): ECC. IN LATITUDE
CC                          POSECC(2): ECC. IN LONGITUDE
CC                          POSECC(3): ECC. IN HEIGHT
CC               IWLFAC : DEFAULT WAVELENGTH FACTORS (L1/L2)   I*4(2)
CC                          1: CYCLE AMBIGUITIES
CC                          2: HALF-CYCLE AMBIGUITIES
CC               IWLSAT : SAT.DEPENDENT WL FACTORS (L1/L2)     I*4(3,*)
CC                          1: CYCLE AMBIGUITIES
CC                          2: HALF-CYCLE AMBIGUITIES
CC                          I=1: WLFAC(L1), I=2: WLFAC(L2), I=3: SVN
CC               NWLSAT : NUMBER OF WL-FACTORS/SVN IN LIST     I*4
CC               NUMTYP : NUMBER OF DIFFERENT OBS.TYPES        I*4
CC               OBSTYP : LIST OF OBSERVATION TYPES           CH*2(*)
CC               IDELTT : OBSERVATION INTERVAL (SEC)           I*4
CC               TFIRST : FIRST OBSERVATION EPOCH              R*8
CC               TLAST  : LAST  OBSERVATION EPOCH              R*8
CC               NSATEL : NUMBER OF SATELLITES                 I*4
CC               NUMSAT : LIST OF SATELLITE NUMBERS            I*4(*)
CC               NUMOBS : NUMBER OF OBSERVATIONS USED          I*4(*,*)
CC                          NUMOBS(I,J): SATELLITE I, OBSTYP J
CC               IRXVRS : RINEX VERSION NUMBER                 I*4
CC                        >  0: GPS FILE
CC                        >100: GLONASS FILE
CC                        >200: GALILEO FILE
CC                        >300: SBAS GEO FILE
CC                        >400: COMPASS/BEIDOU FILE
CC                        >500: MIXED FILE / QZSS FILE
CC                        ADD_GNSS_HERE   CONFLICT!
CC         OUT : IRCODE : RETURN CODE                          I*4
CC                        0: OK
CC                        2: NOT ANTICIPATED VERSION NUMBER
CC                        3: TOO MANY SATELLITES: INCREASE MXLSAT
CC
CC REMARKS    :  - IF DECLARED LENGTH OF RECTYP AND ANTTYP IS 40
CC                 (--> FUNCTION LEN) AND IRUNIT AND IANTEN ARE BOTH
CC                 ZERO THEN RECTYP(21:40) AND ANTTYP(21:40) ARE
CC                 EXPECTED TO CONTAIN ALPHANUMERIC SERIAL NUMBERS
CC               - IF TFIRST (AND TLAST) ARE NEGATIVE, THE TIME SYSTEM IS
CC                 "GLO" (AND NOT "GPS"). (MIXED GPS/GLONASS FILES)
CC
CC AUTHOR     :  W. GURTNER
CC
CC CREATED    :  21-SEP-92
CC
CC CHANGES    :  24-SEP-92 : ??: IRXVRS=1: WRITE NSATEL ONLY IF NOT ZERO
CC               29-AUG-93 : ??: INCLUDE GLONASS FILE
CC               16-AUG-94 : ??: ALPHANUMERIC SERIAL NUMBERS
CC               03-OCT-94 : ??: TFIRST<0: "GLO" TIME SYSTEM
CC               24-OCT-98 : ??: INCLUDE SATELLITE SYSTEM IN SATELLITE NUMBERS
CC               14-JUL-99 : TS: CORRECTED HEADER RECORD 15 (WAS 14!)
CC               30-JAN-03 : ??: INCLUDE GEO
CC               10-MAY-04 : PS: RECORD 11 BUG FIXED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               10-AUG-06 : WG: ALLOW MORE THAN 9 OBSTYPES
CC               28-MAY-07 : AG: E INSTEAD OF T
CC               24-MAY-11 : LP: ENABLE WRITING OF RINEX2 HEADER WITH INPUT
CC                               FROM RINEX3 HEADER
CC               29-FEB-12 : RD: USE R2WTOH AS MODULE
CC               05-JUN-12 : LP: NEW SATELLITE SYSTEMS ADDED;USE g_svnsys
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1992     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_global, ONLY: g_svnsys
      USE s_jmt
      USE s_radgms
      USE s_sjustl
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IANTEN, IDAY  , IDELTT, IFRST , IHOUR , IRCODE,
     1          IRUNIT, IRXVRS, ITIMSY, ITYP  , IYEAR , JSAT  , K     ,
     2          LFNERR, LFNOBS, MAXSAT, MINUTE, MONTH , MXLSAT, MXVERS,
     3          NCOM  , NSATEL, NT    , NUMTYP, NWLSAT, K1    , K2
C
      REAL*8    DAY   , SEC   , TFIRST, TLAST
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C GLOBAL DECLARATIONS
C -------------------
      CHARACTER    STANAM*60,AGENCY*40
      CHARACTER*20 PRGNAM,RUNBY,RCVERS,OPRNAM,STANUM
      CHARACTER    RECTYP*(*),ANTTYP*(*)
      CHARACTER    COMENT(*)*60,CRDATE*9,CRTIME*5
      CHARACTER    OBSTYP(*)*2,CHRSAT*1
      REAL*8       POSECC(3),POSXYZ(3)
      INTEGER*4    IWLFAC(2),IWLSAT(3,*)
      INTEGER*4    NUMSAT(*),NUMOBS(MAXSAT,*)
C
C  LOCAL DECLARATIONS
C  ------------------
      PARAMETER(MXLSAT=90)
      CHARACTER    RXTYPE*20,STRING*60,HEADRC(20)*20,CHR*1
      CHARACTER    FILTYP(7)*20,TIMSYS(3)*3,CWLSAT(MXLSAT)*1
      INTEGER*4    JWLSAT(MXLSAT),KWLSAT(MXLSAT),KWLFAC(2)
C
      DATA ITYP/3/,TIMSYS/'GPS','GLO',' '/
      DATA HEADRC/'RINEX VERSION / TYPE',
     2            'PGM / RUN BY / DATE ',
     3            'COMMENT             ',
     4            'MARKER NAME         ',
     5            'MARKER NUMBER       ',
     6            'OBSERVER / AGENCY   ',
     7            'REC # / TYPE / VERS ',
     8            'ANT # / TYPE        ',
     9            'APPROX POSITION XYZ ',
     *            'ANTENNA: DELTA H/E/N',
     1            'WAVELENGTH FACT L1/2',
     2            '# / TYPES OF OBSERV ',
     3            'INTERVAL            ',
     4            'TIME OF FIRST OBS   ',
     5            'TIME OF LAST OBS    ',
     6            '# OF SATELLITES     ',
     7            'PRN / # OF OBS      ',
     8            '                    ',
     9            '                    ',
     *            'END OF HEADER       '/
      DATA FILTYP/'G (GPS)             ',
     1            'R (GLONASS)         ',
     2            'E (GALILEO)         ',
     3            'S (GEO)             ',
     4            'C (COMPASS)         ',
     5            'M (MIXED)           ',
     6            '                    '/
c     ADD_GNSS_HERE  CONFLICT 'M' and 'J' FLAG in IRXVRS!
C
C RINEX FILE TYPE
      DATA RXTYPE/'OBSERVATION DATA    '/
C
C MAXIMUM RINEX FORMAT VERSION
      DATA MXVERS/2/
C
C     Accept information from RINEX3
      IF(MOD(IRXVRS,100).GT.MXVERS) THEN
        IF (MOD(IRXVRS,100).EQ.3) THEN
           IRXVRS=INT(IRXVRS/100)*100+MXVERS
           write(*,*)'### SR R2WTOH: IRXVRS changed to ',IRXVRS
        ELSE
           GOTO 920
        END IF
      END IF
      IF(MAXSAT.GT.MXLSAT) GOTO 930
C
C RECORD 1        RINEX VERSION / TYPE
      IF(IRXVRS.EQ.1) THEN
        ITYP=7
      ELSE
        ITYP=IRXVRS/100+1
      END IF
      WRITE(LFNOBS,1) MOD(IRXVRS,100),RXTYPE,FILTYP(ITYP),HEADRC(1)
1     FORMAT(I6,14X,A20,A20,A20)
C
C RECORD 2        PGM / RUN BY / DATE
      WRITE(LFNOBS,2) PRGNAM,RUNBY,CRDATE,CRTIME,HEADRC(2)
2     FORMAT(A20,A20,A9,1X,A5,5X,A20)
C
C RECORD 3        COMMENT
      IF(NCOM.GT.1.OR.COMENT(1).NE.' ') THEN
        DO 300 I=1,NCOM
          WRITE(LFNOBS,3) COMENT(I),HEADRC(3)
3         FORMAT(A60,A20)
300     CONTINUE
      END IF
C
C RECORD 4        MARKER NAME
      WRITE(LFNOBS,4) STANAM,HEADRC(4)
4     FORMAT(A60,A20)
C
C RECORD 5        MARKER NUMBER
      IF(IRXVRS.NE.1.AND.STANUM.NE.' ') THEN
        STRING=STANUM
        CALL SJUSTL(STRING)
        WRITE(LFNOBS,5) STRING(1:20),HEADRC(5)
5       FORMAT(A20,40X,A20)
      END IF
C
C RECORD 6        OBSERVER / AGENCY
      STRING( 1:20)=OPRNAM
      CALL SJUSTL(STRING( 1:20))
      STRING(21:60)=AGENCY
      CALL SJUSTL(STRING(21:60))
      WRITE(LFNOBS,6) OPRNAM,AGENCY,HEADRC(6)
6     FORMAT(A20,A40,A20)
C
C RECORD 7        REC # / TYPE / VERS
      IF(IRXVRS.EQ.1) THEN
        WRITE(LFNOBS,7) IRUNIT,RECTYP,RCVERS,HEADRC(7)
7       FORMAT(I6,14X,A20,A20,A20)
      ELSE
        IF(IRUNIT.EQ.0.AND.LEN(RECTYP).EQ.40.AND.
     1     IANTEN.EQ.0.AND.LEN(ANTTYP).EQ.40) THEN
          STRING(1:20)=RECTYP(21:40)
        ELSE
          WRITE(STRING(1:20),72) IRUNIT
72        FORMAT(I6)
        END IF
        CALL SJUSTL(STRING( 1:20))
        WRITE(LFNOBS,71) STRING(1:20),RECTYP(1:20),RCVERS,HEADRC(7)
71      FORMAT(A20,A20,A20,A20)
      END IF
C
C RECORD 8        ANT # / TYPE
      IF(IRXVRS.EQ.1) THEN
        WRITE(LFNOBS,8) IANTEN,ANTTYP,HEADRC(8)
8       FORMAT(I6,14X,A20,20X,A20)
      ELSE
        IF(IRUNIT.EQ.0.AND.LEN(RECTYP).EQ.40.AND.
     1     IANTEN.EQ.0.AND.LEN(ANTTYP).EQ.40) THEN
          STRING(1:20)=ANTTYP(21:40)
        ELSE
          WRITE(STRING(1:20),72) IANTEN
        END IF
        CALL SJUSTL(STRING( 1:20))
        WRITE(LFNOBS,81) STRING(1:20),ANTTYP(1:20),HEADRC(8)
81      FORMAT(A20,A20,20X,A20)
      END IF
C
C RECORD 9        APPROX POSITION XYZ
      WRITE(LFNOBS,9) (POSXYZ(K),K=1,3),HEADRC(9)
9     FORMAT(3F14.4,18X,A20)
C
C RECORD 10       ANTENNA: DELTA H/E/N
      WRITE(LFNOBS,10) POSECC(3),POSECC(2),POSECC(1),HEADRC(10)
10    FORMAT(3F14.4,18X,A20)
C
C RECORD 11       WAVELENGTH FACT L1/2
      IF(IWLFAC(1).NE.0.OR.IWLFAC(2).NE.0) THEN
        WRITE(LFNOBS,11) IWLFAC(1),IWLFAC(2),HEADRC(11)
11      FORMAT(2I6,48X,A20)
      END IF
C
      IF(NWLSAT.NE.0) THEN
        DO 110 I=1,NWLSAT
          JWLSAT(I)=0
110     CONTINUE
C
140     JSAT=0
        IFRST=1
        DO 120 I=1,NWLSAT
          IF(JWLSAT(I).EQ.1) GOTO 120
          IF(IFRST.EQ.1) THEN
            KWLFAC(1)=IWLSAT(1,I)
            KWLFAC(2)=IWLSAT(2,I)
            IFRST=0
          END IF
          IF(IWLSAT(1,I).EQ.KWLFAC(1).AND.IWLSAT(2,I).EQ.KWLFAC(2)) THEN
            JSAT=JSAT+1
            KWLSAT(JSAT)=IWLSAT(3,I)
            JWLSAT(I)=1
          END IF
          IF(JSAT.EQ.7) GOTO 130
120     CONTINUE
C
130     DO 125 I=1,JSAT
          CWLSAT(I) = ' '
c          IF(KWLSAT(I)/100.EQ.1) CWLSAT(I)='R'
c          IF(KWLSAT(I)/100.EQ.2) CWLSAT(I)='E'
c          IF(KWLSAT(I)/100.EQ.3) CWLSAT(I)='S'
c          IF(KWLSAT(I)/100.EQ.4) CWLSAT(I)='C'
c          IF(KWLSAT(I)/100.EQ.5) CWLSAT(I)='J'
c          IF(KWLSAT(I)/100.EQ.0.AND.
c     1       FILTYP(ITYP)(1:1).EQ.'M') CWLSAT(I)='G'
C       ADD_GNSS_HERE
          CWLSAT(I) = g_svnsys(INT(KWLSAT(I)/100))
125     CONTINUE
C
        IF(JSAT.NE.0) THEN
          WRITE(STRING,111)KWLFAC(1),KWLFAC(2),
     1                    JSAT,(CWLSAT(I),MOD(KWLSAT(I),100),I=1,JSAT)
111       FORMAT(2I6,I6,7(3X,A1,I2.2))
          WRITE(LFNOBS,112) STRING(1:60),HEADRC(11)
112       FORMAT(A60,A20)
          GOTO 140
        END IF
      END IF
C
C RECORD 12       # / TYPES OF OBSERV
      WRITE(STRING,121) NUMTYP
121   FORMAT(I6)
      K1=1
122   K2=K1+8
      WRITE(STRING(7:),123) (OBSTYP(K),K=K1,MIN(K2,NUMTYP))
123   FORMAT(9(4X,A2))
      WRITE(LFNOBS,124) STRING,HEADRC(12)
124   FORMAT(A60,A20)
      STRING(1:6)=' '
      K1=K1+9
      IF(K2.LT.NUMTYP) GOTO 122
C
C RECORD 13       INTERVAL
      IF(IRXVRS.EQ.1.OR.IDELTT.NE.0) THEN
        WRITE(LFNOBS,13) IDELTT,HEADRC(13)
13      FORMAT(I6,54X,A20)
      END IF
C
C RECORD 14       TIME OF FIRST OBS
C
C TIME SYSTEM
C
C UTC: SHOW "GLO"
      IF(TFIRST.LT.0.D0) THEN
        ITIMSY=2
        TFIRST=DABS(TFIRST)
        TLAST =DABS(TLAST)
      ELSE
C GPS: SHOW BLANK FOR COMMON GPS FILES
        IF(IRXVRS/100.EQ.0) THEN
          ITIMSY=3
        ELSE
C GPS: SHOW "GPS" FOR MIXED OR GLONASS FILES
          ITIMSY=1
        END IF
      END IF
      CALL JMT(TFIRST,IYEAR,MONTH,DAY)
      ITYP=3
      CALL RADGMS(ITYP,DAY,CHR,IHOUR,MINUTE,SEC)
      SEC=DNINT(SEC*1.D7)/1.D7
      IDAY=IDINT(DAY)
      WRITE(LFNOBS,14) IYEAR,MONTH,IDAY,IHOUR,MINUTE,SEC,
     1                 TIMSYS(ITIMSY),HEADRC(14)
14    FORMAT(5I6,F12.6,6X,A3,9X,A20)
C
C RECORD 15       TIME OF LAST OBS
      IF(TLAST.NE.0.D0) THEN
        CALL JMT(TLAST ,IYEAR,MONTH,DAY)
        CALL RADGMS(ITYP,DAY,CHR,IHOUR,MINUTE,SEC)
        SEC=DNINT(SEC*1.D7)/1.D7
        IDAY=IDINT(DAY)
        WRITE(LFNOBS,15) IYEAR,MONTH,IDAY,IHOUR,MINUTE,SEC,
     1                 TIMSYS(ITIMSY),HEADRC(15)
15      FORMAT(5I6,F12.6,6X,A3,9X,A20)
      END IF
C
C RECORD 16       # OF SATELLITES
      IF((IRXVRS.EQ.1.AND.TLAST.NE.0.D0.AND.NSATEL.NE.0).OR.
     1   (IRXVRS.NE.1.AND.NSATEL.NE.0)) THEN
          WRITE(LFNOBS,16) NSATEL,HEADRC(16)
16        FORMAT(I6,54X,A20)
C
C RECORD 17       PRN / # OF OBS
C
C
          DO 160 I=1,NSATEL
            NT=NUMTYP
            IF(NUMTYP.GT.9) NT=9
            CHRSAT=' '
c            IF(NUMSAT(I)/100.EQ.1) CHRSAT='R'
c            IF(NUMSAT(I)/100.EQ.2) CHRSAT='E'
c            IF(NUMSAT(I)/100.EQ.3) CHRSAT='S'
c            IF(NUMSAT(I)/100.EQ.4) CHRSAT='C'
c            IF(NUMSAT(I)/100.EQ.5) CHRSAT='J'
c            IF(NUMSAT(I)/100.EQ.0.AND.
c     1         FILTYP(ITYP)(1:1).EQ.'M') CHRSAT='G'
C           ADD_GNSS_HERE
            CHRSAT = g_svnsys(INT(NUMSAT(I)/100))
            WRITE(STRING,171) CHRSAT,MOD(NUMSAT(I),100),
     1                        (NUMOBS(I,K),K=1,NT)
171         FORMAT(3X,A1,I2.2,9I6)
            WRITE(LFNOBS,172) STRING,HEADRC(17)
172         FORMAT(A60,A20)
            IF(NUMTYP.GT.9) THEN
              WRITE(STRING,171) CHRSAT,MOD(NUMSAT(I),100),
     1                          (NUMOBS(I,K),K=10,NUMTYP)
              WRITE(LFNOBS,172) STRING,HEADRC(17)
            END IF
160       CONTINUE
      END IF
C
C  BLANK LINE OR END OF HEADER
      IF(IRXVRS.EQ.1) THEN
        WRITE(LFNOBS,*)
      ELSE
        STRING=' '
        WRITE(LFNOBS,20) STRING,HEADRC(20)
20      FORMAT(A60,A20)
      END IF
C
      IRCODE=0
      GOTO 999
C
C  NOT ANTICIPATED VERSION NUMBER
920   IRCODE=2
      WRITE(LFNERR,921) IRXVRS
921   FORMAT(' SR R2WTOH: VERSION NUMBER NOT YET HANDLED:',I5)
      GOTO 999
C
C  TOO MANY SATELLITES
930   IRCODE=3
      WRITE(LFNERR,931) MAXSAT
931   FORMAT(' SR R2WTOH: TOO MANY SATELLITES:',I5)
      GOTO 999
C
999   RETURN
      END SUBROUTINE

      END MODULE
