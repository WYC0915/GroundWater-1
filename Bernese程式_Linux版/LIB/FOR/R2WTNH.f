      MODULE s_R2WTNH
      CONTAINS

C*
      SUBROUTINE R2WTNH(LFNNAV,LFNERR,PRGNAM,RUNBY,
     1                  CRDATE,CRTIME,NCOM,COMENT,
     2                  ALPHA,BETA,A0UTC,A1UTC,ITUTC,NWKUTC,LEAP,
     3                  IRXVRS,IRCODE)
CC
CC NAME       :  R2XWTNH
CC
CC PURPOSE    :  WRITE THE ENTIRE HEADER INFORMATION OF A
CC               RINEX NAVIGATION FILE (VERSION 2)
CC
CC PARAMETERS :
CC         IN :  LFNNAV : LOGICAL FILE NUMBER                  I*4
CC               LFNERR : LFN FOR ERROR MESSAGES               I*4
CC               PRGNAM : PROGRAM NAME                        CH*20
CC               RUNBY  : NAME OF AGENCY CREATING RINEX FILE  CH*20
CC               CRDATE : CREATION DATE                       CH*9
CC               CRTIME : CREATION TIME                       CH*5
CC               NCOM   : NUMBER OF COMMENT LINES              I*4
CC               COMENT : COMMENT LINES                       CH*60(*)
CC               ALPHA  : IONOSPHERE PARAMETERS A0-A3          R*8(4)
CC               BETA   : IONOSPHERE PARAMETERS B0-B3          R*8(4)
CC               A0UTC  : UTC POLYNOMIALS                      R*8
CC               A1UTC  :                                      R*8
CC               ITUTC  : REF.TIME FOR UTC                     I*4
CC               NWKUTC : REF.WEEK FOR UTC                     I*4
CC               LEAP   : LEAP SECONDS                         I*4
CC               IRXVRS : RINEX VERSION NUMBER                 I*4
CC               IRCODE : RETURN CODE (0=OK)                   I*4
CC
CC REMARKS    :  WRITES VERSION 1 OR VERSION 2 HEADERS
CC
CC AUTHOR     :  W. GURTNER
CC
CC VERSION    :  3.4
CC
CC CREATED    :  30-JUN-92 17:27
CC
CC CHANGES    :  20-DEC-94 : ??: DON'T WRITE ONE BLANK COMMENT LINE ONLY
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1989     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IRCODE, IRXVRS, ITUTC , K     , LEAP  , LFNERR,
     1          LFNNAV, NCOM  , NWKUTC
C
      REAL*8    A0UTC , A1UTC
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C GLOBAL DECLARATIONS
C -------------------
      CHARACTER*20 PRGNAM,RUNBY
      CHARACTER    COMENT(*)*60,CRDATE*9,CRTIME*5
      REAL*8       ALPHA(*),BETA(*)
C
C  LOCAL DECLARATIONS
C  ------------------
      CHARACTER    HEADRC(8)*20,RXTYPE*20
C
      DATA HEADRC/'RINEX VERSION / TYPE',
     2            'PGM / RUN BY / DATE ',
     3            'COMMENT             ',
     4            'ION ALPHA           ',
     5            'ION BETA            ',
     6            'DELTA-UTC: A0,A1,T,W',
     7            'LEAP SECONDS        ',
     8            'END OF HEADER       '/
C
C RINEX FILE TYPE
      DATA RXTYPE/'NAVIGATION DATA     '/
C
C RECORD 1        RINEX VERSION / TYPE
      WRITE(LFNNAV,1) IRXVRS,RXTYPE,HEADRC(1)
1     FORMAT(I6,14X,A20,20X,A20)
C
C RECORD 2        PGM / RUN BY / DATE
      WRITE(LFNNAV,2) PRGNAM,RUNBY,CRDATE,CRTIME,HEADRC(2)
2     FORMAT(A20,A20,A9,1X,A5,5X,A20)
C
C RECORD 3        COMMENT
      IF(NCOM.GT.1.OR.COMENT(1).NE.' ') THEN
        DO 300 I=1,NCOM
          WRITE(LFNNAV,3) COMENT(I),HEADRC(3)
3         FORMAT(A60,A20)
300     CONTINUE
      END IF
C
      IF(IRXVRS.GT.1) THEN
C
C RECORD 4        ION ALPHA
        IF(ALPHA(1).NE.0.D0.OR.
     1     ALPHA(2).NE.0.D0.OR.
     2     ALPHA(3).NE.0.D0.OR.
     3     ALPHA(4).NE.0.D0) THEN
          WRITE(LFNNAV,4) (ALPHA(K),K=1,4),HEADRC(4)
4         FORMAT(2X,4D12.4,10X,A20)
        END IF
C RECORD 5        ION BETA
        IF(BETA(1).NE.0.D0.OR.
     1     BETA(2).NE.0.D0.OR.
     2     BETA(3).NE.0.D0.OR.
     3     BETA(4).NE.0.D0) THEN
          WRITE(LFNNAV,5) (BETA(K),K=1,4),HEADRC(5)
5         FORMAT(2X,4D12.4,10X,A20)
        END IF
C RECORD 6        DELTA-UTC: A0,A1,T,W
        IF(A0UTC.NE.0.D0.OR.
     1     A1UTC.NE.0.D0.OR.
     2     ITUTC.NE.0.OR.
     3     NWKUTC.NE.0) THEN
          WRITE(LFNNAV,6) A0UTC,A1UTC,ITUTC,NWKUTC,HEADRC(6)
6         FORMAT(3X,2D19.12,2I9,1X,A20)
        END IF
C RECORD 7        LEAP SECONDS
        IF(LEAP.NE.0) THEN
          WRITE(LFNNAV,7) LEAP,HEADRC(7)
7         FORMAT(I6,54X,A20)
C END OF HEADER LINE
        END IF
        WRITE(LFNNAV,8) HEADRC(8)
8       FORMAT(60X,A20)
C
      ELSE
C
        WRITE(LFNNAV,*)
C
      END IF
C
900   IRCODE=0
      RETURN
      END SUBROUTINE

      END MODULE
