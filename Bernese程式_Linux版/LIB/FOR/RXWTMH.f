      MODULE s_RXWTMH
      CONTAINS

C*
      SUBROUTINE RXWTMH(LFNMET,LFNERR,PRGNAM,RUNBY,CRDATE,CRTIME,
     1                  NCOM,COMENT,STANAM,NUMTYP,OBSTYP,IRCODE)
CC
CC NAME       :  RXWTMH
CC
CC PURPOSE    :  WRITE THE ENTIRE HEADER INFORMATION OF A
CC               RINEX METEOROLOGICAL DATA FILE
CC
CC PARAMETERS :
CC         IN :  LFNMET : LOGICAL FILE NUMBER                  I*4
CC               LFNERR : LFN FOR ERROR MESSAGES               I*4
CC               PRGNAM : PROGRAM NAME                        CH*20
CC               RUNBY  : NAME OF AGENCY CREATING RINEX FILE  CH*20
CC               CRDATE : CREATION DATE                       CH*9
CC               CRTIME : CREATION TIME                       CH*5
CC               NCOM   : NUMBER OF COMMENT LINES              I*4
CC               COMENT : COMENT LINES                        CH*60(*)
CC               STANAM : STATION NAMES                       CH*60
CC               NUMTYP : NUMBER OF DIFFERENT OBS.TYPES        I*4
CC               OBSTYP : LIST OF OBSERVATION TYPES           CH*2(*)
CC        OUT :  IRCODE : RETURN CODE (0=OK)                   I*4
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  W. GURTNER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  89/04/05 12:00
CC
CC CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
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
      INTEGER*4 I     , IRCODE, IRVERS, K     , LFNERR, LFNMET, NCOM  ,
     1          NUMTYP
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C GLOBAL DECLARATIONS
C -------------------
      CHARACTER    PRGNAM*20,RUNBY*20,STANAM*60
      CHARACTER    COMENT(*)*60,CRDATE*9,CRTIME*5
      CHARACTER    OBSTYP(*)*2
C
C  LOCAL DECLARATIONS
C  ------------------
      CHARACTER    HEADRC(5)*20,RXTYPE*20,STRING*60
C
      DATA HEADRC/'RINEX VERSION / TYPE',
     2            'PGM / RUN BY / DATE ',
     3            'COMMENT             ',
     4            'MARKER NAME         ',
     5            '# / TYPES OF OBSERV '/
C
C RINEX FILE TYPE
      DATA RXTYPE/'METEOROLOGICAL DATA '/
C
C RINEX FORMAT VERSION
      DATA IRVERS/1/
C
C RECORD 1        RINEX VERSION / TYPE
      WRITE(LFNMET,1) IRVERS,RXTYPE,HEADRC(1)
1     FORMAT(I6,14X,A20,20X,A20)
C
C RECORD 2        PGM / RUN BY / DATE
      WRITE(LFNMET,2) PRGNAM,RUNBY,CRDATE,CRTIME,HEADRC(2)
2     FORMAT(A20,A20,A9,1X,A5,5X,A20)
C
C RECORD 3        COMMENT
      DO 300 I=1,NCOM
        WRITE(LFNMET,3) COMENT(I),HEADRC(3)
3       FORMAT(A60,A20)
300   CONTINUE
C
C RECORD 4        MARKER NAME
      WRITE(LFNMET,4) STANAM,HEADRC(4)
4     FORMAT(A60,A20)
C
C RECORD 5        # / TYPES OF OBSERV
      WRITE(STRING,51) NUMTYP,(OBSTYP(K),K=1,NUMTYP)
51    FORMAT(I6,9(4X,A2))
      WRITE(LFNMET,52) STRING,HEADRC(5)
52    FORMAT(A60,A20)
C
C  WRITE BLANK LINE
      WRITE(LFNMET,*)
C
900   IRCODE=0
      RETURN
      END SUBROUTINE

      END MODULE
