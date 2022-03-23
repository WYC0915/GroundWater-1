      MODULE s_WTPOLH
      CONTAINS

C*
      SUBROUTINE WTPOLH(LFN,FILTYP,TITLE,POLTYP,NUTNAM,SUBNAM)
CC
CC NAME       :  WTPOLH
CC
CC PURPOSE    :  WRITE HEADER PART OF THE POLE COORDINATE FILE
CC
CC PARAMETER  :
CC        IN  :  LFN     : LOGICAL FILE NUMBER                  I*4
CC               FILTYP  : POLE FILE (HEADER TYPE)              I*4
CC                          =1 BERNESE POLE FORMAT
CC                          =2 IERS/IGS POLE FORMAT VERSION 2
CC               TITLE   : TITLE (SAME AS IN JOBOUTPUT)        CH*80
CC               POLTYP(I): I=1 NUTATION MODEL                  I*4(*)
CC                              1=NO, 2=OBSERVED, 3=HERRING
CC                          I=2 SUBDAILY POLE MODEL
CC                              1=NO, 2=RAY
CC               NUTNAM  : NAME OF THE NUTATION MODEL          CH*16
CC               SUBNAM  : NAME OF THE SUBDAILY POLE MODEL     CH*16
CC
CC REMARKS    :
CC
CC AUTHOR     :  S.FANKHAUSER
CC
CC VERSION    :  3.3
CC
CC CREATED    :  19-NOV-91
CC
CC CHANGES    :  18-JUN-92 : ??: TITLE INSTEAD OF CREATION TIME
CC               07-DEC-92 : ??: USE LENGTH TO WRITE "LINE"
CC               19-APR-94 : SF: WRITE NUTATION MODEL INTO HEADER
CC               04-JUN-96 : TS: ADDED SUBDAILY POLE MODEL
CC               05-JUN-96 : TS: ADDED FILE TYPE
CC               16-JUL-98 : DI: IERS/IGS POLE FORMAT VERSION 2 ADDED
CC               04-DEC-02 : PS: WRITE NAME OF SUDAILY MODEL FROM SUB FILE
CC                               INTO ERP HEADER
CC               07-JAN-03 : PS: WRITE NAME OF NUTATION MODEL FROM NUT FILE
CC                               INTO ERP HEADER
CC               03-FEB-03 : PS: CHANGED INPUT
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-JUN-05 : MM: UNUSED VARIABLES REMOVED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE s_exitrc
      USE f_lengt1
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I1    , LFN
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C GLOBAL DECLARATIONS
C -------------------
      CHARACTER*80 TITLE
C
      INTEGER*4    FILTYP,POLTYP(*)
C
C INTERNAL DECLARATIONS
C ---------------------
      CHARACTER*132 LINE
      CHARACTER*16  SUBNAM,NUTNAM
C

C
C BERNESE POLE FORMAT
C -------------------
      IF (FILTYP.EQ.1) THEN
C
C WRITE 1.LINE OF HEADER
C ----------------------
        WRITE(LFN,'(A)')TITLE
C
C WRITE 2.LINE OF HEADER
C ----------------------
        DO 10 I1=1,120
          LINE(I1:I1)='-'
10      CONTINUE
        WRITE(LFN,'(A)') LINE(1:LENGT1(LINE))
C
C WRITE 3.LINE OF HEADER
C ----------------------
        LINE=' '
C        LINE(1:23) ='CELESTIAL POLE OFFSET: '
        LINE(1:23) ='NUTATION MODEL       : '
        LINE(24:39)= NUTNAM
        LINE(49:69) ='SUBDAILY POLE MODEL: '
        LINE(70:85)= SUBNAM
        WRITE(LFN,'(A)') LINE(1:LENGT1(LINE))
C
C WRITE 4.LINE OF HEADER
C ----------------------
        LINE=' '
        LINE(1:51)='    DATE   TIME   X-POLE   Y-POLE   UT1-UTC GPS-UTC'
        LINE(52:99)='    RMS XP  RMS YP   RMS DT   DE-CPO   DP-CPO   '
        LINE(100:)='RMS EP  RMS PS'
        WRITE(LFN,'(A)') LINE(1:LENGT1(LINE))
C
C WRITE 5.LINE OF HEADER
C ----------------------
        LINE=' '
        LINE(1:50)='YYYY MM DD HH MM    (")      (")      (S)     (S)'
        LINE(51:99)='REM     (")     (")     (S)      (")     (")    '
        LINE(100:)=' (")      (")'
        WRITE(LFN,'(A)') LINE(1:LENGT1(LINE))
C
C WRITE 6.LINE OF HEADER
C ----------------------
        WRITE(LFN,*)
C
C IERS/IGS POLE FORMAT VERSION 2
C ------------------------------
      ELSE IF (FILTYP.EQ.2) THEN
        WRITE(LFN,120)TITLE
120     FORMAT('VERSION 2',/,A,/,145('-'))
C
        LINE=' '
C        LINE(1:23) ='CELESTIAL POLE OFFSET: '
        LINE(1:23) ='NUTATION MODEL       : '
        LINE(24:39)=NUTNAM
        LINE(49:69) ='SUBDAILY POLE MODEL: '
        LINE(70:85)=SUBNAM
        WRITE(LFN,'(A)') LINE(1:LENGT1(LINE))
C
        WRITE(LFN,130)
130     FORMAT(
     1  '  MJD         X-P      Y-P   UT1UTC    LOD   S-X   S-Y  S-UT',
     2  '  S-LD  NR  NF  NT   X-RT   Y-RT  S-XR  S-YR C-XY C-XT C-YT ',
     3  '  DPSI   DEPS  S-DP  S-DE',/,
     4  '              E-6"     E-6"    E-7S E-7S/D   E-6"  E-6" E-7S',
     5  ' E-7S/D                 E-6"/D      E-6"/D    E-2  E-2  E-2',
     6  '    E-6"   E-6"  E-6"  E-6"')
C
C UNDEFINED POLE FORMAT
C ---------------------
      ELSE
        WRITE(LFNERR,901)FILTYP
901     FORMAT(/,' *** SR WTPOLH: UNKNOW POLE HEADER FILE TYPE',/,
     1                       16X,'FILE TYPE: ',I4,/)
        CALL EXITRC(2)
      ENDIF
C
999   RETURN
      END SUBROUTINE

      END MODULE
