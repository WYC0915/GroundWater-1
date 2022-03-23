      MODULE s_RDPOLH
      CONTAINS

C*
      SUBROUTINE RDPOLH(LFN,FILTYP,TITLE,POLTYP,IFORM,IEND,
     1                  NUTNAM,SUBNAM)
CC
CC NAME       :  RDPOLH
CC
CC PURPOSE    :  READ HEADER PART OF THE POLE COORDINATE FILE
CC               OLD AND NEW FORMAT.
CC               BERNESE AND IGS/IERS FORMATS SUPPORTED
CC
CC PARAMETER  :
CC        IN  :  LFN     : LOGICAL FILE NUMBER                  I*4
CC               FILTYP  : POLE FILE (HEADER TYPE)              I*4
CC                          =1 BERNESE POLE FORMAT
CC                          =2 IERS/IGS POLE FORMAT
CC       OUT  :  TITLE   : TITLE (SAME AS IN JOBOUTPUT)        CH*80
CC               POLTYP(I): I=1 NUTATION MODEL                  I*4(*)
CC                              1=NO, 2=OBSERVED, 3=HERRING
CC                          I=2 SUBDAILY POLE MODEL
CC                              1=NO, 2=RAY
CC               IFORM   : VERSION INDICATOR                    I*4
CC                         BERNESE:  0=UNKNOWN 1=NEW 2=OLD
CC                         IGS/IERS: 0=OLD 1=NEW 2='VERSION 2'
CC               IEND    : END DURING READING HEADER            I*4
CC               NUTNAM  : NAME OF THE NUTATION MODEL           CH*16
CC               SUBNAM  : NAME OF THE SUBDAILY POLE MODEL      CH*16
CC
CC REMARKS    :
CC
CC AUTHOR     :  S.FANKHAUSER
CC
CC VERSION    :  3.3
CC
CC CREATED    :  19-NOV-92
CC
CC CHANGES    :  18-JUL-92 : ??: TITLE INSTEAD OF CRETIM, FILNAM REMOVED
CC               19-APR-94 : SF: NUTATION TYPE INCLUDED
CC               10-AUG-94 : MR: CALL EXITRC
CC               05-SEP-94 : MR: NUTATION MODEL IF OLD POLE FORMAT USED
CC               04-JUN-96 : TS: SUBDAILY POLE MODEL ADDED
CC               16-JUL-98 : DI: NEW IGS/IERS FORMAT (VERSION 2)
CC               04-DEC-02 : PS: ADD NUTNAM (not used yet) and SUBNAM
CC               07-JAN-03 : PS: USE NUTNAM
CC               05-FEB-03 : PS: CORRECT USE OF SUBNAM FOR IERS FORMAT
CC               06-NOV-03 : HU: SET POLTYP IN ALL CASES
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE s_exitrc
      USE s_upperc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I1
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C GLOBAL DECLARATIONS
C -------------------
      CHARACTER*80 TITLE
      CHARACTER*9  HLPSTR
      CHARACTER*16 SUBNAM
      CHARACTER*16 NUTNAM
C
      INTEGER*4    LFN,FILTYP,IFORM,IEND,POLTYP(*)
C
C INTERNAL DECLARATIONS
C ---------------------
      CHARACTER*132 LINE
C
C
C INITIALIZATION OF SOME VARIABLES
C --------------------------------
      IEND=0
      POLTYP(1)=99
      POLTYP(2)=99
      IFORM=99
      SUBNAM=' '
      NUTNAM=' '
C
C 1) BERNESE POLE FORMAT
C ======================
      IF (FILTYP.EQ.1) THEN
C
C READ 1.LINE OF HEADER
C ----------------------
        READ(LFN,'(A)',END=900) TITLE
C
C READ 2. LINE OF HEADER
C ----------------------
        READ(LFN,*,END=900)
C
C READ 3. LINE OF HEADER
C -----------------------
        READ(LFN,'(A)',END=900) LINE
C OLD: "CELESTIAL POLE OFFSETS"
        IF (LINE( 1:33).EQ.' MJD    X-POLE   Y-POLE   UT1-UTC' .OR.
     1      LINE(24:33).EQ.'          '                        .OR.
     2      LINE(24:33).EQ.'NO        ')  POLTYP(1)=1
        IF (LINE(24:33).EQ.'OBSERVED  ')  POLTYP(1)=2
        IF (LINE(24:33).EQ.'HERRING   ')  POLTYP(1)=3
C
C NEW: "NUTATION MODEL"
        IF(LINE(1:14).EQ.'NUTATION MODEL') THEN
          NUTNAM=LINE(24:39)
C USE CELESTIAL OFFSETS FROM ERP FILE
          IF (NUTNAM.NE.' ') POLTYP(1)=2
        ENDIF
        IF(LINE(1:22).EQ.'CELESTIAL POLE OFFSET:') THEN
          IF (LINE(24:33).EQ.'NO        ')  NUTNAM='IAU80           '
          IF (LINE(24:33).EQ.'OBSERVED  ')  NUTNAM='IAU80           '
          IF (NUTNAM.NE.' ') POLTYP(1)=2
        ENDIF
        IF (POLTYP(1).EQ.99) THEN
          WRITE(LFNERR,13) LINE(24:33)
13        FORMAT(/,' *** SR RDPOLH: CELESTIAL POLE OFFSET MODEL NOT ',
     1             'VALID: ',A10,/,
     2             16X,'CHECK POLE FILE HEADER',/)
          CALL EXITRC(2)
        ENDIF
C
C GET SUBDAILY POLE MODEL
C -----------------------
        IF (LINE( 1:33).EQ.' MJD    X-POLE   Y-POLE   UT1-UTC' .OR.
     1      LINE(70:79).EQ.'          '                        .OR.
     2      LINE(70:79).EQ.'NO        ')  POLTYP(2)=1
        IF (LINE(70:79).EQ.'RAY       ')  POLTYP(2)=2

C        IF (POLTYP(2).EQ.99) THEN
C           WRITE(LFNERR,14) LINE(70:79)
C14         FORMAT(/,' *** SR RDPOLH: SUBDAILY POLE MODEL NOT ',
C     1              'VALID: ',A10,/,
C     2              16X,'CHECK POLE FILE HEADER',/)
C           CALL EXITRC(2)
C        ENDIF

        IF(LINE(49:67).EQ.'SUBDAILY POLE MODEL') THEN
          SUBNAM=LINE(70:85)
        ENDIF
C
C READ 4. LINE, CHECK IF NEW OR OLD FORMAT
C ----------------------------------------
        READ(LFN,'(A)',END=900)LINE
        IFORM=0
        IF(LINE(1:24).EQ.'    DATE   TIME   X-POLE')THEN
          IFORM=1
          DO 20 I1=1,2
            READ(LFN,*,END=900)
20        CONTINUE
        END IF
        IF(LINE(1:21).EQ.'         (")      (")')IFORM=2
        GOTO 999
C
C 2) IERS/IGS POLE FORMAT
C =======================
      ELSE IF (FILTYP.EQ.2) THEN
C
C READ TITLE AND THE POLE MODEL
C -----------------------------
        READ(LFN,'(A)',END=900) TITLE
        HLPSTR=TITLE(1:9)
        CALL UPPERC(HLPSTR)
        IF (HLPSTR.EQ.'VERSION 2') THEN
          IFORM=2
          READ(LFN,'(A)',END=900) TITLE
        ENDIF
        READ(LFN,*,END=900)
C
C DETERMINE NUTATION MODEL
C ------------------------
        READ(LFN,'(A)',END=900) LINE
        IF (LINE( 1: 5).EQ.'TIME ' .OR.
     1      LINE( 1: 5).EQ.'  MJD' .OR.
     2      LINE(24:33).EQ.'NO        ')  POLTYP(1)=1
        IF (LINE(24:33).EQ.'OBSERVED  ')  POLTYP(1)=2
        IF (LINE(24:33).EQ.'HERRING   ')  POLTYP(1)=3
C        IF (POLTYP(1).EQ.99) THEN
C           WRITE(LFNERR,13) LINE(24:33)
C           CALL EXITRC(2)
C        ENDIF

      IF(LINE(1:14).EQ.'NUTATION MODEL') THEN
        NUTNAM=LINE(24:39)
C USE CELESTIAL OFFSETS FROM ERP FILE
        POLTYP(1)=2
      ENDIF

      IF(LINE(1:22).EQ.'CELESTIAL POLE OFFSET:') THEN
        IF (LINE(24:33).EQ.'NO        ') NUTNAM='IAU80           '
        IF (LINE(24:33).EQ.'OBSERVED  ') NUTNAM='IAU80           '
        IF (POLTYP(1).EQ.99) THEN
           WRITE(LFNERR,13) LINE(24:33)
        ENDIF
      ENDIF



C
C GET SUBDAILY POLE MODEL
C -----------------------
        IF (LINE( 1: 5).EQ.'TIME ' .OR.
     1      LINE( 1: 5).EQ.'  MJD' .OR.
     2      LINE(70:79).EQ.'NO        ')  POLTYP(2)=1
        IF (LINE(70:79).EQ.'RAY       ')  POLTYP(2)=2
C        IF (POLTYP(2).EQ.99) THEN
C           WRITE(LFNERR,14) LINE(70:79)
C           CALL EXITRC(2)
C        ENDIF

      IF(LINE(49:67).EQ.'SUBDAILY POLE MODEL') THEN
        SUBNAM=LINE(70:85)
      ENDIF

C
C READ NEW LINE IF NECESSARY
C --------------------------
        IF (LINE( 1: 5).NE.'TIME ' .AND.
     1      LINE( 1: 5).NE.'  MJD') READ(LFN,'(A)',END=900)LINE
C
C DETERMINE FORMAT TYPE
C ---------------------
        IF (IFORM.EQ.2) THEN
          IFORM=2
        ELSE IF (LINE(1:5).EQ.'TIME ') THEN
          IFORM=0
        ELSE IF (LINE(1:5).EQ.'  MJD') THEN
          IFORM=1
        ELSE
          WRITE(LFNERR,901)TITLE
901       FORMAT(/,' *** SR RDPOLH: UNKNOWN POLE FORMAT',/,
     1                         16X,'POLE FILE TITLE: ',A,/)
          CALL EXITRC(2)
        ENDIF
C
        READ(LFN,*,END=900)
C
        GOTO 999
C
C UNDEFINED POLE FORMAT
C ---------------------
      ELSE
        WRITE(LFNERR,902)FILTYP
902     FORMAT(/,' *** SR RDPOLH: UNKNOW POLE HEADER FILE TYPE',/,
     1                       16X,'FILE TYPE: ',I4,/)
        CALL EXITRC(2)
      ENDIF
C
C END DURING READING HEADER
C -------------------------
900   CONTINUE
      IEND=1
C
999   RETURN
      END SUBROUTINE

      END MODULE
