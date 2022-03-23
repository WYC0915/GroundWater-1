      MODULE f_dgpsut
      CONTAINS

C*
      FUNCTION DGPSUT(TIME)
CC
CC TITLE      :  GET DIFFERENCE OF GPS-TIME - UTC-TIME
CC
CC PURPOSE    :  GET THE DIFFERENCE GPS-TIME - UTC-TIME IN
CC               SECONDS.
CC
CC PARAMETERS :
CC         IN :  TIME   : ACTUAL TIME (MJD)                   R*8
CC     IN/OUT :  ---
CC        OUT :  DGPSUT : GPS-UTC IN SECONDS                  R*8
CC      LOCAL :  ---
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S. FANKHAUSER
CC
CC VERSION    :  3.5  (MAR 94)
CC
CC CREATED    :  23-MAR-94
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC               13-OCT-94 : MR: REPLACE "X" BY "1X" IN FORMAT
CC               23-MAR-95 : MR: REPLACE "X" BY "1X" IN FORMAT
CC               10-SEP-95 : LM: REORDER DATA STATEMENT
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               02-DEC-05 : HB: SET LEAP SECOND FOR EPOCH BEFORE FIRST
CC                               ENTRY IN GPSUTC. TO -10 SEC
CC               21-JAN-06 : HU: WRITE WARNING MESSAGE ONLY ONCE
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1994     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE s_opnfil
      USE f_djul
      USE s_opnerr
      USE s_exitrc
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I1    , IDD   , IFIRST, IHH   , IMI   , IMM   , IOSTAT,
     1          IRC   , IX    , IYY   , MAXVAL, NNVAL , NVAL  , IERR
C
      REAL*8    DGPSUT, SS    , TIME
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C PARAMTETER STATEMENTS
C ---------------------
      PARAMETER(MAXVAL=100)
C
C LOCAL DECLARATIONS
C ------------------
      CHARACTER*132 LINE
      CHARACTER*32  TIMFIL
      REAL*8        DIFF(MAXVAL),T(MAXVAL)
C
C INCLUDE LOGICAL FILE NUMBERS
C ----------------------------
C
C DATA STATEMENTS
C ---------------
      DATA IFIRST/1/
C
C GET FILE NAME OF FILE WITH THE OFFSET COEFFICIENTS
C --------------------------------------------------
      IF (IFIRST.EQ.1) THEN
        IFIRST=0
        IERR  =0
        CALL GTFLNA(1,'GPSUTC ',TIMFIL,IRC)
C
C OPEN THE FILE WITH THE COEFFICIENTS
C -----------------------------------
        CALL OPNFIL(LFNLOC,TIMFIL,'OLD','FORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNLOC,IOSTAT,TIMFIL,'DGPSUT')
C
C GET THE VALUES OF THE COEFFICIENTS
C ----------------------------------
        READ(LFNLOC,1)
1       FORMAT(////)
        DO 10 NVAL=1,100
          READ(LFNLOC,11,END=20) LINE
11        FORMAT(A)
          IF(LINE.EQ.' ')GOTO 20
          READ(LINE,2,ERR=920) DIFF(NVAL),IYY,IMM,IDD,IHH,IMI,SS
2         FORMAT(F5.0,7X,I4,4(1X,I2),1X,F5.2)
          T(NVAL)=DJUL(IYY,IMM,IDD+IHH/24.D0+IMI/1440.D0+SS/86400.D0)
10      CONTINUE
20      CONTINUE
        NNVAL=NVAL-1
        CLOSE(LFNLOC)
      ENDIF
C
C CHOOSE THE DIFFERENC
C --------------------
      DO 30 I1=1,NNVAL
        IF (T(I1).GT.TIME.AND.I1.GT.1)THEN
          IX=I1-1
          GOTO 40
        ELSE IF (T(I1).GT.TIME.AND.I1.EQ.1) THEN
          DGPSUT=DIFF(1)-1
          IF (IERR.EQ.0) THEN
            IERR=1
            GOTO 930
          ELSE
            GOTO 900
          ENDIF
        ENDIF
30    CONTINUE
      IX=NNVAL
40    CONTINUE
      DGPSUT=DIFF(IX)
      GOTO 900
C
C HANDLING THE ERROR MESSAGES
C ---------------------------
920   WRITE(LFNERR,906)TIMFIL
906   FORMAT(/,' *** SR DGPSUT: ERROR READING LEAP SECOND FILE',/,
     1         '                FILE NAME: ',A32)
      CALL EXITRC(2)
930   WRITE(LFNERR,936)TIME,T(1),TIMFIL,DGPSUT
936   FORMAT(/,' ### SR DGPSUT: NO GPS-UTC TIME OFFSET AVAILABLE',/,
     1         '                FOR TIME:            ',F15.5,/,
     2         '                1ST VALUE IN FILE:   ',F15.5,/,
     3         '                LEAP SECOND FILE:    ',A32,/,
     4         '                LEAP SECONDS SET TO: ',F5.0)
!!936   FORMAT(/,' *** SR DGPSUT: NO GPS-UTC TIME OFFSET AVAILABLE',/,
!!     1         '                FOR TIME:          ',F15.5,/,
!!     2         '                1ST VALUE IN FILE: ',F15.5,/,
!!     3         '                LEAP SECOND FILE:  ',A32)
!!      CALL EXITRC(2)
C
C RETURN CODES
C ------------
900   RETURN
      END FUNCTION
C

      END MODULE
