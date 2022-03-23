      MODULE f_poloff
      CONTAINS

C*
      FUNCTION POLOFF(TIME,IXYUTC)
CC
CC TITLE      :  COMPUTE OFFSET 90C04 POLE TO IERS REF. FRAME
CC
CC PURPOSE    :  COMPUTE THE OFFSET OF THE 90 C04 POLE SERIES
CC               WITH RESPECT TO THE IERS REFERENCE FRAMES.
CC
CC PARAMETERS :
CC         IN :  TIME   : ACTUAL TIME (MJD)                   R*8
CC               IXYZ   : POLE COORDINATE                     I*4
CC                        1 = X-POLE, 2 = Y-POLE, 3 = UT1-UTC
CC                        4 = DELTA PSI, 5 = DELTA EPS
CC     IN/OUT :  ---
CC        OUT :  POLOFF : OFFSET IN ARC SECONDS               R*8
CC      LOCAL :  ---
CC
CC COMMON     :  ---
CC
CC INCLUDE    :  ---
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S.FANKHAUSER
CC
CC VERSION    :  3.5  (APR 94)
CC
CC CREATED    :  26-APR-94
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC               13-OCT-94 : MR: REPLACE "X" BY "1X" IN FORMAT
CC               10-SEP-95 : LM: REORDER DATA STATEMENT
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1994     UNIVERSITY OF BERN
CC               SWITZERLAND
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
      INTEGER*4 I1    , IDD   , IFIRST, IMM   , IOSTAT, IRC   , IXYUTC,
     1          IYY
C
      REAL*8    HH    , POLOFF, REFTIM, RINTER, TIME
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C LOCAL DECLARATIONS
C ------------------
      CHARACTER*32 OFFFIL
      REAL*8       A(10),A1(10)
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
        CALL GTFLNA(0,'OFFFIL ',OFFFIL,IRC)
        IF (IRC.EQ.1) THEN
          DO 10 I1=1,10
            A(I1)=0.D0
            A1(I1)=0.D0
10        CONTINUE
          REFTIM=0.D0
        ELSE
C
C OPEN THE FILE WITH THE COEFFICIENTS
C -----------------------------------
          CALL OPNFIL(LFNLOC,OFFFIL,'OLD','FORMATTED',
     1                'READONLY',' ',IOSTAT)
          CALL OPNERR(LFNERR,LFNLOC,IOSTAT,OFFFIL,'POLOFF')
C
C GET THE VALUES OF THE COEFFICIENTS
C ----------------------------------
          READ(LFNLOC,1,ERR=910)IYY,IMM,IDD,HH
1         FORMAT(//,34X,I4,1X,I2,1X,I2,1X,F4.1)
          READ(LFNLOC,2,ERR=910)
2         FORMAT(///)
          READ(LFNLOC,3,ERR=910)(A(I1),I1=1,10)
          READ(LFNLOC,3,ERR=910)(A1(I1),I1=1,10)
3         FORMAT(2X,5(2X,F5.2,2X,F4.2))
          REFTIM=DJUL(IYY,IMM,IDD+HH/24.D0)
          CLOSE(LFNLOC)
        ENDIF
      ENDIF
C
C COMPUTE TIME INTERVAL
C ---------------------
      RINTER=(TIME-REFTIM)/365.242
C
C COMPUTE THE OFFSET
C ------------------
      POLOFF=A(IXYUTC*2-1)+A1(IXYUTC*2-1)*RINTER
      IF (IXYUTC.EQ.3)THEN
        POLOFF=POLOFF/10000.D0
      ELSE
        POLOFF=POLOFF/1000.D0
      ENDIF
      GOTO 900
C
C HANDLING THE ERROR MESSAGE
C --------------------------
910   WRITE(LFNERR,906)OFFFIL
906   FORMAT(/,' *** SR POLOFF: ERROR READING POLE OFFSET FILE',/,
     1         '                FILE NAME: ',A32,/)
      CALL EXITRC(2)
C
C RETURN CODES
C ------------
900   RETURN
      END FUNCTION
C

      END MODULE
