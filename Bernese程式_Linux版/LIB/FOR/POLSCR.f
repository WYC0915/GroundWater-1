      MODULE s_POLSCR
      CONTAINS

C*
      SUBROUTINE POLSCR(IFIL  ,FILKEY,NPVAL ,TSAV  ,POLCOR,GPSUTC,
     1                  ERR   ,POLTYP)
CC
CC NAME       :  POLSCR
CC
CC PURPOSE    :  SAVE THE VALUES OF THE ESTIMATED POLE PARAMETERS.
CC
CC PARAMETERS :
CC         IN :  IFIL   : FILE NUMBER JUST PROCESSED            I*4
CC               FILKEY : FILE KEY TO SEARCH FOR POLE INFO      C*7
CC               NPVAL  : NUMBER OF SAVE TIMES                  I*4
CC               TSAV(I): TIME OF SAVING EPOCH (MJD)            R*8(*)
CC                        I=1,NPVAL
CC               POLCOR(I,J): (I=1,5); POLE COORDINATES         R*8(5,*)
CC                         I=1 :VALUE OF X-POLE (ARC SECONDS)
CC                         I=2 :VALUE OF Y-POLE (ARC SECONDS)
CC                         I=3 :VALUE OF UT1-UTC (SECONDS)
CC                         I=4 :VALUE OF EPS (ARC SECONDS)
CC                         I=5 :VALUE OF PSI (ARC SECONDS)
CC                         J=1,NPVAL
CC               GPSUTC(I):GPS-UTC DIFFERENC GIVEN IN SECONDS   R*8(*)
CC                         I=1,NPVAL
CC               REM     : REMARK                               CH*3
CC               ERR(I,J): (I=1,5) RMS OF POLE COORDINATES      R*8(5,*)
CC                         I=1 :RMS OF X-POLE (ARC SECONDS)
CC                         I=2 :RMS OF Y-POLE (ARC SECONDS)
CC                         I=3 :RMS OF UT1-UTC (SECONDS)
CC                         I=4 :RMS OF EPS (ARC SECONDS)
CC                         I=5 :RMS OF PSI (ARC SECONDS)
CC                         J=1,NPVAL
CC               POLTYP(I): I=1 NUTATION MODEL                  I*4(*)
CC                              1=NO, 2=OBSERVED, 3=HERRING
CC                          I=2 SUBDAILY POLE MODEL
CC                              1=NO, 2=RAY
CC
CC REMARKS    :  PARAMETER SETS MUST BE SORTED CHRONOLOGICALLY
CC
CC AUTHOR     :  E.BROCKMANN
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  24-MAY-93
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC               05-JUN-96 : TS: ADDED SUBDAILY MODEL
CC               05-FEB-03 : PS: CHANGED CALL TO SR WTPOLH
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1992     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE s_opnfil
      USE s_opnerr
      USE s_wtpolh
      USE s_wtpoli
      USE s_exitrc
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IFIL  , IOSTAT, IPOL  , IRC   , NPVAL
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C GLOBAL DECLARATION
C ------------------
      CHARACTER*80 TITLE
      CHARACTER*32 FILPOL
      CHARACTER*16 NUTNAM
      CHARACTER*16 SUBNAM
      CHARACTER*7  FILKEY
      CHARACTER*3  REM
C
      REAL*8       TSAV(*),GPSUTC(*),POLCOR(5,*),ERR(5,*)
C
      INTEGER*4    POLTYP(*)
C
C
C IF OUTPUT FILENAME IS BLANK, THEN NO INFORMATION IS SAVED
C ---------------------------------------------------------
      IF (IFIL.EQ.1) THEN
        CALL GTFLNA(0,FILKEY,FILPOL,IRC)
        IF(IRC.NE.0) THEN
          WRITE(LFNERR,16) FILPOL,FILKEY
16        FORMAT(/,' *** SR POLSCR: YOU HAVE TO SPECIFY A ',/,
     1                        16X,'POLE FILE :',A,/,
     2                        16X,'KEY-WORD  :',A,/)
          CALL EXITRC(2)
        ENDIF
C
C OPEN THE OUTPUT POLE FILE AND WRITE THE HEADER
C ----------------------------------------------
        CALL GTFLNA(1,FILKEY,FILPOL,IRC)
        CALL OPNFIL(LFNRES,FILPOL,'UNKNOWN',' ',' ',' ',IOSTAT)
        IF (IOSTAT.NE.0) THEN
          CALL OPNERR(LFNERR,LFNRES,IOSTAT,FILPOL,'POLSCR')
          CLOSE(LFNRES)
        END IF
        TITLE='SCRATCH POLE FILE'
        NUTNAM='SCRATCH'
        SUBNAM='SCRATCH'
        CALL WTPOLH(LFNRES,1,TITLE,POLTYP,NUTNAM,SUBNAM)
      ENDIF
C
C SAVE THE INFORMATION
C --------------------
      REM='GPS'
      DO 70 IPOL=1,NPVAL
        CALL WTPOLI(LFNRES,TSAV(IPOL),POLCOR(1,IPOL),GPSUTC(IPOL),
     1              REM,ERR(1,IPOL))
70    CONTINUE
C
C RETURN CODES
C ------------
999   RETURN
      END SUBROUTINE

      END MODULE
