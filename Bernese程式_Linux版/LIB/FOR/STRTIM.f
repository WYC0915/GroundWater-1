      MODULE s_STRTIM
      CONTAINS

C*
      SUBROUTINE STRTIM(NTIM,TSTRNG,TMJD)
CC
CC NAME       :  STRTIM
CC
CC PURPOSE    :  READ ONE OR TWO DATES WITH TIME FROM A STRING WITH
CC               THE FOLLOWING FORMAT:
CC                     90-12-24 20:13:34    OR
CC                     90-12-24 20:13:34  90-12-25 21:13:13
CC               AND RETURN THE MODIFIED JULIAN DATE(S)
CC
CC PARAMETERS :
CC         IN :  NTIM   : NUMBER OF DATES (1 OR 2)            I*4
CC               TSTRNG : STRING CONTAINING THE DATE(S)       CH
CC        OUT :  TMJD(I),I=1,2: MODIFIED JULIAN DATE(S)       R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  90/06/29 17:42
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC               01-JUL-99 : PF: CALL IYEAR4 FOR CONVERSION YY->YYYY
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1990     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE f_djul
      USE s_exitrc
      USE f_iyear4
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 ICH1  , ICH2  , IDAY  , IHOUR , IMIN  , IMONTH, ISEC  ,
     1          ITIM  , IYEAR , NTIM
C
      REAL*8    DAY
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER TSTRNG*(*)
      REAL*8    TMJD(*)
C
C
C LOOP OVER DATES
C ---------------
      DO 100 ITIM=1,NTIM
C
C READ FROM STRING
        ICH1=(ITIM-1)*19+1
        ICH2=(ITIM-1)*19+17
        READ(TSTRNG(ICH1:ICH2),1,END=900) IYEAR,IMONTH,IDAY,
     1                                    IHOUR,IMIN,ISEC
1       FORMAT(I2,5(1X,I2))
C
C ADJUST YEAR
        IYEAR = IYEAR4(IYEAR)
C
C CONVERT TO MODIFIED JULIAN DATE
        DAY=IDAY+IHOUR/24.D0+IMIN/1440.D0+ISEC/86400.D0
        TMJD(ITIM)=DJUL(IYEAR,IMONTH,DAY)
C
100   CONTINUE
C
      GOTO 999
C
C STRING TOO SHORT TO READ DATE(S)
C --------------------------------
900   WRITE(LFNERR,901) LEN(TSTRNG)
901   FORMAT(/,' *** SR STRTIM: STRING TOO SHORT TO READ DATE(S)',/,
     1                16X,'LENGTH OF STRING        :',I3,' BYTES',/,
     2                16X,'LENGTH TO READ ONE DATE : 17 BYTES',/,
     3                16X,'LENGTH TO READ TWO DATES: 36 BYTES',/)
      CALL EXITRC(2)
C
C END
999   RETURN
      END SUBROUTINE

      END MODULE
