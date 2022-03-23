      MODULE s_SINDAT
      CONTAINS

C*
      SUBROUTINE SINDAT(ITYPE,TMJD,TSTRNG)
CC
CC NAME       :  SINDAT
CC
CC PURPOSE    :  WRITE AND READ THE FOLLOWIN FORMAT LINE:    95:123:52328
CC
CC PARAMETERS :
CC         IN :  ITYPE  : CONVERSION DIRECTION                I*4
CC                        0: TMJD --> TSTRNG
CC                        1: TSTRNG --> TMJD (START EPOCH)
CC                        2: TSTRNG --> TMJD (END EPOCH)
CC     IN/OUT :  TMJD   : MODIFIED JULIAN DATE                R*8
CC               TSTRNG : STRING CONTAINING THE DATE          CH
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  E.BROCKMANN
CC
CC VERSION    :  3.6
CC
CC CREATED    :  30-MAY-95
CC
CC CHANGES    :  07-SEP-95 : ALLOW BOTH CONVERSIONS
CC               11-MAR-96 : EB: DO NOT TRANFOREM IF NO ':'
CC               11-MAY-99 : HB: SOLVING Y2K-PROBLEM AND
CC                               0.D0  --> 00:000:00000
CC                               1.D20 --> 00:000:00000
CC                               00:000:00000 --> 0.D0  (START EPOCH)
CC                               00:000:00000 --> 1.D20 (END EPOCH)
CC               01-JUL-99 : PF: CALL TO IYEAR FOR CONVERSION YY->YYYY
CC               01-MAY-03 : SS: PREVENT ROUNDING PROBLEM
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               16-AUG-06 : HU: UNDEFINED LOWER BOUNDARY CORRECTED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1990     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE f_djul
      USE f_iyear4
      USE s_jmt
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IDAY  , IMON  , ISEC  , ITYPE , IYEAR , JAN
C
      REAL*8    DAY   , FIRST , RSEC  , TMJDI
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER TSTRNG*(*)
      REAL*8    TMJD
C
C
      DATA JAN/1/
      DATA FIRST/1.D0/
C
      IF (ITYPE.EQ.0) THEN
        TSTRNG=' '
        IF ((TMJD.EQ.1.D20).OR.(TMJD.EQ.0.D0)) THEN
          WRITE(TSTRNG,'(A)') '00:000:00000'
        ELSE
          TMJDI=TMJD+0.1/86400.D0
          CALL JMT(TMJDI,IYEAR,IMON,DAY)
          IDAY=DAY
          RSEC=(DAY-IDAY*1.D0)*86400.D0
          ISEC=RSEC
          IDAY=TMJDI-DJUL(IYEAR,JAN,FIRST)+1
          IYEAR=MOD(IYEAR,100)
          WRITE(TSTRNG,1) IYEAR,IDAY,ISEC
        ENDIF
      ENDIF
      IF ((ITYPE.EQ.1).OR.(ITYPE.EQ.2)) THEN
        IF (TSTRNG(3:3).EQ.':'.AND.TSTRNG(7:7).EQ.':') THEN
          READ(TSTRNG,2) IYEAR,IDAY,ISEC
          IF(IDAY.EQ.0) THEN
            IF(ITYPE.EQ.2) THEN
              TMJD=1.D20
            ELSE
              TMJD=0.D0
            ENDIF
          ELSE
c            IF(IYEAR.LT. 90) IYEAR=IYEAR+2000
c            IF(IYEAR.LT.100) IYEAR=IYEAR+1900
            IYEAR = IYEAR4(IYEAR)
            TMJD=IDAY*1.D0+DJUL(IYEAR,JAN,FIRST)-1.D0+ISEC/86400.D0
          ENDIF
        ENDIF
      ENDIF
C
C FORMAT
C ------
1     FORMAT(I2.2,':',I3.3,':',I5.5)
2     FORMAT(I2,1X,I3,1X,I5)
C
C END
C ---
      RETURN
      END SUBROUTINE

      END MODULE
