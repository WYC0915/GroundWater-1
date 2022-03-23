      MODULE s_MRKEPO
      CONTAINS

C*
      SUBROUTINE MRKEPO(NOBS,NOBSM,IDOBS,STRING,IFREQ,ISAT,ICHR,TEST,
     1                  EPOMRK,EPOSTR)
CC
CC NAME       :  MRKEPO
CC
CC PURPOSE    :  WRITE EPOCH NUMBERS IN A STRING IF THE
CC               STATUS OF THE SATELLITE CHANGES
CC               (OBSERVED/NOT OBSERVED ; MARKED/NOT MARKED)
CC               THE RESULTING STRING IS DISPLAYED AT THE BOTTOM
CC               OF THE GRAPHIC TABLE
CC
CC PARAMETERS :
CC         IN :  NOBS   : NUMBER OF OBSERVATIONS  NOBS(2,MAXSAT,MAXCHR)
CC               NOBSM  : NUMBER OF MARKED OBS.   NOBSM(2,MAXSAT,MAXCHR)
CC               IDOBS  : NUMBER OF OBSERVATIONS PER ONE CHAR. I*4
CC               STRING : GRAPHIC STRING          CH*1(2,MAXSAT,MAXCHR)
CC               IFREQ  : ACTUAL FREQUENCY                     I*4
CC               ISAT   : ACTUAL SATELLITE                     I*4
CC               ICHR   : ACTUAL POSITION IN THE STRING        I*4
CC               TEST   : TEST STRING; INDICATES THE GRAPHIC
CC                        SYMBOL (*,-, ) TO BE TESTED          CH*1
CC               EPOMRK : ARRAY CONTAINING THE LAST EPOCH
CC                        NUMBER OF ONE GRAPHIC SYMBOL      I*4(MAXCHR)
CC     IN/OUT :  EPOSTR : STRING CONTAINING EPOCH NUMBERS      CH*255
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  U. WILD
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  88/08/23 10:31
CC
CC CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IDOBS , MXCCHR, MXCSAT
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C COMMONS
C -------
      COMMON /MCMSAT/ MXCSAT,MXNSAT
      COMMON /MCMCHR/ MXCCHR,MXNCHR
C
C DECLARATIONS
C ------------
      INTEGER*4    IFREQ,ISAT,ICHR,EPOMRK(MXCCHR),EPMRK(255)
      INTEGER*4    NOBS(2,MXCSAT,MXCCHR),NOBSM(2,MXCSAT,MXCCHR)
      CHARACTER*1  STRING(2,MXCSAT,MXCCHR),TEST
      CHARACTER    EPOSTR*255
      CHARACTER*6  MXNSAT,MXNCHR
C
      IF (STRING(IFREQ,ISAT,ICHR-1) .NE. TEST) THEN
        DO 10 I=1,255
          EPMRK(I) = 0
10      CONTINUE
        IF (STRING(IFREQ,ISAT,ICHR-1) .EQ. ' ') THEN
          IF (TEST .EQ. '*') THEN
            IF (NOBS(IFREQ,ISAT,ICHR-1) .EQ. 0) THEN
              EPMRK(ICHR)=EPOMRK(ICHR)-NOBS(IFREQ,ISAT,ICHR)
            ELSE
              EPMRK(ICHR) = EPOMRK(ICHR-1) - NOBS(IFREQ,ISAT,ICHR-1)
            ENDIF
C
            IF (EPMRK(ICHR) .LT. 10) THEN
              IF (EPOSTR(ICHR:ICHR) .EQ. ' ') THEN
                WRITE (EPOSTR(ICHR:ICHR),'(I1)') EPMRK(ICHR)
              ENDIF
            ELSE IF (EPMRK(ICHR) .LT. 100) THEN
              IF (EPOSTR(ICHR:ICHR) .EQ. ' ') THEN
                WRITE (EPOSTR(ICHR:ICHR+1),'(I2)') EPMRK(ICHR)
              ENDIF
            ELSE IF (EPMRK(ICHR) .LT. 1000) THEN
              IF (EPOSTR(ICHR:ICHR) .EQ. ' ') THEN
                WRITE (EPOSTR(ICHR:ICHR+2),'(I3)') EPMRK(ICHR)
              ENDIF
            ELSE IF (EPMRK(ICHR) .LT. 10000) THEN
              IF (EPOSTR(ICHR:ICHR) .EQ. ' ') THEN
                WRITE (EPOSTR(ICHR:ICHR+3),'(I4)') EPMRK(ICHR)
              ENDIF
            ENDIF
C
          ELSE IF (TEST .EQ. '-') THEN
            IF (NOBSM(IFREQ,ISAT,ICHR-1) .EQ. 0) THEN
             EPMRK(ICHR)=EPOMRK(ICHR)-NOBSM(IFREQ,ISAT,ICHR)
            ELSE
              EPMRK(ICHR) = EPOMRK(ICHR-1) - NOBSM(IFREQ,ISAT,ICHR-1)
            ENDIF
C
            IF (EPMRK(ICHR) .LT. 10) THEN
              IF (EPOSTR(ICHR:ICHR) .EQ. ' ') THEN
                WRITE (EPOSTR(ICHR:ICHR),'(I1)') EPMRK(ICHR)
              ENDIF
            ELSE IF (EPMRK(ICHR) .LT. 100) THEN
              IF (EPOSTR(ICHR:ICHR) .EQ. ' ') THEN
                WRITE (EPOSTR(ICHR:ICHR+1),'(I2)') EPMRK(ICHR)
              ENDIF
            ELSE IF (EPMRK(ICHR) .LT. 1000) THEN
              IF (EPOSTR(ICHR:ICHR) .EQ. ' ') THEN
                WRITE (EPOSTR(ICHR:ICHR+2),'(I3)') EPMRK(ICHR)
              ENDIF
            ELSE IF (EPMRK(ICHR) .LT. 10000) THEN
              IF (EPOSTR(ICHR:ICHR) .EQ. ' ') THEN
                WRITE (EPOSTR(ICHR:ICHR+3),'(I4)') EPMRK(ICHR)
              ENDIF
            ENDIF
C
          ENDIF
C
        ELSE IF (STRING(IFREQ,ISAT,ICHR-1) .EQ. '*' .OR.
     1           STRING(IFREQ,ISAT,ICHR-1) .EQ. '-'     ) THEN
          IF (TEST .EQ. '*') THEN
            IF (NOBS(IFREQ,ISAT,ICHR-1) .EQ. 0) THEN
              EPMRK(ICHR) = EPOMRK(ICHR) - NOBS(IFREQ,ISAT,ICHR)
            ELSE
              EPMRK(ICHR) = EPOMRK(ICHR-1) - NOBS(IFREQ,ISAT,ICHR-1)
            ENDIF
          ELSE IF (TEST .EQ. '-') THEN
            IF (NOBSM(IFREQ,ISAT,ICHR-1) .EQ. 0) THEN
              EPMRK(ICHR) = EPOMRK(ICHR) - NOBSM(IFREQ,ISAT,ICHR)
            ELSE
              EPMRK(ICHR) = EPOMRK(ICHR-1) - NOBSM(IFREQ,ISAT,ICHR-1)
            ENDIF
          ELSE IF (TEST .EQ. ' ') THEN
            IF (STRING(IFREQ,ISAT,ICHR-1) .EQ. '*') THEN
              IF (NOBS(IFREQ,ISAT,ICHR-1) .LT. IDOBS) THEN
                EPMRK(ICHR) = EPOMRK(ICHR-1) - (IDOBS - NOBS(IFREQ,
     1                        ISAT,ICHR-1))
              ELSE
                EPMRK(ICHR) = EPOMRK(ICHR-1) + NOBS(IFREQ,ISAT,ICHR)
              ENDIF
            ELSE IF (STRING(IFREQ,ISAT,ICHR-1) .EQ. '-') THEN
              IF (NOBSM(IFREQ,ISAT,ICHR-1) .LT. IDOBS) THEN
                EPMRK(ICHR) = EPOMRK(ICHR-1) - (IDOBS - NOBSM(IFREQ,
     1                        ISAT,ICHR-1))
              ELSE
                EPMRK(ICHR) = EPOMRK(ICHR-1) + NOBSM(IFREQ,ISAT,ICHR)
              ENDIF
            ENDIF
          ENDIF
C
          IF (EPMRK(ICHR) .LT. 10) THEN
            IF (EPOSTR(ICHR:ICHR) .EQ. ' ') THEN
              WRITE (EPOSTR(ICHR:ICHR),'(I1)') EPMRK(ICHR)
            ENDIF
          ELSE IF (EPMRK(ICHR) .LT. 100) THEN
            IF (EPOSTR(ICHR:ICHR) .EQ. ' ') THEN
              WRITE (EPOSTR(ICHR:ICHR+1),'(I2)') EPMRK(ICHR)
            ENDIF
          ELSE IF (EPMRK(ICHR) .LT. 1000) THEN
            IF (EPOSTR(ICHR:ICHR) .EQ. ' ') THEN
              WRITE (EPOSTR(ICHR:ICHR+2),'(I3)') EPMRK(ICHR)
            ENDIF
          ELSE IF (EPMRK(ICHR) .LT. 10000) THEN
            IF (EPOSTR(ICHR:ICHR) .EQ. ' ') THEN
              WRITE (EPOSTR(ICHR:ICHR+3),'(I4)') EPMRK(ICHR)
            ENDIF
          ENDIF
C
        ENDIF
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
