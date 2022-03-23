      MODULE s_R2WTCR
      CONTAINS

C*
      SUBROUTINE R2WTCR(LFNCLK,LFNERR,MAXSTA,MAXSAT,TSTART,
     1    NRSTA ,STANAM,NRSAT ,SATNUM,NREPO ,EPOCH ,
     2    STACLK,STASIG,SATCLK,SATSIG,IRCODE)
CC
CC NAME       :  R2WTCR
CC
CC PURPOSE    :  WRITE RECORDS OF A RINEX CLOCK FILE (VERSION 2.00)
CC
CC PARAMETERS :
CC         IN :  LFNCLK : LOGICAL FILE NUMBER                          I*4
CC            :  LFNERR : LFN FOR ERROR MESSAGES                       I*4
CC            :  MAXSTA : MAXIMAL DIMENSION FOR ARRAYS                 I*4
CC            :  MAXSAT : MAXIMAL DIMENSION FOR ARRAYS                 I*4
CC            :  TSTART : START DAY OF OBSERVATION (MJD)               R*8
CC            :  NRSTA  : NUMBER OF STATIONS (FROM HEADER)             I*4
CC            :  STANAM : STATION NAMES (FROM HEADER)                 CH*4(*)
CC            :  NRSAT  : NUMBER OF SATELLITES (FROM HEADER)           I*4
CC            :  SATNUM : SATELLITE NUMBERS (FROM HEADER)              I*4(*)
CC            :  EPOCH  : CLOCK EPOCHS (GPS TIME) (sec)                R*8(*)
CC            :  STACLK : STATION CLOCK ESTIMATES (microsec)           R*8(*,*)
CC            :  STASIG : STATION CLOCK SIGMAS    (microsec)           R*8(*,*)
CC            :           STACLK(I,J), I: STATION, J: EPOCH
CC            :  SATCLK : SATELLITE CLOCK ESTIMATES (microsec)         R*8(*,*)
CC            :  SATSIG : SATELLITE CLOCK SIGMAS    (microsec)         R*8(*,*)
CC            :           SATCLK(I,J), I: STATION, J: EPOCH
CC        OUT :  IRCODE : RETURN CODE                                  I*4
CC            :           0: OK
CC
CC REMARKS    :  --
CC
CC AUTHOR     :  T.A. SPRINGER
CC
CC VERSION    :  1.0
CC
CC CREATED    :  06-DEC-99
CC
CC CHANGES    :  06-DEC-99 : TS: CREATION
CC               22-FEB-00 : RD: WORKS FOR MORE THAN TWO DAYS
CC               03-APR-01 : RD: THEEPO MAY BE NEGATIVE...
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1999     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE s_jmt
      USE s_radgms
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , ID    , IEPO  , IH    , IM    , IRCODE, ISAT  ,
     1          ISTA  , IYYY  , LFNCLK, LFNERR, MAXSAT, MAXSTA, MM    ,
     2          NREPO , NRSAT , NRSTA
C
      REAL*8    DAY   , SEC   , THEDAY, THEEPO, TIME  , TSTART
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C DECLARATIONS
C ------------
      CHARACTER*4  STANAM(MAXSTA),CHR4
      CHARACTER*1  SIGN
C
      INTEGER*4    SATNUM(MAXSAT)
C
      REAL*8       EPOCH(*)
      REAL*8       STACLK(MAXSTA,*),SATCLK(MAXSAT,*)
      REAL*8       STASIG(MAXSTA,*),SATSIG(MAXSAT,*)
C
C LOOP OVER ALL EPOCHS
C --------------------
      DO IEPO=1,NREPO
        CALL JMT(TSTART,IYYY,MM,DAY)
        CALL RADGMS(3,EPOCH(IEPO)/86400D0,SIGN,IH,IM,SEC)
        THEEPO=EPOCH(IEPO)
        THEDAY=TSTART
        DO 10 I=1,1000000
          IF (THEEPO.GT.-0.5D0 .AND. THEEPO.LT.86400.5D0) GOTO 11
          IF (THEEPO.GT.86400D0) THEN
            CALL JMT(THEDAY+1D0,IYYY,MM,DAY)
            TIME=THEEPO/86400D0-1D0
            CALL RADGMS(3,TIME,SIGN,IH,IM,SEC)
            THEEPO=THEEPO-86400D0
            THEDAY=THEDAY+1D0
          ENDIF
          IF (THEEPO.LT.0D0) THEN
            CALL JMT(THEDAY-1D0,IYYY,MM,DAY)
            TIME=THEEPO/86400D0+1D0
            CALL RADGMS(3,TIME,SIGN,IH,IM,SEC)
            THEEPO=THEEPO+86400D0
            THEDAY=THEDAY-1D0
          ENDIF
10      CONTINUE
11      ID=IDINT(DAY+1/86400D0)
C
C WRITE ALL STATION CLOCKS FOR THIS EPOCH
C ---------------------------------------
        DO ISTA=1,NRSTA
          IF (STACLK(ISTA,IEPO).LT.999999.999999D0) THEN
            WRITE(LFNCLK,51)'AR',STANAM(ISTA),IYYY,MM,ID,IH,IM,SEC,
     1        2,STACLK(ISTA,IEPO)*1D-6,STASIG(ISTA,IEPO)*1D-6
51          FORMAT(A2,1X,A4,1X,I4,4(1X,I2.2),F10.6,I3,2X,2(1X,E19.12))
          ENDIF
        ENDDO
C
C WRITE ALL SATELLITE CLOCKS FOR THIS EPOCH
C -----------------------------------------
        DO ISAT=1,NRSAT
          IF (SATCLK(ISAT,IEPO).LT.999999.999999D0) THEN
            IF (SATNUM(ISAT).GT.100) THEN
              WRITE(CHR4,'(A1,I2.2,1X)')'R',SATNUM(ISAT)-100
            ELSE
              WRITE(CHR4,'(A1,I2.2,1X)')'G',SATNUM(ISAT)
            ENDIF
            WRITE(LFNCLK,51)'AS',CHR4,IYYY,MM,ID,IH,IM,SEC,
     1        2,SATCLK(ISAT,IEPO)*1D-6,SATSIG(ISAT,IEPO)*1D-6
          ENDIF
        ENDDO
C
C NEXT EPOCH
C ----------
      ENDDO
C
      RETURN
      END SUBROUTINE

      END MODULE
