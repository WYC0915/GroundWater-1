      MODULE s_R2RDNR
      CONTAINS

C*
      SUBROUTINE R2RDNR(LFNNAV,LFNERR,IRXVRS,ISVN,EPHDAT,IRCODE)
CC
CC NAME       :  R2RDNR
CC
CC PURPOSE    :  READ  OBSERVATION RECORDS OF A
CC               RINEX NAVIGATION MESSAGE FILE
CC
CC PARAMETERS :
CC         IN :  LFNNAV : LOGICAL FILE NUMBER                  I*4
CC               LFNERR : LFN FOR ERROR MESSAGES               I*4
CC               IRXVRS : RINEX VERSION NUMBER                 I*4
CC        OUT :  ISVN   : PRN OF SATELLITE                     I*4
CC               EPHDAT : VECTOR WITH MESSAGE DATA             R*8(*)
CC                        EPHDAT(1): TOC
CC                          (2)-(4): A0,A1,A2
CC                              (5): AODE
CC                              (6): CRS
CC                                 .
CC                             (13): TOE
CC                                 .
CC                             (28): AODC
CC                             (29): TRANSM.TIME OF MESSAGE
CC                                 .
CC                             (32): SPARE
CC                                   (29-32: VERSION 2 ONLY)
CC               IRCODE : RETURN CODE                          I*4
CC                        0: OK
CC                        3: END OF FILE WITHIN OBS.RECORD
CC                        4: ERROR DECODING DATA
CC                        5: START OF NEW HEADER FOUND
CC                        9: END OF FILE
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  W. GURTNER
CC
CC CREATED    :  01-MAY-91
CC
CC CHANGES    :  17-JUN-94 : ??: SKIP RECORDS CONTAINING ASCII NUL CHARACTERS
CC               01-JUL-99 : PF: CALL IYEAR4 FOR CONVERSION YY->YYYY
CC               04-JAN-02 : HU: REMOVE ^M
CC               01-MAR-02 : DI: ADD ERR= TO ALL READ STATEMENTS
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               24-MAY-07 : AG: FUNCTION PRN2PRN ADDED
CC               29-JUL-10 : AS: EPHDAT definition: EPHDAT(*)->EPHDAT(:)
CC               09-AUG-10 : AS: RINEX 3 support added
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1991     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE s_mjdgps
      USE f_djul
      USE f_iyear4
      USE f_prn2prn
      USE s_chr2svn
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I2    , IDAY  , IHOUR , IRCODE, IRXVRS, ISVN  ,
     1          ITEST , IYEAR , K     , LFNERR, LFNNAV, MINUTE,
     2          MONTH , NWEEK , ISVN2 , ISEC
C
      REAL*8    DAY   , SEC   , TOC
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C GLOBAL DECLARATIONS
C -------------------
      REAL*8       EPHDAT(:)
C
C  LOCAL DECLARATIONS
C  ------------------
      CHARACTER    STRING*80
      CHARACTER(LEN=1)               :: cSvn

C
C RECORD 1
200   READ(LFNNAV,222,END=990,ERR=940) STRING
222   FORMAT(A80)
      IF(STRING(1:1).EQ.CHAR(0)) GOTO 200
      IF(STRING.EQ.' ') GOTO 200
      IF(STRING(61:80).EQ.'RINEX VERSION / TYPE') GOTO 950
      IF(IRXVRS.LT.3) THEN
        READ(STRING,1,ERR=940) ISVN,IYEAR,MONTH,IDAY,IHOUR,MINUTE,SEC,
     1                         (EPHDAT(K),K=2,4)
1       FORMAT(I2,5I3,F5.1,3D19.12)
        IYEAR = IYEAR4(IYEAR)
      ELSEIF(IRXVRS.EQ.3) THEN
        READ(STRING,10,ERR=940) cSvn,iSvn2,IYEAR,MONTH,IDAY,IHOUR,
     1                          MINUTE,iSec,(EPHDAT(K),K=2,4)
10      FORMAT(A1,I2,I5,5I3,3D19.12)
        CALL chr2svn(iSvn2,cSvn,iSvn)
        SEC = DBLE(ISEC)
      ELSE
        WRITE(lfnErr,'(/,16X,A,/)') 'SR R2RDNR: Unknown RINEX format'
        GOTO 999
      ENDIF
      DAY=IDAY+IHOUR/24.D0+MINUTE/1440.D0+SEC/86400.D0
      TOC=DJUL(IYEAR,MONTH,DAY)
      CALL MJDGPS(TOC,EPHDAT(1),NWEEK)
C RENAME SAT IF NECESSARY
      ISVN=PRN2PRN(ISVN,TOC)
C
C RECORDS 2-7 (8)
      IF(IRXVRS.EQ.1) THEN
        I2=25
      ELSE
        I2=29
      ENDIF
      DO 20 I=5,I2,4
        READ(LFNNAV,222,END=930,ERR=940) STRING
C REMOVE ^M
        ITEST=INDEX(STRING,CHAR(13))
        IF(ITEST.GT.0)STRING(ITEST:ITEST)=' '
C
        READ(STRING,2,ERR=940) (EPHDAT(K),K=I,I+3)
2       FORMAT(3X,4D19.12)
20    CONTINUE
C
      IRCODE=0
      GOTO 999
C
C  END OF FILE WITHIN OBS.RECORD
930   IRCODE=3
      WRITE(LFNERR,931)
931   FORMAT(' SR R2RDNR: END OF FILE WITHIN OBS.RECORD')
      GOTO 999
C
C  ERROR DECODING DATA
940   IRCODE=4
      WRITE(LFNERR,941) STRING(1:79)
941   FORMAT(' SR R2RDNR: ERROR DECODING DATA ON THE FOLLOWING LINE:',
     1       /,1X,A)
      GOTO 999
C
C  START OF NEW HEADER FOUND
950   IRCODE=5
      BACKSPACE LFNNAV
      WRITE(LFNERR,951) STRING(1:79)
951   FORMAT(' SR R2RDNR: START OF A NEW HEADER FOUND:',
     1       /,1X,A)
      GOTO 999
C
C  END OF FILE
990   IRCODE=9
      GOTO 999
C
999   RETURN
C
      END SUBROUTINE

      END MODULE
