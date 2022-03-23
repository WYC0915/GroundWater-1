      MODULE s_POSMET
      CONTAINS

C*
      SUBROUTINE POSMET(LFN,TMET,UTLOC,NMETEO,METVAL)
CC
CC NAME       :  POSMET
CC
CC PURPOSE    :  POSITION METEO FILE TO THE TWO RECORDS CLOSEST
CC               TO THE EPOCH TMET
CC
CC PARAMETERS :
CC         IN :  LFN    : LOGICAL FILE NUMBER WITH METEO      I*4
CC               TMET   : EPOCH OF METEO REQUEST              R*8
CC               UTLOC  : CORRECTION TO UTC (HOURS)           R*8
CC               NMETEO : NUMBER OF TROPOSPHERE VALUES (INCL. I*4
CC                        TIME ARGUMENT)
CC        OUT :  METVAL(K,I),I=1,2,..,NMETEO, K=1,2           R*8
CC                        TIME, PRESSURE, TEMP. AND HUMIDITY
CC                        FOR TWO CURRENT RECORDS
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, L.MERVART, M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/09/28 17:07
CC
CC CHANGES    :  29-MAY-94 : MR: ARBITRARY NUMBER OF VALUES IN MET.FILE
CC               31-DEC-96 : WG: TEST FOR 2000 WAS MISSING
CC               01-JUL-99 : PF: CALL IYEAR4 FOR CONVERSION YY->YYYY
CC               27-MAR-00 : HU: Y2K AND EOF PROBLEM SOLVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE f_djul
      USE f_iyear4
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IDD   , IHH   , IOSTAT, ISS   , JJ    , K     ,
     1          LFN   , MM    , MO    , NMETEO
C
      REAL*8    DD    , TMET
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      INTEGER*4 UTLOC
      REAL*8    METVAL(2,*)
C
C RETURN, IF REQUESTED TIME IS BEFORE THE SECOND CURRENT FILE EPOCH
100   CONTINUE
      IF (TMET.LE.METVAL(2,1)) RETURN
C
C COPY SECOND VALUE TO FIRST VALUE
      DO 110 K=1,NMETEO
        METVAL(1,K)=METVAL(2,K)
110   CONTINUE
C
C READ NEXT VALUE
      READ (LFN,*,IOSTAT=IOSTAT) JJ,MO,IDD,IHH,MM,ISS,
     1                     (METVAL(2,K),K=2,NMETEO)
      IF(IOSTAT.NE.0)GOTO 999
C
C COMPUTE NEW EPOCH IN MJD
      IF (JJ.GT.0.OR.MO.GT.0) THEN
        JJ = IYEAR4(JJ)
        DD=IDD+IHH/24.D0+MM/1440.D0+ISS/86400.D0
        METVAL(2,1)=DJUL(JJ,MO,DD)+UTLOC/24.D0
        GOTO 100
      END IF
C
999   RETURN
      END SUBROUTINE

      END MODULE
