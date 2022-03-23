      MODULE s_PRIWDW
      CONTAINS

C*
      SUBROUTINE PRIWDW(NFTOT,ICAMPN,ICAMP,NSAMPL,ZENMAX,ZMXLEO,
     1                  ISASYS,ISOSEL,EDTLVL,WINDOW)
CC
CC NAME       :  PRIWDW
CC
CC PURPOSE    :  PRINT OBSERVATION SELECTION OPTIONS (SAMPLING RATE,
CC               MINIMUM ELEVATION, AND WINDOWS)
CC
CC PARAMETERS :
CC         IN :  NFTOT  : TOTAL NUMBER OF FILES               I*4
CC               ICAMPN(I),I=1,..,NFTOT: CAMPAIGN NUMBER FOR  I*4
CC                        FILE I
CC               ICAMP  : NUMBER OF CAMPAIGN TO BE PRINTED    I*4
CC               NSAMPL : SAMPLING RATE (SEC)                 I*4(3)
CC                        1: OBSERVATIONS
CC                        2: RESUBSTITUTION OF EPOCH PARAMETERS
CC                        3: PREELIMINATE OF EPOCH PARAMETERS
CC               ZENMAX : MAXIMUM ZENITH DISTANCE FOR SATELL. R*8
CC               ZMXLEO : MAXIMUM ZENITH DISTANCE FOR LEO     R*8
CC               ISASYS : SATELLITE SYSTEM TO BE CONSIDERED   I*4
CC                        =0: ALL
CC                        =1: GPS
CC                        =2: GLONASS
CC                        =3: GALILEO
CC                        =4: SBAS
CC               ISOSEL : SPECIAL OBSERVATION DATA SELECTION  I*4
CC                        = 0: NO
CC                        = 1: NIGHT-TIME
CC                        = 2: NON-ECLIPSING
CC                        = 3: NON-ECLIPSING ONLY FOR GPS BLOCK I, II, IIA
CC               EDTLVL : O-C EDITING LEVEL                   R*4
CC               WINDOW(2,I),I=1,..,NFTOT: OBSERVATION WINDOW R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/18 09:32
CC
CC CHANGES    :  27-MAY-91 : ??: DON'T PRINT TRAILING BLANKS
CC               27-SEP-91 : ??: CHANGE TEXT FOR PARAMETER "NSAMPL"
CC               18-SEP-97 : SS: ALLOW 4-DIGIT SAMPLING RATE
CC               05-MAR-03 : CU: REMOVE USE OF SKELETON FILE
CC               30-APR-03 : SS: SATELLITE SYSTEM SELECTION
CC               19-MAY-03 : RD: USE SR TIMST2 FOR PRINTING THE WINDOW
CC               19-JAN-03 : SS/MM: REVISION OF GPSEST INPUT PANELS
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               08-Aug-05 : HB: Use new SR TIMST2 (module)
CC               03-NOV-06 : RD: NON-ECLIPSING ONLY FOR OLD GPS SAT.
CC               28-FEB-07 : AG: USE PI FROM DEFCON
CC               28-MAR-07 : AG: GALILEO and SBAS added
CC               05-NOV-08 : AJ: LEO ELEVATION CUT-OFF ADDED
CC               28-NOV-08 : RD: PRINT THE CORRECT COMBINATIONS FOR ISASYS
CC               27-MAY-09 : RD: SPECIAL SAMPLING FOR RESUBST. OF EPOCH PARAM.
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE d_const, ONLY: pi
      USE s_timst2
      USE f_lengt1
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 ICAMP , IF    , IFIRST, ISASYS, ISOSEL, NFTOT ,
     1          MINELE, MINLEO
C
      REAL*8    EDTLVL, ZENMAX, ZMXLEO
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*132 TEXT
      CHARACTER*41  SELCHR(4)
      CHARACTER*7   SYSCHR(7)
      REAL*8        WINDOW(2,*)
      INTEGER*4     ICAMPN(*),NSAMPL(3)
      DATA SYSCHR /'ALL    ','GPS    ','GLONASS','GALILEO',
     1             'GPS/GLO','GPS/GAL','GLO/GAL'/
      DATA SELCHR /'NO                                       ',
     1             'NIGHT-TIME                               ',
     2             'NON-ECLIPSING                            ',
     3             'NON-ECLIPSING ONLY FOR GPS BLOCK I,II,IIA'/
C
C PRINT TITLE LINES
C -----------------
      WRITE(LFNPRT,"(
     1     ' '
     2  ,/,' '
     3  ,/,' OBSERVATION SELECTION:'
     4  ,/,' ---------------------'
     5  ,/,1X)")
C
      MINELE=IDNINT(90.D0-ZENMAX*180D0/PI)
      MINLEO=IDNINT(90.D0-ZMXLEO*180D0/PI)
      WRITE(LFNPRT,2) NSAMPL(1:3),MINELE,MINLEO,SYSCHR(ISASYS+1),
     1                TRIM(SELCHR(ISOSEL+1))
2     FORMAT(
     1' SAMPLING RATE FOR OBSERVATIONS                :',I5,' SEC',/,
     2' SAMPLING RATE TO RESUBSTITUTE EPOCH PARAMETERS:',I5,' SEC',/,
     3' SAMPLING RATE TO PREELIMINATE EPOCH PARAMETERS:',I5,' SEC',/,
     4' ELEVATION CUT-OFF ANGLE                       :',I5,
     5     ' DEG (STATIONS) ',I5,' DEG (LEOS)',/,
     6' SATELLITE SYSTEM                              :',2X,A,/,
     7' SPECIAL DATA SELECTION                        :',2X,A)
C
      IF (EDTLVL.NE.0.D0) WRITE(LFNPRT,"(' O-C EDITING LEVEL       : ',
     1  F8.3,' METERS')") EDTLVL
C
      IFIRST=0
      DO 30 IF=1,NFTOT
        IF(ICAMPN(IF).NE.ICAMP) GOTO 30
        IF(WINDOW(1,IF).EQ.0.D0.AND.WINDOW(2,IF).EQ.1.D20) GOTO 30
        IF(IFIRST.EQ.0) THEN
          IFIRST=1
C
        WRITE(LFNPRT,"(
     1       ' '
     2    ,/,' FILE         WINDOW START              WINDOW END'
     3    ,/,1X,131('-')
     4    ,/,1X)")
C
        ENDIF
        WRITE(TEXT,3) IF
3       FORMAT(I5)
        CALL TIMST2(1,1,WINDOW(1,IF),TEXT(12:30))
        CALL TIMST2(1,1,WINDOW(2,IF),TEXT(37:55))
        WRITE(LFNPRT,1) TEXT(1:LENGT1(TEXT))
1       FORMAT(A)
30    CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
