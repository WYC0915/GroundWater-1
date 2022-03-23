      MODULE s_DSPRIF
      CONTAINS

C*
      SUBROUTINE DSPRIF(GENUPD,NFIL,TABFIL,NARC,NUNB,NPOTMX,
     1                  DTPI,NITER,Q,ARCFIL,TFL,INDTIM,IANTOF,ORBMOD)
CC
CC NAME       :  DSPRIF
CC
CC PURPOSE    :  PRINT FILENAMES AND OPTIONS USED IN "DEFSTD"
CC
CC PARAMETERS :
CC         IN :  GENUPD : GENERATE-MODE=1, UPDATE MODE=2        I*4
CC               NFIL   : NUMBER OF TABULAR ORBIT FILES         I*4
CC               TABFIL(K,I),I=1,NFIL: TABULAR ORBIT FILENAMES CH*32
CC                        NAMES FOR K=1
CC               NARC   : NUMBER OF ARCS TO BE PROCESSED        I*4
CC               NUNB   : NUMBER OF UNKNOWNS IN ORBIT DETERM.   I*4
CC                        NUNB=6: SIX OSCULATING ELEMENTS ARE EST.
CC                        NUNB=7: ELEMENTS PLUS DIRECT RAD.PRESS. PAR.
CC                        NUNB=8: "  + Y-BIAS
CC                        NUNB=9: "     "      + NO-NAME BIAS
CC               NPOTMX : MAXIMUM DEGREE/ORDER FOR EARTH POT.   I*4
CC               DTPI   : LENGTH FOR PARTIAL INTERVAL FOR       R*8
CC                        INTEGRATION
CC               NITER  : NUMBER OF ITERATIONS IN ORBIT DETERM. I*4
CC               Q      : DEGREE OF THE APPROXIMATING POLYNOM.  I*4
CC               ARCFIL(I),I=1,NARC T-FILE NUMBER FOR ARC I     I*4
CC               TFL(K,I),K=1,2, I=1,2,..,NARC: TIRST/LAST RE-  R*8
CC                       QUESTED TIME FOR ARC I
CC               INDTIM: TIME DEFINITION FOR TAB. ORBIT FILES   I*4
CC                       =1: TIME IN GPS TIME
CC                       =2: TIME IN UTC
CC               IANTOF : CORRECTION FOR ANTENNA OFFSETS        I*4
CC                        =0: TABULAR FILE POSITIONS WITH
CC                            RESPECT TO CENTER OF MASS (NO
CC                            OFFSET CORRECTION)
CC                        =1: ANTENNA OFFSET CORRECTION
CC               ORBMOD : ORBIT MODEL NUMBER (1,2,3...)         I*4
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.4
CC
CC CREATED    :  09-JUL-92
CC
CC CHANGES    :  27-JUL-92 : ??: ADD PARAMETER "NCOL" TO CALL PRFLNA
CC               10-SEP-92 : ??: CHANGED CALL OF "PRFLNA"
CC               13-OCT-92 : ??: NEW OPTION "IANTOF"
CC               12-FEB-93 : ??: ADD MISSING TEST FOR SATELLITE INFO FILE
CC               25-JAN-96 : GB: NEW ORBIT MODEL IMPLEMENTED
CC               26-SEP-96 : TS: PRINT ORBIT MODEL NUMBER
CC               03-OCT-96 : TS: CORRECT TABFIL USAGE
CC               07-NOV-00 : CU: SWITCH TO NEW MENU SYSTEM
CC               13-NOV-00 : RD: WRITE ALSO .PRE & .ERP FILES
CC               18-DEC-00 : HU: USE INTERFACE FOR PRFLNA
CC               25-SEP-02 : HU: REMOVE I_ASTLIB
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               01-SEP-08 : DT: OUTPUT FORMAT FOR INTERVAL F10.2->F10.5
CC               10-AUG-10 : RD: USE TIMST2 INSTEAD OF TIMSTR
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1992     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE d_const, ONLY: DATE, P0, TIME
      USE s_prflna
      USE s_timst2
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IANTOF, IARC  , IFIL  , II    , INDTIM, IRCSAT, JFIL  ,
     1          KFIL  , NARC  , NFIL  , NITER , NPOTMX, NUNB
C
      REAL*8    DTPI
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*40  TSTRNG
      CHARACTER*32  TABFIL(2,*),FILSAT
      CHARACTER*3   REFTIM(2),YESNO1,YESNO2
C
      REAL*8        TFL(2,*)
C
      INTEGER*4     Q,ARCFIL(*),GENUPD,ORBMOD
C
C
      DATA REFTIM/'GPS','UTC'/
C
C WRITE TITLE LINES
C -----------------
      WRITE(LFNPRT,1001) DATE,TIME
1001  FORMAT(/,' ',79('*'),/,' COMPUTATION OF BERNESE STANDARD ',
     1         'ORBITS FROM TABULAR POSITIONS   ',A9,1X,A5,/,
     2         ' ',79('*'),/)

C
C WRITE INPUT AND OUTPUT FILENAMES
C --------------------------------
       CALL prflna
C
C TABULAR ORBIT FILENAMES
C -----------------------
      IF(GENUPD.EQ.1)THEN
        WRITE(LFNPRT,1006)
1006    FORMAT(' ',79('-'),/,' LIST OF TABULAR FILES',/,' ',79('-'),//,
     1         ' FILE  TABULAR ORBIT FILENAME',/,' ----',2X,32('-'),/)
        DO 10 IFIL=1,NFIL
C
C BECAUSE 1/2 COL. PROBLEM SOME INDEX ARE NECESSARY
C -------------------------------------------------
          JFIL=MOD((IFIL-1), 2) +1
          KFIL=INT((IFIL-1)/2)+1
          WRITE(LFNPRT,1007) IFIL,TABFIL(JFIL,KFIL)
1007      FORMAT(I5,2X,A)
10      CONTINUE
        WRITE(LFNPRT,1008)
1008    FORMAT(/,' ',79('-'),//)
      END IF
C
C PRECISE ORBIT FILENAMES
C -----------------------
      IF(GENUPD.EQ.2)THEN
        WRITE(LFNPRT,1016)
1016    FORMAT(' ',79('-'),/,' LIST OF PRECISE FILES',/,' ',79('-'),//,
     1         ' FILE  PRECISE ORBIT FILENAME',/,' ----',2X,32('-'),/)
        DO 15 IFIL=1,NFIL
          WRITE(LFNPRT,1017) IFIL,(TABFIL(II,IFIL)(1:32),II=1,2)
1017      FORMAT(I5,2X,A32,2X,A32)
15      CONTINUE
        WRITE(LFNPRT,1018)
1018    FORMAT(/,' ',79('-'),//)
      END IF
C
C OPTIONS
C -------
      CALL GTFLNA(0,'SATELL ',FILSAT,IRCSAT)
      IF (IRCSAT.EQ.0) THEN
        YESNO1='YES'
      ELSE
        YESNO1=' NO'
      ENDIF
      IF (IANTOF.EQ.0) THEN
        YESNO2=' NO'
      ELSE
        YESNO2='YES'
      ENDIF
C
      WRITE(LFNPRT,1009)
1009  FORMAT(' ',79('-'),/,' OPTIONS',/,' ',79('-'),/)
      WRITE(LFNPRT,1010) REFTIM(INDTIM),NPOTMX,NITER,Q,DTPI,P0*1.D7,
     1                   YESNO1,YESNO2,ORBMOD
1010  FORMAT(' TIME REFERENCE SYSTEM                      :       ',
     1       A3,/,
     2       ' MAX. ORDER/DEGREE OF EARTH POTENTIAL COEFF.:',I10,/,
     3       ' NUMBER OF ITERATIONS                       :',I10,/,
     4       ' POLYNOMIAL DEGREE FOR NUMERICAL INTEGRATION:',I10,/,
     5       ' LENGTH OF NUMERICAL INTEGRATION INTERVALS  :',F10.5,
     6       '  HOURS',/,
     7       ' A PRIORI RADIATION PRESSURE COEFFICIENT    :',F10.5,
     8       '  M/S**2*1.D7',/,
     9       ' ROCK4 / ROCK42 MODEL USED                  :       ',
     1       A3,/,
     2       ' REDUCE TABULAR POSITIONS TO CENTER OF MASS :       ',
     3       A3,/,
     4       ' ORBIT MODEL NUMBER                         :',I10,/)
      WRITE(LFNPRT,1008)
C
C ARC DEFINITIONS
C ---------------
      WRITE(LFNPRT,1011)
1011  FORMAT(' ',79('-'),/,' ARC DEFINITIONS',/,' ',79('-'),//,
     1       ' ARC  START TIME',11X,'END TIME',13X,'FIRST TAB.FILE',
     2       '   # PARAM.',/,
     3       ' ---  ',2(19('-'),2X),5X,'---',12X,'--',/)
      DO 20 IARC=1,NARC
        CALL TIMST2(1,2,TFL(1:2,IARC),TSTRNG)
        WRITE(LFNPRT,1012) IARC,TSTRNG,ARCFIL(IARC),NUNB
1012    FORMAT(I4,2X,A40,I10,I14)
20    CONTINUE
      WRITE(LFNPRT,1008)
C
      RETURN
      END SUBROUTINE

      END MODULE
