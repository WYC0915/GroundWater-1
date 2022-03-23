      MODULE s_PRIPAR
      CONTAINS

C*
      SUBROUTINE PRIPAR(MAXTYP,PRIOPT,TITLES,NPAR  ,LOCQ  ,
     1                  STNAME,OPLOAD,TIMISB)
CC
CC NAME       :  PRIPAR
CC
CC PURPOSE    :  PRINT PARAMETER CHARACTERIZATION LIST
CC
CC PARAMETERS :
CC         IN :  MAXTYP: MAX. NUMBER OF PARAMETER TYPES       I*4
CC               PRIOPT : FLAG, IF PARAMETER LIST IS PRINTED  I*4
CC                         =0 : NO PRINT
CC                         =1 : PRINT
CC               TITLES(I),I=1,2: TITLE LINES                 CH*132
CC               NPAR   : TOTAL NUMBER OF PARAMETERS          I*4
CC               LOCQ(K,I),K=1,..,MAXLCQ, I=1,..,NPAR: PARA-  I*4
CC                        METER CHARACTERIZATION ARRAY
CC               STNAME(I),I=1,..,NSTAT: STATION NAMES FOR    CH*16
CC                        ALL STATIONS
CC               OPLOAD : SCALING FACTORS FOR VIENNA GRID FILES   T_OPTLOAD(3)
CC                        1: ATMOSPHERIC NON-TIDAL LOADING
CC                        2: OCEAN NON-TIDAL LOADING
CC                        3: HYDROSTATIC PRESSURE LOADING
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC CREATED    :  87/11/11 14:36
CC
CC CHANGES    :  27-MAY-91 : ??: DON'T PRINT TRAILING BLANKS
CC               13-FEB-92 : ??: CHANGES DUE TO ERP-ESTIMATION
CC               06-JUL-92 : ??: PRINT COEFF NUMBER OF ERP'S
CC               22-MAR-93 : ??: STOCHASTIC ORBIT PARAMETERS
CC               03-APR-93 : ??: SATELLITE ANTENNA OFFSET PARAMETERS
CC               28-DEC-93 : MR: TIME WINDOWS FOR SAT.ANT. OFFSETS
CC               14-APR-94 : SS: DIFFERENTIAL IONOSPHERE PARAMETERS
CC               19-APR-94 : RW: CPO-MODEL PARAMETERS
CC               25-JUL-94 : MR: PRE-ELIMINATION STATISTICS SIMPLIFIED
CC               06-NOV-94 : MR: ANTENNA PHASE CENTER PARAMETERS
CC               06-JUN-95 : SS: GLOBAL IONOSPHERE MODEL PARAMETERS
CC               05-DEC-95 : SS: NEW IONOSPHERE MODEL (TYPE 2)
CC               25-JAN-96 : GB: NEW ORBIT MODEL IMPLEMENTED
CC               26-MAR-96 : MR: RECEIVER ANTENNA OFFSETS
CC               08-APR-97 : SS: NIELL MAPPING, TROPOSPHERE GRADIENTS
CC               14-AUG-97 : SS: DIFFERENTIAL CODE BIASES
CC               29-APR-98 : SS: DTEC LC
CC               14-APR-00 : SS: ESTIMATE (P1-C1) CODE BIASES
CC               30-JAN-02 : RD: ADD PARAMETER TYPES FOR EPOCHWISE CLOCKS
CC               07-MAY-02 : SS: DCB UPDATE
CC               13-NOV-02 : RS: SATELLITE ANTENNA PHASE CENTER VARIATIONS
CC               05-MAR-03 : CU: REMOVE USE OF SKELETON FILE
CC               08-MAR-03 : HU: MAXTXT EXTENDED FROM 24 TO 26
CC               24-APR-03 : RS: CORRECT SKELETON FILE SUBSTITUTES
CC               19-NOV-03 : RS: PRINT GNROFF, PRINT CORRECT #AZI
CC               21-JUN-05 : MM: COMLFNUM.INC REMOVED, M_BERN ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               23-JUN-07 : AG: DCBSYS extended with "E"
CC               04-AUG-08 : DT: SYSTEM OF DYNAMIC ORBIT PARAMETERS
CC                               (DYX, RSW, DRSW)
CC               28-OCT-08 : DT: USE MAXVAR FROM M_MAXDIM
CC               12-NOV-08 : DT: TAKE ORBIT DESCRIPTION FROM ORBDSC%ORBMOD
CC               12-NOV-08 : DT: RANGE BIASES AS TYPE=26
CC               04-MAY-09 : RD: SCALING OF LOADING MODELS ADDED
CC               09-MAY-09 : RD: SAT/FRQ-SPECIFIC RECEIVER CLOCK BIASES
CC               09-MAY-09 : RD: SEPERATE RECEIVER CLOCKS FOR GPS/GLONASS
CC               04-JAN-10 : SL: HOI SCALING PARAMETERS ADDED
CC               27-AUG-10 : HB: MAXVAR REPLACED BY MAXELE
CC               08-SEP-10 : RD: MERGE SLR-TIME BIAS OPTION
CC               25-NOV-10 : MM: GNSS-SPECIFIC PARAMETERS
CC               20-DEC-10 : DT: ADD LOCQ(1)=28
CC               12-MAR-12 : HB: LAYOUT UNIFICATION FOR STOCHASTIC
CC                               ORBIT PARAMETERS
CC               06-JUN-12 : LP: REPLACE DCBSYS by g_svnsys
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE m_global, ONLY: g_strsys,g_svnsys
      USE p_orbgen, ONLY: orbdsc
      USE s_maxtst
      USE s_exitrc
      USE s_timst2
      USE f_lengt1
      USE p_gpsest, ONLY: t_optLoad
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IFIRST, IFRC  , IFRTIT, IGRP  , II    , IPAR  ,
     1          IRCMAX, IREQ  , ITYP  , ITYPE , JTYP  , MAXTXT, MAXTYP,
     2          MAXELE, MXCLCQ, NPAR
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER(MAXTXT=30,MAXELE=15)
C
      TYPE(t_optLoad), DIMENSION(:):: opLoad
C
      CHARACTER*132 TITLES(2)
      CHARACTER*60  TXTSTRG(MAXTXT,4)
      CHARACTER*19  EPOSTR
      CHARACTER*16  STNAME(*),STHELP,FRCTXT(6)
      CHARACTER*1   COOTYP(3),HILCMP(3),CO2TYP(3),GSPTYP(4)
      CHARACTER*3   COSSIN(2),HILTYP(3),GIMTXT(3),DCBTXT(2)
      CHARACTER*5   ELETYP(MAXELE), RSWSYS(MAXELE-6)
      CHARACTER*8   TYPTXT(3)
      CHARACTER*7   POLTYP(5)
      CHARACTER*6   MXNLCQ
      CHARACTER*5   DCBTYP(3)
      CHARACTER*3   GRDTYP(3,3)
      CHARACTER*1   CHR1
      CHARACTER*4   empiri

      INTEGER*4     PRIOPT,LOCQ(MXCLCQ,*)

      REAL*8        TIMISB(3,*)
C
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
C
      DATA COOTYP/'X','Y','Z'/
      DATA CO2TYP/'N','E','U'/
      DATA GSPTYP/'X','Y','Z','T'/
      DATA ELETYP/'A','E','I','NODE','PER','U0','D0','Y0','X0',
     1                                          'DC','YC','XC',
     2                                          'DS','YS','XS'/

      DATA RSWSYS/'R0','S0','W0', 'RC','SC','WC', 'RS','SS','WS'/

      DATA POLTYP/'X-POLE','Y-POLE','UT1-UTC','DEPS','DPSI'/
      DATA FRCTXT/'radial          ','along-track     ',
     1            'out-of-plane    ',
     2            'Direction to Sun','y-direction     ',
     3            'x-direction     '/
      DATA TYPTXT/'epoch   ','constant','linear  '/
      DATA COSSIN/'COS','SIN'/
      DATA HILTYP/'CON','COS','SIN'/
      DATA HILCMP/'R','S','W'/
      DATA GIMTXT/'COE','HGT','VAR'/
      DATA DCBTXT/'SAT','REC'/
      DATA DCBTYP/'P1-P2','P1-C1','LC'/
      DATA (GRDTYP(1,II),II=1,3)/'ALL','   ','   '/
      DATA (GRDTYP(2,II),II=1,3)/'UP ','N/E','   '/
      DATA (GRDTYP(3,II),II=1,3)/'U  ','N  ','E  '/
C
      DATA (TXTSTRG(1,II),II=1,4)/
     1   ' STATION COORDINATES:                                       ',
     2   ' -------------------                                        ',
     3   '                                                            ',
     4   ' PARAM  TYPE  STATION NAME     COORDINATE                   '/
      DATA (TXTSTRG(2,II),II=1,4)/
     1   ' RECEIVER CLOCKS / TIME BIASES:                             ',
     2   ' -----------------------------                              ',
     3   '                                                            ',
     4   ' PARAM  TYPE  REQUEST  STATION NAME                         '/
      DATA (TXTSTRG(3,II),II=1,4)/
     1   ' ORBITAL ELEMENTS:                                          ',
     2   ' ----------------                                           ',
     3   '                                                            ',
     4   ' PARAM  TYPE  ARC  SAT.  ELEMENT                            '/
      DATA (TXTSTRG(4,II),II=1,4)/
     1   ' AMBIGUITIES:                                               ',
     2   ' -----------                                                ',
     3   '                                                            ',
     4   ' PARAM  TYPE  FILE  CLUSTER  AMB.FRQ.  FREQ.  L5-CLUSTER    '/
      DATA (TXTSTRG(5,II),II=1,4)/
     1   ' RECEIVER ANTENNA OFFSETS:                                  ',
     2   ' ------------------------                                   ',
     3   '                                                            ',
     4   ' PARAM  TYPE  REQUEST  FREQ.  COORDINATE                    '/
      DATA (TXTSTRG(6,II),II=1,4)/
     1   ' SITE-SPECIFIC TROPOSPHERE PARAMETERS:                      ',
     2   ' ------------------------------------                       ',
     3   '                                                            ',
     4   ' PARAM  TYPE  REQUEST  STATION NAME     COMPONENT           '/
      DATA (TXTSTRG(7,II),II=1,4)/
     1   ' LOCAL IONOSPHERE MODELS:                                   ',
     2   ' -----------------------                                    ',
     3   '                     POL.DEG. IN                            ',
     4   ' PARAM  TYPE  MODEL  TIME  LATIT.                           '/
      DATA (TXTSTRG(8,II),II=1,4)/
     1   ' DIFFERENTIAL CODE BIASES:                                  ',
     2   ' ------------------------                                   ',
     3   '                                                            ',
     4   ' PARAM  TYPE  GROUP  SVN / STATION NAME  SYSTEM  TYPE       '/
      DATA (TXTSTRG(9,II),II=1,4)/
     1   ' LOCAL TROPOSPHERE MODELS:                                  ',
     2   ' ------------------------                                   ',
     3   '                    POL.DEG.                                ',
     4   ' PARAM  TYPE  MODEL  HEIGHT                                 '/
      DATA (TXTSTRG(10,II),II=1,4)/
     1   ' EARTH ROTATION PARAMETERS:                                 ',
     2   ' -------------------------                                  ',
     3   '                    PARAM.                                  ',
     4   ' PARAM  TYPE  MODEL SET NR   PAR.TYP   COEFF NR.            '/
      DATA (TXTSTRG(11,II),II=1,4)/
     1   ' STOCHASTIC ORBIT PARAMETERS:                               ',
     2   ' ---------------------------                                ',
     3   '                                                            ',
     4   ' PARAM  TYPE   ARC   SAT.  SET   TYPE      DIRECTION        '/
      DATA (TXTSTRG(12,II),II=1,4)/
     1   ' SATELLITE ANTENNA OFFSETS:                                 ',
     2   ' -------------------------                                  ',
     3   '                                                            ',
     4   ' PARAM  TYPE  REQUEST  GROUP  COORDINATE                    '/
      DATA (TXTSTRG(13,II),II=1,4)/
     1   ' EARTH POTENTIAL PARAMETERS:                                ',
     2   ' --------------------------                                 ',
     3   '                                                            ',
     4   ' PARAM  TYPE  DEGREE N  ORDER M  SIN/COS                    '/
      DATA (TXTSTRG(14,II),II=1,4)/
     1   ' RESONANCE TERMS (HILL THEORY):                             ',
     2   ' -----------------------------                              ',
     3   '                                                            ',
     4   ' PARAM  TYPE  ARC  SAT  R/S/W  CONST/SIN/COS                '/
      DATA (TXTSTRG(15,II),II=1,4)/
     1   ' ALBEDO PARAMETERS:                                         ',
     2   ' -----------------                                          ',
     3   '                                                            ',
     4   ' PARAM  TYPE  GROUP  MODEL                                  '/
      DATA (TXTSTRG(16,II),II=1,4)/
     1   ' CENTER OF MASS:                                            ',
     2   ' --------------                                             ',
     3   '                                                            ',
     4   ' PARAM  TYPE  COORDINATE                                    '/
      DATA (TXTSTRG(17,II),II=1,4)/
     1   ' STOCHASTIC IONOSPHERE PARAMETERS:                          ',
     2   ' --------------------------------                           ',
     3   '                                                            ',
     4   ' PARAM  TYPE  FILE  SAT  EPOCH                              '/
      DATA (TXTSTRG(18,II),II=1,4)/
     1   ' RECEIVER ANTENNA PHASE CENTER VARIATIONS:                  ',
     2   ' ----------------------------------------                   ',
     3   '                              POINT NUMBER       # POINTS   ',
     4   ' PARAM  TYPE  REQUEST  FREQ  ZENITH AZIMUTH   ZENITH AZIMUTH'/
      DATA (TXTSTRG(19,II),II=1,4)/
     1   ' GLOBAL IONOSPHERE MODEL PARAMETERS:                        ',
     2   ' ----------------------------------                         ',
     3   '                                                            ',
     4   ' PARAM  TYPE  REQUEST  MODEL  DEGREE  ORDER                 '/
      DATA (TXTSTRG(20,II),II=1,4)/
     1   ' STATION VELOCITIES:                                        ',
     2   ' ------------------                                         ',
     3   '                                                            ',
     4   ' PARAM  TYPE                                                '/
      DATA (TXTSTRG(21,II),II=1,4)/
     1   ' KINEMATIC COORDINATES:                                     ',
     2   ' ---------------------                                      ',
     3   '                                                            ',
     4   ' PARAM  TYPE  STATION NAME     COORDINATE  EPOCH            '/
      DATA (TXTSTRG(22,II),II=1,4)/
     1   ' SCALING FACTORS FOR VIENNA GRID FILES:                     ',
     2   ' -------------------------------------                      ',
     3   '                                                            ',
     4   ' PARAM  TYPE  GRID TYPE   STATION NAME       COMPONENT      '/
      DATA (TXTSTRG(23,II),II=1,4)/
     1   ' EPOCH WISE CLOCKS STATION CLOCKS:                          ',
     2   ' --------------------------------                           ',
     3   '                                                            ',
     4   ' PARAM  TYPE  STATION NAME           EPOCH    REF           '/
      DATA (TXTSTRG(24,II),II=1,4)/
     1   ' EPOCH WISE CLOCKS SATELLITE CLOCKS:                        ',
     2   ' ----------------------------------                         ',
     3   '                                                            ',
     4   ' PARAM  TYPE  SAT       EPOCH    REF                        '/
      DATA (TXTSTRG(25,II),II=1,4)/
     1   ' SATELLITE ANTENNA PHASE CENTER VARIATIONS:                 ',
     2   ' -----------------------------------------                  ',
     3   '                            POINT NUMBER      # POINTS      ',
     4   ' PARAM  TYPE  REQ.  GROUP  ZENITH AZIMUTH  ZENITH AZIMUTH   '/
      DATA (TXTSTRG(26,II),II=1,4)/
     1   ' RANGE BIASES:                                              ',
     2   ' ------------                                               ',
     3   '                                                            ',
     4   ' PARAM  TYPE  STATION            WL   SATELLITE             '/
      DATA (TXTSTRG(27,II),II=1,4)/
     1   ' HOI SCALING PARAMETERS:                                    ',
     2   ' ----------------------                                     ',
     3   '                                                            ',
     4   ' PARAM  TYPE  STATION NAME     HOI                          '/
      DATA (TXTSTRG(28,II),II=1,4)/
     1   ' HELMERT PARAMETERS:                                        ',
     2   ' ------------------                                         ',
     3   '                                                            ',
     4   ' PARAM  TYPE                                                '/
      DATA (TXTSTRG(30,II),II=1,4)/
     1   ' GNSS-SPECIFIC PARAMETERS:                                  ',
     2   ' ------------------------                                   ',
     3   '                                                            ',
     4   ' PARAM  TYPE  STATION NAME     SYS CMP                      '/
C
C CHECK MAXIMUM DIMENSION
C -----------------------
      CALL MAXTST(1,'PRIPAR','MAXTXT',MAXTXT,MAXTYP,IRCMAX)
      IF (IRCMAX.NE.0) CALL EXITRC(2)
C
C Get system of dynamic orbit parameters
C --------------------------------------
      DO 10 II=1,orbdsc%nlin
        IF (orbdsc%orbmod(ii)(1:7)=='EMPIRI:') THEN
           empiri = orbdsc%orbmod(ii)(9:12)
        ENDIF
10    CONTINUE
      IF (empiri=='RSW') ELETYP(7)=RSWSYS(1)
      IF (empiri=='RSW ' .OR. empiri=='DRSW') THEN
         ELETYP(8:15)=RSWSYS(2:9)
      ENDIF
C
C PRINT TITLE LINES
C -----------------
      IF(PRIOPT.NE.0) THEN
1       FORMAT(A)
        WRITE(LFNPRT,2) TITLES(1)(1:LENGT1(TITLES(1))),
     1                  TITLES(2)(1:LENGT1(TITLES(2)))
2       FORMAT(//,A,/,A,/,' ',131('-'),//)
        WRITE(LFNPRT,"(
     1       ' 11. PARAMETER CHARACTERIZATION LIST'
     2    ,/,' -----------------------------------')")
C
C PRINT PARAMETER CHARACTERIZATION LIST
C -------------------------------------
        IFRTIT=1
        DO 9100 ITYP=1,MAXTYP
          IFIRST=1
C
C LOOP OVER ALL PARAMETERS
          DO 9000 IPAR=1,NPAR
            ITYPE=LOCQ(1,IPAR)
            IF(ITYPE.NE.ITYP) GOTO 9000
            IF(IFIRST.EQ.1) THEN
              IF(IFRTIT.NE.1) THEN
                WRITE(LFNPRT,3)
3               FORMAT(' ')
              ENDIF
C
              WRITE(LFNPRT,"(' ',4(/,A),/,1X,131('-'),/,1X)")
     1          TRIM(TXTSTRG(ITYP,1)),
     2          TRIM(TXTSTRG(ITYP,2)),
CCC (IF 3. LINE IS EMPTY WRITE A BLANK CHARACTER)
     3          (TXTSTRG(ITYP,3)(1:LENGT1(TXTSTRG(ITYP,3)))),
     4          TRIM(TXTSTRG(ITYP,4))
C
              IFRTIT=0
              IFIRST=0
            ENDIF
            GOTO ( 100, 200, 300, 400, 500, 600, 700, 800, 900,1000,
     1            1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,
     2            2100,2200,2300,2400,2500,2600,2700,2800,9000,3000)
     3             ITYPE
C
C STATION PARAMETERS
100         WRITE(LFNPRT,101) IPAR,ITYP,STNAME(LOCQ(2,IPAR)),
     1                        COOTYP(LOCQ(3,IPAR))
101         FORMAT(I5,I6,3X,A16,5X,A1)
            GOTO 9000
C
C RECEIVER CLOCKS
200         IF (LOCQ(6,IPAR).EQ.0.AND.LOCQ(7,IPAR).EQ.0) THEN
              WRITE(LFNPRT,201) IPAR,ITYP,LOCQ(3,IPAR),
     1                          STNAME(LOCQ(2,IPAR))
201           FORMAT(I5,I6,I7,5X,A16)
            ELSE IF (LOCQ(6,IPAR).EQ.0) THEN
              WRITE(LFNPRT,202) IPAR,ITYP,LOCQ(3,IPAR),
     1                          STNAME(LOCQ(2,IPAR)),'SAT',LOCQ(7,IPAR)
            ELSE IF (ABS(LOCQ(6,IPAR)).EQ.1) THEN
              WRITE(LFNPRT,202) IPAR,ITYP,LOCQ(3,IPAR),
     1                          STNAME(LOCQ(2,IPAR)),'FRQ',LOCQ(4,IPAR)
202           FORMAT(I5,I6,I7,5X,A16,5X,A,I5)
            ELSE IF (ABS(LOCQ(6,IPAR)).EQ.2) THEN
              WRITE(LFNPRT,202) IPAR,ITYP,LOCQ(3,IPAR),
     1                          STNAME(LOCQ(2,IPAR)),'SAT',LOCQ(4,IPAR)
            ELSE IF (ABS(LOCQ(6,IPAR)).EQ.4) THEN
              WRITE(LFNPRT,202) IPAR,ITYP,LOCQ(3,IPAR),
     1                          STNAME(LOCQ(2,IPAR)),
     2                         'FRQ   polynomial n =',LOCQ(4,IPAR)-1
            ELSE IF (ABS(LOCQ(6,IPAR)).EQ.5) THEN
              CALL timst2(1,1,timisb(1,locq(4,ipar)),EPOSTR)
              WRITE(LFNPRT,203) IPAR,ITYP,LOCQ(2,IPAR),
     1                          STNAME(LOCQ(2,IPAR)),
     2                         'Time-dep. intersystem bias: ',
     3                          EPOSTR
203           FORMAT(I5,I6,I7,5X,A16,5X,A,A)
            ENDIF
            GOTO 9000
C
C ORBITAL ELEMENTS
300         WRITE(LFNPRT,301) IPAR,ITYP,LOCQ(2,IPAR),LOCQ(3,IPAR),
     1                        ELETYP(LOCQ(4,IPAR))
301         FORMAT(I5,I6,I5,I6,4X,A5)
            GOTO 9000
C
C AMBIGUITIES
400         WRITE(LFNPRT,401) IPAR,ITYP,(LOCQ(I,IPAR),I=2,6)
401         FORMAT(I5,2I6,I8,7X,'L',I1,6X,'L',I1,I11)
            GOTO 9000
C
C RECEIVER ANTENNA OFFSETS
500         WRITE(LFNPRT,501) IPAR,ITYP,LOCQ(2,IPAR),LOCQ(3,IPAR),
     1                        CO2TYP(LOCQ(4,IPAR))
501         FORMAT(I4,I6,2I7,9X,A1)
            GOTO 9000
C
C TROPOSPHERE PARAMETERS FOR INDIVIDUAL STATIONS
600         WRITE(LFNPRT,601) IPAR,ITYP,LOCQ(2,IPAR),
     1                        STNAME(LOCQ(3,IPAR)),CO2TYP(LOCQ(4,IPAR))
601         FORMAT(I5,I6,I7,5X,A16,6X,A1)
            GOTO 9000
C
C LOCAL IONOSPHERE MODELS
700         WRITE(LFNPRT,701) IPAR,ITYP,LOCQ(2,IPAR),LOCQ(4,IPAR),
     1                        LOCQ(3,IPAR)
701         FORMAT(I5,2I6,I7,I6)
            GOTO 9000
C
C DIFFERENTIAL CODE BIASES
800         IGRP=LOCQ(2,IPAR)
            IF (IGRP.EQ.1) THEN
              WRITE(LFNPRT,801) IPAR,ITYP,DCBTXT(IGRP),LOCQ(3,IPAR),
     1          DCBTYP(LOCQ(5,IPAR))
801           FORMAT(I5,I6,3X,A3,I8,24X,A5)
            ELSE
              WRITE(LFNPRT,802) IPAR,ITYP,DCBTXT(IGRP),
     1          STNAME(LOCQ(3,IPAR)),g_svnsys(LOCQ(5,IPAR)-1),
     2          DCBTYP(IABS(LOCQ(6,IPAR)))
802           FORMAT(I5,I6,3X,A3,4X,A16,4X,A1,7X,A5)

            ENDIF
            GOTO 9000
C
C LOCAL TROPOSPHERE MODELS
900         WRITE(LFNPRT,901) IPAR,ITYP,LOCQ(2,IPAR),LOCQ(3,IPAR)
901         FORMAT(I5,2I6,I8)
            GOTO 9000
C
C EARTH ROTATION PARAMETERS
1000        WRITE(LFNPRT,1001)IPAR,ITYP,LOCQ(2,IPAR),LOCQ(3,IPAR),
     1                        POLTYP(LOCQ(4,IPAR)),LOCQ(5,IPAR)
1001        FORMAT(I5,3I6,6X,A7,I7)
            GOTO 9000
C
C STOCHASTIC ORBIT PARAMETERS
1100        IFRC = mod(LOCQ(5,IPAR),10)
            JTYP = 1+(LOCQ(5,IPAR)-mod(LOCQ(5,IPAR),10))/10
            WRITE(LFNPRT,1101)IPAR,ITYP,LOCQ(2,IPAR),LOCQ(3,IPAR),
     1                        LOCQ(4,IPAR),TYPTXT(JTYP),FRCTXT(IFRC)
1101        FORMAT(I5,4I6,4X,A8,3(2X,A16))
            GOTO 9000
C
C SATELLITE ANTENNA OFFSETS
1200        WRITE(LFNPRT,1201) IPAR,ITYP,LOCQ(2,IPAR),LOCQ(5,IPAR),
     1                         COOTYP(LOCQ(3,IPAR))
1201        FORMAT(I5,I6,I8,I8,8X,A1)
            GOTO 9000
C
C EARTH POTENTIAL PARAMETERS
1300        WRITE(LFNPRT,1301) IPAR,ITYP,LOCQ(5,IPAR),LOCQ(6,IPAR),
     1                         COSSIN(LOCQ(4,IPAR))
1301        FORMAT(I5,I6,2I9,7X,A3)
            GOTO 9000
C
C HILL'S RESONANCE TERMS
1400        WRITE(LFNPRT,1401) IPAR,ITYP,LOCQ(2,IPAR),LOCQ(3,IPAR),
     1                         HILCMP(LOCQ(4,IPAR)),
     2                         HILTYP(LOCQ(5,IPAR))
1401        FORMAT(I5,2I6,I5,4X,A1,9X,A3)
            GOTO 9000
C
C EARTH ALBEDO PARAMETERS
1500        WRITE(LFNPRT,1501) IPAR,ITYP,LOCQ(5,IPAR),LOCQ(4,IPAR)
1501        FORMAT(I5,I6,I7,I6)
            GOTO 9000
C
C CENTER OF MASS COORDINATES
1600        WRITE(LFNPRT,1601) IPAR,ITYP,COOTYP(LOCQ(2,IPAR))
1601        FORMAT(I5,I6,7X,A1)
            GOTO 9000
C
C DIFFERENTIAL IONOSPHERE PARAMETERS
1700        WRITE(LFNPRT,1701) IPAR,ITYP,(LOCQ(II,IPAR),II=2,4)
1701        FORMAT(I5,3I6,I7)
            GOTO 9000
C
C RECEIVER ANTENNA PHASE CENTER VARIATIONS
1800        IF (LOCQ(6,IPAR).GT.0) THEN
              WRITE(LFNPRT,1801) IPAR,ITYP,(LOCQ(II,IPAR),II=2,6),
     1                           (LOCQ(7,IPAR)-1)
            ELSE
              WRITE(LFNPRT,1801) IPAR,ITYP,(LOCQ(II,IPAR),II=2,7)
            ENDIF
1801        FORMAT(I5,I6,I8,3I7,I10,I7)
            GOTO 9000
C
C GLOBAL IONOSPHERE MODEL PARAMETERS
1900        IREQ=LOCQ(2,IPAR)
            IF (IREQ.EQ.1 .OR. IREQ.EQ.3) THEN
              WRITE(LFNPRT,1901) IPAR,ITYP,GIMTXT(IREQ),
     1                           (LOCQ(II,IPAR),II=3,5)
            ELSE
              WRITE(LFNPRT,1901) IPAR,ITYP,GIMTXT(IREQ),
     1                           LOCQ(3,IPAR)
            END IF
1901        FORMAT(I5,I6,3X,A3,I9,I7,I8)
            GOTO 9000
C
C STATION VELOCITIES
2000        GOTO 9000
C
C KINEMATIC STATION COORDINATES
2100        WRITE(LFNPRT,2101) IPAR,ITYP,STNAME(LOCQ(2,IPAR)),
     1                         COOTYP(LOCQ(3,IPAR)),LOCQ(4,IPAR)
2101        FORMAT(I5,I6,3X,A16,5X,A1,I12)
            GOTO 9000
C
C SCALING FACTORS FOR VIENNA GRID FILES
2200        STHELP = OPLOAD(LOCQ(2,IPAR))%staLst(LOCQ(3,IPAR))
            IF (OPLOAD(LOCQ(2,IPAR))%staClu(LOCQ(3,IPAR)) == -1) THEN
              STHELP = 'ALL STATIONS'
            ELSE IF (OPLOAD(LOCQ(2,IPAR))%staClu(LOCQ(3,IPAR)) > 0) THEN
              STHELP = ''
              WRITE(STHELP,'(A,1X,I3.3)')
     1              'GROUP',OPLOAD(LOCQ(2,IPAR))%staClu(LOCQ(3,IPAR))
            ENDIF
            WRITE(LFNPRT,2201) IPAR,ITYP,OPLOAD(LOCQ(2,IPAR))%KEYW,
     1                         STHELP,GRDTYP(LOCQ(5,IPAR),LOCQ(4,IPAR))
2201        FORMAT(I5,I6,3X,A9,3X,A16,6X,A3)
            GOTO 9000
C
C EPOCH WISE STATION CLOCKS
2300        CHR1=' '
            IF (LOCQ(7,IPAR).GT.0) CHR1='R'
C           ADD_GNSS_HERE ?
            IF (LOCQ(3,IPAR).EQ.0) THEN
              WRITE(LFNPRT,2301) IPAR,ITYP,STNAME(LOCQ(2,IPAR)),
     1                           LOCQ(4,IPAR),CHR1,' '
            ELSE
              WRITE(LFNPRT,2301) IPAR,ITYP,STNAME(LOCQ(2,IPAR)),
     1                           LOCQ(4,IPAR),CHR1,
     2                           g_strsys(LOCQ(3,IPAR)-1)
            ENDIF
2301        FORMAT(I5,I6,3X,A16,I12,5X,A1,5X,A)
            GOTO 9000
C
C EPOCH WISE SATELLITE CLOCKS
2400        CHR1=' '
            IF (LOCQ(7,IPAR).GT.0) CHR1='R'
            WRITE(LFNPRT,2401) IPAR,ITYP,LOCQ(3,IPAR),LOCQ(4,IPAR),CHR1
2401        FORMAT(I5,2I6,I12,5X,A1)
            GOTO 9000
C           ADD_GNSS_HERE ?
C
C SATELLITE ANTENNA PHASE CENTER VARIATIONS
2500        WRITE(LFNPRT,2501) IPAR,ITYP,(LOCQ(II,IPAR),II=2,6),
     1                         (LOCQ(7,IPAR)-1)
2501        FORMAT(I5,3I6,4I8)
            GOTO 9000
C
C SLR RANGE BIASES
2600        WRITE(LFNPRT,2601) IPAR,ITYP,STNAME(LOCQ(2,IPAR)),
     1                         LOCQ(4,IPAR),LOCQ(5,IPAR)
2601        FORMAT(I5,I6,3X,A16,3X,I2,4X,I3)
            GOTO 9000
C
C HOI SCALING FACTORS
2700        IF (LOCQ(3,IPAR).EQ.1) THEN
              WRITE(LFNPRT,2701) IPAR,ITYP,' ',LOCQ(2,IPAR)
            ELSEIF (LOCQ(3,IPAR).EQ.2) THEN
              WRITE(LFNPRT,2701) IPAR,ITYP,STNAME(LOCQ(4,IPAR)),
     1                           LOCQ(2,IPAR)
            ENDIF
2701        FORMAT(I5,I6,3X,A16,I3)
            GOTO 9000
C
C HELMERT PARAMETERS
2800        GOTO 9000
C
C GNSS-SPECIFIC STATION TRANSLATIONS
3000        WRITE(LFNPRT,3010) IPAR,ITYP,STNAME(LOCQ(2,IPAR)),
     1            g_strSys(LOCQ(4,IPAR)-1),GSPTYP(LOCQ(3,IPAR))
3010        FORMAT(I5,I6,3X,A16,1X,A3,2X,A1)
            GOTO 9000
C
9000      CONTINUE
C
9100    CONTINUE
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
