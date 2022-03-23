      MODULE s_MODOPT
      CONTAINS

C*
      SUBROUTINE MODOPT(ISETOP,NFRCHK,IIONO,IUSFLG,
     1                  MRK1O2,MAXZEN,MXHOLE,MNCONT,IPRNT1,
     2                  MXINTR,IPPROC,Q,DISCLV,LTRIP,SIGWGS,
     3                  IPRNT2,SIGL12,IWLSCR,SWIDTH,
     4                  MINCYC,IRJECT,MXOGAP,MXIOND,IAMNEW,
     5                  NTONLY,L5CLEA,OMCMAX)
CC
CC NAME       :  MODOPT
CC
CC PURPOSE    :  MODIFY OPTIONS FOR MANUAL PREPROCESSING
CC
CC PARAMETERS :
CC     IN/OUT :  ISETOP : OPTIONS ADJUSTED TO FILE HEADER INFO I*4
CC                         =0 : TAKE INPUT FILE OPTIONS IN ANY
CC                              CASE
CC                         =1 : ADJUST FREQUENCIES ETC TO FILE
CC                              HEADER INFORMATION
CC               NFRCHK : FREQUENCY(IES) TO BE CHECKED         I*4
CC                         =1 : L1
CC                         =2 : L2
CC                         =3 : L1 AND L2 TOGETHER (VIA L5 AND L3)
CC                         =4 : L1 AND L2 SEPARATELY
CC               IIONO  : IONOSPHERE INDEX                     I*4
CC                         =0 : IONOSPHERE MODELS NOT APPLIED
CC                         =1 : IONOSPHERE MODELS APPLIED
CC               IUSFLG : USE OR IGNORE OBS FILE FLAGS         I*4
CC
CC               MRK1O2 : FLAG TO MARK UNPAIRED L1/L2 OBSERVA- I*4
CC                        TIONS
CC                         =0 : NO MARKING DONE
CC                         =1 : L1 WITHOUT L2, OR L2 WITHOUT L1
CC                              OBSERVATIONS ARE MARKED
CC               MAXZEN : MAXIMUM SATELLITE ZENITH DISTANCE    I*4
CC                        (DEGREE)
CC               MXHOLE : MAXIMAL GAP IN OBSERVATIONS ALLOWED  I*4
CC                        TO BE CONSIDERED AS CONTINUOUS (SEC)
CC               MNCONT : MINIMAL TIME INTERVAL OF CONTINUOUS  I*4
CC                        OBSERVATIONS (SEC)
CC               IPRNT1 : PRINT LEVEL FOR NON-PARAM. SCREENING I*4
CC                         =0 : NO MESSAGES PRINTED
CC                         =1 : PRINT SUMMARY MESSAGES
CC                         =2 : PRINT ALL MESSAGES
CC               MXINTR : MAXIMUM INTERVAL LENGTH FOR          I*4
CC                        POLYNOMIAL FIT (MIN)
CC               IPPROC : SCREENING FLAG                       I*4
CC                        IPPROC(1): SCREENING SINGLE DIFF.
CC                        IPPROC(2): SCREENING DOUBLE DIFF.
CC                         =0 : SKIP SCREENING
CC                         =1 : SCREEN OBSERVATIONS
CC               Q      : POL. DEGREE USED FOR SCREENING       I*4
CC                        Q(1): FOR SINGLE DIFF. SCREENING
CC                        Q(2): FOR DOUBLE DIFF. SCREENING
CC               DISCLV : MAXIMUM ALLOWED DISCONTINUITY        R*8
CC                        DISCLV(1) FOR SINGLE DIFF. SCREENING
CC                        DISCLV(2) FOR DOUBLE DIFF. SCREENING
CC               LTRIP  : CARRIER TO BE USED FOR TRIPLE DIFF.  I*4
CC                        SOLUTION
CC                         =1 : USE L1
CC                         =2 : USE L2
CC                         =3 : USE L3
CC                         =5 : USE L5
CC               SIGWGS(I),I=1,2,3: A PRIORI SIGMAS FOR        R*8
CC                        STATION COORDINATES (M)
CC               IPRNT2 : PRINT LEVEL FOR CYCLE SLIP DETECTION I*4
CC                         =0 : NO MESSAGES PRINTED
CC                         =1 : PRINT SUMMARY MESSAGES
CC                         =2 : PRINT ALL MESSAGES
CC               SIGL12(L),L=1,2: RMS ERRORS FOR FREQUENCY L   R*8
CC               IWLSCR(L),L=1,2: WAVELENGTH FACTOR FOR        I*4
CC                        FOR FREQUENCY L
CC                         =1 : SCREENING USING CYCLES
CC                         =2 : SCREENING USING HALF CYCLES
CC               SWIDTH(K),K=1,2: NUMBER OF NEAREST INTEGERS   I*4
CC                        TO BE TESTED
CC                         K=1 : IN L1/L2
CC                         K=2 : IN L5
CC               MINCYC : ACCEPT CYCLE SLIPS > "MINCYC" CYCLES I*4
CC               IRJECT : REJECT OUTLIERS (YES=1,NO=0)         I*4
CC               MXOGAP : MAXIMUM OBSERVATION GAP ALLOWED IN   I*4
CC                        OUTLIER REJECTION (SEC)
CC               MXIOND : MAXIMUM IONOSPHERE CHANGE BETWEEN    I*4
CC                        EPOCHS (IN % OF L1 CYCLES) FOR OUT-
CC                        LIER REJECTION
CC               IAMNEW(I),I=1,2,3: SETTING OF NEW AMBIGUITIES I*4
CC                        I=1 : USE CYCLE SLIP FLAG (0/1)
CC                        I=2 : IF PROBLEM IN SLIP-FIXING (0/1)
CC                        I=3 : AFTER GAP LARGER THAN (SEC)
CC                        I=4 : USE AMBIGUITIES FROM FILE (0/1)
CC                        I=5 : MIN. TIME OF OBS. PER AMB. (SEC)
CC               NTONLY : TEST OBS. WITH CYCLE SLIP FLAG       I*4
CC                          ONLY (0/1)
CC               L5CLEA : L5 IS CLEAN (EXCEPT FLAGGED          I*4
CC                          EPOCHS)  (0/1)
CC               OMCMAX : MAXIMUM OBSERV-COMPUTED VALUE (M)    R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M. ROTHACHER, L. MERVART
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/03 08:59
CC
CC CHANGES    :  05-JUN-92 : ??: CHANGES FOR THE NEW MAUPRP VERSION
CC               11-JAN-94 : SF: INTERNAL READ WITH FORMAT "*" REPLACED
CC                               BY INPCI4,INPCR8
CC               25-APR-95 : MR: ADD OPTION FOR MAXIMUM O-C
CC               28-SEP-95 : JJ: DECLARE STAFIX TO BE C*16
CC               04-APR-96 : MR: CHANGE ORDER IN COMMON
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               04-NOV-05 : HB: INPCR8 IS INTEGER*4 AND NOT REAL*8
CC               29-FEB-12 : RD: CORRECT ARRAY DIMENSIONS OF INPCxx ROUTINES
CC               29-FEB-12 : RD: REMOVE UNUSED VARIABLES
CC               27-MAR-12 : RD: USE PROMP1 AS MODULE NOW
CC               28-MAR-12 : RD: USE INPCI4 AS MODULE NOW
CC               28-MAR-12 : RD: USE INPCR8 AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,  ONLY: lfnprt, lfnkbd
      USE s_promp1
      USE f_inpcr8
      USE s_upperc
      USE f_inpci4
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 ICH1  , ICH2  , ICHG  , ICHR  , IFIRST, IIONO , ILAB  ,
     1          ILIN  , IPRNT1, IPRNT2, IRCODE, IRJECT,
     2          ISETOP, ISTART, IUSFLG, IVAL  , L5CLEA, LTRIP ,
     3          MAXLAB, MAXLIN, MAXZEN, MINCYC, MNCONT, MRK1O2, MXHOLE,
     4          MXINTR, MXIOND, MXOGAP, NFRCHK, NTONLY
C
      REAL*8    OMCMAX
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MAXLIN=78,MAXLAB=53)
C
      CHARACTER*80 STRG(1)
      CHARACTER*72 TEXT(MAXLIN)
      CHARACTER*60 OPTION,STRHLP
      CHARACTER*3  LABEL(MAXLAB),LABEL1
      CHARACTER*1  CDUMMY
      INTEGER*4    ILINE(2,MAXLAB),IOPTIO(MAXLAB),ITYPE(2,MAXLAB)
      INTEGER*4    IDUMMY(1)
      REAL*8       ROPTIO(MAXLAB)
C
      INTEGER*4    IAMNEW(*),IPPROC(2),Q(2),IWLSCR(2),SWIDTH(2)
      REAL*8       DISCLV(2),SIGWGS(3),SIGL12(2)
C
      COMMON/CMDOPT/ROPTIO,IOPTIO,TEXT
C
      DATA IFIRST/1/
      DATA LABEL/'A  ','G  ','S  ','C  ','G0 ','G1 ','G2 ','G3 ',
     1           'G4 ','G5 ','G6', 'G7 ','S0 ','S1 ','S2 ','S3 ',
     2           'C0 ','C1 ','C2 ','C3 ','C4 ','C5 ','S01','S02',
     3           'S11','S12','S13','S21','S22','S23','S31','S32',
     4           'S33','S34','S35','C01','C11','C12','C21','C22',
     5           'C23','C24','C25','C26','C31','C41','C42','C43',
     6           'C51','C52','C53','C54','C55'/
      DATA ILINE/ 1,78, 1,11,13,39,41,78, 4, 4, 5, 5, 6, 6, 7, 7,
     1            8, 8, 9, 9,10,10,11,11,16,19,21,25,27,31,33,39,
     2           44,46,48,51,53,60,62,64,66,70,72,78,18,18,19,19,
     3           23,23,24,24,25,25,29,29,30,30,31,31,35,35,36,36,
     4           37,37,38,38,39,39,46,46,50,50,51,51,55,55,56,56,
     5           57,57,58,58,59,59,60,60,64,64,68,68,69,69,70,70,
     6           74,74,75,75,76,76,77,77,78,78/
      DATA ITYPE/ 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1,
     1            1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
     2            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
     3            1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2,
     4            1, 2, 1, 2, 1, 2, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1,
     5            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
     6            1, 1, 1, 1, 1, 1, 1, 1, 1, 1/
C
C INITIALIZATION OF TEXT
C ----------------------
      IF(IFIRST.EQ.1) THEN
        IFIRST=0
        TEXT( 1)='G    GENERAL OPTIONS                                 '
        TEXT( 2)='     ---------------                                 '
        TEXT( 3)='                                                     '
        TEXT( 4)='G0   ADJUST OPTIONS ACCORDING TO FILE HEADER INFO    '
        TEXT( 5)='G1   CHECK FREQUENCIES (L1=1, L2=2, L1&L2=3, L1,L2=4)'
        TEXT( 6)='G2   APPLY IONOSPHERE MODEL                          '
        TEXT( 7)='G3   USE MARKING  FLAGS FROM OBSERVATION FILES       '
        TEXT( 8)='G4   MARK UNPAIRED DUAL FREQUENCY OBSERVATIONS       '
        TEXT( 9)='G5   MINIMUM ELEVATION FOR SATELLITES (DEGREES)      '
        TEXT(10)='G6   MAXIMUM GAP IN OBSERVATIONS (SEC)               '
        TEXT(11)='G7   MINIMUM TIME INTERVAL OF CONTINOUS OBS. (SEC)   '
        TEXT(12)='                                                     '
        TEXT(13)='S    SCREENING AND TRIPLE DIFFERENCE SOLUTION        '
        TEXT(14)='     ----------------------------------------        '
        TEXT(15)='                                                     '
        TEXT(16)='S0   GENERAL SCREENING PARAMETERS:                   '
        TEXT(17)='                                                     '
        TEXT(18)='S01  PRINT MESSAGES (NO=0, SUMMARY=1, ALL=2)         '
        TEXT(19)='S02  MAX. INTERVAL LENGTH (MIN) FOR POLYNOMIAL FIT   '
        TEXT(20)='                                                     '
        TEXT(21)='S1   SINGLE DIFFERENCE SCREENING:                    '
        TEXT(22)='                                                     '
        TEXT(23)='S11  SCREEN SINGLE DIFFERENCES                       '
        TEXT(24)='S12  POLYNOMIAL DEGREE                               '
        TEXT(25)='S13  DISCONTINUITY LEVEL (M)                         '
        TEXT(26)='                                                     '
        TEXT(27)='S2   DOUBLE DIFFERENCE SCREENING:                    '
        TEXT(28)='                                                     '
        TEXT(29)='S21  SCREEN DOUBLE DIFFERENCES                       '
        TEXT(30)='S22  POLYNOMIAL DEGREE                               '
        TEXT(31)='S23  DISCONTINUITY LEVEL (M)                         '
        TEXT(32)='                                                     '
        TEXT(33)='S3   TRIPLE DIFFERENCE SOLUTION:                     '
        TEXT(34)='                                                     '
        TEXT(35)='S31  FREQUENCY TO BE USED (L1=1, L2=2, L3=3, L5=5)   '
        TEXT(36)='S32  A PRIORI SIGMA FOR STATION COORDINATE X (M)     '
        TEXT(37)='S33  A PRIORI SIGMA FOR STATION COORDINATE Y (M)     '
        TEXT(38)='S34  A PRIORI SIGMA FOR STATION COORDINATE Z (M)     '
        TEXT(39)='S35  MAXIMUM VALUE FOR OBSERVED-COMPUTED (M)         '
        TEXT(40)='                                                     '
        TEXT(41)='C    CYCLE SLIP DETECTION AND OUTLIER REJECTION      '
        TEXT(42)='     ------------------------------------------      '
        TEXT(43)='                                                     '
        TEXT(44)='C0   GENERAL CYCLE SLIP DETECTION PARAMETERS:        '
        TEXT(45)='                                                     '
        TEXT(46)='C01  PRINT MESSAGES (NO=0, SUMMARY=1, ALL=2)         '
        TEXT(47)='                                                     '
        TEXT(48)='C1   SIGMA OF PHASE OBSERVATIONS:                    '
        TEXT(49)='                                                     '
        TEXT(50)='C11  SIGMA FOR L1 OBSERVATIONS (M)                   '
        TEXT(51)='C12  SIGMA FOR L2 OBSERVATIONS (M)                   '
        TEXT(52)='                                                     '
        TEXT(53)='C2   WAVELENGTH FACTORS AND SEARCH WIDTHS:           '
        TEXT(54)='                                                     '
        TEXT(55)='C21  DETECT CYCLES (=1) OR HALF CYCLES (=2) IN L1    '
        TEXT(56)='C22  DETECT CYCLES (=1) OR HALF CYCLES (=2) IN L2    '
        TEXT(57)='C23  NUMBER OF INTEGERS TO BE TESTED IN L1/L2        '
        TEXT(58)='C24  NUMBER OF INTEGERS TO BE TESTED IN L5           '
        TEXT(59)='C25  TEST OBS. WITH CYCLE SLIP FLAG ONLY (0/1)       '
        TEXT(60)='C26  L5 IS CLEAN (EXCEPT FLAGGED EPOCHS) (0/1)       '
        TEXT(61)='                                                     '
        TEXT(62)='C3   SIZE OF CYCLE SLIPS TO BE ACCEPTED              '
        TEXT(63)='                                                     '
        TEXT(64)='C31  ACCEPT ALL CYCLE SLIPS LARGER THAN (CYCLES)     '
        TEXT(65)='                                                     '
        TEXT(66)='C4   OUTLIER REJECTION:                              '
        TEXT(67)='                                                     '
        TEXT(68)='C41  OUTLIER REJECTION (NO=0, YES=1)                 '
        TEXT(69)='C42  MAXIMUM OBSERVATION GAP ALLOWED (SEC)           '
        TEXT(70)='C43  MAX. IONOS.DIFF. BETWEEN EPOCHS (% L1 CYCLES)   '
        TEXT(71)='                                                     '
        TEXT(72)='C5   SETTING OF NEW AMBIGUITIES                      '
        TEXT(73)='                                                     '
        TEXT(74)='C51  NEW AMB. IF CYCLE SLIP FLAG SET IN FILE (0/1)   '
        TEXT(75)='C52  NEW AMB. IF PROBLEM IN CYCLE SLIP FIXING (0/1)  '
        TEXT(76)='C53  NEW AMB. AFTER A GAP LARGER THAN (SEC)          '
        TEXT(77)='C54  USE MULTIPLE AMBIGUITIES FROM FILES (0/1)       '
        TEXT(78)='C55  MIN. TIME INTERVAL BETWEEN AMBIGUITIES (SEC)    '
      ENDIF
C
C ARRANGE OPTIONS IN ARRAYS (INTEGER,REAL)
C-----------------------------------------
      IOPTIO( 5)=ISETOP
      IOPTIO( 6)=NFRCHK
      IOPTIO( 7)=IIONO
      IOPTIO( 8)=IUSFLG
      IOPTIO( 9)=MRK1O2
      IOPTIO(10)=90-MAXZEN
      IOPTIO(11)=MXHOLE
      IOPTIO(12)=MNCONT
      IOPTIO(23)=IPRNT1
      IOPTIO(24)=MXINTR
      IOPTIO(25)=IPPROC(1)
      IOPTIO(26)=Q(1)
      ROPTIO(27)=DISCLV(1)
      IOPTIO(28)=IPPROC(2)
      IOPTIO(29)=Q(2)
      ROPTIO(30)=DISCLV(2)
      IOPTIO(31)=LTRIP
      ROPTIO(32)=SIGWGS(1)
      ROPTIO(33)=SIGWGS(2)
      ROPTIO(34)=SIGWGS(3)
      ROPTIO(35)=OMCMAX
      IOPTIO(36)=IPRNT2
      ROPTIO(37)=SIGL12(1)
      ROPTIO(38)=SIGL12(2)
      IOPTIO(39)=IWLSCR(1)
      IOPTIO(40)=IWLSCR(2)
      IOPTIO(41)=SWIDTH(1)
      IOPTIO(42)=SWIDTH(2)
      IOPTIO(43)=NTONLY
      IOPTIO(44)=L5CLEA
      IOPTIO(45)=MINCYC
      IOPTIO(46)=IRJECT
      IOPTIO(47)=MXOGAP
      IOPTIO(48)=MXIOND
      IOPTIO(49)=IAMNEW(1)
      IOPTIO(50)=IAMNEW(2)
      IOPTIO(51)=IAMNEW(3)
      IOPTIO(52)=IAMNEW(4)
      IOPTIO(53)=IAMNEW(5)
C
C ADD VALUES OF VARIABLES TO THE TEXT STRINGS
C -------------------------------------------
      DO 10 ILAB=1,MAXLAB
        IF(ITYPE(1,ILAB).EQ.1) THEN
          IF(ITYPE(2,ILAB).EQ.1) THEN
            WRITE(TEXT(ILINE(1,ILAB))(54:72),1) IOPTIO(ILAB)
1           FORMAT('  -->',I5)
          ELSE
            WRITE(TEXT(ILINE(1,ILAB))(54:72),2) ROPTIO(ILAB)
2           FORMAT('  -->',F10.4)
          ENDIF
        ENDIF
10    CONTINUE
C
C PRINT OVERVIEW
C --------------
      WRITE(LFNPRT,11) TEXT(1),TEXT(13),TEXT(40)
11    FORMAT(/,3(1X,A72,/),' A    ALL OPTIONS')
C
C LOOP OVER ALL MANIPULATIONS OF OPTIONS
C --------------------------------------
      DO 100 ICHG=1,10000
C
C READ OPTION (AND OPTION VALUE) FROM KEYBOARD
C --------------------------------------------
        WRITE(LFNPRT,12)
12      FORMAT(/,' DISPLAY OPTION(S): ENTER OPTION LABEL',
     1         /,' CHANGE  OPTION   : ENTER OPTION LABEL ',
     2           'AND NEW VALUE')
        STRG(1)='CONTINUE         : <RETURN> :'
        CALL PROMP1(1,STRG)
        READ(LFNKBD,13) OPTION
13      FORMAT(A)
        CALL UPPERC(OPTION)
C
C END OF OPTION DISPLAY AND CHANGE
C --------------------------------
        IF(OPTION.EQ.' ') GOTO 110
C
C READ OPTION LABEL FROM OPTION STRING
C ------------------------------------
        ICH1=0
        ICH2=0
        ISTART=1
        DO 20 ICHR=1,60
          IF(ISTART.EQ.1.AND.OPTION(ICHR:ICHR).NE.' ') THEN
            ISTART=0
            ICH1=ICHR
          ENDIF
          IF(ISTART.EQ.0.AND.OPTION(ICHR:ICHR).EQ.' ') THEN
            ICH2=ICHR-1
            GOTO 30
          ENDIF
20      CONTINUE
C
C INVALID LABEL ?
30      IF(ICH1.EQ.0.OR.ICH2.EQ.0.OR.ICH2-ICH1.LT.0.OR.ICH2-ICH1.GT.2)
     1    THEN
          WRITE(LFNPRT,31)
31        FORMAT(/,' INVALID OPTION LABEL')
          GOTO 100
        ENDIF
C
C SEARCH FOR OPTION LABEL
C -----------------------
        LABEL1=OPTION(ICH1:ICH2)
        DO 40 ILAB=1,MAXLAB
          IF(LABEL(ILAB).EQ.LABEL1) GOTO 50
40      CONTINUE
C
C LABEL NOT FOUND
        WRITE(LFNPRT,41) LABEL1
41      FORMAT(/,' OPTION LABEL NOT FOUND: ',A)
        GOTO 100
C
C DISPLAY OPTION(S) IF NO SINGLE OPTION SELECTED OR IF NO VALUE ENTERED
C ---------------------------------------------------------------------
50      IF(ITYPE(1,ILAB).EQ.0.OR.OPTION(ICH2+1:60).EQ.' ') THEN
C
C DISPLAY OPTION(S)
          WRITE(LFNPRT,'( )')
          DO 60 ILIN=ILINE(1,ILAB),ILINE(2,ILAB)
            WRITE(LFNPRT,51) TEXT(ILIN)
51          FORMAT(1X,A72)
60        CONTINUE
          GOTO 100
        ELSE
C
C CHANGE OPTION
          IF(ITYPE(2,ILAB).EQ.1) THEN
            STRHLP=OPTION(ICH2+1:60)
            IVAL=INPCI4(-60,1,0,STRHLP,IOPTIO(ILAB),CDUMMY,
     1                  IDUMMY,IRCODE)
            IF (IRCODE.NE.0) GOTO 70
            WRITE(TEXT(ILINE(1,ILAB))(54:72),1) IOPTIO(ILAB)
          ELSE
            STRHLP=OPTION(ICH2+1:60)
            IVAL=INPCR8(-60,1,0,STRHLP,ROPTIO(ILAB),CDUMMY,
     1                  IDUMMY,IRCODE)
            IF (IRCODE.NE.0) GOTO 70
            WRITE(TEXT(ILINE(1,ILAB))(54:72),2) ROPTIO(ILAB)
          ENDIF
          WRITE(LFNPRT,51) TEXT(ILINE(1,ILAB))
          GOTO 100
C
C BAD OPTION VALUE ENTERED
70        WRITE(LFNPRT,71) LABEL1
71        FORMAT(/,' BAD OPTION VALUE ENTERED FOR LABEL ',A)
          GOTO 100
        ENDIF
100   CONTINUE
C
C PUT OPTIONS BACK INTO SPECIFIC VARIABLES
C-----------------------------------------
110   ISETOP   = IOPTIO( 5)
      NFRCHK   = IOPTIO( 6)
      IIONO    = IOPTIO( 7)
      IUSFLG   = IOPTIO( 8)
      MRK1O2   = IOPTIO( 9)
      MAXZEN   = 90-IOPTIO(10)
      MXHOLE   = IOPTIO(11)
      MNCONT   = IOPTIO(12)
      IPRNT1   = IOPTIO(23)
      MXINTR   = IOPTIO(24)
      IPPROC(1)= IOPTIO(25)
      Q(1)     = IOPTIO(26)
      DISCLV(1)= ROPTIO(27)
      IPPROC(2)= IOPTIO(28)
      Q(2)     = IOPTIO(29)
      DISCLV(2)= ROPTIO(30)
      LTRIP    = IOPTIO(31)
      SIGWGS(1)= ROPTIO(32)
      SIGWGS(2)= ROPTIO(33)
      SIGWGS(3)= ROPTIO(34)
      OMCMAX   = ROPTIO(35)
      IPRNT2   = IOPTIO(36)
      SIGL12(1)= ROPTIO(37)
      SIGL12(2)= ROPTIO(38)
      IWLSCR(1)= IOPTIO(39)
      IWLSCR(2)= IOPTIO(40)
      SWIDTH(1)= IOPTIO(41)
      SWIDTH(2)= IOPTIO(42)
      NTONLY   = IOPTIO(43)
      L5CLEA   = IOPTIO(44)
      MINCYC   = IOPTIO(45)
      IRJECT   = IOPTIO(46)
      MXOGAP   = IOPTIO(47)
      MXIOND   = IOPTIO(48)
      IAMNEW(1)= IOPTIO(49)
      IAMNEW(2)= IOPTIO(50)
      IAMNEW(3)= IOPTIO(51)
      IAMNEW(4)= IOPTIO(52)
      IAMNEW(5)= IOPTIO(53)
C
      RETURN
      END SUBROUTINE

      END MODULE
