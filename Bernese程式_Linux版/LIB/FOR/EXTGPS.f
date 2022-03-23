      MODULE s_EXTGPS
      CONTAINS

C*
      SUBROUTINE EXTGPS(FILNAM,TITLE ,IPGM  ,NUMFIL,NPAR  ,NUMOBS,
     1                  RMS   ,POLCOR,NREQ  ,IRC   ,ISATM ,AMAX  ,
     2                  RMSAM ,IMIDSES,NUMSTA)
CC
CC NAME       :  EXTGPS
CC
CC PURPOSE    :  EXTRACT SOME DATA FROM THE GPSEST JOB OUTPUT FILES
CC
CC PARAMETER  :
CC        IN  : FILNAM  : NAME OF THE OUTPUT FILE (INCL EXT)  CH*(*)
CC        OUT : TITLE   : TITLE LINE OF JOB OUTPUT            CH*(*)
CC              IPGM    : OUTPUT FILE INDEX                   I*4
CC                        = 0: BAD OUTPUT FILE
CC                        = 1: GPSEST
CC                        = 2: ADDNEQ
CC                        = 3: ADDNEQ2
CC                        =11: GPSEST  (V5.0 output)
CC                        =13: ADDNEQ2 (V5.0 output)
CC              NUMFIL  : NUMBER OF SINGLE DIFF FILES         I*4
CC              NPAR    : TOTAL NUMBER OF PARAMETERS          I*4
CC              NUMOBS  : TOTAL NUMBER OF OBSERVATIONS        I*4
CC              RMS     : RMS OF SINGLE DIFF OBS              R*8
CC              POLCOR  : POLE CORRECTIONS AND RMS            R*8(3,10,100)
CC              NREQ    : NUMBER OF PARAMETER SETS            I*4
CC              IRC     : 0= OK.                              I*4
CC              ISATM   : NUMBER OF SATELLITE WITH MAXIMUM    I*4
CC                        SEMI MAJOR AXIS ADJUSTMENT
CC              AMAX    : SEMI MAJOR AXIS ADJUSTMENT          R*8
CC              RMSAM   : RMS OF SEMI MAJOR AXIS ADJUSTMENT   R*8
CC              NUMSTA  : NUMBER OF CONTRIBUTING STATIONS     I*4
CC
CC REMARKS    : ---
CC
CC AUTHOR     :  S.FANKHAUSER, E.BROCKMANN
CC
CC VERSION    :  3.3
CC
CC CREATED    :  20-OCT-92
CC
CC CHANGES    :  17-AUG-94 : MR: SESSION AS CHARACTER*4
CC               24-MAR-95 : MR: NEW TITLE FOR ADDNEQ OUTPUT
CC               08-APR-95 : MR: SESSION CHANGED TO 4 CHARACTERS
CC               10-APR-95 : MR: ALLOW CAMPAIGN NAME NOT EQUAL 8 CHAR.
CC               13-MAI-95 : EB: IGNORE ERROR MESSAGE LINES
CC               03-JAN-96 : SS: RETURN "IPGM"
CC               17-SEP-96 : TS: CHECK FOR "*" BEFORE READING RMS
CC               06-AUG-97 : SS: EXTRACT NUMBER OF CONTRIBUTING STATIONS
CC               30-OCT-97 : SS: SIGMA OF ZERO-DIFFERENCE OBS
CC               14-MAR-00 : SS: CONSIDER ADDNEQ2 OUTPUT FILES
CC               21-JUL-00 : SS: EXTRACT DOY OF ADDNEQ2 CORRECTLY
CC               11-AUG-00 : SS: BUG WRT EXTRACTION OF DOY FIXED
CC               14-AUG-01 : HU: READING OF ADDNEQ2 V5.0 OUTPUT IMPLEMENTED
CC               03-SEP-01 : SS: BUG WRT EXTRACTION OF DOY FIXED
CC               12-NOV-02 : CU: CHANGE READING OF ADDNEQ2 V5.0 OUTPUT
CC                               ACCORDING TO NEW OUTPUT FORMAT
CC               13-NOV-02 : HU: EXTRACT NUMBER OF ADJUSTED ORBIT PARAMETERS
CC               11-DEC-02 : CU: GPSEST V5.0 OUTPUT
CC               03-APR-03 : RD: GET SESSION FOR ADDNEQ2 FROM PRITIT SECTION
CC               06-MAY-03 : MM: SOME ADAPTIONS DUE TO NEW ADDNEQ2 OUTPUT
CC               27-MAY-03 : CU: SOME ADAPTIONS DUE TO NEW ADDNEQ2 OUTPUT
CC               02-JUN-03 : CU: BUG FIXED (ADDNEQ2: REWIND FOR NORB=0)
CC               22-OCT-03 : RD: CORRECT END OF ORBIT PART IN GPSEST
CC               07-SEP-04 : RD: ENABLE GPSEST OUTPUT FOR V5.1
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               12-DEC-05 : CU: CHANGE ORDER OF SEARCH STRINGS
CC               08-FEB-07 : RD: NEQ%MISC%NOBS/NPARMS I4B->R8B
CC               23-MAR-07 : RD: BIG NUMOBS AND NPAR (****** -> 999999)
CC               21-JUN-07 : RD: ADAPT THE READING RANGE FOR NPAR IN GPSEST
CC               18-JAN-11 : DT: CORRECT FORMAT STATEMENT FOR MAX. A IN GPSEST
CC               24-JAN-12 : RD: SWITCH TO VERSION 5.2, USE M_BERN WITH ONLY
CC               11-JUN-12 : RD: ADAPT TO NEW PRITIT
CC               26-SEP-12 : RD: DETECT IF SOLUTION WAS SKIPPED
CC               05-OCT-12 : RD: ADDNEQ2 PARAM-STATISTICS ALSO IF NO SOLUTION
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,  ONLY: lfnerr, lfnloc
      USE s_opnfil
      USE s_opnerr
      USE s_findln
      USE f_lengt0
      USE f_lengt1
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I1     , I2     , I3     , ICHR   , ICOR   , IERR   ,
     1          IMIDSES, IOSTAT , IPGM   , IPOS   , IRC    , IREQ   ,
     2          ISAT   , ISATM  , ISES   , ISES0  , ISESS0 , ITYP   ,
     3          NORB   , NPAR   , NREQ   , NSES   , IDX    , IOS    ,
     4          NUMFIL , NUMOBS , NUMSTA , ILOOK
C
      REAL*8    AMAX   , DA     , RMS    , RMSA   , RMSAM  , RHELP
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C DECLARATIONS
C ------------
C
C GLOBAL DECLARATIONS
C -------------------
      CHARACTER   FILNAM*(*),TITLE*(*)
      REAL*8      POLCOR(3,10,100)
C
C INTERNAL DECLARATIONS
C ---------------------
      CHARACTER*3   DOYSTR
      CHARACTER*80  TOTFIL
      CHARACTER*132 LINE,LOOK,LOOK2(2)
C
      NREQ=0
      NUMSTA=0
      IMIDSES=0
C
      ISATM=0
      AMAX=0.D0
      RMSAM=0.D0
C
      DO 121 I1=1,3
        DO 122 I2=1,10
          DO 123 I3=1,100
            POLCOR(I1,I2,I3)=0.D0
123       CONTINUE
122     CONTINUE
121   CONTINUE
C
C OPEN THE OUTPUT FILE
C --------------------
      TOTFIL=FILNAM
      CALL OPNFIL(LFNLOC,TOTFIL,'OLD','FORMATTED',
     1            'READONLY',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNLOC,IOSTAT,TOTFIL,'EXTGPS')
C
C EXTRACT THE TITLE
C -----------------
10    READ(LFNLOC,'(A)',END=910) LINE
C GPSEST OUTPUT (V5.0 or later)
      IF (LINE(2:17).EQ.'Program : GPSEST' .OR.
     1    LINE(2:25).EQ.'Program        : GPSEST') THEN
        IPGM=11
        ITYP=0
C GPSEST OUTPUT (before V5.0)
      ELSE IF (LINE(101:114).EQ.'PROGRAM GPSEST') THEN
        IPGM=1
        ITYP=0
      ELSEIF (LINE(101:114).EQ.'PROGRAM ADDNEQ') THEN
C ADDNEQ OUTPUT
        IPGM=2
        ITYP=1
      ELSEIF (LINE(2:20).EQ.'ADDNEQ2 output file') THEN
C ADDNEQ2 OUTPUT
        IPGM=3
      ELSEIF (LINE(2:18).EQ.'Program : ADDNEQ2' .OR.
     1        LINE(2:26).EQ.'Program        : ADDNEQ2') THEN
C ADDNEQ2 OUTPUT (V5.0)
        IPGM=13
      ELSE
        GOTO 10
      ENDIF
C
C EXTRACT INFROMATION FROM ADDNEQ2 OUTPUT FILE
C --------------------------------------------
      IF (IPGM.EQ.3) THEN
        READ(LFNLOC,'(/,A)',END=900) TITLE
C
        LOOK='INPUT_FILES'
        CALL FINDLN(LFNLOC,1,0,'EXTGPS',TOTFIL,LOOK,1,LINE,IERR)
C
        ISES0=-1
        LOOK='.NQ0'
        IPOS=0
301     CALL FINDLN(LFNLOC,0,1,'EXTGPS',TOTFIL,LOOK,IPOS,LINE,IERR)
        IF (IERR.EQ.0) THEN
          DOYSTR=LINE(IPOS-3:IPOS-1)
          READ(DOYSTR,'(I3)',ERR=301) ISES
          IF (ISES0.EQ.-1) ISES0=ISES
          GOTO 301
        ENDIF
        IF (ISES0.NE.-1) IMIDSES=(ISES0+ISES)/2
C
        LOOK=' Orbits'

        CALL FINDLN(LFNLOC,1,0,'EXTGPS',TOTFIL,LOOK,1,LINE,IERR)
        READ(LINE(30:35),'(I6)',ERR=900) NORB
CC
C
        LOOK=' Number of Observation Files'
        CALL FINDLN(LFNLOC,1,0,'EXTGPS',TOTFIL,LOOK,1,LINE,IERR)
        READ(LINE(34:45),'(I12)',ERR=900) NUMFIL


C
        LOOK=' Number of Observations'
        CALL FINDLN(LFNLOC,1,0,'EXTGPS',TOTFIL,LOOK,1,LINE,IERR)
C        READ(LINE(34:45),'(I12)',ERR=900) NUMOBS
        READ(LINE(34:45),*,ERR=900) RHELP
        IF (RHELP .GT. 999999999) THEN
          NUMOBS = 999999999
        ELSE
          NUMOBS = NINT(RHELP)
        ENDIF


C
        LOOK=' Number of Parameters'
        CALL FINDLN(LFNLOC,1,0,'EXTGPS',TOTFIL,LOOK,1,LINE,IERR)
C        READ(LINE(34:45),'(I12)',ERR=900) NPAR
        READ(LINE(34:45),*,ERR=900) RHELP
        IF (RHELP .GT. 999999999) THEN
          NPAR = 999999999
        ELSE
          NPAR = NINT(RHELP)
        ENDIF


C
        LOOK=' A posteriori RMS of Unit Weight'
        CALL FINDLN(LFNLOC,1,0,'EXTGPS',TOTFIL,LOOK,1,LINE,IERR)
        READ(LINE(34:45),'(F12.4)',ERR=900) RMS



C
        LOOK=' Arc Number'
        IF (NORB.GT.0) THEN
          CALL FINDLN(LFNLOC,1,0,'EXTGPS',TOTFIL,LOOK,1,LINE,IERR)
          READ(LINE,'(19X,I3,10X,F9.5,3X,F9.5)',ERR=900)
     1      ISATM,AMAX,RMSAM
C
302       CALL FINDLN(LFNLOC,0,1,'EXTGPS',TOTFIL,LOOK,1,LINE,IERR)
          IF (IERR.EQ.0) THEN
            IF (LINE(24:24).EQ.'A') THEN
              READ(LINE,'(19X,I3,10X,F9.5,3X,F9.5)',ERR=900)
     1          ISAT,DA,RMSA
              IF (ABS(DA).GT.ABS(AMAX)) THEN
                ISATM=ISAT
                AMAX=DA
                RMSAM=RMSA
              ENDIF
            ENDIF
            GOTO 302
          ENDIF
        ENDIF
C
        CLOSE(LFNLOC)
        IRC=0
        GOTO 999
C
C EXTRACT INFROMATION FROM ADDNEQ2 OUTPUT FILE (V5.0)
C --------------------------------------------
      ELSEIF (IPGM.EQ.13) THEN
        LOOK='Default session:'
        IDX=0
        CALL FINDLN(LFNLOC,1,0,'EXTGPS',TOTFIL,LOOK,IDX,LINE,IERR)
        IF (IDX.EQ.100) THEN
          READ(LINE,'(116X,I3)',IOSTAT=IOS) ISESS0
          READ(LFNLOC,'(////,1X,A)',END=900) TITLE
        ELSE
          READ(LINE,'(18X,I3)',IOSTAT=IOS) ISESS0
          READ(LFNLOC,'(/////,1X,A)',END=900) TITLE
        ENDIF
C
        LOOK='INPUT NORMAL EQUATION FILES'
        CALL FINDLN(LFNLOC,1,0,'EXTGPS',TOTFIL,LOOK,2,LINE,IERR)
        READ(LFNLOC,'(////)',END=900)
C
C GET DOY FROM THE NAMES OF THE NEQ-FILES IN THE LIST
        ISES0=-1
        LOOK='.NQ0'
        IPOS=0
401     CALL FINDLN(LFNLOC,0,1,'EXTGPS',TOTFIL,LOOK,IPOS,LINE,IERR)
        IF (IERR.EQ.0) THEN
          DOYSTR=LINE(IPOS-3:IPOS-1)
          READ(DOYSTR,'(I3)',ERR=401) ISES
          IF (ISES0.EQ.-1) ISES0=ISES
          GOTO 401
        ENDIF
        IF (ISES0.NE.-1) IMIDSES=(ISES0+ISES)/2
C
C TAKE THE DAY FROM THE TITLE SECTION
        IF (ISESS0.NE.0) IMIDSES=ISESS0
C
c       Find number of orbit parameters
        LOOK=' SUMMARY OF RESULTS'
        CALL FINDLN(LFNLOC,1,0,'EXTGPS',TOTFIL,LOOK,1,LINE,IERR)
        LOOK=' Orbital elements'
        CALL FINDLN(LFNLOC,0,0,'EXTGPS',TOTFIL,LOOK,1,LINE,IERR)
        IF (IERR.EQ.0) THEN
          READ(LINE(54:66),'(I13)',ERR=900) NORB
        ELSE
          NORB=0
          REWIND LFNLOC
          LOOK=' SUMMARY OF RESULTS'
          CALL FINDLN(LFNLOC,1,0,'EXTGPS',TOTFIL,LOOK,1,LINE,IERR)
        ENDIF
C
        LOOK2(1)=' Total number of observations'
        LOOK2(2)=' Solution skipped ...'
        CALL FINDLN(LFNLOC,1,0,'EXTGPS',TOTFIL,LOOK2,1,ILOOK,LINE,IERR)
        IF ( ILOOK == 2 ) THEN
          RMS = 1D20
          CLOSE(LFNLOC)
          IRC = 0
          GOTO 999
        ENDIF
C        READ(LINE(40:51),'(I12)',ERR=900) NUMOBS
        READ(LINE(40:51),*,ERR=900) RHELP
        IF (RHELP .GT. 999999999) THEN
          NUMOBS = 999999999
        ELSE
          NUMOBS = NINT(RHELP)
        ENDIF
C
        LOOK2(1)=' Total number of adjusted parameters'
        LOOK2(2)=' Total number of parameters'
        CALL FINDLN(LFNLOC,1,0,'EXTGPS',TOTFIL,LOOK2,1,ILOOK,LINE,IERR)
C        READ(LINE(40:51),'(I12)',ERR=900) NPAR
        READ(LINE(40:51),*,ERR=900) RHELP
        IF (RHELP .GT. 999999999) THEN
          NPAR = 999999999
        ELSE
          NPAR = NINT(RHELP)
        ENDIF
C
        LOOK2(1)=' A posteriori RMS of unit weight'
        LOOK2(2)=' Total number of observation files'
        CALL FINDLN(LFNLOC,1,0,'EXTGPS',TOTFIL,LOOK2,1,ILOOK,LINE,IERR)
        IF ( ILOOK == 2 ) THEN
          RMS = 1D20
          READ(LINE(40:51),'(I12)',ERR=900) NUMFIL
          CLOSE(LFNLOC)
          IRC = 0
          GOTO 999
        ENDIF
        READ(LINE(40:51),'(F12.5)',ERR=900) RMS
C
        LOOK=' Total number of observation files'
        CALL FINDLN(LFNLOC,1,0,'EXTGPS',TOTFIL,LOOK,1,LINE,IERR)
        READ(LINE(40:51),'(I12)',ERR=900) NUMFIL
C
        LOOK=' Arc PRN Parameter'
        IF (NORB.GT.0) THEN
          CALL FINDLN(LFNLOC,1,0,'EXTGPS',TOTFIL,LOOK,1,LINE,IERR)
          READ(LFNLOC,'(/,5X,I3,34X,F15.4,2X,F9.4)',ERR=900)
     1      ISATM,AMAX,RMSAM
C
402       READ(LFNLOC,'(A)',ERR=900) LINE
          IF (LEN_TRIM(LINE).GT.0) THEN
            IF (LINE(10:10).EQ.'A') THEN
              READ(LINE,'(5X,I3,34X,F15.4,2X,F9.4)',ERR=900)
     1          ISAT,DA,RMSA
              IF (ABS(DA).GT.ABS(AMAX)) THEN
                ISATM=ISAT
                AMAX=DA
                RMSAM=RMSA
              ENDIF
            ENDIF
            GOTO 402
          ENDIF
        ENDIF
C
        CLOSE(LFNLOC)
        IRC=0
        GOTO 999
      ELSE
        READ(LINE(1:80),'(A)') TITLE
      ENDIF
C
C EXTRACT NUMFIL
C --------------
      DO 20 I1=1,1000000
        READ(LFNLOC,'(A)',END=900)LINE
        IF (ITYP.EQ.0) THEN
          IF (LINE(1:22).EQ.' COMBINATION OF NORMAL')THEN
            ITYP=1
            TITLE=' '
            GOTO 31
          ENDIF
          IF (LINE(1:61).EQ.
     1  ' FILE TYP FREQ.  STATION 1        STATION 2        SESS  FIRS')
     2    GOTO 30
        ELSE
          IF (LINE(1:16).EQ.' FILE  FILE NAME') GOTO 31
        ENDIF
20    CONTINUE
C
C READING FOR GPSEST
30    CONTINUE
      READ(LFNLOC,'(A)',END=900)LINE
      READ(LFNLOC,'(A)',END=900)LINE
      IMIDSES = 0
      NSES = 0
      DO 40 I1=1,1000
        READ(LFNLOC,'(A)',END=900)LINE
        IF(LINE(2:5).EQ.'   ') GOTO 50
        READ(LINE(2:5),*,END=50)NUMFIL
        READ(LINE(51:54),*,END=50)ISES
        IF (ISES.NE.IMIDSES) THEN
          IMIDSES = ISES
          NSES    = NSES + 1
        ENDIF
40    CONTINUE
50    CONTINUE
      IMIDSES = IMIDSES - (NSES-1)*1.0/2.0
C
C EXTRACT NUMBER OF CONTRIBUTING STATIONS FROM GPSEST OUTPUT
      DO 55 I1=1,1000000
        READ(LFNLOC,'(A)',END=900) LINE
        IF (LINE(1:32).EQ.' NUM  STATION NAME     OBS E/F/C') GOTO 56
        IF (LINE(1:32).EQ.' num  Station name     obs e/f/h') GOTO 56
55    CONTINUE
C
56    IF (IPGM .EQ. 1) THEN
        READ(LFNLOC,'(/)',END=900)
      ELSE
        READ(LFNLOC,*,END=900)
      ENDIF
      DO 57 I1=1,1000000
        READ(LFNLOC,'(A)',END=900) LINE
        IF (LINE(25:25).EQ.'Y') NUMSTA=NUMSTA+1
        IF (LINE.EQ.' ') GOTO 32
57    CONTINUE
C
      GOTO 32
C
C READING FOR ADDNEQ
31    CONTINUE
      READ(LFNLOC,'(A)',END=900)LINE
      READ(LFNLOC,'(A)',END=900)LINE
      IMIDSES = 0
      NSES = 0
      DO 41 I1=1,1000
        READ(LFNLOC,'(A)',END=900)LINE
        IF(LINE(2:5).EQ.'----') GOTO 51
        IF(LINE(2:5).EQ.'    ') GOTO 41
        IF(LINE(5:7).NE.'   ') GOTO 41
        IF(LINE(2:4).EQ.'-->') GOTO 41
C
C FIND ".NEQ"
        DO 52 ICHR=1,LENGT0(LINE)-3
          IF (LINE(ICHR:ICHR+3).EQ.'.NEQ') GOTO 53
52      CONTINUE
C
        WRITE(LFNERR,*)' ERROR READING NEQ-FILE'
        WRITE(LFNERR,*)' LINE: ',LINE(1:LENGT1(LINE))
        IRC=2
        GOTO 999
C
53      READ(LINE(ICHR-3:ICHR-1),*,ERR=51,END=51)ISES
        IF (ISES.NE.IMIDSES) THEN
          IMIDSES = ISES
          NSES    = NSES + 1
        ENDIF
41    CONTINUE
51    CONTINUE
      IMIDSES = IMIDSES - (NSES-1)*1.0/2.0
C IF NOT YET NUMBER OF FILES IN OUTPUT TAKE NUMBER OF STATIONS INSTEAD
      DO 44 I1=1,1000
        READ(LFNLOC,'(A)',END=900)LINE
        IF(LINE(1:21).EQ.' TOTAL NUMBER OF STAT')THEN
          READ(LINE(1:80),'(29X,I3)')NUMSTA
          NUMFIL=NUMSTA
          GOTO 32
        ENDIF
44    CONTINUE
C
32    CONTINUE
C
C EXTRACT TOTAL NUMBER OF PARAMETERS
C ----------------------------------
      DO 60 I1=1,1000000
        READ(LFNLOC,'(A)',END=900)LINE
        IF (LINE(1:46).EQ.
     1  ' TOTAL NUMBER OF PARAMETERS                  ')
     2  GOTO 70
60    CONTINUE
70    CONTINUE
      READ(LINE(51:59),*,END=900)NPAR
C
C EXTRACT TOTAL NUMBER OF OBSERVATIONS
C ------------------------------------
      DO 80 I1=1,1000000
        READ(LFNLOC,'(A)',END=900)LINE
        IF (LINE(1:29).EQ.' TOTAL NUMBER OF OBSERVATIONS') GOTO 90
80    CONTINUE
90    CONTINUE
      READ(LINE(51:59),*,END=900)NUMOBS
C
C EXTRACT TOTAL NUMBER OF SINGLE DIFF FILES IN CASE OF ADDNEQ
C -----------------------------------------------------------
      IF (ITYP.EQ.0) GOTO 43
      DO 42 I1=1,1000
        READ(LFNLOC,'(A)',END=900)LINE
        IF (LINE(1:40).EQ.
     1  ' ---------------------------------------')
     2  GOTO 43
        IF(LINE(1:29).EQ.' NUMBER OF SINGLE DIFF. FILES')THEN
          READ(LINE(51:59),*,END=900)NUMFIL
          GOTO 43
        ENDIF
42    CONTINUE
43    CONTINUE
C
C EXTRACT SIGMA SINGLE DIFF OBS
C -----------------------------
      DO 100 I1=1,1000000
        READ(LFNLOC,'(A)',END=900)LINE
        IF (LINE(1:20).EQ.' SOLUTION SKIPPED...') THEN
          RMS=1d20
          CLOSE(LFNLOC)
          IRC = 0
          GOTO 999
        ENDIF
        IF (LINE(1:37).EQ.
     1  ' A POSTERIORI SIGMA OF UNIT WEIGHT  :')
     2  GOTO 110
100   CONTINUE
110   CONTINUE
      IF (LINE(39:39).EQ.'*') THEN
        RMS=0.9999
      ELSE
        READ(LINE(39:47),*,END=900)RMS
      ENDIF
C
C EXTRACT MAXIMUM SEMI MAJOR AXIS ADJUSTMENT
C -------------------------------------------
      DO 1110 I1=1,1000000
        READ(LFNLOC,'(A)',END=900)LINE
        IF (LEN_TRIM(LINE).EQ.0) THEN
          READ(LFNLOC,'(A)',END=900)LINE
        ENDIF
CC        IF (LEN_TRIM(LINE).EQ.0) GOTO 1130
        IF (LINE(1:12).EQ.' TROPOSPHERE') GOTO 1130
        IF (LINE(1:19).EQ.' ARC  SAT.  ELEMENT') THEN
          READ(LFNLOC,'(A)',END=900)LINE
          READ(LFNLOC,'(A)',END=900)LINE
          READ(LFNLOC,'(A)',END=900)LINE
          READ(LINE,10000)ISATM,AMAX,RMSAM
10000     FORMAT(6X,I3,14X,F9.2,4X,F11.4)
          GOTO 1111
        ENDIF
1110  CONTINUE
1111  CONTINUE
      DO 1120 I1=1,1000000
        READ(LFNLOC,'(A)',END=900)LINE
        IF (LEN_TRIM(LINE).EQ.0) THEN
          READ(LFNLOC,'(A)',END=900)LINE
        ENDIF
        IF (LEN_TRIM(LINE).EQ.0) GOTO 1130
        IF (LINE(1:12).EQ.' TROPOSPHERE') GOTO 1130
        IF (LINE(13:15).EQ.' A ') THEN
          READ(LINE,10000)ISAT,DA,RMSA
          IF (ABS(DA).GT.ABS(AMAX)) THEN
            ISATM = ISAT
            AMAX  = DA
            RMSAM = RMSA
          ENDIF
        ENDIF
1120  CONTINUE
C
C END MAXIMUN SEMI MAJOR AXIS EXTRACT
C -----------------------------------
1130  CONTINUE
C
C EXTRACT POLE CORRECTIONS
C ------------------------
      DO 120 I1=1,1000000
        READ(LFNLOC,'(A)',END=900)LINE
        IF (LINE(1:53).EQ.
     1  ' CRD. REQ.     (")        RMS        (")/DAY      RMS')
     2  GOTO 130
120   CONTINUE
130   CONTINUE
      DO 140 I1=1,3
        READ(LFNLOC,'(A)',END=900)LINE
140   CONTINUE
      DO 150 I1=1,1000000
        READ(LFNLOC,'(A)',END=900)LINE
        IF ((LINE(3:4).EQ.'X ').OR.
     1      (LINE(3:4).EQ.'Y ').OR.
     2      (LINE(3:4).EQ.'DT')) THEN
          ICOR=0
          IF (LINE(3:4).EQ.'X ') ICOR=1
          IF (LINE(3:4).EQ.'Y ') ICOR=2
          IF (LINE(3:4).EQ.'DT') ICOR=3
          READ(LINE(7:8),*,END=145) IREQ
          READ(LINE(9:21),*,END=145) POLCOR(ICOR,1,IREQ)
          READ(LINE(22:32),*,END=145) POLCOR(ICOR,2,IREQ)
          READ(LINE(34:46),*,END=145) POLCOR(ICOR,3,IREQ)
          READ(LINE(47:57),*,END=145) POLCOR(ICOR,4,IREQ)
          READ(LINE(59:71),*,END=145) POLCOR(ICOR,5,IREQ)
          READ(LINE(72:82),*,END=145) POLCOR(ICOR,6,IREQ)
          READ(LINE(84:96),*,END=145) POLCOR(ICOR,7,IREQ)
          READ(LINE(97:107),*,END=145) POLCOR(ICOR,8,IREQ)
          READ(LINE(109:121),*,END=145) POLCOR(ICOR,9,IREQ)
          READ(LINE(122:132),*,END=145) POLCOR(ICOR,10,IREQ)
        END IF
145     IF(LINE(1:1).NE.' ')GOTO 900
150   CONTINUE
C
900   CONTINUE
      NREQ=IREQ
      CLOSE(LFNLOC)
      IRC=0
      GOTO 999
C
910   WRITE(LFNERR,920) TOTFIL
920   FORMAT(/,' ### SR EXTGPS: NEITHER GPSEST NOR ADDNEQ ',
     1  'OUTPUT FILE',
     2  /,16X,'FILE NAME:  ',A,/)
      IPGM=0
C
999   RETURN
      END SUBROUTINE

      END MODULE
