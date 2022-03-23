      MODULE s_RDPOL
      CONTAINS

C*
      SUBROUTINE RDPOL(LFN   ,IERB1D,IFORM ,CODTIT,POLTYP,POLTIM,POLCOO,
     1                 POLRAT,GPSUTC,REM   ,RMSPOL,RMSRAT,IEND  ,
     2                 NUTNAM,SUBNAM)
CC
CC TITLE      :  READ POLE FILES OF DIFFERENT FORMAT TYPES
CC
CC PURPOSE    :  READ THE INFORMATION OF POLE FILES. 11 DIFFERENT
CC               POLE FORMATS ARE KNOWN. (SEE IFORM).
CC               ALWAYS ONE OBSERVATION (OR HEADER) WILL BE READ.
CC               (= INFORMATION OF ONE LINE OF THE POLE FILE )
CC               IF A VALUE IS NOT PRESENT THEN IT WILL BE SET
CC               TO 0.D0.
CC
CC PARAMETER  :
CC        IN  :  LFN     : LOGICAL FILE NUMBER                  I*4
CC               IERB1D  : IF INPUT FILE = BULLETIN B  THEN     I*4
CC                          1= ONE DAY VALUES
CC                          5= 5 DAY VALUES
CC    IN/OUT  :  IFORM   : FORMAT OF POLE FILE                  I*4
CC                          0 = UNKNOWN FORMAT
CC                          1 = BULLETIN B
CC                          2 = BULLETIN A
CC                          3 = C04 (Valid until Oct. 2007)
CC                          4 = CODE SOLUTION IGS FORMAT
CC                          5 = CODE SOLUTION OLD FORMAT
CC                          6 = CODE SOLUTION NEW FORMAT
CC                        ( 7 = GFZ SOLUTION )
CC                          8 = IGS SOLUTION
CC                          9 = JPL SOLUTION
CC                         10 = NGS SOLUTION
CC                         11 = SIO SOLUTION
CC                         12 = CODE SOLUTION NEW IGS FORMAT
CC                         13 = IGS SOLUTION NEW IGS FORMAT
CC                         14 = GSI GIPSY FORMAT
CC                         15 = CODE SOLUTION IGS FORMAT 'VERSION 2'
CC                         16 = 'VERSION 2' WITH UT1R-UTC, LODR
CC                         17 = 'VERSION 2' WITH UT1R-TAI, LODR
CC                         18 = 'VERSION 2' WITH UT1-TAI, LOD
CC                         19 = SAME AS IFORM=2 BUT OTHER FORMAT
CC                         20 = A NEW C04 FORMAT
CC       OUT  :  CODTIT  : TITLE OF INPUT FILE WITH BERNESE    CH*80
CC                         POLE FORMAT
CC               POLTYP(I): I=1 NUTATION MODEL                  I*4(*)
CC                              1=NO, 2=OBSERVED, 3=HERRING
CC                          I=2 SUBDAILY POLE MODEL
CC                              1=NO, 2=RAY
CC               POLTIM  : TIME OF PARAMETER SET
CC               POLCOO(I): (I=1,5); POLE COORDINATES           R*8(5)
CC                         I=1 :VALUE OF X-POLE (ARC SECONDS)
CC                         I=2 :VALUE OF Y-POLE (ARC SECONDS)
CC                         I=3 :VALUE OF UT1-UTC (SECONDS)
CC                         I=4 :DELTA EPSILON (ARC SECONDS)
CC                         I=5 :DELTA PSI (ARC SECONDS)
CC               POLRAT(I): (I=1,5); POLE RATES                 R*8(5)
CC                         I=1 :RATE VALUE OF X-POLE (ARC SEC/D)
CC                         I=2 :RATE VALUE OF Y-POLE (ARC SEC/D)
CC                         I=3 :RATE VALUE OF UT1-UTC (SEC/D)
CC                         I=4 :RATE DELTA EPSILON (ARC SEC/D)
CC                         I=5 :RATE DELTA PSI (ARC SEC/D)
CC               GPSUTC  : GPS-UTC DIFFERENC GIVEN IN SECONDS   R*8
CC               REM     : REMARK                              CH*3
CC               RMSPOL(I): (I=1,5) RMS OF POLE COORDINATES     R*8(5)
CC                         I=1 :RMS OF X-POLE (ARC SECONDS)
CC                         I=2 :RMS OF Y-POLE (ARC SECONDS)
CC                         I=3 :RMS OF UT1-UTC (SECONDS)
CC                         I=4 :RMS OF DELTA EPS (ARC SEC)
CC                         I=5 :RMS OF DELTA PSI (ARC SEC)
CC               RMSRAT(I): (I=1,5); RMS OF POLE RATES          R*8(5)
CC                         I=1 :RMS OF X-POLE RATE (ARC SEC/D)
CC                         I=2 :RMS OF Y-POLE RATE (ARC SEC/D)
CC                         I=3 :RMS OF UT1-UTC RATE (SEC/D)
CC                         I=4 :RMS OF DELTA EPSILON RATE (ARC SEC/D)
CC                         I=5 :RMS OF DELTA PSI RATE (ARC SEC/D)
CC               IEND    : =1 : END OF FILE REACHED             I*4
CC                         =2 : ERROR READING POLE RECORD
CC                         =3 : UNKNOWN FORMAT OF POLE FILE
CC                         =0 : ELSE
CC               NUTNAM  : NUTATION MODEL NAME                  CH*16
CC               SUBNAM  : SUBDAILY POLE MODEL NAME             CH*16
CC
CC REMARKS    :  NUTNAM AND SUBNAM ARE BLANK IF NOT AVAILABLE
CC
CC AUTHOR     :  S.FANKHAUSER
CC
CC VERSION    :  3.5
CC
CC CREATED    :  26-APR-94
CC
CC CHANGES    :  22-JUN-94 : SF: NEW IGS POLE FORMAT INCLUDED
CC               13-OCT-94 : MR: REPLACE "X" BY "1X" IN FORMAT
CC                               DECLARATION OF "IND"
CC               05-FEB-95 : EB: 13: READ ALSO IGS ORBIT FORMAT
CC               04-JUN-96 : TS: ADDED SUBDAILY POLE MODEL
CC               04-FEB-97 : JJ: MR: NEW GSI GIPSY FORMAT WITH RATES
CC                                   ADD "POLRAT" TO PARAMETER LIST
CC               01-MAR-98 : MR: ADD "RMSRAT" TO PARAMETER LIST
CC               16-JUL-98 : DI: ADD IGS POLE FORMAT 'VERSION 2' (15)
CC               22-JUL-98 : JJ: NEW BULLETIN B FORMAT
CC               23-JUL-98 : MR: ALL VERSION 2 FORMATS TOGETHER
CC               17-DEC-01 : HU: GFZ VERSION 2 FORMAT
CC               05-OCT-02 : HU: CONFORMING TO VERSION 2 FORMAT
CC               09-JAN-03 : PS: RDPOLH CALL CHANGED
CC               12-AUG-03 : MR: ADD INITIALIZATION OF "ICOL"
CC               01-NOV-03 : HU: ADDITIONAL PARAMETERS SUBNAM, NUTNAM
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               08-NOV-06 : MM: NEW C04 FORMAT IMPLEMENTED
CC               22-DEC-06 : SS: AGAIN OLD C04 FORMAT
CC               22-JAN-07 : AG: ANOTHER POLE FORMAT IMPLEMENTED (19)
CC               25-OCT-07 : LO/RD: NEW C04 FORMAT (FORMAT 20)
CC               14-SEP-09 : SL: READ FORMAT FOR BULLETIN A CHANGED
CC               19-JUL-10 : SL: TAB CHARACTERS REMOVED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1994     UNIVERSITY OF BERN
CC               SWITZERLAND
C*
      USE m_bern
      USE s_ut1red
      USE s_rdpolh
      USE s_rdpoli
      USE s_fmtpol
      USE f_dgpsut
      USE f_poloff
      USE s_rdiepi
      USE f_lengt0
      USE s_lodred
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I1    , I3    , IDAY  , IENCOD, IEND  , IERB1D,
     1          IFIRST, IFOCOD, IFORM , IHOUR , ILODSG, IOS   , IRCODE,
     2          IVERS , IYEAR , LFN   , MIN   , MJD   , MONTH ,
     3          NF    , NR    , NT
C
      REAL*8    AKORR , GPSUTC, POLTIM, UTAT
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C GLOBAL DECLARATIONS
C -------------------
      CHARACTER*80 CODTIT
      CHARACTER*16 NUTNAM,SUBNAM
      CHARACTER*3  REM
C
      REAL*8       POLCOO(5),POLRAT(5),RMSPOL(5),RMSRAT(5)
      REAL*8       POLCOR(10)
C
      INTEGER*4    POLTYP(*),NRFPOL(3)
C
C LOCAL DECLARATIONS
C ------------------
      CHARACTER*256 POLINE
      CHARACTER*3   BBREM,MNTH,MNTHO
      CHARACTER*1   IND,IND1
C
      INTEGER*4     IPLCOO(5),IPLRAT(5),IRMSPL(5),IRMSRA(5)
      INTEGER*4     ICOL(11)
C
C
C DATA STATEMENTS
C ---------------
      DATA IFIRST/1/
C
C INITIALIZATION OF THE VARIABLES
C -------------------------------
      IF (IFIRST.EQ.1) THEN
        CODTIT=' '
        NUTNAM=' '
        SUBNAM=' '
        POLTYP(1)=1
        POLTYP(2)=1
        IFIRST=0
      ENDIF
      IEND=0
      IYEAR=0
      MONTH=0
      IDAY=0
      IHOUR=0
      MIN=0
      GPSUTC=0.D0
      POLTIM=0.D0
      REM=' '
      IFOCOD=0
      IENCOD=0
      DO 10 I1=1,5
        POLCOO(I1)=0.D0
        RMSPOL(I1)=0.D0
10    CONTINUE
C
C GET THE POLE FORMAT AND SET THE POINTER TO THE FIRST OBS. RECORD
C ----------------------------------------------------------------
      IF (IFORM.EQ.0) THEN
        CALL FMTPOL(LFN,0,IFORM,IVERS,ICOL,NUTNAM,SUBNAM)
        IF (IFORM.EQ.0) GOTO 920
        IF (IFORM.EQ.1.AND.IERB1D.EQ.1) THEN
          DO 30 I1=1,10000
            READ(LFN,11,END=920)POLINE
11          FORMAT(A)
            IF (LENGT0(POLINE).GT.70.AND.POLINE(14:54).EQ.
     1          'MJD       x        y    UT1-UTC  UT1-UT1R')THEN
              READ(LFN,11)
              MNTHO='XXX'
              BBREM='B11'
              GOTO 40
            ENDIF
30        CONTINUE
40        CONTINUE
        ELSE IF (IFORM.EQ.1.AND.IERB1D.EQ.5) THEN
          BBREM='B5F'
        ELSE IF (IFORM.EQ.5.OR.IFORM.EQ.6) THEN
          REWIND LFN
          CALL RDPOLH(LFN,1,CODTIT,POLTYP,IFOCOD,IENCOD,NUTNAM,SUBNAM)
          IF (IENCOD.GT.0) GOTO 910
        ENDIF
      ELSE
        DO I=1,11
          ICOL(I)=12+I
        ENDDO
      ENDIF
C
C READ FORMAT OF BULLETIN B  (5 DAY VALUES)=  1
C ---------------------------------------------
      IF (IFORM.EQ.1.AND.IERB1D.EQ.5) THEN
        DO 50 I1=1,1000
          READ(LFN,11,END=920)POLINE
          IF (LENGT0(POLINE).GT.50.AND.POLINE(20:20).EQ.'.'.AND.
     1        POLINE(28:28).EQ.'.'.AND.POLINE(36:36).EQ.'.')THEN
            READ(POLINE,51,ERR=910) MJD,POLCOO(1),POLCOO(2),POLCOO(3),
     1                              UTAT,POLCOO(5),POLCOO(4)
51          FORMAT(9X,I7,2F8.4,F9.5,F12.5,2F9.1)
            POLTIM=MJD*1.D0
            GPSUTC=DNINT(POLCOO(3)-UTAT-19)
            CALL UT1RED(POLTIM,AKORR)
            POLCOO(1)=POLCOO(1)+POLOFF(POLTIM,1)
            POLCOO(2)=POLCOO(2)+POLOFF(POLTIM,2)
            POLCOO(3)=POLCOO(3)+AKORR/1000.D0+POLOFF(POLTIM,3)
            POLCOO(4)=POLCOO(4)/1000.D0+POLOFF(POLTIM,5)
            POLCOO(5)=POLCOO(5)/1000.D0+POLOFF(POLTIM,4)
            REM=BBREM
            GOTO 999
          ELSE IF (LENGT0(POLINE).GT.50.AND.POLINE(20:20).EQ.'.'.AND.
     1        POLINE(28:28).EQ.'.'.AND.POLINE(37:37).EQ.'.')THEN
            READ(POLINE,52,ERR=910) MJD,POLCOO(1),POLCOO(2),POLCOO(3),
     1                              UTAT,POLCOO(5),POLCOO(4)
52          FORMAT(9X,I7,F9.5,F8.5,F10.6,F12.6,F8.1,F8.1)
            POLTIM=MJD*1.D0
            GPSUTC=DNINT(POLCOO(3)-UTAT-19)
            CALL UT1RED(POLTIM,AKORR)
            POLCOO(1)=POLCOO(1)+POLOFF(POLTIM,1)
            POLCOO(2)=POLCOO(2)+POLOFF(POLTIM,2)
            POLCOO(3)=POLCOO(3)+AKORR/1000.D0+POLOFF(POLTIM,3)
            POLCOO(4)=POLCOO(4)/1000.D0+POLOFF(POLTIM,5)
            POLCOO(5)=POLCOO(5)/1000.D0+POLOFF(POLTIM,4)
            REM=BBREM
            GOTO 999
          ELSE IF (POLINE(1:22).EQ.' Preliminary extension'.OR.
     1             POLINE(1:23).EQ.'  Preliminary extension') THEN
            READ(LFN,11)
            READ(LFN,11)
            REM='B5E'
            BBREM='B5E'
          ELSE IF (POLINE.NE.' ') THEN
            GOTO 900
          ENDIF
50      CONTINUE
        GOTO 999
C
C READ FORMAT OF BULLETIN B  (1 DAY VALUES)=  1
C ---------------------------------------------
      ELSE IF (IFORM.EQ.1.AND.IERB1D.EQ.1) THEN
        DO 60 I1=1,1000
          READ(LFN,11,END=920)POLINE
          IF (LENGT0(POLINE).GT.60.AND.POLINE(22:22).EQ.'.'.AND.
     1        POLINE(31:31).EQ.'.'.AND.POLINE(40:40).EQ.'.')THEN
            READ(POLINE,61,ERR=910) MNTH,MJD,POLCOO(1),POLCOO(2),
     1                              POLCOO(3),POLCOO(5),POLCOO(4)
61          FORMAT(2X,A3,5X,I7,2F9.4,F10.5,14X,2F7.1)
            POLTIM=MJD*1.D0
            GPSUTC=DGPSUT(POLTIM)
            POLCOO(1)=POLCOO(1)+POLOFF(POLTIM,1)
            POLCOO(2)=POLCOO(2)+POLOFF(POLTIM,2)
            POLCOO(3)=POLCOO(3)+POLOFF(POLTIM,3)
            POLCOO(4)=POLCOO(4)/1000.D0+POLOFF(POLTIM,5)
            POLCOO(5)=POLCOO(5)/1000.D0+POLOFF(POLTIM,4)
            IF (MNTH.EQ.MNTHO.OR.MNTHO.EQ.'XXX')THEN
              REM=BBREM
            ELSE
              BBREM='B12'
              REM='B12'
            ENDIF
            MNTHO=MNTH
            GOTO 999
          ELSE IF (POLINE.NE.' ') THEN
            GOTO 900
          ENDIF
60      CONTINUE
C
C READ FORMAT OF BULLETIN A                =  2
C ---------------------------------------------
      ELSE IF (IFORM.EQ.2) THEN
        READ(LFN,11,END=900)POLINE
        IF (POLINE.EQ.' ') GOTO 900
        READ(POLINE,65,ERR=910)IND,MJD,POLCOO(1),RMSPOL(1),
     1                        POLCOO(2),RMSPOL(2),POLCOO(3),RMSPOL(3),
     2                        POLCOO(5),RMSPOL(5),POLCOO(4),RMSPOL(4)
CCC65      FORMAT(A1,I5,2(1X,2F7.5),1X,2F8.6,4(1X,F7.5))
65      FORMAT(A1,I5,2(F8.5,F7.5),F9.6,F8.6,4F8.5)
        POLTIM=MJD*1.D0
        GPSUTC=DGPSUT(POLTIM)
        POLCOO(1)=POLCOO(1)+POLOFF(POLTIM,1)
        POLCOO(2)=POLCOO(2)+POLOFF(POLTIM,2)
        POLCOO(3)=POLCOO(3)+POLOFF(POLTIM,3)
        POLCOO(4)=POLCOO(4)+POLOFF(POLTIM,5)
        POLCOO(5)=POLCOO(5)+POLOFF(POLTIM,4)
C_KEEP_LOWER_ON
        IF (IND.EQ.'p') THEN
          REM='AE '
        ELSE
          REM='A  '
        ENDIF
C_KEEP_LOWER_OFF
        GOTO 999
C
C READ FORMAT OF C04                       =  3
C ---------------------------------------------
      ELSE IF (IFORM.EQ.3) THEN
        READ(LFN,11,END=900)POLINE
        IF (POLINE.EQ.' ') GOTO 900
        READ(POLINE,75,ERR=910)MJD,(POLCOO(I3),I3=1,3),
     1                         POLCOO(5),POLCOO(4)
75      FORMAT(11X,I5,2F9.5,F10.6,14X,2F9.5)
cc75      FORMAT(11X,I5,1X,2F9.6,F10.7,14X,2F9.5)
        POLTIM=MJD*1.D0
        GPSUTC=DGPSUT(POLTIM)
        POLCOO(1)=POLCOO(1)+POLOFF(POLTIM,1)
        POLCOO(2)=POLCOO(2)+POLOFF(POLTIM,2)
        POLCOO(3)=POLCOO(3)+POLOFF(POLTIM,3)
        POLCOO(4)=POLCOO(4)+POLOFF(POLTIM,5)
        POLCOO(5)=POLCOO(5)+POLOFF(POLTIM,4)
        REM='C04'
        GOTO 999
C
C READ FORMAT OF CODE SOLUTION IGS FORMAT  =  4
C ---------------------------------------------
      ELSE IF (IFORM.EQ.4) THEN
        READ(LFN,11,END=900)POLINE
        IF (POLINE.EQ.' ') GOTO 900
        READ(POLINE,85,ERR=910)POLTIM,(POLCOO(I3),I3=1,3),
     1                         (RMSPOL(I3),I3=1,3)
85      FORMAT(F11.5,2F9.5,F10.6,2F8.5,F9.6)
        GPSUTC=DGPSUT(POLTIM)
        REM='COD'
        GOTO 999
C
C READ FORMAT OF CODE SOLUTION OLD AND NEW FORMAT (= 5, = 6 )
C -----------------------------------------------------------
      ELSE IF (IFORM.EQ.5.OR.IFORM.EQ.6) THEN
        CALL RDPOLI(LFN,POLTIM,POLCOO,GPSUTC,REM,RMSPOL,IFOCOD,
     1              IENCOD)
        IF (IENCOD.EQ.1) GOTO 900
        GOTO 999
C
C READ FORMAT OF GFZ SOLUTION              =  7
C ---------------------------------------------
C
C READ FORMAT OF IGS SOLUTION              =  8
C ---------------------------------------------
      ELSE IF (IFORM.EQ.8) THEN
        READ(LFN,11,END=900)POLINE
        IF (POLINE.EQ.' ') GOTO 900
        READ(POLINE,95,ERR=910)POLTIM,(POLCOO(I3),I3=1,3),
     1                         (RMSPOL(I3),I3=1,3)
95      FORMAT(F7.1,6F9.5)
        GPSUTC=DGPSUT(POLTIM)
        REM='IGS'
        GOTO 999
C
C READ FORMAT OF JPL SOLUTION              =  9
C ---------------------------------------------
      ELSE IF (IFORM.EQ.9) THEN
        READ(LFN,11,END=900)POLINE
        IF (POLINE.EQ.' ') GOTO 900
        READ(POLINE,105,ERR=910)POLTIM,(POLCOO(I3),I3=1,3),
     1                          (RMSPOL(I3),I3=1,3)
105     FORMAT(F8.1,3F9.5,3F8.5)
        GPSUTC=DGPSUT(POLTIM)
        CALL UT1RED(POLTIM,AKORR)
        POLCOO(3)=POLCOO(3)+AKORR/1000.D0
        REM='JPL'
        GOTO 999
C
C READ FORMAT OF NGS SOLUTION              = 10
C ---------------------------------------------
      ELSE IF (IFORM.EQ.10) THEN
        READ(LFN,11,END=900)POLINE
        IF (POLINE.EQ.' ') GOTO 900
        READ(POLINE,125,ERR=910)POLTIM,(POLCOO(I3),I3=1,3),
     1                          (RMSPOL(I3),I3=1,3)
125     FORMAT(F8.1,3F9.5,3F8.5)
        GPSUTC=DGPSUT(POLTIM)
        REM='NGS'
        GOTO 999
C
C READ FORMAT OF SIO SOLUTION              = 11
C ---------------------------------------------
      ELSE IF (IFORM.EQ.11) THEN
        READ(LFN,11,END=900)POLINE
        IF (POLINE.EQ.' ') GOTO 900
        READ(POLINE,115,ERR=910)POLTIM,(POLCOO(I3),I3=1,3),
     1                          (RMSPOL(I3),I3=1,3)
115     FORMAT(F8.1,1X,2F9.5,F10.6,1X,2F8.5,F9.6)
        GPSUTC=DGPSUT(POLTIM)
        REM='SIO'
        GOTO 999
C
C READ FORMAT OF CODE SOLUTION NEW IGS FORMAT  =  12
C -------------------------------------------------
      ELSE IF (IFORM.EQ.12) THEN
        READ(LFN,11,END=900)POLINE
        IF (POLINE.EQ.' ') GOTO 900
        READ(POLINE,135,ERR=910)POLTIM,(IPLCOO(I3),I3=1,3),
     1                         IPLRAT(3),(IRMSPL(I3),I3=1,3),IRMSRA(3),
     2                         IPLRAT(1),IPLRAT(2),IRMSRA(1),IRMSRA(2),
     3                         IPLCOO(5),IPLCOO(4),IRMSPL(5),IRMSPL(4)
135     FORMAT(F8.2,3I8,I6,4I5,12X,2I6,2I5,18X,2I6,2I5)
        DO 128 I1=1,5
          IF (I1.NE.3)THEN
            POLCOO(I1)=IPLCOO(I1)/100000.D0
            POLRAT(I1)=IPLRAT(I1)/100000.D0
            RMSPOL(I1)=IRMSPL(I1)/100000.D0
            RMSRAT(I1)=IRMSRA(I1)/100000.D0
          ELSE
            POLCOO(I1)=IPLCOO(I1)/1000000.D0
            POLRAT(I1)=-IPLRAT(I1)/1000000.D0
            RMSRAT(I1)=IRMSRA(I1)/1000000.D0
          ENDIF
128     CONTINUE
C
        GPSUTC=DGPSUT(POLTIM)
        REM='COD'
        GOTO 999
C
C READ FORMAT OF IGS SOLUTION NEW IGS FORMAT  =  13
C -------------------------------------------------
      ELSE IF (IFORM.EQ.13) THEN
        READ(LFN,11,END=900) POLINE
        IF (POLINE.EQ.' ') GOTO 900
        IRMSPL(4)=0
        IPLCOO(4)=0
        IPLRAT(4)=0
        IRMSPL(5)=0
        IPLCOO(5)=0
        IPLRAT(5)=0
        READ(POLINE,*,IOSTAT=IOS) POLTIM,(IPLCOO(I3),I3=1,3),
     1                            IPLRAT(3),(IRMSPL(I3),I3=1,3),
     2                            ILODSG,NR,NF,NT,(IPLRAT(I3),I3=1,2)
        IF (IOS.NE.0) THEN
          IPLRAT(1)=0
          IPLRAT(2)=0
          READ(POLINE,*,ERR=910) POLTIM,(IPLCOO(I3),I3=1,3),
     1                           IPLRAT(3),(IRMSPL(I3),I3=1,3),
     2                           ILODSG,NR,NF,NT
        ENDIF
        DO 129 I1=1,5
          IF (I1.NE.3)THEN
            POLCOO(I1)=IPLCOO(I1)/100000.D0
            POLRAT(I1)=IPLRAT(I1)/100000.D0
            RMSPOL(I1)=IRMSPL(I1)/100000.D0
          ELSE
            POLCOO(I1)=IPLCOO(I1)/1000000.D0
            POLRAT(I1)=-IPLRAT(I1)/1000000.D0
            RMSPOL(I1)=IRMSPL(I1)/1000000.D0
          ENDIF
129     CONTINUE
        GPSUTC=DGPSUT(POLTIM)
        REM='IGS'
        GOTO 999
C
C READ FORMAT OF JPL SOLUTION              = 14
C ---------------------------------------------
      ELSE IF (IFORM.EQ.14) THEN
        READ(LFN,11,END=900)POLINE
        IF (POLINE.EQ.' ') GOTO 900
        READ(POLINE,*,ERR=910) POLTIM,(IPLCOO(I3),I3=1,3),
     1                         IPLRAT(3),(IRMSPL(I3),I3=1,3),
     2                         ILODSG,NR,NF,NT,(IPLRAT(I3),I3=1,2)
        DO 149 I1=1,5
          IF (I1.NE.3)THEN
            POLCOO(I1)=IPLCOO(I1)/100000.D0
            POLRAT(I1)=IPLRAT(I1)/100000.D0
            RMSPOL(I1)=IRMSPL(I1)/100000.D0
          ELSE
            POLCOO(I1)=IPLCOO(I1)/1000000.D0
            POLRAT(I1)=-IPLRAT(I1)/1000000.D0
            RMSPOL(I1)=IRMSPL(I1)/1000000.D0
          ENDIF
149     CONTINUE
        CALL UT1RED(POLTIM,AKORR)
        POLCOO(3)=POLCOO(3)+AKORR/1000.D0
        CALL LODRED(POLTIM,AKORR)
        POLRAT(3)=POLRAT(3)-AKORR/1000.D0
C
        GPSUTC=DGPSUT(POLTIM)
        REM='JPL'
        GOTO 999
C
C FORMAT 'VERSION 2' WITH UT1-UTC, LOD = 15
C -----------------------------------------
      ELSE IF (IFORM.EQ.15) THEN
C
        CALL RDIEPI(LFN,2,ICOL,POLTIM,POLCOO,POLRAT,RMSPOL,
     1              RMSRAT,POLCOR,NRFPOL,IRCODE)
        IF(IRCODE.EQ.1) GOTO 900
C
        GPSUTC=DGPSUT(POLTIM)
        REM='V_2'
        GOTO 999
C
C FORMAT 'VERSION 2' WITH UT1R-UTC, LODR = 16
C -------------------------------------------
      ELSE IF (IFORM.EQ.16) THEN
C
        CALL RDIEPI(LFN,2,ICOL,POLTIM,POLCOO,POLRAT,RMSPOL,
     1              RMSRAT,POLCOR,NRFPOL,IRCODE)
        IF(IRCODE.EQ.1) GOTO 900
C
        GPSUTC=DGPSUT(POLTIM)
        CALL UT1RED(POLTIM,AKORR)
        POLCOO(3)=POLCOO(3)+AKORR/1000.D0
        CALL LODRED(POLTIM,AKORR)
        POLRAT(3)=POLRAT(3)-AKORR/1000.D0
        REM='VR2'
        GOTO 999
C
C FORMAT 'VERSION 2' WITH UT1R-TAI, LODR = 17
C -------------------------------------------
      ELSE IF (IFORM.EQ.17) THEN
C
        CALL RDIEPI(LFN,2,ICOL,POLTIM,POLCOO,POLRAT,RMSPOL,
     1              RMSRAT,POLCOR,NRFPOL,IRCODE)
        IF(IRCODE.EQ.1) GOTO 900
C
        GPSUTC=DGPSUT(POLTIM)
        CALL UT1RED(POLTIM,AKORR)
        POLCOO(3)=POLCOO(3)+AKORR/1000.D0+19.0+GPSUTC
        CALL LODRED(POLTIM,AKORR)
        POLRAT(3)=POLRAT(3)-AKORR/1000.D0
        REM='VA2'
        GOTO 999
C
C FORMAT 'VERSION 2' WITH UT1-TAI, LOD = 18
C -----------------------------------------
      ELSE IF (IFORM.EQ.18) THEN
C
        CALL RDIEPI(LFN,2,ICOL,POLTIM,POLCOO,POLRAT,RMSPOL,
     1              RMSRAT,POLCOR,NRFPOL,IRCODE)
        IF(IRCODE.EQ.1) GOTO 900
C
        GPSUTC=DGPSUT(POLTIM)
        POLCOO(3)=POLCOO(3)+19.0+GPSUTC
        REM='VA2'
        GOTO 999
C
C READ FORMAT OF BULLETIN A (second format)= 19
C ---------------------------------------------
      ELSE IF (IFORM.EQ.19) THEN
        READ(LFN,11,END=900)POLINE
        IF (POLINE.EQ.' ') GOTO 900
        IF (POLINE(96:125).EQ.'                              ') GOTO 900
        READ(POLINE,66,ERR=910)POLTIM,IND,POLCOO(1),RMSPOL(1),POLCOO(2),
     1                        RMSPOL(2),IND1,POLCOO(3),RMSPOL(3),
     2                        POLCOO(5),RMSPOL(5),POLCOO(4),RMSPOL(4)
66      FORMAT(7X,F8.2,1X,A1,2(1X,2F9.6),2X,A1,2F10.7,18X,2(1X,2F9.3))
        GPSUTC=DGPSUT(POLTIM)
        POLCOO(1)=POLCOO(1)+POLOFF(POLTIM,1)
        POLCOO(2)=POLCOO(2)+POLOFF(POLTIM,2)
        POLCOO(3)=POLCOO(3)+POLOFF(POLTIM,3)
        POLCOO(4)=POLCOO(4)/1000+POLOFF(POLTIM,5)
        POLCOO(5)=POLCOO(5)/1000+POLOFF(POLTIM,4)
        RMSPOL(4)=RMSPOL(4)/1000
        RMSPOL(5)=RMSPOL(5)/1000
C_KEEP_LOWER_ON
        IF (IND.EQ.'P') THEN
          REM='AE '
        ELSE
          REM='A  '
        ENDIF
C_KEEP_LOWER_OFF
        GOTO 999
C
C READ FORMAT OF C04                       = 20
C ---------------------------------------------
      ELSE IF (IFORM.EQ.20) THEN
        READ(LFN,11,END=900)POLINE
        IF (POLINE.EQ.' ') GOTO 900
        READ(POLINE,205,ERR=910)MJD,
     1  (POLCOO(I3),I3=1,3),POLCOO(5),POLCOO(4),
     2  (RMSPOL(I3),I3=1,3),RMSPOL(5),RMSPOL(4)
205     FORMAT(12X,I7,2(F11.6),F12.7,12X,2(F11.6),
     1                2(F11.6),F11.7,11X,2(F12.6))
        POLTIM=MJD*1.D0
        GPSUTC=DGPSUT(POLTIM)
        POLCOO(1)=POLCOO(1)+POLOFF(POLTIM,1)
        POLCOO(2)=POLCOO(2)+POLOFF(POLTIM,2)
        POLCOO(3)=POLCOO(3)+POLOFF(POLTIM,3)
        POLCOO(4)=POLCOO(4)+POLOFF(POLTIM,5)
        POLCOO(5)=POLCOO(5)+POLOFF(POLTIM,4)
        REM='C04'
        GOTO 999
      ENDIF
C
C END OF FILE REACHED
C -------------------
900   CONTINUE
      IEND=1
      CODTIT= ' '
      POLTYP(1)=1
      POLTYP(2)=1
      GOTO 999
C
C ERROR WHILE READING
C -------------------
910   CONTINUE
      WRITE(LFNERR,901)
901   FORMAT(/,' *** SR RDPOL: ERROR READING POLE FILE RECORD',/)
      IEND=2
      GOTO 999
C
920   CONTINUE
      WRITE(LFNERR,902)
902   FORMAT(/,' *** SR RDPOL: UNKNOWN FORMAT OF POLE FILE',/)
      IEND=3
      GOTO 999
C
999   RETURN
      END SUBROUTINE

      END MODULE
