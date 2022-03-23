C*
      PROGRAM CODXTR
CC
CC NAME       :  CODXTR
CC
CC PURPOSE    :  EXTRACTION FOR CODSPP OUTPUT
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  T.A. SPRINGER
CC
CC CREATED    :  28-DEC-1995
CC
CC CHANGES    :  04-APR-96 : MR: DECLARATION OF "NEWSAT"
CC               06-JUN-96 : TS: REMOVED UNUSED VARIABLES
CC               04-FEB-97 : TS: ADDED CHECK FOR MAX.RMS OF 999 M
CC                               USEFULL FOR AUTOMATIC DELETION.
CC               24-SEP-97 : DI: USE MAXSAT.inc
CC               08-OCT-97 : DI: UNIT WEIGHT RMS READING CHANGED
CC               29-OCT-97 : SS: EXTRACT MAX PERCENTAGE OF BAD OBS
CC               06-JUL-99 : PF: CALL IYEAR4 FOR CONVERSION YY->YYYY
CC               18-JUL-99 : TS: SMALL CHANGE FOR MAN. DETECTION
CC               15-JAN-01 : RD: NEW MENU SYSTEM, LIST OF GOOD/BAD FILES
CC               15-JAN-02 : MM: OUTPUT CHANGED
CC               24-JAN-03 : SC: READ TWO CODSPP OUTPUT FILE
CC               27-JAN-03 : SC: DO RMS CHECK
CC               10-FEB-03 : MM: NEW OPTION (REMOVE SAT FROM CLK-BRD-FILE)
CC                               SATCRUX NO LONGER CREATED
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               13-MAR-03 : RD: WRITE FULL PATH IN THE FILE LISTS
CC               23-APR-03 : HU: NULLIFY LOCAL POINTERS
CC               30-MAY-03 : HU: PRINT THREE DIGITS FOR SATELLITE NUMBERS
CC               18-AUG-03 : RD: CORRECT FILE HANDLING
CC               08-OCT-03 : RD: ADD KINEMATIC CODSPP OUTPUT EXTRACTION
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               20-DEC-03 : HU: INITIALIZE STATION NAME
CC               28-JUN-04 : RD: USE MAXCRD FROM M_MAXDIM FOR MAXSTA
CC               21-JUN-05 : MM: LFNUM.inc, COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               06-DEC-05 : RD: MESSAGE IF BOTH SATCLK FILES HAVE THE SAME NAME
CC               15-DEC-06 : RD: ESTIMATE THE MANEUVER EPOCH
CC               15-FEB-07 : AG: SAVE EXIT IF END OF CLKFILE, NEW OPTION LINOUT
CC                               ADDED, REPOS OR CLK RESET ONLY IF DETECTED BY
CC                               MORE THAN 5 STATIONS
CC               27-FEB-07 : AG: CALL DEFCON
CC               19-DEC-07 : RD: CORRECT INDEX ERROR FOR MORE THAN ONE MANSAT
CC               04-SEP-08 : RD: CORRECTED OUTPUT DUE TO POSSIBLE DOUBLED
CC                               STATION LISTING IN OUTPUT FILE
CC               19-JUL-10 : SL: TAB CHARACTERS REMOVED
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               24-NOV-11 : SL: NEW TITLE STRING FOR PRITIT, M_BERN WITH ONLY
CC               30-JAN-12 : SL: OUTPUT FORMAT FOR RMS CHANGED
CC               28-MAR-12 : RD: USE LISTC1 AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1992     UNIVERSITY OF BERN
CC               SWITZERLAND
C*
      USE m_bern,   ONLY: r8b, fileNameLength, keyValueLength,
     1                    lfnPrt, lfnErr, lfnOrb, lfnLoc, lfn001, lfn002
      USE m_cpu,    ONLY: cpu_start
      USE m_maxdim, ONLY: maxsta => maxcrd
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE m_maxdim, ONLY: MAXSAT
      USE s_opnfil
      USE s_defcon
      USE s_pritit
      USE s_dattim
      USE s_readkeys
      USE s_rplenvar
      USE s_opnsys
      USE s_gtflna
      USE s_dimtst
      USE s_prflna
      USE s_gtfile
      USE s_readinpf
      USE s_opnerr
      USE s_prfile
      USE s_fparse
      USE s_exitrc
      USE s_ckoptb
      USE f_djul
      USE f_iyear4
      USE f_modsvn
      USE s_strtim
      USE s_alcerr
      USE s_timst2
      USE s_iordup
      USE f_listc1

      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I      , IFIL   , IMAX   , INDST1 ,
     1          INDST2 , IOBS   , IOS    , IOSTAT , IRC    , IRCODE ,
     2          IREC   , IS     , ISAT   , ISTA   , ISVN   , ITIM   ,
     3          JOBS   , JTIM   , KINEST , LSTSAT , MAXBAD , MAXFIL ,
     4          NFLCOL , NMAN   , NOBBAD , NOBTOT , NRFIL  , NSAT   ,
     5          NSTA   , DUMMY  , JCNT
C
      REAL*8    BADMAX , RMSMXX
C
CCC       IMPLICIT  REAL*8  (A-H,O-Z)
C
C MAXIMAL DIMENSIONS
C ------------------
      PARAMETER (MAXFIL=1000)
C
C MAXSAT: MAXIMUM NUMBER OF SATELLITES
C MAXSTA: MAXIMUM NUMBER OF STATIONS
C MAXFIL: MAXIMUM NUMBER OF CODSPP OUTPUT FILES
C
C DECLARATIONS
C ------------
      CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue
      CHARACTER*80 STRING
      CHARACTER*36 TSTRNG
      CHARACTER*32 FILNAM(MAXFIL),FILSTA(MAXSTA)
      CHARACTER(LEN=fileNameLength) FIL_OK, FILBAD
      CHARACTER(LEN=fileNameLength) clkInp, clkOut
      CHARACTER(LEN=fileNameLength) node, device, dir, name, ext, ver
      CHARACTER(LEN=255) DIRCZH,DIRCZO,DIRPZH,DIRPZO
      CHARACTER(LEN=3)   EXTCZH,EXTCZO,EXTPZH,EXTPZO
      CHARACTER(LEN=80) clkLine
      CHARACTER*16 STANAM(MAXSTA),STNAME,LSTNAM,STATION(MAXSAT,MAXSTA)
      CHARACTER    DATSTR*9,TIMSTR*5,KEY*3
      CHARACTER    STR1*6,STR2*4,STR3*8,STR4*10
C
      INTEGER*4    NUMSAT(MAXSAT),MANSAT(MAXSAT),IDXSAT(MAXSAT)
      INTEGER*4    ICNT(MAXSAT),NEPO(MAXSTA,3)
      INTEGER*4    iClk, actSat, skip
      INTEGER*4    LINOUT,OUT(MAXSTA)
C
      REAL*8       RMS(MAXSTA),BAD(MAXSTA),RMSKIN(MAXSTA,2)
      REAL*8       RMSMAX,RMJD(2)
      REAL(r8b), DIMENSION(:,:,:), ALLOCATABLE :: BADTIM
C
      LOGICAL*1    NEWSAT
      LOGICAL      BADSTA
      LOGICAL      setup1
      LOGICAL      DELPHS
      LOGICAL      KINTIT
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C NULLIFY POINTERS
C ----------------
      NULLIFY(keyValue)
      CALL init_inpkey(inpKey)
C
C GET THE DATE AND TIME
C ---------------------
      CALL DATTIM(DATSTR,TIMSTR)
C
C GET THE NAME OF THE INPUT FILE
C ------------------------------
      CALL readinpf(' ',inpKey)
C
C DEFINE SYSTEM FILES AND CONSTANTS
C ---------------------------------
      CALL OPNSYS
      CALL DEFCON(0)
C
C PRINT TITLE SECTION
C -------------------
      CALL PRITIT('CODXTR','Extract CODSPP program output')
      CALL PRFLNA
C
C DO RMS CHECK
C -------------
      CALL ckoptb(1,(/'RMS_OPT'/),'CODXTR',
     1            'DO BAD STATION DETECTION',irCode,
     2             resultL = setup1)
C
C Read max RMS allowed
C --------------------
      IF (setup1) then
        CALL readkeys('RMSMAX',keyValue,irc)
        IF (LEN_TRIM(keyValue(1)) > 0) THEN
          READ(keyValue(1),*,iostat=ios) rmsmax
          IF (ios /= 0)
     1      WRITE(lfnerr,'(/,A,A,/)')
     2           ' *** PG CODXTR: wrong entry for keyword "RMSMAX"',
     3                          ' in the input file'
        ELSE
          ios=0
          rmsmax = 0D0
        ENDIF
        IF (irc+ios /= 0) GOTO 999
C Read max OUT lines allowed
C --------------------------
        CALL readkeys('LINOUT',keyValue,irc)
        IF (LEN_TRIM(keyValue(1)) > 0) THEN
          READ(keyValue(1),*,iostat=ios) linout
          IF (ios /= 0)
     1      WRITE(lfnerr,'(/,A,A,/)')
     2           ' *** PG CODXTR: wrong entry for keyword "LINOUT"',
     3                          ' in the input file'
        ELSE
          ios=0
          linout = 0
        ENDIF
        IF (irc+ios /= 0) GOTO 999
      ELSE
        rmsmax = 0d0
        linout = 0
      ENDIF
C
C Get file name of good/bad observation files
C -------------------------------------------
      IF (setup1) THEN
        CALL gtflna(0,'FILBAD',filbad,irc)
C
        CALL gtflna(0,'FIL_OK',fil_ok,irc)
C
        CALL readKeys('EXTCZH',keyValue,irc)
        EXTCZH=keyValue(1)
C
        CALL readKeys('EXTCZO',keyValue,irc)
        EXTCZO=keyValue(1)
C
        CALL readKeys('EXTPZH',keyValue,irc)
        EXTPZH=keyValue(1)
C
        CALL readKeys('EXTPZO',keyValue,irc)
        EXTPZO=keyValue(1)
C
        CALL readKeys('DIRCZH',keyValue,irc)
        DIRCZH=keyValue(1)
        CALL rplenvar(1,DIRCZH)
C
        CALL readKeys('DIRCZO',keyValue,irc)
        DIRCZO=keyValue(1)
        CALL rplenvar(1,DIRCZO)
C
        CALL readKeys('DIRPZH',keyValue,irc)
        DIRPZH=keyValue(1)
        CALL rplenvar(1,DIRPZH)
C
        CALL readKeys('DIRPZO',keyValue,irc)
        DIRPZO=keyValue(1)
        CALL rplenvar(1,DIRPZO)
C
        IF (LEN_TRIM(filbad) > 0) THEN
          CALL ckoptb(1,(/'DELPHS'/),'CODXTR',
     1                'Delete code and phase files',irCode,
     2                 resultL = delphs)
        ENDIF
      ELSE
        filbad = ' '
        fil_ok = ' '
      ENDIF
C
C Check availability of broadcast files
C -------------------------------------
      CALL gtflna(0,'CLKINP',clkInp,iClk)
      IF (iClk==0) THEN
        CALL gtflna(0,'CLKOUT',clkOut,iClk)
        IF (iClk/=0) THEN
          WRITE(LFNERR,'(/,A,/,A,/)')
     1      ' ### PG CODXTR: NO BROADCAST CLOCK OUTPUT FILE SPECIFIED',
     2      '                CLOCK FILE WILL NOT BE ADAPTED'
        ELSE IF (clkInp == clkOut) THEN
          WRITE(LFNERR,'(/,A,A,2(/,16X,A),/)') ' ### PG CODXTR: ',
     1      'THE NAMES FOR THE INPUT AND RESULT SATELLITE CLOCK FILE',
     2      'HAVE TO BE DIFFERENT.',
     3      'THE SATELLITE CLOCK FILE WILL NOT BE MODFIED.'
          iClk = 1
        END IF
      END IF
C
C INITIALIZE
C ----------
      NFLCOL=1
      NSTA=0
      NSAT=0
      NMAN=0
      NUMSAT=0
      ICNT=0
      OUT=0
      STATION=''
      MAXBAD=5
      STANAM(1)=' '
      ALLOCATE(BADTIM(MAXSAT,2,MAXBAD),STAT=IRC)
      CALL ALCERR(IRC,'BADTIM',(/MAXSAT,2,MAXBAD/),'CODXTR')
      BADTIM=1d20
C
C READ NAMES OF INPUT FILE : OUTPUT FILES OF CODSPP
C -------------------------------------------------
      CALL readkeys ('RADIO_1',keyValue,irc)
      IF (keyValue(1) .EQ. '1') THEN
        CALL GTFILE('FILLST',NFLCOL,MAXFIL,NRFIL,FILNAM)
        CALL PRFILE('FILLST','',1)
      ELSE IF (keyValue(1) .EQ. '0') THEN
        CALL GTFILE('OUTPUT',NFLCOL,MAXFIL,NRFIL,FILNAM)
        CALL PRFILE('OUTPUT','',1)
      END IF
C
C LOOP OVER ALL FILES
C -------------------
      DO IFIL=1,NRFIL
        KINEST=0
C
C OPEN CODSPP OUTPUT FILE (INPUT)
C--------------------------------
        CALL OPNFIL(LFN001,FILNAM(IFIL),'OLD','FORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFN001,IOSTAT,FILNAM(IFIL),'CODXTR')
C
C LOOP OVER ALL RECORDS
C ---------------------
        DO 100 IREC=1,100000
          READ(LFN001,'(A)',ERR=10,END=10)STRING
          READ(STRING,'(1X,A6)',ERR=999)STR1
          READ(STRING,'(A4)',ERR=999)STR2
          READ(STRING,'(1X,A8)',ERR=999)STR3
          READ(STRING,'(1X,A10)',ERR=999)STR4
          IF ((STR1.EQ.'### SR' .OR. STR1.EQ.'*** SR' .OR.
     1         STR1.EQ.'### PG' .OR. STR1.EQ.'*** PG' .OR.
     2         STR2.EQ.' SR ') .AND.
     3         STRING(1:29).NE.' *** PG CODSPP: FREQUENCY NOT')
     4      WRITE(LFNPRT,'(A)') STRING
C
C IS IT A KINEMATIC INPUT FILE
C ----------------------------
          IF (STR4.EQ.'EPOCH     ') KINEST=1
C
C READ STATION NAME AND FILE NAME IN THE STATIC CASE
C --------------------------------------------------
          IF (KINEST.EQ.0.AND.STR4.EQ.'STATION:  ') THEN
            NSTA=NSTA+1
            IF (NSTA.GT.MAXSTA) THEN
              WRITE(LFNERR,110)NSTA,MAXSTA
110           FORMAT(/,' *** PG CODXTR: TO MANY STATIONS ',/,
     1                     16X,'#STATIONS:',I6,/,
     2                     16X,'MAXIMUM  :',I6)
              GOTO 999
            ENDIF
            READ(STRING,'(11X,A16,8X,A32)')STANAM(NSTA),FILSTA(NSTA)
            RMSKIN(NSTA,1:2)=0D0
          ENDIF
C
C READ PERCENTAGE OF BAD OBSERVATIONS IN THE STATIC CASE
C ------------------------------------------------------
          IF (KINEST.EQ.0.AND.
     1        STRING(2:22).EQ.'BAD OBSERVATIONS    :') THEN
            READ(STRING,'(22X,F13.2)',ERR=999,END=999) BAD(NSTA)
          ENDIF
C
C READ RMS OF UNIT WEIGHT IN THE STATIC CASE
C ------------------------------------------
          IF (KINEST.EQ.0.AND.STR4.EQ.'RMS OF UNI') THEN
            READ(STRING,'(22X,F13.2)',ERR=999,END=999)RMS(NSTA)
          ENDIF
C
C EXTRACT THE KINEMATIC EPOCH SOLUTIONS
C -------------------------------------
          IF (KINEST.EQ.1.AND.STRING(39:39).EQ.'/') THEN
            NSTA=NSTA+1
            CALL DIMTST(1,2,1,'CODXTR','MAXSTA','NUMBER OF STATIONS',
     1                  ' ',NSTA,MAXSTA,IRC)
            READ(STRING,'(I6,14X,A16,I2,1X,I2)')
     1           NEPO(NSTA,1),STANAM(NSTA),IOBS,JOBS
            IF (STRING(67:68).NE.'+-') IOBS=JOBS
            IF (NSTA.EQ.1) THEN
              NOBTOT=JOBS
              NOBBAD=IOBS
              BAD(NSTA)=DBLE(NOBBAD)/DBLE(NOBTOT)*100d0
            ELSE IF (STANAM(NSTA).EQ.STANAM(NSTA-1)) THEN
              NSTA=NSTA-1
              NOBTOT=NOBTOT+JOBS
              NOBBAD=NOBBAD+IOBS
              BAD(NSTA)=DBLE(NOBBAD)/DBLE(NOBTOT)*100d0
              NEPO(NSTA,1)=NEPO(NSTA+1,1)
            ELSE IF (STANAM(NSTA).NE.STANAM(NSTA-1)) THEN
              NOBTOT=JOBS
              NOBBAD=IOBS
              BAD(NSTA)=DBLE(NOBBAD)/DBLE(NOBTOT)*100d0
            ENDIF
          ENDIF
C
C READ THE EPOCH RMS FOR THE KINEMATIC CASE
C -----------------------------------------
          IF (KINEST.EQ.1.AND.STRING(38:41).EQ.'MIN:') THEN
            IF (STRING(50:50).EQ.'*') THEN
              RMSKIN(NSTA,1)=99999.99
            ELSE
              READ(STRING(42:50),*) RMSKIN(NSTA,1)
            ENDIF
C
            READ(LFN001,'(A)') STRING
            IF (STRING(50:50).EQ.'*') THEN
              RMSKIN(NSTA,2)=99999.99
            ELSE
              READ(STRING(42:50),*) RMSKIN(NSTA,2)
            ENDIF
C
            READ(LFN001,'(A)') STRING
            IF (STRING(50:50).EQ.'*') THEN
              RMS(NSTA)=99999.99
            ELSE
              READ(STRING(42:50),*) RMS(NSTA)
            ENDIF
          ENDIF
C
C BAD KINEMATIC EPOCH SOLUTIONS
C -----------------------------
          IF (KINEST.EQ.1.AND.
     1        STRING(2:30).EQ.'NUMBER OF EPOCHS WITH TOO LES') THEN
            READ(STRING(44:50),*) NEPO(NSTA,2)
          ENDIF
C
          IF (KINEST.EQ.1.AND.
     1        STRING(2:30).EQ.'NUMBER OF SINGULAR KINEMATIC ') THEN
            READ(STRING(44:50),*) NEPO(NSTA,3)
          ENDIF
C
C READ STATION NAME AND FILE NAME IN THE KINEMATIC CASE
C -----------------------------------------------------
          IF (KINEST.EQ.1.AND.STR4.EQ.'STATION:  ') THEN
            DO ISTA=1,NSTA
              IF (RMSKIN(ISTA,1).EQ.0D0) CYCLE
              IF (STRING(12:27).EQ.STANAM(ISTA)) THEN
                READ(STRING,'(35X,A32)')FILSTA(ISTA)
                EXIT
              ENDIF
            ENDDO
          ENDIF
C
C LOOP OVER ALL MARKED OBSERVATIONS
C ---------------------------------
          IF (STR3.EQ.'NUMB FIL') THEN
            READ(LFN001,'(A)',ERR=999)STRING
            DO 400 I=1,100000
              READ(LFN001,'(A)',ERR=410,END=410)STRING
              READ(STRING,390,ERR=400) STNAME,KEY,ISVN,TSTRNG,IMAX
390           FORMAT(5X,4X,2X,A16,2X,A3,I4,2X,A36,I6)
              IF(ISVN.EQ.0.OR.IMAX.EQ.0) CYCLE
              CALL STRTIM(2,TSTRNG,RMJD)
              ISAT=MODSVN(ISVN)
C
C COUNT "OUT" LINES
C -----------------
              IF (KEY .EQ. "OUT" .AND. IMAX .GT. 50) THEN
                ISTA=LISTC1(0,16,MAXSTA,STANAM,STNAME,NSTA)
                OUT(ISTA)=OUT(ISTA)+1
              ENDIF
C
C WRITE WARNING IF MAX. DELETED EPOCHS > 50
C -----------------------------------------
              IF (IMAX.GT.50) THEN
                WRITE(LFNPRT,'(A)') STRING
              ENDIF
C
C ADD SATELLITE TO MANOEUVRE LIST
C -------------------------------
              IF ((STNAME.NE.LSTNAM.OR.ISAT.NE.LSTSAT) .AND.
     1            IMAX.GT.50.AND. KEY.EQ.'OUT') THEN
                NEWSAT=.TRUE.
                DO 500 IS=1,NSAT
                  IF (ISAT.EQ.NUMSAT(IS)) THEN
                    JCNT=ICNT(IS)
                    DUMMY=LISTC1(1,16,MAXSTA,STATION(IS,:),STNAME,
     1                                                         ICNT(IS))
                    DO ITIM=1,MAXBAD
                      IF (RMJD(1).LT.BADTIM(IS,1,ITIM)) THEN
                        IF (ITIM.LT.MAXBAD) THEN
                          DO JTIM=ITIM,MAXBAD-1
                            BADTIM(IS,1,JTIM+1)=BADTIM(IS,1,JTIM)
                            BADTIM(IS,2,JTIM+1)=BADTIM(IS,2,JTIM)
                          ENDDO
                         ENDIF
                        BADTIM(IS,1,ITIM)=RMJD(1)
                        BADTIM(IS,2,ITIM)=RMJD(2)
                        EXIT
                      ENDIF
                    ENDDO
                    NEWSAT=.FALSE.
                    IF (JCNT.NE.ICNT(IS).AND.ICNT(IS).EQ.MAXBAD) THEN
                      NMAN=NMAN+1
                      MANSAT(NMAN)=NUMSAT(IS)
                    ENDIF
                  ENDIF
500             CONTINUE
                IF (NEWSAT) THEN
                  NSAT=NSAT+1
                  ICNT(NSAT)=1
                  NUMSAT(NSAT)=ISAT
                  STATION(NSAT,ICNT(NSAT))=STNAME
                ENDIF
              ENDIF
              IF (IMAX.GT.50.AND. KEY.EQ.'OUT') THEN
                LSTNAM=STNAME
                LSTSAT=ISAT
              ENDIF
C
C END LOOP OVER ALL MARKED OBSERVATIONS
C -------------------------------------
400         CONTINUE
410         CONTINUE
          ENDIF
C
C END RECORD LOOP
C ---------------
100     CONTINUE
C
C END FILE LOOP
C -------------
10      CONTINUE
        CLOSE(LFN001)
      ENDDO
C
C SUMMARIZE THE KINEMATIC EPOCH SOLUTIONS
C ---------------------------------------
      KINTIT=.TRUE.
      DO ISTA=1,NSTA
        IF (RMSKIN(ISTA,1).EQ.0D0) CYCLE
        IF (KINTIT) THEN
          WRITE(LFNPRT,'(4(/,A),/)')
     1    ' SUMMARY FOR KINEMATIC SOLUTIONS:',
     2    ' -------------------------------',
     3    '                     RMS OF EPOCH SOLUTION (M)  ' //
     4    '         NUMBER OF EPOCHS',
     5    ' STATION NAME         MEAN      MIN      MAX    ' //
     6    '     TOTAL   FEW OBS   SINGULAR'
          KINTIT=.FALSE.
        ENDIF
        WRITE(LFNPRT,'(1X,A16,3X,3(F8.2,1X),1X,3(4X,I6))')
     1        STANAM(ISTA),RMS(ISTA),RMSKIN(ISTA,1:2),NEPO(ISTA,1:3)
      ENDDO
C
C HANDLE MANOUEVRE
C ----------------
      IF (NMAN.GT.0) THEN
        WRITE(LFNPRT,560)NMAN
560     FORMAT(/,' ### SATELLITE REPOSITIONING OR CLOCK RESET ',
     1           'DETECTED! ',/,'     # SATELLITES    :',I3)
C
        CALL IORDUP(MANSAT,NMAN,IDXSAT)
        DO 510 I=1,NMAN
          TSTRNG=''
          DO ISAT=1,NSAT
            IF(NUMSAT(ISAT).EQ.MANSAT(IDXSAT(I))) THEN
              CALL TIMST2(1,1,BADTIM(ISAT,1,MAXBAD),TSTRNG)
              DO ITIM=1,MAXBAD-1
                IF (BADTIM(ISAT,2,ITIM).GT.BADTIM(ISAT,1,ITIM+1).OR.
     1              BADTIM(ISAT,1,ITIM+1).EQ.1D20) THEN
                  CALL TIMST2(1,1,BADTIM(ISAT,1,ITIM),TSTRNG)
                  EXIT
                ENDIF
              ENDDO
              EXIT
            ENDIF
          ENDDO
          WRITE(LFNPRT,580)MANSAT(IDXSAT(I)),TRIM(TSTRNG)
580       FORMAT('     SATELLITE NUMBER:',I3,10X,A)
510     CONTINUE
        WRITE(LFNPRT,'(A)') ''
      ENDIF
C
C Adapt broadcast clock file
C --------------------------
      IF (nMan.GT.0 .AND. iClk.EQ.0) THEN
C
C Open files
C ----------
        CALL opnfil(LFNORB,clkInp,'OLD','FORMATTED',' ',' ',ioStat)
        CALL opnerr(LFNERR,LFNORB,ioStat,clkInp,'CODXTR')
C
        CALL opnfil(LFNLOC,clkOut,'UNKNOWN','FORMATTED',' ',' ',ioStat)
        CALL opnerr(LFNERR,LFNLOC,ioStat,clkOut,'CODXTR')
C
C Write short message
C -------------------
        WRITE(LFNPRT,'(1X,I3,A)')
     1       nMan,' SATELLITE(S) REMOVED FROM BROADCAST CLOCK FILE.'
        WRITE(LFNPRT,*)'    SATELLITE NUMBER(S):',manSat(idxsat(1:nMan))
        WRITE(LFNPRT,'(A)') ''
C
C Remove satellites
C -----------------
        READ(LFNORB,'(A)',ERR=910) clkLine
        clkLine(44:64)=' (ADAPTED BY CODXTR)'
        WRITE(LFNLOC,'(A)') clkLine
        DO iSat=1,4
          READ(LFNORB,'(A)',ERR=910) clkLine
          WRITE(LFNLOC,'(A)') clkLine
        END DO
C
        DO
          skip = 0
          READ(LFNORB,'(A)',END=111) clkLine
          IF (LEN_TRIM(clkLine)==0) EXIT
          READ(clkLine(1:3),'(I3)',ERR=910) actSat
          DO iSat=1,nMan
            IF (actSat==manSat(iSat)) skip=1
          END DO
          IF (skip==1) CYCLE
          WRITE(LFNLOC,'(A)') clkLine
        END DO
        WRITE(LFNLOC,'(//)')
C
111     CLOSE(LFNORB)
        CLOSE(LFNLOC)
      END IF
C
C GET MAXIMUM RMS
C ---------------
      IF (LEN_TRIM(filbad) > 0) THEN
        CALL opnfil (lfn001,filbad,'UNKNOWN','FORMATTED',
     1               ' ',' ',IOSTAT)
        CALL opnerr(lfnerr,lfn001,iostat,filbad,'CODXTR')
      ENDIF
      IF (LEN_TRIM(fil_ok) > 0) THEN
        CALL opnfil (lfn002,fil_ok,'UNKNOWN','FORMATTED',
     1               ' ',' ',IOSTAT)
        CALL opnerr(lfnerr,lfn002,iostat,fil_ok,'CODXTR')
      ENDIF
C
      RMSMXX=0.D0
      INDST1=1
      DO I=1,NSTA
        BADSTA =.false.
        IF (RMS(I).GT.RMSMXX) THEN
          RMSMXX=RMS(I)
          INDST1=I
        ENDIF
        IF (RMS(I) .GT. RMSMAX .AND. RMSMAX .GT. 0D0) THEN
          WRITE(LFNPRT,1005)RMSMAX,STANAM(I),FILSTA(I),RMS(I)
1005      FORMAT(/,' ### PG CODXTR: RMS LARGER THAN ',F7.2,
     1                           ' M FOR STATION: ',A16,
     2                     /,16X,'FILE: ',A32,
     3                     /,16X,'RMS : ',F8.2,' M')
          BADSTA = .true.
        ELSEIF (BAD(I) .GE. 50.D0 .AND. OUT(I) .GE. linout .AND.
     1                                  linout .NE. 0) THEN
          WRITE(LFNPRT,1007)LINOUT,STANAM(I),FILSTA(I),OUT(I)
1007      FORMAT(/,' ### PG CODXTR: IN CASE OF BAD OBSERVATIONS > 50.0',
     1                           '% NUMBER OF',
     2                     /,16X,'"OUT" LINES LARGER THAN',
     3                        I5,' FOR STATION: ',A16,
     4                     /,16X,'FILE : ',A32,
     5                     /,16X,'LINES: ',I6)
          BADSTA = .true.
        ENDIF
C
        IF (BADSTA) THEN
          IF (LEN_TRIM(filbad) > 0) THEN
            CALL fparse(0, filsta(i), node, device, dir, name, ext,
     1                  ver, irc)
            IF (irc == 0) THEN
              WRITE(lfn001,*)
     1              TRIM(DIRCZH) // TRIM(name) // '.' // TRIM(EXTCZH)
              WRITE(lfn001,*)
     1              TRIM(DIRCZO) // TRIM(name) // '.' // TRIM(EXTCZO)
              IF (DELPHS) THEN
                WRITE(lfn001,*)
     1                TRIM(DIRPZH) // TRIM(name) // '.' // TRIM(EXTPZH)
                WRITE(lfn001,*)
     1                TRIM(DIRPZO) // TRIM(name) // '.' // TRIM(EXTPZO)
              ENDIF
            ENDIF
          ENDIF
        ELSE
          IF (LEN_TRIM(fil_ok) > 0) THEN
            CALL fparse(0, filsta(i), node, device, dir, name, ext,
     1                  ver, irc)
            IF (irc == 0) THEN
              WRITE(lfn002,*)
     1              TRIM(DIRCZH) // TRIM(name) // '.' // TRIM(EXTCZH)
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C
      IF (LEN_TRIM(filbad) > 0) CLOSE(lfn001)
      IF (LEN_TRIM(fil_ok) > 0) CLOSE(lfn002)
C
C GET MAXIMUM PERCENTAGE OF BAD OBSERVATIONS
C ------------------------------------------
      BADMAX=0.D0
      INDST2=0
      DO I=1,NSTA
        IF (BAD(I).GT.BADMAX) THEN
          BADMAX=BAD(I)
          INDST2=I
        ENDIF
        IF (BAD(I).GT.50.D0) THEN
          WRITE(LFNPRT,1006)STANAM(I),FILSTA(I),BAD(I)
1006      FORMAT(/,' ### PG CODXTR: NUMBER OF BAD OBS LARGER THAN ',
     1                           '50% FOR STATION: ',A16,
     2                     /,16X,'FILE   : ',A32,
     3                     /,16X,'BAD OBS: ',F8.2,' %')
        ENDIF
      ENDDO
C
      WRITE(LFNPRT,1010) NSTA,RMSMXX,STANAM(INDST1)
1010  FORMAT(/,1X,I3,' FILES, MAX. RMS:',F8.2,' M FOR STATION: ',A16)
C
      IF (INDST2.GT.0) THEN
        WRITE(LFNPRT,1011) BADMAX,STANAM(INDST2)
1011    FORMAT(11X,' MAX. BAD:',F8.2,' % FOR STATION: ',A16)
      ENDIF
C
998   CALL EXITRC(0)
C
910   WRITE(LFNERR,911) TRIM(clkinp)
911   FORMAT(/,' *** PG CODXTR: READING ERROR IN CLOCK FILE',
     1       /,'                FILE: ',A,/)
999   CALL EXITRC(2)
      END
