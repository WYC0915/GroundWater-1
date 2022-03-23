C*
      PROGRAM PHCCNV
CC
CC NAME       :  PHCCNV
CC
CC PURPOSE    :  CONVERT ANTENNA PHASE CENTER CORRECTIONS INTO THE
CC               BERNESE FORMAT
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  4.0
CC
CC CREATED    :  15-JUN-96
CC
CC CHANGES    :  25-OCT-01 : HB: SWITCH TO NEW MENU
CC               16-DEC-01 : HU: D_CONST ADDED
CC               25-SEP-02 : HU: REMOVE I_ASTLIB
CC               11-AUG-03 : RS: SET MAXAZI 37 --> 73, SET MAXRCV
CC                               100 --> 200, ADD MAXZEN
CC               11-AUG-03 : RS: ADD ANTEX, ADD CALL OF GTSATA, USE
CC                               M_MAXDIM, ADD CALLS OF SR READKEYS
CC               08-SEP-03 : HU: ANTNAM, RECNAM, OPRNAM CHR16 -> CHR20
CC               10-SEP-03 : HU: MERGED
CC               15-OCT-03 : HU: OPTION DO NOT WRITE PATTERNS IF ZERO
CC               05-NOV-03 : RS: ADD GLONASS, BLOCK-SPECIFIC SATELLITE
CC                               ANTENNA PATTERNS, ANTEX: RECTYP=' '
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               21-JUN-05 : MM: LFNUM.inc REMOVED (LFNUMs IN M_BERN)
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               15-AUG-05 : AG: FIND THE INTERNAL BERNESE SENSOR NAME
CC                               IN SATELLIT. FILE, WRITING OUTPUT FILE
CC               01-NOV-05 : AG: WRITING ENTRIES FOR EACH SATELLITE IN BERNESE
CC                               PCV FILE, PREPARING FOR ABSOLUTE PCV
CC               05-DEZ-05 : AG: UPDATE AND CONVERTING AND MERGING OF OLD
CC                               BERNESE PCV FILE ADDED
CC               27-FEB-06 : AG: CONSIDERATION OF ANTENNAS WITHOUT RADOME CODE
CC                               ADDED
CC               03-MAR-06 : AG: NEW WARNING MESSAGE IF AOAD/M_T NOT FOUND
CC               09-MAR-06 : AG: WRONG WARNING MESSAGE CORRECTED
CC               16-MAR-06 : AG: INFO IN CASE OF VALUE DIFFERENCES ADDED
CC               23-MAR-06 : AG: OUTPUT AND ERROR MESSAGES REVIEWED
CC               27-MAR-06 : AG: SPECIAL HANDLING FOR SIMULA
CC                               WRITING OF ZERO PATTERN CORRECTED
CC               28-MAR-06 : AG: ENLARGEMENT IMPROVED
CC               11-APR-06 : AG: SORTING IMPROVED
CC               26-APR-06 : AG: 999999 OPTION FOR RECEIVER ANTENNA NUMBER ADDED
CC               15-AUG-06 : AG: INITIALISATION OF "COUNT" MOVED
CC               26-SEP-06 : AG: SKIPPING OF NON-GPS FREQUENCIES AND
CC                               ERROR MESSAGE IMPROVED
CC               19-JAN-07 : AG: FINDING OF SATELLITE ANTENNAS IMPROVED
CC               27-FEB-07 : AG: CALL DEFCON WITH PARAMETER
CC               21-MAY-07 : AG: CONVERSION FROM ABS TO REL FOR ANTENNAS IN
CC                               ANTEX
CC               31-MAY-07 : AG: CHANGE ORDER SATELLITES <-> RECEIVER
CC               20-JUN-07 : SS: READ 5 OR 6 LAST DIGITS OF ANTEX ANTENNA NUMBER
CC               25-JUN-07 : SS: DECREMENT NFRANT IF INDICATED
CC               05-JUL-07 : SS: IGNORE RECEIVER ANTENNAS WITHOUT GPS
CC                               FREQUENCIES
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               01-OCT-10 : SS: MAXRCV FROM 500 TO 1000
CC               27-OCT-10 : SL: USE M_BERN WITH ONLY, REMOVAL OF UNUSED MOD/VAR
CC               02-NOV-10 : SL: USE UNDEF_C, UNDEF_I
CC               28-MAR-12 : RD: USE LISTC1 AS MODULE NOW
CC               28-MAR-12 : RD: USE SVN2CHR AS MODULE NOW
CC               19-SEP-12 : SS: SKIP CALIBRATIONS FOR LATEST FREQUENCIES
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1990     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnErr, lfn001, lfnPrt,
     1                    fileNameLength, fileNameLength80,
     2                    keyValueLength
      USE m_cpu,    ONLY: cpu_start
      USE s_chr2svn
      USE d_const,  ONLY : date,time
      USE s_cordup
      USE s_defcon
      USE f_djul
      USE s_exitrc
      USE m_global, ONLY :g_atxsys, antGPS, antGLO, antGAL
      USE s_gtflna
      USE d_inpkey, ONLY : inpKey,init_inpkey
      USE f_iyear4
      USE f_listc1
      USE s_opnerr
      USE s_opnfil
      USE s_opnsys
      USE s_pritit
      USE s_prflna
      USE s_prn2svn
      USE s_rdaphc
      USE s_rdsatfil
      USE s_readcrux
      USE s_readinpf
      USE s_readkeys
      USE d_satfil, ONLY : t_satfil,init_satfil,typeMWTR,typeSLR
      USE d_stacrx, ONLY: t_stacrux, undef_c, undef_i
      USE s_svn2chr
      USE s_svn2prn
      USE m_time,   ONLY : t_timint
      USE s_wtaphc

      IMPLICIT NONE

C Dummy list
C ----------
      TYPE(t_stacrux), SAVE            :: stacrux
      TYPE(t_satfil), SAVE             :: satfil
      LOGICAL,      SAVE               :: first= .TRUE.
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IAZI  , ICOR  , IELV  , IFORM , IFRQ  , ILIN1 , ILIN2 ,
     1          ILIN3 , IOSTAT, IRCODE, IRCV  , ISKP  , ISZERO, JRCV  ,
     2          MAXAZI, MAXELV, MAXRCV, NOZERO, NRCV  , YYYY  , MM    ,
     3          DAY   , HH    , MI    , atx   , prn   , sat   ,abs2rel,
     4          backs ,convert,nGPSSAT, oNRCV , iMonth, hrcv  , fRCV  ,
     5          ARCV  , xyz   , ele   , svnr  , help  , iii   , kRCV  ,
     6          add   ,nGLOSAT,nGALSAT, idome , antcnt, RADCOD,antcnt2,
     7          MAXEL2,ANTNUM , aoa   , atx1
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C MAXIMAL DIMENSIONS
C ------------------
      PARAMETER (MAXRCV=1000,MAXELV=19,MAXAZI=73,MAXEL2=16)
C
C MAXRCV: MAXIMUM NUMBER OF RECEIVER/ANTENNA PAIRS RESP. OF ANTENNAS
C MAXELV: MAXIMUM NUMBER OF DIFFERENT ELEVATION ANGLES
C         (19 ALLOWS FOR ELEVATION INCREMENTS DOWN TO 5 DEG)
C MAXAZI: MAXIMUM NUMBER OF DIFFERENT AZIMUTH ANGLES
C         (73 ALLOWS FOR AZIMUTH INCREMENTS DOWN TO 5 DEG)
C
C
C DECLARATIONS
C ------------
      CHARACTER*132 LINE
      CHARACTER*152 XLINE
      CHARACTER*80  TITLE,FILINFO,oTITLE,oFILINFO
      CHARACTER*60  STRING
      CHARACTER*26  ANTHLP(MAXRCV)
      CHARACTER*20  RECTYP(MAXRCV),ANTTYP(MAXRCV),METH(MAXRCV)
      CHARACTER*20  BY(MAXRCV)
      CHARACTER*20  fRECTYP(MAXRCV),fANTTYP(MAXRCV),fMETH(MAXRCV)
      CHARACTER*20  oRECTYP(MAXRCV),oANTTYP(MAXRCV),oMETH(MAXRCV)
      CHARACTER*20  oBY(MAXRCV)
      CHARACTER*20  HEAD,INDTEST,sensor
      CHARACTER*20  outant(MAXRCV),NONEANT(MAXRCV)
      CHARACTER*10  COSPAR(MAXRCV),CSVN
      CHARACTER*10  atxDATE(MAXRCV),SINEX(MAXRCV)
      CHARACTER*10  fDATE(MAXRCV)  ,fSINEX(MAXRCV)
      CHARACTER*10  oDATE(MAXRCV)  ,oSINEX(MAXRCV)
      CHARACTER*13  FRMT2
      CHARACTER*8   FRMT1
      CHARACTER*5   ,Dimension(6)               :: Method
      CHARACTER*5   NOAZI
      CHARACTER*4   svnnr,SVNATX(MAXRCV),difnpcv,diffoff,diffpcv
      CHARACTER*3   ,Dimension(12)              :: Month
      CHARACTER*1   GNSS,sysflag,syst,filtyp,pcvtyp
      CHARACTER(LEN=fileNameLength)             :: filename,FILEXT2
      CHARACTER(LEN=fileNameLength80)           :: FILEXT,FILPHC,FILEXT1
      CHARACTER(LEN=keyValueLength),DIMENSION(:),POINTER:: keyValue
C
      REAL*8        ANTOFF(3,2,MAXRCV) ,ANTPCV(MAXELV,MAXAZI,2,MAXRCV)
      REAL*8        fANTOFF(3,2,MAXRCV),fANTPCV(MAXELV,MAXAZI,2,MAXRCV)
      REAL*8        oANTOFF(3,2,MAXRCV),oANTPCV(MAXELV,MAXAZI,2,MAXRCV)
      REAL*8        rANTOFF(3,2,MAXRCV)
      REAL*8        AOAPCV(MAXELV,2), AOADMTOFF(3,2)
      REAL*8        AOADMTPCV(MAXELV,MAXAZI,2)
      REAL*8        DAZI,ZEN1,ZEN2,DZEN,AZI,DD,SEC,epo,omjd,amjd,help1
C
      TYPE(t_timint),Dimension(MAXRCV)                   :: timint1
      TYPE(t_timint)                                     :: timcheck
C
      INTEGER*4     IANTEN(2,MAXRCV) ,NPCV(2,MAXRCV) ,MAXZEN(MAXRCV)
      INTEGER*4     MODTYP(MAXRCV)   ,NFRANT(MAXRCV)
      INTEGER*4     fIANTEN(2,MAXRCV),fNPCV(2,MAXRCV),fMAXZEN(MAXRCV)
      INTEGER*4     fMODTYP(MAXRCV)  ,fNFRANT(MAXRCV)
      INTEGER*4     oIANTEN(2,MAXRCV),oNPCV(2,MAXRCV),oMAXZEN(MAXRCV)
      INTEGER*4     oMODTYP(MAXRCV)  ,oNFRANT(MAXRCV)
      INTEGER*4     linesat(MAXRCV)
      INTEGER*4     INDEX(MAXRCV), INDEXf(MAXRCV),linatx(MAXRCV)
      INTEGER*4     ISSAT(MAXRCV), CODE(MAXRCV)  ,ONLYELE
      INTEGER*4     ATXFIL,MXFZEN,MXFNAD,NFPCV,FIZMOD,FINMOD,AOADMT
      INTEGER*4     irc,ios,prncheck,find,count,inone,info,nfound
      INTEGER*4     ocount,finidx,iMETH,oiMETH,ocoun2
C
C COMMON BLOCKS
C -------------
      COMMON/PLARGE/ANTOFF,ANTPCV,IANTEN,NFRANT,MODTYP,NPCV,
     1              RECTYP,ANTTYP
C
      Month = (/'JAN','FEB','MAR','APR','MAY','JUN',
     1          'JUL','AUG','SEP','OCT','NOV','DEC'/)
C
      Method = (/'     ','ADOPT','COPIE','CONVE','FIELD','ROBOT'/)
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C INIT VARIABLES
C --------------
      NULLIFY(keyValue)
C
C GET THE NAME OF THE INPUT FILE
C ------------------------------
      CALL init_inpkey(inpKey)
      CALL readinpf(' ',inpKey)
C
C DEFINE SYSTEM FILES
C -------------------
      CALL OPNSYS
C
C DEFINE CONSTANTS
C ----------------
      CALL DEFCON(1)
C
C Write title and file list
C -------------------------
      CALL pritit('PHCCNV',
     1            'Convert External Into Bernese Phase Center File')
      CALL prflna
C
C GET FILENAME OF EXTERNAL PHC FILE
C ---------------------------------
      CALL GTFLNA(1,'PHASEXT',FILEXT,IRCODE)
C
C OPEN EXTERNAL FILE
C ------------------
      CALL OPNFIL(LFN001,FILEXT,'OLD','FORMATTED',
     1            'READONLY',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFN001,IOSTAT,FILEXT,'PHCCNV')
C
C GET FILENAME OF OLD PHASE FILE
C ------------------------------
      CALL GTFLNA(0,'PHASOLD',FILEXT1,IRCODE)
C
C GET FILENAME OF STATION INFIORMATION FILE
C -----------------------------------------
      CALL GTFLNA(0,'STAENLA',FILEXT2,IRCODE)
C
C READ OPTION FOR WRITING ZERO PATTERNS
      CALL READKEYS('NOZERO', keyValue, irc)
      IF (irc == 0 .AND. keyValue(1) == '1') NOZERO = 1
C
C Read option for type of pattern
      CALL READKEYS('ONLYELE', keyValue, irc)
      IF (irc == 0 .AND. keyValue(1) == '1') ONLYELE = 1
C
C Read wether abs2rel or not
      ABS2REL=0
      CALL READKEYS('ABS2REL', keyValue, irc)
      IF (irc == 0 .AND. keyValue(1) == '1') ABS2REL = 1
C
C Read whether conversion or not
      IF (ABS2REL == 0) THEN
        CALL READKEYS('CONVERT', keyValue, irc)
        IF (irc == 0 .AND. keyValue(1) == '1') CONVERT = 1
      ENDIF
C
C Read whether consideration of antennas without radome code
      CALL READKEYS('RADCOD', keyValue, irc)
      IF (irc == 0 .AND. keyValue(1) == '1') RADCOD = 1
C
C Read option for numbering of general antennas (0 or 999999)
      CALL READKEYS('ANTNUM', keyValue, irc)
      IF (irc == 0 .AND. keyValue(1) == '1') ANTNUM = 1
C
C DETECT FORMAT
C -------------
      IFORM=0
      READ(LFN001,'(A)') LINE
      IF (LINE(61:80).EQ.'ANTEX VERSION / SYST') THEN
        IF (LINE(6:8).EQ.'1.1'.OR.
     1      LINE(6:8).EQ.'1.2'.OR.
     2      LINE(6:8).EQ.'1.3') THEN
C ANTEX (Version 1.1, 1.2, 1.3)
          IFORM=2
          READ(LINE(21:21),'(A1)')syst
        ELSE
          WRITE(LFNERR,912) TRIM(FILEXT),LINE(6:8)
912       FORMAT(/,' *** PG PHCCNV: UNKNOWN ANTEX FORMAT VERSION.',
     1           /,16X,'FILE NOT CONVERTED!',
     2           /,16X,'FILE NAME    : ',A,
     3           /,16X,'ANTEX VERSION: ',A,/)
          IRCODE=2
          GOTO 999
        ENDIF
      ELSE
        DO IRCV=1,100000
          READ(LFN001,'(A)',END=9) LINE
          IF (LINE(1:26).EQ.' [north]  [ east]  [  up ]') THEN
C FORMAT BY GERRY MADER
            IFORM=1
            EXIT
          ENDIF
        ENDDO
9       IF (IFORM==0) THEN
          CLOSE(UNIT=LFN001)
          WRITE(LFNERR,901) TRIM(FILEXT)
901       FORMAT(/,' *** PG PHCCNV: UNKNOWN ANTENNA PHASE CENTER ',
     1             'FILE FORMAT.',
     2           /,16X,'FILE NOT CONVERTED!',
     3           /,16X,'FILE NAME: ',A,/)
          IRCODE=2
          GOTO 999
        ENDIF
      ENDIF
C
C FORMAT BY GERRY MADER
C =====================
      IF (IFORM.EQ.1) THEN
C
C SKIP HEADER LINES
C -----------------
        DO ISKP=1,1000000
          READ(LFN001,'(A)',END=910) LINE
          IF (LINE.EQ.' ') GOTO 10
        ENDDO
10      CONTINUE
C
C LOOP OVER ALL ANTENNAS AVAILABLE IN EXTERNAL FILE
C -------------------------------------------------
        IRCV=0
        DO
          READ(LFN001,101,ERR=920,END=110) INDTEST
101       FORMAT(A20)
          IF (INDTEST.EQ.' ') GOTO 110
C
          IRCV=IRCV+1
          IF (IRCV.GT.MAXRCV) THEN
            WRITE(LFNERR,9106) FILEXT,MAXRCV
9106        FORMAT(/,' *** PG PHCCNV: TOO MANY ANTENNAS IN FILE.',
     1                   /,16X,'FILE NAME    : ',A,
     2                   /,16X,'MAXRCV       : ',I6,/)
            IRCODE=2
            GOTO 999
          ENDIF
C
          RECTYP(IRCV)=INDTEST
          DO IFRQ=1,2
            READ(LFN001,*,ERR=920,END=930)
     1                          (ANTOFF(ICOR,IFRQ,IRCV),ICOR=1,3)
            READ(LFN001,*,ERR=920,END=930)
     1                          (ANTPCV(IELV,1,IFRQ,IRCV),IELV= 1,10)
            READ(LFN001,*,ERR=920,END=930)
     1                          (ANTPCV(IELV,1,IFRQ,IRCV),IELV=11,19)
C
C CONVERSION TO METERS
            DO ICOR=1,3
              ANTOFF(ICOR,IFRQ,IRCV)=ANTOFF(ICOR,IFRQ,IRCV)/1000.D0
            ENDDO
            DO IELV=1,19
              ANTPCV(IELV,1,IFRQ,IRCV)=ANTPCV(IELV,1,IFRQ,IRCV)/1000.D0
            ENDDO
          ENDDO
        ENDDO
C
C END OF LOOP, CLOSE FILE
C -----------------------
110     NRCV=IRCV
        CLOSE(UNIT=LFN001)
C
C ADD ADDITIONAL INFORMATION NECESSARY
C ------------------------------------
        TITLE=' '
        FILINFO=' '
        TITLE(1:64)='ANTENNA PHASE CENTER VARIATIONS BY GERRY MADER'
        TITLE(66:74)=DATE
        TITLE(76:80)=TIME
        count=0
C
        DO IRCV=1,NRCV
          IF(ANTNUM == 1)THEN
            IANTEN(1,IRCV)=undef_i
          ELSE
            IANTEN(1,IRCV)=0
          ENDIF
          IANTEN(2,IRCV)=undef_i
          ANTTYP(IRCV)=' '
          NFRANT(IRCV)=2
          MODTYP(IRCV)=1
          NPCV(1,IRCV)=19
          NPCV(2,IRCV)=1
          MAXZEN(IRCV)=90
          SINEX(IRCV) =' '
          ISSAT(IRCV)=0
          METH(IRCV)=' '
          atxDATE(IRCV)=' '

C DO NOT WRITE PCV IF ALL ARE ZERO
          IF (NOZERO.EQ.1) THEN
            ISZERO=1
            DO IFRQ=1,2
              DO IELV=1,19
                IF (ANTPCV(IELV,1,IFRQ,IRCV) /= 0D0) ISZERO=0
              ENDDO
            ENDDO
            IF (ISZERO==1) MODTYP(IRCV)=0
          ENDIF
        ENDDO
C
C ANTEX (Version 1.3)
C ===================
C NOT READ: - HEADER
C           - COMMENTS
C           - RMS VALUE SECTION
C IANTEN (SERIAL No.) HARD-WIRED, IF LT 0 or GT 999999 OR no INTEGER
C CHECK WHETHER DATA COMPLETE
C WRITE OUTPUT FILE
C
      ELSEIF (IFORM.EQ.2) THEN
C
C If called for the first time, read the entire SATELLIT. file
C ============================================================
        IF (first) THEN
          first = .FALSE.

C Get the satellite info file name
          CALL gtflna(1,'SATELL ',filename,IRC)

C Read satellite info file (SATELL)
          CALL init_satfil(satfil)
          CALL rdsatfil(filename,satfil)
        END IF
C
C SKIP HEADER
C -----------
        DO IRCV=1,MAXRCV
          ISSAT(IRCV)=0
        ENDDO
        DO ISKP=1,1000000
          READ(LFN001,'(A)',END=940) LINE
          IF (LINE(61:80).EQ.'PCV TYPE / REFANT   ')THEN
            READ(LINE,'(A1)')pcvtyp
          ELSEIF (LINE(61:80).EQ.'END OF HEADER       ') THEN
C
C INITIALIZE IRCV,AOADMT,FRMT1,FRMT2
            IRCV=0
            nGPSSAT=0
            nGLOSAT=0
            nGALSAT=0
            AOADMT=0
            FRMT1='(##F8.2)'
            FRMT2='(F8.1,##F8.2)'
            GOTO 20
          ENDIF
        ENDDO
20      CONTINUE
C
C LOOP OVER ALL LINES OF THE FILE
C -------------------------------
        DO ILIN1=1,1000000
          READ(LFN001,201,ERR=920,END=210) STRING,HEAD
201       FORMAT(A60,A20)
          IF (HEAD.EQ.'START OF ANTENNA    ') THEN
            IRCV=IRCV+1
            IF (IRCV.GT.MAXRCV) THEN
              WRITE(LFNERR,9206) FILEXT,MAXRCV
9206          FORMAT(/,' *** PG PHCCNV: TOO MANY ANTENNAS IN FILE!',
     1                   /,16X,'FILE NAME    : ',A,
     2                   /,16X,'MAXRCV       : ',I6,/)
              IRCODE=2
              GOTO 999
            ENDIF
C
            SINEX(IRCV)=' '
            METH(IRCV)=' '
            atxDATE(IRCV)=' '
            timint1(ircv)%t(1)=0.0D0
            timint1(ircv)%t(2)=1.0D20
            IF(ANTNUM == 1)THEN
              IANTEN(1,IRCV)=undef_i
            ELSE
              IANTEN(1,IRCV)=0
            ENDIF
            IANTEN(2,ircv)=undef_i
C
C LOOP OVER ALL LINES OF ONE ANTENNA SECTION
C ------------------------------------------
            DO ILIN2=1,1000000
              READ(LFN001,201,ERR=920,END=930) STRING,HEAD
C
              IF (HEAD.EQ.'TYPE / SERIAL NO    ') THEN
                READ(STRING,250,ERR=950) ANTTYP(IRCV),INDTEST,CSVN,
     1                                   COSPAR(IRCV)
250             FORMAT(2A20,2A10)
                outant(IRCV)=INDTEST
                INDTEST = ADJUSTL(INDTEST)

C INITIALIZE RECTYP
                RECTYP(IRCV)=' '
C
C SAVE INDICES OF SATELLITE ANTENNA BLOCKS
                IF (ANTTYP(IRCV)(1:5).EQ. antGPS .OR.
     1              ANTTYP(IRCV)(1:7).EQ. antGLO .OR.
     1              ANTTYP(IRCV)(1:7).EQ. antGAL) THEN
                  ISSAT(IRCV)=1
                  IF(ANTTYP(IRCV)(1:5).EQ. antGPS)nGPSSAT=nGPSSAT+1
                  IF(ANTTYP(IRCV)(1:7).EQ. antGLO)nGLOSAT=nGLOSAT+1
                  IF(ANTTYP(IRCV)(1:7).EQ. antGAL)nGALSAT=nGALSAT+1
                  IF (INDTEST /= ' ') THEN
                    READ(INDTEST,202,iostat=ios)sysflag,prn
202                 FORMAT(A1,I2)
                    IF (ios==0 .AND.
     1                     INDTEST(4:20)=='                 ') THEN
                      CALL chr2svn(prn,sysflag,code(ircv))
                    ELSE
                      WRITE(lfnerr,203)INDTEST
203                   FORMAT(/,' *** PG PHCCNV: Wrong satellite code ',
     1                               '(PRN).',/,16x,'Read code:',A)
                      CALL exitrc(2)
                    ENDIF
                  ELSE
                    code(ircv)=0
                  ENDIF
                  IF (CSVN /= '          ') THEN
                    READ(CSVN(1:4),'(A4)') svnatx(ircv)
                    IF (CSVN(1:1) /= INDTEST(1:1)) THEN
                      WRITE(lfnerr,204)INDTEST
204                   FORMAT(/,' ### PG PHCCNV: Wrong satellite code ',
     1                              '(SVN).',/,16x,'SVN set to blank.',
     2                                        /,16x,'Read code:',A)
                      svnatx(ircv)='    '
                    ENDIF
                  ELSE
                    svnatx(ircv)='    '
                  ENDIF
                ELSE
                  ISSAT(IRCV)=0

C READ ANTANNA NUMBER IF RECEIVER ANTENNA
C
                  INDTEST = ADJUSTR(INDTEST)
                  IF (INDTEST(20:20) /= ' ') THEN
C 6 DIGITS FOR BERNESE PCV
CC                    READ(INDTEST,'(14X,I6)',iostat=ios) code(ircv)
C 5 DIGITS FOR SINEX
                    READ(INDTEST,'(15X,I5)',iostat=ios) code(ircv)
                    IF (ios /= 0 .OR. code(ircv) < 0) THEN
                      code(ircv) = 0
                      WRITE(lfnerr,257)INDTEST
257                   FORMAT(/,' ### PG PHCCNV: Antenna number set to 0',
     1                       /,16X,'Read number: ',A20,/)
                    ELSE
                      IANTEN(1,IRCV)=code(IRCV)
                      IANTEN(2,IRCV)=code(IRCV)
                    ENDIF
                  ENDIF

C SAVE INDICES OF AOAD/M_T
                  IF (INDTEST == ' ' .AND.
     1               (ANTTYP(IRCV) == 'AOAD/M_T            ' .OR.
     2                ANTTYP(IRCV) == 'AOAD/M_T        NONE')) THEN
                    AOADMT=IRCV
                  ENDIF
                ENDIF
C
              ELSEIF (HEAD.EQ.'METH / BY / # / DATE') THEN
                READ(STRING,258,ERR=950)METH(IRCV),BY(IRCV),
     1                                             atxDATE(IRCV)
258             FORMAT(2A20,10X,A10)
C
              ELSEIF (HEAD.EQ.'DAZI                ') THEN
                READ(STRING,251,ERR=950) DAZI
251             FORMAT(2X,F6.1,52X)
C
C NUMBER OF AZIMUTH VALUES
                IF (DAZI .EQ. 0.D0) THEN
                  NPCV(2,IRCV)=1
                ELSE
                  NPCV(2,IRCV)=IDNINT(360.D0/DAZI)+1
                ENDIF
                IF (NPCV(2,IRCV).GT.MAXAZI) THEN
                  WRITE(LFNERR,906) FILEXT,ANTTYP(IRCV),
     1                              NPCV(2,IRCV),MAXAZI
906               FORMAT(/,' *** PG PHCCNV: TOO MANY DIFFERENT',
     1                     ' AZIMUTH ANGLES.',
     2                   /,16X,'FILE NOT CONVERTED!',
     3                   /,16X,'FILE NAME          : ',A32,
     4                   /,16X,'ANTENNA TYPE       : ',A20,
     5                   /,16X,'# OF AZIMUTH ANGLES:',I3,
     6                   /,16X,'MAX # ALLOWED      :',I3,
     7                   /,16X,'INCREASE MAXAZI!',/)
                  IRCODE=2
                  GOTO 999
                ENDIF
C
              ELSEIF (HEAD.EQ.'ZEN1 / ZEN2 / DZEN  ') THEN
                READ(STRING,252,ERR=950) ZEN1,ZEN2,DZEN
252             FORMAT(2X,3F6.1,40X)
                IF (DZEN .EQ. 0.D0) THEN
                  WRITE(LFNERR,902) FILEXT,ANTTYP(IRCV),DZEN
902               FORMAT(/,' *** PG PHCCNV: INVALID VALUE FOR',
     1                     ' ZENITH INCREMENT IN EXTERNAL FILE.',
     2                   /,16X,'FILE NOT CONVERTED!',
     3                   /,16X,'FILE NAME    : ',A32,
     4                   /,16X,'ANTENNA TYPE : ',A20,
     5                   /,16X,'ZEN INCREMENT:',F6.1,/)
                  IRCODE=2
                  GOTO 999
                ELSE
C
C NUMBER OF ELEVATION VALUES
                  NPCV(1,IRCV)=IDNINT((ZEN2-ZEN1)/DZEN)+1
                ENDIF
                IF (NPCV(1,IRCV).GT.MAXELV) THEN
                  WRITE(LFNERR,903) FILEXT,ANTTYP(IRCV),
     1                              NPCV(1,IRCV),MAXELV
903               FORMAT(/,' *** PG PHCCNV: TOO MANY DIFFERENT',
     1                     ' ZENITH/NADIR ANGLES.',
     2                   /,16X,'FILE NOT CONVERTED!',
     3                   /,16X,'FILE NAME         : ',A32,
     4                   /,16X,'ANTENNA TYPE      : ',A20,
     5                   /,16X,'# OF ZENITH ANGLES:',I3,
     6                   /,16X,'MAX # ALLOWED     :',I3,
     7                   /,16X,'INCREASE MAXELV AND XLINE!',/)
                  IRCODE=2
                  GOTO 999
                ENDIF
                MAXZEN(IRCV)=IDNINT(ZEN2)
C
C NUMBER OF FREQUENCIES
              ELSEIF (HEAD.EQ.'# OF FREQUENCIES    ') THEN
                READ(STRING,253,ERR=950) NFRANT(IRCV)
253             FORMAT(I6,54X)
C
              ELSEIF (HEAD.EQ.'VALID FROM          ') THEN
                IF (STRING /= ' ') THEN
                  READ(STRING,"(2I6,I6,2I6,F13.7)",iostat=ios)
     1                  YYYY,MM,DAY,HH,MI,SEC
                  DD=DAY+HH/24.D0+MI/1440.D0+sec/86400.D0
                  timint1(ircv)%t(1) = djul(YYYY,MM,DD)
                ENDIF
C
              ELSEIF (HEAD.EQ.'VALID UNTIL         ') THEN
                IF (STRING /= ' ') THEN
                  READ(STRING,"(2I6,I6,2I6,F13.7)",iostat=ios)
     1                  YYYY,MM,DAY,HH,MI,SEC
                  DD=DAY+HH/24.D0+MI/1440.D0+sec/86400.D0
                  timint1(ircv)%t(2) = djul(YYYY,MM,DD)
                ENDIF
C
              ELSEIF (HEAD.EQ.'SINEX CODE          ') THEN
                READ (STRING,"(A10)") SINEX(IRCV)
C
              ELSEIF (HEAD.EQ.'COMMENT             ') THEN
                CYCLE
C
              ELSEIF (HEAD.EQ.'START OF FREQUENCY  ') THEN
                READ(STRING,254,ERR=950) GNSS,IFRQ
254             FORMAT(3X,A1,I2,54X)
                IF (ISSAT(IRCV). EQ. 0 .AND. IFRQ .GT. 2) GNSS='_'
C
C SKIP GLONASS FREQUENCIES FOR RECEIVER ANTENNAS
                IF (ISSAT(IRCV) .EQ. 0 .AND. GNSS .NE. 'G' .AND.
     1                                                 GNSS.NE.' ') THEN
                  WRITE(LFNERR,934) FILEXT,ANTTYP(IRCV),GNSS,IFRQ
934               FORMAT(/,' ### PG PHCCNV: SKIPPING NON-GPS',
     1                     ' FREQUENCY',
     3                   /,16X,'FILE NAME   : ',A32,
     4                   /,16X,'ANTENNA TYPE: ',A20,
     5                   /,16X,'FREQUENCY   : ',A1,I2.2/)
                  NFRANT(IRCV)=NFRANT(IRCV)-1
                  DO
                    READ(LFN001,201,ERR=960,END=930) STRING,HEAD
                    IF (HEAD.EQ.'END OF FREQUENCY    ') GOTO 290
                  ENDDO
                ENDIF
C
                IF (IFRQ.GT.NFRANT(IRCV) .OR. IFRQ.LT.0) THEN
                  WRITE(LFNERR,904) FILEXT,ANTTYP(IRCV),IFRQ
904               FORMAT(/,' *** PG PHCCNV: INVALID FREQUENCY',
     1                     ' NUMBER IN EXTERNAL FILE.',
     2                   /,16X,'FILE NOT CONVERTED!',
     3                   /,16X,'FILE NAME   : ',A32,
     4                   /,16X,'ANTENNA TYPE: ',A20,
     5                   /,16X,'FREQUENCY NO:',I2,/)
                  IRCODE=2
                  GOTO 999
                ENDIF
C
C READ ALL INFORMATION WITHIN FREQUENCY SECTION
C ---------------------------------------------
                READ(LFN001,201,ERR=960,END=930) STRING,HEAD
                IF (HEAD.NE.'NORTH / EAST / UP   ') GOTO 960
                READ(STRING,255,ERR=950)
     1                            (ANTOFF(ICOR,IFRQ,IRCV),ICOR=1,3)
255             FORMAT(3F10.2,30X)
C
C OFFSETS: CONVERSION TO METERS
C
                DO ICOR=1,3
                  ANTOFF(ICOR,IFRQ,IRCV)=ANTOFF(ICOR,IFRQ,IRCV)/1000.D0
                  IF(ISSAT(IRCV)/=1.AND.ANTOFF(ICOR,IFRQ,IRCV)>0)
     1              ANTOFF(ICOR,IFRQ,IRCV)=ANTOFF(ICOR,IFRQ,IRCV)+1.D-7
                  IF(ISSAT(IRCV)/=1.AND.ANTOFF(ICOR,IFRQ,IRCV)<0)
     1              ANTOFF(ICOR,IFRQ,IRCV)=ANTOFF(ICOR,IFRQ,IRCV)-1.D-7
                  IF(ANTOFF(ICOR,IFRQ,IRCV) < 0.D0 .AND.
     1              ANTOFF(ICOR,IFRQ,IRCV)>-0.00005)
     2                    ANTOFF(ICOR,IFRQ,IRCV) = 0.D0
                ENDDO
C
                READ(LFN001,256,ERR=960,END=930) NOAZI,XLINE
256             FORMAT(3X,A5,A152)
                IF (NOAZI.NE.'NOAZI') GOTO 960
C
                IF (NPCV(2,IRCV).EQ.1 .OR. ONLYELE == 1 .OR.
     1                           AOADMT == IRCV) THEN
                  WRITE(FRMT1(2:3),'(I2)',ERR=970) NPCV(1,IRCV)
                  READ(XLINE,FRMT1,ERR=960,END=930)
     1                (ANTPCV(IELV,1,IFRQ,IRCV),IELV=1,NPCV(1,IRCV))
C
C NON-AZIMUTH-DEPENDENT PATTERNS: CONVERSION TO METERS
                  DO IELV=1,NPCV(1,IRCV)
                    ANTPCV(IELV,1,IFRQ,IRCV)=
     1                              ANTPCV(IELV,1,IFRQ,IRCV)/1000.D0
                    IF(AOADMT == IRCV)THEN
                      AOAPCV(IELV,IFRQ) = ANTPCV(IELV,1,IFRQ,IRCV)
                    ENDIF
                  ENDDO
                  IF (ONLYELE == 1 .AND. NPCV(2,IRCV).NE.1) THEN
                    DO IAZI=1,NPCV(2,IRCV)
                      READ(LFN001,'(A152)',ERR=960,END=930) XLINE
                    ENDDO
                  ENDIF
                ENDIF
                IF(NPCV(2,IRCV) /= 1 .AND. ONLYELE /= 1) THEN
                  WRITE(FRMT2(7:8),'(I2)',ERR=970) NPCV(1,IRCV)
                  DO IAZI=1,NPCV(2,IRCV)
                    READ(LFN001,FRMT2,ERR=960,END=930) AZI,
     1                (ANTPCV(IELV,IAZI,IFRQ,IRCV),IELV=1,NPCV(1,IRCV))
                    IF ((DBLE(IAZI-1)*DAZI).NE.AZI) GOTO 960
C
C AZIMUTH-DEPENDENT PATTERNS: CONVERSION TO METERS
                    DO IELV=1,NPCV(1,IRCV)
                      ANTPCV(IELV,IAZI,IFRQ,IRCV)=
     1                              ANTPCV(IELV,IAZI,IFRQ,IRCV)/1000.D0
                    ENDDO
                  ENDDO
                ENDIF
C
                READ(LFN001,201,ERR=960,END=930) STRING,HEAD
                IF (HEAD.NE.'END OF FREQUENCY    ') GOTO 960
290             CONTINUE
C
C END OF FREQUENCY SECTION
C ------------------------
C
              ELSEIF (HEAD.EQ.'START OF FREQ RMS   ') THEN
                DO ILIN3=1,1000000
                  READ(LFN001,201,ERR=920,END=930) STRING,HEAD
                  IF (HEAD.EQ.'END OF FREQ RMS     ') EXIT
                ENDDO
                CYCLE
C
              ELSEIF (HEAD.EQ.'END OF ANTENNA      ') THEN
C
C IGNORE RECEIVER ANTENNAS WITHOUT GPS FREQUENCIES
                IF (NFRANT(IRCV).EQ.0) IRCV=IRCV-1
                EXIT
C
              ELSEIF (HEAD(1:16).EQ.'TYPE / SERIAL NO'  .OR.
     1                HEAD(1:14).EQ.'END OF ANTENNA'    .OR.
     2                HEAD(1:16).EQ.'# OF FREQUENCIES'  .OR.
     3                HEAD(1:16).EQ.'END OF FREQUENCY'  .OR.
     4                HEAD(1:18).EQ.'START OF FREQUENCY'.OR.
     5                HEAD(1:4) .EQ.'DAZI'              .OR.
     6                HEAD(1:18).EQ.'ZEN1 / ZEN2 / DZEN'.OR.
     7                HEAD(1:17).EQ.'NORTH / EAST / UP')THEN
                WRITE(LFNERR,913) FILEXT,HEAD
                IRCODE=2
                GOTO 999
C
              ELSE
                WRITE(LFNERR,905) FILEXT,HEAD
905             FORMAT(/,' *** PG PHCCNV: UNKNOWN HEADER LABEL IN',
     1                   ' EXTERNAL FILE.',
     2                 /,16X,'FILE NOT CONVERTED!',
     3                 /,16X,'FILE NAME   : ',A32,
     4                 /,16X,'HEADER LABEL: ',A20,/)
                IRCODE=2
                GOTO 999
              ENDIF
            ENDDO
          ELSEIF (HEAD(1:16).EQ.'START OF ANTENNA') THEN
            WRITE(LFNERR,913) FILEXT,HEAD
913         FORMAT(/,' *** PG PHCCNV: MISSING BLANKS AT THE END OF',
     1                   ' HEADER LABEL IN ANTEX FILE.',
     2                 /,16X,'ANTEX FILE DOES NOT CORRESPOND TO ',
     3                       'ANTEX FORMAT DESCRIPTION.',
     4                 /,16X,'FILE NOT CONVERTED!',
     5                 /,16X,'FILE NAME   : ',A32,
     6                 /,16X,'HEADER LABEL: ',A20,/)
            IRCODE=2
            GOTO 999
          ENDIF
        ENDDO
C
C END OF LOOP OVER ALL LINES OF THE FILE, CLOSE FILE
C --------------------------------------------------
210     CLOSE(UNIT=LFN001)
C
        IF(AOADMT==0 .AND. (CONVERT == 1 .OR. ABS2REL == 1))THEN
          WRITE(lfnerr,"(/,' *** PG PHCCNV: Antenna [AOAD/M_T    ',
     1                    '   NONE] not found in ANTEX file.',
     2                    /,16X,'Phase file name: ',A40,
     3                    /,16X,'Phase file not convertable!')"
     4                    )FILEXT
          CALL EXITRC(2)
        ENDIF
C
        IF(nGPSSAT==0 .AND. nGLOSAT==0 .AND. nGALSAT==0) THEN
          WRITE(lfnerr,"(/,' ### PG PHCCNV: No satellite PCV ',
     1                    'found in ANTEX file.'
     2                    /,16X,'ANTEX file name: ',A40,
     3                    /,16X,'No entry in new phase file if no ',
     4                          'entry in input phase file!')"
     5                    )FILEXT
        ENDIF
C
C leading LINES for SATELLITE ANTENNA OUTPUT
        WRITE(lfnprt,"(' Summary of translated patterns for sensors in',
     1                 ' satellite file',/,
     2                 ' ---------------------------------------------',
     3                 '----------------',/,
     4                 '             SATELLIT.          ',
     5                 '                     .ATX                   ',
     6                 'ANTOFF FRQ 1            ANTOFF FRQ 2',/,
     7                 ' sensor name         number PRN  SVN <-- ',
     8                 'sensor name          PRN  SVN   NORTH   EAST ',
     9                 '    UP     NORTH   EAST     UP')")
        WRITE(lfnprt,"(' -------------------- ----- --- ----     ',
     1                '-------------------- --- ----  ------- -------',
     2                 ' ------- ------- ------- -------',/)")
C
C Find internal sensor name in SATELLIT. file
C -------------------------------------------
        DO sat=1,satfil%nsensor
          IF(satfil%sensor(sat)%sensor(1:12)=='MW TRANSM. B'.OR.
     1       satfil%sensor(sat)%sensor(1:12)=='MW TRANSM. G')THEN
            WRITE(lfnerr,"(/,' *** PG PHCCNV: Satellite information ',
     1                  'file with old satellite antenna names.',
     2                  /,16X,'Satellite file name: ',A40,
     3                  /,16X,'Please use new satellite file from CODE!',
     4                  /,16X,'Phase file not converted!')"
     5                  )filename
            CALL EXITRC(2)
          ENDIF
          epo=0.D0
          find=0
          linatx(sat)=0
          DO atx=1,IRCV
            IF (ISSAT(atx) == 1) THEN
              IF (satfil%sensor(sat)%name == ANTTYP(atx)) THEN
C
C If SVN is given, check consistency between PRN and time window
                IF (svnatx(atx) /= '    ' .AND. svnatx(atx)(2:4)
     1                          ==satfil%sensor(sat)%sensor(18:20)) THEN
                  IF (timint1(atx)%t(1) /= 0.0) THEN
                    epo=timint1(atx)%t(1)
C                                         +timint1(atx)%t(2))/2
                  ENDIF
                  CALL SVN2PRN(0,svnatx(atx),epo,prncheck,timcheck,irc)
                  IF (code(atx) /= 0) THEN
                    IF (prncheck /= code(atx))THEN
                      WRITE(lfnerr,"(/,' *** PG PHCCNV: Given SVN and',
     1                               ' PRN inconsistent in ANTEX file.',
     2                               /,16X,'File not converted!',
     3                               /,16X,'PRN: ',I3,/,16X,'SVN: ',A4)
     4                               ")code(atx),svnatx(atx)
                      CALL exitrc(2)
                    ENDIF
C                  ELSE
C                    code(atx)=prncheck
                  ENDIF
                  IF (timint1(atx)%t(1) /= 0.0D0 .AND.
     1              satfil%sensor(sat)%timint%t(1)==timint1(atx)%t(1)
     2                                                             )THEN
                    IF (satfil%sensor(sat)%timint%t(2)>timint1(atx)%t(2)
     1                                                             )THEN
                      WRITE(lfnerr,"(/,' *** PG PHCCNV: Time window ',
     1                         'in satellite information file not ',
     2                       /,16X,'equal to those in ANTEX file.',
     3                       /,16X,'File not converted!',
     4                       /,16X,'ANTEX sensor name: ',A,I3)"
     5                            )ANTTYP(atx),prncheck
                      CALL exitrc(2)
                    ENDIF
                  ENDIF
                ENDIF
C
C keep in mind the corresponding line index of ATX array for sensor in SAT file
                IF (satfil%sensor(sat)%timint%t(1) >=
     1                                  timint1(atx)%t(1) .AND.
     2              satfil%sensor(sat)%timint%t(2) <=
     3                                  timint1(atx)%t(2) .AND.
     4                  satfil%sensor(sat)%svn == code(atx)) THEN
                  IF (find < 4) THEN
                    linatx(sat) = atx
                    find = 4
                  ELSEIF (find == 4) THEN
                    WRITE(lfnerr,206)satfil%sensor(sat)%sensor
                    CALL exitrc(2)
                  ENDIF
                ENDIF
                IF (satfil%sensor(sat)%timint%t(1) >=
     1                                  timint1(atx)%t(1) .AND.
     2              satfil%sensor(sat)%timint%t(2) <=
     3                                      timint1(atx)%t(2)) THEN
                  IF (find < 3 .AND. code(atx) == 0) THEN
                    linatx(sat) = atx
                    find = 3
                  ELSEIF (find == 3 .AND. code(atx) == 0) THEN
                    WRITE(lfnerr,206)satfil%sensor(sat)%sensor
                    CALL exitrc(2)
                  ENDIF
                ENDIF
                IF (satfil%sensor(sat)%svn == code(atx)) THEN
                  IF (find < 2 .AND. svnatx(atx) == '    ' .AND.
     1                                      timint1(atx)%t(1) == 0) THEN
                    linatx(sat) = atx
                    find = 2
                  ELSEIF (find == 2 .AND. svnatx(atx) == '    ' .AND.
     1                                      timint1(atx)%t(1) == 0) THEN
                    WRITE(lfnerr,206)satfil%sensor(sat)%sensor
                    CALL exitrc(2)
                  ENDIF
                ENDIF
                IF (find == 0 .AND. code(atx) == 0 .AND.
     1            svnatx(atx)=='   ' .AND. timint1(atx)%t(1) == 0) THEN
                  linatx(sat) = atx
                  find = 1
                ELSEIF (find == 1 .AND. code(atx) == 0 .AND.
     1            svnatx(atx)=='    ' .AND. timint1(atx)%t(1) == 0) THEN
                    WRITE(lfnerr,206)satfil%sensor(sat)%sensor
                    CALL exitrc(2)
206                 FORMAT(/,' *** PG PHCCNV: More than one entry in',
     1                           ' ANTEX file for sensor in',
     2                     /,16X,' satellite information file!',
     3                     /,16X,'File not converted!',
     4                     /,16X,'sensor name: ',A)
                ENDIF
              ENDIF
            ENDIF
            IF (atx == IRCV .AND. find == 0 .AND.
     1                            satfil%sensor(sat)%name /= ' ') THEN
              IF ((syst == g_atxsys(0) .OR. syst == g_atxsys(1).OR. syst
     1           == g_atxsys(10)) .AND. satfil%sensor(sat)%name(1:5) ==
     2                          antGPS .AND. .NOT.
     3              (nGPSSAT==0 .AND. nGLOSAT==0 .AND. nGALSAT==0)) THEN
                WRITE(lfnerr,207)satfil%sensor(sat)%sensor,
     1                satfil%sensor(sat)%name,satfil%sensor(sat)%svn
C                IRCODE=2
C                GOTO 999
              ELSEIF ((syst==g_atxsys(2) .OR. syst==g_atxsys(10)) .AND.
     1             satfil%sensor(sat)%name(1:7) == antGLO) THEN
                WRITE(lfnerr,207)satfil%sensor(sat)%sensor,
     1                satfil%sensor(sat)%name,satfil%sensor(sat)%svn
C                IRCODE=2
C                GOTO 999
              ELSEIF ((syst==g_atxsys(3) .OR. syst==g_atxsys(10)) .AND.
     1             satfil%sensor(sat)%name(1:7) == antGAL) THEN
                WRITE(lfnerr,207)satfil%sensor(sat)%sensor,
     1                satfil%sensor(sat)%name,satfil%sensor(sat)%svn
C                IRCODE=2
C                GOTO 999
207           FORMAT(/,' ### PG PHCCNV: No entry found in ANTEX file',
     1                    ' for sensor in satellite information file.',
     2               /,16X,'Sensor name: ',A,
     3               /,16X,'Antex name : ',A,
     4               /,16X,'PRN number : ',I3,
     5               /,16X,'Antenna not included in new phase file ',
     6                     'if no entry in input phase file!')
              ENDIF
            ENDIF
          ENDDO
C
C WRITE OUTPUT FOR Satellite ANTENNAS AND CALCULATING THE DIFFERENCES
C                                               TO OFFSETS IN SATELLIT. file
          epo=(satfil%sensor(sat)%timint%t(1)+
     1                                 satfil%sensor(sat)%timint%t(2))/2
          CALL prn2svn(1,satfil%sensor(sat)%svn,epo,svnnr,timcheck,irc)
          IF (linatx(sat) /= 0) THEN
            DO ICOR=1,3
              DO IFRQ=1,2
                IF (abs2rel == 1) THEN
                 ANTOFF(ICOR,IFRQ,linatx(sat)) =
     1                                  satfil%sensor(sat)%antoff(ICOR)
                  ANTPCV(:,:,:,linatx(sat)) = 0d0
                ENDIF
                rANTOFF(ICOR,IFRQ,sat)=1.D-15 + ANTOFF(ICOR,
     1           IFRQ,linatx(sat)) - satfil%sensor(sat)%antoff(ICOR)
                IF (ABS(rANTOFF(ICOR,IFRQ,sat))>1.D-10)THEN
                  WRITE(lfnerr,"(' *** PG PHCCNV: Antenna offsets are ',
     1                      'different in SATELLIT.xxx and ANTEX file!',
     2                     /,16X,'new phase file!',
     3                     /,16X,'Sensor name: ',A20,
     4                     /,16X,'File not converted! Check ',
     5                           'SATELLIT.xxx or ANTEX file!')")
     6                                         satfil%sensor(sat)%sensor
                  CALL EXITRC(2)
                ENDIF
              ENDDO
            ENDDO
            IF (code(linatx(sat))/=0 .AND. svnatx(linatx(sat))/='    ')
     1                                                              THEN
              WRITE(lfnprt,"(' ',A20,I6,I4,1X,A4,5X,A20,I4,1x,A4,1X,
     1        6F8.4)")satfil%sensor(sat)%sensor,satfil%sensor(sat)%numb,
     2        satfil%sensor(sat)%svn,svnnr,ANTTYP(linatx(sat)),
     3        code(linatx(sat)),svnatx(linatx(sat)),
     4        (rANTOFF(ICOR,1,sat),ICOR=1,3),
     5        (rANTOFF(ICOR,2,sat),ICOR=1,3)
            ELSEIF (code(linatx(sat)) /= 0 .AND.
     1                             svnatx(linatx(sat)) == '    ') THEN
              WRITE(lfnprt,"(' ',A20,I6,I4,1X,A4,5X,A20,I4,6X,6F8.4)")
     1              satfil%sensor(sat)%sensor,satfil%sensor(sat)%numb,
     2              satfil%sensor(sat)%svn,svnnr,ANTTYP(linatx(sat)),
     3              code(linatx(sat)),(rANTOFF(ICOR,1,sat),ICOR=1,3),
     4                                (rANTOFF(ICOR,2,sat),ICOR=1,3)
            ELSE
             WRITE(lfnprt,"(' ',A20,I6,I4,1X,A4,5X,A20,'  generic',1X,
     1        6F8.4)")satfil%sensor(sat)%sensor,satfil%sensor(sat)%numb,
     2        satfil%sensor(sat)%svn,svnnr,ANTTYP(linatx(sat)),
     3        (rANTOFF(ICOR,1,sat),ICOR=1,3),
     4        (rANTOFF(ICOR,2,sat),ICOR=1,3)
            ENDIF
          ELSEIF (linatx(sat) == 0 .AND.
     1            satfil%sensor(sat)%sensor(18:20) /= '   ' .AND.
     2            satfil%sensor(sat)%sensor(1:3) == typeMWTR) THEN
             WRITE(lfnprt,"(' ',A20,I6,I4,1X,A4,5X,'no entry in ANTEX!'
     1        )")satfil%sensor(sat)%sensor,satfil%sensor(sat)%numb,
     2        satfil%sensor(sat)%svn,svnnr
          ENDIF
        ENDDO
C
C If external file is given...
        hRCV=IRCV
        IF (FILEXT1 .NE. ' ') THEN
C Read PHASE file that has to be updated resp. converted
C ------------------------------------------------------
          oNRCV=0
          oDATE='          '
          CALL rdaphc(MAXRCV,MAXELV,MAXAZI,FILEXT1,oTITLE,oNRCV ,
     1                oRECTYP,oANTTYP,oIANTEN,oNFRANT,oANTOFF,oMODTYP,
     2                oSINEX ,oNPCV  ,oANTPCV,oMAXZEN,oMETH,oDATE,
     3                oFILINFO)

        ENDIF
C Find AOAD/M_T and check the type of the input PHASE file (REL or ABS)
        IF(oNRCV/=0)THEN
          ARCV=0
          filtyp='R'
          DO ocount=1,oNRCV
            IF ((oANTTYP(ocount)=='AOAD/M_T        NONE' .OR.
     1           oANTTYP(ocount)=='AOAD/M_T            ') .AND.
     2           oIANTEN(2,ocount) == undef_i) THEN
              IF (oMODTYP(ocount) /= 0) THEN
                DO IELV=1,oNPCV(1,ocount)
                  DO IAZI=1,oNPCV(2,ocount)
                    DO IFRQ=1,2
                      IF(ABS(oANTPCV(IELV,IAZI,IFRQ,ocount))>1.D-6)THEN
                        filtyp='A'
                        GOTO 778
                      ENDIF
                    ENDDO
                  ENDDO
                ENDDO
              ENDIF
778           IF(pcvtyp=='A' .AND. filtyp=='R' .AND. abs2rel==0 .AND.
     1                                               convert==0)THEN
                WRITE(lfnerr,"(/,' *** PG PHCCNV: Wrong file type',
     1                      ' of input phase file.',
     2                      /,16X,'Phase file name: ',A40,
     3                      /,16X,'Expected type  : ',A1,
     4                      /,16X,'File not converted!')"
     5                      )FILEXT1,pcvtyp
                CALL EXITRC(2)
              ELSEIF(pcvtyp=='R' .AND. filtyp=='A')THEN
                WRITE(lfnerr,"(/,' *** PG PHCCNV: Wrong file type',
     1                      ' of input phase file.',
     2                      /,16X,'Phase file name: ',A40,
     3                      /,16X,'Expected type  : ',A1,
     4                      /,16X,'Phase file not merged/updated!')"
     5                      )FILEXT1,pcvtyp
                CALL EXITRC(2)
              ELSEIF(pcvtyp == 'R' .AND. filtyp == 'R'.AND.
     1               (convert == 1 .OR. abs2rel == 1)) THEN
                WRITE(lfnerr,"(/,' *** PG PHCCNV: Wrong file type',
     1                      ' of ANTEX file.',
     2                      /,16X,'ANTEX file name: ',A40,
     3                      /,16X,'Expected type  : A',
     4                      /,16X,'Program stopped!')"
     5                      )FILEXT
                CALL EXITRC(2)
C                convert = 0
              ELSEIF(pcvtyp=='A' .AND. filtyp=='A'.AND.abs2rel==1)THEN
                WRITE(lfnerr,"(/,' *** PG PHCCNV: Wrong file type',
     1                      ' of input phase file.',
     2                      /,16X,'Phase file name: ',A40,
     3                      /,16X,'Expected type  : R',
     4                      /,16X,'Phase file not merged/updated!')"
     5                      )FILEXT1,pcvtyp
                CALL EXITRC(2)
              ELSEIF(pcvtyp=='A' .AND. filtyp=='A'.AND.convert==1)THEN
                WRITE(lfnerr,"(/,' ### PG PHCCNV: Nothing to convert!',
     1                      /,16X,'Phase file name: ',A40,
     2                      /,16X,'Expected type  : R',
     3                      /,16X,'Phase file just merged/updated!')"
     4                      )FILEXT1
                convert = 0
              ELSEIF(pcvtyp=='A'.AND.filtyp=='R' .AND. convert==1.AND.
     1                  nGPSSAT==0)THEN
                WRITE(lfnerr,"(/,' *** PG PHCCNV: No GPS satellite PCV',
     1                      ' included in ANTEX file.',
     2                      /,16X,'ANTEX file name: ',A40,
     3                      /,16X,'Phase file not converted!')"
     4                      )FILEXT
                CALL EXITRC(2)
              ENDIF
              ARCV=ocount
            ELSEIF(ocount==oNRCV .AND. convert == 1 .AND. ARCV==0)THEN
              WRITE(lfnerr,"(/,' *** PG PHCCNV: Antenna [AOAD/M_T    ',
     1                    '   NONE] not found in input phase file.',
     2                    /,16X,'Phase file name: ',A40,
     3                    /,16X,'Phase file not convertable!')"
     4                    )FILEXT1
              CALL EXITRC(2)
            ELSEIF((oANTTYP(ocount)(1:12)=='MW TRANSM. B' .AND.
     1                                      nGPSSAT == 0) .OR.
     2             (oANTTYP(ocount)(1:12)=='MW TRANSM. G' .AND.
     3                                      nGLOSAT == 0))THEN
              WRITE(lfnerr,"(/,' *** PG PHCCNV: No(t all) satellite ',
     1                          'antenna names in ANTEX file and ',
     2                    /,16X,'old satellite antenna names in input ',
     3                          'phase file.',
     4                    /,16X,'Phase file name: ',A40,
     5                    /,16X,'Please use new phase file and corresp',
     6                          'onding SATELLIT.xxx file',/,16X,'OR',
     7                    /,16X,'update your phase file first with a ',
     8                          'new SATELLIT.xxx file from CODE',
     9                   /,16X,'and a ANTEX file including satellites!',
     1                    /,16X,'Phase file not converted!')")FILEXT1
              CALL EXITRC(2)
            ENDIF
          ENDDO
C
C Convert abs to rel if requested and deletion of AOAD/M_T NONE from ANTEX array
C ------------------------------------------------------------------------------
        IF (abs2rel == 1 ) THEN
          aoa = 0
          AOADMTOFF(:,:) = ANTOFF(:,:,AOADMT)
          AOADMTPCV(:,:,:) = ANTPCV(:,:,:,AOADMT)
          DO atx=1,hRCV
            IF (atx == AOADMT) THEN
              aoa = 1
              CYCLE
            ENDIF
            atx1=atx-aoa
            IF (ISSAT(atx) == 0) THEN
              ANTTYP(atx1)  = ANTTYP(atx)
              IANTEN(1,atx1)= IANTEN(1,atx)
              IANTEN(2,atx1)= IANTEN(2,atx)
              RECTYP(atx1)  = RECTYP(atx)
              NFRANT(atx1)  = NFRANT(atx)
              MODTYP(atx1)  = MODTYP(atx)
              NPCV(1,atx1)  = NPCV(1,atx)
              NPCV(2,atx1)  = NPCV(2,atx)
              MAXZEN(atx1)  = MAXZEN(atx)
              METH(atx1)    = METH(atx)
              atxDATE(atx1) = atxDATE(atx)
              DO ifrq=1,NFRANT(atx)
                DO xyz=1,3
                  ANTOFF(xyz,ifrq,atx1)=
     1                        ANTOFF(xyz,ifrq,atx)-
     2                        AOADMTOFF(xyz,ifrq)+
     3                       oANTOFF(xyz,ifrq,ARCV)
                ENDDO
                DO ele=1,NPCV(1,atx)
                  DO iazi=1,NPCV(2,atx)
                    IF (NPCV(2,atx)==1) THEN
                      ANTPCV(ele,iazi,ifrq,atx1)=
     1                            ANTPCV(ele,iazi,ifrq,atx)-
     2                            AOAPCV(ele,ifrq)
                    ELSE
                      ANTPCV(ele,iazi,ifrq,atx1)=
     1                            ANTPCV(ele,iazi,ifrq,atx)-
     2                            AOADMTPCV(ele,iazi,ifrq)
                    ENDIF
                  ENDDO
                ENDDO
              ENDDO
            ENDIF
          ENDDO
          hRCV = hRCV - aoa
          IRCV = IRCV - aoa
        ENDIF
C
C Find corresponding antenna in SATELLIT. file and remember index
C for satellite antennas in input phase file
          first = .TRUE.
          DO ocount=1,oNRCV
            linesat(ocount) = 0
            sensor=' '
            svnnr=' '
            prn=0
            READ(oANTTYP(ocount)(18:20),'(i3)',iostat=ios)svnr
            IF (ios .EQ. 0 .AND. svnr .NE. 0) THEN
              IF (svnr > 700 .AND. svnr < 800) svnr = svnr - 600
              CALL svn2chr(svnr,help,syst)
              IF     (oANTTYP(ocount)(1:11)=='MW TRANSM I')THEN
                   WRITE(svnnr,'(A1,A3)')syst,oANTTYP(ocount)(18:20)
              ELSEIF (oANTTYP(ocount)(1:11)=='MW TRANSM S')THEN
                   WRITE(svnnr,'(A1,A3)')syst,oANTTYP(ocount)(18:20)
              ELSEIF (oANTTYP(ocount)(1:11)=='MW TRANSM O')THEN
                   WRITE(svnnr,"('E',A3)")oANTTYP(ocount)(18:20)
              ENDIF
              sensor=oANTTYP(ocount)
              DO sat=1,satfil%nsensor
                IF (sensor==satfil%sensor(sat)%sensor)THEN
                  linesat(ocount) = sat
                  EXIT
                ENDIF
              ENDDO
              IF(linesat(ocount) == 0 .AND. svnnr /= ' ') THEN
                CALL svn2prn(4,svnnr,0d0,prn,timcheck,irc)
                IF (irc == 6) prn=0
                DO sat=1,satfil%nsensor
                  IF (satfil%sensor(sat)%svn  == prn .AND.
     1                satfil%sensor(sat)%type == typeMWTR .AND.
     2                timcheck%t(1)==satfil%sensor(sat)%timint%t(1))THEN
                    linesat(ocount) = sat
                  ENDIF
                ENDDO
                IF(linesat(ocount)/=0)THEN
                  IF (first) THEN
                    first = .FALSE.
                    WRITE (lfnprt,"(//,' Translated satellite antenn',
     1                         'a names from input phase file:',
     2                        /,' ------------------------------------',
     3                          '---------------------',
     4                       //,' Old Antenna name     translated to:',
     5                        /,' --------------------')")
                  ENDIF
                  WRITE(LFNPRT,"(1X,A20,' --> ',A20)")oANTTYP(ocount),
     1                        satfil%sensor(linesat(ocount))%sensor
                ENDIF
              ENDIF
              IF(linesat(ocount)==0)THEN
                WRITE(lfnerr,"(/,' ### PG PHCCNV: Antenna from inp',
     1                     'ut phase file not found in satellite file.',
     2                     /,16X,'Antenna name: ',A20,
     3                     /,16X,'Antenna can not be translated and ',
     4                       'will be not included',/,16X,'in new ',
     4                       'phase file!')")oANTTYP(ocount)
                linesat(ocount)=-1
              ENDIF

            ENDIF
          ENDDO
C
C Loop over all entries in input PHASE file and comparison with ANTEX entries
          first = .TRUE.
          antcnt= 0
          antcnt2= 0
          DO ocount=1,oNRCV
            IF (first) THEN
              first=.FALSE.
              WRITE(lfnprt,"(//,' Values for receiver antennas and ant',
     1                        'ennas that are not in ANTEX taken from:',
     2                       /,' -------------------------------------',
     3                         '--------------------------------------',
     4                       //,' Antenna name         number         ',
     5                          '                                     ',
     6                          '        ',
     7                          ' Differences: ele/azi offsets pattern',
     8                        /,' -------------------- ------')")
            ENDIF
C for receiver antennas only
            IF ( linesat(ocount) .EQ. 0 .AND.
     1           oANTTYP(ocount)(1:3) .NE. typeMWTR(1:3) .AND.
     2           oANTTYP(ocount)(1:4) .NE. typeSLR)      THEN
              IF (oIANTEN(1,ocount) == 0 .AND. ANTNUM == 1)
     1                                       oIANTEN(1,ocount) = undef_i
              IF (oIANTEN(1,ocount) == undef_i .AND. ANTNUM == 0)
     1                                        oIANTEN(1,ocount) = 0
              IF(oANTTYP(ocount)(17:20) .EQ. '    ')THEN
                antcnt = antcnt + 1
                IF (RADCOD == 1) oANTTYP(ocount)(17:20)='NONE'
              ENDIF
              READ(oDATE(ocount),'(F2.0,5X,I2)',iostat=ios)DD,YYYY
              YYYY = IYEAR4(YYYY)
              iMonth = LISTC1(0,3,12,Month,oDATE(ocount)(4:6),12)
              IF (iMonth /= 0) THEN
                omjd = djul(YYYY,iMonth,DD)
              ELSE
                omjd = 0
                IF(oDATE(ocount)(4:6)/='   ')THEN
                  WRITE(lfnerr,"(/,' ### PG PHCCNV: Wrong month ',
     1                        'within date in input phase file.',
     2                        /,16X,'Antenna: ',A20,
     3                        /,16X,'Date   : ',A10,
     4                        /,16X,'Date replaced by zero!')"
     5                        )oANTTYP(ocount),oDATE(ocount)
                ENDIF
              ENDIF
            ENDIF
            oiMETH = LISTC1(0,5,6,Method,oMETH(ocount)(1:5),6)
C If similar entries found in both files check values, method and date
            DO atx=1,hRCV
              add=0
              iMETH = LISTC1(0,5,6,Method,METH(atx)(1:5),6)
              IF (oIANTEN(1,ocount)==IANTEN(1,atx) .AND.
     1                               oANTTYP(ocount) == ANTTYP(atx))THEN
C Check if same values in ANTEX and input PHASE file
C --------------------------------------------------
                IF (convert == 0) THEN
                  difnpcv=' no '
                  diffoff=' no '
                  diffpcv=' no '
C different offset values?
                  DO ICOR=1,3
                    DO IFRQ=1,2
                      help1 = oANTOFF(ICOR,IFRQ,ocount)-
     1                          ANINT(ANTOFF(ICOR,IFRQ,atx)*10000)/10000
                      IF (ABS(help1)>1.D-6) diffoff='yes '
                    ENDDO
                  ENDDO
C different number of elevation/azimut values?
                  IF(oNPCV(1,ocount)==0) THEN
                    DO ele=1,oNPCV(1,atx)
                      DO iazi=1,oNPCV(2,atx)
                        DO ifrq=1,oNFRANT(atx)
                          IF (ABS(ANTPCV(ele,iazi,ifrq,atx))>1.D-7)THEN
                            difnpcv='n.a.'
                            diffpcv='yes '
                          ENDIF
                        ENDDO
                      ENDDO
                    ENDDO
                  ELSE
                    IF (.NOT. oNPCV(1,ocount)>=NPCV(1,atx) .OR.
     1                      oNPCV(2,ocount)/=NPCV(2,atx)) difnpcv='yes '
C different PCV values?
                    DO ele=1,NPCV(1,atx)
                      DO iazi=1,NPCV(2,atx)
                        DO ifrq=1,NFRANT(atx)
                          help1 = oANTPCV(ele,iazi,ifrq,ocount)-
     1                                ANTPCV(ele,iazi,ifrq,atx)
                          IF (ABS(help1)>1.D-7) diffpcv='yes '
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDIF
                  IF (diffpcv=='yes ' .OR. difnpcv=='yes '.OR.
     1                diffoff=='yes ') antcnt2=antcnt2+1
                ELSE
                  difnpcv='n.a.'
                  diffoff='n.a.'
                  diffpcv='n.a.'
                ENDIF
C Check date in ANTEX
C -------------------
                READ(atxDATE(atx),'(F2.0,5X,I2)')DD,YYYY
                YYYY = IYEAR4(YYYY)
                iMonth = LISTC1(0,3,12,Month,atxDATE(atx)(4:6),12)
                IF (iMonth /= 0) THEN
                  amjd = djul(YYYY,iMonth,DD)
                ELSE
                  WRITE(lfnerr,"(/,' *** PG PHCCNV: Wrong month ',
     1                        'within date in ANTEX file.',
     2                        /,16X,'Antenna: ',A20,
     3                        /,16X,'Date   : ',A10,
     4                        /,16X,'File not converted!')"
     5                        )ANTTYP(atx),atxDATE(atx)
                  CALL EXITRC(2)
                ENDIF
C Take values from input Bernese PCV file
                IF(((omjd > amjd .AND. oiMETH > iMETH).OR.
     1             oiMETH > iMETH).AND. convert /= 1) THEN
                  ANTTYP(atx)  = oANTTYP(ocount)
                  IANTEN(1,atx)= oIANTEN(1,ocount)
                  IANTEN(2,atx)= oIANTEN(2,ocount)
                  RECTYP(atx)  = oRECTYP(ocount)
                  NFRANT(atx)  = oNFRANT(ocount)
                  DO ICOR=1,3
                    DO IFRQ=1,2
                      ANTOFF(ICOR,IFRQ,atx)=
     1                            oANTOFF(ICOR,IFRQ,ocount)
                    ENDDO
                  ENDDO
                  MODTYP(atx)  = oMODTYP(ocount)
                  SINEX(atx)   = oSINEX(ocount)
                  NPCV(1,atx)  = oNPCV(1,ocount)
                  NPCV(2,atx)  = oNPCV(2,ocount)
                  DO IELV=1,MAXELV
                    DO IAZI=1,MAXAZI
                      DO IFRQ=1,2
                        ANTPCV(IELV,IAZI,IFRQ,atx)=
     1                              oANTPCV(IELV,IAZI,IFRQ,ocount)
                      ENDDO
                    ENDDO
                  ENDDO
                  MAXZEN(atx)  = oMAXZEN(ocount)
                  METH(atx)    = oMETH(ocount)
                  BY(atx)      = oBY(ocount)
                  atxDATE(atx) = oDATE(ocount)
                  WRITE(lfnprt,"(1X,A20,I7,' from input PHASE file ',
     1                 '(newer date and/or newer calibration method)',
     2                     3x,A4,2(4x,A4))") ANTTYP(atx),IANTEN(1,atx),
     3                    difnpcv,diffoff,diffpcv
C Take values from ANTEX file
                ELSE
                  IF(omjd==amjd) THEN
                    WRITE(lfnprt,"(1X,A20,I7,' from ANTEX file ',
     1                 '(same date and same or newer calibration',
     2                 ' method) ',3(4x,A4))")ANTTYP(atx),IANTEN(1,atx),
     3                    difnpcv,diffoff,diffpcv
                  ELSEIF(omjd<amjd) THEN
                    WRITE(lfnprt,"(1X,A20,I7,' from ANTEX file ',
     1                 '(newer date and same or newer calibration',
     2                 ' method)',3(4x,A4))")ANTTYP(atx),IANTEN(1,atx),
     3                    difnpcv,diffoff,diffpcv
                  ELSE
                    WRITE(lfnprt,"(1X,A20,I7,' from ANTEX file ',
     1                 '(older date but same or newer calibration',
     2                 ' method)',3(4x,A4))")ANTTYP(atx),IANTEN(1,atx),
     3                    difnpcv,diffoff,diffpcv
                  ENDIF
                ENDIF
                GOTO 777
C Add into list if no entry in ANTEX file
C ---------------------------------------
              ELSEIF (atx == hRCV) THEN
C for receiver antennas
C ---------------------
                IF(linesat(ocount) == 0                 .AND.
     1             oANTTYP(ocount)(1:11)/='MW TRANSM. ' .AND.
     2              ((oANTTYP(ocount)(17:20)/='    ' )  .OR.
     3                oANTTYP(ocount)(1:3) == typeMWTR  .OR.
     4                oANTTYP(ocount)(1:4) == typeSLR)) THEN
C Check if NONE antenna exists in input PHAS file with same values (copied case)
                  nfound=0
                  COPLOOK: DO ocoun2=1,oNRCV
                    IF(oANTTYP(ocount)(17:20)=='NONE') EXIT COPLOOK
                    IF(oANTTYP(ocount)(1:16)==oANTTYP(ocoun2)(1:16).AND.
     1                 oANTTYP(ocoun2)(17:20)=='NONE' .AND.
     2                 oIANTEN(2,ocoun2) == undef_i)THEN
                      nfound=1
C different number of elevation/azimut values?
                      IF(oNPCV(1,ocount)/=oNPCV(1,ocoun2) .OR.
     1                   oNPCV(2,ocount)/=oNPCV(2,ocoun2))THEN
                        add=2
                        EXIT COPLOOK
                      ENDIF
C different offset values?
                      DO ICOR=1,3
                        DO IFRQ=1,2
                          help1 = 1.D-15 + oANTOFF(ICOR,IFRQ,ocount)-
     1                                     oANTOFF(ICOR,IFRQ,ocoun2)
                          IF (ABS(help1)>1.D-10)THEN
                            add=2
                            EXIT COPLOOK
                          ENDIF
                        ENDDO
                      ENDDO
C different PCV values?
                      DO ele=1,oNPCV(1,ocount)
                        DO iazi=1,oNPCV(2,ocount)
                          DO ifrq=1,oNFRANT(ocount)
                            help1 = 1.D-15 +
     1                                  oANTPCV(ele,iazi,ifrq,ocount)-
     2                                  oANTPCV(ele,iazi,ifrq,ocoun2)
                          IF (ABS(help1)>1.D-10)THEN
                            add=2
                            EXIT COPLOOK
                          ENDIF
                          ENDDO
                        ENDDO
                      ENDDO
                    ENDIF
                  ENDDO COPLOOK
                IF (nfound == 0) add = 2
                ENDIF
C for satellite antennas
C ----------------------
                IF (linesat(ocount) > 0) THEN
                  IF((satfil%sensor(linesat(ocount))%name(1:5)==antGPS
     1                                        .AND. nGPSSAT == 0) .OR.
     2               (satfil%sensor(linesat(ocount))%name(1:7)==antGLO
     3                                        .AND. nGLOSAT == 0) .OR.
     4               (satfil%sensor(linesat(ocount))%name(1:7)==antGAL
     5                                        .AND. nGALSAT == 0))THEN
                    add=1
C IF NOZERO input for satellites, initialisation for NPCV and MAXZEN
                    IF (oNPCV(1,ocount)==MAXELV) THEN
                      oNPCV(1,ocount)=MAXEL2
                      oMAXZEN(ocount)=15
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
              IF(add>0)THEN
                IRCV=IRCV+1
                ISSAT(IRCV)=0
C
                IF(add==1)THEN
                  ISSAT(IRCV)=1
                  IF(convert==1)THEN
                    WRITE(lfnerr,"(/,' ### PG PHCCNV: Relative pattern',
     1                    ' for satellite antenna in input phase file.',
     2                        /,16X,'Antenna: ',A20,
     4                        /,16X,'Absolute pattern filled with ',
     5                              'zero!')")oANTTYP(ocount)
                  ENDIF
                ENDIF
C Conversion if requested
                IF(convert==1 .AND. add == 2 .AND.
     1             oANTTYP(ocount)(1:3) /= typeMWTR(1:3) .AND.
     2             oANTTYP(ocount)(1:4) /= typeSLR .AND.
     3             oANTTYP(ocount)(1:6) /= 'SIMULA') THEN
                  METH(IRCV)='CONVERTED           '
                  atxDATE(IRCV)=DATE
                  DO ifrq=1,oNFRANT(ocount)
                    DO xyz=1,3
                      oANTOFF(xyz,ifrq,ocount)=
     1                            oANTOFF(xyz,ifrq,ocount)-
     2                            oANTOFF(xyz,ifrq,ARCV)+
     3                            ANTOFF(xyz,ifrq,AOADMT)
                    ENDDO
                    DO ele=1,oNPCV(1,ocount)
                      DO iazi=1,oNPCV(2,ocount)
                        IF (oNPCV(2,ocount)==1) THEN
                          oANTPCV(ele,iazi,ifrq,ocount)=
     1                                oANTPCV(ele,iazi,ifrq,ocount)
     2                                +AOAPCV(ele,ifrq)
                        ELSE
                          oANTPCV(ele,iazi,ifrq,ocount)=
     1                                oANTPCV(ele,iazi,ifrq,ocount)
     2                                +ANTPCV(ele,iazi,ifrq,AOADMT)
                        ENDIF
                      ENDDO
                    ENDDO
                  ENDDO
                  WRITE(lfnprt,"(1X,A20,I7,' --> Converted to ',
     1                    'absolute')")oANTTYP(ocount),oIANTEN(1,ocount)
                ELSE
                  WRITE(lfnprt,"(1X,A20,I7,' from input phase file ',
     1                        '(no entry in ANTEX)')")
     2                        oANTTYP(ocount),oIANTEN(1,ocount)
                ENDIF
                ANTTYP(IRCV)  = oANTTYP(ocount)
                IANTEN(1,IRCV)= oIANTEN(1,ocount)
                IANTEN(2,IRCV)= oIANTEN(2,ocount)
                RECTYP(IRCV)  = oRECTYP(ocount)
                NFRANT(IRCV)  = oNFRANT(ocount)
                DO ICOR=1,3
                  DO IFRQ=1,2
                    ANTOFF(ICOR,IFRQ,IRCV)=oANTOFF(ICOR,IFRQ,ocount)
                  ENDDO
                ENDDO
                MODTYP(IRCV)  = oMODTYP(ocount)
                SINEX(IRCV)   = oSINEX(ocount)
                NPCV(1,IRCV)  = oNPCV(1,ocount)
                NPCV(2,IRCV)  = oNPCV(2,ocount)
                DO IELV=1,MAXELV
                  DO IAZI=1,MAXAZI
                    DO IFRQ=1,2
                      ANTPCV(IELV,IAZI,IFRQ,IRCV)=
     1                            oANTPCV(IELV,IAZI,IFRQ,ocount)
                    ENDDO
                  ENDDO
                ENDDO
                MAXZEN(IRCV)  = oMAXZEN(ocount)
                METH(IRCV)    = oMETH(ocount)
                BY(IRCV)      = oBY(ocount)
                atxDATE(IRCV) = oDATE(ocount)
              ENDIF
            ENDDO
777       ENDDO
          IF (antcnt /= 0 .AND. RADCOD==1) THEN
            WRITE(LFNERR,"(/,' ### PG PHCCNV: ',i3,' receiver antenna(',
     1                             's) without radome code in input ',
     2                    /,16X,'Bernese PCV file.',
     3                    /,16X,'Antenna radome set to NONE!',
     4                    /,16X,'Check output and result file!')")antcnt
          ELSEIF (antcnt /= 0 .AND. RADCOD /=1) THEN
            WRITE(LFNERR,"(/,' ### PG PHCCNV: ',i3,' receiver antenna(',
     1                             's) without radome code in input ',
     2                     /,16X,'Bernese PCV file.',
     3                     /,16X,'Antenna(s) not included in new ',
     4                               'Phase file!')")antcnt
          ENDIF
          IF (antcnt2 /= 0) THEN
            WRITE(LFNERR,"(/,' ### PG PHCCNV: ',i3,' receiver antenna(',
     1                             's) with different values in ANTEX ',
     2                     /,16X,'and input Bernese PCV file.',
     3                     /,16X,'Check output and result file!'
     4                               )")antcnt2
          ENDIF
        ENDIF
C
C Write output for entries in ANTEX file with no entry in input PHASE file
C ----------------------------------------------------------------------
        DO atx=1,hRCV
          IF (ISSAT(atx) == 0) THEN
            DO ocount=1,oNRCV
              IF (oIANTEN(1,ocount)==IANTEN(1,atx) .AND.
     1                              oANTTYP(ocount) == ANTTYP(atx)) EXIT
              IF (ocount == oNRCV) THEN
                WRITE(lfnprt,"(1X,A20,I7,' from ANTEX file ',
     1                        '(no entry in input phase file)')")
     2                        ANTTYP(atx),IANTEN(1,atx)
              ENDIF
            ENDDO
          ENDIF
        ENDDO
C
C Check for unknown satellite antenna entry in ATX file and write output
C ----------------------------------------------------------------------
        first=.TRUE.
        DO atx=1,hRCV
          IF (ISSAT(atx) == 1) THEN
            DO sat=1,satfil%nsensor
              IF (linatx(sat) == atx) EXIT
              IF (sat == satfil%nsensor) THEN
                IF (first) THEN
                  first=.FALSE.
                  WRITE (lfnprt,"(/,/,' Sensor patterns in ANTEX file ',
     1                    'with no corresponding entry in satellite',
     2                    ' file:',/,' ------------------------------',
     3            '-------------------------------------------',/,
     4            ' entry ANTEX sensor name    PRN  SVN',/,' ----- ',
     5            '-------------------- --- ----',/)")
                ENDIF
                WRITE(lfnerr,"(/,' ### PG PHCCNV: Sensor pattern in',
     1                         ' ANTEX file with no corresponding',
     2                       /,16X,'entry in satellite file.',
     3                       /,16X,'Entry in ANTEX file: ',I4,
     4                       /,16X,'ANTEX sensor name: ',A)")
     5                                 atx,ANTTYP(atx)
                IF(code(atx)/=0.AND.svnatx(atx)/='    ')THEN
                  WRITE(string,'(I6,1X,A20,I4,1X,A4)')atx,ANTTYP(atx),
     1                                   code(atx),svnatx(atx)
                ELSEIF (code(atx) /= 0 .AND.
     1                             svnatx(atx) == '    ') THEN
                  WRITE(string,'(I6,1X,A20,I4,5X)')atx,ANTTYP(atx),
     1                                   code(atx)
                ELSE
                  WRITE(string,"(I6,1X,A20,'  generic')")atx,ANTTYP(atx)
                ENDIF
                WRITE(lfnprt,'(A36)')string
              ENDIF
            ENDDO
          ENDIF
        ENDDO
        first=.FALSE.
C
C Enlarge PHASE file with antennas from STACRX not included in ANTEX
C ------------------------------------------------------------------
        count = 0
        IF (FILEXT2 .NE. ' ') THEN
          CALL readcrux(FILEXT2,stacrux)
          kRCV=IRCV
          idome = 0
          DO info=1,stacrux%ninfo
            nfound=0
            IF (stacrux%stainfo(info)%antnam(17:20) == '    ') THEN
              idome = idome + 1
              CYCLE
            ENDIF
            DO atx=1,kRCV
              IF (ISSAT(atx)==0 .AND. stacrux%stainfo(info)%antnam==
     1                 ANTTYP(atx) .AND. IANTEN(2,atx) == undef_i) EXIT
              IF (ISSAT(atx)==0 .AND.
     1            stacrux%stainfo(info)%antnam(1:16)==ANTTYP(atx)(1:16)
     2            .AND. ANTTYP(atx)(17:20) == 'NONE' .AND.
     3                              IANTEN(2,atx) == undef_i) nfound = 1
              IF (atx == kRCV .AND. nfound == 1)THEN
                count=count+1
                DO iii=1,count
                  IF (NONEANT(iii) == stacrux%stainfo(info)%antnam) THEN
                    count=count-1
                    EXIT
                  ELSEIF (iii==count) THEN
                    NONEANT(count)=stacrux%stainfo(info)%antnam
                  ENDIF
                ENDDO
              ELSEIF (atx == kRCV .AND. nfound == 0 .AND.
     1               stacrux%stainfo(info)%antnam /= undef_c) THEN
                WRITE(LFNERR,211)stacrux%stainfo(info)%antnam
211             FORMAT(/,' ### PG PHCCNV: No corresponding NONE ',
     1                  'antenna found in ANTEX file',
     2                 /,16X,'for antenna in station information file.',
     3                 /,16X,'Antenna name: ',A)
              ENDIF
            ENDDO
          ENDDO
          IF (idome /= 0) THEN
            WRITE(LFNERR,"(/,' ### PG PHCCNV: ',i3,' receiver antenna(',
     1                             's) without radome code in station ',
     2                     /,16X,'information file.',
     3                     /,16X,'Antenna(s) not included in new ',
     4                               'phase file!')")idome
          ENDIF
        ENDIF
C
C FILL PATTERNS, IF DESIRED
C -------------------------
C
C READ INFORMATION FROM PHCCNV.INP
        ATXFIL = 0
        CALL READKEYS('ATXFIL', keyValue, irc)
        IF (irc == 0 .AND. keyValue(1) == '1') ATXFIL = 1
C
        IF (ATXFIL == 1) THEN
          CALL READKEYS('MXFZEN', keyValue, irc)
          IF (irc == 0) READ(keyValue(1), *, iostat=ios) MXFZEN
          IF (irc /= 0 .OR. ios /= 0) THEN
            WRITE(LFNERR,907) TRIM(keyValue(1))
907         FORMAT(/,' *** PG PHCCNV: WRONG MAXIMUM ZENITH ANGLE',
     1               ' SPECIFIED FOR',
     2             /,16X,'FILLING OF ANTENNA PATTERNS.',
     3             /,16X,'SPECIFIED VALUE: ',A)
            IRCODE=2
            GOTO 999
          ENDIF
C
          FIZMOD = 0
          CALL READKEYS('RADIO1_1', keyValue, irc)
          IF (irc == 0 .AND. keyValue(1) == '1') THEN
            FIZMOD = 1
          ELSE
            CALL READKEYS('RADIO1_2', keyValue, irc)
            IF (irc == 0 .AND. keyValue(1) == '1') THEN
              FIZMOD = 2
            ELSE
              FIZMOD = 3
            ENDIF
          ENDIF
C
          IF (FIZMOD == 3 .AND. AOADMT == 0) THEN
            WRITE(LFNERR,899)
899         FORMAT(/,' ### PG PHCCNV: ANTENNA AOAD/M_T NOT AVAILABLE.',
     1             /,16X,'PATTERNS FILLED WITH ZEROS.')
            FIZMOD = 1
          ENDIF
C
          CALL READKEYS('MXFNAD', keyValue, irc)
          IF (irc == 0) READ(keyValue(1), *, iostat=ios) MXFNAD
          IF (irc /= 0 .OR. ios /= 0) THEN
            WRITE(LFNERR,908) TRIM(keyValue(1))
908         FORMAT(/,' *** PG PHCCNV: WRONG MAXIMUM NADIR ANGLE',
     1               ' SPECIFIED FOR',
     2             /,16X,'FILLING OF ANTENNA PATTERNS.',
     3             /,16X,'SPECIFIED VALUE: ',A)
            IRCODE=2
            GOTO 999
          ENDIF
C
          FINMOD = 0
          CALL READKEYS('RADIO2_1', keyValue, irc)
          IF (irc == 0 .AND. keyValue(1) == '1') THEN
            FINMOD = 1
          ELSE
            FINMOD = 2
          ENDIF
C
C LOOP OVER ALL PRESENT ANTENNAS
          DO JRCV=1,IRCV
            DZEN=0.D0
            NFPCV=0
C
C SATELLITE ANTENNNAS
C
            IF (ISSAT(JRCV).EQ.1) THEN
              IF (MAXZEN(JRCV).GE.MXFNAD) THEN
                CYCLE
              ELSE
                DZEN=DBLE(MAXZEN(JRCV))/(NPCV(1,JRCV)-1)
                NFPCV=IDNINT(DBLE(MXFNAD-MAXZEN(JRCV))/DZEN)
                IF (NFPCV.LT.1) CYCLE
C
                IF ((NPCV(1,JRCV)+NFPCV).GT.MAXELV) THEN
                  WRITE(LFNERR,909) FILEXT,ANTTYP(JRCV),
     1                              (NPCV(1,JRCV)+NFPCV),MAXELV
909               FORMAT(/,' *** PG PHCCNV: TOO MANY DIFFERENT',
     1                     ' ZENITH/NADIR ANGLES DUE TO FILLING.',
     2                   /,16X,'FILE NOT CONVERTED!',
     3                   /,16X,'FILE NAME         : ',A32,
     4                   /,16X,'ANTENNA TYPE      : ',A20,
     5                   /,16X,'# OF ZENITH ANGLES:',I3,
     6                   /,16X,'MAX # ALLOWED     :',I3,
     7                   /,16X,'INCREASE MAXELV!',/)
                  IRCODE=2
                  GOTO 999
                ENDIF
C
                DO IFRQ=1,NFRANT(JRCV)
                  DO IAZI=1,NPCV(2,JRCV)
                    DO IELV=NPCV(1,JRCV)+1,NPCV(1,JRCV)+NFPCV
                      IF (FINMOD.EQ.1) THEN
                        ANTPCV(IELV,IAZI,IFRQ,JRCV)=0.D0
                      ELSE
                        ANTPCV(IELV,IAZI,IFRQ,JRCV)=
     1                        ANTPCV(NPCV(1,JRCV),IAZI,IFRQ,JRCV)
                      ENDIF
                    ENDDO
                  ENDDO
                ENDDO
                MAXZEN(JRCV)=MXFNAD
                NPCV(1,JRCV)=NPCV(1,JRCV)+NFPCV
              ENDIF
C
C RECEIVER ANTENNAS
            ELSEIF (ISSAT(JRCV).EQ.0) THEN
              IF (MAXZEN(JRCV).GE.MXFZEN) THEN
                CYCLE
              ELSE
                DZEN=DBLE(MAXZEN(JRCV))/(NPCV(1,JRCV)-1)
                NFPCV=IDNINT(DBLE(MXFZEN-MAXZEN(JRCV))/DZEN)
                IF (NFPCV.LT.1) CYCLE
C
                IF ((NPCV(1,JRCV)+NFPCV).GT.MAXELV) THEN
                  WRITE(LFNERR,909) FILEXT,ANTTYP(JRCV),
     1                              (NPCV(1,JRCV)+NFPCV),MAXELV
                  IRCODE=2
                  GOTO 999
                ENDIF
C
                DO IFRQ=1,NFRANT(JRCV)
                  DO IAZI=1,NPCV(2,JRCV)
                    DO IELV=NPCV(1,JRCV)+1,NPCV(1,JRCV)+NFPCV
                      IF (FIZMOD.EQ.1) THEN
                        ANTPCV(IELV,IAZI,IFRQ,JRCV)=0.D0
                      ELSEIF (FIZMOD.EQ.2) THEN
                        ANTPCV(IELV,IAZI,IFRQ,JRCV)=
     1                        ANTPCV(NPCV(1,JRCV),IAZI,IFRQ,JRCV)
                      ELSEIF (FIZMOD.EQ.3) THEN
                        IF (NPCV(2,JRCV).EQ.1) THEN
                          ANTPCV(IELV,IAZI,IFRQ,JRCV)=
     1                                          AOAPCV(IELV,IFRQ)
                        ELSE
                          ANTPCV(IELV,IAZI,IFRQ,JRCV)=
     1                              ANTPCV(IELV,IAZI,IFRQ,AOADMT)
                        ENDIF
                      ENDIF
                    ENDDO
                  ENDDO
                ENDDO
                MAXZEN(JRCV)=MXFZEN
                NPCV(1,JRCV)=NPCV(1,JRCV)+NFPCV
              ENDIF
            ENDIF
          ENDDO
C
        ENDIF
C
C ADD ADDITIONAL INFORMATION NECESSARY
C ------------------------------------
        TITLE=' '
        FILINFO=' '
        TITLE(1:45) ='ANTENNA PHASE CENTER VARIATIONS DERIVED FROM '
        TITLE(46:64)='ANTEX FILE'
        TITLE(66:74)=DATE
        TITLE(76:80)=TIME
        DO backs=0,len_trim(FILEXT)
          IF(FILEXT(len_trim(FILEXT)-backs:
     1                len_trim(FILEXT)-backs) == "/")THEN
            write(FILINFO(81-backs:80),'(A)')
     1           FILEXT(len_trim(FILEXT)-backs+1:len_trim(FILEXT))
            EXIT
          ENDIF
        ENDDO
C
        DO JRCV=1,IRCV
C Write zero pattern (except of SLR)
          IF (ANTTYP(JRCV)(1:4) /= typeSLR) MODTYP(JRCV) = 1
C
C DO NOT WRITE PCV IF ALL ARE ZERO
          IF (NOZERO.EQ.1) THEN
            ISZERO=1
            DO IFRQ=1,NFRANT(JRCV)
              DO IAZI=1,NPCV(2,JRCV)
                DO IELV=1,NPCV(1,JRCV)
                  IF (ANTPCV(IELV,IAZI,IFRQ,JRCV) /= 0.D0) ISZERO=0
                ENDDO
              ENDDO
            ENDDO
            IF (ISZERO==1) MODTYP(JRCV)=0
          ENDIF
        ENDDO
C
C WRITE OUTPUT FOR Receiver ANTENNAS
        WRITE(lfnprt,"(/,/,' Summary of translated receiver ',
     1                     'antenna numbers',
     2              /,' ---------------------------------------------',
     3              /,' Receiver antenna name read number          ',
     4                   '--> translated number',
     5              /,' -------------------- --------------------',
     6                   '          ------',/)")
        DO atx=1,hRCV
          IF (ISSAT(atx) == 0) THEN
            WRITE(lfnprt,'(2(1X,A20),10X,I6)')ANTTYP(atx),outant(atx),
     1                  IANTEN(1,atx)
          ENDIF
        ENDDO
C
      NRCV=ircv
      ENDIF
C
C END OF ANTEX SECTION!!!!!!!!!!!
C -------------------------------
C
C Write kept information to arrays that will be given over to WTAPHC
      first = .TRUE.
      DO atx=1,ircv
        IF (ONLYELE==1 .AND. NPCV(2,atx).NE.1) NPCV(2,atx)=1
C For Receiver antennas
        IF (ISSAT(atx) == 0) THEN
C If NONE look whether antennas existing that are not in ANTEX file
          IF (ANTTYP(atx)(17:20) == 'NONE') THEN
            DO inone=1,count
              IF (ANTTYP(atx)(1:16) == NONEANT(inone)(1:16) .AND.
     1                 IANTEN(2,atx) == undef_i) THEN
                IF (first) THEN
                  first=.FALSE.
                  WRITE (lfnprt,"(//,' Receiver antennas with no',
     1                 ' corresponding entry in ANTEX or input phase ',
     2                 'file',/,' -----------------------------',
     3                 '---------------------------------------------',
     4                 '------------',//,' Rec. antenna name   -->',
     5                 ' filled with NONE antenna values from ANTEX',/,
     6                 ' --------------------')")
                ENDIF
                WRITE(lfnprt,"(' ',A20)")NONEANT(inone)
                NRCV=NRCV+1
                ISSAT(NRCV)=0
                ANTTYP(NRCV)  = NONEANT(inone)
                IANTEN(1,NRCV)= IANTEN(1,atx)
                IANTEN(2,NRCV)= IANTEN(2,atx)
                RECTYP(NRCV)  = RECTYP(atx)
                NFRANT(NRCV)  = NFRANT(atx)
                DO ICOR=1,3
                  DO IFRQ=1,2
                    ANTOFF(ICOR,IFRQ,NRCV)=ANTOFF(ICOR,IFRQ,atx)
                  ENDDO
                ENDDO
                MODTYP(NRCV)  = MODTYP(atx)
                SINEX(NRCV)   = SINEX(atx)
                NPCV(1,NRCV)  = NPCV(1,atx)
                NPCV(2,NRCV)  = NPCV(2,atx)
                DO IELV=1,MAXELV
                  DO IAZI=1,MAXAZI
                    DO IFRQ=1,2
                      ANTPCV(IELV,IAZI,IFRQ,NRCV)=
     1                            ANTPCV(IELV,IAZI,IFRQ,atx)
                    ENDDO
                  ENDDO
                ENDDO
                MAXZEN(NRCV)  = MAXZEN(atx)
                METH(NRCV)    = 'ADOPTED from NONE'
                atxDATE(NRCV) = atxDATE(atx)
              ENDIF
            ENDDO
          ENDIF
C For Satellite antennas
        ELSEIF (ISSAT(atx) == 1) THEN
          DO sat=1,satfil%nsensor
            IF (linatx(sat) == atx) THEN
              NRCV=NRCV+1
              ISSAT(NRCV)   = 1
              ANTTYP(NRCV)  = satfil%sensor(sat)%sensor
              IANTEN(1,NRCV)= satfil%sensor(sat)%numb
              IANTEN(2,NRCV)= satfil%sensor(sat)%numb
              RECTYP(NRCV)  = RECTYP(atx)
              NFRANT(NRCV)  = NFRANT(atx)
              DO ICOR=1,3
                DO IFRQ=1,2
                  ANTOFF(ICOR,IFRQ,NRCV)=rANTOFF(ICOR,IFRQ,sat)
                ENDDO
              ENDDO
              MODTYP(NRCV)  = MODTYP(atx)
              SINEX(NRCV)   = SINEX(atx)
              NPCV(1,NRCV)  = NPCV(1,atx)
              NPCV(2,NRCV)  = NPCV(2,atx)
              DO IELV=1,MAXELV
                DO IAZI=1,MAXAZI
                  DO IFRQ=1,2
                    ANTPCV(IELV,IAZI,IFRQ,NRCV)=
     1                          ANTPCV(IELV,IAZI,IFRQ,atx)
                  ENDDO
                ENDDO
              ENDDO
              MAXZEN(NRCV)  = MAXZEN(atx)
              METH(NRCV)    = METH(atx)
              atxDATE(NRCV) = atxDATE(atx)
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C
C Sort all entries in arrays (satellite antennas, receiver antennas, SLR etc.)
C ----------------------------------------------------------------------------
      DO atx=1,NRCV
        ANTHLP(atx)(1:20) = ANTTYP(atx)
        WRITE(ANTHLP(atx)(21:26),'(I6)') IANTEN(2,atx)
      ENDDO
      CALL cordup(ANTHLP,NRCV,1,26,INDEX)
      fRCV=0
C keep in mind index for satellite antennas
      DO atx=1,NRCV
        IF(ISSAT(INDEX(atx)) == 1 .AND.
     1     ANTTYP(INDEX(atx))(1:5) /= antGPS .AND.
     2     ANTTYP(INDEX(atx))(1:7) /= antGLO .AND.
     3     ANTTYP(INDEX(atx))(1:7) /= antGAL) THEN
          fRCV=fRCV+1
          INDEXf(fRCV)=INDEX(atx)
        ENDIF
      ENDDO
C keep in mind index for receiver antennas
      DO atx=1,NRCV
        IF(ISSAT(INDEX(atx)) == 0 .AND.
     1     ANTTYP(INDEX(atx))(1:3) /= typeMWTR .AND.
     2     ANTTYP(INDEX(atx))(1:4) /= typeSLR) THEN
          fRCV=fRCV+1
          INDEXf(fRCV)=INDEX(atx)
        ENDIF
      ENDDO
C keep in mind index for remaining entries from input phase file
      DO atx=1,NRCV
        IF ((ANTTYP(atx)(1:4)   == typeSLR .OR.
     1      ANTTYP(atx)(18:20) == '   ') .AND. ISSAT(atx) == 0) THEN
          fRCV=fRCV+1
          INDEXf(fRCV)=atx
        ENDIF
      ENDDO
C Write final arrays
      DO finidx=1,fRCV
        fANTTYP(finidx)  = ANTTYP(INDEXf(finidx))
        fIANTEN(1,finidx)= IANTEN(1,INDEXf(finidx))
        fIANTEN(2,finidx)= IANTEN(2,INDEXf(finidx))
        fRECTYP(finidx)  = RECTYP(INDEXf(finidx))
        fNFRANT(finidx)  = NFRANT(INDEXf(finidx))
        DO ICOR=1,3
          DO IFRQ=1,2
            fANTOFF(ICOR,IFRQ,finidx)=ANTOFF(ICOR,IFRQ,INDEXf(finidx))
          ENDDO
        ENDDO
        fMODTYP(finidx)  = MODTYP(INDEXf(finidx))
        fSINEX(finidx)   = SINEX(INDEXf(finidx))
        fNPCV(1,finidx)  = NPCV(1,INDEXf(finidx))
        fNPCV(2,finidx)  = NPCV(2,INDEXf(finidx))
        DO IELV=1,MAXELV
          DO IAZI=1,MAXAZI
            DO IFRQ=1,2
              fANTPCV(IELV,IAZI,IFRQ,finidx)=
     1                    ANTPCV(IELV,IAZI,IFRQ,INDEXf(finidx))
            ENDDO
          ENDDO
        ENDDO
        fMAXZEN(finidx)  = MAXZEN(INDEXf(finidx))
        fMETH(finidx)    = METH(INDEXf(finidx))
        fDATE(finidx)    = atxDATE(INDEXf(finidx))
      ENDDO
C
C GET ANTENNA PHASE CENTER FILE NAME FOR OUTPUT
C ---------------------------------------------
      CALL GTFLNA(0,'PHASRSG',FILPHC,IRC)
C
C WRITE PHASE CENTER VARIATIONS IN BERNESE FORMAT
C -----------------------------------------------
      CALL WTAPHC(MAXRCV ,MAXELV ,MAXAZI ,FILPHC ,TITLE  ,fRCV   ,
     1            fRECTYP,fANTTYP,fIANTEN,fNFRANT,fANTOFF,fMODTYP,
     2            fSINEX ,fNPCV  ,fANTPCV,fMAXZEN,fMETH  ,fDATE  ,
     3            FILINFO)
C
      IRCODE=0
      GOTO 999
C
C ERRORS
C ------
910   WRITE(LFNERR,911) TRIM(FILEXT)
911   FORMAT(/,' *** PG PHCCNV: BLANK LINE NOT FOUND IN EXTERNAL FILE.',
     1         /,16X,'FILE NOT CONVERTED!',
     2         /,16X,'FILE NAME: ',A,/)
      IRCODE=2
      GOTO 999
C
920   WRITE(LFNERR,921) IRCV,TRIM(FILEXT)
921   FORMAT(/,' *** PG PHCCNV: ERROR WHILE READING ANTENNA BLOCK.',
     1         /,16X,'FILE NOT CONVERTED!',
     2         /,16X,'ANTENNA BLOCK NUMBER:',I3,
     3         /,16X,'FILE NAME           : ',A,/)
      IRCODE=2
      GOTO 999
C
930   WRITE(LFNERR,931) IRCV,TRIM(FILEXT)
931   FORMAT(/,' *** PG PHCCNV: END OF EXTERNAL FILE WHILE READING ',
     1         'ANTENNA BLOCK.',
     2         /,16X,'FILE NOT CONVERTED!',
     3         /,16X,'ANTENNA BLOCK NUMBER:',I3,
     4         /,16X,'FILE NAME           : ',A,/)
      IRCODE=2
      GOTO 999
C
940   WRITE(LFNERR,941) TRIM(FILEXT)
941   FORMAT(/,' *** PG PHCCNV: END OF HEADER NOT FOUND IN EXTERNAL',
     1         ' FILE.',
     2         /,16X,'FILE NOT CONVERTED!',
     3         /,16X,'FILE NAME: ',A,/)
      IRCODE=2
      GOTO 999
C
950   WRITE(LFNERR,951) STRING,HEAD
951   FORMAT(' SR PHCCNV: ERROR DECODING DATA ON THE FOLLOWING LINE:',
     1       /,1X,2A,/)
      IRCODE=2
      GOTO 999
C
960   WRITE(LFNERR,961) GNSS,IFRQ,IRCV,TRIM(FILEXT)
961   FORMAT(/,' *** PG PHCCNV: ERROR WHILE READING FREQUENCY SECTION.',
     1         /,16X,'FILE NOT CONVERTED!',
     2         /,16X,'FREQUENCY NO        : ',A1,I2,
     3         /,16X,'ANTENNA BLOCK NUMBER: ',I3,
     4         /,16X,'FILE NAME           : ',A,/)
      IRCODE=2
      GOTO 999
C
970   WRITE(LFNERR,971) IRCV,NPCV(1,IRCV),TRIM(FILEXT)
971   FORMAT(/,' *** PG PHCCNV: ERROR WRITING FORMAT DESCRIPTION.',
     1         /,16X,'FILE NOT CONVERTED!',
     2         /,16X,'ANTENNA BLOCK NUMBER:',I3,
     3         /,16X,'# OF RECORDS        :',I3,
     4         /,16X,'FILE NAME           : ',A,/)
      IRCODE=2
      GOTO 999
C
C END
C ---
999   CALL EXITRC(IRCODE)
      END


