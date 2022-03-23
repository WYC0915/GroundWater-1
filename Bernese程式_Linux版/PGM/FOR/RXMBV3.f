C*
      PROGRAM RXMBV3
CC
CC NAME       :  RXMBV3
CC
CC PURPOSE    :  READ RINEX METEOROLOGICAL DATA FILES AND
CC               COPY THEM INTO BERNESE VERSION 3 FILES
CC               FOLLOWING TYPES ARE AVAILABLE:
CC                 ITYP=1: PRESSURE, TEMPERATURE, HUMIDITY
CC                 ITYP=2: PRESSURE, DRY TEMPERATURE, WET TEMPERATURE
CC                 ITYP=5: PRESSURE, TEMPERATURE, HUMIDITY, AND WET
CC                         ZENITH DELAY
CC                 ITYP=6: PRESSURE, TEMPERATURE, HUMIDITY, AND
CC                         PRECIPITABLE WATER VAPOUR IN ZENITH
CC               ITYP=3 AND ITYP=4 ARE NOT SUPPORTED HERE BUT ARE USED
CC               IN BERNESE.
CC
CC AUTHOR     :  W. GURTNER
CC
CC CREATED    :  89/08/28
CC
CC CHANGES    :  23-DEC-92 : ??: USE OF SR "OPNFIL TO OPEN FILES
CC               20-APR-94 : ??: RINEX VERSION 2 HEADER
CC               10-AUG-94 : MR: CALL EXITRC
CC               31-AUG-94 : MR: WRITE MORE GENERAL METEO FILE
CC               20-SEP-94 : MR: ADD PARAMETER "ISTOPS" TO CALL GTSTNA
CC               26-SEP-95 : TVH:ADD PWV OR ZENITH DELAY OBS
CC               26-SEP-95 : TVH:CHECK FOR FLAGGED TEMP & PR. VALUES
CC                               (-999S)
CC               26-SEP-95 : TVH:CORRECT TYPO IN CHECK FOR TEMP & PR.
CC                               VALUES
CC               26-SEP-95 : TVH:CORRECT OUT FORMAT TO MATCH RINEX MET
CC                               FORMAT
CC               26-SEP-95 : TVH:IF NO UNFLAGGED VALUES CLOSE AND DEL
CC                               MET FILE
CC               29-SEP-95 : MR: REDEFINITION OF METEO FILE TYPES,
CC                               WRITE MODEL NUMBER
CC               20-NOV-95 : TS: ASSUME RH == HR
CC               04-APR-96 : MR: CHANGE FORMAT "X" --> "1X"
CC               12-JUL-96 : TS: CHANGED CALL OF GTSTNA
CC                               (NO TIME AVAILABLE BE CAREFUL)
CC               06-JUL-99 : PF: CALL IYEAR4 FOR CONVERSION YY->YYYY
CC               12-JUL-99 : SS: "STASTR" IN CALL OF SR GTSTNA
CC               16-SEP-99 : MR: MAXDAT TO 60000 (OLD 10000)
CC               09-NOV-00 : CU: SWITCH TO NEW MENU SYSTEM
CC               29-OCT-01 : RD: USE PRITIT, NEW CALL OF GTSTNA
CC               23-APR-03 : HU: NULLIFY LOCAL POINTERS
CC               16-MAY-03 : HB: INITIALIZE STRUCTURE
CC               09-JUL-03 : RD: READ STAINFO FOR GTSTNA IN RXGINPT
CC               22-AUG-03 : HU: CHECK KEYWORD 'STAINFO'
CC               11-SEP-03 : HU: WRITE ONLY LAST EPOCH IF EPOCHS EQUAL
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               15-DEC-03 : CU: READ FIRST EPOCH OF METEO FOR STAINFO FILE
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: LFNUM.inc, COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               27-FEB-07 : AG: CALL DEFCON
CC               21-JUL-08 : PS: INCREASE MAXDAT 60000->100000
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               30-MAY-11 : SL: M_BERN WITH ONLY, MAXTYP 4->9
CC               01-DEC-11 : SL: NEW TITLE STRING FOR PRITIT
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1989     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: fileNameLength,
     1                    lfn001, lfn002, lfnPrt, lfnErr
      USE m_cpu,    ONLY: cpu_start
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE d_stacrx, ONLY: t_stacrux, init_staCrux
      USE s_gtfile2
      USE s_dordup
      USE s_opnfil
      USE s_gtstna
      USE s_r2rdmh
      USE s_gtfile
      USE s_pritit
      USE s_readinpf
      USE s_opnerr
      USE s_rxmb3i
      USE s_exitrc
      USE s_opnsys
      USE s_defcon
      USE s_jmt
      USE s_radgms
      USE s_rxrdmr
      USE s_upperc
      USE s_gtflna
      USE f_djul
      USE f_iyear4
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , ID    , IDAT  , IDELTA, IDELTI, IFIL  , IH    ,
     1          IHR   , IMODEL, IOLD  , IOSTAT, IPR   , IPW   , IRXVRS,
     2          IS    , ISTOPS, ITD   , ITPRAD, ITW   , ITYP  , ITYPI ,
     3          IVAL  , IY    , IZW   , JDAT  , K     , LFNMET,
     4          LFNRNX, MAXCOM, MAXDAT, MAXFIL, MAXTYP, MFIL  , MI    ,
     5          MO    , MXFTYP, NCOM  , NCOMI , NCOMN , NDAT  , NEWOLD,
     6          NFILE , NFLCOL, NUMTYP, IZD   , IZT
C
      REAL*8    DAY   , EPOCHI, SEC
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C MAXIMAL DIMENSIONS
C ------------------
      PARAMETER (MAXFIL=400)
      PARAMETER (MAXDAT=100000)
      PARAMETER (MXFTYP=6)
      PARAMETER (MAXTYP=9)
C
C MAXFIL: MAXIMUM NUMBER OF INPUT
C MXFTYP: MAXIMUM NUMBER OF DIFFERENT MET.FILE TYPES
C MAXTYP: MAXIMUM NUMBER OF DIFFERENT OBSERVATION TYPES
C
C DECLARATIONS
C ------------
      TYPE(t_stacrux) :: stacrux
      CHARACTER*80 HEDLIN(MXFTYP)
      CHARACTER*32 FILMET,FILRNX(MAXFIL),metfil
      CHARACTER(LEN=filenameLength),DIMENSION(:,:),POINTER :: filnam
      CHARACTER*16 STANAI,STNAME(1)
      CHARACTER*8  CMPNAM
      CHARACTER*2  PR,TD,TW,HR
      CHARACTER*2  PW,ZW
      CHARACTER*1  CHR1
      INTEGER*4    INDEX(MAXDAT),NVAL(MXFTYP)
      REAL*8       DATINP(MAXTYP), DUMMY(MAXTYP)
C
C  R2RDMH
      PARAMETER(MAXCOM=10)
      CHARACTER    RUNBY*20,PRGNAM*20,COMENT(MAXCOM)*60
      CHARACTER    CRDATE*9,CRTIME*5,STANAM*60,OBSTYP(MAXTYP)*2
C
C  RXRDMR
      REAL*8       EPOCH(MAXDAT),DATMET(MAXTYP,MAXDAT)
      REAL*8       TFIRST
      INTEGER*4    irc
C
C COMMON BLOCKS
C -------------
      COMMON/LARGE/FILRNX,INDEX,EPOCH,DATMET
C
      DATA PR/'PR'/,TD/'TD'/,TW/'TW'/,HR/'HR'/,ZW/'ZW'/,PW/'PW'/
      DATA HEDLIN/'  JJ MM DD HH MM SS  PPP.PP  TT.TT  HH.HH',
     2            '  JJ MM DD HH MM SS  PPP.PP  TT.TT  TT.TT',
     3            '  JJ MM DD HH MM SS  DDDD.DDDD',
     4            '  JJ MM DD HH MM SS  DDDD.DDDD',
     5            '  JJ MM DD HH MM SS  PPP.PP  TT.TT  HH.HH  DDDD.DD',
     6            '  JJ MM DD HH MM SS  PPP.PP  TT.TT  HH.HH  VVVV.VV'/
      DATA NVAL/3,3,1,0,4,4/
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C DEFINE LOGICAL FILE NUMBERS
C ---------------------------
      LFNRNX=LFN001
      LFNMET=LFN002
C
C NULLIFY POINTERS
C ----------------
      NULLIFY(filnam)
      CALL INIT_STACRUX(staCrux)
C
C GET THE NAME OF THE INPUT FILE
C ------------------------------
      CALL init_inpkey(inpKey)
      CALL readinpf(' ',inpKey)
C
C DEFINE SYSTEM FILES AND CONSTANTS
C ---------------------------------
      CALL OPNSYS
      CALL DEFCON(0)
C
C WRITE THE PROGRAM OUTPUT HEADER
C -------------------------------
      CALL PRITIT('RXMBV3',
     1            'Transfer RINEX meteo files to Bernese format')
C
C READ INPUT/OUTPUT FILENAMES
C ---------------------------
      NFLCOL=1
      CALL GTFILE('OBSFIL ',NFLCOL,MAXFIL,NFILE,FILRNX)
C
C READ OPTIONS FILE
C ---------------------------
      CALL rxmb3I(cmpnam,coment(1),newold,istops,staCrux)
      IF(COMENT(1).EQ.' ') THEN
        NCOM=0
      ELSE
        NCOM=1
      END IF
      NDAT=0
C
      IF (newold .EQ. 1) THEN
        metfil = 'METEON'
      ELSE IF (newold .EQ. 0) THEN
        metfil = 'METEOA'
      END IF
C
      CALL gtflna(0,metfil,filmet,irc)
      IF (newold .EQ. 1 .AND. filmet .EQ. ' ') THEN
        CALL gtfile2('OBSFIL ',2,mFil,filnam)
        filmet = filnam(2,1)
        DEALLOCATE(filnam,stat=irc)
      END IF
C
      WRITE(LFNPRT,11)
11    FORMAT(' FILE  RINEX METEO FILE NAME             ',
     1       'BERNESE METEO FILE NAME           #REC',
     2        /,' ',4('-'),2X,32('-'),2X,32('-'),2X,4('-'),/)
C
      DO 100 IFIL=1,NFILE
C
        CALL OPNFIL(LFNRNX,FILRNX(IFIL),'OLD','FORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNRNX,IOSTAT,FILRNX(IFIL),'RXMBV3')
        CALL R2RDMH(LFNRNX,LFNERR,MAXCOM-NCOM,PRGNAM,RUNBY,
     1              CRDATE,CRTIME,NCOMN,COMENT(NCOM+1),STANAM,
     2              NUMTYP,OBSTYP,IRXVRS,IRC)
C
C  READ FIRST RINEX RECORD TO GET EPOCH FOR STAINFO FILE
        CALL RXRDMR(LFNRNX,LFNERR,NUMTYP,TFIRST,
     1              DUMMY,IRC)
C  ERROR
        IF(IRC.NE.0) THEN
          WRITE(LFNERR,201) IRC,FILRNX(IFIL)
          CALL EXITRC(2)
        END IF
        BACKSPACE LFNRNX
C
        IF(IRC.NE.0) GOTO 999
        NCOMI=NCOM+NCOMN
        CALL UPPERC(STANAM)
        CALL GTSTNA(ISTOPS,1,TFIRST,1,(/STANAM(1:16)/),(/' '/),
     1              'STAINFO',staCrux,STNAME)
        JDAT=0
C
C  DETERMINE MET.TYPE
        IPR=0
        ITD=0
        IHR=0
        IZW=0
        IZD=0
        IZT=0
        ITW=0
        IPW=0
        DO 60 I=1,NUMTYP
          IF(OBSTYP(I).EQ.'PR')                      IPR=I
          IF(OBSTYP(I).EQ.'TD')                      ITD=I
          IF(OBSTYP(I).EQ.'HR'.OR.OBSTYP(I).EQ.'RH') IHR=I
          IF(OBSTYP(I).EQ.'ZW')                      IZW=I
          IF(OBSTYP(I).EQ.'ZD')                      IZD=I
          IF(OBSTYP(I).EQ.'ZT')                      IZT=I
          IF(OBSTYP(I).EQ.'TW')                      ITW=I
          IF(OBSTYP(I).EQ.'PW')                      IPW=I
60      CONTINUE
C
        ITYP=0
        IF(IPR.NE.0.AND.ITD.NE.0.AND.IHR.NE.0)              ITYP=1
        IF(IPR.NE.0.AND.ITD.NE.0.AND.ITW.NE.0)              ITYP=2
        IF(IZT.NE.0)                                        ITYP=3
        IF(IPR.NE.0.AND.ITD.NE.0.AND.IHR.NE.0.AND.IZW.NE.0) ITYP=5
        IF(IPR.NE.0.AND.ITD.NE.0.AND.IHR.NE.0.AND.IPW.NE.0) ITYP=6
        IF(ITYP.EQ.0) THEN
          WRITE(LFNERR,61) FILRNX(IFIL),(OBSTYP(K),K=1,NUMTYP)
61        FORMAT(/,' *** PG RXMBV3: ILLEGAL MET.TYPE IN FILE : ',A,
     1                       /,16X,'MET.TYPES                : ',
     2             10(A2,1X),/)
          CALL EXITRC(2)
        END IF
C
        IF(IFIL.EQ.1) THEN
C
          IF(NEWOLD.EQ.1) THEN
            ITYPI=ITYP
            STANAI=STNAME(1)
          ELSE
C
C  READ EXISTING MET.FILE
            CALL OPNFIL(LFNMET,FILMET,'OLD','FORMATTED',
     1                  ' ',' ',IOSTAT)
            CALL OPNERR(LFNERR,LFNMET,IOSTAT,FILMET,'RXMBV3')
            READ(LFNMET,103) CMPNAM,COMENT(1)
103         FORMAT(A8,11X,A53)
            READ(LFNMET,104) STANAI,IDELTI,ITYPI
104         FORMAT(10X,A16,26X,I3,5X,I2,/)
            DO 110 IDAT=1,MAXDAT
              READ(LFNMET,*,END=120) IY,MO,ID,IH,MI,IS,
     1                               (DATINP(K),K=1,NVAL(ITYP))
              DO 115 IVAL=1,NVAL(ITYP)
                DATMET(IVAL,IDAT)=DATINP(IVAL)
115           CONTINUE
              IF(IY.LT.0) GOTO 120
              IY = IYEAR4(IY)
              DAY=ID+IH/24.D0+MI/1440.D0+IS/86400.D0
              EPOCH(IDAT)=DJUL(IY,MO,DAY)
110         CONTINUE
120         CLOSE(UNIT=LFNMET)
            NDAT=IDAT-1
          END IF
        END IF
C
C  CHECK STATION NAME, FILETYPE
        IF(STANAI.NE.STNAME(1)) THEN
          WRITE(LFNERR,121) FILRNX(IFIL),STANAM,STANAI
121       FORMAT(/,' *** PG RXMBV3: INCONSISTENT STATION NAMES',
     1           /,16X,'FILE                  : ',A,
     2           /,16X,'CURRENT  STATION NAME : ',A,
     3           /,16X,'PREVIOUS STATION NAME : ',A,/)
          CALL EXITRC(2)
        END IF
        IF(ITYP.NE.ITYPI) THEN
          WRITE(LFNERR,122) FILRNX(IFIL),ITYP,ITYPI
122       FORMAT(/,' *** PG RXMBV3: INCONSISTENT OBSERVATION TYPES',
     1           /,16X,'FILE          : ',A,
     2           /,16X,'CURRENT  TYPE : ',I3,
     3           /,16X,'PREVIOUS TYPE : ',I3,/)
          CALL EXITRC(2)
        END IF
C
C  READ ONE RINEX RECORD AT A TIME
200     CALL RXRDMR(LFNRNX,LFNERR,NUMTYP,EPOCHI,
     1              DATINP,IRC)
C  END OF FILE
        IF(IRC.EQ.9) GOTO 210
C  ERROR
        IF(IRC.NE.0) THEN
          WRITE(LFNERR,201) IRC,FILRNX(IFIL)
201       FORMAT(/,' *** PG RXMBV3: RETURN CODE',I3,' IN RINEX FILE ',
     1              A,/)
          CALL EXITRC(2)
        END IF
C
C  CHECK FOR FLAGGED PR OR TEMPS
C  AND SKIP EPOCH IF EITHER IS FLAGGED
C
        IF(DATINP(IPR).LE.-99 .OR. DATINP(ITD).LT. -99) GOTO 200
C
C DO NOT INCREMENT IF EPOCH IS THE SAME FOR NDAT AND NDAT-1
        IF (NDAT.EQ.0) THEN
          JDAT=JDAT+1
          NDAT=NDAT+1
        ELSE
          IF (EPOCHI.NE.EPOCH(NDAT)) THEN
            JDAT=JDAT+1
            NDAT=NDAT+1
          ENDIF
        ENDIF
C
        IF(NDAT.GT.MAXDAT) THEN
          WRITE(LFNERR,202) FILRNX(IFIL)
202       FORMAT(/,' *** PG RXMBV3: TOO MANY DATA RECORDS IN FILE ',A,
     1           /,16X,'ADJUST "MAXDAT" IN THE PROGRAM',/)
          CALL EXITRC(2)
        END IF
        EPOCH(NDAT)=EPOCHI
        IF(ITYP.EQ.1) THEN
          DATMET(IPR,NDAT)=DATINP(1)
          DATMET(ITD,NDAT)=DATINP(2)
          DATMET(IHR,NDAT)=DATINP(3)
        ELSE IF(ITYP.EQ.2) THEN
          DATMET(IPR,NDAT)=DATINP(1)
          DATMET(ITD,NDAT)=DATINP(2)
          DATMET(ITW,NDAT)=DATINP(3)
        ELSE IF(ITYP.EQ.3) THEN
          DATMET(IZT,NDAT)=DATINP(1)
        ELSE IF(ITYP.EQ.5) THEN
          DATMET(IPR,NDAT)=DATINP(1)
          DATMET(ITD,NDAT)=DATINP(2)
          DATMET(IHR,NDAT)=DATINP(3)
          DATMET(IZW,NDAT)=DATINP(4)
        ELSE IF(ITYP.EQ.6) THEN
          DATMET(IPR,NDAT)=DATINP(1)
          DATMET(ITD,NDAT)=DATINP(2)
          DATMET(IHR,NDAT)=DATINP(3)
          DATMET(IPW,NDAT)=DATINP(4)
        END IF
        GOTO 200
C
210     CLOSE(UNIT=LFNRNX)
        WRITE(LFNPRT,211) IFIL,FILRNX(IFIL),FILMET,JDAT
211     FORMAT(I5,2X,A32,2X,A32,I6)
C
100   CONTINUE
C
      WRITE(LFNPRT,'( )')
C
C  SORT RECORDS ACCORDING TO EPOCH
      CALL DORDUP(EPOCH,NDAT,INDEX)
C
C  OPEN NEW MET. FILE, CREATE HEADER
C
C  IF NO GOOD RECORDS DON'T WRITE HEADER
C
      IF (NDAT.LT.1) THEN
       CLOSE(UNIT=LFNMET,STATUS='DELETE')
       GOTO 999
      ENDIF
C
      IDELTA=0
      IMODEL=0
      CALL OPNFIL(LFNMET,FILMET,'UNKNOWN','FORMATTED',
     1            ' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNMET,IOSTAT,FILMET,'RXMBV3')
      WRITE(LFNMET,101) CMPNAM,TRIM(COMENT(1))
101   FORMAT(A8,11X,A)
      WRITE(LFNMET,102) STNAME(1),IDELTA,ITYP,NVAL(ITYP),IMODEL,
     1                  TRIM(HEDLIN(ITYP))
102   FORMAT('STATION : ',A16,3X,'UTC-LOCAL TIME(HOURS) =',I3,
     1       ' TYP=',I2,'  #VALUES=',I3,'  MOD=',I3,
     2             /,A)
C
C  WRITE MET.DATA
      IOLD=0
      DO 300 IDAT=1,NDAT
        I=INDEX(IDAT)
        IF(IOLD.NE.0) THEN
          IF (EPOCH(I).EQ.EPOCH(IOLD)) GOTO 300
        ENDIF
        IOLD=I
        CALL JMT(EPOCH(I),IY,MO,DAY)
        IY=MOD(IY,100)
        ID=DAY
        ITPRAD=3
        CALL RADGMS(ITPRAD,DAY,CHR1,IH,MI,SEC)
        IS=IDNINT(SEC)
        WRITE(LFNMET,301) IY,MO,ID,IH,MI,IS,
     1                    (DATMET(K,I),K=1,NVAL(ITYP))
301     FORMAT(1X,6I3,1X,3F7.2,1X,F8.2)
C
300   CONTINUE
      WRITE(LFNMET,'( )')
      CLOSE(UNIT=LFNMET)
C
999   CALL EXITRC(0)
      END
