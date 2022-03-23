C*
      PROGRAM PREWEI
CC
CC NAME       :  PREWEI
CC
CC PURPOSE    :  IMPROVES THE SATELLITE SPECIFIC WEIGTHS
CC               OF PRECISE ORBIT FILES BASED ON AN
CC               ORBIMP/ORBGEN RMS-LIST FILE
CC
CC REMARKS    :  ACCURACY CODES MAY BE SOURCED DIRECTLY FROM THE LSTFILE
CC               INTO THE OUTPUT PRE-FILE IF THEY ARE <= 0. THE OPTION
CC               "TABLE" IS IGNORED IN THIS CASE.
CC
CC AUTHOR     :  R.WEBER
CC
CC CREATED    :  94/10/25
CC
CC CHANGES    :  01-ARP-96 : TS: SEVERAL COSMETIC CHANGES
CC               02-APR-96 : TS: USE "CEN" FIELD IF DAY NOT FOUND
CC                               (FOR PRED.)
CC               05-JUN-96 : MR: DATA STATEMENT OUT OF ORDER
CC               17-JUN-96 : MR: USE EXITRC, NOT STOP
CC               10-JUL-96 : TS: REMOVE SATELLITE OPTION ADDED
CC               13-MAY-97 : MR: REMOVE UNUSED VARIABLE "TMPNUM"
CC               10-JUL-97 : DI: REPLACE SR RDPHE2 BY SR RDPREH AND
CC                               SR WPRECH BY SR WTPREH
CC               11-JUL-97 : DI: USE SR RDPREI AND SR WTPREI
CC               11-JUL-97 : DI: USE MAXSAT.inc
CC               15-JUL-97 : DI: SEARCH DAY OF YEAR IN PRECISE FILE
CC                               HEADER AND NOT IN PRECISE FILE NAME
CC               23-JUL-97 : MR: CORRECT SVNNUM, SVNNU1 TREATED AS ARRAYS
CC               07-MAY-98 : MR: REORDER IMPLICIT STATEMENTS
CC               27-AUG-98 : MR: USE FUNCTION "MODSVN"
CC               16-AUG-99 : JJ: COMMENT OUT UNUSED VAR LPRE
CC               15-MAR-00 : TS: CORRECT MANSAT HANDLING
CC               03-MAY-00 : TS: SUPPORT ORBGEN LST OUTPUT
CC               30-JUN-00 : SS: BUG WRT ORBGEN OPTION REMOVED
CC               16-NOV-00 : RD: SWITCH TO THE NEW MENU SYSTEM
CC               18-OCT-01 : MM: NEW OPTION FOR COMPUTATION OF ACCURACY
CC               11-NOV-02 : HU: SP3C IMPLEMENTED
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               03-MAY-03 : HU: READ MORE THAN 32 SATELLITES FROM LST FILE
CC               06-AUG-03 : HU: MAXLST INSTEAD OF MAXSAT FOR ORBGEN LIST
CC               07-OCT-03 : SS: ALLOW ZERO-VALUED ACCURACY CODE
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               15-JAN-04 : RD: SIMPLIFY AND CORRECT MANEUVER LOGIC
CC                           SS: AC-VALUE .LE.0 IN THE LSTFIL
CC               10-MAR-04 : HB: CHANGE ORDER OF MODULES
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: (COM)LFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               01-AUG-05 : HU: EPOCH AS STRUCTURE
CC               24-APR-08 : SS: FORMAT STATEMENTS FROM 48 TO 100 SATS
CC               19-JUL-10 : SL: TAB CHARACTERS REMOVED
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               14-NOV-11 : SL: USE M_BERN WITH ONLY, PRITIT CALL ADDED
CC               30-NOV-11 : SL: NEW TITLE STRING FOR PRITIT
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1994     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
      USE m_bern,   ONLY: lfnPrt, lfnErr, lfnRes, lfnEph, lfnLoc, lfn001
      USE m_cpu,    ONLY: cpu_start
      USE m_maxdim, ONLY: MAXSAT
      USE m_epoch,  ONLY: t_epoch
      USE D_INPKEY, ONLY: INPKEY, INIT_INPKEY
      USE s_opnfil
      USE s_gtfile
      USE s_readinpf
      USE s_opnerr
      USE s_rdpreh
      USE s_rdprei
      USE s_wtpreh
      USE s_wtprei
      USE s_defcon
      USE s_pritit
      USE s_exitrc
      USE s_prwinp
      USE s_opnsys
      USE s_jmt
      USE s_gtflna
      USE f_djul
      USE f_modsvn

      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I2RC  , IANZP , IDAY  , IDEL  , IFIL  , IFRMAT,
     1          II    , III   , IIJ   , IIK   , IIL   , IIRC  , ILOOP ,
     2          IMONTH, IOSTAT, IRC   , IRCSCR, IREADE, ISAT  , IY    ,
     3          IYEAR , IYR   , J     , JSAT  , K     , LLDAY , LSAT  ,
     4          MAXFIL, MAXLST, MAXPAR, NDEL  , NEPO  , NFFIL ,
     5          NFLCOL, NOFIL , NSAT  , NSAT1
C
      REAL*8    BASCLK, BASPOS, DTTAB , RMJD  , TEPOCH, TFIRST, XDAY
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
C MAXIMAL DIMENSIONS
C ------------------
C MAXFIL: MAXIMUM NUMBER OF FILES
C MAXSAT: MAXIMUM NUMBER OF SATELLITES
C
      PARAMETER (MAXFIL=100,MAXPAR=20,MAXLST=64)
C
C DECLARATIONS
C ------------
      INTEGER*4     SVNNUM(MAXSAT),NEWWGT(MAXSAT),OLDWGT(MAXSAT)
      INTEGER*4     ORBRMS(MAXLST),LSTNUM(MAXLST),DELNUM(MAXSAT)
      INTEGER*4     SVNNU1(MAXSAT),DELORD(MAXSAT)
      INTEGER*4     MINACC, MAXACC, FLGDEL, TABLE
      INTEGER*4     IWRTE(2),ACCPOS(4,MAXSAT),ACCVEL(4,MAXSAT)
      LOGICAL       FROFIL
C
      REAL*8        RMSP(MAXPAR), POS(3,MAXSAT),VEL(3,MAXSAT)
      REAL*8        DTSATC(MAXSAT), DDTSAT(MAXSAT)
      REAL*8        SDEVP(4,MAXSAT),SDEVV(4,MAXSAT)
      REAL*8        CORRP(6,MAXSAT),CORRV(6,MAXSAT)
C
      CHARACTER*512 STRING
C     CHARACTER*80  TITLE
      CHARACTER*57  TITPRE(4)
      CHARACTER*32  FILNAM,FILSCR,FILPRE,FFILES(2,MAXFIL),OFILES(MAXFIL)
      CHARACTER*5   COOSYS,DATDES
      CHARACTER*4   AGENCY
C     CHARACTER*4   LPRE
      CHARACTER*3   ORBTYP,LCENT,LDAY,TIMSYS
      CHARACTER*2   LYEAR,FILTYP
      CHARACTER*1   EVTFLG(4,MAXSAT)
C
      TYPE(t_epoch) :: TMJD
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
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
C PRINT TITLE
C -----------
      CALL pritit('PREWEI',
     1            'Set accuracy codes in precise orbits')
C
C READ FILE WITH LIST OF RESIDUAL FILES (??)
C -------------------------------------
      NFLCOL=2
      CALL GTFILE('PREFIL',NFLCOL,MAXFIL,NFFIL,FFILES)
C
C READ THE LIST OF OUTPUT FILES
C -----------------------------
      NFLCOL=1
      CALL GTFILE('PREOUT',NFLCOL,MAXFIL,NOFIL,OFILES)
C
C A VALID LIST OF OUTPUT FILES WAS FOUND
C --------------------------------------
      IF (NOFIL .EQ. NFFIL) THEN
        DO IFIL=1,NFFIL
          FFILES(2,IFIL)=OFILES(IFIL)
        ENDDO
      ELSE
        IF (NOFIL .NE. 0) THEN
          WRITE(LFNERR,'(/,A,/,16X,A,/,2(16X,A,I3,/))')
     1    ' *** PGM PREWEI: NUMBER OF SELECTED OUTPUT FILES IS NOT',
     2                    ' EQUAL TO THE NUMBER OF INPUT FILES',
     3                    ' NUMBER OF INPUT FILES:  ',NFFIL,
     4                    ' NUMBER OF OUTPUT FILES: ',NOFIL
          CALL EXITRC(2)
        ENDIF
      ENDIF
C
C READ INPUT OPTIONS
C ------------------
      CALL PRWINP(MAXPAR,IANZP,RMSP,MINACC,MAXACC,FROFIL,FLGDEL,TABLE)
      IREADE=1
C
C WRITE TITLE FOR OUTPUT FILE
C ---------------------------
CC      WRITE(LFNPRT,11) DATE,TIME,TITLE(1:65)
CC 11    FORMAT(/,' ',79('*'),
CC     1       /,' ',' PROGRAM PREWEI',33X,A9,1X,A5,
CC     2       /,' ',79('*'),//,' ',79('-'),
CC     3       /,' TITLE: ',A65,/,' ',79('-'),/)

C
C GET SCRATCH FILE NAME
C ---------------------
      CALL GTFLNA(1,'SCRATCH',FILSCR,IRCSCR)
C
C OPEN SUMMARY LIST-FILE
C ----------------------
      CALL GTFLNA(0,'LIST   ',FILNAM,IRC)
      IF (IRC.EQ.1) THEN
        WRITE(LFNERR,901)
901     FORMAT(//,' *** PR PREWEI: SUMMARY FILE NOT AVAILABLE',//)
        CALL EXITRC(2)
      ENDIF
      CALL OPNFIL(LFN001,FILNAM,'OLD','FORMATTED',
     1            'READONLY',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFN001,IOSTAT,FILNAM,'PREWEI')
C
C ORBIMP FORMAT
C -------------
12    FORMAT(//,4X,68X,A2,/,15X,A3,////)
14    FORMAT(5X,126(I2,1X))
16    FORMAT(4X,126(I3))
C
C ORBGEN FORMAT
C -------------
13    FORMAT(///)
15    FORMAT(5X,126(I3,1X))
17    FORMAT(4X,126(I4))
C
C TEST IF ORBIMP OR ORBGEN SUMMARY
C --------------------------------
      LYEAR='  '
      IYR=-1
      LCENT='   '
      IFRMAT=0
      READ(LFN001,12,ERR=20,IOSTAT=IOSTAT) LYEAR,LCENT
20    READ(LYEAR,'(I2)',ERR=25,IOSTAT=IOSTAT)IY
      IYR=IY
25    IF (IYR.EQ.-1) THEN
        WRITE(LFNERR,902) FILNAM
902     FORMAT(//,1X,'### PR PREWEI: ORBGEN OUTPUT DETECTED!',/,
     1           16X,'FILE NAME    : ',A32,//)
        CLOSE(LFN001)
        CALL OPNFIL(LFN001,FILNAM,'OLD','FORMATTED',
     1            'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFN001,IOSTAT,FILNAM,'PREWEI')
        IFRMAT=1
        READ(LFN001,12)
      ENDIF
C
      READ(LFN001,132) STRING
132   FORMAT(A)
      IF (IFRMAT.EQ.0) THEN
        READ(STRING,14,IOSTAT=IOSTAT) (LSTNUM(K),K=1,MAXLST)
      ELSE
        READ(STRING,15,IOSTAT=IOSTAT) (LSTNUM(K),K=1,MAXLST)
      ENDIF
      IF (IOSTAT.NE.0) THEN
        WRITE(LFNERR,"(/,' *** PG PREWEI: ERROR READING LST FILE',/)")
        CALL EXITRC(2)
      ENDIF
      LSAT=0
      DO 5 III=1,MAXLST
        IF (LSTNUM(III).GT.0) LSAT=LSAT+1
5     CONTINUE
C
C LOOP OVER ALL PREFILES
C ----------------------
      DO 100 ILOOP=1,NFFIL
        WRITE(LFNPRT,115) ILOOP,FFILES(1,ILOOP)
115     FORMAT(/,2X,I4,2X,A32)
        DO J=1,MAXSAT
          SVNNUM(J)=0
          NEWWGT(J)=-99
          OLDWGT(J)=0
        ENDDO
C
C READ HEADERS OF PRECISE ORBIT FILES
C -----------------------------------
        CALL RDPREH(FFILES(1,ILOOP),LFNRES,IFRMAT,NSAT,SVNNUM,OLDWGT,
     1              TFIRST,NEPO,DTTAB,TITPRE,DATDES,COOSYS,ORBTYP,
     2              AGENCY,FILTYP,TIMSYS,BASPOS,BASCLK)
C
C CALCULATE DAY OF YEAR
C ---------------------
        CALL JMT(TFIRST+1D-10,IYEAR,IMONTH,XDAY)
        RMJD=DJUL(IYEAR,1,1D0)
        IDAY=INT(TFIRST+1D-10)-NINT(RMJD)+1
C
C LOOP OVER ALL LINES IN SUMMARY FILE
C -----------------------------------
        DO 30 III=1,1000
          READ(LFN001,132) STRING
          READ(STRING,31) LDAY
31        FORMAT(1X,A3)
C
          IF (LDAY.EQ.'---') GOTO 30
C
C SEARCH FOR CORRESPONDING FILE
C -----------------------------
          IF (LDAY.EQ.'ALL') THEN
            WRITE(LFNERR,903) FILNAM,FFILES(1,ILOOP)
903         FORMAT(//,1X,'### PR PREWEI: DAY-NUMBER NOT AVAILABLE!',/,
     1                16X,'USING VALUES IN "ALL" FIELD',/,
     2                16X,'LIST-FILE    : ',A32,/,
     3                16X,'PRECISE FILE : ',A32,//)
          ELSE
            READ (LDAY,'(I3)') LLDAY
            IF(IDAY.NE.LLDAY) GOTO 30
          ENDIF
C
          IF (IFRMAT.EQ.0) THEN
            READ(STRING,16) (ORBRMS(K),K=1,LSAT)
          ELSE
            READ(STRING,17) (ORBRMS(K),K=1,LSAT)
          ENDIF
C
C HANDLE MANOEUVRE SATELLITES
C ---------------------------
          DO 60 ISAT=1,LSAT
            IF (LSTNUM(ISAT).EQ.MODSVN(LSTNUM(ISAT))) GOTO 60
            DO JSAT=1,LSAT
              IF (LSTNUM(JSAT).EQ.MODSVN(LSTNUM(ISAT))) THEN
                IF (ORBRMS(JSAT).GT.0.AND.ORBRMS(ISAT).GT.0) THEN
                  ORBRMS(JSAT)=(ORBRMS(JSAT)+ORBRMS(ISAT))/2
                  ORBRMS(ISAT)=ORBRMS(JSAT)
                ELSE IF(ORBRMS(JSAT).EQ.0.AND.ORBRMS(ISAT).NE.0) THEN
                  ORBRMS(JSAT)=ORBRMS(ISAT)
                ENDIF
              ENDIF
            ENDDO
60        CONTINUE
C
C DETERMINE WEIGHT
C ----------------
CC          NDEL=0
          DO 200 IIJ=1,NSAT
            DO 210 IIL=1,LSAT
              IF(SVNNUM(IIJ).EQ.MODSVN(LSTNUM(IIL))) THEN
                IF (ORBRMS(IIL).LE.0) THEN
                  NEWWGT(IIJ)=-ORBRMS(IIL)
C
C USE VALUES FROM TABLE?
C ----------------------
                ELSE IF (TABLE.EQ.1) THEN
                  NEWWGT(IIJ)=MINACC+IANZP
                  DO 220 IIK=1,IANZP
                    IF (ORBRMS(IIL)/1D2.LE.RMSP(IIK)) THEN
                      NEWWGT(IIJ)=MINACC+IIK-1
C
C CHECK IF BAD SATELLITES SHOULD BE REMOVED
C -----------------------------------------
CC                      IF (NEWWGT(IIJ).GT.MAXACC) THEN
CC                        NEWWGT(IIJ)=0
CC                        IF (FLGDEL.EQ.1) THEN
CC                          NDEL=NDEL+1
CC                          DELNUM(NDEL)=SVNNUM(IIJ)
CC                        ENDIF
CC                      ENDIF
CC                      IF (RMSP(IIK).EQ.99.999D0) THEN
CC                       NDEL=NDEL+1
CC                        DELNUM(NDEL)=SVNNUM(IIJ)
CC                        NEWWGT(IIJ)=0
CC                      ENDIF
                      GOTO 200
                    ENDIF
220               CONTINUE
C
C COMPUTE ACCURACY FROM RMS?
C --------------------------
                ELSE
                  NEWWGT(IIJ)=NINT(LOG(10d0*ORBRMS(IIL))/LOG(2d0))
                  IF (FROFIL) THEN
                    NEWWGT(IIJ)=MAX(NEWWGT(IIJ),MINACC,OLDWGT(IIJ))
                  ELSEIF (NEWWGT(IIJ).GT.0) THEN
                    NEWWGT(IIJ)=MAX(NEWWGT(IIJ),MINACC)
                  ENDIF
C
C CHECK IF BAD SATELLITES SHOULD BE REMOVED
C -----------------------------------------
CC                  IF (NEWWGT(IIJ).GT.MAXACC) THEN
CC                    NEWWGT(IIJ)=0
CC                    IF (FLGDEL.EQ.1) THEN
CC                      NDEL=NDEL+1
CC                      DELNUM(NDEL)=SVNNUM(IIJ)
CC                    ENDIF
CC                  ENDIF
                ENDIF
                GOTO 200
              ENDIF
210         CONTINUE
            WRITE(LFNERR,904) FFILES(1,ILOOP),SVNNUM(IIJ)
904         FORMAT(//,' ### PR PREWEI: SVN-NUMBER NOT IN LIST',
     1               /,16X,'PRECISE FILE NAME : ',A32,
     2               /,16X,'SVN-NUMBER: ',I4,//)
200       CONTINUE
C
C CHECK IF BAD SATELLITES SHOULD BE REMOVED AND SET MINIMUM VALUE
C ---------------------------------------------------------------
          NDEL=0
          DO IIJ=1,NSAT
C MINIMUM ACCURACY CODE
            IF (FROFIL.AND.NEWWGT(IIJ).NE.0) THEN
              NEWWGT(IIJ)=MAX(NEWWGT(IIJ),MINACC,OLDWGT(IIJ))
            ELSEIF (NEWWGT(IIJ).GT.0) THEN
              NEWWGT(IIJ)=MAX(NEWWGT(IIJ),MINACC)
            ELSEIF (NEWWGT(IIJ).EQ.-99) THEN
              NEWWGT(IIJ)=0
            ENDIF
C DELETE/FLAG/RERATE SATELLITES
            IF (NEWWGT(IIJ).GT.MAXACC) THEN
              IF (FLGDEL.EQ.3) THEN
                NEWWGT(IIJ)=MAXACC
              ELSEIF (FLGDEL.EQ.2) THEN
                NEWWGT(IIJ)=0
              ELSE
                NEWWGT(IIJ)=0
                NDEL=NDEL+1
                DELNUM(NDEL)=SVNNUM(IIJ)
              ENDIF
            ENDIF
          ENDDO
C
          GOTO 300
30      CONTINUE
C
C WRITE SOME INFORMATIVE OUTPUT
C -----------------------------
300     WRITE(LFNPRT,301) NSAT,LSAT,NDEL
301     FORMAT(2X,'  NUMB SAT:',I4,/,
     1         2X,'  NUMB OF LST.SAT:',I4,/,
     2         2X,'  NUMB OF DEL.SAT:',I4,/)
        WRITE(LFNPRT,302) (LSTNUM(I),I=1,LSAT)
        WRITE(LFNPRT,303)
        WRITE(LFNPRT,304) (ORBRMS(I),I=1,LSAT)
C
        WRITE(LFNPRT,302) (SVNNUM(I),I=1,NSAT)
        WRITE(LFNPRT,303)
        WRITE(LFNPRT,304) (NEWWGT(I),I=1,NSAT)
        WRITE(LFNPRT,304) (OLDWGT(I),I=1,NSAT)
302     FORMAT(/,1X,100(I3))
303     FORMAT(1X,79('-'))
304     FORMAT(1X,100(I3))
C
C CORRECT SATELLITE ARRAY IN CASE OF DELETED SATELLITES
C -----------------------------------------------------
        IF (NDEL.GT.0) THEN
          ISAT=0
          DO I=1,NSAT
            IDEL=0
            DO J=1,NDEL
              IF (SVNNUM(I).EQ.DELNUM(J)) IDEL=1
            ENDDO
            IF (IDEL.EQ.0) THEN
              ISAT=ISAT+1
              SVNNU1(ISAT)=SVNNUM(I)
              NEWWGT(ISAT)=NEWWGT(I)
              DELORD(I)=ISAT
            ELSE
              DELORD(I)=0
            ENDIF
          ENDDO
          NSAT1=ISAT
        ELSE
          NSAT1=NSAT
          DO I=1,NSAT1
            SVNNU1(I)=SVNNUM(I)
          ENDDO
        ENDIF
C
C OPEN PRE-FILE OR SCRATCH FILE (TO ALLOW SAME INPUT AND OUTPUT
C PRE-FILE NAME)
C -------------------------------------------------------------
        IF (FFILES(1,ILOOP).NE.FFILES(2,ILOOP)) THEN
          FILPRE=FFILES(2,ILOOP)
        ELSE
          FILPRE=FILSCR
        ENDIF
C
C WRITE PRECISE FILE HEADER
C -------------------------
        CALL WTPREH(FILPRE,LFNEPH,IFRMAT,NSAT1,SVNNU1,NEWWGT,
     1              TFIRST,NEPO,DTTAB,TITPRE,DATDES,COOSYS,ORBTYP,
     2              AGENCY,TIMSYS,BASPOS,BASCLK)
C
C LOOP OVER ALL EPOCHS OF PRECISE FILE
C ------------------------------------
400     CONTINUE
C
C       READ ONE EPOCH
C       --------------
        CALL RDPREI(LFNRES,IFRMAT,IREADE,NSAT,SVNNUM,TMJD,POS,VEL,
     1              DTSATC,DDTSAT,ACCPOS,ACCVEL,EVTFLG,IWRTE,SDEVP,
     2              SDEVV,CORRP,CORRV,IIRC)
        IF (IIRC.EQ.1) GOTO 410
C
C       CHECK IF SATELLITE IS DELETED
C       -----------------------------
        IF (NDEL.GT.0) THEN
          DO I=1,NSAT
            IF (DELORD(I).EQ.0) THEN
            ELSEIF (I.NE.DELORD(I)) THEN
              DO II=1,3
                POS(II,DELORD(I))=POS(II,I)
                VEL(II,DELORD(I))=VEL(II,DELORD(I))
              ENDDO
              DTSATC(DELORD(I))=DTSATC(I)
              DDTSAT(DELORD(I))=DDTSAT(I)
            ENDIF
          ENDDO
        ENDIF
C
C       WRITE ONE EPOCH
C       ---------------
        CALL WTPREI(LFNEPH,IFRMAT,IWRTE,NSAT1,SVNNU1,TMJD,POS,VEL,
     1              DTSATC,DDTSAT,ACCPOS,ACCVEL,EVTFLG,SDEVP,SDEVV,
     2              CORRP,CORRV,I2RC)
C
C       NEXT EPOCH
C       ----------
        GOTO 400
C
410     CONTINUE
C
C       CLOSE PRECISE INPUT AND OUTPUT FILE (OR SCRATCH FILE)
C       -----------------------------------------------------
        WRITE (LFNEPH,4000) 'EOF'
4000    FORMAT(A3)
C
        CLOSE (LFNRES)
        CLOSE (LFNEPH)
C
C IF NECESSARY, COPY SCRATCH FILE BACK TO INPUT PRECISE FILE
C ----------------------------------------------------------
        IF (FFILES(1,ILOOP).EQ.FFILES(2,ILOOP)) THEN
          CALL RDPREH(FILSCR,LFNRES,IFRMAT,NSAT1,SVNNU1,NEWWGT,
     1                TFIRST,NEPO,DTTAB,TITPRE,DATDES,COOSYS,ORBTYP,
     2                AGENCY,FILTYP,TIMSYS,BASPOS,BASCLK)
          CALL WTPREH(FFILES(2,ILOOP),LFNEPH,IFRMAT,NSAT1,SVNNU1,NEWWGT,
     1                TFIRST,NEPO,DTTAB,TITPRE,DATDES,COOSYS,ORBTYP,
     2                AGENCY,TIMSYS,BASPOS,BASCLK)
C
500       CONTINUE
C
C         READ ONE EPOCH
C         --------------
          CALL RDPREI(LFNRES,IFRMAT,IREADE,NSAT1,SVNNU1,TMJD,
     1                POS,VEL,DTSATC,DDTSAT,ACCPOS,ACCVEL,EVTFLG,IWRTE,
     2                SDEVP,SDEVV,CORRP,CORRV,IIRC)
          IF (IIRC.EQ.1) GOTO 510
C
C         WRITE ONE EPOCH
C         ---------------
          CALL WTPREI(LFNEPH,IFRMAT,IWRTE,NSAT1,SVNNU1,TMJD,
     1                POS,VEL,DTSATC,DDTSAT,ACCPOS,ACCVEL,EVTFLG,
     2                SDEVP,SDEVV,CORRP,CORRV,I2RC)
C
C         NEXT EPOCH
C         ----------
          GOTO 500
C
510       CONTINUE
C
C         WRITE END OF FILE RECORD AND CLOSE FILES
C         ----------------------------------------
          WRITE (LFNEPH,4000) 'EOF'
C
          CLOSE (LFNRES)
          CLOSE (LFNEPH)
        ENDIF
C
C       PREPARE SUMMARY-FILE FOR NEW READING
C       ------------------------------------
        REWIND (LFN001)
        IF (IFRMAT.EQ.0) THEN
          READ(LFN001,351)
351       FORMAT(////////)
        ELSE
          READ(LFN001,352)
352       FORMAT(////)
        ENDIF
C
C NEXT PRECISE FILE
C -----------------
100   CONTINUE
C
C END
C ---
      CLOSE(LFNLOC)
      CLOSE(LFN001)
C
      CALL EXITRC(0)
      END
