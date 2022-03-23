C*
      PROGRAM RXNPRE
CC
CC NAME       :  RXNPRE
CC
CC PURPOSE    :  GENERATE A PRECISE ORBIT FILE (REMONDI FORMAT) FROM
CC               GPS AND GLONASS BROADCAST EPHEMERIDES FILES
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  H.HABRICH, M.ROTHACHER, G.BEUTLER
CC
CC CREATED    :  21-FEB-97
CC
CC CHANGES    :  13-MAY-97 : MR: REMOVE UNUSED VARIABLE "V"
CC               16-JUL-97 : MR: SR "RDPREI" RENAMED TO "BPINPT"
CC                4-AUG-97 : WG: INCLUDE LEAP INTO R2RDGH
CC               06-OCT-97 : HH: BUG IN SATELLIT CLOCK CORRECTION
CC               27-FEB-98 : MR: MAXEPH FROM 100 TO 500
CC               12-MAR-98 : DI: I/O SUBR. PRECISE FILES
CC               13-MAR-98 : WG: INCLUDE PRGNAM IN GTBRDG
CC               04-JAN-99 : TS: TAKE "LEAP" FROM GPSUTC FILE NOT FROM NAV.
CC               25-MAY-00 : DI: USE IDNINT INSTEAD OF IDINT TO COMPUTE DT
CC                               AND DNINT TO COMPUTE EPH000
CC               06-JUN-00 : DI: CALL BPINPT WITH ISYS
CC               03-APR-01 : RD: BPINPT->RNINPT BECAUSE NEW/OLD MENU
CC               04-APR-01 : DI: NEW MENU PROGRAM IMPLEMENTED
CC               16-FEB-02 : DI: ADD IEXSHI OPTION (EXCL. SHIFTET GLONASS SATs)
CC               25-SEP-02 : HU: REMOVE I_ASTLIB
CC               12-NOV-02 : HU: SP3C IMPLEMENTED
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               06-JAN-04 : HU: GPS NAV TABULAR INTERVAL IS 2 HOURS
CC               10-MAR-04 : HB: CHANGE ORDER OF MODULES
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: (COM)LFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               01-AUG-05 : HU: EPOCH AS STRUCTURE
CC               27-FEB-07 : AG: CALL DEFCON WITH PARAMETER
CC               09-AUG-10 : SL/RD: S_PRFILE ADDED, LFN00? AND FILTAB CHANGED,
CC                               GTBRDN AND BRDTS? CALLS CHANGED,
CC                               SOME INITIALIZATIONS
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               01-DEC-11 : SL: NEW TITLE STRING FOR PRITIT, M_BERN WITH ONLY
CC               24-JUN-13 : SL: CLOSE RINEX FILES AFTER READING
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnEph, lfnErr, lfn001
      USE m_cpu,    ONLY: cpu_start
      USE m_maxdim, ONLY: maxsat
      USE m_epoch,  ONLY: t_epoch, OPERATOR(.epochToReal.)
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE s_r2rdgh
      USE s_iordup
      USE s_broadc
      USE s_opnfil
      USE s_prflna
      USE s_mjdgps
      USE s_pritit
      USE s_prfile
      USE s_readinpf
      USE s_opnerr
      USE s_getdat
      USE s_wtpreh
      USE s_wtprei
      USE s_gtbrdg
      USE s_r2rdnh
      USE s_defcon
      USE s_exitrc
      USE s_rpinpt
      USE s_opnsys
      USE s_gtbrdn
      USE s_brdts1
      USE s_orbint
      USE s_brdts2
      USE f_dgpsut
      USE f_gpsmjd
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I      , IEPH   , IEPO   , IEXSHI , IFIL   , IFRMAT ,
     1          II     , IOSTAT , IRC    , IRCODE , IRXVRS , ISAT   ,
     2          ITUTC  , IWEEK  , IWEEK0 , J      , JSAT   , L0     ,
     3          LEAP   , LEAPTST, LFNPRE , LFNRXG , LFNRXN , MAXCOM ,
     4          MAXEPH , MAXFIL , MXCEPH , MXCSAT , NCOM   , NEPO   ,
     5          NFIL   , NGSANW , NGSAT  , NSANEW , NSAT   , NSTOT  ,
     6          NWKUTC , NGASAT ,NGASANEW, LFNRGA
C
      REAL*8    A0UTC  , A1UTC  , ACTEPO , AELL   , BASCLK , BASPOS ,
     1          BELL   , CORDAT , CORSYS , DELT   , DT     ,
     2          DTTAB  , EPH000 , EPOEPH , ETEST  , SCELL  ,
     3          SECEPO , TEPO   , TFIRST , TLAST  , TSEC   , TSEG   ,
     4          XMJD0  , FRST
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C MAXIMAL DIMENSIONS
C ------------------
      PARAMETER (MAXFIL=100,MAXCOM=50,MAXEPH=700)
C
C
C MAXSAT: MAXIMUM NUMBER OF SATELLITES
C MAXFIL: MAXIMUM NUMBER OF FILES TO BE PROCESSED IN ONE RUN
C MAXCOM: MAXIMUM NUMBER OF COMENT LINES IN RINEX FILE HEADER
C MACEPH: MAXIMUM NUMBER OF DIFFERENT NAVIGATION MESSAGES PER SATELLITE
C
C DECLARATIONS
C ------------
      REAL*8       EPH(20*MAXEPH,MAXSAT),CLOCK(20*MAXEPH,MAXSAT)
      REAL*8       GAEPH(20*MAXEPH,MAXSAT),GACLOCK(20*MAXEPH,MAXSAT)
      REAL*8       EPHNEW(20*MAXEPH,MAXSAT),CLKNEW(20*MAXEPH,MAXSAT)
      REAL*8       GAEPHNEW(20*MAXEPH,MAXSAT),GACLKNEW(20*MAXEPH,MAXSAT)
      REAL*8       GEPH(16*MAXEPH,MAXSAT),GEPHNW(16*MAXEPH,MAXSAT)
      REAL*8       DXELL(3),SRELL(3),ROT(3,3),XX(3)
      REAL*8       ALPHA(4),BETA(4)
      REAL*8       POS(3,MAXSAT),VEL(3,MAXSAT)
      REAL*8       DTSATC(MAXSAT),DDTSAT(MAXSAT)
      REAL*8       SDEVP(4,MAXSAT),SDEVV(4,MAXSAT)
      REAL*8       CORRP(6,MAXSAT),CORRV(6,MAXSAT)
C
      CHARACTER*60 COMENT(MAXCOM)
      CHARACTER*57 TITLE(4)
      CHARACTER*32 FILTAB(4,MAXFIL)
      CHARACTER*20 PRGNAM,RUNBY
      CHARACTER*9  CRDATE
      CHARACTER*6  MXNSAT,MXNEPH
      CHARACTER*5  COOSYS,DATDES,CRTIME
      CHARACTER*4  AGENCY
      CHARACTER*3  ORBTYP,TIMSYS
      CHARACTER*1  EVTFLG(4,MAXSAT)
C
      INTEGER*4    NRSAT(MAXSAT),NEPH(MAXSAT),ISATN(MAXSAT)
      INTEGER*4    NRGASAT(MAXSAT),NGAEPH(MAXSAT),IGASATN(MAXSAT)
      INTEGER*4    NRSNEW(MAXSAT),NEPNEW(MAXSAT)
      INTEGER*4    NRGASNEW(MAXSAT),NGAEPNEW(MAXSAT)
      INTEGER*4    NRGSAT(MAXSAT),NGEPH(MAXSAT),ISATG(MAXSAT)
      INTEGER*4    NRGSAN(MAXSAT),NGEPHN(MAXSAT)
      INTEGER*4    NRSTOT(MAXSAT),SATWGT(MAXSAT)
      INTEGER*4    ACCPOS(4,MAXSAT),ACCVEL(4,MAXSAT)
C
      TYPE(t_epoch) :: tmjd
C
C COMMON BLOCKS
C -------------
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMEPH/MXCEPH,MXNEPH
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C INITIALIZE COMMON BLOCKS FOR MAXIMAL DIMENSIONS
C -----------------------------------------------
      MXCSAT=MAXSAT
      MXNSAT='MAXSAT'
      MXCEPH=MAXEPH
      MXNEPH='MAXEPH'
C
C DEFINE LOGICAL FILE NUMBERS
C ---------------------------
      LFNRXN=LFN001
      LFNRXG=LFN001+1
      LFNRGA=LFN001+2
      LFNPRE=LFNEPH
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
C GET PZ - 90 DATUM
C -----------------
      CALL GETDAT('PZ - 90         ',AELL,BELL,DXELL,SRELL,SCELL)
C
C PRINT GENERAL TITLE
C -------------------
      CALL PRITIT ('RXNPRE',
     1          'Transfer RINEX navigation files to precise orbits')
C
C PRINT GENERAL FILE NAMES
C ------------------------
      CALL prflna
C
C READ INPUT OPTIONS
C ------------------
      CALL rpinpt(title,tfirst,tlast,dttab,coosys,agency,maxfil,
     1            nfil,filtab,iExShi,ifrmat)
C
      CALL PRFILE('PRT_RNXINP','PROCESSED BROADCAST FILES',4,132,
     1            NFIL,FILTAB)
C
C LOOP OVER ALL INPUT FILES
C -------------------------
      DO 1000 IFIL=1,NFIL
C
C INIT ARRAY
C ----------
        DO I=1,MAXSAT
          SATWGT(I)=0
        ENDDO
        NSAT=0
        NSANEW=0
        NGASAT=0
        NGASANEW=0
        NGSAT=0
        NGSANW=0
C
C NO PROCESS IF OUTPUT FILENAME DOES NOT EXIST
C --------------------------------------------
        DO II=1,4
          IF(FILTAB(II,IFIL).EQ.' ')THEN
            FILTAB(II,IFIL)=' --- '
          ENDIF
        ENDDO
        IF(FILTAB(4,IFIL).EQ.' --- ')GOTO 1000
C
C ARRAYS EPH AND CLOCKS FOR GPS EPHEMERIDES
C -----------------------------------------
        IF(FILTAB(1,IFIL).NE.' --- ')THEN
C
C READ GPS RINEX HEADER
C ---------------------
          IRCODE=0
          CALL OPNFIL(LFNRXN,FILTAB(1,IFIL),'OLD','FORMATTED',
     1                'READONLY',' ',IOSTAT)
          CALL OPNERR(LFNERR,LFNRXN,IOSTAT,FILTAB(1,IFIL),'RXNPRE')
          CALL R2RDNH(LFNRXN,LFNERR,MAXCOM,PRGNAM,RUNBY,CRDATE,CRTIME,
     1                NCOM,COMENT,ALPHA,BETA,A0UTC,A1UTC,ITUTC,NWKUTC,
     2                LEAP,IRXVRS,IRC)
          IRCODE=IRC
          IF(IRC.NE.0)GOTO 999
C
C READ GPS RINEX RECORDS
C ----------------------
          CALL GTBRDN(LFNRXN,LFNERR,0,NSAT,NRSAT,NEPH,EPH,CLOCK,IRXVRS)
          CLOSE(LFNRXN)
C
C TEST GPS EPHEMERIDES
C --------------------
          CALL BRDTS1(FILTAB(1,IFIL),NSAT,NRSAT,NEPH,EPH,CLOCK,
     1                NSANEW,NRSNEW,NEPNEW,EPHNEW,CLKNEW)
        ENDIF
C
C ARRAYS EPH AND CLOCKS FOR GALILEO EPHEMERIDES
C ---------------------------------------------
        IF(FILTAB(3,IFIL).NE.' --- ')THEN
C
C READ GALILEO RINEX HEADER
C -------------------------
          IRCODE=0
          CALL OPNFIL(LFNRGA,FILTAB(3,IFIL),'OLD','FORMATTED',
     1                'READONLY',' ',IOSTAT)
          CALL OPNERR(LFNERR,LFNRGA,IOSTAT,FILTAB(3,IFIL),'RXNPRE')
          CALL R2RDNH(LFNRGA,LFNERR,MAXCOM,PRGNAM,RUNBY,CRDATE,CRTIME,
     1                NCOM,COMENT,ALPHA,BETA,A0UTC,A1UTC,ITUTC,NWKUTC,
     2                LEAP,IRXVRS,IRC)
          IRCODE=IRC
          IF(IRC.NE.0)GOTO 999
C
C READ GALILEO RINEX RECORDS
C --------------------------
          CALL GTBRDN(LFNRGA,LFNERR,2,NGASAT,NRGASAT,NGAEPH,GAEPH,
     1                GACLOCK,IRXVRS)
          CLOSE(LFNRGA)
C
C TEST GALILEO EPHEMERIDES
C ------------------------
          CALL BRDTS1(FILTAB(3,IFIL),NGASAT,NRGASAT,NGAEPH,GAEPH,
     1                GACLOCK,NGASANEW,NRGASNEW,NGAEPNEW,GAEPHNEW,
     2                GACLKNEW)
       ENDIF
C
C ARRAY GEPH FOR GLONASS EPHEMERIDES
C ----------------------------------
        IF(FILTAB(2,IFIL).NE.' --- ')THEN
C
C READ GLONASS RINEX HEADER
C -------------------------
          IRCODE=0
          CALL OPNFIL(LFNRXG,FILTAB(2,IFIL),'OLD','FORMATTED',
     1                'READONLY',' ',IOSTAT)
          CALL OPNERR(LFNERR,LFNRXG,IOSTAT,FILTAB(2,IFIL),'RXNPRE')
          CALL R2RDGH(LFNRXG,LFNERR,MAXCOM,PRGNAM,RUNBY,CRDATE,CRTIME,
     1                NCOM,COMENT,CORSYS,CORDAT,LEAP,IRXVRS,IRC)
          IRCODE=IRC
          IF(IRC.NE.0)GOTO 999
C
C READ GLONASS RINEX RECORDS
C --------------------------
          CALL GTBRDG(LFNRXG,LFNERR,PRGNAM,NGSAT,NRGSAT,NGEPH,GEPH,
     1                IRXVRS)
          CLOSE(LFNRXG)
C
C TEST GLONASS EPHEMERIDES
C --------------------
          CALL BRDTS2(FILTAB(2,IFIL),NGSAT,NRGSAT,NGEPH,GEPH,IEXSHI,
     1                NGSANW,NRGSAN,NGEPHN,GEPHNW)
        ENDIF
C
C DEFINE GPS-UTC (DO NOT TRUST RINEX FILES)
C -----------------------------------------
        LEAPTST=DGPSUT(TFIRST)
        IF (LEAP.EQ.0 .OR. LEAPTST.NE.LEAP) LEAP=LEAPTST
        NEPO=IDINT((TLAST-TFIRST)/DTTAB*86400.D0)+1
C
C DEFINE ASCENDING ORDER FOR ALL SATELLITE
C ----------------------------------------
        CALL IORDUP(NRSNEW,NSANEW,ISATN)
        CALL IORDUP(NRGASNEW,NGASANEW,IGASATN)
        CALL IORDUP(NRGSAN,NGSANW,ISATG)
C
C WRITE HEADER OF PRECISE FILE
C ----------------------------
        NSTOT=NSANEW+NGASANEW+NGSANW
        DO ISAT=1,NSANEW
          NRSTOT(ISAT)=NRSNEW(ISATN(ISAT))
        ENDDO
        DO ISAT=1,NGASANEW
          NRSTOT(ISAT+NSANEW)=NRGASNEW(IGASATN(ISAT))
        ENDDO
        DO ISAT=1,NGSANW
          NRSTOT(ISAT+NGASANEW+NSANEW)=NRGSAN(ISATG(ISAT))
        ENDDO
C
        DATDES = 'U    '
        ORBTYP = 'BRD'
        TIMSYS = 'GPS'
C
        BASPOS = 1.25D0
        BASCLK = 1.025D0
        VEL    = 0D0
        ACCPOS = 0
        ACCVEL = 0
        EVTFLG = ' '
        SDEVP  = 0D0
        SDEVV  = 0D0
        CORRP  = 0D0
        CORRV  = 0D0
C
        CALL WTPREH(FILTAB(4,IFIL),LFNPRE,IFRMAT,NSTOT,NRSTOT,SATWGT,
     1                TFIRST,NEPO,DTTAB,TITLE,DATDES,COOSYS,ORBTYP,
     2                AGENCY,TIMSYS,BASPOS,BASCLK)
C
C LOOP FOR ALL EPOCHS
C -------------------
C         DO 8000 i = 1,4
C         write(*,*)"IFIL: ", i, iFil, filTab(i,iFil)
C8000       CONTINUE
        IWEEK0=IDNINT(EPHNEW(1,1))
        XMJD0=DNINT(GEPHNW(1,1))
C
        TMJD%DAY=INT(TFIRST)
        FRST=TFIRST-TMJD%DAY
        DO 500 IEPO=1,NEPO
          TMJD%FRAC=FRST+(IEPO-1)*DTTAB/86400.D0
          TEPO=.epochToReal.TMJD
          CALL MJDGPS(TEPO,SECEPO,IWEEK)
          TSEC=DNINT(SECEPO)+(IWEEK-IWEEK0)*604800.D0
          TSEG=DNINT((TEPO-XMJD0)*86400.D0)

C
C INITIALIZE ARRAYS (POSITION AND CLOCK)
C --------------------------------------
          DO I=1,MAXSAT
            DO II=1,3
              POS(II,I)=0.D0
            ENDDO
            DTSATC(I)=999999.999999D0
            DDTSAT(I)=999999.999999D0
          ENDDO
C
C LOOP FOR ALL GPS SATELLITES
C ----------------------------
          DO 300 ISAT=1,NSANEW
            JSAT=ISATN(ISAT)
C
C           SELECTION OF PROPER SET OF ELEMENTS
C           -----------------------------------
            IEPH=0
            DELT=2*3600D0
            DO II=NEPNEW(JSAT),1,-1
              L0=(II-1)*20
              EPH000=DNINT(EPHNEW(L0+2,JSAT)/DELT)*DELT
              ETEST=EPH000+(EPHNEW(L0+1,JSAT)-IWEEK0)*604800.D0
              IF(TSEC.GE.ETEST.AND.TSEC.LT.ETEST+DELT) IEPH=II
            ENDDO
C
C           POSITION FOUND
C           --------------
            IF (IEPH.GT.0) THEN
              L0=(IEPH-1)*20
              CALL BROADC(SECEPO,EPHNEW(L0+1,JSAT),POS(1,ISAT))
C
C             ACTUAL SATELLITE CLOCK CORRRECTION
C             ----------------------------------
              DT=TEPO-GPSMJD(CLKNEW(L0+11,JSAT),
     1                                    IDNINT(CLKNEW(L0+1,JSAT)))
              DT=DT*86400.D0
              DTSATC(ISAT)=CLKNEW(L0+14,JSAT)+CLKNEW(L0+13,JSAT)*DT
     1                                    +CLKNEW(L0+12,JSAT)*DT**2
C
C             WRITE POSITION AND CLOCK CORRECTION IN ARRAYS
C             ---------------------------------------------
            ENDIF
C
C NEXT GPS SATELLITE
C ------------------
300       CONTINUE
C
C
C LOOP FOR ALL GALILEO SATELLITES
C -------------------------------
          DO 600 ISAT=1,NGASANEW
            JSAT=IGASATN(ISAT)
C
C           SELECTION OF PROPER SET OF ELEMENTS
C           -----------------------------------
            IEPH=0
            DELT=2*3600D0
            DO II=NGAEPNEW(JSAT),1,-1
              L0=(II-1)*20
              EPH000=DNINT(GAEPHNEW(L0+2,JSAT)/DELT)*DELT
              ETEST=EPH000+(GAEPHNEW(L0+1,JSAT)-IWEEK0)*604800.D0
              IF(TSEC.GE.ETEST.AND.TSEC.LT.ETEST+DELT) IEPH=II
            ENDDO
C
C           POSITION FOUND
C           --------------
            IF (IEPH.GT.0) THEN
              L0=(IEPH-1)*20
              CALL BROADC(SECEPO,GAEPHNEW(L0+1,JSAT),POS(1,ISAT+NSANEW))
C
C             ACTUAL SATELLITE CLOCK CORRRECTION
C             ----------------------------------
              DT=TEPO-GPSMJD(GACLKNEW(L0+11,JSAT),
     1                                    IDNINT(GACLKNEW(L0+1,JSAT)))
              DT=DT*86400.D0
              DTSATC(ISAT+NSANEW)=GACLKNEW(L0+14,JSAT)+
     1                            GACLKNEW(L0+13,JSAT)*DT+
     2                            GACLKNEW(L0+12,JSAT)*DT**2
C
C             WRITE POSITION AND CLOCK CORRECTION IN ARRAYS
C             ---------------------------------------------
            ENDIF
C
C NEXT GALILEO SATELLITE
C ----------------------
600       CONTINUE
C
C
C LOOP FOR ALL GLONASS SATELLITES
C -------------------------------
          DO 400 ISAT=1,NGSANW
            JSAT=ISATG(ISAT)
C
C           SELECTION OF PROPER SET OF ELEMENTS
C           -----------------------------------
            IEPH=0
            DO II=1,NGEPHN(JSAT)
              L0=(II-1)*16
              ACTEPO=(GEPHNW(L0+1,JSAT)-XMJD0)*86400.D0
              ACTEPO=ACTEPO+LEAP
              ETEST=DINT(ACTEPO/1800.D0)*1800.D0
              IF(TSEG.GE.ETEST.AND.TSEG.LT.ETEST+1800.D0) IEPH=II
            ENDDO
C
C           POSITION FOUND
C           --------------
            IF(IEPH.GT.0) THEN
              L0=(IEPH-1)*16
C
C             CORRECTION TO GPS-TIME
C             ----------------------
              EPOEPH=GEPHNW(L0+1,JSAT)+LEAP/86400.D0
              CALL ORBINT(GEPHNW(L0+1,JSAT),TEPO,EPOEPH,LFNERR,
     1                    POS(1,ISAT+NGASANEW+NSANEW),
     2                    DTSATC(ISAT+NGASANEW+NSANEW),IRC)
              IF(IRC.NE.0)GOTO 999
C
C             TRANSFORM PZ-90 TO WGS84, SMALL ROTATIONS  ONLY
C             -----------------------------------------------
              ROT(1,1)=1.D0
              ROT(1,2)= SRELL(3)
              ROT(1,3)=-SRELL(2)
              ROT(2,1)=-SRELL(3)
              ROT(2,2)=1.D0
              ROT(2,3)= SRELL(1)
              ROT(3,1)= SRELL(2)
              ROT(3,2)=-SRELL(1)
              ROT(3,3)=1.D0
              DO I=1,3
                XX(I)=0.D0
                DO J=1,3
                  XX(I)=XX(I)+ROT(I,J)*POS(J,ISAT+NGASANEW+NSANEW)
                ENDDO
                POS(I,ISAT+NGASANEW+NSANEW)=XX(I)*SCELL+DXELL(I)
              ENDDO
            ENDIF
C
C NEXT GLONASS SATELLITE
C ----------------------
400       CONTINUE
C
C WRITE VALUES OF EPOCH "TEPO" INTO PRECISE FILE
C ----------------------------------------------
          CALL WTPREI(LFNPRE,IFRMAT,(/0,0/),NSTOT,NRSTOT,TMJD,
     1                POS,VEL,DTSATC,DDTSAT,ACCPOS,ACCVEL,EVTFLG,
     2                SDEVP,SDEVV,CORRP,CORRV,IRCODE)
C
C NEXT EPOCH
C ----------
500     CONTINUE
C
C WRITE END OF FILE
C -----------------
        WRITE(LFNPRE,'(A3)') 'EOF'
        CLOSE(LFNPRE)
C
C NEXT INPUT FILE
C ---------------
1000  CONTINUE

999   CALL EXITRC(IRCODE)
      END

