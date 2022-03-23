C*
      PROGRAM BRDTAB
CC
CC NAME       :  BRDTAB
CC
CC PURPOSE    :  GENERATE TABULAR EPHEMERIDES IN THE SYSTEM B1950.0 OR
CC               J2000.0 USING BROADCAST EPHEMERIDES AS INPUT
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC CREATED    :  87/10/19 08:41
CC
CC CHANGES    :  11-FEB-92 : WRITE EPOCH NUMBER WITH 5 DIGITS
CC               31-MAY-92 : OPTION J2000.0
CC               16-JUN-92 : OPNFIL USED
CC               09-SEP-93 : NEW TAB. INTERVAL OF 900 SEC; ALWAYS 4
CC                           POSITIONS COMPUTED FOR EACH BROADC.SET;
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               10-AUG-94 : MR: CALL EXITRC
CC               14-FEB-95 : RW: PRINT GENERAL FILE NAMES: PRFLNA
CC               05-JUN-96 : TS: CALL POLDEF CHANGED DUE TO SUBDAILY POLE
CC               21-JUN-96 : TS: DO NOT APPLY SUB-DAILY FOR BROADCAST ORBITS
CC               24-SEP-97 : DI: USE MAXSAT.inc
CC               02-NOV-00 : CU: SWITCH TO THE NEW MENU SYSTEM
CC               18-DEC-00 : HU: USE INTERFACE FOR PRFLNA
CC               25-SEP-02 : HU: REMOVE I_ASTLIB
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               03-MAR-03 : SC: ADD TITLE SECTION
CC               11-JUN-03 : HU: USE GSTIME
CC               14-JUN-03 : HU: DESACTIVATE USE OF GMST2000
CC               06-AUG-03 : HU: NEW TAB FORMAT, NEW CALL FOR GSTIME
CC               13-SEP-03 : HU: INTERFACE FOR RDNUTSUB
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: (COM)LFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               07-JUL-05 : HB: USE T_EPOCH FOR GSTIME
CC                               ADD BIAS TO PARAMETER LIST OF SR NUTEFF
CC               18-JUL-06 : AG: CMC IMPLEMENTED
CC               28-FEB-07 : AG: USE 206264... FROM DEFCON,
CC                               CALL DEFCON WITH PARAMETER
CC               28-MAR-07 : HB/HU: ADD 'BIAS' AS CHARACTERIZATION OF
CC                                  IERS2003 STANDARDS IN TAB-FILE
CC               20-SEP-08 : RD: ADD TIME WINDOW AND SAMPLING
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               06-DEC-10 : RD: CMC FOR ATL ADDED
CC               11-MAY-11 : HB: SET PRCMOD THROUGH D_MODEL
CC               14-NOV-11 : SL: USE M_BERN WITH ONLY, PRITIT CALL CHANGED
CC               24-NOV-11 : SL: NEW TITLE STRING FOR PRITIT
CC               04-MAY-12 : RD: USE DMOD FROM MODULE, REMOVE UNUSED MODULES
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnPrt, lfnErr, lfnOrb
      USE m_cpu,    ONLY: cpu_start
      USE m_time,   ONLY: t_timint
      USE d_const,  ONLY: ars
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE d_model,  ONLY: setModKey,chrValLength,mod_orb_prcMod
      USE m_maxdim, ONLY: maxsat,maxocn
      USE m_epoch,  ONLY: t_epoch, OPERATOR(.realToEpoch.)
      USE l_basfun, ONLY: dmod
      USE s_cmc,    ONLY: getcmc
      USE s_dmlmtv
      USE s_broadc
      USE s_opnfil
      USE s_pritit
      USE s_btinpt
      USE s_gtbrdc
      USE s_defcon
      USE s_opnsys
      USE s_gtflna
      USE s_prceff
      USE s_nuteff
      USE s_sidmat
      USE s_prflna
      USE s_poldef
      USE s_gtfile
      USE s_readinpf
      USE s_opnerr
      USE s_exitrc
      USE s_jmt
      USE s_radgms
      USE s_rdnutsub
      USE f_gpsmjd
      USE f_djul
      USE f_gstime

      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I0    , IBEG  , ID1   , ID2   , IDAY  , IEP   ,
     1          IEPH  , IFIL  , IH1   , IH2   , IORSYS, IOSTAT, IRC   ,
     2          IREC  , ISAT  , IWEEK0, J1    , J2    , JAHR  ,
     3          L0    , M1    , M2    , MAXEPH, MAXFIL, MI1   , MI2   ,
     4          MONTH , MXCEPH, MXCSAT, NEP   , NFIL  , NFLCOL, NSAT
C
      REAL*8    D1    , D2    , DAY   , DTAB  , DTEST , DTREC , EPH000,
     1          EQEQUI, ETEST , GPSUTC, SE1   , SE2   , SZ    ,
     2          TDT   , TEND  , TGPS  , TSEC  , TSECMD, TSTART, TUT1  ,
     3          TUTC  , UT1UTC, XH1   , XH2   , XM1   , XM2   , XMJD  ,
     4          XMJD0 , XPOLE , YPOLE
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C MAXIMAL DIMENSIONS
C ------------------
      PARAMETER (MAXEPH=100,MAXFIL=50)
C
C MAXSAT: MAXIMUM NUMBER OF SATELLITES
C MAXEPH: MAXIMUM NUMBER OF DIFFERENT NAVIGATION MESSAGES PER SATELLITE
C MAXFIL: MAXIMUM NUMBER OF FILES TO BE PROCESSED IN ONE RUN
C
C DECLARATIONS
C ------------
      TYPE(t_timint) :: timwin
      CHARACTER(LEN=chrValLength) :: chrVal
      CHARACTER(LEN=8)            :: pgName='BRDTAB  '

      CHARACTER*53 TITLE
      CHARACTER*32 FILTAB(2,MAXFIL)
      CHARACTER*7  ORBSYS(2)
      CHARACTER*16 NUTNAM,SUBNAM
      CHARACTER*6  MXNSAT,MXNEPH
      CHARACTER*4  BSNAM
      CHARACTER*1  VORZ
      INTEGER*4    NRSAT(MAXSAT),NEPH(MAXSAT)
      REAL*8       EPH(20*MAXEPH,MAXSAT),CLOCK(20*MAXEPH,MAXSAT)
      REAL*8       POS(3)
      REAL*8       SID(3,3),NUT(3,3),PRE(3,3),BIAS(3,3)
      REAL*8       numVal
      CHARACTER*32 tabfil(maxfil)
      REAL*8       cmc(3)              ! CMC offset
      CHARACTER*1  cmcchr(2)
      CHARACTER*16 cmcmod(2)           ! name of ocean loading file
      CHARACTER*20 harstr
      LOGICAL      cmcyn(2)
C
      TYPE(t_epoch) :: TTUT1,TTDT
C
C COMMON BLOCKS
C -------------
      COMMON/LARGE/EPH,CLOCK
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMEPH/MXCEPH,MXNEPH
      DATA ORBSYS/'B1950.0','J2000.0'/
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
C GET THE NAME OF THE INPUT FILE
C ------------------------------
      CALL init_inpkey(inpKey)
      CALL readinpf(' ',inpKey)
C
C DEFINE SYSTEM FILES
C -------------------
      CALL OPNSYS
C
C PRINT TITLE
C -----------
      CALL  pritit(pgName,
     1    'Create tabular orbits from broadcast ephemerides')
C
C DEFINE CONSTANTS
C ----------------
      CALL DEFCON(1)
C
C READ FILE NAMES
C ---------------
      NFLCOL=2
      CALL GTFILE('BRDFIL ',NFLCOL,MAXFIL,NFIL,FILTAB)
C
      CALL GTFLNA(0,'TABFIL',tabfil(1),irc)
      IF (tabfil(1) /= ' ') THEN
        DO i = 1, nfil
          filtab(2,i) = tabfil(i)
        END DO
      END IF
C
C PRINT GENERAL FILE NAMES
C ------------------------
      CALL prflna
C
C READ AND PRINT ALL INPUT FROM INPUT OPTION FILE
C -----------------------------------------------
      CALL BTINPT(IORSYS,cmcyn,DTREC,DTAB,TIMWIN)
C
      WRITE(LFNPRT,57)
57     FORMAT(' FILE  BROADCAST FILENAME             '
     1          ,         '   TABULAR ORBIT FILENAME            INTER'
     2         /,' ',4('-'),'  ',32('-'),'  ',32('-'),'  ',5('-'),/)
C
C GET NAME OF SUBDAILY AND NUTATION MODEL
C ---------------------------------------
      CALL RDNUTSUB(NUTNAM,SUBNAM)
      chrVal = ' '
      chrVal = 'BIAS'
      numVal= 0.D0
      CALL setModKey(mod_orb_prcMod,chrVal,pgName,numVal)
C
C LOOP OVER ALL FILES
C -------------------
      DO 1000 IFIL=1,NFIL
C
C GET EPHEMERIS PARAMETERS FROM EPHEMERIS FILE
C --------------------------------------------
        CALL GTBRDC(FILTAB(1,IFIL),TITLE,NSAT,NRSAT,NEPH,EPH,CLOCK)
C
C FIRST, LAST EPOCH FOR TABULAR ORBIT
C -----------------------------------
        IWEEK0=IDNINT(EPH(1,1))
        XMJD=GPSMJD(0.D0,IWEEK0)
        TSTART=1.D20
        TEND  =0.D0
        DO 20 ISAT=1,NSAT
          DO 10 IEPH=1,NEPH(ISAT)
            L0=20*(IEPH-1)
            EPH000=DINT(EPH(2+L0,ISAT)/3600.D0)*3600.D0
            ETEST=EPH000+(EPH(1+L0,ISAT)-IWEEK0)*604800.D0
            IF(ETEST.LT.TSTART) TSTART=ETEST
            IF(ETEST.GT.TEND) TEND=ETEST
10        CONTINUE
20      CONTINUE
C
C COMPUTE THE SAMPLING OF THE NAV-RECORDS
C ---------------------------------------
        IF (DTREC.EQ.1D20) THEN
          DO ISAT=1,NSAT
            DO IEPH=2,NEPH(ISAT)
              L0=20*(IEPH-1)
              DTEST=EPH(2+L0,ISAT)-EPH(2+L0-20,ISAT)
              DTEST=DTEST-(EPH(1+L0,ISAT)-EPH(1+L0-20,ISAT))*604800D0
              DTEST=NINT(DTEST/900d0)*900D0
              IF (DTREC.GT.DTEST) DTREC = DTEST
            ENDDO
          ENDDO
        ENDIF
        TEND=TEND+DTREC-DTAB
C
C WRITE FILENAMES
C ---------------
        IF (FILTAB(2,IFIL).EQ.' ') THEN
          FILTAB(2,IFIL)='  ---  '
        ENDIF
        WRITE(LFNPRT,56) IFIL,FILTAB(1,IFIL),FILTAB(2,IFIL),NINT(DTREC)
56      FORMAT(I5,2X,A32,2X,A32,2X,I5)
C
C NO PROCESS IF OUTPUT FILENAME DOES NOT EXIST
C --------------------------------------------
        IF (FILTAB(2,IFIL).EQ.'  ---  ') GOTO 1000
C
C OPEN TABULAR ORBIT FILE
C -----------------------
        CALL OPNFIL(LFNORB,FILTAB(2,IFIL),'UNKNOWN','FORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNORB,IOSTAT,FILTAB(2,IFIL),'BRDTAB')
C
C CONSIDER USER SPECIFIED TIME WINDOW
C -----------------------------------
        IF (TIMWIN%T(1).NE. 0D0 .AND.
     1      (TIMWIN%T(1)-XMJD)*86400d0.GT.TSTART) THEN
          TSTART = INT(((TIMWIN%T(1)-XMJD)*86400d0+DTAB-.5d0)/DTAB)*DTAB
        ENDIF
C
        IF (TIMWIN%T(2).NE.1D20 .AND.
     1      (TIMWIN%T(2)-XMJD)*86400d0.LT.TEND) THEN
          TEND = INT(((TIMWIN%T(2)-XMJD)*86400d0+0.5d0)/DTAB)*DTAB
        ENDIF
C
C READ RELEVANT POLE-INFORMATION
C ------------------------------
        TGPS=XMJD+(TSTART+TEND)/2/86400.D0
        CALL POLDEF(TGPS,0,XPOLE,YPOLE,UT1UTC,GPSUTC)
C
C WRITE TITLE LINE OF TABULAR ORBIT FILE
C --------------------------------------
        CALL JMT(TGPS,JAHR,MONTH,DAY)
        XMJD0=DJUL(JAHR-1,12,31.D0)
        IDAY=INT(TGPS-XMJD0)
        WRITE(LFNORB,2) JAHR,IDAY,ORBSYS(IORSYS)
2       FORMAT('TABULAR EPHEMERIS DERIVED FROM BROADCAST:  BR',
     1        I4,'.',I3,'  SYSTEM ',A)
C
C WRITE START AND END TIME
C ------------------------
        CALL JMT(XMJD+TSTART/86400.D0,J1,M1,D1)
        CALL JMT(XMJD+TEND/86400.D0,J2,M2,D2)
        CALL RADGMS(3,DMOD(XMJD+TSTART/86400.D0,1.D0),VORZ,IH1,MI1,SE1)
        CALL RADGMS(3,DMOD(XMJD+TEND/86400.D0,1.D0),VORZ,IH2,MI2,SE2)
        ID1=INT(D1)
        ID2=INT(D2)
        XH1=IH1
        XM1=MI1
        XH2=IH2
        XM2=MI2
! --------------------------------------
! IERS2003 system transformation applied
! --------------------------------------
        BSNAM='BIAS'
! --------------------------------------
        WRITE(LFNORB,3)NUTNAM,BSNAM,SUBNAM
3       FORMAT('CELESTIAL POLE OFFSET: ',A16,A4,5X,
     1         'SUBDAILY POLE MODEL: ',A16)
! --------------------------------------
! CMC for OTL and ATL
! --------------------------------------
        IF (cmcyn(1) .OR. cmcyn(2)) THEN
          CALL getcmc(cmcyn,cmcmod=cmcmod,harstr=harstr)
          cmcchr='N'
          IF (cmcyn(1)) THEN
            cmcchr(1)='Y'
          ELSE
            cmcmod(1)='NONE'
            harstr='----'
          ENDIF
          IF (cmcyn(2)) THEN
            cmcchr(2)='Y'
          ELSE
            cmcmod(2)='NONE'
          ENDIF
          WRITE(LFNORB,'(A,A16,A,A20,A,A1,5X,A,A16,A,A1)')
     1           'LOADING CMC - OTLOAD: ',cmcmod(1),' / ',harstr,
     2           ' - ',cmcchr(1),'ATLOAD: ',cmcmod(2),' - ',cmcchr(2)
        ENDIF
C
C WRITE THE FIRST EPOCH
C ---------------------
        WRITE(LFNORB,7)M1,ID1,J1,XH1,XM1,SE1,M2,ID2,J2,XH2,XM2,SE2
7       FORMAT(I4,2I5,3D20.12,/,I4,2I5,3D20.12)
C
C WRITE TABULAR INTERVAL AND POLE INFORMATION
C -------------------------------------------
        NEP=IDNINT((TEND-TSTART)/DTAB+1.D0)
        WRITE(LFNORB,4) DTAB,NEP,UT1UTC*86400,XPOLE*ARS,
     1                  YPOLE*ARS,NSAT
4       FORMAT(D19.12,I5,/,D24.12,2D25.12,////,I5)
C
C WRITE SATELLITE NUMBERS
C -----------------------
        DO 50 ISAT=1,NSAT
          WRITE(LFNORB,5) NRSAT(ISAT)
5         FORMAT('SVN    ',I4/)
50      CONTINUE
C
C LOOP OVER ALL TABULAR ORBIT EPOCHS
C ----------------------------------
        IREC=0
        DO 300 IEP=1,NEP
          TSEC=TSTART+(IEP-1)*DTAB
          TGPS=XMJD+TSEC/86400.D0
C
C POLE-INFORMATION (TIME ARGUMENT: UTC)
          TUTC=TGPS-GPSUTC
          CALL POLDEF(TUTC,1,XPOLE,YPOLE,UT1UTC,GPSUTC)
C
C PRECESSION AND NUTATION (TIME ARGUMENT: TDB, APPROX. AS TDT)
          TDT=TGPS+(19.D0+32.184D0)/86400.D0
          CALL PRCEFF(IORSYS,5.D0,TDT,PRE)
          CALL NUTEFF(IORSYS,0.1D0,TDT,NUT,EQEQUI,BIAS)
          PRE = matmul(PRE,BIAS)
C
C SIDERIAL TIME (TIME ARGUMENT: UT1)
          TUT1=TGPS-GPSUTC+UT1UTC
          TTUT1=.realToEpoch.TUT1
          TTDT =.realToEpoch.TDT
          SZ=GSTIME(0,TTUT1,TTDT,NUT(2,1),EQEQUI)
          CALL sidmat(tdt,xpole,ypole,sz,sid)
C
C
C CALCULATE CMC IF DESIRED
C ------------------------
          CALL getcmc(cmcyn,TUT1,cmc)
C
C LOOP OVER ALL SATELLITES
C ------------------------
          DO 290 ISAT=1,NSAT
            DO 80 I=1,3
              POS(I)=0.D0
80          CONTINUE
C
C SELECTION OF PROPER SET OF ELEMENTS
C -----------------------------------
            IEPH=0
            DO 90 I=1,NEPH(ISAT)
              I0=(I-1)*20+2
              EPH000=DINT(EPH(I0,ISAT)/3600.D0)*3600.D0
              ETEST=EPH000+(EPH(I0-1,ISAT)-IWEEK0)*604800.D0
              IF(TSEC.GE.ETEST.AND.TSEC.LT.ETEST+DTREC)
     1        IEPH=I
90          CONTINUE
C
C COMPUTE SATELLITE POSITION
            IF(IEPH.NE.0) THEN
              IBEG=20*(IEPH-1)+1
              TSECMD=TSEC-(EPH(IBEG,ISAT)-IWEEK0)*604800.D0
              CALL BROADC(TSECMD,EPH(IBEG,ISAT),POS)
C
C APPLY CMC IF DESIRED
              DO I=1,3
                POS(I)=POS(I)+cmc(I)
              ENDDO
C
              CALL DMLMTV(POS,SID,POS)
              CALL DMLMTV(POS,NUT,POS)
              CALL DMLMTV(POS,PRE,POS)
C
C POSITION IN KM
              DO 100 I=1,3
                POS(I)=POS(I)/1000
100           CONTINUE
            ENDIF
            IREC=IREC+1
C
C WRITE POSITION TO TABULAR ORBIT FILE
            IF(POS(1).NE.0.D0.OR.POS(2).NE.0.D0.OR.POS(3).NE.0.D0) THEN
              WRITE(LFNORB,6) IREC,(POS(I),I=1,3)
            END IF
6           FORMAT(I5,3D25.15)
290       CONTINUE
300     CONTINUE
C
C CLOSE TABULAR ORBIT FILE
C ------------------------
        CLOSE(UNIT=LFNORB)
C
1000  CONTINUE
C
      WRITE(LFNPRT,'(/)')
C
      CALL EXITRC(0)
      END
