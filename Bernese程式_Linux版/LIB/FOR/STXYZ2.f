      MODULE s_STXYZ2
      CONTAINS

C*
      SUBROUTINE STXYZ2(NFIL,PREFIL,ITFIL,TB1,TB2,NMAN,SATMAN,
     1                  TIMMAN,NBAD,SATBAD,TIMBAD,NSAT,NAVNUM,
     2                  IRCTOT,SOURCE,CMCYN,INDTIM,NSAMPL)
CC
CC NAME       :  STXYZ2
CC
CC PURPOSE    :  READ ALL POSITIONS FROM PRECISE ORBIT FILES FOR
CC               A SPECIFIC ARC. TRANSFORM THEM INTO INERTIAL FRAME.
CC               SR FOR PGM ORBGEN. REPLACES SR SETXYZ
CC
CC PARAMETERS :
CC         IN :  NFIL  : NUMBER OF FILES IN THE PRECISE ORBIT FORMAT
CC               PREFIL(K,I),I=1,2,..,NFIL: ORBIT FILE        CH*32
CC                        NAMES FOR K=1, ERP-FILE NAMES
CC                        FOR K=2
CC               ITFIL  : FIRST FILE TO BE USED IN TABLE      I*4
CC               TB1, TB2: ARC BOUNDARIES (MJD)               R*8
CC               NMAN   : NUMBER OF MANOEUVRES IN CRX-FILE    I*4
CC               SATMAN : SATELLITE NUMBERS                   I*4
CC               TIMMAN : MANOEUVRE EPOCHS                    R*8
CC               NBAD   : NUMBER OF BAD TIME INTERVALS        I*4
CC               SATBAD : NUMBERS OF BAD SATELLITES           I*4
CC               TIMBAD : BAD TIME INTERVAL                   R*8
CC        OUT :  NSAT   : TOTAL NUMBER OF SATELLITES FOR ARC  I*4
CC               NAVNUM(I),I=1,2,..,NSAT: SVN-NUMBERS ASSO-   I*4
CC                        CIATED WITH THE INDIVIDUAL SATELLITES
CC               IRCTOT : RETURN CODE                         I*4
CC               SOURCE(K),K=1,2,..,10: ORBIT SOURCE          CH*1
CC               NSAMPL : SAMPLING OF THE PRE-FILES           R*8(2)
CC                        1: MIN./2: MAX. VALUE FROM ALL FILES
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  4.0  (SEP 96)
CC
CC CREATED    :  87/11/30 08:11
CC
CC CHANGES    :  23-JUN-92 : ??: SUBROUTINE "OPNFIL"
CC               07-JAN-93 : ??: ORDER TABULAR FILES IN TIME
CC               10-AUG-94 : MR: CALL EXITRC
CC               29-APR-97 : HH: CHECK FOR POS=0.D0
CC               11-AUG-97 : DI: USE RDPREH INSTEAD OF RDPHED
CC               12-AUG-97 : DI: USE RDPREI INSTEAD OF RDPREC
CC               10-MAY-99 : MR: WRITE "IFIL" TO AUX. FILE
CC               22-AUG-00 : HB: CHANGE CALL OF POLDF1 (ADD ISUBFL)
CC               29-JAN-01 : HU: USE UT1UTC FROM INDIV. POLE FILES
CC               19-FEB-01 : RD: USE ALSO GPSUTC FROM INDIV. POLE FILES
CC               02-NOV-02 : HU: NEW PARAMETERS FOR RDPREH AND RDPREI
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               11-JUN-03 : HU: USE GSTIME
CC               14-JUN-03 : HU: DESACTIVATE USE OF GMST2000
CC               05-AUG-03 : HU: CHECK FOR MANOEUVERS CORRECTED
CC               06-AUG-03 : HU: NEW CALL FOR GSTIME
CC               30-AUG-03 : HU: REMOVE BAD SATELLITES
CC               09-SEP-03 : HU: REMOVE BAD SATELLITES FROM LIST IF NO OBS
CC               01-OCT-03 : SS: BUGFIX CONCERNING EXCLUSION OF SATELLITES
CC               29-JUN-04 : HU/RD/SS: INCREMENT "IRCTOT" CORRECTLY
CC               21-MAY-05 : HU: MOSUPN REPLACED BY NUTEFF, PRCEFF
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               07-JUL-05 : HB: USE T_EPOCH FOR GSTIME
CC                               ADD BIAS TO PARAMETER LIST OF SR NUTEFF
CC               01-AUG-05 : HU: EPOCH AS STRUCTURE
CC               18-JUL-06 : AG: CMC IMPLEMENTED
CC               06-Aug-08 : DT: CHECK TIMSYS FOR PRE-FILE, CORRECT UTC->GPS
CC               29-SEP-08 : RD: PRE-FILES ARE SORTED IN RDORBI
CC                               (MAXFIL IS NOW OBSOLETE)
CC               14-Nov-08 : DT: ADD INDTIM TO PARAMETER LIST -> USE TIME
CC                               SYSTEM ACCORDING TO PANEL SETTINGS
CC               03-DEC-10 : RD: CMC FOR ATL ADDED
CC               24-FEB-11 : RD: REPORT SAMPLING OF PRE FILE
CC               05-MAR-12 : RD: USE MOSUPN AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE m_maxdim, ONLY: MAXSAT,maxocn
      USE m_epoch,  ONLY: t_epoch, OPERATOR(.epochToReal.)
      USE s_cmc,    ONLY: getcmc
      USE s_dmlmtv
      USE s_opnfil
      USE s_mosupn
      USE s_sidmat
      USE s_opnerr
      USE s_rdpreh
      USE s_rdprei
      USE f_gstime
      USE s_exitrc
      USE s_poldf1
      USE s_gtflna
      USE s_nuteff
      USE s_prceff
      USE f_dgpsut

      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IBAD  , IEPO  , IFIL  , IFRMAT, IMAN  ,
     1          IORSYS, IOSTAT, IRC   , IRC2  , IRCTOT, IREADE,
     2          ISAT  , ISBAD , ITCORR, ITFIL , JSAT  , K     , KSAT  ,
     3          LFNTMP, NBAD  , NEPO  , NFIL  , NMAN  ,
     4          NSAT  , NSAT1 , NSATNW, NSFIL , NUMBER
C
      REAL*8    DTTAB , EQEQUI, GPCORR, GPSOLD, GPSUTC, SZ    , T0ARC ,
     1          TB1   , TB2   , TDT   , TGPS  , TUT1  , UT1OLD,
     2          UT1UTC, UTCORR, XR    , YR    , tcorr
C
      LOGICAL   CMCYN(2)
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C
      INTEGER*4    NAVNUM(*),SATMAN(*),SATBAD(*)
      INTEGER*4    SVNNUM(MAXSAT),SATWGT(MAXSAT),SATOBS(MAXSAT)
      INTEGER*4    ACCPOS(4,MAXSAT),ACCVEL(4,MAXSAT),IEREC(2)
      INTEGER*4    indtim              ! time system from panel
C
      REAL*8       TIMMAN(*),TIMBAD(2,*)
      REAL*8       FROMTO(2),PRE(3,3),NUT(3,3),BIAS(3,3)
      REAL*8       SID(3,3),POS(3,MAXSAT)
      REAL*8       VEL(3,MAXSAT),DTSATC(MAXSAT),DDTSAT(MAXSAT)
      REAL*8       SDEVP(4,MAXSAT),SDEVV(4,MAXSAT)
      REAL*8       CORRP(6,MAXSAT),CORRV(6,MAXSAT)
      REAL*8       BASPOS,BASCLK
      REAL*8       NSAMPL(2)
      REAL*8       cmc(3)              ! CMC offset
C
      CHARACTER*57 TITLE(4)
      CHARACTER*32 PREFIL(2,*),FILNAM
      CHARACTER*5  COOSYS,DATDES
      CHARACTER*4  AGENCY
      CHARACTER*3  ORBTYP,TIMSYS
      CHARACTER*2  FILTYP
      CHARACTER*1  SOURCE(10)
      CHARACTER*1  EVTFLG(4,MAXSAT)

      CHARACTER*3  TimPan(2)
C
      TYPE(t_epoch) :: tmjd,TTUT1,TTDT
C
      COMMON/TORIGO/T0ARC
      COMMON/ORBSYS/IORSYS

      DATA TimPan/'GPS','UTC'/
C
C ORDER PRECISE ORBIT FILES ACCORDING TO START TIME
C -------------------------------------------------
      NSAMPL=0D0
      IORSYS=2
      NSAT=0
      SATOBS=0
      DO 10 IFIL=1,NFIL
C
C       READ PRECISE FILE HEADER
C       ------------------------
        LFNTMP=-1
        CALL RDPREH(PREFIL(1,IFIL),LFNTMP,IFRMAT,NSFIL,SVNNUM,SATWGT,
     1              FROMTO(1),NEPO,DTTAB,TITLE,DATDES,COOSYS,ORBTYP,
     2              AGENCY,FILTYP,TIMSYS,BASPOS,BASCLK)
        IF (NSAMPL(1).EQ.0d0.OR.NSAMPL(1).GT.DTTAB)NSAMPL(1)=DTTAB
        IF (NSAMPL(2).EQ.0d0.OR.NSAMPL(2).LT.DTTAB)NSAMPL(2)=DTTAB
C
C       STARTING/ENDING TIME FOR FILE
C       -----------------------------
        FROMTO(2)=FROMTO(1)+(NEPO-1)*DTTAB/86400.D0
C
C       BUILD UP SATELLITE LIST
C       -----------------------
        DO 5 KSAT=1,NSFIL
          DO ISAT=1,NSAT
            IF(SVNNUM(KSAT).EQ.NAVNUM(ISAT)) GOTO 5
          ENDDO
          NSAT=NSAT+1
          IF (NSAT.GT.MAXSAT) GOTO 901
          NAVNUM(NSAT)=SVNNUM(KSAT)
5       CONTINUE
C
C       ADD MANOEUVRE SATELLITES
C       ------------------------
        NSATNW=NSAT
        DO ISAT=1,NSAT
          DO IMAN=1,NMAN
            IF(NAVNUM(ISAT).EQ.SATMAN(IMAN))THEN
               IF((TIMMAN(IMAN).GE.TB1).AND.
     1            (TIMMAN(IMAN).LT.TB2))THEN
                NSATNW=NSATNW+1
                IF (NSATNW.GT.MAXSAT) GOTO 902
                NAVNUM(NSATNW)=NAVNUM(ISAT)+50
              END IF
            END IF
          ENDDO
        ENDDO
10    CONTINUE
      NSAT=NSATNW
C
C CORRECT STARTING TABULAR FILE INDEX
C -----------------------------------
      IRCTOT=0
      DO ITCORR=1,NFIL
        IF (ITFIL.EQ.ITCORR)  GOTO 21
      ENDDO
21    CONTINUE
C
C INITIALIZE ARC DEFINITION
C -------------------------
      IRCTOT=0
C
C OPEN AUXILIARY FILE FOR SAT POS OF ARC
C --------------------------------------
      CALL GTFLNA(1,'TABAUX ',FILNAM,IRC)
      CLOSE(LFN001)
      CALL OPNFIL(LFN001,FILNAM,'UNKNOWN','UNFORMATTED',
     1            ' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFN001,IOSTAT,FILNAM,'STXYZ2')
C
C READ PRECISE ORBIT FILES
C ------------------------
      UTCORR=0
      GPCORR=0
      IREADE=0                ! skip optional EP and EV records
      DO 40 IFIL=ITCORR,NFIL
C
        CALL RDPREH(PREFIL(1,IFIL),LFN002,IFRMAT,NSFIL,SVNNUM,SATWGT,
     1              FROMTO(1),NEPO,DTTAB,TITLE,DATDES,COOSYS,ORBTYP,
     2              AGENCY,FILTYP,TIMSYS,BASPOS,BASCLK)
C
C Correct for GPS-UTC if time system in PRE-file (timsys)
C is different from panel settings (indtim)
C -------------------------------------------------------
        tcorr = 0.0D0
        gpsutc = dgpsut(FROMTO(1))/86400.D0
        IF (timsys=='UTC'.OR.timsys=='GLO') THEN
          tcorr = gpsutc
        ELSEIF (timsys=='TAI') THEN
          tcorr = -19.D0/86400.D0
        ENDIF
C
        IF (indtim==2)  tcorr = tcorr - gpsutc
C
        FROMTO(1) = FROMTO(1) + tcorr
C
        IF (ABS(tcorr)>1.D0/86400.D0) THEN
          WRITE(LFNERR,100) TimPan(indtim),timsys,tcorr*86400.D0
100       FORMAT(/,' ### SR STXYZ2 : Time system of PRE file is',/,
     1             17X,'different from panel settings.',/,
     2             17X,'PRE file: ',A3,/,
     3             17X,'Panel:    ',A3,/,
     4             17X,'Correction = ',F5.1,' sec',/)
        END IF
C
        FROMTO(2)=FROMTO(1)+(NEPO-1)*DTTAB/86400.D0
        IF((FROMTO(1).LT.TB1.AND.FROMTO(2).LT.TB1).OR.
     1     (FROMTO(1).GT.TB2.AND.FROMTO(2).GT.TB2)) GOTO 35
C
C READ THE SATELLITE POSITIONS IN REQUIRED TIME INTERVAL FROM
C TABULAR ORBIT FILES .
C -----------------------------------------------------------
        DO 30 IEPO=1,NEPO
C
          CALL RDPREI(LFN002,IFRMAT,IREADE,NSFIL,SVNNUM,TMJD,
     1                POS,VEL,DTSATC,DDTSAT,ACCPOS,ACCVEL,EVTFLG,
     2                IEREC,SDEVP,SDEVV,CORRP,CORRV,IRC2)
C
C Correct for GPS-UTC if PRE-file is different from panel settings
C ----------------------------------------------------------------
          IF(tcorr/=0.0D0) THEN
            tmjd%frac = tmjd%frac + tcorr
          ENDIF
C
          TGPS=.epochToReal.TMJD
          TTUT1 = tmjd
          TTDT  = tmjd
C
          IF(IRC2.NE.0) THEN
            IF(IRC2.EQ.1) THEN
              GOTO 35
            ELSE
              GOTO 990
            ENDIF
          ENDIF
C
C EPOCH WITHIN REQUESTED ARC ?
C ----------------------------
          IF (TGPS.LT.TB1 .OR. TGPS.GT.TB2) GOTO 30
C
C UT-CORRECTION
C -------------
          IF(IFIL.GT.ITCORR.AND.IEPO.EQ.1)THEN
            CALL POLDF1(PREFIL(2,IFIL-1),TGPS,1,XR,YR,UT1OLD,GPSOLD)
            CALL POLDF1(PREFIL(2,IFIL),TGPS,1,XR,YR,UT1UTC,GPSUTC)
            UTCORR=UTCORR+UT1OLD-UT1UTC
            GPCORR=GPCORR+GPSOLD-GPSUTC
          END IF
C
C GET POLE POSITION, COMPUTE POLAR WOBBLE MATRIX
C ----------------------------------------------
          CALL POLDF1(PREFIL(2,IFIL),TGPS,1,XR,YR,UT1UTC,GPSUTC)
          UT1UTC=UT1UTC+UTCORR
          GPSUTC=GPSUTC+GPCORR
C
C PRECESSION AND NUTATION (TIME ARGUMENT: TDB, APPROX. AS TDT)
C ------------------------------------------------------------
          TDT=TGPS+(19.D0+32.184D0)/86400.D0
          CALL PRCEFF(2,5.D0,TDT,PRE)
          CALL NUTEFF(2,0.1D0,TDT,NUT,EQEQUI,BIAS)
          PRE = matmul(PRE,BIAS)
C
C TRUE SIDERIAL TIME
C ------------------
          TUT1=TGPS-GPSUTC+UT1UTC
          TTUT1%frac = ttut1%frac - GPSUTC + UT1UTC
          TTDT%frac  = ttdt%frac + (19.D0+32.184D0)/86400.D0
          SZ=GSTIME(0,TTUT1,TTDT,NUT(2,1),EQEQUI)
C
C CALCULATE CMC IF DESIRED
C ------------------------
          CALL getcmc(cmcyn,TUT1,cmc)
C
C PRODUCT OF TRANSPOSE OF POLAR MOTION MATRIX AND THE SIDEREAL TIME
C MATRIX
C -----------------------------------------------------------------
          CALL sidmat(tdt,xr,yr,sz,sid)
          DO 29 ISAT=1,NSFIL
C
C           SKIP SATELLITES WITH POSITION 0.D0
C           ----------------------------------
            IF (POS(1,ISAT).EQ.0.D0 .AND.
     1          POS(2,ISAT).EQ.0.D0 .AND.
     2          POS(3,ISAT).EQ.0.D0)       GOTO 29
C
C           APPLY CMC IF DESIRED
C           --------------------
            DO K=1,3
              POS(K,ISAT)=POS(K,ISAT)+cmc(K)
            ENDDO
C
            CALL DMLMTV(POS(1,ISAT),SID,POS(1,ISAT))
            CALL DMLMTV(POS(1,ISAT),NUT,POS(1,ISAT))
            CALL DMLMTV(POS(1,ISAT),PRE,POS(1,ISAT))
C
C           SATELLITE EXCLUDED IN SATCRUX
C           -----------------------------
            ISBAD=0
            DO IBAD=1,NBAD
              IF(SVNNUM(ISAT).EQ.SATBAD(IBAD))THEN
                IF((TGPS.GE.TIMBAD(1,IBAD)).AND.
     1             (TGPS.LE.TIMBAD(2,IBAD)))THEN
                  ISBAD=1
                END IF
              END IF
            ENDDO
            IF (ISBAD.EQ.1) GOTO 29
C
C           MANOEUVRE WITHIN FILE FOR THIS SATELLITE?
C           -----------------------------------------
            NUMBER=SVNNUM(ISAT)
            DO IMAN=1,NMAN
              IF(SVNNUM(ISAT).EQ.SATMAN(IMAN))THEN
                IF((TIMMAN(IMAN).GE.TB1).AND.
     1             (TIMMAN(IMAN).LT.TB2))THEN
                  IF(TGPS.GT.TIMMAN(IMAN))
     1             NUMBER=NUMBER+50
                END IF
              END IF
            ENDDO
C
            DO KSAT=1,NSAT
              IF (NUMBER.EQ.NAVNUM(KSAT)) SATOBS(KSAT)=1
            ENDDO
C
            IRCTOT=IRCTOT+1
            WRITE(LFN001) IFIL,NUMBER,TGPS-T0ARC,(POS(K,ISAT),K=1,3)
29        CONTINUE
30      CONTINUE
35      CLOSE(UNIT=LFN002)
40    CONTINUE
41    CONTINUE
C
C REMOVE SATELLITES THAT ARE BAD DURING ENTIRE INTERVAL
C -----------------------------------------------------
      NSAT1=NSAT
      DO ISAT=NSAT1,1,-1
        IF (SATOBS(ISAT).EQ.0) THEN
          NSAT=NSAT-1
          DO JSAT=ISAT,NSAT
            NAVNUM(JSAT)=NAVNUM(JSAT+1)
          ENDDO
        ENDIF
      ENDDO
C
      SOURCE(1)='P'
      SOURCE(2)='R'
      SOURCE(3)='E'
      SOURCE(4)='C'
      SOURCE(5)='I'
      SOURCE(6)='S'
      SOURCE(7)='E'
      SOURCE(8)=' '
      SOURCE(9)=' '
      SOURCE(10)=' '
C
      GOTO 999
C
C ERROR: NUMBER OF SV'S > MAXIMUM
C -------------------------------
901   WRITE(LFNERR,9001) NSAT,MAXSAT
9001  FORMAT(/,' *** SR STXYZ2 : NUMBER OF TOTAL SAT. FOR ARC >
     1       MAXIMUM NUMBER ',/,
     2       17X,'NUMBER OF SATELLITES : ',I3 ,/,
     3       17X,'MAXIMUM NUMBER OF SAT. ALLOWED : ',I3,/)
      GOTO 990
C
902   WRITE(LFNERR,9002) NSATNW,MAXSAT
9002  FORMAT(/,' *** SR STXYZ2 : NUMBER OF MANOEUVRE SAT. >
     1       MAXIMUM NUMBER ',/,
     2       17X,'NUMBER OF MANOEUVRE SAT. : ',I3 ,/,
     3       17X,'MAXIMUM NUMBER OF SAT. ALLOWED : ',I3,/)
C
C     END IF ERROR OCCURED
C     --------------------
990   CALL EXITRC(2)
C
C END OF SUBROUTINE
C -----------------
999   RETURN
      END SUBROUTINE

      END MODULE
