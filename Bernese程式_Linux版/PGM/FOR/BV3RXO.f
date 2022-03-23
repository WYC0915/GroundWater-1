C*
      PROGRAM BV3RXO
CC
CC NAME       :  BV3RXO
CC
CC PURPOSE    :  TRANSFORM BERNESE GPS SOFTWARE VERSION 3 OBSERVATION
CC               FILES (CODE AND/OR PHASE) INTO RINEX FORMAT
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  W.GURTNER
CC
CC CREATED    :  89/04/07 12:16
CC
CC CHANGES    :  09-DEC-92 : ??: NEW COMMON MCMSTA
CC               22-DEC-92 : ??: USE OF SR "OPNFIL" TO OPEN FILES
CC               22-FEB-93 : ??: ERROR IF NO OBS OF AN OBS.TYPE AT ALL
CC               06-AUG-93 : ??: NEW FORMAT, VERSION 3.5
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               12-AUG-94 : MR: CALL EXITRC
CC               12-AUG-94 : MR: FORMAT 4: SESSION AS CHARACTER*4
CC               24-SEP-97 : DI: USE MAXSAT.inc
CC               30-JUN-00 : RD: INCREASE MAXAMB (OLD=600)
CC               29-OCT-01 : DI: SWITCH TO NEW MENU SYSTEM
CC               17-DEC-01 : MM: USE NEW RECEIVER FILE FORMAT (ANTECC REMOVED)
CC                               PRINT P2 INSTEAD OF C2=C1+(P2-P1)
CC               22-DEC-01 : HU: INTERFACE TO PRFLNA ADDED
CC               07-MAY-02 : SS: DCB UPDATE
CC               06-JUN-02 : HU: WRITE RINEX FORMAT 2
CC               06-AUG-02 : SS: HANDLE ICODER=3
CC               25-SEP-02 : HU: REMOVE I_ASTLIB
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               08-SEP-03 : HU: ANTNAM, RECNAM, OPRNAM CHR16 -> CHR20
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               10-MAR-04 : HB: CHANGE ORDER OF MODULES
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: (COM)LFNUM.inc REMOVED, m_bern ADDED
CC                               VARIABLE CHAR REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               27-FEB-07 : AG: CALL DEFCON WITH PARAMETER
CC               21-JUN-07 : SS: CALL GETRCV WITH ISYST
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               18-Jan-11 : SL: MAXTYP FROM D_RINEX3
CC               19-JAN-11 : RD: USE GETSTA
CC               26-JAN-11 : LP: CALL TO RDHEAD CHANGED
CC               17-FEB-11 : RD: COMMON MCMSTA NOT NEEDED ANYMORE
CC               24-NOV-11 : SL: NEW TITLE STRING FOR PRITIT, M_BERN WITH ONLY
CC               29-FEB-12 : RD: USE R2WTOR AS MODULE
CC               29-FEB-12 : RD: USE R2WTOH AS MODULE
CC               28-MAR-12 : RD: USE SVNSYS AS MODULE
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1989     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,    ONLY: lfnPrt, lfnErr, lfnLoc, lfn001, lfn002
      USE m_cpu,     ONLY: cpu_start
      USE m_maxdim,  ONLY: maxsat
      USE d_inpkey,  ONLY: inpKey, init_inpkey
      USE d_const,   ONLY: DATE, TIME, WLGTH
      USE d_rinex3,  ONLY: maxtyp
C
      USE s_rdobsi
      USE s_opnfil
      USE s_prflna
      USE s_pritit
      USE s_r2wtoh
      USE s_getrcv
      USE s_readinpf
      USE s_opnerr
      USE s_r2wtor
      USE s_rdhead
      USE s_getsta
      USE s_defcon
      USE s_exitrc
      USE s_b3rxoi
      USE s_opnsys
      USE f_tstflg
      USE f_svnsys

      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 ICLASS, IFIL  , IFLAG , IFLGCO, IFREQ , IFRMAT, IFRQ  ,
     1          IOSTAT, IRC   , IRCODE, IRDFLG, IREF  , IRMARK, IRXVRS,
     2          ISARNX, ISAT  , ISATEL, ISATEP, ITIMDI, ITYP  , K     ,
     3          KFIL  , LFNRNX, MAXAMB, MAXCOM, MAXFIL, MAXSTA,
     4          MEATYP, MXCAMB, MXCFIL, MXCSAT, NCENTR, NCOL  ,
     5          NCOM  , NCOMNT, NDIFF , NEPFLG, NEPREC, NFLINP, NFREQR,
     6          NSARNX, NSATEL, NSATEP, NSTAT , NUMAMB, NUMTYP, NWLSAT,
     7          ISYST
C
      REAL*8    AELL  , BELL  , EPSTIM, SCELL , TFIRST, TLAST
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C MAXIMAL DIMENSIONS
C ------------------
      PARAMETER (MAXAMB=900,MAXFIL=100)
      PARAMETER (MAXCOM= 20, MAXSTA= 2)
C
C
C MAXSAT: MAXIMUM NUMBER OF SATELLITES
C MAXAMB: MAXIMUM NUMBER OF AMBIGUITIES IN A FILE
C MAXFIL: MAXIMUM NUMBER OF FILES TO BE PROCESSED
C MAXTYP: MAXIMUM NUMBE ROF DIFFERENT OBSERVATION TYPES
C MAXCOM: MAXIMUM NUMBER OF COMMENT LINES IN RINEX HEADER
C
C DECLARATIONS
C ------------
C
C RDHEAD
      CHARACTER*53  TITLE
      CHARACTER*32  FILNAM(5,MAXFIL),HEADER(2),OBSFIL(2),RNXFIL,RNXOLD
      CHARACTER*32  FILPRT(5)
      CHARACTER*16  CAMPGN,STANAM(2)
      CHARACTER*20  RECTYP(2),OPRNAM(2),ANTTYP(2)
      CHARACTER*4   CSESS(2)
      CHARACTER     CRDATE(2)*9,CRTIME(2)*5
CCC      CHARACTER     CHAR
      REAL*8        POSECC(3,2),AMBIGU(MAXAMB,3),TIMREF(2)
      INTEGER*4     ICLOCK(2)
      INTEGER*4     NUMSAT(MAXSAT),NUMOBS(MAXSAT,2)
      INTEGER*4     NUMMRK(MAXSAT,2),NFREQ(2)
      INTEGER*4     AMBIEP(MAXAMB),AMBSAT(MAXAMB),AMBCLS(MAXAMB,3)
      INTEGER*4     AMBWLF(MAXAMB,2)
      INTEGER*4     IRUNIT(2),IANTEN(2),IWLSAT(3,1)
      INTEGER*4     NEPOCH(2),IDELTT(2),READGEOS
C
C RDOBSI
      INTEGER*4     IFRQS(2),NRSAT(MAXSAT,2),NSAT(2),IRETRN(2)
      REAL*8        OBSTIM(2),OBSERV(MAXSAT,2,2),DELTAT(2,2)
      CHARACTER*1   OBSFLG(MAXSAT,2,2),EPOFLG(2)
C
C GETSTA
      REAL*8       XSTELL(3,MAXSTA),XSTECC(3,MAXSTA),DXELL(3),DRELL(3)
      INTEGER*4    STANUM(MAXSTA),ICENTR(MAXSTA)
      CHARACTER    DATUM*16
C
C R2WTOH
      CHARACTER    STANAX*60,AGENCY*40
      CHARACTER*20 PRGNAM,RUNBY,RCVERS,ANTTYX,RECTYX,OPRNAX,MARKER
      CHARACTER    COMENT(MAXCOM)*60
      CHARACTER    OBSTYP(MAXTYP)*2
      REAL*8       POSXYZ(3,MAXSTA)
      INTEGER*4    SATRNX(MAXSAT),NUMOBX(MAXSAT,MAXTYP)
C
C RXWTOR
      INTEGER*4    SATEPO(MAXSAT),LLI(MAXSAT,MAXTYP)
      INTEGER*4    ISNMIN(MAXTYP),ISNTHR(MAXTYP),ISNMAX(MAXTYP)
      REAL*8       SIGNAL(MAXSAT,MAXTYP),OBSEPO(MAXSAT,MAXTYP)
      REAL*8       EPOCHX(2)
C
C GETRCV
      INTEGER*4    IWLFCR(2),ICODER(2)
C
C LOCAL DECLARATIONS
      INTEGER*4    LFNOBS(2),KFLAG(2),REFOBS(2,2)
C
      CHARACTER*6  MXNSAT,MXNAMB,MXNFIL
C
C COMMON BLOCKS
C -------------
      COMMON /LARGE/ FILNAM,AMBIGU,NUMSAT,NUMOBS,NUMMRK,
     1               AMBIEP,OBSERV,OBSFLG,COMENT,NUMOBX,
     2               SATEPO,LLI,SIGNAL,OBSEPO,AMBSAT,
     3               AMBWLF,AMBCLS
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMAMB/MXCAMB,MXNAMB
      COMMON/MCMFIL/MXCFIL,MXNFIL
C
      DATA ISNTHR/MAXTYP*5/,ISNMAX/MAXTYP*9/,ISNMIN/MAXTYP*1/
C
C PROGRAM NAME
C ------------
      DATA PRGNAM/'BV3RXO'/
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C INFINITESIMAL TIME INTERVAL (DAYS)
      EPSTIM=1.D-9
C
C NO SATELLITE SPECIFIC WAVELENGTH FACTORS
      NWLSAT=0
C
C INITIALIZE COMMON BLOCKS FOR MAXIMAL DIMENSIONS
C -----------------------------------------------
      MXCSAT=MAXSAT
      MXNSAT='MAXSAT'
      MXCAMB=MAXAMB
      MXNAMB='MAXAMB'
      MXCFIL=MAXFIL
      MXNFIL='MAXFIL'
C
C DEFINE LOGICAL FILE NUMBERS
C ---------------------------
      LFNOBS(1)=LFN001
      LFNOBS(2)=LFN001+1
      LFNRNX=LFN002
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
      IFRQS(1)=1
      IFRQS(2)=2
      READGEOS=0
      EPOCHX=0d0
C
C PRINT GENERAL TITLE
C -------------------
      CALL pritit('BV3RXO',
     1   'Transfer Bernese observation files to RINEX')
C
C PRINT GENERAL FILE NAMES
C ------------------------
      NCOL=79
      CALL PRFLNA(NCOL)
C
      WRITE(LFNPRT,11)
11    FORMAT(//,' FILE  BERNESE CODE  HEADER  FILE NAME ',
     1               '  RINEX FILE NAME',19X,'#EPO',
     2        /,'       BERNESE CODE  OBSERV. FILE NAME',
     3        /,'       BERNESE PHASE HEADER  FILE NAME',
     4        /,'       BERNESE PHASE OBSERV. FILE NAME',
     5        /,' ----  ',32('-'),2X,32('-'),2X,'----',/)
C
C READ INPUT OPTION
C -----------------
      CALL B3RXOI(MAXCOM,NCOM,COMENT,IFLGCO,RUNBY,MAXFIL,NFLINP,FILNAM)
C
      IRCODE=0
C
C LOOP OVER ALL FILES
C -------------------
      DO 300 IFIL=1,NFLINP
C
        IRDFLG = 0
C
        HEADER(1)=FILNAM(1,IFIL)
        HEADER(2)=FILNAM(3,IFIL)
        OBSFIL(1)=FILNAM(2,IFIL)
        OBSFIL(2)=FILNAM(4,IFIL)
        RNXFIL   =FILNAM(5,IFIL)
        IF(RNXFIL.EQ.' ') GOTO 290
C
C OPEN RINEX OUTPUT FILE
C ----------------------
        IF(IFIL.EQ.1) THEN
          CALL OPNFIL(LFNRNX,RNXFIL,'UNKNOWN','FORMATTED',
     1                ' ',' ',IOSTAT)
          CALL OPNERR(LFNERR,LFNRNX,IOSTAT,RNXFIL,'BV3RXO')
        ELSEIF(RNXFIL.NE.RNXOLD) THEN
          CLOSE(UNIT=LFNRNX)
          CALL OPNFIL(LFNRNX,RNXFIL,'UNKNOWN','FORMATTED',
     1                ' ',' ',IOSTAT)
          CALL OPNERR(LFNERR,LFNRNX,IOSTAT,RNXFIL,'BV3RXO')
        END IF
C
C INITIALIZE NUMBER OF OBSERVATIONS OF DIFFERENT OBS.TYPES
C --------------------------------------------------------
        DO 20 ITYP=1,MAXTYP
          DO 10 ISAT=1,MAXSAT
            NUMOBX(ISAT,ITYP)=0
10        CONTINUE
20      CONTINUE
C
C INITIALIZE WAVELENGTH FACTORS
C -----------------------------
        IWLFCR(1)=0
        IWLFCR(2)=0
C
C K=1: CODE,  K=2: PHASE
C ----------------------
        NUMTYP=0
        NSARNX=0
        DO 310 K=1,2
C
          IF(HEADER(K).NE.' ') THEN
C
C OPEN AND READ HEADER FILES
C --------------------------
C
            CALL RDHEAD(HEADER(K),MEATYP,NDIFF,NFREQ(K),NEPOCH(K),
     1                  NSATEL,CSESS,IDELTT(K),TIMREF(K),
     2                  CAMPGN,TITLE,CRDATE,CRTIME,IRMARK,NEPFLG,IFRMAT,
     3                  STANAM,RECTYP,ANTTYP,IRUNIT,IANTEN,
     4                  OPRNAM,POSECC,ICLOCK,NUMSAT,NUMOBS,NUMMRK,
     5                  NUMAMB,AMBSAT,AMBIEP,AMBWLF,AMBIGU,AMBCLS,
     6                  READGEOS)
C
C GET INFORMATION ABOUT RECEIVER TYPE FROM RECEIVER FILE
            CALL GETRCV(RECTYP(1),NFREQR,ICODER,IWLFCR,ICLASS,ISYST)
C
C CREATE LIST OF OBSERVATION TYPES
            DO 320 IFREQ=1,NFREQ(K)
              REFOBS(K,IFREQ)=0
              DO 330 ISATEL=1,NSATEL
                IF(NUMOBS(ISATEL,IFREQ).GT.0) GOTO 340
330           CONTINUE
              GOTO 320
340           NUMTYP=NUMTYP+1
              REFOBS(K,IFREQ)=NUMTYP
              IF(K.EQ.1) THEN
                IF(ICODER(IFREQ).EQ.2) THEN
                  WRITE(OBSTYP(NUMTYP),351) 'C',IFREQ
                ELSE
                  WRITE(OBSTYP(NUMTYP),351) 'P',IFREQ
                END IF
              ELSE
                WRITE(OBSTYP(NUMTYP),351) 'L',IFREQ
351             FORMAT(A1,I1)
              END IF
C
C CREATE LIST OF ALL SATELLITES WITH GOOD OBSERVATIONS AND
C KEEP NUMBER OF OBSERVATIONS FOR EACH OBS.TYPE
              DO 360 ISATEL=1,NSATEL
                IF(NUMOBS(ISATEL,IFREQ).GT.0) THEN
                  DO 355 ISARNX=1,NSARNX
                    IF(SATRNX(ISARNX).EQ.NUMSAT(ISATEL)) GOTO 357
355               CONTINUE
                  NSARNX=NSARNX+1
                  SATRNX(NSARNX)=NUMSAT(ISATEL)
                  ISARNX=NSARNX
357               NUMOBX(ISARNX,NUMTYP)=NUMOBS(ISATEL,IFREQ)
                ENDIF
360           CONTINUE
320         CONTINUE
            CLOSE(UNIT=LFNLOC)
          END IF
C
310     CONTINUE
C
C PREPARE RINEX HEADER
C
        IF(IFLGCO.EQ.1) THEN
          COMENT(NCOM+1)=CAMPGN
          COMENT(NCOM+2)=TITLE(1:36)
          NCOMNT=NCOM+2
        ELSE
          NCOMNT=NCOM
        END IF
        STANAX=STANAM(1)
        OPRNAX=OPRNAM(1)
        AGENCY=' '
        MARKER=' '
        RECTYX=RECTYP(1)
        RCVERS=' '
        IF (ANTTYP(1) .NE. ' ') THEN
          ANTTYX=ANTTYP(1)
        ELSE
          ANTTYX=RECTYP(1)
        END IF
C
C GET STATION COORDINATES
        NSTAT=1
        CALL GETSTA(NSTAT,STANAM,STANUM,NCENTR,ICENTR,
     1            POSXYZ,XSTELL,XSTECC,
     2            DATUM,AELL,BELL,DXELL,DRELL,SCELL)
C
C OPEN OBSERVATION FILES
C ----------------------
        KFIL = 0
        DO 510 K = 1,2
          IF(OBSFIL(K).NE.' ') THEN
            CALL OPNFIL(LFNOBS(K),OBSFIL(K),'UNKNOWN','UNFORMATTED',
     1                  ' ',' ',IOSTAT)
            CALL OPNERR(LFNERR,LFNOBS(K),IOSTAT,OBSFIL(K),'BV3RXO')
            KFIL=KFIL+K
            IRETRN(K)=0
            OBSTIM(K)=0.D0
          END IF
          KFLAG(K)=0
510     CONTINUE
        NEPREC = 0
C
C LOOP OVER ALL EPOCHS
C --------------------
C
100     CONTINUE
C
C CODE OR PHASE FILE (ONLY ONE)
        IF(KFIL.NE.3) THEN
C
C READ OBSERVATIONS
          CALL RDOBSI(LFNOBS(KFIL),IFRMAT,NFREQ(KFIL),IFRQS,
     1                OBSTIM(KFIL),DELTAT(1,KFIL),EPOFLG(KFIL),
     2                NSAT(KFIL),NRSAT(1,KFIL),OBSFLG(1,1,KFIL),
     3                OBSERV(1,1,KFIL),IRETRN(KFIL))
          IF(IRETRN(KFIL).EQ.1) GOTO 110
          KFLAG(KFIL)=1
          EPOCHX(1) = OBSTIM(KFIL) + DELTAT(1,KFIL)/86400.D0
          IF (IRDFLG .EQ. 0) THEN
C
C CREATE RINEX HEADER
C -------------------
            TFIRST = EPOCHX(1)
            TLAST=TIMREF(KFIL)+(NEPOCH(KFIL)-1)*IDELTT(KFIL)/86400.D0
            TLAST = TLAST + DELTAT(1,KFIL)/86400.D0
C
C FORMAT VERSION AND SATELLITE SYSTEM
            IRXVRS=2
            IF (SVNSYS(10,NSARNX,SATRNX)) THEN
              IRXVRS=IRXVRS+500
            ELSEIF (SVNSYS(1,NSARNX,SATRNX)) THEN
              IRXVRS=IRXVRS+100
            ENDIF
C
            CALL R2WTOH(LFNRNX,LFNERR,MAXSAT,
     1                  PRGNAM,RUNBY,DATE,TIME,NCOMNT,COMENT,
     2                  STANAX,MARKER,OPRNAX,AGENCY,
     3                  IRUNIT(1),RECTYX,RCVERS,
     4                  IANTEN(1),ANTTYX,POSXYZ,POSECC,
     5                  IWLFCR,IWLSAT,NWLSAT,
     6                  NUMTYP,OBSTYP,IDELTT(KFIL),TFIRST,TLAST,
     7                  NSARNX,SATRNX,NUMOBX,IRXVRS,IRC)
C
C           CALL RXWTOH(LFNRNX,LFNERR,MAXSAT,
C    1                  PRGNAM,RUNBY,DATE,TIME,NCOMNT,COMENT,
C    2                  STANAX,OPRNAX,AGENCY,IRUNIT,RECTYX,RCVERS,
C    3                  IANTEN,ANTTYX,POSXYZ,POSECC,IWLFCR,
C    4                  NUMTYP,OBSTYP,IDELTT(KFIL),TFIRST,TLAST,
C    5                  NSARNX,SATRNX,NUMOBX,IRC)
            IRCODE=IRC
            IF(IRC.NE.0) GOTO 999
            IRDFLG = 1
          ENDIF
        ELSE
C
C CODE AND PHASE FILES
          IF(DABS(OBSTIM(1)-OBSTIM(2)).LT.EPSTIM) THEN
            ITIMDI=0
          ELSE
            ITIMDI=1
          END IF
          IF((OBSTIM(1).LE.OBSTIM(2).OR.ITIMDI.EQ.0) .AND.
     1        IRETRN(1).EQ.0)
     2      CALL RDOBSI(LFNOBS(1),IFRMAT,NFREQ(1),IFRQS,OBSTIM(1),
     3                  DELTAT(1,1),EPOFLG(1),NSAT(1),NRSAT(1,1),
     4                  OBSFLG(1,1,1),OBSERV(1,1,1),IRETRN(1))
          IF(IRETRN(1).EQ.1) OBSTIM(1)=1.D20
          IF(DABS(OBSTIM(1)-OBSTIM(2)).LT.EPSTIM) THEN
            ITIMDI=0
          ELSE
            ITIMDI=1
          END IF
          IF((OBSTIM(2).LE.OBSTIM(1).OR.ITIMDI.EQ.0) .AND.
     1        IRETRN(2).EQ.0)
     2      CALL RDOBSI(LFNOBS(2),IFRMAT,NFREQ(2),IFRQS,OBSTIM(2),
     3                  DELTAT(1,2),EPOFLG(2),NSAT(2),NRSAT(1,2),
     4                  OBSFLG(1,1,2),OBSERV(1,1,2),IRETRN(2))
          IF(IRETRN(2).EQ.1) OBSTIM(2)=1.D20
          IF(IRETRN(1).EQ.1.AND.IRETRN(2).EQ.1) GOTO 110
          IF(DABS(OBSTIM(1)-OBSTIM(2)).LT.EPSTIM) THEN
            ITIMDI=0
          ELSE
            ITIMDI=1
          END IF
          IF(OBSTIM(1).LE.OBSTIM(2).OR.ITIMDI.EQ.0) THEN
            KFLAG(1)=1
            EPOCHX(1)=OBSTIM(1) + DELTAT(1,1)/86400.D0
          ELSE
            KFLAG(1)=0
          END IF
          IF(OBSTIM(2).LE.OBSTIM(1).OR.ITIMDI.EQ.0) THEN
            KFLAG(2)=1
            EPOCHX(1)=OBSTIM(2) + DELTAT(1,2)/86400.D0
          ELSE
            KFLAG(2)=0
          END IF
          IF (IRDFLG .EQ. 0) THEN
C
C CREATE RINEX HEADER
C -------------------
            TFIRST = EPOCHX(1)
            TLAST = TIMREF(2) + (NEPOCH(2)-1)*IDELTT(2)/86400.D0
            TLAST = TLAST + DELTAT(1,2)/86400.D0
C
C FORMAT VERSION AND SATELLITE SYSTEM
            IRXVRS=2
            IF (SVNSYS(10,NSARNX,SATRNX)) THEN
              IRXVRS=IRXVRS+500
            ELSEIF (SVNSYS(1,NSARNX,SATRNX)) THEN
              IRXVRS=IRXVRS+100
            ENDIF
C
            CALL R2WTOH(LFNRNX,LFNERR,MAXSAT,
     1                  PRGNAM,RUNBY,DATE,TIME,NCOMNT,COMENT,
     2                  STANAX,MARKER,OPRNAX,AGENCY,
     3                  IRUNIT(1),RECTYX,RCVERS,
     4                  IANTEN(1),ANTTYX,POSXYZ,POSECC,
     5                  IWLFCR,IWLSAT,NWLSAT,
     6                  NUMTYP,OBSTYP,IDELTT(2),TFIRST,TLAST,
     7                  NSARNX,SATRNX,NUMOBX,IRXVRS,IRC)
C
C           CALL RXWTOH(LFNRNX,LFNERR,MAXSAT,
C    1                  PRGNAM,RUNBY,DATE,TIME,NCOMNT,COMENT,
C    2                  STANAX,OPRNAX,AGENCY,IRUNIT,RECTYX,RCVERS,
C    3                  IANTEN,ANTTYX,POSXYZ,POSECC,IWLFCR,
C    4                  NUMTYP,OBSTYP,IDELTT(2),TFIRST,TLAST,
C    5                  NSARNX,SATRNX,NUMOBX,IRC)
            IRCODE=IRC
            IF(IRC.NE.0) GOTO 999
            IRDFLG = 1
          ENDIF
        END IF
C
C KEEP GOOD OBSERVATIONS
C
        NSATEP=0
        DO 70 K=1,2
          IF(KFLAG(K).NE.0) THEN
            DO 90 ISAT=1,NSAT(K)
              IF((OBSERV(ISAT,1,K).EQ.0.D0.OR.
     1            TSTFLG(OBSFLG(ISAT,1,K),0)).AND.
     2           (OBSERV(ISAT,NFREQ(K),K).EQ.0.D0.OR.
     3            TSTFLG(OBSFLG(ISAT,NFREQ(K),K),0))) GOTO 90
C
C CREATE LIST OF SATELLITES IN CURRENT EPOCH
              DO 60 ISATEP=1,NSATEP
                IF(SATEPO(ISATEP).EQ.NRSAT(ISAT,K)) GOTO 50
60            CONTINUE
              NSATEP=NSATEP+1
              ISATEP=NSATEP
              SATEPO(ISATEP)=NRSAT(ISAT,K)
C
C COPY GOOD OBS TO LIST ACCORDING TO OBSERVATION TYPE
50            DO 80 IFRQ=1,NFREQ(K)
                IREF=REFOBS(K,IFRQ)
                IF(IREF.EQ.0) GOTO 80
                SIGNAL(ISATEP,IREF)=ICHAR(OBSFLG(ISAT,IFRQ,K))/16.D0
                LLI(ISATEP,IREF)=0
                IF (TSTFLG(OBSFLG(ISAT,IFRQ,K),1)) LLI(ISATEP,IREF) = 1
                IF(OBSERV(ISAT,IFRQ,K).NE.0.D0.AND.
     1             .NOT.TSTFLG(OBSFLG(ISAT,IFRQ,K),0)) THEN
                  OBSEPO(ISATEP,IREF)=OBSERV(ISAT,IFRQ,K)
C
C ADJUST TO RINEX UNITS
                  IF(K.EQ.2) THEN
C PHASE: FROM METERS INTO CYCLES OF L1 OR L2
                    OBSEPO(ISATEP,IREF)=-OBSEPO(ISATEP,IREF)/WLGTH(IFRQ)
                  END IF
                ELSE
                  OBSEPO(ISATEP,IREF)=0.D0
                END IF
80            CONTINUE
90          CONTINUE
          END IF
70      CONTINUE
C
C EPOCH FLAG
        IF (EPOFLG(1) .NE. CHAR(0) .OR.
     1      EPOFLG(2) .NE. CHAR(0)     ) THEN
          IFLAG = 1
        ELSE
          IFLAG = 0
        END IF
C
C WRITE OBSERVATIONS TO RINEX FILE
        CALL R2WTOR(LFNRNX,LFNERR,MAXSAT,IRXVRS,
     1              NUMTYP,ISNMIN,ISNTHR,ISNMAX,
     2              EPOCHX,IFLAG,NSATEP,SATEPO,
     3              OBSEPO,SIGNAL,LLI,IRC)
C       CALL RXWTOR(LFNRNX,LFNERR,MAXSAT,
C    1              NUMTYP,ISNMIN,ISNTHR,ISNMAX,
C    2              EPOCHX,IFLAG,NSATEP,SATEPO,
C    3              OBSEPO,SIGNAL,LLI,IRC)
        IRCODE=IRC
        IF(IRC.NE.0) GOTO 999
        NEPREC=NEPREC+1
C
C NEXT EPOCH
C ----------
        GOTO 100
C
C CLOSE OBSERVATION FILES
C -----------------------
110     DO 120 K=1,2
          IF(OBSFIL(K).NE.' ') CLOSE(UNIT=LFNOBS(K))
120     CONTINUE
        RNXOLD=RNXFIL
C
C WRITE INPUT AND OUTPUT FILE NAMES AND NUMBER OF EPOCHS
C ------------------------------------------------------
290     IF(HEADER(1).NE.' ') THEN
          FILPRT(1)=HEADER(1)
          FILPRT(2)=OBSFIL(1)
        ELSE
          FILPRT(1)='  ---'
          FILPRT(2)='  ---'
        ENDIF
C
        IF(HEADER(2).NE.' ') THEN
          FILPRT(3)=HEADER(2)
          FILPRT(4)=OBSFIL(2)
        ELSE
          FILPRT(3)='  ---'
          FILPRT(4)='  ---'
        ENDIF
        IF(RNXFIL.NE.' ') THEN
          FILPRT(5)=RNXFIL
        ELSE
          FILPRT(5)='  ---'
        ENDIF
C
        WRITE(LFNPRT,291) IFIL,FILPRT(1),FILPRT(5),NEPREC,
     1                    FILPRT(2),FILPRT(3),FILPRT(4)
291     FORMAT(I5,2X,A32,2X,A32,I6,/,3(7X,A32,/))
C
C END OF LOOP OVER ALL FILES
C ----------------------------------------------------------------------
300   CONTINUE
999   CALL EXITRC(IRCODE)
      END
