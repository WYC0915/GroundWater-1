      MODULE s_GETRNX
      CONTAINS

C*
      SUBROUTINE GETRNX(MAXREC,MAXCMB,FILNAM,DCBFIL,SMPINT,OBSWIN,
     1                  IEPFLG,RMSTON,IC2USE,ICBEST,NRSAT ,NUMSAT,
     2                  NEPOCH,OBSTIM,OBSREC,OBSFLG,CODTYP,IRCODE,
     3                  USEGEOS,GOBSDEF,SIPFIL)
CC
CC NAME       :  GETRNX
CC
CC PURPOSE    :  READ ALL THE OBSERVATIONS OF A RINEX FILE INTO MEMORY
CC
CC PARAMETERS :
CC        IN  :  MAXREC : MAXIMUM NUMBER OF RECORDS           I*4
CC               MAXCMB : MAXIMUM NUMBER OF OBSERVATION TYPES I*4
CC                        AND COMBINATIONS
CC               FILNAM : NAME OF THE RINEX FILE TO BE READ  CH*(*)
CC               DCBFIL : NAME OF THE DCB FILE TO BE WRITTEN CH*(*)
CC                        (IF ICBEST > 0)
CC               SIPFIL : NAME OF FILE WITH SLANT TEC INFO   CH*(*)
CC               SMPINT : SAMPLING INTERVAL
CC               OBSWIN : OBSERVATION WINDOW (1: START,2:END) R*8(2)
CC               IEPFLG : WHAT TO DO IN CASE OF EVENT FLAGS   I*4
CC                        1: IGNORE LINES, WARNING
CC                        2: WARNING, CONTINUE WITH NEXT FILE
CC                        3: WARNING, STOP WITH ERROR
CC               RMSTON : IF S1&S2-OBS ARE ZERO               I*4
CC                        1: REMOVE ALL OBSERVATIONS
CC               IC2USE : USE C2 IF P2 UNAVAILABLE            I*4
CC                        =0: NO
CC                        =1: YES
CC               ICBEST : DIRECT ESTIMATION OF DCB VALUES     I*4
CC                        =0: NO
CC                        =1: P1-C1
CC                        =2: P1-P2
CC       OUT :   NRSAT  : NUMBER OF OBSERVED SATELLITES       I*4
CC               NUMSAT : SATELLITE NUMBERS                   I*4
CC               NEPOCH : NUMBER OF POINTS IN X AND Y ARRAY   I*4
CC               OBSTIM : ARRAY WITH TIME VALUES (MJD)        R*8(*)
CC               OBSREC : ARRAY WITH OBSERVATIONS             R*8(*,*,*)
CC               OBSFLG : ARRAY WITH OBSERVATION FLAGS       CH*1(*,*,*)
CC               CODTYP : FIRST FREQUENCY CODE TYPE (P1/C1)  CH*2
CC               IRCODE : RETURN CODE                         I*4
CC                        0: OK,
CC                        1: CONTINUE WITH NEXT RINEX FILE
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  T.A. SPRINGER
CC
CC CREATED    :  22-JUL-1996
CC
CC CHANGES    :  06-FEB-97 : TS: CHECK TIME ORDER OF EPOCHS
CC               23-SEP-97 : DI: USE MAXSAT.inc
CC               31-AUG-98 : DI: USE DEFREQ FOR GLONASS SATELLITES
CC               09-SEP-99 : TS: DO NOT USE C1 IF P1 AVAILABLE
CC               08-OCT-99 : TS/SS: "OBSTYP" IN CALL OF SR R2RDOR
CC               15-JAN-02 : MM: SET MAXTYP=10, DIMTST
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               18-FEB-03 : SS: CONSIDER FIRST FREQUENCY CODE TYPE
CC               25-FEB-03 : SS: DEMAND MINIMAL PERCENTAGE OF P1
CC                               MEASUREMENTS
CC               28-FEB-03 : SS: TOLERANCES OF 85/15 CHANGED TO 70/30
CC               01-APR-03 : SS: ALLOCATE "OBSAUX"
CC               30-JUN-03 : HB: ADOPT FOR CHANGES in SR R2RDOH
CC                               (SPACEBORNE)
CC               07-JUL-03 : RD: OBSAUX MUST BE "SAVE"
CC               08-SEP-03 : HU: FILENAMES CHR(*)
CC               13-SEP-03 : HU: INTERFACE FOR DEFREQ
CC               04-DEC-03 : HB: REMOVE ONLY FOR M_BERN,
CC                               COMMENT INCLUDE 'COMLFNUM.inc'
CC               12-JAN-04 : RD: SKIP SAT. W/O FREQ IN DEFREQ
CC                               USE OBSERVATION WINDOW
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               08-Aug-05 : HB: Use new SR TIMST2 (module)
CC               27-SEP-05 : RD: IGNORE EPOCH FLAGS
CC               28-SEP-05 : RD: INPUT OPTION FOR EVENT FLAG HANDLING
CC               06-JAN-06 : RD: WORK-AROUND FOR TEQC-GLONASS LEAP SECOND PROBL.
CC               16-AUG-06 : HU: MAXTYP 10 -> 12
CC               20-NOV-06 : RD: REMOVE OBSERV. IF S1&S2-OBS. ARE ZERO
CC               20-AUG-07 : AG: MAXCHN 24 -> 40
CC               09-JAN-08 : RD: MOVE MAXCHN-TEST (CONFLICT WITH EPO-FLAG 4)
CC               31-JAN-08 : HB: FORMAT FOR MESSAGE:SPORADIC P1 CHANGED
CC               29-JUL-09 : SS: DIRECT ESTIMATION OF DCB VALUES
CC               29-JUL-09 : SS: USE C2 IF P2 UNAVAILABLE
CC               10-AUG-09 : SS: SR RNXDCB IMPLEMENTED
CC               03-NOV-10 : DT: MAXTYP 12->18
CC               18-JAN-11 : SL: MAXTYP FROM D_RINEX3
CC               26-JAN-11 : LP: SATELLITE-SPECIFIC OBS TYPES
CC               04-MAR-11 : MM: USE SATCRUX
CC               20-MAY-11 : LP: CORRECT WRONG GIOVE-A CODE OBSERVATIONS
CC               16-SEP-11 : EO/SL: DIMTST WITHOUT EXIT IF MAXTYP EXCEEDED
CC               03-OCT-11 : LP: Direct estimation of P1-P2 DCBs, too (ICBEST)
CC               01-NOV-11 : LP: Sat-specific obstypes also for C1/P2 receivers
CC               05-NOV-11 : RD: INCREASE RATION P1/C1 FROM 50 TO 60%
CC               01-DEC-11 : SS: RNXDCB CALL CHANGED, TEST FOR ICBEST
CC               01-DEC-11 : SL: USE M_BERN WITH ONLY
CC               13-JAN-12 : SS: CHANGED STATION NAME EXTRACTION
CC               09-FEB-12 : LP: MAXCOM 10->60; bugfix; correct also IOV code obs.
CC               23-FEB-12 : LP: Reinitialize array (OBSEPO...) before each R2RDOR call (new epoch)
CC               08-MAR-12 : LP: Correct huge code offsets for all GALILEO satellites
CC               23-APR-12 : LP: Handling of slant TEC (SIP) file for RNXDCB
CC               30-APR-12 : LP: Correct also GALILEO code offsets of 100000km
CC               04-MAY-12 : RD: USE DMOD FROM MODULE, REMOVE UNUSED VARIABLES
CC               24-MAY-12 : RD: INCREASE RATION P1/C1 FROM 60 TO 80%
CC               28-AUG-12 : SL/PW: MAXCHN SET TO MAXSAT FROM M_MAXDIM
CC               16-NOV-12 : RD: INIT ALL ELEMENTS FROM OBSFLG
CC               07-MAR-13 : SS: MAXCOM FROM 60 TO 300 (DUE TO BISK/POUS)
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1996     UNIVERSITY OF BERN
CC               SWITZERLAND
C*
      USE m_bern,   ONLY: r8b, fileNameLength, lfnErr, lfn001
      USE m_maxdim, ONLY: MAXSAT,MAXBAD
      USE d_const,  ONLY: c
      USE d_rinex3, ONLY: maxtyp, t_gobsdef, obslistbasic
      USE d_satcrx, ONLY: gtsatb
      USE l_basfun, ONLY: dmod
      USE s_dimtst
      USE s_alcerr
      USE s_opnfil
      USE s_defreq
      USE s_exitrc
      USE s_r2rdoh
      USE s_jmt
      USE s_radgms
      USE s_clrflg
      USE s_opnerr
      USE s_r2rdor
      USE s_timst2
      USE s_rnxdcb
      USE s_fparse
      USE f_lengt1
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I4    , IEPOCH, IFIRST, IFLAG , IHOUR , IMONTH,
     1          INTEPO, INTER , IOSTAT, IRC   , IRXVRS, ISAT  , IYEAR ,
     2          J     , JSAT  , K     , L1L2  , LFN   , MAXCHN, MAXCMB,
     3          MAXCOM, MAXREC, MAXSLP, MINUTE, NCOD  , NCOM  ,
     4          NEPFLG, NEPOCH, NEPSAT, NMEA  , NOBSTP, NOBSTPHLP,
     5          NOFRQ , NRANT , NRSAT , NRUNIT, NSATEL, NUMLIN,
     6          NWLSAT, IEPFLG, IRCODE, ISSTON, RMSTON, IC2USE, ICBEST,
     7          NAUX  , ITYP  , IBAD  , NBAD  , ICOMB
C
      REAL*8    DAY   , EPOCH , EPOSAV, RATCOD, RATMEA, RATNAM, SEC   ,
     1          SMPINT, TFIRST, TLAST
      REAL*8    epochArray(2)
C
CCC       IMPLICIT  REAL*8(A-H,O-Z)
C
      PARAMETER(MAXCHN=MAXSAT,MAXCOM=300,MAXSLP=300)
C
C MAXCHN: MAXIMUM NUMBER OF CHANNELS
C MAXTYP: MAXIMUM NUMBER OF OBSERVATION TYPES IN RINEX FILE
C MAXCOM: MAXIMUM NUMBER OF COMMENT LINES
C MAXREC: MAXIMUM NUMBER OF OBS. PER SATELLITE IN FILE (24 HOURS WITH 30SEC SAMPLING)
C MAXARC: MAXIMUM NUMBER OF ARCS PER SATELLITE
C MAXSLP: MAXIMUM NUMBER OF CYCLE SLIPS PER SATELLITE
C MAXCMB: MAXIMUM NUMBER OF OBSERVATION TYPES USED
C MAXSAT: MAXIMUM NUMBER OF SATELLITES
C
C
C GLOBAL VARIABLES
C ----------------
      REAL*8       OBSTIM(MAXREC),OBSREC(MAXREC,MAXCMB,MAXSAT)
C
      CHARACTER*(*) FILNAM,DCBFIL
      CHARACTER*(*),OPTIONAL :: SIPFIL
      CHARACTER*2  CODTYP
      CHARACTER*1  OBSFLG(MAXREC,MAXCMB,MAXSAT),OBST
C
C LOCAL VARIABLES
C ---------------
      CHARACTER(LEN=fileNameLength) FILCRX
      CHARACTER(LEN=fileNameLength) NODE,DEVICE,DIR,NAME,EXT,VER
      CHARACTER    OPNAME*20,AGENCY*40,ANTTYP*20,RECTYP*20
      CHARACTER    COMENT(MAXCOM)*60,STRING*80
      CHARACTER    CH1*1,CRDATE*9,CRTIME*5
      CHARACTER    PRGNAM*20,RCVERS*20,OBSTYP(MAXTYP)*2
      CHARACTER    RUNBY*20,SITNAM*60,SITNUM*40
      CHARACTER    MEATYP(6)*2,AUXTYP*2
      CHARACTER    EPONOF*40,STANAM*16
C
      INTEGER*4    SATBAD(MAXBAD),IOBBAD(MAXBAD),IACBAD(MAXBAD)
      INTEGER*4    IWLFAC(2),IWLSAT(3,MAXSAT)
      INTEGER*4    SATEPO(MAXCHN),NUMSAT(MAXSAT),NUMOBS(MAXSAT,MAXTYP)
      INTEGER*4    LLI(MAXCHN,MAXTYP),ISIGN(MAXCHN,MAXTYP)
      INTEGER*4    SATSLP(2,MAXSAT)
      INTEGER*4    MEANUM(6),NAMNUM(6)
      INTEGER*4    SATNOF(MAXSAT),LSTNOF(MAXSAT),STON(MAXSAT)
      INTEGER*4    CALLED,geosflag,NOBSTYPE,isys,iprn,igeos,indgeos
      INTEGER*4,   OPTIONAL :: USEGEOS
C
      type(t_gobsdef),OPTIONAL :: GOBSDEF
C
      REAL*8       TIMBAD(2,MAXBAD)
      REAL*8       POSECC(3,3),SIGNAL(MAXCHN,MAXTYP)
      REAL*8       POSXYZ(3),OBSEPO(MAXCHN,MAXTYP)
      REAL*8       EPOFRQ(2),OBSWIN(2)
      REAL*8       TIMNOF(2,MAXSAT),CORRECTION
C
      REAL(r8b),DIMENSION(:,:,:),ALLOCATABLE,SAVE :: obsaux
C
      COMMON/CCRINX/SIGNAL,OBSEPO,NUMOBS,LLI,ISIGN
C
CCC      INCLUDE 'COMLFNUM.inc'
      INCLUDE 'COMFREQ.inc'
C
      DATA MEATYP/'L1','L2','P1','P2','C1','C2'/
C
      DATA IFIRST/1/
C
C ALLOCATE LOCAL VARIABLES
C ------------------------
      IF (IFIRST.EQ.1) THEN
        ALLOCATE (obsaux(MAXREC,2,MAXSAT),STAT=IRC)
        CALL alcerr(IRC,'OBSAUX',(/MAXREC,2,MAXSAT/),'GETRNX')
C
        NBAD=0
        CALL GTFLNA(0,'SATCRUX',FILCRX,IRC)
        CALL GTSATB(MAXBAD,FILCRX,NBAD,SATBAD,IOBBAD,IACBAD,TIMBAD)
C
        IFIRST=0
      ENDIF
C
C INIT LIST OF SATELLITES W/O FREQ IN DEFREQ
C ------------------------------------------
      DO ISAT=1,MAXSAT
        LSTNOF(ISAT)=0
      ENDDO
C
C OPEN RINEX FILE
C ---------------
      LFN=LFN001
      CALL OPNFIL(LFN001,FILNAM,'OLD','FORMATTED',
     1            ' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFN001,IOSTAT,FILNAM,'GETRNX')
C
C INITIALIZE VALUES
C -----------------
      IRCODE=0
      NRSAT=0
      IEPOCH=0
      NEPFLG=0
      DO I=1,MAXREC
        OBSTIM(I)=0.D0
        DO K=1,MAXSAT
          DO J=1,MAXCMB
            CALL CLRFLG(OBSFLG(I,J,K),0)
            CALL CLRFLG(OBSFLG(I,J,K),1)
            CALL CLRFLG(OBSFLG(I,J,K),2)
            OBSREC(I,J,K)=0.D0
          ENDDO
        ENDDO
      ENDDO
      obsaux(:,:,:)=0.D0
      MEANUM(:)=0
      NAMNUM(:)=0
      OBSTYP(:)='  '
      NUMSAT(:)=0
      NUMOBS(:,:)=0
C
C READ RINEX HEADER
C -----------------
      NUMLIN=0
      CALLED=0
C
      IF (PRESENT(USEGEOS).AND.PRESENT(GOBSDEF)) THEN
       IF ((USEGEOS==1).AND.(gobsdef%norec>0)) THEN
        CALL R2RDOH(LFN   ,LFNERR,MAXSAT,MAXCOM,NUMLIN,
     1            PRGNAM,RUNBY,CRDATE,CRTIME,NCOM,COMENT,
     2            SITNAM,SITNUM,OPNAME,AGENCY,
     3            NRUNIT,RECTYP,RCVERS,
     4            NRANT ,ANTTYP,POSXYZ,POSECC,
     5            IWLFAC,IWLSAT,NWLSAT,
     6            NOBSTP,OBSTYP,INTER, TFIRST,TLAST,
     7            NSATEL,NUMSAT,NUMOBS,IRXVRS,IRC,USEGEOS=USEGEOS,
     8            GOBSDEF=GOBSDEF)
        CALLED=1
       ENDIF
      ENDIF
      IF (CALLED==0) THEN
        CALL R2RDOH(LFN   ,LFNERR,MAXSAT,MAXCOM,NUMLIN,
     1            PRGNAM,RUNBY,CRDATE,CRTIME,NCOM,COMENT,
     2            SITNAM,SITNUM,OPNAME,AGENCY,
     3            NRUNIT,RECTYP,RCVERS,
     4            NRANT ,ANTTYP,POSXYZ,POSECC,
     5            IWLFAC,IWLSAT,NWLSAT,
     6            NOBSTP,OBSTYP,INTER, TFIRST,TLAST,
     7            NSATEL,NUMSAT,NUMOBS,IRXVRS,IRC)
      ENDIF
C
      IF(IRC.NE.0) THEN
        WRITE(LFNERR,901)IRC,TRIM(FILNAM)
901     FORMAT(/,' ### SR GETRNX: ERROR READING RINEX HEADER',/,
     1                       16X,'RET.CODE OF SR R2RDOH: ',I4,/,
     2                       16X,'STOP PROCESSING FILE : ',A,/)
        IRC=1
        GOTO 250
      END IF
C
      IF(IWLFAC(1).EQ.0) IWLFAC(1)=IWLSAT(1,1)
      IF(IWLFAC(2).EQ.0) IWLFAC(2)=IWLSAT(2,1)
      L1L2=0
      IF(IWLFAC(1).NE.0) L1L2=L1L2+1
      IF(IWLFAC(2).NE.0) L1L2=L1L2+2
C
C CHECK DIMENSION
C ---------------
      CALL DIMTST(0,2,2,'GETRNX','MAXTYP',
     1            'NUMBER OF OBSERVATION TYPES',' ',NOBSTP,MAXTYP,IRC)
      IF(IRC.EQ.1) THEN
        IRCODE=1
        CLOSE(LFN)
        WRITE(LFNERR,'(/A,I3/16X,A,I3,2(/,16X,A),/)')
     1  ' ### SR GETRNX: TOO MANY OBSERVATION TYPES:',NOBSTP,
     2                   'MAXIMUM NUMBER ALLOWED:',MAXTYP,
     3                   'PROCESSING CONTINUED IGNORING THIS FILE!',
     4                   'RINEX FILE NAME: ' // TRIM(FILNAM)

        RETURN
      ENDIF
      NOBSTPHLP   = NOBSTP
C
C LOOP OVER ALL RECORDS
C ---------------------
100   CALLED      = 0
      OBSEPO(:,:) = 0.D0
      ISIGN(:,:)  = 0
      LLI(:,:)    = 0
      SATEPO(:)   = 0
      NOBSTP      = NOBSTPHLP
C
      IF (PRESENT(USEGEOS).AND.PRESENT(GOBSDEF)) THEN
       IF ((USEGEOS==1).AND.(gobsdef%norec>0)) THEN
        CALL R2RDOR(LFN  ,LFNERR,MAXCHN,IRXVRS,NOBSTP,OBSTYP,
     1            epochArray,IFLAG,NEPSAT,SATEPO,
     2            OBSEPO,ISIGN,LLI,IRC,USEGEOS=USEGEOS,
     3            GOBSDEF=GOBSDEF)
        CALLED=1
       ENDIF
      ENDIF
      IF (CALLED==0) THEN
        CALL R2RDOR(LFN  ,LFNERR,MAXCHN,IRXVRS,NOBSTP,OBSTYP,
     1            epochArray,IFLAG,NEPSAT,SATEPO,
     2            OBSEPO,ISIGN,LLI,IRC)
      ENDIF
C
      EPOCH = epochArray(1)
      IF(IRC.NE.0) GOTO 250
C
C SKIP INSERTED HEADER RECORDS
C ----------------------------
      IF(IFLAG.GT.1) THEN
        IF (IFLAG.NE.4.AND.IEPFLG.EQ.1) THEN
          WRITE(LFNERR,'(/A,I3,2(/,16X,A),/)')
     1    ' ### SR GETRNX: EVENT FLAG NOT HANDLED:',IFLAG,
     2                    'PROCESSING CONTINUED IGNORING THIS FLAG!',
     3                    'RINEX FILE NAME: ' // TRIM(FILNAM)
        END IF
        IF (IFLAG.EQ.4.OR.IEPFLG.EQ.1) THEN
          DO 110 I4=1,NEPSAT
            READ(LFN   ,'(A)',END=250) STRING
110       CONTINUE
          GOTO 100
C
        ELSE IF (IEPFLG.EQ.2.OR.IEPFLG.EQ.3) THEN
          WRITE(LFNERR,'(/A,I3,2(/,16X,A),/)')
     1      ' ### SR GETRNX: EVENT FLAG NOT HANDLED:',IFLAG,
     2                  'PROCESSING STOPPED FOR THIS FILE!',
     3                  'RINEX FILE NAME: ' // TRIM(FILNAM)
          IF (IEPFLG.EQ.2) THEN
            CLOSE(LFN)
            NEPOCH=0
            IRCODE=1
            RETURN
C
          ELSE IF (IEPFLG.EQ.3) THEN
            CALL EXITRC(2)
          ENDIF
        ENDIF
      END IF
C
      CALL JMT(EPOCH,IYEAR,IMONTH,DAY)
      CALL RADGMS(3,DAY,CH1,IHOUR,MINUTE,SEC)
C
C REMOVE SATELLITES FROM SATCRX
C -----------------------------
      JSAT=0
      DO 114 ISAT=1,NEPSAT
        indgeos = 0
        geosflag= 0
        NOBSTP = NOBSTPHLP
        IF (PRESENT(USEGEOS).AND.PRESENT(GOBSDEF)) THEN
         IF ((USEGEOS==1).AND.(gobsdef%norec>0)) THEN
          DO igeos=1,gobsdef%norec
           IF (gobsdef%sat(igeos)%eposatind==ISAT) THEN
             geosflag= 1
             indgeos = igeos
             NOBSTP = 4
             EXIT
           ENDIF
          ENDDO
         ENDIF
        ENDIF
C
        DO 115 IBAD=1,NBAD
          IF (SATEPO(ISAT).NE.SATBAD(IBAD).OR.
     1        IACBAD(IBAD).NE.2           .OR.
     2        TIMBAD(1,IBAD).GT.EPOCH     .OR.
     3        TIMBAD(2,IBAD).LT.EPOCH          ) GOTO 115
C
          IF (IOBBAD(IBAD).EQ.3) THEN
            OBSEPO(ISAT,:)=0.D0
          ELSE
            DO ITYP=1,NOBSTP
              IF (geosflag==1) THEN
                IF (indgeos==0) EXIT
                IF ((IOBBAD(IBAD).EQ.1).AND.
     1            (gobsdef%sat(indgeos)%obstyp2(ITYP)(1:1).EQ."L")) THEN
                  OBSEPO(ISAT,ITYP)=0.D0
                ELSEIF ((IOBBAD(IBAD).EQ.2).AND.
     1            (gobsdef%sat(indgeos)%obstyp2(ITYP)(1:1).EQ."P")) THEN
                  OBSEPO(ISAT,ITYP)=0.D0
                ENDIF
              ELSE
C
                IF (IOBBAD(IBAD).EQ.1.AND.
     1            OBSTYP(ITYP)(1:1).EQ."L") THEN
                  OBSEPO(ISAT,ITYP)=0.D0
                ELSEIF (IOBBAD(IBAD).EQ.2.AND.
     1                (OBSTYP(ITYP)(1:1).EQ."C".OR.
     2                 OBSTYP(ITYP)(1:1).EQ."P")) THEN
                  OBSEPO(ISAT,ITYP)=0.D0
                ENDIF
              ENDIF
C
            ENDDO
          ENDIF
115     CONTINUE
C
        IF (.NOT.ALL(OBSEPO(ISAT,:).EQ.0.D0)) THEN
          JSAT=JSAT+1
          IF (JSAT.EQ.ISAT) GOTO 114
          SATEPO(JSAT)=SATEPO(ISAT)
          OBSEPO(JSAT,:)=OBSEPO(ISAT,:)
          ISIGN(JSAT,:)=ISIGN(ISAT,:)
          LLI(JSAT,:)=LLI(ISAT,:)
          IF (geosflag==1) THEN
            gobsdef%sat(indgeos)%eposatind=JSAT
          ENDIF
        ENDIF
114   CONTINUE
C
      NEPSAT=JSAT
C
C CHECK NUMBER OF SATELLITES FOR THIS EPOCH
C -----------------------------------------
      IF(NEPSAT>MAXCHN)THEN
        WRITE(lfnerr,906)MAXCHN,NEPSAT,EPOCH
906     FORMAT(/,' *** SR GETRNX: MAXCHN exceeded!',/,
     1                       16X,'MAXCHN         : ',I4,/,
     2                       16X,'# of satellites: ',I4,/,F15.7)
        IF(NEPSAT>MAXCHN) CALL EXITRC(2)
      ENDIF
C
C KEEP FIRST EPOCH
C ----------------
      IF(IEPOCH.EQ.0) THEN
        EPOSAV=EPOCH
      ELSE
C
C CHECK TIME ORDER OF EPOCHS
C --------------------------
        IF (EPOCH.LE.OBSTIM(IEPOCH)) GOTO 100
      END IF
C
C CHECK OBSERVATION WINDOW
C ------------------------
      IF (EPOCH.LT.OBSWIN(1)) GOTO 100
      IF (EPOCH.GT.OBSWIN(2)) GOTO 250
C
C SAMPLING
C --------
      INTEPO=NINT(DMOD((EPOCH-DINT(EPOSAV))*86400D0,SMPINT))
      IF(INTEPO.EQ.DNINT(SMPINT) .OR. INTEPO.EQ.0 .OR.
     1   SMPINT.EQ.0.0)THEN
        IEPOCH=IEPOCH+1
      ELSE
        GOTO 100
      ENDIF
C
C  CHECK IF SATELLITES ARE SET ACTIVE IN SATELLITE FILE AND SET
C  THE CORRESPONDING FREQUENCIES (CF.'I:COMFREQ')
C  ------------------------------------------------------------
      EPOFRQ(1)= EPOCH
      EPOFRQ(2)= EPOCH
C
      CALLED=0
      IF (PRESENT(USEGEOS).AND.PRESENT(GOBSDEF)) THEN
       IF ((USEGEOS==1).AND.(gobsdef%norec>0)) THEN
        OBST='U'
        CALL DEFREQ(EPOFRQ,NEPSAT,SATEPO,NOFRQ,SATNOF,
     1              USEGEOS=USEGEOS,GOBSDEF=GOBSDEF,MEATYPC=OBST)
        CALLED=1
       ENDIF
      ENDIF
      IF (CALLED==0) THEN
        CALL DEFREQ(EPOFRQ,NEPSAT,SATEPO,NOFRQ,SATNOF)
      ENDIF
C
C  CHECK OBSERVATIONS
C    - COUNT EPOCH FLAGS
C    - DUAL FREQ INSTR: BOTH L1 AND L2 HAVE TO BE PRESENT
C    - GET NUMBER OF OBSERVED SATELLITES PER EPOCH
C    - COUNT LOSSES OF LOCK
C    - DETERMINE NUMBER OF MISSING EPOCHS
C
      IF(IFLAG.GT.1) NEPFLG=NEPFLG+1
      OBSTIM(IEPOCH)=EPOCH
C
      ISSTON=0
      STON(1:NEPSAT)=0
C
      DO 120 I=1,NEPSAT
C
C HAS THIS SATELLITE A FREQUENCY?
C -------------------------------
        DO 125 ISAT=1,NOFRQ
          IF (SATEPO(I).EQ.SATNOF(ISAT)) THEN
            DO JSAT=1,MAXSAT
              IF (LSTNOF(JSAT).EQ.SATEPO(I)) THEN
                TIMNOF(2,JSAT)=EPOCH
                GOTO 120
              ENDIF
              IF (LSTNOF(JSAT).EQ.0) THEN
                LSTNOF(JSAT)=SATEPO(I)
                TIMNOF(1,JSAT)=EPOCH
                TIMNOF(2,JSAT)=EPOCH
                GOTO 120
              ENDIF
            ENDDO
          ENDIF
125     CONTINUE
C
C FILL SATELLITE ARRAY AND FIND RIGHT SATELLITE INDEX
C ---------------------------------------------------
        DO 130 ISAT=1,NRSAT
          IF (SATEPO(I).EQ.NUMSAT(ISAT)) GOTO 140
130     CONTINUE
        NRSAT=NRSAT+1
        NUMSAT(NRSAT)=SATEPO(I)
        ISAT=NRSAT
        SATSLP(1,ISAT)=0
        SATSLP(2,ISAT)=0
140     CONTINUE
C
C ORDER THE OBSERVATIONS
C ----------------------
        isys = INT(SATEPO(I)/100)
        iprn = SATEPO(I)-isys*100
        geosflag = 0
        indgeos  = 0
        IF (PRESENT(USEGEOS).AND.PRESENT(GOBSDEF)) THEN
         IF ((USEGEOS==1).AND.(gobsdef%norec>0)) THEN
          DO igeos=1,gobsdef%norec
           IF ((gobsdef%sat(igeos)%sysnum==isys)
     1        .AND.(gobsdef%sat(igeos)%satnum==iprn)) THEN
              geosflag=1
              indgeos = igeos
              EXIT
           ENDIF
          ENDDO
         ENDIF
        ENDIF
        IF (geosflag==0) THEN
           NOBSTYPE=NOBSTPHLP
        ELSE IF(geosflag==1) THEN
           NOBSTYPE=4
        ENDIF
C
        DO 150 K=1,NOBSTYPE
C
C CONVERT L1 TO METERS
C --------------------
          CALLED=0
          IF(geosflag==0) THEN
           IF(OBSTYP(K).EQ.'L1') THEN
              CALLED=1
           ENDIF
          ELSE IF(geosflag==1) THEN
           IF(gobsdef%sat(indgeos)%obstyp2(K).EQ.'L1') THEN
              CALLED=1
           ENDIF
          ENDIF
C
          IF(CALLED==1) THEN
            OBSREC(IEPOCH,1,ISAT)=OBSEPO(I,K)*WLGT(1,SATEPO(I))
            IF (LLI(I,K).GT.0.AND.LLI(I,K).LT.4) THEN
              SATSLP(1,ISAT)=SATSLP(1,ISAT)+1
              IF (SATSLP(1,ISAT).GT.MAXSLP) THEN
                WRITE(LFNERR,902)SATSLP(1,ISAT),MAXSLP,TRIM(FILNAM)
902             FORMAT(/,' ### SR GETRNX: MAXSLP EXCEEDED',/,
     1                               16X,'NUMBER OF SLIPS : ',I4,/,
     2                               16X,'MAXIMUM NUMBER  : ',I4,/,
     3                               16X,'RINEX FILE      : ',A,/)
                SATSLP(1,ISAT)=1
              ENDIF
            ENDIF
            IF (OBSEPO(I,K).NE.0.D0) THEN
              MEANUM(1)=MEANUM(1)+1
            ELSE
              NAMNUM(1)=NAMNUM(1)+1
            ENDIF
          END IF
C
C CONVERT L2 TO METERS
C --------------------
          CALLED=0
          IF(geosflag==0) THEN
           IF(OBSTYP(K).EQ.'L2') THEN
              CALLED=1
           ENDIF
          ELSE IF(geosflag==1) THEN
           IF(gobsdef%sat(indgeos)%obstyp2(K).EQ.'L2') THEN
              CALLED=1
           ENDIF
          ENDIF
C
          IF(CALLED==1) THEN
            OBSREC(IEPOCH,2,ISAT)=OBSEPO(I,K)*WLGT(2,SATEPO(I))
            IF (LLI(I,K).GT.0.AND.LLI(I,K).LT.4) THEN
              SATSLP(2,ISAT)=SATSLP(2,ISAT)+1
              IF (SATSLP(2,ISAT).GT.MAXSLP) THEN
                WRITE(LFNERR,902)SATSLP(2,ISAT),MAXSLP,TRIM(FILNAM)
                SATSLP(2,ISAT)=1
              ENDIF
            ENDIF
            IF (OBSEPO(I,K).NE.0.D0) THEN
              MEANUM(2)=MEANUM(2)+1
            ELSE
              NAMNUM(2)=NAMNUM(2)+1
            ENDIF
          END IF
C
C P1 IN METERS
C ------------
          CALLED=0
          IF(geosflag==0) THEN
           IF(OBSTYP(K).EQ.'P1') THEN
              CALLED=1
           ENDIF
          ELSE IF(geosflag==1) THEN
           IF(gobsdef%sat(indgeos)%obstyp2(K).EQ.'P1') THEN
              CALLED=1
           ENDIF
          ENDIF
C
          IF (CALLED==1) THEN
            OBSREC(IEPOCH,3,ISAT)=OBSEPO(I,K)
            if(numsat(isat).gt.100.and.numsat(isat).lt.200.and.
     1         dabs(OBSREC(IEPOCH,3,ISAT)).gt.c) then
              do while (OBSREC(IEPOCH,3,ISAT).gt.c)
                OBSREC(IEPOCH,3,ISAT)=OBSREC(IEPOCH,3,ISAT)-c
              enddo
              do while (OBSREC(IEPOCH,3,ISAT).lt.-c)
                OBSREC(IEPOCH,3,ISAT)=OBSREC(IEPOCH,3,ISAT)+c
              enddo
            endif
C
c           Correct for half and full second code offsets for GALILEO satellites
            correction = 0.d0
            if (isys.eq.2) then
             if (dabs(obsrec(iepoch,3,isat)).gt.0.2*c) then
              correction = 0.3*c
              if (dabs(obsrec(iepoch,3,isat)).gt.0.3*c) then
                correction = 0.5*c
                if (dabs(obsrec(iepoch,3,isat)).gt.0.75*c) then
                  correction = c
                  if (dabs(obsrec(iepoch,3,isat)).gt.1.5*c) then
                    correction = 2*c
                  endif
                endif
              endif
              if (obsrec(iepoch,3,isat).lt.0d0) then
                  obsrec(iepoch,3,isat)=
     1                   obsrec(iepoch,3,isat)+correction
              else
                  obsrec(iepoch,3,isat)=
     1                   obsrec(iepoch,3,isat)-correction
              endif
             endif
            endif
C
            IF (OBSEPO(I,K).NE.0.D0) THEN
              MEANUM(3)=MEANUM(3)+1
            ELSE
              NAMNUM(3)=NAMNUM(3)+1
            ENDIF
          END IF
C
C P2 IN METERS
C ------------
          CALLED=0
          IF(geosflag==0) THEN
           IF(OBSTYP(K).EQ.'P2') THEN
              CALLED=1
           ENDIF
          ELSE IF(geosflag==1) THEN
           IF(gobsdef%sat(indgeos)%obstyp2(K).EQ.'P2') THEN
              CALLED=1
           ENDIF
          ENDIF
C
          IF(CALLED==1) THEN
            OBSREC(IEPOCH,4,ISAT)=OBSEPO(I,K)
            if(numsat(isat).gt.100.and.numsat(isat).lt.200.and.
     1         dabs(OBSREC(IEPOCH,4,ISAT)).gt.c) then
              do while (OBSREC(IEPOCH,4,ISAT).gt.c)
                OBSREC(IEPOCH,4,ISAT)=OBSREC(IEPOCH,4,ISAT)-c
              enddo
              do while (OBSREC(IEPOCH,4,ISAT).lt.-c)
                OBSREC(IEPOCH,4,ISAT)=OBSREC(IEPOCH,4,ISAT)+c
              enddo
            endif
c
c           Correct for half and full second code offsets for GALILEO satellites
            correction = 0.d0
            if (isys.eq.2) then
             if (dabs(obsrec(iepoch,4,isat)).gt.0.2*c) then
              correction = 0.3*c
              if (dabs(obsrec(iepoch,4,isat)).gt.0.3*c) then
                correction = 0.5*c
                if (dabs(obsrec(iepoch,4,isat)).gt.0.75*c) then
                  correction = c
                  if (dabs(obsrec(iepoch,4,isat)).gt.1.5*c) then
                    correction = 2*c
                  endif
                endif
              endif
              if (obsrec(iepoch,4,isat).lt.0d0) then
                  obsrec(iepoch,4,isat)=
     1                   obsrec(iepoch,4,isat)+correction
              else
                  obsrec(iepoch,4,isat)=
     1                   obsrec(iepoch,4,isat)-correction
              endif
             endif
            endif
c
            IF (OBSEPO(I,K).NE.0.D0) THEN
              MEANUM(4)=MEANUM(4)+1
            ELSE
              NAMNUM(4)=NAMNUM(4)+1
            ENDIF
          END IF
C
C C1 IN METERS
C ------------
          CALLED=0
          IF(geosflag==0) THEN
           IF(OBSTYP(K).EQ.'C1') THEN
              CALLED=1
           ENDIF
          ENDIF
          IF (CALLED==1) THEN
            obsaux(IEPOCH,1,ISAT)=OBSEPO(I,K)
            if(numsat(isat).gt.100.and.numsat(isat).lt.200.and.
     1         dabs(OBSaux(IEPOCH,1,ISAT)).gt.c) then
              do while (OBSaux(IEPOCH,1,ISAT).gt.c)
                OBSaux(IEPOCH,1,ISAT)=OBSaux(IEPOCH,1,ISAT)-c
              enddo
              do while (OBSaux(IEPOCH,1,ISAT).lt.-c)
                OBSaux(IEPOCH,1,ISAT)=OBSaux(IEPOCH,1,ISAT)+c
              enddo
            endif
c
c           Correct for half and full second code offsets for GALILEO satellites
            correction = 0.d0
            if (isys.eq.2) then
             if (dabs(obsaux(IEPOCH,1,ISAT)).gt.0.2*c) then
              correction = 0.3*c
              if (dabs(obsaux(IEPOCH,1,ISAT)).gt.0.3*c) then
                correction = 0.5*c
                if (dabs(obsaux(IEPOCH,1,ISAT)).gt.0.75*c) then
                  correction = c
                  if (dabs(obsaux(IEPOCH,1,ISAT)).gt.1.5*c) then
                    correction = 2*c
                  endif
                endif
              endif
              if (obsaux(IEPOCH,1,ISAT).lt.0d0) then
                  obsaux(IEPOCH,1,ISAT)=
     1                   obsaux(IEPOCH,1,ISAT)+correction
              else
                  obsaux(IEPOCH,1,ISAT)=
     1                   obsaux(IEPOCH,1,ISAT)-correction
              endif
             endif
            endif
c
            IF (OBSEPO(I,K).NE.0.D0) THEN
              MEANUM(5)=MEANUM(5)+1
            ELSE
              NAMNUM(5)=NAMNUM(5)+1
            ENDIF
          END IF
C
C C2 IN METERS
C ------------
          CALLED=0
          IF(geosflag==0) THEN
           IF(OBSTYP(K).EQ.'C2') THEN
              CALLED=1
           ENDIF
          ENDIF
          IF (CALLED==1) THEN
            obsaux(IEPOCH,2,ISAT)=OBSEPO(I,K)
            if(numsat(isat).gt.100.and.numsat(isat).lt.200.and.
     1         dabs(OBSaux(IEPOCH,2,ISAT)).gt.c) then
              do while (OBSaux(IEPOCH,2,ISAT).gt.c)
                OBSaux(IEPOCH,2,ISAT)=OBSaux(IEPOCH,2,ISAT)-c
              enddo
              do while (OBSaux(IEPOCH,2,ISAT).lt.-c)
                OBSaux(IEPOCH,2,ISAT)=OBSaux(IEPOCH,2,ISAT)+c
              enddo
            endif
c
c           Correct for half and full second code offsets for GALILEO satellites
            correction = 0.d0
            if (isys.eq.2) then
             if (dabs(obsaux(IEPOCH,2,ISAT)).gt.0.2*c) then
              correction = 0.3*c
              if (dabs(obsaux(IEPOCH,2,ISAT)).gt.0.3*c) then
                correction = 0.5*c
                if (dabs(obsaux(IEPOCH,2,ISAT)).gt.0.75*c) then
                  correction = c
                  if (dabs(obsaux(IEPOCH,2,ISAT)).gt.1.5*c) then
                    correction = 2*c
                  endif
                endif
              endif
              if (obsaux(IEPOCH,2,ISAT).lt.0d0) then
                  obsaux(IEPOCH,2,ISAT)=
     1                   obsaux(IEPOCH,2,ISAT)+correction
              else
                  obsaux(IEPOCH,2,ISAT)=
     1                   obsaux(IEPOCH,2,ISAT)-correction
              endif
             endif
            endif
c
            IF (OBSEPO(I,K).NE.0.D0) THEN
              MEANUM(6)=MEANUM(6)+1
            ELSE
              NAMNUM(6)=NAMNUM(6)+1
            ENDIF
          END IF
C
C CHECK FOR S/N-OBS == 0D0
C ------------------------
          CALLED=0
          IF(geosflag==0) THEN
           IF (OBSTYP(K).EQ.'S1'.OR.OBSTYP(K).EQ.'S2') THEN
              CALLED=1
           ENDIF
          ENDIF
          IF (CALLED==1) THEN
            IF (OBSEPO(I,K).EQ.0D0) THEN
              ISSTON=ISSTON+1
              STON(I)=STON(I)+1
            ENDIF
          ENDIF
C
C END OBSERVATION LOOP
C --------------------
150     CONTINUE
C
C END SATELLITE LOOP
C ------------------
120   CONTINUE
C
C CHECK FOR S/N-OBS == 0D0
C ------------------------
      IF (RMSTON.EQ.1.AND.ISSTON.LT.2*NEPSAT) THEN
        DO I=1,NEPSAT
          IF (STON(I) == 2) THEN
            DO ISAT=1,NRSAT
              IF (SATEPO(I).EQ.NUMSAT(ISAT)) THEN
                OBSREC(IEPOCH,:,ISAT)=0D0
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDIF
C
C NEXT RECORD
C -----------
      GOTO 100
C
C END OF RINEX FILE
C -----------------
250   CLOSE(LFN)
      NEPOCH=IEPOCH
C
C TEST SOME OF THE MAXIMUM DIMENSIONS
C -----------------------------------
      IF (NRSAT.GT.MAXSAT) THEN
        WRITE(LFNERR,903)NRSAT,MAXSAT,TRIM(FILNAM)
903     FORMAT(/,' *** SR GETRNX: TOO MANY SATELLITES',/,
     1                       16X,'NUMBER OF SATELLITES     : ',I5,/,
     2                       16X,'MAX. NUMBER OF SATELLITES: ',I5,/,
     3                       16X,'RINEX FILE               : ',A,/)
        CALL EXITRC(2)
      END IF
C
      IF (NEPOCH.GT.MAXREC) THEN
        WRITE(LFNERR,904)NEPOCH,MAXREC,TRIM(FILNAM)
904     FORMAT(/,' *** SR GETRNX: TOO MANY OBSERVATION EPOCHS',/,
     1                       16X,'NUMBER OF EPOCHS     : ',I5,/,
     2                       16X,'MAX. NUMBER OF EPOCHS: ',I5,/,
     3                       16X,'RINEX FILE           : ',A,/)
        CALL EXITRC(2)
      END IF
C
C REPORT SATELLITES REMOVED BECAUSE NO FREQUENCY AVAILABLE IN DEFREQ
C ------------------------------------------------------------------
      DO ISAT=1,MAXSAT
        IF (LSTNOF(ISAT).EQ.0) EXIT
        CALL TIMST2(1,2,TIMNOF(1:2,ISAT),EPONOF)
        WRITE(LFNERR,905) LSTNOF(ISAT),EPONOF,TRIM(FILNAM)
905     FORMAT(/,' ### SR GETRNX: OBSERVATIONS REMOVED BECAUSE NOT ',
     1                           'BOTH FREQUENCIES ARE DEFINED FOR',/,
     2                       16X,'SATELLITE NUMBER : ',I3,/,
     3                       16X,'EPOCHS           : ',A,/,
     4                       16X,'RINEX FILE       : ',A,/)
      ENDDO
C
C TAKE P1 MEASUREMENTS WHEN MINIMAL PERCENTAGE AVAILABLE
C ------------------------------------------------------
      IF (MEANUM(5).GT.0) THEN
        RATCOD=(100.D0*MEANUM(3))/MEANUM(5)
      ELSE
        RATCOD=100.D0
      ENDIF
C
      IF (RATCOD.GT.80.D0) THEN
        CODTYP='P1'
        NCOD=MEANUM(3)
      ELSEIF (ICBEST.EQ.0) THEN
        CODTYP='C1'
        NCOD=MEANUM(5)
C
        DO I=1,NEPOCH
          DO K=1,NRSAT
cccc
c           Turn off this selection for satellites with individual
c           obstype selection
            geosflag=0
            isys = INT(NUMSAT(K)/100)
            iprn = NUMSAT(K)-isys*100
            IF (PRESENT(USEGEOS).AND.PRESENT(GOBSDEF)) THEN
              IF ((USEGEOS==1).AND.(gobsdef%norec>0)) THEN
                DO igeos=1,gobsdef%norec
                  IF ((gobsdef%sat(igeos)%sysnum==isys)
     1              .AND.(gobsdef%sat(igeos)%satnum==iprn)) THEN
                    geosflag=1
                    EXIT
                  ENDIF
                ENDDO
              ENDIF
            ENDIF
cccc
            IF (geosflag == 0) THEN
              OBSREC(I,3,K)=obsaux(I,1,K)
            ENDIF
          ENDDO
        ENDDO
C
        IF (MEANUM(3).GT.0) THEN
          WRITE(LFNERR,911) MEANUM(3),TRIM(FILNAM)
911       FORMAT(/,' ### SR GETRNX: ',
     1          'SPORADIC P1 MEASUREMENTS IGNORED',/,
     2      16X,'NUMBER OF MEASUREMENTS : ',I8,/,
     3      16X,'RINEX FILE             : ',A,/)
        ENDIF
      ENDIF
C
C USE C2 IF P2 UNAVAILABLE
C ------------------------
      IF (MEANUM(6).GT.0 .AND. IC2USE.EQ.1 .AND. ICBEST.EQ.0) THEN
        NAUX=0
        DO I=1,NEPOCH
          DO K=1,NRSAT
            IF (OBSREC(I,4,K).EQ.0.D0 .AND. obsaux(I,2,K).NE.0.D0) THEN
              OBSREC(I,4,K)=obsaux(I,2,K)
              NAUX=NAUX+1
CC              MEANUM(4)=MEANUM(4)+1
            ENDIF
          ENDDO
        ENDDO
C
        IF (NAUX.GT.0) THEN
          WRITE(LFNERR,912) NAUX,TRIM(FILNAM)
912       FORMAT(/,' ### SR GETRNX: ',
     1          'C2 MEASUREMENTS SUBSTITUTED FOR P2 MEASUREMENTS',/,
     2      16X,'NUMBER OF MEASUREMENTS : ',I8,/,
     3      16X,'RINEX FILE             : ',A,/)
        ENDIF
      ENDIF
C
      NMEA=MAXVAL(MEANUM(1:6))
      DO I=5,1,-1
        IF (MEANUM(I).EQ.NMEA) AUXTYP=MEATYP(I)
      ENDDO
C
      IF (NMEA.GT.0) THEN
C
C CHECK WHETHER INDICATED P1 OR C1 MEASUREMENTS AVAILABLE
C -------------------------------------------------------
        CALLED = 0
        IF (PRESENT(USEGEOS).AND.PRESENT(GOBSDEF)) THEN
         IF ((USEGEOS==1).AND.(gobsdef%norec>0)) THEN
          CALLED = 1
          DO I = 1,4
           DO K = 1,4
            IF (MEATYP(I).EQ.obslistbasic(K) .AND.MEANUM(I).EQ.0) THEN
              WRITE(LFNERR,921) MEATYP(I),TRIM(FILNAM)
            ENDIF
           ENDDO
          ENDDO
         ENDIF
        ENDIF
C
        IF (CALLED == 0) THEN
          NOBSTP = NOBSTPHLP
CC        DO I=1,6
          DO I=1,5
           DO K=1,NOBSTP
            IF (MEATYP(I).EQ.OBSTYP(K) .AND. MEANUM(I).EQ.0) THEN
              WRITE(LFNERR,921) MEATYP(I),TRIM(FILNAM)
921           FORMAT(/,' ### SR GETRNX: ',
     1              'INDICATED ',A2,' MEASUREMENTS NOT AVAILABLE',/,
     2          16X,'RINEX FILE: ',A,/)
            ENDIF
           ENDDO
          ENDDO
        ENDIF
C
C COMPARE TOTAL NUMBER OF MEASUREMENTS
C ------------------------------------
        DO I=1,5
          RATMEA=(100.D0*MEANUM(I))/NMEA
          IF (RATMEA.GT.0.D0 .AND. RATMEA.LT.70.D0) THEN
            WRITE(LFNERR,922) MEATYP(I),AUXTYP,RATMEA,TRIM(FILNAM)
922         FORMAT(/,' ### SR GETRNX: ',
     1            'LOW PERCENTAGE OF ',A2,' MEASUREMENTS',/,
     2        16X,'PERCENTAGE (WRT ',A2,') : ',F5.1,/,
     3        16X,'RINEX FILE          : ',A,/)
          ENDIF
C
          RATNAM=(100.D0*NAMNUM(1))/(MEANUM(1)+NAMNUM(1))
          IF (RATNAM.LT.100.D0 .AND. RATNAM.GT.30.D0) THEN
            WRITE(LFNERR,923) MEATYP(I),RATNAM,TRIM(FILNAM)
923         FORMAT(/,' ### SR GETRNX: ',
     1            'MANY MISSING ',A2,' MEASUREMENTS',/,
     2        16X,'PERCENTAGE : ',F5.1,/,
     3        16X,'RINEX FILE : ',A,/)
          ENDIF
        ENDDO
C
C CHECK WHETHER L1, L2, P1/C1, P2 MEASUREMENTS AVAILABLE
C ------------------------------------------------------
        IF (MEANUM(1).EQ.0 .OR.
     1      MEANUM(2).EQ.0 .OR.
     2      NCOD     .EQ.0 .OR.
     3      MEANUM(4).EQ.0) THEN
          WRITE(LFNERR,931) TRIM(FILNAM)
931       FORMAT(/,' ### SR GETRNX: ',
     1          'REQUIRED MEASUREMENTS NOT AVAILABLE',/,
     2      16X,'RINEX FILE: ',A,/)
        ENDIF
      ELSE
        WRITE(LFNERR,932) TRIM(FILNAM)
932     FORMAT(/,' ### SR GETRNX: ',
     1        'NO VALID MEASUREMENTS',/,
     2    16X,'RINEX FILE: ',A,/)
      ENDIF
C
C DIRECT ESTIMATION OF DCB VALUES
C -------------------------------
      IF (ICBEST > 0) THEN
        IF (LENGT1(SITNUM).EQ.9 .AND.
     1      (SITNUM(6:6).EQ.'M' .OR. SITNUM(6:6).EQ.'S')) THEN
          STANAM=SITNAM(1:4)//' '//SITNUM(1:9)
        ELSE IF (SITNAM(1:4).NE.'   ') THEN
          STANAM=SITNAM(1:4)
        ELSE
          STANAM = dcbfil(LENGT1(dcbfil)-11:LENGT1(dcbfil)-8)
        ENDIF
CC        STANAM=SITNAM(1:16)
        CALL FPARSE(0,FILNAM,NODE,DEVICE,DIR,NAME,EXT,VER,IRC)
        STANAM=NAME(1:4)
CC        STANAM(1:4)=NAME(1:4)
c
        CALLED=0
        ICOMB=0
        IF (PRESENT(USEGEOS).AND.PRESENT(GOBSDEF)) THEN
          IF ((USEGEOS==1).AND.(gobsdef%norec>0)) THEN
            CALL RNXDCB(MAXREC,MAXCMB,OBSREC,OBSAUX,NEPOCH,NRSAT,NUMSAT,
     1              OBSTIM,DCBFIL,STANAM,ICBEST,RECTYP,ICOMB,
     2              SIPFIL=SIPFIL,USEGEOS=USEGEOS,GOBSDEF=GOBSDEF)
            CALLED=1
          ENDIF
        ENDIF
        IF (CALLED==0) THEN
          CALL RNXDCB(MAXREC,MAXCMB,OBSREC,OBSAUX,NEPOCH,NRSAT,NUMSAT,
     1              OBSTIM,DCBFIL,STANAM,ICBEST,RECTYP,ICOMB,
     2              SIPFIL=SIPFIL)
        ENDIF
      ENDIF

      RETURN

      END SUBROUTINE getrnx

      END MODULE
