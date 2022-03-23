      MODULE s_CDSCLK
      CONTAINS

C*
      SUBROUTINE CDSCLK(ICLKSV,CODHED,CODOBS,PHAHED,PHAOBS,
     1                  ICLPOL,CLKMOD,MCLOCK,OFFS,NUMSAT,NUMOBS,
     2                  NUMMRK,NUMAMB,AMBSAT,AMBIEP,AMBWLF,AMBIGU,
     3                  AMBCLS,NRSAT,OBSFLG,OBSERV,IFILE,IOUTSV,
     4                  NDEL,LSTDEL,ICLK,CLKHED,CLKREC,SAMPL,IPHSAV)
CC
CC NAME       :  CDSCLK
CC
CC PURPOSE    :  WRITE TYPE OF CLOCK MODELLING INTO CODE AND PHASE
CC               FILE HEADERS AND CLOCK CORRECTIONS TO CODE AND
CC               PHASE OBSERVATION FILES
CC
CC PARAMETERS :
CC         IN :  ICLKSV : SELECTION FOR SAVING THE CLOCKS     I*4
CC                         0: NO SAVE
CC                         1: CODE OBS FILES ONLY
CC                         2: PHASE OBS FILES ONLY
CC                         3: CODE+PHASE OBS FILES
CC               CODHED : CODE HEADER FILE NAME              CH*(*)
CC               CODOBS : CODE OBSERVATION FILE NAME         CH*(*)
CC               PHAHED : PHASE HEADER FILE NAME             CH*(*)
CC               PHAOBS : PHASE OBSERVATION FILE NAME        CH*(*)
CC               ICLPOL : TYPE OF CLOCK MODELLING             I*4
CC                        ICLPOL>0: POL. OF DEGREE ICLPOL
CC                        ICLPOL=-1: ONE OFFSET PER EPOCH
CC               CLKMOD(K,I),K=1,..,ICLPOL,I=1,..,NFTOT:      R*8
CC                        CLOCK PARAMETERS FOR STATION I
CC               MCLOCK(K): RMS ERRORS OF CLOCK PARAMETERS    R*8
CC                        K=1,2,...,ICLPOL
CC               OFFS(IEPO),IEPO=1,MXCEPO: CLOCK OFFSET       R*8
CC               NUMSAT : SATELLITE NUMBERS                   I*4(1)
CC               NUMOBS : NUMBER OF OBSERVATIONS              I*4(*,2)
CC               NUMMRK : NUMBER OF MARKED OBSERVATIONS       I*4(*,2)
CC               NUMAMB : TOTAL NUMBER OF AMBIGUITIES (ALL     I*4
CC                        SATELLITES)
CC               AMBSAT : SATELLITE NUMBER                     I*4(*)
CC                          AMBSAT(I): AMBIGUITY NUMBER I
CC               AMBIEP : AMBIGUITY STARTING EPOCH NUMBER      I*4(*)
CC                          AMBIEP(I): AMBIGUITY NUMBER I
CC               AMBWLF : WAVELENGTH FACTORS                   I*4(*,2)
CC                          AMBWLF(I,J): WL.FACTOR OF AMBIGUITY
CC                                       I, FREQUENCY J
CC                          1: CYCLE AMBIGUITIES
CC                          2: HALF-CYCLE AMBIGUITIES
CC               AMBIGU : AMBIGUITIES                          R*8(*,3)
CC                          AMBIGU(I,K): AMBIGUITY NUMBER I
CC                                       FREQUENCY NUMBER K
CC                            K=1: L1 AMBIGUITIES
CC                            K=2: L2 AMBIGUITIES
CC                            K=3: L5 AMBIGUITIES (WIDELANE)
CC               AMBCLS : AMBIGUITY CLUSTERS                   I*4(*,3)
CC                          AMBIGU(I,K): AMBIGUITY NUMBER I
CC                                       FREQUENCY NUMBER K
CC                            K=1: L1 AMBIGUITIES
CC                            K=2: L2 AMBIGUITIES
CC                            K=3: L5 AMBIGUITIES (WIDELANE)
CC               NRSAT  : NUMBER OF SVN OF CURRENT EPOCH      I*4(*)
CC               OBSFLG(I,J): OBSERVATION FLAG (EPOCH)
CC                            I=1,MXCSAT,J=1,2 (FREQ.)       CH*1(*,*)
CC               OBSERV(I,J): OBSERVATIONS (EPOCH)
CC                            I=1,MXCSAT,J=1,2 (FREQ.)        R*8(*,*)
CC               IFILE  : INDEX OF FILE NUMBER                  I*4
CC               IOUTSV : SAVE MARKED AREAS IN OBS. FILE        I*4
CC                            0: NONE, 1: CODE, 2: PHASE, 3: BOTH
CC               NDEL   : NUMBER OF DELETIONS                   I*4
CC               LSTDEL(K,I),K=1,..,5, I=1,2,..,NDEL            I*4
CC                        DEFINITION OF MARK REQUEST NUMBER I
CC                        (1,I): SV-NUMBER
CC                        (2,I): FIRST EPOCH OF MARKED AREA I
CC                        (3,I): LAST  EPOCH OF MARKED AREA I
CC                        (4,I): FILE NUMBER
CC                        (5,I): MARKED BY
CC                               =1: OUTLIER DETECTION
CC                               =2: MISSING ORBITS
CC                               =3: SATCRUX
CC                               ALL AREAS MARKED OR CHANGED IN THE
CC                               LATEST RUN HAVE A NEGATIVE SIGN
CC               ICLK  : Clock index for clock rinex file     i*4
CC               CLKHED: clock rinex header                   t_clkhead
CC               CLKREC: clock rinex data record              t_clkrec
CC               SAMPL : SAMPLING (SEC)                       R*8
CC         OUT : IPHSAV: SAVE FLAG FOR PHASE FILES            I*4
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.5  (AUG 93)
CC
CC CREATED    :  87/12/11 12:02
CC
CC CHANGES    :  18-JUL-91 : ??: CLOSE AUXILIARY FILE WITH STATUS='DELETE'
CC               29-AUG-91 : ??: OPEN SCRATCH FILE WITH STATUS='UNKNOWN'
CC               27-JUL-92 : LM: CORRECTION THE DIFFERENCE BETWEEN
CC                               CODE AND PHASE HEADER FILE
CC               28-DEC-92 : ??: USE OF SR "OPNFIL" TO OPEN FILES
CC               03-AUG-93 : ??: NEW FORMAT, VERSION 3.5
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               10-AUG-94 : MR: CALL EXITRC
CC               12-AUG-94 : MR: FORMAT 4: SESSION AS CHARACTER*4
CC               23-SEP-97 : DI: USE MAXSAT.inc, CALL MAXTST
CC                               WITH OPTION '1' (GREATER OR EQUAL)
CC               09-NOV-00 : CU: CHANGE DESCRIPTION OF ICLKSV
CC               08-JAN-01 : RD: SAVE MARKED AREAS
CC               10-MAY-01 : RD: WRITE CLOCK RINEX FILE
CC               07-JUN-01 : RD: BUGFIX CLOCK POLYNOM FOR PHASE FILE (1ST EPO)
CC               31-JUL-02 : RD: DO NOT COUNT OBSERV==0 FOR OBS STATISTIC
CC               12-DEC-02 : RD: COMPUTE CORRECT EPOCHS FOR MARKING AREAS
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               08-SEP-03 : HU: FILENAMES DECLARED OPEN
CC                               ANTNAM, RECNAM, OPRNAM CHR16 -> CHR20
CC               10-MAR-04 : HB: CHANGE ORDER OF MODULES
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               30-NOV-05 : RD: MARK REALLY ONLY OUTLIERS
CC               01-MAR-06 : RD: ENABLE WRITING CLK-RINEX FOR 1-HZ DATA
CC                               CHANGE SIGN FOR CLK-RINEX VALUES
CC               12-JUN-06 : HB: USE NEW SR OSAMPL FOR FINDING CORRECT
CC                               SAMPLING EPOCH
CC               14-JUN-06 : HB: SMALL CORRECTION FOR FINDING CORRECT
CC                               SAMPLING EPOCH
CC               05-JUL-06 : RD: DO NOT WRITE CLK RINEX IF NO OFFS COMPUTED
CC               14-DEC-06 : SS: BUGFIX: USE NUMSAP IN CASE OF PHASE
CC               20-MAY-09 : HB: WRITE ORIGINAL REFERENCE TIME INTO CODE
CC                               OBSERVATION HEADER
CC               26-JAN-11 : LP: Sat-specific obs types
CC               30-AUG-11 : SS/RD: MARK GLONASS OBSERVATIONS IF INDICATED
CC               05-MAR-12 : RD: USE WTHEAD AS MODULE NOW
CC               05-MAR-12 : RD: CORRECT FORMAT STATEMENT
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnerr,lfnloc,lfn001,fileNameLength80
      USE m_global, ONLY: g_svnsys
      USE m_maxdim, ONLY: MAXSAT
      USE d_clkrnx, ONLY: t_clkhead,t_clkrec, UNDEF
      USE d_const,  ONLY: DATE, TIME
      USE d_rinex3, ONLY: t_gobsdef
      USE s_rdobsi
      USE s_opnfil
      USE f_tstflg
      USE s_wtobsi
      USE s_opnerr
      USE s_maxtst
      USE s_setflg
      USE s_rdhead
      USE s_osampl
      USE s_exitrc
      USE s_wthead
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 ICL   , ICLK  , ICLKSV, ICLPOL, IDEL  , IDELTT, IEPO  ,
     1          IEPOCH, IEPOCO, IFILE , IFR   , IFRMAT, IFRQ  , IOSTAT,
     2          IOUTSV, IPHSAV, IRC   , IRETRN, IRMARK, ISAT  , ISATEL,
     3          JEPO  , MEATYP, MXCAMB, MXCCLK, MXCEPO, MXCSAT, NDEL  ,
     4          NDIFF , NEPFLG, NEPOCH, NEPOCO, NFREQ , NSAT  , NSATEL,
     5          NUMAMB
C
      REAL*8    CSIGMA, OBSTIM, OBSEPO, OFFSET, TIMRCO, TIMREF, TREL  ,
     1          SAMPL , TSAMPL, TPREV , TNEXT , TIMREC
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      TYPE(t_clkhead):: clkhed
      TYPE(t_clkrec) :: clkrec
      TYPE(t_gobsdef):: gobsdef ! Giove External Obs. Selection info
C
      REAL*8       POSECC(3,2),AMBIGU(MXCAMB,3)
      REAL*8       CLKMOD(MXCCLK),MCLOCK(MXCCLK),DELTAT(2)
      REAL*8       OFFS(MXCEPO),OBSERV(MXCSAT,2)
      REAL*8       MAXCLK,CLKBAD
C
      INTEGER*4    IRUNIT(2),IANTEN(2)
      INTEGER*4    ICLOCK(2),NRSAT(MXCSAT)
      INTEGER*4    NUMSAT(MXCSAT),NUMOBS(MXCSAT,2),NUMMRK(MXCSAT,2)
      INTEGER*4    AMBSAT(MXCAMB),AMBIEP(MXCAMB),AMBWLF(MXCAMB,2)
      INTEGER*4    AMBCLS(MXCAMB,3)
      INTEGER*4    FILFRQ(2), LSTDEL(5,*),usegeos
      INTEGER*4    IEPBAD
C
      CHARACTER*53 TITLE
      CHARACTER*(*) CODHED,CODOBS,PHAHED,PHAOBS
      CHARACTER*(fileNameLength80) SC1FIL
      CHARACTER*16 CAMPGN,STANAM(2)
      CHARACTER*20 RECTYP(2),OPRNAM(2),ANTTYP(2)
      CHARACTER*9  CRDATE(2)
      CHARACTER*6  MXNSAT,MXNAMB,MXNCLK,MXNEPO
      CHARACTER*5  CRTIME(2)
      CHARACTER*4  CSESS(2)
      CHARACTER*1  EPOFLG,OBSFLG(MXCSAT,2)
C
C MAXIMAL DIMENSIONS
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMAMB/MXCAMB,MXNAMB
      COMMON/MCMCLK/MXCCLK,MXNCLK
      COMMON/MCMEPO/MXCEPO,MXNEPO
C
C LOCAL DIMENSIONS
C ----------------
      INTEGER*4  NUMSAP(MAXSAT)
C
      MAXCLK=1.D-3
C
C CHECK MAXIMUM LOCAL DIMENSIONS
C ------------------------------
      CALL MAXTST(1,'CDSCLK',MXNSAT,MAXSAT,MXCSAT,IRC)
      IF(IRC.NE.0) CALL EXITRC(2)
C
C PROCESS CODE FILE
C -----------------
C READ OLD CODE FILE HEADER
      usegeos=0
      gobsdef%norec=0
      CALL RDHEAD(CODHED,MEATYP,NDIFF,NFREQ,NEPOCH,NSATEL,
     1            CSESS,IDELTT,TIMREF,CAMPGN,TITLE,CRDATE,
     2            CRTIME,IRMARK,NEPFLG,IFRMAT,
     3            STANAM,RECTYP,ANTTYP,IRUNIT,IANTEN,
     4            OPRNAM,POSECC,ICLOCK,NUMSAT,NUMOBS,NUMMRK,
     5            NUMAMB,AMBSAT,AMBIEP,AMBWLF,AMBIGU,AMBCLS,
     6            1,usegeos=usegeos,gobsdef=gobsdef)
C
C CLOCK INFORMATION
      IF (ICLPOL .GT. 0) THEN
        ICLOCK(1) = ICLPOL
      ELSE IF (ICLPOL .EQ. -1) THEN
        ICLOCK(1) = 999
      ENDIF
C
C UPDATE MODIFCATION DATE
      CRDATE(2)=DATE
      CRTIME(2)=TIME
C
C OPEN OBSERVATION FILE
      CALL OPNFIL(LFN001,CODOBS,'OLD','UNFORMATTED',
     1            ' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFN001,IOSTAT,CODOBS,'CDSCLK')
C
C OPEN SCRATCH FILE (FOR COPY OF OBS. FILE)
      CALL GTFLNA(1,'AUXFIL1',SC1FIL,IRC)
      CALL OPNFIL(LFNLOC,SC1FIL,'UNKNOWN','UNFORMATTED',
     1            ' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNLOC,IOSTAT,SC1FIL,'CDSCLK')
C
      FILFRQ(1) = 1
      FILFRQ(2) = 2
C
C WRITE RECEIVER CLOCK CORRECTIONS TO CODE OBSERVATION FILE
      IF (ICLPOL .GT. 0) THEN
        DO 100 IEPO = 1,NEPOCH
C
C READ CODE OBSERVATION
          CALL RDOBSI(LFN001,IFRMAT,NFREQ,FILFRQ,OBSTIM,DELTAT,EPOFLG,
     1                NSAT,NRSAT,OBSFLG,OBSERV,IRETRN)
          IF(IRETRN.NE.0) GOTO 110
C
          OBSEPO = OBSTIM + DELTAT(1)/86400.D0
          IF (IEPO .EQ. 1) THEN
            TIMREC = TIMREF
            TIMREF = TIMREC + DELTAT(1)/86400.D0
          ENDIF
C
C EVALUATE CLOCK POLYNOMIAL
          TREL = (OBSEPO - TIMREF)*86400.D0
          OFFSET = 0.D0
          CSIGMA = 0.D0
          IF (TREL .EQ. 0.D0) THEN
            OFFSET = CLKMOD(1)
            CSIGMA = MCLOCK(1)
          ELSE
            DO 150 ICL = 1,ICLPOL
              OFFSET = OFFSET + CLKMOD(ICL)*(TREL**(ICL-1))
              CSIGMA = CSIGMA + MCLOCK(ICL)*(TREL**(ICL-1))
150         CONTINUE
          ENDIF
          DELTAT(2) = - OFFSET
C
C WRITE CLOCK CORRECTION AND OBSERVATIONS TO SCRATCH FILE
          CALL WTOBSI(LFNLOC,IFRMAT,NFREQ,OBSTIM,DELTAT,EPOFLG,
     1                NSAT,NRSAT,OBSFLG,OBSERV)
C
C Put clock into clock rinex record
C ---------------------------------
          IF (ClkHed%NumTyp /= 0) THEN
            CALL OSAMPL(OBSTIM,SAMPL,1D0,TSAMPL,TPREV,TNEXT)
            DO jEpo = 1,SIZE(clkrec%epoch)
              IF (DABS(CLkHed%TFirst+ClkRec%Epoch(jEpo)/86400d0 -
     1                 TSAMPL) < 0.1d0/86400d0) THEN
                ClkRec%Clock(iclk,jepo)=-(deltat(1)+deltat(2))
                ClkRec%Sigma(iclk,jepo)=csigma
              ENDIF
            ENDDO
          ENDIF
C
100     CONTINUE
110     CONTINUE
C
      ELSE IF (ICLPOL .EQ. -1) THEN
C
        CLKBAD=0.D0
        IEPBAD=0
C
        DO 200 IEPO=1,NEPOCH
          CALL RDOBSI(LFN001,IFRMAT,NFREQ,FILFRQ,OBSTIM,DELTAT,EPOFLG,
     1                NSAT,NRSAT,OBSFLG,OBSERV,IRETRN)
          IF(IRETRN.NE.0) GOTO 210
C
C COMPUTE EPOCH NUMBER
          IF (IEPO .EQ. 1) THEN
            TIMREC = TIMREF
            TIMREF = TIMREC + DELTAT(1)/86400.D0
          ENDIF
          OBSEPO = OBSTIM + DELTAT(1)/86400.D0
          IEPOCH = IDNINT(((OBSTIM-TIMREC)*86400.D0)/IDELTT)+1
C
C MARK ALL OBSERVATIONS OF CURRENT EPOCH
          IF (OFFS(IEPOCH) .EQ. 0.D0) THEN
            DO ISAT = 1,NSAT
              DO IFR = 1,NFREQ
                IF (.NOT. TSTFLG(OBSFLG(ISAT,IFR),0)) THEN
                  CALL SETFLG(OBSFLG(ISAT,IFR),0)
                  DO ISATEL=1,NSATEL
                    IF (NUMSAT(ISATEL).NE. NRSAT(ISAT)) CYCLE
                    NUMOBS(ISATEL,IFR)=NUMOBS(ISATEL,IFR)-1
                    NUMMRK(ISATEL,IFR)=NUMMRK(ISATEL,IFR)+1
                  ENDDO
                ENDIF
              ENDDO
            ENDDO
          ENDIF
C
C CHECK WITH RESPECT TO MAXCLK
          IF (DABS(OFFS(IEPOCH)) .GT. MAXCLK) THEN
            DO ISAT = 1,NSAT
              IF (G_SVNSYS(NRSAT(ISAT)/100) .NE. 'R') CYCLE
              DO IFR = 1,NFREQ
                IF (.NOT. TSTFLG(OBSFLG(ISAT,IFR),0)) THEN
                  CALL SETFLG(OBSFLG(ISAT,IFR),0)
                  DO ISATEL=1,NSATEL
                    IF (NUMSAT(ISATEL).NE. NRSAT(ISAT)) CYCLE
                    NUMOBS(ISATEL,IFR)=NUMOBS(ISATEL,IFR)-1
                    NUMMRK(ISATEL,IFR)=NUMMRK(ISATEL,IFR)+1
                    IF (DABS(OFFS(IEPOCH)) .GT. DABS(CLKBAD)) THEN
                      CLKBAD=OFFS(IEPOCH)
                      IEPBAD=IEPOCH
                    ENDIF
                  ENDDO
                ENDIF
              ENDDO
            ENDDO
          ENDIF
C
C WRITE CLOCK OFFSET TO SCRATCH FILE
          DELTAT(2) = - OFFS(IEPOCH)
          CALL WTOBSI(LFNLOC,IFRMAT,NFREQ,OBSTIM,DELTAT,EPOFLG,
     1                NSAT,NRSAT,OBSFLG,OBSERV)
C
C Put clock into clock rinex record
C ---------------------------------
          IF (ClkHed%NumTyp /= 0.AND.offs(iepoch) /= 0d0) THEN
            CALL OSAMPL(OBSTIM,SAMPL,1D0,TSAMPL,TPREV,TNEXT)
            DO jEpo = 1,SIZE(clkrec%epoch)
              IF (DABS(CLkHed%TFirst+ClkRec%Epoch(jEpo)/86400d0 -
     1                 TSAMPL) < 0.1d0/86400d0) THEN
                ClkRec%Clock(iclk,jepo)=-(deltat(1)+deltat(2))
                IF (ClkRec%Sigma(iClk,jEpo).NE.UNDEF)
     1            ClkRec%Sigma(iClk,jEpo)=DSQRT(ClkRec%Sigma(iClk,jEpo))
                EXIT
              ENDIF
            ENDDO
          ENDIF
C
200     CONTINUE
C
        IF (DABS(CLKBAD).GT.0.D0) THEN
          WRITE(LFNERR,
     1      '(/,A,A,/,16X,A,F12.9,A,/,16X,A,F12.9,A,I6,/,16X,A,A,/)')
     2      ' ### SR CDSCLK: GLONASS code observations marked ',
     3      'due to clock offsets',
     4      'exceeding ',MAXCLK,' sec',
     5      'reaching ',CLKBAD,' sec at epoch ',IEPBAD,
     6      'Station name: ',STANAM(1)
        ENDIF
C
      ENDIF
C
210   REWIND LFN001
      REWIND LFNLOC
C
C COPY SCRATCH FILE TO CODE OBSERVATION FILE
      IF (ICLKSV.EQ.1 .OR. ICLKSV.EQ.3) THEN
        DO 250 IEPO = 1,NEPOCH
          CALL RDOBSI(LFNLOC,IFRMAT,NFREQ,FILFRQ,OBSTIM,DELTAT,EPOFLG,
     1              NSAT,NRSAT,OBSFLG,OBSERV,IRETRN)
          IF(IRETRN.NE.0) GOTO 260
C
C MARK CODE-OUTLIERS
          IF (IOUTSV.EQ.1 .OR. IOUTSV.EQ.3) THEN
            JEPO=NINT((OBSTIM-TIMREC)*86400D0/IDELTT)+1
            DO IDEL=1,NDEL
              IF (ABS(LSTDEL(5,IDEL)).EQ.1   .AND.
     1            IFILE .EQ. LSTDEL(4,IDEL)  .AND.
     2            JEPO  .GE. LSTDEL(2,IDEL)  .AND.
     3            JEPO  .LE. LSTDEL(3,IDEL)) THEN
                DO ISAT=1,NSAT
                  IF (NRSAT(ISAT) .EQ. LSTDEL(1,IDEL)) THEN
                    DO ISATEL=1,NSATEL
                      IF (NUMSAT(ISATEL).EQ. NRSAT(ISAT)) THEN
                        DO IFRQ=1,NFREQ
                          IF (OBSERV(ISAT,IFRQ).EQ.0D0) CYCLE
                          IF (.NOT. TSTFLG(OBSFLG(ISAT,IFRQ),0)) THEN
                            CALL SETFLG(OBSFLG(ISAT,IFRQ),0)
                            NUMOBS(ISATEL,IFRQ)=NUMOBS(ISATEL,IFRQ)-1
                            NUMMRK(ISATEL,IFRQ)=NUMMRK(ISATEL,IFRQ)+1
                          ENDIF
                        ENDDO
                      ENDIF
                    ENDDO
                  ENDIF
                ENDDO
              ENDIF
            ENDDO
          ENDIF
C
          CALL WTOBSI(LFN001,IFRMAT,NFREQ,OBSTIM,DELTAT,EPOFLG,
     1              NSAT,NRSAT,OBSFLG,OBSERV)
250     CONTINUE
      ENDIF
C
260   CLOSE (UNIT = LFN001)
      CLOSE (UNIT = LFNLOC,STATUS='DELETE')
C
C WRITE NEW CODE HEADER
      IF (ICLKSV.EQ.1 .OR. ICLKSV.EQ.3) THEN
        CALL WTHEAD(CODHED,MEATYP,NDIFF,NFREQ,NEPOCH,NSATEL,
     1            CSESS,IDELTT,TIMREC,CAMPGN,TITLE,CRDATE,
     2            CRTIME,IRMARK,NEPFLG,IFRMAT,
     3            STANAM,RECTYP,ANTTYP,IRUNIT,IANTEN,
     4            OPRNAM,POSECC,ICLOCK,NUMSAT,NUMOBS,NUMMRK,
     5            NUMAMB,AMBSAT,AMBIEP,AMBWLF,AMBIGU,AMBCLS,
     6            usegeos,gobsdef)
      ENDIF
C
      IF (ASSOCIATED(GOBSDEF%SAT)) THEN
          DEALLOCATE(GOBSDEF%SAT,STAT=IRC)
      ENDIF
C
C SAVE REFERENCE TIME AND NUMBER OF CODE EPOCHS
      TIMRCO = TIMREC
      NEPOCO = NEPOCH
C
C PROCESS PHASE FILE
C ------------------
      IF (PHAOBS .NE. '       '  .AND.
     1    (ICLKSV.EQ.2 .OR. ICLKSV.EQ.3)) THEN
C
C SET SAVE FLAG
        IPHSAV = 1
C
C READ OLD PHASE HEADER
        usegeos=0
        gobsdef%norec=0
        CALL RDHEAD(PHAHED,MEATYP,NDIFF,NFREQ,NEPOCH,NSATEL,
     1              CSESS,IDELTT,TIMREF,CAMPGN,TITLE,CRDATE,
     2              CRTIME,IRMARK,NEPFLG,IFRMAT,
     3              STANAM,RECTYP,ANTTYP,IRUNIT,IANTEN,
     4              OPRNAM,POSECC,ICLOCK,NUMSAP,NUMOBS,NUMMRK,
     5              NUMAMB,AMBSAT,AMBIEP,AMBWLF,AMBIGU,AMBCLS,
     6              1,usegeos=usegeos,gobsdef=gobsdef)
C
C CLOCK INFORMATION
        IF (ICLPOL .GT. 0) THEN
          ICLOCK(1) = ICLPOL
        ELSE IF (ICLPOL .EQ. -1) THEN
          ICLOCK(1) = 999
        ENDIF
C
C UPDATE MODIFCATION DATE
        CRDATE(2)=DATE
        CRTIME(2)=TIME
C
C OPEN OBSERVATION AND SCRATCH FILES
        CALL OPNFIL(LFN001,PHAOBS,'OLD','UNFORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFN001,IOSTAT,PHAOBS,'CDSCLK')
C
        CALL OPNFIL(LFNLOC,SC1FIL,'UNKNOWN','UNFORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNLOC,IOSTAT,SC1FIL,'CDSCLK')
C
C WRITE RECEIVER CLOCK CORRECTIONS TO PHASE OBSERVATION FILE
        IF (ICLPOL .GT. 0) THEN
          DO 400 IEPO = 1,NEPOCH
C
C READ PHASE OBSERVATION
            CALL RDOBSI(LFN001,IFRMAT,NFREQ,FILFRQ,OBSTIM,DELTAT,
     1                  EPOFLG,NSAT,NRSAT,OBSFLG,OBSERV,IRETRN)
            IF(IRETRN.NE.0) GOTO 410
C
C EVALUATE CLOCK POLYNOMIAL
            IF (IEPO .EQ. 1) THEN
              TIMREC = TIMREF
              TIMREF = TIMREC + DELTAT(1)/86400.D0
            ENDIF
            OBSEPO = OBSTIM + DELTAT(1)/86400.D0
            TREL = (OBSEPO - TIMREF)*86400.D0
            OFFSET = 0.D0
            CSIGMA = 0.D0
            IF (TREL .EQ. 0.D0) THEN
              OFFSET = CLKMOD(1)
              CSIGMA = MCLOCK(1)
            ELSE
              DO 450 ICL = 1,ICLPOL
                OFFSET = OFFSET + CLKMOD(ICL)*(TREL**(ICL-1))
                CSIGMA = CSIGMA + MCLOCK(ICL)*(TREL**(ICL-1))
450           CONTINUE
            ENDIF
            DELTAT(2) = - OFFSET
C
C WRITE CLOCK CORRECTION AND OBSERVATIONS TO SCRATCH FILE
            CALL WTOBSI(LFNLOC,IFRMAT,NFREQ,OBSTIM,DELTAT,EPOFLG,
     1                  NSAT,NRSAT,OBSFLG,OBSERV)
C
C Put clock into clock rinex record
C ---------------------------------
            IF (ClkHed%NumTyp /= 0) THEN
              CALL OSAMPL(OBSTIM,SAMPL,1D0,TSAMPL,TPREV,TNEXT)
              DO jEpo = 1,SIZE(clkrec%epoch)
                IF (DABS(CLkHed%TFirst+ClkRec%Epoch(jEpo)/86400d0 -
     1                   TSAMPL) < 0.1d0/86400d0) THEN
                  ClkRec%Clock(iClk,jEpo)=-(deltat(1)+deltat(2))
                  ClkRec%Sigma(iclk,jepo)=csigma
                ENDIF
              ENDDO
            ENDIF
C
400       CONTINUE
410       CONTINUE
C
        ELSE IF (ICLPOL .EQ. -1) THEN
C
          CLKBAD=0.D0
          IEPBAD=0
C
          DO 500 IEPO=1,NEPOCH
            CALL RDOBSI(LFN001,IFRMAT,NFREQ,FILFRQ,OBSTIM,DELTAT,
     1                  EPOFLG,NSAT,NRSAT,OBSFLG,OBSERV,IRETRN)
            IF(IRETRN.NE.0) GOTO 510
C
C COMPUTE EPOCH NUMBER (CODE)
            OBSEPO = OBSTIM + DELTAT(1)/86400.D0
            IEPOCO = IDNINT(((OBSTIM - TIMRCO)*86400.D0)/IDELTT) + 1
C
            IF (IEPOCO .GE. 1 .AND. IEPOCO .LE. NEPOCO) THEN
C
C WRITE PHASE OBERVATION + CLOCK OFFSET TO SCRATCH FILE
              DELTAT(2) = - OFFS(IEPOCO)
C
C MARK ALL OBSERVATIONS OF CURRENT EPOCH
              IF (OFFS(IEPOCO) .EQ. 0.D0) THEN
                DO 420 ISAT = 1,NSAT
                  DO 425 IFR = 1,NFREQ
                    CALL SETFLG(OBSFLG(ISAT,IFR),0)
425               CONTINUE
420             CONTINUE
              ENDIF
C
C CHECK WITH RESPECT TO MAXCLK
              IF (DABS(OFFS(IEPOCO)) .GT. MAXCLK) THEN
                DO ISAT = 1,NSAT
                  IF (G_SVNSYS(NRSAT(ISAT)/100) .NE.'R') CYCLE
                  DO IFR = 1,NFREQ
                    CALL SETFLG(OBSFLG(ISAT,IFR),0)
                    IF (DABS(OFFS(IEPOCO)) .GT. DABS(CLKBAD)) THEN
                      CLKBAD=OFFS(IEPOCO)
                      IEPBAD=IEPOCO
                    ENDIF
                  ENDDO
                ENDDO
              ENDIF
C
              CALL WTOBSI(LFNLOC,IFRMAT,NFREQ,OBSTIM,DELTAT,EPOFLG,
     1                    NSAT,NRSAT,OBSFLG,OBSERV)
C
C Put clock into clock rinex record
C ---------------------------------
              IF (ClkHed%NumTyp /= 0.AND.offs(iepoco) /= 0d0) THEN
                CALL OSAMPL(OBSTIM,SAMPL,1D0,TSAMPL,TPREV,TNEXT)
                DO jEpo = 1,SIZE(clkrec%epoch)
                  IF (DABS(CLkHed%TFirst+ClkRec%Epoch(jEpo)/86400d0 -
     1                     TSAMPL) < 0.1d0/86400d0) THEN
                    ClkRec%Clock(iclk,jepo)=-(deltat(1)+deltat(2))
                    IF (ClkRec%Sigma(iClk,jEpo).NE.UNDEF)
     1                ClkRec%Sigma(iClk,jEpo)=
     2                                DSQRT(ClkRec%Sigma(iClk,jEpo))
                  EXIT
                  ENDIF
                ENDDO
              ENDIF
C
            ELSE
C
C WRITE PHASE OBSERVATION TO SCRATCH FILE (NO CL. OFFSET AVAILABLE)
C
C MARK ALL OBSERVATIONS OF CURRENT EPOCH
              DO 460 ISAT = 1,NSAT
                DO 465 IFR = 1,NFREQ
                  CALL SETFLG(OBSFLG(ISAT,IFR),0)
465             CONTINUE
460           CONTINUE
C
C WRITE PHASE OBSERVATIONS TO SCRATCH FILE
              CALL WTOBSI(LFNLOC,IFRMAT,NFREQ,OBSTIM,DELTAT,EPOFLG,
     1                    NSAT,NRSAT,OBSFLG,OBSERV)
C
            ENDIF
C
500       CONTINUE
C
          IF (DABS(CLKBAD).GT.0.D0) THEN
            WRITE(LFNERR,
     1        '(/,A,A,/,16X,A,F12.9,A,/,16X,A,F12.9,A,I6,/,16X,A,A,/)')
     2        ' ### SR CDSCLK: GLONASS phase observations marked ',
     3        'due to clock offsets',
     4        'exceeding ',MAXCLK,' sec',
     5        'reaching ',CLKBAD,' sec at epoch ',IEPBAD,
     6        'Station name: ',STANAM(1)
          ENDIF
C
        ENDIF
C
510     REWIND LFN001
        REWIND LFNLOC
C
C RECOUNT NUMB. OF VALID OBS. IN PHASE FILE
C (CLK FOR EACH EPOCH AND SAMPL./=1 ==> ALL OBS. WITHOUT CLK WILL BE MARKED)
        DO ISATEL=1,NSATEL
          DO IFRQ=1,NFREQ
            NUMMRK(ISATEL,IFRQ)=0
            NUMOBS(ISATEL,IFRQ)=0
          ENDDO
        ENDDO
C
C COPY SCRATCH FILE TO OBSERVATION FILE
        DO 550 IEPO = 1,NEPOCH
          CALL RDOBSI(LFNLOC,IFRMAT,NFREQ,FILFRQ,OBSTIM,DELTAT,EPOFLG,
     1                NSAT,NRSAT,OBSFLG,OBSERV,IRETRN)
          IF(IRETRN.NE.0) GOTO 560
C
C MARK PHASE OBS. IF  OUTLIERS
          DO ISAT=1,NSAT
            DO ISATEL=1,NSATEL
              IF (NUMSAP(ISATEL).EQ. NRSAT(ISAT)) THEN
                IF (IOUTSV.EQ.2 .OR. IOUTSV.EQ.3) THEN
                  JEPO=NINT((OBSTIM-TIMREC)*86400D0/IDELTT)+1
                  DO IDEL=1,NDEL
                    IF (ABS(LSTDEL(5,IDEL)).EQ.1         .AND.
     1                  IFILE       .EQ. LSTDEL(4,IDEL)  .AND.
     2                  JEPO        .GE. LSTDEL(2,IDEL)  .AND.
     3                  JEPO        .LE. LSTDEL(3,IDEL)  .AND.
     4                  NRSAT(ISAT) .EQ. LSTDEL(1,IDEL)) THEN
                      DO IFRQ=1,NFREQ
                        CALL SETFLG(OBSFLG(ISAT,IFRQ),0)
                      ENDDO
                    ENDIF
                  ENDDO
                ENDIF
                DO IFRQ=1,NFREQ
                  IF (OBSERV(ISAT,IFRQ).EQ.0D0) CYCLE
                  IF (TSTFLG(OBSFLG(ISAT,IFRQ),0)) THEN
                    NUMMRK(ISATEL,IFRQ)=NUMMRK(ISATEL,IFRQ)+1
                  ELSE
                    NUMOBS(ISATEL,IFRQ)=NUMOBS(ISATEL,IFRQ)+1
                  ENDIF
                ENDDO
              ENDIF
            ENDDO
          ENDDO
C
          CALL WTOBSI(LFN001,IFRMAT,NFREQ,OBSTIM,DELTAT,EPOFLG,
     1                NSAT,NRSAT,OBSFLG,OBSERV)
550     CONTINUE
C
560     CLOSE (UNIT=LFN001)
        CLOSE (UNIT=LFNLOC,STATUS='DELETE')
C
C WRITE NEW PHASE HEADER
        CALL WTHEAD(PHAHED,MEATYP,NDIFF,NFREQ,NEPOCH,NSATEL,
     1              CSESS,IDELTT,TIMREF,CAMPGN,TITLE,CRDATE,
     2              CRTIME,IRMARK,NEPFLG,IFRMAT,
     3              STANAM,RECTYP,ANTTYP,IRUNIT,IANTEN,
     4              OPRNAM,POSECC,ICLOCK,NUMSAP,NUMOBS,NUMMRK,
     5              NUMAMB,AMBSAT,AMBIEP,AMBWLF,AMBIGU,AMBCLS,
     6              usegeos,gobsdef)
C
      ENDIF
C
      IF (ASSOCIATED(GOBSDEF%SAT)) THEN
          DEALLOCATE(GOBSDEF%SAT,STAT=IRC)
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
