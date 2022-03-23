C*
      PROGRAM RNXGRA
CC
CC NAME       :  RNXGRA
CC
CC PURPOSE    :  CREATE GRAPHIC TABLE OF RINEX OBSERVATIONS
CC
CC REMARKS    :  MAXFIL .GE. MAXSAT
CC
CC AUTHOR     :  L.MERVART
CC
CC CREATED    :  92/01/14 13:59
CC
CC CHANGES    :  24-JAN-92 : ??: STATION NAME TRANSLATION
CC                               TABLE ADDED
CC               19-JUL-92 : ??: NEW SUMMARY
CC               24-SEP-92 : ??: CHECK AS
CC                6-OCT-92 : ??: ERROR MESSAGE IF EPOCH FLAG OF R2RDOR > 1
CC                4-FEB-93 : ??: IASGRA,INTGRA INTEGER*4, COMMON RLARGE
CC               23-NOV-93 : ??: HANDLE EVENT FLAG 4
CC               14-APR-94 : ??: PRINT NUMBER OF SATELLITES
CC               10-AUG-94 : MR: CALL EXITRC
CC               20-SEP-94 : MR: ADD PARAMETER "istops" TO CALL GTSTNA
CC               15-MAR-95 : RW: INCREASE MAXFIL TO 70
CC               10-APR-95 : MR: IDUMY4 FOR IABS
CC               12-JUL-96 : TS: CHANGED CALL OF GTSTNA
CC               19-JUL-96 : TS: INCREASE MAXFIL TO 200
CC               08-MAY-98 : TS: USE FPARSE TO WRITE 4-CHAR FILENAME IN SUMMARY
CC               02-NOV-98 : TS: SAVE IASC AND IASP
CC               10-FEB-99 : TS: MAXSAT IN INCLUDED FILE (AND INCREASED TO 48)
CC               13-APR-99 : DI: MAXSAT SET TO 124 (GLONASS)
CC               14-APR-99 : MR: CHECK FOR ITYP1,ITYP2=0
CC               15-APR-99 : DI: SEPARATE GPS AND GLONASS SITE STATISTICS
CC               12-JUL-99 : SS: "STASTR" IN CALL OF SR GTSTNA
CC               08-OCT-99 : TS/SS: "OBSTYP" IN CALL OF SR R2RDOR
CC               02-DEC-99 : SS: CONSIDER "T0" TO COMPUTE "ISEC"
CC               25-APR-00 : RD: TAKE ONE REFERENCE EPOCH FOR ALL RINEX FILES
CC               01-NOV-00 : CU: SWITCH TO THE NEW MENU SYSTEM
CC               21-NOV-00 : RD: GET LIST OF GOOD/BAD FILES
CC               17-JAN-01 : RD: GRAPHIC IF FILE STARTED OUTSIDE THE FIGURE
CC               06-JUN-01 : MR: USE "SVNSYS" INSTEAD OF "MIXSV1"
CC               14-AUG-01 : RD: SEPARATE DIRECTORIES FOR EVERY FILE TYPE
CC               22-OCT-01 : RD: NEW SR GTSTNA USING STATION INFO FILE
CC               22-OCT-01 : RD: WRITE THE TRANSLATED STATION NAME TO THE OUTPUT
CC               23-OCT-01 : RD: NEW TITLE SECTION
CC               15-JAN-01 : MM: DIMTST (MAXTYP)
CC               14-AUG-02 : RD: Tolerance for missing epochs
CC               27-FEB-02 : RD: ERROR IF NO FILES ARE SELECTED
CC               16-MAY-03 : HB: INITIALIZE STRUCTURE
CC               30-JUN-03 : HB: ADOPT TO CHANGES IN SR R2RDOH (SPACEBORNE)
CC               09-JUL-03 : RD: READ STAINFO FOR GTSTNA IN RXGINPT
CC               18-AUG-03 : RD: CORRECT FILE HANDLING
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               16-FEB-04 : RD: NEW OPTIONS: PRINT CYCLE SLIP, MIN S/N-RATIO
CC               21-JUN-05 : MM: LFNUM.inc, COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               16-AUG-06 : HU: MAXTYP 10 -> 12
CC               27-FEB-07 : AG: CALL DEFCON
CC               23-MAY-07 : AG: GALILEO AND SBAS ADDED
CC               19-JUL-10 : SL: TAB CHARACTERS REMOVED
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               27-OCT-10 : SL: USE M_BERN WITH ONLY
CC               01-DEC-11 : SL: NEW TITLE STRING FOR PRITIT
CC               01-FEB-12 : RD: HANDLE MORE THAN 10 OBSERVATION TYPES
CC               28-MAR-12 : RD: USE SVNSYS AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1992     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
      USE M_BERN,   ONLY: lfnErr, lfn001, lfnPrt, fileNameLength
      USE m_cpu,    ONLY: cpu_start
      USE D_STACRX, ONLY: t_staCrux, init_staCrux
      USE D_INPKEY, ONLY: inpKey, init_inpkey
      USE P_RNXGRA, ONLY: MAXFIL, MAXCHR, TIMCHR, MAXCOM,
     1                    MAXTYP, MAXSAT, MAXREC, t_rnxgra_opt
C*
      USE s_dimtst
      USE s_opnfil
      USE s_gtstna
      USE s_gtfile
      USE s_r2rdoh
      USE s_pritit
      USE s_readinpf
      USE s_rxginp
      USE s_opnerr
      USE s_r2rdor
      USE s_prfile
      USE s_fparse
      USE s_exitrc
      USE s_rxglst
      USE s_opnsys
      USE s_defcon
      USE s_jmt
      USE s_gtflna
      USE f_djul
      USE f_tstflg
      USE f_svnsys

      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I      , IANTE4 , IANTEN , IASC   , IASCSAV, IASMAX ,
     1          IASP   , IASPSAV, IBEG   , ICHR   , ID     , IDELTT ,
     2          IDTRN4 , IDUMY4 , IEND   , IFIL   , IFLAG  , IFR    ,
     3          IMID   , IOBCHR , IOBMAX , IP1    , IP2    , IPOS   ,
     4          IRC    , IRCHE4 , IRCOD2 , IRCODE , IREC   , IRETC  ,
     5          IRUNI4 , IRUNIT , IRXVR4 , IRXVRS , IS     , ISAT   ,
     6          ISEC   , ISNEG  , ISPOS  , ISTA   , ISYS   , ITP    ,
     7          ITYP   , ITYP1  , ITYP2  , J      , J0     , JFIL   ,
     8          K      , KK     , M0     , MFIL   , NCOM   , NCOM4  ,
     9          NFIL   , NFLCOL , NSATE4 , NSATEL , NSATEP , NSUM   ,
     1          NUMLIN , NUMTY4 , NUMTYP , NWLSA4 , NWLSAT , NTYP
C
      REAL*8    D0     ,EPOCH(1), T0     , TFIRS4 , TFIRST ,
     1          TLAST  , TLAST4
C
CCC       IMPLICIT  REAL*8  (A-H,O-Z)
C
!
! parameters are defined in p_rnxgra now
! to make sure that they are equal in any SR
! ------------------------------------------
!      PARAMETER (MAXSAT=350)
!
!      PARAMETER (MAXFIL=200,MAXCHR=72,MAXCOM=10,
!     1           MAXTYP= 12,
!     2           MAXREC=1000000,TIMCHR=1200D0)
!
! MAXFIL: MAXIMUM NUMBER OF INPUT FILES
! MAXCHR: MAXIMUM NUMBER OF CHARACTERS
! TIMCHR: NUMBER OF SECONDS PER 1 CHARACTER
! MAXCOM: MAXIMUM NUMBER OF COMMENT LINES
! MAXTYP: MAXIMUM NUMBER OF RINEX OBSERVATION TYPES
! MAXSAT: MAXIMUM NUMBER OF SATELLITES
! MAXREC: MAXIMUM NUMBER OF RECORDS IN ONE FILE
C
C
C DECLARATIONS
C ------------
      TYPE(t_stacrux) staCrux
      CHARACTER*1  GRASTR(MAXSAT,MAXCHR),TEXTG2(9)
      CHARACTER*20 STAGRA(MAXFIL)
      CHARACTER(LEN=fileNameLength) :: FILNAM(MAXFIL),SMTFIL(MAXFIL)
      CHARACTER(LEN=fileNameLength) :: SUMFIL
      INTEGER*4    ITIME(3),CYCGRA(MAXSAT,MAXCHR)
      REAL*8       TEPOCH(MAXFIL)
      LOGICAL*4    INSERT,FLGSYS(4)
      TYPE(t_rnxgra_opt) :: opt
C
C FPARSE
      CHARACTER*32 NODE, DEVICE, DIR, NAME, EXT, VER
C
      INTEGER*4    INTGRA(MAXFIL,MAXSAT,MAXCHR)
      INTEGER*4    IASGRA(MAXFIL,MAXSAT,MAXCHR)
      INTEGER*4    LFNSUM(2),SATNUM
C
C  OPNFIL
      INTEGER*4    IOSTAT
C  R2RDOH
      CHARACTER*20 PRGNAM,RUNBY,RCVERS,ANTTYP,RECTYP,OPRNAM
      CHARACTER    AGENCY*40,CRDATE*9,CRTIME*5,STANAM*60
      CHARACTER    STANUM*40,OBSTYP(MAXTYP)*2,COMENT(MAXCOM)*80
      REAL*8       POSXYZ(3),POSECC(3,3)
      INTEGER*4    IWLFAC(2),NUMSAT(MAXSAT)
      INTEGER*4    IWLSAT(3,MAXSAT),NUMOBS(MAXSAT,MAXTYP)
C
      CHARACTER*20 PRGNA4,RUNBY4,RCVER4,ANTTY4,RECTY4,OPRNA4
      CHARACTER*40 AGENC4
      CHARACTER    CRDAT4*9,CRTIM4*5,COMEN4(MAXCOM)*80,OBSTY4(MAXTYP)*2
      CHARACTER    STANA4*60,STANU4*40
      REAL*8       POSXY4(3),POSAN4(3,3)
      INTEGER*4    IWLRN4(2),NUMSA4(MAXSAT),NUMOB4(MAXSAT,MAXTYP)
      INTEGER*4    IWLSA4(3,MAXSAT)
C
C  R2RDOR
      INTEGER*4    SATEPO(MAXSAT),LLI(MAXSAT,MAXTYP)
      INTEGER*4    ISIGN(MAXSAT,MAXTYP)
      REAL*8       OBSEPO(MAXSAT,MAXTYP)
C
C COMMONS
C -------
C
      COMMON/RLARGE/ INTGRA,IASGRA
C
C DATA
C ----
      DATA TEXTG2/'1','2','3','4','5','6','7','8','9'/

C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C NULLIFY POINTER
C ---------------
      CALL INIT_STACRUX(staCrux)
      CALL INIT_INPKEY(inpKey)
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
C GENERATE THE TITLE
C ------------------
      CALL PRITIT('RNXGRA','Create observation statistics')
C
C READ OPTIONS
C ------------
      CALL rxginp(opt,staCrux)
C     CALL RXGOPT(iopt,istops)
C
C READ NAMES OF INPUT FILES
C -------------------------
      NFLCOL=1
      CALL GTFILE('OBSFIL ',NFLCOL,MAXFIL,NFIL,FILNAM)
      NFLCOL=1
      CALL GTFILE('SMTFIL ',NFLCOL,MAXFIL,MFIL,SMTFIL)
C
C PRINT THE FILE NAMES INTO THE OUTPUT
C ------------------------------------
      IF (NFIL.GT.0) CALL PRFILE ('OBSFIL ',' ',NFLCOL)
      IF (MFIL.GT.0) CALL PRFILE ('SMTFIL ',' ',NFLCOL)
C
      CALL DIMTST(1,2,1,'RNXGRA','MAXFIL','NUMBER OF FILES',' ',
     1            NFIL+MFIL,MAXFIL,IRC)
C
      DO IFIL=1,MFIL
        FILNAM(NFIL+IFIL)=SMTFIL(IFIL)
      ENDDO
      NFIL=NFIL+MFIL
C
C NO INPUT FILES SELECTED
C -----------------------
      IF (NFIL.EQ.0) THEN
        WRITE(LFNERR,'(/,A,/)')
     1       ' ### PG RNXGRA: NO RINEX FILES SELECTED.'
        CALL EXITRC(0)
      ENDIF
C
C INITIALIZE VARIABLES
C --------------------
      DO ISYS=1,4
        FLGSYS(ISYS)=.FALSE.
      ENDDO
C
      DO IFIL = 1,MAXFIL
        DO ISAT = 1,MAXSAT
          DO ICHR = 1,MAXCHR
            INTGRA(IFIL,ISAT,ICHR) = 0
            IASGRA(IFIL,ISAT,ICHR) = 0
            GRASTR(IFIL,ICHR) = ' '
          ENDDO
        ENDDO
      ENDDO
C
C LOOP OVER ALL INPUT FILES TO FIND THE INITIAL EPOCH
C ---------------------------------------------------
      DO IFIL = 1,NFIL
C
        CALL OPNFIL(LFN001,FILNAM(IFIL),'OLD','FORMATTED',
     1              'READONLY',' ',IOSTAT)
C
        CALL OPNERR(LFNERR,LFN001,IOSTAT,FILNAM(IFIL),
     1              'RNXGRA')
C
C READ HEADER OF RINEX FILE
C -------------------------
        CALL R2RDOH(LFN001,LFNERR,MAXSAT,MAXCOM,0,
     1              PRGNAM,RUNBY,CRDATE,CRTIME,NCOM,COMENT,
     2              STANAM,STANUM,OPRNAM,AGENCY,
     3              IRUNIT,RECTYP,RCVERS,
     4              IANTEN,ANTTYP,POSXYZ,POSECC,
     5              IWLFAC,IWLSAT,NWLSAT,
     6              NUMTYP,OBSTYP,IDELTT,TFIRST,TLAST,
     7              NSATEL,NUMSAT,NUMOBS,IRXVRS,IRCODE)
C
        CLOSE(LFN001)
C
C CHECK DIMENSION OF MAXTYP
C -------------------------
        CALL DIMTST(1,2,2,'RNXGRA','MAXTYP',
     1       'NUMBER OF OBSERVATION TYPES',' ',NUMTYP,MAXTYP,IRC)
C
C SORT THE START EPOCHS
C ---------------------
        IF (IFIL.EQ.1) TEPOCH(1)=TFIRST
        INSERT=.TRUE.
        DO JFIL=IFIL,2,-1
          IF (TFIRST.LT.TEPOCH(JFIL-1)) THEN
            TEPOCH(JFIL)=TEPOCH(JFIL-1)
            TEPOCH(JFIL-1)=TFIRST
          ELSEIF (INSERT) THEN
            TEPOCH(JFIL)=TFIRST
            INSERT=.FALSE.
          ENDIF
        ENDDO
      ENDDO
C
C TAKE THE MEDIAN AS REFERENCE EPOCH FOR ALL FILES
C ------------------------------------------------
      IMID=NFIL/2
      IF (IMID.EQ.0) IMID=1
      CALL JMT(TEPOCH(IMID),J0,M0,D0)
      D0=DNINT(D0*3600.D0)/3600.D0
      T0=DJUL(J0,M0,D0)
      CALL JMT(T0,J0,M0,D0)
      ITIME(1)=NINT((D0-INT(D0))*24)
      DO J=1,2
        ITIME(J+1)=ITIME(1)+J*12
      ENDDO
C
C LOOP OVER ALL INPUT FILES TO MAKE THE GRAPHIC
C ---------------------------------------------
      DO 201 IFIL = 1,NFIL
C
        CALL OPNFIL(LFN001,FILNAM(IFIL),'OLD','FORMATTED',
     1              'READONLY',' ',IOSTAT)
C
        CALL OPNERR(LFNERR,LFN001,IOSTAT,FILNAM(IFIL),
     1              'RNXGRA')
C
C READ HEADER OF RINEX FILE
C -------------------------
        CALL R2RDOH(LFN001,LFNERR,MAXSAT,MAXCOM,0,
     1              PRGNAM,RUNBY,CRDATE,CRTIME,NCOM,COMENT,
     2              STANAM,STANUM,OPRNAM,AGENCY,
     3              IRUNIT,RECTYP,RCVERS,
     4              IANTEN,ANTTYP,POSXYZ,POSECC,
     5              IWLFAC,IWLSAT,NWLSAT,
     6              NUMTYP,OBSTYP,IDELTT,TFIRST,TLAST,
     7              NSATEL,NUMSAT,NUMOBS,IRXVRS,IRCODE)
C
C COMPUTE MAX. NUMBER OF OBSERVATIONS PER SATELLITE PER CHARACTER
C ---------------------------------------------------------------
        IOBCHR=0
        IF (IDELTT.NE.0) IOBCHR=NINT(TIMCHR/IDELTT)
C
        CALL gtstna(opt%istops,1,tfirst,1,(/ stanam /),(/ ' ' /),
     1              'STAINFO',staCrux,stagra(ifil:ifil))
C
C INIT CYCLE SLIP GRAPHIC RECORD
C ------------------------------
        IF (opt%cycgra.eq.1) CYCGRA(1:MAXSAT,1:MAXCHR) = 0
C
C WHICH COLUMNS ARE TO BE CHECKED ?
C --------------------------------
        ITYP1 = 0
        ITYP2 = 0
        IP1 = 0
        IP2 = 0
        IASP = 0
        IASC = 0
        IF (opt%iopt .GT. 0) THEN
          DO 400 ITYP = 1,NUMTYP
            IF (OBSTYP(ITYP) .EQ. 'L1') ITYP1 = ITYP
            IF (OBSTYP(ITYP) .EQ. 'L2') ITYP2 = ITYP
            IF (OBSTYP(ITYP) .EQ. 'P1') IASP  = ITYP
            IF (OBSTYP(ITYP) .EQ. 'C1') IASC  = ITYP
400       CONTINUE
        ELSE
          DO 401 ITYP = 1,NUMTYP
            IF (OBSTYP(ITYP) .EQ. 'P1') THEN
              ITYP1 = ITYP
              IASP  = ITYP
              IP1 = 1
            END IF
            IF (OBSTYP(ITYP) .EQ. 'P2') THEN
              ITYP2 = ITYP
              IP2 = 1
            END IF
            IF (OBSTYP(ITYP) .EQ. 'C1') IASC  = ITYP
401       CONTINUE
          IF (IP1 .EQ. 0) THEN
            DO 402 ITYP =1,NUMTYP
              IF (OBSTYP(ITYP) .EQ. 'C1') ITYP1 = ITYP
402         CONTINUE
          END IF
          IF (IP2 .EQ. 0) THEN
            DO 403 ITYP =1,NUMTYP
              IF (OBSTYP(ITYP) .EQ. 'C2') ITYP2 = ITYP
403         CONTINUE
          END IF
        END IF
        IF (IABS(opt%iopt) .EQ. 1) ITYP2 = ITYP1
        IF (IABS(opt%iopt) .EQ. 2) ITYP1 = ITYP2
C
C LOOP OVER ALL RECORDS (EPOCHS)
C ------------------------------
        IOBMAX = 0
        IASMAX = 0
C
        DO 202 IREC = 1,MAXREC
C
          DO ISAT = 1,MAXSAT
            SATEPO(ISAT) = 0
          ENDDO
C
          CALL R2RDOR(LFN001,LFNERR,MAXSAT,IRXVRS,
     1                NUMTYP,OBSTYP,EPOCH,IFLAG,NSATEP,SATEPO,
     2                OBSEPO,ISIGN,LLI,IRCOD2)
C
          IF (IRCOD2 .NE. 0) GOTO 205
C
C  EVENT FLAG 4
C  ------------
          IF(IFLAG.EQ.4) THEN
C
C  READ INSERTED HEADER RECORDS (INITIALIZE ALL VARIABLES)
            NUMLIN=-NSATEP
            CALL R2RDOH(LFN001,LFNERR,MAXSAT,MAXCOM,NUMLIN,
     1                  PRGNA4,RUNBY4,CRDAT4,CRTIM4,NCOM4,COMEN4,
     2                  STANA4,STANU4,OPRNA4,AGENC4,
     3                  IRUNI4,RECTY4,RCVER4,
     4                  IANTE4,ANTTY4,POSXY4,POSAN4,
     5                  IWLRN4,IWLSA4,NWLSA4,
     6                  NUMTY4,OBSTY4,IDTRN4,TFIRS4,TLAST4,
     7                  NSATE4,NUMSA4,NUMOB4,IRXVR4,IRCHE4)
            IF(IRCHE4.NE.0) GOTO 205
C
            IF(STANA4.NE.' '.AND.STANA4.NE.STANAM) THEN
              WRITE(LFNERR,1996) STANA4,FILNAM(IFIL)
1996          FORMAT(/,' *** PG RNXGRA: NEW SITE FOUND IN THE FILE:',
     1                    A20,/,16X,'PROCESSING STOPPED FOR FILE!',
     2                        /,16X,'RINEX FILE NAME: ',A32,/)
              GOTO 205
            END IF
C
            GOTO 202
          END IF
C
          DO ISYS=1,4
            FLGSYS(ISYS)=FLGSYS(ISYS).OR.SVNSYS(ISYS-1,NSATEP,SATEPO)
          ENDDO
          IF (IABS(opt%iopt) /= 1) FLGSYS(4) = .false.
C
          ISEC=IDNINT((EPOCH(1)-T0)*86400.D0)
          ICHR=IDINT(ISEC/TIMCHR)+1
          IF (ICHR.GT.MAXCHR) GOTO 205
          IF (ICHR.LE.0 .OR. ISEC.LT.0) GOTO 202
C
C LOOP OVER ALL SATELLITES
C ------------------------
          DO 203 ISAT = 1,MAXSAT
            IFLAG = 0
C
            DO IPOS = 1,MAXSAT
              IF (ISAT .EQ. SATEPO(IPOS)) THEN
                IFLAG = IPOS
              END IF
            ENDDO
C
            IF (IFLAG .NE. 0) THEN
C
C CHECK S/N-RATIO:
              IF (opt%minsig.EQ.0.OR.
     1            (ISIGN(IFLAG,ITYP1).GE.opt%minsig.AND.
     2             ISIGN(IFLAG,ITYP2).GE.opt%minsig)) THEN
                IF (ITYP1.NE.0 .AND. ITYP2.NE.0) THEN
                  IF((OBSEPO(IFLAG,ITYP1).NE.0).AND.
     1               (OBSEPO(IFLAG,ITYP2).NE.0)) THEN
                    INTGRA(IFIL,ISAT,ICHR)=INTGRA(IFIL,ISAT,ICHR)+1
                  END IF
                END IF
C
C  CHECK AS FLAG (BIT 2 IN LLI)
C  ----------------------------
                DO ITP=1,NUMTYP
                  IF(TSTFLG(CHAR(LLI(IFLAG,ITP)),2))
     1              IASGRA(IFIL,ISAT,ICHR)=1
                ENDDO
              END IF
C
C REMEMBER CYCLE SLIP FLAGS
              DO ITP=1,NUMTYP
                IF(TSTFLG(CHAR(LLI(IFLAG,ITP)),0)) CYCGRA(ISAT,ICHR)=1
              ENDDO
            END IF
C
            IF(INTGRA(IFIL,ISAT,ICHR).GT.IOBMAX)
     1         IOBMAX=INTGRA(IFIL,ISAT,ICHR)
203       CONTINUE
C
202     CONTINUE
C
205     CLOSE (UNIT=LFN001)
C
C USE THE THEORETICAL MAX. OBSERVATION VALUE COMPUTED FROM SAMPLING RATE
C ----------------------------------------------------------------------
        IF (IOBCHR.NE.0) IOBMAX=IOBCHR
C
        DO 500 ISAT = 1,MAXSAT
          DO ICHR = 1,MAXCHR
            IF (INTGRA(IFIL,ISAT,ICHR).NE.0) THEN
              IF (INTGRA(IFIL,ISAT,ICHR).GE.IOBMAX-opt%iobtol) THEN
                INTGRA(IFIL,ISAT,ICHR) = 1
                GRASTR(ISAT,ICHR) = '*'
              ELSE
                INTGRA(IFIL,ISAT,ICHR) = -1
                GRASTR(ISAT,ICHR) = '-'
              END  IF
              IF (opt%cycgra.EQ.1.AND.CYCGRA(ISAT,ICHR).EQ.1) THEN
                GRASTR(ISAT,ICHR) = 'C'
              ENDIF
            ELSE
              GRASTR(ISAT,ICHR) = ' '
            END IF
          ENDDO
500     CONTINUE
C
C PRINT GRAPHICS
C --------------
        ID=IDINT(D0)
C
        WRITE (LFNPRT,1001) J0,M0,ID,STAGRA(IFIL)
C
        NTYP=1
        DO WHILE(NTYP<=NUMTYP)
          WRITE (LFNPRT,1002) (OBSTYP(K),K=NTYP,MIN(NTYP+9,NUMTYP))
          NTYP=NTYP+10
        ENDDO
C
        IF (ITYP1 .NE. ITYP2) THEN
          WRITE (LFNPRT,1003) OBSTYP(ITYP1), OBSTYP(ITYP2)
        ELSE
          WRITE (LFNPRT,1003) OBSTYP(ITYP1)
        END IF
C
        DO ISAT = 1,MAXSAT
          IS = 0
          DO ICHR = 1,MAXCHR
            IDUMY4=INTGRA(IFIL,ISAT,ICHR)
            IS = IS + IABS(IDUMY4)
          ENDDO
          IF (IS .NE. 0) THEN
            WRITE (LFNPRT,1004) ISAT,(GRASTR(ISAT,KK),KK=1,MAXCHR)
          END IF
        ENDDO
C
        WRITE (LFNPRT,1005) (ITIME(J),J=1,3)
C
C SAVE IASP AND IASC
C
        IF (IASP.NE.0) IASPSAV = IASP
        IF (IASC.NE.0) IASCSAV = IASC
C
201   CONTINUE
C
C SUMMARIES
C ---------
C SPECIAL SUMMARY FILE
      CALL GTFLNA(0,'SUMMARY',SUMFIL,IRC)
      IF(IRC.EQ.0) THEN
        NSUM=2
        LFNSUM(2)=LFN001
        CALL OPNFIL(LFNSUM(2),SUMFIL,'NEW',' ',' ',' ',IOSTAT)
      ELSE
        NSUM=1
      END IF
      LFNSUM(1)=LFNPRT
C
      DO 1000 I=1,NSUM
C
C PRINT  SUMMARY GRAPHICS (SATELLITES)
C ------------------------------------
        WRITE (LFNSUM(I),1006) J0,M0,ID
C
        IF (opt%iopt .GT. 0) THEN
          WRITE (LFNSUM(I),1008)
        ELSE
         WRITE (LFNSUM(I),1009)
        END IF
        IF (IABS(opt%iopt) .NE. 3) THEN
          IFR = IABS(opt%iopt)
          WRITE (LFNSUM(I),1010) IFR
        ELSE
          WRITE (LFNSUM(I),1011)
        END IF
        SATNUM=0
C
        DO 801 ISAT = 1,MAXSAT
          ISTA = 0
          DO 802 ICHR = 1,MAXCHR
            ISPOS = 0
            ISNEG = 0
            DO IFIL = 1, NFIL
              IF (INTGRA(IFIL,ISAT,ICHR).EQ.+1) ISPOS = ISPOS + 1
              IF (INTGRA(IFIL,ISAT,ICHR).EQ.-1) ISNEG = ISNEG + 1
            ENDDO
            ISTA = ISTA + ISPOS + ISNEG
            IF (ISPOS .NE. 0)  THEN
              IF (ISPOS.LE.9) THEN
                 GRASTR(ISAT,ICHR) = TEXTG2(ISPOS)
              ELSE
                GRASTR(ISAT,ICHR) = '*'
              END IF
            ELSE
              IF (ISNEG .NE. 0) THEN
                GRASTR(ISAT,ICHR) = '-'
              ELSE
                GRASTR(ISAT,ICHR) = ' '
              END IF
            END IF
802       CONTINUE
          IF (ISTA.NE.0) THEN
             WRITE (LFNSUM(I),1004) ISAT,(GRASTR(ISAT,KK),KK=1,MAXCHR)
             SATNUM=SATNUM+1
          ENDIF
801     CONTINUE
C
        WRITE (LFNSUM(I),1005) (ITIME(J),J=1,3)
        WRITE (LFNSUM(I),1013) SATNUM
C
C PRINT  SUMMARY GRAPHICS (STATIONS)
C ---------------------------------
        WRITE (LFNSUM(I),1006) J0,M0,ID
C
        IF (opt%iopt .GT. 0) THEN
          WRITE (LFNSUM(I),1008)
        ELSE
          WRITE (LFNSUM(I),1009)
        END IF
        IF (IABS(opt%iopt) .NE. 3) THEN
          IFR = IABS(opt%iopt)
          WRITE (LFNSUM(I),1010) IFR
        ELSE
          WRITE (LFNSUM(I),1011)
        END IF
C
        DO 700 ISYS = 1,4
          IF (FLGSYS(ISYS)) THEN
            IF (ISYS.EQ.1) THEN
              IBEG=1
              IEND=99
              WRITE (LFNSUM(I),1014)
            ELSEIF (ISYS.EQ.2) THEN
              IBEG=100
              IEND=199
              WRITE (LFNSUM(I),1015)
            ELSEIF (ISYS.EQ.3) THEN
              IBEG=200
              IEND=299
              WRITE (LFNSUM(I),1016)
            ELSEIF (ISYS.EQ.4) THEN
              IBEG=300
              IEND=MAXSAT
              WRITE (LFNSUM(I),1017)
            ENDIF
C
            DO 701 IFIL = 1,NFIL
              DO ICHR = 1,MAXCHR
                ISPOS = 0
                ISNEG = 0
                DO ISAT = IBEG,IEND
                  IF (INTGRA(IFIL,ISAT,ICHR).EQ.+1) ISPOS = ISPOS + 1
                  IF (INTGRA(IFIL,ISAT,ICHR).EQ.-1) ISNEG = ISNEG + 1
                ENDDO
                IF (ISPOS .NE. 0)  THEN
                  IF (ISPOS.LE.9) THEN
                    GRASTR(IFIL,ICHR) = TEXTG2(ISPOS)
                  ELSE
                    GRASTR(IFIL,ICHR) = '*'
                  END IF
                ELSE
                  IF (ISNEG .NE. 0) THEN
                    GRASTR(IFIL,ICHR) = '-'
                  ELSE
                    GRASTR(IFIL,ICHR) = ' '
                  END IF
                END IF
              ENDDO
              CALL FPARSE(1,FILNAM(IFIL),NODE,DEVICE,DIR,NAME,EXT,
     1                    VER,IRETC)
              WRITE (LFNSUM(I),1007) NAME(1:4),(GRASTR(IFIL,KK),
     1                            KK = 1,MAXCHR)
701         CONTINUE
C
            WRITE (LFNSUM(I),1005) (ITIME(J),J=1,3)
          ENDIF
C
700     CONTINUE
C
C
C PRINT AS CHECKING
C -----------------
        IF (IASCSAV.NE.0 .AND. IASPSAV.NE.0) THEN
          WRITE (LFNSUM(I),1012)
          DO 901 ISAT = 1,MAXSAT
            ISTA = 0
            DO 902 ICHR = 1,MAXCHR
              ISPOS = 0
              ISNEG = 0
              DO IFIL = 1, NFIL
                IF (IASGRA(IFIL,ISAT,ICHR).EQ.1) ISPOS = ISPOS + 1
              ENDDO
              ISTA = ISTA + ISPOS
              IF (ISPOS .NE. 0)  THEN
                IF (ISPOS.LE.9) THEN
                   GRASTR(ISAT,ICHR) = TEXTG2(ISPOS)
                ELSE
                  GRASTR(ISAT,ICHR) = '*'
                END IF
              ELSE
                IF (ISNEG .NE. 0) THEN
                  GRASTR(ISAT,ICHR) = '-'
                ELSE
                  GRASTR(ISAT,ICHR) = ' '
                END IF
              END IF
902         CONTINUE
            IF (ISTA.NE.0)
     1        WRITE (LFNSUM(I),1004) ISAT,(GRASTR(ISAT,KK),KK=1,MAXCHR)
901       CONTINUE
C
          WRITE (LFNSUM(I),1005) (ITIME(J),J=1,3)
C
        END IF
C
1000  CONTINUE
C
      IF(NSUM.EQ.2) CLOSE(UNIT=LFNSUM(2))
C
C GET A LIST OF FILES
C -------------------
      IF (opt%getlst == 1) CALL RXGLST(NFIL,FILNAM,INTGRA,OPT)
C
C FORMATS
C -------
1001  FORMAT (' DATE : ',3I4,'   STATION : ',A20,/)
1002  FORMAT (1X,'TYPES OF OBSERVATIONS : ',10A4)
1003  FORMAT (1X,'CHECKED OBSERVATIONS  : ',2A4,/)
1004  FORMAT (1X,I4,' |',72A)
1005  FORMAT (1X,5('-'),'+',4(17('-'),'+')/
     1        1X,4(' '),I2,34(' '),I2, 34(' '),I2,/)
1006  FORMAT (' DATE : ',3I4,/)
1007  FORMAT (1X,A4,' |',72A)
1008  FORMAT (1X,'PHASE OBSERVATIONS')
1009  FORMAT (1X,'CODE OBSERVATIONS')
1010  FORMAT (1X,'FREQUENCY : ',I4,/)
1011  FORMAT (1X,'BOTH FREQUENCIES',/)
1012  FORMAT (1X,'ANTI-SPOOFING :',/)
1013  FORMAT (/1X,'NUMBER OF SATELLITES INCLUDED IN DATA FILES: ',I2/)
1014  FORMAT (1X,'GPS SATELLITES :',/)
1015  FORMAT (1X,'GLONASS SATELLITES :',/)
1016  FORMAT (1X,'GALILEO SATELLITES :',/)
1017  FORMAT (1X,'SBAS SATELLITES :',/)
C
      CALL EXITRC(0)
      END
