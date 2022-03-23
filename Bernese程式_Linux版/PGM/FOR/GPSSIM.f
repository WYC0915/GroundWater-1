C*
      PROGRAM GPSSIM
CC
CC NAME       :  GPSSIM
CC
CC PURPOSE    :  CREATE PSEUDORANGE FILES AND PHASE FILES FOR BERNESE
CC               GPS SOFTWARE SYSTEM VERSION 3.2.
CC               METEO FILES MAY ALSO BE CREATED
CC
CC REMARKS    :  ---
CC
CC AUTHORS    :  G.BEUTLER, M.ROTHACHER
CC
CC VERSION    :  3.5  (AUG 93)
CC
CC CREATED    :  88/02/23 20:00
CC
CC CHANGES    :  04-JUN-92 : ??: NEW PARAMETER IN CALL "ORBINF"
CC               09-DEC-92 : EB: NEW COMMON MCMSTA
CC               03-MAR-93 : ??: NEW PARAMETERS "ICORR","ZEN" IN GPHECC
CC                               NEW PARAMETERS "RECTYP","ANTTYP","IANTEN"
CC                               IN SMPRNG
CC               27-AUG-93 : ??: NEW FORMAT, VERSION 3.5
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               08-APR-94 : MR: ADD PARAM. "AZI" TO CALL OF GPHECC
CC               10-AUG-94 : MR: CALL EXITRC
CC               14-AUG-94 : MR: FORMAT 4: SESSION AS CHARACTER*4
CC               26-MAR-96 : MR: ADD "CSESS" TO CALL GPHECC AND SMPRNG
CC               24-SEP-97 : DI: USE MAXSAT.inc
CC               23-OCT-97 : SS: ELEVATION-DEPENDENT SIGMAS
CC               05-AUG-99 : SS: USE SAME CORE MODELING MODULE (SR
CC                               PRANGE) AS USED BY GPSEST
CC               07-AUG-99 : MR: SET GLONASS FREQUENCIES
CC               05-NOV-99 : SS: "IEXTRA=-1"
CC               22-JUN-00 : RD: INCREASED MAXEPO (OLD=4000)
CC               28-JUN-00 : RD: >1 AMBIGUITY PER SATELLITE AND STATION
CC               13-SEP-00 : RD: INCREASED MAXEPO (OLD=5000)
CC               30-JAN-01 : DS/MS: LEO SIMULATION
CC               25-MAY-01 : DS: MAXFIL=60 -> MAXFIL=140
CC               30-SEP-01 : DS: WHITE NOISE FOR SPACEBORNE LEO RECEIVER
CC               30-SEP-01 : DS: ELEV. DEP. WEIGHTING FOR LEO
CC               01-JUN-01 : HB: SWITCH TO NEW MENU SYSTEM
CC               15-JUN-01 : HB: rename d_stacrux in d_stacrx
CC               15-DEC-01 : HU: USE D_CONST
CC               17-DEC-01 : MM: Use new RECEIVER file format (antecc removed)
CC               23-FEB-02 : HU: Read mxosvn
CC               07-MAY-02 : SS: DCB UPDATE
CC               27-JUN-02 : RD/DS: MERGING VERSIONS BE AND MUNICH
CC               06-AUG-02 : SS: MAKE USE OF ICLASS
CC               25-SEP-02 : HU: REMOVE I_ASTLIB
CC               19-NOV-02 : HB: UPDATE PARAMETER LIST OF SMPRNG (METEO-FILE)
CC               20-NOV-02 : HB: METEO-FILE SIMULATION REMOVED
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               18-MAY-03 : HU: INITIALIZE STRUCTURE
CC               28-MAY-03 : RD: NEW CALL OF SR GPHECC
CC               23-JUN-03 : HB: INTERFACE FOR SR STAFLG
CC               11-AUG-03 : RS: RECTYP=' ' IN CALL OF GPHECC
CC               08-SEP-03 : HU: OPRNAM REMOVED FROM COMMON LARGE1
CC               13-SEP-03 : HU: INTERFACE FOR DEFREQ
CC               19-SEP-03 : RD: PUT OPT%IONO INTO SR SMPRNG
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               24-NOV-03 : HU: CHECK IONO ON/OFF FLAG
CC               15-JAN-04 : HU: USE IONO(3) AS FLAG
CC               29-MAR-04 : CU: ADD DUMMY VARIABLE TO CALL OF SR METEO
CC               24-MAY-04 : HU: OBSERV. HEADER FORMAT 5
CC               28-JUN-04 : RD: USE MAXSTA FROM M_MAXDIM
CC               21-JUN-05 : MM: LFNUM.inc REMOVED (LFNUMs IN M_BERN)
CC               23-JUN-05 : MM: COMMON STATEMENT CORRECTED (LARGE1)
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               27-FEB-07 : AG: CALL DEFCON WITH PARAMETER
CC               12-JUN-07 : AG: USE INIT_BUF INSTEAD OF GPHECC
CC               21-JUN-07 : SS: CALL GETRCV WITH ISYST
CC               01-NOV-07 : HB: NEW PARAMETER SECIPL FOR GTSCLK ADDED
CC               04-MAY-09 : RD: CONSIDER VIENNA GRID FILES IN XYZTIM
CC               04-MAY-09 : RD: CHANGE OPT%TFIRST/TLAST TO OPT%OBSWIN
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               27-Oct-10 : SL: USE M_BERN WITH ONLY, REMOVAL OF UNUSED MODULES
CC               27-JAN-11 : RD: GETSTA CAN BE USED AS A MODULE NOW
CC               03-FEB-11 : CR: ADD SWITCH FOR PERIODIC REL. J2-CORRECTION
CC               17-FEB-11 : RD: COMMON MCMSTA NOT NEEDED ANYMORE
CC               15-SEP-11 : RD: USE THE COORDINATES CORRECTLY
CC               14-NOV-11 : SL: NEW TITLE STRING FOR PRITIT
CC               29-FEB-12 : RD: USE METEO AS MODULE
CC               05-MAR-12 : RD: REMOVE UNUSED VARIABLES FROM SMPRNG
CC               20-AUG-12 : SL: SHAPE OF METEO PARAMETER CHANGED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
C*
      USE m_bern,   ONLY: lfnErr
      USE m_cpu,    ONLY: cpu_start
      USE m_maxdim, ONLY: maxsat, maxsta
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE d_stacrx, ONLY: MTypeSPACE
      USE d_phaecc, ONLY: init_buf
      USE d_grid,   ONLY: initGridBuffer,prtGridInfo
      USE p_gpssim, ONLY: t_gpssim_opt, init_gpssim_opt
      USE s_sminpn
      USE s_smwtph
      USE s_pritit
      USE s_staflg
      USE s_smprng
      USE s_orbinf
      USE s_defreq
      USE s_defcon
      USE s_opnsys
      USE s_xyzell
      USE s_prflna
      USE s_smwtcd
      USE s_getrcv
      USE s_readinpf
      USE s_getsta
      USE s_exitrc
      USE s_smclki
      USE s_meteo
      USE s_sminpf

      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I0    , ICLASS, IFRMAT, IFRQ  , INDION,
     1          IORSYS, ISAT  , IST   , ISTA  , MAXARC, MAXCLK,
     2          MAXEPO, MAXFIL, MXCARC, MXCCLK, MXCEPO, MXCFIL, MXCSAT,
     3          NARC  , NCENTR, NEPO  , NFREQ , NFRQCD, NFRQPH,
     4          NSATEL, NSTTOT, ISYST , IREL2
C
      REAL*8    AELL  , BELL  , FILCOD, FILPHA, SCELL ,
     1          STASES
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C MAXIMUM DIMENSIONS
C ------------------
      PARAMETER (MAXFIL=140,MAXARC=20,MAXEPO=21000,MAXCLK=10)
C
C
C MAXFIL : MAXIMUM NUMBER OF FILES (STATIONS)
C MAXSAT : MAXIMUM NUMBER OF SATELLITES
C MAXARC : MAXIMUM NUMBER OF ARCS
C MAXEPO : MAXIMUM NUMBER OF EPOCHS
C MAXCLK : MAXIMUM NUMBER OF CLOCK PARAMETERS
C MAXSTA : MAXIMUM NUMBER OF STATIONS
C
C DECLARATIONS
C ------------
      TYPE(t_gpssim_opt) :: opt
      CHARACTER*20 MARTYP
      CHARACTER*16 CDMY1(1),DATUM
      CHARACTER*6  MXNEPO,MXNARC,MXNSAT,MXNCLK,MXNFIL
      CHARACTER*1  SOURCE(10,MAXARC)
C
      REAL*8 XSTAT(3,MAXFIL),XSTELL(3,MAXFIL)
      REAL*8 CLOCK(MAXCLK,MAXFIL),PRANG(MAXEPO,2,MAXSAT,2)
      REAL*8 XSTECC(3,MAXFIL),DXELL(3),DRELL(3),TBOUND(2,MAXARC)
      REAL*8 WGSSES(3,2,MAXFIL)
      REAL*8 ELLSES(3,2,MAXFIL)
      REAL*8 ZENDIS(MAXEPO,MAXSAT)
      REAL*8 DUMMY3(3,1),DUMMY(1)
C
      INTEGER*4 SVN(MAXSAT),NOBSVN(MAXSAT),STANUM(MAXFIL)
      INTEGER*4 ICENTR(MAXFIL),ARCINT(MAXARC),NAVNUM(MAXSAT*MAXARC)
      INTEGER*4 NCLOCK(MAXFIL),NUMSAT(MAXARC)
      INTEGER*4 ICODE(2),IWLF(2),IFLAG
C
C COMMON BLOCKS
C -------------
      COMMON/LARGE1/ FILCOD,FILPHA,STASES,XSTAT,XSTELL,
     1               CLOCK,PRANG,XSTECC,NAVNUM,WGSSES,ELLSES,ZENDIS
      COMMON/MCMFIL/MXCFIL,MXNFIL
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMARC/MXCARC,MXNARC
      COMMON/MCMEPO/MXCEPO,MXNEPO
      COMMON/MCMCLK/MXCCLK,MXNCLK
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C INITIALIZE COMMON BLOCKS FOR MAXIMAL DIMENSIONS
C -----------------------------------------------
      MXCFIL=MAXFIL
      MXNFIL='MAXFIL'
      MXCSAT=MAXSAT
      MXNSAT='MAXSAT'
      MXCEPO=MAXEPO
      MXNEPO='MAXEPO'
      MXCARC=MAXARC
      MXNARC='MAXARC'
      MXCCLK=MAXCLK
      MXNCLK='MAXCLK'
C
C DEFINE LOGICAL FILE NUMBERS
C ---------------------------
C
C INITIALIZE STRUCTURES
C ---------------------
      CALL init_gpssim_opt(opt)
      CALL init_inpkey(inpKey)
C
C GET THE NAME OF THE INPUT FILE
C ------------------------------
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
C WRITE TITLE AND INPUT FILE NAMES
C --------------------------------
      CALL PRITIT('GPSSIM','Generate simulated observation data')
      CALL PRFLNA
C
C FILE FORMAT NUMBER
C ------------------
      IFRMAT=5
C
C READ ALL INPUT INFORMATION FROM INPUT FILE
C ------------------------------------------
      CALL sminpn(opt)
C
C READ ALL OUTPUT FILE NAMES AND STATION INFORMATION
C --------------------------------------------------
      CALL sminpf(opt)
C
C OPEN CORRECT METEO FILES
C ------------------------
      CALL METEO(1,opt%itropo,opt%iextra,opt%nsta,opt%stanam(1:1),0,
     1           0.D0,DUMMY3,DUMMY,DUMMY,0.D0,DUMMY,DUMMY,DUMMY,DUMMY)
C
C CHECK, IF IONOSP. REFERENCE STATION IS INCLUDED IN THE STATION LIST
C -------------------------------------------------------------------
      IF (opt%iono(3).NE.0) THEN
        DO 10 ISTA=1,opt%nsta
          IF (opt%stanam(ISTA).EQ.opt%ionref) THEN
            INDION=ISTA
            NSTTOT=opt%nsta
            GOTO 20
          ENDIF
10      CONTINUE
C
C ADD IONOS. REF. STATION TO THE LIST OF STATIONS
        NSTTOT=opt%nsta+1
        opt%stanam(opt%nsta+1)=opt%ionref
        INDION=NSTTOT
      ELSE
        NSTTOT=opt%nsta
        INDION=1
      ENDIF
C
C READ STATION COORDINATES FOR THE SESSION
C ----------------------------------------
20    CALL GETSTA(NSTTOT,opt%stanam,STANUM,NCENTR,ICENTR,
     1            XSTAT,XSTELL,XSTECC,
     2            DATUM,AELL,BELL,DXELL,DRELL,SCELL)
C
C READ NUMBER OF SATELLITES TO BE OBSERVED, SVN-NUMBERS
C -----------------------------------------------------
      CALL ORBINF(1,(/opt%obswin%T(1)/),NARC,ARCINT,NUMSAT,
     1            SOURCE,TBOUND,NAVNUM,IORSYS)
      NSATEL=NUMSAT(ARCINT(1))
C
C CHECK MAXIMAL NUMBER OF SATELLITES
      IF (NSATEL.GT.MAXSAT) THEN
        WRITE(LFNERR,901) NSATEL,MAXSAT
901     FORMAT(/,' *** PG GPSSIM: TOO MANY SATELLITES',/,
     1                       16X,'NUMBER OF SATELLITES :',I3,/,
     2                       16X,'MAX. NUMBER OF SATEL.:',I3,/)
        CALL EXITRC(2)
      END IF
C
      I0=0
      DO 30 I=1,ARCINT(1)-1
        I0=I0+NUMSAT(I)
30    CONTINUE
      DO 40 ISAT=1,NSATEL
        SVN(ISAT)=NAVNUM(I0+ISAT)
40    CONTINUE
C
C READ CLOCK ERRORS
C -----------------
      CALL SMCLKI(opt%nSta,opt%staNam,NCLOCK,CLOCK)
C
C SET GPS AND GLONASS FREQUENCIES
C -------------------------------
      CALL DEFREQ(opt%obswin%t,NSATEL,SVN)
C
C Initialize antenna buffers
C -------------------------------------
      CALL init_buf(bufsize=(/opt%nSta,NSATEL/))
C
C INIT THE BUFFER FOR VIENNA GRID FILES
C -------------------------------------
      CALL initGridBuffer(bufferSize=1)
      CALL prtGridInfo
C
C LOOP OVER ALL STATIONS OF THE SESSION
C -------------------------------------
      DO 100 IST=1,opt%nSta

C
C GET RECEIVER INFORMATION
C ------------------------
        CALL GETRCV(opt%RECTYP(IST),NFREQ,ICODE,IWLF,ICLASS,ISYST)
C
        NFRQPH=NFREQ
        IF (ICLASS.EQ.0) THEN
          NFRQCD=1
        ELSE
          NFRQCD=2
        END IF
C
C HANDLING STATION TYPES
C ---------------------------
        CALL STAFLG(opt%stanam(IST),opt%obswin%t(1),IFLAG,MARTYP)
        IF (MARTYP.EQ.MTypeSPACE) GOTO 28
C
C COMPUTE ELLIPSOIDIAL STATION COORDINATES
C ----------------------------------------
        DO IFRQ=1,NFRQPH
          WGSSES(1:3,IFRQ,IST)=XSTAT(1:3,IST)
          CALL XYZELL(AELL,BELL,DXELL,DRELL,SCELL,WGSSES(1,IFRQ,IST),
     1                ELLSES(1,IFRQ,IST))
        END DO
C
C GENERATE ERROR FREE PSEUDORANGES
C --------------------------------
28      CALL SMPRNG(opt%stanam(IST),WGSSES(1,1,IST),
     1              ELLSES(1,1,IST),opt%SECIPL,NSATEL,SVN,
     2              opt%itropo,opt%iextra,NCLOCK(IST),
     3              CLOCK(1,IST),opt%iono,XSTAT(1,INDION),opt%obswin,
     4              opt%rectyp(ist),opt%anttyp(ist),
     5              opt%antuni(IST),opt%csess,opt%inter,opt%ielev,
     6              opt%mxosvn,IREL2,NEPO,PRANG,ZENDIS,NOBSVN,opt%ix,
     7              NFRQPH,XSTELL(1,IST))
C
C WRITE CODE FILE
C ---------------
        IF (opt%codsel == 1) THEN
          CALL SMWTCD(opt%filcod(:,IST),NFRQCD,NEPO,NSATEL,opt%csess,
     1                IWLF,opt%inter,opt%obswin%t(1),opt%campgn,
     2                opt%title,IFRMAT,opt%stanam(IST),opt%rectyp(ist),
     3                opt%anttyp(ist),opt%recuni(IST),opt%antuni(IST),
     4                opt%oprnam(IST),SVN,NCLOCK(IST),CLOCK(1,IST),
     5                NOBSVN,PRANG(1,1,1,1),ZENDIS,opt%rms(1,1),
     6                opt%leorms(1,1),opt%icoelv,opt%leoelv,opt%ix)
        END IF
C
C WRITE PHASE FILE
C ----------------
        IF (opt%phasel == 1) THEN
          CALL SMWTPH(opt%filpha(:,IST),NFRQPH,NEPO,NSATEL,opt%csess,
     1                IWLF,opt%inter,opt%obswin%t(1),opt%campgn,
     2                opt%title,IFRMAT,opt%stanam(IST),opt%rectyp(ist),
     3                opt%anttyp(ist),opt%recuni(IST),opt%antuni(IST),
     4                opt%oprnam(IST),SVN,NCLOCK(IST),CLOCK(1,IST),
     5                NOBSVN,PRANG(1,1,1,2),ZENDIS,opt%rms(1,2),
     6                opt%leorms(1,2),opt%icoelv,opt%leoelv,opt%ix,
     7                opt%nslips,opt%isize,opt%jl12eq)
        END IF

100   CONTINUE

C
C CLOSE METEO FILE
C ----------------
      CALL METEO(2,opt%itropo,opt%iextra,0,CDMY1,0,
     1           0.D0,DUMMY3,DUMMY,DUMMY,0.D0,DUMMY,DUMMY,DUMMY,DUMMY)
C
C END OF PROGRAM
C --------------
      CALL EXITRC(0)
      END


