      MODULE s_CLKSAV
      CONTAINS

C*
      SUBROUTINE CLKSAV(TITLE ,NPAR  ,LOCQ  ,RMS,   XXX   ,ANOR,
     1                  XXX0  ,XREF  ,SECIPL,RNXCLK,CLKHED,CLKREC,
     2                  OPTELI,NEPOBS,NSHD  ,SATSHD,TIMSHD,IRAUX2,
     2                  CLKSYS,CLKPAR)
CC
CC NAME       :  CLKSAV
CC
CC PURPOSE    :  SAVE ESTIMATED SATELLITE/STATION CLOCKS
CC
CC PARAMETERS :
CC         IN :  TITLE  : TITLE FOR FILE                      CH*80
CC               NPAR   : TOTAL NUMBER OF PARAMETERS          I*4
CC               LOCQ(K,I),K=1,..,MAXLCQ,I=1,2,...            I*4
CC                        CHARACTERIZATION OF PARAMETERS
CC               RMS    : RMS ERROR OF UNIT WEIGHT (M)        R*8
CC               XXX(I),I=1,2,.... SOLUTION VECTOR            R*8
CC               ANOR(I),I=1,2,..,NPAR*(NPAR+1)/2: INVERSE OF R*8
CC                        NORMAL EQUATION MATRIX (UPPER
CC                        TRIANGLE ONLY, COLUMNWISE LINEAR.)
CC               XXX0(I),I=1,...NPAEPO, APRIORI VALUES FOR    R*8
CC                       EPOCH PARAMETERS
CC               XREF     VALUES FOR REFERENCE CLOCKS         T_PAR
CC               SECIPL : INTERVAL FOR CLOCK INTERPOLATION    R*8
CC               RNXCLK:  WHAT TO DO IF NO INPUT CLOCK RINEX  I*4
CC                        FOR SATELLITE CLOCKS:
CC                         -1: IGNORE CLOCK RINEX FILE
CC                          0: TRY ALSO SAT CLK FILE
CC                          1: USE OBS. (INTERPOL. CLK RNX)
CC                          2: SKIP OBS.
CC                          3: USE OBS. (SAT CLK = ZERO)
CC               CLKHED : CLOCK HEADER INFORMATION            T_CLKHEAD
CC                          %NUMREF=0: FIX REF-CLOCKS
CC                          %NUMREF=2: SUM FOR REF-CLOCKS
CC               CLKREC : %NEPO: EPOCHS WITH HIGHEST SAMPL.   T_CLKREC
CC                        %EPOCH(1): LAST EPOCH
CC                                (SEC. SINCE CLKHED%TFIRST)
CC                        %CLOCK(ISTA,IEPO): APRIORI STATION
CC                                CLOCK (IF NOT PREELIM BI or EP)
CC               OPTELI(I),I=1,..,MAXTYP: OPTION FOR PRE-     I*4
CC                        ELIMINATION OF PARAMETER TYPES:
CC                        =0 : NOT PRE-ELIMINATED
CC                        =1 : PRE-ELIMINATED BEFORE INVERSION
CC                        =2 : PRE-ELIMINATED AFTER  INVERSION
CC                        =3 : PRE-ELIMINATED EPOCH-WISE
CC               NEPOBS(I): MIN: # OF OBS. FOR EPOCH PARAM.S  I*4(3)
CC                        (I=1 STA-CLK / I=2 SAT-CLK / I=3 KIN.)
CC               NSHD   : NUMBER OF SATELLITES IN SHADOW      I*4
CC               SATSHD : PRN NUMBERS IN SHADOW               I*4(*)
CC               TIMSHD : EXCLUDED TIME INTERVAL              R*8(2,*)
CC               IRAUX2 : EPOCH RESULT SCRATCH FILE IS        I*4
CC                        0: AVAILABLE / 1: NOT AVAIL.
CC               CLKSYS : =1: ONE REC.CLK FOR EACH SAT.SYS    I*4
CC               CLKPAR(I,1..CLKHED%NCLK) : CHECKS WETHER  I*4(2,*)
CC                        A CLOCK IS OBSERVED OR NOT
CC                        I=1: GPS IF CLKSYS==1
CC                        I=2: GLONASS  IF CLKSYS==1
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  T.A. SPRINGER
CC
CC VERSION    :  4.0
CC
CC CREATED    :  09-AUG-95
CC
CC CHANGES       13-MAY-97 : MR: REMOVE UNUSED VARIABLES "SIGN1","SIGN2"
CC               23-MAY-97 : TS: CORRECTED "XXX" UNITS
CC               25-JAN-02 : RD: WRITE ALL CLOCK RESULT FILES HERE
CC                               (CLOCK RINEX+RESULTS FROM EPOCH-PREELIM)
CC               11-DEC-02 : RD: COMPLETE CLOCK RINEX FILE FOR ALL(?) CASES
CC               28-JAN-03 : RD: NUMBER OF OBS. FOR KIN. POS.(CLKOBS->NEPOBS)
CC                               USE IRAUX2 FOR EPOCH RESULT SCRATCH FILE
CC               19-MAR-03 : MR: READ TOBS FOR PARAMETER TYPE 21
CC               02-DEC-03 : HB: INITIALIZE CLOCK AND SIGMA VALUES WITH
CC                               UNDEF = 999999.999999D0
CC               04-APR-05 : RD: BUGFIX FOR WRITING CLK RINEX FROM IRAUX2
CC               27-MAY-05 : HU: DECLARATION CORRECTED
CC               21-Jun-05 : MM: COMLFNUM.inc removed, M_BERN included
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               11-JUN-06 : HB: READ A PRIORI VALUES FOR KIN FROM LFNAUX
CC               29-JUN-06 : HB: GET A PRIORI VALUES FROM XXX0 FOR NOT
CC                               PRE_ELIMINATED PARAMETERS
CC               24-NOV-06 : AG: LEAP SECOND, PGMNAM, PCVSTR SET IN CLKHED
CC               14-JUN-07 : RD: CORRECT BERNESE SAT-CLK WHEN EST. KIN.POS.
CC               01-NOV-07 : HB: ADD PARAMETER SECIPL FOR GTSCLK
CC               04-FEB-08 : HB: DEFINE JCLK FOR REFERENCE CLOCK HANDLING
CC               09-MAY-09 : RD: SEPERATE RECEIVER CLOCKS FOR GPS/GLONASS
CC               29-MAY-09 : RD: INPUT CLOCKS ALSO FROM INPUT CLK RNX FILE
CC               10-JUN-09 : RD: USE "UNDEF" TO INIT. CLOCKS
CC               21-SEP-09 : RD: ECLIPSING FLAG ADDED
CC               21-MAY-10 : MF: CALL INIT_CLKREC
CC               30-NOV-10 : RD: CORRECT LOOP OVER ALL SYSTEMS
CC               17-MAY-11 : LM: REMOVE DOUBLE ENTRY OF UNDEF
CC               08-SEP-11 : RD: REFERENCE SATELITE CLOCKS FOR PPP
CC               28-MAR-12 : RD: USE SVN2CHR AS MODULE NOW
CC               16-JUL-13 : RD: CORRECT APRIORI VALUE FOR WRITING CLOCK RINEX
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1995     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE M_BERN,   ONLY: LFNERR,LFN002,PGMVER
      USE D_CLKRNX, ONLY: T_CLKHEAD,T_CLKREC,UNDEF,COPY_CLKHEAD,
     1                    CLKIDX,INIT_CLKREC
      USE d_const,  ONLY: C, DATE, TIME
      USE d_par,    ONLY: t_par
      USE f_ikf
      USE f_dgpsut
      USE s_opnfil
      USE s_alcerr
      USE s_wtcrxr
      USE s_mjdgps
      USE s_opnerr
      USE s_gtflna
      USE s_gtsclk
      USE s_gtsensor
      USE s_wtcrxh
      USE s_clrflg
      USE s_setflg
      USE s_svn2chr
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IAC   , ICLK  , IEPO  , II    , INUM  , IOSTAT, RNXCLK,
     1          IPAR  , IRAUX2, IRC   , IRCODE, IREF  , IRRCLK, IRSCLK,
     2          ISTA  , ISVN  , ITYP  , JCLK  , JJ    , JSAT  , LFNAUX,
     3          LFNRCK, LFNSCK, MXCLCQ, NOBS  , NPAR  , NQSATC, NQSTAC,
     4          NREF  , NWEEK , CLKSYS, LFNRGP, LFNRGL, JSTA  , SATNUM,
     5          ISHD  , NCLK  , NSHD  , JEPO  , JREF
C
      REAL*8    AII   , CLOCKC, COEF  , DXXX  , RMS   , RMS1  , SECOND,
     1          SECIPL, TOBS  , XAPR0
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C GLOBAL DIMENSIONS
      TYPE(T_CLKHEAD) CLKHED
      TYPE(T_CLKREC)  CLKREC
      TYPE(T_CLKHEAD),DIMENSION(CLKSYS+1) :: HEDOUT
      TYPE(T_CLKREC), DIMENSION(CLKSYS+1) :: CLKRES
      TYPE(T_PAR)     XREF(:,:)
C
      CHARACTER*80 TITLE
      CHARACTER*32 FLSCLK,FLRCLK,FILAUX, FLRGPS,FLRGLO
      CHARACTER*16 CLKNAM
      CHARACTER*6  MXNLCQ
      CHARACTER*1  SATCHR
C
      REAL*8       XXX(*),ANOR(*),XXX0(*)
      REAL*8       XTMP(3),ATMP(6),TIMSHD(2,*)
C
      INTEGER*4    LOCQ(:,:),LCQ2(MXCLCQ),OPTELI(*),SATSHD(*)
      INTEGER*4    LQTMP(MXCLCQ,3)
      INTEGER*4    NEPOBS(3)
      INTEGER*4    CLKPAR(2,*)
C
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
C
C INITIALIZE
C ----------
      DO II = 1,SIZE(CLKRES)
        CALL init_clkrec(CLKRES(II))
      ENDDO
C
C GET RESULT FILE NAMES
C ---------------------
      CALL GTFLNA(0,'CLKSAV ',FLSCLK,IRSCLK)
      CALL GTFLNA(0,'CLKRNX ',FLRCLK,IRRCLK)
      IF (IRRCLK.EQ.1.AND.IRSCLK.EQ.1) RETURN
C
      LFNSCK=LFN002
      LFNRCK=LFN002+1
      LFNRGP=LFN002+2
      LFNRGL=LFN002+3
      LFNAUX=LFN002+4
C
C OPEN BERNESE SATELLITE CLOCK FILE, WRITE THE HEADER
C ---------------------------------------------------
      IF (IRSCLK.EQ.0) THEN
        CALL OPNFIL(LFNSCK,FLSCLK,'UNKNOWN','FORMATTED',
     1            ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNSCK,IOSTAT,FLSCLK,'CLKSAV')
C
        WRITE(LFN002,1000) TITLE,DATE,TIME
1000    FORMAT(A64,1X,A9,1X,A5,/,80('-'),//,'SAT WEEK   ',
     1                   'TOC #PAR     A0 (SEC)',7X,
     2                   'A1 (SEC/SEC)    A2 (SEC/SEC**2)',/)
C
      ENDIF
C
C OPEN CLOCK RINEX FILE, WRITE THE HEADER
C ---------------------------------------
      IF (IRRCLK.EQ.0.AND.CLKSYS.EQ.0) THEN
        CALL OPNFIL(LFNRCK,FLRCLK,'UNKNOWN','FORMATTED',
     1            ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNRCK,IOSTAT,FLRCLK,'CLKSAV')
        ClkHed%pcvstr=''
        CALL gtsensor(pcvmod=ClkHed%pcvstr(1:10))
        ClkHed%LeapSec=DGPSUT(clkHed%TFirst)
        ClkHed%pgmnam = 'GPSEST V'//PGMVER//'      '
C
        IF (CLKHED%NUMREF.EQ.2) THEN
          CLKHED%NUMREF=1
          CALL WTCRXH(LFNRCK,LFNERR,CLKHED,IRCODE)
          CLKHED%NUMREF=2
        ELSE
          CALL WTCRXH(LFNRCK,LFNERR,CLKHED,IRCODE)
        ENDIF

      ENDIF
C
C
C OPEN CLOCK RINEX FILE, WRITE THE HEADER
C ---------------------------------------
      IF (IRRCLK.EQ.0.AND.CLKSYS.EQ.1) THEN
C
        IF (CLKHED%NUMREF.EQ.2) THEN
          CLKHED%NUMREF=1
          DO II=1,CLKSYS+1
            HEDOUT(II)=COPY_CLKHEAD(CLKHED)
          ENDDO
          CLKHED%NUMREF=2
        ELSE
          DO II=1,CLKSYS+1
            HEDOUT(II)=COPY_CLKHEAD(CLKHED)
          ENDDO
        ENDIF
C
C ADD A COMMENT
C -------------
        DO II=1,CLKSYS+1
          DEALLOCATE(HEDOUT(II)%COMMENT,STAT=IAC)
          ALLOCATE(HEDOUT(II)%COMMENT(CLKHED%NCOMMENT+1),STAT=IAC)
          CALL ALCERR(IAC,'HEDOUT(II)%COMMENT',(/CLKHED%NCOMMENT+1/),
     1                'CLKSAV')
          HEDOUT(II)%COMMENT(1:HEDOUT(II)%NCOMMENT)=
     1                              CLKHED%COMMENT(1:CLKHED%NCOMMENT)
          HEDOUT(II)%NCOMMENT=HEDOUT(II)%NCOMMENT+1
        ENDDO
C
        HEDOUT(1)%COMMENT(HEDOUT(1)%NCOMMENT) =
     1        'RECEIVER CLOCKS FOR EACH SATELLITE SYSTEM: GPS PART'
        HEDOUT(2)%COMMENT(HEDOUT(2)%NCOMMENT) =
     1        'RECEIVER CLOCKS FOR EACH SATELLITE SYSTEM: GLONASS PART'
C
C CHECK AVAILABLILITY OF THE CLOCKS
C ---------------------------------
        DO II=1,CLKSYS+1
          JCLK=0
          JSTA=0
          JSAT=0
          DO ICLK=1,HEDOUT(II)%NSTA+HEDOUT(II)%NSAT
            IF(CLKPAR(II,ICLK).GT.0) THEN
              JCLK=JCLK+1
              IF (JCLK.LT.ICLK) THEN
                HEDOUT(II)%CLKNAME(JCLK)=HEDOUT(II)%CLKNAME(ICLK)
                IF (JSTA.EQ.0) HEDOUT(II)%STACOORD(1:3,JCLK)=
     1                         HEDOUT(II)%STACOORD(1:3,ICLK)
              ENDIF
            ENDIF
            IF (ICLK.EQ.HEDOUT(II)%NSTA)                 JSTA=JCLK
            IF (ICLK.EQ.HEDOUT(II)%NSTA+HEDOUT(II)%NSAT) JSAT=JCLK-JSTA
          ENDDO
          HEDOUT(II)%NSTA=JSTA
          HEDOUT(II)%NSAT=JSAT
C
C UPDATE REFERENCE CLOCK INFORMATION
          JCLK=0
          DO ICLK=1,HEDOUT(II)%REF(1)%NREF
            JSTA=CLKIDX(HEDOUT(II),
     1           CLKNAM=HEDOUT(II)%REF(1)%CLK(ICLK)%NAME)
            IF (JSTA.NE.0) THEN
              JCLK=JCLK+1
              IF (JCLK.LT.ICLK) THEN
                HEDOUT(II)%REF(1)%CLK(JCLK)=HEDOUT(II)%REF(1)%CLK(ICLK)
              ENDIF
              HEDOUT(II)%REF(1)%CLK(JCLK)%IDX=JSTA
            ENDIF
          ENDDO
          HEDOUT(II)%REF(1)%NREF=JCLK
        ENDDO
C
        FLRGPS=TRIM(FLRCLK) // '_g'
        CALL OPNFIL(LFNRGP,FLRGPS,'UNKNOWN','FORMATTED',
     1            ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNRGP,IOSTAT,FLRGPS,'CLKSAV')
        CALL WTCRXH(LFNRGP,LFNERR,HEDOUT(1),IRCODE)
C
        FLRGLO=TRIM(FLRCLK) // '_r'
        CALL OPNFIL(LFNRGL,FLRGLO,'UNKNOWN','FORMATTED',
     1            ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNRGL,IOSTAT,FLRGLO,'CLKSAV')
        CALL WTCRXH(LFNRGL,LFNERR,HEDOUT(2),IRCODE)
C
        DO II=1,CLKSYS+1
          DEALLOCATE(HEDOUT(II)%COMMENT   ,STAT=IAC)
          DEALLOCATE(HEDOUT(II)%DATTYP    ,STAT=IAC)
          DEALLOCATE(HEDOUT(II)%REF(1)%CLK,STAT=IAC)
          DEALLOCATE(HEDOUT(II)%REF       ,STAT=IAC)
          DEALLOCATE(HEDOUT(II)%CLKNAME   ,STAT=IAC)
          DEALLOCATE(HEDOUT(II)%STACOORD  ,STAT=IAC)
        ENDDO
      ENDIF
C
C OPEN SCRATCH FILE FOR EPOCH-WISE SOLUTION
C -----------------------------------------
      IF (IRAUX2.EQ.0.AND.(OPTELI(23).EQ.3.OR.OPTELI(24).EQ.3)) THEN
        CALL GTFLNA(1,'AUXFIL1',FILAUX,IRC)
        CALL OPNFIL(LFNAUX,FILAUX,'OLD','UNFORMATTED',
     1            'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNAUX,IOSTAT,FILAUX,'CLKSAV')
      ENDIF
C
C ALLOCATE THE CLOCK RESULT RECORDS
C ---------------------------------
      CLKRES(:)%NEPO=1
      NQSTAC=0
      NQSATC=0
      DO IPAR=1,NPAR
        IF (LOCQ(1,IPAR).EQ.23.OR.LOCQ(1,IPAR).EQ.24) THEN
          IF (LOCQ(4,IPAR).GT.CLKRES(1)%NEPO) THEN
             CLKRES(:)%NEPO=LOCQ(4,IPAR)
          ENDIF
        ENDIF
        IF (LOCQ(1,IPAR).EQ.23) NQSTAC=NQSTAC+1
        IF (LOCQ(1,IPAR).EQ.24) NQSATC=NQSATC+1
      ENDDO
C
      DO II = 1,CLKSYS+1
        NCLK=CLKHED%NSTA+CLKHED%NSAT
        ALLOCATE(CLKRES(II)%EPOCH(CLKRES(II)%NEPO),STAT=IAC)
        CALL ALCERR(IAC,'CLKRES(II)%EPOCH',(/CLKRES(II)%NEPO/),'CLKSAV')
C
        ALLOCATE(CLKRES(II)%CLOCK(NCLK,CLKRES(II)%NEPO),STAT=IAC)
        CALL ALCERR(IAC,'CLKRES(II)%CLOCK',
     1             (/NCLK,CLKRES(II)%NEPO/),'CLKSAV')
C
        ALLOCATE(CLKRES(II)%SIGMA(NCLK,CLKRES(II)%NEPO),STAT=IAC)
        CALL ALCERR(IAC,'CLKRES(II)%SIGMA',
     1             (/NCLK,CLKRES(II)%NEPO/),'CLKSAV')
C
        ALLOCATE(CLKRES(II)%CLKFLG(NCLK,CLKRES(II)%NEPO),STAT=IAC)
        CALL ALCERR(IAC,'CLKRES(II)%CLKFLG',
     1              (/NCLK,CLKRES(II)%NEPO/),'CLKSAV')
C
        CLKRES(II)%EPOCH=0D0
        CLKRES(II)%CLOCK=UNDEF
        CLKRES(II)%SIGMA=UNDEF
        DO IEPO = 1,CLKRES(II)%NEPO
          DO ICLK = 1,NCLK
            DO JJ = 0,7
              CALL CLRFLG(CLKRES(II)%CLKFLG(ICLK,IEPO),JJ)
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
C LOOP OVER ALL CLOCK REQUESTS
C ----------------------------
      IF (OPTELI(23).NE.3.OR.OPTELI(24).NE.3) THEN
        IPAR=0
      ELSE
        IPAR=NPAR
      ENDIF
C
      DO 100
        IF (IPAR.EQ.NPAR.AND.
     1      OPTELI(23).NE.3.AND.OPTELI(24).NE.3.AND.
     2      IRAUX2.NE.0) GOTO 9000
C
C EPOCH-PREELIMINATED CLOCK PARAMETER?
C ------------------------------------
        IF (IPAR.GE.NPAR) THEN
          READ(LFNAUX,END=9000) ITYP,INUM
C
          IF (ITYP.EQ.21) THEN
            DO JJ=1,3
              READ(LFNAUX) TOBS,(LQTMP(II,JJ),II=1,MXCLCQ),
     1                     XAPR0,(ATMP(IKF(JJ,II)),II=1,3),
     2                     XTMP(JJ)
            ENDDO
            LCQ2(:) = LQTMP(:,1)
          ENDIF
C
!          IF (ITYP.EQ.23.AND.OPTELI(23).EQ.3)
          IF (ITYP.EQ.23)
     1      READ(LFNAUX) TOBS,(LCQ2(II),II=1,MXCLCQ),XAPR0,DXXX,AII
C
!          IF (ITYP.EQ.24.AND.OPTELI(24).EQ.3)
          IF (ITYP.EQ.24)
     1      READ(LFNAUX) TOBS,(LCQ2(II),II=1,MXCLCQ),XAPR0,DXXX,AII
        ENDIF
C
C "NORMAL" CLOCK PARAMETER?
C -------------------------
        IF (IPAR.LT.NPAR) THEN
          IPAR=IPAR+1
C
          DO II=1,MXCLCQ
            LCQ2(II)=LOCQ(II,IPAR)
          ENDDO
C
          TOBS=0d0
          XAPR0=UNDEF
          IF (XXX0(IPAR)/=1.D20) THEN
            IF (LCQ2(1)==23) THEN
              XAPR0=-XXX0(IPAR)
            ELSE
              XAPR0= XXX0(IPAR)
            ENDIF
          ENDIF
          DXXX=XXX(IPAR)
          AII =ANOR(IKF(IPAR,IPAR))
        ENDIF
C
C IS THIS A CLOCK PARAMETER?
C --------------------------
        IF (LCQ2(1).NE.23 .AND. LCQ2(1).NE.24) GOTO 100
C
C COMPUTE AND CHECK RMS
C ---------------------
        IF (LCQ2(7).EQ.-1) THEN
          RMS1=0d0
        ELSE
          IF (AII.LE.0D0) GOTO 100
          RMS1=RMS*DSQRT(AII)*1.0D6/C
ccc          IF (OPTELI(LCQ2(1)).EQ.3) RMS1=DSQRT(AII)*1.0D6/C
        ENDIF
C
C CHECK NUMBER OF OBSERVATIONS
C ----------------------------
        NOBS=LCQ2(6)
        IF (LCQ2(7).NE.-1) THEN
          IF (LCQ2(1).EQ.23.AND.NOBS.LT.NEPOBS(1)) GOTO 100
          IF (LCQ2(1).EQ.24.AND.NOBS.LT.NEPOBS(2)) GOTO 100
        ENDIF
C
C COMPUTE THE EPOCH FOR THE CLOCK
C WRITE A COMPLETE EPOCH INTO CLOCK RINEX FILE (FOR EPOCH-PREELIM ONLY)
C ---------------------------------------------------------------------
        IEPO=LCQ2(4)
        IF (TOBS.EQ.0d0) THEN
          TOBS=CLKHED%TFIRST+
     1         DBLE(IEPO-1)*CLKREC%EPOCH(1)/CLKREC%NEPO/86400.D0
        ENDIF
C
        IF ((OPTELI(23).EQ.3.AND.OPTELI(24).EQ.3) .OR.
     1      (OPTELI(23).EQ.3.AND.NQSATC.EQ.0)     .OR.
     2      (OPTELI(24).EQ.3.AND.NQSTAC.EQ.0))   THEN
          IEPO=1
          IF (IRRCLK.EQ.0.AND.(TOBS-CLKHED%TFIRST)*86400D0-
     1                         CLKRES(1)%EPOCH(IEPO).GT.0.5D0) THEN
C
            IF (CLKSYS.EQ.0) THEN
              CALL WTCRXR(LFNRCK,LFNERR,CLKHED,CLKRES(1),IRCODE)
            ELSE
              CALL WTCRXR(LFNRGP,LFNERR,CLKHED,CLKRES(1),IRCODE)
              CALL WTCRXR(LFNRGL,LFNERR,CLKHED,CLKRES(2),IRCODE)
            ENDIF
C
            DO II = 1,CLKSYS+1
              CLKRES(II)%EPOCH=0D0
              CLKRES(II)%CLOCK=UNDEF
              CLKRES(II)%SIGMA=UNDEF
              DO JEPO = 1,CLKRES(II)%NEPO
                DO ICLK = 1,CLKHED%NSTA+CLKHED%NSAT
                  DO JJ = 0,7
                    CALL CLRFLG(CLKRES(II)%CLKFLG(ICLK,JEPO),JJ)
                  ENDDO
                ENDDO
              ENDDO
            ENDDO
          ENDIF
        ENDIF
        DO II = 1,CLKSYS+1
          CLKRES(II)%EPOCH(IEPO)=(TOBS-CLKHED%TFIRST)*86400D0
        ENDDO
C
C GET RESULT FOR A STATION CLOCK
C -----------------------------
        IF (LCQ2(1).EQ.23) THEN
          ISTA=LCQ2(2)
C
          IF (XAPR0.EQ.UNDEF) XAPR0=CLKREC%CLOCK(ISTA,IEPO)
C
          II = 1
          IF (CLKSYS.EQ.1) II = LCQ2(3)
          IF (II.NE.0) THEN
            CLKRES(II)%CLOCK(ISTA,IEPO)=(XAPR0-DXXX/C)*1D6
            CLKRES(II)%SIGMA(ISTA,IEPO)=RMS1
C
C EPOCH-WISE FIXED REFERENCE CLOCKS
          ELSE
            DO II=1,2
              IF (CLKPAR(II,ISTA).EQ.0) CYCLE
              CLKRES(II)%CLOCK(ISTA,IEPO)=(XAPR0-DXXX/C)*1D6
              CLKRES(II)%SIGMA(ISTA,IEPO)=RMS1
            ENDDO
          ENDIF
        ENDIF
C
C GET RESULT FOR A SATELLITE CLOCK
C --------------------------------
        IF (LCQ2(1).EQ.24) THEN
          ISVN=LCQ2(3)
C
          IF (XAPR0.EQ.UNDEF)
     1      CALL GTSCLK(RNXCLK,TOBS,ISVN,SECIPL,2,XAPR0,IRC)
C
          CALL svn2chr(ISVN,satNum,satChr)
          CLKNAM=' '
          WRITE(CLKNAM,'(A1,I2.2)') satChr,satNum
C
          II = 1
          IF (CLKSYS.EQ.1) II = INT(ISVN/100)+1
C
          DO JSAT=CLKHED%NSTA+1,CLKHED%NSTA+CLKHED%NSAT
            IF (CLKHED%CLKNAME(JSAT).EQ.CLKNAM) THEN
              CLKRES(II)%CLOCK(JSAT,IEPO)=(XAPR0-DXXX/C)*1D6
              CLKRES(II)%SIGMA(JSAT,IEPO)=RMS1
              DO ISHD=1,NSHD
                IF (ISVN.EQ.SATSHD(ISHD).AND.
     1              TOBS.GE.TIMSHD(1,ISHD).AND.TOBS.LE.TIMSHD(2,ISHD))
     2            CALL SETFLG(CLKRES(II)%CLKFLG(JSAT,IEPO),0)
              ENDDO
            ENDIF
          ENDDO
C
C WRITE RESULT INTO BERNESE SAT. CLOCK FILE
C -----------------------------------------
          IF (IRSCLK.EQ.0) THEN
              CALL MJDGPS(TOBS,SECOND,NWEEK)
C
              COEF = XAPR0-DXXX/C
C
              WRITE(LFNSCK,1010)ISVN,NWEEK,SECOND,1,COEF
1010          FORMAT(I3,1X,I4,1X,F8.0,2X,I1,1X,10D17.9)
          ENDIF
        ENDIF
C
C NEXT PARAMETER
C --------------
100   CONTINUE
C
C ADD (FIXED) REFERENCE CLOCK VALUES FOR "NORMAL" SOLUTION
C --------------------------------------------------------
9000  CONTINUE
      IF((NQSTAC.NE.0.OR.NQSATC.NE.0).AND.
     1    CLKHED%NUMREF.EQ.1) THEN
        NREF=CLKHED%NUMREF
C
        DO II = 1,CLKSYS+1
C
C LOOP ALL EPOCHS
          DO 200 IEPO=1,CLKRES(II)%NEPO
            TOBS=CLKHED%TFIRST+CLKRES(II)%EPOCH(IEPO)/86400D0
C
C LOOP ALL REFERENCE CLOCKS
            DO 230 IREF=1,CLKHED%REF(NREF)%NREF
              DO JREF=1,CLKHED%REF(NREF)%NREF

C
C DIFFERENT HANDLING FOR STATION AND SATELLITE CLOCKS
                IF (xref(jRef,iepo)%locq(2)==
     1                      CLKHED%REF(NREF)%CLK(IREF)%IDX) THEN
!                      IF (SIZE(CLKREC%CLOCK,2).LT.IEPO) GOTO 230
                  JCLK=CLKHED%REF(NREF)%CLK(IREF)%IDX
                IF (xref(jRef,IEPO)%x0.NE.UNDEF)
     1            CLKRES(II)%CLOCK(JCLK,IEPO)=-xref(jRef,IEPO)%x0*1D6
!!     1                CLKRES%CLOCK(JCLK,IEPO)=CLKREC%CLOCK(JCLK,IEPO)*1D6
                ELSEIF(xref(jRef,iepo)%locq(3)==
     1                          CLKHED%REF(NREF)%CLK(IREF)%IDX0) THEN
                  JCLK=CLKHED%REF(NREF)%CLK(IREF)%IDX
                  CLKRES(II)%CLOCK(JCLK,IEPO)=xref(jRef,IEPO)%x0*1D6
                ENDIF
C
              ENDDO
230         CONTINUE
200       CONTINUE
        ENDDO
      ENDIF
C
C WRITE CLOCK RINEX FILE
C ----------------------
      IF (IRRCLK.EQ.0.AND.CLKSYS.EQ.0) THEN
        CALL WTCRXR(LFNRCK,LFNERR,CLKHED,CLKRES(1),IRCODE)
      ENDIF
C
      IF (IRRCLK.EQ.0.AND.CLKSYS.EQ.1) THEN
        CALL WTCRXR(LFNRGP,LFNERR,CLKHED,CLKRES(1),IRCODE)
        CALL WTCRXR(LFNRGL,LFNERR,CLKHED,CLKRES(2),IRCODE)
      ENDIF
C
C CLOSE ALL FILES
C ---------------
      IF (IRRCLK.EQ.0) THEN
        IF (CLKSYS.EQ.0) THEN
          CLOSE(LFNRCK)
        ELSE
          CLOSE(LFNRGP)
          CLOSE(LFNRGL)
        ENDIF
      ENDIF
      IF (IRSCLK.EQ.0) CLOSE(LFNSCK)

      IF (IRAUX2.EQ.0.AND.(OPTELI(23).EQ.3.OR.OPTELI(24).EQ.3))
     1  CLOSE(UNIT=LFNAUX)
C
C DEALLOCATE MEMORY
C -----------------
      DO II = 1,CLKSYS+1
        DEALLOCATE(CLKRES(II)%EPOCH,STAT=IAC)
        DEALLOCATE(CLKRES(II)%CLOCK,STAT=IAC)
        DEALLOCATE(CLKRES(II)%SIGMA,STAT=IAC)
        DEALLOCATE(CLKRES(II)%CLKFLG,STAT=IAC)
      ENDDO
C
      RETURN
      END SUBROUTINE

      END MODULE
