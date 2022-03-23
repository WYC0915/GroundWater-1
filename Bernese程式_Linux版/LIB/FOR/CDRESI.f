      MODULE s_CDRESI
      CONTAINS
C*
      SUBROUTINE CDRESI(IFIL,PGMNAM,CAMPGN,STITLE,ICLPOL,
     1                  PRIOPT,IRSSAV,OBSFIL,FILFRQ,NORDIM,
     2                  NSATEL,SATNUM,INDSVN,DPAR,TIMREF,IDELTT,OFFS,
     3                  DOFFS,KINEST,CLKHED,CLKREC,RMSSAT)
CC
CC NAME       :  CDRESI
CC
CC PURPOSE    :  IF REQUESTED:
CC               - PRINT ELEVATIONS
CC               - PRINT RESIDUALS
CC               - WRITE RESIDUAL FILE
CC
CC PARAMETERS :
CC         IN :  IFIL   : FILE NUMBER                         I*4
CC               PGMNAM : PROGRAM NAME                      CHR*6
CC               CAMPGN : CAMPAING NAME                     CHR*16
CC               STITLE : SHORT TITLE                       CHR*64
CC               ICLPOL : TYPE OF CLOCK MODELING              I*4
CC                        ICLPOL>0: POLYNOMIAL OF DEGREE ICLPOL
CC                        ICLPOL=-1: ONE OFFSET PER EPOCH
CC               PRIOPT(I): PRINT OPTIONS                     I*4(*)
CC                        I=1,2
CC               IRSSAV : SAVE FLAG FOR RESIDUALS             I*4
CC               OBSFIL : OBS FILE NAME (DD NAME)           CHR*32
CC               FILFRQ : FREQUENCY TO BE PROCESSED9          I*4
CC               NORDIM : DIM. OF NORMAL EQU. SYSTEM          I*4
CC               NSATEL : NUMBER OF SATELLITES IN FILE        I*4
CC               SATNUM(I): ARRAY OF SVN NUMBERS IN FILE      I*4(*)
CC                        I=1,2,...,NSATEL
CC               INDSVN(I): INDEX SVN --> SATNUM(I)           I*4(*)
CC                        I=1,2,...,MAXSVN
CC               DPAR(I): PARAMETER IMPROVEMENTS              R*8(*)
CC                        I=1,2,...,NORDIM
CC               TIMREF : REFERENC EPOCH                      R*8
CC               IDELTT : OBS INTERVAL (SEC)                  I*4
CC               OFFS(IEPO),IEPO=1,MXCEPO: CLOCK OFFSET       R*8
CC               DOFFS(IEPO),IEPO=1,MXCEPO: CLOCK OFFSET
CC               (SEE SR CDCOMP: DOFFS IS USED FOR RESIDUAL
CC                               COMPUTATION)                 R*8
CC               KINEST: ESTIMATION OF KINEMATIC COORDINATES  I*4
CC                         =0 NO KINEMATIC ESTIMATION
CC                         =1 ESTIMATE KINEMATIC COORDINATES
CC               CLKHED: clock rinex header                   t_clkhead
CC      IN/OUT:  CLKREC: clock rinex data record              t_clkrec
CC        OUT :  RMSSAT(ISAT,ISAT) : SATELLITE DEPENDENT      R*8(*)
CC                              ACCURACY STATISTICS
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  T.SCHILDKNECHT, L.MERVART
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/12/02 08:11
CC
CC CHANGES    :  26-MAY-92 : ??: SATELLITE DEPENDENT RMS
CC               11-JUN-92 : ??: WRITE FLAGS INTO THE RESIDUAL FILE
CC               17-FEB-93 : ??: WRITE TITLE LINES BETWEEN RESIDUALS
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               10-AUG-94 : MR: CALL EXITRC
CC               06-SEP-97 : HH: ADD GPS/GLONASS MIXED OBSERVATION TYPE
CC               30-OCT-97 : DI: USE NEW FUNCTION "MIXSVN"
CC               19-NOV-97 : SS: INCLUDE "PGMVER"
CC               27-JAN-00 : TS: SMALL CHANGE IN WRITING RESIDUALS
CC               29-MAY-00 : HU: IMPLICIT MOVED TO FIRST LINE
CC               10-MAY-01 : RD: WRITE CLOCK RINEX FILE
CC               06-JUN-01 : MR: USE "SVNSYS" INSTEAD OF "MIXSVN"
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               31-JUL-03 : RD: COMPUTE CORRECT RESIDUALS FOR KINEMATIC STA.
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               01-MAR-06 : RD: ENABLE WRITING CLK-RINEX FOR 1-HZ DATA
CC               20-MAR-06 : RD: BUG-FIX FOR CHANGE FROM 01-MAR-06
CC               27-SEP-11 : SL: USE M_BERN WITH ONLY
CC               28-MAR-12 : RD: USE SVNSYS AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
C*
C
C  DECLARATIONS
C  ------------
      USE m_bern,   ONLY: lfn001, lfnRes
      USE m_maxdim, ONLY: MAXSAT
      USE d_clkrnx, ONLY: t_clkhead,t_clkrec, unDef
      USE d_const,  ONLY: C
      USE s_cdpele
      USE s_maxtst
      USE f_svnsys
      USE s_cdpres
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , ICLPOL, IDELTT, IEPO  , IEPOCH, IFIL  , IFIRST,
     1          II    , IRC   , IRSSAV, ISAT  , ISEL  , JEPO  , JLINES,
     2          KINEST, KK    , LFNSCR, MXCEPO, MXCSAT, MXCSVN, NLINES,
     3          NORDIM, NSACT , NSATEL, NUMLIN, ISATOK
C
      REAL*8    OBSEPO, TIMRE1
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      TYPE(t_clkhead):: clkhed
      TYPE(t_clkrec) :: clkrec
C
      REAL*8 TIMREF,OFFS(MXCEPO),DOFFS(MXCEPO)
      REAL*8 AMAT(3,MAXSAT),DPHI(MAXSAT),DPAR(*)
      REAL*8 ELEACT(MAXSAT),RESIDU(MAXSAT),RMSSAT(MXCSAT,*)
C
      INTEGER*4 PRIOPT(*),SATNUM(MXCSAT),SATNRA(MAXSAT)
      INTEGER*4 INDSVN(MXCSVN)
      INTEGER*4 FILFRQ
C
      CHARACTER*64 STITLE
      CHARACTER*32 OBSFIL
      CHARACTER*16 CAMPGN
      CHARACTER*6  MXNSAT,MXNSVN,MXNEPO,PGMNAM
      CHARACTER*1  FLGACT(MAXSAT),FLGAMB
      LOGICAL      FLGMIX
C
C  COMMON FOR CONSTANTS AND MAXIMAL DIMENSIONS
C  -------------------------------------------
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMSVN/MXCSVN,MXNSVN
      COMMON/MCMEPO/MXCEPO,MXNEPO
      DATA IFIRST/1/,NUMLIN/20/
      DATA FLGAMB/' '/
C
C LOGICAL UNIT NUMBER FOR SCRATCH FILE
C ------------------------------------
      LFNSCR = LFN001 + 10
C
C  CHECK DIMENSIONS
C  ----------------
      IF(IFIRST.EQ.1) THEN
        CALL MAXTST(1,'CDRESI',MXNSAT,MAXSAT,MXCSAT,IRC)
        IF(IRC.NE.0) CALL EXITRC(2)
        IFIRST=0
      ENDIF
C
C CHECK OBSERVATION TYPE
C ----------------------
C
      FLGMIX=SVNSYS(-1,NSATEL,SATNUM)
C
C  PRINT ELEVATIONS (IF REQUESTED)
C  -------------------------------
      IF(PRIOPT(1).EQ.1) THEN
C  PRINT TITLE
        ISEL=1
        CALL CDPELE(PGMNAM,CAMPGN,STITLE,OBSFIL,FILFRQ,
     1            ISEL,NSATEL,SATNUM,INDSVN,OBSEPO,TIMREF,
     2            IDELTT,NSACT,SATNRA,FLGACT,ELEACT)
C  PRINT ELEVATIONS
        ISEL=2
        TIMRE1=-1.D20
        DO 420 IEPO=1,100000
          READ(LFNSCR,END=425)OBSEPO,NSACT,
     1                  (SATNRA(II),FLGACT(II),II=1,NSACT)
          READ(LFNSCR)(ELEACT(II),II=1,NSACT)
          READ(LFNSCR)((AMAT(II,KK),II=1,3),DPHI(KK),KK=1,NSACT)
          IF (KINEST.EQ.1) READ(LFNSCR) DPAR(1:3)
          IF((OBSEPO-TIMRE1)*86400.D0+.25.GT.IDELTT*10.D0) THEN
            TIMRE1=OBSEPO
          ELSE
            GOTO 420
          ENDIF
          CALL CDPELE(PGMNAM,CAMPGN,STITLE,OBSFIL,FILFRQ,
     1            ISEL,NSATEL,SATNUM,INDSVN,OBSEPO,TIMREF,
     1            IDELTT,NSACT,SATNRA,FLGACT,ELEACT)
420     CONTINUE
425     CONTINUE
        REWIND(UNIT=LFNSCR)
      ENDIF
C
C  PRINT RESIDUALS AND/OR WRITE PLOT FILE  (IF REQUESTED)
C  ------------------------------------------------------
C  WRITE TITLE ON PRINT CHANNEL
      IF(PRIOPT(2).EQ.1 .AND. IRSSAV .NE. 1) THEN
        ISEL=1
        CALL CDPRES(PGMNAM,CAMPGN,STITLE,OBSFIL,FILFRQ,
     1          ISEL,ICLPOL,NSATEL,SATNUM,INDSVN,OBSEPO,TIMREF,
     2          IDELTT,OFFS,NSACT,SATNRA,FLGACT,RESIDU)
      ENDIF
C  COMPUTE AND WRITE RESIDUALS (ON PRINT AND/OR PLOT CHANNEL)
      JLINES=0
      DO 450 IEPO=1,100000
        READ(LFNSCR,END=455)OBSEPO,NSACT,
     1                (SATNRA(II),FLGACT(II),II=1,NSACT)
        IEPOCH = IDNINT(((OBSEPO-TIMREF)*86400.D0)/IDELTT)+1
        IF(PRIOPT(1).EQ.1) THEN
          READ(LFNSCR)(ELEACT(II),II=1,NSACT)
        ENDIF
        READ(LFNSCR)((AMAT(II,KK),II=1,3),DPHI(KK),KK=1,NSACT)
        IF (KINEST.EQ.1) READ(LFNSCR) DPAR(1:3)
C
        ISATOK=-1-3*KINEST
        DO ISAT=1,NSACT
          IF (FLGACT(ISAT) .EQ. 'G') ISATOK=ISATOK+1
        ENDDO
C
        DO 440 ISAT=1,NSACT
          RESIDU(ISAT)=0.D0
          DO 430 I=1,3
            RESIDU(ISAT)=RESIDU(ISAT)+AMAT(I,ISAT)*DPAR(I)
430       CONTINUE
          IF (FLGMIX .AND. KINEST.EQ.0) THEN
            IF (SATNRA(ISAT).GE.100.AND.SATNRA(ISAT).LT.200) THEN
              RESIDU(ISAT)=RESIDU(ISAT)+DPAR(4)
            ENDIF
            DO 432 I=5,NORDIM
              IF (ICLPOL .GT. 0) THEN
                IF(OBSEPO.NE.TIMREF) THEN
                  RESIDU(ISAT)=RESIDU(ISAT)+DPAR(I)*((OBSEPO-TIMREF)*
     1                         86400.D0)**(I-5)
                ELSE
                  IF(I.EQ.5) RESIDU(ISAT)=RESIDU(ISAT)+DPAR(I)
                ENDIF
              ELSE
                RESIDU(ISAT)=RESIDU(ISAT)+DOFFS(IEPOCH)*C
              ENDIF
432         CONTINUE
          ELSE
            DO 435 I=4,NORDIM
              IF (ICLPOL .GT. 0) THEN
                IF(OBSEPO.NE.TIMREF) THEN
                  RESIDU(ISAT)=RESIDU(ISAT)+DPAR(I)*((OBSEPO-TIMREF)*
     1                         86400.D0)**(I-4)
                ELSE
                  IF(I.EQ.4) RESIDU(ISAT)=RESIDU(ISAT)+DPAR(I)
                ENDIF
              ELSE
                RESIDU(ISAT)=RESIDU(ISAT)+DOFFS(IEPOCH)*C
              ENDIF
435         CONTINUE
          ENDIF
          RESIDU(ISAT)=RESIDU(ISAT)-DPHI(ISAT)
C
C  COMPUTE SATELLITE DEPENDED ACCURACY VARIABLE
          IF (FLGACT(ISAT) .EQ. 'G')
     1    RMSSAT(INDSVN(SATNRA(ISAT)),IFIL) =
     2                RMSSAT(INDSVN(SATNRA(ISAT)),IFIL) +
     3                RESIDU(ISAT)*RESIDU(ISAT)
C  WRITE ON RESIDUAL FILE
          IF (IRSSAV.EQ.1.AND.FLGACT(ISAT).EQ.'G') THEN
            IEPOCH = IDNINT((OBSEPO - TIMREF)/IDELTT*86400.D0)+1
            WRITE(LFNRES) IFIL,IEPOCH,FILFRQ,SATNRA(ISAT),0,
     1                    RESIDU(ISAT),FLGAMB
          ENDIF
C SAVE COV-INFORMATION FOR CLOCK RINEX FILE
          IF (CLKHED%NUMTYP /= 0 .AND. FLGACT(ISAT) == 'G') THEN
            DO jEpo = 1,SIZE(clkrec%epoch)
              IF (DABS(CLkHed%TFirst+ClkRec%Epoch(jEpo)/86400d0 -
     1             OBSEPO) < 0.1d0/86400d0) THEN
                IF (ISATOK.GT.0) THEN
                  ClkRec%Sigma(ifil,jepo)=ClkRec%Sigma(ifil,jepo) +
     1                 ((RESIDU(ISAT)/C)**2)/DBLE(ISATOK)
                ELSE
                  ClkRec%Sigma(ifil,jepo) = unDef
                ENDIF
              ENDIF
            ENDDO
          ENDIF
440     CONTINUE
C  WRITE ON PRINT CHANNEL
        IF(PRIOPT(2) .EQ. 1 .AND. IRSSAV .NE. 1) THEN
          NLINES=NLINES+1
          ISEL=2
          CALL CDPRES(PGMNAM,CAMPGN,STITLE,OBSFIL,FILFRQ,
     1          ISEL,ICLPOL,NSATEL,SATNUM,INDSVN,OBSEPO,TIMREF,
     2          IDELTT,OFFS,NSACT,SATNRA,FLGACT,RESIDU)
          IF(NLINES.EQ.NUMLIN) THEN
            ISEL=3
            CALL CDPRES(PGMNAM,CAMPGN,STITLE,OBSFIL,FILFRQ,
     1            ISEL,ICLPOL,NSATEL,SATNUM,INDSVN,OBSEPO,TIMREF,
     2            IDELTT,OFFS,NSACT,SATNRA,FLGACT,RESIDU)
            NLINES=0
          END IF
        ENDIF
450   CONTINUE
C
455   CONTINUE
C
C  CLOSE SCRATCH FILE
C  ------------------
      CLOSE (UNIT=LFNSCR)
C
      RETURN
C
      END SUBROUTINE

      END MODULE
