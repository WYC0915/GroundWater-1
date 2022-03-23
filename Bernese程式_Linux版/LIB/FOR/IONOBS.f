      MODULE s_IONOBS
      CONTAINS

C*
      SUBROUTINE IONOBS(HFIL  ,OFIL  ,MTYP  ,REFTIM,ICARR ,IDELT ,
     1                  NSVN  ,NUMSVN,IFLFMT,NOBS  ,DPHI  ,FLAG  ,
     2                  OBSFLG,NRSAT ,IFRQS ,OBSERV,TIMOBS,NOBSVN,
     3                  TIMEND)
CC
CC NAME       :  IONOBS
CC
CC PURPOSE    :  INITIALIZES FILE SPECIFIC ARRAYS AND READS IN ALL
CC               OBSERVATIONS OF ONE FILE
CC
CC PARAMETERS :
CC         IN :  HFIL   : HEADER FILE NAME                   CH*32
CC               OFIL   : OBSERVATION FILE NAME              CH*32
CC               MTYP   : MEASURMENT TYPE (1:PHASE,2:CODE)    I*4
CC               REFTIM : REFERENCE EPOCH                     R*8
CC               ICARR  : CARRIER TO BE READ FROM FILE        I*4
CC               IDELT  : SAMPLING RATE  (SEC)                I*4
CC               NSVN   : NUMBER OF SATELLITES                I*4
CC               NUMSVN : SATELLITE NUMBERS I*4(*)
CC               IFLFMT : FILE FORMAT NUMBER                  I*4
CC        OUT :  NOBS   : NUMBER OF OBSERVATIONS              I*4
CC               DPHI   : OBSERVATION - ARRAY
CC                        DPHI(I,J,K), I = 1,...,MAXEPO
CC                                     J = 1,...,MAXSAT
CC                                     K = 1,2: FREQUENCIES   R*8(*,*,*)
CC               FLAG   : OBSERVATION FLAG                   CH*1(*,*,*)
CC               OBSFLG : OBSERVATION FLAG (ONE EPOCH)       CH*1(*,*)
CC               NRSAT  : SATELLITE NUMBERS (ONE EPOCH)       I*4(*)
CC               IFRQS  : FREQUENCIES REQUESTED               I*4(*)
CC               OBSERV : OBSERVATIONS OF ONE EPOCH           R*8(*,*)
CC               TIMOBS : OBSERVATION EPOCHS                  R*8(*)
CC               NOBSVN : NUMBER OF OBSERVATIONS PER SATELLITE
CC                        NOBSVN(I),I=1,100                   I*4(*)
CC               TIMEND : LAST OBS. EPOCH                     R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  U. WILD
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  89/08/04 15:12
CC
CC CHANGES    :  28-DEC-92 : ??: USE OF SR "OPNFIL" TO OPEN FILES
CC               29-OCT-93 : MR: MARK BAD OBSERV. WITH SATCRUX-FILE
CC               17-AUG-94 : MR: MAXBAD=200 (OLD: MAXBAD=20)
CC               10-SEP-95 : LM: REORDER DATA STATEMENT
CC               23-AUG-96 : TS: MAXBAD IN INCLUDE FILE
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               26-FEB-08 : RD: USE GTSATB FROM D_SATCRX
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1989     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE m_maxdim, ONLY: MAXBAD
      USE d_satcrx, ONLY: gtsatb
      USE s_rdobsi
      USE s_opnfil
      USE s_clrflg
      USE s_opnerr
      USE s_setflg
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IBAD  , ICARR , IDELT , IEPO  , IFIRST, IFLFMT,
     1          IFRQ  , IOBS  , IOSTAT, IRC   , IRETRN, ISAT  , ISVN  ,
     2          LFNOBS, MTYP  , MXCEPO, MXCFRQ, MXCSAT, NBAD  , NFRQS ,
     3          NOBS  , NSAT  , NSVN
C
      REAL*8    OBSTIM, REFTIM, TIMEND
C
CCC       IMPLICIT REAL*8(A-H,O-Z)
C
C
C COMMONS
C -------
      COMMON/MCMEPO/MXCEPO,MXNEPO
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMFRQ/MXCFRQ,MXNFRQ
C
C DECLARATIONS
C ------------
      REAL*8       TIMOBS(MXCEPO),DPHI(MXCEPO,MXCSAT,2)
      REAL*8       OBSERV(MXCSAT,MXCFRQ),DELTAT(2),TIMBAD(2,MAXBAD)
C
      INTEGER*4    NUMSVN(MXCSAT)
      INTEGER*4    NAVSAT(100),IFRQS(MXCFRQ),NRSAT(MXCSAT)
      INTEGER*4    NOBSVN(100)
      INTEGER*4    IOBBAD(MAXBAD),IACBAD(MAXBAD),SATBAD(MAXBAD)
C
      CHARACTER*32 HFIL,OFIL,FILCRX
      CHARACTER*6  MXNEPO,MXNSAT,MXNFRQ
      CHARACTER*1  FLAG(MXCEPO,MXCSAT,MXCFRQ)
      CHARACTER*1  OBSFLG(MXCSAT,MXCFRQ),EPOFLG
C
C
      DATA IFIRST/1/
C
C GET BAD SATELLITES FROM SATCRUX-FILE
C ------------------------------------
      IF (IFIRST.EQ.1) THEN
        IFIRST=0
        CALL GTFLNA(0,'SATCRUX',FILCRX,IRC)
        CALL GTSATB(MAXBAD,FILCRX,NBAD,SATBAD,IOBBAD,IACBAD,TIMBAD)
      ENDIF
C
      LFNOBS = LFN001
C
C INITIALIZE ARRAYS
C ------------------
      DO 30 IEPO=1,MXCEPO
        TIMOBS(IEPO) = 0.D0
        DO 20 ISAT=1,MXCSAT
          DO 10 IFRQ=1,MXCFRQ
            CALL CLRFLG(FLAG(IEPO,ISAT,IFRQ),0)
            CALL CLRFLG(FLAG(IEPO,ISAT,IFRQ),1)
            DPHI(IEPO,ISAT,IFRQ) = 0.D0
10        CONTINUE
20      CONTINUE
30    CONTINUE
C
      DO 40 I = 1,100
        NOBSVN(I) = 0
40    CONTINUE
C
C NAVSAT(K) CONTAINS INDEX, WHERE TO STORE OBS OF SV NUMBER K INTO DPHI
C ---------------------------------------------------------------------
      DO 50 ISAT=1,NSVN
        NAVSAT(NUMSVN(ISAT))=ISAT
50    CONTINUE
C
C CARRIER TO BE READ FROM FILE
C ----------------------------
      IF (ICARR .EQ. 3) THEN
        NFRQS = 2
        IFRQS(1) = 3
        IFRQS(2) = 4
      ELSE IF (ICARR .EQ. 4) THEN
        NFRQS = 1
        IFRQS(1) = 4
      ENDIF
C
C OPEN OBSERVATION FILE
C ---------------------
      CALL OPNFIL(LFNOBS,OFIL,'OLD','UNFORMATTED',
     1            'READONLY',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNOBS,IOSTAT,OFIL,'IONOBS')
C
C LOOP OVER ALL OBSERVATION EPOCHS
C --------------------------------
100   CONTINUE
C
        CALL RDOBSI(LFNOBS,IFLFMT,NFRQS,IFRQS,OBSTIM,DELTAT,EPOFLG,
     1              NSAT,NRSAT,OBSFLG,OBSERV,IRETRN)
        IF (IRETRN .NE. 0) GOTO 110
C
C COMPUTE AND CHECK OBSERVATION NUMBER
C ------------------------------------
        IOBS=IDNINT((OBSTIM-REFTIM)*86400.D0/IDELT)+1
        IF (IOBS .LT. 0 .OR. IOBS .GT. MXCEPO) THEN
          WRITE(LFNERR,101) IOBS,MXCEPO
101       FORMAT(/,' *** SR IONOBS: ERROR IN EPOCH NUMBER',/,
     1                             15X,'EPOCH NUMBER: ',I6,/,
     2                             15X,'MAXEPO      : ',I6)
          GOTO 100
        ENDIF
        TIMOBS(IOBS)=OBSTIM
C
C STORE OBSERVATIONS OF ONE EPOCH IN ARRAY DPHI
C ---------------------------------------------
        DO 90 I=1,NSAT
          ISVN=NRSAT(I)
          NOBSVN(NAVSAT(ISVN)) = NOBSVN(NAVSAT(ISVN)) + 1
          DO 80 IFRQ=1,NFRQS
            FLAG(IOBS,NAVSAT(ISVN),IFRQ)=OBSFLG(I,IFRQ)
            CALL CLRFLG(FLAG(IOBS,NAVSAT(ISVN),IFRQ),1)
C
C CHECK SATELLITE PROBLEM FILE FOR BAD SATELLITE
            DO 115 IBAD=1,NBAD
              IF (SATBAD(IBAD).EQ.NRSAT(I)    .AND.
     1            TIMBAD(1,IBAD).LE.OBSTIM    .AND.
     2            TIMBAD(2,IBAD).GT.OBSTIM    .AND.
     3            (IOBBAD(IBAD).EQ.MTYP       .OR.
     4             IOBBAD(IBAD).EQ.3)) THEN
                CALL SETFLG(FLAG(IOBS,NAVSAT(ISVN),IFRQ),0)
              ENDIF
115         CONTINUE
C
            IF(OBSERV(I,IFRQ).EQ.0.D0) THEN
              CALL SETFLG(FLAG(IOBS,NAVSAT(ISVN),IFRQ),0)
            ELSE
C
              IF(MTYP.EQ.1)THEN
                DPHI(IOBS,NAVSAT(ISVN),IFRQ)=-OBSERV(I,IFRQ)
CC              DPHI(IOBS,NAVSAT(ISVN),IFRQ)=OBSERV(I,IFRQ)
              ELSE
                DPHI(IOBS,NAVSAT(ISVN),IFRQ)=OBSERV(I,IFRQ)
              ENDIF
            ENDIF
C
80        CONTINUE
90      CONTINUE
C
C NEXT OBSERVATION EPOCH
C ----------------------
      GOTO 100
C
C CLOSE OBSERVATION FILE
C ----------------------
110   CLOSE(UNIT=LFNOBS)
C
C HIGHEST ELEMENT IN DPHI,TIMOBS ETC
C ----------------------------------
      NOBS=IOBS
      TIMEND = REFTIM + (NOBS*IDELT)/86400.D0
C
      RETURN
      END SUBROUTINE

      END MODULE
