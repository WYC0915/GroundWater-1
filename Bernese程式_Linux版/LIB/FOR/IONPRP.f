      MODULE s_IONPRP
      CONTAINS

C*
      SUBROUTINE IONPRP(MODTYP,IFIL,MTYP,NOBS,PRTLEV,NFRQ,NSVN,NUMSVN,
     1                  DPHI,FLAG,IDELT,REFTIM,TIMOBS,IPOL,RMSOBS,
     2                  INTMAX,LOCQ,NIONP,NUMAMB,AMBIEP,
     3                  NAMBP,OFFSET)
CC
CC NAME       :  IONPRP
CC
CC PURPOSE    :  PREPROCESSING OF OBSERVATION FILES
CC               CODE:   OUTLIERS ARE MARKED
CC               PHASE:  FOR EACH CYCLE SLIP DETECTED IN THE
CC                       PREPROCESSING A NEW AMBIGUITY PARAMETER
CC                       WILL BE SET UP  (ARRAY LOCQ)
CC
CC PARAMETERS :
CC         IN :  MODTYP : MODEL TYPE (1: SINGLE LAYER)        I*4
CC               IFIL   : FILE NUMBER                         I*4
CC               MTYP   : OBSERVATION TYPE (1:PHASE,2:CODE)   I*4
CC               NOBS   : NUMBER OF OBSERVATIONS              I*4
CC               PRTLEV : PRINT LEVEL (0:NO/1:YES)            I*4
CC               NFRQ   : NUMBER OF FREQUENCY FOR
CC                        PREPROCESSING: L3=3,L4=4            I*4
CC               NSVN   : NUMBER OF SATELLITES                I*4
CC               NUMSVN : SATELLITE NUMBERS
CC                        NUMSVN(I), I=1,MAXSAT               I*4(*)
CC               DPHI   : OBSERVATIONS
CC                        DPHI(I,J,K),I=1,MAXEPO
CC                                    J=1,MAXSAT
CC                                    K=1,2: FREQUENCIES      R*8(*,*,*)
CC               FLAG   : OBSERVATION FLAG                   CH*1(*,*,*)
CC               IDELT  : OBSERVATION INTERVAL                I*4
CC               REFTIM : REFERENCE EPOCH                     R*8
CC               TIMOBS : OBSERVATION EPOCHS                  R*8(*)
CC               IPOL   : POLYNOMIAL DEGREE FOR PREPROC.      I*4
CC               RMSOBS : RMS OF ONE OBSERVATION              R*8
CC               INTMAX : PREPROCESSING PARAMETERS            I*4
CC               LOCQ   : PARAMETER INFORMATION ARRAY
CC                        LOCQ(I,J), I=1,MAXLCQ
CC                                   J=1,MAXPAR               I*4(*,*)
CC               NIONP  : NUMBER OF IONOSPHERE PARAMETERS     I*4
CC        OUT :  NUMAMB : NUMBER OF AMBIGUITIES PER SVN
CC                        NUMAMB(I), I=1,MAXSAT               I*4(*)
CC               AMBIEP : NUMBER OF EPOCHS WHEN A NEW AMB.
CC                        PARAMETER HAS TO BE SET UP
CC                        AMBIEP(I,J), I=1,MAXAMP
CC                                     J=1,MAXSAT             I*4(*,*)
CC               NAMBP  : NUMBER OF AMBIGUITY PARAMETERS      I*4
CC               OFFSET : OFFSET IN PHASE OBSERVATIONS
CC                        (CYCLE SLIP CORRECTION)
CC                        OFFSET(I,J), I=1,MAXMAP
CC                                     J=1,MAXSAT             R*8(*,*)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  U. WILD
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  89/08/04 14:28
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1989     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE f_tstflg
      USE s_chkobs
      USE s_setflg
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IAMB  , IDELT , IEPOCH, IFIL  , IFRQ  , INTMAX, IOBS  ,
     1          IPAR  , IPOL  , ISAT  , ITYP  , JJ    , KK    , LAST  ,
     2          MODTYP, MTYP  , MXCAMB, MXCAMP, MXCEPO, MXCLCQ, MXCPAR,
     3          MXCSAT, NAMBP , NBAD  , NFRQ  , NFRQS , NIONP , NOBS  ,
     4          NSLIPS, NSVN
C
      REAL*8    REFTIM, RMSOBS
C
CCC       IMPLICIT REAL*8(A-H,O-Z)
C
C DECLARATIONS
C ------------
      REAL*8        OFFSET(MXCAMP,MXCSAT),DPHI(MXCEPO,MXCSAT,2)
      REAL*8        TIMOBS(MXCEPO)
C
      INTEGER*4     NUMAMB(MXCSAT),AMBIEP(MXCAMP,MXCSAT)
      INTEGER*4     LOCQ(MXCLCQ,MXCPAR),NUMSVN(MXCSAT)
      INTEGER*4     PRTLEV
C
      CHARACTER*6   MXNEPO,MXNSAT,MXNAMP,MXNPAR,MXNLCQ,MXNAMB
      CHARACTER*1   FLAG(MXCEPO,MXCSAT,2)
C
C COMMONS
C -------
      COMMON/MCMEPO/MXCEPO,MXNEPO
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMAMP/MXCAMP,MXNAMP
      COMMON/MCMPAR/MXCPAR,MXNPAR
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
      COMMON/MCMAMB/MXCAMB,MXNAMB
C
C LOGICAL FILE NUMBERS
C --------------------
C
      IF (NFRQ .EQ. 3) THEN
        NFRQS = 2
      ELSE
        NFRQS = 1
      ENDIF
C
C INITIALIZATION OF ARRAYS
C ------------------------
      DO 10 ISAT=1,MXCSAT
        NUMAMB(ISAT) = 0
10    CONTINUE
C
      DO 30 JJ=1,MXCAMP
        DO 30 KK=1,MXCSAT
          OFFSET(JJ,KK) = 0.D0
          AMBIEP(JJ,KK) = 0
30    CONTINUE
C
      NAMBP = 0
C
C IF TWO NEIGHBOURING OBS ARE EQUAL: MOST PROBABLY ERROR
C ------------------------------------------------------
      DO 80 IFRQ=1,NFRQS
        DO 70 ISAT=1,NSVN
          DO 40 IOBS=1,NOBS
            IF(.NOT.TSTFLG(FLAG(IOBS,ISAT,IFRQ),0)) GOTO 50
40        CONTINUE
50        LAST=IOBS
          DO 60 IOBS=LAST+1,NOBS
            IF(.NOT.TSTFLG(FLAG(IOBS,ISAT,IFRQ),0)) THEN
              IF(DPHI(IOBS,ISAT,IFRQ).EQ.DPHI(LAST,ISAT,IFRQ)) THEN
                CALL SETFLG(FLAG(LAST,ISAT,IFRQ),0)
                CALL SETFLG(FLAG(IOBS,ISAT,IFRQ),0)
              END IF
              LAST=IOBS
            END IF
60        CONTINUE
C
70      CONTINUE
80    CONTINUE
C
C PREPROCESSING (CHECK FOR CYCLE SLIPS OR OUTLIERS(CODE))
C -------------------------------------------------------
      DO 100 ISAT=1,NSVN
        IF (PRTLEV .GT. 0) THEN
          WRITE(LFNPRT,101) NUMSVN(ISAT)
101       FORMAT(/,' SATELLITE: ',I5,/,1X,16('-'))
        ENDIF
        CALL CHKOBS(MTYP,PRTLEV,IPOL,INTMAX,NOBS,REFTIM,IDELT,
     1              RMSOBS,TIMOBS,DPHI(1,ISAT,1),FLAG(1,ISAT,1),
     2              NBAD,NSLIPS)
        IF (MTYP .EQ. 1) THEN
          DO 90 IOBS=1,NOBS
            IF (TSTFLG(FLAG(IOBS,ISAT,1),1)) THEN
              NUMAMB(ISAT) = NUMAMB(ISAT)+1
              IF (NUMAMB(ISAT) .GT. MXCAMB) THEN
                WRITE(LFNERR,91) NUMAMB(ISAT),MXCAMB
91              FORMAT(/,' *** SR IONPRP: TOO MANY AMB. PARAMETERS',/,
     1                 16X,'NUMBER OF AMB.:',I6,/,
     2                 16X,'MAXIMUM NUMBER  :',I6,/)
                CALL EXITRC(2)
              ENDIF
              IEPOCH=IDNINT((TIMOBS(IOBS)-REFTIM)*86400.D0/IDELT)+1
              AMBIEP(NUMAMB(ISAT),ISAT) = IEPOCH
              OFFSET(NUMAMB(ISAT),ISAT) = DPHI(IOBS,ISAT,NFRQS)
            ENDIF
90        CONTINUE
        ENDIF
100   CONTINUE
C
C UPDATE AMBIGUITY PARAMETERS INFORMATION
C ---------------------------------------
      IF (MTYP .EQ. 1 .AND. MODTYP .EQ. 1) THEN
        ITYP = 4
        IPAR = NIONP
        DO 120 ISAT=1,NSVN
          DO 110 IAMB=1,NUMAMB(ISAT)
            IPAR = IPAR+1
            NAMBP = NAMBP+1
C
            IF (NAMBP .GT. MXCAMP) THEN
              WRITE (LFNERR,102) NAMBP,MXCAMP
102           FORMAT(/,' *** SR IONPRP : TOO MANY AMBIG. PARAM.',/,
     1                               17X,'NUMBER OF AMB. PAR.:',I3,/,
     2                               17X,'MAXAMP             :',I3,/)
              CALL EXITRC(2)
            ENDIF
C
            LOCQ(1,IPAR) = ITYP
            LOCQ(2,IPAR) = IFIL
            LOCQ(3,IPAR) = NUMSVN(ISAT)
            LOCQ(4,IPAR) = 0
            LOCQ(5,IPAR) = 4
            LOCQ(6,IPAR) = AMBIEP(IAMB,ISAT)
            IF (IAMB .NE. NUMAMB(ISAT)) THEN
              LOCQ(7,IPAR) = AMBIEP(IAMB+1,ISAT)-1
            ELSE
              LOCQ(7,IPAR) = 99999
            ENDIF
110       CONTINUE
120     CONTINUE
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
