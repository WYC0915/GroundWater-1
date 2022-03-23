      MODULE s_PRIGEN
      CONTAINS

C*
      SUBROUTINE PRIGEN(TITLES,SIGAPR,ICOELV,NORRES,CORSTR,DTSIM ,
     1                  NFTOT ,CSESS ,NSESS ,SESSID,FILNUM,STRAMB,
     2                  SIGAMB,AR2MOD,AR2INF,ISYNCR,IPOLAR,IZEROD,
     3                  IQXX  ,IPHSEP,ISASYS,IREL2)
CC
CC NAME       :  PRIGEN
CC
CC PURPOSE    :  PRINT GENERAL OPTIONS USED AND SESSION INFORMATION
CC
CC PARAMETERS :
CC         IN :  TITLES(I),I=1,2: TITLE LINES                 CH*132
CC               SIGAPR : A PRIORI SIGMA OF UNIT WEIGHT (M)   R*8
CC               ICOELV(1:2,J) : MODEL FOR ELEV.-DEP.         I*4(2,*)
CC                          OBS. WEIGHTING
CC                         I=1: STATION, AIRSPACE
CC                         I=2: LEO
CC                         J=MEATYP
CC                        =0: EQUAL WEIGHTING FOR ALL OBS.
CC                        >0: MODEL NUMBER (SEE SR WGTELV)
CC               NORRES : RESIDUAL COMPUTATION                I*4
CC                        =1: REAL RESIDUALS SAVED
CC                        =2: L1-NORMALIZED RESIDUALS SAVED
CC                        =3: NORMALIZED WITH APRIORI WGT ONLY
CC               CORSTR : CORRELATION STRATEGY                I*4
CC               DTSIM  : INTERVAL TO IDENTIFY EPOCH          R*8
CC               NFTOT  : TOTAL NUMBER OF FILES               I*4
CC               CSESS(2,I),I=1,..,NFTOT: SESSION IDENTIFIERS CH*4
CC                        FOR FILE I
CC               NSESS  : NUMBER OF SESSIONS TO BE PROCESSED  I*4
CC               SESSID(I),I=1,..,NSESS: SESSION IDENTIFIERS  CH*4
CC               FILNUM(I),I=1,..,MAXFLS: ARRAY TO STORE FILE I*4
CC                        NUMBERS OF ONE SESSION
CC               STRAMB : (1): AMBIGUITY RESOLUTION STRATEGY  I*4(*)
CC                             =-1: AMBIGUITY PRE-ELIMINATION
CC                             = 0: NO AMBIGUITY RESOLUTION
CC                             = 1: ROUND-TO-NEAREST-INTEGER
CC                             = 2: GENERAL SEARCH
CC                             = 3: SIGMA-DEPENDENT
CC                             = 4: QUASI-IONOSPHERE-FREE
CC                             = 5: LAMBDA
CC                        (2): AMBIGUITY PRE-ELIMINATION
CC                             =-1: ONCE PER SESSION
CC                             = 0: EVERY EPOCH
CC                             = N: EVERY N SECONDS
CC                        (3): SELECTION OF GNSS
CC                             = 0: ALL
CC                             = 1: GPS
CC                             = 2: GLONASS
CC                             = 3: GALILEO
CC                        (4): CONSIDEREDD GPS QUARTER-CYCLE BIASES
CC                             = 0: NEVER
CC                             = 1: IF INDICATED
CC                             = 2: ALWAYS
CC                             =-N: PRN NUMBER N
CC               SIGAMB : OPTIONS FOR SIGMA-DEPENDENT OR      R*8(4)
CC                        QIF AMBIGUITY RESOLUTION STRATEGY
CC                   IF STRAMB(1)=3 (SIGMA)
CC                          SIGAMB(1): AT LEAST 1 INTEGER WITHIN
CC                                     SIGAMB(1)*SIGMA ALLOWED
CC                          SIGAMB(2): MAXIMUM  SIGMA ALLOWED FOR
CC                                     THE AMBIGUITY WHICH SHOULD
CC                                     BE RESOLVED
CC                          SIGAMB(3): MINIMAL SIGMA USED IN TEST
CC                          SIGAMB(4): MAX. NUMBER OF AMBIGUITIES
CC                             TO BE SOLVED IN ONE ITERATION STEP
CC                          SIGAMB(5): GLONASS AMBIGUITY RESOLUTION
CC                                     BETWEEN DIFFERENT FREQUENCY
CC                                     CHANNELS:
CC                                     = 0: NEVER
CC                                     = 1: SAME REVEIVER TYPE
CC                                     = 2: SAME REVEIVER MODEL
CC                                     = 3: SAME REVEIVER GROUP
CC                                     =-1: ALWAYS
CC                    IF STRAMB(1)=4 (QIF)
CC                          SIGAMB(1): SEARCH WIDTH IN WIDE-LANE
CC                                     CYCLES
CC                          SIGAMB(2): MAXIMAL ALLOWED RMS ERROR
CC                                     OF NARROW-LANE AMBIGUITY
CC                                     (BET13*X1+BET23*X2) IN
CC                                     NARROW-LANE CYCLES
CC                          SIGAMB(3): MAXIMAL ALLOWED DISTANCE IN
CC                                 L1&L2 SPACE FROM GRID POINT WHICH IS
CC                                 SUPPOSED TO BE THE CORRECT SOLUTION
CC                                 (IN NARROW-LANE CYCLES)
CC                          SIGAMB(4): MAX. NUMBER OF AMBIGUITIY PAIRS
CC                             TO BE SOLVED IN ONE ITERATION STEP
CC                    IF STRAMB(1)=5 (LAMBDA)
CC                          SIGAMB(1): MAXIMUM ALLOWED RMS RATIO
CC                                     CONCERNING FIXED TO FLOAT
CC                          SIGAMB(2): MAXIMUM ALLOWED RMS OF UNIT
CC                                     WEIGHT FOR FIXED SOLUTION
CC                          SIGAMB(3): RESOLUTION MODE CONCERNING
CC                                     INVOLVED GNSS
CC                                     =1: GNSS BY GNSS
CC                                     =2: MIXED-GNSS
CC                                     =3: SEPARATE-GNSS
CC               AR2MOD : MODE OF PROCESSING FOR AMBIGUITY    I*4
CC                        RESOLUTION STRATEGY 2
CC                        =0 : RESOLVE ALL PRESENT AMBIGUITIES
CC                        =1 : BASELINE-WISE AMBIGUITY RESOLUTION
CC               AR2INF : INFORMATION FOR A.R., STRATEGY 2    R*8
CC                        (1) : SEARCH WIDTH IN UNITS OF STD DEV
CC                        (2) : MAX ALLOWED RMS(FIXED)/RMS(FLOAT)
CC                        (3) : MIN ALLOWED
CC                              RMS(2-ND AMB)/RMS(1-ST AMB)
CC                        (4) : SEARCH WIDTH FOR GEOMETRY-
CC                              FREE LC (IN L1 CYCLES)
CC                              =X : USE THIS VALUE
CC                              =0 : COMPUTE FORMAL WIDTH
CC               ISYNCR : =1: APPLY SYNCHRONIZATION ERRORS    I*4
CC               IPOLAR : POLARIZATION EFFECT                 I*4
CC                        =0: NONE, =1: GEOM., =2: FULL
CC               IZEROD : FLAG TO IDENTIFY CODE OR PHASE      I*4
CC                        ONLY (=2) AND CODE+PHASE (=1) SOLUTION
CC               IQXX   : FLAG FOR VAR/COV COMP.(YES=1,NO=0)  I*4
CC               IPHSEP : ONLY PHASE FOR RESUBST OF EPO-PARAM. I*4
CC               ISASYS : SATELLITE SYSTEM TO BE CONSIDERED   I*4
CC                        =0: ALL
CC                        =1: GPS
CC                        =2: GLONASS
CC               IREL2  : FLAG FOR PERIODIC RELATIVISTIC J2   I*4
CC                        CORRECTION
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/18 16:44
CC
CC CHANGES    :  27-MAY-91 : ??: DON'T PRINT TRAILING BLANKS
CC               31-JUL-92 : ??: OPTIONS FOR "FARA" (AR2MOD,AR2INF)
CC               04-AUG-92 : LM: NEW OPTION SIGAMB(4)
CC               10-NOV-92 : ??: BASELINE-WISE AND NETWORK-WISE
CC                               RESOLUTION WERE EXCHANGED
CC               26-APR-93 : ??: PRE-ELIMINATION OF ANY PARAMETER TYPE
CC               09-APR-94 : MR: CORRECT DESCRIPTION OF SIGMA STRATEGY
CC               13-APR-94 : LM: STRATEGY 4
CC               25-JUL-94 : MR: PRE-ELIMINATION STATISTICS SIMPLIFIED
CC               10-AUG-94 : MR: CALL EXITRC
CC               12-AUG-94 : MR: FORMAT 4: SESSION AS CHARACTER*4
CC               12-AUG-94 : SS: NEW: SEARCH WIDTH FOR L4 LC
CC               22-AUG-94 : SS: NEW LOGIC FOR "AR2INF(4)"
CC               14-AUG-95 : LM: STRAMB=ARRAY
CC               05-SEP-95 : MR: CORRECT FORMAT FOR SESSION LIST
CC               30-JAN-97 : MR: ELEV.DEP. OBS. WEIGHTING
CC               03-APR-97 : SS: TEXT FOR OBS. WEIGHTING MODEL
CC               11-AUG-97 : SS: NEW OPTION "STRAMB(2)"
CC               08-OCT-97 : SS: RESIDUAL NORMALIZATION
CC               30-OCT-97 : SS: SIGMA OF ZERO-DIFFERENCE OBS
CC               24-JUN-02 : DS: LEO ELEV.DEP.WEIGHT. FLAG IN ICOELV(2)
CC               05-MAR-03 : CU: REMOVE USE OF SKELETON FILE
CC               15-APR-03 : CU: BUG FIXED (FORMAT STATEMENTS)
CC               11-JUN-03 : HU: PRINT CONVENTION FOR TIDAL CORRECTIONS
CC               22-JUL-03 : RD: NORMALIZED ON APRIORI WEIGHTS ONLY
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               18-SEP-06 : HU: POLARIZATION EFFECT OPTION
CC               17-JUN-07 : RD: ONLY PHASE FOR RESUBSTITUTION OF EPO-PARAM.
CC               18-JUN-07 : RD: ICOELV INDIV. FOR EACH MEATYP
CC               26-OCT-10 : CR: PER. RELATIVISTIC J2-CORRECTION OPTION
CC               16-FEB-11 : SS: SIGAMB(5) FOR GLONASS AMBIGUITY RESOLUTION
CC               17-FEB-11 : SS: STRAMB(3) FOR SELECTION OF GNSS
CC               17-FEB-11 : SS: LAMBDA AMBIGUITY RESOLUTION STRATEGY
CC               18-FEB-11 : SS: IGNORED GPS QUARTER-CYCLE BIASES
CC               15-FEB-12 : SS: REFINED QUARTER-CYBLE BIAS HANDLING
CC               16-FEB-12 : SS: NEGATIVE STRAMB(4) (PRN NUMBER)
CC               20-AUG-12 : SL: SYNTAX ERROR IN WRITE STATEMENT CORRECTED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE d_const, ONLY: WGTPHA
      USE s_exitrc
      USE f_lengt1
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IF    , II    , IRCRES, ISES  , ISYNCR,
     1          MXCFLS, NFLSES, NFTOT , NORRES, NSESS , IPOLAR,
     2          IZEROD, IQXX  , IPHSEP, ISASYS, IREL2
C
      REAL*8    DTSIM , SIGAPR
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*132 TITLES(2)
      CHARACTER*48  STRSIG
      CHARACTER*32  FILNAM
      CHARACTER*24  AR2TXT(2)
      CHARACTER*19  AR3TXT(5)
      CHARACTER*13  AR5TXT(3)
      CHARACTER*7   AMBTXT(4)
      CHARACTER*6   MXNFLS
      CHARACTER*12  STRELV
      CHARACTER*4   CSESS(2,*),SESSID(*)
C
      REAL*8        SIGAMB(*),AR2INF(*)
C
      INTEGER*4     CORSTR,STRAMB(*),FILNUM(*)
      INTEGER*4     AR2MOD,ICOELV(2,*)
C
C
      COMMON/MCMFLS/MXCFLS,MXNFLS
C
      DATA AMBTXT/'ALL    ',
     1            'GPS    ',
     2            'GLONASS',
     3            'GALILEO'/
C
      DATA AR2TXT/'NETWORK-WISE RESOLUTION ',
     1            'BASELINE-WISE RESOLUTION'/
      DATA AR3TXT/'ALWAYS             ',
     1            'NEVER              ',
     2            'SAME RECEIVER TYPE ',
     3            'SAME RECEIVER MODEL',
     4            'SAME RECEIVER GROUP'/
      DATA AR5TXT/'GNSS BY GNSS ',
     1            'MIXED-GNSS   ',
     2            'SEPARATE-GNSS'/
C
C WRITE TITLE LINES
C -----------------
      WRITE(LFNPRT,1) TITLES(1)(1:LENGT1(TITLES(1))),
     1                TITLES(2)(1:LENGT1(TITLES(2)))
1     FORMAT(//,A,/,A,/,' ',131('-'),//)
C
      WRITE(LFNPRT,"(
     1     ' 3. GENERAL OPTIONS'
     2  ,/,' ------------------'
     3  ,/,' '
     3  ,/,' TIDAL CORRECTION OF STATION COORDINATES :'
     3    ,'   IERS CONVENTIONS 2000'
     3  ,/,' '
     3  ,/,' '
     4  ,/,' A PRIORI SIGMA OF UNIT WEIGHT:'
     5  ,/,' -----------------------------'
     6  ,/,1X)")
C
C GENERAL OPTIONS
C ---------------
C A PRIORI SIGMA AND MODEL FOR ELEVATION-DEPENDENT WEIGHTING

CBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
C      STRSIG='(SIGMA OF ONE-WAY L1 PHASE OBSERVABLE AT ZENITH)'
C      IF (ICOELV.EQ.0) THEN
C        STRSIG='(SIGMA OF ONE-WAY L1 PHASE OBSERVABLE)'
C        STRELV='NONE'
C      ELSEIF (ICOELV.EQ.1) THEN
C        STRELV='1/COS(Z)'
C      ELSE
C        WRITE(STRELV,'(I1)') ICOELV
C      ENDIF
C      IF (WGTPHA.NE.1.D0) STRSIG=' '
C      WRITE(LFNPRT,3) SIGAPR,STRSIG(1:LENGT1(STRSIG)),
C     1  STRELV(1:LENGT1(STRELV))
C3     FORMAT(' A PRIORI SIGMA OF UNIT WEIGHT           : ',F7.3,' M',
C     1  2X,A,//
C     2  ' MODEL FOR ELEVATION-DEPENDENT WEIGHTING :   ',A)

      STRSIG='(SIGMA OF ONE-WAY L1 PHASE OBSERVABLE AT ZENITH)'
      IF (ICOELV(1,1).EQ.0) THEN
        STRSIG='(SIGMA OF ONE-WAY L1 PHASE OBSERVABLE)'
        STRELV='NONE'
      ELSEIF (ICOELV(1,1).EQ.1 .AND. ICOELV(1,2).EQ.2) THEN
        STRELV='1/COS(Z) FOR PHASE    1/COS(Z)**2 FOR CODE'
      ELSEIF (ICOELV(1,1).EQ.1) THEN
        STRELV='1/COS(Z)'
      ELSEIF (ICOELV(1,1).EQ.2) THEN
        STRELV='1/COS(Z)**2'
      ELSE
        WRITE(STRELV,'(I1)') ICOELV(1,1)
      ENDIF

      IF (WGTPHA.NE.1.D0) STRSIG=' '
      WRITE(LFNPRT,3) SIGAPR,STRSIG(1:LENGT1(STRSIG)),
     1  STRELV(1:LENGT1(STRELV))
3     FORMAT(' A PRIORI SIGMA OF UNIT WEIGHT           : ',F7.3,' M',
     1  2X,A,//
     2  ' MODEL FOR ELEVATION-DEPENDENT WEIGHTING :   ',A)



CBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB



C
C PRINT RESIDUAL COMPUTATION MODE
      CALL GTFLNA(0,'RESIDRS',FILNAM,IRCRES)
      IF (IRCRES.EQ.0) THEN
        IF (NORRES.EQ.1) THEN
          WRITE(LFNPRT,31)
31        FORMAT(/,' RESIDUAL COMPUTATION MODE               :   ',
     1      'REAL RESIDUALS SAVED')
        ELSE IF (NORRES.EQ.2) THEN
          WRITE(LFNPRT,32)
32        FORMAT(/,' RESIDUAL COMPUTATION MODE               :   ',
     1      'L1-NORMALIZED RESIDUALS SAVED')
        ELSE
          WRITE(LFNPRT,33)
33        FORMAT(/,' RESIDUAL COMPUTATION MODE               :   ',
     1      'STORED RESIDUALS ARE NORMALIZED WITH A PRIORI WEIGHTS')
        ENDIF
      ENDIF
C
C POLARIZATION EFFECT
      IF (IPOLAR.EQ.1) THEN
        WRITE(LFNPRT,
     1    "(/,' POLARIZATION EFFECT                     :   ',A)")
     2                                                     'GEOMETRICAL'
      ELSEIF (IPOLAR.EQ.2) THEN
        WRITE(LFNPRT,
     1    "(/,' POLARIZATION EFFECT                     :   ',A)")
     2                                                     'FULL'
      ELSE
        WRITE(LFNPRT,
     1    "(/,' POLARIZATION EFFECT                     :   ',A)")
     2                                                  'NOT CONSIDERED'
      ENDIF
C
C PERIODIC RELATIVISTIC J2-CORRECTION
      IF(IREL2.EQ.0) THEN
        WRITE(LFNPRT,
     1    "(/,' PER. RELATIVISTIC J2-CORRECTION         :   ',A)")
     2                                                     'NOT APPLIED'
      ELSEIF(IREL2.EQ.1) THEN
        WRITE(LFNPRT,
     1    "(/,' PER. RELATIVISTIC J2-CORRECTION         :   ',A)")
     2                                                     'APPLIED'
      ENDIF
C
C CORRELATIONS
      WRITE(LFNPRT,"(
     1     ' '
     2  ,/,' '
     3  ,/,' CORRELATIONS AND SESSIONS:'
     4  ,/,' -------------------------'
     5  ,/,1X)")
C
      IF(CORSTR.EQ.1) WRITE(LFNPRT,"(
     1    ' STRATEGY       : CORRELATIONS WITHIN BASELINE')")
      IF(CORSTR.EQ.2) WRITE(LFNPRT,"(
     1    ' STRATEGY       : CORRELATIONS WITHIN OBSERVATIONS ',
     1    'OF THE SAME FREQUENCY')")
      IF(CORSTR.EQ.3) WRITE(LFNPRT,"(
     1    ' STRATEGY       : CORRELATIONS CORRECTLY MODELLED')")
C
C CORRECT CORRELATIONS: PRINT SESSIONS WITH THEIR FILES
      IF(CORSTR.NE.1) THEN
        WRITE(LFNPRT,4) DTSIM*86400.D0
4       FORMAT(' TIME INTERVAL  :',F11.5,' SEC (TO IDENTIFY EPOCH)')
C
        WRITE(LFNPRT,"(
     1       ' '
     2    ,/,' SESS  #FILE  FILE NUMBERS'
     3    ,/,1X,131('-')
     4    ,/,1X)")
C
C FIND FILES OF SESSION ISES
        DO 38 ISES=1,NSESS
          NFLSES=0
          DO 37 IF=1,NFTOT
            IF(CSESS(1,IF).NE.SESSID(ISES)) GOTO 37
            NFLSES=NFLSES+1
            FILNUM(NFLSES)=IF
37        CONTINUE
          IF(NFLSES.GT.MXCFLS) THEN
            WRITE(LFNERR,5) NFLSES,MXCFLS,SESSID(ISES)
5           FORMAT(/,' *** SR PRIGEN: TOO MANY FILES IN SESSION',/,
     1                         16X,'NUMBER OF FILES IN SESSION: ',I4,/,
     2                         16X,'MAXIMUM NUMBER OF FILES   : ',I4,/,
     3                         16X,'SESSION IDENTIFIER        : ',A4,/)
            CALL EXITRC(2)
          ENDIF
          WRITE(LFNPRT,6) SESSID(ISES),NFLSES,(FILNUM(I),I=1,NFLSES)
6         FORMAT(1X,A4,I5,3X,30(I4),/,10(13X,30(I4),/))
38      CONTINUE
      ENDIF
C
C BACKSUBSITUTION OF EPOCH PARAMETERS
      IF (IZEROD.NE.0) THEN
        WRITE(LFNPRT,"(/,' BACKSUBSITUTION OF EPOCH PARAMETERS:')")
        WRITE(LFNPRT,"(  ' ----------------------------------- ')")
        IF (IQXX.EQ.1) THEN
          WRITE(LFNPRT,
     1    "(/,' VAR-COVAR COMPUTATION FOR EPOCH-PARAMETERS:    ',A)")
     2                                            'CORRECTLY CONSIDERED'
        ELSE
          WRITE(LFNPRT,
     1    "(/,' VAR-COVAR COMPUTATION FOR EPOCH-PARAMETERS:    ',A)")
     2                                            'SIMPLIFIED'
        ENDIF
        IF (IPHSEP.EQ.1) THEN
          WRITE(LFNPRT,
     1    "(/,' OBSERVATIONS CONSIDERED FOR BACK-SUBSTITUTION: ',A)")
     2                                         'ONLY PHASE MEASUREMENTS'
        ELSE
          WRITE(LFNPRT,
     1    "(/,' OBSERVATIONS CONSIDERED FOR BACK-SUBSTITUTION: ',A)")
     2                                         'ALL MEASUREMENTS'
        ENDIF
      ENDIF
C
C AMBIGUITY RESOLUTION STRATEGY
      WRITE(LFNPRT,"(
     1     ' '
     2  ,/,' '
     3  ,/,' AMBIGUITY RESOLUTION STRATEGY:'
     4  ,/,' -----------------------------'
     5  ,/,1X)")
C
C ADDITIONAL INFORMATION FOR AMBIGUITY PRE-ELIMINATION
        IF (STRAMB(1).EQ.-1) THEN
          IF (STRAMB(2).EQ.-1) THEN
            WRITE(LFNPRT,"(
     1        ' AMBIGUITIES PRE-ELIMINATED ONCE PER SESSION')")
          ELSEIF (STRAMB(2).EQ.0) THEN
            WRITE(LFNPRT,"(
     1        ' AMBIGUITIES PRE-ELIMINATED EVERY EPOCH')")
          ELSE
            WRITE(LFNPRT,"(
     1        ' AMBIGUITIES PRE-ELIMINATED EVERY ',I5,' SECONDS')")
     2        STRAMB(2)
          ENDIF
        ENDIF
C
        IF (STRAMB(1).EQ.0) WRITE(LFNPRT,"(
     1    ' AMBIGUITIES NOT RESOLVED')")
        IF (STRAMB(1).EQ.1) WRITE(LFNPRT,"(
     1    ' AMBIGUITIES ROUNDED TO NEAREST INTEGERS')")
        IF (STRAMB(1).EQ.2) WRITE(LFNPRT,"(
     1    ' GENERAL SEARCH (FARA)')")
        IF (STRAMB(1).EQ.3) WRITE(LFNPRT,"(
     1    ' SIGMA-DEPENDENT AMBIGUITY RESOLUTION')")
        IF (STRAMB(1).EQ.4) WRITE(LFNPRT,"(
     1    ' QUASI-IONOSPHERE-FREE AMBIGUITY RESOLUTION (QIF)')")
        IF (STRAMB(1).EQ.5) WRITE(LFNPRT,"(
     1    ' LAMBDA AMBIGUITY RESOLUTION')")
C
        IF (STRAMB(1).GT.0) WRITE(LFNPRT,"(
     1    ' APPLIED TO: ',A)") TRIM(AMBTXT(STRAMB(3)+1))
C
        IF (STRAMB(4).EQ.0) WRITE(LFNPRT,"(
     1    ' GPS QUARTER-CYCLE PHASE BIASES: NOT CONSIDERED')")
        IF (STRAMB(4).EQ.1) WRITE(LFNPRT,"(
     1    ' GPS QUARTER-CYCLE PHASE BIASES: CONSIDERED',
     2    ' (IF INDICATED)')")
        IF (STRAMB(4).EQ.2) WRITE(LFNPRT,"(
     1    ' GPS QUARTER-CYCLE PHASE BIASES: CONSIDERED',
     2    ' (ALWAYS)')")
        IF (STRAMB(4).LT.0) WRITE(LFNPRT,"(
     1    ' GPS QUARTER-CYCLE PHASE BIASES: CONSIDERED',
     2    ' (ONLY FOR PRN NUMBER',I4,')')") -STRAMB(4)
C
C ADDITIONAL INFORMATION FOR GENERAL SEARCH AMBIGUITY RESOLUTION
      IF(STRAMB(1).EQ.2) THEN
        WRITE(LFNPRT,61) TRIM(AR2TXT(AR2MOD+1)),(AR2INF(I),I=1,4)
61      FORMAT(/,' SEARCH OPTIONS : ',A,/,
     1           18X,'SEARCH WIDTH (IN UNITS OF RMS)         :',F7.3,/,
     2           18X,'MAXIMUM FOR RMS(FIXED)/RMS(FLOAT)      :',F7.3,/,
     3           18X,'MINIMUM FOR RMS(2ND BEST)/RMS(BEST)    :',F7.3,/,
     4           18X,'SEARCH WIDTH FOR L4 (L1 CYCLES)        :',F7.3)
      ENDIF
C
C ADDITIONAL INFORMATION FOR SIGMA-DEP. AMBIGUITY RESOLUTION
      IF(STRAMB(1).EQ.3) THEN
        WRITE(LFNPRT,62) (SIGAMB(II),II=1,3),IDNINT(SIGAMB(4))
62      FORMAT(/,' CRITERIA:  EXACTLY 1 INTEGER AROUND REAL ',
     1           'ESTIMATE WITHIN',F7.3,' * SIGMA OF AMBIGUITY',/,
     2           '            MAXIMUM SIGMA OF AN AMBIGUITY ',
     3           'TO BE RESOLVED:',F7.3,' CYCLES',/,
     4           '            MINIMAL SIGMA OF AMBIGUITY IS ',
     5           'SET TO        :',F7.3,' CYCLES',/,
     6           '            MAXIMUM NUMBER OF AMBIG. RESOL',
     7           'VED IN 1 STEP :',I7)
        IF (ISASYS.NE.1) WRITE(LFNPRT,
     1         "('            GLONASS RESOLUTION BETWEEN ',
     2           'DIFFERENT FREQ.  :  ',A)")
     3    TRIM(AR3TXT(IDNINT(SIGAMB(5)+2)))
      ENDIF
C
C ADDITIONAL INFORMATION FOR QIF AMBIGUITY RESOLUTION
      IF(STRAMB(1).EQ.4) THEN
        WRITE(LFNPRT,63) (SIGAMB(II),II=1,3),IDNINT(SIGAMB(4))
63      FORMAT(/,' CRITERIA:  SEARCH WIDTH IN WIDE-LANE CYCLES',
     1           '            :',F8.3,'  WL CYCLES',/,
     2           '            MAX. RMS OF RESOLVABLE NARROW-LA',
     3           'NE AMBIGUITY:',F8.3,'  NL CYCLES',/,
     4           '            MAX. FRACT. PART OF RESOLVABLE N',
     5           'L AMBIGUITY :',F8.3,'  NL CYCLES',/,
     6           '            MAX. NUMBER OF AMBIG. PAIRS SOLV',
     7           'ED IN 1 STEP:',I8)
      ENDIF
C
C ADDITIONAL INFORMATION FOR LAMBDA AMBIGUITY RESOLUTION
      IF(STRAMB(1).EQ.5) THEN
        WRITE(LFNPRT,64) (SIGAMB(II),II=1,2)
64      FORMAT(/,' CRITERIA:  MAXIMUM ALLOWED RMS RATIO RE F',
     1           'IXED TO FLOAT :',F7.3,/,
     2           '            MAXIMUM ALLOWED RMS OF UNIT WE',
     3           'IGHT FOR FIXED:',F7.3,' METERS')
        IF (ISASYS.NE.1) WRITE(LFNPRT,
     1         "('            RESOLUTION MODE CONCERNING ',
     2           'INVOLVED GNSS    :  ',A)")
     3    TRIM(AR5TXT(IDNINT(SIGAMB(3))))
      ENDIF
C
C SYNCHRONIZATION ERRORS
      WRITE(LFNPRT,"(
     1     ' '
     2  ,/,' '
     3  ,/,' SYNCHRONIZATION ERRORS:'
     4  ,/,' ----------------------'
     5  ,/,1X)")
C
      IF (ISYNCR.EQ.0) WRITE(LFNPRT,"(
     1  ' STRATEGY       : SYNCHRONIZATION ERRORS NOT APPLIED')")
      IF (ISYNCR.EQ.1) WRITE(LFNPRT,"(
     1  ' STRATEGY       : SYNCHRONIZATION ERRORS APPLIED')")
C
      RETURN
      END SUBROUTINE

      END MODULE
