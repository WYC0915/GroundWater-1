      MODULE s_STDSAV
      CONTAINS
C*
      SUBROUTINE STDSAV(TITLE,NPAR,SIGMA0,LOCQ,XXX,Q,
     1                  NSTCEP,SCASTC,INTSTC,TIMSTC,
     2                  FILSTD,FILRPR,FILORB,IARCOF,
     3                  NMXINT,NMXSAP,NMXARC)
CC
CC NAME       :  STDSAV
CC
CC PURPOSE    :  UPDATE ORBITAL ELEMENTS, COMPUTE MEAN ERRORS OF
CC               ELEMEMTS, SAVE OLD OSCULATING ELEMENTS, NEW ELEMENTS
CC               AND COORESPONDING MEAN ERRORS ON FILE ORBITS
CC
CC PARAMETERS :
CC         IN :  TITLE  : GENERAL TITLE                       CH*80
CC               NPAR   : TOTAL NUMBER OF PARAMETERS          I*4
CC               SIGMA0 : MEAN ERROR OF UNIT WEIGHT           R*8
CC               LOCQ(K,I),K=1,..,MAXLCQ,I=1,.,NPAR: CHARAC-  I*4
CC                        TERIZATION OF PARAMETERS
CC               XXX(I),I=1,PNAR: ESTIMATED PARAMETERS        R*8
CC               Q(I),I=1,1,..,NPAR*(NPAR+1)/2: UPPER TRIAN-  R*8
CC                        GULAR PART OF Q-MATRIX
CC               NSTCEP : NUMBER OF STOCH PARAMETERS/EPOCH    I*4
CC               SCASTC : SCALING FACTOR FOR                  R*8(*)
CC                        (1): STOCHASTIC PULSES
CC                        (2): STOCHASTIC ACCELERATIONS
CC               INTSTC : INTERVAL NUMBERS FOR STOCH. PARMS.  I*4(*,*,*)
CC               TIMSTC : CORRESPONDING EPOCHS                R*8(*,*,*,*)
CC               FILSTD : STANDARD ORBIT FILE                 CH*(*)
CC                        BLANK: GET NAMES VIA GTFLNA
CC               FILRPR : RADIATION PRESSURE FILE             CH*(*)
CC               FILORB : ELE FILE                            CH*(*)
CC               IARCOF : NUMBER OF ARCS ALREADY IN LOCQ, BUT I*4
CC                        NOT TO BE UPDATED
CC               NMXINT : MAX. NUMBER OF INTEGR. INTERVAL     I*4
CC               NMXSAP : NUMBER OF SATELLITES WITH STOCH.    I*4
CC               NMXARC : NUMBER OF ARCS                      I*4
CC
CC REMARKS    :  NOT MORE THAN MAXSAT SATELLITES
CC               NOT MORE THAN MAXARC ARCS
CC               INTERNAL SUBROUTINES ARE DEFINED AND USED IN THIS SR
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/03 11:36
CC
CC CHANGES    :  21-MAR-91 : ??: SR OPNFIL TO OPEN RPR FILE
CC               12-DEC-91 : ??: CORRECT SAVING , IF ORBITAL ELEMENTS
CC                               HAVE BEEN ELIMINATED FROM NORMAL EQN.
CC               29-MAY-92 : ??: CHANGE FORMAT FOR ANGLES TO F16.9
CC               30-MAY-92 : ??: ERROR IN STORED "U0" IF NOT ESTIMATED
CC                               "ELEOLD(7)" --> "TPERIO" .
CC               03-JUN-92 : ??: ONE DIGIT MORE IN ECCENTRICITY FORMAT
CC               08-JAN-93 : ??: USE OF SR "OPNFIL" TO OPEN FILES
CC               20-MAR-93 : ??: STOCHASTIC ORBIT PARAMETERS
CC               28-JUL-93 : EB: OPEN STD-FILE, RPR-FILE AND ELE-FILE
CC                               WITH PARAMETERS FILSTD,FILRPR,FILORB
CC                               POSSIBLE
CC                2-AUG-93 : EB: SCALING OF P0/P2 FROM 1.D5 TO 1.D9
CC               10-AUG-93 : EB: ADDITIONAL PARAMETER IARCOF
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               10-AUG-94 : MR: CALL EXITRC
CC               25-JAN-96 : GB: NEW ORBIT MODEL IMPLEMENTED
CC               07-SEP-97 : LM: CORRECT CALL PRTDER
CC               25-SEP-97 : DI: USE MAXSAT.inc
CC               25-AUG-99 : JJ: CHANGED CHDUMM FROM '' TO ' '
CC               23-JUN-02 : DS: ADD CHECKS TO AVOID LEO ORBITAL ELEMENTS
CC               22-JAN-03 : HU: WRITE TOSC WITH HIGHER PRECISION
CC                               USE M_BERN, D_CONST, FILENAMELENGTH
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               06-AUG-03 : HU: NEW STD FORMAT
CC               12-DEC-03 : AJ: DIFF. TYPES OF STOCH. PARAMETERS
CC               14-JAN-04 : HU: USE LFNOR1 INSTEAD OF LFNLOC, CONFLICT WITH
CC                               CALL OF GTSATM IN PRTDER
CC               26-JAN-04 : HU: CLOSE LFNOR1 IF OPEN
CC               31-JAN-05 : HU: WRITE FORMAT, DO NOT OVERWRITE NUT MODEL
CC               13-APR-05 : AJ: WRITE STOCHASTIC A PRIORI PARAMETERS
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-FEB-07 : AG: USE 206264... FROM DEFCON
CC               29-MAY-08 : DT: ALLOW SLR ORBITS (ASSUMING LOCQ(3)>=951)
CC               04-AUG-08 : DT: SYSTEM FOR DYNAMIC ORBIT PARAMETERS
CC               02-SEP-08 : DT: INTERVAL LENGTHS IN SECONDS INSTEAD OF HOURS
CC               28-OCT-08 : DT: USE MAXVAR FROM M_MAXDIM
CC               03-DEC-10 : HB: ADD PARAMETERS FOR DECLARATIONS IN SR PRTDER
CC               29-APR-11 : SL: M_BERN WITH ONLY, STOCH.PULSES FOR GNSS
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
C*
      USE m_bern,   ONLY: i4b, r8b, fileNameLength,
     1                    lfnErr, lfnOr1, lfnOrb, lfnRpr
      USE m_maxdim, ONLY: MAXSAT, MAXINT, maxVar
      USE D_CONST,  ONLY: GM,PI,ARS
      USE f_ikf
      USE s_opnfil
      USE s_prtder
      USE s_opnerr
      USE s_maxtst
      USE s_exitrc
      USE s_rdnutsub
      USE s_gtflna
      USE s_alcerr
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IAOLD , IARC  , IARCOF, ICRARC, IELE  , IFMT  ,
     1          IFMTEL, IFRC  , INDFRC, INDSAT, INT   ,
     2          IORSYS, IOSTAT, IPAR  , IQ    , IRC   , IRC1  ,
     3          IRC2  , IRCORB, IRCRPR, ISAT  , ISTC  , ISTOC , ISVN  ,
     4          ISVOLD, K     , MAXARC, MXCARC, MXCLCQ,
     5          MXCSAT, MXCSTC, MXCVAR, NARC  , NINT  , NLIN  , NORB  ,
     6          NPAR  , NRAD  , NSTCEP, NVAR  , NPRSTC, KSTC  , NSTCA ,
     7          KFRC  , MODSTC_A      , ISTART, ILAST , NFRC  , IPRSTC,
     8          ISVN_OLD      , INTSTC_OLD    , MODSTC, IARC_A, ISVN_A,
     9          ISTC_A, IFRC_A, INDFRC_A      , INDSAT_A      , NSTC,
     1          NMXINT, NMXSAP, NMXARC, mSat
C
      REAL*8    DUMMY , E0    , HELP  , SIGMA0, TDUMMY, TPERIO, V0    ,
     1          XM0   , ZERO
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MAXARC=20)
C
      CHARACTER(LEN=6), PARAMETER :: srName= 'STDSAV'
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMARC/MXCARC,MXNARC
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
      COMMON/MCMSTC/MXCSTC,MXNSTC
      COMMON/MCMVAR/MXCVAR,MXNVAR
C
      TYPE t_frcout
        INTEGER(i4b) :: arc
        INTEGER(i4b) :: svn
        INTEGER(i4b) :: frctyp
        REAL(r8b)    :: apriori
        REAL(r8b)    :: result
        REAL(r8b)    :: rms
      END TYPE t_frcout
C
      TYPE t_stcout
        INTEGER(i4b) :: nstcep
        INTEGER(i4b) :: intstc
        REAL(r8b)    :: timstc
      END TYPE t_stcout
C
      REAL*8 Q(*),XXX(*)
      REAL*8 ELE(7,MAXSAT,MAXARC),TOSC(MAXARC)
      REAL*8 ELEOLD(MAXVAR,MAXSAT,MAXARC)
      REAL*8 ELENEW(MAXVAR,MAXSAT,MAXARC)
      REAL*8 RMS(MAXVAR,MAXSAT,MAXARC)
      REAL*8 TIMSTC(3,MXCSTC,MXCSAT,*),SCASTC(*)
      REAL*8 DRDELE(3),ELESAT(7),RPRPAR(MAXVAR)
      REAL*8 TIMSTCA(NMXINT,NMXSAP,NMXARC)
      REAL*8 PARSTCA(3,NMXINT,NMXSAP,NMXARC)
      REAL*8 hlp_r(2)
C
      INTEGER*4 SVN(MAXSAT,MAXARC),NSAT(MAXARC)
      INTEGER*4 LOCQ(MXCLCQ,*)
      INTEGER*4 INTSTC(MXCSTC,MXCSAT,*)
      INTEGER*4 INTSTCA(NMXINT,NMXSAP,NMXARC)
      INTEGER*4 NSTCEPA(NMXINT),FRCTYPA(3,NMXINT)
      INTEGER(i4b),DIMENSION(:,:),ALLOCATABLE :: lcqApr
      INTEGER*4 hlp_i(2)
C
      CHARACTER*1  FLG(MAXVAR,MAXSAT,MAXARC)
      CHARACTER*6  MXNSAT,MXNARC,MXNLCQ,MXNSTC,MXNVAR
      CHARACTER*8  ANLTYP
      CHARACTER*21 COMENT(MAXVAR+2)
      CHARACTER*(*) FILRPR,FILORB,FILSTD
      CHARACTER*(fileNameLength) CHDUMM
      CHARACTER*80 TITLE
      CHARACTER*80 LINE
      CHARACTER*16 NUTNAM,SUBNAM
      CHARACTER*4  empiri
C
      LOGICAL OPENED
C
      TYPE(t_stcout), DIMENSION(:), ALLOCATABLE   :: stcout
      TYPE(t_frcout), DIMENSION(:,:), ALLOCATABLE :: frcout
C
cc      COMMON/CSTDSV/ELE,RPR,SVN
C
C CHECK MAXIMUM LOCAL DIMENSIONS
C ------------------------------
      CALL MAXTST(1,srName,MXNSAT,MAXSAT,MXCSAT,IRC1)
      CALL MAXTST(1,srName,MXNARC,MAXARC,MXCARC,IRC2)
      IF(IRC1.NE.0.OR.IRC2.NE.0) CALL EXITRC(2)
C
C SET MXCVAR
C ----------
      MXNVAR='MAXVAR'
      MXCVAR=MAXVAR
C
C GET ORBIT FILENAMES : FILSTD=' ' TAKE NAMES FROM N-FILE (SR GTFLNA)
C -------------------------------------------------------------------
      IF (FILSTD.EQ.' ') THEN
C
C GET ORBIT OUTPUT FILENAME (NO SAVE, IF FILENAME BLANK)
        CALL GTFLNA(0,'ORBITRS',FILORB,IRCORB)
        IF(IRCORB.NE.0) RETURN
C
C CHECK WHETHER RADIATION PRESSURE FILE EXISTS
        CALL GTFLNA(0,'RPRCOE ',FILRPR,IRC)
        IF(IRC.NE.0) THEN
          WRITE(LFNERR,901)
901       FORMAT(/,' *** SR STDSAV: NO RADIATION PRESSURE FILE FOUND',/,
     1                         16X,'ORBITAL ELEMENTS NOT SAVED !',/)
          RETURN
        ENDIF
C
C GET STANDARD ORBIT FILE NAME
        CALL GTFLNA(1,'STDORB ',FILSTD,IRC)
      ENDIF
C
C CHECK IF ORBITAL PARAMETERS HAVE BEEN ESTIMATED
C -----------------------------------------------
      DO 5 IPAR=1,NPAR
C
C Skip LEOs but allow SLR satellites
CCC        IF(LOCQ(1,IPAR).EQ.3 .AND. LOCQ(3,IPAR).LT.900) GOTO 15
        IF(LOCQ(1,IPAR).NE.3) CYCLE
        IF(LOCQ(3,IPAR).LT.900 .OR. LOCQ(3,IPAR).GE.951) GOTO 15
5     CONTINUE
      RETURN
C
C GET NAMES OF NUTATION AND SUBDAILY MODELS
15    CALL RDNUTSUB(NUTNAM,SUBNAM)
C
C OPEN OUTPUT ELE FILE
      INQUIRE(UNIT=LFNOR1,OPENED=OPENED)
      IF (OPENED) CLOSE (LFNOR1)
      CALL OPNFIL(LFNOR1,FILORB,'UNKNOWN','FORMATTED',
     1            ' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNOR1,IOSTAT,FILORB,srName)
C
C WRITE TITLE LINES
      WRITE(LFNOR1,101) TITLE
101   FORMAT(A80)
C
C READ STANDARD ORBIT FILE
C ========================
C
C OPEN STANDARD ORBIT FILE
      CLOSE(UNIT=LFNORB)
      CALL OPNFIL(LFNORB,FILSTD,'OLD','UNFORMATTED',
     1            'READONLY',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNORB,IOSTAT,FILSTD,'GETORB')
C
C NUMBER OF ARCS
      READ(LFNORB) NARC
C
C READ FORMAT NUMBER AND ORBIT DESCRIPTION
      IFMTEL=1
      IF (NARC.LT.0) THEN
        WRITE(LFNOR1,"(80('-'))")
        READ(LFNORB) IFMT,NARC
        READ(LFNORB) NLIN
        WRITE(LFNOR1,"('FORMAT: ',2I6)") IFMTEL,NLIN
        DO I=1,NLIN
          READ(LFNORB) LINE
cc          IF (LINE(1:7).EQ.'NUTSUB:') THEN
cc            WRITE(LINE,"('NUTSUB: ',A16,1X,A16)") NUTNAM,SUBNAM
cc          ENDIF
C
C System for dynamical orbit parameters
          IF(LINE(1:6).EQ.'EMPIRI')THEN
            empiri=LINE(9:12)
C
C Change to new format: Interval lengths in [s] instead of [h]
          ELSEIF(LINE(1:6).EQ.'INTEGR') THEN
            READ(LINE(9:17),"(F9.5)") hlp_r(1)
            READ(LINE(23:31),"(F9.5)") hlp_r(2)
            DO K=1,2
              hlp_i(K)=DNINT(hlp_r(K)*3600)
            ENDDO
            WRITE(LINE(1:6),"(A6)") 'INTEG2'
            WRITE(LINE(9:17),"(I9)") hlp_i(1)
            WRITE(LINE(23:31),"(I9)") hlp_i(2)
          ENDIF
C
          WRITE(LFNOR1,"(A80)") LINE
C
        ENDDO
        WRITE(LFNOR1,"(80('-'))")
      ENDIF
C
C LOOP OVER ALL ARCS
C ------------------
      mSat=0
      DO 50 IARC=1,NARC
C
C READ OSCULATION EPOCH AND INTERVAL BOUNDARIES
        READ(LFNORB) NSAT(IARC),NINT,IQ,(SVN(I,IARC),I=1,NSAT(IARC))
        IF(nSat(iArc).GE.mSat) mSat=nSat(iArc)
        READ(LFNORB) TOSC(IARC),ZERO
        IF(ZERO.EQ.0.D0.OR.ZERO.EQ.2.D0) THEN
          DO 10 I=1,NINT+1
            READ(LFNORB) TDUMMY
10        CONTINUE
        ENDIF
C
C READ OSCULATING ELEMENTS, SKIP POLYNOMIAL COEFFICIENTS
        DO 20 ISAT=1,NSAT(IARC)
          READ(LFNORB) (ELEOLD(K,ISAT,IARC),K=1,7)
20      CONTINUE
        DO 40 INT=1,NINT
          READ(LFNORB) DUMMY
          DO 30 K=1,IQ+1
            READ(LFNORB) DUMMY
30        CONTINUE
40      CONTINUE
C
C RECOMPUTE THE SIXTH ELEMENT
        DO ISAT=1,NSAT(IARC)
          IF(ELEOLD(6,ISAT,IARC).NE.0.D0)CYCLE
          TPERIO=ELEOLD(7,ISAT,IARC)
          XM0=DSQRT(GM/ELEOLD(1,ISAT,IARC)**3)*(-TPERIO)
          E0=XM0
          DO 138 I=1,10
            E0=XM0+ELEOLD(2,ISAT,IARC)*DSIN(E0)
138       CONTINUE
          HELP=DSQRT((1+ELEOLD(2,ISAT,IARC))/(1-ELEOLD(2,ISAT,IARC)))
          V0=2*DATAN(HELP*DTAN(E0/2))
          ELEOLD(6,ISAT,IARC)=V0+ELEOLD(5,ISAT,IARC)
        ENDDO
50    CONTINUE
C
C ALLOCATE STRUCTURES
C -------------------
      ALLOCATE(lcqApr(mxcLcq,nmxInt*mSat),stat=irc)
      CALL ALCERR(irc,'lcqApr',(/mxcLcq,nmxInt*mSat/),srName)
      ALLOCATE(stcOut(nmxInt*mSat),stat=irc)
      CALL ALCERR(irc,'stcOut',(/nmxInt*mSat/),srName)
      ALLOCATE(frcOut(3,nmxInt*mSat),stat=irc)
      CALL ALCERR(irc,'frcOut',(/3,nmxInt*mSat/),srName)
C
C LOOP OVER ALL ARCS AND ALL SATELLITES
C -------------------------------------
      CHDUMM=' '
      NPRSTC=0
      DO 100 IARC=1,NARC
        DO 70 ISAT=1,NSAT(IARC)
          CALL PRTDER(CHDUMM,SVN(ISAT,IARC),1,0,1,TOSC(IARC),NMXINT,
     1                ICRARC,IORSYS,NVAR,NRAD,DRDELE,ELESAT,RPRPAR,
     2                ANLTYP,IRCRPR,NSTCA,FRCTYPA,NSTCEPA,
     3                INTSTCA(1,ISAT,IARC),TIMSTCA(1,ISAT,IARC),
     4                PARSTCA(1,1,ISAT,IARC))
C
C GET A PRIORI RPR-PARAMETERS
          DO 60 K=1,NRAD
            ELEOLD(6+K,ISAT,IARC)=RPRPAR(K)
60        CONTINUE
C
C GET LOCQ OF A PRIORI STOCHASTIC PARAMETERS
          DO 67 KSTC=1,NSTCA
            DO 65 KFRC=1,NSTCEPA(KSTC)
              NPRSTC=NPRSTC+1
C
              LCQAPR(1,NPRSTC) = 11
              LCQAPR(2,NPRSTC) = IARC
              LCQAPR(3,NPRSTC) = SVN(ISAT,IARC)
              LCQAPR(4,NPRSTC) = KSTC
              LCQAPR(5,NPRSTC) = FRCTYPA(KFRC,KSTC)
              LCQAPR(6,NPRSTC) = KFRC
              LCQAPR(7,NPRSTC) = ISAT
65          CONTINUE
67        CONTINUE
70      CONTINUE
100   CONTINUE
C
C GET TYPE OF A PRIORI STOCHASTIC PARAMETERS
      MODSTC_A = 0
      IF(NPRSTC.GT.0) THEN
        IF (LCQAPR(5,NPRSTC).LT.10) THEN
          MODSTC_A = 0
        ELSEIF(LCQAPR(5,NPRSTC).LT.20) THEN
          MODSTC_A = 1
        ELSEIF(LCQAPR(5,NPRSTC).LT.30) THEN
          MODSTC_A = 2
        END IF
      END IF
C
C UPDATE ORBITAL ELEMENTS
C =======================
C
C COMMENTS
C --------
      COMENT(1)='ARC-NUMBER          ='
      COMENT(2)='SATELLITE           ='
      COMENT(3)='A                   ='
      COMENT(4)='E                   ='
      COMENT(5)='I                   ='
      COMENT(6)='NODE                ='
      COMENT(7)='PERIGEE             ='
      COMENT(8)='ARG. OF LAT (START) ='
      IF(empiri.EQ.'DYX ')THEN
        COMENT(9)='D0                  ='
        COMENT(10)='Y0                  ='
        COMENT(11)='X0                  ='
        COMENT(12)='DC                  ='
        COMENT(13)='YC                  ='
        COMENT(14)='XC                  ='
        COMENT(15)='DS                  ='
        COMENT(16)='YS                  ='
        COMENT(17)='XS                  ='
      ELSEIF(empiri.EQ.'RSW ')THEN
        COMENT(9)='R0                  ='
        COMENT(10)='S0                  ='
        COMENT(11)='W0                  ='
        COMENT(12)='RC                  ='
        COMENT(13)='SC                  ='
        COMENT(14)='WC                  ='
        COMENT(15)='RS                  ='
        COMENT(16)='SS                  ='
        COMENT(17)='WS                  ='
      ELSEIF(empiri.EQ.'DRSW')THEN
        COMENT(9)='D0                  ='
        COMENT(10)='S0                  ='
        COMENT(11)='W0                  ='
        COMENT(12)='RC                  ='
        COMENT(13)='SC                  ='
        COMENT(14)='WC                  ='
        COMENT(15)='RS                  ='
        COMENT(16)='SS                  ='
        COMENT(17)='WS                  ='
      ENDIF
C
C INIT OUTPUT VARIABLES
C ---------------------
      ELENEW = ELEOLD
      RMS    = 0d0
      FLG    = '*'
C
C LOOP OVER ALL PARAMETERS
C ------------------------
      DO IPAR=1,nPar
C
C SKIP NON-ORBITAL PARAMETERS
C Skip LEOs but allow SLR satellites (>=951)
CCC        IF(LOCQ(1,IPAR).NE.3 .OR. LOCQ(3,IPAR).GE.900) CYCLE
        IF(LOCQ(1,IPAR).NE.3) CYCLE
        IF(LOCQ(3,IPAR).GE.900 .AND. LOCQ(3,IPAR).LT.951) CYCLE
C
C PARAMETER INFO
        NORB=LOCQ(5,IPAR)
        IARC=LOCQ(2,IPAR)-IARCOF
        ISVN=LOCQ(3,IPAR)
        IELE=LOCQ(4,IPAR)
C
C FIND SATELLITE
        DO 110 ISAT=1,NSAT(IARC)
          IF(ISVN.EQ.SVN(ISAT,IARC)) GOTO 120
110     CONTINUE
120     CONTINUE
C
C INITIALIZE NEW ELEMENTS
        FLG(IELE,ISAT,IARC) = ' '
C
C UPDATE KEPLERIAN ELEMENTS
        IF(IELE.LE.6) THEN
          IF(IELE.GT.1.AND.IELE.LE.6) THEN
            ELENEW(IELE,ISAT,IARC)=ELEOLD(IELE,ISAT,IARC)+
     1             XXX(IPAR)/ARS
            RMS(IELE,ISAT,IARC)=SIGMA0*DSQRT(Q(IKF(IPAR,IPAR)))/ARS
          ELSE
            ELENEW(IELE,ISAT,IARC)=ELEOLD(IELE,ISAT,IARC)+XXX(IPAR)
            RMS(IELE,ISAT,IARC)=SIGMA0*DSQRT(Q(IKF(IPAR,IPAR)))
          END IF
C
C UPDATE RADIATION PRESSSURE COEFF.
        ELSE
          ELENEW(IELE,ISAT,IARC)=ELEOLD(IELE,ISAT,IARC)+XXX(IPAR)*1.D-9
          RMS(IELE,ISAT,IARC)=SIGMA0*DSQRT(Q(IKF(IPAR,IPAR)))*1.D-9
        END IF
      ENDDO
C
C WRITE OLD AND NEW ELEMENTS ON OUTPUT-FILE
C -----------------------------------------
      DO IARC=1,NARC
        DO ISAT=1,NSAT(IARC)
          WRITE(LFNOR1,151)
     1           COMENT(1),IARC,COMENT(2),SVN(ISAT,IARC),TOSC(IARC)
151       FORMAT(A21,I3,1X,A21,I3,' TOSC=',F20.12,/,75('-'))
C
          DO IELE=1,6+NRAD
C
C TRANSFORM ANGULAR ELEMENTS INTO DEGREES
            IF(IELE.GE.3.AND.IELE.LE.6) THEN
              ELENEW(IELE,ISAT,IARC)=ELENEW(IELE,ISAT,IARC)*180/PI
              ELEOLD(IELE,ISAT,IARC)=ELEOLD(IELE,ISAT,IARC)*180/PI
              IF (RMS(IELE,ISAT,IARC).NE.0)
     1          RMS(IELE,ISAT,IARC)=RMS(IELE,ISAT,IARC)*180/PI
            ENDIF
            IF(IELE.EQ.1) THEN
              WRITE(LFNOR1,161) COMENT(2+IELE),
     1                          ELEOLD(IELE,ISAT,IARC),
     2                          ELENEW(IELE,ISAT,IARC),
     3                          RMS(IELE,ISAT,IARC),
     4                          FLG(IELE,ISAT,IARC),IORSYS
161           FORMAT(A21,2F16.5,' +-',F12.3,2X,A1,' ORBSYS',I2)
            ELSE IF(IELE.EQ.2) THEN
              WRITE(LFNOR1,162) COMENT(2+IELE),
     1                          ELEOLD(IELE,ISAT,IARC),
     2                          ELENEW(IELE,ISAT,IARC),
     3                          RMS(IELE,ISAT,IARC),
     4                          FLG(IELE,ISAT,IARC),IORSYS
162           FORMAT(A21,2F16.10,' +-',F12.9,2X,A1,' ORBSYS',I2)
            ELSE IF(IELE.LE.6) THEN
              WRITE(LFNOR1,163) COMENT(2+IELE),
     1                          ELEOLD(IELE,ISAT,IARC),
     2                          ELENEW(IELE,ISAT,IARC),
     3                          RMS(IELE,ISAT,IARC),
     4                          FLG(IELE,ISAT,IARC),IORSYS
163           FORMAT(A21,2F16.9,' +-',F12.9,2X,A1,' ORBSYS',I2)
            ELSE
              WRITE(LFNOR1,165) COMENT(2+IELE),
     1                          ELEOLD(IELE,ISAT,IARC),
     2                          ELENEW(IELE,ISAT,IARC),
     3                          RMS(IELE,ISAT,IARC),
     4                          FLG(IELE,ISAT,IARC),ANLTYP
165           FORMAT(A21,2D16.9,' +-',D12.5,2X,A1,1X,A8)
            ENDIF
          ENDDO
        ENDDO
      ENDDO

C LOOP OVER ALL ESTIMATED STOCHASTIC ORBIT PARAMETERS
C ---------------------------------------------------
C
C INITIALIZE COUNTERS
      ISTART = 0
      ILAST  = 0
C
      NFRC = 0
      NSTC = 0
      ISVN_OLD   = -1
      INTSTC_OLD = -1
C
      DO 400 IPAR=1,NPAR
C Skip LEOs but allow SLR satellites (>=951)
CCC        IF (LOCQ(1,IPAR).NE.11 .OR. LOCQ(3,IPAR).GE.900) GOTO 400
        IF (LOCQ(1,IPAR).NE.11) GOTO 400
        IF (LOCQ(3,IPAR).GE.900 .AND. LOCQ(3,IPAR).LT.951) GOTO 400
C
        IARC  =LOCQ(2,IPAR)-IARCOF
        ISVN  =LOCQ(3,IPAR)
        ISTC  =LOCQ(4,IPAR)
        IFRC  =LOCQ(5,IPAR)
        INDFRC=LOCQ(6,IPAR)
        INDSAT=LOCQ(7,IPAR)
C
C GET TYPE OF ESTIMATED STOCHASTIC PARAMETERS
        IF (IFRC.LT.10) THEN
          MODSTC = 0
        ELSEIF(IFRC.LT.20) THEN
          MODSTC = 1
        ELSEIF(IFRC.LT.30) THEN
          MODSTC = 2
        END IF
C
C DIFFERENT TYPES OF STOCH. PARAMETERS NOT ALLOWED
        IF(MODSTC_A.NE.MODSTC.AND.NPRSTC.GT.0) THEN
          WRITE(LFNERR,401) MODSTC_A,MODSTC
401       FORMAT(/,' *** SR STDSAV: MIXING OF STOCH.',
     1             ' PARAM. NOT ALLOWED',
     2           /,17X,'A PRIORI  TYPE:',I1
     3           /,17X,'ESTIMATED TYPE:',I1/)
          CALL EXITRC(2)
        END IF
C
C LOOP OVER ALL (REMAINING) A PRIORI STOCHASTIC PARAMETERS
C --------------------------------------------------------
        IF(ISTART.LT.NPRSTC) THEN
          DO 500 IPRSTC=1+ISTART,NPRSTC
            IARC_A  =LCQAPR(2,IPRSTC)
            ISVN_A  =LCQAPR(3,IPRSTC)
            ISTC_A  =LCQAPR(4,IPRSTC)
            IFRC_A  =LCQAPR(5,IPRSTC)
            INDFRC_A=LCQAPR(6,IPRSTC)
            INDSAT_A=LCQAPR(7,IPRSTC)
C
C CHECK ARC: A PRIORI IS FIRST
            IF (IARC_A.LT.IARC) THEN
              CALL SAVAPR
              IF (ILAST.EQ.NPRSTC) CALL SAVEST
C
C CHECK ARC: SAME ARC
            ELSEIF (IARC_A.EQ.IARC) THEN
C
C CHECK SATELLITE: A PRIORI IS FIRST
              IF (ISVN_A.LT.ISVN) THEN
                CALL SAVAPR
                IF (ILAST.EQ.NPRSTC) CALL SAVEST
C
C CHECK SATELLITE: SAME SATELLITE
              ELSEIF (ISVN_A.EQ.ISVN) THEN
C
C CHECK EPOCH: A PRIORI IS FIRST
                IF (INTSTCA(ISTC_A,INDSAT_A,IARC_A).LT.
     1              INTSTC(ISTC,INDSAT,IARC+IARCOF)) THEN
                  CALL SAVAPR
                  IF (ILAST.EQ.NPRSTC) CALL SAVEST
C
C CHECK EPOCH: SAME EPOCH
                ELSEIF (INTSTCA(ISTC_A,INDSAT_A,IARC_A).EQ.
     1                  INTSTC(ISTC,INDSAT,IARC+IARCOF)) THEN
C
C CHECK FORCETYPE: A PRIORI IS FIRST
                  IF (IFRC_A.LT.IFRC) THEN
                    CALL SAVAPR
                    IF (ILAST.EQ.NPRSTC) CALL SAVEST
C
C CHECK FORCETYPE: SAME FORCETYPE
                  ELSEIF (IFRC_A.EQ.IFRC) THEN
                    CALL SAVAPR
C
C ADD ESTIMATED TO SAVED A PRIORI VALUES
                    IF(IFRC.LT.10) THEN
                      FRCOUT(NFRC,NSTC)%RESULT=FRCOUT(NFRC,NSTC)%RESULT+
     1                                         XXX(IPAR)/SCASTC(1)
                      FRCOUT(NFRC,NSTC)%RMS=SIGMA0*DSQRT(
     1                Q(IKF(IPAR,IPAR))/SCASTC(1))
                    ELSE
                      FRCOUT(NFRC,NSTC)%RESULT=FRCOUT(NFRC,NSTC)%RESULT+
     1                                         XXX(IPAR)/SCASTC(2)
                      FRCOUT(NFRC,NSTC)%RMS=SIGMA0*DSQRT(
     1                Q(IKF(IPAR,IPAR))/SCASTC(2))
                    END IF
                    GOTO 501
C
C CHECK FORCETYPE: ESTIMATED IS FIRST
                  ELSEIF (IFRC_A.GT.IFRC) THEN
                    CALL SAVEST
                    GOTO 501
                  END IF
C
C CHECK EPOCH: ESTIMATED IS FIRST
                ELSEIF (INTSTCA(ISTC_A,INDSAT_A,IARC_A).GT.
     1                  INTSTC(ISTC,INDSAT,IARC+IARCOF)) THEN
                  CALL SAVEST
                  GOTO 501
                END IF
C
C CHECK SATELLITE: ESTIMATED IS FIRST
              ELSEIF (ISVN_A.GT.ISVN) THEN
                CALL SAVEST
                GOTO 501
              END IF
C
C CHECK ARC: ESTIMATED IS FIRST
            ELSEIF (IARC_A.GT.IARC) THEN
              CALL SAVEST
              GOTO 501
            END IF
500       CONTINUE
501       CONTINUE
C
C SAVE NUMBER OF LATEST FOUND A PRIORI VALUE
          ISTART=ILAST
C
        ELSE
C
C SAVE REMAINING ESTIMATED STOCHASTIC PARAMETERS
          CALL SAVEST
        END IF
400   CONTINUE
C
C SAVE REMAINING A PRIORI STOCHASTIC PARAMETERS
      IF(ISTART.LT.NPRSTC) THEN
        DO 600 IPRSTC=1+ISTART,NPRSTC
          IARC_A  =LCQAPR(2,IPRSTC)
          ISVN_A  =LCQAPR(3,IPRSTC)
          ISTC_A  =LCQAPR(4,IPRSTC)
          IFRC_A  =LCQAPR(5,IPRSTC)
          INDFRC_A=LCQAPR(6,IPRSTC)
          INDSAT_A=LCQAPR(7,IPRSTC)
C
          CALL SAVAPR
600     CONTINUE
      END IF
C
C WRITE ALL COLLECTED STOCHASTIC ORBIT PARAMETERS
C -----------------------------------------------
      ISTOC=1
      IAOLD=-1
      ISVOLD=-1
      DO 700 KSTC=1,NSTC
        DO 800 KFRC=1,STCOUT(KSTC)%NSTCEP
C
C WRITE ONLY GNSS ORBITAL PARAMETERS (use STDSAV2 for LEOs)
          IF (FRCOUT(KFRC,KSTC)%SVN.GE.900 .AND.
     1        FRCOUT(KFRC,KSTC)%SVN.LT.951) GOTO 800
C
C FOR FIRST STOCHASTIC PARAMETER, WRITE GENERAL TITLE
          IF (ISTOC.EQ.1) THEN
            ISTOC=0
            WRITE(LFNOR1,810)
810         FORMAT('*** STOCHASTIC ORBIT PARAMETERS ***',/,80('-'))
          END IF
C
C IF A NEW SATELLITE OR ARC WAS ENCOUNTERED, WRITE TITLE LINE
          IF (FRCOUT(KFRC,KSTC)%SVN.NE.ISVOLD.OR.
     1        FRCOUT(KFRC,KSTC)%ARC.NE.IAOLD) THEN
            IAOLD =FRCOUT(KFRC,KSTC)%ARC
            ISVOLD=FRCOUT(KFRC,KSTC)%SVN
            WRITE(LFNOR1,151) COMENT(1),FRCOUT(KFRC,KSTC)%ARC,COMENT(2),
     1                        FRCOUT(KFRC,KSTC)%SVN,TOSC(IAOLD)
          END IF
C
C WRITE RESULT LINE
          WRITE(LFNOR1,820) FRCOUT(KFRC,KSTC)%FRCTYP,
     1                      STCOUT(KSTC)%NSTCEP,STCOUT(KSTC)%INTSTC,
     2                      STCOUT(KSTC)%TIMSTC,
     3                      FRCOUT(KFRC,KSTC)%APRIORI,
     4                      FRCOUT(KFRC,KSTC)%RESULT,
     5                      FRCOUT(KFRC,KSTC)%RMS
820       FORMAT(2I2,I4,F12.5,1X,2D16.9,D15.5)
800     CONTINUE
700   CONTINUE
C
C END OF UPDATE
C -------------
      CLOSE(UNIT=LFNOR1)
      CLOSE(UNIT=LFNORB)
      CLOSE(UNIT=LFNRPR)
C
C DEALLOCATE STRUCTURES
C ---------------------
      IF(ALLOCATED(lcqApr)) DEALLOCATE(lcqApr,stat=irc)
      IF(ALLOCATED(stcOut)) DEALLOCATE(stcOut,stat=irc)
      IF(ALLOCATED(frcOut)) DEALLOCATE(frcOut,stat=irc)
C
      RETURN
C
C DEFINITION OF ADDITIONAL SUBROUTINES
C ====================================
      CONTAINS
C
C SAVE A PRIORI STOCHASTIC PARAMETER
C ----------------------------------
      SUBROUTINE SAVAPR
C
C NEW STOCH. EPOCH OR NEW SATELLITE ENCOUNTERED?
        IF (INTSTCA(ISTC_A,INDSAT_A,IARC_A).NE.INTSTC_OLD .OR.
     1      ISVN_A.NE.ISVN_OLD) THEN
          INTSTC_OLD = INTSTCA(ISTC_A,INDSAT_A,IARC_A)
          ISVN_OLD = ISVN_A
          NSTC = NSTC + 1
          NFRC = 0
        END IF
        NFRC = NFRC + 1
C
C MORE THAN 3 STOCH. PARAM. PER EPOCH
        IF(NFRC.GT.3) CALL MORET3
C
C FILL STRUCTURES FOR ELE-FILE
        STCOUT(NSTC)%NSTCEP = NFRC
        STCOUT(NSTC)%INTSTC = INTSTCA(ISTC_A,INDSAT_A,IARC_A)
        STCOUT(NSTC)%TIMSTC = TIMSTCA(ISTC_A,INDSAT_A,IARC_A)
C
        FRCOUT(NFRC,NSTC)%ARC     = IARC_A
        FRCOUT(NFRC,NSTC)%SVN     = ISVN_A
        FRCOUT(NFRC,NSTC)%FRCTYP  = IFRC_A
        FRCOUT(NFRC,NSTC)%APRIORI =
     1         PARSTCA(INDFRC_A,ISTC_A,INDSAT_A,IARC_A)
        FRCOUT(NFRC,NSTC)%RESULT  =
     1         PARSTCA(INDFRC_A,ISTC_A,INDSAT_A,IARC_A)
        FRCOUT(NFRC,NSTC)%RMS     = 0.D0
C
        ILAST=IPRSTC
C
      END SUBROUTINE SAVAPR
C
C SAVE ESTIMATED STOCHASTIC PARAMETER
C -----------------------------------
      SUBROUTINE SAVEST
C
C NEW STOCH. EPOCH OR NEW SATELLITE ENCOUNTERED?
        IF (INTSTC(ISTC,INDSAT,IARC+IARCOF).NE.INTSTC_OLD .OR.
     1      ISVN.NE.ISVN_OLD) THEN
          INTSTC_OLD = INTSTC(ISTC,INDSAT,IARC+IARCOF)
          ISVN_OLD = ISVN
          NSTC = NSTC + 1
          NFRC = 0
        END IF
        NFRC = NFRC + 1
C
C MORE THAN 3 STOCH. PARAM. PER EPOCH
        IF(NFRC.GT.3) CALL MORET3
C
C FILL STRUCTURES FOR ELE-FILE
        STCOUT(NSTC)%NSTCEP = NFRC
        STCOUT(NSTC)%INTSTC = INTSTC(ISTC,INDSAT,IARC+IARCOF)
C
        IF(IFRC.LT.20) THEN
          STCOUT(NSTC)%TIMSTC = TIMSTC(1,ISTC,INDSAT,IARC+IARCOF)
        ELSE
          STCOUT(NSTC)%TIMSTC = TIMSTC(2,ISTC,INDSAT,IARC+IARCOF)
        END IF
C
        FRCOUT(NFRC,NSTC)%ARC     = IARC
        FRCOUT(NFRC,NSTC)%SVN     = ISVN
        FRCOUT(NFRC,NSTC)%FRCTYP  = IFRC
        FRCOUT(NFRC,NSTC)%APRIORI = 0.D0
        FRCOUT(NFRC,NSTC)%RESULT  = XXX(IPAR)
        FRCOUT(NFRC,NSTC)%RMS     = SIGMA0*DSQRT(Q(IKF(IPAR,IPAR)))
C
        IF(IFRC.LT.10) THEN
          FRCOUT(NFRC,NSTC)%RESULT = FRCOUT(NFRC,NSTC)%RESULT/SCASTC(1)
          FRCOUT(NFRC,NSTC)%RMS    = FRCOUT(NFRC,NSTC)%RMS/SCASTC(1)
        ELSE
          FRCOUT(NFRC,NSTC)%RESULT = FRCOUT(NFRC,NSTC)%RESULT/SCASTC(2)
          FRCOUT(NFRC,NSTC)%RMS    = FRCOUT(NFRC,NSTC)%RMS/SCASTC(2)
        END IF
C
      END SUBROUTINE SAVEST
C
C TOO MANY STOCH. PARAMETERS
C --------------------------
      SUBROUTINE MORET3
C
        WRITE(LFNERR,402)
402     FORMAT(/,' *** SR STDSAV: TOO MANY STOCH.',
     1           ' PARAM. PER EPOCH',
     2         /,17X,'ALLOWED NUMBER: 3',/)
        CALL EXITRC(2)
C
      END SUBROUTINE MORET3
C
      END SUBROUTINE

      END MODULE
