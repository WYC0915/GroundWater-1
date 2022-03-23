      MODULE s_SEQEPO
      CONTAINS

C*
      SUBROUTINE SEQEPO(OPTELI,NFLSES,FILNUM,FILACT,NSATFL,SVNFIL,
     1                  NFRFIL,ICARR ,OBSFLG,OBSERV,OBSNUM,NSTAT ,
     2                  ICENTR,NKIN  ,STKIN ,NPAR  ,NPARMS,PARLST,
     3                  LOCQ  ,ANOR  ,BNOR  ,PARTYP,NOBSPA,NPAEPO,
     4                  STFIL ,NCLKST,CLKSTA,NCLKSA,CLKSAT,CLKHED,
     5                  STNAME,NDIFF ,MEATYP,ISASYS,CLKSYS)
CC
CC NAME       :  SEQEPO
CC
CC PURPOSE    :  DEFINE THE SEQUENCE OF DIFFERENTIAL IONOSPHERE
CC               PARAMETERS (EPOCH BY EPOCH)
CC
CC PARAMETERS :
CC         IN :  OPTELI(I),I=1,..,MAXTYP: OPTION FOR PRE-     I*4
CC                        ELIMINATION OF PARAMETER TYPES:
CC                        =0 : NOT PRE-ELIMINATED
CC                        =1 : PRE-ELIMINATED BEFORE INVERSION
CC                        =2 : PRE-ELIMINATED AFTER  INVERSION
CC                        =3 : PRE-ELIMINATED EPOCH-WISE
CC               NFLSES : NUMBER OF FILES IN CURRENT SESSION  I*4
CC               FILNUM(I),I=1,..,NFLSES: FILE NUMBERS        I*4
CC               FILACT(I),I=1,..,NFLSES: FILES INVOLVED IN   CH*1
CC                        CURRENT EPOCH
CC               NSATFL(I),I=1,..,NFLSES: NUMBER OF SATEL-    I*4
CC                        LITES PER FILE
CC               SVNFIL(K,I),K=1,..,NSATFL(I),I=1,..,NFLSES:  I*4
CC                        SATELLITE NUMBERS FOR EACH FILE
CC               NFRFIL(I),I=1,..,NFTOT: NUMBER OF FRE-       I*4
CC                        QUENCIES PER FILE
CC               ICARR(K,I),K=1,..,NFRFIL(I),I=1,..,NFTOT:    I*4
CC                        REQUESTED CARRIERS
CC               OBSFLG : OBSERVATION FLAGS                   CH*1
CC                      (I,J,IF): SATELLITE I
CC                                FREQUENCY J
CC                                FILE IF
CC               OBSERV : OBSERVATIONS                        R*8
CC                      (I,J,IF): SATELLITE I
CC                                FREQUENCY J
CC                                FILE IF
CC               OBSNUM(I),I=1,..,NFLSES: OBSERV. NUMBERS FOR I*4
CC                        DIFFERENT FILES IN SESSION
CC               NSTAT  : NUMBER OF STATIONS INVOLVED         I*4
CC               ICENTR(I),I=1,..,NSTAT: CENTER STATION NUMB. I*4
CC               NKIN   : NUMBER OF KIN. STATIONS             I*4
CC               STKIN(I),I=1,2,... STATION NUMBERS TO BE     I*4
CC                        ESTIMATED IN KIN. MODUS
CC               STFIL(L,I),L=1,2 , I=1,2,...,NFTOT: STATION  I*4
CC                        NUMBERS INVOLVED IN OBSFILE I
CC               NCLKST : NUMBER OF EPOCH WISE STATION CLOCKS I*4
CC               CLKSTA(I),I=1,..,MAXSTA: STATION NUMBERS FOR I*4
CC                        CLOCK ESTIMATION
CC               NCLKSA : NUMBER OF EPOCH WISE SATELLITE CLOCKS I*4
CC               CLKSAT(I),I=1,..,MAXSAT: SATELLITE NUMBERS FOR I*4
CC                        CLOCK ESTIMATION
CC               CLKHED : CLOCK HEADER INFORMATION            T_CLKHEAD
CC                          %NUMREF=0: FIX REF-CLOCKS
CC                          %NUMREF=2: SUM FOR REF-CLOCKS
CC               STNAME(I),I=1,..,NSTAT: STATION NAMES        CH*16
CC               NDIFF(I),I=1,..,MAXFIL DIFFERENCE TYPE       I*4
CC               MEATYP(I),I=1,..,NFTOT: MEASUREMENT TYPE     I*4
CC               ISASYS : SATELLITE SYSTEM TO BE CONSIDERED   I*4
CC                        =0: ALL
CC                        =1: GPS
CC                        =2: GLONASS
CC               CLKSYS : =1: ONE REC.CLK FOR EACH SAT.SYS    I*4
CC     IN/OUT :  NPAR   : TOTAL NUMBER OF PARAMETERS          I*4
CC               NPARMS : NUMBER OF PARAMETERS TO COMPUTE RMS I*4
CC               PARLST(I,K), I=1,..,5,K=1,..,MAXTYP: NUMBER  I*4
CC                        OF PARAMETERS:
CC                        I=1: #PARAMETERS OF TYPE I (NPARMS)
CC                        I=2: #SET-UP
CC                        I=3: #NO-OBS
CC                        I=4: #REF. PARAMETERS
CC                        I=5: #SINGULAR
CC               LOCQ(I,K),K=1,..,NPAR,I=1,..,MAXLCQ:         I*4
CC                        DEFINITION OF EACH PARAMETER
CC               ANOR   : NORMAL EQUATION MATRIX              R*8
CC               BNOR   : RIGHT HAND SIDE OF NEQ SYSTEM       R*8
CC               PARTYP : PARAMETER DESCRIPTION               t_partyp(*)
CC               NOBSPA : NUM.OF OBSERV PER PARAMETER         I*4(*,*)
CC                        NOBSPA(MAXMEA*ISYS+IMEA,IPAR)
CC        OUT :  NPAEPO : NUMBER OF EPOCH-SPECIFIC PARAMETERS I*4
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  L.MERVART, S.SCHAER
CC
CC VERSION    :  3.5
CC
CC CREATED    :  11-APR-94
CC
CC CHANGES    :  27-JUL-94 : MR: UPDATE PARAMETER LIST "PARLST"
CC               10-AUG-94 : MR: CALL EXITRC
CC               01-JUN-95 : SS: CHECK MAXIMUM NUMBER OF PARAMETERS
CC               08-JUN-95 : LM: KINEMATIC COORDINATES
CC               25-AUG-95 : SS: SET UP OBSERVED SIPS ONLY
CC               28-SEP-95 : JJ: DECLARE MXNFRQ AS C*6 INSTEAD OF I*4
CC               27-MAR-96 : TS: CLOCK/SLR CHANGES
CC               14-AUG-97 : SS: CONSISTENT USE OF "OPTELI"
CC               25-OCT-00 : RD: COUNT OBS. FOR CLOCKS
CC               22-JAN-02 : RD: CONDITION OF SUM FOR REFERENCE CLOCKS
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               12-AUG-05 : RD: CONSIDER SATELLITE SYSTEM FOR SIP
CC               17-JUN-08 : RD: COUNTER FOR OBSERV. PER PARAMETER ADDED
CC               09-MAY-09 : RD: SEPERATE RECEIVER CLOCKS FOR GPS/GLONASS
CC               19-JUL-10 : SL: TAB CHARACTERS REMOVED
CC               16-Nov-10 : RD: DISTINGUISH BETWEEN PIECE-WISE LINEAR PARAM.
CC               28-MAR-12 : RD: USE SVN2CHR AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1994     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: LFNERR
      USE d_clkrnx, ONLY: t_clkhead
      USE d_par,    ONLY: parType_epochSpecific
      USE p_gpsest, ONLY: t_partyp
      USE f_ikf
      USE f_tstflg
      USE f_mixsvn
      USE s_exitrc
      USE s_svn2chr
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 ICLK  , IFIL  , IFNUM , IFRQ  , IK    , IKIN  , IHLP  ,
     1          IPAR  , IREF  , ISAT  , ISTAT , ITFLG , ITYP  , K     ,
     2          KPAR  , MAXLOC, MAXPAR, MXCFRQ, MXCLCQ, MXCLOC, MXCPAR,
     3          MXCSAT, NCLKSA, NCLKST, NFIL  , NFLSES, NKIN  , NPAEPO,
     4          NPAR  , NPARMS, NPOLD , NSTAT , ISVN  , ISASYS, II    ,
     5          ICLOCK, CLKSYS
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      TYPE(t_clkhead) CLKHED
      TYPE(t_parTyp), DIMENSION(:) :: parTyp
C
      CHARACTER*1  OBSFLG(MXCSAT,MXCFRQ,*),FILACT(*),SVNCHR
      CHARACTER*6  MXNSAT,MXNLCQ,MXNPAR,MXNLOC,MXNFRQ
      CHARACTER*16 STNAME(*),CLKNAM
C
      INTEGER*4    LOCQ(MXCLCQ,*),PARLST(5,*),OPTELI(*)
      INTEGER*4    FILNUM(*),NSATFL(*),SVNFIL(MXCSAT,*)
      INTEGER*4    NFRFIL(*),ICARR(MXCFRQ,*),OBSNUM(*)
      INTEGER*4    ICENTR(*),STKIN(*),STFIL(*),MEATYP(*)
      INTEGER*4    CLKSTA(*),CLKSAT(*),NDIFF(*),NOBSPA(:,:)
C
      REAL*8       ANOR(*),BNOR(*),OBSERV(MXCSAT,MXCFRQ,*)
C
C COMMON FOR MAXIMAL DIMENSIONS
C -----------------------------
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
      COMMON/MCMPAR/MXCPAR,MXNPAR
      COMMON/MCMLOC/MXCLOC,MXNLOC
      COMMON/MCMFRQ/MXCFRQ,MXNFRQ
C
C
      MAXPAR=MXCPAR
      MAXLOC=MXCLOC
C
C SAVE TOTAL NUMBER OF PARAMETERS
      NPOLD=NPAR
C
C STOCHASTIC IONOSPHERE PARAMETERS
C --------------------------------
      ITYP=17
      IF (OPTELI(ITYP).EQ.3) THEN
        DO 100 IFIL=1,NFLSES
          IF (FILACT(IFIL).EQ.'U') GO TO 100
          IFNUM=FILNUM(IFIL)
          DO 200 ISAT=1,NSATFL(IFIL)
C
C SELECT SATELLITE SYSTEM
            CALL SVN2CHR(SVNFIL(ISAT,IFIL),ISVN,SVNCHR)
            IF (ISASYS.EQ.1.AND.SVNCHR.NE.'G')GOTO 200
            IF (ISASYS.EQ.2.AND.SVNCHR.NE.'R')GOTO 200
            IF (ISASYS.EQ.3.AND.SVNCHR.NE.'E')GOTO 200
            IF (ISASYS.EQ.4.AND.SVNCHR.NE.'G'.AND.SVNCHR.NE.'R')GOTO 200
            IF (ISASYS.EQ.5.AND.SVNCHR.NE.'G'.AND.SVNCHR.NE.'E')GOTO 200
            IF (ISASYS.EQ.6.AND.SVNCHR.NE.'R'.AND.SVNCHR.NE.'E')GOTO 200
C           ADD_GNSS_HERE
C
C IS SATELLITE OBSERVED AND UNMARKED AT LEAST ON ONE FREQUENCY ?
            ITFLG=0
            DO 150 IFRQ=1,NFRFIL(IFNUM)
              IF (.NOT.TSTFLG(OBSFLG(ISAT,IFRQ,IFIL),0).AND.
     1          OBSERV(ISAT,IFRQ,IFIL).NE.0.D0.AND.
     2          ICARR(IFRQ,IFNUM).NE.3) ITFLG=1
150         CONTINUE
            IF (ITFLG.EQ.0) GO TO 200
            NPAR=NPAR+1
            PARLST(1,ITYP)=PARLST(1,ITYP)+1
            PARLST(2,ITYP)=PARLST(2,ITYP)+1
            IF (NPAR.GT.MAXPAR) GO TO 980
            IF (NPAR.GT.MAXLOC) GO TO 990
            LOCQ(1,NPAR)=ITYP
            LOCQ(2,NPAR)=IFNUM
            LOCQ(3,NPAR)=SVNFIL(ISAT,IFIL)
            LOCQ(4,NPAR)=OBSNUM(IFIL)
            LOCQ(5,NPAR)=0
            PARTYP(NPAR)%TYPE=parType_epochSpecific
200       CONTINUE
100   CONTINUE
      END IF
C
C KINEMATIC COORDINATES
C ---------------------
      ITYP=21
      IF (OPTELI(ITYP).EQ.3) THEN
        DO 500 ISTAT=1,NSTAT
          IF (ICENTR(ISTAT).EQ.ISTAT) THEN
C
            DO 510 IKIN=1,NKIN
              IF (STKIN(IKIN).EQ.ISTAT) THEN
                DO 530 K=1,3
                  NPAR=NPAR+1
                  PARLST(1,ITYP)=PARLST(1,ITYP)+1
                  PARLST(2,ITYP)=PARLST(2,ITYP)+1
                  IF (NPAR.GT.MAXPAR) GO TO 980
                  IF (NPAR.GT.MAXLOC) GO TO 990
                  LOCQ(1,NPAR)=ITYP
                  LOCQ(2,NPAR)=ISTAT
                  LOCQ(3,NPAR)=K
                  LOCQ(4,NPAR)=-1
                  LOCQ(5,NPAR)=0
                  LOCQ(6,NPAR)=0
                  PARTYP(NPAR)%TYPE=parType_epochSpecific
530             CONTINUE
              END IF
510         CONTINUE
C
          END IF
500     CONTINUE
      END IF
C
C Epoch wise station clocks
C -------------------------
      ITYP=23
      IF (OPTELI(ITYP).EQ.3) THEN
C
C Station loop
C ------------
        DO 1000 ICLK=1,NCLKST
          DO 1010 IFIL=1,NFLSES
            NFIL=FILNUM(IFIL)
            IF ((NDIFF(NFIL).EQ.0).AND.
     1         (CLKSTA(ICLK).EQ.STFIL((NFIL*2)-1))) THEN
C
              ICLOCK=-1
              IF (CLKSYS.EQ.1) THEN
                ICLOCK=MIXSVN(NSATFL(IFIL),SVNFIL(1,IFIL))
              ENDIF
C
              DO II=1,2
                IF (II.EQ.1.AND.ICLOCK.EQ.2) CYCLE
C
                NPAR=NPAR+1
                PARLST(1,ITYP)=PARLST(1,ITYP)+1
                PARLST(2,ITYP)=PARLST(2,ITYP)+1
                IF (NPAR.GT.MAXPAR) GO TO 980
                IF (NPAR.GT.MAXLOC) GO TO 990
                LOCQ(1,NPAR)=ITYP
                LOCQ(2,NPAR)=CLKSTA(ICLK)
                IF (CLKSYS.EQ.0) THEN
                  LOCQ(3,NPAR)=0
                ELSE
                  LOCQ(3,NPAR)=II
                ENDIF
                LOCQ(4,NPAR)=-1
                LOCQ(5,NPAR)=NFIL
                LOCQ(6,NPAR)=0
                LOCQ(7,NPAR)=0
                IF (CLKHED%NUMREF.EQ.2) THEN
                  CLKNAM=STNAME(CLKSTA(iCLK))
                  DO 1030 IREF=1,CLKHED%REF(1)%NREF
                    IF (CLKHED%REF(1)%CLK(IREF)%NAME.EQ.CLKNAM) THEN
                      LOCQ(7,NPAR)=IREF
                    ENDIF
1030              CONTINUE
                ENDIF
                PARTYP(NPAR)%TYPE=parType_epochSpecific
C
                IF (ICLOCK.NE.1) EXIT
              ENDDO
              GOTO 1000
            END IF
1010      CONTINUE
1000    CONTINUE
      END IF
C
C Epoch wise satellite clocks
C -------------------------
      ITYP=24
      IF (OPTELI(ITYP).EQ.3) THEN
C
C Satellite loop
C --------------
        DO 1100 ICLK=1,NCLKSA
          DO 1110 IFIL=1,NFLSES
            DO 1120 ISAT=1,NSATFL(IFIL)
C
C             SELECT SATELLITE SYSTEM
              CALL SVN2CHR(SVNFIL(ISAT,IFIL),ISVN,SVNCHR)
              IF (ISASYS.EQ.1 .AND. SVNCHR.NE.'G') GOTO 1120
              IF (ISASYS.EQ.2 .AND. SVNCHR.NE.'R') GOTO 1120
C              IF (ISASYS.EQ.3 .AND. SVNCHR.NE.'E') GOTO 1120
C             ADD_GNSS_HERE
C
              IF(CLKSAT(ICLK).EQ.SVNFIL(ISAT,IFIL)) THEN
                NPAR=NPAR+1
                PARLST(1,ITYP)=PARLST(1,ITYP)+1
                PARLST(2,ITYP)=PARLST(2,ITYP)+1
                IF (NPAR.GT.MAXPAR) GO TO 980
                IF (NPAR.GT.MAXLOC) GO TO 990
                LOCQ(1,NPAR)=ITYP
                LOCQ(2,NPAR)=0
                LOCQ(3,NPAR)=clksat(iclk)
                LOCQ(4,NPAR)=-1
                LOCQ(5,NPAR)=1
                LOCQ(6,NPAR)=0
                LOCQ(7,NPAR)=0
                IF (CLKHED%NUMREF.EQ.2) THEN
                  CLKNAM = ' '
                  CALL SVN2CHR(CLKSAT(ICLK),IHLP,CLKNAM(1:1))
                  WRITE(CLKNAM(2:3),'(I2.2)') IHLP
                  DO 1130 IREF=1,CLKHED%REF(1)%NREF
                    IF (CLKHED%REF(1)%CLK(IREF)%NAME.EQ.CLKNAM) THEN
                      LOCQ(7,NPAR)=IREF
                    ENDIF
1130              CONTINUE
                ENDIF
                PARTYP(NPAR)%TYPE=parType_epochSpecific
                GOTO 1100
              ENDIF
1120      CONTINUE
1110      CONTINUE
1100    CONTINUE
      END IF
C
      NPAEPO=NPAR-NPOLD
      NPARMS=NPARMS+NPAEPO
C
C INITIALIZATION OF NORMAL EQUATION MATRICES ANOR AND BNOR
C --------------------------------------------------------
      DO 400 IPAR=NPAR-NPAEPO+1,NPAR
        BNOR(IPAR)=0.D0
        DO 300 KPAR=1,NPAR
          IK=IKF(IPAR,KPAR)
          ANOR(IK)=0.D0
300     CONTINUE
        NOBSPA(:,IPAR)=0
400   CONTINUE
C
      GO TO 999
C
C TOO MANY PARAMETERS FOR ANOR AND BNOR
C -------------------------------------
980   WRITE(LFNERR,252) NPAR,MAXPAR
252   FORMAT(/,' *** SR SEQEPO: TOO MANY (EPOCH-SPECIFIC) PARA',
     1          'METERS',/,16X,'NUMBER OF PARAMETERS >=',I5,/,
     2                     16X,'MAXIMUM NUMBER        :',I5,/)
      CALL EXITRC(2)
C
C TOO MANY PARAMETERS FOR LOCQ
C ----------------------------
990   WRITE(LFNERR,251) NPAR,MAXLOC
251   FORMAT(/,' *** SR SEQEPO: TOO MANY PARAMETERS TO BE CHARACTER',
     1            'IZED',/,16X,'NUMBER OF PARAMETERS >=',I5,/,
     2                     16X,'MAXIMUM NUMBER        :',I5,/)
      CALL EXITRC(2)
C
999   RETURN
      END SUBROUTINE

      END MODULE
