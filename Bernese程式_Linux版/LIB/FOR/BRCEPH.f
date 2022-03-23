      MODULE s_BRCEPH
      CONTAINS

C*
      SUBROUTINE BRCEPH(EPOCH,SATNUM,X,CLOCKC,EPH,CLOCK,IRCODE)
CC
CC NAME       :  BRCEPH
CC
CC PURPOSE    :  GET BROADCAST EPHEMERIS (EFEC COORD.) AND SATELLITE
CC               CLOCK CORRECTION DATA OF ONE EPOCH FOR ONE SATELLITE
CC               THE BROADCAST FILE IS INTERNALLY READ ON THE FIRST
CC               CALL
CC
CC               STRUCTURE OF ARRAYS CLOCK AND EPH:
CC                 FOR FURTHER INFORMATION SEE
CC                 VAN DIERENDONCK ET AL. "GPS NAVIGATION MESSAGE"
CC                 NAVIGATION I, P.55-73, INSTITUTE OF NAVIGATION,
CC                 WASHINGTON, 1980
CC                 C. PAYNE "NAVSTAR GPS: 1982", PROC. OF 3.INTERN.
CC                 GEOD. SYMP. ON SAT. DOP. POS., VOL 2, 1982 (P. 1015)
CC
CC               ARRAY CLOCK :
CC
CC               CLOCK(1)            : GPS-WEEK
CC               CLOCK(2)            : L2 CODE INDICATOR
CC               CLOCK(3)            : USER RANGE ACCURACY (M)
CC               CLOCK(4)            : SV HEALTH MSB (NAVIGATION DATA)
CC               CLOCK(5)            : SV HEALTH LSB'S (SIGNAL COMPONENT
CC               CLOCK(6)            : L2 P DATA FLAG
CC               CLOCK(7)            : NOT USED
CC               CLOCK(8)            : NOT USED
CC               CLOCK(9)            : TGD
CC               CLOCK(10)           : AODC
CC               CLOCK(11)           : TOC
CC               CLOCK(12)           : A2
CC               CLOCK(13)           : A1
CC               CLOCK(14)           : A0
CC               CLOCK(14+I),I=1,2,..,6 : NOT USED
CC
CC               ARRAY EPH   :
CC               EPH(1)       : GPS-WEEK (=CLOCK(1))
CC               EPH(2)       : T0E
CC               EPH(3)       : A
CC               EPH(4)       : E
CC               EPH(5)       : I
CC               EPH(6)       : R.A. OF ASCENDING NODE
CC               EPH(7)       : PERIGEE
CC               EPH(8)       : MEAN ANOMALY (T0E)
CC               EPH(9)       : DN (CORRECTION TO MEAN MOTION)
CC               EPH(10)      : RATE OF NODE
CC               EPH(11)      : CUS
CC               EPH(12)      : CUC
CC               EPH(13)      : CRS
CC               EPH(14)      : CRC
CC               EPH(15)      : CIS
CC               EPH(16)      : CIC
CC               EPH(17)      : AODE
CC               EPH(18)      : IDOT
CC               EPH(I),I=19,20 : NOT USED
CC
CC               EPHEMERIS COMPUTATION: SEE
CC                 VAN DIERENDONCK ET AL. "GPS NAVIGATION MESSAGE"
CC                 NAVIGATION I, P.55-73, INSTITUTE OF NAVIGATION,
CC                 WASHINGTON, 1980
CC
CC PARAMETERS :
CC         IN :  EPOCH  : EPOCH (MJD)                              R*8
CC               SATNUM : SPACE VEHICLE NUMBER  OF SATELLITE       I*4
CC        OUT :  X(I)  :  EARTH FIXED SATELLITE                  R*8(3)
CC                        COORDINATES IN WGS SYSTEM
CC               CLOCKC : ACTUAL SATELLITE CLOCK CORRECTION (SEC)  R*8
CC      LOCAL :  EPH(20*MXCEPH,MXCSAT)                         R*8(*,*)
CC               CLOCK(20*MXCEPH,MXCSAT)                       R*8(*,*)
CC        OUT :  IRCODE : RETURN CODE                              I*4
CC                        =0: OK
CC                        =1: SATELLITE NOT FOUND IN BROADC.FILE
CC
CC AUTHOR     :  T. SCHILDKNECHT
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/10/19 09:11
CC
CC CHANGES    :  02-JUN-92 : ??: GM NOT FROM CONSTANT FILE ! WGS-84 VALUE
CC                               USED (GMWGS=398.6005D12)
CC               28-OCT-93 : ??: HANDLING OF MANOEUVRES, ADD RETURN CODE
CC                               TO PARAMETERS
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               10-AUG-94 : MR: CALL EXITRC
CC               17-AUG-94 : MR: MAXMAN=100 (OLD MAXMAN=10)
CC               23-SEP-97 : DI: USE MAXSAT.inc
CC               29-MAY-00 : HU: IMPLICIT MOVED TO FIRST LINE
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               26-FEB-08 : RD: USE GTSATM FROM D_SATCRX
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
C
C DECLARATIONS
C
      USE m_bern
      USE m_maxdim, ONLY: MAXSAT
      USE d_const,  ONLY: OMEGA
      USE d_satcrx, ONLY: gtsatm
      USE s_maxtst
      USE s_gtbrdc
      USE s_exitrc
      USE f_gpsmjd
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , II    , IMAN  , IMIS  , INIT  , IRC   , IRCODE,
     1          ISAMAN, ISAT1 , ISC   , ISE   , ISTRTC, ISTRTE, ITSET ,
     2          KK    , MAXMAN, MXCEPH, MXCSAT, NMAN  , NMIS  , NSAT
C
      REAL*8    A     , CLKEPO, CP    , CTEST , DI    , DMINC , DMINE ,
     1          DR    , DT    , DU    , E     , EPHEPO, ETEST , EX    ,
     2          GMWGS , PHI   , R     , SP    , TTT   , U     ,
     3          V     , XI    , XM    , XN    , XNODE , XP    , YP
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MAXMAN=100)
C
      CHARACTER*53 TITLE
      CHARACTER*32 FILBRD
      CHARACTER*6  MXNSAT,MXNEPH
C
      REAL*8       X(3),CLOCKC,EPOCH
      REAL*8       EPH(20*MXCEPH,MXCSAT),CLOCK(20*MXCEPH,MXCSAT)
      REAL*8       TIMMAN(MAXMAN)
C
      INTEGER*4    SATNUM,NRSAT(MAXSAT),NEPH(MAXSAT),SATMAN(MAXMAN)
      INTEGER*4    SATMIS(MAXSAT)
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMEPH/MXCEPH,MXNEPH
C
      DATA INIT/1/
      DATA GMWGS /398.6005D12/
C
C CHECK MAXIMAL LOCAL DIMENSIONS
C ------------------------------
      CALL MAXTST(1,'BRCEPH',MXNSAT,MAXSAT,MXCSAT,IRC)
      IF(IRC.NE.0) CALL EXITRC(2)
C
C  IF "INIT"=1, THEN READ EPHEMERIS AND CLOCK CORRECTIONS DATA SETS
C  ----------------------------------------------------------------
      IF(INIT.EQ.1) THEN
C
C GET BROADCAST MESSAGES
        CALL GTFLNA(1,'BRDEPH ',FILBRD,IRC)
        CALL GTBRDC(FILBRD,TITLE,NSAT,NRSAT,NEPH,EPH,CLOCK)
        NMIS=0
C
C GET SATELLITE MANOEUVRES
        CALL GTSATM(MAXMAN,NMAN,SATMAN,TIMMAN)
        INIT=0
      ENDIF
C
C  IF "INIT" NOT =1, THEN COMPUTE COORDINATES AND CLOCK CORRECTIONS
C  FOR REQUESTED SATELLITE (SATNUM)
C  ----------------------------------------------------------------
C
C  DETERMINE  INTERNAL SAT. NUMBER FOR EPHEMERIS
      ISAT1=0
      ISAMAN=0
      DO 75 II=1,NSAT
        IF (NRSAT(II).EQ.SATNUM) ISAT1=II
        IF (NRSAT(II).EQ.SATNUM+50) ISAMAN=II
75    CONTINUE
C
C SATELLITE AFTER MANOEUVRE: SVN=SVN+50
      DO 145 IMAN=1,NMAN
        IF (SATMAN(IMAN).EQ.SATNUM .AND.
     1      ISAMAN.NE.0            .AND.
     2      TIMMAN(IMAN).LE.EPOCH) THEN
          ISAT1=ISAMAN
          GOTO 147
        ENDIF
145   CONTINUE
147   CONTINUE
C
C SATELLITE FOUND ?
      IF(ISAT1.EQ.0) THEN
        DO 148 IMIS=1,NMIS
          IF (SATMIS(IMIS).EQ.SATNUM) GOTO 149
148     CONTINUE
        NMIS=NMIS+1
        SATMIS(NMIS)=SATNUM
        WRITE(LFNERR,77) SATNUM
77      FORMAT(/' ### SR BRCEPH: SATELLITE NOT FOUND IN EPH.-FILE'/,
     1                      16X,'SATELLITE:',I3,/)
149     IRCODE=1
        GOTO 999
      END IF
C
C  FIND CLOSEST EPOCH TO TIME TTT
      ISE=1
      ISC=1
      TTT=EPOCH-1800.D0/86400
      DMINE=1.D10
      DMINC=1.D10
      DO 80 KK=1,NEPH(ISAT1)
        ITSET=(KK-1)*20
        EPHEPO=GPSMJD(EPH(2+ITSET,ISAT1),IDINT(EPH(1+ITSET,ISAT1)))
        CLKEPO=GPSMJD(CLOCK(11+ITSET,ISAT1),
     1                IDINT(CLOCK(1+ITSET,ISAT1)))
        ETEST=DABS(TTT-EPHEPO)
        CTEST=DABS(TTT-CLKEPO)
        IF(ETEST.GT.DMINE)GO TO 78
        DMINE=ETEST
        ISE=KK
78      CONTINUE
        IF(CTEST.GT.DMINC)GO TO 80
        DMINC=CTEST
        ISC=KK
80    CONTINUE
      ISTRTE=20*(ISE-1)
      ISTRTC=20*(ISC-1)
C
C COMPUTE EPHEMERIS (SEE "NAVIGATION")
      A=EPH(ISTRTE+3,ISAT1)
      XN=DSQRT(GMWGS/A**3)
      DT=EPOCH-GPSMJD(EPH(ISTRTE+2,ISAT1),IDINT(EPH(ISTRTE+1,ISAT1)))
      DT=DT*86400.D0
      XN=XN+EPH(ISTRTE+9,ISAT1)
      XM=EPH(ISTRTE+8,ISAT1)+XN*DT
      EX=XM
      E=EPH(ISTRTE+4,ISAT1)
      DO 60 I=1,10
        EX=XM+E*DSIN(EX)
60    CONTINUE
      V=2.D0*DATAN(DSQRT((1.D0+E)/(1.D0-E))*DTAN(EX/2.D0))
      PHI=V+EPH(ISTRTE+7,ISAT1)
      CP=DCOS(2.D0*PHI)
      SP=DSIN(2.D0*PHI)
      DU=EPH(ISTRTE+11,ISAT1)*SP+EPH(ISTRTE+12,ISAT1)*CP
      DR=EPH(ISTRTE+14,ISAT1)*CP+EPH(ISTRTE+13,ISAT1)*SP
      DI=EPH(ISTRTE+16,ISAT1)*CP+EPH(ISTRTE+15,ISAT1)*SP
      R=A*(1-E*DCOS(EX))+DR
      U=PHI+DU
      XI=EPH(ISTRTE+5,ISAT1)+DI+EPH(ISTRTE+18,ISAT1)*DT
      XP=R*DCOS(U)
      YP=R*DSIN(U)
      XNODE=EPH(ISTRTE+6,ISAT1)+(EPH(ISTRTE+10,ISAT1)
     1      -OMEGA)*DT-OMEGA*EPH(ISTRTE+2,ISAT1)
      X(1)=XP*DCOS(XNODE)-YP*DCOS(XI)*DSIN(XNODE)
      X(2)=XP*DSIN(XNODE)+YP*DCOS(XI)*DCOS(XNODE)
      X(3)=YP*DSIN(XI)
C
C COMPUTE CLOCK CORRECTION
      DT=EPOCH-
     1     GPSMJD(CLOCK(ISTRTC+11,ISAT1),IDINT(CLOCK(ISTRTC+1,ISAT1)))
      DT=DT*86400.D0
      CLOCKC=CLOCK(ISTRTC+14,ISAT1)
     1       +CLOCK(ISTRTC+13,ISAT1)*DT
     2       +CLOCK(ISTRTC+12,ISAT1)*DT**2
C
      IRCODE=0
C
999   CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
