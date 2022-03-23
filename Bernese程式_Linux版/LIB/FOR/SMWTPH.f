      MODULE s_SMWTPH
      CONTAINS

C*
      SUBROUTINE SMWTPH(FILPHA,NFREQ,NEPO,NSATEL,CSESS,IWLF,IDELTT,
     1                  TSTART,CAMPGN,TITLE,IFRMAT,STANAM,RECTYP,
     2                  ANTTYP,IRUNIT,IANTEN,OPRNAM,NUMSAT,
     3                  NCLOCK,CLOCK,NOBSVN,PRANG,ZENDIS,RMS,LEORMS,
     4                  ICOELV,LEOELV,IX,NSLIPS,ISIZE,JL12EQ)
CC
CC NAME       :  SMWTPH
CC
CC
CC PURPOSE    :  WRITE PHASE HEADER AND PHASE OBSERVATION FILE
CC
CC PARAMETERS :
CC         IN :  FILPHA(2): PHASE HEADER AND OBS.FILE NAME  CH*32
CC               NFREQ  : NUMBER OF FREQUENCIES IN PHASE     I*4
CC               NEPO   : NUMBER OF EPOCHS
CC               NSATEL : NUMBER OF SATELLITES               I*4
CC               CSESS(2): SESSION IDENTIFIER, FILE IDENT.   CH*4
CC                        WITHIN SESSION
CC               IWLF(I),I=1,NFREQ: WAVELENGTH FACTORS       I*4
CC               IDELTT : INTERVAL BETWEEN OBSERVAT. IN SEC  I*4
CC               TSTART : STARTING TIME                      R*8
CC               CAMPGN : CAMPAIGN NAME                      CH*16
CC               TITLE  : TITLE LINE                         CH*53
CC               IFRMAT : FORMAT NUMBER                      I*4
CC               STANAM : STATION NAME                       CH*16
CC               RECTYP : RECEIVER TYPE                      CH*20
CC               ANTTYP : ANTENNA TYPE                       CH*20
CC               IRUNIT : UNIT NUMBER                        I*4
CC               IANTEN : ANTENNA NUMBER                     I*4
CC               OPRNAM : OPERATOR NAME                      CH*20
CC               NUMSAT(I),I=1,2,..,NSATEL: SVN NUMBERS      I*4
CC               NCLOCK : NUMBER OF CLOCK VALUES             I*4
CC               CLOCK(I),I=1,2,..,NCLOCK: CLOCK COEFFIC.    R*8
CC               NOBSVN(I),I=1,2,..,NSATEL: NUMBER OF OBS.   I*4
CC               PRANG(I,L,K),I=1,..,NEPO,L=1,2,             R*8
CC                        K=1,..,NSATEL:
CC                        PSEUDORANGE FOR FREQ. L AND SAT K
CC               ZENDIS(I,J),I=1,..,MAXEPO,J=1,..,MAXSAT:    R*8(*,*)
CC                        ZENITH DISTANCES IN RAD
CC               RMS(L),L=1,2: RMS FOR FREQUENCY L           R*8
CC               LEORMS(L),L=1,2: RMS FOR FREQUENCY L (LEO)  R*8
CC               ICOELV : MODEL FOR ELEV.-DEP. OBS. WEIGHTINGI*4
CC                        =0: EQUAL WEIGHTING FOR ALL OBS.
CC                        >0: MODEL NUMBER (SEE SR WGTELV)
CC               LEOELV : MODEL FOR ELEV.-DEP. OBS.          I*4
CC                        WEIGHTING, LEO ONLY
CC                        =0: EQUAL WEIGHTING FOR ALL OBS.
CC                        >0: MODEL NUMBER (SEE SR WGTELV)
CC               IX     : INITIAL RANDOM NUMBER              I*4
CC               NSLIPS : NUMBER OF SLIPS TO BE GENERATED    I*4
CC               ISIZE  : AVERAGE SIZE                       I*4
CC               JL12EQ : SAME SLIPS FOR L1 AND L2 (NO=0,    I*4
CC                          YES=1)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC CREATED    :  88/02/26 08:39
CC
CC CHANGES    :  11-JAN-93 : ??: USE OF SR "OPNFIL" TO OPEN FILES
CC               27-AUG-93 : ??: NEW FORMAT, VERSION 3.5
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               10-AUG-94 : MR: CALL EXITRC
CC               14-AUG-94 : MR: FORMAT 4: SESSION AS CHARACTER*4
CC               24-SEP-97 : DI: USE MAXSAT.inc
CC               23-OCT-97 : SS: ELEVATION-DEPENDENT SIGMAS
CC               07-AUG-99 : MR: ADD GLONASS FREQUENCIES
CC               13-JUN-00 : RD: COVI HAS TO BE REAL*4
CC               28-AUG-01 : DS: NO ELEVATION DEPENDENT WEIGHTING FOR LEO
CC               30-SEP-01 : DS: WHITE NOISE FOR SPACEBORNE LEO RECEIVER
CC               30-SEP-01 : DS: ELEV. DEP. WEIGHTING FOR LEO
CC               26-JUN-02 : RD: USE D_STACRX INSTEAD OF D_STACRUX
CC               10-OCT-02 : MR: CHANGE COMPUTATION OF OBSTIM, NEPOCH
CC               28-JAN-03 : RS: COVI REAL*4 -> REAL*8
CC               28-JAN-03 : AJ: BUGFIX FOR AMBIGUITY EPOCHS
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               23-JUN-03 : HB: INTERFACE FOR SR STAFLG
CC               08-SEP-03 : HU: ANTNAM, RECNAM, OPRNAM CHR16 -> CHR20
CC               10-MAR-04 : HB: CHANGE ORDER OF MODULES
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               03-DEC-08 : RD: VARIABLE ERROR -> NOISE (COMPILER FILTER)
CC               26-JAN-11 : LP: CHANGED CALL TO WTHEAD
CC               05-MAR-12 : RD: USE WTHEAD AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: i4b, r8b, lfnprt, lfnerr, lfnloc
      USE m_maxdim, ONLY: MAXSAT
      USE d_stacrx, ONLY: MTypeSPACE
      USE d_const,  ONLY: DATE, PI, TIME
      USE d_rinex3, ONLY: t_gobsdef
      USE s_opnfil
      USE s_normal
      USE s_wtobsi
      USE s_randu
      USE s_opnerr
      USE s_maxtst
      USE s_staflg
      USE s_wgtelv
      USE s_exitrc
      USE s_wthead
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IAMB  , IANTEN, ICLK  , ICLOCK, ICOEL2, ICOELV,
     1          IDELTT, IEPO  , IFLAG , IFREQ , IFRMAT, IOSTAT, IRC   ,
     2          IRMARK, IRUNIT, ISAT  , ISATEL, ISEPA , ISIZE , ISLIP ,
     3          IX    , IY    , JEPO  , JL12EQ, JSTART, JUMPAC, JUMPS ,
     4          LEOELV, LOC   , MAXAMB, MAXSLP, MEATYP, MXCAMB, MXCEPO,
     5          MXCSAT, NCLOCK, NDIFF , NEPFLG, NEPO  , NEPOCH, NFREQ ,
     6          NFRQ  , NSATEL, NSATNW, NSEPA , NSEPO , NSLIPS, NUMAMB
C
      REAL*8    ELEVA , NOISE , OBSTIM, RMSELV, TFIRST, TIMREF, TLAST ,
     1          TSTART, YFL
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
C LOCAL MAXIMUM DIMENSIONS
      PARAMETER (MAXAMB=500,MAXSLP=100)
C
C DECLARATIONS
C
      TYPE(t_gobsdef) :: gobsdef ! Giove External Obs. Selection info
C
      INTEGER*4 NUMSAT(*),NOBSVN(*),NUMOBS(MAXSAT,2),NUMMRK(MAXSAT,2)
      INTEGER*4 AMBIEP(MAXAMB),SVNEP(MAXSAT)
      INTEGER*4 SVNEPA(MAXSAT),USEGEOS
      INTEGER*4 SATEP(MAXSAT),JUMP(3,MAXSLP,2)
      INTEGER*4 IWLF(2),SATNEW(MAXSAT)
      INTEGER*4 AMBSAT(MAXAMB),AMBWLF(MAXAMB,2),AMBCLS(MAXAMB,3)
C
      REAL*8 CLOCK(*),PRANG(MXCEPO,2,*),OBSERV(MAXSAT,2)
      REAL*8 POSECC(3),AMBIGU(MAXAMB,3),DELTAT(2),ZENDIS(MXCEPO,*)
      REAL*8 RMS(*),LEORMS(*),RMS2(2)
C
      REAL*8 COVI
C
      CHARACTER*53 TITLE
      CHARACTER*32 FILPHA(2)
      CHARACTER*20 MARTYP
      CHARACTER*20 RECTYP,ANTTYP,OPRNAM
      CHARACTER*16 STANAM,CAMPGN
      CHARACTER*9  CRDATE(2)
      CHARACTER*6  MXNEPO,MXNSAT,MXNAMB
      CHARACTER*5  CRTIME(2)
      CHARACTER*4  CSESS(2)
      CHARACTER*1  EPOFLG,OBSFLG(MAXSAT,2)
C
      LOGICAL*4    YES
C
C VARIABLES TO SERVE WTHEAD (assigned for baseline files with two stations)
      CHARACTER(LEN=16),DIMENSION(2)      :: hlp_staNam
      CHARACTER(LEN=20),DIMENSION(2)      :: hlp_recTyp
      CHARACTER(LEN=20),DIMENSION(2)      :: hlp_antTyp
      INTEGER(i4b),DIMENSION(2)           :: hlp_irUnit
      INTEGER(i4b),DIMENSION(2)           :: hlp_iAnten
      CHARACTER(LEN=20),DIMENSION(2)      :: hlp_oprNam
      REAL(r8b),DIMENSION(3,2)            :: hlp_posecc
      INTEGER(i4b),DIMENSION(2)           :: hlp_iClock
C
      INCLUDE 'COMFREQ.inc'
      COMMON/MCMEPO/MXCEPO,MXNEPO
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMAMB/MXCAMB,MXNAMB
C
      MXCAMB=MAXAMB
      MXNAMB='MAXAMB'
C
C CHECK MAXIMUM NUMBER OF SATELLITES
C ----------------------------------
      CALL MAXTST(0,'SMWTPH',MXNSAT,MAXSAT,MXCSAT,IRC)
      IF(IRC.NE.0) CALL EXITRC(2)
C
C CHECK MAXIMUM NUMBER OF SLIPS
C -----------------------------
      IF(NSLIPS.GT.MAXSLP) THEN
        WRITE(LFNERR,901) NSLIPS,MAXSLP
901     FORMAT(/,' *** SR SMWTPH: TOO MANY CYCLE SLIPS',/,
     1                       16X,'NUMBER OF CYCLE SLIPS:',I4,/,
     2                       16X,'MAX. NUMBER OF SLIPS :',I4,/)
        CALL EXITRC(2)
      END IF
C
C GENERAL HEADER INFORMATION
C --------------------------
      MEATYP=1
      NDIFF =0
      IRMARK=0
      NEPFLG=0
      ICLOCK=NCLOCK
C
      DO 10 I=1,2
        CRDATE(I)=DATE
        CRTIME(I)=TIME
10    CONTINUE
C
      DO 20 I=1,3
        POSECC(I)=0.D0
20    CONTINUE
C
C INITIALIZE NUMBER OF FREQ.
C --------------------------
      IF (NFREQ.EQ.2) THEN
        NFRQ=3
      ELSE
        NFRQ=1
      END IF
C
C LOOP OVER ALL SATELLITES TO PREPARE HEADER INFO
C -----------------------------------------------
      DO 40 ISAT=1,NSATEL
        DO 30 IFREQ=1,NFREQ
C
C NUMBER OF GOOD AND BAD OBSERVATIONS
          NUMOBS(ISAT,IFREQ)=NOBSVN(ISAT)
          NUMMRK(ISAT,IFREQ)=0
30      CONTINUE
40    CONTINUE
C
C GENERATE CYCLE SLIPS
C --------------------
      IF(NSLIPS.NE.0) THEN
        WRITE(LFNPRT,601) STANAM,NSLIPS,ISIZE
601     FORMAT(//,' STATION : ',A16,/,
     1              I6,'  CYCLE SLIPS OF AVERAGE SIZE',I8,
     2            ' APPLIED',//,' FREQ.  NUMBER  EPOCH',
     3            '  SAT.   # OF CYCLES',/)
      ENDIF
C
C LOOP OVER FREQUENCIES AND SLIPS
C -------------------------------
      DO 600 IFREQ=1,NFREQ
        DO 590 ISLIP=1,NSLIPS
C
C DEFINE EPOCH FOR SLIP
          CALL RANDU(IX,IY,YFL)
          IX=IY
          LOC=YFL*NEPO+1
          IF(LOC.GT.NEPO) LOC=NEPO
          NSEPO=0
C
C DEFINE SATELLITE FOR SLIP
          DO 510 I=1,NSATEL
            IF(PRANG(LOC,IFREQ,I).NE.0) THEN
              NSEPO=NSEPO+1
              SATEP(NSEPO)=NUMSAT(I)
            END IF
510       CONTINUE
          IF(NSEPO.EQ.0) GOTO 590
          CALL RANDU(IX,IY,YFL)
          IX=IY
          ISAT=YFL*NSEPO+1
          IF(ISAT.GT.NSEPO) ISAT=NSEPO
C
C DEFINE SIZE OF SLIP
          CALL RANDU(IX,IY,YFL)
          IX=IY
          JUMPS=ISIZE*(YFL-.5D0)*2
          IF(JUMPS.EQ.0) JUMPS=1
          JUMP(1,ISLIP,IFREQ)=LOC
          JUMP(2,ISLIP,IFREQ)=SATEP(ISAT)
          JUMP(3,ISLIP,IFREQ)=JUMPS
C
C WRITE CYCLE SLIP INFO
          WRITE(LFNPRT,505)IFREQ,ISLIP,LOC,SATEP(ISAT),JUMPS
505       FORMAT(I4,2I8,I6,I13)
C
590     CONTINUE
600   CONTINUE
C
C SAME SLIP FOR L2
      IF(JL12EQ.EQ.1.AND.NFREQ.EQ.2) THEN
        WRITE(LFNPRT,602)
602     FORMAT(' SAME SLIPS FOR L2')
        WRITE(LFNPRT,*)
        WRITE(LFNPRT,*)
        DO 620 ISLIP=1,NSLIPS
          DO 610 I=1,3
            JUMP(I,ISLIP,2)=JUMP(I,ISLIP,1)
610       CONTINUE
620     CONTINUE
      ENDIF
C
C OPEN PHASE OBSERVATION FILE
C ---------------------------
      CALL OPNFIL(LFNLOC,FILPHA(2),'UNKNOWN','UNFORMATTED',
     1            ' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILPHA(2),'SMWTPH')
C
C WRITE OBSERVATIONS
C ------------------
      DELTAT(1)=0.D0
      EPOFLG=CHAR(0)
      TFIRST=1.D20
C
C LOOP OVER ALL EPOCHS
C ----------------------------
      IAMB=0
      NSEPA=0
      JEPO=1
      JSTART=0
C
      DO 100 IEPO=1,NEPO
C
C OBSERVATION TIME AND CLOCK CORRECTION
        OBSTIM=TSTART+(IEPO-1)*IDELTT/86400.D0
        IF(NCLOCK.EQ.0) THEN
          DELTAT(2)=0.D0
        ELSE
          DELTAT(2)=-CLOCK(1)
          DO 50 ICLK=2,NCLOCK
            DELTAT(2)=DELTAT(2)-CLOCK(ICLK)*(OBSTIM-TSTART)**(ICLK-1)
50        CONTINUE
        ENDIF
C
C USE STATISTICS FOR STATION OR LEO
C ---------------------------------
        CALL STAFLG(STANAM,OBSTIM,IFLAG,MARTYP)
        IF (MARTYP.NE.MTypeSPACE) THEN
          ICOEL2=ICOELV
          RMS2(1:2)=RMS(1:2)
        ELSE
          ICOEL2=LEOELV
          RMS2(1:2)=LEORMS(1:2)
        ENDIF
C
C OBSERVATIONS AND OBSERVATION FLAGS
        NSEPO=0
        DO 70 ISAT=1,NSATEL
          IF(PRANG(IEPO,    1,ISAT).NE.0.D0.OR.
     1       PRANG(IEPO,NFREQ,ISAT).NE.0.D0) THEN
            NSEPO=NSEPO+1
            SVNEP(NSEPO)=NUMSAT(ISAT)
            JSTART=1
C
C SET UP AMBIGUITY
            YES=.TRUE.
            DO ISEPA=1,NSEPA
              IF (SVNEPA(ISEPA).EQ.SVNEP(NSEPO)) YES=.FALSE.
            END DO
            IF (YES) THEN
              IAMB=IAMB+1
C
C CHECK MAXIMUM NUMBER OF AMBIGUITIES
C -----------------------------------
              CALL MAXTST(0,'SMWTPH',MXNAMB,MAXAMB,MXCAMB,IRC)
              IF(IRC.NE.0) CALL EXITRC(2)
C
              AMBSAT(IAMB)=SVNEP(NSEPO)
              AMBIEP(IAMB)=JEPO
              AMBWLF(IAMB,1:NFREQ)=IWLF(1:NFREQ)
              AMBCLS(IAMB,1:NFRQ)=IAMB
              AMBIGU(IAMB,1:NFRQ)=0.D0
            END IF
C
            DO 60 IFREQ=1,NFREQ
              IF(PRANG(IEPO,IFREQ,ISAT).NE.0.D0) THEN
C
C ADJUST ELEVATION-DEPENDENT SIGMA
                ELEVA=(PI/2.D0-ZENDIS(IEPO,ISAT))*180.D0/PI
                CALL WGTELV(ICOEL2,ELEVA,COVI)
                RMSELV=RMS2(IFREQ)*DSQRT(DBLE(COVI))
                CALL NORMAL(RMSELV,0.D0,IX,NOISE)
C
C APPLY CYCLE SLIPS
                JUMPAC=0
                DO 55 ISLIP=1,NSLIPS
                  IF(JUMP(2,ISLIP,IFREQ).EQ.SVNEP(NSEPO).AND.
     1               IEPO.GE.JUMP(1,ISLIP,IFREQ)) THEN
                    JUMPAC=JUMPAC+JUMP(3,ISLIP,IFREQ)
C
C SET UP AMBIGUITY
                    IF (IEPO.EQ.JUMP(1,ISLIP,IFREQ)) THEN
                      IAMB=IAMB+1
C
C CHECK MAXIMUM NUMBER OF AMBIGUITIES
C -----------------------------------
                      CALL MAXTST(0,'SMWTPH',MXNAMB,MAXAMB,MXCAMB,IRC)
                      IF(IRC.NE.0) CALL EXITRC(2)
C
                      AMBSAT(IAMB)=SVNEP(NSEPO)
                      AMBIEP(IAMB)=IEPO
                      AMBWLF(IAMB,1:NFREQ)=IWLF(1:NFREQ)
                      AMBCLS(IAMB,1:NFRQ)=IAMB
                      AMBIGU(IAMB,1:NFRQ)=0.D0
                    END IF
                  ENDIF
55              CONTINUE
                OBSERV(NSEPO,IFREQ)=-PRANG(IEPO,IFREQ,ISAT)+NOISE
     1                             +JUMPAC*WLGT(IFREQ,SVNEP(NSEPO))
              ELSE
                OBSERV(NSEPO,IFREQ)=0.D0
              ENDIF
              OBSFLG(NSEPO,IFREQ)=CHAR(0)
60          CONTINUE
          ENDIF
70      CONTINUE
C
        IF(NSEPO.GT.0) THEN
          IF(TFIRST.EQ.1.D20) TFIRST=OBSTIM
          TLAST=OBSTIM
          CALL WTOBSI(LFNLOC,IFRMAT,NFREQ,OBSTIM,DELTAT,EPOFLG,
     1                NSEPO,SVNEP,OBSFLG,OBSERV)
        ENDIF
C
        SVNEPA(:)=SVNEP(:)
        NSEPA=NSEPO
C
        IF(JSTART.EQ.1) JEPO=JEPO+1
100   CONTINUE
C
C CLOSE OBSERVATION FILE
C ----------------------
      CLOSE(UNIT=LFNLOC)
C
C NUMBER OF AMBIGUITIES
C ---------------------
      NUMAMB=IAMB
C
C FIRST OBSERVATION EPOCH AND NUMBER OF EPOCHS
C --------------------------------------------
      TIMREF=TFIRST
      NEPOCH=IDNINT(86400.D0*(TLAST-TIMREF)/IDELTT)+1
ccc      NEPOCH=IDINT(86400.D0*(TLAST-TIMREF)/IDELTT)+1
C
C REMOVE SATELLITES WITH NO OBSERVATIONS
C --------------------------------------
      NSATNW=0
      DO 200 ISATEL=1,NSATEL
        IF(NUMOBS(ISATEL,1).EQ.0.AND.NUMOBS(ISATEL,2).EQ.0) GOTO 200
C
        NSATNW=NSATNW+1
        SATNEW(NSATNW)=NUMSAT(ISATEL)
        DO 190 IFREQ=1,NFREQ
          NUMOBS(NSATNW,IFREQ)=NUMOBS(ISATEL,IFREQ)
          NUMMRK(NSATNW,IFREQ)=NUMMRK(ISATEL,IFREQ)
190     CONTINUE
200   CONTINUE
C
C WRITE PHASE HEADER
C -----------------
      USEGEOS=0
      GOBSDEF%NOREC=0
      hlp_staNam(1)    =STANAM; hlp_staNam(2) =''
      hlp_recTyp(1)    =recTyp; hlp_recTyp(2) =''
      hlp_antTyp(1)    =antTyp; hlp_antTyp(2) =''
      hlp_irUnit(1)    =irUnit; hlp_irUnit(2) = 0
      hlp_iAnten(1)    =iAnten; hlp_iAnten(2) = 0
      hlp_oprNam(1)    =oprNam; hlp_oprNam(2) =''
      hlp_posecc(1:3,1)=posecc; hlp_posecc(1:3,2)=0d0
      hlp_iClock(1)    =iClock; hlp_iClock(2) = 0
C
      CALL WTHEAD(FILPHA(1),
     1            MEATYP,NDIFF,NFREQ,NEPOCH,NSATNW,
     2            CSESS,IDELTT,TIMREF,CAMPGN,TITLE,CRDATE,
     3            CRTIME,IRMARK,NEPFLG,IFRMAT,hlp_STANAM,
     4            hlp_RECTYP,hlp_ANTTYP,hlp_IRUNIT,hlp_IANTEN,
     5            hlp_OPRNAM,hlp_POSECC,hlp_ICLOCK,SATNEW,NUMOBS,NUMMRK,
     6            NUMAMB,AMBSAT,AMBIEP,AMBWLF,AMBIGU,AMBCLS,
     7            USEGEOS,GOBSDEF)
C
      RETURN
      END SUBROUTINE

      END MODULE
