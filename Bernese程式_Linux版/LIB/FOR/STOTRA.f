      MODULE s_STOTRA
      CONTAINS

C*
      SUBROUTINE STOTRA(IDIR,NPAR,LOCQ,NSTCEP,NSTCEF,NSTCEF2,
     1                  NSASTC1,NSASTC2,TIMSTC,TIMSTC2,NUMSTC1,NUMSTC2,
     2                  NSTOCH,STOTYP,SVNSTO,TSTOCH,IFIL,STOBND,NSTCEP2)
CC
CC NAME       :  STOTRA
CC
CC PURPOSE    :  TRANSFORM LOCQ FOR STOCHASTIC PARAMETERS FROM
CC               OLD LOCQ DESCRIPTION TO NEW ONE
CC
CC               OLD:
CC               LOCQ(1,IPAR)=11
CC               LOCQ(2,IPAR)=IARC
CC               LOCQ(3,IPAR)=SATELLITE NUMBER
CC               LOCQ(4,IPAR)=1,NSTCEF(ISTC,IA):#STOC. EVENTS
CC               LOCQ(5,IPAR)=FRCTYP(IPE)
CC               LOCQ(6,IPAR)=IPE
CC               LOCQ(7,IPAR)=SATELLITE NUMBER IN ARRAY
CC                            NUMSTC1 OR NUMSTC2
CC               NEW:
CC               LOCQ(1,IPAR)=11
CC               LOCQ(2,IPAR)=IARC
CC               LOCQ(3,IPAR)=SATELLITE NUMBER
CC           --> LOCQ(4,IPAR)=1,NSTOCH
CC               LOCQ(5,IPAR)=FRCTYP(IPE)
CC           --> LOCQ(6,IPAR)=1
CC           --> LOCQ(7,IPAR)=
CC
CC PARAMETERS :
CC         IN :  NPAR   : NUMBER OF PARAMETERS IN LOCQ        I*4
CC               LOCQ(I,K),K=1,...,NPAR,I=1,..,MAXLCQ:        I*4
CC                        DEFINITION OF EACH PARAMETER
CC               NSTCEP : NUMBER OF STOCHASTIC FORCES         I*4
CC                        PER EPOCH
CC               NSTCEF : NUMBER OF STOCHASTIC EPOCHS/ARC     I*4(*)
CC                        FOR GPS/GLONASS
CC               NSTCEF2: NUMBER OF STOCHASTIC EPOCHS/ARC     I*4(*)
CC                        FOR LEOs
CC               NSASTC1: NUMBER OF SATS WITH STOCH ORBITS    I*4
CC                        GPS/GLONASS ONLY
CC               NSASTC2: NUMBER OF LEOs WITH STOCH ORBITS    I*4
CC               NUMSTC1: CORRESPONDING SAT NUMBERS           I*4(*)
CC               NUMSTC2: CORRESPONDING LEO NUMBERS           I*4(*)
CC               TIMSTC : STOCHASTIC EPOCHS (=INT BOUNDARIES) R*8(*,*,*,*)
CC                        FOR GPS/GLONASS
CC               TIMSTC2: STOCHASTIC EPOCHS (=INT BOUNDARIES) R*8(*,*,*,*)
CC                        FOR LEOs
CC               SIGSTC : A PRIORI SIGMAS FOR STOCH REQUESTS  R*8(*,*)
CC               NSTOCH : NUMBER OF STOCHASTIC PARAMETERS     I*4
CC               STOTYP : TYPE OF STOCHASTIC PULSE            I*4(*)
CC               SVNSTO : SAT. NUMBER FOR STOCH PULSE I       I*4(*)
CC               TSTOCH : EPOCHS FOR STOCHASTIC PULSES        R*8(*)
CC               IFIL   : ACTUAL FILE NUMBER                  I*4
CC               STOBND : INDEX WHETHER PULSE I WAS SET UP AT I*4(*)
CC                        THE ARC BOUNDARY (STOBND(I)=IFIL) OR
CC                        INTERNALLY (STOBND(I)=0)
CC               NSTCEP2: NUMBER OF STOCHASTIC FORCES         I*4
CC                        PER EPOCH (LEO)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  E.BROCKMANN
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  05-AUG-94
CC
CC CHANGES    :  21-FEB-95 : EB: IDIR=1,MORE THAN 1 STOCH EPOCH IN
CC                               NEQ
CC               17-MAR-95 : EB: NEW LOGIC FOR IDIR=-1
CC               26-JUN-95 : EB: NEW LOGIC FOR IDIR=-1 (ADD STOCH IN TIMSTC)
CC               11-JUN-01 : DS: LEO STOCH. ORBIT
CC               23-JUN-02 : DS: REMOVE SIGSTC FROM PARAMETER LIST
CC               23-JUN-02 : DS: SEPARATE LEO & GPS NUMBER OF STOCH. FORCES
CC                               PER EPOCH
CC               23-NOV-04 : AJ: ADDITIONAL COMPONENT FOR TIMSTC
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               16-JAN-08 : HB: TIMSTC AND TIMSTC2 WITHOUT FIXED BOUNDARIES
CC               24-JUN-08 : DT: ASSUMING SVN>=951 FOR SLR
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1994     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I123   , IARC   , IDIR   , IFIL   , IPAR   , IPULSE ,
     1          IRSW   , ISAT   , IST    , ISTO   , ISTOCH1, ISTOCH2,
     2          MXCLCQ , MXCSAT , MXCSTC , NPAR   , NSASTC1, NSASTC2,
     3          NSTCEP , NSTCEP2, NSTOCH
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*6   MXNSAT,MXNSTC,MXNLCQ
      REAL*8        TIMSTC(:,:,:,:),TIMSTC2(:,:,:,:)
      REAL*8        TSTOCH(*)
      INTEGER*4     LOCQ(MXCLCQ,*),STOTYP(*),SVNSTO(*)
      INTEGER*4     NUMSTC1(*),NUMSTC2(*),STOBND(*)
      INTEGER*4     NSTCEF(MXCSAT,*),NSTCEF2(MXCSAT,*)
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMSTC/MXCSTC,MXNSTC
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
C
      IF (IDIR.EQ.2) NSTOCH=0
      IF (IDIR.GE.1) THEN
        IARC=1
C
C   LOOP OVER ALL SATELLITES WITH STOCH. ORBITS
C ---------------------------------------------
C
C   1) GPS/GLONASS
C ----------------
        DO 820 ISAT=1,NSASTC1
          DO 817 ISTO=1,NSTCEF(ISAT,IARC)
            DO 815 IPULSE=1,NSTCEP
              DO 819 IPAR=1,NPAR
                IF(LOCQ(1,IPAR).EQ.11.AND.LOCQ(3,IPAR).EQ.NUMSTC1(ISAT)
     1            .AND.LOCQ(6,IPAR).EQ.IPULSE.AND.LOCQ(7,IPAR).GT.0)
     2            THEN
                  IF (IDIR.EQ.2.AND.ISTO.NE.LOCQ(4,IPAR)) GOTO 819
                  IRSW=LOCQ(5,IPAR)
                  GOTO 120
                ENDIF
819           CONTINUE
C
CC              WRITE(LFNERR,900)NUMSTC1(ISAT)
CC900           FORMAT(' *** SR STOTRA: WARNING: STOCHASTIC SATELLITE #: '
CC     1                ,I3,' NOT FOUND IN NEQ')
              GOTO 815
C
120           NSTOCH=NSTOCH+1
C
CC              WRITE(LFNERR,*)'STOTRA+1',(LOCQ(K,IPAR),K=1,7)
              LOCQ(4,IPAR)=NSTOCH
              SVNSTO(NSTOCH)=NUMSTC1(ISAT)
              STOTYP(NSTOCH)=IRSW
              TSTOCH(NSTOCH)=TIMSTC(1,ISTO,ISAT,IARC)
              STOBND(NSTOCH)=-1
C
CC              WRITE(LFNERR,*)'NSTOCH',NSTOCH,SVNSTO(NSTOCH),
CC     1                       TSTOCH(NSTOCH)
C
              LOCQ(4,IPAR)=NSTOCH
              LOCQ(6,IPAR)=1
              LOCQ(7,IPAR)=STOBND(NSTOCH)
C
815         CONTINUE
817       CONTINUE
820     CONTINUE
C
C   2) LEO
C --------
        DO 1820 ISAT=1,NSASTC2
          DO 1817 ISTO=1,NSTCEF2(ISAT,IARC)
            DO 1815 IPULSE=1,NSTCEP2
              DO 1819 IPAR=1,NPAR
                IF(LOCQ(1,IPAR).EQ.11.AND.LOCQ(3,IPAR).EQ.NUMSTC2(ISAT)
     1            .AND.LOCQ(6,IPAR).EQ.IPULSE.AND.LOCQ(7,IPAR).GT.0)
     2            THEN
                  IF (IDIR.EQ.2.AND.ISTO.NE.LOCQ(4,IPAR)) GOTO 1819
                  IRSW=LOCQ(5,IPAR)
                  GOTO 1120
                ENDIF
1819           CONTINUE
C
CC              WRITE(LFNERR,900)NUMSTC2(ISAT)
CC900           FORMAT(' *** SR STOTRA: WARNING: STOCHASTIC SATELLITE #: '
CC     1                ,I3,' NOT FOUND IN NEQ')
              GOTO 1815
C
1120           NSTOCH=NSTOCH+1
C
CC              WRITE(LFNERR,*)'STOTRA+1',(LOCQ(K,IPAR),K=1,7)
              LOCQ(4,IPAR)=NSTOCH
              SVNSTO(NSTOCH)=NUMSTC2(ISAT)
              STOTYP(NSTOCH)=IRSW
              TSTOCH(NSTOCH)=TIMSTC2(1,ISTO,ISAT,IARC)
              STOBND(NSTOCH)=-1
C
CC              WRITE(LFNERR,*)'NSTOCH',NSTOCH,SVNSTO(NSTOCH),
CC     1                       TSTOCH(NSTOCH)
C
              LOCQ(4,IPAR)=NSTOCH
              LOCQ(6,IPAR)=1
              LOCQ(7,IPAR)=STOBND(NSTOCH)
C
1815         CONTINUE
1817       CONTINUE
1820     CONTINUE

      ENDIF
      IF (IDIR.EQ.-1) THEN
        IARC=1
        DO IPAR=1,NPAR
          IF (LOCQ(1,IPAR).EQ.11) THEN
            IST=LOCQ(4,IPAR)
C
CC            WRITE(LFNERR,*)'STOTRA-1-BEFOR',(LOCQ(K,IPAR),K=1,7)
C
C GPS/GLONASS
C -----------
            DO ISAT=1,NSASTC1
              ISTOCH1=0
              IF (NUMSTC1(ISAT).EQ.SVNSTO(IST)) THEN
                DO ISTO=1,MXCSTC
                  IF (TIMSTC(1,ISTO,ISAT,IARC).NE.0.D0) THEN
                    IF (TSTOCH(IST).EQ.TIMSTC(1,ISTO,ISAT,IARC))THEN
                      ISTOCH1=ISTO
                      GOTO 250
                    ENDIF
                  ENDIF
                ENDDO
                GOTO 250
              ENDIF
            ENDDO
250         CONTINUE
C
C LEO
C ---
            DO ISAT=1,NSASTC2
              ISTOCH2=0
              IF (NUMSTC2(ISAT).EQ.SVNSTO(IST)) THEN
                DO ISTO=1,MXCSTC
                  IF (TIMSTC2(1,ISTO,ISAT,IARC).NE.0.D0) THEN
                    IF (TSTOCH(IST).EQ.TIMSTC2(1,ISTO,ISAT,IARC))THEN
                      ISTOCH2=ISTO
                      GOTO 1250
                    ENDIF
                  ENDIF
                ENDDO
                GOTO 1250
              ENDIF
            ENDDO
1250         CONTINUE


C
            I123=0
            IF (TSTOCH(IST-1).EQ.TSTOCH(IST).AND.
     1          TSTOCH(IST-2).EQ.TSTOCH(IST).AND.
     2          SVNSTO(IST-1).EQ.SVNSTO(IST).AND.
     3          SVNSTO(IST-2).EQ.SVNSTO(IST)) THEN
                I123=3
            ELSE IF (TSTOCH(IST-1).EQ.TSTOCH(IST).AND.
     1          TSTOCH(IST+1).EQ.TSTOCH(IST).AND.
     2          SVNSTO(IST-1).EQ.SVNSTO(IST).AND.
     3          SVNSTO(IST+1).EQ.SVNSTO(IST)) THEN
                I123=2
            ELSE IF (TSTOCH(IST+1).EQ.TSTOCH(IST).AND.
     1          TSTOCH(IST+2).EQ.TSTOCH(IST).AND.
     2          SVNSTO(IST+1).EQ.SVNSTO(IST).AND.
     3          SVNSTO(IST+2).EQ.SVNSTO(IST)) THEN
                I123=1
            ENDIF
            IF (ISTOCH1.EQ.0.OR.ISTOCH2.EQ.0) WRITE(LFNERR,900)
900         FORMAT(' *** SR STOTRA: WARNING: STOCHASTIC',
     1                ' PULSE NR NOT FOUND',/)
            IF (I123.EQ.0)WRITE(LFNERR,901)
901         FORMAT(' *** SR STOTRA: WARNING: STOCHASTIC',
     1                ' PULSE COMPONENT NOT FOUND',/)
            IF (LOCQ(4,IPAR).LT.900 .OR. LOCQ(4,IPAR).GE.951) THEN
               LOCQ(4,IPAR)=ISTOCH1
            ELSE
               LOCQ(4,IPAR)=ISTOCH2
            END IF
            LOCQ(6,IPAR)=I123
            DO ISAT=1,NSASTC1
              IF(LOCQ(3,IPAR).EQ.NUMSTC1(ISAT)) LOCQ(7,IPAR)=ISAT
            ENDDO
            DO ISAT=1,NSASTC2
              IF(LOCQ(3,IPAR).EQ.NUMSTC2(ISAT)) LOCQ(7,IPAR)=ISAT
            ENDDO
CC            WRITE(LFNERR,*)'STOTRA-1-AFTER',(LOCQ(K,IPAR),K=1,7),
CC     1            TSTOCH(IST),IST
          ENDIF
        ENDDO
      ENDIF
C
999   CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
