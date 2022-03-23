      MODULE s_CYCMRK
      CONTAINS

C*
      SUBROUTINE CYCMRK(NSAT,NFREQ,SVN,IEP,NSLIP,LSTSLP,SLPLST,
     1                  NDEL,LSTDEL,OBSERV,OBSFLG)
CC
CC NAME       :  CYCMRK
CC
CC PURPOSE    :  APPLY CYCLE SLIPS, MARK OBSERVATIONS FOR ALL
CC               SATELLITES PERTAINING TO EPOCH IEP
CC
CC PARAMETERS :
CC         IN :  NSAT   : NUMBER OF SATELLITES CONSIDERED     I*4
CC               NFREQ  : NUMBER OF FREQUENCIES IN FILE       I*4
CC               SVN(I),I=1,..,NSAT: SV-NUMBERS               I*4
CC               NSLIP  : NUMBER OF SLIPS                     I*4
CC               LSTSLP(K,I),K=1,..,6, I=1,2,..,NSLIP         I*4
CC                        SLIP DESCRIPTION:
CC                     (1,I): EPOCH NUMBER
CC                     (2,I): SV NUMBER
CC                     (3,I): FREQUENCY NUMBER
CC                     (4,I): WAVELENGTH FACTOR
CC                     (5,I): WAVELENGTH FACTOR OF L5
CC                            IF AN L1 AND L2 SLIP OCCURS AT
CC                            THE SAME EPOCH FOR THE SAME
CC                            SATELLITE:
CC                              (5,I)=3 FOR L1, INDICATING THAT
CC                                      AN L2 SLIP OCCURED TOO
CC                              (5,I)=WAVELENGTH OF L5 FOR L2
CC                     (6,I): DETECTED BY
CC                            =1: SINGLE FREQ. SLIP DETECTION
CC                            =2: DUAL   FREQ. SLIP DETECTION
CC                            =3: CLOCK
CC                            =4: USER
CC                            =5: MILLI-SEC JUMP
CC                            ALL CYCLE SLIPS DETECTED IN THE
CC                            LATEST RUN HAVE A NEGATIVE SIGN
CC               SLPLST(I),I=1,2,..,NSLIP: SLIPS              R*8
CC               NDEL   : NUMBER OF DELETIONS                 I*4
CC               LSTDEL(K,I),K=1,..,5, I=1,2,..,NDEL          I*4
CC                        DEFINITION OF MARK REQUEST NUMBER I
CC                        (1,I): SV-NUMBER
CC                        (2,I): FIRST EPOCH OF MARKED AREA I
CC                        (3,I): LAST  EPOCH OF MARKED AREA I
CC                        (4,I): FREQUENCY (1=L1, 2=L2)
CC                        (5,I): MARKED BY
CC                               =1: SINGLE FREQ. REJECTION
CC                               =2: DUAL   FREQ. REJECTION
CC                               =3: UNPAIRED L1/L2 OBSERVATIONS
CC                               =4: USER
CC                               =5: SMALL ELEVATION
CC                               =6: SMALL PIECES
CC                               =7: BAD OBSERVED-COMPUTED
CC                               =8: GPS SATELLITE CLOCK IS MISSING
CC                               ALL AREAS MARKED OR CHANGED IN THE
CC                               LATEST RUN HAVE A NEGATIVE SIGN
CC     IN/OUT :  OBSERV(I,L),I=1,2,..,MXCSAT,L=1,2:           I*4
CC                        OBSERVATIONS. MXCSAT TRANSFERRED VIA
CC                        COMMON MCMSAT
CC               OBSFLG(I,L),I=1,2,..,MXCSAT,L=1,2:           I*4
CC                        OBS.-FLAGS. MXCSAT TRANSFERRED VIA
CC                        COMMON MCMSAT
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  88/05/24 14:48
CC
CC CHANGES    :  27-MAR-92 : ??: NO CYCLE SLIP FLAG SET ANY MORE
CC               16-JUN-93 : ??: ADD OPTION "ALL SATELLITES"=99
CC               24-APR-95 : ??: UPDATE DESCRIPTION FOR LSTDEL
CC               28-JUL-98 : HH: MODIFICATIONS GLONASS
CC               28-OCT-99 : TS: CORRECT HANDLING OF CLOCK JUMPS FOR GLONASS
CC               10-JAN-02 : DS: UPDATE DESCRIPTION OF LSTDEL(5,I)="8"
CC               01-AUG-02 : RD: HANDLE MILLI-SEC JUMPS FOR ZD PHASE
CC               30-AUG-03 : HU: SHARED DO LABELS REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE s_setflg
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IDEL  , IEP   , ISAT  , ISLIP , L     , MXCSAT, NDEL  ,
     1          NFREQ , NSAT  , NSLIP
C
      REAL*8    DJUMP , SLPTOT
C
CCC       IMPLICIT   REAL*8 (A-H,O-Z)
CCC       IMPLICIT   INTEGER*4 (I-N)
C
      INTEGER*4   SVN(*),LSTSLP(6,*),LSTDEL(5,*)
      REAL*8      SLPLST(*),OBSERV(MXCSAT,*)
      CHARACTER*1 OBSFLG(MXCSAT,*)
      CHARACTER*6 MXNSAT
C
      INCLUDE 'COMFREQ.inc'
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
C
C APPLY CYCLE SLIPS, MARK OBSERVATIONS
C ------------------------------------
      DO 30 ISAT=1,NSAT
        DO L=1,NFREQ
          SLPTOT=0.D0
          DJUMP=0.D0
C
C APPLY SLIPS
          DO 10 ISLIP=1,NSLIP
            IF (IABS(LSTSLP(6,ISLIP)).NE.3 .AND.
     1          IABS(LSTSLP(6,ISLIP)).NE.5) THEN                        GLONASS
              IF(IEP      .GE.LSTSLP(1,ISLIP)    .AND.
     1           (SVN(ISAT).EQ.LSTSLP(2,ISLIP).OR.
     2            99       .EQ.LSTSLP(2,ISLIP))  .AND.
     3           L        .EQ.LSTSLP(3,ISLIP).    AND.
     4           OBSERV(ISAT,L).NE.0.D0) THEN
                   SLPTOT=SLPTOT+SLPLST(ISLIP)/LSTSLP(4,ISLIP)
              END IF                                                    GLONASS
            ELSE                                                        GLONASS
              IF(IEP      .GE.LSTSLP(1,ISLIP)    .AND.
     1           (SVN(ISAT).EQ.LSTSLP(2,ISLIP).OR.
     2            99       .EQ.LSTSLP(2,ISLIP))  .AND.
     3           L        .EQ.LSTSLP(3,ISLIP).    AND.
     4           OBSERV(ISAT,L).NE.0.D0) THEN
                    DJUMP=DJUMP+SLPLST(ISLIP)/LSTSLP(4,ISLIP)           GLONASS
              END IF                                                    GLONASS
            ENDIF                                                       GLONASS
10        CONTINUE                                                      GLONASS
          OBSERV(ISAT,L)=OBSERV(ISAT,L)
     1                   +SLPTOT*WLGT(L,SVN(ISAT))
     2                   +DJUMP*WLGT(L,SVN(1))
C
C MARK OBSERVATIONS
          DO 20 IDEL=1,NDEL
            IF(IEP.GE.LSTDEL(2,IDEL)           .AND.
     1         IEP.LE.LSTDEL(3,IDEL)           .AND.
     2         (SVN(ISAT).EQ.LSTDEL(1,IDEL).OR.
     3          99       .EQ.LSTDEL(1,IDEL))   .AND.
     3         L.EQ.LSTDEL(4,IDEL)) THEN
                 CALL SETFLG(OBSFLG(ISAT,L),0)
            END IF
20        CONTINUE
        ENDDO
30    CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
