      MODULE s_NXTEPO
      CONTAINS

C*
      SUBROUTINE NXTEPO(LFNOBS,NSAT  ,SVN   ,PAR   ,NFRAUX,FRQAUX,
     1                  NSTART,NSLIP ,LSTSLP,SLPLST,IAMNEW,IDELTT,
     2                  IPRNT2,IEPOCH,NSATEP,SVNEP ,DELTAT,INDSAT,
     3                  ABSACT,NDEL  ,LSTDEL,NNEWAM,LSTAMB,ABSLST,
     4                  TIMLST,FILFLG,AMSFLG,AOBS  ,NFREQ)
CC
CC NAME       :  NXTEPO
CC
CC PURPOSE    :  COMPUTE RESIDUALS FOR ALL SINGLE DIFFERENCE
CC               OBSERVATIONS AND ALL FREQUENCIES.
CC               DEFINE CLOCK INCREMENT WITH RESPECT TO THE
CC               PRECEEDING EPOCH
CC
CC PARAMETERS :
CC         IN :  LFNOBS : LOGICAL FILE NUMBER FOR OBS EQNS    I*4
CC               NSAT   : NUMBER OF SATELLITES                I*4
CC               SVN(I),I=1,..,NSAT: SV NUMBERS               I*4
CC               PAR(I),I=1,2,3: TRIPLE DIFF. SOLUTION        R*8
CC               NFRAUX : NUMBER OF FREQUENCIES ON AUX. FILE  I*4
CC               FRQAUX(I),I=1,2,..,NFRAUX: FREQUENCIES       I*4
CC               NSTART : STARTING NUMBER FOR CYCLE SLIPS     I*4
CC                        TO BE APPLIED
CC               NSLIP  : TOTAL NUMBER OF SLIPS               I*4
CC               LSTSLP(K,I),K=1,..,6, I=1,2,...,NSLIP: SLIP  I*4
CC                        DESCRIPTION
CC               SLPLST(I),I=1,2,..,NSLIP: SLIPS              I*4
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
CC                               =6: SMALL PIECE
CC                               =7: BAD OBSERVED-COMPUTED
CC                               ALL AREAS MARKED OR CHANGED IN THE
CC                               LATEST RUN HAVE A NEGATIVE SIGN
CC               IAMNEW(I),I=1,2,3: SETTING OF NEW AMBIGUITIES I*4
CC                        I=1 : USE CYCLE SLIP FLAG (0/1)
CC                        I=2 : IF PROBLEM IN SLIP-FIXING (0/1)
CC                        I=3 : AFTER GAP LARGER THAN (SEC)
CC                        I=4 : USE AMBIGUITIES FROM FILE (0/1)
CC                        I=5 : MIN. TIME OF OBS. PER AMB. (SEC)
CC               IDELTT : OBSERVATION INTERVAL (SEC)           I*4
CC               IPRNT2 : PRINT LEVEL FOR CYCLE SLIP DETECT.   I*4
CC                        =0: NO MESSAGES PRINTED
CC                        =1: PRINT SUMMARY
CC                        =2: ALL MESSAGES PRINTED
CC               NFREQ  : NUMBER OF FREQUENCIES IN FILE        I*4
CC        OUT :  IEPOCH : EPOCH NUMBER                         I*4
CC               NSATEP : NUMBER OF SATELLITES OF EPOCH        I*4
CC               SVNEP(I),I=1,2,..,NSATEP: CORRESPONDING       I*4
CC                        SV-NUMBERS
CC               DELTAT(I,J): RCV CLOCK RECORD FROM OBS FILES  R*8(2,2)
CC                        I=1 LAST EPOCH, I=2 ACTUAL EPOCH
CC                        J=1,2 CLOCK RECORD (1: RINEX, 2: CODSPP)
CC               INDSAT(I),I=1,2,..,NSATEP: INDEX IN SVN       I*4
CC               ABSACT(K,ISAT),K=1,2,..,NFRAUX,ISAT+1,..,NSAT R*8
CC                        SINGLE DIFFERENCE RESIDUALS
CC               AOBS(1:3,ISAT) SINGLE DIFFERENCE PARTIALS     R*8
CC    IN/OUT :   NNEWAM(ISATEL) : NUMBER OF AMBIGUITIES        I*4
CC               LSTAMB(I,ISATEL,IAMB)  LIST OF AMBIGUITIES    I*4
CC                        I=1 : THE FIRST EPOCH
CC                        I=2 : TYPE: 1 ... FILE
CC                                    2 ... CYCLE SLIP
CC                                    3 ... USER
CC                                    4 ... GAP
CC                                    5 ... PREPROCESSING PROBLEM
CC                                    6 ... CLOCK EVENT
CC                        I=3 : THE LAST EPOCH WITH OBERVATIONS
CC               ABSLST(K,ISAT),K=1,2,3, ISAT=1,2,..,NSAT      R*8
CC                        TERMS "OBS-COMPUTED" FROM PREVIOUS
CC                        EPOCHS
CC               TIMLST(K,ISAT),K=1,2,3, ISAT=1,2,..,NSAT:     I*4
CC                        LAST OBS. EPOCH
CC               FILFLG(IFRQ,ISAT),ISAT=1,2,..,NSAT: FLAGS    CH*1
CC                        FOR SATELLITES
CC                        =O: O.K.
CC                        =C: CYCLE SLIP FLAG IN FILE
CC                        =A: NEW AMBIGUITY
CC                        =M: MOVE THE FLAG INTO THE NEXT EPOCH
CC               AMSFLG : INDICATER OF MAXAMS EXCEEDINGS      CH*1
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER, L.MERVART
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  88/05/02 12:25
CC
CC CHANGES    :  05-JUN-92 : ??: CHANGES FOR THE NEW MAUPRP VERSION
CC               16-JUN-93 : ??: ADD OPTION "ALL SATELLITES"=99
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               10-AUG-94 : MR: CALL EXITRC
CC               24-APR-95 : MR: ADD MARKING TYPE "O-C"
CC               05-MAR-96 : TS: HANDLING OF "MAXAMS" EXCEEDINGS
CC               23-SEP-97 : DI: USE MAXSAT.inc
CC               28-JUL-98 : HH: MODIFICATIONS FOR GLONASS
CC               28-OCT-99 : TS: CORRECT HANDLING OF CLOCK JUMPS FOR GLONASS
CC               03-JAN-02 : DS: ADD AOBS TO THE OUTPUT
CC               01-AUG-02 : RD: HANDLE MILLI-SEC JUMPS FOR ZD PHASE
CC               06-DEC-02 : RD: UPDATE OF LSTAMB
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               10-DEC-03 : RD: MODIFY CONDITION TO UPDATE OF LSTAMB
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_maxdim, ONLY: MAXSAT
      USE s_setflg
      USE s_updamb
      USE s_exitrc
      USE f_tstflg
      USE s_mrkobs
      USE s_maxtst
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IAMB  , IAMFLG, IDEL  , IDELTT, IEPO  , IEPOCH,
     1          IFIRST, IFREQ , IFRQ  , INTEPO, IPRNT2, IRC   , ISAT  ,
     2          ISATEP, ISEP  , ISL   , K     , L     , LFNOBS, LSTEPO,
     3          MXCSAT, NDEL  , NFRAUX, NFREQ , NSAT  , NSATEP, NSLIP ,
     4          NSTART
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
C
      INTEGER*4   SVN(*),SVNEP(*),INDSAT(*),FRQAUX(*),LSTSLP(6,*)
      INTEGER*4   LSTDEL(5,*),TIMLST(3,*),NSAEFF(3),ISAEFF(3)
      INTEGER*4   LSTAMB(3,MXCSAT,*),NNEWAM(MXCSAT),IAMNEW(5)
C
      REAL*8      ABSACT(3,*),ABSEPO(3),SLPLST(*)
      REAL*8      AOBS(3,*),PAR(3),ABSLST(3,*)
      REAL*8      SLPTOT(2),JUMP(2)
      REAL*8      DELTAT(2,2),DELTT0(2)
C
      CHARACTER*1 OBSFLG(3),FILFLG(3,*),AMSFLG
      CHARACTER*6 MXNSAT
C
      INCLUDE 'COMFREQ.inc'
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
C
      DATA IFIRST/1/
C
C CHECK LOCAL DIMENSION
C ---------------------
      IF(IFIRST.EQ.1) THEN
        IFIRST=0
        CALL MAXTST(1,'NXTEPO',MXNSAT,MAXSAT,MXCSAT,IRC)
        IF(IRC.NE.0) CALL EXITRC(2)
      ENDIF
C
C READ OBSERVATIONS FOR CURRENT EPOCH
C -----------------------------------
1     READ(LFNOBS)IEPOCH,NSATEP,(SVNEP(K),K=1,NSATEP),
     1            (DELTT0(I),I=1,2)
      DO 3 ISATEP=1,NSATEP
        DO 2 K=1,NSAT
          IF(SVNEP(ISATEP).EQ.SVN(K))THEN
            INDSAT(ISATEP)=K
            GO TO 3
          END IF
2       CONTINUE
3     CONTINUE
C
C SAVE THE CLOCK OF THE LAST EPOCH
C --------------------------------
      DELTAT(1,1:2)=DELTAT(2,1:2)
      DELTAT(2,1:2)=DELTT0(1:2)
C
C INITIALIZE ACTUAL NUMBER OF UNMARKED SATELLITES
      DO 6 IFRQ=1,NFRAUX
        NSAEFF(IFRQ)=0
6     CONTINUE
C
C LOOP OVER ALL SATELLITES
      DO 10 ISAT=1,NSAT
C
C SET ABSACT
        DO 11 IFRQ=1,NFRAUX
          ABSACT(IFRQ,ISAT)=1.D20
11      CONTINUE
C
C SET ABSLST IF A NEW AMBIGUITY IS SET
        IAMFLG = 0
        DO 12 IAMB=1,NNEWAM(ISAT)
          DO 13 IFRQ=1,NFRAUX
            IF ((LSTAMB(1,ISAT,IAMB) .GT. TIMLST(IFRQ,ISAT)) .AND.
     1           (LSTAMB(1,ISAT,IAMB) .LE. IEPOCH)) IAMFLG = 1
13        CONTINUE
12      CONTINUE
        IF (IAMFLG .EQ. 1) THEN
          DO 14 IFRQ=1,NFRAUX
            ABSLST(IFRQ,ISAT)=1.D20
            FILFLG(IFRQ,ISAT)='A'
14        CONTINUE
        END IF
C
10    CONTINUE
C
C COMPUTE RESIDUALS, READ CYCLE SLIP FLAGS
C ----------------------------------------
      DO 30 ISEP=1,NSATEP
        ISAT=INDSAT(ISEP)
        READ(LFNOBS)(AOBS(K,ISAT),K=1,3),
     1              (ABSEPO(L),OBSFLG(L),L=1,NFRAUX)
        DO 20 IFRQ=1,NFRAUX
          IFREQ=FRQAUX(IFRQ)
          IF (ABSEPO(IFRQ) .NE. 0.D0) THEN
            IF ((FILFLG(IFRQ,ISAT) .NE. 'A') .AND.
     1          ((FILFLG(IFRQ,ISAT) .EQ. 'M') .OR.
     2           (TSTFLG(OBSFLG(IFRQ),1)))) THEN
              FILFLG(IFRQ,ISAT) = 'C'
            ELSE
              FILFLG(IFRQ,ISAT) = 'O'
            END IF
          END IF
C
C SET FLAGS (PIECES WITH SMALL NUMBER OF OBSERVATIONS OR BAD O-C)
          DO 25 IDEL=1,NDEL
            IF (IABS(LSTDEL(5,IDEL)) .EQ. 6  .OR.
     1          IABS(LSTDEL(5,IDEL)) .EQ. 7) THEN
              IF ((LSTDEL(1,IDEL) .EQ. SVNEP(ISEP)) .AND.
     1            (LSTDEL(2,IDEL) .LE. IEPOCH)      .AND.
     2            (LSTDEL(3,IDEL) .GE. IEPOCH)      .AND.
     3            ((LSTDEL(4,IDEL) .EQ. IFREQ) .OR.
     4             (IFREQ .GT. 2))) THEN
                 CALL SETFLG(OBSFLG(IFRQ),0)
              END IF
            END IF
25        CONTINUE
C
          IF(ABSEPO(IFRQ).NE.0.D0.AND.
     1       .NOT.TSTFLG(OBSFLG(IFRQ),0)) THEN
            NSAEFF(IFRQ)=NSAEFF(IFRQ)+1
            ISAEFF(IFRQ)=ISAT
            ABSACT(IFRQ,ISAT)=-ABSEPO(IFRQ)
            IF(IFREQ.NE.4)THEN
              DO 15 K=1,NFRAUX
                ABSACT(IFRQ,ISAT)=ABSACT(IFRQ,ISAT)+AOBS(K,ISAT)*PAR(K)
15            CONTINUE
            END IF
C
C APPLY CYCLE SLIPS DETECTED IN CURRENT ITERATION
C -----------------------------------------------
            IF (IFRQ.LE.2) THEN
              SLPTOT(IFRQ)=0.D0
              JUMP(IFRQ)=0.D0                                           GLONASS
              DO 17 ISL=NSTART,NSLIP                                    GLONASS
               IF (IABS(LSTSLP(6,ISL)).NE.3 .AND.                       GLONASS
     1             IABS(LSTSLP(6,ISL)).NE.5) THEN                       GLONASS
                IF((SVNEP(ISEP).EQ.LSTSLP(2,ISL) .OR.
     1              99         .EQ.LSTSLP(2,ISL))    .AND.
     2             IFREQ.EQ.LSTSLP(3,ISL))THEN
                   SLPTOT(IFRQ)=SLPTOT(IFRQ)+SLPLST(ISL)/LSTSLP(4,ISL)
                END IF                                                  GLONASS
               ELSE                                                     GLONASS
                IF((SVNEP(ISEP).EQ.LSTSLP(2,ISL) .OR.
     1              99         .EQ.LSTSLP(2,ISL))    .AND.
     2             IFREQ.EQ.LSTSLP(3,ISL))THEN
                    JUMP(IFRQ)=JUMP(IFRQ)+SLPLST(ISL)/LSTSLP(4,ISL)     GLONASS
                  END IF                                                GLONASS
                ENDIF                                                   GLONASS
17            CONTINUE                                                  GLONASS
              ABSACT(IFRQ,ISAT)=ABSACT(IFRQ,ISAT)
     1                          -SLPTOT(IFRQ)*WLGT(IFREQ,SVN(ISAT))     GLONASS
     2                          -JUMP(IFRQ)*WLGT(IFREQ,SVN(1))          GLONASS
            ELSE
              ABSACT(IFRQ,ISAT)=ABSACT(IFRQ,ISAT)
     1                    -(SLPTOT(1)-SLPTOT(2))*WLGT(5,SVN(ISAT))      GLONASS
     2                    -(JUMP(1)-JUMP(2))*WLGT(5,SVN(1))             GLONASS
            END IF
          ELSE
            ABSACT(IFRQ,ISAT)=1.D20
          END IF
20      CONTINUE
30    CONTINUE
C
C MARK SINGLE REMAINING SATELLITE
      DO 40 IFRQ=1,NFRAUX
        IF(NSAEFF(IFRQ).EQ.1) THEN
          ISAT=ISAEFF(IFRQ)
          ABSACT(IFRQ,ISAT)=1.D20
          IF (FRQAUX(IFRQ).LE.2)
     1      CALL MRKOBS(6,SVN(ISAT),IEPOCH,FRQAUX(IFRQ),NSAT,SVN,
     2                  NDEL,LSTDEL)
        ENDIF
40    CONTINUE
C
C SET UP NEW AMBIGUITY IF BREAK
      DO 60 ISEP=1,NSATEP
        ISAT=INDSAT(ISEP)
        DO 50 IFRQ=1,NFRAUX
          IF(ABSACT(IFRQ,ISAT).NE.1.D20 .AND.
     1       ABSLST(IFRQ,ISAT).NE.1.D20) THEN
            INTEPO=(IEPOCH-TIMLST(IFRQ,ISAT))*IDELTT
            IF (INTEPO .GT. IAMNEW(3)) THEN
              IF(FRQAUX(IFRQ).LE.2)
     1          CALL UPDAMB(ISAT,TIMLST(IFRQ,ISAT)+1,IEPOCH,4,
     2                      NNEWAM,LSTAMB,NSAT,SVN,IPRNT2,AMSFLG,IRC)
              ABSLST(IFRQ,ISAT)=1.D20
            ENDIF
          ENDIF
50      CONTINUE
60    CONTINUE
C
C CHECK INTERVAL FOR LAST AMBIGUITIES, DELETE OBSERVATIONS
C AND UPDATE AMBIGUITIES ACCORDING TO IAMNEW(5)
C ---------------------------------------------------------
      DO ISAT=1,NSAT
        LSTEPO=MAX0(TIMLST(1,ISAT),TIMLST(2,ISAT))

        DO IAMB=NNEWAM(ISAT),1,-1
          IF (IEPOCH.GE.LSTAMB(1,ISAT,IAMB)) EXIT
        END DO
C
        IF ((IEPOCH-LSTEPO)*IDELTT.GT.IAMNEW(3)
     1    .AND. (LSTEPO-LSTAMB(1,ISAT,IAMB))*IDELTT
     2    .LT.IAMNEW(5)
     3    .AND. LSTEPO-LSTAMB(1,ISAT,IAMB).GE.0
C RD: These two additional conditions are needed otherwise
C     "Set up new ambiguzity if break" will not work, see above
     4    .AND. ABSLST(1,ISAT).NE.1D20
     5    .AND. ABSLST(2,ISAT).NE.1D20) THEN
C
C MARK OBSERVATIONS
C -----------------
          DO IEPO=LSTAMB(1,ISAT,IAMB),LSTEPO
            DO IFRQ=1,NFREQ
              CALL MRKOBS(6,SVN(ISAT),IEPO,IFRQ,NSAT,SVN,
     1                    NDEL,LSTDEL)
            END DO
          END DO
          CALL UPDAMB(ISAT,LSTAMB(1,ISAT,IAMB),LSTAMB(1,ISAT,IAMB),0,
     1                NNEWAM,LSTAMB,NSAT,SVN,IPRNT2,AMSFLG,IRC)
C
        END IF
      END DO
C
      RETURN
      END SUBROUTINE

      END MODULE
