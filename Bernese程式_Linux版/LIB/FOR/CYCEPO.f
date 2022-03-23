      MODULE s_CYCEPO
      CONTAINS

C*
      SUBROUTINE CYCEPO(IRESID,IREFIL,IEPOCH,
     1                  NSTEP,NSLIP,LSTSLP,SLPLST,
     2                  NSAT,SVN,NFRAUX,FRQAUX,
     3                  NNEWAM,LSTAMB,LASTFL,ABSACT,RHS)
CC
CC NAME       :  CYCEPO
CC
CC PURPOSE    :  APPLY CYCLE SLIPS FOUND IN CURRENT EPOCH
CC               TO ARRAYS ABSACT, RHS.
CC               WRITE TRIPLE DIFFERENCE RESIDUALS ON FILE
CC               (IF REQUESTED)
CC
CC PARAMETERS :
CC         IN :  IRESID : WRITE RESIDUALS ON FILE (YES=1)     I*4
CC               IREFIL : FILE NUMBER IN RESIDUAL FILE        I*4
CC               IEPOCH : EPOCH NUMBER                        I*4
CC               NSTEP  : STARTING VALUE FOR CYCLE SLIPS      I*4
CC                        IN ARRAYS LSTSLP, SLPLST
CC               NSLIP  : NUMBER OF SLIPS                     I*4
CC               LSTSLP(K,I),K=1,..,6, I=1,2,..,NSLIP: SLIP   I*4
CC                        DEFINITION
CC               SLPLST(I),I=1,2,..,NSLIP: SLIPS              R*8
CC               NSAT   : NUMBER OF SATELLITES                I*4
CC               SVN(I),I=1,2,..,NSAT: SV NUMBERS             I*4
CC               NFRAUX : NUMBER OF FREQUENCIES PROCESSED     I*4
CC               FRQAUX(I),I=1,2,..,NFRAUX: FREQUENCIES       I*4(*)
CC               NNEWAM(ISATEL) : NUMBER OF AMBIGUITIES      I(*)*4
CC               LSTAMB(I,ISATEL,IAMB) : LIST OF         I(2,*,*)*4
CC                        AMBIGUITIES
CC                        I = 1 ... FIRST EPOCH
CC                        I = 2 ... ITYPE
CC                                 =  1 ... FILE HEADER
CC                                 =  2 ... CYCLE SLIP FLAG
CC                                 =  3 ... USER
CC                                 =  4 ... GAP
CC                                 =  5 ... PREPROC. PROBLEM
CC                                 =  6 ... CLOCK EVENT
CC                        I = 3 ... LAST EPOCH WITH OBSERVATIONS
CC               LASTFL(IFRQ,ISAT) : LAST EPOCH WITH NEW      I*4
CC                        AMBIGUITY (THE ARRAY IS INITIALIZED
CC                        FOR EACH FILE IN SUBROUTINE PREPHA)
CC     IN/OUT :  ABSACT(K,I),K=1,2,3, I=1,2,..,NSAT: TERMS    R*8
CC                        "OBSERVED-COMPUTED" ON SINGLE DIFFERENCE
CC                        LEVEL
CC               RHS(K,I),K=1,2,3, I=1,2,..,NSAT: SAME AS     R*8
CC                        BUT DIFFERENCED IN TIME
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER, L.MERVART
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  88/06/20 08:20
CC
CC CHANGES    :  05-JUN-92 : ??: CHANGES FOR THE NEW MAUPRP VERSION
CC               16-JUN-93 : ??: ADD OPTION "ALL SATELLITES"=99
CC               21-JAN-97 : MR: CHECK FOR "IRFSAT=0"
CC               14-OCT-98 : HH: MODIFICATIONS FOR GLONASS
CC               01-AUG-02 : RD: HANDLE MILLI-SEC JUMPS FOR ZD PHASE
CC               06-DEC-02 : RD: UPDATE OF LSTAMB
CC               30-AUG-03 : HU: SHARED DO LABELS REMOVED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IAMB  , IEPOCH, IFREQ , IFRQ  , IREFIL, IRESID, ISAT  ,
     1          ISL   , MXCSAT, NFRAUX, NFROUT, NSAT  , NSLIP , NSTEP
C
CCC       IMPLICIT   REAL*8 (A-H,O-Z)
CCC       IMPLICIT   INTEGER*4 (I-N)
C
      CHARACTER*1 FLGAMB
      CHARACTER*6 MXNSAT
      INTEGER*4   LSTSLP(6,*),SVN(*),FRQAUX(*),IRFSAT(2)
      INTEGER*4   NNEWAM(*),LSTAMB(3,MXCSAT,*),LASTFL(2,*)

      REAL*8      SLPTOT(2)
      REAL*8      JUMP(2)                                               GLONASS
      REAL*8      SLPLST(*),ABSACT(3,*),RHS(3,*)
C
      INCLUDE 'COMFREQ.inc'
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
C
C APPLY CYCLE SLIPS TO ARRAY ABSACT
C ---------------------------------
      DO 20 ISAT=1,NSAT
        DO IFRQ=1,NFRAUX
          IF(ABSACT(IFRQ,ISAT).NE.1.D20)THEN
            IFREQ=FRQAUX(IFRQ)
            IF(IFRQ.LE.2)THEN
              SLPTOT(IFRQ)=0.D0
              JUMP(IFRQ)=0.D0                                           GLONASS
              DO 10 ISL=NSTEP,NSLIP
                IF (IABS(LSTSLP(6,ISL)).NE.3 .AND.                      GLONASS
     1              IABS(LSTSLP(6,ISL)).NE.5) THEN                      GLONASS
                  IF((SVN(ISAT).EQ.LSTSLP(2,ISL).OR.                    GLONASS
     1                99       .EQ.LSTSLP(2,ISL))   .AND.               GLONASS
     2               IFREQ.EQ.LSTSLP(3,ISL))THEN                        GLONASS
                    SLPTOT(IFRQ)=SLPTOT(IFRQ)+SLPLST(ISL)/LSTSLP(4,ISL) GLONASS
                  END IF                                                GLONASS
                ELSE                                                    GLONASS
                  IF((SVN(ISAT).EQ.LSTSLP(2,ISL).OR.                    GLONASS
     1                99       .EQ.LSTSLP(2,ISL))   .AND.               GLONASS
     2               IFREQ.EQ.LSTSLP(3,ISL))THEN                        GLONASS
                    JUMP(IFRQ)=JUMP(IFRQ)+SLPLST(ISL)/LSTSLP(4,ISL)     GLONASS
                  END IF                                                GLONASS
                ENDIF                                                   GLONASS
10            CONTINUE
              ABSACT(IFRQ,ISAT)=ABSACT(IFRQ,ISAT)                       GLONASS
     1                          -SLPTOT(IFRQ)*WLGT(IFREQ,SVN(ISAT))     GLONASS
     2                          -JUMP(IFRQ)*WLGT(IFREQ,SVN(1))          GLONASS
            ELSE
              ABSACT(IFRQ,ISAT)=ABSACT(IFRQ,ISAT)-
     1                     (SLPTOT(1)-SLPTOT(2))*WLGT(5,SVN(ISAT))      GLONASS
     2                    -(JUMP(1)-JUMP(2))*WLGT(5,SVN(1))             GLONASS
            END IF
          END IF
        ENDDO
20    CONTINUE
C
C APPLY CYCLE SLIPS TO ARRAY RHS
C ------------------------------
      DO 40 ISAT=1,NSAT
        DO IFRQ=1,NFRAUX
          IF(RHS(IFRQ,ISAT).NE.1.D20)THEN
            IF(IFRQ.LE.2)THEN
              IFREQ=FRQAUX(IFRQ)
              SLPTOT(IFRQ)=0.D0
              JUMP(IFRQ)=0.D0                                           GLONASS
              DO 30 ISL=NSTEP,NSLIP                                     GLONASS
                IF (IABS(LSTSLP(6,ISL)).NE.3 .AND.                      GLONASS
     1              IABS(LSTSLP(6,ISL)).NE.5) THEN                      GLONASS
                  IF((SVN(ISAT).EQ.LSTSLP(2,ISL).OR.                    GLONASS
     1                99       .EQ.LSTSLP(2,ISL))   .AND.               GLONASS
     2               IFREQ.EQ.LSTSLP(3,ISL))THEN                        GLONASS
                    SLPTOT(IFRQ)=SLPTOT(IFRQ)+SLPLST(ISL)/LSTSLP(4,ISL) GLONASS
                  END IF                                                GLONASS
                ELSE                                                    GLONASS
                  IF((SVN(ISAT).EQ.LSTSLP(2,ISL).OR.                    GLONASS
     1                99       .EQ.LSTSLP(2,ISL))   .AND.               GLONASS
     2               IFREQ.EQ.LSTSLP(3,ISL))THEN                        GLONASS
                    JUMP(IFRQ)=JUMP(IFRQ)+SLPLST(ISL)/LSTSLP(4,ISL)     GLONASS
                  END IF                                                GLONASS
                ENDIF                                                   GLONASS
30            CONTINUE                                                  GLONASS
              RHS(IFRQ,ISAT)=RHS(IFRQ,ISAT)                             GLONASS
     1                       -SLPTOT(IFRQ)*WLGT(IFREQ,SVN(ISAT))        GLONASS
     2                       -JUMP(IFRQ)*WLGT(IFREQ,SVN(1))             GLONASS
            ELSE                                                        GLONASS
              RHS(IFRQ,ISAT)=RHS(IFRQ,ISAT)-                            GLONASS
     1                     (SLPTOT(1)-SLPTOT(2))*WLGT(5,SVN(ISAT))      GLONASS
     2                    -(JUMP(1)-JUMP(2))*WLGT(5,SVN(1))             GLONASS
            END IF
          END IF
        ENDDO
40    CONTINUE
C
C WRITE RESIDUALS
C ---------------
      IF(IRESID.EQ.1)THEN
        IF(NFRAUX.EQ.1)THEN
          NFROUT=1
          IRFSAT(1)=0
        ELSE
          NFROUT=2
          IRFSAT(1)=0
          IRFSAT(2)=0
        END IF
85      DO 100 ISAT=1,NSAT
          DO 90 IFRQ=1,NFROUT
            IF(RHS(IFRQ,ISAT).NE.1.D20) THEN
              IF(IRFSAT(IFRQ).NE.0) THEN
                FLGAMB=' '
C HAS THE REFERENCE SATELLITE A FLAG?
                DO 50 IAMB=1,NNEWAM(IRFSAT(IFRQ))
                  IF (LASTFL(IFRQ,IRFSAT(IFRQ))   .LT.
     1                LSTAMB(1,IRFSAT(IFRQ),IAMB) .AND.
     2                LSTAMB(1,IRFSAT(IFRQ),IAMB) .LT.
     3                IEPOCH) FLGAMB='A'
50              CONTINUE
                LASTFL(IFRQ,IRFSAT(IFRQ))=IEPOCH
C HAS THE SECOND SATELLITE A FLAG?
                DO 60 IAMB=1,NNEWAM(ISAT)
                  IF (LASTFL(IFRQ,ISAT)   .LT.
     1                LSTAMB(1,ISAT,IAMB) .AND.
     2                LSTAMB(1,ISAT,IAMB) .LT.
     3                IEPOCH) FLGAMB='A'
60              CONTINUE
C
                WRITE(LFNRES) IREFIL,IEPOCH,FRQAUX(IFRQ),
     1                        SVN(IRFSAT(IFRQ)),SVN(ISAT),
     2                        RHS(IFRQ,IRFSAT(IFRQ))-RHS(IFRQ,ISAT),
     3                        FLGAMB
              ENDIF
              IRFSAT(IFRQ)=ISAT
            ENDIF
90        CONTINUE
100     CONTINUE
        IF (IRFSAT(1).NE.0) THEN
          DO 80 IFRQ=1,NFROUT
            LASTFL(IFRQ,IRFSAT(IFRQ))=IEPOCH
80        CONTINUE
        ENDIF
C
      END IF
C
      RETURN
      END SUBROUTINE

      END MODULE
