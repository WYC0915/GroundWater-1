      MODULE s_UPDAMB
      CONTAINS

C*
      SUBROUTINE UPDAMB(ISATEL,IEPO1 ,IEPO2 ,ITYPE ,NNEWAM,LSTAMB,
     1                  NSATEL,SVN   ,IPRNT2,AMSFLG,IRC   )
CC
CC NAME       :  UPDAMB
CC
CC PURPOSE    :  UPDATE LIST OF AMBIGUITIES
CC
CC PARAMETERS :
CC         IN :  ISATEL : SATELLITE INDEX IN FILE SPECIFIC      I*4
CC                        ARRAY
CC               IEPO1  : STARTING EPOCH FOR REMOVAL OF         I*4
CC                        AMBIGUITIES
CC               IEPO2  : LAST EPOCH FOR REMOVAL OF AMBIGUITIES I*4
CC                        AND EPOCH TO SET NEW AMBIGUITY (IF
CC                        ITYPE > 0)
CC               ITYPE  : TYPE OF ACTION TO UPDATE AMBIGUITY    I*4
CC                        LIST
CC                        =   0  ... DELETE AMBIGUITIES
CC                        >   0  ... SET NEW AMBIGUITY AND
CC                                   DELETE EXCESS AMBIGUITIES
CC                        <   0  ... SET NEW AMBIGUITY ONLY IF
CC                                   AN EXCESS AMBIGUITY WILL BE
CC                                   DELETED HERE
CC                        REASON FOR NEW AMBIGUITY:
CC                        = -1,1 ... FILE HEADER
CC                        = -2,2 ... CYCLE SLIP FLAG
CC                        = -3,3 ... USER
CC                        = -4,4 ... BIG GAP
CC                        = -5,5 ... PREPROCESSING PROBLEM
CC                        = -6,6 ... CLOCK EVENT
CC               NSATEL ... NUMBER OF SATELLITES                I*4
CC               SVN(I), I=1,2,...,NSATEL ... SATELLITE NUMBERS I*4
CC               IPRNT2 : PRINT LEVEL                           I*4
CC                        =0, 1 : NO MESSAGE PRINTED
CC                        =2    : MESSAGE PRINTED
CC
CC     IN/OUT :  NNEWAM(ISATEL) : NUMBER OF AMBIGUITIES      I(*)*4
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
CC               AMSFLG : INDICATER OF MAXAMS EXCEEDINGS       CH*1
CC        OUT :  IRC : RETURN CODE                              I*4
CC                           = 0 ... UPDATE IMPOSSIBLE OR
CC                                   UNNECESSARY
CC                           = 1 ... UPDATE OK
CC                           = 2 ... MAXAMS EXCEEDED
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  L.MERVART, M.ROTHACHER
CC
CC VERSION    :  3.4
CC
CC CREATED    :  05-JUN-92
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC               30-SEP-94 : MR: NEW ERROR MESSAGE FOR "MXCAMS"
CC               05-MAR-96 : TS: SET RETURN CODE AND AMSFLG IN CASE OF "MXCAMS"
CC               06-DEC-02 : RD: UPDATE OF LSTAMB
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               19-APR-06 : HB: INCREASE FORMAT STATEMENTS FOR EPOCH
CC                               WRITING (I4->I6)
CC
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IAMB  , IEPO1 , IEPO2 , IPRNT2, IRC   , IRCAUX, ISATEL,
     1          ITYPE , JAMB  , MXCAMS, MXCSAT, NAMB  , NSATEL
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMAMS/MXCAMS,MXNAMS
C
C EXPLICIT DECLARATIONS
C ---------------------
      INTEGER*4    NNEWAM(MXCSAT),LSTAMB(3,MXCSAT,*),SVN(*)
C
      CHARACTER*1  AMSFLG
      CHARACTER*4  TEXT(6)
      CHARACTER*6  MXNSAT,MXNAMS
C
      DATA TEXT/'FILE','FLAG','USER','GAP ','PREP','CLK '/
C
      IRC = 0
      IF (ITYPE .EQ. 0) THEN
C
C REMOVE AMBIGUITIES
C ------------------
        NAMB = 1
        DO 100 IAMB=2,NNEWAM(ISATEL)
          IF ((LSTAMB(1,ISATEL,IAMB) .LT. IEPO1) .OR.
     1        (LSTAMB(1,ISATEL,IAMB) .GT. IEPO2)) THEN
            NAMB = NAMB + 1
            LSTAMB(1,ISATEL,NAMB) = LSTAMB(1,ISATEL,IAMB)
            LSTAMB(2,ISATEL,NAMB) = LSTAMB(2,ISATEL,IAMB)
            LSTAMB(3,ISATEL,NAMB) = LSTAMB(3,ISATEL,IAMB)
            IF (LSTAMB(1,ISATEL,IAMB) .LT. IEPO1) THEN
              DO JAMB=IAMB+1,NNEWAM(ISATEL)
                IF (LSTAMB(1,ISATEL,JAMB) .LT. IEPO1) EXIT
                IF (LSTAMB(1,ISATEL,JAMB) .GT. IEPO2) EXIT
                LSTAMB(3,ISATEL,NAMB) = LSTAMB(3,ISATEL,JAMB)
              ENDDO
            ENDIF
          ELSE
            IF (IPRNT2 .EQ. 2)
     1        WRITE(LFNPRT,1) SVN(ISATEL),LSTAMB(1,ISATEL,IAMB)
1           FORMAT(' AMB.DEL., SAT:',I4,'   EPO: ',I6)
            IRC = 1
          END IF
100     CONTINUE
        NNEWAM(ISATEL) = NAMB
        RETURN
      ELSE
C
C MULTIPLE AMBIGUITIES
C --------------------
C REMOVE AMBIGUITIES BETWEEN IEPO1,IEPO2-1
C
        IRCAUX = 0
        NAMB = 1
        DO 200 IAMB=2,NNEWAM(ISATEL)
          IF ((LSTAMB(1,ISATEL,IAMB) .LT. IEPO1) .OR.
     1        (LSTAMB(1,ISATEL,IAMB) .GT. IEPO2-1)) THEN
            NAMB = NAMB + 1
            LSTAMB(1,ISATEL,NAMB) = LSTAMB(1,ISATEL,IAMB)
            LSTAMB(2,ISATEL,NAMB) = LSTAMB(2,ISATEL,IAMB)
            LSTAMB(3,ISATEL,NAMB) = LSTAMB(3,ISATEL,IAMB)
            IF (LSTAMB(1,ISATEL,IAMB) .LT. IEPO1) THEN
              DO JAMB=IAMB+1,NNEWAM(ISATEL)
                IF (LSTAMB(1,ISATEL,JAMB) .LT. IEPO1) EXIT
                IF (LSTAMB(1,ISATEL,JAMB) .GT. IEPO2-1) EXIT
                LSTAMB(3,ISATEL,NAMB) = LSTAMB(3,ISATEL,JAMB)
              ENDDO
            ENDIF
          ELSE
            IF (IPRNT2 .EQ. 2)
     1        WRITE(LFNPRT,1) SVN(ISATEL),LSTAMB(1,ISATEL,IAMB)
            IRCAUX = 1
          END IF
200     CONTINUE
        NNEWAM(ISATEL) = NAMB
C
C SET NEW AMBIGUITY
C
        IF ((IRCAUX .EQ. 1) .OR. (ITYPE .GT. 0)) THEN
          DO 250 IAMB=1,NNEWAM(ISATEL)
            IF(IEPO2 .EQ. LSTAMB(1,ISATEL,IAMB)) RETURN
250       CONTINUE
C
          IF (NNEWAM(ISATEL)+1.GT.MXCAMS) THEN
            WRITE(LFNERR,901) NNEWAM(ISATEL)+1,MXCAMS,SVN(ISATEL)
901         FORMAT(/,' ### SR UPDAMB: TOO MANY AMBIGUITIES PER ',
     1               'SATELLITE',
     2             /,16X,'NUMBER OF AMBIGUITIES >=',I4,
     3             /,16X,'MAXIMUM # OF AMBIG.    :',I4,
     4             /,16X,'SATELLITE              :',I4,/)
            AMSFLG = 'B'
            IRC = 2
            RETURN
          ENDIF
C
          DO 300 IAMB=NNEWAM(ISATEL),1,-1
            IF(IEPO2 .GT. LSTAMB(1,ISATEL,IAMB)) THEN
              LSTAMB(1,ISATEL,IAMB+1) = IEPO2
              LSTAMB(2,ISATEL,IAMB+1) = -IABS(ITYPE)
              IF (LSTAMB(3,ISATEL,IAMB).GE.IEPO2) THEN
                LSTAMB(3,ISATEL,IAMB+1) = LSTAMB(3,ISATEL,IAMB)
                LSTAMB(3,ISATEL,IAMB)   = IEPO2-1
              ELSE
                LSTAMB(3,ISATEL,IAMB+1) = IEPO2
              ENDIF
              IF (IPRNT2 .EQ. 2)
     1           WRITE(LFNPRT,2) SVN(ISATEL),LSTAMB(1,ISATEL,IAMB+1),
     2                        TEXT(-LSTAMB(2,ISATEL,IAMB+1))
2             FORMAT(' MULTIPLE AMBIGUITY, SAT:',I4,'   EPO: ',I6,
     1               '   REASON: ',A4)
              GO TO 310
            END IF
            LSTAMB(1,ISATEL,IAMB+1)=LSTAMB(1,ISATEL,IAMB)
            LSTAMB(2,ISATEL,IAMB+1)=LSTAMB(2,ISATEL,IAMB)
            LSTAMB(3,ISATEL,IAMB+1)=LSTAMB(3,ISATEL,IAMB)
300       CONTINUE
310       NNEWAM(ISATEL) = NNEWAM(ISATEL) + 1
          IRC = 1
          RETURN
        END IF
      END IF
      END SUBROUTINE

      END MODULE
