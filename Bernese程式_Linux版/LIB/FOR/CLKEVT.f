      MODULE s_CLKEVT
      CONTAINS

C*
      SUBROUTINE CLKEVT(IPRNT2,ITITL2,JMPOPT,NSAT  ,SVN   ,
     1                  NFRAUX,IDELTT,NEPOCH,NCLKEV,LSTCLK,
     2                  NDEL  ,LSTDEL,NNEWAM,LSTAMB,AMSFLG)
CC
CC NAME       :  CLKEVT
CC
CC PURPOSE    :  HANDLING OF CLOCK EVENTS IN MAUPRP, ZD CASE
CC
CC PARAMETERS :
CC         IN :  IPRNT2 : PRINT LEVEL FOR CYCLE SLIP DETECT.  I*4
CC                        =0: NO MESSAGES PRINTED
CC                        =1: PRINT SUMMARY
CC                        =2: ALL MESSAGES PRINTED
CC               ITITL2 : TITLE LINE FLAG                     I*4
CC               JMPOPT : CLOCK EVENT OPTIONS                  I*4(6)
CC                         (1): 0/1 ALLOW MS-JUMP CYCLE SLIPS
CC                         (2): MIN. SIZE OF A CLOCK EVENT (NS)
CC                         (3): MARK EPOCHS WITH CLOCK EVENTS
CC                              UP TO (IN S)
CC                         (4): 0/1 AMBIGUITIES FOR ALL SATELLITES
CC                         (5): 0/1 FLAG IF MS-JUMP IN FILE
CC                         (6): 0/1 FLAG IF A CLOCK EVENT IN FILE
CC               NSAT   : NUMBER OF SATELLITES                I*4
CC               SVN(I),I=1,2,..,NSAT: SV NUMBERS             I*4
CC               NFRAUX : NUMBER OF FREQUENCIES ON AUX.-FILE  I*4
CC               IDELTT : TIME INTERVAL BETWEEN SUBSEQUENT    I*4
CC                        OBSERVATION EPOCHS
CC               NEPOCH : NUMBER OF EPOCHS IN  FILE           I*4
CC               NCLKEV : NUMBER OF CLOCK EVENTS IN LIST      I*4
CC               LSTCLK : (1,I): EPOCH OF THE CLOCK EVENT
CC                        (2,I): MAGNITUDE OF THE CLOCK EVENT
CC                               IN NANOSECONDS
CC                        (3,I): CLOCK ESTIMATE FROM CODSPP
CC                               IN NANOSECONDS
CC                        (4,I): ACTION (0:NONE,1:MARK,2:AMB)
CC    IN/OUT :   NNEWAM(ISATEL) : NUMBER OF AMBIGUITIES        I*4
CC               LSTAMB(I,ISATEL,IAMB)  LIST OF AMBIGUITIES    I*4
CC                        I=1 : THE FIRST EPOCH
CC                        I=2 : TYPE: 1 ... FILE
CC                                    2 ... CYCLE SLIP
CC                                    3 ... USER
CC                                    4 ... GAP
CC                                    5 ... PREPROCESSING PROBLEM
CC                                    6 ... CLOCK EVENT
CC                        I=3 : THE LAST EPOCH WITH OBSERVATIONS
CC               AMSFLG : INDICATER OF MAXAMS EXCEEDINGS      CH*1
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
CC                               =6: SMALL PIECE
CC                               =7: BAD OBSERVED-COMPUTED
CC                               ALL AREAS MARKED OR CHANGED IN THE
CC                               LATEST RUN HAVE A NEGATIVE SIGN
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  R.DACH
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  06-DEC-2002
CC
CC CHANGES    :  21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               19-JUL-10 : SL: TAB CHARACTERS REMOVED
CC               29-FEB-12 : RD: CORRECT ARRAY DIMENSIONS OF IWLF0 AND XSLIP0
CC               26-MAR-12 : RD: USE WTMSGS AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnprt
      USE s_mrkobs
      USE s_updamb
      USE s_wtmsgs
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IAMB  , ICKSUM, ICLK  , ICLK2 , IDELTT, IEPO  , IEPO1 ,
     1          IEPO2 , IFRQ  , IFRQ3 , IMSG  , IPRNT2, IRC   , IRSAT0,
     2          ISAT  , ISVN  , ITITL2, JCLK  , MXCSAT, NCLKEV,
     3          NDEL  , NEPOCH, NFRAUX, NSAT
C
CCC       IMPLICIT REAL*8    (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      CHARACTER*1  AMSFLG
      CHARACTER*6  MXNSAT
C
      INTEGER*4    JMPOPT(6),SVN(*)
      INTEGER*4    NNEWAM(*),LSTAMB(3,MXCSAT,*)
      INTEGER*4    LSTDEL(5,*),IWLF0(3)
      INTEGER*4    LSTCLK(4,*)
C
      REAL*8       SLIP0(3),XSLIP0(3)
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
C
      DATA IFRQ3/3/,IRSAT0/0/
C
C
C INIT VARIABLES
C --------------
      IWLF0 =0
      SLIP0 =0.D0
      XSLIP0=0.D0
C
C HANDLE CLOCK EVENTS: MARK EPOCHS
C --------------------------------
      IF (NCLKEV.GT.0.AND.JMPOPT(3).GT.0) THEN
C
C LOOP CLOCK EVENT LIST
C ---------------------
        DO ICLK=1,NCLKEV
C
C ALREADY HANDLED
C ---------------
          IF (LSTCLK(4,ICLK).NE.0) CYCLE
C
C CHECK FOLLOWING EVENTS
C ----------------------
          ICKSUM=0
          ICLK2=0
          DO JCLK=ICLK,NCLKEV
C
C MAXIMUM TIME INTERVAL EXCEEDED
C ------------------------------
            IF ((LSTCLK(1,JCLK)-LSTCLK(1,ICLK))*IDELTT.GT.JMPOPT(3))THEN
              ICLK2=JCLK-1
              EXIT
            ENDIF
C
C THE CLOCK HAS BEEN JUMPED BACK: MARK OF OBS. IS POSSIBLE
C --------------------------------------------------------
            ICKSUM=ICKSUM+LSTCLK(2,JCLK)
            IF (ABS(ICKSUM).LT.JMPOPT(2)) THEN
              ICLK2=JCLK
              EXIT
            ENDIF
          ENDDO

          IF (ICLK2.EQ.0) CYCLE
          IF (ABS(ICKSUM).GE.JMPOPT(2)) CYCLE
C
C AN INTERVAL WAS FOUND
C ---------------------
          IEPO1=LSTCLK(1,ICLK)
          IEPO2=LSTCLK(1,ICLK2)

          IF (IEPO1.EQ.IEPO2) CYCLE
C
C LOOP ALL CLOCK EVENTS WHICH MAY BE "SOLVED" BY MARKING
C ------------------------------------------------------
          DO JCLK=ICLK,ICLK2

            LSTCLK(4,JCLK)=1
            IEPO=LSTCLK(1,JCLK)

            IF (IPRNT2.GE.2) THEN
              WRITE(LFNPRT,'(/,A,I9,A,/,14X,A,I5,A,/)')
     1        ' CLOCK EVENT: CLOCK EVENT DETECTED: ',
     2                                         LSTCLK(2,JCLK),' NS',
     3              'ALL OBSERVATIONS OF THE EPOCH ',IEPO,' MARKED'
            ENDIF
          ENDDO
C
C LOOP THE EPOCHS FOR MARKING OBSERVATIONS
C ----------------------------------------
          DO IEPO=IEPO1,IEPO2
            DO ISAT=1,NSAT
C
              ISVN=SVN(ISAT)
C
              DO IFRQ=1,2
                IF (IFRQ.GT.NFRAUX) EXIT
                CALL MRKOBS(8,ISVN,IEPO,IFRQ,NSAT,SVN,NDEL,LSTDEL)
              ENDDO
C
            ENDDO
C
            IF(IPRNT2.GE.2) THEN
              IMSG=11
              CALL WTMSGS(IMSG,ITITL2,IEPO,99,IRSAT0,
     1                    IFRQ3,IWLF0,SLIP0,XSLIP0)
            ENDIF
C
          ENDDO
        ENDDO
      ENDIF
C
C HANDLE CLOCK EVENTS: MARK EPOCHS AT THE BEGINNING OR THE END OF THE FILE
C --------------------------------
      IF (NCLKEV.GT.0.AND.JMPOPT(3).GT.0) THEN
C
C LOOP CLOCK EVENT LIST
C ---------------------
        DO ICLK=1,NCLKEV
C
C ALREADY HANDLED
C ---------------
          IF (LSTCLK(4,ICLK).NE.0) CYCLE
C
          IEPO1 = 0
          IEPO2 = 0
C
C CLOCK EVENT AT THE BEGINNING OF THE FILE
C ----------------------------------------
          IF ((LSTCLK(1,ICLK)-1)*IDELTT.LE.JMPOPT(3))THEN
            IEPO1 = 1
            IEPO2 = LSTCLK(1,ICLK)
          ENDIF
C
C CLOCK EVENT AT THE END OF THE FILE
C -----------------------------------
          IF ((NEPOCH-LSTCLK(1,ICLK))*IDELTT.LE.JMPOPT(3))THEN
            IEPO1 = LSTCLK(1,ICLK)
            IEPO2 = NEPOCH
          ENDIF
C
C AN INTERVAL WAS FOUND
C ---------------------
          IF (IEPO1.EQ.0.OR.IEPO2.EQ.0) CYCLE
C
C LOOP ALL CLOCK EVENTS WHICH MAY BE "SOLVED" BY MARKING
C ------------------------------------------------------
          LSTCLK(4,ICLK)=1
          IEPO=LSTCLK(1,ICLK)

          IF (IPRNT2.GE.2) THEN
              WRITE(LFNPRT,'(/,A,I9,A,/,14X,A,I5,A,/)')
     1        ' CLOCK EVENT: CLOCK EVENT DETECTED: ',
     2                                         LSTCLK(2,JCLK),' NS',
     3              'ALL OBSERVATIONS OF THE EPOCH ',IEPO,' MARKED'
          ENDIF
C
C LOOP THE EPOCHS FOR MARKING OBSERVATIONS
C ----------------------------------------
          DO IEPO=IEPO1,IEPO2
            DO ISAT=1,NSAT
C
              ISVN=SVN(ISAT)
C
              DO IFRQ=1,2
                IF (IFRQ.GT.NFRAUX) EXIT
                CALL MRKOBS(8,ISVN,IEPO,IFRQ,NSAT,SVN,NDEL,LSTDEL)
              ENDDO
C
            ENDDO
C
            IF(IPRNT2.GE.2) THEN
              IMSG=11
              CALL WTMSGS(IMSG,ITITL2,IEPO,99,IRSAT0,
     1                    IFRQ3,IWLF0,SLIP0,XSLIP0)
            ENDIF
C
          ENDDO
        ENDDO
      ENDIF
C
C HANDLE CLOCK EVENTS: NEW AMBIGUITIES FOR ALL SATELLITES
C -------------------------------------------------------
      IF (NCLKEV.GT.0.AND.JMPOPT(4).EQ.1) THEN
C
C LOOP ALL CLOCK EVENTS WHICH ARE STILL NOT HANDLED
C -------------------------------------------------
        DO ICLK=1,NCLKEV
C
          IF (LSTCLK(4,ICLK).NE.0) CYCLE

          LSTCLK(4,ICLK)=2
          IEPO=LSTCLK(1,ICLK)

          IF (IPRNT2.GE.2) THEN
            WRITE(LFNPRT,'(A,I9,A,/,14X,A,I5,/)') ' CLOCK EVENT: '//
     1            'CLOCK EVENT DETECTED: ',LSTCLK(2,ICLK),' NS',
     2            'AMBIGUITIES FOR ALL SATELLITES AT EPOCH ',IEPO
          ENDIF
C
C SETUP A NEW AMBIGUITY FOR ALL SATELLITES
C ----------------------------------------
          DO ISAT=1,NSAT
            DO IAMB=1,NNEWAM(ISAT)
              IF (LSTAMB(1,ISAT,IAMB).GT.IEPO) EXIT
              IF (LSTAMB(1,ISAT,IAMB).LE.IEPO.AND.
     1            LSTAMB(3,ISAT,IAMB).GE.IEPO) THEN
                CALL UPDAMB(ISAT  ,IEPO  ,IEPO  ,6     ,NNEWAM,LSTAMB,
     1                      NSAT  ,SVN   ,IPRNT2,AMSFLG,IRC)
                EXIT
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF

      RETURN
      END SUBROUTINE

      END MODULE
