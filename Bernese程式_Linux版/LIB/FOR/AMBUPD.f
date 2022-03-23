      MODULE s_AMBUPD
      CONTAINS

C*
      SUBROUTINE AMBUPD(IPRNT2,ITITL2,IAMNEW,NSAT  ,SVN   ,NFRAUX,
     1                  IDELTT,NNEWAM,LSTAMB,AMSFLG,NDEL  ,LSTDEL)
CC
CC NAME       :  AMBUPD
CC
CC PURPOSE    :  REMOVE AMBIGUITIES WITH NO/FEW OBSERVATIONS
CC
CC PARAMETERS :
CC         IN :  IPRNT2 : PRINT LEVEL FOR CYCLE SLIP DETECT.  I*4
CC                        =0: NO MESSAGES PRINTED
CC                        =1: PRINT SUMMARY
CC                        =2: ALL MESSAGES PRINTED
CC               ITITL2 : TITLE LINE FLAG                     I*4
CC               IAMNEW(I): SETTING OF NEW AMBIGUITIES        I*4
CC                        I=1 : USE CYCLE SLIP FLAG (0/1)
CC                        I=2 : IF PROBLEM IN SLIP-FIXING (0/1)
CC                        I=3 : AFTER GAP LARGER THAN (SEC)
CC                        I=4 : USE AMBIGUITIES FROM FILE (0/1)
CC                        I=5 : MIN. TIME OF OBS. PER AMB. (SEC)
CC               NSAT   : NUMBER OF SATELLITES                I*4
CC               SVN(I),I=1,2,..,NSAT: SV NUMBERS             I*4
CC               NFRAUX : NUMBER OF FREQUENCIES ON AUX.-FILE  I*4
CC               IDELTT : TIME INTERVAL BETWEEN SUBSEQUENT    I*4
CC                        OBSERVATION EPOCHS
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
CC CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               29-FEB-12 : RD: CORRECT ARRAY DIMENSIONS OF IWLF0 AND XSLIP0
CC               26-MAR-12 : RD: USE WTMSGS AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE s_mrkobs
      USE s_updamb
      USE s_wtmsgs
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IAMB  , IDEL  , IDELTT, IEPO  , IEPO1 , IEPO2 , IFRQ  ,
     1          IFRQ3 , IMSG  , IPRNT2, IRC   , IRSAT0, ISAT  , ITITL2,
     2          JDEL  , JFRQ  , MXCSAT, NDEL  , NFRAUX, NSAT
C
CCC       IMPLICIT REAL*8    (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      CHARACTER*1  AMSFLG
      CHARACTER*6  MXNSAT
C
      INTEGER*4    IAMNEW(5),SVN(*)
      INTEGER*4    NNEWAM(*),LSTAMB(3,MXCSAT,*)
      INTEGER*4    LSTDEL(5,*)
      INTEGER*4    NUMEPO(2),IWLF0(3)
C
      REAL*8       SLIP0(3), XSLIP0(3)
C
      LOGICAL      MRKEPO
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
C
      DATA IFRQ3/3/,IRSAT0/0/
C
C INIT VARIABLES
C --------------
      IWLF0 =0
      SLIP0 =0.D0
      XSLIP0=0.D0
C
C LOOP ALL SATELLITES
C -------------------
      DO ISAT=1,NSAT
C
C LOOP THE AMBIGUITIES OF THE SATELLITE
C -------------------------------------
        IAMB=1
        DO WHILE (IAMB.LT.NNEWAM(ISAT))
          IAMB=IAMB+1
C
C INIT THE FIRST/LAST EPOCH
C -------------------------
          NUMEPO(1)=0
          NUMEPO(2)=0
C
C GET START/END EPOCH OF THE AMBIGUITY
C ------------------------------------
          IEPO1=LSTAMB(1,ISAT,IAMB)
          IEPO2=LSTAMB(3,ISAT,IAMB)
C
C CHECK ALL EPOCHS/FREQUENCIES OF THE AMBIGUITY
C ---------------------------------------------
          DO IEPO=IEPO1,IEPO2
            MRKEPO = .FALSE.
            DO IFRQ=1,NFRAUX
              IF (IFRQ.GT.2) CYCLE
C
C CHECK FOR MARK FLAGS FOR THE CORRESPONDING OBSERVATION
C ------------------------------------------------------
              DO IDEL=1,NDEL
C
                IF (LSTDEL(1,IDEL).NE.SVN(ISAT)) CYCLE
                IF (LSTDEL(3,IDEL).LT.IEPO) CYCLE
                IF (LSTDEL(2,IDEL).GT.IEPO) CYCLE
                IF (LSTDEL(4,IDEL).NE.IFRQ) CYCLE
C
                MRKEPO = .TRUE.
                EXIT
C
              ENDDO
            ENDDO
C
C GET FIRST/LAST EPOCH WITH A VALID OBSERVATION
C ---------------------------------------------
            IF (.NOT. MRKEPO) THEN
C
              IF (NUMEPO(1).EQ.0) NUMEPO(1)=IEPO
              IF (NUMEPO(2).EQ.0 .OR. NUMEPO(2).LT.IEPO) NUMEPO(2)=IEPO
C
            ENDIF
C
          ENDDO
C
C INTERVAL BETWEEN FIRST/LAST OBSERVATIONS CORRESPONDS WITH THE INPUT PARAMETER
C -----------------------------------------------------------------------------
          IF ((NUMEPO(2)-NUMEPO(1))*IDELTT.GE.IAMNEW(5)) CYCLE
C
C MARK ALL OBSERVATIONS OF THE AMBIGUITY
C --------------------------------------
          DO IEPO=IEPO1,IEPO2
            JFRQ=0
            DO IFRQ=1,NFRAUX
              IF (IFRQ.GT.2) CYCLE
              JDEL=0
              DO IDEL=1,NDEL
                IF (LSTDEL(1,IDEL).NE.SVN(ISAT)) CYCLE
                IF (LSTDEL(3,IDEL).LT.IEPO) CYCLE
                IF (LSTDEL(2,IDEL).GT.IEPO) CYCLE
                IF (LSTDEL(4,IDEL).NE.IFRQ) CYCLE
                JDEL=IDEL
                EXIT
              ENDDO
              IF (JDEL.EQ.0) THEN
                CALL MRKOBS(6,SVN(ISAT),IEPO,IFRQ,NSAT,SVN,NDEL,LSTDEL)
                JFRQ=1
              ENDIF
            ENDDO
            IF(JFRQ.EQ.1.AND.IPRNT2.GE.2) THEN
              IMSG=7
              CALL WTMSGS(IMSG,ITITL2,IEPO,SVN(ISAT),IRSAT0,
     1                    IFRQ3,IWLF0,SLIP0,XSLIP0)
            ENDIF
          ENDDO
C
C REMOVE THE AMBIGUITY
C --------------------
          CALL UPDAMB(ISAT,IEPO1,IEPO2,0,NNEWAM,LSTAMB,NSAT,SVN,
     1                IPRNT2,AMSFLG,IRC)
          IAMB=1
        ENDDO
      ENDDO

      RETURN
      END SUBROUTINE

      END MODULE
