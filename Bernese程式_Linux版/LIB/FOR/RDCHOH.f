      MODULE s_RDCHOH
      CONTAINS

C*
      SUBROUTINE RDCHOH(FILNAM,LFN,IFRMAT,TITLE,TFIRST,TLAST,NEPO,
     1                  DTTAB,COOSYS)
CC
CC NAME       :  RDCHOH
CC
CC PURPOSE    :  READ HEADER OF THE CHAMP ORBIT FORMAT CHORB
CC
CC
CC PARAMETERS :
CC         IN :  FILNAM : FILE NAME OF PRECISE ORBIT FILE     CH*32
CC               LFN    : LOGICAL FILE NUMBER FOR INPUT FILE   I*4
CC                        = -1: USE LFNLOC AS LFN AND CLOSE FILE
CC                              AFTER READING THE HEADER
CC                        >= 0: LFN IS USED FOR OPENING THE FILE
CC                              AND FILE STAYS OPEN AFTER READING
CC                              THE HEADER
CC        OUT :  IFRMAT : FORMAT TYPE                          I*4
CC                        =0 : FIRST CHORB FORMAT
CC               TITLE  : TITLE LINE                          CH*57
CC               TFIRST : TIME OF FIRST EPOCH (MJD)            R*8
CC               TLAST  : TIME OF LAST EPOCH  (MJD)            R*8
CC               NEPO   : NUMBER OF EPOCHS                     I*4
CC               DTTAB  : TABULAR INTERVAL (SEC)               R*8
CC               COOSYS : COORDINATE SYSTEM                    CH*5
CC
CC REMARKS    :  CTS IS SET TO ITR96 AND TIME FRAME TO TT
CC
CC AUTHOR     :  D. SVEHLA
CC
CC VERSION    :  5.0
CC
CC CREATED    :  23-JAN-01
CC
CC CHANGES    :  21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC               UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*

C
C DECLARATIONS
C ------------
      USE m_bern

      USE s_opnfil
      USE s_opnerr
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IFRMAT, IOSTAT, LFN   , LFN1  , NEPO
C
CCC       IMPLICIT INTEGER*4 (I-N)
CCC       IMPLICIT REAL*8 (A-H,O-Z)

      REAL*8 TFIRST,TLAST,DTTABI,DTTAB
      REAL*8 MFIRST,MLAST,XDAY1,XDAY2,TEPOCH1,TEPOCH2

      CHARACTER(LEN=lineLength)    :: LINE
      CHARACTER(LEN=fileNameLength):: FILNAM

      CHARACTER*57 TITLE
      CHARACTER*6  FIRST6
      CHARACTER*5  COOSYS
      CHARACTER*3  TFRAME

C
C OPEN INPUT FILE
C ---------------
      IF(LFN.EQ.-1) THEN
        LFN1=LFNLOC
      ELSE
        LFN1=LFN
      END IF

      CALL OPNFIL(LFN1,FILNAM,'OLD','FORMATTED','READONLY',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFN1,IOSTAT,FILNAM,'RDCHOH')

C
C READ CHORB HEADER
C -----------------
      TITLE(1:57)=' '
      MFIRST=0.D0
      TFIRST=0.D0
      MLAST=0.D0
      TLAST=0.D0
      COOSYS='     '

      FIRST6='     '
      DO WHILE (FIRST6.NE.'ORBIT ')
        READ (LFN1,'(A)',ERR=901,END=911) LINE
        FIRST6(1:6)=LINE(1:6)
C
C READ TITLE
C ----------
        IF (FIRST6=='DSIDP ') THEN
          READ (LINE,100,ERR=901,END=911) TITLE
100       FORMAT(7X,A57)

C
C READ PERIOD
C -----------
        ELSE IF (FIRST6=='PERIOD' ) THEN
          READ (LINE,200,ERR=901,END=911) MFIRST, TFIRST, MLAST, TLAST
200       FORMAT(7X,F6.0,F12.0,F7.0,F12.0)

C CHECK PERIOD
          IF(MFIRST.GT.MLAST) GOTO 920
C
C READ TIME FRAME
C ---------------
        ELSE IF (FIRST6=='TFRAME' ) THEN
          READ (LINE,300,ERR=901,END=911) TFRAME
300       FORMAT(7X,A3)

C CHECK TIME FRAME
          IF(TFRAME.NE.'TT ') GOTO 925
C
C READ CTS
C --------
        ELSE IF (FIRST6=='RFRAME' ) THEN
          READ (LINE,400,ERR=901,END=911) COOSYS
400       FORMAT(7X,A5)

        END IF
      END DO
C
C COUNTING RECORDS & TABULAR INTERVAL DETERMINATION
C -------------------------------------------------
      NEPO=0
      DO
        READ (LFN1,'(A)',ERR=901,END=600) LINE
          NEPO=NEPO+1
          IF (IDNINT(NEPO/2.D0).EQ.(NEPO/2.D0)) THEN
            READ (LINE,'(F6.0,F11.0)') XDAY2, TEPOCH2
          ELSE
            READ (LINE,'(F6.0,F11.0)') XDAY1, TEPOCH1
          END IF
          IF(NEPO.GT.1) THEN
            DTTABI=DABS(TEPOCH2-TEPOCH1)*1.D-6+8640.D0*(XDAY2-XDAY1)
            IF(NEPO.EQ.2) DTTAB=DTTABI
            IF (DTTABI.LT.DTTAB) DTTAB=DTTABI
          END IF
      END DO
C
C CONVERSION  TT -> GPSTIME -> MJD
C --------------------------------
600    TFIRST=TFIRST*1.D-6                     ![s]
       TLAST=TLAST*1.D-6                       ![s]
       TFIRST=(TFIRST-(19.+32.184))/86400.D0+MFIRST/10.D0+51544.5
       TLAST=(TLAST-(19.+32.184))/86400.D0+MLAST/10.D0+51544.5
C
C  SEARCH FOR ORBIT RECORDS
C -------------------------
      IF (LFN.NE.-1) THEN
        REWIND(LFN1)
        FIRST6='     '
        DO WHILE (FIRST6.NE.'ORBIT ')
          READ (LFN1,'(A)',ERR=901,END=911) LINE
          FIRST6(1:6)=LINE(1:6)
        END DO
        GOTO 999
      END IF
C
C  CLOSE FILE
C -----------
      CLOSE(UNIT=LFN1)

C
C ERROR: FILE READING
C -------------------
901   WRITE(LFNERR,4000) FILNAM,LFN1
4000  FORMAT(/,' *** SR RDCHOH : FILE READING FAILED',/,
     1       17X,'FILE NAME : ',A32,/,
     2       17X,'FILE UNIT : ',I6,/)
      CALL EXITRC(2)

C
C ERROR: NO COMPLETE HEADER FOUND
C -------------------------------
911   WRITE(LFNERR,4005) FILNAM,LFN1
4005  FORMAT(/,' *** SR RDCHOH : NO COMPLETE HEADER FOUND',/,
     1       17X,'FILE NAME : ',A32,/,
     2       17X,'FILE UNIT : ',I6,/)
      CALL EXITRC(2)

C
C ERROR: FIRST EPOCH AND LAST EPOCH ARE NOT IN THE WRONG ORDER
C ------------------------------------------------------------
920   WRITE(LFNERR,4010) MFIRST,MLAST,FILNAM
4010  FORMAT(/,' *** SR RDCHOH : ',/,
     1       17X,'DAY OF THE FIRST EPOCH : ',F6.0 ,/,
     2       17X,'DAY OF THE LAST EPOCH : ',F6.0,/,
     3       17X,'FILE NAME: ',A32,/)
      CALL EXITRC(2)

C
C ERROR: TIME FRAME IS NOT TT
C ---------------------------
925   WRITE(LFNERR,4020) TFRAME,FILNAM
4020  FORMAT(/,' *** SR RDCHOH : ',/,
     1       17X,'TIME FRAME IS NOT TT : ',A3 ,/,
     2       17X,'FILE NAME: ',A32,/)
      CALL EXITRC(2)
C
C END OF SUBROUTINE
C -----------------
999   RETURN
      END SUBROUTINE

      END MODULE
