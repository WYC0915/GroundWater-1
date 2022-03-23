      MODULE s_OPNFIL
      CONTAINS
C*
      SUBROUTINE OPNFIL(LFN,FILNAM,STATUS,FORM,ACTION,RESERV,IOSTAT)
CC
CC NAME       : OPNFIL
CC
CC PURPOSE    : OPEN FILE
CC
CC PARAMETERS :
CC         IN : LFN        : LOGICAL FILENUMBER                     I*4
CC              FILNAM     : FILENAME OF FILE TO BE OPENED         CH*(*)
CC              STATUS     : 'OLD','NEW','UNKNOWN','SCRATCH'       CH*(*)
CC              FORM       : 'FORMATTED','UNFORMATTED','BINARY'    CH*(*)
CC              ACTION     : 'READONLY',' '                        CH*(*)
CC              RESERV     : RESERVED                              CH*(*)
CC        OUT : IOSTAT     : STATUS                                 I*4
CC
CC REMARKS    : SEE COMMENTS FOR DIFFERENT DIALECTS
CC
CC CREATED    : 25-APR-91
CC
CC CHANGES    : 23-AUG-93 : ??: "BINARY" SIMULATED WITH DIRECT ACCESS
CC              23-AUG-94 : MR: CALL EXITRC
CC              26-SEP-94 : JJ: PUT FILNAM INTO A STRING SO IT CAN
CC                              BE PASSED IN AS A CONSTANT
CC              28-FEB-96 : ??: UNIFIED (UNIX,VMS,LAHEY)
CC              03-APR-96 : MR: CORRECT _END_
CC              07-MAY-96 : MR: NEW SYSTEM DEFINITIONS (UNDERSCORE)
CC                              USE SUBROUTINE "SYSTYP"
CC              30-JAN-97 : WG: BINARY OPEN FOR DEC_OSF1
CC              05-FEB-97 : JJ: USE SYSNAM INSTEAD OF SYSTYP
CC              22-OCT-97 : MR: OPEN FORMATTED FILES WITH RECL=1024
CC              22-JAN-00 : HU: OPEN UNFORMATTED AS TRANSPARENT FOR LAHEY
CC              09-AUG-00 : HU: REPLACE ENVIRONMENT VARIABLES
CC              07-JUN-02 : HU: TRANSPARENT REMOVED FOR LAHEY
CC              17-FEB-03 : LM: USE PREPROCESSOR COMMANDS, NO SYSNAM
CC              19-FEB-03 : RD: STOP IF RPLENVAR FAILED
CC              13-AUG-03 : RD: LOOP IF "OPEN" FAILED
CC                              WARNING IF "FILNAM" OR "LFN" IS STILL OPEN
CC              18-AUG-03 : RD: NO WARNINGS FOR PG GETKEY
CC              29-Oct-03 : HU: RECL=1024 FOR WINDOWS
CC              17-NOV-03 : RD: PREVENT "PANIC-LOOP"
CC              10-MAR-04 : HB: REMOVE ONLY-STATEMENT FOR M_BERN
CC                              ifdef FOR CMP_IFC ON LINUX (RECL=1024)
CC              07-APR-04 : HU: IOSTAT IN INQUIRE STATEMENT
CC              01-JUL-04 : RD: RECL=1024 -> 10240 FOR CMP_IFC ON LINUX
CC              23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC              13-DEC-11 : SL: NO SPECIAL RECL FOR IFC, M_BERN WITH ONLY
CC              24-MAY-12 : RD: MESSGES ON OPENED FILES ONLY FOR "AIUB"
CC              26-SEP-12 : SL: NO RECL FOR OS_WIN32
CC              29-OCT-12 : RD: REMOVE #ifdef OS_VMS
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE   INSTITUTE FOR APPIELD GEODESY
CC      1989     UNIVERSITY OF BERN       FRANKFURT/MAIN
CC               SWITZERLAND              GERMANY
CC
C*
      USE M_BERN,   ONLY: program_Name
      USE D_INPKEY, ONLY: inpKey, myStatus_Run
      USE s_sjustl
      USE s_rplenvar
      USE s_exitrc
      USE s_upperc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IDEL  , IOSTAT, IRERUN, LFN   , LFNTST, LRECL
C
      CHARACTER FILNAM*(*),STATUS*(*),FORM*(*),ACTION*(*),RESERV*(*)
      CHARACTER*255 FLNAMU,FILTST
      CHARACTER*20  TEST,ST,FO,AC
      LOGICAL   OPENED
C
C SPECIAL ERROR HANDLING IN EXITRC DURING RUNNING OF OPNFIL
      inpKey%isOPNFIL = .TRUE.
C
C  LEFT-JUSTIFY FILENAME (AS A PRECAUTION!)
      FLNAMU=FILNAM
      CALL SJUSTL(FLNAMU)
C
C REPLACE ENVIRONMENT VARIABLES
      IF (inpKey%status .EQ. myStatus_Run .AND.
     1    INDEX(FLNAMU,'$') .NE. 0) THEN
        CALL RPLENVAR(1,FLNAMU)
        DO iDel = 1,inpKey%nDel
          IF (inpKey%delFil(iDel) .EQ. FILNAM) THEN
            inpKey%delFil(iDel) = FLNAMU
            EXIT
          ENDIF
        ENDDO
      ENDIF
C
C  CLOSE FILE IF IT HAS BEEN OPENED BEFORE
      INQUIRE(FILE=FLNAMU,OPENED=OPENED,NUMBER=LFNTST,IOSTAT=IOSTAT)
      IF (IOSTAT .NE. 0) RETURN
      IF(OPENED) THEN
#ifdef GRP_AIUB
        IF (program_Name.NE.'GETKEY') THEN
          WRITE(*,'(/,A,/,16X,A,/)')
     1    ' ### SR OPNFIL: ' //
     2         'The file is still open. It will be closed now.',
     3         'File name: ' // TRIM(FILNAM)
        ENDIF
#endif
        CLOSE(UNIT=LFNTST)
      ENDIF
C
C  IS THE LFN STILL USED?
      INQUIRE(UNIT=LFN,OPENED=OPENED,NAME=FILTST,IOSTAT=IOSTAT)
      IF(OPENED) THEN
#ifdef GRP_AIUB
        IF (program_Name.NE.'GETKEY') THEN
          WRITE(*,'(/,A,I6,A,2(/,16X,A),/)')
     1    ' ### SR OPNFIL: A file with the LFN ',lfn,' is still open.',
     2         'whereas this number is used to open another file.',
     3         'File name: ' // TRIM(FILTST)
        ENDIF
#endif
        CLOSE(UNIT=LFN)
      ENDIF
C
      TEST=STATUS
      IF (inpKey%status .LE. myStatus_Run) CALL UPPERC(TEST)
      IF(TEST.EQ.'NEW') THEN
        ST='UNKNOWN'
C  DEFAULT
      ELSEIF (TEST.EQ.' ') THEN
        ST='UNKNOWN'
      ELSEIF (TEST.EQ.'UNKNOWN') THEN
        ST=TEST
      ELSEIF (TEST.EQ.'SCRATCH') THEN
        ST=TEST
      ELSEIF (TEST.EQ.'OLD') THEN
        ST=TEST
      ELSE
        GOTO 920
      END IF
C
      TEST=FORM
      IF (inpKey%status .LE. myStatus_Run) CALL UPPERC(TEST)
      IF(TEST.EQ.'FORMATTED') THEN
        FO=TEST
C DEFAULT
      ELSEIF (TEST.EQ.' ') THEN
        FO='FORMATTED'
      ELSEIF (TEST.EQ.'UNFORMATTED') THEN
        FO=TEST
      ELSEIF (TEST.EQ.'BINARY') THEN
        FO=TEST
      ELSE
        GOTO 920
      END IF
C
C  READONLY OR SHARED READING
C
      TEST=ACTION
      IF (inpKey%status .LE. myStatus_Run) CALL UPPERC(TEST)
      IF(TEST.EQ.'READONLY') THEN
        AC='READ'
      ELSEIF (TEST.EQ.' ') THEN
        AC=' '
      ELSE
        GOTO 920
      END IF
C
C  OPEN FILE
C  ---------
      DO IRERUN=1,10000
C
#ifdef OS_UNIX
        IF(FO.NE.'BINARY') THEN
          OPEN(UNIT=LFN,FILE=FLNAMU,STATUS=ST,FORM=FO,IOSTAT=IOSTAT)
        ELSE

CC SIMULATE "BINARY" WITH DIRECT ACCESS
#ifdef OS_DEC_OSF1
          LRECL=512
#else
          LRECL=128
#endif
          OPEN(UNIT=LFN,FILE=FLNAMU,STATUS=ST,FORM='UNFORMATTED',
     1         ACCESS='DIRECT',RECL=LRECL,IOSTAT=IOSTAT)

        END IF
#ifdef OS_IBM_AIX
        IF (ST.EQ.'OLD'.AND.IOSTAT.EQ.0.AND.FO.NE.'BINARY') REWIND LFN
#endif
#endif

#ifdef OS_WIN32
        IF(FO.EQ.'FORMATTED') THEN
          IF(AC.EQ.' ') THEN
            OPEN(UNIT=LFN,FILE=FLNAMU,STATUS=ST,FORM=FO,IOSTAT=IOSTAT)
          ELSE
            OPEN(UNIT=LFN,FILE=FLNAMU,STATUS=ST,FORM=FO,IOSTAT=IOSTAT,
     1           ACTION=AC)
          END IF
        ELSEIF(FO.EQ.'UNFORMATTED') THEN
          IF(AC.EQ.' ') THEN
            OPEN(UNIT=LFN,FILE=FLNAMU,STATUS=ST,FORM=FO,IOSTAT=IOSTAT)
          ELSE
            OPEN(UNIT=LFN,FILE=FLNAMU,STATUS=ST,ACTION=AC,
     1           FORM=FO,IOSTAT=IOSTAT)
          END IF
        ELSEIF(FO.EQ.'BINARY') THEN
          IF(AC.EQ.' ') THEN
            OPEN(UNIT=LFN,FILE=FLNAMU,STATUS=ST,FORM='UNFORMATTED',
     1           IOSTAT=IOSTAT)
          ELSE
            OPEN(UNIT=LFN,FILE=FLNAMU,STATUS=ST,FORM='UNFORMATTED',
     1           ACTION=AC,IOSTAT=IOSTAT)
          END IF
        END IF
#endif
C
C END OF REPEATING THE OPEN STATEMENT
        IF (IOSTAT.EQ.0) THEN
          IF (IRERUN.NE.1.AND.
     1        program_Name.NE.'GETKEY') THEN
            WRITE(*,'(/,A,/,16X,A,/,16X,A,I5,A,/)')
     1      ' ### SR OPNFIL: Problem to access file for opening',
     2           'File name : ' // TRIM(FLNAMU),
     3           'Successful after ',IRERUN,' open commands'
          ENDIF
          EXIT
        ENDIF
      ENDDO
C
C SET SPECIAL HANDLING OF EXITRC BACK
      inpKey%isOPNFIL = .FALSE.
      RETURN
C
920   WRITE(*,921) TEST,LFN,FLNAMU
921   FORMAT(/,' SR OPNFIL: UNKNOWN KEYWORD    : ',A,
     1       /,'            LOGICAL FILENUMBER : ',I2,
     2       /,'            FILENAME           : ',A,/)
      CALL EXITRC(2)
C
      END SUBROUTINE

      END MODULE
