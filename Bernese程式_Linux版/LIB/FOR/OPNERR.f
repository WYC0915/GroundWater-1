      MODULE s_OPNERR
      CONTAINS

C*
      SUBROUTINE OPNERR(LFNERR,LFNOPN,IOSTAT,FILNAM,SRNAME)
CC
CC NAME       :  OPNERR
CC
CC PURPOSE    :  PRINT ERROR MESSAGE ACCORDING TO "IOSTAT" IF A FILE
CC               COULD NOT BE OPENED
CC
CC PARAMETERS :
CC         IN :  LFNERR : LOGICAL FILE NUMBER FOR ERROR       I*4
CC                          MESSAGES
CC               LFNOPN : LOGICAL FILE NUMBER OF THE FILE     I*4
CC                          THAT WAS OPENED
CC               IOSTAT : RETURN CODE FROM "OPEN"-STATEMENT   I*4
CC               FILNAM : FILE NAME OF "OPEN"-STATEMENT       CH*(*)
CC               SRNAME : NAME OF SUBROUTINE THAT TRIED TO    CH*(*)
CC                          OPEN THE FILE
CC
CC REMARKS    :  YOU MAY SPECIFY DIFFERENT MESSAGES FOR DIFFERENT
CC               "IOSTAT"-VALUES IF YOU HAVE THE NECESSARY INFORMATION
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  89/10/07 13:16
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC               03-APR-96 : MR: UNIFY VERSIONS (LAHEY, UNIX, VMS)
CC               28-MAY-02 : HU: TRIM FILENAME
CC               17-FEB-03 : LM: USE PREPROCESSOR COMMANDS
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               29-OCT-12 : RD: REMOVE #ifdef OS_VMS
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1989     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE M_BERN, ONLY: program_Name
C
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IOST  , IOSTAT, LFNERR, LFNOPN
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      CHARACTER*(*) FILNAM
      CHARACTER*(*) SRNAME
C
C ERROR MESSAGE IF OPEN FAILED
C ----------------------------
#ifdef OS_UNIX
      IOST=IOSTAT
      IF (IOST.EQ.903) IOST=0
#endif
#ifdef OS_WIN32
      IOST=MOD(IOSTAT,256)
#endif
      IF(IOST.NE.0) THEN
        IF(LFNERR.EQ.0) THEN
          WRITE(*,101) trim(FILNAM),SRNAME,IOST,LFNOPN
        ELSE
          WRITE(LFNERR,101) trim(FILNAM),SRNAME,IOST,LFNOPN
        END IF
101     FORMAT(/,' *** SR OPNERR: OPEN FAILED',/,
     1                       16X,'FILE NAME : ',A,/,
     2                       16X,'PGM./SUBR.: ',A10,/,
     3                       16X,'IOSTAT    : ',I6,/,
     3                       16X,'FILE UNIT : ',I6,/)
        CALL EXITRC(2)
      ENDIF
C
Cccccccccccccccccccccccccccccccccccccc
C
C Temporary for debugging only!!!!
C
C Report all file sucessfully opened
C ----------------------------------
      IF (1.EQ.2.AND.
     1    program_Name.NE.'GETKEY') THEN
        write(*,'(/,A,I6,A,/,16X,A,/)') ' +++ SR OPNERR: ' //
     1      'File was opened on LFN ',lfnopn,' by SR/PG '//TRIM(SRNAME),
     2      'File name: ' // TRIM(filnam)
      ENDIF
Cccccccccccccccccccccccccccccccccccccc
      RETURN
      END SUBROUTINE

      END MODULE
