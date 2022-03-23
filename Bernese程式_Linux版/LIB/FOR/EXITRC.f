      MODULE s_EXITRC
      CONTAINS

C*
      SUBROUTINE EXITRC(IRCODE)
CC
CC NAME       : EXITRC
CC
CC PURPOSE    : USER EXIT ROUTINE
CC
CC PARAMETERS :
CC         IN : IRCODE : RETURN CODE                                 I*4
CC                       0,1 : NORMAL EXIT
CC                       ELSE: ERROR  EXIT
CC
CC REMARKS    : VAX VERSION:
CC
CC                NORMAL EXIT:  CALL EXIT(1)
CC                ERROR  EXIT:  CALL EXIT(3)
CC
CC                EXIT SETS THE LOGICAL $SEVERITY TO 1 OR 3 RESPECTI-
CC                VELY.
CC                TEST OF $SEVERITY .GE. 2 WILL REACT TO SEVERITY CODES
CC                FROM THE SYSTEM (2=ERROR, 4=SEVERE ERROR) AND FROM
CC                THIS ROUTINE (3).
CC
CC              DOS VERSION AND UNIX VERSIONS:
CC
CC                NORMAL EXIT:  CALL EXIT(0)
CC                ERROR  EXIT:  CALL EXIT(1)
CC
CC
CC AUTHOR     : W. GURTNER
CC              ASTRONOMICAL INSTITUTE, UNIVERSITY OF BERN
CC              SWITZERLAND
CC
CC CREATED    : 11-AUG-94
CC
CC CHANGES    : 23-AUG-94 : MR: SOURCE CONTAINS ALL THREE VERSIONS
CC              07-MAY-96 : MR: USE SUBROUTINE "SYSTYP"
CC              04-JUN-96 : MR: USE SYSTYP ONLY FOR UNIX
CC              12-NOV-96 : MR: CLOSE ALL OPEN FILES (AIX PROBLEM)
CC                              USE EXITCC C-SUBROUTINE FOR UNIX
CC              06-FEB-03 : RD: SET MENUAUX_IRCODE
CC              19-FEB-03 : RD: REMEMBER INP FILE NAME FOR MENUAUX
CC              10-MAR-03 : RD: TRY TO OPEN INP-FILE FOR MENUAUX
CC              15-MAY-03 : HU: INITIALIZE STRUCTURES
CC              14-AUG-03 : RD: CLOSE OPENED FILE BEFORE READKEYS
CC              05-NOV-03 : RD: PUT MENUAUX STUFF INTO EXIT_MEN
CC                              ADD SR MENU_DEL TO DELETE SCRATCH FILES
CC              17-NOV-03 : RD: PUT ORIG. EXITRC INTO EXT_PGM (NO "PANIC-LOOP")
CC              23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC              23-SEP-10 : RD: "CANNOT_USE" STATEMENTS ADDED
CC
C*
      USE M_BERN,   ONLY: lfnerr, program_Name
      USE D_INPKEY, ONLY: inpKey, myStatus_Run, myStatus_Stop
C CANNOT_USE s_exit_men
C CANNOT_USE s_exit_del
C CANNOT_USE s_exit_pgm
C
C
C SET STATUS TO "STOP PROGRAM"
C ----------------------------
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IRCODE
C
      IF (inpKey%status .EQ. myStatus_Run .AND.
     1    .NOT. inpKey%isOPNFIL) THEN
        inpKey%status = myStatus_Stop
C
C WRITE THE RETURN CODE FOR MENUAUX
C ---------------------------------
        CALL exit_men(irCode)
C
C DELETE ALL SCRATCH FILES LISTED IN INPKEY%DELFIL
C ------------------------------------------------
        CALL exit_del
C
C WRITE A WARNING IF UN-NORMAL STOP
C ---------------------------------
      ELSE IF (program_Name .NE. 'GETKEY') THEN
        IF (inpKey%isOPNFIL) THEN
          WRITE(LFNERR,'(/,A,/)') ' ### SR EXITRC: ' //
     1    'PROGRAM HAS BEEN STOPPED IN OPNFIL'
        ELSE IF (inpKey%status .LT. myStatus_Run .AND.
     1           inpKey%status .GT. 0) THEN
          WRITE(LFNERR,'(/,A,/)') ' ### SR EXITRC: ' //
     1    'PROGRAM HAS BEEN STOPPED DURING INITIALIZATION'
        ELSE IF (inpKey%status .GT. myStatus_Run) THEN
          WRITE(LFNERR,'(/,A,/)')' ### SR EXITRC: ' //
     1    'PROGRAM HAS BEEN STOPPED DURING EXITING'
        ENDIF
      ENDIF
C
C  RETURN CODES
C  ------------
      inpKey%status = myStatus_Stop
      CALL exit_pgm(irCode)

      RETURN
      END SUBROUTINE

      END MODULE
