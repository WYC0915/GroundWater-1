      MODULE s_RDSATH
      CONTAINS

C*
      SUBROUTINE RDSATH(FILCLK,LFN   ,TITLE )
CC
CC NAME       :  RDSATH
CC
CC PURPOSE    :  READ HEADER OF SATELLITE CLOCK FILE
CC
CC PARAMETERS :
CC        IN  :  FILCLK : SATELLITE CLOCK FILE NAME             CH*32
CC               LFN    : LOGICAL FILE NUMBER FOR SAT.CLK FILE   I*4
CC                        = -1: USE LFNLOC AS LFN AND CLOSE FILE
CC                              AFTER READING THE HEADER
CC                        >= 0: LFN IS USED FOR OPENING THE FILE
CC                              AND FILE STAYS OPEN AFTER READING
CC                              THE HEADER
CC        OUT :  TITLE  : TITLE IN SATELLITE CLOCK FILE         CH*80
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  19-AUG-98
CC
CC CHANGES    :  21-JUN-05 : MM: COMLFNUM.inc REMOVED, M_BERN ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1998     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE M_BERN
      USE s_opnfil
      USE s_opnerr
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IOSTAT, LFN   , LFNHLP
C
CCC       IMPLICIT REAL*8(A-H,O-Z)
C
      CHARACTER*80  TITLE
      CHARACTER*32  FILCLK
C
C
C LOGICAL FILE NUMBER
C -------------------
      LFNHLP=LFN
      IF (LFN.EQ.-1) THEN
        LFNHLP=LFNLOC
      ENDIF
C
C OPEN SATELLITE CLOCK FILE
C -------------------------
      CALL OPNFIL(LFNHLP,FILCLK,'OLD','FORMATTED',
     1            'READONLY',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNHLP,IOSTAT,FILCLK,'RDSATH')
C
C READ TITLE LINES AND SUBDAILY ERP MODEL NAME
C --------------------------------------------
      READ(LFNHLP,1,END=910,ERR=920) TITLE
1     FORMAT(A80,////)
C
C CLOSE FILE IF LFN = -1
C ----------------------
      IF (LFN.EQ.-1)  CLOSE(UNIT=LFNHLP)
      GOTO 999
C
C END OF FILE REACHED
C -------------------
910   WRITE(LFNERR,911) FILCLK
911   FORMAT(/,' *** SR RDSATH: UNEXPECTED END OF FILE',
     1       /,16X,'FILE NAME: ',A,/)
      CALL EXITRC(2)
C
C ERROR READING FILE
C ------------------
920   WRITE(LFNERR,921) FILCLK
921   FORMAT(/,' *** SR RDSATH: ERROR READING FILE',
     1       /,16X,'FILE NAME: ',A,/)
      CALL EXITRC(2)
C
C END
C ---
999   RETURN
      END SUBROUTINE

      END MODULE
