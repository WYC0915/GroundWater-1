      MODULE s_EXTGIM
      CONTAINS

C*
      SUBROUTINE EXTGIM(NFIL,FILNAM,FILTYP)
CC
CC NAME       :  EXTGIM
CC
CC PURPOSE    :  EXTRACT ESSENTIAL INFORMATION CONCERNING GLOBAL
CC               IONOSPHERE MODELS FROM GPSEST OUTPUT FILES
CC
CC PARAMETERS :
CC         IN :  NFIL   : TOTAL NUMBER OF OUTPUT FILES        I*4
CC               FILNAM(I),I=1,..,NFIL: LIST OF FILE NAMES    CH*32(*)
CC               FILTYP(I),I=1,..,NFIL: OUTPUT FILE INDEX     I*4(*)
CC                        = 0: BAD OUTPUT FILE
CC                        = 1: GPSEST
CC                        = 2: ADDNEQ
CC                        = 3: ADDNEQ2
CC                        =11: GPSEST (V5.0)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S.SCHAER
CC
CC VERSION    :  4.0
CC
CC CREATED    :  29-DEC-95
CC
CC CHANGES    :  03-JAN-96 : SS: USE "FILTYP"
CC               04-APR-96 : MR: PUT LABEL 310 INTO DO LOOP
CC               05-JUN-96 : LM: DATA STATEMENT OUT OF ORDER
CC               26-JAN-98 : SS: STATION-SPECIFIC GIMS
CC               14-MAR-00 : SS: CALL OF SR FINDLN WITH "NLIN=0"
CC               20-DEC-00 : SS: VARIABLE "TYPOUT" REMOVED
CC               28-MAR-02 : SS: EXTRACT ADDNEQ2 GIM RESULTS
CC               11-DEC-02 : CU: GPSEST V5.0 OUTPUT
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1995     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE s_opnfil
      USE s_opnerr
      USE s_findln
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IERR  , IFIL  , IL    , IOSTAT, IPGM  , IPOS  , IRCGIM,
     1          NFIL
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*132 LINE,LOOK
      CHARACTER*32  FILNAM(*),FILGIM,FILOUT
C
      INTEGER*4     FILTYP(*)
C
C
C RETURN, IF GIM SUMMARY FILE NOT REQUESTED
C -----------------------------------------
      CALL GTFLNA(0,'GIMOUT ',FILGIM,IRCGIM)
      IF (IRCGIM.EQ.1) GOTO 999
C
C OPEN GIM SUMMARY FILE
C ---------------------
      CALL OPNFIL(LFN001,FILGIM,'UNKNOWN','FORMATTED',
     1  ' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFN001,IOSTAT,FILGIM,'EXTGIM')
C
C WRITE HEADER LINES
C ------------------
      WRITE(LFN001,910)
910   FORMAT(' MODEL / STATION     MIN LAT  MAX LAT   ',
     1  'MEAN TEC  RMS ERR   ',
     2  'MAX TEC  RMS ERR  LAT     LON       ',
     3  'MIN TEC  RMS ERR  LAT     LON',/,
     4  '                     (DEG)    (DEG)     ',
     5  '(TECU)    (TECU)    ',
     6  '(TECU)   (TECU)   (DEG)   (DEG)     ',
     7  '(TECU)   (TECU)   (DEG)   (DEG)',/,
     8  1X,131('-'))
C
C LOOP OVER ALL GPSEST/ADDNEQ OUTPUT FILES
C ----------------------------------------
      DO 110 IFIL=1,NFIL
C
C SKIP BAD OUTPUT FILE
        IPGM=FILTYP(IFIL)
        IF (IPGM.EQ.0) GOTO 110
C
C OPEN GPSEST/ADDNEQ OUTPUT FILE
        FILOUT=FILNAM(IFIL)
        CALL OPNFIL(LFNLOC,FILOUT,'OLD','FORMATTED',
     1    'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILOUT,'EXTGIM')
C
C LOOK FOR ESSENTIAL GIM INFORMATION
        IF (IPGM.EQ.1.OR.IPGM.EQ.11) THEN
          LOOK='MIN LAT  MAX LAT   MEAN TEC  RMS ERR'
        ELSE
          LOOK='Min lat  Max lat   Mean TEC  RMS err'
        ENDIF
        IPOS=0
        CALL FINDLN(LFNLOC,0,0,'EXTGIM',FILOUT,LOOK,IPOS,LINE,IERR)
        IF (IERR.EQ.1) GOTO 310
C
        IF (IPGM.EQ.1.OR.IPGM.EQ.11) THEN
          READ(LFNLOC,'(//)')
        ELSE
          READ(LFNLOC,'(/)')
        ENDIF
C
C WRITE EXTRACTED INFORMATION INTO SUMMARY FILE
        DO 220 IL=1,100000
          READ(LFNLOC,'(A)',END=105) LINE
C
C CONVERT OLD OUTPUT LINES
          IF (IPOS.EQ.13) LINE=LINE(1:8)//'         '//LINE(9:120)
C
          IF (LINE.NE.' ') THEN
            WRITE(LFN001,'(A)') LINE
          ELSE
            GOTO 105
          ENDIF
220     CONTINUE
        GOTO 105
C
310     WRITE(LFNERR,920) FILOUT
920     FORMAT(/,' ### SR EXTGIM: NO GIM INFORMATION FOUND',
     1    /,16X,'FILE NAME: ',A,/)
C
105     CLOSE(LFNLOC)
C
110   CONTINUE
C
C CLOSE GIM SUMMARY FILE
C ----------------------
      WRITE(LFN001,*)
      CLOSE (LFN001)
C
999   RETURN
      END SUBROUTINE

      END MODULE
