      MODULE s_GTOCNL
      CONTAINS

C*
      SUBROUTINE GTOCNL(STANAM,OCNAMP,OCNPHS,IRCOCN)
CC
CC NAME       :  GTOCNL
CC
CC PURPOSE    :  READ OCEAN LOADING TABLE FOR STATION
CC
CC PARAMETERS :
CC         IN :  STANAM: STATION NAME                          CH*16
CC         OUT:  OCNAMP: OCEAN LOADING AMPLITUDES (M)           R*8(3,MAXOCN,*)
CC               OCNPHS: OCEAN LOADING PHASES   (RAD)           R*8(3,MAXOCN,*)
CC               IRCOCN: OCEAN LOADING RETURN CODE              I*4
CC                       =0 : O.K.
CC                       =1 : NO OCNLOAD FILE FOUND
CC                       =2 : STATION NOT FOUND IN OCNLOAD FILE
CC
CC REMARKS    :  --
CC
CC AUTHOR     :  T.A. SPRINGER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  10-MAR-98
CC
CC CHANGES    :  10-MAR-98 : TS: CREATED
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               01-APR-03 : SS: STOP IN CASE OF READING ERROR
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1998     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE m_maxdim, ONLY: MAXOCN
      USE d_const, ONLY: PI
      USE s_opnfil
      USE s_exitrc
      USE s_gtflna
      USE s_opnerr
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IOSTAT, IRCOCN, J
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C
      CHARACTER*32 FILNAM
      CHARACTER*16 STANAM
      CHARACTER*14 OLSTAT
      CHARACTER*10 OLNUMB
      CHARACTER*4  OLNAME
C
      REAL*8    OCNAMP(3,MAXOCN)
      REAL*8    OCNPHS(3,MAXOCN)
C
C
C GET OCEAN LOADING TABLES FOR STATION
C ------------------------------------
      CALL GTFLNA(0,'OCNLOAD',FILNAM,IRCOCN)
      IF (IRCOCN.EQ.0) THEN
        CALL OPNFIL(LFNLOC,FILNAM,'OLD','FORMATTED','READONLY',' ',
     1              IOSTAT)
        CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILNAM,'GTOCNL')
        IRCOCN=2
C
C READ OCEAN LOADING VALUES FOR STATION IN OCEAN LOADING FILE
C -----------------------------------------------------------
20      READ(LFNLOC,21,ERR=20,END=40)OLNAME
21      FORMAT(2X,A4)
        IF (OLNAME.NE.STANAM(1:4)) GOTO 20
        READ(LFNLOC,22,ERR=20,END=40)OLNUMB
22      FORMAT(//,2X,A10)
        OLSTAT=OLNAME//OLNUMB
        IF (OLSTAT.EQ.STANAM(1:14) .OR. OLNAME.EQ.OLNUMB(2:5)) THEN
          READ(LFNLOC,23,ERR=60,END=60)(OCNAMP(3,I),I=1,MAXOCN),
     1                                 (OCNAMP(2,I),I=1,MAXOCN),
     2                                 (OCNAMP(1,I),I=1,MAXOCN),
     3                                 (OCNPHS(3,I),I=1,MAXOCN),
     4                                 (OCNPHS(2,I),I=1,MAXOCN),
     5                                 (OCNPHS(1,I),I=1,MAXOCN)
23        FORMAT(3(1X,11(F7.5),/),3(1X,11(F7.1),/))
          GOTO 50
C
60        WRITE(LFNERR,901) STANAM,FILNAM
901       FORMAT(/,' *** SR GTOCNL: ERROR READING OCEAN LOADING ',
     1                             'CORRECTION VALUES',/,
     2                         16X,'STATION NAME : ',A16,/,
     3                         16X,'FILE NAME    : ',A32,/)
          CALL EXITRC(2)
C
C CONVERT PHASES TO RADIANS
C -------------------------
50        CONTINUE
          DO 30 I=1,3
            DO 30 J=1,MAXOCN
              OCNPHS(I,J)=OCNPHS(I,J)*PI/180.D0
30        CONTINUE
          IRCOCN=0
        ELSE
          GOTO 20
        ENDIF
C
C STATION NAME NOT FOUND IN OCEAN LOADING FILE
C --------------------------------------------
40      CONTINUE
        IF (IRCOCN.NE.0) THEN
          WRITE(LFNERR,902) IRCOCN,STANAM,FILNAM
902       FORMAT(/,' ### SR GTOCNL: OCEAN LOADING CORRECTION VALUES ',
     1                             'NOT FOUND',/,
     2                         16X,'RETURN CODE  : ',I2,/,
     3                         16X,'STATION NAME : ',A16,/,
     4                         16X,'FILE NAME    : ',A32,/)
CC          CALL EXITRC(2)
          IRCOCN=0
          DO I=1,MAXOCN
            OCNAMP(1,I)=0.D0
            OCNAMP(2,I)=0.D0
            OCNAMP(3,I)=0.D0
            OCNPHS(1,I)=0.D0
            OCNPHS(2,I)=0.D0
            OCNPHS(3,I)=0.D0
          ENDDO
        ENDIF
        CLOSE(LFNLOC)
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
