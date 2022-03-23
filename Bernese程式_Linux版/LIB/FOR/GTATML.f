      MODULE s_GTATML
      CONTAINS
C*
      SUBROUTINE GTATML(STANAM,ATMSIN,ATMCOS,IRCATM)
CC
CC NAME       :  GTATML
CC
CC PURPOSE    :  READ ATMOSPHERIC TIDAL LOADING TABLE FOR STATION
CC
CC PARAMETERS :
CC         IN :  STANAM: STATION NAME                          CH*16
CC         OUT:  ATMSIN: ATM-TIDAL LOADING SINE-TERM   (M)      R*8(3,MAXATM,*)
CC               ATMCOS: ATM-TIDAL LOADING COSINE-TERM (M)      R*8(3,MAXATM,*)
CC               IRCATM: ATM-TIDAL LOADING RETURN CODE          I*4
CC                       =0 : O.K.
CC                       =1 : NO ATMLOAD FILE FOUND
CC                       =2 : STATION NOT FOUND IN ATMLOAD FILE
CC
CC REMARKS    :  ADAPTED FROM GTOCNL.f
CC
CC AUTHOR     :  R. DACH
CC
CC VERSION    :  4.1
CC
CC CREATED    :  29-JUN-09
CC
CC CHANGES    :  29-JUN-09 : RD: CREATED
CC               16-SEP-10 : RD: CLOSE FILE
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1998     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE m_maxdim, ONLY: MAXATM
      USE d_const,  ONLY: PI
      USE s_opnfil
      USE s_exitrc
      USE s_gtflna
      USE s_opnerr
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IOSTAT, IRCATM, J
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C
      CHARACTER*32 FILNAM
      CHARACTER*16 STANAM
      CHARACTER*16 ALNAME
C
      REAL*8    ATMSIN(3,MAXATM)
      REAL*8    ATMCOS(3,MAXATM)
C
C
C GET OCEAN LOADING TABLES FOR STATION
C ------------------------------------
      CALL GTFLNA(0,'ATMLOAD',FILNAM,IRCATM)
      IF (IRCATM.EQ.0) THEN
        CALL OPNFIL(LFNLOC,FILNAM,'OLD','FORMATTED','READONLY',' ',
     1              IOSTAT)
        CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILNAM,'GTATML')
        IRCATM=2
C
C READ OCEAN LOADING VALUES FOR STATION IN OCEAN LOADING FILE
C -----------------------------------------------------------
20      READ(LFNLOC,21,ERR=20,END=40)ALNAME
21      FORMAT(A)
        IF (ALNAME.NE.STANAM) GOTO 20
        READ(LFNLOC,23,ERR=60,END=60)
     1      (ATMCOS(3,I),ATMSIN(3,I),I=1,MAXATM),
     2      (ATMCOS(1,I),ATMSIN(1,I),I=1,MAXATM),
     3      (ATMCOS(2,I),ATMSIN(2,I),I=1,MAXATM)
23      FORMAT(3(4(F12.4),/))
        GOTO 50
C
60      WRITE(LFNERR,901) STANAM,FILNAM
901     FORMAT(/,' *** SR GTATML: ERROR READING ATMOSPHERIC TIDAL ',
     1                           'LOADING CORRECTION VALUES',/,
     2                       16X,'STATION NAME : ',A16,/,
     3                       16X,'FILE NAME    : ',A32,/)
        CALL EXITRC(2)
C
C CONVERT FROM MILLIMETERS TO METERS
C ----------------------------------
50      CONTINUE
        DO 30 I=1,3
          DO 30 J=1,MAXATM
            ATMSIN(I,J)=ATMSIN(I,J)/1000.D0
            ATMCOS(I,J)=ATMCOS(I,J)/1000.D0
30      CONTINUE
        IRCATM=0
C
C STATION NAME NOT FOUND IN OCEAN LOADING FILE
C --------------------------------------------
40      CONTINUE
        IF (IRCATM.NE.0) THEN
          WRITE(LFNERR,902) IRCATM,STANAM,FILNAM
902       FORMAT(/,' ### SR GTATML: ATMOSPHERIC TIDAL LOADING ',
     1                             'CORRECTION VALUES NOT FOUND',/,
     2                         16X,'RETURN CODE  : ',I2,/,
     3                         16X,'STATION NAME : ',A16,/,
     4                         16X,'FILE NAME    : ',A32,/)
CC          CALL EXITRC(2)
          IRCATM=0
          DO I=1,MAXATM
            DO J=1,3
              ATMSIN(J,I)=0.D0
              ATMCOS(J,I)=0.D0
            ENDDO
          ENDDO
        ENDIF
        CLOSE(LFNLOC)
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
