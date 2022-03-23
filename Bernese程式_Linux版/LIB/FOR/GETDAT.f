      MODULE s_GETDAT
      CONTAINS
C*
      SUBROUTINE GETDAT(DATUM,AELL,BELL,DXELL,DRELL,SCELL,TYPE)
CC
CC NAME       :  GETDAT
CC
CC PURPOSE    :  GET LOCAL GEODETIC DATUM FROM DATUM FILE
CC
CC PARAMETERS :
CC         IN :  DATUM  : LOCAL GEODETIC DATUM                CH*16
CC        OUT :  AELL   : SEMI-MAJOR AXIS OF ELLIPSOID        R*8
CC               BELL   : SEMI-MINOR AXIS OF ELLIPSOID        R*8
CC               DXELL(I),I=1,2,3: SHIFTS TO WGS-84 IN METERS R*8
CC               DRELL(I),I=1,2,3: ROTATIONS TO WGS-84 IN     R*8
CC                        RADIAN
CC               SCELL  : SCALE FACTOR TO WGS-84              R*8
CC               TYPE   : FRAME TYPE (GLOBAL/REGIONAL/LOCAL)  CH*16
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC CREATED    :  87/11/19 08:34
CC
CC CHANGES    :  28-DEC-92 : USE OF SR "OPNFIL" TO OPEN FILES
CC               10-AUG-94 : MR: CALL EXITRC
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-FEB-07 : AG: USE 206264... FROM DEFCON
CC               19-DEC-11 : SL: NEW PARAMETER TYPE, M_BERN W/ ONLY, SRNAME
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnLoc, lfnErr
      USE d_const,  ONLY: ars
      USE s_exitrc
      USE s_opnfil
      USE s_gtflna
      USE s_opnerr
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IOSTAT, IRC
C
      REAL*8    AELL  , BELL  , FINV  , SCELL
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*32 DATFIL
      CHARACTER*16 DATUM,DATUM1,TYPE1
      REAL*8       DXELL(3),DRELL(3)
C
      CHARACTER(LEN=16), OPTIONAL     :: TYPE
C
      CHARACTER(LEN=6), PARAMETER     :: srName = 'GETDAT'
C
C OPEN FILE WITH LOCAL GEODETIC DATUM INFORMATION
C -----------------------------------------------
      CALL GTFLNA(1,'DATUM  ',DATFIL,IRC)
      CALL OPNFIL(LFNLOC,DATFIL,'OLD','FORMATTED',
     1            'READONLY',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNLOC,IOSTAT,DATFIL,srName)
C
      READ(LFNLOC,2)
2     FORMAT(///)
10    READ(LFNLOC,3,END=30) DATUM1,AELL ,DXELL(1),DRELL(1),
     1                      TYPE1, FINV ,DXELL(2),DRELL(2),
     2                             SCELL,DXELL(3),DRELL(3)
3     FORMAT(A16,7X,F12.3,7X,F12.4,7X,F10.4,/,
     1       A16,7X,F12.7,7X,F12.4,7X,F10.4,/,
     2          23X,D12.4,7X,F12.4,7X,F10.4,/)
      IF(DATUM.NE.DATUM1) GOTO 10
      IF(PRESENT(TYPE)) THEN
        TYPE = ADJUSTL(TYPE1)
        IF(LEN_TRIM(TYPE) == 0) THEN
          WRITE(lfnErr,'(/,1X,A,A,A,/,16X,A,2(/,16X,A,A),/)')
     1      '*** SR ',srName,': FRAME TYPE (GLOBAL/REGIONAL/LOCAL) ',
     1                         'HAS TO BE SPECIFIED',
     1                         'FILE  : ',DATFIL,
     1                         'DATUM : ',TRIM(DATUM)
          CALL EXITRC(2)
        ENDIF
      ENDIF
C
      BELL=AELL*(1-1/FINV)
      SCELL=1.D0+SCELL
      DO 20 I=1,3
        DRELL(I)=DRELL(I)/ars
20    CONTINUE
      CLOSE(UNIT=LFNLOC)
      GOTO 999
C
C DATUM NOT FOUND
30    WRITE(LFNERR,4) DATUM
4     FORMAT(/,' *** SR GETDAT: LOCAL GEODETIC DATUM NOT FOUND',/,
     1                     16X,'DATUM: ',A16,/)
      CALL EXITRC(2)
C
999   RETURN
C
      END SUBROUTINE
C
      END MODULE
