      MODULE s_PRISTA
      CONTAINS
C*
      SUBROUTINE PRISTA(NSTAT,STNAME,STANUM,XSTAT,XSTELL,XSTECC,
     1                  NCENTR,ICENTR,NFIX,STFIX,NFREE,STFREE,TIMREF,
     2                  DATUM)
CC
CC NAME       :  PRISTA
CC
CC PURPOSE    :  PRINT A PRIORI STATION COORDINATES AND STATION
CC               ECCENTRICITIES
CC
CC PARAMETERS :
CC         IN :  NSTAT  : NUMBER OF STATIONS INVOLVED         I*4
CC               STNAME(I),I=1,..,NSTAT: STATION NAMES        CH*16
CC               STANUM(I),I=1,..,NSTAT: EXT. STATION NUMBERS I*4
CC               XSTAT(3,I),I=1,..,NSTAT: CARTES. COORDINATES R*8
CC                        IN WGS-84
CC               XSTELL(3,I),I=1,..,NSTAT: ELLIPSOIDAL COORD. R*8
CC                        IN LOCAL GEODETIC DATUM
CC               XSTECC(3,I),I=1,..,NSTAT: STATION ECCENTR.   R*8
CC                        IN WGS-84 SYSTEM (METERS)
CC               NCENTR : NUMBER OF CENTER STATIONS NOT       I*4
CC                        DIRECTLY OBSERVED
CC               ICENTR(I),I=1,..,NSTAT: NUMBER OF CENTER     I*4
CC                        STATION BELONGING TO STATION I
CC               NFIX   : NUMBER OF FIXED STATIONS (CENTERS)  I*4
CC               STFIX(I),I=1,..,NFIX: NUMBERS OF FIXED STAT. I*4
CC               NFREE  : NUMBER OF STATIONS (FREE NETW.)     I*4
CC               STFREE(I),I=1,..,NFREE: NUMBERS OF STAT. (FREE) I*4
CC               TIMREF : REFERENCE TIME                      R*8
CC               DATUM  : Local geodetic datum                CH*16
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC CREATED    :  87/11/19 15:36
CC
CC CHANGES    :  27-MAY-91 : ??: DON'T PRINT TRAILING BLANKS
CC               23-SEP-91 : ??: INCLUDE COORD.FILENAME INTO OUTPUT
CC               07-SEP-01 : RD: HAVE A VERSION WITHOUT SKELETON FILE
CC                               ADD FREE NETWORK AS ADDITIONAL FLAG
CC               25-SEP-02 : HU: REMOVE I_ASTLIB
CC               10-DEC-02 : CU: DON'T USE LFNPLT (SKELETON FILE),
CC                               CHANGE FORMAT OF TITLE LINES
CC               24-NOV-03 : HU: DO NOT PRINT COORDINATES FOR LEO
CC               21-JUN-05 : MM: COMLFNUM.INC REMOVED, M_BERN ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               03-FEB-11 : SL: M_BERN WITH ONLY, PRINT 5 DIGITS INSTEAD OF 4
CC                               NEW PARAMETER DATUM
CC               15-FEB-11 : SL: LON/LAT IN DECIMAL NOTATION
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC               UNIVERSITY OF BERN
CC               SWITZERLAND
C*
      USE m_bern,   ONLY: lfnPrt
      USE d_stacrx, ONLY: MTypeSPACE

      USE s_staflg
      USE s_eccell
      USE s_radgms
      USE s_gtflna

      IMPLICIT NONE

C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IC    , IDEG1 , IDEG2 , IFIRST, IFIX  , IFLAG ,
     1          IFREE , IMIN1 , IMIN2 , IRC   , ISTAT , NCENTR, NFIX  ,
     2          NFREE , NSTAT
C
      REAL*8    SEC1  , SEC2  , TIMREF
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*132 TEXT
      CHARACTER*16  STNAME(*)
      CHARACTER*5   ESTFIX
      CHARACTER*1   OBS,SIGN1,SIGN2
      REAL*8        XSTAT(3,*),XSTELL(3,*),XSTECC(3,*),LOCECC(3)
      INTEGER*4     STANUM(*),ICENTR(*),STFIX(*),STFREE(*)
      CHARACTER*20  MARTYP
      CHARACTER*16,OPTIONAL :: DATUM

C WRITE TITLE LINES
C -----------------
      CALL GTFLNA(0,'COORD  ',TEXT,IRC)
      IF(.NOT.PRESENT(DATUM)) DATUM = 'WGS - 84        '
      WRITE(lfnPrt,1)
     2  'A priori station coordinates:',TRIM(TEXT),
     2  'A priori station coordinates','A priori station coordinates',
     2  DATUM,'Ellipsoidal in local geodetic datum',
     2  'num','Station name    ','obs','e/f/h',
     2  'X (m)','Y (m)','Z (m)',
     2  'Latitude','Longitude','Height (m)'
1     FORMAT(1X,A,14X,A,//,44X,A,17X,A,/,54X,A16,15X,A,/,132('-'),/,
     1       1X,A3,2X,A16,1X,A3,1X,A5,1X,3(7X,A5,4X),2(3X,A9,4X),A10,
     1       /,132('-'))

C A PRIORI STATION COORDINATES: CENTER STATIONS
C ---------------------------------------------
      DO 40 ISTAT=1,NSTAT
        CALL STAFLG(STNAME(ISTAT),TIMREF,IFLAG,MARTYP)
        IF(MARTYP.EQ.MTYPESPACE)   GOTO 40
        IF(ICENTR(ISTAT).NE.ISTAT) GOTO 40
C
C STATION DIRECTLY OBSERVED ?
        IF(ISTAT.GT.NSTAT-NCENTR) THEN
          OBS='N'
        ELSE
          OBS='Y'
        ENDIF
C
C STATION FIXED ?
        ESTFIX='ESTIM'
        DO 20 IFIX=1,NFIX
          IF(STFIX(IFIX).EQ.ISTAT) THEN
            ESTFIX='FIXED'
            GOTO 30
          ENDIF
20      CONTINUE
30      CONTINUE
        DO 25 IFREE=1,NFREE
          IF(STFREE(IFREE).EQ.ISTAT) THEN
            ESTFIX='HELMR'
            GOTO 35
          ENDIF
25      CONTINUE
35      CONTINUE
C
        CALL RADGMS(1,XSTELL(1,ISTAT),SIGN1,IDEG1,IMIN1,SEC1)
        CALL RADGMS(1,XSTELL(2,ISTAT),SIGN2,IDEG2,IMIN2,SEC2)
        WRITE(LFNPRT,2) STANUM(ISTAT),STNAME(ISTAT),OBS,ESTFIX,
     1                  (XSTAT(I,ISTAT),I=1,3),
     1                  SIGN(IDEG1+(IMIN1+SEC1/60)/60,XSTELL(1,ISTAT)),
     1                  SIGN(IDEG2+(IMIN2+SEC2/60)/60,XSTELL(2,ISTAT)),
     1                  XSTELL(3,ISTAT)
2       FORMAT(I4,2X,A16,2X,A1,2X,A5,1X,3(F15.5,1X),
     1         2(F14.7,1X),F12.5)
40    CONTINUE

C A PRIORI ECCENTER STATION COORDINATES
C -------------------------------------
      OBS='Y'
      ESTFIX='ECCEN'
C
      IFIRST=1
      DO 50 ISTAT=1,NSTAT-NCENTR
        IF(ICENTR(ISTAT).EQ.ISTAT) GOTO 50
        IF(IFIRST.EQ.1) THEN
          WRITE(LFNPRT,3)
3         FORMAT(' ')
          IFIRST=0
        ENDIF
        CALL RADGMS(1,XSTELL(1,ISTAT),SIGN1,IDEG1,IMIN1,SEC1)
        CALL RADGMS(1,XSTELL(2,ISTAT),SIGN2,IDEG2,IMIN2,SEC2)
        WRITE(LFNPRT,2) STANUM(ISTAT),STNAME(ISTAT),OBS,ESTFIX,
     1                  (XSTAT(I,ISTAT),I=1,3),
     1                  SIGN(IDEG1+(IMIN1+SEC1/60)/60,XSTELL(1,ISTAT)),
     1                  SIGN(IDEG2+(IMIN2+SEC2/60)/60,XSTELL(2,ISTAT)),
     1                  XSTELL(3,ISTAT)
50    CONTINUE

C STATION ECCENTRICITIES
C ----------------------
C
      IFIRST=1
      DO 70 ISTAT=1,NSTAT-NCENTR
        IC=ICENTR(ISTAT)
        IF(IC.EQ.ISTAT) GOTO 70
        IF(IFIRST.EQ.1) THEN
C
C TITLE LINES
          CALL GTFLNA(0,'ECCENT ',TEXT,IRC)
          WRITE(LFNPRT,'(//,A,/,5(/,A))')
     1      ' A priori station eccentricities:           '// TRIM(TEXT),
     2      '                                           A priori ' //
     2      'station eccentricities',
     3      '                                               in'    //
     3      ' local geodetic datum',
     4      ' ---------------------------------------------------' //
     4      '----------------------------------------------------' //
     4      '----------------------------',
     5      ' num  Station name      Center name         N (m)   ' //
     5      '    E (m)       U (m) ',
     6      ' ---------------------------------------------------' //
     6      '----------------------------------------------------' //
     6      '----------------------------'
          IFIRST=0
        ENDIF
C
C TRANSFORM ECCENTRICITIES INTO LOCAL ELLIPSOIDAL SYSTEM
        CALL ECCELL(XSTELL(1,IC),XSTECC(1,ISTAT),LOCECC)
        WRITE(LFNPRT,4) STANUM(ISTAT),STNAME(ISTAT),STNAME(IC),
     1                  LOCECC
4       FORMAT(I4,2X,A16,2X,A16,3F11.5)
70    CONTINUE
C
      WRITE(LFNPRT,'(/)')
C
      RETURN

      END SUBROUTINE

      END MODULE
