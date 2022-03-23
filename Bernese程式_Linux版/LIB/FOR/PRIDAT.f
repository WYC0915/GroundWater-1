      MODULE s_PRIDAT
      CONTAINS

C*
      SUBROUTINE PRIDAT(DATUM,AELL,BELL,DXELL,DRELL,SCELL)
CC
CC NAME       :  PRIDAT
CC
CC PURPOSE    :  PRINT LOCAL GEODETIC DATUM PARAMETERS
CC
CC PARAMETERS :
CC         IN :  TITLES(I),I=1,2: TITLE LINES                 CH*132
CC               DATUM  : LOCAL GEODETIC DATUM                CH*16
CC               AELL   : SEMI-MAJOR AXIS OF ELLIPSOID        R*8
CC               BELL   : SEMI-MINOR AXIS OF ELLIPSOID        R*8
CC               DXELL(I),I=1,3: SHIFTS TO WGS-84             R*8
CC               DRELL(I),I=1,3: ROTATIONS TO WGS-84          R*8
CC               SCELL  : SCALE FACTOR TO WGS-84              R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/18 18:32
CC
CC CHANGES    :  27-MAY-91 : ??: DON'T PRINT TRAILING BLANKS
CC               07-SEP-01 : RD: HAVE A VERSION WITHOUT SKELETON FILE
CC               25-SEP-02 : HU: REMOVE I_ASTLIB
CC               10-DEC-02 : CU: DON'T USE LFNPLT (SKELETON FILE), CHANGE
CC                               FORMAT OF TITLE LINES, PRINT DATUM FILE
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-FEB-07 : AG: USE 206264... FROM DEFCON
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
C
      USE m_bern
      USE d_const, ONLY: ars
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IRC
C
      REAL*8    AELL , BELL , FINV , SCELL
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16  DATUM
      CHARACTER*32  TEXT
      REAL*8        DXELL(3),DRELL(3)
C
C PRINT TITLE LINES
C -----------------
      CALL GTFLNA(0,'DATUM  ',TEXT,IRC)
      WRITE(LFNPRT,'(A,3(/,A))')
     1  ' Local geodetic datum:                      ' // TRIM(TEXT),
     2  ' ',
     3  ' Datum name         Ell. param./ Scale      Shifts to WG' //
     3  'S-84       Rotations to WGS-84',
     4  ' -------------------------------------------------------' //
     4  '--------------------------------------------------------' //
     4  '--------------------'
C
C LOCAL GEODETIC DATUM PARAMETERS
C -------------------------------
      FINV=AELL/(AELL-BELL)
      WRITE(LFNPRT,3) DATUM,AELL ,DXELL(1),DRELL(1)*ars,
     1                      FINV ,DXELL(2),DRELL(2)*ars,
     2                      SCELL-1.D0,DXELL(3),DRELL(3)*ars
3     FORMAT(' ',A16,3X,'A  =',F13.3,' m',5X,'DX =',F12.4,' m',5X,
     1                                       'RX =',F12.5,' arcsec',/,
     2              20X,'1/F=',F13.7,     7X,'DY =',F12.4,' m',5X,
     3                                       'RY =',F12.5,' arcsec',/,
     4              20X,'SC =',D13.5,     7X,'DZ =',F12.4,' m',5X,
     5                                       'RZ =',F12.5,' arcsec')
C
      WRITE(LFNPRT,'(/)')
C
      RETURN
      END SUBROUTINE

      END MODULE
