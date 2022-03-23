      MODULE s_PRISTS
      CONTAINS

C*
      SUBROUTINE PRISTS(NSTWGT,ISTWGT,STWGT,NVEWGT,IVEWGT,VEWGT,
     1                  STNAME,STANUM,WGTFILE,PGM)
CC
CC NAME       :  PRISTS
CC
CC PURPOSE    :  PRINT A PRIORI SIGMAS FOR STATION COORDINATES
CC
CC PARAMETERS :
CC         IN :  NSTWGT : NUMBER OF STATIONS WITH A PRIORI    I*4
CC                        SIGMAS
CC               ISTWGT(I),I=1,..,NSTWGT: STATION NUMBERS     I*4
CC               STWGT(3,I),I=1,..,NSTWGT: A PRIORI SIGMAS    R*8
CC               NVEWGT : NUMBER OF VELOCITIES WITH A PRIORI  I*4
CC                        SIGMAS
CC               IVEWGT(I),I=1,..,NSTWGT: VELOCITY NUMBERS    I*4
CC               VEWGT(3,I),I=1,..,NSTWGT: A PRIORI SIGMAS    R*8
CC               STNAME(I),I=1,..,NSTAT: STATION NAMES        CH*16
CC               STANUM(I),I=1,..,NSTAT: EXT. STATION NUMBERS I*4
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/19 15:36
CC
CC CHANGES    :  27-MAY-91 : ??: DON'T PRINT TRAILING BLANKS
CC               20-APR-94 : ??: 2 DIGITS MORE FOR STATION SIGMAS
CC               07-SEP-01 : RD: HAVE A VERSION WITHOUT SKELETON FILE
CC                               ADD VELOCITIES
CC               25-SEP-02 : HU: REMOVE I_ASTLIB
CC               10-DEC-02 : CU: DON'T USE LFNPLT (SKELETON FILE), PRINT
CC                               DIFFERENT TITLE LINES FOR GPSEST AND ADDNEQ2
CC                               (PGM), PRINT SIGMA FILE NAME
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , ISTA  , ISTW  , IVEL  , NSTWGT, NVEWGT
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16  STNAME(*)
      REAL*8        STWGT(3,*), VEWGT(3,*)
      INTEGER*4     STANUM(*),ISTWGT(*),IVEWGT(*),PGM
      CHARACTER*80  WGTFILE
C
C WRITE TITLE LINES
C -----------------
      IF(NSTWGT.EQ.0 .AND. NVEWGT.EQ.0) RETURN
C
      WRITE(LFNPRT,'(A,/)')
     1  ' A priori sigma:                            ' // TRIM(WGTFILE)
C
      IF (PGM == 1) THEN
        WRITE(LFNPRT,'(A,4(/,A))')
     1    '                            Station coordinates a priori'//
     1    ' sigma',
     2    '                                 in local geodetic datum',
     3    ' ------------------------------------------------------' //
     3    '-------------------------------------------------------' //
     3    '----------------------',
     4    ' num  Station name          N (m)        E (m)       '   //
     4    ' U (m)',
     5    ' ------------------------------------------------------' //
     5    '-------------------------------------------------------' //
     5    '----------------------'
      ELSEIF (PGM == 2) THEN
        WRITE(LFNPRT,'(A,4(/,A))')
     1    '                            Station coordinates a priori'//
     1    ' sigma          Station velocities a priori sigma',
     2    '                                 in local geodetic datum'//
     2    '                     in local geodetic datum',
     3    ' ------------------------------------------------------' //
     3    '-------------------------------------------------------' //
     3    '----------------------',
     4    ' num  Station name          N (m)        E (m)       '   //
     4    ' U (m)             N (m/year)   E (m/year)    U (m/year)',
     5    ' ------------------------------------------------------' //
     5    '-------------------------------------------------------' //
     5    '----------------------'
      ENDIF
C
C A PRIORI SIGMAS FOR STATION COORDINATES
C ---------------------------------------
      DO 20 ISTW=1,NSTWGT
        DO 30 IVEL=1,NVEWGT
          IF (ISTWGT(ISTW).EQ.IVEWGT(IVEL)) THEN
            WRITE(LFNPRT,3) STANUM(ISTWGT(ISTW)),STNAME(ISTWGT(ISTW)),
     1                      (STWGT(I,ISTW),I=1,3),(VEWGT(I,IVEL),I=1,3)
3           FORMAT(I4,2X,A16,3F13.5,5X,3F13.5)
            GOTO 20
          ENDIF
30      CONTINUE
        WRITE(LFNPRT,2) STANUM(ISTWGT(ISTW)),STNAME(ISTWGT(ISTW)),
     1                  (STWGT(I,ISTW),I=1,3)
2       FORMAT(I4,2X,A16,3F13.5)
20    CONTINUE
C
C A PRIORI SIGMAS FOR STATION VELOCITIES
C --------------------------------------
      DO 40 IVEL=1,NVEWGT
        DO 50 ISTA=1,NSTWGT
          IF (IVEWGT(IVEL).EQ.ISTWGT(ISTA)) GOTO 40
50      CONTINUE
        WRITE(LFNPRT,4) STANUM(IVEWGT(IVEL)),STNAME(IVEWGT(IVEL)),
     1                  (VEWGT(I,IVEL),I=1,3)
4       FORMAT(I4,2X,A16,44X,3F13.5)
40    CONTINUE
C
      WRITE(LFNPRT,'(/)')
C
      RETURN
      END SUBROUTINE

      END MODULE
