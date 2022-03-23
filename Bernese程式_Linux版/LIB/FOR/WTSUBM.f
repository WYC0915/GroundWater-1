      MODULE s_WTSUBM
      CONTAINS

C*
      SUBROUTINE WTSUBM(FILSUB,TITLE ,SUBNAM,SUBFAR,NSUB  ,
     1                  SUBPER,SUBMLT,SUBCOE)
CC
CC NAME       :  WTSUBM
CC
CC PURPOSE    :  WRITE SUBDAILY ERP MODEL FILE
CC
CC PARAMETERS :
CC        IN  :  FILSUB : FILE NAME OF SUBDAILY ERP MODEL FILE  CH*(*)
CC               TITLE  : TITLE IN SUBDAILY ERP MODEL           CH*80
CC               SUBNAM : SUBDAILY ERP MODEL NAME               CH*16
CC               SUBFAR(I,J),I=1,..,6,J=1,..,6: COEFFICIENTS    R*8
CC                        TO COMPUTE FUNDAMENTAL ARGUMENTS
CC                        I=1,..5: TERMS WITH DT**(I-1) IN
CC                                 ARCSEC PER CENTURY ETC.
CC                        I=6    : NUMBER OF FULL REVOLUTIONS
CC                                 CORRESPONDING TO 1296000"
CC                        J= 1: L = MEAN ANOMALY OF THE MOON
CC                        J= 2: L'= MEAN ANOMALY OF THE SUN
CC                        J= 3: F = MEAN LONGITUDE OF THE MOON - O
CC                        J= 4: D = MEAN LONGITUDE OF THE MOON - MEAN
CC                                  LONGITUDE OF THE SUN
CC                        J= 5: O = MEAN LONGITUDE OF THE NODE OF THE
CC                                  MOON
CC                        J= 6: T = GREENWICH MEAN SIDEREAL TIME
CC               NSUB   : NUMBER OF SUBDAILY ERP PERIODS         I*4
CC               SUBPER(K),K=1,..,NSUB: APPROX. SUBDAILY PERIODS R*8
CC                        IN DAYS
CC               SUBMLT(J,K),J=1,..,6,K=1,..,NSUB: MULTIPLIERS  I*4
CC                        OF THE FUNDAMENTAL ARGUMENTS FOR EACH
CC                        SUBDAILY TERM (PERIOD)
CC               SUBCOE(L,K),L=1,..,4,K=1,..,NSUB: COEFFICIENTS  R*8
CC                        OF SUBDAILY ERP MODEL IN ARCSEC AND SEC
CC                        L=1: COS PM
CC                        L=2: SIN PM
CC                        L=3: COS UT
CC                        L=4: SIN UT
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  26-FEB-98
CC
CC CHANGES    :  15-AUG-99 : JJ: RM UNUSED VARS LINE, SUBCOI
CC               20-MAY-03 : HU: OPEN FILENAME LENGTH
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1998     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE s_opnfil
      USE s_opnerr
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 II    , IOSTAT, IPER  , IREC  , JJ    , MAXPER, NSUB
C
CCC       IMPLICIT REAL*8(A-H,O-Z)
C
      PARAMETER (MAXPER=1000)
C
      CHARACTER*80  TITLE
      CHARACTER*(*)  FILSUB
      CHARACTER*16  SUBNAM
      CHARACTER*2   ARGTXT(6)
C
      REAL*8       SUBFAR(6,6),SUBPER(MAXPER),SUBCOE(4,MAXPER)
C
      INTEGER*4    SUBMLT(6,MAXPER)
C
C
      DATA ARGTXT /'L ','L''','F ','D ','O ','T '/
C
C OPEN SUBDAILY ERP MODEL FILE
C ----------------------------
      CALL OPNFIL(LFNLOC,FILSUB,'UNKNOWN','FORMATTED',
     1            ' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILSUB,'WTSUBM')
C
C WRITE TITLE LINES AND SUBDAILY ERP MODEL NAME
C --------------------------------------------
      WRITE(LFNLOC,1) TITLE,SUBNAM
1     FORMAT(A80,/,80('-'),//,
     1       'SUBDAILY ERP MODEL NAME: ',A,//)
C
C WRITE TITLES FOR FUNDAMENTAL ARGUMENTS
C --------------------------------------
      WRITE(LFNLOC,11)
11    FORMAT('FUNDAMENTAL ARGUMENTS:',/,21('-'),//,
     1       'ARG',10X,'A0',18X,'A1',18X,'A2',18X,'A3',18X,'A4',
     2       15X,'R',/,
     3       12X,'(")',17X,'("/C)',13X,'("/C**2)',12X,'("/C**3)',
     4       12X,'("/C**4)',/)
C
C WRITE COEFFICIENTS OF FUNDAMENTAL ARGUMENTS
C ------------------------------------------
      DO IREC=1,6
        WRITE(LFNLOC,2) ARGTXT(IREC),(SUBFAR(II,IREC),II=1,6)
2       FORMAT(A2,1X,5F20.10,F10.0)
      ENDDO
C
C TITLES FOR TABLE WITH NUTATION TERMS
C ------------------------------------
      WRITE(LFNLOC,3)
3     FORMAT(//,'   FUNDAMENTAL ARGUMENTS   PERIOD        POLAR MOT',
     1          'ION            UT1',/,
     2          '                           (HOURS)        (0.001 M',
     3          'AS)         (0.001 MS)',/,
     4          '   L   L''  F   D   O   T                PMCOS     ',
     5          'PMSIN     UTCOS     UTSIN',/)
C
C WRITE SUBDAILY TERMS: MULTIPLIERS, PERIOD, AND COEFFICIENTS
C ----------------------------------------------------------
      DO IPER=1,NSUB
        WRITE(LFNLOC,4) (SUBMLT(II,IPER),II=1,6),SUBPER(IPER)*24.D0,
     1                  (SUBCOE(JJ,IPER)*1.D6,JJ=1,4)
4       FORMAT(6I4,F10.3,1X,4F10.2)
      ENDDO
      WRITE(LFNLOC,'( )')
C
      CLOSE(UNIT=LFNLOC)
C
C END
C ---
999   RETURN
      END SUBROUTINE

      END MODULE
