      MODULE s_WTNUTM_O
      CONTAINS

C*
      SUBROUTINE WTNUTM(FILNUT,TITLE ,NUTNAM,NUTPRE,NUTFAR,NNUT  ,
     1                  NUTPER,NUTMLT,NUTCOE)
CC
CC NAME       :  WTNUTM
CC
CC PURPOSE    :  WRITE NUTATION MODEL FILE
CC
CC PARAMETERS :
CC        IN  :  FILNUT : FILE NAME OF NUTATION MODEL FILE      CH*32
CC               TITLE  : TITLE IN NUTATION MODEL FILE          CH*80
CC               NUTNAM : NUTATION MODEL NAME                   CH*16
CC               NUTPRE(I),I=1,2: CORRECTIONS TO PRECESSION      R*8
CC                        RATE IN ARCSEC PER CENTURY
CC                        I=1: LONGITUDE
CC                        I=2: OBLIQUITY
CC               NUTFAR(I,J),I=1,..,6,J=1,..,11: COEFFICIENTS    R*8
CC                        TO COMPUTE FUNDAMENTAL ARGUMENTS
CC                        I=1,..5: TERMS WITH DT**(I-1) IN
CC                                 ARCSEC PER CENTURY
CC                        I=6    : NUMBER OF FULL REVOLUTIONS
CC                                 CORRESPOINDING TO 1296000"
CC                        J= 1: L = MEAN ANOMALY OF THE MOON
CC                        J= 2: L'= MEAN ANOMALY OF THE SUN
CC                        J= 3: F = MEAN LONGITUDE OF THE MOON - O
CC                        J= 4: D = MEAN LONGITUDE OF THE MOON - MEAN
CC                                  LONGITUDE OF THE SUN
CC                        J= 5: O = MEAN LONGITUDE OF THE NODE OF THE
CC                                  MOON
CC                        J= 6: LV= MEAN LONGITUDE OF VENUS
CC                        J= 7: LE= MEAN LONGITUDE OF EARTH
CC                        J= 8: LM= MEAN LONGITUDE OF MARS
CC                        J= 9: LJ= MEAN LONGITUDE OF JUPITER
CC                        J=10: LS= MEAN LONGITUDE OF SATURN
CC                        J=11: PA= MEAN LONGITUDE OF GENERAL
CC                                  PRECESSION IN LONGITUDE
CC               NNUT   : NUMBER OF NUTATION PERIODS             I*4
CC               NUTPER(K),K=1,..,NNUT: APPROX. NUTATION PERIODS R*8
CC                        IN DAYS
CC               NUTMLT(J,K),J=1,..,11,K=1,..,NNUT: MULTIPLIERS  I*4
CC                        OF THE FUNDAMENTAL ARGUMENTS FOR EACH
CC                        NUTATION TERM (PERIOD)
CC               NUTCOE(L,K),L=1,..,6,K=1,..,NNUT: COEFFICIENTS  R*8
CC                        OF NUTATION MODEL IN ARCSEC
CC                        L: AI,AI',BI,BI',AI'',BI'' ACCORDING
CC                           TO THE IERS DEFINITION (IERS CON-
CC                           VENTIONS 1996, P.26)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  07-NOV-97
CC
CC CHANGES    :  15-AUG-99 : JJ: RM UNUSED VARS LINE, NUTCOI
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
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
      INTEGER*4 II    , IOSTAT, IPER  , IREC  , JJ    , MAXPER, NNUT
C
CCC       IMPLICIT REAL*8(A-H,O-Z)
C
      PARAMETER (MAXPER=1000)
C
      CHARACTER*80  TITLE
      CHARACTER*32  FILNUT
      CHARACTER*16  NUTNAM
      CHARACTER*2   ARGTXT(11)
C
      REAL*8       NUTFAR(6,11),NUTPER(MAXPER),NUTCOE(11,MAXPER)
      REAL*8       NUTPRE(2)
C
      INTEGER*4    NUTMLT(11,MAXPER)
C
C
      DATA ARGTXT /'L ','L''','F ','D ','O ',
     1             'LV','LE' ,'LM','LJ','LS','PA'/
C
C OPEN NUTATION MODEL FILE
C ------------------------
      CALL OPNFIL(LFNLOC,FILNUT,'UNKNOWN','FORMATTED',
     1            ' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILNUT,'WTNUTM')
C
C WRITE TITLE LINES AND NUTATION MODEL NAME
C -----------------------------------------
      WRITE(LFNLOC,1) TITLE,NUTNAM
1     FORMAT(A80,/,80('-'),//,
     1       'NUTATION MODEL NAME: ',A,//)
C
C WRITE PRECESSION RATE CORRECTIONS
C --------------------------------
      WRITE(LFNLOC,11) NUTPRE(1),NUTPRE(2)
11    FORMAT('PRECESSION CORRECTIONS TO IAU 1976 PRECESSION MODEL:',/,
     1       51('-'),//,
     2       'CORRECTION IN LONGITUDE ("/C):',F9.4,/,
     3       'CORRECTION IN OBLIQUITY ("/C):',F9.4,///,
     4       'FUNDAMENTAL ARGUMENTS:',/,21('-'),//,
     5       'ARG',10X,'A0',18X,'A1',18X,'A2',18X,'A3',18X,'A4',
     6       15X,'R',/,
     7       12X,'(")',17X,'("/C)',13X,'("/C**2)',12X,'("/C**3)',
     8       12X,'("/C**4)',/)
C
C WRITE COEFFICIENTS OF FUNDAMENTAL ARGUMENTS
C -------------------------------------------
      DO IREC=1,5
        WRITE(LFNLOC,2) ARGTXT(IREC),(NUTFAR(II,IREC),II=1,6)
2       FORMAT(A2,1X,5F20.10,F10.0)
      ENDDO
      WRITE(LFNLOC,'( )')
C
      DO IREC=6,11
        WRITE(LFNLOC,2) ARGTXT(IREC),(NUTFAR(II,IREC),II=1,6)
      ENDDO
C
C TITLES FOR TABLE WITH NUTATION TERMS
C ------------------------------------
      WRITE(LFNLOC,3)
3     FORMAT(//,'     MULTIPLIERS OF FUNDAMENTAL ARGUMENTS      PER',
     1          'IOD           LONGITUDE          OBLIQUITY',/,
     2          '                                               (DA',
     3          'YS)          (0.001 MAS)        (0.001 MAS)',/,
     4          '   L   L''  F   D   O  LV  LE  LM  LJ  LS  PA      ',
     5          '            AI        AI''       BI        BI''     ',
     6          ' AI''''      BI''''',/)
C
C WRITE NUTATION TERMS: MULTIPLIERS, PERIOD, AND COEFFICIENTS
C -----------------------------------------------------------
      DO IPER=1,NNUT
        WRITE(LFNLOC,4) (NUTMLT(II,IPER),II=1,11),NUTPER(IPER),
     1                  (IDNINT(NUTCOE(JJ,IPER)*1.D6),JJ=1,6)
4       FORMAT(11I4,F10.2,1X,6I10)
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
