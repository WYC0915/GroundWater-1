      MODULE s_RDNUTM_O
      CONTAINS

C*
      SUBROUTINE RDNUTM_O(MAXPER,FILNUT,TITLE ,NUTNAM,NUTPRE,NUTFAR,
     1                    NNUT  ,NUTPER,NUTMLT,NUTCOE)
CC
CC NAME       :  RDNUTM
CC
CC PURPOSE    :  READ NUTATION MODEL FILE
CC
CC PARAMETERS :
CC        IN  :  MAXPER : MAXIMUM NUMBER OF NUTATION PERIODS     I*4
CC               FILNUT : FILE NAME OF NUTATION MODEL FILE      CH*32
CC        OUT :  TITLE  : TITLE IN NUTATION MODEL FILE          CH*80
CC               NUTNAM : NUTATION MODEL NAME                   CH*16
CC               NUTPRE(I),I=1,2: CORRECTIONS TO PRECESSION      R*8
CC                        RATE IN ARCSEC PER CENTURY
CC                        I=1: LONGITUDE
CC                        I=2: OBLIQUITY
CC               NUTFAR(I,J),I=1,..,6,J=1,..,11: COEFFICIENTS    R*8
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
CC CHANGES    :  13-MAR-03 : HU: MODULE RENAMED TO RDNUTM_O
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
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 ICOE  , II    , IOSTAT, IPER  , IREC  , JJ    , MAXPER,
     1          NNUT
C
CCC       IMPLICIT REAL*8(A-H,O-Z)
C
      CHARACTER*132 LINE
      CHARACTER*80  TITLE
      CHARACTER*32  FILNUT
      CHARACTER*16  NUTNAM
C
      REAL*8       NUTFAR(6,11),NUTPER(MAXPER),NUTCOE(11,MAXPER)
      REAL*8       NUTPRE(2)
C
      INTEGER*4    NUTMLT(11,MAXPER),NUTCOI(6)
C
C
C OPEN NUTATION MODEL FILE
C ------------------------
      CALL OPNFIL(LFNLOC,FILNUT,'OLD','FORMATTED',
     1            'READONLY',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILNUT,'RDNUTM')
C
C READ TITLE LINES AND NUTATION MODEL NAME
C ----------------------------------------
      READ(LFNLOC,1,END=910) TITLE,NUTNAM
1     FORMAT(A80,///,21X,A16,//)
C
C READ PRECESSION RATE CORRECTIONS
C --------------------------------
      READ(LFNLOC,11,END=910) NUTPRE(1),NUTPRE(2)
11    FORMAT(///,30X,F9.4,/,30X,F9.4,////////)
C
C READ COEFFICIENTS OF FUNDAMENTAL ARGUMENTS
C ------------------------------------------
      DO IREC=1,5
        READ(LFNLOC,2,END=910,ERR=920) LINE
2       FORMAT(A)
        READ(LINE(3:),*,ERR=920) (NUTFAR(II,IREC),II=1,6)
      ENDDO
      READ(LFNLOC,'( )')
C
      DO IREC=6,11
        READ(LFNLOC,2,END=910,ERR=920) LINE
        READ(LINE(3:),*,ERR=920) (NUTFAR(II,IREC),II=1,6)
      ENDDO
      READ(LFNLOC,'(/////)')
C
C READ NUTATION TERMS: MULTIPLIERS, PERIOD, AND COEFFICIENTS
C ----------------------------------------------------------
      DO IPER=1,100000
        READ(LFNLOC,2,END=100,ERR=920) LINE
        IF (LINE.EQ.' ') GOTO 100
        IF (IPER.GT.MAXPER) THEN
          WRITE(LFNERR,930) MAXPER,FILNUT
930       FORMAT(/,' *** SR RDNUTM: TOO MANY NUTATION PERIODS IN ',
     1             'NUTATION FILE',
     2           /,16X,'MAXIMUM NUMBER OF PERIODS ALLOWED:',I5,
     3           /,16X,'NUTATION FILE NAME               : ',A,/)
          CALL EXITRC(2)
        ENDIF
        READ(LINE,*,ERR=920) (NUTMLT(II,IPER),II=1,11),NUTPER(IPER),
     1                       (NUTCOI(JJ),JJ=1,6)
        DO ICOE=1,6
          NUTCOE(ICOE,IPER)=NUTCOI(ICOE)/1.D6
        ENDDO
      ENDDO
C
C END OF TERMS REACHED
C --------------------
100   CONTINUE
      NNUT=IPER-1
C
      CLOSE(UNIT=LFNLOC)
      GOTO 999
C
C END OF FILE REACHED
C -------------------
910   WRITE(LFNERR,911) FILNUT
911   FORMAT(/,' *** SR RDNUTM: UNEXPECTED END OF FILE',
     1       /,16X,'FILE NAME: ',A,/)
      CALL EXITRC(2)
C
C ERROR READING FILE
C ------------------
920   WRITE(LFNERR,921) FILNUT
921   FORMAT(/,' *** SR RDNUTM: ERROR READING FILE',
     1       /,16X,'FILE NAME: ',A,/)
      CALL EXITRC(2)
C
C END
C ---
999   RETURN
      END SUBROUTINE

      END MODULE
