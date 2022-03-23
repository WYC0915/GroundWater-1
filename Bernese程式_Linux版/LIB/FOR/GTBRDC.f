      MODULE s_GTBRDC
      CONTAINS

C*
      SUBROUTINE GTBRDC(FILBRD,TITLE,NSAT,NRSAT,NEPH,EPH,CLOCK)
CC
CC NAME       :  GTBRDC
CC
CC PURPOSE    :  READ BROADCAST EPHEMERIDES FILE
CC               STRUCTURE OF ARRAYS CLOCK AND EPH:
CC               SEE VAN DIERENDONCK ET AL. "GPS NAVIGATION
CC               MESSAGE", NAVIGATION I, P.55-73, INSTITUTE OF
CC               NAVIGATION, WASHINGTON, 1980
CC               C. PAYNE "NAVSTAR GPS: 1982", PROC. OF 3.INTERN. GEOD.
CC               SYMP. ON SAT. DOP. POS., VOL 2, 1982 (P. 1015)
CC
CC PARAMETERS :
CC         IN :  FILBRD : BROADCAST FILE NAME                  CH*32
CC        OUT :  TITLE  : TITLE LINE                           CH*53
CC               NSAT   : NUMBER OF SATELLITES                 I*4
CC               NRSAT  : SATELLITE NUMBERS                    I*4(*)
CC                        NRSAT(I):
CC                          I: SATELLITE
CC               NEPH   : NUMBER OF EPHEMERIDES AVAILABLE      I*4(*)
CC                        NEPH(I):
CC                          I: SATELLITE
CC               EPH    : EPHEMERIDES INFORMATION              R*8(*,*)
CC                        EPH(I,K):
CC                          I: EPHEMERIDE ELEMENT
CC                          K: SATELLITE
CC                          EPH(1,*) : GPS-WEEK
CC                          EPH(2,*) : T0E
CC                          EPH(3,*) : A
CC                          EPH(4,*) : E
CC                          EPH(5,*) : I
CC                          EPH(6,*) : R.A. OF ASCENDING NODE
CC                          EPH(7,*) : PERIGEE
CC                          EPH(8,*) : MEAN ANOMALY (T0E)
CC                          EPH(9,*) : DN (CORRECTION TO MEAN MOTION)
CC                          EPH(10,*): RATE OF NODE
CC                          EPH(11,*): CUS
CC                          EPH(12,*): CUC
CC                          EPH(13,*): CRS
CC                          EPH(14,*): CRC
CC                          EPH(15,*): CIS
CC                          EPH(16,*): CIC
CC                          EPH(17,*): AODE
CC                          EPH(18,*): IDOT
CC                          EPH(19,*): NOT USED
CC                          EPH(20,*): NOT USED
CC                          EPH(21,*): GPS WEEK OF THE NEXT EPHEMERIDE
CC                              :        :
CC               CLOCK  : CLOCK INFORMATION                    R*8(*,*)
CC                        CLOCK(I,K)
CC                          I: EPHEMERIDE ELEMENT
CC                          K: SATELLITE
CC                          CLOCK(1,*) : GPS-WEEK
CC                          CLOCK(2,*) : L2 CODE INDICATOR
CC                          CLOCK(3,*) : USER RANGE ACCURACY (M)
CC                          CLOCK(4,*) : SV HEALTH MSB (NAVIG. DATA)
CC                          CLOCK(5,*) : SV HEALTH LSB'S (SIGNAL COMP.)
CC                          CLOCK(6,*) : L2 P DATA FLAG
CC                          CLOCK(7,*) : NOT USED
CC                          CLOCK(8,*) : NOT USED
CC                          CLOCK(9,*) : TGD
CC                          CLOCK(10,*): AODC
CC                          CLOCK(11,*): TOC
CC                          CLOCK(12,*): A2
CC                          CLOCK(13,*): A1
CC                          CLOCK(14,*): A0
CC                          CLOCK(14+I),I=1,2,..,6 : NOT USED
CC                          CLOCK(21,*): GPS WEEK OF NEXT EPHEMERIDE
CC                              :        :
CC
CC REMARKS    :  NEW BROADCAST FILE FORMAT
CC
CC AUTHOR     :  G. BEUTLER, M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/10/19 09:11
CC
CC CHANGES    :  28-DEC-92 : ??: USE OF SR "OPNFIL" TO OPEN FILES
CC               09-FEB-93 : ??: READ WITH FORMAT *
CC               10-AUG-94 : MR: CALL EXITRC
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE s_exitrc
      USE s_opnfil
      USE s_opnerr
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I0    , IOSTAT, IREC  , ISAT  , MESS  , MXCEPH,
     1          MXCSAT, NSAT
C
CCC       IMPLICIT REAL*8(A-H,O-Z)
      REAL*8       EPH(20*MXCEPH,MXCSAT),CLOCK(20*MXCEPH,MXCSAT)
      INTEGER*4    SVN,NRSAT(MXCSAT),NEPH(MXCSAT)
      CHARACTER*53 TITLE
      CHARACTER*32 FILBRD
      CHARACTER*6  MXNSAT,MXNEPH
C
C COMMON FOR MAXIMAL DIMENSIONS AND LOGICAL FILE NUMBERS
C ------------------------------------------------------
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMEPH/MXCEPH,MXNEPH
C
C OPEN EPHEMERIDES FILE
C ---------------------
      CALL OPNFIL(LFNLOC,FILBRD,'OLD','FORMATTED',
     1            'READONLY',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILBRD,'GTBRDC')
C
C INITIALIZE NUMBER OF SATELLITES, SATELLITE NUMBERS,
C NUMBER OF EPHEMERIDES FOR EACH SATELLITE
C ---------------------------------------------------
      NSAT=0
      DO 10 I=1,MXCSAT
        NEPH(I)=0
        NRSAT(I)=0
10    CONTINUE
C
C READ EPHEMERIS PARAMETERS AND CLOCK PARAMETERS
C ----------------------------------------------
      DO 100 MESS=1,10000
        IF(NSAT.EQ.0) THEN
          READ(LFNLOC,20,END=110) TITLE,SVN
20        FORMAT(A53,/,11X,I3)
        ELSE
          READ(LFNLOC,21,END=110) SVN
21        FORMAT(11X,I3)
        END IF
        IF(SVN.EQ.0)GO TO 110
C
C NEW SATELLITE ?
        DO 30 ISAT=1,NSAT
          IF(SVN.EQ.NRSAT(ISAT))GO TO 40
30      CONTINUE
        NSAT=NSAT+1
        IF(NSAT.GT.MXCSAT)THEN
          WRITE(LFNERR,35) MXCSAT
35        FORMAT(/,' *** SR GTBRDC: TOO MANY SATELLITES',/,
     1                   16X,'MAXIMUM NUMBER OF SAT. ALLOWED:',I3,/)
          CALL EXITRC(2)
        END IF
        NRSAT(NSAT)=SVN
        ISAT=NSAT
40      CONTINUE
C
C READ ENTIRE MESSAGE
        NEPH(ISAT)=NEPH(ISAT)+1
        IF(NEPH(ISAT).GT.MXCEPH)THEN
          WRITE(LFNERR,36) MXCEPH,NRSAT(ISAT)
36        FORMAT(/,' *** SR GTBRDC: TOO MANY MESSAGES PER SATELLITE',/,
     1                         16X,'MAXIMUM NUMBER ALLOWED:',I4,/,
     2                         16X,'SATELLITE             :',I4,/)
          CALL EXITRC(2)
        END IF
        I0=20*(NEPH(ISAT)-1)
        DO 50 IREC=1,20
          READ(LFNLOC,*) EPH(I0+IREC,ISAT)
50      CONTINUE
        DO 60 IREC=1,20
          READ(LFNLOC,*) CLOCK(I0+IREC,ISAT)
60      CONTINUE
100   CONTINUE
C
110   CONTINUE
C
C CLOSE BROADCAST FILE
C --------------------
      CLOSE(UNIT=LFNLOC)
C
      RETURN
      END SUBROUTINE

      END MODULE
