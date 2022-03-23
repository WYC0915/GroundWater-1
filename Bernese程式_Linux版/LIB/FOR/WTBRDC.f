      MODULE s_WTBRDC
      CONTAINS

C*
      SUBROUTINE WTBRDC(FILBRD,TITLE,NSAT,NRSAT,NEPH,EPH,CLOCK)
CC
CC NAME       :  WTBRDC
CC
CC PURPOSE    :  WRITE BROADCAST MESSAGE FILE (BERNESE FORMAT)
CC
CC PARAMETERS :
CC         IN :  FILBRD : NAME OF BRDFILE TO BE WRITTEN       CH*(*)
CC                        IF FILBRD =' ' THEN NOTHING
CC                        WILL BE WRITTEN
CC               TITLE  : TITLE LINE                          CH*53
CC               NSAT   : NUMBER OF SATELLITES                I*4
CC               NRSAT(I),I=1,..,NSAT: SATELLITE NUMBERS      I*4
CC               NEPH(I),I=1,..,NSAT: NUMBER OF EPHEMERIS     I*4
CC                        FOR SATELLITE I
CC               EPH    : EPHEMERIDES INFORMATION             R*8(*,*)
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
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/12/07 09:03
CC
CC CHANGES    :  11-JAN-93 : ??: USE OF SR "OPNFIL" TO OPEN FILES
CC               10-FEB-93 : ??: FORMAT D30.20 --> D28.18 (LAHEY PROBLEM)
CC               20-MAY-03 : HU: OPEN FILENAME LENGTH
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE d_const, ONLY: DATE, TIME
      USE s_opnfil
      USE s_opnerr
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I0    , IOSTAT, IREC  , ISAT  , MESS  , MXCEPH, NSAT
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*53 TITLE
      CHARACTER*(*) FILBRD
      CHARACTER*6  MXNEPH
      REAL*8       EPH(20*MXCEPH,*),CLOCK(20*MXCEPH,*)
      INTEGER*4    NEPH(*),NRSAT(*)
C
      COMMON/MCMEPH/MXCEPH,MXNEPH
C
C IF FILBRD = ' ' THEN NOTHING IS WRITTEN
C ---------------------------------------
      IF(FILBRD.EQ.' ') GOTO 999
C
C OPEN BROADCAST FILE TO BE WRITTEN
C ---------------------------------
      CALL OPNFIL(LFNORB,FILBRD,'UNKNOWN','FORMATTED',
     1            ' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNORB,IOSTAT,FILBRD,'WTBRDC')
C
C TITLE
C -----
      WRITE(LFNORB,5010)TITLE,DATE,TIME
5010  FORMAT(A53,12X,A9,1X,A5)
C
C SAVE ALL EPHEMERIS AND CLOCK MESSAGES
C -------------------------------------
      DO 5100 ISAT=1,NSAT
        DO 5090 MESS=1,NEPH(ISAT)
          WRITE(LFNORB,5020) NRSAT(ISAT),MESS
5020      FORMAT('SVN-NUMBER=',I3,'  MESSAGE-NR=',I3)
          I0=(MESS-1)*20
          DO 5030 IREC=1,20
            WRITE(LFNORB,5035) EPH(I0+IREC,ISAT)
5035        FORMAT(D28.18)
5030      CONTINUE
          DO 5040 IREC=1,20
            WRITE(LFNORB,5035) CLOCK(I0+IREC,ISAT)
5040      CONTINUE
5090    CONTINUE
5100  CONTINUE
C
C CLOSE BROADCAST FILE
C --------------------
      CLOSE(UNIT=LFNORB)
C
999   RETURN
      END SUBROUTINE

      END MODULE
