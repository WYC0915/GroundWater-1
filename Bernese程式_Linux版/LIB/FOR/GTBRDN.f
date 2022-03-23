      MODULE s_GTBRDN
      CONTAINS

C*
      SUBROUTINE GTBRDN(LFNRNX,LFNERR,ISYS,NSAT,NRSAT,NEPH,EPH,CLOCK,
     1                  IRXVRS)
CC
CC NAME       :  GTBRDN
CC
CC PURPOSE    :  READ RINEX EPHEMERIDES FILE
CC               STRUCTURE OF ARRAYS CLOCK AND EPH:
CC               SEE VAN DIERENDONCK ET AL. "GPS NAVIGATION
CC               MESSAGE", NAVIGATION I, P.55-73, INSTITUTE OF
CC               NAVIGATION, WASHINGTON, 1980
CC               C. PAYNE "NAVSTAR GPS: 1982", PROC. OF 3.INTERN. GEOD.
CC               SYMP. ON SAT. DOP. POS., VOL 2, 1982 (P. 1015)
CC
CC PARAMETERS :
CC         IN :  LFNRNX : LOGICAL FILE NUMBER                  I*4
CC               LFNERR : LFN FOR ERROR MESSAGES               I*4
CC               ISYS   : GNSS IDENTIFIER (SEE M_GLOBAL)       I*4
CC               IRXVRS : RINEX VERSION NUMBER                 I*4
CC        OUT :  NSAT   : NUMBER OF SATELLITES                 I*4
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
CC REMARKS    :
CC
CC AUTHOR     :  G. BEUTLER, M.ROTHACHER, H. HABRICH
CC
CC VERSION    :  4.1  (FEB 97)
CC
CC CREATED    :  27-FEB-97
CC
CC CHANGES    :  14-SEP-98 MR: ADD TEST FOR EQUAL MESSAGES
CC               10-APR-03 SS: REJECT UNREGISTERED SATELLITES
CC               23-APR-03 SS: DISPLAY UNHEALTHY SATELLITES
CC               23-JUN-05 MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               08-AUG-05 HB: USE NEW SR TIMST2 (MODULE)
CC               28-JUL-10 AS/DT/MM: INCREASE EPHRNX 28->32
CC               09-AUG-10 SL/RD: ISYS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE s_exitrc
      USE s_satblk
      USE f_gpsmjd
      USE s_r2rdnr
      USE s_rxv3br
      USE s_timst2
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I0    , IBLK  , IEPH  , IFRQ  , IRCREC, IREC  ,
     1          IRXVRS, ISAT  , ISVN  , IWEEK , LFNERR, LFNRNX, MXCEPH,
     2          MXCSAT, NSAT  , ISYS
C
      REAL*8    XEPO
C
CCC       IMPLICIT REAL*8(A-H,O-Z)
C
      REAL*8       EPHRNX(32),EPHV3(20),CLKV3(20)
      REAL*8       EPH(20*MXCEPH,MXCSAT),CLOCK(20*MXCEPH,MXCSAT)
C
      INTEGER*4    NRSAT(MXCSAT),NEPH(MXCSAT)
      INTEGER*4    IUNR(MXCSAT),NUNH(MXCSAT)
C
      CHARACTER*19 EPOSTR
      CHARACTER*6  MXNSAT,MXNEPH
C
C COMMON FOR MAXIMAL DIMENSIONS AND LOGICAL FILE NUMBERS
C ------------------------------------------------------
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMEPH/MXCEPH,MXNEPH
C
C INITIALIZE NUMBER OF SATELLITES, SATELLITE NUMBERS,
C NUMBER OF EPHEMERIDES FOR EACH SATELLITE
C ---------------------------------------------------
      NSAT=0
      DO 10 I=1,MXCSAT
        NEPH(I)=0
        NRSAT(I)=0
        IUNR(I)=0
        NUNH(I)=0
10    CONTINUE
C
C READ ONE RINEX ARRAY AT A TIME
C ------------------------------
100   CALL R2RDNR(LFNRNX,LFNERR,IRXVRS,ISVN,EPHRNX,IRCREC)

      IF(ISVN .LT. 100) ISVN=ISVN+100*ISYS
C  END OF FILE
      IF(IRCREC.EQ.9) GOTO 110
      IF(IRCREC.NE.0)THEN
        CALL EXITRC(IRCREC)
      ENDIF
c new header found abfangen !!
C
C  TRANSFORM RINEX ARRAY INTO BERNESE FORMAT
      CALL RXV3BR(EPHRNX,EPHV3,CLKV3)
C
C NEW SATELLITE ?
      DO 130 ISAT=1,NSAT
          IF(ISVN.EQ.NRSAT(ISAT))GO TO 140
130   CONTINUE
      NSAT=NSAT+1
      IF(NSAT.GT.MXCSAT)THEN
        WRITE(LFNERR,1000) MXCSAT
1000     FORMAT(/,' *** SR GTBRDN: TOO MANY SATELLITES',/,
     1                 16X,'MAXIMUM NUMBER OF SAT. ALLOWED:',I3,/)
         CALL EXITRC(2)
      END IF
      NRSAT(NSAT)=ISVN
      ISAT=NSAT
140   CONTINUE
C
C CHECK IF MESSAGE ALREADY READ
      DO IEPH=1,NEPH(ISAT)
        I0=20*(IEPH-1)
        DO IREC=1,20
          IF (EPH(I0+IREC,ISAT).NE.EPHV3(IREC)) GOTO 145
          IF (CLOCK(I0+IREC,ISAT).NE.CLKV3(IREC)) GOTO 145
        ENDDO
C MESSAGE ALREADY AVAILABLE
        GOTO 100
145     CONTINUE
      ENDDO
C
C REJECT UNREGISTERED SATELLITES
CC      IWEEK=IDNINT(EPHV3(1))
CC      XEPO=GPSMJD(EPHV3(2),IWEEK)
      IWEEK=IDNINT(EPHRNX(23))
      XEPO=GPSMJD(EPHRNX(13),IWEEK)
      CALL SATBLK(ISVN,XEPO,IFRQ,IBLK)
      IF (IBLK.EQ.0) THEN
        IF (IUNR(ISAT).EQ.0) THEN
          CALL TIMST2(1,1,XEPO,EPOSTR)
          WRITE(LFNERR,1020) ISVN,EPOSTR
1020      FORMAT(/,' ### SR GTBRDN: SATELLITE NOT REGISTERED IN ',
     1      'SATELLITE INFORMATION FILE',/,
     2      16X,'SATELLITE NUMBER : ',I3,/,
     3      16X,'EPOCH            : ',A19,/)
          IUNR(ISAT)=1
        ENDIF
C MESSAGE NOT ACCEPTED
        GOTO 100
      ENDIF
C
C  CHECK WHETHER SATELLITE SET UNHEALTHY
CC      IF (CLOCK(4).NE.0.D0) NUNH(ISAT)=NUNH(ISAT)+1
      IF (EPHRNX(26).NE.0.D0) NUNH(ISAT)=NUNH(ISAT)+1
C
C READ ENTIRE MESSAGE
      NEPH(ISAT)=NEPH(ISAT)+1
      IF(NEPH(ISAT).GT.MXCEPH)THEN
        WRITE(LFNERR,1010) MXCEPH,NRSAT(ISAT)
1010    FORMAT(/,' *** SR GTBRDN: TOO MANY MESSAGES PER SATELLITE',/,
     1                         16X,'MAXIMUM NUMBER ALLOWED:',I4,/,
     2                         16X,'SATELLITE             :',I4,/)
        CALL EXITRC(2)
      END IF
      I0=20*(NEPH(ISAT)-1)
      DO 150 IREC=1,20
        EPH(I0+IREC,ISAT)=EPHV3(IREC)
        CLOCK(I0+IREC,ISAT)=CLKV3(IREC)
150   CONTINUE
C NEXT RECORD
      GOTO 100
C
110   CONTINUE
C
C DISPLAY UNHEALTHY SATELLITES
      DO ISAT=1,NSAT
        IF (NUNH(ISAT).GT.0) THEN
          IF (NUNH(ISAT).EQ.NEPH(ISAT)) THEN
            WRITE(LFNERR,1031) NRSAT(ISAT)
1031        FORMAT(/,' ### SR GTBRDN: SATELLITE PERMANENTLY SET ',
     1        'UNHEALTHY',/,
     2        16X,'SATELLITE NUMBER: ',I3,/)
          ELSE
            WRITE(LFNERR,1032) NRSAT(ISAT)
1032        FORMAT(/,' ### SR GTBRDN: SATELLITE TEMPORARILY SET ',
     1        'UNHEALTHY',/,
     2        16X,'SATELLITE NUMBER: ',I3,/)
          ENDIF
        ENDIF
      ENDDO
C
      RETURN
      END SUBROUTINE

      END MODULE
