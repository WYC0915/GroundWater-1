      MODULE s_GTBRDG
      CONTAINS

C*
      SUBROUTINE GTBRDG(LFNRNX,LFNERR,PRGNAM,NSAT,NRSAT,NEPH,EPH,IRXVRS)
CC
CC NAME       :  GTBRDG
CC
CC PURPOSE    :  READ RINEX GLONASS EPHEMERIDES FILE
CC
CC PARAMETERS :
CC         IN :  LFNRNX : LOGICAL FILE NUMBER                  I*4
CC               LFNERR : LFN FOR ERROR MESSAGES               I*4
CC               PRGNAM : RINEX CREATION PROGRAM NAME         CH*(*)
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
CC                          EPH(1,*) : TB UTC
CC                          EPH(2,*) : TAU N      (A0)
CC                          EPH(3,*) : GAMMA N    (A1)
CC                          EPH(4,*) : TK (SECONDS INOPT CURRENT UTC DAY)
CC                          EPH(5,*) : X0
CC                          EPH(6,*) : X1
CC                          EPH(7,*) : X2
CC                          EPH(8,*) : BN (C/A CODE ONLY)
CC                          EPH(9,*) : Y0
CC                          EPH(10,*): Y1
CC                          EPH(11,*): Y2
CC                          EPH(12,*): FREQUENCY NUMBER
CC                          EPH(13,*): Z0
CC                          EPH(14,*): Z1
CC                          EPH(15,*): Z2
CC                          EPH(16,*): E (AGE OF OPER. INFORMATION)
CC                              :        :
CC
CC REMARKS    :
CC
CC AUTHOR     :  H. HABRICH
CC
CC VERSION    :  4.1  (FEB 97)
CC
CC CREATED    :  27-FEB-97
CC
CC CHANGES    :  13-MAR-98 : DI: CALL R2RDGR WITH 'PRGNAM'
CC               14-SEP-98 : MR: ADD TEST FOR EQUAL MESSAGES
CC               10-APR-03 : SS: REJECT UNREGISTERED SATELLITES AND
CC                               VERIFY FREQUENCY NUMBERS
CC               23-APR-03 : SS: DISPLAY UNHEALTHY SATELLITES
CC               04-FEB-05 : RD: SKIP NAV.-POSITIONS IN THE GEOCENTER
CC               17-FEB-05 : HU: ERROR MESSAGE CORRECTED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               08-Aug-05 : HB: Use new SR TIMST2 (module)
CC               03-DEC-07 : SS: CORRECT IFRQB IF INDICATED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE s_exitrc
      USE s_satblk
      USE s_r2rdgr
      USE s_timst2
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I0    , IBLK  , IEPH  , IFRQB , IFRQG , IRCREC,
     1          IREC  , IRXVRS, ISAT  , ISVN  , LFNERR, LFNRNX, MXCEPH,
     2          MXCSAT, NSAT
C
CCC       IMPLICIT REAL*8(A-H,O-Z)
      REAL*8       EPHRNX(16)
      REAL*8       EPH(16*MXCEPH,MXCSAT)
      INTEGER*4    NRSAT(MXCSAT),NEPH(MXCSAT)
      INTEGER*4    IUNR(MXCSAT),NUNH(MXCSAT)
      CHARACTER*19 EPOSTR
      CHARACTER*6  MXNSAT,MXNEPH
      CHARACTER    PRGNAM*(*)
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
100   CALL R2RDGR(LFNRNX,LFNERR,IRXVRS,PRGNAM,ISVN,EPHRNX,IRCREC)
C  END OF FILE
      IF(IRCREC.EQ.9) GOTO 110
      IF(IRCREC.NE.0)THEN
        CALL EXITRC(IRCREC)
      ENDIF
c new header found abfangen !!
C
C "ZOMBIE-MESSAGE": SATELLITE IN THE GEOCENTER
C --------------------------------------------
      IF (EPHRNX( 5).EQ.0d0.AND.
     1    EPHRNX( 9).EQ.0d0.AND.
     2    EPHRNX(13).EQ.0d0) THEN
        CALL TIMST2(1,1,EPHRNX(1),EPOSTR)
        WRITE(LFNERR,1005) ISVN,EPOSTR
1005     FORMAT(/,' ### SR GTBRDG: SKIP INVALID NAVIGATION MESSAGE',/,
     1                 16X,'(SATELLITE IN THE GEOCENTER)',/,
     2                 16X,'SATELLITE NUMBER : ',I3,/,
     3                 16X,'EPOCH            : ',A19,/)
        GOTO 100
      ENDIF
C
C NEW SATELLITE ?
      DO 130 ISAT=1,NSAT
          IF(ISVN.EQ.NRSAT(ISAT))GO TO 140
130   CONTINUE
      NSAT=NSAT+1
      IF(NSAT.GT.MXCSAT)THEN
        WRITE(LFNERR,1000) MXCSAT
1000     FORMAT(/,' *** SR GTBRDG: TOO MANY SATELLITES',/,
     1                 16X,'MAXIMUM NUMBER OF SAT. ALLOWED:',I3,/)
         CALL EXITRC(2)
      END IF
      NRSAT(NSAT)=ISVN
      ISAT=NSAT
140   CONTINUE
C
C KM -> M
C
      EPHRNX( 5)=EPHRNX( 5)*1.D3
      EPHRNX( 6)=EPHRNX( 6)*1.D3
      EPHRNX( 7)=EPHRNX( 7)*1.D3
      EPHRNX( 9)=EPHRNX( 9)*1.D3
      EPHRNX(10)=EPHRNX(10)*1.D3
      EPHRNX(11)=EPHRNX(11)*1.D3
      EPHRNX(13)=EPHRNX(13)*1.D3
      EPHRNX(14)=EPHRNX(14)*1.D3
      EPHRNX(15)=EPHRNX(15)*1.D3
C
C CHECK IF MESSAGE ALREADY READ
      DO IEPH=1,NEPH(ISAT)
        I0=16*(IEPH-1)
        DO IREC=1,16
          IF (IREC.NE.4) THEN
            IF (EPH(I0+IREC,ISAT).NE.EPHRNX(IREC)) GOTO 145
          ENDIF
        ENDDO
C MESSAGE ALREADY AVAILABLE
        GOTO 100
145     CONTINUE
      ENDDO
C
C REJECT UNREGISTERED SATELLITES AND VERIFY FREQUENCY NUMBERS
      CALL SATBLK(ISVN,EPHRNX(1),IFRQG,IBLK)
      IF (IBLK.NE.-1) THEN
        IFRQB=IDNINT(EPHRNX(12))
        IF (IFRQB.GT.24) IFRQB=IFRQB-256
        IF (IBLK.EQ.0) THEN
          IF (IUNR(ISAT).EQ.0) THEN
            CALL TIMST2(1,1,EPHRNX(1),EPOSTR)
            WRITE(LFNERR,1020) ISVN,IFRQB,EPOSTR
1020        FORMAT(/,' ### SR GTBRDG: SATELLITE NOT REGISTERED IN ',
     1        'SATELLITE INFORMATION FILE',/,
     2        16X,'SATELLITE NUMBER : ',I3,/,
     3        16X,'FREQUENCY NUMBER : ',I3,/,
     4        16X,'EPOCH            : ',A19,/)
            IUNR(ISAT)=1
          ENDIF
C MESSAGE NOT ACCEPTED
          GOTO 100
        ELSE
          IF (IFRQB.NE.IFRQG) THEN
            CALL TIMST2(1,1,EPHRNX(1),EPOSTR)
            WRITE(LFNERR,1030) ISVN,IFRQB,IFRQG,NEPH(ISAT)+1,EPOSTR
1030        FORMAT(/,' ### SR GTBRDG: FREQUENCY NUMBER INCONSISTENT ',
     1        'WITH GIVEN SATELLITE INFORMATION',/,
     2        16X,'SATELLITE NUMBER           : ',I3,/,
     3        16X,'BROADCAST FREQUENCY NUMBER : ',I3,/,
     4        16X,'GIVEN FREQUENCY NUMBER     : ',I3,/,
     5        16X,'MESSAGE NUMBER             : ',I3,/,
     6        16X,'EPOCH                      : ',A,/)
          ENDIF
        ENDIF
      ENDIF
C
C  CHECK WHETHER SATELLITE SET UNHEALTHY
      IF (EPHRNX(8).NE.0.D0) NUNH(ISAT)=NUNH(ISAT)+1
C
C READ ENTIRE MESSAGE
      NEPH(ISAT)=NEPH(ISAT)+1
      IF(NEPH(ISAT).GT.MXCEPH)THEN
        WRITE(LFNERR,1010) MXCEPH,NRSAT(ISAT)
1010    FORMAT(/,' *** SR GTBRDG: TOO MANY MESSAGES PER SATELLITE',/,
     1                         16X,'MAXIMUM NUMBER ALLOWED:',I4,/,
     2                         16X,'SATELLITE             :',I4,/)
        CALL EXITRC(2)
      END IF
      I0=16*(NEPH(ISAT)-1)
      DO 150 IREC=1,16
        EPH(I0+IREC,ISAT)=EPHRNX(IREC)
150   CONTINUE
C
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
1031        FORMAT(/,' ### SR GTBRDG: SATELLITE PERMANENTLY SET ',
     1        'UNHEALTHY',/,
     2        16X,'SATELLITE NUMBER: ',I3,/)
          ELSE
            WRITE(LFNERR,1032) NRSAT(ISAT)
1032        FORMAT(/,' ### SR GTBRDG: SATELLITE TEMPORARILY SET ',
     1        'UNHEALTHY',/,
     2        16X,'SATELLITE NUMBER: ',I3,/)
          ENDIF
        ENDIF
      ENDDO
C
      RETURN
      END SUBROUTINE

      END MODULE
