      MODULE s_RDPREI
      CONTAINS

C*
      SUBROUTINE RDPREI(LFN,IFRMAT,IREADE,NSAT,SATNUM,TMJD,
     1                  POS,VEL,DTSATC,DDTSAT,ACCPOS,ACCVEL,EVTFLG,
     2                  IEREC,SDEVP,SDEVV,CORRP,CORRV,IRCODE)
CC
CC NAME       :  RDPREI
CC
CC PURPOSE    :  READ INFROMATION FOR ONE OBSERVATION EPOCH FROM A
CC               PRECISE ORBIT FILE (ASSUMING THAT THE FILE WAS OPENED)
CC
CC PARAMETERS :
CC         IN :  LFN    : LOGICAL FILE NUMBER FOR INPUT FILE   I*4
CC               IFRMAT : FORMAT TYPE                          I*4
CC                        =0 : SHORT FORMAT (SV1, POS)
CC                        =1 : LARGE FORMAT (SV1, POS+VEL)
CC                        =2 : SP3 FORMAT (POS+CLOCKS))
CC                        =3 : SP3 FORMAT (POS+VEL+CLOCKS)
CC                        =4 : SP3-c FORMAT (POS+CLOCKS))
CC                        =5 : SP3-c FORMAT (POS+VEL+CLOCKS)
CC               IREADE : =1: READ OPTIONAL EP AND EV RECORDS  I*4
CC               NSAT   : NUMBER OF SATELLITES                 I*4
CC               SATNUM : SATELLITE NUMBERS                    I*4(*)
CC        OUT :  TMJD   : TIME OF EPOCH                        T_EPOCH
CC               POS    : SATELLITE POSITIONS (M)              R*8(3,*)
CC               VEL    : SATELLITE VELOCITY (M/S)             R*8(3,*)
CC               DTSATC : SATELLITE CLOCK ERROR (SEC)          R*8(*)
CC               DDTSAT : RATE OF CHANGE OF SAT. CLOCK ERROR   R*8(*)
CC                        (SEC/SEC)
CC               ACCPOS : ACCURACY CODE FOR POS AND CLK        I*4(4,*)
CC               ACCVEL : ACCURACY CODE FOR VEL AND CLK RATE   I*4(4,*)
CC               EVTFLG : EVENT FLAG FOR SATELLITE             CH*1(4,*)
CC               IEREC  : AVAILABILITY OF E-RECORD FOR POS,VEL I*4(2)
CC                        =0: NO OPTIONAL E-RECORD AVAILABLE
CC                        =1: OPTIONAL E-RECORD AVAILABLE
CC               SDEVP  : SDEV FOR POS AND CLK                 R*8(4,*)
CC               SDEVV  : SDEV FOR VEL AND CLK RATE            R*8(4,*)
CC               CORRP  : CORRELATION FOR POS AND CLK          R*8(6,*)
CC               CORRV  : CORRELATION FOR VEL AND CLK RATE     R*8(6,*)
CC               IRCODE : RETURN CODE
CC                        0 - ALL OK
CC                        1 - EOF
CC
CC REMARKS    :  NOT TESTED FOR THE OLD FORMATS SP1 AND SP2 !!
CC
CC               SP3-C PROVIDES ACCPOS, ACCVEL, EVTFLG,
CC               AND OPTIONALLY SDEVP, SDEVV, CORRP, CORRP:
CC
CC               ACCPOS AND ACCVEL FOR I=1,3: SDEV FOR POS AND VEL
CC                 0   : UNKNOWN
CC                 99  : TOO LARGE, UNUSABLE
CC                 ELSE: BASPOS**ACCPOS GIVES SDEV FOR POS IN MM
CC                       BASPOS**ACCVEL GIVES SDEV FOR VEL IN 1E-4 MM/SEC
CC               ACCPOS AND ACCVEL FOR I=4: SDEV FOR CLOCK AND CLOCK RATE
CC                 0   : UNKNOWN
CC                 999 : TOO LARGE, UNUSABLE
CC                 ELSE: BASPOS**ACCPOS GIVES SDEV FOR CLK IN PSEC
CC                       BASPOS**ACCVEL GIVES SDEV FOR CLKRATE IN 1E-4 PSEC/SEC
CC               EVTFLG(I): I=1: CLOCK EVENT FLAG (blank or E)
CC                          I=2: CLOCK PREDICTION FLAG (blank or P)
CC                          I=3: MANEUVER FLAG (blank or M)
CC                          I=4: ORBIT PREDICTION FLAG (blank or P)
CC               SDEVP AND SDEVV FOR I=1,3: SDEV FOR POS AND VEL
CC                 IN M RESP. M/SEC
CC               SDEVP AND SDEVV FOR I=4: SDEV FOR CLK AND CLK RATE
CC                 IN SEC RESP. SEC/SEC
CC               CORRP(I): CORRELLATION COEFFICIENTS
CC                          I=1: X-Y, I=2: X-Z,   I=3: X-CLK
CC                          I=4: Y-Z, I=5: Y-CLK, I=6: Z-CLK
CC               CORRV(I): CORRELLATION COEFFICIENTS
CC                          I=1: VX-VY, I=2: VX-VZ,   I=3: VX-RCLK
CC                          I=4: VY-VZ, I=5: VY-RCLK, I=6: VZ-RCLK
CC
CC AUTHOR     :  D.INEICHEN
CC
CC VERSION    :  4.1
CC
CC CREATED    :  09-JUL-97
CC
CC CHANGES    :  28-MAY-01 : HB: ADD SR TYP2SVN (SUPPORT OF LEOs)
CC               02-NOV-02 : HU: SP3-c IMPLEMENTED
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               08-JUN-03 : HU: INITIALIZE SATTYP
CC               11-SEP-03 : HU: ERROR MESSAGE REVISED
CC               06-JAN-04 : HU: ALLOW FOR MISSING SATELLITES
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               01-AUG-05 : HU: EPOCH AS STRUCTURE
CC               24-MAY-07 : AG: USE OF FUNCTION PRN2PRN
CC               29-FEB-12 : RD: CORRECT ARRAY DIMENSIONS OF DUMMY ARGUMENTS
CC               05-MAR-12 : RD: USE LISTI4 AS MODULE NOW
CC               05-MAR-12 : RD: USE TYP2SVN AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
CC
      USE m_bern,   ONLY: lfnerr
      USE m_maxdim, ONLY: MAXSAT
      USE m_epoch,  ONLY: t_epoch, OPERATOR(.epochtoreal.)
      USE s_typ2svn
      USE f_djul
      USE s_exitrc
      USE f_prn2prn
      USE f_listi4
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IDAY  , IFRMAT, IHOUR , IMIN  , IMONTH, IRCODE,
     1          IREADE, ISAT  , IYEAR , LFN   , NSAT
C
      REAL*8    SEC   , XDAY
C
CCC       IMPLICIT INTEGER*4 (I-N)
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER*4    SATNUM(*)
      INTEGER*4    ACCPOS(4,*),ACCVEL(4,*)
      INTEGER*4    IEREC(2),IDEV(4),ICORR(6)
C
      REAL*8       POS(3,*)
      REAL*8       VEL(3,*),DTSATC(*),DDTSAT(*)
      REAL*8       SDEVP(4,*),SDEVV(4,*),CORRP(6,*),CORRV(6,*)
C
      CHARACTER*80 RECRD4
      CHARACTER*80 FILNAM
      CHARACTER*1  SATTYP(1)
      CHARACTER*1  EVTFLG(4,*)
C
      REAL*8       XPOS(3),XVEL(3),XDT
      INTEGER*4    IACC(4),ISVN(1)
      CHARACTER*1  EFLG(4)
C
      TYPE(t_epoch) :: tmjd
C
      IF(NSAT.GT.MAXSAT) GOTO 930
C
C ASSUME HEADER HAS BEEN READ - NEXT RECORD MUST BE DATE
C ------------------------------------------------------
      SATTYP=' '
      IRCODE=0
      IEREC =0
C
C READ RECORD
C -----------
      READ(LFN,1001,END=400) RECRD4
1001  FORMAT(A80)
C
C END OF FILE
C -----------
      IF (RECRD4(1:3).EQ.'EOF') GOTO 400
C
C EPOCH LINE
C ----------
      IF(RECRD4(1:2).EQ.' *'.OR.RECRD4(1:2).EQ.'* ') THEN
        IF (IFRMAT.LE.1) THEN
          READ(RECRD4,1005,ERR=940) IYEAR,IMONTH,IDAY,IHOUR,IMIN,SEC
1005      FORMAT(4X,I4,4(1X,I2),1X,F10.7)
        ELSE
          READ(RECRD4,1010,ERR=940) IYEAR,IMONTH,IDAY,IHOUR,IMIN,SEC
1010      FORMAT(3X,I4,4(1X,I2),1X,F11.8)
        ENDIF
C
C CONVERT YEAR, MONTH, DAY, HOUR, MINUTE, SECOND INTO MJD
C -------------------------------------------------------
        TMJD%FRAC= (IHOUR*3600D0+IMIN*60D0+SEC)/86400D0
        XDAY     = IDAY+1D-10
        TMJD%DAY = NINT(DJUL(IYEAR,IMONTH,XDAY))
C
C INITIALIZE RECORDS
C ------------------
        ISAT  =0
        POS(1:3,1:NSAT)=0D0
        VEL(1:3,1:NSAT)=0D0
        DTSATC(1:NSAT)=999999.999999D0
        DDTSAT(1:NSAT)=999999.999999D0
        ACCPOS(1:4,1:NSAT)=0
        ACCVEL(1:4,1:NSAT)=0
        EVTFLG(1:4,1:NSAT)=' '
        SDEVP(1:4,1:NSAT) =0D0
        SDEVV(1:4,1:NSAT) =0D0
        CORRP(1:6,1:NSAT) =0D0
      ELSE
        GOTO 960
      ENDIF
C
C LOOP OVER RECORDS FOR ONE EPOCH
C -------------------------------
      DO
C
C READ RECORD
C -----------
        READ(LFN,1001,END=400) RECRD4
C
C EPOCH LINE: BACKSPACE AND EXIT
C ----------
        IF(RECRD4(1:2).EQ.' *'.OR.RECRD4(1:2).EQ.'* '.OR.
     1     RECRD4(1:3).EQ.'EOF') THEN
          BACKSPACE (LFN)
          EXIT
        ENDIF
C
C FORMAT 0
C ========
        IF (IFRMAT .EQ. 0) THEN
          READ(RECRD4,1015,ERR=940) ISVN(1),XPOS(1:3)
1015      FORMAT(3X,I2,3F13.5)
C
C CONVERT FROM SP3 SV NUMBER TO SV NUMBER
          CALL TYP2SVN(1,ISVN,SATTYP,ISVN)
          ISVN(1)=PRN2PRN(ISVN(1),.epochtoreal.(TMJD))
          ISAT=LISTI4(0,MAXSAT,SATNUM,ISVN(1),NSAT)
          IF (ISAT.EQ.0) GOTO 910
C
C SAVE POSITION
          POS(1:3,ISAT)=XPOS(1:3)*1000D0
C
C FORMAT 1
C ========
        ELSE IF (IFRMAT .EQ. 1) THEN
          READ(RECRD4,1020,ERR=940) ISVN(1),XPOS(1:3),XVEL(1:3)
1020      FORMAT(3X,I2,3(1X,F12.5),3(1X,F11.8))
C
C CONVERT FROM SP3 SV NUMBER TO SV NUMBER
          CALL TYP2SVN(1,ISVN,SATTYP,ISVN)
          ISVN(1)=PRN2PRN(ISVN(1),.epochtoreal.(TMJD))
          ISAT=LISTI4(0,MAXSAT,SATNUM,ISVN(1),NSAT)
          IF (ISAT.EQ.0) GOTO 910
C
C SAVE POSITION
          POS(1:3,ISAT)=XPOS(1:3)*1000D0
          VEL(1:3,ISAT)=XVEL(1:3)*1D-1
C
C FORMAT 2...5
C ============
        ELSE IF (IFRMAT.EQ.2 .OR. IFRMAT.EQ.3 .OR.
     1           IFRMAT.EQ.4 .OR. IFRMAT.EQ.5) THEN
C
C POSITION RECORD
C ---------------
          IF (RECRD4(1:1).EQ.'P') THEN
            IF (IFRMAT.LE.3) THEN
              READ(RECRD4,1025,ERR=940) SATTYP(1),ISVN(1),XPOS(1:3),XDT
              IACC(1:4)=0
              EFLG(1:4)=' '
            ELSE
              READ(RECRD4,1025,ERR=940) SATTYP(1),ISVN(1),XPOS(1:3),XDT,
     1                                  IACC(1:4),EFLG(1:4)
            ENDIF
1025        FORMAT(1X,A1,I2,3F14.6,F14.6,3(1X,I2),1X,I3,1X,2A1,2X,2A1)
C
C CONVERT FROM SP3 SV NUMBER TO SV NUMBER
            CALL TYP2SVN(1,ISVN,SATTYP,ISVN)
            ISVN(1)=PRN2PRN(ISVN(1),.epochtoreal.(TMJD))
            ISAT=LISTI4(0,MAXSAT,SATNUM,ISVN(1),NSAT)
            IF (ISAT.EQ.0) GOTO 910
C
C SAVE POSITION
            POS(1:3,ISAT)   =XPOS(1:3)*1000D0
            IF(XDT.LT.999999D0) DTSATC(ISAT)=XDT*1D-6
            ACCPOS(1:4,ISAT)=IACC(1:4)
            EVTFLG(1:4,ISAT)=EFLG(1:4)
          ENDIF
C
C OPTIONAL EP-LINE FOR SP3C
C -------------------------
          IF (RECRD4(1:2).EQ.'EP'.AND.IREADE.EQ.1) THEN
            IF (ISAT.EQ.0) GOTO 920
            READ(RECRD4,1027,ERR=940) IDEV(1:4),ICORR(1:6)
1027        FORMAT(3X,3(1X,I4),(1X,I7),6(1X,I8))
            SDEVP(1:3,ISAT)=IDEV(1:3)*1D-3
            SDEVP(4,ISAT)  =IDEV(4)*1D-12
            CORRP(1:6,ISAT)=ICORR(1:6)*1D-7
            IEREC(1)=1
          ENDIF
C
C VELOCITY
          IF (RECRD4(1:1).EQ.'V') THEN
            IF (IFRMAT.LE.3) THEN
              READ(RECRD4,1025,ERR=940) SATTYP(1),ISVN(1),XVEL(1:3),XDT
              IACC(1:4)=0
            ELSE
              READ(RECRD4,1025,ERR=940) SATTYP(1),ISVN(1),XVEL(1:3),XDT,
     1                                              IACC(1:4)
            ENDIF
C
C CONVERT FROM SP3 SV NUMBER TO SV NUMBER
            CALL TYP2SVN(1,ISVN,SATTYP,ISVN)
            ISVN(1)=PRN2PRN(ISVN(1),.epochtoreal.(TMJD))
            ISAT=LISTI4(0,MAXSAT,SATNUM,ISVN(1),NSAT)
            IF (ISAT.EQ.0) GOTO 910
C
C SAVE VELOCITY
            VEL(1:3,ISAT)   =XVEL(1:3)*1D-1
            IF(XDT.LT.999999D0) DDTSAT(ISAT)=XDT*1D-10
            ACCVEL(1:4,ISAT)=IACC(1:4)
          ENDIF
C
C OPTIONAL EV-LINE FOR SP3C
C -------------------------
          IF (RECRD4(1:2).EQ.'EV'.AND.IREADE.EQ.1) THEN
            IF (ISAT.EQ.0) GOTO 920
            READ(RECRD4,1027,ERR=940) IDEV(1:4),ICORR(1:6)
            SDEVV(1:3,ISAT) =IDEV(1:3)*1D-7
            SDEVV(4,ISAT)   =IDEV(4)*1D-16
            CORRV(1:6,ISAT) =ICORR(1:6)*1D-7
            IEREC(2)=1
          ENDIF
        ELSE
          GOTO 900
        ENDIF
      ENDDO
      GOTO 999
C
400   IRCODE=1
      GOTO 999
C
C ERROR: NO CORRECT FORMAT
C ------------------------
900   INQUIRE(UNIT=LFN, NAME=FILNAM)
      WRITE(LFNERR,9001) IFRMAT, FILNAM, LFN
9001  FORMAT(/,' *** SR RDPREI : FORMAT IFRMAT=',I4,' IS NOT SUPPORTED'
     1       ,/,17X,'FILE NAME : ',A40,/,
     2       17X,'FILE UNIT : ',I6,/)
      CALL EXITRC(2)
C
C ERROR: SATELLITE NOT IN LIST
C ----------------------------
910   INQUIRE(UNIT=LFN, NAME=FILNAM)
      WRITE(LFNERR,9005) ISVN(1), FILNAM
9005  FORMAT(/,' *** SR RDPREI : SATELLITE',I3,
     1       ' NOT IN LIST',/,
     2       17X,'FILE NAME : ',A40,/)
      CALL EXITRC(2)
C
C ERROR: E-RECORD UNEXPECTED, NO SATELLITE DEFINED
C ------------------------------------------------
920   INQUIRE(UNIT=LFN, NAME=FILNAM)
      WRITE(LFNERR,9010) FILNAM, LFN
9010  FORMAT(/,' *** SR RDPREI : E-RECORD UNEXPECTED, ',
     1                          'NO SATELLITE DEFINED',/,
     1       17X,'FILE NAME : ',A40,/,
     2       17X,'FILE UNIT : ', I6,/)
      CALL EXITRC(2)
C
C ERROR: NUMBER OF SV'S > MAXIMUM
C -------------------------------
930   INQUIRE(UNIT=LFN, NAME=FILNAM)
      WRITE(LFNERR,9015) NSAT,MAXSAT,FILNAM
9015  FORMAT(/,' *** SR RDPREI : NUMBER OF SAT. (NSAT) > MAXIMUM',/,
     1       17X,'NUMBER OF SATELLITES : ',I3 ,/,
     2       17X,'MAXIMUM NUMBER OF SAT. ALLOWED : ',I3,/,
     3       17X,'FILE NAME: ',A40,/)
      CALL EXITRC(2)
C
C ERROR: READING ERROR
C --------------------
940   INQUIRE(UNIT=LFN, NAME=FILNAM)
      WRITE(LFNERR,9025) IYEAR,IMONTH,IDAY,IHOUR,IMIN,SEC,FILNAM
9025  FORMAT(/,' *** SR RDPREI : READING ERROR',/,
     1       17X,'EPOCH:',I5,4I3,F6.2,/,
     3       17X,'FILE NAME: ',A40,/)
      CALL EXITRC(2)
C
C ERROR: RECORD UNEXPECTED, NO EPOCH DEFINED
C ------------------------------------------
960   INQUIRE(UNIT=LFN, NAME=FILNAM)
      WRITE(LFNERR,9035) FILNAM, LFN
9035  FORMAT(/,' *** SR RDPREI : EPOCH RECORD EXPECTED',/,
     1       17X,'FILE NAME : ',A40,/,
     2       17X,'FILE UNIT : ', I6,/)
      CALL EXITRC(2)
C
C END OF SUBROUTINE
C -----------------
999   RETURN
      END SUBROUTINE

      END MODULE
