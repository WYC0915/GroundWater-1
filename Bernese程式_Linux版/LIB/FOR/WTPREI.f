      MODULE s_WTPREI
      CONTAINS

C*
      SUBROUTINE WTPREI(LFN,IFRMAT,IWRTE,NSAT,SATNUM,TMJD,POS,VEL,
     1                  DTSATC,DDTSAT,ACCPOS,ACCVEL,EVTFLG,SDEVP,SDEVV,
     2                  CORRP,CORRV,IRCODE)
CC
CC NAME       :  WTPREI
CC
CC PURPOSE    :  WRITE INFROMATION FOR ONE OBSERVATION EPOCH INTO A
CC               PRECISE ORBIT FILE (ASSUMING THAT THE FILE IS OPEN)
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
CC               IWRTE  : =1: WRITE OPTIONAL EP AND EV RECORDS I*4(2)
CC                            FOR POS AND VEL
CC               NSAT   : NUMBER OF SATELLITES                 I*4
CC               SATNUM : SATELLITE NUMBERS                    I*4(*)
CC               TMJD   : TIME OF EPOCH                        T_EPOCH
CC               POS    : SATELLITE POSITION (M)               R*8(*,*)
CC               VEL    : SATELLITE VELOCITIY (M/S)            R*8(*,*)
CC               DTSATC : SATELLITE CLOCK ERROR (SEC)          R*8(*)
CC               DDTSAT : RATE OF CHANGE OF SAT. CLOCK ERROR   R*8(*)
CC                        (SEC/SEC)
CC               ACCPOS : ACCURACY CODE FOR POS AND CLK        I*4(4,*)
CC               ACCVEL : ACCURACY CODE FOR VEL AND CLK RATE   I*4(4,*)
CC               EVTFLG : EVENT FLAG FOR SATELLITE             CH*1(4,*)
CC               SDEVP  : SDEV FOR POS AND CLK                 R*8(4,*)
CC               SDEVV  : SDEV FOR VEL AND CLK RATE            R*8(4,*)
CC               CORRP  : CORRELATION FOR POS AND CLK          R*8(6,*)
CC               CORRV  : CORRELATION FOR VEL AND CLK RATE     R*8(6,*)
CC               IRCODE : RETURN CODE (ALWAYS ZERO AT PRESENT) I*4
CC
CC REMARKS    :  IN SP3 POSITIONS IN METERS
CC                      VELOCITY  IN DM/S
CC                      CLOCK     IN US
CC                      CLOCK RATE IN 10**-4 US/S
CC               SP3C   SDEVP IN METERS, SEC
CC                      SDEVV IN M/SEC, SEC/SEC
CC
CC
CC AUTHOR     :  D.INEICHEN
CC
CC VERSION    :  4.1
CC
CC CREATED    :  09-JUL-97
CC
CC CHANGES    :  23-JUL-97 : DI: CALL JMT WITH TEPOCH+1D-10
CC               11-MAR-98 : DI: USE LOCAL ARRAY SATNU2
CC               12-MAR-98 : DI: SATTYP=G (GPS) IN MIXED FILES
CC               14-FEB-01 : DS: CALL SVN2TYP (" ","G","R","L")
CC               08-NOV-02 : HU: SP3-c IMPLEMENTED
CC               02-DEC-02 : HU: WRITE ZERO AC-CODES AS BLANKS
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               08-JUN-03 : HU: WRITE POS AND VEL FOR SP1 IN CORRECT ORDER
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               23-JUL-05 : HU: MISSING DECLARATIONS ADDED
CC               01-AUG-05 : HU: EPOCH AS STRUCTURE, CALL RADGMS WITH I=13
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE m_maxdim, ONLY: MAXSAT
      USE m_epoch,  ONLY: t_epoch
      USE s_svn2typ
      USE s_exitrc
      USE s_jmt
      USE s_radgms
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IDAY  , IFRMAT, IHOUR , II    , IMIN  , IMONTH,
     1          IRCODE, ISAT  , IYEAR , K     , LFN   , NSAT  , IEPO
C
      REAL*8    DEV   , SEC   , XDAY1 , TEPO  , FEPO
C
CCC       IMPLICIT INTEGER*4 (I-N)
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      REAL*8       POS(3,*),VEL(3,*)
      REAL*8       DTSATC(*),DDTSAT(*)
      REAL*8       SDEVP(4,*),SDEVV(4,*),CORRP(6,*),CORRV(6,*)
C
      INTEGER*4    SATNUM(*),SATNU2(85)
      INTEGER*4    ACCPOS(4,*),ACCVEL(4,*)
      INTEGER*4    IDEV(4),ICORR(6),IWRTE(2)
C
      CHARACTER*40 FILNAM
      CHARACTER*80 STRING
      CHARACTER*1  SATTYP(85),EVTFLG(4,*)
C
      TYPE(t_epoch) :: TMJD
C
C CONVERT MJD INTO YEAR, MONTH, DAY, HOUR, MIN, SEC
C -------------------------------------------------
      IF (NSAT.GT.MAXSAT) GOTO 910
      FEPO =TMJD%FRAC+1D-14
      IEPO =TMJD%DAY+INT(FEPO)
      FEPO =FEPO-INT(FEPO)
      TEPO =IEPO
      CALL JMT(TEPO,IYEAR,IMONTH,XDAY1)
      IDAY=INT(XDAY1)
C
      CALL RADGMS(13,FEPO,' ',IHOUR,IMIN,SEC)
C
C COPY GLOBAL ARRAY SATNUM TO LOCAL ARRAY
C ---------------------------------------
      CALL SVN2TYP(NSAT,SATNUM,SATNU2,SATTYP)
C
C WRITE EPOCH LINE
C ----------------
      IF (IFRMAT.LE.1) THEN
        WRITE(LFN,1001) IYEAR,IMONTH,IDAY,IHOUR,IMIN,SEC
1001    FORMAT(1X,'*  ',I4,4(1X,I2),1X,F10.7)
      ELSEIF (IFRMAT.EQ.2. OR .IFRMAT.EQ.3) THEN
        WRITE(LFN,1005) IYEAR,IMONTH,IDAY,IHOUR,IMIN,SEC
1005    FORMAT('*  ',I4,4(1X,I2),1X,F11.8)
      ELSEIF (IFRMAT.EQ.4. OR .IFRMAT.EQ.5) THEN
        WRITE(LFN,1005) IYEAR,IMONTH,IDAY,IHOUR,IMIN,SEC
      ELSE
        GOTO 900
      END IF
C
C CONVERT POS,VEL,DTSATC AND DDTSAT INTO NEEDED UNITS
C ---------------------------------------------------
      DO ISAT=1,NSAT
        IF (IFRMAT.EQ.4.OR.IFRMAT.EQ.5) THEN
          IF (SATTYP(ISAT).EQ.' ') SATTYP(ISAT)='G'
          DO I=1,4
            ACCPOS(I,ISAT)=MIN(999,ACCPOS(I,ISAT))
            ACCVEL(I,ISAT)=MIN(999,ACCVEL(I,ISAT))
          ENDDO
        ENDIF
        DO I=1,3
          POS(I,ISAT)=POS(I,ISAT)*1D-3
          IF (IFRMAT.EQ.1 .OR. IFRMAT.EQ.3 .OR. IFRMAT.EQ.5)THEN
            VEL(I,ISAT)=VEL(I,ISAT)*10.0D0
          ENDIF
        ENDDO
        IF(DTSATC(ISAT).LT.999999D0) DTSATC(ISAT)=DTSATC(ISAT)*1D6
        IF(DDTSAT(ISAT).LT.999999D0 .AND. IFRMAT.EQ.3)
     1                              DDTSAT(ISAT)=DDTSAT(ISAT)*1D10
      ENDDO
C
C  LOOP OVER ALL SATELLITES
C  ------------------------
      DO II=1,NSAT
C
C       WRITE OBSERVATIONS FOR ALL SATELLITES
C       -------------------------------------
        IF(IFRMAT.EQ.0) THEN
          WRITE(LFN,1010) SATNU2(II),(POS(K,II),K=1,3)
1010      FORMAT(' SV',I2,3(1X,F12.5))
        ELSEIF (IFRMAT.EQ.1) THEN
          WRITE(LFN,1015) SATNU2(II),
     1                    (POS(K,II),K=1,3),(VEL(K,II),K=1,3)
1015      FORMAT(' SV',I2,3(1X,F12.5),3(1X,F11.8))
        ELSEIF (IFRMAT.EQ.2. OR .IFRMAT.EQ.3) THEN
          WRITE(LFN,3020) SATTYP(II),SATNU2(II),
     1                    (POS(K,II),K=1,3),DTSATC(II)
3020      FORMAT('P',A1,I2,3F14.6,F14.6)
C
C         VELOCITY RECORD ?
C         -----------------
          IF(IFRMAT.EQ.3) THEN
            WRITE(LFN,3025) SATTYP(II),SATNU2(II),
     1                      (VEL(K,II),K=1,3),DDTSAT(II)
3025        FORMAT('V',A1,I2,3F14.6,F14.6)
          ENDIF
C
C SP3-C
C -----
        ELSEIF (IFRMAT.EQ.4. OR .IFRMAT.EQ.5) THEN
          WRITE(STRING,4020) SATTYP(II),SATNU2(II),
     1                       POS(1:3,II),DTSATC(II),
     2                       ACCPOS(1:4,II),EVTFLG(1:4,II)
4020      FORMAT('P',A1,I2.2,3F14.6,F14.6,3(1X,I2),1X,I3,1X,2A1,2X,2A1)
          IF (ACCPOS(1,II).EQ.0) STRING(62:63)='  '
          IF (ACCPOS(2,II).EQ.0) STRING(65:66)='  '
          IF (ACCPOS(3,II).EQ.0) STRING(68:69)='  '
          IF (ACCPOS(4,II).EQ.0) STRING(71:73)='   '
          WRITE(LFN,"(A)") TRIM(STRING)
C
C OPTIONAL EP-RECORD
          IF (IWRTE(1).EQ.1) THEN
            DO I=1,3
              DEV     =SDEVP(I,II)*1D3
              IDEV(I) =IDNINT(MIN(9999D0,DEV))
            ENDDO
            DEV       =SDEVP(4,II)*1D12
            IDEV(4)   =IDNINT(MIN(9999999D0,DEV))
            DO I=1,6
              ICORR(I)=CORRP(I,II)*1D7
              IF (ICORR(I).GT. 9999999) ICORR(I)= 9999999
              IF (ICORR(I).LT.-9999999) ICORR(I)=-9999999
            ENDDO
            WRITE(LFN,4022) IDEV(1:4),ICORR(1:6)
4022        FORMAT('EP',1X,3(1X,I4),1X,I7,6(1X,I8))
          ENDIF
C
C         VELOCITY RECORD ?
C         -----------------
          IF(IFRMAT.EQ.5) THEN
            WRITE(STRING,4025) SATTYP(II),SATNU2(II),
     1                         VEL(1:3,II),DDTSAT(II),ACCVEL(1:4,II)
4025        FORMAT('V',A1,I2.2,3F14.6,F14.6,3(1X,I2),1X,I3)
            IF (ACCVEL(1,II).EQ.0) STRING(62:63)='  '
            IF (ACCVEL(2,II).EQ.0) STRING(65:66)='  '
            IF (ACCVEL(3,II).EQ.0) STRING(68:69)='  '
            IF (ACCVEL(4,II).EQ.0) STRING(71:73)='   '
            WRITE(LFN,"(A)") TRIM(STRING)
C
C OPTIONAL EV-RECORD
            IF (IWRTE(2).EQ.1) THEN
              DO I=1,3
                DEV     =SDEVV(I,II)*1D7
                IDEV(I) =IDNINT(MIN(9999D0,DEV))
              ENDDO
              DEV       =SDEVV(4,II)*1D16
              IDEV(4)   =IDNINT(MIN(9999999D0,DEV))
              DO I=1,6
                ICORR(I)=CORRV(I,II)*1D7
                IF (ICORR(I).GT. 9999999) ICORR(I)= 9999999
                IF (ICORR(I).LT.-9999999) ICORR(I)=-9999999
              ENDDO
              WRITE(LFN,4027) IDEV(1:4),ICORR(1:6)
4027          FORMAT('EV',1X,3(1X,I4),1X,I7,6(1X,I8))
            ENDIF
          ENDIF
        ELSE
          GOTO 900
        ENDIF
      ENDDO
C
      GOTO 999
C
C ERROR: FORMAT NR. 'IFRMAT' NOT CORRECT
C --------------------------------------
900   INQUIRE(UNIT=LFN, NAME=FILNAM)
      WRITE(LFNERR,9001) IFRMAT, FILNAM, LFN
9001  FORMAT(/,' *** SR WTPREI : FORMAT IFRMAT=',I4,' IS NOT SUPPORTED'
     1       ,/,17X,'FILE NAME : ',A40,/,
     2       17X,'FILE UNIT : ',I6,/)
      CALL EXITRC(2)
C
C ERROR: NUMBER OF SV'S > MAXIMUM
C -------------------------------
910   INQUIRE(UNIT=LFN, NAME=FILNAM)
      WRITE(LFNERR,9005) NSAT,MAXSAT,FILNAM
9005  FORMAT(/,' *** SR WTPREI : NUMBER OF SAT. (NSAT) > MAXIMUM',/,
     1       17X,'NUMBER OF SATELLITES : ',I3 ,/,
     2       17X,'MAXIMUM NUMBER OF SAT. ALLOWED : ',I3,/,
     3       17X,'FILE NAME: ',A40,/)
      CALL EXITRC(2)
C
C END OF SUBROUTINE
C -----------------
999   IRCODE=0
      RETURN
      END SUBROUTINE

      END MODULE
