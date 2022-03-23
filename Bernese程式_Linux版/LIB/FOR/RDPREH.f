      MODULE s_RDPREH
      CONTAINS

C*
      SUBROUTINE RDPREH(FILNAM,LFN,IFRMAT,NSAT,SATNUM,SATWGT,TFIRST,
     1                  NEPO,DTTAB,TITLE,DATDES,COOSYS,ORBTYP,AGENCY,
     2                  FILTYP,TIMSYS,BASPOS,BASCLK)
CC
CC NAME       :  RDRREH
CC
CC PURPOSE    :  READ HEADER OF PRECISE ORBIT FILE, RETURN RELEVANT
CC               INFORMATION
CC
CC PARAMETERS :
CC         IN :  FILNAM : FILE NAME OF PRECISE ORBIT FILLE     CH*(*)
CC               LFN    : LOGICAL FILE NUMBER FOR INPUT FILE   I*4
CC                        = -1: USE LFNLOC AS LFN AND CLOSE FILE
CC                              AFTER READING THE HEADER
CC                        >= 0: LFN IS USED FOR OPENING THE FILE
CC                              AND FILE STAYS OPEN AFTER READING
CC                              THE HEADER
CC        OUT :  IFRMAT : FORMAT TYPE                          I*4
CC                        =0 : SHORT FORMAT (SV1, POS)
CC                        =1 : LARGE FORMAT (SV1, POS+VEL)
CC                        =2 : SP3 FORMAT (POS+CLOCKS)
CC                        =3 : SP3 FORMAT (POS+VEL+CLOCKS)
CC                        =4 : SP3-c FORMAT (POS+CLOCKS)
CC                        =5 : SP3-c FORMAT (POS+VEL+CLOCKS)
CC               NSAT   : NUMBER OF SATELLITES IN FILE         I*4
CC               SATNUM : SATELLITE NUMBERS                    I*4(*)
CC               SATWGT : SATELLITE SPECIFIC ACCURACIES        I*4(*)
CC               TFIRST : TIME OF FIRST EPOCH (MJD)            R*8
CC               NEPO   : NUMBER OF EPOCHS                     I*4
CC               DTTAB  : TABULAR INTERVAL (SEC)               R*8
CC               TITLE  : TITLE LINES                          CH*57(4)
CC               DATDES : DATA DESCRIPTION                     CH*5
CC               COOSYS : COORDINATE SYSTEM                    CH*5
CC               ORBTYP : ORBIT TYPE                           CH*3
CC               AGENCY : AGENCY GENERATING ORBIT              CH*4
CC               FILTYP : FILE TYPE "G ","M ","R ","L ","E "   CH*2
CC                        FOR SP3-c
CC               TIMSYS : TIME SYSTEM, "GPS", "UTC", FOR SP3-c CH*3
CC               BASPOS : BASE FOR POSITION AND VELOCITY       R*8
CC                        EPOCH WISE ACCURACY CODES, FOR SP3-c
CC               BASCLK : BASE FOR CLOCK EPOCH WISE ACCURACY   R*8
CC                        CODES, FOR SP3-c
CC
CC REMARKS    :  NOT TESTED FOR THE OLD FORMATS SP1 AND SP2 !!
CC
CC AUTHOR     :  D. INEICHEN
CC
CC VERSION    :  4.1
CC
CC CREATED    :  09-JUL-97
CC
CC CHANGES    :  22-JUL-97 : MR: USE LOCAL VARIABLE FOR "LFN"
CC               28-MAY-01 : HB: ADD SR TYP2SVN (SUPPORT OF LEOs)
CC               02-NOV-02 : HU: SP3-c IMPLEMENTED
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               24-MAY-07 : AG: USE OF FUNCTION PRN2PRN
CC               28-MAR-12 : RD: USE SVNSYS AS MODULE NOW
CC               28-MAR-12 : RD: USE TYP2SVN AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnerr, lfnloc
      USE M_GLOBAL, ONLY: g_svnsys
      USE m_maxdim, ONLY: MAXSAT
      USE s_typ2svn
      USE s_opnfil
      USE s_opnerr
      USE s_exitrc
      USE f_prn2prn
      USE f_svnsys
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IFRMAT, II    , IMJD  , IOSTAT, J     , LFN   ,
     1          LFN1  , NEPO  , NSAT
C
      REAL*8    DTTAB , FMJD
C
CCC       IMPLICIT INTEGER*4 (I-N)
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CC
      INTEGER*4    SATNUM(*), SATWGT(*)
      INTEGER*4    SATNU2(85), SATWG2(85)
C
      REAL*8       TFIRST, BASPOS, BASCLK
C
      CHARACTER*80 RECRD1,RECRD2
      CHARACTER*57 TITLE(4)
      CHARACTER*(*) FILNAM
      CHARACTER*5  COOSYS,DATDES
      CHARACTER*4  AGENCY
      CHARACTER*3  ORBTYP,TIMSYS
      CHARACTER*2  FILTYP
      CHARACTER*1  SATTYP(85)
C
C INITIALISATION OF VARIABLES
C ---------------------------
      SATNU2=0
      SATWG2=0
      SATTYP=' '
      TITLE =' '
      DATDES=' '
      COOSYS=' '
      ORBTYP=' '
      AGENCY=' '
      FILTYP=' '
      TIMSYS=' '
      BASPOS=0D0
      BASCLK=0D0
C
C OPEN INPUT FILE
C ---------------
      LFN1=LFN
      IF( LFN.EQ.-1) THEN
        LFN1=LFNLOC
      ENDIF
C
      CALL OPNFIL(LFN1,FILNAM,'OLD','FORMATTED','READONLY',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFN1,IOSTAT,FILNAM,'RDPREH')
C
C READ THE FIRST AND THE SECOND RECORD
C ------------------------------------
      DO
        READ(LFN1,1001,ERR=901,END=911) RECRD1
        IF (RECRD1(1:1).EQ.'#'.OR.RECRD1(2:2).EQ.'#') EXIT
      ENDDO
      READ(LFN1,1001,ERR=901,END=911) RECRD2
1001  FORMAT(A80)
C
C DETECT FORMAT TYPE
C ------------------
      IF(RECRD2(2:3).EQ.'##') THEN
        IFRMAT=0
      ELSE IF(RECRD2(1:2).EQ.'##') THEN
        IF (RECRD1(1:2).EQ.'#c') THEN
          IFRMAT=4
          IF(RECRD1(3:3).EQ.'V') IFRMAT=5
        ELSE
          IFRMAT=2
          IF(RECRD1(3:3).EQ.'V') IFRMAT=3
        ENDIF
      ELSE
        IFRMAT=1
      ENDIF
C
C HEADER RECORDS FOR SHORT FORMAT (POSITIONS ONLY)
C ------------------------------------------------
      IF(IFRMAT.EQ.0) THEN
        READ(RECRD1,1002) NEPO,ORBTYP(1:1),AGENCY(1:3)
        READ(RECRD2,1003) DTTAB,IMJD,FMJD
        READ(LFN1,1004,ERR=901,END=911) NSAT,(SATNU2(I),I=1,17)
        READ(LFN1,1005,ERR=901,END=911) (SATNU2(I),I=18,34),COOSYS(1:2)
1002    FORMAT(32X,I6,1X,A1,1X,A3)
1003    FORMAT(4X,F14.7,3X,I5,3X,F15.14)
1004    FORMAT(4X,I3,3X,17I2)
1005    FORMAT(4X,17I2,A2)
C
C HEADER RECORDS FOR LARGE FORMAT (POSITIONS AND VELOCITIES)
C ----------------------------------------------------------
      ELSE IF(IFRMAT.EQ.1) THEN
        READ(RECRD1,2001) DTTAB,IMJD,FMJD,NEPO,ORBTYP(1:1),AGENCY(1:3)
        READ(RECRD2,2002) NSAT,(SATNU2(I),I=1,34),COOSYS(1:2)
2001    FORMAT(32X,F14.7,1X,I5,1X,F15.14,1X,I6,A1,1X,A3)
2002    FORMAT(3X,I2,1X,34I2,A2)
C
C HEADER RECORDS FOR THE NEW FORMAT (POSITIONS AND CLOCKS)
C --------------------------------------------------------
      ELSE IF (IFRMAT.GE.2.AND.IFRMAT.LE.5) THEN
        READ(RECRD1,3001) NEPO,DATDES,COOSYS,ORBTYP,AGENCY
        READ(RECRD2,3002) DTTAB,IMJD,FMJD
        READ(LFN1,3003,ERR=901,END=911) NSAT,
     1      (SATTYP(I),SATNU2(I),I=1,17)
        DO II=18,69,17
          READ(LFN1,3004,ERR=901,END=911) (SATTYP(J),SATNU2(J),
     1         J=II,II+16)
        ENDDO
        DO II=1,69,17
          READ(LFN1,3005,ERR=901,END=911) (SATWG2(J),J=II,II+16)
        ENDDO
        IF (IFRMAT.EQ.2.OR.IFRMAT.EQ.3) THEN
          READ(LFN1,3006,END=911)
        ELSE
          READ(LFN1,3007,ERR=901,END=911)  FILTYP,TIMSYS,BASPOS,BASCLK
        ENDIF
        READ(LFN1,3008,ERR=901,END=911)  (TITLE(I),I=1,4)
3001    FORMAT(32X,I7,1X,A5,1X,A5,1X,A3,1X,A4)
3002    FORMAT(24X,F14.8,1X,I5,1X,F15.13)
3003    FORMAT(4X,I2,3X,17(A1,I2))
3004    FORMAT(9X,17(A1,I2))
3005    FORMAT(9X,17(1X,I2))
3006    FORMAT(/////)
3007    FORMAT(3X,A2,4X,A3,//,3X,F10.7,1X,F12.7,///)
3008    FORMAT(3(3X,A57,/),3X,A57)
C FILE TYP
        IF (SVNSYS(10,NSAT,SATNU2)) THEN
          FILTYP=g_svnsys(10)
        ELSE
          FILTYP=SATTYP(1)
        ENDIF
      ENDIF
C
C NUMBER OF SATELLITES NOT GREATER THAN MAXIMUM NUMBER ?
C ------------------------------------------------------
      IF(NSAT.GT.MAXSAT) GOTO 921
C
C START OF GPS-MEASUREMENTS, SATELLITE NUMBERS
C --------------------------------------------
      TFIRST=IMJD+FMJD

      DO I=1,NSAT
        SATNUM(I)=SATNU2(I)
        SATWGT(I)=SATWG2(I)
      ENDDO
C
C CONVERT FROM SP3 SV NUMBER TO SV NUMBER
C ---------------------------------------
      CALL TYP2SVN(NSAT,SATNUM,SATTYP,SATNUM)
C
C RENAME SAT IF NECESSARY
C -----------------------
      DO I=1,NSAT
        SATNUM(I)=PRN2PRN(SATNUM(I),TFIRST)
      ENDDO
C
C CLOSE FILE
C ----------
      IF (LFN.EQ.-1) CLOSE(UNIT=LFN1)
      GOTO 999
C
C ERROR: FILE READING
C -------------------
901   WRITE(LFNERR,4000) TRIM(FILNAM),LFN1
4000  FORMAT(/,' *** SR RDPREH : FILE READING FAILED',/,
     1       17X,'FILE NAME : ',A,/,
     2       17X,'FILE UNIT : ',I6,/)
      CALL EXITRC(2)
C
C ERROR: NO COMPLETE HEADER FOUND
C -------------------------------
911   WRITE(LFNERR,4001) TRIM(FILNAM),LFN1
4001  FORMAT(/,' *** SR RDPREH : NO COMPLETE HEADER FOUND',/,
     1       17X,'FILE NAME : ',A,/,
     2       17X,'FILE UNIT : ',I6,/)
      CALL EXITRC(2)
C
C ERROR: NUMBER OF SV'S > MAXIMUM
C -------------------------------
921   WRITE(LFNERR,4002) NSAT,MAXSAT,TRIM(FILNAM)
4002  FORMAT(/,' *** SR RDPREH : NUMBER OF SAT. (NSAT) > MAXIMUM',/,
     1       17X,'NUMBER OF SATELLITES : ',I3 ,/,
     2       17X,'MAXIMUM NUMBER OF SAT. ALLOWED : ',I3,/,
     3       17X,'FILE NAME: ',A,/)
      CALL EXITRC(2)
C
C END OF SUBROUTINE
C -----------------
999   RETURN
      END SUBROUTINE

      END MODULE
