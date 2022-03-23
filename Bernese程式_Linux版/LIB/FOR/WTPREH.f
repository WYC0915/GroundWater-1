      MODULE s_WTPREH
      CONTAINS

C*
      SUBROUTINE WTPREH(FILNAM,LFN,IFRMAT,NSAT,SATNUM,SATWGT,TFIRST,
     1                  NEPO,DTTAB,TITLE,DATDES,COOSYS,ORBTYP,AGENCY,
     2                  TIMSYS,BASPOS,BASCLK)
CC
CC NAME       :  WTPREH
CC
CC PURPOSE    :  WRITE HEADER OF PRECISE ORBIT FILE, LEAVE THE
CC               ORBIT FILE OPEN
CC
CC PARAMETERS :
CC         IN :  FILNAM : FILE NAME OF PRECISE ORBIT FILE      CH*(*)
CC               LFN    : LOGICAL FILE NUMBER FOR INPUT FILE   I*4
CC               IFRMAT : FORMAT TYPE                          I*4
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
CC               TITLE  : TILE LINES                           CH*57(4)
CC               DATDES : DATA DESCRIPTION                     CH*5
CC               COOSYS : COORDINATE SYSTEM                    CH*5
CC               ORBTYP : ORBIT TYPE                           CH*3
CC               AGENCY : AGENCY GENERATING ORBIT              CH*4
CC               TIMSYS : TIME SYSTEM, "GPS", "UTC", FOR SP3-c CH*3
CC               BASPOS : BASE FOR POSITION AND VELOCITY       R*8
CC                        EPOCH WISE ACCURACY CODES, FOR SP3-c
CC               BASCLK : BASE FOR CLOCK EPOCH WISE ACCURACY   R*8
CC                        CODES, FOR SP3-c
CC
CC REMARKS    :  NOT TESTED FOR OLD FORMATS SP1 AND SP2 !!
CC
CC AUTHOR     :  D. INEICHEN
CC
CC VERSION    :  4.1
CC
CC CREATED    :  09-JUL-97
CC
CC CHANGES    :  23-JUL-97 : DI: CALL JMT WITH TFIRST+1D-10
CC               11-MAR-98 : DI: USE LOCAL ARRAYS SATNU2,SATWG2
CC               12-MAR-98 : DI: SATTYP=G (GPS) IN MIXED FILES
CC               18-NOV-98 : MR: GLONASS FORMAT ADDITIONS
CC               15-FEB-01 : DS: CALL SVN2TYP
CC               19-FEB-01 : DS: LEO FORMAT SUPPORT
CC               08-NOV-02 : HU: SP3C FORMAT SUPPORT
CC               14-NOV-02 : HU: HANDLE BLANK AGENCY STRING
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               01-DEC-08 : DT: CONVERSION FROM GPS-TIME TO UTC,TAI,GAL
CC               28-MAR-12 : RD: USE SVNSYS AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE M_BERN,   ONLY: lfnerr
      USE M_GLOBAL, ONLY: g_svnsys
      USE m_maxdim, ONLY: maxsat
      USE s_opnfil
      USE s_mjdgps
      USE s_opnerr
      USE f_svnsys
      USE s_svn2typ
      USE s_exitrc
      USE s_jmt
      USE s_radgms
      USE f_dgpsut
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IDAY1 , IFRMAT, IHOUR1, II    , III   , IMIN1 ,
     1          IMJD  , IMONT1, IOSTAT, IYEAR1, J     , LFN   , NEPO  ,
     2          NSAT  , NWEEK
C
      REAL*8    BASCLK, BASPOS, DTTAB , FMJD  , SEC1  , SECOND, TFIRST,
     1          XDAY1 , epoch , tcor
C
CCC       IMPLICIT REAL*8(A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
C
      INTEGER*4    SATNUM(*), SATNU2(85), SATWGT(*), SATWG2(85)
C
      CHARACTER*57 TITLE(4)
      CHARACTER*(*) FILNAM
      CHARACTER*5  COOSYS,DATDES
      CHARACTER*4  AGENCY,BLANK,JUSTR
      CHARACTER*3  ORBTYP,SYSCHR,TIMSYS,SATCHR(85)
      CHARACTER*1  SATTYP(85),POSVEL,FMTCHR,TYPCHR
C
      BLANK = '    '
C
C OPEN PRECISE ORBIT FILE FOR OUTPUT
C ----------------------------------
      CALL OPNFIL(LFN,FILNAM,'UNKNOWN','FORMATTED',' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFN,IOSTAT,FILNAM,'WTPREH')
C
C COPY GLOBAL ARRAYS SATNUM AND SATWGT TO LOCAL ARRAYS OF SIZE 85
C ---------------------------------------------------------------
      DO III=1,85
         SATNU2(III)=0
         SATWG2(III)=0
         SATTYP(III)=' '
      ENDDO
C
      IF (NSAT.GT.MAXSAT) GOTO 901
C
C CONVERT FROM SV NUMBER TO SV TYPE AND SV NUMBER
C -----------------------------------------------
      CALL SVN2TYP(NSAT,SATNUM,SATNU2,SATTYP)
C
C MIXED SATELLITE SYSTEMS
C -----------------------
      IF (IFRMAT.EQ.2 .OR. IFRMAT.EQ.3) THEN
        IF (SVNSYS(10,NSAT,SATNUM)) THEN
          FMTCHR='b'
          SYSCHR='ccc'
          TYPCHR=g_svnsys(10)
          IF (SVNSYS(0,NSAT,SATNUM)) SYSCHR='GPS'
        ELSEIF (SVNSYS(0,NSAT,SATNUM)) THEN
          FMTCHR='a'
          SYSCHR='ccc'
          TYPCHR='c'
        ELSE
          FMTCHR='b'
          SYSCHR='ccc'
          TYPCHR=SATTYP(1)
        ENDIF
      ELSEIF (IFRMAT.EQ.4 .OR. IFRMAT.EQ.5) THEN
        FMTCHR='c'
        DO I=1,85
          SATCHR(I)='  0'
        ENDDO
        DO I=1,NSAT
          IF (SATTYP(I).EQ.' ') SATTYP(I)='G'
          WRITE(SATCHR(I),"(A1,I2.2)") SATTYP(I),SATNU2(I)
        ENDDO
        SYSCHR=TIMSYS
        IF (SVNSYS(10,NSAT,SATNUM)) THEN
          TYPCHR=g_svnsys(10)
        ELSE
          TYPCHR=SATTYP(1)
        ENDIF
      ENDIF
C
C FILL LOCAL ARRAY
C -----------------------------------------------
      DO III=1,NSAT
        SATWG2(III)=SATWGT(III)
      ENDDO
C
C CONVERT MODIFIED JULIAN DATE INTO YEAR, MONTH, DAY,
C HOUR, MINUTE AND SECOND AND INTO GPS TIME
C ---------------------------------------------------
      tcor = 0.D0
      IF (TIMSYS=='GPS'.OR.TIMSYS=='GAL') THEN
        tcor = 1D-10
C
      ELSEIF (TIMSYS=='UTC'.OR.TIMSYS=='GLO') THEN
        tcor = 1D-10 - dgpsut(TFIRST)/86400.0
C
      ELSEIF (TIMSYS=='TAI') THEN
        tcor = 1D-10 + 19.0/86400.0
      ENDIF
C
      epoch = TFIRST + tcor
C
      IF (ABS(tcor)>1D-10) THEN
        WRITE(LFNERR,"(/,' ### SR WTPREH: The time system of the',
     1                   ' PRE-file is changed to: ',A,/)")
     2                   timsys
      END IF
C
      IMJD=INT(epoch)
      FMJD= DNINT(MOD(epoch,1D0)*8.64D8)/8.64D8
C
      CALL JMT(epoch,IYEAR1,IMONT1,XDAY1)
      IDAY1=INT(XDAY1)
      CALL RADGMS(3,FMJD,' ',IHOUR1,IMIN1,SEC1)
C
      CALL MJDGPS(epoch,SECOND,NWEEK)
      SECOND=DNINT(SECOND*1D4)/1D4
C
C
C WRITE FIRST TWO RESP. FOUR LINES OF PRECISE ORBIT FILE
C -------------------------------------------------------
C
      IF(IFRMAT.EQ.0) THEN
        WRITE(LFN,1003) IYEAR1,IMONT1,IDAY1,IHOUR1,IMIN1,SEC1,NEPO,
     1                     ORBTYP(1:1),AGENCY(1:3)
        WRITE(LFN,1004) DTTAB,IMJD,FMJD
        WRITE(LFN,1005) NSAT,(SATNU2(I),I=1,17)
        WRITE(LFN,1006) (SATNU2(I),I=18,34),COOSYS(1:2),NWEEK
1003    FORMAT(1X,'#  ',I4,4I3,1X,F10.7,1X,I6,1X,A1,1X,A3)
1004    FORMAT(1X,'## ',F14.7,3X,I5,3X,F15.14)
1005    FORMAT(1X,'+  ',I3,3X,17I2)
1006    FORMAT(1X,'++ ',17I2,A2,I4)
C
      ELSEIF (IFRMAT.EQ.1) THEN
        WRITE(LFN,2003) IYEAR1,IMONT1,IDAY1,IHOUR1,IMIN1,SEC1,DTTAB,
     1                    IMJD,FMJD,NEPO,ORBTYP(1:1),AGENCY(1:3)
        WRITE(LFN,2005) NSAT,(SATNU2(I),I=1,34),
     1                     COOSYS(1:2),NWEEK
2003    FORMAT(1X,'#  ',I4,4I3,1X,F10.7,1X,F14.7,1X,I5,1X,F15.14,1X,
     1         I6,A1,1X,A3)
2005    FORMAT(1X,'+ ',I2,1X,34I2,A2,I4)
C
      ELSEIF (IFRMAT.EQ.2. OR .IFRMAT.EQ.3) THEN
        POSVEL='P'
        IF(IFRMAT.EQ.3) POSVEL='V'
        WRITE(LFN,3003) FMTCHR,POSVEL,IYEAR1,IMONT1,IDAY1 ,IHOUR1,
     1                  IMIN1 ,SEC1  ,NEPO  ,DATDES,COOSYS,ORBTYP,
     2                  AGENCY
        WRITE(LFN,3004) NWEEK,SECOND,DTTAB,IMJD,FMJD
        WRITE(LFN,3005) NSAT,(SATTYP(I),SATNU2(I),I=1,17)
        DO II=18,69,17
          WRITE(LFN,3006) (SATTYP(J),SATNU2(J),J=II,II+16)
        ENDDO
        DO II=1,69,17
          WRITE(LFN,3007) (SATWG2(J),J=II,II+16)
        ENDDO
        WRITE(LFN,3999) TYPCHR,SYSCHR
        WRITE(LFN,3008) (TITLE(I),I=1,4)
3003    FORMAT('#',2A1,I4,4(1X,I2),1X,F11.8,1X,I7,1X,A5,1X,A5,
     1         1X,A3,1X,A4)
3004    FORMAT('##',1X,I4,1X,F15.8,1X,F14.8,1X,I5,1X,F15.13)
3005    FORMAT('+ ',2X,I2,3X,17(A1,I2))
3006    FORMAT('+ ',7X,17(A1,I2))
3007    FORMAT('++',7X,17I3)
3999    FORMAT('%',A1,2(' cc'),' ',A3,' ccc',4(' cccc'),4(' ccccc'),/,
     1         '%c',2(' cc'),2(' ccc'),4(' cccc'),4(' ccccc'),/
     2         2('%f','  0.0000000','  0.000000000','  0.00000000000',
     3           '  0.000000000000000',/),
     4         '%i',4(4X,'0'),4(6X,'0'),9X,'0',/,
     5         '%i',4(4X,'0'),4(6X,'0'),9X,'0')
3008    FORMAT(3('/* ',A57,/),'/* ',A57)
C
      ELSEIF (IFRMAT.EQ.4. OR .IFRMAT.EQ.5) THEN
        POSVEL='P'
        IF(IFRMAT.EQ.5) POSVEL='V'
        IF (LEN_TRIM(AGENCY).EQ.0) THEN
          AGENCY=BLANK
        ELSEIF (LEN_TRIM(AGENCY).LT.4) THEN
          JUSTR=BLANK(LEN_TRIM(AGENCY):3)//AGENCY(1:LEN_TRIM(AGENCY))
          AGENCY=JUSTR
        ENDIF
        WRITE(LFN,4003) FMTCHR,POSVEL,IYEAR1,IMONT1,IDAY1 ,IHOUR1,
     1                  IMIN1 ,SEC1  ,NEPO  ,DATDES,COOSYS,ORBTYP,
     2                  AGENCY
        WRITE(LFN,4004) NWEEK,SECOND,DTTAB,IMJD,FMJD
        WRITE(LFN,4005) NSAT,(SATCHR(I),I=1,17)
        DO II=18,69,17
          WRITE(LFN,4006) (SATCHR(J),J=II,II+16)
        ENDDO
        DO II=1,69,17
          WRITE(LFN,4007) (SATWG2(J),J=II,II+16)
        ENDDO
        WRITE(LFN,4999) TYPCHR,SYSCHR,BASPOS,BASCLK
        WRITE(LFN,4008) (TITLE(I),I=1,4)
4003    FORMAT('#',2A1,I4,4(1X,I2),1X,F11.8,1X,I7,1X,A5,1X,A5,
     1         1X,A3,1X,A4)
4004    FORMAT('##',1X,I4,1X,F15.8,1X,F14.8,1X,I5,1X,F15.13)
4005    FORMAT('+ ',2X,I2,3X,17A3)
4006    FORMAT('+ ',7X,17A3)
4007    FORMAT('++',7X,17I3)
4999    FORMAT('%c',1X,A1,' ',' cc',' ',A3,' ccc',4(' cccc'),
     1                                            4(' ccccc'),/,
     1         '%c',2(' cc'),2(' ccc'),4(' cccc'),4(' ccccc'),/,
     2         '%f',1X,F10.7,1X,F12.9,'  0.00000000000',
     3           '  0.000000000000000',/,
     4         '%f','  0.0000000','  0.000000000','  0.00000000000',
     5           '  0.000000000000000',/,
     6         '%i',4(4X,'0'),4(6X,'0'),9X,'0',/,
     7         '%i',4(4X,'0'),4(6X,'0'),9X,'0')
4008    FORMAT(3('/* ',A57,/),'/* ',A57)
      ELSE
        GOTO 910
      ENDIF
      GOTO 999
C
C ERROR: NUMBER OF SV'S > MAXIMUM
C -------------------------------
901   WRITE(LFNERR,9001) NSAT,MAXSAT,TRIM(FILNAM)
9001  FORMAT(/,' *** SR WTPREH : NUMBER OF SAT. (NSAT) > MAXIMUM',/,
     1       17X,'NUMBER OF SATELLITES : ',I3 ,/,
     2       17X,'MAXIMUM NUMBER OF SAT. ALLOWED : ',I3,/,
     3       17X,'FILE NAME: ',A,/)
      CALL EXITRC(2)
C
C ERROR: FORMAT NR. 'IFRMAT' NOT CORRECT
C --------------------------------------
910   WRITE(LFNERR,9005) IFRMAT, TRIM(FILNAM), LFN
9005  FORMAT(/,' *** SR WTPREH : FORMAT IFRMAT=',I4,' IS NOT SUPPORTED'
     1       ,/,17X,'FILE NAME : ',A,/,
     2       17X,'FILE UNIT : ',I6,/)
      CALL EXITRC(2)
C
C END OF SUBROUTINE
C -----------------
999   RETURN
      END SUBROUTINE

      END MODULE
