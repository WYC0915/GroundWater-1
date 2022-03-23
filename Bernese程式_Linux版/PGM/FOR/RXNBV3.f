C*
      PROGRAM RXNBV3
CC
CC NAME       :  RXNBV3
CC
CC PURPOSE    :  READ RINEX NAVIGATION DATA FILES AND
CC               COPY THEM INTO BERNESE VERSION 3 FILES
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  W. GURTNER
CC
CC CREATED    :  89/04/18 11:30
CC
CC CHANGES    :  09-SEP-91 : ??: EPH,CLOCK=0.D0: CHANGE OUTPUT FORMAT
CC                               (TO GT AROUND LAHEY V4.1 COMPILER ERROR)
CC               23-DEC-92 : ??: USE OF SR "OPNFIL" TO OPEN FILES
CC               09-FEB-93 : ??: FORMAT D30.20 --> D28.18
CC                               (LAHEY V.4 PROBLEM)
CC               09-NOV-00 : ??: SWITCH TO NEW MENU SYTEM
CC               25-OCT-01 : RD: USE PRITIT, EXTENTED ERROR MSG
CC               19-FEB-03 : SS: EXTRACT GPS BROADCAST GD/DCB VALUES
CC               25-FEB-03 : SS: ADJUST INCORRECTLY DECODED GD VALUES
CC               22-APR-03 : SS: REJECT UNREGISTERED SATELLITES;
CC                               DISPLAY UNHEALTHY SATELLITES
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               26-FEB-04 : SS: GD TEST VALUE FROM 35 TO 50
CC               10-MAR-04 : HB: CHANGE ORDER OF MODULES
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: (COM)LFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               08-AUG-05 : HB: USE NEW SR TIMST2 (MODULE)
CC               27-FEB-07 : AG: CALL DEFCON WITH PARAMETER
CC               05-NOV-07 : RD: CONSIDER SATCRUX
CC               26-FEB-08 : RD: USE GTSATB FROM D_SATCRX
CC               09-MAY-09 : RD: NEW CALL OF WTCBFL
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               14-NOV-11 : SL: USE M_BERN WITH ONLY, PRITIT CALL CHANGED
CC               01-DEC-11 : SL: NEW TITLE STRING FOR PRITIT
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1989     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfn001, lfn002, lfnPrt, lfnErr
      USE m_cpu,    ONLY: cpu_start
      USE m_maxdim, ONLY: MAXSAT, MAXBAD
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE d_const,  ONLY: DATE, FILTITLE, TIME
      USE d_satcrx, ONLY: gtsatb
      USE s_dimtst
      USE s_satblk
      USE s_opnfil
      USE s_gtfile
      USE s_pritit
      USE s_readinpf
      USE s_opnerr
      USE s_rxv3br
      USE s_timst2
      USE s_wtcbfl
      USE s_r2rdnh
      USE s_defcon
      USE s_exitrc
      USE s_opnsys
      USE s_gtflna
      USE s_r2rdnr
      USE s_prflna
      USE f_gpsmjd
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IBLK  , ICBTYP, IFIL  , IFRQ  , INEW  , IOLD  ,
     1          IOSTAT, IRC   , IRCDCB, IRCHDR, IRCODE, IRCREC, IRXVRS,
     2          ISAT  , ISVN  , ITUTC , IWEEK , K     , LEAP  , LFNBRD,
     3          LFNRNX, MAXCOM, MAXFIL, NCOM  , NFILE , NFLCOL, NMTOT ,
     4          NUMDCB, NUMREC, NUMSAT, NWKUTC, IBAD  , NBAD  , NUMIFB
C
      REAL*8    A0UTC , A1UTC , TMESS , XDCB  , XEPO  , XFAC  ,
     1          XGD
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C MAXIMAL DIMENSIONS
C ------------------
      PARAMETER (MAXFIL=100)
C
C MAXFIL: MAXIMUM NUMBER OF INPUT AND OUTPUT FILES
C
C DECLARATIONS
C ------------
      CHARACTER*53 TITLE
      CHARACTER*32 FILNAM(2,MAXFIL),FILOUT,FILBAD
      CHARACTER*19 EPOSTR
      REAL*8       EPH(20),CLOCK(20)
      REAL*8       TIMBAD(2,MAXBAD)
      INTEGER*4    NMESS(99),NUNH(99),IUNR(99)
      INTEGER*4    SATBAD(MAXBAD),IOBBAD(MAXBAD),IACBAD(MAXBAD)
C
C  RXRDNH
      PARAMETER(MAXCOM=10)
      CHARACTER    RUNBY*20,PRGNAM*20,COMENT(MAXCOM)*60
      CHARACTER    CRDATE*9,CRTIME*5
      REAL*8       ALPHA(4),BETA(4)
C
C  RXRDNR
      REAL*8       EPHRNX(32)
C
C  WTCBFL
      CHARACTER*80 TITDCB
      CHARACTER*80 FILDCB
      CHARACTER*16 DCBID2(1),DCBID3(2,1)
      CHARACTER*1  DCBSYS(1)
      REAL*8       DCBVA1(2,MAXSAT),DCBVA2(2,1),DCBVA3(4,1)
      INTEGER*4    DCBID1(MAXSAT),DCBIN1(MAXSAT),DCBIN2(1),DCBIN3(1)
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C DEFINE LOGICAL FILE NUMBERS
C ---------------------------
      LFNRNX=LFN001
      LFNBRD=LFN002
C
C GET THE NAME OF THE INPUT FILE
C ------------------------------
      CALL init_inpkey(inpKey)
      CALL readinpf(' ',inpKey)
C
C DEFINE SYSTEM FILES
C -------------------
      CALL OPNSYS
C
C DEFINE CONSTANTS
C ----------------
      CALL DEFCON(1)
C
C WRITE THE PROGRAM OUTPUT HEADER
C -------------------------------
      CALL PRITIT('RXNBV3',
     1            'Transfer RINEX navigation files to Bernese format')
      CALL PRFLNA
C
C READ INPUT/OUTPUT FILENAMES
C ---------------------------
      NFLCOL=2
      CALL GTFILE('BRDFIL ',NFLCOL,MAXFIL,NFILE,FILNAM)
C
      CALL GTFLNA(0,'OUTFIL ',filout,irc)
      IF (irc .EQ. 0) THEN
        IF (filout .NE. ' ' .AND. nfile .EQ. 1) THEN
          filnam(2,1) =  filout
        ELSE IF (filout .NE. ' ' .AND. nfile .GT. 1) THEN
          WRITE(lfnerr,'(/,A,/,16X,A,/)')
     1          ' *** PG RXNBV3: The specification of output' //
     2                             ' file names is only allowed',
     3                          'if one file is processed!'
          CALL exitrc(2)
        END IF
      END IF
C
      WRITE(LFNPRT,11)
11    FORMAT(   ' FILE  RINEX FILENAME                    ',
     1          'BERNESE BROADCAST FILENAME        #MESS',
     2        /,' ',4('-'),2X,32('-'),2X,32('-'),2X,5('-'),/)
C
      CALL GTFLNA(0,'DCBOUT ',FILDCB,IRCDCB)
      NUMSAT=0
      NUMDCB=0
      IOLD=0
C
C GET BAD SATELLITES
C ------------------
      CALL GTFLNA(0,'SATCRUX',FILBAD,IRC)
      IF (IRC.EQ.0.AND.LEN_TRIM(FILBAD).NE.0) THEN
        CALL GTSATB(MAXBAD,FILBAD,NBAD,SATBAD,IOBBAD,IACBAD,TIMBAD)
      ELSE
        NBAD=0
      ENDIF
C
      DO 100 IFIL=1,NFILE
C
        IRCODE=0
        CALL OPNFIL(LFNRNX,FILNAM(1,IFIL),'OLD','FORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNRNX,IOSTAT,FILNAM(1,IFIL),'RXNBV3')
        CALL R2RDNH(LFNRNX,LFNERR,MAXCOM,PRGNAM,RUNBY,
     1                  CRDATE,CRTIME,NCOM,COMENT,
     2                  ALPHA,BETA,A0UTC,A1UTC,ITUTC,NWKUTC,LEAP,
     3                  IRXVRS,IRC)
        IRCODE=IRC
        IF(IRC.NE.0) GOTO 999
C
        INEW=0
        IF(IFIL.EQ.1) THEN
          INEW=1
        ELSEIF(FILNAM(2,IFIL).NE.FILNAM(2,IFIL-1)) THEN
          INEW=1
          CLOSE(UNIT=LFNBRD)
        END IF
C
C  OPEN NEW BROADCAST FILE, CREATE HEADER
        IF(INEW.NE.0) THEN
          IF(FILNAM(2,IFIL).NE.' ') THEN
            CALL OPNFIL(LFNBRD,FILNAM(2,IFIL),'UNKNOWN','FORMATTED',
     1                  ' ',' ',IOSTAT)
            CALL OPNERR(LFNERR,LFNBRD,IOSTAT,FILNAM(2,IFIL),'RXNBV3')
            IF(NCOM.EQ.0) THEN
              TITLE=' '
            ELSE
              TITLE=COMENT(1)
            END IF
            WRITE(LFNBRD,101) TITLE,DATE,TIME
101         FORMAT(A53,12X,A9,1X,A5)
          ENDIF
          NMTOT=0
          DO 110 I=1,99
            NMESS(I)=0
            NUNH(I)=0
            IUNR(I)=0
110       CONTINUE
        END IF
C
C  READ ONE RINEX ARRAY AT A TIME
200       CALL R2RDNR(LFNRNX,LFNERR,IRXVRS,ISVN,EPHRNX,IRCREC)
C  END OF FILE
          IF(IRCREC.EQ.9) GOTO 210
C  NEW HEADER FOUND
          IF(IRCREC.EQ.5) THEN
            CALL R2RDNH(LFNRNX,LFNERR,MAXCOM,PRGNAM,RUNBY,
     1                  CRDATE,CRTIME,NCOM,COMENT,
     2                  ALPHA,BETA,A0UTC,A1UTC,ITUTC,NWKUTC,LEAP,
     3                  IRXVRS,IRCHDR)
            IRCODE=IRC
            IF(IRCHDR.NE.0) GOTO 999
            GOTO 200
          END IF
          IRCODE=IRCREC
          IF(IRCREC.NE.0) GOTO 999
C
C  TRANSFORM RINEX ARRAY INTO BERNESE FORMAT
          CALL RXV3BR(EPHRNX,EPH,CLOCK)
C
          IF (ISVN.LT.1 .OR. ISVN.GT.99) THEN
            WRITE(LFNERR,921) ISVN
921         FORMAT(/,' ### PG RXNBV3: ILLEGAL SATELLITE NUMBER',/,
     1        16X,'SATELLITE NUMBER: ',I3,/)
            GOTO 200
          ENDIF
C
C  REJECT UNREGISTERED SATELLITES
CC          IWEEK=IDNINT(EPHRNX(23))
CC          XEPO=GPSMJD(EPHRNX(13),IWEEK)
          IWEEK=IDNINT(EPH(1))
          XEPO=GPSMJD(EPH(2),IWEEK)
          CALL SATBLK(ISVN,XEPO,IFRQ,IBLK)
          IF (IBLK.EQ.0) THEN
            IF (IUNR(ISVN).EQ.0) THEN
              CALL TIMST2(1,1,XEPO,EPOSTR)
              WRITE(LFNERR,922) ISVN,EPOSTR
922           FORMAT(/,' ### PG RXNBV3: SATELLITE NOT REGISTERED IN ',
     1          'SATELLITE INFORMATION FILE',/,
     2          16X,'SATELLITE NUMBER : ',I3,/,
     3          16X,'EPOCH            : ',A19,/)
              IUNR(ISVN)=1
            ENDIF
            GOTO 200
          ENDIF
C
C  CHECK WHETHER SATELLITE SET UNHEALTHY
CC          IF (EPHRNX(26).NE.0.D0) NUNH(ISVN)=NUNH(ISVN)+1
          IF (CLOCK(4).NE.0.D0) NUNH(ISVN)=NUNH(ISVN)+1
C
C CEHCK WHETHER THE SATELLITE IS FLAGGED IN SATCRUX
C -------------------------------------------------
          DO IBAD=1,NBAD
            IF (SATBAD(IBAD).EQ.ISVN.AND.
     1          XEPO.GE.TIMBAD(1,IBAD).AND.XEPO.LE.TIMBAD(2,IBAD)) THEN
              GOTO 200
            ENDIF
          ENDDO
C
C  WRITE MESSAGE ONTO OUTPUT FILE
          NMESS(ISVN)=NMESS(ISVN)+1
          IF(FILNAM(2,IFIL).NE.' ') THEN
            WRITE(LFNBRD,201) ISVN,NMESS(ISVN)
201         FORMAT('SVN-NUMBER=',I3,'  MESSAGE-NR=',I3)
            DO 220 K=1,20
              IF(EPH(K).EQ.0.D0) THEN
                WRITE(LFNBRD,203) 0.
203             FORMAT(F7.1)
              ELSE
                WRITE(LFNBRD,202) EPH(K)
202             FORMAT(D28.18)
              ENDIF
220         CONTINUE
            DO 230 K=1,20
              IF(CLOCK(K).EQ.0.D0) THEN
                WRITE(LFNBRD,203) 0.
              ELSE
                WRITE(LFNBRD,202) CLOCK(K)
              ENDIF
230         CONTINUE
          ENDIF
C
          NMTOT=NMTOT+1
C
C EXTRACT GPS BROADCAST GROUP DELAY (GD) VALUES
C ---------------------------------------------
          IF (IRCDCB.EQ.0) THEN
            XGD=EPHRNX(27)/2.D0**(-31)
            IF (DABS(XGD-DNINT(XGD)).GT.1.D-3) THEN
              WRITE(LFNERR,901) ISVN,XGD,NMESS(ISVN),FILNAM(1,IFIL)
901           FORMAT(/,' ### PG RXNBV3: ',
     1              'UNQUANTIZED GD VALUE IGNORED',/,
     2          16X,'PRN        : ',I3,/
     3          16X,'GD VALUE   : ',F8.3,/,
     4          16X,'MESSAGE    : ',I3,/,
     5          16X,'RINEX FILE : ',A32,/)
              XGD=DNINT(1.D3*XGD)/1.D3
              GOTO 200
            ELSE
              XGD=DNINT(XGD)
C
C ADJUST INCORRECTLY DECODED GD VALUES
              IF (XGD.GT.127.D0) THEN
                WRITE(LFNERR,902) ISVN,NMESS(ISVN),FILNAM(1,IFIL)
902             FORMAT(/,' ### PG RXNBV3: ',
     1                'INCORRECTLY DECODED GD VALUE ADJUSTED',/,
     2            16X,'PRN        : ',I3,/
     3            16X,'MESSAGE    : ',I3,/,
     4            16X,'RINEX FILE : ',A32,/)
                XGD=XGD-256.D0
              ENDIF
            ENDIF
C
C REJECT UNREALISTIC GD VALUES
            IF (DABS(XGD).GT.50.D0) THEN
              WRITE(LFNERR,903) ISVN,XGD,NMESS(ISVN),FILNAM(1,IFIL)
903           FORMAT(/,' ### PG RXNBV3: ',
     1              'UNREALISTIC GD VALUE IGNORED',/,
     2          16X,'PRN        : ',I3,/
     3          16X,'GD VALUE   : ',F8.3,/,
     4          16X,'MESSAGE    : ',I3,/,
     5          16X,'RINEX FILE : ',A32,/)
              GOTO 200
            ENDIF
C
            XFAC=-(77.D0**2-60.D0**2)/60.D0**2
            XDCB=1.D9*XFAC*XGD*2.D0**(-31)
C
C RECONVERT INCORRECT GD VALUES (BROADCAST BEFORE APRIL 1999)
            TMESS=EPHRNX(23)+EPHRNX(13)/604800.D0
            IF (TMESS.LT.1008.D0) THEN
              XDCB=1/XFAC*XDCB
              IOLD=1
C
              IF (TMESS.GE.1006.D0) GOTO 200
            ENDIF
C
            DO ISAT=1,NUMSAT
              IF (ISVN.EQ.DCBID1(ISAT)) THEN
                IF (DCBVA1(1,ISAT).EQ.0.D0 .AND. XDCB.NE.0.D0) THEN
                  DCBVA1(1,ISAT)=XDCB
                  NUMDCB=NUMDCB+1
                ENDIF
C
                IF (XDCB.NE.DCBVA1(1,ISAT)) THEN
                  WRITE(LFNERR,904) ISVN,XDCB,NMESS(ISVN),
     1              FILNAM(1,IFIL)
904               FORMAT(/,' ### PG RXNBV3: ',
     1                  'DIFFERING GD/DCB VALUE FOUND',/,
     2              16X,'PRN        : ',I3,/
     3              16X,'DCB VALUE  : ',F8.3,/,
     4              16X,'MESSAGE    : ',I3,/,
     5              16X,'RINEX FILE : ',A32,/)
                ENDIF
                GOTO 200
              ENDIF
            ENDDO
C
            NUMSAT=NUMSAT+1
            CALL DIMTST(1,2,1,'RXNBV3','MAXSAT','SATELLITES',
     1        ' ',NUMSAT,MAXSAT,IRC)
C
            DCBID1(NUMSAT)=ISVN
            DCBVA1(1,NUMSAT)=XDCB
            DCBVA1(2,NUMSAT)=0.D0
C
            IF (XDCB.NE.0.D0) NUMDCB=NUMDCB+1
          ENDIF
C
          GOTO 200
C
210     CONTINUE
C
C DISPLAY UNHEALTHY SATELLITES
C ----------------------------
        DO ISVN=1,99
          IF (NUNH(ISVN).GT.0) THEN
            IF (NUNH(ISVN).EQ.NMESS(ISVN)) THEN
              WRITE(LFNERR,931) ISVN
931           FORMAT(/,' ### PG RXNBV3: SATELLITE PERMANENTLY SET ',
     1          'UNHEALTHY',/,
     2          16X,'SATELLITE NUMBER: ',I3,/)
            ELSE
              WRITE(LFNERR,932) ISVN
932           FORMAT(/,' ### PG RXNBV3: SATELLITE TEMPORARILY SET ',
     1          'UNHEALTHY',/,
     2          16X,'SATELLITE NUMBER: ',I3,/)
            ENDIF
          ENDIF
        ENDDO
C
        CLOSE(UNIT=LFNRNX)
        WRITE(LFNPRT,211) IFIL,FILNAM(1,IFIL),FILNAM(2,IFIL),NMTOT
211     FORMAT(I5,2X,A32,2X,A32,I6)
C
100   CONTINUE
C
      WRITE(LFNPRT,'(/)')
C
C WRITE BERNESE DCB FILE CONTAINING CONVERTED GROUP DELAY (GD) VALUES
C -------------------------------------------------------------------
      IF (IRCDCB.EQ.0) THEN
         IF (IOLD.EQ.1) THEN
           WRITE(LFNERR,911)
911        FORMAT(/,' ### PG RXNBV3: OLD GPS BROADCAST GROUP DELAY ',
     1       'VALUES RECONVERTED OR REJECTED',/)
         ENDIF
C
         IF (NUMDCB.GT.0) THEN
           TITDCB=FILTITLE
           NUMREC=0
           NUMIFB=0
           ICBTYP=1
           CALL WTCBFL(FILDCB,TITDCB,NUMSAT,NUMREC,NUMIFB,ICBTYP,
     1                 DCBID1,DCBVA1,DCBID2,DCBVA2,DCBSYS,DCBID3,
     2                 DCBVA3,DCBIN1,DCBIN2,DCBIN3)
C
           WRITE(LFNPRT,311) NUMSAT
311        FORMAT(/,' TOTAL NUMBER OF GPS BROADCAST GROUP DELAY ',
     1       'VALUES EXTRACTED: ',I3,/)
         ELSE
           WRITE(LFNERR,912)
912        FORMAT(/,' ### PG RXNBV3: NO GPS BROADCAST GROUP DELAY ',
     1       'VALUES FOUND',/)
         ENDIF
      ENDIF
C
999   CALL EXITRC(IRCODE)
C
      END
