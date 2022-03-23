      MODULE s_EXTCRD
      CONTAINS

C*
      SUBROUTINE EXTCRD(NFIL,FILNAM,FILTYP)
CC
CC NAME       :  EXTCRD
CC
CC PURPOSE    :  EXTRACT COORDINATE DIFFERENCES FROM GPSEST
CC               OUTPUT FILES
CC
CC PARAMETERS :
CC         IN :  NFIL   : TOTAL NUMBER OF OUTPUT FILES        I*4
CC               FILNAM(I),I=1,..,NFIL: LIST OF FILE NAMES    CH*32(*)
CC               FILTYP(I),I=1,..,NFIL: OUTPUT FILE INDEX     I*4(*)
CC                        = 0: BAD OUTPUT FILE
CC                        = 1: GPSEST
CC                        = 2: ADDNEQ
CC                        = 3: ADDNEQ2
CC                        =11: GPSEST  (V5.0)
CC                        =13: ADDNEQ2 (V5.0)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S.SCHAER
CC
CC VERSION    :  4.0
CC
CC CREATED    :  17-JAN-96
CC
CC CHANGES    :  05-JUN-96 : LM: DATA STATEMENT OUT OF ORDER
CC               30-OCT-97 : SS: SIGMA OF ZERO-DIFFERENCE OBS
CC               10-NOV-97 : SS: HANDLE FILES WITHOUT AMBIGUITIES
CC               28-NOV-97 : SS: WRITE SUMMARY LINE
CC               11-DEC-97 : SS: "MAXFIL" FROM 200 TO 400
CC               15-AUG-99 : JJ: COMMENT OUT UNUSED VAR TYPOUT
CC               14-MAR-00 : SS: CONSIDER ADDNEQ2 OUTPUT FILES
CC               20-DEC-00 : SS: VARIABLE "TYPOUT" REMOVED
CC               11-DEC-02 : CU: GPSEST V5.0 OUTPUT
CC               04-APR-04 : HU: EXTRACT V5.0 TITLE CORRECTLY
CC               28-JUN-04 : RD: USE MAXSTA FROM P_GPSEST
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               31-JAN-08 : SL: EXTRACT BASELINE NAME
CC               15-FEB-08 : SL: SINGLE BASELINE AND OUTPUT FORMAT
CC               20-FEB-08 : SL: COMPUTE SLOPE DISTANCE W/O SLOPE TAB
CC               26-MAR-10 : RD: REPLACE MAXFIL BY NFIL
CC               19-JUL-10 : SL: tab characters removed
CC               12-NOV-11 : RD: RECOVER ZERO-DIFF. CODE+PHASE PROCESSING
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1996     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE P_GPSEST, ONLY: maxsta
C
      USE s_opnfil
      USE s_opnerr
      USE s_exitrc
      USE s_gtflna
      USE s_fparse
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , ICOR  , IFIL  , IOSTAT, IPART ,
     1          IPGM  , IRCCRD, IREC  , ISTA  , J     , JSTA  ,
     2          NAMB  , NBAS  , NFIL  , NOBS  , NUMFIX, IRC
C
      REAL*8    XDHT  , XLEN  , XRMS
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*133 LINE
      CHARACTER*32  FILNAM(*),FILCRD,FILOUT,HLPFIL
      CHARACTER*32  BASFIL,NODE,DEVICE,DIRNAM,EXT,VER
      CHARACTER*16  STANAM(MAXSTA,NFIL),TESTC
      CHARACTER*8   TITLE(NFIL)
C
      REAL*8        XMFIX(3,MAXSTA,NFIL),HTFIX(MAXSTA,NFIL)
      REAL*8        DXSTA(3,MAXSTA,NFIL),DLENG(MAXSTA,NFIL)
      REAL*8        XOSTA(3,MAXSTA,NFIL),XNSTA(3,MAXSTA,NFIL)
      REAL*8        DESTA(4,MAXSTA,NFIL),DSTOT(3,4),DLTOT(3)
      REAL*8        HT(MAXSTA,NFIL)
      REAL*8        DHT(MAXSTA,NFIL)
      REAL*8        RDXSTA(3,MAXSTA,NFIL),RDLENG(MAXSTA,NFIL)
      REAL*8        RDESTA(3,MAXSTA,NFIL)
      REAL*8        RMSFIL(NFIL),DIST(3),BASLEN(MAXSTA,NFIL)
C
      INTEGER*4     FILTYP(*)
      INTEGER*4     NRSTA(NFIL),STANUM(MAXSTA,NFIL),NUMOBS(NFIL)
      INTEGER*4     NUMAMB(NFIL)
C
C
C RETURN, IF CRD SUMMARY FILE NOT REQUESTED
C -----------------------------------------
      CALL GTFLNA(0,'CRDOUT ',FILCRD,IRCCRD)
      IF (IRCCRD.EQ.1) GOTO 999
C
C OPEN CRD SUMMARY FILE
C ---------------------
      CALL OPNFIL(LFN001,FILCRD,'UNKNOWN','FORMATTED',
     1  ' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFN001,IOSTAT,FILCRD,'EXTCRD')
C
C INITIALIZE SOME VARIABLES
C -------------------------
      NBAS=0
      NOBS=0
      NAMB=0
      XRMS=0.D0
      XLEN=0.D0
      XDHT=0.D0
      DO J=1,3
        DO I=1,4
          DSTOT(J,I)=0.D0
        ENDDO
        DLTOT(J)=0.D0
      ENDDO
C
C LOOP OVER ALL FILES
C -------------------
      DO 1000 IFIL=1,NFIL
C
C SKIP BAD OUTPUT FILE
C --------------------
        IPGM=FILTYP(IFIL)
        IF (IPGM.NE.1.AND.IPGM.NE.11) GOTO 1000
C
C OPEN GPSEST OUTPUT FILE (INPUT)
C -------------------------------
        FILOUT=FILNAM(IFIL)
        CALL OPNFIL(LFNLOC,FILOUT,'OLD','FORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILOUT,'EXTCRD')
C
C EXTRACT BASELINE NAME
C ---------------------
        DO IREC=1,100000
          READ(LFNLOC,'(A)',END=900) LINE
          IF (LINE(1:30).EQ.' FILE  OBSERVATION FILE HEADER') GOTO 405
        ENDDO
405     READ(LFNLOC,'(//,7X,A32)') BASFIL
        CALL FPARSE(0,BASFIL,NODE,DEVICE,DIRNAM,TITLE(IFIL),EXT,VER,IRC)
        IF (IRC.NE.0) GOTO 900
C
C CHECK WHETHER FURTHER BASELINES ARE LISTED
        READ(LFNLOC,'(A)') LINE
C
C CHECK WHETHER MULTI-BASELINE FILE IS PROCESSED
        DO WHILE (LINE .NE. ' ')
          READ(LINE,'(7X,A32)') BASFIL
          CALL FPARSE(0,BASFIL,NODE,DEVICE,DIRNAM,HLPFIL,EXT,VER,IRC)
          IF (HLPFIL.NE.TITLE(IFIL)) THEN
            WRITE(LFNERR,940) FILOUT
940         FORMAT(/,' ### SR EXTCRD: MORE THAN ONE BASELINE FOUND',
     1        /,16X,'FILE NAME: ',A,/)
            CLOSE(LFNLOC)
            GOTO 1000
          ENDIF
          READ(LFNLOC,'(A)') LINE
        ENDDO
C
C FIND THE NUMBER OF FIXED STATIONS IN FILE
C -----------------------------------------
        NUMFIX=0
        DO IREC=1,1000000000
          READ(LFNLOC,1003,END=55) LINE
          IF(LINE(1:33).EQ.' NUM  STATION NAME     OBS E/F/C '.OR.
     1       LINE(1:33).EQ.' num  Station name     obs e/f/h ')THEN
            IF(IPGM.EQ.1) THEN
              READ(LFNLOC,'(/)',END=900)
            ELSE
              READ(LFNLOC,*,END=900)
            ENDIF
411         READ(LFNLOC,1003) LINE
            IF(LINE.EQ.' ') GOTO 420
C
C READ COORDINATES OF FIXED STATIONS
            IF(LINE(28:30).EQ.'FIX') THEN
              NUMFIX=NUMFIX+1
              READ(LINE,'(33X,F15.4,1X,F15.4,1X,F15.4,41X,F11.4)')
     1        XMFIX(1,NUMFIX,IFIL),XMFIX(2,NUMFIX,IFIL),
     2        XMFIX(3,NUMFIX,IFIL),HTFIX(NUMFIX,IFIL)
            ENDIF
            GOTO 411
          ENDIF
        ENDDO
C
C END OF FILE REACHED WITHOUT FINDING SIGMA OF SINGLE DIFF.
C ---------------------------------------------------------
55      WRITE(LFNERR,1015) FILOUT
1015    FORMAT(/,' ### SR EXTCRD: END OF FILE REACHED',/,
     1                       16X,'NO OBS E/F/C RECORD FOUND ',/,
     2                       16X,'FILE: ',A32,/)
        GOTO 990
420     CONTINUE
C
C LOOP OVER TWO POSSIBLE PARTS IN FILE
C ------------------------------------
        IPART=0
430     CONTINUE
        DO IREC=1,1000000000
          READ(LFNLOC,1003,END=65) LINE
          IF(LINE(5:19).EQ.
     1       ' RESULTS (PART ') THEN
             IPART=IPART+1
             GOTO 440
          ENDIF
        ENDDO
65      CONTINUE
        IF (IPART.EQ.0) THEN
          WRITE(LFNERR,1027) FILOUT
1027      FORMAT(/,' ### SR EXTCRD: END OF FILE REACHED',/,
     1                       16X,'NO PARTS FOUND AT ALL',/,
     2                       16X,'FILE: ',A32,/)
        ENDIF
        GOTO 990
440     CONTINUE
C
C FIND NUMBER OF AMBIGUITIES
C --------------------------
        READ(LFNLOC,'(//////)')
C
        DO IREC=1,10000000
          READ(LFNLOC,1003,END=66) LINE
          IF(LINE(1:47).EQ.
     1       ' AMBIGUITIES                                   ') THEN
             READ(LINE,'(54X,I5)') NUMAMB(IFIL)
             GOTO 443
          ELSEIF (LINE(1:27).EQ.' TOTAL NUMBER OF PARAMETERS') THEN
             NUMAMB(IFIL)=0
             GOTO 443
          ENDIF
        ENDDO
66      WRITE(LFNERR,1028) FILOUT
1028    FORMAT(/,' ### SR EXTCRD: END OF FILE REACHED',/,
     1                       16X,'NO AMBIGUITIES FOUND ',/,
     2                       16X,'FILE: ',A32,/)
        GOTO 990
443     CONTINUE
C
C FIND SIGMA OF SINGLE DIFFERENCE OBSERVATION
C -------------------------------------------
        DO 50 IREC=1,1000000
          READ(LFNLOC,1003,END=60) LINE
1003      FORMAT(A133)
          IF(LINE(1:29).EQ.
     1       ' TOTAL NUMBER OF OBSERVATIONS') THEN
            READ(LINE(51:59),'(i9)') NUMOBS(IFIL)
            READ(LFNLOC,'(/////)')
            READ(LFNLOC,1004) RMSFIL(IFIL)
1004        FORMAT(38X,F9.4)
C
C CONVERT TO MILLIMETER
            RMSFIL(IFIL)=RMSFIL(IFIL)*1.D3
            GOTO 70
          ENDIF
50      CONTINUE
C
C END OF FILE REACHED WITHOUT FINDING SIGMA OF SINGLE DIFF.
C ---------------------------------------------------------
60      WRITE(LFNERR,1005) FILOUT
1005    FORMAT(/,' ### SR EXTCRD: END OF FILE REACHED',/,
     1                       16X,'NO NUMBER OF OBS. FOUND',/,
     2                       16X,'FILE: ',A32,/)
        GOTO 990
C
C FIND STATION COORDINATE TABLE IN OUTPUT FILE
C --------------------------------------------
70      DO 80 IREC=1,1000000
          READ(LFNLOC,1003,END=90) LINE
          IF(LINE(1:40).EQ.
     1       ' NUM  STATION NAME     PARAMETER    A PR') GOTO 100
80      CONTINUE
C
C END OF FILE REACHED WITHOUT FINDING STATION COORDINATES
C -------------------------------------------------------
90      WRITE(LFNERR,1006) FILOUT
1006    FORMAT(/,' ### SR EXTCRD: END OF FILE REACHED',/,
     1                       16X,'NO STATION COORDINATES FOUND',/,
     2                       16X,'FILE: ',A32,/)
        GOTO 990
C
C JUMP TWO MORE LINES
C -------------------
100     READ(LFNLOC,'(/)')
C
C LOOP OVER ALL STATIONS
C ----------------------
        ISTA=0
        DO 200 JSTA=1,10000
          ISTA=ISTA+1
          READ(LFNLOC,1003,END=220) LINE
C
C LAST STATION READ ?
          IF(LEN_TRIM(LINE).EQ.0) GOTO 220
C
C CHECK MAXIMUM NUMBER OF STATIONS
          IF(ISTA.GT.MAXSTA) THEN
            WRITE(LFNERR,1021) ISTA,MAXSTA,FILOUT
1021        FORMAT(/,' *** SR EXTCRD: TOO MANY ESTIMATED STATIONS',/,
     1                           16X,'NUMBER OF STATIONS    >=',I4,/,
     2                           16X,'MAX. NUMBER OF STATIONS:',I4,/,
     3                           16X,'FILE                   : ',A32,/)
            CALL EXITRC(2)
          ENDIF
C
C READ STATION NAME AND COORDINATES
          READ(LINE,1007,END=210) STANUM(ISTA,IFIL),STANAM(ISTA,IFIL),
     1                    XOSTA(1,ISTA,IFIL),XNSTA(1,ISTA,IFIL),
     2                    DXSTA(1,ISTA,IFIL),RDXSTA(1,ISTA,IFIL)
1007      FORMAT(I4,2X,A16,8X,2F19.4,2F12.4)
          READ(LFNLOC,1010,END=210) TESTC,
     1                    XOSTA(2,ISTA,IFIL),XNSTA(2,ISTA,IFIL),
     2                    DXSTA(2,ISTA,IFIL),RDXSTA(2,ISTA,IFIL)
1010      FORMAT(A16,14X,2F19.4,2F12.4)
          IF (TESTC.NE.' ') THEN
            READ(LFNLOC,'(/////)')
            ISTA=ISTA-1
            GOTO 200
          ENDIF
          READ(LFNLOC,1008,END=210)
     1      XOSTA(3,ISTA,IFIL),XNSTA(3,ISTA,IFIL),
     2      DXSTA(3,ISTA,IFIL),RDXSTA(3,ISTA,IFIL)
1008      FORMAT(30X,2F19.4,2F12.4,/)
          READ(LFNLOC,1009,END=210)
     1      HT(ISTA,IFIL),
     2      (DESTA(I,ISTA,IFIL),RDESTA(I,ISTA,IFIL),I=1,3)
1009      FORMAT(57X,F13.4,F10.4,F12.4,/,2(70X,F10.4,F12.4,/))
C
C CONVERSION TO MILLIMETER
          DO 150 ICOR=1,3
            XOSTA(ICOR,ISTA,IFIL)=XOSTA(ICOR,ISTA,IFIL)*1.D3
            XNSTA(ICOR,ISTA,IFIL)=XNSTA(ICOR,ISTA,IFIL)*1.D3
            DXSTA(ICOR,ISTA,IFIL)=DXSTA(ICOR,ISTA,IFIL)*1.D3
            DESTA(ICOR,ISTA,IFIL)=DESTA(ICOR,ISTA,IFIL)*1.D3
            RDESTA(ICOR,ISTA,IFIL)=RDESTA(ICOR,ISTA,IFIL)*1.D3
            RDXSTA(ICOR,ISTA,IFIL)=RDXSTA(ICOR,ISTA,IFIL)*1.D3
150       CONTINUE
C
C COMPUTE "DS"
          DESTA(4,ISTA,IFIL)=DSQRT(DESTA(1,ISTA,IFIL)**2
     1                            +DESTA(2,ISTA,IFIL)**2
     2                            +DESTA(3,ISTA,IFIL)**2)
C
200     CONTINUE
C
C END OF FILE IN THE MIDDLE OF A STATION BLOCK
C --------------------------------------------
210     WRITE(LFNERR,1011) FILOUT,STANUM(ISTA,IFIL),
     1                     STANAM(ISTA,IFIL)
1011    FORMAT(/,' *** SR EXTCRD: END OF FILE REACHED IN THE MIDDLE',/,
     1                       16X,'OF A STATION COORDINATE BLOCK',/,
     2                       16X,'FILE          : ',A32,/,
     3                       16X,'STATION NUMBER:',I4,/,
     4                       16X,'STATION NAME  : ',A16,/)
        GOTO 990
C
C COMPUTE SLOPE DISTANCES AND NUMBER OF STATIONS ESTIMATED
C --------------------------------------------------------
220     CONTINUE
        IF (NUMFIX.EQ.1) THEN
          DHT(1,IFIL)=HT(1,IFIL)-HTFIX(1,IFIL)
          DIST(1)=DSQRT((XOSTA(1,1,IFIL)-XMFIX(1,1,IFIL)*1.D3)**2
     1                 +(XOSTA(2,1,IFIL)-XMFIX(2,1,IFIL)*1.D3)**2
     2                 +(XOSTA(3,1,IFIL)-XMFIX(3,1,IFIL)*1.D3)**2)
          DIST(2)=DSQRT((XNSTA(1,1,IFIL)-XMFIX(1,1,IFIL)*1.D3)**2
     1                 +(XNSTA(2,1,IFIL)-XMFIX(2,1,IFIL)*1.D3)**2
     2                 +(XNSTA(3,1,IFIL)-XMFIX(3,1,IFIL)*1.D3)**2)
          DIST(3)=DSQRT(RDXSTA(1,1,IFIL)**2
     1                 +RDXSTA(2,1,IFIL)**2
     2                 +RDXSTA(3,1,IFIL)**2)
        ELSEIF (NUMFIX.EQ.0) THEN
          DHT(1,IFIL)=HT(1,IFIL)-HT(2,IFIL)
          IF (DABS(XOSTA(1,2,IFIL))+DABS(XOSTA(2,2,IFIL))+
     1        DABS(XOSTA(3,2,IFIL)).NE.0d0) THEN
            DIST(1)=DSQRT((XOSTA(1,1,IFIL)-XOSTA(1,2,IFIL))**2
     1                   +(XOSTA(2,1,IFIL)-XOSTA(2,2,IFIL))**2
     2                   +(XOSTA(3,1,IFIL)-XOSTA(3,2,IFIL))**2)
            DIST(2)=DSQRT((XNSTA(1,1,IFIL)-XNSTA(1,2,IFIL))**2
     1                   +(XNSTA(2,1,IFIL)-XNSTA(2,2,IFIL))**2
     2                   +(XNSTA(3,1,IFIL)-XNSTA(3,2,IFIL))**2)
          ELSE
            DIST(:)=0d0
          ENDIF
        ENDIF
        BASLEN(1,IFIL)=DIST(2)/1.D6
        DLENG(1,IFIL)=(DIST(2)-DIST(1))
        RDLENG(1,IFIL)=DIST(3)
        NRSTA(IFIL)=ISTA-1
C
C CHECK IF THERE IS A SECOND PART
C -------------------------------
        IF (IPART.LT.2) GOTO 430
        GOTO 990
C
C CLOSE FILE
C ----------
990     CLOSE(LFNLOC)
C
1000  CONTINUE
C
C WRITE TITLE FOR COORDINATE DIFFERENCES
C --------------------------------------
      WRITE(LFN001,1016)
1016  FORMAT(
     1 ' BASELINE  #OBS.  #AMB  RMS(MM)   L(KM)   DHT(M)  DH(MM) +- ',
     2 '   DN(MM) +-    DE(MM) +-    DL(MM) +-    DS(MM)',
     3 /,1X,107('-'))
C
C WRITE COORDINATE DIFFERENCES FOR ALL FILES AND STATIONS
C -------------------------------------------------------
      DO 1100 IFIL=1,NFIL
        IPGM=FILTYP(IFIL)
        IF (IPGM.NE.1.AND.IPGM.NE.11) GOTO 1100
        DO 1050 ISTA=1,NRSTA(IFIL)
          IF(ISTA.EQ.1) THEN
            WRITE(LFN001,1017) TITLE(IFIL),NUMOBS(IFIL),
     1        NUMAMB(IFIL),RMSFIL(IFIL),BASLEN(ISTA,IFIL),
     2        DHT(ISTA,IFIL),
     3        (DESTA(I,ISTA,IFIL),RDESTA(I,ISTA,IFIL),I=1,3),
     4        DLENG(ISTA,IFIL),RDLENG(ISTA,IFIL),
     5        DESTA(4,ISTA,IFIL)
1017        FORMAT(1X,A8,I6,1X,I5,1X,F7.1,F9.1,F9.1,4(F8.1,F5.1),
     1        F8.1)
C
            NBAS=NBAS+1
            NOBS=NOBS+NUMOBS(IFIL)
            NAMB=NAMB+NUMAMB(IFIL)
            XRMS=XRMS+RMSFIL(IFIL)**2
            XLEN=XLEN+BASLEN(ISTA,IFIL)
            XDHT=XDHT+DHT(ISTA,IFIL)
            DO I=1,4
              DSTOT(1,I)=DSTOT(1,I)+DESTA(I,ISTA,IFIL)
              DSTOT(2,I)=DSTOT(2,I)+DESTA(I,ISTA,IFIL)**2
            ENDDO
            DLTOT(1)=DLTOT(1)+DLENG(ISTA,IFIL)
            DLTOT(2)=DLTOT(2)+DLENG(ISTA,IFIL)**2
          ENDIF
1050    CONTINUE
1100  CONTINUE
C
C WRITE SUMMARY LINE
C ------------------
      IF (NBAS.GT.0) THEN
        NOBS=IDNINT(1.D0*NOBS/NBAS)
        NAMB=IDNINT(1.D0*NAMB/NBAS)
        XRMS=DSQRT(XRMS/NBAS)
        XLEN=XLEN/NBAS
        XDHT=XDHT/NBAS
        IF (NBAS.GT.1) THEN
          DO I=1,4
            DSTOT(3,I)=DSQRT((DSTOT(2,I)-DSTOT(1,I)**2/NBAS)/(NBAS-1))
          ENDDO
          DLTOT(3)=DSQRT((DLTOT(2)-DLTOT(1)**2/NBAS)/(NBAS-1))
        ENDIF
        DO I=1,4
          DSTOT(1,I)=DSTOT(1,I)/NBAS
          DSTOT(2,I)=DSQRT(DSTOT(2,I)/NBAS)
        ENDDO
        DLTOT(1)=DLTOT(1)/NBAS
        DLTOT(2)=DSQRT(DLTOT(2)/NBAS)
      ENDIF
C
      WRITE(LFN001,1018) NBAS,NOBS,NAMB,XRMS,XLEN,XDHT,
     1  (DSTOT(2,I),I=1,3),DLTOT(2),DSTOT(2,4),
     2  (DSTOT(1,I),DSTOT(3,I),I=1,3),DLTOT(1),DLTOT(3)
1018  FORMAT(1X,107('-'),/,
     1  1X,'TOT:',I4,I6,1X,I5,1X,F7.1,F9.1,F9.1,4F13.1,F8.1,/,
     1  47X,4(F8.1,F5.1))
C
C CLOSE CRD SUMMARY FILE
C ----------------------
      WRITE(LFN001,*)
      CLOSE(LFN001)
      GOTO 999
C
900   WRITE(LFNERR,901) FILOUT
901   FORMAT(/,' ### SR EXTCRD: NO CONTENTS FOUND IN OUTPUT FILE',
     1                   /,16X,'OUTPUT FILE NAME: ',A32,/)
C
999   RETURN
      END SUBROUTINE

      END MODULE
