C*
      PROGRAM DEFXTR
CC
CC NAME       :  DEFXTR
CC
CC PURPOSE    :  READ THE ECLIPSING SATELLITES FROM DEFSTD OUTPUT
CC
CC REMARKS    :  SLIGHTLY MODIFIED VERSION FROM DEFXTR
CC
CC AUTHOR     :  S.FANKHAUSER
CC
CC CREATED    :  22-OCT-1992 8:00
CC
CC CHANGES    :  12-AUG-94 : MR: CALL EXITRC
CC            :  28-SEP-95 : JJ: DECLARE YES AS L*4 INSTEAD OF L*1
CC            :  05-OCT-95 : JJ: ADD KEEP_LOWER COMMENTS
CC               04-APR-96 : SF: CHANGE FORMAT X --> 1X
CC               06-JUN-96 : TS: REMOVED UNUSED VARIABLES
CC               24-MAR-97 : TS: AUTOMATIC ARC SPLITTING
CC               24-SEP-97 : DI: USE MAXSAT.inc
CC               16-JUN-99 : TS: BUG CORRECTED IN EXTDEF CALL
CC               03-JUL-00 : TS: FORMAT FROM I3 TO I4
CC               07-NOV-00 : CU: SWITCH TO NEW MENU SYSTEM
CC               30-OCT-01 : DI: WRITE ISATNR WITH I3
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               28-FEB-03 : SC: ADD TITLE SECTION
CC               23-APR-03 : HU: NULLIFY LOCAL POINTERS
CC               23-SEP-03 : RD: LENGTH OF FILE NAME MUST NOT BE FIX
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               21-JUN-05 : MM: LFNUM.inc, COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               27-FEB-07 : AG: CALL DEFCON
CC               05-DEC-07 : RD: INIT IWKSUM, INCREASE FORMAT TO 100 SAT.
CC               28-JUL-09 : SS: ARC SPLIT OPTIONS SPECIFIC TO EACH GNSS
CC               10-AUG-10 : SS: CHECK QUALITY OF ORBIT FITTING
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               24-AUG-11 : SS/RD/SL: NEW DEFINITION OF TMJD,
CC                               ARC SPLIT FOR IDTARC=4 PROHIBITTED
CC               25-AUG-11 : PS: WRITE ARC SPLITTING OPTIONS TO OUTPUT
CC               26-AUG-11 : SL: USE M_BERN WITH ONLY, MORE OUTPUT, USE PGNAME
CC               05-SEP-11 : SL/SS: ALLOW ASPLIT FOR ULTRAS
CC               13-SEP-11 : SL: FACTOR>0D0, OUTPUT MOD.+ENDING BLANK LINE
CC               14-NOV-11 : SL: NEW TITLE STRING FOR PRITIT
CC               04-MAY-12 : RD: USE DMOD FROM MODULE
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1992     UNIVERSITY OF BERN
CC               SWITZERLAND
C*
      USE m_bern,   ONLY: keyValueLength, lfnPrt, lfnErr, lfn001,
     1                    lfn002
      USE m_cpu,    ONLY: cpu_start
      USE m_maxdim, ONLY: maxsat
      USE m_global, ONLY: maxsys, g_strsys, g_svnsys
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE d_const,  ONLY: filTitle, date, time
CCC      USE l_basfun, ONLY: dmod
C
      USE s_opnfil
      USE s_defcon
      USE s_prflna
      USE s_gtfile
      USE s_pritit
      USE s_readinpf
      USE s_opnerr
      USE s_asplit
      USE s_readkeys
      USE s_ckoptr
      USE s_stripdir
      USE s_exitrc
      USE s_opnsys
      USE s_extdef
      USE s_jmt
      USE s_gtflna
      USE f_djul
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I1    , I2    , I3    , IARC  , IDAY  , IDTARC, IDUMMY,
     1          II    , IJ    , IOSTAT, IR    , IRC   , IRCCRX, IRCFIL,
     2          IRCSPL, ISATNR, IWKSUM, J     , JAN   , K     , M     ,
     3          MAXFIL, NBAD  , NFIL  , NFLCOL, IRCSUM, ISYS
C
      REAL*8    D     , FIRST , RMSMAX, SMJD  , RMSTOL
C
CCC       IMPLICIT  REAL*8  (A-H,O-Z)
C
      PARAMETER (MAXFIL=100)
C
C MAXFIL: MAXIMUM NUMBER OF INPUT FILES
C MAXSAT: MAXIMUM NUMBER OF SATELLITES
C
      CHARACTER*62  ECLIN
      CHARACTER*32  FILNAM(MAXFIL),FILOUT,FILCRX
      CHARACTER*32  FLNAME
      CHARACTER*1   ECL(MAXSAT,MAXFIL)
C
      INTEGER*4     ECLTIM(MAXSAT,MAXFIL),IRMS(MAXSAT),IRMSECL(MAXSAT)
      INTEGER*4     SVN(MAXSAT,MAXFIL),SVNECL(MAXSAT,MAXFIL)
      INTEGER*4     NSAT(MAXFIL),SVNBAD(MAXSAT)
      INTEGER*4     NECL(MAXFIL)
      INTEGER*4     DOY(2,MAXFIL)
C
      REAL*8        RMSSAT(MAXSAT,MAXFIL),RMSBAD(MAXSAT),EDIT(MAXSYS)
      REAL*8        FACLIM(2,MAXSYS)
      REAL*8        TMJD(2)
C     Variables for readkey
      CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue
C
      CHARACTER(LEN=6), PARAMETER :: pgName = 'DEFXTR'
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C NULLIFY POINTERS
C ----------------
      NULLIFY(keyValue)
C
C GET THE NAME OF THE INPUT FILE
C ------------------------------
      CALL init_inpkey(inpKey)
      CALL readinpf(' ',inpKey)
C
C DEFINE SYSTEM FILES AND CONSTANTS
C ---------------------------------
      CALL OPNSYS
      CALL DEFCON(0)

C PRINT TITLE IN OUTPUT FILE
C --------------------------
      CALL pritit(pgName,'Extract ORBGEN program output')
      CALL PRFLNA
C
C GET LIST OF FILES
C -----------------
      NFLCOL=1
      CALL readkeys ('RADIO_1',keyValue,irc)
      IF (keyValue(1) .EQ. '1') THEN
        CALL GTFILE('JOBOUT',NFLCOL,MAXFIL,NFIL,FILNAM)
      ELSE IF (keyValue(1) .EQ. '0') THEN
        CALL GTFILE('OUTPUT',NFLCOL,MAXFIL,NFIL,FILNAM)
      END IF
C
C READ QUALITY CHECK OPTIONS
C --------------------------
      CALL readkeys('TOL_RMS',keyValue,irc)
      CALL ckoptr(1,'TOL_RMS',keyValue,pgName,
     1            'Maximally tolerated RMS value',irc,ircSum,
     2            empty=0d0,ge=0d0,result1=RMSTOL)
C
C READ ARC SPLIT OPTIONS
C ----------------------
      FACLIM(:,:)=0d0
      CALL readkeys('FAC_GPS',keyValue,irc)
      CALL ckoptr(1,'FAC_GPS',keyValue,pgName,
     1            'Factor wrt mean RMS (GPS)',irc,ircSum,
     2            empty=0d0,gt=0d0,result1=FACLIM(1,1))
      CALL readkeys('FAC_GLO',keyValue,irc)
      CALL ckoptr(1,'FAC_GLO',keyValue,pgName,
     1            'Factor wrt mean RMS (GLONASS)',irc,ircSum,
     2            empty=0d0,gt=0d0,result1=FACLIM(1,2))
      CALL readkeys('LIM_GPS',keyValue,irc)
      CALL ckoptr(1,'LIM_GPS',keyValue,pgName,
     1            'Lower limit for RMS (GPS)',irc,ircSum,
     2            empty=0d0,ge=0d0,result1=FACLIM(2,1))
      CALL readkeys('LIM_GLO',keyValue,irc)
      CALL ckoptr(1,'LIM_GLO',keyValue,pgName,
     1            'Lower limit for RMS (GLONASS)',irc,ircSum,
     2            empty=0d0,ge=0d0,result1=FACLIM(2,2))
C
      WRITE(lfnprt,'(A)') ' THRESHOLD VALUES FOR RMS'
      WRITE(lfnprt,'(A)') ' ------------------------'
      WRITE(lfnPrt,'(1X,A7,2(2X,A19))')
     1  'SYSTEM ','FACTOR WRT MEAN RMS','LOWER LIMIT FOR RMS'
      WRITE(lfnPrt,'(1X,A7,2(2X,A19))')
     1  '-------','-------------------','-------------------'
      DO ISYS=1,MAXSYS
        IF(FACLIM(1,ISYS).NE.0d0)
     1    WRITE(lfnPrt,'(1X,A7,2X,F12.4,7X,F14.4,A)')
     2    g_strsys(ISYS-1),FACLIM(1,ISYS),FACLIM(2,ISYS),' m'
      ENDDO
      WRITE(lfnPrt,'(/)')
C
C OPEN RESULT FILE AND EVENTUALLY THE SCRATCH FILE
C ------------------------------------------------
      CALL GTFLNA(1,'OUTFIL ',FILOUT,IRCFIL)
      CALL OPNFIL (LFN001,FILOUT,'NEW','FORMATTED',
     1               ' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFN001,IOSTAT,FILNAM(1),pgName)
      CALL GTFLNA(0,'WEEKFM ',FILOUT,IRCFIL)
      IWKSUM=0
      IF (IRCFIL.EQ.0) THEN
        IWKSUM=1
        CALL OPNFIL (LFN002,FILOUT,'NEW','FORMATTED',
     1                 ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFN002,IOSTAT,FILNAM(1),pgName)
      ENDIF
C
C LOOP OVER ALL FILES
C -------------------
      DO 10 I1=1,NFIL
        IARC=1
        CALL EXTDEF(FILNAM(I1),IARC,TMJD,NSAT(I1),SVN(1,I1),
     1              RMSSAT(1,I1),NECL(I1),SVNECL(1,I1),ECLTIM(1,I1),
     2              IDUMMY,IRC)
CCC        IF (DMOD(TMJD(1),1.D0).GT.0.999)  TMJD(1)=TMJD(1)+1.D0
CCC        IF (DMOD(TMJD(2),1.D0).LT.1.0E-3) TMJD(2)=TMJD(2)-1.D0
        TMJD(1)=DINT(TMJD(1)+0.001d0)
CCC        TMJD(2)=DINT(TMJD(2)+0.001d0)
C ATTENTION: ASPLIT FOR ULTRA-RAPID UPDATES (WITH >12 HOURS)
        TMJD(2)=DINT(TMJD(2)+0.501d0)
        JAN=1
        FIRST=1.D0
        DO 210 I2=1,2
          CALL JMT(TMJD(I2),J,M,D)
          DOY(I2,I1)=IDNINT(TMJD(I2)-DJUL(J,JAN,FIRST))+1
210     CONTINUE
C_KEEP_LOWER_ON
C        ECLIN='0123456789abcdefghijklmnopqrstuvwxyz'//
C     1                  'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
C_KEEP_LOWER_OFF
        ECLIN='                                    '//
     1                  '                          '
        DO 230 I2=1,MAXSAT
          ECL(I2,I1)=' '
230     CONTINUE
        DO 220 I2=1,NECL(I1)
          I3=IDNINT(ECLTIM(I2,I1)/60.D0)
          ECL(I2,I1)=ECLIN(I3:I3)
220     CONTINUE
        ISATNR = SVN(1,I1)
        RMSMAX = RMSSAT(1,I1)
        DO II=2,NSAT(I1)
          IF (RMSSAT(II,I1).GT.RMSMAX) THEN
            RMSMAX = RMSSAT(II,I1)
            ISATNR = SVN(II,I1)
          ENDIF
        ENDDO
C
C FIND RMS OF ECLIPSING SATELLITES
C --------------------------------
        DO II=1,NECL(I1)
          DO IJ=1,NSAT(I1)
            IF (SVNECL(II,I1).EQ.SVN(IJ,I1)) THEN
              IRMSECL(II) = IDNINT(RMSSAT(IJ,I1)*1000D0)
            ENDIF
          ENDDO
        ENDDO
C
        IDAY = (DOY(1,I1)+DOY(2,I1))/2
        IF (IWKSUM.EQ.1) THEN
          DO IR=1,NSAT(I1)
            IRMS(IR) = IDNINT(RMSSAT(IR,I1)*100.0)
          ENDDO
          WRITE(LFN002,10005)IDAY,(IRMS(IR),IR=1,NSAT(I1))
10005     FORMAT(1X,I3,100I4)
        ENDIF
        FLNAME=FILNAM(I1)
        CALL STRIPDIR(FLNAME)
        WRITE(LFN001,10000)FLNAME,NSAT(I1),NECL(I1),
     1                     RMSMAX,ISATNR
10000   FORMAT(2X,A15,1X,'# Sat.: ',I3,' , # Eclipsing: ',I2,
     1         ' , Max. RMS:',F6.3,' for sat. ',I3)
        WRITE(LFN001,10010)IDAY,((SVNECL(K,I1)),K=1,NECL(I1))
10010   FORMAT('  (DOY: ',I3,')',6X,
     1             'Eclips. Sat. : ',20I4)
        WRITE(LFN001,10020)((ECLTIM(K,I1)/60),K=1,NECL(I1))
10020   FORMAT(18X,'Min in eclips: ',20I4)
        WRITE(LFN001,10030)(IRMSECL(K),K=1,NECL(I1))
10030   FORMAT(18X,'RMS (mm)     : ',20I4)
        WRITE(LFN001,'(A)')
10    CONTINUE
C
      CLOSE(LFN001)
      IF(IWKSUM.EQ.1) CLOSE(LFN002)
C
C ARC SPLIT INFORMATION FOR SATCRUX FILE
C --------------------------------------
      IDTARC=IDNINT(TMJD(2)-TMJD(1))
      CALL GTFLNA(0,'ASPLIT ',FILOUT,IRCSPL)
      IF((NFIL.EQ.1).AND.(IRCSPL.EQ.0)) THEN
CCC       IF((IDTARC.EQ.2).OR.(IDTARC.EQ.4)) THEN
        IF((IDTARC.EQ.2)) THEN
          CALL OPNFIL(LFN002,FILOUT,'NEW','FORMATTED',
     1                   ' ',' ',IOSTAT)
          CALL OPNERR(LFNERR,LFN002,IOSTAT,FILOUT,pgName)
          SMJD=DNINT(TMJD(1))+DBLE(IDTARC/2)
          CALL ASPLIT(SMJD,NSAT(1),SVN(1,1),RMSSAT(1,1),FACLIM,EDIT,
     1                NBAD,SVNBAD,RMSBAD,IRC)
          WRITE(lfnPrt,'(A)')
     1      ' SYSTEM   COMPUTED MEAN RMS  EDITING LEVEL'
          WRITE(lfnPrt,'(A)')
     1      ' -------  -----------------  -------------'
          DO ISYS=1,MAXSYS
            IF(FACLIM(1,ISYS).NE.0d0)
     1        WRITE(lfnPrt,'(1X,A7,2X,F12.1,A,2X,F10.1,A)')
     2          g_strsys(ISYS-1),
     3          EDIT(ISYS)/FACLIM(1,ISYS)*1000D0,' mm',
     4          EDIT(ISYS)*1000D0,' mm'
          ENDDO
          WRITE(lfnPrt,'(/)')
          WRITE(lfnPrt,'(1X,A4,2X,A)') 'Sat.','RMS (mm)'
          WRITE(lfnPrt,'(A)') ' ----  ---------'
          DO K=1,NSAT(1)
            WRITE(lfnPrt,'(1X,I4,2X,F7.1)',ADVANCE='NO')
     1        SVN(K,1),RMSSAT(K,1)*1000D0
            DO J=1,NBAD
              IF(SVNBAD(J).EQ.SVN(K,1))
     1          WRITE(lfnPrt,'(1X,A)',ADVANCE='NO') '*'
            ENDDO
            WRITE(lfnPrt,'(A)')
          ENDDO
C
          filTitle(65:80)=' '//date//' '//time
          WRITE(lfn002,'(A)') filTitle
          WRITE(lfn002,'(A)',ADVANCE='NO')
     1      'Mean RMS and editing level:'
          DO ISYS=1,MAXSYS
            IF(FACLIM(1,ISYS).NE.0d0)
     1        WRITE(lfn002,'(1X,A1,F6.3,A,F6.3,1X)',ADVANCE='NO')
     1          g_svnsys(ISYS-1),
     1          EDIT(ISYS)/FACLIM(1,ISYS),'/',EDIT(ISYS)
          ENDDO
          WRITE(lfn002,'(A)')
C
          CALL GTFLNA(0,'SATCRUX',FILCRX,IRCCRX)
          CALL JMT(SMJD,J,M,D)
          IF(NBAD.GT.0) THEN
            WRITE(LFN002,301) NBAD,J,M,IDNINT(D),
     1                        TRIM(FILCRX),TRIM(FILNAM(1))
            WRITE(LFN002,302) (SVNBAD(J),J=1,NBAD)
            WRITE(LFN002,303) (IDNINT(RMSBAD(J)*1000D0),J=1,NBAD)
301         FORMAT('Arc splitting for',I3,' sat. on ',
     1        I4.4,'-',I2.2,'-',I2.2,' in ',A,/,'based on ',A)
302         FORMAT(2X,'Sat.    :',100I4)
303         FORMAT(2X,'RMS (mm):',100I4)
          ELSE
            WRITE(lfn002,301) NBAD,J,M,IDNINT(D),
     1                        TRIM(FILCRX),TRIM(FILNAM(1))
          ENDIF
        ENDIF
        WRITE(lfn002,'(A)')
        CLOSE(lfn002)
      ENDIF
C
C ERROR IF MAXIMALLY TOLERATED ORBIT FITTING RMS EXCEEDED
C -------------------------------------------------------
      IF(IRCSPL.NE.0.AND.RMSTOL.GT.0D0.AND.RMSMAX.GT.RMSTOL) THEN
        WRITE(LFNERR,901) ISATNR,RMSMAX,RMSTOL
901     FORMAT(/,' *** PG DEFXTR: ',
     1         'MAXIMALLY TOLERATED ORBIT FITTING RMS EXCEEDED',
     2         /,16X,'SATELLITE:',I5,
     3         /,16X,'RMS      :',F9.3,
     4         /,16X,'TOLERANCE:',F9.3,/)
        CALL EXITRC(2)
      ENDIF
C
      CALL EXITRC(0)
      END
