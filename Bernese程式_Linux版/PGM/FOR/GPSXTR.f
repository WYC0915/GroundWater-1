C*
      PROGRAM GPSXTR
CC
CC NAME       :  GPSXTR
CC
CC PURPOSE    :  THIS PROGRAM EXTRACTS SOME INFORMATION OF THE GPSEST
CC               JOB OUTPUT FILES IN A FORMAT SUITALBE TO BE INCLUDED
CC               INTO THE DAILY PROTOCOLS.
CC
CC REMARKS    :
CC
CC AUTHOR     :  S.FANKHAUSER
CC
CC CREATED    :  20-OCT-92
CC
CC CHANGES    :  08-DEC-92 : TS: CHANGED OUTPUT FORMAT TO BE COMPATIBLE
CC                               WITH THE DAILY PROTOCOL LAYOUT.
CC               09-DEC-92 : TS: ADDED OUTPUT POSSIBILITY FOR WEEKLY
CC                               SUMMARY FORMAT
CC               15-OCT-93 : RW: POLE PARAMETER SETS
CC                               ERROR PROPAGATION
CC               12-AUG-94 : MR: CALL EXITRC
CC               17-DEC-94 : RW: CHANGE OUTPUT FORMAT FOR # OF PARM TO I5
CC               19-APR-95 : TS: CHANGED INDEX SEARCH FOR GPSEST OUTPUT NAME
CC               13-MAI-95 : EB: IMEAN=7 FOR WEEKLY SOLUTIONS
CC               03-JAN-96 : SS: EXTRACT GIM INFORMATION USING SR EXTGIM
CC               03-JAN-96 : SS: DEFINE "FILTYP"
CC               17-JAN-96 : SS: EXTRACT INFORMATION CONCERNING AMBIGUITY
CC                               RESOLUTION (QIF) USING SR EXTAMB
CC               17-JAN-96 : SS: EXTRACT COORDINATE DIFFERENCES USING
CC                               SR EXTCRD
CC               21-FEB-96 : TS: SEVERAL SMALL IMPROVEMENTS
CC               28-FEB-96 : MR: "X" IN FORMAT NOT ALLOWED --> "1X"
CC               04-APR-96 : SF: CHARACTER EXCEEDS LINE OF 72 CHARACTERS
CC               15-AUG-96 : TS: NUMOBS OUTPUT USING I7
CC               15-OCT-96 : MR: WRITE BLANK LINE ONLY IF IOU1=1
CC               10-JAN-97 : TS: NUMFIL OUTPUT USING I4
CC               14-APR-97 : TS: USE SUBR FPARSE TO GET FILENAME
CC               21-MAY-97 : MR: MAXFIL FROM 500 TO 2000
CC               06-AUG-97 : SS: EXTRACT NUMBER OF CONTRIBUTING STATIONS
CC               21-OCT-98 : TS: ADDED KINEMATIC COORDINATE EXTRACTIONS
CC               01-JUN-99 : SS: MAXFIL FROM 2000 TO 4000
CC               15-SEP-99 : TS: NAG BUG CORRECTED
CC               14-MAR-00 : SS: CONSIDER ADDNEQ2 OUTPUT FILES
CC               15-JUN-00 : SS: WRITE #PAR WITH I6
CC               14-AUG-01 : HU: SWITCH TO NEW MENU
CC               28-MAY-02 : SS: WEEKLY SUMMARY FORMAT STATEMENT MODIFIED
CC               25-SEP-02 : HU: REMOVE I_ASTLIB
CC               22-APR-03 : SS: WRITE 3-DIGIT DOY
CC               23-APR-03 : HU: NULLIFY LOCAL POINTERS
CC               04-NOV-03 : RD: REMOVE RADIO_2 (ADDNEQ.Lxx == GPSEST.LXX)
CC               19-NOV-03 : RD: READ INP-FILENAME IN READINPF
CC               28-JAN-04 : PS: NUMOBS OUTPUT USING I8
CC               23-FEB-04 : SS: WRITE 10-CHARACTER OUTPUT FILENAME
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: LFNUM.inc, COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               27-FEB-07 : AG: CALL DEFCON WITH PARAMETER
CC               23-MAR-07 : RD: BIG NUMOBS AND NPAR (****** -> 999999)
CC               15-FEB-08 : SL: CHANGE PROGRAM TITLE
CC               27-MAY-09 : RD: EXTRACT CLOCK ESTIMATES
CC               23-SEP-10 : RD: ENABLE CPU COUNTER
CC               24-NOV-10 : SL: USE M_BERN WITH ONLY
CC               17-FEB-11 : MM: NEW OPTIONS FOR AMBIGUITY SUMMARY
CC               11-NOV-11 : RD: REPORT PROVIDED SUMMARY FILES
CC               14-NOV-11 : SL: NEW TITLE STRING FOR PRITIT
CC               26-SEP-12 : RD: NO SUMMARY FROM EXTGPS IF SOLUTION WAS SKIPPED
CC               26-OCT-12 : RD: EXTENT FILENAME IN SUMMARY
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1992     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnErr, lfnRes, lfn001, lfn002, lfnprt,
     1                    i4b, keyValueLength, fileNameLength
      USE m_cpu,    ONLY: cpu_start
      USE d_inpkey, ONLY: inpKey, init_inpkey
      USE m_maxdim, ONLY: maxsta,maxsat
C
      USE s_opnfil
      USE s_pritit
      USE s_extgps
      USE s_readkeys
      USE s_extgim
      USE s_defcon
      USE s_opnsys
      USE s_gxinpt
      USE s_gtflna
      USE s_extkin
      USE s_prflna
      USE s_extcrd
      USE s_gtfile
      USE s_extamb
      USE s_readinpf
      USE s_opnerr
      USE s_prfile
      USE s_fparse
      USE s_exitrc
      USE f_lengt0
      USE f_lengt1
      USE s_extclk
      USE s_extcl2

      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I1      , I2      , I3      , I4      , IA      ,
     1          IB      , ICAMPSUM, IDOY    , IHLP1   , IHLP2   ,
     2          IMEAN   , IOSTAT  , IOU1    , IOU2    , IRC     ,
     3          IRCOU1  , IRCOU2  , IRETC   , ISAT    , ISES    ,
     4          IWKSUM  , LFNPOL  , LLF     ,
     5          LLP     , LLT     , MAXFIL  , MOD     , NFIL    ,
     6          NFLCOL  , NPAR    , NREQ    , NUMFIL  , NUMOBS  ,
     7          NUMSTA  , IHELP1  , IHELP2  , IHELP3
C
      REAL*8    DA      , DUTPOLE , DXPOLE  , DYPOLE  , P1      ,
     1          P2      , P3      , PHLP1   , PHLP2   , RFAC    ,
     2          RMS     , RMSA    , RMSW    , UTPOLE  , XPOLE   ,
     3          YPOLE
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C MAXIMAL DIMENSIONS
C ------------------
      PARAMETER (MAXFIL=4000)
C
C MAXFIL: MAXIMUM NUMBER OF GPSOUTPUT FILES
C
C DECLARATIONS
C ------------
      CHARACTER*134 POLTXT
      CHARACTER*80  TITLE,TITNEW
      CHARACTER*32  FILNAM(MAXFIL),FILOUT,POLOUT
      CHARACTER*32  NODE, DEVICE, DIR, NAME, EXT, VER
      CHARACTER*16  CLKNAM(2,MAXSTA+MAXSAT)
      CHARACTER*10  RMSHLP
      CHARACTER*2   HLPCH
      CHARACTER*7   KEYWRD
      CHARACTER*3   AMBID
C
      REAL*8        POLCOR(3,10,100)
C
      INTEGER*4     FILTYP(MAXFIL)
      INTEGER*4     AMBLOD
C Variables for readkey
      CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue
C
C Variables for list of summary files
      INTEGER(i4b) :: ISUM
      INTEGER(i4b) :: SUMIRC
      CHARACTER(LEN=fileNameLength) :: sumfil
      CHARACTER(LEN=6), DIMENSION(11), PARAMETER :: sumLst =
     1   (/ 'GPSOUT','CRDOUT','KINOUT','GIMOUT','CLKOUT','CLKDEV',
     2      'QIFOUT','AMBOUT','CAMPFM','WEEKFM','POLOUT' /)
C
C START CPU COUNTER
C -----------------
      CALL cpu_start(.TRUE.)
C
C NULLIFY POINTERS
C ----------------
      NULLIFY(keyValue)
      CALL init_inpkey(inpKey)
C
C GET THE NAME OF THE INPUT FILE
C ------------------------------
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
C WRITE TITLE AND INPUT FILE NAMES
C --------------------------------
      CALL PRITIT('GPSXTR','Extract GPSEST/ADDNEQ2 program output')
      CALL PRFLNA
C
C GET GPSEST/ADDNEQ FILE NAMES
C ----------------------------
      NFLCOL=1
      CALL readkeys ('RADIO_3',keyValue,irc)
      IF (keyValue(1) .EQ. '1') THEN
        KEYWRD='OUTPUT'
      ELSE
        KEYWRD='JOBGPS'
      END IF
      CALL GTFILE(KEYWRD,NFLCOL,MAXFIL,NFIL,FILNAM)
      CALL PRFILE (KEYWRD,'GPSEST/ADDNEQ2 INPUT FILES',NFLCOL)
C
C READ INPUT OPTIONS
C ------------------
      CALL GXINPT(TITNEW,IMEAN,AMBLOD,AMBID)
C
C OPEN THE GPSXTR OUTPUT FILES
C ----------------------------
      IOU1=0
      CALL GTFLNA(0,'GPSOUT ',FILOUT,IRCOU1)
      IF (IRCOU1.EQ.0) THEN
        CALL OPNFIL(LFN001,FILOUT,'UNKNOWN','FORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFN001,IOSTAT,FILOUT,'GPSXTR')
        IOU1=1
      END IF
C
C OPEN WEEK SUMMARY FORMAT FILE IF REQUESTED
C ------------------------------------------
      CALL GTFLNA(0,'WEEKFM ',FILOUT,IWKSUM)
      IF (IWKSUM.EQ.0) THEN
        CALL OPNFIL(LFN002,FILOUT,'UNKNOWN','FORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFN002,IOSTAT,FILOUT,'GPSXTR')
      END IF
C
C OPEN CAMPAIGN SUMMARY FORMAT FILE IF REQUESTED
C ----------------------------------------------
      CALL GTFLNA(0,'CAMPFM ',FILOUT,ICAMPSUM)
      IF (ICAMPSUM.EQ.0) THEN
        CALL OPNFIL(LFNRES,FILOUT,'UNKNOWN','FORMATTED',
     1              ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNRES,IOSTAT,FILOUT,'GPSXTR')
        IOU2=1
      END IF
C
C OPEN POLE OUTPUT FILE IF REQUESTED
C ----------------------------------
      IOU2=0
      CALL GTFLNA(0,'POLOUT ',POLOUT,IRCOU2)
      IF (IRCOU2.EQ.0) THEN
         LFNPOL=LFN002+1
         CALL OPNFIL(LFNPOL,POLOUT,'UNKNOWN','FORMATTED',
     1               ' ',' ',IOSTAT)
         CALL OPNERR(LFNERR,LFNPOL,IOSTAT,POLOUT,'GPSXTR')
        IOU2=1
      END IF
C
C LOOP OVER ALL OUTPUT FILES
C --------------------------
      DO 10 I1=1,NFIL
        CALL EXTGPS(FILNAM(I1)   ,TITLE ,FILTYP(I1)   ,NUMFIL,
     1              NPAR  ,NUMOBS,RMS   ,POLCOR,NREQ  ,IRC   ,
     2              ISAT  ,DA    ,RMSA  ,ISES  ,NUMSTA)
C
        IF (IMEAN.EQ.7)ISES=ISES-2
        IF (TITLE(2:6).EQ.'SCEN:') THEN
          READ(TITLE(7:9),*) MOD
          READ(TITLE(27:29),*) IDOY
          IF (IMEAN.EQ.1) IDOY=IDOY-1
          IF (IMEAN.EQ.3) IDOY=IDOY+1
        ELSE
          MOD=0
          IDOY=0
        END IF
        IF (IOU1.EQ.1) THEN
          CALL FPARSE(1,FILNAM(I1),NODE,DEVICE,DIR,NAME,EXT,
     1                VER,IRETC)
          FILOUT=NAME(1:LENGT0(NAME))//EXT(1:LENGT0(EXT))
          IHELP1=NUMFIL
          IHELP2=NUMOBS
          IHELP3=NPAR
          IF (IHELP1.GT.   9999) IHELP1=   9999
          IF (IHELP2.GT.9999999) IHELP2=9999999
          IF (IHELP3.GT. 999999) IHELP3= 999999
          IF (RMS == 1D20) THEN
            RMSHLP = '  ---'
          ELSEIF (RMS*1.0D3 > 999.9D0) THEN
            RMSHLP = '999.9'
          ELSE
            WRITE(RMSHLP,'(F5.1)') RMS*1.0D3
          ENDIF
C
          WRITE(LFN001,1)FILOUT,RMSHLP(1:5),IHELP1,IHELP2,IHELP3
1         FORMAT(1X,A16,1X,'Rms:',A,' , # fil.:',I4,' , # obs.:',
     1             I8,' , # par.:',I6)
          IF (ISAT == 0) THEN
            WRITE(LFN001,10000)ISES
10000       FORMAT(1X,'(DOY: ',I3.3,')')
          ELSE
            WRITE(LFN001,10001)ISES,DA,RMSA,ISAT
10001       FORMAT(1X,'(DOY: ',I3.3,')',19X,'Max. correction in a:',
     1             F7.2,' +-',F5.2,' for sat.:',I3)
          ENDIF
        ENDIF
        IF (IWKSUM.EQ.0) THEN
          IHELP1=NUMFIL
          IHELP2=NUMOBS
          IHELP3=NPAR
          IF (IHELP1.GT. 999999999) IHELP1=999999999
          IF (IHELP2.GT. 999999999) IHELP2=999999999
          IF (IHELP3.GT. 999999999) IHELP3=999999999
          IF (RMS == 1D20) THEN
            RMSHLP = '       ---'
          ELSEIF (RMS*1.0D3 > 99999999.9D0) THEN
            RMSHLP = '99999999.9'
          ELSE
            WRITE(RMSHLP,'(F10.1)') RMS*1.0D3
          ENDIF
          WRITE(LFN002,10005)ISES,NUMFIL,NUMOBS,NPAR,RMSHLP
10005     FORMAT(I3.3,1X,I11,1X,I9,1X,I15,1X,A)
        END IF
        IF (IOU2.EQ.1) THEN
          IF (IMEAN.EQ.0)THEN
            DO 20 I2=1,NREQ
              DO 30 I3=1,3
                POLTXT='                                        '//
     1                 '                                        '//
     2                 '                                        '//
     3                 '              '
                IF (I3.EQ.1) THEN
                  HLPCH='X '
                ELSE IF (I3.EQ.2) THEN
                  HLPCH='Y '
                ELSE IF (I3.EQ.3) THEN
                  HLPCH='DT'
                END IF
                POLTXT(3:4)=HLPCH
                DO 40 I4=1,5
                  PHLP1=POLCOR(I3,2*I4-1,I2)
                  PHLP2=POLCOR(I3,2*I4,I2)
                  P1=POLCOR(1,2*I4,I2)
                  P2=POLCOR(2,2*I4,I2)
                  P3=POLCOR(3,2*I4,I2)
                  IF (P1.NE.0.D0.OR.P2.NE.0.D0.OR.P3.NE.0.D0) THEN
                    IHLP1=10+25*(I4-1)
                    IHLP2=IHLP1+24
                    WRITE(POLTXT(IHLP1:IHLP2),2)PHLP1,PHLP2
2                   FORMAT(F12.7,F11.7,1X)
                  END IF
40              CONTINUE
                WRITE(POLTXT(6:8),1001) I2
1001            FORMAT(I3)
                LLP=LENGT0(POLTXT)
                IF (LLP.LT.98.AND.I3.EQ.1.AND.I2.EQ.1) THEN
                  LLF=LENGT0(FILNAM(I1))
                  WRITE(POLTXT(LLP+2:LLP+2+LLF),'(A)')FILNAM(I1)(1:LLF)
                ELSE IF (LLP.LT.98.AND.I3.EQ.2.AND.I2.EQ.1) THEN
                  LLT=LENGT0(TITLE)
                  WRITE(POLTXT(LLP+1:LLP+1+LLT),'(A)')TITLE(1:LLT)
                END IF
                WRITE(LFNPOL,*)POLTXT(1:LENGT1(POLTXT))
30            CONTINUE
              WRITE(LFNPOL,*)
20          CONTINUE
          ELSE IF(IMEAN.NE.0) THEN
            IF (IMEAN.EQ.9) THEN
              IA=1
              IB=NREQ
            ELSE
              IF (NREQ.GE.3) THEN
                IA=(IMEAN-1)*NREQ/3+1
                IB=IMEAN*NREQ/3
              ELSE IF (NREQ.LT.3) THEN
                IA=1
                IB=1
              END IF
            END IF
C
C            RFAC=IMEAN-0.5D0
C
C   THE LINE ABOVE IS REPLACED BY
            RFAC=0.5
C   DUE TO THE FACT THAT IMEAN SHOULD POINT TO THE MEAN PARAMETER SET
            DO 50 I2=IA,IB
              POLTXT='                                        '//
     1               '                                        '//
     2               '                                        '//
     3               '              '
              DO 60 I3=1,3
                IHLP1=10+23*(I3-1)
                IHLP2=IHLP1+22
                PHLP1=0.0
                PHLP2=0.0
                DO 70 I4=0,4
                  IF (POLCOR(I3,2*I4+1,I2).NE.0.D0.OR.I4.EQ.0) THEN
                    PHLP1=PHLP1+POLCOR(I3,2*I4+1,I2)*RFAC**I4
C
C   THE PURE LAW OF ERROR PROPAGATION IS IMPLEMENTED INSTEAD OF CORRECT
C   CORRELATION HANDLING. THEREFORE THE COMPUTED MEAN ERRORS OF FUNCTIONS
C   OF THE ESTIMATED POLE-PARAMETERS ARE TOO PESSIMISTIC. (PHLP2)
C
                    PHLP2=PHLP2+(POLCOR(I3,2*I4+2,I2)*RFAC**I4)**2
                   ELSE
                    GOTO 80
                  END IF
70              CONTINUE
80              CONTINUE
                PHLP2=SQRT(PHLP2)
                WRITE(POLTXT(IHLP1:IHLP2),3)PHLP1,PHLP2
3               FORMAT(F11.7,F10.7)
                IF ((PHLP1.NE.0.0).OR.(PHLP2.NE.0.0)) THEN
                  IF (I3.EQ.1) THEN
                    WRITE(LFN001,10010)
10010               FORMAT(30X,'Pole correction (middle of interval)')
                    WRITE(LFN001,10011)PHLP1*1.D3,PHLP2*1.D3
10011               FORMAT(41X,'in x(mas):',F7.2,' +-',F6.3)
                    XPOLE =  PHLP1*1.D3
                    DXPOLE = PHLP2*1.D3
                  ENDIF
                  IF (I3.EQ.2) THEN
                    WRITE(LFN001,10020)PHLP1*1.D3,PHLP2*1.D3
10020               FORMAT(41X,'in y(mas):',F7.2,' +-',F6.3)
                    YPOLE =  PHLP1*1.D3
                    DYPOLE = PHLP2*1.D3
                  ENDIF
                  IF (I3.EQ.3) THEN
                    WRITE(LFN001,10030)PHLP1*1.D6,PHLP2*1.D6
10030               FORMAT(36X,'in t(sec*1/D6):',F7.2,' +-',F5.2)
                    UTPOLE =  PHLP1*1.D6
                    DUTPOLE = PHLP2*1.D6
                  ENDIF
                ENDIF
60            CONTINUE
              IF (IDOY.NE.0.AND.MOD.NE.0) THEN
                WRITE(POLTXT(2:4),1001) IDOY
                WRITE(POLTXT(6:8),1001) MOD
              END IF
CC              WRITE(LFN001,*)POLTXT(1:LENGT1(POLTXT))
50          CONTINUE
          END IF
        END IF
        IF (IOU1.EQ.1) WRITE(LFN001,*)
        IF (ICAMPSUM.EQ.0) THEN
          RMSW = RMS*1.D3
          IHELP1=NUMOBS
          IHELP2=NPAR
          IHELP3=NUMFIL
          IF (IHELP1.GT. 999999) IHELP1= 999999
          IF (IHELP2.GT.  99999) IHELP2=  99999
          IF (IHELP3.GT.    999) IHELP3=    999
          IF (RMS == 1D20) THEN
            RMSHLP = '  ---'
          ELSEIF (RMSW > 999.9D0) THEN
            RMSHLP = '999.9'
          ELSE
            WRITE(RMSHLP,'(F5.1)') RMSW
          ENDIF
          WRITE(LFNRES,10015)ISES,RMSHLP(1:5),IHELP1,IHELP2,IHELP3,DA,
     1                       ISAT,XPOLE,DXPOLE,YPOLE,DYPOLE,UTPOLE,
     2                       DUTPOLE,NUMSTA
10015     FORMAT(1X,I3.3,A,I8,I6,I4,F7.2,I4,1X,
     1           2(F6.2,'+-',F5.3),F8.2,'+-',F5.2,I7)
        END IF
10    CONTINUE
C
      IF (IOU1.EQ.1) CLOSE(LFN001)
      IF (IWKSUM.EQ.0) CLOSE(LFN002)
      IF (ICAMPSUM.EQ.0) CLOSE(LFNRES)
      IF (IOU2.EQ.1) CLOSE(LFNPOL)
C
C EXTRACT COORDINATE DIFFERENCES
C ------------------------------
      CALL EXTCRD(NFIL,FILNAM,FILTYP)
C
C EXTRACT ESSENTIAL GIM INFORMATION
C ---------------------------------
      CALL EXTGIM(NFIL,FILNAM,FILTYP)
C
C EXTRACT ESSENTIAL INFORMATION CONCERNING AMBIGUITY RESOLUTION (QIF)
C -------------------------------------------------------------------
      CALL EXTAMB(NFIL,FILNAM,FILTYP,AMBLOD,AMBID)
C
C EXTRACT KINEMATIC COORDINATE ESTIMATES
C --------------------------------------
      CALL EXTKIN(NFIL,FILNAM,FILTYP)
C
C EXTRACT CLOCK ESTIMATES
C -----------------------
      CALL EXTCLK(NFIL,FILNAM,FILTYP,CLKNAM)
C
C EXTRACT CLOCK ESTIMATES
C -----------------------
      CALL EXTCL2(NFIL,FILNAM,FILTYP,CLKNAM)
C
C LIST SUMMARIES HAVE BEEN PROVIDED
C ---------------------------------
      WRITE(LFNPRT,'(A,3(/,A))')
     1 ' LIST OF PROVIDED SUMMARY FILES:',
     1 ' ---------------------------------------'//
     2 '----------------------------------------',
     3 ' Filename                           Summ'//
     4 'ary',
     5 ' ---------------------------------------'//
     6 '----------------------------------------'

      DO ISUM = 1,SIZE(SUMLST)
        CALL GTFLNA(0,SUMLST(ISUM),SUMFIL,SUMIRC)
        IF (SUMIRC.NE.0 .OR. LEN_TRIM(SUMFIL).EQ.0) CYCLE
        CALL READKEYS('MSG_'//SUMLST(ISUM),KEYVALUE,SUMIRC)
        WRITE(LFNPRT,'(1X,A,3X,A)') SUMFIL,TRIM(KEYVALUE(1))
      ENDDO
      WRITE(LFNPRT,'(A)')
     1 ' ---------------------------------------'//
     2 '----------------------------------------'
C
C END
C ---
      CALL EXITRC(0)
      END
