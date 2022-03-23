      MODULE s_WTCBFL
      CONTAINS

C*
      SUBROUTINE WTCBFL(DCBFIL,TITLE ,NUMSAT,NUMREC,NUMIFB,ICBTYP,
     1                  DCBID1,DCBVA1,DCBID2,DCBVA2,DCBSYS,DCBID3,
     2                  DCBVA3,DCBIN1,DCBIN2,DCBIN3)
CC
CC NAME       :  WTCBFL
CC
CC PURPOSE    :  WRITE CODE BIAS FILE
CC
CC PARAMETERS :
CC         IN :  DCBFIL : EXTERNAL CODE BIAS FILE NAME        CH*32
CC               TITLE  : GENERAL TITLE                       CH*80
CC               NUMSAT : NUMBER OF SATELLITES                I*4
CC               NUMREC : NUMBER OF RECEIVERS                 I*4
CC               NUMIFB : NUMBER OF INTER-FREQ. BIASES        I*4
CC               ICBTYP : CODE BIAS TYPE                      I*4
CC                        =1: P1-P2
CC                        =2: P1-C1
CC                        =3: LC
CC                        =4: P2-C2
CC                        =5: INTER-FREQ.
CC               DCBID1 : SATELLITE NUMBERS                   I*4(*)
CC                        =0: UNDEFINED
CC               DCBVA1 : DCB VALUES AND RMS FOR SATELLITES   R*8(2,*)
CC               DCBID2 : STATION NAMES                       CH*16(*)
CC                        =' ': UNDEFINED
CC               DCBVA2 : DCB VALUES AND RMS FOR RECEIVERS    R*8(2,*)
CC               DCBSYS : SYSTEM FLAG                         CH*1(*)
CC                        ='G'/' ': GPS
CC                        ='R': GLONASS
CC                        ='E': GALILEO
CC                        ='S': SBAS
CC                        ='C': COMPASS
CC                        ='J': QZSS
C
C                           ADD_GNSS_HERE
C
CC               DCBID3 : SATELLITES/STATION NAMES            CH*16(2,*)
CC                        =' ': UNDEFINED
CC               DCBVA3 : IFCB VALUES AND RMS FOR RECEIVERS   R*8(4,*)
CC                        RESP. THE VALIDITY INTERVAL
CC        OUT :  DCBIN1 : INDICES OF SATELLITES               I*4(*)
CC               DCBIN2 : INDICES OF RECEIVERS                I*4(*)
CC               DCBIN3 : INDICES OF IFCB                     I*4(*)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S.SCHAER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  20-OCT-97
CC
CC CHANGES    :  23-OCT-97 : SS: ORDER RECEIVER LIST
CC               19-JAN-98 : SS: USE ALSO SR OPNERR
CC               12-FEB-98 : SS: 0.001-NS RESOLUTION
CC               06-APR-98 : SS: STRING CORRECTED
CC               21-DEC-98 : SS: GPS/GLONASS DCBS FOR RECEIVERS
CC               07-APR-00 : SS: HANDLE (P1-C1) CODE BIAS INFORMATION
CC               16-AUG-02 : SS: CREATE AUXILIARY DCB FILE SPECIFIC TO
CC                               CC2NONCC UTILITY
CC               21-AUG-02 : SS: HANDLE THIRD TYPE OF DCB VALUES
CC               13-MAR-03 : SS: ACCOMMODATE LEO DCB VALUES
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               09-MAY-09 : RD: ADD INTER-FREQUENCY CODE BIASES
CC               29-JUL-09 : SS: HANDLE P2-C2 DCB VALUES
CC               08-JUN-11 : LP: ENABLE 'FLAG' SETTING FOR GALILEO AND OTHERS
CC               30-NOV-11 : SL: USE M_BERN WITH ONLY
CC               06-JUN-12 : LP: Use g_svnsys and maxsys to set FLAG, IPRN
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,    ONLY: lfnErr, lfnLoc
      USE m_global,  ONLY: g_svnsys, maxsys
      USE s_iordup
      USE s_opnfil
      USE s_opnerr
      USE s_cordup
      USE s_exitrc
      USE s_timst2
      USE f_lengt1
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , ICBTYP, IOSTAT, IPRN  , IREC  , ISAT  ,
     1          NUMREC, NUMSAT, NUMIFB
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*80  TITLE
      CHARACTER*40  HLPSTR
      CHARACTER*32  DCBFIL,FORFIL
      CHARACTER*32  DCBHL3(NUMIFB)
      CHARACTER*16  DCBID2(*),DCBID3(2,*)
      CHARACTER*9   AUXVAL(40)
      CHARACTER*5   TYPSTR
      CHARACTER*1   DCBSYS(*),FLAG
C
      REAL*8        DCBVA1(2,*),DCBVA2(2,*),DCBVA3(4,*)
C
      INTEGER*4     DCBID1(*)
      INTEGER*4     DCBIN1(*),DCBIN2(*),DCBIN3(*)
C
C NOTHING TO DO
C -------------
      IF (NUMSAT+NUMREC+NUMIFB.EQ.0) RETURN
C
C ORDER SATELLITE LIST
C --------------------
      CALL IORDUP(DCBID1,NUMSAT,DCBIN1)
C
C ORDER RECEIVER LIST
C -------------------
      DO IREC=1,NUMREC
        IF (DCBID2(IREC)(1:1).EQ.'L' .AND.
     1      DCBID2(IREC)(4:4).EQ.' ') DCBID2(IREC)(1:1)='~'
      ENDDO
C
      CALL CORDUP(DCBID2,NUMREC,1,16,DCBIN2)
C
      DO IREC=1,NUMREC
        IF (DCBID2(IREC)(1:1).EQ.'~' .AND.
     1      DCBID2(IREC)(4:4).EQ.' ') DCBID2(IREC)(1:1)='L'
      ENDDO
C
C OPEN CODE BIAS OUTPUT FILE
C --------------------------
      CALL OPNFIL(LFNLOC,DCBFIL,'UNKNOWN','FORMATTED',
     1  ' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNLOC,IOSTAT,DCBFIL,'WTCBFL')
C
      WRITE(LFNLOC,'(A80,/,80("-"))') TITLE
C
C WRITE GENERAL TITLE
C -------------------
      IF (NUMSAT+NUMREC.GT.0) THEN
        IF (ICBTYP.EQ.1) THEN
          TYPSTR='P1-P2'
        ELSEIF (ICBTYP.EQ.2) THEN
          TYPSTR='P1-C1'
        ELSEIF (ICBTYP.EQ.3) THEN
          TYPSTR='LC'
        ELSEIF (ICBTYP.EQ.4) THEN
          TYPSTR='P2-C2'
        ELSE
          WRITE(LFNERR,930)
930       FORMAT(/,' *** SR WTCBFL: ILLEGAL CODE BIAS TYPE',/)
          CALL EXITRC(2)
        ENDIF
C
        WRITE(LFNLOC,910) TYPSTR(1:LENGT1(TYPSTR))
910     FORMAT(/
     1    'DIFFERENTIAL (',A,') CODE BIASES ',
     2    'FOR SATELLITES AND RECEIVERS:',//,
     3    'PRN / STATION NAME        VALUE (NS)  RMS (NS)',/,
     4    '***   ****************    *****.***   *****.***')
C
C WRITE SATELLITE-SPECIFIC INFO
C -----------------------------
        DO ISAT=1,NUMSAT
          IF (DCBID1(DCBIN1(ISAT)).EQ.0) GOTO 111
          IF ((INT(DCBID1(DCBIN1(ISAT))/100))>(MAXSYS-1)) THEN
            WRITE(LFNERR,953) DCBID1(DCBIN1(ISAT))
953         FORMAT(/,' ### SR WTCBFL: UNKNOWN SATSYS FOR SAT',I3,/)
            CYCLE
          ELSE
            FLAG = g_svnsys(INT(DCBID1(DCBIN1(ISAT))/100))
            IPRN = DCBID1(DCBIN1(ISAT))-
     1             (INT(DCBID1(DCBIN1(ISAT))/100))*100
          ENDIF
C
C
          WRITE(LFNLOC,921) FLAG,IPRN,(DCBVA1(I,DCBIN1(ISAT)),I=1,2)
921       FORMAT(A1,I2.2,23X,F9.3,3X,F9.3)
CC921       FORMAT(A1,I2.2,20X,F12.3,F12.3)
        ENDDO
111     CONTINUE
C
C WRITE RECEIVER-SPECIFIC INFO
C ----------------------------
        DO IREC=1,NUMREC
          IF (DCBID2(DCBIN2(IREC)).EQ.' ') GOTO 112
C
          WRITE(LFNLOC,922) DCBSYS(DCBIN2(IREC)),DCBID2(DCBIN2(IREC)),
     1      (DCBVA2(I,DCBIN2(IREC)),I=1,2)
922       FORMAT(A1,5X,A16,4X,F9.3,3X,F9.3)
CC922       FORMAT(A1,5X,A16,1X,F12.3,F12.3)
        ENDDO
112     CONTINUE
      ENDIF
C
C PART II: INTER-FRERQUENCY CODE BIASES
C -------------------------------------
      IF (NUMIFB.GT.0) THEN
        DO I=1,NUMIFB
          DCBHL3(I) = DCBID3(2,I) // TRIM(DCBID3(1,I))
        ENDDO
        CALL CORDUP(DCBHL3,NUMIFB,1,19,DCBIN3)
C
        WRITE(LFNLOC,911)
911     FORMAT(/
     1    'DIFFERENTIAL (INTER-FREQ) CODE BIASES ',
     2    'FOR SATELLITES AND RECEIVERS:',//,
     3    'PRN / STATION NAME        VALUE (NS)  RMS (NS)     ',
     4    'VALID   FROM                  TO',/,
     5    '***   ****************    *****.***   *****.***    ',
     6    '**** ** ** ** ** **  **** ** ** ** ** **')
C
C WRITE SATELLITE-SPECIFIC INFO
C -----------------------------
        DO I=1,NUMIFB
          CALL TIMST2(2,2,(/DCBVA3(3,DCBIN3(I)),DCBVA3(4,DCBIN3(I))/),
     1                HLPSTR)
          WRITE(LFNLOC,'(A3,3X,A16,4X,F9.3,3X,F9.3,4X,A)')
     1          DCBID3(1,DCBIN3(I))(1:3),DCBID3(2,DCBIN3(I)),
     2          DCBVA3(1:2,DCBIN3(I)),HLPSTR
        ENDDO
      ENDIF
C
C WRITE BLANK LINE
C ----------------
      WRITE(LFNLOC,*)
C
C CLOSE CODE BIAS OUTPUT FILE
C ---------------------------
      CLOSE (UNIT=LFNLOC)
C
C CREATE AUXILIARY DCB FILE SPECIFIC TO CC2NONCC UTILITY
C ------------------------------------------------------
      IF (ICBTYP.EQ.2.AND.NUMSAT.GT.0) THEN
        FORFIL=DCBFIL(1:LENGT1(DCBFIL)-3)//'F'
C
        WRITE(LFNERR,951) FORFIL
951     FORMAT(/,' ### SR WTCBFL: AUXILIARY DCB FILE SPECIFIC TO ',
     1    'CC2NONCC UTILITY CREATED',/,
     2    16X,'FILE NAME: ',A32,/)
C
        CALL OPNFIL(LFNLOC,FORFIL,'UNKNOWN','FORMATTED',
     1    ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FORFIL,'WTCBFL')
C
        DO I=1,40
          AUXVAL(I)=' -9.999d9'
        ENDDO
C
        DO ISAT=1,NUMSAT
          IPRN=DCBID1(ISAT)
          IF (IPRN.LE.40) THEN
            AUXVAL(IPRN)(9:9)='0'
            WRITE(AUXVAL(IPRN)(1:7),'(F7.3)') DCBVA1(1,ISAT)
          ELSE
            WRITE(LFNERR,952) IPRN
952         FORMAT(/,' ### SR WTCBFL: PRN ',I3,' SKIPPED',/)
          ENDIF
        ENDDO
C
        WRITE(LFNLOC,960) TITLE(1:60),(AUXVAL(I),I=1,40)
960     FORMAT('c',/,'c         ',A60,/,'c',/,
     1    '      data bias /',A9,4(',',A9),
     2    7(',',/,'     +           ',A9,4(',',A9)),'  /')
C
        CLOSE (UNIT=LFNLOC)
      ENDIF
C
      RETURN
C
      END SUBROUTINE

      END MODULE
