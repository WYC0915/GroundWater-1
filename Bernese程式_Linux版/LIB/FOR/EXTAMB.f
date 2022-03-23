      MODULE s_EXTAMB
      CONTAINS

C*
      SUBROUTINE EXTAMB(NFIL,FILNAM,FILTYP,AMBLOD,AMBID)
CC
CC NAME       :  EXTAMB
CC
CC PURPOSE    :  EXTRACT ESSENTIAL INFORMATION CONCERNING AMBIGUITY
CC               RESOLUTION (QIF,SIGMA) FROM GPSEST OUTPUT FILES
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
CC               AMBLOD : OUTPUT LEVELOF DETAIL               I*4
CC                        =-1: ALL
CC                        = 0: EACH&ALL
CC                        = 1: GPS
CC                        = 2: GLONASS
CC                        = 3: GALILEO
CC               AMBID  : #AR_ID                              CH*3
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S.SCHAER
CC
CC VERSION    :  4.0
CC
CC CREATED    :  16-JAN-96
CC
CC CHANGES    :  17-JAN-96 : SS: PRINT FINAL QIF STATISTICS
CC               18-JAN-96 : SS: PRINT NUMBER OF FILES
CC               04-APR-96 : MR: PUT LABEL 310 INTO DO LOOP
CC               05-JUN-96 : LM: DATA STATEMENT OUT OF ORDER
CC               11-JAN-97 : MR: ALLOW TO EXTRACT SIGMA STRATEGY
CC               15-JAN-97 : SS: BOTH OUTPUT FORMATS MODIFIED
CC               26-FEB-97 : SS: HANDLE MELBOURNE-WUEBBENA FILES
CC               30-OCT-97 : SS: SIGMA OF ZERO-DIFFERENCE OBS
CC               10-MAY-99 : SS: ALLOW MULTI-BASELINE FILES
CC               07-FEB-00 : HU: WRITE AMB FRAC PARTS ONLY IF FILE OPEN
CC               14-MAR-00 : SS: CONSIDER ADDNEQ2 OUTPUT FILES
CC               20-DEC-00 : SS: VARIABLE "TYPOUT" REMOVED
CC               06-AUG-01 : DS: EXTRACT RECTYP
CC               27-MAR-02 : RD: ERROR HANDLING FOR SIGMA=***
CC               04-JUL-02 : SS: ESTIMATE ORBIT ACCURACY
CC               17-SEP-02 : SS: HANDLE CASE WITH NO UNKNOWN AMB
CC               11-DEC-02 : CU: GPSEST V5.0 OUTPUT
CC               08-SEP-03 : HU: RECNAM CHR16 -> CHR20
CC               22-JAN-04 : HB: COMMENT write(*,*)-statement
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               29-AUG-07 : SS: HANDLE GLONASS AMBIGUITY RESOLUTION
CC               31-AUG-07 : MK/SS: HANDLE KINEMATIC CASE WITHOUT ERROR
CC               31-AUG-07 : HU/SS: ERROR HANDLING IMPROVED, USE SYMING
CC               15-FEB-08 : SL: EXTRACT BASELINE LENGTH W/O SLOPE TAB
CC               26-MAR-10 : RD: REPLACE MAXFIL BY NFIL
CC               24-NOV-10 : SL: STATION NAMES TO OUTPUT, M_BERN W/ ONLY
CC               17-FEB-11 : MM: NEW OPTIONS (AMBLOD, AMBID)
CC               10-JUL-12 : RD: USE SYMINVG INSTEAD OF SYMING
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1996     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnErr, lfn001, lfn002, lfnLoc
      USE m_global, ONLY: G_SVNSYS
      USE f_ikf
      USE s_opnfil
      USE s_opnerr
      USE s_solve
      USE s_syminvg
      USE s_fparse
      USE s_exitrc
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IAR   , IAR0  , IFIL  , IFREQ , IFREQ0,
     1          IL    , IMUL  , IOSTAT, IPGM  , IPRAMB, IPRQIF, IRC   ,
     2          IRCAMB, IRCQIF, NSING , ITER  , JL    , NAMB1 ,
     3          NBAST , NDOF  , NFIL  , NOBS  , IONE,
     4          ISVN1 , ISVN2 , NRED  , IRCODE,io1,io2, AMBLOD
C
      REAL*8    FPL3  , FPL5  , FPLR  , RMS1  , RMS1T , RMS2  ,
     2          RMS2T , RMSL3 , RMSL5 , RMSTOT, SLOPE , SLOPET, XFAC  ,
     3          XOBS  , XRAT
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*133 LINE
      CHARACTER*32  FILNAM(*),FILQIF,FILAMB,FILOUT
      CHARACTER*32  BASFIL,NODE,DEVICE,DIRNAM,EXT,VER
      CHARACTER*20  RECTYP(2)
      CHARACTER*8   BASNAM,WUBNAM
      CHARACTER*3   AMBSYS,SYSSTR,AMBID
      CHARACTER*4   staNam4(2)
      CHARACTER*7   AMBSTR
      CHARACTER*2   FRC
C
      REAL*8        ANOR(3),BNOR(2)
      REAL*8        XSOL(2),XOA(2,2)
      REAL*8        XOSTA(3,2),SLOPES(3),RMS1S(3),RMS2S(3)
      REAL*8        FPL3M(3),FPL3MT(3),FPL3R(3),FPL3RT(3)
      REAL*8        FPL5M(3),FPL5MT(3),FPL5R(3),FPL5RT(3)
      REAL*8        XSOLV(3),NSOLV(3),XSOLVT(3),NSOLVT(3)
      REAL*8        FPL5RA,FPL3RA,FPL5RAT,FPL3RAT
      REAL*8        NSOLA,NSOLAT,XSOLA,XSOLAT
C
      INTEGER*4     FILTYP(*),PARFLG(2)
      INTEGER*4     NAMB(3,2),NAMBT(3,2),NAMBA(2),NAMBAT(2)
      INTEGER*4     ISYS,IPART
      INTEGER*4     NBAS(3)
C
C
C RETURN, IF NEITHER QIF STATISTICS NOR FRACTIONAL PARTS REQUESTED
C ----------------------------------------------------------------
      CALL GTFLNA(0,'QIFOUT ',FILQIF,IRCQIF)
      CALL GTFLNA(0,'AMBOUT ',FILAMB,IRCAMB)
C
      IF (IRCQIF.EQ.1.AND.IRCAMB.EQ.1) GOTO 999
C
C INITIALIZE LINE IDENTIFIER
C --------------------------
      AMBSTR = '#AR'
      IF (LEN_TRIM(AMBID) > 0) AMBSTR = TRIM(AMBSTR)//"_"//TRIM(AMBID)
C
C INITIALIZE FLAGS FOR HEADER PRINTING
C ------------------------------------
      IPRQIF=1
      IPRAMB=1
C
      IAR0=0
      IFREQ0=0
      IFREQ=0
C
C OPEN SUMMARY FILES
C ------------------
      IF (IRCQIF.EQ.0) THEN
        CALL OPNFIL(LFN001,FILQIF,'UNKNOWN','FORMATTED',
     1    ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFN001,IOSTAT,FILQIF,'EXTAMB')
      ENDIF
      IF (IRCAMB.EQ.0) THEN
        CALL OPNFIL(LFN002,FILAMB,'UNKNOWN','FORMATTED',
     1    ' ',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFN002,IOSTAT,FILAMB,'EXTAMB')
      ENDIF
C
C LOOP OVER ALL GPSEST OUTPUT FILES
C ---------------------------------
      FPL5MT=0.D0
      FPL5RT=0.D0
      FPL3MT=0.D0
      FPL3RT=0.D0
C
      NAMBT=0
      NAMBAT=0
C
      RMS1T=0.D0
      RMS2T=0.D0
      SLOPET=0.D0
      RMS1S=0.D0
      RMS2S=0.D0
      SLOPES=0.d0
C
      NBAST=0
      NBAS=0
C
C INITIALIZE VARIABLES CONCERNING ORBIT ACCURACY ESTIMATION
      DO I=1,3
        ANOR(I)=0.D0
      ENDDO
      DO I=1,2
        BNOR(I)=0.D0
      ENDDO
      RMSTOT=0.D0
      NOBS=0
C
      DO 110 IFIL=1,NFIL
        AMBSYS(1:3)='   '
        NAMB=0
        NAMBA=0
C
C SKIP BAD OUTPUT FILE
        IPGM=FILTYP(IFIL)
        IF (IPGM.NE.1.AND.IPGM.NE.11) GOTO 110
C
C OPEN GPSEST OUTPUT FILE
        FILOUT=FILNAM(IFIL)
        CALL OPNFIL(LFNLOC,FILOUT,'OLD','FORMATTED',
     1    'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILOUT,'EXTAMB')
C
C EXTRACT BASELINE NAME
        IRCODE=1
        DO IL=1,100000
          READ(LFNLOC,'(A)',END=310) LINE
          IF (LINE(1:30).EQ.' FILE  OBSERVATION FILE HEADER') GOTO 410
        ENDDO
410     READ(LFNLOC,'(//,7X,A32,44X,A20,2X,A20)') BASFIL,
     1       RECTYP(1),RECTYP(2)
        CALL FPARSE(0,BASFIL,NODE,DEVICE,DIRNAM,BASNAM,EXT,VER,IRC)
        IRCODE=2
        IF (IRC.NE.0) GOTO 310
C
C CHECK WHETHER FURTHER BASELINES ARE LISTED
        READ(LFNLOC,'(A)') LINE
C
C HANDLE MELBOURNE-WUEBBENA OBSERVATION FILES
        CALL FPARSE(0,LINE,NODE,DEVICE,DIRNAM,WUBNAM,EXT,VER,IRC)
        IF (BASNAM.EQ.WUBNAM) READ(LFNLOC,'(A)') LINE
C
C CHECK WHETHER MULTI-BASELINE FILE IS PROCESSED
        IF (LINE.NE.' ') THEN
          WRITE(LFNERR,940) FILOUT
940       FORMAT(/,' ### SR EXTAMB: MORE THAN ONE BASELINE FOUND',
     1      /,16X,'FILE NAME: ',A,/)
          IMUL=1
          CALL FPARSE(0,FILOUT,NODE,DEVICE,DIRNAM,BASNAM,EXT,VER,IRC)
        ELSE
          IMUL=0
        ENDIF
C
C LOOK FOR AR STRATEGY
        IRCODE=3
        DO IL=1,100000
          READ(LFNLOC,'(A)',END=310) LINE
          IF (LINE(1:31).EQ.
     1      ' AMBIGUITY RESOLUTION STRATEGY:') GOTO 210
        ENDDO
210     CONTINUE
C
        READ(LFNLOC,'(//,A)') LINE
        IF (LINE(1:22).EQ.' QUASI-IONOSPHERE-FREE') THEN
          IAR=4
        ELSEIF (LINE(1:22).EQ.' SIGMA-DEPENDENT AMBIG') THEN
          IAR=3
        ELSE
          IRCODE=4
          GOTO 310
        ENDIF
C
        IF (IAR0.EQ.0) THEN
          IAR0=IAR
        ELSEIF (IAR.NE.IAR0) THEN
          WRITE(LFNERR,980)
980       FORMAT(/,' *** SR EXTAMB: MIXTURE OF AMBIGUITY RESOLUTION ',
     1      'STRATEGIES NOT ALLOWED',/)
          CALL EXITRC(2)
        ENDIF
C
C EXTRACT BASELINE LENGTH
        IRCODE=8
        DO IL=1,100000

          READ(LFNLOC,1003,END=310) LINE
1003      FORMAT(A133)
          IF(LINE(1:33).EQ.' NUM  STATION NAME     OBS E/F/C '.OR.
     1       LINE(1:33).EQ.' num  Station name     obs e/f/h ')THEN
            READ(LFNLOC,'(/,6X,A4,23X,F15.4,1X,F15.4,1X,F15.4)',
     1        iostat=io1) staNam4(1),XOSTA(1,1),XOSTA(2,1),XOSTA(3,1)
            READ(LFNLOC,'(6X,A4,23X,F15.4,1X,F15.4,1X,F15.4)',
     1        iostat=io2) staNam4(2),XOSTA(1,2),XOSTA(2,2),XOSTA(3,2)
            SLOPE=DSQRT((XOSTA(1,2)-XOSTA(1,1))**2
     1                 +(XOSTA(2,2)-XOSTA(2,1))**2
     2                 +(XOSTA(3,2)-XOSTA(3,1))**2)
CC            WRITE(LFNERR,1099) XOSTA(1,1)
CC1099        FORMAT(/,' EXTAMB ',F15.4,/)
CC
CC          IF (LINE(8:12).EQ.'| O |') THEN
CC            READ(LINE(13:24),'(F12.4)') SLOPE
CC
            IF (IMUL.EQ.1) SLOPE=0.D0
            SLOPE=SLOPE/1.D3
            SLOPET=SLOPET+SLOPE
            NBAST=NBAST+1
            GOTO 240
          ELSEIF (LINE.EQ.' AMBIGUITY RESOLUTION:') THEN
            NBAST=NBAST+1
            SLOPE=0D0
            GOTO 240
          ENDIF
        ENDDO
240     CONTINUE
C
C EXTRACT NUMBER OF AMBIGUITIES (BEFORE AR)
        IRCODE=5
        DO IL=1,100000
          READ(LFNLOC,'(A)',END=310) LINE
          IF (LINE(1:15).EQ.' PARAMETER TYPE') THEN
            IRCODE=6
            DO JL=1,100000
              READ(LFNLOC,'(A)',END=310) LINE
              IF (LINE(1:12).EQ.' AMBIGUITIES') THEN
                READ(LINE(51:59),'(I9)') NAMB1
                GOTO 220
              ENDIF
            ENDDO
          ENDIF
        ENDDO
220     CONTINUE
C
C EXTRACT RMS OF UNIT WEIGHT (BEFORE AR)
        IRCODE=7
        DO IL=1,100000
          READ(LFNLOC,'(A)',END=310) LINE
          IF (LINE(1:37).EQ.
     1      ' A POSTERIORI SIGMA OF UNIT WEIGHT  :') THEN
            READ(LINE(39:47),'(F9.4)',ERR=225) RMS1
            RMS1=RMS1*1.D3
            RMS1T=RMS1T+RMS1**2
            GOTO 230
225         RMS1=1D6
            GOTO 230
          ENDIF
        ENDDO
230     CONTINUE
C
C EXTRACT FRACTIONAL PARTS AND NUMBER OF AMBIGUITIES (AFTER AR)
        FPL5M=0.D0
        FPL5R=0.D0
        FPL3M=0.D0
        FPL3R=0.D0
C
C HANDLE CASE WITH NO UNKNOWN AMBIGUITIES
        IF (NAMB1.EQ.0) THEN
          RMS2=RMS1
          RMS2T=RMS2T+RMS2**2
C
          WRITE(LFNERR,925) FILOUT
925       FORMAT(/,' ### SR EXTAMB: NO UNKNOWN AMBIGUITIES FOUND',
     1      /,16X,'FILE NAME: ',A,/)
C
          GOTO 270
        ENDIF
C
        IRCODE=9
        IPART=1
        DO 250 IL=1,100000
          READ(LFNLOC,'(A)',END=310) LINE
          IF (LINE(1:34).EQ.' AMBI  FILE SAT. EPOCH FRQ WLF CLU') THEN
            READ(LFNLOC,'(/)',END=310)
            IRCODE=10
            DO JL=1,100000
              READ(LFNLOC,'(A)',END=310) LINE
              IF (LINE.EQ.' ') THEN
                IPART=2
                GOTO 250
              ENDIF
CC TO COUNT ONLY AMBIGUITIES FOR WHICH L5 WAS RESOLVED USE:
CC              READ(LINE,'(10X,I5,7X,I3,52X,A2)') ISVN1,IFREQ,FRC
CC              IF (FRC.EQ.'  ') CYCLE
              READ(LINE,'(10X,I5,7X,I3,51X,A2)') ISVN1,IFREQ,FRC
              IF (FRC.EQ.'. ') CYCLE
C
              ISYS=ISVN1/100+1
              IF (ISYS-1.GT.2) THEN
                WRITE(LFNERR,'(/,A,/,2A,/)')
     1               " *** SR EXTAMB: UNSUPPORTED GNSS FOUND",
     2               "                GNSS: ",G_SVNSYS(ISYS-1)
                CALL EXITRC(2)
              ENDIF
              AMBSYS(ISYS:ISYS)=G_SVNSYS(ISYS-1)
              NAMB(ISYS,IPART)=NAMB(ISYS,IPART)+1
C
              IF (IFREQ0.EQ.0) THEN
                IF (IFREQ.NE.0) IFREQ0=IFREQ
CC              ELSEIF (IFREQ.NE.IFREQ0 .AND. IFREQ.NE.3 .AND.
CC     1                IAR.NE.4 .AND. IFREQ0.NE.-1) THEN
              ELSEIF (IFREQ.NE.IFREQ0 .AND. IFREQ.NE.3 .AND.
     1                IFREQ0.NE.-1) THEN
                IF (IAR.NE.4) WRITE(LFNERR,985)
985             FORMAT(/,' ### SR EXTAMB: MIXTURE OF FREQUENCIES ',
     1                   'FOUND',/)
                IFREQ0=-1
              ENDIF
            ENDDO
          ELSEIF (LINE(1:32).EQ.' AMBIGUITY RESOLUTION ITERATION:') THEN
            IRCODE=11
            READ(LINE(33:38),'(I6)') ITER
            IF (IAR.EQ.4) THEN
              READ(LFNLOC,'(/////)',END=310)
            ELSEIF (IAR.EQ.3) THEN
              READ(LFNLOC,'(////)',END=310)
            ELSE
              GOTO 250
            ENDIF
            IRCODE=12
            DO JL=1,100000
              READ(LFNLOC,'(A)',END=310) LINE
              IF (LINE.EQ.' ') GOTO 250
              IF (IAR.EQ.4) THEN
                READ(LINE(56:89),'(3F8.3,2I5)')
     1            FPL5,FPL3,RMSL3,ISVN1,ISVN2
C
C "RMSL5" NOT GIVEN
                RMSL5=0.D0
              ELSEIF (IAR.EQ.3) THEN
                READ(LINE,'(5X,I4,35X,F16.3,F8.3,2I5)')
     1            IFREQ,FPL5,RMSL5,ISVN1,ISVN2
                FPL5=FPL5-DNINT(FPL5)
                FPL3=0.D0
                RMSL3=0.D0
              ELSE
                GOTO 250
              ENDIF
C
C WRITE HEADER LINES OF AMBIGUITY FRACTIONAL LISTING
              IF (IRCAMB.EQ.0.AND.IPRAMB.EQ.1) THEN
                WRITE(LFN002,920) IAR
920             FORMAT(' Ambiguity Resolution Strategy ',I1,/,
     1            1X,31('-'),//,
     2            ' File     Sta1 Sta2    Length  Freq.  Amb.Frac.   ',
     3            'Amb.RMS Iter. Sat1 Sat2',/,
     4            '                        (km)           (Cycles)   ',
     5            '(Cycles)',/,1X,72('-'))
                IPRAMB=0
              ENDIF
C
C WRITE FRACTIONAL PARTS
              IF (IRCAMB.EQ.0) THEN
                IF (IAR.EQ.4) THEN
                  WRITE(LFN002,950) BASNAM,staNam4(1),staNam4(2),SLOPE,
     1              5,FPL5,RMSL5,ITER,ISVN1,ISVN2
                  WRITE(LFN002,950) BASNAM,staNam4(1),staNam4(2),SLOPE,
     1              3,FPL3,RMSL3,ITER,ISVN1,ISVN2
                ELSEIF (IAR.EQ.3) THEN
                  WRITE(LFN002,950) BASNAM,staNam4(1),staNam4(2),SLOPE,
     1              IFREQ,FPL5,RMSL5,ITER,ISVN1,ISVN2
                ENDIF
950             FORMAT(1X,A8,1X,A4,1X,A4,1X,F10.3,I5,2F11.3,I6,2I5)
              ENDIF
CC C
CC C UPDATE STATISTICS
CC               FPL5=DABS(FPL5)
CC               FPL3=DABS(FPL3)
CC C
CC               IF (FPL5.GT.FPL5M) FPL5M=FPL5
CC               IF (FPL3.GT.FPL3M) FPL3M=FPL3
CC               FPL5R=FPL5R+FPL5**2
CC               FPL3R=FPL3R+FPL3**2
CC C
CC               IF (FPL5.GT.FPL5MT) FPL5MT=FPL5
CC               IF (FPL3.GT.FPL3MT) FPL3MT=FPL3
CC               FPL5RT=FPL5RT+FPL5**2
CC               FPL3RT=FPL3RT+FPL3**2
C
C UPDATE STATISTICS FOR EACH GNSS
              ISYS=ISVN1/100+1
              FPL5=DABS(FPL5)
              FPL3=DABS(FPL3)
C
              IF (FPL5.GT.FPL5M(ISYS)) FPL5M(ISYS)=FPL5
              IF (FPL3.GT.FPL3M(ISYS)) FPL3M(ISYS)=FPL3
              FPL5R(ISYS)=FPL5R(ISYS)+FPL5**2
              FPL3R(ISYS)=FPL3R(ISYS)+FPL3**2
C
              IF (FPL5.GT.FPL5MT(ISYS)) FPL5MT(ISYS)=FPL5
              IF (FPL3.GT.FPL3MT(ISYS)) FPL3MT(ISYS)=FPL3
              FPL5RT(ISYS)=FPL5RT(ISYS)+FPL5**2
              FPL3RT(ISYS)=FPL3RT(ISYS)+FPL3**2
C
            ENDDO
          ELSEIF (LINE(1:15).EQ.' PARAMETER TYPE') THEN
            IRCODE=13
            GOTO 260
          ENDIF
250     CONTINUE
260     CONTINUE
C
C EXTRACT RMS OF UNIT WEIGHT (AFTER AR)
        IRCODE=14
        DO IL=1,100000
          READ(LFNLOC,'(A)',END=310) LINE
          IF (LINE(1:37).EQ.
     1      ' A POSTERIORI SIGMA OF UNIT WEIGHT  :') THEN
            READ(LINE(39:47),'(F9.4)',ERR=265) RMS2
            RMS2=RMS2*1.D3
            RMS2T=RMS2T+RMS2**2
            GOTO 270
265         RMS2=1D6
            GOTO 270
          ENDIF
        ENDDO
270     CONTINUE
C
C REDUCE NUMBER OF AMBIGUITIES IN CASE OF GLONASS (OR GNSS)
        NRED=1
        IF (IFREQ0.EQ.-1) NRED=2
        IF (NAMB(2,1).EQ.0.AND.NAMB(3,1).EQ.0) NRED=0
        DO ISYS=1,3
          IF (NAMB(ISYS,2).GT.0) NAMB(ISYS,:)=NAMB(ISYS,:)-NRED
        ENDDO
C
C SOME STATISTICS
        FPL5RA=SUM(FPL5R)
        FPL3RA=SUM(FPL3R)
        NAMBA(1)=SUM(NAMB(:,1))
        NAMBA(2)=SUM(NAMB(:,2))
        NSOLA=NAMBA(1)-NAMBA(2)
        IF (NAMBA(1).NE.0) THEN
          XSOLA=100.D0*NSOLA/NAMBA(1)
        ELSE
          XSOLA=0.D0
        ENDIF
        IF (NSOLA.NE.0) THEN
          FPL5RA=DSQRT(FPL5RA/NSOLA)
          FPL3RA=DSQRT(FPL3RA/NSOLA)
        ELSE
          FPL5RA=0.D0
          FPL3RA=0.D0
        ENDIF
C
        DO ISYS=1,3
          NAMBT(ISYS,1:2)=NAMBT(ISYS,1:2)+NAMB(ISYS,1:2)
          NSOLV(ISYS)=NAMB(ISYS,1)-NAMB(ISYS,2)
          IF (NAMB(ISYS,1).NE.0) THEN
            XSOLV(ISYS)=100.D0*NSOLV(ISYS)/NAMB(ISYS,1)
          ELSE
            XSOLV(ISYS)=0.D0
          ENDIF
          IF (NSOLV(ISYS).NE.0) THEN
            FPL5R(ISYS)=DSQRT(FPL5R(ISYS)/NSOLV(ISYS))
            FPL3R(ISYS)=DSQRT(FPL3R(ISYS)/NSOLV(ISYS))
          ELSE
            FPL5R(ISYS)=0.D0
            FPL3R(ISYS)=0.D0
          ENDIF
        ENDDO
C
C WRITE HEADER LINES OF QIF STATISTICS
        IF (IFREQ0.EQ.-1) IFREQ=1
        IF (IRCQIF.EQ.0.AND.IPRQIF.EQ.1) THEN
          IF (IAR.EQ.4.) THEN
            WRITE(LFN001,910)
910         FORMAT(
     1        ' File     Sta1 Sta2    Length     Before     After    ',
     1        'Res  Sys  Max/RMS L5    Max/RMS L3    ',
     2        'Receiver 1           Receiver 2',/,
     2        '                        (km)    #Amb (mm)  #Amb (mm)  ',
     2        '(%)       (L5 Cycles)   (L3 Cycles)',/,
     3        1X,132('-'))
          ELSEIF (IAR.EQ.3) THEN
            WRITE(LFN001,915) IFREQ,IFREQ
915         FORMAT(
     1        ' File     Sta1 Sta2    Length     Before     After    ',
     1        'Res  Sys  Max/RMS L',I1,'    ',
     1        'Receiver 1           Receiver 2',/,
     2        '                        (km)    #Amb (mm)  #Amb (mm)  ',
     2        '(%)       (L',I1,' Cycles)',/,
     3        1X,118('-'))
          ENDIF
          IPRQIF=0
        ENDIF
C
C WRITE GNSS-SPECIFIC STATISTICS
        IONE=0
        DO ISYS=1,3
          IF (NAMB(ISYS,1).GT.0) THEN
            NBAS(ISYS)=NBAS(ISYS)+1
            SLOPES(ISYS)=SLOPES(ISYS)+SLOPE
            RMS1S(ISYS)=RMS1S(ISYS)+RMS1**2
            RMS2S(ISYS)=RMS2S(ISYS)+RMS2**2
            IONE=IONE+1
          ENDIF
        ENDDO
C
        DO ISYS=1,3
          IF (AMBLOD.NE.0.AND.AMBLOD.NE.ISYS) CYCLE
          IF (NAMB(ISYS,1).EQ.0) CYCLE
          IF (AMBLOD.EQ.0.AND.IONE.EQ.1) CYCLE
C
          SYSSTR='   '
          SYSSTR(ISYS:ISYS)=AMBSYS(ISYS:ISYS)
C
          IF (IAR.EQ.4) THEN
            WRITE(LFN001,960)
     1        BASNAM,staNam4(1),staNam4(2),SLOPE,NAMB(ISYS,1),RMS1,
     1        NAMB(ISYS,2),RMS2,XSOLV(ISYS),SYSSTR,FPL5M(ISYS),
     1        FPL5R(ISYS),FPL3M(ISYS),FPL3R(ISYS),RECTYP(1),RECTYP(2),
     1        AMBSTR
960         FORMAT(1X,A8,1X,A4,1X,A4,1X,F10.3,1X,2(I5,1X,F4.1,1X),
     1             F5.1,1X,A3,1X,2(2(F6.3,1X)),1X,A20,1X,A20,2X,A7)
          ELSEIF (IAR.EQ.3) THEN
            WRITE(LFN001,965)
     1        BASNAM,staNam4(1),staNam4(2),SLOPE,NAMB(ISYS,1),RMS1,
     1        NAMB(ISYS,2),RMS2,XSOLV(ISYS),SYSSTR,FPL5M(ISYS),
     1        FPL5R(ISYS),RECTYP(1),RECTYP(2),AMBSTR
965         FORMAT(1X,A8,1X,A4,1X,A4,1X,F10.3,1X,2(I5,1X,F4.1,1X),
     1             F5.1,1X,A3,1X,2(F6.3,1X),1X,A20,1X,A20,2X,A7)
          ENDIF
        ENDDO
C
C WRITE TOTAL STATISTICS
        IF (AMBLOD.EQ.0.OR.AMBLOD.EQ.-1) THEN
C
          IF (IAR.EQ.4) THEN
            WRITE(LFN001,960)
     1        BASNAM,staNam4(1),staNam4(2),SLOPE,NAMBA(1),RMS1,
     1        NAMBA(2),RMS2,XSOLA,AMBSYS,MAXVAL(FPL5M,1),
     1        FPL5RA,MAXVAL(FPL3M,1),FPL3RA,RECTYP(1),RECTYP(2),AMBSTR
          ELSEIF (IAR.EQ.3) THEN
            WRITE(LFN001,965)
     1        BASNAM,staNam4(1),staNam4(2),SLOPE,NAMBA(1),RMS1,
     1        NAMBA(2),RMS2,XSOLA,AMBSYS,MAXVAL(FPL5M,1),
     1        FPL5RA,RECTYP(1),RECTYP(2),AMBSTR
          ENDIF
        ENDIF
C
C VALUE FOR ORBIT ACCURACY
        IF (IAR.EQ.4) THEN
          FPLR=FPL3RA
        ELSEIF (IAR.EQ.3.AND.IFREQ.EQ.3) THEN
          FPLR=FPL5RA
        ELSE
          FPLR=0.D0
        ENDIF
C
        IF (FPLR.GT.0.D0) THEN
          NOBS=NOBS+1
          XOBS=(107.D0*FPLR)**2
          XFAC=(SLOPE/20000.D0)**2
          ANOR(1)=ANOR(1)+1.D0
          ANOR(2)=ANOR(2)+XFAC
          ANOR(3)=ANOR(3)+XFAC**2
          BNOR(1)=BNOR(1)+XOBS
          BNOR(2)=BNOR(2)+XFAC*XOBS
          RMSTOT=RMSTOT+XOBS**2
        ENDIF
C
C NEXT FILE
        CLOSE(LFNLOC)
        GOTO 110
C
310     WRITE(LFNERR,930) FILOUT,IRCODE
930     FORMAT(/,' ### SR EXTAMB: INFORMATION NOT FOUND',
     1    /,16X,'FILE NAME: ',A,/,
     2    /,16X,'POSITION : ',I4,/)
        CLOSE(LFNLOC)
C
110   CONTINUE
C
C OVERALL STATISTICS
      FPL5RAT=SUM(FPL5RT)
      FPL3RAT=SUM(FPL3RT)
      NAMBAT(1)=SUM(NAMBT(:,1))
      NAMBAT(2)=SUM(NAMBT(:,2))
      NSOLAT=NAMBAT(1)-NAMBAT(2)
      IF (NAMBAT(1).NE.0) THEN
        XSOLAT=100.D0*NSOLAT/NAMBAT(1)
      ELSE
        XSOLAT=0.D0
      ENDIF
      IF (NSOLAT.NE.0) THEN
        FPL5RAT=DSQRT(FPL5RAT/NSOLAT)
        FPL3RAT=DSQRT(FPL3RAT/NSOLAT)
      ELSE
        FPL5RAT=0.D0
        FPL3RAT=0.D0
      ENDIF
C
      DO ISYS=1,3
        NSOLVT(ISYS)=NAMBT(ISYS,1)-NAMBT(ISYS,2)
        IF (NAMBT(ISYS,1).NE.0) THEN
          XSOLVT(ISYS)=100.D0*NSOLVT(ISYS)/NAMBT(ISYS,1)
        ELSE
          XSOLVT(ISYS)=0.D0
        ENDIF
        IF (NSOLVT(ISYS).NE.0) THEN
          FPL5RT(ISYS)=DSQRT(FPL5RT(ISYS)/NSOLVT(ISYS))
          FPL3RT(ISYS)=DSQRT(FPL3RT(ISYS)/NSOLVT(ISYS))
        ELSE
          FPL5RT(ISYS)=0.D0
          FPL3RT(ISYS)=0.D0
        ENDIF
        IF (NBAS(ISYS).NE.0) THEN
          SLOPES(ISYS)=SLOPES(ISYS)/NBAS(ISYS)
          RMS1S(ISYS)=DSQRT(RMS1S(ISYS)/NBAS(ISYS))
          RMS2S(ISYS)=DSQRT(RMS2S(ISYS)/NBAS(ISYS))
        ELSE
          SLOPES(ISYS)=0.D0
          RMS1S(ISYS)=0.D0
          RMS2S(ISYS)=0.D0
        ENDIF
      ENDDO
C
      IF (NBAST.NE.0) THEN
        SLOPET=SLOPET/NBAST
        RMS1T=DSQRT(RMS1T/NBAST)
        RMS2T=DSQRT(RMS2T/NBAST)
      ELSE
        SLOPET=0.D0
        RMS1T=0.D0
        RMS2T=0.D0
      ENDIF
C
C PRINT FINAL STATISTIC
C ---------------------
      IF (IRCQIF.EQ.0) THEN
C
C GNSS-SPECIFIC STATISTIC
        IONE=0
        DO ISYS=1,3
          IF (NAMBT(ISYS,1).GT.0) IONE=IONE+1
        ENDDO
C
        IF (IAR.EQ.4) THEN
          WRITE(LFN001,'(1X,131("-"))')
        ELSE
          WRITE(LFN001,'(1X,117("-"))')
        ENDIF
        AMBSYS='   '
        DO ISYS=1,3
          IF (NAMBT(ISYS,1).EQ.0) CYCLE
          AMBSYS(ISYS:ISYS)=G_SVNSYS(ISYS-1)
          IF (AMBLOD.NE.0.AND.AMBLOD.NE.ISYS) CYCLE
          IF (AMBLOD.EQ.0.AND.IONE.EQ.1) CYCLE
C
          SYSSTR='   '
          SYSSTR(ISYS:ISYS)=G_SVNSYS(ISYS-1)
C
          IF (IAR.EQ.4) THEN
            WRITE(LFN001,970)
     1        NBAS(ISYS),SLOPES(ISYS),NAMBT(ISYS,1),RMS1S(ISYS),
     1        NAMBT(ISYS,2),RMS2S(ISYS),XSOLVT(ISYS),SYSSTR,
     1        FPL5MT(ISYS),FPL5RT(ISYS),FPL3MT(ISYS),FPL3RT(ISYS),AMBSTR
970         FORMAT(1X,'Tot:',I4,5X,5X,1X,F10.3,1X,
     1        2(I5,1X,F4.1,1X),F5.1,1X,A3,1X,2(2(F6.3,1X)),44X,A7)
            ELSEIF (IAR.EQ.3) THEN
              WRITE(LFN001,975)
     1          NBAS(ISYS),SLOPES(ISYS),NAMBT(ISYS,1),RMS1S(ISYS),
     1          NAMBT(ISYS,2),RMS2S(ISYS),XSOLVT(ISYS),SYSSTR,
     1          FPL5MT(ISYS),FPL5RT(ISYS),AMBSTR
975         FORMAT(1X,'Tot:',I4,5X,5X,1X,F10.3,1X,
     1        2(I5,1X,F4.1,1X),F5.1,1X,A3,1X,2(F6.3,1X),44X,A7)
          ENDIF
C
        ENDDO
C
C WRITE TOTAL STATISTICS
        IF (AMBLOD.EQ.0.OR.AMBLOD.EQ.-1) THEN
          IF (IAR.EQ.4) THEN
            WRITE(LFN001,970)
     1        NBAST,SLOPET,NAMBAT(1),RMS1T,NAMBAT(2),RMS2T,
     1        XSOLAT,AMBSYS,MAXVAL(FPL5MT,1),FPL5RAT,MAXVAL(FPL3MT,1),
     1        FPL3RAT,AMBSTR
          ELSEIF (IAR.EQ.3) THEN
            WRITE(LFN001,975)
     1        NBAST,SLOPET,NAMBAT(1),RMS1T,NAMBAT(2),RMS2T,
     1        XSOLAT,AMBSYS,MAXVAL(FPL5MT),FPL5RAT,AMBSTR
          ENDIF
        ENDIF
        WRITE(LFN001,'(/)')
C
C PRINT ESTIMATED ORBIT ACCURACY
        NDOF=NOBS-2
        IF (NDOF.GT.0) THEN
          CALL SYMINVG(2,ANOR,0,NSING,PARFLG)
          CALL SOLVE(2,ANOR,BNOR,XSOL)
          DO I=1,2
            RMSTOT=RMSTOT-BNOR(I)*XSOL(I)
          ENDDO
          RMSTOT=DSQRT(RMSTOT/NDOF)
          DO I=1,2
            IF (XSOL(I).GT.0.D0) THEN
              XOA(1,I)=DSQRT(XSOL(I))
              XOA(2,I)=RMSTOT*DSQRT(ANOR(IKF(I,I)))/(2.D0*XOA(1,I))
            ELSE
              XOA(1,I)=0.D0
              XOA(2,I)=0.D0
            ENDIF
          ENDDO
          IF (XOA(2,2).GT.0.D0) THEN
            XRAT=XOA(1,2)/XOA(2,2)
          ELSE
            XRAT=0.D0
          ENDIF
          IF (XRAT.GT.5.D0) THEN
            WRITE(LFN001,935) (XOA(I,2),I=1,2),(XOA(I,1),I=1,2),
     1        XOA(1,1)/107.D0
935         FORMAT(' Estimated Orbit Accuracy: ',F6.1,'+-',F6.1,' mm',
     1        /,' Basic Noise of L3 Amb   : ',F6.1,'+-',F6.1,' mm / ',
     2        F6.3,' L3 Cycles',/)
          ELSE
            WRITE(LFNERR,955)
955         FORMAT(/,' ### SR EXTAMB: ORBIT ACCURACY NOT ESTIMATED',/)
          ENDIF
        ENDIF
C
        CLOSE (LFN001)
      ENDIF
      IF (IRCAMB.EQ.0) THEN
        WRITE(LFN002,*)
        CLOSE (LFN002)
      ENDIF
C
999   RETURN
      END SUBROUTINE

      END MODULE
