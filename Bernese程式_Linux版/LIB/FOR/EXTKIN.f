      MODULE s_EXTKIN
      CONTAINS

C*
      SUBROUTINE EXTKIN(NFIL,FILNAM,FILTYP)
CC
CC NAME       :  EXTKIN
CC
CC PURPOSE    :  EXTRACT KINEMATIC COORDINATE ESTIMATES FOR IGS COMBINATION
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
CC REMARKS    :  **** CURRENTLY ONLY FOR 1 GPSEST OUTPUT FILE ****
CC
CC AUTHOR     :  T.A. SPRINGER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  21-OCT-98
CC
CC CHANGES    :  21-OCT-98 : TS: CORRECT PROBLEMS WITH "****" ENTRIES
CC               14-MAR-00 : SS: CONSIDER ADDNEQ2 OUTPUT FILES
CC               18-APR-01 : SS: ACCEPT ALSO CODE-ONLY SOLUTIONS
CC               26-APR-01 : MM: WARNING IF TWO OR MORE KINEMATIC STATIONS
CC                               ARE IN GPSEST OUTPUT
CC               30-JAN-02 : RD: BUGFIX IF PARAMETER LIST IS PRINTED
CC               31-JAN-02 : RD: TWO MORE LINES TO SKIP (NEW PGM OUTPUT)
CC               13-NOV-02 : RD: READ REALLY MAXEPO EPOCHS...
CC               11-DEC-02 : CU: GPSEST V5.0 OUTPUT
CC               27-JUN-03 : RD: ADAPT TO PRIKIN MODIFICATIONS
CC                               ALLOW CORRECTION ZERO FOR ONE OR TWO COMPONENTS
CC               21-AUG-03 : RD: MAXEPO IS ALLOCATABLE NOW
CC               06-MAY-04 : HB: IRECT-LOOP FROM 1 TO MMXEPO
CC               07-MAY-04 : HB: IRECT-LOOP FROM 1 TO MMXEPO + 10
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               19-JUL-10 : SL: TAB CHARACTERS REMOVED
CC               14-JAN-12 : RD: EXTENT THE FORMAT FOR PARAMETERS/OBSERVATIONS
CC               30-JUL-12 : RD: SR STATIS WITH OPTIONAL ARGUMENTS
CC               30-JUL-12 : RD: USE M_BERN WITH ONLY
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1998     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE M_BERN,  ONLY: i4b, r8b, lfnerr, lfnloc, lfn001
C
      USE s_opnfil
      USE s_alcerr
      USE s_sjustl
      USE s_opnerr
      USE s_statis
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I      , IAC    , IEPO   , IFIL   , IL1O   , IL2O   ,
     1          IOSTAT , IP1O   , IP2O   , IPGM   , IRCKIN , IREC   ,
     2          IRECT  , MAXEPO , MAXFIL , MMXEPO , NAMB   , NCLKSTA,
     3          NCODEL3, NEPO   , NFIL   , NKINCRD, NOBSTOT, NPARTOT,
     4          NPHASL3, NUMFIX
C
      REAL*8    DEMEA  , DEMED  , DERMS  , DESIG  , DESTA  , DH     ,
     1          DLENG  , DLON   , DNMEA  , DNMED  , DNRMS  , DNSIG  ,
     2          DPHI   , DUMEA  , DUMED  , DURMS  , DUSIG  , DXSTA  ,
     3          HHO    , RDESTA , RDLENG , RDXSTA , RL3O   , RP3O   ,
     4          SIGHGT , SIGLON , SIGMA  , SIGPHI , SIGTOL
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MAXFIL=1,MAXEPO=288)
C
      CHARACTER*255 LINE
      CHARACTER*80  TITLE(MAXFIL)
      CHARACTER*32  FILNAM(*),FILKIN,FILOUT
      CHARACTER*16  STNAME,STNAM2
      CHARACTER*1   VPO,VLO
C
C      REAL*8        DN(MAXEPO),DE(MAXEPO),DU(MAXEPO)
      REAL(R8B), DIMENSION(:),ALLOCATABLE :: DN
      REAL(R8B), DIMENSION(:),ALLOCATABLE :: DE
      REAL(R8B), DIMENSION(:),ALLOCATABLE :: DU
      REAL*8        XYZAPR(3),XYZEST(3),DIFXYZ(3)
C
      INTEGER*4     FILTYP(*)
      INTEGER(I4B), DIMENSION(:),ALLOCATABLE :: NE
C      INTEGER*4     NE(MAXEPO)
C
      COMMON/CEXTKN/DXSTA,DLENG,DESTA,RDXSTA,RDLENG,RDESTA
C

      MMXEPO = MAXEPO
C
C RETURN, IF KINEMATIC SUMMARY FILE NOT REQUESTED
C -----------------------------------------------
      CALL GTFLNA(0,'KINOUT ',FILKIN,IRCKIN)
      IF (IRCKIN.EQ.1) GOTO 999
C
C OPEN KINEMATIC SUMMARY FILE
C ---------------------------
      CALL OPNFIL(LFN001,FILKIN,'UNKNOWN','FORMATTED',
     1  ' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFN001,IOSTAT,FILKIN,'EXTKIN')
C
C CHECK MAXIMUM NUMBER OF FILES
C -----------------------------
      IF (NFIL.GT.MAXFIL) THEN
        WRITE(LFNERR,901) NFIL,MAXFIL
901     FORMAT(/,' ### SR EXTKIN: TOO MANY GPSEST OUTPUT FILES',/,
     1       16X,'NUMBER OF FILES        :',I4,/,
     2       16X,'MAXIMUM NUMBER ALLOWED :',I4,/,
     3       16X,'USING LAST FILE ONLY    ')
CCCC        CALL EXITRC(2)
      ENDIF
C
C INITIALIZE SOME VARIABLES
C -------------------------
      NAMB=0
      NKINCRD=0
      NCLKSTA=0
      NPARTOT=0
C
      NPHASL3=0
      NCODEL3=0
      NOBSTOT=0
C
      SIGMA=0D0
C
      NEPO=0
C
      SIGTOL=0.1D0
C
      DO I=1,3
        XYZAPR(I)=0.D0
        XYZEST(I)=0.D0
        DIFXYZ(I)=0.D0
      ENDDO
C
      ALLOCATE(DN(MAXEPO),STAT=IAC)
      CALL ALCERR(IAC,'DN',(/MAXEPO/),'EXTKIN')
      ALLOCATE(DE(MAXEPO),STAT=IAC)
      CALL ALCERR(IAC,'DE',(/MAXEPO/),'EXTKIN')
      ALLOCATE(DU(MAXEPO),STAT=IAC)
      CALL ALCERR(IAC,'DU',(/MAXEPO/),'EXTKIN')
      ALLOCATE(NE(MAXEPO),STAT=IAC)
      CALL ALCERR(IAC,'NE',(/MAXEPO/),'EXTKIN')
C
C LOOP OVER ALL FILES
C -------------------
CCCC      DO 100 IFIL=1,NFIL
      IFIL=NFIL
C
C SKIP BAD OUTPUT FILE
C --------------------
        IPGM=FILTYP(IFIL)
        IF (IPGM.NE.1.AND.IPGM.NE.11) GOTO 100
C
C OPEN GPSEST OUTPUT FILE (INPUT)
C -------------------------------
        FILOUT=FILNAM(IFIL)
        CALL OPNFIL(LFNLOC,FILOUT,'OLD','FORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILOUT,'EXTKIN')
C
C READ TITLE OF GPSEST RUN
C ------------------------
        READ(LFNLOC,101,END=900) TITLE(IFIL)
101     FORMAT(///,1X,A80)
C
C READ THE WHOLE OUTPUT FILE
C --------------------------
        NUMFIX=0
        NEPO=0
        DO 10 IREC=1,1000000000
          READ(LFNLOC,'(A)',END=90)LINE
C
C FIND AND READ THE PARAMETER LISTING
C -----------------------------------
          IF (LINE(1:33).EQ.' PARAMETER TYPE                  ') THEN
            DO IRECT=1,1000
              READ(LFNLOC,'(A)',END=900)LINE
              IF (LINE(1:33).EQ.' AMBIGUITIES                     ')
     1          READ(LINE(55:59),*,END=900)NAMB
              IF (LINE(1:33).EQ.' KINEMATIC COORDINATES           ')
     1          READ(LINE(55:59),*,END=900)NKINCRD
              IF (LINE(1:33).EQ.' EPOCH WISE STATION CLOCKS       ')
     1          READ(LINE(55:59),*,END=900)NCLKSTA
              IF (LINE(1:33).EQ.' TOTAL NUMBER OF PARAMETERS      ')THEN
                READ(LINE(55:59),*,END=900)NPARTOT
                GOTO 10
              ENDIF
            ENDDO
          ENDIF
C
C FIND AND READ THE OBSERVATION LISTING
C -------------------------------------
          IF (LINE(1:33).EQ.' NUMBER OF OBSERVATIONS (PART 1):') THEN
            DO IRECT=1,1000
              READ(LFNLOC,'(A)',END=900)LINE
              IF (LINE(1:33).EQ.' PHASE           L3              ')
     1          READ(LINE(54:59),*,END=900)NPHASL3
              IF (LINE(1:33).EQ.' CODE            L3              ')
     1          READ(LINE(54:59),*,END=900)NCODEL3
              IF (LINE(1:33).EQ.' TOTAL NUMBER OF OBSERVATIONS    ')THEN
                READ(LINE(54:59),*,END=900)NOBSTOT
                IF (NPHASL3.EQ.0) SIGTOL=10.D0
                GOTO 10
              ENDIF
            ENDDO
          ENDIF
C
C READ THE SIGMA
C --------------
          IF (LINE(1:37).EQ.' A POSTERIORI SIGMA OF UNIT WEIGHT  :')
     1    THEN
            IF (LINE(39:39).EQ.'*') THEN
              SIGMA=9.999
            ELSE
              READ(LINE(39:47),*,END=900)SIGMA
            ENDIF
            NUMFIX=1
          ENDIF
C
C FIND AND READ THE KINEMATIC COORDINATES OUTPUT
C ----------------------------------------------
          IF (NUMFIX.EQ.1.AND.
     1        LINE(1:33).EQ.' KINEMATIC COORDINATES:          ') THEN
            IF (NKINCRD/3.GT.MAXEPO) THEN
              MMXEPO=NKINCRD/3
              DEALLOCATE(DN,STAT=IAC)
              ALLOCATE(DN(MMXEPO),STAT=IAC)
              CALL ALCERR(IAC,'DN',(/MMXEPO/),'EXTKIN')
              DEALLOCATE(DE,STAT=IAC)
              ALLOCATE(DE(MMXEPO),STAT=IAC)
              CALL ALCERR(IAC,'DE',(/MMXEPO/),'EXTKIN')
              DEALLOCATE(DU,STAT=IAC)
              ALLOCATE(DU(MMXEPO),STAT=IAC)
              CALL ALCERR(IAC,'DU',(/MMXEPO/),'EXTKIN')
              DEALLOCATE(NE,STAT=IAC)
              ALLOCATE(NE(MMXEPO),STAT=IAC)
              CALL ALCERR(IAC,'NE',(/MMXEPO/),'EXTKIN')
            ENDIF
            LINE = ' '
            DO WHILE (LINE(32:32).NE.'-')
              READ(LFNLOC,'(A)') LINE
            ENDDO
            STNAME=' '
            DO IRECT=1,MMXEPO + 10
CC            DO IRECT=1,10000
C
C             Read next line (skip one empty line)
              READ(LFNLOC,'(A)',END=90)LINE
              IF (LEN_TRIM(LINE).EQ.0) READ(LFNLOC,'(A)',END=90)LINE
C
C             Singular epoch
              IF (INDEX(LINE,'### SINGULAR EPOCH ###').NE.0) GOTO 104
C
C             LEO records
              IF (INDEX(LINE,'LEO Earth-fixed XYZ').NE.0) THEN
                READ(LINE,'(I6,22X,16A,1X,3(F10.3,4X,F7.3))',ERR=104)
     1               IEPO,STNAM2,DPHI,SIGPHI,DLON,SIGLON,DH,SIGHGT
                IF (STNAME.EQ.' ') STNAME=STNAM2
                IF (STNAME.NE.STNAM2) IEPO=0
              ELSE IF (INDEX(LINE,'LEO fixed').NE.0) THEN
                READ(LINE,'(I6,22X,16A,1X,3(F10.3,4X,F7.3))',ERR=104)
     1               IEPO,STNAM2,DPHI,SIGPHI,DLON,SIGLON,DH,SIGHGT
                IF (STNAME.EQ.' ') STNAME=STNAM2
                IF (STNAME.NE.STNAM2) IEPO=0
C
C             Station records
              ELSE IF (STNAME.EQ.' ') THEN
                IEPO = -1
                CALL SJUSTL(LINE)
                READ(LINE,'(A16,10X, 2(A1,2I3,F10.6,2X), F15.4)',
     1               END=900,ERR=102)
     2               STNAME, VPO,IP1O,IP2O,RP3O,VLO,IL1O,IL2O,RL3O, HHO
                GOTO 106
102             READ(LINE,'(A16,1X, 2(A1,2I3,F10.6,1X), 1X,F15.4)',
     1               END=900)
     2               STNAME, VPO,IP1O,IP2O,RP3O,VLO,IL1O,IL2O,RL3O, HHO
106             CONTINUE
              ELSE
                READ(LINE,'(I5,27X,3(F10.4,4X,F5.3))',ERR=103)
     1               IEPO,DPHI,SIGPHI,DLON,SIGLON,DH,SIGHGT
                GOTO 105
103             READ(LINE,'(I6,16X,3(F10.4,4X,F5.3))',ERR=104)
     1               IEPO,DPHI,SIGPHI,DLON,SIGLON,DH,SIGHGT
105             CONTINUE
              ENDIF
              IF (IEPO.GT.0) THEN
                IF ((DPHI.NE.0D0 .OR. DLON.NE.0D0 .OR. DH.NE.0D0) .AND.
     1              SIGPHI.LE.SIGTOL .AND. SIGLON.LE.SIGTOL .AND.
     2              SIGHGT.LE.SIGTOL) THEN
                  IF (NEPO.EQ.SIZE(NE)) THEN
                    WRITE(LFNERR,905) NEPO,SIZE(NE)
905                 FORMAT(/,' ### SR EXTKIN: READ NOT MORE THAN ',I4,
     1              ' EPOCHS (MAXEPO = ',I4,')',/)
                    CALL STATIS(NEPO,DN,xMed=DNMED,xMean =DNMEA,
     1                                  xRms=DNSIG,xSigma=DNRMS)
                    CALL STATIS(NEPO,DE,xMed=DEMED,xMean =DEMEA,
     1                                  xRms=DESIG,xSigma=DERMS)
                    CALL STATIS(NEPO,DU,xMed=DUMED,xMean =DUMEA,
     1                                  xRms=DUSIG,xSigma=DURMS)
                    GOTO 90
                  ENDIF
                  NEPO=NEPO+1
                  NE(NEPO)=IEPO
                  DN(NEPO)=DPHI
                  DE(NEPO)=DLON
                  DU(NEPO)=DH
                ELSE
                  WRITE(LFNERR,903) IEPO
903               FORMAT(/,' ### SR EXTKIN: EPOCH ',I4,
     1              ' NOT ACCEPTED',/)
                ENDIF
              ELSE IF (IEPO.NE.-1) THEN
                READ(LINE,501,ERR=104) STNAM2
501             FORMAT(7X,A16)
                IF (STNAM2.NE.' ') THEN
                   WRITE(LFNERR,904) STNAME
904                FORMAT(/,' ### SR EXTKIN: TWO OR MORE KINEMATIC',
     1                      ' STATIONS IN OUTPUT FILE.',/,
     2                  16X,'EXTRACTION OF KINEMATIC COORDINATES',/,
     3                  16X,'ONLY FOR FIRST STATION: ',A16)
                ENDIF
                CALL STATIS(NEPO,DN,xMed=DNMED,xMean =DNMEA,
     1                              xRms=DNSIG,xSigma=DNRMS)
                CALL STATIS(NEPO,DE,xMed=DEMED,xMean =DEMEA,
     1                              xRms=DESIG,xSigma=DERMS)
                CALL STATIS(NEPO,DU,xMed=DUMED,xMean =DUMEA,
     1                              xRms=DUSIG,xSigma=DURMS)
                GOTO 90
              ENDIF
104         ENDDO
          ENDIF
C
C READ THE NEXT LINE
C ------------------
10      CONTINUE
C
C CLOSE FILE
C ----------
90      CLOSE(LFNLOC)
C
100   CONTINUE
C
C WRITE KINEMATIC SUMMARY FILE
C ----------------------------
      IF (NEPO.GT.0) THEN
        WRITE(LFN001,201)TITLE
201     FORMAT(1X,A80)
C
        WRITE(LFN001,210)NEPO,NCLKSTA,NKINCRD/3,
     1                   XYZAPR,XYZEST,DIFXYZ,
     2                   DNMEA,DEMEA,DUMEA
210     FORMAT('*XEPO   ',3I15,/,
     1         '*INI    ',3F15.4,/,
     2         '*EST    ',3F15.4,/,
     3         '*DIFXYZ ',3F15.4,/,
     4         '*DIFNEU ',3F15.4)
C
        WRITE(LFN001,220)
220     FORMAT(79('-'))
C
        WRITE(LFN001,230)
230     FORMAT('  EPOCH',13X,'DN',13X,'DE',13X,'DU')
        DO IEPO=1,NEPO
          WRITE(LFN001,240)NE(IEPO),DN(IEPO),DE(IEPO),DU(IEPO)
240       FORMAT(I7,1X,3F15.4)
        ENDDO
C
        WRITE(LFN001,220)
C
        WRITE(LFN001,250)DNMEA,DEMEA,DUMEA,
     1                   DNSIG,DESIG,DUSIG,
     2                   DNRMS,DERMS,DURMS,SIGMA
250     FORMAT('*AVG    ',3F15.4,/,
     1         '*SIG    ',3F15.4,/,
     2         '*RMS    ',3F15.4,/,
     3         '*RMSTC  ', F15.4)
C
        WRITE(LFN001,220)
C
        WRITE(LFN001,260)STNAME(1:4),NAMB,NKINCRD/3,NCLKSTA,NPARTOT,
     1                   NCODEL3,NPHASL3,NOBSTOT
260     FORMAT(' STATION:',1X,A4,/,
     1         ' PARAMS :',4I7,' (AMB, CRD, CLK, TOT)',/,
     2         ' OBSERV :',3I7)
      ENDIF
C
C CLOSE KINEMATIC SUMMARY FILE
C ----------------------------
      CLOSE(LFN001)
C
      DEALLOCATE(DN,STAT=IAC)
      DEALLOCATE(DE,STAT=IAC)
      DEALLOCATE(DU,STAT=IAC)
      DEALLOCATE(NE,STAT=IAC)
C
900   CONTINUE
      IF (NEPO.EQ.0) THEN
        WRITE(LFNERR,902) FILOUT
902     FORMAT(/,' ### SR EXTKIN: NO CONTENTS FOUND IN OUTPUT FILE',
     1                     /,16X,'OUTPUT FILE NAME: ',A32,/)
      ENDIF
C
999   RETURN
      END SUBROUTINE

      END MODULE
