      MODULE s_ORBINF
      CONTAINS

C*
      SUBROUTINE ORBINF(NFTOT,TFIRST,NARC,ARCINT,NUMSAT,
     1                  SOURCE,TBOUND,NAVNUM,IORSYS,cmcyn,timSTD)
CC
CC PURPOSE    :  READ RELEVANT INFORMATION CONCERNING SATELLITE ARCS
CC               FROM ORBIT FILE.
CC
CC PARAMETERS :
CC         IN :  NFTOT  : NUMBER OF FILES TO BE PROCESSED     I*4
CC               TFIRST(I),I=1,2,..,NFTOT: FIRST OBSERVATION  R*8
CC                        TIME IN FILE I (MJD)
CC        OUT :  NARC   : NUMBER OF ARCS ON FILE              I*4
CC               ARCINT(I),I=1,2,..,NFTOT:ARC NUMBER ASSOCIA- I*4
CC                        TED WITH FILE I
CC               NUMSAT(I),I=1,2,...,NARC: NUMBER OF SATELLI- I*4
CC                        TES IN ARC I
CC               SOURCE(K,I),K=1,..,10,I=1,2,..,NARC          CH*1
CC               TBOUND(K,I),K=1,2, I=1,2,..,NARC: INTERVAL   R*8
CC                        BOUNDARIES FOR ARC I (MJD)
CC               NAVNUM(L),L=1,2,... : NAV-NUMBERS FOR ALL    I*4
CC                        ARCS
CC               IORSYS : ORBIT SYSTEM                        I*4
CC                        =1: B1950.0
CC                        =2: J2000.0
CC               timSTD : Time system of STD orbit           CH*3
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/02 16:42
CC
CC CHANGES    :  13-DEC-91 : ??: DEFINE ARC FOR EACH FILE WITH OPEN
CC                               RIGHT BOUNDARY (LOWER THAN)
CC               04-JUN-92 : ??: OPTION J2000.0. RETURN "IORSYS"
CC                               OPNFIL USED
CC               10-AUG-94 : MR: CALL EXITRC
CC               22-JAN-00 : HU: BACKSPACE REMOVED, NO LONGER SUPPORTED
CC               06-AUG-03 : HU: NEW STD FORMAT, CHECK NUTATION MODEL
CC               01-SEP-03 : HU: USE INTERFACE FOR RDNUTSUB
CC               30-SEP-03 : HB: TAKE MAXINT FROM M_MAXDIM
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               18-JUL-06 : AG: CMC ADDED
CC               28-MAR-07 : HB: CHANGE CALL OF CHKSYS (IERS2003 CONV)
CC               14-NOV-08 : DT: Read time system from STD; add timSTD
CC               03-DEC-10 : RD: CMC FOR ATL ADDED
CC               15-JAN-11 : MF: LFNLOC -> LFN001 (RUNTIME ERROR),
CC                               LFN COLLISION WHEN CALLING SR RDCMC,
CC                               CHECK PRESENCE OF timSTD
CC               26-MAR-12 : RD: SWITCH FROM TIMSTR TO TIMST2
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: LFN001, LFNERR
      USE m_maxdim, ONLY: MAXINT
      USE s_cmc,    ONLY: chkcmc
      USE s_opnfil
      USE s_opnerr
      USE s_timst2
      USE s_exitrc
      USE s_chksys
      USE s_rdnutsub
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IARC  , IFIRST, IFMT  , INT   , IOF   , IORSYS,
     1          IOSTAT, IQ    , IRC   , IS    , ISAT0 , K     , METHOD,
     2          MXCARC, MXCSAT, NARC  , NFTOT , NINT  , NINT1 , NLIN  ,
     3          NSAT
C
      REAL*8    AXIS  , POLYCO, TEST  , TLOCAL, TOSC0
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      REAL*8       TFIRST(*),TBOUND(2,*)
      REAL*8       TB12(MAXINT+1)
      INTEGER*4    ARCINT(*),NUMSAT(*),NAVNUM(*)
      CHARACTER*1  SOURCE(10,*),cmcchr
      CHARACTER*6  MXNARC,MXNSAT
      CHARACTER*19 TSTRNG
      CHARACTER*32 FILORB
      CHARACTER*80 LINE
      CHARACTER*16 NUTNAM,SUBNAM,tcmcmod(2)
C
      LOGICAL,           DIMENSION(2), OPTIONAL  :: cmcyn
      CHARACTER(LEN=3),                OPTIONAL  :: timSTD
      LOGICAL      tcmcyn(2)
C
      COMMON/CORBNF/ TB12
      COMMON/MCMARC/MXCARC,MXNARC
      COMMON/MCMSAT/MXCSAT,MXNSAT
      DATA IFIRST/1/
C
C OPEN FILE WITH STANDARD ORBITS
C ------------------------------
      IF(IFIRST.EQ.1) THEN
        IFIRST=0
        CALL rdnutsub(nutnam,subnam)
        CALL GTFLNA(1,'STDORB ',FILORB,IRC)
        CALL OPNFIL(LFN001,FILORB,'OLD','UNFORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFN001,IOSTAT,FILORB,'ORBINF')
C
C READ NUMBER OF ARCS ON FILE, CHECK MAXIMUM NUMBER OF ARCS
        NUTNAM=' '
        SUBNAM=' '
        tcmcmod=' '
        cmcchr=' '
        tcmcyn =.FALSE.
        IF (PRESENT(timSTD)) timSTD = 'GPS'
        READ(LFN001) NARC
C
C READ FORMAT NUMBER AND ORBIT DESCRIPTION
        IF (NARC.LT.0) THEN
          READ(LFN001) IFMT,NARC
          READ(LFN001) NLIN
          DO I=1,NLIN
            READ(LFN001) LINE
            IF (LINE(1:7).EQ.'NUTSUB:') THEN
!              READ(LINE,"(8X,A16,1X,A16)") NUTNAM,SUBNAM
              CALL CHKSYS(1,LINE,IRC)
            ELSEIF (LINE(1:7).EQ.'OTLOAD:') THEN
              READ(LINE,"(8X,A16,6X,A1)")tcmcmod(1),cmcchr
              IF (cmcchr == 'Y') tcmcyn(1) = .TRUE.
            ELSEIF (LINE(1:7).EQ.'ATLOAD:') THEN
              READ(LINE,"(8X,A16,6X,A1)")tcmcmod(2),cmcchr
              IF (cmcchr == 'Y') tcmcyn(2) = .TRUE.
            ELSEIF (LINE(1:7).EQ.'TIMSYS:') THEN
              IF (PRESENT(timSTD)) READ(LINE,"(8X,A3)") timSTD
            ENDIF
          ENDDO
          IF (PRESENT(cmcyn)) THEN
            cmcyn=tcmcyn
            CALL chkcmc(tcmcyn,tcmcmod)
          ENDIF
        ENDIF
        IF(NARC.GT.MXCARC) THEN
          WRITE(LFNERR,502) NARC,MXCARC
502       FORMAT(/,' *** SR ORBINF: TOO MANY SATELLITE ARCS',/,
     1                         16X,'NUMBER OF ARCS: ',I4,/,
     2                         16X,'MAXIMUM NUMBER: ',I4,/)
          CALL EXITRC(2)
        END IF
C
C COLLECT INFORMATION FOR EACH ARC
        ISAT0=0
        DO 40 IARC=1,NARC
          READ(LFN001) NSAT,NINT,IQ,(NAVNUM(ISAT0+I),I=1,NSAT),
     1                 (SOURCE(K,IARC),K=1,10)
C
C CHECK MAXIMUM NUMBER OF INTEGRATION INTERVALS
          IF(NINT.GT.MAXINT) THEN
            WRITE(LFNERR,503) NINT,MAXINT
503         FORMAT(/,' *** SR ORBINF: TOO MANY INTEGRATION INTERVALS',
     1                         /,16X,'NUMBER OF INTERVALS: ',I8,/,
     2                           16X,'MAXIMUM NUMBER     : ',I8,/)
            CALL EXITRC(2)
          ENDIF
C
C CHECK MAXIMUM NUMBER OF SATELLITES
          IF(NSAT.GT.MXCSAT) THEN
            WRITE(LFNERR,504) IARC,NSAT,MXCSAT
504         FORMAT(/,' *** SR ORBINF: TOO MANY SATELLITES IN ARC',/,
     1                           16X,'ARC NUMBER          : ',I4,/,
     2                           16X,'NUMBER OF SATELLITES: ',I4,/,
     3                           16X,'MAX. NUMBER OF SAT. : ',I4,/)
            CALL EXITRC(2)
          ENDIF
C
          NINT1=NINT+1
          ISAT0=ISAT0+NSAT
C
C READ INTERVAL BOUNDARIES
          READ(LFN001) TOSC0,TB12(1)
          IF(TB12(1).EQ.0.D0) THEN
            IORSYS=1
            METHOD=2
          ELSE IF(TB12(1).EQ.2.D0) THEN
            IORSYS=2
            METHOD=2
          ELSE
            IORSYS=1
            METHOD=1
          ENDIF
          IF (METHOD.EQ.2) THEN
            DO 10 I=1,NINT1
              READ(LFN001) TB12(I)
10          CONTINUE
          ELSE
C           BACKSPACE LFN001
C           READ(LFN001) TOSC0,(TB12(I),I=1,NINT1)
            WRITE(LFNERR,904)
904         FORMAT(/,' *** SR ORBINF: OLD BINARY FORMAT NO ',
     1                               'LONGER SUPPORTED',/,
     1                           16X,'REQUIRES "BACKSPACE"',/)
            CALL EXITRC(2)
          ENDIF
C
C SKIP REMAINING ARC INFORMATION
          DO 20 IS=1,NSAT
            READ(LFN001) AXIS
20        CONTINUE
          DO 30 INT=1,NINT
            READ(LFN001) TLOCAL
            DO 30 K=1,IQ+1
              READ(LFN001) POLYCO
30        CONTINUE
C
C SAVE RELEVANT INFORMATION
          NUMSAT(IARC)=NSAT
          TBOUND(1,IARC)=TB12(1)
          TBOUND(2,IARC)=TB12(NINT1)
40      CONTINUE
      END IF
C
C DETECT INTERNAL ARC NUMBER FOR EACH OBSERVATION-FILE
      DO 70 IOF=1,NFTOT
        TEST=TFIRST(IOF)
        ARCINT(IOF)=0
        DO 50 IARC=1,NARC
          IF(TEST.GE.TBOUND(1,IARC).AND.TEST.LT.TBOUND(2,IARC)) THEN
            ARCINT(IOF)=IARC
            GOTO 70
          ENDIF
50      CONTINUE
C
C NO ARC FOUND CONTAINING FIRST EPOCH OF FILE
        CALL TIMST2(1,1,TEST,TSTRNG)
        WRITE(LFNERR,60) IOF,TSTRNG
60      FORMAT(/,' *** SR ORBINF: NO ORBIT INFORMATION FOUND',/,
     1                     16X,'OBSERVATION FILE: ',I3,/,
     2                     16X,'FIRST EPOCH     : ',A19,/)
        CALL EXITRC(2)
70    CONTINUE
C
      CLOSE(UNIT=LFN001)
C
      RETURN
      END SUBROUTINE

      END MODULE
