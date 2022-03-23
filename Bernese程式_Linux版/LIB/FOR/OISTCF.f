      MODULE s_OISTCF
      CONTAINS

C*
      SUBROUTINE OISTCF(FILORB,NFTOT,TMIDLE,NSASTC,NUMSTC,
     1                  NSTDAY,NSTCEP,FRCTYP,SIGSTC,NSPEC,NUMSPC,
     2                  TIMSPC,SIGSPC,NARC,ARCINT,NUMSAT,SOURCE,
     3                  TBOUND,NAVNUM,IORSYS,NSTCEF,TIMSTC,INTSTC,
     4                  NMXINT)
CC
CC NAME       :  OISTCF
CC
CC PURPOSE    :  READ RELEVANT INFORMATION CONCERNING SATELLITE ARCS
CC               FROM ORBIT FILE.
CC               DEFINE STOCHASTIC REQUESTS
CC
CC PARAMETERS :
CC         IN :  FILORB : NAME OF STANDARD ORBIT FILE         CH*32
CC               NFTOT  : NUMBER OF FILES TO BE PROCESSED     I*4
CC               TMIDLE(I),I=1,2,..,NFTOT: MID OF OBSERVATION R*8
CC                        INTERVAL IN FILE I (MJD)
CC               NSASTC : NUMBER OF SATS WITH STOCH ORBITS    I*4
CC               NUMSTC : CORRESPONDING SAT NUMBERS           I*4(*)
CC               NSTDAY : NUMBER OF STOCH EPOCHS/DAY FOR EACH I*4(*)
CC                        SATELLITE
CC               NSTCEP : NUMBER OF STOCHASTIC                I*4
CC                        FORCES PER EPOCH
CC               FRCTYP : CORRESPONDING FORCE TYPES           I*4(*)
CC               SIGSTC : A PRIORI SIGMAS FOR STOCH REQUESTS  R*8(*,*)
CC               NSPEC  : NUMBER OF SPECIAL STOCH. REQUESTS   I*4
CC               NUMSPC : SATELLITE NUMBERS                   I*4(*)
CC               TIMSPC : TIMES FOR THESE REQUESTS            R*8(*)
CC               SIGSPC : CORRESPONSING A PRIORI SIGMAS       R*8(*,*)
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
CC               NSTCEF : NUMBER OF STOCHASTIC EPOCHS/ARC     I*4(*)
CC               TIMSTC : STOCHASTIC EPOCHS (=INT BOUNDARIES) R*8(*,*,*,*)
CC               INTSTC : CORRESPONDING INTERVAL NUMBERS      I*4(*,*,*)
CC               NMXINT : MAX. NUMBER OF INTEGR. INTERVALS    I*4
CC
CC REMARKS    :  - THIS SUBROUTINE WAS ORIGINALLY CALLED ORBINF.
CC                 IN DECEMBER 1992 IT WAS RENAMED OISTCI TO HANDLE
CC                 THE STOCHASTIC ORBIT PARAMETERS IN ADDITION TO
CC                 THE OLD FUNCTIONS
CC               - 21-MAY-01 THIS SR WAS RENAMED OISTCF WITH
CC                 ADDITIONAL INPUT PARAMETER FILORB
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  14-JAN-93
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC                2-SEP-94 : EB: NO CHECK FOR STOCHASTIC PARA. FOR OBS.
CC               13-APR-95 : RW: SPEC.STOCHASTIC REQUEST
CC                               CORRECT PULSE SETTING AT END OF ARC
CC               02-JUN-95 : LM: CALL SR STOECL
CC               02-NOV-95 : MR: CORRECT CALL EXITRC (HAD NO PARAMETER)
CC               23-SEP-97 : DI: USE MAXSAT.inc
CC               22-JAN-00 : HU: BACKSPACE REMOVED, NO LONGER SUPPORTED
CC               21-MAY-01 : DS: CREATION, SR OISTCI RENAMED OISTCF,
CC                               WITH NEW INPUT PARAMETER FILORB
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               07-MAR-03 : HU: MAXINT MOVED TO M_MAXDIM
CC               06-AUG-03 : HU: NEW STD FORMAT, CHECK NUTATION MODEL
CC               10-DEC-03 : AJ: ADDITIONAL INPUT PARAMETER FRCTYP
CC               10-MAR-04 : HB: REMOVE ONLY-STATEMENT FOR M_BERN
CC               14-APR-05 : AJ: UPPER LIMIT IN LOOP 44: NINT+1
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-MAR-07 : HB: CHANGE CALL OF CHKSYS (IERS2003 CONV)
CC               03-DEC-10 : HB: ADD PARAMETER NMXINT
CC               29-FEB-12 : RD: USE TIMST2 INSTEAD OF TIMSTR
CC               29-FEB-12 : RD: REMOVE UNUSED VARIABLES
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1993     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: fileNameLength, lfnerr, lfnloc
      USE m_maxdim, ONLY: MAXSAT, MAXINT
      USE s_opnfil
      USE s_stoecl
      USE s_stcprp
      USE s_opnerr
      USE s_maxtst
      USE s_timst2
      USE s_exitrc
      USE s_chksys
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IA    , IARC  , IFIRST, IFMT  , IND   , INT   ,
     1          IOF   , IORSYS, IOSTAT, IQ    , IRC   , IS    , ISASTC,
     2          ISAT  , ISAT0 , ISPEC , ISTC  , K     , KARC  , METHOD,
     3          MXCARC, MXCSAT, MXCSTC, NARC  , NFTOT , NINT  , NINT1 ,
     4          NLIN  , NSASTC, NSAT  , NSPEC , NSTCEP, NUMBER, NUMREQ,
     5          NMXINT
C
      REAL*8    AXIS  , POLYCO, TEST  , TLOCAL, TMIN  , TOSC0
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON/MCMARC/MXCARC,MXNARC
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMSTC/MXCSTC,MXNSTC
C
      REAL*8       TMIDLE(*),TBOUND(2,*)
      REAL*8       TB12(MAXINT+1)
      REAL*8       TIMSTC(3,MXCSTC,MXCSAT,*),TIMSPC(*),SIGSTC(3,*)
      REAL*8       SIGSPC(3,*)
      INTEGER*4    ARCINT(*),NUMSAT(*),NAVNUM(*)
      INTEGER*4    NUMSTC(*),NSTDAY(*),FRCTYP(*),NSTCEF(MXCSAT,*)
      INTEGER*4    STCINT(MAXINT),INTSTC(MXCSTC,MXCSAT,*),NUMSPC(*)
      INTEGER*4    ARCSPC(MAXSAT),INTSPC(MAXSAT)
      CHARACTER*1  SOURCE(10,*)
      CHARACTER*6  MXNARC,MXNSAT,MXNSTC
      CHARACTER*19 TSTRNG
      CHARACTER*16 NUTNAM,SUBNAM
      CHARACTER*80 LINE
      CHARACTER(LEN=fileNameLength)   :: FILORB
C
      COMMON/CORBNF/ TB12
      DATA IFIRST/1/
C
C OPEN FILE WITH STANDARD ORBITS
C ------------------------------
C      IF(IFIRST.EQ.1) THEN
        CALL MAXTST(1,'OISTCF',MXNSAT,MAXSAT,MXCSAT,IRC)
        IF(IRC.NE.0) CALL EXITRC(2)
        IFIRST=0
        CALL OPNFIL(LFNLOC,FILORB,'OLD','UNFORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILORB,'OISTCF')
C
C READ NUMBER OF ARCS ON FILE, CHECK MAXIMUM NUMBER OF ARCS
        NUTNAM=' '
        SUBNAM=' '
        READ(LFNLOC) NARC
C
C READ FORMAT NUMBER AND ORBIT DESCRIPTION
        IF (NARC.LT.0) THEN
          READ(LFNLOC) IFMT,NARC
          READ(LFNLOC) NLIN
          DO I=1,NLIN
            READ(LFNLOC) LINE
            IF (LINE(1:7).EQ.'NUTSUB:') THEN
!              READ(LINE,"(8X,A16,1X,A16)") NUTNAM,SUBNAM
              CALL CHKSYS(1,LINE,IRC)
            ENDIF
          ENDDO
        ELSE
C DEFAULT FOR OLD FORMAT
          LINE='NUTSUB: IAU80            RAY'
          CALL CHKSYS(1,LINE,IRC)
        ENDIF
        IF(NARC.GT.MXCARC) THEN
          WRITE(LFNERR,502) NARC,MXCARC
502       FORMAT(/,' *** SR OISTCF: TOO MANY SATELLITE ARCS',/,
     1                         16X,'NUMBER OF ARCS: ',I4,/,
     2                         16X,'MAXIMUM NUMBER: ',I4,/)
          CALL EXITRC(2)
        END IF
C
C INITIALIZE ARRAYS FOR SPECIAL STOCHASTIC REQUESTS
        DO 11 ISPEC=1,NSPEC
          ARCSPC(ISPEC)=0
          INTSPC(ISPEC)=0
11      CONTINUE
C
C COLLECT INFORMATION FOR EACH ARC
        ISAT0=0
        DO 40 IARC=1,NARC
          READ(LFNLOC) NSAT,NINT,IQ,(NAVNUM(ISAT0+I),I=1,NSAT),
     1                 (SOURCE(K,IARC),K=1,10)
C
C CHECK MAXIMUM NUMBER OF INTEGRATION INTERVALS
          IF(NINT.GT.MAXINT) THEN
            WRITE(LFNERR,503) NINT,MAXINT
503         FORMAT(/,' *** SR OISTCF: TOO MANY INTEGRATION INTERVALS',
     1                         /,16X,'NUMBER OF INTERVALS: ',I4,/,
     2                           16X,'MAXIMUM NUMBER     : ',I4,/)
            CALL EXITRC(2)
          ENDIF
C
C CHECK MAXIMUM NUMBER OF SATELLITES
          IF(NSAT.GT.MXCSAT) THEN
            WRITE(LFNERR,504) IARC,NSAT,MXCSAT
504         FORMAT(/,' *** SR OISTCF: TOO MANY SATELLITES IN ARC',/,
     1                           16X,'ARC NUMBER          : ',I4,/,
     2                           16X,'NUMBER OF SATELLITES: ',I4,/,
     3                           16X,'MAX. NUMBER OF SAT. : ',I4,/)
            CALL EXITRC(2)
          ENDIF
C
          NINT1=NINT+1
C
C READ INTERVAL BOUNDARIES
          READ(LFNLOC) TOSC0,TB12(1)
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
              READ(LFNLOC) TB12(I)
10          CONTINUE
          ELSE
C           BACKSPACE LFNLOC
C           READ(LFNLOC) TOSC0,(TB12(I),I=1,NINT1)
            WRITE(LFNERR,904)
904         FORMAT(/,' *** SR OISTCF: OLD BINARY FORMAT NO ',
     1                               'LONGER SUPPORTED',/,
     1                           16X,'REQUIRES "BACKSPACE"',/)
            CALL EXITRC(2)
          ENDIF
C
C SKIP REMAINING ARC INFORMATION
          DO 20 IS=1,NSAT
            READ(LFNLOC) AXIS
20        CONTINUE
          DO 30 INT=1,NINT
            READ(LFNLOC) TLOCAL
            DO 30 K=1,IQ+1
              READ(LFNLOC) POLYCO
30        CONTINUE
C
C SAVE RELEVANT INFORMATION
          NUMSAT(IARC)=NSAT
          TBOUND(1,IARC)=TB12(1)
          TBOUND(2,IARC)=TB12(NINT1)
C
C DEFINE STOCHASTIC QUANTITIES PER EPOCH
C --------------------------------------
C
C EXPAND THE SPECIAL REQUESTS "97", "98" AND "99"

          CALL STOECL(FILORB,NUMSAT(IARC),NAVNUM(ISAT0+1),
     1                TBOUND(1,IARC),TBOUND(2,IARC),NSASTC,NUMSTC,
     2                NSTDAY,SIGSTC,NSPEC,NUMSPC,TIMSPC,SIGSPC)
          ISAT0=ISAT0+NSAT
C
C LOOP OVER ALL SATELLITES WITH STOCHASTIC ORBITS
          DO 45 ISASTC=1,NSASTC
            CALL STCPRP(NINT,TB12,NSTCEP,NSTDAY(ISASTC),
     1                  FRCTYP,NSTCEF(ISASTC,IARC),STCINT,
     2                  TIMSTC(1,1,ISASTC,IARC))
            IF(NINT > NMXINT) NMXINT = NINT
            DO 44 K=1,NINT+1
              IF(STCINT(K).NE.0)THEN
                IND=STCINT(K)
                INTSTC(IND,ISASTC,IARC)=K
              END IF
44          CONTINUE
45        CONTINUE
C
C HANDLE SPECIAL STOCHASTIC REQUESTS, PART 1
C ------------------------------------------
          DO 49 ISPEC=1,NSPEC
            IF(TIMSPC(ISPEC).GE.TB12(1).AND.
     1         TIMSPC(ISPEC).LE.TB12(NINT+1))THEN
              ARCSPC(ISPEC)=IARC
              TMIN=10000000.
              DO 46 INT=1,NINT+1
                TEST=DABS(TIMSPC(ISPEC)-TB12(INT))
                IF(TEST.LT.TMIN)THEN
                  TMIN=TEST
                  INTSPC(ISPEC)=INT
                END IF
46            CONTINUE
              TIMSPC(ISPEC)=TB12(INTSPC(ISPEC))
            END IF
49        CONTINUE
40      CONTINUE
C
C HANDLE SPECIAL STOCHASTIC REQUESTS, PART 2:
C ------------------------------------------
        DO 80 ISPEC=1,NSPEC
          IARC=ARCSPC(ISPEC)
          INT =INTSPC(ISPEC)
          IF(IARC.EQ.0.OR.INT.EQ.0)GO TO 80
C
C (A) NEW/OLD SATELLITE ?
          DO 51 ISAT=1,NSASTC
            IF(NUMSPC(ISPEC).EQ.NUMSTC(ISAT))THEN
              NUMBER=ISAT
C
C CHECK WHETHER THE REQUESTED EPOCH IS IDENTICAL WITH THAT
C OF A "NORMAL" REQUEST
              DO 1052 ISTC=1,NSTCEF(ISAT,IARC)
                IF(TIMSPC(ISPEC).EQ.TIMSTC(1,ISTC,ISAT,IARC))GO TO 80
1052          CONTINUE
              GO TO 61
            END IF
51        CONTINUE
C
C NEW SATELLITE, ADD NEW REQUEST
C ------------------------------
          NSASTC=NSASTC+1
          NUMBER=NSASTC
          NUMSTC(NUMBER)=NUMSPC(ISPEC)
          DO 52 IA=1,NARC
            NSTCEF(NUMBER,IA)=0
52        CONTINUE
C
C DEFINE REQUEST
61        DO 62 K=1,3
            SIGSTC(K,NUMBER)=SIGSPC(K,ISPEC)
62        CONTINUE
          DO 59 KARC=1,NARC
            IF(IARC.EQ.KARC)THEN
              NSTCEF(NUMBER,IARC)=NSTCEF(NUMBER,IARC)+1
              NUMREQ=NSTCEF(NUMBER,IARC)
C
C CHECK MAXIMUM DIMENSIONS
              IF (NUMREQ.GT.MXCSTC) THEN
                WRITE(LFNERR,901) NUMSTC(NUMBER),IARC,NUMREQ,MXCSTC
901             FORMAT(/,' *** SR OISTCF: TOO MANY STOCHASTIC ORBIT',
     1                   ' PARAMETER REQUESTS',/,
     2                               16X,'SATELLITE NUMBER    :',I5,/,
     3                               16X,'ARC NUMBER          :',I5,/,
     4                               16X,'NUMBER OF REQUESTS >=',I5,/,
     5                               16X,'MAXIMUM NUMBER      :',I5,/)
                CALL EXITRC(2)
              ENDIF
              TIMSTC(1,NUMREQ,NUMBER,IARC)=TIMSPC(ISPEC)
              INTSTC(NUMREQ,NUMBER,IARC)=INTSPC(ISPEC)
            END IF
59        CONTINUE
80      CONTINUE
C      END IF
C
C DETECT INTERNAL ARC NUMBER FOR EACH OBSERVATION-FILE
      DO 70 IOF=1,NFTOT
        TEST=TMIDLE(IOF)
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
60      FORMAT(/,' *** SR OISTCF: NO ORBIT INFORMATION FOUND',/,
     1                     16X,'OBSERVATION FILE: ',I3,/,
     2                     16X,'FIRST EPOCH     : ',A19,/)
        CALL EXITRC(2)
70    CONTINUE
C
      CLOSE(UNIT=LFNLOC)
C
      RETURN
      END SUBROUTINE

      END MODULE
