      MODULE s_GETORF
      CONTAINS

C*
      SUBROUTINE GETORF(FILSTD,SVN,IANT,IDER,ISTOP,T,ICRARC,IORSYS,X,
     1                  TOSC,ELE,IRCODE,cmcyn)
CC
CC NAME       :  GETORF
CC
CC PURPOSE    :  COMPUTE POSITION (AND DERIVATIVES UP TO IDER)
CC               OF SATEL. WITH NUMBER SVN AND STORE THEM IN ARRAY X.
CC               MOREOVER RETURN THE OSCULATION EPOCH AND ELEMENTS OF
CC               CURRENT ARC.
CC
CC PARAMETERS :
CC         IN :  FILSTD : NAME OF STANDARD ORBIT FILE         CH*32
CC               SVN    : SVN-NUMBER OF SATELLITE             I*4
CC                        IF SVN < 0 THEN ALWAYS RETURN POS-
CC                        ITION OF THE SATELLITE, NOT OF
CC                        SVN+50 IF A MANOEUVRE HAPPENED
CC               IANT   : ANTENNA OFFSET CORRECTION FLAG      I*4
CC                        =0 : NO CORRECTION (CENTER OF MASS)
CC                        =1 : ANTENNA OFFSET CORRECTION
CC                        =2 : SLR REFLECTOR OFFSET CORRECTION
CC               IDER   : NUMBER OF DERIVATIVES TO BE COMP.   I*4
CC                        =0 : POSITION IS COMPUTED
CC                        =1 : POS.+VELOCITY IS COMPUTED
CC                        =2 : POS.+VEL.+ACCELERATION
CC                          E. T. C.
CC               ISTOP  : FLAG FOR STOP ON ERROR              I*4
CC                        =0 : NO STOP, RETURN CODE SET
CC                        =1 : STOP ON ERROR
CC                        =2 : NO STOP, IF SATELLITE MISSING.
CC                             PRINT WARNING ONCE PER SATEL.
CC               T      : TIME IN MODIFIED JULIAN DATE        R*8
CC        OUT :  ICRARC : CURRENT ARC NUMBER                  I*4
CC               IORSYS : ORBIT SYSTEM                        I*4
CC                        =1 : B1950.0
CC                        =2 : J2000.0
CC               X(I),I=1,2,3, 4,5,6, ... POSITION (M),       R*8
CC                      VELOCITY (M/S), ACC(M/S**2), ...
CC               TOSC   : OSCULATION EPOCH FOR ELEMENTS       R*8
CC               ELE(I),I=1,2,..,7 OSCULATING ELEMENTS        R*8
CC               IRCODE : RETURN CODE                         I*4
CC                        =0: OK
CC                        =1: NO ARC FOUND FOR TIME "T"
CC                        =2: ARC FOUND, BUT NO SATELLITE
CC                            "SVN" IN THIS ARC
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/10/28 15:50
CC
CC CHANGES    :  31-MAY-92 : ??: ALLOW FOR A SMALL EXTRAPOLATION AT ARC
CC                               BOUNDARIES
CC               04-JUN-92 : ??: NEW PARAMETER LIST AND RETURN CODE
CC               15-JUN-92 : ??: SATELLITE MANOEUVRES: RETURN CORRECT
CC                               SATELLITE POSITIONS (SVN OR SVN+50)
CC                               CORRECT ANTENNA OFFSETS (BLOCK I AND II)
CC               24-DEC-92 : ??: MAXSAT=40 DUE TO SATELLITE INFO FILE
CC               04-APR-93 : ??: SUNEFF: USE TDT INSTEAD OF GPS TIME
CC               28-OCT-93 : ??: OPTION ISTOP=2
CC               29-NOV-93 : MR: ADD FILE NAME TO ERROR MESSAGES
CC               10-AUG-94 : MR: CALL EXITRC
CC               17-AUG-94 : MR: MAXMAN=100 (OLD: MAXMAN=20)
CC               17-APR-95 : MR: SET IAOLD AND INTOLD IF ISTOP=0
CC               19-APR-95 : MR: SVN < 0: DISREGARD MANOUEVRES
CC               02-OCT-95 : WG: IANT=2: APPLY SLR REFLECTOR COORDINATES
CC               23-SEP-97 : DI: USE MAXSAT.inc AND INCLUDE
CC                               'I:MAXSAA' DUE TO SATELLITE INFO FILE
CC               23-SEP-97 : DI: REMOVE 'COMMON MCMSAT' AND 'MAXTST'
CC               22-JAN-00 : HU: BACKSPACE REMOVED, NO LONGER SUPPORTED
CC               15-AUG-01 : HB: INCREASE MAXINT 500 -> 800
CC               22-JAN-03 : HU: HANDLING OF OVERFLOW TRANSFERRED TO YPOL
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               07-MAR-03 : HU: MAXINT MOVED TO M_MAXDIM
CC               06-AUG-03 : HU: NEW STD FORMAT, CHECK NUTATION MODEL
CC               11-AUG-03 : RS: USE M_BERN, CHANGE CALL OF GTSATA
CC               30-AUG-03 : HU: SHARED DO LABELS REMOVED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               22-JUL-05 : HB: INQUIRE, CLOSE AND REOPEN STD FILE AGAIN
CC                               INSTEAD OF REWIND
CC               28-JUL-05 : RD: OPEN STRING LENGTH FOR FILSTD
CC               08-Aug-05 : HB: Use new SR TIMST2 (module)
CC               09-NOV-05 : AG: SENNUM for GTSATA CALL ADDED
CC               18-JUL-06 : AG: CMC IMPLEMENTED
CC               28-MAR-07 : HB: CHANGE CALL OF CHKSYS (IERS2003 CONV)
CC               30-MAY-07 : AG: USE s_suneff
CC               26-FEB-08 : RD: USE GTSATM FROM D_SATCRX
CC               01-OCT-10 : CR: NEW CALL OF SUNEFF
CC               02-DEC-10 : RD: CMC FOR ATL ADDED
CC               06-AUG-11 : HB: READ ORBIT DESCRIPTION (USE P_ORBGEN)
CC               26-MAR-12 : RD: SWITCH FROM TIMSTR TO TIMST2
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY:lfnor1,lfnerr,staNam2Length
      USE m_maxdim, ONLY: MAXSAT, MAXSAA, MAXINT
      USE d_satcrx, ONLY: gtsatm
      USE s_cmc,    ONLY: chkcmc
      USE p_orbgen, ONLY: orbdsc
      USE s_timst2
      USE s_opnfil
      USE s_suneff
      USE s_ypol
      USE s_exitrc
      USE s_vprod
      USE s_chksys
      USE s_gtsata
      USE s_opnerr
      USE s_maxtst
      USE s_timst2
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IANT  , IAOLD , IARC  , ICRARC, IDER  , IFIRST,
     1          IFMT  , IGTANT, II    , III   , IMAN  , IMANOK, IMIS  ,
     2          IND   , INT   , INTOLD, IORSY1, IORSYS, IOSTAT, IPOL  ,
     3          IQ    , IRC   , IRC1  , IRCODE, ISAANT, ISAT  , ISTOP ,
     4          K     , KI    , KK    , L     , MAXARC, MAXMAN, MAXPOL,
     5          METHOD, MXCARC, NARC  , NINT  , NLIN  , NMAN  , NMIS  ,
     6          NSAANT, NSAT
C
      REAL*8    DTBND1, DTBND2, DTBNDS, DTINT1, DTINT2, DUMMY , DUMMY1,
     1          DUMMY2, H     , REY   , RSAT  , T     , T0    , TDT   ,
     2          TOSC  , TOSC1 , TPOL
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(MAXARC=20,MAXPOL=15,MAXMAN=100)
C
      REAL*8       X(*),ELE(7),DUM3(3)
      REAL*8       TBOUND(MAXINT+1),ELESAT(7,MAXSAT),ANTOFF(6,MAXSAA)
      REAL*8       TFIRST(MAXARC),TLAST(MAXARC),Y(3*(MAXPOL+1)*MAXSAT)
      REAL*8       FAC(21),HELP(3*MAXSAT),TIMMAN(MAXMAN)
      REAL*8       EX(3),EY(3),EZ(3),XSUN(4),TIMINT(2,MAXSAA)
C
      INTEGER*4    SVN,SVNMAN,NAVNUM(MAXSAT),SATMAN(MAXMAN)
      INTEGER*4    SATANT(MAXSAA),SATBLK(MAXSAA),SENNUM(MAXSAA)
      INTEGER*4    SATMIS(MAXSAT*MAXARC),ARCMIS(MAXSAT*MAXARC)
C
      CHARACTER*80 LINE
      CHARACTER*32 FILOLD
      CHARACTER*20 STRING
      CHARACTER*19 TSTRNG
      CHARACTER*16 NUTNAM,SUBNAM
      CHARACTER*6  MXNARC
      CHARACTER*1  cmcchr
      CHARACTER(LEN=*)                                 :: FILSTD
      CHARACTER(LEN=STANAM2LENGTH),DIMENSION(2,MAXSAA) :: SATNAM
C
      LOGICAL      OPNFLG

      LOGICAL          , DIMENSION(2), OPTIONAL  :: cmcyn
      CHARACTER(LEN=16), DIMENSION(2), SAVE      :: tcmcmod
      LOGICAL          , DIMENSION(2), SAVE      :: tcmcyn
C
C COMMON BLOCKS
      COMMON/CGTORF/ TBOUND,ELESAT,Y,HELP
      COMMON/MCMARC/MXCARC,MXNARC
      DATA IFIRST/1/,IGTANT/1/,FILOLD/' '/
C
C DISREGARD MANOEUVRES IF SVN < 0
C -------------------------------
      IF (SVN.LT.0) THEN
        SVN=-SVN
        IMANOK=0
      ELSE
        IMANOK=1
      ENDIF
C
C GET SATELLITE ANTENNA OFFSETS (FIRST TIME WHEN ANTENNA OFFSETS NEEDED)
C ----------------------------------------------------------------------
      IF (IANT.NE.0.AND.IGTANT.EQ.1) THEN
        CALL GTSATA(MAXSAA,NSAANT,SATANT,ANTOFF,TIMINT,SATNAM,SATBLK,
     1                                                          SENNUM)
        IGTANT=0
      ENDIF
C
C GET RELEVANT ARC INFORMATION IF GETORF IS CALLED FOR THE FIRST TIME:
C -------------------------------------------------------------------
      IF(IFIRST.EQ.1) THEN
C
C CHECK LOCAL MAXIMUM DIMENSIONS
        CALL MAXTST(1,'GETORF',MXNARC,MAXARC,MXCARC,IRC1)
        IF(IRC1.NE.0) CALL EXITRC(2)
C
C MISSING SATELLITES
        NMIS=0
C
C GET SATELLITE MANOEUVRES
        CALL GTSATM(MAXMAN,NMAN,SATMAN,TIMMAN)
C
C COMPUTE FACTORIALS
        FAC(1)=1.D0
        DO 1 I=1,20
          FAC(I+1)=FAC(I)*I
1       CONTINUE
C
C EXTRAPOLATION INTERVAL AT ARC BOUNDARIES (1 SEC)
        DTBNDS=1.D0/86400.D0
        IFIRST=0
      ENDIF
C
      IF(FILSTD.NE.FILOLD) THEN
        FILOLD=FILSTD
        NUTNAM=' '
        SUBNAM=' '
        tcmcmod=' '
        cmcchr=' '
        tcmcyn =.FALSE.
        INQUIRE(UNIT=LFNOR1,OPENED=opnflg)
        IF (opnflg) CLOSE(UNIT=LFNOR1)
C
C OPEN STANDARD ORBIT FILE
        CALL OPNFIL(LFNOR1,FILSTD,'OLD','UNFORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNOR1,IOSTAT,FILSTD,'GETORF')
C
C READ NUMBER OF ARCS
        READ(LFNOR1) NARC
        IF (NARC.LT.0) THEN
          READ(LFNOR1) IFMT,NARC
          READ(LFNOR1) NLIN
          orbdsc%nlin=NLIN
          DO I=1,NLIN
            READ(LFNOR1) LINE
C Save orbit descriprion
            orbdsc%orbmod(I)=LINE
C
           IF (LINE(1:7).EQ.'NUTSUB:') THEN
!              READ(LINE,"(8X,A16,1X,A16)") NUTNAM,SUBNAM
              CALL CHKSYS(1,LINE,IRC)
            ELSEIF (LINE(1:7).EQ.'OTLOAD:') THEN
              READ(LINE,"(8X,A16,6X,A1)")tcmcmod(1),cmcchr
              IF (cmcchr == 'Y') tcmcyn(1) = .TRUE.
            ELSEIF (LINE(1:7).EQ.'ATLOAD:') THEN
              READ(LINE,"(8X,A16,6X,A1)")tcmcmod(2),cmcchr
              IF (cmcchr == 'Y') tcmcyn(2) = .TRUE.
            ENDIF
          ENDDO
        ELSE
C DEFAULT FOR OLD FORMAT
          LINE='NUTSUB: IAU80            RAY'
          CALL CHKSYS(1,LINE,IRC)
        ENDIF
        IF(NARC.GT.MAXARC)THEN
          WRITE(LFNERR,1002)NARC,MAXARC,FILSTD
1002      FORMAT(/,' *** SR GETORF: TOO MANY ARCS',/,
     1                         16X,'NUMBER OF ARCS:',I7,/,
     2                         16X,'MAXIMUM NUMBER:',I7,/,
     3                         16X,'FILE          : ',A,/)
          CALL EXITRC(2)
        END IF
C
C READ FIRST AND LAST OBSERVATION TIMES FOR ALL ARCS
C
C READ OSCULATION EPOCH AND INTERVAL BOUNDARIES
        DO 5 IARC=1,NARC
          READ(LFNOR1) NSAT,NINT,IQ,(NAVNUM(I),I=1,NSAT)
          IF(NSAT.GT.MAXSAT)THEN
            WRITE(LFNERR,1003)NSAT,MAXSAT,FILSTD
1003        FORMAT(/,' *** SR GETORF: TOO MANY SATELLITES',/,
     1                           16X,'NUMBER OF SATELLITES:',I3,/,
     2                           16X,'MAXIMUM NUMBER      :',I3,/,
     3                           16X,'FILE                : ',A,/)
            CALL EXITRC(2)
          END IF
          IF(NINT.GT.MAXINT)THEN
            WRITE(LFNERR,1004)NINT,MAXINT,FILSTD
1004        FORMAT(/,' *** SR GETORF: TOO MANY INTEGR. INTERVALS',/,
     1                           16X,'NUMBER OF INTERVALS:',I4,/,
     2                           16X,'MAXIMUM NUMBER     :',I4,/,
     3                           16X,'FILE               : ',A,/)
            CALL EXITRC(2)
          END IF
          IF(IQ.GT.MAXPOL)THEN
            WRITE(LFNERR,1005)IQ,MAXPOL,FILSTD
1005        FORMAT(/,' *** SR GETORF: POLYNOMIAL DEGREE TOO HIGH',/,
     1                           16X,'POLYNOMIAL DEGREE:',I3,/,
     2                           16X,'MAX. POLY. DEGREE:',I3,/,
     3                           16X,'FILE             : ',A,/)
            CALL EXITRC(2)
          END IF
          READ(LFNOR1) TOSC1,TBOUND(1)
          IF(TBOUND(1).EQ.0.D0) THEN
            IORSY1=1
            METHOD=2
          ELSE IF(TBOUND(1).EQ.2.D0) THEN
            IORSY1=2
            METHOD=2
          ELSE
            IORSY1=1
            METHOD=1
          ENDIF
          IF (METHOD.EQ.2) THEN
            DO 51 I=1,NINT+1
              READ(LFNOR1) TBOUND(I)
51          CONTINUE
          ELSE
C           BACKSPACE LFNOR1
C           READ(LFNOR1) TOSC1,(TBOUND(I),I=1,NINT+1)
            WRITE(LFNERR,904)
904         FORMAT(/,' *** SR GETORF: OLD BINARY FORMAT NO ',
     1                               'LONGER SUPPORTED',/,
     1                           16X,'REQUIRES "BACKSPACE"',/)
            CALL EXITRC(2)
          END IF
C
C SKIP REMAINING PART OF ARC
          DO 3 I=1,NSAT
            READ(LFNOR1) DUMMY
3         CONTINUE
          DO 4 INT=1,NINT
            READ(LFNOR1) DUMMY
            DO K=1,IQ+1
              READ(LFNOR1) DUMMY
            ENDDO
4         CONTINUE
          TFIRST(IARC)=TBOUND(1)
          TLAST(IARC)=TBOUND(NINT+1)
5       CONTINUE
        REWIND LFNOR1
        READ(LFNOR1) NARC
        IF (NARC.LT.0) THEN
          READ(LFNOR1) IFMT,NARC
          READ(LFNOR1) NLIN
          DO I=1,NLIN
            READ(LFNOR1) LINE
          ENDDO
        ENDIF
        READ(LFNOR1) NSAT,NINT,IQ,(NAVNUM(I),I=1,NSAT)
        IF(METHOD.EQ.1)THEN
          READ(LFNOR1) TOSC1,(TBOUND(I),I=1,NINT+1)
        ELSE
          READ(LFNOR1) TOSC1
          DO 52 I=1,NINT+1
            READ(LFNOR1) TBOUND(I)
52        CONTINUE
        END IF
        IAOLD=1
C
C READ SET OF OSCULATING ELEMENTS
        DO 10 ISAT=1,NSAT
          READ(LFNOR1) (ELESAT(K,ISAT),K=1,7)
10      CONTINUE
C
C READ SET OF POLYNOMIAL COEFFICIENTS FOR FIRST INTERVAL
C
C 1. T0 (ORIGIN OF DEVELOPMENT) AND H (INTERVAL LENGTH)
        READ(LFNOR1) T0,H
        DO 20 I=1,IQ+1
          READ(LFNOR1) (HELP(K),K=1,3*NSAT)
          DO K=1,NSAT
            IND=3*(K-1)*(IQ+1)+3*(I-1)
            DO L=1,3
              Y(IND+L)=HELP(3*(K-1)+L)
            ENDDO
          ENDDO
20      CONTINUE
        INTOLD=1
      END IF
C END OF INITIALIZATION
C -------------------------------------------------------------------
C
      IF (PRESENT(cmcyn)) THEN
        cmcyn=tcmcyn
        CALL chkcmc(cmcyn,tcmcmod)
      ENDIF
C
C IS TIME ARGUMENT IN ONE OF THE ARCS ?
      DO 30 IARC=1,NARC
C
C ALLOW FOR EXTRAPOLATION AT ARC BOUNDARIES
        IF (IARC.EQ.1) THEN
          DTBND1=DTBNDS
        ELSE IF (TFIRST(IARC)-TLAST(IARC-1).GE.2*DTBNDS) THEN
          DTBND1=DTBNDS
        ELSE
          DTBND1=0.D0
        ENDIF
        IF (IARC.EQ.NARC) THEN
          DTBND2=DTBNDS
        ELSE IF (TFIRST(IARC+1)-TLAST(IARC).GE.2*DTBNDS) THEN
          DTBND2=DTBNDS
        ELSE
          DTBND2=0.D0
        ENDIF
C
        IF (T.GE.TFIRST(IARC)-DTBND1 .AND.
     1      T.LE.TLAST (IARC)+DTBND2 )      GOTO 40
30    CONTINUE
C
C NO SUITABLE ARC FOUND
      IF (ISTOP.EQ.0) THEN
        ICRARC=IAOLD
        IRCODE=1
        GOTO 999
      ELSE
        CALL TIMST2(1,1,T,TSTRNG)
        WRITE(LFNERR,31) TSTRNG,FILSTD
31      FORMAT(/,' *** SR GETORF: NO ARC FOUND',/,
     1                       16X,'TIME: ',A,/,
     2                       16X,'FILE: ',A,/)
        CALL EXITRC(2)
      ENDIF
C
C IS TIME T IN AN ARC BEFORE CURRENT ARC?
C -------------------------------------------------------------------
40    IF(IARC.LT.IAOLD)THEN
        INQUIRE(UNIT=LFNOR1,OPENED=opnflg)
        IF (opnflg) CLOSE(UNIT=LFNOR1)
C
C OPEN STANDARD ORBIT FILE
        CALL OPNFIL(LFNOR1,FILSTD,'OLD','UNFORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNOR1,IOSTAT,FILSTD,'GETORF')
        READ(LFNOR1) NARC
        IF (NARC.LT.0) THEN
          READ(LFNOR1) IFMT,NARC
          READ(LFNOR1) NLIN
          DO I=1,NLIN
            READ(LFNOR1) LINE
          ENDDO
        ENDIF
C
C SKIP FIRST IARC-1 ARCS
        DO 50 III=1,IARC-1
          READ(LFNOR1) NSAT,NINT,IQ
          IF(METHOD.EQ.1)THEN
            READ(LFNOR1) DUMMY
          ELSE
            DO 53 I=1,NINT+2
              READ(LFNOR1) DUMMY
53          CONTINUE
          END IF
C
C SKIP REMAINING PART OF ARC
            DO 43 I=1,NSAT
              READ(LFNOR1) DUMMY
43          CONTINUE
            DO 44 KK=1,NINT
              READ(LFNOR1) DUMMY
              DO K=1,IQ+1
                READ(LFNOR1) DUMMY
              ENDDO
44          CONTINUE
50      CONTINUE
C
C READ FIRST TWO RECORDS OF NEW ARC
        READ(LFNOR1) NSAT,NINT,IQ,(NAVNUM(I),I=1,NSAT)
        IF(METHOD.EQ.1)THEN
          READ(LFNOR1) TOSC1,(TBOUND(I),I=1,NINT+1)
        ELSE
          READ(LFNOR1) TOSC1
          DO 54 I=1,NINT+1
            READ(LFNOR1) TBOUND(I)
54        CONTINUE
        END IF
C
C READ SET OF OSCULATING ELEMENTS
        DO 60 ISAT=1,NSAT
          READ(LFNOR1) (ELESAT(K,ISAT),K=1,7)
60      CONTINUE
C
C INITIALIZE CURRENT INTERVAL
        INTOLD=0
      END IF
C -------------------------------------------------------------------
C
C IS TIME T IN AN ARC AFTER CURRENT ARC ?
C -------------------------------------------------------------------
      IF(IARC.GT.IAOLD)THEN
C
C SKIP REMAINING PART OF CURRENT ARC
        DO 70 I=INTOLD+1,NINT
          READ(LFNOR1) DUMMY1,DUMMY2
          DO K=1,IQ+1
            READ(LFNOR1) DUMMY
          ENDDO
70      CONTINUE
C
C SKIP ARCS BETWEEN CURRENT AND NEW ARC
        DO 80 I=IAOLD+1,IARC-1
          READ(LFNOR1) NSAT,NINT,IQ
          IF(METHOD.EQ.1)THEN
            READ(LFNOR1) DUMMY
          ELSE
            DO 55 KI=1,NINT+2
              READ(LFNOR1) DUMMY
55          CONTINUE
          END IF
C
C SKIP REMAINING PART OF ARC
            DO 73 II=1,NSAT
              READ(LFNOR1) DUMMY
73          CONTINUE
            DO 74 INT=1,NINT
              READ(LFNOR1) DUMMY
              DO K=1,IQ+1
                READ(LFNOR1) DUMMY
              ENDDO
74          CONTINUE
80      CONTINUE
C
C READ FIRST TWO RECORDS OF NEW ARC
        READ(LFNOR1) NSAT,NINT,IQ,(NAVNUM(I),I=1,NSAT)
        IF(METHOD.EQ.1)THEN
          READ(LFNOR1) TOSC1,(TBOUND(I),I=1,NINT+1)
        ELSE
          READ(LFNOR1) TOSC1
          DO 56 I=1,NINT+1
            READ(LFNOR1) TBOUND(I)
56        CONTINUE
        END IF
C
C READ SET OF OSCULATING ELEMENTS
        DO 90 ISAT=1,NSAT
          READ(LFNOR1) (ELESAT(K,ISAT),K=1,7)
90      CONTINUE
C
C INITIALIZE INTERVAL
        INTOLD=0
      END IF
C -------------------------------------------------------------------
C
C NEW ARC EQUALS OLD ARC
C -------------------------------------------------------------------
C
C DETECT SUBINTERVAL
      DO 100 INT=1,NINT
        IF (INT.EQ.1) THEN
          DTINT1=DTBND1
        ELSE
          DTINT1=0.D0
        ENDIF
        IF (INT.EQ.NINT) THEN
          DTINT2=DTBND2
        ELSE
          DTINT2=0.D0
        ENDIF
        IF(T.GE.TBOUND(INT  )-DTINT1 .AND.
     1     T.LE.TBOUND(INT+1)+DTINT2 )     GOTO 110
100   CONTINUE
C -------------------------------------------------------------------
C
C READ NEW COEFFICIENTS IF INT NOT EQUAL INTOLD
C -------------------------------------------------------------------
110   IF(INTOLD.EQ.INT)GO TO 140
C
C IF INT < INTOLD, START READING FILE FROM BEGINNING
      IF(INT.LT.INTOLD) THEN
        IAOLD=IARC+1
        GOTO 40
      ENDIF
C -------------------------------------------------------------------
C
C SKIP INTERVALS INTOLD+1,INTOLD+2,..,INT-1
C -------------------------------------------------------------------
C
C SKIP REMAINING PART OF CURRENT INTERVAL
        DO 120 I=INTOLD+1,INT-1
          READ(LFNOR1) DUMMY
          DO K=1,IQ+1
            READ(LFNOR1) DUMMY
          ENDDO
120     CONTINUE
C
C READ SET OF POLYNOMIAL COEFFICIENTS FOR NEW INTERVAL
C
C 1. T0 (ORIGIN OF DEVELOPMENT) AND H (INTERVAL LENGTH)
        READ(LFNOR1) T0,H
        DO 130 I=1,IQ+1
          READ(LFNOR1) (HELP(K),K=1,3*NSAT)
          DO K=1,NSAT
            IND=3*(K-1)*(IQ+1)+3*(I-1)
            DO L=1,3
              Y(IND+L)=HELP(3*(K-1)+L)
            ENDDO
          ENDDO
130     CONTINUE
C -------------------------------------------------------------------
C
C COMPUTATION OF POSITION, VELOCITY, .. AND DEFINE PROPER SET OF
C                                           ELEMENTS
C -------------------------------------------------------------------
140     CONTINUE
C
C SATELLITE AFTER MANOEUVRE: SVN=SVN+50
        SVNMAN=SVN
        IF (IMANOK.EQ.1) THEN
          DO 145 IMAN=1,NMAN
            IF (SATMAN(IMAN).EQ.SVNMAN.AND.
     1          TIMMAN(IMAN).GT.TFIRST(IARC)-DTBND1.AND.
     2          TIMMAN(IMAN).LE.T) THEN
              SVNMAN=SVNMAN+50
              GOTO 147
            ENDIF
145       CONTINUE
147       CONTINUE
        ENDIF
C
C DETECT SATELLITE NUMBER
        DO 150 ISAT=1,NSAT
          IF(SVNMAN.EQ.NAVNUM(ISAT))GO TO 160
150     CONTINUE
C
C SATELLITE NOT FOUND
        IF (ISTOP.EQ.0) THEN
          ICRARC=IARC
          IAOLD=IARC
          INTOLD=INT
          IRCODE=2
          GOTO 999
        ELSEIF (ISTOP.EQ.1) THEN
          WRITE(LFNERR,151) SVNMAN,IARC,FILSTD
151       FORMAT(/,' *** SR GETORF: SATELLITE NOT FOUND',/,
     1                         16X,'SATELLITE:',I3,/,
     2                         16X,'ARC      :',I3,/,
     3                         16X,'FILE     : ',A,/)
          CALL EXITRC(2)
        ELSE
          DO 152 IMIS=1,NMIS
            IF (SATMIS(IMIS).EQ.SVNMAN .AND.
     1          ARCMIS(IMIS).EQ.IARC)  GOTO 154
152       CONTINUE
C
          NMIS=NMIS+1
          SATMIS(NMIS)=SVNMAN
          ARCMIS(NMIS)=IARC
          WRITE(LFNERR,153) SVNMAN,IARC,FILSTD
153       FORMAT(/,' ### SR GETORF: SATELLITE NOT FOUND',/,
     1                         16X,'SATELLITE:',I3,/,
     2                         16X,'ARC      :',I3,/,
     3                         16X,'FILE     : ',A,/)
154       ICRARC=IARC
          IAOLD=IARC
          INTOLD=INT
          IRCODE=2
          GOTO 999
        ENDIF
C
160     TPOL=(T-T0)*86400.D0
        IPOL=1+3*(ISAT-1)*(IQ+1)
ccc     IF(DABS(TPOL).LT.4.D-5)TPOL=0.D0
        CALL YPOL(IDER,IQ,3,H,FAC,TPOL,Y(IPOL),X)
        IAOLD=IARC
        INTOLD=INT
C
C SATELLITE ANTENNA OFFSET CORRECTIONS
C ------------------------------------
        IF (IANT.NE.0) THEN
          DO 162 ISAANT=1,NSAANT
            IF (SATANT(ISAANT).EQ.SVN. AND. T.GE.TIMINT(1,ISAANT)
     1          .AND.T.LE.TIMINT(2,ISAANT)) GOTO 163
162       CONTINUE
C
C SATELLITE ANTENNA OFFSET NOT FOUND
          CALL TIMST2(1,1,T,STRING)
          WRITE(LFNERR,1006) SVN,STRING
1006      FORMAT(/,' *** SR GETORF: SATELLITE ANTENNA OFFSET',
     1             ' NOT FOUND',/,
     2             16X,'IN SATELLITE INFORMATION FILE',/,
     3             16X,'SATELLITE NUMBER:',I4,/,
     4             16X,'EPOCH           : ',A20,/)
          CALL EXITRC(2)
163       CONTINUE
C
C UNIT VECTOR EZ
          RSAT=DSQRT(X(1)**2+X(2)**2+X(3)**2)
          DO 164 K=1,3
            EZ(K)=-X(K)/RSAT
164       CONTINUE
C
C UNIT VECTOR EY
          TDT=T+(19.D0+32.184D0)/86400.D0
          CALL SUNEFF(IORSY1,2.D0,TDT,XSUN,DUM3)
          CALL VPROD(EZ,XSUN,EY)
          REY=DSQRT(EY(1)**2+EY(2)**2+EY(3)**2)
          DO 165 K=1,3
            EY(K)=EY(K)/REY
165       CONTINUE
C
C UNIT VECTOR EX
          CALL VPROD(EY,EZ,EX)
C
C ANTENNA OFFSET CORRECTION
          DO 161 K=1,3
            IF(IANT.EQ.1) THEN
              X(K)=X(K)+EX(K)*ANTOFF(1,ISAANT)
     1                 +EY(K)*ANTOFF(2,ISAANT)
     2                 +EZ(K)*ANTOFF(3,ISAANT)
C
C SLR REFLECTORS
            ELSEIF(IANT.EQ.2) THEN
              X(K)=X(K)+EX(K)*ANTOFF(4,ISAANT)
     1                 +EY(K)*ANTOFF(5,ISAANT)
     2                 +EZ(K)*ANTOFF(6,ISAANT)
            END IF
161       CONTINUE
        ENDIF
C
C DEFINE PROPER SET OF ELEMENTS
        TOSC=TOSC1
        DO 170 K=1,7
          ELE(K)=ELESAT(K,ISAT)
170     CONTINUE
C
C SET RETURN CODE
        ICRARC=IARC
        IRCODE=0
C
999     CONTINUE
        IORSYS=IORSY1
C
        RETURN
        END SUBROUTINE

      END MODULE
