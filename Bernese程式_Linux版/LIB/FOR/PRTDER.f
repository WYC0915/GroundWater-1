      MODULE s_PRTDER
      CONTAINS

C*
      SUBROUTINE PRTDER(FILNAM,SVN,IPAR,IDER,ISTOP,T,NMXINT,ICRARC,
     1                  IORSYS,NVAR,NRAD,X,ELESAT,RPRPAR,ANLTYP,IRCODE,
     2                  NSTCA,FRCTYPA,NSTCEPA,INTSTCA,TIMSTCA,
     3                  PARSTCA)
CC
CC NAME       :  PRTDER
CC
CC PURPOSE    :  COMPUTE PARTIAL DERIVATIVE (AND ITS TIME DERIVATIVE)
CC               OF SATELLITE "SVN". STORE RESULT IN ARRAY "X".
CC               IN ADDITION THE NUMBER OF STORED DERIVATIVES "NVAR",
CC               THE NUMBER OF RADIATION PRESSURE PARAMETERS "NRAD",
CC               THE ORBITAL ELEMENTS, THE RADIATION PRESSURE VALUES,
CC               THE OSCULATION EPOCH ARE RETURNED.
CC               THE OLD AND THE NEW ORBIT MODEL ARE ACCOMODATED.
CC               *** BASED ON FORMER SUBROUTINE GETRPR ***
CC
CC PARAMETERS :
CC         IN :  FILNAM : FILE-NAME WITH PARTIAL DERIVATIVES  CH*32
CC                        IF FILNAM=' ', THE DEFAULT NAME
CC                        WITH INTERNAL NAME "RPRCOE" IS USED.
CC               SVN    : SVN-NUMBER OF SATELLITE             I*4
CC               IPAR   : PARAMETER NUMBER                    I*4
CC                        =1 : SEMIMAJOR AXIS
CC                        =2 : ECCENTRICITY
CC                        =3 : INCLINATION
CC                        =4 : R.A. OF ASCENDING NODE
CC                        =5 : PERIGEE
CC                        =6 : ARG. OF LATITUDE
CC                        =7 : DIRECT RAD.PRESS.COEFFICIENT
CC                        =8 : Y-BIAS
CC                        =9 : X-BIAS
CC                        =10: DIRECT RAD.PRESS.COEFFICIENT (COS-TERM)
CC                        =11: Y-BIAS (COS-TERM)
CC                        =12: X-BIAS (COS-TERM)
CC                        =13: DIRECT RAD.PRESS.COEFFICIENT (SIN-TERM)
CC                        =14: Y-BIAS (SIN-TERM)
CC                        =15: X-BIAS (SIN-TERM)
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
CC               NMXINT : NUMBER OF INTEGR. INTERVALS         I*4
CC                        NECESSARY FOR OPTIONAL ARGUMENTS
CC        OUT :  ICRARC : CURRENT ARC NUMBER                  I*4
CC               IORSYS : ORBIT SYSTEM                        I*4
CC                        =1 : B1950.0
CC                        =2 : J2000.0
CC               NVAR   : NUMBER OF VARIATIONAL EQUATIONS     I*4
CC               NRAD   : NUMBER OF RADIATION PRESSURE PARMS  I*4
CC               X(I),I=1,2,3, 4,5,6, ... PARTIAL,            R*8
CC                        FIRST DERIVATIVE, ...
CC               ELESAT : ORBITAL ELEMENTS FOR REQUESTED SAT  R*8
CC               RPRPAR : RADIATION PRESSURE PARAMETERS       R*8
CC               IRCODE : RETURN CODE                         I*4
CC                        =0: OK
CC                        =1: NO ARC FOUND FOR TIME "T"
CC                        =2: ARC FOUND, BUT NO SATELLITE
CC                            "SVN" IN THIS ARC
CC               NSTCA  : NUMBER OF STOCH. EPOCHS (OPTIONAL)  I*4
CC               FRCTYPA: PARAMETER TYPES (OPTIONAL)          I*4
CC               NSTCEPA: NUMBER OF PARAMETERS PER EPOCH      I*4
CC                        (OPTIONAL)
CC               INTSTCA: INTERVAL NUMBERS OF STOCHASTIC      I*4
CC                         PERTURBATIONS (OPTIONAL)
CC               TIMSTCA: EPOCHS WITH STOCH. PERTURBATIONS    R*8
CC                        (OPTIONAL)
CC               PARSTCA: STOCHASTIC PARAMETERS               R*8
CC                        (OPTIONAL)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/02 11:34
CC
CC CHANGES    :  05-MAI-92 : ??: MAXINT=200 (OLD MAXINT=100)
CC               04-JUN-92 : ??: ALLOW FOR A SMALL EXTRAPOLATION AT ARC
CC                               BOUNDARIES; NEW PARAMETER LIST AND
CC                               RETURN CODE
CC               15-JUN-92 : ??: SATELLITE MANOEUVRES: RETURN CORRECT
CC                               SATELLITE POSITIONS (SVN OR SVN+50)
CC               28-OCT-93 : ??: OPTION ISTOP=2
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               14-JUN-94 : RW: CORRECT INDEX SETTING: PARAM(3,MAXSAT)
CC               29-JUN-94 : ??: CORRECT ISTOP=2 OPTION
CC               10-AUG-94 : MR: CALL EXITRC
CC               17-AUG-94 : MR: MAXMAN=100 (OLD: MAXMAN=20)
CC               02-JAN-95 : GB: NEW ORBIT MODEL. PARTIAL DERIVATIVES
CC                               W.R.T. ORBITAL ELEMENTS INCLUDED;
CC                               NEW PARAMETERS IN SUBROUTINE STATEMENT.
CC                               OLD ORBIT FORMAT IS ACCOMODATED, TOO.
CC                               *** SR RENAMED FROM GET RPR TO PRTDER ***
CC               23-SEP-97 : DI: USE MAXSAT.inc
CC               22-JAN-00 : HU: BACKSPACE REMOVED, NO LONGER SUPPORTED
CC               22-JAN-03 : HU: HANDLING OF OVERFLOW TRANSFERRED TO YPOL
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               07-MAR-03 : HU: MAXINT MOVED TO M_MAXDIM
CC               18-AUG-03 : RD: CLOSE FILE BEFORE OPEN THE NEXT ONE
CC               24-NOV-03 : HU: GTFLNA OUTSIDE IF STRUCTURE
CC               23-NOV-04 : AJ: MAXVAR=15 -> 18
CC               05-APR-05 : AJ: READING STOCHASTIC PARAMETERS
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               23-JUL-05 : HU: MISSING DECLARATIONS ADDED
CC               26-FEB-08 : RD: USE GTSATM FROM D_SATCRX
CC               24-Sep-08 : DT: Use maxVar from M_MAXDIM
CC               03-DEC-10 : HB: ADD PARAMETER NMXINT FOR OPTIONAL
CC                               ARGUMENTS
CC               26-MAR-12 : RD: SWITCH FROM TIMSTR TO TIMST2
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: i4b,r8b,lfnerr,lfnrpr
      USE m_maxdim, ONLY: MAXSAT,MAXINT, maxVar
      USE d_satcrx, ONLY: gtsatm
      USE s_opnfil
      USE s_opnerr
      USE s_maxtst
      USE s_timst2
      USE s_ypol
      USE s_exitrc
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I0    , IAOLD , IARC  , ICRARC, IDER  , II    ,
     1          III   , IMAN  , IMIS  , INT   , INTOLD, IORSY1, IORSYS,
     2          IOSTAT, IPAR  , IPOL  , IQ    , IRC   , IRC1  , IRC2  ,
     3          IRC3  , IRCODE, ISAT  , ISTOP , K     , KI    , L     ,
     4          MAXARC, MAXMAN, MAXPOL, METHOD, MXCARC, MXCSAT, NMXINT,
     5          MXCVAR, NARC  , NEWOLD, NINT  , NMAN  , NMIS  , NRAD  ,
     6          NSAT  , NVAR  , NVSAV , LFORM , ISTC  , IFRC
C
      REAL*8    DTBND1, DTBND2, DTBNDS, DTINT1, DTINT2, DUMMY , H     ,
     1          T     , T0    , T1    , T2    , TB1   , TB2   , TPOL
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MAXARC=20,MAXPOL=15,MAXMAN=100)
C
      REAL*8       X(*),RPRPAR(*),ELESAT(*)
      REAL*8       PARAM(MAXVAR,MAXSAT),TFIRST(MAXARC),TLAST(MAXARC)
      REAL*8       Y(3*MAXVAR*(MAXPOL+1)*MAXSAT)
      REAL*8       FAC(21),TBOUND(MAXINT+1),TIMMAN(MAXMAN)
      REAL*8       SCALPA(MAXVAR)
      REAL*8       TIMSTC(MAXINT,MAXSAT),PARSTC(3,MAXINT,MAXSAT)
C
      INTEGER*4    TOTREC,SVN,SVNMAN,NAVNUM(MAXSAT),SATMAN(MAXMAN)
      INTEGER*4    SATMIS(MAXSAT*MAXARC),ARCMIS(MAXSAT*MAXARC)
      INTEGER*4    LOCQ(6,MAXVAR),SKPSTC(MAXARC)
      INTEGER*4    FRCTYP(3,MAXINT,MAXSAT),NSTC(MAXSAT)
      INTEGER*4    NSTCEP(MAXINT,MAXSAT),INTSTC(MAXINT,MAXSAT)
C
      INTEGER(i4b),                    OPTIONAL      :: NSTCA
      INTEGER(i4b),DIMENSION(3,NMXINT),     OPTIONAL      :: FRCTYPA
      INTEGER(i4b),DIMENSION(NMXINT),       OPTIONAL      :: NSTCEPA
      INTEGER(i4b),DIMENSION(NMXINT),       OPTIONAL      :: INTSTCA
C
      REAL(r8b),DIMENSION(NMXINT),          OPTIONAL      :: TIMSTCA
      REAL(r8b),DIMENSION(3,NMXINT),        OPTIONAL      :: PARSTCA
C
      CHARACTER*32 FILRPR,FILNAM,OLDNAM
      CHARACTER*2  LINE1
      CHARACTER*19 TSTRNG
      CHARACTER*8  ANLTYP,ANLSAV
      CHARACTER*6  MXNARC,MXNSAT,MXNVAR
      CHARACTER*1  SOURCE(10)
C
      COMMON/CGTRAD/PARAM,Y,TBOUND
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMARC/MXCARC,MXNARC
      COMMON/MCMVAR/MXCVAR,MXNVAR
      DATA OLDNAM/'-1                              '/
C
      IF(FILNAM(1:1).EQ.' ')THEN
        CALL GTFLNA(1,'RPRCOE ',FILRPR,IRC)
      ELSE
        FILRPR=FILNAM
      END IF
C
C GET RELEVANT ARC INFORMATION IF PRTDER IS CALLED FOR THE FIRST TIME:
C -------------------------------------------------------------------
      IF(FILRPR.NE.OLDNAM) THEN
C
C CHECK LOCAL MAXIMUM DIMENSIONS
        CALL MAXTST(1,'PRTDER',MXNSAT,MAXSAT,MXCSAT,IRC1)
        CALL MAXTST(1,'PRTDER',MXNARC,MAXARC,MXCARC,IRC2)
        CALL MAXTST(0,'PRTDER',MXNVAR,MAXVAR,MXCVAR,IRC3)
        IF(IRC1.NE.0.OR.IRC2.NE.0.OR.IRC3.NE.0) CALL EXITRC(2)
C
C MISSING SATELLITES
        NMIS=0
C
C GET SATELLITE MANOEUVRES
        CALL GTSATM(MAXMAN,NMAN,SATMAN,TIMMAN)
C
        IF (OLDNAM.NE.'-1') CLOSE(LFNRPR)
C
        OLDNAM=FILRPR
C
        CALL OPNFIL(LFNRPR,FILRPR,'OLD','UNFORMATTED',
     1              'READONLY',' ',IOSTAT)
        CALL OPNERR(LFNERR,LFNRPR,IOSTAT,FILRPR,'PRTDER')
C
C COMPUTE FACTORIALS
        FAC(1)=1.D0
        DO 1 I=1,20
          FAC(I+1)=FAC(I)*I
1       CONTINUE
C
C EXTRAPOLATION INTERVAL AT ARC BOUNDARIES (1 SEC)
        DTBNDS=1.D0/86400.D0
C
C READ FORMAT
        REWIND LFNRPR
        READ(LFNRPR) LINE1
        IF(LINE1.EQ.'#P') THEN
          LFORM=1
        ELSE
          LFORM=0
          REWIND LFNRPR
        END IF
C
C READ NUMBER OF ARCS
        READ(LFNRPR)NARC
        IF(NARC.GT.MAXARC) THEN
          WRITE(LFNERR,1002) NARC,MAXARC
1002      FORMAT(/,' *** SR PRTDER: TOO MANY ARCS',/,
     1                         16X,'NUMBER OF ARCS:',I3,/,
     2                         16X,'MAXIMUM NUMBER:',I3,/)
          CALL EXITRC(2)
        ENDIF
C
C READ FIRST AND LAST OBSERVATION TIMES FOR ALL ARCS
C --------------------------------------------------
        DO 5 IARC=1,NARC
          READ(LFNRPR)NSAT,NINT,IQ,(NAVNUM(I),I=1,NSAT),
     1                (SOURCE(I),I=1,10)
          IF(NSAT.GT.MAXSAT) THEN
            WRITE(LFNERR,1003) NSAT,MAXSAT
1003        FORMAT(/,' *** SR PRTDER: TOO MANY SATELLITES',/,
     1                           16X,'NUMBER OF SATELLITES:',I3,/,
     2                           16X,'MAXIMUM NUMBER      :',I3,/)
            CALL EXITRC(2)
          ENDIF
          IF(NINT.GT.MAXINT) THEN
            WRITE(LFNERR,1004) NINT,MAXINT
1004        FORMAT(/,' *** SR PRTDER: TOO MANY INTEGR. INTERVALS',/,
     1                           16X,'NUMBER OF INTERVALS:',I3,/,
     2                           16X,'MAXIMUM NUMBER     :',I3,/)
            CALL EXITRC(2)
          ENDIF
          IF(IQ.GT.MAXPOL) THEN
            WRITE(LFNERR,1005) IQ,MAXPOL
1005        FORMAT(/,' *** SR PRTDER: POLYNOMIAL DEGREE TOO HIGH',/,
     1                           16X,'POLYNOMIAL DEGREE:',I3,/,
     2                           16X,'MAX. POLY. DEGREE:',I3,/)
            CALL EXITRC(2)
          ENDIF
          READ(LFNRPR) TFIRST(IARC),TLAST(IARC),TBOUND(1)
C
C NEW/OLD ORBIT MODEL (NEWOLD=1:NEW MODEL, NEWOLD=0: OLD MODEL)
          IF(TBOUND(1).GE.10.D0)THEN
            NEWOLD=1
            TBOUND(1)=TBOUND(1)-10.D0
          ELSE
            NEWOLD=0
          END IF
C
C ORBIT SYSTEM, FILE TYPE
C -----------------------
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
C
C READ NUMBER OF VARIATIONAL EQNS FOR NEW MODEL
C ---------------------------------------------
          IF(NEWOLD.EQ.1)THEN
            READ(LFNRPR)NVAR,ANLTYP
            IF(NVAR.GT.MAXVAR)THEN
              WRITE(LFNERR,6010)NVAR,MXCVAR
6010          FORMAT(//,' ** SR PRTDER: NVAR (IN FILE) (',I3,
     1               ') GT MAXVAR=',I3,//)
              CALL EXITRC(2)
            END IF
          ELSE
            NVAR=3
            ANLTYP='OLD'
          END IF
C
C SAVE NVAR, ANLTYP AS PURELY INTERNAL VARIABLES
C ----------------------------------------------
          NVSAV=NVAR
          ANLSAV=ANLTYP
C
C READ BOUNDARIES OF INTERVALS
C ----------------------------
          IF (METHOD.EQ.2) THEN
            DO 51 I=1,NINT+1
              READ(LFNRPR) TBOUND(I)
51          CONTINUE
          ELSE
C           BACKSPACE LFNRPR
C           READ(LFNRPR) T1,T2,(TBOUND(I),I=1,NINT+1)
            WRITE(LFNERR,904)
904         FORMAT(/,' *** SR PRTDER: OLD BINARY FORMAT NO ',
     1                               'LONGER SUPPORTED',/,
     1                           16X,'REQUIRES "BACKSPACE"',/)
            CALL EXITRC(2)
          END IF
          TFIRST(IARC)=TBOUND(1)
          TLAST(IARC)=TBOUND(NINT+1)
C
C SKIP REMAINING PART OF ARC
          SKPSTC(IARC)=0
          IF(NEWOLD.EQ.0)THEN
            TOTREC=NSAT+(1+3*(IQ+1)*NSAT)*NINT
          ELSE
            TOTREC=NVAR*NSAT
            DO 2 I=1,TOTREC
              READ(LFNRPR)DUMMY
2           CONTINUE
            IF(LFORM.EQ.1) THEN
              DO 303 ISAT=1,NSAT
                READ(LFNRPR) NSTC(ISAT)
                SKPSTC(IARC)=SKPSTC(IARC)+1
                DO 302 ISTC=1,NSTC(ISAT)
                  READ(LFNRPR) FRCTYP(1,ISTC,ISAT),NSTCEP(ISTC,ISAT),
     1                         DUMMY
                  SKPSTC(IARC)=SKPSTC(IARC)+1
                  DO 301 IFRC=2,NSTCEP(ISTC,ISAT)
                    READ(LFNRPR)DUMMY
                    SKPSTC(IARC)=SKPSTC(IARC)+1
301               CONTINUE
302             CONTINUE
303           CONTINUE
            END IF
            TOTREC=(1+NVAR*(IQ+1)*NSAT)*NINT
          END IF
          DO 3 I=1,TOTREC
            READ(LFNRPR)DUMMY
3         CONTINUE

5       CONTINUE
C --------------------------------------------------
C
C READ INFORMATION FOR FIRST INTERVAL OF FIRST ARC
C --------------------------------------------------
        REWIND LFNRPR
        IF(LFORM.EQ.1) READ(LFNRPR)LINE1
        READ(LFNRPR)NARC
        READ(LFNRPR)NSAT,NINT,IQ,(NAVNUM(I),I=1,NSAT),
     1              (SOURCE(I),I=1,10)
        IF(METHOD.EQ.1) THEN
          READ(LFNRPR)T1,T2,(TBOUND(I),I=1,NINT+1)
        ELSE
          READ(LFNRPR) T1,T2
          IF(NEWOLD.EQ.1)THEN
            READ(LFNRPR)NVAR
          END IF
          DO 52 I=1,NINT+1
            READ(LFNRPR) TBOUND(I)
52        CONTINUE
        ENDIF
        IAOLD=1
C
C READ SET OF RADIATION PRESSURE COEFFICIENTS
        DO 10 ISAT=1,NSAT
          IF(NEWOLD.EQ.0)THEN
            READ(LFNRPR)(PARAM(K,ISAT),K=1,3)
          ELSE
            DO 9 K=1,NVAR
              READ(LFNRPR)PARAM(K,ISAT),SCALPA(K),(LOCQ(L,K),L=1,6)
9           CONTINUE
          END IF
10      CONTINUE
C
C READ STOCHASTIC PULSES
        IF(LFORM.EQ.1) THEN
          DO 503 ISAT=1,NSAT
            READ(LFNRPR) NSTC(ISAT)
            DO 502 ISTC=1,NSTC(ISAT)
              IFRC=0
              DO 501
                IFRC=IFRC+1
                READ(LFNRPR) FRCTYP(IFRC,ISTC,ISAT),
     1                       NSTCEP(ISTC,ISAT),INTSTC(ISTC,ISAT),
     2                       TIMSTC(ISTC,ISAT),PARSTC(IFRC,ISTC,ISAT)
                IF(IFRC.EQ.NSTCEP(ISTC,ISAT)) GOTO 502
501           CONTINUE
502         CONTINUE
503       CONTINUE
        END IF
C
C READ SET OF POLYNOMIAL COEFFICIENTS FOR FIRST INTERVAL
C
C 1. T0 (ORIGIN OF DEVELOPMENT) AND H (INTERVAL LENGTH)
        READ(LFNRPR)T0,H
        I0=0
        TOTREC=NVAR*(IQ+1)*NSAT
        DO 20 I=1,TOTREC
          READ(LFNRPR)(Y(I0+K),K=1,3)
          I0=I0+3
20      CONTINUE
        INTOLD=1
C --------------------------------------------------
      ELSE
        NVAR=NVSAV
        ANLTYP=ANLSAV
      END IF
C
C END OF INITIALIZATION
C -------------------------------------------------------------------
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
        WRITE(LFNERR,31) TSTRNG
31      FORMAT(/,' *** SR PRTDER: NO ARC FOUND',/,
     1                       16X,'TIME: ',A,/)
        CALL EXITRC(2)
      ENDIF
C
C IS TIME T IN AN ARC BEFORE CURRENT ARC?
C -------------------------------------------------------------------
40    IF(IARC.LT.IAOLD)THEN
C
C T IS IN ARC BEFORE CURRENT ARC: REWIND,
C SKIP FIRST IARC-1 ARCS, READ NEW RPR COEFFICIENTS
C -------------------------------------------------
        REWIND LFNRPR
        IF(LFORM.EQ.1) READ(LFNRPR) LINE1
        READ(LFNRPR)NARC
        DO 50 III=1,IARC-1
          READ(LFNRPR)NSAT,NINT,IQ
          IF(METHOD.EQ.1) THEN
            READ(LFNRPR)T1,T2
          ELSE
            READ(LFNRPR)T1,T2
            DO 53 I=1,NINT+1
              READ(LFNRPR)DUMMY
53          CONTINUE
          ENDIF
          IF(NEWOLD.EQ.0)THEN
            TOTREC=NSAT+(1+3*NSAT*(IQ+1))*NINT
          ELSE
            IF(LFORM.EQ.1) THEN
              TOTREC=NSAT*NVAR+SKPSTC(III)+(1+NVAR*NSAT*(IQ+1))*NINT
            ELSEIF(LFORM.EQ.0) THEN
              TOTREC=NSAT*NVAR+(1+NVAR*NSAT*(IQ+1))*NINT
            END IF
          END IF
C
C SKIP REMAINING PART OF ARC
          DO 43 I=1,TOTREC
            READ(LFNRPR)DUMMY
43        CONTINUE
50      CONTINUE
C
C READ FIRST TWO RECORDS OF NEW ARC
        READ(LFNRPR)NSAT,NINT,IQ,(NAVNUM(I),I=1,NSAT)
        IF(METHOD.EQ.1) THEN
          READ(LFNRPR)TB1,TB2,(TBOUND(I),I=1,NINT+1)
        ELSE
          READ(LFNRPR) TB1,TB2
          IF(NEWOLD.EQ.1)THEN
            READ(LFNRPR)NVAR
          END IF
          DO 54 I=1,NINT+1
            READ(LFNRPR) TBOUND(I)
54        CONTINUE
        ENDIF
C
C READ SET OF RADIATION PRESSURE COEFFICIENTS
        DO 60 ISAT=1,NSAT
          IF(NEWOLD.EQ.0)THEN
            READ(LFNRPR)(PARAM(K,ISAT),K=1,3)
          ELSE
            DO 55 K=1,NVAR
              READ(LFNRPR)PARAM(K,ISAT),SCALPA(K),(LOCQ(L,K),L=1,6)
55          CONTINUE
          END IF
60      CONTINUE
C
C READ STOCHASTIC PULSES
        IF(LFORM.EQ.1) THEN
          DO 703 ISAT=1,NSAT
            READ(LFNRPR) NSTC(ISAT)
            DO 702 ISTC=1,NSTC(ISAT)
              IFRC=0
              DO 701
                IFRC=IFRC+1
                READ(LFNRPR) FRCTYP(IFRC,ISTC,ISAT),
     1                       NSTCEP(ISTC,ISAT),INTSTC(ISTC,ISAT),
     2                       TIMSTC(ISTC,ISAT),PARSTC(IFRC,ISTC,ISAT)
                IF(IFRC.EQ.NSTCEP(ISTC,ISAT)) GOTO 702
701           CONTINUE
702         CONTINUE
703       CONTINUE
        END IF
C -------------------------------------------------
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
C -------------------------------------------------
        IF(NEWOLD.EQ.0)THEN
          TOTREC=(NINT-INTOLD)*(1+3*NSAT*(IQ+1))
        ELSE
          TOTREC=(NINT-INTOLD)*(1+NVAR*NSAT*(IQ+1))
        END IF
        DO 70 I=1,TOTREC
          READ(LFNRPR)DUMMY
70      CONTINUE
C
C SKIP ARCS BETWEEN CURRENT AND NEW ARC
C -------------------------------------------------
        DO 80 I=IAOLD+1,IARC-1
          READ(LFNRPR)NSAT,NINT,IQ
          IF(METHOD.EQ.1) THEN
            READ(LFNRPR)T1,T2
          ELSE
            READ(LFNRPR)T1,T2
            READ(LFNRPR)NVAR
            DO 56 KI=1,NINT+1
              READ(LFNRPR)DUMMY
56          CONTINUE
          ENDIF
          IF(NEWOLD.EQ.0)THEN
            TOTREC=NSAT+(1+3*NSAT*(IQ+1))*NINT
          ELSE
            IF(LFORM.EQ.1) THEN
              TOTREC=NSAT*NVAR+SKPSTC(I)+(1+NVAR*NSAT*(IQ+1))*NINT
            ELSEIF(LFORM.EQ.0) THEN
              TOTREC=NSAT*NVAR+(1+NVAR*NSAT*(IQ+1))*NINT
            END IF
          END IF
C
C SKIP REMAINING PART OF ARC
          DO 73 II=1,TOTREC
            READ(LFNRPR)DUMMY
73        CONTINUE
80      CONTINUE
C
C READ FIRST TWO RECORDS OF NEW ARC
C -------------------------------------------------
        READ(LFNRPR)NSAT,NINT,IQ,(NAVNUM(I),I=1,NSAT)
        IF(METHOD.EQ.1) THEN
          READ(LFNRPR)T1,T2,(TBOUND(I),I=1,NINT+1)
        ELSE
          READ(LFNRPR) T1,T2
          IF(NEWOLD.EQ.1)THEN
            READ(LFNRPR)NVAR
          END IF
          DO 81 I=1,NINT+1
            READ(LFNRPR) TBOUND(I)
81        CONTINUE
        ENDIF
C
C READ SET OF RPR PARAMETERS
        DO 90 ISAT=1,NSAT
          IF(NEWOLD.EQ.0)THEN
            READ(LFNRPR)(PARAM(K,ISAT),K=1,3)
          ELSE
            DO 85 K=1,NVAR
              READ(LFNRPR)PARAM(K,ISAT),SCALPA(K),(LOCQ(L,K),L=1,6)
85          CONTINUE
          END IF
90      CONTINUE
C
C READ STOCHASTIC PULSES
        IF(LFORM.EQ.1) THEN
          DO 903 ISAT=1,NSAT
            READ(LFNRPR) NSTC(ISAT)
            DO 902 ISTC=1,NSTC(ISAT)
              IFRC=0
              DO 901
                IFRC=IFRC+1
                READ(LFNRPR) FRCTYP(IFRC,ISTC,ISAT),
     1                       NSTCEP(ISTC,ISAT),INTSTC(ISTC,ISAT),
     2                       TIMSTC(ISTC,ISAT),PARSTC(IFRC,ISTC,ISAT)
                IF(IFRC.EQ.NSTCEP(ISTC,ISAT)) GOTO 902
901           CONTINUE
902         CONTINUE
903       CONTINUE
        END IF
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
        TOTREC=(INT-1-INTOLD)*(1+NVAR*NSAT*(IQ+1))
        DO 120 I=1,TOTREC
          READ(LFNRPR)DUMMY
120     CONTINUE
C
C READ SET OF POLYNOMIAL COEFFICIENTS FOR NEW INTERVAL
C
C 1. T0 (ORIGIN OF DEVELOPMENT) AND H (INTERVAL LENGTH)
        READ(LFNRPR)T0,H
C
C 2. COEFFICIENTS
        I0=0
        TOTREC=NVAR*NSAT*(IQ+1)
        DO 130 I=1,TOTREC
          READ(LFNRPR)(Y(I0+K),K=1,3)
          I0=I0+3
130     CONTINUE
C -------------------------------------------------------------------
C
C COMPUTATION OF PARTIAL AND ITS DERIVATIVES
C -------------------------------------------------------------------
140     CONTINUE
C
C SATELLITE AFTER MANOEUVRE: SVN=SVN+50
        SVNMAN=SVN
        DO 145 IMAN=1,NMAN
          IF (SATMAN(IMAN).EQ.SVNMAN.AND.
     1        TIMMAN(IMAN).GT.TFIRST(IARC)-DTBND1.AND.
     2        TIMMAN(IMAN).LE.T) THEN
            SVNMAN=SVNMAN+50
            GOTO 147
          ENDIF
145     CONTINUE
147     CONTINUE
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
          WRITE(LFNERR,151) SVNMAN,IARC
151       FORMAT(/,' *** SR PRTDER: SATELLITE NOT FOUND',/,
     1                         16X,'SATELLITE:',I3,/,
     2                         16X,'ARC      :',I3,/)
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
          WRITE(LFNERR,153) SVNMAN,IARC
153       FORMAT(/,' ### SR PRTDER: SATELLITE NOT FOUND',/,
     1                         16X,'SATELLITE:',I3,/,
     2                         16X,'ARC      :',I3,/)
154       ICRARC=IARC
          IAOLD=IARC
          INTOLD=INT
          IRCODE=2
          GOTO 999
        ENDIF
C
160     TPOL=(T-T0)*86400.D0
        IPOL=1+3*NVAR*(ISAT-1)*(IQ+1)+3*(IPAR-1)*(IQ+1)
ccc     IF(DABS(TPOL).LT.4.D-5)TPOL=0.D0
        CALL YPOL(IDER,IQ,3,H,FAC,TPOL,Y(IPOL),X)
        IAOLD=IARC
        INTOLD=INT
C
C DEFINE CURRENT RPR COEFFICIENTS AND ORBITAL ELEMENTS
C ----------------------------------------------------
        DO 170 K=1,NVAR
          IF(NEWOLD.EQ.0)THEN
            RPRPAR(K)=PARAM(K,ISAT)
          ELSE
            IF(K.LE.6)THEN
              ELESAT(K)  =PARAM(K,ISAT)
            ELSE
              RPRPAR(K-6)=PARAM(K,ISAT)
            END IF
          END IF
170     CONTINUE
C
C DEFINE CURRENT STOCH. PULSES
C ----------------------------
        IF(PRESENT(NSTCA)) THEN
          NSTCA=0
          DO 1100 II=1,NMXINT
            NSTCEPA(II)=0
            INTSTCA(II)=0
            TIMSTCA(II)=0.D0
            FRCTYPA(1:3,II)=0
            PARSTCA(1:3,II)=0.D0
1100      CONTINUE
C
          IF(LFORM.EQ.1) THEN
            NSTCA=NSTC(ISAT)
            DO 1102 ISTC=1,NSTC(ISAT)
              NSTCEPA(ISTC)=NSTCEP(ISTC,ISAT)
              INTSTCA(ISTC)=INTSTC(ISTC,ISAT)
              TIMSTCA(ISTC)=TIMSTC(ISTC,ISAT)
              DO 1101 IFRC=1,NSTCEP(ISTC,ISAT)
                FRCTYPA(IFRC,ISTC)=FRCTYP(IFRC,ISTC,ISAT)
                PARSTCA(IFRC,ISTC)=PARSTC(IFRC,ISTC,ISAT)
1101          CONTINUE
1102        CONTINUE
          END IF
        END IF
C
C DEFINE NUMBER OF RPR PARAMETERS, DE-SCALE PARAMETERS
C ----------------------------------------------------
        IF(NEWOLD.EQ.0)THEN
          NRAD=3
        ELSE
          NRAD=NVAR-6
          DO 180 K=1,3*(IDER+1)
            X(K)=X(K)*SCALPA(IPAR)
180       CONTINUE
        END IF
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
