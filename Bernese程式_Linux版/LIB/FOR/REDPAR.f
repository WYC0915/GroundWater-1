      MODULE s_REDPAR
      CONTAINS

C*
      SUBROUTINE REDPAR(OPTELI,NFLSES,FILNUM,IFDONE,NFTOT,
     1                  STFIL,TRPLMS,NEPOCH,TIMREF,IDELTT,NPAR,
     2                  NAMB,NPARN,NPASES,NAMSES,NPARMS,PARLST,ANOR,
     3                  BNOR,XXX0,LOCQ,PARTYP,RMS,NOBSPA,PARFLG,
     4                  CLFRTO,ARCINT)
CC
CC NAME       :  REDPAR
CC
CC PURPOSE    :  ELIMINATE AMBIGUITIES OF SATELLITES OF CURRENT SESSION
CC               THAT HAVE NOT BEEN OBSERVED. MOREOVER, PREELIMI-
CC               NATE AMBIGUITY PARAMETERS OF CURRENT SESSION,
CC               SAME FOR TROPOSPHERE, ORBIT, AND STATION CLOCK
CC               PARAMETERS
CC
CC
CC PARAMETERS :
CC         IN :  OPTELI(I),I=1,..,MAXTYP: OPTION FOR PRE-     I*4
CC                        ELIMINATION OF PARAMETER TYPES:
CC                        =0 : NOT PRE-ELIMINATED
CC                        =1 : PRE-ELIMINATED BEFORE INVERSION
CC                        =2 : PRE-ELIMINATED AFTER  INVERSION
CC                        =3 : PRE-ELIMINATED EPOCH-WISE
CC               NFLSES: NUMBER OF FILES IN SESSION           I*4
CC               FILNUM(I),I=1,..,NFLSES: FILE NUMBERS        I*4
CC               IFDONE : FILE ALREADY PROCESSED              I*4
CC                          IFDONE(I): I=1,..NFTOT: FILE
CC                          =0: NOT YET PROCESSED
CC                          =1: ALREADY PROCESSED
CC               NFTOT : TOTAL NUMBER OF FILES                I*4
CC               STFIL(K,I),K=1,2, I=1,..,NFTOT: NUMBERS OF   I*4
CC                       OBSERVING STATIONS IN COORDINATE FILE
CC               TRPLMS(K,I),K=1,2,I=1,..,NTRSTA: TROPOS-     R*8
CC                       PHERE PARAMETER EST. FROM .. TO
CC               TIMREF(I),I=1,..,NFTOT: FILE REFERENCE TIMES R*8
CC               NEPOCH(I),I=1,..,NFTOT: NUMBER OF EPOCHS     I*4
CC               IDELTT(I),I=1,..,NFTOT: OBSERVATION INTER-   I*4
CC                        VALS (SEC)
CC               CLFRTO : TIME INTERVAL FOR CLOCK ERRORS      R*8(2,1)
CC                        CLFRTO(1,K): START OF THE INTERVAL
CC                        CLFRTO(2,K): END OF THE INTERVAL
CC                        IN JULIAN DATE FOR CLOCK REQUEST K
CC               ARCINT(I),I=1,2,...,NFTOT : ARC NUMBER       I*4
CC                        BELONGING TO FILE I
CC     IN/OUT :  NPAR  : TOTAL NUMBER OF PARAMETERS NOT YET   I*4
CC                       ELIMINATED
CC               NAMB  : NUMBER OF AMBIGUITIES                I*4
CC               NPARN : NPAR-NAMB                            I*4
CC               NPASES: NUMBER OF PARAMETERS OF THE SESSION: I*4
CC                       PARAMETERS WITHOUT AMBIGUITIES +
CC                       AMBIGUITIES OF THE SESSION
CC               NPARMS: NUMBER OF PARAMETERS TO CALCULATE    I*4
CC                       RMS ERROR
CC               PARLST(I,K), I=1,..,5,K=1,..,MAXTYP: NUMBER  I*4
CC                        OF PARAMETERS:
CC                        I=1: #PARAMETERS OF TYPE I (NPARMS)
CC                        I=2: #SET-UP
CC                        I=3: #NO-OBS
CC                        I=4: #REF. PARAMETERS
CC                        I=5: #SINGULAR
CC               ANOR(I),=1,2,..,NPAR(*(NPAR+1)/2             R*8
CC               BNOR(I),=1,2,..,NPAR                         R*8
CC               XXX0(I),=1,2,..,NPAR                         R*8
CC                        A PRIORI VALUES
CC               LOCQ(K,I),K=1,2,..,MAXLCQ, I=1,2,..,NPAR:    I*4
CC                       PARAMETER CHARACTERIZATION
CC               PARTYP: PARAMETER DESCRIPTION                t_partyp(*)
CC               RMS   : SUM OF RESIDUALS SQUARES             R*8
CC               NOBSPA : NUM.OF OBSERV PER PARAMETER         I*4(*,*)
CC                        NOBSPA(MAXMEA*ISYS+IMEA,IPAR)
CC               HS1(I),I=1,..,NAMSES: AUX. ARRAY             R*8
CC               HS2(I),I=1,..,NAMSES: AUX. ARRAY             R*8
CC               HS3(I),I=1,..,NAMSES: AUX. ARRAY             CH*1
CC               PARFLG(K),K=1,..NPAR: FLAG FOR SINGULAR PAR. I*4
CC                        =0 : PARAMETER NOT SINGULAR
CC                        =1 : PARAMETER SINGULAR
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC VERSION    :  3.2
CC
CC CREATED    :  87/10/28 08:37
CC
CC CHANGES    :  24-FEB-92 : ??: WRITE MESSAGE WHEN ELIMINATING AMBIGUITIES
CC               06-MAR-92 : ??: WRITE CORRECT PARAMETER NUMBER IN MESSAGE
CC               22-JUN-92 : ??: TURBO VERSION
CC               12-JUL-92 : ??: WRITE ONLY NUMBER OF ELIMINATED PARAMETERS
CC               26-APR-93 : EB: PREELIMINATION OF TROPOS. PARAMETERS
CC                           EB: PREELIMINATION OF ORBITS AND CLOCKS
CC               25-JUL-94 : MR: PRE-ELIMINATION STATISTICS SIMPLIFIED
CC               27-JUL-94 : MR: SYMING INSTEAD OF SYMIN8, "PARFLG"
CC               05-AUG-94 : MR: UPDATE "NPARMS" FOR SINGULAR PARAM.
CC               10-AUG-94 : MR: CALL EXITRC INSTEAD OF STOP
CC               29-SEP-94 : MR: WRONG "NPASES" AND "NPARED"
CC               21-APR-95 : GB: TURBO VERSION FOR BASELINE PROCESSING
CC                               NEW VARIABLE INDJ1 IN PARAMETER LIST OF SR
CC               04-MAY-95 : GB: NEW TURBO VERSION BASED ON SR REDTRB
CC                               (CHEAP SOLUTION WITH INDJ1 ELIMINATED)
CC               05-SEP-95 : LM: CORRECT INITIALIZATION, CHECK A0I-DIMENS.
CC               06-JUN-96 : MR: SET I0MAX=1, IF NO PARAM.
CC               23-JUN-02 : MR: CHANGE ORDER OF TESTS: NPASES BEFORE ANOR(II)
CC               23-JUN-02 : MR: CORRECT DIMENSIONS IN DMATRD CALLS
CC               21-OCT-04 : RD: ALLOCATE A0I, AII, IDEL LOCALLY
CC               21-JUN-05 : MM: COMLFNUM.INC REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               29-JUN-06 : HB: ADD VECTOR WITH A PRIORI VALUES TO
CC                               PARAMETER LIST AND ALSO REDUCE THIS
CC               17-JUN-08 : RD: COUNTER FOR OBSERV. PER PARAMETER ADDED
CC               16-NOV-10 : RD: RED_PARTYP ADDED
CC               10-JUL-12 : RD: USE SYMINVG INSTEAD OF SYMING
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE M_BERN,   ONLY: r8b
      USE M_GLOBAL, ONLY: MAXSYS
      USE P_GPSEST, ONLY: MAXMEA,red_partyp,t_partyp
C
      USE f_ikf
      USE s_alcerr
      USE s_syminvg
      USE s_updpar
      USE s_redtrb
      USE s_imatrd
      USE s_dmatrd
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I0MAX , IAC   , IF    , IFS   , II    , IK01  ,
     1          IK11  , ILC   , IONE  , IPAR  , ISING , ISIX  ,
     2          K     , K1    , MAXLCQ, MXCLCQ, NAMB  , NAMEFF, NAMSES,
     3          NDEL  , NDELA , NDELPA, NDIM  , NELTOT, NFLSES, NFTOT ,
     4          NPAR  , NPARED, NPARMS, NPARN , NPASES, NPNEW
C
      REAL*8    RMS   , SCL1  , SCL2  , TIM1  , TIM2  , TROP1 , TROP2
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      TYPE(t_partyp), DIMENSION(:) :: partyp
C
      CHARACTER*6 MXNLCQ
      CHARACTER(LEN=6), PARAMETER :: SRNAME = 'REDPAR'
C
      INTEGER*4   LOCQ(MXCLCQ,*),IDEL(NPAR),FILNUM(*),IFDONE(*)
      INTEGER*4   STFIL(2,*),NEPOCH(*),IDELTT(*),ARCINT(*)
      INTEGER*4   OPTELI(*),PARLST(5,*),PARFLG(*)
      INTEGER*4   NOBSPA(:,:)
C
      REAL*8      ANOR(*),BNOR(*),XXX0(*)
      REAL*8      TRPLMS(2,*),TIMREF(*),CLFRTO(2,*)
      REAL(r8b),   DIMENSION(:), ALLOCATABLE :: A0I,AII
C
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
      MAXLCQ=MXCLCQ
C
C IF NO PRE-ELIMINATION DO NOTHING
C --------------------------------
      IF (OPTELI(2).NE.1 .AND.
     1    OPTELI(3).NE.1 .AND.
     2    OPTELI(4).NE.1 .AND.
     3    OPTELI(6).NE.1)       GOTO 999
C
      DO 50 IPAR=1,NPAR
        IDEL(IPAR)=0
50    CONTINUE
C
C PART 1: ELIMINATE AMBIGUITIES OF SATELLITES WHICH HAVE NOT
C         BEEN OBSERVED IN CURRENT SESSION
C ---------------------------------------------------------
      NAMEFF=0
      NDEL=0
      IF (OPTELI(4).EQ.1) THEN
        DO 100 I=1,NAMB
          ILC=LOCQ(2,NPARN+I)
          II=IKF(NPARN+I,NPARN+I)
          IDEL(NPARN+I)=0
          DO 200 IFS=1,NFLSES
            IF=FILNUM(IFS)
            IF (ILC.NE.IF) GO TO 200
            IF (NPARN+I.LE.NPASES .AND. ANOR(II).NE.0.D0) THEN
              NAMEFF=NAMEFF+1
            ELSE
              NDEL=NDEL+1
              IDEL(NPARN+I)=1
              PARLST(1,4)=PARLST(1,4)-1
              PARLST(3,4)=PARLST(3,4)+1
            END IF
200       CONTINUE
100     CONTINUE
C
        CALL DMATRD('S',NPASES,NPASES,IDEL,ANOR,NPNEW,NPNEW)
        CALL DMATRD('R',NPASES,1,IDEL,BNOR,NPNEW,IONE)
        CALL DMATRD('R',NPASES,1,IDEL,XXX0,NPNEW,IONE)
        CALL IMATRD('C',MAXMEA*MAXSYS,NPAR,IDEL,NOBSPA,ISIX,NPNEW)
        CALL IMATRD('C',MAXLCQ,NPAR,IDEL,LOCQ,ISIX,NPNEW)
        CALL red_partyp(npar,idel,partyp)
        NAMB=NAMB-NDEL
        NPAR=NPAR-NDEL
        NPARMS=NPARMS-NDEL
        NPASES=NPARN+NAMSES-NDEL
C
        NDELA=0
        DO 700 I=1,NAMB
          IDEL(NPARN+I)=0
          DO 750 IFS=1,NFLSES
            IF(LOCQ(2,NPARN+I).EQ.FILNUM(IFS)) THEN
              IDEL(NPARN+I)=1
              NDELA=NDELA+1
            ENDIF
750       CONTINUE
700     CONTINUE
C
      END IF
C
C PART 2: WHICH PARAMETERS MAY BE ELIMINATED ?
C ------------------------------------------
      NELTOT=0
C
      DO 300 IPAR=1,NPAR-NAMB
        IDEL(IPAR)=0
C
C TROPOSPHERE
C
        IF (LOCQ(1,IPAR).EQ.6 .AND. OPTELI(6).EQ.1) THEN
          DO 350 IF=1,NFTOT
            IF (STFIL(1,IF).EQ.LOCQ(3,IPAR) .OR.
     1          STFIL(2,IF).EQ.LOCQ(3,IPAR)) THEN
              TIM1=TIMREF(IF)
              TIM2=TIMREF(IF)+(NEPOCH(IF)-1)*IDELTT(IF)/24.D0/3600.D0
              TROP1=TRPLMS(1,LOCQ(2,IPAR))
              TROP2=TRPLMS(2,LOCQ(2,IPAR))
              IF (IFDONE(IF).EQ.0 .AND.
     1            TROP2.GT.TIM1 .AND. TROP1.LT.TIM2) GO TO 300
            END IF
350       CONTINUE
          IDEL(IPAR)=1
          NELTOT=NELTOT+1
          IF (ANOR(IKF(IPAR,IPAR)).EQ.0.D0) THEN
            PARLST(1,6)=PARLST(1,6)-1
            PARLST(3,6)=PARLST(3,6)+1
            PARLST(5,6)=PARLST(5,6)-1
          ENDIF
          GOTO 300
        ENDIF
C
C ORBIT PARAMETER
C
        IF (LOCQ(1,IPAR).EQ.3 .AND. OPTELI(3).EQ.1) THEN
          DO 351 IF=1,NFTOT
            TIM1=TIMREF(IF)
            TIM2=TIMREF(IF)+(NEPOCH(IF)-1)*IDELTT(IF)/24.D0/3600.D0
            IF (IFDONE(IF).EQ.0 .AND.
     1          ARCINT(IF).EQ.LOCQ(2,IPAR)) GO TO 300
351       CONTINUE
          IDEL(IPAR)=1
          NELTOT=NELTOT+1
          IF (ANOR(IKF(IPAR,IPAR)).EQ.0.D0) THEN
            PARLST(1,3)=PARLST(1,3)-1
            PARLST(3,3)=PARLST(3,3)+1
            PARLST(5,3)=PARLST(5,3)-1
          ENDIF
          GOTO 300
        END IF
C
C STATION CLOCKS
C
        IF (LOCQ(1,IPAR).EQ.2 .AND. OPTELI(2).EQ.1) THEN
          DO 352 IF=1,NFTOT
            IF (STFIL(1,IF).EQ.LOCQ(2,IPAR) .OR.
     1        STFIL(2,IF).EQ.LOCQ(2,IPAR)) THEN
              TIM1=TIMREF(IF)
              TIM2=TIMREF(IF)+(NEPOCH(IF)-1)*IDELTT(IF)/24.D0/3600.D0
              SCL1=CLFRTO(1,LOCQ(3,IPAR))
              SCL2=CLFRTO(2,LOCQ(3,IPAR))
              IF (IFDONE(IF).EQ.0 .AND.
     1            SCL2.GT.TIM1 .AND. SCL1.LT.TIM2) GO TO 300
            ENDIF
352       CONTINUE
          IDEL(IPAR)=1
          NELTOT=NELTOT+1
          IF (ANOR(IKF(IPAR,IPAR)).EQ.0.D0) THEN
            PARLST(1,2)=PARLST(1,2)-1
            PARLST(3,2)=PARLST(3,2)+1
            PARLST(5,2)=PARLST(5,2)-1
          ENDIF
        END IF
300   CONTINUE
C
C PART 3: REDUCE ANOR, BNOR, RMS, LOCQ
C ------------------------------------
      NDELPA=NDELA+NELTOT
      IF (NDELPA.EQ.0) GOTO 999
C
C NUMBER OF PARAMETERS FOR REDUCTION OF MATIRCES
      IF (OPTELI(4).EQ.1) THEN
        NPARED=NPASES
      ELSE
        NPARED=NPAR
      ENDIF
C
      ALLOCATE(AII(IKF(NDELPA,NDELPA)),STAT=IAC)
      CALL ALCERR(IAC,'AII',(/IKF(NDELPA,NDELPA)/),SRNAME)
      AII=0D0
C
      ALLOCATE(A0I(NDELPA*(NPARED-NDELPA)),STAT=IAC)
      CALL ALCERR(IAC,'A0I',(/NDELPA*(NPARED-NDELPA)/),SRNAME)
      A0I=0D0
C
C GENERATE TWO AUXILIARY MATRICES A0I, AII
      IK11=0
      IK01=0
      K1=0
      DO 400 K=1,NPARED
        IF (IDEL(K).EQ.1) THEN
          K1=K1+1
          DO 450 I=1,NPARED
            IF (IDEL(I).EQ.1 .AND. I.LE.K) THEN
              IK11=IK11+1
              AII(IK11)=ANOR(IKF(I,K))
            ELSE IF (IDEL(I).EQ.0) THEN
              IK01=IK01+1
              A0I(IK01)=ANOR(IKF(I,K))
            END IF
450       CONTINUE
        END IF
400   CONTINUE
      IF (K1.GT.0) THEN
        I0MAX=IK01/K1
      ELSE
        I0MAX=1
      ENDIF
C
C INVERT AII
      NDIM=NELTOT+NAMEFF
      CALL SYMINVG(NDIM,AII,1,ISING,PARFLG)
C
C UPDATE NUMBER OF SINGULAR PARAMETERS IN "PARLST"
C
C UPDATE NUMBER OF SINGULAR PARAMETERS IN "PARLST"
      CALL UPDPAR(1,NPARED,LOCQ,IDEL,PARFLG,NPARMS,PARLST)
C
C REDUCE RMS, ANOR, BNOR USING SR REDTRB
C --------------------------------------
      CALL REDTRB(NPARED,A0I,AII,IDEL,I0MAX,ANOR,BNOR,RMS)
C
C PHYSICALLY ELIMINATE PRE-REDUCED TERMS IN NEQ-SYSTEM
C ----------------------------------------------------
      CALL DMATRD('S',NPARED,NPARED,IDEL,ANOR,NPNEW,NPNEW)
      CALL DMATRD('R',NPARED,1,IDEL,BNOR,NPNEW,IONE)
      CALL DMATRD('R',NPARED,1,IDEL,XXX0,NPNEW,IONE)
      CALL IMATRD('C',MAXMEA*MAXSYS,NPAR,IDEL,NOBSPA,ISIX,NPNEW)
      CALL IMATRD('C',MAXLCQ,NPAR,IDEL,LOCQ,ISIX,NPNEW)
      CALL red_partyp(npar,idel,partyp)
C
      NAMB=NAMB-NDELA
      NPAR=NPAR-NDELA-NELTOT
      NPARN=NPAR-NAMB
C
      DEALLOCATE(AII,STAT=IAC)
      DEALLOCATE(A0I,STAT=IAC)
C
999   RETURN
      END SUBROUTINE

      END MODULE
