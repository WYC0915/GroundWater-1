      MODULE s_REDAMB
      CONTAINS

C*
      SUBROUTINE REDAMB(NFLSES,FILNUM,OBSNUM,NPAR  ,NAMB  ,NPARN ,
     1                  NPASES,NAMSES,NPARMS,PARLST,ANOR  ,BNOR  ,
     2                  XXX0  ,LOCQ  ,PARTYP,RMS   ,NOBSPA,PARFLG)
CC
CC NAME       :  REDAMB
CC
CC PURPOSE    :  PRE-ELIMINATE AMBIGUITY PARAMETERS AS SOON AS ALL
CC               OBSERVATIONS BELONGING TO A AMBIGUITY CLUSTER HAVE
CC               BEEN PROCESSED
CC
CC PARAMETERS :
CC         IN :  NFLSES: NUMBER OF FILES IN SESSION           I*4
CC               FILNUM(I),I=1,..,NFLSES: FILE NUMBERS        I*4
CC               OBSNUM(I),I=1,..,NFLSES: OBSERV. NUMBERS FOR I*4
CC                        DIFFERENT FILES IN SESSION
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
CC               PARTYP : PARAMETER DESCRIPTION               t_partyp(*)
CC               RMS   : SUM OF RESIDUALS SQUARES             R*8
CC               NOBSPA : NUM.OF OBSERV PER PARAMETER         I*4(*,*)
CC                        NOBSPA(MAXMEA*ISYS+IMEA,IPAR)
CC               PARFLG(K),K=1,..NPAR: FLAG FOR SINGULAR PAR. I*4
CC                        =0 : PARAMETER NOT SINGULAR
CC                        =1 : PARAMETER SINGULAR
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER, S.SCHAER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  07-AUG-97
CC
CC CHANGES    :  11-AUG-97 : SS: NEW OPTION "STRAMB(2)"
CC               13-FEB-03 : RS: NO-OBS-AMBIGUITIES: ANOR AND BNOR
CC                               HAVE TO BE ZERO
CC               21-OCT-04 : RD: ALLOCATE A0I, AII, IDEL LOCALLY
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               29-JUN-06 : HB: ADD VECTOR WITH A PRIORI VALUES TO
CC                               PARAMETER LIST AND ALSO REDUCE THIS
CC               17-JUN-08 : RD: COUNTER FOR OBSERV. PER PARAMETER ADDED
CC               16-NOV-10 : RD: RED_PARTYP ADDED
CC               10-JUL-12 : RD: USE SYMINVG INSTEAD OF SYMING
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
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
      INTEGER*4 I     , I0MAX , IAC   , IEPO  , IFIL  , IFS   , II    ,
     1          IK01  , IK11  , IONE  , IPAR  , ISING , ISIX  ,
     2          K     , K1    , MAXLCQ, MXCLCQ, NAMB  , NAMSES, NDEL  ,
     3          NDELA , NFLSES, NPAR  , NPARED, NPARMS, NPARN , NPASES,
     4          NPNEW
C
      REAL*8    RMS
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      TYPE(t_partyp), DIMENSION(:) :: partyp
C
      CHARACTER*6 MXNLCQ
      CHARACTER(LEN=6), PARAMETER :: SRNAME = 'REDAMB'
C
      INTEGER*4   LOCQ(MXCLCQ,*),IDEL(NPAR),FILNUM(*)
      INTEGER*4   PARLST(5,*),PARFLG(*),OBSNUM(*)
      INTEGER*4   NOBSPA(:,:)
C
      REAL*8      ANOR(*),BNOR(*),XXX0(*)
      REAL(r8b),  DIMENSION(:), ALLOCATABLE :: A0I,AII
C
C
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
      MAXLCQ=MXCLCQ
C
C INITIALIZE "IDEL"
C -----------------
      DO 50 IPAR=1,NPAR
        IDEL(IPAR)=0
50    CONTINUE
C
C PART 1: ELIMINATE AMBIGUITIES OF SATELLITES WHICH HAVE NOT
C         BEEN OBSERVED IN CURRENT SESSION
C ---------------------------------------------------------
      NDEL=0
      DO 100 I=NPARN+1,NPASES
        IDEL(I)=0
        IFIL=LOCQ(2,I)
        IEPO=LOCQ(7,I)
        II=IKF(I,I)
C
        DO 200 IFS=1,NFLSES
          IF (IFIL.EQ.FILNUM(IFS) .AND.
     1        IEPO.LE.OBSNUM(IFS)) THEN
            IF (ANOR(II).EQ.0.D0 .AND. BNOR(I).EQ.0.D0) THEN
              NDEL=NDEL+1
              IDEL(I)=1
              PARLST(1,4)=PARLST(1,4)-1
              PARLST(3,4)=PARLST(3,4)+1
            ENDIF
          ENDIF
200     CONTINUE
100   CONTINUE
C
      IF (NDEL.NE.0) THEN
        CALL DMATRD('S',NPASES,NPASES,IDEL,
     1              ANOR,NPNEW,NPNEW)
        CALL DMATRD('R',NPASES,1,IDEL,BNOR,NPNEW,IONE)
        CALL DMATRD('R',NPASES,1,IDEL,XXX0,NPNEW,IONE)
        CALL IMATRD('C',MAXMEA*MAXSYS,NPAR,IDEL,NOBSPA,ISIX,NPNEW)
        CALL IMATRD('C',MAXLCQ,NPAR,IDEL,LOCQ,ISIX,NPNEW)
        CALL red_partyp(npar,idel,partyp)
        NAMB=NAMB-NDEL
        NPAR=NPAR-NDEL
        NPARMS=NPARMS-NDEL
        NPASES=NPASES-NDEL
        NAMSES=NAMSES-NDEL
      ENDIF
C
C PART 2: AMBIGUITIES TO BE PRE-ELIMINATED
C ----------------------------------------
      DO 690 IPAR=1,NPAR
        IDEL(IPAR)=0
690   CONTINUE
C
      NDELA=0
      DO 700 I=NPARN+1,NPASES
        IDEL(I)=0
        IFIL=LOCQ(2,I)
        IEPO=LOCQ(7,I)
        DO 750 IFS=1,NFLSES
          IF (IFIL.EQ.FILNUM(IFS) .AND.
     1        IEPO.LE.OBSNUM(IFS)) THEN
            IDEL(I)=1
            NDELA=NDELA+1
          ENDIF
750     CONTINUE
700   CONTINUE
C
C PART 3: REDUCE ANOR, BNOR, RMS, LOCQ
C ------------------------------------
      IF (NDELA.NE.0) THEN
C
C NUMBER OF PARAMETERS FOR REDUCTION OF MATIRCES
        NPARED=NPASES
C
        ALLOCATE(AII(IKF(NDELA,NDELA)),STAT=IAC)
        CALL ALCERR(IAC,'AII',(/IKF(NDELA,NDELA)/),SRNAME)
        AII=0D0
C
        ALLOCATE(A0I(NDELA*(NPARED-NDELA)),STAT=IAC)
        CALL ALCERR(IAC,'A0I',(/NDELA*(NPARED-NDELA)/),SRNAME)
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
450         CONTINUE
          END IF
400     CONTINUE
        IF (K1.GT.0) THEN
          I0MAX=IK01/K1
        ELSE
          I0MAX=1
        ENDIF
C
C INVERT AII
        CALL SYMINVG(NDELA,AII,1,ISING,PARFLG)
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
        NPAR=NPAR-NDELA
        NPARN=NPAR-NAMB
        NPASES=NPASES-NDELA
        NAMSES=NAMSES-NDELA
C
        DEALLOCATE(AII,STAT=IAC)
        DEALLOCATE(A0I,STAT=IAC)
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
