      MODULE s_AMBREF
      CONTAINS

C*
      SUBROUTINE AMBREF(MAXLOC,MAXAMP,NFIL  ,NFRFIL,ICARR ,OBSCLS,
     1                  ICLUST,NUMAMB,AMBSAT,AMBIEP,AMBCLS,MIXED ,
     2                  OPTDIP,NEPOCH,NPAR  ,NAMB  ,NPARN ,NPARMS,
     3                  NPALCQ,PARLST,ANOR  ,BNOR  ,XXX0  ,LOCQ  ,
     4                  PARTYP,INDP  ,IDEL  ,NOBSPA,NREF  ,REFAMB)
CC
CC NAME       :  AMBREF
CC
CC PURPOSE    :  ELIMINATE AMBIGUITY AND DIFFERENTIAL IONOSPHERE
CC               PARAMETERS FOR SATELLITES THAT HAVE BECOME
CC               REFERENCE SATELLITES
CC
CC PARAMETERS :
CC         IN :  MAXLOC : MAXIMUM NUMBER OF PARAMETERS TO BE  I*4
CC                        CHARACTERIZED
CC               MAXAMP : MAXIMUM NUMBER OF AMBIGUITY PAR.    I*4
CC               NFIL   : NUMBER OF FILES                     I*4
CC               NFRFIL(IFIL) : NUMBER OF FREQUENCIES TO      I*4
CC                              BE PROCESSED.
CC               ICARR(IFRQ,IFIL)  : FREQUENCIES TO BE        I*4
CC                                   PROCESSED
CC               OBSCLS(IAMP) :  CLUSTER NUMBER OF AMB.PAR.   I*4
CC               ICLUST(IFIL) :  NUMBER OF CLUSTERS           I*4
CC               NUMAMB(I),I=1,..,NFTOT: NUMBER OF AMBIGU.    I*4
CC               AMBSAT(J,I),J=1,..,NUMAMB(I),I=1,..,NFTOT:   I*4
CC                        AMBIGUITY SATELLITE NUMBERS
CC               AMBIEP(J,I),J=1,..,NUMAMB(I),I=1,..,NFTOT:   I*4
CC                        STARTING EPOCH NRS FOR AMBIGUITIES
CC               AMBCLS(L,K,I),L=1,..,NUMAMB(I), K=1,2,3,     I*4
CC                        I=1,..,NFTOT: AMBIGUITY CLUSTERS
CC               MIXED  : SATELLITE SYSTEM                    I*4
CC                        =0: GPS ONLY
CC                        =1: MIXED (GPS AND GLONASS)
CC                        =2: GLONASS ONLY
CC               OPTDIP : OPTIONS FOR DIFF. ION. PARAMETERS   I*4(3)
CC                        (1): =0: NO DIFF. ION. PARAMETERS
CC                             =1: ONE PAR. PER EPOCH AND SAT.
CC                             =2: PARAMETERS EPOCH-WISE PRE-
CC                                 ELIMINATED
CC                        (2): ELIMINATION OF REF. ION. PAR.
CC                        (3): ELEV.-DEP. PAR. CONSTRAINING
CC               NEPOCH : NUMBER OF EPOCHS IN FILE            I*4(*)
CC     IN/OUT :  NPAR   : TOTAL NUMBER OF PARAMETERS          I*4
CC               NAMB   : NUMBER OF AMBIGUITIES               I*4
CC               NPARN  : NPAR-NAMB                           I*4
CC               NPARMS : NUMBER OF PARAMETERS TO COMPUTE RMS I*4
CC               NPALCQ : TOTAL NUMBER OF PARAMETERS IN LOCQ  I*4
CC                        INCLUDING THOSE ELIMINATED IN REDNEQ
CC               PARLST(I,K), I=1,..,5,K=1,..,MAXTYP: NUMBER  I*4
CC                        OF PARAMETERS:
CC                        I=1: #PARAMETERS OF TYPE I (NPARMS)
CC                        I=2: #SET-UP
CC                        I=3: #NO-OBS
CC                        I=4: #REF. PARAMETERS
CC                        I=5: #SINGULAR
CC               ANOR(I),I=1,2,..,NPAR*(NPAR+1)/2             R*8
CC               BNOR(I),I=1,2,..,NPAR                        R*8
CC               XXX0(I),=1,2,..,NPAR                         R*8
CC                        A PRIORI VALUES
CC               LOCQ(K,I),K=1,2,..,MAXLCQ, I=1,..,NPAR+NDEL  I*4
CC                       PARAMETER CHARACTERIZATION
CC               INDP(I),I=1,..,NPAR: INDEX FOR PARAMETER     I*4
CC                       LOCATION IN REDUCED ANOR,BNOR,...
CC               IDEL(I),I=1,..,NPAR: AUXILIARY ARRAY         I*4
CC               NOBSPA : NUM.OF OBSERV PER PARAMETER         I*4(*,*)
CC                        NOBSPA(MAXMEA*ISYS+IMEA,IPAR)
CC         OUT : NREF   : NUMBER OF REFERENCE AMBIGUITIES     I*4
CC               REFAMB(IREFAMB) : REFERENCE AMBIGUITIES      I*4
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER, L.MERVART, S.SCHAER
CC
CC VERSION    :  3.4
CC
CC CREATED    :  07-AUG-92
CC
CC CHANGES    :  19-FEB-93 : ??: NEW PARAMETER "NPALCQ". KEEP LOCQ INFO
CC                               FOR ELIMINATED PARAMETERS AT END OF LOCQ.
CC                               ONLY RESOLVABLE AMBIGUITIES AS REFERENCES
CC               22-FEB-93 : ??: SET ARRAY OBSCLS FOR SECOND LINEAR COMB.
CC                               TOO (IF TWO LC'S ARE PROCESSED)
CC               05-MAR-93 : ??: IF NO RESOLVABLE AMBIGUITIES AS REFERENCE
CC                               POSSIBLE, TAKE ANOTHER ONE
CC               30-SEP-93 : SS: SELECT AND ELIMINATE REF. ION. PAR.
CC               25-JUL-94 : MR: PRE-ELIMINATION STATISTICS SIMPLIFIED
CC               10-AUG-94 : MR: CALL EXITRC
CC               05-AUG-97 : SS: ELEV.-DEP. SIP CONSTRAINING
CC               25-JUN-98 : HH: MODIFICATIONS FOR GLONASS
CC               04-AUG-99 : MR: REFERENCE AMBIGUITIES FOR GLONASS
CC               06-AUG-99 : SS: FLAG UNOBSERVED IONOSPHERE PARAMETERS
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               29-JUN-06 : HB: ADD VECTOR WITH A PRIORI VALUES TO
CC                               PARAMETER LIST AND ALSO REDUCE THIS
CC               17-JUN-08 : RD: COUNTER FOR OBSERV. PER PARAMETER ADDED
CC               16-NOV-10 : RD: RED_PARTYP ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1992     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE M_GLOBAL, ONLY: MAXSYS
      USE P_GPSEST, ONLY: MAXMEA, red_partyp,t_partyp
C
      USE f_ikf
      USE s_exitrc
      USE s_imatrd
      USE s_dmatrd
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IA1   , IA2   , IAM1  , IAMB  , IAMNEW, IAMP  , IAMR  ,
     1          ICAR  , ICLS  , IEPO  , IFIL  , ILCQ  , INDREF,
     2          INUL  , IONE  , IP    , IPAMB , IPAR  , IPARP , IPREF ,
     3          IREF  , IRESOL, IRIP  , ISIX  , MAXAMP, MAXLCQ, MAXLOC,
     4          MAXRIP, MIXED , MXCAMB, MXCFRQ, MXCLCQ, MZDMAX, MZDMIN,
     5          MZDTST, NAMB  , NDAMB , NDEL  , NDELN , NFIL  , NPALCQ,
     6          NPAR  , NPARMS, NPARN , NPNEW , NREF  , NREF2 , NUMRIP
C
      REAL*8    AIDEAL, ATEST
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MAXRIP=250)
C
      TYPE(t_partyp), DIMENSION(:) :: parTyp
C
      CHARACTER*6 MXNLCQ,MXNFRQ,MXNAMB
C
      INTEGER*4 LOCQ(MXCLCQ,*),INDP(*),IDEL(*)
      INTEGER*4 ICLUST(*),OBSCLS(*),REFAMB(*)
      INTEGER*4 NFRFIL(*),ICARR(MXCFRQ,*)
      INTEGER*4 NUMAMB(*),AMBSAT(MXCAMB,*),AMBIEP(MXCAMB,*)
      INTEGER*4 AMBCLS(MXCAMB,3,*),PARLST(5,*)
      INTEGER*4 REFDIP(MAXRIP),OPTDIP(*),NEPOCH(*)
      INTEGER*4 NOBSPA(:,:)
C
      REAL*8 ANOR(*),BNOR(*),XXX0(*)
C
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
      COMMON/MCMFRQ/MXCFRQ,MXNFRQ
      COMMON/MCMAMB/MXCAMB,MXNAMB
      MAXLCQ=MXCLCQ
C
C SELECT REFERENCE AMBIGUITIES
C ----------------------------
      NREF=0
      IF (MIXED.EQ.0) THEN
        DO 200 IFIL=1,NFIL
          DO 300 ICLS=1,ICLUST(IFIL)
            AIDEAL=0.D0
            IRESOL=0
            DO 400 IAMP=1,NAMB
              IP=NPARN+IAMP
              IF (LOCQ(2,IP)   .EQ. IFIL          .AND.
     1            LOCQ(5,IP)   .EQ. ICARR(1,IFIL) .AND.
     2            OBSCLS(IAMP) .EQ. ICLS)         THEN
                ATEST=ANOR(IKF(IP,IP))
C
                IF ((LOCQ(4,IP).LE.2 .OR. LOCQ(4,IP).EQ.5)) THEN
                  IF (IRESOL.EQ.0) THEN
                    IRESOL=1
                    AIDEAL=ATEST
                    INDREF=IP
                  ELSE IF (AIDEAL.LT.ATEST) THEN
                    AIDEAL=ATEST
                    INDREF=IP
                  END IF
                ELSE
                  IF (IRESOL.EQ.0 .AND. AIDEAL.LT.ATEST) THEN
                    AIDEAL=ATEST
                    INDREF=IP
                  ENDIF
                ENDIF
              ENDIF
400         CONTINUE
            IF (AIDEAL.NE.0.D0) THEN
              NREF=NREF+1
              REFAMB(NREF)=INDREF
            END IF
300       CONTINUE
200     CONTINUE
C
C FIND CORRESPONDING AMBIGUITIES OF 2ND FREQUENCY
C -----------------------------------------------
        NREF2=0
        DO 500 IREF=1,NREF
          IAMR=REFAMB(IREF)
          IFIL=LOCQ(2,IAMR)
          IF (NFRFIL(IFIL).EQ.1) GO TO 500
          DO 510 IA1=1,NUMAMB(IFIL)
            IF (AMBCLS(IA1,1,IFIL).EQ.LOCQ(3,IAMR)) GO TO 511
510       CONTINUE
511       CONTINUE
          DO 600 IAMP=1,NAMB
            IF (LOCQ(2,NPARN+IAMP).EQ.IFIL           .AND.
     1          LOCQ(5,NPARN+IAMP).NE. LOCQ(5,IAMR)) THEN
              DO 610 IA2=1,NUMAMB(IFIL)
                IF (AMBCLS(IA2,2,IFIL).EQ.LOCQ(3,NPARN+IAMP)) GO TO 611
610           CONTINUE
611           CONTINUE
              IF (AMBSAT(IA1,IFIL).EQ.AMBSAT(IA2,IFIL) .AND.
     1            AMBIEP(IA1,IFIL).EQ.AMBIEP(IA2,IFIL)) THEN
                NREF2=NREF2+1
                REFAMB(NREF+NREF2)=NPARN+IAMP
                GO TO 500
              END IF
            END IF
600       CONTINUE
500     CONTINUE
C
        NREF=NREF+NREF2
      ENDIF
C
C COPY OBSCLS OF FIRST FREQUENCY TO OBSCLS FOR SECOND FREQUENCY
C -------------------------------------------------------------
      DO 650 IAM1=1,NAMB
        IFIL=LOCQ(2,NPARN+IAM1)
        ICAR=LOCQ(5,NPARN+IAM1)
        IF (NFRFIL(IFIL).NE.1 .AND. ICAR.EQ.ICARR(1,IFIL)) THEN
          DO 660 IA1=1,NUMAMB(IFIL)
            IF (AMBCLS(IA1,1,IFIL).EQ.LOCQ(3,NPARN+IAM1)) GO TO 661
660       CONTINUE
661       CONTINUE
          DO 640 IAMP=1,NAMB
            IF (LOCQ(2,NPARN+IAMP).EQ. LOCQ(2,NPARN+IAM1)  .AND.
     1          LOCQ(5,NPARN+IAMP).NE. LOCQ(5,NPARN+IAM1)) THEN
              DO 670 IA2=1,NUMAMB(IFIL)
                IF (AMBCLS(IA2,2,IFIL).EQ.LOCQ(3,NPARN+IAMP)) GO TO 671
670           CONTINUE
671           CONTINUE
              IF (AMBSAT(IA1,IFIL).EQ.AMBSAT(IA2,IFIL) .AND.
     1            AMBIEP(IA1,IFIL).EQ.AMBIEP(IA2,IFIL)) THEN
                OBSCLS(IAMP)=OBSCLS(IAM1)
                GOTO 650
              END IF
            END IF
640       CONTINUE
        ENDIF
650   CONTINUE
C
C CHECK MAXIMUM DIMENSIONS NEEDED FOR LOCQ AND OBSCLS REORDERING
C --------------------------------------------------------------
      IF (NPALCQ+NREF .GT. MAXLOC) THEN
        WRITE(LFNERR,901) NPALCQ+NREF,MAXLOC
901     FORMAT(/,' *** SR AMBREF: TOO MANY PARAMETERS ',
     1                           'TO BE CHARACTERIZED',/,
     2                       16X,'NUMBER OF PARAMETERS >=',I5,/,
     3                       16X,'MAXIMUM NUMBER        :',I5,/)
        CALL EXITRC(2)
      END IF
C
      IF (NAMB+NREF .GT. MAXAMP) THEN
        WRITE(LFNERR,902) NAMB+NREF,MAXAMP
902     FORMAT(/,' *** SR AMBREF: TOO MANY AMBIGUITIES',
     1                           ' TO BE CHARACTERIZED',/,
     2                       16X,'NUMBER OF AMBIGUITIES >=',I5,/,
     3                       16X,'MAXIMUM NUMBER         :',I5,/)
        CALL EXITRC(2)
      END IF
C
C MOVE LOCQ-INFO OF ELIMINATED PARAMETERS BY "NREF" TO THE END OF THE
C LOCQ ARRAY TO GET FREE SPACE FOR THE REFERENCE AMBIGUITY INFO
C -------------------------------------------------------------------
      DO 850 IPAR=NPALCQ,NPAR+1,-1
        DO 840 ILCQ=1,MAXLCQ
          LOCQ(ILCQ,IPAR+NREF)=LOCQ(ILCQ,IPAR)
840     CONTINUE
850   CONTINUE
C
C SELECT REFERENCE IONOSPHERE PARAMETERS
C --------------------------------------
      NUMRIP=0
      IF(OPTDIP(1).EQ.1) THEN
        MZDMAX=324000
        DO 805 IFIL=1,NFIL
          DO 810 IEPO=1,NEPOCH(IFIL)
            MZDMIN=MZDMAX
            DO 815 IPAR=1,NPARN
              IF(LOCQ(1,IPAR).EQ.17.AND.
     1           LOCQ(2,IPAR).EQ.IFIL.AND.
     2           LOCQ(4,IPAR).EQ.IEPO.AND.
     3           ANOR(IKF(IPAR,IPAR)).NE.0.D0)THEN
                MZDTST=LOCQ(6,IPAR)
                IF(MZDTST.LT.MZDMIN)THEN
                  MZDMIN=MZDTST
                  IRIP=IPAR
                ENDIF
              ENDIF
815         CONTINUE
            IF(MZDMIN.LT.MZDMAX)THEN
              NUMRIP=NUMRIP+1
              REFDIP(NUMRIP)=IRIP
            ENDIF
810       CONTINUE
805     CONTINUE
      ENDIF
C
C CHECK MAXIMUM NUMBER OF REFERENCE IONOSPHERE PARAMETERS
C -------------------------------------------------------
      IF(NUMRIP.GT.MAXRIP)THEN
        WRITE(LFNERR,906) NUMRIP,MAXRIP
906     FORMAT(/,' *** SR AMBREF: TOO MANY REFERENCE IONOSPHERE ',
     1         'PARAMETERS',/,
     2         16X,'NUMBER OF PARAMETERS:',I5,/,
     3         16X,'MAXIMUM NUMBER      :',I5,/)
        CALL EXITRC(2)
      ENDIF
C
C INCREASE DEGREE OF FREEDOM (NPARMS) IF DIFF. IONOSPHERE PARAMETERS
C ARE ESTIMATED ON THE SINGLE-DIFF. LEVEL (REFERENCES KEPT)
      NPARMS=NPARMS-NUMRIP
      IF (OPTDIP(2).EQ.0) NUMRIP=0
C
C UPDATE NUMBER OF PARAMETERS
      PARLST(1, 4)=PARLST(1, 4)-NREF
      PARLST(1,17)=PARLST(1,17)-NUMRIP
C
C ELIMINATE PARAMETERS
C --------------------
C INITIALIZE
      DO 700 IPAR=1,NPALCQ+NREF
        IDEL(IPAR)=0
700   CONTINUE
C
C ELIMINATE REFERENCE AMBIGUITIES
C -------------------------------
      DO 800 IREF=1,NREF
        IDEL(REFAMB(IREF))=1
800   CONTINUE
C
C ELIMINATE REFERENCE IONOSPHERE PARAMETERS, SAVE REF. SAT. NUMBERS
C AND THEIR MEAN ZENITH DISTANCES (IN ARC SEC)
C -----------------------------------------------------------------
      DO 820 IRIP=1,NUMRIP
        DO 825 IPAR=1,NPARN
          IF(LOCQ(1,IPAR).EQ.17.AND.
     1       LOCQ(2,IPAR).EQ.LOCQ(2,REFDIP(IRIP)).AND.
     2       LOCQ(4,IPAR).EQ.LOCQ(4,REFDIP(IRIP)))THEN
            IF(LOCQ(3,IPAR).EQ.LOCQ(3,REFDIP(IRIP)))THEN
              IDEL(IPAR)=1
            ELSE
              LOCQ(5,IPAR)=LOCQ(3,REFDIP(IRIP))
              LOCQ(7,IPAR)=LOCQ(6,REFDIP(IRIP))
            ENDIF
          ENDIF
825     CONTINUE
820   CONTINUE
C
C FLAG UNOBSERVED IONOSPHERE PARAMETERS
C -------------------------------------
      DO IPAR=1,NPARN
        IF (LOCQ(1,IPAR).EQ.17 .AND.
     1    ANOR(IKF(IPAR,IPAR)).EQ.0.D0) THEN
          IDEL(IPAR)=1
          NPARMS=NPARMS-1
          PARLST(1,17)=PARLST(1,17)-1
          PARLST(3,17)=PARLST(3,17)+1
          PARLST(4,17)=PARLST(4,17)-1
        ENDIF
      ENDDO
C
C FINAL REDUCTION OF NORMAL EQUATIONS
C -----------------------------------
      NDEL=0
      NDAMB=0
      IPARP=0
      DO 20 IPAR=1,NPAR
        IPARP=IPARP+1
        DO 10 INUL=1,100000
          IF (INDP(IPARP).NE.0) GOTO 15
          IPARP=IPARP+1
10      CONTINUE
15      CONTINUE
        IF(IDEL(IPAR).EQ.0) THEN
          INDP(IPARP)=IPAR-NDEL
        ELSE
          NDEL=NDEL+1
          INDP(IPARP)=0
          IF (IPAR.GT.NPARN) THEN
            NDAMB=NDAMB+1
            IDEL(NPAR+NDAMB)=0
            DO 30 ILCQ=1,MXCLCQ
              LOCQ(ILCQ,NPAR+NDAMB) = LOCQ(ILCQ,IPAR)
30          CONTINUE
            OBSCLS(NAMB+NDAMB)=OBSCLS(IPAR-NPARN)
          END IF
        END IF
20    CONTINUE
C
      CALL DMATRD('S',NPAR,NPAR,IDEL,ANOR,NPNEW,NPNEW)
      CALL DMATRD('R',NPAR,1,IDEL,BNOR,NPNEW,IONE)
      CALL DMATRD('R',NPAR,1,IDEL,XXX0,NPNEW,IONE)
      CALL IMATRD('C',MAXMEA*MAXSYS,NPAR,IDEL,NOBSPA,ISIX,NPNEW)
      CALL IMATRD('C',MAXLCQ,NPALCQ+NDAMB,IDEL,LOCQ,ISIX,NPNEW)
C
      CALL red_parTyp(npar,idel,partyp)
C
      IAMNEW=0
      DO 40 IAMP=1,NAMB+NDAMB
        IF (IDEL(NPARN+IAMP).EQ.1) GO TO 40
        IAMNEW=IAMNEW+1
        OBSCLS(IAMNEW)=OBSCLS(IAMP)
40    CONTINUE
C
      NPAR=NPAR-NDEL
      NAMB=NAMB-NDAMB
      NDELN=NDEL-NDAMB
      NPARN=NPARN-NDELN
      NPARMS=NPARMS-NDAMB
C
C UPDATE NUMBER OF REFERENCE PARAMETERS
C -------------------------------------
      PARLST(4, 4)=PARLST(4, 4)+NDAMB
      PARLST(4,17)=PARLST(4,17)+NDELN
C
C SET REF. AMB. (ONE PER CLUSTER) IN LOCQ(7,IPAR)
C -----------------------------------------------
      DO 900 IAMB=1,NAMB+NREF
        IPAMB=NPARN+IAMB
        DO 950 IREF=1,NREF
          IPREF=NPAR+IREF
          IF (LOCQ(2,IPREF).EQ.LOCQ(2,IPAMB) .AND.
     1        LOCQ(5,IPREF).EQ.LOCQ(5,IPAMB) .AND.
     2        OBSCLS(NAMB+IREF).EQ.OBSCLS(IAMB)) THEN
            LOCQ(7,IPAMB)=IPREF
            GOTO 900
          ENDIF
950     CONTINUE
C
C NO REFERENCE: GPS/GLONASS COMBINED
        LOCQ(7,IPAMB)=IPAMB
900   CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
