      MODULE s_RESOUT
      CONTAINS

C*
      SUBROUTINE RESOUT(TITLE ,NFTOT ,STFIL ,STNAME,TIMREF,CSESS ,
     1                  NFRFIL,ICARR ,MEATYP,NPAR  ,ANOR  ,XXX   ,
     2                  INDP  ,INDA  ,ASING ,BSING ,WEIGHT,ISV12 ,
     3                  IFIL  ,IOBNUM,IFRSES,IDELTT,NSATEL,SATNUM,
     4                  NDIFF ,NORRES)
CC
CC NAME       :  RESOUT
CC
CC PURPOSE    :  WRITE RESIDUALS OF PROGRAM GPSEST TO AN OUTPUT FILE
CC               OPTIONAL "NORMALIZED" RESIDUALS CAN BE COMPUTED
CC               USING THE RESIDUAL COVARIANCE MATRIX:
CC                                              T
CC                             Qvv = W - A Qxx A
CC
CC               THE NORMILIZED RESIDUALS ARE THEN:
CC                             RESN(i) = RES(i)/DSQRT(Qvv(i,i))
CC
CC PARAMETERS :
CC         IN :  TITLE  : TITLE LINE                          CH*80
CC               NFTOT  : TOTAL NUMBER OF FILES               I*4
CC               STFIL(K,I),K=1,2, I=1,..,NFTOT: STATION NRS  I*4
CC                        INVOLVED IN FILE I
CC               STNAME(I),I=1,2,..: STATION NAMES            CH*16
CC               TIMREF(I),I=1,2,..,NFTOT: REFERENCE TIME FOR R*8
CC                        FOR FILE I
CC               CSESS(K,I),K=1,2, I=1,..,NFTOT:              CH*4
CC                        K=1: SESSION IDENTIFIER,
CC                        K=2: FILE IDENTIFIER IN SESSION
CC               NFRFIL(I),I=1,2,...,NFTOT: NUMBER OF REQ.    I*4
CC                        FREQUENCIES PER FILE
CC               ICARR(K,I),K=1,..,NFRFIL(I),I=1,..,NFTOT     I*4
CC                        K=1:FIRST REQ. FREQ, K=2: SECOND ...
CC               MEATYP(I),I=1,2,..,NFTOT: MEASUREMENT TYPE   I*4
CC                        IN FILE I
CC               NPAR   : NUMBER OF PARAMETERS                I*4
CC               ANOR(I),I=1,..,NPAR*(NPAR+1)/2               R*8
CC               XXX(I),I=1,2,..,NPAR: SOLUTION VECTOR        R*8
CC               INDP(I),I=1,2,.. :AUX. ARRAY                 I*4
CC               INDA(I),I=1,2,.. :AUX. ARRAY                 I*2
CC               ASING(I),I=1,2,.. : NON ZERO ELEMENTS OF ALL R*8
CC                        DOUBLE DIFF OBS EQNS OF ONE EPOCH
CC               BSING(I),I=1,2,..: OBS-COMP FOR ALL OBSER-   R*8
CC                        VATIONS OF ONE EPOCH
CC               WEIGHT(I),I=1,2,..: WEIGHT MATRIX OF ALL SIM. R*8
CC                        OBS. EQNS
CC               ISV12(2,I),I=1,2,..: AUX. ARRAY              I*4
CC               IFIL(I),I=1,2,..: AUX. ARRAY                 I*4
CC               IOBNUM(I),I=1,2,..: AUX. ARRAY               I*4
CC               IFRSES(I),I=1,2,..: AUX. ARRAY               I*4
CC               IDELTT(I),I=1,2,..,NFTOT: SPACING BETWEEN    I*4
CC                        OBSERVATIONS (SEC)
CC               NSATEL(I),I=1,..,NFTOT: NUMBER OF SATELLITES I*4(*)
CC               SATNUM(K,I),K=1,..,NSATEL(I),I=1,..,NFTOT:   I*4
CC                        SATELLITE NUMBERS
CC               NDIFF(I):I=1,..,NFTOT: DIFFERENCE TYPE       I*4
CC                        NDIFF=0: ZERO DIFFERENCE
CC                        NDIFF=1: SINGLE DIFFERENCE
CC               NORRES:  FLAG FOR RESIDUAL NORMALIZATION     I*4
CC                        =1 REAL RESIDUALS
CC                        =2 NORALIZATION WEIGHT+APOST. COV
CC                        =3 NORALIZATION ON APRIORI WEIGHTS
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/12/11 12:02
CC
CC CHANGES    :  10-JUN-92 : LM: WRITE THE FLAG INTO THE FILE
CC               12-MAR-93 : MR: SKIP REFERENCE AMBIGUITY
CC               14-AUG-94 : MR: FORMAT 4: SESSION AS CHARACTER*4
CC               24-APR-96 : TS: ZERO DIFFERENCE PROCESSING CHANGES
CC               26-APR-96 : TS: SMALL "NDIFF" BUG CORRECTED
CC               14-MAY-96 : TS: SMALL BUG CORRECTED
CC               13-NOV-96 : HM: ADD PARAMETER "IFRSES" TO THE CALL
CC                               TO HANDLE PROPERLY CORRELATION
CC                               STRATEGY 3; ALLOW FOR "ITYP=2"
CC               09-OCT-97 : TS: OPTIONAL NORMALIZED RESIDUALS
CC               10-OCT-97 : TS: SMALL BUG CORRECTED (TEST=0)
CC               25-NOV-97 : TS: MAXSAS IN INCLUDED FILE
CC               30-MAR-98 : TS: SLIGHTLY CHANGED SCRATCH FORMAT (2 DUMMY'S)
CC               27-JAN-00 : TS: ABUSE ISV12 FOR ELE AND AZI IF NDIFF=1
CC               29-JAN-02 : RD: SKIP SR IF NO RESIDRS SPECIFIED
CC               09-FEB-02 : RD: WEIGHT REAL*4->REAL*8
CC               27-AUG-02 : RD: HANDLE NEW FORMATTED RESIDUAL FILES
CC               16-SEP-02 : SS: PREVENT INDEXING WITH 0
CC               18-SEP-02 : RD: RESIDUAL NORMALIZATION IMPROVED
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               26-MAR-03 : RD: DO NOT READ SCRATCH FILE IF IT IS NOT OPEN
CC               15-May-03 : HB: INITIALIZE STRUCTURE
CC               28-MAY-03 : RD: CORRECT SETTING OF RESHED - ARRAY
CC               22-JUL-03 : RD: NORMALIZED ON APRIORI WEIGHTS ONLY
CC               10-MAR-04 : HB: CHANGE ORDER OF MODULES
CC               08-JUL-04 : RD: USE SYMIN8-HELP ARRAYS FROM GPSEST
CC               11-OCT-04 : CU: CLOSE LFNRES
CC               16-NOV-04 : RD: GET SOME DIMENSIONS AS PARAMETERS FROM GPSEST
CC               04-JAN-05 : RD: REMOVE UNUSED PARAMETERS
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               02-SEP-05 : HB: SPLIT SCRATCH FILE INTO FOUR FILES
CC                               (ONLY QUICK SOLUTION, NOT GENERALLY SOLVED)
CC               14-SEP-05 : HB: SCRATCH FILE SPLITTING GENERALLY SOLVED
CC               01-DEC-05 : CU: CLOSE LFNLOC IF NO OBSERVATIONS FOUND
CC               23-FEB-06 : RD: REMOVE DTFIL FROM SCRATCH RECORD
CC               21-MAY-10 : MF: CALL SR INIT_FILHEAD
CC               10-JUL-12 : RD: USE SYMINVG INSTEAD OF SYMIN8
CC               10-JUL-12 : RD: USE M_BERN WITH ONLY
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnerr, lfnloc, lfnres
      USE d_resFil, ONLY: t_resHead, resHed_GPSEST0, resHed_GPSEST1,
     1                    init_reshead, init_filhead
      USE d_const, ONLY: WGTCOD, WGTPHA
      USE f_ikf
      USE s_alcerr
      USE s_inquire
      USE s_syminvg
      USE s_wtresh2
      USE s_gtflna
      USE s_opnfil
      USE s_opnerr
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IDUMM1, IDUMM2, IEPO  , IND   , IO    ,
     1          IPAR  , IRC   , IRCRES, IREC  , ISING , J     , JFIL  ,
     2          K     , KPAR  , MAXEQN, MXCEQN, MXCFRQ, MXCSAT, MXESNG,
     3          NDIM  , NFTOT , NITEM , NORRES, NPAR  , NREC
C
      REAL*8    SIGV  , TEST  , WGTT
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
C
      TYPE(t_resHead) reshed
C
      CHARACTER*255 NEWFIL
      CHARACTER*80 TITLE
      CHARACTER*32 FILNAM,FILAUX
      CHARACTER*16 STNAME(*)
      CHARACTER*6  MXNFRQ,MXNEQN,MXNSAT
      CHARACTER*4  CSESS(2,*)
      CHARACTER*1  FLGAMB
C
      REAL*8       TIMREF(*),ANOR(*),XXX(*),ASING(*),BSING(*)
      REAL*8       HELP1(NPAR)
      REAL*8       VSIG(1000)
      REAL*8       WEIGHT(*),WGTGEN(3)
C
      INTEGER*4    NFRFIL(*),ICARR(MXCFRQ,*),MEATYP(*),NSATEL(*)
      INTEGER*4    INDP(*),STFIL(2,*),IDELTT(*),ISV12(2,MXCEQN)
      INTEGER*4    IFIL(MXCEQN),IOBNUM(MXCEQN),SATNUM(MXCSAT,*),NDIFF(*)
      INTEGER*4    IFRSES(MXCEQN)
      INTEGER*4    INDA(*)
      INTEGER*4    IOSTAT
C
      LOGICAL      YES
C
      COMMON/CRSOUT/VSIG
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMFRQ/MXCFRQ,MXNFRQ
      COMMON/MCMEQN/MXCEQN,MXNEQN
C
      DATA FLGAMB/' '/
C
C NULLIFY POINTERS
C ----------------
      CALL init_reshead(resHed)

C INITIALIZE SOME VARIABLES
C -------------------------
      WGTGEN(1)=1.D0
      WGTGEN(2)=WGTCOD/WGTPHA
      WGTGEN(3)=1.D0
C
C IS THERE SOMETHING TO DO?
C -------------------------
      CALL GTFLNA(0,'RESIDRS',FILNAM,IRCRES)
      IF (IRCRES.NE.0) RETURN
C
C INIT RESIDUAL HEADER RECORD
C ---------------------------
      RESHED%TITLE = TITLE
      IF (NDIFF(1).EQ.0) THEN
        RESHED%DSC  = resHed_GPSEST0
      ELSE
        RESHED%DSC  = resHed_GPSEST1
      ENDIF
      RESHED%DSC%NPAR = NPAR
      RESHED%NFIL = NFTOT
      ALLOCATE(RESHED%FILHEAD(NFTOT),STAT=irc)
      CALL ALCERR(IRC,'RESHED%FILHEAD',(/NFTOT/),'RESEPO')
      DO JFIL=1,NFTOT
        CALL init_filhead(RESHED%FILHEAD(JFIL))
        IF (NFRFIL(JFIL) .NE.2 .OR.
     1      ICARR(1,JFIL).NE.1 .OR. ICARR(2,JFIL).NE.2)
     2    RESHED%DSC%ITYP=1
        RESHED%FILHEAD(JFIL)%MEATYP = MEATYP(JFIL)
        RESHED%FILHEAD(JFIL)%NFRFIL = NFRFIL(JFIL)
        RESHED%FILHEAD(JFIL)%ICARR(1:NFRFIL(JFIL)) =
     1                                   ICARR(1:NFRFIL(JFIL),JFIL)
        RESHED%FILHEAD(JFIL)%STANAM(1) = STNAME(STFIL(1,JFIL))
        IF (NDIFF(1).GT.0)
     1    RESHED%FILHEAD(JFIL)%STANAM(2) = STNAME(STFIL(2,JFIL))
        RESHED%FILHEAD(JFIL)%CSESS(:) = CSESS(:,JFIL)
        RESHED%FILHEAD(JFIL)%IDELTT = IDELTT(JFIL)
        RESHED%FILHEAD(JFIL)%TIMREF = TIMREF(JFIL)
        RESHED%FILHEAD(JFIL)%NSATEL = NSATEL(JFIL)
        ALLOCATE(RESHED%FILHEAD(JFIL)%NUMSAT(NSATEL(JFIL)),STAT=IRC)
        CALL ALCERR(IRC,'RESHED%FILHEAD(JFIL)%NUMSAT',
     1             (/NSATEL(JFIL)/),'RESEPO')
        RESHED%FILHEAD(JFIL)%NUMSAT(:) = SATNUM(1:NSATEL(JFIL),JFIL)
      ENDDO
C
C WRITE THE RESIDUAL FILE HEADER
C ------------------------------
      CALL WTRESH2(LFNLOC,RESHED)
C
      DO JFIL=1,NFTOT
        DEALLOCATE(RESHED%FILHEAD(JFIL)%NUMSAT,STAT=IRC)
      ENDDO
      DEALLOCATE(RESHED%FILHEAD,STAT=irc)
C
C RETURN IF SCRATCH FILE IS NOT AVAILABLE
C (E.G., NO OBSERVATIONS FOUND IN PRCEPO)
C -------------------------------------
      CALL INQUIRE(UNIT=LFNRES,OPENED=YES)
      IF (.NOT. YES) THEN
        CLOSE(UNIT=LFNLOC)
        RETURN
      ENDIF
C
C REWIND FILE WITH OBSERVATION EQUATIONS
CC      REWIND LFNRES
      CLOSE(unit=lfnres)
      CALL GTFLNA(1,'AUXFIL ',FILAUX,IRC)
      CALL OPNFIL(LFNRES,FILAUX,'UNKNOWN','UNFORMATTED',
     1     ' ',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNRES,IOSTAT,FILAUX,'RESOUT')
C
C COMPUTE AND WRITE ALL RESIDUALS
C -------------------------------
      DO 200 IEPO=1,1000000
C
C READ FIRST RECORD OF ONE EPOCH
        READ(LFNRES,END=210)NDIM,MAXEQN,MXESNG,IDUMM1,IDUMM2

C END OF SCRATCH FILE => NEW FILE HAS TO BE OPENED
        IF (NDIM == -1) THEN
          READ(LFNRES)newFil
          CLOSE(unit=LFNRES)
          CALL OPNFIL(LFNRES,NEWFIL,'UNKNOWN','UNFORMATTED',
     1                ' ',' ',IOSTAT)
          CALL OPNERR(LFNERR,LFNRES,IOSTAT,NEWFIL,'WTRESI')
        READ(LFNRES,END=210)NDIM,MAXEQN,MXESNG,IDUMM1,IDUMM2
        ENDIF

        NREC=(NDIM+1)/2
        NITEM=NDIM*(NDIM+1)/2/NREC
C
C READ WEIGHT MATRIX
        DO 120 IREC=1,NREC
          READ(LFNRES)(WEIGHT((IREC-1)*NITEM+I),I=1,NITEM)
120     CONTINUE
C
        IF (NORRES.EQ.2.OR.NORRES.EQ.3) THEN
          CALL SYMINVG(NDIM,WEIGHT,1,ISING)
        ENDIF
C
C READ MATRIX ASING, INDA AND BSING
        DO 130 IO=1,NDIM
          READ(LFNRES)IFIL(IO),IFRSES(IO),IOBNUM(IO),
     1                (ISV12(K,IO),K=1,2),
     2                (ASING(IO+(K-1)*MAXEQN),INDA(IO+(K-1)*MAXEQN),
     3                K=1,MXESNG),BSING(IO)
130     CONTINUE
C
C LOOP OVER ALL OBSERVATIONS (DOUBLE DIFFERENCES)
C -----------------------------------------------
        DO 150 I=1,NDIM
          IF (NORRES.EQ.2) THEN
            DO J=1,NPAR
              HELP1(J)=0D0
            ENDDO
          ENDIF
C
C COMPUTE RESIDUALS OF EPOCH
C --------------------------
          VSIG(I)=-BSING(I)
          DO 140 K=1,MXESNG
            IND=I+(K-1)*MAXEQN
            KPAR=INDA(IND)
            IF (KPAR.EQ.0) GOTO 145
            IPAR=INDP(KPAR)
            IF (IPAR.EQ.0) GOTO 140
            VSIG(I)=VSIG(I)+ASING(IND)*XXX(IPAR)
C
C COMPUTE HELP MATRIX FOR THE RESIDUAL VARIANCES (HELP = AOBS*ANOR)
C -----------------------------------------------------------------
            IF (NORRES.EQ.2) THEN
              DO 160 J=1,NPAR
                HELP1(J)=HELP1(J)+ASING(IND)*ANOR(IKF(IPAR,J))
160           CONTINUE
            ENDIF
140       CONTINUE
145       CONTINUE
C                                               T
C COMPUTE THE RESIDUAL VARIANCES (AOBS*ANOR*AOBS ) (HELP1=AOBS*ANOR)
C ------------------------------------------------------------------
          IF (NORRES.EQ.2) THEN
            WGTT=0D0
            DO 170 K=1,MXESNG
              IND=I+(K-1)*MAXEQN
              KPAR=INDA(IND)
              IF (KPAR.EQ.0) GOTO 149
              IPAR=INDP(KPAR)
              IF (IPAR.EQ.0) GOTO 170
              WGTT=WGTT+HELP1(IPAR)*ASING(IND)
170         CONTINUE
          ENDIF
C
149       CONTINUE
C
C COMPUTE NORMALIZED RESIDUAL
C ---------------------------
          IF (NORRES.EQ.2) THEN
            TEST=WEIGHT(IKF(I,I))-WGTT*(WGTGEN(MEATYP(ifil(i))))
          ELSE IF (NORRES.EQ.3) THEN
            TEST=WEIGHT(IKF(I,I))
          ENDIF
C
          IF (NORRES.EQ.2.OR.NORRES.EQ.3) THEN
            IF (TEST.LE.0.0001) THEN
              TEST=1d0/WGTGEN(MEATYP(IFIL(I)))
            ENDIF
            SIGV=DSQRT(TEST)
            VSIG(I)=VSIG(I)/SIGV
          ENDIF
C
C NEXT RESIDUAL
C -------------
150     CONTINUE
C
C WRITE RESIDUALS ON OUTPUT FILE
C ------------------------------
        DO 300 I=1,NDIM
          WRITE(LFNLOC) IFIL(I),IOBNUM(I),IFRSES(I),
     1         (ISV12(K,I),K=1,2),VSIG(I),FLGAMB
300     CONTINUE
200   CONTINUE
210   CONTINUE
C
      CLOSE(UNIT=LFNLOC)
      CLOSE(UNIT=LFNRES)
C
      RETURN
      END SUBROUTINE

      END MODULE
