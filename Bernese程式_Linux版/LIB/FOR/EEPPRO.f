      MODULE s_EEPPRO
      CONTAINS

C*
      SUBROUTINE EEPPRO(REFEPO,SUMPER,NPAR,PARNAM,PARTIM,RMS,
     1                  XXXAPR,XXX,ANOR)
CC
CC NAME       :  EEPPRO
CC
CC PURPOSE    :  PRINT PROGRADE AND RETROGRADE TERMS
CC
CC PARAMETERS :
CC        IN  :  REFEPO(I),I=1,2: REFERENCE EPOCHS               R*8
CC                        (1): REFERENCE EPOCH FOR DRIFT
CC                             PARAMETERS, FIRST EPOCH PROCESSED
CC                        (2): LAST EPOCH PROCESSED
CC               SUMPER : MAXIMUM PERIOD FOR STATISTICS          R*8
CC               NPAR   : MAXIMUM NUMBER OF PARAMETERS ALLOWED   I*4
CC               PARNAM(I),I=1,..,NPAR: PARAMETER NAMES         CH*20
CC               PARTIM(2,I),I=1,..,NPAR: PARAMETER WINDOWS      R*8
CC                          FROM,TO IN MJD
CC               RMS    : SUM OF O-C**2                          R*8
CC               XXXAPR(I),I=1,..,NPAR: A PRIORI VALUES OF PARA. R*8
CC               XXX(I),I=1,..,NPAR: SOLUTION VECTOR             R*8
CC               ANOR(I),I=1,..,NPAR*(NPAR+1)/2: NORMAL EQUATION R*8
CC                        MATRIX
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  07-NOV-97
CC
CC CHANGES    :  12-AUG-03 : PS: CALL TO F90-SR RDNUTM
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-JUN-05 : MM: UNUSED VARIABLES REMOVED
CC               16-JUL-04 : PS: INCREASED NUMBER OF DIGITS IN OUTPUT
CC               08-AUG-05 : HB: USE NEW SR TIMST2 (module)
CC               28-FEB-07 : AG: USE 206264... FROM DEFCON
CC               21-OCT-08 : HB: USE s_nutval ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*

! Modules
! -------
      USE m_bern
      USE d_const,  ONLY : ars
      USE d_nutmod, ONLY : t_nutat

      USE s_rdnutm
      USE f_ikf
      USE s_proret
      USE s_timst2
      USE s_nutval
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 ICMP  , IFIRST, II    , IPA1  , IPAR  , IPRINT,
     1          IRCNUT, ITR2  , ITRM  , ITYP  , NPAR  , NTERM
C
      REAL*8    AMPRM2, ARG   , ARGR  , EPS0  , EPS0AS, FAC   , PERIOD,
     1          RMS   , SINE0 , SUMPER
C
CCC       IMPLICIT REAL*8(A-H,O-Z)

! Local types
! -----------
      TYPE(t_nutat)                  :: nutat

! Local Parameters
! ----------------
      INTEGER(i4b)                   :: maxPer
      PARAMETER (MAXPER=1000)
C
      CHARACTER*40 TSTRNG
      CHARACTER*32 FILNUT
      CHARACTER*20 PARNAM(*),PARTRM
      CHARACTER*6  AMPTXT(4),AMPTX2(4)
C
      REAL*8       PARTIM(2,*),XXXAPR(*),XXX(*),ANOR(*)
      REAL*8       AMPAPR(4),AMPLI(4),AMPRMS(4),AMPRMA(4)
      REAL*8       RMSDIF(2,4),RMSWGT(2,4),RMSDTT(2),RMSWTT(2)
      REAL*8       MEADIF(2,4),MEADTT(2),SUMWGT(2,4),SUMWTT(2)
      REAL*8       REFEPO(2),WINDOW(2)
C
      INTEGER*4    INDTRM(4),IARG(14)
C
      DATA IFIRST/1/
      DATA EPS0AS/84381.412D0/
      DATA AMPTXT/'A+ IN ','A+ OUT','A- IN ','A- OUT'/
      DATA AMPTX2/'DPSI S','DPSI C','DEPS C','DEPS S'/
C
      IF (IFIRST.EQ.1) THEN
C
C OBLIQUITY OF ECLIPTIC
C ---------------------
        EPS0=EPS0AS/ars
        SINE0=DSIN(EPS0)
C
C INITIALIZE "IARG"
C -----------------
        DO ICMP=1,14
          IARG(ICMP)=0
        ENDDO
C
C GET FUNDAMENTAL ARGUMENTS
C -------------------------
        CALL GTFLNA(0,'NUTREF ',FILNUT,IRCNUT)
        IF (IRCNUT.EQ.0) THEN
          CALL rdnutm(filnut,nutat)
        ELSE
          nutat%NNUT=0
        ENDIF
C
      ENDIF
C
      IPRINT=1
C
C INITIALIZE RMS OF DIFFERENCES BETWEEN REFERENCE MODEL AND ESTIMATION
C --------------------------------------------------------------------
      NTERM=0
      DO ITYP=1,2
        DO ITRM=1,4
          MEADIF(ITYP,ITRM)=0.D0
          RMSDIF(ITYP,ITRM)=0.D0
          RMSWGT(ITYP,ITRM)=0.D0
          SUMWGT(ITYP,ITRM)=0.D0
        ENDDO
      ENDDO
C
C LOOP OVER ALL FREQUENCIES
C -------------------------
      DO IPAR=1,NPAR
        IF (PARNAM(IPAR)(1:2).EQ.'ES') THEN
          DO ITRM=1,4
            INDTRM(ITRM)=0
          ENDDO
          PARTRM=PARNAM(IPAR)
C
C SEARCH FOR CORRESPONDING TERMS
          DO IPA1=1,NPAR
            IF (PARNAM(IPA1)(3:).EQ.PARTRM(3:)   .AND.
     1          PARTIM(1,IPA1).EQ.PARTIM(1,IPAR) .AND.
     2          PARTIM(2,IPA1).EQ.PARTIM(2,IPAR)) THEN
              IF (PARNAM(IPA1)(1:2).EQ.'ES') INDTRM(4)=IPA1
              IF (PARNAM(IPA1)(1:2).EQ.'EC') INDTRM(3)=IPA1
              IF (PARNAM(IPA1)(1:2).EQ.'PS') INDTRM(1)=IPA1
              IF (PARNAM(IPA1)(1:2).EQ.'PC') INDTRM(2)=IPA1
            ENDIF
          ENDDO
C
C ERROR MESSAGE IF ONE TERM NOT FOUND
          DO ITRM=1,4
            IF (INDTRM(ITRM).EQ.0) THEN
              WRITE(LFNERR,910) PARTRM
910           FORMAT(/,' *** SR EEPPRO: NOT ALL PARAMETERS OF ONE',
     1                 ' FREQUENCY FOUND TO COMPUTE',
     2               /,16X,'PROGRADE AND RETROGARDE TERMS',
     3               /,16X,'PARAMETER: ',A,/)
              GOTO 10
            ENDIF
          ENDDO
C
C COMPUTE PROGARDE AND RETROGARDE AMPLITUDES AND THEIR FORMAL ERRORS
C AMPLI: A+IN, A-IN,A+OUT,A-OUT   (IN: COS, OUT: SIN)
C
          CALL PRORET(INDTRM,RMS,XXX,ANOR,AMPLI,AMPRMS)
          CALL PRORET(INDTRM,0.D0,XXXAPR,ANOR,AMPAPR,AMPRMA)
C
C GET PERIOD OF TERM
          READ(PARNAM(IPAR)(3:17),'(5I3)') (IARG(II),II=1,5)
          IF (IRCNUT.EQ.0) THEN
            CALL NUTVAL(REFEPO(1),nutat%NUTFAR,IARG,ARG,ARGR,PERIOD)
          ELSE
            PERIOD=0.D0
          ENDIF
C
C SUM UP DIFFERENCES BETWEEN ESTIMATION AND REFERENCE MODEL
          IF (PERIOD.LE.SUMPER) THEN
            NTERM=NTERM+1
            DO ITRM=1,4
C
              MEADIF(1,ITRM)=MEADIF(1,ITRM)+DABS(AMPLI(ITRM))
              RMSDIF(1,ITRM)=RMSDIF(1,ITRM)+AMPLI(ITRM)**2
              RMSWGT(1,ITRM)=RMSWGT(1,ITRM)+
     1                       (AMPLI(ITRM)/AMPRMS(ITRM))**2
              SUMWGT(1,ITRM)=SUMWGT(1,ITRM)+1.D0/AMPRMS(ITRM)**2
C
              ITR2=INDTRM(ITRM)
              AMPRM2=RMS*DSQRT(ANOR(IKF(ITR2,ITR2)))
              MEADIF(2,ITRM)=MEADIF(2,ITRM)+DABS(XXX(ITR2))
              RMSDIF(2,ITRM)=RMSDIF(2,ITRM)+XXX(ITR2)**2
              RMSWGT(2,ITRM)=RMSWGT(2,ITRM)+(XXX(ITR2)/AMPRM2)**2
              SUMWGT(2,ITRM)=SUMWGT(2,ITRM)+1.D0/AMPRM2**2
C
            ENDDO
          ENDIF
C
C PRINT TITLE LINES
          IF (IPRINT.EQ.1) THEN
            IPRINT=0
            WRITE(LFNPRT,1001)
1001        FORMAT(1X,'AMPLITUDES (MAS):',/,
     1             1X,16('-'),//
     2             1X,'PARAMETER            A PRIORI     ESTIMATED',
     3                '       EST-APR        RMS')
          ENDIF
C
C PRINT PRO- AND RETROGRADE TERMS
          WINDOW(1)=DMAX1(PARTIM(1,IPAR),REFEPO(1))
          WINDOW(2)=DMIN1(PARTIM(2,IPAR),REFEPO(2))
          CALL TIMST2(1,2,WINDOW,TSTRNG)
          WRITE(LFNPRT,1002) PARNAM(IPAR)(3:17),PERIOD,TSTRNG
1002      FORMAT(/,1X,A15,F14.3,' days   Interval: ',A)
          DO ITRM=1,4
            WRITE(LFNPRT,1003) AMPTXT(ITRM),AMPAPR(ITRM),
     1                         AMPLI(ITRM)+AMPAPR(ITRM),
     2                         AMPLI(ITRM),AMPRMS(ITRM)
1003        FORMAT(1X,A6,9X,4F14.4)
          ENDDO
          DO ITRM=1,4
            ITR2=INDTRM(ITRM)
            AMPRM2=RMS*DSQRT(ANOR(IKF(ITR2,ITR2)))
            WRITE(LFNPRT,1003) AMPTX2(ITRM),XXXAPR(ITR2),
     1                         XXXAPR(ITR2)+XXX(ITR2),
     2                         XXX(ITR2),AMPRM2
          ENDDO
C
        ENDIF
10      CONTINUE
C
      ENDDO
C
      WRITE(LFNPRT,'(/)')
C
C WRITE RMS OF DIFFERENCES BETWEEN ESTIMATION AND REFERENCE MODEL
C ---------------------------------------------------------------
      DO ITYP=1,2
        MEADTT(ITYP)=0.D0
        RMSDTT(ITYP)=0.D0
        RMSWTT(ITYP)=0.D0
        SUMWTT(ITYP)=0.D0
        DO ITRM=1,4
          IF (ITRM.LT.3 .AND.ITYP.EQ.2) THEN
            FAC=SINE0
          ELSE
            FAC=1.D0
          ENDIF
          MEADTT(ITYP)=MEADTT(ITYP)+MEADIF(ITYP,ITRM)*FAC
          RMSDTT(ITYP)=RMSDTT(ITYP)+RMSDIF(ITYP,ITRM)*FAC**2
          RMSWTT(ITYP)=RMSWTT(ITYP)+RMSWGT(ITYP,ITRM)
          SUMWTT(ITYP)=SUMWTT(ITYP)+SUMWGT(ITYP,ITRM)/FAC
        ENDDO
      ENDDO
C
      IF (NTERM.NE.0) THEN
        DO ITYP=1,2
          MEADTT(ITYP)=MEADTT(ITYP)/(NTERM*4)
          RMSDTT(ITYP)=DSQRT(RMSDTT(ITYP)/(NTERM*4))
          RMSWTT(ITYP)=DSQRT(RMSWTT(ITYP)/(NTERM*4))
CCC          RMSWTT(ITYP)=DSQRT(RMSWTT(ITYP)/SUMWTT(ITYP))
          DO ITRM=1,4
            MEADIF(ITYP,ITRM)=MEADIF(ITYP,ITRM)/NTERM
            RMSDIF(ITYP,ITRM)=DSQRT(RMSDIF(ITYP,ITRM)/NTERM)
            RMSWGT(ITYP,ITRM)=
     1        DSQRT(RMSWGT(ITYP,ITRM)/NTERM)
CCC            RMSWGT(ITYP,ITRM)=
CCC     1        DSQRT(RMSWGT(ITYP,ITRM)/SUMWGT(ITYP,ITRM))
          ENDDO
        ENDDO
C
        WRITE(LFNPRT,1004) SUMPER,NTERM,
     1                     (MEADTT(II),RMSDTT(II),RMSWTT(II),II=1,2)
1004    FORMAT(1X,'RMS OF DIFFERENCES "EST-APR":',/,1X,28('-'),//,
     1         1X,'LONGEST PERIOD CONSIDERED:',F10.3,' DAYS',/,
     2         1X,'NUMBER OF FREQUENCIES    :',I10,//,
     3         1X,'PARAMETERS             MEAN ABS     UNWEIGHTED',
     4            '     WEIGHTED',//,
     5         1X,'PRO/RET',8X,F15.4,2F14.4,/
     6         1X,'PSI/EPS',8X,F15.4,2F14.4,/)
C
        DO ITRM=1,4
          WRITE(LFNPRT,1005) AMPTXT(ITRM),MEADIF(1,ITRM),
     1                       RMSDIF(1,ITRM),RMSWGT(1,ITRM)
1005      FORMAT(1X,A6,9X,F15.4,2F14.4)
        ENDDO
        DO ITRM=1,4
          WRITE(LFNPRT,1005) AMPTX2(ITRM),MEADIF(2,ITRM),
     1                       RMSDIF(2,ITRM),RMSWGT(2,ITRM)
        ENDDO
C
        WRITE(LFNPRT,'(/)')
      ENDIF
C
C END
      RETURN
      END SUBROUTINE

      END MODULE
