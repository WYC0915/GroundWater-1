      MODULE s_EEINIT
      CONTAINS

C*
      SUBROUTINE EEINIT(NPAR,PARNAM,PARTIM,PARSIG,NOBS,
     1                  RMS,ANOR,BNOR,XXXAPR)
CC
CC NAME       :  EEINIT
CC
CC PURPOSE    :  INITIALIZE NORMAL EQUATION MATRICES FOR PROGRAM ERPEST
CC
CC PARAMETERS :
CC        IN  :  NPAR   : MAXIMUM NUMBER OF PARAMETERS ALLOWED   I*4
CC               PARNAM(I),I=1,..,NPAR: PARAMETER NAMES         CH*20
CC               PARTIM(2,I),I=1,..,NPAR: PARAMETER TIME         R*8
CC                        INTERVAL
CC               PARSIG(I),I=1,..,NPAR: A PRIORI SIGMAS FOR      R*8
CC                        PARAMETERS
CC        OUT :  NOBS   : NUMBER OF OBSERVATIONS                 I*4
CC               RMS    : RMS ERROR                              R*8
CC               ANOR(I),I=1,..,NPAR*(NPAR+1)/2: FIRST DESIGN    R*8
CC                        MATRIX
CC               BNOR(I),I=1,..,NPAR: B MATRIX                   R*8
CC               XXXAPR(I),I=1,..,NPAR: A PRIORI PARAM. VALUES   R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  07-NOV-97
CC
CC CHANGES    :  02-JUN-03 : PS: CALL TO F90-SR RDNUTM
CC               12-AUG-03 : PS: CORRECTED ORDER OF NUTCOE
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-JUN-05 : MM: UNUSED VARIABLES REMOVED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*

! Modules
! -------
      USE m_bern
      USE d_nutmod, ONLY: t_nutat

      USE f_ikf
      USE s_rdnutm
      USE s_rdsubm
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IARG  , II    , INUT  , IPA1  , IPA2  , IPAR  ,
     1          IPOT  , IRCNUT, IRCPOT, IRCSUB, ISUB  , NOBS  , NPAR  ,
     2          NPOT  , NSUB
C
      REAL*8    OMC   , POT1  , POT2  , RMS   , WGTPOT
C
CCC       IMPLICIT REAL*8(A-H,O-Z)

! Local types
! -----------
      TYPE(t_nutat)                  :: nutat          ! Nutation model
                                                       ! parameters

! Local Parameters
! ----------------
      INTEGER(i4b)                   :: maxPer
      PARAMETER (MAXPER=1000)
C
      CHARACTER*80 TITLES,TITLEP
      CHARACTER*32 FILNUT,FILSUB,FILPOT
      CHARACTER*20 PARNAM(*)
      CHARACTER*16 SUBNAM,POTNAM
C
      REAL*8       PARTIM(2,*),PARSIG(*),ANOR(*),BNOR(*),XXXAPR(*)
      REAL*8       SUBFAR(6,6),SUBPER(MAXPER),SUBCOE(4,MAXPER)
      REAL*8       POTFAR(6,6),POTPER(MAXPER),POTCOE(4,MAXPER)
C
      INTEGER*4    IFDARG(6),IFDAR1(6)
      INTEGER*4    SUBMLT(6,MAXPER)
      INTEGER*4    POTMLT(6,MAXPER)
C
C INITIALIZE NOBS,RMS, ANOR, AND BNOR
C -----------------------------------
      NOBS=0
      RMS=0.D0
C
      DO IPA1=1,NPAR
        BNOR(IPA1)=0.D0
        DO IPA2=IPA1,NPAR
          ANOR(IKF(IPA1,IPA2))=0.D0
        ENDDO
      ENDDO
C
C SET A PRIORI CONSTRAINTS
C ------------------------
CCC      DO IPAR=1,NPAR
CCC        ANOR(IKF(IPAR,IPAR))=SIGMA0**2/PARSIG(IPAR)**2
CCC      ENDDO
C
C A PRIORI PARAMETER VALUES
C -------------------------
      DO IPAR=1,NPAR
        XXXAPR(IPAR)=0.D0
      ENDDO
C
C GET REFERENCE NUTATION MODEL
      CALL GTFLNA(0,'NUTREF ',FILNUT,IRCNUT)
      IF (IRCNUT.EQ.0) THEN

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! nutat%nutcoe is in mas
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        CALL rdnutm(filnut,nutat)
        nutat%nutcoe=nutat%nutcoe/1000.D0

      ELSE
        nutat%NNUT=0
      ENDIF
C
C GET REFERENCE SUBDAILY MODEL
      CALL GTFLNA(0,'SUBREF ',FILSUB,IRCSUB)
      IF (IRCSUB.EQ.0) THEN
        CALL RDSUBM(MAXPER,FILSUB,TITLES,SUBNAM,SUBFAR,NSUB  ,
     1              SUBPER,SUBMLT,SUBCOE)
      ELSE
        NSUB=0
      ENDIF
C
C SET A PRIORI VALUES
      DO IPAR=1,NPAR
C
C NUTATION TERMS
        IF (PARNAM(IPAR)(1:2).EQ.'ES' .OR.
     1      PARNAM(IPAR)(1:2).EQ.'EC' .OR.
     2      PARNAM(IPAR)(1:2).EQ.'PS' .OR.
     3      PARNAM(IPAR)(1:2).EQ.'PC') THEN
          READ(PARNAM(IPAR)(3:17),'(5I3)') (IFDARG(II),II=1,5)
          DO INUT=1,nutat%nnut
            DO IARG=1,5
              IF (nutat%nutmlt(IARG,INUT).NE.IFDARG(IARG)) GOTO 10
            ENDDO

C TERM FOUND
          IF (PARNAM(IPAR)(1:2).EQ.'ES')
     1        XXXAPR(IPAR)=nutat%NUTCOE(7,INUT)*1000.D0
            IF (PARNAM(IPAR)(1:2).EQ.'EC')
     1        XXXAPR(IPAR)=nutat%NUTCOE(5,INUT)*1000.D0
            IF (PARNAM(IPAR)(1:2).EQ.'PS')
     1        XXXAPR(IPAR)=nutat%NUTCOE(1,INUT)*1000.D0
            IF (PARNAM(IPAR)(1:2).EQ.'PC')
     1        XXXAPR(IPAR)=nutat%NUTCOE(3,INUT)*1000.D0
            GOTO 30
10          CONTINUE
          ENDDO
        ELSEIF (PARNAM(IPAR)(1:2).EQ.'SS' .OR.
     1          PARNAM(IPAR)(1:2).EQ.'SC' .OR.
     2          PARNAM(IPAR)(1:2).EQ.'US' .OR.
     3          PARNAM(IPAR)(1:2).EQ.'UC') THEN
          READ(PARNAM(IPAR)(3:20),'(6I3)') (IFDARG(II),II=1,6)
          DO ISUB=1,NSUB
            DO IARG=1,6
              IF (SUBMLT(IARG,ISUB).NE.IFDARG(IARG)) GOTO 20
            ENDDO
C TERM FOUND
            IF (PARNAM(IPAR)(1:2).EQ.'SS')
     1        XXXAPR(IPAR)=SUBCOE(2,ISUB)*1000.D0
            IF (PARNAM(IPAR)(1:2).EQ.'SC')
     1        XXXAPR(IPAR)=SUBCOE(1,ISUB)*1000.D0
            IF (PARNAM(IPAR)(1:2).EQ.'US')
     1        XXXAPR(IPAR)=SUBCOE(4,ISUB)*1000.D0
            IF (PARNAM(IPAR)(1:2).EQ.'UC')
     1        XXXAPR(IPAR)=SUBCOE(3,ISUB)*1000.D0
            GOTO 30
20          CONTINUE
          ENDDO
C TERM NOT FOUND: A PRIORI REMAINS ZERO
        ENDIF
30      CONTINUE
      ENDDO

C
C CONSTRAINING OF SIDEBANDS FOR OCEAN TIDES
C -----------------------------------------
      CALL GTFLNA(0,'SUBPOT ',FILPOT,IRCPOT)
      IF (IRCPOT.EQ.0) THEN
        CALL RDSUBM(MAXPER,FILPOT,TITLEP,POTNAM,POTFAR,NPOT  ,
     1              POTPER,POTMLT,POTCOE)
      ELSE
        NPOT=0
      ENDIF
C
      IF (NPOT.NE.0) THEN
        WGTPOT=(1.D0/POTFAR(1,1))**2
        DO IPAR=1,NPAR
          IF (PARNAM(IPAR)(1:2).EQ.'SS' .OR.
     1        PARNAM(IPAR)(1:2).EQ.'SC' .OR.
     2        PARNAM(IPAR)(1:2).EQ.'US' .OR.
     3        PARNAM(IPAR)(1:2).EQ.'UC') THEN
C
C SEARCH FOR IDENTICAL TERM EXCEPT MULTIPLIER OF THE NODE ARGUMENT
            DO IPA1=IPAR+1,NPAR
              IF (PARNAM(IPA1)( 1:14).EQ.PARNAM(IPAR)( 1:14) .AND.
     1            PARNAM(IPA1)(18:20).EQ.PARNAM(IPAR)(18:20)) THEN
                READ(PARNAM(IPAR)(3:20),'(6I3)') (IFDARG(II),II=1,6)
                READ(PARNAM(IPA1)(3:20),'(6I3)') (IFDAR1(II),II=1,6)
C
C SEARCH FOR THE TWO TIDAL POTENTIAL VALUES
                POT1=0.D0
                DO IPOT=1,NPOT
                  DO IARG=1,6
                    IF (POTMLT(IARG,IPOT).NE.IFDARG(IARG)) GOTO 25
                  ENDDO
                  POT1=POTCOE(1,IPOT)*1.D6
                  GOTO 35
25                CONTINUE
                ENDDO
C
35              CONTINUE
                POT2=0.D0
                DO IPOT=1,NPOT
                  DO IARG=1,6
                    IF (POTMLT(IARG,IPOT).NE.IFDAR1(IARG)) GOTO 45
                  ENDDO
                  POT2=POTCOE(1,IPOT)*1.D6
                  GOTO 55
45                CONTINUE
                ENDDO
55              CONTINUE
C
                IF (POT1.NE.0.D0 .AND. POT2.NE.0.D0) THEN
                  ANOR(IKF(IPAR,IPAR))= ANOR(IKF(IPAR,IPAR))+
     1                                    WGTPOT
                  ANOR(IKF(IPAR,IPA1))= ANOR(IKF(IPAR,IPA1))-
     1                                    WGTPOT*POT1/POT2
                  ANOR(IKF(IPA1,IPA1))= ANOR(IKF(IPA1,IPA1))+
     1                                    WGTPOT*(POT1/POT2)**2
                  OMC=(-XXXAPR(IPAR)+(POT1/POT2)*XXXAPR(IPA1))
                  BNOR(IPAR)=BNOR(IPAR)+WGTPOT*OMC
                  BNOR(IPA1)=BNOR(IPA1)-WGTPOT*(POT1/POT2)*OMC
                  RMS=RMS+WGTPOT*OMC**2
CC                  ANOR(IKF(IPAR,IPAR))= ANOR(IKF(IPAR,IPAR))+
CC     1                                    WGTPOT*POT2*POT2
CC                  ANOR(IKF(IPAR,IPA1))= ANOR(IKF(IPAR,IPA1))-
CC     1                                    WGTPOT*POT1*POT2
CC                  ANOR(IKF(IPA1,IPA1))= ANOR(IKF(IPA1,IPA1))+
CC     1                                    WGTPOT*POT1*POT1
CC                  OMC=(-POT2*XXXAPR(IPAR)+POT1*XXXAPR(IPA1))*
CC     1                  WGTPOT
CC                  BNOR(IPAR)=BNOR(IPAR)+POT2*OMC
CC                  BNOR(IPA1)=BNOR(IPA1)-POT1*OMC
CC                  RMS=RMS+WGTPOT*OMC**2
                ENDIF
C
                GOTO 65
              ENDIF
            ENDDO
65          CONTINUE
          ENDIF
        ENDDO
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
