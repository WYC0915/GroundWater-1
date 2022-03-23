      MODULE s_RELVEL
      CONTAINS

C*
      SUBROUTINE RELVEL(RFAK,RELWGT,SIGAPR,STVEL,STANUM,ISIVEL,NVEL,
     1                  NPAR,LOCQ,ANOR,COVAPR)
CC
CC NAME       :  RELVEL
CC
CC PURPOSE    :  INTRODUCE RELATIVE WEIGHTS FOR SITE VELOCITIES
CC               (IF ISIVEL(I).EQ.1)
CC
CC PARAMETERS :
CC         IN    RFAK   : 1 / -1: ADD / SUBSTRACT RESTRICTION R*8
CC               RELWGT : RELATIVE WEIGHT TO BE ADDED/
CC                        SUBSTRACTED
CC               SIGAPR : APRIORI SIGMA                       R*8
CC               STVEL(I): I=1,NVEL STATION NUMBERS OF
CC                         VELOCITIES                         I*4(*)
CC               STANUM(I),I=1,2,..,NSTAT  : STATION NUMBERS  I*4
CC               ISIVEL(I): I=1,NVEL SITE VELOCITY TO BE
CC                         ESTIMATE (1) OR DIFFERENT SITE
CC                         NAMES WILL MEAN DIFFERENT VELOC.
CC                         (0)                                I*4(*)
CC               NVEL : NUMBER OF VELOCITY STATIONS           I*4
CC               NPAR  : NUMBER OF PARAMETERS (WITHOUT PRE-   I*4
CC                        ELIMINATED PARAMETERS)
CC                        NORMAL EQUATION MATRIX (UPPER
CC                        TRIANGLE ONLY, COLUMNWISE LINEAR.)
CC               LOCQ(K,I),K=1,..,MAXLCQ, I=1,2,...,NPAR:     I*4
CC                        CHARACTERISTICS FOR EACH PARAMETER
CC     IN/OUT :  ANOR(I),I=1,2,..,NPAR*(NPAR+1)/2: INVERSE OF R*8
CC                        NORMAL EQUATION MATRIX (UPPER
CC                        TRIANGLE ONLY, COLUMNWISE LINEAR.)
CC               COVAPR(I,K),I=1,2,K=1,3*NSTAT*(3*NSTAT-1)/2  R*8
CC                        I=1:COORD., I=2:VELOCITIES,
CC                        APRIORI COVARIANCE INFORMATION
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  E. BROCKMANN
CC
CC VERSION    :  3.4  (JAN 94)
CC
CC CREATED    :  14-FEB-96
CC
CC CHANGES    :  14-FEB-96 : EB: START
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1993     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE f_ikf
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 ICO   , IHLP1 , IHLP2 , IK    , IP    , IPIP  ,
     1          IPIP1 , IPIP2 , IVEL  , JVEL  , MXCLCQ, NPAR  , NVEL
C
      REAL*8    RELWGT, RFAK  , SIGAPR, WDIF
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      REAL*8        ANOR(*),COVAPR(2,*)
      CHARACTER*6   MXNLCQ
      INTEGER*4     LOCQ(MXCLCQ,*)
      INTEGER*4     STVEL(*),ISIVEL(*),STANUM(*)
C
C
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
C
      WDIF=RFAK*(SIGAPR/RELWGT)**2
C
      DO 100 IVEL=1,NVEL
        IF (ISIVEL(IVEL).EQ.0) GOTO 100
C SKIP, IF ALREADY BEFORE THE WEIGHTS IS INTRODUCED
        DO JVEL=IVEL-1,1,-1
          IF (ISIVEL(JVEL).EQ.1.AND.STVEL(IVEL).EQ.STVEL(JVEL))GOTO 100
        ENDDO
C
        DO 200 IP=1,NPAR
          IF (LOCQ(1,IP).NE.20)GOTO 200
          IF (STANUM(LOCQ(2,IP)).EQ.STVEL(IVEL).AND.LOCQ(3,IP).EQ.1)
     1    THEN
C SEARCH FOR ADDITIONAL VELOCITIES WITH THAT CHARACTERISTIC
            DO 300 IK=IP+3,NPAR
              IF (LOCQ(1,IK).NE.20)GOTO 300
              IF (STANUM(LOCQ(2,IK)).EQ.STANUM(LOCQ(2,IP)).AND.
     1              LOCQ(3,IK).EQ.LOCQ(3,IP)) THEN
                DO ICO=1,3
                  IPIP =IKF(IP+ICO-1,IP+ICO-1)
                  IPIP1=IKF(IK+ICO-1,IK+ICO-1)
                  IPIP2=IKF(IK+ICO-1,IP+ICO-1)
                  ANOR(IPIP) =ANOR(IPIP) +WDIF
                  ANOR(IPIP1)=ANOR(IPIP1)+WDIF
                  ANOR(IPIP2)=ANOR(IPIP2)-WDIF
C
                  IHLP1=(LOCQ(2,IP)-1)*3+ICO
                  IHLP2=(LOCQ(2,IK)-1)*3+ICO
                  IPIP =IKF(IHLP1,IHLP1)
                  IPIP1=IKF(IHLP2,IHLP2)
                  IPIP2=IKF(IHLP2,IHLP1)
                  COVAPR(2,IPIP) =COVAPR(2,IPIP) +1.D0/WDIF
                  COVAPR(2,IPIP1)=COVAPR(2,IPIP1)+1.D0/WDIF
                  COVAPR(2,IPIP2)=COVAPR(2,IPIP2)+1.D0/WDIF
                  WRITE(LFNERR,*)'RELVEL',IVEL,STVEL(IVEL),IP,IK,ICO,
     1              LOCQ(2,IP),LOCQ(2,IK),IHLP1,IHLP2,1.D0/WDIF,
     2               COVAPR(2,IPIP2)
                ENDDO
              ENDIF
300         CONTINUE
            WRITE(LFNERR,*)'END OF IVEL',IVEL
            GOTO 100
          ENDIF
200     CONTINUE
100   CONTINUE
C
      END SUBROUTINE

      END MODULE
