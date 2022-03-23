      MODULE s_EEADDN
      CONTAINS

C*
      SUBROUTINE EEADDN(NPAR,AMAT,OMC,WEIGHT,NOBS,RMS,ANOR,BNOR,AHELP)
CC
CC NAME       :  EEADDN
CC
CC PURPOSE    :  ADD OBSERVARTION TO NORMAL EQUATION SYSTEM
CC
CC PARAMETERS :
CC        IN  :  NPAR   : MAXIMUM NUMBER OF PARAMETERS ALLOWED   I*4
CC               AMAT(I),I=1,..,NPAR: PARTIAL DERIVATIVES W.R.T. R*8
CC                        PARAMETERS
CC               OMC    : OBSERVED-COMPUTED                      R*8
CC               WEIGHT : OBSERVATION WEIGHT                     R*8
CC     IN/OUT :  NOBS   : NUMBER OF OBSERVATIONS                 I*4
CC               RMS    : SUM OF O-C**2                          R*8
CC               ANOR(I),I=1,..,NPAR*(NPAR+1)/2: NORMAL EQUATION R*8
CC                        MATRIX
CC               BNOR(I),I=1,..,NPAR: B-MATRIX                   R*8
CC               AHELP(I),I=1,..,NPAR: AUXILIARY ARRAY           R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  07-NOV-97
CC
CC CHANGES    :  10-MAR-98 : MR: CHECK FOR ZERO ELEMENTS IN AMAT
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1997     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE f_ikf
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IP    , IPA1  , IPA2  , IPAR  , NOBS  , NPAR
C
      REAL*8    OMC   , RMS   , WEIGHT
C
CCC       IMPLICIT REAL*8(A-H,O-Z)
C
      REAL*8       AMAT(*),ANOR(*),BNOR(*),AHELP(*)
C
C
C NUMBER OF OBSERVATIONS
C ----------------------
      NOBS=NOBS+1
C
C ADD OBSERVATION TO "ANOR"
C -------------------------
      DO IPAR=1,NPAR
        IF (AMAT(IPAR).NE.0.D0) THEN
          AHELP(IPAR)=AMAT(IPAR)*WEIGHT
        ELSE
          AHELP(IPAR)=0.D0
        ENDIF
      ENDDO
C
      DO IPA1=1,NPAR
        IF (AHELP(IPA1).NE.0.D0) THEN
          DO IPA2=IPA1,NPAR
            IF (AMAT(IPA2).NE.0.D0) THEN
              IP=IKF(IPA1,IPA2)
              ANOR(IP)=ANOR(IP)+AHELP(IPA1)*AMAT(IPA2)
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C
C ADD OBSERVATION TO "BNOR"
C -------------------------
      DO IPAR=1,NPAR
        IF (AHELP(IPAR).NE.0.D0) THEN
          BNOR(IPAR)=BNOR(IPAR)+AHELP(IPAR)*OMC
        ENDIF
      ENDDO
C
C SUM UP RMS ERROR
C ----------------
      RMS=RMS+WEIGHT*OMC**2
C
      RETURN
      END SUBROUTINE

      END MODULE
