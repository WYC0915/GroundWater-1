      MODULE s_SDAMBF
      CONTAINS

C*
      SUBROUTINE SDAMBF(NFREQ,NSAT,NRSAT,OBSFLG,NSATEL,NUMSAT,IAMFLG)
CC
CC NAME       :  SDAMBF
CC
CC PURPOSE    :  KEEP TRACK OF CYCLE SLIP FLAGS:
CC               IAMFLG(ISATEL,IFRQ)=0 : NO CYCLE SLIP HAPPENED SINCE
CC                                       LAST SAVED EPOCH
CC               IAMFLG(ISATEL,IFRQ)=1 : CYCLE SLIP SINCE LAST SAVED
CC                                       EPOCH
CC
CC PARAMETERS :
CC         IN :  NFREQ  : NUMBER OF FREQUENCIES IN FILE        I*4
CC               NSAT   : NUMBER OF SATELLITES AT EPOCH        I*4
CC               NRSAT(I),I=1,..,NSAT: SATELLITE NUMBERS FOR   I*4
CC                        CURRENT EPOCH
CC               OBSFLG(I,K),I=1,..,NSAT,K=1,NFREQ: OBSERVA-  CH*1
CC                        TION FOR SATELLITE I, FREQUENCY K
CC               NSATEL : TOTAL NUMBER OF SATELLITES IN FILE   I*4
CC               NUMSAT(I),I=1,..,NSATEL: SATELLITE NUMBERS    I*4
CC                        IN FILE
CC               IAMFLG(I,K): CYCLE SLIP FLAG FOR SATELLITE I, I*4
CC                        FREQUENCY K:
CC                        =0 : NO CYCLE SLIP SINCE LAST EPOCH
CC                             SAVED IN SINGLE DIFF. FILE
CC                        =1 : CYCLE SLIP OCCURRED SINCE LAST
CC                             EPOCH SAVED IN SINGLE DIFF. FILE
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 94)
CC
CC CREATED    :  92/02/20 23:59
CC
CC CHANGES    :  28-SEP-95 : JJ: DECLARE TSTFLG AS L*4 INSTEAD OF L*1
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1992     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE f_tstflg
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IFRQ  , ISAT  , ISATEL, MXCSAT, NFREQ , NSAT  , NSATEL
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C EXPLICIT DECLARATIONS
C ---------------------
      CHARACTER*1 OBSFLG(MXCSAT,2)
      CHARACTER*6 MXNSAT
      INTEGER*4   NRSAT(*),NUMSAT(*),IAMFLG(MXCSAT,2)
C
C COMMON WITH MAXIMAL DIMENSIONS
C ------------------------------
      COMMON/MCMSAT/MXCSAT,MXNSAT
C
C UPDATE CYCLE SLIP FLAG
C ----------------------
      DO 100 ISAT=1,NSAT
        DO 90 IFRQ=1,NFREQ
          IF (TSTFLG(OBSFLG(ISAT,IFRQ),1)) THEN
C
C FIND FILE SPECIFIC SATELLITE INDEX
            DO 10 ISATEL=1,NSATEL
              IF (NUMSAT(ISATEL).EQ.NRSAT(ISAT)) GOTO 20
10          CONTINUE
            GOTO 90
20          IAMFLG(ISATEL,IFRQ)=1
          ENDIF
90      CONTINUE
100   CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
