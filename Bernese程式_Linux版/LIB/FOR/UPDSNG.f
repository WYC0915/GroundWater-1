      MODULE s_UPDSNG
      CONTAINS

C*
      SUBROUTINE UPDSNG(IEPOCH,NSAT,NFRAUX,NONZER,NDIFF,CLOCK,
     1                  ABSACT,ABSLST,TIMLST,FRSTOB)
CC
CC NAME       :  UPDSNG
CC
CC PURPOSE    :  UPDATE ARRAY ABSLST AFTER HAVING PROCESSED
CC               ONE EPOCH
CC
CC PARAMETERS :
CC         IN :  IEPOCH : EPOCH NUMBER                        I*4
CC               NSAT   : TOTAL NUMBER OF SATELLITES          I*4
CC               NFRAUX : NUMBER OF FREQUENCIES               I*4
CC               NONZER(I),I=1,2,..,NFRAUX: NUMBER OF SATEL-  I*4
CC                        WITH GOOD OBSERVATIONS
CC               NDIFF  : 0: ZERO/ 1: SIGNLE DIFFERENCE FILES I*4
CC               CLOCK(I),I=1,2,..,5 CLOCK INCREMENTS         R*8
CC     IN/OUT :  ABSACT(K,I),K=1,2,3, I=1,2,..,NSAT: SINGLE   R*8
CC                        DIFF "RESIDUALS" OF CURRENT EPOCH
CC               ABSLST(K,I),K=1,2,3, I=1,2,..,NSAT: SINGLE   R*8
CC                        DIFF "RESIDUALS" OF PRECEEDING EPOCH
CC               TIMLST(K,I),K=1,2,3, I=1,2,..,NSAT:          I*4
CC                        LAST OBSERVATION EPOCH
CC               FRSTOB(K,I),K=1,2,3, I=1,2,..,NSAT: CH*1
CC                        ='Y': FIRST OBS OF SATELLITE
CC                        ELSE: NOT FIRST OBS OF SATELLITE
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  88/05/02 15:10
CC
CC CHANGES    :  06-DEC-02 : RD: COMPARE NONZER WITH NDIFF (INSTEAD OF "1")
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1988     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IEPOCH, IFRQ  , ISAT  , NDIFF , NFRAUX, NSAT
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      CHARACTER*1 FRSTOB(3,*)
      INTEGER*4   NONZER(*),TIMLST(3,*)
      REAL*8      ABSLST(3,*),ABSACT(3,*),CLOCK(*)
C
      DO 100 IFRQ=1,NFRAUX
        IF(NONZER(IFRQ).GT.NDIFF) THEN
          DO 90 ISAT=1,NSAT
            IF(ABSACT(IFRQ,ISAT).NE.1.D20) THEN
              IF(ABSLST(IFRQ,ISAT).EQ.1.D20) THEN
                FRSTOB(IFRQ,ISAT)='Y'
              ELSE
                FRSTOB(IFRQ,ISAT)='N'
              END IF
              TIMLST(IFRQ,ISAT)=IEPOCH
              ABSLST(IFRQ,ISAT)=ABSACT(IFRQ,ISAT)
            ELSE IF(ABSLST(IFRQ,ISAT).NE.1.D20)THEN
              ABSLST(IFRQ,ISAT)=ABSLST(IFRQ,ISAT)+CLOCK(IFRQ)
            END IF
90        CONTINUE
        ENDIF
100   CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
