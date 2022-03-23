      MODULE s_PRISAT
      CONTAINS

C*
      SUBROUTINE PRISAT(NFTOT,ICAMPN,ICAMP,NSATEL,SATNUM,INDEX)
CC
CC NAME       :  PRISAT
CC
CC PURPOSE    :  PRINT SATELLITES IN OBSERVATION FILES
CC
CC PARAMETERS :
CC         IN :  NFTOT  : TOTAL NUMBER OF FILES               I*4
CC               ICAMPN(I),I=1,..,NFTOT: CAMPAIGN NUMBER OF   I*4
CC                        FILE I
CC               ICAMP  : CAMPAIGN NUMBER TO BE PRINTED       I*4
CC               NSATEL(I),I=1,..,NFTOT: NUMBER OF SATELLITES I*4
CC                        IN FILE I
CC               SATNUM(K,I),K=1,..,NSATEL(I),I=1,..,NFTOT:   I*4
CC                        SATELLITE NUMBERS IN FILE I
CC      LOCAL :  INDEX(K),K=1,..,MAXSAT: INDEX FOR SATELLITE  I*4
CC                        ORDERING
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/18 14:49
CC
CC CHANGES    :  27-MAY-91 : ??:DON'T PRINT TRAILING BLANKS
CC               21-DEC-93 : MR: ORDER OF SATELLITES
CC               23-JUN-98 : MR: ALLOW SEVERAL SATELLITE LINES
CC               05-MAR-03 : CU: REMOVE USE OF SKELETON FILE
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE s_iordup
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , ICAMP , IF    , IREC  , MXCSAT, NFTOT , NSAPRT,
     1          NSAREC, NSAT
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*6   MXNSAT
      INTEGER*4     NSATEL(*),SATNUM(MXCSAT,*),ICAMPN(*),INDEX(*)
      COMMON/MCMSAT/MXCSAT,MXNSAT
C
C PRINT TITLE LINES
C -----------------
      WRITE(LFNPRT,"(
     1     ' '
     2  ,/,' '
     3  ,/,' SATELLITES:'
     4  ,/,' ----------'
     5  ,/,' '
     6  ,/,' FILE  #SAT  SATELLITES'
     7  ,/,1X,131('-')
     8  ,/,1X)")
C
C PRINT SATELLITE NUMBERS
C -----------------------
      DO 20 IF=1,NFTOT
        IF(ICAMPN(IF).NE.ICAMP) GOTO 20
        CALL IORDUP(SATNUM(1,IF),NSATEL(IF),INDEX)
C
        NSAREC=30
        NSAT=0
        DO IREC=1,100
          IF (NSATEL(IF).GT.NSAT) THEN
            IF (NSATEL(IF)-NSAT.GT.NSAREC) THEN
              NSAPRT=NSAREC
            ELSE
              NSAPRT=NSATEL(IF)-NSAT
            ENDIF
            IF (IREC.EQ.1) THEN
              WRITE(LFNPRT,2) IF,NSATEL(IF),
     1                        (SATNUM(INDEX(I),IF),I=1,NSAPRT)
2             FORMAT(2I5,2X,30I4)
            ELSE
              WRITE(LFNPRT,3) (SATNUM(INDEX(I),IF),
     1                        I=NSAT+1,NSAT+NSAPRT)
3             FORMAT(12X,30I4)
            ENDIF
            NSAT=NSAT+NSAREC
            IF (NSAT.GT.NSATEL(IF)) GOTO 15
          ENDIF
        ENDDO
C
15      CONTINUE
20    CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
