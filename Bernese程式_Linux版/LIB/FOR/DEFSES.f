      MODULE s_DEFSES
      CONTAINS

C*
      SUBROUTINE DEFSES(NFTOT ,CSESS ,MELWUB,IZEROD,CORSTR,NSATEL,
     1                  SATNUM,NFRFIL,NALLSAT,ALLSAT,ISASYS,
     2                  NSESS,SESSID,NMXFLS,NMXSAS,NMXFRS)
CC
CC NAME       :  DEFSES
CC
CC PURPOSE    :  DEFINE SESSIONS FOR BERNESE GPS SOFTWARE VERSION 3.0
CC
CC PARAMETERS :
CC         IN :  NFTOT  : NUMBER OF FILES                     I*4
CC               CSESS(K,I),K=1,2,I=1,2,..,NFTOT              CH*4
CC                    (1,I): SESSION IDENTIFIER
CC               MELWUB : MELBOURNE-WUEBBENA LC               I*4
CC                        =0: NO
CC                        =1: YES
CC                        =2: DTEC LC
CC               IZEROD : FLAG FOR ZD-PROCESSING RESIDUALS    I*4
CC                        (YES=1)
CC               CORSTR : CORRELATION STRATEGY TO BE USED     I*4
CC                    =1: USE ONLY CORRELATIONS WITHIN BASELINE
CC                    =2: MODEL CORRELATIONS FOR OBSERVATIONS
CC                        OF THE SAME FREQUENCY
CC                    =3: MODEL CORRELATIONS CORRECTLY
CC               NSATEL(I),I=1,..,NFTOT: NUMBER OF SATELLITES I*4
CC               SATNUM(K,I),K=1,..,NSATEL(I),I=1,..,NFTOT:   I*4
CC                        SATELLITE NUMBERS IN FILE I
CC               NFRFIL(I),I=1,..,NFTOT: NUMBER OF FREQ.      I*4
CC                        TO BE PROCESSED
CC               NALLSAT: TOTAL NUMBER OF SATELLITES          I*4
CC               ISASYS : SATELLITE SYSTEM TO BE CONSIDERED   I*4
CC                        =0: ALL
CC                        =1: GPS
CC                        =2: GLONASS
CC        OUT :  NSESS  : NUMBER OF SESSIONS                  I*4
CC               SESSID(I),I=1,2,..,NSESS: SESSION IDENTIF.   CH*4
CC               NMXFLS : COMPUTED VALUE FOR MAXFLS           I*4
CC               NMXSAS : COMPUTED VALUE FOR MAXSAS           I*4
CC               NMXFRS : COMPUTED VALUE FOR MAXFRS           I*4
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/10/15 09:49
CC
CC CHANGES    :  12-AUG-94 : MR: FORMAT 4: SESSION AS CHARACTER*4
CC               30-AUG-95 : MR: ADD TEST FOR ODD NUMBER OF FILES
CC               30-MAR-98 : TS: SIMULTANEOUS CODE AND PHASE ZD PROCESSING
CC               29-APR-98 : SS: DTEC LC
CC               11-OCT-04 : RD: GET VALUES FOR MAXFLS AND MAXSAS
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               23-May-10 : RD: CONSIDER ONLY OBSERVED SATELLITES
CC               28-MAR-12 : RD: USE SVN2CHR AS MODULE NOW
CC               04-MAY-12 : RD: USE DMOD FROM MODULE
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: LFNERR
      USE l_basfun, ONLY: dmod
      USE s_exitrc
      USE s_svn2chr
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IFIL   , IFLSES , ISASES , ISASYS , ISAT   , ISATEL ,
     1          ISES   , ISESS  , IZEROD , JSAT   , K      , MELWUB ,
     2          NALLSAT, NFTOT  , NMXFLS , NMXFRS , NMXSAS , NSESS  ,
     3          INUM
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      INTEGER*4    CORSTR,SATLST(NALLSAT),NSATEL(*),SATNUM(:,:)
      INTEGER*4    NFRFIL(*),ALLSAT(:)
      CHARACTER*4  CSESS(2,*),SESSID(*)
      CHARACTER*1  SVNCHR
C
C
C CREATE SESSIONS
C ---------------
      IF(CORSTR.EQ.1)THEN
        NSESS=NFTOT
        DO 30 K=1,NSESS
          WRITE(SESSID(K),'(I4.4)') K
30      CONTINUE
      ELSE
        NSESS=0
        DO 20 IFIL=1,NFTOT
          DO 10 ISES=1,NSESS
            IF(CSESS(1,IFIL).EQ.SESSID(ISES)) GOTO 20
10        CONTINUE
          NSESS=NSESS+1
          SESSID(NSESS)=CSESS(1,IFIL)
20      CONTINUE
      ENDIF
C
C MELBOURNE-WUEBBENA
C ------------------
      IF (CORSTR.EQ.1) THEN
        IF (MELWUB.EQ.1 .OR. IZEROD.EQ.1) THEN
         IF (DMOD(NSESS/2.D0,1.D0).NE.0.D0) THEN
           WRITE(LFNERR,901) NSESS
901        FORMAT(/,' *** SR DEFSES: ODD NUMBER OF FILES FOR ',
     1              'MELBOURNE-WUEBBENA STRATEGY NOT ALLOWED',
     2            /,16X,'NUMBER OF FILES:',I5,/)
           CALL EXITRC(2)
         ENDIF
         NSESS=NSESS/2
       ENDIF
      ENDIF
C
C COMPUTE THE MAXIMUM NUMBER OF FILES PER SESSION
C -----------------------------------------------
      IF (CORSTR.EQ.1) THEN
        NMXFLS=1
        IF (MELWUB.EQ.1 .OR. IZEROD.EQ.1) NMXFLS=2
      ELSE IF (CORSTR.GT.0) THEN
        NMXFLS=0
        DO ISESS=1,NSESS
          IFLSES=0
          DO IFIL=1,NFTOT
            IF (CSESS(1,IFIL).NE.SESSID(ISESS)) CYCLE
            IFLSES=IFLSES+1
          ENDDO
          IF (IFLSES.GT.NMXFLS) NMXFLS=IFLSES
        ENDDO
      ENDIF
C
C COMPUTE THE MAXIMUM NUMBER OF SATELLITES PER SESSION
C ----------------------------------------------------
      NMXSAS=0
      IF (CORSTR.EQ.1) THEN
        DO IFIL=1,NFTOT
          ISASES=0
          DO ISATEL=1,NSATEL(IFIL)
            CALL SVN2CHR(SATNUM(ISATEL,IFIL),INUM,SVNCHR)
            IF (SVNCHR.EQ.'G' .AND.
     1          (ISASYS.EQ.2.OR.ISASYS.EQ.3.OR.ISASYS.EQ.6)) CYCLE
            IF (SVNCHR.EQ.'R' .AND.
     1          (ISASYS.EQ.1.OR.ISASYS.EQ.3.OR.ISASYS.EQ.5)) CYCLE
            IF (SVNCHR.EQ.'E' .AND.
     1          (ISASYS.EQ.1.OR.ISASYS.EQ.2.OR.ISASYS.EQ.4)) CYCLE
            IF (SVNCHR.EQ.'S' .AND. ISASYS.NE.0) CYCLE
            JSAT=0
            DO ISAT=1,NALLSAT
              IF (SATNUM(ISATEL,IFIL).EQ.ALLSAT(ISAT))THEN
                JSAT=ISATEL
                EXIT
              ENDIF
            ENDDO
            IF (JSAT.EQ.0) CYCLE
            ISASES=ISASES+1
          ENDDO
          IF (ISASES.GT.NMXSAS) NMXSAS=ISASES
        ENDDO
      ELSE IF (CORSTR.GT.0) THEN
        NMXSAS=0
        DO ISESS=1,NSESS
          ISASES=0
          DO IFIL=1,NFTOT
            IF (CSESS(1,IFIL).NE.SESSID(ISESS)) CYCLE
            DO ISATEL=1,NSATEL(IFIL)
              CALL SVN2CHR(SATNUM(ISATEL,IFIL),INUM,SVNCHR)
              IF (SVNCHR.EQ.'G' .AND.
     1            (ISASYS.EQ.2.OR.ISASYS.EQ.3.OR.ISASYS.EQ.6)) CYCLE
              IF (SVNCHR.EQ.'R' .AND.
     1            (ISASYS.EQ.1.OR.ISASYS.EQ.3.OR.ISASYS.EQ.5)) CYCLE
              IF (SVNCHR.EQ.'E' .AND.
     1            (ISASYS.EQ.1.OR.ISASYS.EQ.2.OR.ISASYS.EQ.4)) CYCLE
              IF (SVNCHR.EQ.'S' .AND. ISASYS.NE.0) CYCLE
C
              JSAT=0
              DO ISAT=1,NALLSAT
                IF (SATNUM(ISATEL,IFIL).EQ.ALLSAT(ISAT))THEN
                  JSAT=ISATEL
                  EXIT
                ENDIF
              ENDDO
              IF (JSAT.EQ.0) CYCLE
C
              JSAT=0
              DO ISAT=1,ISASES
                IF (SATNUM(ISATEL,IFIL).NE.SATLST(ISAT)) CYCLE
                JSAT=ISAT
                EXIT
              ENDDO
              IF (JSAT.EQ.0) THEN
                ISASES=ISASES+1
                SATLST(ISASES)=SATNUM(ISATEL,IFIL)
              ENDIF
            ENDDO
          ENDDO
          IF (ISASES.GT.NMXSAS) NMXSAS=ISASES
        ENDDO
      ENDIF
C
C COMPUTE THE MAXIMUM NUMBER OF FREQUENCIES PER SESSION
C -----------------------------------------------------
      NMXFRS = 1
      IF (CORSTR.GT.0) THEN
        DO IFIL=1,NFTOT
          IF (NFRFIL(IFIL).GT.NMXFRS) NMXFRS=NFRFIL(IFIL)
        ENDDO
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
