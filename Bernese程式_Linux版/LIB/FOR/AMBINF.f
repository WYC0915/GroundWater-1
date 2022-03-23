      MODULE s_AMBINF
      CONTAINS

C*
      SUBROUTINE AMBINF(ICARR,NAMB,IAMBDF,AMBCLS,NCLS,NUMCLS,IR5CLS)
CC
CC NAME       :  AMBINF
CC
CC PURPOSE    :
CC
CC PARAMETERS :
CC         IN :  ICARR  :   CARRIER (L1,L2,L3,L4,L5)          I*4
CC               NAMB   :   NUMBER OF AMBIGUITIES             I*4
CC               IAMBDF :   DEFINITION OF AMB. HANDLING       I*4
CC                          1: IGNORE ALL CLUSTER INFORMATION,
CC                             ESTIMATE NEW AMBIGUITIES
CC                          2: TAKE ONLY WIDE-LANE AMBIGUITIES
CC                             FROM FILE
CC                          3: TAKE ALL INFORMATION AVAILABLE
CC                             FROM FILE
CC                          4: TAKE INFORMATION ABOUT L1 & L2
CC                             BUT NOT ABOUT L5
CC               AMBCLS : AMBIGUITY CLUSTERS                   I*4(*,3)
CC                          AMBCLS(I,K): AMBIGUITY NUMBER I
CC                                       FREQUENCY NUMBER K
CC                            K=1: L1 AMBIGUITIES
CC                            K=2: L2 AMBIGUITIES
CC                            K=3: L5 AMBIGUITIES (WIDELANE)
CC        OUT :  NCLS :  NUMBER OF CLUSTERS                   I*4
CC               NUMCLS(ICLS),ICLS=1,..,NCLS CLUSTER NUMBERS  I*4(*)
CC               IR5CLS(ICLS),ICLS=1,..,NCLS # OF AMB. IN     I*4(*)
CC                              CORRESPONDING L5 CLUSTER
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  L.MERVART
CC
CC VERSION    :  3.5
CC
CC CREATED    :  05-MAY-93
CC
CC CHANGES    :  18-APR-94 : MR: INITIALIZE L2-AMBCLS, IF WIDELANE
CC                               INTRO.
CC               10-AUG-94 : MR: CALL EXITRC
CC               10-DEC-03 : MR: DECIDE WHETHER L1 OR L2 CLUSTER NUMBER
CC                               SHOULD BE USED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               22-FEB-11 : SS: SYNCHRONIZE L1 AND L2/L5 CLUSTER NUMBERS
CC               24-MAY-12 : RD: SOME MESSGES ONLY FOR "AIUB"
CC               24-MAY-12 : RD: USE M_BERN WITH ONLY
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1993     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,  ONLY: lfnerr
      USE s_maxtst
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IAMB  , IAMB2 , IAMBDF, ICARR , ICLS  , IFIRST, IFLAG ,
     1          IFRQ  , IRC   , MAXAMB, MXCAMB, NAMB  , NCLS  ,
     2          NAMB1 , NAMB2 , NAMB3
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MAXAMB=1000)
C
      INTEGER*4    AMBCLS(MXCAMB,3),NUMCLS(*),IR5CLS(*)
      INTEGER*4    IHELP(MAXAMB),SYNCLS(MAXAMB,2)
      CHARACTER*6  MXNAMB
C
      COMMON/MCMAMB/MXCAMB,MXNAMB
C
      DATA IFIRST/1/
C
C CHECK MAXIMUM LOCAL DIMENSIONS
C ------------------------------
      IF (IFIRST.EQ.1) THEN
        CALL MAXTST(1,'AMBINF',MXNAMB,MAXAMB,MXCAMB,IRC)
        IF(IRC.NE.0) CALL EXITRC(2)
        IFIRST=0
      ENDIF
C
      DO 50 ICLS=1,NCLS
        IR5CLS(ICLS)=0
50    CONTINUE
C
C SYNCHRONIZE L1 AND L2/L5 CLUSTER NUMBERS
C ----------------------------------------
      IF ((IAMBDF.EQ.3 .OR. IAMBDF.EQ.4) .AND.
     1    (ICARR.EQ.3 .OR. ICARR.EQ.4 .OR. ICARR.EQ.5)) THEN
C
        DO IAMB=1,NAMB
          SYNCLS(IAMB,1)=0
          SYNCLS(IAMB,2)=0
        ENDDO
        NAMB1=0
        NAMB2=0
        NAMB3=0
        DO IAMB=NAMB,1,-1
          IF (SYNCLS(IAMB,1).EQ.0) SYNCLS(IAMB,1)=IAMB
          IF (SYNCLS(IAMB,2).EQ.0) SYNCLS(IAMB,2)=IAMB
C
          DO IAMB2=IAMB-1,1,-1
            IF (AMBCLS(IAMB2,1).EQ.AMBCLS(IAMB,1) .AND.
     1          AMBCLS(IAMB2,2).EQ.AMBCLS(IAMB,2)) THEN
              SYNCLS(IAMB2,1)=SYNCLS(IAMB,1)
              SYNCLS(IAMB2,2)=SYNCLS(IAMB,2)
            ELSEIF (IAMBDF.EQ.3 .AND.
     1              AMBCLS(IAMB2,1).EQ.AMBCLS(IAMB,1) .AND.
     2              AMBCLS(IAMB2,3).EQ.AMBCLS(IAMB,3)) THEN
              SYNCLS(IAMB2,1)=SYNCLS(IAMB,1)
              NAMB3=NAMB3+1
            ENDIF
            IF (AMBCLS(IAMB2,1).EQ.AMBCLS(IAMB,1) .AND.
     1          AMBCLS(IAMB2,2).NE.AMBCLS(IAMB,2)) NAMB1=NAMB1+1
            IF (AMBCLS(IAMB2,1).NE.AMBCLS(IAMB,1) .AND.
     1          AMBCLS(IAMB2,2).EQ.AMBCLS(IAMB,2)) NAMB2=NAMB2+1
          ENDDO
        ENDDO
#ifdef GRP_AIUB
        IF (NAMB1+NAMB2+NAMB3.GT.0) WRITE(LFNERR,"(/,' ### SR AMBINF: '
     1    'SYNCHRONIZED L1/L2/L5 AMBIGUITY CLUSTER NUMBERS',
     2    /,16X,'#L1/#L2/#L5: ',3I4,/)") NAMB1,NAMB2,NAMB3
#endif
C
        DO IAMB=1,NAMB
          AMBCLS(IAMB,1)=SYNCLS(IAMB,1)
          AMBCLS(IAMB,2)=SYNCLS(IAMB,2)
        ENDDO
      ENDIF
C
C RELEVANT FREQUENCY IN ARRAY 'AMBCLS'
C ------------------------------------
      IF (ICARR.EQ.2) THEN
        IFRQ=2
      ELSE IF (ICARR.EQ.5) THEN
        IFRQ=3
      ELSE
        IFRQ=1
      END IF
C
      IF ( IAMBDF.EQ.1                    .OR.
     1    (IAMBDF.EQ.2  .AND. ICARR.NE.5)) THEN
        NCLS=NAMB
        DO 100 ICLS=1,NCLS
          NUMCLS(ICLS)=ICLS
          AMBCLS(ICLS,IFRQ)=ICLS
          IF ((ICARR.EQ.3 .OR. ICARR.EQ.4) .AND. IFRQ.EQ.1)
     1      AMBCLS(ICLS,2)=ICLS
100     CONTINUE
C
      ELSE IF (IAMBDF.NE.4                    .OR.
     1         (ICARR.NE.3 .AND. ICARR.NE.4)) THEN
        NCLS=0
        DO 200 IAMB=1,NAMB
          DO 210 ICLS=1,NCLS
            IF (NUMCLS(ICLS).EQ.AMBCLS(IAMB,IFRQ)) GO TO 200
210       CONTINUE
          NCLS=NCLS+1
          NUMCLS(NCLS)=AMBCLS(IAMB,IFRQ)
200     CONTINUE
C
      ELSE
        DO 300 ICLS=1,NAMB
          NUMCLS(ICLS)=0
          IHELP(ICLS) =0
300     CONTINUE
        NCLS=0
C
C SET CLUSTER NUMBER TO AMBIGUITY NUMBER IF NOT BOTH, L1 AND L2
C ARE RESOLVED
        DO IAMB=1,NAMB
          IF (AMBCLS(IAMB,1).NE.AMBCLS(IAMB,2)) THEN
            AMBCLS(IAMB,1)=IAMB
            AMBCLS(IAMB,2)=IAMB
          ENDIF
        ENDDO
C
        DO 400 IAMB=1,NAMB
          IFLAG=0
          DO 420 ICLS=1,NCLS
            IF (NUMCLS(ICLS) .EQ. AMBCLS(IAMB,1)  .AND.
     1          IHELP(ICLS)  .EQ. AMBCLS(IAMB,2)  .AND.
     2          NUMCLS(ICLS) .EQ. IHELP(ICLS))    THEN
              IFLAG=1
              GO TO 421
            END IF
420       CONTINUE
421       CONTINUE
          IF (IFLAG.EQ.0) THEN
            NCLS=NCLS+1
            NUMCLS(NCLS)=AMBCLS(IAMB,1)
            IHELP(NCLS)=AMBCLS(IAMB,2)
            ICLS=NCLS
          ELSE
            AMBCLS(IAMB,1)=NUMCLS(ICLS)
            AMBCLS(IAMB,2)=IHELP(ICLS)
          END IF
400     CONTINUE
      END IF
C
      IF (IAMBDF.EQ.2 .OR. IAMBDF.EQ.3) THEN
        DO 600 ICLS=1,NCLS
          IR5CLS(ICLS)=1
          DO 610 IAMB=1,NAMB
            DO 620 IAMB2=IAMB+1,NAMB
              IF ((NUMCLS(ICLS)  .EQ. AMBCLS(IAMB,IFRQ)   .OR.
     1            NUMCLS(ICLS)   .EQ. AMBCLS(IAMB2,IFRQ)) .AND.
     2            AMBCLS(IAMB,3) .EQ. AMBCLS(IAMB2,3))
     3          IR5CLS(ICLS)=IR5CLS(ICLS)+1
620         CONTINUE
610       CONTINUE
600     CONTINUE
      END IF
C
      RETURN
      END SUBROUTINE

      END MODULE
