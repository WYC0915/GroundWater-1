      MODULE s_AMBSET
      CONTAINS

C*
      SUBROUTINE AMBSET(AMBDEF,ICARR ,IAMB  ,NUMAMB,AMBCLS,OBS   ,
     1                  PRDIF ,AMBSAT,AMBWLF,NFRFIL,NFREQ ,IFRQFL,
     2                  IAMB1 ,AMB0  ,AMBIGU)
CC
CC NAME       :  AMBSET
CC
CC PURPOSE    :  DEFINE A PRIORI VALUES FOR AMBIGUITIES
CC
CC PARAMETERS :
CC         IN :  AMBDEF : TYPE OF AMBIGUITY DEFINITION        I*4
CC                        =1: NEW DEFINITION
CC                        =2: USE L5 IF AVAILABLE
CC                        =3: USE L1 AND L2 IF AVAILABLE
CC                        =4: USE L1 AND L2 BUT NOT L5
CC               ICARR  : CARRIER                             I*4
CC               IAMB   : AMBIGUITY NUMBER                    I*4
CC               NUMAMB : TOTAL NUMBER OF AMBIGUITIES         I*4
CC               AMBCLS(I,L),I=1,..,NUMAMB, L=1,2,3:          I*4
CC                        AMBIGUITY CLUSTERS
CC               OBS    : OBSERVATION                         R*8
CC               PRDIF  : PSEUDORANGE DIFFERENCE              R*8
CC               AMBSAT(I),I=1,..,NUMAMB: SATELLITE NUMBER    I*4
CC                        FOR AMBIGUITY NUMBER I
CC               AMBWLF(I,J),I=1,..,NUMAMB,J=1,2: WAVELENGTH  I*4
CC                        FACTORS FOR BOTH FREQUENCIES
CC               NFRFIL : NUMBER OF PROCESSED LINEAR COMB.    I*4
CC               NFREQ  : NUMBER OF FREQUENCIES IN FILE       I*4
CC               IFRQFL : FREQUENCY INDEX (1 OR 2)            I*4
CC        OUT :  IAMB1  : AMBIGUITY INITIALIZATION FLAG      CH*1
CC               AMB0(I,J): A PRIORI AMB.,I=IAMB,J=1,2        R*8
CC               AMBIGU(I,K),I=1,..,NUMAMB,K=1,2,3: AMBI-     R*8
CC                        GUITIES
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/09/30 09:57
CC
CC CHANGES    :  28-SEP-95 : JJ: DECLARE MXNFRQ AS C*6 INSTEAD OF I*4
CC               14-AUG-97 : SS: "MEATYP" REMOVED
CC               24-JUN-98 : HH: MODIFICATIONS FOR GLONASS
CC               04-AUG-99 : MR: ADDITIONAL MODIFICATIONS FOR GLONASS
CC               15-AUG-99 : JJ: COMMENT OUT UNUSED VAR IFAK
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IA    , IAMB  , ICARR , IFREQ , IFRQ  , IFRQFL, IR1CLS,
     1          IR5CLS, IRFCLS, MXCAMB, MXCFRQ, MXCSAT, NEWAMB, NFREQ ,
     2          NFRFIL, NUMAMB
C
      REAL*8    OBS   , OLDAM1, OLDAM2, OLDAM3, PRDIF , WAVE
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*6  MXNSAT,MXNAMB,MXNFRQ
      CHARACTER*1  IAMB1(MXCAMB,MXCFRQ)
C
      REAL*8       AMBIGU(MXCAMB,3),AMB0(MXCAMB,MXCFRQ)
C
      INTEGER*4    AMBDEF,AMBWLF(MXCAMB,2),IWLF(2),AMBCLS(MXCAMB,3)
      INTEGER*4    AMBSAT(MXCAMB)
      INTEGER*4    SVNSAT
C
      INCLUDE 'COMFREQ.inc'
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMAMB/MXCAMB,MXNAMB
      COMMON/MCMFRQ/MXCFRQ,MXNFRQ
C
C     DATA IFAK/1/
C
C WAVELENGTH FACTORS
      DO 30 IFREQ=1,NFREQ
        IWLF(IFREQ)=AMBWLF(IAMB,IFREQ)
30    CONTINUE
C
C RELEVANT FREQUENCY IN ARRAY 'AMBCLS'
      IF (ICARR.EQ.2) THEN
        IFRQ=2
      ELSE IF (ICARR.EQ.5) THEN
        IFRQ=3
      ELSE
        IFRQ=1
      END IF
C
C AMBIGUITY ALREADY RESOLVED (I.E. MORE THAN ONE AMBIGUITY IN CLUSTER)
C --------------------------------------------------------------------
      IRFCLS=1
      DO 90 IA=1,NUMAMB
        IF (IA             .NE. IAMB                .AND.
     1      AMBCLS(IA,IFRQ) .EQ. AMBCLS(IAMB,IFRQ)) THEN
          IRFCLS=2
          GOTO 91
        ENDIF
90    CONTINUE
91    CONTINUE
C
C IS IT POSSIBLE TO INTRODUCE WIDE-LANE?
C --------------------------------------
      IR5CLS=1
      IF (AMBDEF.EQ.2 .OR. AMBDEF.EQ.3) THEN
        DO 100 IA=1,NUMAMB
          IF (IA             .NE. IAMB          .AND.
     1        AMBCLS(IAMB,3) .EQ. AMBCLS(IA,3)) THEN
            IR5CLS = 2
            GO TO 101
          END IF
100     CONTINUE
101     CONTINUE
      END IF
C
C IS IT POSSIBLE TO INTRODUCE L1&L2 ?
C -----------------------------------
      IR1CLS=1
      IF (AMBDEF.EQ.3 .OR. AMBDEF.EQ.4) THEN
        DO 150 IA=1,NUMAMB
          IF (IA             .NE. IAMB          .AND.
     1        AMBCLS(IAMB,1) .EQ. AMBCLS(IA,1)) THEN
            IF (AMBDEF.EQ.3) THEN
               IR1CLS = 2
               GO TO 151
            ELSE IF (AMBDEF         .EQ. 4             .AND.
     1               AMBCLS(IAMB,2) .EQ. AMBCLS(IA,2)) THEN
              IR1CLS = 2
              GO TO 151
            END IF
          END IF
150     CONTINUE
151     CONTINUE
      END IF
C
C HANDLING THE ACTUAL AMBIGUITY
C -----------------------------
      SVNSAT=AMBSAT(IAMB)
C
C L1 FREQUENCY
      IF (ICARR.EQ.1) THEN
        NEWAMB=DNINT((OBS-PRDIF)/(WLGT(ICARR,SVNSAT)/IWLF(ICARR)))
C
C L2 FREQUENCY
      ELSE IF (ICARR.EQ.2) THEN
        IF (NFREQ.EQ.2                     .AND.
     1      (AMBDEF.EQ.2 .OR. AMBDEF.EQ.3) .AND.
     2      IR5CLS.NE.1)                   THEN
          NEWAMB=DNINT((OBS-PRDIF+FACLIN(ICARR,2,SVNSAT)*
     1                          WLGT(2,SVNSAT)/IWLF(1)/IWLF(2)*
     2                          AMBIGU(IAMB,3))/
     3                          (WLGT(ICARR,SVNSAT)/IWLF(1)))
        ELSE
          NEWAMB=DNINT((OBS-PRDIF)/(WLGT(ICARR,SVNSAT)/IWLF(ICARR)))
        END IF
C
C L3,L4 FREQUENCIES
      ELSE IF(ICARR.LE.4) THEN
        IF ((AMBDEF.EQ.2 .OR. AMBDEF.EQ.3) .AND.
     1      IR5CLS.NE.1)                   THEN
          NEWAMB=DNINT((OBS-PRDIF+FACLIN(ICARR,2,SVNSAT)*
     1                         WLGT(2,SVNSAT)/IWLF(1)/IWLF(2)*
     2                         AMBIGU(IAMB,3))/
     3                         (WLGT(ICARR,SVNSAT)/IWLF(1)))
        END IF
C
C L5 FREQUENCY
      ELSE
        NEWAMB=DNINT((OBS-PRDIF)/
     1                       (WLGT(ICARR,SVNSAT)/IWLF(1)/IWLF(2)))
      END IF
C
C LOOP OVER ALL AMBIGUITIES:
C --------------------------
      OLDAM1=AMBIGU(IAMB,1)
      OLDAM2=AMBIGU(IAMB,2)
      OLDAM3=AMBIGU(IAMB,3)
C
      DO 200 IA=1,NUMAMB
        IF (AMBCLS(IA,IFRQ).NE.AMBCLS(IAMB,IFRQ)) GO TO 200
C
        SVNSAT=AMBSAT(IA)
C
C L1 FREQUENCY
        IF (ICARR.EQ.1) THEN
          IF (IRFCLS.EQ.1) THEN
            AMBIGU(IA,1)=AMBIGU(IA,1)+
     1                    NEWAMB-OLDAM1
          ENDIF
          AMB0(IA,1)=AMBIGU(IA,1)*WLGT(ICARR,SVNSAT)/IWLF(ICARR)
           IAMB1(IA,1)='D'
          IF (NFRFIL.EQ.2.                   .AND.
     1        (AMBDEF.EQ.2 .OR. AMBDEF.EQ.3) .AND.
     2        IR5CLS.NE.1)                    THEN
            AMB0(IA,2)=AMBIGU(IA,1)*WLGT(2,SVNSAT)/IWLF(1) -
     1               FACLIN(2,2,SVNSAT)*WLGT(2,SVNSAT)/IWLF(1)/IWLF(2)*
     2               AMBIGU(IA,3)
            IAMB1(IA,2)='D'
          END IF
C
C L2 FREQUENCY
        ELSE IF (ICARR.EQ.2) THEN
          IF  ((AMBDEF.EQ.2 .OR. AMBDEF.EQ.3) .AND.
     1         IR5CLS.NE.1)                   THEN
            IF (IRFCLS.EQ.1) THEN
              AMBIGU(IA,1)=AMBIGU(IA,1)+
     1                      NEWAMB-OLDAM1
            ENDIF
            AMB0(IA,1)=AMBIGU(IA,1)*WLGT(ICARR,SVNSAT)/IWLF(1) -
     1           FACLIN(ICARR,2,SVNSAT)*WLGT(2,SVNSAT)/IWLF(1)/IWLF(2)*
     2              AMBIGU(IA,3)
            IAMB1(IA,1)='D'
          ELSE
            IF (IRFCLS.EQ.1) THEN
              AMBIGU(IA,2)=AMBIGU(IA,2)+
     1                      NEWAMB-OLDAM2
            ENDIF
            AMB0(IA,IFRQFL)=AMBIGU(IA,2)*WLGT(ICARR,SVNSAT)/IWLF(ICARR)
            IAMB1(IA,IFRQFL)='D'
          ENDIF
C
C L3,L4 FREQUENCIES
        ELSE IF (ICARR.LE.4) THEN
          IF ((AMBDEF.EQ.3 .OR. AMBDEF.EQ.4) .AND. IR1CLS.GT.1)  THEN
            AMB0(IA,1) =
     1      FACLIN(ICARR,1,SVNSAT)*AMBIGU(IA,1)*WLGT(1,SVNSAT)/IWLF(1)+
     2      FACLIN(ICARR,2,SVNSAT)*AMBIGU(IA,2)*WLGT(2,SVNSAT)/IWLF(2)
            IAMB1(IA,1)='D'
            IAMB1(IA,2)='D'
          ELSE IF ((AMBDEF.EQ.2 .OR. AMBDEF.EQ.3)  .AND.
     1         IR5CLS.NE.1)                        THEN
            IF (IRFCLS.EQ.1) THEN
              AMBIGU(IA,1)=AMBIGU(IA,1)+
     1                      NEWAMB-OLDAM1
            ENDIF
            AMB0(IA,1)=AMBIGU(IA,1)*WLGT(ICARR,SVNSAT)/IWLF(1) -
     1           FACLIN(ICARR,2,SVNSAT)*WLGT(2,SVNSAT)/IWLF(1)/IWLF(2)*
     2              AMBIGU(IA,3)
            IAMB1(IA,1)='D'
            IAMB1(IA,2)='D'
          ELSE
            WAVE=FACLIN(ICARR,1,SVNSAT)*WLGT(1,SVNSAT)/IWLF(1) +
     1           FACLIN(ICARR,2,SVNSAT)*WLGT(2,SVNSAT)/IWLF(2)
            AMB0(IA,1)=DNINT((OBS-PRDIF)/WAVE)*WAVE
            IAMB1(IA,1)='D'
          END IF
C
C L5 FREQUENCY
        ELSE
          IF (IRFCLS.EQ.1) THEN
            AMBIGU(IA,3)=AMBIGU(IA,3)+NEWAMB-OLDAM3
          ENDIF
          AMB0(IA,1)=AMBIGU(IA,3)*WLGT(ICARR,SVNSAT)/IWLF(1)/IWLF(2)
          IAMB1(IA,1)='D'
        ENDIF
200   CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
