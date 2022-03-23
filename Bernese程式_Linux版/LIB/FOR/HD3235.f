      MODULE s_HD3235
      CONTAINS

C*
      SUBROUTINE HD3235(MAXAMO,MAXSAT,MEATYP,NDIFF,NFREQ,NSATEL,NUMSAT,
     1                  NUMAMO,IWLFAC,AMBIEO,AMBIGO,AMBFLO,
     2                  NUMAMB,AMBSAT,AMBIEP,AMBWLF,AMBIGU,AMBCLS)
CC
CC NAME       :  RDHEAD
CC
CC PURPOSE    :  CONVERT THE INFORMATION ABOUT AMBIGUITIES FROM
CC               THE HEADER FILE VERSION 3.2 TO VERSION 3.5
CC
CC PARAMETERS :
CC        IN  :  MAXAMO : MAXIMUM NUMBER OF AMP. PER SATLELL.  I*4
CC               MAXSAT : MAXIMUM NUMBER OF SATELLITES         I*4
CC               MEATYP : MEASUREMENT TYPE                     I*4
CC                          1: PHASE OBSERVATIONS
CC                          2: CODE OBSERVATIONS
CC               NDIFF  : NUMBER OF DIFFERENCES                I*4
CC                          0: ZERO-DIFFERENCES
CC                          1: SINGLE-DIFFERENCES
CC               NFREQ  : NUMBER OF FREQUENCIES  (1 OR 2)      I*4
CC               NSATEL : NUMBER OF SATELLITES                 I*4
CC               NUMSAT : SATELLITE NUMBERS                    I*4(*)
CC               NUMAMO : NUMBER OF AMBIGUITIES PER SATELLITE  I*4(*)
CC               IWLFAC : WAVELENGTH FACTORS                   I*4(*,2)
CC                          IWLFAC(I,J): WL.FACTOR OF SATELL-
CC                                       ITE I, FREQUENCY J
CC               AMBIEO : AMBIGUITY STARTING EPOCH NUMBER      I*4(*,*)
CC                          AMBIEP(I,J): AMBIGUITY NUMBER I,
CC                                       SATELLITE NUMBER J
CC               AMBIGO : AMBIGUITIES                        R*8(*,*,3)
CC                          AMBIGU(I,J,K): AMBIGUITY NUMBER I
CC                                         SATELLITE NUMBER J
CC                                         FREQUENCY NUMBER K
CC                            K=1: L1 AMBIGUITIES
CC                            K=2: L2 AMBIGUITIES
CC                            K=3: L5 AMBIGUITIES (WIDELANE)
CC               AMBFLO : AMBIGUITY RESOLUTION FLAG          CH*1(*,*,3)
CC                          U: UNRESOLVED AMBIGUITY
CC                          R: RESOLVED AMBIGUITY
CC                          AMBFLG(I,J,K): AMBIGUITY NUMBER I
CC                                         SATELLITE NUMBER J
CC                                         FREQUENCY NUMBER K
CC       OUT  :  NUMAMB : TOTAL NUMBER OF AMBIGUITIES (ALL     I*4
CC                        SATELLITES)
CC               AMBSAT : SATELLITE NUMBER                     I*4(*)
CC                          AMBSAT(I): AMBIGUITY NUMBER I
CC               AMBIEP : AMBIGUITY STARTING EPOCH NUMBER      I*4(*)
CC                          AMBIEP(I): AMBIGUITY NUMBER I
CC               AMBWLF : WAVELENGTH FACTORS                   I*4(*,2)
CC                          AMBWLF(I,J): WL.FACTOR OF AMBIGUITY
CC                                       I, FREQUENCY J
CC                          1: CYCLE AMBIGUITIES
CC                          2: HALF-CYCLE AMBIGUITIES
CC               AMBIGU : AMBIGUITIES                          R*8(*,3)
CC                          AMBIGU(I,K): AMBIGUITY NUMBER I
CC                                       FREQUENCY NUMBER K
CC                            K=1: L1 AMBIGUITIES
CC                            K=2: L2 AMBIGUITIES
CC                            K=3: L5 AMBIGUITIES (WIDELANE)
CC               AMBCLS : AMBIGUITY CLUSTERS                   I*4(*,3)
CC                          AMBIGU(I,K): AMBIGUITY NUMBER I
CC                                       FREQUENCY NUMBER K
CC                            K=1: L1 AMBIGUITIES
CC                            K=2: L2 AMBIGUITIES
CC                            K=3: L5 AMBIGUITIES (WIDELANE)
CC
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  L. MERVART
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  04-MAY-93
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1993     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IAMB  , IAMO  , ICLS  , IFREQ , IFRQ  , IRSCLS, ISAT  ,
     1          MAXAMO, MAXSAT, MEATYP, MXCAMB, MXCSAT, NDIFF , NFREQ ,
     2          NFRQ  , NSATEL, NUMAMB
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C EXPLICIT DECLARATIONS
C ---------------------
      CHARACTER*6  MXNSAT,MXNAMB
      CHARACTER*1  AMBFLO(MAXAMO,MAXSAT,3)
      REAL*8       AMBIGO(MAXAMO,MAXSAT,3),AMBIGU(MXCAMB,3)
      INTEGER*4    NUMSAT(MXCSAT),NUMAMO(*),IWLFAC(MAXSAT,2)
      INTEGER*4    AMBIEO(MAXAMO,MAXSAT)
      INTEGER*4    AMBSAT(*),AMBIEP(*)
      INTEGER*4    AMBWLF(MXCAMB,2),AMBCLS(MXCAMB,3)
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMAMB/MXCAMB,MXNAMB
C
      IF (NFREQ.EQ.2) THEN
        NFRQ=3
      ELSE
        NFRQ=1
      END IF
C
C DEFINE NUMBER OF AMBIGUITIES, CLUSTERS AND VALUES OF AMBIGUITIES
C ----------------------------------------------------------------
      IF (MEATYP.EQ.1 .AND. NDIFF.EQ.0) THEN
        DO 50 ISAT=1,NSATEL
          NUMAMO(ISAT)=1
          AMBIEO(1,ISAT)=1
          DO 55 IFRQ=1,3
            AMBIGO(1,ISAT,IFRQ)=0.D0
            AMBFLO(1,ISAT,IFRQ)='U'
55        CONTINUE
50      CONTINUE
      END IF
C
      DO 100 IFRQ=1,NFRQ
        IAMB=0
        IRSCLS=0
        ICLS=0
        DO 110 ISAT=1,NSATEL
          DO 120 IAMO= 1,NUMAMO(ISAT)
            IAMB=IAMB+1
            IF (AMBFLO(IAMO,ISAT,IFRQ).EQ.'R') THEN
              IF (IRSCLS.EQ.0) THEN
                ICLS=ICLS+1
                AMBCLS(IAMB,IFRQ)=ICLS
                IRSCLS=ICLS
              ELSE
                AMBCLS(IAMB,IFRQ)=IRSCLS
              END IF
            ELSE
              ICLS=ICLS+1
              AMBCLS(IAMB,IFRQ)=ICLS
            END IF
            AMBIGU(IAMB,IFRQ)=AMBIGO(IAMO,ISAT,IFRQ)
120       CONTINUE
110     CONTINUE
100   CONTINUE
      NUMAMB=IAMB
C
C CHECK MAXIMUM DIMENSIONS FOR AMBIGUITIES
C --------------------------------------------------------------
      IF (NUMAMB .GT. MXCAMB) THEN
        WRITE(LFNERR,901) NUMAMB,MXCAMB
901     FORMAT(/,' *** SR HD3235: TOO MANY AMBIGUITIES ',
     1                           'TO BE CHARACTERIZED',/,
     2                       16X,'NUMBER OF AMBIGUITIES >=',I5,/,
     3                       16X,'MAXIMUM NUMBER        :',I5,/)
        CALL EXITRC(2)
      END IF
C
C DEFINE SATELLITES, EPOCHS AND WAVE LENGTH FACTORS OF AMBIGUITIES
C ----------------------------------------------------------------
      IAMB=0
      DO 200  ISAT=1,NSATEL
        DO 210 IAMO=1,NUMAMO(ISAT)
          IAMB=IAMB+1
          AMBSAT(IAMB)=NUMSAT(ISAT)
          AMBIEP(IAMB)=AMBIEO(IAMO,ISAT)
          DO 220 IFREQ=1,NFREQ
              AMBWLF(IAMB,IFREQ)=IWLFAC(ISAT,IFREQ)
220       CONTINUE
210     CONTINUE
200   CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
