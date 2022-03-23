      MODULE s_PRIAMB
      CONTAINS

C*
      SUBROUTINE PRIAMB(PRIOPT,TITLES,NFTOT,ICAMP,ICAMPN,NFREQ,
     1                  NUMAMB,AMBSAT,AMBIEP,AMBWLF,AMBIGU,AMBCLS)
CC
CC NAME       :  PRIAMB
CC
CC PURPOSE    :  PRINT AMBIGUITIES STORED IN OBSERVATION FILES
CC
CC PARAMETERS :
CC         IN :  PRIOPT : FLAG, IF AMBIGUITIES ARE PRINTED    I*4
CC                         =0 : NO PRINT
CC                         =1 : PRINT
CC               TITLES(I),I=1,2: TITLE LINES                 CH*132
CC               NFTOT  : TOTAL NUMBER OF FILES               I*4
CC               ICAMP  : INDEX OF CAMPAIGN                   I*4
CC               ICAMPN(I),I=1,..,NFTOT: CAMPAIGN NUMBER OF   I*4
CC                        FILE I
CC               NFREQ(I),I=1,..,NFTOT: NUMBER OF FREQUENCIES I*4
CC               NUMAMB(I),I=1,..,NFTOT: NUMBER OF AMBIGU.    I*4
CC               AMBSAT(J,I),J=1,..,NUMAMB(I),I=1,..,NFTOT:   I*4
CC                        AMBIGUITY SATELLITE NUMBERS
CC               AMBIEP(J,I),J=1,..,NUMAMB(I),I=1,..,NFTOT:   I*4
CC                        STARTING EPOCH NRS FOR AMBIGUITIES
CC               AMBCLS(L,K,I),L=1,..,NUMAMB(I), K=1,2,3,     I*4
CC                        I=1,..,NFTOT: AMBIGUITY CLUSTERS
CC               AMBWLF(K,J,I),K=1,..,J=1,2,I=1,..,NFTOT:     I*4
CC                        WAVELENGTH FACTORS
CC                        K: AMBIGUITY
CC                        J: FREQUENCY
CC                        I: FILE
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/11 14:36
CC
CC CHANGES    :  27-MAY-91 : DON'T PRINT TRAILING BLANKS
CC               21-DEC-93 : MR: BETTER AMBIGUITY PRINT
CC               05-MAR-03 : CU: REMOVE USE OF SKELETON FILE
CC               15-APR-03 : CU: BUG FIXED (FORMAT STATEMENTS)
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE f_lengt1
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IAMB  , ICAMP , IF    , IFIRST, IFRAMB,
     1          MXCAMB, NAMB  , NFTOT
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*132 TITLES(2),TEXT
      CHARACTER*6   MXNAMB
      INTEGER*4     ICAMPN(*),PRIOPT
      INTEGER*4     NUMAMB(*),AMBIEP(MXCAMB,*),NFREQ(*)
      INTEGER*4     AMBSAT(MXCAMB,*),AMBCLS(MXCAMB,3,*)
      INTEGER*4     AMBWLF(MXCAMB,2,*)
      REAL*8        AMBIGU(MXCAMB,3,*)
C
      COMMON/MCMAMB/MXCAMB,MXNAMB
C
C PRINT AMBIGUITIES
C -----------------
      IF(PRIOPT.EQ.1) THEN
        IFIRST=1
        DO 60 IF=1,NFTOT
          IF(ICAMPN(IF).NE.ICAMP) GOTO 60
          IF(NFREQ(IF).EQ.1) THEN
            NAMB=1
          ELSE
            NAMB=3
          ENDIF
          IFRAMB=1
          DO 40 IAMB=1,NUMAMB(IF)
C
C TITLE LINES IF IFIRST=1
            IF(IFIRST.EQ.1) THEN
              IFIRST=0
              WRITE(LFNPRT,2) TITLES(1)(1:LENGT1(TITLES(1))),
     1                        TITLES(2)(1:LENGT1(TITLES(2)))
2             FORMAT(//,A,/,A,/,' ',131('-'),//)
C
              WRITE(LFNPRT,"(
     1             ' AMBIGUITIES:'
     2          ,/,' -----------'
     3          ,/,' '
     4          ,/,' FILE  AMB  SAT  EPO  WLF     L1-AMBIG. CLU   '
     4            ,'  L2-AMBIG. CLU     L5-AMBIG. CLU'
     5          ,/,1X,131('-')
     6          ,/,1X)")
C
            ENDIF
            IF(IFRAMB.EQ.1) THEN
              WRITE(TEXT,3) IF,IAMB,AMBSAT(IAMB,IF),
     1                      AMBIEP(IAMB,IF),
     2                      (AMBWLF(IAMB,I,IF),I=1,2),
     3                      (AMBIGU(IAMB,I,IF),
     4                       AMBCLS(IAMB,I,IF),I=1,NAMB)
3             FORMAT(I5,I5,I4,I6,I3,'/',I1,3(F14.0,I4))
              IFRAMB=0
            ELSE
              WRITE(TEXT,4) IAMB,AMBSAT(IAMB,IF),
     1                      AMBIEP(IAMB,IF),
     2                      (AMBWLF(IAMB,I,IF),I=1,2),
     3                      (AMBIGU(IAMB,I,IF),
     4                       AMBCLS(IAMB,I,IF),I=1,NAMB)
4             FORMAT(5X,I5,I4,I6,I3,'/',I1,3(F14.0,I4))
            ENDIF
            IF(NFREQ(IF).EQ.1) TEXT(16:17)='  '
            WRITE(LFNPRT,1) TEXT(1:LENGT1(TEXT))
1           FORMAT(A)
40        CONTINUE
          WRITE(LFNPRT,5)
5         FORMAT(' ')
60      CONTINUE
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
