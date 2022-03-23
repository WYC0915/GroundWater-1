      MODULE s_PRIAMI
      CONTAINS

C*
      SUBROUTINE PRIAMI(IFLG,NAMB,NPN,NREF,LOCQ,X,RMS,D3,IAMB1,IAMB2,
     1                  NUMAMB,AMBSAT,AMBIEP,AMBWLF,AMBIGU,
     2                  AMBCLS,ICARR,NDIFF)
CC
CC NAME       :  PRIAMI
CC
CC PURPOSE    :  PRINT AMBIGUITIES AFTER RESOLUTION OR PARTIAL
CC               RESOLUTION
CC
CC PARAMETERS :
CC         IN :  IFLG   : USE INFO IN ARRAYS "IAMB1","IAMB2"  I*4
CC               NAMB   : NUMBER OF AMBIGUITY PARAMETERS      I*4
CC               NPN    : NUMBER OF PARAMETERS WITHOUT AMBI-  I*4
CC                        GUITIES
CC               NREF   : NUMBER OF REFERENCE AMBIGUITIES     I*4
CC               LOCQ(K,I),K=1,..,MAXLCQ,I=1,..,NP: PARAMETER I*4
CC                        CHARACTERIZATION ARRAY
CC               X(I),I=1,..: SOLUTION VECTOR                 R*8
CC               RMS    : RMS-ERROR                           R*8
CC               D3(I),I=1,..: NORMAL EQUATION MATRIX A       R*8
CC                        (UPPER TRIANGLE ONLY)
CC               IAMB1(I),I=1,..,NPAR: FLAG, WHETHER AMBIG.   I*4
CC                        RESOLVED (=0) OR NOT (=1)
CC               IAMB2(I),I=1,..,NPAR: FLAG, WHETHER AMBIG.   I*4
CC                        RESOLVABLE (=1) OR NOT (=0)
CC               NUMAMB(I),I=1,..,NFTOT: NUMBER OF AMBIGU.    I*4
CC               AMBSAT(J,I),J=1,..,NUMAMB(I),I=1,..,NFTOT:   I*4
CC                        AMBIGUITY SATELLITE NUMBERS
CC               AMBIEP(J,I),J=1,..,NUMAMB(I),I=1,..,NFTOT:   I*4
CC                        STARTING EPOCH NRS FOR AMBIGUITIES
CC               AMBWLF(L,K,I),L=1,..,NUMAMB(I), K=1,2 ,      R*8
CC                        I=1,..,NFTOT: WAVELENGTH FACTORS
CC               AMBIGU(L,K,I),L=1,..,NUMAMB(I), K=1,2,3,     R*8
CC                        I=1,..,NFTOT: AMBIGUITIES
CC               AMBCLS(L,K,I),L=1,..,NUMAMB(I), K=1,2,3,     I*4
CC                        I=1,..,NFTOT: AMBIGUITY CLUSTERS
CC               ICARR(K,IFIL), K=1..NFRFIL(I),I=1,..NFTOT    I*4
CC                        FREQUENCIES TO BE PROCESSED FOR
CC                        FILE I
CC               NDIFF(I):I=1,..,NFTOT: DIFFERENCE TYPE       I*4
CC                        NDIFF=0: ZERO DIFFERENCE
CC                        NDIFF=1: SINGLE DIFFERENCE
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER, L.MERVART
CC
CC VERSION    :  3.4
CC
CC CREATED    :  05-AUG-92
CC
CC CHANGES    :  24-AUG-92 : ??: ARBITRARY SELECTION OF REF. AMB.
CC               11-APR-94 : MR: BETTER AMBIGUITY PRINTING
CC                7-OCT-95 : TS: TEST RMS.GE.0 TO PREVENT "CRASH"
CC               04-AUG-99 : HH: MODIFICATIONS FOR GLONASS
CC               17-APR-00 : RD: APRIORI AMB. FOR ZERO DIFF SOLUTIONS
CC               01-DEC-01 : MR: SET REF. AMBIGUITY IF NREF=0
CC               23-JAN-02 : RD: REMOVE ZAMBIG
CC               28-JAN-03 : RD: NO REF.AMB IN ZD CASE
CC               29-JAN-03 : RD: TAKE CARE ON IREF.EQ.0 IN ZD CASE
CC               26-MAY-03 : RD: MAGNIFY OUTPUT FOR ZD-CASE
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1992     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE f_ikf
      USE f_lengt1
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IA    , IA1   , IA2   , IAMB  , IAMFRQ, IAMFRR, ICAR  ,
     1          ICLS  , ICLSRF, IEPO  , IEPORF, IFIL  , IFILO , IFILRF,
     2          IFLG  , IFLG1 , IFREQ , IFRQ  , IFRQRF, INPN  ,
     3          IREF  , IREFPR, ISVN  , ISVNRF, IWLF  , MXCAMB,
     4          MXCFRQ, MXCLCQ, NAMB  , NPN   , NREF
C
      REAL*8    AMBTOT, DCARRI, RMS   , RMSI  , WLGTRF
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*132 TEXT
      CHARACTER*6   MXNLCQ,MXNAMB,MXNFRQ
C
      REAL*8 X(*),D3(*),AMBIGU(MXCAMB,3,*)
C
      INTEGER*4 IAMB1(*),IAMB2(*),LOCQ(MXCLCQ,*)
      INTEGER*4 NUMAMB(*),AMBSAT(MXCAMB,*)
      INTEGER*4 AMBIEP(MXCAMB,*),AMBWLF(MXCAMB,2,*)
      INTEGER*4 AMBCLS(MXCAMB,3,*),ICARR(MXCFRQ,*),NDIFF(*)
C
      INCLUDE 'COMFREQ.inc'
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
      COMMON/MCMAMB/MXCAMB,MXNAMB
      COMMON/MCMFRQ/MXCFRQ,MXNFRQ
C
C WRITE THE FIRST LINE
C --------------------
      WRITE (LFNPRT,1000)
1000  FORMAT (/,36X,'REFERENCE',
     1        /,' AMBI  FILE SAT. EPOCH FRQ WLF CLU  AMBI CLU    ',
     2          'AMBIGUITY   RMS   TOTAL AMBIGU.    DL/L',
     3        /,' ',131('-'))
C
C LOOP OVER ALL AMBIGUITIES
C -------------------------
      IFILO=0
      DO 100 IAMB=1,NAMB+NREF
C
        INPN=NPN+IAMB
        IFIL=LOCQ(2,INPN)
        ICLS=LOCQ(3,INPN)
        IFRQ=LOCQ(4,INPN)
        ICAR=LOCQ(5,INPN)
        IREF=LOCQ(7,INPN)
C RELEVANT FREQUENCY IN ARRAY 'ambcls'
        IF (ICAR.EQ.2) THEN
          IFREQ=2
        ELSE IF (ICAR.EQ.5) THEN
          IFREQ=3
        ELSE
          IFREQ=1
        END IF
        DO 510 IA1=1,NUMAMB(IFIL)
          IF (AMBCLS(IA1,IFREQ,IFIL).EQ.ICLS) GO TO 511
510     CONTINUE
511     CONTINUE
        ISVN=AMBSAT(IA1,IFIL)
        IEPO=AMBIEP(IA1,IFIL)
        IF(IFRQ.NE.5) THEN
          IAMFRQ=IFRQ
        ELSE
          IAMFRQ=3
        ENDIF
C
C DEFINE WAVELENGTH FACTOR
        IF (IFRQ.LE.2) THEN
          IWLF=AMBWLF(IA1,IFRQ,IFIL)
        ELSEIF (IFRQ.LE.4) THEN
          IWLF=0
        ELSE
          IWLF=MAX0(AMBWLF(IA1,1,IFIL),AMBWLF(IA1,2,IFIL))
        ENDIF
C
        IF (IAMB.LE.NAMB) THEN
          IF (IREF.GT.0) THEN
            IFILRF=LOCQ(2,IREF)
            ICLSRF=LOCQ(3,IREF)
            IFRQRF=LOCQ(4,IREF)
            DO 610 IA2=1,NUMAMB(IFIL)
              IF (AMBCLS(IA2,IFREQ,IFIL).EQ.ICLSRF) GO TO 611
610         CONTINUE
611         CONTINUE
            ISVNRF=AMBSAT(IA2,IFIL)
            IEPORF=AMBIEP(IA2,IFIL)
            IF(IFRQRF.NE.5) THEN
              IAMFRR=IFRQRF
            ELSE
              IAMFRR=3
            ENDIF
            WLGTRF=WLGT(IFRQRF,ISVNRF)
          ELSE
            ICLSRF=0
            WLGTRF=0D0
          ENDIF
C
          IF (IFLG.EQ.1) THEN
            IF (IAMB1(INPN).EQ.0) THEN
              IFLG1=0
            ELSE
              IFLG1=1
            ENDIF
          ELSE
            IFLG1=1
          ENDIF
C
C NEW LINE FOR NEW FILE
          IF (IFIL.NE.IFILO) THEN
            IFILO=IFIL
            WRITE(LFNPRT,'( )')
          ENDIF
C
C TOTAL AMBIGUITIES
          IF (IFRQ.EQ.3 .OR. IFRQ.EQ.4) THEN
            AMBTOT=0.D0
          ELSE
            AMBTOT=AMBIGU(IA1,IAMFRQ,IFIL)+X(INPN)
          ENDIF
C
          IREFPR=IREF-NPN
C
C WRITE AMBIGUITY LINE
C
          IF(IFLG1.NE.0) THEN
            IF (D3(IKF(INPN,INPN)).GE.0.D0) THEN
              RMSI=RMS*DSQRT(D3(IKF(INPN,INPN)))
            ELSE
              RMSI=999.99
              WRITE(LFNERR,901)
901           FORMAT(/,' ### SR PRIAMI: NEGATIVE RMS ',/)
            ENDIF
            WRITE(TEXT,1001) IAMB,IFIL,ISVN,IEPO,IFRQ,IWLF,ICLS,
     1                       IREFPR,ICLSRF,
     2                       X(INPN),RMSI,AMBTOT
1001        FORMAT(3I5,I7,I3,I4,I5,2I5,F13.2,F7.2,F15.2)
          ELSE
            IA=IDNINT(X(INPN))
C
C DIFFERENCE OF ORIGINAL CARRIER IN CYCLES
            DCARRI=(WLGT(IFRQ,ISVN)-WLGTRF)/WLGT(IFRQ,ISVN)
            WRITE(TEXT,1002) IAMB,IFIL,ISVN,IEPO,IFRQ,IWLF,ICLS,
     1                       IREFPR,ICLSRF,
     2                       IA,AMBTOT,DCARRI
1002        FORMAT(3I5,I7,I3,I4,I5,2I5,I13,7X,F13.0,3x,F10.5)
          ENDIF
        ELSE
          AMBTOT=AMBIGU(IA1,IAMFRQ,IFIL)
          WRITE(TEXT,1003) IAMB,IFIL,ISVN,IEPO,IFRQ,IWLF,ICLS,
     1                     AMBTOT
1003      FORMAT(3I5,I7,I3,I4,I5,8X,'--- REFERENCE ---',5X,F13.0)
C
          IF (IAMB.EQ.NAMB+1) WRITE(LFNPRT,'( )')
        END IF
C
        IF (IFRQ.EQ.3 .OR. IFRQ.EQ.4) THEN
          TEXT(29:29)='-'
          TEXT(65:79)=' '
        ENDIF
C To prevent printing of reference ambiguities for ZD-case only
C        IF (NDIFF(IFIL).EQ.0) THEN
C To prevent printing for ZD-case and
C combined GPS+GLONASS reference ambiguities
        IF (NREF.EQ.0) THEN
          TEXT(35:39)='    -'
          TEXT(40:44)='    -'
        ENDIF
C
        WRITE(LFNPRT,1004) TEXT(1:LENGT1(TEXT))
1004    FORMAT(A)
C
100   CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
