      MODULE s_DSPDEL
      CONTAINS

C*
      SUBROUTINE DSPDEL(ITITLE,ISUMRY,INEW,IDETYP,IDEFRQ,IDESVN,
     1                  NFREQ,NSATEL,NUMSAT,
     2                  NDEL,LSTDEL,IRC)
CC
CC NAME       :  DSPDEL
CC
CC PURPOSE    :  DISPLAY MARKED OBSERVATION AREAS
CC               ACCORDING TO THE OPTIONS "IDETYP,IDEFRQ,IDESVN"
CC
CC PARAMETERS :
CC         IN :  ITITLE : DISPLAY GENERAL TITLE               I*4
CC                        =0: NO GENERAL TITLE
CC                        =1: GENERAL TITLE DISPLAYED
CC               ISUMRY : DISPLAY SUMMARY OF MARKED AREAS     I*4
CC                        =0: NO SUMMARY
CC                        =1: SHORT SUMMARY DISPLAYED
CC                        =2: LONG  SUMMARY DISPLAYED
CC               INEW   : DISPLAY NEW OR CHANGED MARKED AREAS
CC                        =0: DISPLAY ALL
CC                        =1: DISPLAY NEW OR CHANGED AREAS ONLY
CC               IDETYP : TYPE OF MARKED AREA                 I*4
CC                        =0: ALL MARKED AREAS
CC                        =1: SINGLE FREQ. REJECTION
CC                        =2: DUAL   FREQ. REJECTION
CC                        =3: UNPAIRED L1/L2 OBSERVATIONS
CC                        =4: USER
CC                        =5: SMALL ELEVATION
CC                        =6: SMALL PIECES
CC                        =7: BAD OBSERVED-COMPUTED
CC               IDEFRQ : FREQUENCY OF MARKED AREA            I*4
CC                        =0: ALL (BOTH) FREQUENCIES
CC                        =1: AREAS WITH L1 OBSERV. MARKED
CC                        =2: AREAS WITH L2 OBSERV. MARKED
CC               IDESVN : SATELLITE(S) WITH MARKED AREAS      I*4
CC                        =0: ALL SATELLITES
CC                        =I: SATELLITE NUMBER I
CC               NFREQ  : NUMBER OF FREQUENCIES IN FILE       I*4
CC               NSATEL : TOTAL NUMBER OF SATELLITES          I*4
CC               NUMSAT(I),I=1,..,NSATEL: SATELLITE NUMBERS   I*4
CC               NDEL   : NUMBER OF AREAS OF MARKED OBS.      I*4
CC               LSTDEL(K,I),K=1,..,5, I=1,2,..,NDEL          I*4
CC                        DEFINITION OF MARK REQUEST NUMBER I
CC                        (1,I): SV-NUMBER
CC                        (2,I): FIRST EPOCH OF MARKED AREA I
CC                        (3,I): LAST  EPOCH OF MARKED AREA I
CC                        (4,I): FREQUENCY (1=L1, 2=L2)
CC                        (5,I): MARKED BY
CC                               =1: SINGLE FREQ. REJECTION
CC                               =2: DUAL   FREQ. REJECTION
CC                               =3: UNPAIRED L1/L2 OBSERVATIONS
CC                               =4: USER
CC                               =5: SMALL ELEVATION
CC                               =6: SMALL PIECES
CC                               =7: BAD OBSERVED-COMPUTED
CC                               =8: GPS SATELLITE CLOCK IS MISSING
CC                               ALL AREAS MARKED OR CHANGED IN THE
CC                               LATEST RUN HAVE A NEGATIVE SIGN
CC               IRC    : RETURN CODE                         I*4
CC                        =0 : AT LEAST ONE MARKED AREA
CC                             DISPLAYED
CC                        =1 : NO MARKED AREA DISPLAYED WITH
CC                             ENTERED SPECIFICATIONS
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER, L.MERVART
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  89/08/10 16:09
CC
CC CHANGES    :  05-JUN-92 : ??: CHANGES FOR THE NEW MAUPRP VERSION
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               10-AUG-94 : MR: CALL EXITRC
CC               24-APR-95 : MR: ADD MARKING TYPE "O-C"
CC               23-SEP-97 : DI: USE MAXSAT.inc
CC               10-JAN-02 : DS: DISPLAY MISSING OF GPS SAT CLOCK "8"
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               29-FEB-08 : RD: INDICATE CYCLE SLIP PROBLEMS
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1989     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE m_maxdim, ONLY: MAXSAT
      USE s_maxtst
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IDEFRQ, IDEL  , IDESVN, IDETYP, IEP1  , IEP2  , IFIRST,
     1          IFRQ  , INEW  , IOLTYP, IRC   , ISATEL, ISUMRY, ISVN  ,
     2          ITITL1, ITITLE, ITYP  , ITYPA , MAXDLT, MXCSAT, NDEDSP,
     3          NDEL  , NFREQ , NSATEL
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MAXDLT=9)
C
      CHARACTER*6  MXNSAT
      CHARACTER*3  DELTXT(2*MAXDLT)
      CHARACTER*1  NEWTXT
C
      INTEGER*4    NUMSAT(*),LSTDEL(5,*),NDELET(MAXDLT,2,MAXSAT)
      INTEGER*4    NDELSA(2,MAXSAT),NDELTT(MAXDLT,2),NDELFR(2)
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
C
      DATA IFIRST/1/
      DATA DELTXT/'SNG','DUA','UNP','USR','ELV','GAR','O-C','CLK','PRP',
     1            '*R*','*R*','*R*','*R*','*R*','*R*','*R*','*R*','*R*'/
C
C INITIALIZATION OF DEFAULTS
C --------------------------
      IF(IFIRST.EQ.1) THEN
        IFIRST=0
        CALL MAXTST(1,'DSPDEL',MXNSAT,MAXSAT,MXCSAT,IRC)
        IF(IRC.NE.0) CALL EXITRC(2)
      ENDIF
C
C DISPLAY SUMMARY OF MARKED AREAS
C -------------------------------
      IF(ISUMRY.GE.1) THEN
C
C WRITE GENERAL TITLE
        IF(ITITLE.EQ.1) THEN
          WRITE(LFNPRT,1)
1         FORMAT(//,1X,72('-'),
     1            /,' SUMMARY OF MARKED OBSERVATION AREAS',
     2            /,1X,72('-'),/)
        ENDIF
C
C INITIALIZATION
        DO 30 IFRQ=1,NFREQ
          NDELFR(IFRQ)=0
          DO 20 ITYP=1,MAXDLT
            NDELTT(ITYP,IFRQ)=0
            DO 10 ISATEL=1,NSATEL
              NDELET(ITYP,IFRQ,ISATEL)=0
              NDELSA(IFRQ,ISATEL)=0
10          CONTINUE
20        CONTINUE
30      CONTINUE
C
C SUMMARY
        NDEDSP=0
        DO 40 IDEL=1,NDEL
          ISVN=LSTDEL(1,IDEL)
          DO 35 ISATEL=1,NSATEL
            IF(NUMSAT(ISATEL).EQ.ISVN) GOTO 38
35        CONTINUE
38        IFRQ=LSTDEL(4,IDEL)
          ITYP=IABS(LSTDEL(5,IDEL))
          IF(ITYP.GT.MAXDLT) GOTO 40
          IF(INEW.EQ.1.AND.LSTDEL(5,IDEL).GT.0) GOTO 40
          NDEDSP=NDEDSP+1
          NDELET(ITYP,IFRQ,ISATEL)=NDELET(ITYP,IFRQ,ISATEL)+1
          NDELSA(IFRQ,ISATEL)=NDELSA(IFRQ,ISATEL)+1
          NDELTT(ITYP,IFRQ)=NDELTT(ITYP,IFRQ)+1
          NDELFR(IFRQ)=NDELFR(IFRQ)+1
40      CONTINUE
C
C DISPLAY SUMMARY
        DO 50 IFRQ=1,NFREQ
          WRITE(LFNPRT,41) IFRQ,NDELFR(IFRQ)
41        FORMAT(' NUMBER OF MARKED AREAS IN L',I1,':',I5)
50      CONTINUE
        IF(ISUMRY.GE.2) THEN
          IF(NDEDSP.EQ.0) THEN
            IRC=1
            GOTO 999
          ENDIF
C
          IF(NFREQ.EQ.1) THEN
            WRITE(LFNPRT,51)
51          FORMAT(/,1X,'SATELLITE    SNG.FRQ. UNPAIR.    USER',
     1                  ' ELEVAT.  SM. PIECES  OBS-CMP  TOTAL',
     2             /,1X,79('-'),/)
            DO 60 ISATEL=1,NSATEL
              WRITE(LFNPRT,59) NUMSAT(ISATEL),
     1                         NDELET(1,1,ISATEL),NDELET(3,1,ISATEL),
     2                         NDELET(4,1,ISATEL),NDELET(5,1,ISATEL),
     3                         NDELET(6,1,ISATEL),NDELET(7,1,ISATEL),
     4                         NDELSA(1,ISATEL)
59            FORMAT(I6,5X,6I8,I15)
60          CONTINUE
            WRITE(LFNPRT,61) NDELTT(1,1),NDELTT(3,1),NDELTT(4,1),
     1                       NDELTT(5,1),NDELTT(6,1),NDELTT(7,1),
     2                       NDELFR(1)
61          FORMAT(/,1X,79('-'),
     1             /,1X,'  TOTAL   ',6I8,I15,
     2             /,1X,79('-'))
          ELSE
            WRITE(LFNPRT,62)
62          FORMAT(/,6X,'SING.FRQ.  DUAL FRQ.  UNPAIRED   USER',
     1             7X,'ELEVATION  SM.PIECES  OBS-CMP  TOTAL',
     2             /,1X ,'S. ',8(2X,'#L1  #L2 '),
     3             /,1X,79('-'),/)
            DO 70 ISATEL=1,NSATEL
              WRITE(LFNPRT,69) NUMSAT(ISATEL),
     1                         ((NDELET(ITYP,IFRQ,ISATEL),IFRQ=1,2),
     2                                               ITYP=1,MAXDLT),
     3                         (NDELSA(IFRQ,ISATEL),IFRQ=1,2)
69            FORMAT(I3,8(I6,I5))
70          CONTINUE
            WRITE(LFNPRT,71) ((NDELTT(ITYP,IFRQ),IFRQ=1,2),ITYP=1,
     1                         MAXDLT),
     2                       (NDELFR(IFRQ),IFRQ=1,2)
71          FORMAT(/,1X,79('-'),
     1             /,1X,'T.',8(I6,I5),
     2             /,1X,79('-'))
          ENDIF
        ENDIF
        IRC=0
        GOTO 999
      ENDIF
C
C DISPLAY LIST OF MARKED AREAS
C ----------------------------
C
C WRITE GENERAL TITLE
      IF(ITITLE.EQ.1) THEN
        WRITE(LFNPRT,72)
72      FORMAT(//,1X,72('-'),
     1          /,' MARKED OBSERVATION AREAS',
     2          /,1X,72('-'))
      ENDIF
C
      IF(NDEL.EQ.0.AND.INEW.EQ.0) THEN
        WRITE(LFNPRT,73)
73      FORMAT(/,' NO MARKED OBSERVATION AREAS FOUND',/)
        IRC=1
        GOTO 999
      ENDIF
C
      ITITL1=1
      IOLTYP=-1
C
C LOOP OVER ALL MARKED AREAS
      DO 100 IDEL=1,NDEL
        ISVN=LSTDEL(1,IDEL)
        IEP1=LSTDEL(2,IDEL)
        IEP2=LSTDEL(3,IDEL)
        IFRQ=LSTDEL(4,IDEL)
        ITYPA=IABS(LSTDEL(5,IDEL))
        ITYP=ITYPA
        IF(ITYP.GT.MAXDLT) ITYP=ITYP-MAXDLT
C
        IF(INEW.EQ.1.AND.LSTDEL(5,IDEL).GT.0) GOTO 100
C
        IF(IDETYP.NE.0.AND.ITYP.NE.IDETYP) GOTO 100
        IF(IDEFRQ.NE.0.AND.IFRQ.NE.IDEFRQ) GOTO 100
        IF(IDESVN.NE.0.AND.ISVN.NE.IDESVN) GOTO 100
C
C TITLE
        IF(ITITL1.EQ.1) THEN
          ITITL1=0
          WRITE(LFNPRT,74)
74        FORMAT(/,1X,'NUMB   TYP N      EPOCHS      SAT   FRQ',
     1           '   #EPOCHS'/,1X,72('-'))
        ENDIF
C
C NEW OR CHANGED AREA INDICATOR
        IF(LSTDEL(5,IDEL).GT.0) THEN
          NEWTXT=' '
        ELSE
          NEWTXT='*'
        ENDIF
C
C BLANK LINE, IF NEW TYPE
        IF(ITYP.NE.IOLTYP) WRITE(LFNPRT,'( )')
C
C DISPLAY MARKED AREA
        WRITE(LFNPRT,76) IDEL,DELTXT(ITYPA),NEWTXT,IEP1,IEP2,ISVN,IFRQ,
     1                   IEP2-IEP1+1
76      FORMAT(I5,3X,A3,1X,A1,I7,' -',I6,I6,I5,I9)
C
        IOLTYP=ITYP
100   CONTINUE
C
C NO MATCHES ?
      IF(ITITL1.EQ.1.AND.INEW.EQ.0) THEN
        WRITE(LFNPRT,103)
103     FORMAT(/,' NO MARKED AREAS FOUND WITH ENTERED SPECIFICATIONS',/)
        IRC=1
        GOTO 999
      ENDIF
      WRITE(LFNPRT,104)
104   FORMAT(/,1X,72('-'))
      IRC=0
C
C RETURN
999   RETURN
      END SUBROUTINE

      END MODULE
