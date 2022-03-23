      MODULE s_HEDIN2
      CONTAINS

C*
      SUBROUTINE HEDIN2(NFIL,FILHED,MEAFIL,NFRFIL,NEPFIL,NSAFIL,CSESS,
     1                  IDTFIL,TIMFIL,STAFIL,SVNFIL,
     2                  TITLE,NSTAT,STNAME,STFIL,NUMOBS,NUMMRK,
     3                  NUMAMB,AMBSAT,AMBIEP,AMBWLF,AMBIGU,AMBCLS)
CC
CC NAME       :  HEDIN2
CC
CC PURPOSE    :  READ HEADER INFORMATION FOR PREPROCESSING PROGRAM
CC
CC PARAMETERS :
CC         IN :  NFIL   : NUMBER OF FILES                     I*4
CC               FILHED(I),I=1,..,NFIL: FILE HEADER NAMES     CH*(*)
CC        OUT :  MEAFIL(I),I=1,..: MEASUREMENT TYPE           I*4
CC               NFRFIL(I),I=1,.. : NUMBER OF FREQUENCIES IN  I*4
CC                        FILE I
CC               NEPFIL(I),I=1,..: NUMBER OF EPOCHS IN FILE   I*4
CC               NSAFIL(I),I=1,2,..,NFIL: NUMBER OF SATEL-    I*4
CC                        LITES PER FILE
CC               CSESS(K,I),K=1,2,I=1,..: SESSION IDENTIFIER CH*4
CC                        IDENT. OF FILES FROM SAME BASELINE/SESSION
CC               IDTFIL(I),I=... : OBSERVATION INTERVAL (SEC) I*4
CC               TIMFIL(I),I=... : REFERENCE TIME OF FILE     R*8
CC               STAFIL(K,I),K=1,2,I=1,.. : STATION NAMES IN  CH*16
CC                        FILE I
CC               SVNFIL(K,I),K=1,..,MXCSAT,I=...: SATELLITE   I*4
CC                        NUMBERS
CC               TITLE  : TITLE OF ONE OF THE FILES           CH*53
CC               NSTAT  : NUMBER OF STATIONS IN OBS. FILES    I*4
CC               STNAME(I),I=1,2,..,NSTAT: STATION NAMES      CH*16
CC               STFIL(K,I),K=1,2, I=...: INTERNAL NUMBERS    I*4
CC                     OF STATIONS OBSERVING IN FILE I
CC     IN/OUT :
CC               NUMOBS(I,J) LOCAL ARRAY WITH NUMBER OF OBSER-I*4
CC                        VATIONS
CC               NUMMRK(I,J) LOCAL ARRAY WITH NUMBER OF MARK. I*4
CC                        OBSERVATIONS
CC               NUMAMB : TOTAL NUMBER OF AMBIGUITIES (ALL     I*4
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
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.5  (AUG 93)
CC
CC CREATED    :  89/09/29 18:00
CC
CC CHANGES    :  09-AUG-93 : ??: VERSION 3.5, NEW FORMAT
CC               10-AUG-94 : MR: CALL EXITRC
CC               12-AUG-94 : MR: FORMAT 4: SESSION AS CHARACTER*4
CC               08-SEP-03 : HU: ANTNAM, RECNAM CHR16 -> CHR20,
CC                               FILENAMES CHR(*)
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS
CC               26-JAN-11 : LP: CHANGED RDHEAD CALL
CC               14-FEB-11 : RD: REMOVE MAXSTA-COMMON (NOT NEEDED)
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1989     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE s_rdhead
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C -------------------------------
      INTEGER*4 MXCAMB, MXCSAT, NSTAT, IF, NFIL, IST, NDIFF, ISTAT,
     1          NEPFLG, IRMARK, NUMAMB, IFRMAT
C
      CHARACTER*53 TITLE,TITLE1
      CHARACTER*(*) FILHED(*)
      CHARACTER*16 STNAME(:),STAFIL(2,*)
      CHARACTER*20 OPRNAM(2),RECTYP(2),ANTTYP(2)
      CHARACTER*16 CAMPGN
      CHARACTER*9  CRDATE(2)
      CHARACTER*6  MXNSAT,MXNAMB
      CHARACTER*5  CRTIME(2)
      CHARACTER*4  CSESS(2,*)
C
      REAL*8       TIMFIL(*),POSECC(3,2),AMBIGU(MXCAMB,3)
C
      INTEGER*4    NSAFIL(*)
      INTEGER*4    MEAFIL(*),IDTFIL(*)
      INTEGER*4    IRUNIT(2),ICLOCK(2),SVNFIL(MXCSAT,*)
      INTEGER*4    AMBIEP(*),AMBSAT(*),AMBWLF(MXCAMB,2)
      INTEGER*4    AMBCLS(MXCAMB,3)
      INTEGER*4    STFIL(2,*),IANTEN(2)
      INTEGER*4    NUMOBS(MXCSAT,2),NUMMRK(MXCSAT,2)
      INTEGER*4    NFRFIL(*),NEPFIL(*),READGEOS
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMAMB/MXCAMB,MXNAMB
C
C LOOP OVER ALL FILES
C -------------------
      NSTAT=0
      READGEOS=0
      DO 50 IF=1,NFIL
C
C READ HEADER FILE
C ----------------
        CALL RDHEAD(FILHED(IF),MEAFIL(IF),NDIFF,NFRFIL(IF),
     1       NEPFIL(IF),NSAFIL(IF),
     2       CSESS(1,IF),IDTFIL(IF),TIMFIL(IF),
     3       CAMPGN,TITLE1,CRDATE,CRTIME,IRMARK,NEPFLG,IFRMAT,
     4       STAFIL(1,IF),RECTYP,ANTTYP,IRUNIT,IANTEN,
     5       OPRNAM,POSECC,ICLOCK,SVNFIL(1,IF),NUMOBS,NUMMRK,
     6       NUMAMB,AMBSAT,AMBIEP,AMBWLF,AMBIGU,AMBCLS,READGEOS)
C
C TITLE
        IF(IF.EQ.1) TITLE=TITLE1
C
C NEW STATION ?
        DO 45 IST=1,NDIFF+1
          DO 20 ISTAT=1,NSTAT
            IF(STNAME(ISTAT).EQ.STAFIL(IST,IF)) THEN
              STFIL(IST,IF)=ISTAT
              GOTO 45
            ENDIF
20        CONTINUE
          NSTAT=NSTAT+1
          IF(NSTAT.GT.SIZE(STNAME)) THEN
            WRITE(LFNERR,21) NSTAT,SIZE(STNAME)
21          FORMAT(/,' *** SR HEDIN2: TOO MANY STATIONS',/,
     1                           16X,'NUMBER OF STATIONS:',I4,/,
     2                           16X,'MAXIMUM NUMBER    :',I4,/)
            CALL EXITRC(2)
          ENDIF
          STNAME(NSTAT)=STAFIL(IST,IF)
          STFIL(IST,IF)=NSTAT
45      CONTINUE
C
50    CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
