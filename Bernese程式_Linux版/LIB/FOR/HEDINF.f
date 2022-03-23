      MODULE s_HEDINF
      CONTAINS

C*
      SUBROUTINE HEDINF(NFTOT ,HEADER,MAXCMP,NCAMP ,CAMPGN,NDIFF ,
     1                  NSTAT ,STNAME,STFIL ,NSCAMP,STCAMP,MEATYP,
     2                  NSATEL,CSESS ,IDELTT,TIMREF,ICAMPN,NFREQ ,
     3                  NEPOCH,IRMARK,IRUNIT,POSECC,NUMOBS,NUMMRK,
     4                  NUMOBT,NUMMRT,ICLOCK,
     5                  RECTYP,ANTTYP,IANTEN,IFRMAT,NEPFLG,SATNUM,
     6                  NUMAMB,AMBSAT,AMBIEP,AMBWLF,AMBIGU,AMBCLS,
     7                  ISASYS,MIXED,USEGEOS,GOBSDEF)
CC
CC NAME       :  HEDINF
CC
CC PURPOSE    :  READ RELEVANT HEADER INFORMATION FOR NFTOT FILES
CC
CC PARAMETERS :
CC         IN :  NFTOT  : TOTAL NUMBER OF FILES               I*4
CC               HEADER(I),I=1,2,..,NFTOT: HEADER FILE NAMES  CH*(*)
CC               MAXCMP : MAXIMUM NUMBER OF CAMPAIGNS         I*4
CC        OUT :  NCAMP  : NUMBER OF CAMPAIGNS                 I*4
CC               CAMPGN(I),I=1,..,NCAMP: CAMPAIGNS            CH*16
CC               NDIFF(I),I=1,NFTOT: DIFFERENCE LEVEL         I*4
CC               NSTAT  : NUMBER OF STATIONS IN OBS. FILES    I*4
CC               STNAME(I),I=1,2,..,NSTAT: STATION NAMES      CH*16
CC               STFIL(K,I),K=1,2, I=...: INTERNAL NUMBERS    I*4
CC                     OF STATIONS OBSERVING IN FILE I
CC               NSCAMP(I),I=1,..,NCAMP: NUMBER OF STATIONS   I*4
CC                        FOR CAMPAIGN I
CC               STCAMP(K,I),K=1,..,NSCAMP,I=1,..,NCAMP:      I*4
CC                        STATION INDICES FOR CAMPAGN I
CC               MEATYP(I),I=1,2,..,NFTOT: MEASUREMENT TYPE   I*4
CC               NSATEL(I),I=1,2,..,NFTOT: NUMBER OF SATEL-   I*4
CC                        LITES PER FILE
CC               CSESS(K,IF),K=1,2,I=1,..: SESSION IDENTIFIER CH*4
CC                        IDENT. OF FILES FROM SAME BASELINE/SESSION
CC               IDELTT(I),I=... : TABLE INTERVAL IN SEC      I*4
CC               TIMREF(I),I=... : REFERENCE TIME OF FILE     R*8
CC               ICAMPN(I),I=1,..: CAMPAIGN NUM. OF THE FILE  I*4
CC               NFREQ(I),I=1,.. : NUMBER OF FREQUENCIES IN   I*4
CC                        FILE I
CC               NEPOCH(I),I=1,..: NUMBER OF EPOCHS IN FILE   I*4
CC               IRMARK(I),I=1,..: REMARK NUMBER OF FILE I    I*4
CC               IRUNIT(K,I),K=1,2,I=...: RECEIVER UNIT NRS.  I*4
CC               POSECC(K,J,L),K=1,2,3,J=1,2,L=1,...          R*8
CC                        POSITIONING ECCENTRICITIES
CC                        K: COORDINATE
CC                        L: STATION
CC                        I: FILE
CC               NUMOBS(I,J) LOCAL ARRAY WITH NUMBER OF OBSER-I*4
CC                        VATIONS
CC               NUMMRK(I,J) LOCAL ARRAY WITH NUMBER OF MARK. I*4
CC                        OBSERVATIONS
CC               NUMOBT(I,J): TOTAL NUMBER OF GOOD OBSERVAT.  I*4
CC                        I: FREQUENCY INDEX (1 OR 2)
CC                        J: FILE INDEX
CC               NUMMRT(I,J): TOTAL NUMBER OF BAD OBSERVAT.   I*4
CC                        I: FREQUENCY INDEX (1 OR 2)
CC                        J: FILE INDEX
CC               ICLOCK : TYPE OF CLOCK PARAMETERS             I*4(2)
CC                          ICLOCK(I): FOR STATION I
CC                          =  0: NO CLOCK CORRECTION IN FILE
CC                          >  0: NUMBER OF CLOCK PARAMETERS
CC                                (=POLYNOMIAL DEGREE + 1)
CC                          =999: ONE OFFSET PER EPOCH ESTIM.
CC               RECTYP(K,I),K=1,2,I=...: RECEIVER TYPES      CH*20
CC               ANTTYP(K,I),K=1,2,I=...: ANTENNA  TYPES      CH*20
CC               IANTEN: RECEIVER ANTENNA NUMBERS             I*4(2)
CC               IFRMAT: FILE FORMAT NUMBER                   I*4
CC               NEPFLG: NUMBER OF FLAGGED EPOCHS             I*4
CC               SATNUM(K,I),K=1,..,MXCSAT,I=...: SATELLITE   I*4
CC                        NUMBERS
CC               NUMAMB(I),I=1,..,NFTOT: NUMBER OF AMBIGU.    I*4
CC               AMBSAT(J,I),J=1,..,NUMAMB(I),I=1,..,NFTOT:   I*4
CC                        AMBIGUITY SATELLITE NUMBERS
CC               AMBIEP(J,I),J=1,..,NUMAMB(I),I=1,..,NFTOT:   I*4
CC                        STARTING EPOCH NRS FOR AMBIGUITIES
CC               AMBWLF(K,J,I),K=1,..,J=1,2,I=1,..,NFTOT:     I*4
CC                        WAVELENGTH FACTORS
CC                        K: AMBIGUITY
CC                        J: FREQUENCY
CC                        I: FILE
CC               AMBIGU(K,J,I),... : VALUES FOR AMBIGUITIES   R*8
CC               AMBCLS(L,K,I),L=1,..,NUMAMB(I), K=1,2,3,     I*4
CC                        I=1,..,NFTOT: AMBIGUITY CLUSTERS
CC               ISASYS : SATELLITE SYSTEM TO BE CONSIDERED   I*4
CC                        =0: ALL
CC                        =1: GPS
CC                        =2: GLONASS
CC                        =3: GALILEO
CC                        =4: GPS/GLO
CC                        =5: GPS/GAL
CC                        =6: GLO/GAL
CC                        ADD_GNSS_HERE
CC               MIXED  : SATELLITE SYSTEM                    I*4
CC                        =0: GPS ONLY
CC                        =1: MIXED (GPS AND/OR GLONASS AND/OR GALILEO)
CC                        =2: GLONASS ONLY
CC                        =3: GALILEO ONLY
CC                        ADD_GNSS_HERE
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, M.ROTHACHER
CC
CC CREATED    :  87/10/14 14:47
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC               12-AUG-94 : MR: FORMAT 4: SESSION AS CHARACTER*4
CC               07-SEP-94 : MR: REMOVE UNNECESSARY VARIABLES
CC               07-APR-95 : MR: TOTAL NUMBER OF OBSERV. AS PARAM.
CC               23-JUN-98 : MR: ADD "MIXED" TO CALL HEDINF
CC               19-NOV-01 : RD: INIT STFIL TO "0"
CC               08-SEP-03 : HU: ANTNAM, RECNAM CHR16 -> CHR20,
CC                               FILENAMES CHR(*)
CC               08-MAR-04 : RD: SYNCHRONIZE USER INPUT ISASYS AND MIXED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               03-APR-07 : AG: USE SVNSYS INSTEAD OF MIXSVN
CC               26-JAN-11 : LP: Sat-specific obs types; DEFREQ changes;bugfix;
CC                               MIX removed
CC               14-FEB-11 : RD: REMOVE MAXSTA-COMMON (NOT NEEDED)
CC               06-JUN-12 : LP: USE MAXSYS; USE F_SVNSYS AS MODULE NOW
CC               30-JUL-12 : RD: CHECK OBSNUM TO COMPUTE VARIABLE MIXED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,  ONLY: lfnerr
      USE m_global, ONLY: maxsys
      USE d_rinex3, ONLY: t_gobsdef
      USE f_mixsvn
      USE s_rdhead
      USE s_exitrc
      USE f_svnsys
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 ICAMP , IF    , IFREQ , ISASYS, ISATEL, ISCAMP, IST   ,
     1          ISTAT , MAXCMP, MIXED , MXCAMB, MXCSAT,
     2          NCAMP , NFTOT , NSTAT , NSAHLP
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      REAL*8        TIMREF(*),POSECC(3,2,*)
      REAL*8        AMBIGU(MXCAMB,3,*)
C
      INTEGER*4     MEATYP(*),NSATEL(*),SATHLP(MXCSAT)
      INTEGER*4     IDELTT(*),IRUNIT(2,*),ICLOCK(2,*)
      INTEGER*4     SATNUM(MXCSAT,*),STFIL(2,*)
      INTEGER*4     AMBWLF(MXCAMB,2,*),NUMAMB(*)
      INTEGER*4     AMBSAT(MXCAMB,*),AMBCLS(MXCAMB,3,*)
      INTEGER*4     AMBIEP(MXCAMB,*)
      INTEGER*4     IANTEN(2,*),NUMOBS(MXCSAT,2),NUMMRK(MXCSAT,2)
      INTEGER*4     NUMOBT(2,*),NUMMRT(2,*)
      INTEGER*4     NFREQ(*),NEPOCH(*),IRMARK(*)
      INTEGER*4     NSCAMP(*),STCAMP(:,:),ICAMPN(*)
      INTEGER*4     NEPFLG(*),IFRMAT(*),NDIFF(*),isys,usegeos(*)
C
      CHARACTER*53  TITFIL
      CHARACTER*(*) HEADER(*)
      CHARACTER*16  STNAME(*),STANAM(2)
      CHARACTER*16  CAMPGN(*),CAMPG1
      CHARACTER*20  RECTYP(2,*),ANTTYP(2,*),OPRNAM(2)
      CHARACTER*9   CRDATE(2)
      CHARACTER*6   MXNSAT,MXNAMB
      CHARACTER*5   CRTIME(2)
      CHARACTER*4   CSESS(2,*)
C
      TYPE(t_gobsdef) :: gobsdef(*) ! Giove External Obs. Selection info
C
      LOGICAL       IsSYS(10)
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMAMB/MXCAMB,MXNAMB
C
C LOOP OVER ALL FILES
C -------------------
      NCAMP=0
      NSTAT=0
      IsSYS=.false.

      DO 50 IF=1,NFTOT
C
C READ OBSERVATION HEADER FILE
C ----------------------------
        USEGEOS(IF)=0
        GOBSDEF(IF)%norec=0
        CALL RDHEAD(HEADER(IF),MEATYP(IF),NDIFF(IF),NFREQ(IF),
     1       NEPOCH(IF),NSATEL(IF),
     2       CSESS(1,IF),IDELTT(IF),TIMREF(IF),
     3       CAMPG1,TITFIL,CRDATE,CRTIME,IRMARK(IF),NEPFLG(IF),
     4       IFRMAT(IF),STANAM,RECTYP(1,IF),ANTTYP(1,IF),
     5       IRUNIT(1,IF),IANTEN(1,IF),OPRNAM,POSECC(1,1,IF),
     6       ICLOCK(1,IF),SATNUM(1,IF),NUMOBS,NUMMRK,
     7       NUMAMB(IF),AMBSAT(1,IF),AMBIEP(1,IF),AMBWLF(1,1,IF),
     8       AMBIGU(1,1,IF),AMBCLS(1,1,IF),1,usegeos=usegeos(IF),
     9       gobsdef=gobsdef(IF))
C
C NEW CAMPAIGN ?
        DO 12 ICAMP=1,NCAMP
          IF(CAMPGN(ICAMP).EQ.CAMPG1) GOTO 15
12      CONTINUE
        NCAMP=NCAMP+1
        CAMPGN(NCAMP)=CAMPG1
        NSCAMP(NCAMP)=0
        ICAMP=NCAMP
        IF(NCAMP.GT.MAXCMP) THEN
          WRITE(LFNERR,14) NCAMP,MAXCMP
14        FORMAT(/,' *** SR HEDINF: TOO MANY CAMPAIGNS',/,
     1                         16X,'NUMBER OF CAMPAIGNS:',I3,/,
     2                         16X,'MAXIMUM NUMBER     :',I3,/)
          CALL EXITRC(2)
        ENDIF
15      CONTINUE
C
C SET CAMPAIGN NUMBER OF FILE
        ICAMPN(IF)=ICAMP
C
C NEW STATION ?
        STFIL(1,IF)=0
        STFIL(2,IF)=0
        DO 45 IST=1,NDIFF(IF)+1
          DO 20 ISTAT=1,NSTAT
            IF(STNAME(ISTAT).EQ.STANAM(IST)) THEN
              STFIL(IST,IF)=ISTAT
              GOTO 30
            ENDIF
20        CONTINUE
          NSTAT=NSTAT+1
          STNAME(NSTAT)=STANAM(IST)
          STFIL(IST,IF)=NSTAT
          IF(NSTAT.GT.SIZE(STCAMP,1)) THEN
            WRITE(LFNERR,21) NSTAT,SIZE(STCAMP,1)
21          FORMAT(/,' *** SR HEDINF: TOO MANY STATIONS',/,
     1                           16X,'NUMBER OF STATIONS:',I4,/,
     2                           16X,'MAXIMUM NUMBER    :',I4,/)
            CALL EXITRC(2)
          ENDIF
30        CONTINUE
          DO 40 ISCAMP=1,NSCAMP(ICAMP)
            IF(STCAMP(ISCAMP,ICAMP).EQ.STFIL(IST,IF)) GOTO 45
40        CONTINUE
          NSCAMP(ICAMP)=NSCAMP(ICAMP)+1
          STCAMP(NSCAMP(ICAMP),ICAMP)=STFIL(IST,IF)
45      CONTINUE
C
C TOTAL NUMBER OF GOOD AND BAD OBSERVATIONS
        DO 100 IFREQ=1,NFREQ(IF)
          NUMOBT(IFREQ,IF)=0
          NUMMRT(IFREQ,IF)=0
          DO 90 ISATEL=1,NSATEL(IF)
            NUMOBT(IFREQ,IF)=NUMOBT(IFREQ,IF)+NUMOBS(ISATEL,IFREQ)
            NUMMRT(IFREQ,IF)=NUMMRT(IFREQ,IF)+NUMMRK(ISATEL,IFREQ)
90        CONTINUE
100     CONTINUE
C
C MIXED SATELLITES ?
        NSAHLP=0
        DO ISATEL=1,NSATEL(IF)
          IF (MIN(NUMOBS(ISATEL,1),NUMOBS(ISATEL,NFREQ(IF))).GT.0) THEN
            NSAHLP=NSAHLP+1
            SATHLP(NSAHLP)=SATNUM(ISATEL,IF)
          ENDIF
        ENDDO
C
        DO isys=0,(maxsys-1)
          IF(SVNSYS(isys,NSAHLP,SATHLP)) IsSYS(isys+1)=.true.
CC                        ADD_GNSS_HERE
        ENDDO
C
50    CONTINUE
C
C Check compatibility with the users selection for the satellite system
C ---------------------------------------------------------------------
      IF (IsSYS(1)) MIXED=0
      IF (IsSYS(2)) MIXED=2
      IF (IsSYS(3)) MIXED=3
      IF ((IsSYS(1) .AND. IsSYS(2)) .OR.
     1    (IsSYS(1) .AND. IsSYS(3)) .OR.
     2    (IsSYS(2) .AND. IsSYS(3)))    MIXED=1
CC                        ADD_GNSS_HERE
      IF(MIXED.EQ.0.AND.ISASYS.NE.1.AND.ISASYS.NE.4.AND.ISASYS.NE.5.AND.
     1                                  ISASYS.NE.0)THEN
        WRITE(LFNERR,'(/,A,/,16X,A,/)') ' *** SR HEDINF: ' //
     1  'The observation files contain only GPS observations.',
     2  'The user selection for the satellite system is not GPS.'
        CALL EXITRC(2)
      ELSEIF(MIXED.EQ.2.AND.ISASYS.NE.2.AND.ISASYS.NE.4.AND.ISASYS.NE.6
     1                                 .AND.ISASYS.NE.0) THEN
        WRITE(LFNERR,'(/,A,/,16X,A,/)') ' *** SR HEDINF: ' //
     1  'The observation files contain only GLONASS observations.',
     2  'The user selection for the satellite system is not GLONASS.'
        CALL EXITRC(2)
      ELSEIF(MIXED.EQ.3.AND.ISASYS.NE.3.AND.ISASYS.NE.5.AND.ISASYS.NE.6
     1                                 .AND.ISASYS.NE.0) THEN
        WRITE(LFNERR,'(/,A,/,16X,A,/)') ' *** SR HEDINF: ' //
     1  'The observation files contain only GALILEO observations.',
     2  'The user selection for the satellite system is not GALILEO.'
        CALL EXITRC(2)
      ENDIF
C
      IF (MIXED.EQ.1.AND.ISASYS.EQ.1) MIXED=0
      IF (MIXED.EQ.1.AND.ISASYS.EQ.2) MIXED=2
      IF (MIXED.EQ.1.AND.ISASYS.EQ.3) MIXED=3
CC                        ADD_GNSS_HERE
C
      RETURN
      END SUBROUTINE

      END MODULE
