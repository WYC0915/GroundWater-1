      MODULE s_GESTIN
      CONTAINS

C*
      SUBROUTINE GESTIN(OBSFIL,NUMFIL,COOFIL,ECCFIL,ISASYS,NLIST ,
     1                  NUMLST,NAMLST,ISOBST,TIMFIL,STAFIL,NSTFIL,
     2                  NFRFIL,NANT  ,RECNAM,ANTNAM,ANTNUM,NSAT  ,
     3                  SATLST,ISOBSA,NMXSTA,NMXAMB,IRCODE)
CC
CC NAME       :  GESTIN
CC
CC PURPOSE    :  DETERMINE WHICH STATIONS ARE INVOLVED
CC               - READ ALL SITES FROM COORDINATE FILE
CC               - READ ALL HEADERS OF OBS.FILES
CC               - MAKE LIST OF ACTUALLY USED STATIONS
CC               (PROGRAM GPSEST_P)
CC
CC PARAMETERS :
CC         IN :  OBSFIL : LIST OF ALL FULL OBS.FILENAMES      CH*(*)(*)
CC               NUMFIL : NUMBER OF OBS.FILES                  I*4
CC               COOFIL : FULL NAME OF COORDINATES FILE       CH*(*)
CC               ECCFIL : FULL NAME OF ECCENTRICITY FILE      CH*(*)
CC               ISASYS : SATELLITE SYSTEM TO BE CONSIDERED    I*4
CC                        0: ALL
CC                        1: GPS
CC                        2: GLONASS
CC                        3: Galileo
CC                        4: GPS+GLONASS
CC                        5: GPS+Galileo
CC                        6: GLONASS+Galileo
CC               MAXSTA : MAXIMUM NUMBER OF STATIOBS           I*4
CC        OUT :  NLIST  : NUMBER OF STATIONS IN LIST           I*4
CC               NUMLST : LIST OF STATION NUMBERS              I*4(*)
CC               NAMLST : LIST OF STATION NAMES               CH*16(*)
CC               ISOBST : STATION WAS OBSERVED                 I*4(*)
CC               TIMFIL : START/END TIMES OF OBS FILES         R*8(2,*)
CC               STAFIL : LIST OF STATIONS OF FILES           CH*16(2,*)
CC               NSTFIL : STATION NUMBERS IN THE FILES         I*4(2,*)
CC               NFRFIL : NUMBER OF FREQUENCIES IN OBS FILES   I*4(*)
CC               NANT   : NUMBER OF DIFFERENT RECEIVER/ANT.    I*4
CC                        PAIRS
CC               RECNAM(I),I=1,..,NANT: RECEIVER NAMES        CH*20(*)
CC               ANTNAM(I),I=1,..,NANT: ANTENNA  NAMES        CH*20(*)
CC               ANTNUM(I),I=1,..,NANT: ANTENNA NUMBERS        I*4(*)
CC               NSAT   : NUMBER OF SATELLITES IN LIST         I*4
CC               SATLST(I),I=1,..,NSAT: LIST OF SATELLITE NUM. I*4(*)
CC               ISOBSA : SATELLITE WAS OBSERVED               I*4(*)
CC               NMXSTA : computed value for MAXSTA            I*4
CC               NMXAMB : MAX(numamb(1:nftot))                 I*4
CC               IRCODE : RETURN CODE                          I*4
CC                        2: EXIT
CC
CC REMARKS    :
CC
CC AUTHOR     :  W. GURTNER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  04-DEC-89
CC
CC CHANGES    :  22-JAN-93 : ??: OPNFIL
CC               25-MAY-93 : LM: NEW FORMAT  (RDHEAD)
CC               28-OCT-93 : SF: UPDATE HEADER INFORMATION
CC               23-NOV-93 : SF: SET MAXSAT TO 30
CC               14-AUG-94 : MR: FORMAT 4: SESSION AS CHARACTER*4
CC               27-AUG-94 : MR: NO PROMP1 FOR NON-INTERACTIVE MODE
CC               18-SEP-94 : MR: FIRST STATION WAS NOT FIRST STATION
CC               19-SEP-95 : JJ: MAXSTA TO 200 FROM 150
CC               05-OCT-95 : JJ: ADD KEEP_LOWER COMMENTS
CC               22-APR-96 : TS: ZERO DIFFERENCE CHANGES (NDIFF)
CC               22-SEP-97 : DI: USE MAXSAT.inc
CC               01-OCT-97 : TS: MAXSTA=1000
CC               24-SEP-98 : MR: GET LIST OF RECEIVERS AND ANTENNAS
CC               02-MAR-00 : LM: VARIABLE LENGTH OF OBSFIL,COOFIL,ECCFIL
CC               31-MAY-01 : RD: SET END TIME ALSO FOR 0-DIFF FILES
CC               08-JUN-01 : RD: MAX-DIM. FROM INC-FILES INSTEAD OF LOCAL DEF.
CC                               LOCAL MAXSTA IS NOW MAXCRD
CC                               SWITCH OFF INTERACTIVE MODE
CC               28-JUN-01 : RD: GENERATE ALSO A LIST OF SATELLITES
CC               05-JUN-02 : RD: LFNxxx NUMBERS ARE TAKEN FROM GPSEST
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               08-MAR-03 : MR: MAXAMB MOVED TO M_MAXDIM
CC               08-MAR-03 : HU: REMOVE UNUSED MAXxxx
CC               27-JUN-03 : SS: CHECK MAXSTA
CC               08-SEP-03 : HU: ANTNAM, RECNAM CHR16 -> CHR20
CC               11-OCT-04 : RD: ALLSTA/ALLSAT-LISTS ARE POINTERS NOW
CC               24-MAY-05 : RD: HAVE AT LEAST NMXAMB = 1
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               13-FEB-08 : LP: ERROR HANDLING FOR COORDINATE FILE
CC               06-JUL-09 : SL: WRITE STATEMENT CORRECTED
CC               27-MAY-10 : RD: CONSIDER ISASYS WHEN GENERATING THE LISTS
CC               16-JAN-11 : RD: IGNORE STATION NUMBERS FROM FILE
CC               26-JAN-11 : LP: CHANGED RDHEAD CALL
CC               28-MAR-12 : RD: USE SVN2CHR AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1989     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
C
C  GLOBAL
      USE m_bern,   ONLY: i4b, LFNERR, LFNLOC
      USE m_maxdim, ONLY: MAXAMB,MAXSAT
C
      USE s_opnfil
      USE s_alcerr
      USE s_cksizei1
      USE s_rdhead
      USE f_lengt1
      USE s_cksizec1
      USE s_svn2chr
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IANT  , IDELTT, IFILE , IFRMAT, ILIST , IOSTAT,
     1          IRC   , IRCODE, IRMARK, ISAT  , JSAT  , K     , L     ,
     2          MAXCRD, MEATYP, NANT  , NDIFF , NECC  , NEPFLG, ISASYS,
     3          NEPOCH, NFREQ , NINP  , NLIST , NMXAMB, NMXSTA, NSAT  ,
     4          NSATEL, NUMAMB, NUMFIL, INUM  , ISTA  , JSTA
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      CHARACTER    STAFIL(2,*)*16
      CHARACTER(LEN=*) COOFIL,OBSFIL(*),ECCFIL
      CHARACTER*20 RECNAM(*),ANTNAM(*)
C
      INTEGER*4     NFRFIL(*),NSTFIL(2,*),ANTNUM(*)
      INTEGER(i4b),     DIMENSION(:), POINTER :: NUMLST
      INTEGER(i4b),     DIMENSION(:), POINTER :: SATLST
      CHARACTER(LEN=*), DIMENSION(:), POINTER :: NAMLST
      INTEGER(i4b),     DIMENSION(:), POINTER :: ISOBST
      INTEGER(i4b),     DIMENSION(:), POINTER :: ISOBSA
C
      REAL*8       TIMFIL(2,*)
C
C
C  LOCAL
      PARAMETER (MAXCRD=1000)
      CHARACTER(LEN=6), PARAMETER :: SRNAME = 'GESTIN'
      CHARACTER    NAMINP(MAXCRD)*16,STRING*80,SVNCHR*1
      CHARACTER    MARKER(MAXCRD)*16,CENTER(MAXCRD)*16
      INTEGER*4    NUMINP(MAXCRD),MRKNUM(MAXCRD)
C
C MAXSAT: MAXIMUM NUMBER OF SATELLITES
C
      CHARACTER*53 TITLE
      CHARACTER*16 CAMPGN,STANAM(2)
      CHARACTER*20 RECTYP(2),ANTTYP(2),OPRNAM(2)
      CHARACTER*9  CRDATE(2)
      CHARACTER*5  CRTIME(2)
      CHARACTER*4  CSESS(2)
C
      REAL*8       POSECC(3,2),AMBIGU(MAXAMB,3)
      REAL*8       TIMREF
C
      INTEGER*4    IRUNIT(2),IANTEN(2),ICLOCK(2)
      INTEGER*4    NUMSAT(MAXSAT),NUMOBS(MAXSAT,2),NUMMRK(MAXSAT,2)
      INTEGER*4    AMBIEP(MAXAMB),AMBSAT(MAXAMB)
      INTEGER*4    AMBWLF(MAXAMB,2),AMBCLS(MAXAMB,3),READGEOS
C
      LOGICAL      ISSORT
C
      COMMON/LGESTI/NAMINP,MARKER,CENTER,AMBIEP,AMBIGU
C
C LOGICAL FILE NUMBERS AND FILE NAMES
C (These variables are set in GPSEST)
C -----------------------------------
C      INCLUDE 'LFNUM.inc'
C
C  READ COORDINATES FILE
C  ---------------------
      CALL OPNFIL(LFNLOC,COOFIL,'OLD',' ','READONLY',' ',IOSTAT)
      IF(IOSTAT.NE.0) GOTO 930
      READ(LFNLOC,11) STRING,STRING
11    FORMAT(A80,//,22X,A16,///)
      DO 100 I=1,MAXCRD
        READ(LFNLOC,12,END=110) NUMINP(I),NAMINP(I)
12      FORMAT(I3,2X,A16,2X,A16)
        IF(NAMINP(I).EQ.' ') GOTO 110
100   CONTINUE
110   CLOSE(UNIT=LFNLOC)
      NINP=I-1
      NECC=0
C
C  READ ECCENTER FILE
C  ------------------
      IF(ECCFIL.NE.' ') THEN
        CALL OPNFIL(LFNLOC,ECCFIL,'OLD',' ','READONLY',' ',IOSTAT)
        IF(IOSTAT.NE.0) GOTO 940
        READ(LFNLOC,11) STRING,STRING
C
C  GET MARKER AND CENTER NAMES
        DO 200 I=1,MAXCRD
          READ(LFNLOC,12,ERR=950,END=210) MRKNUM(I),MARKER(I),CENTER(I)
          IF(MARKER(I).EQ.' ') GOTO 210
200     CONTINUE
210     CLOSE(UNIT=LFNLOC)
        NECC=I-1
      END IF
      IF(NINP.EQ.MAXCRD.OR.NECC.EQ.MAXCRD) THEN
        WRITE(LFNERR,81) MAXCRD
81      FORMAT(/,' *** SR GESTIN: MAXIMUM NUMBER OF STATIONS ',
     1                      'REACHED OR EXCEEDED'
     2       /,16X,'INCREASE "MAXCRD" IN SUBROUTINE OR',
     3       /,16X,'REDUCE STATIONS IN COORD/ECC FILE',
     4       /,16X,'MAX. NUMBER OF STATIONS ALLOWED:',I5,/)
        GOTO 920
      END IF
C
C READ HEADER FILES
C -----------------
      NLIST  = 0
      NANT   = 0
      NSAT   = 0
      NMXAMB = 1
      NMXSTA = 0
      READGEOS=0
C
      DO 10 IFILE=1,NUMFIL
        CALL RDHEAD(OBSFIL(IFILE),MEATYP,NDIFF,NFREQ,NEPOCH,NSATEL,
     1                    CSESS,IDELTT,TIMREF,CAMPGN,TITLE,CRDATE,
     2                    CRTIME,IRMARK,NEPFLG,IFRMAT,
     3                    STANAM,RECTYP,ANTTYP,IRUNIT,IANTEN,
     4                    OPRNAM,POSECC,ICLOCK,NUMSAT,NUMOBS,NUMMRK,
     5                    NUMAMB,AMBSAT,AMBIEP,AMBWLF,
     6                    AMBIGU,AMBCLS,READGEOS)
        NFRFIL(IFILE)=NFREQ
        TIMFIL(1,IFILE)=TIMREF
        STAFIL(1,IFILE)=STANAM(1)
        IF (NDIFF.EQ.0) THEN
          TIMFIL(2,IFILE)=0.D0
          TIMFIL(2,IFILE)=TIMREF+(NEPOCH-1)*IDELTT/86400.D0
          STAFIL(2,IFILE)=' '
        ELSE
          TIMFIL(2,IFILE)=TIMREF+(NEPOCH-1)*IDELTT/86400.D0
          STAFIL(2,IFILE)=STANAM(2)
        ENDIF
C
        IF (NUMAMB.GT.NMXAMB) NMXAMB = NUMAMB
C
C  COMPOSE LIST OF RECEIVER/ANTENNA PAIRS
C  --------------------------------------
        DO 310 K=1,NDIFF+1
          DO 300 IANT=1,NANT
            IF (RECNAM(IANT).EQ.RECTYP(K) .AND.
     1          ANTNAM(IANT).EQ.ANTTYP(K) .AND.
     2          ANTNUM(IANT).EQ.IANTEN(K) ) GOTO 310
300       CONTINUE
          NANT=NANT+1
          RECNAM(NANT)=RECTYP(K)
          ANTNAM(NANT)=ANTTYP(K)
          ANTNUM(NANT)=IANTEN(K)
310     CONTINUE
C
C  MARK STATIONS OF OBS.FILE IN STATION LIST
C  -----------------------------------------
C
        DO 30 K=1,NDIFF+1
C  LOOK FOR STATION IN ECCENTRICITY FILE
          DO 50 L=1,NECC
            IF(STANAM(K).EQ.MARKER(L)) THEN
C  FOUND: LOOK FOR CENTER IN STATION LIST
              NSTFIL(K,IFILE)=MRKNUM(L)
              DO 60 I=1,NINP
                IF(CENTER(L).EQ.NAMINP(I)) THEN
C
C  ADD NEW STATION TO LIST IF NOT YET INCLUDED
                  DO ILIST=1,NLIST
                    IF (NAMLST(ILIST).EQ.NAMINP(I))THEN
                      DO ISAT=1,NSATEL
                        IF (ISOBST(ILIST).EQ.1) EXIT
                        CALL SVN2CHR(NUMSAT(ISAT),INUM,SVNCHR)
                        IF (SVNCHR.EQ.'G' .AND. (ISASYS.EQ.2.OR.
     1                      ISASYS.EQ.3.OR.ISASYS.EQ.6)) CYCLE
                        IF (SVNCHR.EQ.'R' .AND. (ISASYS.EQ.1.OR.
     1                      ISASYS.EQ.3.OR.ISASYS.EQ.5)) CYCLE
                        IF (SVNCHR.EQ.'E' .AND. (ISASYS.EQ.1.OR.
     1                      ISASYS.EQ.2.OR.ISASYS.EQ.4)) CYCLE
                        IF (SVNCHR.EQ.'S' .AND. ISASYS.NE.0) CYCLE
                        IF (NUMOBS(ISAT,1)+NUMOBS(ISAT,NFREQ).NE.0) THEN
                          ISOBST(ILIST) = 1
                        ENDIF
                      ENDDO
                      GOTO 30
                    ENDIF
                  ENDDO
C
C ALLOCATE THE LIST ARRAYS
                  IF (NLIST.EQ.0) THEN
                    ALLOCATE(NUMLST(NUMFIL),STAT=IRC)
                    CALL ALCERR(IRC,'NUMLST',(/NUMFIL/),SRNAME)
                    ALLOCATE(NAMLST(NUMFIL),STAT=IRC)
                    CALL ALCERR(IRC,'NAMLST',(/NUMFIL/),SRNAME)
                    ALLOCATE(ISOBST(NUMFIL),STAT=IRC)
                    CALL ALCERR(IRC,'ISOBST',(/NUMFIL/),SRNAME)
                  ENDIF
C
C PUT THE NEW STATION INTO THE LIST
                  NMXSTA=NMXSTA+2
                  NLIST=NLIST+1
                  CALL CKSIZEI1(NUMLST,NLIST,NUMFIL)
                  CALL CKSIZEC1(NAMLST,NLIST,NUMFIL)
                  CALL CKSIZEI1(ISOBST,NLIST,NUMFIL)
                  NUMLST(NLIST)=NLIST
                  NAMLST(NLIST)=NAMINP(I)
                  ISOBST(NLIST)=0
                  DO ISAT=1,NSATEL
                    IF (ISOBST(NLIST).EQ.1) EXIT
                    CALL SVN2CHR(NUMSAT(ISAT),INUM,SVNCHR)
                    IF (SVNCHR.EQ.'G' .AND. (ISASYS.EQ.2.OR.
     1                  ISASYS.EQ.3.OR.ISASYS.EQ.6)) CYCLE
                    IF (SVNCHR.EQ.'R' .AND. (ISASYS.EQ.1.OR.
     1                  ISASYS.EQ.3.OR.ISASYS.EQ.5)) CYCLE
                    IF (SVNCHR.EQ.'E' .AND. (ISASYS.EQ.1.OR.
     1                  ISASYS.EQ.2.OR.ISASYS.EQ.4)) CYCLE
                    IF (SVNCHR.EQ.'S' .AND. ISASYS.NE.0) CYCLE
                    IF (NUMOBS(ISAT,1)+NUMOBS(ISAT,NFREQ).NE.0) THEN
                      ISOBST(NLIST) = 1
                    ENDIF
                  ENDDO
                  GOTO 30
                END IF
60            CONTINUE
              WRITE(LFNERR,161) CENTER(L),COOFIL(1:LENGT1(COOFIL)),
     1                     ECCFIL(1:LENGT1(ECCFIL))
161           FORMAT(/,' *** SR GESTIN: CENTER NAME OF ECCENTRICITY',
     1                 ' FILE NOT FOUND',
     2               /,16X,'IN COORDINATE FILE',
     3               /,16X,'CENTER NAME    : ',A,
     4               /,16X,'COORDINATE FILE: ',A,
     5               /,16X,'ECCENTER FILE  : ',A,/)
              GOTO 920
            END IF
50        CONTINUE
C
C  STATION NOT IN ECCENTRICITY FILE: LOOK IN STATION LIST
          DO 20 I=1,NINP
            IF(STANAM(K).EQ.NAMINP(I)) THEN
              NSTFIL(K,IFILE)=NUMINP(I)
C
C  ADD NEW STATION TO LIST IF NOT YET INCLUDED
              DO ILIST=1,NLIST
                IF (NAMLST(ILIST).EQ.NAMINP(I)) THEN
                  DO ISAT=1,NSATEL
                    IF (ISOBST(ILIST).EQ.1) EXIT
                    CALL SVN2CHR(NUMSAT(ISAT),INUM,SVNCHR)
                    IF (SVNCHR.EQ.'G' .AND. (ISASYS.EQ.2.OR.
     1                  ISASYS.EQ.3.OR.ISASYS.EQ.6)) CYCLE
                    IF (SVNCHR.EQ.'R' .AND. (ISASYS.EQ.1.OR.
     1                  ISASYS.EQ.3.OR.ISASYS.EQ.5)) CYCLE
                    IF (SVNCHR.EQ.'E' .AND. (ISASYS.EQ.1.OR.
     1                  ISASYS.EQ.2.OR.ISASYS.EQ.4)) CYCLE
                    IF (SVNCHR.EQ.'S' .AND. ISASYS.NE.0) CYCLE
                    IF (NUMOBS(ISAT,1)+NUMOBS(ISAT,NFREQ).NE.0) THEN
                      ISOBST(ILIST) = 1
                    ENDIF
                  ENDDO
                  GOTO 30
                ENDIF
              ENDDO
C
C ALLOCATE THE LIST ARRAYS
              IF (NLIST.EQ.0) THEN
                ALLOCATE(NUMLST(NUMFIL),STAT=IRC)
                CALL ALCERR(IRC,'NUMLST',(/NUMFIL/),SRNAME)
                ALLOCATE(NAMLST(NUMFIL),STAT=IRC)
                CALL ALCERR(IRC,'NAMLST',(/NUMFIL/),SRNAME)
                ALLOCATE(ISOBST(NUMFIL),STAT=IRC)
                CALL ALCERR(IRC,'ISOBST',(/NUMFIL/),SRNAME)
              ENDIF
C
C PUT THE NEW STATION INTO THE LIST
              NMXSTA=NMXSTA+1
              NLIST=NLIST+1
              CALL CKSIZEI1(NUMLST,NLIST,NUMFIL)
              CALL CKSIZEC1(NAMLST,NLIST,NUMFIL)
              CALL CKSIZEI1(ISOBST,NLIST,NUMFIL)
              NUMLST(NLIST)=NLIST
              NAMLST(NLIST)=NAMINP(I)
              ISOBST(NLIST)=0
              DO ISAT=1,NSATEL
                IF (ISOBST(NLIST).EQ.1) EXIT
                CALL SVN2CHR(NUMSAT(ISAT),INUM,SVNCHR)
                IF (SVNCHR.EQ.'G' .AND. (ISASYS.EQ.2.OR.
     1              ISASYS.EQ.3.OR.ISASYS.EQ.6)) CYCLE
                IF (SVNCHR.EQ.'R' .AND. (ISASYS.EQ.1.OR.
     1              ISASYS.EQ.3.OR.ISASYS.EQ.5)) CYCLE
                IF (SVNCHR.EQ.'E' .AND. (ISASYS.EQ.1.OR.
     1              ISASYS.EQ.2.OR.ISASYS.EQ.4)) CYCLE
                IF (SVNCHR.EQ.'S' .AND. ISASYS.NE.0) CYCLE
                IF (NUMOBS(ISAT,1)+NUMOBS(ISAT,NFREQ).NE.0) THEN
                  ISOBST(NLIST) = 1
                ENDIF
              ENDDO
              GOTO 30
            END IF
20        CONTINUE
C
          WRITE(LFNERR,121) STANAM(K),COOFIL(1:LENGT1(COOFIL))
121       FORMAT(/,' *** SR GESTIN: STATION NAME NOT FOUND IN ',
     1             'COORDINATE FILE',
     2           /,16X,'STATION NAME   : ',A,
     3           /,16X,'COORDINATE FILE: ',A,/)
          GOTO 920
C
30      CONTINUE
C
C GENERATE A LIST OF SATLLITES
C ----------------------------
        DO 40 ISAT = 1, NSATEL
          DO 45 JSAT = 1, NSAT
            IF (NUMSAT(ISAT) .EQ. SATLST(JSAT)) THEN
              IF (NUMOBS(iSat,1) + NUMOBS(iSat,nFreq) == 0) GOTO 40
              CALL SVN2CHR(NUMSAT(ISAT),INUM,SVNCHR)
              IF (SVNCHR.EQ.'G' .AND. (ISASYS.EQ.2.OR.
     1            ISASYS.EQ.3.OR.ISASYS.EQ.6)) GOTO 40
              IF (SVNCHR.EQ.'R' .AND. (ISASYS.EQ.1.OR.
     1            ISASYS.EQ.3.OR.ISASYS.EQ.5)) GOTO 40
              IF (SVNCHR.EQ.'E' .AND. (ISASYS.EQ.1.OR.
     1            ISASYS.EQ.2.OR.ISASYS.EQ.4)) GOTO 40
              IF (SVNCHR.EQ.'S' .AND. ISASYS.NE.0) GOTO 40
              ISOBSA(JSAT)=1
              GOTO 40
            ENDIF
45        CONTINUE
C
C ALLOCATE THE LIST ARRAYS
          IF (NSAT.EQ.0) THEN
            ALLOCATE(SATLST(NSATEL),STAT=IRC)
            CALL ALCERR(IRC,'SATLST',(/NSATEL/),SRNAME)
            ALLOCATE(ISOBSA(NSATEL),STAT=IRC)
            CALL ALCERR(IRC,'ISOBSA',(/NSATEL/),SRNAME)
          ENDIF
          NSAT = NSAT + 1
          CALL CKSIZEI1(SATLST,NSAT,NSATEL)
          CALL CKSIZEI1(ISOBSA,NSAT,NSATEL)
          SATLST(NSAT) = NUMSAT(ISAT)
          ISOBSA(NSAT) = 0
          IF (NUMOBS(iSat,1) + NUMOBS(iSat,nFreq) == 0) GOTO 40
          CALL SVN2CHR(NUMSAT(ISAT),INUM,SVNCHR)
          IF (SVNCHR.EQ.'G' .AND. (ISASYS.EQ.2.OR.
     1        ISASYS.EQ.3.OR.ISASYS.EQ.6)) GOTO 40
          IF (SVNCHR.EQ.'R' .AND. (ISASYS.EQ.1.OR.
     1        ISASYS.EQ.3.OR.ISASYS.EQ.5)) GOTO 40
          IF (SVNCHR.EQ.'E' .AND. (ISASYS.EQ.1.OR.
     1        ISASYS.EQ.2.OR.ISASYS.EQ.4)) GOTO 40
          IF (SVNCHR.EQ.'S' .AND. ISASYS.NE.0) GOTO 40
          ISOBSA(NSAT)=1
40      CONTINUE
C
10    CONTINUE
C
C SORT THE LIST OF SATELLITES
C ---------------------------
70    ISSORT = .TRUE.
      DO 75 ISAT = 1, NSAT - 1
        IF (SATLST(ISAT) .GT. SATLST(ISAT+1)) THEN
          ISSORT = .FALSE.
          JSAT = SATLST(ISAT)
          SATLST(ISAT) = SATLST(ISAT+1)
          SATLST(ISAT+1) = JSAT
          JSAT = ISOBSA(ISAT)
          ISOBSA(ISAT) = ISOBSA(ISAT+1)
          ISOBSA(ISAT+1) = JSAT
        ENDIF
75    CONTINUE
      IF (.NOT. ISSORT) GOTO 70
C
C SORT THE LIST OF STATIONS
C -------------------------
80    ISSORT = .TRUE.
      DO 85 ISTA = 1, NLIST - 1
        IF (NAMLST(ISTA) .GT. NAMLST(ISTA+1)) THEN
          ISSORT = .FALSE.
          STANAM(1:2) = NAMLST(ISTA:ISTA+1)
          NAMLST(ISTA) = STANAM(2)
          NAMLST(ISTA+1) = STANAM(1)
          JSTA = ISOBST(ISTA)
          ISOBST(ISTA) = ISOBST(ISTA+1)
          ISOBST(ISTA+1) = JSTA
        ENDIF
85    CONTINUE
      IF (.NOT. ISSORT) GOTO 80
C
      IRCODE=0
      GOTO 999
C
930   WRITE(LFNERR,931) TRIM(COOFIL)
931   FORMAT(/,' *** SR GESTIN: ERROR OPENING COORDINATE FILE',/,
     1         16X,'COORDINATE FILENAME: ',A,/)
      GOTO 920
C
940   WRITE(LFNERR,941) TRIM(ECCFIL)
941   FORMAT(/,' *** SR GESTIN: ERROR OPENING ECCENTRICITY FILE',/,
     1         16X,'COORDINATE ECCENTRICITY FILENAME: ',A,/)
      GOTO 920
C
950   WRITE(LFNERR,951) TRIM(COOFIL)
951   FORMAT(/,' *** SR GESTIN: FORMAT ERROR IN COORDINATE FILE',/,
     1         16X,'COORDINATE FILENAME: ',A,/)
      GOTO 920
C
920   IRCODE=2
      GOTO 999
C
999   RETURN
      END SUBROUTINE

      END MODULE
