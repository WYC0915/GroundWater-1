      MODULE s_GTVELO
      CONTAINS

C*
      SUBROUTINE GTVELO(FILKEY,INUV1A,NFLAG,FLAGS,NSTAT,XSTAT,STNAME,
     1                  XVEL,VELFLG,VFOUND,IRCVEL)
CC
CC NAME       :  GTVELO
CC
CC PURPOSE    :  READ VELOCITY FIELD FROM FILE FILVEL AND SORT IN SAME ORDER
CC               AS STANAM
CC               (IF FILVEL IS SPECIFIED IN N-FILE, ELSE USE NUVEL1 MODEL)
CC
CC               GET ALL VELOCITIES WITH FLAG IN "FLAGS"
CC               IF ONE OF THE FLAGS IS '@' ALL STATIONS ARE RETURNED
CC               IF ONE OF THE FLAGS IS '#' ALL STATIONS WITH NON-
CC               BLANK FLAGS ARE RETURNED
CC
CC PARAMETERS :
CC         IN :  FILKEY : OPEN FILE IN N FILE WITH KEY        CH*7
CC                        BLANK: SEARCH FOR KEYWORDS FIXVEL OR
CC                               VELTRN
CC               INUV1A : =0 NUVEL1                           I*4
CC                        =1 NUVEL1A
CC               NFLAG  : NUMBER OF FLAGS                     I*4
CC               FLAGS(I),I=1,..,NFLAG: FLAGS TO BE SEARCHED  CH*1
CC                        FOR
CC               NSTAT  : NUMBER OF STATIONS
CC               XSTAT(K,I),K=1,2,3;I=1,..,NSTAT: RECTANGULAR R*8
CC                        STATION COORDINATES
CC               STNAME(I),I=1,2,..,NSTAT  : STATION NAMES    CH*16
CC        OUT :  XVEL(K,I),K=1,3,I=1,NSTAT: ACCORDING
CC                        VELOCITIES                          R*8(*,*)
CC               VELFLG(I),I=1,..,NSTAT: STATION FLAGS        CH*1
CC               VFOUND(I),I=1,..,NSTAT: VELOCITY FOUND OR NOT L(*)
CC               IRCVEL : 1: NO SPECIAL VELOCITIES READ       I*4
CC                        0: SPECIAL VELOCITY READ (FILE/NUVEL)
CC
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  E. BROCKMANN
CC
CC VERSION    :  3.5
CC
CC CREATED    :  16-JAN-1994 11:00
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC               19-FEB-96 : EB: NEW CALL (INUV1A)
CC               30-AUG-02 : MR: MAXSTA from 500 --> 700
CC               28-JUN-04 : RD: USE GETCO3 TO READ VEL-FILES
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               19-JAN-11 : RD: FLAG STATIONS WHERE VELOCITIES WERE FOUND
CC               14-FEB-11 : RD: REMOVE MAXSTA-COMMON (UNUSED)
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1994     UNIVERSITY OF BERN
CC               SWITZERLAND
C*
      USE M_BERN
      USE s_nuvela
      USE s_getco3
      USE s_nuvel1
      USE s_exitrc
      USE s_gtflna
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IFLAG , INUV1A, IRC   , IRC1  , IRC2  , IRCVEL,
     1          ISTA  , IVEL  , K     , NFLAG , NSTAT , NVEL
C
      REAL*8    T     , T0
C
CCC       IMPLICIT  REAL*8  (A-H,O-Z)
C
C
c      PARAMETER (MAXSTA=700)
C MAXFIL: MAXIMUM NUMBER OF INPUT FILES
C MAXSTA: MAXIMUM NUMBER STATIONS IN VELOCITY FILE
C
C DECLARATIONS
C ------------
      CHARACTER*16  STNAME(*)
      CHARACTER*1   VELFLG(*),FLAGS(*)
C
      REAL*8        XVEL(3,*),XSTAT(3,*)
      LOGICAL       VFOUND(*)
C
C LOCAL DECLARATIONS
C ------------------
      CHARACTER*32  FILTRN,FILVEL
      CHARACTER(LEN=*) ::   FILKEY
      REAL*8        XHLP(3),VHLP(3)
C
      CHARACTER(LEN=staNameLength), DIMENSION(:),   POINTER :: STAVEL
      CHARACTER(LEN= 4),            DIMENSION(:),   POINTER :: PSIT
      CHARACTER(LEN= 1),            DIMENSION(:),   POINTER :: STFLAG
      INTEGER(i4b),                 DIMENSION(:),   POINTER :: NRVEL
      REAL(r8b),                    DIMENSION(:,:), POINTER :: VEL
C
C INITIALISATION
C --------------
      IRCVEL=1
      DO 5 I=1,NSTAT
        XVEL(1,I)=0.D0
        XVEL(2,I)=0.D0
        XVEL(3,I)=0.D0
        VFOUND(I)=.FALSE.
5     CONTINUE
C
C TEST IF VELOCITY FIELD FILE EXIST
C ---------------------------------
      IF (FILKEY.EQ.'       ') THEN
        CALL GTFLNA(0,'FIXVEL ',FILVEL,IRC1)
        CALL GTFLNA(0,'VELTRN ',FILTRN,IRC2)
      ELSE
        CALL GTFLNA(0,FILKEY,FILVEL,IRC1)
        IRC2=1
      ENDIF
C
      IF (IRC1.EQ.0.OR.IRC2.EQ.0) IRCVEL=0
      IF (IRC1.NE.0.AND.IRC2.NE.0) GOTO 900
C
      IF (IRC1.EQ.0) THEN
C
C OPEN VELOCITY FIELD FILE
C ------------------------
CC        CALL GTFLNA(1,'FIXVEL ',FILVEL,IRC)
        NULLIFY(NRVEL)
        NULLIFY(stavel)
        NULLIFY(VEL)
        NULLIFY(STFLAG)
        CALL getco3(FILVEL,1,(/'@'/),NVEL,STAVEL,
     1              STANUM=NRVEL,XSTAT=VEL,STAFLG=STFLAG)
c        CALL OPNFIL(LFNLOC,FILVEL,'OLD',' ','READONLY',' ',IOSTAT)
c        CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILVEL,'GTVELO')
C
C READ VELOCITY OF THE STATIONS
C -----------------------------
c        NVEL=0
c        READ(LFNLOC,10)
c10      FORMAT(/////)
c        DO 20 ISTA=1,100000
c          IF (ISTA.GT.MAXSTA) THEN
c            WRITE(LFNERR,901) FILVEL,ISTA,MAXSTA
c901         FORMAT(/,' *** SR GTVELO: TOO MANY STATIONS IN FILE ',A32
c     1              ,/,16X,'NUMBER OF STATIONS     :',I3,/,
c     2                 16X,'MAX. NUMBER OF STATIONS:',I3,/)
c            CALL EXITRC(2)
c          ENDIF
C
c          READ(LFNLOC,21,END=30) NRVEL(ISTA),STAVEL(ISTA),
c     1                           (VEL(K,ISTA),K=1,3),STFLAG(ISTA)
c21        FORMAT(I3,2X,A16,3F15.4,4X,A1)
c          IF(STAVEL(ISTA).EQ.' ') GOTO 30
C
c20      CONTINUE
c30      NVEL=ISTA-1
C
        DO 100 ISTA=1,NSTAT
          DO 200 IVEL=1,NVEL
            IF (STNAME(ISTA).EQ.STAVEL(IVEL)) THEN
C
C FLAG OF STATION OK ?
              DO 220 IFLAG=1,NFLAG
                IF(FLAGS(IFLAG).EQ.'@')                        GOTO 210
                IF(FLAGS(IFLAG).EQ.'#'.AND.STFLAG(IVEL).NE.' ')GOTO 210
                IF(FLAGS(IFLAG).EQ.STFLAG(IVEL))               GOTO 210
220           CONTINUE
              GOTO 100
C
210           DO 300 K=1,3
                IF (VEL(K,IVEL).GT.10.D0) THEN
                  WRITE(LFNERR,905)STNAME(ISTA)
905               FORMAT(/,' *** SR GTVELO: ERROR: VELOCITY ',/,
     1            16X,'LARGER 10 M (!) FOUND FOR STATION: ',A16,/)
                  CALL EXITRC(2)
                ENDIF
                XVEL(K,ISTA)=VEL(K,IVEL)
300           CONTINUE
              VELFLG(ISTA)=STFLAG(IVEL)
              VFOUND(ISTA)=.TRUE.
              GOTO 100
            ENDIF
200       CONTINUE
C          WRITE(LFNERR,903)STNAME(ISTA)
C903       FORMAT(/,' *** SR GTVELO: WARNING: NO VELOCITY ',/,
C     1       16X,'FOUND FOR STATION: ',A16,/)
100     CONTINUE
c        CLOSE(UNIT=LFNLOC)
        DEALLOCATE(NRVEL,STAT=IRC)
        DEALLOCATE(STAVEL,STAT=IRC)
        DEALLOCATE(VEL,STAT=IRC)
        DEALLOCATE(STFLAG,STAT=IRC)
C
      ELSE
C
C OPEN STATION-PLATE TRANSLATION TABLE
C ------------------------------------
        CALL GTFLNA(1,'VELTRN ',FILTRN,IRC)
        NULLIFY(NRVEL)
        NULLIFY(stavel)
        NULLIFY(PSIT)
        CALL getco3(FILTRN,1,(/'@'/),NVEL,STAVEL,
     1              PLATE=PSIT)
c        CALL OPNFIL(LFNLOC,FILTRN,'OLD',' ','READONLY',' ',IOSTAT)
c        CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILTRN,'GTVELO')
C
C READ PLATES OF THE STATIONS
C ---------------------------
c        NVEL=0
c        READ(LFNLOC,10)
c        DO 120 ISTA=1,100000
c          IF (ISTA.GT.MAXSTA) THEN
c            WRITE(LFNERR,901) FILTRN,ISTA,MAXSTA
c            CALL EXITRC(2)
c          ENDIF
c          READ(LFNLOC,121,END=130) NRVEL(ISTA),STAVEL(ISTA),
c     1                             PSIT(ISTA)
c121       FORMAT(I3,2X,A16,45X,9X,A4)
c          IF(STAVEL(ISTA).EQ.' ') GOTO 130
c120     CONTINUE
c130     NVEL=ISTA-1
C
        DO 150 ISTA=1,NSTAT
          DO 250 IVEL=1,NVEL
            IF (STNAME(ISTA).EQ.STAVEL(IVEL)) THEN
              T0=1.D0
              T=2.D0
              IF (INUV1A.EQ.0) THEN
                CALL NUVEL1(PSIT(IVEL),T0,XSTAT(1,ISTA),XSTAT(2,ISTA),
     1                    XSTAT(3,ISTA),T,XHLP(1),XHLP(2),XHLP(3),
     2                    VHLP(1),VHLP(2),VHLP(3))
              ELSE
                CALL NUVELA(PSIT(IVEL),T0,XSTAT(1,ISTA),XSTAT(2,ISTA),
     1                    XSTAT(3,ISTA),T,XHLP(1),XHLP(2),XHLP(3),
     2                    VHLP(1),VHLP(2),VHLP(3))
              ENDIF
              IF (XHLP(1).EQ.0.D0.AND.XHLP(2).EQ.0.D0
     1            .AND.XHLP(3).EQ.0.D0) THEN
                WRITE(LFNERR,904)PSIT(IVEL),STNAME(ISTA)
904             FORMAT(/,' *** SR GTVELO: PLATENAME ',A4,/,
     1               16X,'WAS NOT FOUND FOR STATION: ',A16,/)
                CALL EXITRC(2)
              ENDIF
              DO 350 K=1,3
                XVEL(K,ISTA)=VHLP(K)
350           CONTINUE
              VELFLG(ISTA)='V'
              VFOUND(ISTA)=.TRUE.
              GOTO 150
            ENDIF
250       CONTINUE
C          WRITE(LFNERR,902)STNAME(ISTA)
C902       FORMAT(/,' *** SR GTVELO: WARNING: NO VELOCITY ',/,
C     1       16X,'FOUND FOR STATION: ',A16,/)
150     CONTINUE
c        CLOSE(UNIT=LFNLOC)
        DEALLOCATE(NRVEL,STAT=IRC)
        DEALLOCATE(STAVEL,STAT=IRC)
        DEALLOCATE(PSIT,STAT=IRC)
      ENDIF
C
900   RETURN
C
      END SUBROUTINE

      END MODULE
