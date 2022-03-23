      MODULE s_IONSES
      CONTAINS

C*
      SUBROUTINE IONSES(CSESS,SESSID,NFTOT,NDIFF,FILNAM,STANAM,
     1                  MEATYP,NEPOCH,NSATEL,IDELTT,TIMREF,
     2                  NUMSAT,XSTAT,XSTELL,IFRMAT,NFIL,INDFIL,
     3                  HEDFIS,OBSFIS,STNAMS,MTYPS,NEPOS,NSATS,
     4                  IDELTS,TIMRES,NUMSAS,XSTAS,XSTELS,IFRMAS)
CC
CC NAME       :  IONSES
CC
CC PURPOSE    :  THIS SUBROUTINE SELECTS ALL FILES OF ONE SESSION
CC               AND STORES ALL INFORMATION NEEDED FOR FURTHER
CC               COMPUTATIONS IN SESSION SPECIFIC ARRAYS
CC
CC PARAMETERS :
CC         IN :  CSESS  : SESSION DEFINITION                 CH*4(2)
CC                          CSESS(1): SESSION IDENTIFIER
CC                          CSESS(2): FILE IDENT. OF THE SAME
CC                                    SESSION
CC               SESSID : SESSION IDENTIFIERS                CH*4(*)
CC               NFTOT  : TOTAL NUMBER OF FILES               I*4
CC               NDIFF  : DIFFERENCE LEVEL (ZERO/SINGLE)      I*4
CC               FILNAM : NAMES OF HEADER AND OBSERVATION    CH*32
CC                        FILES:
CC                          FILNAM(1,I),I=1,NFTOT: HEADERS
CC                          FILNAM(2,I),I=1,NFTOT: OBSERV.
CC               STANAM : STATION NAMES
CC                        STANAM(I,J),I=1,2,J=1,MAXFIL       CH*16(*,*)
CC               MEATYP : MEASUREMENT TYPE (1:PHASE,2:CODE)
CC                        MEATYP(I), I=1,MAXFIL               I*4(*)
CC               NEPOCH : NUMBER OF EPOCHS IN ONE FILE
CC                        NEPOCH(I),I=1,MAXFIL                I*4(*)
CC               NSATEL : NUMBER OF SATELLITES IN ONE FILE
CC                        NSATEL(I),I=1,MAXFIL                I*4(*)
CC               IDELTT : SAMPLING RATE (SEC)
CC                        IDELTT(I),I=1,MAXFIL                I*4(*)
CC               TIMREF : REFERENCE TIME
CC                        TIMREF(I),I=1,MAXFIL                I*4(*)
CC               NUMSAT : SATELLITE NUMBERS
CC                        NUMSAT(I,J),I=1,MAXSAT,J=1,MAXFIL   I*4(*,*)
CC               XSTAT  : STATION COORDINATES
CC                        XSTAT(I,J),I=1,3,J=1,MAXFIL         R*8(*,*)
CC               XSTELL : STATION COORDINATES (ELLIPSOIDAL)
CC                        XSTELL(I,J),I=1,3,J=1,MAXFIL        R*8(*,*)
CC               IFRMAT : FILE FORMAT NUMBERS                 I*4
CC                        IFRMAT(J),J=1,MAXFIL
CC        OUT :  NFIL   : NUMBER OF FILES IN CURRENT SESSION  I*4
CC               INDFIL : FILE INDEX                          I*4
CC               HEDFIS : NAMES OF HEADER FILES
CC                        HEDFIS(I),I=1,NFIL                 CH*32(*)
CC               OBSFIS : NAMES OF OBSERVATION FILES
CC                        OBSFIS(I),I=1,NFIL                 CH*32(*)
CC               STNAMS : STATION NAMES
CC                        STNAMS(I,J),I=1,2,J=1,MAXFIL       CH*16(*,*)
CC               MTYPS  : MEASUREMENT TYPE (1:PHASE,2:CODE)
CC                        MTYPS(I), I=1,NFIL                  I*4(*)
CC               NEPOS  : NUMBER OF EPOCHS IN ONE FILE
CC                        NEPOS(I),I=1,NFIL                   I*4(*)
CC               NSATS  : NUMBER OF SATELLITES IN ONE FILE
CC                        NSATS(I),I=1,NFIL                   I*4(*)
CC               IDELTS : SAMPLING RATE (SEC)
CC                        IDELTS(I),I=1,NFIL                  I*4(*)
CC               TIMRES : REFERENCE TIME
CC                        TIMRES(I),I=1,NFIL                  I*4(*)
CC               NUMSAS : SATELLITE NUMBERS
CC                        NUMSAS(I,J),I=1,MAXSAT,J=1,NFIL     I*4(*,*)
CC               XSTAS  : STATION COORDINATES
CC                        XSTAS(I,J),I=1,3,J=1,MAXFIL         R*8(*,*)
CC               XSTELS : STATION COORDINATES (ELLIPSOIDAL)
CC                        XSTELS(I,J),I=1,3,J=1,MAXFIL        R*8(*,*)
CC               IFRMAS : FILE FORMAT NUMBERS
CC                        IFRMAS(J),J=1,MAXFIL                I*4
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  U. WILD
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  89/08/04 09:42
CC
CC CHANGES    :  14-AUG-94 : MR: FORMAT 4: SESSION AS CHARACTER*4
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1989     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IFIL  , INDEX , ISAT  , J     , MXCFIL, MXCSAT,
     1          NDIFF , NFIL  , NFILS , NFTOT
C
CCC       IMPLICIT REAL*8(A-H,O-Z)
C
C COMMONS
C -------
      COMMON/MCMFIL/MXCFIL,MXNFIL
      COMMON/MCMSAT/MXCSAT,MXNSAT
C
      REAL*8         XSTAT(3,2,MXCFIL),XSTELL(3,2,MXCFIL)
      REAL*8         XSTAS(3,2,MXCFIL),XSTELS(3,2,MXCFIL)
      REAL*8         TIMREF(MXCFIL),TIMRES(MXCFIL)
C
      INTEGER*4      IFRMAT(MXCFIL)
      INTEGER*4      MEATYP(MXCFIL),NEPOCH(MXCFIL),NSATEL(MXCFIL)
      INTEGER*4      IDELTT(MXCFIL)
      INTEGER*4      MTYPS(MXCFIL),NEPOS(MXCFIL),NSATS(MXCFIL)
      INTEGER*4      IDELTS(MXCFIL)
      INTEGER*4      NUMSAT(MXCSAT,MXCFIL),NUMSAS(MXCSAT,MXCFIL)
      INTEGER*4      INDFIL(MXCFIL),IFRMAS(MXCFIL)
C
      CHARACTER*32   FILNAM(2,*),HEDFIS(MXCFIL),OBSFIS(MXCFIL)
      CHARACTER*16   STANAM(2,MXCFIL),STNAMS(2,MXCFIL)
      CHARACTER*6    MXNSAT,MXNFIL
      CHARACTER*4    CSESS(2,MXCFIL),SESSID(MXCFIL)
C
      DATA INDEX/0/,NFILS/0/
C
C INITIALIZATION
C --------------
      NFIL   = 0
      INDEX = INDEX+1
C
C SELECT ALL FILES OF ONE SESSION
C -------------------------------
      DO 20 IFIL = 1,NFTOT
        IF (CSESS(1,IFIL) .EQ. SESSID(INDEX)) THEN
          NFIL = NFIL+1
          HEDFIS(NFIL) = FILNAM(1,IFIL)
          OBSFIS(NFIL) = FILNAM(2,IFIL)
          MTYPS(NFIL)  = MEATYP(IFIL)
          NEPOS(NFIL)  = NEPOCH(IFIL)
          NSATS(NFIL)  = NSATEL(IFIL)
          IDELTS(NFIL) = IDELTT(IFIL)
          TIMRES(NFIL) = TIMREF(IFIL)
          IFRMAS(NFIL) = IFRMAT(IFIL)
          DO 25 J = 1,NDIFF+1
            STNAMS(J,NFIL) = STANAM(J,IFIL)
25        CONTINUE
C
          DO 30 ISAT = 1,NSATS(NFIL)
            NUMSAS(ISAT,NFIL) = NUMSAT(ISAT,IFIL)
30        CONTINUE
C
          DO 40 J = 1,NDIFF+1
            DO 40 I = 1,3
              XSTAS(I,J,NFIL)  = XSTAT(I,J,IFIL)
              XSTELS(I,J,NFIL) = XSTELL(I,J,IFIL)
40        CONTINUE
C
          INDFIL(NFILS+NFIL) = IFIL
C
        ENDIF
C
20    CONTINUE
C
      NFILS = NFILS + NFIL
C
      RETURN
      END SUBROUTINE

      END MODULE
