      MODULE s_WTOBSI
      CONTAINS

C*
      SUBROUTINE WTOBSI(LFNOBS,IFRMAT,NFREQ,OBSTIM,DELTAT,EPOFLG,
     1                  NSAT,NRSAT,OBSFLG,OBSERV)
CC
CC NAME       :  WTOBSI
CC
CC PURPOSE    :  WRITE OBSERVATIONS OF ONE EPOCH INTO AN OBSERVATION
CC               FILE (PHASE OR CODE, ZERO OR SINGLE DIFFERENCES)
CC
CC PARAMETERS :
CC         IN :  LFNOBS : LOGICAL FILE NUMBER OF THE OBS.FILE  I*4
CC               IFRMAT : FILE FORMAT NUMBER                   I*4
CC               NFREQ  : NUMBER OF FREQUENCIES                I*4
CC                          1: L1 OBSERVATIONS
CC                          2: L1 AND L2 OBSERVATIONS
CC               OBSTIM : OBSERVATION TIME IN MODIF.JUL.DATE   R*8
CC                        WITHOUT FRACTION OF SECOND
CC               DELTAT : SMALL TIME CORRECTIONS (SEC)         R*8(2)
CC                        FOR ZERO DIFF.:
CC                          DELTAT(1) = FRACTION OF SECOND OF
CC                            OBSERVATION TIME
CC                          DELTAT(2) = RECEIVER CLOCK CORREC-
CC                            TION
CC                        FOR SINGLE DIFF.:
CC                          DELTAT(1) = CORRECTION TO "OBSTIM" TO
CC                            GET OBSERV.TIME OF RECEIVER 1 +
CC                            CLOCK CORRECTION OF RECEIVER 1
CC                          DELTAT(2) = CORRECTION TO "OBSTIM" TO
CC                            GET OBSERV.TIME OF RECEIVER 2 +
CC                            CLOCK CORRECTION OF RECEIVER 2
CC               EPOFLG : EPOCH FLAG                           CH*1
CC               NSAT   : NUMBER OF SATELLITES AT THIS EPOCH   I*4
CC               NRSAT  : SATELLITE NUMBERS AT THIS EPOCH      I*4(*)
CC               OBSFLG : OBSERVATION FLAG                     CH*1(*,2)
CC                          BIT 0 = 1: OBS. MARKED
CC                          BIT 1 = 1: CYCLE SLIP REMOVED
CC                          BIT 4-7  : SIGNAL STRENGTH
CC                          OBSFLG(I,J): SATELLITE I
CC                                       FREQUENCY J
CC               OBSERV : OBSERVATIONS OF THE FREQUENCIES RE-  R*8(*,2)
CC                        QUESTED
CC                          OBSERV(I,J): SATELLITE I
CC                                       FREQUENCY J
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M.ROTHACHER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/09/23 16:41
CC
CC CHANGES    :  23-NOV-93 : SF: SET MAXSAT TO 30
CC               10-AUG-94 : MR: CALL EXITRC
CC               22-SEP-97 : DI: USE MAXSAT.inc
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
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
      INTEGER*4 I     , IFIRST, IFRMAT, IFRQ  , IRC   , ISAT  , LFNOBS,
     1          MXCSAT, NFREQ , NSAT
C
      REAL*8    OBSTIM
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C MAXIMAL DIMENSIONS FOR INTERNAL ARRAYS
C --------------------------------------
C
C EXPLICIT DECLARATIONS
C ---------------------
      CHARACTER*1 EPOFLG,OBSFLG(MXCSAT,2)
      CHARACTER*6 MXNSAT
C
      REAL*8      DELTAT(2),OBSERV(MXCSAT,2)
C
      INTEGER*4   NRSAT(*)
      INTEGER*2   NFRQ1,NSAT1,NRSAT1(MAXSAT)
C
C
C COMMON WITH MAXIMAL DIMENSIONS
C ------------------------------
      COMMON/MCMSAT/MXCSAT,MXNSAT
      DATA IFIRST/0/
C
C CHECK LOCAL MAXIMUM DIMENSIONS
C ------------------------------
      IF(IFIRST.EQ.0) THEN
        CALL MAXTST(1,'WTOBSI',MXNSAT,MAXSAT,MXCSAT,IRC)
        IF(IRC.NE.0) CALL EXITRC(2)
        IFIRST=1
      ENDIF
C
C NO SATELLITES TO BE WRITTEN
C ---------------------------
      IF(NSAT.EQ.0) GOTO 999
C
C CHECK NUMBER OF FREQUENCIES
C ---------------------------
      IF(NFREQ.LT.1.OR.NFREQ.GT.2) THEN
        WRITE(LFNERR,1) NFREQ,LFNOBS
1       FORMAT(/,' *** SR WTOBSI: ILLEGAL NUMBER OF FREQUENCIES: ',I2,/,
     1                       16X,'LOGICAL FILE NUMBER          : ',I2,/)
        CALL EXITRC(2)
      ENDIF
C
C CONVERSIONS TO INTEGER*4
C ------------------------
      NFRQ1=NFREQ
      NSAT1=NSAT
      DO 10 ISAT=1,NSAT
        NRSAT1(ISAT)=NRSAT(ISAT)
10    CONTINUE
C
C WRITE NEXT RECORD OF THE OBSERVATION FILE
C -----------------------------------------
      WRITE(LFNOBS) NFRQ1,NSAT1,OBSTIM,(DELTAT(I),I=1,2),EPOFLG,
     1              (NRSAT1(ISAT),(OBSFLG(ISAT,IFRQ),OBSERV(ISAT,IFRQ),
     2                             IFRQ=1,NFRQ1),ISAT=1,NSAT1)
C
999   RETURN
      END SUBROUTINE

      END MODULE
