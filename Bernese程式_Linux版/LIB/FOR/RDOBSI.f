      MODULE s_RDOBSI
      CONTAINS

C*
      SUBROUTINE RDOBSI(LFNOBS,IFRMAT,NFRQS,IFRQS,OBSTIM,DELTAT,EPOFLG,
     1                  NSAT,NRSAT,OBSFLG,OBSERV,IRETRN)
CC
CC NAME       :  RDOBSI
CC
CC PURPOSE    :  READ OBSERVATIONS OF ONE EPOCH (PHASE OR CODE,ZERO OR
CC               SINGLE DIFFERENCES) AND RETURN OBSERVATIONS FOR THE
CC               FREQUENCIES REQUESTED (ARRAY "IFRQS")
CC
CC PARAMETERS :
CC         IN :  LFNOBS : LOGICAL FILE NUMBER OF THE OBS.FILE  I*4
CC               IFRMAT : FILE FORMAT NUMBER                   I*4
CC               NFRQS  : NUMBER OF FREQUENCIES REQUESTED      I*4
CC               IFRQS  : FREQUENCIES REQUESTED                I*4(*)
CC                          1: L1 OBSERVATION
CC                          2: L2 OBSERVATION
CC                          3: L3 (IONOSPHERE FREE LINEAR COMB.)
CC                          4: L4 (IONOSPHERE)
CC                          5: L5 (WIDELANE)
CC        OUT :  OBSTIM : OBSERVATION TIME IN MODIF.JUL.DATE   R*8
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
CC               OBSFLG : OBSERVATION FLAG                     CH*1(*,*)
CC                          BIT 0 = 1: OBS. MARKED
CC                          BIT 1 = 1: CYCLE SLIP REMOVED
CC                          BIT 4-7  : SIGNAL STRENGTH
CC                          OBSFLG(I,J): SATELLITE I
CC                                       FREQUENCY J
CC               OBSERV : OBSERVATIONS OF THE FREQUENCIES RE-  R*8(*,*)
CC                        QUESTED
CC                          OBSERV(I,J): SATELLITE I
CC                                       FREQUENCY J
CC               IRETRN : RETURN CODE                          I*4
CC                          0: OBSERVATION FOUND
CC                          1: END OF FILE REACHED
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
CC               22-OCT-97 : HH: USE MAXSAT.inc" AND "COMFREQ
CC               15-AUG-99 : JJ: RM UNUSED VAR INIT
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               11-MAR-10 : SL: ONLY added to USE m_bern
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnErr
      USE m_maxdim, ONLY: MAXSAT
      USE f_ior
      USE s_maxtst
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , ICH1  , ICH2  , IFIRST, IFRMAT, IFRQ  , IRC   ,
     1          IRETRN, ISAT  , LFNOBS, MXCSAT, NFRQS , NSAT
C
      REAL*8    OBSTIM
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C EXPLICIT DECLARATIONS
C ---------------------
C
      CHARACTER*6 MXNSAT
      CHARACTER*1 EPOFLG,OBSFLG(MXCSAT,*),FLAG(MAXSAT,2)
C
      REAL*8      DELTAT(2),OBSERV(MXCSAT,*),OBS(MAXSAT,2)
      REAL*8      FAC1(MAXSAT),FAC2(MAXSAT)
C
      INTEGER*4   IFRQS(*),NRSAT(*)
C
      INTEGER*2   NFRQ1,NSAT1,NRSAT1(MAXSAT)
C
C COMMON WITH GENERAL CONSTANTS
C -----------------------------
      INCLUDE 'COMFREQ.inc'
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
C
      DATA IFIRST/1/
C
C CHECK LOCAL MAXIMUM DIMENSIONS
C ------------------------------
      IF(IFIRST.EQ.1) THEN
        CALL MAXTST(1,'RDOBSI',MXNSAT,MAXSAT,MXCSAT,IRC)
        IF(IRC.NE.0) CALL EXITRC(2)
        IFIRST=0
      ENDIF
C
C READ NEXT RECORD OF THE OBSERVATION FILE
C ----------------------------------------
      READ(LFNOBS,END=9999) NFRQ1,NSAT1,OBSTIM,(DELTAT(I),I=1,2),EPOFLG,
     1                      (NRSAT1(ISAT),
     2                      (FLAG(ISAT,IFRQ),OBS(ISAT,IFRQ),
     3                      IFRQ=1,NFRQ1),ISAT=1,NSAT1)
C
C CHECK NUMBER OF SATELLITES
C --------------------------
      IF(NSAT1.GT.MXCSAT) THEN
        WRITE(LFNERR,1) NSAT1,MXCSAT,LFNOBS
1       FORMAT(/,' *** SR RDOBSI: TOO MANY SATELLITES  :',I3,/,
     1                       16X,'MAXIMUM # OF SAT.    :',I3,/,
     2                       16X,'LOGICAL FILE NUMBER  :',I3,/)
        CALL EXITRC(2)
      ENDIF
C
C CONVERSIONS TO INTEGER*4
C ------------------------
      NSAT=NSAT1
      DO 10 ISAT=1,NSAT
        NRSAT(ISAT)=NRSAT1(ISAT)
10    CONTINUE
C
C LOOP OVER REQUESTED FREQUENCIES
C -------------------------------
      DO 1000 IFRQ=1,NFRQS
C
C ONLY ONE FREQUENCY: NO LINEAR COMBINATIONS CAN BE FORMED AND NO L2
        IF(NFRQ1.EQ.1.AND.IFRQS(IFRQ).GT.1) GOTO 900
        GOTO (100,100,300,400,500) IFRQS(IFRQ)
C
C ILLEGAL FREQUENCY
        WRITE(LFNERR,2) IFRQS(IFRQ),LFNOBS
2       FORMAT(/,' *** SR RDOBSI: ILLEGAL FREQUENCY  : L',I1,/,
     1                       16X,'LOGICAL FILE NUMBER: ',I2,/)
        CALL EXITRC(2)
C
C L1,L2 OBSERVATION
100     DO 110 ISAT=1,NSAT
          OBSERV(ISAT,IFRQ)=OBS(ISAT,IFRQS(IFRQ))
          OBSFLG(ISAT,IFRQ)=FLAG(ISAT,IFRQS(IFRQ))
110     CONTINUE
        GOTO 1000
C
C L3 OBSERVATION
300     DO 310 ISAT=1,NSAT
          FAC1(ISAT)=+FRQ(1,NRSAT(ISAT))**2/
     1               (FRQ(1,NRSAT(ISAT))**2-FRQ(2,NRSAT(ISAT))**2)
          FAC2(ISAT)=-FRQ(2,NRSAT(ISAT))**2/
     1               (FRQ(1,NRSAT(ISAT))**2-FRQ(2,NRSAT(ISAT))**2)
310     CONTINUE
        GOTO 800
C
C L4 OBSERVATION
400     DO 410 ISAT=1,NSAT
          FAC1(ISAT)=+1
          FAC2(ISAT)=-1
410     CONTINUE
        GOTO 800
C
C L5 OBSERVATION
500     DO 510 ISAT=1,NSAT
          FAC1(ISAT)=+WLGT(5,NRSAT(ISAT))/WLGT(1,NRSAT(ISAT))
          FAC2(ISAT)=-WLGT(5,NRSAT(ISAT))/WLGT(2,NRSAT(ISAT))
510     CONTINUE
        GOTO 800
C
C FORM LINEAR COMBINATION
800     DO 810 ISAT=1,NSAT
          IF(OBS(ISAT,1).EQ.0.D0.OR.OBS(ISAT,2).EQ.0.D0) THEN
            OBSERV(ISAT,IFRQ)=0.D0
            OBSFLG(ISAT,IFRQ)=CHAR(0)
          ELSE
            OBSERV(ISAT,IFRQ)=FAC1(ISAT)*OBS(ISAT,1)+
     1                        FAC2(ISAT)*OBS(ISAT,2)
            ICH1=MOD(ICHAR(FLAG(ISAT,1)),16)
            ICH2=MOD(ICHAR(FLAG(ISAT,2)),16)
            OBSFLG(ISAT,IFRQ)=CHAR(IOR(ICH1,ICH2))
          ENDIF
810     CONTINUE
        GOTO 1000
C
C OBSERVATION NOT AVAILABLE
900     DO 910 ISAT=1,NSAT
          OBSERV(ISAT,IFRQ)=0.D0
          OBSFLG(ISAT,IFRQ)=CHAR(0)
910     CONTINUE
C
1000  CONTINUE
C
      IRETRN=0
      RETURN
C
C END OF FILE REACHED
C -------------------
9999  IRETRN=1
      RETURN
      END SUBROUTINE

      END MODULE
