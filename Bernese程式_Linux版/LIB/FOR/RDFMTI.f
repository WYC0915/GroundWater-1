      MODULE s_RDFMTI
      CONTAINS

C*
      SUBROUTINE RDFMTI(LFNFMT,IFRMAT,NFREQ,OBSTIM,DELTAT,EPOFLG,
     1                  NSAT,NRSAT,OBSFLG,OBSERV,IRETRN)
CC
CC NAME       :  RDFMTI
CC
CC PURPOSE    :  READ OBSERVATIONS OF ONE EPOCH FROM A FORMATTED
CC               OBSERVATION FILE (PHASE OR CODE, ZERO OR SINGLE
CC               DIFFERENCES)
CC
CC PARAMETERS :
CC         IN :  LFNFMT : LOGICAL FILE NUMBER OF THE OBS.FILE  I*4
CC               IFRMAT : FILE FORMAT NUMBER                   I*4
CC               NFREQ  : NUMBER OF FREQUENCIES                I*4
CC                          1: L1 OBSERVATIONS
CC                          2: L1 AND L2 OBSERVATIONS
CC         OUT:  OBSTIM : OBSERVATION TIME IN MODIF.JUL.DATE   R*8
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
CC               22-SEP-97 : DI: USE MAXSAT.inc
CC               22-OCT-97 : MR: FORMAT REPETITION SET TO 24
CC               01-JUL-99 : PF: CALL IYEAR4 FOR CONVERSION YY->YYYY
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               08-JUN-10 : DT: EPOCH NUMBER INCREASED (I5->I8)
CC               22-SEP-10 : DT: EPOCH POSITION CORRECTED
CC               08-DEC-10 : HB: FORMAT EXTENDED
CC               26-JAN-11 : LP: EPOCH POSITION for 2nd FREQ CORRECTED;
CC                               FORMAT-CHECK CORRECTED (USE IFRMAT # FROM HEADER)
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE m_maxdim, ONLY: MAXSAT
      USE f_djul
      USE s_clrflg
      USE s_maxtst
      USE s_setflg
      USE s_exitrc
      USE f_iyear4
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , ICHDAT, ICHEND, IDAY  , IEPOCH, IFLG  , IFRMAT,
     1          IFRQ  , IHOUR , IMIN  , IMONTH, IRC   , IRETRN, ISAT  ,
     2          ISEC  , IYEAR , LFNFMT, MAXFLG, MXCSAT, NFREQ ,
     3          NSAT
C
      REAL*8    DAY   , OBSTIM
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MAXFLG=2)
C
C
C COMMON WITH MAXIMAL DIMENSIONS
C ------------------------------
      COMMON/MCMSAT/MXCSAT,MXNSAT
C
C EXPLICIT DECLARATIONS
C ---------------------
      CHARACTER*1   EPOFLG,OBSFLG(MXCSAT,2),EPFTXT
      CHARACTER*6   MXNSAT
      CHARACTER*559 LINE
      REAL*8        DELTAT(2),OBSERV(MXCSAT,2)
      INTEGER*4     NRSAT(MXCSAT)
C
C LOCAL DIMENSIONS
      CHARACTER*1   FLAG(MAXSAT,2,MAXFLG)
      INTEGER*4     ISGNAL(MAXSAT,2)
C
C CHECK MAXIMUM DIMENSION MAXSAT
      CALL MAXTST(1,'RDFMTI',MXNSAT,MAXSAT,MXCSAT,IRC)
      IF(IRC.NE.0) CALL EXITRC(2)
C
C CHECK NUMBER OF FREQUENCIES
C ---------------------------
      IF(NFREQ.LT.1.OR.NFREQ.GT.2) THEN
        WRITE(LFNERR,1) NFREQ,LFNFMT
1       FORMAT(/,' *** SR RDFMTI: ILLEGAL NUMBER OF FREQUENCIES: ',I2,/,
     1                       16X,'LOGICAL FILE NUMBER          : ',I2)
        CALL EXITRC(2)
      ENDIF
C
C READ NEXT RECORD(S) OF THE OBSERVATION FILE
C -------------------------------------------
10    READ(LFNFMT,7,END=999) LINE
C
C CHECK FOR BLANK LINE: SATELLITE SCENARIO CHANGE
      IF(LINE.EQ.' ') GOTO 10

C CHECK IF IEPOCH WRITTEN WITH 5 or 8 digits
C 5 digits
      IF (IFRMAT<6) THEN
        READ(LINE,5) IEPOCH,IHOUR,IMIN,ISEC,EPFTXT,NSAT,
     1              (OBSERV(ISAT,1),
     2              (FLAG(ISAT,1,IFLG),IFLG=MAXFLG,1,-1),
     3              ISGNAL(ISAT,1),NRSAT(ISAT),ISAT=1,NSAT)
5       FORMAT(I5,1X,3(1X,I2),1X,A1,I3,24(F14.3,2A1,I1,I4))
        ICHDAT=20+21*NSAT+1
        ICHEND=20+21*NSAT+35
C 8 digits
      ELSE
        READ(LINE,8) IEPOCH,IHOUR,IMIN,ISEC,EPFTXT,NSAT,
     1              (OBSERV(ISAT,1),
     2              (FLAG(ISAT,1,IFLG),IFLG=MAXFLG,1,-1),
     3              ISGNAL(ISAT,1),NRSAT(ISAT),ISAT=1,NSAT)
8       FORMAT(I8,1X,3(1X,I2),1X,A1,I3,24(F14.3,2A1,I1,I4))
        ICHDAT=23+21*NSAT+1
        ICHEND=23+21*NSAT+35
      ENDIF
      READ(LINE(ICHDAT:ICHEND),6) IYEAR,IMONTH,IDAY,(DELTAT(I),I=1,2)
6     FORMAT(3(1X,I2),2F13.9)
7     FORMAT(A)
C
C READ L2 OBSERVATIONS
      IF(NFREQ.EQ.2) THEN
        IF (IFRMAT<6) THEN
C 5 digits
          READ(LFNFMT,9) (OBSERV(ISAT,2),(FLAG(ISAT,2,IFLG),
     1                IFLG=MAXFLG,1,-1),ISGNAL(ISAT,2),
     2                NRSAT(ISAT),ISAT=1,NSAT)
9         FORMAT(20X,24(F14.3,2A1,I1,I4))
        ELSE
C 8 digits
          READ(LFNFMT,11) (OBSERV(ISAT,2),(FLAG(ISAT,2,IFLG),
     1                IFLG=MAXFLG,1,-1),ISGNAL(ISAT,2),
     2                NRSAT(ISAT),ISAT=1,NSAT)
11        FORMAT(23X,24(F14.3,2A1,I1,I4))
        ENDIF
      ENDIF
C
C CONVERSION TO MJD
C -----------------
      IYEAR = IYEAR4(IYEAR)
      DAY=IDAY+IHOUR/24.D0+IMIN/1440.D0+ISEC/86400.D0
      OBSTIM=DJUL(IYEAR,IMONTH,DAY)
C
C EPOCH FLAG
C ----------
      IF(EPFTXT.EQ.' ') THEN
        EPOFLG=CHAR(0)
      ELSE
        EPOFLG=CHAR(1)
      ENDIF
C
C SET FLAG BITS (INCLUDING SIGNAL STRENGTH)
C -----------------------------------------
      DO 60 ISAT=1,NSAT
        DO 50 IFRQ=1,NFREQ
          OBSFLG(ISAT,IFRQ)=CHAR(ISGNAL(ISAT,IFRQ)*16)
          DO 40 IFLG=1,MAXFLG
            IF(FLAG(ISAT,IFRQ,IFLG).EQ.' ') THEN
              CALL CLRFLG(OBSFLG(ISAT,IFRQ),IFLG-1)
            ELSE
              CALL SETFLG(OBSFLG(ISAT,IFRQ),IFLG-1)
            ENDIF
40        CONTINUE
50      CONTINUE
60    CONTINUE
C
      IRETRN=0
      RETURN
C
C END OF FILE REACHED
C -------------------
999   IRETRN=1
      RETURN
      END SUBROUTINE

      END MODULE
