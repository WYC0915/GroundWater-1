      MODULE s_WTFMTI
      CONTAINS

C*
      SUBROUTINE WTFMTI(LFNFMT,INIT,
     1                  IFRMAT,NFREQ,TIMREF,IDELTT,DTBLNK,OBSTIM,
     2                  DELTAT,EPOFLG,NSAT,NRSAT,OBSFLG,OBSERV)
CC
CC NAME       :  WTFMTI
CC
CC PURPOSE    :  WRITE OBSERVATIONS OF ONE EPOCH INTO A FORMATTED
CC               OBSERVATION FILE (PHASE OR CODE, ZERO OR SINGLE
CC               DIFFERENCES)
CC
CC PARAMETERS :
CC         IN :  LFNFMT : LOGICAL FILE NUMBER OF THE OBS.FILE  I*4
CC               INIT   : INITIALIZATION FOR NEW FILE (YES=1)  I*4
CC               IFRMAT : FILE FORMAT NUMBER                   I*4
CC               NFREQ  : NUMBER OF FREQUENCIES                I*4
CC                          1: L1 OBSERVATIONS
CC                          2: L1 AND L2 OBSERVATIONS
CC               TIMREF : REFERENCE EPOCH IN MODIF.JUL.DATE    R*8
CC                        WITHOUT FRACTION OF SECOND
CC               IDELTT : OBSERVATION INTERVAL IN SEC          I*4
CC               DTBLNK : MINIMUM DIFFERENCE BETWEEN THE       R*8
CC                        EXPECTED AND THE ACTUAL OBSERVATION
CC                        EPOCH TO INSERT A BLANK LINE IN THE
CC                        FORMATTED FILE
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
CC CREATED    :  87/09/23 16:41
CC
CC CHANGES    :  23-NOV-93 : SF: SET MAXSAT TO 30
CC               10-AUG-94 : MR: CALL EXITRC
CC               22-SEP-97 : DI: USE MAXSAT.inc
CC               22-OCT-97 : MR: FORMAT REPETITION SET TO 24
CC                               STRING LENGTH OF 'LINE' SET TO 559
CC               13-MAY-98 : MR: REMOVE "DFLOAT"
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               15-AUG-06 : HB: SHIFT SECOND LINE OF OBSERVATION RECORD
CC               08-JUN-10 : DT: EPOCH NUMBER INCREASED (I5->I8)
CC               22-SEP-10 : DT: EPOCH POSITION CORRECTED
CC               26-JAN-11 : LP: EPOCH POSITION FOR 2nd FREQ CORRECTED;
CC                               DELTAT WRITTEN WITH EXPONENTIAL FORMAT
CC               28-JUN-11 : HB: SHIFT ALSO SECOND LINE OF OBSERVATIONS
CC                               RECORD DUE TO INCREASED EPOCH NUMBER
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE m_maxdim, ONLY: MAXSAT
      USE f_tstflg
      USE s_maxtst
      USE s_exitrc
      USE s_jmt
      USE s_radgms
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , ICHDAT, ICHEND, IDAY  , IDELTT, IEPOCH, IFIRST,
     1          IFLG  , IFRMAT, IFRQ  , IHOUR , IMIN  , IMONTH, INIT  ,
     2          IRC   , ISAT  , ISEC  , IYEAR , LFNFMT, MAXFLG, MXCSAT,
     3          NFREQ , NSAT  , NSATO
C
      REAL*8    DAY   , DTBLNK, OBSTIM, OBSTIO, SEC   , TIMREF
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
      CHARACTER*1   EPOFLG,OBSFLG(MXCSAT,2),VORZ,EPFTXT
      CHARACTER*6   MXNSAT
      CHARACTER*8   TIMTXT,DATTXT
      CHARACTER*559 LINE
      REAL*8        DELTAT(2),OBSERV(MXCSAT,2)
      INTEGER*4     NRSAT(MXCSAT)
C
C LOCAL DIMENSIONS
      CHARACTER*1   FLAG(MAXSAT,2,MAXFLG),FLGTXT(MAXFLG)
      INTEGER*4     NRSATO(MAXSAT),ISGNAL(MAXSAT,2)
      DATA IFIRST/1/
      DATA FLGTXT/'M','S'/
C
C CHECK MAXIMUM LOCAL DIMENSION
      IF(IFIRST.EQ.1) THEN
        IFIRST=0
        CALL MAXTST(1,'WTFMTI',MXNSAT,MAXSAT,MXCSAT,IRC)
        IF(IRC.NE.0) CALL EXITRC(2)
      ENDIF
C
C CHECK NUMBER OF FREQUENCIES
C ---------------------------
      IF(NFREQ.LT.1.OR.NFREQ.GT.2) THEN
        WRITE(LFNERR,1) NFREQ,LFNFMT
1       FORMAT(/,' *** SR WTFMTI: ILLEGAL NUMBER OF FREQUENCIES: ',I2,/,
     1                       16X,'LOGICAL FILE NUMBER          : ',I2,/)
        CALL EXITRC(2)
      ENDIF
C
C SATELLITE SCENARIO CHANGE OR BREAK: INSERT BLANK LINE
C -----------------------------------------------------
      IF(INIT.EQ.1) THEN
        INIT=0
      ELSE
        IF(DABS(OBSTIO+IDELTT/86400.D0-OBSTIM).GT.DTBLNK.OR.
     1     NSAT.NE.NSATO) THEN
          WRITE(LFNFMT,2)
2         FORMAT(' ')
        ELSE
          DO 10 ISAT=1,NSAT
            IF(NRSAT(ISAT).NE.NRSATO(ISAT)) THEN
              WRITE(LFNFMT,2)
              GOTO 20
            ENDIF
  10      CONTINUE
  20      CONTINUE
        ENDIF
      ENDIF
C
C SAVE OBSERVATION TIME AND SATELLITE SCENARIO IN "NRSATO"
C --------------------------------------------------------
      OBSTIO=OBSTIM
      NSATO =NSAT
      DO 30 ISAT=1,NSAT
        NRSATO(ISAT)=NRSAT(ISAT)
30    CONTINUE
C
C EPOCH NUMBER
C ------------
      IEPOCH=IDNINT((OBSTIM-TIMREF)*86400.D0/IDELTT+1.D0)
C
C CONVERSION TO DATE AND TIME
C ---------------------------
      OBSTIM=OBSTIM+1.D-8
      CALL JMT(OBSTIM,IYEAR,IMONTH,DAY)
      IYEAR=MOD(IYEAR,100)
      IDAY=IDINT(DAY)
      CALL RADGMS(3,DAY,VORZ,IHOUR,IMIN,SEC)
      ISEC=IDNINT(SEC)
      WRITE(DATTXT,3) IYEAR,IMONTH,IDAY
      WRITE(TIMTXT,4) IHOUR,IMIN,ISEC
3     FORMAT(I2,'-',I2,'-',I2)
4     FORMAT(I2,':',I2,':',I2)
      IF(DATTXT(4:4).EQ.' ') DATTXT(4:4)='0'
      IF(DATTXT(7:7).EQ.' ') DATTXT(7:7)='0'
      IF(TIMTXT(4:4).EQ.' ') TIMTXT(4:4)='0'
      IF(TIMTXT(7:7).EQ.' ') TIMTXT(7:7)='0'
C
C EPOCH FLAG
C ----------
      IF(TSTFLG(EPOFLG,0)) THEN
        EPFTXT='F'
      ELSE
        EPFTXT=' '
      ENDIF
C
C GET FLAG BITS (INCLUDING SIGNAL STRENGTH)
C -----------------------------------------
      DO 60 ISAT=1,NSAT
        DO 50 IFRQ=1,NFREQ
          ISGNAL(ISAT,IFRQ)=ICHAR(OBSFLG(ISAT,IFRQ))/16
          DO 40 IFLG=1,MAXFLG
            IF(TSTFLG(OBSFLG(ISAT,IFRQ),IFLG-1)) THEN
              FLAG(ISAT,IFRQ,IFLG)=FLGTXT(IFLG)
            ELSE
              FLAG(ISAT,IFRQ,IFLG)=' '
            ENDIF
40        CONTINUE
50      CONTINUE
60    CONTINUE
C
C WRITE NEXT RECORD OF THE OBSERVATION FILE
C -----------------------------------------
      ICHDAT=23+21*NSAT+1
      ICHEND=23+21*NSAT+35
      LINE=' '
      WRITE(LINE,5) IEPOCH,TIMTXT,EPFTXT,NSAT,
     1              (OBSERV(ISAT,1),
     2               (FLAG(ISAT,1,IFLG),IFLG=MAXFLG,1,-1),
     3               ISGNAL(ISAT,1),NRSAT(ISAT),ISAT=1,NSAT)
      WRITE(LINE(ICHDAT:ICHEND),6) DATTXT,(DELTAT(I),I=1,2)
      WRITE(LFNFMT,7) LINE(1:ICHEND)
5     FORMAT(I8,2X,A8,1X,A1,I3,24(F14.3,2A1,I1,I4))
6     FORMAT(1X,A8,2F13.9)
C6     FORMAT(1X,A8,2(1X,E12.6))
7     FORMAT(A)
      IF(NFREQ.EQ.2) WRITE(LFNFMT,8) (OBSERV(ISAT,2),(FLAG(ISAT,2,IFLG),
     1                                IFLG=MAXFLG,1,-1),ISGNAL(ISAT,2),
     2                                NRSAT(ISAT),ISAT=1,NSAT)
8     FORMAT(23X,24(F14.3,2A1,I1,I4))
C
      RETURN
      END SUBROUTINE

      END MODULE
