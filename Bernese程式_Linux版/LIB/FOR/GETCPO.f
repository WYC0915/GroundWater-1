      MODULE s_GETCPO
      CONTAINS

C*
      SUBROUTINE GETCPO(TREQ,T,DEPS,DPSI)
CC
CC NAME       :  GETCPO
CC
CC PURPOSE    :  GIVEN THE TIME TREQ, THE VALUES
CC               DEPS,DPSI ARE EXTRACTED FOR FOR TIMES T(1), T(2)
CC               FROM THE POLE FILE.
CC               T(1) IS THE LARGEST TABULAR TIME
CC               SMALLER THEN TREQ, T(2) THE SMALLEST TABULAR
CC               TIME LARGER THEN TREQ.
CC               IF T(2)-T(1) > 10, AN ERROR MESSAGE IS PRINTED,
CC               PROCESSING IS STOPPED.
CC
CC PARAMETERS :
CC         IN :  TREQ            : TIME OF REQUEST              R*8
CC        OUT :  T(I),I=1,2      : TABULAR TIMES                R*8
CC               DEPS(I),I=1,2   : CEL. POLE OFFSETS AT T(I)    R*8
CC               DPSI(I),I=1,2
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  R. WEBER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  09-APR-94
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC               28-MAY-96 : MR: HERRING IN 0.001 MAS
CC               06-JUN-96 : TS: SUBDAILY POLE MODEL CHANGES
CC               09-SEP-98 : LM: 1-MINUTE DIFFERENCE ALLOWED - CORRECTION
CC               04-DEC-02 : PS: CALL SR RDPOLH WITH NUTNAM AND SUBNAM
CC               05-NOV-03 : HU: USE GETPOL TO READ DEPS, DPSI FROM FILE
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-JUN-05 : MM: UNUSED VARIABLES REMOVED
CC               28-FEB-07 : AG: USE 206264... FROM DEFCON
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1994     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE d_const, ONLY: ars
      USE s_rdpolh
      USE s_getpol
      USE s_opnfil
      USE s_gtflna
      USE s_opnerr
      USE s_herr87
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IEND  , IFORM , IOSTAT, IRC
C
      REAL*8    TREQ
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*80 DUMMYC
      CHARACTER*32 FILPOL
      CHARACTER*16 NUTNAM
      CHARACTER*16 SUBNAM
C
      REAL*8       T(2),DEPS(2),DPSI(2)
      REAL*8       X(2),Y(2),UT1UTC(2),GPSUTC(2)
C
      INTEGER*4    POLTYP(2)
C
C
C OPEN POLE FILE
      CALL GTFLNA(1,'POLE   ',FILPOL,IRC)
      CALL OPNFIL(LFNLOC,FILPOL,'OLD',' ','READONLY',' ',IOSTAT)
      CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILPOL,'GETCPO')
C
C READ HEADER AND FIRST PARAMETER SET OF POLE FILE
      CALL RDPOLH(LFNLOC,1,DUMMYC,POLTYP,IFORM,IEND,NUTNAM,SUBNAM)
      CLOSE(UNIT=LFNLOC)
C
C DESTINCTION BETWEEN DIFFERENT APRIORI CPO-MODELS
C     POLTYP(1)=1  ... DEPS=DPSI=0.0
      IF (POLTYP(1).EQ.1) THEN
        DEPS(1)=0.0
        DEPS(2)=0.0
        DPSI(1)=0.0
        DPSI(2)=0.0
        T(1)=0.D0
        T(2)=1.D6
C
C     POLTYP(1)=2  ... DEPS,DPSI OUT OF POLE-FILE
      ELSE IF (POLTYP(1).EQ.2) THEN
        CALL GETPOL(TREQ,0,T,X,Y,UT1UTC,GPSUTC,DEPS,DPSI,POLTYP)
C
C     POLTYP(1)=3  ... DEPS,DPSI OUT OF HERRING MODEL
      ELSE IF (POLTYP(1).EQ.3) THEN
        T(1)=TREQ
        T(2)=TREQ+1.D0/24.D0
        DO 140 I=1,2
          CALL HERR87(T(I),DPSI(I),DEPS(I))
C
C RETURN VALUES IN PROPER UNITS (RADIANS)
          DEPS(I)=DEPS(I)/ars/1D03
          DPSI(I)=DPSI(I)/ars/1D03
140     CONTINUE
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
