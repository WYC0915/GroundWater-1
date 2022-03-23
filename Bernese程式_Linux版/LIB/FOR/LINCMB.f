      MODULE s_LINCMB
      CONTAINS

C*
      SUBROUTINE LINCMB(MAXREC,MAXTYP,MAXSAT,NEPOCH,OBSTIM,OBSREC,
     1                  ISAT  ,SVN   ,ITYP  ,NOBS  ,LINTIM,LINOBS,
     2                  LINFLG)
CC
CC NAME       :  LINCMB
CC
CC PURPOSE    :  CREATE LINEAR COMBINATION OBSERVATION TYPE BASED ON
CC               ITYP INPUT. LINEAR COMBINATION WILL THEN USED IN RNXSMT
CC               THE LINEAR COMBINATIONS ARE RETURNED IN METERS EXCEPT FOR
CC               THE MELWUB COMBINATION WHICH IS RETURNED IN L5 CYCLES.
CC
CC PARAMETERS :
CC        IN  :  MAXREC : MAXIMUM NUMBER OF RECORDS           I*4
CC               MAXTYP : MAXIMUM NUMBER OF OBS. TYPES        I*4
CC               MAXSAT : MAXIMUM NUMBER OF SATELLITES        I*4
CC               MAXARC : MAXIMUM NUMBER OF ARC PER SATELLITE I*4
CC               NEPOCH : NUMBER OF POINTS IN X AND Y ARRAY   I*4
CC               OBSTIM : ARRAY WITH TIME VALUES (MJD)        R*8(*)
CC               OBSREC : ARRAY WITH OBSERVATIONS             R*8(*,*,*)
CC               ISAT   : REQUESTED SATELLITE                 I*4
CC               SVN    : NUMBER OF REQUESTED SATELLITE       I*4
CC               ITYP   :  1 : L1                             I*4
CC                         2 : L2
CC                         3 : L3
CC                         4 : L4
CC                         5 :    P4 (was L5)
CC                         6 : L1-P1
CC                         7 : L2-P2
CC                         8 : L3-P3
CC                         9 : L4+P4 (ONLY NOISE!)
CC                        10 : MELWUB
CC        OUT :  NOBS   : ACTUAL NUMBER OF POINTS FOR SAT.    I*4
CC               LINTIM : OBSERVATION TIME (HOURS)            R*8(NOBS)
CC               LINOBS : REQUESTED LINEAR OBSERVATIONS       R*8(NOBS)
CC               LINFLG : OBSERVATION FLAG                   CH*1(NOBS)
CC                        BIT=0: OUTLIER
CC                        BIT=1: NEW AMBIGUITY
CC
CC REMARKS    :  FACTORS VALID IF INPUT OBSERVATIONS IN METERS (NOT CYCLES)
CC
CC AUTHOR     :  T.A. SPRINGER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  22-JUL-96
CC
CC CHANGES    :  31-AUG-98 : DI: USE 'COMFREQ' (FOR GLONASS SATELLITES)
CC               16-JUN-05 : MM: UNUSED COMCONST.inc REMOVED
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1996     UNIVERSITY OF BERN
CC               SWITZERLAND
C*
      USE m_bern
      USE f_tstflg
      USE s_clrflg
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , ISAT  , ITYP  , MAXREC, MAXSAT, MAXTYP, NEPOCH,
     1          NOBS
C
      REAL*8    FACT1 , FACT2 , FACT3 , FACT4
C
CCC       IMPLICIT  REAL*8(A-H,O-Z)
C
      REAL*8      OBSTIM(MAXREC),OBSREC(MAXREC,MAXTYP,MAXSAT)
      REAL*8      LINTIM(*),LINOBS(*)
C
      INTEGER*4   SVN
C
      CHARACTER*1 LINFLG(*)
C
      INCLUDE   'COMFREQ.inc'
C
C CHECK ITYP
C ----------
      IF (ITYP.GT.10) THEN
        WRITE(LFNERR,901)ITYP
901     FORMAT(/,' *** SR LINCMB: UNKNOWN COMBINATION TYPE',/,
     1                      16X,' GIVEN ITYP: ',I4,/)
        CALL EXITRC(2)
      ENDIF
C
C OBSERVATIONS L1
C ---------------
      IF (ITYP.EQ.1) THEN
        FACT1= 1.D0
        FACT2= 0.D0
        FACT3= 0.D0
        FACT4= 0.D0
      ENDIF
C
C OBSERVATIONS L2
C ---------------
      IF (ITYP.EQ.2) THEN
        FACT1= 0.D0
        FACT2= 1.D0
        FACT3= 0.D0
        FACT4= 0.D0
      ENDIF
C
C OBSERVATIONS L3
C ---------------
      IF (ITYP.EQ.3) THEN
        FACT1= FACLIN(3,1,SVN)
        FACT2= FACLIN(3,2,SVN)
        FACT3= 0.D0
        FACT4= 0.D0
      ENDIF
C
C OBSERVATIONS L4
C ---------------
      IF (ITYP.EQ.4) THEN
        FACT1= 1.D0
        FACT2=-1.D0
        FACT3= 0.D0
        FACT4=-0.D0
      ENDIF
C
C OBSERVATIONS P4
C ---------------
      IF (ITYP.EQ.5) THEN
cc        FACT1= FRQ(1,SVN)/(FRQ(1,SVN)-FRQ(2,SVN))
cc        FACT2=-FRQ(2,SVN)/(FRQ(1,SVN)-FRQ(2,SVN))
        FACT1= 0.D0
        FACT2=-0.D0
        FACT3= 1.D0
        FACT4=-1.D0
      ENDIF
C
C OBSERVATIONS L1-P1
C ----------------------
      IF (ITYP.EQ.6) THEN
        FACT1= 1.D0
        FACT2= 0.D0
        FACT3=-1.D0
        FACT4= 0.D0
      ENDIF
C
C OBSERVATIONS L2-P2
C ----------------------
      IF (ITYP.EQ.7) THEN
        FACT1= 0.D0
        FACT2= 1.D0
        FACT3= 0.D0
        FACT4=-1.D0
      ENDIF
C
C OBSERVATIONS L3-P3
C ------------------
      IF (ITYP.EQ.8) THEN
        FACT1= FACLIN(3,1,SVN)
        FACT2= FACLIN(3,2,SVN)
        FACT3=-FACLIN(3,1,SVN)
        FACT4=-FACLIN(3,2,SVN)
      ENDIF
C
C OBSERVATIONS L4+P4
C ------------------
      IF (ITYP.EQ.9) THEN
        FACT1= 1.D0
        FACT2=-1.D0
        FACT3= 1.D0
        FACT4=-1.D0
      ENDIF
C
C OBSERVATIONS MELWUB
C -------------------
      IF (ITYP.EQ.10) THEN
        FACT1= FACLIN(5,1,SVN)
        FACT2= FACLIN(5,2,SVN)
        FACT3=-FRQ(1,SVN)/(FRQ(1,SVN)+FRQ(2,SVN))
        FACT4=-FRQ(2,SVN)/(FRQ(1,SVN)+FRQ(2,SVN))
      ENDIF
C
C CREATE THE REQUESTED OBSERVATION TYPE
C -------------------------------------
      NOBS=0
      DO 10 I=1,NEPOCH
        IF ((OBSREC(I,1,ISAT).NE.0.D0).AND.
     1      (OBSREC(I,2,ISAT).NE.0.D0).AND.
     2      (OBSREC(I,3,ISAT).NE.0.D0).AND.
     3      (OBSREC(I,4,ISAT).NE.0.D0)) THEN
          NOBS=NOBS+1
          LINTIM(NOBS)= (OBSTIM(I)-DNINT(OBSTIM(1)))*24D0
          LINOBS(NOBS)= FACT1*OBSREC(I,1,ISAT)
     1                 +FACT2*OBSREC(I,2,ISAT)
     2                 +FACT3*OBSREC(I,3,ISAT)
     3                 +FACT4*OBSREC(I,4,ISAT)
C
C INITIALIZE ARCS AND FLAGS ON FIRST CALL (MELWUB)
C ------------------------------------------------
          IF (ITYP.EQ.10) THEN
            LINOBS(NOBS)=LINOBS(NOBS)/WLGT(5,SVN)
            CALL CLRFLG(LINFLG(NOBS),0)
            CALL CLRFLG(LINFLG(NOBS),1)
            CALL CLRFLG(LINFLG(NOBS),2)
          ENDIF
C
C SET OBSERVATION TO ZERO IF FLAGGED BAD
C --------------------------------------
          IF (TSTFLG(LINFLG(NOBS),0)) THEN
            LINOBS(NOBS)=0D0
          ENDIF
        ENDIF
10    CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
