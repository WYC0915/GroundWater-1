      MODULE s_RESTIN
      CONTAINS

C*
      SUBROUTINE RESTIN(ITYP,IREF,RFAK,NSTAT,STNUMB,ICENTR,XSTAT,
     1                  XSTREF,NFIX,STFIX,SIGAPR,LOCQ,NPAR,IHELM,
     2                  A,B,XFREE,ETPE,EREST,WREST,DBNOR,DOMEG)
CC
CC NAME       :  RESTIN
CC
CC PURPOSE    :  INITIALIZE RESTRICTION EQUATION FOR FREE SOLUTIONS
CC
CC PARAMETERS :
CC         IN :  ITYP   : 1: COORDINATES, 20: VELOCITIES      I*4
CC               IREF   : 0: SPECIAL REFERENCE                I*4
CC                           COORDINATES/VELOCITIES
CC                        1: TAKE APRIORI SETS AS REFERENCE
CC               RFAK   : 1 / -1: ADD / SUBSTRACT RESTRICTION R*8
CC               NSTAT  : NUMBER OF STATIONS                  I*4
CC               STNUMB(I),I=1,2,..,NSTAT  : STATION NUMBERS  I*4
CC               ICENTR(I),I=1,..,NSTAT: NUMBER OF THE CENTER I*4
CC                        STATION FOR STATION I
CC               XSTAT(K,I),K=1,2,3;I=1,..,NSTAT: RECTANGULAR R*8
CC                        STATION COORDINATES (APRIORI VALUES)
CC                        (ITYP:20 XSTAT=(0))
CC               XSTREF(K,I),K=1,2,3;I=1,..,NSTAT: RECT.      R*8
CC                        STATION COORDINATES (AS NEW REFERENCE)
CC                        (ITYP:20 XSTREF=REFERENCE VELOCITIES)
CC               NFIX   : NUMBER OF FIXED STATIONS            I*4
CC               STFIX(I),I=1,2,... STATION NUMBERS TO BE     I*4
CC                        KEPT FIXED
CC               SIGAPR : A PRIORI SIGMA                      R*8
CC               LOCQ(K,I),K=1,..,MAXLCQ, I=1,2,...,NPAR:     I*4
CC                        CHARACTERISTICS FOR EACH PARAMETER
CC               NPAR  : NUMBER OF PARAMETERS (WITHOUT PRE-   I*4
CC                        ELIMINATED PARAMETERS)
CC               IHELM(I),I=1,7: I=1,3: TRANSLATION,          I*4
CC                               I=4,6: ROTATION
CC                               I=7: SCALE (0:NO, 1:YES)
CC     IN/OUT :  A(I),I=1,2,..,NPAR*(NPAR+1)/2: INVERSE OF    R*8
CC                        NORMAL EQUATION MATRIX (UPPER
CC                        TRIANGLE ONLY, COLUMNWISE LINEAR.)
CC               B(I),I=1,2,..,NPAR: RIGHT HAND SIDE OF       R*8
CC                        NORMAL EQUATION
CC               XFREE(I): I=1,3 : FIX COORDINATES IN FREE    R*8
CC                        SOLUTION
CC               ETPE(K),K=1,NSTAT*(NSTAT-1)/2 COLUMNWISE
CC                        LINEARISED
CC                        PART OF THE NORMALEQATION WHICH
CC                        FIXES THE DATUM OF THE NETWORK FOR
CC                        TOTALLY FREE SOLUTIONS              R*8
CC               EREST(I,J),I=1,7:3 TRANSLATION, 3 ROTATIONS,
CC                        1 SCALE,
CC                        J=1,3*NSTAT: RESTRICTION MATRIX     R*8
CC               WREST(I),I=1,7:3 TRANSLATION, 3 ROTATIONS,
CC                        1 SCALE, RIGHT HAND RESTRICTION
CC               DBNOR(I),I=1,2,..,: ADDITION                 R*8
CC                        OF WEIGHTS TO RIGHT SIDE OF
CC                        NORMAL EQUATION MATRIX
CC                        DUE TO INPUT OPTIONS
CC               DOMEG : CORRECTION OF RMS DUE TO SPECIAL     R*8
CC                       RESTRICTIONS WITH W.NE.0.D0
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  E. BROCKMANN
CC
CC VERSION    :  3.4  (JAN 94)
CC
CC CREATED    :  5-FEB-93
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC               28-FEB-95 : EB: WARNING OUTPUT MODIFIED
CC               18-SEP-95 : JJ: INCREASE MAXSTA TO 200
CC               15-FEB-96 : EB: STFIX DEPENDING OF RFAK.EQ.1 OR.-1
CC               06-JUN-96 : MR: REMOVED UNUSED VARIABLES
CC               28-JUN-04 : RD: USE MAXSTA FROM P_ADDNEQ
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               22-SEP-05 : RD: USE NEW MODULE D_NEQ.F90
CC               14-FEB-11 : RD: REMOVE MAXSTA-COMMON (UNUSED)
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1993     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE d_neq,    ONLY: maxSta => maxStaSin
C
      USE f_ikf
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I      , ICOMP1 , ICOMP2 , IFIX   , IFREE  , IJ     ,
     1          IND    , IPAR   , IRC    , IREF   , IST    ,
     2          ISTAT  , ITYP   , J      , K      , KL     , MXCLCQ ,
     3          NFIX   , NPAR   , NSTAT
C
      REAL*8    DOMEG  , P1     , P2     , RFAK   , SIGAPR , WEIGHT1,
     1          WEIGHT2, WTPW
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C
      REAL*8        XSTAT(3,*),EREST(7,*),XFREE(*),XSTREF(3,*)
      REAL*8        ETPE(*),A(*),B(*),DBNOR(*),WREST(*)
      CHARACTER*6   MXNLCQ
      INTEGER*4     LOCQ(MXCLCQ,*)
      INTEGER*4     ICENTR(*),STFIX(*),STNUMB(*),IHELM(*)
C LOKAL
      REAL*8        ETPW(3*MAXSTA)
      INTEGER*4     INDLOC(3*MAXSTA)

C
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
C
C WEIGHT FOR RESTRICTION EQUATION
C -------------------------------
C       DATA P1/1.D-5/,P2/1.D-7/
C OLD D-3, D-3
       DATA P1/1.D-4/,P2/1.D-4/
       WEIGHT1=(SIGAPR/P1)**2
       WEIGHT2=(SIGAPR/P2/6370000.D0)**2
C
C CHECK ITYP VALUE
C ----------------
       IF (.NOT.(ITYP.EQ.1.OR.ITYP.EQ.20)) THEN
         WRITE(LFNERR,901) ITYP
901      FORMAT(/,' *** SR RESTIN: WRONG PARAMETER TYPE:',I4,/,
     1        16X,'RESTIN ONLY DEFINED FOR PARAMETER 1 AND 20',/)
         CALL EXITRC(2)
       ENDIF
C
C INITIALISE RESTRICTION MATRIX
C -----------------------------
      DO 30 I=1,7
        WREST(I)=0.D0
        DO 40 J=1,3*NSTAT
          INDLOC(J)=0.D0
          EREST(I,J)=0.D0
40      CONTINUE
30    CONTINUE
      DO 35 I=1,3
        XFREE(I)=0.D0
35    CONTINUE
C
C COMPUTE RESTRICTION MATRIX
C --------------------------
      DO 60 ISTAT=1,NSTAT
        IF(ICENTR(ISTAT).NE.ISTAT) GOTO 60
        DO 55 IPAR=1,NPAR
          IF (LOCQ(1,IPAR).NE.ITYP.OR.LOCQ(3,IPAR).NE.1.OR.
     1        LOCQ(2,IPAR).NE.ISTAT) GOTO 55
          DO 50 IFIX=1,NFIX
            ICOMP1=STNUMB(ISTAT)
            ICOMP2=-99
            IF (RFAK.EQ.-1) ICOMP1=ISTAT
            IF (RFAK.EQ.-1) ICOMP2=ISTAT
            IF (STFIX(IFIX).EQ.ICOMP1.OR.STFIX(IFIX).EQ.ICOMP2)THEN
C
              IST=(ISTAT-1)*3+1
C
              DO 65 IFREE=1,3
                INDLOC(IST+IFREE-1)=IPAR+IFREE-1
                IF (IREF.EQ.1) THEN
                  XFREE(IFREE)=XFREE(IFREE)+XSTAT(IFREE,ISTAT)
                ELSE
                  XFREE(IFREE)=XFREE(IFREE)+XSTREF(IFREE,ISTAT)
                  IF (XSTREF(1,ISTAT).EQ.0.D0.AND.XSTREF(2,ISTAT)
     1               .EQ.0.D0 .AND. XSTREF(3,IST).EQ.0.D0.AND.
     2               ITYP.EQ.1) THEN
                     WRITE(LFNERR,902)STNUMB(ISTAT)
902                  FORMAT(/,' *** SR RESTIN: WARNING: ',
     1                        'FIXSTATION NOT FOUND',/,
     2                    16X,'IN SPECIAL COORDINATE FILE. ',/,
     3                    16X,'STATION NUMBER ',I4,/)
                  ENDIF
                ENDIF
65            CONTINUE
C
C UNIT MATRIX FOR TRANSLATION
              DO 70 I=1,3
                IF (IHELM(I).EQ.1) EREST(I,IST-1+I)=1.D0
C
                IF (IHELM(I).EQ.1.AND.IREF.EQ.0) THEN
                  WREST(I)=WREST(I)+XSTREF(I,ISTAT)-XSTAT(I,ISTAT)
                ENDIF
70            CONTINUE
C
C ROTATION MATRIX FOR ROTATION
              IF (IREF.EQ.1) THEN
                IF (IHELM(4).EQ.1) THEN
                  EREST(4,IST-1+2)=-XSTAT(3,ISTAT)
                  EREST(4,IST-1+3)= XSTAT(2,ISTAT)
                ENDIF
                IF (IHELM(5).EQ.1) THEN
                  EREST(5,IST-1+1)= XSTAT(3,ISTAT)
                  EREST(5,IST-1+3)=-XSTAT(1,ISTAT)
                ENDIF
                IF (IHELM(6).EQ.1) THEN
                  EREST(6,IST-1+1)=-XSTAT(2,ISTAT)
                  EREST(6,IST-1+2)= XSTAT(1,ISTAT)
                ENDIF
              ELSE
                IF (IHELM(4).EQ.1) THEN
                  EREST(4,IST-1+2)=-XSTREF(3,ISTAT)
                  EREST(4,IST-1+3)= XSTREF(2,ISTAT)
                  WREST(4)=WREST(4)
     1             +(XSTREF(2,ISTAT)-XSTAT(2,ISTAT))*(-XSTREF(3,ISTAT))
     2             +(XSTREF(3,ISTAT)-XSTAT(3,ISTAT))*( XSTREF(2,ISTAT))
                ENDIF
                IF (IHELM(5).EQ.1) THEN
                  EREST(5,IST-1+1)= XSTREF(3,ISTAT)
                  EREST(5,IST-1+3)=-XSTREF(1,ISTAT)
                  WREST(5)=WREST(5)
     1             +(XSTREF(1,ISTAT)-XSTAT(1,ISTAT))*( XSTREF(3,ISTAT))
     2             +(XSTREF(3,ISTAT)-XSTAT(3,ISTAT))*(-XSTREF(1,ISTAT))
                ENDIF
                IF (IHELM(6).EQ.1) THEN
                  EREST(6,IST-1+1)=-XSTREF(2,ISTAT)
                  EREST(6,IST-1+2)= XSTREF(1,ISTAT)
                  WREST(6)=WREST(6)
     1             +(XSTREF(1,ISTAT)-XSTAT(1,ISTAT))*(-XSTREF(2,ISTAT))
     2             +(XSTREF(2,ISTAT)-XSTAT(2,ISTAT))*( XSTREF(1,ISTAT))
                ENDIF
              ENDIF
C
C SCALE
              IF (IREF.EQ.1) THEN
                IF (IHELM(7).EQ.1) THEN
                  EREST(7,IST-1+1)= XSTAT(1,ISTAT)
                  EREST(7,IST-1+2)= XSTAT(2,ISTAT)
                  EREST(7,IST-1+3)= XSTAT(3,ISTAT)
                ENDIF
              ELSE
                IF (IHELM(7).EQ.1) THEN
                  EREST(7,IST-1+1)= XSTREF(1,ISTAT)
                  EREST(7,IST-1+2)= XSTREF(2,ISTAT)
                  EREST(7,IST-1+3)= XSTREF(3,ISTAT)
                  WREST(7)=WREST(7)
     1             +(XSTREF(1,ISTAT)-XSTAT(1,ISTAT))*(XSTREF(1,ISTAT))
     2             +(XSTREF(2,ISTAT)-XSTAT(2,ISTAT))*(XSTREF(2,ISTAT))
     3             +(XSTREF(3,ISTAT)-XSTAT(3,ISTAT))*(XSTREF(3,ISTAT))
                ENDIF
              ENDIF
C
            ENDIF
50        CONTINUE
55      CONTINUE
60    CONTINUE
C
      DO 75 IFREE=1,3
        XFREE(IFREE)=XFREE(IFREE)/(NFIX*1.D0)
75    CONTINUE
C
C COMPUTE ET P E (COLUMNWISE LINEARISED) AND
C INTRODUCE A-PRIORI WEIGHTS FOR STATION COORDINATES DUE
C TO DATUM DEFINITION FOR TOTALLY FREE SOLUTIONS
C ------------------------------------------------------
      WTPW=0.D0
C ADD WTPW TO DOMEG
      DO 95 K=1,7
        IF (K.LE.3) THEN
          WTPW=WTPW+WREST(K)*WEIGHT1*WREST(K)
        ELSE
          WTPW=WTPW+WREST(K)*WEIGHT2*WREST(K)
        ENDIF
95    CONTINUE
      DOMEG=DOMEG+WTPW
C
      DO 80 I=1,3*NSTAT
C ADD ETPW TO B
        ETPW(I)=0.D0
        DO 85 K=1,7
          IF (K.LE.3) THEN
            ETPW(I)=ETPW(I)+EREST(K,I)*WEIGHT1*WREST(K)
          ELSE
            ETPW(I)=ETPW(I)+EREST(K,I)*WEIGHT2*WREST(K)
          ENDIF
85      CONTINUE
        IND=INDLOC(I)
        B(IND)=B(IND)+RFAK*ETPW(I)
        DBNOR(IND)=DBNOR(IND)+RFAK*ETPW(I)
C
C ADD ETPE TO A
        DO 90 J=1,I
          IJ=IKF(I,J)
          KL=IKF(INDLOC(I),INDLOC(J))
          ETPE(IJ)=0.D0
          DO 100 K=1,7
            IF (K.LE.3) THEN
              ETPE(IJ)=ETPE(IJ)+
     1                 EREST(K,I)*WEIGHT1*EREST(K,J)
            ELSE
              ETPE(IJ)=ETPE(IJ)+
     1                 EREST(K,I)*WEIGHT2*EREST(K,J)
            ENDIF
100       CONTINUE
          A(KL)=A(KL)+RFAK*ETPE(IJ)
90      CONTINUE
80    CONTINUE
C
      END SUBROUTINE

      END MODULE
