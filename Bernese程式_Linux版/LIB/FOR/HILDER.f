      MODULE s_HILDER
      CONTAINS

C*
      SUBROUTINE HILDER(LOCQ,SVN,IARC,TOSC,ELE,TMJD,XNORM,IORSYS,DRDP)
CC
CC NAME       :  HILDER
CC
CC PURPOSE    :  COMPUTE PARTIAL DERIVATIVE OF AN ORBIT (CHARACTERIZED
CC               BY THE ELEMENET ARRAY "ELE" WITH RESPECT TO A PARAMETER
CC               CHARACTERIZED BY THE ARRAY LOCQ
CC               THE FOLLOWING PARAMETER TYPES MAY BE ACCOMODATED :
CC               ITYP=13 : POTENTIAL PARAMETERS
CC               ITYP=14 : RESONANCE TERMS ACCORDING TO HILL'S THEORY
CC               ITYP=15 : ALBEDO FORCE
CC               ITYP=99 : SOLUTION OF HOMOGENEOUS EQNS (TEST PURPOSES)
CC
CC PARAMETERS :
CC         IN :  LOCQ   : PARAMETER CHARACTERIZATION          I*4(*,*)
CC               SVN    : SATELLITE NUMBER                    I*4
CC               IARC   : ARC NUMBER                          I*4
CC               TOSC   : OSCULATION EPOCH (MJD)              R*8
CC               ELE    : OSCULATING ELEMENTS                 R*8(*)
CC               TMJD   : TIME OF REQUEST (MJD)               R*8
CC               XNORM  : NORMALIZATION FACTOR FOR DERIVS     R*8
CC               IORSYS : ORBIT SYSTEM                        I*4
CC                        =1 : B1950.0
CC                        =2 : J2000.0
CC       OUT  :  DRDP   : PARTAL DERIVATIVE OF ORBIT WITH     R*8(*)
CC                        RESPECT TO PARAMETER
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.3
CC
CC CREATED    :  93/04/18
CC
CC CHANGES    :  10-AUG-94 : MR: CALL EXITRC
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE d_const, ONLY: GM, PI
      USE s_rhshil
      USE s_subdiv
      USE s_ypol
      USE s_exitrc
      USE s_ddreh
      USE s_dmlmav
      USE s_intmat
      USE s_maxtst
      USE s_updcoe
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IARC  , IFIRST, IFMAT , ILOC  , IORSYS, IRC1  ,
     1          IREV  , ISYS  , K     , MAXLCQ, MAXPTS, MAXSYS, MXCLCQ,
     2          NEWSYS, NPTS  , NREV  , NSYS
C
      REAL*8    A     , A1    , ARG   , C0    , E     , EE    , FE    ,
     1          HEFF  , TMJD  , TOLMIN, TOSC  , TPER  , TSEC  , U     ,
     2          UDAYS , ULAT  , XN    , XNORM , XNREV
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
      PARAMETER (MAXSYS=250,MAXLCQ=7,MAXPTS=16)
      INTEGER*4    SVN,LOCQ(*)
      REAL*8       ELE(*),DRDP(*)
      CHARACTER*6  MXNLCQ
C
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
      INTEGER*4 LOCHIL(MAXLCQ,MAXSYS),ARCSYS(MAXSYS)
      INTEGER*4 Q,FRCTYP
      REAL*8    TIMREL(MAXPTS),MAT(MAXPTS,MAXPTS),HT0(2,MAXSYS)
      REAL*8    RSW(3,MAXPTS),RHS(6,MAXPTS),FACT(MAXPTS+1)
      REAL*8    YCOE((MAXPTS+1)*6,MAXSYS),YSTART(6,MAXSYS),Y(6)
      REAL*8    DRMAT(3,3)
C
      DATA IFIRST/1/
C
C CHECK MAXIMUM DIMENSIONS UPON FIRST CALL, INITIALIZE
C ----------------------------------------------------
      IF(IFIRST.EQ.1) THEN
        CALL MAXTST(0,'HILDER',MXNLCQ,MAXLCQ,MXCLCQ,IRC1)
        IF(IRC1.NE.0) CALL EXITRC(2)
        DO 10 ISYS=1,MAXSYS
          ARCSYS(ISYS)=-1
10      CONTINUE
        NSYS=0
        IFMAT=1
        IFIRST=0
      END IF
C
C NEW SYSTEM OF PARTIALS TO BE INTEGRATED ?
C ---------------------------------------
      NEWSYS=0
      DO 40 ISYS=1,NSYS
        IF(LOCQ(1).EQ.LOCHIL(1,ISYS))THEN
          IF(LOCQ(3).EQ.LOCHIL(3,ISYS).OR.
     1       (LOCQ(1).EQ.99.AND.SVN.EQ.LOCHIL(3,ISYS)).OR.
     2       ((LOCQ(1).EQ.13.OR.LOCQ(1).EQ.15).
     3       AND.SVN.EQ.LOCHIL(3,ISYS)))THEN
            IF(LOCQ(4).EQ.LOCHIL(4,ISYS).AND.
     1         LOCQ(5).EQ.LOCHIL(5,ISYS).AND.
     2         LOCQ(6).EQ.LOCHIL(6,ISYS).AND.
     3         LOCQ(7).EQ.LOCHIL(7,ISYS))THEN
              IF(IARC.NE.LOCHIL(2,ISYS))THEN
                NEWSYS=1
                LOCHIL(2,ISYS)=IARC
                GO TO 45
              ELSE
                GO TO 60
              END IF
            END IF
          END IF
        END IF
40    CONTINUE
45    CONTINUE
C
C NEW SET OF VARIATIONAL EQNS HAS TO BE SET UP :
C --------------------------------------------
      NSYS=NSYS+1
      IF(NSYS.GT.MAXSYS)THEN
        WRITE(LFNERR,46)MAXSYS
46      FORMAT(/,' *** SR HILDER : MAX. NUMBER OF VAR EQNS (',I3,
     1            ' EXCEEDED)',/)
        CALL EXITRC(2)
      END IF
      ISYS=NSYS
      DO 50 ILOC=1,MAXLCQ
        LOCHIL(ILOC,ISYS)=LOCQ(ILOC)
50    CONTINUE
      LOCHIL(2,ISYS)=IARC
      LOCHIL(3,ISYS)=SVN
      NEWSYS=1
C
60    CONTINUE
C
C REVOLUTION PERIOD OF CIRCULAR REFERENCE ORBIT :
C ---------------------------------------------
      A =ELE(1)
      XN=DSQRT(GM/A**3)
      U =2*PI/XN
C
C PARAMETERS DEFINING THE HOMOGENEOUS SOLUTION OF HILL'S EQNS
C -----------------------------------------------------------
C THEORY : G. BEUTLER (1993). "HILL'S EQUATIONS OF MOTION AND THEIR
C                              USE TO COMPUTE THE PARTIAL DERIVATIVES
C                              OF THE ORBIT WITH RESPECT TO DYNAMICAL
C                              PARAMETERS."
C (1) OUT OF PLANE COMPONENT : CHARACTERIZED BY PARAMETERS A AND B
C                              (EQN. (16)). WE USE THE ELLIPTICAL
C                              MOTION, THEREFORE BOTH PARAMETERS ARE =0
C                              (HENCE THEY NEED NOT BE STORED).
C (2) MOTION IN ORBITAL PLANE: CHARACTERIZED BY PARAMETERS C0,C1,A1,B1
C                              IN THE ELLIPTICAL MOTION C1 AND B1 ARE =0.
C                              HENCE THEY NEED NOT BE STORED.
C ==> THE HOMOGENEOUS SOLUTION IS CHARACTERIZED BY THE TWO ADDITIONAL
C                              ELEMENTS C0 AN A1.
C
C NEW INTEGRATION ?
C ---------------
      FRCTYP=LOCQ(1)
      IF(NEWSYS.EQ.1.AND.FRCTYP.NE.99)THEN
C
C PARTITION OF THE INTEGRATION INTERVAL (TOSC,TOSC+U) :
        IF(NPTS.GT.MAXPTS)THEN
          WRITE(LFNERR,65)NPTS
65        FORMAT(/,' *** SR HILDER : NUMBER OF POINTS FOR INT ',
     1             'EXCEEDED :',I3,/)
          CALL EXITRC(2)
        END IF
C
C COEFFICIENT MATRIX (FOR FIRST CALL) :
        IF(IFMAT.EQ.1)THEN
          IFMAT=0
          Q=10
          NPTS=Q
          CALL SUBDIV(3,NPTS,TIMREL)
          DO 68 I=1,NPTS
            TIMREL(I)=TIMREL(I)+1
68        CONTINUE
          CALL INTMAT(NPTS,TIMREL,MAT)
          FACT(1)=1.D0
          DO 66 K=2,MAXPTS+1
            FACT(K)=FACT(K-1)*(K-1)
66        CONTINUE
        END IF
        HT0(1,ISYS)=U
        HT0(2,ISYS)=TOSC
        DO 67 K=1,6
          YSTART(K,ISYS)=0.D0
67      CONTINUE
C
C RIGHT HAND SIDES OF HILL'S EQUATIONS OF MOTION
C ----------------------------------------------
        CALL RHSHIL(FRCTYP,HT0(1,ISYS),HT0(2,ISYS),TOSC,LOCHIL(1,ISYS),
     1              IORSYS,ELE,XN,NPTS,TIMREL,XNORM,RSW,RHS)
C
C INTEGRATION
C -----------
        HEFF=HT0(1,ISYS)/2
        CALL UPDCOE(1,Q,6,HEFF,FACT,MAT,RHS,YSTART(1,ISYS),YCOE(1,ISYS))
      END IF
C
C NORMAL REQUEST :
C --------------
      TOLMIN=0.d0
      UDAYS = U/86400.D0
      TPER=ELE(7)
      HEFF=HT0(1,ISYS)/2
      NREV=(TMJD-HT0(2,ISYS))/UDAYS
C
C TIME LT TOSC ? (=NOT ALLOWED)
      IF(FRCTYP.NE.99)THEN
        IF(TMJD.LT.TOSC-TOLMIN)THEN
          WRITE(LFNERR,70)
70        FORMAT(/,' *** SR HILDER : TIME LT TOSC!',/)
          CALL EXITRC(2)
        END IF
C
C REQUEST IN PREVIOUS REVOLUTION ?
        XNREV=(TMJD-HT0(2,ISYS))/UDAYS
        IF(XNREV.LT.0.d0)THEN
          HT0(2,ISYS)=TOSC
          DO 80 K=1,6
            YSTART(K,ISYS)=0.D0
80        CONTINUE
          CALL RHSHIL(FRCTYP,HT0(1,ISYS),HT0(2,ISYS),TOSC,
     1                LOCHIL(1,ISYS),IORSYS,ELE,XN,NPTS,TIMREL,
     2                XNORM,RSW,RHS)
          CALL UPDCOE(1,Q,6,HEFF,FACT,MAT,RHS,YSTART(1,ISYS),
     1                YCOE(1,ISYS))
          NREV=(TMJD-HT0(2,ISYS))/UDAYS
        END IF
        DO 200 IREV=1,NREV
          TSEC=U
          CALL YPOL(0,Q,6,HEFF,FACT,TSEC,YCOE(1,ISYS),YSTART(1,ISYS))
          HT0(2,ISYS)=HT0(2,ISYS)+UDAYS
          CALL RHSHIL(FRCTYP,HT0(1,ISYS),HT0(2,ISYS),TOSC,
     1                LOCHIL(1,ISYS),IORSYS,ELE,XN,NPTS,TIMREL,
     2                XNORM,RSW,RHS)
          CALL UPDCOE(1,Q,6,HEFF,FACT,MAT,RHS,YSTART(1,ISYS),
     1                YCOE(1,ISYS))
200     CONTINUE
        TSEC=(TMJD-HT0(2,ISYS))*86400.D0
        CALL YPOL(0,Q,6,HEFF,FACT,TSEC,YCOE(1,ISYS),Y)
C
C THE SOLUTION OF THE DEQ - SYSTEM (AT TIME TMJD)
C
C DRDP(1) = C0 + A1*COS(NT)+B1*SIN(NT)
C DRDP(2) = -1.5*C0*N*T + C1 -2*A1*SIN(NT)+2*B1*COS(NT)
C DRDP(3) = A*COS(NT)+B*SIN(NT)
C
C WHERE T:=T-TPER, AND :
C
C A1' = -(R*SIN(N*T)+2*S*COS(NT)) , B1' = (R*COS(NT)-2*S*SIN(NT))
C C0' = 2*S/N                     , C1' = 3*T*S - 2*R/N
C A'  = -W*SIN(NT)/N              , B'  = W*COS(NT)/N
C
        ARG=XN*((TMJD-TOSC)*86400.D0-0*TPER)
        DRDP(1)=Y(3) + Y(1)*DCOS(ARG) + Y(2)*DSIN(ARG)
        DRDP(2)=-1.5D0*ARG*Y(3)+ Y(4)  -2*Y(1)*DSIN(ARG)
     1                                 +2*Y(2)*DCOS(ARG)
        DRDP(3)=Y(5)*DCOS(ARG)+Y(6)*DSIN(ARG)
      ELSE
        ARG=XN*((TMJD-TOSC)*86400.D0-0*TPER)
        E=ELE(2)
        EE=DSQRT(1.D0-E**2)
        FE=(1+E-EE)/EE
        C0=2*A*(FE-E)
        A1=A*(E-2*FE)
        DRDP(1)=C0 + A1*DCOS(ARG)
        DRDP(2)=-3*C0*ARG-2*A1*DSIN(ARG)
        DRDP(3)=0.D0
      END IF
C
C BACK TRAFO INTO ORBITAL SYSTEM :
C ------------------------------
C
C ARGUMENT OF LATITUDE
      ULAT=ARG+ELE(5)
      ULAT=ARG+ELE(6)
      CALL DDREH(3,-ULAT,DRMAT)
      CALL DMLMAV(DRDP,DRMAT,DRDP)
      CALL DDREH(1,-ELE(3),DRMAT)
      CALL DMLMAV(DRDP,DRMAT,DRDP)
      CALL DDREH(3,-ELE(4),DRMAT)
      CALL DMLMAV(DRDP,DRMAT,DRDP)

      RETURN
      END SUBROUTINE

      END MODULE
