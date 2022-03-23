      MODULE s_KEPDER
      CONTAINS

C*
      SUBROUTINE KEPDER(LOCQ,SVN,IARC,TOSC,ELE,TMJD,XNORM,IORSYS,DRDP)
CC
CC NAME       :  KEPDER
CC
CC PURPOSE    :  COMPUTE PARTIAL DERIVATIVE OF AN ORBIT (CHARACTERIZED
CC               BY THE ELEMENET ARRAY "ELE" WITH RESPECT TO A PARAMETER
CC               CHARACTERIZED BY THE ARRAY LOCQ.
CC               The correct Two-Body-Approximation is used
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
CC               03-DEC-10 : HB: ADD PARAMETER FOR SR PRTDER
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE d_const, ONLY: GM, PI
      USE s_rhskep
      USE s_subdiv
      USE s_ypol
      USE s_exitrc
      USE s_ddreh
      USE s_dmlmav
      USE s_intmat
      USE s_maxtst
      USE s_updcoe
      USE s_rvprtp
      USE s_prtder
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IARC  , IFIRST, IFMAT , ILOC  , IORSYS, IRC1  ,
     1          IREV  , ISYS  , K     , MAXLCQ, MAXPTS, MAXSYS, MXCLCQ,
     2          NEWSYS, NPTS  , NREV  , NSYS
C
      REAL*8    A     , A1    , ARG   , C0    , E     , EE    , FE    ,
     1          HEFF  , TMJD  , TOSC  , TPER  , TSEC  , U     ,
     2          UDAYS , ULAT  , XN    , XNORM , XNREV
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
      PARAMETER (MAXSYS=250,MAXLCQ=7,MAXPTS=16)
      INTEGER*4    SVN,LOCQ(*)
      REAL*8       ELE(*),DRDP(*)
      CHARACTER*6  MXNLCQ
      CHARACTER(LEN=fileNameLength) :: FILxx
C
      COMMON/MCMLCQ/MXCLCQ,MXNLCQ
      INTEGER*4 LOCHIL(MAXLCQ,MAXSYS),ARCSYS(MAXSYS)
      INTEGER*4 Q,FRCTYP, ll
      REAL*8    TIMREL(MAXPTS),MAT(MAXPTS,MAXPTS),HT0(2,MAXSYS)
      REAL*8    RSW(3,MAXPTS),RHS(6,MAXPTS),FACT(MAXPTS+1)
      REAL*8    YCOE((MAXPTS+1)*6,MAXSYS),YSTART(6,MAXSYS),Y(6)
      REAL*8    DRMAT(3,3)
      REAL(r8b) DRDELE(3,6), DVDELE(3,6)
      REAL(r8b) TIMMJD, XVSAT(6), RPRPAR(50), ELExx(7)
      CHARACTER*8 ANLTYP
      INTEGER(i4b) :: ICRARCxx,IORSYSxx,NVARxx,NRADxx,IRCxx
C
      DATA IFIRST/1/
C
C CHECK MAXIMUM DIMENSIONS UPON FIRST CALL, INITIALIZE
C ----------------------------------------------------
      IF(IFIRST.EQ.1) THEN
        CALL MAXTST(0,'KEPDER',MXNLCQ,MAXLCQ,MXCLCQ,IRC1)
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
46      FORMAT(/,' *** SR KEPDER : MAX. NUMBER OF VAR EQNS (',I3,
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
      U =2*PI/XN/10
C
C PARAMETERS DEFINING THE HOMOGENEOUS SOLUTION OF HILL'S EQNS
C -----------------------------------------------------------
C THEORY : G. BEUTLER (2005). Methods of Celestial mechanics

C
C NEW INTEGRATION ?
C ---------------
      FRCTYP=LOCQ(1)
      IF(NEWSYS.EQ.1.AND.FRCTYP.NE.99)THEN
C
C PARTITION OF THE INTEGRATION INTERVAL (TOSC,TOSC+U) :
        IF(NPTS.GT.MAXPTS)THEN
          WRITE(LFNERR,65)NPTS
65        FORMAT(/,' *** SR KEPDER : NUMBER OF POINTS FOR INT ',
     1             'EXCEEDED :',I3,/)
          CALL EXITRC(2)
        END IF
C
C COEFFICIENT MATRIX (FOR FIRST CALL) :
        IF(IFMAT.EQ.1)THEN
          IFMAT=0
          Q=MAXPTS
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
C RIGHT HAND SIDES OF EQUATIONS OF MOTION
C ---------------------------------------
        CALL RHSKEP(FRCTYP,HT0(1,ISYS),HT0(2,ISYS),TOSC,LOCHIL(1,ISYS),
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
      UDAYS = U/86400.D0
      TPER=ELE(7)
      HEFF=HT0(1,ISYS)/2
      NREV=(TMJD-HT0(2,ISYS))/UDAYS
C
C TIME LT TOSC ? (=NOT ALLOWED)
        IF(TMJD.LT.TOSC)THEN
          WRITE(LFNERR,70)
70        FORMAT(/,' *** SR KEPDER : TIME LT TOSC!',/)
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
          CALL RHSKEP(FRCTYP,HT0(1,ISYS),HT0(2,ISYS),TOSC,
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
          CALL RHSKEP(FRCTYP,HT0(1,ISYS),HT0(2,ISYS),TOSC,
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
C Get partials w.r.t. osc ele:
      TSEC=(TMJD-TOSC)*86400.D0


      CALL RVPRTP(LOCHIL(2,1),LOCHIL(3,1),GM,TSEC,0.d0,
     1              ELE(1),ELE(2),ELE(3),ELE(4),ELE(5),ELE(7),
     1              DRDELE,DVDELE)

        do ll=1,6
          Call PRTDER(FILxx,lochil(3,1),ll,0,0,TMJD,1,ICRARCxx,
     1                IORSYSxx,NVARxx,NRADxx,xvsat,ELExx,RPRPAR,ANLTYP,
     1                IRCxx)
cc            ircxx=17
            IF(ircxx==0)THEN
              DRDELE(1:3,ll) = xvsat(1:3)
          ELSE
              write(lfnprt,*)'ircxx,ll=',ircxx, ll
            ENDIF
          ENDDO

C        drdp = sum_l=1^6 dr/de_l(t) * y(l)

        drdp(1:3) = 0.d0
        DO ll=1,6
          drdp(1:3) = drdp(1:3) + y(ll)*drdele(1:3,ll)
        ENDDO

      RETURN
      END SUBROUTINE

      END MODULE
