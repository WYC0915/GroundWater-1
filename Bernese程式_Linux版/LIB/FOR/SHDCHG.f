      MODULE s_SHDCHG
      CONTAINS

C*
      SUBROUTINE SHDCHG(IORSYS,TBEG,TEND,T0ARC,NPOINT,T0,H,Q,FAC,YCOE,
     1                  ISHOLD,INDCHG,TCHANG)
CC
CC NAME       :  SHDCHG
CC
CC PURPOSE    :  DETECT WHETHER OR NOT THERE WAS A TRANSIT
CC               THROUGH THE EARTH SHADOW BOUNDARY
CC               IF ISHOLD=0 : A LIGHT --> SHADOW TRNASIT IS SOUGHT
CC               IF ISHOLD=1 : A SHADOW --> LIGHT TRNASIT IS SOUGHT
CC               INDCHG = -1 : NO TRANSIT FOUND
CC               INDCHG = +1 : A L --> S TRANSIT WAS DETECTED
CC               INDCHG =  0 : A S --> L TRANSIT WAS DETECTED
CC               TCHANG : TIME OF TRANSIT
CC
CC PARAMETERS :
CC         IN :  IORSYS : ORBIT SYSTEM                        I*4
CC                        =1: B1950.0
CC                        =2: J2000.0
CC               TBEG   : START OF INTERVAL                   R*8
CC               TEND   : END OF INTERVAL                     R*8
CC               T0ARC  : TIME ORIGIN FOR ARC                 R*8
CC               NPOINT : NUMBER OF POINTS OF GRID FOR SEARCH R*8
CC               T0     : ORIGIN OF DEVELOPMENT (SAT POS)     R*8
CC               H      : NORMALIZATION CONSTANT (SAT POS)    R*8
CC               Q      : POLYNOMIAL DEGREE (SAT POS)         I*4
CC               FAC    : FACTORIALS                          R*8(*)
CC               YCOE   : POLYNOMIAL COEFFICIENTS             R*8(*,*)
CC               ISHOLD : SHADOW INDEX AT TBEG                I*4
CC                        = 0 : IN SUNLIGHT
CC                        = 1 : IN SHADOW
CC        OUT :  INDCHG : NEW LIGHT SHADOW INDEX              I*4
CC               TCHANG : TIME OF TRANSIT (=0.D0 IF THERE     R*8
CC                        WAS NO TRANSIT)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.4  (JAN 94)
CC
CC CREATED    :  92/05/31
CC
CC CHANGES    :  05-JUN-92 : NEW PARAMETER "IORSYS"; NEW CALL "SUNEFF"
CC               10-AUG-94 : MR: CALL EXITRC
CC               29-SEP-94 : MR: CORRECT ERROR IN COMPUTATION OF "DEXT"
CC               23-OCT-94 : GB: USE TDT TO CALL SUNEFF
CC               09-JAN-98 : TS: HANDLE SPECIAL CASE OF "D2=0"
CC               11-JUN-03 : HU: NEW CALL FOR MOSUPN
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               01-OCT-10 : CR: NEW CALL OF MOSUPN
CC               01-OCT-10 : CR: CALL s_MOSUPN AS MODULE
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1990     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE s_mosupn
      USE s_ypol
      USE s_exitrc
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IFIRST, INDCHG, IORSYS, IPOINT, ISHOLD, IZERO ,
     1          K     , MAXPTS, NPOINT
C
      REAL*8    COSW  , D0    , D1    , D2    , D3    , DER   , DET   ,
     1          DEXT  , DT    , DTEXT , DUM   , H     , R     , SINW  ,
     2          T0    , T0ARC , T2    , TBEG  , TCHANG, TEND  , TSEC  ,
     3          TZ1   , TZ2   , XDUM  , YDUM
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      PARAMETER (MAXPTS=200)
C
      REAL*8    YCOE(3,*),FAC(*),XSUN(4),XSAT(3)
      REAL*8    DUMPRE(3,3),DUMNUT(3,3),DUMMON(4),DUMVEL(3)
C
      INTEGER*4 Q
C
      REAL*8    T(MAXPTS),D(MAXPTS)
C
C
      DATA IFIRST/1/
C
C INITIALIZE FACTORIALS, CHECK MAX DIM
C ------------------------------------
      IF(IFIRST.EQ.1)THEN
        IFIRST=0
        IF(NPOINT.GT.MAXPTS)THEN
          WRITE(LFNERR,5) NPOINT,MAXPTS
5         FORMAT(/,' *** SR SHDCHG: TOO MANY GRID POINTS REQUESTED',/,
     1                         16X,'POINTS REQUESTED      :',I4,/,
     2                         16X,'MAXIMUM NUMBER ALLOWED:',I4,/)
          CALL EXITRC(2)
        END IF
        FAC(1) = 1.D0
        DO 10 I = 2,Q+1
          FAC(I) = (I-1)*FAC(I-1)
10      CONTINUE
      END IF
C
C INITIALIZE NORMAL CALL
C ----------------------
      INDCHG=ISHOLD
      TCHANG=0.D0
C
C COMPUTE DISTANCE FROM SHADOW CYLINDER AT GRID POINTS
C ----------------------------------------------------
      DT=(TEND-TBEG)/(NPOINT-1)
      DO 30 IPOINT=1,NPOINT
        T(IPOINT)=TBEG+(IPOINT-1)*DT
        TSEC=(T(IPOINT)-T0ARC-T0)*86400.D0
        IF(DABS(TSEC).LT.1.D-4) TSEC=0.D0
        CALL YPOL(0,Q,3,H,FAC,TSEC,YCOE,XSAT)
        CALL MOSUPN(T(IPOINT),2,DUM,DUM,DUM,XDUM,YDUM,DUM,DUM,
     1                  DUMPRE,DUMNUT,XSUN,DUMMON,DUM,DUMVEL)

        R=0.D0
        COSW=0.D0
        DO 20 K=1,3
          COSW=COSW+XSAT(K)*XSUN(K)
          R =R +XSAT(K)*XSAT(K)
20      CONTINUE
        R=DSQRT(R)
        COSW=COSW/(R*XSUN(4))
        SINW=DSQRT(1.D0-COSW**2)
        D(IPOINT)=SINW*R-6367395.1D0
        IF(COSW.GT.0.D0)THEN
          D(IPOINT)=D(IPOINT)+COSW*10000000.D0
        END IF
30    CONTINUE
C
C FIND CORRECT ROOTS OF FUNCTION D(T)
C -----------------------------------
      IZERO=-1
      DO 50 IPOINT=2,NPOINT
        IF(ISHOLD.EQ.0.D0)THEN
          IF(D(IPOINT).LT.0.D0)THEN
            IZERO=IPOINT-1
            IF(IPOINT.EQ.NPOINT)IZERO=IZERO-1
            GO TO 60
          END IF
        ELSE
          IF(D(IPOINT).GT.0.D0)THEN
            IZERO=IPOINT-1
            IF(IPOINT.EQ.NPOINT)IZERO=IZERO-1
            GO TO 60
          END IF
        END IF
C
C IF NO ROOT WAS FOUND IN INTERVAL (T(IPOINT-2),T(IPOINT)),
C LOOK FOR EXTREMA IN THIS INTERVAL
        IF(IPOINT.GT.2)THEN
          D0=D(IPOINT-1)
          D1=(D(IPOINT)-D(IPOINT-2))/(2*DT)
          D2=(D(IPOINT-2)+D(IPOINT)-2*D0)/(2*DT**2)
          if (d2.ne.0) then
            DTEXT=-D1/(2*D2)
          else
            dtext=0.0d0
          endif
          IF(DABS(DTEXT).LE.DABS(DT))THEN
            DEXT=D0+D1*DTEXT+D2*DTEXT**2
            IF(DEXT*D0.LT.0.D0)THEN
              IZERO=IPOINT-1
              GO TO 60
            END IF
          END IF
        END IF
50    CONTINUE
60    IF(IZERO.EQ.-1)GO TO 999
C
C "PRECISE" TIME T OF ROOT D(T)
C -----------------------------
      IF(IZERO.EQ.NPOINT-1)IZERO=IZERO-1
      D1=D(IZERO)
      T2=T(IZERO+1)
      D2=D(IZERO+1)
      D3=D(IZERO+2)
      D0=D2
      D2=(D1+D3-2*D0)/(2*DT**2)
      D1=(D3-D1)/(2*DT)
      DET=D1**2-4*D0*D2
      IF(DET.LT.0)GO TO 999
      TZ1=(-D1-DSQRT(DET))/(2*D2) + T2
      TZ2=(-D1+DSQRT(DET))/(2*D2) + T2
C
C GET THE CORRECT ROOT
C --------------------
      IF(TEND.GT.TBEG)THEN
        IF(TZ1.GE.TBEG.AND.TZ1.LE.TEND)THEN
          DER=D1+2*D2*(TZ1-T2)
          IF(DER.GE.0.D0)THEN
            INDCHG=0
          ELSE
            INDCHG=1
          END IF
          IF(INDCHG.NE.ISHOLD)THEN
            TCHANG=TZ1
            GO TO 999
          END IF
        END IF
        IF(TZ2.GE.TBEG.AND.TZ2.LE.TEND)THEN
          DER=D1+2*D2*(TZ2-T2)
          IF(DER.GE.0.D0)THEN
            INDCHG=0
          ELSE
            INDCHG=1
          END IF
          IF(INDCHG.NE.ISHOLD)THEN
            TCHANG=TZ2
            GO TO 999
          END IF
        END IF
      ELSE
        IF(TZ1.GE.TEND.AND.TZ1.LE.TBEG)THEN
          DER=D1+2*D2*(TZ1-T2)
          DER=-DER
          IF(DER.GE.0.D0)THEN
            INDCHG=0
          ELSE
            INDCHG=1
          END IF
          IF(INDCHG.NE.ISHOLD)THEN
            TCHANG=TZ1
            GO TO 999
          END IF
        END IF
        IF(TZ2.GE.TEND.AND.TZ2.LE.TBEG)THEN
          DER=D1+2*D2*(TZ2-T2)
          DER=-DER
          IF(DER.GE.0.D0)THEN
            INDCHG=0
          ELSE
            INDCHG=1
          END IF
          IF(INDCHG.NE.ISHOLD)THEN
            TCHANG=TZ2
            GO TO 999
          END IF
        END IF
      END IF
C
999   RETURN
      END SUBROUTINE

      END MODULE
