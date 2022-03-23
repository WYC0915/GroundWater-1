      MODULE s_PDSTC
      CONTAINS

C*
      SUBROUTINE PDSTC(LOCQ,IORSYS,TOBS,TSEC,ELE,TOSC,TIMSTC,
     1                 TUV,SCAL,DRDELE,NDIFF,ONEOR2,DER)
CC
CC NAME       :  PDSTC
CC
CC PURPOSE    :  COMPUTE PARTIALS WITH RESPECT TO STOCHASTIC ORBIT
CC               PARAMETERS
CC
CC PARAMETERS :
CC        IN  :  LOCQ   : CHARACTERIZATION OF PARAMETER         I*4(*)
CC               IORSYS : ORBIT SYSTEM                          I*4
CC                        =1: B1950.0
CC                        =2: J2000.0
CC               TOBS   : OBSERVATION TIME                      R*8
CC               TSEC   : OBSERVATION TIME IN SEC SINCE         R*8
CC                        OSCULATION EPOCH
CC               ELE    : OSCULATING ELEMENTS (SEE GETORB)      R*8(*)
CC               TOSC   : OSCULATION EPOCH (MJD)                R*8
CC               TIMSTC : STOCHASTIC EPOCHS                     R*8(*,*,*,*)
CC               TUV    : UNIT VECTORS STATION --> SATELLITE    R*8(*,*)
CC               SCAL   : SCALING FACTOR                        R*8(*)
CC               DRDELE : PARTIALS OF POSITION VECTOR OF        R*8(3,*)
CC                        SATELLITE WITH RESPECT TO ELEMENTS
CC                        A, E, I, NODE, PER, U0
CC               NDIFF  : DIFFERENCE TYPE                       I*4
CC                        NDIFF=0: ZERO DIFFERENCE
CC                        NDIFF=1: SINGLE DIFFERENCE
CC               ONEOR2 : INDICATOR FOR LEO AS A STATION        I*4
CC                        IN DD PROCESSING
CC                        ONEOR2=1 LEO IS FIRST STATION
CC                        ONEOR2=2 LEO IS SECOND STATION
CC       OUT  :  DER    : RESULTING PARTIAL DERIVATIVE          R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G. BEUTLER
CC
CC VERSION    :  3.4 (JAN 93)
CC
CC CREATED    :  92/12/30
CC
CC CHANGES    :  09-NOV-93 : MR: "SVN" REMOVED FROM PARAMETER LIST
CC               10-AUG-94 : MR: CALL EXITRC
CC               23-SEP-97 : DI: USE INCLUDE 'MAXSAT.inc'
CC               06-JUN-01   DS: NEW PARAMETERS: NDIFF, ONEOR2
CC               06-JUN-01   DS: DER COMPUTATION FOR LEO (ZD AND DD PROCESSING)
CC                               DEPENDING ON NDIFF
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               12-DEC-03 : AJ: ADDITIONAL INDEX FOR SCAL AND TIMSTC
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               30-MAY-07 : AG: USE s_suneff
CC               01-OCT-10 : CR: NEW CALL OF SUNEFF
CC               29-FEB-12 : RD: CORRECT ARRAY DIMENSIONS OF SVNSYS ROUTINE
CC               28-MAR-12 : RD: USE SVNSYS AS MODULE NOW
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1992     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_maxdim, ONLY: MAXSAT
      USE d_const, ONLY: GM
      USE s_cootra
      USE s_vprod
      USE s_ephem
      USE s_maxtst
      USE s_suneff
      USE s_eleprt
      USE f_svnsys
      USE s_exitrc
      USE s_ddreh
      USE s_dmlmav
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IAOLD , IARC  , IELE  , IFIRST, INDSTC, IORSYS,
     1          IP    , IPE   , IRC   , ISA   , ISAT  , IST   , K     ,
     2          L     , MAXSTC, MXCSAT, MXCSTC, NDIFF
C
      REAL*8    A     , ABSE2 , DER   , DIST  , E     , EX    , RSAT  ,
     1          SZ    , TDT   , TOBS  , TOSC  , TPER  , TSEC  , TSEC0 ,
     2          TSTC  , UT1GPS, V     , XM    , XN    , XPOL  , YPOL
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
      PARAMETER (MAXSTC=30)
C
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMSTC/MXCSTC,MXNSTC
C
C GLOBAL DECLARATIONS
      CHARACTER*6 MXNSAT,MXNSTC
      REAL*8      ELE(*),TIMSTC(2,MXCSTC,MXCSAT,*),TUV(3,*),DRDELE(3,*)
      REAL*8      SCAL(*)
      INTEGER*4   LOCQ(*), ONEOR2, NUMSVN(1)

C
C LOCAL DECLARATIONS
      INTEGER*4   FRCTYP,STCDEF(3,MAXSTC,MAXSAT)
      REAL*8      DELEDP(6,3,MAXSTC,MAXSAT),XSAT(3),VSAT(3)
      REAL*8      DRDSTC(3),DUM3(3)
      REAL*8      E1(3),E2(3),E3(3),XSUN(4),ESUN(3),RSW(3),ELEPAR(8)
      REAL*8      ROT(3,3)
C
      DATA IAOLD/-1/,IFIRST/1/
C
C MAXIMUM DIMENSION
      IF(IFIRST.EQ.1)THEN
        IFIRST=0
        CALL MAXTST(1,'PDSTC ',MXNSAT,MAXSAT,MXCSAT,IRC)
        IF(IRC.NE.0) CALL EXITRC(2)
        CALL MAXTST(1,'PDSTC ',MXNSTC,MAXSTC,MXCSTC,IRC)
        IF(IRC.NE.0) CALL EXITRC(2)
      END IF
C
C EXTRACT NECESSARY INFORMATION FROM LOCQ
      IARC     =LOCQ(2)
      NUMSVN(1)=LOCQ(3)
      INDSTC   =LOCQ(4)
      FRCTYP   =LOCQ(5)
      IPE      =LOCQ(6)
      ISAT     =LOCQ(7)
      TSTC     =TIMSTC(1,INDSTC,ISAT,IARC)
C
C DEFINE DERIVATIVE
      IF(TOBS.LT.TSTC)THEN
        DER=0.D0
      ELSE
C
C NEW ARC ?
        IF(IARC.NE.IAOLD)THEN
          IAOLD=IARC
          DO 1 IP=1,3
            DO 1 IST=1,MAXSTC
              DO 1 ISA=1,MAXSAT
                STCDEF(IP,IST,ISA)=0
1         CONTINUE
        END IF
C
C IS ARRAY DELEDP ALREADY DEFINED ?
        IF(STCDEF(IPE,INDSTC,ISAT).EQ.0)THEN
          STCDEF(IPE,INDSTC,ISAT)=1
C
C COMPUTE COMPONENTS R, S, AND W FOR EACH INDIVIDUAL FORCE
          IF(FRCTYP.EQ.1)THEN
            RSW(1)=1
            RSW(2)=0
            RSW(3)=0
          ELSE IF(FRCTYP.EQ.2)THEN
            RSW(1)=0
            RSW(2)=1
            RSW(3)=0
          ELSE IF(FRCTYP.EQ.3)THEN
            RSW(1)=0
            RSW(2)=0
            RSW(3)=1
          ELSE IF(FRCTYP.GE.4)THEN
            TDT=TSTC+(19.D0+32.184D0)/86400.D0
            CALL SUNEFF(IORSYS,2.D0,TDT,XSUN,DUM3)
            TSEC0=(TSTC-TOSC)*86400.D0
            CALL EPHEM(GM,ELE(1),ELE(2),ELE(3),ELE(4),ELE(5),ELE(7),
     1                 TSEC0,XSAT,VSAT)
            A=ELE(1)
            E=ELE(2)
            TPER=ELE(7)
            DIST=0.D0
            DO 10 K=1,3
              ESUN(K)=XSUN(K)-XSAT(K)
              DIST=DIST+ESUN(K)**2
10          CONTINUE
            DIST=DSQRT(DIST)
            DO 20 K=1,3
              ESUN(K)=ESUN(K)/DIST
20          CONTINUE
            XN=DSQRT(GM/A**3)
            XM=XN*(TSEC0-TPER)
            EX=XM
            DO 30 K=1,10
              EX=XM+E*DSIN(EX)
30          CONTINUE
            V=2*DATAN(DSQRT((1+E)/(1-E))*TAN(EX/2))
            IF(FRCTYP.EQ.4)THEN
              CALL DDREH(3,ELE(4),ROT)
              CALL DMLMAV(ESUN,ROT,RSW)
              CALL DDREH(1,ELE(3),ROT)
              CALL DMLMAV(RSW,ROT,RSW)
              CALL DDREH(3,ELE(5)+V,ROT)
              CALL DMLMAV(RSW,ROT,RSW)
            ELSE
              CALL VPROD(XSAT,ESUN,E2)
              ABSE2=DSQRT(E2(1)**2+E2(2)**2+E2(3)**2)
              DO 40 I=1,3
                E2(I)=E2(I)/ABSE2
40            CONTINUE
              IF(FRCTYP.EQ.5)THEN
                CALL DDREH(3,ELE(4),ROT)
                CALL DMLMAV(E2,ROT,RSW)
                CALL DDREH(1,ELE(3),ROT)
                CALL DMLMAV(RSW,ROT,RSW)
                CALL DDREH(3,ELE(5)+V,ROT)
                CALL DMLMAV(RSW,ROT,RSW)
              ELSE IF(FRCTYP.EQ.6)THEN
                RSAT=DSQRT(XSAT(1)**2+XSAT(2)**2+XSAT(3)**2)
                DO 50 K=1,3
                  E3(K)=-XSAT(K)/RSAT
50              CONTINUE
                CALL VPROD(E2,E3,E1)
                CALL DDREH(3,ELE(4),ROT)
                CALL DMLMAV(E1,ROT,RSW)
                CALL DDREH(1,ELE(3),ROT)
                CALL DMLMAV(RSW,ROT,RSW)
                CALL DDREH(3,ELE(5)+V,ROT)
                CALL DMLMAV(RSW,ROT,RSW)
              END IF
            END IF
          END IF
          TSEC0=(TSTC-TOSC)*86400.D0
C
C PARTIAL DERIVATIVES OF ELEMENTS WITH RESPECT TO PARAMETER P OF
C TYPE P*RSW, P BEING THE PARAMETER, RSW THE UNIT VECTOR SPECIFIED
          CALL ELEPRT(GM,ELE(1),ELE(2),ELE(3),ELE(4),ELE(5),ELE(7),
     1                0.D0,TSEC0,RSW,ELEPAR)
C
C COMPUTE CORRESPONDING CHANGES FOR OSCULATING ELEMENTS AT EPOCH TOSC
          DO 60 IELE=1,5
            DELEDP(IELE,IPE,INDSTC,ISAT)=ELEPAR(IELE)
60        CONTINUE
          DELEDP(6,IPE,INDSTC,ISAT)=ELEPAR(8)
        END IF
C
C COMPUTE PARTIAL DERIVATIVE
        DO 130 K=1,3
          DRDSTC(K)=0.D0
          DO 120 L=1,6
            DRDSTC(K)=DRDSTC(K)+DRDELE(K,L)*
     1                DELEDP(L,IPE,INDSTC,ISAT)/SCAL(1)
120       CONTINUE
130     CONTINUE
        CALL COOTRA(IORSYS,0,TOBS,DRDSTC,SZ,XPOL,YPOL,UT1GPS)

        DER=0.D0
        IF(SVNSYS(9,1,NUMSVN)) THEN
          DO K=1,3
            IF (NDIFF.EQ.1) THEN
              IF (ONEOR2.EQ.1) THEN
                DER=DER-TUV(K,1)*DRDSTC(K)
              ELSE
               DER=DER+TUV(K,2)*DRDSTC(K)
              END IF
            ELSE
              DER=DER-TUV(K,1)*DRDSTC(K)
            ENDIF
          END DO
        ELSE
          DO K=1,3
            IF (NDIFF.EQ.1) THEN
              DER=DER+(TUV(K,1)-TUV(K,2))*DRDSTC(K)
            ELSE
              DER=DER+TUV(K,1)*DRDSTC(K)
            END IF
          END DO
        END IF

      END IF
C
      RETURN
      END SUBROUTINE

      END MODULE
