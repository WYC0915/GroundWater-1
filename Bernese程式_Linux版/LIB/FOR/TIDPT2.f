      MODULE s_TIDPT2
      CONTAINS

C*
      SUBROUTINE TIDPT2(XSAT,SUN,MOON,TMJD,THETA,PRE,NUT,SID,
     1                  XP,YP,IZTID,MPOL,A)
CC
CC NAME       :  TIDPT2
CC
CC PURPOSE    :  PERTURBING ACCELERATION DUE TO TIDES
CC               CORRESPONDING TO IERS STANDARDS 1996.
CC               STEP 1 CORRECTIONS OF SOLID EARTH TIDES
CC               INCLUDED, STEP 2 ONLY TERM DUE TO K1.
CC               SOLID EARTH POLE TIDES INCLUDED
CC               OCEAN TIDE TERMS UP TO N=M=4 INCLUDED
CC               USING ONE OF THE FILES OF UTX
CC               (PARAMETER NTERM=4, DEFINED IN THIS SR)
CC
CC PARAMETERS :
CC        IN  :  XSAT    : POSITION VECTOR OF SATELLITE              R*8
CC               SUN     : GEOCENTRIC POSITION VECTOR OF SUN         R*8
CC               MOON    : GEOCENTRIC POSITION VECTOR OF MOON        R*8
CC               TMJD    : TIME IN MJD                               R*8
CC               THETA   : SIDEREAL TIME GREENWICH                   R*8
CC               PRE     : PRECESSION MATRIX                         R*8
CC               NUT     : NUTATION MATRIX                           R*8
CC               SID     : SIDEREAL TIME MATRIX                      R*8
CC               XP, YP  : COORDINATES OF POLE (RADIAN)              R*8
CC               IZTID   : 0: TIDE FREE, 1: ZERO TIDE                I*4
CC               MPOL    : MEAN POLE                                 I*4
CC                         1: OLD DEF, 2: IERS2003
CC        OUT :  A       : RESULTING ACCELERATION (M/S)              R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  4.0  (JUNE 96)
CC
CC CREATED    :  99/09/12
CC
CC CHANGES    :  27-MAY-98 : JJ: MOVE DATA TO AFTER INCLUDE
CC               03-MAY-00 : HB: USE OF NEW SR DUPRLB INSTEAD OF SR DENORM,
CC                               LPCOEF,LEGPOL, AND DGPRLB,
CC                               ADD THE FIRST THREE POTENTIAL TERMS
CC                               CPOT(I),SPOT(I)=0.D0,(I=1,3), REMOVE PNM
CC                               AND COEFLP, CHANGE DECLARATION OF CPOT AND
CC                               SPOT, CHANGE CALL OF SR OTIDES (INDICES OF
CC                               CPOT,SPOT
CC               30-JUL-02 : HU: USE INTERFACE FOR DUPRLB
CC               06-JAN-04 : HU: PARAMETER IZTID FOR CONVERSION OF ZERO TIDE
CC                               GRAVITY MODELS TO TIDE FREE MODELS
CC               14-DEC-04 : HB: ADDINTERFACE FOR SR OTIDES
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-JUN-05 : MM: UNUSED VARIABLES REMOVED
CC               08-SEP-05 : HB: CHANGE CALL OF SR OTIDES (INDICES OF CPOT
CC                               AND SPOT), 4->1
CC               26-AUG-06 : HB: ADD MEAN POLE (IERS2003)
CC               24-OCT-06 : HB: SET DIFFERENT K20r FOR REMOVING PERMANENT TIDE
CC               21-NOV-06 : HB: USE MAXPOT FROM M_MAXDIM
CC               11-DEC-06 : HB: CONVERT MAXPOT WITH 2.D0*MAXPOT FOR DSQRT
CC               25-JAN-07 : HB: SOLID EARTH TIDES CORRECTION WITH NEW
CC                               SR SETIDES
CC               28-FEB-07 : AG: USE 206264... FROM DEFCON
CC               28-MAR-07 : HB/UH: SOLID EARTH POLE TIDE IN IERS2003 STANDARDS
CC               29-MAR-07 : HB: CONSIDER OTIDES UP TO MAX. SELECTED DEGREE
CC                               OF GRAVITY FIELD (IF AVAILABLE)
CC               05-AUG-08 : DT: REMOVE XMIN FROM CALL TO OTIDES;
CC                               REMOVE UNUSED VARIABLES
CC               03-DEC-10 : KS: NTERM FOR OCEAN TIDES TAKEN FROM PANEL-MXOCTI
CC               06-May-11 : HB: Add Oceanic Pole Tide (IERS2010)
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1996     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE m_maxdim,ONLY: MAXPOT
      USE d_const, ONLY: AE, GM, GMM, GMS,ars
      USE d_model, ONLY: getModKey, mod_orb_subMod, chrValLength
      USE p_orbgen, ONLY: orbdsc
      USE s_readkeys
      USE s_ckopti
      USE s_duprlb
      USE s_nutarg
      USE s_dgpxyz
      USE s_exitrc
      USE s_otides
      USE s_meanpol
      USE s_setides
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IFIRST, IZTID , K     , M     ,
     1          MPOL  , NACT  , irc   , irCode
C
      REAL*8    TDT   , THETA , TMJD  , UPOT  ,
     1          XP    , XQUER , YP    , YQUER
C
      CHARACTER(LEN=chrValLength), SAVE :: subMod
      CHARACTER(LEN=8) :: srNget

      REAL*8    XSAT(*),SUN(*),MOON(*),A(*),PRE(3,*),NUT(3,*)
      REAL*8    XSM(3),CPOT((MAXPOT+1)*(MAXPOT+2)/2),
     1                 SPOT((MAXPOT+1)*(MAXPOT+2)/2),DU1RLB(3)
      REAL*8    DUMMY(3,3),SID(3,*)
      REAL(r8b), SAVE :: K20r
      REAL*8    BETA(6),FNUT(5)
      REAL(r8b)       :: numVal

      INTEGER(i4b),SAVE :: NTERM
C
C
      DATA IFIRST/1/
C
C DEFINE LOVE NUMBERS
C -------------------
      IF(IFIRST.EQ.1)THEN
        IFIRST=0
C
C MAXIMUM ORDER OF GRAVITY FIELD DEVELOPMENT CONSIDERED
C (NTERM <= MAXPOT)
C -----------------------------------------------------
        NTERM=4
CCC        NTERM=8
        NTERM=orbdsc%otddeg

        IF(NTERM.GT.MAXPOT)THEN
          WRITE(LFNERR,101)NTERM,MAXPOT
101       FORMAT(//,' *** SR TIDPT2: NTERM0',I3,' GT MAXPOT=',I3,//)
          CALL EXITRC(2)
        END IF
C
C LOVE NUMBERS FOR REMOVING PERMANENT TIDE
C ----------------------------------------
        K20r=0.30D0
C
! Get IERS Conventions string
! ---------------------------
        CALL getModKey(mod_orb_subMod,subMod,srNget,numVal)

      END IF
C
C MEAN POLE
C ---------
      CALL MEANPOL(MPOL,TMJD,XQUER,YQUER)
C
C INITIALIZE TERMS OF GEOPOTENTIAL
C --------------------------------
      DO 1 M=1,(NTERM+1)*(NTERM+2)/2
        CPOT(M)=0.D0
        SPOT(M)=0.D0
1     CONTINUE
!
! DOODSON ARGUMENTS
! -----------------
      TDT=TMJD+(19.D0+32.184D0)/86400.D0
      CALL NUTARG(TDT,THETA,BETA,FNUT)

! Solid Earth tide corrections
! ----------------------------
      CALL SETIDES(SUN,MOON,PRE,NUT,SID,BETA,CPOT,SPOT)
C
C REMOVE PERMANENT TIDE FROM C02 (FOR JGM-3 NOT FOR GEM-T3)
C ---------------------------------------------------------
      IF (IZTID.EQ.1) CPOT(4)=CPOT(4)+1.3914129D-8*K20r
C
C SOLID EARTH POLE TIDE (Ch. 6.5, eq(22) and Ch.7.1.4, eq(22))
C ------------------------------------------------------------
!! IERS1996
!!!       CPOT(5)=CPOT(5)-1.290D-9*(XP-XQUER)*ARS
!!!       SPOT(5)=SPOT(5)+1.290D-9*(YP-YQUER)*ARS
      CPOT(5)=CPOT(5)-1.333D-9*((XP-XQUER)-0.0115D0*(YP-YQUER))*ARS
      SPOT(5)=SPOT(5)+1.333D-9*((YP-YQUER)+0.0115D0*(XP-XQUER))*ARS
C
C OCEANIC POLE TIDE (Ch. 6.5, eq(24) and Ch.7.1.4, eq(22))
C --------------------------------------------------------
!!!!!!!!!!!!!! IERS2010 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF (subMod(1:8) == 'IERS2010') THEN
        CPOT(5)=CPOT(5)-2.1778D-10*((XP-XQUER)+0.01724D0*(YP-YQUER))*ARS
        SPOT(5)=SPOT(5)+1.7232D-10*((YP-YQUER)+0.03365D0*(XP-XQUER))*ARS
      ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
C APPLY OCEAN TIDES
C -----------------
      CALL OTIDES(NTERM,BETA,CPOT,SPOT,NACT)
      IF(NACT < 4) NACT=4
      IF(NACT*(NACT+1)/2.GT.SIZE(CPOT))THEN
        WRITE(LFNERR,102)NACT,MAXPOT
102     FORMAT(//,' *** SR TIDPT2: NACT (SR OTIDES)',/,
     1            '                IS TOO LARGE: ',I3,/,
     2            '                MAXPOT= ',I3,//)
        CALL EXITRC(2)
      END IF
C
C COMPUTE ACCELERATION
C --------------------
      CALL duprlb(ae,gm,nact,cpot,spot,xsat,1,upot,du1rlb,dummy)
      CALL DGPXYZ(XSAT,1,DU1RLB,DUMMY,XSM,DUMMY)
      DO 110 K=1,3
        A(K)=A(K)+XSM(K)
110   CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
