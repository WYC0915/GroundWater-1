      MODULE s_DEQRHS
      CONTAINS

C*
      SUBROUTINE DEQRHS(NUMSAT,ITIM,TJJ,YSAT,NVAR,LOCQ,ORBMOD,F,A0,DFDP)
CC
CC NAME       :  DEQRHS
CC
CC PURPOSE    :  COMPUTATION OF RIGHT HAND SIDES OF LINEARIZED
CC               EQUATIONS OF MOTIONS OF A SATELLITES:
CC
CC                (2)
CC               R    = A R + F, F = F0 - A * R + F1
CC
CC               AND ITS NVAR DERIVATIVES Z  WITH RESPECT TO THE
CC                                         I
CC               PARAMETERS P
CC                           I
CC                (2)
CC               Z    = A Z + DFDP , I=1,2,..,NVAR
CC
CC
CC PARAMETERS :
CC         IN :  NUMSAT : SATELLITE NUMBER                      I*4
CC               ITIM=0,1 (ITIM=1: RPR SPRINGER MODEL)          I*4
CC               TJJ TIME IN MJD-T0ARC                          R*8
CC               (DEFINED IN SR SATINT)
CC               YSAT(I),I=1,2,3 COORDINATES OF SATELLITE       R*8
CC               IN SYSTEM B1950.0 OR IN J2000.0
CC               NVAR : NUMBER OF VARIATIONAL EQNS              I*4
CC               LOCQ(K,I),K=1,2,..,6, I=1,2,..,NVAR: DESCRIP-  I*4
CC                         TION OF PARAMETERS
CC               ORBMOD : ORBIT MODEL ARRAY                     I*4(*)
CC        OUT :  F(I),I=1,2,3 : SEE ABOVE EQUATION              R*8
CC               F1(I),I=1,2,3: LEAST SIGNIFICANT PART OF       R*8
CC                      SATELLITE ACCELERATION
CC               A0(I),I=1,2,3: JACOBIAN OF F0 (WITH RESPECT    R*8
CC                      TO COORDINATES)
CC               DFDP(I),I=1,2,...,3*NVAR: PARTIAL DERIVATIVES  R*8
CC                      OF SAT. ACCELERATION WITH RESPECT TO
CC                      PARAMETERS
CC
CC REMARKS    :  THREE LABELED COMMON AREAS ARE USED HERE:
CC
CC               A.) COMMON POTCOE
CC                 CPOT(I),SPOT(I) DENORMALIZED COEFFICIENTS CNM,SNM
CC                 OF EARTH GRAVITY FIELD
CC                 COEFLP(I) ARE AUXILIARY COEFFICIENTS USED TO COMPUTE
CC                 THE EARTH'S GRAVITATIONAL ATTRACTION
CC                 NTERM IS THE UPPER BOUNDARY FOR THE POTENTIAL SERIES
CC                 DEVELOPMENT
CC               B.) COMMON CONST : SEE SR DEFCON
CC               C.) COMMON RPRESS: RADIATION PRESSURE PARAMETERS
CC
CC                 AXIS SYSTEMS ARE: D, Y, B
CC                              AND: X, Y, Z
CC
CC AUTHOR     :  G. BEUTLER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/12/01 14:39
CC
CC CHANGES    :  31-MAY-92 : ??: ROCK4/42 MODEL, COMMON/RPRESS CHANGED
CC               04-JUN-92 : ??: OPTION J2000.0. ADD COMMON/ORBSYS
CC               22-JUN-92 : ??: OPTION P0-DRIFT, COMMON/RPRESS
CC               15-JUL-92 : ??: REGULARIZATION OF E2 (IF NECESSARY)
CC               16-JUL-92 : ??: Y-BIAS DRIFT INSTEAD OF P0-DRIFT
CC               16-NOV-92 : ??: CORRECT LIGHT <--> SHADOW TRANSIT
CC                               FOR VARIATIONAL EQNS
CC               23-OCT-94 : GB: INTRODUCTION OF SR GRVSM2 TO COMPUTE
CC                               FIRST DERIVATIVE OF FORCE MORE ACCURATELY
CC               23-OCT-94 : GB: USE SR MOSUPN TO COMPUTE POSITIONS OF
CC                               SUN AND MOON, XPOLE, YPOLE, UT1-UTC,
CC                               GPS-UTC, PRECESSION, NUTATION.
CC               10-JAN-95 : GB: MODIFICATIONS TO TAKE NEW RADIATION
CC                               PRESSURE MODELS INTO ACCOUNT
CC                               (SR ROCKMD REPLACED BY SR RCKMD2)
CC                               A SCALING PARAMETER (SCLFAC) IS COMING
CC                               OUT OF SR RCKMD2, WHICH IS USED IN THIS
CC                               SR TO SCALE THE RADIATION PRESSURE (RPRMOM)
CC                               AND THE PARTIALS. A NEW VARIABLE RPRMO2(3)
CC                               HAD TO BE DEFINED.
CC               08-MAR-95 : GB: REMOVE EVERY TRACE OF IE2, E2 (!)
CC               27-APR-95 : GB: X-COMP OF RADIATION PRESSURE
CC               28-DEC-95 : GB: NEW ORBIT MODEL
CC               08-JAN-96 : MR: RCKMD2 RENAMED TO ROCKMD
CC               27-JUN-96 : TS: ADDED ORBIT MODEL ARRAY
CC               24-AUG-96 : GB: "ORBMOD=3": 1 PER REV PARMS USED
CC               12-SEP-96 : GB: SR PLAPOS (PERTURBATIONS BY JUPITER,
CC                               VENUS, MARS)
CC                               SR TIDPT2: SOLID EARTH TIDES ACCORDING
CC                                          TO IERS STANDARDS 96 INCL
CC                                          OCEAN TIDES
CC               15-OCT-96 : TS: CORRECTED DECLARATION OF YSAT(3) TO YSAT(*)
CC               28-OCT-96 : TS: MINOR CORRECTION OF THE E1,E2,E3 VECTORS
CC               29-JUN-98 : TS: ADDED CODE RPR-MODEL
CC               30-NOV-99 : HB: INCREASE DIMENSION OF PNM FROM 800 TO 2000,
CC                               DIMENSION OF COEFLP FROM 40 TO 50
CC                               BECAUSE OF MORE EARTH POTENTIAL COEFFICIENTS
CC               03-MAY-00 : HB: USE OF NEW SR DUPRLB INSTEAD OF SR DENORM,
CC                               LPCOEF,LEGPOL, AND DGPRLB FOR EARTH POTENTIAL
CC                               COEFFICIENTS => COMMON POTCOE: NORMALIZED
CC                               COEFFICIENTS OF THE EARTH GRAVITY FIELD,
CC                               INCLUDE I:MAXPOT, REMOVE COEFLP FROM COMMON
CC                               POTCOE, REMOVE COMMON CDQRHS, CHANGE
CC                               DECLARATIONS OF CPOT AND SPOT
CC               21-AUG-00 : MR: ADD TIME AS PARAMETER TO SR ROCKMD
CC               30-JUN-02 : HU: USE INTERFACE FOR DUPRLB
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               11-JUN-03 : HU: USE GSTIME
CC               11-JUN-03 : HU: DESACTIVATE USE OF GMST2000
CC               06-AUG-03 : HU: NEW CALL FOR GSTIME
CC               30-AUG-03 : HU: SHARED DO LABELS REMOVED
CC               23-OCT-03 : HU: CONSIDER MOON'S PENUMBRA FOR DERIVATIVES
CC               12-DEC-03 : AJ: ADD STOCHASTIC ACCELERATIONS
CC               06-JAN-04 : HU: COMMON POTCOE, CALL FOR TIDPT2 MODIFIED
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-JUN-05 : MM: UNUSED VARIABLES REMOVED
CC               07-JUL-05 : HB: USE T_EPOCH FOR GSTIME
CC               26-AUG-06 : HU: MEAN POLE: NEW CALL FOR TIDPT2
CC               18-OCT-06 : MP: ITIM ADDED
CC               04-MAY-08 : RD: NUMSAT ADDED TO CALL OF SR ARGSUN
CC               17-JUL-08 : DT: CALL TO ROCKMD CHANGED (NONRPR)
CC               21-JUL-08 : DT: IMPLEMENT DRSW MODEL (ORBMOD(6)==2)
CC               13-AUG-10 : CR: EARTH RADIATION PRESSURE ADDED
CC               12-OCT-10 : HB: TAKE MAXINT FROM P_ORBGEN AND NOT M_MAXDIM
CC               26-NOV-10 : RD: CALL GTSATR WITH MODSVN
CC               01-DEC-10 : CR: SOLAR CONSTANT CORRECTED
CC               17-MAR-11 : CR: CALL S_MOSUPN AS MODULE
CC               06-MAY-11 : HB: USE D_MODEL TO GET MEAN POLE MODEL INFORMATION
CC               01-SEP-11 : RD: RECOVER "B0" FOR ROCKMD FROM ARGSUN
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern
      USE m_maxdim, ONLY: MAXPOT
      USE m_epoch,  ONLY: t_epoch, OPERATOR(.realToEpoch.)
      USE d_const,  ONLY: AE, GM, GMM, GMS, PI
      USE d_satfil, ONLY: t_satfil, init_satfil
      USE d_model,  ONLY: getModKey, chrValLength, mod_orb_meaPol
      USE p_orbgen, ONLY: maxint
      USE s_argsun
      USE s_dmlmtv
      USE s_duprlb
      USE s_mosupn
      USE s_rswvec
      USE s_tidpt2
      USE s_shadow
      USE s_grvsm2
      USE s_plapos
      USE s_eclmoon
      USE s_vprod
      USE s_sidmat
      USE s_dgpxyz
      USE f_gstime
      USE s_tidalf
      USE s_arglat
      USE s_tidpot
      USE s_gravsm
      USE s_dmlmam
      USE s_rockmd
      USE s_dmlmtm
      USE s_genrel
      USE s_dmlmav
      USE s_boxwerpf
      USE f_modsvn
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , I0    , IFTID , II    , IORSYS, IPAR  , IPOTNM,
     1          IRAD  , ISHAD , ISHUSE, ITYP  , K     , NTERM , NUMSVN,
     2          NVAR  , ITIM  , NUMSAT, ISTC  , IFRC  , IP    , NSTMOM,
     3          INTMOM, REFF  , ANTTHR, ERPMOD, GRDTYP
C
      REAL*8    ABSE2 , AGL   , B0    , DISTP , DISTU , DUM   , EQEQUI,
     1          FECL  , FRAC  , GMA   , GMJ   , GMV   , GPSUTC, DELTAT,
     2          RPRMOM, RSAT  , RSAT2 , RSAT3 , RSUN  , SCALPA, SCLFAC,
     3          SCLFCP, SIDTIM, T0ARC , TJ    , TJJ   , TOSC  , U0    ,
     4          UPOT  , UT1UTC, XPOLE , YPOLE , U0D   , S0
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER*4 LOCQ(6,*),ORBMOD(*)
      INTEGER*4 ISTMOM(MAXINT),NSCMOM(MAXINT),FCTMOM(3,MAXINT)
C
      REAL*8 YSAT(:),F(3),F0(3),F1(3),Y(3),DU1RLB(3),DU1XYZ(3)
      REAL*8 ACCEL(3),NONRPR(3)
      REAL*8 DFDP(*)
      REAL*8 DU2RLB(3,3),DU2XYZ(3,3),SUN(4),MOON(4),JUP(4),VEN(4)
      REAL*8 MAR(4),A0MAR(3,3)
      REAL*8 PRE(3,3),NUT(3,3),SID(3,3)
      REAL*8 E2(3),E1(3),E3(3)
      REAL*8 EX(3),EZ(3)
      REAL*8 KAPPA(3,3),HELP(3,3),A0(3,3),DU1RLH(3),DU1XYH(3)
      REAL*8 A0SUN(3,3),A0MOON(3,3),A0JUP(3,3),A0VEN(3,3)
      REAL*8 ER(3),ES(3),EW(3)
      REAL*8 CPOT((MAXPOT+1)*(MAXPOT+2)/2),SPOT((MAXPOT+1)*(MAXPOT+2)/2)
      REAL*8 AGL1,AGL3
      REAL*8 FERP(3),DUMVEL(3),SUNVEL(3)
      REAL*8 ABSEZ
      REAL*8 PSCMOM(3,MAXINT)
      REAL*8 TSTMOM(MAXINT)
C
      TYPE(t_epoch) :: tut1,ttt
C
      CHARACTER(LEN=chrValLength)  :: chrmPol
      CHARACTER(LEN=8)   :: srNget
      INTEGER(i4b), SAVE :: mPol
      REAL(r8b), SAVE    :: LOVE
      REAL(r8b)          :: numVal
      LOGICAL, SAVE      :: first=.TRUE.
C
      COMMON/POTCOE/CPOT,SPOT,NTERM,IPOTNM,IFTID
      COMMON/ALBMDF/ANTTHR,ERPMOD
      COMMON/RPRESS/RPRMOM(21),SCALPA(21),TOSC,NUMSVN,ISHUSE
      COMMON/STCACC/INTMOM,NSTMOM,ISTMOM,NSCMOM,
     1              PSCMOM,TSTMOM,FCTMOM
      COMMON/TORIGO/T0ARC
      COMMON/ORBSYS/IORSYS
C
      IF (first) THEN
        first = .FALSE.
C 0. LOVE'S CONSTANT
        IF (ORBMOD(3).EQ.0) THEN
          LOVE=.285D0
        ELSEIF (ORBMOD(3).EQ.1) THEN
          LOVE=.300D0
        ENDIF
        CALL getModKey(mod_orb_meaPol,chrmPol,srNget,numVal)
        mPol = IDNINT(numVal)
      ENDIF
C
C 2. COMPUTATION OF PRECESSION- AND NUTATION-MATRICES AND OF
C    SOLAR AND LUNAR POSITIONS AND DISTANCES AT TIME TJJ+T0ARC
C
      TJ=TJJ+T0ARC
      CALL MOSUPN(TJ,-1,DUM,DUM,DUM,XPOLE,YPOLE,UT1UTC,GPSUTC,
     1                  PRE,NUT,SUN,MOON,EQEQUI,SUNVEL)
C
C 3. KEPLER - FORCE - TERM
      RSAT2=YSAT(1)**2+YSAT(2)**2+YSAT(3)**2
      RSAT=DSQRT(RSAT2)
      RSAT3=RSAT*RSAT2
      DO 10 I=1,3
        F0(I)=-GM*YSAT(I)/RSAT3
        F1(I)=0.D0
        ACCEL(I)=0.D0
        NONRPR(I)=0.D0
10    CONTINUE
C
C 3A GENERAL RELATIVISTIC TERMS
      IF (ORBMOD(5).EQ.1) CALL GENREL(YSAT,F1)
C
C 4. ADD LUNAR AND SOLAR GRAVITATIONAL ATTRACTION
      CALL GRAVSM(GMS,YSAT,SUN,F1)
      CALL GRAVSM(GMM,YSAT,MOON,F1)
      CALL GRVSM2(GMS,YSAT,SUN,A0SUN)
      CALL GRVSM2(GMM,YSAT,MOON,A0MOON)
C
C 4X. ADD GRAVITATIONAL PERTURBATIONS DUE TO JUPITER AND VENUS
      IF(ORBMOD(7).EQ.1)THEN
        CALL PLAPOS(5,TJ,SUN,GMJ,JUP)
        CALL GRAVSM(GMJ,YSAT,JUP,F1)
        CALL PLAPOS(2,TJ,SUN,GMV,VEN)
        CALL GRAVSM(GMV,YSAT,VEN,F1)
        CALL PLAPOS(4,TJ,SUN,GMA,MAR)
        CALL GRAVSM(GMA,YSAT,MAR,F1)
        CALL GRVSM2(GMJ,YSAT,JUP,A0JUP)
        CALL GRVSM2(GMV,YSAT,VEN,A0VEN)
        CALL GRVSM2(GMA,YSAT,MAR,A0MAR)
      END IF
C
C 4A. ADD TIDAL PERTURBATIONS DUE TO SUN AND MOON
      IF (ORBMOD(4).EQ.0) CALL TIDALF(LOVE,YSAT,SUN,MOON,F1)
C
C 5. ADD RADIATION PRESSURE
C    (IF SATELLITE IS IN SUNLIGHT)
      CALL SHADOW(YSAT,SUN,ISHAD)
cc see shmhdl
      CALL eclmoon(ysat,moon,sun,frac,fecl,distp,distu)
      RSUN=DSQRT((YSAT(1)-SUN(1))**2+(YSAT(2)-SUN(2))**2+
     1           (YSAT(3)-SUN(3))**2)
C
      ABSEZ=DSQRT(YSAT(1)**2+YSAT(2)**2+YSAT(3)**2)
      DO 14 K=1,3
        E3(K)=(SUN(K)-YSAT(K))/RSUN
        EZ(K)=-YSAT(K)/ABSEZ
14    CONTINUE
C
      CALL VPROD(YSAT,E3,E2)
      ABSE2=DSQRT(E2(1)**2+E2(2)**2+E2(3)**2)
      DO 15 I=1,3
        E2(I)=E2(I)/ABSE2
15    CONTINUE
C
      CALL VPROD(E3,E2,E1)
      CALL VPROD(E2,EZ,EX)
C
      CALL RSWVEC(YSAT,ER,ES,EW)
C
C USE EXTERNAL SHADOW ASSESSMENT ?
C ------------------------------
      IF(ISHUSE.NE.-1)THEN
        ISHAD=ISHUSE
      END IF
C
      CALL ARGLAT(YSAT,AGL)
      CALL ARGSUN(TJ,SUN,DUMVEL,YSAT,NUMSAT,B0,DUM,U0,DUM)
      AGL1=AGL-U0
      AGL3=3.0D0*AGL1
C
C GET RPR (and empirical along-track for model=3)
C -----------------------------------------------
      DO 20 I=1,3
        ACCEL(I)=0.D0
        NONRPR(I)=0.D0
20    CONTINUE

      CALL ROCKMD(TJ,NUMSVN,YSAT,SUN,E2,AGL,B0,U0,ACCEL,SCLFAC,NONRPR)
C
C Apply non-RPR accelerations from satellite information file
C -----------------------------------------------------------
      DO I=1,3
        F1(I)=F1(I)+NONRPR(I)
      END DO
C
C Apply solar radiation pressure if not in shadow
C -----------------------------------------------
      IF(ISHAD.EQ.0)THEN
        DO I=1,3
          F1(I)=F1(I)+ACCEL(I)
        END DO
C
C A PRIORI MODEL: ROCK4/ROCK42
C ----------------------------
        IF(ORBMOD(6).EQ.0)THEN
           DO I=1,3
              IF (ITIM.EQ.0) THEN
                 F1(I)=F1(I)+RPRMOM(1)*SCLFAC*E3(I)
     1                      +RPRMOM(2)*SCLFAC*E2(I)
     2                      +RPRMOM(3)*SCLFAC*E1(I)
     3                      +RPRMOM(4)*SCLFAC*E3(I)*DCOS(AGL)
     4                      +RPRMOM(5)*SCLFAC*E2(I)*DCOS(AGL)
     5                      +RPRMOM(6)*SCLFAC*E1(I)*DCOS(AGL)
     6                      +RPRMOM(7)*SCLFAC*E3(I)*DSIN(AGL)
     7                      +RPRMOM(8)*SCLFAC*E2(I)*DSIN(AGL)
     8                      +RPRMOM(9)*SCLFAC*E1(I)*DSIN(AGL)
              ELSE
                 F1(I)=F1(I)+RPRMOM(1)*SCLFAC*E3(I)
     1                      +RPRMOM(2)*SCLFAC*E2(I)
     2                      +RPRMOM(3)*SCLFAC*E1(I)
     3                      +RPRMOM(4)*SCLFAC*EX(I)*DSIN(AGL1)
     4                      +RPRMOM(5)*SCLFAC*E1(I)*DSIN(AGL1)
     5                      +RPRMOM(6)*SCLFAC*E1(I)
     6                      +RPRMOM(7)*SCLFAC*EX(I)*DSIN(AGL3)
     7                      +RPRMOM(8)*SCLFAC*E3(I)*DSIN(AGL1)
     8                      +RPRMOM(9)*SCLFAC*EZ(I)*DSIN(AGL1)
              END IF
           END DO
C DRSW system (Lageos)
C --------------------
        ELSE IF(ORBMOD(6).EQ.2)THEN
           DO I=1,3
              F1(I)=F1(I)+RPRMOM(1)*SCLFAC*E3(I)
           END DO
        END IF
      END IF
C
C ONCE PER REV MODEL
C ------------------
      IF(ORBMOD(6).EQ.1)THEN
        CALL RSWVEC(YSAT,ER,ES,EW)
        DO 22 I=1,3
          F1(I)=F1(I)+RPRMOM(1)*ER(I)
     1               +RPRMOM(2)*ES(I)
     2               +RPRMOM(3)*EW(I)
     3               +RPRMOM(4)*ER(I)*DCOS(AGL)
     4               +RPRMOM(5)*ES(I)*DCOS(AGL)
     5               +RPRMOM(6)*EW(I)*DCOS(AGL)
     6               +RPRMOM(7)*ER(I)*DSIN(AGL)
     7               +RPRMOM(8)*ES(I)*DSIN(AGL)
     8               +RPRMOM(9)*EW(I)*DSIN(AGL)
22      CONTINUE
C
C DRSW system (Lageos)
C --------------------
      ELSE IF(ORBMOD(6).EQ.2) THEN
        CALL RSWVEC(YSAT,ER,ES,EW)
        DO 23 I=1,3
          F1(I)=F1(I)+RPRMOM(2)*ES(I)
     1               +RPRMOM(3)*EW(I)
     2               +RPRMOM(4)*ER(I)*DCOS(AGL)
     3               +RPRMOM(5)*ES(I)*DCOS(AGL)
     4               +RPRMOM(6)*EW(I)*DCOS(AGL)
     5               +RPRMOM(7)*ER(I)*DSIN(AGL)
     6               +RPRMOM(8)*ES(I)*DSIN(AGL)
     7               +RPRMOM(9)*EW(I)*DSIN(AGL)
23      CONTINUE
      END IF
C
C STOCH. ACCELERATIONS (ONLY IN UPDATE MODE)
C ------------------------------------------
      ISTC=0
      DO 30 IP=1,NSTMOM
        IF(ISTMOM(IP).LE.INTMOM)THEN
          ISTC=IP
        END IF
30    CONTINUE
      IF(ISTC.EQ.0)GO TO 60
C
      DELTAT=TSTMOM(ISTC+1)-TSTMOM(ISTC)
C
      DO 50 IFRC=1,NSCMOM(ISTC)
        IF(FCTMOM(IFRC,ISTC).EQ.11)THEN
          DO 31 I=1,3
            F1(I)=F1(I)+PSCMOM(IFRC,ISTC)*ER(I)
31        CONTINUE
        ELSE IF(FCTMOM(IFRC,ISTC).EQ.12)THEN
          DO 32 I=1,3
            F1(I)=F1(I)+PSCMOM(IFRC,ISTC)*ES(I)
32        CONTINUE
        ELSE IF(FCTMOM(IFRC,ISTC).EQ.13)THEN
          DO 33 I=1,3
            F1(I)=F1(I)+PSCMOM(IFRC,ISTC)*EW(I)
33        CONTINUE
        ELSE IF(FCTMOM(IFRC,ISTC).EQ.14)THEN
          DO 34 I=1,3
            F1(I)=F1(I)+PSCMOM(IFRC,ISTC)*E3(I)
34        CONTINUE
        ELSE IF(FCTMOM(IFRC,ISTC).EQ.15)THEN
          DO 35 I=1,3
            F1(I)=F1(I)+PSCMOM(IFRC,ISTC)*E2(I)
35        CONTINUE
        ELSE IF(FCTMOM(IFRC,ISTC).EQ.16)THEN
          DO 36 I=1,3
            F1(I)=F1(I)+PSCMOM(IFRC,ISTC)*E1(I)
36        CONTINUE
C
C APPLY PIECEWISE LINEAR ACCELERATIONS
C ------------------------------------
        ELSE IF(FCTMOM(IFRC,ISTC).EQ.21)THEN
          DO 41 I=1,3
            F1(I)=F1(I)+((TJ-TSTMOM(ISTC))*PSCMOM(IFRC,ISTC+1)+
     1                   (TSTMOM(ISTC+1)-TJ)*PSCMOM(IFRC,ISTC))*ER(I)
     2                   /DELTAT
41        CONTINUE
        ELSE IF(FCTMOM(IFRC,ISTC).EQ.22)THEN
          DO 42 I=1,3
            F1(I)=F1(I)+((TJ-TSTMOM(ISTC))*PSCMOM(IFRC,ISTC+1)+
     1                   (TSTMOM(ISTC+1)-TJ)*PSCMOM(IFRC,ISTC))*ES(I)
     2                   /DELTAT
42        CONTINUE
        ELSE IF(FCTMOM(IFRC,ISTC).EQ.23)THEN
          DO 43 I=1,3
            F1(I)=F1(I)+((TJ-TSTMOM(ISTC))*PSCMOM(IFRC,ISTC+1)+
     1                   (TSTMOM(ISTC+1)-TJ)*PSCMOM(IFRC,ISTC))*EW(I)
     2                   /DELTAT
43        CONTINUE
        ELSE IF(FCTMOM(IFRC,ISTC).EQ.24)THEN
          DO 44 I=1,3
            F1(I)=F1(I)+((TJ-TSTMOM(ISTC))*PSCMOM(IFRC,ISTC+1)+
     1                   (TSTMOM(ISTC+1)-TJ)*PSCMOM(IFRC,ISTC))*E3(I)
     2                   /DELTAT
44        CONTINUE
        ELSE IF(FCTMOM(IFRC,ISTC).EQ.25)THEN
          DO 45 I=1,3
            F1(I)=F1(I)+((TJ-TSTMOM(ISTC))*PSCMOM(IFRC,ISTC+1)+
     1                   (TSTMOM(ISTC+1)-TJ)*PSCMOM(IFRC,ISTC))*E2(I)
     2                   /DELTAT
45        CONTINUE
        ELSE IF(FCTMOM(IFRC,ISTC).EQ.26)THEN
          DO 46 I=1,3
            F1(I)=F1(I)+((TJ-TSTMOM(ISTC))*PSCMOM(IFRC,ISTC+1)+
     1                   (TSTMOM(ISTC+1)-TJ)*PSCMOM(IFRC,ISTC))*E1(I)
     2                   /DELTAT
46        CONTINUE
        END IF
50    CONTINUE
C
60    CONTINUE
C
C 6. ADD ALL TERMS DUE TO EARTH POTENTIAL
C 6.1 TRANSFORMATION INTO EARTH-FIXED SYSTEM
      CALL DMLMAV(YSAT,PRE,Y)
      CALL DMLMAV(Y,NUT,Y)
      TUT1 = .realToEpoch.(tj+ut1utc-gpsutc)
      TTT  = .realToEpoch.tj
      SIDTIM=GSTIME(0,TUT1,TTT,NUT(2,1),EQEQUI)
      CALL SIDMAT(tj,XPOLE,YPOLE,SIDTIM,SID)
      CALL DMLMAV(Y,SID,Y)
C
C 6.2 COMPUTATION OF LEGENDRE POLYNOMIALS UP TO DEGREE AND ORDER NTERM
C     (IS NOW MADE IN SR DUPRLB)
C 6.3 COMPUTATION OF DERIVATIVES OF POTENTIAL WITH RESPECT TO R,LAMBDA,
C     BETA
      CALL DUPRLB(AE,GM,NTERM,CPOT,SPOT,Y,2,UPOT,DU1RLB,DU2RLB)
      DO II=1,3
        DU1RLH(II)=DU1RLB(II)
      END DO
C
C 6.4 COMPUTATION OF DERIVATIVES OF EARTH POTENTIAL WITH RESPECT TO
C     X,Y,Z
      CALL DGPXYZ(Y,1,DU1RLB,DU2RLB,DU1XYZ,DU2XYZ)
      CALL DGPXYZ(Y,0,DU1RLH,DU2RLB,DU1XYH,DU2XYZ)
C
C TIDAL CORRECTIONS INCLUDING K1-TERM
C -----------------------------------
      IF (ORBMOD(4).EQ.1) THEN
        CALL TIDPOT(LOVE,Y,SUN,MOON,SIDTIM,PRE,NUT,SID,ORBMOD,DU1XYZ)
      ELSEIF (ORBMOD(4).GT.1) THEN
        CALL TIDPT2(Y,SUN,MOON,TJ,SIDTIM,PRE,NUT,SID,XPOLE,YPOLE,
     1              IFTID,mPol,DU1XYZ)
      ENDIF
C
C 6.5 TRANSFORM BACK INTO SYSTEM B1950.0 OR J2000.0
      CALL DMLMAM(NUT,PRE,HELP)
      CALL DMLMAM(SID,HELP,KAPPA)
      CALL DMLMTV(DU1XYZ,KAPPA,DU1XYZ)
      CALL DMLMTV(DU1XYH,KAPPA,DU1XYH)
      CALL DMLMAM(DU2XYZ,KAPPA,HELP)
      CALL DMLMTM(KAPPA,HELP,A0)
C
C EARTH RADIATION PRESSURE & ANTENNA THRUST
C -----------------------------------------
ccc
cccC     FORCE IN INERTIAL REFERENCE FRAME
ccc      REFF = 0
ccc
cccC     GRID TYPE
ccc      GRDTYP = 3
ccc
cccC     SOLAR CONSTANT
ccc      S0 = 1367D0
ccc
cccC     A PRIORI ACCELERATION
ccc      IF((ERPMOD.GT.0).OR.(ANTTHR.EQ.1))THEN
ccc         CALL BOXWERPF(MODSVN(NUMSAT),TJ,ERPMOD,ANTTHR,GRDTYP,REFF,YSAT,
ccc     1                 SUN,EZ,E3,E2,EX,E1,S0,KAPPA,FERP)
ccc      ELSE
ccc         DO K=1,3
ccc            FERP(K) = 0D0
ccc         ENDDO
ccc      ENDIF
ccc
ccc      DO K=1,3
ccc         F1(K) = F1(K) + FERP(K)
ccc      ENDDO
ccc
cccC     OUTPUT FILE -> ACCELERATION (CHANGE REFF TO 1,2 OR 3)
cccC      WRITE(LFNPRT,"('SAT',I3.3,3ES15.3,3F9.2)")
cccC     1      numsat,FERP(1),FERP(2),FERP(3),B0*180D0/PI,
cccC     2      U0D*180D0/PI
C
C KEPLER - PART OF MATRIX A0, ADD TERMS DUE TO SUN AND MOON :
C ---------------------------------------------------------
      DO 65 I=1,3
        DO K=1,3
          A0(I,K)=A0(I,K)+GM/RSAT3*3*YSAT(I)*YSAT(K)/RSAT2
          IF(I.EQ.K)A0(I,K)=A0(I,K)-GM/RSAT3
          A0(I,K)=A0(I,K)+A0SUN(I,K)+A0MOON(I,K)
          IF(ORBMOD(7).EQ.1)THEN
            A0(I,K)=A0(I,K)+A0JUP(I,K)+A0MAR(I,K)
CC ??           A0(I,K)=A0(I,K)+A0JUP(I,K)+A0MAR(I,K)+A0VEN(I,K)
          END IF
        ENDDO
65    CONTINUE
      CALL DMLMAV(YSAT,A0,F)
C
C 6.6 ADD EARTH POTENTIAL TERMS
      DO 70 I=1,3
        F0(I)=(F0(I)+DU1XYH(I))
        F1(I)=(F1(I)-DU1XYH(I)+DU1XYZ(I))
        F(I)=-F(I)+F0(I)+F1(I)
70    CONTINUE
C
C 7. PARTIAL DERIVATIVES OF FORCE WITH RESPECT TO PARAMETERS
C ----------------------------------------------------------
cc see shmhdl
      sclfcp=frac*sclfac
cc    sclfcp=sclfac
      DO 300 IPAR=1,NVAR
        ITYP=LOCQ(1,IPAR)
        I0=3*(IPAR-1)
        IF(ITYP.EQ.3)THEN
C
C ORBIT PARAMETERS
C ----------------
          IRAD=LOCQ(4,IPAR)
          IF(IRAD.LT.7)THEN
            DO 110 K=1,3
              DFDP(I0+K)=0.D0
110         CONTINUE
          ELSE IF(IRAD.EQ.7)THEN
            IF(ORBMOD(6).EQ.0 .OR. ORBMOD(6).EQ.2)THEN
              IF(ISHAD.EQ.0)THEN
                DO 120 K=1,3
                  DFDP(I0+K)=sclfcp/SCALPA(IRAD)*E3(K)
120             CONTINUE
              ELSE
                DO 125 K=1,3
                  DFDP(I0+K)=0.D0
125             CONTINUE
              END IF
            ELSE IF(ORBMOD(6).EQ.1)THEN
              DO 1201 K=1,3
                DFDP(I0+K)=1.D0/SCALPA(IRAD)*ER(K)
1201          CONTINUE
            END IF
C
          ELSE IF(IRAD.EQ.8)THEN
            IF(ORBMOD(6).EQ.0)THEN
              IF(ISHAD.EQ.0)THEN
                DO 130 K=1,3
                  DFDP(I0+K)=sclfcp/SCALPA(IRAD)*E2(K)
130             CONTINUE
              ELSE
                DO 135 K=1,3
                  DFDP(I0+K)=0.D0
135             CONTINUE
              END IF
            ELSE IF(ORBMOD(6).EQ.1 .OR. ORBMOD(6).EQ.2)THEN
              DO 1301 K=1,3
                DFDP(I0+K)=1.D0/SCALPA(IRAD)*ES(K)
1301          CONTINUE
            END IF
C
          ELSE IF(IRAD.EQ.9) THEN
            IF(ORBMOD(6).EQ.0)THEN
              IF(ISHAD.EQ.0)THEN
                DO 140 K=1,3
                  DFDP(I0+K)=sclfcp/SCALPA(IRAD)*E1(K)
140             CONTINUE
              ELSE
                DO 145 K=1,3
                  DFDP(I0+K)=0.D0
145             CONTINUE
              END IF
            ELSE IF(ORBMOD(6).EQ.1 .OR. ORBMOD(6).EQ.2)THEN
              DO 1401 K=1,3
                DFDP(I0+K)=1.D0/SCALPA(IRAD)*EW(K)
1401          CONTINUE
            END IF
C
          ELSE IF(IRAD.EQ.10)THEN
            IF(ORBMOD(6).EQ.0)THEN
              IF(ISHAD.EQ.0)THEN
                DO K=1,3
                   IF (ITIM.EQ.0)THEN
                      DFDP(I0+K)=sclfcp/SCALPA(IRAD)*E3(K)*DCOS(AGL)
                   ELSE
                      DFDP(I0+K)=SCLFAC/SCALPA(IRAD)*EX(K)*DSIN(AGL1)
                   ENDIF
               END DO
              ELSE
                DO 155 K=1,3
                  DFDP(I0+K)=0.D0
155             CONTINUE
              END IF
            ELSE IF(ORBMOD(6).EQ.1 .OR. ORBMOD(6).EQ.2)THEN
              DO 1501 K=1,3
                DFDP(I0+K)=1.D0/SCALPA(IRAD)*ER(K)*DCOS(AGL)
1501          CONTINUE
            END IF
C
          ELSE IF(IRAD.EQ.11)THEN
            IF(ORBMOD(6).EQ.0)THEN
              IF(ISHAD.EQ.0)THEN
                DO K=1,3
                   IF (ITIM.EQ.0) THEN
                      DFDP(I0+K)=sclfcp/SCALPA(IRAD)*E2(K)*DCOS(AGL)
                   ELSE
                      DFDP(I0+K)=SCLFAC/SCALPA(IRAD)*E1(K)*DSIN(AGL1)
                   ENDIF
               END DO
              ELSE
                DO 165 K=1,3
                  DFDP(I0+K)=0.D0
165             CONTINUE
              END IF
            ELSE IF(ORBMOD(6).EQ.1 .OR. ORBMOD(6).EQ.2)THEN
              DO 1601 K=1,3
                DFDP(I0+K)=1.D0/SCALPA(IRAD)*ES(K)*DCOS(AGL)
1601          CONTINUE
            END IF
C
          ELSE IF(IRAD.EQ.12)THEN
            IF(ORBMOD(6).EQ.0)THEN
              IF(ISHAD.EQ.0)THEN
                DO K=1,3
                   IF (ITIM.EQ.0) THEN
                      DFDP(I0+K)=sclfcp/SCALPA(IRAD)*E1(K)*DCOS(AGL)
                   ELSE
                      DFDP(I0+K)=SCLFAC/SCALPA(IRAD)*E1(K)
                   END IF
               END DO
              ELSE
                DO 175 K=1,3
                  DFDP(I0+K)=0.D0
175             CONTINUE
              END IF
            ELSE IF(ORBMOD(6).EQ.1 .OR. ORBMOD(6).EQ.2)THEN
              DO 1701 K=1,3
                DFDP(I0+K)=1.D0/SCALPA(IRAD)*EW(K)*DCOS(AGL)
1701          CONTINUE
            END IF
C
          ELSE IF(IRAD.EQ.13)THEN
            IF(ORBMOD(6).EQ.0)THEN
              IF(ISHAD.EQ.0)THEN
                DO K=1,3
                   IF (ITIM.EQ.0) THEN
                      DFDP(I0+K)=sclfcp/SCALPA(IRAD)*E3(K)*DSIN(AGL)
                   ELSE
                      DFDP(I0+K)=SCLFAC/SCALPA(IRAD)*EX(K)*DSIN(AGL3)
                   END IF
                END DO
              ELSE
                DO 185 K=1,3
                  DFDP(I0+K)=0.D0
185             CONTINUE
              END IF
            ELSE IF(ORBMOD(6).EQ.1 .OR. ORBMOD(6).EQ.2)THEN
              DO 1801 K=1,3
                DFDP(I0+K)=1.D0/SCALPA(IRAD)*ER(K)*DSIN(AGL)
1801          CONTINUE
            END IF
C
          ELSE IF(IRAD.EQ.14)THEN
            IF(ORBMOD(6).EQ.0)THEN
              IF(ISHAD.EQ.0)THEN
                DO K=1,3
                   IF (ITIM.EQ.0) THEN
                      DFDP(I0+K)=sclfcp/SCALPA(IRAD)*E2(K)*DSIN(AGL)
                   ELSE
                      DFDP(I0+K)=SCLFAC/SCALPA(IRAD)*E3(K)*DSIN(AGL1)
                   END IF
                END DO
              ELSE
                DO 195 K=1,3
                  DFDP(I0+K)=0.D0
195             CONTINUE
              END IF
            ELSE IF(ORBMOD(6).EQ.1 .OR. ORBMOD(6).EQ.2)THEN
              DO 1901 K=1,3
                DFDP(I0+K)=1.D0/SCALPA(IRAD)*ES(K)*DSIN(AGL)
1901          CONTINUE
            END IF
C
          ELSE IF(IRAD.EQ.15)THEN
            IF(ORBMOD(6).EQ.0)THEN
              IF(ISHAD.EQ.0)THEN
                DO K=1,3
                   IF (ITIM.EQ.0) THEN
                      DFDP(I0+K)=sclfcp/SCALPA(IRAD)*E1(K)*DSIN(AGL)
                   ELSE
                      DFDP(I0+K)=SCLFAC/SCALPA(IRAD)*EZ(K)*DSIN(AGL1)
                   END IF
                END DO
              ELSE
                DO 205 K=1,3
                  DFDP(I0+K)=0.D0
205             CONTINUE
              END IF
            ELSE IF(ORBMOD(6).EQ.1 .OR. ORBMOD(6).EQ.2)THEN
              DO 2001 K=1,3
                DFDP(I0+K)=1.D0/SCALPA(IRAD)*EW(K)*DSIN(AGL)
2001          CONTINUE
            END IF
          ELSE IF(IRAD.EQ.16)THEN
            DO 210 K=1,3
              DFDP(I0+K)=1.D0/SCALPA(IRAD)*ER(K)
210         CONTINUE
          ELSE IF(IRAD.EQ.17)THEN
            DO 220 K=1,3
              DFDP(I0+K)=1.D0/SCALPA(IRAD)*ES(K)
220         CONTINUE
          ELSE IF(IRAD.EQ.18)THEN
            DO 230 K=1,3
              DFDP(I0+K)=1.D0/SCALPA(IRAD)*EW(K)
230         CONTINUE

          ELSE IF(IRAD.EQ.19)THEN
            DO 240 K=1,3
              DFDP(I0+K)=1.D0/SCALPA(IRAD)*TJJ*ER(K)
240         CONTINUE
          ELSE IF(IRAD.EQ.20)THEN
            DO 250 K=1,3
              DFDP(I0+K)=1.D0/SCALPA(IRAD)*TJJ*ES(K)
250         CONTINUE
          ELSE IF(IRAD.EQ.21)THEN
            DO 260 K=1,3
              DFDP(I0+K)=1.D0/SCALPA(IRAD)*TJJ*EW(K)
260         CONTINUE
          END IF
        END IF
300   CONTINUE
      RETURN
      END SUBROUTINE

      END MODULE
