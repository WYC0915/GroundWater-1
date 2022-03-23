      MODULE s_BOXWERPF
      CONTAINS
C*
      SUBROUTINE BOXWERPF(NUMSAT,TJ,ERM,ANT,GRD,REFF,YSAT,SUN,
     1                    Z_SAT,D_SUN,Y_SAT,X_SAT,B_SUN,S0,KAPPA,FORCE)
CC
CC NAME       :  BOXWERPF
CC
CC PURPOSE    :  COMPUTATION OF EARTH RADIATION PRESSURE ACTING ON A
CC               BOX-WING SATELLITE
CC
CC PARAMETERS :
CC         IN :  NUMSAT   : PRN NUMBER                        I*4
CC               TJ       : TIME IN MJD                       R*8
CC               ERM      : EARTH RADIATION MODEL             I*4
CC                          0 = NONE
CC                          1 = EARTH RADIATION PRESSURE (ANALYTICAL)
CC                          2 = EARTH RADIATION PRESSURE (CERES DATA)
CC               ANT      : 0 = NO ANTENNA THRUST             I*4
CC                          1 = WITH ANTENNA THRUST
CC               GRD      : 1 = 2.5 DEGREE GRID               I*4
CC                          2 = 5.0 DEGREE GRID
CC                          3 = 10  DEGREE GRID
CC               REFF     : FORCE IN REFERENCE FRAME:         I*4
CC                          0 = INERTIAL
CC                          1 = BODY FIXED (Z,Y,X)
CC                          2 = SUN FIXED (D,Y,B)
CC                          3 = ORBITAL (RADIAL, ALONG- AND CROSS-TRACK)
CC               YSAT     : SATELLITE POSITION [m] AND        R*8(6)
CC                          VELOCITY [m/s] (INERTIAL),
CC                          (POSITION,VELOCITY) = (RX,RY,RZ,VX,VY,VZ)
CC               SUN      : SUN POSITION VECTOR [m]           R*8(3)
CC                          (INERTIAL)
CC               ORIENTATION OF THE SATELLITE IN INERTIAL REFERENCE FRAME
CC               DESCRIBED BY THE FOLLOWING FIVE UNIT VECTORS:
CC               Z_SAT    : NAVIGATION ANTENNAS               R*8(3)
CC                          (TOWARDS THE EARTH)
CC               D_SUN    : SOLAR PANELS (TOWARDS THE SUN)    R*8(3)
CC               Y_SAT    : ALONG SOLAR PANELS BEAMS          R*8(3)
CC               X_SAT    : BUS FACE ALWAYS ILLUMINATED       R*8(3)
CC                          BY THE SUN
CC               B_SUN    : PERPENDICULAR TO D_SUN AND Y_SAT  R*8(3)
CC               S0       : SOLAR CONSTANT                    R*8
CC               KAPPA    : ROTATION MATRIX FROM INERTIAL     R*8
CC                          TO EARTH-FIXED REFERENCE FRAME
CC                          (FOR ERM=2)
CC        OUT :  FORCE    : ACCELERATION VECTOR [M/S**2]      R*8(3)
CC
CC AUTHOR     :  C.J. RODRIGUEZ-SOLANO
CC               rodriguez@bv.tum.de
CC
CC VERSION    :  5.1 (AUG 2010)
CC
CC CREATED    :  13-AUG-10
CC
CC CHANGES    :  09-FEB-11 : MF: CALL GTSATR WITH MODSVN
CC               17-MAR-11 : CR: GET ATTITUDE INFORMATION AS INPUT
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      2010     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE d_const, ONLY: C, AU, PI
      USE d_satfil, ONLY: t_rprmod
      USE s_sprod
      USE s_vprod
      USE s_gtflna
      USE s_opnfil
      USE s_opnerr
      USE s_boxwsurf
      USE s_rockprop
      USE s_rswvec
      USE s_dmlmtv
      USE s_gtsatr
      USE f_modsvn
      USE m_bern
C
      IMPLICIT NONE
C
      INTEGER*4 NUMSAT,BLKNUM,ERM,ANT,GRD,REFF,SBLK,INDB
      INTEGER*4 IFIRST,IRC,IOSTAT,II,JJ,K,INTDUM
      INTEGER*4 LATK,LONK,LATKMX,LONKMX,GRDCER
C
      REAL*8 YSAT(6),SUN(3),FORCE(3)
      REAL*8 S0,TOA,ALB,MASS,TJ,READUM
C
C     EARTH AND SATELLITE PROPERTIES
      REAL*8 CERES_R(72,144),CERES_E(72,144)
      REAL*8 CERGRE(72,144),CERGEM(72,144)
      REAL*8 D_AREA_ALL(72,144),V_NS_ALL(72,144,3)
      REAL*8 AREA(4,2,15),REFL(4,2,15),DIFU(4,2,15),ABSP(4,2,15)
      REAL*8 AREA2(4,2,15),REFL2(4,2,15),DIFU2(4,2,15),ABSP2(4,2,15)
      REAL*8 REFLIR(4,2,15),DIFUIR(4,2,15),ABSPIR(4,2,15)
      REAL*8 AREAS(4,2),REFLS(4,2),DIFUS(4,2),ABSPS(4,2)
      REAL*8 AREA2S(4,2),REFL2S(4,2),DIFU2S(4,2),ABSP2S(4,2)
      REAL*8 REFLIRS(4,2),DIFUIRS(4,2),ABSPIRS(4,2)
C
C     ATTITUDE
      REAL*8 RADVEC(3),ALGVEC(3),CRSVEC(3),ESUN(3)
      REAL*8 Z_SAT(3),D_SUN(3),Y_SAT(3),X_SAT(3),B_SUN(3)
      REAL*8 ATTSURF(3,4)
C
      REAL*8 ABSNCFVI,ABSNCFIR,ALBFAC,PHASEVI,PHASEIR
      REAL*8 NCFVEC(3)
      REAL*8 D_ANG,GRDANG,GRDNUM,LATIND,LONIND
      REAL*8 D_AREA,LAT_IN,LON_IN,DIST2,ABSDIST
      REAL*8 COSLAT
      REAL*8 V_NS(3),V_INS(3),V_DIST(3),V_SAT(3)
      REAL*8 COS_IN,COS_RE,DUMMY1,DUMMY2,PSIDOT,ABSSUN,ABSPOS
      REAL*8 REFL_CF,EMIT_CF,E_REFL,E_EMIT
      REAL*8 FORCE_LL(3),FREF(3)
      REAL*8 PSI,ANTFORCE
      REAL*8 KAPPA(3,3)
      REAL*8 DUMPART(4,2,3)
C
      CHARACTER*8  ANLTYP
      CHARACTER*3  SVNNR
      CHARACTER*3  PLANE
C
      CHARACTER(LEN=fileNameLength) :: filnam
      CHARACTER(LEN=5000)           :: line
      CHARACTER(LEN=25)             :: item
C
      TYPE(T_RPRMOD) RPRMOD
C
      DATA ifirst/1/


C BLOCK NUMBER AND MASS
      CALL GTSATR(1,MODSVN(NUMSAT),TJ,RPRMOD,SVNNR,BLKNUM,MASS,ANLTYP,
     1            PLANE,READUM,READUM,INTDUM,READUM)

C Top of Atmosphere for CERES
      TOA = 6371000D0 + 30000D0
C Albedo of the Earth
      ALB = 0.3D0
C Initialization of force vector
      FORCE(1) = 0D0
      FORCE(2) = 0D0
      FORCE(3) = 0D0

C ----------------------------------------
C LOAD SATELLITE PROPERTIES AND CERES DATA
C ----------------------------------------

      IF ((IFIRST==1).AND.(ERM.GT.0)) THEN

C     PROPERTIES FOR ALL SATELLITES BLOCKS
        DO SBLK = 1,105
          IF((SBLK.LE.10).OR.(SBLK.GT.100))THEN

            CALL ROCKPROP(SBLK,AREAS,REFLS,DIFUS,ABSPS,AREA2S,REFL2S,
     1                    DIFU2S,ABSP2S,REFLIRS,DIFUIRS,ABSPIRS)

            IF(SBLK.LE.10)THEN
              INDB = SBLK
            ELSEIF(SBLK.GT.100)THEN
              INDB = SBLK - 90
            ENDIF

            DO II = 1,4
              DO JJ = 1,2
                AREA(II,JJ,INDB) = AREAS(II,JJ)
                REFL(II,JJ,INDB) = REFLS(II,JJ)
                DIFU(II,JJ,INDB) = DIFUS(II,JJ)
                ABSP(II,JJ,INDB) = ABSPS(II,JJ)

                AREA2(II,JJ,INDB) = AREA2S(II,JJ)
                REFL2(II,JJ,INDB) = REFL2S(II,JJ)
                DIFU2(II,JJ,INDB) = DIFU2S(II,JJ)
                ABSP2(II,JJ,INDB) = ABSP2S(II,JJ)

                REFLIR(II,JJ,INDB) = REFLIRS(II,JJ)
                DIFUIR(II,JJ,INDB) = DIFUIRS(II,JJ)
                ABSPIR(II,JJ,INDB) = ABSPIRS(II,JJ)
              ENDDO
            ENDDO
          ENDIF
        ENDDO


        IF(ERM.EQ.2)THEN
C     REFLECTIVITY ----------------------------------------------
          CALL GTFLNA(0,'ALBREFL',FILNAM,IRC)
          CERES_R = 0
          IF (IRC.EQ.0) THEN
            CALL OPNFIL(LFNLOC,FILNAM,'UNKNOWN','FORMATTED',
     1                  ' ',' ',IOSTAT)
            CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILNAM,'BOXWERPF')
            DO ii=1,72
              READ(LFNLOC,"(A)") line
              DO jj=1,144
                item = line((jj-1)*25+1:jj*25)
                IF (index(item,'NaN') /= 0) THEN
                  CERES_R(ii,jj) = 0D0
                ELSE
                  READ(ITEM,*) CERES_R(ii,jj)
                ENDIF
              ENDDO
            ENDDO
            CLOSE(LFNLOC)
C     write(*,*)albrfl(1,1),albrfl(5,7),albrfl(55,144)
          ENDIF
C     EMISSIVITY ------------------------------------------------
          CALL GTFLNA(0,'ALBEMIS',FILNAM,IRC)
          CERES_E = 0
          IF (IRC.EQ.0) THEN
            CALL OPNFIL(LFNLOC,FILNAM,'UNKNOWN','FORMATTED',
     1                  ' ',' ',IOSTAT)
            CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILNAM,'BOXWERPF')
            DO ii=1,72
              READ(LFNLOC,"(A)") line
              DO jj=1,144
                item = line((jj-1)*25+1:jj*25)
                IF (index(item,'NaN') /= 0) THEN
                  CERES_E(ii,jj) = 0D0
                ELSE
                  READ(ITEM,*) CERES_E(ii,jj)
                ENDIF
              ENDDO
            ENDDO
            CLOSE(LFNLOC)
          ENDIF


C     PRE-INTEGRATION, INDEPENDENT OF SATELLITE POSITION
          GRDANG = 2.5D0
          LATIND = 36.5D0
          LONIND = 72.5D0
          IF(GRD.EQ.1)THEN
            GRDNUM = 1D0
            LATKMX = 72
            LONKMX = 144
          ELSEIF(GRD.EQ.2)THEN
            GRDNUM = 2D0
            GRDCER = 2
            LATKMX = 36
            LONKMX = 72
          ELSEIF(GRD.EQ.3)THEN
            GRDNUM = 4D0
            GRDCER = 4
            LATKMX = 18
            LONKMX = 36
          ENDIF

          GRDANG = GRDANG*GRDNUM
          LATIND = (LATIND-0.5D0)/GRDNUM + 0.5D0
          LONIND = (LONIND-0.5D0)/GRDNUM + 0.5D0

          D_ANG = (PI*GRDANG/180D0)**2

          DO LATK = 1,LATKMX
            DO LONK = 1,LONKMX

              LAT_IN = (LATK-LATIND)*GRDANG*(PI/180D0)
              LON_IN = (LONK-LONIND)*GRDANG*(PI/180D0)

C             Sphere normal vector and differential of area
              COSLAT = DCOS(LAT_IN)
              D_AREA_ALL(LATK,LONK) = (TOA**2)*COSLAT*D_ANG
              V_NS_ALL(LATK,LONK,1) = COSLAT*DCOS(LON_IN)
              V_NS_ALL(LATK,LONK,2) = COSLAT*DSIN(LON_IN)
              V_NS_ALL(LATK,LONK,3) = DSIN(LAT_IN)

C             New matrix of Reflectivity and Emissivity
              CERGRE(LATK,LONK) = 0D0
              CERGEM(LATK,LONK) = 0D0
              IF(GRD.EQ.1)THEN
                CERGRE(LATK,LONK) = CERES_R(LATK,LONK)
                CERGEM(LATK,LONK) = CERES_E(LATK,LONK)
              ELSEIF((GRD.EQ.2).OR.(GRD.EQ.3))THEN
                DO II = 0,(GRDCER-1)
                  DO JJ = 0,(GRDCER-1)
                    CERGRE(LATK,LONK) = CERGRE(LATK,LONK)
     1                     + CERES_R(GRDCER*LATK-II,GRDCER*LONK-JJ)
                    CERGEM(LATK,LONK) = CERGEM(LATK,LONK)
     1                     + CERES_E(GRDCER*LATK-II,GRDCER*LONK-JJ)
                  ENDDO
                ENDDO
                CERGRE(LATK,LONK) = CERGRE(LATK,LONK)/(GRDNUM**2)
                CERGEM(LATK,LONK) = CERGEM(LATK,LONK)/(GRDNUM**2)
              ENDIF

            ENDDO
          ENDDO
C
        ENDIF
        IFIRST = 0
      ENDIF


C ------------------
C SATELLITE ATTITUDE
C ------------------

      CALL RSWVEC(YSAT,RADVEC,ALGVEC,CRSVEC)

      IF(ERM.GT.0)THEN
        DO K=1,3
          ATTSURF(K,1) = Z_SAT(K)
          ATTSURF(K,2) = Y_SAT(K)
          ATTSURF(K,3) = X_SAT(K)
          ATTSURF(K,4) = D_SUN(K)
        ENDDO
      ENDIF

C ----------------------------
C OPTICAL PROPERTIES PER BLOCK
C ----------------------------

      IF(ERM.GT.0)THEN
        IF(BLKNUM.LE.10)THEN
          INDB = BLKNUM
        ELSEIF(BLKNUM.GT.100)THEN
          INDB = BLKNUM - 90
        ENDIF

        DO II = 1,4
          DO JJ = 1,2
            AREAS(II,JJ) = AREA(II,JJ,INDB)
            REFLS(II,JJ) = REFL(II,JJ,INDB)
            DIFUS(II,JJ) = DIFU(II,JJ,INDB)
            ABSPS(II,JJ) = ABSP(II,JJ,INDB)

            AREA2S(II,JJ) = AREA2(II,JJ,INDB)
            REFL2S(II,JJ) = REFL2(II,JJ,INDB)
            DIFU2S(II,JJ) = DIFU2(II,JJ,INDB)
            ABSP2S(II,JJ) = ABSP2(II,JJ,INDB)

            REFLIRS(II,JJ) = REFLIR(II,JJ,INDB)
            DIFUIRS(II,JJ) = DIFUIR(II,JJ,INDB)
            ABSPIRS(II,JJ) = ABSPIR(II,JJ,INDB)
          ENDDO
        ENDDO
      ENDIF

C ----------------------
C EARTH RADIATION MODELS
C ----------------------

      IF(ERM.GT.0)THEN
        ABSSUN = DSQRT(SUN(1)**2 + SUN(2)**2 + SUN(3)**2)
        DO K=1,3
          ESUN(K) = SUN(K)/ABSSUN
        ENDDO

        ABSPOS = DSQRT(YSAT(1)**2 + YSAT(2)**2 + YSAT(3)**2)

        PSIDOT = ESUN(1)*RADVEC(1)+ESUN(2)*RADVEC(2)+ESUN(3)*RADVEC(3)
        IF(DABS(PSIDOT).GT.(1D0-1D-6))THEN
          PSI = 0D0
        ELSE
          PSI = DACOS(PSIDOT)
        ENDIF
        S0 = S0*(AU/ABSSUN)**2
      ENDIF

C     ANALYTICAL MODEL
      IF(ERM.EQ.1)THEN

        NCFVEC(1) = RADVEC(1)
        NCFVEC(2) = RADVEC(2)
        NCFVEC(3) = RADVEC(3)
        ALBFAC    = (PI*TOA**2)*(S0/C)/(ABSPOS**2)
        PHASEVI   = (2*ALB/(3*PI**2))*((PI-PSI)*DCOS(PSI)+DSIN(PSI))
        PHASEIR   = (1-ALB)/(4*PI)
        ABSNCFVI  = ALBFAC*PHASEVI
        ABSNCFIR  = ALBFAC*PHASEIR

        CALL BOXWSURF(AREAS,   REFLS,   DIFUS,  ABSPS,
     1                AREA2S,  REFL2S,  DIFU2S, ABSP2S,
     2                         REFLIRS, DIFUIRS,ABSPIRS,
     3                ABSNCFVI,ABSNCFIR,NCFVEC, ATTSURF,
     4                FORCE,   DUMPART, DUMPART,DUMPART)



C     NUMERICAL MODEL (CERES DATA)
      ELSEIF(ERM.EQ.2)THEN

        ABSSUN = DSQRT(SUN(1)**2 + SUN(2)**2 + SUN(3)**2)
        DO K=1,3
          ESUN(K) = SUN(K)/ABSSUN
        ENDDO

        DO LATK = 1,LATKMX
          DO LONK = 1,LONKMX

            D_AREA  = D_AREA_ALL(LATK,LONK)
            V_NS(1) = V_NS_ALL(LATK,LONK,1)
            V_NS(2) = V_NS_ALL(LATK,LONK,2)
            V_NS(3) = V_NS_ALL(LATK,LONK,3)

            CALL DMLMTV(V_NS,KAPPA,V_INS)

C           Distance and direction from point in the Earth to satellite
            V_DIST(1) = YSAT(1)-TOA*V_INS(1)
            V_DIST(2) = YSAT(2)-TOA*V_INS(2)
            V_DIST(3) = YSAT(3)-TOA*V_INS(3)
            DIST2     = V_DIST(1)**2 +V_DIST(2)**2 +V_DIST(3)**2
            ABSDIST   = DSQRT(DIST2)
            V_SAT(1)  = V_DIST(1)/ABSDIST
            V_SAT(2)  = V_DIST(2)/ABSDIST
            V_SAT(3)  = V_DIST(3)/ABSDIST

C           Cosine of angles of incident and reflected radiation
            CALL SPROD(ESUN,V_INS,COS_IN,DUMMY1,DUMMY2)
            CALL SPROD(V_SAT,V_INS,COS_RE,DUMMY1,DUMMY2)


            IF(COS_RE.GE.0)THEN

C             Reflectivity and emissivity coefficients
              REFL_CF = CERGRE(LATK,LONK)
              EMIT_CF = CERGEM(LATK,LONK)

C             Reflected Irradiance
              IF(COS_IN.GE.0)THEN
                E_REFL=(REFL_CF/(PI*DIST2))*COS_RE*COS_IN*S0*D_AREA
              ELSE
                E_REFL=0D0
              ENDIF

C             Emitted Irradiance
              E_EMIT = (EMIT_CF/(4*PI*DIST2))*COS_RE*S0*D_AREA

C             Non-conservative force
              ABSNCFVI  = E_REFL/C
              ABSNCFIR  = E_EMIT/C
              NCFVEC(1) = V_SAT(1)
              NCFVEC(2) = V_SAT(2)
              NCFVEC(3) = V_SAT(3)

              CALL BOXWSURF(AREAS,   REFLS,   DIFUS,  ABSPS,
     1                      AREA2S,  REFL2S,  DIFU2S, ABSP2S,
     2                               REFLIRS, DIFUIRS,ABSPIRS,
     3                      ABSNCFVI,ABSNCFIR,NCFVEC, ATTSURF,
     4                      FORCE_LL,DUMPART, DUMPART,DUMPART)

              FORCE(1) = FORCE(1) + FORCE_LL(1)
              FORCE(2) = FORCE(2) + FORCE_LL(2)
              FORCE(3) = FORCE(3) + FORCE_LL(3)
            ENDIF

          ENDDO
        ENDDO

      ENDIF

C     NAVIGATION ANTENNA THRUST (SIMPLE MODEL)
      IF((ANT.EQ.1).AND.(BLKNUM.LT.100))THEN
        ANTFORCE = 80D0/C
        FORCE(1) = FORCE(1) + ANTFORCE*RADVEC(1)
        FORCE(2) = FORCE(2) + ANTFORCE*RADVEC(2)
        FORCE(3) = FORCE(3) + ANTFORCE*RADVEC(3)
      ENDIF

      IF((REFF.GT.0).AND.((ERM.GT.0).OR.(ANT.EQ.1)))THEN
        DO K=1,3
          FREF(K) = 0D0
        ENDDO

C       FORCE IN BODY-FIXED REFERENCE FRAME
        IF(REFF.EQ.1)THEN
          DO K=1,3
            FREF(1) = FREF(1) + FORCE(K)*Z_SAT(K)
            FREF(2) = FREF(2) + FORCE(K)*Y_SAT(K)
            FREF(3) = FREF(3) + FORCE(K)*X_SAT(K)
          ENDDO

C       FORCE IN SUN-FIXED REFERENCE FRAME
        ELSEIF(REFF.EQ.2)THEN
          DO K=1,3
            FREF(1) = FREF(1) + FORCE(K)*D_SUN(K)
            FREF(2) = FREF(2) + FORCE(K)*Y_SAT(K)
            FREF(3) = FREF(3) + FORCE(K)*B_SUN(K)
          ENDDO

C       FORCE IN ORBITAL REFERENCE FRAME
        ELSEIF(REFF.EQ.3)THEN
          DO K=1,3
            FREF(1) = FREF(1) + FORCE(K)*RADVEC(K)
            FREF(2) = FREF(2) + FORCE(K)*ALGVEC(K)
            FREF(3) = FREF(3) + FORCE(K)*CRSVEC(K)
          ENDDO
        ENDIF

        DO K=1,3
          FORCE(K) = FREF(K)
        ENDDO
      ENDIF

C     CONVERSION TO ACCELERATION
      DO K=1,3
        FORCE(K) = FORCE(K)/MASS
      ENDDO


      END SUBROUTINE
      END MODULE
