      MODULE s_ROCKMD
      CONTAINS

C*
      SUBROUTINE ROCKMD(TMJD,NUMSVN,SAT,SUN,E2,USAT,BETA0,U0,ACCEL,
     1                  SCLFAC,NONRPR)
CC
CC NAME       :  ROCKMD
CC
CC PURPOSE    :  COMPUTE ACCELERATION DUE TO RADIATION PRESSURE
CC               ACCORDING TO ROCK4 (BLOCK-I), RESP. ROCK42 (BLOCK-II)
CC               SATELLITES
CC
CC PARAMETERS :
CC       IN   :  TMJD   : TIME IN MODIFIED JULIAN DATE          R*8
CC               NUMSVN : NUMBER OF SATELLITE                   I*4
CC               SAT    : SATELLITE POSITION                    R*8(*)
CC               SUN    : POSITION OF SUN                       R*8(*)
CC               E2     : UNIT VECTOR IN DIRECTION OF           R*8(*)
CC                        SOLAR PANELS AXES
CC               USAT   : ARG. OF LATITUDE OF THE SATELLITE     R*8
CC               BETA0  : ELEVATION OF SUN ABOVE ORBITAL PLANE  R*8
CC               U0     : ARG. OF LATITUDE OF THE SUN           R*8
CC                        IN THE ORBITAL PLANE
CC    IN/OUT  :  ACCEL  : RESULTING ACCELERATION (UPDATED)      R*8(*)
CC               SCLFAC : SCALING FACTOR FOR DIFFERENT RPR-     R*8
CC                        MODELS
CC               NONRPR : Accelerations other than solar RPR    R*8(3)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G. BEUTLER
CC
CC VERSION    :  3.4  (JAN 94)
CC
CC CREATED    :  92/05/23
CC
CC CHANGES    :  12-JUN-92 : ??: MODULO OF SATELLITE NUMBER DUE TO SHIFTED
CC                               SATELLITES HAVING NUMBER ISVN+50
CC               20-JUN-92 : ??: USE SUBROUTINE "GTSATR" TO READ SATELLITE
CC                               RAD.PRESS. PARAMETERS
CC               16-NOV-92 : ??: DIRECT RAD.PRESSURE WAS NOT OK IF SATEL.
CC                               WAS IN ECLIPSE (DUE TO BAD E2-VECTOR)
CC               10-AUG-94 : MR: CALL EXITRC
CC               08-MAR-95 : GB: RPR MODELS Z, S, T ACCOMODATED
CC                               NEW VARIABLE ANLTYP FROM RDANLT
CC                               NEW VARIABLE SCLFAC TO CALLING ROUTINE
CC                               (USUALLY SR DEQRHS)
CC                               FOR OLD SATELLITE INFO FILE : CALL GTSATR
CC                               FOR NEW    ..      ..   ..  : CALL GTSTR2
CC                               *** RCKMD2 *** IS A MODIFICATION OF ROCKMD.
CC                               WHICH WILL BE NO LONGER USED
CC               08-JAN-96 : MR: NEW GTSATR, ANALYSIS TYPE = 'OLD'
CC               29-APR-97 : HH: ADD GLONASS, ALLOW IROCKM=0
CC               31-JUL-97 : TS: NEW MODEL FOR BLOCK-IIR SATELLITES (T30)
CC               19-SEP-97 : MR: USE I:MAXSAA
CC               29-JUN-98 : TS: ADDED CODE-MODEL
CC               21-AUG-00 : MR: ADD TIME INFORMATION TO PARAMETERS,
CC                               TIME INTERVALS FOR SATELLITE INFO FILE
CC               17-FEB-03 : LM: USE M_MAXDIM
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               08-Aug-05 : HB: Use new SR TIMST2 (module)
CC               21-SEP-05 : HU: BLOCK 4 NOW INCLUDING 5 AND 6
CC               28-SEP-05 : HU: BLOCK 4 IS DEFAULT
CC               17-NOV-05 : HU: 3*USAT-U0 REPLACED BY 3*(USAT-U0)
CC               29-DEC-05 : HU: RELAX COMPARISON OF EPOCH RANGE
CC               21-SEP-06 : HU: NEW PLONER MODEL
CC               09-OCT-06 : AG: USE MODSVN(NUMSVN) FOR PRN2SVN CALL
CC               16-OCT-06 : AG: TAKE AU FROM CONST.
CC               17-OCT-06 : MP: NEW GTSATR, RPR PARAMETERS TAKEN FROM
CC                               SATELLITE FILE
CC               22-NOV-06:  MP: BUG CORRECTED IF NO RPR MODEL AVAIABLE
CC                               TEST FOR CORRECT NUMBER OF RPR PARAMETERS
CC               12-SEP-07 : RD: INIT DS2=0D0
CC               10-JUL-08 : DT: ADD SAT CHARACTERISTICS TO GTSATR
CC               11-JUL-08 : DT: ADD RPRMODEL=3 (D0, ALONG-TRACK) -> LAGEOS
CC               17-JUL-08 : DT: NON-RPR ACCELERATIONS SEPARATED (NONRPR)
CC               03-JUN-10 : HU: READ ORBITAL PLANE
CC               02-SEP-10 : CR: READ SPACE VEHICLE NUMBER
CC               04-MAY-12 : RD: USE M_BERN WITH ONLY
CC               04-MAY-12 : RD: REMOVE UNUSED LABELS, VARIABLES AND MODULES
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1992     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnerr
      USE d_const,  ONLY: AU, C
      USE d_satfil, ONLY: t_rprmod
      USE s_gtsatr
      USE f_modsvn
      USE s_vprod
      USE s_timst2
      USE s_exitrc
      USE s_rswvec
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 K     , NUMSVN, ADMOD
C
      REAL*8    B     , B02   , B04   , BETA0 ,
     1          COSB  , DIST  , DNORM , B06   ,
     2          FRCSUN, RAD   , SCAL  , SCLFAC, SINB  , TMJD  ,
     3          U0    , U3    , USAT  , U1    ,
     4          FORMF , C_RPR , ADRAG
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*8  ANLTYP
      CHARACTER*3  SVNNR
      CHARACTER*3  PLANE
C
      REAL*8       SUN(*),SAT(*),E2(*),ACCEL(*),NONRPR(3)
      REAL*8       MASS,FORCE(5),E1(3),E3(3),EX(3),ESUN(3)
      REAL*8       E_V(3),VEL
      REAL*8       D0,Y0,B0
      REAL*8       Z1,X1,X3
      REAL*8       Z1C2,Z1S2,Z1C4,Z1S4,Z1C6
      REAL*8       X1C2,X1S2
      REAL*8       X3C2,X3S2,X3C4,X3S4
      REAL*8       DC2,DC4,DS2,YC2,BC2,BC4
      REAL*8       F_AT

      REAL*8       C_SUN
C
      INTEGER*4    IBLOCK,IROCKM
C
      TYPE(T_RPRMOD) RPRMOD
C
C Initialize several values
C -------------------------
      FORMF = 1.D0
      C_RPR = 1.D0
      ADMOD = 0
      ADRAG = 0.D0
C solar constant
      C_SUN = 1380.D0
C
C GET RPR PARAMETERS
C ------------------
      CALL GTSATR(1,MODSVN(NUMSVN),TMJD,RPRMOD,SVNNR,IBLOCK,MASS,ANLTYP,
     1            PLANE,FORMF,C_RPR,ADMOD,ADRAG)
C
C COMPUTE ANGLE B = ANGLE BETWEEN POS Z AXIS AND DIRECTION TO THE SUN
C -------------------------------------------------------------------
      SCAL=0.D0
      DIST=0.D0
      RAD=0.D0
      DO 60 K=1,3
        ESUN(K)=SUN(K)-SAT(K)
        SCAL=SCAL-SAT(K)*ESUN(K)
        DIST=DIST+ESUN(K)**2
        RAD=RAD+SAT(K)**2
60    CONTINUE
      DNORM=DSQRT(DIST*RAD)
      DIST=DSQRT(DIST)
      RAD=DSQRT(RAD)
      COSB=SCAL/DNORM
      SINB=DSQRT(1.D0-COSB**2)
      B=DATAN2(SINB,COSB)
      DO 61 K=1,3
        ESUN(K)=ESUN(K)/DIST
61    CONTINUE
C
C SCALING FACTOR FOR DIFFERENT ANAYSIS TYPES
C ------------------------------------------
      IF(ANLTYP(1:1).EQ.'O')THEN
        SCLFAC=1.D0
      ELSE
        SCLFAC=(AU/DIST)**2
      END IF
C
C COMPUTE FORCE IN BODY-FIXED SYSTEM
C ----------------------------------
      FORCE(1)=0.0D0
      FORCE(2)=0.0D0
      FORCE(3)=0.0D0
      FORCE(4)=0.0D0
      FORCE(5)=0.0D0
      FRCSUN=0.0D0
C
      IROCKM=RPRMOD%RPMODEL
C
C ROCK-T MODEL
C ------------
      IF (IROCKM.EQ.1) THEN
         IF (RPRMOD%NRPRCOE.NE.3) THEN
            WRITE(LFNERR,"(' *** SR ROCKMD: NUMBER OF RPR COEFFICIENTS'
     1           ' DO NOT MATCH MODELNUMBER:',/,
     2           '                SVN: ',I6,/,
     3           '                # RPR COEFF. EXPECTED:  3',/,
     4           '                # RPR COEFF. FOUND:   ',I3)")
     5           NUMSVN,RPRMOD%NRPRCOE
            CALL EXITRC(2)
         ENDIF
         IF (IBLOCK.EQ.1) THEN
            FORCE(1)=-0.01D0*SINB+0.08D0*DSIN(2*B+0.9D0)
     1               -0.06D0*DCOS(4*B+0.08D0)+0.08D0
            FORCE(3)= 0.20D0*DSIN(2*B-0.3D0)-0.03D0*DSIN(4*B)
            FRCSUN=-4.54D0
         ELSEIF (IBLOCK.EQ.2 .OR.
     1           IBLOCK.EQ.3) THEN
            FORCE(1)=-.265D0*SINB + .16D0*DSIN(3*B) + .10D0*DSIN(5*B)
     1                            - .07D0*DSIN(7*B)
            FORCE(3)= .265D0*COSB
            FRCSUN =-8.695D0
         ELSE
            FORCE(1)=-11.0D0*SINB - 0.2D0*DSIN(3*B) + 0.2D0*DSIN(5*B)
            FORCE(3)=-11.3D0*COSB + 0.1D0*DCOS(3*B) + 0.2D0*DCOS(5*B)
            FRCSUN= 0.D0
         ENDIF
C
C ROCK-S MODEL (FOR BLOCK-IIR SATELLITES TAKEN IDENTICAL AS T-MODEL)
C ------------------------------------------------------------------
      ELSEIF (IROCKM.EQ.2) THEN
         IF (RPRMOD%NRPRCOE.NE.3) THEN
            WRITE(LFNERR,"(' *** SR ROCKMD: NUMBER OF RPR COEFFICIENTS'
     1           ' DO NOT MATCH MODELNUMBER:',/,
     2           '                SVN: ',I6,/,
     3           '                # RPR COEFF. EXPECTED:  3',/,
     4           '                # RPR COEFF. FOUND:   ',I3)")
     5           NUMSVN,RPRMOD%NRPRCOE
            CALL EXITRC(2)
         ENDIF
         IF (IBLOCK.EQ.1) THEN
            FORCE(1)=+0.10D0*DSIN(2*B+1.1D0)
     1               -0.05D0*DCOS(4*B)+0.06D0
            FORCE(3)=+0.17D0*DSIN(2*B-0.4D0)-0.05D0*DSIN(4*B)
     1               -0.06D0
            FRCSUN=-4.34D0
         ELSEIF (IBLOCK.EQ.2 .OR.
     1           IBLOCK.EQ.3) THEN
            FORCE(1)=-0.15D0*SINB + .05D0*DCOS(2*B)
     1              - .056D0*DSIN(4*B+1.4D0) + 0.07D0
            FORCE(3)= 0.15D0*COSB + 0.024D0*DSIN(2*B-0.8D0)
     1              - .047D0*DSIN(4*B+0.9D0)+0.02D0
            FRCSUN=-7.95D0
         ELSE
            FORCE(1)=-11.0D0*SINB - 0.2D0*DSIN(3*B) + 0.2D0*DSIN(5*B)
            FORCE(3)=-11.3D0*COSB + 0.1D0*DCOS(3*B) + 0.2D0*DCOS(5*B)
            FRCSUN= 0.D0
         ENDIF
C
C Spherical satellites (e.g., Lageos)
C -----------------------------------
C
C FORCES: D0 = 4 and along track (E_V)
C --------------------------------------------
      ELSEIF (IROCKM.EQ.3) THEN
         IF (RPRMOD%NRPRCOE.NE.2) THEN
            WRITE(LFNERR,"(' *** SR ROCKMD: NUMBER OF RPR COEFFICIENTS'
     1           ' DO NOT MATCH MODELNUMBER:',/,
     2           '                SVN: ',I6,/,
     3           '                # RPR COEFF. EXPECTED:  3',/,
     4           '                # RPR COEFF. FOUND:   ',I3)")
     5           NUMSVN,RPRMOD%NRPRCOE
            CALL EXITRC(2)
         ENDIF

         D0=RPRMOD%RPRCOE(1)
         F_AT=(-1.D0)*RPRMOD%RPRCOE(2)
CC         F_R =RPRMOD%RPRCOE(3)  radial

         FORCE(4) = (-1.D0)*D0*FORMF*C_SUN/C
C
C SPRINGER MODEL (GPS)
C --------------------
      ELSEIF (IROCKM.EQ.8) THEN
         IF (RPRMOD%NRPRCOE.NE.19) THEN
            WRITE(LFNERR,"(' *** SR ROCKMD: NUMBER OF RPR COEFFICIENTS'
     1           ' DO NOT MATCH MODELNUMBER:',/,
     2           '                SVN: ',I6,/,
     3           '                # RPR COEFF. EXPECTED: 19',/,
     4           '                # RPR COEFF. FOUND:   ',I3)")
     5           NUMSVN,RPRMOD%NRPRCOE
            CALL EXITRC(2)
         ENDIF
C
C SET FORCES D,Y,B,X,Z = 4,2,5,1,3
C --------------------------------
         D0=RPRMOD%RPRCOE(1)
         DC2=RPRMOD%RPRCOE(4)
         DC4=RPRMOD%RPRCOE(5)
C
         Y0=RPRMOD%RPRCOE(2)
         YC2=RPRMOD%RPRCOE(6)
C
         B0=RPRMOD%RPRCOE(3)
         BC2=RPRMOD%RPRCOE(7)
         BC4=RPRMOD%RPRCOE(8)
C
         Z1=RPRMOD%RPRCOE(9)
         Z1C2=RPRMOD%RPRCOE(10)
         Z1S2=RPRMOD%RPRCOE(11)
         Z1C4=RPRMOD%RPRCOE(12)
         Z1S4=RPRMOD%RPRCOE(13)
C
         X1=RPRMOD%RPRCOE(14)
         X1C2=RPRMOD%RPRCOE(15)
         X1S2=RPRMOD%RPRCOE(16)
C
         X3=RPRMOD%RPRCOE(17)
         X3C2=RPRMOD%RPRCOE(18)
         X3S2=RPRMOD%RPRCOE(19)
C
         B02=2.0D0*BETA0
         B04=4.0D0*BETA0
C
         FORCE(4)=D0+DC2*DCOS(B02)+DC4*DCOS(B04)
         FORCE(2)=Y0+YC2*DCOS(B02)
         FORCE(5)=B0+BC2*DCOS(B02)+BC4*DCOS(B04)
C
         FORCE(1)=(X1+X1C2*DCOS(B02)+X1S2*DSIN(B02))*DSIN(USAT-U0)
     1           +(X3+X3C2*DCOS(B02)+X3S2*DSIN(B02))*DSIN(3*(USAT-U0))
         FORCE(3)=(Z1+Z1C2*DCOS(B02)+Z1S2*DSIN(B02)
     1           +Z1C4*DCOS(B04)+Z1S4*DSIN(B04))*DSIN(USAT-U0)
C
C SPRINGER MODEL (GLONASS)
C -----------------------------------------
      ELSEIF (IROCKM.EQ.7) THEN
         IF (RPRMOD%NRPRCOE.NE.12) THEN
            WRITE(LFNERR,"(' *** SR ROCKMD: NUMBER OF RPR COEFFICIENTS'
     1           ' DO NOT MATCH MODELNUMBER',/,
     2           '                SVN: ',I6,/,
     3           '                # RPR COEFF. EXPECTED: 12',/,
     4           '                # RPR COEFF. FOUND:   ',I3)")
     5           NUMSVN,RPRMOD%NRPRCOE
            CALL EXITRC(2)
         ENDIF
         D0=RPRMOD%RPRCOE(1)
         DC2=RPRMOD%RPRCOE(3)
         DS2=0D0
C
         Y0=RPRMOD%RPRCOE(2)
         YC2=RPRMOD%RPRCOE(4)
C
         Z1=RPRMOD%RPRCOE(5)
         Z1C2=RPRMOD%RPRCOE(6)
         Z1C6=RPRMOD%RPRCOE(7)
C
         X3=RPRMOD%RPRCOE(8)
         X3C2=RPRMOD%RPRCOE(9)
         X3S2=RPRMOD%RPRCOE(10)
         X3C4=RPRMOD%RPRCOE(11)
         X3S4=RPRMOD%RPRCOE(12)
C
C SET FORCES D,Y,B,X,Z = 4,2,5,1,3
C --------------------------------
         U1 =1.0D0*(USAT-U0)
         U3 =3.0D0*(USAT-U0)
         B02=2.0D0*BETA0
         B04=4.0D0*BETA0
         B06=6.0D0*BETA0
C
         FORCE(4)=D0+DC2*DCOS(B02)+DS2*DSIN(B02)
         FORCE(2)=Y0+YC2*DCOS(B02)
         FORCE(5)=0.0D0
         FORCE(1)=(X3+X3C2*DCOS(B02)+X3S2*DSIN(B02)
     1            +X3C4*DCOS(B04)+X3S4*DSIN(B04))*DSIN(U3)
         FORCE(3)=(Z1+Z1C2*DCOS(B02)+Z1C6*DCOS(B06))*DSIN(U1)
C
C JPL MODEL (GPS_XYZ)
C -------------------
      ELSEIF (IROCKM.EQ.9) THEN
         IF (RPRMOD%NRPRCOE.NE.3) THEN
            WRITE(LFNERR,"(' *** SR ROCKMD: NUMBER OF RPR COEFFICIENTS'
     1           ' DO NOT MATCH MODELNUMBER:',/,
     2           '                SVN: ',I6,/,
     3           '                # RPR COEFF. EXPECTED:  3',/,
     4           '                # RPR COEFF. FOUND:   ',I3)")
     5           NUMSVN,RPRMOD%NRPRCOE
            CALL EXITRC(2)
         ENDIF
         FORCE(1)=-8.7927900D0*SINB      - 0.00875225D0*DSIN(2*B)
     1            + 0.0508852D0*DSIN(3*B) + 0.01358980D0*DSIN(4*B)
     2            + 0.1873290D0*DSIN(5*B) - 0.00146316D0*DSIN(6*B)
     3            - 0.0496712D0*DSIN(7*B)
         FORCE(3)=-8.43D0*COSB
         FORCE(2)= (0.1D0+0.5D0*DSIN(BETA0)
     1           +0.3D0/DSIN(BETA0))*COSB*1.0D-9
         FRCSUN= 0.D0
C
      ELSEIF (IROCKM.EQ.0) THEN
         IF ((RPRMOD%NRPRCOE.NE.0).AND.(RPRMOD%NRPRCOE.NE.3)) THEN
            WRITE(LFNERR,"(' *** SR ROCKMD: NUMBER OF RPR COEFFICIENTS'
     1           ' DO NOT MATCH MODELNUMBER',/,
     2           '                SVN: ',I6,/,
     3           '                # RPR COEFF. EXPECTED: 0 or 3',/,
     4           '                # RPR COEFF. FOUND:   ',I3)")
     5           NUMSVN,RPRMOD%NRPRCOE
            CALL EXITRC(2)
         ENDIF
      ENDIF
C
C COMPUTE UNIT VECTORS E3, E1 AND EX
C ----------------------------------
      DO 70 K=1,3
        E3(K)=-SAT(K)/RAD
70    CONTINUE
      CALL VPROD(E2,E3,E1)
      DO 90 K=1,3
        E1(K)=-E1(K)
90    CONTINUE
      CALL VPROD(ESUN,E2,EX)
C
C ALONG-TRACK
C -----------
CCC         CALL RSWVEC(SAT,ER,ES,EW)
      VEL = SQRT(SAT(4)**2+SAT(5)**2+SAT(6)**2)
      DO K=1,3
        E_V(K) = SAT(K+3)/VEL
      END DO
C
C COMPUTE FORCE IN ORBIT SYSTEM
C -----------------------------
      DO 100 K=1,3
        IF ((IROCKM.EQ.8).OR.(IROCKM.EQ.7)) THEN
           ACCEL(K)=ACCEL(K)+(
     1          +FORCE(1)*E1(K)
     2          +FORCE(2)*E2(K)
     3          +FORCE(3)*E3(K)
     4          +FORCE(4)*ESUN(K)
     5          +FORCE(5)*EX(K))*SCLFAC
C
        ELSE IF (IROCKM.EQ.3) THEN
C Solar radiation Pressure force
           ACCEL(K)=ACCEL(K)+(
     1          +FORCE(4)*ESUN(K)*SCLFAC)
C
C Other forces than solar radiation pressure
           NONRPR(K)=NONRPR(K)+F_AT*E_V(K)
C
        ELSE IF (IROCKM.NE.0) THEN
           ACCEL(K)=ACCEL(K)+((FORCE(1)*E1(K)
     1                       +FRCSUN*ESUN(K)
     2                       +FORCE(3)*E3(K))/MASS*1.D-5
     3                       +RPRMOD%RPRCOE(1)*ESUN(K)
     4                       +RPRMOD%RPRCOE(2)*E2(K)
     5                       +RPRMOD%RPRCOE(3)*EX(K))*SCLFAC
     6                       -FORCE(2)*E2(K)
        ELSE IF (RPRMOD%NRPRCOE.EQ.3 .AND. IROCKM.NE.3) THEN
           ACCEL(K)=ACCEL(K)+(
     1                       RPRMOD%RPRCOE(1)*ESUN(K)
     2                      +RPRMOD%RPRCOE(2)*E2(K)
     3                      +RPRMOD%RPRCOE(3)*EX(K))*SCLFAC
        END IF
100   CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
