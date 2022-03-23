      MODULE s_GETSP3
      CONTAINS

C*
      SUBROUTINE GETSP3(SVN,IANT,IDER,ISTOP,T,X,
     1                  TOSC,ELE,CLKCOR,IRCODE)
CC
CC NAME       :  GETSP3
CC
CC PURPOSE    :  COMPUTE POSITION (AND DERIVATIVES UP TO IDER)
CC               OF SATELLITE WITH NUMBER SVN AND STORE THEM IN ARRAY X.
CC               MOREOVER RETURN THE OSCULATION EPOCH AND ELEMENTS OF
CC               CURRENT ARC. THE SUBROUTINE ACCESSES THE SP3-FILES.
CC               IN ADDITION THE GPS SATELLITE CLOCK CORRECTIONS ARE
CC               CALCULATED BY LINEAR INTERPOLATION.
CC               THE SUBROUTINE IS BASED ON THE SR GETORB ACCESSING THE
CC               BERNESE STANDARD ORBITS
CC
CC PARAMETERS :
CC         IN :  SVN    : SVN-NUMBER OF SATELLITE             I*4
CC                        IF SVN < 0 THEN ALWAYS RETURN POS-
CC                        ITION OF THE SATELLITE, NOT OF
CC                        SVN+50 IF A MANOEUVRE HAPPENED
CC               IANT   : ANTENNA OFFSET CORRECTION FLAG      I*4
CC                        =0 : NO CORRECTION (CENTER OF MASS)
CC                        =1 : ANTENNA OFFSET CORRECTION
CC                        =2 : SLR REFLECTOR OFFSET CORRECTION
CC               IDER   : NUMBER OF DERIVATIVES TO BE COMP.   I*4
CC                        =0 : POSITION IS COMPUTED
CC                        =1 : POS.+VELOCITY IS COMPUTED
CC                        =2 : POS.+VEL.+ACCELEARATION
CC                          E. T. C.
CC               ISTOP  : FLAG FOR STOP ON ERROR              I*4
CC                        =0 : NO STOP, RETURN CODE SET
CC                        =1 : STOP ON ERROR
CC                        =2 : NO STOP, IF SATELLITE MISSING.
CC                             PRINT WARNING ONCE PER SATEL.
CC               T      : TIME IN MODIFIED JULIAN DATE        R*8
CC        OUT :  X(I),I=1,2,3, 4,5,6, ... POSITION (M),       R*8
CC                      VELOCITY (M/S), ACC(M/S**2), ...
CC               TOSC   : OSCULATION EPOCH FOR ELEMENTS       R*8
CC               ELE(I),I=1,2,..,7 OSCULATING ELEMENTS        R*8
CC               CLKCOR : CLOCK CORRECTION FOR SAT ISVN       R*8
CC               IRCODE : RETURN CODE                         I*4
CC                        =0: OK
CC                        =1: NO ARC FOUND FOR TIME "T"
CC                        =2: ARC FOUND, BUT NO SATELLITE
CC                            "SVN" IN THIS ARC
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  5.0  (MAR 2003)
CC
CC CREATED    :  03/03/03 08:30
CC
CC CHANGES    :  11-APR-05 : CU: Add check NSCLK(ISVN).GT.0
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               08-AUG-05 : HB: USE NEW SR TIMST2 (MODULE)
CC               09-NOV-05 : AG: SENNUM FOR GTSATA CALL ADDED
CC               30-MAY-07 : AG: USE S_SUNEFF
CC               26-FEB-08 : RD: USE GTSATM FROM D_SATCRX
CC               04-MAY-08 : RD: SVN ADDED TO CALL OF SR XYZELE
CC               01-OCT-10 : CR: NEW CALL OF SUNEFF
CC               05-MAR-12 : RD: USE GETOBS AS MODULE
CC               05-MAR-12 : RD: USE LISTI4 AS MODULE NOW
CC               08-NOV-12 : RD: INIT ARRAYS BEFORE READING RECORDS
CC               08-NOV-12 : RD: NO RECORDS FOR A SATELLITE IN SP3
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      2003     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnerr, staNam2Length
      USE m_maxdim, ONLY: MAXSAT,MAXSAA
      USE d_const,  ONLY: GM
      USE d_satcrx, ONLY: gtsatm
      USE d_clkrnx, ONLY: undef
      USE s_suneff
      USE s_getobs
      USE s_exitrc
      USE f_listi4
      USE s_xyzele
      USE s_vprod
      USE s_itpole
      USE s_polevn
      USE s_gtsata
      USE s_timst2
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IALL  , IANT  , IDER  , IEP   , IEPO  , IFIRST, IFLAG ,
     1          IFRST , IGTANT, II    , ILIST , IMANOK, IORSYS, IRCODE,
     2          ISAANT, ISAT  , ISTOP , ISVN  , ITYPFL, K     ,
     3          MAXEPO, MAXMAN, MAXPOL, MXCEPO, MXCFIL, MXCSAT, NMAN  ,
     4          NMIS  , NSAANT, NSAT
C
      REAL*8    AA    , CLKCOR, DELTAT, EE    , EXA   , H     ,
     1          HSAVE , PER   , REY   , RSAT  ,
     2          T     , T0ARC , TDT   , TOSC  , TPER  , TREL  , U0    ,
     3          XI    , XKN   , XM0   , XN0
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      PARAMETER (MAXEPO=800,MAXPOL=15,MAXMAN=100)
C
      REAL*8       X(*),ELE(7)
      REAL*8       ELESAT(7,MAXSAT),ANTOFF(6,MAXSAA)
      REAL*8       TIMMAN(MAXMAN)
      REAL*8       EX(3),EY(3),EZ(3),XSUN(4),DUM3(3)
      REAL*8       TIMINT(2,MAXSAA)
      REAL*8       RMSRAT(2)
      REAL*8       EPOSP3(MAXEPO,MAXSAT),POSSP3(3,MAXEPO,MAXSAT)
      REAL*8       TOSCS(MAXSAT),COE(3,MAXPOL+1,MAXSAT),XVH(3,2)
      REAL*8       CLOCKC(MAXEPO,MAXSAT),TSCLK(MAXEPO,MAXSAT)
      REAL*8       RMSMIN(2),RMSOBS(MAXEPO),DTMOBS(MAXEPO)
C
      INTEGER*4    Q,SVN,NAVNUM(MAXSAT),SATMAN(MAXMAN)
      INTEGER*4    SATANT(MAXSAA)
      INTEGER*4    NEPSAT(MAXSAT),NSCLK(MAXSAT)
      INTEGER*4    IFSAVE(MAXSAT),IFCLK(2,MAXSAT)
      INTEGER*4    SAVSVN(MAXSAT)
      INTEGER*4    SATBLK(MAXSAA),SENNUM(MAXSAA)
      INTEGER*4    OBSTYP(MAXEPO)
C
      CHARACTER*20 STRING
      CHARACTER*16 SENSOR
      CHARACTER(LEN=staNam2Length),DIMENSION(2,MAXSAA) :: SATNAM
      CHARACTER*6  MXNSAT,MXNFIL,MXNEPO
C
C COMMON BLOCKS
C -------------
      COMMON/MCMFIL/MXCFIL,MXNFIL
      COMMON/MCMSAT/MXCSAT,MXNSAT
      COMMON/MCMEPO/MXCEPO,MXNEPO
C
      COMMON/TORIGO/T0ARC
      COMMON/ORBSYS/IORSYS
C
      DATA IFIRST/1/,IGTANT/1/
C
C DISREGARD MANOEUVRES IF SVN < 0
C -------------------------------
      MXCSAT=MAXSAT
      MXCEPO=MAXEPO
      IF (SVN.LT.0) THEN
        SVN=-SVN
        IMANOK=0
      ELSE
        IMANOK=1
      ENDIF
C
C GET SATELLITE ANTENNA OFFSETS (FIRST TIME WHEN ANTENNA OFFSETS NEEDED)
C ----------------------------------------------------------------------
      IF (IANT.NE.0.AND.IGTANT.EQ.1) THEN
        CALL GTSATA(MAXSAA,NSAANT,SATANT,ANTOFF,TIMINT,SATNAM,SATBLK,
     1                                                          SENNUM)
        IGTANT=0
      ENDIF
C
C INITIIALIZE PROCEDURE
C ---------------------
      IF(IFIRST.EQ.1) THEN
        IFIRST=0
C
C MISSING SATELLITES
        NMIS=0
C
C GET SATELLITE MANOEUVRES
        CALL GTSATM(MAXMAN,NMAN,SATMAN,TIMMAN)
C
C READ TABULAR POSITIONS
        IALL=-99
        ITYPFL=1
        RMSRAT(1)=0.D0
        RMSRAT(2)=0.D0
        MXCEPO=MAXEPO
        EPOSP3=0.D0
        POSSP3=0.D0
        CLOCKC=UNDEF
        CALL GETOBS(IALL,ITYPFL,RMSRAT,RMSMIN,NSAT,NAVNUM,NEPSAT,
     1              EPOSP3,POSSP3,CLOCKC,RMSOBS,DTMOBS,OBSTYP,
     2              SENSOR,DELTAT)
C
C COMPUTE OSCULATING ELEMENTS FOR ALL SATELLITES USING FIRST EPOCH
C AS OSCULATION EPOCH
        Q=10
        DO ISAT=1,NSAT
          H = (EPOSP3(Q+1,ISAT)-EPOSP3(1,ISAT))*86400
          CALL ITPOLE(Q,3,H,POSSP3(1,1,ISAT),COE(1,1,ISAT),IFLAG)
C
          TREL = 0.D0
          CALL POLEVN(IDER+1,Q,3,TREL,1.D0,COE(1,1,ISAT),XVH)
          CALL XYZELE(GM,0.D0,XVH,XVH(1,2),ABS(SVN),
     2                AA,EE,XI,XKN,PER,TPER)
C
C COMPUTE U0 (ARG.OF LATITUDE AT T0)
          XN0=SQRT(GM/AA**3)
          XM0=XN0*(0.D0-TPER)
          EXA=XM0
          DO K=1,10
            EXA = XM0+EE*SIN(EXA)
          ENDDO
          U0=2.D0*ATAN( SQRT((1+EE)/(1-EE))*TAN(EXA/2) )
C
C STORE ELEMENTS
          ELESAT(1,ISAT)=AA
          ELESAT(2,ISAT)=EE
          ELESAT(3,ISAT)=XI
          ELESAT(4,ISAT)=XKN
          ELESAT(5,ISAT)=PER
          ELESAT(6,ISAT)=U0
          ELESAT(7,ISAT)=TPER
C
          TOSCS(ISAT)=EPOSP3(1,ISAT)
C
          HSAVE=H
C
          IFSAVE(ISAT)=1
C
C GET RID OF "BAD" CLOCK EPOCHS
C -----------------------------
          NSCLK(ISAT)=0
          DO IEPO=1,NEPSAT(ISAT)
            IF(CLOCKC(IEPO,ISAT).LT.1.D4)THEN
              NSCLK(ISAT)=NSCLK(ISAT)+1
              TSCLK(NSCLK(ISAT),ISAT)=EPOSP3(NSCLK(ISAT),ISAT)
              CLOCKC(NSCLK(ISAT),ISAT)=CLOCKC(IEPO,ISAT)
            ENDIF
          ENDDO
          IFCLK(1,ISAT)=1
          IFCLK(2,ISAT)=2
C
        ENDDO
      END IF
C
C END OF INITIALIZATION
C -------------------------------------------------------------------
C
C START NORMAL PROCESSING
C ***********************
C
C SATELLITE INDEX
C ---------------
      DO ISVN=1,NSAT
        IF(SVN.EQ.NAVNUM(ISVN).AND.NEPSAT(ISVN).GT.0)THEN
          GO TO 10
        ENDIF
      ENDDO
      II = LISTI4(0,maxSat,SAVSVN,SVN,ILIST)
      IF (II==0) THEN
        WRITE(LFNERR,5)SVN
5       FORMAT(' *** SR GETSP3: SVN',I3,' NOT IN SP3-FILE',//)
        IFLAG=2
        II = LISTI4(1,maxSat,SAVSVN,SVN,ILIST)
      ENDIF
      IRCODE=2
      IF(ISTOP.EQ.1) THEN
        CALL EXITRC(2)
      ELSE
        RETURN
      ENDIF
10    CONTINUE
C
C BEST INTERPOLATION EPOCHS
C -------------------------
      IFRST=(T-0.5*HSAVE/86400.d0-TOSCS(ISVN))*86400.d0/(HSAVE/Q)+1
C
      IF(IFRST.LT.1)IFRST=1
      IF(IFRST+Q.GT.NEPSAT(ISVN))IFRST=NEPSAT(ISVN)-Q
C
C NEW SUB-INTERVAL
      IF(IFRST.NE.IFSAVE(ISVN))THEN
        CALL ITPOLE(Q,3,HSAVE,POSSP3(1,IFRST,ISVN),COE(1,1,ISVN),IFLAG)
        IFSAVE(ISVN)=IFRST
C       WRITE(*,*)'SVN,IFIRST=',SVN,IFRST
      ENDIF
C
C CALCULATE POSITION VECTOR PLUS DERIVATIVES
C ------------------------------------------
      TREL = (T-EPOSP3(IFRST,ISVN))*86400.d0
cc      WRITE(*,*)'SVN,T,TREL=',SVN,T,TREL/h
      CALL POLEVN(IDER+1,Q,3,TREL,1.D0,COE(1,1,ISVN),X)
C
C SATELLITE ANTENNA OFFSET CORRECTIONS
C ------------------------------------
      IF (IANT.NE.0) THEN
        DO 162 ISAANT=1,NSAANT
          IF (SATANT(ISAANT).EQ.SVN.AND.T.GE.TIMINT(1,ISAANT).AND.
     1                T.LE.TIMINT(2,ISAANT)) GOTO 163
162     CONTINUE
C
C SATELLITE ANTENNA OFFSET NOT FOUND
        CALL TIMST2(1,1,T,STRING)
        WRITE(LFNERR,1006) SVN,STRING
1006    FORMAT(/,' *** SR GETSP3: SATELLITE ANTENNA OFFSET',
     1           ' NOT FOUND',/,
     2           16X,'IN SATELLITE INFORMATION FILE',/,
     3           16X,'SATELLITE NUMBER:',I4,/,
     4           16X,'EPOCH           : ',A20,/)
        CALL EXITRC(2)
163     CONTINUE
C
C UNIT VECTOR EZ
        RSAT=DSQRT(X(1)**2+X(2)**2+X(3)**2)
        DO 164 K=1,3
          EZ(K)=-X(K)/RSAT
164     CONTINUE
C
C UNIT VECTOR EY
        TDT=T+(19.D0+32.184D0)/86400.D0
        CALL SUNEFF(2,2.D0,TDT,XSUN,DUM3)
        CALL VPROD(EZ,XSUN,EY)
        REY=DSQRT(EY(1)**2+EY(2)**2+EY(3)**2)
        DO 165 K=1,3
          EY(K)=EY(K)/REY
165     CONTINUE
C
C UNIT VECTOR EX
        CALL VPROD(EY,EZ,EX)
C
C ANTENNA OFFSET CORRECTION
        DO 161 K=1,3
          IF(IANT.EQ.1) THEN
            X(K)=X(K)+EX(K)*ANTOFF(1,ISAANT)
     1               +EY(K)*ANTOFF(2,ISAANT)
     2               +EZ(K)*ANTOFF(3,ISAANT)
C
C SLR REFLECTORS
          ELSEIF(IANT.EQ.2) THEN
            X(K)=X(K)+EX(K)*ANTOFF(4,ISAANT)
     1               +EY(K)*ANTOFF(5,ISAANT)
     2               +EZ(K)*ANTOFF(6,ISAANT)
          END IF
161     CONTINUE
      ENDIF
C
C DEFINE PROPER SET OF ELEMENTS
      TOSC=TOSCS(ISVN)
      DO K=1,7
        ELE(K)=ELESAT(K,ISVN)
      ENDDO
C
C LINEARLY INTERPOLATE SATELLITE CLOCK CORRECTION
C -----------------------------------------------
C
C CURRENT TIME IS WITHIN OLD SUBINTERVAL?
      IF(T.LT.EPOSP3(IFCLK(1,ISVN),ISVN).OR.
     1   T.GT.EPOSP3(IFCLK(2,ISVN),ISVN))THEN
          DO IEP=2,NSCLK(ISVN)
            IF(T.LT.EPOSP3(IEP,ISVN))THEN
              IFCLK(1,ISVN)=IEP-1
              IFCLK(2,ISVN)=IEP
              EXIT
            ENDIF
          ENDDO
C_cu
          IF (NSCLK(ISVN).GT.0) THEN
            IF(T.GE.EPOSP3(NSCLK(ISVN),ISVN))THEN
              IFCLK(1,ISVN)=NSCLK(ISVN)-1
              IFCLK(2,ISVN)=NSCLK(ISVN)
            ENDIF
          ENDIF
C_cu
        ENDIF
C
C INTERPOLATE CLOCK
C -----------------
C_cu
        IF (NSCLK(ISVN).GT.0) THEN
          CLKCOR =
     1      CLOCKC(IFCLK(1,ISVN),ISVN)+(T-EPOSP3(IFCLK(1,ISVN),ISVN))/
     2      (TSCLK(IFCLK(2,ISVN),ISVN)-TSCLK(IFCLK(1,ISVN),ISVN))*
     3      (CLOCKC(IFCLK(2,ISVN),ISVN)-CLOCKC(IFCLK(1,ISVN),ISVN))
        ENDIF
C_cu
C
C SET RETURN CODE
      IRCODE=0
C
      RETURN
      END SUBROUTINE

      END MODULE
