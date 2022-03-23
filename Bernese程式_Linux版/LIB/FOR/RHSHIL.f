      MODULE s_RHSHIL
      USE M_EPOCH,  ONLY: OPERATOR(+)
      CONTAINS

C*
      SUBROUTINE RHSHIL(FRCTYP,H     ,T0    ,TOSC  ,LOCHIL,IORSYS,
     1           ELE   ,XN    ,NPTS  ,TIMREL,XNORM ,RSW   ,RHS   )
CC
CC NAME       :  RHSHIL
CC
CC PURPOSE    :  COMPUTE RIGHT HAND SIDES OF HILL' EQQNS OF MOTION
CC               FRCTYP=13 : PARTIALS WITH RESPECT TO EARTH POTENTIAL
CC                           PARAMETERS
CC                     =14 : RESONANCE TERMS
CC                     =15 : EARTH ALBEDO MODEL
CC                     =99 : SUBROUTINE IS SKIPPED
CC                     =OTHER VALUES : ERROR MESSAGE, STOP EXECUTION
CC
CC PARAMETERS :
CC         IN :  FRCTYP : FORCE TYPE (SEE ABOVE)              I*4
CC               H      : LENGTH OF INTEGRATION INTERVAL      R*8
CC               T0     : ORIGIN OF SERIES DEVELOPMENT        R*8
CC               TOSC   : OSCULATION EPOCH (MJD)              R*8
CC               LOCHIL : PARAMETER DESCRIPTION               I*4(*)
CC               IORSYS : REFERENCE SYSTEM OF ELEMENTS        I*4
CC                        =1 : SYSTEM 1950.0
CC                        =2 : J2000.0
CC               ELE    : OSCULATING ELEMENTS                 R*8(*)
CC               NPTS   : NUMBER OF POINTS IN ARRAY RHS       I*4
CC               TIMREL : TIMES OF REQUEST (MJD)              R*8(*)
CC               XNORM  : NORMALIZATION FACTOR FOR DERIVS.    R*8
CC               RSW    : AUXILIARY ARRAY                     R*8(3,*)
CC       OUT  :  RHS    : RIGHT HAND SIDES OF HILL'S EQNS     R*8(3,*)
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
CC               05-JUN-96 : TS: CALL POLDEF CHANGED DUE TO SUBDAILY POLE
CC               15-JAN-03 : PS: USE NUTMTX INSTEAD OF NUTN20
CC                               CONSISTENCY CHECK NUTATION MODELS
CC               08-MAR-03 : HU: INTERFACE FOR NUTMTX ADDED
CC               06-JUN-03 : HU: FILPOL, DUMMYC, TYPTMP DECLARED
CC               11-JUN-03 : HU: USE GSTIME
CC               14-JUN-03 : HU: DESACTIVATE USE OF GMST2000
CC               06-AUG-03 : HU: NEW CALL FOR GSTIME
CC               16-JUN-05 : MM: COMCONST.inc REPLACED BY d_const
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               07-JUL-05 : HB: NEW PARAMETER FOR NUTMTX, TYPE T_EPOCH
CC                               USED FOR GSTIME
CC               21-JAN-06 : HU: CALL FOR NUTMTX CHANGED
CC               16-OCT-06 : AG: TAKE AU FROM CONST.
CC
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE M_BERN
      USE M_EPOCH,  ONLY: t_epoch, OPERATOR(.realToEpoch.)
      USE d_const, ONLY: AE, GM, PI, AU
      USE s_rdpolh
      USE s_dmlmtv
      USE s_opnfil
      USE f_gstime
      USE s_nutmtx
      USE s_poldef
      USE s_prenew
      USE s_nutnew
      USE s_exitrc
      USE f_fakult
      USE s_ephem
      USE s_sidmat
      USE s_gtflna
      USE s_opnerr
      USE s_sun
      USE s_dmlmav
      USE s_pren20
      USE s_ddreh
      USE s_sun20
      USE s_dvdxyz
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IEND  , IFIRST, IFORM , IND   , IORSYS, IOSTAT, IPTS  ,
     1          IRC   , K     , M     , MLONG , MP    , N     , NLAT  ,
     2          NPTS
C
      REAL*8    ARG   , BB    , COSB  , COSSUN, COSZEN, CPHI  , DSURF ,
     1          DSURFE, E     , EQEQUI, EX    , GPSUTC,
     2          H     , HH    , HI    , HI1   , PNM   , PNM1  , PNM2  ,
     3          POL   , R     , RR    , RTOP  , SIDTIM, SINB  , SPHI  ,
     4          T0    , TANB  , TIMMJD, TIMSEC, TOSC  , TDT   ,
     5          TPER  , TPRN  , UT1GPS, UT1UTC, V     , XITER , XKNTER,
     6          XL    , XLL   , XM    , XN    , XNORM , XR    , YR
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
      DATA IFIRST/1/
C
      CHARACTER*16 NUTNAM, NUTNA2, SUBNAM
      CHARACTER(LEN=fileNameLength)   :: FILPOL
      CHARACTER*80 DUMMYC
C
      INTEGER*4    FRCTYP,LOCHIL(*),TYPTMP(2)
C
      REAL*8       TIMREL(*),ELE(*),RSW(3,*),RHS(6,*)
      REAL*8       PRE(3,3),NUT(3,3),BIAS(3,3),XSAT(3),SID(3,3),VSAT(3)
      REAL*8       DRN(3,3),DRI(3,3),DRU(3,3)
      REAL*8       DRNS(3,3),DRIS(3,3),SUNPOS(4),XSTAT(3),XTOP(3),
     1             ETOP(3)
C
      TYPE(t_epoch) :: TIMUT1,TIMTT,TMJD
C
C PARAMETERS OF THE EARTH'S POTENTIAL
C -----------------------------------
      IF(FRCTYP.EQ.13)THEN
C
C PARAMETERS OF THE EARTH'S POTENTIAL
C -----------------------------------
C LOCHIL - DESCRIPTION :
C LOCHIL(1)=13
C LOCHIL(4)=1,2 : 1: COS(M*LAMBDA) TERM, 2: SIN(M*LAMBDA) TERM,
C LOCHIL(5)=N   : DEGREE OF POTENTIAL TERM
C LOCHIL(6)=M   : ORDER OF POTENTIAL TERM
C
C
C ROTATION MATRICES TO GET INTO ORBIT SYSTEM
        CALL DDREH(3,ELE(4),DRN)
        CALL DDREH(1,ELE(3),DRI)
C
C TRANSFORMATION INTO EARTH FIXED SYSTEM
        TPRN=T0+H/2/86400.D0
        CALL POLDEF(TPRN,1,XR,YR,UT1UTC,GPSUTC)
        IF (IORSYS.EQ.1) THEN
          CALL PRENEW(TPRN,PRE)
          CALL NUTNEW(TPRN,NUT)
          EQEQUI=NUT(2,1)
        ELSE
          CALL PREN20(TPRN,PRE)
          CALL NUTMTX(0,TPRN,NUT,NUTNAM,EQEQUI,BIAS)
C
C CHECK IF NAME OF NUTATION MODEL IN ERP FILE IS CONSISTENT
C WITH CHOICE IN INPUT PANEL, IF NOT => WARNING
C ----------------------------------------------------------------
          IF (IFIRST.EQ.1) THEN
           IFIRST=0
C
C Nutation Model Name from ERP-file
           CALL GTFLNA(1,'POLE   ',FILPOL,IRC)
           CALL OPNFIL(LFNLOC,FILPOL,'OLD',' ','READONLY',' ',IOSTAT)
           CALL OPNERR(LFNERR,LFNLOC,IOSTAT,FILPOL,'NUTEFF')
           CALL RDPOLH(LFNLOC,1,DUMMYC,TYPTMP,IFORM,IEND,NUTNA2,SUBNAM)

           IF(NUTNAM.NE.NUTNA2) THEN
             WRITE(LFNERR,375) NUTNAM,NUTNA2
375         FORMAT(/,' ### SR RHSHIL: DIFFERENT NUTATION MODELS ',/,
     1           16X,'ERP FILE     : ',A,/,
     2           16X,'INPUT PANEL  : ',A,/)
           ENDIF
          ENDIF


        ENDIF
        DO 100 IPTS=1,NPTS
          TIMSEC=TIMREL(IPTS)*H/2 +(T0-TOSC)*86400.D0
          TIMMJD=TOSC+TIMSEC/86400.D0
          TMJD = .realToEpoch.TIMMJD
          UT1GPS=UT1UTC-GPSUTC
          TIMUT1=TMJD + UT1GPS
          TIMTT =TMJD + ((19.D0+32.184D0)/86400.D0)
          CALL EPHEM(GM,ELE(1),ELE(2),ELE(3),ELE(4),ELE(5),
     1               ELE(7),TIMSEC,XSAT,VSAT)
          CALL DMLMAV(XSAT,PRE,XSAT)
          CALL DMLMAV(XSAT,NUT,XSAT)
          SIDTIM=GSTIME(0,TIMUT1,TIMTT,NUT(2,1),EQEQUI)
          TDT = TIMMJD + (19.D0+32.184D0)/86400.D0
          CALL SIDMAT(TDT,XR,YR,SIDTIM,SID)
          CALL DMLMAV(XSAT,SID,XSAT)
C
C VALUE OF ASSOCIATED LEGENDRE POLYNOMIAL (DEGREE N, ORDER M)
          N=LOCHIL(5)
          M=LOCHIL(6)

          R=DSQRT(XSAT(1)**2+XSAT(2)**2+XSAT(3)**2)
          SINB=XSAT(3)/R
          COSB=DSQRT(1.D0-SINB**2)
          TANB=SINB/COSB
          PNM  =FAKULT(2*N)/(2**N*FAKULT(N))*COSB**N
          PNM1 =TANB*PNM
          IF(M.EQ.N)THEN
            POL=PNM
            PNM1=0.D0
          ELSE IF(M.EQ.N-1)THEN
            POL=PNM1
            PNM1=PNM
          ELSE
            DO 10 MP=N-2,M,-1
              PNM2=(-PNM+2*(MP+1)*TANB*PNM1)/((N-MP)*(N+MP+1))
                IF(MP > M)THEN
                PNM =PNM1
                PNM1=PNM2
              ENDIF
10          CONTINUE
            POL=PNM2
          END IF
C
C GRADIENT OF POTENTIAL
          CALL DVDXYZ(LOCHIL(4),N,M,POL,PNM1,SINB,COSB,TANB,
     1                R,XSAT,RSW(1,IPTS))
          CALL DMLMTV(RSW(1,IPTS),SID,RSW(1,IPTS))
          CALL DMLMTV(RSW(1,IPTS),NUT,RSW(1,IPTS))
          CALL DMLMTV(RSW(1,IPTS),PRE,RSW(1,IPTS))
C
C ROTATION INTO RSW SYSTEM
          CALL DMLMAV(RSW(1,IPTS),DRN,RSW(1,IPTS))
          CALL DMLMAV(RSW(1,IPTS),DRI,RSW(1,IPTS))
          XM=XN*(TIMSEC-ELE(7))
          EX=XM
          E=ELE(2)
          DO 15 K=1,10
            EX=XM+E*DSIN(EX)
15        CONTINUE
          V=2*DATAN(DSQRT((1+E)/(1-E))*DTAN(EX/2))
          CALL DDREH(3,V+ELE(5),DRU)
          CALL DMLMAV(RSW(1,IPTS),DRU,RSW(1,IPTS))
          DO 20 K=1,3
            RSW(K,IPTS)=RSW(K,IPTS)/XNORM
20        CONTINUE
C
C scaling factor, including transition from unnormalized to fully normalized SH coefficient
            RSW(1:3,IPTS) = RSW(1:3,IPTS)/XNORM
            IF(M == 0)THEN
              RSW(1:3,IPTS) = DSQRT(2.d0*N+1.D0)*RSW(1:3,IPTS)
            ELSE
              RSW(1:3,IPTS) = DSQRT(2.d0*(2.d0*N+1.D0)*
     1                         (fakult(n-m))/fakult(n+m))*RSW(1:3,IPTS)
            ENDIF

100     CONTINUE
      ELSE IF(FRCTYP.EQ.14)THEN
C
C RESONANCE TERMS
C ---------------
C LOCHIL - DESCRIPTION :
C LOCHIL(1)=14
C LOCHIL(2)=ARC NUMBER
C LOCHIL(3)=SATELLITE NUMBER
C LOCHIL(4)=1,2,3 : R, S, W COMPONENT
C LOCHIL(5)=1,2,3 : CONSTANT-, COS-, SIN- TERM
C
        TPER=ELE(7)
        DO 300 IPTS=1,NPTS
          TIMSEC=TIMREL(IPTS)*H/2 +(T0-TOSC)*86400.D0
          TIMMJD=TOSC+TIMSEC/86400.D0
          DO 210 K=1,3
            RSW(K,IPTS)=0.D0
210       CONTINUE
          IND=LOCHIL(4)
          IF(LOCHIL(5).EQ.1)THEN
            RSW(IND,IPTS)=1.D0/XNORM
          ELSE IF(LOCHIL(5).EQ.2)THEN
            RSW(IND,IPTS)=DCOS(XN*(TIMSEC-TPER))/XNORM
          ELSE IF(LOCHIL(5).EQ.3)THEN
            RSW(IND,IPTS)=DSIN(XN*(TIMSEC-TPER))/XNORM
          END IF
300     CONTINUE
      ELSE IF(FRCTYP.EQ.15)THEN
C
C EARTH ALBEDO MODEL
C ------------------
C LOCHIL - DESCRIPTION :
C LOCHIL(1)=15
C LOCHIL(4)=ITYP: PARAMETER TYPE (1, 2, OR 3)
C LOCHIL(5)=IB  : BLOCK NUMBER OF PARAMETER
C
C NUMBER OF SURFACE ELEMENTS, SURF. ELEMENT
        NLAT =10
        MLONG=20
        DSURF=2*PI/(NLAT*MLONG)
C
C ROTATION MATRICES TO GET INTO ORBIT SYSTEM
        CALL DDREH(3,ELE(4),DRN)
        CALL DDREH(1,ELE(3),DRI)
C
C POSITION OF SUN AT MID OF ARC
        TPRN=T0+H/2/86400.D0
        IF (IORSYS.EQ.1) THEN
          CALL SUN(TPRN,SUNPOS,RR,XLL,BB)
        ELSE
          CALL SUN20(TPRN,SUNPOS,RR,XLL,BB)
        ENDIF
        DO 410 K=1,3
C          SUNPOS(K)=1.49597870D11*SUNPOS(K)
          SUNPOS(K)=AU*SUNPOS(K)
410     CONTINUE
C        SUNPOS(4)=1.49597870D11*RR
        SUNPOS(4)=AU*RR
C
C TRANSFORMATION INTO SYSTEM WITH TERMINATOR AS FUNDAMENTAL PLANE
        XKNTER=DATAN2(SUNPOS(1),-SUNPOS(2))
        XITER=DATAN2(DSQRT(SUNPOS(1)**2+SUNPOS(2)**2),SUNPOS(3))
        CALL DDREH(3,XKNTER,DRNS)
        CALL DDREH(1,XITER,DRIS)
        DO 500 IPTS=1,NPTS
          TIMSEC=TIMREL(IPTS)*H/2 +(T0-TOSC)*86400.D0
          TIMMJD=TOSC+TIMSEC/86400.D0
          CALL EPHEM(GM,ELE(1),ELE(2),ELE(3),ELE(4),ELE(5),
     1               ELE(7),TIMSEC,XSAT,VSAT)
C
C ROTATE INTO TERMINATOR SYSTEM
          CALL DMLMAV(XSAT,DRNS,XSAT)
          CALL DMLMAV(XSAT,DRIS,XSAT)
C
C ADD ALBEDO FORCE OVER ALL SURFACE ELEMENTS
          DO 425 K=1,3
            RSW(K,IPTS)=0.D0
425       CONTINUE
          DO 450 N=1,NLAT
            HI =1.D0-(N-1)*1.D0/NLAT
            HI1=1.D0-N*1.D0/NLAT
            HH =1.D0-(HI+HI1)/2
            SPHI=DSIN(HH)
            CPHI=DSQRT(1.D0-SPHI**2)
            DO 450 M=1,MLONG
              XL=2*PI*(M-0.5D0)/MLONG
              DSURFE=DSURF*SPHI
C
C RECTANGULAR COORDINATES OF CENTER OF SURFACE ELEMENT IN TERMINATOR SYSTEM
              XSTAT(1)=CPHI*DCOS(XL)
              XSTAT(2)=CPHI*DSIN(XL)
              XSTAT(3)=SPHI
              RTOP=0.D0
              COSZEN=0.D0
              DO 420 K=1,3
                XTOP(K)=XSAT(K)/AE-XSTAT(K)
                RTOP=RTOP+XTOP(K)**2
                COSZEN=COSZEN+XTOP(K)*XSTAT(K)
420           CONTINUE
              RTOP=DSQRT(RTOP)
              COSZEN=COSZEN/RTOP
              COSSUN=XTOP(3)/RTOP
              DO 421 K=1,3
                ETOP(K)=XTOP(K)/RTOP
421           CONTINUE
C
C ADD ALBEDO FORCE DUE TO CURRENT SURFACE ELEMENT
              IF(COSZEN.GT.0.D0)THEN
                DO 430 K=1,3
                  IF(LOCHIL(4).EQ.1)THEN
C
C ABSORPTION, SPERICALLY SYMMETRIC SATELLITE
                    RSW(K,IPTS)=RSW(K,IPTS)+DSURFE*COSZEN/RTOP**2*
     1                          ETOP(K)
                  ELSE IF(LOCHIL(4).EQ.2)THEN
C
C ABSORPTION, SOLAR PANELS
                    RSW(K,IPTS)=RSW(K,IPTS)+DSURFE*COSZEN/RTOP**2*
     1                          DABS(COSSUN)*ETOP(K)
                  ELSE IF(LOCHIL(4).EQ.3)THEN
C
C REFLECTED LIGTH, SOLAR PANELS ONLY
                    IF(K.LT.3)THEN
                      RSW(K,IPTS)=0.D0
                    ELSE
                      RSW(K,IPTS)=RSW(K,IPTS)
     1                            +2*DSURFE*COSZEN*COSSUN/RTOP**2
                    END IF
                  ELSE
                    WRITE(LFNERR,427)LOCHIL(4)
427                 FORMAT(/,' *** SR RHSHIL : INVALID ALBEDO ',
     1                       'PARM TYPE:',I3,/)
                    CALL EXITRC(2)
                  END IF
430             CONTINUE
              END IF
450       CONTINUE
C
C ROTATE BACK INTO INERTIAL SYSTEM
          CALL DMLMTV(RSW(:,IPTS),DRIS,RSW(:,IPTS))
          CALL DMLMTV(RSW(:,IPTS),DRNS,RSW(:,IPTS))
C
C ROTATE INTO (R,S,W)-SYSTEM
          CALL DMLMAV(RSW(:,IPTS),DRN,RSW(:,IPTS))
          CALL DMLMAV(RSW(:,IPTS),DRI,RSW(:,IPTS))
          XM=XN*(TIMSEC-ELE(7))
          EX=XM
          E=ELE(2)
          DO 460 K=1,10
            EX=XM+E*DSIN(EX)
460       CONTINUE
          V=2*DATAN(DSQRT((1+E)/(1-E))*DTAN(EX/2))
          CALL DDREH(3,V+ELE(5),DRU)
          CALL DMLMAV(RSW(1,IPTS),DRU,RSW(1,IPTS))
          DO 470 K=1,3
            RSW(K,IPTS)=RSW(K,IPTS)/XNORM
470       CONTINUE
500     CONTINUE
      ELSE
        WRITE(LFNERR,70)FRCTYP
70      FORMAT(//,' SR RHSHIL : INVALID PARAMETER TYPE :',I4,/)
      END IF
C
C INTEGRATION OF THE FOLLOWING DEQ-SYSTEM (ALL QUADRATURES)
C IN THE INTERVAL (0,U)
C WITH T=TSEC-TPER
C A1' = -(R*SIN(N*T)+2*S*COS(NT)) , B1' = (R*COS(NT)-2*S*SIN(NT))
C C0' = 2*S/N                     , C1' = 3*T*S - 2*R/N
C A'  = -W*SIN(NT)/N              , B'  = W*COS(NT)/N
C
      TPER=ELE(7)
      DO 400 IPTS=1,NPTS
        TIMSEC=TIMREL(IPTS)*H/2+(T0-TOSC)*86400.D0
        ARG=XN*(TIMSEC-TPER*0)
        RHS(1,IPTS)=-(RSW(1,IPTS)*DSIN(ARG)+2*RSW(2,IPTS)*DCOS(ARG))/XN
        RHS(2,IPTS)= (RSW(1,IPTS)*DCOS(ARG)-2*RSW(2,IPTS)*DSIN(ARG))/XN
        RHS(3,IPTS)= 2*RSW(2,IPTS)/XN
        RHS(4,IPTS)= 3*(TIMSEC-TPER*0)*RSW(2,IPTS) - 2*RSW(1,IPTS)/XN
        RHS(5,IPTS)=-RSW(3,IPTS)*DSIN(ARG)/XN
        RHS(6,IPTS)= RSW(3,IPTS)*DCOS(ARG)/XN
400   CONTINUE
C
      RETURN
      END SUBROUTINE

      END MODULE
