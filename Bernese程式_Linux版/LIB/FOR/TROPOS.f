      MODULE s_TROPOS
      CONTAINS
C*
      SUBROUTINE TROPOS(Z,HS,T,P,RH,MODEL,WL,DR)
CC
CC NAME       :  TROPOS
CC
CC PURPOSE    :  THIS SR CALCULATES THE TROP. DISTANCE CORRECTION DR
CC               THEORY:
CC               AD 1: SEE I. BAUERSIMA "GPS II", MITTEILUNGEN DER
CC                     SATELLITENBEOBACHTUNGSSTATION ZIMMERWALD NR. 10
CC                     MODEL DEFINED BY SAASTAMOINEN
CC               AD 2: DISS. REMONDI, P.37 FF
CC               AD 3: DAVIDSON ET AL, TR 90 UNB P.73
CC               AD 4: MARINI AND MURRAY (1973)
CC
CC PARAMETERS :
CC         IN :  Z      : ZENITH-DISTANCE (RADIANS)           R*8
CC               HS     : LATITUDE (RAD), LONGITUDE (RAD),    R*8(3)
CC                        HEIGHT OF OBSERVATORY ABOVE
CC                        SEA-LEVEL (M)
CC               T      : TEMPERATURE (DEG. CELSIUS)          R*8
CC               P      : ATMOSPHERIC PRESSURE (MBAR)         R*8
CC               RH     : RELATIVE HUMIDITY                   R*8
CC               MODEL  : 1 : SAASTAMOINEN                    I*4
CC                        2 : MODIFIED HOPFIELD
CC                        3 : SIMPLIFIED HOPFIELD
CC                        4 : MARINI MURRAY (SLR)
CC                        8 : MENDES-PAVLIS (SLR)
CC                        ORIGINAL MODEL + 10 : DRY PART ONLY
CC                        11: SAASTAMOINEN DRY PART ONLY
CC                        12: MODIFIED HOPFIELD DRY ONLY
CC                        13: SIMPLIFIED HOPFIELD DRY ONLY
CC               WL     : WAVELENGTH FOR MARINI-MURRAY        R*8
CC        OUT :  DR     : TROPOSPHERIC CORRECTION (M)         R*8
CC
CC REMARKS    :  MARINI MURRAY: GEOGR LAT: 45 DEG, WL=532 NM
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/03 12:02
CC
CC CHANGES    :   6-APR-92 : ??: LTAB SEEMS TO BE UNNECESSARY IF TRUE AND
CC                               NOT APPARENT ZENITH DISTANCE IS USED
CC               21-APR-94 : ??: LIMIT REL HUMIDITY TO 100 PERCENTS
CC               10-AUG-94 : MR: CALL EXITRC
CC               29-SEP-95 : WG: MARINI MURRAY
CC               29-SEP-95 : MR: NEW MODELS FOR DRY DELAY ONLY
CC               03-OCT-95 : JJ: FIX CALL USING DMIN1 -> MIN1
CC               08-NOV-95 : TS: FIX CALL USING MIN1 -> AMIN1
CC               21-OCT-96 : MR: K AS REAL*4 WAS USED AS LOOP INDEX
CC               31-MAR-99 : MR: USE "MODEFF" INSTEAD OF "MODEL" IN
CC                               COMPUTED "GOTO"
CC               15-AUG-99 : JJ: COMMENT OUT UNUSED VARS HTAB, ZTAB,
CC                               LTAB
CC               27-JAN-00 : TS: "DIRTY" TRICK FOR MARINNI MURRAY WL
CC               28-JAN-03 : RS: CHANGE ALL REAL*4 -> REAL*8
CC               07-Mar-03 : HU: REPLACE AMIN1 BY DMIN1
CC               15-Mar-03 : HU: R*8 VALUES IN DATA BCOR
CC               29-MAR-03 : CU: NEW INPUT PARAMETER WL (WAVELENGTH FOR RANGE
CC                               MEASUREMENTS), REMOVE COMMON MARRINI
CC               21-JUN-05 : MM: COMLFNUM.inc REMOVED, m_bern ADDED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               13-DEC-05 : CU: USE D_CONST: PI,
CC                               USE ELLIP. LAT. OF STATION (PHI)
CC               21-JAN-06 : HU: CHECK WHETHER CONSTANTS ARE DEFINED
CC               11-NOV-08 : DT: ADD TROP.MODEL MENDES-PAVLIS INCLUDING
CC                               MAPPING FUNCTION (SR MENDPAV);
CC                               MAGNUS-TETENS FOR SAT.WATER VAPOR PRESSURE
CC               16-FEB-12 : RS: CORRECT SAASTAMOINEN MODEL FOR CALC/SOLVE
CC                               COMPARISONS
CC               05-SEP-12 : SL: ALLOW STAT.HEIGHT GE 5KM FOR SAASTAMOINEN
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
C*
      USE m_bern,  ONLY: LFNERR
      USE d_const, ONLY: PI, CONST_DEF
      USE s_exitrc
      USE s_mendpav
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , IK    , MODEFF, MODEL , ICORR
C
      REAL*8    A     , AE    , B     , COS2PH, COSTH , DRI   , E     ,
     1          ELEV  , FL    , G     , H4    , HL    , HREF  , P4    ,
     2          PHI   , R     , RH4   , RHUM  , S     , SINE  ,
     3          SINTH , T4    , THETA , Z4
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
C PARAMETERS:
C
C ICORR  : HOW TO EVALUATE SAASTAMOINEN-FORMULA I*4
C          0: COMPATIBLE WITH V5.0
C          1: CORRECT (COMPATIBLE, E.G., WITH CALC/SOLVE)
      PARAMETER(ICORR=0)
C
      REAL*8    BCOR(7)
C     REAL*8    LTAB(8,13)
C     REAL*8    ZTAB(13)
C     REAL*8    HTAB(8)
      REAL*8    ALPHA(9),H(2),N(2),KW,KD,K
C
      REAL*8    Z,T,P,RH,DR
      REAL*8    HS(3)
      REAL*8    WL
      REAL*8    f, f2
C
      REAL*8    k0,k1,k2,k3, w0,w1,w2,w3
      REAL*8    xc, CO2
      REAL*8    Fs, Fh, FnH, D_H, D_wet, ZTD
      REAL*8    MAPFUN
C
C
      DATA BCOR/1.156D0,1.006D0,0.874D0,0.757D0,0.654D0,0.563D0,
     1          0.49D0/
C     DATA HTAB/0.,0.5,1.,1.5,2.,3.,4.,5./
C     DATA ZTAB/60.,66.,70.,73.,75.,76.,77.,78.,78.5,79.,79.5,
C    1          79.75,80./
C
C  INTERNAL REPRESENTATION OF TABLE LTAB IS TRANSPOSED TO FOLLOWING
C  IMAGE!
C     DATA LTAB/.003,.003,.002,.002,.002,.002,.001,.001,
C    1          .006,.006,.005,.005,.004,.003,.003,.002,
C    2          .012,.011,.010,.009,.008,.006,.005,.004,
C    3          .020,.018,.017,.015,.013,.011,.009,.007,
C    4          .031,.028,.025,.023,.021,.017,.014,.011,
C    5          .039,.035,.032,.029,.026,.021,.017,.014,
C    6          .050,.045,.041,.037,.033,.027,.022,.018,
C    7          .065,.059,.054,.049,.044,.036,.030,.024,
C    8          .075,.068,.062,.056,.051,.042,.034,.028,
C    9          .087,.079,.072,.065,.059,.049,.040,.033,
C    1          .102,.093,.085,.077,.070,.058,.047,.039,
C    2          .111,.101,.092,.083,.076,.063,.052,.043,
C    3          .121,.110,.100,.091,.083,.068,.056,.047/
C
C NO MODEL AT ALL
      IF(MODEL.EQ.0) THEN
        DR=0.D0
        RETURN
      ENDIF
C
C CONSTANTS DEFINED?
      IF (CONST_DEF.NE.1) THEN
        WRITE(LFNERR,"(/' *** SR TROPOS: CONSTANTS NOT DEFINED')")
        CALL EXITRC(2)
      ENDIF
C
C MODEL WITH ONLY DRY DELAY
      IF (MODEL.LE.10) THEN
        MODEFF=MODEL
        RHUM=RH
      ELSE
        MODEFF=MODEL-10
        RHUM=0.0
      ENDIF
C
      Z4=Z
      H4=HS(3)
      T4=T+273.15
      P4=P
      RH4=DMIN1(RHUM,100.0D0)
C
C  WATER VAPOR PRESSURE
      E=RH4/100.*EXP(-37.2465+0.213166*T4-0.000256908*T4*T4)
C
C Select model
C -------------
      IF (MODEFF.LE.4) THEN
        GOTO (100,200,300,400) MODEFF
      ELSEIF (MODEFF.EQ.8) THEN
        GOTO 800
      ELSE
        WRITE(LFNERR,1) MODEL
1       FORMAT(/,' *** SR TROPOS: ILLEGAL TROPOSPHERE MODEL',/,
     1                     16X,'MODEL TYPE:',I3,/)
        CALL EXITRC(2)
      ENDIF
C
C  MODEL 1: SAASTAMOINEN
C
C  HEIGHT IN KM
100   HL=H4/1000.
      IF(HL.LE.0.) THEN
        B=BCOR(1)
      ELSEIF(HL.GE.6.) THEN
        B=BCOR(7)
      ELSE
        I=INT(HL)+1
C  REFERENCE HEIGHT FOR LINEAR INTERPOLATION IN TABLE BCOR
        HREF=DBLE(I)-1.
        B=BCOR(I)+(BCOR(I+1)-BCOR(I))*(HL-HREF)
      ENDIF
C
C TROPOSPHERIC DISTANCE CORRECTION DR
      IF (ICORR.EQ.0) THEN
C
C THE COEFFICIENT "1225" SHOULD BE "1255", BUT THE WRONG VALUE HAS BEEN
C KEPT TO STAY COMPATIBLE WITH ALREADY EXISTING TROPOSPHERE RESULT
C FILES
        DR=2.277E-3*(P4+(1225/T4+.05)*E-B*TAN(Z4)**2)/COS(Z4)
      ELSE
C Correct formula for Calc/Solve comparisons
        DR=2.277d-3*(P4+(1255/T4+.05)*E-B*TAN(Z4)**2)/COS(Z4)*
     1     (1+2.6d-3*COS(2.d0*HS(1))+2.8d-4*H4/1.d3)
      ENDIF
C
C  CORRECTION TERM DL
C
C  LOOK FOR NEIGHBOURING VALUES IN TABLE LTAB
CC      ZL=Z4*180/PI
CC      IF(ZL.LT.60.) RETURN
CC      IF(ZL.GT.80.) ZL=80.
CC      DO 110 I=2,8
CC        IF(HL.LE.HTAB(I)) GOTO 120
CC110   CONTINUE
CCC  UPPER LINE
CC120   IUPP=I
CCC  LOWER LINE
CC      ILOW=I-1
CC      DO 130 K=2,13
CC        IF(ZL.LE.ZTAB(K)) GOTO 140
CC130   CONTINUE
CCC  UPPER COLUMN
CC140   KUPP=K
CCC  LOWER COLUMN
CC      KLOW=K-1
CCC  INTERPOLATE INTO LOWER LINE (ARG. ZL)
CC      DLLOW=LTAB(ILOW,KLOW)+(LTAB(ILOW,KUPP)-LTAB(ILOW,KLOW))/
CC     1         (ZTAB(KUPP)-ZTAB(KLOW))*(ZL-ZTAB(KLOW))
CCC  INTERPOLATE INTO UPPER LINE (ARG. AL)
CC      DLUPP=LTAB(IUPP,KLOW)+(LTAB(IUPP,KUPP)-LTAB(IUPP,KLOW))/
CC     1         (ZTAB(KUPP)-ZTAB(KLOW))*(ZL-ZTAB(KLOW))
CCC  INTERPOLATE VERTICALLY (ARG. HL)
CC      DL   =DLLOW+(DLUPP-DLLOW)/(HTAB(IUPP)-HTAB(ILOW))*(HL-HTAB(ILOW))
CCC  APPLY CORRECTION TO DR
CC      DR=DR+DL
C
      RETURN
C
C  MODEL 2: MODIFIED HOPFIELD
C
200   N(1)=0.776E-4*P4/T4
      N(2)=0.373*E/T4/T4
      H(1)=40.136+0.14872*(T4-273.16)
      H(2)=11.
C  ELEVATION ANGLE
      THETA=PI/2.-Z4
      AE=6378.+H4/1000.
      DR=0.D0
      SINTH=SIN(THETA)
      COSTH=COS(THETA)
      DO 210 I=1,2
        R=SQRT((AE+H(I))**2-(AE*COSTH)**2)-AE*SINTH
        A=-SINTH/H(I)
        B=-COSTH**2/(2.*H(I)*AE)
        ALPHA(1)=1.
        ALPHA(2)=4.*A
        ALPHA(3)=6.*A*A+4.*B
        ALPHA(4)=4.*A*(A*A+3.*B)
        ALPHA(5)=A**4+12.*A*A*B+6.*B*B
        ALPHA(6)=4.*A*B*(A*A+3.*B)
        ALPHA(7)=B*B*(6.*A*A+4.*B)
        ALPHA(8)=4.*A*B**3
        ALPHA(9)=B**4
        DRI=0.
        DO 220 IK=1,9
          DRI=DRI+ALPHA(IK)/IK*R**IK
220     CONTINUE
        DR=DR+1.E3*DRI*N(I)
210   CONTINUE
C
      RETURN
C
C  MODEL 3: SIMPLIFIED HOPFIELD
C
300   ELEV=90.-Z4*180./PI
      KD=1.552E-5*P4/T4*((148.72*T4-488.3552)-H4)
      KW=7.46512E-2*E/T4**2*(11000.-H4)
      DR=KD/SIN(SQRT((ELEV**2+6.25))*PI/180.)
     1  +KW/SIN(SQRT((ELEV**2+2.25))*PI/180.)
C
      RETURN
C
C  MODEL 4: MARINI MURRAY
C
C  WELLENLAENGE ND:YAG (MIKROMETER)
400   CONTINUE
C
C  ERROR IF NO WAVELENGTH GIVEN
      IF (WL == 0D0) THEN
        WRITE(LFNERR,'(/,A,A,/,A)')
     1    ' *** SR TROPOS: Required wavelength to use the ',
     2                    'MARINI-MURRAY troposphere',
     3    '                model not found.'
        CALL EXITRC(2)
      ENDIF
C
C  HEIGHT IN KM
      HL   = H4/1000D0
C
C  DAMPFDRUCK
      S    = 7.5D0 * (T4-273.15D0) / (237.3D0+(T4-273.15D0))
      E    = RH4 / 100D0 * 6.11D0 * 10D0**S
C
C DIV. KOEFF.
      PHI  = HS(1)
      COS2PH = COS(2D0*PHI)
      FL   = 0.9650D0 + 0.0164D0/WL**2D0 + 0.000228D0/WL**4D0
      K    = 1.163D0 - 0.00968D0*COS2PH - 0.00104D0*T4 + 0.00001435D0*P4
      A    = 0.002357D0*P4 + 0.000141D0*E
      B    = 1.084D-8*P4*T4*K + 4.734D-8*P4*P4/T4*2D0/(3D0-1D0/K)
      G    = 1D0 - 0.0026D0*COS2PH - 0.00031D0*HL
      SINE = SIN(PI/2D0-Z4)
C
C  DISTANZKORREKTUR IN METER
      DR   = FL/G*(A+B)/(SINE+B/(A+B)/(SINE+0.01D0))
C
      RETURN
C
C  MODEL 8: MENDES-PAVLIS
C
800   CONTINUE
C
C  ERROR IF NO WAVELENGTH GIVEN
      IF (WL == 0D0) THEN
        WRITE(LFNERR,'(/,A,A,/,A)')
     1    ' *** SR TROPOS: Required wavelength to use the ',
     2                    'MENDES-PAVLIS troposphere',
     3    '                model not found.'
        CALL EXITRC(2)
      ENDIF
C
C Carbone dioxide content [ppm]
      xc = 375.0d0
      CO2 = 1.0d0 + 0.534d-6 * (xc - 450.0d0)
C Coefficients needed for hydrostatic component
      k0 = 238.0185d0
      k1 = 19990.975d0
      k2 = 57.362d0
      k3 = 579.55174d0
C Coefficients needed for non-hydrostatic component
      w0 =  295.235d0
      w1 =  2.6422d0
      w2 = -0.032380d0
      w3 =  0.004028d0
C
C Partial pressure of Water vapor
CCC      S = 7.5D0 * (T4-273.15D0) / (237.3D0+(T4-273.15D0))
CCC      E = RH4 / 100.0D0 * 6.11D0 * 10.D0**S
C nach Magnus-Tetens:
      S = 0.7858D0 + 7.5D0 * T / (237.3D0 + T)
      E = RH4/100.0D0 * EXP(2.3026D0 * S)
C
C Frequency
      f  = 1.0d0 / WL
      f2 = f**2
C
C Hydrostatic component
      Fs = 1.0d0 - 2.66d-3*DCOS(2.0d0*HS(1)) - 2.8d-7*HS(3)
      Fh = 1.d-2*  CO2 * (k1*(k0+f2)/(k0-f2)**2 + k3*(k2+f2)/(k2-f2)**2)
      D_H = 2.416579d-3 * P4 * Fh / Fs
C
C Non-hydrostatic component
      FnH = 3.101d-3 *(w0 + 3.0d0*w1*f2 + 5.0d0*w2*f**4 + 7.0d0*w3*f**6)
      D_wet = 1.0d-4 * (5.316d0*Fnh - 3.759d0*Fh) * E / Fs
C
C Total Zenith Delay
      ZTD = D_H + D_wet
C
C Mapping
      CALL MENDPAV(HS(1),HS(3),T,Z,MAPFUN)
      DR = ZTD * MAPFUN
C
      RETURN
      END SUBROUTINE
C
      END MODULE
