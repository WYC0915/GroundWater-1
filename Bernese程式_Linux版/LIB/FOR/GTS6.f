      MODULE s_GTS6
      CONTAINS

      SUBROUTINE GTS6(IYD,SEC,ALT,GLAT,GLONG,STL,F107A,F107,AP,MASS,D,T)
C        Neutral Thermosphere Model above 72.5 km for MSISE-90
C         A.E.Hedin 3/9/90
C         Coefficients not changed for 120km and above, but results may differ
C        by a few percent from MSIS-86 (GTS5) with introduction of a
C        latitude dependent accel. of gravity.
C         Lower thermosphere reformulated for better continuation into
C        lower atmosphere.
C        For efficiency:
C         Exospheric temperature left at average value for alt below 120km;
C         120 km gradient left at average value for alt below 72 km;
C     INPUT:
C        IYD - YEAR AND DAY AS YYYYDDD
C        SEC - UT(SEC)
C        ALT - ALTITUDE(KM) (GREATER THAN 72.5 KM)
C        GLAT - GEODETIC LATITUDE(DEG)
C        GLONG - GEODETIC LONGITUDE(DEG)
C        STL - LOCAL APPARENT SOLAR TIME(HRS)
C        F107A - 3 MONTH AVERAGE OF F10.7 FLUX
C        F107 - DAILY F10.7 FLUX FOR PREVIOUS DAY
C        AP - MAGNETIC INDEX(DAILY) OR WHEN SW(9)=-1. :
C           - ARRAY CONTAINING:
C             (1) DAILY AP
C             (2) 3 HR AP INDEX FOR CURRENT TIME
C             (3) 3 HR AP INDEX FOR 3 HRS BEFORE CURRENT TIME
C             (4) 3 HR AP INDEX FOR 6 HRS BEFORE CURRENT TIME
C             (5) 3 HR AP INDEX FOR 9 HRS BEFORE CURRENT TIME
C             (6) AVERAGE OF EIGHT 3 HR AP INDICIES FROM 12 TO 33 HRS PRIOR
C                    TO CURRENT TIME
C             (7) AVERAGE OF EIGHT 3 HR AP INDICIES FROM 36 TO 59 HRS PRIOR
C                    TO CURRENT TIME
C        MASS - MASS NUMBER (ONLY DENSITY FOR SELECTED GAS IS
C                 CALCULATED.  MASS 0 IS TEMPERATURE.  MASS 48 FOR ALL.
C     Note:  Ut, Local Time, and Longitude are used independently in the
C            model and are not of equal importance for every situation.
C            For the most physically realistic calculation these three
C            variables should be consistent (STL=SEC/3600+GLONG/15).
C     OUTPUT:
C        D(1) - HE NUMBER DENSITY(CM-3)
C        D(2) - O NUMBER DENSITY(CM-3)
C        D(3) - N2 NUMBER DENSITY(CM-3)
C        D(4) - O2 NUMBER DENSITY(CM-3)
C        D(5) - AR NUMBER DENSITY(CM-3)
C        D(6) - TOTAL MASS DENSITY(GM/CM3)
C        D(7) - H NUMBER DENSITY(CM-3)
C        D(8) - N NUMBER DENSITY(CM-3)
C        T(1) - EXOSPHERIC TEMPERATURE
C        T(2) - TEMPERATURE AT ALT
C
C           The following is for test and special purposes:
C           (1) LOWER BOUND QUANTITIES IN COMMON/GTS3C/
C           (2) TO TURN ON AND OFF PARTICULAR VARIATIONS CALL TSELEC(SW)
C               WHERE SW IS A 25 ELEMENT ARRAY CONTAINING 0. FOR OFF, 1.
C               FOR ON, OR 2. FOR MAIN EFFECTS OFF BUT CROSS TERMS ON
C               FOR THE FOLLOWING VARIATIONS
C               1 - F10.7 EFFECT ON MEAN  2 - TIME INDEPENDENT
C               3 - SYMMETRICAL ANNUAL    4 - SYMMETRICAL SEMIANNUAL
C               5 - ASYMMETRICAL ANNUAL   6 - ASYMMETRICAL SEMIANNUAL
C               7 - DIURNAL               8 - SEMIDIURNAL
C               9 - DAILY AP             10 - ALL UT/LONG EFFECTS
C              11 - LONGITUDINAL         12 - UT AND MIXED UT/LONG
C              13 - MIXED AP/UT/LONG     14 - TERDIURNAL
C              15 - DEPARTURES FROM DIFFUSIVE EQUILIBRIUM
C              16 - ALL TINF VAR         17 - ALL TLB VAR
C              18 - ALL TN1 VAR           19 - ALL S VAR
C              20 - ALL TN2 VAR           21 - ALL NLB VAR
C              22 - ALL TN3 VAR           23 - TURBO SCALE HEIGHT VAR
C
C              To get current values of SW: CALL TRETRV(SW)
C
C
      USE l_basfun, ONLY: dmod
      USE f_globe6
      USE f_glob6s
      USE f_ccor
      USE f_vtst
      USE f_densu
      USE f_dnet
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*4 (I-N)
C
      LOGICAL METER
      DIMENSION ZN1(5)
      COMMON/GTS3C/TLB,S,DB04,DB16,DB28,DB32,DB40,DB48,DB01,ZA,T0,Z0
     1 ,G0,RL,DD,DB14,TR12
      COMMON/MESO6/TN1(5),TN2(4),TN3(5),TGN1(2),TGN2(2),TGN3(2)
      DIMENSION D(8),T(2),MT(10),AP(7),ALTL(8)
      COMMON/LOWER6/PTM(10),PDM(10,8)
      COMMON/PARM6/PT(150),PD(150,9),PS(150),PDL(25,2),PTL(100,4),
     1 PMA(100,10)
      COMMON/CSW/SW(25),ISW,SWC(25)
      COMMON/TTEST/TINFG,GB,ROUT,TT(15)
      COMMON/DMIX/DM04,DM16,DM28,DM32,DM40,DM01,DM14
      COMMON/METSEL/IMR
      SAVE
      DATA MT/48,0,4,16,28,32,40,1,49,14/
      DATA ALTL/200.D0,400.D0,160.D0,200.D0,240.D0,450.D0,320.D0,450.D0/
      DATA MN1/5/,ZN1/120.D0,110.D0,100.D0,90.D0,72.5D0/
      DATA DGTR/1.74533D-2/,DR/1.72142D-2/,ALAST/-999.D0/
Ce        Test for changed input
      V2=VTST(IYD,SEC,GLAT,GLONG,STL,F107A,F107,AP,2)
C
      YRD=IYD
      ZA=PDL(16,2)
      ZN1(1)=ZA
      DO 2 J=1,8
        D(J)=0.D0
    2 CONTINUE
Ce       TINF VARIATIONS NOT IMPORTANT BELOW ZA OR ZN1(1)
      IF(ALT.GT.ZN1(1)) THEN
        IF(V2.EQ.1..OR.ALAST.LE.ZN1(1)) TINF=PTM(1)*PT(1)
     1     *(1.+SW(16)*GLOBE6(YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP,PT))
      ELSE
        TINF=PTM(1)*PT(1)
      ENDIF
      T(1)=TINF
Ce         GRADIENT VARIATIONS NOT IMPORTANT BELOW ZN1(5)
      IF(ALT.GT.ZN1(5)) THEN
        IF(V2.EQ.1.OR.ALAST.LE.ZN1(5)) G0=PTM(4)*PS(1)
     1    *(1.+SW(19)*GLOBE6(YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP,PS))
      ELSE
        G0=PTM(4)*PS(1)
      ENDIF
Ce      Calculate these temperatures only if input changed
      IF(V2.EQ.1.)
     1            TLB=PTM(2)*(1.+SW(17)*GLOBE6(YRD,SEC,GLAT,GLONG,STL,
     1            F107A,F107,AP,PD(1,4)))*PD(1,4)
      S=G0/(TINF-TLB)
Ce       Lower thermosphere temp variations not significant for
Ce        density above 300 km
      IF(ALT.LT.300.D0) THEN
        IF(V2.EQ.1.D0.OR.ALAST.GE.300.D0) THEN
          TN1(2)=PTM(7)*PTL(1,1)/(1.D0-SW(18)*GLOB6S(PTL(1,1)))
          TN1(3)=PTM(3)*PTL(1,2)/(1.D0-SW(18)*GLOB6S(PTL(1,2)))
          TN1(4)=PTM(8)*PTL(1,3)/(1.D0-SW(18)*GLOB6S(PTL(1,3)))
          TN1(5)=PTM(5)*PTL(1,4)/(1.D0-SW(18)*SW(20)*GLOB6S(PTL(1,4)))
          TGN1(2)=PTM(9)*PMA(1,9)*(1.D0+SW(18)*SW(20)*GLOB6S(PMA(1,9)))
     1                *TN1(5)*TN1(5)/(PTM(5)*PTL(1,4))**2
        ENDIF
      ELSE
        TN1(2)=PTM(7)*PTL(1,1)
        TN1(3)=PTM(3)*PTL(1,2)
        TN1(4)=PTM(8)*PTL(1,3)
        TN1(5)=PTM(5)*PTL(1,4)
        TGN1(2)=PTM(9)*PMA(1,9)
     1              *TN1(5)*TN1(5)/(PTM(5)*PTL(1,4))**2
      ENDIF
C
      Z0=ZN1(4)
      T0=TN1(4)
      ZLB=PTM(6)
      TR12=1.D0
C
      IF(MASS.EQ.0) GO TO 50
C       N2 variation factor at Zlb
      G28=SW(21)*GLOBE6(YRD,SEC,GLAT,GLONG,STL,F107A,F107,
     1            AP,PD(1,3))
C        Variation of turbopause height
      DAY=DMOD(YRD,1000.D0)
      ZHF=PDL(25,2)
     1    *(1.D0+SW(5)*PDL(25,1)*DSIN(DGTR*GLAT)*DCOS(DR*(DAY-PT(14))))
C
      YRD=IYD
      T(1)=TINF
      XMM=PDM(5,3)
C
      DO 10 J = 1,10
        IF(MASS.EQ.MT(J))   GO TO 15
10    CONTINUE
      WRITE(6,100) MASS
      GO TO 90
15    IF(ALT.GT.ALTL(6).AND.MASS.NE.28.AND.MASS.NE.48) GO TO 17
C
C       **** N2 DENSITY ****
C
C      Diffusive density at Zlb
      DB28 = PDM(1,3)*DEXP(G28)*PD(1,3)
C      Diffusive density at Alt
      D(3)=DENSU(ALT,DB28,TINF,TLB, 28.D0,0.D0,T(2),ZLB,S,MN1,ZN1,TN1,
     1            TGN1)
      DD=D(3)
C      Turbopause
      ZH28=PDM(3,3)*ZHF
      ZHM28=PDM(4,3)*PDL(6,2)
      XMD=28.D0-XMM
C      Mixed density at Zlb
      B28=DENSU(ZH28,DB28,TINF,TLB,XMD,-1.D0,TZ,ZLB,S,MN1,ZN1,TN1,TGN1)
      IF(ALT.GT.ALTL(3).OR.SW(15).EQ.0.D0) GO TO 17
C      Mixed density at Alt
      DM28=DENSU(ALT,B28,TINF,TLB,XMM,0.D0,TZ,ZLB,S,MN1,ZN1,TN1,TGN1)
C      Net density at Alt
      D(3)=DNET(D(3),DM28,ZHM28,XMM,28.D0)
17    CONTINUE
      GO TO (20,50,20,25,90,35,40,45,25,48),  J
20    CONTINUE
C
C       **** HE DENSITY ****
C
C       Density variation factor at Zlb
      G4 = SW(21)*GLOBE6(YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP,PD(1,1))
C      Diffusive density at Zlb
      DB04 = PDM(1,1)*DEXP(G4)*PD(1,1)
C      Diffusive density at Alt
      D(1)=DENSU(ALT,DB04,TINF,TLB, 4.D0,-0.4D0,T(2),ZLB,S,MN1,ZN1,TN1,
     1            TGN1)
      DD=D(1)
      IF(ALT.GT.ALTL(1).OR.SW(15).EQ.0.D0) GO TO 24
C      Turbopause
      ZH04=PDM(3,1)
      ZHM04=ZHM28
C      Mixed density at Zlb
      B04=DENSU(ZH04,DB04,TINF,TLB,4.D0-XMM,-1.4D0,
     1  T(2),ZLB,S,MN1,ZN1,TN1,TGN1)
C      Mixed density at Alt
      DM04=DENSU(ALT,B04,TINF,TLB,XMM,0.D0,T(2),ZLB,S,MN1,ZN1,TN1,TGN1)
C      Net density at Alt
      D(1)=DNET(D(1),DM04,ZHM04,XMM,4.D0)
C      Correction to specified mixing ratio at ground
      RL=DLOG(B28*PDM(2,1)/B04)
      ZC04=PDM(5,1)*PDL(1,2)
      HC04=PDM(6,1)*PDL(2,2)
C      Net density corrected at Alt
      D(1)=D(1)*CCOR(ALT,RL,HC04,ZC04)
24    CONTINUE
      IF(MASS.NE.48)   GO TO 90
25    CONTINUE
C
C      **** O DENSITY ****
C
C       Density variation factor at Zlb
      G16= SW(21)*GLOBE6(YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP,PD(1,2))
C      Diffusive density at Zlb
      DB16 =  PDM(1,2)*DEXP(G16)*PD(1,2)
C       Diffusive density at Alt
      D(2)=DENSU(ALT,DB16,TINF,TLB, 16.D0,0.D0,T(2),ZLB,S,MN1,ZN1,
     1            TN1,TGN1)
      DD=D(2)
      IF(ALT.GT.ALTL(2).OR.SW(15).EQ.0.D0) GO TO 34
C  Corrected from PDM(3,1) to PDM(3,2)  12/2/85
C       Turbopause
      ZH16=PDM(3,2)
      ZHM16=ZHM28
C      Mixed density at Zlb
      B16=DENSU(ZH16,DB16,TINF,TLB,16.D0-XMM,-1.D0,
     1            T(2),ZLB,S,MN1,ZN1,TN1,TGN1)
C      Mixed density at Alt
      DM16=DENSU(ALT,B16,TINF,TLB,XMM,0.D0,T(2),ZLB,S,MN1,ZN1,TN1,TGN1)
C      Net density at Alt
      D(2)=DNET(D(2),DM16,ZHM16,XMM,16.D0)
C       Correction to specified mixing ratio at ground
      RL=DLOG(B28*PDM(2,2)*DABS(PDL(17,2))/B16)
      HC16=PDM(6,2)*PDL(4,2)
      ZC16=PDM(5,2)*PDL(3,2)
      D(2)=D(2)*CCOR(ALT,RL,HC16,ZC16)
C       Chemistry correction
      HCC16=PDM(8,2)*PDL(14,2)
      ZCC16=PDM(7,2)*PDL(13,2)
      RC16=PDM(4,2)*PDL(15,2)
C      Net density corrected at Alt
      D(2)=D(2)*CCOR(ALT,RC16,HCC16,ZCC16)
34    CONTINUE
      IF(MASS.NE.48 .AND. MASS.NE.49) GO TO 90
35    CONTINUE
C
C       **** O2 DENSITY ****
C
C       Density variation factor at Zlb
      G32= SW(21)*GLOBE6(YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP,PD(1,5))
C      Diffusive density at Zlb
      DB32 = PDM(1,4)*DEXP(G32)*PD(1,5)
C       Diffusive density at Alt
      D(4)=DENSU(ALT,DB32,TINF,TLB, 32.D0,0.D0,T(2),ZLB,S,MN1,ZN1,TN1,
     1            TGN1)
      IF(MASS.EQ.49) THEN
        DD=DD+2.D0*D(4)
      ELSE
        DD=D(4)
      ENDIF
      IF(ALT.GT.ALTL(4).OR.SW(15).EQ.0.D0) GO TO 39
C       Turbopause
      ZH32=PDM(3,4)
      ZHM32=ZHM28
C      Mixed density at Zlb
      B32=DENSU(ZH32,DB32,TINF,TLB,32.D0-XMM,-1.D0,
     1            T(2),ZLB,S,MN1,ZN1,TN1,TGN1)
C      Mixed density at Alt
      DM32=DENSU(ALT,B32,TINF,TLB,XMM,0.D0,T(2),ZLB,S,MN1,ZN1,TN1,TGN1)
C      Net density at Alt
      D(4)=DNET(D(4),DM32,ZHM32,XMM,32.D0)
C       Correction to specified mixing ratio at ground
      RL=DLOG(B28*PDM(2,4)/B32)
      HC32=PDM(6,4)*PDL(8,2)
      ZC32=PDM(5,4)*PDL(7,2)
C      Net density corrected at Alt
      D(4)=D(4)*CCOR(ALT,RL,HC32,ZC32)
39    CONTINUE
      IF(MASS.NE.48)   GO TO 90
40    CONTINUE
C
C       **** AR DENSITY ****
C
C       Density variation factor at Zlb
      G40= SW(21)*GLOBE6(YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP,PD(1,6))
C      Diffusive density at Zlb
      DB40 = PDM(1,5)*DEXP(G40)*PD(1,6)
C       Diffusive density at Alt
      D(5)=DENSU(ALT,DB40,TINF,TLB, 40.D0,0.D0,T(2),ZLB,S,MN1,ZN1,TN1,
     1           TGN1)
      DD=D(5)
      IF(ALT.GT.ALTL(5).OR.SW(15).EQ.0.D0) GO TO 44
C       Turbopause
      ZH40=PDM(3,5)
      ZHM40=ZHM28
C      Mixed density at Zlb
      B40=DENSU(ZH40,DB40,TINF,TLB,40.D0-XMM,-1.D0,
     1  T(2),ZLB,S,MN1,ZN1,TN1,TGN1)
C      Mixed density at Alt
      DM40=DENSU(ALT,B40,TINF,TLB,XMM,0.D0,T(2),ZLB,S,MN1,ZN1,TN1,TGN1)
C      Net density at Alt
      D(5)=DNET(D(5),DM40,ZHM40,XMM,40.D0)
C       Correction to specified mixing ratio at ground
      RL=DLOG(B28*PDM(2,5)/B40)
      HC40=PDM(6,5)*PDL(10,2)
      ZC40=PDM(5,5)*PDL(9,2)
C      Net density corrected at Alt
      D(5)=D(5)*CCOR(ALT,RL,HC40,ZC40)
44    CONTINUE
      IF(MASS.NE.48)   GO TO 90
45    CONTINUE
C
C        **** HYDROGEN DENSITY ****
C
C       Density variation factor at Zlb
      G1 = SW(21)*GLOBE6(YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP,PD(1,7))
C      Diffusive density at Zlb
      DB01 = PDM(1,6)*DEXP(G1)*PD(1,7)
C       Diffusive density at Alt
      D(7)=DENSU(ALT,DB01,TINF,TLB,1.D0,-0.4D0,T(2),ZLB,S,MN1,ZN1,TN1,
     1           TGN1)
      DD=D(7)
      IF(ALT.GT.ALTL(7).OR.SW(15).EQ.0.D0) GO TO 47
C       Turbopause
      ZH01=PDM(3,6)
      ZHM01=ZHM28
C      Mixed density at Zlb
      B01=DENSU(ZH01,DB01,TINF,TLB,1.D0-XMM,-1.4D0,
     1  T(2),ZLB,S,MN1,ZN1,TN1,TGN1)
C      Mixed density at Alt
      DM01=DENSU(ALT,B01,TINF,TLB,XMM,0.D0,T(2),ZLB,S,MN1,ZN1,TN1,TGN1)
C      Net density at Alt
      D(7)=DNET(D(7),DM01,ZHM01,XMM,1.D0)
C       Correction to specified mixing ratio at ground
      RL=DLOG(B28*PDM(2,6)*DABS(PDL(18,2))/B01)
      HC01=PDM(6,6)*PDL(12,2)
      ZC01=PDM(5,6)*PDL(11,2)
      D(7)=D(7)*CCOR(ALT,RL,HC01,ZC01)
C       Chemistry correction
      HCC01=PDM(8,6)*PDL(20,2)
      ZCC01=PDM(7,6)*PDL(19,2)
      RC01=PDM(4,6)*PDL(21,2)
C      Net density corrected at Alt
      D(7)=D(7)*CCOR(ALT,RC01,HCC01,ZCC01)
47    CONTINUE
48    CONTINUE
C
C        **** ATOMIC NITROGEN DENSITY ****
C
C       Density variation factor at Zlb
      G14 = SW(21)*GLOBE6(YRD,SEC,GLAT,GLONG,STL,F107A,F107,AP,PD(1,8))
C      Diffusive density at Zlb
      DB14 = PDM(1,7)*DEXP(G14)*PD(1,8)
C       Diffusive density at Alt
      D(8)=DENSU(ALT,DB14,TINF,TLB,14.D0,0.D0,T(2),ZLB,S,MN1,ZN1,TN1,
     1     TGN1)
      DD=D(8)
      IF(ALT.GT.ALTL(8).OR.SW(15).EQ.0.D0) GO TO 49
C       Turbopause
      ZH14=PDM(3,7)
      ZHM14=ZHM28
C      Mixed density at Zlb
      B14=DENSU(ZH14,DB14,TINF,TLB,14.D0-XMM,-1.D0,
     1  T(2),ZLB,S,MN1,ZN1,TN1,TGN1)
C      Mixed density at Alt
      DM14=DENSU(ALT,B14,TINF,TLB,XMM,0.D0,T(2),ZLB,S,MN1,ZN1,TN1,TGN1)
C      Net density at Alt
      D(8)=DNET(D(8),DM14,ZHM14,XMM,14.D0)
C       Correction to specified mixing ratio at ground
      RL=DLOG(B28*PDM(2,7)*DABS(PDL(3,1))/B14)
      HC14=PDM(6,7)*PDL(2,1)
      ZC14=PDM(5,7)*PDL(1,1)
      D(8)=D(8)*CCOR(ALT,RL,HC14,ZC14)
C       Chemistry correction
      HCC14=PDM(8,7)*PDL(5,1)
      ZCC14=PDM(7,7)*PDL(4,1)
      RC14=PDM(4,7)*PDL(6,1)
C      Net density corrected at Alt
      D(8)=D(8)*CCOR(ALT,RC14,HCC14,ZCC14)
49    CONTINUE
      IF(MASS.NE.48) GO TO 90
C
C       TOTAL MASS DENSITY
C
      D(6) = 1.66D-24*(4.D0*D(1)+16.D0*D(2)+28.D0*D(3)+32.D0*D(4)+
     1            40.D0*D(5)+D(7)+14.D0*D(8))
      DB48=1.66D-24*(4.D0*DB04+16.D0*DB16+28.D0*DB28+32.D0*DB32+
     1            40.D0*DB40+DB01+14.D0*DB14)
      GO TO 90
C       TEMPERATURE AT ALTITUDE
50    CONTINUE
      DDUM=DENSU(ALT,1.D0,TINF,TLB,0.D0,0.D0,T(2),ZLB,S,MN1,ZN1,TN1,
     1           TGN1)
      GO TO 90
90    CONTINUE
C       ADJUST DENSITIES FROM CGS TO KGM
      IF(IMR.EQ.1) THEN
        DO 95 I=1,8
          D(I)=D(I)*1.D6
95      CONTINUE
        D(6)=D(6)/1000.D0
      ENDIF
      ALAST=ALT
      RETURN
100   FORMAT(1X,'MASS', I5, '  NOT VALID')
      ENTRY METER6(METER)
      IMR=0
      IF(METER) IMR=1
      END SUBROUTINE

      END MODULE
