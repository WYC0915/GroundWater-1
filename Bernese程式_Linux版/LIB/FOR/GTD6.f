      MODULE s_GTD6
      CONTAINS

      SUBROUTINE GTD6(IYD,SEC,ALT,GLAT,GLONG,STL,F107A,F107,AP,MASS,D,T)
C        Neutral Atmosphere Empirical Model from the surface to lower
C          exosphere  MSISE90 (JGR, 96, 1159-1172, 1991)
C         A.E.Hedin 4/24/90;6/3/91(add SAVE)
C         2/11/93 correct switch initialization and mks calculation
C           See subroutine GHP6 to specify a pressure rather than
C           altitude.
C     INPUT:
C        IYD - YEAR AND DAY AS YYYYDDD or just DDD (day of year from 1 to 365)
C        SEC - UT(SEC)
C        ALT - ALTITUDE(KM)
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
C            F107, F107A, and AP effects are not large below 80 km
C            and these can be set to 150., 150., and 4. respectively.
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
C      TO GET OUTPUT IN M-3 and KG/M3:   CALL METER6(.TRUE.)
C
C      O, H, and N set to zero below 72.5 km
C      Exospheric temperature set to average for altitudes below 120 km.
C
C           The following is for test and special purposes:
C            TO TURN ON AND OFF PARTICULAR VARIATIONS CALL TSELEC(SW)
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
      USE s_tselec
      USE s_gts6
      USE f_glob6s
      USE f_vtst
      USE s_glatf
      USE f_densm
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*4 (I-N)
C
      DIMENSION D(8),T(2),AP(7),D6(8),T6(2)
      DIMENSION ZN3(5),ZN2(4),SV(25)
      COMMON/GTS3C/TLB,S,DB04,DB16,DB28,DB32,DB40,DB48,DB01,ZA,T0,Z0
     1 ,G0,RL,DD,DB14,TR12
      COMMON/MESO6/TN1(5),TN2(4),TN3(5),TGN1(2),TGN2(2),TGN3(2)
      COMMON/LOWER6/PTM(10),PDM(10,8)
      COMMON/PARM6/PT(150),PD(150,9),PS(150),PDL(25,2),PTL(100,4),
     1 PMA(100,10)
      COMMON/DATIM6/ISD(3),IST(2),NAM(2)
      COMMON/DATIME/ISDATE(3),ISTIME(2),NAME(2)
      COMMON/CSW/SW(25),ISW,SWC(25)
      COMMON/MAVG6/PAVGM(10)
      COMMON/DMIX/DM04,DM16,DM28,DM32,DM40,DM01,DM14
      COMMON/PARMB/GSURF,RE
      COMMON/METSEL/IMR
      SAVE
      EXTERNAL GTD6BK
      DATA MN3/5/,ZN3/32.5D0,20.D0,15.D0,10.D0,0.D0/
      DATA MN2/4/,ZN2/72.5D0,55.D0,45.D0,32.5D0/
      DATA ZMIX/62.5D0/,ALAST/99999.D0/,MSSL/-999/
      DATA SV/25*1.D0/

      IF(ISW.NE.64999) CALL TSELEC(SV)
C      Put identification data into common/datime/
      DO 1 I=1,3
        ISDATE(I)=ISD(I)
1     CONTINUE
      DO 2 I=1,2
        ISTIME(I)=IST(I)
        NAME(I)=NAM(I)
2     CONTINUE
C
Ce        Test for changed input
      V1=VTST(IYD,SEC,GLAT,GLONG,STL,F107A,F107,AP,1)
C       Latitude variation of gravity (none for SW(2)=0)
      XLAT=GLAT
      IF(SW(2).EQ.0) XLAT=45.D0
      CALL GLATF(XLAT,GSURF,RE)
C
      XMM=PDM(5,3)
C
C       THERMOSPHERE/UPPER MESOSPHERE [above ZN2(1)]
      ALTT=DMAX1(ALT,ZN2(1))
      MSS=MASS
Ce       Only calculate N2 in thermosphere if alt in mixed region
      IF(ALT.LT.ZMIX.AND.MASS.GT.0) MSS=28
Ce       Only calculate thermosphere if input parameters changed
Ce         or altitude above ZN2(1) in mesosphere
      IF(V1.EQ.1.D0.OR.ALT.GT.ZN2(1).OR.ALAST.GT.ZN2(1).OR.MSS.NE.MSSL)
     1            THEN
        CALL GTS6(IYD,SEC,ALTT,GLAT,GLONG,STL,F107A,F107,AP,MSS,D6,T6)
        DM28M=DM28
C         metric adjustment
        IF(IMR.EQ.1) DM28M=DM28*1.D6
        MSSL=MSS
      ENDIF
      T(1)=T6(1)
      T(2)=T6(2)
      IF(ALT.GE.ZN2(1)) THEN
        DO 5 J=1,8
          D(J)=D6(J)
    5   CONTINUE
        GOTO 10
      ENDIF
C
C       LOWER MESOSPHERE/UPPER STRATOSPHERE [between ZN3(1) and ZN2(1)]
C         Temperature at nodes and gradients at end nodes
C         Inverse temperature a linear function of spherical harmonics
Ce        Only calculate nodes if input changed
      IF(V1.EQ.1.D0.OR.ALAST.GE.ZN2(1)) THEN
        TGN2(1)=TGN1(2)
        TN2(1)=TN1(5)
        TN2(2)=PMA(1,1)*PAVGM(1)/(1.D0-SW(20)*GLOB6S(PMA(1,1)))
        TN2(3)=PMA(1,2)*PAVGM(2)/(1.D0-SW(20)*GLOB6S(PMA(1,2)))
        TN2(4)=PMA(1,3)*PAVGM(3)/(1.D0-SW(20)*SW(22)*GLOB6S(PMA(1,3)))
       TGN2(2)=PAVGM(9)*PMA(1,10)*(1.D0+SW(20)*SW(22)*GLOB6S(PMA(1,10)))
     1              *TN2(4)*TN2(4)/(PMA(1,3)*PAVGM(3))**2
        TN3(1)=TN2(4)
      ENDIF
      IF(ALT.GE.ZN3(1)) GOTO 6
C
C       LOWER STRATOSPHERE AND TROPOSPHERE [below ZN3(1)]
C         Temperature at nodes and gradients at end nodes
C         Inverse temperature a linear function of spherical harmonics
Ce        Only calculate nodes if input changed
      IF(V1.EQ.1.D0.OR.ALAST.GE.ZN3(1)) THEN
        TGN3(1)=TGN2(2)
        TN3(2)=PMA(1,4)*PAVGM(4)/(1.D0-SW(22)*GLOB6S(PMA(1,4)))
        TN3(3)=PMA(1,5)*PAVGM(5)/(1.D0-SW(22)*GLOB6S(PMA(1,5)))
        TN3(4)=PMA(1,6)*PAVGM(6)/(1.D0-SW(22)*GLOB6S(PMA(1,6)))
        TN3(5)=PMA(1,7)*PAVGM(7)/(1.D0-SW(22)*GLOB6S(PMA(1,7)))
        TGN3(2)=PMA(1,8)*PAVGM(8)*(1.D0+SW(22)*GLOB6S(PMA(1,8)))
     1              *TN3(5)*TN3(5)/(PMA(1,7)*PAVGM(7))**2
      ENDIF
6     CONTINUE
      IF(MASS.EQ.0) GOTO 50
Ce          Linear transition to full mixing at ZMIX from almost
Ce            full mixing at ZN2(1) to improve efficiency
      DMC=0
      IF(ALT.GT.ZMIX) DMC=1.D0-(ZN2(1)-ALT)/(ZN2(1)-ZMIX)
      DZ28=D6(3)
C      ***** N2 DENSITY ****
      DMR=D6(3)/DM28M-1.D0
      D(3)=DENSM(ALT,DM28M,XMM,TZ,MN3,ZN3,TN3,TGN3,MN2,ZN2,TN2,TGN2)
      D(3)=D(3)*(1.D0+DMR*DMC)
C      ***** HE DENSITY ****
      D(1)=0.D0
      IF(MASS.NE.4.AND.MASS.NE.48) GOTO 204
      DMR=D6(1)/(DZ28*PDM(2,1))-1.D0
      D(1)=D(3)*PDM(2,1)*(1.D0+DMR*DMC)
204   CONTINUE
C      **** O DENSITY ****
        D(2)=0.D0
  216   CONTINUE
C      ***** O2 DENSITY ****
        D(4)=0.D0
        IF(MASS.NE.32.AND.MASS.NE.48) GOTO 232
          DMR=D6(4)/(DZ28*PDM(2,4))-1.D0
          D(4)=D(3)*PDM(2,4)*(1.D0+DMR*DMC)
  232   CONTINUE
C      ***** AR DENSITY ****
        D(5)=0.D0
        IF(MASS.NE.40.AND.MASS.NE.48) GOTO 240
          DMR=D6(5)/(DZ28*PDM(2,5))-1.D0
          D(5)=D(3)*PDM(2,5)*(1.D0+DMR*DMC)
  240   CONTINUE
C      ***** HYDROGEN DENSITY ****
        D(7)=0.D0
C      ***** ATOMIC NITROGEN DENSITY ****
        D(8)=0.D0
C
C       TOTAL MASS DENSITY
C
        IF(MASS.EQ.48) THEN
         D(6) = 1.66D-24*(4.D0*D(1)+16.D0*D(2)+28.D0*D(3)+32.D0*D(4)+
     1                40.D0*D(5)+D(7)+14.D0*D(8))
         IF(IMR.EQ.1) D(6)=D(6)/1000.D0
        ENDIF
        T(2)=TZ
   10 CONTINUE
      GOTO 90
   50 CONTINUE
      DD=DENSM(ALT,1.D0,0.D0,TZ,MN3,ZN3,TN3,TGN3,MN2,ZN2,TN2,TGN2)
      T(2)=TZ
   90 CONTINUE
      ALAST=ALT
      RETURN
      END SUBROUTINE

      END MODULE
