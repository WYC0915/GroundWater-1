      MODULE s_SUN20
      CONTAINS

C*
      SUBROUTINE SUN20(XMJD,X,R,L,B)
CC
CC NAME       :  SUN20
CC
CC PURPOSE    :  COMPUTATION OF POSITION OF THE SUN AT TIME XMJD
CC               (MODIFIED JULIAN DATE). THIS SR WAS WRITTEN USING
CC               SIMON NEWCOMB'S "TABLES OF THE SUN".
CC
CC               PRECISION  :        MAXIMUM        MEAN
CC                                       DIFFERENCES
CC                              L      .08"         .03"
CC                              B      .03"         .005"
CC                              R     3.E-7        .5E-7
CC
CC PARAMETERS :
CC         IN :  XMJD   : EPOCH IN MODIFIED JULIAN DATE IN    R*8
CC                        BARYCENTRIC DYNAMICAL TIME
CC                        CORRESPONDING TO EPHEMERIS TIME
CC        OUT :  X(K),K=1,2,3 : RECTANGULAR COORDINATES OF    R*8
CC                        THE SUN IN EQUATORIAL SYSTEM
CC                        J2000.0 (IN AU)
CC               R      : DISTANCE EARTH-SUN (IN AU)          R*8
CC               L , B  : ECLIPTICAL LONGITUDE, LATITUDE IN   R*8
CC                        MEAN SYSTEM OF EPOCH XMJD (FK4!)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER, U.HUGENTOBLER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  31-MAY-92
CC
CC CHANGES    :  23-SEP-93 : ??: DECLARATION OF "PR" AND "VR" AS REAL*8
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               28-FEB-07 : AG: USE PI, RHO AND ARS FROM DEFCON
CC               04-MAY-12 : RD: USE DMOD FROM MODULE
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1992     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE d_const,  ONLY: pi,rho,ars
      USE l_basfun, ONLY: dmod
      USE s_prae
      USE s_fk4fk5
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , K
C
      REAL*8    BMO   , C     , D     , DEPOCH, F     , GE    , GJ    ,
     1          GM    , GME   , GS    , GV    , RJ    , RM    ,
     2          RME   , RMO   , RS    , RV    , TT    , US    , W     ,
     3          W1    , W2    , WH    , XBJ   , XBM   , XBS   , XBV   ,
     4          XL1   , XLJ   , XLM   , XLME  , XLMO  , XLS   , XLV   ,
     5          XM
C
        REAL*8 XMJD,T,L,B,R,X(3),Y(3),GD,EKL,TH,PRAEZ(3,3),V(3)
        REAL*8 PR,VR
        INTEGER*4 JME(4),IME(4),S1ME(4),K1ME(4),S2ME(4),K2ME(4),
     1            JV(39),IV(39),S1V(39),K1V(39),S2V(39),K2V(39),
     2            JM(45),IM(45),S1M(45),K1M(45),S2M(45),K2M(45),
     3            JJ(21),IJ(21),S1J(21),K1J(21),S2J(21),K2J(21),
     4            JS(11),IS(11),S1S(11),K1S(11),S2S(11),K2S(11)
        INTEGER*4 JBV(22),IBV(22),SBV(22),KBV(22),
     1            JBM(3),IBM(3),SBM(3),KBM(3),
     2            JBJ(7),IBJ(7),SBJ(7),KBJ(7),
     3            JBS(2),IBS(2),SBS(2),KBS(2)
        DATA JME/4*-1/
        DATA IME/1,2,3,4/
        DATA S1ME/13,5,15,23/
        DATA K1ME/243,225,357,326/
        DATA S2ME/28,6,18,5/
        DATA K2ME/335,130,267,239/
        DATA JV/4*-1,5*-2,5*-3,5*-4,4*-5,4*-6,4*-7,5*-8,2*-9,-10/
        DATA IV/0,1,2,3,0,1,2,3,4,2,3,4,5,6,3,4,5,6,7,5,6,7,8,
     1          6,7,8,9,7,8,9,10,8,9,12,13,14,9,10,10/
        DATA S1V/75,4838,74,9,3,116,5526,2497,44,13,666,1559,
     1           1024,17,3,210,144,152,6,84,37,123,154,38,14,
     2           10,14,20,6,3,0,11,0,42,0,32,6,0,3/
        DATA K1V/2962,2991,2076,2490,1620,1488,1483,3159,3123,
     1           1760,1777,3453,3182,3150,1980,2062,1954,3438,
     2           3220,2356,2218,1953,3596,2641,2530,2300,
     3           120,2940,2790,2880,0,3220,0,2592,0,488,
     4           3510,0,180/
        DATA S2V/94,2359,69,16,4,160,6842,869,52,21,1045,1497,194,
     1           19,6,376,196,94,6,163,59,141,26,80,25,14,12,42,12,
     2           4,4,24,6,44,12,33,13,4,8/
        DATA K2V/2050,2091,3485,3300,900,584,583,2267,388,900,
     1           876,2552,495,430,900,1163,1052,2548,590,1454,
     2           1322,1054,2700,1743,1640,1350,2840,2035,1940,
     3           1660,1350,2340,2180,1697,2220,1387,2610,2560,
     4           2930/
        DATA JM/3*1,4*2,4*3,4*4,4*5,4*6,3*7,4*8,3*9,3*10,
     1          2*11,12,2*13,2*15,2*17/
        DATA IM/2,1,0,3,2,1,0,4,3,2,1,4,3,2,1,5,4,3,2,6,5,4,3,
     1          6,5,4,7,6,5,4,7,6,5,7,6,5,7,6,7,8,7,
     2          9,8,10,9/
        DATA S1M/6,273,48,41,2043,1770,28,4,129,425,8,34,500,585,
     1           9,7,85,204,3,0,20,154,101,6,49,106,3,10,52,21,4,
     2           28,62,5,19,5,17,44,6,13,45,21,0,4,26/
        DATA K1M/2180,2177,2603,3460,3439,2004,1480,2840,2942,
     1           3389,70,710,1052,3341,3250,1720,546,1008,180,
     2           0,1860,2274,963,3010,1765,2227,720,3070,3489,
     3           2152,570,2980,3460,680,1110,3380,590,1059,2320,
     4           1840,2278,3090,0,2430,1130/
        DATA S2M/8,150,28,52,2057,151,31,6,168,215,6,49,478,105,
     1           10,12,107,89,3,5,30,139,27,10,60,38,5,15,45,8,
     2           6,34,17,8,15,0,20,9,5,15,5,22,6,4,0/
        DATA K2M/1300,1277,3470,2554,2538,2950,2343,1800,2035,
     1           2490,900,3397,152,659,530,900,3246,110,1080,
     2           2170,957,1373,1880,2090,862,1329,3490,2170,2597,
     3           3100,3290,2081,2570,3370,230,0,3300,210,1430,
     4           940,1430,2200,2610,1530,0/
        DATA JJ/5*1,4*2,4*3,4*4,4*5/
        DATA IJ/-3,-2,-1,0,1,-3,-2,-1,0,-4,-3,-2,-1,-4,-3,-2,-1,
     1           -5,-4,-3,-2/
        DATA S1J/3,163,7208,2600,73,69,2731,1610,73,5,164,556,210,
     1          16,44,80,23,0,5,7,9/
        DATA K1J/1980,1986,1795,2632,2763,808,871,1095,2526,1580,
     2          1705,827,985,2590,1682,777,930,0,2590,1640,710/
        DATA S2J/5,208,7067,244,80,103,4026,1459,8,9,281,803,174,29,
     1           74,113,17,3,10,12,14/
        DATA K2J/1120,1120,895,3386,65,3505,3571,195,2630,
     2           690,812,3526,86,1700,799,3477,30,2520,1690,760,
     3     3430/
        DATA JS/4*1,4*2,2*3,4/
        DATA IS/-2,-1,0,1,-3,-2,-1,0,-2,-1,-2/
        DATA S1S/11,419,320,8,0,108,112,17,21,17,3/
        DATA K1S/1050,1006,2695,2700,0,2906,2936,2770,2890,
     1          2910,2880/
        DATA S2S/15,429,8,8,3,162,112,0,32,17,4/
        DATA K2S/110,106,3530,0,1980,2006,2031,0,2001,2010,
     1           1940/
        DATA JBV/4*-1,4*-2,5*-3,3*-4,2*-5,3*-6,-8/
        DATA IBV/0,1,2,3,1,2,3,4,2,3,4,5,6,3,5,6,6,7,
     1           5,7,8,12/
        DATA SBV/29,5,92,7,23,12,67,14,14,8,210,7,4,6,31,
     1           12,9,19,6,4,4,10/
        DATA KBV/1450,3230,937,2620,1730,1490,1230,1110,2010,
     1           1870,1518,1530,2960,2320,18,1800,270,180,2880,
     2           570,570,610/
        DATA JBM/2*2,4/
        DATA IBM/-2,0,-3/
        DATA SBM/8,8,7/
        DATA KBM/900,3460,1880/
        DATA JBJ/4*1,2,2*3/
        DATA IBJ/-2,-1,0,1,-1,-2,-1/
        DATA SBJ/7,17,16,23,166,6,18/
        DATA KBJ/1800,2730,1800,2680,2655,1710,2670/
        DATA JBS/2*1/
        DATA IBS/-1,1/
        DATA SBS/6,6/
        DATA KBS/2600,2800/
        T=(XMJD-15019.5D0)/36525.D0
        TT=T
        L=279.D0+41.D0/60+48.04D0/3600
        L=L+(129602768.13D0*T+1.089D0*T**2)/3600
        L=DMOD(L/rho,2*PI)
        GD=358.D0+28.D0/60+33.D0/3600
        GD=GD+(129596579.1D0*T-.54D0*T**2-.012D0*T**3)/3600
        GE=DMOD(GD/rho,2*PI)
        TH=(XMJD+3242.297D0)/365.25D0
        GME=DMOD((248.07D0+1494.7235D0*TH)/rho,2*PI)
        GV =DMOD((63.07037D0+22518.442986D0*T)/rho,2*PI)
        GJ =DMOD((221.64742D0+32964.466939D0*T)/rho,2*PI)
        GS =DMOD((193.13230D0+34777.259042D0*T)/rho,2*PI)
        GM =DMOD((165.94905D0+16859.069667D0*T)/rho,2*PI)
        XL1=6.4*SIN((231.19+20.2*TT)/rho)
     1      +(1.882-.016*TT)*SIN((57.24+150.27*TT)/rho)
     2      +.266*SIN((31.8+119.0*TT)/rho)
     3      +.202*SIN((315.6+893.3*TT)/rho)
        L=L+XL1/ars
        GE=GE+XL1/ars
        GV=GV-XL1/ars
        GJ=GJ+XL1/ars
        GS=GS+XL1/ars
        W1=299.1+(GV-GE)*rho
        GD=63.07037D0+22518.442986D0*T
        W2=90.+DMOD(GD,360.D0)
        C=(6910.057-17.24*TT-.052*TT**2)*SIN(GE)
     1  +(72.338-.361*TT)*SIN(2*GE)
     2    +(1.054-.001*TT)*SIN(3*GE)+.018*SIN(4*GE)
        R=3057-15*T+COS(GE)*(-727412.D0+1814*T+5*T**2)
     1    +COS(2*GE)*(-9138+46*T)+COS(3*GE)*(-145+T)
     2    +COS(4*GE)*(-2)
        XLME=0
        RME=0
        DO 10 K=1,4
        W=-(JME(K)*GME+IME(K)*GE)
        W1=K1ME(K)/rho
        W2=K2ME(K)/rho
        XLME=XLME+S1ME(K)*COS(W+W1)
10      RME=RME+S2ME(K)*COS(W+W2)
        XLV=0
        RV=0
        DO 20 K=1,39
        W=-(JV(K)*GV+IV(K)*GE)
        W1=K1V(K)/10.d0/rho
        W2=K2V(K)/10.d0/rho
        IF(K.EQ.2)W1=299.1017/rho
        IF(K.EQ.7)W1=148.3133/rho
        IF(K.EQ.8)W1=315.9433/rho
        IF(K.EQ.12)W1=345.2533/rho
        IF(K.EQ.11)W1=177.71/rho
        IF(K.EQ.13)W1=318.12/rho
        IF(K.EQ.2)W2=209.08/rho
        IF(K.EQ.7)W2=58.3183/rho
        IF(K.EQ.11)W2=87.57/rho
        IF(K.EQ.12)W2=255.25/rho
        IF(K.EQ.16)W2=116.28/rho
        WH=-JV(K)*330.9017/rho
     1    -(IV(K)+JV(K))*GE-JV(K)*(GV+PI)
        XLV=XLV+S1V(K)*COS(WH+W1)
20      RV=RV+S2V(K)*COS(WH+W2)
        XLM=0
        RM=0
        DO 30 K=1,45
        W=(-JM(K)*GM+IM(K)*GE)
        W1=K1M(K)/10.d0/rho
        W2=K2M(K)/10.d0/rho
        IF(K.EQ.5)W1=343.8883/rho
        IF(K.EQ.6)W1=200.4017/rho
        IF(K.EQ.10)W1=338.88/rho
        IF(K.EQ.13)W1=105.18/rho
        IF(K.EQ.14)W1=334.05/rho
        IF(K.EQ.5)W2=253.8283/rho
        IF(K.EQ.13)W2=15.17/rho
        WH=-JM(K)*127.0633/rho+(IM(K)-JM(K))*GE+JM(K)*GM
        XLM=XLM+S1M(K)*COS(WH+W1)
30      RM=RM+S2M(K)*COS(WH+W2)
        XLJ=0
        RJ=0
        DO 40 K=1,21
        W=-(JJ(K)*GJ+IJ(K)*GE)
        W1=K1J(K)/10.d0/rho
        W2=K2J(K)/10.d0/rho
        IF(K.EQ.3)W1=179.5317/rho
        IF(K.EQ.4)W1=263.2167/rho
        IF(K.EQ.7)W1=87.145/rho
        IF(K.EQ.8)W1=109.4933/rho
        IF(K.EQ.12)W1=82.65/rho
        IF(K.EQ.3)W2=89.545/rho
        IF(K.EQ.7)W2=357.1083/rho
        IF(K.EQ.8)W2=19.4667/rho
        IF(K.EQ.12)W2=352.56/rho
        WH=-JJ(K)*88.4450/rho-(JJ(K)+IJ(K))*GE+JJ(K)*GJ
        XLJ=XLJ+S1J(K)*COS(WH+W1)
40      RJ=RJ+S2J(K)*COS(WH+W2)
        XLS=0
        RS=0
        DO 50 K=1,11
        W=-(JS(K)*GS+IS(K)*GE)
        W1=K1S(K)/10.d0/rho
        W2=K2S(K)/10.d0/rho
        IF(K.EQ.2)W1=100.58/rho
        IF(K.EQ.3)W1=269.46/rho
        WH=-JS(K)*10.2417/rho-(IS(K)+JS(K))*GE+JS(K)*GS
        XLS=XLS+S1S(K)*COS(WH+W1)
50      RS=RS+S2S(K)*COS(WH+W2)
        XBV=0
        DO 60 K=1,22
        W=(KBV(K)/10.-JBV(K)*330.9017)/rho
     1    -(IBV(K)+JBV(K))*GE-JBV(K)*(GV+PI)
60      XBV=XBV+SBV(K)*COS(W)
        XBM=0
        DO 70 K=1,3
        W=(KBM(K)/10.-JBM(K)*127.0633)/rho
     1     -(IBM(K)+JBM(K))*GE+JBM(K)*GM
70      XBM=XBM+SBM(K)*COS(W)
        XBJ=0
        DO 80 K=1,7
        W=(KBJ(K)/10.-JBJ(K)*88.445)/rho
     1    -(IBJ(K)+JBJ(K))*GE+JBJ(K)*GJ
80      XBJ=XBJ+SBJ(K)*COS(W)
        XBS=0
        DO 90 K=1,2
        W=(KBS(K)/10.-JBS(K)*10.2417)/rho
     1    -(IBS(K)+JBS(K))*GE+JBS(K)*GS
90      XBS=XBS+SBS(K)*COS(W)
C MONDSTOERUNGEN
        GD=296.104608D0+477198.849108D0*T+.9192D-2*T**2+14.D-6*T**3
        XM=DMOD(GD/rho,2*PI)
        GD=350.737486D0+445267.114217D0*T-.1436D-2*T**2+2.D-6*T**3
        D=DMOD(GD/rho,2*PI)
        GD=11.D0+15.D0/60+3.2D0/3600
     1     +(1739527290.54D0*T-11.56D0*T**2-.12D-2*T**3)/3600
        F=DMOD(GD/rho,2*PI)
        GD=259+10.D0/60+59.79D0/3600-6962911.23D0*T/3600
     2     +(7.48*T**2+.0086*T**3)/3600
        US=L-DMOD(GD/rho,2*PI)
        XLMO=6.454*SIN(D)+.013*SIN(3*D)+.177*SIN(D+XM)
     1       -.424*SIN(D-XM)+.039*SIN(3*D-XM)-.064*SIN(D+GE)
     2       +.172*SIN(D-GE)-.013*SIN(D-XM-GE)-.013*SIN(2*US)
        RMO=1336*COS(D)+3*COS(3*D)+37*COS(D+XM)-133*COS(D-XM)
     1      +8*COS(3*D-XM)-14*COS(D+GE)+36*COS(D-GE)
     2     -3*COS(D-GE-XM)+3*COS(2*US)
        BMO=.576*SIN(F)+.016*SIN(F+XM)-.047*SIN(F-XM)
     1      +.021*SIN(F-2*US)+.005*SIN(F-2*US-XM)
     2      +.005*SIN(F+GE)+.005*SIN(F-GE)
        L=L+(XLME+XLV+XLM+XLJ+XLS)/1.D3/ars
        L=L+(C+XLMO)/ars
        B=(XBV+XBM+XBJ+XBS)/1.D3/ars
        B=-B+BMO/ars
        R=(R+(RME+RV+RM+RJ+RS)/10+RMO)*1.D-8
        R=10.D0**R
        EKL=(23.D0+27.D0/60+8.26D0/3600)
        EKL=EKL-(46.845*T+.59D-2*T**2-.181D-2*T**3)/3600
        EKL=EKL/rho
        X(1)=DCOS(L)*DCOS(B)
        X(2)=DSIN(L)*DCOS(B)
        X(3)=DSIN(B)
        Y(1)=X(1)
        Y(2)=X(2)*DCOS(EKL)-X(3)*DSIN(EKL)
        Y(3)=X(2)*DSIN(EKL)+X(3)*DCOS(EKL)
C PRECESSION TO 1950.0
        CALL PRAE(XMJD,PRAEZ)
        DO 77 I=1,3
          X(I)=0.D0
          V(I)=0.D0
          DO 77 K=1,3
            X(I)=X(I)+PRAEZ(K,I)*Y(K)
77      CONTINUE
C TRANSFORMATION FROM B1950.0 TO J2000.0
        CALL FK4FK5(X,V,0D0,0D0,X,V,PR,VR)
        DEPOCH=(XMJD-51544.5D0)/36525.D0
        DO 79 I=1,3
          X(I)=R*(X(I)+V(I)*DEPOCH)
79      CONTINUE
C
        RETURN
        END SUBROUTINE

      END MODULE


