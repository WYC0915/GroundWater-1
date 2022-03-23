      MODULE f_globe6
      CONTAINS

      FUNCTION GLOBE6(YRD,SEC,LAT,LONG,TLOC,F107A,F107,AP,P)
C       CALCULATE G(L) FUNCTION
C       Upper Thermosphere Parameters
C
      USE s_tselec
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*4 (I-N)
C
      REAL*8 LAT, LONG,LONGL
      DIMENSION P(150),SV(25),AP(7)
      COMMON/TTEST/TINF,GB,ROUT,T(15)
      COMMON/CSW/SW(25),ISW,SWC(25)
      COMMON/LPOLY/PLG(9,4),CTLOC,STLOC,C2TLOC,S2TLOC,C3TLOC,S3TLOC,
     1 IYR,DAY,DF,DFA,APD,APDF,APT(4),XLONG,CLONG,SLONG
      SAVE
      DATA DGTR/1.74533D-2/,DR/1.72142D-2/, XL/1000.D0/,TLL/1000.D0/
      DATA SW9/1.D0/,DAYL/-1.D0/,P14/-1000.D0/,P18/-1000.D0/
      DATA P32/-1000.D0/
      DATA HR/.2618D0/,SR/7.2722D-5/,SV/25*1.D0/,NSW/14/,P39/-1000.D0/
      DATA LONGL/-999.D0/
C       3hr Magnetica activity functions
      G0(A)=(A-4.D0+(P(26)-1.D0)*(A-4.D0+(DEXP(-DABS(P(25))*(A-4.D0))-
     1            1.D0)/DABS(P(25))))
      SUMEX(EX)=1.D0+(1.D0-EX**19)/(1.D0-EX)*EX**(.5D0)
      SG0(EX)=(G0(AP(2))+(G0(AP(3))*EX+G0(AP(4))*EX*EX+G0(AP(5))*EX**3
     1       +(G0(AP(6))*EX**4+G0(AP(7))*EX**12)*(1.D0-EX**8)/(1.D0-EX))
     1       )/SUMEX(EX)
C
      IF(ISW.NE.64999) CALL TSELEC(SV)
      DO 10 J=1,14
        T(J)=0
10    CONTINUE
      IF(SW(9).GT.0D0) SW9=1.D0
      IF(SW(9).LT.0D0) SW9=-1.D0
      IYR = YRD/1000.D0
      DAY = YRD - IYR*1000.D0
      XLONG=LONG
      IF(XL.EQ.LAT)   GO TO 15
C          CALCULATE LEGENDRE POLYNOMIALS
      C = DSIN(LAT*DGTR)
      S = DCOS(LAT*DGTR)
      C2 = C*C
      C4 = C2*C2
      S2 = S*S
      PLG(2,1) = C
      PLG(3,1) = 0.5*(3.*C2 -1.D0)
      PLG(4,1) = 0.5*(5.D0*C*C2-3.D0*C)
      PLG(5,1) = (35.D0*C4 - 30.D0*C2 + 3.D0)/8.D0
      PLG(6,1) = (63.D0*C2*C2*C - 70.D0*C2*C + 15.D0*C)/8.D0
      PLG(7,1) = (11.D0*C*PLG(6,1) - 5.D0*PLG(5,1))/6.D0
C     PLG(8,1) = (13.D0*C*PLG(7,1) - 6.D0*PLG(6,1))/7.D0
      PLG(2,2) = S
      PLG(3,2) = 3.D0*C*S
      PLG(4,2) = 1.5*(5.D0*C2-1.D0)*S
      PLG(5,2) = 2.5*(7.D0*C2*C-3.D0*C)*S
      PLG(6,2) = 1.875*(21.D0*C4 - 14.D0*C2 +1.D0)*S
      PLG(7,2) = (11.D0*C*PLG(6,2)-6.D0*PLG(5,2))/5.D0
C     PLG(8,2) = (13.D0*C*PLG(7,2)-7.D0*PLG(6,2))/6.D0
C     PLG(9,2) = (15.D0*C*PLG(8,2)-8.D0*PLG(7,2))/7.D0
      PLG(3,3) = 3.D0*S2
      PLG(4,3) = 15.D0*S2*C
      PLG(5,3) = 7.5*(7.D0*C2 -1.D0)*S2
      PLG(6,3) = 3.D0*C*PLG(5,3)-2.D0*PLG(4,3)
      PLG(7,3)=(11.D0*C*PLG(6,3)-7.D0*PLG(5,3))/4.D0
      PLG(8,3)=(13.D0*C*PLG(7,3)-8.D0*PLG(6,3))/5.D0
      PLG(4,4) = 15.D0*S2*S
      PLG(5,4) = 105.D0*S2*S*C
      PLG(6,4)=(9.D0*C*PLG(5,4)-7.D0*PLG(4,4))/2.D0
      PLG(7,4)=(11.D0*C*PLG(6,4)-8.D0*PLG(5,4))/3.D0
      XL=LAT
15    CONTINUE
      IF(TLL.EQ.TLOC)   GO TO 16
      IF(SW(7).EQ.0.D0.AND.SW(8).EQ.0.D0.AND.SW(14).EQ.0.D0) GOTO 16
      STLOC = DSIN(HR*TLOC)
      CTLOC = DCOS(HR*TLOC)
      S2TLOC = DSIN(2.D0*HR*TLOC)
      C2TLOC = DCOS(2.D0*HR*TLOC)
      S3TLOC = DSIN(3.D0*HR*TLOC)
      C3TLOC = DCOS(3.D0*HR*TLOC)
      TLL = TLOC
16    CONTINUE
      IF(LONG.NE.LONGL) THEN
        CLONG=DCOS(DGTR*LONG)
        SLONG=DSIN(DGTR*LONG)
      ENDIF
      LONGL=LONG
      IF(DAY.NE.DAYL.OR.P(14).NE.P14) CD14=DCOS(DR*(DAY-P(14)))
      IF(DAY.NE.DAYL.OR.P(18).NE.P18) CD18=DCOS(2.D0*DR*(DAY-P(18)))
      IF(DAY.NE.DAYL.OR.P(32).NE.P32) CD32=DCOS(DR*(DAY-P(32)))
      IF(DAY.NE.DAYL.OR.P(39).NE.P39) CD39=DCOS(2.D0*DR*(DAY-P(39)))
      DAYL = DAY
      P14 = P(14)
      P18 = P(18)
      P32 = P(32)
      P39 = P(39)
C         F10.7 EFFECT
      DF = F107 - F107A
      DFA=F107A-150.D0
      T(1) =  P(20)*DF + P(21)*DF*DF + P(22)*DFA
     1            + P(30)*DFA**2
      F1 = 1.D0 + (P(48)*DFA +P(20)*DF+P(21)*DF*DF)*SWC(1)
      F2 = 1.D0 + (P(50)*DFA+P(20)*DF+P(21)*DF*DF)*SWC(1)
C        TIME INDEPENDENT
      T(2) =
     1            (P(2)*PLG(3,1) + P(3)*PLG(5,1)+P(23)*PLG(7,1))
     2            +(P(15)*PLG(3,1))*DFA*SWC(1)
     3            +P(27)*PLG(2,1)
C        SYMMETRICAL ANNUAL
      T(3) =
     1            (P(19) )*CD32
C        SYMMETRICAL SEMIANNUAL
      T(4) =
     1            (P(16)+P(17)*PLG(3,1))*CD18
C        ASYMMETRICAL ANNUAL
      T(5) =  F1*
     1            (P(10)*PLG(2,1)+P(11)*PLG(4,1))*CD14
C         ASYMMETRICAL SEMIANNUAL
      T(6) =    P(38)*PLG(2,1)*CD39
C        DIURNAL
      IF(SW(7).EQ.0D0) GOTO 200
      T71 = (P(12)*PLG(3,2))*CD14*SWC(5)
      T72 = (P(13)*PLG(3,2))*CD14*SWC(5)
      T(7) = F2*
     1            ((P(4)*PLG(2,2) + P(5)*PLG(4,2) + P(28)*PLG(6,2)
     2            + T71)*CTLOC
     4            + (P(7)*PLG(2,2) + P(8)*PLG(4,2) +P(29)*PLG(6,2)
     5            + T72)*STLOC)
200   CONTINUE
C        SEMIDIURNAL
      IF(SW(8).EQ.0D0) GOTO 210
      T81 = (P(24)*PLG(4,3)+P(36)*PLG(6,3))*CD14*SWC(5)
      T82 = (P(34)*PLG(4,3)+P(37)*PLG(6,3))*CD14*SWC(5)
      T(8) = F2*
     1            ((P(6)*PLG(3,3) + P(42)*PLG(5,3) + T81)*C2TLOC
     3            +(P(9)*PLG(3,3) + P(43)*PLG(5,3) + T82)*S2TLOC)
210   CONTINUE
C        TERDIURNAL
      IF(SW(14).EQ.0D0) GOTO 220
      T(14) = F2*
     1    ((P(40)*PLG(4,4)+(P(94)*PLG(5,4)+P(47)*PLG(7,4))*CD14*SWC(5))*
     2            S3TLOC
     3    +(P(41)*PLG(4,4)+(P(95)*PLG(5,4)+P(49)*PLG(7,4))*CD14*SWC(5))*
     4            C3TLOC)
220   CONTINUE
C          MAGNETIC ACTIVITY BASED ON DAILY AP

      IF(SW9.EQ.-1.D0) GO TO 30
      APD=(AP(1)-4.D0)
      P44=P(44)
      P45=P(45)
      IF(P44.LT.0D0) P44=1.D-5
      APDF = (APD+(P45-1.D0)*(APD+(DEXP(-P44  *APD)-1.D0)/P44  ))
      IF(SW(9).EQ.0D0) GOTO 40
      T(9)=APDF*(P(33)+P(46)*PLG(3,1)+P(35)*PLG(5,1)+
     1   (P(101)*PLG(2,1)+P(102)*PLG(4,1)+P(103)*PLG(6,1))*CD14*SWC(5)+
     2   (P(122)*PLG(2,2)+P(123)*PLG(4,2)+P(124)*PLG(6,2))*SWC(7)*
     3   DCOS(HR*(TLOC-P(125))))
      GO TO 40
30    CONTINUE
      IF(P(52).EQ.0D0) GO TO 40
      EXP1 = DEXP(-10800.D0*DABS(P(52))/(1.D0+P(139)*(45.D0-DABS(LAT))))
      IF(EXP1.GT..99999D0) EXP1=.99999D0
      EXP2 = DEXP(-10800.D0*DABS(P(54)))
      IF(EXP2.GT..99999D0) EXP2=.99999D0
      IF(P(25).LT.1.D-4) P(25)=1.D-4
      APT(1)=SG0(EXP1)
      APT(3)=SG0(EXP2)
      IF(SW(9).EQ.0D0) GOTO 40
      T(9) = APT(1)*(P(51)+P(97)*PLG(3,1)+P(55)*PLG(5,1)+
     1 (P(126)*PLG(2,1)+P(127)*PLG(4,1)+P(128)*PLG(6,1))*CD14*SWC(5)+
     2 (P(129)*PLG(2,2)+P(130)*PLG(4,2)+P(131)*PLG(6,2))*SWC(7)*
     3 DCOS(HR*(TLOC-P(132))))
40    CONTINUE
      IF(SW(10).EQ.0.OR.LONG.LE.-1000.D0) GO TO 49
C        LONGITUDINAL
      IF(SW(11).EQ.0D0) GOTO 230
      T(11)= (1.D0+P(81)*DFA*SWC(1))*
     1((P(65)*PLG(3,2)+P(66)*PLG(5,2)+P(67)*PLG(7,2)
     2 +P(104)*PLG(2,2)+P(105)*PLG(4,2)+P(106)*PLG(6,2)
     3 +SWC(5)*(P(110)*PLG(2,2)+P(111)*PLG(4,2)+P(112)*PLG(6,2))*CD14)*
     4     CLONG
     5 +(P(91)*PLG(3,2)+P(92)*PLG(5,2)+P(93)*PLG(7,2)
     6 +P(107)*PLG(2,2)+P(108)*PLG(4,2)+P(109)*PLG(6,2)
     7 +SWC(5)*(P(113)*PLG(2,2)+P(114)*PLG(4,2)+P(115)*PLG(6,2))*CD14)*
     8  SLONG)
230   CONTINUE
C        UT AND MIXED UT,LONGITUDE
      IF(SW(12).EQ.0D0) GOTO 240
      T(12)=(1.D0+P(96)*PLG(2,1))*(1.D0+P(82)*DFA*SWC(1))*
     1            (1.D0+P(120)*PLG(2,1)*SWC(5)*CD14)*
     2            ((P(69)*PLG(2,1)+P(70)*PLG(4,1)+P(71)*PLG(6,1))*
     3            DCOS(SR*(SEC-P(72))))
      T(12)=T(12)+SWC(11)*
     1            (P(77)*PLG(4,3)+P(78)*PLG(6,3)+P(79)*PLG(8,3))*
     2      DCOS(SR*(SEC-P(80))+2.D0*DGTR*LONG)*(1.D0+P(138)*DFA*SWC(1))
240   CONTINUE
C        UT,LONGITUDE MAGNETIC ACTIVITY
      IF(SW(13).EQ.0D0) GOTO 48
      IF(SW9.EQ.-1.D0) GO TO 45
      T(13)= APDF*SWC(11)*(1.D0+P(121)*PLG(2,1))*
     1            ((P( 61)*PLG(3,2)+P( 62)*PLG(5,2)+P( 63)*PLG(7,2))*
     2            DCOS(DGTR*(LONG-P( 64))))
     3 +APDF*SWC(11)*SWC(5)*
     4 (P(116)*PLG(2,2)+P(117)*PLG(4,2)+P(118)*PLG(6,2))*
     5            CD14*DCOS(DGTR*(LONG-P(119)))
     6            + APDF*SWC(12)*
     7            (P( 84)*PLG(2,1)+P( 85)*PLG(4,1)+P( 86)*PLG(6,1))*
     8            DCOS(SR*(SEC-P( 76)))
      GOTO 48
45    CONTINUE
      IF(P(52).EQ.0D0) GOTO 48
      T(13)=APT(1)*SWC(11)*(1.D0+P(133)*PLG(2,1))*
     1            ((P(53)*PLG(3,2)+P(99)*PLG(5,2)+P(68)*PLG(7,2))*
     2            DCOS(DGTR*(LONG-P(98))))
     3            +APT(1)*SWC(11)*SWC(5)*
     4            (P(134)*PLG(2,2)+P(135)*PLG(4,2)+P(136)*PLG(6,2))*
     5            CD14*DCOS(DGTR*(LONG-P(137)))
     6            +APT(1)*SWC(12)*
     7            (P(56)*PLG(2,1)+P(57)*PLG(4,1)+P(58)*PLG(6,1))*
     8            DCOS(SR*(SEC-P(59)))
48    CONTINUE
49    CONTINUE
      TINF=P(31)
      DO 50 I = 1,NSW
        TINF = TINF + DABS(SW(I))*T(I)
50    CONTINUE
      GLOBE6 = TINF
      RETURN
      END FUNCTION

      END MODULE
