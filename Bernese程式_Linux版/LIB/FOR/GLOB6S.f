      MODULE f_glob6s
      CONTAINS

      FUNCTION GLOB6S(P)
C      VERSION OF GLOBE FOR LOWER ATMOSPHERE 1/17/90
C
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*4 (I-N)
C
      REAL*8 LONG
      COMMON/LPOLY/PLG(9,4),CTLOC,STLOC,C2TLOC,S2TLOC,C3TLOC,S3TLOC,
     $ IYR,DAY,DF,DFA,APD,APDF,APT(4),LONG,CLONG,SLONG
      COMMON/CSW/SW(25),ISW,SWC(25)
      DIMENSION P(100),T(14)
      SAVE
      DATA DR/1.72142D-2/
cccccc          DGTR/1.74533D-2/
      DATA DAYL/-1.D0/,P32,P18,P14,P39/4*-1000.D0/
      DO 10 J=1,14
        T(J)=0.D0
   10 CONTINUE
      IF(DAY.NE.DAYL.OR.P32.NE.P(32)) CD32=DCOS(DR*(DAY-P(32)))
      IF(DAY.NE.DAYL.OR.P18.NE.P(18)) CD18=DCOS(2.*DR*(DAY-P(18)))
      IF(DAY.NE.DAYL.OR.P14.NE.P(14)) CD14=DCOS(DR*(DAY-P(14)))
      IF(DAY.NE.DAYL.OR.P39.NE.P(39)) CD39=DCOS(2.*DR*(DAY-P(39)))
      DAYL=DAY
      P32=P(32)
      P18=P(18)
      P14=P(14)
      P39=P(39)
C
C       F10.7
      T(1)=P(22)*DFA
C       TIME INDEPENDENT
      T(2)=P(2)*PLG(3,1)+P(3)*PLG(5,1)+P(23)*PLG(7,1)
     $     +P(27)*PLG(2,1)+P(28)*PLG(4,1)+P(29)*PLG(6,1)
C       SYMMETRICAL ANNUAL
      T(3)=(P(19)+P(48)*PLG(3,1)+P(30)*PLG(5,1))*CD32
C       SYMMETRICAL SEMIANNUAL
      T(4)=(P(16)+P(17)*PLG(3,1)+P(31)*PLG(5,1))*CD18
C       ASYMMETRICAL ANNUAL
      T(5)=(P(10)*PLG(2,1)+P(11)*PLG(4,1)+P(36)*PLG(6,1))*CD14
C       ASYMMETRICAL SEMIANNUAL
      T(6)=(P(38)*PLG(2,1))*CD39
C        DIURNAL
      IF(SW(7).EQ.0.D0) GOTO 200
      T71 = P(12)*PLG(3,2)*CD14*SWC(5)
      T72 = P(13)*PLG(3,2)*CD14*SWC(5)
      T(7) =
     1 ((P(4)*PLG(2,2) + P(5)*PLG(4,2)
     2 + T71)*CTLOC
     4 + (P(7)*PLG(2,2) + P(8)*PLG(4,2)
     5 + T72)*STLOC)
  200 CONTINUE
C        SEMIDIURNAL
      IF(SW(8).EQ.0.D0) GOTO 210
      T81 = (P(24)*PLG(4,3)+P(47)*PLG(6,3))*CD14*SWC(5)
      T82 = (P(34)*PLG(4,3)+P(49)*PLG(6,3))*CD14*SWC(5)
      T(8) =
     1 ((P(6)*PLG(3,3) + P(42)*PLG(5,3) + T81)*C2TLOC
     3 +(P(9)*PLG(3,3) + P(43)*PLG(5,3) + T82)*S2TLOC)
  210 CONTINUE
C        TERDIURNAL
      IF(SW(14).EQ.0.D0) GOTO 220
      T(14) = P(40)*PLG(4,4)*S3TLOC
     $ +P(41)*PLG(4,4)*C3TLOC
  220 CONTINUE
C       MAGNETIC ACTIVITY
      IF(SW(9).EQ.0.D0) GOTO 40
      IF(SW(9).EQ.1.D0)
     $ T(9)=APDF*(P(33)+P(46)*PLG(3,1)*SWC(2))
      IF(SW(9).EQ.-1.D0)
     $ T(9)=(P(51)*APT(3)+P(97)*PLG(3,1)*APT(3)*SWC(2))
   40 CONTINUE
      IF(SW(10).EQ.0.D0.OR.SW(11).EQ.0.D0.OR.LONG.LE.-1000.D0) GO TO 49
C        LONGITUDINAL
      T(11)= (1.+PLG(2,1)*(P(81)*SWC(5)*DCOS(DR*(DAY-P(82)))
     $           +P(86)*SWC(6)*DCOS(2.D0*DR*(DAY-P(87))))
     $        +P(84)*SWC(3)*DCOS(DR*(DAY-P(85)))
     $           +P(88)*SWC(4)*DCOS(2.D0*DR*(DAY-P(89))))
     $ *((P(65)*PLG(3,2)+P(66)*PLG(5,2)+P(67)*PLG(7,2)
     $   +P(75)*PLG(2,2)+P(76)*PLG(4,2)+P(77)*PLG(6,2)
     $    )*CLONG
     $  +(P(91)*PLG(3,2)+P(92)*PLG(5,2)+P(93)*PLG(7,2)
     $   +P(78)*PLG(2,2)+P(79)*PLG(4,2)+P(80)*PLG(6,2)
     $    )*SLONG)
   49 CONTINUE
      TT=0.
      DO 50 I=1,14
        TT=TT+ABS(SW(I))*T(I)
50    CONTINUE
      GLOB6S=TT
      RETURN
      END FUNCTION

      END MODULE
