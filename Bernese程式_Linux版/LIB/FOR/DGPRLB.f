      MODULE s_DGPRLB
      CONTAINS

C*
      SUBROUTINE DGPRLB(AEQ,GM,NTERM,PLM,CPOT,SPOT,
     1                  XGESAT,ITYP,DU1RLB,DU2RLB)
CC
CC NAME       :  DGPRLB
CC
CC PURPOSE    :  FIRST AND SECOND DERIVATIVES OF EARTHS POTENTIAL
CC               EARTH FIXED COORDINATES (RADIUS,LONG,LAT)
CC
CC PARAMETERS :
CC         IN :  AEQ    : EQUATORIAL RADIUS                        R*8
CC               GM     : GRAV.CONSTANT * EARTH'S MASS             R*8
CC               NTERM  : MAX. ORDER OF POTENTIAL COEFFICIENTS     I*4
CC               PLM(I) : LEGENDRE POLYNOMIALS UP TO ORDER 'NTERM'
CC                        I=1,..,(NTERM+3)*(NTERM+2)/2-9+NTERM
CC                        (S. SUBR. 'LEGPOL')                      R*8
CC               CPOT(I),SPOT(I): COEFFICIENTS OF POTENTIAL        R*8
CC                                (S. SUBR. 'POTINP')
CC               XGESAT(I): COORDINATES OF SATELLITE (EARTH FIXED)
CC                            I=1,2,3                              R*8
CC               ITYP   : ITYP=0: FIRST AND SECOND DERIVATIVES
CC                        ITYP=1: ONLY FIRST DERIVATIVES
CC                        ITYP=2: ONLY SECOND DERIVATIVES          I*4
CC        OUT :  DU1RLB(I):   DU/DR     DU/DL     DU/DB            R*8
CC               DU2RLB(I,K): D2U/DRDR  D2U/DRDL  D2U/DRDB         R*8
CC                            D2U/DRDL  D2U/DLDL  D2U/DLDB
CC                            D2U/DRDB  D2U/DLDB  D2U/DBDB
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/12/10 18:57
CC
CC CHANGES    :  18-FEB-93 : ??: DSIN(DFLOAT(M)*XL),DCOS(DFLOAT(M)*XL)
CC               13-MAY-98 : MR: REPLACE "DFLOAT" BY "DBLE"
CC               30-AUG-03 : HU: SHARED DO LABELS REMOVED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1987     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 I     , ICS   , IPL   , ITYP  , J     , K     , M     ,
     1          M1    , N     , N1    , NTERM
C
      REAL*8    ADR   , ADRN  , AEQ   , COSB  , COSMXL, CPOT  , DS1   ,
     1          DS2   , DU1RLB, DU2RLB, GM    , HILF1 , HILF2 , PLM   ,
     2          R     , R2    , R3    , RXY   , RXY2  , S     , SINMXL,
     3          SPOT  , SS    , TANB  , XGESAT, XL
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION PLM(*),CPOT(*),SPOT(*),XGESAT(3),DU1RLB(3),DU2RLB(3,3)
      DIMENSION SS(14),S(9)
C
      R2  =XGESAT(1)**2+XGESAT(2)**2+XGESAT(3)**2
      R   =DSQRT(R2)
      R3  =R2*R
      RXY2=XGESAT(1)**2+XGESAT(2)**2
      RXY =DSQRT(RXY2)
      XL  =DATAN2(XGESAT(2),XGESAT(1))
      TANB=XGESAT(3)/RXY
      COSB=RXY/R
      ADR =AEQ/R
C
C COMPUTATION OF DU/DR,DU/DL, DU/DB
C
      DO 10 I=1,14
        SS(I)=0.D0
10    CONTINUE
      IPL=0
      ICS=0
      DO 100 N=2,NTERM
        N1=N+1
        DO 20 I=1,9
          S(I)=0.D0
20      CONTINUE
        DO 30 M1=1,N1
          M=M1-1
          IPL=IPL+1
          ICS=ICS+1
          SINMXL=DSIN(DBLE(M)*XL)
          COSMXL=DCOS(DBLE(M)*XL)
          DS1=CPOT(ICS)*COSMXL+SPOT(ICS)*SINMXL
          DS2=SPOT(ICS)*COSMXL-CPOT(ICS)*SINMXL
          HILF1=PLM(IPL)*DS1
          HILF2=PLM(IPL)*DS2
          S(1)=S(1)+HILF1
          S(2)=S(2)+PLM(IPL+1)*DS1
          S(3)=S(3)+HILF1*M
          S(4)=S(4)+HILF2*M
          IF(ITYP.EQ.1) GOTO 30
          S(5)=S(5)+HILF1*M**2
          S(6)=S(6)+PLM(IPL+1)*DS1*M
          S(7)=S(7)+PLM(IPL+2)*DS1
          S(8)=S(8)+HILF2*M**2
          S(9)=S(9)+PLM(IPL+1)*DS2*M
30      CONTINUE
        IPL=IPL+2
        ADRN=ADR**N
        DO 40 J=1,9
          SS(J)=SS(J)+S(J)*ADRN
40      CONTINUE
        SS(10)=SS(10)+S(1)*ADRN*N
        IF(ITYP.EQ.1) GOTO 100
        SS(11)=SS(11)+S(1)*ADRN*N**2
        SS(12)=SS(12)+S(2)*ADRN*N
        SS(13)=SS(13)+S(3)*ADRN*N
        SS(14)=SS(14)+S(4)*ADRN*N
100   CONTINUE
C
C  FIRST DERIVATIVES
      IF(ITYP.EQ.2) GOTO 150
      DU1RLB(1)=-GM/R2*(SS(10)+SS(1))
      DU1RLB(2)= GM/R*SS(4)
      DU1RLB(3)= GM/R*(-TANB*SS(3)+SS(2))
C
C  SECOND DERIVATIVES
150   IF(ITYP.EQ.1) GOTO 200
      DU2RLB(1,1)= GM/R3*(SS(11)+3*SS(10)+2*SS(1))
      DU2RLB(1,2)=-GM/R2*(SS(14)+SS(4))
      DU2RLB(1,3)=-GM/R2*(SS(2)+SS(12)-TANB*(SS(3)+SS(13)))
      DU2RLB(2,2)=-GM/R *SS(5)
      DU2RLB(2,3)= GM/R *(-TANB*SS(8)+SS(9))
      DU2RLB(3,3)= GM/R *(SS(7)-TANB*(2*SS(6)+SS(2))+TANB**2* SS(5)-
     1                   1.D0/COSB**2*SS(3))
      DO 110 I=1,3
        DO K=1,3
          DU2RLB(K,I)=DU2RLB(I,K)
        ENDDO
110   CONTINUE
C
200   RETURN
      END SUBROUTINE

      END MODULE
