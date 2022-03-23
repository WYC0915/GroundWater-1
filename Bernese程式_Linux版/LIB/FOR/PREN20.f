      MODULE s_PREN20
      CONTAINS

C*
      SUBROUTINE PREN20(XMJD,PRAEZ)
CC
CC NAME       :  PREN20
CC
CC PURPOSE    :  COMPUTATION OF PRECESSION-MATRIX FROM 2000.0
CC               TO EPOCH XMJD, XMJD IN TDB (BARYCENTRIC DYNAMICAL
CC               TIME)
CC               SEE "USNO CIRCULAR NO 163",1981
CC               (IAU RESOLUTIONS)
CC
CC PARAMETERS :
CC         IN :  XMJD   : EPOCH IN MODIFIED JULIAN DATE IN   R*8
CC                        BARYCENTRIC DYNAMICAL TIME
CC        OUT :  PRAEZ  : PRECESSION - MATRIX                R*8(3,3)
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  T.SCHILDKNECHT
CC
CC VERSION    :  3.3
CC
CC CREATED    :  31-MAY-92
CC
CC CHANGES    :  23-JUN-2005 MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               04-Jul-2005 HU: Switch from three-rotation to four-rotation
CC                               approach for precession (IERS Conv 2003)
CC               28-FEB-2007 AG: USE 206264... FROM DEFCON
CC               23-MAR-2007 HB/AG: CORRECT USAGE OF ARS
CC               05-MAY-2011 HB: IAU2006 PRECESSION ADDED,
CC                               3-ROTATION APPROACH REACTIVATED FOR V5.0
CC               19-MAY-2011 HB: REDUCED ONLY-LIST FROM D_MODEL,
CC                               VARIABLE SRNAME REMOVED
CC               07-JUN-2011 HB: UPDATE OF DESCRIPTIONS
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC               UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE d_const, ONLY: ars, pi
      USE d_model, ONLY: getModKey, chrValLength,
     1                   mod_orb_nutMod, mod_orb_prcMod
      USE s_ddreh
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 irChk

      REAL*8  ARC2RAD, CHIA , EPS0 , OMGA , PSIA , TL   , XMJD
      REAL*8  XA,ZA,TA,COSXA,SINXA,COSZA,SINZA,COSTA,SINTA, numVal
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 PRAEZ(3,3),ROT(3,3)

      CHARACTER(LEN=chrValLength) :: chrVal
      CHARACTER(LEN=16), SAVE :: nutNam, prcNam
      CHARACTER(LEN=8)  :: srNget

      LOGICAL, SAVE :: first=.TRUE.
C
! Get model keys
! --------------
      IF (first) THEN
        first=.FALSE.
        chrVal = ' '
        CALL getModKey(mod_orb_nutMod,chrVal,srNget,numVal)
        nutNam = chrVal(1:16)
        chrVal = ' '
        prcNam = ' '
        CALL getModKey(mod_orb_prcMod,chrVal,srNget,numVal)
        prcNam = chrVal(1:16)
      ENDIF

C  TIME INTERVAL (IN JUL. CENTURIES) BETWEEN EPOCH AND J2000.0
      TL=(XMJD-51544.5D0)/36525.D0

      IF (prcNam(1:4) == 'V50 ') THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C  THREE ROTATION APPROACH (Version 5.0)
! IERS TN No.32, CH.5.5.2, Eq.(31)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C  ROTATION ANGLES XI,Z AND THETA
        XA=2306.2181  D0*TL
     1       +0.30188 D0*TL**2
     2       +0.017998D0*TL**3
        ZA=2306.2181  D0*TL
     1       +1.09468 D0*TL**2
     2       +0.018203D0*TL**3
        TA=2004.3109  D0*TL
     1       -0.42665 D0*TL**2
     2       -0.041833D0*TL**3
!      XA=XA*pi/648000D0
!      ZA=ZA*pi/648000D0
!      TA=TA*pi/648000D0
        XA=XA/206264.8D0
        ZA=ZA/206264.8D0
        TA=TA/206264.8D0
        COSXA=DCOS(XA)
        SINXA=DSIN(XA)
        COSZA=DCOS(ZA)
        SINZA=DSIN(ZA)
        COSTA=DCOS(TA)
        SINTA=DSIN(TA)
C
C  ROTATION MATRIX
        PRAEZ(1,1)= COSXA*COSZA*COSTA-SINXA*SINZA
        PRAEZ(2,1)= COSXA*SINZA*COSTA+SINXA*COSZA
        PRAEZ(3,1)= COSXA*SINTA
        PRAEZ(1,2)=-SINXA*COSZA*COSTA-COSXA*SINZA
        PRAEZ(2,2)=-SINXA*SINZA*COSTA+COSXA*COSZA
        PRAEZ(3,2)=-SINXA*SINTA
        PRAEZ(1,3)=-COSZA*SINTA
        PRAEZ(2,3)=-SINZA*SINTA
        PRAEZ(3,3)= COSTA
C
      ELSEIF (prcNam(1:4) == 'BIAS') THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!C  FOUR ROTATION APPROACH (> Version 5.0)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! IAU2000: IERS TN No.32, Ch 5.5.2
! ================================
      IF (nutNam(1:10) /= 'IAU2000R06'.AND.
     1    nutNam(1:8)  /= 'IAU2006') THEN
        EPS0 = 84381.448D0

! Eq.(32)
        PSIA = (5038.47875D0
     1       + (  -1.07259D0
     2       + (  -0.001147D0) *TL) *TL) *TL
C
        OMGA = 84381.448D0
     1       + (  -0.02524D0
     2       + (   0.05127D0
     3       + (  -0.007726D0) *TL) *TL) *TL
C
        CHIA = (  10.55260D0
     1       + (  -2.38064D0
     2       + (  -0.001125D0) *TL) *TL) *TL

! IAU2006: IERS TN No.36, Ch 5.6.4
! ================================
        ELSEIF (nutNam(1:10) == 'IAU2000R06' .OR.
     1          nutNam(1:8)  == 'IAU2006') THEN
          EPS0 = 84381.406D0
! Eq. (5.39)
          PSIA = (5038.481507D0
     1         + (  -1.0790069D0
     2         + (  -0.00114045D0
     3         + (   0.000132851D0
     4         + (  -0.0000000951D0) *TL) *TL) *TL) *TL) *TL
C
          OMGA = EPS0
     1         + (  -0.025754D0
     2         + (   0.0512623D0
     3         + (  -0.00772503D0
     4         + (  -0.000000467D0
     5         + (   0.0000003337D0) *TL) *TL) *TL) *TL) *TL
C
! Eq. (5.40)
          CHIA = (  10.556403D0
     1         + (  -2.3814292D0
     2         + (  -0.00121197D0
     3         + (   0.000170663D0
     4         + (  -0.0000000560D0) *TL) *TL) *TL) *TL) *TL
        ENDIF
C
! IERS TN No.36, Ch.5.4.5, first way
        CALL DDREH(1, EPS0/ars, PRAEZ)
        CALL DDREH(3,-PSIA/ars, ROT)
        PRAEZ = MATMUL (ROT,PRAEZ)
        CALL DDREH(1,-OMGA/ars, ROT)
        PRAEZ = MATMUL (ROT,PRAEZ)
        CALL DDREH(3, CHIA/ars, ROT)
        PRAEZ = MATMUL (ROT,PRAEZ)
      ENDIF
C
      RETURN
      END SUBROUTINE

      END MODULE
