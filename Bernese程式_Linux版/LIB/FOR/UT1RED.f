      MODULE s_UT1RED
      CONTAINS

C*
      SUBROUTINE UT1RED(TREQ,KORR)
CC
CC NAME       :  UT1RED
CC
CC PURPOSE    :  COMPUTATION OF THE CORRECTION OF UT1-UT1R
CC               (= SHORT PERIODIC TERMS UP TO 35 DAYS
CC
CC PARAMETERS :
CC         IN :  TREQ   : TIME OF REQUEST (MJD)               R*8
CC        OUT :  KORR   : UT1-UT1R (IN UNITS OF MSEC)         R*8
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  S.FANKHAUSER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  23-SEP-91
CC
CC CHANGES    :  10-JAN-94 : SF: IMPLICIT NONE REPLACED (SPECIAL UNIX
CC                                                       COMPILER)
CC               28-JAN-03 : RS: L,S,F,D,O,ARCMOD REAL*4 -> REAL*8
CC               23-JUN-05 : MM: IMPLICIT NONE
CC               28-FEB-07 : AG: USE PI FROM DEFCON
CC               06-MAY-11 : HB: IERS2010 CONVENTIONS UPDATE
CC               04-MAY-12 : RD: USE DMOD FROM MODULE, USE M_BERN WITH ONLY
CC               04-MAY-12 : RD: USE RHO INSTEAD OF PI FROM DEFCON
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      1992     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: r8b
      USE d_const,  ONLY: rho
      USE d_model,  ONLY: getModKey, mod_orb_nutMod, chrValLength
      USE l_basfun, ONLY: dmod
      IMPLICIT NONE
CCC       IMPLICIT REAL*8 (A-H,O-Z)
CCC       IMPLICIT INTEGER*4 (I-N)
C
C GLOBAL DECLARATION
C ------------------
      REAL*8  KORR,TREQ
C
C INTERNAL DECLARATION
C --------------------
      REAL*8  T,X,KOR1,KOR2,KOR3,KOR4
      REAL*8  L,S,F,D,O
      REAL*8  ARCMOD

      LOGICAL, SAVE    :: first = .TRUE.
      CHARACTER(LEN=chrValLength), SAVE :: nutMod
      CHARACTER(LEN=8) :: srNget
      REAL(r8b)        :: numVal

C
C DEFINITION OF AN INTERNAL FUNCTION
C ----------------------------------
      ARCMOD(X)=DMOD(X,360.D0)/RHO

      IF (first) THEN
        first=.FALSE.

! Get IERS Conventions string
! ---------------------------
        CALL getModKey(mod_orb_nutMod,nutMod,srNget,numVal)

      ENDIF
C
C      PI=3.141592653589793D0
      T=TREQ-51544.5D0
C
C CALCULATE THE ARGUMENTS
C -----------------------
      L=ARCMOD(134.96D0+13.064993*T)
      S=ARCMOD(357.53D0+ 0.985600*T)
      F=ARCMOD( 93.27D0+13.229350*T)
      D=ARCMOD(297.85D0+12.190749*T)
      O=ARCMOD(125.04D0- 0.052954*T)
C
C L= MEAN ANOMALY OF THE MOON
C S= MEAN ANOMALY OF THE SUN
C F= Z-O: Z: MEAN LONGITUDE OF THE MOON
C D= MEAN ELONGATION OF THE MOON FROM THE SUN
C O= MEAN LONGITUDE OF THE ASCENDING NODE
C
C CALCULATION OF THE SHORT PERIODIC TERMS
C ---------------------------------------
      KOR1=0.D0
      KOR2=0.D0
      KOR3=0.D0
      KOR4=0.D0
C
! OLD
      IF (nutMod(1:10) /= 'IAU2000R06' .AND.
     1    nutMod(1:7)  /= 'IAU2006') THEN
        KOR1=KOR1
     1        -8.26D0*SIN(   L                )
     2        -7.76D0*SIN(         2*F    +2*O)
     3        -3.21D0*SIN(         2*F    +  O)
     4        -1.82D0*SIN(-  L        +2*D    )
     5        -0.99D0*SIN(   L    +2*F    +2*O)
     6        -0.73D0*SIN(             2*D    )
     7        +0.54D0*SIN(   L            +  O)
     8        +0.53D0*SIN(   L            -  O)
     9        +0.44D0*SIN(-  L    +2*F    +2*O)
        KOR2=KOR2
     1        -0.41D0*SIN(   L    +2*F    +  O)
     2        -0.34D0*SIN( 2*L                )
     3        -0.30D0*SIN(         2*F        )
     4        -0.20D0*SIN(-  L    +2*F+2*D+2*O)
     5        +0.18D0*SIN(-  L    +2*F    +  O)
     6        +0.13D0*SIN(-  L        +2*D+  O)
     7        -0.12D0*SIN(         2*F+2*D+2*O)
     8        +0.12D0*SIN(-  L        +2*D-  O)
     9        -0.10D0*SIN( 2*L    +2*F    +2*O)
     1        +0.10D0*SIN(   L    +2*F-2*D+2*O)
        KOR3=KOR3
     1        -0.09D0*SIN(-  L-  S    +2*D    )
     2        -0.08D0*SIN(-  L    +2*F+2*D+  O)
     3        -0.08D0*SIN(   L        +2*D    )
     4        -0.06D0*SIN(   L-  S            )
     5        -0.05D0*SIN(        +2*F+2*D+  O)
     6        +0.05D0*SIN(             2*D-  O)
     7        -0.05D0*SIN(             2*D+  O)
     8        -0.05D0*SIN(    -  S    +2*D    )
     9        +0.05D0*SIN(   L    +2*F-2*D+  O)
     1        +0.05D0*SIN(-  L    +2*F        )
        KOR4=KOR4
     1        +0.05D0*SIN(               D    )
     2        -0.04D0*SIN( 2*L    +2*F    +  O)
     3        -0.04D0*SIN(   L    +2*F        )
     4        +0.04D0*SIN(   L+  S            )
     5        +0.03D0*SIN(       S+2*F    +2*O)
     6        -0.02D0*SIN(   L    +2*F+2*D+2*O)
     7        -0.02D0*SIN( 3*L                )
     8        +0.02D0*SIN( 2*L    +2*F-2*D+2*O)
     9        +0.02D0*SIN( 2*L            -  O)
     1        +0.02D0*SIN( 2*L            +  O)
     2        -0.02D0*SIN(    -  S+2*F    +2*O)
     3        +0.02D0*SIN(   L    -2*F+2*D-  O)
      ELSE

! IERS2010 Conventions, Ch.8.1, Table 8.1
! ---------------------------------------
        KOR1=KOR1
     1       -8.4046D0*SIN(   L                )
     1       +0.2500D0*COS(   L                )
     2       -7.8468D0*SIN(         2*F    +2*O)
     2       +0.5320D0*COS(         2*F    +2*O)
     3       -3.1873D0*SIN(         2*F    +  O)
     3       +0.2010D0*COS(         2*F    +  O)
     4       -1.8236D0*SIN(-  L        +2*D    )
     5       -0.9926D0*SIN(   L    +2*F    +2*O)
     6       -0.7341D0*SIN(             2*D    )
     7       +0.5443D0*SIN(   L            +  O)
     8       +0.5339D0*SIN(   L            -  O)
     9       +0.4352D0*SIN(-  L    +2*F    +2*O)
        KOR2=KOR2
     1       -0.4108D0*SIN(   L    +2*F    +  O)
     2       -0.3384D0*SIN( 2*L                )
     3       -0.2989D0*SIN(         2*F        )
     4       -0.1974D0*SIN(-  L    +2*F+2*D+2*O)
     5       +0.1767D0*SIN(-  L    +2*F    +  O)
     6       +0.1316D0*SIN(-  L        +2*D+  O)
     7       -0.1231D0*SIN(         2*F+2*D+2*O)
     8       +0.1175D0*SIN(-  L        +2*D-  O)
     9       -0.0987D0*SIN( 2*L    +2*F    +2*O)
     1       +0.1006D0*SIN(   L    +2*F-2*D+2*O)
        KOR3=KOR3
     1       -0.0855D0*SIN(-  L-  S    +2*D    )
     2       -0.0818D0*SIN(-  L    +2*F+2*D+  O)
     3       -0.0761D0*SIN(   L        +2*D    )
     4       -0.0555D0*SIN(   L-  S            )
     5       -0.0508D0*SIN(        +2*F+2*D+  O)
     6       +0.0470D0*SIN(             2*D-  O)
     7       -0.0526D0*SIN(             2*D+  O)
     8       -0.0508D0*SIN(    -  S    +2*D    )
     9       +0.0498D0*SIN(   L    +2*F-2*D+  O)
     1       +0.0470D0*SIN(-  L    +2*F        )
        KOR4=KOR4
     1       +0.0470D0*SIN(               D    )
     2       -0.0404D0*SIN( 2*L    +2*F    +  O)
     3       -0.0385D0*SIN(   L    +2*F        )
     4       +0.0395D0*SIN(   L+  S            )
     5       +0.0254D0*SIN(       S+2*F    +2*O)
     6       -0.0235D0*SIN(   L    +2*F+2*D+2*O)
     7       -0.0179D0*SIN( 3*L                )
     8       +0.0216D0*SIN( 2*L    +2*F-2*D+2*O)
     9       +0.0216D0*SIN( 2*L            -  O)
     1       +0.0179D0*SIN( 2*L            +  O)
     2       -0.0244D0*SIN(    -  S+2*F    +2*O)
     3       +0.0179D0*SIN(   L    -2*F+2*D-  O)
      ENDIF
C
      KORR=(KOR1+KOR2+KOR3+KOR4)/10.D0
C
      RETURN
      END SUBROUTINE

      END MODULE
