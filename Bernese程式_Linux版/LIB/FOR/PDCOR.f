      MODULE s_PDCOR
      CONTAINS

C*
      SUBROUTINE PDCOR(Y,D,SZ,XP,YP,DER)
CC
CC NAME       :  PDCOR
CC
CC PURPOSE    :  THIS SUBROUTINE CALCULATES THE PARTIAL DERIVATIVE
CC               OF THE DIFFERENTIAL PHASE OBSERVABLE D1-D2 (IN M)
CC               WITH RESPECT TO THE EARTH-FIXED COORDINATES OF THE
CC               SECOND RECEIVER
CC
CC PARAMETERS :
CC         IN :  Y      : TOPOC. POSITION OF SATELLITE WITH   R*8(3)
CC                        RESPECT TO SECOND RECEIVER (TRUE
CC                        EQUATOR OF DATE)
CC               D      : TOPOC. DISTANCE OF SATELLITE        R*8
CC               SZ     : SIDEREAL TIME                       R*8
CC               XP,YP  : COORDINATES OF TRUE POLE (RADIANS)  R*8
CC        OUT :  DER    : ARRAY CONTAINING THE PARTIALS       R*8(3)
CC
CC REMARK     :  THIS SR MAY BE USED TO COMPUTE THE PARTIALS
CC               WITH RESPECT TO THE FIRST RECEIVER, IF FOR
CC               Y,D THE VALUES FOR THE FIRST RECEIVER ARE
CC               SPECIFIED. THE USER HAS TO CHANGE THE SIGN
CC               OF DER IN THIS CASE
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  3.4  (JAN 93)
CC
CC CREATED    :  87/11/02 16:55
CC
CC CHANGES    :  23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
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
      REAL*8    CS, D , SS, SZ, XP, YP
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 Y(3),DER(3)
      CS=DCOS(-SZ)
      SS=DSIN(-SZ)
      DER(1)=(Y(1)*CS-Y(2)*SS+Y(3)*XP)/D
      DER(2)=(Y(1)*SS+Y(2)*CS-Y(3)*YP)/D
      DER(3)=(Y(1)*(-CS*XP+SS*YP)+Y(2)*(SS*XP+CS*YP)
     1       +Y(3))/D
      RETURN
      END SUBROUTINE

      END MODULE
