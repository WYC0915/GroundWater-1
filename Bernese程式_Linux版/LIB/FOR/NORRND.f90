MODULE s_NORRND
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE norrnd(s,m,ix,v1,v2)

! -------------------------------------------------------------------------
!
! NAME       :  NORRND
!
!    CALL NORRND(S,M,IX,V1,V2)
!
! PURPOSE    :  GENERATE TWO NORMALLY DISTRIBUTED RANDOM NUMBERS V1,V2
!               WITH MEAN M AND VARIANCE S**2
!
! PARAMETERS :
!         IN :  S      : ROOT OF VARIANCE                    R*8
!               M      : EXPECTATION VALUE OF RANDOM NUMBER  R*8
!     IN/OUT :  IX     : INTEGER RANDOM VARIABLE             I*4
!        OUT :  V1,V2  : RESULT                              R*8
!
! SR CALLED  :  RANDU
!
! REMARKS    :  SET IX # 0 ON FIRST CALL  (INFINITE LOOP FOR IX=0)
!
! AUTHOR     :  U. HUGENTOBLER
!
! VERSION    :
!
! CREATED    :   31-JAN-90            LAST MODIFIED : 18-NOV-00
!
! MODIFIED   :   18-NOV-00 HU: FORTRAN 90
!
!
! COPYRIGHT  :  ASTRONOMICAL INSTITUTE
!      1988      UNIVERSITY OF BERN
!                    SWITZERLAND
!
! -------------------------------------------------------------------------

  USE m_bern

  USE s_randu
  IMPLICIT NONE

! PARAMETER LIST
! --------------

! DUMMY LIST
! ----------
  INTEGER(i4b)    :: ix
  REAL(r8b)       :: s,m,v1,v2

! LOCAL PARAMETERS
! ----------------
  INTEGER(i4b)    :: iy
  REAL(r8b)       :: x1,x2,xx1,xx2,vv1,vv2

  CALL randu(ix,iy,x1)
  ix=iy
  CALL randu(ix,iy,x2)
  ix=iy

  xx2=x2*6.28318530718d0
  xx1=x1
  xx1=SQRT(-2.e0*LOG(xx1))
  vv1=xx1*SIN(xx2)
  vv2=xx1*COS(xx2)
  v1 =vv1*s+m
  v2 =vv2*s+m

  RETURN
END SUBROUTINE norrnd

END MODULE
