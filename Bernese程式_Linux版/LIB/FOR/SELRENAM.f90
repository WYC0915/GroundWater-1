MODULE s_SELRENAM
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE selrenam(stacrux,flag)

! ----------------------------------------------------------------------------
! Purpose:    Adds renamsta part to current stacrux type
!
! Author:     D. Lenhart
!
! Created:    25-Apr-2003
!
! Changes:    02-May-2003 PS: Initialize remark with space, Added flag
!             14-May-2003 DL: Removed selection from selfile
!                             Changed pos to POINTER
!                             Added count of stations
!                             Set end time of last station info as timint%t(2)
!             30-Aug-2003 HU: Use descri and remark
!             04-Dec-2003 CU: Use 16 character string for stanam comparison
!             28-Jun-2005 MM: Unused variables removed
!             01-Oct-2010 SL: use m_bern with ONLY, flag (INTEGER->CHARACTER)
!             27-Apr-2012 RD: Nullify all pointers
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! ----------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b
  USE d_stacrx, ONLY: t_stacrux

  IMPLICIT NONE

! List of parameters
! ------------------
  TYPE(t_stacrux)    ::  stacrux
  CHARACTER(LEN=3)   ::  flag

! Local Variables
! ---------------
  INTEGER(i4b)                         ::  i, j, siz
  INTEGER(i4b)                         ::  k
  INTEGER(i4b), DIMENSION(:), POINTER  ::  pos
  CHARACTER(LEN=16)                    ::  sign


! Nullify pointers
! ----------------
  NULLIFY(pos)

! Count stations in stacrux info
! ------------------------------
  siz = 1
  i   = 1

  Count_sta: DO WHILE (i <= stacrux%ninfo)
    sign = stacrux%stainfo(i)%stanam(1:16)

    DO WHILE (sign == stacrux%stainfo(i)%stanam(1:16))
      i = i+1
      IF (i > stacrux%ninfo) EXIT
    END DO

    siz = siz+1

  END DO Count_sta

! Allocate memory for pos
! -----------------------
  ALLOCATE(pos(siz))

! Find position of first station info
! -----------------------------------
  pos(1) = 1
  siz    = 1
  i      = 1

  Loop_info: DO WHILE (i <= stacrux%ninfo)
    sign = stacrux%stainfo(i)%stanam(1:16)

    DO WHILE (sign == stacrux%stainfo(i)%stanam(1:16))
      i = i+1
      IF (i > stacrux%ninfo) EXIT
    END DO

    siz = siz+1
    pos(siz) = i

  END DO Loop_info

  siz = siz-1


! Allocate memory for stacrux%renamsta
! ------------------------------------
  ALLOCATE(stacrux%renamsta(siz))

! Copy info into stacrux%renamsta
! -------------------------------
  DO j = 1,siz
    i = pos(j)
    k = pos(j+1)-1
    stacrux%renamsta(j)%stanam       = stacrux%stainfo(i)%stanam
    stacrux%renamsta(j)%timint%t(1)  = stacrux%stainfo(i)%timint%t(1)
    stacrux%renamsta(j)%timint%t(2)  = stacrux%stainfo(k)%timint%t(2)
    stacrux%renamsta(j)%oldnam(1:4)  = stacrux%stainfo(i)%stanam(1:4)
    stacrux%renamsta(j)%oldnam(5:16) = '*           '
    stacrux%renamsta(j)%remark       = stacrux%stainfo(i)%remark
    stacrux%renamsta(j)%flg          = flag
  END DO

  stacrux%nrenam = siz

END SUBROUTINE selrenam

END MODULE
