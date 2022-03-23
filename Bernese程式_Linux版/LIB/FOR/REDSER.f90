MODULE s_REDSER
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE redser(crrec,crant,file)

! -------------------------------------------------------------------------
! Purpose:    Reduces station information file for antenna and receiver
!             serial numbers and deletes doubled entries
!
! Author:     A.Steinbach
!
! Created:    29-Nov-2007
! Last mod.:  30-Nov-2007
!
! Changes:    06-Jan-2011 SL: use m_bern with ONLY, use undef_i
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b
  USE d_stacrx, ONLY: t_stacrux,init_stacrux,undef_i

  IMPLICIT NONE

! List of parameters
! ------------------
! IN/OUT:
  INTEGER(i4b)                        :: crant
  INTEGER(i4b)                        :: crrec
  TYPE(t_stacrux)                     :: file

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------

! Local Variables
! ---------------
  INTEGER(i4b)                        :: i,j,k

! Replace entries different from '999999' and delete doubled entries
! ------------------------------------------------------------------
  DO i=1,file%ninfo
    IF(crant == 1) file%stainfo(i)%antnum = undef_i
    IF(crrec == 1) file%stainfo(i)%recnum = undef_i
  ENDDO

  DO i=1,file%ninfo-1
    k = 0
    DO j=i+1,file%ninfo-1
      IF(file%stainfo(i)%timint%t(2) /= 1d20                        .AND. &
         file%stainfo(i)%stanam      == file%stainfo(i+1)%stanam    .AND. &
         file%stainfo(i)%flg         == file%stainfo(i+1)%flg       .AND. &
         file%stainfo(i)%recnam      == file%stainfo(i+1)%recnam    .AND. &
         file%stainfo(i)%antnam      == file%stainfo(i+1)%antnam    .AND. &
!         file%stainfo(i)%recser      == file%stainfo(i+1)%recser    .AND. &
!         file%stainfo(i)%antser      == file%stainfo(i+1)%antser    .AND. &
         file%stainfo(i)%recnum      == file%stainfo(i+1)%recnum    .AND. &
         file%stainfo(i)%antnum      == file%stainfo(i+1)%antnum    .AND. &
         file%stainfo(i)%antecc(1)   == file%stainfo(i+1)%antecc(1) .AND. &
         file%stainfo(i)%antecc(2)   == file%stainfo(i+1)%antecc(2) .AND. &
         file%stainfo(i)%antecc(3)   == file%stainfo(i+1)%antecc(3))  THEN
        k = i+1
        file%stainfo(k)%timint%t(1) = file%stainfo(i)%timint%t(1)
        EXIT
      ENDIF
    ENDDO

    IF(k /= 0) THEN
      file%stainfo(i)%staNam = ' '
      CYCLE
    ENDIF

  ENDDO

  j = 0
  DO i=1,file%ninfo
    IF(file%stainfo(i)%staNam == ' ') CYCLE
    j = j+1
    IF(j /= i) file%stainfo(j) = file%stainfo(i)
  ENDDO
  file%ninfo = j

  RETURN

END SUBROUTINE redser

END MODULE
