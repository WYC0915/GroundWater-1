MODULE s_SELOPT
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.0
! -------------------------------------------------------------------------

  SUBROUTINE selopt(file,staList,timint)

! -------------------------------------------------------------------------
! Purpose:    Searches in station information file for stations via station
!             selection list and time window
!
! Author:     A.Steinbach
!
! Created:    13-Nov-2007
! Last mod.:  06-Jan-2011
!
! Changes:    06-Jan-2011 SL: use m_bern with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b
  USE d_stalst, ONLY: t_staList
  USE d_stacrx, ONLY: t_stacrux

  IMPLICIT NONE

! List of parameters
! ------------------
! IN/OUT:
  TYPE(t_stacrux)                 :: file    ! sta file structure

! IN
  TYPE(t_staList)                 :: staList ! sta file structure
  REAL(r8b), DIMENSION(2)         :: timint  ! Resulting time window (MJD)

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------

! Local Variables
! ---------------
  INTEGER(i4b)                    :: i,j,k

! Get selection options and filter sta file
! -----------------------------------------
! TYPE 001
! --------
  DO i=1,file%nrenam
    k = 1
    IF (staList%nSta /= 0) THEN
      k = 0
      DO j=1,staList%nSta
        IF (file%renamsta(i)%staNam == staList%staNam(j) ) THEN
          k = j
          EXIT
        ENDIF
      ENDDO
    ENDIF
!    IF (timint(1) /= 0d0 .AND. timint(2) /= 1d20) THEN
      IF (k == 0 .OR. &
          file%renamsta(i)%timint%t(1) >= timint(2) .OR. &
          file%renamsta(i)%timint%t(2) <= timint(1)) THEN
        file%renamsta(i)%staNam = ' '
        CYCLE
      ENDIF
!    ENDIF
  ENDDO

  j = 0
  DO i=1,file%nrenam
    IF (file%renamsta(i)%staNam == ' ') CYCLE
    j = j + 1
    IF (j /= i) file%renamsta(j) = file%renamsta(i)
  ENDDO
  file%nrenam = j

! TYPE 002
! --------
  DO i=1,file%ninfo
    k = 1
    IF (staList%nSta /= 0) THEN
      k = 0
      DO j=1,staList%nSta
        IF (file%stainfo(i)%staNam == staList%staNam(j) ) THEN
          k = j
          EXIT
        ENDIF
      ENDDO
    ENDIF
!    IF (timint(1) /= 0d0 .AND. timint(2) /= 1d20) THEN
      IF (k == 0 .OR. &
          file%stainfo(i)%timint%t(1) >= timint(2) .OR. &
          file%stainfo(i)%timint%t(2) <= timint(1)) THEN
        file%stainfo(i)%staNam = ' '
        CYCLE
      ENDIF
      IF (k /= 0 .AND. file%stainfo(i)%timint%t(1) < timint(1)) file%stainfo(i)%timint%t(1) = timint(1)
      IF (k /= 0 .AND. file%stainfo(i)%timint%t(2) > timint(2)) file%stainfo(i)%timint%t(2) = timint(2)
!    ENDIF
  ENDDO

  j = 0
  DO i=1,file%ninfo
    IF (file%stainfo(i)%staNam == ' ') CYCLE
    j = j + 1
    IF (j /= i) file%stainfo(j) = file%stainfo(i)
  ENDDO
  file%ninfo = j

! TYPE 003
! --------
  DO i=1,file%nprob
    k = 1
    IF (staList%nSta /= 0) THEN
      k = 0
      DO j=1,staList%nSta
        IF (file%staprob(i)%staNam == staList%staNam(j) ) THEN
          k = j
          EXIT
        ENDIF
      ENDDO
    ENDIF
!    IF (timint(1) /= 0d0 .AND. timint(2) /= 1d20) THEN
      IF (k == 0 .OR. &
          file%staprob(i)%timint%t(1) >= timint(2) .OR. &
          file%staprob(i)%timint%t(2) <= timint(1)) THEN
        file%staprob(i)%staNam = ' '
        CYCLE
      ENDIF
!    ENDIF
  ENDDO

  j = 0
  DO i=1,file%nprob
    IF (file%staprob(i)%staNam == ' ') CYCLE
    j = j + 1
    IF (j /= i) file%staprob(j) = file%staprob(i)
  ENDDO
  file%nprob = j

! TYPE 004
! --------
  DO i=1,file%ncoovel
    k = 3
    IF (staList%nSta /= 0) THEN
      k = 0
      DO j=1,staList%nSta
        IF (file%coovel(i)%staNam(1) == staList%staNam(j) .AND. &
            (k == 0 .OR. k == 2)) k = k+1
        IF (file%coovel(i)%staNam(2) == staList%staNam(j) .AND. &
            (k == 0 .OR. k == 1)) k = k+2
      ENDDO
    ENDIF
    IF ( k /= 3 ) file%coovel(i)%staNam(1) = ' '
  ENDDO

  j = 0
  DO i=1,file%ncoovel
    IF (file%coovel(i)%staNam(1) == ' ') CYCLE
    j = j + 1
    IF (j /= i) file%coovel(j) = file%coovel(i)
  ENDDO
  file%ncoovel = j

! TYPE 005
! --------
  DO i=1,file%nstatype
    k = 1
    IF (staList%nSta /= 0) THEN
      k = 0
      DO j=1,staList%nSta
        IF (file%statype(i)%staNam == staList%staNam(j) ) THEN
          k = j
          EXIT
        ENDIF
      ENDDO
    ENDIF
!    IF (timint(1) /= 0d0 .AND. timint(2) /= 1d20) THEN
      IF (k == 0 .OR. &
          file%statype(i)%timint%t(1) >= timint(2) .OR. &
          file%statype(i)%timint%t(2) <= timint(1)) THEN
        file%statype(i)%staNam = ' '
        CYCLE
      ENDIF
!    ENDIF
  ENDDO

  j = 0
  DO i=1,file%nstatype
    IF (file%statype(i)%staNam == ' ') CYCLE
    j = j + 1
    IF (j /= i) file%statype(j) = file%statype(i)
  ENDDO
  file%nstatype = j

  RETURN

END SUBROUTINE selopt

END MODULE
