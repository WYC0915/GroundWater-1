MODULE s_QLGTSTA
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE qlgtsta (stainfo,abbinfo,stanam,window,sta4,sitnam,sitnum,found)

! -------------------------------------------------------------------------
! Purpose:    Rename station name according to station information file,
!             get 4-character abbreviation from abbreviation table
!
! Author:     C. Urschl
!
! Created:    05-Nov-2003
! Last mod.:  14-Nov-2005
!
! Changes:    14-Nov-2005 CU: Replace *** with ###
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_stacrx, ONLY: t_stacrux
  USE d_abbrev, ONLY: t_abbrev


  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  TYPE(t_stacrux)                 :: stainfo ! Station information
  TYPE(t_abbrev)                  :: abbinfo ! Abbreviation table
  CHARACTER(LEN=4)                :: stanam  ! Station name
  REAL(r8b), DIMENSION(2)         :: window  ! Time window

! output:
  CHARACTER(LEN=60)               :: sitnam  ! Renamed station name
  CHARACTER(LEN=20)               :: sitnum  ! DOMES number
  CHARACTER(LEN=4)                :: sta4    ! Abbreviation
  LOGICAL                         :: found


! Local Variables
! ---------------
  CHARACTER(LEN=staNameLength)    :: sitnamnum
  INTEGER(i4b)                    :: iinfo


! Rename station according to station info file
! ---------------------------------------------
  found = .FALSE.

  DO iinfo = 1, SIZE(stainfo%renamsta)

    IF (stainfo%renamsta(iinfo)%oldnam(1:4) == stanam    .AND. &
        stainfo%renamsta(iinfo)%timint%t(1) <= window(1) .AND. &
        stainfo%renamsta(iinfo)%timint%t(2) >= window(2)) THEN ! found
      sitnam = stainfo%renamsta(iinfo)%stanam(1:4)
      sitnum = stainfo%renamsta(iinfo)%stanam(6:)
      sitnamnum = stainfo%renamsta(iinfo)%stanam
      found = .TRUE.
      EXIT
    ELSEIF (iinfo == SIZE(stainfo%renamsta)) THEN           ! not found
      WRITE(lfnerr,'(2(2A,/),A,/)')                                      &
           ' ### SR QLGTSTA: Station name not found in station ',       &
                                             'information file.',       &
           '                 Station name: ', TRIM(stanam),             &
           '                 RINEX file cannot be written!'
    ENDIF

  ENDDO

  IF (.NOT. found) RETURN


! Find 4 character abbreviation for station name
! ----------------------------------------------
  found = .FALSE.

  DO iinfo = 1, SIZE(abbinfo%abb)

    IF (abbinfo%abb(iinfo)%stanam == sitnamnum) THEN        ! found
      sta4 = abbinfo%abb(iinfo)%staab4
      found = .TRUE.
      EXIT
    ELSEIF (iinfo == SIZE(abbinfo%abb)) THEN                ! not found
      WRITE(lfnerr,'(A,/,2A,/,A,/)')                                        &
           ' ### SR QLGTSTA: Station name not found in abbreviation table.',&
           '                 Station name: ', TRIM(sitnamnum),              &
           '                 RINEX file cannot be written!'
    ENDIF

  ENDDO

  RETURN

END SUBROUTINE qlgtsta

END MODULE
