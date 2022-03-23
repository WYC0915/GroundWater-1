MODULE s_PRIGRD
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE prigrd(opLoad,titles)

! ------------------------------------------------------------------------
! Purpose:    Print the information on the Vienna grid files and the
!             corresponding scaling factor to be estimated
!
! Author:     R. Dach
!
! Created:    04-May-2009
! Last mod.:  04-May-2009
!
! Changes:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_grid,   ONLY: prtGridInfo, getGridInp
  USE p_gpsest, ONLY: t_optLoad

  USE s_gtfile2

  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  TYPE(t_optLoad), DIMENSION(:):: opLoad ! Scaling factors for vienna grid files
                                         ! 1: Atmospheric non-tidal loading
                                         ! 2: Ocean non-tidal loading
                                         ! 3: Hydrostatic pressure loading
  CHARACTER(LEN=*),DIMENSION(:):: titles ! Program output title lines

! output:


! Local parameters
! ----------------
  CHARACTER(LEN=18), DIMENSION(3) :: parStr = &
  (/ 'for all components', 'for vert and horiz', 'for Up,North,East ' /)

! Local variables
! ---------------
  CHARACTER(LEN=132)              :: line
  CHARACTER(LEN=fileNameLength),   &
          DIMENSION(:,:), POINTER :: filnam

  INTEGER(i4b)                    :: nFil
  INTEGER(i4b)                    :: ii


! Init variables
! --------------
  NULLIFY(filnam)

! Print the title lines to open a new section
! -------------------------------------------
  WRITE(lfnprt,'(//,A,/,A,/,1X,131("-"),/)') TRIM(titles(1)),TRIM(titles(2))

! Print the information on the Vienna grid files
! ----------------------------------------------
  CALL prtGridInfo

! Print information on the scaling parameters
! -------------------------------------------
  WRITE(lfnprt,'(//,A,/,1X,131("-"),/)')            &
        ' Grid type     #fil  Scaling factors for ' // &
        'stations          Components and sigmas for scaling factors'

  DO ii = 1,SIZE(opLoad)

    ! Check whether there are some grid files
    CALL gtfile2(getGridInp(opLoad(ii)%Keyw),1,nFil,filnam)

    IF ( opLoad(ii)%nSta == 0 ) THEN
      WRITE(lfnprt,'(1X,A12,I5,3X,A)') &
        opLoad(ii)%keyw,nFil,'no scaling factors are estimated'

    ELSE IF ( opLoad(ii)%staClu(1) == 0 ) THEN
      WRITE(lfnprt,'(1X,A12,I5,3X,A,3X,A,3X,3F9.3))') &
        opLoad(ii)%keyw, nFil,'one scaling factor for each station', &
        parStr(opLoad(ii)%nPar),opLoad(ii)%sigma(1:opLoad(ii)%nPar)

    ELSE IF ( opLoad(ii)%staClu(1) > 0 ) THEN
      WRITE(lfnprt,'(1X,A12,I5,3X,A,3X,A,3X,3F9.3))') &
        opLoad(ii)%keyw, nFil,'scaling factors within groups      ', &
        parStr(opLoad(ii)%nPar),opLoad(ii)%sigma(1:opLoad(ii)%nPar)

    ELSE IF ( opLoad(ii)%staClu(1) == -1 ) THEN
      WRITE(lfnprt,'(1X,A12,I5,3X,A,3X,A,3X,3F9.3))') &
        opLoad(ii)%keyw, nFil,'one scaling factor for all stations', &
        parStr(opLoad(ii)%nPar),opLoad(ii)%sigma(1:opLoad(ii)%nPar)

    ENDIF

  ENDDO

  IF (ASSOCIATED(filnam)) DEALLOCATE(filnam)

  RETURN

  END SUBROUTINE prigrd

END MODULE
