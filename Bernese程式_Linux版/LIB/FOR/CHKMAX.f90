MODULE s_CHKMAX
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE chkmax(varnam,descr, imx,   nmx,   max,   maxDef,   mxc,   irc)

! -------------------------------------------------------------------------
! Purpose:    Checks and defines the array dimensions for GPSEST
!
! Author:     R. Dach
!
! Created:    09-Nov-2004
!
! Changes:    02-Jun-2009 HB: Write also actual dimension in message from
!                             SR DIMTST, use dble for soft limit
!             27-Nov-2012 RD/SL: Use specific messages instead of SR DIMTST
!             06-Jun-2013 RD: New parameter: where the parameter is defined
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern, ONLY: i4b, r8b, lfnerr, program_name

  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*) :: varnam ! Parameter name
  CHARACTER(LEN=*) :: descr  ! Parameter description
  INTEGER(i4b)     :: imx    ! User input (if > 0)
  INTEGER(i4b)     :: nmx    ! Adjusted value
  INTEGER(i4b)     :: max    ! Maximum dimension from P_GPSEST.f90
  CHARACTER(LEN=*) :: maxDef ! P_GPSEST.f90/M_MAXDIM.f90

! output:
  INTEGER(i4b)     :: mxc    ! Dimension to be used
  INTEGER(i4b)     :: irc    ! Return code

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  REAL(r8b),       PARAMETER :: ratio  = 2.0d0
  CHARACTER(LEN=6),PARAMETER :: srName = 'CHKMAX'

! Local Variables
! ---------------


! Take the user input
! -------------------
  IF (imx > 0) THEN

    ! Current value is bigger than the user input
    IF ( nmx > imx ) THEN
      WRITE(lfnerr,'(/,A,2(/,16X,A),/,16X,A,I8,A,/,16X,A,I8,/)')           &
      ' ### SR ' // TRIM(srName) // ': ' //                                &
      'User defined DIMENSION for PARAMETER "' // TRIM(varnam) // '"',     &
      'in program "' // TRIM(program_name) // '" (' // TRIM(descr) // ')', &
      'might be too small.',                                               &
      'User defined dimension: ',imx,'  (defined in input panel)',         &
      'Expected dimension:     ',nmx

    ! User input is smaller than the source-code accepted value
    ELSEIF ( ratio * DBLE(max) > DBLE(imx) ) THEN
      WRITE(lfnerr,'(/,A,2(/,16X,A),2(/,16X,A,I8,A),2(/,16X,A,I8),/)')     &
      ' ### SR ' // TRIM(srName) // ': ' //                                &
      'User defined dimension for parameter "' // TRIM(varnam) // '"',     &
      'in program "' // TRIM(program_name) // '" (' // TRIM(descr) // ')', &
      'is smaller than the hard limit.',                                   &
      'User defined dimension: ',imx,'  (defined in input panel)',         &
      'Soft limit for "' // TRIM(varnam) // '":',max,                      &
                                   '  (defined in ' // TRIM(maxDef) // ')',&
      'Hard limit for "' // TRIM(varnam) // '":',NINT(ratio * DBLE(max)),  &
      'Expected dimension:     ',nmx
    ENDIF
    mxc = imx
    irc = 0

! Check the adjusted dimension
! ----------------------------
  ELSE
    irc = 0

    ! Exceeding the hard limit
    IF ( DBLE(nmx) >= ratio * DBLE(max) ) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I8,A,2(/,16X,A,I8),/)')           &
      ' *** SR ' // TRIM(srName) // ': ' //                                &
      'Dimension for parameter "' // TRIM(varnam) // '" exceeded',         &
      'in program "' // TRIM(program_name) // '" (' // TRIM(descr) // ')', &
      'Value of  "' // TRIM(varnam) // '":',max,                           &
                                   '  (defined in ' // TRIM(maxDef) // ')',&
      'Limit for "' // TRIM(varnam) // '":',NINT(ratio * DBLE(max)),       &
      'Current value:     ',nmx
      irc = 1

    ! Exceeding the soft limit
    ELSEIF( nmx > max ) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I8,A,2(/,16X,A,I8),/)')           &
      ' ### SR ' // TRIM(srName) // ': ' //                                &
      'Unusual big number of parameter "' // TRIM(varnam) // '" detected', &
      'in program "' // TRIM(program_name) // '" (' // TRIM(descr) // ')', &
      'Soft limit for "' // TRIM(varnam) // '":',max,                      &
                                   '  (defined in ' // TRIM(maxDef) // ')',&
      'Hard limit for "' // TRIM(varnam) // '":',NINT(ratio * DBLE(max)),  &
      'Current value:          ',nmx

    ENDIF
    mxc = nmx
  ENDIF

  RETURN
END SUBROUTINE chkmax

END MODULE
