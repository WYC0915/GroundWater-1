MODULE s_VELDII
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE veldii(refTyp,velref,velnam,idif)

! -------------------------------------------------------------------------
! Purpose:    Reads the input options for VELDIF
!
! Author:     S. Schaer
!
! Created:    25-Mar-2008
!
! Changes:    28-Mar-2008 SS: Adapted to version 5.1
!             20-Dec-2011 DT: refTyp added to call of SR
!
! SR used:    readKeys, ckoptl, ckoptr, ckoptc, exitrc
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
!!  USE i_gpslib, ONLY: readKeys, ckoptl, ckoptr, ckoptc
  USE s_readkeys
  USE s_ckoptl
  USE s_ckoptr
  USE s_ckoptc
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
  INTEGER(i4b)                    :: refTyp       ! Type of reference velocity
                                                  ! 1: VEL file
                                                  ! 2: Station
                                                  ! 3: Velocity vector
  REAL(r8b), DIMENSION(3)         :: velRef       ! Reference velocity vector
  CHARACTER(LEN=staNameLength)    :: velNam       ! Reference station name
  INTEGER(i4b)                    :: iDif         ! Type of differencing
                                                  ! 0: 3-dimensional
                                                  ! 1: horizontal only

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER &
                                  :: keyValue
  INTEGER(i4b)                    :: irc
  INTEGER(i4b)                    :: irCode

! Inititialize variables
! ----------------------
  irCode    = 0
  iDif      = 0
  velRef(:) = 0d0
  velNam    = ''

  NULLIFY(keyValue)

! Type of reference velocity
! --------------------------
  CALL readKeys('VELREF', keyValue, irc)
  CALL ckoptc(1,'VELREF', keyValue,                                &
              (/'FILE    ','STATION ','VELOCITY'/),                &
              'Type of reference velocity', 'VELDII', irc, irCode, &
              maxVal=1, valList=(/1,2,3/), result1=refTyp)

! Reference station name
! ----------------------
  IF ( refTyp == 2 ) THEN
    CALL readKeys('REFNAM', keyValue, irc)
    CALL ckoptl(0, 'REFNAM', keyValue, 'VELDII',                   &
                'Reference station name', irc, irCode,             &
                empty=' ', maxVal=1, maxLength=staNameLength,      &
                result1=velNam)

! Reference velocity vector
! -------------------------
  ELSEIF ( refTyp == 3 ) THEN
    CALL readKeys('REFVX', keyValue, irc)
    CALL ckoptr(1, 'REFVX', keyValue, 'VELDII',                    &
                'VX value', irc, irCode,                           &
                empty=0d0, maxVal=1, result1=velRef(1))

    CALL readKeys('REFVY', keyValue, irc)
    CALL ckoptr(1, 'REFVY', keyValue, 'VELDII',                    &
                'VY value', irc, irCode,                           &
                empty=0d0, maxVal=1, result1=velRef(2))

    CALL readKeys('REFVZ', keyValue, irc)
    CALL ckoptr(1, 'REFVZ', keyValue, 'VELDII',                    &
                'VZ value', irc, irCode,                           &
                empty=0d0, maxVal=1, result1=velRef(3))

  ENDIF

! Type of differencing
!  is not supported at the moment !!!
! -----------------------------------
  IF ( refTyp > 3 ) THEN
    CALL readKeys('TYPDIF', keyValue, irc)
    CALL ckoptc(1,'TYPDIF', keyValue,                              &
                (/'3-DIMENSIONAL  ','HORIZONTAL_ONLY'/),           &
                'Type of differencing', 'VELDII', irc, irCode,     &
                maxVal=1, valList=(/0,1/), result1=iDif)
  ENDIF

! Exit in case of ckopt errors
! ----------------------------
  IF (irCode /= 0) CALL exitrc(2)

  RETURN
END SUBROUTINE veldii

END MODULE

