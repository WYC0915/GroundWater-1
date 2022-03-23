MODULE s_RDSCLK
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdsclk(title)

! -------------------------------------------------------------------------
! Purpose:    Reads input file for RXNCLK
!
! Author:     R. Dach
!
! Created:    20-Jul-2011
!
! Changes:    20-Jul-2011 RD: Extract this function from pgm CCRNXC
!             14-Nov-2011 SL: m_bern w/ ONLY, no PRITIT call (moved to program)
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,    ONLY: i4b, keyValueLength
  USE s_ckoptl
  USE s_prflna
  USE s_prfile
  USE s_exitrc
  USE s_readkeys
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:

! output:
  CHARACTER(LEN=60)      :: Title     ! Title line for satellite clock file

! Local Parameter
! ---------------

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER    :: srName = 'RDSCLK'

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength),  &
    DIMENSION(:), POINTER        :: keyValue

  INTEGER(i4b)                   :: irc
  INTEGER(i4b)                   :: irCode     ! Check the input options

! Initialization
! --------------
  irCode = 0

  NULLIFY(keyValue)

! Title line
! ----------
  CALL readKeys('TITLE',keyValue,irc)
  CALL ckoptl(0,'TITLE',keyValue,srName,                                    &
              'Printing options: title line',irc,irCode,                    &
              maxVal=1,empty=' ',result1=title)

! Stop if an error in the input options found
! -------------------------------------------
  IF (irCode /= 0) CALL exitrc(2)

! Start writing the protocol
! --------------------------
  CALL prflna(80)
  CALL prfile('RCLKINP', ' ', 2, 80)

  RETURN

  END SUBROUTINE

END MODULE
