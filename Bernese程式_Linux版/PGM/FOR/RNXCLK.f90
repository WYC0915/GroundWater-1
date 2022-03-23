
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

PROGRAM rnxclk

! -------------------------------------------------------------------------
! Purpose:    Read satellite clock information from clock RINEX files
!
! Author:     R. Dach
!
! Created:    20-Jul-2011
!
! Changes:    20-Jul-2011 RD: Extract this function from pgm CCRNXC
!             14-Nov-2011 SL: no m_bern, no init_inpkey, PRITIT call added
!
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_cpu,    ONLY: cpu_start
  USE d_inpkey, ONLY: inpKey
  USE d_const,  ONLY: date,time
  USE s_readinpf
  USE s_opnsys
  USE s_defcon
  USE s_pritit
  USE s_rdsclk
  USE s_rxcbv3
  USE s_exitrc

  IMPLICIT NONE
!
! Local variables
! ---------------
  CHARACTER(LEN=60)      :: Title    ! Title line for satellite clock file
  CHARACTER(LEN=20)      :: CrDate   ! Creation date

! Start CPU Counter
! -----------------
  CALL cpu_start(.TRUE.)

! Nullify pointers
! ----------------


! Get the name of the input file
! ------------------------------
  CALL readinpf(' ',inpKey)

! Open system files, define constants
! -----------------------------------
  CALL opnsys
  CALL defcon(0)
  CALL pritit('RNXCLK','Extract satellite clock from clock RINEX files',80)

! Get the new creation date
! -------------------------
  CrDate = date // ' ' // time
!
! Read input file
! ---------------
  CALL rdsclk(title)

! Extract satellite clocks
! -------------------------
  CALL rxcbv3(title,crDate)

  CALL exitrc(0)

END PROGRAM rnxclk
