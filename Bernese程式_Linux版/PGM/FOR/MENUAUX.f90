
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

PROGRAM menuaux

! -------------------------------------------------------------------------
! Purpose:    Auxiliary MENU program (wrapper for MENUAUXS routine)
!
! Author:     L. Mervart
!
! Created:    11-Jan-2000
! Last mod.:  27-Oct-2010
!
! Changes:    16-Aug-2002 LM: Use mygetarg
!             06-Feb-2003 RD: Set global program variables
!             19-Feb-2003 RD: Remember INP-file name before EXITRC
!             23-Sep-2010 RD: Enable CPU counter
!             27-Oct-2010 SL: use m_bern with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: program_Name, program_Dscr
  USE m_cpu,    ONLY: cpu_start
  USE p_menaux, ONLY: inpFileName

  USE s_mygetarg
  USE s_menuauxs
  USE s_exitrc

  IMPLICIT NONE

! Local variables
! ---------------

! Start CPU Counter
! -----------------
  CALL cpu_start(.FALSE.)

! Set the program name
! --------------------
  program_Name = 'MENUAUX'
  program_Dscr = 'Interface program: Fortran <--> Menu'

  CALL mygetarg(inpFileName)

  CALL menuauxs(inpFileName)

  CALL exitrc(0)

END PROGRAM menuaux
