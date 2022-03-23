! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  PROGRAM leoaux

!-------------------------------------------------------------------------
! Purpose:    Read LEO-Auxiliary File and store it in three files for
!             Attitude, Accelerations and Manoeuvres
!
! Author:     H.Bock
!
! Created:    19-Oct-2000
!
! Changes:    25-Sep-2002 HU: Remove i_astlib
!             19-Nov-2003 RD: Read INP-filename in READINPF
!             21-Jun-2005 MM: LFNUM.inc removed (LFNUMs in m_bern)
!             27-Feb-2007 AG: Call DEFCON with parameter
!             23-Sep-2010 RD: Enable CPU counter
!             06-Oct-2010 RD: Exitrc added at the end
!             29-Nov-2011 SL: new title string for pritit, m_bern with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
!-------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b
  USE m_cpu,    ONLY: cpu_start
  USE d_inpkey, ONLY: inpKey, init_inpkey
  USE p_leoaux, ONLY : t_leoaux_opt
  USE s_prflna
  USE s_pritit
  USE s_rdlaui
  USE s_readinpf
  USE s_svlaux
  USE s_defcon
  USE s_opnsys
  USE s_exitrc
  IMPLICIT NONE

! Local Types
! -----------
  TYPE(t_leoaux_opt) :: opt

! Local Variables
! ---------------
  INTEGER(i4b) :: iretrn

! Start CPU Counter
! -----------------
  CALL cpu_start(.TRUE.)

! Get the name of the input file
! ------------------------------
  CALL init_inpkey(inpKey)
  CALL readinpf(' ',inpKey)

! Define system files
! -------------------
  CALL opnsys
!
! Define constants
! ----------------
  CALL defcon(1)

! Write title and file list
! -------------------------
  CALL pritit('LEOAUX','Prepare LEO auxiliary information')
  CALL prflna

! Read INPUT file
! ---------------
  CALL rdlaui(opt)

! Read and store LEO-Auxiliary Information
! ----------------------------------------
  CALL svlaux(opt,iretrn)

  CALL exitrc(0)

  END PROGRAM leoaux
