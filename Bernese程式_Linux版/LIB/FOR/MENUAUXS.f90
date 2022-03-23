MODULE s_MENUAUXS
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE menuauxs(inpFileName)

! -------------------------------------------------------------------------
! Purpose:    Auxiliary MENU program (subroutine)
!
! Author:     L.Mervart
!
! Created:    11-Jan-2000
!
! Changes:    08-Feb-2001 RD: Add menuaux for CRDMRG flag list
!             13-Feb-2001 RD: Add menuaux for CCRNXC station/satellite list
!             08-May-2001 RD: Store an empty crd-file
!             16-May-2001 RD: Init clkHead before reading the file
!             19-Jun-2001 RD: Get the list of rec/ant. records from obs-files
!             20-Jun-2001 RD: Automatic setup for antenna offset/PCV estimation
!             26-Jun-2001 RD: Use alcerr for allocation
!             26-Jun-2001 RD: Satellite list for DCB reference
!             28-Jun-2001 RD: Write also a pure satellite list
!             04-Jul-2001 RD: Put some stuff into SRs
!             15-Aug-2001 HB: Add menu_sta and menu_sim for GPSSIM
!             21-Aug-2001 RD: Create/edit/save station info file
!             22-Aug-2001 RD: Create/edit/save station selection file
!             22-Aug-2001 RD: Create/edit/save station sigma file
!             17-Sep-2001 RD: Make getcoo variables allocatable
!             25-Sep-2001 RD: HELMER_LST is not necessary anymore
!             17-Oct-2001 HU: New action POLXTR
!             07-Dec-2001 RD: Extent length of filList
!                             (may contain long entries; not only file names)
!             10-Dec-2001 RD: New call of sr MENU_PCF
!             28-Jan-2002 MM: Remove DCBSETUP (sat list for DCB reference)
!             07-Jun-2002 RD: Edit station obervation sigma file
!             08-Jul-2002 RD: Script selection list for new BPE
!             04-Oct-2002 RD: m_menu removed (menuauxInpfile has length 255 now)
!             13-Dec-2002 RD: New quotes-handling in SR writekey
!             08-Jan-2003 RD: Add new action "ADD_ONE"
!             06-Feb-2003 RD: Set MENUAUX_IRCODE
!             11-Feb-2003 RD: Browse observation file
!             23-Apr-2003 AJ: Nullify local pointers
!             15-Mai-2003 HU: Initialize structures
!             27-Oct-2003 RS: Receiver-independent antenna lists
!             19-Nov-2003 RD: Read INP-filename in READINPF
!             21-Jun-2005 MM: LFNUM.inc removed
!             28-Feb-2007 AG: Call DEFCON with parameter
!             10-Jun-2009 RD: Read receiver/satellite clock lists from NEQ
!             19-Aug-2009 LO: MENU_FST for FODITS added
!             27-Nov-2009 RD: A more ADDNEQ2 selection lists added
!             30-Nov-2010 MM: GNSS-specific parameter selection list
!             25-Aug-2011 UM: Action SIG_LIST6 introduced
!             23-Nov-2011 SL: use m_bern with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, keyNameLength, keyValueLength, lfnErr, &
                      t_key, init_key
  USE d_inpkey, ONLY: inpKey, init_inpkey
  USE p_menaux, ONLY: menuCount
  USE s_menu_sta
  USE s_alcerr
  USE s_menu_pld
  USE s_menu_sat
  USE s_menu_crd
  USE s_menu_sig
  USE s_menu_ant
  USE s_menu_clu
  USE s_menu_ecc
  USE s_menu_bsl
  USE s_menu_sim
  USE s_menu_fix
  USE s_menu_pcf
  USE s_readinpf
  USE s_menu_ohd
  USE f_tstkey
  USE s_menu_obs
  USE s_writekey
  USE s_menu_neq
  USE s_menu_crx
  USE s_menu_ref
  USE s_menu_rao
  USE s_menu_abb
  USE s_menu_rap
  USE s_readkeys
  USE s_menu_sos
  USE s_menu_bpe
  USE s_defcon
  USE s_exitrc
  USE s_menu_res
  USE s_opnsys
  USE s_menu_vel
  USE s_menu_pol
  USE s_menu_hlm
  USE s_menu_fst
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*), INTENT(IN) :: inpFileName

! output

! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER       :: srName = 'menuauxs'

! Local variables
! ---------------
  TYPE(t_key)                       :: output

  CHARACTER(LEN=keyNameLength)      :: action
  CHARACTER(LEN=keyValueLength),     &
           DIMENSION(:), POINTER    :: keyValue
  CHARACTER(LEN=keyValuelength)     :: menuauxInpFile

  INTEGER(i4b)                      :: ii
  INTEGER(i4b)                      :: irc
  INTEGER(i4b)                      :: istat

  NULLIFY(keyValue)
  CALL init_key(output)
  CALL init_inpkey(inpKey)

! Set the Name of the Input File
! ------------------------------
  CALL readinpf(inpFileName, inpKey)
  CALL readkeys('', keyValue, irc)

  CALL opnsys

  IF(tstKey('CONST')) THEN
    CALL defcon(1)
  ELSE
    CALL defcon(0)
  ENDIF

! Write the return code
! ---------------------
  IF ( tstKey('MENUAUX_IRCODE') ) THEN
    output%name = 'MENUAUX_IRCODE'
    ALLOCATE(output%value(1),stat=irc)
    CALL alcerr(irc,'output%value',(/1/),srName)
    output%value(1) = '5'
    CALL writeKey(inpFileName,(/output/),1,irc)
    DEALLOCATE(output%value,stat=irc)
  ENDIF

! Decide which operation is to be performed
! -----------------------------------------
  CALL readkeys('MENUAUX_ACTION', keyValue, irc)
  action = keyValue(1)
  IF (action /= 'OBS'          .AND. action /= 'NEQ'          .AND. &
      action /= 'NEQCLK'       .AND. action /= 'NEQTRP'       .AND. &
      action /= 'NEQDCBR'      .AND. action /= 'NEQGRID'      .AND. &
      action /= 'NEQRCLK'      .AND. action /= 'NEQSCLK'      .AND. &
      action /= 'NEQGSP'       .AND.                                &
      action /= 'ANTLST'       .AND. action /= 'ANTGRP'       .AND. &
      action /= 'ANTLSTRI'     .AND. action /= 'ANTGRPRI'     .AND. &
      action /= 'RAOSETUP'     .AND. action /= 'RAPSETUP'     .AND. &
      action /= 'SATLST'       .AND. action /= 'STALST'       .AND. &
      action /= 'HELMERT'      .AND. action /= 'CRDMRG'       .AND. &
      action /= 'SELREF_STA'   .AND. action /= 'SELREF_SAT'   .AND. &
      action /= 'CRD_EDIT'     .AND. action /= 'CRD_SAVE'     .AND. &
      action /= 'STAX_EDIT'    .AND. action /= 'STAX_SAVE'    .AND. &
      action /= 'STAX001'      .AND. action /= 'STAX002'      .AND. &
      action /= 'STAX005'                                     .AND. &
      action /= 'ABB_EDIT'     .AND. action /= 'ABB_SAVE'     .AND. &
      action /= 'ECC_EDIT'     .AND. action /= 'ECC_SAVE'     .AND. &
      action /= 'ECC_LIST'     .AND.                                &
      action /= 'VEL_EDIT'     .AND. action /= 'VEL_SAVE'     .AND. &
      action /= 'FIX_EDIT'     .AND. action /= 'FIX_SAVE'     .AND. &
      action /= 'SIG_EDIT'     .AND. action /= 'SIG_SAVE'     .AND. &
      action /= 'SIG_LIST1'    .AND. action /= 'SIG_LIST2'    .AND. &
      action /= 'SIG_LIST3'    .AND. action /= 'SIG_LIST4'    .AND. &
      action /= 'SIG_LIST6'    .AND.                                &
      action /= 'SOS_EDIT'     .AND. action /= 'SOS_SAVE'     .AND. &
      action /= 'SOS_LIST'                                    .AND. &
      action /= 'CLU_EDIT'     .AND. action /= 'CLU_SAVE'     .AND. &
      action /= 'BSL_EDIT'     .AND. action /= 'BSL_SAVE'     .AND. &
      action /= 'PLD_EDIT'     .AND. action /= 'PLD_SAVE'     .AND. &
      action /= 'ADD_ONE'      .AND.                                &
      action /= 'B2A_OBSHEAD'  .AND. action /= 'A2B_OBSHEAD'  .AND. &
      action /= 'B2A_OBSFILE'  .AND.                                &
      action /= 'RESID'                                       .AND. &
      action /= 'FST'                                         .AND. &
      action /= 'SIMLST'       .AND. action /= 'POLXTR'       .AND. &
      action /= 'BPE_SCRIPT'   .AND. action /= 'BPE_VAR'      .AND. &
      action /= 'PCF_EDIT'     .AND. action /= 'PCF_SAVE'     .AND. &
      action /= 'PCF_SPECIALS' .AND. action /= 'PCF_PARAMS'    ) THEN
    WRITE(lfnerr,*) ' *** menuaux: action not correct: ', action
    CALL exitrc(2)
  END IF

! Remember the Name of Input File
! -------------------------------
  CALL readkeys('', keyValue, irc)
  menuauxInpFile = TRIM( keyValue(1) )

! Init the output array
! ---------------------
  output%name='MENUAUX_RESULT'
  ALLOCATE(output%value(0),stat=iStat)
  CALL alcerr(iStat,'output%value',(/0/),'menuauxs')
  CALL writeKey(menuauxInpFile,(/output/),1,irc)

! Read/Edit, Create, and Store a coordinate file
! ----------------------------------------------
  IF (action == 'CRD_EDIT' .OR. action == 'CRD_SAVE') THEN
    CALL menu_crd(action)

! Read/Edit, Create, and Store a station info file
! ------------------------------------------------
  ELSEIF (action == 'STAX_EDIT' .OR. action == 'STAX_SAVE' .OR. &
          action(1:6) == 'STAX00') THEN
    CALL menu_crx(action, output)

! Read/Edit, Create, and Store a station info file
! ------------------------------------------------
  ELSEIF (action == 'ABB_EDIT' .OR. action == 'ABB_SAVE') THEN
    CALL menu_abb(action)

! Read/Edit, Create, and Store an eccentricity file
! -------------------------------------------------
  ELSEIF (action == 'ECC_EDIT'  .OR. action == 'ECC_SAVE'  .OR. &
          action == 'ECC_LIST') THEN
    CALL menu_ecc(action, menuauxinpFile, output)

! Read/Edit, Create, and Store a station velocity file
! ----------------------------------------------------
  ELSEIF (action == 'VEL_EDIT' .OR. action == 'VEL_SAVE') THEN
    CALL menu_vel(action)

! Read/Edit, Create, and Store a station selection file
! -----------------------------------------------------
  ELSEIF (action == 'FIX_EDIT' .OR. action == 'FIX_SAVE') THEN
    CALL menu_fix(action)

! Read/Edit, Create, and Store a station sigma file
! -------------------------------------------------
  ELSEIF (action == 'SIG_EDIT'  .OR. action == 'SIG_SAVE'  .OR. &
          action == 'SIG_LIST1' .OR. action == 'SIG_LIST2' .OR. &
          action == 'SIG_LIST3' .OR. action == 'SIG_LIST4' .OR. &
          action == 'SIG_LIST6') THEN
    CALL menu_sig(action, menuauxinpFile, output)

! Read/Edit, Create, and Store an eccentricity file
! -------------------------------------------------
  ELSEIF (action == 'SOS_EDIT'  .OR. action == 'SOS_SAVE'  .OR. &
          action == 'SOS_LIST') THEN
    CALL menu_sos(action, menuauxinpFile, output)

! Read/Edit, Create, and Store a station cluster file
! -----------------------------------------------------
  ELSEIF (action == 'CLU_EDIT' .OR. action == 'CLU_SAVE') THEN
    CALL menu_clu(action)

! Read/Edit, Create, and Store a station cluster file
! -----------------------------------------------------
  ELSEIF (action == 'BSL_EDIT' .OR. action == 'BSL_SAVE') THEN
    CALL menu_bsl(action)

! Read/Edit, Create, and Store a tectonic plate file
! --------------------------------------------------
  ELSEIF (action == 'PLD_EDIT' .OR. action == 'PLD_SAVE') THEN
    CALL menu_pld(action)

! Add one to the Menu Counter
! ---------------------------
  ELSEIF (action == 'ADD_ONE') THEN

    DEALLOCATE(output%value,stat=irc)

    ALLOCATE(output%value(1),stat=irc)
    CALL alcerr(irc,'output%value',(/1/),srName)

    output%value(1) = 'last'

    IF (TSTKEY('NEWFILE')) CALL readKeys('NEWFILE', output%value,irc)

    DO ii = 1,SIZE(menuCount)
      IF (ii == SIZE(menuCount)) THEN
        output%value(1) = 'last'
        EXIT
      ELSE IF (output%value(1) == menuCount(ii)) THEN
        output%value(1) = menuCount(ii+1)
        EXIT
      ENDIF
    ENDDO

! RUNBPE panel
! ------------
  ELSEIF (action == 'BPE_SCRIPT' .OR. action == 'BPE_VAR') THEN
    CALL menu_bpe(action,menuauxinpFile,output)

! Edit PCF File
! -------------
  ELSEIF (action(1:3) == 'PCF') THEN
    CALL menu_pcf(action, menuauxinpFile, output)

! Conversion Binary <--> ASCII of an Observation Header File
! ----------------------------------------------------------
  ELSEIF (action == 'B2A_OBSHEAD' .OR. action == 'A2B_OBSHEAD' .OR. &
          action == 'B2A_OBSFILE') THEN
    CALL menu_ohd(action)

! Prepare a List of Station Names from an obs File
! ------------------------------------------------
  ELSEIF (action == 'OBS') THEN
    CALL menu_obs(action, output)

! Read Information from the header files
! --------------------------------------
  ELSEIF (action == 'SATLST') THEN
    CALL menu_sat(action, output)

! Read Information from the header files
! --------------------------------------
  ELSEIF (action == 'ANTLST' .OR. action == 'ANTGRP' .OR. &
          action == 'ANTLSTRI' .OR. action == 'ANTGRPRI') THEN
    CALL menu_ant(action, output)

! Generate the default settings for antenna offset est.
! -----------------------------------------------------
  ELSEIF (action == 'RAOSETUP') THEN
    CALL menu_rao(action, menuauxInpFile, output)

! Generate the default settings for antenna PCV est.
! -----------------------------------------------------
  ELSEIF (action == 'RAPSETUP') THEN
    CALL menu_rap(action, menuauxInpFile, output)

! Prepare a List of Station Names from a neq File
! -----------------------------------------------
  ELSEIF (action(1:3) == 'NEQ') THEN
    CALL menu_neq(action, output)

! Helmert
! -------
  ELSEIF (action == 'HELMERT') THEN
    CALL menu_hlm(action, menuauxInpFile, output)
!
! CCRNXC: Manual selection of reference clocks
! --------------------------------------------
  ELSEIF (action(1:6) == 'SELREF') THEN
    CALL menu_ref(action, output)

! Select Baseline from one Residual File
! --------------------------------------
  ELSEIF (action == 'RESID') THEN
    CALL menu_res(action, output)

! Show station list out of CRD-file
! ---------------------------------
  ELSEIF (action == 'STALST') THEN
    CALL menu_sta(action, output)

! GPSSIM: Create list of station names, receiver names etc.
! ---------------------------------------------------------
  ELSEIF (action == 'SIMLST') THEN
    CALL menu_sim(action, menuauxInpFile, output)

! Prepare uniline for Polxtr
! --------------------------
  ELSEIF (action == 'POLXTR') THEN
    CALL menu_pol(action, menuauxInpFile, output)

! Prepare FODITS list of stations
! -------------------------------
  ELSEIF (action == 'FST') THEN
    CALL menu_fst(action, menuauxInpFile, output)

  ENDIF

! Write the resulting file
! ------------------------
  output%name = 'MENUAUX_RESULT'
  CALL writeKey(menuauxInpFile,(/output/),1,irc)

  DEALLOCATE(keyValue,stat=irc)

  RETURN

END SUBROUTINE menuauxs

END MODULE
