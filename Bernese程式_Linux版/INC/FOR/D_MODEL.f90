
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.2
! -------------------------------------------------------------------------

MODULE d_model

! -------------------------------------------------------------------------
! Purpose:    This module defines model keys and the SRs to set, get
!             and check them
!
! Author:     H. Bock
!
! Created:    14-Jan-2011
!
! Changes:    19-May-2011 HB: Layout corrections
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE s_exitrc

! Declare access rights
  PRIVATE
  PUBLIC :: mod_orb_ethPot, mod_orb_ocTide, mod_orb_seTide, mod_orb_atTide,&
            mod_orb_jplEph, mod_orb_nutMod, mod_orb_subMod, mod_orb_prcMod,&
            mod_orb_sePTid, mod_orb_ocPTid, mod_orb_meaPol, mod_orb_relEff,&
            mod_orb_albedo, mod_orb_3bodAt, mod_orb_GM    , mod_orb_solRad,&
            mod_orb_ethShi, mod_orb_thmFor, mod_orb_dynPol, mod_orb_shadow,&
            mod_orb_timSys, mod_orb_subFmt, &
            mod_orb_eqmPol, mod_orb_eqmInt, mod_orb_veqPol, mod_orb_veqInt,&
            mod_orb_empiri,&
            setModKey, getModKey, chkModKey,&
            chrValLength

! =========================================================================
! Parameter definitions
! =========================================================================
  INTEGER(i4b), PARAMETER    :: chrValLength = 20

! Identifiers: Models for orbit
  INTEGER(i4b), PARAMETER    :: mod_orb        =   0       ! General orbit model identifier
  INTEGER(i4b), PARAMETER    :: mod_orb_ethPot =   1       ! Earth potential
  INTEGER(i4b), PARAMETER    :: mod_orb_ocTide =   2       ! Ocean tides
  INTEGER(i4b), PARAMETER    :: mod_orb_seTide =   3       ! Solid Earth tides
  INTEGER(i4b), PARAMETER    :: mod_orb_atTide =   4       ! Atmospheric tides
  INTEGER(i4b), PARAMETER    :: mod_orb_jplEph =   5       ! JPL ephemerides
  INTEGER(i4b), PARAMETER    :: mod_orb_nutMod =   6       ! Nutation model
  INTEGER(i4b), PARAMETER    :: mod_orb_subMod =   7       ! Subdaily ERP model
  INTEGER(i4b), PARAMETER    :: mod_orb_prcMod =   8       ! Precession model
  INTEGER(i4b), PARAMETER    :: mod_orb_sePTid =   9       ! Solid Earth pole tide
  INTEGER(i4b), PARAMETER    :: mod_orb_ocPTid =  10       ! Oceanic pole tide
  INTEGER(i4b), PARAMETER    :: mod_orb_meaPol =  11       ! Mean pole
  INTEGER(i4b), PARAMETER    :: mod_orb_relEff =  12       ! Relativistic effects
  INTEGER(i4b), PARAMETER    :: mod_orb_albedo =  13       ! Albedo
  INTEGER(i4b), PARAMETER    :: mod_orb_3bodAt =  14       ! Third body attractions
  INTEGER(i4b), PARAMETER    :: mod_orb_GM     =  15       ! GM
  INTEGER(i4b), PARAMETER    :: mod_orb_solRad =  16       ! direct solar radiation
  INTEGER(i4b), PARAMETER    :: mod_orb_ethShi =  17       ! earthshine
  INTEGER(i4b), PARAMETER    :: mod_orb_thmFor =  18       ! thermal forces
  INTEGER(i4b), PARAMETER    :: mod_orb_dynPol =  19       ! dynamic polar motion
  INTEGER(i4b), PARAMETER    :: mod_orb_shadow =  20       ! shadow
  INTEGER(i4b), PARAMETER    :: mod_orb_timSys =  21       ! time system
  INTEGER(i4b), PARAMETER    :: mod_orb_subFmt =  23       ! SUB-file format

  INTEGER(i4b), PARAMETER    :: mod_orb_eqmPol =  51       ! Equations of motion: polynomial degree
  INTEGER(i4b), PARAMETER    :: mod_orb_eqmInt =  52       ! Equations of motion: Length of interval
  INTEGER(i4b), PARAMETER    :: mod_orb_veqPol =  53       ! Variational equations: polynomial degree
  INTEGER(i4b), PARAMETER    :: mod_orb_veqInt =  54       ! Variational equations: Length of interval
  INTEGER(i4b), PARAMETER    :: mod_orb_empiri =  55       ! Empirical parameters

! Model structure
! ---------------
  TYPE t_model
    INTEGER(i4b)                :: key
    CHARACTER(LEN=chrValLength) :: chrVal
    CHARACTER(LEN=8)            :: srName
    REAL(r8b)                   :: numVal
  END TYPE t_model

  TYPE(t_model),DIMENSION(100) :: model

  INTEGER(i4b), SAVE :: nSet = 0
  INTEGER(i4b)       :: iSet

  LOGICAL,SAVE       :: first = .TRUE.


CONTAINS

! -------------------------------------------------------------------------
! Initialize structure
! -------------------------------------------------------------------------

  SUBROUTINE init_model
    model(:)%key      = 0
    model(:)%chrVal   = ' '
    model(:)%srName   = ' '
    model(:)%numVal   = 0.D0
  END SUBROUTINE init_model


! SR setModKey: Set model key values
! ----------------------------------
  SUBROUTINE setModKey(key,chrVal,srName,numVal)

! IN:
    INTEGER(i4b)        :: key
    CHARACTER(LEN=chrValLength)   :: chrVal
    CHARACTER(LEN=8)    :: srName
    REAL(r8b)           :: numVal

! Local variables
    LOGICAL             :: keyExist

    IF (first) THEN
      first = .FALSE.
      CALL init_model
    ENDIF

    keyExist = .FALSE.

    DO iSet=1,nSet
      IF (model(iSet)%key == key) THEN
        WRITE(lfnErr,'(/,1X,A,A8,A,A,/,19X,A,I5,/,19X,A,A,/,19X,A,A,/)')&
             '### SR ',srName,' : Model key already set by SR ',model(iSet)%srName, &
             'Model key: ',model(iSet)%key,&
             'Model chrVal: ',TRIM(model(iSet)%chrVal),&
             'Input chrVal: ',TRIM(chrVal)
        keyExist = .TRUE.
        EXIT
      ENDIF
    ENDDO

    IF (.NOT.keyExist) THEN
      nSet = nSet + 1

      model(nSet)%key=key
      model(nSet)%chrVal=chrVal
      model(nSet)%srName = srName
      model(nSet)%numVal = numVal
    ENDIF

  END SUBROUTINE setModKey


! SR getModKey: Get model key values
! ----------------------------------
  SUBROUTINE getModKey(key,chrVal,srName,numVal)
! IN:
    INTEGER(i4b)        :: key

! OUT:
    CHARACTER(LEN=chrValLength)   :: chrVal
    CHARACTER(LEN=8)    :: srName
    REAL(r8b)           :: numVal

    LOGICAL             :: keyGet

    IF (first) THEN
      first = .FALSE.
      CALL init_model
      WRITE(lfnErr,'(/,A,I5,/)')&
           '*** Module D_MODEL: Model keys not yet set!! ',key
      CALL exitrc(2)
    ENDIF

    keyGet = .FALSE.
    chrVal = ' '
    srName = ' '
    numVal = 0.D0
    DO iSet=1,nSet
      IF (key == model(iSet)%key) THEN
        chrVal = model(iSet)%chrVal
        srName = model(iSet)%srName
        numVal = model(iSet)%numVal
        keyGet =.TRUE.
        EXIT
      ENDIF
    ENDDO

    IF (.NOT.keyGet) THEN
      WRITE(lfnErr,'(/,A,/,20X,A,I5,/)') &
           '*** Module D_MODEL: Model key not found ',     &
                               'Model key: ',key
      CALL exitrc(2)
    ENDIF


  END SUBROUTINE getModKey


! SR chkModKey: check consistency of model keys or check, if already available
! ----------------------------------------------------------------------------
  SUBROUTINE chkModKey(iMod,key,chrVal,numVal,irChk)
! IN:
    INTEGER(i4b)        :: iMod     ! =0: No stop; =1: Stop, if inconsistent
                                    ! =2: Check, if already available
    INTEGER(i4b)        :: key
    CHARACTER(LEN=chrValLength)   :: chrVal
    REAL(r8b)           :: numVal

! OUT:
    INTEGER(i4b)        :: irChk

! Local variables
    LOGICAL             :: chkDescr
    LOGICAL             :: chkVal

    chkDescr = .FALSE.
    chkVal = .FALSE.
    irChk = 999

    IF (iMod < 2) THEN
      DO iSet=1,nSet
        IF (key == model(iSet)%key) THEN
          IF (chrVal == model(iSet)%chrVal) chkDescr = .TRUE.
          IF (numVal == model(iSet)%numVal) chkVal = .TRUE.
          IF (chkDescr.AND.chkVal) THEN
            irChk = 0
            EXIT
          ELSE
            irChk = 1
            IF (iMod == 0) WRITE(lfnErr,'(A)')'### SR chkModKey: Models inconsistent'
            IF (iMod == 1) WRITE(lfnErr,'(A)')'*** SR chkModKey: Models inconsistent'
            WRITE(lfnErr,'(18X,A,A,/,18X,A,A,/)')&
                 'Model description (input): ',chrVal,&
                 'Model description (data) : ',model(iSet)%chrVal
            IF (iMod == 1) CALL EXITRC(2)
          ENDIF
        ENDIF
      ENDDO
    ELSEIF (iMod == 2) THEN
      DO iSet=1,nSet
        IF (key == model(iSet)%key) THEN
          chrVal = model(iSet)%chrVal
          numVal = model(iSet)%numVal
          irChk = 2
          EXIT
        ENDIF
      ENDDO
    ENDIF

  END SUBROUTINE chkModKey

END MODULE d_model

