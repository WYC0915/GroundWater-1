MODULE s_SELEDT
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE seledt(iFil,opt,obsHead,nEdt,edtLst,iEdit,fildid)

! -------------------------------------------------------------------------
! Purpose:    Generate the edit list corresponding to the file
!
! Author:     R. Dach
!
! Created:    19-Feb-2002
! Last mod.:  21-May-2003
!
! Changes:    22-Jul-2002  HB: Use modified t_obsHead
!             17-May-2003  HU: Initialize structure
!             21-May-2003  RD: Init time window (/ 0d0,1d20 /)
!
! SR called:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_gpsobs, ONLY: t_obsHead
  USE d_edit,   ONLY: t_edtRec, t_edit, init_edit
  USE p_satmrk, ONLY: t_satmrk_opt
  USE s_alcerr
  USE s_rdedit2
  USE s_mrkall
  USE s_exitrc
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  INTEGER(i4b)             :: iFil     ! Actual file number in list
  TYPE(t_satmrk_opt)       :: opt      ! SATMRK input options
  TYPE(t_obsHead)          :: obsHead  ! Observation file header information

! input/output:
  INTEGER(i4b)             :: iEdit    ! Index in fildid (file edit) list

! output:
  INTEGER(i4b)             :: nEdt     ! Number of edit requests
  TYPE(t_edtRec),           &
    DIMENSION(:),  POINTER :: edtLst   !  Definition of edit requests
  INTEGER(i4b),             &
    DIMENSION(:,:),POINTER :: fildid   ! Reports what edits done

! List of functions
! -----------------

! Local types
! -----------

! Local parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER :: srName = 'seledt'

! Local Variables
! ---------------
  TYPE(t_edit), SAVE          :: edit

  INTEGER(i4b),SAVE           :: ifirst=1
  INTEGER(i4b)                :: iEdf,iEdt
  INTEGER(i4b)                :: kedFil
  INTEGER(i4b)                :: iSat
  INTEGER(i4b)                :: iSatel
  INTEGER(i4b)                :: irc

  REAL(r8b)                   :: timdif

!
! Alternative 1:
!
! Synchronisation of two files
! ----------------------------
  IF (opt%syc == 1) THEN

    CALL mrkAll(iFil,opt,obsHead,nEdt,edtLst)

    opt%minAmb = 0
    opt%iSampl = 0

    filDid = 0

    iEdit  = 1

!
! Alternative 2:
!
! Get edit info: edit file
! ------------------------
  ELSE IF (opt%edtFil /= '') THEN

! Read the edit file into memory
! ------------------------------
    IF (iFil == 1 .AND. iEdit == 0) THEN
      IF (ifirst==1) CALL init_edit(edit)
      ifirst=0

      edit%filNam = opt%edtFil

      CALL rdedit2(edit)

      opt%minAmb = edit%minAmb
      opt%iSampl = edit%iSampl

      DEALLOCATE(filDid,stat=irc)

      ALLOCATE(filDid(8,edit%nEdFil),stat=irc)
      CALL alcerr(irc,'filDid',(/8,edit%nedFil/),srName)

      filDid = -1
    ENDIF

! Search the correct file number
! ------------------------------
    kedFil = 0

    DO iedf = iEdit+1,edit%nedFil+1

      IF (iedf > edit%nedFil) THEN
        iEdit = iEdf
        RETURN
      ENDIF

      IF (edit%head(iEdf)%cseedt(1) /= obsHead%csess(1)  .OR. &
          edit%head(iEdf)%cseedt(2) /= obsHead%csess(2)  .OR. &
          edit%head(iEdf)%idtedt    /= obsHead%ideltt    .OR. &
          edit%head(iEdf)%meaedt    /= obsHead%meatyp    .OR. &
          edit%head(iEdf)%staedt(1) /= obsHead%sta(1)%stanam ) CYCLE

      IF (obsHead%nDiff  == 1 .AND. &
          edit%head(iEdf)%staedt(2) /= obsHead%sta(2)%stanam ) CYCLE

      IF (obsHead%nDiff  == 0 .AND. &
          LEN_TRIM(edit%head(iEdf)%staedt(2)) > 0) CYCLE

      timdif = 86400.D0 * DABS(edit%head(iEdf)%timedt - obsHead%timref)

      IF (timdif >= 1.D0) THEN
        WRITE(lfnerr,'(/,A,/,16X,A,F10.3,A,/,16X,2A,/)')                      &
             ' ### SR SELEDT: Difference concerning first observation epoch', &
                             'Difference: ',timdif,' seconds',                &
                             'Observation file: ',TRIM(opt%filNam(1,iFil))
        IF (timdif > obsHead%iDeltT/2) CYCLE
      ENDIF

      filDid(:,iedf) = 0
      kedFil = iedf
      iEdit  = iedf

      EXIT

    ENDDO

! Corresponding file not found
! ----------------------------
    IF (kedFil == 0) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,A,/,16X,A,/)')                     &
            ' ### SR SELEDT: No file found in editing file list',   &
            'Corresponding to the file: ',TRIM(opt%filNam(1,iFil)), &
            'No editing is performed for this file !'
    ENDIF

! Count the number of requests
! ----------------------------
    nEdt = 0
    DO iEdt = 1,edit%nEdt

      IF (kedFil == 0) EXIT

      IF (kedFil /= edit%rec(iEdt)%lstEdt(6)) CYCLE

      DO iSatel = 1,obsHead%nSatel

        IF (edit%rec(iEdt)%lstEdt(1) == obsHead%sat(iSatel)%numSat) THEN
          nEdt = nEdt+1
          EXIT
        ENDIF

      ENDDO
    ENDDO

! Allocate the memory for editing requests
! ----------------------------------------
    DEALLOCATE(edtLst,stat=irc)

    ALLOCATE(edtLst(nEdt),stat=irc)
    CALL alcerr(irc,'edtLst',(/nEdt/),srName)


! Copy the requests concerning the file and satellites
! ----------------------------------------------------
    nEdt = 0
    DO iEdt = 1,edit%nEdt

      IF (kedFil == 0) EXIT

      IF (kedFil /= edit%rec(iEdt)%lstEdt(6)) CYCLE

      DO iSatel = 1,obsHead%nSatel

        IF (edit%rec(iEdt)%lstEdt(1) == obsHead%sat(iSatel)%numSat) THEN

          nEdt = nEdt+1
          edtLst(nEdt) = edit%rec(iEdt)

          EXIT

        ENDIF

      ENDDO
    ENDDO

! Is it non-sense or a feature in the F77 program?
! ------------------------------------------------
    IF (1 == 2) THEN
      edtLst(1:nEdt)%lstEdt(7) = opt%mrkTyp
    ENDIF

!
! Alternative 3:
!
! Read option input file (if editing info file not available)
! -----------------------------------------------------------
  ELSE IF (opt%edtFil=='') THEN

    filDid = 0
    iEdit  = 1

! Check options: do not remove all satellites for all epochs
! ----------------------------------------------------------
    IF (opt%mrkTyp    == 2 .AND. opt%mrkSat(1) ==    0  .AND. &
        opt%mrkEpo(1) == 1 .AND. opt%mrkEpo(2) == 9999) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,/)')                         &
            ' *** SR SELEDT: Remove all satellites ',         &
                            'in all epochs is not allowed!'
      CALL exitrc(2)
    END IF

! Get the number of satellites involved
! -------------------------------------
    nEdt = 0
    DO iSat = 1,size(opt%mrkSat)

      IF (opt%mrkSat(iSat) == 0) THEN
        nEdt = obsHead%nSatel
        EXIT
      ENDIF

      DO iSatel = 1, obsHead%nSatel
        IF (opt%mrkSat(iSat) == obsHead%sat(iSatel)%numSat) THEN
          nEdt = nEdt+1
          EXIT
        ENDIF
      ENDDO

    ENDDO

! Allocate the memory for editing requests
! ----------------------------------------
    DEALLOCATE(edtLst,stat=irc)

    ALLOCATE(edtLst(nEdt),stat=irc)
    CALL alcerr(irc,'edtLst',(/ nEdt /),srName)

! Put edit information into record
! --------------------------------
    nEdt = 0

    DO iSatel = 1, obsHead%nSatel

      DO iSat = 1,SIZE(opt%mrkSat)

        IF (opt%mrkSat(iSat) == 0 .OR. &
            opt%mrkSat(iSat) == obsHead%sat(iSatel)%numSat) THEN

          nEdt = nEdt+1

          edtLst(nEdt)%lstEdt(1) = obsHead%sat(iSatel)%numSat

          IF (opt%mrkEpo(1) /= 0 .OR. opt%mrkEpo(2) /= 0) THEN
            edtLst(nEdt)%lstEdt(2:3) = opt%mrkEpo(:)
          ELSE
            edtLst(nEdt)%lstEdt(2:3) = (/ 1,obsHead%nEpoch /)
            IF (opt%mrkTim(1) /= 0d0) &
              edtLst(nEdt)%lstEdt(2) = IDNINT((opt%mrkTim(1)-obsHead%timref) * &
                                       86400d0/DBLE(obsHead%ideltt)+1.D0)
            IF (opt%mrkTim(2) /= 1d20) &
              edtLst(nEdt)%lstEdt(3) = IDNINT((opt%mrkTim(2)-obsHead%timref) * &
                                       86400d0/DBLE(obsHead%ideltt)+1.D0)
          ENDIF

          edtLst(nEdt)%lstEdt(4) = opt%mrkFrq
          edtLst(nEdt)%lstEdt(5) = 0
          edtLst(nEdt)%lstEdt(6) = iFil

          edtLst(nEdt)%lstEdt(7) = opt%mrkTyp

        ENDIF

      ENDDO

    ENDDO

! No minimum number of obs. per ambiguity for manual
! --------------------------------------------------
    IF (iFil == 1) THEN
      opt%minAmb = 0
      opt%iSampl = 0
    ENDIF

  ENDIF

  RETURN
END SUBROUTINE seledt

END MODULE
