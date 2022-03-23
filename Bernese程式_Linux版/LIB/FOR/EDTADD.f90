MODULE s_EDTADD
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE edtAdd(iTyp,iFil,satNum,lc,numObs,obsTim,edt)

! -------------------------------------------------------------------------
! Purpose:   Add EDIT-request to edt-structure (from CLKEST or LEOKIN)
!
! Author:    H. Bock
!
! Created:   20-Jun-2002
!
! Changes:   27-Apr-2012 RD: Nullify all pointers, use m_bern with only
!
! Copyright: Astronomical Institute
!            University of Bern
!            Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b
  USE d_edit,   ONLY: t_edit, t_edtRec
  USE s_alcerr
  IMPLICIT NONE

! List of parameters
! ------------------
! IN:
  INTEGER(i4b) :: iTyp    ! Type of edit-request =1: Mark Code observation
                          !                      =4: Set up new ambiguity and
                          !                          mark phase observations
  INTEGER(i4b) :: iFil    ! File number in edt-file
  INTEGER(i4b) :: satNum  ! Satellite number
  INTEGER(i4b) :: lc      ! Linear Combination to be marked
  INTEGER(i4b) :: numObs  ! Number of minimum good observations
  REAL(r8b)    :: obsTim  ! Observation epoch

! IN/OUT:
  TYPE(t_edit) :: edt     ! edt-structure

! Functions
! ---------
! Local parameters
! ----------------
! Local types
! -----------

! Local variables
! ---------------
  TYPE(t_edtRec),DIMENSION(:),POINTER  :: hlpRec

  INTEGER(i4b) :: iEpoch
  INTEGER(i4b) :: iEdt,nEdt
  INTEGER(i4b) :: iac

! Nullify pointers
! ----------------
  NULLIFY(hlpRec)

! Compute epoch index
! -------------------
  iEpoch=IDNINT((obsTim-edt%head(iFil)%timEdt)*86400.D0&
       &/edt%head(iFil)%idtEdt+1.D0)

  nEdt = edt%nEdt

! Look if ambiguity is already set up
! -----------------------------------
  IF (iTyp == 4) THEN
    DO iEdt = 1,nEdt
      IF (edt%rec(iEdt)%lstEdt(1) == satNum.AND.&
           edt%rec(iEdt)%lstEdt(4) == lc    .AND.&
           edt%rec(iEdt)%lstEdt(6) == iFil  .AND.&
           edt%rec(iEdt)%lstEdt(7) == iTyp ) THEN
        IF (edt%rec(iEdt)%lstEdt(2) == iEpoch.AND.&
           edt%rec(iEdt)%lstEdt(3) == iEpoch) RETURN

! Mark phase observations if they are shortly before the new ambiguity
! --------------------------------------------------------------------
        IF (iAbs(iEpoch-edt%rec(iEdt)%lstEdt(3)) <= edt%minAmb) THEN
          edt%rec(iEdt)%lstEdt(3) = iEpoch - 1
          edt%rec(iEdt)%lstEdt(7) = 1
        ENDIF
      ENDIF
    ENDDO

! Look if already request for this satellite is there
! ---------------------------------------------------
  ELSEIF (iTyp == 1) THEN
    DO iEdt = 1,nEdt
      IF (edt%rec(iEdt)%lstEdt(1) == satNum.AND.&
           iAbs(iEpoch - edt%rec(iEdt)%lstEdt(3)) <= numObs + 1.AND.&
           edt%rec(iEdt)%lstEdt(4) == lc      .AND.&
           edt%rec(iEdt)%lstEdt(6) == iFil    .AND.&
           edt%rec(iEdt)%lstEdt(7) == iTyp ) THEN
        edt%rec(iEdt)%lstEdt(3) = iEpoch
        RETURN
      ENDIF
    ENDDO
  ENDIF

! Add new edt-request
! (ambiguity for phase observations or mark for code observations)
! ----------------------------------------------------------------
  edt%nEdt=edt%nEdt+1

  IF (edt%nEdt>size(edt%rec)) THEN
    ALLOCATE(hlpRec(size(edt%rec)),stat=iac)
    CALL alcErr(iac, 'hlpRec', (/size(edt%rec)/), 'edtAdd')
    hlpRec=edt%rec
    DEALLOCATE(edt%rec,stat=iac)
    ALLOCATE(edt%rec(size(hlpRec)+100),stat=iac)
    CALL alcErr(iac, 'edt%rec', (/size(hlpRec)+100/), 'edtAdd')
    edt%rec(1:size(hlpRec))=hlpRec
    DEALLOCATE(hlpRec,stat=iac)
  ENDIF

  edt%rec(edt%nEdt)%lstEdt(1) = satNum
  edt%rec(edt%nEdt)%lstEdt(2) = iEpoch
  edt%rec(edt%nEdt)%lstEdt(3) = iEpoch
  edt%rec(edt%nEdt)%lstEdt(4) = lc
  edt%rec(edt%nEdt)%lstEdt(5) = 1
  edt%rec(edt%nEdt)%lstEdt(6) = iFil
  edt%rec(edt%nEdt)%lstEdt(7) = iTyp

  RETURN
  END SUBROUTINE edtAdd

END MODULE
