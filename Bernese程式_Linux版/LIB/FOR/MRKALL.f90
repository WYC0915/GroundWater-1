MODULE s_MRKALL
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE mrkAll(iFil,opt,obsHead,nedt,edtLst)

! --------------------------------------------------------------------
!  Purpose   : Preparation of an edit file for satmrk to
!              syncronize phase and code observation marks
!
!  Author    : H. Bock
!
!  Created   : 24-Sep-2001
!  Last mod. : 28-Jun-2005
!
!  Changes   : 09-Oct-2001  HB: Dynamisation of MAXFIL and MAXEDT
!              29-Dec-2001  HU: Interface to alcerr added
!              14-Feb-2002  RD: Do not include LFNUM.inc
!                               Deallocate EDT-arrays
!                               Obs. may be marked+deleted (nEdt counting)
!              25-Feb-2002  RD: Use edit-structures
!              22-Jul-2002  HB: Use modified t_obsHead
!              17-Feb-2003  LM: Use m_maxdim
!              23-Apr-2003  AJ: Nullify local pointers
!              16-May-2003  MM: Initialize and deallocate structure
!              13-Sep-2003  HU: Interface for defreq
!              28-Jun-2005  MM: Unused variables removed
!
!  SR called : alcErr, opnFil, opnErr, rdhead2, rdobsi, exitrc,
!              init_obsHead
!
!  Copyright : Astronomical Institute
!              University of Bern
!              Switzerland
! --------------------------------------------------------------------

! Modules
! ------
  USE m_bern
  USE p_satmrk, ONLY : t_satmrk_opt
  USE d_gpsobs, ONLY : t_obshead,init_obsHead
  USE d_edit,   ONLY : t_edtRec
  USE s_rdobsi
  USE s_alcerr
  USE s_opnfil
  USE f_tstflg
  USE s_opnerr
  USE s_rdhead2
  USE s_defreq
  IMPLICIT NONE

! List of parameters
! ----------
! input
  INTEGER(i4b)                         :: iFil    ! File index
  TYPE(t_satmrk_opt)                   :: opt     ! SATMRK options
  TYPE(t_obshead)                      :: obsHead ! Header of file to sync.

! output
  INTEGER(i4b)                         :: nEdt    ! # of editing requests
  TYPE(t_edtRec),DIMENSION(:),POINTER  :: edtLst  ! Definition of edit requests

! Local parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER   :: srName = 'mrkall'

! Local variables
! ---------------
  TYPE(t_obshead)               :: synHead ! Header of master file

  CHARACTER(LEN=6)              :: mxnSat
  CHARACTER(LEN=1)              :: epoFLg  ! Epoch flag
  CHARACTER(LEN=1),              &
    DIMENSION(:,:), POINTER     :: obsFlg  ! Observation flag: obsFlg(iSat,iFrq)

  INTEGER(i4b),                  &
    DIMENSION(:),   POINTER     :: nrSat   ! Satellite numbers for obs.
  INTEGER(i4b),     DIMENSION(2):: iFrqs   ! Frequency list for rdobsi
  INTEGER(i4b)                  :: nSat    ! Number of sat. per epoch
  INTEGER(i4b)                  :: mxcSat,mxoSat
  INTEGER(i4b),                  &
    DIMENSION(:,:), POINTER     :: lstObs
  INTEGER(i4b)                  :: iSat
  INTEGER(i4b)                  :: iSatel,jSatel
  INTEGER(i4b)                  :: iEpoch
  INTEGER(i4b)                  :: iEpo
  INTEGER(i4b)                  :: iFrq
  INTEGER(i4b)                  :: ios,irc

  REAL(r8b)                     :: obsTim  ! Epoch of obervation (MJD)
  REAL(r8b),                     &
    DIMENSION(:,:), POINTER     :: observ  ! Observation: observ(iSat,iFrq)
  REAL(r8b),        DIMENSION(2):: deltaT  ! Clock corrections for epoch
  REAL(r8b),        DIMENSION(2):: epoFrq


  COMMON/MCMSAT/MXCSAT,MXNSAT

  NULLIFY(obsFlg)
  NULLIFY(nrSat)
  NULLIFY(lstObs)
  NULLIFY(observ)
  CALL init_obsHead(synHead)

! Deallocate (if not the first call anymore)
! ------------------------------------------
  DEALLOCATE(edtLst,stat=irc)

! Read header of the observation files
! -----------------------------------
  CALL rdhead2(opt%filSyc(1,ifil), synhead)

! Store (old) common maxsat
! -------------------------
  mxosat = mxcsat
  mxcsat = synHead%nSatel

! Allocate arrays for reading of the observation file
! ---------------------------------------------------
  iFrqs(:) = (/ 1,2 /)

  ALLOCATE(nrSat(synHead%nSatel),stat=irc)
  CALL alcerr(irc,'nrSat' ,(/ synHead%nSatel /), srName)

  ALLOCATE(observ(synHead%nSatel,synHead%nFreq), stat=irc)
  CALL alcerr(irc,'observ',(/ synHead%nSatel,synHead%nFreq /), srName)

  ALLOCATE(obsflg(synHead%nSatel,synHead%nFreq), stat=irc)
  CALL alcerr(irc,'obsflg',(/ synHead%nSatel,synHead%nFreq /), srName)

! Init last obs. status flags
! ---------------------------
  ALLOCATE(lstObs(synHead%nSatel,synHead%nFreq), stat=irc)
  CALL alcerr(irc,'lstObs',(/ synHead%nSatel,synHead%nFreq /), srName)

  IF ((obsHead%timref - synHead%timref) * 86400d0 < -1d0) THEN
    lstObs = 2
  ELSE
    lstObs = 0
  ENDIF

! Define satellite frequencies
! ----------------------------
  epofrq(1)=synHead%timref
  epofrq(2)=synHead%timref+(synHead%nEpoch-1)*DBLE(synHead%iDeltT)/86400.D0

  CALL defreq(epofrq,synHead%nSatel,synHead%sat(:)%numSat)

! Open the observation file
! -------------------------
  CALL opnfil(lfn001,opt%filSyc(2,iFil),'UNKNOWN','UNFORMATTED', &
              ' ',' ',ios)
  CALL opnerr(lfnErr,lfn001,ios,opt%filSyc(2,iFil),srName)

! Loop over all observation epochs (to count the edit requests)
! -------------------------------------------------------------
  nEdt = 0
  DO iEpo=1,synHead%nEpoch

    CALL rdobsi(lfn001,synHead%iFrmat,synHead%nFreq,iFrqs,  &
                obsTim,deltat,epoFlg,nSat,nrSat,obsFlg,observ,irc)

! Check end of file
! -----------------
    IF (irc == 1) EXIT

! Loop satellites of the epoch
! ----------------------------
    DO iSat = 1,nSat

      DO iSatel = 1,synHead%nSatel
        IF (nrSat(iSat) == synHead%sat(iSatel)%numSat) EXIT
      ENDDO

      DO iFrq = 1,synHead%nFreq

! Observation deleted
! -------------------
        IF (observ(iSat,iFrq) == 0d0          .AND. &
            lstObs(iSatel,iFrq) /= 2) THEN
          nEdt = nedt + 1
          lstObs(iSatel,iFrq) = 2
        ENDIF

! Observation marked and not deleted
! ----------------------------------
        IF (observ(iSat,iFrq) /= 0d0    .AND. &
            tstflg(obsFlg(iSat,iFrq),0) .AND. &
            lstObs(iSatel,iFrq) /= 1) THEN
          nEdt = nedt + 1
          lstObs(iSatel,iFrq) = 1
        ENDIF

! Observation not marked and not deleted
! --------------------------------------
        IF (observ(iSat,iFrq) /= 0d0          .AND. &
            .NOT. tstflg(obsFlg(iSat,iFrq),0) .AND. &
            lstObs(iSatel,iFrq) /= 0) THEN
          nEdt = nedt + 1
          lstObs(iSatel,iFrq) = 0
        ENDIF

      ENDDO  ! Next frequency of the sat.

    ENDDO  ! Next satellite of the epo.

! Satellites not avail. this epoch
! --------------------------------
    iSatelLoop: DO iSatel = 1,synHead%nSatel
      DO iSat = 1,nSat
        IF (nrSat(iSat) == synHead%sat(iSatel)%numSat) CYCLE iSatelLoop
      ENDDO

      DO iFrq = 1,synHead%nFreq
        IF (lstObs(iSatel,iFrq) /= 2) THEN
          nEdt = nedt + 1
          lstObs(iSatel,iFrq) = 2
        ENDIF
      ENDDO
    ENDDO iSatelLoop


  ENDDO  ! Next epoch from file

! Master file is longer than obs. file
! ------------------------------------
  IF (((obsHead%timref+(obsHead%nEpoch-1)*obsHead%iDeltT/86400d0) - &
       (synHead%timref+(synHead%nEpoch-1)*synHead%iDeltT/86400d0)) * 86400d0 &
        > 1d0) THEN

    DO iSatel = 1,synHead%nSatel
      DO iFrq = 1,synHead%nFreq
        IF (lstObs(iSatel,iFrq) /= 2) nEdt = nedt+1
      ENDDO
    ENDDO

  ENDIF

! More satellites in the obs. file
! --------------------------------
  IF (obsHead%nSatel > synHead%nSatel) &
    nEdt = nEdt + (obsHead%nSatel-synHead%nSatel)*synHead%nFreq

! More frequencies in the obs. file
! ---------------------------------
  IF (obsHead%nFreq > synHead%nFreq) &
    nEdt = nEdt + (obsHead%nFreq-synHead%nFreq)*obsHead%nSatel


! Allocate memory
! ---------------
  ALLOCATE(edtLst(nEdt),stat=irc)
  CALL alcerr(irc, 'edtLst', (/ nEdt /), srName)

  edtLst(:)%lstCyc = 0.D0

! Re-init last obs. status flags
! ------------------------------
  nEdt = 0
  IF ((obsHead%timref - synHead%timref) * 86400d0 < -1d0) THEN

    DO iSatel = 1,synHead%nSatel
      DO iFrq = 1,synHead%nFreq
        nEdt = nedt + 1
        edtLst(nEdt)%lstEdt(1) = synHead%sat(iSatel)%numSat
        edtLst(nEdt)%lstEdt(2) = 1
        edtLst(nEdt)%lstEdt(3) = obsHead%nEpoch
        edtLst(nEdt)%lstEdt(4) = iFrq
        edtLst(nEdt)%lstEdt(5) = 0
        edtLst(nEdt)%lstEdt(6) = iFil
        edtLst(nEdt)%lstEdt(7) = 2

        lstObs(iSatel,iFrq) = nEdt
      ENDDO
    ENDDO

  ELSE
    lstObs = 0
  ENDIF

! Second epoch loop
! -----------------
  REWIND(lfn001)

  DO iEpo=1,synHead%nEpoch

    CALL rdobsi(lfn001,synhead%iFrmat,synHead%nFreq,iFrqs,  &
                obsTim,deltat,epoFlg,nSat,nrSat,obsFlg,observ,irc)

! Check end of file
! -----------------
    IF (irc == 1) EXIT

! Compute epoch number
! --------------------
    iEpoch=IDNINT((obsTim-obsHead%timRef)*86400.D0/obsHead%iDeltt+1.D0)

! Loop satellites of the epoch
! ----------------------------
    DO iSat = 1,nSat

      DO iSatel = 1,synHead%nSatel
        IF (nrSat(iSat) == synHead%sat(iSatel)%numSat) EXIT
      ENDDO

      DO iFrq = 1,synHead%nFreq

! Observation deleted
! -------------------
        IF (observ(iSat,iFrq) == 0d0) THEN

          IF (lstObs(iSatel,iFrq) /= 0) THEN
            IF (edtLst(lstObs(iSatel,iFrq))%lstEdt(7) == 2) CYCLE
            edtLst(lstObs(iSatel,iFrq))%lstEdt(3) = iEpoch - 1
          ENDIF

          nEdt = nedt + 1
          edtLst(nEdt)%lstEdt(1) = synHead%sat(iSatel)%numSat
          edtLst(nEdt)%lstEdt(2) = iEpoch
          edtLst(nEdt)%lstEdt(3) = obsHead%nEpoch
          edtLst(nEdt)%lstEdt(4) = iFrq
          edtLst(nEdt)%lstEdt(5) = 0
          edtLst(nEdt)%lstEdt(6) = iFil
          edtLst(nEdt)%lstEdt(7) = 2

          lstObs(iSatel,iFrq) = nEdt
        ENDIF

! Observation marked and not deleted
! ----------------------------------
        IF (observ(iSat,iFrq) /= 0d0 .AND. tstflg(obsFlg(iSat,iFrq),0)) THEN

          IF (lstObs(iSatel,iFrq) /= 0) THEN
            IF (edtLst(lstObs(iSatel,iFrq))%lstEdt(7) == 1) CYCLE
            edtLst(lstObs(iSatel,iFrq))%lstEdt(3) = iEpoch - 1
          ENDIF

          nEdt = nedt + 1
          edtLst(nEdt)%lstEdt(1) = synHead%sat(iSatel)%numSat
          edtLst(nEdt)%lstEdt(2) = iEpoch
          edtLst(nEdt)%lstEdt(3) = obsHead%nEpoch
          edtLst(nEdt)%lstEdt(4) = iFrq
          edtLst(nEdt)%lstEdt(5) = 0
          edtLst(nEdt)%lstEdt(6) = iFil
          edtLst(nEdt)%lstEdt(7) = 1

          lstObs(iSatel,iFrq) = nEdt
        ENDIF

! Observation not marked and not deleted
! --------------------------------------
        IF (observ(iSat,iFrq) /= 0d0          .AND. &
            .NOT. tstflg(obsFlg(iSat,iFrq),0) .AND. &
            lstObs(iSatel,iFrq) /= 0) THEN

          edtLst(lstObs(iSatel,iFrq))%lstEdt(3) = iEpoch - 1

          lstObs(iSatel,iFrq) = 0
        ENDIF

      ENDDO  ! Next frequency of the sat.

    ENDDO  ! Next satellite of the epo.

! Satellites not avail. this epoch
! --------------------------------
    jSatelLoop: DO iSatel = 1,synHead%nSatel
      DO iSat = 1,nSat
        IF (nrSat(iSat) == synHead%sat(iSatel)%numSat) CYCLE jSatelLoop
      ENDDO

      DO iFrq = 1,synHead%nFreq

        IF (lstObs(iSatel,iFrq) /= 0) THEN
          IF (edtLst(lstObs(iSatel,iFrq))%lstEdt(7) == 2) CYCLE
          edtLst(lstObs(iSatel,iFrq))%lstEdt(3) = iEpoch - 1
        ENDIF

        nEdt = nedt + 1
        edtLst(nEdt)%lstEdt(1) = synHead%sat(iSatel)%numSat
        edtLst(nEdt)%lstEdt(2) = iEpoch
        edtLst(nEdt)%lstEdt(3) = obsHead%nEpoch
        edtLst(nEdt)%lstEdt(4) = iFrq
        edtLst(nEdt)%lstEdt(5) = 0
        edtLst(nEdt)%lstEdt(6) = iFil
        edtLst(nEdt)%lstEdt(7) = 2

        lstObs(iSatel,iFrq) = nEdt
      ENDDO
    ENDDO jSatelLoop

  ENDDO  ! Next epoch

! Master file is longer than obs. file
! ------------------------------------
  IF (((obsHead%timref+(obsHead%nEpoch-1)*obsHead%iDeltT/86400d0) - &
       (synHead%timref+(synHead%nEpoch-1)*synHead%iDeltT/86400d0)) * 86400d0 &
        > 1d0) THEN

    obsTim = synHead%timref+(synHead%nEpoch-1)*synHead%iDeltT/86400d0
    iEpoch=IDNINT((obsTim-obsHead%timRef)*86400.D0/obsHead%iDeltt+1.D0)

    DO iSatel = 1,synHead%nSatel
      DO iFrq = 1,synHead%nFreq

        IF (lstObs(iSatel,iFrq) /= 0) THEN
          IF (edtLst(lstObs(iSatel,iFrq))%lstEdt(7) == 2) CYCLE
          edtLst(lstObs(iSatel,iFrq))%lstEdt(3) = iEpoch
        ENDIF

        nEdt = nedt + 1
        edtLst(nEdt)%lstEdt(1) = synHead%sat(iSatel)%numSat
        edtLst(nEdt)%lstEdt(2) = iEpoch+1
        edtLst(nEdt)%lstEdt(3) = obsHead%nEpoch
        edtLst(nEdt)%lstEdt(4) = iFrq
        edtLst(nEdt)%lstEdt(5) = 0
        edtLst(nEdt)%lstEdt(6) = iFil
        edtLst(nEdt)%lstEdt(7) = 2

        lstObs(iSatel,iFrq) = nEdt
      ENDDO
    ENDDO

  ENDIF

! More satellites in the obs. file
! --------------------------------
  IF (obsHead%nSatel > synHead%nSatel) THEN
    DO iSatel = 1,obsHead%nSatel
      iSat = 0
      DO jSatel = 1,synHead%nSatel
        IF (obsHead%sat(iSatel)%numSat == synHead%sat(jSatel)%numSat) iSat = jSatel
      ENDDO
      IF (iSat /= 0) CYCLE

      nEdt = nEdt + 1
      edtLst(nEdt)%lstEdt(1) = obsHead%sat(iSatel)%numSat
      edtLst(nEdt)%lstEdt(2) = 1
      edtLst(nEdt)%lstEdt(3) = obsHead%nEpoch
      edtLst(nEdt)%lstEdt(4) = 3
      edtLst(nEdt)%lstEdt(5) = 0
      edtLst(nEdt)%lstEdt(6) = iFil
      edtLst(nEdt)%lstEdt(7) = 2
    ENDDO
  ENDIF

! More frequencies in the obs. file
! ---------------------------------
  IF (obsHead%nFreq > synHead%nFreq) THEN
    DO iSatel = 1,obsHead%nSatel
      nEdt = nEdt + 1
      edtLst(nEdt)%lstEdt(1) = obsHead%sat(iSatel)%numSat
      edtLst(nEdt)%lstEdt(2) = 1
      edtLst(nEdt)%lstEdt(3) = obsHead%nEpoch
      edtLst(nEdt)%lstEdt(4) = 2
      edtLst(nEdt)%lstEdt(5) = 0
      edtLst(nEdt)%lstEdt(6) = iFil
      edtLst(nEdt)%lstEdt(7) = 2
    ENDDO
  ENDIF


  CLOSE(lfn001)

! Deallocate local arrays
! -----------------------
  DEALLOCATE(nrSat,  stat=irc)
  DEALLOCATE(observ, stat=irc)
  DEALLOCATE(obsflg, stat=irc)
  DEALLOCATE(lstObs, stat=irc)
  DEALLOCATE(synHead%sat,stat=ios)
  DEALLOCATE(synHead%ambigu,stat=ios)

! Prevent any deletions for synchronization, only mark
! (usually inactive, but may be somebody will use it...)
! ----------------------------------------------------
! edtLst(:)%lstEdt(7) = 1
!

! restore (old) common maxsat
! ---------------------------
  mxcsat = mxosat

  RETURN
END SUBROUTINE mrkAll

END MODULE
