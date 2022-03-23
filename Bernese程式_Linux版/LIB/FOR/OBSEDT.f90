MODULE s_OBSEDT
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE obsedt(iFil,opt,obsHead,nEdt,edtLst,iEdit,fildid, &
                    obstim,ifrLst,nAmb,ambLst)

! -------------------------------------------------------------------------
! Purpose:    Performs all editings for a observation file
!
! Author:     R. Dach
!
!
! Created:    20-Feb-2002
!
! Changes:    28-Feb-2002 RD+HB: Make add of ambiguity work
!             25-Apr-2002 HU: Format statement corrected
!             07-May-2002 HB: Bug fixed for sort order of ambiguities
!             22-Jul-2002 HB: Use modified t_obsHead
!             30-Jul-2002 HU: Use interface for alcerr
!             17-Feb-2003 LM: Use m_maxdim
!             23-Apr-2003 AJ: Nullify local pointers
!             13-Sep-2003 HU: Interface for defreq
!             08-Feb-2012 RD: Resolve "out-of-bounds" for updating amblst
!             20-Sep-2012 RD: Use M_BERN with ONLY
!             03-May-2013 PW: ambLst(i1:i2)%lstEdt splitted
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnerr, lfn001, fileNameLength
  USE m_maxdim, ONLY: maxamb
  USE d_gpsobs, ONLY: t_obsHead
  USE d_ambigu, ONLY: t_ambigu
  USE d_edit,   ONLY: t_edtRec
  USE d_satfrq, ONLY: wlgt
  USE p_satmrk, ONLY: t_satmrk_opt
  USE s_rdobsi
  USE s_opnfil
  USE s_alcerr
  USE f_tstflg
  USE s_mjdgps
  USE s_wtobsi
  USE s_clrflg
  USE s_opnerr
  USE s_setflg
  USE s_defreq
  USE s_sdambf
  USE s_gtflna
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  INTEGER(i4b)                :: iFil     ! Actual file number in list
  TYPE(t_satmrk_opt)          :: opt      ! SATMRK input options
  TYPE(t_obsHead)             :: obsHead  ! Observation file header information
  INTEGER(i4b)                :: nEdt     ! Number of edit requests
  TYPE(t_edtRec),           &
    DIMENSION(:),  POINTER    :: edtLst   ! Definition of edit requests
  INTEGER(i4b)                :: iEdit    ! Index in fildid (file edit) list

! output
  INTEGER(i4b),             &
    DIMENSION(:,:),POINTER :: fildid   ! Reports what edits done
  REAL(r8b),  DIMENSION(2) :: obstim   ! First/last epoch with obs. (MJD)
  INTEGER(i4b),             &
    DIMENSION(:,:),POINTER :: ifrLst   ! First/last epo. number per sat.
                                       !  ifrLst(1:2,iSat)
  INTEGER(i4b)             :: nAmb     ! Number of entries in ambLst
  TYPE(t_edtRec),           &
    DIMENSION(:),  POINTER :: ambLst   ! Mark requests for opt%minAmb

! Local parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER   :: srName = 'obsedt'

! Local Variables
! ---------------
  TYPE(t_ambigu),DIMENSION(:),ALLOCATABLE  :: hlpAmb
                                           ! Auxiliary ambigu-type for
                                           ! allocation of more memory
  CHARACTER(LEN=fileNameLength) :: scrFil
  CHARACTER(LEN=6)              :: mxnSat
  CHARACTER(LEN=1)              :: epoFLg  ! Epoch flag
  CHARACTER(LEN=1),              &
    DIMENSION(:,:), POINTER     :: obsFlg  ! Observation flag: obsFlg(iSat,iFrq)

  INTEGER(i4b),                  &
    DIMENSION(:),   POINTER     :: nrSat   ! Satellite numbers for obs.
  INTEGER(i4b),     DIMENSION(2):: iFrqs   ! Frequency list for rdobsi
  INTEGER(i4b)                  :: nSat    ! Number of sat. per epoch
  INTEGER(i4b),                  &
    DIMENSION(:,:), POINTER     :: ambFlg  ! Is an ambiguity to set?
                                           !   ambFlg(iSat,iFrq)
  INTEGER(i4b)                  :: iEpo
  INTEGER(i4b)                  :: iEpoch
  INTEGER(i4b)                  :: iEdt
  INTEGER(i4b)                  :: iSat,kSat
  INTEGER(i4b)                  :: iSatel
  INTEGER(i4b)                  :: iSatNw,iCluNw,iWlfNw
  INTEGER(i4b)                  :: iFrq
  INTEGER(i4b)                  :: iAmb,jAmb
  INTEGER(i4b)                  :: nWeek
  INTEGER(i4b)                  :: mxcSat,mxoSat
  INTEGER(i4b)                  :: lfnobs,lfnscr
  INTEGER(i4b)                  :: i1,i2,ii
  INTEGER(i4b)                  :: ios,irc

  REAL(r8b),        DIMENSION(2):: epoFrq
  REAL(r8b)                     :: obsEpo  ! Epoch of observation (MJD)
  REAL(r8b),                     &
    DIMENSION(:,:), POINTER     :: observ  ! Observation: observ(iSat,iFrq)
  REAL(r8b),        DIMENSION(2):: deltaT  ! Clock corrections for epoch
  REAL(r8b)                     :: gpsSec

  LOGICAL                       :: delAmb

  COMMON/MCMSAT/MXCSAT,MXNSAT

  NULLIFY(obsFlg)
  NULLIFY(nrSat)
  NULLIFY(ambFlg)
  NULLIFY(observ)

! Store (old) common maxsat
! -------------------------
  mxosat = mxcsat
  mxcsat = obsHead%nSatel

! Define logical file numbers
! ---------------------------
  lfnobs=lfn001
  lfnscr=lfn001+1

! Open observation file and scratch file
! --------------------------------------
  CALL opnfil(lfnobs,opt%filNam(2,iFil),'OLD','UNFORMATTED',' ',' ',ios)
  CALL opnerr(lfnerr,lfnobs,ios,opt%filNam(2,iFil),srName)

  CALL gtflna(1,'AUXFIL',scrFil,irc)
  CALL opnfil(lfnscr,scrFil,'UNKNOWN','UNFORMATTED',' ',' ',ios)
  CALL opnerr(lfnerr,lfnscr,ios,scrFil,srName)

! Initialize number of good and bad observations
! ----------------------------------------------
  DO ii = 1,2
    obsHead%sat(1:obsHead%nSatel)%numObs(ii) = 0
    obsHead%sat(1:obsHead%nSatel)%numMrk(ii) = 0
  ENDDO

! Initialize first and last observation number
! --------------------------------------------
  obstim(1) = 0.D0

  DEALLOCATE(ifrLst,stat=irc)

  ALLOCATE(ifrLst(2,obsHead%nSatel),stat=irc)
  CALL alcerr(irc,'ifrLst',(/2,obsHead%nSatel/),srName)

  ifrLst(1,:)=1000000
  ifrLst(2,:)=0

! Initialize ambiguity flags
! --------------------------
  ALLOCATE(ambFlg(obsHead%nSatel,2),stat=irc)
  CALL alcerr(irc,'ambFlg',(/obsHead%nSatel,2/),srName)

  ambFlg(:,:) = 0

! Allocate arrays for reading of the observation file
! ---------------------------------------------------
  iFrqs(:) = (/ 1,2 /)

  ALLOCATE(nrSat(obsHead%nSatel),stat=irc)
  CALL alcerr(irc,'nrSat' ,(/ obsHead%nSatel /), srName)

  ALLOCATE(observ(obsHead%nSatel,obsHead%nFreq), stat=irc)
  CALL alcerr(irc,'observ',(/ obsHead%nSatel,obsHead%nFreq /), srName)

  ALLOCATE(obsflg(obsHead%nSatel,obsHead%nFreq), stat=irc)
  CALL alcerr(irc,'obsflg',(/ obsHead%nSatel,obsHead%nFreq /), srName)

! Define satellite frequencies
! ----------------------------
  epofrq(1)=obsHead%timref
  epofrq(2)=obsHead%timref+(obsHead%nEpoch-1)*DBLE(obsHead%iDeltT)/86400.D0

  CALL defreq(epofrq,obsHead%nSatel,obsHead%sat(:)%numSat)


! Loop over all epochs
! --------------------
  DO iEpo = 1,obsHead%nEpoch

! Read observations
! -----------------
    CALL rdobsi(lfnobs,obsHead%iFrmat,obsHead%nFreq,iFrqs,  &
                obsEpo,deltaT,epoFlg,nSat,nrSat,obsflg,observ,irc)

! Check end of file
! -----------------
    IF (irc == 1) EXIT

! Compute epoch number
! --------------------
    iEpoch=IDNINT((obsEpo-obsHead%timref)*86400.D0/DBLE(obsHead%iDeltT)+1.D0)

! Update cycle slip flags
! -----------------------
    CALL sdambf(obsHead%nFreq,nSat,nrSat,obsflg,obsHead%nSatel,&
         obsHead%sat(:)%numSat,ambFlg)

! Loop over all editing requests
! ------------------------------
    iEdtLoop: DO iEdt = 1,nEdt

! Amiguities will be handled later
! --------------------------------
      IF (IABS(edtLst(iEdt)%lstEdt(7)) == 4) CYCLE iEdtLoop

! Check epoch
! -----------
      ! Mark/reset/delete
      IF (IABS(edtLst(iEdt)%lstEdt(7)) <= 2) THEN

        IF(edtLst(iEdt)%lstEdt(2) > iEpoch .OR. &
           edtLst(iEdt)%lstEdt(3) < iEpoch) CYCLE iEdtLoop

      ! Apply ambiguity
      ELSEIF (edtLst(iEdt)%lstEdt(7) == 3) THEN

        IF (edtLst(iEdt)%lstEdt(2) > iEpoch) CYCLE iEdtLoop

      ! Set a cycle slip
      ELSEIF (edtLst(iEdt)%lstEdt(7) == 5) THEN

        IF (edtLst(iEdt)%lstEdt(2) /= iEpoch) CYCLE iEdtLoop

      ! Remove a cycle slip
      ELSEIF (edtLst(iEdt)%lstEdt(7) == -5) THEN

        IF(edtLst(iEdt)%lstEdt(2) > iEpoch .OR. &
           edtLst(iEdt)%lstEdt(3) < iEpoch) CYCLE iEdtLoop

      ENDIF

! Find satellite in observations
! ------------------------------
      kSat = 0
      DO iSat = 1,nSat
        IF (nrSat(iSat) == edtLst(iEdt)%lstEdt(1)) kSat = iSat
      ENDDO
      IF (kSat == 0) CYCLE iEdtLoop

! Loop over all frequencies involved in request
! ---------------------------------------------
      DO iFrq=1,obsHead%nFreq

        IF (iFrq == 1 .AND. edtLst(iEdt)%lstEdt(4) == 2) CYCLE
        IF (iFrq == 2 .AND. edtLst(iEdt)%lstEdt(4) == 1) CYCLE

! Mark an observation
! -------------------
        IF (edtLst(iEdt)%lstEdt(7) == 1 .AND. observ(kSat,iFrq) /= 0.D0) THEN
          IF (.NOT. tstflg(obsflg(kSat,iFrq),0)) &
             filDid(1,iEdit) = filDid(1,iEdit) + 1
          CALL setflg(obsflg(kSat,iFrq),0)
        ENDIF

! Reset an observation mark
! -------------------------
        IF (edtLst(iEdt)%lstEdt(7) == -1 .AND. observ(kSat,iFrq) /= 0.D0) THEN
          IF (tstflg(obsflg(kSat,iFrq),0))  &
             filDid(2,iEdit) = filDid(2,iEdit) + 1
          CALL clrflg(obsflg(kSat,iFrq),0)
        ENDIF

! Remove an observation
! ---------------------
        IF (edtLst(iEdt)%lstEdt(7) == 2 .AND. observ(kSat,iFrq) /= 0.D0) THEN
          filDid(3,iEdit)=filDid(3,iEdit)+1
          observ(kSat,iFrq)=0.D0
        ENDIF

! Apply cycle slip (with correct wavelangth factor)
! -------------------------------------------------
        IF (edtLst(iEdt)%lstEdt(7) == 3 .AND. observ(kSat,iFrq) /= 0.D0) THEN

          DO iAmb=1,obsHead%numamb
            IF (obsHead%ambigu(iAmb)%ambSat == nrSat(kSat)) EXIT
          ENDDO

          observ(kSat,iFrq)=observ(kSat,iFrq)+       &
                            wlgt(iFrq,nrSat(kSat))*  &
                            edtLst(iEdt)%lstCyc/obsHead%ambigu(iAmb)%ambwlf(iFrq)
          filDid(4,iEdit)=filDid(4,iEdit)+1

        ENDIF

! Set/reset cycle slip flag
! -------------------------
        IF (IABS(edtLst(iEdt)%lstEdt(7)) == 5) THEN

! Find index in array "numsat" for satellite "kSat"
! -------------------------------------------------
          DO iSatel = 1,obsHead%nSatel
            IF(nrSat(kSat) == obsHead%sat(iSatel)%numSat) EXIT
          ENDDO

          IF (edtLst(iEdt)%lstEdt(7) == 5) THEN
            IF (ambFlg(iSatel,iFrq) == 0) &
              filDid(5,iEdit) = filDid(5,iEdit) + 1
            ambFlg(iSatel,iFrq) = 1
          ELSE
            IF (observ(kSat,iFrq) /= 0.D0) THEN
              IF (tstflg(obsflg(kSat,iFrq),1)) &
                filDid(6,iEdit) = filDid(6,iEdit) + 1
              CALL clrflg(obsflg(kSat,iFrq),1)
              ambFlg(iSatel,iFrq) = 0
            ENDIF
          ENDIF
        ENDIF

      ENDDO ! Frequency loop

! Next editing request
! --------------------
    ENDDO iEdtLoop


! Loop over all satellites to update info
! ---------------------------------------
    iSatNw = 0
    DO iSat = 1,nSat

! Find index in array "numsat" for satellite "isat"
! -------------------------------------------------
      DO iSatel = 1,obsHead%nSatel
        IF (nrSat(iSat) == obsHead%sat(iSatel)%numSat) EXIT
      ENDDO

! Update number of good and bad observations
! ------------------------------------------
      DO iFrq = 1,obsHead%nFreq

        IF (observ(iSat,iFrq) == 0.D0) CYCLE

        IF (tstflg(obsflg(iSat,iFrq),0)) THEN
          obsHead%sat(iSatel)%numMrk(iFrq) = obsHead%sat(iSatel)%numMrk(iFrq) + 1
        ELSE
          obsHead%sat(iSatel)%numObs(iFrq) = obsHead%sat(iSatel)%numObs(iFrq) + 1
        ENDIF

      ENDDO

! L1 or L2 or both frequencies still available ?
! ----------------------------------------------
      IF (observ(iSat,1) /= 0.D0 .OR. observ(iSat,obsHead%nFreq) /= 0.D0) THEN

! Set cycle slip flag if necessary
! --------------------------------
        DO iFrq = 1,obsHead%nFreq

          IF (ambFlg(iSatel,iFrq) == 1 .AND. observ(iSat,iFrq) /= 0.D0) THEN

            CALL setflg(obsflg(iSat,iFrq),1)
            ambFlg(iSatel,iFrq)=0

          ENDIF

        ENDDO

! Copy observations only if at least one frequency available
        iSatNw = iSatNw + 1

        nrSat(iSatNw) = nrSat(iSat)

        observ(iSatNw,1:obsHead%nFreq) = observ(iSat,1:obsHead%nFreq)
        obsflg(iSatNw,1:obsHead%nFreq) = obsflg(iSat,1:obsHead%nFreq)

! Update first and last observation number
! ----------------------------------------
        IF (ifrLst(1,iSatel) > iEpoch) ifrLst(1,iSatel)=iEpoch
        IF (ifrLst(2,iSatel) < iEpoch) ifrLst(2,iSatel)=iEpoch

      ENDIF

    ENDDO  ! Loop all satellites of the epoch

    nSat=iSatNw

! No satellites for this epoch
! ----------------------------
    IF (nSat > 0) THEN

! Save first and last epoch
! -------------------------
      IF (obstim(1) == 0.D0) obstim(1)=obsEpo
      obstim(2)=obsEpo

! Write observations
! ------------------
      CALL wtobsi(lfnscr,obsHead%iFrmat,obsHead%nFreq,obsEpo,deltaT,epoFlg, &
                  nSat,nrSat,obsflg,observ)

    ENDIF

! Next epoch
! ----------
  ENDDO

! Close observation and scratch file
! ----------------------------------
  CLOSE(lfnobs)
  CLOSE(lfnscr)

!
! PART 2:
! ======
!
! Add/remove ambiguties according to requests
! -------------------------------------------
  jEdtLoop: DO iEdt = 1,nEdt


! Wrong file , wrong editing type (not ambiguities)
! -------------------------------------------------
    IF (IABS(edtLst(iEdt)%lstEdt(7)) /= 4) CYCLE


! Add ambiguity
! -------------
    IF (edtLst(iEdt)%lstEdt(7) == 4) THEN

! Check if ambigutiy already set
! ------------------------------
      DO iAmb = 1,obsHead%numAmb

        IF (obsHead%ambigu(iAmb)%ambSat == edtLst(iEdt)%lstEdt(1) .AND. &
            obsHead%ambigu(iAmb)%ambIep == edtLst(iEdt)%lstEdt(2)) THEN
           WRITE(lfnerr,'(/,A,/,16X,A,I6,/,16X,A,I6,/,16X,A,A,/)') &
                 ' ### SR OBSEDT: Ambiguity already set up',       &
                 'Satellite       :',edtLst(iEdt)%lstEdt(1),       &
                 'Epoch           :',edtLst(iEdt)%lstEdt(2),       &
                 'Header file name: ',TRIM(opt%filNam(1,iFil))
          CYCLE jEdtLoop
        ENDIF

      ENDDO

! Check dimensions
! ----------------
      IF (obsHead%numAmb+1 > maxAmb) THEN
        WRITE(lfnerr,'(/,A,I3,/,16X,A,I3,/,16X,A,A,/,16X,A,/)')           &
              ' ### SR OBSEDT: Too many ambiguities:',obsHead%numAmb,     &
              'Maximum # of ambig. :',maxAmb,                             &
              'Header file name    : ',TRIM(opt%filNam(1,iFil)),          &
              'New ambiguity could not be added !'
        EXIT jEdtLoop
      ELSEIF (obsHead%numAmb+1 > size(obsHead%ambigu)) THEN
        ALLOCATE(hlpAmb(size(obsHead%ambigu)),stat=irc)
        CALL alcerr(irc,'hlpAmb',(/size(obsHead%ambigu)/),srName)
        hlpAmb(1:size(obsHead%ambigu)) = obsHead%ambigu(1:size(obsHead%ambigu))
        DEALLOCATE(obsHead%ambigu)
        ALLOCATE(obsHead%ambigu(size(hlpAmb)+10),stat=irc)
        CALL alcerr(irc,'obsHead%ambigu',(/size(hlpAmb)+10/),srName)
        obsHead%ambigu(1:size(hlpAmb)) = hlpAmb(1:size(hlpAmb))
        DEALLOCATE(hlpAmb)
      ENDIF

! Copy old ambiguities to keep sort order
! ---------------------------------------
      obsHead%ambigu(obsHead%numAmb+1)%ambCls(:) = 0

      DO iAmb = obsHead%numAmb,1,-1

        IF (obsHead%ambigu(iAmb)%ambIep >= edtLst(iEdt)%lstEdt(2).OR.&
             obsHead%ambigu(iAmb)%ambSat /= edtLst(iEdt)%lstEdt(1)) THEN
          obsHead%ambigu(iAmb+1)%ambSat = obsHead%ambigu(iAmb)%ambSat
          obsHead%ambigu(iAmb+1)%ambIep = obsHead%ambigu(iAmb)%ambIep

          obsHead%ambigu(iAmb+1)%ambWlf(:) = obsHead%ambigu(iAmb)%ambWlf(:)
          obsHead%ambigu(iAmb+1)%ambCls(:) = obsHead%ambigu(iAmb)%ambCls(:)
          obsHead%ambigu(iAmb+1)%ambigu(:) = obsHead%ambigu(iAmb)%ambigu(:)

          CYCLE

        ENDIF

! Add the new ambiguity to the list; it needs a new cluster number
! ----------------------------------------------------------------
        obsHead%ambigu(iAmb+1)%ambSat = edtLst(iEdt)%lstEdt(1)
        obsHead%ambigu(iAmb+1)%ambIep = edtLst(iEdt)%lstEdt(2)

        DO iFrq = 1,(obsHead%nFreq-1) * 2 + 1

          iCluNw = 0
          iWlfNw = 0

          DO jAmb = 1,obsHead%numAmb+1

            IF (obsHead%ambigu(jAmb)%ambCls(iFrq) > iCluNw) &
                 iCluNw = obsHead%ambigu(jAmb)%ambCls(iFrq)

            IF (iFrq < 3 .AND. jAmb /= iAmb+1 .AND. &
                 obsHead%ambigu(jAmb)%ambSat == obsHead%ambigu(iAmb+1)%ambSat) &
                 iWlfNw = obsHead%ambigu(jAmb)%ambWlf(iFrq)

          ENDDO

          IF (iFrq < 3 .AND. iWlfNw == 0) THEN
            iWlfNw = obsHead%ambigu(1)%ambWlf(iFrq)

            WRITE(lfnerr,'(/,A,3(/,16X,A,I6),/,16X,2A,/,16X,A,I6,/)')        &
                 ' ### SR OBSEDT: Unknown wavelength factor for a new ambiguity', &
                                 'Satellite:         ',obsHead%ambigu(iAmb+1)%ambSat,      &
                                 'Epoch:             ',obsHead%ambigu(iAmb+1)%ambIep,      &
                                 'Frequency:         ',iFrq,                      &
                                 'File name:         ',TRIM(opt%filNam(1,iFil)),  &
                                 'Assumed wl factor: ',iWlfNw
          ENDIF

          obsHead%ambigu(iAmb+1)%ambCls(iFrq) = iCluNw + 1
          obsHead%ambigu(iAmb+1)%ambigu(iFrq) = 0.D0
          IF (iFrq < 3) obsHead%ambigu(iAmb+1)%ambWlf(iFrq) = iWlfNw

        ENDDO

        EXIT
      ENDDO

      filDid(7,iEdit) = filDid(7,iEdit) + 1
      obsHead%numAmb = obsHead%numAmb + 1


! Remove ambiguity (leave ambiguities with epoch 1)
! -------------------------------------------------
    ELSE

      jAmb = 0

      DO iAmb = 1,obsHead%numAmb

        IF (obsHead%ambigu(iAmb)%ambSat == edtLst(iEdt)%lstEdt(1) .AND. &
            obsHead%ambigu(iAmb)%ambIep >= edtLst(iEdt)%lstEdt(2) .AND. &
            obsHead%ambigu(iAmb)%ambIep <= edtLst(iEdt)%lstEdt(3) .AND. &
            obsHead%ambigu(iAmb)%ambIep /= 1) THEN
          filDid(8,iEdit) = filDid(8,iEdit) + 1
          CYCLE
        ENDIF

        jAmb = jAmb + 1

        obsHead%ambigu(jAmb)%ambSat = obsHead%ambigu(iAmb)%ambSat
        obsHead%ambigu(jAmb)%ambIep = obsHead%ambigu(iAmb)%ambIep

        obsHead%ambigu(jAmb)%ambWlf(:) = obsHead%ambigu(iAmb)%ambWlf(:)
        obsHead%ambigu(jAmb)%ambCls(:) = obsHead%ambigu(iAmb)%ambCls(:)
        obsHead%ambigu(jAmb)%ambigu(:) = obsHead%ambigu(iAmb)%ambigu(:)

      ENDDO

      obsHead%numAmb = jAmb

    ENDIF

! Next edit request
! -----------------
  ENDDO jEdtLoop

!
! Part 3:
! =======
!
! Copy observations back and count observations per ambiguity
! -----------------------------------------------------------
  DEALLOCATE(ambLst,stat=irc)

  ALLOCATE(ambLst(obsHead%nFreq*obsHead%numAmb),stat=irc)
  CALL alcerr(irc,'ambLst',(/ obsHead%nFreq*obsHead%numAmb /),srName)

  DO iAmb = 1, obsHead%numAmb

    i2 = obsHead%nFreq*iAmb
    i1 = i2-obsHead%nFreq+1

    ambLst(i1:i2)%lstEdt(1) = obsHead%ambigu(iAmb)%ambSat
    ambLst(i1:i2)%lstEdt(2) = obsHead%ambigu(iAmb)%ambIep
    ambLst(i1:i2)%lstEdt(3) = obsHead%nEpoch
    DO jAmb = iAmb+1,obsHead%numAmb
      IF (obsHead%ambigu(iAmb)%ambSat == obsHead%ambigu(jAmb)%ambSat .AND. &
          obsHead%ambigu(iAmb)%ambIep <  obsHead%ambigu(jAmb)%ambIep) THEN
        ambLst(i1:i2)%lstEdt(3) = obsHead%ambigu(jAmb)%ambIep-1
        EXIT
      ENDIF
    ENDDO
    ambLst(i2)%lstEdt(4) = 2
    ambLst(i1)%lstEdt(4) = 1
    ambLst(i1:i2)%lstEdt(5) = 0         ! Count valid obs.
    ambLst(i1:i2)%lstEdt(6) = 0         ! Count marked and valid obs.
    ambLst(i1:i2)%lstEdt(7) = 1
    ambLst(i1:i2)%lstCyc    = 0d0
  ENDDO

! Reopen observation and scratch file
! -----------------------------------
  CALL opnfil(lfnobs,opt%filNam(2,iFil),'OLD','UNFORMATTED',' ',' ',irc)
  CALL opnerr(lfnerr,lfnobs,irc,opt%filNam(2,iFil),srName)

  CALL opnfil(lfnscr,scrfil,'OLD','UNFORMATTED',' ',' ',irc)
  CALL opnerr(lfnerr,lfnscr,irc,scrfil,srName)

! Copy all observations from scratch file back to observation file
! ----------------------------------------------------------------
  DO iEpo=1,obsHead%nEpoch

! Read observations
! -----------------
    CALL rdobsi(lfnscr,obsHead%iFrmat,obsHead%nFreq,iFrqs,  &
                obsEpo,deltaT,epoFlg,nSat,nrSat,obsflg,observ,irc)

! Check end of file
! -----------------
    IF(irc == 1) EXIT

! Write observations
! ------------------
    CALL wtobsi(lfnobs,obsHead%iFrmat,obsHead%nFreq,obsEpo,deltaT,epoFlg, &
                nSat,nrSat,obsflg,observ)

! Ambiguity statistic for phase files only
! ----------------------------------------
    IF (obsHead%meaTyp /= 1) CYCLE

! Compute epoch number
! --------------------
    iEpoch=IDNINT((obsEpo-obsHead%timref)*86400.D0/DBLE(obsHead%iDeltT)+1.D0)

! Count number of valid obervations
! ---------------------------------
    DO iSat = 1,nSat

      DO iAmb = 1,SIZE(ambLst)
        IF (nrSat(iSat) /= ambLst(iAmb)%lstEdt(1)) CYCLE

        IF (iEpoch < ambLst(iAmb)%lstEdt(2) .OR. &
            iEpoch > ambLst(iAmb)%lstEdt(3)) CYCLE

        IF (observ(iSat,ambLst(iAmb)%lstEdt(4)) == 0d0) CYCLE

        ambLst(iAmb)%lstEdt(6) = ambLst(iAmb)%lstEdt(6) + 1

        IF (opt%minAmb == 0) CYCLE

        IF (opt%iSampl /= 0) THEN
          CALL mjdgps(obsepo,gpsSec,nWeek)
          IF (DABS(gpsSec-DNINT(gpsSec/opt%iSampl)*opt%iSampl) > 1d0) CYCLE
        ENDIF

        IF (tstflg(obsflg(iSat,ambLst(iAmb)%lstEdt(4)),0)) CYCLE

        ambLst(iAmb)%lstEdt(5) = ambLst(iAmb)%lstEdt(5) + 1
      ENDDO

    ENDDO

  ENDDO

! Close observation and scratch file
! ----------------------------------
  CLOSE(lfnobs)
  CLOSE(lfnscr,STATUS='DELETE')

! Check for markes because of opt%minAmb
! --------------------------------------
  nAmb = 0

  iAmb = 0
  DO WHILE (iAmb < SIZE(ambLst))
    iAmb = iAmb + 1

! All observations for an ambiguity are deleted -> remove ambig.
! --------------------------------------------------------------
    IF (ambLst(iAmb)%lstEdt(6) == 0 .AND. &
        ambLst(iAmb)%lstEdt(4) == 1) THEN

      delAmb = (obsHead%nFreq == 1)
      IF ( iAmb < SIZE(ambLst) ) THEN
        delAmb = delAmb .OR. (ambLst(iAmb+1)%lstEdt(6) == 0)
      ENDIF

      IF ( delAmb ) THEN
        nAmb = nAmb + 1
        IF (nAmb /= iAmb) ambLst(nAmb) = ambLst(iAmb)

        ambLst(nAmb)%lstEdt(6) = iFil
        ambLst(nAmb)%lstEdt(7) = -4   ! Delete ambig.

        iAmb = iAmb + (obsHead%nFreq+1)
        CYCLE
      ENDIF
    ENDIF

! Mark obs. to have a minimum number of obs. per ambiguity
! --------------------------------------------------------
    IF (opt%minAmb == 0) CYCLE

    IF (ambLst(iAmb)%lstEdt(5) >= opt%minAmb) CYCLE

    nAmb = nAmb + 1
    IF (nAmb /= iAmb) ambLst(nAmb) = ambLst(iAmb)

    ambLst(nAmb)%lstEdt(6) = iFil
    ambLst(nAmb)%lstEdt(7) = 1     ! Mark obs.
  ENDDO


! Deallocate local arrays
! -----------------------
  DEALLOCATE(ambFlg, stat=irc)
  DEALLOCATE(nrSat,  stat=irc)
  DEALLOCATE(observ, stat=irc)
  DEALLOCATE(obsflg, stat=irc)

! restore (old) common maxsat
! ---------------------------
  mxcsat = mxosat

  RETURN
END SUBROUTINE obsedt

END MODULE
