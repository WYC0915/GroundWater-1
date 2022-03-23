MODULE s_RXOFRQ
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rxofrq(erropt,rnxfile,meatyp,satsta,name,rnxfrq,epoch,irCode)

! -------------------------------------------------------------------------
! Purpose:    Verify the frequency value from the RINEX file using the
!             frequency information file. Do special action in case of
!             inconsistency.
!
! Author:     C. Urschl
!
! Created:    23-Oct-2003
!
! Changes:    09-Nov-2004 CU: Correct sequence to update FRQ file
!             03-Jul-2009 SL: if statement corrected (empty lines removed)
!             20-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnerr, &
                      fileNameLength, staNameLength
  USE d_frqfil, ONLY: t_frqinfo, t_freq, init_frqfil
  USE p_rxobv3, ONLY: t_rxobv3_err

  USE s_alcerr
  USE s_wtfreq
  USE s_gtfreq
  USE s_exitrc
  USE s_rdfreq
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  TYPE(t_rxobv3_err)            :: erropt ! Action in case of inconsistency
                                          ! 1 = WARNING
                                          ! 2 = SKIP
                                          ! 3 = ERROR
                                          ! 4 = UPDATE
  CHARACTER(LEN=fileNameLength) :: rnxfile! Name of RINEX file
  INTEGER(i4b)                  :: meatyp ! Measurement type
                                          ! 1 = PHASE
                                          ! 2 = CODE
                                          ! 3 = RANGE
  CHARACTER(LEN=staNameLength)  :: satsta ! Satellite channel or station name
  CHARACTER(LEN=2)              :: name   ! Name of frequency
  REAL(r8b)                     :: rnxfrq ! Frequency value from RINEX file
  REAL(r8b)                     :: epoch  ! Epoch
  INTEGER(i4b)                  :: irCode ! 2 = Skip file

! Local Parameters
! ----------------
  CHARACTER(LEN=5), DIMENSION(3), PARAMETER :: typstrg = &
    (/'PHASE','CODE ','RANGE'/)
  CHARACTER(LEN=4), DIMENSION(3), PARAMETER :: unit    = &
    (/'1/s ','1/s ','nm  '/)

  LOGICAL, SAVE                             :: first   = .TRUE.
  LOGICAL, SAVE                             :: update  = .FALSE.

! Local Variables
! ---------------
  TYPE(t_frqinfo), SAVE                     :: frqinfo
  TYPE(t_freq), DIMENSION(:), ALLOCATABLE   :: hlpfreq
  TYPE(t_freq)                              :: idx1, idx2

  REAL(r8b)                                 :: reqfreq

  INTEGER(i4b)                              :: ios
  INTEGER(i4b)                              :: iOrd
  INTEGER(i4b)                              :: iline, nline

  LOGICAL                                   :: sorted

  irCode = 0

! Update frequency information file, deallocate arrays
! ----------------------------------------------------
  IF (rnxfile == '') THEN

    IF (first) THEN
      WRITE(lfnerr,'(3(A,/))')                                                 &
      ' *** SR RXOFRQ: Dear programmer! This subroutine is called for the ',   &
      '                first time. The RINEX file name is set to blank. That ',&
      '                means: deallocate arrays. But this is not possible at', &
      '                the first call.'
    ENDIF

    IF (update) CALL wtfreq(frqinfo)
    IF (ASSOCIATED(frqinfo%freq))     DEALLOCATE(frqinfo%freq,stat=ios)
    IF (ASSOCIATED(frqinfo%footline)) DEALLOCATE(frqinfo%footline,stat=ios)

    RETURN

  ENDIF

! Read frequency info from freqency info file into array
! ------------------------------------------------------
  IF (first) THEN
    first = .FALSE.
    CALL init_frqfil(frqinfo)
    CALL rdfreq(frqinfo)
  ENDIF

! Get requested frequency from array
! ----------------------------------
  CALL gtfreq(frqinfo%freq,meatyp,satsta,name,epoch,reqfreq)

! Check frequency
! ---------------
  IF (reqfreq == 0) THEN     ! frequency not found in freq.file

    IF (erropt%verFrq == 1) &         ! WARNING
      WRITE(lfnerr,'(2(A,/),2(A,A,/))')                                    &
      ' ### SR RXOFRQ: The frequency information from the RINEX file ',    &
      '                cannot be found in the frequency information file.',&
      '                RINEX file:                 ',rnxfile,              &
      '                Frequency information file: ',frqinfo%frqfile

    IF (erropt%verFrq == 2) THEN      ! SKIP rinex file
      WRITE(lfnerr,'(2(A,/),2(A,A,/),A,/)')                                &
      ' ### SR RXOFRQ: The frequency information from the RINEX file ',    &
      '                cannot be found in the frequency information file.',&
      '                RINEX file:                 ',rnxfile,              &
      '                Frequency information file: ',frqinfo%frqfile,      &
      '                Bernese observation file will not be written!'
      irCode = 2
    ENDIF

    IF (erropt%verFrq == 3) THEN      ! ERROR
      WRITE(lfnerr,'(2(A,/),2(A,A,/),A,/)')                                &
      ' *** SR RXOFRQ: The frequency information from the RINEX file ',    &
      '                cannot be found in the frequency information file.',&
      '                RINEX file:                 ',rnxfile,              &
      '                Frequency information file: ',frqinfo%frqfile,      &
      '                Program stopped!'
      CALL exitrc(2)
    ENDIF

    IF (erropt%verFrq == 4) THEN      ! UPDATE frequency information array
      update = .TRUE.
      nline = SIZE(frqinfo%freq)

      ALLOCATE(hlpfreq(nline+1),stat=ios)
      CALL alcerr(ios,"hlpfreq",(/nline+1/),'RXOFRQ')

      DO iline = 1, nline             ! Copy array
        hlpfreq(iline)%typstrg   = frqinfo%freq(iline)%typstrg
        hlpfreq(iline)%satsta    = frqinfo%freq(iline)%satsta
        hlpfreq(iline)%name      = frqinfo%freq(iline)%name
        hlpfreq(iline)%value     = frqinfo%freq(iline)%value
        hlpfreq(iline)%unit      = frqinfo%freq(iline)%unit
        hlpfreq(iline)%window(:) = frqinfo%freq(iline)%window(:)
        hlpfreq(iline)%remark    = frqinfo%freq(iline)%remark
      ENDDO

    ! Update array
      hlpfreq(nline+1)%typstrg   = typstrg(meatyp)
      hlpfreq(nline+1)%satsta    = satsta
      hlpfreq(nline+1)%name      = name
      hlpfreq(nline+1)%value     = rnxfrq
      hlpfreq(nline+1)%unit      = unit(meatyp)
      hlpfreq(nline+1)%window(1) = NINT(epoch)
      hlpfreq(nline+1)%window(2) = 1d20
      hlpfreq(nline+1)%remark    = 'RXOBV3 update'

      sorted = .FALSE.                ! Sort array
      DO WHILE (.NOT. sorted)
        sorted = .TRUE.
        DO iOrd = 1, nline
          idx1 = hlpfreq(iOrd)
          idx2 = hlpfreq(iOrd+1)
          IF ( (idx1%satsta(1:4) > idx2%satsta(1:4)) .OR. &
               (idx1%satsta     == idx2%satsta      .AND. &
                idx1%name(2:2)   > idx2%name(2:2))   .OR. &
               (idx1%satsta     == idx2%satsta      .AND. &
                idx1%name       == idx2%name        .AND. &
                idx1%window(1)   > idx2%window(1)) ) THEN

            hlpfreq(iOrd)   = idx2
            hlpfreq(iOrd+1) = idx1
            sorted          = .FALSE.

          ENDIF
        ENDDO
      ENDDO

      DO iline = 1, nline             ! Set window(2) if new entry
        idx1 = hlpfreq(iline)
        idx2 = hlpfreq(iline+1)
        IF ( (idx1%typstrg  == idx2%typstrg) .AND. &
             (idx1%satsta   == idx2%satsta)  .AND. &
             (idx1%name     == idx2%name)    .AND. &
             (idx1%window(2) > idx2%window(1)) ) THEN
          hlpfreq(iline)%window(2) = idx2%window(1) - 1d0/86400d0
        ENDIF
      ENDDO

      IF(ASSOCIATED(frqinfo%freq)) DEALLOCATE(frqinfo%freq,stat=ios)
      ALLOCATE(frqinfo%freq(nline+1),stat=ios)
      CALL alcerr(ios,"frqinfo%freq",(/nline+1/),'RXOFRQ')

      DO iline = 1, nline+1           ! Copy updated array
        frqinfo%freq(iline)%typstrg   = hlpfreq(iline)%typstrg
        frqinfo%freq(iline)%satsta    = hlpfreq(iline)%satsta
        frqinfo%freq(iline)%name      = hlpfreq(iline)%name
        frqinfo%freq(iline)%value     = hlpfreq(iline)%value
        frqinfo%freq(iline)%unit      = hlpfreq(iline)%unit
        frqinfo%freq(iline)%window(:) = hlpfreq(iline)%window(:)
        frqinfo%freq(iline)%remark    = hlpfreq(iline)%remark
      ENDDO

      DEALLOCATE(hlpfreq,stat=ios)

      WRITE(lfnerr,'(2(A,/),2(A,A,/),A,/)')                                &
      ' ### SR RXOFRQ: The frequency information from the RINEX file ',    &
      '                cannot be found in the frequency information file.',&
      '                RINEX file:                 ',rnxfile,              &
      '                Frequency information file: ',frqinfo%frqfile,      &
      '                Frequency information file updated!'

    ENDIF

  ELSEIF(reqfreq /= rnxfrq) THEN    ! frequency found in freq. info file not
                                    ! equal with rinex header
                                    ! WARNING, take frequency from freq. file
    WRITE(lfnerr,'(2(A,/),2(A,A,/,A,D13.4,/))')                             &
    ' ### SR RXOFRQ: The frequency value from the RINEX file is not equal', &
    '                to the value found in the frequency information file.',&
    '                RINEX file:                 ',rnxfile,                 &
    '                    Frequency value:        ',rnxfrq,                  &
    '                Frequency information file: ',frqinfo%frqfile,         &
    '                    Frequency value:        ',reqfreq

  ENDIF


  RETURN

END SUBROUTINE rxofrq

END MODULE
