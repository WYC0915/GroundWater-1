MODULE s_RDNUTM
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdnutm(filnam,nutat)

! -------------------------------------------------------------------------
! Purpose:    Read nutation model file  (new format)
!
! Author:     C. Urschl
!
! Created:    16-Dec-2002
! Last mod.:  16-Jun-2003
!
! Changes:    15-May-2003 HU: Deallocate arrays first
!             21-May-2003 RD: Make the deallocation safe
!             16-Jun-2003 HU: Read corrections to precession
!
! SR used:    opnfil, opnerr, alcerr, exitrc
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_nutmod, ONLY: t_nutat, maxarg, maxcoeff

  USE s_opnfil
  USE s_alcerr
  USE s_opnerr
  USE s_exitrc
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  CHARACTER(LEN=*)  :: filnam  ! File name

! output:
  TYPE(t_nutat)     :: nutat   ! Nutation model parameter

! List of functions
! -----------------

! Local types
! -----------

! Local parameters
! ----------------

! Local variables
! ---------------
  INTEGER(i4b)                   :: iper
  INTEGER(i4b)                   :: irec
  INTEGER(i4b)                   :: ios
  INTEGER(i4b)                   :: ii, jj

  CHARACTER(LEN=255)             :: line


! Open nutation model file
! ------------------------
  nutat%filnut = filnam
  CALL opnfil(lfnloc,nutat%filnut,'OLD','FORMATTED','READONLY',' ',ios)
  CALL opnerr(lfnerr,lfnloc,ios,nutat%filnut,'RDNUTM')

! Count number of terms (28+maxarg header lines)
! ---------------------
  DO ii = 1, 28+maxarg
    READ(lfnloc,*)
  ENDDO
  nutat%nnut=0
  DO
    READ(lfnloc,"(A)",IOSTAT=ios) line
    IF (ios > 0) THEN
      WRITE(lfnerr,'(/,A,/,A,A,/)')                                   &
          ' *** SR RDNUTM: Error reading file while counting lines.',  &
          '                File name: ', nutat%filnut
      CALL exitrc(2)
    ENDIF
    IF (ios < 0) EXIT
    IF (line == ' ') CYCLE
    nutat%nnut=nutat%nnut+1
  ENDDO
  REWIND(lfnloc)

! Format version (not used)
! --------------
  nutat%ivers = 1

! Allocate some arrays
! --------------------
  IF (ASSOCIATED(nutat%nutmlt)) &
    DEALLOCATE(nutat%nutmlt,STAT=ios)
  ALLOCATE(nutat%nutmlt(maxarg,nutat%nnut),STAT=ios)
  CALL alcerr(ios,'nutat%nutmlt',(/maxarg,nutat%nnut/),'rdnutm')

  IF (ASSOCIATED(nutat%nutper)) &
    DEALLOCATE(nutat%nutper,STAT=ios)
  ALLOCATE(nutat%nutper(nutat%nnut),STAT=ios)
  CALL alcerr(ios,'nutat%nutper',(/nutat%nnut/),'rdnutm')

  IF (ASSOCIATED(nutat%nutcoe)) &
    DEALLOCATE(nutat%nutcoe,STAT=ios)
  ALLOCATE(nutat%nutcoe(maxcoeff,nutat%nnut),STAT=ios)
  CALL alcerr(ios,'nutat%nutmlt',(/maxcoeff,nutat%nnut/),'rdnutm')

! Read title lines and nutation model name
! ----------------------------------------
  READ(lfnloc,'(A80,///,21X,A16,//)',IOSTAT=ios) nutat%title, nutat%nutnam

  IF (ios < 0) THEN
    WRITE(lfnerr,'(/,A,/,A,A,/)')                 &
      ' *** SR RDNUTM: Unexpected end of file (1).',  &
      '                File name: ', nutat%filnut
    CALL exitrc(2)
  ENDIF

! Read precession offsets and rate corrections
! --------------------------------------------
  READ(lfnloc,'(//2(/,30X,F14.8),2(/,30X,F12.6),8(/))',IOSTAT=ios) &
    nutat%nutpre(1),nutat%nutpre(2),nutat%nutpre(3),nutat%nutpre(4)
  IF (ios < 0) THEN
    WRITE(lfnerr,'(/,A,/,A,A,/)')                 &
      ' *** SR RDNUTM: Unexpected end of file (2).',  &
      '                File name: ', nutat%filnut
    CALL exitrc(2)
  ENDIF

! Read coefficients of fundamental arguments
! ------------------------------------------
  DO irec = 1, 5

    READ(lfnloc,'(A)',IOSTAT=ios) line
    IF (ios < 0) THEN
      WRITE(lfnerr,'(/,A,/,A,A,/)')                 &
        ' *** SR RDNUTM: Unexpected end of file (3).',  &
        '                File name: ', nutat%filnut
      CALL exitrc(2)
    ELSEIF (ios > 0) THEN
      WRITE(lfnerr,'(/,A,/,A,A,/)')                 &
        ' *** SR RDNUTM: Error reading file (1).',      &
        '                File name: ', nutat%filnut
      CALL exitrc(2)
    ELSE
      READ(line(3:),*,IOSTAT=ios) (nutat%nutfar(ii,irec),ii=1,6)
      IF (ios /= 0) THEN
        WRITE(lfnerr,'(/,A,/,A,A,/)')                 &
          ' *** SR RDNUTM: Error reading file (2).',      &
          '                File name: ', nutat%filnut
        CALL exitrc(2)
      ENDIF
    ENDIF

  ENDDO

  READ(lfnloc,'( )')

  DO irec=6, maxarg
    READ(lfnloc,'(A)',IOSTAT=ios) line
    IF (ios < 0) THEN
      WRITE(lfnerr,'(/,A,/,A,A,/)')                 &
        ' *** SR RDNUTM: Unexpected end of file (4).',  &
        '                File name: ', nutat%filnut
      CALL exitrc(2)
    ELSEIF (ios > 0) THEN
      WRITE(lfnerr,'(/,A,/,A,A,/)')                 &
        ' *** SR RDNUTM: Error reading file (3).',      &
        '                File name: ', nutat%filnut
      CALL exitrc(2)
    ELSE
      READ(line(3:),*,IOSTAT=ios) (nutat%nutfar(ii,irec),ii=1,6)
      IF (ios /= 0) THEN
        WRITE(lfnerr,'(/,A,/,A,A,/)')                 &
          ' *** SR RDNUTM: Error reading file (4).',      &
          '                File name: ', nutat%filnut
        CALL exitrc(2)
      ENDIF
    ENDIF
  ENDDO
  READ(lfnloc,'(/////)')

! Read nutation terms: multipliers, period, and coefficients
! ----------------------------------------------------------
  DO iper = 1, nutat%nnut

    READ(lfnloc,*,IOSTAT=ios) (nutat%nutmlt(ii,iper),ii=1,maxarg), &
        nutat%nutper(iper), (nutat%nutcoe(jj,iper),jj=1,maxcoeff)
    IF (ios > 0) THEN
       WRITE(lfnerr,'(/,A,/,A,A,/)')                  &
          ' *** SR RDNUTM: Error reading file (5).',      &
          '                File name: ', nutat%filnut
       CALL exitrc(2)
    ENDIF

  ENDDO

  CLOSE(lfnloc)

  RETURN

END SUBROUTINE rdnutm

END MODULE
