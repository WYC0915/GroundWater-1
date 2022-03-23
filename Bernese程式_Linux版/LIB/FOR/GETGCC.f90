MODULE s_GETGCC
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE getgcc(filgcc,epoch,gcc,siggcc,irCode)

! -------------------------------------------------------------------------
! Purpose:    Get GCC values from file
!
! Remarks:
!
! Author:     U. Hugentobler
!
! Created:    25-Mar-2002
!
! Changes:    20-May-2010 MF: Initialize gccdat
!             24-Oct-2012 DT: Initialize gccdat only if not yet associated;
!                             m_bern with ONLY
!
! SR used:    rdgccrd
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,    ONLY: i4b, r8b, lfnerr
  USE d_gccdat,  ONLY: t_gccdat, init_gccdat

  USE s_rdgccrd
  USE s_exitrc
  IMPLICIT NONE
!
! Variables in parameter list
! ---------------------------
! IN:
  CHARACTER(LEN=*)       :: filgcc    ! File name, blank: use GTFLNA mechanism
                                      ! with keyword GEOCENTR
  REAL(r8b)              :: epoch     ! Requested epoch (MJD)
! OUT:
  REAL(r8b),DIMENSION(3) :: gcc       ! Geocenter coordinates (X,Y,Z)
  REAL(r8b),DIMENSION(3) :: siggcc    ! Sigma of geocenter coordinates
  INTEGER(i4b)           :: irCode    ! Return code of this SR
                                      !  =2: No file available
! Local Variables
! ---------------
  INTEGER(i4b)           :: irc
  INTEGER(i4b)           :: irec,irec1

  REAL(r8b)              :: delta=0.5D0   ! extend window boundaries (days)
  REAL(r8b)              :: dt,dt1

  TYPE(t_gccdat),SAVE    :: gccdat

  irCode=0


! Initialize some variables
! -------------------------
  IF ( .NOT. ASSOCIATED(gccdat%gccrec) ) THEN
    CALL init_gccdat(gccdat)
  END IF

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
! Read file
! =========
  CALL rdgccrd(filgcc,gccdat,irc)
  IF (irc == 2) THEN
    gcc   =0D0
    siggcc=0D0
    irCode=2
    RETURN
  ENDIF

  dt=HUGE(1D0)
  irec1=0

  DO irec=1,gccdat%nrec
! outside time interval incl. delta-limit
    IF (epoch < gccdat%gccrec(irec)%timint(1)-delta .OR. &
        epoch > gccdat%gccrec(irec)%timint(2)+delta) CYCLE

! inside time interval
    IF (epoch >= gccdat%gccrec(irec)%timint(1) .AND. &
        epoch <= gccdat%gccrec(irec)%timint(2)) THEN
      irec1=irec
      EXIT
    ENDIF

! inside time interval incl. delta-limit
    IF (epoch < gccdat%gccrec(irec)%timint(1)) THEN
      dt1 = gccdat%gccrec(irec)%timint(1)-epoch
      IF (dt1 < dt) THEN
        dt    = dt1
        irec1 = irec
      ENDIF
    ENDIF
    IF (epoch > gccdat%gccrec(irec)%timint(2)) THEN
      dt1 = epoch - gccdat%gccrec(irec)%timint(2)
      IF (dt1 < dt) THEN
        dt    = dt1
        irec1 = irec
      ENDIF
    ENDIF
  ENDDO

  IF (irec1 == 0) THEN
    WRITE(lfnerr,"(/,' *** SR GETGCC: No gcc coordinates found in table',&
                     & /,16X,'Requested epoch:',F15.6,/)") epoch
    CALL exitrc(2)
  ENDIF

  gcc(1:3)   =gccdat%gccrec(irec1)%gcc(1:3)
  siggcc(1:3)=gccdat%gccrec(irec1)%siggcc(1:3)

  RETURN
END SUBROUTINE getgcc

END MODULE
