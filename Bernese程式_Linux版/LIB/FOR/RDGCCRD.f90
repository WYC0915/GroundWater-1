MODULE s_RDGCCRD
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdgccrd(filgcc,gccdat,irCode)

! -------------------------------------------------------------------------
! Purpose:    Read file of geocenter coordinates.
!
! Remarks:    The routine reads gcc files containing piece-wise
!             constant estimates.
!
! Author:     U. Hugentobler
!
! Created:    24-Mar-2002
! Last mod.:  15-May-2003
!
! Changes:    15-May-2003 HU: Deallocate array
!
! SR used:    gtflna, opnfil, opnerr, exitrc, alcerr
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_gccdat, ONLY: t_gccdat

  USE s_opnfil
  USE s_alcerr
  USE s_exitrc
  USE s_gtflna
  USE s_opnerr
  IMPLICIT NONE
!
! Variables in parameter list
! ---------------------------
! IN:
  CHARACTER(LEN=*)       :: filgcc    ! file name, blank: use GTFLNA mechanism
                                      ! with keyword GEOCENTR

! OUT:
  TYPE(t_gccdat)         :: gccdat    ! Structure of geocenter file
  INTEGER(i4b)           :: irCode    ! Return code of this SR
                                      !  =1: File already read
                                      !  =2: No file available

! Local Variables
! ---------------
  INTEGER(i4b),SAVE      :: init=1    ! =0: buffer initialized
  INTEGER(i4b)           :: iostat
  INTEGER(i4b)           :: ircgcc
  INTEGER(i4b)           :: nrec,irec

  REAL(r8b),DIMENSION(2) :: timint
  REAL(r8b),DIMENSION(3) :: gcc,siggcc

  CHARACTER(LEN=fileNameLength),SAVE :: filgco='$'
  CHARACTER(LEN=fileNameLength)      :: filnam

  irCode=0
  gccdat%ifrmt=1

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
! Get Filenamee
! -------------
! Apply GTFLNA mechanism
  filnam=filgcc
  IF (filgcc == ' ') THEN
    CALL gtflna(0,'GCCINP',filnam,ircgcc)
    IF (ircgcc == 1) THEN
      gccdat%nrec=0
      irCode=2
      RETURN
    ENDIF
  ENDIF

! READ FILE
! =========
!
! Reading necessary for init=1, filgcc /= filgco
! ----------------------------------------------
  IF (init == 1 .OR. filnam /= filgco) THEN
    IF (init == 0) DEALLOCATE(gccdat%gccrec)
    init  =0
    filgco=filnam

    CALL opnfil(lfnloc,filnam,'OLD',' ', 'READONLY',' ',iostat)
    CALL opnerr(lfnerr,lfnloc,iostat,filnam,'RDGEOC')

! Count number of entries
! -----------------------
    nrec=0
    DO
      READ(lfnloc,*,IOSTAT=iostat)
      IF (IOSTAT /= 0) EXIT
      nrec=nrec+1
    ENDDO
    REWIND(lfnloc)
    gccdat%nrec=nrec

! Allocate memory
! ---------------
    IF (nrec > 0) THEN
      ALLOCATE(gccdat%gccrec(nrec),STAT=iostat)
      CALL alcerr(iostat,'gccdat%gccrec',(/nrec/),'RDGEOC')
    ENDIF

! Read file
! ---------
    ReadLoop: DO irec=1,nrec
      READ(lfnloc,*,IOSTAT=iostat)timint(1:2),gcc(1:3),siggcc(1:3)
      IF (iostat < 0) EXIT ReadLoop
      IF (iostat > 0) THEN
        WRITE(lfnerr,"(/,' *** SR RDGEOC: Error reading data line:',&
                     & /,16X,'Record line:',I8,/)")irec
        CALL exitrc(2)
      ENDIF

      gccdat%gccrec(irec)%timint(1:2)=timint(1:2)
      gccdat%gccrec(irec)%gcc(1:3)   =gcc(1:3)
      gccdat%gccrec(irec)%siggcc(1:3)=siggcc(1:3)
    ENDDO ReadLoop
    CLOSE(lfnloc)

! FILE ALREADY READ
! -----------------
  ELSE
    irCode=1
  ENDIF

  RETURN
END SUBROUTINE rdgccrd

END MODULE
