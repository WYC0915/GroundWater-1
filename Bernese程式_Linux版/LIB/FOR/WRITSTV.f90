MODULE s_writstv
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE writstv(stvfil,nsat,tutc,statevec,satcos,comment)


! -------------------------------------------------------------------------
! Purpose:    Write STV file
!
! Author:     C. Urschl
!
! Created:    10-Mar-2005
!
! Changes:    20-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------


! Modules
! -------
  USE m_bern,  ONLY: i4b, r8b, lfnerr, lfnloc, fileNameLength
  USE s_alcerr
  USE s_opnfil
  USE s_opnerr
  USE s_cordup

  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=fileNameLength)              :: stvfil   ! STV file name
  INTEGER(i4b)                               :: nsat     ! Number of satellites
  REAL(r8b),         DIMENSION(:)            :: tutc     ! Epoch in UTC
  REAL(r8b),         DIMENSION(:,:)          :: statevec ! State vector
  CHARACTER(LEN=9),  DIMENSION(:)            :: satcos   ! COSPAR ID
  CHARACTER(LEN=20), DIMENSION(:)            :: comment  ! Comment

! Local Variables
! ---------------
  INTEGER(i4b)                               :: isat
  INTEGER(i4b)                               :: irc
  INTEGER(i4b)                               :: ii
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE    :: index

  REAL(r8b),    DIMENSION(3)                 :: dummy

  CHARACTER(LEN=10), PARAMETER               :: srname = 'SR WRITSTV'

! Allocate and init
! -----------------
  dummy = 0d0
  ALLOCATE (index(nsat),STAT=irc)
  CALL alcerr(irc,'index',(/nsat/),srname)

! Open stv file
! -------------
  CALL opnfil(lfnloc,stvfil,'NEW','','',' ',irc )
  CALL opnerr(lfnerr,lfnloc,irc,stvfil,srname)

! Print header
! ------------
  WRITE(lfnloc,'(A,2(/,3A))')                                                 &
    '# State vector in J2000.0',                                              &
    '#   TOSC              X               Y               Z               ', &
    'XDOT            YDOT            ZDOT       BSTAR      MMDOT1     ',      &
    'MMDOT2    COSPARID OBJID',                                               &
    '#   [MJD]            [m]             [m]             [m]              ', &
    '[m/s]           [m/s]           [m/s]      [1/rE]     [1/d**2]   ',      &
    '[1/d**3]'

! Sort array
! ----------
  CALL cordup(satcos,nsat,1,9,index)

! Print state vectors (J2000)
! ---------------------------
  DO isat = 1, nsat

    ii = index(isat)

    WRITE(lfnloc,'(F15.9,6(F16.5),3(F11.8),1X,A2,A4,3X,A)') &
    ! Epoch in MJD (UTC)
      tutc(ii),                        &
    ! State vector (m, m/s)
      statevec(ii,1:6),                &
    ! Dummy
      dummy,                           &
    ! COSPAR-ID
      satcos(ii)(3:4),satcos(ii)(6:9), &
    ! Comment
      TRIM(comment(ii))

  ENDDO

! Close stv file
! --------------
  CLOSE(lfnloc)

  DEALLOCATE(index)

  RETURN


  END SUBROUTINE writstv

END MODULE s_writstv
