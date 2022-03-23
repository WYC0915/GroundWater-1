MODULE s_RDRESH2
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE rdresh2(lfn,resHed)

! -------------------------------------------------------------------------
! Purpose:    Writes the header of a Bernese Residual Files
!
! Author:     R. Dach
!
! Created:    26-Aug-2002
! Last mod.:  21-May-2010
!
! Changes:    16-May-2003 HU: Deallocate arrays
!             21-May-2003 RD: Make the deallocation safe
!             29-Dec-2003 HU: Implicit none added
!             07-Feb-2005 HB: Adopt for ifc-Compiler, Version 8.1
!             21-May-2010 MF: Call sr init_filhead
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_resfil, ONLY: t_resHead, init_filhead
  USE s_alcerr
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                   :: lfn     ! Unit number for reading

! output:
  TYPE(t_resHead)                :: resHed  ! Residual header from file

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=7), PARAMETER   :: srName = 'rdresh2'

! Local Variables
! ---------------
  INTEGER(i4b),DIMENSION(10000) :: buffer
  INTEGER(i4b)                  :: iFil
  INTEGER(i4b)                  :: nSat
  INTEGER(i4b)                  :: irc

! Deallocate files information records
! ------------------------------------
  IF (ASSOCIATED(resHed%filHead)) THEN
    DO iFil=1,resHed%nFil
      IF (iFil > SIZE(resHed%filHead)) EXIT
      DEALLOCATE(resHed%filHead(iFil)%numSat,stat=irc)
    ENDDO
    DEALLOCATE(resHed%filHead,stat=irc)
  ENDIF

! Read title of residual file and number of files ...
! ----------------------------------------------------
  READ(lfn) reshed%TITLE
  READ(lfn) reshed%nFil,reshed%dsc%iTyp,reshed%dsc%nDiff,reshed%dsc%nPar

! Complete file description
  IF (reshed%dsc%nDiff == 99) THEN
    READ(lfn) reshed%dsc

! Assumptions for old residual file
  ELSE IF (reshed%dsc%nDiff == 0) THEN
    reshed%dsc%iRecFmt = 1
    reshed%dsc%pgName  = 'unknown'
    reshed%dsc%nresta  = 0
    reshed%dsc%nresat  = 0
    reshed%dsc%nresep  = 0
    reshed%dsc%iElvaz  = 0
  ELSE IF (reshed%dsc%nDiff == 2) THEN
    reshed%dsc%iRecFmt = 1
    reshed%dsc%pgName  = 'unknown'
    reshed%dsc%nresta  = 1
    reshed%dsc%nresat  = 1
    reshed%dsc%nresep  = 0
    reshed%dsc%iElvaz  = 0
  ENDIF

! Allocate files information records
! ----------------------------------
  ALLOCATE(resHed%filHead(resHed%nFil),stat=irc)
  CALL alcerr(irc,'resHed%filHed',(/resHed%nFil/),srName)

  DO iFil = 1,resHed%nFil
    CALL init_filhead(resHed%filHead(iFil))
  END DO

! General information for each file
! ---------------------------------
  DO iFil = 1,resHed%nFil
    resHed%filHead(iFil)%icarr  = 0
    resHed%filHead(iFil)%stanam = ' '
    READ(lfn) resHed%filHead(iFil)%meatyp,           &
              resHed%filHead(iFil)%nfrfil,           &
              resHed%filHead(iFil)%icarr (1:resHed%filHead(iFil)%nfrfil),&
              resHed%filHead(iFil)%stanam(1:resHed%dsc%nResta+1),        &
              resHed%filHead(iFil)%csess (1:2),      &
              resHed%filHead(iFil)%ideltt,           &
              resHed%filHead(iFil)%timref
  ENDDO
!
! SV-numbers for each file
! ------------------------
  DO iFil=1,resHed%nFil
    READ(lfn) resHed%filHead(iFil)%nSatel,        &
              buffer(1:resHed%filHead(iFil)%nSatel)

!!!!!!!!! Problems with ifc81 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#ifdef CMP_IFC8
    nSat = resHed%filHead(iFil)%nSatel
    ALLOCATE(resHed%filHead(iFil)%numSat(nSat),stat=irc)
    CALL alcerr(irc,'resHed%filHead(iFil)%numSat',(/nSat/),srName)
#else
    ALLOCATE(resHed%filHead(iFil)%numSat(resHed%filHead(iFil)%nSatel),stat=irc)
    CALL alcerr(irc,'resHed%filHead(iFil)%numSat', &
                (/resHed%filHead(iFil)%nSatel/),srName)
#endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    resHed%filHead(iFil)%numSat(:) = buffer(1:resHed%filHead(iFil)%nSatel)
  ENDDO

END SUBROUTINE rdresh2

END MODULE
