MODULE s_STAECC
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE staecc(numSta,staNam)

! -------------------------------------------------------------------------
! Purpose:    Renames eccenter to center if an ECCENT file available
!
! Author:     R. Dach
!
! Created:    18-Apr-2002
! Last mod.:  29-Dec-2003
!
! Changes:    17-May-2003 HU: Initialize structure
!             29-Dec-2003 HU: Implicit none added
!
! SR called:  gtflna, readEcc
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_eccent, ONLY: t_eccent, init_eccent
  USE s_gtflna
  USE s_readecc
  IMPLICIT NONE

! List of Parameters
! ------------------
! input
  INTEGER(i4b)                  :: numSta

! input/output
  CHARACTER(LEN=*),DIMENSION(:) :: staNam

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6),PARAMETER    :: srName = 'staecc'

! Local Variables
! ---------------
  TYPE(t_eccent)                :: eccent

  CHARACTER(LEN=fileNameLength) :: eccFil

  INTEGER(i4b)                  :: iSta
  INTEGER(i4b)                  :: iEcc
  INTEGER(i4b)                  :: irc,iac


! Is an ECCENT file available?
! ----------------------------
  CALL gtflna(0,'ECCENT',eccFil,irc)

  IF (irc /= 0 .OR. numSta == 0) RETURN

! Read the eccenter file
! ----------------------
  CALL init_eccent(eccent)
  CALL readecc(eccFil,eccent)

! Rename station names to center names
! ------------------------------------
  DO iSta = 1,numSta
    DO iEcc=1,eccent%nEcc
      IF (staNam(iSta) == eccent%ecc(iEcc)%staNam) &
        staNam(iSta) = eccent%ecc(iEcc)%cenNam
    ENDDO
  ENDDO

  DEALLOCATE(eccent%ecc,stat=iac)

  RETURN
END SUBROUTINE staecc

END MODULE
