MODULE s_READDT
CONTAINS

! ------------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! ------------------------------------------------------------------------------

  SUBROUTINE ReadDt(filnam,DTTAB)

! ------------------------------------------------------------------------------
!
! Purpose    : read header of precise orbit file and return tabular interval
!
! SR called  :
!
! Remarks    :
!
! Author     : D. Svehla
!
! Created    : 30-Mar-2002
! last mod.  : 08-Mar-2003
!
! Changes    : 30-Mar-2003 HU: Use m_maxdim
!              08-Mar-2003 HU: Interface for sr rdpreh used
!                              Call for sr rdpreh updated
!
! Copyright  : Astronomical Institute
!              University of Bern
!              Switzerland
! ------------------------------------------------------------------------------

  USE m_bern
  USE m_maxdim, ONLY: maxsat

  USE s_rdpreh
  IMPLICIT NONE

!
! Parameters
! ==========
!
! IN :
! ----
  CHARACTER(LEN=fileNameLength)   :: filnam  ! Precise orbit file
!
! OUT:
! ----
  REAL(r8b)                       :: DTTAB   ! tabular interval from header of
                                             ! precise orbit file
!
! Local variables
! ---------------
  CHARACTER(LEN=57),DIMENSION(4)  :: TITLE
  CHARACTER(LEN=5)                :: COOSYS,DATDES
  CHARACTER(LEN=4)                :: AGENCY
  CHARACTER(LEN=3)                :: ORBTYP,TIMSYS
  CHARACTER(LEN=2)                :: FILTYP

  INTEGER(i4b),DIMENSION(MAXSAT)  :: SATNUM,SATWGT
  INTEGER(i4b)                    :: IFRMAT,NSAT,NEPO

  REAL(r8b)                       :: TFIRST,BASPOS,BASCLK
  REAL(r8b),SAVE                  :: DTTABH

  LOGICAL,SAVE  :: first=.TRUE.
!
! FIRST CALL OF SUBROUTINE
! ========================
  IF (first) THEN
    first=.FALSE.
!
! READ READ HEADER OF PRECISE ORBIT FILE
! ======================================
    CALL RDPREH(filnam,lfnloc,IFRMAT,NSAT,SATNUM,SATWGT,TFIRST,         &
                NEPO,DTTABH,TITLE,DATDES,COOSYS,ORBTYP,AGENCY,          &
                FILTYP,TIMSYS,BASPOS,BASCLK)
    CLOSE(lfnloc)
  END IF

  DTTAB=DTTABH


  RETURN
  END SUBROUTINE ReadDt

END MODULE
