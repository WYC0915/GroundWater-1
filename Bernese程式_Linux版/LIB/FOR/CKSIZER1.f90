MODULE s_CKSIZER1
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

 SUBROUTINE cksizer1(rArray,nElemt,increm,init)

! -------------------------------------------------------------------------
! Purpose:    Checks the size of the (r8b)-pointer array using nElemt
!             If nElemt is outside use increm to resize rArray
!
! Author:     R. Dach
!
! Created:    16-Jan-2003
! Last mod.:  08-Feb-2012
!
! Changes:    23-Apr-2003 CU: Nullify local pointers
!             27-May-2009 RD: Optional argument for initialization
!             08-Feb-2012 RD: Copy pointer correctly
!
! SR Used:    alcerr
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern

  USE s_alcerr
  IMPLICIT NONE

! List of Parameters
! ------------------
!
! input/output
  REAL(r8b), DIMENSION(:), POINTER :: rArray  ! Array to check/resize

! input
  INTEGER(i4b)                     :: nElemt  ! Array element to check
  INTEGER(i4b)                     :: increm  ! Increment to resize the array
  REAL(r8b), OPTIONAL              :: init    ! Value to init the new allocated
                                              ! array (default: 0d0)


! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER      :: srName = 'cksizer1'

! Local Variables
! ---------------
  INTEGER(i4b)                     :: nSize
  INTEGER(i4b)                     :: ii
  INTEGER(i4b)                     :: irc

  REAL(r8b), DIMENSION(:), POINTER :: rHlp

  NULLIFY(rHlp)

! Check the size
! --------------
  DO WHILE (nElemt > SIZE(rArray))

    nSize = SIZE(rArray)

! Allocate the buffer
! -------------------
    ALLOCATE(rHlp(nSize),stat=irc)
    CALL alcerr(irc,'rHlp',(/nSize/),srName)

! Copy the content into the buffer
! --------------------------------
    DO ii = 1,nSize
      rHlp(ii) = rArray(ii)
    ENDDO

! Reallocate the array
! --------------------
    IF (ASSOCIATED(rArray)) DEALLOCATE(rArray,stat=irc)

    ALLOCATE(rArray(nSize+increm),stat=irc)
    CALL alcerr(irc,'rArray',(/nSize+increm/),srName)

! Copy the content of the buffer back
! -----------------------------------
    DO ii = 1,nSize
      rArray(ii) = rHlp(ii)
    ENDDO
    IF (PRESENT(init)) THEN
      rArray(nSize+1:nSize+increm) = init
    ELSE
      rArray(nSize+1:nSize+increm) = 0d0
    ENDIF

! Deallocate the buffer
! ---------------------
    DEALLOCATE(rHlp,stat=irc)

! Repreat the check of the size
! -----------------------------
  ENDDO

  RETURN
END SUBROUTINE cksizer1

END MODULE
