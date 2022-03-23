MODULE s_CKSIZEI1
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

 SUBROUTINE cksizei1(iArray,nElemt,increm)

! -------------------------------------------------------------------------
! Purpose:    Checks the size of the (i4b)-pointer array using nElemt
!             If nElemt is outside use increm to resize iArray
!
! Author:     R. Dach
!
! Created:    22-Dec-2004
! Last mod.:  08-Feb-2012
!
! Changes:    08-Feb-2012 RD: Copy pointer correctly
!
! SR Used:    alcerr
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern, ONLY: i4b

  USE s_alcerr
  IMPLICIT NONE

! List of Parameters
! ------------------
!
! input/output
  INTEGER(i4b), DIMENSION(:), POINTER :: iArray  ! Array to check/resize

! input
  INTEGER(i4b)                        :: nElemt  ! Array element to check
  INTEGER(i4b)                        :: increm  ! Increment to resize the array


! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER      :: srName = 'cksizei1'

! Local Variables
! ---------------
  INTEGER(i4b), DIMENSION(:), POINTER :: iHlp
  INTEGER(i4b)                        :: nSize
  INTEGER(i4b)                        :: ii
  INTEGER(i4b)                        :: irc


  NULLIFY(iHlp)

! Check the size
! --------------
  DO WHILE (nElemt > SIZE(iArray))

    nSize = SIZE(iArray)

! Allocate the buffer
! -------------------
    ALLOCATE(iHlp(nSize),stat=irc)
    CALL alcerr(irc,'iHlp',(/nSize/),srName)

! Copy the content into the buffer
! --------------------------------
    DO ii = 1,nSize
      iHlp(ii) = iArray(ii)
    ENDDO

! Reallocate the array
! --------------------
    IF (ASSOCIATED(iArray)) DEALLOCATE(iArray,stat=irc)

    ALLOCATE(iArray(nSize+increm),stat=irc)
    CALL alcerr(irc,'iArray',(/nSize+increm/),srName)

! Copy the content of the buffer back
! -----------------------------------
    iArray = 0
    DO ii = 1,nSize
      iArray(ii) = iHlp(ii)
    ENDDO

! Deallocate the buffer
! ---------------------
    DEALLOCATE(iHlp,stat=irc)

! Repreat the check of the size
! -----------------------------
  ENDDO

  RETURN
END SUBROUTINE cksizei1

END MODULE
