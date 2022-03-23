MODULE s_CKSIZEC1
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

 SUBROUTINE cksizec1(cArray,nElemt,increm)

! -------------------------------------------------------------------------
! Purpose:    Checks the size of the string-pointer array using nElemt
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
  USE m_bern

  USE s_alcerr
  IMPLICIT NONE

! List of Parameters
! ------------------
!
! input/output
  CHARACTER(LEN=*), DIMENSION(:),    &
                    POINTER         :: cArray  ! Array to check/resize

! input
  INTEGER(i4b)                      :: nElemt  ! Array element to check
  INTEGER(i4b)                      :: increm  ! Increment to resize the array


! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER      :: srName = 'cksizec1'

! Local Variables
! ---------------
  CHARACTER(LEN=255), DIMENSION(:), &
                      POINTER      :: cHlp

  INTEGER(i4b)                     :: nSize
  INTEGER(i4b)                     :: ii
  INTEGER(i4b)                     :: irc


  NULLIFY(cHlp)

! Check the size
! --------------
  DO WHILE (nElemt > SIZE(cArray))

    nSize = SIZE(cArray)

! Allocate the buffer
! -------------------
    ALLOCATE(cHlp(nSize),stat=irc)
    CALL alcerr(irc,'cHlp',(/nSize/),srName)
    cHlp = ''

! Copy the content into the buffer
! --------------------------------
    DO ii = 1,nSize
      cHlp(ii) = cArray(ii)
    ENDDO

! Reallocate the array
! --------------------
    IF (ASSOCIATED(cArray)) DEALLOCATE(cArray,stat=irc)

    ALLOCATE(cArray(nSize+increm),stat=irc)
    CALL alcerr(irc,'cArray',(/nSize+increm/),srName)

! Copy the content of the buffer back
! -----------------------------------
    cArray = ''
    DO ii = 1,nSize
      cArray(ii) = cHlp(ii)
    ENDDO

! Deallocate the buffer
! ---------------------
    DEALLOCATE(cHlp,stat=irc)

! Repreat the check of the size
! -----------------------------
  ENDDO

  RETURN
END SUBROUTINE cksizec1

END MODULE
