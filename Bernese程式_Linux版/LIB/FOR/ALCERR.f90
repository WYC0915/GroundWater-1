MODULE s_ALCERR
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE alcErr(iostat,varnam,varsiz,srname)

! -------------------------------------------------------------------------
! Purpose:    Display an error message if an array could not be allocated
!             Stops the program with an error if (iostat/=0)
!
! Author:     R. Dach
!
! Created:    14-Feb-2001
! Last mod.:  __-___-____
!
! Changes:    __-___-____ __:
!
! SR used:    exitrc
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules:
  USE m_bern

  USE s_exitrc
  IMPLICIT NONE
!
! Variables from parameter list
! -----------------------------
! input:
  INTEGER(i4b)                :: iostat   ! error status from allocation
                                          ! (0: OK, else stop with error)
  CHARACTER(LEN=*)            :: varnam   ! name of the allocated variable
  INTEGER(i4b), DIMENSION(:)  :: varsiz   ! size fo the allocated variable
  CHARACTER(LEN=*)            :: srname   ! name of the subrountine

! output:

! List of functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------

! Local Variables
! ---------------
  CHARACTER(LEN=LineLength)   :: hlpstr   ! string for variable size array

  INTEGER(i4b)                :: iVar     ! Counter for variable size array
  INTEGER(i4b)                :: i1,i2    ! String index

  IF (iostat /= 0) THEN
    IF (lfnerr == 0) lfnerr=6

    hlpstr='('
    DO iVar=1,SIZE(varsiz)
      i1=(iVar-1)*10+2
      i2=  iVar  *10+2
      WRITE(hlpstr(i1:i2),'(i8,a2)') varsiz(iVar),', '
    ENDDO
    i2=i2-2
    WRITE(hlpstr(i2:i2),'(a1)') ')'

    WRITE(lfnerr,'(/,A,3(/,16X,A),/,16X,A,I6,/)')        &
      ' *** SR ALCERR: ALLOCATION OF AN ARRAY FAILED',   &
                      'VARIABLE NAME : '//TRIM(varnam),  &
                      'SIZE OF ARRAY : '//TRIM(hlpstr),  &
                      'PGM./SUBR.    : '//TRIM(srname),  &
                      'ERROR STATUS  : ',iostat
    CALL exitrc(2)
  ENDIF

  RETURN
  END SUBROUTINE alcerr

END MODULE
