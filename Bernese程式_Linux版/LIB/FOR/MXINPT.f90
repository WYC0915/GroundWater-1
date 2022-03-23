MODULE s_MXINPT
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE mxinpt(iSngDF)

! -------------------------------------------------------------------------
! Purpose:    This is a new version of the old subroutine MXINPT.f that
!             reads the input options of the program MPRXTR
!
! Author:     D. Ineichen
!
! Created:    27-Sep-2001
!
! Changes:    23-Apr-2003  AJ: Nullify local pointers
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern

  USE s_readkeys
  USE s_exitrc
  IMPLICIT NONE

! List of parameters
! ------------------
  INTEGER(i4b)         :: iSngDF   ! Single-difference flag
                                   ! =1: Write only bad single-difference
                                   !     files into deletion file list
                                   ! =2: Write zero- and single-difference
                                   !     files into the deletion file list

! Local variables
! ---------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER       :: keyValue
  INTEGER(i4b)                                               :: irc, irCode

  NULLIFY(keyValue)

! Init variables
! --------------
  irCode = 0

! Read main options
! -----------------
  CALL readkeys('ISNGDF', keyValue, irc)
  irCode = irCode + irc
  IF      (keyValue(1) == 'SINGLE'  ) THEN
   iSngDF = 1
  ELSE IF (keyValue(1) == 'BOTH'    ) THEN
   iSngDF = 2
  ELSE
    WRITE(lfnerr,'(A,/)')                                                   &
          ' *** SR MXINPT: Wrong entry for keyword "ISNGDF" in input file'
    irCode = irCode + 1
  END IF

! Problems reading input options
! ------------------------------
  IF (irCode /= 0) CALL exitrc(2)

  DEALLOCATE(keyValue,stat=irc)

END SUBROUTINE mxinpt

END MODULE
