
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE p_stamrg

! -------------------------------------------------------------------------
! Purpose:    This module defines structures for program STAMERGE
!
! Author:     A.Steinbach
!
! Created:    07-Nov-2007
! Last mod.:  08-Feb-2011
!
! Changes:    06-Jan-2011 SL: use m_bern with ONLY
!             08-Feb-2011 SL: variable creduc removed
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b,r8b,fileNameLength
  USE d_stalst, ONLY: t_staList

! Options
! -------
  TYPE t_stamrg_opt

 ! Filenames
 ! ---------
  CHARACTER(LEN=fileNameLength)      :: mtrFil   ! master STAINFO file
  CHARACTER(LEN=fileNameLength)      :: secFil   ! secondary STAINFO file
  CHARACTER(LEN=fileNameLength)      :: resFil   ! combined STAINFO file

 ! Sort master sta file
 ! --------------------
  INTEGER(i4b)                       :: csort    ! 0: no sort, 1: do sort

 ! Consider all entries in section 3
 ! --------------------------------
  INTEGER(i4b)                       :: cconsid  ! 0: consider, 1: do not

 ! Actions for sta info sections
 ! -----------------------------
  INTEGER(i4b)                       :: ctype01  ! 0: do nothing
  INTEGER(i4b)                       :: ctype02  ! 1: compare
  INTEGER(i4b)                       :: ctype03  ! 2: merge
  INTEGER(i4b)                       :: ctype04  ! 3: delete
  INTEGER(i4b)                       :: ctype05  !

  INTEGER(i4b)                       :: crant    ! 0: use
  INTEGER(i4b)                       :: crrec    ! 1: use not

! Selections
! ----------
  REAL(r8b),DIMENSION(2)             :: timwin   ! selected time window
  TYPE(t_staList)                    :: staList  ! Station list

  END TYPE t_stamrg_opt

END MODULE p_stamrg
