
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE p_chghed

! -------------------------------------------------------------------------
! Purpose:    This module defines structures for program CHGHED
!
! Author:     R. Dach
!
! Created:    11-Dec-2000
! Last mod.:  09-Jul-2003
!
! Changes:    09-Jul-2003 RD: t_chghed_flg may be removed
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
!
!
! Changes for variables of type character
! ---------------------------------------
  TYPE t_chghed_chr
   CHARACTER(LEN=20)       :: CHROLD       ! old values (chr-typ)
   CHARACTER(LEN=20)       :: CHRNEW       ! new values (chr-typ)
  END TYPE t_chghed_chr
!
! Changes for variables of type integer
! -------------------------------------
  TYPE t_chghed_int
   INTEGER(i4b)            :: INTOLD       ! old values (int-typ)
   INTEGER(i4b)            :: INTNEW       ! old values (int-typ)
  END TYPE t_chghed_int
!
! Changes for variables of type real
! ----------------------------------
  TYPE t_chghed_real
   REAL(r8b)               :: REAOLD       ! old values (real-typ)
   REAL(r8b)               :: REANEW       ! new values (real-typ)
  END TYPE t_chghed_real
!
! Option record
! -------------
  TYPE t_chghed
   INTEGER(i4b)            :: iOptio  ! option for parameter
                                      !  0: no change       1: 1st station
                                      !  2: 2nd station     3: both stations
   INTEGER(i4b)            :: OPTANY  ! changes any old entry ( 0: no / 1: yes )
   CHARACTER(LEN=20)       :: TEXT    ! text for protocoll file
   CHARACTER(LEN=keyNameLength),DIMENSION(3) &
                           :: keyList ! List of key words
   INTEGER(i4b) :: IDXCHR ! index in character list
   INTEGER(i4b) :: IDXINT ! Index in integer list
   INTEGER(i4b) :: IDXREA ! Index in real list
  END TYPE t_chghed
!

END MODULE p_chghed
