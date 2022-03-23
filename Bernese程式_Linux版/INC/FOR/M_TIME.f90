
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE m_time

! -------------------------------------------------------------------------
! Purpose:    This module defines structures, operators and functions for
!             handling time intervals
!
! Author:     L. Mervart
!
! Created:    22-NOV-1997
! Last mod.:  04-Nov-2003
!
! Changes:    06-MAY-1999 : HB: Extracting from MODULE p_addneq
!             04-Nov-2003 : HB: Rename PROCEUDRE isIn -> isInInt
!                               (Compiler-problem: Portland LINUX)
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern


! Generally useful structures
! ---------------------------
  TYPE t_timint
    REAL(r8b),DIMENSION(2) :: t                  ! t(1) ... left boundary
  END TYPE t_timint                              ! t(2) ... right boundary

  TYPE t_time
    SEQUENCE
    REAL(r8b) :: mean     ! middle of the interval
    REAL(r8b) :: half     ! half interval length
  END TYPE t_time

! Operator declaration
! --------------------
  INTERFACE OPERATOR(.isIn.)
    MODULE PROCEDURE isInInt
  END INTERFACE

  INTERFACE OPERATOR(+)
    MODULE PROCEDURE addTimint
  END INTERFACE

CONTAINS

! Operator definitions
! --------------------
  LOGICAL FUNCTION isInInt(epoch,timint)
    REAL(r8b),INTENT(IN)       :: epoch
    TYPE(t_timint),INTENT(IN)  :: timint
    REAL(r8b),PARAMETER        :: dT = (1.d0/24.d0)/6.d0    ! 10 minutes
    IF ( timint%t(1) - dT <= epoch    .AND. &
         timint%t(2) + dT >= epoch )  THEN
      isInInt = .TRUE.
    ELSE
      isInInt = .FALSE.
    END IF
  END FUNCTION isInInt

  TYPE(t_timint) FUNCTION addTimint(timint1,timint2)
    TYPE(t_timint),INTENT(IN)  :: timint1
    TYPE(t_timint),INTENT(IN)  :: timint2

    IF ( timint1%t(1) < timint2%t(1) ) THEN
      addTimint%t(1) = timint1%t(1)
    ELSE
      addTimint%t(1) = timint2%t(1)
    END IF

    IF ( timint1%t(2) > timint2%t(2) ) THEN
      addTimint%t(2) = timint1%t(2)
    ELSE
      addTimint%t(2) = timint2%t(2)
    END IF

  END FUNCTION addTimint

END MODULE m_time


