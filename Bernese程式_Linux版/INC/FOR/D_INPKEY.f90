
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE d_inpKey

! -------------------------------------------------------------------------
! Purpose:    This module defines the structure for the input keyword list
!
! Author:     R. Dach
!
! Created:    17-Nov-2003
! Last mod.:  11-Mar-2010
!
! Changes:    18-Feb-2004 HB: No initialization in type declaration
!             11-Mar-2010 SL: ONLY added to USE m_bern
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,    ONLY: keyValueLength, i4b, t_key

  IMPLICIT NONE

! Input Keyword list structure
! ----------------------------
  TYPE t_inpKey
    CHARACTER(LEN=keyValueLength) :: inpFileName  ! Name of input file
    INTEGER(i4b)                  :: nKeys        ! Size of keys
    TYPE(t_key), DIMENSION(:),     &
                 POINTER          :: keys         ! Keyword buffer
    INTEGER(i4b)                  :: nDel         ! Size of delFil
    CHARACTER(LEN=keyValueLength), &
    DIMENSION(:),POINTER          :: delFil       ! Files to be deleted at
                                                  ! the end of the pgm run
                                                  ! (keyword: "DELETE_FILES")
    LOGICAL                       :: isOPNFIL     ! SR OPNFIL is running,
                                                  ! no special actions in EXITRC
    INTEGER(i4b)                  :: status       ! Status for program run:
                                                  ! 0: Init was done
                                                  ! 1: start reading input file
                                                  ! 2: INP-file is buffered
                                                  ! 3: Stop program in EXITRC
  END TYPE t_inpKey

! Input Keyword list variable
! ---------------------------
  TYPE(t_inpKey) :: inpKey

! Status parameters
! -----------------
  ! Structure has been initialized:
  INTEGER(i4b), PARAMETER :: myStatus_Init = 0

  ! Start reading input file:
  INTEGER(i4b), PARAMETER :: myStatus_Read = 1

  ! INP-file is buffered, program is running
  INTEGER(i4b), PARAMETER :: myStatus_Run  = 2

  ! Program is going to finish in EXITRC
  INTEGER(i4b), PARAMETER :: myStatus_Stop = 3

! Initialize structure
! --------------------
CONTAINS

  SUBROUTINE init_inpKey(inpKey)
    TYPE(t_inpKey)  :: inpKey
    inpKey%inpFileName = ' '
    inpKey%nKeys       = 0
    NULLIFY(inpKey%keys)
    inpKey%nDel        = 0
    NULLIFY(inpKey%delFil)
    inpKey%isOpnfil    = .FALSE.
    inpKey%status      = myStatus_Init
  END SUBROUTINE init_inpKey

END MODULE d_inpKey
