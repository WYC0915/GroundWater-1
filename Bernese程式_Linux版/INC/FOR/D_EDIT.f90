! ------------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! ------------------------------------------------------------------------------

MODULE d_edit

! ------------------------------------------------------------------------------
! Purpose:    This module defines structures for the EDIT-Info-File
!
! Author:     H. Bock
!
! Created:    20-Feb-2002
! Last mod.:  13-May-2003
!
! Changes:    02-Oct-2002 RD: Separate resMax for each meaTyp
!             12-May-2003 SS: maxEdt from 15000 to 30000
!             13-May-2003 CU: Nullify pointers
!             16-Aug-2011 LP: MaxEdt from 30000 to 50000
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! ------------------------------------------------------------------------------

! Modules
! -------
  USE m_bern

! Parameter
! ---------
  INTEGER(i4b),PARAMETER :: maxEdt=50000

! Structures
! ----------
! Structure for Header of Edit-Info-File
! --------------------------------------
  TYPE t_edtHead
    CHARACTER(LEN=4),DIMENSION(2)             :: cseEdt ! session nr and nr of files
                                                        ! from same basel/session
    CHARACTER(LEN=staNameLength),DIMENSION(2) :: staEdt ! station names
    REAL(r8b)                                 :: timEdt ! ref time of file
    INTEGER(i4b)                              :: idtEdt ! table interval in sec
    INTEGER(i4b)                              :: meaEdt ! measurement types
  END TYPE t_edtHead

! Structure for Records of Edit-Info-File
! ---------------------------------------
  TYPE t_edtRec
    REAL(r8b)                 :: lstCyc
    INTEGER(i4b),DIMENSION(7) :: lstEdt ! Definition of editing request
                                        !  (1): SV-number
                                        !  (2): first epoch of marked area i
                                        !  (3): last  epoch of marked area i
                                        !  (4): frequency (1=L1, 2=L2)
                                        !  (5): edit request from  ...
                                        !  (6): file number
                                        !  (7): editing type (action)
                                        !       =  1: mark observations
                                        !       = -1: reset observations
                                        !       =  2: eliminate observations
                                        !       =  3: apply cycle slip
                                        !       =  4: set up new ambiguity
                                        !       = -4: remove ambiguity
                                        !       =  5: set cycle slip flag
                                        !       = -5: reset cycle slip flag
  END TYPE t_edtRec

! Structure for complete Edit-Info-File
! -------------------------------------
  TYPE t_edit
    TYPE(t_edtHead),DIMENSION(:),POINTER :: head
    TYPE(t_edtRec),DIMENSION(:),POINTER  :: rec

    CHARACTER(LEN=80)                   :: title  ! Title for Edit Info File
    CHARACTER(LEN=fileNameLength)       :: filNam ! Name of the Edit File

    REAL(r8b),  DIMENSION(3)            :: resMax ! Residual size for outlier
                                                  ! detection (for each meatyp)
    INTEGER(i4b)                        :: nEdFil ! Number of files in Edit file list
    INTEGER(i4b)                        :: nEdt   ! Number of editing requests
    INTEGER(i4b)                        :: nSampl ! Sampling interval of residual
                                                  ! files in seconds
    INTEGER(i4b)                        :: minAmb ! Minimum number of observations
                                                  ! per ambiguity
    INTEGER(i4b)                        :: minInt ! Minimum time interval for small pieces
                                                  ! of data (seconds)
    INTEGER(i4b)                        :: iSampl ! Sampling rate for counting obs.
  END TYPE t_edit

! Initialize structure
! --------------------
CONTAINS

  SUBROUTINE init_edit(edit)
    TYPE(t_edit)   :: edit

    NULLIFY(edit%head)
    NULLIFY(edit%rec)
    edit%nEdFil = 0
    edit%nEdt   = 0
  END SUBROUTINE init_edit

END MODULE d_edit
