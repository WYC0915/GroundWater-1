! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  MODULE d_resfil

! -------------------------------------------------------------------------
! Purpose:    This module defines the data types for the
!             Bernese Residual Files
!
! Author:     R. Dach
!
! Created:    23-Aug-2002
! Last mod.:  21-May-2010
!
! Changes:    19-Sep-2002 RD: Bug in MAUPRP0 default record
!             13-May-2003 CU: Nullify pointers
!             21-May-2010 MF: Define sr init_filhead
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern

! Residual description record
! ---------------------------
  TYPE t_descr
    INTEGER(i4b)                    :: iRecFmt ! Record format number
    CHARACTER(LEN=10)               :: pgName ! Program name, created the file
    INTEGER(i4b)                    :: nResat ! Diff level of satellites
    INTEGER(i4b)                    :: nResta ! Diff level of stations
    INTEGER(i4b)                    :: nResep ! Diff level of epochs
    INTEGER(i4b)                    :: iTyp   ! Type of residual file
                                              ! =1: only the linear combinations
                                              !     of L1 and L2 which are
                                              !     written on the file may be
                                              !     displayed
                                              ! =2: L1 and L2 residuals are
                                              !     stored so that any linear
                                              !     combination may be displayed
    INTEGER(i4b)                    :: nDiff  ! Difference level of observations
                                              ! =0: zero diff.
                                              ! =1: single diff.
                                              ! =2: double/triple diff.
                                              ! =99: detailed descr.-new record
    INTEGER(i4b)                    :: nPar   ! Number of parameters
    INTEGER(i4b)                    :: iElvAz ! Elev./Azimuth record available
                                              ! =0:  not available
                                              ! =-1: Tim s trick (coded in the
                                              !          2nd satellite number)
  END TYPE t_descr

! Default description records for programs writing residual files
! ---------------------------------------------------------------
  TYPE(t_descr),          PARAMETER :: resHed_ORBGEN = &
       t_descr( 1,'ORBGEN',          & ! iRecFmt, pgName
                0, 0, 0,             & ! nResat nResta nResep
                1,99, 0, 0 )           ! iTyp nDiff nPar iElvAz

  TYPE(t_descr),          PARAMETER :: resHed_CODSPP = &
       t_descr( 1,'CODSPP',          & ! iRecFmt, pgName
                0, 0, 0,             & ! nResat nResta nResep
                1,99, 0, 0 )           ! iTyp nDiff nPar iElvAz

  TYPE(t_descr),          PARAMETER :: resHed_IONEST = &
       t_descr( 1,'IONEST',          & ! iRecFmt, pgName
                0, 0, 0,             & ! nResat nResta nResep
                1,99, 0, 0 )           ! iTyp nDiff nPar iElvAz

  TYPE(t_descr),          PARAMETER :: resHed_MAUPRP0 = &
       t_descr( 1,'MAUPRP',          & ! iRecFmt, pgName
                1, 0, 1,             & ! nResat nResta nResep
                2,99, 4, 0 )           ! iTyp nDiff nPar iElvAz

  TYPE(t_descr),          PARAMETER :: resHed_MAUPRP1 = &
       t_descr( 1,'MAUPRP',          & ! iRecFmt, pgName
                1, 1, 1,             & ! nResat nResta nResep
                2,99, 3, 0 )           ! iTyp nDiff nPar iElvAz

  TYPE(t_descr),          PARAMETER :: resHed_GPSEST0 = &
       t_descr( 1,'GPSEST',          & ! iRecFmt, pgName
                0, 0, 0,             & ! nResat nResta nResep
                1,99, 0,-1 )           ! iTyp nDiff nPar iElvAz

  TYPE(t_descr),          PARAMETER :: resHed_GPSEST1 = &
       t_descr( 1,'GPSEST',          & ! iRecFmt, pgName
                1, 1, 0,             & ! nResat nResta nResep
                1,99, 0, 0 )           ! iTyp nDiff nPar iElvAz


! Observation file header information
! -----------------------------------
  TYPE t_filHead
    INTEGER(i4b)                    :: meatyp ! Obs-type (1=phase, 2=code)
    INTEGER(i4b)                    :: nFrFil ! Number of freq.
    INTEGER(i4b),      DIMENSION(3) :: iCarr  ! 1:first req. freq, k=2: second.
    CHARACTER(LEN=16), DIMENSION(2) :: staNam ! Station names
    CHARACTER(LEN=4),  DIMENSION(2) :: csess  ! k=1: Session identifier,
                                              ! k=2: File identifier in session
    INTEGER(i4b)                    :: ideltt ! Spacing between observations
    REAL(r8b)                       :: timref ! Reference time
    INTEGER(i4b)                    :: nsatel ! Number of satellites
    INTEGER(i4b),      DIMENSION(:), &
                       POINTER      :: numsat ! SV-numbers
  END TYPE t_filHead


! Header information of the residual file
! ---------------------------------------
  TYPE t_resHead
    CHARACTER(LEN=80)               :: title  ! Title line

    TYPE(t_descr)                   :: dsc    ! Residual decription record

    INTEGER(i4b)                    :: nFil   ! Total number of files
    TYPE(t_filHead),  DIMENSION(:),  &
                      POINTER       :: filHead ! File header information
  END TYPE t_resHead


! Residual record type
! --------------------
  TYPE t_resRec
    INTEGER(i4b)                    :: iFile  ! Index in filHead list
    INTEGER(i4b)                    :: iEpoch ! Epoch number
    INTEGER(i4b)                    :: iFreq  ! Frequency
    INTEGER(i4b), DIMENSION(2)      :: svnNum ! Satellite numbers
    REAL(r8b)                       :: value  ! Residual value
    CHARACTER(LEN=1)                :: resflg ! Residual flag
  END TYPE t_resRec

! Initialize structure
! --------------------
CONTAINS

  SUBROUTINE init_reshead(resHead)
    TYPE(t_resHead)  :: resHead

    NULLIFY(resHead%filHead)
    resHead%nFil = 0
  END SUBROUTINE init_reshead

  SUBROUTINE init_filhead(filHead)
    TYPE(t_filHead)  :: filHead

    NULLIFY(filHead%numsat)
    filHead%nsatel = 0
  END SUBROUTINE init_filhead

END MODULE d_resfil


