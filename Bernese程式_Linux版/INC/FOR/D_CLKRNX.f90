
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE d_clkrnx

! -------------------------------------------------------------------------
! Purpose:    This module defines structures and global variables for
!             the programs / subroutines using CLK RINEX files
!
! Author:     R. Dach
!
! Created:    09-Aug-2000
!
! Changes:    18-Feb-2001 RD: More than 1 reference clock per file
!             14-May-2001 RD: Improved structure od reference clock array
!             13-May-2003 CU: Nullify pointers
!             02-Dec-2003 HB: Add PARAMETER unDef = 999999.999999D0
!             24-Nov-2006 AG: timsys, pgmnam, dcbstr, pcvstr added to ClkHead
!             09-May-2009 RD: Seperate receiver clocks for GPS/GLONASS
!             21-Sep-2009 RD: Eclipsing flag added
!             30-Nov-2010 MF: Add call to init_ref in sr copy_clkHead
!             28-Mar-2012 RD: Use SVN2CHR as module now
!             16-Jul-2013 RD: Add all remaining records to copy_clkHead
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, staNameLength
  USE m_time,   ONLY: t_timint

  USE s_svn2chr
  USE s_alcerr

  IMPLICIT NONE

! Parameter
! ---------
  REAL(r8b),         PARAMETER   :: unDef = 999999.999999D0  ! For initialize
                                                             ! values and sigmas
  CHARACTER(LEN=8),  PARAMETER, PRIVATE :: srName = 'd_clkrnx'

! Type for clock rinex header
! ---------------------------
  TYPE t_refclk
    CHARACTER(LEN=staNameLength) :: Name   ! Name of the reference station
    REAL(r8b)                    :: Sigma  ! Constraints for the ref-clock (ms)
    INTEGER(i4b)                 :: Idx    ! Index of the ref-clock in sta-list
    INTEGER(i4b)                 :: Idx0   ! Index of the ref-clock in the
                                           ! reference file
  END TYPE t_refclk

  TYPE t_ref
    INTEGER(i4b)                 :: nRef   ! Number of reference stations
                                           ! in file
    TYPE(t_refclk), DIMENSION(:), POINTER  &
                                 :: clk    ! List of reference clocks
    TYPE(t_timint)               :: refWin ! Ref-clock window in the file (sec)
  END TYPE t_ref

! Difference of the reference of this file with resp. to the reference file
!       REAL(r8b)        , DIMENSION(:,:), POINTER    :: RefDif
!       REAL(r8b)        , DIMENSION(:), POINTER      :: RefDifMean

! Type for clock rinex header
! ---------------------------
  TYPE t_clkhead
    REAL(r8b)                    :: RnxVers  ! Version fo the RINEX files
    INTEGER(i4b)                 :: nComment ! Number of comment lines
    CHARACTER(LEN=60), DIMENSION(:), POINTER &
                                 :: Comment  ! Comment lines
    CHARACTER(LEN=20)            :: ProgNam  ! Program name
    CHARACTER(LEN=20)            :: RunBy    ! "Run By"
    CHARACTER(LEN=20)            :: CrDate   ! Creation date
    INTEGER(i4b)                 :: LeapSec  ! Leap Second
    INTEGER(i4b)                 :: NumTyp   ! Number of data types
    CHARACTER(LEN= 2), DIMENSION(:), POINTER &
                                 :: DatTyp   ! List of data types
    CHARACTER(LEN=staNameLength) :: Clk0Name ! Name for the clocks
    CHARACTER(LEN=60)            :: CalName  ! Calibration station identifier
    CHARACTER(LEN= 3)            :: AC       ! Analysis center ID
    CHARACTER(LEN=55)            :: ACName   ! Analysis center name
    CHARACTER(LEN= 3)            :: timsys   ! Time system
    CHARACTER(LEN=17)            :: pgmnam   ! Program that applies DCBs and PCVs
    CHARACTER(LEN=40)            :: dcbStr   ! Line for DCBs applied
    CHARACTER(LEN=40)            :: pcvstr   ! Line for PCV model
    REAL(r8b)                    :: TFirst   ! First Epoch (MJD)
    INTEGER(i4b)                 :: numRef   ! Number of reference clock records
    TYPE(t_ref),       DIMENSION(:), POINTER &
                                 :: ref      ! List of all reference clock sets
    CHARACTER(LEN=50)            :: TRFName  ! Terrestrial reference frame
    INTEGER(i4b)                 :: nSta     ! Number of station clocks
    INTEGER(i4b)                 :: nSat     ! Number of satellite clocks
    CHARACTER(len=staNameLength), &
           DIMENSION(:), POINTER :: ClkName  ! Names of the sta/sat clocks
    REAL(r8b),       DIMENSION(:,:), POINTER &
                                 :: StaCoord ! Coordinates of the stations (m)
  END TYPE t_clkhead

! Type for clock rinex header
! ---------------------------
  TYPE t_clkrec
    INTEGER(i4b)                       :: iEpo  ! Epoch index of the file
    INTEGER(i4b)                       :: nEpo  ! Number of epoch in file
    REAL(r8b), DIMENSION(:),   POINTER :: Epoch ! Epoch of the clock record
                                                ! (s after TFirst)
    REAL(r8b), DIMENSION(:,:), POINTER :: Clock ! Values for the clocks (ms)
    REAL(r8b), DIMENSION(:,:), POINTER :: Sigma ! Sigma of the clocks (ms)
    CHARACTER(LEN=1),                   &
               DIMENSION(:,:), POINTER :: ClkFlg ! Flags for the clocks
                                                 ! 0: eclipsing satellites
  ENDTYPE t_clkrec

! Initialize structure
! --------------------
CONTAINS

  SUBROUTINE init_clkhead(clkhead)
    TYPE(t_clkhead) :: clkhead

    NULLIFY(clkhead%comment)
    NULLIFY(clkhead%DatTyp)
    NULLIFY(clkhead%clkName)
    NULLIFY(clkhead%ref)
    NULLIFY(clkhead%staCoord)
    clkhead%numRef = 0
  END SUBROUTINE init_clkhead

  SUBROUTINE init_clkrec(clkrec)
    TYPE(t_clkrec)  :: clkrec

    NULLIFY(clkrec%Epoch)
    NULLIFY(clkrec%Clock)
    NULLIFY(clkrec%Sigma)
  END SUBROUTINE init_clkrec

  SUBROUTINE init_ref(ref)
    TYPE(t_ref)  :: ref

    NULLIFY(ref%clk)
    ref%nRef = 0
  END SUBROUTINE init_ref


! Copy the header structure
! -------------------------
  FUNCTION copy_clkHead(clkHead)
    TYPE(t_clkhead)                    :: clkhead
    TYPE(t_clkhead)                    :: copy_clkhead

    INTEGER(i4b)                       :: ii
    INTEGER(i4b)                       :: iac

    copy_clkHead%RnxVers  = clkHead%RnxVers
    copy_clkHead%nComment = clkHead%nComment

    ALLOCATE(copy_clkHead%comment(copy_clkHead%nComment),stat=iac)
    CALL alcerr(iac,'copy_clkHead%comment',(/copy_clkHead%nComment/),srName)
    DO ii = 1,copy_clkHead%nComment
      copy_clkHead%comment(ii) = clkHead%comment(ii)
    ENDDO

    copy_clkHead%ProgNam  = clkHead%ProgNam
    copy_clkHead%RunBy    = clkHead%RunBy
    copy_clkHead%CrDate   = clkHead%CrDate
    copy_clkHead%LeapSec  = clkHead%LeapSec
    copy_clkHead%NumTyp   = clkHead%NumTyp

    ALLOCATE(copy_clkHead%DatTyp(copy_clkHead%NumTyp),stat=iac)
    CALL alcerr(iac,'copy_clkHead%DatTyp',(/copy_clkHead%NumTyp/),srName)
    DO ii = 1,copy_clkHead%NumTyp
      copy_clkHead%DatTyp(ii) = clkHead%DatTyp(ii)
    ENDDO

    copy_clkHead%Clk0Name = clkHead%Clk0Name
    copy_clkHead%CalName  = clkHead%CalName
    copy_clkHead%AC       = clkHead%AC
    copy_clkHead%ACName   = clkHead%ACName
    copy_clkHead%pgmnam   = clkHead%pgmnam
    copy_clkHead%timsys   = clkHead%timsys
    copy_clkHead%dcbStr   = clkHead%dcbStr
    copy_clkHead%pcvStr   = clkHead%pcvStr
    copy_clkHead%TFirst   = clkHead%TFirst
    copy_clkHead%numRef   = clkHead%numRef

    ALLOCATE(copy_clkHead%ref(copy_clkHead%numRef),stat=iac)
    CALL alcerr(iac,'copy_clkHead%ref',(/copy_clkHead%numRef/),srName)
    DO ii = 1,copy_clkHead%numRef
      CALL init_ref(copy_clkHead%ref(ii))
      copy_clkHead%ref(ii) = copy_ref(clkHead%ref(ii))
    ENDDO

    copy_clkHead%TRFName  = clkHead%TRFName
    copy_clkHead%nSta     = clkHead%nSta
    copy_clkHead%nSat     = clkHead%nSat

    ii = copy_clkHead%nSta + copy_clkHead%nSat
    ALLOCATE(copy_clkHead%ClkName(ii),stat=iac)
    CALL alcerr(iac,'copy_clkHead%ClkName',(/ii/),srName)
    DO ii = 1,copy_clkHead%nSta + copy_clkHead%nSat
      copy_clkHead%ClkName(ii) = clkHead%ClkName(ii)
    ENDDO

    ALLOCATE(copy_clkHead%StaCoord(3,copy_clkHead%nSta),stat=iac)
    CALL alcerr(iac,'copy_clkHead%StaCoord',(/3,copy_clkHead%nSta/),srName)
    DO ii = 1,copy_clkHead%nSta
      copy_clkHead%StaCoord(1:3,ii) = clkHead%StaCoord(1:3,ii)
    ENDDO

  END FUNCTION copy_clkHead

! Copy the header structure
! -------------------------
  FUNCTION copy_ref(ref)
    TYPE(t_ref)                        :: ref
    TYPE(t_ref)                        :: copy_ref

    INTEGER(i4b)                       :: ii
    INTEGER(i4b)                       :: iac

    copy_ref%nRef   = ref%nRef
    copy_ref%refWin = ref%refWin

    ALLOCATE(copy_ref%clk(copy_ref%nRef),stat=iac)
    CALL alcerr(iac,'copy_ref%clk',(/copy_ref%nRef/),srName)
    DO ii = 1,copy_ref%nRef
      copy_ref%clk(ii)%name  = ref%clk(ii)%name
      copy_ref%clk(ii)%sigma = ref%clk(ii)%sigma
      copy_ref%clk(ii)%idx   = ref%clk(ii)%idx
      copy_ref%clk(ii)%idx0  = ref%clk(ii)%idx0
    ENDDO

  END FUNCTION copy_ref


! Get the index in the list of clocks for a station or satellite
! --------------------------------------------------------------
  FUNCTION clkIdx(clkhead,clknam,satnum)

    TYPE(t_clkhead)                    :: clkhead
    CHARACTER(staNameLength), OPTIONAL :: clknam
    INTEGER(i4b),             OPTIONAL :: satnum
    INTEGER(i4b)                       :: clkIdx

    CHARACTER(LEN=3) :: svnnam
    CHARACTER(LEN=1) :: svnchr

    INTEGER(i4b) :: svnnum
    INTEGER(i4b) :: iClk

    clkIdx=0

    IF(PRESENT(clknam)) THEN
      DO iClk = 1,clkHead%nSta+clkHead%nSat
        IF (clkHead%clkName(iClk) == clknam) THEN
          clkIdx = iClk
          EXIT
        ENDIF
      ENDDO
    ELSE IF(PRESENT(satnum)) THEN
      CALL svn2chr(satnum,svnnum,svnchr)
      WRITE(svnnam,'(A1,I2.2)') svnchr,svnnum

      DO iClk = clkHead%nSta+1,clkHead%nSta+clkHead%nSat
        IF (clkHead%clkName(iClk) == svnnam) THEN
          clkIdx = iClk
          EXIT
        ENDIF
      ENDDO
    ENDIF

  END FUNCTION clkIdx

END MODULE d_clkrnx
