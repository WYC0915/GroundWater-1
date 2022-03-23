
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE d_rgbfil

! -------------------------------------------------------------------------
! Purpose:    This module defines the structure used for the
!             SLR correction file.
!
! Author:     D. Thaller
!
! Created:    28-May-2009
! Last mod.:  29-Sep-2009
!
! Changes:    06-Jul-2009 DT: Add pointCode for SINEX file (rgbSNX)
!             29-Sep-2009 DT: Changes for general SLR correction file;
!                             Add SR wtSlrFil
!
! SR contained: init_slrFil, wtSlrFil
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE m_epoch, ONLY : t_timWin

! Declare access rights
! ---------------------
!  PRIVATE
!  PUBLIC  :: init_slrFil, wtSlrFil


! Structures for SLR correction file
! ----------------------------------
  TYPE t_rgb
    CHARACTER(LEN=3)                                 :: corrTyp

    CHARACTER(LEN=staNam2Length)                     :: staNam
    INTEGER(i4b)                                     :: satNum

    CHARACTER(LEN=1)                                 :: WLchar
    INTEGER(i4b)                                     :: WLind

    REAL(r8b)                                        :: value
    REAL(r8b)                                        :: sigma
    TYPE(t_timWin)                                   :: timWin
    CHARACTER(LEN=6)                                 :: solFlag
    CHARACTER(LEN=23)                                :: remark
  END TYPE t_rgb

  TYPE t_slrInfo
    CHARACTER(LEN=fileNameLength)                    :: slrFile
    CHARACTER(LEN=lineLength), DIMENSION(7)          :: headline
    REAL(r8b)                                        :: version
    TYPE(t_rgb)              , DIMENSION(:), POINTER :: rgb
    INTEGER(i4b)                                     :: nrgb
!!!    TYPE(t_com)              , DIMENSION(:), POINTER :: CoM
!!!    INTEGER(i4b)                                     :: nCoM
  END TYPE t_slrInfo

  REAL(r8b), PARAMETER  :: currentVersion = 1.0

! PointCode for estimated range bias parameters in SINEX file
! -----------------------------------------------------------
  CHARACTER(LEN=2), DIMENSION(6)  :: rgbSNX = &
                    (/'L1','L2','E1','E2','LC','EC'/)


! Code for Data Handling in SINEX file (column "M")
! -------------------------------------------------
  INTEGER(i4b), PARAMETER :: nDataFlg = 6

  CHARACTER(LEN=1), DIMENSION(0:6)  :: dataSNX = &
                    (/'X','E','R','S','T','P','C'/)

! Corresponding Type and Flag in Bernese SLR correction file
! ----------------------------------------------------------
  CHARACTER(LEN=3), DIMENSION(1:6)  :: typBSW = &
                    (/'RGB','RGB','SCB','TIM','P  ','COM'/)

  CHARACTER(LEN=6), DIMENSION(1:6)  :: flagBSW = &
                    (/'estim ','apri  ','apri  ','apri  ','apri  ','apri  '/)


CONTAINS


! -----------------------------------------------------------------------------
! Initialize structure
! -----------------------------------------------------------------------------
  SUBROUTINE init_slrFil(slrInfo)

    TYPE(t_slrinfo) :: slrInfo

    NULLIFY(slrInfo%rgb)

    slrInfo%version = currentVersion
    slrInfo%slrFile = ''
    slrInfo%headline(:) = ''
    slrInfo%nrgb = 0

  END SUBROUTINE init_slrFil


! -----------------------------------------------------------------------------
! Write SLR correction file
! -----------------------------------------------------------------------------
  SUBROUTINE wtSlrFil(slrInfo)

    USE m_epoch,  ONLY: OPERATOR(.epochToReal.)
    USE m_time,   ONLY: t_timint
    USE d_const,  ONLY: filtitle

    USE s_opnerr
    USE s_opnfil
    USE s_timst2

    ! List of parameters
    TYPE(t_slrInfo)   :: slrInfo

    ! Local variables
    ! ---------------
    TYPE(t_timint)                 :: timInt

    CHARACTER(LEN=timStrgLength2)  :: timStr
    CHARACTER(LEN=2)               :: wl
    CHARACTER(LEN=4)               :: satNum

    INTEGER(i4b)      :: ios
    INTEGER(i4b)      :: icor


  ! Open SLR correction file
  ! ------------------------
    CALL opnfil(lfnloc,slrInfo%slrFile,'UNKNOWN','FORMATTED',' ',' ',ios)
    CALL opnerr(lfnerr,lfnloc,ios,slrInfo%slrFile,'RGBOUT')


  ! Write header lines
  ! ------------------

    WRITE(lfnloc,'(A,/,94("-"),//,A,F5.2,//,3A,/,3A)')                 &
      filtitle,                                                        &
      'Format Version: ', currentVersion,                             &
      'TYP  Station name           Sat  WL  Value (m)    Sigma (m)',   &
      '    Valid from           to                   ',                &
      'Flag    Remark',                                                &
      '***  ********************  ****  **  ****.******  ****.******', &
      '  *******************  *******************  ',                  &
      '******  ***********************'

  ! Write record lines
  ! ------------------
    DO icor=1, slrInfo%nrgb

!!! entfällt sobald nur noch Epochs verwendet werden
      timInt%t(1) = .epochToReal.slrInfo%rgb(icor)%timWin%t(1)
      timInt%t(2) = .epochToReal.slrInfo%rgb(icor)%timWin%t(2)

      CALL timst2(1,2,timInt%t,timStr)

      wl = '  '
      IF ( slrInfo%rgb(icor)%WLchar == 'R' ) &
        WRITE(wl, '(A1,I1)') slrInfo%rgb(icor)%WLchar, slrInfo%rgb(icor)%WLind

      satNum = '    '
      IF ( slrInfo%rgb(icor)%satNum /= 1000 ) &
        WRITE(satNum, '(I4)') slrInfo%rgb(icor)%satNum


      WRITE(lfnloc, '(A3,2X,A16,6X,A4,2X,A2,2(2X,F11.6),2X,A,2X,A6,2X,A23)') &
         slrInfo%rgb(icor)%corrTyp, slrInfo%rgb(icor)%staNam, satNum, wl,    &
         slrInfo%rgb(icor)%value, slrInfo%rgb(icor)%sigma,                   &
         timStr, slrInfo%rgb(icor)%solFlag, slrInfo%rgb(icor)%remark

    ENDDO

    CLOSE(lfnloc)

  END SUBROUTINE wtSlrFil


END MODULE d_rgbfil


