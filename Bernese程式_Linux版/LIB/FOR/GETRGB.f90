MODULE s_GETRGB
CONTAINS

! ---------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! ---------------------------------------------------------------------

SUBROUTINE getrgb(corrTyp, staNam, sat, WL, time, rgbReq, corFound)

! ---------------------------------------------------------------------
! Purpose:    Get data correction (bias, CoM)  from SLR data handling
!             file for station / satellite / wavelength / epoch
!
! Author:     D. Thaller
!
! Created:    28-May-2009
! Last mod.:  25-Aug-2010
!
! Changes:    30-Sep-2009 DT: Changes for general SLR data handling file
!             25-Aug-2010 HB: staNam2Length replaced with staNameLength,
!                             problem with Portland compiler
!
! SR used:    gtflna, opnfil, opnerr, timst2
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! ---------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_epoch

  USE d_rgbfil, ONLY: t_slrInfo, t_rgb, typBSW, nDataFlg

  USE s_readrgb
  USE s_gtflna


  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=3)                           :: corrTyp ! Type of SLR correction
  CHARACTER(LEN=staNameLength)               :: staNam  ! Station name
  INTEGER(i4b)                               :: sat     ! Satellite
  INTEGER(i4b)                               :: WL      ! Wavelength
  TYPE(t_epoch)                              :: time    ! Epoch

! output:
  TYPE(t_rgb)                                :: rgbReq   ! Requested RGB structure
  INTEGER(i4b)                               :: corFound ! Return code
                                                         ! 0 = Value for request found
                                                         ! 1 = No valid value found


! Local Variables
! ---------------
  TYPE(t_slrInfo), SAVE                      :: slrInfo   ! Content of RGB file

  CHARACTER(LEN=fileNameLength)              :: filRGB    ! Name of RGB file

  INTEGER(i4b)                               :: irgb, ind
  INTEGER(i4b)                               :: irc
  INTEGER(i4b)                               :: ityp
  INTEGER(i4b), DIMENSION(3)                 :: satFound  ! (1) exact request fulfilled
                                                          ! (2) "type-specific" found
                                                          ! (3) "all satellite" found

  LOGICAL, SAVE                              :: first = .TRUE.
  LOGICAL                                    :: okTyp


! Initialize some variables
! -------------------------
  corFound = 1
  satFound(:) = 0

  rgbReq%staNam = ''
  rgbReq%satNum = 0
  rgbReq%value  = 0d0
  rgbReq%sigma  = 0d0
  rgbReq%timWin%t(1) = 0d0
  rgbReq%timWin%t(2) = 0d0

! Check requested type of correction
! ----------------------------------
  okTyp = .FALSE.
  DO ityp = 1, nDataFlg
    IF ( corrTyp == typBSW(ityp) ) THEN
      okTyp = .TRUE.
      EXIT
    END IF
  END DO

  IF ( .NOT. okTyp ) THEN
    WRITE(lfnerr, '(A,/,16X,A,A3/)')  &
          ' ### SR GETRGB: Requested type of correction not defined.', &
          'Correction type: ', corrTyp
    RETURN
  END IF


! Read SLR file if first call
! ---------------------------
  IF ( first ) THEN
    first = .FALSE.

    CALL gtflna(0, 'RGBINP ', filRGB, irc)

  ! Return, if no a priori SLR file specified
  ! -----------------------------------------
    IF ( filRGB == '' ) RETURN

  ! Read RGB file
  !--------------
    CALL readrgb(filRGB, slrInfo)

  ENDIF

! Look for RGB record that fits to the actual request
! ---------------------------------------------------
  DO irgb = 1, slrInfo%nrgb

    IF ( slrInfo%rgb(irgb)%corrTyp(1:3) /= corrTyp(1:3) ) CYCLE

    IF ( staNam(1:14) /= slrInfo%rgb(irgb)%staNam(1:14) .AND. &
         slrInfo%rgb(irgb)%staNam(1:1) /= ' '                  ) CYCLE

    IF ( .NOT. isInWin(time,slrInfo%rgb(irgb)%timWin) )  CYCLE

    IF ( wl /= slrInfo%rgb(irgb)%WLind .AND. &
         slrInfo%rgb(irgb)%WLind > 0          ) CYCLE

!!    rgbReq%corrTyp = corrTyp
!!    rgbReq%staNam  = slrInfo%rgb(irgb)%staNam
!!    rgbReq%timWin  = slrInfo%rgb(irgb)%timWin
!!
!!    rgbReq%WLchar = 'R'
!!!    rgbReq%WLind  = slrInfo%rgb(irgb)%WLind
!!    rgbReq%WLind  = wl
!!
!!    rgbReq%remark = ' '

   ! Exact request found
   ! -------------------
    IF ( slrInfo%rgb(irgb)%satNum == sat ) THEN

      satFound(1:3) = irgb
      EXIT


    ELSE

    ! Search all sat value: exact request must be fulfilled
    ! -----------------------------------------------------
      IF ( sat == 1000 ) CYCLE

    ! Given type-specific, Search special sat
    ! ---------------------------------------
      IF ( slrInfo%rgb(irgb)%satNum < 0 .AND. sat > 0       .AND. &
           1+int(sat/100) == ABS(slrInfo%rgb(irgb)%satNum)  .AND. &
           satFound(2) == 0                                      ) &

        satFound(2) = irgb

    ! Given all sat, Search special sat
    ! or Given all sat, Search type-specific
    ! --------------------------------------
      IF ( slrInfo%rgb(irgb)%satNum == 1000 .AND. &
           satFound(3)==0 )  &

        satFound(3) = irgb

    ENDIF

  END DO

! Fill rgbReq structure
! ---------------------
  DO ind = 1, 3

    IF ( satFound(ind) > 0 ) THEN

      corFound = 0
      rgbReq%corrTyp = corrTyp
      rgbReq%staNam  = slrInfo%rgb(satFound(ind))%staNam
      rgbReq%satNum  = slrInfo%rgb(satFound(ind))%satNum
      rgbReq%value   = slrInfo%rgb(satFound(ind))%value
      rgbReq%sigma   = slrInfo%rgb(satFound(ind))%sigma
      rgbReq%timWin  = slrInfo%rgb(satFound(ind))%timWin
      rgbReq%WLchar  = 'R'
      rgbReq%WLind   = wl
      rgbReq%remark = ' '

      EXIT
    END IF

  END DO

  RETURN

END SUBROUTINE getrgb

END MODULE
