MODULE s_RGBSTORE
CONTAINS

! ---------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! ---------------------------------------------------------------------

SUBROUTINE rgbstore(neq)

! ---------------------------------------------------------------------
! Purpose:    Store range bias results and SLR data correction info
!
! Author:     D. Thaller
!
! Created:    28-May-2009
! Last mod.:  29-Sep-2009
!
! Changes:    29-Sep-2009 DT: Use only as interface to D_RGBFIL
!             16-Dec-2010 RD: t_neq is taken from D_NEQ instead of P_ADDNEQ
!
! SR used:    alcerr, init_slrFil, wtSlrFil
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! ---------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_epoch,  ONLY: ASSIGNMENT(=)
  USE d_neq,    ONLY: t_neq
  USE d_rgbfil, ONLY: t_slrInfo, init_slrFil, wtSlrFil
  USE p_addneq, ONLY: opt, comstat

  USE s_alcerr
  USE f_ikf

  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  TYPE(t_neq)                                :: neq  ! Normal equation

! Local Variables
! ---------------
  TYPE(t_slrInfo)                            :: slrInfo

  CHARACTER(LEN=8), PARAMETER                :: srName = 'rgbstore'

  INTEGER(i4b)                               :: ipar
  INTEGER(i4b)                               :: icor
  INTEGER(i4b)                               :: iac


! Initialize some variables
! -------------------------
  CALL init_slrFil(slrInfo)
  slrInfo%nrgb = 0

  icor = 0


! Return, if no RGB output file specified
! ---------------------------------------
  IF ( opt%rgbout == '' ) RETURN


! Check for range bias parameters
! -------------------------------
  DO  ipar = 1, neq%misc%npar
    IF ( neq%par(ipar)%locq(1) == 26 ) icor = icor + 1
  ENDDO


! Allocate SLR file structure
! ---------------------------
  ALLOCATE(slrInfo%rgb(icor), STAT=iac)
  CALL alcerr(iac, 'slrInfo%rgb', (/icor/), srName)


! Fill SLR structure
! ------------------
  slrInfo%slrFile = opt%rgbout
  slrInfo%nrgb    = icor

  icor = 0

  DO  ipar = 1, neq%misc%npar

    IF ( neq%par(ipar)%locq(1) /= 26 ) CYCLE

    icor = icor + 1

    slrInfo%rgb(icor)%corrTyp = 'RGB'
    slrInfo%rgb(icor)%staNam  = neq%par(ipar)%name
    slrInfo%rgb(icor)%satNum  = neq%par(ipar)%locq(5)
    slrInfo%rgb(icor)%WLchar  = 'R'
    slrInfo%rgb(icor)%WLind   = neq%par(ipar)%locq(4)
    slrInfo%rgb(icor)%value   = neq%xxx(ipar)+neq%par(ipar)%x0
    slrInfo%rgb(icor)%sigma   = comstat%rms * DSQRT(neq%anor(ikf(ipar,ipar)))

    slrInfo%rgb(icor)%solFlag = 'sol   '
    slrInfo%rgb(icor)%remark  = ''

    slrInfo%rgb(icor)%timWin%t(1) = neq%par(ipar)%time%mean - &
                                    neq%par(ipar)%time%half
    slrInfo%rgb(icor)%timWin%t(2) = neq%par(ipar)%time%mean + &
                                    neq%par(ipar)%time%half

  ENDDO


! Write SLR correction file
! -------------------------
  CALL wtSlrFil(slrInfo)


  RETURN

END SUBROUTINE rgbstore

END MODULE
