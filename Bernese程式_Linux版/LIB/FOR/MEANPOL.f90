MODULE s_meanpol
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE meanpol(iSel,tReq,xQuer,yQuer)

!-----------------------------------------------------------------------
! Purpose:    Provide the mean pol coordinates depending on iSel
!
! Author:     H. Bock
!
! Created:    04-Jan-2006
! Last mod.:  07-Jun-2011
!
! Changes:    28-Feb-2007 AG: Use 206264... from DEFCON
!             06-May-2011 HB: IERS2010 conventions added
!             07-Jun-2011 HB: Update of descriptions
!
! SR used:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! --------------------------------------------------------------------

! Modules
! -------
    USE m_bern
    USE d_const, ONLY: ars
    USE s_exitrc
    IMPLICIT NONE

! List of Parameters
! ------------------
! input:
    INTEGER(i4b) :: iSel   ! selection parameter: =1: old definition
                           !                      =2: IERS2003 convention
                           !                      =3: IERS2010 convention
    REAL(r8b)    :: tReq   ! time in MJD

! output:
    REAL(r8b)    :: xQuer  ! mean x-pole coordinate (radian)
    REAL(r8b)    :: yQuer  ! mean y-pole coordinate (radian)

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
    CHARACTER(LEN=7), PARAMETER :: srName = 'meanpol'


! Local Variables
! ---------------
    REAL(r8b) :: tRel

! Selection of mean pol
! ---------------------
    IF (iSel == 1) THEN
      xQuer = 0.033D0
      yQuer = 0.331D0
    ELSEIF (iSel == 2) THEN
      tRel = (tReq - 51544.D0)/365.25D0
      xQuer = 0.054D0 + 0.00083D0*tRel
      yQuer = 0.357D0 + 0.00395D0*tRel
    ELSEIF (iSel == 3) THEN  ! IERS TN No.36, Ch 7.1.4, Eq(7.25), Tab.7.7
      tRel = (tReq - 51544.D0)/365.25D0
      IF (tReq < 55197.D0) THEN        ! until 2010.0
        xQuer = 0.055974D0 + (0.0018243D0 + 0.00018413D0*tRel + 0.000007024D0*tRel*tRel)*tRel
        yQuer = 0.346346D0 + (0.0017896D0 - 0.00010729D0*tRel - 0.000000908D0*tRel*tRel)*tRel
      ELSE                             ! after 2010.0
        xQuer = 0.023513D0 + 0.0076141D0*tRel
        yQuer = 0.358891D0 - 0.0006287D0*tRel
      ENDIF
    ELSE
      WRITE(lfnErr,'(A,A,A,/,16X,A,I3)')&
           '### SR ',srName,': Unknown selection parameter ',&
                              'for mean pol: ',iSel
      CALL exitrc(2)
    ENDIF

    xQuer = xQuer/ars
    yQuer = yQuer/ars

    RETURN
  END SUBROUTINE meanpol

END MODULE s_meanpol
