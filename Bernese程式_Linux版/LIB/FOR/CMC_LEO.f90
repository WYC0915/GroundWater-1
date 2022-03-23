MODULE s_CMC_LEO
CONTAINS

! -----------------------------------------------------------------------------
! Bernese GPS Software Version 5.0
! -----------------------------------------------------------------------------

  SUBROUTINE cmc_leo(XAPR1,XAPR2,XWGSEPO1,XWGSEPO2,TOSC,   &
                     CMCYN_GNSS,CMCYN_LEO)

! -----------------------------------------------------------------------------
! Purpose    :  APPLY CMC-CORRECTION TO COORDINATES
!               SR: CMC_LEO
!
! Author     :  L. Prange
!
! Created    :  24-Apr-2008
! Last mod.  :  16-Dec-2010
!
! Changes    :  16-Dec-2010 RD: add CMC for ATL
!
! SR used    :
!
! Remarks    :
!
! Copyright  :  Astronomical Institute
!               University of Bern
!               Switzerland
! -----------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_maxdim, ONLY: MAXOCN
  USE s_exitrc
  USE s_cmc,    ONLY: getcmc
  IMPLICIT NONE

! List of Parameters
! ------------------
! IN :
! ----
  REAL(r8b)                       :: tosc    ! observation epoch
  REAL(r8b),DIMENSION(3)          :: xapr1   ! apriori LEO Position (in)
  REAL(r8b),DIMENSION(3)          :: xwgsepo1! wgs LEO Position (in)
  LOGICAL,  DIMENSION(2)          :: cmcyn_GNSS ! CMC apllied in GNSS STD file?
  LOGICAL,  DIMENSION(2)          :: cmcyn_LEO  ! CMC apllied in LEO STD file?


! OUT :
! ----
  REAL(r8b),DIMENSION(3)          :: xapr2   ! corrected apriori LEO Position
  REAL(r8b),DIMENSION(3)          :: xwgsepo2! corrected wgs LEO Position


! Local variables
! ---------------
  REAL(r8b),DIMENSION(3)               :: cmcGNS    ! CMC correction
  REAL(r8b),DIMENSION(3)               :: cmcLEO    ! CMC correction


! Get CMC corrections
! -------------------
  CALL getcmc(cmcyn_GNSS,TOSC,cmcGNS)
  CALL getcmc(cmcyn_LEO ,TOSC,cmcLEO )

! Apply the corrections
! ---------------------
  xapr2(1:3)    = xapr1(1:3) - cmcLEO(1:3)
  xwgsepo2(1:3) = xwgsepo1(1:3) - cmcLEO(1:3) + cmcGNS(1:3)


  END SUBROUTINE cmc_leo

END MODULE
