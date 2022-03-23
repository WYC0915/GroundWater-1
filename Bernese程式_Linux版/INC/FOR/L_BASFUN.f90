
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE l_basfun

! -------------------------------------------------------------------------
! Purpose:    This module provides some basic routine
!
! Author:     R. Dach
!
! Created:    04-May-2012
!
! Changes:
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------


CONTAINS

! -----------------------------------------------------------------------------
!
! Purpose:    Computes the remainder of the division of A by P.
!             It is calculated as A - (INT(A/P) * P).
!
! Remark:     Replace instinct function because of some compiler uncertainties
!             in the results
!
! Author:     R. Dach
!
! Created:    04-May-2012
!
! Changes:
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
!
! -----------------------------------------------------------------------------
  FUNCTION DMOD(A,P)
! -----------------------------------------------------------------------------
    USE m_bern,  ONLY: r8b

    REAL(r8b), INTENT(IN)         :: A
    REAL(r8b), INTENT(IN)         :: P
    REAL(r8b)                     :: DMOD

    REAL(r8b)                     :: R1
    REAL(r8b)                     :: R2

    R1=DBLE(A)
    R2=DBLE(P)

    dmod = R1 - (DINT(R1/R2) * R2)
  END FUNCTION
END MODULE l_basfun

