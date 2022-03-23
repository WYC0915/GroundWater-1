! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE p_orbgen

! -------------------------------------------------------------------------
! Purpose:    This module defines structures for program ORBGEN
!
! Author:     U. Hugentobler
!
! Created:    27-JAN-2005
! Last mod.:  03-Dec-2010
!
! Changes:    05-Aug-2008 DT: Add otdmin (Min. magnitude of ocean tides)
!                             Add variable orbdsc for orbit description
!             26-Aug-2010 DT: Add MAXINT (=22000)
!             03-Dec-2010 KS: Add otddeg (max degree of ocean tides model)
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern

! Maximum number of orbit model descriptions
! ------------------------------------------
  INTEGER(i4b),PARAMETER  :: maxomd=20
  INTEGER(i4b),PARAMETER  :: maxint=22000

! Orbit model type
! ----------------
  TYPE t_orbmodel
    INTEGER(i4b)                        :: nlin
    CHARACTER(LEN=80),DIMENSION(maxomd) :: orbmod
    REAL(r8b)                           :: otdmin
    INTEGER(i4b)                        :: otddeg
  END TYPE t_orbmodel

! Orbit model default
! -------------------
  TYPE(t_orbmodel) orbmdft

! Description of Orbit model
! --------------------------
  TYPE(t_orbmodel), SAVE  :: orbdsc

END MODULE p_orbgen
