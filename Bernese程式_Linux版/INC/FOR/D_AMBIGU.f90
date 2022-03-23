! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE d_ambigu

! -------------------------------------------------------------------------
! Purpose:    This module defines the data type for the ambiguities
!
! Author:     H. Bock
!
! Created:    09-Jul-2002
! Last mod.:  __-___-____
!
! Changes:    __-___-____ __:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern

! Ambiguities
! -----------
  TYPE t_ambigu
    REAL(r8b)   ,DIMENSION(3)   :: ambigu  ! Ambiguities
    INTEGER(i4b)                :: ambsat  ! Satellite Number
    INTEGER(i4b)                :: ambiep  ! Ambiguity Epoch Numb.
    INTEGER(i4b),DIMENSION(2)   :: ambwlf  ! Wavelength Factors
    INTEGER(i4b),DIMENSION(3)   :: ambcls  ! Ambiguity Clusters
    LOGICAL                     :: solved  ! Flag for resolved or not
  END TYPE t_ambigu

End MODULE d_ambigu
