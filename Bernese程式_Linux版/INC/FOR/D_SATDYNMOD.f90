MODULE d_satdynmod

  ! Variables that describe satellite dynamics

  USE m_bern
  USE m_maxdim,  ONLY: maxPot

  REAL(r8b)               :: m_albedo ! albedo rpr
  REAL(r8b)               :: m_rprfac ! parameter for albedo model (WEISS NICHT, WO GESETZT!!)
  INTEGER(i4b)            :: m_nlon   ! parameter for albedo model (WEISS NICHT, WO GESETZT!!)
  INTEGER(i4b)            :: m_nlat   ! parameter for albedo model (WEISS NICHT, WO GESETZT!!)
  INTEGER(i4b)            :: m_modtyp ! satellite albedo model (e.g. black ball model)

  REAL(r8b), DIMENSION(7) :: m_ap     ! parameter for atmosphere model (input via menu)
  REAL(r8b)               :: m_cdrag  ! parameter for atmosphere model (input via menu)
  REAL(r8b)               :: m_f107   ! parameter for atmosphere model
  REAL(r8b)               :: m_qdm    ! parameter for atmosphere model (input via menu)
  REAL(r8b)               :: m_scldrg ! atmosphere model scale factor (estimated parameter)

  REAL(r8b)               :: m_crad   ! parameter for classical (simple) rpr model (input via menu)
  REAL(r8b)               :: m_qdmrad ! parameter for classical (simple) rpr model (input via menu)
  REAL(r8b)               :: m_scldrp ! rpr model scale factor (estimated parameter)
  INTEGER(i4b)            :: m_ishad  ! shadow flag

  INTEGER(i4b), PARAMETER              :: maxEmpTerm = 22
  INTEGER(i4b)                         :: m_rswdyx   ! decomposition of empirical forces
  REAL(r8b),   DIMENSION(3,maxEmpTerm) :: m_dyncoe   ! empirical force terms

  REAL(r8b),   DIMENSION(3) :: m_sclacc ! scale factor for accelerometer data
  REAL(r8b),   DIMENSION(3) :: m_accoff ! offset for accelerometer data
  REAL(r8b)                 :: m_scltid = 0.0 ! scale parameter for tide-generating potential
END MODULE
