MODULE s_submod
CONTAINS
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE submod(xtdb, subfar, nsub, submlt, subcoe, erpsub, erpsur)

! ---------------------------------------------------------------------
! Purpose:    Compute subdaily ERP corrections in x-pole, y-pole,
!             and UT1
!
! Remark:     Adapted from old version SUBMOD.f
!
!             Values are not recomputed if the routine is called
!             with a time argument differing by less than 1 second
!             from that of the previous call
!
! Author:     M. Rothacher
!
! Created:    26-Feb-1998
! Last mod.:  07-Jun-2011
!
! Changes:    31-Oct-2003 HU: Do not recompute for same time argument
!             30-Jun-2005 HB: Written in Fortran 90 and add nutational
!                             corrections of pole coordinates
!             21-Oct-2008 HB: Make SUBMOD a module
!             06-May-2011 HB: Both SUB-file formats supported,
!                             array subNut is included in new SUB-file format
!                             for IERS2010 conventions(IERS2010XY.SUB)
!             07-Jun-2011 HB: Update of descriptions
!
! SR used:     subval
!
! Copyright:   Astronomical Institute
!              University of Bern
!              Switzerland
!-------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_model,  ONLY: getModKey, mod_orb_subFmt,&
                      chrValLength
  USE s_subval
  IMPLICIT NONE

  INTEGER(i4b), PARAMETER :: MAXPER=1000

! List of parameters
! ------------------
! input:
  REAL(r8b)             :: xtdb   ! Epoch in mod. julian date (TDB)
  REAL(r8b), &
    DIMENSION(6,6)      :: subfar ! i=1,..,6,j=1,..,6: coefficients
                                  ! to compute fundamental arguments
                                  ! i=1,..5: Terms with dt**(i-1) in
                                  !          arcsec per century etc.
                                  ! i=6    : Number of full revolutions
                                  !          corresponding to 1296000"
                                  ! j= 1: L = Mean anomaly of the Moon
                                  ! j= 2: L'= Mean anomaly of the Sun
                                  ! j= 3: F = Mean longitude of the Moon - O
                                  ! j= 4: D = Mean longitude of the Moon - mean
                                  !           longitude of the Sun
                                  ! j= 5: O = Mean longitude of the node of the
                                  !           Moon
                                  ! j= 6: TP= Greenwich mean sidereal time
  INTEGER(i4b)          :: nsub   ! Number of subdaily erp periods
  INTEGER(i4b), &
    DIMENSION(6,*)      :: submlt ! j=1,..,6,k=1,..,nsub: Multipliers
                                  ! of the fundamental arguments for each
                                  ! subdaily term (period)
  REAL(r8b), &
    DIMENSION(6,*)      :: subcoe ! l=1,..,6,k=1,..,nsub: Coefficients
                                  ! of subdaily erp model in arcsec and sec
                                  ! l=1:  cos X
                                  ! l=2:  sin X
                                  ! l=3:  cos Y
                                  ! l=4:  sin Y
                                  ! l=5:  cos UT
                                  ! l=6:  sin UT

! output:
  REAL(r8b),DIMENSION(3):: erpsub ! Subdaily ERP correction
                                  ! i=1: x-pole (arcsec)
                                  ! i=2: y-pole (arcsec)
                                  ! i=3: UT1    (sec)
  REAL(r8b),DIMENSION(3):: erpsur ! Subdaily ERP correction for
                                  ! i=1: x-pole (arcsec/day)
                                  ! i=2: y-pole (arcsec/day)
                                  ! i=3: UT1    (sec/day)

! Local parameters
! ----------------
  REAL(r8b),SAVE :: eposav=0D0

! Arguments and coefficients for nutational corrections
! of pole coordinates
! -----------------------------------------------------
  INTEGER(i4b), DIMENSION(6,10), PARAMETER :: fundArg = &
       reshape( source =                                &
       (/-1, 0,-2, 0,-1, 1,  &
         -1, 0,-2, 0,-2, 1,  &
          1, 0,-2,-2,-2, 1,  &
          0, 0,-2, 0,-1, 1,  &
          0, 0,-2, 0,-2, 1,  &
         -1, 0, 0, 0, 0, 1,  &
          0, 0,-2, 2,-2, 1,  &
          0, 0, 0, 0, 0, 1,  &
          0, 0, 0, 0,-1, 1,  &
          1, 0, 0, 0, 0, 1/),&
        shape = (/6,10/) )

! IERS TN No.32, Ch. 5.4.2, Tab.5.1a, only n=2
! --------------------------------------------
  REAL(r8b), DIMENSION(4,10), PARAMETER :: subNut =  &
       reshape( source =                             &
       (/-0.44D-6,   0.25D-6,  -0.25D-6,  -0.44D-6,  &
         -2.31D-6,   1.32D-6,  -1.32D-6,  -2.31D-6,  &
         -0.44D-6,   0.25D-6,  -0.25D-6,  -0.44D-6,  &
         -2.14D-6,   1.23D-6,  -1.23D-6,  -2.14D-6,  &
        -11.36D-6,   6.52D-6,  -6.52D-6, -11.36D-6,  &
          0.84D-6,  -0.48D-6,   0.48D-6,   0.84D-6,  &
         -4.76D-6,   2.73D-6,  -2.73D-6,  -4.76D-6,  &
         14.27D-6,  -8.19D-6,   8.19D-6,  14.27D-6,  &
          1.93D-6,  -1.11D-6,   1.11D-6,   1.93D-6,  &
          0.76D-6,  -0.43D-6,   0.43D-6,   0.76D-6/),&
        shape = (/4,10/) )

! IERS2010 Conventions:
! ---------------------
! => coefficients for subNut are included in IERS2010XY.SUB
! IERS TN No.36, Ch. 5.5.1, Table 5.1a, only n=2

! Local variables
! ---------------
  REAL(r8b),DIMENSION(6),SAVE :: erpSav
  REAL(r8b)    :: tu
  REAL(r8b)    :: period
  REAL(r8b)    :: arg, argr
  INTEGER(i4b) :: iSub
  INTEGER(i4b) :: iNut

  REAL(r8b),DIMENSION(3)           :: tstSUB

  LOGICAL, SAVE     :: first = .TRUE.
  CHARACTER(LEN=chrValLength)       :: chrVal
  CHARACTER(LEN=8)  :: srNget
  INTEGER(i4b),SAVE :: subFmt
  REAL(r8b)         :: numVal

  IF (first) THEN
    first=.FALSE.

! Get SUB-file format
! -------------------
    CALL getModKey(mod_orb_subFmt,chrVal,srNget,numVal)
    subFmt = IDNINT(numVal)
  ENDIF

! RETURN IF EOPCH DID NOT CHANGE (TOLERANCE OF 1 SEC)
! ------------------------------
  IF (DABS(eposav-xtdb) <= 1D-5) THEN
    erpsub=erpsav(1:3)
    erpsur=erpsav(4:6)
    RETURN
  ENDIF

! INITIALIZATION
! --------------
!  TU=(xtdb-51544.5D0)/36525.D0

  erpsub(1:3)=0.D0
  erpsur(1:3)=0.D0

! COMPUTE SUBDAILY CORRECTIONS AND THEIR RATES
! --------------------------------------------
  DO iSub=nSub,1,-1

    CALL subval(xtdb,subfar,submlt(1,iSub),arg,argr,period)

! new computation, valid for both possible SUB-file formats
       erpsub(1)=erpsub(1)+subcoe(1,iSub)*DCOS(arg) &
                          +subcoe(2,iSub)*DSIN(arg)
       erpsub(2)=erpsub(2)+subcoe(3,iSub)*DCOS(arg) &
                          +subcoe(4,iSub)*DSIN(arg)
       erpsub(3)=erpsub(3)+subcoe(5,iSub)*DCOS(arg) &
                          +subcoe(6,iSub)*DSIN(arg)

       erpsur(1)=erpsur(1)-subcoe(1,iSub)*DSIN(arg)*argr &
                          +subcoe(2,iSub)*DCOS(arg)*argr
       erpsur(2)=erpsur(2)-subcoe(3,iSub)*DSIN(arg)*argr &
                          +subcoe(4,iSub)*DCOS(arg)*argr
       erpsur(3)=erpsur(3)-subcoe(5,iSub)*DSIN(arg)*argr &
                          +subcoe(6,iSub)*DCOS(arg)*argr

!!!!!!!!!!!old
!!    erpsub(1)=erpsub(1)-subcoe(1,iSub)*DCOS(arg) &
!!                       +subcoe(2,iSub)*DSIN(arg)
!!    erpsub(2)=erpsub(2)+subcoe(2,iSub)*DCOS(arg) &
!!                       +subcoe(1,iSub)*DSIN(arg)
!!    erpsub(3)=erpsub(3)+subcoe(3,iSub)*DCOS(arg) &
!!                       +subcoe(4,iSub)*DSIN(arg)
!!
!!    erpsur(1)=erpsur(1)+subcoe(1,iSub)*DSIN(arg)*argr &
!!                       +subcoe(2,iSub)*DCOS(arg)*argr
!!    erpsur(2)=erpsur(2)-subcoe(2,iSub)*DSIN(arg)*argr &
!!                       +subcoe(1,iSub)*DCOS(arg)*argr
!!    erpsur(3)=erpsur(3)-subcoe(3,iSub)*DSIN(arg)*argr &
!!                       +subcoe(4,iSub)*DCOS(arg)*argr
!!!!!!!!!!!old

  ENDDO

! Nutational corrections of pole coordinates
! -----------------------------------------------------------------
  IF (subFmt == 1) THEN
    DO iNut = 10,1,-1
      CALL subval(xtdb,subfar,fundarg(1:6,iNut),arg,argr,period)

      erpsub(1)=erpsub(1)+subNut(1,iNut)*DSIN(arg) &
                         +subNut(2,iNut)*DCOS(arg)
      erpsub(2)=erpsub(2)+subNut(3,iNut)*DSIN(arg) &
                         +subNut(4,iNut)*DCOS(arg)

      erpsur(1)=erpsur(1)+subNut(1,iNut)*DCOS(arg)*argr &
                         -subNut(2,iNut)*DSIN(arg)*argr
      erpsur(2)=erpsur(2)+subNut(3,iNut)*DCOS(arg)*argr &
                         -subNut(4,iNut)*DSIN(arg)*argr
    ENDDO
  ENDIF

! Save values for next call
! -------------------------
  eposav=xtdb
  erpsav(1:3)=erpsub
  erpsav(4:6)=erpsur

  RETURN
END SUBROUTINE submod
END MODULE
