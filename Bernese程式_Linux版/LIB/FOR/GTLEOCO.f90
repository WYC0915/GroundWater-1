MODULE s_GTLEOCO
CONTAINS

! -----------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -----------------------------------------------------------------------------

  SUBROUTINE gtleoco(leoname,epo,ityp,ider,xleo,sz,xpol,ypol,ircode,cmcyn)

! -----------------------------------------------------------------------------
! Purpose    :  Get LEO coordinates (center of mass) in different systems from
!               SR: GETORF
!
! Author     :  D. Svehla
!
! Created    :  28-Feb-2001
! Last mod.  :  14-Nov-2011
!
! Changes    :  16-Dec-2001 HU: Use implicit none
!               10-Oct-2002 DS: Allow for several LEOs
!               12-Nov-2002 HB: Use F90-Standards and set return code in
!                               SR getorf
!               28-Jul-2005 RD: Open string length for leoname
!               29-Apr-2008 LP: Use CMC info from SR getorf
!               16-Dec-2010 RD: CMC for ATL added
!               14-Nov-2011 HB: Two calls for SR GETORF due
!                               to cmc modifications
!
! SR used    :  cootra, ddreh, dmlmav, getorf, gtflna, leoprn
!
! Remarks    :  sz,xpol,ypol are returned only if ityp=1 or 2
!
! Copyright  :  Astronomical Institute
!               University of Bern
!               Switzerland
! -----------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE s_cootra
  USE s_truearth
  USE s_getorf
  USE s_leoprn
  USE s_gtflna
  IMPLICIT NONE

! List of Parameters
! ------------------
! IN :
! ----
  CHARACTER(LEN=*)                :: leoname! LEO name
  REAL(r8b)                       :: epo    ! Epoch in MJD
  INTEGER(i4b)                    :: ityp   ! Type of system
                                            !  =0:B1950 or J2000.0
                                            !  =1:true equator. system of epoch
                                            !  =2:earth-fixed system
  INTEGER(i4b)                    :: ider   ! Return:
                                            !  =0:only position
                                            !  =1:position+velocity
                                            !  =2:position+velocity+acceleration
! OUT:
! ----
  REAL(r8b),DIMENSION(*)          :: xleo   ! Position (m), velocity (m/s),
                                            !   acceleration(m/s**2)
  REAL(r8b)                       :: sz     ! True sideral time
  REAL(r8b)                       :: xpol   ! Pole coordinates
  REAL(r8b)                       :: ypol   !   in (rad)
  INTEGER(i4b)                    :: ircode ! Return code
                                            !  =0:OK
                                            !  =1:no arc found for epoch
                                            !  =2:arc found, but no LEO
                                            !     in this arc
  LOGICAL, DIMENSION(2), OPTIONAL :: cmcyn  ! Apply CMC correction (y/n)


! List of Functions
! -----------------
! Local Types
! -----------
! Local Parameters
! ----------------

! Local variables
! ---------------
  INTEGER(i4b),      SAVE         :: leosvn
  INTEGER(i4b)                    :: irc,iorsys,icrarc

  REAL(r8b)                       :: tosc,ut1gps
  REAL(r8b),DIMENSION(7)          :: ele

  CHARACTER(LEN=fileNameLength), SAVE   :: leostd
  CHARACTER(LEN=staNameLength),  SAVE   :: leoold

  LOGICAL, DIMENSION(2), SAVE           :: tcmcyn
  LOGICAL,               SAVE           :: first= .TRUE.


  tcmcyn =.FALSE.


! If called for the first time get LEO standard orbit file
! ========================================================
  IF (first) THEN
    first = .FALSE.
    leoold(:)=' '
!
! Get filename of standard orbit file
! -----------------------------------
    CALL GTFLNA(1,'LEOSTD ',leostd,irc)
  END IF
!
! Get LEO number
! --------------
  IF (leoname.NE.leoold) THEN
    CALL LEOPRN(leoname,epo,leosvn)
    leoold(:)=leoname(:)
  END IF

! Get LEO coordinates in inertial system B1950.0 or J2000.0
! =========================================================
  IF (PRESENT(cmcyn)) THEN
    CALL getorf(leostd,leosvn,0,ider,0,epo,icrarc,iorsys,&
         xleo,tosc,ele,ircode,tcmcyn)
  ELSE
    CALL getorf(leostd,leosvn,0,ider,0,epo,icrarc,iorsys,&
         xleo,tosc,ele,ircode)
  END IF

! CMC information
! ===============
  IF (PRESENT(cmcyn)) cmcyn=tcmcyn

  IF (ityp == 0.OR.ircode /= 0) RETURN

! Transform into true equatorial system of epoch
! ==============================================
  CALL cootra(iorsys,0,epo,xleo,sz,xpol,ypol,ut1gps)
  IF (ityp == 1) RETURN

! Transform into earth-fixed system
! ==============================================
  CALL truearth(xleo,xleo,0,ider,sz,xpol,ypol)
  IF (ityp==2) RETURN

! END
! ---
  END SUBROUTINE gtleoco

END MODULE
