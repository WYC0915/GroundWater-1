! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE s_GETSTA
CONTAINS

  SUBROUTINE GETSTA(nstat,stname,stanum,ncentr,icentr,    &
                    xstat,xstell,xstecc,                  &
                    datum,aell,bell,dxell,drell,scell)

! -------------------------------------------------------------------------
! Purpose:    Get coordinates and eccentricities for all
!             stations to be processed
!
! Author:     M.Rothacher
!
! Created:    03-Nov-1987
!
! Changes:    07-Oct-1991 ECCENTRICITIES IN XYZ ALSO POSSIBLE
!             11-May-1992 CHECK OF ECCTYP WAS ON WRONG PLACE
!             07-Dec-1992 ADD CHECK FOR "MAXSTA"
!             28-Dec-1992 USE OF SR "OPNFIL" TO OPEN FILES
!             04-Nov-1993 MR: READING OF LAT.,LONG.,HEIGHT CORRECTED
!             10-Aug-1994 MR: CALL EXITRC
!             02-May-2002 RD: USE SR FOR READING ECC FILE
!             15-May-2003 HB: INITIALIZE STRUCTURE
!             21-Jun-2005 MM: COMLFNUM.inc REMOVED, m_bern ADDED
!             23-Jun-2005 MM: IMPLICIT NONE AND DECLARATIONS ADDED
!             19-Jan-2011 RD: Transfer from F77 to F90
!             19-Jan-2011 RD: Wrapper to GETSTAF
!             19-Dec-2011 SL: use m_bern with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------
! Used modules:
  USE m_bern,    ONLY: i4b, r8b, staNameLength, fileNameLength
  USE s_gtflna
  USE s_getstaf
  IMPLICIT NONE

! List of parameters
! ------------------
! input:

! input/output:
  INTEGER(i4b)                               :: nstat  ! Number of stations
  CHARACTER(LEN=staNameLength), DIMENSION(:) :: stname ! Station names

! output:
  INTEGER(i4b),                 DIMENSION(:) :: stanum ! Station numbers
  INTEGER(i4b)                               :: ncentr ! Number of center
                                                       ! stations which are
                                                       ! not directly observed
  INTEGER(i4b),                 DIMENSION(:) :: icentr ! Index of the center
                                                       ! station for station
  REAL(r8b),                    DIMENSION(:,:):: xstat ! Rectangular
                                                       ! station coordinates
  REAL(r8b),                    DIMENSION(:,:)::xstell ! Ellipsoidal station
                                                       ! coordinates in speci-
                                                       ! fied datum
  REAL(r8b),                    DIMENSION(:,:)::xstecc ! Geocentric station ecc-
                                                       ! entricities (dx,dy,dz)
  CHARACTER(LEN=staNameLength)               :: datum  ! Local geodetic datum
  REAL(r8b)                                  :: aell   ! Semi-major axis of ell.
  REAL(r8b)                                  :: bell   ! Semi-minor axis of ell.
  REAL(r8b),                    DIMENSION(3) :: dxell  ! Shifts to WGS-84 (M)
  REAL(r8b),                    DIMENSION(3) :: drell  ! Rotational to WGS-84
  REAL(r8b)                                  :: scell  ! Scale factor between
                                                       ! local geodetic datum
                                                       ! and WGS-84

! Local variables
! ---------------
  CHARACTER(LEN=fileNameLength) :: crdnam,eccnam
  INTEGER(i4b)                  :: irc

! Get fielnames
! -------------
  CALL gtflna(1,'COORD  ',crdnam,irc)
  CALL gtflna(0,'ECCENT ',eccnam,irc)

! Read the coordinate file
! ------------------------
  CALL getstaf(crdnam,eccnam,nstat,stname,stanum,  &
               ncentr,icentr,xstat,xstell,xstecc,  &
               datum,aell,bell,dxell,drell,scell)

   RETURN

   END SUBROUTINE getsta

END MODULE s_getsta
