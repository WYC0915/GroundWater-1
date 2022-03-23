! ------------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! ------------------------------------------------------------------------------

MODULE d_satfil

! ------------------------------------------------------------------------------
! Purpose:    This module defines structures and global variables for
!             the SR rdsatfil
!
! Author:     D. Svehla
!
! Created:    15-Mar-2001
! Last mod.:  19-Jul-2010
!
! Changes:    13-Jun-2001 HB: add ONLY : t_timint to USE m_time
!             13-May-2003 CU: Nullify pointers
!             27-Jul-2005 AG: SVN and ANTEX sensor name added
!             05-Aug-2005 AG: sensor type, number and signal list added
!             10-Jul-2006 AG: PCV model name added
!             17-Oct-2006 MP: rprmodel added
!             19-Jul-2010 SL: tab characters removed
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! ------------------------------------------------------------------------------

  USE m_bern
  USE m_time, ONLY : t_timint

! Global Parameers, strings for sensor type
! --------------------------------
  CHARACTER(LEN=4), SAVE           :: typeMWTR  = 'MW  '
  CHARACTER(LEN=4), SAVE           :: typeSLR   = 'SLR '
  CHARACTER(LEN=4), SAVE           :: typeDORI  = 'DORI'
  CHARACTER(LEN=4), SAVE           :: typeALT   = 'ALT '
  CHARACTER(LEN=4), SAVE           :: typePOD   = 'POD '
  CHARACTER(LEN=4), SAVE           :: typeOCC   = 'OCC '

  REAL(r8b),PARAMETER              :: satfilvers = 1.0D0

! Structures
! ----------

! PART 1: PHYSICAL SATELLITE PARAMETERS
! -------------------------------------

  TYPE t_satellite
    INTEGER(i4b)                :: svn     !Satellite number (PRN)
    CHARACTER(Len=4)            :: svnnr   !System flag with satellite number
                                           !(SVN)
    INTEGER(i4b)                :: iblock  !Block number
    CHARACTER(LEN=9)            :: cospar  !Cospar ID
    INTEGER(i4b)                :: attflag !Attitude flag
    TYPE(t_timint)              :: timint  !Time intervals in JULIAN DATE
                                           !Begin&end of interval (m_time)
    REAL(r8b)                   :: mass    !Satellite mass
    REAL(r8b)                   :: formf   !Form factor
    REAL(r8b)                   :: radpres !Radiation pressure coeff. C0
    INTEGER(i4b)                :: admodel !Air drag model
    REAL(r8b)                   :: adrag   !Air drag coeff.
    CHARACTER(LEN=3)            :: plane   !plane
  END TYPE t_satellite


! PART 2: ON-BOARD SENSORS
! ------------------------

  TYPE t_sensor
    INTEGER(i4b)                   :: svn     !Satellite number (PRN)
    CHARACTER(Len=4)               :: type    !Sensor type
    CHARACTER(LEN=staNam2Length)   :: sensor  !Sensor name
    INTEGER(i4b)                   :: numb    !special sensor number
    TYPE(t_timint)                 :: timint  !Time intervals in JULIAN DATE
                                              !Begin&end of interval (m_time)
    REAL(r8b),        DIMENSION(3) :: antoff  !Sensor offsets in
                                              !satellite reference frame
                                              !(see e.g. diss Feltens and
                                              !REMARK in SATELL file) (m)
    REAL(r8b),        DIMENSION(3) :: bore    !Sensor boresight vector
    REAL(r8b),        DIMENSION(3) :: azim    !Sensor reference azimuth vector
    CHARACTER(LEN=20)              :: name    !Antex sensor name
    INTEGER(i4b)                   :: ifrq    !Transmitted frequency
    INTEGER(i4b)                   :: nsigli  !Number of entries in sigli
    CHARACTER(LEN=3), DIMENSION(:), POINTER :: sigli   !List of signals
  END TYPE t_sensor


! PART 3: RPR-PARAMETERS
! ----------------------

  TYPE t_rprmod
    INTEGER(i4b)                     :: svn     !Satellite number (PRN)
    TYPE(t_timint)                   :: timint  !Time intervals in JULIAN DATE
                                                !Begin&end of interval (m_time)
    INTEGER(i4b)                     :: rpmodel !Radiation pressure model
    INTEGER(i4b)                     :: nrprcoe !Number of entries in rprcoe
    REAL(r8b),DIMENSION(:), POINTER  :: rprcoe  !Radiation pressure parameters
  END TYPE t_rprmod

! STRUCTURE: satfile
! -------------------

  TYPE t_satfil
    REAL(r8b)                      :: fvers   !Format version
    CHARACTER(LEN=10)              :: pcvmod  !PCV model name
    CHARACTER(LEN=8)               :: rpmodel !Radiation pressure model
                                              ! =T950101 : T-MODEL (scaled)
                                              ! =S950101 : S-MODEL (scaled)
                                              ! =Z950101 : Z-MODEL (scaled)
                                              ! =OLD     : S-MODEL (unscaled)
                                              ! =C980101 : CODE-MODEL (scaled)
                                              ! =J980101 : JPL-MODEL  (scaled)
                                              ! =C061001 : NEW CODE-MODEL
                                              !                       (scaled)
    INTEGER(i4b)                             :: nsatellite !Number of satellit.
    INTEGER(i4b)                             :: nsensor    !Number of sensors
    INTEGER(i4b)                             :: nrprmod    !Number of rpr-models
    TYPE(t_satellite), DIMENSION(:), POINTER :: satellite
    TYPE(t_sensor),    DIMENSION(:), POINTER :: sensor
    TYPE(t_rprmod),    DIMENSION(:), POINTER :: rprmod
  END TYPE t_satfil

! Initialize structure
! --------------------
CONTAINS

  SUBROUTINE init_satfil(satfil)
    TYPE(t_satfil)  :: satfil

    NULLIFY(satfil%satellite)
    NULLIFY(satfil%sensor)
    NULLIFY(satfil%rprmod)
    satfil%nsatellite = 0
    satfil%nsensor    = 0
    satfil%nrprmod    = 0
  END SUBROUTINE init_satfil

END MODULE d_satfil
