! ------------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! ------------------------------------------------------------------------------

MODULE d_rinex3

! ------------------------------------------------------------------------------
! Purpose:    This module defines structures and global variables for
!             RINEX 3.0 files
!
! Author:     A.Gaede
!
! Created:    29-Feb-2007
!
! Changes:    20-Jan-2011 LP,SL: maxtyp defined here, DUMMY OBSTYPES added
!             26-Jan-2011 LP: indxs, t_satsig and t_gobdef added for sat-
!                             specific observation selection
!             16-Sep-2011 EO/SL: maxtyp extended to 26, to cover all observation
!                                types defined in RINEX 2.12.
!             21-Mar-2012 SL: maxtyp extended to 42, use MAXSYS from m_global
!             24-Apr-2012 LP: dimension of obstyp in t_satsig changed 4->8
!             01-May-2012 LP: functions RIN2RIN3 and RIN3RIN2 added;
!                             obslist and r32r2 removed;maxtyp 42->98
!             23-May-2012 LP: Scale factor for observation types added to t_satsig
!             26-Jun-2012 LP: Set rnxvers to 3.02
!             20-Aug-2012 LP: Bugfix: obsindR212 index added to t_satsig for RINEX2.12
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! ------------------------------------------------------------------------------

  USE m_bern,    ONLY: i4b, r8b
  USE m_global,  ONLY: MAXSYS
  IMPLICIT NONE

! Global Paramters
! ----------------
  INTEGER(i4b),PARAMETER           :: maxtyp = 110   ! Max. number of possible obstypes
                                                     ! (adapt OBSTYPESR2 and
                                                     ! OBSTYPESR3 with true obstype codes,
                                                     ! and r32r2 with dummies!)
  REAL(r8b),PARAMETER              :: rnxvers = 3.02 ! Latest supported RINEX3 version
! only for version 5.0
  INTEGER(i4b), DIMENSION(0:MAXSYS-1,maxtyp)    :: indx
  INTEGER(i4b), DIMENSION(0:MAXSYS-1,49,4)      :: indxs

! priority list for observation types from RINEX 3 to RINEX 2
! -----------------------------------------------------------
  CHARACTER(LEN=2), DIMENSION(maxtyp)  :: OBSTYPESR2 = &
      (/'C1' ,'C2' ,'C5' ,'P1' ,'P2' ,'L1' ,'L2' ,'L5' ,'S1' ,'S2' , &
        'S5' ,'D1' ,'D2' ,'D5' ,'C6' ,'L6' ,'S6' ,'D6' ,'C7' ,'L7' , &
        'S7' ,'D7' ,'C8' ,'L8' ,'S8' ,'D8' ,'CA' ,'LA' ,'SA' ,'DA' , &
        'CB' ,'LB' ,'SB' ,'DB' ,'CC' ,'LC' ,'SC' ,'DC' ,'CD' ,'LD' , &
        'SD' ,'DD' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' , &
        '  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' , &
        '  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' , &
        '  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' , &
        '  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' , &
        '  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' , &
        '  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' ,'  ' /)
  CHARACTER(LEN=3), DIMENSION(maxtyp)  :: OBSTYPESR3 = &
      (/'C1C','C2C','C5I','C1P','C2P','L1P','L2P','L5I','S1P','S2P', &
        'S5I','D1P','D2P','D5I','C6A','L6A','S6A','D6A','C7I','L7I', &
        'S7I','D7I','C8I','L8I','S8I','D8I','C1C','L1C','S1C','D1C', &
        'C1C','L1C','S1C','D1C','C2C','L2C','S2C','D2C','C2C','L2C', &
        'S2C','D2C','C1A','L1A','S1A','D1A','C1B','L1B','S1B','D1B', &
        'C1X','L1X','S1X','D1X','C5Q','L5Q','S5Q','D5Q','C5X','L5X', &
        'S5X','D5X','C6B','L6B','S6B','D6B','C6X','L6X','S6X','D6X', &
        'C7Q','L7Q','S7Q','D7Q','C7X','L7X','S7X','D7X','C8Q','L8Q', &
        'S8Q','D8Q','C8X','L8X','S8X','D8X','C2I','L2I','S2I','D2I', &
        'C2Q','L2Q','S2Q','D2Q','C2X','L2X','S2X','D2X','C1W','L1W', &
        'S1W','D1W','C2W','L2W','S2W','D2W','C2D','L2D','S2D','D2D'/)

  CHARACTER(LEN=2), DIMENSION(4)  :: obslistbasic = &
      (/'P1','P2','L1','L2'/)


! Structures
! ==========

! Antenna phase center
! --------------------
!  TYPE t_phase
!    CHARACTER(LEN=1)                       :: satcod  !Satellite system
!    REAL(r8b), DIMENSION(:,3),POINTER      :: antpha  !Average antenna phase center
!  END TYPE t_phase

! Observation types
! -----------------
  TYPE t_obstyp
    INTEGER(i4b)                           :: obsnum
    CHARACTER(LEN=3),DIMENSION(:),POINTER  :: obstyp
    INTEGER(i4b),DIMENSION(:),POINTER      :: factor
    REAL(r8b), DIMENSION(:,:),POINTER      :: antpha
    INTEGER(i4b),DIMENSION(:,:),POINTER    :: numobs
  END TYPE t_obstyp

! DCBs applied
! ------------
  TYPE t_dcb
    CHARACTER(LEN=17)                      :: prognam
    CHARACTER(LEN=40)                      :: source
  END TYPE t_dcb

! PCVs applied
! ------------
  TYPE t_pcv
    CHARACTER(LEN=17)                      :: prognam
    CHARACTER(LEN=40)                      :: source
  END TYPE t_pcv

! Number of observations
! ----------------------
!  TYPE t_numobs
!    CHARACTER(LEN=1)                       :: satcod
!    INTEGER(i4b)                           :: prn
!    INTEGER(i4b),DIMENSION(:),POINTER      :: numobs
!  END TYPE t_numobs

! RINEX 3.0 Header
! ----------------

  TYPE t_rxohead
    REAL(r8b)                              :: rnxvers !Version of RINEX file
    CHARACTER(LEN=1)                       :: filtyp  !File type
    CHARACTER(LEN=1)                       :: satsys  !satellite system
    CHARACTER(LEN=20)                      :: prognam !Program creating current file
    CHARACTER(LEN=20)                      :: runby   !Agency creating current file
    CHARACTER(LEN=20)                      :: crdate  !Time of creation
!    CHARACTER(LEN=60),DIMENSION(:),POINTER :: comment !Comment lines
    CHARACTER(LEN=60)                      :: mrknam  !Antenna marker name
    CHARACTER(LEN=20)                      :: mrknum  !Antenna marker number
    CHARACTER(LEN=20)                      :: mrktyp  !Antenna marker type
    CHARACTER(LEN=20)                      :: observ  !Observer name
    CHARACTER(LEN=40)                      :: agency  !Agency name
    CHARACTER(LEN=20)                      :: recnum  !Receiver number
    CHARACTER(LEN=20)                      :: rectyp  !Receiver type
    CHARACTER(LEN=20)                      :: recvers !Receiver version
    CHARACTER(LEN=20)                      :: antnum  !Antenna number
    CHARACTER(LEN=20)                      :: anttyp  !Antenna type
    REAL(r8b), DIMENSION(3)                :: aprpos  !Aproximate marker position
    REAL(r8b), DIMENSION(3)                :: anthen  !Antenna delta H/E/N
    REAL(r8b), DIMENSION(3)                :: antxyz  !Antenna delta X/Y/Z
!    TYPE(t_phase),DIMENSION(:),POINTER     :: phase   !Structure for antenna phase center
    REAL(r8b), DIMENSION(3)                :: antbore !Antenna bore sight vector
    REAL(r8b)                              :: zeroazi !Azimut of zero-direction
    REAL(r8b), DIMENSION(3)                :: zeroxyz !Zero-direction of antenna
    REAL(r8b), DIMENSION(3)                :: comxyz  !Center of mass of vehicle
!    INTEGER(i4b)                           :: nobstyp !# of satellite systems
    TYPE(t_obstyp),DIMENSION(0:(MAXSYS-1)) :: otyp    !Structure for observation types
    CHARACTER(LEN=20)                      :: streng  !Signal strength
    REAL(r8b)                              :: obsint  !Observation interval
    REAL(r8b)                              :: tfirst  !Time of first observation
    REAL(r8b)                              :: tlast   !Time of last observation
    CHARACTER(LEN=3)                       :: timsys  !Time system of first and last obs
    INTEGER(i4b)                           :: recloff !Receiver clock offset applied
    TYPE(t_dcb),DIMENSION(:),POINTER       :: dcb     !Line for DCBs applied
    TYPE(t_pcv),DIMENSION(:),POINTER       :: pcv     !Line for PCV model
    INTEGER(i4b)                           :: LeapSec !Leap second
    INTEGER(i4b)                           :: nsat    !# of satellites
!    TYPE(t_numobs),DIMENSION(:),POINTER    :: nobs    !# of observations
    INTEGER(i4b)                           :: end     !End of file flag
  END TYPE t_rxohead

  TYPE(t_rxohead)                          :: rxohead

! observation records
! -------------------
  TYPE t_obs
    CHARACTER(LEN=1)                       :: satcod
    INTEGER(i4b)                           :: prn
    REAL(r8b),DIMENSION(:),POINTER         :: obs
    INTEGER(i4b),DIMENSION(:),POINTER      :: lli
    INTEGER(i4b),DIMENSION(:),POINTER      :: streng
  END TYPE t_obs

! RINEX 3.0 data records
! ----------------------

  TYPE t_rxoobs
    real(r8b)                              :: epoch   !Epoch for data records
    INTEGER(i4b)                           :: flag    !Epoch flag
    INTEGER(i4b)                           :: nusat   !# of satellites observed
    REAL(r8b)                              :: recloff !Receiver clock offset
    TYPE(t_obs),DIMENSION(:),POINTER       :: obsrec  !observation records
  END TYPE t_rxoobs

  TYPE(t_rxoobs)                           :: rxoobs


! Satellite-specific observation type selection
! ---------------------------------------------
  TYPE t_satsig
    CHARACTER(LEN=3)                       :: satname !Name of satellite in RNX (e.g., E16)
    INTEGER(i4b)                           :: sysnum  !Index of sat. system (0..(maxsys-1))
    CHARACTER(LEN=1)                       :: syschar !Ident. char. of sat. sys. (G,R,E,S,J,C)
    INTEGER(i4b)                           :: satnum  !PRN of satellite (1..32)
    CHARACTER(LEN=3),DIMENSION(8)          :: obstyp  !Code+phase obs to be used
    CHARACTER(LEN=2),DIMENSION(8)          :: obstyp2 !Available code+phase channels
    INTEGER(i4b),DIMENSION(8)              :: obsindR212!Index of RINEX2 obstypes in OBSTYPESR2
    INTEGER(i4b),DIMENSION(8)              :: factor  !Scale factor for observation types
    INTEGER(i4b)                           :: nfreqc  !Number of code freqs.
    INTEGER(i4b)                           :: nfreqp  !Number of phase freqs.
    INTEGER(i4b)                           :: eposatind!Ind. of sat. in epoch (overwrite each epoch)
  END TYPE t_satsig

  TYPE t_gobsdef
    INTEGER(i4b)                           :: norec   !# of records
    TYPE(t_satsig),DIMENSION(:),POINTER    :: sat     !One record for each sat.
    INTEGER(i4b)                           :: nfreqc  !Number of code freqs.
    INTEGER(i4b)                           :: nfreqp  !Number of phase freqs.
  END TYPE t_gobsdef

! RINEX observation type statistics
! ---------------------------------
  TYPE t_satstat
    CHARACTER(LEN=3)                       :: satname !Name of satellite in RNX (e.g., E16)
    INTEGER(i4b)                           :: satnum  !PRN of satellite (1..32)
    INTEGER(i4b)                           :: eposatind!Ind. of sat. in epoch (overwrite each epoch)
    INTEGER(i4b),DIMENSION(MAXTYP)         :: numobs  !Number of observations per satellite and obstype
    INTEGER(i4b)                           :: obssum  !Sum of observations for this satellite
  END TYPE t_satstat

  TYPE t_sysstat
    CHARACTER(LEN=1)                       :: syschar !Ident. char. of sat. sys. (G,R,E,S,J,C)
    INTEGER(i4b)                           :: sysnum  !Index of sat. system (0..(maxsys-1))
    INTEGER(i4b),DIMENSION(MAXTYP)         :: indxs   !Index of obstype per satsys
    INTEGER(i4b),DIMENSION(MAXTYP)         :: numobs  !Number of observations per satsys and obstype
    TYPE(t_satstat),DIMENSION(49)          :: sat     !One record for each sat.
    INTEGER(i4b)                           :: obssum  !Sum of observations for this satsys
  END TYPE t_sysstat

  TYPE t_rinstat
    TYPE(t_sysstat),DIMENSION(0:(MAXSYS-1)):: sys     !One record for each satsys
    INTEGER(i4b),DIMENSION(MAXTYP)         :: numobs  !Number of observations per obstype
    INTEGER(i4b)                           :: obssum  !Sum of observations for this station
    INTEGER(i4b)                           :: numsat  !Number of satellites in record
  END TYPE t_rinstat

! Galileo external observation selection
! --------------------------------------

CONTAINS

! Initialize structure
! --------------------
  SUBROUTINE init_rxohead(rxohead)
    TYPE(t_rxohead)         :: rxohead
    INTEGER(i4b)            :: isys

    rxohead%rnxvers = 0d0
    rxohead%filtyp  = ""
    rxohead%satsys  = ""
    rxohead%prognam = ""
    rxohead%runby   = ""
    rxohead%crdate  = ""
    rxohead%mrknam  = ""
    rxohead%mrknum  = ""
    rxohead%mrktyp  = ""
    rxohead%observ  = ""
    rxohead%agency  = ""
    rxohead%recnum  = ""
    rxohead%rectyp  = ""
    rxohead%recvers = ""
    rxohead%antnum  = ""
    rxohead%anttyp  = ""
    rxohead%aprpos  = 0d0
    rxohead%anthen  = 0d0
    rxohead%antxyz  = 0d0
!    NULLIFY(rxohead%phase)
    rxohead%antbore = 0d0
    rxohead%zeroazi = 0d0
    rxohead%zeroxyz = 0d0
    rxohead%comxyz  = 0d0
!    rxohead%nobstyp = 0
    rxohead%otyp%obsnum = 0
    DO isys=0,MAXSYS-1
      NULLIFY(rxohead%otyp(isys)%obstyp)
      NULLIFY(rxohead%otyp(isys)%factor)
      NULLIFY(rxohead%otyp(isys)%antpha)
      NULLIFY(rxohead%otyp(isys)%numobs)
    ENDDO
    rxohead%streng  = ""
    rxohead%obsint  = 0d0
    rxohead%tfirst  = 0d0
    rxohead%tlast   = 0d0
    rxohead%timsys  = ""
    rxohead%recloff = 0
    NULLIFY(rxohead%dcb)
    NULLIFY(rxohead%pcv)
    rxohead%LeapSec = 0
    rxohead%nsat    = 0
!    NULLIFY(rxohead%nobs)
    rxohead%end     = 0
  END SUBROUTINE init_rxohead

  SUBROUTINE init_rxoobs(rxoobs)
    TYPE(t_rxoobs)          :: rxoobs

    rxoobs%epoch   = 0d0
    rxoobs%flag    = 0
    rxoobs%nusat   = 0
    rxoobs%recloff = 0d0
    NULLIFY(rxoobs%obsrec)
  END SUBROUTINE init_rxoobs

! -----------------------------------------------------------------------------
! Transform RINEX2 obs codes (2 char) to RINEX3 obs codes (3 char)
! -----------------------------------------------------------------------------
  FUNCTION RIN2RIN3(OBSTR2)
    ! List of Parameters
    ! input:
    CHARACTER(LEN=2)          :: OBSTR2 ! RINEX2 obstype

    ! output:
    CHARACTER(LEN=3)          :: RIN2RIN3 ! RINEX3 obstype

    ! local:
    INTEGER(i4b) :: ityp

    RIN2RIN3='   '
    DO ityp=1,maxtyp
      IF (OBSTYPESR2(ityp).EQ.OBSTR2) THEN
        RIN2RIN3=OBSTYPESR3(ityp)
        EXIT
      ENDIF
    ENDDO
  END FUNCTION RIN2RIN3

! -----------------------------------------------------------------------------
! Transform RINEX3 obs codes (3 char) to RINEX2 obs codes (2 char)
! -----------------------------------------------------------------------------
  FUNCTION RIN3RIN2(OBSTR3)
    ! List of Parameters
    ! input:
    CHARACTER(LEN=3)          :: OBSTR3 ! RINEX3 obstype

    ! output:
    CHARACTER(LEN=2)          :: RIN3RIN2 ! RINEX2 obstype

    ! local:
    INTEGER(i4b) :: ityp

    RIN3RIN2='  '
    DO ityp=1,maxtyp
      IF (OBSTYPESR3(ityp).EQ.OBSTR3) THEN
        RIN3RIN2=OBSTYPESR2(ityp)
        EXIT
      ENDIF
    ENDDO
  END FUNCTION RIN3RIN2



END MODULE d_rinex3
