! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  MODULE p_leoaux

! -------------------------------------------------------------------------
! Purpose:   Module for the LEO-Auxiliary file
!
! Author:    H.Bock
!
! Created:   25-Jul-2000
! Last mod.: 12-Jul-2011
!
! Changes:   29-Jan-2003 HB: Add versio and revis to t_chphead
!            03-Jan-2005 HB: Add concat to t_leoaux_opt
!            11-Mar-2008 HB: Remove concat from t_leoaux_opt
!                            add strqua and str_id to t_leoaux_opt
!            12-Jul-2011 HB: Remove str_id from t_leoaux_opt, because it
!                            is no longer needed
!
! Copyright: Astronomical Institute
!            University of Bern
!            Switzerland
! -----------------------------------------------------------------------

! Modules
! -------
  USE m_bern

! Local Parameters
! ---------------
  INTEGER(i4b), PARAMETER :: maxTyp=12
  INTEGER(i4b), PARAMETER :: maxacc=10

! Options
! -------
  TYPE t_leoaux_opt
    CHARACTER(LEN=shortLineLength) :: title
    CHARACTER(LEN=5)       :: satNam
    REAL(r8b),DIMENSION(2) :: window
    INTEGER(i4b),DIMENSION(maxTyp) :: typLst
    INTEGER(i4b)           :: ixyz
    INTEGER(i4b)           :: strqua
  END TYPE t_leoaux_opt

! Possible data types
! -------------------
  TYPE t_chpdatTyp
    CHARACTER(LEN=3)  :: typ  ! data type
    CHARACTER(LEN=3)  :: ref  ! reference system
    CHARACTER(LEN=50) :: form ! format of data type
  END TYPE t_chpdatTyp

  TYPE t_chphead
    CHARACTER(LEN=3)                 :: versio ! version
    CHARACTER(LEN=7)                 :: satNum ! satellite number
    CHARACTER(LEN=20)                :: satNam ! satellite name
    CHARACTER(LEN=27)                :: timfst ! first epoch (string)
    CHARACTER(LEN=27)                :: timlst ! last epoch (string)
    INTEGER(i4b),DIMENSION(maxTyp)   :: flgTyp ! available data types
    type(t_chpdatTyp),DIMENSION(maxTyp) :: datTyp ! data types
    REAL(r8b),DIMENSION(3,3)         :: aclk   ! calibration data of acl
    REAL(r8b),DIMENSION(3,3)         :: acak   ! calibration data of aca
    INTEGER(i4b)                     :: revis  ! revision number of version
    INTEGER(i4b),DIMENSION(3)        :: faclk  ! flag for acl calibration
    INTEGER(i4b),DIMENSION(3)        :: facak  ! flag for aca calibration
    REAL(r8b)                        :: mass   ! mass of satellite
    REAL(r8b),DIMENSION(4)           :: asc2sc ! attitude calibration data
    CHARACTER(LEN=3)                 :: filt   ! filter for smoothing
  END TYPE t_chphead

! Miscellaneous data of LEO
! -------------------------
  TYPE t_chpdata
    INTEGER(i4b),DIMENSION(maxTyp)    :: flgdat ! flags for available types
    CHARACTER(LEN=27)                 :: timstr ! epoch (as string)
    REAL(r8b),DIMENSION(3)            :: acl    ! linear accelerations
    INTEGER(i4b)                      :: sacl   ! sampling of acl
    REAL(r8b),DIMENSION(3)            :: aca    ! angular accelerations
    INTEGER(i4b)                      :: saca   ! sampling of aca
    REAL(r8b),DIMENSION(maxacc,3)     :: acc    ! acceleration corrections
    INTEGER(i4b),DIMENSION(maxacc,2)  :: facc   ! type of correction
    REAL(r8b),DIMENSION(4)            :: att    ! attitude (quaternions)
    INTEGER(i4b),DIMENSION(4)         :: starfl ! flags for star imager
    INTEGER(i4b),DIMENSION(14)        :: thrflg ! flags for thrusters
    REAL(r8b)                         :: quatt  ! quality of attitude
    REAL(r8b)                         :: thrpul ! duration of thruster pulses
    REAL(r8b),DIMENSION(4)            :: hk1    ! house-keeping
    REAL(r8b),DIMENSION(4)            :: hk2    ! house-keeping
    INTEGER(i4b),DIMENSION(6)         :: flhk3  ! house-keeping
    INTEGER(i4b)                      :: nbc    ! house-keeping
    REAL(r8b),DIMENSION(4)            :: hk3    ! house-keeping
    REAL(r8b),DIMENSION(5)            :: hk4    ! house-keeping
    REAL(r8b)                         :: tmpacc ! temperature of acc sensor cage
    REAL(r8b),DIMENSION(3)            :: fgm    ! magnetic field vectors
    INTEGER(i4b)                      :: fgmnr  ! FGM number
  END TYPE t_chpdata

  type(t_chphead),SAVE :: head

  INTERFACE init
    MODULE PROCEDURE initia
  END INTERFACE

  CONTAINS

    SUBROUTINE initia(head)
      IMPLICIT NONE
      type(t_chphead),INTENT(INOUT) :: head
      LOGICAL,SAVE   :: first=.TRUE.
      IF (first) THEN
        first=.FALSE.
        head%datTyp(1)%typ= 'tim'
        head%datTyp(2)%typ= 'acl'
        head%datTyp(3)%typ= 'aca'
        head%datTyp(4)%typ= 'acc'
        head%datTyp(5)%typ= 'att'
        head%datTyp(6)%typ= 'thr'
        head%datTyp(7)%typ= 'hk1'
        head%datTyp(8)%typ= 'hk2'
        head%datTyp(9)%typ= 'hk3'
        head%datTyp(10)%typ='hk4'
        head%datTyp(11)%typ='hka'
        head%datTyp(12)%typ='fgm'
!          head%datTyp(1)%form= '(A3,1X,I4,1X,I2,1X,I2,1X,I2,1X,I2,1X,F10.7)'
        head%datTyp(1)%form= '(A3,1X,A27)'
        head%datTyp(2)%form= '(A3,5X,3(1X,F13.10),1X,I2)'
        head%datTyp(3)%form= '(A3,5X,3(1X,F13.10),1X,I2)'
        head%datTyp(4)%form= '(A3,1X,I2,1X,I1,3(1X,F13.10))'
        head%datTyp(5)%form= '(A3,1X,4(I1),4(1X,F13.10),1X,F5.2)'
        head%datTyp(6)%form= '(A3,1X,14(I1),2X,F12.3)'
        head%datTyp(7)%form= '(A3,4(1X,F10.6))'
        head%datTyp(8)%form= '(A3,4(1X,F10.6))'
        head%datTyp(9)%form= '(A3,2X,6I1,1X,I2,4(1X,F10.6))'
        head%datTyp(10)%form= '(A3,5(1X,F10.6))'
        head%datTyp(11)%form= '(A3,5X,F7.3)'
        head%datTyp(12)%form= '(A3,3(1X,F10.1),1X,I1)'
! Initialization
        head%flgTyp(:) = 0
        head%asc2sc(:) = 0.D0
        head%aclk(:,:) = 0.D0
        head%acak(:,:) = 0.D0
        head%faclk(:)  = 2
        head%facak(:)  = 2
        head%satNum    = ''
        head%satNam    = ''
        head%satNum    = '0003902'
        head%satNam    = 'CHAMP'
      END IF
    END SUBROUTINE initia

 END MODULE p_leoaux
