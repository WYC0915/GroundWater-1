
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE d_stacrx

! -------------------------------------------------------------------------
! Purpose   : This module defines structures and global variables for
!             the STACRUX file
!
! Author    : H. Bock
!
! Created   : 06-May-1999
! Last mod. : 01-Oct-2010
!
! Changes:    09-Feb-2001 DS: TYPE 5
!             12-Mar-2001 DS: m_stacrux renamed to d_stacrux
!             12-Mar-2001 DS: Use m_bern
!             12-Mar-2001 DS: Use staNameLength from m_bern
!             12-Mar-2001 DS: Define MTypeSPACE
!             15-Jun-2001 HB: d_stacrux renamed to d_stacrx,
!                             add ONLY. t_timint
!             22-Oct-2001 RD: Define parameters for undefined values in TYPE 2
!             20-Feb-2002 DS: Define MTypeAIR   for AIRBORNE
!             18-Sep-2002 DS: Define MTypeSHIP  for SHIP
!             18-Sep-2002 DS: Define MTypeGRD   for GROUND
!             10-Mar-2003 HU: descri added to t_stainfo
!             13-May-2003 CU: Nullify pointers
!             04-Aug-2008 DT: Add MTypeSLR for SLR station
!             20-Nov-2008 DT: Add version, technique to type stacrux;
!                             add CDPSOD to type stainfo
!             24-Jun-2009 DT: Changed Type 004 (relative constraints)
!                             t_coovel -> t_staRelPar
!             28-Jul-2009 DT: Add staRelPar%applied
!             29-Sep-2010 SL: rec and ant S/N added to t_staInfo, vers=1.01
!             01-Oct-2010 SL: use m_bern with ONLY, flg(INTEGER->CHARACTER),
!                             remark of TYPE 003 (LEN=24->60)
!             02-Nov-2010 SL: undef_c redefined (UNKNOWN->UNDEFINED)
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern, ONLY : i4b,r8b,staNameLength
  USE m_time, ONLY : t_timint

! Parameter
! =========
! Parameters indicating undefined values in a station info file:
  CHARACTER(LEN=20),PARAMETER :: undef_c = ' *** UNDEFINED ***  ' ! name, S/N
  INTEGER(i4b),     PARAMETER :: undef_i = 999999          ! rec/ant number
  REAL(r8b),        PARAMETER :: undef_e = 999.9999d0      ! ant-ecc.
  REAL(r8b),        PARAMETER :: undef_s = 99.99999d0      ! pos/vel sigma
  REAL(r8b),        PARAMETER :: undef_t = 99999.99999d0   ! relative constraint

! Global variables
! ================
  CHARACTER(LEN=20), SAVE           :: MTypeSPACE = 'SPACEBORNE          '
  CHARACTER(LEN=20), SAVE           :: MTypeAIR   = 'AIRBORNE            '
  CHARACTER(LEN=20), SAVE           :: MTypeSHIP  = 'SHIP                '
  CHARACTER(LEN=20), SAVE           :: MTypeGRD   = 'GROUND              '
  !! RD: "MTypeGRD" should be removed after CDCOMP update
  CHARACTER(LEN=20), SAVE           :: MTypeSLR   = 'SLR                 '

! Generally useful structures
! ===========================
! TYPE 001:
! ---------
  TYPE t_renamsta
    CHARACTER(LEN=staNameLength)    :: stanam
    CHARACTER(LEN=3)                :: flg
    TYPE(t_timint)                  :: timint
    CHARACTER(LEN=staNameLength)    :: oldnam
    CHARACTER(LEN=24)               :: remark
  END TYPE t_renamsta

! TYPE 002:
! ---------
  TYPE t_stainfo
    CHARACTER(LEN=staNameLength)    :: stanam
    CHARACTER(LEN=3)                :: flg
    TYPE(t_timint)                  :: timint
    CHARACTER(LEN=20)               :: recnam
    CHARACTER(LEN=20)               :: antnam
    CHARACTER(LEN=20)               :: recser
    CHARACTER(LEN=20)               :: antser
    INTEGER(i4b)                    :: recnum
    INTEGER(i4b)                    :: antnum
    REAL(r8b), DIMENSION(3)         :: antecc
    CHARACTER(LEN=22)               :: descri
    CHARACTER(LEN=24)               :: remark
    CHARACTER(LEN=8)                :: cdpsod  ! for SLR
  END TYPE t_stainfo

! TYPE 003:
! ---------
  TYPE t_staprob
    CHARACTER(LEN=staNameLength)    :: stanam
    CHARACTER(LEN=3)                :: flg
    TYPE(t_timint)                  :: timint
    CHARACTER(LEN=60)               :: remark
  END TYPE t_staprob

! TYPE 004: (for new format later: TYPE 006; remove t_coovel)
! ---------
  TYPE t_coovel
    CHARACTER(LEN=staNameLength), DIMENSION(2) :: stanam
    REAL(r8b),                    DIMENSION(6) :: constr
  END TYPE t_coovel

  TYPE t_staRelPar
    CHARACTER(LEN=staNameLength), DIMENSION(2) :: stanam
    CHARACTER(LEN=3)                           :: flg
    TYPE(t_timint)                             :: timWin
    CHARACTER(LEN=8)                           :: parTyp
    CHARACTER(LEN=3), DIMENSION(2)             :: sys     ! sys(1): Local Tie
                                                          ! sys(2): Constraint
    REAL(r8b), DIMENSION(3)                    :: locTie
    REAL(r8b), DIMENSION(3)                    :: constr
    REAL(r8b), DIMENSION(3)                    :: correl
    CHARACTER(LEN=24)                          :: remark
    LOGICAL                                    :: applied
  END TYPE t_staRelPar

! TYPE 005:
! ---------
  TYPE t_statype
    CHARACTER(LEN=staNameLength)    :: stanam
    CHARACTER(LEN=3)                :: flg
    TYPE(t_timint)                  :: timint
    CHARACTER(LEN=20)               :: markertype
    CHARACTER(LEN=24)               :: remark
  END TYPE t_statype

! STRUCTURE: STACRUX
! ------------------
  TYPE t_stacrux
    INTEGER(i4b)                            :: nrenam
    INTEGER(i4b)                            :: ninfo
    INTEGER(i4b)                            :: nprob
    INTEGER(i4b)                            :: ncoovel
    INTEGER(i4b)                            :: nrelpar
    INTEGER(i4b)                            :: nstatype
    REAL(r8b)                               :: version
    CHARACTER(LEN=30)                       :: technique
    TYPE(t_renamsta), DIMENSION(:), POINTER :: renamsta
    TYPE(t_stainfo),  DIMENSION(:), POINTER :: stainfo
    TYPE(t_staprob),  DIMENSION(:), POINTER :: staprob
    TYPE(t_coovel),   DIMENSION(:), POINTER :: coovel
    TYPE(t_staRelPar),DIMENSION(:), POINTER :: staRelPar
    TYPE(t_statype),  DIMENSION(:), POINTER :: statype
  END TYPE t_stacrux

CONTAINS

! -------------------------------------------------------------------------
! Initialize structure
! -------------------------------------------------------------------------

  SUBROUTINE init_stacrux(stacrux)

    TYPE(t_stacrux) :: stacrux

    NULLIFY(stacrux%renamsta)
    NULLIFY(stacrux%stainfo)
    NULLIFY(stacrux%staprob)
    NULLIFY(stacrux%coovel)
    NULLIFY(stacrux%staRelPar)
    NULLIFY(stacrux%statype)
    stacrux%nrenam   = 0
    stacrux%ninfo    = 0
    stacrux%nprob    = 0
    stacrux%ncoovel  = 0
    stacrux%nrelpar  = 0
    stacrux%nstatype = 0
    stacrux%version  = 1.01d0
    stacrux%technique= ''

  END SUBROUTINE init_stacrux

END MODULE d_stacrx
