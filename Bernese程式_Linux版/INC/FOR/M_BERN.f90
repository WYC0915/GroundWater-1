
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE m_bern

! -------------------------------------------------------------------------
! Purpose:    This module defines several global variables for all
!             bernese routines writen in Fortran 90
!
! Author:     L. Mervart
!
! Created:    22-Nov-1997
!
! Changes:    03-May-2000 HB: INCLUDE I:MAXPOT
!             18-May-2000 HU: INCLUDE I:MAXPOT MOVED TO ROUTINES
!             20-Jan-2001 RD: ADD PARAMETER staNameLength
!             05-Mar-2001 DS: ADD PARAMETER staNam2Length
!             23-Jan-2002 CU: Add PARAMETER timStrgLength,timStrgLength2
!             29-Jan-2002 RD: Add r4b for REAL*4 types
!             01-Oct-2002 HU: Add fileNameLength80
!             28-Jan-2003 RS: r4b deleted
!             06-Feb-2003 RD: Add ProgramName and ProgramDescr
!             18-Feb-2003 HU: Save Program_Name and Program_Dscr
!             18-Feb-2003 HU: Parameter pgmver added, define backslash
!             13-May-2003 CU: Nullify pointers
!             23-Mar-2004 HU: V5.0 --> V5.1
!             21-Jun-2005 MM: LFNUMs moved to m_bern
!             20-Sep-2006 RD: longLineLength added
!             20-Feb-2011 RD: staFla2Length added
!             11-Jul-2011 HB: lineLength1024 added
!             15-Oct-2012 RD/SL: V5.1 --> V5.2
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER*4                 i4ex
  REAL*8                    r8ex
  INTEGER,PARAMETER      :: i4b = KIND(i4ex)
  INTEGER,PARAMETER      :: r8b = KIND(r8ex)

  INTEGER(i4b), PARAMETER :: keyNameLength   =  32
  INTEGER(i4b), PARAMETER :: keyValueLength  = 255

  INTEGER(i4b), PARAMETER :: filePathLength  =  32
  INTEGER(i4b), PARAMETER :: fileNameLength  =  32
  INTEGER(i4b), PARAMETER :: fileNameLength80=  80
  INTEGER(i4b), PARAMETER :: fileExtLength   =   3

  INTEGER(i4b), PARAMETER :: shortLineLength =  80
  INTEGER(i4b), PARAMETER :: lineLength      = 255
  INTEGER(i4b), PARAMETER :: longLineLength  = 512
  INTEGER(i4b), PARAMETER :: lineLength1024  =1024

  INTEGER(i4b), PARAMETER :: staNameLength   =  16
  INTEGER(i4b), PARAMETER :: staNam2Length   =  20
  INTEGER(i4b), PARAMETER :: staFlagLength   =   1
  INTEGER(i4b), PARAMETER :: staFla2Length   =   5

  INTEGER(i4b), PARAMETER :: timStrgLength   =  19
  INTEGER(i4b), PARAMETER :: timStrgLength2  =  40

  CHARACTER(LEN=3), PARAMETER :: pgmver='5.2'

  CHARACTER(LEN=8), SAVE  :: program_Name    = ' '
  CHARACTER(LEN=80),SAVE  :: program_Dscr    = ' '

  CHARACTER(LEN=1), PARAMETER :: backslash = char(92)

  TYPE t_key
    CHARACTER(LEN=keyNameLength)                         :: name
    CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: value
  END TYPE

! Logical file numbers
! --------------------
!                  Extended file numbers !   FORTRAN standard
  INTEGER(i4b)    ::  LFNKBD =   5       !      LFNKBD =  5
  INTEGER(i4b)    ::  LFNPRT =   6       !      LFNPRT =  6
  INTEGER(i4b)    ::  LFNERR =   6       !      LFNERR =  6
  INTEGER(i4b)    ::  LFN001 =  11       !      LFN001 = 11
  INTEGER(i4b)    ::  LFN002 = 501       !      LFN002 = 51
  INTEGER(i4b)    ::  LFNEPH = 990       !      LFNEPH = 90
  INTEGER(i4b)    ::  LFNOR1 = 991       !      LFNOR1 = 91
  INTEGER(i4b)    ::  LFNRP1 = 992       !      LFNRP1 = 92
  INTEGER(i4b)    ::  LFNORB = 993       !      LFNORB = 93
  INTEGER(i4b)    ::  LFNRPR = 994       !      LFNRPR = 94
  INTEGER(i4b)    ::  LFNPLT = 995       !      LFNPLT = 95
  INTEGER(i4b)    ::  LFNRES = 996       !      LFNRES = 96
  INTEGER(i4b)    ::  LFNERF = 997       !      LFNERF = 97
  INTEGER(i4b)    ::  LFNPRF = 998       !      LFNPRF = 98
  INTEGER(i4b)    ::  LFNLOC = 999       !      LFNLOC = 99


! Initialize structure
! --------------------
CONTAINS

  SUBROUTINE init_key(key)
    TYPE(t_key)  :: key
    NULLIFY(key%value)
  END SUBROUTINE init_key

END MODULE m_bern
