MODULE s_RDIHOI
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdihoi(opthoi,sighoi)

! -------------------------------------------------------------------------
! Purpose:    Read GPSEST input options wrt HOI scaling parameters.
!
! Author:     S.Lutz
!
! Created:    04-Jan-2010
! Last mod.:  04-Jan-2010
!
! Changes:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE s_readkeys
  USE s_ckoptc
  USE s_ckoptr
  IMPLICIT NONE

! List of Parameters
! ------------------
  INTEGER(i4b),DIMENSION(*)    :: opthoi ! options for hoi scaling par.
                                         ! (1-3): 1.- order
                                         !      =0: no
                                         !      =1: one for all
                                         !      =2: station specific
  REAL(r8b),DIMENSION(*)       :: sighoi ! a priori sigmas for hoi scaling par.
                                         ! (1-3): 1.- order

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
! HOI scaling factors keywords
  CHARACTER(LEN=keyNameLength), DIMENSION(3), PARAMETER :: hoiKeyw = &
  (/'HOI1', 'HOI2', 'HOI3'/)
  CHARACTER(LEN=keyNameLength), DIMENSION(3), PARAMETER :: sigKeyw = &
  (/'SIGHOI1','SIGHOI2','SIGHOI3'/)

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), &
         DIMENSION(:), POINTER  :: keyValue

  INTEGER(i4b)                  :: irc,iac,irCode
  INTEGER(i4b)                  :: ii

! Init the variables
! ------------------
  irCode      = 0
  opthoi(1:3) = 0
  sighoi(1:3) = 0d0

  NULLIFY(keyValue)

! Loop all entries
! ----------------
  DO ii = 1,3
    CALL readKeys(hoiKeyw(ii), keyValue, irc)
    CALL ckoptc(1,hoiKeyw(ii), keyValue,                                  &
                (/'NO             ','ONE_FOR_ALL    ','ONE_PER_STATION'/),&
                'HOI scaling parameter setup','RDIHOI',irc,irCode,        &
                valList=(/0,1,2/), result1=opthoi(ii))

    CALL readKeys(sigKeyw(ii), keyValue, irc)
    CALL ckoptr(1,sigKeyw(ii), keyValue,'RDIHOI',                        &
              'HOI scaling paramter sigmas',irc,irCode,  &
              empty=0d0,ge=0d0,result1=sighoi(ii))
  ENDDO

! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=iac)

  RETURN

END SUBROUTINE rdihoi

END MODULE
