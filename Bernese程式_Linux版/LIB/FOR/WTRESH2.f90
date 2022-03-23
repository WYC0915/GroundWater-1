MODULE s_WTRESH2
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE wtresh2(lfn,resHed)

! -------------------------------------------------------------------------
! Purpose:    Writes the header of a Bernese Residual Files
!
! Author:     R. Dach
!
! Created:    26-Aug-2002
! Last mod.:  29-Dec-2003
!
! Changes:    25-Sep-2002 HU: Remove i_astlib
!             29-Dec-2003 HU: Implicit none added
!             11-Nov-2004 HU: Remove CLOSE statement
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_resfil, ONLY: t_resHead
  USE s_opnfil
  USE s_inquire
  USE s_opnerr
  USE s_gtflna
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                   :: lfn     ! Unit number for writing
  TYPE(t_resHead)                :: resHed  ! Residual header to write

! output:

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=7), PARAMETER   :: srName = 'wtresh2'

! Local Variables
! ---------------
  CHARACTER(LEN=fileNameLength) :: filNam

  INTEGER(i4b)                  :: iFil
  INTEGER(i4b)                  :: k
  INTEGER(i4b)                  :: irc,iostat

  LOGICAL                       :: isOpen

! Open residual file
! ------------------
  CALL INQUIRE(lfn,OPENED=isOpen)
  IF (.NOT. isOpen) THEN
    CALL gtflna(0,'RESIDRS',filnam,irc)
    IF(irc /= 0) RETURN

    CALL opnfil(lfn,filnam,'UNKNOWN','UNFORMATTED',' ',' ',iostat)
    CALL opnerr(lfnerr,lfn,iostat,filnam,srName)
  ENDIF

! Write title of residual file and number of files ...
! ----------------------------------------------------
  WRITE(lfn) resHed%title
  WRITE(lfn) resHed%nFil,resHed%dsc%iTyp,99,resHed%dsc%nPar
  WRITE(lfn) resHed%dsc

! General information for each file
! ---------------------------------
  DO iFil=1,resHed%nFil
    WRITE(lfn) resHed%filHead(iFil)%meatyp,           &
               resHed%filHead(iFil)%nfrfil,           &
              (resHed%filHead(iFil)%icarr(k), k=1,resHed%filHead(iFil)%nfrfil),&
              (resHed%filHead(iFil)%stanam(k),k=1,resHed%dsc%nResta+1),   &
              (resHed%filHead(iFil)%csess(k), k=1,2), &
               resHed%filHead(iFil)%ideltt,           &
               resHed%filHead(iFil)%timref
  ENDDO
!
! SV-numbers for each file
! ------------------------
  DO iFil=1,resHed%nFil
    WRITE(lfn) resHed%filHead(iFil)%nSatel,        &
              (resHed%filHead(iFil)%numsat(k),k=1,resHed%filHead(iFil)%nSatel)
  ENDDO

END SUBROUTINE wtresh2

END MODULE
