MODULE s_ETRSIN
CONTAINS

! ------------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! ------------------------------------------------------------------------------

SUBROUTINE etrsin(title,datum,dxyz,drot)

! ------------------------------------------------------------------------------
! Purpose:   Read Input File for PROGRAM ETRS89 (new version of ETRSIN.f)
!
! Author:    H.Bock
!
! Created:   25-Oct-2001
!
! Changes:   23-Apr-2003 CU: Nullify local pointers
!            10-Aug-2005 SS: Inverse transformation;
!                            epoch variable removed
!            28-Feb-2007 AG: Use 206264... from DEFCON
!            14-Dec-2011 SL: use m_bern with ONLY, write format, DATSEL
!
! Copyright: Astronomical Institute
!            University of Bern
!            Switzerland
! ------------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, keyValueLength, lfnErr
  USE d_const,  ONLY: ars
  USE s_ckoptr
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptl
  USE s_ckoptb
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
! OUT:
  CHARACTER(LEN=80) :: title         ! General Title Line
  CHARACTER(LEN=16) :: datum         ! Local Geodetic Datum
  REAL(r8b),DIMENSION(3) :: dxyz     ! Translation Parameters in m
  REAL(r8b),DIMENSION(3) :: drot     ! Rotation Angles in radian

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength),DIMENSION(:),POINTER   :: keyValue
  CHARACTER(LEN=6), PARAMETER                          :: srname = 'ETRSIN'

  INTEGER(i4b) :: irCode
  INTEGER(i4b) :: irc
  INTEGER(i4b) :: invTrafo

! Initialization
! --------------
  irCode = 0
  NULLIFY(keyValue)

! Title line
! ----------
  CALL readKeys('TITLE',keyValue,irc)
  CALL ckoptl(0,'TITLE',keyValue,srName,                                    &
              'Printing options: title line',irc,irCode,                    &
              maxVal=1,maxLength=80,result1=title)

! Get Datum selection
! -------------------
  CALL readkeys('DATSEL',keyValue,irc)
  IF(irc /= 0) CALL exitrc(2)
  datum = TRIM(keyValue(1))
  IF(datum /= 'MANUAL') RETURN

! Read Local Geodetic Datum
! -------------------------
  CALL readKeys('LGTDAT',keyValue,irc)
  CALL ckoptl(0,'LGTDAT',keyValue,srName,                                   &
              'Local Geodetic Datum',irc,irCode,                            &
              maxVal=1,maxLength=16,result1=datum)

! Read Translation Parameters
! ---------------------------
  CALL readKeys('TRAX', keyValue, irc)
  CALL ckoptr(1,'TRAX',keyValue,srName,                                     &
              'Translation Parameter in x-Coordinate.',irc,irCode,          &
              maxVal=1,error=9999.D0,result1=dxyz(1))

  CALL readKeys('TRAY', keyValue, irc)
  CALL ckoptr(1,'TRAY',keyValue,srName,                                     &
              'Translation Parameter in y-Coordinate.',irc,irCode,          &
              maxVal=1,error=9999.D0,result1=dxyz(2))

  CALL readKeys('TRAZ', keyValue, irc)
  CALL ckoptr(1,'TRAZ',keyValue,srName,                                     &
              'Translation Parameter in z-Coordinate.',irc,irCode,          &
              maxVal=1,error=9999.D0,result1=dxyz(3))

! Read Rotation Parameters (ARC SEC)
! ----------------------------------
  CALL readKeys('ROTX', keyValue, irc)
  CALL ckoptr(1,'ROTX',keyValue,srName,                                     &
              'Rotation Parameter around x-Axis.',irc,irCode,               &
              maxVal=1,error=9999.D0,result1=drot(1))

  CALL readKeys('ROTY', keyValue, irc)
  CALL ckoptr(1,'ROTY',keyValue,srName,                                     &
              'Rotation Parameter around y-Axis.',irc,irCode,               &
              maxVal=1,error=9999.D0,result1=drot(2))

  CALL readKeys('ROTZ', keyValue, irc)
  CALL ckoptr(1,'ROTZ',keyValue,srName,                                     &
              'Rotation Parameter around z-Axis.',irc,irCode,               &
              maxVal=1,error=9999.D0,result1=drot(3))

  drot(:)=drot(:)/ars

! Inverse transformation
! ----------------------
  CALL ckoptb(1,(/'INVTRAFO'/),srName,'Inverse transformation', &
              irCode, result1=invTrafo)
  IF(irCode /= 0) CALL exitrc(2)

  IF(invTrafo == 1) THEN
    dxyz(:) = -dxyz(:)
    drot(:) = -drot(:)

    IF(DATUM(1:4) /= 'ITRF') &
      WRITE(lfnerr,"(/,' ### SR ',A,': ', &
      & 'ITRF datum identifier expected', &
      & /,16X,'Datum identifier: ',A,/)") srName,datum
  ELSE
    IF(DATUM(1:4) /= 'ETRF') &
      WRITE(lfnerr,"(/,' ### SR ',A,': ', &
      & 'ETRF datum identifier expected', &
      & /,16X,'Datum identifier: ',A,/)") srName,datum
  ENDIF
  DATUM = DATUM(2:)

  DEALLOCATE(keyValue,stat=irc)

! Stop if an error in the input options found
! -------------------------------------------
  IF(irCode /= 0) CALL exitrc(2)

  RETURN

END SUBROUTINE etrsin

END MODULE
