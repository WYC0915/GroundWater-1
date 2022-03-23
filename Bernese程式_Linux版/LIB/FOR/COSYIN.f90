MODULE s_COSYIN
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE cosyin(dXYZ,dRot,dScale)

! -------------------------------------------------------------------------
! Purpose:    Reads input options for COOSYS and generate some output
!
! Author:     M.Meindl
!
! Created:    18-Sep-2001
!
! Changes:    25-Sep-2002 HU: Remove i_astlib
!             23-Apr-2003 CU: Nullify local pointers
!             28-Feb-2007 AG: USE 206264... from DEFCON
!             14-Nov-2011 SL: m_bern w/ ONLY, no PRITIT call (moved to program)
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, keyValueLength, lfnPrt, lfnErr
  USE d_const,  ONLY: ars
  USE s_prflna
  USE s_readkeys
  USE s_exitrc
  IMPLICIT NONE

! List of parameters
! ------------------
! input:

! output:
  REAL(r8b),DIMENSION(3)           :: dXYZ         ! translation (in m)
  REAL(r8b),DIMENSION(3)           :: dRot         ! rotation (in arcsec)
  REAL(r8b)                        :: dScale       ! scale (in ppm)

! Local variables
! ---------------
  CHARACTER(len=keyValueLength),DIMENSION(:),POINTER ::keyValue
  INTEGER(i4b)                                       ::irc, ios
  INTEGER(i4b)                                       ::ii
  CHARACTER(len=6),DIMENSION(3)                      ::shiftL, rotL

! Initialize some variables
! -------------------------
  shiftL  = (/'SHIFTX','SHIFTY','SHIFTZ'/)
  rotL    = (/'ROTX','ROTY','ROTZ'/)
  dXYZ(:) = 0
  dRot(:) = 0
  dScale  = 0

  NULLIFY(keyValue)

! Read transformation parameters
! ------------------------------
! translations
  DO ii = 1,3
    CALL readkeys(shiftL(ii), keyValue, irc)
    IF (TRIM(keyValue(1)) == '') keyValue(1) = '0'
    IF (irc == 0) READ(keyValue(1), *, iostat=ios) DXYZ(ii)
    IF (irc /= 0 .OR. ios /= 0) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,A10)')                             &
           ' *** PG COOSYS: WRONG TRANSFORMATION PARAMETER ',          &
           'SPECIFIED VALUE: ', keyValue(1)
      CALL exitrc(2)
    END IF
  END DO

! rotations
  DO ii = 1,3
    CALL readkeys(rotL(ii), keyValue, irc)
    IF (TRIM(keyValue(1)) == '') keyValue(1) = '0'
    IF (irc == 0) READ(keyValue(1), *, iostat=ios) DROT(ii)
    IF (irc /= 0 .OR. ios /= 0) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,A10)')                             &
           ' *** PG COOSYS: WRONG TRANSFORMATION PARAMETER ',          &
           'SPECIFIED VALUE: ', keyValue(1)
      CALL exitrc(2)
    END IF
    DROT(ii)=DROT(ii)/ars
  END DO

! scale
  CALL readkeys('SCALE', keyValue, irc)
  IF (TRIM(keyValue(1)) == '') keyValue(1) = '0'
  IF (irc == 0) READ(keyValue(1), *, iostat=ios) DSCALE
  IF (irc /= 0 .OR. ios /= 0) THEN
    WRITE(lfnerr,'(/,A,/,16X,A,A10)')                               &
         ' *** PG COOSYS: WRONG TRANSFORMATION PARAMETER ',            &
         'SPECIFIED VALUE: ', keyValue(1)
    CALL exitrc(2)
  END IF
  DSCALE=DSCALE*1.D-6

! Print some output
! -----------------
  CALL prflna

  WRITE(lfnprt,*) 'Transformation parameters:'
  WRITE(lfnprt,*) '--------------------------'
  DO ii=1,3
    WRITE(lfnprt,'(A6,A2,F13.4,A2,8X,A4,A2,F8.4,A7)')               &
         shiftL(ii),': ',dXYZ(ii),'m',rotL(ii),': ',                   &
         dRot(ii)*ars,'arcsec'
  END DO
  WRITE(lfnprt,'(A8,F13.4,A4,//)') 'SCALE: ', DSCALE*1.D6,' ppm'

  DEALLOCATE(keyValue,stat=irc)

! End
! ---
END SUBROUTINE cosyin

END MODULE
