MODULE s_RXMB3I
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rxmb3I(campgn,coment,newold,istops,staCrux)

! -------------------------------------------------------------------------
!
! Purpose:    This is a new version of the old subroutine RXMB3I.f that
!             reads the input options of the program RXMBV3
!
! Author:     C. Urschl
!
! Created:    07-Sep-2000
! Last mod.:  09-Jul-2003
!
! Changes:    29-Oct-2001  RD: Extract campaign name
!             23-Apr-2003  RD: Nullify local pointers
!             09-Jul-2003  RD: Read staCrux here, handle flags
!
! SRs used:   readkeys, fparse, exitrc, gtflna, readCrux, gtstaflg
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_stacrx, ONLY: t_staCrux

  USE s_exitrc
  USE s_readkeys
  USE s_gtstaflg
  USE s_gtflna
  USE s_readcrux
  USE s_fparse
  IMPLICIT NONE

! List of Parameters
! ------------------
  CHARACTER(LEN=8)    :: campgn  ! campaign name
  CHARACTER(LEN=60)   :: coment  ! general title line
  INTEGER(i4b)        :: newold  ! 1   : create new met.file
                                 ! else: append to existing met.file
  INTEGER(i4b)        :: istops  ! 1 : stop, if station not found in station
                                 !     name translation table
                                 ! 0 : do not stop, if station not found in
                                 !     station name translation table
  TYPE(t_staCrux)     :: staCrux ! Station information for renaming

! Local Variables
! ---------------
  CHARACTER(LEN=fileNameLength)                        :: staFil
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue
  CHARACTER(LEN=fileNameLength) :: node    ! used by FPARSE
  CHARACTER(LEN=fileNameLength) :: device  ! used by FPARSE
  CHARACTER(LEN=fileNameLength) :: dir     ! used by FPARSE
  CHARACTER(LEN=fileNameLength) :: name    ! used by FPARSE
  CHARACTER(LEN=fileExtLength)  :: ext     ! used by FPARSE
  CHARACTER(LEN=fileExtLength)  :: ver     ! used by FPARSE
  INTEGER(i4b)                  :: irc,irCode


! Init varaibles
! --------------
  irCode = 0

  NULLIFY(keyValue)

! Read Campaign Characterization
! ------------------------------
  CALL readkeys('CAMPAIGN', keyValue, irc)
  CALL fparse(0,keyValue(1),node,device,dir,name,ext,ver,irc)
  campgn=name

! Read Comment Line
! -----------------
  CALL readkeys('TITLE', keyValue, irc)
  coment = keyValue(1)

! Read Input Parameters
! ---------------------
  CALL readkeys('NEWOLD', keyValue, irc)
  IF (keyValue(1) == 'NEW') THEN
    newold = 1
  ELSE IF (keyValue(1) == 'APPEND') THEN
    newold = 0
  ELSE
    WRITE(lfnerr,*)' *** SR RXMB3I: invalid entry for NEW/APPEND: ',keyValue(1)
    CALL exitrc(2)
  END IF

! Read Stop Flag
! --------------
  CALL readkeys('STNMSTOP', keyValue, irc)
  IF (keyValue(1) == '1') THEN
    istops = 1
  ELSE IF (keyValue(1) == '0') THEN
    istops = 0
  END IF

! Get staInfo for renaming
! ------------------------
  CALL gtflna(0,'STAINFO',staFil,irc)

  IF (irc == 0 .AND. LEN_TRIM(staFil) > 0) THEN
    CALL readCrux(staFil,staCrux)

    CALL gtstaflg('USEFLG',                                         &
                  (/'FLG001','      ','      ','      ','      '/), &
                  staCrux)
  ENDIF

  IF (irCode /= 0) CALL exitrc(2)

! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=irc)

END SUBROUTINE rxmb3I

END MODULE
