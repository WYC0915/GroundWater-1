MODULE s_PRN2COS
CONTAINS

! -----------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -----------------------------------------------------------------------------

SUBROUTINE prn2cos(isel,prn,epo,cospar,irc)

! -----------------------------------------------------------------------------
! Purpose:    Conversion of PRN number to COSPAR number
!             e.g.  906 (CHAMP) -> 2000-039B, using satellite info file
!
! Author:     C. Urschl
!
! Created:    26-Oct-2003
! Last mod.:  08-Aug-2005
!
! Changes:    14-Mar-2005 CU: Extend error handling
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!
! SR used:    gtflna, rdsatfil, timst2
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -----------------------------------------------------------------------------

  USE m_bern
  USE d_satfil, ONLY: t_satfil,init_satfil

  USE s_timst2
  USE s_exitrc
  USE s_gtflna
  USE s_rdsatfil
  IMPLICIT NONE

! Dummy list
! ----------
  TYPE(t_satfil), SAVE  :: satfil


! Parameters
! ----------
! In:
  INTEGER(i4b)                 :: isel    ! Problem handling:
                                          ! 0 : none
                                          ! 1 : warning, if prn not found
                                          ! 11: warning, if prn or cospar-id not found
                                          ! 2 : error  , if prn not found
                                          ! 22: error  , if prn or cospar-id not found
                                          ! 21: error  , if prn not found or
                                          !     warning, if cospar-id not found
  INTEGER(i4b)                 :: prn     ! Satellite number
  REAL(r8b)                    :: epo     ! Epoch

! Out:
  CHARACTER(LEN=9)             :: cospar  ! Cospar number
  INTEGER(i4b)                 :: irc     ! Error return code


! Local Variables
! ---------------
  CHARACTER(LEN=fileNameLength):: filename
  CHARACTER(LEN=19)            :: tstrng
  INTEGER(i4b)                 :: icrx

  LOGICAL,      SAVE           :: first= .TRUE.
  LOGICAL                      :: found


! If called for the first time, read the entire sat.info file
! -----------------------------------------------------------
  IF (first) THEN
    first = .FALSE.

! Get the satellite info file name
    CALL gtflna(1,'SATELL ',filename,irc)

! Read satellite info file
    CALL init_satfil(satfil)
    CALL rdsatfil(filename,satfil)
  END IF


! Searching the satellite in the satfil-structure
! -----------------------------------------------
  found  = .FALSE.
  cospar = ' '

  DO icrx = 1, satfil%nsatellite

    IF (satfil%satellite(icrx)%svn == prn) THEN
      IF (epo.GE.satfil%satellite(icrx)%timint%t(1).AND.&
          epo.LE.satfil%satellite(icrx)%timint%t(2)) THEN
        found = .TRUE.
        cospar = satfil%satellite(icrx)%cospar(1:9)
        EXIT
      END IF
    END IF

  END DO

  IF (cospar == ' ') THEN
    CALL timst2(1,1,epo,tstrng)

    IF (.NOT. found) THEN
      IF (isel == 1 .OR. isel == 11) THEN
        WRITE(lfnerr,'(A,/,A,I3,/,A,A)' )                                 &
          ' ### SR PRN2COS: PRN number not found in satellite info file', &
          '                 PRN number: ',prn,                            &
          '                 Epoch:      ',tstrng
        irc = 1
      ELSEIF (isel == 2 .OR. isel == 22 .OR. isel == 21) THEN
        WRITE(lfnerr,'(A,/,A,I3,/,A,A)' )                                 &
          ' *** SR PRN2COS: PRN number not found in satellite info file', &
          '                 PRN number: ',prn,                            &
          '                 Epoch:      ',tstrng
        CALL exitrc(2)
      ENDIF
    ELSE
      IF (isel == 11 .OR. isel == 21) THEN
        WRITE(lfnerr,'(A,/,A,I3,/,A,A)' )                                 &
          ' ### SR PRN2COS: COSPAR-ID not found in satellite info file',  &
          '                 PRN number: ',prn,                            &
          '                 Epoch:      ',tstrng
        irc = 1
      ELSEIF (isel == 22) THEN
        WRITE(lfnerr,'(A,/,A,I3,/,A,A)' )                                 &
          ' *** SR PRN2COS: COSPAR-ID not found in satellite info file',  &
          '                 PRN number: ',prn,                            &
          '                 Epoch:      ',tstrng
        CALL exitrc(2)
      ENDIF
    ENDIF

  ELSE
    irc = 0
  ENDIF


  RETURN

END SUBROUTINE prn2cos

END MODULE
