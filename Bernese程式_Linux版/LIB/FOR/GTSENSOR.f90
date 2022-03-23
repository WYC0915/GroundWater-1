MODULE s_gtsensor
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE gtsensor(prn,epo,type1,sensor,sensnr,timint,antoff,bore,   &
                        azim,antexna,ifrq,nsigli,siglist,pcvmod)

! -------------------------------------------------------------------------
! Purpose:    Reads sensor data for given PRN, epoch and sensor type
!             Output parameters are all optional
!
! Author:     A. Gaede
!
! Created:    10-Aug-2005
! Last mod.:  10-Aug-2006
!
! Changes:    10-Jul-2006 AG: all is optional, pcvmod added
!             10-Aug-2006 HB: Use new SR TIMST2 (module)
!
! Remarks:    Nullify the "siglist" in the calling program!!!!!
!
! SRs called: rdsatfil, init_satfil, gtflna, exitrc, timest2
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_satfil, ONLY : t_satfil,init_satfil, typeSLR
  USE s_alcerr
  USE s_exitrc
  USE s_gtflna
  USE s_rdsatfil
  USE m_time, ONLY : t_timint
  USE s_timst2
  IMPLICIT NONE

! Dummy list
! ----------
  TYPE(t_satfil), SAVE             :: satfil

! List of Parameters
! ------------------
! IN:
    INTEGER(i4b),      OPTIONAL    :: prn     !PRN number
    REAL(r8b),         OPTIONAL    :: epo     !current epoch
    CHARACTER(4),      OPTIONAL    :: type1   !sensor type
! OUT:
    CHARACTER(LEN=staNam2Length),               &
                       OPTIONAL    :: sensor  !Sensor name
    INTEGER(i4b),      OPTIONAL    :: sensnr  !special sensor number
    TYPE(t_timint),    OPTIONAL    :: timint  !Time intervals in JULIAN DATE
                                              !Begin&end of interval (m_time)
    REAL(r8b),OPTIONAL,DIMENSION(3):: antoff  !Sensor offsets in
                                              !satellite reference frame
                                              !(see e.g. diss Feltens and
                                              !REMARK in SATELL file) (m)
    REAL(r8b),OPTIONAL,DIMENSION(3):: bore    !Sensor boresight vector
    REAL(r8b),OPTIONAL,DIMENSION(3):: azim    !Sensor reference azimuth vector
    CHARACTER(LEN=20), OPTIONAL    :: antexna !Antex sensor name
    INTEGER(i4b),      OPTIONAL    :: ifrq    !Transmitted frequency
    INTEGER(i4b),      OPTIONAL    :: nsigli  !Number of entries in sigli
    CHARACTER(LEN=3),  OPTIONAL,DIMENSION(:),                 &
                           POINTER :: siglist !List of signals
    CHARACTER(LEN=10), OPTIONAL    :: pcvmod  !Antenna model

! Local Variables
! ---------------
  CHARACTER(LEN=8),PARAMETER       :: srname = 'gtsensor'
  CHARACTER(LEN=fileNameLength)    :: filename
  INTEGER(i4b)                     :: icrx,count,found,irc
  CHARACTER(19)                    :: tstrng
  LOGICAL,      SAVE               :: first= .TRUE.


! If called for the first time, read the entire SATELL file
! =========================================================
  IF (first) THEN
    first = .FALSE.

! Get the satellite info file name
    CALL gtflna(1,'SATELL ',filename,irc)

! Read satellite info file (SATELL)
    CALL init_satfil(satfil)
    CALL rdsatfil(filename,satfil)
  END IF

! Searching the sensor in the satfil-structure
! ===============================================
  IF (PRESENT(prn)) THEN
    IF (.NOT. PRESENT(epo) .OR. .NOT. PRESENT(type1)) THEN
      WRITE(lfnerr, &
           "(/,' *** SR GTSENSOR: epo and/or type are not given for call of ', &
                    &             'subroutine',/)")
      CALL exitrc(2)
    ENDIF
    found=0
    count=0
    DO icrx = 1,satfil%nsensor
      IF (satfil%sensor(icrx)%svn == prn .AND.               &
           satfil%sensor(icrx)%type == type1 .AND.             &
           epo .GE. satfil%sensor(icrx)%timint%t(1) .AND.     &
           epo .LE. satfil%sensor(icrx)%timint%t(2)) THEN
        IF (found == 0) THEN
          found = icrx
        ELSE
          found = -icrx
        ENDIF
        count=count+1
      ENDIF
    ENDDO

    IF (found > 0) THEN
      IF (PRESENT(sensor))  sensor  = satfil%sensor(found)%sensor
      IF (PRESENT(sensnr))  sensnr  = satfil%sensor(found)%numb
      IF (PRESENT(timint))  timint  = satfil%sensor(found)%timint
      IF (PRESENT(antoff))  antoff  = satfil%sensor(found)%antoff
      IF (PRESENT(bore))    bore    = satfil%sensor(found)%bore
      IF (PRESENT(azim))    azim    = satfil%sensor(found)%azim
      IF (PRESENT(antexna)) antexna = satfil%sensor(found)%name
      IF (PRESENT(ifrq))    ifrq    = satfil%sensor(found)%ifrq
      IF (PRESENT(nsigli))  nsigli  = satfil%sensor(found)%nsigli
      IF (PRESENT(siglist)) THEN
        IF (ASSOCIATED(siglist)) DEALLOCATE (siglist,stat=irc)
        IF( satfil%sensor(found)%nsigli /= 0) THEN
          ALLOCATE(siglist(satfil%sensor(found)%nsigli),stat=irc)
          CALL alcerr(irc,'siglist',(/satfil%sensor(found)%nsigli/),srName)
          siglist = satfil%sensor(found)%sigli
        ELSE
          ALLOCATE(siglist(1),stat=irc)
          CALL alcerr(irc,'siglist',(/1/),srName)
          siglist(1)=' '
        ENDIF
      ENDIF
    ENDIF
! more than one sensor with PRN or no sensor found for epoch
    IF (found <= 0) THEN
      CALL timst2(1,1,epo,tstrng)
    ENDIF
    IF (found < 0) THEN
      WRITE(lfnerr, &
           "(/,' *** SR GTSENSOR: ',I2,A5,' sensors with PRN number found for epoch', &
           & /,'                  PRN number: ',I4,                                   &
           & /,'                  Epoch     : ',A,/)") count,type1,prn, tstrng
      CALL exitrc(2)
    ELSE IF(found == 0) THEN
      WRITE(lfnerr, &
           "(/,' *** SR GTSENSOR: No sensors with PRN number found for epoch',   &
           & /,'                  PRN number: ',I4,                              &
           & /,'                  Epoch     : ',A,/)")prn, tstrng
      CALL exitrc(2)
    ENDIF
  ENDIF
  IF (PRESENT(pcvmod)) pcvmod=satfil%pcvmod

END SUBROUTINE gtsensor


END MODULE
