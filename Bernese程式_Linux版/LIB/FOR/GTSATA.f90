MODULE s_GTSATA
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE gtsata(MAXSAA,NSAANT,SATANT,ANTOFF,TIMINT,SATNAM,SATBLK,SENNUM)

! -------------------------------------------------------------------------
! Purpose:    This is the new version of SR GTSATA which reads structure
!             satfil instead of file "SATELL" and returns sensor offsets,
!             sensor names (microwave transmitter and SLR reflector) and
!             satellite block numbers
!
! Author:     D. Svehla
!
! Created:    15-Mar-2001
! Last mod.:  09-Nov-2005
!
! Changes:    16-Dec-2001 HU: Use implicit none
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             04-Oct-2002 RD: Correct reading of satfil%satellite record
!             16-May-2003 CU: Initialize structure
!             11-Aug-2003 RS: Read new format of satellite information file,
!                             Return sensor names and satellite block numbers
!             16-Sep-2003 HU: Check if no mw sensor found for svn < 300
!             05-Aug-2005 AG: Change searching satellite in satellite info file
!                             by strings (in satellite name) to VAR sensor type,
!                             e.g. instead of 'MW TRANS' now typeMWTR
!             09-NOV-2005 AG: SENNUM for Sensor number added
!             16-MAR-2006 AG: Correct reading of SENNUM
!
! SR used:    gtflna, init_satfil, rdsatfil
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_satfil, ONLY: t_satfil, init_satfil, typeMWTR, typeSLR

  USE s_exitrc
  USE s_gtflna
  USE s_rdsatfil
  IMPLICIT NONE

! Parameters
! ==========
!
! IN :
! ----
  INTEGER(i4b)                    :: MAXSAA ! Maximum number of satellites

!
! OUT :
! -----
  INTEGER(i4b)                    :: NSAANT ! Number of satellites
  INTEGER(i4b),DIMENSION(MAXSAA)  :: SATANT ! Satellite numbers
  REAL(r8b),   DIMENSION(6,MAXSAA):: ANTOFF ! Sensor offsets(1:3) &
                                            ! only SLR offsets(4:6)
  REAL(r8b),   DIMENSION(2,MAXSAA):: TIMINT ! Time intervals given in MJD
                                            ! begin & end
  CHARACTER(LEN=staNam2Length), DIMENSION(2,MAXSAA) :: SATNAM
                                            ! Satellite names
  INTEGER(i4b),DIMENSION(MAXSAA)  :: SATBLK ! Block numbers
  INTEGER(i4b),DIMENSION(MAXSAA)  :: SENNUM ! Sensor numbers

!
! Dummy list
! ----------
  TYPE(t_satfil), SAVE         :: satfil
!
! Local Variables
! ---------------
  INTEGER(i4b)                 :: icrx, IRC, ii

  CHARACTER(LEN=fileNameLength),SAVE :: filename

  LOGICAL,      SAVE           :: first= .TRUE.

!
! If called for the first time, read the entire satellite file SATELL
! ===================================================================
  IF (first) THEN
    first = .FALSE.

!
! Get the satellite info file name
! ---------------------------------------------------
    CALL gtflna(1,'SATELL ',filename,IRC)

!
! Read satellite info file (SATELL)
! ---------------------------------
    CALL init_satfil(satfil)
    CALL rdsatfil(filename,satfil)
  END IF

!
! Read the structure
! ==================
  NSAANT=satfil%nsatellite

!
! Check maximum number of satellites
! ----------------------------------
  IF (NSAANT.GT.MAXSAA) GOTO 900

  SATANT(1:NSAANT)  =satfil%satellite(1:NSAANT)%svn
  SATBLK(1:NSAANT)  =satfil%satellite(1:NSAANT)%iblock
  TIMINT(1,1:NSAANT)=satfil%satellite(1:NSAANT)%timint%t(1)
  TIMINT(2,1:NSAANT)=satfil%satellite(1:NSAANT)%timint%t(2)
!
  DO icrx = 1, NSAANT
    ANTOFF(1:6,icrx)=0.D0
    SATNAM(1:2,icrx)=' '
    SENNUM(icrx)=0

!
    DO ii = 1, satfil%nsensor
      IF (satfil%sensor(ii)%svn==SATANT(icrx).AND.                  &
!          satfil%sensor(ii)%timint%t(1).GE.TIMINT(1,icrx).AND.      &
!          satfil%sensor(ii)%timint%t(2).LE.TIMINT(2,icrx)) THEN
          satfil%sensor(ii)%timint%t(1).GE.TIMINT(1,icrx).AND.      &
          satfil%sensor(ii)%timint%t(1) < TIMINT(2,icrx) .OR.       &
          satfil%sensor(ii)%svn==SATANT(icrx).AND.                  &
          satfil%sensor(ii)%timint%t(2) > TIMINT(1,icrx).AND.       &
          satfil%sensor(ii)%timint%t(2).LE.TIMINT(2,icrx)) THEN
!        IF (satfil%sensor(ii)%sensor(1:9).EQ.'MW TRANSM') THEN
        IF (satfil%sensor(ii)%type.EQ.typeMWTR) THEN
          SENNUM(icrx)=satfil%sensor(ii)%numb
          ANTOFF(1:3,icrx)=satfil%sensor(ii)%antoff(1:3)
          SATNAM(1,icrx)=satfil%sensor(ii)%sensor
!        ELSE IF (satfil%sensor(ii)%sensor(1:8).EQ.'SLR REFL') THEN
        ELSE IF (satfil%sensor(ii)%type.EQ.typeSLR) THEN
          ANTOFF(4:6,icrx)=satfil%sensor(ii)%antoff(1:3)
          SATNAM(2,icrx)=satfil%sensor(ii)%sensor
        END IF
      END IF
    END DO
!
    IF (SATANT(ICRX)<300 .AND. SATNAM(1,icrx)==' ') THEN
      WRITE(lfnerr,"(' ### GTSATA: No sensor found for sat',I4,' in ',A)") &
            satant(icrx),TRIM(filename)
    ENDIF
  END DO

  RETURN

900  WRITE(lfnerr,'(A,/,16X,A,I4,16X,A,A,/)')                        &
           '*** SR GTSATA: too many satellites in satellite file',   &
                          'maximum number of satellites:', MAXSAA,   &
                          'satellite file: ',TRIM(filename)
  CALL exitrc(2)

END SUBROUTINE gtsata

END MODULE
