MODULE s_GTSATR
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE gtsatr(ISTOP,SVN,TMJD,RPRMOD,SVNNR,IBLOCK,MASS,ANLTYP,PLANE,  &
                  formf,C_rpr,admod,adrag)

! -------------------------------------------------------------------------
! Purpose:    This is the new version of SR GTSATR which reads structure
!             satfil instead of file "SATELL" and returns parameters of
!             radiation pressure model
!
! Author:     D. Svehla
!
! Created:    15-Mar-2001
!
! Changes:    16-Dec-2001 HU: Use implicit none
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             16-May-2003 CU: Initialize structure
!             19-Nov-2003 HB: Use boundary NSARAD for copying from
!                             satfil-type
!             18-Oct-2006 MP: returns parameters of rpr model only for
!                             one specified satellite instead of all
!                             satellites
!             12-Mar-2007 CU: Correct format statements for error messages
!             11-Jul-2008 DT: Add formf, C_rpr, adrag, admod
!             03-Jun-2010 HU: Read orbital plane
!             02-Sep-2010 CR: Read space vehicle number
!             29-Aug-2011 PS: Epoch in error message
!             29-Aug-2011 SL: use m_bern with ONLY
!             14-Sep-2011 SL/PS: format statement corrected
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, fileNameLength, timStrgLength, &
                      lfnErr
  USE d_satfil, ONLY: t_satfil, t_rprmod, init_satfil

  USE s_exitrc
  USE s_gtflna
  USE s_rdsatfil
  USE s_timst2
  USE s_alcerr
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                    :: ISTOP   ! IF NO MODEL FOUND:
                                             !         0 REUTRN
                                             !         1 WARNING
                                             !         2 STOP WITH ERROR
  INTEGER(i4b)                    :: SVN     ! PRN number of satellite
  REAL(r8b)                       :: TMJD    ! Epoch in MJD


! output:
  TYPE(t_RPRMOD)                  :: RPRMOD  ! Satellite description
  INTEGER(i4b)                    :: IBLOCK  ! Block number
  CHARACTER(LEN=3)                :: SVNNR   ! Space vehicle number
  REAL(r8b)                       :: MASS    ! Mass of satellite
  CHARACTER(LEN=8)                :: ANLTYP
  CHARACTER(LEN=3)                :: PLANE   ! plane number
  REAL(r8b)                       :: formf   ! area/mass
  REAL(r8b)                       :: C_rpr   ! radiation pressure factor
  INTEGER(i4b)                    :: admod   ! model for air drag/albedo
  REAL(r8b)                       :: adrag   ! coefficient for air drag


! Local Parameters
! ----------------

! List of functions
! -----------------

! Local Variables
! ---------------
  INTEGER(i4b)                    :: irc
  TYPE(t_satfil), SAVE            :: SATFIL  ! satellite description

  INTEGER(i4b)                    :: ii,isat,jsat

  CHARACTER(LEN=fileNameLength)   :: filename
  CHARACTER(LEN=timStrgLength)    :: timstr

  LOGICAL, ALLOCATABLE, DIMENSION(:), SAVE :: prtWarn
  LOGICAL, SAVE                   :: first = .TRUE.


! If called for the first time, read the entire satellite file SATELL
! -------------------------------------------------------------------
  IF (first) THEN
    first = .FALSE.

! Get the satellite info file name
! --------------------------------
    CALL gtflna(1,'SATELL ',filename,irc)

! Read satellite info file (SATELL)
! ---------------------------------
    CALL init_satfil(satfil)
    CALL rdsatfil(filename,satfil)

    ALLOCATE(prtWarn(satfil%nsatellite),stat=irc)
    CALL alcerr(irc, 'prtWarn', (/satfil%nrprmod/), 'gtsatr')
    prtWarn=.TRUE.
  END IF

! FIND SATELLITE INDEX FOR SATELLITE PARAMETER ARRAYS
! ---------------------------------------------------
  isat = -1
  DO ii=1,satfil%nsatellite
     IF ((satfil%satellite(ii)%svn == svn).AND.           &
          (TMJD >= satfil%satellite(ii)%timint%t(1)).AND. &
          (TMJD <= satfil%satellite(ii)%timint%t(2))) THEN
        isat = ii
        EXIT
     END IF
  END DO
  IF (isat == -1) THEN
     CALL timst2(1,1,tmjd,timstr)
     WRITE(lfnerr,'(/,A,/,A,I4,/,A,A,/)')                      &
       ' *** SR GTSATR: Could not read satellite description', &
       '                for satellite:   ', svn,               &
       '                Requested Epoch: ', timstr
     CALL exitrc(2)
  ELSE
     anltyp = satfil%rpmodel
     svnnr  = satfil%satellite(isat)%svnnr
     iblock = satfil%satellite(isat)%iblock
     mass   = satfil%satellite(isat)%mass
     plane  = satfil%satellite(isat)%plane

     formf = satfil%satellite(isat)%formf
     C_rpr = satfil%satellite(isat)%radpres
     admod = satfil%satellite(isat)%admodel
     adrag = satfil%satellite(isat)%adrag
  END IF


! FIND SATELLITE INDEX FOR SATELLITE RPR PARAMETERS
! -------------------------------------------------
  jsat = -1
  DO ii=1,satfil%nrprmod
     IF ((satfil%rprmod(ii)%svn == svn).AND.           &
          (TMJD >= satfil%rprmod(ii)%timint%t(1)).AND. &
          (TMJD <= satfil%rprmod(ii)%timint%t(2))) THEN
        jsat = ii
        EXIT
     END IF
  END DO

  IF (jsat == -1) THEN
     rprmod%svn = svn
     rprmod%timint = satfil%satellite(jsat)%timint
     rprmod%nrprcoe = 0
     NULLIFY(rprmod%rprcoe)
     IF (ISTOP==2) THEN
        CALL timst2(1,1,tmjd,timstr)
        WRITE(lfnerr,'(/,A,/,A,I4,/,A,A,/)')                               &
          ' *** SR GTSATR: Could not read radiation pressure parameters ', &
          '                for satellite:   ', svn,                        &
          '                Requested Epoch: ', timstr
        CALL exitrc(2)
     END IF
     IF (ISTOP==1.AND.prtwarn(isat)) THEN
        CALL timst2(1,1,tmjd,timstr)
        WRITE(lfnerr,'(/,A,/,A,I4,/,A,A,/)')                               &
          ' ### SR GTSATR: Could not read radiation pressure parameters ', &
          '                for satellite:   ', svn,                        &
          '                Requested Epoch: ', timstr
        prtwarn(isat) = .FALSE.
     END IF
  ELSE
     rprmod = satfil%rprmod(jsat)
  END IF

  RETURN

END SUBROUTINE gtsatr

END MODULE
