MODULE s_RDISAP
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdisap(maxsgr, maxspv, nanspv, nsaspv, satspv, gnrspv, nptspv, &
                  sigspv, nadmax, globalWindow, nallsat, allsatnum)

! -------------------------------------------------------------------------
! Purpose:    Reads the satellite antenna pcv input options for GPSEST
!
! Author:     R. Schmid
!
! Created:    11-Nov-2002
!
! Changes:    23-Apr-2003 RD: Nullify local pointers
!             15-May-2003 HU: Initialize structures
!             11-Aug-2003 RS: Correct block-specific grouping of
!                             satellites
!             13-Aug-2003 RS: New SAPII_IIA cards
!             10-Nov-2003 RS: Nadir and azimuth increments, use m_time,
!                             check manual input, add calls of gtsata and
!                             timst2, use m_maxdim, change name of grpspv
!                             to gnrspv
!             19-Jan-2003 SS/MM: Revision of GPSEST input panels
!             26-May-2005 RD: Use dimension MAXSGR for SATOFF and SATSPV
!             28-Jun-2005 MM: Unused variables removed
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             21-Sep-2005 HU: Block-specific setup according to SATELLIT.
!             09-NOV-2005 AG: SENNUM for GTSATA CALL ADDED
!             04-Oct-2006 AG: Satellite specific antenna PCV implemented
!             05-Mar-2012 RD: Use LISTI4 as module now
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnerr, &
                      keyValueLength, fileNameLength, shortLineLength, &
                      staNam2Length
  USE m_time,   ONLY: t_timint
  USE m_maxdim, ONLY: maxsaa
  USE d_satfil, ONLY: t_satfil,init_satfil
  USE d_const,  ONLY: pi
  USE s_dimtst
  USE s_timst2
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptc
  USE s_gtflna
  USE s_gtsata
  USE s_iordup
  USE s_rdsatfil
  USE f_listi4
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                 :: maxsgr ! maximum number of satellites per group
  INTEGER(i4b)                 :: maxspv ! maximum number of satellite antenna
                                         ! phase center variation groups
  INTEGER(i4b)                 :: nAllSat      ! number of all satellites
  INTEGER(i4b), DIMENSION(*)   :: allSatNum    ! satellite numbers

  TYPE(t_timint)         :: globalWindow ! window to be processed
                                         ! (from-to, MJD)

! output:
  INTEGER(i4b)                 :: nanspv ! number of satellite antenna phase
                                         ! center groups to be estimated
  INTEGER(i4b), DIMENSION(*)   :: nsaspv ! nsaspv(i),i=1,..,nanspv
                                         ! number of satellites belonging to
                                         ! antenna phase center group i
  INTEGER(i4b),                 &
         DIMENSION(maxsgr,*)   :: satspv ! satspv(j,i),j=1,..,nsaspv(i)
                                         !             i=1,..,nanspv
                                         ! satellite numbers of each antenna
                                         ! phase center group
  INTEGER(i4b), DIMENSION(*)   :: gnrspv ! gnrspv(i),i=1,..,nanspv
                                         ! user-defined number of satellite
                                         ! antenna phase center group i
  INTEGER(i4b), DIMENSION(2,*) :: nptspv ! nptspv(j,i),j=1,2
                                         !             i=1,..,nanspv
                                         ! number of points to be estimated
                                         ! in elevation (j=1) and azimuth (j=2)
                                         ! direction
  REAL(r8b),    DIMENSION(*)   :: sigspv ! sigspv(i),i=1,..,nanspv
                                         ! a priori sigmas (in meters)
  REAL(r8b)                    :: nadmax ! maximum nadir angle for satellite
                                         ! antenna pattern estimation (in rad)

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------

! Dummy list
! ----------
  TYPE(t_satfil), SAVE  :: satfil

! Local Variables
! ---------------
  INTEGER(i4b)                        :: irc, ios
  INTEGER(i4b)                        :: irCode = 0
  INTEGER(i4b)                        :: sapDazi, sapSel
  INTEGER(i4b)                        :: ii, jj, iGrp, iGrpmax, isat
  INTEGER(i4b)                        :: jgrp,nsaant,isaant
  INTEGER(i4b), DIMENSION(maxsaa)     :: satant,satblk,sennum
  INTEGER(i4b)                        :: maxGrpNr=999
  INTEGER(i4b), DIMENSION(999)        :: nsaloc,grploc
  INTEGER(i4b), DIMENSION(maxsgr,999) :: satloc
  INTEGER(i4b), DIMENSION(maxspv)     :: ihelp,indx

  CHARACTER(LEN=keyValueLength) , &
         DIMENSION(:)  , POINTER      :: keyValue
  CHARACTER(LEN=shortLineLength), &
         DIMENSION(16)                :: hlpStr
  CHARACTER(LEN=fileNameLength)       :: filename
  CHARACTER(LEN=staNam2Length),   &
         DIMENSION(2,maxsaa)          :: satnam
  CHARACTER(LEN=staNam2Length)        :: refnam
  CHARACTER(LEN=20)                   :: string1, string2

  REAL(r8b)                           :: sapDnad
  REAL(r8b)                           :: sapSigma
  REAL(r8b),    DIMENSION(6,maxsaa)   :: santof
  REAL(r8b),    DIMENSION(2,maxsaa)   :: timint

  LOGICAL                             :: newGroup
  LOGICAL,      SAVE                  :: first = .TRUE.
  LOGICAL                             :: notfound

! Init variables
! --------------
  NULLIFY(keyValue)

! Read the default values
! -----------------------
  CALL readKeys('SAPNADMAX', keyValue, irc)
  IF (irc == 0) READ(keyValue(1), *, iostat=ios) nadmax
  nadmax = nadmax/180.D0*pi
  IF (irc /= 0 .OR. ios /= 0) THEN
    nadmax = 14.D0/180.D0*pi
    IF (LEN_TRIM(keyValue(1)) > 0) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,A,/)')                    &
        ' *** SR RDISAP: Wrong maximum nadir angle specified for', &
                        'satellite antenna phase center variation',&
                        'Specified value:       ',TRIM(keyValue(1))
      CALL exitrc(2)
    ENDIF
  ENDIF

  CALL readKeys('SAPDNAD', keyValue, irc)
  IF (irc == 0) READ(keyValue(1), *, iostat=ios) sapDnad
  IF (irc /= 0 .OR. ios /= 0) THEN
    WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,A,/)')                    &
      ' *** SR RDISAP: Wrong nadir increment specified for',     &
                      'satellite antenna phase center variation',&
                      'Specified value:       ',TRIM(keyValue(1))
    CALL exitrc(2)
  ENDIF

  CALL readKeys('SAPDAZI', keyValue, irc)
  IF (irc == 0) READ(keyValue(1), *, iostat=ios) sapDazi
  IF (irc /= 0 .OR. ios /= 0) THEN
    WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,A,/)')                    &
      ' *** SR RDISAP: Wrong azimuth increment specified for',   &
                      'satellite antenna phase center variation',&
                      'Specified value:       ',TRIM(keyValue(1))
    CALL exitrc(2)
  ENDIF

  CALL readKeys('SAPSIGMA', keyValue, irc)
  IF (irc == 0) READ(keyValue(1), *, iostat=ios) sapSigma
  IF (irc /= 0 .OR. ios /= 0) THEN
    sapSigma = 0d0
    IF (LEN_TRIM(keyValue(1)) > 0) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,A,/)')                    &
        ' *** SR RDISAP: Wrong a priori sigma specified for',      &
                        'satellite antenna phase center variation',&
                        'Specified value:       ',TRIM(keyValue(1))
      CALL exitrc(2)
    ENDIF
  ENDIF

! Which type of selection was set?
! --------------------------------
  CALL readKeys('SAPSEL', keyValue, irc)
  CALL ckoptc(1,'SAPSEL', keyValue, &
              (/'BLOCK/TYPE  ','MANUAL      ','SAT_SPECIFIC'/), &
              'rdisap', 'SAP setup', irc, irCode, &
              valList=(/1,2,3/), result1=sapSel)

  IF (irCode /= 0) CALL exitrc(2)

! Read the block-specific input according to SATELLIT.
! ----------------------------------------------------
  IF (sapSel == 1) THEN

! Read satellite info file (SATELL)
! ---------------------------------
    IF (first) THEN
      first = .FALSE.
      CALL gtflna(1,'SATELL ',filename,IRC)
      CALL init_satfil(satfil)
      CALL rdsatfil(filename,satfil)
    ENDIF

! Compile groups
! --------------
    nanspv=0
    DO isat = 1,satfil%nsatellite
      IF (satfil%satellite(isat)%iblock > 0 .AND. &
          satfil%satellite(isat)%iblock < 300) THEN
        IF (satfil%satellite(isat)%timint%t(1) < globalWindow%t(1) .AND. &
            satfil%satellite(isat)%timint%t(2) > globalWindow%t(2)) THEN
          iGrp=listi4(1,maxspv,ihelp,satfil%satellite(isat)%iblock,nanspv)
        ENDIF
      ENDIF
    ENDDO
    CALL iordup(ihelp,nanspv,indx)
    DO iGrp=1,nanspv
      gnrspv(iGrp)=ihelp(indx(iGrp))
    ENDDO
    nsaspv(1:nanspv)=0
    DO isat = 1,satfil%nsatellite
      IF (satfil%satellite(isat)%iblock > 0 .AND. &
          satfil%satellite(isat)%iblock < 300) THEN
        IF (satfil%satellite(isat)%timint%t(1) < globalWindow%t(1) .AND. &
            satfil%satellite(isat)%timint%t(2) > globalWindow%t(2)) THEN
          iGrp=listi4(0,maxspv,gnrspv,satfil%satellite(isat)%iblock,nanspv)
          IF (iGrp > 0) THEN
            nsaspv(iGrp) = nsaspv(iGrp) + 1
            satspv(nsaspv(iGrp),iGrp) = satfil%satellite(isat)%svn
          ENDIF
        ENDIF
      ENDIF
    ENDDO

    DO ii = 1,nanspv
      nptspv(1,ii) = IDNINT(nadmax/pi*180.D0/sapDnad)+1
      nptspv(2,ii) = IDNINT(360.D0/DBLE(sapDazi))+1
      sigspv(ii) = sapSigma
    ENDDO

! read the manual input
! ---------------------
  ELSE IF (sapSel == 2) THEN

    DO ii = 1,999
      nsaloc(ii) = 0
      grploc(ii) = 0
      DO jj = 1,maxsgr
        satloc(jj,ii) = 0
      ENDDO
    ENDDO

! Read the satellite groups
    CALL readKeys('SAPGRP', keyValue, irc)

    iGrpmax = 0
    DO ii = 1, SIZE(keyValue)
      READ(keyValue(ii), *, iostat=ios) (hlpStr(jj), jj=1,16)

      READ(hlpStr(1), *, iostat=ios) iGrp

      IF (ios == 0) THEN
        IF (iGrp > maxGrpNr) THEN
          WRITE(lfnerr,'(/,A,2(/,16X,A,I5),/)')                            &
           ' *** SR RDISAP: Satellite group number not allowed',           &
                    'Satellite antenna pattern group:      ',iGrp,         &
                    'Maximum group number allowed:         ',maxGrpNr
          CALL exitrc(2)
        ENDIF
        IF (iGrp > iGrpmax) iGrpmax = iGrp

! Get the list of satellites per group
        DO jj = 2, 16
          IF (LEN_TRIM(hlpStr(jj)) == 0) CYCLE
          nsaloc(iGrp) = nsaloc(iGrp) + 1

          IF (nsaloc(igrp) > maxsgr) THEN
            WRITE(lfnerr,'(/,A,3(/,16X,A,I5),/,16X,A,/)')                    &
             ' *** SR RDISAP: Too many satellites per group selected',       &
                      'Group for satellite antenna patterns: ',iGrp,         &
                      'Number of satellites:                 ',nsaloc(igrp), &
                      'Maximum number allowed:               ',maxsgr,       &
                      'Increase maxsat or reduce the num. of satellites.'
            CALL exitrc(2)
          ENDIF

          READ(hlpStr(jj), *, iostat=ios) satloc(nsaloc(iGrp),iGrp)
          IF (ios /= 0) nsaloc(iGrp) = nsaloc(iGrp) - 1
        ENDDO

! Generate the list of group IDs
        newGroup = .TRUE.
        DO jj = 1,iGrpmax
          IF (grploc(jj) == iGrp) newGroup = .FALSE.
        ENDDO

        IF (newGroup) THEN
          nanspv = nanspv + 1

          CALL dimtst(1,1,2,'rdisap','maxspv',               &
                      'satellite antenna pattern groups',    &
                      'Include file "GPSEST.inc" is used.',  &
                      nanspv,maxspv,irc)

          grploc(iGrp) = iGrp
        ENDIF
      ENDIF
    ENDDO

! Rearrange phase center variation groups
    jj = 0
    DO ii = 1,iGrpmax
      IF (nsaloc(ii) > 0) THEN
        jj = jj + 1
        nsaspv(jj) = nsaloc(ii)
        gnrspv(jj) = grploc(ii)
        DO isat = 1,nsaloc(ii)
          satspv(isat,jj) = satloc(isat,ii)
        ENDDO
        nptspv(1,jj) = IDNINT(nadmax/pi*180.D0/sapDnad)+1
        nptspv(2,jj) = IDNINT(360.D0/DBLE(sapDazi))+1
        sigspv(jj) = sapSigma
      ENDIF
    ENDDO

! Check whether all satellites of one group have the same name in the
! satellite information file
    CALL gtsata(maxsaa,nsaant,satant,santof,timint,satnam,satblk,sennum)

    DO jgrp = 1,nanspv
      IF (nsaspv(jgrp) > 1) THEN
        refnam = ' '
        DO isat = 1,nsaspv(jgrp)
          notfound = .TRUE.
          DO isaant = 1,nsaant
            IF ( satant(isaant) == satspv(isat,jgrp) .AND. &
                timint(1,isaant) < globalWindow%t(1) .AND. &
                timint(2,isaant) > globalWindow%t(2)) THEN
              notfound = .FALSE.
              IF (isat == 1) THEN
                refnam = satnam(1,isaant)
              ELSEIF (satnam(1,isaant) /= refnam) THEN
                WRITE(lfnerr,'(/,A,A,/,16X,A,3(/,16X,A,I5),/)')             &
                 ' ### SR RDISAP: Satellite (antenna) name in satellite ',  &
                          'antenna pattern group differs from',             &
                          'the name of the first satellite of the group',   &
                          'Internal group number :',jgrp,                   &
                          'External group number :',gnrspv(jgrp),           &
                          'Satellite number      :',satspv(isat,jgrp)
              ENDIF
              EXIT
            ENDIF
          ENDDO
          IF (notfound) THEN
            CALL timst2(1,1,globalWindow%t(1),string1)
            CALL timst2(1,1,globalWindow%t(2),string2)
            WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,2(/,16X,A,A),/,16X,A)') &
             ' ### SR RDISAP: Satellite (antenna) name not found in ',    &
                      'satellite information file',                       &
                      'Satellite number:',satspv(isat,jgrp),              &
                      'Start time      : ',string1,                       &
                      'End time        : ',string2,                       &
                      'No check of the satellite (antenna) name!'
          ENDIF
        ENDDO
      ENDIF
    ENDDO

! Read the satellite-specific input according to observed satellites
! ------------------------------------------------------------------
  ELSEIF (sapSel == 3) THEN

! Compile groups
! --------------
    nanspv=nallsat
    nsaspv(1:nanspv)=1
    gnrspv(1:nanspv)=allsatnum(1:nanspv)
    satspv(1,1:nanspv) = gnrspv(1:nanspv)

    DO ii = 1,nanspv
      nptspv(1,ii) = IDNINT(nadmax/pi*180.D0/sapDnad)+1
      nptspv(2,ii) = IDNINT(360.D0/DBLE(sapDazi))+1
      sigspv(ii) = sapSigma
    ENDDO

  ENDIF   !sapSel

! Deallocate local variables
! ---------------------------
  DEALLOCATE(keyValue,stat=irc)

  RETURN
END SUBROUTINE rdisap

END MODULE
