MODULE s_RDIGIM
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdigim(maxgim, globalwindow, dtSim,  &
                  optgim, polgim, siggim, namgim, epogim)

! -------------------------------------------------------------------------
! Purpose:    Reads the global ionosphere input options for GPSEST
!
! Author:     R. Dach
!
! Created:    27-Jun-2001
! Last mod.:  21-May-2010
!
! Changes:    30-Jul-2001  RD: "Time window" is a special option
!             21-Aug-2001  RD: change naming of ionosphere models
!             18-Sep-2001  SS: New TEC mapping functions
!             21-Dec-2001  HU: Use d_const
!             13-Nov-2002  SS: Naming in case of dynamic model
!             27-Mar-2003  RD: New parameter time window definition
!             19-Jan-2003  SS/MM: Revision of GPSEST input panels
!             21-May-2010  MF: Nullify gimWin
!
! SR used:    readkeys, ckoptc, ckopti, ckoptr, ckoptb, ckoptt, gtflna,
!             exitrc, rdpwin, parint, dimtst
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_time,   ONLY: t_timint
  USE d_const,  ONLY: pi
  USE s_ckoptr
  USE s_dimtst
  USE s_ckoptt
  USE s_parint
  USE s_rdpwin
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  USE s_ckoptc
  USE s_gtflna
  USE s_ckopti
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)               :: maxgim       ! maximum number of
                                             ! global ionosphere models
  TYPE(t_timint)             :: globalWindow ! window to be processed
                                             ! (from - to, MJD)
  REAL(r8b)                  :: dtsim        ! max. interval to identify
                                             ! epoch (in days)

! output:
  INTEGER(i4b), DIMENSION(*) :: optgim ! options for global ionosphere model
                                       ! (1): maximum degree
                                       ! (2): maximum order
                                       ! (3): flag for reference frame
                                       !      =1: geographical
                                       !      =2: geomagnetic
                                       ! (4): flag for position of the sun
                                       !      =1: mean
                                       !      =2: true
                                       ! (5): estimation of layer height
                                       !      =0: no
                                       !      =1: one parameter in all
                                       !      =2: one parameter per model
                                       ! (6): mode of temporal modeling
                                       !      =1: static model
                                       !      =2: dynamic model
                                       ! (7): total number of models
                                       ! (8): mapping function
                                       !      =0: none
                                       !      =1: cosz (single-layer model)
                                       !      =2: mslm (modified slm)
                                       !      =3: esm (extended slab model)
                                       ! (9): station-specific models
                                       ! (10): component to be estimated
                                       !       =1: deterministic
                                       !       =2: stochastic
  REAL(r8b), DIMENSION(3,*) :: polgim  ! i=1: height of single layer (m)
                                       ! i=2: lat. of north geomagnetic pole
                                       ! i=3: east longitude
  REAL(r8b), DIMENSION(*)   :: siggim  ! absolute sigma for
                                       ! (1): ion. coefficients (tecu)
                                       ! (2): single-layer height (m)
                                       ! relative sigma for
                                       ! (3): ion. coefficients (tecu)
                                       ! (4): single-layer height (m)
  CHARACTER(LEN=16),        &
               DIMENSION(*) :: namgim  ! model numbers
  REAL(r8b), DIMENSION(2,*) :: epogim  ! periods of validity / ref epochs (mjd)

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER :: srName = 'rdigim'

! GIM coeff-Keywords
  CHARACTER(LEN=keyNameLength), DIMENSION(17), PARAMETER :: gimKeyw = &
      (/ 'MAXDEG ', 'MAXORD ', 'REFFRM ', 'LONSUN ', 'GLOBION', &
         'STADYN ', 'NUMION ', 'MAPFUN ', 'MLTMOD ', '       ', &
         'HGTION ', 'LATPOL ', 'LONPOL ',                       &
         'SIGCOE ', 'RELCOE ', 'SIGHGT ', 'RELHGT '             /)

! Error messages for GIM-Keywords
  CHARACTER(LEN=shortLineLength), DIMENSION(17), PARAMETER :: gimErr = &
       (/ 'Maximum Degree of Spherical Harmonics', &
          'Maximum Order of Spherical Harmonics ', &
          'Reference Frame Definition           ', &
          'Longitude of the Sun                 ', &
          'Estimation of Single-Layer Height    ', &
          'Mode of Temporal Modeling            ', &
          'Number of Coefficient Sets           ', &
          'Mapping Function                     ', &
          'Station-Specific Models              ', &
          '                                     ', &
!
          'A Priori Height of Single Layer      ', &
          'Latitude  of Geomagnetic Pole        ', &
          'Longitude of Geomagnetic Pole        ', &
!
          'Absolute Sigma for Coefficients      ', &
          'Relative Sigma for Coefficients      ', &
          'Absolute Sigma for Height Parameters ', &
          'Relative Sigma for Height Parameters '  /)

! Local Variables
! ---------------
  TYPE(t_timint),                  &
            DIMENSION(:), POINTER :: gimWin

  CHARACTER(LEN=keyValueLength) , &
         DIMENSION(:)  , POINTER  :: keyValue
  CHARACTER(LEN=fileNameLength)   :: ionFile
  CHARACTER(LEN=staNameLength)    :: gimName

  INTEGER(i4b)                    :: ionMod
  INTEGER(i4b)                    :: iOpt
  INTEGER(i4b)                    :: ii
  INTEGER(i4b)                    :: irCode
  INTEGER(i4b)                    :: irc

  REAL(r8b)                       :: t_0,dt_0
  REAL(r8b)                       :: gimTim
  REAL(r8b)                       :: rHlp

! Init some variables
! -------------------
  irCode = 0

  NULLIFY(keyValue)
  NULLIFY(gimWin)

  optgim(1:10) = 0

! Detect which type of model is requested
! ---------------------------------------
  ionMod = 1

! Number of height parameters
! ---------------------------
  CALL readKeys('HGTEST', keyValue, irc)
  CALL ckoptc(1,'HGTEST', keyValue,(/'NONE','ALL ','EACH'/), srName,     &
              'Ionosphere height setup',irc,irCode,                 &
              maxVal=1,valList=(/0,1,2/),error=0,result1=optgim(5))

  IF (optgim(5) > 0) ionMod = 2

  CALL gtflna(0,'IONOS', ionFile, irc)
  IF (irc /= 0 .OR. LEN_TRIM(ionFile) == 0) ionMod = 1

! Model type height
! -----------------
  IF (ionMod == 2) THEN

! General setting for:
! -------------------
!!    optgim(1:10) = 0
    optgim( 3)    = 1    ! Reference Frame Definition: geographical
    optgim( 4)    = 1    ! Longitude of the Sun      : mean
    optgim( 6)    = 1    ! Mode of temporal modeling : static
    optgim( 8)    = 1    ! Mapping function          : cosz
    optgim(10)    = 1    ! Component to be estimated : deterministic

! Read GIM model characteristics
! ------------------------------
    siggim(1:4) = 0d0
    DO iOpt = 1, 2

      CALL readKeys(gimKeyw(iOpt+15), keyValue, irc)

      CALL ckoptr(1,gimKeyw(iOpt+15), keyValue, srName,               &
                  gimErr(iOpt+15),irc,irCode,                         &
                  maxVal=1,empty=0d0,ge=0d0, result1=rHlp)

      siggim(iOpt*2) = 1d3 * rHlp
    ENDDO


! Model type coefficients
! -----------------------
  ELSE IF (ionMod == 1) THEN

! Read GIM estimation options
! ---------------------------
    optgim(1:9) = 0
    optgim(10)  = 1
    DO iOpt = 1, 9

      ! Degree and order of the model
      IF (iOpt == 1 .OR. iOpt == 2) THEN

        CALL readKeys(gimKeyw(iOpt), keyValue, irc)

        CALL ckopti(1,gimKeyw(iOpt), keyValue, srName,  &
                    gimErr(iOpt),irc,irCode,            &
                    maxVal=1,ge=0,result1=optgim(iOpt))

      ! Reference frame
      ELSE IF (iOpt == 3) THEN

        CALL readKeys(gimKeyw(iOpt), keyValue, irc)

        CALL ckoptc(1,gimKeyw(iOpt), keyValue,          &
                    (/'GEOGRAPHIC ','GEOMAGNETIC'/),    &
                    srName,gimErr(iOpt),irc,irCode,     &
                    maxVal=1,result1=optgim(iOpt))

      ! Longitude of the sun
      ELSE IF (iOpt == 4) THEN

        CALL readKeys(gimKeyw(iOpt), keyValue, irc)

        CALL ckoptc(1,gimKeyw(iOpt), keyValue,          &
                    (/'MEAN','TRUE'/),                  &
                    srName,gimErr(iOpt),irc,irCode,     &
                    maxVal=1,result1=optgim(iOpt))

      ! Ionosphere model
      ELSE IF (iOpt == 5) THEN
        optgim(iOpt) = 0

      ! Mode of temporal modelling
      ELSE IF (iOpt == 6) THEN

        CALL readKeys(gimKeyw(iOpt), keyValue, irc)

        CALL ckoptc(1,gimKeyw(iOpt), keyValue,          &
                    (/'STATIC ','DYNAMIC'/),            &
                    srName,gimErr(iOpt),irc,irCode,     &
                    maxVal=1,result1=optgim(iOpt))

      ! Number of models
      ELSE IF (iOpt == 7) THEN

        CALL readKeys(gimKeyw(iOpt), keyValue, irc)

        CALL ckoptt(1,gimKeyw(iOpt), keyValue, srName,  &
                    gimErr(iOpt),irc,irCode,            &
                    maxVal=1,gt=0d0,result1=gimTim)

        ! Get the parameter time window definition
        CALL rdpwin('PAR_OFF',(/' ',' '/),t_0,dt_0)

        ! Get the corresponding time windows
        CALL parint(globalWindow,dtSim,t_0,dt_0,gimTim, &
                    'Global ionosphere models',optgim(iOpt),gimWin)

        ! One paarmeter more for dynamic models
        IF (optgim(6) == 2) optgim(iOpt) = optgim(iOpt) + 1

      ! Mapping function
      ELSE IF (iOpt == 8) THEN

        CALL readKeys(gimKeyw(iOpt), keyValue, irc)

        CALL ckoptc(1,gimKeyw(iOpt), keyValue,          &
                    (/'NONE','COSZ','MSLM','ESM '/),    &
                    srName,gimErr(iOpt),irc,irCode,     &
                    maxVal=1,valList=(/0,1,2,3/),result1=optgim(iOpt))

      ! Station specific model
      ELSE IF (iOpt == 9) THEN

        CALL ckoptb(1,(/gimKeyw(iOpt)/),srName,         &
                    gimErr(iOpt),irCode,                &
                    result1=optgim(iOpt))

      ENDIF

    ENDDO

! Check maximum dimension
! -----------------------
    CALL dimtst(1,1,2,'rdigim','maxgim',               &
                'global ionosphere models',            &
                'Include file "MAXGIM.inc" is used.',  &
                optgim(7),maxgim,irc)

! Get the basic name of the iono-models
! -------------------------------------
    gimName = ''
    CALL readKeys('GIMNAM',keyValue,irc)
    IF (irc == 0) gimName = keyValue(1)

! Read GIM model characteristics
! ------------------------------
    polgim(1:3,1:optgim(7)) = 0d0
    DO iOpt = 1, 3

      CALL readKeys(gimKeyw(iOpt+10), keyValue, irc)

      CALL ckoptr(1,gimKeyw(iOpt+10), keyValue, srName, &
                  gimErr(iOpt+10),irc,irCode,           &
                  maxVal=1,result1=rHlp)

      IF (iOpt == 1) THEN
        polgim(iOpt,1:optgim(7)) = 1d3 * rHlp
      ELSE
        polgim(iOpt,1:optgim(7)) = PI/180d0 * rHlp
      ENDIF
    ENDDO


! Read GIM model characteristics
! ------------------------------
    siggim(1:4) = 0d0
    DO iOpt = 1, 2

      CALL readKeys(gimKeyw(iOpt+13), keyValue, irc)

      CALL ckoptr(1,gimKeyw(iOpt+13), keyValue, srName,  &
                  gimErr(iOpt+13),irc,irCode,            &
                  maxVal=1,empty=0d0,ge=0d0,result1 = rHlp)

      siggim(iOpt*2-1) = rHlp
    ENDDO


! Compute the time intervals
! --------------------------
    epogim(1:2,1:maxgim) = 0d0

! for static models
    IF (optgim(6) == 1) THEN

      DO ii = 1, optgim(7)
        epogim(1:2,ii) = gimWin(ii)%t

        namgim(ii) = gimName(1:4)
        IF (optgim(7) > 1) THEN
          WRITE(namgim(ii)(5:7),'(a,i2.2)') '-', ii
        ELSE
          WRITE(namgim(ii)(5:7),'(a,i2.2)') '-', 0
        ENDIF
      ENDDO

! for dynamical models
    ELSEIF (optgim(6) == 2) THEN
      IF (optgim(7) == 1) THEN
        WRITE(lfnerr,'(/,A,/,16X,A,/)')                                       &
        ' *** SR RDIGIM: At least two sets of coefficients have to be set up',&
                        'for dynamic ionosphere models.'
        CALL exitrc(2)
      ENDIF

      DO ii = 1, optgim(7)
        IF (ii < optGim(7)) THEN
          epogim(1,ii) = gimWin(ii)%t(1)
        ELSE
          epogim(1,ii) = gimWin(ii-1)%t(2)
        ENDIF

        namgim(ii) = gimName
        WRITE(namgim(ii)(5:7),'(a,i2.2)') '-', ii

      ENDDO

    ENDIF

  ENDIF

  DEALLOCATE(gimWin, stat=irc)
  DEALLOCATE(keyValue,stat=irc)

  IF (irCode /= 0) CALL exitrc(2)

  RETURN
END SUBROUTINE rdigim

END MODULE
