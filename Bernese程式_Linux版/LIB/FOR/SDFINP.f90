MODULE s_SDFINP
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE sdfinp(simdT, ambdT, ambSlp, iMost, optMin, optFoc, redBas, &
                  redMin, redImp, bonMax, bonLen, crtSat, crtBas)

! -------------------------------------------------------------------------
! Purpose:    This is a new version of the old subroutine SDFINP.f that
!             reads the input options of the program SNGDIF
!
! Author:     L. Mervart
!
! Created:    02-Jun-2000
!
! Changes:    22-Oct-2001 MM: changes for new menu
!             16-Nov-2001 MM: new (and renaming of) strategies and options
!             23-Apr-2003 RD: Nullify local pointers
!             17-May-2004 RD: Option for merging ambiguities
!             13-Dec-2006 SS: Allow optFoc=0
!             19-Jul-2007 SS: Add bonus depending on baseline length
!             25-Feb-2011 RD: Handle marginally observ. satellites
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern

  USE s_exitrc
  USE s_ckoptr
  USE s_ckoptb
  USE s_ckoptc
  USE s_readkeys
  USE s_ckopti
  IMPLICIT NONE

! List of Parameters
! ------------------
  REAL(r8b)    :: simdT  ! maximal time interval to identify
                         ! simultaneous observations (in days)
  REAL(r8b)    :: ambdT  ! set up of a new ambiguity for a satellite
                         ! disappearing for more than a time "ambdT" (in days)
                         ! (0d0: Merge only ambiguities from input files)
  INTEGER(i4b) :: ambSlp ! new ambiguity if cycle slip flag (1 = YES)
  INTEGER(i4b) :: iMost  ! most observations? (0:NO  1:YES)
  INTEGER(i4b) :: optMin ! miminum number of observations
  INTEGER(i4b) :: optFoc ! max length of short baselines
                         ! (fast observation counting)
  INTEGER(i4b) :: redBas ! redundant baselines (0/1)
  REAL(r8b)    :: redMin ! minumum length of a red. baseline
  REAL(r8b)    :: redImp ! minumum improvement in the shortest way
                         ! between two baselines
  REAL(r8b)    :: bonMax ! Bonus depending on baseline length (0: NO)
  REAL(r8b), DIMENSION(3) :: bonLen ! List of maximum baseline lengths
  REAL(r8b)    :: crtSat ! Percentage to identify a marginally obs. satellite
  INTEGER(i4b) :: crtBas ! Maximum number of additional baselines

! Local variables
! ---------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue
  CHARACTER(LEN=fileNameLength)                        :: srName
  LOGICAL                                              :: help
  INTEGER(i4b)                                         :: irc, ircSum

! Initialize some variables
! -------------------------
  srName = 'sdfinp'
  ambSlp = 0
  optMin = 0
  optFoc = 0
  redBas = 0
  redMin = 0
  redImp = 0
  bonMax = 0d0
  bonLen(:) = 0d0
  crtSat = 0d0
  crtBas = 0
  ircSum = 0

  NULLIFY(keyValue)


! Read all options
! ----------------

! dT for simultaneous observations
  CALL readkeys('SIMDT',keyValue,irc)
  CALL ckoptr(1,'SIMDT',keyValue,srName,                                   &
              'dT for simultaneous observations',irc,ircSum,               &
              maxVal=1,ge=0d0,result1=simdT)
  simdT = simdT/86400.0

! Merge ambiguities
  CALL ckoptb(1,(/'AMBMRG'/),srName,                                       &
              'Merge ambiguties from input files',ircSum,                  &
              resultL=help)
  IF (help) THEN
    ambDt = 0d0

! dT for new ambiguity
  ELSE
    CALL readkeys('AMBDT',keyValue,irc)
    CALL ckoptr(1,'AMBDT',keyValue,srName,                                 &
                'dT for setting up a new ambiguity',irc,ircSum,            &
                maxVal=1,empty=0d0,ge=0d0,result1=ambdT)
    IF (ambdT /= 0d0) ambdT = ambdT/1440.0
  ENDIF

! new ambiguity if CS flag
  IF (ambdt /= 0d0) THEN
    CALL ckoptb(1,(/'AMBSLP'/),srName,                                     &
                'new ambiguity if cycleslip flag is set',ircSum,           &
                resultL=help)
    IF (help) ambSlp = 1
  ENDIF

! check Strategy to set iMost
  CALL readkeys('STRATEGY',keyValue,irc)
  CALL ckoptc(1,'STRATEGY',keyValue,                                       &
             (/'MANUAL  ','OBS-MAX ','SHORTEST','STAR    ','DEFINED '/),   &
              srName,'Processing strategy',irc,ircSum,                     &
              maxVal=1,valList=(/0,1,0,0,0/),result1=iMost)

! Read remaining options only in case of OBS-MAX
! ----------------------------------------------
  IF (iMost == 1) THEN

! minimum number of common observations
    CALL readkeys('OPTMIN',keyValue,irc)
    CALL ckopti(1,'OPTMIN',keyValue,srName,                                &
                'Minimum number of observations',irc,ircSum,               &
                maxVal=1,ge=1,result1=optMin)

! maximum baseline length for foc
    CALL readkeys('OPTFOC', keyValue, irc)
    CALL ckopti(1,'OPTFOC',keyValue,srName,                                &
                'Maximum baseline length',irc,ircSum,                      &
                maxVal=1,ge=0,result1=optFoc)

! allow redundant baselines
    CALL ckoptb(1,(/'REDBAS'/),srName,                                     &
                'allow redundant baselines',ircSum,                        &
                resultL=help)

    IF (help) THEN
      redBas = 1

! minimum distance for redundant baselines
      CALL readkeys('REDMIN', keyValue, irc)
      CALL ckoptr(1,'REDMIN',keyValue,srName,                              &
                  'Minimum length for redundant baseline',irc,ircSum,      &
                  maxVal=1,ge=0d0,result1=redMin)
      redMin = redMin*1000d0

! minimum length improvement
      CALL readkeys('REDIMP', keyValue, irc)
      CALL ckoptr(1,'REDIMP',keyValue,srName,                              &
                  'Min. length improvement for red. baseline',irc,ircSum,  &
                  maxVal=1,ge=0d0,result1=redImp)
      redImp = redImp*1000d0

! additional baselines for marginally observed satellites:
! 1) Percentage of observations to identify such a satellite
      CALL readkeys('CRTSAT', keyValue, irc)
      CALL ckoptr(1,'CRTSAT',keyValue,srName,                              &
                  'Percentage to identify a marginally obs. sat.',irc,ircSum,  &
                  maxVal=1,ge=0d0,result1=crtSat)
      crtSat = crtSat/100d0

! 2) Maximum number of additional baselines
      CALL readkeys('CRTBAS', keyValue, irc)
      CALL ckopti(1,'CRTBAS',keyValue,srName,                              &
                  'Maximum number of asdditional satellites',irc,ircSum,   &
                  maxVal=1,ge=0,result1=crtBas)

    ENDIF

! Bonus depending on baseline length
    CALL readkeys('BONMAX',keyValue,irc)
    CALL ckoptr(1,'BONMAX',keyValue,srName,                                &
                'Bonus depending on baseline length',irc,ircSum,           &
                maxVal=1,empty=0d0,ge=0d0,result1=bonMax)
    bonMax = bonMax/100d0

    IF (bonMax > 0d0) THEN

! Maximum baseline length 1
      CALL readkeys('BONLEN1', keyValue, irc)
      CALL ckoptr(1,'BONLEN1',keyValue,srName,                             &
                  'Maximum baseline length 1',irc,ircSum,                  &
                  maxVal=1,ge=0d0,result1=bonLen(1))
! Maximum baseline length 2
      CALL readkeys('BONLEN2', keyValue, irc)
      CALL ckoptr(1,'BONLEN2',keyValue,srName,                             &
                  'Maximum baseline length 2',irc,ircSum,                  &
                  maxVal=1,ge=0d0,result1=bonLen(2))
! Maximum baseline length 3
      CALL readkeys('BONLEN3', keyValue, irc)
      CALL ckoptr(1,'BONLEN3',keyValue,srName,                             &
                  'Maximum baseline length 3',irc,ircSum,                  &
                  maxVal=1,ge=0d0,result1=bonLen(3))

      bonLen(:) = bonLen(:)*1000d0
    ENDIF
  ENDIF

! All options read, on error exit
! -------------------------------
  IF (ircSum /= 0) CALL exitrc(2)

! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=irc)

END SUBROUTINE sdfinp

END MODULE
