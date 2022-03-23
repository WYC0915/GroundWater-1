MODULE s_KININP
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE kininp(filNam,filKin,ifrmat,datdes,orbtyp,                     &
                  agency,satwgt,titpre,localWin,ichkkin,edtkin,iwrqxx)

! -------------------------------------------------------------------------
! Purpose:   Read all input information for the program KINPRE
!            from the input file
!
! Author:    D. Svehla
!
! Created:   12-Sep-2002
! Last mod.: 31-Jan-2012
!
! Changes:   07-Mar-2003 HU: SP3C implemented
!            16-Apr-2003 DS: Editing of kinematic orbit
!            17-Apr-2003 DS: Accuracy info
!            18-Apr-2003 DS: Edited KIN file output
!            23-Apr-2003 AJ: Nullify local pointers
!            19-May-2003 RD: Use SR gttimwin instead of SR readsess
!            31-Jan-2012 HB: Set ifrmat=4, no selection in INP-file
!
! SR used:
!
! Copyright: Astronomical Institute
!            University of Bern
!            Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE s_gtfile2
  USE s_gtfile
  USE s_gttimwin
  USE s_prfile
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptc
  USE s_ckopti
  USE s_ckoptl
  IMPLICIT NONE


! List of Parameters
! ------------------
  CHARACTER(LEN=fileNameLength),DIMENSION(:,:),POINTER :: filNam ! Input/Output File Names
  CHARACTER(LEN=fileNameLength),DIMENSION(:,:),POINTER :: filKin ! Edited kin file
  INTEGER(i4b)                   :: ifrmat  ! Format type
  CHARACTER(LEN=5)               :: datdes  ! Data description
  CHARACTER(LEN=3)               :: orbtyp  ! Orbit type
  CHARACTER(LEN=4)               :: agency  ! Agency generating orbit

  INTEGER(i4b)                   :: satwgt  ! LEO specific accuracy
  CHARACTER(LEN=57),DIMENSION(4) :: titpre  ! Title lines
  REAL(r8b),        DIMENSION(2) :: localWin! Time window
  INTEGER(i4b)                   :: ichkkin ! Apply editing (comparison with
                                            ! standard orbit)
                                            ! 0 = NO
                                            ! 1 = YES
  REAL(r8b)                      :: edtkin  ! Editing level
  INTEGER(i4b)                   :: iwrqxx  ! Write accuracy info

! List of Functions
! -----------------
! Local Types
! -----------
! Local Parameters
! ----------------
  INTEGER(i4b),PARAMETER :: maxFil=50

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength),DIMENSION(:),POINTER   :: keyValue
  CHARACTER(LEN=fileNameLength),DIMENSION(maxFil)      :: preFil
  CHARACTER(LEN=45)  :: srname

  INTEGER(i4b)       :: irCode
  INTEGER(i4b)       :: irc
  INTEGER(i4b)       :: ios
  INTEGER(i4b)       :: nflCol
  INTEGER(i4b)       :: nFil
  INTEGER(i4b)       :: mFil
  INTEGER(i4b)       :: iFil
  INTEGER(i4b)       :: kFil


! Initialization
! --------------
  NULLIFY(keyValue)

  irCode = 0
  srname = 'KININP'

! Get Input and Output File Names
! -------------------------------
  nflCol=2
  nFil=0
  CALL gtfile2('KININP',nflCol,nFil,filNam)
  CALL gtfile('PREOUT',1,maxFil,mFil,preFil)

  IF (mfil > 0) THEN
    IF (nFil /= mFil) THEN
      WRITE(lfnErr,'(/,2A,/,16X,A,I5,/,16X,A,/)') &
           ' *** SR KININP: Number of Input and ', &
           'Output Files is Different', &
           '# of LEO Orbit Files :',nFil, &
           '# of Precise Files : 1'
      CALL exitrc(2)
    ENDIF

    DO iFil = 1, mFil
      filnam(2,iFil) = preFil(iFil)
    ENDDO
  ENDIF
  CALL prfile('KININP', ' ', 2)

! Options
! -------
!  CALL readKeys('SENSOR',keyValue,irc)
!  CALL ckoptl(0,'SENSOR',keyValue,srName,                                 &
!              'On-board Sensor',irc,irCode,                               &
!              maxVal=1,result1=leonam)

! Read editing options
! --------------------
  ichkkin=0
  CALL readKeys('ICHKKIN', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1') ichkkin = 1

  IF (ichkkin == 1) THEN

  CALL gtfile2('KINOUT',1,kFil,filKin)
!    CALL gtfile2('KINOUT',1,maxFil,kFil,filKin)
    IF (nFil /= kFil) THEN
      WRITE(lfnErr,'(/,2A,/,16X,A,I5,/,16X,A,/)') &
         ' *** SR KININP: Number of Input and ', &
         'Output Files is Different', &
         '# of LEO Orbit Files :',nFil, &
         '# of Screened KIN Files : 1'
      CALL exitrc(2)
    ENDIF
  END IF

! Read edition level
! ------------------
  edtkin = 0d0
  CALL readKeys('EDTKIN', keyValue, irc)
  IF (irc == 0) READ(keyValue(1),*,iostat=ios) edtkin
  IF ( irc /= 0                                  .OR. &
      (ios /= 0 .AND. LEN_TRIM(keyValue(1)) > 0) .OR. &
       edtkin < 0d0 )                              THEN
    WRITE(lfnerr,'(/,A,/,16X,A,A,/)')                                      &
    ' *** SR KININP: Wrong editing level for kinematic orbit!', &
                    'Specified Value: ',TRIM(keyValue(1))
    CALL exitrc(2)
  ENDIF

! Write accuracy info
! -------------------
  iwrqxx=0
  CALL readKeys('IWRQXX', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1') iwrqxx = 1

!!! Type of pre file to be written
!!! ------------------------------
!!  CALL readKeys('TYPE', keyValue, irc)
!!  CALL ckoptc(1,'TYPE',keyValue,                         &
!!   (/'SV1,POS         ', 'SV1,POS+VEL     ',             &
!!     'SP3,POS+CLK     ', 'SP3,POS+VEL+CLK ',             &
!!     'SP3C,POS+CLK    ', 'SP3C,POS+VEL+CLK' /),&
!!       srName,'Format Type',irc,irCode,  &
!!       maxVal=1,valList=(/0,1,2,3,4,5/),result1=ifrmat)

  ifrmat = 4

  CALL readKeys('DATADES',keyValue,irc)
  CALL ckoptl(0,'DATADES',keyValue,srName,                                &
              'Data Description',irc,irCode,empty=' ',maxLength=5,        &
              maxVal=1,result1=datDes)

  CALL readKeys('ORBTYP',keyValue,irc)
  CALL ckoptl(0,'ORBTYP',keyValue,srName,                                 &
              'Orbit Type.',irc,irCode,empty=' ',maxLength=3,             &
              maxVal=1,result1=orbTyp)

  CALL readKeys('AGENCY',keyValue,irc)
  CALL ckoptl(0,'AGENCY',keyValue,srName,                                 &
              'Agency Generating the Orbit.',irc,irCode,empty=' ',        &
              maxLength=4,maxVal=1,result1=agency)

  CALL readKeys('TIPRE1',keyValue,irc)
  CALL ckoptl(0,'TIPRE1',keyValue,srName,                                 &
              'Title Line for Precise Orbit File.',irc,irCode,empty=' ',  &
              maxLength=57,maxVal=1,result1=titpre(1))

  CALL readKeys('TIPRE2',keyValue,irc)
  CALL ckoptl(0,'TIPRE2',keyValue,srName,                                 &
              'Title Line for Precise Orbit File.',irc,irCode,empty=' ',  &
              maxLength=57,maxVal=1,result1=titpre(2))

  CALL readKeys('TIPRE3',keyValue,irc)
  CALL ckoptl(0,'TIPRE3',keyValue,srName,                                 &
              'Title Line for Precise Orbit File.',irc,irCode,empty=' ',  &
              maxLength=57,maxVal=1,result1=titpre(3))

  CALL readKeys('TIPRE4',keyValue,irc)
  CALL ckoptl(0,'TIPRE4',keyValue,srName,                                 &
              'Title Line for Precise Orbit File.',irc,irCode,empty=' ',  &
              maxLength=57,maxVal=1,result1=titpre(4))

  CALL readKeys('ACCURA', keyValue, irc)
  CALL ckopti(1,'ACCURA',keyValue,srName,                                 &
              'Special LEO Accuracy.',irc,irCode,                         &
              maxVal=1,empty=0,error=0,result1=satwgt)

!  CALL readKeys('COOSYS',keyValue,irc)
!  CALL ckoptl(0,'COOSYS',keyValue,srName,                                 &
!              'LEO Coordinate System',irc,irCode,empty=' ',        &
!              maxLength=5,maxVal=1,result1=COOSYS)

! Read the Time Window
! --------------------
  CALL gttimwin(' ',(/'RADIO_0','RADIO_1','RADIO_2'/),        &
                (/'SESSION_YEAR','SESSION_STRG'/),            &
                (/'STADAT','STATIM','ENDDAT','ENDTIM'/),      &
                localWin)

! Stop if an error in the input options found
! -------------------------------------------
  IF (irCode /= 0) CALL exitrc(2)

  DEALLOCATE(keyValue,stat=irc)

  RETURN
END SUBROUTINE kininp

END MODULE
