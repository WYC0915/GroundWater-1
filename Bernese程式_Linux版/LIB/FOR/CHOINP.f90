MODULE s_CHOINP
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE choinp(filNam,leonam,ifrmat,datdes,orbtyp,agency,satwgt,titpre)

! -------------------------------------------------------------------------
! Purpose:   Read all input information for the program CHOPRE
!            from the input file
!
! Author:    D. Svehla
!
! Created:   23-Jan-2001
! Last mod.: 23-Apr-2003
!
! Changes:   22-Oct-2001 HB: F90, new input file
!            12-Nov-2002 HU: SP3c format implemented
!            23-Apr-2003 CU: Nullify local pointers
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
  CHARACTER(LEN=staNam2Length)   :: leonam  ! On-board sensor
  INTEGER(i4b)                   :: ifrmat  ! Format type
  CHARACTER(LEN=5)               :: datdes  ! Data description
  CHARACTER(LEN=3)               :: orbtyp  ! Orbit type
  CHARACTER(LEN=4)               :: agency  ! Agency generating orbit

  INTEGER(i4b)                   :: satwgt  ! LEO specific accuracy
  CHARACTER(LEN=57),DIMENSION(3) :: titpre  ! Title lines

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
  INTEGER(i4b)       :: nflCol
  INTEGER(i4b)       :: nFil
  INTEGER(i4b)       :: mFil
  INTEGER(i4b)       :: iFil

! Initialization
! --------------
  irCode = 0
  srname = 'CHOINP'

  NULLIFY(keyValue)

! Get Input and Output File Names
! -------------------------------
  nflCol=2
  nFil=0
  CALL gtfile2('CHOINP',nflCol,nFil,filNam)
  CALL gtfile('PREOUT',1,maxFil,mFil,preFil)

  IF (mfil > 0) THEN
    IF (nFil /= mFil) THEN
      WRITE(lfnErr,'(/,2A,/,16X,A,I5,/,16X,A,/)') &
           ' *** SR CHOINP: Number of Input and ', &
           'Output Files is Different', &
           '# of CHAMP Orbit Files :',nFil, &
           '# of Precise Files : 1'
      CALL exitrc(2)
    ENDIF

    DO iFil = 1, mFil
      filnam(2,iFil) = preFil(iFil)
    ENDDO
  ENDIF

  CALL prfile('CHOINP', ' ', 2)

! Options
! -------
  CALL readKeys('SENSOR',keyValue,irc)
  CALL ckoptl(0,'SENSOR',keyValue,srName,                                 &
              'On-board Sensor',irc,irCode,                               &
              maxVal=1,result1=leonam)

  CALL readKeys('TYPE', keyValue, irc)
  CALL ckoptc(1,'TYPE',keyValue,                                           &
           (/'SV1,POS         ', 'SV1,POS+VEL     ', 'SP3,POS+CLK     ',   &
             'SP3,POS+VEL+CLK ', 'SP3C,POS+CLK    ', 'SP3C,POS+VEL+CLK' /),&
              srName,'Format Type',irc,irCode,  &
              maxVal=1,valList=(/0,1,2,3,4,5/),result1=ifrmat)

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

  CALL readKeys('ACCURA', keyValue, irc)
  CALL ckopti(1,'ACCURA',keyValue,srName,                                 &
              'Special LEO Accuracy.',irc,irCode,                         &
              maxVal=1,empty=0,error=0,result1=satwgt)

  DEALLOCATE(keyValue,stat=irc)

! Stop if an error in the input options found
! -------------------------------------------
  IF (irCode /= 0) CALL exitrc(2)

  RETURN
END SUBROUTINE choinp

END MODULE
