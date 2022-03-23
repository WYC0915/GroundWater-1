MODULE s_SUBINP
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE subinp(filNam,idfOpt,ifrTyp,incexc,nioe,ioeTyp,ioemlt,&
                  dtSamp,window)

! -------------------------------------------------------------------------
! Purpose:   Read Input File for Program SUBDIF (new F90-version of SUBINP)
!
! Author:    H.Bock
!
! Created:   24-oct-2001
!
! Changes:   23-May-2003 PS: corrected sampling time
!            23-May-2003 PS: allow special output name for comparison only for
!                            only for one file
!            27-Apr-2012 RD: Nullify all pointers, use m_bern with only
!
! Copyright: Astronomical Institute
!            University of Bern
!            Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnerr, &
                      fileNameLength, keyValueLength
  USE s_gtfile2
  USE s_ckoptr
  USE s_alcerr
  USE s_ckoptu
  USE s_gttimwin
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  USE s_ckoptc
  IMPLICIT NONE

! List of Parameters
! ------------------
! OUT:
  CHARACTER(LEN=fileNameLength),DIMENSION(:,:),POINTER :: filNam ! Input File Names
  INTEGER(i4b),DIMENSION(2)   :: idfOpt  ! OPTIONS FOR THE COMPUTATION OF DIFF.   I
                                         ! (1)=0: RMS ONLY OVER TERMS AVAILABLE
                                         !        IN BOTH MODELS
                                         ! (1)=1: RMS OVER ALL TERMS AVAILABLE
                                         !        IN ONE OR THE OTHER MODELS
                                         ! (2)=0: DIFFERENCES BETWEEN MODELS NOT
                                         !        PRINTED
                                         ! (2)=1: PRINT DIFFERENCES BETWEEEN MODELS
  INTEGER(i4b),DIMENSION(2,4) :: ifrTyp  ! FREQUENCY BANDS TO
                                         ! BE INCLUDED IN THE Statistics
                                         ! I=1: POLAR MOTION
                                         ! I=2: UT
                                         ! K=1: DIURNAL PROGRADE
                                         ! K=2: DIURNAL RETROGRADE
                                         ! K=3: SEMI-D. PROGRADE
                                         ! K=4: SEMI-D. RETROGRADE
  INTEGER(i4b)                :: incexc  ! INCLUDE OR EXCLUDE FLAG FOR LIST OF
                                         ! TERMS IN "IOEMLT"
                                         ! =-1: EXCLUDE TERMS LISTED IN "IOEMLT"
                                         ! = 0: IGNORE  TERMS LISTED IN "IOEMLT"
                                         ! = 1: INCLUDE TERMS IN "IOEMLT" ONLY
  INTEGER(i4b)                :: nioe    ! NUMBER OF TERMS TO BE INCL/EXCL
  INTEGER(i4b),DIMENSION(:),POINTER   :: ioeTyp  ! TYPE OF TERM TO BE INCLUDED/EXCLUDED
                                                 ! = 1: POLAR MOTION; = 2: UT
  INTEGER(i4b),DIMENSION(:,:),POINTER :: ioemlt  ! MULTIPLIERS OF TERMS TO BE INCLUDED/EXCLUDED
  REAL(r8b)                   :: dtSamp  ! SAMPLING TIME INTERVAL FOR SUBDAILY
                                         ! SERIES IN DAYS
  REAL(r8b),DIMENSION(2)      :: window  ! Time Window

! List of Functions
! -----------------
! Local Types
! -----------
! Local Parameters
! ----------------

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength),DIMENSION(:),  POINTER   :: keyValue
  CHARACTER(LEN=keyValueLength),DIMENSION(:,:),POINTER   :: hlppmt
  CHARACTER(LEN=keyValueLength),DIMENSION(:,:),POINTER   :: hlputt
  CHARACTER(LEN=fileNameLength),DIMENSION(:,:),POINTER   :: filHlp1
  CHARACTER(LEN=fileNameLength),DIMENSION(:,:),POINTER   :: filHlp2
  CHARACTER(LEN=45)                                      :: srname


  INTEGER(i4b) :: irCode
  INTEGER(i4b) :: ircSave
  INTEGER(i4b) :: irc
  INTEGER(i4b) :: iac
  INTEGER(i4b) :: iTerm
  INTEGER(i4b) :: iComp
  INTEGER(i4b) :: imlt
  INTEGER(i4b) :: nFil1
  INTEGER(i4b) :: nFil2
  INTEGER(i4b) :: nFil

  LOGICAL      :: hlp

! Initialization
! --------------
  NULLIFY(keyValue)
  NULLIFY(hlputt)
  NULLIFY(hlppmt)
  NULLIFY(filHlp1)
  NULLIFY(filHlp2)

  irCode = 0
  srName = 'SUBINP'

! Read list of input file names
! -----------------------------
  CALL gtfile2('SUBINP',2,nFil1,filHlp1)

! Options for Comparison
! ----------------------
  CALL readKeys('TRMPRS', keyValue, irc)
  CALL ckoptc(1,'TRMPRS',keyValue,                                          &
       (/ 'NO ','YES' /),srName,'Use Terms Present in one Model Only',      &
       irc,irCode,maxVal=1,valList=(/0,1/),result1=idfOpt(1))

  CALL readKeys('PRTDIF', keyValue, irc)
  CALL ckoptc(1,'PRTDIF',keyValue,                                          &
       (/ 'NO ','YES' /),srName,'Print Differences Between Models',         &
       irc,irCode,maxVal=1,valList=(/0,1/),result1=idfOpt(2))

  CALL readKeys('ICOMP', keyValue, irc)
  CALL ckoptc(1,'ICOMP',keyValue,                                           &
       (/ 'NO ','YES' /),srName,'Comparison Between Models',                &
       irc,irCode,maxVal=1,valList=(/0,1/),result1=iComp)
  IF (iComp==0) THEN
    dtSamp=0.D0
    window(:)=0.D0
    nFil=nFil1
    nFil2=-1
  ELSE
    CALL gtfile2('ERPOUT',1,nFil2,filHlp2)

    IF (nFil1>=nFil2) THEN
      nFil=nFil1
    ELSEIF (nFil1<nFil2) THEN
      CALL exitrc(2)
    ENDIF

! Time Window
! -----------
! Read infomation for the time window
! -----------------------------------
  CALL gttimwin(' ',(/'RADIO_1','RADIO_2'/),               &
                (/'SESSION_YEAR','SESSION_STRG'/),         &
                (/'STADAT','STATIM','ENDDAT','ENDTIM'/), window(:))
  ENDIF

  ALLOCATE(filNam(2,nFil),stat=iac)
  CALL alcerr(iac,'filNam',(/2,nFil/),'subinp')
  filNam(:,:)=' '
  filNam(1,:) = filHlp1(1,:)
  IF (nFil2>0) THEN
    filNam(2,:) = filHlp2(1,:)
  ELSEIF (nFil2==0) THEN
    filNam(2,:) = filHlp1(2,:)
  ENDIF

  IF (nFil1 > 1 .AND. nFil2 .NE. 0) THEN
    WRITE(lfnerr,'(A,/,A,/,A)')  &
      ' *** PGM SUBDIF: too many input files', &
      '                 You can only specifiy a output file name', &
      '                 for comparison if you use one input file'
      CALL exitrc(2)
    ENDIF


! Sampling Rate
! -------------
  CALL readKeys('SAMPL', keyValue, irc)
  CALL ckoptr(1,'SAMPL',keyValue,srName,                                    &
              'Sampling Rate.',irc,irCode,                                  &
              maxVal=1,gt=0.D0,error=0.D0,result1=dtSamp)
  dtSamp=dtSamp/1440.D0

! Frequencies to be Included
! --------------------------
  ifrTyp(:,:)=0
  CALL ckoptb(1,(/'PMDP'/),srName,'Frequencies to be Included: PMDP',       &
       irCode,resultL=hlp)
  IF (hlp) ifrTyp(1,1)=1

  CALL ckoptb(1,(/'PMDR'/),srName,'Frequencies to be Included: PMDR',       &
       irCode,resultL=hlp)
  IF (hlp) ifrTyp(1,2)=1

  CALL ckoptb(1,(/'PMSP'/),srName,'Frequencies to be Included: PMSP',       &
       irCode,resultL=hlp)
  IF (hlp) ifrTyp(1,3)=1

  CALL ckoptb(1,(/'PMSR'/),srName,'Frequencies to be Included: PMSR',       &
       irCode,resultL=hlp)
  IF (hlp) ifrTyp(1,4)=1

  CALL ckoptb(1,(/'UTDP'/),srName,'Frequencies to be Included: UTDP',       &
       irCode,resultL=hlp)
  IF (hlp) ifrTyp(2,1)=1

  CALL ckoptb(1,(/'UTDR'/),srName,'Frequencies to be Included: UTDR',       &
       irCode,resultL=hlp)
  IF (hlp) ifrTyp(2,2)=1

  CALL ckoptb(1,(/'UTSP'/),srName,'Frequencies to be Included: UTSP',       &
       irCode,resultL=hlp)
  IF (hlp) ifrTyp(2,3)=1

  CALL ckoptb(1,(/'UTSR'/),srName,'Frequencies to be Included: UTSR',       &
       irCode,resultL=hlp)
  IF (hlp) ifrTyp(2,4)=1

! List of terms used?
! -------------------
  CALL readKeys('USELST', keyValue, irc)
  CALL ckoptc(1,'USELST',keyValue,                                          &
       (/ 'DO_NOT_USE','INCLUDE   ','EXCLUDE   ' /),srName,                 &
       'How to use List of Terms',irc,irCode,                               &
       maxVal=1,valList=(/0,1,-1/),result1=incexc)

! List of Terms
! -------------
  CALL readKeys('PMTERM',keyValue,irc)

  ALLOCATE (hlppmt(7,SIZE(keyValue)),stat=iac)
  CALL alcerr(iac,'hlppmt',(/7,SIZE(keyValue)/),'subinp')

  ircSave = irCode
  CALL ckoptu(1,'PMTERM',keyValue,srName,'PM-Terms',irc,irCode,7,           &
       maxVal=SIZE(hlppmt,2),result2=hlppmt)
  ircSave = irCode - ircSave

  CALL readKeys('UTTERM',keyValue,irc)

  ALLOCATE (hlputt(7,SIZE(keyValue)),stat=iac)
  CALL alcerr(iac,'hlputt',(/7,SIZE(keyValue)/),'subinp')

  ircSave = irCode
  CALL ckoptu(1,'UTTERM',keyValue,srName,'UT-Terms',irc,irCode,7,           &
       maxVal=SIZE(hlputt,2),result2=hlputt)
  ircSave = irCode - ircSave

  nioe = SIZE(hlppmt,2) + SIZE(hlputt,2)

! Allocate memory
! ---------------
  ALLOCATE (ioeTyp(nioe),stat=iac)
  CALL alcerr(iac,'ioeTyp',(/nioe/),'subinp')
  ALLOCATE (ioemlt(6,nioe),stat=iac)
  CALL alcerr(iac,'ioemlt',(/6,nioe/),'subinp')

  DO iTerm = 1,SIZE(hlppmt,2)
    ioeTyp(iTerm) = 1
    DO imlt = 1,6
      READ(hlppmt(imlt+1,iTerm),*)ioemlt(imlt,iTerm)
    ENDDO
  ENDDO

  DO iTerm = SIZE(hlppmt,2)+1,nioe
    ioeTyp(iTerm) = 2
    DO imlt = 1,6
      READ(hlputt(imlt+1,iTerm-SIZE(hlppmt,2)),*)ioemlt(imlt,iTerm)
    ENDDO
  ENDDO

! Stop if an error in the input options found
! -------------------------------------------
  IF (irCode /= 0) CALL exitrc(2)

  RETURN
END SUBROUTINE subinp

END MODULE
