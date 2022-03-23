MODULE s_ORBCIN
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE orbcin(flNams,title,usesat,isamp,nslist,satlst, &
                  tstart,tend,ip,dcomax,mincoo,prtlev,isyst)
! -------------------------------------------------------------------------
! Purpose:   Read Input File for PROGRAM ORBCMP (new version of ORBCIN.f)
!
! Author:    H.Bock
!
! Created:   23-Oct-2001
!
! Changes:   21-Nov-2001 HU: Orbit comparison in inertial frame
!            17-Feb-2003 LM: Use m_maxdim
!            23-Apr-2003 AJ: Nullify local pointers
!            20-May-2003 RD: Use SR gttimwin instead of readsess
!            30-Oct-2003 HU: Allow empty title, staStr is CHR*255
!            25-Nov-2003 RD: Allow empty dcomax,
!                            adapt prtlev to new keywords
!            06-May-2011 HB: Add rdstdh to initialize model names,
!                            SR rdNutSub and setModKey if no STD file selected
!            12-Mar-2012 RD: Use SPLSTR as module now
!
! Copyright: Astronomical Institute
!            University of Bern
!            Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_maxdim, ONLY : maxsat
  USE d_model,  ONLY : setModKey, chrValLength, mod_orb_prcMod
  USE d_stdorb, ONLY: t_stdhead, init_stdHead
  USE s_gtfile2
  USE s_ckoptr
  USE s_alcerr
  USE s_splstr
  USE s_gttimwin
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  USE s_ckoptc
  USE s_priwin
  USE s_ckopti
  USE s_ckoptl
  USE s_gtflna
  USE s_rdstdh
  USE s_rdNutSub
  IMPLICIT NONE

! List of Parameters
! ------------------
  CHARACTER(LEN=fileNameLength),DIMENSION(:,:),POINTER :: flNams ! List of Input Files
  CHARACTER(LEN=80)             :: title    ! Title Line
  INTEGER(i4b)                  :: useSat   ! How to use Satellite List
                                            ! =0: Do not use Satellite List
                                            ! =1: Use Only Satellites in List
  INTEGER(i4b)                  :: iSamp    ! Sampling Rate
                                            ! =1: Use all
                                            ! =2: Use Only Every Second Satellite
  INTEGER(i4b)                  :: nsList   ! Number of Satellites in List
  INTEGER(i4b),DIMENSION(:),POINTER     :: satLst   ! Satellite Numbers in List
  REAL(r8b)                     :: tStart   ! Start Time (MJD)
  REAL(r8b)                     :: tEnd     ! End Time (MJD)
  INTEGER(i4b),DIMENSION(*)     :: IP       ! Helmert Transformation Parameter
  REAL(r8b)                     :: dcoMax   ! Max. Coordinate Difference
  INTEGER(i4b)                  :: minCoo   ! Min. Number of Common Sat.pos in both Files
  INTEGER(i4b)                  :: prtLev   ! Printing Level
                                            ! =0: Only Summary Printed
                                            ! =1: Summary plus Summary Info in
                                            !     File "OUTPUT" Printed
                                            ! =2: In Addition Residuals in File
                                            !     "OUTPUT" are Printed
  INTEGER(i4b)                  :: isyst    ! System for comparison
                                            ! =1: Earth-fixed
                                            ! =2: Inertial (J2000)
                                            ! =3: Orbit system (RSW)

! List of Functions
! -----------------
! Local Types
! -----------
  TYPE(t_stdhead)        :: stdHead   ! Structure of std header info

! Local Parameters
! ----------------

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength),DIMENSION(:),POINTER   :: keyValue
  CHARACTER(LEN=fileNameLength)      :: filNam
  CHARACTER(LEN=chrValLength)        :: chrVal
  CHARACTER(LEN=45)                  :: srname
  CHARACTER(LEN=16)                  :: nutNam, subNam
  CHARACTER(LEN=2)                   :: delLst
  CHARACTER(LEN=255)                 :: satStr
  CHARACTER(LEN=3),DIMENSION(maxSat) :: satNum

  REAL(r8b),DIMENSION(2)             :: timWin

  INTEGER(i4b) :: irCode
  INTEGER(i4b) :: irc
  INTEGER(i4b) :: iac
  INTEGER(i4b) :: iSat
  INTEGER(i4b) :: nFil

  NULLIFY(keyValue)

! Initialization
! --------------
  irCode = 0
  srName = ' '
  srName = 'ORBCIN'
  CALL init_stdHead(stdHead)

! Read list of input file names
! -----------------------------
  CALL gtfile2('PREINP',1,nFil,flNams)

! Title line
! ----------
  CALL readKeys('TITLE',keyValue,irc)

  CALL ckoptl(0,'TITLE',keyValue,srName,                                    &
              'Printing options: title line',irc,irCode,                    &
              empty=' ',maxVal=1,maxLength=80,result1=title)

! Read Options
! ------------
  CALL readKeys('PRTLEV', keyValue, irc)
  CALL ckoptc(1,'PRTLEV',keyValue,                                          &
   (/ 'SUMMARY        ', '+SUMM_IN_OUTPUT', '+RESIDUALS     ',              &
      'NONE           ', 'SUMMARY        ', 'RESIDUALS      ' /),           &
       srName,'Printing Options',irc,irCode,  &
       maxVal=1,valList=(/0,1,2,0,1,2/),result1=prtLev)

  CALL readKeys('ISAMP', keyValue, irc)
  CALL ckopti(1,'ISAMP',keyValue,srName,                                    &
              'Sampling Rate.',irc,irCode,                                  &
              maxVal=1,empty=1,error=0,result1=iSamp)

  CALL readKeys('MINCOO', keyValue, irc)
  CALL ckopti(1,'MINCOO',keyValue,srName,                                   &
              'Minimum Number of Coordinates.',irc,irCode,                  &
              maxVal=1,error=0,result1=minCoo)

  CALL readKeys('DCOMAX', keyValue, irc)
  CALL ckoptr(1,'DCOMAX',keyValue,srName,                                   &
              'Max. Coordinate Differencs Allowed.',irc,irCode,             &
              maxVal=1,empty=0d0,error=9999.D0,result1=dcoMax)

  CALL readKeys('USESAT', keyValue, irc)
  CALL ckoptc(1,'USESAT',keyValue,                                          &
              (/'EXCLUDED', 'INCLUDED' /),srName,                           &
              'How to use Satellite List',irc,irCode,                       &
              maxVal=1,valList=(/0,1/),result1=useSat)

  CALL readKeys('SYSTEM', keyValue, irc)
  CALL ckoptc(1,'SYSTEM',keyValue,                                          &
              (/'EARTH-FIXED','INERTIAL   ','RSW        '/),                &
              srName,'System definition',irc,irCode,maxVal=1,result1=isyst)

  CALL ckoptb(1,(/'OFFSETX'/),srName,'Helmert Trafo: Offset in x', irCode,  &
              result1=ip(1))
  CALL ckoptb(1,(/'OFFSETY'/),srName,'Helmert Trafo: Offset in y', irCode,  &
              result1=ip(2))
  CALL ckoptb(1,(/'OFFSETZ'/),srName,'Helmert Trafo: Offset in z', irCode,  &
              result1=ip(3))
  CALL ckoptb(1,(/'ROTX'/),srName,'Helmert Trafo: Rotation in x', irCode,   &
              result1=ip(4))
  CALL ckoptb(1,(/'ROTY'/),srName,'Helmert Trafo: Rotation in y', irCode,   &
              result1=ip(5))
  CALL ckoptb(1,(/'ROTZ'/),srName,'Helmert Trafo: Rotation in z', irCode,   &
              result1=ip(6))
  CALL ckoptb(1,(/'SCALE'/),srName,'Helmert Trafo: Scale', irCode,          &
              result1=ip(7))

! Satellite List
! --------------
  CALL readKeys('SATLST',keyValue,irc)
  CALL ckoptl(1,'SATLST',keyValue,srName,                                   &
              'Satellite Number List',irc,irCode,                           &
              maxVal=1,empty=' ',result1=satStr)
  delLst=' ,'
  CALL splstr(satStr,maxSat,delLst,nsList,satNum,irc)
  IF (irc==2) THEN
    irCode=irCode+1
    write(lfnErr,'(A,/,A,I4)')&
         '### SR ORBCIN: Too many satellite numbers found in ',&
         '               satellite number list: ',nsList
  ENDIF

  ALLOCATE (satLst(nsList),stat=iac)
  CALL alcerr(iac,'satLst',(/nsList/),'orbcin')
  DO iSat=1,nsList
    READ(satNum(iSat),*)satLst(iSat)
  ENDDO

! Time Window
! -----------
! Read infomation for the time window
! -----------------------------------
  CALL gttimwin('USEWIN',(/'RADIO_1','RADIO_2'/),        &
                (/'SESSION_YEAR','SESSION_STRG'/),       &
                (/'STADAT','STATIM','ENDDAT','ENDTIM'/), &
                timWin(:))
  tStart = timWin(1)
  tEnd   = timWin(2)

  CALL priwin(1,timWin)

! Get model information from standard orbit file header
! -----------------------------------------------------
  CALL gtflna(0,'STDORB',filNam,irc)
  IF (irc == 0) THEN
    CALL rdstdh(filNam,stdHead,irc)
  ELSE
    CALL rdNutSub(subNam,nutNam)
    chrVal = ' '
    chrVal(1:4)='BIAS'
    CALL setModKey(mod_orb_prcMod,chrVal,srName(1:8),0.D0)
  ENDIF
! Stop if an error in the input options found
! -------------------------------------------
  IF (irCode /= 0) CALL exitrc(2)

  DEALLOCATE(keyValue,stat=iac)

  RETURN
END SUBROUTINE orbcin

END MODULE
