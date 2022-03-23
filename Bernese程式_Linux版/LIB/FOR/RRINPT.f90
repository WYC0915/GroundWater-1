MODULE s_RRINPT
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

 SUBROUTINE rrinpt(edt,icarr,numCls,hUnit,iWeight,wgtCls)

! -------------------------------------------------------------------------
! Purpose:    This is a new version of the old subroutine RRINPT.f that
!             reads the input options of the program RESRMS
!
! Author:     C. Urschl
!
! Created:    18-Oct-2000
!
! Changes:    21-Dec-2001 HU: Use d_const
!             02-Oct-2002 RD: Use ckopt subroutine
!                             Seperate MAXRMS per MEATYP
!             15-Jan-2003 RD: Add station observ. sigma factors
!             23-Apr-2003 RD: Nullify local pointers
!             23-Oct-2003 MM: Replace one uniline by lineedit (WGTCLASS0)
!             20-May-2009 DT: Use frequencies as available in file (FREQ)
!             29-Dec-2011 RD: Change keyword ISAMP -> SAMPL (unification)
!             27-Apr-2012 RD: Nullify all pointers, use m_bern with only
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b,lfnprt,  &
                      keyNameLength, keyValueLength, shortLineLength, &
                      fileNameLength
  USE d_const,  ONLY: date,time
  USE d_edit,   ONLY: t_edit

  USE s_ckoptr
  USE s_alcerr
  USE s_ckoptu
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  USE s_ckoptc
  USE s_ckopti
  USE s_gtflna
  USE s_ckoptl
  IMPLICIT NONE

! List of Parameters
! ------------------
! output
  TYPE(t_edit)                      :: edt        ! Entries for output file
  INTEGER(i4b)                      :: icarr      ! Frequency to be checked
                                                  ! 0 = all as available in RES file
  INTEGER(i4b)                      :: numCls     ! Size of the histogram
  REAL(r8b)                         :: hUnit      ! Bin width of the histgram
  INTEGER(i4b)                      :: iWeight    ! Method of observation sigma
                                                  ! computation:
                                                  !     0: no
                                                  !     1: median
                                                  !     2: histogram
  REAL(r8b), DIMENSION(:,:),POINTER :: wgtCls     ! Class definition for
                                                  ! observation sigmas


! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6),              PARAMETER               :: srName = 'rrinpt'

  CHARACTER(LEN=keyNameLength), DIMENSION(3,2),PARAMETER :: bigReskw = &
  reshape( source =                                                    &
         (/ 'DOBIGRES ', 'DOBIGRES2', 'DOBIGRES3' ,                    &
            'BIGRES   ', 'BIGRES2  ', 'BIGRES3  ' /),                  &
  shape = (/ 3,2 /) )

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:),  POINTER  :: keyValue
  CHARACTER(LEN=keyValueLength), DIMENSION(:,:),POINTER  :: hlpStr
  CHARACTER(LEN=shortLineLength)                         :: line
  CHARACTER(LEN=fileNameLength)                          :: sosFil

  INTEGER(i4b)                                           :: iMea
  INTEGER(i4b)                                           :: ircSav
  INTEGER(i4b)                                           :: irc, irCode

  LOGICAL                                                :: doit

! Init the error counter
! ----------------------
  irCode = 0

  NULLIFY(keyValue)
  NULLIFY(hlpStr)

! Read Title Line
! ---------------
  CALL readKeys('TITLE', keyValue, irc)
  CALL ckoptl(0,'TITLE',keyValue,srName, &
              'Title line',irc,irCode,   &
              empty=' ',maxVal=1,result1=edt%title)

  edt%title(66:75) = DATE
  edt%title(76:80) = TIME

! Read Frequency
! --------------
  CALL readKeys('FREQ', keyValue, irc)
  CALL ckoptc(1,'FREQ',keyValue,                                   &
              (/'AS_IS','L1   ','L2   ','L3   ','L4   ','L5   '/), &
              srName,'Frequency to check',irc,irCode,maxVal=1,     &
              result1=iCarr)

  iCarr = iCarr - 1

! Read Residual Size for Outlier Detection
! ----------------------------------------
  edt%resMax = 0d0

  DO iMea = 1,SIZE(bigReskw,1)

    CALL ckoptb(1,(/ bigResKw(iMea,1) /), srName,                   &
                'Select measurement type',irCode,                   &
                resultL=doit)
    IF (.NOT. doit) CYCLE

    CALL readKeys(bigResKw(iMea,2),keyValue,irc)
    CALL ckoptr(1,bigResKw(iMea,2),keyValue,srName,                 &
                'Big residual value',irc,irCode,                    &
                maxVal=1,empty=0d0,ge=0d0,result1=edt%resMax(iMea))

  ENDDO


! Read Sampling Interval of Residual Files
! ----------------------------------------
  CALL readKeys('SAMPL',keyValue, irc)
  CALL ckopti(1,'SAMPL',keyValue,srName,                           &
              'Sampling interval of residual file',irc,irCode,      &
              maxVal=1,ge=1,result1=edt%nSampl)

! Read Minimum Time Interval for small Data Pieces
! ------------------------------------------------
  CALL readKeys('IPIECE',keyValue, irc)
  CALL ckopti(1,'IPIECE',keyValue,srName,                           &
              'Time interval for small data pieces',irc,irCode,     &
              maxVal=1,empty=0,ge=0,result1=edt%minint)

! Read Minimum Number of Observations
! -----------------------------------
  CALL ckoptb(1,(/ 'DOMINAMB' /), srName,                           &
                'Select check of ambiguities',irCode,               &
                resultL=doit)

  IF (doit) THEN

    CALL readKeys('IMINAMB', keyValue, irc)
    CALL ckopti(1,'IMINAMB',keyValue,srName,                        &
                'Minimum number of obs. per ambiguity',irc,irCode,  &
                maxVal=1,empty=0,ge=0,result1=edt%minamb)

    IF (edt%minamb == 0) doit = .FALSE.

  ENDIF


! Read Sampling Rate for counting the Obs.
! ----------------------------------------
  IF (doit) THEN

    CALL readKeys('ISPLAMB', keyValue, irc)
    CALL ckopti(1,'ISPLAMB',keyValue,srName,                        &
                'Sampling for counting the observ.',irc,irCode,     &
                maxVal=1,empty=0,ge=0,result1=edt%iSampl)

  ELSE

    edt%minAmb = 0
    edt%iSampl = 0

  ENDIF


! Size of the histogram (-NUMCLS ... NUMCLS)
! ------------------------------------------
  CALL readKeys('NHISTO',keyValue,irc)
  CALL ckopti(1,'NHISTO',keyValue,srName,                           &
              'Size of the histogram',irc,irCode,                   &
              maxVal=1,empty=1,ge=1,result1=numCls)

! Bin width of the histogram
! --------------------------
  CALL readKeys('HISTWD',keyValue,irc)
  CALL ckoptr(1,'HISTWD',keyValue,srName,                           &
              'Bin width of the histogram',irc,irCode,              &
              maxVal=1,empty=1d0,gt=0d0,result1=hUnit)

! Method of measurement noise computation
! ---------------------------------------
  CALL gtflna(0,'STAWGTRS',sosFil,irc)

  iWeight = 0
  IF (LEN_TRIM(sosFil) > 0) THEN
    CALL readKeys('WGTTYPE',keyValue,irc)
    CALL ckoptc(1,'WGTTYPE',keyValue,(/'MEDIAN   ','HISTOGRAM'/),srName,   &
                'Method of measurement noise computation',irc,irCode,      &
                maxVal=1,result1=iWeight)
  ENDIF

! Weight class definition
! -----------------------
  IF (LEN_TRIM(sosFil) > 0) THEN

    CALL readKeys('WGTCLASS',keyValue,irc)

    ALLOCATE(hlpStr(2,SIZE(keyValue)),stat=irc)
    CALL alcerr(irc,'hlpStr',(/2,SIZE(keyValue)/),srName)

    ALLOCATE(wgtcls(2,SIZE(keyValue)+1),stat=irc)
    CALL alcerr(irc,'wgtcls',(/2,SIZE(keyValue)+1/),srName)
    wgtCls = 0d0

    ircSav = irCode
    CALL ckoptu(1,'WGTCLASS',keyValue,srName,                       &
                'Sigma factor definition',irc,irCode,2,             &
                maxVal=SIZE(hlpStr,2),result2=hlpStr)
    ircSav = irCode-ircSav

    CALL ckoptr(1,'WGTCLASS',hlpStr(1,:),srName,                    &
                'Sigma factor definition',ircSav,irCode,            &
                colTit='sigma factor',maxVal=SIZE(wgtCls,2)-1,      &
                gt=0d0,result2=wgtCls(1,2:))

    CALL ckoptr(1,'WGTCLASS',hlpStr(2,:),srName,                    &
                'Sigma factor definition',ircSav,irCode,            &
                colTit='noise limit',maxVal=SIZE(wgtCls,2)-1,       &
                gt=0d0,result2=wgtCls(2,2:))

    CALL readKeys('WGTCLASS0',keyValue,irc)
    CALL ckoptr(1,'WGTCLASS0',keyValue,srName,                        &
                'Sigma factor definition',irc,irCode,                 &
                maxVal=1,gt=0d0,result1=wgtCls(1,1))
    ircSav = irCode-ircSav

    DEALLOCATE(hlpStr,stat=irc)

  ENDIF


! Exit when an error was found
! ----------------------------
  IF (irCode /= 0) CALL exitrc(2)

! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=irc)

! Report input options in program output
! --------------------------------------
  WRITE(lfnprt,'(2(A,/))') &
        ' PROGRAM INPUT OPTIONS:',' ---------------------'

  ! Big residual limit for phase
  line = ' Outlier detection level (phase)               : disabled'
  IF (edt%resMax(1) > 0D0) &
    WRITE(line(49:80),'(F9.3,A)') edt%resMax(1),'  meter'
  WRITE(lfnprt,'(A)') TRIM(line)

  ! Big residual limit for code
  line = ' Outlier detection level (code)                : disabled'
  IF (edt%resMax(2) > 0D0) &
    WRITE(line(49:80),'(F9.3,A)') edt%resMax(2),'  meter'
  WRITE(lfnprt,'(A)') TRIM(line)

  ! Big residual limit for range
  line = ' Outlier detection level (range)               : disabled'
  IF (edt%resMax(3) > 0D0) &
    WRITE(line(49:80),'(F9.3,A)') edt%resMax(3),'  meter'
  WRITE(lfnprt,'(A,/)') TRIM(line)

  ! Sampling of the residuals
  line = ' Sampling rate of residuals                    : '
  WRITE(line(49:80),'(I9,A)') edt%nSampl   ,'  second'
  WRITE(lfnprt,'(A,/)') TRIM(line)

  ! Small data piece specification
  line = ' Minimum time interval for small pieces        : disabled'
  IF (edt%minInt > 0) &
    WRITE(line(49:80),'(I9,A)') edt%minInt   ,'  second'
  WRITE(lfnprt,'(A,/)') TRIM(line)

  ! Minimum number of observations per ambiguity
  line = ' Minimum number of observations per ambiguity  : disabled'
  IF (edt%minAmb > 0) &
    WRITE(line(49:80),'(I9,A)') edt%minAmb
  WRITE(lfnprt,'(A)') TRIM(line)

  ! Sampling for ambiguity counting
  line = ' Sampling rate for counting observations       : disabled'
  IF (edt%minAmb > 0 .AND. edt%iSampl > 0) &
    WRITE(line(49:80),'(I9,A)') edt%iSampl   ,'  second'
  WRITE(lfnprt,'(A,/)') TRIM(line)


  ! Size of the histogram
  line = ' Size of the histogram                         : '
  WRITE(line(49:80),'(I5,A,I5)') -numCls,' ...',numCls
  WRITE(lfnprt,'(A)') TRIM(line)

  ! Bin width for the histogram
  line = ' Bin width for the histogram                   : '
  WRITE(line(49:80),'(F9.1,A)') hUnit,'  millimeters'
  WRITE(lfnprt,'(A,/)') TRIM(line)


  ! Station observation sigma factors
  line = ' Station observation sigma factor computation  : disabled'
  IF (iWeight == 1) &
    WRITE(line(49:80),'(A)') ' from median of residuals'
  IF (iWeight == 2) &
    WRITE(line(49:80),'(A)') ' from histogram of residuals'
  WRITE(lfnprt,'(A,//)') TRIM(line)

  RETURN
END SUBROUTINE rrinpt

END MODULE
