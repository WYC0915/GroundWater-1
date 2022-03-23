MODULE s_HMINPI
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE hminpi(nstat, stname, stanum, staflg,  ityp, &
                  iopt,  ip,     iunit,  ioutl,  resmx, ircode)

! -------------------------------------------------------------------------
! Purpose:    This is a new version of the old subroutine HMINPI.f that
!             reads the input options of the program HELMR1
!
! Author:     L. Mervart
!
! Created:    21-Sep-2000
! Last mod.:  16-May-2003
!
! Changes:    30-Aug-2001 RD: use the station selection file as default
!             17-Sep-2001 RD: mark or exclude stations not in USESTA
!             25-Sep-2001 RD: new station selection in input file
!             02-Oct-2001 RD: modified handling of srName
!             16-Mar-2003 HU: Read ioutl, resmx
!             23-Apr-2003 AJ: Nullify local pointers
!             16-May-2003 CU: Initialize structure
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_stalst, ONLY: t_staList, init_stalist

  USE s_ckoptr
  USE s_alcerr
  USE s_ckoptu
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  USE s_ckoptc
  USE s_gtflna
  USE s_readstsg
  USE s_ckoptl
  IMPLICIT NONE

! List of Parameters
! ------------------
  INTEGER(i4b),                      INTENT(IN)  :: nstat
  CHARACTER(LEN=16), DIMENSION(*),   INTENT(IN)  :: stname
  INTEGER(i4b),      DIMENSION(*),   INTENT(IN)  :: stanum
  CHARACTER(LEN=1),  DIMENSION(2,*), INTENT(IN)  :: staflg
  INTEGER(i4b),      DIMENSION(*) :: ityp    ! 0 = used, 1 = marked, 2 = excl.
  INTEGER(i4b)                    :: iopt    ! 1 = (N,E,U), 2 = (X,Y,Z)
  INTEGER(i4b),      DIMENSION(7) :: ip      ! 3 shifts, 3 rotations, scale
  INTEGER(i4b)                    :: iunit   ! 1 = m, 2 = mm
  INTEGER(i4b)                    :: ioutl   ! 1 = outlier rejection
  REAL(r8b),         DIMENSION(3) :: resmx   ! maximum residuum, meter
  INTEGER(i4b)                    :: ircode  ! 0 = O.K.

! Local Parameters
! ----------------
  CHARACTER(LEN=5), DIMENSION(7), PARAMETER :: hlmKey = &
    (/ 'HLM_1', 'HLM_2', 'HLM_3', 'HLM_4', 'HLM_5', 'HLM_6', 'HLM_7' /)

  CHARACTER(LEN=12), DIMENSION(7), PARAMETER :: hlmDescr = &
    (/ 'shift in X  ', 'shift in Y  ', 'shift in Z  ', &
       'rot around X', 'rot around Y', 'rot around Z', 'scale       ' /)


! Local Variables
! ---------------
  TYPE(t_staList)                                        :: staList

  CHARACTER(LEN=keyValueLength), DIMENSION(:),   POINTER :: keyValue
  CHARACTER(LEN=keyValueLength), DIMENSION(:,:), POINTER :: hlpStr
  CHARACTER(LEN=fileNameLength)                          :: fixFil
  CHARACTER(LEN=shortLineLength)                         :: srName

  INTEGER(i4b), DIMENSION(:), POINTER                    :: whatToDo
  INTEGER(i4b)                                           :: iSta, jSta
  INTEGER(i4b)                                           :: ii, iSelect
  INTEGER(i4b)                                           :: iac, irc, ircSave

  NULLIFY(keyValue)
  NULLIFY(hlpStr)
  NULLIFY(whatToDo)

  irCode = 0
  srName = 'hminpi'

  CALL init_stalist(staList)

! Type of station selection
! -------------------------
  CALL ckoptb(1,(/ 'RADIO_1', 'RADIO_2' /), srName,                         &
              'Type of station selection', irCode, error=1, result1=iSelect)

! No interactive action was done
! ------------------------------
  IF (iSelect == 1) THEN

! Read the station selection file
! -------------------------------
    staList%nSta = 0
    CALL gtflna(0,'USESTA',fixfil,irc)
    IF (irc == 0) CALL readstsg(fixfil,0,staList)

    IF (staList%nSta > 0) THEN

! What to do with stations not selected
! -------------------------------------
      CALL readKeys('OTHSTA', keyValue, irc)

      CALL ckoptc(1, 'OTHSTA', keyValue, (/ 'MARK   ', 'EXCLUDE' /), srName,&
                  'Handling of stations not in file list',irc,irCode,       &
                  maxVal=1,error=2,result1=iTyp(1))

      IF (nStat > 1) iTyp(2:nstat) = iTyp(1)

! Handle station selection
! ------------------------
      DO iSta = 1, nstat
        DO jSta = 1, staList%nSta
          IF (stname(iSta)(1:16) == staList%stanam(jSta)) iTyp(ista) = 0
        ENDDO
      ENDDO

! Nothing found in station selction file
! --------------------------------------
    ELSE

      iTyp(1:nStat) = 0

    ENDIF

    DEALLOCATE(staList%staNam, stat=irc)

! Manual station selection
! ------------------------
  ELSE
    CALL readKeys('STASELECT',keyValue,irc)

    ircSave = irCode

! Extract uniline
! ---------------
    ALLOCATE(hlpStr(4,SIZE(keyValue)), stat=iac)
    CALL alcerr(iac,'hlpStr',(/4,SIZE(keyValue)/),'hminpi')

    CALL ckoptu(1,'STASELECT',keyValue,srName,                              &
                'Manual station selection',irc,irCode,4,                    &
                maxVal=SIZE(hlpStr,2),result2=hlpStr)

! Get the list of stations
! ------------------------
    ALLOCATE(staList%staNam(SIZE(keyValue)), stat=iac)
    CALL alcerr(iac,'staList%staNam',(/SIZE(keyValue)/),'hminpi')

    CALL ckoptl(1,'STASELECT',hlpStr(1,:),srName,                           &
                'Manual station selection',irCode-ircSave,irCode,           &
                colTit='Station name',                                      &
                maxVal=SIZE(staList%staNam),error='XXERR',init='XXERR',     &
                result2=staList%staNam)

! Get the "what to do" flags for the stations
! -------------------------------------------
    ALLOCATE(whatToDo(SIZE(keyValue)), stat=iac)
    CALL alcerr(iac,'whatToDo',(/SIZE(keyValue)/),'hminpi')

    CALL ckoptc(1,'STASELECT',hlpStr(4,:),(/ 'M', 'm', 'E', 'e', ' '/),     &
                srName, 'Manual station selection',irCode-ircSave,irCode,   &
                colTit='Station selection',maxVal=SIZE(whatToDo),           &
                valList=(/ 1,1,2,2,0 /), result2=whatToDo)

! Handle station selection
! ------------------------
    iTyp(1:nStat) = 2
    DO iSta = 1, nstat
      DO jSta = 1, SIZE(keyValue)
        IF (stname(iSta)(1:16) == staList%stanam(jSta)) &
          iTyp(ista) = whatToDo(jSta)
      ENDDO
    ENDDO

    DEALLOCATE(hlpStr, stat=irc)
    DEALLOCATE(whatToDo, stat=irc)
    DEALLOCATE(staList%staNam, stat=irc)

  ENDIF  ! Type of station selection


! Type and Unit of the Residuals
! ------------------------------
  CALL readkeys('RESIDTYPE', keyValue, irc)

  CALL ckoptc(1,'RESIDTYPE', keyValue, (/ 'NEU', 'XYZ'/), srName,         &
              'Type of residuals',irc,irCode,                             &
              maxVal=1, result1=iOpt)


  CALL readkeys('RESIDUNIT', keyValue, irc)

  CALL ckoptc(1,'RESIDUNIT', keyValue, (/ 'M ', 'MM'/), srName,           &
              'Unit of residuals',irc,irCode,                             &
              maxVal=1, result1=iUnit)


! Computed Parameters
! -------------------
  DO ii = 1, 7
    CALL ckoptb(1,(/ hlmKey(ii) /), srName,                               &
                'Parameter to be compute ('//trim(hlmdescr(ii))//')',     &
                irCode, error=0, result1=ip(ii))
  END DO

! Outlier Rejection
! -----------------
  CALL ckoptb(1,(/ 'REJECT' /), srName, 'Outlier rejection',              &
              irCode, error=0, result1=ioutl)

  CALL readkeys('NLIMIT', keyValue, irc)
  CALL ckoptr(1,'NLIMIT', keyValue, srName, 'Maximum residuum in N',      &
              irc, irCode, empty=9D9, ge=0D0, maxVal=1,result1=resmx(1))
  IF (resmx(1)==0D0) resmx(1)=9D9

  CALL readkeys('ELIMIT', keyValue, irc)
  CALL ckoptr(1,'ELIMIT', keyValue, srName, 'Maximum residuum in E',      &
              irc, irCode, empty=9D9, ge=0D0, maxVal=1,result1=resmx(2))
  IF (resmx(2)==0D0) resmx(2)=9D9

  CALL readkeys('ULIMIT', keyValue, irc)
  CALL ckoptr(1,'ULIMIT', keyValue, srName, 'Maximum residuum in U',      &
              irc, irCode, empty=9D9, ge=0D0, maxVal=1,result1=resmx(3))
  IF (resmx(3)==0D0) resmx(3)=9D9

! convert mm to meter
  resmx=resmx/1D3

! Stop if error in input file
! ---------------------------
  IF (irCode /= 0) CALL exitrc(2)

  DEALLOCATE(keyValue,stat=irc)

END SUBROUTINE hminpi

END MODULE
