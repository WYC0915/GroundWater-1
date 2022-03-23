MODULE s_SMINPF
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE sminpf(opt)

!---------------------------------------------------------------------
! Purpose  :  Read output file names for program GPSSIM ( new version
!             of SR SMINPF.f for new menu sytem)
!
! Author   :  H.Bock
!
! Created  :  30-May-2001
!
! Changes  :  05-Sep-2001 HU: Interface for alcerr added
!             20-Nov-2002 HB: Meteo-file simulation removed
!             17-Jan-2003 CU: Bug fixed (opt%csess(2) not used),
!                             allocate(stanam(nsta+1))
!             23-Apr-2003 RD: Nullify local pointers
!             08-Sep-2003 HU: antnam, recnam chr16 -> chr20
!             02-Oct-2003 RD: Use ABBREV for station abbreviation
!             14-Oct-2003 RD: Use CKOPT-SRs to read input options
!             03-Nov-2003 RD: Call SR gtabbv instead of SR getabb
!             24-Nov-2003 HU: Write station information
!             20-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
!---------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnprt, lfnerr, &
                      keyValueLength, fileNameLength
  USE d_abbrev, ONLY: t_abbrev, init_abbrev
  USE p_gpssim, ONLY: t_gpssim_opt
  USE s_gtabbv
  USE s_alcerr
  USE s_ckoptu
  USE s_readabb
  USE s_prfile
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  USE s_ckopti
  USE s_gtflna
  USE s_ckoptl
  IMPLICIT NONE

! List of parameters
! ------------------
! out:

  TYPE(t_gpssim_opt) :: opt

! List of Functions
! -----------------
! Local Types
! -----------
! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER :: srName = 'sminpf'

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength),DIMENSION(:),POINTER :: keyValue
  INTEGER(i4b) :: irc
  INTEGER(i4b) :: irCode

  TYPE(t_abbrev) :: abbrev
  INTEGER(i4b)   :: nAbb
  INTEGER(i4b), DIMENSION(:), POINTER :: abbIdx
  CHARACTER(LEN=fileNameLength) :: abbFil

  CHARACTER(LEN=keyValueLength), DIMENSION(:,:), ALLOCATABLE :: hlpStr
  CHARACTER(LEN=3),              DIMENSION(4) :: obsext
  CHARACTER(LEN=fileNameLength), DIMENSION(4) :: obspth
  CHARACTER(LEN=20)                           :: recTmp
  INTEGER(i4b) :: iac
  INTEGER(i4b) :: ista
  INTEGER(i4b) :: ii

! Init variables
! --------------
  NULLIFY(keyValue)

  NULLIFY(abbIdx)
  CALL init_abbrev(abbrev)

  irCode = 0

! Observation types to be simulated
! ---------------------------------
  CALL ckoptb(1,(/'CODSEL'/), srName,                &
              'Simulate code observations', irCode,  &
              result1=opt%codsel)

  CALL ckoptb(1,(/'PHASEL'/), srName,                &
              'Simulate phase observations', irCode, &
              result1=opt%phasel)

! Get the number of stations to be selected
! -----------------------------------------
  CALL readKeys('STAINF', keyValue, irc)
  IF (irc == 0) THEN
    opt%nsta = SIZE(keyValue)
  ELSE
    opt%nsta = 0
  ENDIF

! Read the content in a buffer
! ----------------------------
  ALLOCATE(hlpStr(6,opt%nsta),stat=iac)
  CALL alcerr(iac,'hlpStr',(/6,opt%nSta/),srName)

  IF (opt%nsta > 0) THEN
    CALL ckoptu(1,'STAINF',keyValue,srName,                         &
                'Station information setup', irc, irCode,           &
                numCol=SIZE(hlpStr,1),maxVal=SIZE(hlpStr,2),result2=hlpStr)

    ! Empty uniline: no station selected:
    IF (opt%nsta == 1 .AND. LEN_TRIM(hlpStr(1,1)) == 0) opt%nsta = 0
  ENDIF

! Extract station names from the buffer
! -------------------------------------
  ALLOCATE(opt%stanam(opt%nsta+1),stat=iac)
  CALL alcerr(iac, 'opt%stanam', (/opt%nsta+1/), srName)

  IF (opt%nsta > 0) THEN
    CALL ckoptl(1,'STAINF',hlpStr(1,:),srName,                      &
                'Station information setup', irc, irCode,           &
                colTit='Station name',                              &
                maxVal=opt%nsta,result2=opt%stanam(1:opt%nsta))
  ENDIF

! Extract receiver names from the buffer
! --------------------------------------
  ALLOCATE(opt%rectyp(opt%nsta),stat=iac)
  CALL alcerr(iac, 'opt%rectyp', (/opt%nsta/), srName)

  IF (opt%nsta > 0) THEN
    CALL ckoptl(1,'STAINF',hlpStr(2,:),srName,                      &
                'Station information setup', irc, irCode,           &
                colTit='Receiver type',empty=' ',                   &
                maxVal=opt%nsta,result2=opt%rectyp)

    ! Add "simula"
    DO iSta = 1,opt%nSta
      IF (opt%rectyp(iSta)=='SIMULA') CYCLE

      recTmp = 'SIMULA ' // opt%rectyp(ista)
      opt%rectyp(ista) = recTmp
    ENDDO

  ENDIF

! Extract antenna names from the buffer
! -------------------------------------
  ALLOCATE(opt%anttyp(opt%nsta),stat=iac)
  CALL alcerr(iac, 'opt%anttyp', (/opt%nsta/), srName)

  IF (opt%nsta > 0) THEN
    CALL ckoptl(1,'STAINF',hlpStr(3,:),srName,                      &
                'Station information setup', irc, irCode,           &
                colTit='Antenna type',                              &
                maxVal=opt%nsta,result2=opt%anttyp)
  ENDIF

! Extract operator names from the buffer
! -------------------------------------
  ALLOCATE(opt%oprnam(opt%nsta),stat=iac)
  CALL alcerr(iac, 'opt%oprnam', (/opt%nsta/), srName)

  IF (opt%nsta > 0) THEN
    CALL ckoptl(1,'STAINF',hlpStr(6,:),srName,                      &
                'Station information setup', irc, irCode,           &
                colTit='Operator name',empty=' ',                   &
                maxVal=opt%nsta,result2=opt%oprnam)
  ENDIF

! Extract receiver number from the buffer
! ---------------------------------------
  ALLOCATE(opt%recuni(opt%nsta),stat=iac)
  CALL alcerr(iac, 'opt%recuni', (/opt%nsta/), srName)

  IF (opt%nsta > 0) THEN
    CALL ckopti(1,'STAINF',hlpStr(4,:),srName,                      &
                'Station information setup', irc, irCode,           &
                colTit='Receiver number',empty=0,ge=0,le=999999,    &
                maxVal=opt%nsta,result2=opt%recuni)
  ENDIF

! Extract antenna number from the buffer
! --------------------------------------
  ALLOCATE(opt%antuni(opt%nsta),stat=iac)
  CALL alcerr(iac, 'opt%antuni', (/opt%nsta/), srName)

  IF (opt%nsta > 0) THEN
    CALL ckopti(1,'STAINF',hlpStr(5,:),srName,                      &
                'Station information setup', irc, irCode,           &
                colTit='Antenna number',empty=0,ge=0,le=999999,     &
                maxVal=opt%nsta,result2=opt%antuni)
  ENDIF

! Deallocate the buffer
! ---------------------
  DEALLOCATE(hlpStr,stat=iac)

! Read station abbreviation file
! ------------------------------
  CALL gtflna(1,'ABBREV',abbFil,irc)
  CALL readAbb(abbFil,abbrev)

  ALLOCATE(opt%fstnam(opt%nsta),stat=iac)
  CALL alcerr(iac, 'opt%fstnam', (/opt%nsta/), srName)

  DO ista=1,opt%nsta

    ! Get station abbreviation
    CALL gtabbv(0,opt%stanam(ista),1,abbFil,abbrev,nAbb,abbIdx)

    IF (nAbb == 0) THEN
      WRITE(lfnerr,'(/,A,2(/,16X,A,A),/)')                                  &
            ' *** SR SMINPF: Station not found in abbreviation table',      &
                            'Station name:       ', TRIM(opt%stanam(iSta)), &
                            'Abbreviation table: ', TRIM(abbFil)
      CALL exitrc(2)
    ELSE IF (nAbb > 1) THEN
      WRITE(lfnerr,'(/,A,2(/,16X,A,A),/)')                                  &
            ' ### SR SMINPF: More than one station abbreviation found',     &
                            'Station name:       ', TRIM(opt%stanam(iSta)), &
                            'Abbreviation table: ', TRIM(abbFil)
    ENDIF
    opt%fstnam(ista) = abbrev%abb(abbIdx(1))%staab4

  ENDDO

  DEALLOCATE(abbIdx,stat=irc)
  DEALLOCATE(abbrev%abb,stat=irc)

! Read extensions
! ---------------
  CALL readKeys('OBSFIL_EXT_COL_1', keyValue, irc)
  CALL ckoptl(1,'OBSFIL_EXT_COL_1', keyValue, srName,             &
              'Extension of code obs. header files',irc,irCode,   &
              maxVal=1,maxLength=3,result1=obsext(1))
  CALL readKeys('OBSFIL_EXT_COL_2', keyValue, irc)
  CALL ckoptl(1,'OBSFIL_EXT_COL_2', keyValue, srName,             &
              'Extension of code obs. data files',irc,irCode,     &
              maxVal=1,maxLength=3,result1=obsext(2))
  CALL readKeys('OBSFIL_EXT_COL_3', keyValue, irc)
  CALL ckoptl(1,'OBSFIL_EXT_COL_3', keyValue, srName,             &
              'Extension of phase obs. header files',irc,irCode,  &
              maxVal=1,maxLength=3,result1=obsext(3))
  CALL readKeys('OBSFIL_EXT_COL_4', keyValue, irc)
  CALL ckoptl(1,'OBSFIL_EXT_COL_4', keyValue, srName,             &
              'Extension of phase obs. data files',irc,irCode,    &
              maxVal=1,maxLength=3,result1=obsext(4))

! Read paths
! ----------
  CALL readKeys('OBSFIL_PTH_COL_1', keyValue, irc)
  CALL ckoptl(1,'OBSFIL_PTH_COL_1', keyValue, srName,       &
              'Path to code obs. header files',irc,irCode,  &
              maxVal=1,result1=obspth(1))
  CALL readKeys('OBSFIL_PTH_COL_2', keyValue, irc)
  CALL ckoptl(1,'OBSFIL_PTH_COL_2', keyValue, srName,       &
              'Path to code obs. data files',irc,irCode,    &
              maxVal=1,result1=obspth(2))
  CALL readKeys('OBSFIL_PTH_COL_3', keyValue, irc)
  CALL ckoptl(1,'OBSFIL_PTH_COL_3', keyValue, srName,       &
              'Path to phase obs. header files',irc,irCode, &
              maxVal=1,result1=obspth(3))
  CALL readKeys('OBSFIL_PTH_COL_4', keyValue, irc)
  CALL ckoptl(1,'OBSFIL_PTH_COL_4', keyValue, srName,       &
              'Path to phase obs. data files',irc,irCode,   &
              maxVal=1,result1=obspth(4))

! Stop program for an error in reading the options
! ------------------------------------------------
  IF (irCode /= 0) CALL exitrc(2)

! Allocate arrays for the file names
! ----------------------------------
  IF (opt%codsel == 1) THEN
    ALLOCATE(opt%filcod(2,opt%nsta),stat=iac)
    CALL alcerr(iac, 'opt%filcod', (/2,opt%nsta/), srName)
  ENDIF
  IF (opt%phasel == 1) THEN
    ALLOCATE(opt%filpha(2,opt%nsta),stat=iac)
    CALL alcerr(iac, 'opt%filpha', (/2,opt%nsta/), srName)
  ENDIF

! File names of observation files
! -------------------------------
  DO ista=1,opt%nsta
    IF (opt%codsel == 1) THEN
      DO ii=1,2
        opt%filcod(ii,ista) = trim(obspth(ii))//trim(opt%fstnam(ista))//&
             trim(opt%csess(1))//'.'//trim(obsext(ii))
      ENDDO
    ENDIF
    IF (opt%phasel == 1) THEN
      DO ii=3,4
        opt%filpha(ii-2,ista) = trim(obspth(ii))//trim(opt%fstnam(ista))//&
             trim(opt%csess(1))//'.'//trim(obsext(ii))
      ENDDO
    ENDIF
  ENDDO

! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=irc)

! Print station information
! -------------------------
  WRITE(lfnprt,"(' STATION INFORMATION', &
            &  /,' -------------------', &
            & //,1X,79('-'),             &
            &  /,' Sta  Station name     Receiver type        ', &
            &    'Antenna type         RecNr  AntNr',            &
            &  /,1X,79('-'))")
  DO ista=1,opt%nsta
    WRITE(lfnprt,"(1X,A4,1X,A16,2(1X,A20),2(I7))") &
         opt%fstnam(ista)(1:4),opt%stanam(ista),opt%rectyp(ista), &
         opt%anttyp(ista),opt%recuni(ista),opt%antuni(ista)
  ENDDO
  WRITE(lfnprt,"(1X,79('-'),//)")

! Print filenames
! ---------------
  IF (opt%codsel == 1) &
    CALL prfile('OBSFIL','CODE observation files to be simulated',2,80,  &
         opt%nSta,opt%filcod)

  IF (opt%phasel == 1) &
    CALL prfile('OBSFIL','PHASE observation files to be simulated',2,80, &
         opt%nSta,opt%filpha)

  RETURN
  END SUBROUTINE sminpf

END MODULE
