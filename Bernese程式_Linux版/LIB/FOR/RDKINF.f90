MODULE s_RDKINF
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdkinf(filnam,kinSta)

! -------------------------------------------------------------------------
! Purpose:    Read the kinematic station coordinate file
!
! Author:     R. Dach
!
! Created:    24-Jun-2003
! Last mod.:  21-Sep-2010
!
! Changes:    07-Feb-2005 HB: Adopt for ifc-Compiler, Version 8.1
!             21-Sep-2010 RD: ST2TIM can be used as a module now
!
! SR called:  opnfil,opnerr,dimtst,st2tim,alcerr,exitrc
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_maxdim, ONLY: maxSta
  USE d_kinSta, ONLY: t_kinSta

  USE s_dimtst
  USE s_opnfil
  USE s_alcerr
  USE s_opnerr
  USE s_st2tim
  USE s_exitrc
  USE f_gpsmjd
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)              :: filNam            ! File name to read

! output:
  TYPE(t_kinSta)                :: kinSta            ! Kinematic stations

! Local Parameters
! ----------------
  CHARACTER(LEN=6),PARAMETER    :: srName = 'rdkinf'

! Local Variables
! ---------------
  CHARACTER(LEN=80)             :: line
  CHARACTER(LEN=staNameLength),  &
              DIMENSION(maxSta) :: stName
  CHARACTER(LEN=staNameLength)  :: newSta
  CHARACTER(LEN=timStrgLength)  :: timStr

  INTEGER(i4b)                  :: nStat
  INTEGER(i4b)                  :: iStat
  INTEGER(i4b)                  :: iSta
  INTEGER(i4b)                  :: nWeek
  INTEGER(i4b)                  :: iostat
  INTEGER(i4b)                  :: numEpo
  INTEGER(i4b)                  :: irc

  REAL(r8b)                     :: second

! Deallocate an old data rocord
! -----------------------------
  IF (ASSOCIATED(kinSta%sta)) THEN
    DO iSta = 1,kinSta%nSta
      IF (kinSta%sta(iSta)%nEpo == 0) CYCLE
      IF (.NOT. ASSOCIATED(kinSta%sta(iSta)%kin)) CYCLE
      DEALLOCATE(kinSta%sta(iSta)%kin,stat=irc)
    ENDDO
    DEALLOCATE(kinSta%sta,stat=irc)
  ENDIF

! Open kin. file
! --------------
  CALL opnfil(lfnloc,filnam,'OLD','FORMATTED','READONLY',' ',iostat)
  CALL opnerr(lfnerr,lfnloc,iostat,filnam,srName)

! Generate the list of stations
! -----------------------------
  nStat = 0
  READ(lfnloc,'(/////)')

  getStaLoop: DO
    READ(lfnloc,'(1X,A)',iostat=iostat) newSta
    IF (LEN_TRIM(newSta) == 0 .OR. iostat/= 0) EXIT

    DO iStat = 1,nStat
      IF (stName(iStat) == newSta) CYCLE getStaLoop
    ENDDO

    nStat = nStat + 1
    CALL dimtst(1,2,2,srName,'MAXSTA','kinematic stations',' ',nStat,maxSta,irc)
    stName(nStat) = newSta
  ENDDO getStaLoop

  ALLOCATE(kinSta%sta(nStat),stat=irc)
  CALL alcerr(irc,'kinSta%sta',(/nStat/),srName)

  kinSta%nSta = nStat
  kinSta%sta(1:nStat)%staNam = stName(1:nStat)

! Count the number of epochs per station
! --------------------------------------
  REWIND(lfnloc)

  kinSta%sta(:)%nEpo = 0

  READ (lfnloc,'(A80,//,22X,A16,9X,A19,///)',IOSTAT=iostat) &
        kinSta%title,kinSta%datum%name,timstr

  DO
    READ(lfnloc,'(1X,A)',iostat=iostat) newSta
    IF (LEN_TRIM(newSta) == 0 .OR. iostat/= 0) EXIT

    DO iSta = 1,kinSta%nSta
      IF (kinSta%sta(iSta)%staNam == newSta) THEN
        kinSta%sta(iSta)%nEpo = kinSta%sta(iSta)%nEpo + 1
        EXIT
      ENDIF
    ENDDO

  ENDDO

  DO iSta = 1,kinSta%nSta
!!!!!!!!! Problems with ifc81 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#ifdef CMP_IFC8
    numEpo = kinSta%sta(iSta)%nEpo
    ALLOCATE(kinSta%sta(iSta)%kin(numEpo),stat=irc)
    CALL alcerr(irc,'kinSta%sta(iSta)%kin',(/numEpo/),srName)
#else
    ALLOCATE(kinSta%sta(iSta)%kin(kinSta%sta(iSta)%nEpo),stat=irc)
    CALL alcerr(irc,'kinSta%sta(iSta)%kin',(/kinSta%sta(iSta)%nEpo/),srName)
#endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ENDDO


! Read the data records per station
! ---------------------------------
  REWIND(lfnloc)

  kinSta%sta(:)%nEpo = 0

  READ (lfnloc,'(A80,//,22X,A16,9X,A19,///)',IOSTAT=iostat) &
        kinSta%title,kinSta%datum%name,timstr

  CALL st2tim(1,1,timstr,kinSta%refEpo)

  DO
    READ(lfnloc,'(A)',iostat=iostat) line
    IF (LEN_TRIM(line) == 0 .OR. iostat/= 0) EXIT

    newSta = line(2:17)

    DO iSta = 1,kinSta%nSta
      IF (kinSta%sta(iSta)%staNam == newSta) THEN
        kinSta%sta(iSta)%nEpo = kinSta%sta(iSta)%nEpo + 1
        READ (line,'(1X,16X,1X,I4,1X,F8.0,1X,3F15.4,1X,A1)',IOSTAT=iostat) &
              nweek,second,                                                &
              kinSta%sta(iSta)%kin(kinSta%sta(iSta)%nEpo)%xyz(1:3),        &
              kinSta%sta(iSta)%kin(kinSta%sta(iSta)%nEpo)%kinFlg

        IF (iostat /= 0) THEN
          WRITE(lfnerr,'(/,A,/,16X,A,A,/)')                             &
           ' *** SR RDKINF: Error reading kinematic coordinates',       &
                           'File: ',TRIM(filnam)

          CALL exitrc(2)
        ENDIF

        kinSta%sta(iSta)%kin(kinSta%sta(iSta)%nEpo)%xyzEpo=GPSMJD(second,nweek)
        EXIT ! Station loop
      ENDIF
    ENDDO

  ENDDO

  CLOSE(lfnloc)


  RETURN
END SUBROUTINE rdkinf

END MODULE
