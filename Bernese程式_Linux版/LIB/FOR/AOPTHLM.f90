MODULE s_AOPTHLM
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE aopthlm(opt, parHlm)

! -------------------------------------------------------------------------
! Purpose:    Reads input options for ADDNEQ2:
!             options and constraining for Helmert parameters
!
! Author:     D. Thaller
!
! Created:    29-Nov-2010
! Last mod.:  29-Nov-2010
!
! Changes:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE p_addneq, ONLY: t_opt, t_parHlm,  hlmFil

  USE s_alcerr
  USE s_covcoi
  USE s_exitrc
  USE s_readkeys
  IMPLICIT NONE

! List of parameters
! ------------------
! input:

! input/output:
  TYPE(t_opt)                            :: opt    ! Options for ADDNEQ2
  TYPE(t_parHlm)                         :: parHlm ! Helmert parameters

! output:


! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=7),PARAMETER               :: srName = 'aopthlm'
  CHARACTER(LEN=12),DIMENSION(3),PARAMETER :: strStack = &
  (/'No stacking ', 'Within group', 'Stack all   '/)

! Local variables
! ---------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER  :: keyValue
  CHARACTER(LEN=8), DIMENSION(7)                        :: sheles

  INTEGER(i4b)                                          :: jfil
  INTEGER(i4b)                                          :: nhelm
  INTEGER(i4b)                                          :: ihelm
  INTEGER(i4b)                                          :: iac, irc


! Initialization
! --------------
  NULLIFY(keyValue)

  NULLIFY(hlmFil%ihelm)
  NULLIFY(hlmFil%rhelm)
  NULLIFY(hlmFil%indgrp)
  NULLIFY(hlmFil%grpnam)

  hlmFil%nNeq = SIZE(opt%neqFileName)


! Read WGT file
! -------------
  IF (opt%covcomi == ' ') RETURN

  ALLOCATE(hlmFil%indgrp(hlmFil%nNeq),stat=iac)
  CALL alcerr(iac, 'hlmFil%indgrp', (/hlmFil%nNeq/), srname)
  hlmFil%indgrp(:)  = 0

  ALLOCATE(hlmFil%grpnam(hlmFil%nNeq),stat=iac)
  CALL alcerr(iac, 'hlmFil%grpnam', (/hlmFil%nNeq/), srName)
  hlmFil%grpnam(:)  = '   '

  ALLOCATE(hlmFil%ihelm(7,hlmFil%nNeq),stat=iac)
  CALL alcerr(iac, 'hlmFil%ihelm', (/7,hlmFil%nNeq/), srName)
  hlmFil%ihelm(:,:) = 0

  ALLOCATE(hlmFil%rhelm(7,hlmFil%nNeq),stat=iac)
  CALL alcerr(iac, 'hlmFil%rhelm', (/7,hlmFil%nNeq/), srName)
  hlmFil%rhelm(:,:) = 0.d0

  ALLOCATE(hlmFil%fact(hlmFil%nNeq),stat=iac)
  CALL alcerr(iac, 'hlmFil%fact', (/hlmFil%nNeq/), srName)
  hlmFil%fact(:) = 0.d0

  ALLOCATE(hlmFil%filNam(hlmFil%nNeq),stat=iac)
  CALL alcerr(iac, 'hlmFil%filNam', (/hlmFil%nNeq/), srName)
  hlmFil%filNam(:) = ''


  CALL covcoi(opt%covcomi, hlmFil%nNeq, opt%neqFileName, hlmFil%fact, hlmFil%indgrp, &
              hlmFil%grpnam, nhelm, hlmFil%ihelm, hlmFil%rhelm)


! Write scaling factors into protocol file
! ----------------------------------------
  WRITE(lfnprt,'(A,/,A,3(/,2A))')                                                &
      ' Rescaling of normal equations and Helmert parameters:',                  &
      ' ----------------------------------------------------',                   &
      '                                                                        ',&
      '             Helmert values',                                             &
      ' File  Name                              Rescaling value        dx[m]   ',&
      ' dy[m]    dz[m]   rx[mas]  ry[mas]  rz[mas]   s[ppm]',                    &
      ' -----------------------------------------------------------------------',&
      '----------------------------------------------------'

  DO jfil = 1, hlmFil%nNeq

    hlmFil%filNam(jfil) = opt%neqFileName(jfil)

    DO ihelm = 1, 7
      sheles(ihelm) = '   --   '
      IF (hlmFil%ihelm(ihelm,jfil) == 1)  &
         WRITE(sheles(ihelm),'(F8.4)') hlmFil%rhelm(ihelm,jfil)

      IF (hlmFil%ihelm(ihelm,jfil) == 2) sheles(ihelm) = '  est   '
    ENDDO
    WRITE(lfnprt,'(1X,I4,2X,A32,2X,ES19.13,2X,7(A8,1X))') &
          jfil,hlmFil%filNam(jfil),hlmFil%fact(jfil),sheles(1:7)

  ENDDO


! Stacking options
! ----------------
  opt%helmert(:)%stack = 0

  CALL readkeys('CMB_TRA', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == 'NO_STACKING') THEN
    opt%helmert(1:3)%stack = 0
  ELSE IF (irc == 0 .AND. keyValue(1) == 'IDENT_GROUP') THEN
    opt%helmert(1:3)%stack = 1
  ELSE IF (irc == 0 .AND. keyValue(1) == 'STACK_ALL') THEN
    opt%helmert(1:3)%stack = 2
  END IF

  CALL readkeys('CMB_ROT', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == 'NO_STACKING') THEN
    opt%helmert(4:6)%stack = 0
  ELSE IF (irc == 0 .AND. keyValue(1) == 'IDENT_GROUP') THEN
    opt%helmert(4:6)%stack = 1
  ELSE IF (irc == 0 .AND. keyValue(1) == 'STACK_ALL') THEN
    opt%helmert(4:6)%stack = 2
  END IF

  CALL readkeys('CMB_SC', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == 'NO_STACKING') THEN
    opt%helmert(7)%stack = 0
  ELSE IF (irc == 0 .AND. keyValue(1) == 'IDENT_GROUP') THEN
    opt%helmert(7)%stack = 1
  ELSE IF (irc == 0 .AND. keyValue(1) == 'STACK_ALL') THEN
    opt%helmert(7)%stack = 2
  END IF

  IF ( nhelm > 0 ) THEN
    WRITE(lfnprt,'(/,3(A,A,/))')  &
        ' Stacking of translation parameters: ', strStack(opt%helmert(1)%stack+1), &
        ' Stacking of rotation parameters:    ', strStack(opt%helmert(4)%stack+1), &
        ' Stacking of scale parameters:       ', strStack(opt%helmert(7)%stack+1)
  END IF


! Deallocate the special request definition
! -----------------------------------------
  DEALLOCATE(keyValue,stat=iac)

  RETURN

END SUBROUTINE aopthlm

END MODULE
