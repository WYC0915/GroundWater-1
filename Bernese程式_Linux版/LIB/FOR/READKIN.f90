MODULE s_READKIN
CONTAINS

! ------------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! ------------------------------------------------------------------------------

  SUBROUTINE ReadKin(filnam,staNam,epoch,iflag,DALL,XYZ,irc)

! ------------------------------------------------------------------------------
! NAME       : ReadKin
!
! PURPOSE    : read KIN file
!
! REMARKS    : max error to indentify one epoch set to 5.0D-6
!
! AUTHOR     : D. Svehla
!
! VERSION    : 5.0
!
! CREATED    : 10-Dec-2001
!
! CHANGES    : 03-Jul-2002 HU: Error handling improved
!            : 20-Jul-2002 DS: If DALL==1 and first RETURN
!              04-Oct-2002 RD: Corrected format statement
!              13-Nov-2002 RD: Check station name
!                              Use linCount for counting the lines
!                              Use alcerr for allocation
!              19-Jun-2003 RD: Improve format of error messages
!                              Prevent kEpo > nEpo
!              24-Jun-2003 RD: More than one station possible
!                              Use structure and RDKINF for reading the file
!              25-Jun-2003 RD: Update the buffer if file name has been changed
!              28-Jul-2005 RD: Open string length for stanam
!              20-Sep-2012 RD: Correctly deallocate arrays
!              20-Sep-2012 RD: Use M_BERN with ONLY
!              20-Sep-2012 RD: Remove unused variables
!
! COPYRIGHT  : ASTRONOMICAL INSTITUTE
!               UNIVERSITY OF BERN
!                   SWITZERLAND
! ------------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnerr, fileNameLength
  USE d_kinSta, ONLY: t_kinSta, init_kinSta

  USE s_alcerr
  USE s_exitrc
  USE s_rdkinf
  IMPLICIT NONE

!
! Parameters
! ==========
!
! IN :
! ----
  CHARACTER(LEN=fileNameLength)   :: filnam  ! File with kin. coordinates
  CHARACTER(LEN=*)                :: staNam  ! Station name
!  CHARACTER(LEN=staNameLength)    :: staNam  ! Station name
  REAL(r8b)                       :: epoch   ! Epoch in MJD
  INTEGER(i4b)                    :: iflag   ! Flag for getting coordinates
                                             !  =0 : get epoch with any flag
                                             !  =1 : get only epoch with "K"
  INTEGER(i4b)                    :: DALL    ! Deallocate arrays
                                             !  =0 : No
                                             !  =1 : YES
!
! OUT:
! ----
  REAL(r8b),DIMENSION(3)          :: XYZ     ! kin coordinates (X,Y,Z)
  INTEGER(i4b)                    :: irc     !  =0 OK
                                             !  =1 no kin data for req. epoch
                                             !  =2 station staNam not in file

! Local parameters
! ----------------
  CHARACTER(LEN=7),PARAMETER                     :: srName = 'readkin'

! Local variables
! ---------------
  TYPE(t_kinSta),                           SAVE :: kinSta

  CHARACTER(LEN=255),                       SAVE :: filSav = ' *** unknown *** '

  INTEGER(i4b)                                   :: iepo
  INTEGER(i4b)                                   :: nstart
  INTEGER(i4b)                                   :: iSta, jSta
  INTEGER(i4b), DIMENSION(:),ALLOCATABLE,   SAVE :: kEpo
  INTEGER(i4b)                                   :: iac

  REAL(r8b)                                      :: ErrMax

  LOGICAL,                                  SAVE :: first=.TRUE.

!
! INITIALIZE
! ----------
  ErrMax=5.0D-6
!
! Deallocate arrays if possible
! -----------------------------
  IF (DALL==1 .OR. filsav /= filnam) THEN
    filSav = ' *** unknown *** '
    IF (first) THEN
      CALL init_kinSta(kinSta)
      IF (DALL == 1) RETURN
    ELSE
      IF (ASSOCIATED(kinSta%sta)) THEN
        DO iSta = 1,kinSta%nSta
          IF (kinSta%sta(iSta)%nEpo == 0) CYCLE
          IF (.NOT. ASSOCIATED(kinSta%sta(iSta)%kin)) CYCLE
          DEALLOCATE(kinSta%sta(iSta)%kin,stat=iac)
        ENDDO
        DEALLOCATE(kinSta%sta,stat=iac)
      ENDIF
      IF (ALLOCATED(kEpo)) DEALLOCATE (kEpo,stat=iac)
      first=.TRUE.
      IF (DALL == 1) RETURN
    ENDIF
  END IF
!
! FIRST CALL OF SUBROUTINE
! ========================
  irc=0
  IF (first) THEN
    first  = .FALSE.
    filsav = filnam

! Read the kin. file
! ------------------
    CALL rdkinf(filnam,kinSta)

! Allocate index
! --------------
    ALLOCATE(kEpo(kinSta%nSta),stat=iac)
    CALL alcerr(iac,'kEpo',(/kinSta%nSta/),srName)

    kepo=1

  END IF

! Check Station name
! ------------------
  iSta = 0
  DO jSta = 1,kinSta%nSta
    IF (kinSta%sta(jSta)%staNam == staNam) THEN
      iSta = jSta
      EXIT
    ENDIF
  ENDDO

  IF (iSta == 0) THEN
    irc    = 2
    XYZ    = 0d0
    RETURN
  ENDIF

! LOOK FOR REQUESTED EPOCH
! ------------------------
! By keeping the index in mind, so that you don't have to loop the whole list
! for each call

  IF (epoch >= kinSta%sta(iSta)%kin(kEpo(iSta))%xyzEpo-ErrMax .AND. &
      epoch <= kinSta%sta(iSta)%kin(kEpo(iSta))%xyzEpo+ErrMax) THEN
    IF (iflag == 0 .OR. &
       (iflag == 1 .AND. kinSta%sta(iSta)%kin(kEpo(iSta))%kinFlg == 'K')) THEN
      XYZ(1:3) = kinSta%sta(iSta)%kin(kEpo(iSta))%xyz
      IF (kEpo(iSta) < kinSta%sta(iSta)%nEpo) kepo(iSta) = kepo(iSta)+1
      RETURN
    END IF
  ELSE IF (epoch > kinSta%sta(iSta)%kin(kEpo(iSta))%xyzEpo+ErrMax) THEN
    IF (kepo(iSta) < kinSta%sta(iSta)%nepo) THEN
      nstart=kepo(iSta)+1
    ELSE
      nstart=kinSta%sta(iSta)%nepo
    END IF
    DO iepo=nstart,kinSta%sta(iSta)%nepo
      IF (epoch  < kinSta%sta(iSta)%kin(iEpo)%xyzEpo-ErrMax) GOTO 900
      IF (epoch >= kinSta%sta(iSta)%kin(iEpo)%xyzEpo-ErrMax .AND. &
          epoch <= kinSta%sta(iSta)%kin(iEpo)%xyzEpo+ErrMax) THEN
        IF (iflag==0 .OR. &
           (iflag==1 .AND. kinSta%sta(iSta)%kin(iEpo)%kinFlg == 'K')) THEN
          XYZ(1:3) = kinSta%sta(iSta)%kin(iEpo)%xyz
          kepo(iSta) = iepo+1
          IF (kEpo(iSta) > kinSta%sta(iSta)%nEpo) &
            kEpo(iSta) = kinSta%sta(iSta)%nEpo
          RETURN
        END IF
      END IF
    END DO
  ELSE IF (epoch < kinSta%sta(iSta)%kin(kEpo(iSta))%xyzEpo-ErrMax) THEN
    DO iepo=kepo(iSta)-1,1,-1
      IF (epoch  > kinSta%sta(iSta)%kin(iEpo)%xyzEpo+ErrMax) GOTO 900
      IF (epoch >= kinSta%sta(iSta)%kin(iEpo)%xyzEpo-ErrMax .AND. &
          epoch <= kinSta%sta(iSta)%kin(iEpo)%xyzEpo+ErrMax) THEN
        IF (iflag==0 .OR. &
           (iflag==1 .AND. kinSta%sta(iSta)%kin(iEpo)%kinFlg == 'K')) THEN
          XYZ(1:3) = kinSta%sta(iSta)%kin(iEpo)%xyz
          kepo(iSta)=iepo+1
          IF (kEpo(iSta) > kinSta%sta(iSta)%nEpo) &
            kEpo(iSta) = kinSta%sta(iSta)%nEpo
          RETURN
        END IF
      END IF
    END DO
  END IF

900  WRITE(lfnerr,'(/,A,/,17X,A,A,/,17X,A,F15.7,/,17X,A,A,/)')      &
           ' ### SR READKIN: No kin. coordinates found for the ' // &
           'requested epoch and file','Station:  ',TRIM(staNam),    &
          'Epoch:    ',epoch,'File:     ',TRIM(filnam)

  irc=1

  RETURN

  WRITE(lfnerr,'(A,/,17X,A,A,/)')                               &
         ' *** SR READKIN: Error reading kinematic coordinates', &
        'File: ',TRIM(filnam)

  CALL exitrc(2)

  END SUBROUTINE ReadKin

END MODULE
