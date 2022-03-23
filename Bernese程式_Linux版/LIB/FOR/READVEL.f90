MODULE s_READVEL
CONTAINS

! ------------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! ------------------------------------------------------------------------------

  SUBROUTINE ReadVel(filnam,staNam,epoch,iflag,DALL,VXYZ,irc)

! ------------------------------------------------------------------------------
! NAME       : ReadVel
!
! PURPOSE    : read VEL file
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
!              28-Jul-2005 RD: Open string length for stanam
!
! COPYRIGHT  : ASTRONOMICAL INSTITUTE
!               UNIVERSITY OF BERN
!                   SWITZERLAND
! ------------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnerr, lfnloc, &
                      fileNameLength, staNameLength, lineLength

  USE s_alcerr
  USE s_opnfil
  USE s_exitrc
  USE f_gpsmjd
  USE s_opnerr
  IMPLICIT NONE

!
! Parameters
! ==========
!
! IN :
! ----
  CHARACTER(LEN=fileNameLength)   :: filnam  ! File with kin. coordinates
!  CHARACTER(LEN=staNameLength)    :: staNam  ! Station name
  CHARACTER(LEN=*)                :: staNam  ! Station name
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
  REAL(r8b),DIMENSION(3)          :: VXYZ    ! kin velocities V (X,Y,Z)
  INTEGER(i4b)                    :: irc     !  =0 OK
                                             !  =1 no kin veloc. for req. epoch
!
! Local variables
! ---------------
  CHARACTER(LEN=1),DIMENSION(:),ALLOCATABLE,SAVE :: kinFlag
  CHARACTER(LEN=lineLength)                      :: line
  CHARACTER(LEN=staNameLength)                   :: statmp
  CHARACTER(LEN=80)                              :: title
  CHARACTER(LEN=19)                              :: timstr
  CHARACTER(LEN=16)                              :: datum

  INTEGER(i4b)                    :: iostat,nweek,iepo,i,nstart
  INTEGER(i4b)                    :: iac
  INTEGER(i4b),SAVE               :: kepo
  INTEGER(i4b),SAVE               :: nepo

  REAL(r8b)                                      :: second
  REAL(r8b)                                      :: ErrMax
  REAL(r8b),DIMENSION(:),  ALLOCATABLE,SAVE      :: kinT
  REAL(r8b),DIMENSION(:,:),ALLOCATABLE,SAVE      :: kinVXYZ

  LOGICAL,SAVE  :: first=.TRUE.
!
! INITIALIZE
! ----------
  ErrMax=5.0D-6

!
! Deallocate arrays if possible
! -----------------------------
  IF (DALL==1 .AND. (.NOT. first)) THEN
    IF (ALLOCATED(kinFlag)) DEALLOCATE (kinFlag)
    IF (ALLOCATED(kinT))    DEALLOCATE (kinT)
    IF (ALLOCATED(kinVXYZ)) DEALLOCATE (kinVXYZ)
    first=.TRUE.
    RETURN
  ELSE IF (DALL==1 .AND. first) THEN
    RETURN
  END IF
!
! FIRST CALL OF SUBROUTINE
! ========================
  irc=0
  IF (first) THEN
    first=.FALSE.
    kepo=1
!
! OPEN KIN FILE
! =============
    CALL opnfil(lfnloc,filnam,'OLD','FORMATTED','READONLY',' ',iostat)
    CALL opnerr(lfnerr,lfnloc,iostat,filnam,'ReadVel')
!
! Read number of records
! ----------------------
    READ (lfnloc,'(A80,//,22X,A16,9X,A19,///)',IOSTAT=iostat) title, &
                                                              datum,timstr
    IF (iostat /= 0) GOTO 910
    nepo=0
    DO
      READ (lfnloc,'(A)',IOSTAT=iostat) line
      IF (iostat < 0) EXIT
      IF (iostat > 0) GOTO 910
      IF (line(2:staNameLength+1)==staNam(1:staNameLength)) nepo=nepo+1
    END DO
    REWIND (lfnloc)
!
! ALLOCATE MEMORY
! ---------------
    ALLOCATE(kinVXYZ(3,nepo),stat=iac)
    CALL alcerr(iac,'kinVXYZ',(/3,nepo/),'ReadVel')
    ALLOCATE(kinT(nepo),stat=iac)
    CALL alcerr(iac,'kinT',(/nepo/),'ReadVel')
    ALLOCATE(kinFlag(nepo),stat=iac)
    CALL alcerr(iac,'kinFlag',(/nepo/),'ReadVel')
!
! READ VEL FILE
! -------------
    READ (lfnloc,'(A80,//,22X,A16,9X,A19,///)',IOSTAT=iostat) title, &
                                                              datum,timstr
    IF (iostat /= 0) GOTO 910
!
    iepo=0
    DO
      READ (lfnloc,'(A)',IOSTAT=iostat) line
      IF (iostat < 0) EXIT
      IF (iostat > 0) GOTO 910
      IF (line(2:staNameLength+1)==staNam) THEN
        iepo=iepo+1
        READ (line,'(1X,A16,1X,I4,1X,F8.0,1X,3F15.4,1X,A1)',IOSTAT=iostat) &
              statmp,nweek,second,(kinVXYZ(i,iepo),i=1,3),kinFlag(iepo)
        IF (iostat /= 0) GOTO 910
        kinT(iepo)=GPSMJD(second,nweek)
        kinVXYZ(1:3,iepo)=kinVXYZ(1:3,iepo)/10.0D0
      END IF
    END DO
    CLOSE (lfnloc)
  END IF
!
! LOOK FOR REQUESTED EPOCH
! ------------------------
! By keeping the index in mind, so that you don't have to loop the whole list
! for each call

  IF (epoch.GE.kinT(kepo)-ErrMax .AND. epoch.LE.kinT(kepo)+ErrMax)THEN
    IF (iflag==0 .OR. (iflag==1 .AND. kinFlag(kepo)=='K')) THEN
      VXYZ(1:3)=kinVXYZ(1:3,kepo)
      kepo=kepo+1
      RETURN
    END IF
  ELSE IF (epoch.GT.kinT(kepo)+ErrMax) THEN
    IF (kepo.LT.nepo) THEN
      nstart=kepo+1
    ELSE
      nstart=nepo
    END IF
    DO iepo=nstart,nepo
      IF (epoch.LT.kinT(iepo)-ErrMax) GOTO 900
      IF (epoch.GE.kinT(iepo)-ErrMax .AND. epoch.LE.kinT(iepo)+ErrMax) THEN
        IF (iflag==0 .OR. (iflag==1 .AND. kinFlag(iepo)=='K')) THEN
          VXYZ(1:3)=kinVXYZ(1:3,iepo)
          kepo=iepo+1
          RETURN
        END IF
      END IF
    END DO
  ELSE IF (epoch.LT.kinT(kepo)-ErrMax) THEN
    DO iepo=kepo-1,1,-1
      IF (epoch.GT.kinT(iepo)+ErrMax) GOTO 900
      IF (epoch.GE.kinT(iepo)-ErrMax .AND. epoch.LE.kinT(iepo)+ErrMax) THEN
        IF (iflag==0 .OR. (iflag==1 .AND. kinFlag(iepo)=='K')) THEN
          VXYZ(1:3)=kinVXYZ(1:3,iepo)
          kepo=iepo+1
          RETURN
        END IF
      END IF
    END DO
  END IF

900  WRITE(lfnerr,'(A,/,18X,A,F15.7,/,18X,A,A,/)')                 &
           ' *** SR ReadVel: No kin. velocities found for the ' // &
           'requested epoch and file',                             &
           'Epoch:',epoch,'File: ',TRIM(filnam)

  irc=1

  RETURN

910  WRITE(lfnerr,'(A,/,18X,A,A,/)')                              &
           ' *** SR ReadVel: Error reading kinematic velocities', &
          'File: ',TRIM(filnam)

  CALL exitrc(2)

  END SUBROUTINE ReadVel

END MODULE
