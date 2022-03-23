MODULE s_READPRE
CONTAINS

! ------------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! ------------------------------------------------------------------------------

  SUBROUTINE ReadPre(filnam,SVN,epoch,DALL,XYZV,irc)

! ------------------------------------------------------------------------------
!
! Purpose   : Read PRE file
!
! Remarks   : max error to indentify one epoch set to 5.0D-6
!
! Author    : D. Svehla
!
! Created   : 30-Mar-2002
!
! Changes   : 20-Jul-2002 DS: If DALL==1 and first RETURN
!             07-Mar-2003 HU: Use m_maxdim
!             08-Mar-2003 HU: Interface for sr rdpreh, rdprei used
!             01-Apr-2003 HU: Format statement corrected
!             28-Jun-2005 MM: Unused variables removed
!             01-Aug-2005 HU: Epoch as structure
!             20-Sep-2012 RD: Correctly deallocate arrays
!             20-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright : Astronomical Institute
!             University of Bern
!             Switzerland
! ------------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnerr, lfnloc, fileNameLength
  USE m_maxdim, ONLY: maxsat
  USE m_epoch,  ONLY: t_epoch, OPERATOR(.epochToReal.)

  USE s_alcerr
  USE s_rdpreh
  USE s_rdprei
  IMPLICIT NONE

!
! Parameters
! ==========
!
! IN :
! ----
  CHARACTER(LEN=fileNameLength)   :: filnam  ! Precise orbit file
  INTEGER(i4b)                    :: SVN     ! Satellite number
  REAL(r8b)                       :: epoch   ! Epoch in MJD
  INTEGER(i4b)                    :: DALL    ! Deallocate arrays
                                             !  =0 : No
                                             !  =1 : YES
!
! OUT:
! ----
  REAL(r8b),DIMENSION(6)          :: XYZV    ! precise coordinates
                                             ! with/without velocities
  INTEGER(i4b)                    :: irc     !  =0 OK
                                             !  =1 no pre data for req. epoch
!
! Local variables
! ---------------
  CHARACTER(LEN=57),DIMENSION(4)  :: TITLE
  CHARACTER(LEN=5)                :: COOSYS,DATDES
  CHARACTER(LEN=4)                :: AGENCY
  CHARACTER(LEN=3)                :: ORBTYP,TIMSYS
  CHARACTER(LEN=2)                :: FILTYP
  CHARACTER(LEN=1),DIMENSION(4,MAXSAT) :: EVTFLG

  INTEGER(i4b),DIMENSION(MAXSAT)  :: SATWGT,SATNUM
  INTEGER(i4b),DIMENSION(4,MAXSAT):: ACCPOS,ACCVEL
  INTEGER(i4b),DIMENSION(2)       :: IEREC
  INTEGER(i4b)                    :: IFRMAT,isvn

  REAL(r8b)                       :: TFIRST,DTTAB,BASPOS,BASCLK
  REAL(r8b),DIMENSION(MAXSAT)     :: DTSATC,DDTSAT
  REAL(r8b),DIMENSION(4,MAXSAT)   :: SDEVP,SDEVV
  REAL(r8b),DIMENSION(6,MAXSAT)   :: CORRP,CORRV

  TYPE(t_epoch)                   :: tmjd

  INTEGER(i4b)                    :: iepo,nstart,IRCODE,iac
  INTEGER(i4b),SAVE               :: kepo
  INTEGER(i4b),SAVE               :: nepo,NSAT

  REAL(r8b)                                      :: ErrMax
  REAL(r8b),DIMENSION(:),    ALLOCATABLE,SAVE    :: preT
  REAL(r8b),DIMENSION(:,:,:),ALLOCATABLE,SAVE    :: preXYZ

  LOGICAL,SAVE  :: first=.TRUE.
!
! INITIALIZE
! ----------
  ErrMax=5.0D-6
!
! Deallocate arrays if possible
! -----------------------------
  IF (DALL==1 .AND. (.NOT. first)) THEN
    IF (ALLOCATED(preT))   DEALLOCATE (preT)
    IF (ALLOCATED(preXYZ)) DEALLOCATE (preXYZ)
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
! READ READ HEADER OF PRECISE ORBIT FILE
! ======================================
      CALL RDPREH(filnam,lfnloc,IFRMAT,NSAT,SATNUM,SATWGT,TFIRST,         &
                  nepo,DTTAB,TITLE,DATDES,COOSYS,ORBTYP,AGENCY,           &
                  FILTYP,TIMSYS,BASPOS,BASCLK)
!
! ALLOCATE MEMORY
! ---------------
    ALLOCATE(preXYZ(6,NSAT,nepo),stat=iac)
    CALL alcerr(iac,'preXYZ',(/6,NSAT,nepo/),'readpre')
    ALLOCATE(preT(nepo),stat=iac)
    CALL alcerr(iac,'preT',(/nepo/),'readpre')


    preXYZ(6,1:NSAT,1:nepo)=0.D0
!
! READ PRECISE ORBIT FILE
! -----------------------
    DO iepo=1,nepo
      CALL RDPREI(lfnloc,IFRMAT,0,NSAT,SATNUM,tmjd,                       &
                  preXYZ(1:3,1:NSAT,iepo),preXYZ(4:6,1:NSAT,iepo),        &
                  DTSATC,DDTSAT,ACCPOS,ACCVEL,EVTFLG,IEREC,               &
                  SDEVP,SDEVV,CORRP,CORRV,IRCODE)
      preT(iepo)=.epochToReal.tmjd
    END DO
    CLOSE(lfnloc)
  END IF
!
! LOOK FOR REQUESTED EPOCH
! ------------------------
! By keeping the index in mind, so that you don't have to loop the whole list
! for each call

  IF (epoch.GE.preT(kepo)-ErrMax .AND. epoch.LE.preT(kepo)+ErrMax)THEN
    DO isvn=1,NSAT
      IF (SATNUM(isvn)==SVN) THEN
        XYZV(1:6)=preXYZ(1:6,isvn,kepo)
        kepo=kepo+1
        GOTO 999
      END IF
    END DO
  ELSE IF (epoch.GT.preT(kepo)+ErrMax) THEN
    IF (kepo.LT.nepo) THEN
      nstart=kepo+1
    ELSE
      nstart=nepo
    END IF
    DO iepo=nstart,nepo
      IF (epoch.LT.preT(iepo)-ErrMax) GOTO 900
      IF (epoch.GE.preT(iepo)-ErrMax .AND. epoch.LE.preT(iepo)+ErrMax) THEN
        DO isvn=1,NSAT
          IF (SATNUM(isvn)==SVN) THEN
            XYZV(1:6)=preXYZ(1:6,isvn,iepo)
            kepo=iepo+1
            GOTO 999
          END IF
        END DO
      END IF
    END DO
  ELSE IF (epoch.LT.preT(kepo)-ErrMax) THEN
    DO iepo=kepo-1,1,-1
      IF (epoch.GT.preT(iepo)+ErrMax) GOTO 900
      IF (epoch.GE.preT(iepo)-ErrMax .AND. epoch.LE.preT(iepo)+ErrMax) THEN
        DO isvn=1,NSAT
          IF (SATNUM(isvn)==SVN) THEN
            XYZV(1:6)=preXYZ(1:6,isvn,iepo)
            kepo=iepo+1
            GOTO 999
          END IF
        END DO
      END IF
    END DO
  END IF

900  WRITE(lfnerr,'(A74,/,16X,A6,F15.7,/,16X,A6,A32,/)')'*** SR ReadPre: &
         &NO PRE. COORDINATES FOUND FOR THE REQUESTED EPOCH AND FILE',   &
         &'EPOCH:',epoch,'FILE :',filnam

  irc=1

  999 CONTINUE
  RETURN
  END SUBROUTINE ReadPre

END MODULE
