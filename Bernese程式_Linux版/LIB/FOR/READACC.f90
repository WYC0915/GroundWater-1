MODULE s_READACC
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE readacc(epoch,acc,irc,sat_index)

!!-------------------------------------------------------------------------
!! NAME       : readacc
!!
!! PURPOSE    : read LEO acceleration file
!!
!! SR CALLED  : gtflna, opnfil, opnerr, exitrc, alcErr
!!
!! REMARKS    :
!!
!! AUTHOR     :  H.BOCK
!!
!! VERSION    :  5.0
!!
!! CREATED    :  28-AUG-2000
!!
!! CHANGES    :  25-JAN-2001 HB: USE M_BERN INSTEAD OF M_CHAMP
!!               05-Oct-2007 LP: USE ACCELEROMETER FILE FOR SIMULATION ALSO
!!                                (using an own keyword, without smoothing,
!!                                 without reduction to zero)
!!               07-Dec-2007 GB: Read and use more than one accelerometer file
!                13-Jan-2009 RD: Use '' as EOF-identifier in NEXTLINE
!                27-Apr-2012 RD: Nullify all pointers, use m_bern with only
!                27-Apr-2012 RD: Remove unused variables
!!
!! COPYRIGHT  :  ASTRONOMICAL INSTITUTE
!!      2000      UNIVERSITY OF BERN
!!                    SWITZERLAND
!!-------------------------------------------------------------------------

  USE m_bern,  ONLY: i4b, r8b, lfnloc, lfnerr, &
                     keyValueLength, lineLength, fileNameLength
  USE p_gravdet, ONLY: m_indeo

  USE s_exitrc
  USE s_opnfil
  USE s_alcerr
  USE s_gtflna
  USE s_opnerr
  USE f_nextline
  USE s_ckopti
  USE s_readkeys
  USE s_gtfile
  IMPLICIT NONE

! DUMMY ARGUMENTS
! ---------------
! IN:
  REAL(r8b)                :: epoch     ! requested epoch

! OUT:
  REAL(r8b),DIMENSION(3,2) :: acc       ! accelerations (linear and angular)
  INTEGER(i4b)             :: irc       ! return code
                                        ! 0: okay
                                        ! 1: no data found or in wrong system
  INTEGER(i4b),INTENT(IN), OPTIONAL :: sat_index ! satellite index (optional)

! PARAMETERS
! ----------
  CHARACTER(LEN=lineLength)                   :: line
  CHARACTER(LEN=fileNameLength)               :: filNam
  CHARACTER(LEN=3)                            :: xyzflg
  CHARACTER(LEN=fileNameLength),DIMENSION(2,2) :: FILNAME

  INTEGER(i4b)                                :: ioStat
  INTEGER(i4b)                                :: iac
  INTEGER(i4b)                                :: iEpo
  INTEGER(i4b),DIMENSION(2),SAVE              :: kEpo
  INTEGER(i4b)                                :: kkepo
  INTEGER(i4b),DIMENSION(2),SAVE              :: lEpo
  INTEGER(i4b),DIMENSION(2),SAVE              :: nEpo
  INTEGER(i4b),SAVE                           :: nnEpo=86401
  INTEGER(i4b)                                :: nhalf
  INTEGER(i4b),DIMENSION(3)                   :: ACC_ACT
  INTEGER(i4b),SAVE                           :: nfil
  INTEGER(i4b)                                :: index


  REAL(r8b)                                   :: epo
  REAL(r8b),DIMENSION(2),SAVE                 :: mjdEpo
  REAL(r8b),DIMENSION(:),ALLOCATABLE          :: varinf
  REAL(r8b),DIMENSION(:,:),ALLOCATABLE,SAVE   :: epoSeq
  REAL(r8b),DIMENSION(:),ALLOCATABLE, SAVE    :: epos
  REAL(r8b),DIMENSION(:,:,:,:),ALLOCATABLE,SAVE :: aclaca
  REAL(r8b),DIMENSION(:,:,:),ALLOCATABLE,SAVE :: aclaca_smooth
  REAL(r8b), DIMENSION(3)                     :: mean_value
  REAL(r8b)                                   :: max_along, epo_help

  LOGICAL,DIMENSION(2),SAVE  :: first=.TRUE.

! Declaration for readKeys
! ------------------------
  CHARACTER(LEN=keyValueLength),DIMENSION(:),POINTER :: keyValue
  INTEGER(i4b) :: irCode
  CHARACTER(LEN=7),  PARAMETER :: srName = 'READACC'

! INITIALIZATION
! --------------
  NULLIFY(keyValue)
  irc=0
  irCode = 0

! Satellite Index
! ---------------
  index = 1
  IF(PRESENT(sat_index))index=sat_index

! FIRST CALL OF SUBROUTINE
! ------------------------
  IF (first(index)) THEN
    first(index)=.FALSE.
    lEpo(index)=1
    kEpo(index)=1

    IF (m_indeo==1) THEN
    ! Look for the name of the Accelerometer file in Simulation mode
      CALL gtflna(1,'ACCEL2',filNam,irc)
    ELSE
    ! read list of accelerometer file names
      CALL GTFILE('ACCEL  ',1,2,NFIL,FILNAME)
      filnam=FILNAME(index,1)
    ENDIF
    CALL opnfil(lfnloc,filNam,'OLD',' ', ' ',' ',ioStat)
    CALL opnerr(lfnerr,lfnloc,ioStat,filNam,'READACC')
    IF (ioStat/=0) THEN
      CALL EXITRC(2)
    ENDIF
    nEpo(index)=0

! READ SYSTEM FLAG
! ----------------
    line = nextline(lfnloc,0) ! RSW
    READ(line,'(A)')xyzflg
    IF (xyzflg/='RSW') THEN
      WRITE(lfnerr,'(A,/,A)')&
           '*** sr readacc: the accelerations are stored in the wrong &
           &system: ',line
      irc=1
    ENDIF

! READ MJD
! --------
    line = nextline(lfnloc,0)
    READ(line,'(F7.0)')mjdEpo(index)
    LINE_LOOP: DO
      line = nextline(lfnloc,0)
      IF (line == '') EXIT LINE_LOOP
      nEpo(index) = nEpo(index)+1
    ENDDO LINE_LOOP
    REWIND (lfnloc)

! Exit, if number of epochs > nnepo
! ---------------------------------
    IF(nEpo(index) > nnEpo)THEN
      write(lfnerr,*)'max number of epochs exceeded'
      CALL exitrc(2)
    ENDIF

! ALLOCATE MEMORY
! ---------------
    IF(.NOT.(ALLOCATED(eposeq)))THEN
      ALLOCATE(epoSeq(nnEpo,2),stat=iac)
      CALL alcErr(iac, 'epoSeq', (/nnEpo,2/), 'readacc')
    ENDIF
    IF(ALLOCATED(epos))DEALLOCATE(epos)
    ALLOCATE(epos(nnEpo),stat=iac)
    CALL alcErr(iac, 'epos', (/nnEpo/), 'readacc')
    IF(.NOT.(ALLOCATED(aclaca)))THEN
      ALLOCATE(aclaca(3,2,nnEpo,2),stat=iac)
      CALL alcErr(iac, 'aclaca', (/3,2,nnEpo,2/), 'readacc')
    ENDIF
    IF(ALLOCATED(aclaca_smooth))DEALLOCATE(aclaca_smooth)
    ALLOCATE(aclaca_smooth(3,2,nnEpo),stat=iac)
    CALL alcErr(iac, 'aclaca_smooth', (/3,2,nnEpo/), 'readacc')
    IF(ALLOCATED(varinf))DEALLOCATE(varinf)
    ALLOCATE(varinf(nnEpo),stat=iac)
    CALL alcErr(iac, 'varinf', (/nnEpo/), 'readacc')

    line = nextline(lfnloc,0) ! RSW
    line = nextline(lfnloc,0) ! MJD

! READ ACCELERATION DATA
! ----------------------
    ACC_ACT(:)=0
    CALL readKeys('ACCR', keyValue, irc)
    CALL ckopti(1,'ACCR', keyValue, srName,       &
              'R-COMPONENT OF ACC-VECTOR',irc,irCode,       &
              maxVal=1,ge=0,le=1, result1=ACC_ACT(1))
    CALL readKeys('ACCS', keyValue, irc)
    CALL ckopti(1,'ACCS', keyValue, srName,       &
              'S-COMPONENT OF ACC-VECTOR',irc,irCode,       &
              maxVal=1,ge=0,le=1, result1=ACC_ACT(2))
    CALL readKeys('ACCW', keyValue, irc)
    CALL ckopti(1,'ACCW', keyValue, srName,       &
              'W-COMPONENT OF ACC-VECTOR',irc,irCode,       &
              maxVal=1,ge=0,le=1, result1=ACC_ACT(3))

    DATA_LOOP: DO iEpo=1,nEpo(index)
      line = nextline(lfnloc,0)
      IF (line == '') EXIT DATA_LOOP
      READ(line,'(7F19.14)')epoSeq(iEpo,index),aclaca(:,1,iEpo,index),aclaca(:,2,iEpo,index)
      IF (ACC_ACT(1).EQ.0) aclaca(1,1,iepo,index)=0.D0
      IF (ACC_ACT(2).EQ.0) aclaca(2,1,iepo,index)=0.D0
      IF (ACC_ACT(3).EQ.0) aclaca(3,1,iepo,index)=0.D0
    ENDDO DATA_LOOP

! close acc-file
! --------------
    CLOSE (lfnloc)
!
! refer accelerations to zero mean in parameter estimation mode
! -------------------------------------------------------------
    IF (m_indeo==2) THEN

!      max_along=-1.d6
      do iac=1,3
        mean_value(iac)=0.d0
        do iEpo=1,nEpo(index)
          mean_value(iac)=mean_value(iac)+aclaca(iac,1,iEpo,index)
          if(iac == 2 .and. aclaca(iac,1,iEpo,index) > max_along)then
            max_along=aclaca(iac,1,iEpo,index)
          endif
        enddo
      enddo
      mean_value(1:3)=mean_value(1:3)/nEpo(index)
!      mean_value(2)=max_along
!
      do iEpo=1,nEpo(index)
        aclaca(1:3,1,iEpo,index)=aclaca(1:3,1,iEpo,index)-mean_value(1:3)
      enddo

!
! read half window size for the smoothing filter in parameter estimation mode
! ---------------------------------------------------------------------------
      CALL readKeys('HSMOOACC', keyValue, irc)

      CALL ckopti(1,'HSMOOACC', keyValue, srName,       &
              'half size of smoothing filter window',irc,irCode,       &
              maxVal=1,ge=0,le=999, result1=nhalf)
!
! smooth the accelerometer time series
! ------------------------------------
      do iEpo=nhalf+1,nEpo(index)-nhalf
        aclaca_smooth(:,:,iEpo)=0.d0
        do kkEpo=iEpo-nhalf,iEpo+nHalf
          aclaca_smooth(:,:,iEpo)=aclaca_smooth(:,:,iEpo)+aclaca(:,:,kkEpo,index)
        enddo
        aclaca_smooth(:,:,iEpo)=aclaca_smooth(:,:,iEpo)/(2*nhalf+1)
      enddo
!
! set first and last nhalf values
      do iEpo=1,nhalf
        aclaca_smooth(:,:,iEpo)=aclaca(:,:,1,index) + &
       (aclaca_smooth(:,:,nhalf+1)-aclaca(:,:,1,index))/(EpoSeq(nhalf+1,index)-EpoSeq(1,index))* &
         (EpoSeq(iEpo,index)-EpoSeq(1,index))
        aclaca_smooth(:,:,nEpo(index)-nhalf+iEpo)=aclaca_smooth(:,:,nEpo(index)-nhalf) + &
       (aclaca(:,:,nEpo(index),index)-aclaca_smooth(:,:,nEpo(index)-nhalf))/(EpoSeq(nEpo(index),index)- &
          EpoSeq(nEpo(index)-nhalf,index))* &
         (EpoSeq(nEpo(index)-nhalf+iEpo,index)-EpoSeq(nEpo(index)-nhalf,index))
      enddo
!
! copy smoothed series into original ones
      do iEpo=1,nEpo(index)
!      write(lfnprt,"(f12.8,3d14.6,3d14.6)")epoSeq(iEpo,index),aclaca(1:3,1,iEpo,index),aclaca_smooth(1:3,1,iEpo)
        aclaca(:,:,iEpo,index)=aclaca_smooth(:,:,iEpo)
     enddo
!
! de-allocate aclaca_smooth
      DEALLOCATE(aclaca_smooth)
      DEALLOCATE(epos)
      DEALLOCATE(varinf)
    ENDIF
    kEpo(index)=1
  ENDIF

! LOOK FOR REQUESTED EPOCH
! ------------------------
  epo=epoch-mjdEpo(index)
  ! exit with error code, if requested epoch is out of tabular epochs
  IF(epo < epoSeq(1,index)-0.05D0 .or. &
     epo > epoSeq(nEpo(index),index)+0.05D0)THEN
         write(lfnerr,*)'epo, firs, last=', epo,epoSeq(1,index)-0.05D0,epoSeq(nEpo(index),index)+0.05D0
         call exitrc(2)
  ENDIF
  IF (epo >= epoSeq(kEpo(index),index)) THEN
  ! forward search
  ! --------------
    IF(kEpo(index) < nEpo(index))THEN
          lEpo(index)=kEpo(index)
    ELSE
      lEpo(index)=nEpo(index)-1
        ENDIF
!!!    lEpo(index)=1
    EPO1_LOOP: DO iEpo=lEpo(index),nEpo(index)-1
      epo_help=epoSeq(iEpo+1,index)
      if(iEpo == nEpo(index)-1 .AND. epo > epoSeq(nEpo(index),index)) then
            epo_help=epoSeq(nEpo(index),index)+0.05D0
          endif
      IF (epo >= epoSeq(iEpo,index).AND. epo <= epo_help) THEN
        acc(:,:)=(aclaca(:,:,iEpo+1,index)*(epo-epoSeq(iEpo,index))+ &
                 (aclaca(:,:,iEpo,index)*(epoSeq(iEpo+1,index)-epo)))/ &
                 (epoSeq(iEpo+1,index)-epoSeq(iEpo,index))
        kEpo(index)=iEpo
        EXIT EPO1_LOOP
      ENDIF
    ENDDO EPO1_LOOP

  ELSE
  ! backward search
  ! ---------------
    IF (kEpo(index) < nEpo(index)) THEN
          lEpo(index)=kEpo(index)+1
    ELSE
          lEpo(index)=nEpo(index)-1
        ENDIF

    EPO2_LOOP: DO iEpo=lEpo(index),2,-1
      if(iEpo == 2 .AND. epo < epoSeq(1,index)) then
            epo_help=epoSeq(1,index)-0.05D0
      else
            epo_help=epoSeq(iEpo-1,index)
          endif
      IF (epo >= epo_help .AND. epo <= epoSeq(iEpo,index)) THEN
          acc(:,:)=(aclaca(:,:,iEpo,index)*(epo-epoSeq(iEpo-1,index))+&
                   (aclaca(:,:,iEpo-1,index)*(epoSeq(iEpo,index)-epo)))/&
                   (epoSeq(iEpo,index)-epoSeq(iEpo-1,index))
!!        acc(:,:)=(aclaca(:,:,iEpo+1,index)*(epo-epoSeq(iEpo,index))+&
!!                 (aclaca(:,:,iEpo,index)*(epoSeq(iEpo+1,index)-epo)))/&
!!                 (epoSeq(iEpo+1,index)-epoSeq(iEpo,index))
        kEpo(index)=iEpo-1
        EXIT EPO2_LOOP
      ENDIF
    ENDDO EPO2_LOOP
  ENDIF

  IF (irCode /= 0) CALL exitrc(2)

  RETURN
  END SUBROUTINE readacc

END MODULE
