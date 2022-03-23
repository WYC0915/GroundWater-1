MODULE s_OBSSIN
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE obssin(iepspl,timspl,filNam,nFil)

! ------------------------------------------------------------------------
! NAME       :  OBSSIN
!
! PURPOSE    :  READ OPTION INPUT FILE FOR PROGRAM "OBSSPL"
!
! SR CALLED  :  DJUL
!
! REMARKS    :  ---
!
! AUTHOR     :  H. BOCK
!
! VERSION    :  5.0
!
! CREATED    :  20-SEP-2001        LAST MODIFIED : 30-Apr-2008
!
! CHANGES    :  23-APR-2003  AJ: NULLIFY LOCAL POINTERS
!               04-OCT-2003  HU: LIST INPUT FILES
!                                HANDLE CASE WITH NO OUTPUT FILE SPECIFIED
!               09-OCT-2003  HB: CONSIDER SECONDS FOR COMPUTING EPOCH
!               14-OCT-2003  RD: DO NOT READ KEYVALUE WITHOUT IOSTAT
!               05-DEC-2007  LP: Process also SLR data; use SR OBSFILLST
!               30-Apr-2008  DT: Add keyWord list to OBSFILLST;
!                                dimension of fileNumber 1->5
!               30-Apr-2008  DT: Change order of files to PZ-CZ-PS-CS-RZ
!
! COPYRIGHT  :  ASTRONOMICAL INSTITUTE
!      2001      UNIVERSITY OF BERN
!                    SWITZERLAND
!-------------------------------------------------------------------------

  USE m_bern
  USE s_alcerr
  USE s_readkeys
  USE s_fparse
  USE s_exitrc
  USE s_ckoptc
  USE f_djul
  USE f_lengt0
  USE s_obsfillst
  IMPLICIT NONE

! Parameter
! ---------
  INTEGER(i4b) :: iepSpl         ! Split Observation Number
  REAL(r8b)    :: timSpl         ! Split Time
  CHARACTER(LEN=fileNameLength),   &
            DIMENSION(:,:), POINTER :: filNam ! list of observation files
  INTEGER(i4b) :: nFil

! Local Parameter
! ---------------
  CHARACTER(LEN=6), PARAMETER      :: srName = 'obssin'


! Local Variables
! ---------------
  REAL(r8b)    :: day

  INTEGER(i4b) :: ircc
  INTEGER(i4b) :: iac
  INTEGER(i4b) :: ios
  INTEGER(i4b) :: iretc
  INTEGER(i4b) :: iFil,jFil
  INTEGER(i4b) :: iYear
  INTEGER(i4b) :: iMth
  INTEGER(i4b) :: iDay
  INTEGER(i4b) :: iHour
  INTEGER(i4b) :: iMin
  INTEGER(i4b) :: iSec
  INTEGER(i4b) :: iSel
  INTEGER(i4b) :: obstype    ! 1: GNSS/2: Range obs
  INTEGER(i4b) :: irCode     ! error status
  INTEGER(i4b) :: nflcol     ! columns of the obsfilelist
  INTEGER(i4b) :: ii
  INTEGER(i4b),DIMENSION(5) :: filenumber


  CHARACTER(LEN=fileNameLength)  :: newstr
  CHARACTER(LEN=fileNameLength)  :: node
  CHARACTER(LEN=fileNameLength)  :: device
  CHARACTER(LEN=fileNameLength)  :: dir
  CHARACTER(LEN=fileNameLength)  :: name
  CHARACTER(LEN=fileNameLength)  :: ext
  CHARACTER(LEN=fileNameLength)  :: ver
  CHARACTER(LEN=fileNameLength),   &
            DIMENSION(:,:), POINTER :: dummy
  CHARACTER(LEN=fileNameLength),   &
            DIMENSION(:,:), POINTER :: filhlp


! Declaration for readKeys
! ------------------------
  CHARACTER(LEN=keyValueLength),DIMENSION(:),POINTER :: keyValue
  INTEGER(i4b) :: irc

! Init variables
! --------------
  NULLIFY(keyValue)
  NULLIFY(dummy)
  NULLIFY(filhlp)
  nFil = 0
  nflcol=0
  ircc=0


! Get the list of obs Files
! ----------------------------------
  CALL OBSFILLST('OBSTYPE',(/'GNSS ','Range'/),(/'PZHFIL  ','CZHFIL  ','BOTHZERO'/), &
  (/'PSHFIL  ','CSHFIL  ','BOTHSING'/),'RZHFIL',0,2,1,nflcol,filenumber,filhlp,dummy)

  DO ii=1,5
    nFil = nFil + filenumber(ii)
  ENDDO

  ALLOCATE(filNam(4,nFil), stat=iac)
  CALL alcerr(iac, 'filNam', (/4,nFil/), srName)
  filNam=' '
  filnam(1,:)=filhlp(1,:)
  filnam(2,:)=filhlp(2,:)
  DEALLOCATE(filhlp,stat=iac)
  DEALLOCATE(dummy,stat=iac)


! Read file name for second file
! ------------------------------
  CALL readKeys('FILNEW', keyValue, irc)
  IF (len_trim(keyValue(1)) == 0) THEN
    write(lfnErr,'(A,/,A)')&
         '*** SR obssin: No name or last character for the output file(s)',&
         '               is specified.'
    CALL exitrc(2)
  ENDIF
  newstr = ADJUSTL(keyValue(1))


! New file name of second file
! ----------------------------
  isel = 1
  IF (LENGT0(trim(newstr)) == 1) THEN
    DO iFil=1,nFil
     CALL fparse(1,filNam(1,iFil),node,device,dir,name,ext,ver,iretc)
     name(8:8)= newstr
     filNam(3,iFil)=trim(node)//trim(device)//trim(dir)//trim(name)//trim(ext)
     CALL fparse(1,filNam(2,iFil),node,device,dir,name,ext,ver,iretc)
     name(8:8)= newstr
     filNam(4,iFil)=trim(node)//trim(device)//trim(dir)//trim(name)//trim(ext)
    ENDDO
  ELSE
    DO iFil=1,nFil
     CALL fparse(isel,filNam(1,iFil),node,device,dir,name,ext,ver,iretc)
     name= newstr
     filNam(3,iFil)=trim(node)//trim(device)//trim(dir)//trim(name)//'.'//trim(ext)
     CALL fparse(isel,filNam(2,iFil),node,device,dir,name,ext,ver,iretc)
     name= newstr
     filNam(4,iFil)=trim(node)//trim(device)//trim(dir)//trim(name)//'.'//trim(ext)
     DO jFil=1,iFil
        IF (jFil>1) THEN
           IF ((filNam(3,iFil)==filNam(3,jFil)).OR.(filNam(4,iFil)==filNam(4,jFil))) THEN
                  write(lfnErr,'(A,/,A,/,A)')&
                  '*** SR obssin: There is only one file name given for the',&
                  '               second result file but a list of files should be',&
                  '               splitted. This is not possible.'
                  CALL exitrc(2)
           ENDIF
        ENDIF
     ENDDO
    ENDDO
  ENDIF


! Read Split Epoch Number
! -----------------------
  iepSpl = 0
  timSpl = 0.D0
  CALL readKeys('RADIO_1', keyValue, irc)
  IF (keyValue(1) == '1') THEN
    CALL readKeys('OBSNUM', keyValue, irc)
    READ(keyValue(1),*,iostat=ios)iepSpl
    ircc=ircc+ios
  ELSE
! Read Split Epoch (Date and Time)
! --------------------------------
    CALL readKeys('STADAT', keyValue, irc)
    READ(keyValue(1),*,iostat=ios)iYear,iMth,iDay
    ircc=ircc+ios
    CALL readKeys('STATIM', keyValue, irc)
    READ(keyValue(1),*,iostat=ios)iHour,iMin,iSec
    ircc=ircc+ios
    day=iDay+iHour/24.D0+iMin/1440.D0+iSec/86400.D0
    timSpl=DJUL(iYear,iMth,day)
  ENDIF

  IF (ircc/=0) THEN
    write(lfnErr,*)&
         '*** SR obssin: No split observation time specified'
    CALL exitrc(2)
  ENDIF

  DEALLOCATE(keyValue,stat=iac)

  RETURN
  END SUBROUTINE obssin


END MODULE
