MODULE s_SGINP
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE sginp(jfreq,mchar,window,filNam,nFil,pltamb)

!!-------------------------------------------------------------------------
!! NAME       :  SGINP
!!
!! PURPOSE    :  READ INPUT OPTIONS FROM INPUT OPTION FILE
!!               (FOR PGM SATGRA)
!!
!! SR CALLED  :  readKeys,exitrc,st2tim
!!
!! REMARKS    :  ---
!!
!! AUTHOR     :  H. BOCK
!!
!! VERSION    :  5.0
!!
!! CREATED    :  19-SEP-2001        LAST MODIFIED :  21-Sep-2010
!!
!! CHANGES    :  02-FEB-2002  DS: PLOT AMBIGUITIES FOR PHASE FILES
!!               23-APR-2003  RD: NULLIFY LOCAL POINTERS
!!               14-OCT-2003  RD: ADD IOSTAT FOR READING THE OPTIONS
!!               05-Dec-2007  LP: Process also SLR data; use SR OBSFILLST
!                30-Apr-2008  DT: Add keyWord list to OBSFILLST;
!                                 dimension of fileNumber 1->5
!                30-Apr-2008  DT: Change order of files to PZ-CZ-PS-CS-RZ
!                21-Sep-2010  RD: ST2TIM can be used as a module now
!!
!! COPYRIGHT  :  ASTRONOMICAL INSTITUTE
!!      2001      UNIVERSITY OF BERN
!!                    SWITZERLAND
!!-------------------------------------------------------------------------

  USE m_bern
  USE s_readkeys
  USE s_st2tim
  USE s_exitrc
  USE s_ckoptc
  USE s_obsfillst
  IMPLICIT NONE

! Parameters
! ----------
  INTEGER(i4b) :: jfreq,pltamb
  INTEGER(i4b) :: mchar
  REAL(r8b),DIMENSION(2) :: window
  CHARACTER(LEN=fileNameLength),   &
            DIMENSION(:,:), POINTER :: filNam ! list of observation files
  INTEGER(i4b) :: nFil

! Local Parameter
! ---------------
  CHARACTER(LEN=6), PARAMETER      :: srName = 'sginp'

! Local Variables
! ---------------
  INTEGER(i4b) :: ios
  INTEGER(i4b) :: irc
  INTEGER(i4b) :: iac
  INTEGER(i4b) :: obstype    ! 1: GNSS/2: Range obs
  INTEGER(i4b) :: nflcol     ! columns of the obsfilelist
  INTEGER(i4b) :: irCode     ! error status
  INTEGER(i4b) :: ii

  INTEGER(i4b),DIMENSION(5) :: filenumber

  CHARACTER(LEN=fileNameLength),   &
            DIMENSION(:,:), POINTER :: dummy

  CHARACTER(LEN=19) :: timstr

! Declaration for readKeys
! ------------------------
  CHARACTER(LEN=keyValueLength),DIMENSION(:),POINTER :: keyValue

! Init variables
! --------------
  NULLIFY(keyValue)
  NULLIFY(dummy)
  nFil = 0
  nflcol=0
  filenumber(:)=0

! Get the list of obs Files
! -------------------------
  CALL OBSFILLST('OBSTYPE',(/'GNSS ','Range'/),(/'PZHFIL  ','CZHFIL  ','BOTHZERO'/), &
  (/'PSHFIL  ','CSHFIL  ','BOTHSING'/),'RZHFIL',0,2,0,nflcol,filenumber,filNam,dummy)

  DO ii=1,5
    nFil = nFil + filenumber(ii)
  ENDDO

  DEALLOCATE(dummy,stat=iac)


! Read other options
! ------------------
  CALL readKeys('FREQ', keyValue, irc)
  IF (keyValue(1) == 'L1') THEN
    jfreq = 1
  ELSEIF (keyValue(1) == 'L2') THEN
    jfreq = 2
  ELSEIF (keyValue(1) == 'L3') THEN
    jfreq = 3
  ELSEIF (keyValue(1) == 'L1&L2') THEN
    jfreq = 4
  ELSE
    write(lfnErr,'(A,/,15X,A,A)')' *** SR sginp: Wrong entry for',&
         '"Frequency": ',trim(keyValue(1))
    CALL exitrc(2)
  ENDIF

  CALL readKeys('NCHAR', keyValue, irc)
  IF (irc == 0) READ(keyValue(1),*,iostat=ios) mchar
  IF (mchar > 255 .OR. mchar < 1 .OR. ios /= 0 .OR. irc /= 0) THEN
    write(lfnErr,'(A,/,15X,A,A)')' *** SR sginp: Wrong entry for',&
         '"Number of Characters": ',trim(keyValue(1))
    CALL exitrc(2)
  ENDIF

  CALL readKeys('PLOTAMB', keyValue, irc)
  pltamb = 0
  IF (irc == 0) THEN
    IF (keyValue(1) == '1') pltamb = 1
  ELSE
    CALL exitrc(2)
  ENDIF

  timstr=''
  window(:)=(/0.D0,1d20/)
  CALL readKeys('STADAT', keyValue, irc)
  IF (irc ==  0) THEN
    timstr(1:10) = ADJUSTL(keyValue(1))
  ELSE
    CALL exitrc(2)
  ENDIF

  CALL readKeys('STATIM', keyValue, irc)
  IF (irc ==  0) THEN
    timstr(12:19) = ADJUSTL(keyValue(1))
  ELSE
    CALL exitrc(2)
  ENDIF

  IF (timstr/=' ') CALL st2tim(1,1,timstr,window(1))

  CALL readKeys('ENDDAT', keyValue, irc)
  IF (irc ==  0) THEN
    timstr(1:10) = ADJUSTL(keyValue(1))
  ELSE
    CALL exitrc(2)
  ENDIF

  CALL readKeys('ENDTIM', keyValue, irc)
  IF (irc ==  0) THEN
    timstr(12:19) = ADJUSTL(keyValue(1))
  ELSE
    CALL exitrc(2)
  ENDIF
  IF (timstr/=' ') CALL st2tim(1,1,timstr,window(2))
  IF (window(1)>window(2)) THEN
    write(lfnErr,'(A,2(/,A,F15.6))')&
         'SR satmin: Wrong observation window given:',&
         '    Start: ',window(1),&
         '      End: ',window(2)
    CALL exitrc(2)
  ENDIF

! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=iac)

  RETURN
 END subroutine sginp

END MODULE
