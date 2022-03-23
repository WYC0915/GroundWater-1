MODULE s_CCRXNINP
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE ccrxninp(lfnerl,lfnprl,maxfil,maxcom,ncom0,coment, &
                    runby,minmes,iflflg,nfleph,fileph,       &
                    char4,devout,tcoll1,tcoll2,filseq)

! -------------------------------------------------------------------------
! Purpose:    Concatenate RINEX navigation files
!
! Remarks:    Option reading routine for standalone program CCRINEXN
!             for new menu system
!
! Author:     U.Hugentobler
!
! Created:    16-Nov-2001
!
! Changes:    21-May-2002 HU: Reading of comment lines modified
!                             Lfnums from LFNUM.inc
!             25-Sep-2002 HU: Remove i_astlib
!             23-Apr-2003 CU: Nullify local pointers
!             19-May-2003 RD: Use SR gttimwin instead of SR readsess
!             19-Nov-2003 RD: Read INP-filename in READINPF
!             21-Jun-2005 MM: LFNUM.inc removed
!             14-Nov-2011 SL: m_bern w/ ONLY, PRITIT call changed
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, keyValueLength, keyNameLength, lfnPrt, lfnErr
  USE d_inpkey, ONLY: inpKey, init_inpkey
  USE s_dimtst
  USE s_prflna
  USE s_pritit
  USE s_readinpf
  USE s_gttimwin
  USE s_prfile
  USE s_readkeys
  USE s_exitrc
  USE s_opnsys
  USE s_priwin
  USE s_ckopti
  USE s_ckoptl
  IMPLICIT NONE

! List of parameters
! ------------------
! Input:
! Input/Output:
  INTEGER(i4b)                  :: lfnerl  ! lfn of err output file
  INTEGER(i4b)                  :: lfnprl  ! lfn of job output file
! Input:
  INTEGER(i4b)                  :: maxfil  ! maximum number of files
  INTEGER(i4b)                  :: maxcom  ! maximum number of comment lines
! Output:
  INTEGER(i4b)                  :: ncom0   ! number of comment lines
  CHARACTER(LEN=*),DIMENSION(*) :: coment  ! comment lines
  CHARACTER(LEN=*)              :: runby   ! agency running program
  INTEGER(i4b)                  :: minmes  ! minimum number of messages
                                           ! per satellite
  INTEGER(i4b)                  :: iflflg  ! flag for rinex filename creation
                                           ! (set to 2!)
  INTEGER(i4b)                  :: nfleph  ! number of input rinex files
  CHARACTER(LEN=*),DIMENSION(*) :: fileph  ! input RINEX file names including
                                           ! full path (i=1...nfleph)
  CHARACTER(LEN=4)              :: char4   ! first 4 characters of filename
  CHARACTER(LEN=*)              :: devout  ! device/path for rinex output files
  REAL(r8b)                     :: tcoll1  ! start epoch of window (MJD)
  REAL(r8b)                     :: tcoll2  ! end   epoch of window (MJD)
                                           ! set to 1d20 if not defined
  CHARACTER(LEN=1)              :: filseq  ! sequence number for output file
                                           ! blank if not given

! Local Variables
! ---------------
  INTEGER(i4b)                  :: irc,irCode=0
  INTEGER(i4b)                  :: ifil,ncom,icom,ll

  REAL(r8b),DIMENSION(2)        :: window

  CHARACTER(LEN=4)              :: sess
  CHARACTER(LEN=25)             :: srname ='sr ccrxninp (pg ccrinexn)'
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue
  CHARACTER(LEN=keyNameLength)  :: keyWord

! Set local logical file numbers
! ------------------------------
!!!  INCLUDE 'LFNUM.inc'
! job output
  lfnprt=lfnprl
! error output
  lfnerr=lfnerl

  NULLIFY(keyValue)

! Read input file into a buffer
! -----------------------------
  CALL init_inpkey(inpKey)
  CALL readinpf(' ',inpKey)

! Open system files
! -----------------
  CALL opnsys

! Write title and file names
! --------------------------
  CALL pritit('CCRINEXN','Concatenate GPS RINEX navigation files')
  CALL prflna
  CALL prfile('FILELIST','RINEX FILES',1)

! Flag for rinex file creation is set to 2
! ----------------------------------------
  iflflg=2

! Read coment lines
! -----------------
  ncom=3
  ncom0=0
  DO icom=1,ncom
    WRITE(keyWord,"('COMMENT',I1)")icom
    CALL readkeys(keyWord, keyValue, irc)
    CALL ckoptl(0,keyWord, keyValue, 'sr ccrxninp', 'comment line', &
                  irc, irCode, maxVal=1, maxLength=LEN(coment(1)),  &
                  empty=' ')
    IF (LEN_TRIM(keyValue(1)) /= 0) THEN
      ncom0=ncom0+1
      CALL dimtst(0,2,2,'CCRXNINP','MAXCOM','Comment lines', &
                                            ' ',ncom0,maxcom,irc)
      coment(ncom0)=keyValue(1)
    ENDIF
  ENDDO

! Read additional information
! ---------------------------
  CALL readkeys('AGENCY', keyValue, irc)
  CALL ckoptl(1,'AGENCY', keyValue, srName, 'Agency running program', &
              irc, irCode, maxLength=LEN(runby), maxVal=1, result1=runby)

  CALL readkeys('MINMESS', keyValue, irc)
  CALL ckopti(1,'MINMESS', keyValue, srName, 'Minimum number of messages', &
              irc, irCode, ge=1, maxVal=1, result1=minmes)

! Input file list
! ---------------
  CALL readkeys('FILELIST', keyValue, irc)
  nfleph=SIZE(keyValue)
  CALL dimtst(1,2,2,'CCRXNINP','MAXFIL','number of input RINEX files', &
              ' ',nfleph,maxfil,irc)
  DO ifil=1,nfleph
    fileph(ifil)=keyValue(ifil)
    IF (fileph(ifil) /= keyValue(ifil)) THEN
      WRITE(lfnerr,"(/,' *** SR CCRXNINP: Filename too long.', &
                  &  /,'                  Name: ',A,           &
                  &  /,'                  Maximum allowed length:',I6,/)") &
                  TRIM(keyValue(ifil)),LEN(fileph(1))
      CALL exitrc(2)
    ENDIF

  ENDDO

  CALL readkeys('CHAR4', keyValue, irc)
  CALL ckoptl(1,'CHAR4', keyValue, srName,                              &
              'First four characters of output file name', irc, irCode, &
              maxLength=LEN(char4), maxVal=1, empty='    ', result1=char4)

  CALL readkeys('FILSEQ', keyValue, irc)
  CALL ckoptl(1,'FILSEQ', keyValue, srName, 'File sequence number', &
              irc, irCode, maxLength=4, empty=' ', maxVal=1, result1=sess)
  ll=MAX(LEN_TRIM(sess),1)
  filseq=sess(ll:ll)

  CALL readkeys('DEVOUT', keyValue, irc)
  CALL ckoptl(1,'DEVOUT', keyValue, srName, 'Output path', &
              irc, irCode, maxLength=LEN(devout), maxVal=1, result1=devout)

! Time window
! -----------
  CALL gttimwin('USEWIN',(/'RADIO_1','RADIO_2'/),        &
                (/'SESSION_YEAR','SESSION_STRG'/),       &
                (/'STADAT','STATIM','ENDDAT','ENDTIM'/), &
                window)
  tcoll1=window(1)
  tcoll2=window(2)

  CALL priwin(1,window)

! Return logical file numbers
! ---------------------------
! job output
  lfnprl=lfnprt
! error output
  lfnerl=lfnerr

  DEALLOCATE(keyValue,stat=irc)

  IF (irCode /= 0) CALL exitrc(2)

  RETURN

END SUBROUTINE ccrxninp

END MODULE
