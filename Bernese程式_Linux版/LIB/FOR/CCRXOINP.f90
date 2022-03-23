MODULE s_CCRXOINP
CONTAINS

! -------------------------------------------------------------------------
! Bernese GNSS Software Version 5.2
! -------------------------------------------------------------------------

SUBROUTINE ccrxoinp(lfnerl,lfnprl,maxfil,maxcom,ncom0,coment, &
                    runby,iflflg,inters,ioffst,issyst,nflinp,filraw, &
                    char4,devout,tcoll1,tcoll2,filseq,refepo)

! -------------------------------------------------------------------------
! Purpose:    Concatenate RINEX observation files
!
! Remarks:    Option reading routine for standalone program CCRINEXO
!             for new menu system
!
! Author:     U. Hugentobler
!
! Created:    15-Nov-2001
!
! Changes:    21-May-2002     Take last char from session, if necessary
!             25-Sep-2002 HU: Remove i_astlib
!             23-Apr-2003 CU: Nullify local pointers
!             19-May-2003 RD: Use SR gttimwin instead of SR readsess
!             19-Nov-2003 RD: Read INP-filename in READINPF
!             21-Dec-2004 HB: New parameter refepo, reference epoch for
!                             output file naming (blank means AUTO)
!             21-Jun-2005 MM: LFNUM.inc removed
!             24-May-2011 LP: ISSYST=3 for GALILEO added
!             14-Nov-2011 SL: m_bern w/ ONLY, PRITIT call changed
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------
  USE m_bern,   ONLY: i4b, r8b, fileNameLength, keyValueLength, keyNameLength, &
                      lfnPrt, lfnErr
  USE d_inpkey, ONLY: inpKey, init_inpkey

  USE s_dimtst
  USE s_prflna
  USE f_djul
  USE s_pritit
  USE s_readinpf
  USE s_gttimwin
  USE s_prfile
  USE s_readkeys
  USE s_fparse
  USE s_exitrc
  USE s_ckoptc
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
  INTEGER(i4b)                  :: iflflg  ! flag for rinex filename creation
                                           ! (set to 2!)
  INTEGER(i4b)                  :: inters  ! sampling interval (s)
                                           ! 0: take all observations
  INTEGER(i4b)                  :: ioffst  ! sampling offset to full minute (s)
  INTEGER(i4b)                  :: issyst  ! satellite system
                                           ! 1: GPS, 2: GLONASS, 0: GPS+GLONASS
  INTEGER(i4b)                  :: nflinp  ! number of input rinex files
  CHARACTER(LEN=*),DIMENSION(*) :: filraw  ! input RINEX file names including
                                           ! full path (i=1...nflinp)
  CHARACTER(LEN=4),DIMENSION(*) :: char4   ! 4-character codes (i=1...nflinp)
                                           ! (first 4 characters of filename)
  CHARACTER(LEN=*)              :: devout  ! device/path for rinex output files
  REAL(r8b)                     :: tcoll1  ! start epoch of window (MJD)
  REAL(r8b)                     :: tcoll2  ! end   epoch of window (MJD)
                                           ! set to 1d20 if not defined
  CHARACTER(LEN=1)              :: filseq  ! sequence number for output file
                                           ! blank if not given
  REAL(r8b)                     :: refepo  ! Reference epoch for output file
                                           ! blank if not given

! Local Variables
! ---------------
  INTEGER(i4b)                  :: irc,irCode=0
  INTEGER(i4b)                  :: ifil,ncom,icom,ll
  INTEGER(i4b)                  :: iYear, iMonth

  REAL(r8b),DIMENSION(2)        :: window
  REAL(r8b)                     :: day

  CHARACTER(LEN=4)              :: sess
  CHARACTER(LEN=25)             :: srname ='sr ccrxoinp (pg ccrinexo)'
  CHARACTER(LEN=fileNameLength) :: node,device,dir,name,ext,ver
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue
  CHARACTER(LEN=keyNameLength)  :: keyWord
  CHARACTER(LEN=10)             :: crefep

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
  CALL pritit('CCRINEXO','Concatenate RINEX observation files')
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
    CALL ckoptl(0,keyWord, keyValue, 'sr ccrxoinp', 'comment line', &
                  irc, irCode, maxVal=1, maxLength=LEN(coment(1)),  &
                  empty=' ')
    IF (LEN_TRIM(keyValue(1)) /= 0) THEN
      ncom0=ncom0+1
      CALL dimtst(0,2,2,'CCRXOINP','MAXCOM','Comment lines', &
                                            ' ',ncom0,maxcom,irc)
      coment(ncom0)=keyValue(1)
    ENDIF
  ENDDO

! Read additional information
! ---------------------------
  CALL readkeys('AGENCY', keyValue, irc)
  CALL ckoptl(1,'AGENCY', keyValue, srName, 'Agency running program', &
              irc, irCode, maxLength=LEN(runby), maxVal=1, result1=runby)

  CALL readkeys('SAMPLING', keyValue, irc)
  CALL ckopti(1,'SAMPLING', keyValue, srName, 'Sampling rate', &
              irc, irCode, empty=0, ge=0, maxVal=1, result1=inters)

  CALL readkeys('OFFSET', keyValue, irc)
  CALL ckopti(1,'SAMPLING', keyValue, srName, 'Offset',  &
              irc, irCode, empty=0, ge=0, maxVal=1, result1=ioffst)

! ADD_GNSS_HERE
  CALL readkeys('SATSYST', keyValue, irc)
  CALL ckoptc(1,'SATSYST', keyValue,                         &
              (/'GPS+GLONASS','GPS        ','GLONASS    ','GALILEO    '/), &
              srName, 'Satellite system', irc, irCode,       &
              maxVal=1, valList=(/0,1,2,3/), result1=issyst)

! Input file list
! ---------------
  CALL readkeys('FILELIST', keyValue, irc)
  nflinp=SIZE(keyValue)
  CALL dimtst(1,2,2,'CCRXIONP','MAXFIL','number of input RINEX files', &
              ' ',nflinp,maxfil,irc)
  DO ifil=1,nflinp
    filraw(ifil)=keyValue(ifil)
    IF (filraw(ifil) /= keyValue(ifil)) THEN
      WRITE(lfnerr,"(/,' *** SR CCRXOINP: Filename too long.', &
                  &  /,'                  Name: ',A,           &
                  &  /,'                  Maximum allowed length:',I6,/)") &
                  TRIM(keyValue(ifil)),LEN(filraw(1))
      CALL exitrc(2)
    ENDIF

    CALL fparse(0,filraw(ifil),node,device,dir,name,ext,ver,irc)
    char4(ifil)=name(1:4)
  ENDDO

  CALL readkeys('DEVOUT', keyValue, irc)
  CALL ckoptl(1,'DEVOUT', keyValue, srName, 'Output path', &
              irc, irCode, maxLength=LEN(devout), maxVal=1, result1=devout)

  CALL readkeys('REFEPO', keyValue, irc)
  CALL ckoptl(1,'REFEPO', keyValue, srName, 'Reference epoch', &
              irc, irCode, maxLength=10, empty=' ', maxVal=1, result1=crefep)
  IF (crefep == ' ') THEN
    refepo = -1.D0
  ELSE
    READ(crefep,*)iYear,iMonth,day
    refepo = DJUL(iYear,iMonth,day)
  ENDIF

  CALL readkeys('FILSEQ', keyValue, irc)
  CALL ckoptl(1,'FILSEQ', keyValue, srName, 'File sequence number', &
              irc, irCode, maxLength=4, empty=' ', maxVal=1, result1=sess)
  ll=MAX(LEN_TRIM(sess),1)
  filseq=sess(ll:ll)

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

END SUBROUTINE ccrxoinp

END MODULE
