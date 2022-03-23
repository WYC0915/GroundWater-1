MODULE s_CCPRIN
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE ccprin(maxfil,maxcom,ncom,coment,iflflg,imerge,nfil,filpre,   &
                  devout,char4i,ssess,filext,tcoll1,tcoll2,sampl,nfrmt, &
                  filscr,ifmt,ipred,iacc,izero,refepo)

! -------------------------------------------------------------------------
! Purpose:    Concatenate or merge precise orbit files
!
! Remarks:    Option reading routine for standalone program CCPREORB
!             for new menu system
!
! Author:     U. Hugentobler
!
! Created:    23-May-2002
! Last mod.:  08-Nov-2011
!
! Changes:    25-Sep-2002 HU: Remove i_astlib
!             10-Oct-2002 HU: New option: Set accuracy codes to zero
!             12-Nov-2002 HU: Read TOSP3C
!             02-Feb-2003 HU: New options, pritit, prflna moved to main pgm
!             16-May-2003 RD: Use SR gttimwin instead of readsess
!             11-Sep-2003 HU: Set iacc to zero for izero=1
!             04-Apr-2006 HB: Add sampling rate selection
!             24-Apr-2006 HB: Add selection if velocity should be written
!             21-May-2008 DT: Set sampl=0 for merging
!             08-Nov-2011 RD: New parameter refepo, reference epoch for
!                             output file naming (blank means AUTO)
!
! SR used:    opnsys, gtflna, priwin,
!             readkeys, ckoptb, ckoptl, ckoptr, gttimwin, exitrc
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------
  USE m_bern

  USE s_dimtst
  USE s_gttimwin
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  USE s_ckoptc
  USE s_ckoptr
  USE s_gtflna
  USE s_ckoptl
  USE s_ckoptd
  IMPLICIT NONE

! List of parameters
! ------------------
! Input:
  INTEGER(i4b)                  :: maxfil  ! maximum number of files
  INTEGER(i4b)                  :: maxcom  ! maximum number of comment lines
! Output:
  INTEGER(i4b)                  :: ncom    ! number of comment lines (=4)
  CHARACTER(LEN=*),DIMENSION(:) :: coment  ! comment lines
  INTEGER(i4b)                  :: iflflg  ! flag for rinex filename creation
                                           ! (set to 2!)
  INTEGER(i4b)                  :: imerge  ! 1: merge files
  INTEGER(i4b)                  :: nfil    ! number of input files
  CHARACTER(LEN=*),DIMENSION(:) :: filpre  ! input precise file names including
                                           ! full path (i=1...nfil)
  CHARACTER(LEN=*)              :: devout  ! device/path for pre output files
  CHARACTER(LEN=4)              :: char4i  ! 4-character code
                                           ! (first 4 character of filename)
  CHARACTER(LEN=1)              :: ssess   ! session character
  CHARACTER(LEN=*)              :: filext  ! extension for output files
  REAL(r8b)                     :: tcoll1  ! start epoch of window (MJD)
  REAL(r8b)                     :: tcoll2  ! end   epoch of window (MJD)
                                           ! set to zero if not defined
  REAL(r8b)                     :: sampl   ! sampling (sec) for output file
  INTEGER(i4b)                  :: nfrmt   ! writing velocities or not
  CHARACTER(LEN=*)              :: filscr  ! scratch file name
  INTEGER(i4b)                  :: ifmt    ! output format
                                           !  0: as is
                                           !  1: sp3
                                           !  2: sp3c
  INTEGER(i4b)                  :: ipred   ! Consider priority flags:
                                           !  1: Flagged epoch has low priority
  INTEGER(i4b)                  :: iacc    ! Consider accuracy codes:
                                           !  1: Take sats with good ac codes
  INTEGER(i4b)                  :: izero   ! 1: set accuracy codes of second
                                           !    file to merge to zero
  REAL(r8b)                     :: refepo  ! Reference epoch for output file
                                           ! blank if not given

! Local Variables
! ---------------
  INTEGER(i4b)                  :: irc,irCode=0
  INTEGER(i4b)                  :: icom,isel,ll

  REAL(r8b),DIMENSION(2)        :: window

  CHARACTER(LEN=4)              :: sess
  CHARACTER(LEN=25)             :: srname ='sr ccprin (pg ccpreorb)'
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue
  CHARACTER(LEN=keyNameLength)  :: keyWord

! Flag for output file creation is set to 2
! -----------------------------------------
  iflflg=2

  NULLIFY(keyValue)

! Read coment lines
! -----------------
  ncom=4
  CALL dimtst(0,2,2,'CCPRIN','MAXCOM','Comment lines',' ',ncom,maxcom,irc)
  DO icom=1,ncom
    WRITE(keyWord,"('COMMENT',I1)")icom
    CALL readkeys(keyWord, keyValue, irc)
    CALL ckoptl(0,keyWord, keyValue, 'sr ccprin', 'comment line', &
                  irc, irCode, maxVal=1, maxLength=LEN(coment(1)),  &
                  empty=' ')
    coment(icom)=keyValue(1)
  ENDDO

! Input file list
! ---------------
  CALL ckoptb(1, (/'RADIO_C','RADIO_M'/), srName, 'Concanenate or merge', &
              irCode, result1=isel)
! Concatenate
  IF (isel==1) THEN
    imerge=0
    CALL readkeys('CONCAT', keyValue, irc)
    CALL ckoptl(1,'CONCAT', keyValue, srName, 'files to concatenate', &
                irc, irCode, maxLength=LEN(filpre(1)), maxVal=maxfil, &
                nResult=nfil, result2=filpre)

! Merge
  ELSE
    imerge=1
    nfil  =2
    CALL dimtst(0,2,2,'CCPRIN','MAXFIL','Max. number of files', &
                ' ',2,maxcom,irc)
    CALL gtflna(1, 'MERGE1', filpre(1), irc)
    CALL gtflna(1, 'MERGE2', filpre(2), irc)
  ENDIF

  CALL readkeys('FORMAT', keyValue, irc)
  CALL ckoptc(1,'FORMAT', keyValue,(/'ASIS','SP3 ','SP3C'/), 'sr bpinpt', &
              'Format', irc, irCode, valList=(/0,1,2/), maxVal=1,         &
              result1=ifmt)

  CALL readkeys('DEVOUT', keyValue, irc)
  CALL ckoptl(1,'DEVOUT', keyValue, srName, 'Output path', &
              irc, irCode, maxLength=LEN(devout), maxVal=1, result1=devout)

  CALL readkeys('CHAR4', keyValue, irc)
  CALL ckoptl(1,'CHAR4', keyValue, srName, 'Character code', &
              irc, irCode, maxLength=4, empty=' ', maxVal=1, result1=char4i)

  CALL readkeys('EXTOUT', keyValue, irc)
  CALL ckoptl(1,'EXTOUT', keyValue, srName, 'Output extension', &
              irc, irCode, maxLength=LEN(filext), empty=' ', maxVal=1, &
              result1=filext)

  CALL readkeys('REFEPO', keyValue, irc)
  CALL ckoptd(1,'REFEPO', keyValue, srName, 'Reference epoch', &
              irc, irCode, empty=0d0, maxVal=1, result1=refepo)

  CALL readkeys('FILSEQ', keyValue, irc)
  CALL ckoptl(1,'FILSEQ', keyValue, srName, 'File sequence number', &
              irc, irCode, maxLength=4, empty=' ', maxVal=1, result1=sess)
  ll=MAX(LEN_TRIM(sess),1)
  ssess=sess(ll:ll)

! Read sampling (only for concatenation)
! --------------------------------------
  IF ( imerge == 0 ) THEN
    CALL readkeys('SAMPL', keyValue, irc)
    CALL ckoptr(1,'SAMPL', keyValue, srName,'Sampling rate (seconds)', &
                irc,irCode,maxVal=3600,ge=0d0,result1=sampl)
  ELSE
    sampl = 0d0
  END IF

! Read selection if velocity should be written
! --------------------------------------------
  CALL readkeys('POSVEL', keyValue, irc)
  CALL ckoptc(1,'POSVEL', keyValue,(/'ASIS','-VEL','+VEL'/), srName , &
                'Format', irc, irCode, valList=(/0,1,2/), maxVal=1, &
                 result1=nfrmt)

! Time window
! -----------
  CALL gttimwin('USEWIN',(/'RADIO_1','RADIO_2'/),        &
                (/'SESSION_YEAR','SESSION_STRG'/),       &
                (/'STADAT','STATIM','ENDDAT','ENDTIM'/), &
                window)
  tcoll1=window(1)
  tcoll2=window(2)


! Merge: Set accuracy codes for second file to zero
! -------------------------------------------------
  izero=0
  IF (imerge==1) THEN
    CALL ckoptb(1, (/'ACZERO'/), srName, 'Set accuracy codes to zero', &
                irCode, result1=izero)
  ENDIF

! Merge: Priority of good accuracy codes
! --------------------------------------
  iacc=0
  IF (imerge==1) THEN
    CALL ckoptb(1, (/'ACCPRIO'/), srName, 'Priority of accuracy codes', &
                irCode, result1=iacc)
  ENDIF
  IF (izero==1) iacc=0

! Merge: Reduce priority of epochs with prediction flag
! -----------------------------------------------------
  ipred=0
  IF (imerge==1) THEN
    CALL ckoptb(1, (/'PREDPRIO'/), srName, 'Priority of prediction flags', &
                irCode, result1=ipred)
  ENDIF

  CALL gtflna(1, 'AUXFIL', filscr, irc)

  DEALLOCATE(keyValue,STAT=irc)

  IF (irCode /= 0) CALL exitrc(2)
  RETURN
END SUBROUTINE ccprin

END MODULE
