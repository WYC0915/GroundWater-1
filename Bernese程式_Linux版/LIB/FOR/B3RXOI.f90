MODULE s_B3RXOI
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE b3rxoi(maxCom,nCom,coment,iFlgCo,runBy,maxFil,nFlInp,filnam)

! -------------------------------------------------------------------------
! Purpose:    This is a new version of the old subroutine B3RXOI.f that
!             reads the input options of the program BV3RXO
!
! Author:     D. Ineichen
!
! Created:    29-Oct-2001
! Last mod.:  17-May-2011
!
! Changes:    23-Apr-2003 CU: Nullify local pointers
!             01-Mar-2006 HB: Declare first dimension of filnam
!             17-May-2011 LM: Work-around for Lahey compiler
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern


  USE s_dimtst
  USE s_gtfile
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  USE s_gtflna
  USE s_ckoptl
  IMPLICIT NONE

! List of parameters
! ------------------
! IN:
  INTEGER(i4b)                     :: maxCom  ! Maximum number of comment lines
  INTEGER(i4b)                     :: maxFil  ! Maximum number of input files

! OUT:
  INTEGER(i4b)                     :: nCom    ! Number of comment lines
  CHARACTER(len=*), DIMENSION(:)   :: coment  ! Comment lines
  INTEGER(i4b)                     :: iFlgCo  ! 1: Add campaign name and title
                                              !    line to comments
  CHARACTER(len=*)                 :: runBy   ! Agency information for RINEX
                                              ! header
  INTEGER(i4b)                     :: nFlInp  ! Number of input files
  CHARACTER(len=*), DIMENSION(5,*) :: filnam  ! Name files of input files

! Local variables
! ---------------
  CHARACTER(LEN=fileNameLength)    :: srName, rxnFil
  CHARACTER(LEN=fileNameLength)    :: filTmp1, filTmp2, filTmp3, filTmp4
  INTEGER(i4b)                     :: irc, irCode, iRadio, iBoth
  INTEGER(i4b)                     :: nFlCol, ii

  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue

! Init variables
! --------------
  irCode = 0
  srName = 'b3rxoi'
  NULLIFY(keyValue)

! Read input and output filenames
! -------------------------------
  CALL ckoptb(1,(/'RADIO_1','RADIO_2'/),srName,                        &
              'Use phase of code files', irCode,result1=iRadio)

  nFlCol=5
  IF (iRadio==1) THEN
    CALL gtfile('PHSFIL',nFlCol,maxFil,nFlInp,filnam)

    CALL readKeys('BOTH',keyValue,irc)
    CALL ckoptb(1,(/'BOTH'/),srName,'Use corresponding code files',   &
                   irCode,result1=iBoth)
  ELSE
    CALL gtfile('CODFIL',nFlCol,maxFil,nFlInp,filnam)
  ENDIF

  CALL gtflna(0,'RXNFIL',rxnFil,irc)

  DO ii= 1,nFlInp
    IF (iRadio==1) THEN
      filTmp1 = filnam(1,ii)
      filTmp2 = filnam(2,ii)
      filTmp3 = filnam(3,ii)
      filTmp4 = filnam(4,ii)
      IF (iBoth==1) THEN
        filnam(1,ii) = filTmp3
        filnam(2,ii) = filTmp4
      ELSE
        filnam(1,ii) = ' '
        filnam(2,ii) = ' '
      ENDIF
      filnam(3,ii) = filTmp1
      filnam(4,ii) = filTmp2
    ELSE
      filnam(3,ii) = ' '
      filnam(4,ii) = ' '
    ENDIF
    IF (rxnFil /= ' ') filnam(5,ii) = rxnFil
  ENDDO

! Read main options
! -----------------
  CALL readKeys('RUNBY',keyValue,irc)
  CALL ckoptl(0,'RUNBY',keyValue,srName,'Run by',irc,irCode,                 &
              empty=' ',maxLength=20,maxVal=1,result1=runBy)

  CALL readKeys('IFLGCO',keyValue,irc)
  CALL ckoptb(1,(/'IFLGCO'/),srName,'Add campaign/title as comment', irCode, &
              result1=iFlgCo)

  nCom = 3
  CALL dimtst(1,1,2,'b3rxoi','maxCom','comment lines',                       &
              ' ',nCom, maxCom, irc)

  CALL readKeys('COMMENT1',keyValue,irc)
  CALL ckoptl(0,'COMMENT1',keyValue,srName,'Line 1',irc,irCode,              &
              maxLength=60,maxVal=1,result1=coment(1),empty= ' ')

  CALL readKeys('COMMENT2',keyValue,irc)
  CALL ckoptl(0,'COMMENT2',keyValue,srName,'Line 2',irc,irCode,              &
              maxLength=60,maxVal=1,result1=coment(2),empty=' ')

  CALL readKeys('COMMENT3',keyValue,irc)
  CALL ckoptl(0,'COMMENT3',keyValue,srName,'Line 3',irc,irCode,              &
              maxLength=60,maxVal=1,result1=coment(3),empty=' ')

! Use only non-blank coments
! --------------------------
  nCom = 0
  DO ii=1,3
    IF (coment(ii) /= ' ') THEN
      nCom = nCom+1
      coment(nCom) = coment(ii)
    ENDIF
  ENDDO

  DEALLOCATE(keyValue,stat=irc)

! Problems reading input options
! ------------------------------
  IF (irCode /= 0) CALL exitrc(2)

END SUBROUTINE b3rxoi

END MODULE
