MODULE s_PRFILE
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE prfile (keyW,title,nCol,nChar,nLst,filLst)

! -------------------------------------------------------------------------
!
! Purpose:    This subroutine prints file name lists into the program
!             output file.
!
! Author:     D. Ineichen
!
! Created:    04-Jul-2001
!
! Changes:    11-Jul-2001 DI: Small format corrections
!             10-Sep-2001 HU: Use gtfile2
!             19-Mar-2003 RD: Write long string with format (because IFC)
!             23-Apr-2003 AJ: Nullify local pointers
!             01-May-2003 HU: Optional arguments nlst, fillst added
!             14-Oct-2003 RD: Do not read keyValue without iostat
!             11-Jun-2012 RD: Deallocate also colTit, use m_bern with ONLY
!             17-Jul-2012 PW: Init colTit variable
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,  ONLY: i4b, lfnprt, &
                     fileNameLength, keyValueLength, lineLength

  USE s_gtfile2
  USE s_alcerr
  USE s_readkeys
  IMPLICIT NONE

! List of parameters
! ------------------
! Input:
  CHARACTER(LEN=*)               :: keyW    ! The key word to be listed
  CHARACTER(LEN=*)               :: title   ! Title of the output section
                                            ! If title == ' ' no title will be
                                            ! written
  INTEGER(i4b)                   :: nCol    ! Number of colums
  INTEGER(i4b), OPTIONAL         :: nChar   ! Number of characters for output
  INTEGER(i4b), OPTIONAL         :: nLst    ! Number of files in list
  CHARACTER(LEN=*), DIMENSION(:,:), OPTIONAL  &
                                 :: filLst  ! File list (nCol,nLst)
! Output:

! Used functions
! --------------

! Local parameters
! ----------------

! Local types
! -----------

! Local Variables
! ---------------
  INTEGER(i4b)                                              :: width
  INTEGER(i4b)                                              :: iChar
  INTEGER(i4b)                                              :: iCol
  INTEGER(i4b)                                              :: iFil
  INTEGER(i4b)                                              :: irc, iac
  INTEGER(i4b)                                              :: colPerLin
  INTEGER(i4b)                                              :: nBlock
  INTEGER(i4b)                                              :: iBlock
  INTEGER(i4b)                                              :: ii
  INTEGER(i4b)                                              :: low, upp
  INTEGER(i4b)                                              :: nFil

  CHARACTER(LEN=6)                                          :: numHlp
  CHARACTER(LEN=keyValueLength)                             :: keyHlp
  CHARACTER(LEN=lineLength)                                 :: line
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER      :: keyValue
  CHARACTER(LEN=fileNameLength), DIMENSION(:), ALLOCATABLE  :: colTit
  CHARACTER(LEN=fileNameLength), DIMENSION(:,:),POINTER     :: filNam

  NULLIFY(keyValue)
  NULLIFY(filNam)

! Get the string of line
! ----------------------
  line=''
  IF (PRESENT(nChar)) THEN
    DO iChar = 1,nChar
      line=TRIM(line)//'-'
    ENDDO
    width = nChar
  ELSE
    DO iCHar = 1,79
      line=TRIM(line)//'-'
    ENDDO
    width = 79
  ENDIF

! Get the column titles
! ---------------------
  ALLOCATE(colTit(nCol),stat=iac)
  CALL alcErr(iac, 'colTit', (/nCol/),'prfile')

  DO iCol = 1,nCol
    WRITE(keyHlp,'(a,a,i1)') keyW(1:LEN_TRIM(keyW)), "_TXT_COL_", icol
    CALL readkeys(keyHlp, keyValue, irc)
    IF (irc == 0) THEN
!      READ(keyValue(1),'(A)') colTit(iCol)
      colTit(iCol) = ADJUSTL(keyValue(1))
    ELSE
      colTit(icol) = 'Title not specified'
    END IF
  ENDDO

! Get the filenames
! -----------------
  IF (PRESENT(nLst) .AND. PRESENT (filLst)) THEN
    ALLOCATE (filNam(nCol,nLst),STAT=irc)
    CALL alcerr(irc,'filnam',(/nCol,nLst/),'PRFILE')
    nFil=nLst
    filNam(1:nCol,1:nFil)=filLst(1:nCol,1:nFil)
  ELSE
    CALL gtfile2(keyW,nCol,nFil,filNam)
  ENDIF

! Test how many columns can be written on one line
! ------------------------------------------------
  colPerLin = (width-5)/(fileNameLength+1)
  nBlock = (ncol-1)/colPerLin +1

! Write file list to output file
! ------------------------------
  IF (title /= ' ') THEN
    WRITE(lfnprt,'(1X,A)') TRIM(title)
    WRITE(lfnprt,'(1X,A,/)') line(1:LEN_TRIM(title))
  ENDIF

  DO iBlock=1,nBlock
    low = (iBlock-1)*colPerLin+1
    upp = MIN(low+colPerLin-1,nCol)

    WRITE(lfnprt,'(1X,A)') TRIM(line)
    WRITE(lfnprt,'(1X,10A)')'File  ',(colTit(ii)//' ',ii=low,upp)
    WRITE(lfnprt,'(1X,A)') TRIM(line)

    DO iFil=1,nFil
      WRITE(numHlp,'(I4,2X)') iFil
      WRITE(lfnprt,'(1X,10A)') numHlp,(filNam(ii,iFil)//' ',ii=low,upp)
    ENDDO

    IF (iBlock==nBlock) THEN
      WRITE(lfnprt,'(1X,A)') TRIM(line)
      WRITE(lfnprt,'(/)')
    ELSE
      WRITE(lfnprt,*)
    ENDIF
  ENDDO

  DEALLOCATE(keyValue,stat=iac)
  DEALLOCATE(filNam,stat=iac)
  DEALLOCATE(colTit,stat=iac)

END SUBROUTINE prfile

END MODULE
