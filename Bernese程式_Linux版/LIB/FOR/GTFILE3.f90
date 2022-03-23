MODULE s_GTFILE3
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE gtfile3(intnam, nflcol, nfil, filnam)

! -------------------------------------------------------------------------
! Purpose:    This is a new version of the old subroutine GTFILE.f.
!             The subroutine prepares the list of file names. It assumes
!             that there is a list of filenames with keyword intnam in the
!             input file. If "nflcol" > 1 the routine uses the keywords
!             intnam // "_EXT_COL_2", intnam // "_EXT_COL_3" etc. to resolve
!             the remaining filenames.
!
!             The array filnam(nflcol,nfil) is allocated within the routine
!             according to the input parameter nflcol and the number of files
!             nfil found in the input file. Declare the array filnam as
!             follows in the calling routine:
!
!             CHARACTER(LEN=fileNameLength80),DIMENSION(:,:),POINTER :: filnam
!
! Author:     L. Mervart
!
! Created:    29-Jul-2004
!
! Changes:    29-Jul-2004 HU: Copy from gtfile2, different declaration
!             19-Sep-2012 RD: Correctly deallocate arrays
!             19-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, lfnerr, backslash, fileNameLength80,  &
                      keyValueLength, fileExtLength, filePathLength

  USE s_alcerr
  USE s_readkeys
  USE s_fparse
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
! Input:
  CHARACTER(LEN=*)               :: intnam  ! keyword name
  INTEGER(i4b)                   :: nflcol  ! number of columns
! Output:
  INTEGER(i4b)                   :: nfil    ! number of files per column
                                            ! nfil=0 if first filename blank
  CHARACTER(LEN=fileNameLength80),&
          DIMENSION(:,:),POINTER :: filnam  ! list of filenames
                                            ! i=1,...,nflcol, k=1,...,nfil

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER   :: keyValue
  CHARACTER(LEN=keyValueLength)                          :: keyHlp
  CHARACTER(LEN=keyValueLength)                          :: node,device,dir
  CHARACTER(LEN=keyValueLength)                          :: name,extn,ver

  CHARACTER(LEN=fileExtLength),DIMENSION(:),ALLOCATABLE  :: ext
  CHARACTER(LEN=filePathLength),DIMENSION(:),ALLOCATABLE :: path
  INTEGER(i4b)                                           :: icol,ifil
  INTEGER(i4b)                                           :: irc,ll

  NULLIFY(keyValue)

! Count the number of files in the list
! -------------------------------------
  CALL readkeys(intnam, keyValue, irc)
  nfil = SIZE(keyValue)

! Allocate array filnam
! ---------------------
  DEALLOCATE(filnam,STAT=irc)
  ALLOCATE (filnam(nflcol,nfil),STAT=irc)
  CALL alcerr(irc,'filnam',(/nflcol,nfil/),'GTFILE3')
  filnam=' '

! Allocate array for extensions and paths
! ---------------------------------------
  ALLOCATE (ext(nflcol),STAT=irc)
  CALL alcerr(irc,'ext',(/nflcol/),'GTFILE3')
  ALLOCATE (path(nflcol),STAT=irc)
  CALL alcerr(irc,'path',(/nflcol/),'GTFILE3')

! nfil=0 if no filnames given
! ---------------------------
  IF (keyValue(1) == ' ') nfil = 0

! Read all Extensions
! -------------------
  IF (nflcol > 9) THEN
    WRITE(lfnerr,*) ' *** gtfile: too many columns: ', nflcol
    CALL exitrc(2)
  ENDIF
  DO icol = 2, nflcol
    WRITE(keyHlp,'(a,a,i1)') intnam(1:LEN_TRIM(intnam)), "_EXT_COL_", icol
    CALL readkeys(keyHlp, keyValue, irc)
    IF (irc == 0) READ(keyValue(1),*,IOSTAT=irc) ext(icol)
    IF (irc /= 0) ext(icol) = '.'
  ENDDO

! Read all Paths
! --------------
  DO icol = 2, nflcol
    WRITE(keyHlp,'(a,a,i1)') intnam(1:LEN_TRIM(intnam)), "_PTH_COL_", icol
    CALL readkeys(keyHlp, keyValue, irc)
    IF (irc == 0) THEN
      path(icol)=keyValue(1)
    ELSE
      path(icol) = '.'
    ENDIF
  ENDDO

! Read the List of Filenames
! --------------------------
  CALL readkeys(intnam, keyValue, irc)

! Compose the resulting List of Files (Extensions)
! -----------------------------------
  DO ifil = 1, nfil
    CALL fparse(1,keyValue(ifil),node,device,dir,name,extn,ver,irc)
    ll=LEN_TRIM(name)
    IF (name(ll:ll) == '.') name(ll:ll)=' '
    DO icol = 1, nflcol
      IF (icol == 1) THEN
        filnam(icol,ifil) = keyValue(ifil)
      ELSE
        IF (path(icol) == '.' .AND. ext(icol) == '.') THEN
          filnam(icol,ifil) = name
        ELSEIF (path(icol) == '.') THEN
          filnam(icol,ifil) = TRIM(name)//'.'//ext(icol)
        ELSEIF (ext(icol) == '.') THEN
          filnam(icol,ifil) = TRIM(path(icol))//name
        ELSE
          filnam(icol,ifil) = TRIM(path(icol))//TRIM(name)//'.'//ext(icol)
        ENDIF
      ENDIF
    ENDDO
  ENDDO

  DEALLOCATE(keyValue,stat=irc)
  DEALLOCATE(ext,STAT=irc)
  DEALLOCATE(path,STAT=irc)

END SUBROUTINE gtfile3

END MODULE
