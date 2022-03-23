MODULE s_OBSFILLST
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE obsfillst(OBSKEY,obsKeyLst,ZEROKEY,SINGKEY,RANGEKEY,obssync, &
                       hedobs,prtfillst,nflcol,filenumber,filelist,       &
                       syncfilelist)

! -------------------------------------------------------------------------
!  Purpose:   Create a list of obsfiles to be used by different progs
!
!  Author:    L.Prange
!
!  Created:   05-Dec-2007
!
!  Changes:   30-Apr-2008 DT: Add obsKeyLst to parameter list
!                             dimension of fileNumber 1->5
!             30-Apr-2008 DT: Stop if no files selected;
!             30-Apr-2008 DT: Change order of files to PZ-CZ-PS-CS-RZ
!                             (as it is used in GPSEST)
!             29-May-2008 DT: Init filHlp%nFil=0
!             06-Aug-2010 DT: nflCol corrected
!             27-Sep-2010 RD: Solve (portland) compiler problem
!             28-Apr-2012 RD: Nullify all pointers, use m_bern with only
!             07-Aug-2012 RD: Correct file counting in case of BOTH
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, lfnerr, &
                      keyNameLength, keyValueLength, fileNameLength
  USE s_gtfile2
  USE s_alcerr
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  USE s_ckoptc
  USE s_prfile
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  CHARACTER(LEN=*)              :: OBSKEY    ! Keyword for OBSTYPE selection
  CHARACTER(LEN=*),DIMENSION(:) :: obsKeyLst ! Keyword list for observation type
  CHARACTER(LEN=*),DIMENSION(:) :: ZEROKEY   ! Keyword list for zero diff. files
  CHARACTER(LEN=*),DIMENSION(:) :: SINGKEY   ! Keyword list for single diff. files
  CHARACTER(LEN=*)              :: RANGEKEY  ! Keyword for RANGE files
!!!  INTEGER(i4b)    :: obstype    ! 1: GNSS/2: Range obs
  INTEGER(i4b)                  :: obssync    ! 0: no sync/1: sync CODE and PHASE obs
  INTEGER(i4b)                  :: hedobs     ! 1: header files only/2: header and obs files
  INTEGER(i4b)                  :: prtfillst  ! 0: no/1: print list of input obs files

! output:
  INTEGER(i4b),DIMENSION(5)     :: filenumber ! number of files in the filelist
  INTEGER(i4b)                  :: nflcol     ! number of columns of the filelist
  CHARACTER(LEN=fileNameLength),DIMENSION(:,:),POINTER :: filelist ! list of files
  CHARACTER(LEN=fileNameLength),DIMENSION(:,:),POINTER :: syncfilelist ! list of files
!                                                 to be synchronized (for SATMRK only)

! List of functions
! -----------------

! Local types
! -----------
  TYPE t_fillst
    INTEGER(i4b)                     :: nFil
    INTEGER(i4b)                     :: both
    CHARACTER(LEN=fileNameLength),   &
             DIMENSION(:,:), POINTER :: filLst ! temp. list of observation files
  END TYPE t_fillst

! Local parameters
! ----------------
  CHARACTER(LEN=9), PARAMETER :: srName = 'obsfillst'

! Local Variables
! ---------------
  TYPE(t_fillst), DIMENSION(5)      :: filHlp

  CHARACTER(LEN=keynameLength),      &
                  DIMENSION(1)      :: keyHlp
  CHARACTER(LEN=keyValueLength),     &
            DIMENSION(:),   POINTER :: keyValue

  INTEGER(i4b)                      :: numFil
  INTEGER(i4b)                      :: iFil,jFil,kFil
  INTEGER(i4b)                      :: ii
  INTEGER(i4b)                      :: irCode, iac, irc
  INTEGER(i4b), DIMENSION(SIZE(obsKeyLst)) :: hlpList

  INTEGER(i4b)    :: obstype    ! 1: GNSS/2: Range obs


! Init variables
! --------------
  irCode = 0
  NULLIFY(filHlp(1)%filLst)
  NULLIFY(filHlp(2)%filLst)
  NULLIFY(filHlp(3)%filLst)
  NULLIFY(filHlp(4)%filLst)
  NULLIFY(filHlp(5)%filLst)
  NULLIFY(keyValue)
  filenumber(:) = 0

  DO ii=1,5
    filHlp(ii)%nFil = 0
  END DO

! Observation type
! ----------------
  CALL readKeys(OBSKEY, keyValue, irc)
  IF ( SIZE(obsKeyLst) == 2 ) THEN
    hlpList = (/1,2/)
  ELSEIF ( SIZE(obsKeyLst) == 3 ) THEN
    hlpList = (/1,1,2/)
  END IF
  CALL ckoptc(1,OBSKEY, keyValue, obsKeyLst,                     &
              srName, 'Observation type',irc, irCode, maxVal=1,  &
              valList=hlpList, result1=obstype)

! Re-initialize ambiguity-mode for SATMARK: GNSS Phase header files only
! ----------------------------------------------------------------------
  IF ((obssync==4) .AND. (obstype==1)) THEN
! get list of files
    nflCol = 1
    CALL gtfile2(ZEROKEY(1),nflcol,filHlp(1)%nFil,filHlp(1)%fillst)
    CALL gtfile2(SINGKEY(1),nflcol,filHlp(2)%nFil,filHlp(2)%fillst)

    filenumber(1) = filHlp(1)%nFil
    filenumber(3) = filHlp(2)%nFil
    numFil = filHlp(1)%nFil + filHlp(2)%nFil
    ALLOCATE(filelist(1,numFil),stat=iac)
    CALL alcerr(iac,'filelist',(/1,numFil/),srName)

    DO ii=1,filHlp(1)%nFil
      filelist(1,ii) = filHlp(1)%fillst(1,ii)
    END DO
    DO ii=1,filHlp(2)%nFil
      filelist(1,filHlp(1)%nFil+ii) = filHlp(2)%fillst(1,ii)
    END DO
    DO ii = 1,5
      DEALLOCATE(filHlp(ii)%filLst,stat=iac)
    ENDDO ! Loop all file lists

    RETURN
  END IF


! Range-mode for CHGHED: Range Header files only
! ----------------------------------------------
  IF ((obstype==2) .AND. (hedobs==1)) THEN
! get list of files
    nflCol = 1
    CALL gtfile2(RANGEKEY,nflcol,filHlp(5)%nFil,filHlp(5)%fillst)
    filenumber(5)   = filHlp(5)%nFil
    ALLOCATE(filelist(1,filenumber(5)),stat=iac)
    CALL alcerr(iac,'filelist',(/1,filenumber(5)/),srName)
    DO ii=1,filHlp(5)%nFil
      filelist(1,ii) = filHlp(5)%fillst(1,ii)
    END DO
    DO ii = 1,5
      DEALLOCATE(filHlp(ii)%filLst,stat=iac)
    ENDDO ! Loop all file lists
    RETURN
  END IF


! All other modes: get the list of observation files
! --------------------------------------------------
  IF (obssync > 1) obssync = 0

  nflCol = 2

  ! GNSS:
  IF (obstype == 1) THEN

    ! Option "BOTH" available and Hed+Obs files are considered
    IF ( SIZE(ZEROKEY) > 2 .AND. hedobs == 2 )  nflCol = 4

    CALL gtfile2(ZEROKEY(1), nflcol, filHlp(1)%nFil, filHlp(1)%fillst)
    CALL gtfile2(ZEROKEY(2), nflcol, filHlp(2)%nFil, filHlp(2)%fillst)
    CALL gtfile2(SINGKEY(1), nflcol, filHlp(3)%nFil, filHlp(3)%fillst)
    CALL gtfile2(SINGKEY(2), nflcol, filHlp(4)%nFil, filHlp(4)%fillst)

  ! Range:
  ELSE IF (obstype == 2) THEN
    CALL gtfile2(RANGEKEY, nflcol, filHlp(5)%nFil, filHlp(5)%fillst)
  ENDIF

! Select both was set?
! --------------------
  ! GNSS:
  IF (obstype == 1) THEN
    ! NO CODE AND PHASE OBS SYNCHRONISATION
    IF (obssync == 0) THEN
      ! CHECK FOR BOTH, CODE AND PHASE FILES, IF BOTH-BUTTON AVAILABLE
      IF (SIZE(ZEROKEY)==3) THEN
         keyHlp(1) = ZEROKEY(3)
         CALL ckoptb(1,keyHlp, srName,               &
                  'Use both zero diff. file types',irCode,   &
                  result1 = filHlp(1)%both)
      ELSE
         filHlp(1)%both = 0
      ENDIF
      filHlp(2)%both = filHlp(1)%both

      IF (SIZE(SINGKEY)==3) THEN
         keyHlp(1) = SINGKEY(3)
         CALL ckoptb(1,keyHlp, srName,                &
                  'Use both sing. diff. file types',irCode,   &
                  result1 = filHlp(3)%both)
      ELSE
         filHlp(3)%both = 0
      ENDIF
      filHlp(4)%both = filHlp(3)%both

    ! CODE AND PHASE OBS SYNCHRONISATION IN PG SATMARK
    ELSE
      filHlp(:)%both = 1
    ENDIF
  ! Range:
  ELSE IF (obstype == 2) THEN
      filHlp(:)%both = 0
  ENDIF


! Allocate the obs-file list
! --------------------------
  numFil = 0
  IF (obstype == 1) THEN
    DO ii = 1,4
      numFil = numFil + (filHlp(ii)%both+1) * filHlp(ii)%nFil
    ENDDO
  ELSE IF (obstype == 2) THEN
      numFil = filHlp(5)%nFil
  ENDIF

! Stop program if no files specified
! ----------------------------------
  IF ( numFil == 0 ) THEN
    WRITE(lfnerr,'(/,A,/)')  &
          ' *** SR OBSFILLST: No observation files specified for the program run!'
    CALL exitrc(2)
  END IF


  ALLOCATE(filelist(2,numFil), stat=iac)
  CALL alcerr(iac,'filelist',(/ 2,numFil /),srName)
  filelist = ' '

  IF ((obssync == 1) .AND. (obstype==1)) THEN
    ALLOCATE(syncfilelist(2,numFil), stat=iac)
    CALL alcerr(iac,'syncfilelist',(/ 2,numFil /),srName)
    syncfilelist = ' '
  ENDIF


! Prepare the list of files
! -------------------------
  fileNumber(:)=0
  numFil = 0

! GNSS HEADER AND OBS FILES:
  IF ((obstype==1) .AND. (hedobs==2)) THEN
   DO ii = 1,4
    DO iFil = 1,filHlp(ii)%nFil
      DO jFil = 1,4,2
        DO kFil = 1,numFil+1

          ! Put a new file set into the list
          ! --------------------------------
          IF (kFil > numFil) THEN

            numFil = numFil+1
            IF ( jFil == 1 ) THEN
              fileNumber(ii)=fileNumber(ii)+1
            ELSE IF ( ii == 1 .OR. ii == 3 ) THEN
              fileNumber(ii+1)=fileNumber(ii+1)+1
            ELSE
              fileNumber(ii-1)=fileNumber(ii-1)+1
            ENDIF
            filelist(1:2,numFil) = filHlp(ii)%filLst(jFil:jFil+1,iFil)

            IF (obssync == 1) THEN
              IF (jFil == 1) &
                syncfilelist(1:2,numFil) = filHlp(ii)%filLst(3:4,iFil)
              IF (jFil == 3) &
                syncfilelist(1:2,numFil) = filHlp(ii)%filLst(1:2,iFil)
            ENDIF

          ENDIF

          ! File is already in the list
          ! ----------------------------
          IF (filelist(1,kFil) == filHlp(ii)%filLst(jFil,iFil)) EXIT

        ENDDO ! Loop all entries in filelist

        ! Do "both" only if requested
        ! ---------------------------
        IF (filHlp(ii)%both == 0) EXIT
      ENDDO
    ENDDO ! Loop all files in filHlp(ii)%filLst
   ENDDO ! Loop all file lists

! GNSS HEADER FILES only:
  ELSE IF ((obstype==1) .AND. (hedobs==1)) THEN
   DO ii = 1,4
    DO iFil = 1,filHlp(ii)%nFil
      DO jFil = 1,2
        DO kFil = 1,numFil+1

          ! Put a new file set into the list
          ! --------------------------------
          IF (kFil > numFil) THEN
            numFil = numFil+1
            IF ( jFil == 1 ) THEN
              fileNumber(ii)=fileNumber(ii)+1
            ELSE IF ( ii == 1 .OR. ii == 3 ) THEN
              fileNumber(ii+1)=fileNumber(ii+1)+1
            ELSE
              fileNumber(ii-1)=fileNumber(ii-1)+1
            ENDIF
            filelist(1,numFil) = filHlp(ii)%filLst(jFil,iFil)
          ENDIF

          ! File is allready in the list
          ! ----------------------------
          IF (filelist(1,kFil) == filHlp(ii)%filLst(jFil,iFil)) EXIT

        ENDDO ! Loop all entries in filelist

        ! Do "both" only if requested
        ! ---------------------------
        IF (filHlp(ii)%both == 0) EXIT
      ENDDO
    ENDDO ! Loop all files in filHlp(ii)%filLst
   ENDDO ! Loop all file lists

! Range header and obs files:
  ELSE IF (obstype == 2) THEN
    fileNumber(5)=fileNumber(5)+filHlp(5)%nFil
    DO iFil = 1,filHlp(5)%nFil
        DO kFil = 1,numFil+1
          ! Put a new file set into the list
          ! --------------------------------
          IF (kFil > numFil) THEN
            numFil = numFil+1
            filelist(1:2,numFil) = filHlp(5)%filLst(1:2,iFil)
          ENDIF
          ! File is allready in the list
          ! ----------------------------
          IF (filelist(1,kFil) == filHlp(5)%filLst(1,iFil)) EXIT
        ENDDO ! Loop all entries in filelist
    ENDDO ! Loop all files in filHlp(5)%filLst
  ENDIF


! Write File List
! ---------------
  DO ii = 1,5
    IF (filHlp(ii)%both == 1) THEN
        filHlp(ii)%both=4
    ELSE
        filHlp(ii)%both=2
    ENDIF

  ENDDO

  IF (prtfillst == 1) THEN
    IF (obstype == 1) THEN
      IF (filHlp(1)%nFil > 0) CALL prfile(ZEROKEY(1),' ',filHlp(1)%both,138)
      IF (filHlp(2)%nFil > 0) CALL prfile(ZEROKEY(2),' ',filHlp(2)%both,138)
      IF (filHlp(3)%nFil > 0) CALL prfile(SINGKEY(1),' ',filHlp(3)%both,138)
      IF (filHlp(4)%nFil > 0) CALL prfile(SINGKEY(2),' ',filHlp(4)%both,138)
    ELSE IF (obstype == 2) THEN
      IF (filHlp(5)%nFil > 0) CALL prfile(RANGEKEY,' ',filHlp(5)%both,138)
    ENDIF
  ENDIF


! An error in input options
! -------------------------
  IF (irCode /= 0) CALL exitrc(2)


! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=iac)
  DO ii = 1,5
    DEALLOCATE(filHlp(ii)%filLst,stat=iac)
  ENDDO ! Loop all file lists

  RETURN

END SUBROUTINE obsfillst

END MODULE
