MODULE s_OBSFMTFL
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE obsfmtfl(nfil, filnam)

! -------------------------------------------------------------------------
! Purpose:    This is a new version of the old subroutine OBSFMTFL.f that
!             reads the input options of the program OBSFMT
!
! Author:     C. Urschl
!
! Created:    16-aug-2000
! Last mod.:  18-Jul-2008
!
! Changes:    22-oct-2001 HB: Complete change of Input-File
!             23-Apr-2003 AJ: Nullify local pointers
!             19-Aug-2003 HU: Input file list corrected
!             18-Jul-2008 DT: Add range observation files
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE s_gtfile2
  USE s_alcerr
  USE s_prfile
  USE s_readkeys
  IMPLICIT NONE

! List of Parameters
! ------------------
  INTEGER(i4b)                     :: nfil    ! number of files
  CHARACTER(LEN=fileNameLength),   &
            DIMENSION(:,:), POINTER :: filNam ! list of observation files

! List of Functions
! -----------------
! Local Types
! -----------
! Local Parameters
! ----------------

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue

  CHARACTER(LEN=fileNameLength),   &
            DIMENSION(:,:), POINTER :: filHlp1 ! temp. list of observation files
  CHARACTER(LEN=fileNameLength),   &
            DIMENSION(:,:), POINTER :: filHlp2 ! temp. list of observation files
  CHARACTER(LEN=fileNameLength),   &
            DIMENSION(:,:), POINTER :: filHlp3 ! temp. list of observation files
  CHARACTER(LEN=fileNameLength),   &
            DIMENSION(:,:), POINTER :: filHlp4 ! temp. list of observation files
  CHARACTER(LEN=fileNameLength),   &
            DIMENSION(:,:), POINTER :: filHlp5 ! temp. list of observation files (range)

  INTEGER(i4b) :: irc
  INTEGER(i4b) :: iac
  INTEGER(i4b) :: iOpt
  INTEGER(i4b) :: zFil1,zFil2
  INTEGER(i4b) :: sFil1,sFil2
  INTEGER(i4b) :: zFil,sFil,rFil
  INTEGER(i4b) :: iFil,jFil,kFil
  INTEGER(i4b) :: zboth,sboth
  INTEGER(i4b), DIMENSION(2) :: nflcol

  NULLIFY(keyValue)
  NULLIFY(filHlp1)
  NULLIFY(filHlp2)
  NULLIFY(filHlp3)
  NULLIFY(filHlp4)
  NULLIFY(filHlp5)

! Get the list of observation Files
! ----------------------------
  zFil=0
  sFil=0
  rFil=0
  nflcol(1) = 6  ! for GNSS
  nflcol(2) = 3  ! for Range

  CALL gtfile2('CZHFIL', nflcol(1), zFil1, filhlp1)
  CALL gtfile2('PZHFIL', nflcol(1), zFil2, filhlp2)

  CALL readKeys('BOTHZERO', keyValue, irc)
  IF (keyValue(1) == '1') THEN
    zboth=6
    zFil=2*zFil1+2*zFil2
  ELSE
    zboth=3
    zfil = zfil1+zfil2
  ENDIF

  CALL gtfile2('CSHFIL', nflcol(1), sFil1, filhlp3)
  CALL gtfile2('PSHFIL', nflcol(1), sFil2, filhlp4)

  CALL readKeys('BOTHSING', keyValue, irc)
  IF (keyValue(1) == '1') THEN
    sboth=6
    sFil=2*sFil1+2*sFil2
  ELSE
    sboth=3
    sfil = sfil1+sfil2
  ENDIF

  CALL gtfile2('RZHFIL', nflcol(2), rFil, filhlp5)

  ALLOCATE(filNam(3,zFil+sFil+rFil), stat=iac)
  CALL alcerr(iac, 'filNam', (/3,zFil+sFil+rFil/), 'obsfmtfl')
  filNam(:,:)=' '

! c) get the list of zero diff Files
! ----------------------------------
  CALL readKeys('BOTHZERO', keyValue, irc)
  nFil = 0
  IF (keyValue(1) == '1') THEN
    DO iopt=1,2
      IF (iopt == 1) THEN
        DO iFil=1,zFil1
          jFilLoop1: DO jFil=1,2
            DO kFil=1,nFil
              IF (Filhlp1(2*jFil, iFil) == filNam(2,kFil)) CYCLE jFilLoop1
            ENDDO
            nFil=nFil+1
            filNam(1,nFil)=filhlp1(3*jFil-2, iFil)
            filNam(2,nFil)=filhlp1(3*jFil-1, iFil)
            filNam(3,nFil)=filhlp1(3*jFil, iFil)
          ENDDO jFilLoop1
        ENDDO
      ELSE
        DO iFil=1,zFil2
          jFilLoop2: DO jFil=1,2
            DO kFil=1,nFil
              IF (Filhlp2(2*jFil, iFil) == filNam(2,kFil)) CYCLE jFilLoop2
            ENDDO
            nFil=nFil+1
            filNam(1,nFil)=filhlp2(3*jFil-2, iFil)
            filNam(2,nFil)=filhlp2(3*jFil-1, iFil)
            filNam(3,nFil)=filhlp2(3*jFil, iFil)
          ENDDO jFilLoop2
        ENDDO
      ENDIF
    ENDDO
  ELSE
    DO iOpt=1,2
      IF (iOpt == 1) THEN
        iFilLoop1: DO iFil=1,zFil1
          DO kFil=1,nFil
            IF (filhlp1(2,iFil) == filNam(2,kFil)) CYCLE iFilLoop1
          ENDDO
          nFil=nFil+1
          filNam(1,nFil)=filhlp1(1,iFil)
          filNam(2,nFil)=filhlp1(2,iFil)
          filNam(3,nFil)=filhlp1(3,iFil)
        ENDDO iFilLoop1
      ELSE
        iFilLoop2: DO iFil=1,zFil2
          DO kFil=1,nFil
            IF (filhlp2(2,iFil) == filNam(2,kFil)) CYCLE iFilLoop2
          ENDDO
          nFil=nFil+1
          filNam(1,nFil)=filhlp2(1,iFil)
          filNam(2,nFil)=filhlp2(2,iFil)
          filNam(3,nFil)=filhlp2(3,iFil)
        ENDDO iFilLoop2
      ENDIF
    ENDDO
  ENDIF

  DEALLOCATE(filhlp1,stat=iac)
  DEALLOCATE(filhlp2,stat=iac)

! c) get the list of single diff Files
! ----------------------------------
  CALL readKeys('BOTHSING', keyValue, irc)
  IF (keyValue(1) == '1') THEN
    DO iopt=1,2
      IF (iopt == 1) THEN
        DO iFil=1,sfil1
          jFilLoop3: DO jFil=1,2
            DO kFil=1,nFil
              IF (Filhlp3(2*jFil, iFil) == filNam(2,kFil)) CYCLE jFilLoop3
            ENDDO
            nFil=nFil+1
            filNam(1,nFil)=filhlp3(3*jFil-2, iFil)
            filNam(2,nFil)=filhlp3(3*jFil-1, iFil)
            filNam(3,nFil)=filhlp3(3*jFil, iFil)
          ENDDO jFilLoop3
        ENDDO
      ELSE
        DO iFil=1,sFil2
          jFilLoop4: DO jFil=1,2
            DO kFil=1,nFil
              IF (Filhlp4(2*jFil, iFil) == filNam(2,kFil)) CYCLE jFilLoop4
            ENDDO
            nFil=nFil+1
            filNam(1,nFil)=filhlp4(3*jFil-2, iFil)
            filNam(2,nFil)=filhlp4(3*jFil-1, iFil)
            filNam(3,nFil)=filhlp4(3*jFil, iFil)
          ENDDO jFilLoop4
        ENDDO
      ENDIF
    ENDDO
  ELSE
    DO iOpt=1,2
      IF (iOpt == 1) THEN
        iFilLoop3: DO iFil=1,sFil1
          DO kFil=1,nFil
            IF (filhlp3(2,iFil) == filNam(2,kFil)) CYCLE iFilLoop3
          ENDDO
          nFil=nFil+1
          filNam(1,nFil)=filhlp3(1,iFil)
          filNam(2,nFil)=filhlp3(2,iFil)
          filNam(3,nFil)=filhlp3(3,iFil)
        ENDDO iFilLoop3
      ELSE
        iFilLoop4: DO iFil=1,sFil2
          DO kFil=1,nFil
            IF (filhlp4(2,iFil) == filNam(2,kFil)) CYCLE iFilLoop4
          ENDDO
          nFil=nFil+1
          filNam(1,nFil)=filhlp4(1,iFil)
          filNam(2,nFil)=filhlp4(2,iFil)
          filNam(3,nFil)=filhlp4(3,iFil)
        ENDDO iFilLoop4
      ENDIF
    ENDDO
  ENDIF
  DEALLOCATE(filhlp3,stat=iac)
  DEALLOCATE(filhlp4,stat=iac)

! Get the list of range observation files
! ---------------------------------------
  iFilLoop5: DO iFil=1,rFil
    DO kFil=1,nFil
      IF (filhlp5(2,iFil) == filNam(2,kFil)) CYCLE iFilLoop5
    ENDDO
    nFil=nFil+1
    filNam(1,nFil)=filhlp5(1,iFil)
    filNam(2,nFil)=filhlp5(2,iFil)
    filNam(3,nFil)=filhlp5(3,iFil)
  ENDDO iFilLoop5
  DEALLOCATE(filhlp5,stat=iac)

! Write File List
! ---------------
  IF (zfil1 > 0) CALL prfile('CZHFIL',' ',zboth,130)
  IF (zfil2 > 0) CALL prfile('PZHFIL',' ',zboth,130)
  IF (sfil1 > 0) CALL prfile('CSHFIL',' ',sboth,130)
  IF (sfil2 > 0) CALL prfile('PSHFIL',' ',sboth,130)
  IF (rfil  > 0) CALL prfile('RZHFIL',' ',3    ,130)

  DEALLOCATE(keyValue,stat=iac)

  RETURN
END SUBROUTINE obsfmtfl

END MODULE
