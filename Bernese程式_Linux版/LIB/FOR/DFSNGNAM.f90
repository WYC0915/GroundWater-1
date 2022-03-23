MODULE s_DFSNGNAM
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE dfSngNam(staZero, nSngFil, icombi, name4)

! -------------------------------------------------------------------------
!
! Purpose:    Define the Names of Single-difference Files using the
!             Table of 2-character Abbreviations
!
! Author:     L. Mervart
!
! Created:    02-Jun-2000
! Last mod.:  03-Nov-2003
!
! Changes:    13-Dec-2000 RD: filAbbr chr(fileNameLength), old chr(80)
!             05-Sep-2001 HU: Dynamic allocation of arrays
!             30-Sep-2001 HU: Interface for gtstab moved to I_ASTLIB
!             06-Oct-2001 HU: Interface for gtstab moved back to I_GPSLIB
!             18-Mar-2003 RD: Use structure for station abbreviations
!             23-Apr-2003 CU: Deallocate local pointers
!             16-May-2003 AJ: Initialize structure
!             03-Nov-2003 RD: Call SR gtabbv instead of SR getabb
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_abbrev, ONLY: t_abbrev,init_abbrev

  USE s_gtabbv
  USE s_readabb
  USE s_exitrc
  USE s_gtflna
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*), DIMENSION(*)   :: staZero ! Station Names
  INTEGER(i4b)                     :: nSngFil ! number of single-dif. files
  INTEGER(i4b)    , DIMENSION(2,*) :: icombi  ! file numbers to be combined
                                              ! to form a single-diff. file

! output:
  CHARACTER(LEN=*), DIMENSION(*)   :: name4   ! single difference files

! Functions
! ---------

! Local Types
! -----------

! Local Parameters
! ----------------

! Local Variables
! ---------------
  TYPE(t_abbrev)                                       :: abbrev

  CHARACTER(LEN=fileNameLength)                        :: filAbbr
  CHARACTER(LEN=2), DIMENSION(2)                       :: ab2

  INTEGER(i4b)                                         :: nsta
  INTEGER(i4b),     DIMENSION(:), POINTER              :: abbIdx
  INTEGER(i4b)                                         :: ii,jj
  INTEGER(i4b)                                         :: i0,j0
  INTEGER(i4b)                                         :: irc

  LOGICAL                                              :: prtErr

! Read Station Name Abbreviations
! -------------------------------
  CALL init_abbrev(abbrev)
  CALL gtflna(1,'ABBREV ', filAbbr, irc)
  CALL readAbb(filAbbr,abbrev)

  NULLIFY(abbIdx)

  DO ii = 1, nSngFil
    ab2(:) = ''

    DO jj = 1,2

      CALL gtAbbv(0,staZero(icombi(jj,ii)),1,filabbr,abbrev,nSta,abbIdx)

      IF (nSta == 0) THEN
        WRITE(LFNERR,*) '### SR DFSNGNAM: No abbreviation found for ',  &
                           TRIM(staZero(icombi(jj,ii)))
        CALL exitrc(2)
      ELSE IF (nSta > 1) THEN

        prtErr = .TRUE.
        IF (ii > 1) THEN
          DO i0 = 1,ii-1
            DO j0 = 1,2
              IF (staZero(icombi(jj,ii)) == staZero(icombi(j0,i0))) THEN
                prtErr = .FALSE.
                EXIT
              ENDIF
            ENDDO
          ENDDO
        ENDIF

        IF (prtErr) THEN
          WRITE(LFNERR,*) '### SR DFSNGNAM: More than one abbreviation ', &
                          'found for ' // TRIM(staZero(icombi(jj,ii)))
        ENDIF

        ab2(jj) = abbrev%abb(abbIdx(nSta))%staAb2

      ELSE

        ab2(jj) = abbrev%abb(abbIdx(nSta))%staAb2

      ENDIF

    ENDDO
    name4(ii) = ab2(1)(1:2) // ab2(2)(1:2)
  ENDDO

  DEALLOCATE(abbIdx,stat=irc)
  DEALLOCATE(abbrev%abb,stat=irc)

  RETURN
END SUBROUTINE dfSngNam

END MODULE
