MODULE s_WRITECC
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE writEcc(eccFil,eccent)

! -------------------------------------------------------------------------
! Purpose:    Write the content of Bernese Eccentricity file
!
! Author:     R. Dach
!
! Created:    07-May-2002
! Last mod.:  07-May-2002
!
! Changes:    __-___-____ __:
!
! SR called:  opnfil, opnerr
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_eccent, ONLY: eccTyp,t_eccent

  USE s_opnfil
  USE s_opnerr
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)              :: eccFil              ! Name of the file
  TYPE(t_eccent)                :: eccent               ! Eccentricities

! output:

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=7),PARAMETER    :: srName = 'writecc'

! Local Variables
! ---------------
  INTEGER(i4b)                  :: iEcc
  INTEGER(i4b)                  :: ii
  INTEGER(i4b)                  :: ios


! Nothing to do
! -------------
  IF (LEN_TRIM(eccfil) == 0) RETURN

! Open the file for writing
! -------------------------
  CALL opnfil(lfnloc,eccfil,'UNKNOWN','FORMATTED',' ',' ',ios)
  CALL opnerr(lfnerr,lfnloc,ios,eccfil,srName)

! Write the header of the file
! ----------------------------
  WRITE(lfnloc,'(A,/,A,/,A38,3X,A36,/,49X,A18)')          &
          TRIM(eccent%title),                             &
          '----------------------------------------' //   &
          '----------------------------------------',     &
          'LOCAL GEODETIC DATUM: ' // eccent%datum%name,  &
          'SYSTEM : ' // eccTyp(eccent%eccTyp)(1:1) //    &
          ' (G: GEOCENTRIC, L: LOCAL)',                   &
          'CENTER --> STATION'
  IF (eccent%eccTyp == 1) THEN
    WRITE(lfnloc,'(A,/)') &
          'NUM  STATION NAME      CENTER NAME      ' //  &
          '    DN (M)     DE (M)     DH (M)'
  ELSE
    WRITE(lfnloc,'(A,/)') &
          'NUM  STATION NAME      CENTER NAME      ' //  &
          '    DX (M)     DY (M)     DZ (M)'
  ENDIF

! Write the eccentricity records
! ------------------------------
  DO iEcc = 1,eccent%nEcc

    WRITE(lfnloc,'(I3,2X,A16,2X,A16,3F11.4)')                       &
         eccent%ecc(iEcc)%staNum, eccent%ecc(iEcc)%staNam,          &
         eccent%ecc(iEcc)%cenNam,(eccent%ecc(iEcc)%xEccen(ii),ii=1,3)

  ENDDO

  CLOSE(lfnloc)

  RETURN
END SUBROUTINE writecc

END MODULE
