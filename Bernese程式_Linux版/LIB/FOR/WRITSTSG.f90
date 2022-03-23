MODULE s_WRITSTSG
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE writstsg(filnam, nSigma, staList)

! -------------------------------------------------------------------------
! Purpose:    Write station selection / station sigma files
!               - nSigma == 0: for a station list without sigmas
!               - nSigma == 1: Coordinate/Velocity sigmas (ADDNEQ2)
!               - nSigma == 2: Troposhere sigmas (abs_vert, rel_vert)
!               - nSigma == 3: Coordinate/Velocity sigmas (north, east, up)
!               - nSigma == 4: Troposhere sigmas
!                              (abs_vert, rel_vert, abs_grd, rel_grd)
!               - nSigma must be equalk to the number of sigma columns in the
!                      file (otherwise the SR will stop the pgm with an err!)
!
! Author:     R. Dach
!
! Created:    23-Aug-2001
! Last mod.:  23-Aug-2001
!
! Changes:    __-___-____ __:
!
! SR used:    opnfil, opnerr, alcerr, exitrc
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_stalst, ONLY: t_staList
  USE s_exitrc
  USE s_opnfil
  USE s_opnerr
  USE s_maxtst
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)  :: filNam    ! Name of the file
  INTEGER(i4b)      :: nSigma    ! Number of sigmas expected

! output:
  TYPE(t_staList)   :: staList   ! Station list with sigmas

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------

! Local Variables
! ---------------
  CHARACTER(LEN=shortLineLength) :: line1, line2

  INTEGER(i4b)                   :: iSta, iSig
  INTEGER(i4b)                   :: i1, i2
  INTEGER(i4b)                   :: ios

! Init variables
! --------------
  i1 = 0
  i2 = 0

! Check the dimensions of the arrays
! ----------------------------------
  CALL maxtst(1, 'writstsg', 'staList%nSta', SIZE(staList%staNam), &
              staList%nSta, ios)
  IF (nSigma > 0) THEN
    CALL maxtst(1, 'writstsg', 'staList%nSta', SIZE(staList%sigma,2), &
                staList%nSta, i1)
    CALL maxtst(0, 'writstsg', 'nSigma', SIZE(staList%sigma,1), nSigma, i2)
  ENDIF
  IF (ios /= 0 .OR. i1 /= 0 .OR. i2 /= 0) THEN
    WRITE(lfnerr,'(18X,A,/)') 'This is a bug in the source code!'
    CALL exitrc(2)
  ENDIF

! Open the file
! -------------
  CALL opnfil(lfnloc,filnam,'UNKNOWN','FORMATTED',' ',' ',ios)
  CALL opnerr(lfnerr,lfnloc,ios,filnam,'writstsg')

! Write title and header
! ----------------------
  line1 = 'Station name        '
  line2 = '****************    '

  DO iSig = 1, nSigma
    i1 = 21 + 10 * (iSig-1)
    i2 = 20 + 10 *  iSig
    WRITE(line1(i1:i2),'(A,I1)') '    sigma',iSig
    WRITE(line2(i1:i2),'(A)') '   **.****'
  ENDDO

  WRITE(lfnloc,'(A,/,80("-"),/,/,A,/,A)') staList%title,line1,line2

! Write a data line
! -----------------
  DO iSta = 1, staList%nSta
    line1 = staList%staNam(iSta)

    DO iSig = 1, nSigma
      i1 = 21 + 10 * (iSig-1)
      i2 = 20 + 10 *  iSig
      WRITE(line1(i1:i2),'(F10.4)') staList%sigma(iSig,iSta)
    ENDDO

    WRITE(lfnloc,'(A)') TRIM(line1)
  ENDDO ! next staion

  WRITE(lfnloc,*)
  CLOSE(lfnloc)

  RETURN
END SUBROUTINE writstsg

END MODULE
