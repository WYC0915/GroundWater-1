MODULE s_WTNUTM
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE wtnutm(filnam, nutat)

! -------------------------------------------------------------------------
! Purpose:    Write nutation model file (new format)
!
! Author:     C. Urschl
!
! Created:    13-Dec-2002
! Last mod.:  16-Jun-2003
!
! Changes:    16-Jun-2003 HU: Write nutation offsets due to precession
!
! SR used:    opnfil, opnerr
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_nutmod, ONLY: t_nutat, maxarg, maxcoeff

  USE s_opnfil
  USE s_opnerr
  IMPLICIT NONE

! List of parameters
! ------------------
! input:
  TYPE(t_nutat)      :: nutat
  CHARACTER(LEN=*)   :: filnam ! New nutation model file

! input/output:

! output:

! List of functions
! -----------------

! Local types
! -----------

! Local parameters
! ----------------
  CHARACTER(LEN=2), DIMENSION(14), PARAMETER :: argtxt = &
    (/'L ','L''','F ','D ','O ','LQ','LV','LE','LM',     &
      'LJ','LS','LU','LN','PA'/)

! Local variables
! ---------------
  INTEGER(i4b)            :: ios
  INTEGER(i4b)            :: irec
  INTEGER(i4b)            :: iper
  INTEGER(i4b)            :: ii, jj

! Open nutation model file
! ------------------------
  nutat%filnut = filnam
  CALL opnfil(lfnloc,nutat%filnut,'UNKNOWN','FORMATTED',' ',' ',ios)
  CALL opnerr(lfnerr,lfnloc,ios,nutat%filnut,'WTNUTM')

! Write title lines and nutation model name
! -----------------------------------------
  WRITE(lfnloc,'(A80,/,A,//,A,A,//)') nutat%title, &
    '----------------------------------------'  // &
    '----------------------------------------',    &
    'Nutation model name: ',nutat%nutnam

! Write precession rate corrections
! ---------------------------------
  WRITE(lfnloc,'(2(A,/),2(/,A,F13.7),2(/,A,F11.5),///,2(A,/),/,2(A,/))') &
    'Precession corrections to IAU 1976 precession model:',          &
    '--------------------------------------------------- ',          &
    'Offset in longitude (")      :', nutat%nutpre(1),               &
    'Offset in obliquity (")      :', nutat%nutpre(2),               &
    'Correction in longitude ("/c):', nutat%nutpre(3),               &
    'Correction in obliquity ("/c):', nutat%nutpre(4),               &
    'Fundamental arguments:',                                        &
    '--------------------- ',                                        &
    'ARG          A0                  A1                  A2 '  //   &
    '                 A3                  A4               R',       &
    '            (")                 ("/c)               ("/c**2)'// &
    '            ("/c**3)            ("/c**4)'


! Write coefficients of fundamental arguments
! -------------------------------------------
  DO irec = 1, 5
    WRITE(lfnloc,'(A2,1X,5F20.10,F10.0)') &
      argtxt(irec),(nutat%nutfar(ii,irec),ii=1,6)
  ENDDO
  WRITE(lfnloc,'( )')

  DO irec = 6, maxarg
    WRITE(lfnloc,'(A2,1X,5F20.10,F10.0)') &
      argtxt(irec),(nutat%nutfar(ii,irec),ii=1,6)
  ENDDO

! Titles for table with nutation terms
! ------------------------------------
  WRITE(lfnloc,'(//,3(A,/))') &
    '     MULTIPLIERS OF FUNDAMENTAL ARGUMENTS                       PERIOD '//&
    '                   LONGITUDE                                   OBLIQUITY',&
    '                                                                (days) '//&
    '     LS         LS''        LC         LC''        OC         OC''  '   //&
    '      OS         OS''',                                                   &
    '   L   L''  F   D   O  LQ  LV  LE  LM  LJ  LS  LU  LN  PA           '   //&
    '        (mas)      (mas/c)    (mas)      (mas/c)    (mas)      (mas/c) '//&
    '   (mas)      (mas/c)'

! Write nutation terms: multipliers, period, and coefficients
! -----------------------------------------------------------
  DO iper=1,nutat%nnut
    WRITE(lfnloc,'(14I4,1X,F13.3,1X,8F11.4)')                  &
      (nutat%nutmlt(ii,iper),ii=1,maxarg), nutat%nutper(iper), &
      (nutat%nutcoe(jj,iper),jj=1,maxcoeff)
  ENDDO
  WRITE(lfnloc,'( )')

  CLOSE(lfnloc)
  RETURN

END SUBROUTINE wtnutm

END MODULE
