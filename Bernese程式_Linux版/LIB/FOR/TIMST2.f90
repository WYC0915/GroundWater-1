MODULE s_timst2

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

! -------------------------------------------------------------------------
! Purpose:   Write one or two dates with time on a string with
!            the following format:
!             (1)  1990-12-24 20:13:34    or
!                  1990-12-24 20:13:34  1990-12-25 21:13:13
!             (2)  1990 12 24 20 13 34    or
!                  1990 12 24 20 13 34  1990 12 25 21 13 13
!                the string length is either 19 or 40 characters
!             (3)  2009 08 08 00 01 30.0000000
!                the string length need to be adopted according to the
!                  specified number of fractionals of the second
!
! Author:     M. Rothacher, U. Hugentobler
!
! Created:    30-Jul-2005
! Last mod.:  21-Sep-2010
!
! Changes:    30-Jul-2005 HU: Copy from TIMST2.f
!             21-Sep-2010 RD: Format type 3 added, more detailed messages
!
! SR used:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

INTERFACE timst2
  MODULE PROCEDURE timst2_arg1, timst2_arg2
END INTERFACE

CONTAINS

! -------------------------------------------------------------------------
! Subroutine with non-array time variable
! -------------------------------------------------------------------------

  SUBROUTINE timst2_arg1(ifmt,ntim,tmjd,tstrng,nFrac)
    USE m_bern
    USE s_jmt
    USE s_radgms
    USE s_exitrc
    IMPLICIT NONE

! List of parameters
! ------------------
! input:
    INTEGER(i4b)                   :: ifmt   ! Format specification
    INTEGER(i4b)                   :: ntim   ! Number of arguments
    REAL(r8b)                      :: tmjd   ! Epoch
    INTEGER(i4b), OPTIONAL         :: nFrac  ! Number of fractionals of a
                                             ! second in case of (ifmt==3)

! output
    CHARACTER(LEN=*)               :: tstrng ! Result string
  !
! Local variables
! ---------------
    INTEGER(i4b)     :: iday  , ihour , imin  , imonth, &
                        isec  , itim  , iyear
    REAL(r8b)        :: day   , sec   , thlp
    CHARACTER(LEN=1) :: vorz
    CHARACTER(LEN=40):: frmStr

! Rounding problems
! -----------------
    tHlp   = tMjd
    IF (PRESENT(nFrac)) THEN
       tHlp   = tHlp+10.d0**(-nfrac-7)
    ELSE
       tHlp   = tHlp+10.d0**(-7)
    ENDIF

! Generate string
! ---------------
    tstrng=' '

! String blank if tmjd=0.D0 and/or tmjd=1.D20
! -------------------------------------------
    IF (tmjd /= 0.D0 .AND. tmjd /= 1.D20) THEN

! Make some checks
! ----------------
      IF (ifmt == 1 .OR. ifmt == 2) THEN
        IF (LEN(tstrng) < 19) THEN
          WRITE(lfnErr,'(/,A,A,2(/,16X,A,I4),/)')' *** SR TIMST2: ', &
                'The result string is too short.',                   &
                'Current  length of the string: ', LEN(tstrng),      &
                'Required length of the string: ', 19
          CALL exitrc(2)
        ENDIF
      ELSEIF (ifmt == 3) THEN
        IF (.NOT. PRESENT(nFrac)) THEN
          WRITE(lfnErr,'(/,A,A,2(/,16X,A),/)')   ' *** SR TIMST2: ', &
                'The number of fractionals for the seconds',         &
                'in the resulting string is not specified.',         &
                '(required optional argument for format type 3)'
          CALL exitrc(2)
        ENDIF
        IF (LEN(tstrng) < 20+nFrac) THEN
          WRITE(lfnErr,'(/,A,A,2(/,16X,A,I4),/)')' *** SR TIMST2: ', &
                'The result string is too short.',                   &
                'Current  length of the string: ', LEN(tstrng),      &
                'Required length of the string: ', 20+nFrac
          CALL exitrc(2)
        ENDIF
      ELSE
        WRITE(lfnErr,'(/,A,A,I4,/,16X,A,/)')     ' *** SR TIMST2: ', &
              'Unknown format identified: ',ifmt,                    &
              '(only values between 1 and 3 are supported)'
        CALL exitrc(2)
      ENDIF

! Convert modified julian date to date and time
      CALL jmt(thlp,iyear,imonth,day)
      iday=IDINT(day)
      CALL radgms(3,day,vorz,ihour,imin,sec)
      isec=IDNINT(sec)

! Write on string
      IF (ifmt == 1) THEN
        WRITE(tstrng,'(I4.4,2("-",I2.2),I3.2,2(":",I2.2))') &
              iyear,imonth,iday,ihour,imin,NINT(sec)
      ELSEIF (ifmt == 2) THEN
        WRITE(tstrng,'(I4.4,2(" ",I2.2),I3.2,2(" ",I2.2))') &
              iyear,imonth,iday,ihour,imin,NINT(sec)
      ELSEIF (ifmt == 3) THEN
        WRITE(frmStr,'(A,I2.2,A,I2.2,A)') &
              "(I4.4,2(' ',I2.2),2(' ',I2.2),F",nFrac+4,'.',nFrac,')'
        WRITE(tstrng,frmStr) iyear,imonth,iday,ihour,imin,sec
      ENDIF

    ENDIF

! End
    RETURN
  END SUBROUTINE timst2_arg1


! -------------------------------------------------------------------------
! Subroutine with array time variable
! -------------------------------------------------------------------------

  SUBROUTINE timst2_arg2(ifmt,ntim,twin,tstrng,nFrac)
    USE m_bern
    USE s_exitrc
    IMPLICIT NONE

! List of parameters
! ------------------
! input:
    INTEGER(i4b)                   :: ifmt   ! Format specification
    INTEGER(i4b)                   :: ntim   ! Number of arguments
    REAL(r8b), DIMENSION(2)        :: twin   ! Two epochs
    INTEGER(i4b), OPTIONAL         :: nFrac  ! Number of fractionals of a
                                             ! second in case of (ifmt==3)

! output
    CHARACTER(LEN=*)               :: tstrng ! Result string

! Local variables
! ---------------
    INTEGER(i4b)                   :: itim, ich1, ich2

! Generate string
! ---------------
    tstrng=' '

! String blank if tmjd=0.D0 and/or tmjd=1.D20
! -------------------------------------------
    IF (twin(1) /= 0.D0 .OR. twin(2) /= 1.D20) THEN

! Make some checks
! ----------------
      IF (ifmt == 1 .OR. ifmt == 2) THEN
        IF (LEN(tstrng) < 40) THEN
          WRITE(lfnErr,'(/,A,A,2(/,16X,A,I4),/)') ' *** SR TIMST2: ', &
                'The result string is too short.',                    &
                'Current  length of the string: ', LEN(tstrng),       &
                'Required length of the string: ', 40
          CALL exitrc(2)
        ENDIF
      ELSEIF (ifmt == 3) THEN
        IF (.NOT. PRESENT(nFrac)) THEN
          WRITE(lfnErr,'(/,A,A,2(/,8X,A),/)')     ' *** SR TIMST2: ', &
                'The number of fractionals for the seconds',          &
                'in the resulting string is not specified.',          &
                '(required optional argument for format type 3)'
          CALL exitrc(2)
        ENDIF
        IF (LEN(tstrng) < 2*(20+nFrac)+2) THEN
          WRITE(lfnErr,'(/,A,A,2(/,16X,A,I4),/)') ' *** SR TIMST2: ', &
                'The result string is too short:',                    &
                'Current  length of the string: ', LEN(tstrng),       &
                'Required length of the string: ', 2*(20+nFrac)+2
          CALL exitrc(2)
        ENDIF
      ELSE
        WRITE(lfnErr,'(/,A,A,I4,/,16X,A,/)')      ' *** SR TIMST2: ', &
              'Unknown format identified: ',ifmt,                     &
              '(only values between 1 and 3 are supported)'
        CALL exitrc(2)
      ENDIF

! Write both epochs
! -----------------
      DO itim = 1,2
        IF (ifmt == 1 .OR. ifmt == 2) THEN
          ich1=(itim-1)*21+1
          ich2=(itim-1)*21+19
          CALL timst2_arg1(ifmt,1,twin(itim),tstrng(ich1:ich2))
        ELSEIF (ifmt == 3) THEN
          ich1=(itim-1)*(22+nFrac)+1
          ich2=(itim-1)*(22+nFrac)+20+nFrac
          CALL timst2_arg1(ifmt,1,twin(itim),tstrng(ich1:ich2),nFrac)
        ENDIF
      ENDDO
    ENDIF


    RETURN
  END SUBROUTINE timst2_arg2

END MODULE
