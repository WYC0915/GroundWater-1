MODULE s_st2tim

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

! -------------------------------------------------------------------------
! Purpose:   Read one or two dates with time on a string with
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
! Remarks:    If the date string is (partly) blank, then
!             tmjd(1)=0.d0 and/or tmjd(2)=1.d20
!
! Author:     M. Rothacher, U. Hugentobler
!
! Created:    30-Jul-2005
! Last mod.:  10-Jan-2011
!
! Changes:    30-Jul-2005 HU: Copy from TIMST2.f
!             21-Sep-2010 RD: Format type 3 added, more detailed messages
!             10-Jan-2011 SL: use m_bern with ONLY
!             10-Jan-2011 SL/SS: Report illegal time interval
!
! SR used:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

INTERFACE st2tim
  MODULE PROCEDURE st2tim_arg1, st2tim_arg2
END INTERFACE

CONTAINS

! -------------------------------------------------------------------------
! Subroutine with non-array time variable
! -------------------------------------------------------------------------

  SUBROUTINE st2tim_arg1(ifmt,ntim,tstrng,tmjd,nFrac)

    USE m_bern,    ONLY: i4b, r8b, lfnErr
    USE f_djul
    USE s_exitrc
    IMPLICIT NONE

! List of parameters
! ------------------
! input:
    INTEGER(i4b)                   :: ifmt   ! Format specification
    INTEGER(i4b)                   :: ntim   ! Number of arguments
    CHARACTER(LEN=*)               :: tstrng ! String containing the date(s)
    INTEGER(i4b), OPTIONAL         :: nFrac  ! Number of fractionals of a
                                             ! second in case of (ifmt==3)
! output:

! output
    REAL(r8b)                      :: tmjd   ! Modified julian date(s)
  !
! Local variables
! ---------------
    INTEGER(i4b)     :: iday  , ihour , imin  , imonth, &
                        isec  , itim  , iyear
    INTEGER(i4b)     :: ios
    REAL(r8b)        :: day   , sec   , thlp
    CHARACTER(LEN=1) :: vorz
    CHARACTER(LEN=40):: frmStr

! Init output values
! ------------------
    tmjd=0d0

! String blank means tmjd=0.D0
! ----------------------------
    IF (tstrng /= '') THEN

! Make some checks
! ----------------
      IF (ifmt == 1 .OR. ifmt == 2) THEN
        IF (LEN(tstrng) < 19) THEN
          WRITE(lfnErr,'(/,A,A,2(/,16X,A,I4),/)')' *** SR ST2TIM: ', &
                'The input string is too short.',                    &
                'Current  length of the string: ', LEN(tstrng),      &
                'Required length of the string: ', 19
          CALL exitrc(2)
        ENDIF
      ELSEIF (ifmt == 3 .AND. PRESENT(nFrac)) THEN
        IF (LEN(tstrng) < 20+nFrac) THEN
          WRITE(lfnErr,'(/,A,A,2(/,16X,A,I4),/)')' *** SR ST2TIM: ', &
                'The input string is too short.',                    &
                'Current  length of the string: ', LEN(tstrng),      &
                'Required length of the string: ', 20+nFrac
          CALL exitrc(2)
        ENDIF
      ELSEIF (ifmt /= 3) THEN
        WRITE(lfnErr,'(/,A,A,I4,/,16X,A,/)')     ' *** SR ST2TIM: ', &
              'Unknown format identified: ',ifmt,                    &
              '(only values between 1 and 3 are supported)'
        CALL exitrc(2)
      ENDIF

! Read from string
      IF (ifmt == 1.OR. ifmt == 2) THEN
        READ(tstrng,'(I4.4,5(1X,I2))',iostat=ios) &
              iyear,imonth,iday,ihour,imin,isec
        sec = DBLE(isec)
      ELSEIF (ifmt == 3 .AND. PRESENT(nFrac)) THEN
        WRITE(frmStr,'(A,I2.2,A,I2.2,A)') &
              "(I4.4,4(1XI2),F",nFrac+4,'.',nFrac,')'
        READ(tstrng,frmStr) iyear,imonth,iday,ihour,imin,sec
      ELSEIF (ifmt == 3) THEN
        READ(tstrng(1:16),'(I4.4,4(1X,I2))',iostat=ios) &
              iyear,imonth,iday,ihour,imin
        READ(tstrng(17:),*,iostat=ios) sec
      ENDIF

      IF (ios /= 0) THEN
        WRITE(lfnErr,'(/,A,/,16X,A,/)')                             &
              ' *** SR ST2TIM: Cannot extract the datum/time.',     &
              'String: "' // TRIM(tstrng) // '"'
        CALL exitrc(2)
      ENDIF

! Convert to modified julian date
      tmjd = djul(iyear,imonth,DBLE(iday)+ &
                  DBLE(ihour)/24.d0+DBLE(imin)/1440.d0+sec/86400.d0)

    ENDIF

! End
    RETURN

  END SUBROUTINE st2tim_arg1


! -------------------------------------------------------------------------
! Subroutine with array time variable
! -------------------------------------------------------------------------

  SUBROUTINE st2tim_arg2(ifmt,ntim,tstrng,twin,nFrac)

    USE m_bern,   ONLY: i4b, r8b, lfnErr
    USE s_exitrc
    IMPLICIT NONE

! List of parameters
! ------------------
! input:
    INTEGER(i4b)                   :: ifmt   ! Format specification
    INTEGER(i4b)                   :: ntim   ! Number of arguments
    CHARACTER(LEN=*)               :: tstrng ! String containing the date(s)
    INTEGER(i4b), OPTIONAL         :: nFrac  ! Number of fractionals of a
                                             ! second in case of (ifmt==3)

! output
    REAL(r8b), DIMENSION(2)        :: twin   ! Two epochs

! Local variables
! ---------------
    INTEGER(i4b)                   :: itim, ich1, ich2, nHelp

! Init output values
! ------------------
    twin = (/ 0d0, 1d20 /)

! String blank means tmjd=0.D0 and tmjd=1.D20
! -------------------------------------------
    IF (tstrng /= '') THEN

! Make some checks
! ----------------
      IF (ifmt == 1 .OR. ifmt == 2) THEN
        IF (LEN(tstrng) < 40) THEN
          WRITE(lfnErr,'(/,A,A,2(/,16X,A,I4),/)') ' *** SR ST2TIM: ', &
                'The input string is too short.',                     &
                'Current  length of the string: ', LEN(tstrng),       &
                'Required length of the string: ', 40
          CALL exitrc(2)
        ENDIF
      ELSEIF (ifmt == 3.AND.PRESENT(nFrac)) THEN
        IF (LEN(tstrng) < 2*(20+nFrac)+2) THEN
          WRITE(lfnErr,'(/,A,A,2(/,16X,A,I4),/)') ' *** SR ST2TIM: ', &
                'The input string is too short:',                     &
                'Current  length of the string: ', LEN(tstrng),       &
                'Required length of the string: ', 2*(20+nFrac)+2
          CALL exitrc(2)
        ENDIF
      ELSEIF (ifmt /= 3) THEN
        WRITE(lfnErr,'(/,A,A,I4,/,16X,A,/)')      ' *** SR ST2TIM: ', &
              'Unknown format identified: ',ifmt,                     &
              '(only values between 1 and 3 are supported)'
        CALL exitrc(2)
      ENDIF

! Read two epochs
! ---------------
      DO itim = 1,2
        IF (ifmt == 1 .OR. ifmt == 2) THEN
          ich1=(itim-1)*21+1
          ich2=(itim-1)*21+19
          IF (tstrng(ich1:ich2) /= '') &
            CALL st2tim_arg1(ifmt,1,tstrng(ich1:ich2),twin(itim))
        ELSEIF (ifmt == 3 .AND. PRESENT(nFrac)) THEN
          ich1=(itim-1)*(22+nFrac)+1
          ich2=(itim-1)*(22+nFrac)+20+nFrac
          IF (tstrng(ich1:ich2) /= '') &
            CALL st2tim_arg1(ifmt,1,tstrng(ich1:ich2),twin(itim),nFrac)
        ELSEIF (ifmt == 3) THEN
          nHelp = LEN(tstrng)/2-1
          ich1=(itim-1)*(nHelp+1)+1
          ich2=(itim-1)*(nHelp+2)+nHelp
          IF (tstrng(ich1:ich2) /= '') &
            CALL st2tim_arg1(ifmt,1,tstrng(ich1:ich2),twin(itim))
        ENDIF
      ENDDO

! Check if t2 > t1
! ----------------
      IF(twin(2) .LE. twin(1)) THEN
        WRITE(lfnErr,'(/,A,/,16X,A,/)') &
          ' ### SR ST2TIM: Illegal time interval:', &
          tstrng
!!!        CALL exitrc(2)
      ENDIF

    ENDIF

    RETURN

  END SUBROUTINE st2tim_arg2

END MODULE
