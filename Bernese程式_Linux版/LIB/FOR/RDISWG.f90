MODULE s_RDISWG
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdiswg(maxwgt, sigapr, globalWindow, nwgt, satwgt, wgtwgt, timwgt)

! -------------------------------------------------------------------------
! Purpose:    Reads the satellite specific weighting for GPSEST
!
! Author:     R. Dach
!
! Created:    29-Jun-2001
! Last mod.:  08-Aug-2005
!
! Changes:    27-Mar-2003  RD: New data type for globalWindow
!             01-Apr-2003  HU: Comment in DIMTST adapted
!             23-Apr-2003  RD: Nullify local pointers
!             08-Aug-2005  HB: Use new SR TIMST2 (module)
!
! SR used:    dimtst, djul, exitrc, readKeys, timst2
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_time,   ONLY: t_timint
  USE s_dimtst
  USE s_readkeys
  USE f_djul
  USE s_exitrc
  USE s_timst2
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)               :: maxwgt       ! max. number of satellite
                                             ! specific sigmas
  REAL(r8b)                  :: sigapr       ! a priori sigma
  TYPE(t_timint)             :: globalWindow ! window to be processed
                                             ! (from - to, MJD)

! output:
  INTEGER(i4b)               :: nwgt         ! number of intervals with
                                             ! weighted satellites
  INTEGER(i4b), DIMENSION(*) :: satwgt       ! numbers of weighted satellites
  REAL(r8b), DIMENSION(2,*)  :: timwgt       ! time intervals with weighted
                                             ! satellites (MJD)
  REAL(r8b), DIMENSION(*)    :: wgtwgt       ! satellite specific sigma

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), &
         DIMENSION(:), POINTER  :: keyValue
  CHARACTER(LEN=keyValueLength), &
         DIMENSION(6)           :: hlpStr
  CHARACTER(LEN=40)             :: epost1,epost2

  INTEGER(i4b)                  :: yy1, mon1, dd1
  INTEGER(i4b)                  :: hh1, min1, ss1
  INTEGER(i4b)                  :: irc, ios, iac
  INTEGER(i4b)                  :: ii, jj

  REAL(r8b)                     :: day1
  REAL(r8b)                     :: rHlp

! Init the variables
! ------------------
  nwgt = 0

  NULLIFY(keyValue)

! Get the string of requests
! --------------------------
  CALL readKeys('SVNSIG', keyValue, irc)
  nwgt = SIZE(keyValue)

! Test the dimension of the arrays
! --------------------------------
  CALL dimtst(1,1,2,'rdiswg','maxwgt',                          &
              'satellite specific sigma',                       &
              'Parameter is defined in module "P_GPSEST.f90".', &
              nwgt,maxwgt,irc)

! Extract the request strings
! ---------------------------
  DO ii = 1, SIZE(keyValue)
    READ(keyValue(ii),*,iostat=irc) (hlpStr(jj),jj=1,6)
    IF (irc == 0) THEN

! Satellite number
! ----------------
      READ(hlpStr(1),*,iostat=ios) satwgt(ii)
      IF (ios /= 0 .OR. satwgt(ii) <= 0) THEN
        WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/,16X,A,A,/)')               &
        ' *** SR RDISWG: Wrong specification for the satellite number in', &
                        'satellite specific weighting!',                   &
                        'Record number:   ', ii,                           &
                        'Specified value: ', TRIM(hlpStr(1))
        nwgt = nwgt - 1
      ENDIF

! Read the start date
! -------------------
      READ(hlpStr(2),*,iostat=ios) yy1, mon1, dd1
      IF (ios == 0) READ(hlpStr(3),*,iostat=ios) hh1, min1, ss1

      IF (ios == 0) THEN
        day1 = 1d0*dd1 + (( ss1 / 60.0 + min1) / 60.0 + hh1) / 24.0
        timwgt(1,ii) = djul(yy1,mon1,day1)
      ENDIF

      IF (ios /= 0 .OR. timwgt(1,ii) < 1000d0) THEN
        WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/,16X,A,A,/)')               &
        ' *** SR RDISWG: Wrong specification of the start epoch in',       &
                        'satellite specific weighting!',                   &
                        'Record number:   ', ii,                           &
                        'Specified value: ',                               &
                                 TRIM(hlpStr(2)) // '  ' // TRIM(hlpStr(3))
        nWgt = nWgt - 1
      ENDIF

! Read the end date
! -----------------
      READ(hlpStr(4),*,iostat=ios) yy1, mon1, dd1
      IF (ios == 0) READ(hlpStr(5),*,iostat=ios) hh1, min1, ss1

      IF (ios == 0) THEN
        day1 = 1d0*dd1 + (( ss1 / 60.0 + min1) / 60.0 + hh1) / 24.0
        timwgt(2,ii) = djul(yy1,mon1,day1)
      ENDIF

      IF (ios /= 0 .OR. timwgt(2,ii) < 1000d0) THEN
        WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/,16X,A,A,/)')               &
        ' *** SR RDISWG: Wrong specification of the end epoch in',         &
                        'satellite specific weighting!',                   &
                        'Record number:   ', ii,                           &
                        'Specified value: ',                               &
                                 TRIM(hlpStr(4)) // '  ' // TRIM(hlpStr(5))
        nWgt = nWgt - 1
      ENDIF

! Check the start/end epochs
! --------------------------
      IF (timwgt(1,ii) > timwgt(2,ii)) THEN
        CALL timst2(1,1,timwgt(1,ii),epost1)
        CALL timst2(1,1,timwgt(2,ii),epost2)
        WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,2(/,16X,A),/)')              &
        ' *** SR RDISWG: Wrong epoch specification of the ',               &
                        'Satellite specific weighting!',                   &
                        'Record number:   ', ii,                           &
                        'Start epoch: '//TRIM(epost1),                     &
                        'End epoch:   '//TRIM(epost2)
        nWgt = nWgt - 1

      ELSE IF (timwgt(1,ii) > globalWindow%t(2) .OR. &
               timwgt(2,ii) < globalWindow%t(1)) THEN
        CALL timst2(1,2,timwgt(1:2,ii),epost1)
        CALL timst2(1,2,globalWindow%t,  epost2)
        WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,2(/,16X,A),/)')                  &
        ' ### SR RDISWG: The epoch specification of the satellite specific ',  &
                        'weighting is outside from the observation interval!', &
                        'Record number:   ', ii,                               &
                        'Specified epoch: '//TRIM(epost1),                     &
                        'Observation:     '//TRIM(epost2)
      ENDIF

! Satellite weight
! ----------------
      READ(hlpStr(6),*,iostat=ios) rHlp
      IF (ios /= 0 .OR. rHlp <= 0D0) THEN
        WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/,16X,A,A,/)')               &
        ' *** SR RDISWG: Wrong specification for the satellite sigma in',  &
                        'satellite specific weighting!',                   &
                        'Record number:   ', ii,                           &
                        'Specified value: ', TRIM(hlpStr(6))
        nwgt = nwgt - 1
      ELSE
        wgtwgt(ii) = (sigapr/rHlp)**2
      ENDIF

! Record was not readable
! -----------------------
    ELSE
      WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I5,/)')                         &
      ' *** SR RDISWG: Wrong specification for the satellite specific ', &
                      'weighting! The record connot be extracted.',      &
                      'Record number:   ', ii
      nwgt = nwgt - 1
    ENDIF
  ENDDO
  IF (nWgt /= SIZE(keyValue)) CALL exitrc(2)

! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=iac)


  RETURN
END SUBROUTINE rdiswg

END MODULE
