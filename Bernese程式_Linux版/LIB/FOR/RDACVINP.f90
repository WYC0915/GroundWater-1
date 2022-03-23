! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE s_rdacvinp

! -------------------------------------------------------------------------
! Purpose:    Reads the input panel of ATXCNV
!
! Author:     A. Gaede
!
! Created:    09-Jul-2007
! Last mod:   08-Aug-2007
!
! Changes:    08-Aug-2007 AG: Debug option and ONLYGPS implemented
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------
! Used modules:
  USE m_bern
  USE s_exitrc
  USE s_readkeys
  USE s_ckoptc

! No implicits
  IMPLICIT NONE

! Local types
  TYPE t_opt
    INTEGER(i4b)   :: nozero
    INTEGER(i4b)   :: onlyele
    INTEGER(i4b)   :: onlygps
    INTEGER(i4b)   :: abs2rel
    INTEGER(i4b)   :: convert
    INTEGER(i4b)   :: radcod
    INTEGER(i4b)   :: antnum
    INTEGER(i4b)   :: atxfil
    INTEGER(i4b)   :: fizmod
    INTEGER(i4b)   :: finmod
    INTEGER(i4b)   :: fiindv
    INTEGER(i4b)   :: mxfnad
    INTEGER(i4b)   :: mxfzen
    INTEGER(i4b)   :: debug
  END TYPE t_opt

  TYPE(t_opt)        :: opt

CONTAINS

  SUBROUTINE rdacvinp

! Local variables
    CHARACTER(LEN=keyValueLength),DIMENSION(:),POINTER :: keyValue
    INTEGER(i4b)       ::irc

! Init variables
! --------------
    NULLIFY(keyValue)

    opt%nozero  = 0
    opt%onlyele = 0
    opt%abs2rel = 0
    opt%convert = 0
    opt%radcod  = 0
    opt%antnum  = 0
    opt%atxfil  = 0
    opt%fizmod  = 0
    opt%finmod  = 0
    opt%fiindv  = 0
    opt%mxfnad  = 0
    opt%mxfzen  = 0
    opt%debug  = 0

! Read option for writing zero patterns
    CALL readkeys('NOZERO', keyValue, irc)
    IF (irc == 0 .AND. keyValue(1) == '1') opt%nozero = 1

! Read option for type of pattern
    CALL readkeys('ONLYELE', keyValue, irc)
    IF (irc == 0 .AND. keyValue(1) == '1') opt%onlyele = 1

! Read option for type of pattern
    CALL readkeys('ONLYGPS', keyValue, irc)
    IF (irc == 0 .AND. keyValue(1) == '1') opt%onlygps = 1

! Read wether abs2rel or not
    CALL readkeys('ABS2REL', keyValue, irc)
    IF (irc == 0 .AND. keyValue(1) == '1') opt%abs2rel = 1

! Read whether conversion or not
    IF (opt%abs2rel == 0) THEN
      CALL readkeys('CONVERT', keyValue, irc)
      IF (irc == 0 .AND. keyValue(1) == '1') opt%convert = 1
    ENDIF

! Read whether consideration of antennas without radome code
    CALL readkeys('RADCOD', keyValue, irc)
    IF (irc == 0 .AND. keyValue(1) == '1') opt%radcod = 1

! Read option for numbering of general antennas (0 or 999999)
    CALL readkeys('ANTNUM', keyValue, irc)
    IF (irc == 0 .AND. keyValue(1) == '1') opt%antnum = 1

! Read information for ANTEX

    CALL readkeys('ATXFIL', keyValue, irc)
    IF (irc == 0 .AND. keyValue(1) == '1') opt%atxfil = 1

    IF (opt%atxfil == 1) THEN
      CALL readkeys('MXFZEN', keyValue, irc)
      IF (irc == 0) READ(keyValue(1), *, iostat=irc) opt%mxfzen
      IF (irc /= 0) THEN
        WRITE(lfnerr,"(/,' *** PG PHCCNV: Wrong maximum zenith angle', &
                     &   ' specified for',                             &
                     & /,16X,'filling of antenna patterns.',           &
                     & /,16X,'Specified value: ',A)") TRIM(keyValue(1))
        CALL exitrc(2)
      ENDIF

      CALL readKeys('FILREC', keyValue, irc)
      CALL ckoptc(1,'FILREC', keyValue, &
              (/'ZEROS     ','LAST_VALUE','AOAD/M_T  '/), &
              'rdacvinp', 'Fill missing rec', irc, irc, &
              valList=(/1,2,3/), result1=opt%fizmod)

      CALL readkeys('MXFNAD', keyValue, irc)
      IF (irc == 0) READ(keyValue(1), *, iostat=irc) opt%mxfnad
      IF (irc /= 0) THEN
        WRITE(LFNERR,"(/,' *** PG PHCCNV: Wrong maximum nadir angle', &
                     &   ' specified for',                            &
                     & /,16X,'filling of antenna patterns.',          &
                     & /,16X,'SPECIFIED VALUE: ',A)") TRIM(keyValue(1))
        CALL exitrc(2)
      ENDIF

      CALL readKeys('FILSAT', keyValue, irc)
      CALL ckoptc(1,'FILSAT', keyValue, &
              (/'ZEROS     ','LAST_VALUE'/), &
              'rdacvinp', 'Fill missing sat', irc, irc, &
              valList=(/1,2/), result1=opt%finmod)

      CALL readKeys('FILINDV', keyValue, irc)
      CALL ckoptc(1,'FILINDV', keyValue, &
              (/'GPS_VALUES  ','GROUP_VALUES','GROUP_DIFF  '/), &
              'rdacvinp', 'Fill missing rec', irc, irc, &
              valList=(/1,2,3/), result1=opt%fiindv)

    ENDIF

! Read debug option
    CALL readkeys('DEBUG', keyValue, irc)
    IF (irc == 0 .AND. keyValue(1) == '1') opt%debug = 1

  END SUBROUTINE rdacvinp

END MODULE s_rdacvinp
