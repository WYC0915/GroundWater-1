MODULE s_OTIDNAM
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE otidnam(tponam,otdnam)

! -------------------------------------------------------------------------
! Purpose:    Get ocean tide model name
!
! Author:     U. Hugentobler
!
! Created:    14-Jan-2005
! Last mod.:  08-Feb-2007
!
! Changes:    08-Feb-2007: HB: Get also model name for solid Earth tides
!
! SR used:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE s_opnfil
  USE s_gtflna
  USE s_opnerr
  USE f_nextline
  IMPLICIT NONE

! List of parameters
! ------------------
! OUT:
  CHARACTER(LEN=16)                   :: tponam     ! Solid Earth tide model name
  CHARACTER(LEN=16)                   :: otdnam     ! Ocean tide model name

! Local Variables
! ---------------
  INTEGER(i4b)                        :: irc,ios
  INTEGER(i4b)                        :: l1,l2,l3,l4
  CHARACTER(LEN=fileNameLength)       :: filnam
  CHARACTER(LEN=shortLineLength)      :: title,key,model
  CHARACTER(LEN=lineLength)           :: line

  tponam='NO'
  otdnam='NO'

! Solid Earth tides
! -----------------
  CALL gtflna(0,'SETIDES ',filnam,irc)
  IF (irc == 0) THEN
    CALL opnfil(lfnloc,filnam,'OLD','FORMATTED','READONLY',' ',ios)
    CALL opnerr(lfnerr,lfnloc,ios,filnam,'OTIDNAM')
    line = nextline(lfnloc,1)
    IF (line(1:22)=='SOLID EARTH TIDE MODEL') THEN
      READ(line(25:40),'(A16)')tponam
    ENDIF
    line = nextline(lfnloc,1)
    IF (line(1:10)=='ELASTICITY') THEN
      READ(line(25:28),'(A4)')tponam(13:16)
    ENDIF
    CLOSE(lfnloc)
  ENDIF

! Ocean tides
! -----------
  CALL gtflna(0,'OTIDES ',filnam,irc)
  IF (irc /= 0) RETURN

  l1 = INDEX(filnam, "/", BACK=.TRUE.)
  l2 = INDEX(filnam, backslash, BACK=.TRUE.)
  l3 = MAX(l1,l2)+1
  l4 = INDEX(filnam, ".", BACK=.TRUE.)-1
  IF (l4 <= l3) RETURN

! CSR3.0
  IF ( filnam(l3:l4) == 'OT_CSR30' ) THEN
      otdnam = 'CSR3.0'
  ELSE

! ICGEM format?
    CALL opnfil(lfnloc,filnam,'OLD','FORMATTED','READONLY',' ',ios)
    CALL opnerr(lfnerr,lfnloc,ios,filnam,'OTIDNAM')
    READ(lfnloc,'(A80)') title
    otdnam = title(3:18)

    l1=INDEX(title,'product_type')
    l2=INDEX(title,'ocean_tides')

    IF (l1 > 0 .AND. l2 > 0) THEN
      otdnam = 'ICGEM unknown'
      DO
        READ(lfnloc,*,IOSTAT=ios)key, model
        IF (ios /= 0) EXIT
        IF (key == 'modelname') THEN
          otdnam = 'ICGEM '//model
          EXIT
        ENDIF
      ENDDO
    ELSE
! Else: Use filename
      otdnam = filnam(l3:l4)
    ENDIF
    CLOSE (lfnloc)
  ENDIF

  RETURN

END SUBROUTINE otidnam

END MODULE
