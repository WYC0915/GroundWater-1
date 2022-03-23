MODULE s_RDSTAX
PRIVATE :: rdstax100, rdstax101
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdstax(filename, stacrux, title)

! -------------------------------------------------------------------------
! Purpose:    Reads station crux file for RXOBV3 (inconsistencies in RINEX
!             header wrt. the station info file which are accepted without
!             warning/error)
!
! Author:     R. Dach
!
! Created:    13-May-2003
! Last mod.:  27-Oct-2010
!
! Changes:    19-Jul-2010 SL: tab characters removed
!             21-Sep-2010 RD: ST2TIM can be used as a module now
!             27-Oct-2010 SL: use m_bern with ONLY, consider STAINFO vers 1.01
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnLoc, lfnErr
  USE d_stacrx, ONLY: t_stacrux

  USE s_opnfil
  USE s_opnerr
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)             :: filename

! output:
  TYPE(t_stacrux)              :: stacrux   ! Only section 2 is used
  CHARACTER(LEN=*),OPTIONAL    :: title

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER  :: srName = 'RDSTAX'

! Local Variables
! ---------------
  CHARACTER(LEN=206)           :: line
  INTEGER(i4b)                 :: ios
  REAL(r8b)                    :: version

! Initializations
! ---------------
  version = 0d0

! Read the STACRX file
! --------------------
  CALL opnfil(lfnLoc,filename,'OLD','FORMATTED','READONLY',' ',ios)
  CALL opnerr(lfnErr,lfnLoc,ios,filename,srName)

  DO
    READ(lfnloc,'(A)',END=999) line
    IF(line(1:15) == 'FORMAT VERSION:') THEN
      IF(line(17:20) == '1.00') THEN
        version = 1.00d0
        EXIT
      ELSEIF(line(17:20) == '1.01') THEN
        version = 1.01d0
        EXIT
      ENDIF
    ELSEIF(line(1:12) == 'STATION NAME') THEN
      EXIT
    ENDIF
  ENDDO

  CLOSE(lfnloc)

! Call appropriate rdstax subroutine
! ----------------------------------
  IF(version <= 1.00d0) THEN
    IF(PRESENT(title)) THEN
      CALL rdstax100(filename,stacrux,title)
    ELSE
      CALL rdstax100(filename,stacrux)
    ENDIF
  ELSEIF(version == 1.01d0) THEN
    IF(PRESENT(title)) THEN
      CALL rdstax101(filename,stacrux,title)
    ELSE
      CALL rdstax101(filename,stacrux)
    ENDIF
  ENDIF
  stacrux%stainfo%cdpsod = ''

  RETURN
  999 CONTINUE
  WRITE(lfnerr,*) ' *** SR RDSTAX: Error reading STACRX file : ',filename
  CALL exitrc(2)

END SUBROUTINE rdstax

! ------------------------------------------------------------------------

SUBROUTINE rdstax100(filename,stacrux,title)

! -------------------------------------------------------------------------

! Changes:    27-Oct-2010 SL: use m_bern with ONLY, use undef_c

! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, lfnLoc, lfnErr
  USE d_stacrx, ONLY: t_stacrux, undef_c
  USE s_alcerr
  USE s_opnfil
  USE f_lincount
  USE s_st2tim
  USE s_opnerr
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)             :: filename

! output:
  TYPE(t_stacrux)              :: stacrux   ! Only section 2 is used
  CHARACTER(LEN=*),OPTIONAL    :: title

! Local Parameters
! ----------------
  CHARACTER(LEN=9), PARAMETER  :: srName = 'RDSTAX100'

! Local Variables
! ---------------
  CHARACTER(LEN=206)           :: line
  CHARACTER(LEN=40)            :: datstr
  INTEGER(i4b)                 :: nlin
  INTEGER(i4b)                 :: icrx
  INTEGER(i4b)                 :: ios
  INTEGER(i4b)                 :: iac

! Get the size of the file
! ------------------------
  nLin = linCount(filename,5)

! Init and allocate arrays
! ------------------------
  staCrux%nrenam   = 0
  NULLIFY(staCrux%renamSta)

  staCrux%ninfo    = 0
  ALLOCATE(staCrux%staInfo(nLin),stat=iac)
  CALL alcerr(iac,'staCrux%staInfo',(/nLin/),srName)

  staCrux%nprob    = 0
  NULLIFY(staCrux%staProb)

  staCrux%ncoovel  = 0
  NULLIFY(staCrux%coovel)

  staCrux%nstatype = 0
  NULLIFY(staCrux%statype)

! Open the STACRUX file
! ---------------------
  CALL opnfil(lfnloc,filename,'OLD','FORMATTED','READONLY',' ',ios)
  CALL opnerr(lfnerr,lfnloc,ios,filename,srName)

! Read the title
! --------------
  READ(lfnloc,'(A,////)',iostat=ios) line
  IF (PRESENT(title) .AND. ios == 0) title = line

! Read the data records
! ---------------------
  icrx = 0
  DO WHILE (ios == 0)

    READ (lfnloc,'(A)',iostat=ios) line

    IF (ios /= 0) CYCLE
    IF (LEN_TRIM(line) == 0) EXIT

    icrx = icrx + 1
    READ (line,                                                           &
          '( A16,4X,3X,A40,2X,2(A20,2X),2(I6,2X),3(F8.4,2X),2X,A)',     &
          iostat=ios)                                                     &
          stacrux%stainfo(icrx)%stanam,   datstr,                         &
          stacrux%stainfo(icrx)%recnam,   stacrux%stainfo(icrx)%antnam,   &
          stacrux%stainfo(icrx)%recnum,   stacrux%stainfo(icrx)%antnum,   &
          stacrux%stainfo(icrx)%antecc(1),stacrux%stainfo(icrx)%antecc(2),&
          stacrux%stainfo(icrx)%antecc(3),stacrux%stainfo(icrx)%remark

    CALL st2tim(1, 2, datstr, stacrux%stainfo(icrx)%timint%t )

    stacrux%stainfo(icrx)%recser = undef_c
    stacrux%stainfo(icrx)%antser = undef_c

    IF (ios /= 0) icrx = icrx - 1

  END DO

  stacrux%nInfo = icrx

! Close file
! ----------
  CLOSE(lfnloc)

  RETURN

END SUBROUTINE rdstax100

! -------------------------------------------------------------------------

SUBROUTINE rdstax101(filename,stacrux,title)

! -------------------------------------------------------------------------

! Changes:    __-___-____ __:

! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, lfnLoc, lfnErr
  USE d_stacrx, ONLY: t_stacrux, undef_i
  USE s_alcerr
  USE s_opnfil
  USE f_lincount
  USE s_st2tim
  USE s_opnerr
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)             :: filename

! output:
  TYPE(t_stacrux)              :: stacrux   ! Only section 2 is used
  CHARACTER(LEN=*),OPTIONAL    :: title

! Local Parameters
! ----------------
  CHARACTER(LEN=9), PARAMETER  :: srName = 'RDSTAX101'

! Local Variables
! ---------------
  CHARACTER(LEN=206)           :: line
  CHARACTER(LEN=40)            :: datstr
  INTEGER(i4b)                 :: nlin
  INTEGER(i4b)                 :: icrx
  INTEGER(i4b)                 :: ios
  INTEGER(i4b)                 :: iac

! Get the size of the file
! ------------------------
  nLin = linCount(filename,5)

! Init and allocate arrays
! ------------------------
  staCrux%nrenam   = 0
  NULLIFY(staCrux%renamSta)

  staCrux%ninfo    = 0
  ALLOCATE(staCrux%staInfo(nLin),stat=iac)
  CALL alcerr(iac,'staCrux%staInfo',(/nLin/),srName)

  staCrux%nprob    = 0
  NULLIFY(staCrux%staProb)

  staCrux%ncoovel  = 0
  NULLIFY(staCrux%coovel)

  staCrux%nstatype = 0
  NULLIFY(staCrux%statype)

! Open the STACRUX file
! ---------------------
  CALL opnfil(lfnloc,filename,'OLD','FORMATTED','READONLY',' ',ios)
  CALL opnerr(lfnerr,lfnloc,ios,filename,srName)

! Read the title
! --------------
  READ(lfnloc,'(A,//////)',iostat=ios) line
  IF(PRESENT(title) .AND. ios == 0) title = line

! Read the data records
! ---------------------
  icrx = 0
  DO WHILE(ios == 0)

    READ(lfnloc,'(A)',iostat=ios) line
    IF(ios /= 0) CYCLE
    IF(LEN_TRIM(line) == 0) EXIT

    icrx = icrx + 1
    READ(line,'(A16,4X,2X,A40,2X,2(A20,2X,A20,2X),3(F8.4,2X),A)', &
         iostat=ios) &
      stacrux%stainfo(icrx)%stanam, &
      datstr, &
      stacrux%stainfo(icrx)%recnam, &
      stacrux%stainfo(icrx)%recser, &
      stacrux%stainfo(icrx)%antnam, &
      stacrux%stainfo(icrx)%antser, &
      stacrux%stainfo(icrx)%antecc(1), &
      stacrux%stainfo(icrx)%antecc(2), &
      stacrux%stainfo(icrx)%antecc(3), &
      stacrux%stainfo(icrx)%remark

    CALL st2tim(1,2,datstr,stacrux%stainfo(icrx)%timint%t)

    stacrux%stainfo(icrx)%recnum = undef_i
    stacrux%stainfo(icrx)%antnum = undef_i

    IF(ios /= 0) icrx = icrx - 1

  END DO

  stacrux%nInfo = icrx

! Close file
! ----------
  CLOSE(lfnloc)

  RETURN

END SUBROUTINE rdstax101

END MODULE
