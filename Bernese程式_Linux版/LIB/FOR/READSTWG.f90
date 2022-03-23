MODULE s_READSTWG
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE readStwg(sosFil,staWgt)

! -------------------------------------------------------------------------
! Purpose:    Read the content of Bernese Station Observation Weight file
!
! Author:     R. Dach
!
! Created:    10-Jun-2002
! Last mod.:  21-Sep-2010
!
! Changes:    08-Mar-2003 HU: Interface for sr alcerr used
!             21-Sep-2010 RD: ST2TIM can be used as a module now
!
! SR called:  alcerr, exitrc, opnfil, opnerr, linCount
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_global, ONLY: g_meaStr
  USE d_stawgt, ONLY: t_staWgt

  USE s_alcerr
  USE s_opnfil
  USE f_lincount
  USE s_opnerr
  USE s_st2tim
  USE s_exitrc
  USE s_upperc
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)              :: sosFil            ! Name of the file

! output:
  TYPE(t_staWgt)                :: staWgt            ! Station weight structure

! Local Parameters
! ----------------
  CHARACTER(LEN=8),PARAMETER    :: srName = 'readStwg'

! Local Variables
! ---------------
  CHARACTER(LEN=timStrgLength2) :: wgtTim
  CHARACTER(LEN=5)              :: meaWgt

  INTEGER(i4b)                  :: nWgt
  INTEGER(i4b)                  :: ii
  INTEGER(i4b)                  :: ios,iac


! Init the data records
! ---------------------
  staWgt%title      = ' '
  staWgt%nWgt       = 0

! Nothing to do
! -------------
  IF (LEN_TRIM(sosfil) == 0) RETURN

! Allocate the data record
! ------------------------
  nWgt = linCount(sosfil,5)

  DEALLOCATE(staWgt%wgt,stat=iac)

  ALLOCATE(staWgt%wgt(nWgt),stat=iac)
  CALL alcerr(iac,'staWgt%wgt',(/nWgt/),srName)

  staWgt%nWgt = nWgt

  staWgt%wgt(:)%staNam      = ' '
  staWgt%wgt(:)%weight      = 1d0
  staWgt%wgt(:)%meaTyp      =   0
  staWgt%wgt(:)%timwin%t(1) = 0d0
  staWgt%wgt(:)%timwin%t(2) = 1d20

! Open the file for reading
! -------------------------
  CALL opnfil(lfnloc,sosfil,'OLD','FORMATTED','READONLY',' ',ios)
  CALL opnerr(lfnerr,lfnloc,ios,sosfil,srName)

! Read the header of the file
! ---------------------------
  READ(lfnloc,'(A,////)',iostat=ios) staWgt%title

  IF (ios /= 0) THEN
    WRITE(lfnerr,'(/,A,/,18X,A,/)')                                      &
    ' *** SR READSTWG: Error reading header of the station weight file', &
                      'File name:  ' // TRIM(sosfil)
    CALL exitrc(2)
  ENDIF

  nWgt = 0
  DO WHILE (nWgt < staWgt%nWgt)

! Read the next data record
    nWgt = nWgt+1
    READ(lfnloc,'(A16,4X,2X,A5,2X,F9.3,2X,A)',iostat=ios)           &
         staWgt%wgt(nWgt)%staNam, meaWgt,                           &
         staWgt%wgt(nWgt)%weight, wgtTim

! Empty station name (end of file)
    IF (LEN_TRIM(staWgt%wgt(nWgt)%staNam) == 0) THEN
      staWgt%nWgt = nWgt-1
      EXIT
    ENDIF

! Error reading data record
    IF (ios /= 0) THEN
      WRITE(lfnerr,'(/,A,/,17X,A,/,17X,A,I6,/)')                            &
      ' *** SR READSTWG: Error reading records of the station weight file', &
                        'File name:  ' // TRIM(sosfil),                     &
                        'Line number:' ,  nWgt
      CALL exitrc(2)
    ENDIF

! Extract time string
    CALL st2tim(1,2,wgtTim,staWgt%wgt(nWgt)%timwin%t)

! Extract measurement type
    CALL upperc(meaWgt)

    staWgt%wgt(nWgt)%meaTyp = 0
    DO ii = 1,SIZE(g_meaStr)
      IF (meaWgt == g_meaStr(ii)) staWgt%wgt(nWgt)%meaTyp = ii
    ENDDO

    IF (staWgt%wgt(nWgt)%meaTyp == 0) THEN
      WRITE(lfnerr,'(/,A,/,18X,A,2(/,18X,A,A),/)')                             &
      ' *** SR READSTWG: Unknown measurement type found in the station weight',&
              'data record (Valid values: PHASE, CODE, RANGE):',               &
              'Station name:     ',staWgt%wgt(nWgt)%staNam,                    &
              'Measurement type: ',meaWgt
      CALL exitrc(2)
    ENDIF

  ENDDO

  CLOSE(lfnloc)

  RETURN
END SUBROUTINE readStwg

END MODULE
