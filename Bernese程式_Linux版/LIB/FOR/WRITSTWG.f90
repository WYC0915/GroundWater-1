MODULE s_WRITSTWG
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE writStwg(sosFil,staWgt)

! -------------------------------------------------------------------------
! Purpose:    Write the content of Bernese Station Observation Weight file
!
! Author:     R. Dach
!
! Created:    10-Jun-2002
! Last mod.:  08-Aug-2005
!
! Changes:    08-Aug-2005 HB: Use new SR TIMST2 (module)
!                             Modify call of TIMST2 =>
!                             staWgt%wgt(iWgt)%timwin%t
!
! SR called:  opnfil, opnerr, timst2
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

  USE s_opnfil
  USE s_exitrc
  USE s_opnerr
  USE s_timst2
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)              :: sosFil            ! Name of the file
  TYPE(t_staWgt)                :: staWgt            ! Station weight structure

! output:

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=8),PARAMETER    :: srName = 'writStwg'

! Local Variables
! ---------------
  CHARACTER(LEN=timStrgLength2) :: wgtTim
  INTEGER(i4b)                  :: iWgt
  INTEGER(i4b)                  :: ios


! Nothing to do
! -------------
  IF (LEN_TRIM(sosfil) == 0) RETURN

! Open the file for writing
! -------------------------
  CALL opnfil(lfnloc,sosfil,'UNKNOWN','FORMATTED',' ',' ',ios)
  CALL opnerr(lfnerr,lfnloc,ios,sosfil,srName)

! Write the header of the file
! ----------------------------
  WRITE(lfnloc,'(4(A,/),A)')                              &
          TRIM(staWgt%title),                             &
          '----------------------------------------' //   &
          '----------------------------------------',     &
          '                                        ' //   &
          'VALID:  from                  to',             &
          'STATION NAME          MEATYP  SIG-FACT  ' //   &
          'yyyy mm dd hh mm ss  yyyy mm dd hh mm ss',     &
          '****************      *****   ****.***  ' //   &
          '**** ** ** ** ** **  **** ** ** ** ** **'

! Write the station obs. weighting records
! ----------------------------------------
  DO iWgt = 1,staWgt%nWgt

    IF (staWgt%wgt(iWgt)%meaTyp < 1 .OR. &
        staWgt%wgt(iWgt)%meaTyp > SIZE(g_meaStr)) THEN
      WRITE(lfnerr,'(/,A,/,18X,A,/,18X,A,A,/,18X,A,I4,/)')                     &
      ' *** SR WRITSTWG: Unknown measurement type found in the station weight',&
              'data record (Valid values: 1: Phase, 2: Code, 3: Range):',      &
              'Station name:     ',staWgt%wgt(iWgt)%staNam,                    &
              'Measurement type: ',staWgt%wgt(iWgt)%meaTyp
      CALL exitrc(2)
    ENDIF

    CALL timst2(2,2,staWgt%wgt(iWgt)%timwin%t,wgtTim)

    IF (staWgt%wgt(iWgt)%weight > 1D6) THEN
      WRITE(lfnloc,'(A16,4X,2X,A5,2X,E9.3,2X,A)')                       &
           staWgt%wgt(iWgt)%staNam, g_meaStr(staWgt%wgt(iWgt)%meaTyp),  &
           staWgt%wgt(iWgt)%weight, TRIM(wgtTim)
    ELSE
      WRITE(lfnloc,'(A16,4X,2X,A5,2X,F9.3,2X,A)')                       &
           staWgt%wgt(iWgt)%staNam, g_meaStr(staWgt%wgt(iWgt)%meaTyp),  &
           staWgt%wgt(iWgt)%weight, TRIM(wgtTim)
    ENDIF
  ENDDO

  CLOSE(lfnloc)

  RETURN
END SUBROUTINE writStwg

END MODULE
