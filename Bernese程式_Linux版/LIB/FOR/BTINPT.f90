MODULE s_BTINPT
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE btinpt(iorsys,cmcyn,dtrec,dtab,timwin)

! -------------------------------------------------------------------------
! Purpose:    This is a new version of the old subroutine BTINPT.f that
!             reads the input option of the program BRDTAB
!
! Author:     L. Mervart
!
! Created:    04-Jun-2000
! Last mod.:  06-Dec-2010
!
! Changes:    23-Apr-2003 CU: Nullify local pointers
!             18-Jul-2006 AG: CMC added
!             20-Sep-2008 RD: Add time window and sampling
!             16-Nov-2010 HB: Typos corrected
!             06-Dec-2010 RD: CMC for ATL added
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE m_time, ONLY: t_timint

  USE s_readkeys
  USE s_ckoptb
  USE s_ckoptc
  USE s_ckopti
  USE s_gttimwin
  USE s_priwin
  USE s_gtflna
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
  INTEGER(i4b)          :: iorsys  ! reference system (1: B1950.0, 2: J2000.0)
  LOGICAL, DIMENSION(2) :: cmcyn   ! 1: OTL, 2: ATL
  REAL(r8b)             :: dtrec   ! sampling of the navigation messages
                                   ! (1d20: take the smallest T0)
  REAL(r8b)             :: dtab    ! sampling of the tabular records
  TYPE(t_timint)        :: timwin  ! Time interval of tabular records

! Local Parameter
! ---------------
  CHARACTER(LEN=6), PARAMETER    :: srName = 'BTINPT'
  CHARACTER(LEN=5), DIMENSION(2), PARAMETER :: orbsys = (/ 'B1950','J2000' /)

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue
  CHARACTER(LEN=filenameLength)                        :: filnam

  INTEGER(i4b)                                         :: itab
  INTEGER(i4b)                                         :: irCode
  INTEGER(i4b)                                         :: irc


! Init some variables
! -------------------
  irCode = 0
  NULLIFY(keyValue)

! Read the refernce frame
! -----------------------
  CALL readkeys('SYSTEM', keyValue, irc)

  CALL ckoptc(1,'SYSTEM', keyValue, orbsys, srName,                        &
              'Reference system',irc,irCode,                               &
              result1=iorsys)


! Set flag for applying CMC
! -------------------------
  cmcyn = .FALSE.
  CALL gtflna(0,'OCNLOAD',filnam,irc)
  IF (irc == 0 .AND. LEN_TRIM(filnam) > 0) THEN
    CALL ckoptb(1,(/'CMCYN_O'/),srName,                                    &
                'Apply OTL CMC correction',irc,                            &
                resultL=cmcyn(1))
  ENDIF

  CALL gtflna(0,'ATMLOAD',filnam,irc)
  IF (irc == 0 .AND. LEN_TRIM(filnam) > 0) THEN
    CALL ckoptb(1,(/'CMCYN_A'/),srName,                                    &
                'Apply ATL CMC correction',irc,                            &
                resultL=cmcyn(2))
  ENDIF

! Read the sampling of the navigation messages
! --------------------------------------------
  CALL readkeys('DTREC', keyValue, irc)

  CALL ckopti(1,'DTREC', keyValue, srName,                                &
              'Sampling of the navigation messages',irc,irCode,           &
              empty=0,ge=0,result1=itab)

  dtrec = DBLE(itab)
  IF (itab == 0) dtrec = 1d20


! Read the sampling interval
! --------------------------
  CALL readkeys('DTAB', keyValue, irc)

  CALL ckopti(1,'DTAB', keyValue, srName,                                 &
              'Sampling of the tabular records',irc,irCode,               &
              empty=0,ge=0,result1=itab)

  IF (itab == 0) itab = 900
  dtab = DBLE(itab)


! Get the time window
! -------------------
  CALL gtTimWin(' ',(/'RADIO_0','RADIO_1','RADIO_2'/),           &
                (/'SESSION_YEAR','SESSION_STRG'/),               &
                (/'STADAT', 'STATIM', 'ENDDAT', 'ENDTIM'/),      &
                timwin%t)

  DEALLOCATE(keyValue,stat=irc)
  IF (irCode /= 0) CALL exitrc(2)

!
! WRITE TITLE AND OPTIONS
! -----------------------
  WRITE(LFNPRT,'(//,A,/,A)')                                     &
    ' CREATING TABULAR ORBITS FROM BROADCAST EPHEMERIS FILES:',  &
    '   System:   ' // ORBSYS(IORSYS)
  IF (cmcyn(1)) THEN
    WRITE(LFNPRT,'(A)') &
    '   Ocean tidal loading CMC corrections applied'
  ELSE
    WRITE(LFNPRT,'(A)') &
    '   Ocean tidal loading CMC corrections not applied'
  ENDIF
  IF (cmcyn(2)) THEN
    WRITE(LFNPRT,'(A)') &
    '   Atmospheric tidal loading CMC corrections applied'
  ELSE
    WRITE(LFNPRT,'(A)') &
    '   Atmospheric tidal loading CMC corrections not applied'
  ENDIF
  IF (DTREC == 1D20) THEN
    WRITE(LFNPRT,'(A)') &
    '   Sampling of the navigation records: automatically detected'
  ELSE
    WRITE(LFNPRT,'(A,I5,A)') &
    '   Sampling of the navigation records:',NINT(DTREC),' seconds'
  ENDIF
  WRITE(LFNPRT,'(A,I5,A,/)') &
    '   Sampling of tabular records:       ',NINT(DTAB),' seconds'

  CALL priwin(0,timwin%t)
END SUBROUTINE btinpt

END MODULE
