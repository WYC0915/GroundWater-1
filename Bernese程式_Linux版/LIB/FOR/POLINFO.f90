MODULE s_polinfo
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE polinfo(filnam,filpol,title,iform, &
                     nutnam,subnam,tfirst,tlast,tstep,tpred)

! -------------------------------------------------------------------------
! Purpose:    Read pole information
!
! Author:     U. Hugentobler
!
! Created:    20-Jun-2006
! Last mod.:  __-___-____
!
! Changes:    __-___-____ __:
!
! SR used:    gtflna, opnfil, opnerr, rdpolh, rdpoli
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE s_gtflna
  USE s_opnfil
  USE s_opnerr
  USE s_rdpolh
  USE s_rdpoli
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
! Input
  CHARACTER(LEN=*)                :: filnam   ! Input file name
                                              ! blank: use GTFLNA('POLE')
! Output:
  CHARACTER(LEN=fileNameLength80) :: filpol   ! Filename
  CHARACTER(LEN=80)               :: title    ! File title
  INTEGER(i4b)                    :: iform    ! Bernese format
                                              ! 0: unknown, 1: new, 2: old
  CHARACTER(LEN=16)               :: nutnam   ! Nutation model
  CHARACTER(LEN=16)               :: subnam   ! Subdaily model
  REAL(r8b)                       :: tfirst   ! First epoch in file
  REAL(r8b)                       :: tlast    ! Last epoch in file
  REAL(r8b)                       :: tstep    ! Step size (days)
  REAL(r8b)                       :: tpred    ! First predicted epoch
                                              ! (first epoch with rem='AE',
                                              !  for Bulletin A) or 0D0


! Local variables
! ---------------
  INTEGER(i4b)                    :: irc, iostat
  INTEGER(i4b)                    :: iend
  INTEGER(i4b),DIMENSION(2)       :: poltyp

  REAL(r8b)                       :: poltim, told, gpsutc
  REAL(r8b),DIMENSION(5)          :: polcoo, rmspol

  CHARACTER(LEN=3)                :: rem


! Open pole file
  IF (filnam == ' ') THEN
    CALL gtflna(1,'POLE   ',filpol,irc)
  ELSE
    filpol = filnam
  ENDIF
  CALL opnfil(lfnloc,FILPOL,'OLD',' ','READONLY',' ',iostat)
  CALL opnerr(lfnerr,lfnloc,iostat,filpol,'POLINFO')

! Read header and first parameter set of pole file
  CALL rdpolh(lfnloc,1,title,poltyp,iform,iend,nutnam,subnam)

! Read records
  tstep=1D20
  told =0D0
  tpred=0D0
  DO
    CALL rdpoli(lfnloc,poltim,polcoo,gpsutc,rem,rmspol,iform,iend)
    IF (iend == 2) CALL exitrc(2)
    IF (iend == 1) EXIT
    tlast = poltim
    IF (told == 0D0) THEN
      tfirst = poltim
    ELSE
      IF (DABS(poltim-told) < tstep) tstep = DABS(poltim-told)
    ENDIF
    told = poltim
    IF ((rem == 'AE ' .OR. rem == 'EXT') .AND. tpred == 0D0) tpred=poltim
  ENDDO
  CLOSE (lfnloc)

  RETURN
END SUBROUTINE polinfo

END MODULE
