MODULE s_ISBSAV
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.0
! -------------------------------------------------------------------------

SUBROUTINE isbsav(title,npar,locq,xxx,anor,rms,timisb,stname,iCarr)

! -------------------------------------------------------------------------
! Purpose:    Save ISB results computed by GPSEST
!
! Author:     R. Dach
!
! Created:    21-Nov-2009
! Last mod.:  21-Nov-2009
!
! Changes:
!
! Copyright:  Astronomical Institute
!              University of Berne
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE m_time,   ONLY: t_timint
  USE d_isbFil, ONLY: t_isbFil, writeIsb, getisb
  USE d_const,  ONLY: C

  USE f_ikf
  USE s_alcerr
  USE s_gtflna
  IMPLICIT NONE

! List of Parameters
! ------------------
  CHARACTER(LEN=*)                                :: title
  INTEGER(i4b)                                    :: nPar
  INTEGER(i4b),     DIMENSION(:,:)                :: locq
  REAL(r8b),        DIMENSION(:)                  :: xxx
  REAL(r8b),        DIMENSION(:)                  :: anor
  REAL(r8b)                                       :: rms
  REAL(r8b),        DIMENSION(3,*)                :: timisb
  CHARACTER(LEN=*), DIMENSION(:)                  :: stname
  INTEGER(i4b),     DIMENSION(:,:)                :: iCarr

! Local parameter
! ---------------
  CHARACTER(LEN=6), PARAMETER                     :: srName = 'isbsav'

! Local Variables
! ---------------
  TYPE(t_isbFil)                                  :: isbOut

  CHARACTER(LEN=fileNameLength)                   :: filnam
  CHARACTER(LEN=20)                               :: oldSta

  INTEGER(i4b)                                    :: iac,irc
  INTEGER(i4b)                                    :: iPar
  INTEGER(i4b)                                    :: iSta,jSta
  INTEGER(i4b)                                    :: iIsb,nIsb
  INTEGER(I4b)                                    :: numIsb, numSta



! Store the results
! -----------------
  CALL gtflna(0,'ISBOUT',filnam,irc)
  IF (filnam == '') RETURN

! Count number of stations and parameter per station
! --------------------------------------------------
  oldSta = ''
  numSta = 1
  numIsb = 0
  nIsb   = 0

  DO iPar = 1,nPar

    IF ( locq(1,iPar) == 2 .AND. locq(6,iPar) == 5 ) THEN
      IF (oldSta == '') oldSta = stName(locq(2,iPar))
      IF (oldSta == stName(locq(2,iPar))) THEN
        nIsb = nIsb + 1
        IF (nIsb > numIsb) numIsb = nIsb
      ELSE
        numSta = numSta + 1
        oldSta = stName(locq(2,iPar))
        nIsb = 0
      ENDIF
    ENDIF
  ENDDO

! Allocate the structure to store the results
! -------------------------------------------
  ALLOCATE(isbOut%isbSta(numSta),stat=iac)
  CALL alcerr(iac,'isbFil%isbSta',(/numSta/),srName)

  isbOut%filnam = filnam
  isbOut%title  = title
  isbOut%nSta   = 0

! Copy the results into the structure
! -----------------------------------
  DO iPar = 1,nPar
    IF (locq(1,iPar) /= 2 .OR. locq(6,iPar) /= 5) CYCLE

    ! Find the station
    jSta = 0
    DO iSta = 1,isbOut%nSta
      IF (isbOut%isbSta(iSta)%staNam == stname(locq(2,ipar))) THEN
        jSta = iSta
        EXIT
      ENDIF
    ENDDO

    ! A new station
    IF (jSta == 0) THEN
      isbOut%nSta = isbOut%nSta + 1
      jSta = isbOut%nSta
      ALLOCATE(isbOut%isbSta(jSta)%isbRec(numIsb),stat=iac)
      CALL alcerr(iac,'isbOut%isbSta(jSta)%isbRec',(/numIsb/),srName)

      isbOut%isbSta(jSta)%staNam = stname(locq(2,iPar))
      isbOut%isbSta(jSta)%nIsb   = 0
    ENDIF

    ! Add the new value to the output record
    ! --------------------------------------
    isbOut%isbSta(jSta)%nIsb = isbOut%isbSta(jSta)%nIsb + 1
    iIsb = isbOut%isbSta(jSta)%nIsb

    isbOut%isbSta(jSta)%isbRec(iIsb)%epoch  = timisb(1,locq(4,ipar))
    isbOut%isbSta(jSta)%isbRec(iIsb)%isb    = xxx(iPar)/C*1D9 + &
          getIsb(0,isbOut%isbSta(jSta)%staNam,                  &
                 isbOut%isbSta(jSta)%isbRec(iIsb)%epoch,        &
                 isbOut%isbSta(jSta)%isbRec(iIsb)%iCarr,irc)
    isbOut%isbSta(jSta)%isbRec(iIsb)%rms    = &
                              rms*DSQRT(aNor(ikf(iPar,iPar)))/C*1D9
    isbOut%isbSta(jSta)%isbRec(iIsb)%iCarr  = locq(3,iPar)
  ENDDO

! Write the data into the file
! ----------------------------
  CALL writeIsb(isbOut)

! Clean up the memory
! -------------------
  DO iSta = 1,isbOut%nSta
    DEALLOCATE(isbOut%isbSta(iSta)%isbRec,stat=iac)
  ENDDO
  DEALLOCATE(isbOut%isbSta,stat=iac)

END SUBROUTINE isbsav


END MODULE
