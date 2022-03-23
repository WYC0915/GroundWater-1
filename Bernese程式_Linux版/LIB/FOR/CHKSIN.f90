MODULE s_CHKSIN
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE chksin(sin,sin_1,flag,neq)

! -------------------------------------------------------------------------
! Purpose:    This subroutine reports changes between the sinex information
!             sin and sin_1 (different receivers, antennas, ...)
!
! Author:     A. Jaeggi
!
! Created:    06-Nov-2002
!
! Changes:    04-Feb-2003 SS: Problematic DEALLOCATE command deactivated
!             27-May-2003 CU: Some format changes in WRITE statements
!             28-Aug-2003 AJ: Output bug fixed
!             08-Sep-2003 HU: Antenna name chr16 -> chr20
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             23-Mar-2006 RD: Adapt header for radome codes
!             19-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnprt, &
                      staNameLength, timStrgLength2
  USE d_neq,    ONLY: t_sinex,t_neq,maxStaSin

  USE s_alcerr
  USE s_maxtst
  USE s_timst2
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_sinex)                           :: sin   ! (Resulting) Sinex-Info
  TYPE(t_sinex)                           :: sin_1 ! New Sinex Info
  INTEGER(i4b)                            :: flag  ! Subroutine Mode
                                                   ! 0: Collect changes
                                                   ! 1: Print changes
  TYPE(t_neq),OPTIONAL                    :: neq   ! Needed for flag = 1

! Local Type
! ----------
  TYPE t_staTim
    CHARACTER(LEN=staNameLength)                :: staNam
    REAL(r8b)                                   :: tbeg
  END TYPE t_staTim

! Local Parameters
! ----------------
  CHARACTER(LEN=6),PARAMETER                    :: srName = 'chksin'

! Local Variables
! ---------------
  LOGICAL                                       :: found
  INTEGER(i4b)                                  :: irc
  INTEGER(i4b)                                  :: ii
  INTEGER(i4b)                                  :: kk
  INTEGER(i4b), SAVE                            :: nstation
  INTEGER(i4b), SAVE                            :: ifirst=1
  INTEGER(i4b), SAVE                            :: counter
  CHARACTER(LEN=timStrgLength2)                 :: timStrHlp
  TYPE(t_staTim),DIMENSION(maxStaSin), SAVE     :: staTim
  TYPE(t_sinex),DIMENSION(:),ALLOCATABLE        :: hlp
  TYPE(t_sinex),DIMENSION(:),ALLOCATABLE, SAVE  :: changes

! Initialize Counters
! -------------------
  IF (ifirst == 1) THEN
    staTim%staNam = ' '
    staTim%tbeg = 0.D0
    ifirst = 0
    counter = 0
    nstation = 0
    ALLOCATE(changes(maxStaSin),stat=irc)
    CALL alcerr(irc,'changes',(/maxStaSin/),srName)
  END IF
  found = .FALSE.

  SELECT CASE (flag)

! Set up list with changes (flag = 0)
! ===================================
  CASE (0)

! Check for Changes between sin, sin_1 (antecc, antnum, antsta, antrec)
! ---------------------------------------------------------------------
    IF (sin_1%antecc(1)/=sin%antecc(1).OR.sin_1%antecc(2)/=sin%antecc(2).OR. &
        sin_1%antecc(3)/=sin%antecc(3).OR.sin_1%antnum/=sin%antnum.OR.       &
        sin_1%antsta(1:20)/=sin%antsta(1:20).OR.                             &
        sin_1%antrec(1:20)/=sin%antrec(1:20)) THEN
      counter = counter + 1

! Station already in staTim%staNam
! ---------------------------------
      DO ii = 1,nstation
        IF (staTim(ii)%staNam == sin%stname) THEN
          found = .TRUE.
          EXIT
        END IF
      END DO

! Extent array if necessary
! -------------------------
      IF (counter > SIZE(changes)) THEN
        ALLOCATE(hlp(SIZE(changes)),stat=irc)
        CALL alcerr(irc,'hlp',(/SIZE(changes)/),srName)
        hlp = changes
        DEALLOCATE(changes)
        ALLOCATE(changes(SIZE(hlp)+20),stat=irc)
        CALL alcerr(irc,'changes',(/SIZE(changes)+20/),srName)
        changes(1:SIZE(hlp)) = hlp
        DEALLOCATE(hlp)
      ENDIF

! Save changes
! ------------
      changes(counter) = sin

! Save t(1) of sin_1 for next call
! --------------------------------
      IF (found) THEN
        changes(counter)%timint%t(1) = staTim(ii)%tbeg
        staTim(ii)%tbeg = sin_1%timint%t(1)
      ELSE
        nstation = nstation + 1
        CALL maxtst(1,srname,'staTim',maxStaSin,nstation,irc)
        IF (irc == 1) CALL exitrc(2)
        staTim(nstation)%staNam = sin%stname
        staTim(nstation)%tbeg = sin_1%timint%t(1)
      END IF
    END IF

! Print all changes (flag = 1)
! ============================
  CASE (1)

! Collect all changes for one station, add last entry
! ---------------------------------------------------
    DO kk = 1,nstation
      DO ii = 1, neq%misc%nstat_sinex
        IF (neq%misc%sinex(ii)%stname == staTim(kk)%staNam) THEN
          counter = counter + 1

! Extent array if necessary
! -------------------------
          IF (counter > SIZE(changes)) THEN
            ALLOCATE(hlp(SIZE(changes)),stat=irc)
            CALL alcerr(irc,'hlp',(/SIZE(changes)/),srName)
            hlp = changes
            DEALLOCATE(changes)
            ALLOCATE(changes(SIZE(hlp)+20),stat=irc)
            CALL alcerr(irc,'changes',(/SIZE(changes)+20/),srName)
            changes(1:SIZE(hlp)) = hlp
            DEALLOCATE(hlp)
          ENDIF

          changes(counter) = neq%misc%sinex(ii)
          changes(counter)%timint%t(1) = staTim(kk)%tbeg
          EXIT
        END IF
      END DO
    END DO

! Write summary in output-file
! ----------------------------
    WRITE(lfnprt,'(2(A,/))')       &
      ' Station inconsistencies:', &
      ' -----------------------'

    IF (nstation /= 0) THEN
      WRITE(lfnprt,'(2(A,/),A)')                                              &
        ' ----------------------------------------------------------------'// &
        '-------------------------------------------------------------------',&
        ' Station             First obs. epoch     Last obs. epoch      Receiver'//&
        ' type        Antenna type    dome  Ant.Nr. Ecc(1)  Ecc(2)  Ecc(3)',  &
        ' ----------------------------------------------------------------'// &
        '-------------------------------------------------------------------'

      DO kk = 1,nstation
        DO ii = 1, counter
          IF (staTim(kk)%staNam == changes(ii)%stname) THEN
            CALL timst2(1, 2, changes(ii)%timint%t, timStrHlp)
            WRITE(lfnprt,'(1x,a16,4x,a40,2x,a20,1x,a20,2x,i6,3(1x,f7.4))')  &
                  changes(ii)%stname,timStrHlp,changes(ii)%antrec(1:20), &
                  changes(ii)%antsta(1:20),changes(ii)%antnum,           &
                  changes(ii)%antecc(1),changes(ii)%antecc(2),           &
                  changes(ii)%antecc(3)
          END IF
        END DO
        WRITE(lfnprt,*)
      END DO
    ELSE
      WRITE(lfnprt,"(A)") ' No inconsistencies detected'
    END IF

! Deallocate array
! ----------------
!!    DEALLOCATE(changes)

  END SELECT

END SUBROUTINE chksin


END MODULE
