MODULE s_CHKSLP
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE chkslp(maxsat,iprnt2,ititl2,nsat  ,svn   ,nfraux,      &
                  nslip ,lstslp,ndel  ,lstdel,nnewam,lstamb,amsflg)

! -------------------------------------------------------------------------
! Purpose:    Check list of corrected cycle slips for epoch-by-epoch
!             "cycle slip" events
!
! Author:     R. Dach
!
! Created:    29-Feb-2008
!
! Changes:    18-Jan-2010 RD: Decread tslip=10->4
!             26-Jan-2012 RD/EO: remove also epoch before such a sequence
!             29-Feb-2012 RD: Correct array dimensions of iwlf0 and xslip0
!             26-Mar-2012 RD: Use WTMSGS as module now
!             20-Nov-2012 RD: Check percentage of epochs by type integer
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,  ONLY: i4b, r8b
  USE s_mrkobs
  USE s_wtmsgs
  USE s_updamb
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                  :: maxsat ! Maximum number of satellites
  INTEGER(i4b)                  :: iprnt2 ! Print level for cycle slip detect.
                                          !  =0: no messages printed
                                          !  =1: print summary
                                          !  =2: all messages printed
  INTEGER(i4b)                  :: ititl2 ! Title line flag
  INTEGER(i4b)                  :: nsat   ! Number of satellites
  INTEGER(i4b), DIMENSION(*)    :: svn    ! List of SV numbers
  INTEGER(i4b)                  :: nfraux ! Number of frequencies on aux.-file

! input/output:
  INTEGER(i4b)                  :: nslip  ! Number of slips detected
  INTEGER(i4b), DIMENSION(6,*)  :: lstslp ! Slip description
                                          ! 1: epoch number
                                          ! 2: sv number
                                          ! 3: frequency number
                                          ! 4: wavelength factor
                                          ! 5: wavelength factor of l5
                                          !    if an l1 and l2 slip occurs at
                                          !    the same epoch for the same
                                          !    satellite:
                                          !    =3 for l1, indicating that
                                          !       an l2 slip occured too
                                          !    =wavelength of l5 for l2
                                          ! 6: detected by
                                          !    =1: single freq. slip detection
                                          !    =2: dual freq. slip detection
                                          !    =3: clock
                                          !    =4: user
                                          !    all cycle slips detected in the
                                          !    latest run have a negative sign
  INTEGER(i4b)                  :: ndel   ! Number of deletions
  INTEGER(i4b), DIMENSION(5,*)  :: lstdel ! Definition of mark request number
                                          ! 1: sv-number
                                          ! 2: first epoch of marked area i
                                          ! 3: last  epoch of marked area i
                                          ! 4: frequency (1=L1, 2=L2)
                                          ! 5: marked by
                                          !    =1: single freq. rejection
                                          !    =2: dual   freq. rejection
                                          !    =3: unpaired l1/l2 observations
                                          !    =4: user
                                          !    =5: small elevation
                                          !    =6: small piece
                                          !    =7: bad observed-computed
                                          !    all areas marked or changed in
                                          !    the latest run have a negative
                                          !    sign
  INTEGER(i4b), DIMENSION(*)    :: nnewam ! Number of ambiguities for satellite
  INTEGER(i4b),                  &
           DIMENSION(3,maxsat,*):: lstamb ! List of ambiguities (i,iSat,iAmb)
                                          ! 1: The first epoch
                                          ! 2: Type: =1: file
                                          !          =2: cycle slip
                                          !          =3: user
                                          !          =4: gap
                                          !          =5: preprocessing problem
                                          !          =6: clock event
                                          ! 3: The last epoch with observations
  CHARACTER(LEN=*)              :: amsflg ! Indicater of maxams exceedings


! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6),PARAMETER :: srName = 'chkslp'


! Local Variables
! ---------------
  INTEGER(i4b), DIMENSION(3) :: iwlf0
  INTEGER(i4b)               :: iSlip, jSlip, lSlip, mSlip, tSlip
  INTEGER(i4b)               :: iEpo
  INTEGER(i4b)               :: iFrq
  INTEGER(i4b)               :: iSat
  INTEGER(i4b)               :: irc

  REAL(r8b), DIMENSION(3)    :: slip0
  REAL(r8b), DIMENSION(3)    :: xslip0


! Init local variables
! --------------------
  iwlf0  = 0
  slip0  = 0d0
  xslip0 = 0d0

! Count the number of cycle slips in the interval of tslip epochs
! ---------------------------------------------------------------
  DO iSlip = 1,nSlip-1
    IF (IABS(lstslp(6,islip)) > 1000) CYCLE
    mSlip = 0
!!    tSlip = 10
    tSlip = 4
    lSlip = iSlip
    DO jSlip = iSlip+1,nSlip
      IF (lstslp(2,iSlip) /= lstslp(2,jSlip)) CYCLE    ! svn
      IF (lstslp(3,iSlip) /= lstslp(3,jSlip)) CYCLE    ! frq
      IF (lstslp(1,jSlip) - lstslp(1,lSlip) > tSlip) EXIT
      mSlip = mSlip + 1
      lSlip = jSlip
    ENDDO

! Check whether we have a problem with the current slip
! -----------------------------------------------------
    IF (lstslp(1,lSlip) - lstslp(1,iSlip) <= tSlip) CYCLE
    IF (IDNINT(DBLE(mSLip)/DBLE(lstslp(1,lSlip) - lstslp(1,iSlip))*100d0) < 80 )CYCLE

! Indicate slips to be removed as outliers
! ----------------------------------------
    mSlip = 0
    DO jSlip = iSlip,lSlip
      IF (lstslp(2,iSlip) /= lstslp(2,jSlip)) CYCLE    ! svn
      IF (lstslp(3,iSlip) /= lstslp(3,jSlip)) CYCLE    ! frq

      IF (lstslp(6,jslip) < 0) lstslp(6,jslip) = lstslp(6,jslip) - 1000
      IF (lstslp(6,jslip) > 0) lstslp(6,jslip) = lstslp(6,jslip) + 1000

    ENDDO

! Mark observations as outliers due to cycle slip resolution problem
! ------------------------------------------------------------------
    DO iEpo = MAX(1,lstslp(1,iSlip)-1),lstslp(1,lSlip)
      DO iFrq = 1, MIN(2,nfraux)
        CALL mrkobs(9,lstslp(2,iSlip),iEpo,ifrq,  &
                    nsat,svn,ndel  ,lstdel)
      ENDDO
      IF (iprnt2 >= 2) THEN
        CALL wtmsgs(12,ITITL2,iEpo,lstslp(2,iSlip),0,3,iwlf0,slip0,xslip0)
      ENDIF
    ENDDO

! Update the list of ambiguities
! ------------------------------
    DO iSat = 1,nSat
      IF (svn(isat) == lstslp(2,iSlip)) THEN
        CALL updamb(iSat,lstslp(1,iSlip),lstslp(1,lSlip)+1, &
                    5,nnewam,lstamb,nsat,svn,iprnt2,amsflg,irc)
        EXIT
      ENDIF
    ENDDO
  ENDDO

  RETURN

END SUBROUTINE chkslp

END MODULE

