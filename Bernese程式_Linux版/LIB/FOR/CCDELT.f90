MODULE s_CCDELT
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE ccdelt(iEpo1, iEpo2, iClk, OutClkHead, OutClkRec, &
                    nJump, clkJump, nDeltaT, mDeltaT, sDeltaT)

! -------------------------------------------------------------------------
! Purpose:    Computes Mean and s-dev. of delta T (ms/s)
!
! Parameters:
!        in : iEpo1     : First epoch                               i4b
!             iEpo2     : Last epoch                                i4b
!             iClk      : Index of the clock                        i4b
!             OutClkHead: Header of the output file                 t_clkhead
!             OutClkRec : Data records of the output file           t_clkrec
!             nJump     : Number of clock jumps                     i4b
!             ClkJump   : Detected clock jumps                      t_Jump
!       out : nDeltaT   : Number of delta T used                    i4b
!             mDeltaT   : Mean Delta T                              r8b
!             sDeltaT   : S-Dev of Delta T                          r8b
!
!
! Author:     R. Dach
!
! Created:    18-Feb-2001
! Last mod.:  21-Jan-2004
!
! Changes:    02-May-2001  RD: use the DABS of DeltaT
!             21-Dec-2001  HU: Use m_bern, ONLY for modules
!             21-Jan-2004  RD: Use m_bern, ONLY for modules
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

!
  USE m_bern
  USE d_clkrnx, ONLY: t_clkhead,t_clkrec
  USE p_ccrnxc, ONLY: t_Jump
  IMPLICIT NONE
!
! Variables from parameter list
! -----------------------------
  INTEGER(i4b)    :: iEpo1      ! First epoch
  INTEGER(i4b)    :: iEpo2      ! Last epoch
  INTEGER(i4b)    :: iClk       ! Index of the clock
  TYPE(t_clkhead) :: OutClkHead ! Header of the output file
  TYPE(t_clkrec)  :: OutClkRec  ! Data records of the output file
  INTEGER(i4b)    :: nJump      ! Number of clock jumps
  TYPE(t_Jump), DIMENSION(:), POINTER &
                  :: ClkJump    ! Detected clock jumps
  INTEGER(i4b)    :: nDeltaT    ! Number of delta T used
  REAL(r8b)       :: mDeltaT    ! Mean Delta T
  REAL(r8b)       :: sDeltaT    ! S-Dev of Delta T
!
!******************************************************************************
!
! Local Variables
! ---------------
  INTEGER(i4b)    :: iEpo       ! Counter for epochs
  INTEGER(i4b)    :: jEpo       ! Counter for epochs
  INTEGER(i4b)    :: iJump      ! Counter for jump list
!
  LOGICAL         :: isJump     ! Is there a jump in the interval?
!
! Compute mean dt per clock
! -------------------------
  nDeltaT=0
  mDeltaT=0d0
  sDeltaT=0d0
  iEpo=iEpo1
  DO WHILE (iEpo < iEpo2)
    IF (OutClkRec%Clock(iClk,iEpo) /= 999999.999999d0) THEN
      jEpo=iEpo+1
      DO WHILE (jEpo <= OutClkRec%nEpo)
        IF (OutClkRec%Clock(iClk,jEpo) /= 999999.999999d0) EXIT
        jEpo=jEpo+1
      ENDDO
      iJump=1
      isJump=.FALSE.
      DO WHILE (iJump <= nJump)
        isJump = isJump .OR.                        &
                 (clkJump(iJump)%iClk == iClk .AND. &
                  clkJump(iJump)%jEpo >  iEpo .AND. &
                  clkJump(iJump)%iEpo <  jEpo)
        iJump=iJump+1
      ENDDO
      IF (jEpo <= OutClkRec%nEpo .AND. .NOT. isJump) THEN
        nDeltaT = nDeltaT + 1
        mDeltaT = mDeltaT + DABS( &
                  (OutClkRec%Clock(iClk,jEpo)-OutClkRec%Clock(iClk,iEpo)) / &
                  (OutClkRec%Epoch(jEpo)-OutClkRec%Epoch(iEpo)))
      ENDIF
      iEpo=jEpo-1
    ENDIF
    iEpo=iEpo+1
  ENDDO
  IF (nDeltaT > 0) THEN
    mDeltaT = mDeltaT / DBLE(nDeltaT)
  ELSE
    mDeltaT = 0d0
  ENDIF
!
! Compute std-dev. dt per clock
! -----------------------------
  IF (nDeltaT > 1) THEN
    iEpo=iEpo1
    DO WHILE (iEpo < iEpo2)
      IF (OutClkRec%Clock(iClk,iEpo) /= 999999.999999d0) THEN
!
        jEpo=iEpo+1
        DO WHILE (jEpo <= OutClkRec%nEpo)
          IF (OutClkRec%Clock(iClk,jEpo) /= 999999.999999d0) EXIT
          jEpo=jEpo+1
        ENDDO
!
        iJump=1
        isJump=.FALSE.
        DO WHILE (iJump <= nJump)
          isJump = isJump .OR.                        &
                   (clkJump(iJump)%iClk == iClk .AND. &
                    clkJump(iJump)%jEpo >  iEpo .AND. &
                    clkJump(iJump)%iEpo <  jEpo)
          iJump=iJump+1
        ENDDO
!
        IF (jEpo <= OutClkRec%nEpo .AND. .NOT. isJump) THEN
          sDeltaT = sDeltaT + ( DABS( &
                    (OutClkRec%Clock(iClk,jEpo)-OutClkRec%Clock(iClk,iEpo)) / &
                    (OutClkRec%Epoch(jEpo)-OutClkRec%Epoch(iEpo)))-mDeltaT) ** 2
        ENDIF
        iEpo=jEpo-1
      ENDIF
      iEpo=iEpo+1
    ENDDO
    sDeltaT = DSQRT(sDeltaT / DBLE(nDeltaT-1))
  ELSE
    sDeltaT = 0d0
  ENDIF

  RETURN
  END SUBROUTINE

END MODULE
