MODULE s_CHKEVT
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE chkevt(defevt,cycevt,iprflg,rmsl3 ,maxevt,nEpoch,maxcmb, &
                  nrSat ,numsat,obstim,obsrec,obsflg,nEvt  ,timevt, &
                  sizevt,sysevt)

! -------------------------------------------------------------------------
! Purpose:    Check for clock events in the RNXSMT program
!
! Author:     R. Dach
!
! Created:    19-Jan-2004
!
! Changes:    07-Aug-2007 RD: Min. numb. of satellites for a clock event
!             28-Mar-2012 RD: Use SVN2CHR as module now
!             28-Mar-2012 RD: Remove unused modules
!             15-Aug-2012 LP: Boundary check for sysevt
!             31-Oct-2012 SS: Commented last change
!             16-Nov-2012 RD: More safe computation for IFC
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnprt
  USE m_global, ONLY: g_svnsys
  USE d_satfrq, ONLY: faclin
  USE d_const,  ONLY: C
  USE f_tstflg
  USE s_major1
  USE s_svn2chr
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  REAL(r8b)                      :: defevt  ! Tolerance between L3 and P3 clock
                                            ! (input option in seconds)
  REAL(r8b)                      :: cycevt  ! Tolerance to identify a ms-jump
                                            ! in seconds, 0d0=no repair
  INTEGER(i4b)                   :: iprflg  ! print flag 0=summary only
                                            !            1=detailed output
  REAL(r8b)                      :: rmsl3   ! Noise level for L3-P3
  INTEGER(i4b)                   :: maxevt  ! Maximum number of clock events
  INTEGER(i4b)                   :: nEpoch  ! Number of epochs
  INTEGER(i4b)                   :: maxcmb  ! Number of frequencies
  INTEGER(i4b)                   :: nrSat   ! Number of satellites
  INTEGER(i4b), DIMENSION(:)     :: numsat  ! List with satellite numbers
  REAL(r8b),    DIMENSION(:)     :: obstim  ! Epochs of the observations (MJD)

! input/output:
  REAL(r8b),    DIMENSION(:,:,:) :: obsrec  ! Observations (iEpo,iFrq,iSat)
                                            ! in meters

! input:
  CHARACTER(LEN=1),               &
                DIMENSION(:,:,:) :: obsflg  ! Observation flags

! output:
  INTEGER(i4b)                   :: nEvt    ! Number of clock events in list
  REAL(r8b),    DIMENSION(:)     :: timevt  ! Epochs of clock events
                                            ! Hours since DNINT(obstim(1))
  REAL(r8b),    DIMENSION(:)     :: sizevt  ! Size of the event in meters
                                            ! (1d20 if unknown size)
  CHARACTER(LEN=1), DIMENSION(:) :: sysevt  ! Satellite type character for
                                            ! which the event took place

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER    :: srName = 'chkevt'
  INTEGER(i4b),     PARAMETER    :: minClk = 3


! Local Variables
! ---------------
  CHARACTER(LEN=1), DIMENSION(12):: sysChr
  CHARACTER(LEN=1)               :: satchr

  INTEGER(i4b)                   :: iEpo, jEpo
  INTEGER(i4b)                   :: iCmb
  INTEGER(i4b)                   :: iTyp, nTyp
  INTEGER(i4b)                   :: iSat, nSat
  INTEGER(i4b)                   :: satnum
  INTEGER(i4b)                   :: nClock
  INTEGER(i4b)                   :: iDiff
  INTEGER(i4b), DIMENSION(2)     :: numevt

  REAL(r8b),    DIMENSION(nrSat) :: clkDiff
  REAL(r8b)                      :: clkCode, clkPhas
  REAL(r8b)                      :: dClk
  REAL(r8b)                      :: dummy

  LOGICAL                        :: prtHead


! Init variables
! --------------
  nEvt    = 0
  IF (defevt == 0d0) RETURN

  prtHead = .TRUE.
  numevt  = 0

! Get the list of system characters
! ---------------------------------
  nTyp = 1
  sysChr(nTyp) = '*'  ! All systems
  DO iTyp = 0,SIZE(g_svnsys)-1
    IF (g_svnsys(iTyp) == ' ') CYCLE
    DO iSat = 1,nrSat
      CALL svn2chr(numsat(isat),satnum,satchr)
      IF (g_svnsys(iTyp) == satchr) THEN
        nTyp = nTyp+1
        sysChr(nTyp) = g_svnsys(iTyp)
        EXIT
      ENDIF
    ENDDO
  ENDDO
  IF (nTyp == 2) nTyp = 1  ! Only one satellite system in the file

! Loop all epochs
! ---------------
  DO iEpo = 2,nEpoch
    jEpo = iEpo-1

! Loop all satellite system types
! -------------------------------
    iTypLoop: DO iTyp = 1,nTyp
      nClock = 0

! Loop all satellites (only of the current system type)
! -----------------------------------------------------
      iSatLoop: DO iSat = 1,nrSat
        clkDiff(iSat) = 1d20

        CALL svn2chr(numsat(isat),satnum,satchr)
        IF (sysChr(iTyp) /= '*' .AND. sysChr(iTyp) /= satchr) CYCLE

! Check that observations are available for the satellite
! (for this and the previous epoch)
! -------------------------------------------------------
        DO iCmb = 1,maxcmb

          IF (obsrec(jEpo,iCmb,iSat) == 0d0 .OR.  &
              obsrec(iEpo,iCmb,iSat) == 0d0) CYCLE iSatLoop

          IF (tstflg(obsflg(jEpo,iCmb,iSat),0) .OR.  &
              tstflg(obsflg(iEpo,iCmb,iSat),0)) CYCLE iSatLoop

        ENDDO

! Compute the change of the clock for each frequency
! --------------------------------------------------
        nClock = nClock + 1

        clkDiff(iSat) =                                              &
                  faclin(3,1,numSat(iSat))*(obsrec(jepo,1,isat) -    &
                                            obsrec(iepo,1,isat))
        clkDiff(iSat) = clkDiff(iSat) +                              &
                  faclin(3,2,numSat(iSat))*(obsrec(jepo,2,isat) -    &
                                            obsrec(iepo,2,isat))
        clkDiff(iSat) = clkDiff(iSat) -                              &
                  faclin(3,1,numSat(iSat))*(obsrec(jepo,3,isat) -    &
                                            obsrec(iepo,3,isat))
        clkDiff(iSat) = clkDiff(iSat) -                              &
                  faclin(3,2,numSat(iSat))*(obsrec(jepo,4,isat) -    &
                                            obsrec(iepo,4,isat))

      ENDDO iSatLoop


! Compute the clock values
! ------------------------
      IF (nClock > 0) THEN
        nSat = nClock

        CALL major1(nrsat,clkDiff,iDiff,dummy)

        nClock = 0
        clkCode = 0d0
        clkPhas = 0d0
        DO iSat = 1,nrSat
          IF (clkDiff(iSat) /= 1d20 .AND. &
              DABS(clkDiff(iSat)-clkDiff(iDiff)) < 4d0*rmsl3) THEN
            nClock = nClock + 1
            clkPhas = clkPhas +                                      &
                  faclin(3,1,numSat(iSat))*(obsrec(jepo,1,isat) -    &
                                            obsrec(iepo,1,isat))  +  &
                  faclin(3,2,numSat(iSat))*(obsrec(jepo,2,isat) -    &
                                            obsrec(iepo,2,isat))
            clkCode = clkCode +                                      &
                  faclin(3,1,numSat(iSat))*(obsrec(jepo,3,isat) -    &
                                            obsrec(iepo,3,isat))  +  &
                  faclin(3,2,numSat(iSat))*(obsrec(jepo,4,isat) -    &
                                            obsrec(iepo,4,isat))
!          else if (clkDiff(iSat) /= 1d20) then
!            obsrec(jepo,1,isat) = 0d0
!            obsrec(iepo,1,isat) = 0d0
          ENDIF
        ENDDO

      ENDIF

      IF (nClock > 0 .AND. nClock >= minClk) THEN
        clkPhas = clkPhas/DBLE(nClock)
        clkCode = clkCode/DBLE(nClock)

! Is there any code-phase clock inconsitency?
! -------------------------------------------
        IF (DABS(clkPhas-clkCode)/C > defevt) THEN

          ! Report the event in an extended output
          IF (iprflg == 1 .AND. prtHead) THEN
            WRITE(lfnprt,'(A,/,A)')       &
            ' EPOCH    PHASE-CLOCK (M)     CODE-CLOC' // &
            'K (M)     DIFF.(MSEC)    HANDLING    SYS',  &
            ' --------------------------------------' // &
            '----------------------------------------'
            prtHead=.FALSE.
          ENDIF

! Is it a ms-Jump: repair as cycle slip
! -------------------------------------
          dClk=DNINT((clkPhas-clkCode)/C*1d3)/1d3*C

          IF (DABS(dClk-(clkPhas-clkCode))/C < cycevt .AND. &
              DABS(dClk)/C > defevt .AND. cycevt /= 0d0) THEN

            nEvt = nEvt+1
            IF (nEvt <= maxevt) THEN
              timEvt(nEvt) = (obstim(iEpo)-DNINT(obstim(1)))*24d0
              sizEvt(nEvt) = dclk
              sysEvt(nEvt) = sysChr(iTyp)
            ENDIF

            DO jEpo = iEpo,nEpoch
              DO iCmb = 1,2
                DO iSat = 1,nrSat

                  CALL svn2chr(numsat(isat),satnum,satchr)
                  IF (sysChr(iTyp) /= '*' .AND. sysChr(iTyp) /= satchr) CYCLE

                  IF (obsrec(jEpo,iCmb,iSat) == 0d0) CYCLE
                  obsrec(jEpo,iCmb,iSat) = obsrec(jEpo,iCmb,iSat)+dclk
                ENDDO
              ENDDO
            ENDDO

           ! Report the event in an extended output
           numevt(1) = numevt(1)+1
           IF (iprflg == 1)                                   &
             WRITE(lfnprt,'(I6,2F19.4,F16.6,4X,A,2i5)')       &
             iepo,clkPhas,clkCode,(clkPhas-clkCode)/C*1d3,    &
             'CYCLE SLIP   ' // syschr(iTyp),nClock,nSat

! Other clock event: split arc
! ----------------------------
          ELSE
            nEvt = nEvt+1

            IF (nEvt <= maxevt) THEN
              timEvt(nEvt) = (obstim(iEpo)-DNINT(obstim(1)))*24d0
              sizEvt(nEvt) = 1d20
              sysEvt(nEvt) = sysChr(iTyp)
            ENDIF

            ! Report the event in an extended output
            numevt(2) = numevt(2)+1
            IF (iprflg == 1)                                   &
              WRITE(lfnprt,'(I6,2F19.4,F16.6,4X,A,2i5)')       &
              iepo,clkPhas,clkCode,(clkPhas-clkCode)/C*1d3,    &
              'ARC SPLIT    ' // sysChr(iTyp),nClock,nSat

            ! If "new arc" for all satellite systems ("*"), then skip the
            ! remaining single satellite system tests
            IF (sysChr(iTyp) == '*') EXIT iTypLoop
          ENDIF
        ENDIF

      ENDIF ! Clock event found

    ENDDO iTypLoop

  ENDDO ! next epoch

! Write summary for clock events:
! -------------------------------
!!  nSat = nEvt
!!  DO iSat = 1,nSat
!!    IF (nEvt > maxevt) EXIT
!!    IF (sysEvt(iSat).EQ.g_svnsys(1)) nEvt = nEvt - 1
!!    WRITE(*,*) iSat,nEvt,sysEvt(iSat),g_svnsys(1)
!!  ENDDO
  IF (nEvt <= maxevt) THEN
    WRITE(lfnprt,'(A,/,3(A,I4,3X),/,A,/)')             &
          ' --------------------------------------' // &
          '----------------------------------------',  &
          ' NUMBER OF CLOCK EVENTS: ',numevt(1)+numevt(2),               &
          ' HANDLED AS CYCLE SLIP: ',NUMEVT(1),' ARC SPLIT: ',NUMEVT(2), &
          ' --------------------------------------' // &
          '----------------------------------------'
  ELSE
    WRITE(lfnprt,'(A,/,3(A,I4,3X),/,2(A,/))')          &
          ' --------------------------------------' // &
          '----------------------------------------',  &
          ' NUMBER OF CLOCK EVENTS: ',numevt(1)+numevt(2),               &
          ' HANDLED AS CYCLE SLIP: ',NUMEVT(1),' ARC SPLIT: ',NUMEVT(2), &
          ' NO FURTHER PROCESSING OF THIS FILE...',    &
          ' --------------------------------------' // &
          '----------------------------------------'
  ENDIF
  RETURN
END SUBROUTINE chkevt

END MODULE
