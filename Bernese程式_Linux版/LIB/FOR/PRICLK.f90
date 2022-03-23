MODULE s_PRICLK
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE priclk(mxclcq,stName,nepobs,nclkst,nclksa, &
                  tobs,rms,locq,xxx0,dxxx,Aii)

! -------------------------------------------------------------------------
! Purpose:    Prints epochwise clock estimates in GPSEST
!
! Author:     R. Dach
!
! Created:    24-Jan-2002
! Last mod.:  10-Jun-2009
!
! Changes:    18-Feb-2002  RD: Min. number of obs. requ. for ref-clock
!             28-Jan-2003  RD: Number of obs. for kin. pos.(clkobs->nepobs)
!             09-May-2009  RD: Seperate receiver clocks for GPS/GLONASS
!             10-Jun-2009  RD: Use "undef" to init. clocks
!
! SR used:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_global, ONLY: g_svnsys
  USE d_const,  ONLY: c
  USE d_clkrnx, ONLY: undef

  USE s_lowerc
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                     :: mxclcq ! size of locq
  CHARACTER(LEN=*), DIMENSION(*)   :: stName ! List of station names
  INTEGER(i4b), DIMENSION(3)       :: nepobs ! Min # of obs. for epoch param.s
                                             ! 1: sta-clk / 2: sat-clk / 3: kin
  INTEGER(i4b)                     :: nclkst ! number of est. station clocks
  INTEGER(i4b)                     :: nclksa ! number of est. sat. clocks
  REAL(r8b)                        :: tobs   ! MJD for clock value
  REAL(r8b)                        :: rms    ! a posteriori rms
  INTEGER(i4b),  DIMENSION(mxclcq) :: locq   ! Parameter characterization
  REAL(r8b)                        :: xxx0   ! apriori value
  REAL(r8b)                        :: dxxx   ! solution (improvement)
  REAL(r8b)                        :: Aii    ! Element of COV matrix

! output:

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  REAL(r8b), PARAMETER             :: sngClk = undef

! Local Variables
! ---------------
  CHARACTER(LEN=1), DIMENSION(2)   :: chr1

  INTEGER(i4b)                     :: iTyp
  INTEGER(i4b)                     :: iSta
  INTEGER(i4b)                     :: iSat
  INTEGER(i4b)                     :: iFil
  INTEGER(i4b)                     :: nObs
  INTEGER(i4b)                     :: nRef

  REAL(r8b)                        :: rms1
  REAL(r8b)                        :: xxx1

! Check the parameter type
! ------------------------
  IF (locq(1) /= 23 .AND. locq(1) /= 24) RETURN

! Check COV-information
! ---------------------
  IF (Aii > 0.0) THEN
    rms1 = 1.0D9*rms*DSQRT(Aii)/C
  ELSE
    rms1 = sngClk
  ENDIF


! Write a clock result record
! ---------------------------

! Get information from locq
  iTyp = locq(1)
  iSta = locq(2)
  iSat = locq(3)
  iFil = locq(5)
  nObs = locq(6)
  nRef = locq(7)

! Compute the solution
  xxx1 = xxx0*1d6-dxxx*1d6/C

! Flag the solution
  chr1 = ' '
  IF (nRef > 0) chr1(1) = 'R'
  IF (nObs < nepobs(iTyp-22)) chr1(1) = '*'

! Individual receiver clocks for each satellite system
  IF( iTyp == 23 .AND. iSat /= 0 ) THEN
    chr1(2) = g_svnsys(ISAT-1)
    CALL lowerc(chr1(2))
  ENDIF

! Write the line into the program output
! --------------------------------------

! Estimated station clock
  IF (iTyp == 23 .AND. rms1 < sngClk .AND. nObs > 0) THEN
    WRITE(lfnprt,'(1X,I4,2X,I4,4(2X,F17.6),2X,F14.3,I6,A2,A1," # ",A16)') &
          iTyp,iSta,tobs,xxx0*1d6,-dxxx*1d6/c,xxx1,rms1,nObs,chr1(1:2),stName(iSta)

! Estimated satellite clock
  ELSE IF (iTyp == 24 .AND. rms1 < sngClk .AND. nObs > 0) THEN
    WRITE(lfnprt,'(1X,I4,2X,I4,4(2X,F17.6),2X,F14.3,I6,A2)') &
          iTyp,iSat,tobs,xxx0*1d6,-dxxx*1d6/c,xxx1,rms1,nObs,chr1(1)

! Station clock fixed as reference
  ELSE IF (iTyp == 23 .AND. nRef == -1 .AND. nClkSt > 0) THEN
    WRITE(lfnprt,'(1X,I4,2X,I4,2(2X,F17.6),2X,A17,2X,F17.6,2X,A14,6X,A2,"  # ",A16)') &
          iTyp,iSta,tobs,xxx0*1d6,'---',xxx1,'---','R',stName(iSta)

! Satellite clock fixed as reference
  ELSE IF (iTyp == 24 .AND. nRef == -1 .AND. nClkSa > 0) THEN
    WRITE(lfnprt,'(1X,I4,2X,I4,2(2X,F17.6),2X,A17,2X,F17.6,2X,A14,6X,A2)') &
          iTyp,iSat,tobs,xxx0*1d6,'---',xxx1,'---','R'

! Singular station clock (ususally not printed)
  ELSE IF (iTyp == 23 .AND. rms1 == sngClk .AND. nObs > 0 .AND. 1 == 2) THEN
    WRITE(lfnprt,'(1X,I4,2X,I4,2X,F17.6,3(2X,A17),2X,A14,I6,A2,"  # ",A16)') &
          iTyp,iSta,tobs,'---','---','---','---',nObs,'S',chr1(2),stName(iSta)

! Singular satellite clock (ususally not printed)
  ELSE IF (iTyp == 24 .AND. rms1 == sngClk .AND. nObs > 0 .AND. 1 == 2) THEN
    WRITE(lfnprt,'(1X,I4,2X,I4,2X,F17.6,3(2X,A17),2X,A14,I6,A2)') &
          iTyp,iSat,tobs,'---','---','---','---',nObs,'S'

  ENDIF

  RETURN
END SUBROUTINE priclk

END MODULE
