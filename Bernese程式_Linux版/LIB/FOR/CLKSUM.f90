MODULE s_CLKSUM
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE clksum(mxclcq,clkhed,stName,stFil,nepobs, &
                  iPar0 ,nParN,locq,sigapr,Anor,setSum)

! -------------------------------------------------------------------------
! Purpose:    Sets the condition of sum for epoch clocks
!
! Author:     R. Dach
!
! Created:    21-Jan-2002
! Last mod.:  09-May-2009
!
! Changes:    18-Feb-2002  RD: Use only clocks with a min. number of obs.
!             29-Nov-2002  RD: Set sum only for clock type indicated by "setSum"
!             28-Jan-2003  RD: Number of obs. for kin. pos.(clkobs->nepobs)
!             21-Jan-2004  RD: Stronger weight for the sum
!                              Process only a range of parameters
!             09-May-2009  RD: Seperate receiver clocks for the indiv. GNSS
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_global, ONLY: maxsys
  USE d_clkrnx, ONLY: t_clkhead
  USE f_ikf
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                     :: mxclcq ! Size of locq
  TYPE(t_clkHead)                  :: clkhed ! Header information for
                                             ! clock rinex output file
  CHARACTER(LEN=*),DIMENSION(*)    :: stName ! List of station names
  INTEGER(i4b), DIMENSION(2,*)     :: stFil  ! Index for files in station name
  INTEGER(i4b), DIMENSION(3)       :: nepobs ! Min # of obs. for epoch param.s
                                             ! 1: sta-clk / 2: sat-clk / 3: kin
  INTEGER(i4b)                     :: iPar0  ! First parameter to check
  INTEGER(i4b)                     :: nParN  ! # of Param. without ambig.
  INTEGER(i4b), DIMENSION(mxclcq,*) :: locq  ! Parameter description
  REAL(r8b)                        :: sigapr ! A priori sigma of weight unit
  REAL(r8b),    DIMENSION(*)       :: Anor   ! Updated normal equation matrix
  LOGICAL,      DIMENSION(2)       :: setSum ! Condition of sum has to be set
                                             ! 1: station / 2: satellite

! Local Variables
! ---------------
  INTEGER(i4b), DIMENSION(maxsys+1):: clksys
  INTEGER(i4b)   :: numRef
  INTEGER(i4b)   :: iSys
  INTEGER(i4b)   :: iEpo
  INTEGER(i4b)   :: iP1,iP2
  INTEGER(i4b)   :: iPar,jPar
  INTEGER(i4b)   :: IPIP
  INTEGER(i4b)   :: iRef

! Nothing to do if no condition of sum
! ------------------------------------
  IF (clkhed%numref /= 2) RETURN

! Get the first and lst epoch number from locq(4,iPar)
! ----------------------------------------------------
  iP1 = -1
  iP2 = -1
  DO ipar = iPar0,nParN
    IF (locq(1,ipar) /= 23 .AND. locq(1,iPar) /= 24) CYCLE
    IF (locq(1,ipar) == 23 .AND. .NOT. setSum(1)) CYCLE
    IF (locq(1,ipar) == 24 .AND. .NOT. setSum(2)) CYCLE
    IF (locq(6,iPar) == 0) CYCLE
    IF (iP1 == -1 .OR. iP1 > locq(4,iPar)) iP1 = locq(4,iPar)
    IF (iP2 == -1 .OR. iP2 < locq(4,iPar)) iP2 = locq(4,iPar)

! No more ref. (for sum) if too few obs.
    IF (locq(1,ipar) == 23 .AND. nepObs(1) /= 0 .AND. &
        locq(6,iPar) < nepObs(1)) locq(7,ipar) = 0
    IF (locq(1,ipar) == 24 .AND. nepObs(2) /= 0 .AND. &
        locq(6,iPar) < nepObs(2)) locq(7,ipar) = 0
  ENDDO

! Loop all epochs
! ---------------
  DO iEpo = iP1,iP2

! Count number of reference clocks for this epoch
! -----------------------------------------------
    numRef = 0
    clksys = 0
    DO iPar = iPar0,nParN

      IF (locq(1,ipar) /= 23 .AND. locq(1,iPar) /= 24) CYCLE
      IF (locq(1,ipar) == 23 .AND. .NOT. setSum(1)) CYCLE
      IF (locq(1,ipar) == 24 .AND. .NOT. setSum(2)) CYCLE
      IF (locq(4,iPar) /= iEpo) CYCLE
      IF (locq(6,iPar) == 0 .OR. locq(7,iPar) == 0) CYCLE
      numRef = numRef+1
      IF (locq(1,ipar) == 23) &
        clksys(locq(3,IPAR)+1) = clksys(locq(3,IPAR)+1) + 1
    ENDDO ! loop all parameters

    IF (numRef == 0) CYCLE   ! next epoch
    iRef = 0

! Add weights for reference clocks
! --------------------------------

! Here we have three cases:
! 1.) a common receiver clock parameter for all satellite systems was setup
! -------------------------------------------------------------------------
    IF (clksys(1) > 0) THEN
      iRef = iRef + 1
      DO iPar = iPar0,nParN
        IF (locq(1,iPar) /= 23 .AND. locq(1,iPar) /= 24) CYCLE
        IF (locq(1,ipar) == 23 .AND. .NOT. setSum(1)   ) CYCLE
        IF (locq(1,ipar) == 24 .AND. .NOT. setSum(2)   ) CYCLE
        IF (locq(4,iPar) /= iEpo) CYCLE
        IF (locq(6,iPar) ==  0 .OR.  locq(7,iPar) ==  0) CYCLE
        IF (locq(1,iPar) == 23 .AND. locq(3,iPar) /=  0) CYCLE

        DO jPar = iPar,nParN
          IF (locq(1,jPar) /= 23 .AND. locq(1,jPar) /= 24) CYCLE
          IF (locq(1,jpar) == 23 .AND. .NOT. setSum(1)   ) CYCLE
          IF (locq(1,jpar) == 24 .AND. .NOT. setSum(2)   ) CYCLE
          IF (locq(4,jPar) /= iEpo) CYCLE
          IF (locq(6,jPar) ==  0 .OR.  locq(7,jPar) ==  0) CYCLE
          IF (locq(1,jPar) == 23 .AND. locq(3,jPar) /=  0) CYCLE

          IPIP=IKF(iPar,jPar)
          Anor(IPIP)=Anor(IPIP) + (sigapr/numRef*1000d0)**2
        ENDDO ! loop all parameters: jPar
      ENDDO ! loop all parameters: iPar
    ENDIF

! 2.) Individual receiver clocks for each satellite system
! --------------------------------------------------------
    DO iSys = 1,maxsys
      IF (clksys(1) == 0 .AND. clksys(iSys+1) /= 0) THEN
        iRef = iRef + 1
        DO iPar = iPar0,nParN
          IF (locq(1,iPar) /= 23 .AND. locq(1,iPar) /= 24) CYCLE
          IF (locq(1,ipar) == 23 .AND. .NOT. setSum(1)   ) CYCLE
          IF (locq(1,ipar) == 24 .AND. .NOT. setSum(2)   ) CYCLE
          IF (locq(4,iPar) /= iEpo) CYCLE
          IF (locq(6,iPar) ==  0 .OR.  locq(7,iPar) ==    0) CYCLE
          IF (locq(1,iPar) == 23 .AND. locq(3,iPar) /= iSys) CYCLE
          IF (locq(1,iPar) == 24 .AND. INT(locq(3,iPar)/100) /= iSys-1) CYCLE

          DO jPar = iPar,nParN
            IF (locq(1,jPar) /= 23 .AND. locq(1,jPar) /= 24) CYCLE
            IF (locq(1,jpar) == 23 .AND. .NOT. setSum(1)   ) CYCLE
            IF (locq(1,jpar) == 24 .AND. .NOT. setSum(2)   ) CYCLE
            IF (locq(4,jPar) /= iEpo) CYCLE
            IF (locq(6,jPar) ==  0 .OR.  locq(7,jPar) ==   0 ) CYCLE
            IF (locq(1,jPar) == 23 .AND. locq(3,jPar) /= iSys) CYCLE
            IF (locq(1,jPar) == 24 .AND. INT(locq(3,jPar)/100) /= iSys-1) CYCLE

            IPIP=IKF(iPar,jPar)
            Anor(IPIP)=Anor(IPIP) + (sigapr/numRef*1000d0)**2
          ENDDO ! loop all parameters: jPar
        ENDDO ! loop all parameters: iPar
      ENDIF

    ENDDO

! 3.) only satellite clocks are reference
! -------------------------------------------------------------------------
    IF (iRef == 0) THEN
      iRef = iRef + 1
      DO iPar = iPar0,nParN
        IF (locq(1,iPar) /= 23 .AND. locq(1,iPar) /= 24) CYCLE
        IF (locq(1,ipar) == 23 .AND. .NOT. setSum(1)   ) CYCLE
        IF (locq(1,ipar) == 24 .AND. .NOT. setSum(2)   ) CYCLE
        IF (locq(4,iPar) /= iEpo) CYCLE
        IF (locq(6,iPar) ==  0 .OR.  locq(7,iPar) ==  0) CYCLE
        IF (locq(1,iPar) == 23 .AND. locq(3,iPar) /=  0) CYCLE

        DO jPar = iPar,nParN
          IF (locq(1,jPar) /= 23 .AND. locq(1,jPar) /= 24) CYCLE
          IF (locq(1,jpar) == 23 .AND. .NOT. setSum(1)   ) CYCLE
          IF (locq(1,jpar) == 24 .AND. .NOT. setSum(2)   ) CYCLE
          IF (locq(4,jPar) /= iEpo) CYCLE
          IF (locq(6,jPar) ==  0 .OR.  locq(7,jPar) ==  0) CYCLE
          IF (locq(1,jPar) == 23 .AND. locq(3,jPar) /=  0) CYCLE

          IPIP=IKF(iPar,jPar)
          Anor(IPIP)=Anor(IPIP) + (sigapr/numRef*1000d0)**2
        ENDDO ! loop all parameters: jPar
      ENDDO ! loop all parameters: iPar
    ENDIF

  ENDDO ! loop epochs

  RETURN
END SUBROUTINE clksum

END MODULE
