MODULE s_PRIREF
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE priref(mxclcq,iTyp,nclkst,nclksa,secIpl,clkhed,clkrec, &
                  stName,nEpSam,nepobs,rnxclk,nParN,locq,Anor)

! -------------------------------------------------------------------------
! Purpose:    Prints (fixed) reference clocks in GPSEST
!
! Author:     R. Dach
!
! Created:    25-Jan-2002
! Last mod.:  10-Jun-2009
!
! Changes:    28-Jan-2003  RD: Number of obs. for kin. pos.(clkobs->nepobs)
!             01-Nov-2007  HB: Add parameter secIpl
!             29-May-2009  RD: Input clocks also from input clk rnx file
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
  USE d_clkrnx, ONLY: t_clkhead,t_clkrec,undef
  USE f_ikf
  USE s_priclk
  USE s_gtsclk
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                     :: mxclcq ! size of locq
  INTEGER(i4b)                     :: iTyp   ! Parameter type
                                             !   23: sta-clock/24: sat-clock
  INTEGER(i4b)                     :: nclkst ! number of est. station clocks
  INTEGER(i4b)                     :: nclksa ! number of est. sat. clocks
  REAL(r8b)                        :: secIpl ! Interval for clock interpolation
  TYPE(t_clkHead)                  :: clkhed ! Header information for
                                             ! clock rinex output file
  TYPE(t_clkRec)                   :: clkrec ! Station clock apriori values
  CHARACTER(LEN=*), DIMENSION(*)   :: stName ! List of station names
  INTEGER(i4b)                     :: nEpSam ! Sampling of preeliminated epoch
                                             ! parameters
  INTEGER(i4b), DIMENSION(3)       :: nepobs ! Min # of obs. for epoch param.s
                                             ! 1: sta-clk / 2: sat-clk / 3: kin
  INTEGER(i4b)                     :: rnxclk ! What to do if no input clock
                                             ! rinex for satellite clocks:
                                             !  -1: ignore clock rinex file
                                             !   0: Try also sat clk file
                                             !   1: Use obs. (interpol. clk rnx)
                                             !   2: Skip obs.
                                             !   3: Use obs. (sat clk = zero)
  INTEGER(i4b)                     :: nParN  ! # of Param. without ambig.
  INTEGER(i4b), DIMENSION(mxclcq,*) :: locq  ! Parameter description
  REAL(r8b),    DIMENSION(*)       :: Anor   ! Updated normal equation matrix

! output:

! Local Variables
! ---------------
  INTEGER(i4b), DIMENSION(mxclcq) :: lcq2
  INTEGER(i4b)                    :: iRef
  INTEGER(i4b)                    :: iClk
  INTEGER(i4b)                    :: iSvn
  INTEGER(i4b)                    :: iEpo,jEpo
  INTEGER(i4b)                    :: numEpo
  INTEGER(i4b)                    :: iPar
  INTEGER(i4b)                    :: irc

  REAL(r8b)                       :: tObs
  REAL(r8b)                       :: dSec
  REAL(r8b)                       :: xxx0

! Check the parameter type
! ------------------------
  IF (iTyp /= 23 .AND. iTyp /= 24) RETURN

  IF (iTyp == 23 .AND. nClkSt == 0) RETURN
  IF (iTyp == 24 .AND. nClkSa == 0) RETURN

! Write only, if fixed reference clocks
! -------------------------------------
  IF (clkHed%numRef == 2) RETURN

! Loop all reference clocks
! -------------------------
  DO iRef = 1,clkHed%ref(1)%nRef

    iClk = clkhed%ref(1)%clk(iRef)%idx
    iSVN = clkhed%ref(1)%clk(iRef)%idx0

! Set the locq information for printing
! -------------------------------------
    lcq2(1) = iTyp
    lcq2(2) = iClk
    lcq2(3) = iSvn
    lcq2(6) = 0
    lcq2(7) = -1

! Station reference clocks
! ------------------------
    IF (iTyp == 23 .AND. iClk<=clkHed%nSta) THEN
      DO iepo = 1, clkrec%nEpo
        tObs = clkhed%TFirst+DBLE(iEpo-1)*clkrec%epoch(1)/clkrec%nEpo/86400d0
        IF (nEpSam == 0) THEN
          jEpo = iEpo
        ELSE
          DSEC=DBLE(IEPO-1)*CLKREC%EPOCH(1)/CLKREC%NEPO
          NUMEPO=NINT(clkrec%epoch(1)/DBLE(nEpSam))+1
          JEPO=NINT(DSEC/CLKREC%EPOCH(1)*(NUMEPO-1))+1
        ENDIF
        IF (clkrec%clock(iClk,jEpo) /= undef) THEN
          tObs = clkhed%TFirst+DBLE(iEpo-1)*clkrec%epoch(1)/clkrec%nEpo/86400d0
          DO iPar = 1,nParN
            IF (locq(1,iPar) /= 23 .AND. locq(1,iPar) /= 24) CYCLE
            IF (iEpo == locq(4,iPar) .AND. ANOR(IKF(iPar,IPar)) > 0d0) THEN
              xxx0 = clkrec%clock(iClk,jEpo)
              CALL priclk(mxclcq,stName,nepobs,nclkst,nclksa,  &
                          tobs,1d0,lcq2,xxx0,0d0,1d0)
              EXIT  ! Parameter loop; next epoch
            ENDIF
          ENDDO
        ENDIF
      ENDDO

! Satellite reference clocks
! --------------------------
    ELSE IF (iTyp == 24 .AND. iClk > clkHed%nSta) THEN
      DO iepo = 1, clkrec%nEpo
        tObs = clkhed%TFirst+DBLE(iEpo-1)*clkrec%epoch(1)/clkrec%nEpo/86400d0
        DO iPar = 1,nParN
          IF (locq(1,iPar) /= 23 .AND. locq(1,iPar) /= 24) CYCLE
          IF (iEpo == locq(4,iPar) .AND. ANOR(IKF(iPar,IPar)) > 0d0) THEN
            CALL gtsclk(rnxclk,tObs,iSvn,secIpl,2,xxx0,irc)
            CALL priclk(mxclcq,stName,nepobs,nclkst,nclksa,  &
                        tobs,1d0,lcq2,xxx0,0d0,1d0)
            EXIT  ! Parameter loop; next epoch
          ENDIF
        ENDDO
      ENDDO
    ENDIF

  ENDDO ! loop all reference clocks

  RETURN
END SUBROUTINE priref

END MODULE
