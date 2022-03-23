! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE s_stcogn

! -------------------------------------------------------------------------
! Purpose: Handling of pseudo-stochstic pulses in program ORBGEN
!          program generalization upon request by Rolf Dach for use
!          by the IGS Analysis Coordinator
!
! Author:     G. Beutler
!
! Created:    10-Sep-2007
!
! Changes:    24-Feb-2011 RD: Revise pulse setup during maneuvers
!             08-Nov-2011 RD: Rounding problems if the arc starts within a day
!             08-Feb-2012 RD: Bugfix when applying the pulses
!             20-Sep-2012 RD: Use all modules with ONLY
!
! Copyright:  Astronomical Institute
!              University of Berne
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b
  USE m_maxdim, ONLY: maxsat

  IMPLICIT NONE


! Module Variables
! ----------------
  INTEGER(i4b)                                    :: m_stoch ! # sat with pulses
  INTEGER(i4b)                                    :: m_nstcmax ! max number of stc epo/sat
  INTEGER(i4b)                                    :: m_npar ! number of parms w/o stoch.
  INTEGER(i4b), DIMENSION(:),       ALLOCATABLE   :: m_npar_tot ! total # of parms (incl stoch)
  INTEGER(i4b)                                    :: m_nsat ! total number of satellites
  INTEGER(i4b), DIMENSION(:),       ALLOCATABLE   :: m_nstcepo    ! # stoch epochs for each sat
  INTEGER(i4b), DIMENSION(:),       ALLOCATABLE   :: m_satnumbers ! all satellite numbers
  INTEGER(i4b), DIMENSION(:),       ALLOCATABLE   :: m_stcnumbers ! satellite numbers with stoch
  INTEGER(i4b), DIMENSION(:),       ALLOCATABLE   :: m_indtim     ! index for next stoch epoch
  INTEGER(i4b), DIMENSION(:,:),     ALLOCATABLE   :: m_nobs       ! number of observation per int
  INTEGER(i4b), DIMENSION(:),       ALLOCATABLE   :: m_nobs_tot   ! number of observation per int
  INTEGER(i4b), DIMENSION(:),       ALLOCATABLE   :: m_indsat     ! index of stoch sat in external list
  REAL(r8b),    DIMENSION(:,:),     ALLOCATABLE   :: m_stcepo     ! stochastic epochs for each satellite
  REAL(r8b),    DIMENSION(:,:),     ALLOCATABLE   :: m_stcinter   ! stoch. interval boundaries

  REAL(r8b),    DIMENSION(:,:),     ALLOCATABLE   :: m_inipar   ! initial orbit parameters
  REAL(r8b),    DIMENSION(:,:),     ALLOCATABLE   :: m_curpar   ! current orbit parameters
  REAL(r8b),    DIMENSION(:,:),     ALLOCATABLE   :: m_rmspar   ! rms values of parameters
  REAL(r8b),    DIMENSION(:,:,:),   ALLOCATABLE   :: m_dele     ! element changes per subinterval

  REAL(r8b),    DIMENSION(:,:,:),   ALLOCATABLE   :: m_atpa
  REAL(r8b),    DIMENSION(:,:,:),   ALLOCATABLE   :: m_atpb
  REAL(r8b),    DIMENSION(:,:,:,:), ALLOCATABLE   :: m_beta

  REAL(r8b),    DIMENSION(:,:),     ALLOCATABLE   :: m_atpa_tot
  REAL(r8b),    DIMENSION(:,:),     ALLOCATABLE   :: m_atpb_tot
  REAL(r8b),    DIMENSION(:,:),     ALLOCATABLE   :: m_sol_tot
  REAL(r8b),    DIMENSION(:,:),     ALLOCATABLE   :: m_sol_apr
  REAL(r8b),    DIMENSION(:,:),     ALLOCATABLE   :: m_rms
  REAL(r8b),    DIMENSION(:),       ALLOCATABLE   :: m_rms_tot
  REAL(r8b),    DIMENSION(:,:),     ALLOCATABLE   :: m_rpress
  REAL(r8b),    DIMENSION(:,:),     ALLOCATABLE   :: m_rpress_ini
  REAL(r8b),    DIMENSION(:,:),     ALLOCATABLE   :: m_rpress_rms
  REAL(r8b),    DIMENSION(:,:,:),   ALLOCATABLE   :: m_pulse    ! stoc. pulses
  REAL(r8b),    DIMENSION(:,:,:),   ALLOCATABLE   :: m_pulse_rms! RMS of pulses


  TYPE t_stcopt
    INTEGER(i4b)                    :: useStc !  0: none
                                              ! -1: pulses from satcrux
                                              ! >0: number of entries in list
    INTEGER(i4b), DIMENSION(MAXSAT) :: prnstc ! list of sats with pulses
                                              ! 99: all sats.
    REAL(r8b)                       :: interv ! interval for pulses in minutes
    LOGICAL                         :: algstc ! align to inegration intervals
  END TYPE

CONTAINS


! Initialization
! -----------------------------------------------------------------------
  SUBROUTINE stcogn_init(stcopt,nsat,navnum,t0arc,nint,tb12,npar, &
                         nman,satman,timman,nsampl)


! -------------------------------------------------------------------------
! Purpose: The program initializes the analysis of m_nsat = nsat satellites
!          in program ORBGEN in the environment of stochastic parameters.
!          m_stoch = 0: no satellites with stochastic pulses.
!          The following cases are dealt with:
!           - no stochastics
!           - all satellites with stochastics with the same grid
!           - stochastic satellites provided via satcrux file
!
!          m_stoch > 0: m_stoch satellites with stochgastic pulses
!          m_satnumbers(isat), isat=1,m_nsat: satellite numbers (all)
!          m_stcnumbers(isat), isat=1,2,...,m_stoch: corresponding
!          satellite numbers
!          m_nstcepo(isat), isat=1,2,...,m_stoch: number of stochastic epochs for
!                                each stochastic satellite
!          m_indsat(isat), isat=1,2,...,m_sat: index of sat in stc list
!          m_indtim(isat), isat=1,2,...,m_stoch: index for next stochastic
!                                epoch
!          m_stcepo(inter,isat), inter=1,2,..., nstcsat(isat),
!                                isat=1,2,...,m_stoch : stochastic epochs
!          m_stcinter(inter,isat), inter=1,2,..., nstcsat(isat)+2,
!                                isat=1,2,...,m_sat : stochastic intervals
!          m_nstcmax : max. number of stochastic epochs (over all sats)
!          m_pulse(iRSW,iepo,isat): current solution for the pulses
!                                iRSW=1,3: component
! -------------------------------------------------------------------------

! Modules used
! ------------
  USE m_bern,   ONLY: i4b, r8b
  USE d_satcrx, ONLY: gtsatp

  USE s_alcerr
  USE f_modsvn


    IMPLICIT NONE

! Parameters
! ----------
    TYPE(t_stcopt),               INTENT(IN) :: stcopt  ! options from panel
    INTEGER(i4b),                 INTENT(IN) :: nsat    ! number of satellites
    INTEGER(i4b), DIMENSION(:),   INTENT(IN) :: navnum  ! satellite numbers
    INTEGER(i4b),                 INTENT(IN) :: nint    ! number of intervals
    INTEGER(i4b),                 INTENT(IN) :: npar    ! number of parameters (w/o stochastics)
    INTEGER(i4b),                 INTENT(IN) :: nman    ! number of manoeuvres
    INTEGER(i4b), DIMENSION(:),   INTENT(IN) :: satman  ! satellite numbers with manoeuvres
    REAL(r8b),                    INTENT(IN) :: t0arc   ! mjd arc start
    REAL(r8b),    DIMENSION(:),   INTENT(IN) :: tb12    ! time (rel) interval boundaries
    REAL(r8b),    DIMENSION(:),   INTENT(IN) :: timman  ! manoeuvre times
    REAL(r8b),                    INTENT(IN) :: nsampl  ! sampling of the tab/pre file


! Local Variables
! ---------------
  CHARACTER(LEN=11), PARAMETER :: srName = 'stcogn_init'

  INTEGER(i4b)                               :: MAXPLS = 1000  ! return code
  INTEGER(i4b)                               :: NPLS    ! number of pulses
  INTEGER(i4b)                               :: IPLS    ! loop variable
  INTEGER(i4b)                               :: NEWSAT  ! aux variable
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE    :: SATPLS  ! Satellites with pulses
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE    :: SATAUX  ! auxiliary satellite file
  INTEGER(i4b)                               :: irc     ! return code
  INTEGER(i4b)                               :: isat, ksat    ! loop index
  INTEGER(i4b)                               :: istoch  ! approx. number of intervals
  INTEGER(i4b)                               :: inter   ! number of intervals
  INTEGER(i4b)                               :: num_inter  ! number of int. intervals/stoch int.
  INTEGER(i4b)                               :: nstc  ! aux. variable
  INTEGER(i4b)                               :: iman  ! loop variable
  INTEGER(i4b)                               :: iepo  ! loop variable
  INTEGER(i4b)                               :: jepo  ! loop variable
  INTEGER(i4b)                               :: ishift  ! aux. variable
  INTEGER(i4b)                               :: isat_ext, isat_stc, last_pulse ! aux. variable
  REAL(r8b), DIMENSION(:), ALLOCATABLE       :: timpls  ! Pulse epochs

! initialize number of stochastic pulses
  m_nstcmax=0
! save number of satellites,
  m_nsat=nsat
! allocate and fill array with numbers of all satellites
  IF (ALLOCATED(m_satnumbers))  DEALLOCATE(m_satnumbers)
  ALLOCATE(m_satnumbers(m_nsat),stat=irc)
  CALL alcerr(irc,'m_satnumbers',(/m_nsat/),srName)
  DO isat=1,m_nsat
    m_satnumbers(isat) = navnum(isat)
  ENDDO
  m_stoch=0

  IF(stcopt%useStc /= 0)THEN


! initialization when stochastics are set
! ---------------------------------------
    IF(stcopt%usestc == 1.AND.stcopt%prnStc(1) == 99)THEN
      m_stoch = nsat

    ELSEIF(stcopt%usestc > 0)THEN
      m_stoch = stcopt%usestc

! use satcrux-file
! ----------------
    ELSEIF(stcopt%usestc == -1)THEN

      IF (ALLOCATED(satpls))  DEALLOCATE(satpls)
      ALLOCATE(satpls(MAXPLS),stat=irc)
      CALL alcerr(irc,'satpls',(/maxpls/),srName)
      IF (ALLOCATED(timpls))  DEALLOCATE(timpls)
      ALLOCATE(timpls(MAXPLS),stat=irc)
      CALL alcerr(irc,'timpls',(/maxpls/),srName)
      IF (ALLOCATED(sataux))  DEALLOCATE(sataux)
      ALLOCATE(sataux(MAXPLS),stat=irc)
      CALL alcerr(irc,'sataux',(/maxpls/),srName)

! Get individual pulses
      CALL gtsatp(MAXPLS,NPLS,SATPLS,TIMPLS)

! Adapt satellite number in case of maneuvers
      DO ipls=1,npls
        IF(satpls(ipls) == modsvn(satpls(ipls)) .AND. &
           timpls(ipls)-t0arc >= tb12(1) .AND. &
           timpls(ipls)-t0arc <= tb12(nint+1)) THEN
          DO iman = 1,nman
            IF(satman(iman) == satpls(ipls)  .AND. &
               timman(iman)-t0arc >= tb12(1) .AND. &
               timman(iman)-t0arc <= tb12(nint+1) .AND. &
               timpls(ipls) > timman(iman)) THEN
              satpls(ipls) = satpls(ipls)+50
              EXIT
            ENDIF
          ENDDO
        ENDIF
      ENDDO


! get number of manoeuvre satellites
      m_stoch=0
      do ipls=1,npls
        newsat=1
        do isat=1,m_stoch
          if(satpls(ipls) == sataux(isat)  .AND. &
             timpls(ipls)-t0arc >= tb12(1)+3d0/864d0 .AND. &
             timpls(ipls)-t0arc <= tb12(nint+1)-3d0/864d0)then
            newsat=0
            exit
          endif
        enddo
        if(newsat == 1)then
          m_stoch=m_stoch+1
          sataux(m_stoch)=satpls(ipls)
        endif
      enddo
!!!      write(*,*)'npls=',npls,m_stoch,sataux(1:m_stoch)
! allocate arrays m_nstcepo, m_stcnumbers,
      IF (ALLOCATED(m_nstcepo))  DEALLOCATE(m_nstcepo)
      ALLOCATE(m_nstcepo(m_stoch),stat=irc)
      CALL alcerr(irc,'m_nstcepo',(/m_stoch/),srName)
      IF (ALLOCATED(m_stcnumbers))  DEALLOCATE(m_stcnumbers)
      ALLOCATE(m_stcnumbers(m_stoch),stat=irc)
      CALL alcerr(irc,'m_stcnumbers',(/m_stoch/),srName)
! fill in array m_stcnumbers
      do isat=1,m_stoch
        m_stcnumbers(isat) = sataux(isat)
      enddo
!
      DEALLOCATE(sataux)
! set array m_nstcepo, define m_nstcmax
      m_nstcmax=0
      do isat=1,m_stoch
        m_nstcepo(isat) = 0
        do ipls = 1,npls
          if(m_stcnumbers(isat) == satpls(ipls) .AND. &
             timpls(ipls)-t0arc > tb12(1)+3d0/864d0 .AND. &
             timpls(ipls)-t0arc < tb12(nint+1)-3d0/864d0)then
            m_nstcepo(isat) = m_nstcepo(isat) + 1
            if(m_nstcepo(isat) > m_nstcmax)m_nstcmax = m_nstcepo(isat)
          endif
        enddo
      enddo
! define array m_stcepo
      IF (ALLOCATED(m_stcepo))  DEALLOCATE(m_stcepo)
      ALLOCATE(m_stcepo(m_nstcmax,m_stoch),stat=irc)
      CALL alcerr(irc,'m_stcepo',(/m_nstcmax,m_stoch/),srName)
      do istoch=1,m_stoch
        iepo=0
        do ipls=1,npls
          if(m_stcnumbers(istoch) == satpls(ipls) .AND. &
             timpls(ipls)-t0arc > tb12(1)+3d0/864d0 .AND. &
             timpls(ipls)-t0arc < tb12(nint+1)-3d0/864d0)then
            iepo=iepo+1
            m_stcepo(iepo,istoch) = timpls(ipls)-t0arc
          endif
        enddo
      enddo
! deallocate arrays from sr GTSATP
      DEALLOCATE(satpls)
      DEALLOCATE(timpls)
    ENDIF

! set corresponding arrays, if pulses are NOT defined via satcrux-file
! ********************************************************************
    if(stcopt%usestc /= -1)then
! allocate and fill array with numbers of stochastic satellites
      IF (ALLOCATED(m_stcnumbers))  DEALLOCATE(m_stcnumbers)
      ALLOCATE(m_stcnumbers(m_stoch),stat=irc)
      CALL alcerr(irc,'m_stcnumbers',(/m_stoch/),srName)

! special satellite list or all satellites
      m_stcnumbers(1:stcopt%usestc) = stcopt%prnStc(1:stcopt%usestc)
      IF(stcopt%prnStc(1) == 99)THEN
        m_stcnumbers(1:nsat) = navnum(1:nsat)
      ENDIF

! allocate number of stochastic epochs
      IF (ALLOCATED(m_nstcepo))  DEALLOCATE(m_nstcepo)
      ALLOCATE(m_nstcepo(m_stoch),stat=irc)
      CALL alcerr(irc,'m_nstcepo',(/m_stoch/),srName)
!

! define number of stochastic intervals for each satellite
! (currently all identical)
      IF(stcopt%algStc)THEN
!
! allign stochastic epochs with interval boundaries
        num_inter=dnint(  stcopt%interv/( (tb12(nint+1)-tb12(1))*1440.d0/nint )  )
        if(num_inter == 0) num_inter=1
! allocate arrays for stochastic epochs, fill array
        m_nstcmax=nint/num_inter
        if(tb12(nint+1) > m_nstcmax*stcopt%interv/1440.)m_nstcmax=m_nstcmax+1
        if(m_nstcmax*num_inter == nint)m_nstcmax=m_nstcmax-1
!
        IF (ALLOCATED(m_stcepo))  DEALLOCATE(m_stcepo)
        ALLOCATE(m_stcepo(m_nstcmax,m_stoch),stat=irc)
        CALL alcerr(irc,'m_stcepo',(/m_nstcmax,m_stoch/),srName)
        DO inter = 1,m_nstcmax
          m_stcepo(inter,1:m_stoch) = tb12(num_inter*inter+1)
        ENDDO
! number of stochastic epochs for each satellite
        DO isat=1,m_stoch
          m_nstcepo(isat)=m_nstcmax
        ENDDO
! Check for maneuvers
        DO isat=1,m_stoch
          jepo = 0
          DO iepo=1,m_nstcepo(isat)
            jepo=jepo+1
            DO iMan = 1,nMan
              IF (satman(iman) == m_stcnumbers(isat)                      .AND.&
                  timman(iman)-nsampl/86400d0 <= t0arc+m_stcepo(iepo,isat).AND.&
                  timman(iman) >  t0arc+tb12(1)) THEN
                jepo=jepo-1
              ELSE IF (satman(iman) == modsvn(m_stcnumbers(isat))         .AND.&
                  timman(iman)+nsampl/86400d0 >= t0arc+m_stcepo(iepo,isat).AND.&
                  timman(iman) <  t0arc+tb12(nint+1)) THEN
                IF ( m_stcnumbers(isat) /= modsvn(m_stcnumbers(isat)) ) THEN
                  jepo=jepo-1
                ENDIF
              ENDIF
            ENDDO
            IF (jepo > 0 .AND. jEpo /= iEpo .AND. &
                m_stcnumbers(isat) /= modsvn(m_stcnumbers(isat)) ) THEN
              m_stcepo(jepo,isat) = m_stcepo(iepo,isat)
            ENDIF
          ENDDO
          m_nstcepo(isat) = jepo
        ENDDO
      ELSE
!
! do NOT align stochastic epochs with interval boundaries
! >>>> IT DOES NOT WORK: stcopt%algStc = .TRUE. in RDORBI <<<<
        m_nstcmax = (tb12(nint+1)-tb12(1))/(stcopt%interv/1440.d0)+1d0
        do inter=m_nstcmax,1,-1
          if(inter*stcopt%interv/1440 < tb12(nint+1)-30./1440.)exit
        enddo
        m_nstcmax=inter
        do isat=1,m_stoch
          m_nstcepo(isat)=m_nstcmax
        enddo
! allocate arrays for stochastic epochs, fill array
        IF (ALLOCATED(m_stcepo))  DEALLOCATE(m_stcepo)
        ALLOCATE(m_stcepo(m_nstcmax,m_stoch),stat=irc)
        CALL alcerr(irc,'m_stcepo',(/m_nstcmax,m_stoch/),srName)
        do istoch=1,m_nstcmax
          m_stcepo(istoch,1:m_stoch) = istoch*(stcopt%interv/1440.d0)
        enddo
      ENDIF
! set corresponding arrays, if pulses are NOT defined via satcrux-file -- end
! ***************************************************************************
    ENDIF

! allocate partial normal equation systems
    IF (ALLOCATED(m_atpa))  DEALLOCATE(m_atpa)
    IF (ALLOCATED(m_atpb))  DEALLOCATE(m_atpb)
    IF (ALLOCATED(m_beta))  DEALLOCATE(m_beta)
    IF (ALLOCATED(m_rms))   DEALLOCATE(m_rms)
    IF (ALLOCATED(m_nobs))  DEALLOCATE(m_nobs)
    IF (ALLOCATED(m_rms_tot)) DEALLOCATE(m_rms_tot)
    IF (ALLOCATED(m_nobs_tot)) DEALLOCATE(m_nobs_tot)
    IF (ALLOCATED(m_npar_tot)) DEALLOCATE(m_npar_tot)
    IF (ALLOCATED(m_indsat)) DEALLOCATE(m_indsat)
    IF (ALLOCATED(m_pulse)) DEALLOCATE(m_pulse)
    IF (ALLOCATED(m_pulse_rms)) DEALLOCATE(m_pulse_rms)

    ALLOCATE(m_atpa(npar*(npar+1)/2,m_nstcmax+1,m_nsat),stat=irc)
    CALL alcerr(irc,'m_atpa',(/npar*(npar+1)/2,m_nstcmax+1,m_nsat/),srName)
    ALLOCATE(m_atpb(npar,m_nstcmax+1,m_nsat),stat=irc)
    CALL alcerr(irc,'m_atpb',(/npar,m_nstcmax+1,m_nsat/),srName)
    ALLOCATE(m_beta(6,3,m_nstcmax+1,m_stoch),stat=irc)
    CALL alcerr(irc,'m_beta',(/6,3,m_nstcmax+1,m_stoch/),srName)
    ALLOCATE(m_rms (m_nstcmax+1,m_nsat),stat=irc)
    CALL alcerr(irc,'m_rms',(/m_nstcmax+1,m_nsat/),srName)
    ALLOCATE(m_nobs(m_nstcmax+1,m_nsat),stat=irc)
    CALL alcerr(irc,'m_nobs',(/m_nstcmax+1,m_nsat/),srName)
    ALLOCATE(m_rms_tot (m_nsat),stat=irc)
    CALL alcerr(irc,'m_rms_tot',(/m_nsat/),srName)
    ALLOCATE(m_nobs_tot(m_nsat),stat=irc)
    CALL alcerr(irc,'m_nobs_tot',(/m_nsat/),srName)
    ALLOCATE(m_npar_tot(m_nsat),stat=irc)
    CALL alcerr(irc,'m_npar_tot',(/m_nsat/),srName)
    ALLOCATE(m_indsat(m_nsat),stat=irc)
    CALL alcerr(irc,'m_indsat',(/m_nsat/),srName)
    ALLOCATE(m_pulse(3,m_nstcmax,m_nsat),stat=irc)
    CALL alcerr(irc,'m_pulse',(/3,m_nstcmax,m_nsat/),srName)
    ALLOCATE(m_pulse_rms(3,m_nstcmax,m_nsat),stat=irc)
    CALL alcerr(irc,'m_pulse_rms',(/3,m_nstcmax,m_nsat/),srName)
    m_atpa=0
    m_atpb=0
    m_beta=0
    m_rms=0
    m_nobs=0
    m_rms_tot=0
    m_nobs_tot=0
    m_indsat=0
    m_npar=npar
    m_pulse = 0d0
    m_pulse_rms = 0d0
! allocate and fill array with stochastic interval boundaries
! for all satellites (also for those w/o stochastics)
! define array m_indsat
    IF (ALLOCATED(m_stcinter))  DEALLOCATE(m_stcinter)
    ALLOCATE(m_stcinter(m_nstcmax+2,m_nsat),stat=irc)
    CALL alcerr(irc,'m_stcinter',(/m_nstcmax+2,m_nsat/),srName)
    do isat = 1,m_nsat
      m_indsat(isat)=0
      nstc=0
      DO ksat=1,m_stoch
        IF(m_stcnumbers(ksat) == m_satnumbers(isat))THEN
          m_indsat(isat)=ksat
          nstc=m_nstcepo(ksat)
          exit
        ENDIF
      ENDDO
!!      write(*,*)'isat,m_stoch,m_nstcepo=',isat, m_stoch, m_nstcepo(isat)
      if(nstc > 0)then
        m_stcinter(1,isat) = tb12(1)
        m_stcinter(m_nstcepo(ksat)+2,isat) = tb12(nint+1)
        do inter=1,nstc
          m_stcinter(inter+1,isat) = m_stcepo(inter,ksat)
        enddo
      else
        m_stcinter(1,isat) = tb12(1)
        m_stcinter(2,isat) = tb12(nint+1)
      endif
    enddo
!
! initialization of array with next stochastic epoch
    IF (ALLOCATED(m_indtim))  DEALLOCATE(m_indtim)
    ALLOCATE(m_indtim(m_stoch),stat=irc)
    CALL alcerr(irc,'m_indtim',(/m_stoch/),srName)
    m_indtim = 1
!
! initialize array dele
    IF (ALLOCATED(m_dele))  DEALLOCATE(m_dele)
    ALLOCATE(m_dele(6,m_nstcmax+1,m_nsat),stat=irc)
    CALL alcerr(irc,'m_dele',(/6,m_nstcmax+1,m_nsat/),srName)
!
! eliminate pulses without observations for manoeuvre satellites
! --------------------------------------------------------------
    DO isat=1,nman
! manoeuvre in time window
      IF(timman(isat) - t0arc < tb12(1) .or. &
         timman(isat) - t0arc > tb12(nint+1))THEN
        cycle
      ENDIF
! original manoeuvre satellite in list of all satellites with stoch
      isat_stc = 0
      do ksat=1,m_stoch
        if(satman(isat) == m_stcnumbers(ksat))then
          isat_stc=ksat
          exit
        endif
      enddo
      if(isat_stc == 0)cycle

! manoeuvre satellite in list of all external satellites
      isat_ext = 0
      do ksat=1,m_nsat
        if(satman(isat) == m_satnumbers(ksat))then
          isat_ext=ksat
          exit
        endif
      enddo
      if(isat_ext == 0)cycle
! find last stochastic epoch for obs before manoeuvre
      last_pulse =0
      do iepo=1,m_nstcepo(isat_stc)
        if(timman(isat) -t0arc < m_stcepo(iepo,isat_stc))then
          last_pulse=iepo-1
          exit
        endif
      enddo
      if(last_pulse == 0) cycle

      m_stcinter(last_pulse+2,isat_ext) = m_stcinter(m_nstcepo(isat_stc)+2,isat_ext)
      m_nstcepo(isat_stc) = last_pulse
! handle observations after manoeuvre
! original manoeuvre satellite in list of all satellites with stoch
      isat_stc = 0
      do ksat=1,m_stoch
        if(satman(isat) + 50 == m_stcnumbers(ksat))then
          isat_stc=ksat
          exit
        endif
      enddo
      if(isat_stc == 0)cycle

! manoeuvre satellite in list of all external satellites
      isat_ext = 0
      do ksat=1,m_nsat
        if(satman(isat) + 50 == m_satnumbers(ksat))then
          isat_ext=ksat
          exit
        endif
      enddo
      if(isat_ext == 0)cycle
! eliminate pulses before manoeuvre
      ishift = 0
      do iepo = 1, m_nstcepo(isat_stc)
        if(timman(isat) - t0arc + 1.d0/24.d0 > m_stcepo(iepo,isat_stc))then
          ishift=ishift + 1
        endif
      enddo
      if(ishift > 0)then
        do iepo = 1, m_nstcepo(isat_stc)-ishift
          m_stcepo(iepo,isat_stc) = m_stcepo(iepo+ishift,isat_stc)
          m_stcinter(iepo+1,isat_ext) = m_stcepo(iepo,isat_stc)
        enddo
        m_stcinter(m_nstcepo(isat_stc)-ishift+2,isat_ext) = m_stcinter(m_nstcepo(isat_stc)+2,isat_ext)
        m_stcinter(m_nstcepo(isat_stc)-ishift+3:m_nstcepo(isat_stc)+2,isat_ext) = 0
        m_nstcepo(isat_stc) = m_nstcepo(isat_stc) - ishift
      endif
    ENDDO
  ELSE

! ------------------------------------------
! initialization when no stochastics are set
! ------------------------------------------
    m_stoch=0
    IF (ALLOCATED(m_stcinter))  DEALLOCATE(m_stcinter)
    ALLOCATE(m_stcinter(2,m_nsat),stat=irc)
    CALL alcerr(irc,'m_stcinter',(/2,m_nsat/),srName)
    do isat = 1,m_nsat
      m_stcinter(1,isat) = tb12(1)
      m_stcinter(2,isat) = tb12(nint+1)
    enddo
! allocate partial normal equation systems
    IF (ALLOCATED(m_atpa))  DEALLOCATE(m_atpa)
    IF (ALLOCATED(m_atpb))  DEALLOCATE(m_atpb)
    IF (ALLOCATED(m_rms))   DEALLOCATE(m_rms)
    IF (ALLOCATED(m_nobs))   DEALLOCATE(m_nobs)
    IF (ALLOCATED(m_rms_tot)) DEALLOCATE(m_rms_tot)
    IF (ALLOCATED(m_nobs_tot)) DEALLOCATE(m_nobs_tot)
    IF (ALLOCATED(m_npar_tot)) DEALLOCATE(m_npar_tot)
    IF (ALLOCATED(m_indsat)) DEALLOCATE(m_indsat)
    ALLOCATE(m_atpa(npar*(npar+1)/2,1,m_nsat),stat=irc)
    CALL alcerr(irc,'m_atpa',(/npar*(npar+1)/2,1,m_nsat/),srName)
    ALLOCATE(m_atpb(npar,1,m_nsat),stat=irc)
    CALL alcerr(irc,'m_atpb',(/npar,1,m_nsat/),srName)
    ALLOCATE(m_rms (m_nstcmax+1,m_nsat),stat=irc)
    CALL alcerr(irc,'m_rms',(/m_nstcmax+1,m_nsat/),srName)
    ALLOCATE(m_nobs(m_nstcmax+1,m_nsat),stat=irc)
    CALL alcerr(irc,'m_nobs',(/m_nstcmax+1,m_nsat/),srName)
    ALLOCATE(m_rms_tot (m_nsat),stat=irc)
    CALL alcerr(irc,'m_rms_tot',(/m_nsat/),srName)
    ALLOCATE(m_npar_tot(m_nsat),stat=irc)
    CALL alcerr(irc,'m_npar_tot',(/m_nsat/),srName)
    ALLOCATE(m_nobs_tot(m_nsat),stat=irc)
    CALL alcerr(irc,'m_nobs_tot',(/m_nsat/),srName)
    ALLOCATE(m_indsat(m_nsat),stat=irc)
    CALL alcerr(irc,'m_indsat',(/m_nsat/),srName)
    m_atpa=0
    m_atpb=0
    m_rms=0
    m_nobs=0
    m_rms_tot=0
    m_nobs_tot=0
    m_indsat=0
    m_npar=npar
  ENDIF

!initialize inial and current parameters, rms values
  IF (ALLOCATED(m_inipar)) DEALLOCATE(m_inipar)
  ALLOCATE(m_inipar(m_npar+3*m_nstcmax,m_nsat),stat=irc)
  CALL alcerr(irc,'m_inipar',(/m_npar+3*m_nstcmax,m_nsat/),srName)

  IF (ALLOCATED(m_curpar)) DEALLOCATE(m_curpar)
  ALLOCATE(m_curpar(m_npar+3*m_nstcmax,m_nsat),stat=irc)
  CALL alcerr(irc,'m_curpar',(/m_npar+3*m_nstcmax,m_nsat/),srName)

  IF (ALLOCATED(m_rmspar)) DEALLOCATE(m_rmspar)
  ALLOCATE(m_rmspar(m_npar+3*m_nstcmax,m_nsat),stat=irc)
  CALL alcerr(irc,'m_rmspar',(/m_npar+3*m_nstcmax,m_nsat/),srName)

  END SUBROUTINE stcogn_init


  SUBROUTINE stcogn_inipar(iter,maxvar,isat,locq,elesat,rpress)

! -------------------------------------------------------------------------
! Purpose: The sr defines the initial osculating elements, rpr parameters,
!          and stochastic parameters
!          It defines moreover the element changes for sr stcogn_applyPos
! -------------------------------------------------------------------------
    USE m_bern,   ONLY: i4b, r8b
    USE s_alcerr

    IMPLICIT NONE

! Parameters
! ----------
    INTEGER(i4b),                  INTENT(IN) :: iter     ! iteration number
    INTEGER(i4b),                  INTENT(IN) :: maxvar     ! max number of dyn parameters
    INTEGER(i4b),                  INTENT(IN) :: isat     ! satellite number
    INTEGER(i4b), DIMENSION(:,:),  INTENT(IN) :: locq     ! parameter identifier
    REAL(r8b), DIMENSION(:),       INTENT(IN) :: elesat   ! initial osculating elements
    REAL(r8b), DIMENSION(:),       INTENT(INOUT) :: rpress   ! radiation pressure initial values
! local parameters
    CHARACTER(LEN=13), PARAMETER :: srName = 'stcogn_inipar'

    INTEGER(i4b)                              :: ksat     ! loop index
    INTEGER(i4b)                              :: ipar     ! loop index
    INTEGER(i4b)                              :: int      ! loop index
    INTEGER(i4b)                              :: istoch   ! loop index
    INTEGER(i4b)                              :: irc      ! return code
    INTEGER(i4b)                              :: isat_stc ! aux variable



! first iteration step
! ********************
    IF(iter == 1)then
! osculating elements
! -------------------
      m_inipar(1:6,isat) = elesat(1:6)
! dynamical parameters
! --------------------
      do ipar=7,m_npar
        m_inipar(ipar,isat) = rpress(locq(4,ipar))
      enddo
! store radiation pressure parameters in module
! ---------------------------------------------
      if(isat == 1)then
        IF (ALLOCATED(m_rpress))  DEALLOCATE(m_rpress)
        ALLOCATE(m_rpress(maxvar,m_nsat),stat=irc)
        CALL alcerr(irc,'m_rpress',(/maxvar,m_nsat/),srName)
        m_rpress = 0
        IF (ALLOCATED(m_rpress_ini))  DEALLOCATE(m_rpress_ini)
        ALLOCATE(m_rpress_ini(maxvar,m_nsat),stat=irc)
        CALL alcerr(irc,'m_rpress_ini',(/maxvar,m_nsat/),srName)
        m_rpress_ini = 0
        IF (ALLOCATED(m_rpress_rms))  DEALLOCATE(m_rpress_rms)
        ALLOCATE(m_rpress_rms(maxvar,m_nsat),stat=irc)
        CALL alcerr(irc,'m_rpress_rms',(/maxvar,m_nsat/),srName)
        m_rpress_rms = 0
      endif
!
      m_rpress(1:maxvar,isat)=rpress(1:maxvar)
      m_rpress_ini(1:maxvar,isat)=m_rpress(1:maxvar,isat)

! stochastic parameters
! ---------------------
      isat_stc=0
      do ksat=1,m_stoch
        if(m_satnumbers(isat) == m_stcnumbers(ksat))then
          isat_stc=ksat
          exit
        endif
      enddo
      if(isat_stc > 0)then
        m_npar_tot(isat) = m_npar+3*m_nstcepo(isat_stc)
      else
        m_npar_tot(isat) = m_npar
      endif
!
      do ipar=m_npar+1, m_npar_tot(isat)
        m_inipar(ipar,isat)=0
      enddo
    ELSE
! subsequent iteration steps (iter > 1)
! *************************************
      m_inipar(1:m_npar_tot(isat),isat) = m_curpar(1:m_npar_tot(isat),isat)

! initialize arrays for next iterations
! -------------------------------------
      IF( isat ==1 )THEN
        m_atpa = 0
        m_atpb = 0
        m_rms  = 0
        m_nobs = 0
      ENDIF
!
! set new values for radiation pressure
! -------------------------------------
      m_rpress_ini(1:maxvar,isat)=m_rpress(1:maxvar,isat)
      rpress(1:maxvar)=m_rpress(1:maxvar,isat)
!
! define ele changes for stochastic intervals
! -------------------------------------------
      istoch=m_indsat(isat)
      IF(istoch > 0)THEN
        m_dele(:,:,isat)=0.d0
!
! find satellite in stochastic list
! ---------------------------------

        DO int=1,(m_npar_tot(isat)-m_npar)/3
          m_dele(1:6,int+1,isat) = m_dele(1:6,int,isat) + &
                                 m_curpar(m_npar+3*(int-1)+1,isat)*m_beta(1:6,1,int,istoch) + &
                                 m_curpar(m_npar+3*(int-1)+2,isat)*m_beta(1:6,2,int,istoch) + &
                                 m_curpar(m_npar+3*(int-1)+3,isat)*m_beta(1:6,3,int,istoch)
        ENDDO

      ENDIF
    ENDIF


  END SUBROUTINE stcogn_inipar




  SUBROUTINE stcogn_update(isat_ext,tobs,drms,danor,dbnor)

! -------------------------------------------------------------------------
! Purpose: The SR updates the neq system for the satellite isat_ext
!          with the pseudo observations of one epoch. The neq-
!          contribution is referred to the correct subinterval.
! -------------------------------------------------------------------------

    USE m_bern,  ONLY: i4b, r8b

    IMPLICIT NONE

    INTEGER(i4b),                  INTENT(IN) :: isat_ext ! satellite number
    REAL(r8b),                     INTENT(IN) :: tobs     ! observation time
    REAL(r8b),                     INTENT(IN) :: drms     ! rms update
    REAL(r8b), DIMENSION(:),       INTENT(IN) :: danor    ! observation time
    REAL(r8b), DIMENSION(:),       INTENT(IN) :: dbnor    ! observation time

! Local Variables
! ---------------
  INTEGER(i4b)                                :: isat_stc    ! sat number in stoch list

  INTEGER(i4b)                                :: inter       ! loop index
  INTEGER(i4b), SAVE                          :: int_old = 0 ! loop index
  INTEGER(i4b)                                :: int_act     ! current interval nr

! get satellite index in list of stochastic satellites
    isat_stc=m_indsat(isat_ext)
! subinterval number
    IF(isat_stc == 0)THEN
      int_act = 1
    ELSE
!
! get current stochastic interval number
      int_act=0
      IF(int_old /= 0) THEN
        IF (tobs >= m_stcinter(int_old,isat_ext)   .and. &
            tobs <= m_stcinter(int_old+1,isat_ext)) int_act = int_old
      ENDIF
      IF (int_act == 0) THEN
        DO inter=1,m_nstcepo(isat_stc)+1
          IF( ( inter == 1 .AND. &
                DABS(tobs-m_stcinter(inter,isat_ext))   < 1d-11 ) .OR. &
              ( inter == m_nstcepo(isat_stc)+1 .AND. &
                DABS(tobs-m_stcinter(inter+1,isat_ext)) < 1d-11 ) .OR. &
              ( tobs >= m_stcinter(inter,isat_ext) .AND. &
                tobs <= m_stcinter(inter+1,isat_ext )           ) ) THEN
             int_act = inter
             int_old=int_act
             exit
          ENDIF
        ENDDO
      ENDIF
    ENDIF
! update neq-system
  m_rms(int_act,isat_ext) = m_rms(int_act,isat_ext) + drms
  m_nobs(int_act,isat_ext) = m_nobs(int_act,isat_ext) + 3
  m_atpb(1:m_npar,int_act,isat_ext) = m_atpb(1:m_npar,int_act,isat_ext) + dbnor(1:m_npar)
  m_atpa(1:m_npar*(m_npar+1)/2,int_act,isat_ext)= &
                               m_atpa(1:m_npar*(m_npar+1)/2,int_act,isat_ext)+ &
                                                  danor(1:m_npar*(m_npar+1)/2)

  END SUBROUTINE stcogn_UPDATE

  SUBROUTINE stcogn_beta(maxq1,svn,ta,tb,q,h,nvar,fac,ycoe,zcoe)

! -------------------------------------------------------------------------
! Purpose: The SR sets the matrix beta for all satellites for one
!          stochastic epoch.
!          See Beutler et al. JoG (2006) 80, 353-372
! -------------------------------------------------------------------------

    USE m_bern, ONLY: i4b, r8b

    USE s_ypol
    USE s_vprod
    USE s_dminv

    IMPLICIT NONE

! Parameters
! ----------

    INTEGER(i4b),                  INTENT(IN) :: maxq1  ! Max. polynomial degree
    INTEGER(i4b),                  INTENT(IN) :: svn    ! satellite number
    INTEGER(i4b),                  INTENT(IN) :: q      ! order of integration (polynomials)
    INTEGER(i4b),                  INTENT(IN) :: nvar   ! number of variationa equations
    REAL(r8b),                     INTENT(IN) :: ta, tb ! current interval boundaries
    REAL(r8b),                     INTENT(IN) :: h      ! scaling factor for time argument
    REAL(r8b), DIMENSION(:),       INTENT(IN) :: fac    ! polynomial coeff. (ref orbit)
    REAL(r8b), DIMENSION(3,maxq1,*)           :: ycoe   ! polynomial coeff. (ref orbit)
    REAL(r8b), DIMENSION(:,:)                 :: zcoe   ! polynomial coefficients (partials)

! local variables
    INTEGER(i4b)                              :: isat      ! loop index
    INTEGER(i4b)                              :: istoch    ! sat number in stoch list
    INTEGER(i4b)                              :: ii, kk    ! loop index
    INTEGER(i4b)                              :: ind_tim   ! aux.variable
    INTEGER(i4b), DIMENSION(6)                :: lll, mmm  ! loop index
    REAL(r8b)                                 :: t_curr    ! current stochastic epoch
    REAL(r8b)                                 :: tsec      ! current stochastic epoch
    REAL(r8b)                                 :: det       ! aux. variable
    REAL(r8b), DIMENSION(6)                   :: xsat      ! position and velocity
    REAL(r8b), DIMENSION(3)                   :: e_r, e_v, e_s, e_w ! unit vectors
    REAL(r8b), DIMENSION(3*2*nvar)            :: drvdp  ! partials w.r.t. osc ele and dyn parms
    REAL(r8b), DIMENSION(3,6)                 :: drdele ! partials w.r.t. osc ele and dyn parms
    REAL(r8b), DIMENSION(3,6)                 :: dvdele ! partials w.r.t. osc ele and dyn parms
    REAL(r8b), DIMENSION(6,6)                 :: mat    ! cond. matrix

! no stochastics?
    IF(m_stoch ==0)THEN
      return
    ENDIF
    istoch=0
    DO isat=1,m_stoch
      IF(svn == m_stcnumbers(isat))THEN
        istoch=isat
        exit
      ENDIF
    ENDDO
    IF(istoch == 0)THEN
      return
    ENDIF
    ind_tim=m_indtim(istoch)
!
    IF(ind_tim > m_nstcepo(istoch))THEN
      return
    ENDIF

! satellite found.
! current stochastic epoch in interval?
!!    write(*,*)'ind_tim,istoch=',ind_tim,istoch
    IF(m_stcepo(ind_tim,istoch) >= ta .AND. &
       m_stcepo(ind_tim,istoch) <= tb)THEN
      t_curr = m_stcepo(m_indtim(istoch),istoch)
! compute position and velocity
      tsec=(t_curr-ta)*86400.D0
      DO isat=1,m_nsat
! find satellite index in external list
        IF(m_stcnumbers(istoch) == m_satnumbers(isat))THEN
          CALL ypol(1,q,3,h,fac,tsec,ycoe(1,1,isat),xsat)
          CALL ypol(1,q,3*nvar,h,fac,tsec,zcoe(:,isat),drvdp)
          exit
        ENDIF
      ENDDO
! unit vector (radial direction)
      e_r(1:3) = xsat(1:3) / SQRT(DOT_PRODUCT(xsat(1:3),xsat(1:3)))
      e_v(1:3) = xsat(4:6) / SQRT(DOT_PRODUCT(xsat(4:6),xsat(4:6))) ! unit vector (tangential)
      call vprod(e_r,e_v,e_w)
      e_w(1:3) = e_w(1:3)/SQRT(DOT_PRODUCT(e_w(1:3),e_w(1:3)))
      call vprod(e_w,e_r,e_s)

! compute partials
      DO ii=1,3
        DO kk=1,6
          DRDELE(ii,kk)=drvdp((KK-1)*3+II)
          DVDELE(ii,kk)=drvdp(3*NVAR+(KK-1)*3+II)
        ENDDO
      ENDDO
! compute matrix beta for current stochastic epoch (with pulses in R, S, W)
      mat(1:3,1:6) = drdele(1:3,1:6)
      mat(4:6,1:6) = dvdele(1:3,1:6)

      call DMINV(mat,6,det,lll,mmm)

      do ii =1,6
        m_beta(ii,1:3,ind_tim,istoch) = 0
        do kk = 1,3
          m_beta(ii,1,ind_tim,istoch) = m_beta(ii,1,ind_tim,istoch) + mat(ii,3+kk)*e_r(kk)
          m_beta(ii,2,ind_tim,istoch) = m_beta(ii,2,ind_tim,istoch) + mat(ii,3+kk)*e_s(kk)
          m_beta(ii,3,ind_tim,istoch) = m_beta(ii,3,ind_tim,istoch) + mat(ii,3+kk)*e_w(kk)
        enddo
      enddo
      m_indtim(istoch) = m_indtim(istoch) + 1
    ENDIF

  END SUBROUTINE stcogn_beta


  SUBROUTINE stcogn_solve(iter,locq, elesat, rpress)

! -------------------------------------------------------------------------
! Purpose: The SR solves all neq-systems for a particular iteration step
!          The parameters a priori, the estimated parameters and their
!          mean errors are available
! -------------------------------------------------------------------------

    USE m_bern,  ONLY: i4b, r8b, lfnerr

    USE f_ikf
    USE s_syminvg
    USE s_alcerr

    IMPLICIT NONE

! Parameters
! ----------
    INTEGER(i4b),                  INTENT(IN) :: iter     ! iteration number
    INTEGER(i4b), DIMENSION(:,:),  INTENT(IN) :: locq     ! parameter description
    REAL(r8b), DIMENSION(:,:),     INTENT(IN) :: elesat   ! initial osculating elements
    REAL(r8b), DIMENSION(:,:),     INTENT(IN) :: rpress   ! radiation pressure parameters

! local parameters
! ----------------
    CHARACTER(LEN=12), PARAMETER    :: srName = 'stcogn_solve'
    REAL(r8b),         PARAMETER,    &                    ! Constraints if too
                       DIMENSION(3) :: sigma = &          ! few obs. are avail.
                       (/ 1D-10,    &                     ! RPR, const.
                          1D-12,    &                     ! RPR, once-per-rev.
                          1D-08 /)                        ! stoch. pulses

    INTEGER(i4b), SAVE                       :: iter_old=0 ! previous iteration number
    INTEGER(i4b)                             :: max_npar   ! max number of parms (inc. stoch.)
    INTEGER(i4b)                             :: irc        ! return code
    INTEGER(i4b)                             :: isat       ! Loop index
    INTEGER(i4b)                             :: iepo       ! Loop index
    INTEGER(i4b)                             :: int, kint  ! loop index
    INTEGER(i4b)                             :: ii, kk, ll ! loop index
    INTEGER(i4b)                             :: il         ! loop index
    INTEGER(i4b)                             :: iikk, llkk ! loop index
    INTEGER(i4b)                             :: help       ! loop index
    INTEGER(i4b)                             :: iPar, jPar ! loop index
    INTEGER(i4b)                             :: isat_stc   ! corresp. index in stc-arrays
    INTEGER(i4b)                             :: nint       ! number of sub-intervals (stoch)
    INTEGER(i4b)                             :: npnp       ! aux. variable
    INTEGER(i4b)                             :: npar_tot   ! total number of parameters
    INTEGER(i4b)                             :: nsing      ! aux. parameter
    INTEGER(i4b),DIMENSION(:), ALLOCATABLE   :: parflg     ! aux. matrix
    REAL(r8b), DIMENSION(:,:), ALLOCATABLE   :: NB         ! aux. matrix
!    REAL(r8b), DIMENSION(:),   ALLOCATABLE   :: scl_tot    ! scale parameters
    REAL(r8b)                                :: sngLim     ! aux. variable

! allocate total neq-systems
! --------------------------
    IF(iter /=iter_old)THEN
      max_npar=m_npar+3*m_nstcmax
      IF (ALLOCATED(m_atpa_tot))  DEALLOCATE(m_atpa_tot)
      IF (ALLOCATED(m_atpb_tot))  DEALLOCATE(m_atpb_tot)
      IF (ALLOCATED(m_sol_tot))   DEALLOCATE(m_sol_tot)

      ALLOCATE(m_atpa_tot(max_npar*(max_npar+1)/2,m_nsat),stat=irc)
      CALL alcerr(irc,'m_atpa_tot',(/max_npar*(max_npar+1)/2,m_nsat/),srName)
      ALLOCATE(m_atpb_tot(max_npar,m_nsat),stat=irc)
      CALL alcerr(irc,'m_atpb_tot',(/max_npar,m_nsat/),srName)
      ALLOCATE(m_sol_tot(max_npar,m_nsat),stat=irc)
      CALL alcerr(irc,'m_sol_tot',(/max_npar,m_nsat/),srName)
      ALLOCATE(nb(m_npar,m_nstcmax),stat=irc)
      CALL alcerr(irc,'nb',(/m_npar,m_nstcmax/),srName)
!      ALLOCATE(scl_tot(m_npar+3*m_nstcmax),stat=irc)
!      CALL alcerr(irc,'scl_tot',(/m_npar+3*m_nstcmax/),srName)
      ALLOCATE(parflg(max_npar),stat=irc)
      CALL alcerr(irc,'parflg',(/max_npar/),srName)
      m_atpa_tot = 0
      m_atpb_tot = 0
      m_sol_tot  = 0
! set up and invert combined neq-system for all satellites
! ********************************************************
      DO isat=1,m_nsat
! satellite index in stochastics list
        isat_stc=m_indsat(isat)

        if(isat_stc > 0)THEN
          nint = 1 + m_nstcepo(isat_stc)
          npar_tot = m_npar+3*m_nstcepo(isat_stc)
        else
          nint=1
          npar_tot = m_npar
        endif
!!!        write(*,*)'isat,isat_stc,npar_tot=',isat,isat_stc,npar_tot
        m_npar_tot(isat) = npar_tot
        npnp = m_npar*(m_npar+1)/2

! total sum of terms o-c, total number of observations
! ----------------------------------------------------
        m_rms_tot(isat)=0
        m_nobs_tot(isat)=0
        Do int=1,nint
          m_rms_tot(isat)=m_rms_tot(isat)   + m_rms(int,isat)
          m_nobs_tot(isat)=m_nobs_tot(isat) + m_nobs(int,isat)
        enddo

        DO int=nint,2,-1
          m_atpa(1:npnp,int-1,isat)=m_atpa(1:npnp,int-1,isat)+m_atpa(1:npnp,int,isat)
          m_atpb(1:m_npar,int-1,isat)=m_atpb(1:m_npar,int-1,isat)+m_atpb(1:m_npar,int,isat)
        ENDDO

! non-stochastic part of combined neq system
! ------------------------------------------
        m_atpa_tot(1:npnp,isat) = m_atpa(1:npnp,1,isat)
        m_atpb_tot(1:m_npar,isat) = m_atpb(1:m_npar,1,isat)

! stochastic part, right-hand side
! --------------------------------
        do int=1,nint-1
          do ii=1,3
            do ll=1,6
              m_atpb_tot(m_npar+(int-1)*3+ii,isat) = m_atpb_tot(m_npar+(int-1)*3+ii,isat) + &
              m_beta(ll,ii,int,isat_stc)*m_atpb(ll,int+1,isat)
            enddo
          enddo
        enddo

! stochastic part, left-hand side, first m_npar lines
! ---------------------------------------------------
        DO int=1,nint-1
          DO ii=1,m_npar
            DO kk=1,3
              iikk = ikf(ii,m_npar+(int-1)*3+kk)
              DO ll=1,6
                il=ikf(ii,ll)
                m_atpa_tot(iikk,isat) = m_atpa_tot(iikk,isat) + &
                                        m_atpa(il,int+1,isat)*m_beta(ll,kk,int,isat_stc)
              ENDDO
            ENDDO
          ENDDO
        ENDDO

! stochastic part, left-hand side, blocks B_i*(ATPAi*B_k)
! -------------------------------------------------------
        DO kint=1,nint-1
          DO int=1,kint
            DO ii=1,3
              IF(int == kint)THEN
                help=ii
              ELSE
                help=1
              ENDIF
              DO kk=help,3
                iikk=ikf(m_npar+(int-1)*3+ii,m_npar+(kint-1)*3+kk)
                DO ll=1,6
                  llkk = ikf(ll,m_npar+(kint-1)*3+kk)
                  m_atpa_tot(iikk,isat) = m_atpa_tot(iikk,isat) + &
                     m_beta(ll,ii,int,isat_stc)*m_atpa_tot(llkk,isat)
                ENDDO
              ENDDO
            ENDDO
          ENDDO
        ENDDO

! add constraints if too few observatinos are available
! -----------------------------------------------------
        ! less than 2 epochs: satellite is excluded

        jPar = 0
        ! if less than 5 epochs: constraints on 1-per-rev. RPR and pulses
        IF (m_nobs_tot(isat)+jPar < m_npar) THEN
          DO iPar = 7,m_npar
            IF (locq(4,ipar) >= 10) THEN
              m_atpa_tot(IKF(iPar,iPar),isat) = &
                m_atpa_tot(IKF(iPar,iPar),isat) + (1d0/sigma(2))**2
              jPar = jPar + 1
            ENDIF
          ENDDO
        ENDIF

        ! if only two epochs: constraints on all RPR and pulses
        IF (m_nobs_tot(isat)+jPar < m_npar) THEN
          DO iPar = 7,m_npar
            IF (locq(4,iPar) >= 7 .AND. locq(4,iPar) <= 9) THEN
              m_atpa_tot(IKF(iPar,iPar),isat) = &
                m_atpa_tot(IKF(iPar,iPar),isat) + (1d0/sigma(1))**2
              jPar = jPar + 1
            ENDIF
          ENDDO
        ENDIF

        ! too few obs. for all requ. pulses: constraints on pulses
        IF (m_nobs_tot(isat)+jPar < npar_tot .OR. &
            m_nobs_tot(isat)      < m_npar) THEN
          DO iPar = m_npar+1,npar_tot
            m_atpa_tot(IKF(iPar,iPar),isat) = &
              m_atpa_tot(IKF(iPar,iPar),isat) + (1d0/sigma(3))**2
            jPar = jPar + 1
          ENDDO
        ENDIF

! invert total normal equation system
! -----------------------------------
        sngLim = 1.d-14
        CALL syminvg(npar_tot,m_atpa_tot(1,isat) , 1,&
                         nsing, parflg, sbrName='stcogn_solve', bigajj_limit=sngLim)
        IF(nsing /=0)THEN
          DO ipar=1,npar_tot
            IF (parflg(ipar) == 0) CYCLE
            IF (ipar <=6) THEN
              WRITE(lfnerr,'(/,A,1X,A,I3,A,I5,/,12X,A,/)') &
              ' ### stcogn_solve:', &
              'Satellite ',m_satnumbers(isat),'  number of singularities=',nsing, &
              'CAUTION: an osculating element is singular!'
              EXIT
            ELSE
              WRITE(lfnerr,'(/,A,1X,A,I3,A,I5,/)') &
              ' ### stcogn_solve:', &
              'Satellite ',m_satnumbers(isat),'  number of singularities=',nsing
              EXIT
            ENDIF
          ENDDO
        ENDIF


!  solution vector
!  ---------------
        do ii=1,npar_tot
          m_sol_tot(ii,isat) = 0.d0
          do kk=1,npar_tot
            iikk=ikf(ii,kk)
            m_sol_tot(ii,isat) = m_sol_tot(ii,isat) + &
                                 m_atpa_tot(iikk,isat)*m_atpb_tot(kk,isat)
          enddo
        enddo
!
! compute rms of pseudo observation
! ---------------------------------
        do kk = 1, npar_tot
          m_rms_tot(isat) = m_rms_tot(isat) - m_atpb_tot(kk,isat) * m_sol_tot(kk,isat)
        enddo

        IF(m_rms_tot(isat) > 0.d0 .AND. m_nobs_tot(isat)-npar_tot+jPar > 0)THEN
          m_rms_tot(isat) =sqrt(m_rms_tot(isat)/(m_nobs_tot(isat)-npar_tot+jPar))
        ELSE
          m_rms_tot(isat) =0.d0
        ENDIF
!!!        if(m_npar_tot(isat) > m_npar)then
!!!          write(lfnprt,*)'isat, rms, pulses=',m_satnumbers(isat), m_rms_tot(isat),(m_sol_tot(ii,isat),ii=m_npar+1,m_npar_tot(isat))
!!!        endif
!!!        write(*,*)'isat, rms, pulses=',isat, m_rms_tot(isat)

! updated parameters and associated rms errors
! --------------------------------------------
        DO ii=1,npar_tot

          m_curpar(ii,isat) = m_inipar(ii,isat) + m_sol_tot(ii,isat)

          iikk=ikf(ii,ii)
          IF(m_atpa_tot(iikk,isat) > 0.d0)THEN
            m_rmspar(ii,isat)=m_rms_tot(isat)*dsqrt(m_atpa_tot(iikk,isat))
          ELSE
            m_rmspar(ii,isat)=0.d0
          ENDIF
        ENDDO

!!!        write(*,*)'isat, par=',isat, m_curpar(1:6,isat), m_inipar(1:6,isat)
!
! set array m_rpress, m_rpress_rms
        do ii=7,m_npar
          m_rpress(locq(4,ii)-6,isat) = m_curpar(ii,isat)
          m_rpress_rms(locq(4,ii)-6,isat) = m_rmspar(ii,isat)
        enddo

! cumulate the pulses
        IF (m_indsat(isat) > 0) THEN
          DO iepo=1,m_nstcepo(m_indsat(isat))
            m_pulse(1:3,iepo,m_indsat(isat)) =     &
              m_pulse(1:3,iepo,m_indsat(isat)) +   &
              m_curpar(m_npar+3*(iepo-1)+1:m_npar+3*(iepo-1)+3,isat)
            m_pulse_rms(1:3,iepo,m_indsat(isat)) = &
              m_rmspar(m_npar+3*(iepo-1)+1:m_npar+3*(iepo-1)+3,isat)
          ENDDO
        ENDIF

      ENDDO

! end of iteration step, set iter_old
! -----------------------------------
      DEALLOCATE(nb)
!      DEALLOCATE(scl_tot)
      DEALLOCATE(parflg)

      iter_old = iter

! end of setting up and inverting combined neq-system for all satellites
! **********************************************************************
    ENDIF

  END SUBROUTINE stcogn_solve


  SUBROUTINE stcogn_applyPos(isat,tobs,drdele, drv)

! -------------------------------------------------------------------------
! Purpose: The SR returns the corrections due to the stochastic parameters
!          for a particular satellite
! -------------------------------------------------------------------------

   USE m_bern, ONLY: i4b, r8b

   IMPLICIT NONE

! Parameters
! ----------
    INTEGER(i4b),                  INTENT(IN)  :: isat     ! satellite index
    REAL(r8b),                     INTENT(IN)  :: tobs     ! observation time
    REAL(r8b), DIMENSION(:,:),     INTENT(IN)  :: drdele   ! partials of orbit w.r.t. osc. ele
    REAL(r8b), DIMENSION(:)                    :: drv      ! corrections due to stochastics

! local parameters
! ----------------
    INTEGER(i4b)                               :: int      ! loop index
    INTEGER(i4b)                               :: iele     ! loop index
    INTEGER(i4b)                               :: istoch   ! index in stoch list

!
! index in stochastic list
! ------------------------
   drv(1:6)=0.d0
   istoch=m_indsat(isat)

!
! return, if satellite has no stochastic parms
! --------------------------------------------
    IF(istoch == 0)THEN
      return
    ENDIF

!
! find stochastic interval corresponding to tobs
! ----------------------------------------------
    int=0
    DO int=1,m_nstcepo(istoch)+1
      IF(tobs >= m_stcinter(int,isat) .AND. &
         tobs <= m_stcinter(int+1,isat))THEN
         exit
      ENDIF
    ENDDO

    if(int==0)return
!
! correction due to stochastics
! -----------------------------
    do iele=1,6
      drv(1:3) = drv(1:3) + m_dele(iele,int,isat)*drdele(1:3,iele)
    enddo

  END SUBROUTINE stcogn_applyPos



  SUBROUTINE stcogn_applyVel(isat,tobs,xvsat)

! -------------------------------------------------------------------------
! Purpose: The SR returns the corrections due to the stochastic parameters
!          for a particular satellite
! -------------------------------------------------------------------------

   USE m_bern, ONLY: i4b, r8b

   USE s_vprod

   IMPLICIT NONE

! Parameters
! ----------
    INTEGER(i4b),                 INTENT(IN)   :: isat     ! satellite index
    REAL(r8b),                    INTENT(IN)   :: tobs     ! observation time
    REAL(r8b), DIMENSION(:,:,:),  INTENT(INOUT):: xvsat    ! pos/vel of the satellite

! local parameters
! ----------------
    INTEGER(i4b)                               :: iepo     ! loop index
    REAL(r8b)                                  :: rsat, dew! length of vectors
    REAL(r8b), DIMENSION(3)                    :: ER,ES,EW ! unit vectors


! Do we have a pulse for this satellite?
! --------------------------------------
    IF(m_indsat(isat) > 0)THEN

! Find the correct epoch
      DO iepo=1,m_nstcepo(m_indsat(isat))
        IF (DABS(m_stcepo(iepo,m_indsat(isat))-tobs) < 0.5d0/86400d0) THEN

! Compute relevant unit vectors (UVs):
! ER: UV in radial direction
! EW: UV normal to orbital plane
! ES: UV in s-direction (ES=EW*ER)
! -------------------------------------
          rsat = DSQRT(xvsat(1,1,isat)**2 + &
                       xvsat(2,1,isat)**2 + &
                       xvsat(3,1,isat)**2)

          ER(1:3)=xvsat(1:3,1,isat)/rsat

          CALL vprod(xvsat(1:3,1,isat),xvsat(1:3,2,isat),EW)
          dew=DSQRT(EW(1)**2+EW(2)**2+EW(3)**2)
          EW(1:3)=EW(1:3)/dew

          CALL vprod(EW,ER,ES)

! APPLY THE VELOCITY CHANGE FROM THE PULSE
! ----------------------------------------
          xvsat(1:3,2,isat) = xvsat(1:3,2,isat) + &
            m_pulse(1,iepo,m_indsat(isat))*ER(1:3)
          xvsat(1:3,2,isat) = xvsat(1:3,2,isat) + &
            m_pulse(2,iepo,m_indsat(isat))*Es(1:3)
          xvsat(1:3,2,isat) = xvsat(1:3,2,isat) + &
            m_pulse(3,iepo,m_indsat(isat))*Ew(1:3)
        ENDIF
      ENDDO
    ENDIF
  END SUBROUTINE stcogn_applyVel


END MODULE

