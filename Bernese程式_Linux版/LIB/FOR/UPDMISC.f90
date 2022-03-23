MODULE s_UPDMISC
  USE m_time,   ONLY: OPERATOR(+)
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE updmisc(neq,neq_1,ifil)

! -------------------------------------------------------------------------
! Purpose:    This subroutine updates miscellaneous information by
!             stacking of two NEQ systems
!
! Author:     L. Mervart
!
! Created:    22-Nov-1997
!
! Changes:    09-MAR-2000 LM: Correction in taecml computation
!             28-AUG-2000 SS: Cope with changing miscellaneous NEQ info
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             28-Jan-2003 AJ: Call chksin
!             08-Sep-2003 HU: Check datum, nutnam, subnam
!             23-Mar-2004 DT: Copy datum, nutnam, subnam from neq_1 only if
!                             neq%version>1
!             28-Jul-2005 RD: Put the OPERATOR(+)-import on the top
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             13-Feb-2007 AG: Update of neq%misc wrt satellite antenna
!                             group definition refined
!             04-May-2009 RD: Scaling of loading models added
!             06-May-2009 RD: Revision of satellite grouping for multi-year sol.
!             17-Sep-2010 RD: Skip section if no satellites in the group lists
!             08-Oct-2010 RD: Error if mixing troposphere models
!             12-Oct-2010 RD: Init nsmpnq and ielvnq
!             27-Oct-2010 SL: use m_bern with ONLY
!             05-Mar-2012 RD: Use LISTI4 as module now
!             27-Apr-2012 RD: Nullify all pointers
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, lfnErr, lfnPrt
  USE m_global, ONLY: maxsys
  USE m_maxdim, ONLY: maxsat
  USE m_time,   ONLY: t_timint
  USE d_neq,    ONLY: t_neq,maxStaSin,maxOff
  USE d_trpest, ONLY: undef_Trp
  USE p_addneq, ONLY: comstat,opt
  USE s_iordup
  USE s_alcerr

  USE s_chksin
  USE s_exitrc
  USE f_listi4
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_neq)             :: neq       ! Resulting NEQ-system
  TYPE(t_neq), INTENT(IN) :: neq_1     ! Input NEQ-system
  INTEGER(i4b)            :: ifil

! Local Variables
! ---------------
  TYPE(t_timint)          :: timtmp
  LOGICAL                 :: offset,pattern
  INTEGER(i4b)            :: ipar
  INTEGER(i4b)            :: istat
  INTEGER(i4b)            :: istat_1
  INTEGER(i4b)            :: istatHlp
  INTEGER(i4b)            :: iTropM,iTropM_1
  INTEGER(i4b)            :: iGrp,nGrp,isat,nanoff,nanspv,dummy
  INTEGER(i4b)            :: iSys
  INTEGER(i4b),DIMENSION(maxOff)              :: gnroff_1,gnrspv_1
  INTEGER(i4b),DIMENSION(:),ALLOCATABLE       :: gnroffold,gnrspvold
  INTEGER(i4b),DIMENSION(:),POINTER           :: gnroff,gnrspv
  INTEGER(i4b),DIMENSION(:),ALLOCATABLE       :: indx,nsaoff
  INTEGER(i4b),DIMENSION(:,:),ALLOCATABLE     :: satoff
  INTEGER(i4b)            :: iKey

! Nullify pointers
! ----------------
  NULLIFY(gnroff)
  NULLIFY(gnrspv)

! Just copy the information about a priori orbit files
! ----------------------------------------------------
  neq%misc%orbfil = neq_1%misc%orbfil

! Update nobs, lTPl, nparms, nftot
! --------------------------------
  neq%misc%nobs   =  neq%misc%nobs   +  neq_1%misc%nobs
  neq%misc%lTPl   =  neq%misc%lTPl   +  neq_1%misc%lTPl
  neq%misc%nftot  =  neq%misc%nftot  +  neq_1%misc%nftot


! Check for the compatibility of the troposphere models
! -----------------------------------------------------
  IF ( ( neq%misc%itropo   /= undef_Trp .AND. &
         neq_1%misc%itropo /= undef_Trp .AND. &
         neq%misc%itropo   /= neq_1%misc%itropo ) .OR. &
       ( neq%misc%iextra   /= undef_Trp .AND. &
         neq_1%misc%iextra /= undef_Trp .AND. &
         neq%misc%iextra   /= neq_1%misc%iextra ) .OR. &
       ( neq%misc%itrmap   /= undef_Trp .AND. &
         neq_1%misc%itrmap /= undef_Trp .AND. &
         neq%misc%itrmap   /= neq_1%misc%itrmap ) .OR. &
       ( neq%misc%itrgrd   /= undef_Trp .AND. &
         neq_1%misc%itrgrd /= undef_Trp .AND. &
         neq%misc%itrgrd   /= neq_1%misc%itrgrd ) ) THEN

    IF (neq%misc%iExtra==0) THEN
      iTropM = neq%misc%iTropo
    ELSE IF (neq%misc%iExtra==1) THEN
      iTropM = -neq%misc%iTropo
    ELSE IF (neq%misc%iExtra==2) THEN
      iTropM = neq%misc%iTropo+100
    END IF

    IF (neq_1%misc%iExtra==0) THEN
      iTropM_1 = neq_1%misc%iTropo
    ELSE IF (neq_1%misc%iExtra==1) THEN
      iTropM_1 = -neq_1%misc%iTropo
    ELSE IF (neq_1%misc%iExtra==2) THEN
      iTropM_1 = neq_1%misc%iTropo+100
    END IF

    WRITE(lfnerr,'(/,A,/,17X,A,3(/,17X,A,I5,A,I3),/)')                &
    ' *** SR UPDMISC: You try to mix different troposphere models ' , &
    'from your input NEQ-files. This is not supported.',              &
    'A priori model:   ',iTropM,         ' / ',iTropM_1,              &
    'mapping function: ',neq%misc%itrmap,' / ',neq_1%misc%itrmap,     &
    'Gradient model:   ',neq%misc%itrgrd,' / ',neq_1%misc%itrgrd

    CALL exitrc(2)
  ENDIF

! Update the troposphere information
! ----------------------------------
  IF (neq%misc%nsmpnq == 0) neq%misc%nsmpnq = neq_1%misc%nsmpnq
  neq%misc%nsmpnq = MIN(neq%misc%nsmpnq,neq_1%misc%nsmpnq)
  IF (neq%misc%ielvnq == 0) neq%misc%ielvnq = neq_1%misc%ielvnq
  neq%misc%ielvnq = MIN(neq%misc%ielvnq,neq_1%misc%ielvnq)

  IF (neq_1%misc%itropo /= undef_Trp) neq%misc%itropo = neq_1%misc%itropo
  IF (neq_1%misc%iextra /= undef_Trp) neq%misc%iextra = neq_1%misc%iextra
  IF (neq_1%misc%itrmap /= undef_Trp) neq%misc%itrmap = neq_1%misc%itrmap
  IF (neq_1%misc%itrgrd /= undef_Trp) neq%misc%itrgrd = neq_1%misc%itrgrd

 IF ( neq_1%version >1 ) THEN

    if (ifil>1 .AND. neq%misc%datum /= neq_1%misc%datum) THEN
      write(lfnerr,"(' ### SR UPDMISC: Datum string changed:', &
                 & /,'                 File',I4,': ',A,        &
                 & /,'                 File',I4,': ',A,/)")    &
                      ifil-1, neq%misc%datum, ifil, neq_1%misc%datum
    ENDIF
    neq%misc%datum = neq_1%misc%datum

    if (ifil>1 .AND. neq%misc%nutmod /= neq_1%misc%nutmod) THEN
      write(lfnerr,"(' ### SR UPDMISC: Nutation model changed:', &
                 & /,'                 File',I4,': ',A,        &
                 & /,'                 File',I4,': ',A,/)")    &
                      ifil-1, neq%misc%nutmod, ifil, neq_1%misc%nutmod
    ENDIF
    neq%misc%nutmod = neq_1%misc%nutmod

    if (ifil>1 .AND. neq%misc%submod /= neq_1%misc%submod) THEN
      write(lfnerr,"(' ### SR UPDMISC: Subdaily model changed:', &
                 & /,'                 File',I4,': ',A,          &
                 & /,'                 File',I4,': ',A,/)")      &
                      ifil-1, neq%misc%submod, ifil, neq_1%misc%submod
    ENDIF
    neq%misc%submod = neq_1%misc%submod

  ENDIF

! Update group definition for satellite antenna offsets and patterns
! ------------------------------------------------------------------
    nanoff = 0
    nanspv = 0
    gnroff_1 = 0
    gnrspv_1 = 0
    offset = .false.
    pattern = .false.
! Search in second NEQ (neq_1)
    DO ipar=1, neq_1%misc%npar
      IF (neq_1%par(ipar)%locq(1) /= 12 .AND. neq_1%par(ipar)%locq(1) /= 25) CYCLE
!    offsets
      IF (neq_1%par(ipar)%locq(1) == 12 .AND. neq_1%par(ipar)%locq(4) /= 0 ) THEN
        offset = .true.
        gnroff_1(neq_1%par(ipar)%locq(4)) = neq_1%par(ipar)%locq(5)
!    pattern
      ELSEIF (neq_1%par(ipar)%locq(1) == 25 .AND. neq_1%par(ipar)%locq(2) /= 0 ) THEN
        pattern = .true.
        gnrspv_1(neq_1%par(ipar)%locq(2)) = neq_1%par(ipar)%locq(3)
      ENDIF
    ENDDO

    IF (.NOT. offset .AND. pattern) gnroff_1 = gnrspv_1
! Search in stacked NEQ (neq)
    ALLOCATE(gnroff(maxOff),stat=dummy)
    CALL alcerr(dummy,'gnroff',(/maxOff/),'updmisc')
    gnroff = 0
    ALLOCATE(gnrspv(maxOff),stat=dummy)
    CALL alcerr(dummy,'gnrspv',(/maxOff/),'updmisc')
    gnrspv = 0
    ALLOCATE(gnroffold(maxOff),stat=dummy)
    CALL alcerr(dummy,'gnroffold',(/maxOff/),'updmisc')
    gnroffold = 0
    ALLOCATE(gnrspvold(maxOff),stat=dummy)
    CALL alcerr(dummy,'gnrspvold',(/maxOff/),'updmisc')
    gnrspvold = 0

    DO ipar=1, neq%misc%npar
      IF (neq%par(ipar)%locq(1) /= 12 .AND. neq%par(ipar)%locq(1) /= 25) CYCLE
!    offsets
      IF (neq%par(ipar)%locq(1) == 12 .AND. neq%par(ipar)%locq(4) /= 0 ) THEN
          iGrp = listi4(1,maxSat,gnroff(:),neq%par(ipar)%locq(5),nanoff)
          gnroffold(neq%par(ipar)%locq(4)) = neq%par(ipar)%locq(5)
!    pattern
      ELSEIF (neq%par(ipar)%locq(1) == 25 .AND. neq%par(ipar)%locq(2) /= 0 ) THEN
          iGrp = listi4(1,maxSat,gnrspv(:),neq%par(ipar)%locq(3),nanspv)
          gnrspvold(neq%par(ipar)%locq(2)) = neq%par(ipar)%locq(3)
      ENDIF
    ENDDO
    IF (nanoff /= 0 .AND. nanspv /= 0 .AND. nanoff /= nanspv) THEN
      WRITE(lfnprt,'(/,A,/,17X,A,/)') &
         ' *** SR UPDMISC: Number of satellite antenna offset groups differs', &
                          'from number of satellite antenna pattern groups.'
      call exitrc(2)
    ELSEIF (nanspv /= 0) THEN
      nanoff = nanspv
      gnroff = gnrspv
      gnroffold = gnrspvold
    ENDIF

! Allocate indx and sort gnroff
    IF (nanoff > 0) THEN
      ALLOCATE(indx(nanoff),stat=dummy)
      CALL alcerr(dummy,'indx',(/nanoff/),'updmisc')
      indx = 0
      CALL iordup(gnroff,nanoff,indx)

! Allocate satoff and nsaoff
      ALLOCATE(nsaoff(nanoff),stat=dummy)
      CALL alcerr(dummy,'nsaoff',(/nanoff/),'updmisc')
      nsaoff = 0

      ALLOCATE(satoff(maxsat,nanoff),stat=dummy)
      CALL alcerr(dummy,'satoff',(/maxSat,nanoff/),'updmisc')
      satoff = 0

! Combine satellite numbers in groups coming from NEQs
      DO iGrp = 1,nanoff
        IF (neq%misc%nanoff /= 0) THEN
          nGrp = listi4(0,maxsat,gnroffold,gnroff(indx(iGrp)),neq%misc%nanoff)
          DO isat=1,neq%misc%nsaoff(nGrp)
            dummy=listi4(1,maxSat,satoff(:,indx(iGrp)),neq%misc%satoff(isat,nGrp),nsaoff(indx(iGrp)))
          ENDDO
        ENDIF
        nGrp = listi4(0,maxsat,gnroff_1,gnroff(indx(iGrp)),neq_1%misc%nanoff)
        DO isat=1,neq_1%misc%nsaoff(nGrp)
          dummy=listi4(1,maxSat,satoff(:,indx(iGrp)),neq_1%misc%satoff(isat,nGrp),nsaoff(indx(iGrp)))
        ENDDO
        neq%misc%nsaoff(iGrp) = nsaoff(indx(iGrp))
        neq%misc%satoff(:,iGrp) = satoff(:,indx(iGrp))
      ENDDO
      neq%misc%nanoff = nanoff

! Set new internal group number in locq
      IF (neq%misc%nanoff /= 0) THEN
        DO ipar = 1, neq%misc%npar
          IF (neq%par(ipar)%locq(1) /= 12 .AND. neq%par(ipar)%locq(1) /= 25) CYCLE
          IF (neq%par(ipar)%locq(1) == 12 .AND. neq%par(ipar)%locq(4) /= 0 ) THEN
            DO iGrp = 1,neq%misc%nanoff
              IF ( gnroff(indx(iGrp)) == neq%par(ipar)%locq(5) ) THEN
                neq%par(ipar)%locq(2) = iGrp
                neq%par(ipar)%locq(4) = iGrp
                EXIT
              ENDIF
            ENDDO
          ELSE IF (neq%par(ipar)%locq(1) == 25 .AND. neq%par(ipar)%locq(2) /= 0 ) THEN
            DO iGrp = 1,neq%misc%nanoff
              IF ( gnroff(indx(iGrp)) == neq%par(ipar)%locq(3) ) THEN
                neq%par(ipar)%locq(2) = iGrp
                EXIT
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDIF

! Deallocate the arrays
      DEALLOCATE(indx)
      DEALLOCATE(nsaoff)
      DEALLOCATE(satoff)
    ENDIF
    DEALLOCATE(gnroff)
    DEALLOCATE(gnrspv)
    DEALLOCATE(gnroffold)
    DEALLOCATE(gnrspvold)
! End update satellite antenna group definitions (na wenn dat mal funktioniert!!!)

!!!!!  END IF

! Handle the SINEX information
! ----------------------------
  LOOP_istat_1: DO istat_1 = 1, neq_1%misc%nstat_sinex

    IF (comstat%taecml(1,1,ifil) == 0.0                                 .OR. &
        comstat%taecml(1,1,ifil) > neq_1%misc%sinex(istat_1)%timint%t(1) ) THEN
      comstat%taecml(1,1,ifil) = neq_1%misc%sinex(istat_1)%timint%t(1)
    END IF

    IF (comstat%taecml(2,1,ifil) == 0.0                                 .OR. &
        comstat%taecml(2,1,ifil) < neq_1%misc%sinex(istat_1)%timint%t(2) ) THEN
      comstat%taecml(2,1,ifil) = neq_1%misc%sinex(istat_1)%timint%t(2)
    END IF

! Check if the station appears for the first time in neq_1
! (this cannot be guaranteed if the station was renamed in STACRUX)
      DO istatHlp = 1, istat_1 - 1
        IF (neq_1%misc%sinex(istat_1)%stname ==   &
            neq_1%misc%sinex(istatHlp)%stname   ) THEN
          CYCLE LOOP_istat_1
        END IF
      END DO

    DO istat = 1, neq%misc%nstat_sinex
      IF (neq_1%misc%sinex(istat_1)%stname ==    &
          neq%misc%sinex(istat)%stname        ) THEN

        IF (opt%snxinc == 1) THEN
          CALL chksin(neq%misc%sinex(istat),neq_1%misc%sinex(istat_1),0)
        END IF
!
! Copy most recent info into final neq

        timtmp = neq%misc%sinex(istat)%timint +     &
                 neq_1%misc%sinex(istat_1)%timint
        neq%misc%sinex(istat) = neq_1%misc%sinex(istat_1)
        neq%misc%sinex(istat)%timint = timtmp

        comstat%nflsta(istat)                      = comstat%nflsta(istat) + 1
        comstat%indfil(istat,comstat%nflsta(istat)) = ifil

        DO iSys = 0,maxsys-1
          IF (LEN_TRIM(neq%misc%sinex(istat)%antpcv(iSys)%atxStr) == 0) &
            neq%misc%sinex(istat)%antpcv(iSys) = neq_1%misc%sinex(istat_1)%antpcv(iSys)
        ENDDO
        CYCLE LOOP_istat_1
      END IF
    END DO

    neq%misc%nstat_sinex = neq%misc%nstat_sinex + 1

    IF ( neq%misc%nstat_sinex > maxStaSin ) THEN
      WRITE(lfnerr,*) ' *** SR UPDMISC: maxStaSin < ', neq%misc%nstat_sinex
      CALL exitrc(2)
    END IF

    neq%misc%sinex( neq%misc%nstat_sinex ) = neq_1%misc%sinex( istat_1 )

    comstat%nflsta( neq%misc%nstat_sinex )  = &
                               comstat%nflsta( neq%misc%nstat_sinex ) + 1
    comstat%indfil(istat,comstat%nflsta(neq%misc%nstat_sinex))  = ifil

  END DO LOOP_istat_1

! Scaling factors for Vienna grid file
  DO iKey = 1,SIZE(neq_1%misc%grdNeq)
    IF (neq%misc%grdNeq(iKey) == neq_1%misc%grdNeq(iKey)) CYCLE
    IF (neq%misc%grdNeq(iKey) == '' ) THEN
      neq%misc%grdNeq(iKey) = neq_1%misc%grdNeq(iKey)
    ENDIF
  ENDDO

  comstat%nparl(ifil)  = neq_1%misc%npar
  comstat%titind(ifil) = neq_1%misc%title(1)
  comstat%datcre(ifil) = ''
  comstat%timcre(ifil) = ''

END SUBROUTINE updmisc


END MODULE
