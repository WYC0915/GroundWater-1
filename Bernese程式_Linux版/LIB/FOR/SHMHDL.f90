MODULE s_SHMHDL
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE shmhdl(iter  ,niter ,iorsys,t0arc ,tb12  ,t0    ,hint  , &
                    qpol  ,nvar  ,fac   ,locq  ,int   ,numsvn,orbmod, &
                    rprmom,scalpa,npoint,ycoe  ,zcoe  ,mooecl)

! ------------------------------------------------------------------------
! Purpose:    Correct coefficients of standard orbit representation for
!             satellite passages through the Moon's umbra/penumbra.
!
! Author:     U. Hugentobler
!
! Created:    10-Jan-2002
!
! Changes:    04-Feb-2002 HU: Initialize mooecl%iecl
!             06-Jun-2002 HU: Format statement corrected
!             30-Jul-2002 HU: Use interface for eclmoon
!             16-May-2003 HU: Deallocate arrays
!             21-May-2003 RD: Make the deallocation safe
!             11-Jun-2003 HU: New call to mosupn
!             18-Oct-2003 HU: Correction of variational equations
!             04-May-2008 RD: Numsat added to call of sr ARGSUN
!             17-Jul-2008 DT: Call to ROCKMD changed
!             01-Oct-2010 CR: Call MOSUPN as a module
!             03-Oct-2010 CR: New call of ARGSUN and MOSUPN
!             28-Jan-2011 SL: use m_bern with ONLY
!             05-Mar-2012 RD: Use listi4 as module now
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnErr
  USE d_const,  ONLY: gm
  USE d_shadow, ONLY: t_mooecl,maxsat,maxmev

  USE s_argsun
  USE s_alcerr
  USE s_mosupn
  USE s_eclmoon
  USE s_vprod
  USE s_qzeros
  USE s_arglat
  USE s_shadow1
  USE s_ypol
  USE s_exitrc
  USE f_listi4
  USE s_rockmd
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                :: iter   ! Current iteration step
  INTEGER(i4b)                :: niter  ! Total numer of iterations
  INTEGER(i4b)                :: iorsys ! Reference system index
                                        ! (passed thorugh common to mosupn!)
  REAL(r8b)                   :: t0arc  ! Reference time for arc (MJD)
  REAL(r8b),DIMENSION(*)      :: tb12   ! Interval boundaries (day fractions)
  REAL(r8b)                   :: t0     ! Reference time for arc
  REAL(r8b)                   :: hint   ! Step size
  INTEGER(i4b)                :: qpol   ! Integration order
  INTEGER(i4b)                :: nvar   ! Number of variational eqns
  REAL(r8b),DIMENSION(*)      :: fac    ! Faculties
  INTEGER(i4b),DIMENSION(6,*) :: locq   ! Definition of orbit parms
  INTEGER(i4b)                :: int    ! Current interval number
  INTEGER(i4b)                :: numsvn ! Satellite number
  INTEGER(i4b),DIMENSION(*)   :: orbmod ! Orbit model flag array
  REAL(r8b),DIMENSION(*)      :: rprmom ! current rpr parameters
  REAL(r8b),DIMENSION(*)      :: scalpa ! Scaling of partials
  INTEGER(i4b)                :: npoint ! Number of subintervals

! input/output:
  REAL(r8b),DIMENSION(3,*)    :: ycoe   ! Modified coefficients for orbit
  REAL(r8b),DIMENSION(*)      :: zcoe   ! Modified coeff for var eqns.

! output
  TYPE(t_mooecl)              :: mooecl ! Structure containing shadowing info

! Local variables
! ---------------
  INTEGER(i4b)                  :: ipnt,ishad,iac,isvn,ievt,ivar,ii,jj,kk
  INTEGER(i4b)                  :: ityp,irad
  INTEGER(i4b),DIMENSION(3)     :: nevent
  INTEGER(i4b),DIMENSION(3,3)   :: ievent
  INTEGER(i4b)                  :: ifirst=1
  REAL(r8b)                     :: tbeg,tend,epoch,tsec,cosd,dummy
  REAL(r8b)                     :: dt,dtsec,dtsec2,dtsec3
  REAL(r8b)                     :: frac,fecl,dists,distp,distu,frcmin
  REAL(r8b)                     :: rsun,abse2,agl,b0,u0,sclfac,valext
  REAL(r8b)                     :: rsat,rsat2,rsat3,rsat5,rsat7
  REAL(r8b)                     :: rz,rdr,rdz,zdr
  REAL(r8b),DIMENSION(3,2)      :: dxv,xvsat
  REAL(r8b),DIMENSION(3,nvar,2) :: dz,zsat
  REAL(r8b),DIMENSION(4)        :: sun,xmoon,dum4
  REAL(r8b),DIMENSION(3,3)      :: dum33
  REAL(r8b),DIMENSION(3)        :: dsun,e1,e2,e3
  REAL(r8b),DIMENSION(3)        :: force,force0,a0,a1,a2,a3
  REAL(r8b),DIMENSION(3,nvar)   :: forcep,force0p
  REAL(r8b),DIMENSION(3,3)      :: table,tevent
  REAL(r8b),DIMENSION(3)        :: dummy3

! Function
! --------

  frcmin=100D0

  IF (ifirst==1) THEN
    mooecl%iecl=0
    ifirst=0
  ENDIF

! Start and end of interval
! -------------------------
  tbeg   = t0arc+tb12(int)
  tend   = t0arc+tb12(int+1)
  dt     = (tend-tbeg)/npoint
  dtsec  = dt*86400D0
  dtsec2 = dtsec**2
  dtsec3 = dtsec**3

! Initialization of integration
! -----------------------------
  dxv  =0D0
  dz   =0D0

! Loop over subintervals
! ----------------------
  DO ipnt = 0,npoint
    epoch = tbeg+ipnt*dt

! Positions for satellite, Sun, and Moon
! --------------------------------------
! satellite position
    tsec=(epoch-t0arc-t0)*86400.D0
    CALL ypol(1,qpol,3,hint,fac,tsec,ycoe,xvsat)

! derivatives w.r.t. parameters
    CALL ypol(1,qpol,3*nvar,hint,fac,tsec,zcoe,zsat)

! Sun's shadow
    CALL mosupn(epoch,2,dummy,dummy,dummy,dummy,dummy, &
                dummy,dummy,dum33,dum33,sun,dum4,dummy,dummy3)
    CALL shadow1(xvsat,sun,ishad,dists)

! Moon's shadow
    CALL mosupn(epoch,3,dummy,dummy,dummy,dummy,dummy, &
                dummy,dummy,dum33,dum33,dum4,xmoon,dummy,dummy3)
    CALL eclmoon(xvsat(:,1),xmoon,sun,frac,fecl,distp,distu)
    IF (frac<frcmin) frcmin=frac

! Moon too far from Sun
    cosd=DOT_PRODUCT(sun(1:3),xmoon(1:3))/sun(4)/xmoon(4)
    IF (cosd < .99D0) RETURN

! Identify exact times for shadow border passages
! -----------------------------------------------
    IF (iter==niter) THEN
      IF (ipnt>2) THEN
        table(1,1:3)=table(2,1:3)
        table(2,1:3)=table(3,1:3)
        table(3,1)  =distp
        table(3,2)  =distu
        table(3,3)  =dists
      ELSE
        table(ipnt+1,1)=distp
        table(ipnt+1,2)=distu
        table(ipnt+1,3)=dists
      ENDIF
      nevent=0
      IF (ipnt>1) THEN
        CALL qzeros(ipnt-1,table(1:3,1), &
                           nevent(1),ievent(:,1),tevent(:,1),valext)
        CALL qzeros(ipnt-1,table(1:3,2), &
                           nevent(2),ievent(:,2),tevent(:,2),dummy)
        CALL qzeros(ipnt-1,table(1:3,3), &
                           nevent(3),ievent(:,3),tevent(:,3),dummy)
      ENDIF
! Status at beginning of interval
      IF ((ipnt==0 .OR. ipnt==npoint) .AND. (frac<1D0 .OR. ishad==1)) THEN
        IF (frac==0D0) THEN
          ievt=2
        ELSEIF (frac < 0) THEN
          ievt=1
        ELSE
          ievt=3
        ENDIF
        IF (ipnt==0) THEN
          nevent(ievt)=nevent(ievt)+1
          ievent(nevent(ievt),ievt)=-1
          tevent(nevent(ievt),ievt)=0D0
        ELSE
          nevent(ievt)=nevent(ievt)+1
          ievent(nevent(ievt),ievt)=1
          tevent(nevent(ievt),ievt)=0D0
        ENDIF
      ENDIF

! Save events
! -----------
! Loop over events
      DO jj=1,3
        LoopEvents: DO ii=1,nevent(jj)
          IF (ievent(ii,jj) ==  2 .OR. &
              ievent(ii,jj) == -2 .AND. jj==1 .AND. valext >= 0 .OR. &
              ievent(ii,jj) == -2 .AND. jj==2 .OR. &
              ievent(ii,jj) == -2 .AND. jj==3 ) CYCLE LoopEvents
! Allocate array
          IF (mooecl%iecl == 0) THEN
            IF (ASSOCIATED(mooecl%sat)) DEALLOCATE( mooecl%sat, STAT=iac )
            ALLOCATE( mooecl%sat(maxsat), STAT=iac )
            CALL alcerr(iac, 'mooecl%sat', (/maxsat/), 'shmhdl')
            mooecl%iecl=1
            mooecl%nsat=0
            mooecl%sat(1:maxsat)%nevent=0
          ENDIF

! Get satellite number
          isvn = LISTI4(1,maxsat,mooecl%sat(:)%svn,numsvn,mooecl%nsat)
! Join status at interval border
          IF (tevent(ii,jj)==0D0 .AND. ievent(ii,jj)<0) THEN
            DO ievt=1,mooecl%sat(isvn)%nevent
              IF (ABS(mooecl%sat(isvn)%tevent(ievt)-tbeg)<SPACING(tbeg).AND. &
                  (mooecl%sat(isvn)%ievent(ievt)==1 .OR.    &
                   mooecl%sat(isvn)%ievent(ievt)==2 .OR.    &
                   mooecl%sat(isvn)%ievent(ievt)==3)) THEN
                DO kk=ievt+1,mooecl%sat(isvn)%nevent
                  mooecl%sat(isvn)%ievent(kk-1)=mooecl%sat(isvn)%ievent(kk)
                  mooecl%sat(isvn)%tevent(kk-1)=mooecl%sat(isvn)%tevent(kk)
                ENDDO
                mooecl%sat(isvn)%nevent=mooecl%sat(isvn)%nevent-1
                CYCLE LoopEvents
              ENDIF
            ENDDO
          ENDIF

          IF (mooecl%sat(isvn)%nevent >= maxmev) THEN
            WRITE(lfnerr,"(/,' *** SR SHMHDL: Too many moon eclipse events', &
                        &  /,'                Satellite            :',I6,    &
                        &  /,'                Max. number of events:',I6)")  &
                                     numsvn,mooecl%sat(isvn)%nevent
            CALL exitrc(2)
          ENDIF

! Fill arrays
          ievt = mooecl%sat(isvn)%nevent+1
          mooecl%sat(isvn)%nevent = ievt
          IF (jj==1) THEN
            IF (ievent(ii,jj) == -1) mooecl%sat(isvn)%ievent(ievt) = -2
            IF (ievent(ii,jj) == -2) mooecl%sat(isvn)%ievent(ievt) =  0
            IF (ievent(ii,jj) ==  1) mooecl%sat(isvn)%ievent(ievt) =  2
          ELSEIF(jj==2) THEN
            IF (ievent(ii,jj) == -1) mooecl%sat(isvn)%ievent(ievt) = -1
            IF (ievent(ii,jj) ==  1) mooecl%sat(isvn)%ievent(ievt) =  1
          ELSE
            IF (ievent(ii,jj) == -1) mooecl%sat(isvn)%ievent(ievt) = -3
            IF (ievent(ii,jj) ==  1) mooecl%sat(isvn)%ievent(ievt) =  3
          ENDIF
          mooecl%sat(isvn)%tevent(ievt) = epoch+dt*tevent(ii,jj)
          mooecl%sat(isvn)%eclmin(ievt) = 1-frcmin
        ENDDO LoopEvents
      ENDDO
    ENDIF

! Compute force
! -------------
    force=0D0
    forcep=0D0
    IF (ishad==0) THEN

! Unit vectors in dyx directions
      dsun(1:3)=sun(1:3)-xvsat(1:3,1)
      rsun=SQRT(DOT_PRODUCT(dsun(1:3),dsun(1:3)))

      e3(1:3)=dsun(1:3)/rsun
      CALL vprod(xvsat(1:3,1),e3,e2)
      abse2=SQRT(DOT_PRODUCT(e2,e2))
      e2(1:3)=e2(1:3)/abse2
      CALL vprod(e3,e2,e1)

! Angular arguments
      CALL arglat(xvsat,agl)
      CALL argsun(epoch,sun,dummy3,xvsat,numsvn,b0,dummy,u0,dummy)

! A priori model: rock4/rock42
      CALL rockmd(epoch,numsvn,xvsat,sun,e2,agl,b0,u0,force,sclfac,dummy3)
      force =(frac-1)*force
      sclfac=(frac-1)*sclfac

! Once per rev parameters
      IF (orbmod(6)==0) THEN
        force(1:3)=force(1:3)+sclfac*(rprmom(1)*e3(1:3)                 &
                                     +rprmom(2)*e2(1:3)                 &
                                     +rprmom(3)*e1(1:3)                 &
                                     +rprmom(4)*e3(1:3)*COS(agl)        &
                                     +rprmom(5)*e2(1:3)*COS(agl)        &
                                     +rprmom(6)*e1(1:3)*COS(agl)        &
                                     +rprmom(7)*e3(1:3)*SIN(agl)        &
                                     +rprmom(8)*e2(1:3)*SIN(agl)        &
                                     +rprmom(9)*e1(1:3)*SIN(agl))
      ENDIF

! Partial derivatives of force with respect to parameters
! -------------------------------------------------------
      DO ivar=1,nvar
        ityp=locq(1,ivar)
        irad=locq(4,ivar)
! Dynamical orbit parameters
        IF (ityp==3 .AND. irad >6 .AND. orbmod(6)==0) THEN
          IF (irad==7) THEN
            forcep(1:3,ivar)=sclfac/scalpa(irad)*e3(1:3)
          ELSEIF (irad==8) THEN
            forcep(1:3,ivar)=sclfac/scalpa(irad)*e2(1:3)
          ELSEIF (irad==9) THEN
            forcep(1:3,ivar)=sclfac/scalpa(irad)*e1(1:3)
          ELSEIF (irad==10) THEN
            forcep(1:3,ivar)=sclfac/scalpa(irad)*e3(1:3)*cos(agl)
          ELSEIF (irad==11) THEN
            forcep(1:3,ivar)=sclfac/scalpa(irad)*e2(1:3)*cos(agl)
          ELSEIF (irad==12) THEN
            forcep(1:3,ivar)=sclfac/scalpa(irad)*e1(1:3)*cos(agl)
          ELSEIF (irad==13) THEN
            forcep(1:3,ivar)=sclfac/scalpa(irad)*e3(1:3)*sin(agl)
          ELSEIF (irad==14) THEN
            forcep(1:3,ivar)=sclfac/scalpa(irad)*e2(1:3)*sin(agl)
          ELSEIF (irad==15) THEN
            forcep(1:3,ivar)=sclfac/scalpa(irad)*e1(1:3)*sin(agl)
          ENDIF
        ENDIF
      ENDDO

! 'Encke force'
      rsat =SQRT(DOT_PRODUCT(xvsat(1:3,1),xvsat(1:3,1)))
      rsat2=rsat*rsat
      rsat3=rsat*rsat2
      rsat5=rsat3*rsat2
      rdr  =DOT_PRODUCT(xvsat(1:3,1),dxv(1:3,1))

      force=-gm*(dxv(1:3,1)/rsat3-3*xvsat(1:3,1)*rdr/rsat5) + force

! Correction for variational equations
      rsat7=rsat5*rsat2
      DO ivar=1,nvar
        rz =DOT_PRODUCT(xvsat(1:3,1),zsat(1:3,ivar,1))
        rdz=DOT_PRODUCT(xvsat(1:3,1),dz(1:3,ivar,1))
        zdr=DOT_PRODUCT(zsat(1:3,ivar,1),dxv(1:3,1))

        forcep(1:3,ivar)=-gm*(dz(1:3,ivar,1)/rsat3           &
                              -3*dxv(1:3,1)*rz/rsat5         &
                              -3*zsat(1:3,ivar,1)*rdr/rsat5  &
                              +15*xvsat(1:3,1)*rz*rdr/rsat7  &
                              -3*xvsat(1:3,1)*zdr/rsat5      &
                              -3*xvsat(1:3,1)*rdz/rsat5)     &
                         +forcep(1:3,ivar)
      ENDDO
    ENDIF

! Integrate
! ---------
    IF (ipnt > 0) THEN
! ..position
      a0=dxv(1:3,1)
      a1=dxv(1:3,2)
      a2=force0
      a3=(force-force0)/dtsec

      dxv(1:3,1) = a0 + a1*dtsec + a2*dtsec2/2 + a3*dtsec3/6
      dxv(1:3,2) = a1 + a2*dtsec + a3*dtsec2/2

! ..variation equations
      DO ivar=1,nvar
        a0=dz(1:3,ivar,1)
        a1=dz(1:3,ivar,2)
        a2=force0p(1:3,ivar)
        a3=(forcep(1:3,ivar)-force0p(1:3,ivar))/dtsec

        dz(1:3,ivar,1) = a0 + a1*dtsec + a2*dtsec2/2 + a3*dtsec3/6
        dz(1:3,ivar,2) = a1 + a2*dtsec + a3*dtsec2/2
      ENDDO

    ENDIF

    force0 =force
    force0p=forcep
  ENDDO


! Correct standard orbit coefficients
! -----------------------------------
! Condition: position and velocity at beginning and end of integration
!            interval are represented exactly

  a2 =  3*dxv(1:3,1)-dxv(1:3,2)*hint
  a3 = -2*dxv(1:3,1)+dxv(1:3,2)*hint

  ycoe(1:3,3) = ycoe(1:3,3) + a2
  ycoe(1:3,4) = ycoe(1:3,4) + a3

! there is some error, problem cured in deqrhs
!  DO ivar=1,nvar
!    a2 =  3*dz(1:3,ivar,1)-dz(1:3,ivar,2)*hint
!    a3 = -2*dz(1:3,ivar,1)+dz(1:3,ivar,2)*hint
!
!    ii=ivar*3
!    zcoe(ii+1:ii+3) = zcoe(ii+1:ii+3) + a2
!    zcoe(ii+4:ii+6) = zcoe(ii+4:ii+6) + a3
!  ENDDO

  RETURN

  END SUBROUTINE shmhdl

END MODULE
