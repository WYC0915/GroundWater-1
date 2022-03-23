MODULE s_CECOMB
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE cecomb(ityp,opt,npar,clk,wgtclk,dclk,wgtcld,epoch,mepo, &
                  ClkHead,ClkRec,icrx,icrxrf,ircode)

! -------------------------------------------------------------------------
! Purpose:    Combine clocks and clock differences to consistent clock set
!
! Remarks:    - Subroutine of CLKEST
!
! Author:     U. Hugentobler
!
! Created:    21-Aug-2001
!
! Changes:    11-Aug-2002 HU: Sum condition on code implemented
!             30-Aug-2002 HU: Use UNDEF, write info file
!             02-Sep-2005 HB: Use new SR gtsclk instead of gtsclm
!             30-Aug-2006 RD: Use UNDEF from D_CLKRNX
!             01-Nov-2007 HB: Add parameter secIpl for SR gtsclk
!             02-Feb-2009 HB/RD/AS: Application of a priori clock no longer
!                             needed (GALILEO modifications)
!             29-May-2009 RD: Remove s_gtsclk (not used)
!             26-Jun-2009 RD: Use WGTCLD to identify blocks for cond. of sum
!             19-Sep-2012 RD: Use M_BERN with ONLY
!             19-Sep-2012 RD: Remove unused variables and parameters
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnerr, lfnplt
  USE p_clkest, ONLY: t_clkopt
  USE d_clkrnx, ONLY: t_clkhead, t_clkrec, undef
  USE s_alcerr
  USE s_setdia
  USE s_tridiag
  USE s_exitrc
  IMPLICIT NONE

! DUMMY ARGUMENTS
! ---------------
! Input:
  INTEGER(i4b)                :: ityp      ! clock type
                                           !  1: satellite clock
                                           !  2: station clock
  TYPE(t_clkopt)              :: opt       ! option structure for clkest
! Input/Output:
  INTEGER(i4b)                :: npar      ! number of clock values
  REAL(r8b),DIMENSION(:)      :: clk       ! clock values
! Input:
  REAL(r8b),DIMENSION(:)      :: wgtclk    ! clock weights
  REAL(r8b),DIMENSION(:)      :: dclk      ! clock difference values
  REAL(r8b),DIMENSION(:)      :: wgtcld    ! clock difference weights
  REAL(r8b),DIMENSION(:)      :: epoch     ! epoch array
  INTEGER(i4b),DIMENSION(:)   :: mepo      ! epoch index array
  TYPE(t_clkhead)             :: ClkHead   ! header structure of CLK RINEX file
  TYPE(t_clkrec)              :: ClkRec    ! data structure of CLK RINEX file
  INTEGER(i4b)                :: icrx      ! index of clock in CLK RINEX
                                           !  0: not available
  INTEGER(i4b)                :: icrxrf    ! index of ref clock in CLK RINEX

! Output
  INTEGER(i4b)                :: ircode    !  0: all ok
                                           !  1: clocks removed
                                           !  2: all clocks removed

! LOCAL VARIABLES
! ---------------
  INTEGER(i4b)                :: ios
  INTEGER(i4b)                :: iclk,jclk
  INTEGER(i4b)                :: ipar,i1,i2,ii,jj,ifix
  INTEGER(i4b)                :: ngap,nrm,num
  INTEGER(i4b),DIMENSION(:),ALLOCATABLE :: fix

  REAL(r8b),PARAMETER         :: wgtfix=1D12   ! fixing weight
  REAL(r8b)                   :: epornx
  REAL(r8b)                   :: wgt,sump,sumc
  REAL(r8b),DIMENSION(:),ALLOCATABLE :: diag,offdia,rhs,clkfix


! debug
  CHARACTER*1,DIMENSION(:),ALLOCATABLE :: idiag,ioffdg

! Allocate Arrays
! ---------------

  ALLOCATE (diag(npar),STAT=ios)
  CALL ALCERR(ios,'diag',(/npar/),'CECOMB')
  ALLOCATE (offdia(npar),STAT=ios)
  CALL ALCERR(ios,'offdia',(/npar/),'CECOMB')
  ALLOCATE (rhs(npar),STAT=ios)
  CALL ALCERR(ios,'rhs',(/npar/),'CECOMB')
  ALLOCATE (fix(npar),STAT=ios)
  CALL ALCERR(ios,'fix',(/npar/),'CECOMB')
  ALLOCATE (clkfix(npar),STAT=ios)
  CALL ALCERR(ios,'clkfix',(/npar/),'CECOMB')
  IF (opt%debug==1) THEN
    ALLOCATE (idiag(npar),STAT=ios)
    CALL ALCERR(ios,'idiag',(/npar/),'CECOMB')
    ALLOCATE (ioffdg(npar),STAT=ios)
    CALL ALCERR(ios,'ioffdg',(/npar/),'CECOMB')
  ENDIF

! Do not use code
! ---------------
  IF (opt%nocode.EQ.1) wgtclk(1:npar)=0D0

! Set up tridiagonal matrix
! -------------------------
  CALL setdia(npar,clk,wgtclk,dclk,wgtcld,diag,offdia,rhs)


! Fix on clkrnx clocks
! --------------------
  fix(1:npar)=0
  IF (opt%ifix == 1) THEN
    IF (opt%cmbsel /= 1) THEN
      WRITE(lfnerr,"(/,' *** SR CECOMB: ifix=1 and cmbsel>1 not allowed',/)")
      CALL exitrc(2)
    ENDIF
    IF (icrx /= 0) THEN

! Loop over epochs
      iclk=1
      DO ipar=1,npar
! Find epoch in rnx file
        DO jclk=iclk,ClkRec%nEpo
          epornx=ClkHead%Tfirst+ClkRec%Epoch(jclk)/86400D0
          IF (ABS(epoch(mepo(ipar))-epornx).LT.1D-8) THEN
            IF(ClkRec%Clock(icrx,jclk)   /= UNDEF .AND. &
               ClkRec%Clock(icrxrf,jclk) /= UNDEF)THEN
! rnx clock referring to  internal reference
                clkfix(ipar)=ClkRec%Clock(icrx,jclk)*1D-6- &
                             ClkRec%Clock(icrxrf,jclk)*1D-6
              fix(ipar)=1
            ENDIF
            iclk=jclk
            EXIT
          ELSEIF (epornx > epoch(mepo(ipar))) THEN
            EXIT
          ENDIF
        ENDDO
      ENDDO
    ENDIF

!!    if (ityp==2.AND.ircode==90) then
!!      do ipar=1,npar
!!        write(lfnerr,*)ipar,epoch(mepo(ipar)),clk(ipar),dclk(ipar),wgtclk(ipar), &
!!                       wgtcld(ipar),fix(ipar),clkfix(ipar)
!!      enddo
!!    endif

! No code: remove non-fixed data pieces
! -------------------------------------
    IF (opt%remove==1 .OR. opt%nocode==1) THEN

! Remove data pieces away by dtfix from fixing epoch
! --------------------------------------------------
      IF (opt%dtfix > 0D0) THEN
        i2=1
        DO ipar=1,npar
          IF (fix(ipar) == 1) THEN
! smaller epochs
            DO ii=ipar,1,-1
              IF (epoch(mepo(ii)) < epoch(mepo(ipar))-opt%dtfix) EXIT
            ENDDO
            IF (i2>1.AND.i2<=ii) THEN
              offdia(i2-1)=0D0
              wgtcld(i2-1)=0D0
            ENDIF
            DO jj=i2,ii
              diag(jj)   = 0D0
              offdia(jj) = 0D0
            ENDDO
! larger epochs
            DO ii=ipar,npar
              IF (epoch(mepo(ii)) > epoch(mepo(ipar))+opt%dtfix) EXIT
            ENDDO
            i2=ii
          ENDIF
        ENDDO
        IF (i2>1) THEN
          offdia(i2-1)=0D0
          wgtcld(i2-1)=0D0
        ENDIF
        DO jj=i2,npar
          diag(jj)   = 0D0
          offdia(jj) = 0D0
        ENDDO
      ENDIF

! Remove not fixed data pieces
! ----------------------------
      i1=1
      ifix=0
      DO ipar=1,npar
        IF (fix(ipar) == 1) ifix=1
        IF (offdia(ipar) == 0D0) THEN
          IF (ifix==0) THEN
            IF (i1>1) THEN
              wgtcld(i1-1)=0D0
              offdia(i1-1)=0D0
            ENDIF
            DO ii=i1,ipar
              diag(ii)   = 0D0
              offdia(ii) = 0D0
            ENDDO
          ENDIF
          ifix=0
          i1=ipar+1
        ENDIF
      ENDDO
    ENDIF
  ENDIF

  if(opt%debug==1)then
    idiag ='.'
    ioffdg='.'
    do ipar=1,npar
      if(diag(ipar).ne.0)  idiag(ipar) ='X'
      if(fix(ipar) == 1)   idiag(ipar) ='M'
      if(offdia(ipar).ne.0)ioffdg(ipar)='X'
    enddo
    write(lfnplt,*)'offd: ',ioffdg(1:npar)
    write(lfnplt,*)'diag: ',idiag(1:npar)
  endif

! Remove data pieces with no observations (zero on diagonal)
! ---------------------------------------
  i1 =0
  i2 =0
  SqueezeLoop: DO
    i2=i2+1
    IF (diag(i2) /= 0D0) THEN
!
! gap of values found: shift entries in arrays
!
!            i1   i2           npar
! before   ..------|    |------------|
!                  |   /            /
!                  |  /            /
!                  | /            /
! after    ..------||------------|
!
      ngap=i2-i1-1
      IF (ngap > 0) THEN
        IF (i1 > 0) wgtcld(i1)=0D0
        DO ii=1,npar-i2+1
          diag(i1+ii)  =diag(i2-1+ii)
          mepo(i1+ii)  =mepo(i2-1+ii)
          clk(i1+ii)   =clk(i2-1+ii)
          dclk(i1+ii)  =dclk(i2-1+ii)
          wgtclk(i1+ii)=wgtclk(i2-1+ii)
          wgtcld(i1+ii)=wgtcld(i2-1+ii)
          fix(i1+ii)   =fix(i2-1+ii)
          clkfix(i1+ii)=clkfix(i2-1+ii)
        ENDDO
        DO ii=npar-ngap+1,npar
          diag(ii)=0D0
        ENDDO
        i2=i1+1
      ENDIF
      i1=i2
    ENDIF
!
! end of loop
    IF (i2==npar) THEN
      IF (diag(i2)==0D0) THEN
        nrm=npar-i1
      ELSE
        nrm=0D0
      ENDIF
      EXIT SqueezeLoop
    ENDIF
  ENDDO Squeezeloop
! nrm: total number of observations to remove
  npar=npar-nrm

! TRIDIAG Mechanism
! =================
  IF (opt%cmbsel==1) THEN

! Set diagonal and offdiagonal with shifted observations
! ------------------------------------------------------
    IF (nrm > 0) THEN
      CALL setdia(npar,clk,wgtclk,dclk,wgtcld,diag,offdia,rhs)
    ENDIF

! Apply fixing
! ------------
    IF (opt%ifix == 1) THEN
      DO ipar=1,npar
        IF (fix(ipar) == 1) THEN
          diag(ipar) = diag(ipar) + wgtfix
          rhs(ipar)  = rhs(ipar)  + clkfix(ipar)*wgtfix
        ENDIF
      ENDDO
    ENDIF

! Fix first clock if no code used and no fixing to a priori
! ---------------------------------------------------------
    IF (opt%nocode==1 .AND. opt%ifix==0) THEN
      DO ipar=1,npar
        IF (ipar==1) THEN
          diag(1) = diag(1) + wgtfix
! fix to previous value after phase loss
        ELSEIF(offdia(ipar)==0D0 .AND. ipar < npar) THEN
          diag(ipar+1) = diag(ipar+1)+wgtfix
          rhs(ipar+1)  = rhs(ipar+1) +clk(ipar)*wgtfix
        ENDIF
      ENDDO
    ENDIF

! Inversion of tridiagonal matrix
! -------------------------------
    IF (npar > 0) CALL TRIDIAG(npar,diag,offdia,rhs,clk)

! Summation Condition on Code
! ===========================
  ELSEIF (opt%cmbsel==2) THEN

! a priori weight
    IF (opt%nocode==1) THEN
      wgt=0D0
    ELSE
      wgt=(opt%aprsip/opt%aprsic)**2
    ENDIF

! Loop over connected data pieces
    i1=1
    DO WHILE (i1 < npar)
      num=0
      sumc=0D0
      sump=0D0
      DO ipar=i1,npar
        num =num+1
        sumc=sumc+clk(ipar)
        i2=ipar
        IF (wgtcld(ipar) == 0D0 .OR. ipar==npar ) EXIT
        sump=sump+(ipar-i1+1)*dclk(ipar)
      ENDDO

      sumc=sumc/num
      sump=sump/num
      DO ipar=i2,i1,-1
        IF (ipar<i2) sump=sump-dclk(ipar)
        clk(ipar)=sump+wgt*clk(ipar)
        IF (opt%nocode==0) clk(ipar)=clk(ipar)+sumc
        clk(ipar)=clk(ipar)/(1+wgt)
      ENDDO
      i1=i2+1
    ENDDO

  ELSE
    WRITE(lfnerr,"(/,' SR CECOMB: Combination selection not allowed:',I6,/)") &
                                  opt%cmbsel
    CALL exitrc(2)
  ENDIF

  DEALLOCATE(diag,  STAT=ios)
  DEALLOCATE(offdia,STAT=ios)
  DEALLOCATE(rhs,   STAT=ios)
  DEALLOCATE(fix,   STAT=ios)
  DEALLOCATE(clkfix,STAT=ios)
  IF (opt%debug==1) THEN
    DEALLOCATE(idiag, STAT=ios)
    DEALLOCATE(ioffdg,STAT=ios)
  ENDIF

  RETURN
END SUBROUTINE cecomb

END MODULE
