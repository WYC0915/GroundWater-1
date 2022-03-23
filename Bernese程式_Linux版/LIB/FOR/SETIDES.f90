MODULE s_SETIDES
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE SETIDES(SUN,MOON,pre,nut,sid,beta,cpot,spot)

! -------------------------------------------------------------------------
! Purpose  :  Solid Earth tide model corrections
!
! Author   :  H. Bock
!
! Created  :  22-Jan-2007
!
! Changes  :  17-Apr-2007 HB: Stop, if no SETIDES-file found
!             13-Jan-2009 RD: Use '' as EOF-identifier in NEXTLINE
!             20-Jan-2011 HB: Add Chapter and equation numbers from
!                             IERS Conventions
!             07-Jun-2011 HB: Update of descriptions
!             20-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! ------------------------------------------------------------------------

! Modules
! -------
    USE m_bern,  ONLY: i4b, r8b, lfnerr, lfnloc, &
                       fileNameLength, lineLength
    USE d_const, ONLY: ae, gm, gmm, gms
    USE s_gtflna
    USE s_opnfil
    USE s_opnerr
    USE s_exitrc
    USE s_alcerr
    USE s_nutarg
    USE s_dmlmav
    USE f_lengt0
    USE f_nextline
    IMPLICIT NONE

! Parameters
! ----------
! IN:
    REAL(r8b),DIMENSION(*)   :: SUN
    REAL(r8b),DIMENSION(*)   :: MOON
    REAL(r8b),DIMENSION(3,*) :: pre
    REAL(r8b),DIMENSION(3,*) :: nut
    REAL(r8b),DIMENSION(3,*) :: sid
    REAL(r8b),DIMENSION(*)   :: beta
! IN/OUT:
    REAL(r8b),DIMENSION(*)   :: cpot
    REAL(r8b),DIMENSION(*)   :: spot

! Variables
! ---------
    CHARACTER(LEN=lineLength)     :: line
    CHARACTER(LEN=80)             :: title
    CHARACTER(LEN=fileNameLength) :: filtpo
    CHARACTER(LEN=16)             :: tponam
    CHARACTER(LEN=16)             :: elastic

    REAL(r8b),DIMENSION(:),ALLOCATABLE,SAVE :: Aip,Aop
    REAL(r8b),DIMENSION(7),SAVE   :: rek
    REAL(r8b),DIMENSION(7),SAVE   :: imk
    REAL(r8b),DIMENSION(7),SAVE   :: kpl
    REAL(r8b),DIMENSION(7)        :: pnm
    REAL(r8b),DIMENSION(4)        :: coslg,sinlg,facj
    REAL(r8b),DIMENSION(4)        :: xsun,xmoon
    REAL(r8b),DIMENSION(3)        :: xsm
    REAL(r8b)                     :: the,aer3,aer4,gsm,rr
    REAL(r8b)                     :: sinphi,cosphi,xlong

    INTEGER(i4b),DIMENSION(:,:),ALLOCATABLE,SAVE ::  fBet
    INTEGER(i4b),SAVE                            ::  nTPO
    INTEGER(i4b),SAVE                            ::  iFirst=1
    INTEGER(i4b)                  :: irc,iac,iostat,ism
    INTEGER(i4b)                  :: ii,jj,kk,nn,mm

! First call of SR
! ----------------
    IF (iFirst==1) THEN
      iFirst=0

! Initialization
! --------------
      rek(1:7)=0.D0
      imk(1:7)=0.D0
      kpl(1:7)=0.D0

! Open solid Earth tides file
! ---------------------------
      CALL gtflna(0,'SETIDES ',filtpo,irc)
      IF (irc /= 0) THEN
        WRITE(lfnerr,'(//,A,//)')' *** SR SETIDES: NO SETIDES-FILE FOUND'
        CALL exitrc(2)
      ENDIF
      CALL opnfil(lfnloc,filtpo,'OLD','FORMATTED','READONLY',' ',iostat)
      CALL opnerr(lfnerr,lfnloc,iostat,filtpo,'SETIDES')

! Read title lines and solid Earth tides model name
! -------------------------------------------------
      READ(LFNLOC,'(A80)')title

      line=nextline(lfnloc,1)
      IF (line(1:24)/='SOLID EARTH TIDE MODEL: ') THEN
        WRITE(lfnerr,'(//,A,A,/,A,/)')&
             ' *** SR SETIDES: FILE ',TRIM(filtpo),&
             '                 HAS A WRONG CONTENT!'
        CALL exitrc(2)
      ENDIF

! Read model name
! ---------------
      READ(line(25:41),'(A)')tponam

! Read ELASTIC/ANELASTIC
! ----------------------
      line=nextline(lfnloc,1)
      READ(line(25:41),'(A)')elastic

! Read Love numbers
! -----------------
      ii=0
      DO
        line=nextline(lfnloc,1)
        IF (line      == '' )EXIT
        IF (line(1:13)=='LOVE NUMBERS:')EXIT
      ENDDO
      DO
        line=nextline(lfnloc,1)
        IF (line      =='')EXIT
        IF (line(1:11)=='AMPLITUDES:')EXIT
        ii=ii+1
        IF (ii > SIZE(rek)) THEN
          WRITE(lfnerr,'(//,A,/,A,A,/)')&
               ' ### SR SETIDES: TOO MANY LOVE NUMBERS',&
               '                 IN FILE: ', TRIM(filtpo)
          CALL exitrc(2)
        ENDIF
        READ(LINE,*) nn,mm,&
         rek((nn*(nn+1))/2+mm-2),imk((nn*(nn+1))/2+mm-2),kpl((nn*(nn+1))/2+mm-2)
      ENDDO

! Count number of terms
! ---------------------
      ii=0
      DO
        line=nextline(lfnloc,0)
        IF (line=='')EXIT
        ii=ii+1
      ENDDO
      nTpo=ii
      REWIND(lfnloc)

! Allocate arrays for amplitudes
! ------------------------------
      ALLOCATE(fBet(6,nTpo),stat=iac)
      CALL alcerr(iac, 'fBet', (/6,nTpo/), 'setides')
      ALLOCATE(Aip(nTpo),stat=iac)
      CALL alcerr(iac, 'Aip', (/nTpo/), 'setides')
      ALLOCATE(Aop(nTpo),stat=iac)
      CALL alcerr(iac, 'Aop', (/nTpo/), 'setides')

! Read file again and save amplitudes
! -----------------------------------
      DO
        line=nextline(lfnloc,1)
        IF (line      =='')EXIT
        IF (line(1:11)=='AMPLITUDES:')EXIT
      ENDDO
      ii=0
      DO
        line=nextline(lfnloc,0)
        IF (line == '')EXIT
        ii=ii+1
        READ(LINE(10:LENGT0(LINE)),*)fBet(1:6,ii),Aip(ii),Aop(ii)
        Aip(ii)=Aip(ii)*1D-12
        Aop(ii)=Aop(ii)*1D-12
      ENDDO
      CLOSE(LFNLOC)
    ENDIF

! Transformation into Earth-fixed system
! --------------------------------------
    CALL dmlmav(SUN,pre,xsun)
    CALL dmlmav(xsun,nut,xsun)
    CALL dmlmav(xsun,sid,xsun)
    CALL dmlmav(MOON,pre,xmoon)
    CALL dmlmav(xmoon,nut,xmoon)
    CALL dmlmav(xmoon,sid,xmoon)

! Step 1 corrections, IERS TN No.36, Ch. 6.2.1, Eqs.6.6 and 6.7, Tab.6.3
! ----------------------------------------------------------------------
! Loop over SUN and MOON:
    DO ism=1,2
      facj=0.D0
      pnm=0.D0
      coslg=0.D0
      sinlg=0.D0

! SUN or MOON?
      IF (ism == 1) THEN
        gsm=GMM
        DO kk=1,3
          xsm(kk)=xmoon(kk)
        ENDDO
      ELSE
        gsm=gms
        DO kk=1,3
          xsm(kk)=xsun(kk)
        ENDDO
      ENDIF
      rr=DSQRT(xsm(1)**2+xsm(2)**2+xsm(3)**2)
      sinphi=xsm(3)/rr
      cosphi=DSQRT(1.D0-sinphi**2)
      xlong =DATAN2(xsm(2),xsm(1))
      aer3=(ae/rr)**3

! Legendre Polynomials
! --------------------
      pnm(1)=DSQRT(5.D0)       * 0.5D0*(3.D0*sinphi**2-1.D0)        ! P20
      pnm(2)=DSQRT(5.D0/3.D0)  *  3.D0*sinphi*DSQRT(1.D0-sinphi**2) ! P21
      pnm(3)=DSQRT(5.D0/12.D0) *  3.D0*(1.D0-sinphi**2)             ! P22
      pnm(4)=DSQRT(7.D0)       * 0.5D0*(5.D0*sinphi**3-3.D0*sinphi) ! P30
      pnm(5)=DSQRT(7.D0/6.D0)  * 1.5D0*(5.D0*sinphi**2-1.D0)*cosphi ! P31
      pnm(6)=DSQRT(7.D0/60.D0) * 15.D0*sinphi*cosphi**2             ! P32
      pnm(7)=DSQRT(7.D0/360.D0)* 15.D0*cosphi**3                    ! P33

      DO mm=0,2
        facj(mm+1)=gsm*aer3*pnm(mm+1)/(gm*5)

! SINE and COSINE
        sinlg(mm+1)=dsin(mm*xlong)
        coslg(mm+1)=dcos(mm*xlong)
! CPOT:
! -----
! TERMS WITH N=2
        cpot(4+mm)=cpot(4+mm)+ rek(mm+1)*facj(mm+1)*coslg(mm+1)&
                             + imk(mm+1)*facj(mm+1)*sinlg(mm+1)

! TERMS WITH N=4, INDUCED FROM N=2
        cpot(11+mm)=cpot(11+mm)+ kpl(mm+1)*facj(mm+1)*coslg(mm+1)

! SPOT:
! -----
! TERMS WITH N=2
        IF (mm >0) THEN
          spot(4+mm)=spot(4+mm)+ rek(mm+1)*facj(mm+1)*sinlg(mm+1)&
                               - imk(mm+1)*facj(mm+1)*coslg(mm+1)
! TERMS WITH N=4, INDUCED FROM N=2
          spot(11+mm)=spot(11+mm)+ kpl(mm+1)*facj(mm+1)*sinlg(mm+1)
        ENDIF
      ENDDO

! TERMS WITH N=3
      aer4=aer3*ae/rr
      facj=0.D0
      DO mm=0,3
        facj(mm+1)=gsm*aer4*pnm(mm+4)/(gm*7)
! SINE and COSINE
        IF (mm == 3) THEN
          sinlg(mm+1)=dsin(mm*xlong)
          coslg(mm+1)=dcos(mm*xlong)
        ENDIF
        cpot(7+mm)=cpot(7+mm)+ rek(mm+4)*facj(mm+1)*coslg(mm+1)&
                             + imk(mm+4)*facj(mm+1)*sinlg(mm+1)
        IF (mm>0) THEN
          spot(7+mm)=spot(7+mm)+ rek(mm+4)*facj(mm+1)*sinlg(mm+1)&
                               - imk(mm+4)*facj(mm+1)*coslg(mm+1)
        ENDIF

      ENDDO
    ENDDO

! STEP 2 CORRECTIONS, Ch. 6.2.1, Eq.6.8
! -------------------------------------
    DO ii=1,nTPO
      the=0.D0
      DO jj=1,6
        the=the+fBet(jj,ii)*beta(jj)
      ENDDO
      IF (fBet(1,ii)==0) THEN
        cpot(4)=cpot(4)+Aip(ii)*dcos(the)-Aop(ii)*dsin(the)
      ELSEIF (fBet(1,ii)==1) THEN
        cpot(5)=cpot(5)+Aip(ii)*dsin(the)+Aop(ii)*dcos(the)
        spot(5)=spot(5)+Aip(ii)*dcos(the)-Aop(ii)*dsin(the)
      ELSEIF (fBet(1,ii)==2) THEN
        cpot(6)=cpot(6)+Aip(ii)*dcos(the)-Aop(ii)*dsin(the)
        spot(6)=spot(6)+Aip(ii)*dsin(the)-Aop(ii)*dcos(the)
      ENDIF
    ENDDO

  END SUBROUTINE SETIDES

END MODULE s_SETIDES
