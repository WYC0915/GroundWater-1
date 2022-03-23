      MODULE s_DEQVR1
      CONTAINS

C*
      SUBROUTINE DEQVR1(TMJD,RHS, satpos, stdorb, act_sat)
CC
CC NAME       :  DEQVR1
CC
CC
CC PURPOSE    : PARTIAL DERIVATIVE OF FIRST TIME DERIVATIVE
CC              OF OSCULATING ELEMENTS W.R.T. DYNAMICAL PARAMETER(S)
CC              VERSION USING PARTIALS W.R.T. OSCULATING ELEMENTS
CC
CC PARAMETERS :
CC        IN  :  TMJD   : TIME IN MJD                               R*8
CC        OUT :  RHS    : RESULT                                    R*8
CC
CC AUTHOR     :  G.BEUTLER
CC
CC VERSION    :  4.1
CC
CC CREATED    :  07-JUN-00
CC
CC CHANGES    :  01-OCT-00 : DI: GEOPOTENTIAL COEFFICIENTS ADDED
CC               08-FEB-01 : DI: MERGE VERSIONS OF HB AND DI
CC               16-JAN-07 : LP: STRUCTURE gravField INTRODUCED
CC               01-OCT-10 : CR: NEW CALL OF MOSUPN
CC               17-MAR-11 : CR: CALL MOSUPN AS MODULE
CC               04-MAY-12 : RD: USE DMOD FROM MODULE, USE M_BERN WITH ONLY
CC               24-SEP-12 : RD: TAKE AU FROM D_CONST
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC      2000     UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE M_BERN,   ONLY: i4b, r8b
      USE D_CONST,  ONLY: PI, OMEGA, GM, AU
      USE p_gravdet,ONLY: m_fromto, m_numsat, m_idim, m_nvar, m_locq,
     1                    m_prcopt, m_iarc
      USE d_satdynmod,ONLY:m_crad, m_qdmrad, m_nlon, m_nlat, m_rprfac,
     1                    m_modtyp, m_ap, m_f107, m_cdrag, m_qdm
      USE m_maxdim, ONLY: MAXPOT
C
      USE s_gtd6
      USE s_dmlmtv
      USE s_mosupn
      USE s_rswvec
      USE s_vprod
      USE s_sidset
      USE s_dgpxyz
      USE s_dminv
      USE s_getdat
      USE s_accalb
      USE s_dmlmam
      USE s_readacc
      USE s_xyzele
      USE s_jmt
      USE s_polevn
      USE s_ddreh
      USE s_dmlmav
      USE s_dfdcds
      USE s_xyzell
      USE f_thetan
      USE f_djul
      USE s_exitrc
      USE s_ni_getorb
      USE s_tidalf
      USE s_gravField ,ONLY: gravField, getPot2
      USE s_stdorbit_t

      IMPLICIT NONE

      TYPE(t_stdOrb), DIMENSION(:), OPTIONAL :: stdOrb
      TYPE(t_satPos), DIMENSION(:), OPTIONAL :: satPos
      INTEGER(i4b),                 OPTIONAL :: act_sat

      REAL(r8b) :: tsec
      REAL(r8b) :: tmjd
      INTEGER(i4b) :: itmjd
      INTEGER(i4b) :: k
      INTEGER(i4b) :: i
      REAL(r8b) :: xn
      REAL(r8b) :: a
      REAL(r8b) :: xm
      REAL(r8b) :: tper
      REAL(r8b) :: exx
      REAL(r8b) :: e
      INTEGER(i4b) :: itt
      REAL(r8b) :: de
      REAL(r8b) :: v
      REAL(r8b) :: u
      REAL(r8b) :: per
      REAL(r8b) :: r
      INTEGER(i4b) :: ifirst_pot, ifirst_pot_2
      INTEGER(i4b) :: ivar
      INTEGER(i4b) :: i_deg
      REAL(r8b) :: thi
      INTEGER(i4b) :: n_per_rev
      REAL(r8b) :: xi
      REAL(r8b) :: xkn
      REAL(r8b) :: rss
      REAL(r8b) :: sclfac
      REAL(r8b) :: eeyy
      REAL(r8b) :: sidtim
      REAL(r8b) :: ut1utcl
      REAL(r8b) :: gpsutcl
      INTEGER(i4b) :: ii
      REAL(r8b) :: rrr
      REAL(r8b) :: bb
      REAL(r8b) :: xl
      REAL(r8b) :: xfac
      REAL(r8b) :: solcon
      REAL(r8b) :: asun
      REAL(r8b) :: sclscl
      REAL(r8b) :: xlat
      REAL(r8b) :: xlong
      REAL(r8b) :: radeq
      REAL(r8b) :: vel
      REAL(r8b) :: vvel
      REAL(r8b) :: tmjd0
      REAL(r8b) :: doy
      INTEGER(i4b) :: idoy
      REAL(r8b) :: utsec
      REAL(r8b) :: alphas
      REAL(r8b) :: soltim
      REAL(r8b) :: height
      REAL(r8b) :: densty
      INTEGER(i4b) :: irc
      INTEGER(i4b) :: ivar0
      INTEGER(i4b) :: mm
      INTEGER(i4b) :: jj
      REAL(r8b) :: dd
      INTEGER(i4b) :: leonum
      REAL(r8b) :: yp
      REAL(r8b) :: xp
      REAL(r8b) :: scell
      REAL(r8b) :: dummy
      REAL(r8b) :: bell
      REAL(r8b) :: ydum
      REAL(r8b) :: aell
      REAL(r8b) :: xdum
      REAL(r8b) :: dum
      REAL(r8b) :: det


      INTEGER*4 LHLP(6),MHLP(6)
      REAL*8    RHS(*),ROT1(3,3)
      REAL*8    SUNPOS(4),MOOPOS(4),RSW(3),EX(3),EY(3),EZ(3),RR(3),
     1          VV(3),ESUN(3),DUM3(3)
      REAL*8    MAT(6,6)
      REAL*8    XVOSC(42)
!
      INTEGER(i4b)             :: DEGREE,ORDER
      INTEGER(i4b),SAVE        :: IFIRST=1
      REAL(r8b),DIMENSION(3)   :: XVHPOT,DU1RLB,DU1XYZ
      REAL(r8b),DIMENSION(4)   :: DUMMYN
      REAL(r8b),DIMENSION(3,3) :: PRE,NUT,SID,DU2RLB,DU2XYZ,HELP,KapPA
      REAL(r8b),DIMENSION(3,3) :: DUMMYT
      REAL(r8b),DIMENSION(3,(MAXPOT+1)*(MAXPOT+2)/2) :: dfdc,dfds
      Real(r8b) :: apP(7)

      REAL*8    XVH(3,2),XVELL(3),DENS(8),TEMP(2)
      REAL*8    DRELL(3),DXELL(3)
      REAL*8    RSWMAT(3,3),RV(6)

      REAL(r8b),DIMENSION(3,2) :: ACCNGR
      REAL(r8b),DIMENSION(3,3) :: ERSW
      real*8    r3kn(3,3),r1i(3,3),r3u(3,3),hlp(3)
      CHARACTER*16 DATUM
C
      INTEGER(i4b),SAVE :: IFD  =1
      INTEGER(i4b),SAVE :: GTD6TEST=0
      REAL(r8b) :: ut1utc, gpsutc

ccc      GTD6TEST=GTD6TEST+1


C
C TIME IN SECONDS
C ---------------
      TSEC=(TMJD-m_fromto(1))*86400.D0
!
! INITIALIZE ITMJD FOR ACCELERATIONS
! ----------------------------------
      ITMJD = 0
      gpsutc = 0.d0
      dum    = 0.d0
      dummy  = 0.d0
      xdum   = 0.d0
      ydum   = 0.d0
      dummyN(:)   = 0.d0
      dummyT(:,:) = 0.d0
C
C ORBITAL ELEMENTS
C ----------------
C
C PARTIALS W.R.T. INITIAL CONDITIONS
C ----------------------------------
C
C POSITION AND VELOCITY AT OBSERVATION TIME "TMJD"
      call ni_getorb(m_numsat,1,tmjd,xvosc)
C
C SAVE POSITION AND VELOCITY FOR LATER USE
      DO K=1,3
        RR(K)=XVOSC(K)
        VV(K)=XVOSC(m_idim+k)
      ENDDO
C
C COMPUTE JACOBIAN
      DO I=1,3
        DO K=1,6
          MAT(I,K)  =XVOSC(I+k*3)
          MAT(3+I,K)=XVOSC(I+K*3+m_idim)
        ENDDO
      ENDDO
C
C INVERT JACOBIAN
C ---------------
      CALL DMINV(MAT,6,DET,LHLP,MHLP)
C
C OSCULATING ELEMENTS AT TIME "TMJD"
      CALL XYZELE(GM,TSEC,RR,VV,0,A,E,XI,XKN,PER,TPER)
C
C ARGUMENT OF LATITUDE U
      XN=DSQRT(GM/A**3)
      XM=XN*(TSEC-TPER)
      EXX=XM+E*DSIN(XM)
C
      DO ITT=1,6
        DE=(XM-EXX+E*DSIN(EXX))/(1.D0-E*DCOS(EXX))
        EXX=EXX+DE
      ENDDO
C
      V=2*DATAN(DSQRT((1+E)/(1-E))*DTAN(EXX/2))
C
      U=PER+V
      R=DSQRT(RR(1)**2+RR(2)**2+RR(3)**2)

C
C COMPUTE RIGHT HAND SIDES FOR ALL EQUATIONS
C ------------------------------------------
      ifirst_pot=1
      ifirst_pot_2=1
      DO IVAR=1,m_nvar
C
C RADIATION PRESSURE PARAMETERS
C -----------------------------
         IF(m_locq(4,6+IVAR,m_iarc).GE. 6+1.AND.
     1      m_locq(4,6+IVAR,m_iarc).LE.12+3)THEN
!
           IF(m_locq(7,6+IVAR,m_iarc).EQ.1)THEN
!
! R,S,W-DECOMPOSITION OF EMPIRICAL FORCES
! ---------------------------------------
            IF(m_locq(4,6+IVAR,m_iarc).EQ.6+1)THEN
              i_deg=mod(m_locq(5,6+IVAR,m_iarc),100)
              if(tmjd-m_fromto(1) /= 0.d0)then
                thi=(tmjd-m_fromto(1))**i_deg
              else
                thi=1.d0
              endif
C
C CONSTANT RADIAL TERM
              RSW(1)=1.D0*thi
              RSW(2)=0.D0
              RSW(3)=0.D0
            ELSE IF(m_locq(4,6+IVAR,m_iarc).EQ.6+2)THEN
              i_deg=mod(m_locq(5,6+IVAR,m_iarc),100)
              if(tmjd-m_fromto(1) /= 0.d0)then
                thi=(tmjd-m_fromto(1))**i_deg
              else
                thi=1.d0
              endif
C
C CONSTANT ALONG-TRACK TERM
              RSW(1)=0.D0
              RSW(2)=1.D0*thi
              RSW(3)=0.D0
            ELSE IF(m_locq(4,6+IVAR,m_iarc).EQ.6+3)THEN
              i_deg=mod(m_locq(5,6+IVAR,m_iarc),100)
              if(tmjd-m_fromto(1) /= 0.d0)then
                thi=(tmjd-m_fromto(1))**i_deg
              else
                thi=1.d0
              endif
C
C CONSTANT OUT OF PLANE TERM
              RSW(1)=0.D0
              RSW(2)=0.D0
              RSW(3)=1.D0*thi
C
C ONCE PER REV RADIAL COS-TERM
            ELSE IF(m_locq(4,6+IVAR,m_iarc).EQ.9+1)THEN
              n_per_rev=m_locq(5,6+IVAR,m_iarc)
              RSW(1)=DCOS(n_per_rev*U)
              RSW(2)=0.D0
              RSW(3)=0.D0
C
C ONCE PER REV ALONG-TRACK COS-TERM
            ELSE IF(m_locq(4,6+IVAR,m_iarc).EQ.9+2)THEN
              n_per_rev=m_locq(5,6+IVAR,m_iarc)
              RSW(1)=0.D0
              RSW(2)=DCOS(n_per_rev*U)
              RSW(3)=0.D0
C
C ONCE PER REV OUT OF PLANE COS-TERM
            ELSE IF(m_locq(4,6+IVAR,m_iarc).EQ.9+3)THEN
              n_per_rev=m_locq(5,6+IVAR,m_iarc)
              RSW(1)=0.D0
              RSW(2)=0.D0
              RSW(3)=DCOS(n_per_rev*U)
C
C ONCE PER REV RADIAL SIN-TERM
            ELSE IF(m_locq(4,6+IVAR,m_iarc).EQ.12+1)THEN
              n_per_rev=m_locq(5,6+IVAR,m_iarc)
              RSW(1)=DSIN(n_per_rev*U)
              RSW(2)=0.D0
              RSW(3)=0.D0
C
C ONCE PER REV ALONG-TRACK SIN-TERM
            ELSE IF(m_locq(4,6+IVAR,m_iarc).EQ.12+2)THEN
              n_per_rev=m_locq(5,6+IVAR,m_iarc)
              RSW(1)=0.D0
              RSW(2)=DSIN(n_per_rev*U)
              RSW(3)=0.D0
C
C ONCE PER REV OUT OF PLANE SIN-TERM
            ELSE IF(m_locq(4,6+IVAR,m_iarc).EQ.12+3)THEN
              n_per_rev=m_locq(5,6+IVAR,m_iarc)
              RSW(1)=0.D0
              RSW(2)=0.D0
              RSW(3)=DSIN(n_per_rev*U)
            ENDIF
C
C ROTATE BACK INTO EQUATORIAL SYSTEM
            CALL DDREH(3,-U,ROT1)
            CALL DMLMAV(RSW,ROT1,RSW)
C
            CALL DDREH(1,-XI,ROT1)
            CALL DMLMAV(RSW,ROT1,RSW)
C
            CALL DDREH(3,-XKN,ROT1)
            CALL DMLMAV(RSW,ROT1,RSW)
         ELSE IF(m_locq(7,6+IVAR,m_iarc).EQ.2.and.
     1           m_locq(4,6+IVAR,m_iarc).lt.16)THEN
!
! BERNESE EMPIRICAL ORBIT MODEL
! -----------------------------
            CALL MOSUPN(TMJD,2,DUM,DUM,DUM,XDUM,YDUM,DUM,DUM,
     1                DUMMYT,DUMMYT,SUNPOS,DUMMYN,dummy,DUM3)
            RSS=DSQRT((RR(1)-SUNPOS(1))**2+(RR(2)-SUNPOS(2))**2
     1               +(RR(3)-SUNPOS(3))**2)
            DO K=1,3
              EZ(K)=-RR(K)/R
              ESUN(K)=(SUNPOS(K)-RR(K))/RSS
            ENDDO
C            SCLFAC=(1.49597870D11/RSS)**2
            SCLFAC=(AU/RSS)**2
C
C UNIT VECTOR EY:
            CALL VPROD(EZ,ESUN,EY)
            EEYY=DSQRT(EY(1)**2+EY(2)**2+EY(3)**2)
            DO K=1,3
              EY(K)=EY(K)/EEYY
            ENDDO
C
C UNIT VECTOR EX:
            CALL VPROD(ESUN,EY,EX)
C
            IF(m_locq(4,6+IVAR,m_iarc).EQ.6+1)THEN
              i_deg=mod(m_locq(5,6+IVAR,m_iarc),100)
              if(tmjd-m_fromto(1) /= 0.d0)then
                thi=(tmjd-m_fromto(1))**i_deg
              else
                thi=1.d0
              endif
C
C CONSTANT TERM IN DIRECTION SUN-SATELLITE
              DO K=1,3
                RSW(K)=ESUN(K)*thi*SCLFAC
              ENDDO
            ELSE IF(m_locq(4,6+IVAR,m_iarc).EQ.6+2)THEN
C
C CONSTANT TERM IN Y-DIRECTION
              i_deg=mod(m_locq(5,6+IVAR,m_iarc),100)
              if(tmjd-m_fromto(1) /= 0.d0)then
                thi=(tmjd-m_fromto(1))**i_deg
              else
                thi=1.d0
              endif
              DO K=1,3
                RSW(K)=EY(K)*thi*SCLFAC
              ENDDO
            ELSE IF(m_locq(4,6+IVAR,m_iarc).EQ.6+3)THEN
C
C CONSTANT TERM IN X-DIRECTION
              i_deg=mod(m_locq(5,6+IVAR,m_iarc),100)
              if(tmjd-m_fromto(1) /= 0.d0)then
                thi=(tmjd-m_fromto(1))**i_deg
              else
                thi=1.d0
              endif
              DO K=1,3
                RSW(K)=EX(K)*thi*SCLFAC
              ENDDO
C
C ONCE PER REV, COS-TERM IN DIRECTION OF SUN
            ELSE IF(m_locq(4,6+IVAR,m_iarc).EQ.9+1)THEN
              n_per_rev=m_locq(5,6+IVAR,m_iarc)
              DO K=1,3
                RSW(K)=ESUN(K)*DCOS(n_per_rev*U)*SCLFAC
              ENDDO
C
C ONCE PER REV, COS-TERM IN Y-DIRECTION
            ELSE IF(m_locq(4,6+IVAR,m_iarc).EQ.9+2)THEN
              n_per_rev=m_locq(5,6+IVAR,m_iarc)
              DO K=1,3
                RSW(K)=EY(K)*DCOS(n_per_rev*U)*SCLFAC
              ENDDO
C
C ONCE PER REV, COS TERM IN X-DIRECTION
            ELSE IF(m_locq(4,6+IVAR,m_iarc).EQ.9+3)THEN
              n_per_rev=m_locq(5,6+IVAR,m_iarc)
              DO K=1,3
                RSW(K)=EX(K)*DCOS(n_per_rev*U)*SCLFAC
              ENDDO
C
C ONCE PER REV, SIN-TERM IN DIRECTION OF SUN
            ELSE IF(m_locq(4,6+IVAR,m_iarc).EQ.12+1)THEN
              n_per_rev=m_locq(5,6+IVAR,m_iarc)
              DO K=1,3
                RSW(K)=ESUN(K)*DSIN(n_per_rev*U)*SCLFAC
              ENDDO
C
C ONCE PER REV, SIN-TERM IN Y-DIRECTION
            ELSE IF(m_locq(4,6+IVAR,m_iarc).EQ.12+2)THEN
              n_per_rev=m_locq(5,6+IVAR,m_iarc)
              DO K=1,3
                RSW(K)=EY(K)*DSIN(n_per_rev*U)*SCLFAC
              ENDDO
C
C ONCE PER REV, COS TERM IN X-DIRECTION
            ELSE IF(m_locq(4,6+IVAR,m_iarc).EQ.12+3)THEN
              n_per_rev=m_locq(5,6+IVAR,m_iarc)
              DO K=1,3
                RSW(K)=EX(K)*DSIN(n_per_rev*U)*SCLFAC
              ENDDO
            ENDIF
          ENDIF
        ELSE IF(m_locq(1,6+IVAR,m_iarc)==13)THEN

! GEOPOTENTIAL COEFFICIENTS
! -------------------------
! READ AE,GM
          IF(IFIRST==1) THEN
            IFIRST=0
!!!!            CALL getPot2(TMJD)
          ENDIF

! TRANSFORM SATELLITE POSITION INTO EARTH FIXED SYSTEM
          if(ifirst_pot == 1)then
!!!            CALL MOSUPN(TMJD,1,DUMMY,DUMMY,DUMMY,XP,
!!!     1                  YP,ut1utcL,gpsutcL,PRE,NUT,DUMMYN,DUMMYN,
!!!     &                  dummy)
            CALL MOSUPN(TMJD,-1,dummy,dummy,dummy,XP,
     1                  YP,UT1UTCL,GPSUTCL,PRE,NUT,SUNPOS,MOOPOS,dummy,
     2                  DUM3)
            CALL DMLMAV(RR,PRE,XVHPOT)
            CALL DMLMAV(XVHPOT,NUT,XVHPOT)
            SIDTIM=THETAN(TMJD+ut1utcL-gpsutcL)+NUT(2,1)
            CALL SIDSET(SIDTIM,XP,YP,SID)
            CALL DMLMAV(XVHPOT,SID,XVHPOT)
            ifirst_pot=0
          endif
!
          IF(m_locq(4,6+IVAR,m_iarc) <=2)THEN
!
            DEGREE=m_locq(5,6+IVAR,m_iarc)
            ORDER =m_locq(6,6+IVAR,m_iarc)
! all partials w.r.t. gravity field parms when first gravity field parameter is encountered
            if(ifirst_pot_2 == 1)then
              call dfdcds(gravField%ae,gravField%gm,m_prcopt(4),xvhpot,
     1                    dfdc,dfds)
              ifirst_pot_2=0
            endif
!
! PARTIAL DERIVATIVES, COS-TERMS
            IF(m_locq(4,6+IVAR,m_iarc)==1) THEN
              II=DEGREE*(DEGREE+1)/2+1+ORDER
              du1rlb(1:3)=dfdc(1:3,ii)
            ELSE
!
! PARTIAL DERIVATIVES, SIN-TERMS
              II=DEGREE*(DEGREE+1)/2+1+ORDER
              du1rlb(1:3)=dfds(1:3,ii)
            ENDIF

c cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c compute Term manually for n=m=2:
c terms match "spucke-mucke"
c cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccc          write(*,*)'du1rlb=',du1rlb
ccc          rrr=sqrt(xvhpot(1)**2+xvhpot(2)**2+xvhpot(3)**2)
ccc            bb=asin(xvhpot(3)/rrr)
ccc            xl=atan2(xvhpot(2),xvhpot(1))
ccc            xfac=3*sqrt(5.d0/12.d0)
ccc        if(m_locq(4,6+IVAR,m_iarc)==1)then
ccc      du1xyz(1)=-3*xfac*gravField%gm*(gravField%ae/rrr)**2/rrr
ccc     1          **2*cos(bb)**2*cos(2*xl)
ccc      du1xyz(2)=-2*xfac*gravField%gm*(gravField%ae/rrr)**2/rrr
ccc     1          *cos(bb)**2*sin(2*xl)
ccc      du1xyz(3)=-2*xfac*gravField%gm*(gravField%ae/rrr)**2/rrr
ccc     1          *cos(bb)*sin(bb)*cos(2*xl)
ccc            else
ccc      du1xyz(1)=-3*xfac*gravField%gm*(gravField%ae/rrr)**2/rrr
ccc     1          **2*cos(bb)**2*sin(2*xl)
ccc      du1xyz(2)=+2*xfac*gravField%gm*(gravField%ae/rrr)**2/rrr
ccc     1          *cos(bb)**2*cos(2*xl)
ccc      du1xyz(3)=-2*xfac*gravField%gm*(gravField%ae/rrr)**2/rrr
ccc     1          *cos(bb)*sin(bb)*sin(2*xl)
ccc            endif
ccc               write(*,*)'fracts=',du1rlb(1)/du1xyz(1),
ccc        1       du1rlb(2)/du1xyz(2),
ccc        1       du1rlb(3)/du1xyz(3)
c cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!
!  DERIVATIVES OF EARTH POTENTIAL WITH RESPECT TO X,Y,Z
            CALL DGPXYZ(XVHPOT,1,DU1RLB,DU2RLB,DU1XYZ,DU2XYZ)
ccc          write(*,*)'du1xyz=',du1xyz
!
! TRANSFORMATION BACK TO SYSTEM J2000.0
            CALL DMLMAM(NUT,PRE,HELP)
            CALL DMLMAM(SID,HELP,KapPA)
            CALL DMLMTV(DU1XYZ,KapPA,RSW)
          ELSEIF(m_locq(4,6+IVAR,m_iarc) == 3)THEN
            RSW(1:3)=0.d0
            CALL TIDALF(1.d0,RR,SUNPOS,MOOPOS,RSW)
            ifirst_pot=0
          ENDIF
!
! DIRECT RADIATION PRESSURE SCALING FACTOR
! ----------------------------------------
        ELSE IF(m_locq(4,6+IVAR,m_iarc).EQ.19)THEN
          CALL MOSUPN(TMJD,2,DUM,DUM,DUM,XDUM,YDUM,DUM,DUM,
     1                DUMMYT,DUMMYT,SUNPOS,DUMMYN,dummy,DUM3)
          SOLCON=4.56D-6
!          ASUN=1.49597870D+11
          ASUN=AU

          RSS=DSQRT((RR(1)-SUNPOS(1))**2+(RR(2)-SUNPOS(2))**2
     1                +(RR(3)-SUNPOS(3))**2)
C          SCLFAC=(1.49597870D11/RSS)**2
          SCLFAC=(AU/RSS)**2
C
C CLASSICAL RPR-MODEL
C -------------------
          if(r < 9000000.d0)then
            sclscl=(sunpos(1)*rr(1)+sunpos(2)*rr(2)
     1                             +sunpos(3)*rr(3))/r/rss
            sclscl=abs(sclscl)
          else
            sclscl=1
          endif

! Test: spherical instead of flat satellite
          sclscl =1
!
          DO K=1,3
            RSW(K)=-m_crad/2*m_qdmrad*(ASUN/SUNPOS(4))**2*SOLCON*
     1                  (SUNPOS(K)-RR(K))/RSS*SCLFAC*sclscl
          ENDDO
!
! EARTH m_albedo
! ------------
        ELSE IF(m_locq(4,6+IVAR,m_iarc).EQ.27)THEN
          CALL MOSUPN(TMJD,2,DUM,DUM,DUM,XDUM,YDUM,DUM,DUM,
     1                DUMMYT,DUMMYT,SUNPOS,DUMMYN,dummy,DUM3)
          RV(1:3)=RR(1:3)
          RV(4:6)=VV(1:3)
          CALL RSWVEC(RV,RSWMAT(1,1),RSWMAT(1,2),RSWMAT(1,3))
          CALL ACCALB(1D0,m_nlon,m_nlat,RR,SUNPOS,RSWMAT,
     1                m_rprfac,LEONUM,m_modtyp,RSW)
!
! DRAG SCALING FACTOR
! -------------------
        ELSE IF(m_locq(4,6+IVAR,m_iarc).EQ.20)THEN

C
C GET PRECESSION, NUTATION, POLAR COORDINATES, TIME SCALES, SOLAR COORDINATES

          CALL MOSUPN(TMJD,1,DUM,DUM,DUM,XP,YP,ut1utc,gpsutc,
     1                PRE,NUT,DUMMYN,DUMMYN,dummy,DUM3)
          CALL MOSUPN(TMJD,2,DUM,DUM,DUM,XDUM,YDUM,DUM,DUM,
     1                DUMMYT,DUMMYT,SUNPOS,DUMMYN,dummy,DUM3)
C
C TRANSFORMATION INTO EARTH-FIXED SYSTEM
          CALL DMLMAV(RR,PRE,XVH)
          CALL DMLMAV(XVH,NUT,XVH)
          SIDTIM=THETAN(TMJD+ut1utc-gpsutc)+NUT(2,1)
          CALL SIDSET(SIDTIM,XP,YP,SID)
          CALL DMLMAV(XVH,SID,XVH)
C
C READ GEODETIC DATUM UPON FIRST CALL
          IF(IFD.EQ.1)THEN
            IFD=0
            DATUM='WGS - 84'
            CALL GETDAT(DATUM,AELL,BELL,DXELL,DRELL,SCELL)
          END IF
C
C ELLIPSOIDAL COORDINATES
          CALL XYZELL(AELL,BELL,DXELL,DRELL,SCELL,XVH,XVELL)
          XLAT =XVELL(1)
          XLONG=XVELL(2)
C
C TRANSFORM VELOCITY INTO EARTH-FIXED SYSTEM
          CALL DMLMAV(VV,PRE,XVH(1,2))
          CALL DMLMAV(XVH(1,2),NUT,XVH(1,2))
          CALL DMLMAV(XVH(1,2),SID,XVH(1,2))
C
C RELATIVE VELOCITY IN ROTATING SYSTEM
          RADEQ    = DSQRT(XVH(1,1)**2+XVH(2,1)**2)
          XVH(1,2) = XVH(1,2)+RADEQ*OMEGA*DSIN(XLONG)
          XVH(2,2) = XVH(2,2)-RADEQ*OMEGA*DCOS(XLONG)
C
C VELOCITY
          VEL=DSQRT(XVH(1,2)**2+XVH(2,2)**2+XVH(3,2)**2)
          VVEL=DSQRT(VV(1)**2+VV(2)**2+VV(3)**2)
C
C DAY OF YEAR
          CALL JMT(TMJD,JJ,MM,DD)
          TMJD0=DJUL(JJ,1,0.D0)
          DOY=TMJD-TMJD0
          IDOY=DOY
C
C UTC IN SEC
          UTSEC=(DOY-IDOY)*86400.D0
C
C RIGHT ASCENSION OF SUN
          ALPHAS=DATAN2(SUNPOS(2),SUNPOS(1))
C
C HOUR ANGLE OF SUN, SOLAR TIME (HOURS)
          SOLTIM=(SIDTIM-ALPHAS+XLONG)*12/PI+12.D0
          SOLTIM=DMOD(SOLTIM,24.D0)
          IF(SOLTIM.LT.0.D0)SOLTIM=SOLTIM+24.D0
C
C LATITUDE AND LONGITUDE IN DEG, HEIGHT IN KM
          XLAT  =180/PI*XVELL(1)
          XLONG =180/PI*XVELL(2)
          HEIGHT=XVELL(3)/1000.D0

C
C DENSITY OF ATMOSPHERE (G/CM**3) FROM MSIS-E90
          app(1:7)=m_ap
          CALL GTD6(IDOY,UTSEC,HEIGHT,XLAT,XLONG,SOLTIM,
     1                m_f107,m_f107,apP,48,DENS,TEMP)
C
C DENSITY IM KG/M**3
          DENSTY=1000.D0*DENS(6)
C
C DERIVATIVE OF ACCELERATION DUE TO AIR DRAG W.R.T. SCALING PARAMETER
          RSW=-m_cdrag/2*m_qdm*DENSTY*VEL**2*VV(1:3)/VVEL

!
! OFFSETS FOR ACCELERATION DATA
! -----------------------------
        ELSE IF(m_locq(4,6+IVAR,m_iarc).EQ.21)THEN
          IF (ITMJD.EQ.0) THEN
            CALL readacc(tmjd,accngr,irc)
            ITMJD=1
          END IF
          IF (IRC.EQ.0) THEN
            RV(1:3)=RR(1:3)
            RV(4:6)=VV(1:3)
            CALL RSWVEC(RV,ERSW(1,1),ERSW(1,2),ERSW(1,3))

!          TT=DMOD(TMJD,1.D0)+18481.D0 ! ATTENTION!!!! only for doy 220/2000

            RSW(1:3)=ERSW(1:3,1)
!     1             *(1+7.611D-6*dexp(-(TT-18470)/7.675)/7.675)
          ELSE
            RSW(1:3)=0.D0
          END IF
        ELSE IF(m_locq(4,6+IVAR,m_iarc).EQ.22)THEN
          IF (ITMJD.EQ.0) THEN
            CALL readacc(tmjd,accngr,irc)
            ITMJD=1
          END IF
          IF (IRC.EQ.0) THEN
            RV(1:3)=RR(1:3)
            RV(4:6)=VV(1:3)
            CALL RSWVEC(RV,ERSW(1,1),ERSW(1,2),ERSW(1,3))

            RSW(1:3)=ERSW(1:3,2)
          ELSE
            RSW=0.D0
          END IF
        ELSE IF(m_locq(4,6+IVAR,m_iarc).EQ.23)THEN
          IF (ITMJD.EQ.0) THEN
            CALL readacc(tmjd,accngr,irc)
            ITMJD=1
          END IF
          IF (IRC.EQ.0) THEN
            RV(1:3)=RR(1:3)
            RV(4:6)=VV(1:3)
            CALL RSWVEC(RV,ERSW(1,1),ERSW(1,2),ERSW(1,3))

            RSW(1:3)=ERSW(1:3,3)
          ELSE
            RSW=0.D0
          END IF
!
! SCALING FACTOR FOR ACCELERATION DATA
        ELSE IF(m_locq(4,6+IVAR,m_iarc).EQ.24)THEN
          IF (ITMJD.EQ.0) THEN
            CALL readacc(tmjd,accngr,irc)
            ITMJD=1
          END IF
          IF (irc==0) THEN
!
! TRANSFORM ACCELERATION INTO INERTIAL SYSTEM
            RV(1:3)=RR(1:3)
            RV(4:6)=VV(1:3)
            CALL RSWVEC(RV,ERSW(1,1),ERSW(1,2),ERSW(1,3))
!
! TRANSFORMATION
! --------------
            DO K=1,3
              RSW(K)=ERSW(K,1)*ACCNGR(1,1)/1000.D0
            ENDDO
          ELSE
            RSW=0.D0
          END IF
        ELSE IF(m_locq(4,6+IVAR,m_iarc).EQ.25)THEN

          IF (ITMJD.EQ.0) THEN
            CALL readacc(tmjd,accngr,irc)
            ITMJD=1
          END IF
          IF (irc==0) THEN
!
! TRANSFORM ACCELERATION INTO INERTIAL SYSTEM
            RV(1:3)=RR(1:3)
            RV(4:6)=VV(1:3)
            CALL RSWVEC(RV,ERSW(1,1),ERSW(1,2),ERSW(1,3))
!
! TRANSFORMATION
! --------------
            DO K=1,3
              RSW(K)=ERSW(K,2)*ACCNGR(2,1)/1000.D0
            ENDDO
          ELSE
            RSW=0.D0
          END IF

        ELSE IF(m_locq(4,6+IVAR,m_iarc).EQ.26)THEN
          IF (ITMJD.EQ.0) THEN
            CALL readacc(tmjd,accngr,irc)
            ITMJD=1
          END IF
          IF (irc==0) THEN
!
! TRANSFORM ACCELERATION INTO INERTIAL SYSTEM
! UNIT VECTOR RADIAL ER
! ---------------------
!            ERSW(1,:)=RR(1:3)/R
!
! UNIT VECTOR ALONGTRACK ES
!            VVSAT=VV(1)**2+VV(2)**2+VV(3)**2
!            VRSAT=DOT_PRODUCT(VV(1:3),ERSW(1,:))
!            ERSW(2,:)=(VV(1:3)-VRSAT*ERSW(1,:))/DSQRT(VVSAT-VRSAT**2)
!
! UNIT VECTOR CROSSTRACK EW
!            CALL VPROD(ERSW(1,:),ERSW(2,:),ERSW(3,:))
            RV(1:3)=RR(1:3)
            RV(4:6)=VV(1:3)
            CALL RSWVEC(RV,ERSW(1,1),ERSW(1,2),ERSW(1,3))
!
! TRANSFORMATION
! --------------
            DO K=1,3
              RSW(K)=ERSW(K,3)*ACCNGR(3,1)/1000.D0
            ENDDO
          ELSE
            RSW=0.D0
          END IF

        ENDIF
C
C EQUATION FOR A
        IVAR0=(IVAR-1)*6
C
C COMPUTE DERIVATIVES
C -------------------
        DO I=1,6
          RHS(IVAR0+I)=0.D0
          DO K=3,1,-1
            RHS(IVAR0+I)=RHS(IVAR0+I)+MAT(I,3+K)*RSW(K)
          ENDDO
          rhs(ivar0+i)=rhs(ivar0+i)*86400.D0
        ENDDO
C
!
! write rhs for y-bias
!        if(m_locq(4,6+IVAR,m_iarc).EQ.17)then
!        write(lfnprt,4747)tmjd,(rhs(k+ivar0),k=1,6),(rsw(k),k=1,3)
! 4747     format(f15.7,9d13.5)
!        end if
!
      ENDDO
999   CONTINUE
      RETURN

      END SUBROUTINE

      END MODULE
