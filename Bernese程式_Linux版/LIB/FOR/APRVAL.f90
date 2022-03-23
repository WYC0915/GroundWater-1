MODULE s_aprval
  CONTAINS

! ------------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! ------------------------------------------------------------------------------

  SUBROUTINE aprval(TOBS,MXCLCQ,NPAR,LOCQ,ISNG,INDA,MAXEQN,&
                    NDIFF,IFRQ,NKIN,STKIN,NSTAT,STNAME,STANAM,SAFLEP,&
                    WGSAPR,DTFIL,SATCLK,CLKHED,XXX0,XREF)

! ------------------------------------------------------------------------------
!
!  Purpose    :  Store a priori values of epoch parameters in GPSEST
!
!  Author     :  H. Bock
!
!  Created    :  07-Jun-2006
!
!  Changes    :  02-Aug-2006 HB: Put MXCLCQ into parameter list (problems with
!                                NAG compiler on ubecx)
!                04-Feb-2008 HB: Check if reference station or satellite clock
!                                is already in list
!                27-Mar-2008 HB: Correction for kinematic mode for second
!                                station in baseline
!                05-Sep-2010 RD: Reference clocks added
!                28-Mar-2012 RD: Use SVN2CHR as module now
!                28-Mar-2012 RD: Remove unused variables
!
!  Copyright  :  Astronomical Institute
!                University of Bern
!                Switzerland
! ------------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnerr, staNameLength
  USE d_par,    ONLY: t_par
  USE d_clkrnx, ONLY: t_clkhead
  USE s_exitrc
  USE s_svn2chr

  IMPLICIT NONE

!
! Parameters
! ==========
!
! IN :
! ----
  REAL(r8b)    :: tobs
  INTEGER(i4b) :: mxclcq
  INTEGER(i4b) :: npar
  INTEGER(i4b),DIMENSION(MXCLCQ,*) :: locq
  INTEGER(i4b) :: iSng
  INTEGER(i4b),DIMENSION(*) :: INDA
  INTEGER(i4b) :: maxEqn
  INTEGER(i4b) :: nDiff
  INTEGER(i4b) :: nKin
  INTEGER(i4b),DIMENSION(*) :: stKin
  INTEGER(i4b) :: nStat
  CHARACTER(LEN=staNameLength),DIMENSION(*) :: stName
  CHARACTER(LEN=staNameLength),DIMENSION(2) :: staNam
  INTEGER(i4b) :: saFlEp

  REAL(r8b),DIMENSION(3,*) :: wgsApr
  REAL(r8b),DIMENSION(2)   :: dtfil
  REAL(r8b)                :: satClk

  TYPE(t_clkHead):: clkHed

! OUT:
! ----
  REAL(r8b),DIMENSION(*) :: xxx0
  TYPE(t_par),DIMENSION(*) :: xRef

! Local variables
! ---------------
  CHARACTER(LEN=staNameLength) :: satNam
  CHARACTER(LEN=1)  :: svnchr

  REAL(r8b), SAVE   :: tobsSav = 1.D20

  INTEGER(i4b)      :: svnnum
  INTEGER(i4b)      :: iPar,iSta,iKin
  INTEGER(i4b)      :: recOK,satOK
  INTEGER(i4b)      :: icRef,iFrq,jRef,iRecSta,iRefSat
  INTEGER(i4b),SAVE :: iRef = 0

! Initialization
! --------------
  IF (tobs /= tobsSav) THEN
    tobsSav = tobs
    iRef = 0
    DO jRef=1,clkHed%ref(1)%nRef
      xRef(jRef)%locq(1:7) = 0
      xRef(jRef)%name    = ''
      xRef(jRef)%x0 = 0.D0
    ENDDO
  ENDIF
  recOK=0
  satOK=0

! Find parameter in locq
! ----------------------
  DO iPar=1,npar
    IF (INDA(iSng+(ipar-1)*MAXEQN)==0)EXIT
!!    IF (XXX0(INDA(iSng+(ipar-1)*MAXEQN))/=0.D0)CYCLE
    IF (LOCQ(1,INDA(iSng+(iPar-1)*MAXEQN)) < 21)CYCLE
    IF (LOCQ(1,INDA(iSng+(iPar-1)*MAXEQN)) == 21) THEN
      DO iKin=1,nKin
        IF (STKIN(iKin)==LOCQ(2,INDA(iSng+(ipar-1)*MAXEQN))) THEN
          DO iSta=1,ndiff+1
            IF (STNAME(STKIN(IKIN))==STANAM(iSta)) THEN
              XXX0(INDA(iSng+(ipar-1)*MAXEQN))=wgsapr(LOCQ(3,INDA(iSng+(ipar-1)*MAXEQN)),iSta)
            ENDIF
          ENDDO
        ENDIF
      ENDDO
    ENDIF
    IF (LOCQ(1,INDA(iSng+(ipar-1)*MAXEQN)) == 23) THEN
      XXX0(INDA(iSng+(ipar-1)*MAXEQN))=dtFil(1)
      recOK=1
    ENDIF
    IF (LOCQ(1,INDA(iSng+(ipar-1)*MAXEQN)) == 24) THEN
        IF (LOCQ(3,INDA(iSng+(ipar-1)*MAXEQN)) == saflep) THEN
          XXX0(INDA(iSng+(ipar-1)*MAXEQN))=satClk
          satOK=1
        ENDIF
    ENDIF

  ENDDO
  IF (nDiff==0.AND.iFrq/=4) THEN
    IF (recOK==0) THEN
      DO icRef=1,clkHed%ref(1)%nRef
        IF (clkHed%ref(1)%clk(icRef)%Name == staNam(1)) THEN
          iRecSta=0
! Station already in list?
          DO jRef=1,iRef
            IF(xRef(jRef)%name == staNam(1))iRecSta=1
          ENDDO
          IF (iRecSta==1) EXIT
          iRef=iRef+1
          IF (iRef > clkHed%ref(1)%nRef) THEN
            WRITE(lfnErr,'(/,A,I3,/,A,I3,/)')&
             ' *** SR aprval: Number of reference clocks too large: ',iRef,&
             '                Maximum number allowed:               ',clkHed%ref(1)%nRef
            CALL exitrc(2)
          ELSE
            xRef(iRef)%locq(:) = 0
            xRef(iRef)%locq(1) = 23
            xRef(iRef)%locq(2) = clkHed%ref(1)%clk(icRef)%Idx
            xRef(iRef)%locq(7) = icRef
            xRef(iRef)%name    = clkHed%ref(1)%clk(icRef)%Name
            xRef(iRef)%x0      = dtFil(1)
            EXIT
          ENDIF
        ENDIF
      ENDDO
    ENDIF

    IF (satOK==0) THEN
! Satellite already in list?
      DO jRef=1,iRef
        IF(xRef(jRef)%locq(3) == saflep)RETURN
      ENDDO

      CALL svn2chr(saflep,svnnum,svnchr)
      WRITE(satNam,'(A,I2.2)') svnchr,svnnum

      DO icRef=1,clkHed%ref(1)%nRef
        IF (clkHed%ref(1)%clk(icRef)%Name == satNam) THEN

! Satellite already in list?
          iRefSat = 0
          DO jRef=1,iRef
            IF(xRef(jRef)%locq(3) == saflep) iRefSat = 1
          ENDDO
          IF (iRefSat == 1) EXIT

          iRef=iRef+1
          IF (iRef > clkHed%ref(1)%nRef) THEN
            WRITE(lfnErr,'(/,A,I3,/,A,I3,/)')&
                 ' *** SR aprval: Number of reference clocks too large: ',iRef,&
                 '                Maximum number allowed:               ',clkHed%ref(1)%nRef
            CALL exitrc(2)
          ENDIF
          xRef(iRef)%locq(:) = 0
          xRef(iRef)%locq(1) = 24
          xRef(iRef)%locq(3) = saflep
          xRef(iRef)%locq(7) = icRef
          xRef(iRef)%name    = clkHed%ref(1)%clk(icRef)%Name
          xRef(iRef)%x0      = satclk
          EXIT
        ENDIF
      ENDDO
    ENDIF
  ENDIF

  END SUBROUTINE aprval

END MODULE
