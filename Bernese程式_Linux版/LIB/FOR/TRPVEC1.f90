MODULE s_TRPVEC1
CONTAINS

! --------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! --------------------------------------------------------------------------

SUBROUTINE trpvec1(iPara,model,epoch,neq,iTrGrd,grdInf)

! --------------------------------------------------------------------------
! Purpose:  Compute
!           (a) azimuth and tilting angle of the tropospheric delay vector
!               based on its north, east, and zenith component
!           (b) the formal accuracy of the tilting angle
!           (c) the error ellipse of the horizontal components
!           (d) the gradient parameters and their errors for the requested
!               time
!
! Remark:   This subroutine is the ADDNEQ2 counterpart of TRPVEC.f
!
! Author:     M. Meindl
!
! Created:    27-May-2003
! Last mod.:  04-Jan-2011
!
! Changes:    22-Sep-2005 RD: Use new module D_NEQ.f90
!             24-Aug-2006 AG: iTrGrd == 3 for tilting angle added
!             13-Feb-2008 RD: Adapt to par%name == chr*20
!             04-Jan-2011 PS: Chen/Herring gradient mapping added
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! --------------------------------------------------------------------------


! Modules
! -------
  USE m_bern
  USE d_neq,    ONLY: t_neq
  USE p_addneq, ONLY: comstat
  USE d_const,  ONLY: PI

  USE f_ikf
  IMPLICIT NONE


! List of parameters
! ------------------
! input
  INTEGER(i4b),DIMENSION(5)     :: iPara  ! Parameter index
                                          ! (/north1,east1,north2,east2,up/)
  REAL(r8b)                     :: model  ! Troposphere model value
  REAL(r8b)                     :: epoch  ! Requested epoch
  TYPE(t_neq)                   :: neq    ! Normal equation
  INTEGER(i4b),DIMENSION(2)     :: iTrGrd ! Gradient information

! output
  REAL(r8b),DIMENSION(12)       :: grdInf ! Computed gradient information


! Local Variables
! ---------------
! transformation
  REAL(r8b),DIMENSION(5)             :: xxxA
  REAL(r8b)                          :: dT
  REAL(r8b),DIMENSION(3,5)           :: xFt
  REAL(r8b),DIMENSION(5,5)           :: Qxy
  REAL(r8b),DIMENSION(3,3)           :: Qxx
  REAL(r8b)                          :: Q11, Q22, Q12
  REAL(r8b),DIMENSION(3)             :: xxxHlp
  REAL(r8b)                          :: xxx1, xxx2, xxx3

! computation of gradient info
  REAL(r8b)                          :: xTol
  REAL(r8b)                          :: xVal1, xVal2, xVal3
  REAL(r8b),DIMENSION(3)             :: pd
  REAL(r8b)                          :: Qsum1, Qsum2

! indices and loop variables
  INTEGER(i4b)                       :: ii, jj

! Compute gradient for requested epoch
! ------------------------------------
  xxxA = (/neq%xxx(iPara(1)),neq%xxx(iPara(2)),neq%xxx(iPara(3)),          &
                             neq%xxx(iPara(4)),neq%xxx(iPara(5))/)
  IF (iPara(3)/=iPara(1)) THEN
    dT   = (epoch-neq%par(iPara(1))%time%mean)/                            &
           (neq%par(iPara(3))%time%mean-neq%par(iPara(1))%time%mean)
  ELSE
    dT   = 0.d0
  END IF


! transformation function
  xFt(1,:) = (/1.d0-dT,0.d0,dT,0.d0,0.d0/)
  xFt(2,:) = (/0.d0,1.d0-dT,0.d0,dT,0.d0/)
  xFt(3,:) = (/0.d0,0.d0,0.d0,0.d0,1.d0/)

! part of aNor concerning gradients
  DO ii=1,5
    DO jj=1,5
      Qxy(ii,jj) = neq%aNor(ikf(iPara(ii),iPara(jj)))
    END DO
  END DO

! compute gradient vector, covariance matrix
  xxxHlp = MATMUL(xFt,xxxA)
  Qxx    = MATMUL(MATMUL(xFt,Qxy),TRANSPOSE(xFt))

  Q11    = Qxx(1,1)
  Q22    = Qxx(2,2)
  Q12    = Qxx(1,2)
  xxx1   = xxxHlp(1)
  xxx2   = xxxHlp(2)
  xxx3   = xxxHlp(3)+model


! Tilting angle, azimuth of tropospheric delay vector
! ---------------------------------------------------
  IF (iTrGrd(1)==1.OR.iTrGrd(1)==3.OR.iTrGrd(1)==4) THEN
    xTol = 1.d0
    IF (xxx3>xTol) THEN
      xVal1 = SQRT(xxx1**2+xxx2**2)
      xVal2 = xVal1/xxx3
    ELSE
      write(lfnerr,"(/,A,/17X,A,A,/,17X,A,F12.5,/,2(17X,A,F6.3,A,/))")     &
        " ### sr trpvec1: Tilting angle and its rms error not computed",   &
        "Requested station             : ",TRIM(neq%par(iPAra(5))%name),   &
        "Requested epoch               : ",epoch,                          &
        "Zenith delay                  : ",xxx3," M",                      &
        "Minimum zenith delay tolerated: ",xTol," M"
      xVal2 = 0.d0
    END IF

    grdInf(1) = 648000.d0/PI*xVal2
  END IF

  xVal1 = 0.d0
  IF (xxx1/=0.d0 .AND. xxx2/=0.d0) xVal1 = atan2(xxx2,xxx1)+PI
  grdInf(4) = 180.d0/PI*xVal1


! Formal accuracy of tilting angle
! --------------------------------
  IF (iTrGrd(1)==1.OR.iTrGrd(1)==3.OR.iTrGrd(1)==4) THEN
    xVal1 = sqrt(xxx1**2+xxx2**2)
    IF (xxx3>xTol .AND. xVal1>0.d0) THEN
      pd(1) = xxx1/xxx3/xVal1
      pd(2) = xxx2/xxx3/xVal1
      pd(3) = -xVal1/xxx3**2
    ELSE
      pd    = 0.d0
    END IF

    Qsum1 = 0.d0
    DO ii=1,3
      Qsum2 = 0.d0
      DO jj=1,3
        Qsum2 = Qsum2+pd(jj)*Qxx(jj,ii)
      END DO
      Qsum1 = Qsum1+Qsum2*pd(ii)
    END DO

    grdInf(2) = 648000.d0/PI*comstat%rms*sqrt(Qsum1)

    xVal1 = xxx1**2+xxx2**2
    IF (xVal1>0.d0) THEN
      pd(1) = -xxx2/xVal1
      pd(2) =  xxx1/xVal1
      pd(3) =  0.d0

      Qsum1 = 0.d0
      DO ii=1,3
        Qsum2 = 0.d0
        DO jj=1,3
          Qsum2 = Qsum2+pd(jj)*Qxx(jj,ii)
        END DO
        Qsum1 = Qsum1+Qsum2*pd(ii)
      END DO

      grdInf(5) = 180.d0/PI*comstat%rms*sqrt(Qsum1)
    ELSE
      grdInf(5) = 0.d0
    END IF
  END IF


! The error ellipse of the horizontal components
! ----------------------------------------------
  xVal1     = sqrt((Q11-Q22)**2+4.d0*Q12**2)
  grdInf(6) = comstat%rms*sqrt((Q11+Q22+xVal1)/2.d0)
  grdInf(7) = comstat%rms*sqrt((Q11+Q22-xVal1)/2.d0)

  xVal1 = 2.d0*Q12
  xVal2 = Q11-Q22
  xVal3 = 0.d0
  IF (xVal1/=0.d0 .AND. xVal2/=0) xVal3 = atan2(xVal1,xVal2)/2.d0

  grdInf(8) = 180.d0/PI*xVal3


! Test value
! ----------
  IF (iTrGrd(1)==1.OR.iTrGrd(1)==3.OR.iTrGrd(1)==4) THEN
    IF (grdInf(2)>0.d0) THEN
      grdInf(3) = grdInf(1)/grdInf(2)
    ELSE
      grdInf(3) = 0.d0
    END IF
  END IF


! North and east component of gradient and rms
! --------------------------------------------
  grdInf(9)  = xxx1
  grdInf(10) = comstat%rms*sqrt(Q11)
  grdInf(11) = xxx2
  grdInf(12) = comstat%rms*sqrt(Q22)


! Subroutine ends here
! --------------------
  RETURN
END SUBROUTINE trpvec1

END MODULE
