MODULE s_CRDELIM
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE crdelim(neq,iPar,ipart,ifil)

! -------------------------------------------------------------------------
! Purpose:    This subroutine applies N, E, and Up constraints on
!             coordinates and velocities (one complete set of coordinates
!             X, Y, Z, VX, VY, VZ is handled per call)
!
! Author:     M. Meindl
!
! Created:    05-Jun-2003
! Last mod.:  12-Dec-2005
!
! Changes:    10-Jun-2003 mm: Minor changes
!             29-Oct-2003 HB: Correct format statements
!             11-Feb-2005 SS: Bugfix wrt computation of Mxv and Cxx
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             12-Dec-2005 CU: Count number of pseudo-observations npseu
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------


  USE m_bern
  USE d_datum,  ONLY: datum
  USE d_neq,    ONLY: t_neq
  USE p_addneq, ONLY: opt

  USE f_ikf
  USE s_err3d
  USE f_gtweight
  USE f_istobeel
  USE s_xyzell
  IMPLICIT NONE


! List of parameters
! ------------------
  TYPE(t_neq)                        :: neq
  INTEGER(i4b)                       :: iPar
  INTEGER(i4b)                       :: ipart
  INTEGER(i4b)                       :: ifil


! Local variables
! ---------------
! misc
  INTEGER(i4b)                                 :: ik
  INTEGER(i4b),DIMENSION(6)                    :: indPar
  LOGICAL                                      :: isVelo

! loop variables
  INTEGER(i4b)                                 :: iPar1
  INTEGER(i4b)                                 :: ii, jj

! vectors, matrices
  REAL(r8b),DIMENSION(3,3)                     :: xNEU, vNEU
  REAL(r8b),DIMENSION(3,3)                     :: xXYZ, vXYZ
  REAL(r8b),DIMENSION(3)                       :: xStat, xStEll
  REAL(r8b),DIMENSION(6,6)                     :: Cxx, Cxv, Mxv

! time
  REAL(r8b)                                    :: deltaT

! transformations
  REAL(r8b)                                    :: radius, cosPhi



! Initialize some variables
! -------------------------
  isVelo = .FALSE.
  xNEU   = 0.d0
  vNEU   = 0.d0
  Cxv    = 0.d0
  Cxx    = 0.d0
  Mxv    = 0.d0


! Check for complete coordinate set
! ---------------------------------
  IF (neq%par(iPar+1)%name    /= neq%par(iPar)%name .OR.                   &
      neq%par(iPar+1)%locq(1) /= 1                  .OR.                   &
      neq%par(iPar+1)%locq(3) /= 2                  .OR.                   &
      neq%par(iPar+1)%locq(4) == 2                  .OR.                   &
      neq%par(iPar+2)%name    /= neq%par(iPar)%name .OR.                   &
      neq%par(iPar+2)%locq(1) /= 1                  .OR.                   &
      neq%par(iPar+2)%locq(3) /= 3                  .OR.                   &
      neq%par(iPar+2)%locq(4) == 2                      ) THEN

    write(lfnerr,'(/,A,/,16X,A,/,16X,A,A,/)')                              &
         ' ### SR CRDELIM: Incomplete station coordinate set.',            &
                          'No constraints applied.',                       &
                          'Station name: ' // TRIM(neq%par(iPar)%name)
    RETURN
  ENDIF


! Get N, E, and U coordinate
! --------------------------
  xStat(1) = neq%par(iPar)%x0
  xStat(2) = neq%par(iPar+1)%x0
  xStat(3) = neq%par(iPar+2)%x0

  CALL xyzell(datum%aEll,datum%bEll,datum%dxEll,datum%drEll,               &
              datum%scEll,xStat,xStEll)

  radius = SQRT(xStat(1)**2+xStat(2)**2+xStat(3)**2)
  cosPhi = COS(xStEll(1))
  IF (cosPhi==0.d0) cosPhi = 1


! Get weights for first coordinate set
! ------------------------------------
  xNEU(1,1) = gtweight(neq%par(ipar),'A')/radius**2
  xNEU(2,2) = gtweight(neq%par(ipar+1),'A')/(radius*cosPhi)**2
  xNEU(3,3) = gtweight(neq%par(ipar+2),'A')
  IF (xNEU(1,1) > 0d0) THEN
    neq%misc%npseu = neq%misc%npseu + 1
    IF (isToBeEl(neq%par(ipar),ipart,ifil)) &
      neq%misc%npseuel = neq%misc%npseuel + 1
  ENDIF
  IF (xNEU(2,2) > 0d0) THEN
    neq%misc%npseu = neq%misc%npseu + 1
    IF (isToBeEl(neq%par(ipar+1),ipart,ifil)) &
      neq%misc%npseuel = neq%misc%npseuel + 1
  ENDIF
  IF (xNEU(3,3) > 0d0) THEN
    neq%misc%npseu = neq%misc%npseu + 1
    IF (isToBeEl(neq%par(ipar+2),ipart,ifil)) &
      neq%misc%npseuel = neq%misc%npseuel + 1
  ENDIF


! Look for second coordinate set
! ------------------------------
  DO iPar1=iPar,neq%misc%nPar
    IF (neq%par(iPar1)%locq(1)==1               .AND.                      &
        neq%par(iPar1)%locq(3)==1               .AND.                      &
        neq%par(iPar1)%locq(4)==2               .AND.                      &
        neq%par(iPar1)%name==neq%par(iPar)%name      ) THEN


! Check for complete coordinate set
! ---------------------------------
      IF (neq%par(iPar1+1)%name    /= neq%par(iPar1)%name .OR.             &
          neq%par(iPar1+1)%locq(1) /= 1                   .OR.             &
          neq%par(iPar1+1)%locq(3) /= 2                   .OR.             &
          neq%par(iPar1+1)%locq(4) /= 2                   .OR.             &
          neq%par(iPar1+2)%name    /= neq%par(iPar1)%name .OR.             &
          neq%par(iPar1+2)%locq(1) /= 1                   .OR.             &
          neq%par(iPar1+2)%locq(3) /= 3                   .OR.             &
          neq%par(iPar1+2)%locq(4) /= 2                       ) THEN

        write(lfnerr,'(/,A,/,16X,A,/,16X,A,A,/)')                          &
         ' ### SR CRDELIM: Incomplete station coordinate set.',            &
                          'No constraints applied.',                       &
                          'Station name: ' // TRIM(neq%par(iPar1)%name)
        CYCLE
      END IF


! Get weights for velocity
! ------------------------
      vNEU(1,1) = gtweight(neq%par(ipar1),'A')/radius**2
      vNEU(2,2) = gtweight(neq%par(ipar1+1),'A')/(radius*cosPhi)**2
      vNEU(3,3) = gtweight(neq%par(ipar1+2),'A')

      IF (vNEU(1,1) > 0d0) THEN
        neq%misc%npseu = neq%misc%npseu + 1
        IF (isToBeEl(neq%par(ipar),ipart,ifil)) &
          neq%misc%npseuel = neq%misc%npseuel + 1
      ENDIF
      IF (vNEU(2,2) > 0d0) THEN
        neq%misc%npseu = neq%misc%npseu + 1
        IF (isToBeEl(neq%par(ipar+1),ipart,ifil)) &
          neq%misc%npseuel = neq%misc%npseuel + 1
      ENDIF
      IF (vNEU(3,3) > 0d0) THEN
        neq%misc%npseu = neq%misc%npseu + 1
        IF (isToBeEl(neq%par(ipar+2),ipart,ifil)) &
          neq%misc%npseuel = neq%misc%npseuel + 1
      ENDIF

      isVelo = .TRUE.
      EXIT
    END IF
  END DO


! Create transformation and constraint matrix
! -------------------------------------------
! constraint matrix
  CALL err3d(xStEll(1),xStEll(2),xStEll(3),datum%aEll,datum%bEll,          &
             1,xXYZ,xNEU)
  CALL err3d(xStEll(1),xStEll(2),xStEll(3),datum%aEll,datum%bEll,          &
             1,vXYZ,vNEU)

  Cxv(1:3,1:3) = xXYZ
  Cxv(4:6,4:6) = vXYZ

! transformation matrix
  IF (isVelo) THEN
    deltaT = neq%par(iPar1)%time%mean-neq%par(iPar)%time%mean
    DO ii=1,3
!!      Mxv(ii,ii)     = (opt%timRefCrd-neq%par(iPar)%time%mean)/deltaT
!!      Mxv(ii,ii+3)   = (neq%par(iPar1)%time%mean-opt%timRefCrd)/deltaT
      Mxv(ii,ii)     = 0.5d0
      Mxv(ii,ii+3)   = 0.5d0
      Mxv(ii+3,ii)   = -365.25d0/deltaT
      Mxv(ii+3,ii+3) =  365.25d0/deltaT
    END DO
  END IF


! Compute constraint matrix for two coordinate sets
! -------------------------------------------------
  IF (isVelo) THEN
    Cxx = matmul(matmul(transpose(Mxv),Cxv),Mxv)
  ELSE
    Cxx = Cxv
  END IF


! Add weights to normal equation
! ------------------------------
  indPar = (/iPar,iPar+1,iPar+2,iPar1,iPar1+1,iPar1+2/)
  DO ii=1,6
    DO jj=ii,6
      IF (Cxx(ii,jj)==0.d0) CYCLE
      ik = ikf(indPar(ii),indPar(jj))
      neq%aNor(ik) = neq%aNor(ik)+Cxx(ii,jj)
    END DO
  END DO


! The end
! -------
  RETURN
END SUBROUTINE crdelim


END MODULE
