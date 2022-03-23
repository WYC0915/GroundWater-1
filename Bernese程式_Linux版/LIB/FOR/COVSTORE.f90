MODULE s_COVSTORE
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE covstore(neq)

! -------------------------------------------------------------------------
! Purpose:    Save covariance results
!
! Author:     U. Hugentobler
!
! Created:    15-Jan-2007
! Last mod:   23-Nov-2011
!
! Changes:    13-Sep-2004 mm: A posteriori rms in COV-file
!             03-Feb-2005 mm: Warning for velocities
!                             Correct station name array
!             22-Sep-2005 RD: Use new modules D_NEQ.f90 and D_PAR.f90
!             15-Jan-2007 mm: Transformation from CRD/CRD to CRD/VEL
!             04-May-2009 RD: Scaling of loading models added
!             21-Nov-2009 RD: Intersystem bias added
!             12-Aug-2010 DT: Range biases added
!             06-Dec-2010 DT: Helmert parameters added
!             23-Nov-2011 RD: Copy aNor with the correct size
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE m_time,   ONLY: t_timint
  USE d_par,    ONLY: maxlcq
  USE d_neq,    ONLY: t_neq
  USE p_addneq, ONLY: opt,comstat
  USE d_const,  ONLY: filtitle
  USE d_datum,  ONLY: datum

  USE s_alcerr
  USE s_covsav
  USE s_covsv1
  USE f_ikf
  USE s_xyzell
  USE s_eccell
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_neq) :: neq

! Local Variables
! ---------------
  CHARACTER(LEN=8),   PARAMETER                   :: srName = 'covstore'

  REAL(r8b),   DIMENSION(:,:), ALLOCATABLE        :: timisb
  INTEGER(i4b)                                    :: iPar, iSta
  INTEGER(i4b)                                    :: staPos, nSta
  INTEGER(i4b)                                    :: nIsb
  INTEGER(i4b)                                    :: iac
  INTEGER(i4b),DIMENSION(:,:), ALLOCATABLE        :: locq
  CHARACTER(LEN=16)                               :: staNam
  CHARACTER(LEN=16),DIMENSION(:), ALLOCATABLE     :: staLst
  CHARACTER(LEN=fileNameLength)                   :: filnam = ' '
  REAL(r8b),DIMENSION(:),ALLOCATABLE              :: aNor
  REAL(r8b),DIMENSION(2,2)                        :: C_mat,N22
  REAL(r8b),DIMENSION(2,neq%misc%nPar-2)          :: N12
  REAL(r8b)                                       :: dT, dTm
  INTEGER(i4b)                                    :: iPar1, iPar2, ii, ip1
  LOGICAL                                         :: estVel = .FALSE.


! Return, if no output file specified
! -----------------------------------
  IF ( opt%covarrs == '' .AND. opt%covttrs == '') RETURN


! Allocate memory
! ---------------
  ALLOCATE(aNor(neq%misc%nPar*(neq%misc%nPar+1)/2),STAT=iac)
  CALL alcerr(iac, 'aNor', (/(neq%misc%nPar*(neq%misc%nPar+1)/2)/), srName)
  ALLOCATE( locq(maxlcq,opt%maxpar),   STAT=iac )
  CALL alcerr(iac, 'locq', (/maxlcq,opt%maxpar/), srName)
  ALLOCATE( staLst(opt%maxpar),   STAT=iac )
  CALL alcerr(iac, 'staLst', (/opt%maxpar/), srName)


! Copy aNor and locq
! ------------------
  aNor = neq%aNor(1:neq%misc%nPar*(neq%misc%nPar+1)/2)
  DO iPar=1,neq%misc%nPar
    locq(:,iPar) = neq%par(iPar)%locq(:)
  ENDDO


! Velocities estimated?
! ---------------------
  DO iPar=1,neq%misc%nPar
    IF (neq%par(iPar)%locq(1)==1 .AND. neq%par(iPar)%locq(4)==2) THEN
      estVel = .TRUE.
      EXIT
    ENDIF
  ENDDO


! Transform aNor from CRD/CRD to CRD/VEL
! --------------------------------------
  IF (estVel .AND. opt%covMode == 1) THEN

! Find corresponding coordinate parameters
    LOOP_ipar1: DO iPar1=1,neq%misc%nPar

      IF (neq%par(iPar1)%locq(1) /= 1 .OR. &
          neq%par(iPar1)%locq(4) /= 1      ) CYCLE

      DO iPar2=iPar1+1,neq%misc%nPar
        IF (neq%par(iPar2)%locq(1) /= 1                      .OR. &
            neq%par(iPar2)%locq(4) /= 2                      .OR. &
            neq%par(iPar2)%locq(3) /= neq%par(iPar1)%locq(3) .OR. &
            neq%par(iPar2)%name    /= neq%par(iPar1)%name          ) CYCLE

! Time intervals and transformation matrix
        dT  = (neq%par(iPar2)%time%mean-neq%par(iPar1)%time%mean)/365.25d0
        dTm = (opt%timRefCrd-neq%par(iPar1)%time%mean)/365.25d0

        C_mat(1,1) = 1-dTm/dT
        C_mat(1,2) =   dTm/dT
        C_mat(2,1) =    -1/dT
        C_mat(2,2) =     1/dT

! Relevant var-covar parts
        N22(1,1) = aNor(ikf(iPar1,iPar1))
        N22(1,2) = aNor(ikf(iPar1,iPar2))
        N22(2,1) = aNor(ikf(iPar2,iPar1))
        N22(2,2) = aNor(ikf(iPar2,iPar2))

        ip1 = 0
        DO ii=1,neq%misc%nPar
          IF (ii==iPar1 .OR. ii==iPar2) CYCLE
          ip1 = ip1+1
          N12(1,ip1) = aNor(ikf(ii,ipar1))
          N12(2,ip1) = aNor(ikf(ii,ipar2))
        END DO

! Compute transformation
        N22 = MATMUL(C_mat,N22)
        N22 = MATMUL(N22,TRANSPOSE(C_mat))
        N12 = MATMUL(C_mat,N12)

! Back-substitution
        aNor(ikf(iPar1,iPar1)) = N22(1,1)
        aNor(ikf(iPar1,iPar2)) = N22(1,2)
        aNor(ikf(iPar2,iPar1)) = N22(2,1)
        aNor(ikf(iPar2,iPar2)) = N22(2,2)

        ip1 = 0
        DO ii=1,neq%misc%nPar
          IF (ii==iPar1 .OR. ii==iPar2) CYCLE
          ip1 = ip1+1
          aNor(ikf(ii,iPar1)) = N12(1,ip1)
          aNor(ikf(ii,iPar2)) = N12(2,ip1)
        END DO

        locq(1,ipar2) = 20
        CYCLE LOOP_ipar1
      ENDDO

    ENDDO LOOP_ipar1
  ENDIF


! Copy arrays
! -----------
  nSta = 0
  DO ipar=1,neq%misc%nPar
    staNam       = neq%par(iPar)%name

! Find station in list
    staPos = 0
    DO iSta=1,nSta
      IF (staNam == staLst(iSta)) THEN
        staPos = iSta
        EXIT
      END IF
    END DO

! Update list
    IF (staPos == 0) THEN
      nSta   = nSta+1
      staPos = nSta
      staLst(staPos) = staNam
    END IF

! Modify locq for "old" COVSAV/COVSV1 subroutine
    SELECT CASE (locq(1,iPar))
      CASE (1)
        locq(2,iPar) = staPos                           ! CRD
      CASE (2)
        locq(2,iPar) = staPos                           ! RCV CLK
      CASE (6)
        locq(3,iPar) = staPos                           ! TRP
      CASE (8)
        IF (locq(2,iPar) /= 1) locq(3,iPar) = staPos    ! DCB (sta)
      CASE (20)
        locq(2,iPar) = staPos                           ! VEL
      CASE (21)
        locq(2,iPar) = staPos                           ! KIN CRD
      CASE (22)
        locq(7,iPar) = staPos                           ! Scale grid
      CASE (23)
        locq(2,iPar) = staPos                           ! EPO CLK
      CASE (26)
        locq(2,iPar) = staPos                           ! SLR RGB
      CASE (28)
        locq(4,iPar) = staPos                           ! HELMERT
      CASE (30)
        locq(2,iPar) = staPos                           ! GNSS-SPECIFIC
    END SELECT

  ENDDO

! Inter-system bias
! -----------------
  nIsb = 0
  DO iPar = 1,neq%misc%nPar
    IF (neq%par(iPar)%locq(1) == 2 .AND. neq%par(iPar)%locq(6) == 5) THEN
      nIsb = nIsb + 1
    ENDIF
  ENDDO

  ALLOCATE(timisb(3,nIsb),stat=iac)
  CALL alcerr(iac,'timisb',(/3,nIsb/),srName)

  nIsb = 0
  DO iPar = 1,neq%misc%nPar
    IF (neq%par(iPar)%locq(1) == 2 .AND. neq%par(iPar)%locq(6) == 5) THEN
      nIsb = nIsb + 1
      timisb(1,nIsb) = neq%par(iPar)%time%mean
      locq(4,iPar) = nIsb
    ENDIF
  ENDDO


! Save coordinate covariances
! ---------------------------
  CALL covsav(filnam,filTitle,comstat%rms,neq%misc%nobs,neq%misc%nparms, &
              neq%misc%npar,aNor,locq, staLst)


! Save complete covariances
! -------------------------
  CALL covsv1(filTitle,comstat%rms,neq%misc%nobs,neq%misc%nparms, &
              neq%misc%npar,aNor,locq, staLst, timisb)


! Deallocate
! ----------
  DEALLOCATE(locq,staLst,aNor,timisb,STAT=iac)

END SUBROUTINE covstore


END MODULE
