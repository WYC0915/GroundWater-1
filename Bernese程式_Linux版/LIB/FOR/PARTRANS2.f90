MODULE s_PARTRANS2
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE partrans2(neq,req)

  ! -------------------------------------------------------------------------
  ! Purpose:    The subroutine perform the parameter transformation
  !             into a periodic function according to request req
  !
  ! Author:     L. Mervart
  !
  ! Created:    17-Apr-2009
  !
  ! Changes:    20-Sep-2012 RD: Use M_BERN with ONLY
  !             20-Sep-2012 RD: Remove unused variables and modules
  !
  ! Copyright:  Astronomical Institute
  !             University of Bern
  !             Switzerland
  ! -------------------------------------------------------------------------

  ! Modules
  ! -------
  USE m_bern,   ONLY: i4b, r8b
  USE m_time,   ONLY: OPERATOR(.isIn.)
  USE d_par,    ONLY: maxLcq, parType_periodicOffset, parType_periodicDrift
  USE d_par,    ONLY: parType_periodicCos, parType_periodicSin
  USE d_neq,    ONLY: t_neq
  USE p_addneq, ONLY: t_req

  USE f_ikf
  USE s_alcerr
  IMPLICIT NONE

  ! List of Parameters
  ! ------------------
  TYPE(t_neq),  INTENT(INOUT) :: neq
  TYPE(t_req),  INTENT(IN)    :: req  ! Transformation request

  ! Local Variables
  ! ---------------
  REAL(r8b),DIMENSION(:,:),ALLOCATABLE  :: C_Mat
  REAL(r8b),DIMENSION(:,:),ALLOCATABLE  :: N12
  REAL(r8b),DIMENSION(:,:),ALLOCATABLE  :: N22
  REAL(r8b),DIMENSION(:)  ,ALLOCATABLE  :: bNor
  REAL(r8b)                             :: dt

  INTEGER(i4b),DIMENSION(neq%misc%npar) :: crsp_2
  INTEGER(i4b),DIMENSION(neq%misc%npar) :: crsp_1
  INTEGER(i4b)                          :: ipar2
  INTEGER(i4b)                          :: ip1,ip2,np1,np2
  INTEGER(i4b)                          :: ilcq
  INTEGER(i4b)                          :: iac
  LOGICAL                               :: ok

  ! Make sure that it is a correct request
  ! --------------------------------------
  IF ( (req%type /= parType_periodicOffset .AND. &
        req%type /= parType_periodicDrift  .AND. &
        req%type /= parType_periodicCos    .AND. &
        req%type /= parType_periodicSin   ) .OR. &
       req%omega == 0.0                   ) THEN
    RETURN
  ENDIF

  ! Init some variables
  ! -------------------
  np1    = 0
  np2    = 0
  crsp_2 = 0
  crsp_1 = 0

  ! Find the corresponding parameters
  ! ---------------------------------
  DO ipar2 = 1, neq%misc%npar

    ok = .TRUE.
    DO ilcq = 1, maxLcq
      IF ( neq%par(ipar2)%locq(ilcq)  /= req%locq(ilcq) .AND. &
           req%locq(ilcq)             /= 0 ) ok = .FALSE.
    END DO

    IF( ok                                                            .AND. &
       (LEN_TRIM(req%name)==0 .OR. neq%par(ipar2)%name == req%name  ) .AND. &
       (neq%par(ipar2)%time%mean .isIn. req%timint))  THEN

      np2 = np2 + 1
      crsp_2(np2) = ipar2

    ELSE
      np1 = np1 + 1
      crsp_1(np1) = ipar2
    END IF
  END DO

  IF (np2 < 4) RETURN

  ! Allocate the memory
  ! -------------------
  ALLOCATE( C_Mat(np2,4), stat=iac )
  CALL alcerr(iac, 'C_Mat', (/np2,4/), 'partrans2')
  ALLOCATE( N22(np2,np2), stat=iac )
  CALL alcerr(iac, 'N22', (/np2,np2/), 'partrans2')
  IF (np1 >= 1) THEN
    ALLOCATE( N12(np1,np2), stat=iac )
    CALL alcerr(iac, 'N12', (/np1,np2/), 'partrans2')
  ENDIF
  ALLOCATE( bNor(np2), stat=iac )
  CALL alcerr(iac, 'bNor', (/np2/), 'partrans2')

  ! Create the necessary matrices
  ! -----------------------------
  DO ip2 = 1, np2
    ipar2 = crsp_2(ip2)

    dt = neq%par(ipar2)%time%mean - req%timint%t(1)

    C_Mat(ip2,1) = 1.0
    C_Mat(ip2,2) = dt
    C_Mat(ip2,3) = COS(req%omega * dt)
    C_Mat(ip2,4) = SIN(req%omega * dt)

    bNor(ip2) = neq%bNor(ipar2)

    neq%par(ipar2)%locq(1) = 0

    DO ip1 = 1, np1
      N12(ip1,ip2) = neq%aNor(ikf(crsp_1(ip1),ipar2))
    END DO
    DO ip1 = 1, np2
      N22(ip1,ip2) = neq%aNor(ikf(crsp_2(ip1),ipar2))
    END DO
  END DO

  ! Compute the transformation
  ! --------------------------
  N22(1:4,1:4) = MATMUL( TRANSPOSE(C_Mat), MATMUL(N22,C_Mat) )
  IF (np1 >=1 ) N12(1:np1,1:4) = MATMUL( N12, C_Mat)
  bNor(1:4)    = MATMUL( TRANSPOSE(C_Mat), bNor)

  ! Backward substitution
  ! ---------------------
  DO ip2 = 1,4
    ipar2 = crsp_2(ip2)
    neq%bNor(ipar2) = bNor(ip2)

    DO ilcq = 1, maxlcq
      IF (req%locq(ilcq) /= 0 ) neq%par(ipar2)%locq(ilcq) = req%locq(ilcq)
    END DO

    neq%par(ipar2)%time%mean  = (req%timint%t(2) + req%timint%t(1)) / 2.0
    neq%par(ipar2)%time%half  = (req%timint%t(2) - req%timint%t(1)) / 2.0
    neq%par(ipar2)%omega      = req%omega
    IF     (ip2 == 1) THEN
      neq%par(ipar2)%type = parType_periodicOffset
    ELSEIF (ip2 == 2) THEN
      neq%par(ipar2)%type = parType_periodicDrift
      neq%par(ipar2)%x0   = 0.0
    ELSEIF (ip2 == 3) THEN
      neq%par(ipar2)%type = parType_periodicCos
      neq%par(ipar2)%x0   = 0.0
    ELSE
      neq%par(ipar2)%type = parType_periodicSin
      neq%par(ipar2)%x0   = 0.0
    ENDIF

    DO ip1 = 1,np1
      neq%aNor(ikf(crsp_1(ip1),ipar2)) = N12(ip1,ip2)
    END DO
    DO ip1 = 1,2
      neq%aNor(ikf(crsp_2(ip1),ipar2)) = N22(ip1,ip2)
    END DO
  END DO

  ! Free memory
  ! -----------
  DEALLOCATE( bNor, stat=iac )
  IF (np1 >= 1) DEALLOCATE( N12, stat=iac )
  DEALLOCATE( N22, stat=iac )
  DEALLOCATE( C_Mat, stat=iac )

END SUBROUTINE partrans2

END MODULE
