MODULE s_PARTRANS
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE partrans(neq,req,part,ifil)

! -------------------------------------------------------------------------
! Purpose:    The subroutine perform the parameter transformation
!             according to request req.
!
! Author:     L. Mervart
!
! Created:    22-Nov-1997
!
! Changes:    26-Jun-2001 RD: Use alcerr for allolcation
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             06-Nov-2002 RD: Check boundaries of the requests
!             26-Nov-2002 MM: No warning for pos/vel
!             05-Feb-2003 RD: Check of the requests moved to NEQTRANS
!             22-Sep-2005 RD: Use new modules D_NEQ.f90 and D_PAR.f90
!             27-Apr-2009 LM/SL: parTypes and partrans2 (Transformation
!                                into periodic function) added
!             21-Nov-2009 RD: Inter-system biases added
!             24-Oct-2010 RD: Remove type flag from transformed param.
!             09-Sep-2011 HU/PS: set a priori value
!             03-Jul-2012 RD: Consider leap-second
!             02-Aug-2012 DT: Update statistics (nparms)
!             20-Sep-2012 RD: Use M_BERN with ONLY
!             20-Sep-2012 RD: Remove unused parameters from PARTRANS2
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, r8b, lfnerr
  USE m_time,   ONLY: OPERATOR(.ISIN.)
  USE d_par,    ONLY: maxLcq, parType_periodic, parType_linear
  USE d_neq,    ONLY: t_neq
  USE p_addneq, ONLY: t_req

  USE s_partrans2
  USE f_ikf
  USE s_alcerr
  USE f_istobeel
  USE f_gtaprpol
  USE f_ut1_ut1r
  IMPLICIT NONE

! List of Parameters
! ------------------
! input/output:
  TYPE(t_neq)                           :: neq

! input:
  TYPE(t_req),INTENT(IN)                :: req  ! Transformation request
  INTEGER(i4b)                          :: part
  INTEGER(i4b)                          :: ifil

! Local Variables
! ---------------
  REAL(r8b),DIMENSION(:,:),ALLOCATABLE  :: C_Mat
  REAL(r8b),DIMENSION(:,:),ALLOCATABLE  :: N12
  REAL(r8b),DIMENSION(:,:),ALLOCATABLE  :: N22
  REAL(r8b),DIMENSION(:)  ,ALLOCATABLE  :: bNor
  REAL(r8b)                             :: polHelp
  REAL(r8b)                             :: t1,t2,ti

  INTEGER(i4b),DIMENSION(neq%misc%npar) :: crsp_2
  INTEGER(i4b),DIMENSION(neq%misc%npar) :: crsp_1
  INTEGER(i4b)                          :: ipar2
  INTEGER(i4b)                          :: ip1,ip2,np1,np2
  INTEGER(i4b)                          :: ilcq
  INTEGER(i4b)                          :: iac
  LOGICAL                               :: ok

! Transformation into periodic function
! -------------------------------------
  IF ( req%type(1:1) == parType_periodic ) THEN
    CALL partrans2(neq,req)
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

  IF (np2 < 2) RETURN

! Allocate the memory
! -------------------
  ALLOCATE( C_Mat(np2,2), stat=iac )
  CALL alcerr(iac, 'C_Mat', (/np2,2/), 'partrans')
  ALLOCATE( N22(np2,np2), stat=iac )
  CALL alcerr(iac, 'N22', (/np2,np2/), 'partrans')
  IF (np1 >= 1) THEN
    ALLOCATE( N12(np1,np2), stat=iac )
    CALL alcerr(iac, 'N12', (/np1,np2/), 'partrans')
  ENDIF
  ALLOCATE( bNor(np2), stat=iac )
  CALL alcerr(iac, 'bNor', (/np2/), 'partrans')

! Create the necessary matrices
! -----------------------------
  DO ip2 = 1, np2
    ipar2 = crsp_2(ip2)

    t1 = req%timint%t(1)
    ti = neq%par(ipar2)%time%mean
    t2 = req%timint%t(2)

    IF ( neq%par(ipar2)%locq(1) == 10  .AND. &  ! Drift
         neq%par(ipar2)%locq(5) == 2 ) THEN
      C_Mat(ip2,1) =   -1.d0 /(t2-t1)
      C_Mat(ip2,2) =    1.d0 /(t2-t1)
    ELSE
      C_Mat(ip2,1) = (t2-ti)/(t2-t1)
      C_Mat(ip2,2) = (ti-t1)/(t2-t1)
    END IF

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
  N22(1:2  ,1:2) = MATMUL( TRANSPOSE(C_Mat), MATMUL(N22,C_Mat) )
  IF (np1 >=1 ) N12(1:np1,1:2) = MATMUL( N12, C_Mat)
  bNor(1:2)      = MATMUL( TRANSPOSE(C_Mat), bNor)

! Backward substitution
! ---------------------
  DO ip2 = 1,2
    ipar2 = crsp_2(ip2)
    neq%bNor(ipar2) = bNor(ip2)

    DO ilcq = 1, maxlcq
      IF (req%locq(ilcq) /= 0 ) neq%par(ipar2)%locq(ilcq) = req%locq(ilcq)
    END DO

    neq%par(ipar2)%time%mean = req%timint%t(ip2)
    neq%par(ipar2)%time%half = 0.d0

    IF ( neq%par(ipar2)%locq(1) == 10 ) THEN
      neq%par(ipar2)%locq(6) = 1
      IF ( neq%par(ipar2)%locq(5) == 2 ) THEN    ! Drift
        neq%par(ipar2)%locq(5) = 1
        neq%par(ipar2)%locq(3) = neq%par(ipar2)%locq(3) + 1
        neq%par(ipar2)%x0      = neq%par(crsp_2(1))%x0
      END IF

      IF ( neq%par(ipar2)%locq(4) == 3 ) THEN    ! Leap second
        polHelp = gtaprpol(neq%par(ipar2),0) - ut1_ut1r(neq%par(ipar2))
        IF ( polHelp - neq%par(ipar2)%x0  > 500d0 ) THEN
          neq%par(ipar2)%x0 = neq%par(ipar2)%x0 + 1000d0
        ELSE IF ( polHelp - neq%par(ipar2)%x0  < -500d0 ) THEN
          neq%par(ipar2)%x0 = neq%par(ipar2)%x0 - 1000d0
        END IF
      END IF
    END IF

    IF ( neq%par(ipar2)%locq(1) == 2 .AND. &
         neq%par(ipar2)%locq(6) == 5 ) THEN
        neq%par(ipar2)%locq(5) = NINT(DABS(req%timint%t(2)- &
                                 req%timint%t(1))*86400d0)
    END IF

    IF ( neq%par(ipar2)%locq(1) == 1          .AND. &
         isToBeEl(neq%par(ipar2),part,ifil) ) THEN
      WRITE(lfnerr,'(" ### SR PARTRANS: Coordinates of station ",a16  ,/, &
                   & 18X, "have been transformed into the time ",f16.5,/, &
                   & 18X, "and will be pre-eliminated")')                 &
            neq%par(ipar2)%name, neq%par(ipar2)%time%mean
    END IF

    DO ip1 = 1,np1
      neq%aNor(ikf(crsp_1(ip1),ipar2)) = N12(ip1,ip2)
    END DO
    DO ip1 = 1,2
      neq%aNor(ikf(crsp_2(ip1),ipar2)) = N22(ip1,ip2)
    END DO
  END DO

! Adapt the parameter type description
! ------------------------------------
  ip1 = crsp_2(1)
  ip2 = crsp_2(2)

  neq%par(ip1)%type = parType_linear
  neq%par(ip2)%type = parType_linear

  neq%par(ip1)%omega = req%timint%t(2) - req%timint%t(1)
  neq%par(ip2)%omega = req%timint%t(2) - req%timint%t(1)

! Set a priori value (mandatory for leap second)
  neq%par(crsp_2(2))%x0=neq%par(crsp_2(np2))%x0

! Update statistics
! -----------------
  neq%misc%nparms = neq%misc%nparms - np2 + 2

! Free memory
! -----------
  DEALLOCATE( bNor, stat=iac )
  IF (np1 >= 1) DEALLOCATE( N12, stat=iac )
  DEALLOCATE( N22, stat=iac )
  DEALLOCATE( C_Mat, stat=iac )

END SUBROUTINE partrans

END MODULE
