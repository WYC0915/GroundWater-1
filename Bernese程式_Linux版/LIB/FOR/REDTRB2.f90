MODULE s_REDTRB2
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

      SUBROUTINE REDTRB2(NPAR,NLIN,NII,A0I,AII,IDEL,ANOR,BNOR,RMS)

! -------------------------------------------------------------------------
! Purpose:    efficient pre-elimnation of a normal equation system
!             of the form
!
!             | a00, a0i |   x1 |    b1 |
!             |          | *    | =     |  = anor * x = b
!             | .. , aii |   x2 |    b2 |
!
!             at time of sr-call anor is the complete neq matrix
!             (upper triangular part only consisting of a00,a0i, aii)
!             after execution only the part corresponding to a00
!             (other terms correctly pre-eliminated) are returned).
!             the matrix anor must not be ordered in the way assumed
!             the parameters corresponding to a00 are characterized b
!             idel(ipar)=0, those corresponding to a0i, aii with
!             idel(ipar)=1.
!             the sr assumes that aii is already inverted !
!
! Author:     G. Beutler, L. Mervart
!
! Created:    22-NOV-1997
!
! Changes:    26-JUN-2001 RD: Use alcerr for allocation
!             06-JAN-2005 HU: Efficiency increased using MATMUL
!             11-May-2005 RD: Get dimensions as parameters
!             23-Jun-2005 MM: IMPLICIT NONE and declarations added
!             20-Sep-2012 RD: Correctly deallocate arrays
!             20-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

! List of Parameters
! ------------------
!         IN  :  NPAR   : TOTAL DIMENSION OF MATRIX ANOR            I*4
!                NLIN   : DIMENSION OF A00                          I*4
!                NII    : DIMENSION OF AII                          I*4
!                A0I    : MATRIX A0I (SEE ABOVE)                    R*8
!                AII    : MATRIX AII (SEE ABOVE)                    R*8
!                IDEL   : PARAMETER CHARACTERIZATION                I*4
!      IN/OUT :  ANOR   : COMPLETE NEQ SYSTEM                       R*8
!                BNOR   : RHS OF NEQ SYSTEM                         R*8
!                RMS    : SUM OF SQUARES OF TERMS "OBS-COMP"        R*8


      USE m_bern, ONLY: i4b, r8b, lfnerr
!
      USE f_ikf
      USE s_alcerr
      USE s_exitrc
      IMPLICIT NONE
!
! DECLARATIONS INSTEAD OF IMPLICIT
! --------------------------------
      INTEGER(i4b) IAC , IK  , IP  , IPAR, KP  , KPAR, N0  , N1  ,&
                   NII , NLIN, NPAR
!
      REAL(r8b)    RMS
!
!!!       IMPLICIT  REAL*8 (A-H,O-Z)
!!!       IMPLICIT  INTEGER*4 (I-N)
!
      INTEGER(i4b) IDEL(:)
      REAL(r8b)    A0I(:,:),AII(:,:),ANOR(:),BNOR(:)
!
      INTEGER(i4b),DIMENSION(:),  ALLOCATABLE ::  INDPA0
      INTEGER(i4b),DIMENSION(:),  ALLOCATABLE ::  INDPA1
!
      REAL(r8b),   DIMENSION(:),  ALLOCATABLE ::  HELP
      REAL(r8b),   DIMENSION(:,:),ALLOCATABLE ::  HHELP
      REAL(r8b),   DIMENSION(:,:),ALLOCATABLE ::  ANORHLP
!
      ALLOCATE (INDPA0 (nlin), stat=iac )
      CALL alcerr(iac, 'indpa0', (/nlin/), 'redtrb2')
      ALLOCATE (INDPA1 (nii), stat=iac )
      CALL alcerr(iac, 'indpa1', (/nii/), 'redtrb2')
      ALLOCATE (HELP (nii), stat=iac )
      CALL alcerr(iac, 'help', (/nii/), 'redtrb2')
      ALLOCATE (HHELP (nlin,nii), stat=iac )
      CALL alcerr(iac, 'hhelp', (/nlin,nii/), 'redtrb2')
      ALLOCATE (ANORHLP (nlin,nlin), stat=iac )
      CALL alcerr(iac, 'anorhlp', (/nlin,nlin/), 'redtrb2')
!
! DEFINE NUMBER OF PARAMETERS IN PART 1 AND IN PART 2 OF THE NEQ-SYSTEM
! KEEP TRACK OF POSITION IN ORIGINAL NEQ SYSTEM
! ---------------------------------------------------------------------
      N0 = 0
      N1 = 0
      DO IPAR = 1, NPAR
        IF (IDEL (IPAR) .EQ.0) THEN
          N0 = N0 + 1
          IF (N0.GT.NLIN) THEN
            WRITE (LFNERR, 5) IDEL (IPAR), NLIN
    5 FORMAT      (//,' ** SR REDTRB : LOCAL DIMENSION MAXPA',I1,       &
     &                ' TOO SMALL (=',I5,')',//)
            CALL EXITRC (2)
          ENDIF
          INDPA0 (N0) = IPAR
        ELSE
          N1 = N1 + 1
          IF (N1.GT.NII) THEN
            WRITE (LFNERR, 5) IDEL (IPAR), NII
            CALL EXITRC (2)
          ENDIF
          INDPA1 (N1) = IPAR
        ENDIF
      END DO
!
! REDUCE RMS AND BNOR
! -------------------
!
! COMPUTE HELP=A11*B1, REDUCE RMS
      DO ip = 1, nii
        ipar = indpa1 (ip)
        help (ip) = 0.D0
        DO kp = 1, nii
          kpar = indpa1 (kp)
          help (ip) = help (ip) + AII (ip, kp) * bnor (kpar)
        END DO
        rms = rms - help (ip) * bnor (ipar)
      END DO
!
! REDUCE BNOR
      DO ip = 1, nlin
        ipar = indpa0 (ip)
        bnor (ipar) = bnor (ipar) - DOT_PRODUCT (A0I (ip,:), help)
      END DO
!
! REDUCE ANOR
! -----------
      IF (nlin > 0) THEN
        hhelp=MATMUL(a0i,aii)
        anorhlp=MATMUL(hhelp,TRANSPOSE(a0i))
        DO kp=1,nlin
          kpar = indpa0 (kp)
          DO ip=1,kp
            ipar = indpa0 (ip)
            ik = ikf (ipar, kpar)
            ANOR(ik)=ANOR(ik)-anorhlp(ip,kp)
          ENDDO
        ENDDO
      ENDIF
!
      DEALLOCATE (INDPA0, stat=iac)
      DEALLOCATE (INDPA1, stat=iac)
      DEALLOCATE (HELP, stat=iac)
      DEALLOCATE (HHELP, stat=iac)
      DEALLOCATE (ANORHLP, stat=iac)
!
      RETURN
      END SUBROUTINE REDTRB2

END MODULE
