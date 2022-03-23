MODULE s_REDTRB
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

      SUBROUTINE REDTRB(NPAR,A0I,AII,IDEL,NLIN,ANOR,BNOR,RMS)

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
!             23-Jun-2005 MM: IMPLICIT NONE and declarations added
!             20-Sep-2012 RD: Use M_BERN with ONLY
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

! List of Parameters
! ------------------
!         IN  :  NPAR   : TOTAL DIMENSION OF MATRIX ANOR            I*4
!                A0I    : MATRIX A0I (SEE ABOVE)                    R*8
!                AII    : MATRIX AII (SEE ABOVE)                    R*8
!                IDEL   : PARAMETER CHARACTERIZATION                I*4
!                NLIN   : NUMBER OF LINES OF MATRIX A0I             I*4
!      IN/OUT :  ANOR   : COMPLETE NEQ SYSTEM                       R*8
!                BNOR   : RHS OF NEQ SYSTEM                         R*8
!                RMS    : SUM OF SQUARES OF TERMS "OBS-COMP"        R*8
!
      USE m_bern, ONLY: i4b, r8b, lfnerr
!
      USE f_ikf
      USE s_alcerr
      USE s_exitrc
      IMPLICIT NONE
!
! DECLARATIONS INSTEAD OF IMPLICIT
! --------------------------------
      INTEGER(i4b) IAC   , IK    , IP    , IPAR  , IPP   , KP    ,&
                   KPAR  , KPP   , LK    , LP    , LPP   , MAXPA0, MAXPA1,&
                   N0    , N1    , NLEFF , NLIN  , NONZER, NPAR
!
      REAL(r8b)    RMS
!
!!!       IMPLICIT  REAL*8 (A-H,O-Z)
!!!       IMPLICIT  INTEGER*4 (I-N)
!
      INTEGER(i4b) IDEL(*)
      REAL(r8b)    A0I(NLIN,*),AII(*),ANOR(*),BNOR(*)
!
      INTEGER(i4b),DIMENSION(:),  ALLOCATABLE ::  INDPA0
      INTEGER(i4b),DIMENSION(:),  ALLOCATABLE ::  INDPA1
      INTEGER(i4b),DIMENSION(:),  ALLOCATABLE ::  NCOL
      INTEGER(i4b),DIMENSION(:),  ALLOCATABLE ::  LINNUM
      INTEGER(i4b),DIMENSION(:,:),ALLOCATABLE ::  INDCOL
!
      REAL(r8b),   DIMENSION(:),  ALLOCATABLE ::  HELP
!
      MAXPA0 = nlin
      MAXPA1 = npar - nlin
!
      ALLOCATE (INDPA0 (MAXPA0), stat=iac )
      CALL alcerr(iac, 'indpa0', (/maxpa0/), 'redtrb')
      ALLOCATE (INDPA1 (MAXPA1), stat=iac )
      CALL alcerr(iac, 'indpa1', (/maxpa1/), 'redtrb')
      ALLOCATE (NCOL (MAXPA0), stat=iac )
      CALL alcerr(iac, 'ncol', (/maxpa0/), 'redtrb')
      ALLOCATE (LINNUM (MAXPA0), stat=iac )
      CALL alcerr(iac, 'linnum', (/maxpa0/), 'redtrb')
      ALLOCATE (INDCOL (MAXPA0, MAXPA1), stat=iac )
      CALL alcerr(iac, 'indcol', (/maxpa0, maxpa1/), 'redtrb')
      ALLOCATE (HELP (MAXPA1), stat=iac )
      CALL alcerr(iac, 'help', (/maxpa1/), 'redtrb')
!
! DEFINE NUMBER OF PARAMETERS IN PART 1 AND IN PART 2 OF THE NEQ-SYSTEM
! KEEP TRACK OF POSITION IN ORIGINAL NEQ SYSTEM
! ---------------------------------------------------------------------
      N0 = 0
      N1 = 0
      DO IPAR = 1, NPAR
        IF (IDEL (IPAR) .EQ.0) THEN
          N0 = N0 + 1
          IF (N0.GT.MAXPA0) THEN
            WRITE (LFNERR, 5) IDEL (IPAR), MAXPA0
    5 FORMAT      (//,' ** SR REDTRB : LOCAL DIMENSION MAXPA',I1,       &
     &                ' TOO SMALL (=',I5,')',//)
            CALL EXITRC (2)
          ENDIF
          INDPA0 (N0) = IPAR
        ELSE
          N1 = N1 + 1
          IF (N1.GT.MAXPA1) THEN
            WRITE (LFNERR, 5) IDEL (IPAR), MAXPA1
            CALL EXITRC (2)
          ENDIF
          INDPA1 (N1) = IPAR
        ENDIF
      END DO
!
! ANALYSE MATRIX A0I :
! ------------------
! NLEFF       : NUMBER OF NON-EMPTY LINES OF MATRIX A0I
! LINNUM(I)   : LINE NUMBER OF I-TH NON EMPTY LINE OF MATRIX A0I
! NCOL(I)     : NUMBER OF NON-ZERO ELEMENTS IN LINE NCOL(I) OF MATRIX A0
! INDCOL(I,K) : COLUMN NUMBER OF K-TH NON-ZERO ELEMENT IN LINE NCOL(I)
!               OF MATRIX A
!
      NLEFF = 0
      DO IPAR = 1, N0
        NONZER = 0
        DO KPAR = 1, N1
          IF (A0I (IPAR, KPAR) .NE.0.D0) THEN
            NONZER = NONZER + 1
            IF (NONZER.EQ.1) THEN
              NLEFF = NLEFF + 1
              LINNUM (NLEFF) = IPAR
            ENDIF
            NCOL (NLEFF) = NONZER
            INDCOL (NLEFF, NONZER) = KPAR
          ENDIF
        END DO
      END DO
!
! REDUCE RMS AND BNOR
! -------------------
!
! COMPUTE HELP=A11*B1, REDUCE RMS
      DO IP = 1, N1
        IPAR = INDPA1 (IP)
        HELP (IP) = 0.D0
        DO KP = 1, N1
          IK = IKF (IP, KP)
          KPAR = INDPA1 (KP)
          HELP (IP) = HELP (IP) + AII (IK) * BNOR (KPAR)
        END DO
        RMS = RMS - HELP (IP) * BNOR (IPAR)
      END DO
!
! REDUCE BNOR
      DO IPP = 1, NLEFF
        IP = LINNUM (IPP)
        IPAR = INDPA0 (IP)
        DO KPP = 1, NCOL (IPP)
          KP = INDCOL (IPP, KPP)
          KPAR = INDPA1 (KP)
          BNOR (IPAR) = BNOR (IPAR) - A0I (IP, KP) * HELP (KP)
        END DO
      END DO
!
! REDUCE ANOR
! -----------
      DO IPP = 1, NLEFF
        IP = LINNUM (IPP)
        IPAR = INDPA0 (IP)
        DO KP = 1, N1
          HELP (KP) = 0.D0
          DO LPP = 1, NCOL (IPP)
            LP = INDCOL (IPP, LPP)
            LK = IKF (LP, KP)
            HELP (KP) = HELP (KP) + A0I (IP, LP) * AII (LK)
          END DO
        END DO
!
        DO KPP = 1, IPP
          KP = LINNUM (KPP)
          KPAR = INDPA0 (KP)
          IK = IKF (IPAR, KPAR)
          DO LPP = 1, NCOL (KPP)
            LP = INDCOL (KPP, LPP)
            ANOR (IK) = ANOR (IK) - HELP (LP) * A0I (KP, LP)
          END DO
        END DO
      END DO
!
      DEALLOCATE (HELP, stat=iac)
      DEALLOCATE (INDCOL, stat=iac)
      DEALLOCATE (LINNUM, stat=iac)
      DEALLOCATE (NCOL, stat=iac)
      DEALLOCATE (INDPA1, stat=iac)
      DEALLOCATE (INDPA0, stat=iac)
!
      RETURN
      END SUBROUTINE REDTRB

END MODULE
