! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE s_NEQREAD

! -------------------------------------------------------------------------
! Purpose:    This subroutine reads the NEQ system from file. It computes
!             the necessary dimension according to required orbital
!             transformation. If the velocities of the stations are
!             estimated, the subroutine expands the NEQ system.
!
! Author:     L.Mervart
!
! Created:    22-Nov-1997
!
! Changes:    24-Oct-2001 RD: add file name to the rdstacrx call for output
!             03-Dec-2001 HU: Set up geocenter coordinates
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             29-Dec-2001 HU: Interfaces to  nqrdhead,neqalloc,rdstacrx added
!             25-Sep-2002 HU: Remove i_astlib
!             22-May-2003 RD: Use maxsat from m_maxdim (instead of p_addneq)
!             06-Feb-2004 HU: Truncate station names
!             13-Feb-2004 HU: Error concerning noabc corrected
!             10-Mar-2004 MM: Handle old p/w constant troposphere parameters
!             06-Jan-2005 HU: Update structure staProblem
!             24-Apr-2005 SS: Truncate 4-character station names
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             12-Dec-2005 CU: Initialize npseu, npseuel
!             06-Mar-2006 RD/MM: Update of staProblem after calling RDSTACRX
!             23-Jan-2008 RD: Read t_par using a function from the module
!             13-Feb-2008 RD: Adapt to par%name == chr*20
!             23-Jul-2008 RD: Use lfnres instead of lfnloc
!             06-May-2009 HU/RD: Correct NEQs affected by step2 bug
!             14-Aug-2009 SL: isStaCrx replaced by parelimi,neqclean
!             30-Oct-2009 SL: ipart added, PARELIMI and ISTOBEEL calls changed
!             03-Mar-2010 RD: Remove obsolete staProblem record
!             19-Aug-2010 MF: Dummy argument staInfo removed
!             04-Oct-2010 RD: Clean some implicit fortran type conversions
!             24-Oct-2010 RD: Distinguish between piece-wise linear param.
!             26-Oct-2010 SL: truncation bug corrected, use m_bern with ONLY,
!                             removal of unused modules
!             29-Nov-2010 DT: Add Helmert parameters
!             13-Dec-2011 SL: if condition for parelimi calls corrected
!             14-Jun-2012 RD: Handle also empty NEQs (npar == 0)
!             14-Jun-2012 RD: Remove unused variables
!             26-Jun-2012 RD: Check and report availability of parameters
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

INTERFACE neqread
  MODULE PROCEDURE neqread1, neqread2
END INTERFACE

CONTAINS

!----------------------------------------------------------------------------
! Original Routine - uses ADDNEQ2 options
!----------------------------------------------------------------------------
SUBROUTINE neqread1(neq,ifil,orb,ipart)

  USE m_bern,   ONLY: i4b, lfnerr
  USE d_neq,    ONLY: t_neq
  USE p_addneq, ONLY: t_orb,opt,comstat,maxDyn

  IMPLICIT NONE

  ! List of Parameters
  ! ------------------
  TYPE(t_neq)  :: neq
  INTEGER(i4b) :: ifil
  TYPE(t_orb)  :: orb
  INTEGER(i4b),OPTIONAL :: ipart

  ! Local variables
  ! ---------------
  INTEGER(i4b) :: ii, jj
  LOGICAL      :: printed

  CALL neqreadHlp(neq,ifil,orb,opt,comstat,maxDyn,ipart)

  ! Report pre-elimination requests without parameters
  IF ( PRESENT (iPart) ) THEN
    IF (iPart == 1) THEN
      IF ( ASSOCIATED(opt%elimi) .AND. ifil == SIZE(opt%neqFileName)) THEN
        DO ii=1,SIZE(opt%elimi)
          IF ( opt%elimi(ii)%part /= 999 ) CYCLE

          IF ( .NOT. opt%elimi(ii)%wParam ) THEN
            DO jj=1,SIZE(opt%elimi)
              IF ( opt%elimi(jj)%part /= 999 ) CYCLE
              IF ( opt%elimi(jj)%descr /= opt%elimi(ii)%descr ) CYCLE
              IF ( opt%elimi(jj)%wParam ) THEN
                opt%elimi(ii)%wParam = .TRUE.
                EXIT
              ENDIF
            ENDDO
          ENDIF

          IF ( opt%elimi(ii)%mode /= 999 .AND. .NOT. opt%elimi(ii)%wParam ) THEN
            printed = .FALSE.
            DO jj=1,ii-1
              IF ( opt%elimi(jj)%part /= 999 ) CYCLE
              IF ( opt%elimi(jj)%descr == opt%elimi(ii)%descr ) printed = .TRUE.
            ENDDO
            IF ( printed ) CYCLE
            WRITE(lfnerr,'(/,A,2(/,17X,A),/)') ' ### SR NEQREAD: ' //          &
              'Parameter of type "' // TRIM(opt%elimi(ii)%descr) // '" is not',&
              'in the input NEQs but it is indicated as "present"',            &
              'in "Parameter Pre-Elimination" panel.'
          ENDIF
        ENDDO
      ENDIF
    ENDIF
  ENDIF

END SUBROUTINE

!----------------------------------------------------------------------------
! Routine that can be used in any program
!----------------------------------------------------------------------------
SUBROUTINE neqread2(neqFileName, neq)
  USE m_bern,   ONLY: i4b, fileNameLength, staNameLength
  USE d_neq,    ONLY: t_neq
  USE p_addneq, ONLY: t_orb,opt,comstat,maxDyn
  USE s_neqinit
  USE s_cominit
  USE s_alcerr

  IMPLICIT NONE

  ! List of Parameters
  ! ------------------
  CHARACTER(LEN=fileNameLength), INTENT(IN) :: neqFileName
  TYPE(t_neq)                               :: neq

  ! Local Variables
  ! ---------------
  INTEGER(i4b), PARAMETER :: ifil = 1
  TYPE(t_orb)             :: orb
  INTEGER(i4b)            :: iac

  IF (.NOT. ASSOCIATED(opt%neqFileName)) THEN
    ALLOCATE (opt%neqFileName(1), STAT = iac)
    CALL alcerr(iac, 'opt%neqFileName',(/1/), 'neqread')
    ALLOCATE (opt%orbFil(4,1), STAT = iac)
    CALL alcerr(iac, 'opt%orbFil', (/4,1/), 'neqread')
    ALLOCATE(opt%req(0), STAT = iac)
    CALL alcerr(iac, 'opt%req', (/0/), 'neqread')
  ENDIF

  opt%neqFileName(1) = neqFileName
  opt%orbFil(1,1)    = ''
  opt%noabc          = staNameLength

  CALL neqinit(neq)
  CALL cominit(orb)

  CALL neqreadHlp(neq,ifil,orb,opt,comstat,maxDyn)
END SUBROUTINE

!----------------------------------------------------------------------------
! Auxiliary Routine
!----------------------------------------------------------------------------
SUBROUTINE neqreadHlp(neq,ifil,orb,opt,comstat,maxDyn,ipart)

  USE m_bern,   ONLY: i4b, r8b, lfnErr, lfnRes, staNameLength
  USE m_maxdim, ONLY: maxsat
  USE d_par,    ONLY: readpar, &
                      parType_linearLeftPoint,   &
                      parType_linearMiddlePoint, &
                      parType_linearRightPoint
  USE d_neq,    ONLY: t_neq, readRec
  USE p_addneq, ONLY: t_orb, t_opt, t_comstat, hlmFil

  USE s_opnfil
  USE s_nqaddgcc
  USE s_nqaddhlm
  USE s_nqrdhead
  USE s_opnerr
  USE s_rdstacrx
  USE s_neqalloc
  USE f_istobeel
  USE s_corrtid
  USE s_parelimi
  USE s_neqclean
  USE s_gtpartyp
  USE s_exitrc

  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_neq)        :: neq
  INTEGER(i4b)       :: ifil
  TYPE(t_orb)        :: orb
  TYPE(t_opt)        :: opt
  TYPE(t_comstat)    :: comstat
  INTEGER(i4b)       :: maxDyn
  INTEGER(i4b),OPTIONAL:: ipart

! Local Variables
! ---------------
  CHARACTER(LEN=staNameLength) :: name
  INTEGER(i4b)                 :: nDynSat
  INTEGER(i4b)                 :: nStcSat
  INTEGER(i4b)                 :: pardim
  INTEGER(i4b)                 :: ii, neqDim
  INTEGER(i4b)                 :: jj
  INTEGER(i4b)                 :: irCode, ios
  INTEGER(i4b)                 :: ireq
  INTEGER(i4b)                 :: npar_old
  INTEGER(i4b)                 :: nhelm, ihelm
  INTEGER(i4b),DIMENSION(3)    :: ipos
  REAL(r8b)                    :: tbeg
  REAL(r8b)                    :: tend
  REAL(r8b),DIMENSION(3)       :: xsta
  REAL(r8b),DIMENSION(3)       :: dtide
  LOGICAL                      :: trpWarn

  trpWarn = .TRUE.

  CALL opnfil(lfnres,opt%neqFileName(ifil),'OLD','UNFORMATTED', &
              'READONLY',' ',ios)
  CALL opnerr(lfnerr,lfnres,ios,opt%neqFileName(ifil),'NEQREAD')

  CALL nqrdhead(lfnres,neq)
  comstat%elimi    = 0
  neq%misc%npseu   = 0
  neq%misc%npseuel = 0

! Compute the NEQ system dimension necessary due to orbit transformation
! ----------------------------------------------------------------------
  IF (opt%orbFil(1,ifil) == '' .OR. ifil == 1) THEN
    pardim = neq%misc%npar
  ELSE
    IF (opt%numdyn < 0) THEN
      nDynSat = maxSat
    ELSE
      nDynSat = opt%numdyn
    END IF
    IF (opt%numstc < 0) THEN
      nStcSat = maxSat
    ELSE
      nStcSat = opt%numstc
    END IF
    pardim = neq%misc%npar + orb%nstoch + 3*nStcSat + maxDyn*nDynSat*(ifil-1)
  END IF

! Compute the NEQ system dimension necessary due to station velocities
! --------------------------------------------------------------------
  DO ireq = 1, SIZE(opt%req)
    IF (opt%req(ireq)%locq(1) == 1) THEN
      pardim = pardim + 3 * neq%misc%nstat_sinex
      EXIT
    END IF
  END DO

! Compute the NEQ system dimension necessary due to Helmert parameters
! --------------------------------------------------------------------
  nhelm = 0
  IF ( opt%covcomi /= '' .AND. ipart == 1 ) THEN
    DO ii = 1, 7
      IF ( hlmFil%ihelm(ii,ifil) == 2 )  nhelm = nhelm + 1
    END DO
    pardim = pardim + nhelm
  END IF

! Increase the NEQ system dimension if geocenter coordinates have to be set up
  IF (opt%setupgcc == 1) pardim = pardim + 3

! Allocate memory for NEQ structure
! ---------------------------------
  CALL neqalloc(neq,pardim)

  DO ii = 1, neq%misc%npar
    neq%par(ii) = readPar(lfnres,1,neq%version)

! Truncate station names after position noabc
! -------------------------------------------
    IF (opt%noabc < staNameLength .AND.                                 &
        (neq%par(ii)%locq(1) ==  1 .OR.                                 &
         neq%par(ii)%locq(1) ==  2 .OR.                                 &
         neq%par(ii)%locq(1) ==  6 .OR.                                 &
         neq%par(ii)%locq(1) ==  8 .AND. neq%par(ii)%locq(2) ==  2 .OR. &
         neq%par(ii)%locq(1) == 19) ) THEN
      neq%par(ii)%name(opt%noabc+1:)=' '
! Truncate 4-character station names (at request of swisstopo)
!!      IF (neq%par(ii)%name(7:) == ' ') neq%par(ii)%name(5:)=' '
    ENDIF


! Initialize the file boundaries
! ------------------------------
    tbeg = neq%par(ii)%time%mean - neq%par(ii)%time%half;
    tend = neq%par(ii)%time%mean + neq%par(ii)%time%half;
    IF (comstat%taecml(1,1,ifil) == 0.0d0   .OR. &
        comstat%taecml(1,1,ifil) > tbeg ) THEN
      comstat%taecml(1,1,ifil) = tbeg
    END IF

    IF (comstat%taecml(2,1,ifil) == 0.0d0   .OR. &
        comstat%taecml(2,1,ifil) < tend ) THEN
      comstat%taecml(2,1,ifil) = tend
    END IF

! Handle old ZPD representation (piecewise constant -> piecewise linear)
! ----------------------------------------------------------------------
    IF (neq%par(ii)%locq(1)==6) THEN
      IF (neq%par(ii)%time%half/=0.d0) THEN
        IF (trpWarn) THEN
          WRITE(lfnerr,'(/,A,/,17X,A,/,17X,A,I4,/)')                        &
          ' ### SR NEQREAD: Piecewise constant troposphere representation.',&
          'This may cause problems in case of parameter manipulation.',     &
          'Normal equation file: ',iFil
          trpWarn = .FALSE.
        ENDIF
        neq%par(ii)%time%half = 0.d0
      ENDIF
    ENDIF

! Redefine station numbers
! ------------------------
    IF ( neq%par(ii)%locq(1) == 1 ) THEN  ! Coordinates
      neq%par(ii)%locq(2) = ii
      IF ( neq%par(ii)%locq(4) == 0 ) neq%par(ii)%locq(4) = 1  ! not velocity
    END IF
    IF ( neq%par(ii)%locq(1) == 6 ) THEN  ! Troposphere
      neq%par(ii)%locq(3) = ii
    END IF

  END DO

!  DO ii = 1, neq%misc%npar
!    IF (neq%par(ii)%locq(1) /= 1) CYCLE
!    IF (neq%par(ii)%locq(4) == 1) THEN
!      neq%par(ii)%locq(4) = 0
!      DO jj = ii+1, neq%misc%npar
!        IF (neq%par(ii)%name == neq%par(jj)%name .AND. &
!            neq%par(ii)%locq(3) == neq%par(jj)%locq(3)) THEN
!          neq%par(ii)%locq(4) = 1
!          EXIT
!        ENDIF
!      ENDDO
!    ENDIF
!  ENDDO

! Correct step2 bug
! -----------------
  IF (opt%step2 == 1) THEN
    IF (comstat%taecml(1,1,ifil) >= opt%step2_t(1) .AND. &
        comstat%taecml(2,1,ifil) <= opt%step2_t(2)) THEN
      DO ii=1, neq%misc%npar
        IF (neq%par(ii)%locq(1) == 1 .AND. neq%par(ii)%locq(3) == 1 &
                                     .AND. neq%par(ii)%locq(4) == 1) THEN
           name = neq%par(ii)%name(1:staNameLength)
           ipos(1) = ii
           DO jj=1, neq%misc%npar
             IF (neq%par(jj)%locq(1) == 1 .AND. neq%par(jj)%locq(4) == 1 &
                                          .AND. neq%par(jj)%name == name) THEN
               IF (neq%par(jj)%locq(3) == 2) THEN
                 ipos(2) = jj
               ELSEIF (neq%par(jj)%locq(3) == 3) THEN
                 ipos(3) = jj
               ENDIF
             ENDIF
           ENDDO

           xsta(1:3) = neq%par(ipos(1:3))%x0
           CALL CORRTID(xsta,comstat%taecml(1,1,ifil), &
                             comstat%taecml(2,1,ifil),dtide)
           neq%par(ipos(1:3))%x0 = xsta(1:3)+dtide(1:3)
        ENDIF
      ENDDO
    ENDIF
  ENDIF
!
! change locq(5) for stochastic parameters
! ************************ dirty change gb (shame) ***********************
  do ii = 1,neq%misc%npar
    if(neq%par(ii)%locq(1) == 11)then
      neq%par(ii)%locq(5)=mod(neq%par(ii)%locq(5),10)
    endif
  enddo
! ************************ dirty change gb (shame) ***********************

  IF (neq%version <= 3) THEN
    DO ii = 1, neq%misc%npar*(neq%misc%npar+1)/2
      READ(lfnres) neq%aNor(ii)
    END DO

    DO ii = 1, neq%misc%npar
      READ(lfnres) neq%bNor(ii)
    END DO
  ELSE
    neqDim = neq%misc%npar*(neq%misc%npar+1)/2
    IF (neqDim > 0) CALL readRec(lfnres,neqDim,neq%aNor)

    neqDim = neq%misc%npar
    IF (neqDim > 0) CALL readRec(lfnres,neqDim,neq%bNor)

  ENDIF

  CLOSE(lfnres)

! Read the STACRUX file
! ---------------------
  CALL gtparTyp(neq)
  CALL rdstacrx(ifil,neq)
  IF(PRESENT(ipart)) THEN
    IF(ipart == 1) THEN
      CALL parelimi(neq,3,ifil)
    ELSE
      CALL parelimi(neq,3,0)
    ENDIF
  ELSE
    CALL parelimi(neq,3,0)
  ENDIF
  CALL neqclean(neq)

! Expand the NEQ system due to new station velocities
! ---------------------------------------------------
  Loop_ireq: DO ireq = 1, SIZE(opt%req)
    IF (opt%req(ireq)%locq(1) == 1) THEN

      DO ii = 1, neq%misc%npar
        IF ( neq%par(ii)%locq(1) == 1   .AND. &
             neq%par(ii)%locq(4) == 2 ) EXIT Loop_ireq
      END DO

      npar_old = neq%misc%npar

      DO ii = 1, npar_old
        IF ( neq%par(ii)%locq(1) == 1             .AND. &
             .NOT. isToBeEl(neq%par(ii),3,ifil) ) THEN
          neq%misc%npar                    = neq%misc%npar + 1
          neq%par(neq%misc%npar)           = neq%par(ii)
          neq%par(neq%misc%npar)%locq(4)   = 2     ! mark velocity
          neq%par(neq%misc%npar)%time%half = 0.d0
        END IF
      END DO

      neq%misc%nparms = neq%misc%nparms + DBLE(neq%misc%npar - npar_old)

      EXIT Loop_ireq
    END IF
  END DO Loop_ireq

! Set up geocenter coordinates
! ----------------------------
  IF (opt%setupgcc == 1) CALL nqaddgcc(neq,ifil)


! Set up Helmert parameters
! -------------------------
  IF ( nhelm > 0 ) THEN

    ihelm = 0
    pardim = size(neq%par)  ! Possible reallocation in NQADDVEL or NQADDMLC

    DO ii = 1, pardim
      IF ( neq%par(ii)%locq(1) == 0 .AND. ihelm < nhelm ) THEN
        neq%par(ii)%locq(1) = 28
        ihelm = ihelm + 1
      END IF

    END DO

    CALL nqaddhlm(neq,ifil,nhelm)

  END IF

! Check the parameter list and selected parameters
  irCode = 0
  IF ( ASSOCIATED(opt%elimi) ) THEN
    IF ( PRESENT (iPart) ) THEN
      IF (iPart == 1) THEN
        DO ii = 1,neq%misc%npar
          IF ( ISTOBEEL(neq%par(ii), 999, 0) ) irCode = irCode + 1
        ENDDO
      ENDIF
    ENDIF
  ENDIF
  IF (irCode /= 0) CALL exitrc(2)


END SUBROUTINE


END MODULE
