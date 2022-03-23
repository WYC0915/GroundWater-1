MODULE s_DIFCOMP
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE difcomp(ifil, neq, neq_1, dx)

! -------------------------------------------------------------------------
! Purpose:    This subroutine computes the differences between the combined
!             solution and one individual solution
!
! Author:     L. Mervart
!
! Created:    27-Mar-1999
!
! Changes:    09-MAR-2000 LM: Helmert computed optionally only
!             09-MAR-2000 LM: Correction if locq(4) == 3
!             31-OCT-2000 RD: Neg. degree of freedom for Helmert Trans.
!             26-JUN-2001 RD: Use alcerr for allocation
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             27-Feb-2003 HU: DATUM from D_DATUM
!             15-Dec-2003 mm: Consider velocities (unless estimated)
!                             Output (HELMERT) changed
!             26-Jul-2005 ss: dx%rms
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             10-Mar-2009 SS: Corrected sign error for computation of
!                             residuals in case of inactive transformation
!             13-Mar-2009 SS: Store RMS errors and convert RMS and diff
!                             information from X/Y/Z into N/E/U
!             06-May-2009 RD: Repeatability for all parameters in a PLT-file
!             02-Mar-2010 PW: Redundant paranthesis removed
!             31-Aug-2010 LO: Text of correlation matrix in PLT-file changed
!             08-Nov-2010 RD: Select HELMERT parameter for repeatability
!             14-Dec-2010 RD: Do not report singular parameters in PLT-file
!             20-Jan-2012 RD: Make sure that "nStat" is really "MAXSTA"
!             19-Sep-2012 RD: Use M_BERN with ONLY
!             05-Oct-2012 RD: New call of NEQPRT
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnerr, lfnprt, lfnplt, &
                      fileNameLength, shortLineLength
  USE d_datum,  ONLY: datum
  USE d_neq,    ONLY: t_neq
  USE p_addneq, ONLY: t_dx,opt,comstat

  USE s_alcerr
  USE f_tstequiv
  USE f_ikf
  USE s_hlmtra
  USE s_neqprt
  USE s_opnfil
  USE s_opnerr
  USE s_gtflna
  USE s_xyzell
  USE s_err3d
  USE s_eccell
  IMPLICIT NONE

! List of Parameters
! ------------------
  INTEGER(i4b)                         :: ifil    ! index of neq-file
  TYPE(t_neq)                          :: neq
  TYPE(t_neq)                          :: neq_1
  TYPE(t_dx), DIMENSION(neq%misc%npar) :: dx

! Local Variables
! ---------------
  CHARACTER(LEN=fileNameLength)             :: pltnam
  CHARACTER(LEN=shortLineLength)            :: line
  INTEGER(i4b)                              :: ii,jj
  INTEGER(i4b)                              :: i1,i2
  INTEGER(i4b)                              :: iac
  INTEGER(i4b)                              :: ios
  INTEGER(i4b)                              :: ipar
  INTEGER(i4b)                              :: ipar2
  INTEGER(i4b)                              :: ip1
  INTEGER(i4b)                              :: nstat
  INTEGER(i4b)                              :: icrd
  INTEGER(i4b)                              :: ista
  INTEGER(i4b)                              :: jsta
  INTEGER(i4b)                              :: ksta
  INTEGER(i4b)                              :: iCmp
  INTEGER(i4b)                              :: iCov
  INTEGER(i4b), DIMENSION(:)  , ALLOCATABLE :: itypSta
  INTEGER(i4b), DIMENSION(:)  , ALLOCATABLE :: parIndex
  INTEGER(i4b), DIMENSION(:)  , ALLOCATABLE :: p1Index
  INTEGER(i4b)                              :: nipHelm       ! # of entries in ipHelm
  INTEGER(i4b)                              :: nParHelm
  INTEGER(i4b)                              :: nCrdHelm
  INTEGER(i4b), SAVE                        :: ircPlt
  INTEGER(i4b), SAVE                        :: numCalls = 0
  INTEGER(i4b)                              :: idx(neq_1%misc%npar)
  REAL(r8b)                                 :: delta
  REAL(r8b)                                 :: t1
  REAL(r8b)                                 :: t2
  REAL(r8b)                                 :: tt
  REAL(r8b)   , DIMENSION(:,:), ALLOCATABLE :: xyzGlobal
  REAL(r8b)   , DIMENSION(:,:), ALLOCATABLE :: xyzLocal
  REAL(r8b)   , DIMENSION(:,:), ALLOCATABLE :: crdResid
  REAL(r8b)   , DIMENSION(7)                :: paramHelm
  REAL(r8b)   , DIMENSION(7)                :: sigmaHelm
  REAL(r8b)                                 :: rmsHelm
  REAL(r8b)   , DIMENSION(3)                :: xEst, xEstEll, xHlp
  REAL(r8b)   , DIMENSION(3,3)              :: xCov, xCovEll
  REAL(r8b)                                 :: radius, cosPhi
  LOGICAL     , SAVE                        :: first = .TRUE.
  LOGICAL     , SAVE                        :: velFlg
  LOGICAL                                   :: singul

  numCalls = numCalls + 1

! Count the number of stations
! ----------------------------
  nstat = 0
  ista  = 0
  DO ip1 = 1, neq_1%misc%npar
    IF ( neq_1%par(ip1)%locq(1) == 1  .AND. &
         neq_1%par(ip1)%locq(3) == 1  .AND. &
         neq_1%par(ip1)%locq(4) <  2 ) THEN
      nstat = nstat + 1
    END IF
  END DO

  DO ipar = 1, neq%misc%npar
    IF ( neq%par(ipar)%locq(1) == 1  .AND. &
         neq%par(ipar)%locq(3) == 1  .AND. &
         neq%par(ipar)%locq(4) <  2 ) THEN
      ista = ista + 1
    END IF
  END DO
  IF (iSta > nstat) nstat=ista

  iSta = 0

  ALLOCATE(xyzGlobal(3,nstat), stat=iac)
  CALL alcerr(iac, 'xyzGlobal', (/3,nstat/), 'difcomp')
  ALLOCATE(xyzLocal(3,nstat), stat=iac)
  CALL alcerr(iac, 'xyzLocal', (/3,nstat/), 'difcomp')
  ALLOCATE(crdResid(3,nstat), stat=iac)
  CALL alcerr(iac, 'crdResid', (/3,nstat/), 'difcomp')
  ALLOCATE(itypSta(nstat), stat=iac)
  CALL alcerr(iac, 'itypSta', (/nstat/), 'difcomp')
  ALLOCATE(parIndex(nstat), stat=iac)
  CALL alcerr(iac, 'parIndex', (/nstat/), 'difcomp')
  ALLOCATE(p1Index(nstat), stat=iac)
  CALL alcerr(iac, 'p1Index', (/nstat/), 'difcomp')

! Have the velocities been estimated ?
! ------------------------------------
  IF (first) THEN
    first = .FALSE.
    velFlg = .FALSE.
    DO ipar = 1, neq%misc%npar
      IF (neq%par(ipar)%locq(1) == 1 .AND. neq%par(ipar)%locq(4) >= 2) THEN
        velFlg = .TRUE.
        EXIT
      END IF
    END DO
  END IF

! Compute the difference
! ----------------------
  Loop_ipar: DO ipar = 1, neq%misc%npar

    IF (neq%par(ipar)%locq(1) == 1 .AND. neq%par(ipar)%locq(4) >= 2) THEN
      dx(ipar)%flag = .TRUE.
      dx(ipar)%diff = 0.d0
      dx(ipar)%rms  = 0.d0
      CYCLE  Loop_ipar
    ELSE
      dx(ipar)%flag = .FALSE.
    END IF

    DO ip1 = 1, neq_1%misc%npar

      IF ( neq%par(ipar)%locq(1) == 1 .AND. velFlg ) THEN

        IF ( neq%par(ipar)%locq(4) == 1                       .AND. &
             neq%par(ipar)%locq(3) == neq_1%par(ip1)%locq(3)  .AND. &
             neq%par(ipar)%name    == neq_1%par(ip1)%name    ) THEN
          DO ipar2 = 1, neq%misc%npar
            IF ( neq%par(ipar2)%locq(1) == 1                       .AND. &
                 neq%par(ipar2)%locq(3) == neq_1%par(ip1)%locq(3)  .AND. &
                 neq%par(ipar2)%name    == neq_1%par(ip1)%name    ) THEN

              t1 = neq%par(ipar)%time%mean
              t2 = neq%par(ipar2)%time%mean
              tt = neq_1%par(ip1)%time%mean

              IF ( neq%par(ipar2)%locq(4) == 2 ) THEN
                dx(ipar)%flag = .TRUE.
                IF (t2-t1 /= 0.d0) THEN
                  dx(ipar)%diff = (neq_1%par(ip1)%x0 + neq_1%xxx(ip1)) - &
                                  (neq%par(ipar)%x0  + neq%xxx(ipar) ) * &
                                  (t2-tt)/(t2-t1)                      - &
                                  (neq%par(ipar2)%x0 + neq%xxx(ipar2)) * &
                                  (tt-t1)/(t2-t1)
                ELSE
                  dx(ipar)%diff = neq_1%xxx(ip1) - neq%xxx(ipar)
                END IF
                idx(ip1) = ipar
              ELSE IF ( neq%par(ipar2)%locq(4) == 3 ) THEN
                dx(ipar)%flag = .TRUE.
                dx(ipar)%diff = (neq_1%par(ip1)%x0 + neq_1%xxx(ip1)) - &
                                (neq%par(ipar)%x0  + neq%xxx(ipar) ) - &
                                (neq%par(ipar2)%x0 + neq%xxx(ipar2)) * &
                                (tt-t1) / 365.25d0
                idx(ip1) = ipar
              END IF

              dx(iPar)%t = neq_1%par(ip1)%time

! Store the coordinates for Helmert
! ---------------------------------
              IF ( dx(ipar)%flag ) THEN
                icrd = neq%par(ipar)%locq(3)
                IF (icrd == 1) THEN
                  ista = ista + 1
                  parIndex(ista) = ipar
                  p1Index(ista) = ip1
                END IF
                xyzLocal(icrd, ista)  = neq_1%par(ip1)%x0 + neq_1%xxx(ip1)
                xyzGlobal(icrd, ista) = xyzLocal(icrd, ista) - dx(ipar)%diff

                ! Check for singularity
                IF (neq%aNor(ikf(iPar,iPar))==0.d0 .AND.               &
                    neq%bNor(iPar)==0.d0 .AND. neq%xxx(iPar)==0.d0) THEN
                  xyzGlobal(icrd, ista) = 1d20
                ENDIF
                IF (neq_1%aNor(ikf(ip1,ip1))==0.d0 .AND.               &
                    neq_1%bNor(ip1)==0.d0 .AND. neq_1%xxx(ip1)==0.d0) THEN
                  xyzLocal(icrd, ista) = 1d20
                ENDIF
                CYCLE  Loop_ipar
              END IF
            END IF
          END DO
        END IF

      ELSE

        IF ( tstequiv(neq,ipar,neq_1,ip1) ) THEN
          dx(ipar)%flag = .TRUE.
          dx(ipar)%diff = neq_1%xxx(ip1) - neq%xxx(ipar)
          dx(iPar)%t    = neq_1%par(ip1)%time
          idx(ip1) = ipar

! Store the coordinates for Helmert
! ---------------------------------
          IF ( neq%par(ipar)%locq(1) == 1 ) THEN
            icrd = neq%par(ipar)%locq(3)
            IF (icrd == 1) THEN
              ista = ista + 1
              parIndex(ista) = ipar
              p1Index(ista) = ip1
            END IF
            xyzLocal(icrd, ista)  = neq_1%par(ip1)%x0 + neq_1%xxx(ip1)
            xyzGlobal(icrd, ista) = xyzLocal(icrd, ista) - dx(ipar)%diff

            ! Check for singularity
            IF (neq%aNor(ikf(iPar,iPar))==0.d0 .AND.               &
                neq%bNor(iPar)==0.d0 .AND. neq%xxx(iPar)==0.d0) THEN
              xyzGlobal(icrd, ista) = 1d20
            ENDIF
            IF (neq_1%aNor(ikf(ip1,ip1))==0.d0 .AND.               &
                neq_1%bNor(ip1)==0.d0 .AND. neq_1%xxx(ip1)==0.d0) THEN
              xyzLocal(icrd, ista) = 1d20
            ENDIF
          END IF

          CYCLE  Loop_ipar
        END IF

      END IF

    END DO

  END DO Loop_ipar

! Remove singular coordinates
! ---------------------------
  kSta = 0
  DO jSta = 1,iSta
    singul = .FALSE.
    DO icrd = 1,3
      singul = singul .OR. (xyzLocal(icrd, jsta) == 1d20)
      singul = singul .OR. (xyzGlobal(icrd, jsta) == 1d20)
    ENDDO
    IF (singul) THEN
      dx(parIndex(jsta))%flag = .FALSE.
      CYCLE
    ELSE
      kSta = kSta + 1
    ENDIF
    IF ( kSta /= jSta ) THEN
      xyzLocal(:, ksta)  =  xyzLocal(:, jsta)
      xyzGlobal(:, ksta) =  xyzGlobal(:, jsta)
      parIndex(ksta) = parIndex(jsta)
      p1Index(ksta)  = p1Index(jsta)
    ENDIF
  ENDDO
  iSta = kSta

  IF (opt%indvSol > 0) THEN

    itypSta(:) = 0
!
! Count the number of ipHelm-elements
! -----------------------------------
    nipHelm=0
    DO ii=1,7
      nipHelm=nipHelm+opt%ipHelm(ii)
    END DO
!
    IF (nipHelm > 0) then
      CALL hlmtra(ista, xyzLocal, xyzGlobal, itypSta, opt%ipHelm, 2,   &
                  datum%aell, datum%bell, datum%dxell, datum%drell,    &
                  datum%scell, crdResid, rmsHelm, nParHelm, nCrdHelm,  &
                  paramHelm, sigmaHelm)

!
! negative degree of freedom (not enough stations)
! ------------------------------------------------
      IF ( nCrdHelm < nipHelm ) THEN
        paramHelm(:)=0D0

        DO ii = 1, ista
            dx( parIndex(ii)   )%diff = 0D0
            dx( parIndex(ii)+1 )%diff = 0D0
            dx( parIndex(ii)+2 )%diff = 0D0
        END DO

      ELSE
!
! take the residuals from HLMTRA
! ------------------------------
        DO ii = 1, ista
            dx( parIndex(ii)   )%diff = -crdResid(1,ii)
            dx( parIndex(ii)+1 )%diff = -crdResid(2,ii)
            dx( parIndex(ii)+2 )%diff = -crdResid(3,ii)
        END DO
      END IF
!
! Write the transformation parameter
! ----------------------------------
      IF (numCalls==1) THEN
        WRITE(lfnprt,'(2(/,A),2/,A,2(/,2A))')                              &
    ' Helmert Transformation Parameters With Respect to Combined Solution:',&
    ' -------------------------------------------------------------------', &
    '                     Translation (m)             Rotation (")',        &
    ' Sol   Rms (m)      X       Y       Z         ',                       &
    'X       Y       Z     Scale (ppm)',                                    &
    ' --------------------------------------------',                        &
    '---------------------------------'
      END IF
      line = ''
      WRITE(line,'(I4,2X,F9.5,2(2X,3F8.4),2X,F9.5)')                   &
        numCalls,rmsHelm,(paramHelm(ii),ii=1,7)
      DO jj = 1,7
        IF (opt%ipHelm(jj) == 0) THEN
          i1 = 18 + (jj-1)*8 + ( (jj-1)/3 ) * 2
          i2 = i1 + 8 + ( jj/7 )
          IF (jj == 7) THEN
            WRITE(line(i1:i2+1),'(A)') '  ----   '
          ELSE
            WRITE(line(i1:i2),'(A)') '  ----  '
          ENDIF
        ENDIF
      ENDDO
      WRITE(lfnprt,'(A)') TRIM(line)
    END IF
  END IF

!
! Store RMS errors and convert RMS and diff information from X/Y/Z into N/E/U
! ---------------------------------------------------------------------------
  Loop_ista: DO ii = 1,ista
    ip1  = p1Index(ii)
    ipar = parIndex(ii)

    DO iCmp = 0,2
!!      xEst(iCmp+1) = neq%par(ipar+iCmp)%x0+neq%xxx(ipar+iCmp)
      xEst(iCmp+1) = neq_1%par(ip1+iCmp)%x0+neq_1%xxx(ip1+iCmp)
      DO iCov = 0,2
        xCov(iCmp+1,iCov+1) = neq_1%aNor(ikf(ip1+iCmp,ip1+iCov))
      ENDDO
    ENDDO

    CALL xyzell(datum%aEll,datum%bEll,datum%dxEll,datum%drEll, &
                datum%scEll,xEst,xEstEll)
    CALL err3d(xEstEll(1),xEstEll(2),xEstEll(3),datum%aEll, &
               datum%bEll,-1,comstat%rms**2*xCov,xCovEll)

    radius = SQRT(xEst(1)**2+xEst(2)**2+xEst(3)**2)
    cosPhi = COS(xEstEll(1))

! RMS errors (N/E/U):
    IF (xCovEll(1,1) >= 0.d0) THEN
      dx(ipar  )%rms(1) = radius*SQRT(xCovEll(1,1))
    ELSE
      dx(ipar  )%rms(1) = 0.d0
    ENDIF
    IF (xCovEll(2,2) >= 0.d0) THEN
      dx(ipar+1)%rms(1) = radius*cosPhi*SQRT(xCovEll(2,2))
    ELSE
      dx(ipar+1)%rms(1) = 0.d0
    ENDIF
    IF (xCovEll(3,3) >= 0.d0) THEN
      dx(ipar+2)%rms(1) = SQRT(xCovEll(3,3))
    ELSE
      dx(ipar+2)%rms(1) = 0.d0
    ENDIF

! Correlation coefficients (N-U/E-U/N-E):
    IF (xCovEll(1,1) > 0.d0 .AND. xCovEll(3,3) > 0.d0) THEN
      dx(ipar  )%rms(2) = xCovEll(1,3)/SQRT(xCovEll(1,1)*xCovEll(3,3))
    ELSE
      dx(ipar  )%rms(2) = 0.d0
    ENDIF
    IF (xCovEll(2,2) > 0.d0 .AND. xCovEll(3,3) > 0.d0) THEN
      dx(ipar+1)%rms(2) = xCovEll(2,3)/SQRT(xCovEll(2,2)*xCovEll(3,3))
    ELSE
      dx(ipar+1)%rms(2) = 0.d0
    ENDIF
    IF (xCovEll(1,1) > 0.d0 .AND. xCovEll(2,2) > 0.d0) THEN
      dx(ipar+2)%rms(2) = xCovEll(1,2)/SQRT(xCovEll(1,1)*xCovEll(2,2))
    ELSE
      dx(ipar+2)%rms(2) = 0.d0
    ENDIF

! Instructions on how to reconstruct the N/E/U variance-covariance matrix
! -----------------------------------------------------------------------
! [ covNN covNE covNU ]    1      [ rmsN**2 corNE*rmsN*rmsU corNU*rmsN*rmsU ]
! |       covEE covEU ] = ----- * [         rmsE**2         corEU*rmsE*rmsU ]
! [             covUU ]   m0**2   [                         rmsU**2         ]

! Differences (transformed from X/Y/Z into N/E/U):
    CALL eccell(xEstEll,dx(ipar:ipar+2)%diff,xHlp)
    dx(ipar:ipar+2)%diff = xHlp

! Normalized differences (N/E/U):
    DO iCmp = 0,2
      IF (dx(ipar+iCmp)%rms(1) > 0.d0) THEN
        dx(ipar+iCmp)%rms(3) = dx(ipar+iCmp)%diff/dx(ipar+iCmp)%rms(1)
      ELSE
        dx(ipar+iCmp)%rms(3) = 0.d0
      ENDIF
    ENDDO

  ENDDO Loop_ista


  DEALLOCATE(parIndex, stat=iac)
  DEALLOCATE(p1Index, stat=iac)
  DEALLOCATE(itypSta, stat=iac)
  DEALLOCATE(crdResid, stat=iac)
  DEALLOCATE(xyzLocal, stat=iac)
  DEALLOCATE(xyzGlobal, stat=iac)


  IF (iFil == 1) THEN
    ircPlt = 1
    CALL gtflna(0,'PLOT_ALL',pltnam,ircPlt)
    IF (ircPlt == 0) THEN
      CALL opnfil(lfnplt, pltnam, 'UNKNOWN', 'FORMATTED', ' ', ' ', ios)
      CALL opnerr(lfnerr, lfnplt, ios, pltnam, 'DIFCOMP')
    ENDIF
  ENDIF

! prepare the output for the "PLT(all)"-file
! ------------------------------------------
  IF (ircPlt == 0) THEN

    DO ip1 = 1,neq_1%misc%npar
      IF (neq_1%aNor(ikf(ip1,ip1))==0.d0 .AND.               &
          neq_1%bNor(ip1)==0.d0 .AND. neq_1%xxx(ip1)==0.d0) CYCLE

      ipar = idx(ip1)

      IF (neq%aNor(ikf(ipar,ipar))==0.d0 .AND.               &
          neq%bNor(ipar)==0.d0 .AND. neq%xxx(ipar)==0.d0) CYCLE

      delta = (neq_1%par(ip1)%x0 + neq_1%xxx(ip1)) - &
              (neq%par(ipar)%x0 + neq%xxx(ipar))
      neq_1%xxx(ip1)    = delta
      neq_1%par(ip1)%x0 = neq%par(ipar)%x0 + neq%xxx(ipar)
    ENDDO

    write(lfnplt,'(//,1X,80("="),/,1X,A,I6,5X,A,A,/,1X,80("="),/)') &
         'Results from NEQ',ifil,'Filename:  ',TRIM(opt%neqFileName(ifil))
    CALL neqprt(lfnplt,1,ifil,neq_1)

    IF (iFil == SIZE(opt%neqFileName)) close(lfnplt)
  ENDIF

END SUBROUTINE difcomp


END MODULE
