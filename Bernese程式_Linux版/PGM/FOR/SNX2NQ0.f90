
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

PROGRAM snx2nq0

! -------------------------------------------------------------------------
! Purpose:    Conversion SINEX --> normal equation file
!
! Author:     L.Mervart
!
! Created:    08-Oct-1998
!
! Changes:    08-Sep-2000 HB: Use fileNameLength from m_bern
!             08-Oct-2001 LM: Switch to new menu
!             01-Nov-2001 HU: Interface for READSIN, SINTRAN3, NEQSTORE
!             03-Nov-2001 HU: Parameter optrot added
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             28-Aug-2002 DT: Parameter flg_neq added
!             29-Aug-2002 DT: Set of parameter invert for call of READSIN
!                             depending on input option flg_neq
!             25-Sep-2002 HU: Remove i_astlib
!             26-SEP-2002 DT: Call of SR prfile changed: #Col=3
!                             #characters in output = 105
!             26-SEP-2002 DT: Call of SR readsin with new argument xstat
!             26-SEP-2002 DT: Write a station coordinate file; new variable
!                             stanum, stname, staflg, datum, title, nstafil;
!                             Call SR wtcoor
!             27-SEP-2002 DT: Call SR readkeys to get the TITLE for the
!                             station coordinate file
!             27-SEP-2002 DT: Parameter optcrd added to check if CRD file
!                             is desired (SR s2n0in)
!             22-NOV-2002 DT: Write a station velocity file (check together
!                             with CRD file): SR wtvelo
!             22-NOV-2002 DT: new parameters xvel, plate
!             22-NOV-2002 DT: Call of SR prfile changed: #Col=4
!                             #characters in output = 140
!             23-Apr-2003 HU: Nullify local pointers
!             11-Oct-2003 HU: Use DATUMSTR as keyword instead of DATUM
!             30-Oct-2003 HU: filTitle not used from d_const
!             19-Nov-2003 RD: Read INP-filename in READINPF
!             26-Jan-2004 HU: Initialize datum, nutmod, submod
!             10-Mar-2004 CU: Reconstruct original NEQ info - on request
!             21-Jun-2005 MM: LFNUM.inc removed (LFNUMs in m_bern)
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             17-Mar-2006 AG: Use subroutine S_NEQSORT.f90
!             17-Jul-2006 HU: Epoch for CRD file corrected
!             31-Oct-2006 AG: unused variables removed
!             27-Feb-2007 AG: Call DEFCON with parameter
!             20-Aug-2007 AG: Noneq implemented
!             12-Mar-2009 SL: sintim in sintran3 call
!             06-May-2009 HU/RD: Correct SNX files affected by step2 bug
!             23-Sep-2010 RD: Enable CPU counter
!             06-Oct-2010 RD: Exitrc added at the end
!             15-Oct-2010 DT: Selecting set of coord for CRD/VEL (snxSta);
!                             Calls to S2N0IN, READSIN changed
!             20-Oct-2010 DT: Write FIX file (Call to s2n0in changed);
!                             invert is set in READSIN (removed from Call)
!             17-Feb-2011 RD: Use WTSTAT instead of WTCOOR and WTVELO
!             02-Dec-2011 SL: new title string for pritit, m_bern with ONLY
!             05-Oct-2012 DT: Write only generated files in output list
!             22-Oct-2012 DT: Initialize nutmod, submod with latest models
!             24-Oct-2012 SS: Consider flgCon (for creation of FIX file)
!             29-Oct-2012 SS: Generalized optfix
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,    ONLY: i4b, r8b, fileNameLength, keyValueLength, &
                       staNameLength, lfnPrt, lfnErr, lfn002
  USE m_cpu,     ONLY: cpu_start
  USE m_time,    ONLY: t_timint, OPERATOR(.isIn.)
  USE d_inpkey,  ONLY: inpKey, init_inpkey
  USE d_neq,     ONLY: t_neq
  USE p_addneq,  ONLY: opt
  USE p_snx2nq0, ONLY: t_snxSta
  USE s_readsin
  USE s_s2n0in
  USE s_alcerr
  USE s_prflna
  USE s_pritit
  USE s_readinpf
  USE s_prfile
  USE s_sintran3
  USE s_readkeys
  USE s_defcon
  USE s_wtstat
  USE s_opnsys
  USE s_neqsort
  USE s_neqstore
  USE s_corrtid
  USE s_exitrc
  USE s_timst2
  USE s_opnfil
  USE s_opnerr
  IMPLICIT NONE

! Local variables
! ---------------
  TYPE (t_neq)                             :: neq
  TYPE (t_timint)                          :: sintim
  TYPE(t_snxSta)                           :: snxSta

  INTEGER(i4b)                             :: iac, irc

  INTEGER(i4b)                             :: nfiles
  INTEGER(i4b)                             :: ipar1
  INTEGER(i4b)                             :: ipar2
  INTEGER(i4b)                             :: ifil
  INTEGER(i4b)                             :: optrot
  INTEGER(i4b)                             :: flg_neq
  INTEGER(i4b)                             :: nstafil
  INTEGER(i4b)                             :: optcrd
  INTEGER(i4b)                             :: optfix
  INTEGER(i4b)                             :: noneq
  INTEGER(i4b)                             :: step2
  INTEGER(i4b)                             :: ii, jj
  INTEGER(i4b), DIMENSION(3)               :: ipos
  INTEGER(i4b)                             :: berneq
  INTEGER(i4b)                             :: crdset

  REAL(r8b)                                :: dummy
  REAL(r8b), DIMENSION(:,:), ALLOCATABLE   :: xstat
  REAL(r8b), DIMENSION(:,:), ALLOCATABLE   :: xvel
  REAL(r8b)                                :: sigma0
  REAL(r8b)                                :: timCrd
  REAL(r8b)                                :: Tcrdset
  REAL(r8b)                                :: dt
  REAL(r8b), DIMENSION(2)                  :: step2t
  REAL(r8b), DIMENSION(3)                  :: xsta
  REAL(r8b), DIMENSION(3)                  :: dtide

  LOGICAL                                  :: velFil=.FALSE.

  CHARACTER(LEN=fileNameLength), DIMENSION(:,:), POINTER :: files
  CHARACTER(LEN=80)                                      :: title
  CHARACTER(LEN=16)                                      :: datum
  CHARACTER(LEN=16), DIMENSION(:), ALLOCATABLE           :: stname
  CHARACTER(LEN=1), DIMENSION(:), ALLOCATABLE            :: staflg
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER   :: keyValue
  CHARACTER(LEN=1)                                       :: help
  CHARACTER(LEN=staNameLength)                           :: name
  CHARACTER(LEN=20)                                      :: timStr

! Start CPU Counter
! -----------------
  CALL cpu_start(.TRUE.)

! Nullify pointers
! ----------------
  NULLIFY(files)
  NULLIFY(keyValue)
  CALL init_inpkey(inpKey)

! Get the name of the input file
! ------------------------------
  CALL readinpf(' ',inpKey)

! Open system files, define constants
! -----------------------------------
  CALL opnsys
  CALL defcon(1)

! Write title and file names
! --------------------------
  CALL pritit('SNX2NQ0','Transfer SINEX files to normal equation files')
  CALL prflna

! Read input options (list of input and output files)
! ---------------------------------------------------
  CALL s2n0in(nfiles, files, optrot, flg_neq, optcrd, optfix, berneq, &
              sigma0, noneq, step2, step2t, crdset, Tcrdset)

  IF ( optcrd == 1 .AND. crdset == 1 ) THEN
    WRITE(lfnprt, '(1X,A,/)') &
          'CRD/VEL file saved with latest coordinate set'

  ELSEIF ( optcrd == 1 .AND. crdset == 2 ) THEN
    CALL timst2(1, 1, Tcrdset, timStr)

    WRITE(lfnprt, '(1X,A,1X,A20,/)') &
          'CRD/VEL file saved with coordinate set valid at epoch: ', &
          timStr
  ENDIF


  IF ( noneq == 1 ) THEN
    files(2,:) = ''
  END IF
  IF ( optcrd == 0 ) THEN
    files(3,:) = ''
    files(4,:) = ''
  END IF
  IF ( optfix == 0 ) THEN
    files(5,:) = ''
  END IF

  CALL prfile('INPFIL','INPUT AND OUTPUT FILES',5,170,nfiles,files)


! Loop over all files
! -------------------
  DO ifil = 1, nfiles

    CALL readsin(files(1,ifil), neq, optrot, dummy, flg_neq, &
                 berneq, sigma0, sintim, timCrd, noneq,      &
                 crdset, Tcrdset, snxSta)

! Correct step2 bug
! -----------------
    IF (step2 == 1) THEN
      IF (sintim%t(1) >= step2t(1) .AND. sintim%t(2) <= step2t(2)) THEN
        DO ii=1, neq%misc%npar
          IF (neq%par(ii)%locq(1) == 1 .AND. neq%par(ii)%locq(3) == 1 &
                                       .AND. neq%par(ii)%locq(4) == 0) THEN
            name = neq%par(ii)%name
            ipos(1) = ii
            DO jj=1, neq%misc%npar
              IF (neq%par(jj)%locq(1) == 1 .AND. neq%par(jj)%locq(4) == 0 &
                                           .AND. neq%par(jj)%name == name) THEN
                IF (neq%par(jj)%locq(3) == 2) THEN
                  ipos(2) = jj
                ELSEIF (neq%par(jj)%locq(3) == 3) THEN
                  ipos(3) = jj
                ENDIF
              ENDIF
            ENDDO
            xsta(1:3) = neq%par(ipos(1:3))%x0
            CALL CORRTID(xsta,sintim%t(1),sintim%t(2),dtide)
            neq%par(ipos(1:3))%x0 = xsta(1:3)+dtide(1:3)
          ENDIF
        ENDDO
      ENDIF
    ENDIF

! Write a station coordinate and velocity file
! --------------------------------------------
    IF ( optcrd == 1) THEN

      nstafil = snxSta%nStaInt

      ALLOCATE(xstat(3,nstafil), stat=iac)
      CALL alcerr(iac, 'xstat', (/3,nstafil/), 'snx2nq0')
      xstat = 0d0
      ALLOCATE(xvel(3,nstafil), stat=iac)
      CALL alcerr(iac, 'xvel', (/3,nstafil/), 'snx2nq0')
      xvel = 0d0

      ALLOCATE(stname(nstafil), stat=iac)
      CALL alcerr(iac, 'stname', (/nstafil/), 'snx2nq0')
      ALLOCATE(staflg(nstafil), stat=iac)
      CALL alcerr(iac, 'staflg', (/nstafil/), 'snx2nq0')
      staflg(:) = ' '

      CALL readkeys('TITLE',keyValue,irc)
      IF (irc == 0) THEN
        title = keyValue(1)
      ENDIF

      CALL readkeys('DATUMSTR',keyValue,irc)
      IF (irc == 0) THEN
        datum = keyValue(1)
      ENDIF

      CALL readkeys('FLAG',keyValue,irc)
      IF (irc == 0) THEN
        help = keyValue(1)
      ENDIF

      dt = (Tcrdset - timCrd) / 365.25d0

      DO ipar1 = 1, snxSta%nStaInt
        stname(ipar1) = snxSta%staInt(ipar1)%staNam(1:16)
        staflg(ipar1) = help

        DO ipar2 = 1, 3

          xvel(ipar2,ipar1)  = snxSta%staInt(ipar1)%vel(ipar2)

          IF ( .NOT.velFil                     .AND. &
               snxSta%staInt(ipar1)%vel(ipar2) /= 0d0 ) &
             velFil = .TRUE.

         ! Extrapolate coord. to requested epoch
         ! -------------------------------------
          IF ( crdset == 2 .AND. velFil ) THEN
            xstat(ipar2,ipar1) = snxSta%staInt(ipar1)%crd(ipar2) +    &
                                 snxSta%staInt(ipar1)%vel(ipar2) * dt

         ! Use epoch from SINEX
         ! --------------------
          ELSE
            xstat(ipar2,ipar1) = snxSta%staInt(ipar1)%crd(ipar2)

          ENDIF

        ENDDO
      ENDDO

      IF ( crdset == 2 .AND. velFil ) timCrd = Tcrdset

      CALL wtstat(0,files(3,ifil),title,datum,nstafil,stname,xstat, &
                  staflg=staflg,timcrd=timCrd)

      IF ( velFil ) THEN
        CALL wtstat(0,files(4,ifil),title,datum,nstafil,stname,xvel, &
                    staflg=staflg)
      ENDIF

      DEALLOCATE(staflg, stat=iac)
      DEALLOCATE(stname, stat=iac)
      DEALLOCATE(xvel,   stat=iac)
      DEALLOCATE(xstat,  stat=iac)

    ENDIF

! Write FIX file
! --------------
    IF ( optfix >= 1 ) THEN

      CALL opnfil(lfn002,files(5,ifil),'UNKNOWN','FORMATTED',' ',' ',irc)
      CALL opnerr(lfnerr,lfn002,irc,files(5,ifil),'snx2nq0')

      WRITE(lfn002, '(A,/,80("-"),//,A,/,A)') &
                    title,                    &
                    'Station name',           &
                    '****************'

      DO ipar1 = 1, snxSta%nStaInt
        IF (optfix == 1 .AND. snxSta%staInt(ipar1)%flgCon == 2) CYCLE
        IF (optfix >= 3 .AND. snxSta%staInt(ipar1)%flgCon /= optfix-3) CYCLE
        WRITE(lfn002, '(A20)') snxSta%staInt(ipar1)%staNam
      ENDDO

      CLOSE(lfn002)
    ENDIF

! Transform the station velocities into the coordinates
! -----------------------------------------------------
    IF (noneq == 0) THEN
      LOOP_ipar2: DO ipar2 = 1, neq%misc%npar
        IF ( neq%par(ipar2)%locq(1) == 1  .AND. &
             neq%par(ipar2)%locq(4) == 3 ) THEN

          DO ipar1 = 1, neq%misc%npar
            IF ( neq%par(ipar1)%locq(1) == 1                        .AND. &
                 neq%par(ipar1)%locq(4) == 0                        .AND. &
                 neq%par(ipar1)%locq(3) == neq%par(ipar2)%locq(3)   .AND. &
                 neq%par(ipar1)%name    == neq%par(ipar2)%name    ) THEN

              CALL sintran3(neq,neq%aNor(:),neq%bNor(:),ipar1,ipar2,sintim)

              neq%par(ipar1)%locq(4) = 1
              neq%par(ipar2)%locq(4) = 2

              CYCLE LOOP_ipar2
            END IF
          END DO

        END IF
      END DO LOOP_ipar2

      opt%neqout = files(2,ifil)

      neq%misc%datum  = ' '
      neq%misc%nutmod = 'IAU2000R06'
      neq%misc%submod = 'IERS2010'

      CALL neqsort(neq)
      CALL neqstore(opt%neqout,neq)

    ENDIF

  END DO

  CALL exitrc(0)

END PROGRAM snx2nq0

