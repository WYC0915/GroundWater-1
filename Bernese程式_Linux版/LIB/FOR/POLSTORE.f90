MODULE s_POLSTORE
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE polstore(neq)

! -------------------------------------------------------------------------
! Purpose:    An interface routine between the program ADDNEQ2 and
!             routines which store the pole parameter into files
!             (in both, IERS and Bernese, formats)
!
! Author:     L. Mervart
!
! Created:    22-Nov-1997
!
! Changes:    26-Jun-2001 RD: Use alcerr for allocation
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             08-Feb-2002 MM: New gtweight call
!             26-Mar-2002 SS: Use filtitle
!             03-Feb-2003 DT: Set CrdFlag=0 if only one Offset available
!             04-Feb-2003 PS: Use nutnam and subnam
!                             Call to sr getpol changed
!             06-Feb-2003 MR: Test for Leap seconds corrected
!             19-Feb-2003 PS: Changed default Nutation Model
!             14-Mar-2003 PS: Warning if default Nutation Model is used
!             04-Apr-2003 RD: opt%polstep in sec now
!             23-Apr-2003 AJ: Nullify local pointers
!             14-Jun-2003 HU: Default nutation IAU80
!             12-Aug-2003 RS: Check for ipar1, ipar2 =0 (no writing)
!             18-Aug-2003 RD: Correct file handling
!             01-Sep-2003 HU: Use interface for whicherp
!             08-Sep-2003 HU: Get nutnam, subnam from NEQ
!             05-Nov-2003 HU: Additional arguments for getpol
!             23-Mar-2004 DT: Take nutnam and subnam from neq%misc only
!                             if neq%version>1
!             31-Jan-2005 HU: Rounding problem wrt last written record
!             28-Jun-2005 MM: Unused variables removed
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             03-Oct-2005 MM: ABS added for rates RMS (IERS format)
!             25-Aug-2008 DT: Scale nutation correctly if taken from apriori
!             17-Nov-2008 MF: Test leap second for output file
!             17-Nov-2008 PS: bugfix for days before days with leap seconds
!                             in combination with 1h subdaily ERPs
!             20-Dec-2010 DT: bugfix w.r.t. constant pole (double epochs)
!             03-Jan-2011 DT: Count #Satellites (NT in IEP file) and
!                             #Stations "fixed" in case of FREENET correctly
!             05-Mar-2012 RD: Use listi4 as module now
!             21-Nov-2012 DT: Bugfix in rmsrat computation (+2 -> -2)
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnerr, lfnres, keyValueLength
  USE m_maxdim, ONLY: maxsat
  USE d_neq,    ONLY: t_neq
  USE p_addneq, ONLY: opt,comstat
  USE d_const,  ONLY: pi,filtitle

  USE f_ikf
  USE s_alcerr
  USE s_opnfil
  USE s_poldef
  USE s_opnerr
  USE s_ut1red
  USE f_gtweight
  USE s_getpol
  USE s_rdpolh
  USE s_readkeys
  USE s_wtpole
  USE s_cpodef
  USE s_wtpolh
  USE s_wtpoli
  USE s_whicherp
  USE s_ckoptl
  USE s_sort
  USE f_listi4
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_neq)   :: neq

! Local Variables
! ---------------
  INTEGER(i4b)                              :: ipar
  INTEGER(i4b), DIMENSION(:,:), ALLOCATABLE :: ipar1
  INTEGER(i4b), DIMENSION(:,:), ALLOCATABLE :: ipar2
  INTEGER(i4b)                              :: icrd
  INTEGER(i4b)                              :: ipol
  INTEGER(i4b)                              :: npol, npol2
  INTEGER(i4b)                              :: ios, iac
  INTEGER(i4b)                              :: filtyp
  INTEGER(i4b), DIMENSION(2)                :: polTyp
  INTEGER(i4b)                              :: isubfl
  INTEGER(i4b)                              :: ik11
  INTEGER(i4b)                              :: ik12
  INTEGER(i4b)                              :: ik22
  INTEGER(i4b)                              :: nsta
  INTEGER(i4b)                              :: nfix
  INTEGER(i4b)                              :: nsat
  INTEGER(i4b)                              :: ip1
  INTEGER(i4b)                              :: ip2
  INTEGER(i4b)                              :: ic1
  INTEGER(i4b)                              :: ic2
  INTEGER(i4b)                              :: irc
  INTEGER(i4b)                              :: irCode
  INTEGER(i4b)                              :: iform
  INTEGER(i4b)                              :: iend
  INTEGER(i4b), DIMENSION(2)                :: ptdummy
  INTEGER(i4b), DIMENSION(5)                :: nErp
  INTEGER(i4b), DIMENSION(5)                :: ErpFlag ! 0 = offset-only
                                                       ! 1 = pwl polygon
                                                       ! 2 = pwl (offset+drift)
  INTEGER(i4b), DIMENSION(maxSat)           :: satLst
  INTEGER(i4b)                              :: iPos
  REAL(r8b)                                 :: PoleTimeMin
  REAL(r8b)                                 :: PoleTimeMax
  REAL(r8b)                                 :: PoleTimeMean
  REAL(r8b)                                 :: val1
  REAL(r8b)                                 :: val2
  REAL(r8b)                                 :: dt1
  REAL(r8b)                                 :: dt2
  REAL(r8b),    DIMENSION(2)                :: tabTim
  REAL(r8b),    DIMENSION(2)                :: xPol
  REAL(r8b),    DIMENSION(2)                :: yPol
  REAL(r8b),    DIMENSION(2)                :: ut1utc
  REAL(r8b),    DIMENSION(2)                :: gpsutc
  REAL(r8b),    DIMENSION(2)                :: deps
  REAL(r8b),    DIMENSION(2)                :: dpsi
  REAL(r8b),    DIMENSION(5)                :: aprPol
  REAL(r8b),    DIMENSION(5)                :: erpunit
  REAL(r8b),    DIMENSION(:),   ALLOCATABLE :: polTim
  REAL(r8b),    DIMENSION(:,:), ALLOCATABLE :: coef1
  REAL(r8b),    DIMENSION(:,:), ALLOCATABLE :: coef2
  REAL(r8b),    DIMENSION(:,:), ALLOCATABLE :: polcoo
  REAL(r8b),    DIMENSION(:,:), ALLOCATABLE :: rmspol
  REAL(r8b),    DIMENSION(5)                :: rate
  REAL(r8b),    DIMENSION(5)                :: rmsrat
  REAL(r8b),    DIMENSION(3)                :: correl
  REAL(r8b)                                 :: korr
  REAL(r8b)                                 :: weight
  REAL(r8b)                                 :: sigma
  REAL(r8b)                                 :: hlp
  LOGICAL,      DIMENSION(5)                :: CrdFlag
  CHARACTER(LEN=3)                          :: remark
  CHARACTER(LEN=16)                         :: nutnam
  CHARACTER(LEN=16)                         :: subnam
  CHARACTER(LEN=16)                         :: nutnaerp
  CHARACTER(LEN=16)                         :: subnaerp
  CHARACTER(LEN=32)                         :: polfil
  CHARACTER(LEN=45)                         :: srName= 'SR polstore'
  CHARACTER(LEN=80)                         :: title
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue

  NULLIFY(keyValue)

  IF (opt%polers == '' .AND. opt%ierspol == '') RETURN

  erpunit(1) = 1000.d0 * 180.d0 * 3600.d0 / PI
  erpunit(2) = erpunit(1)
  erpunit(3) = 1000.d0 *  24.d0 * 3600.d0
  erpunit(4) = erpunit(1)
  erpunit(5) = erpunit(1)

  nutnam=''
  subnam=''

  PoleTimeMin = HUGE(0.d0)
  PoleTimeMax = 0.d0
  CrdFlag     = .FALSE.
  npol2       = 0
  nsta        = 0
  nfix        = 0
!!!  nsat        = 24
  nSat        = 0
  nErp(:)     = 0
  ErpFlag(:)  = 0

  DO ipar = 1,neq%misc%npar

! Number of Stations etc.
! -----------------------
    IF ( neq%par(ipar)%locq(1) == 1 ) THEN
      nsta = nsta + 1
      weight = gtweight(neq%par(ipar),'A')
      IF (weight > 0.d0) THEN
        sigma = 1.d0 / SQRT(weight) * opt%sigma0 / neq%par(ipar)%scale
        IF (sigma > 0.d0 .AND. sigma < 0.001d0 .OR. &
            sigma == 10d0                          ) THEN
          nfix = nfix + 1
        END IF
      END IF
    END IF

! Count number of satellites
! --------------------------
    IF ( neq%par(iPar)%locq(1) == 3 ) THEN
      iPos = listi4(1, maxSat, satLst, neq%par(iPar)%locq(3), nSat)
    END IF

! Find the Time Interval for Pole Parameters and subnam, nutnam
! -------------------------------------------------------------
    IF ( neq%par(ipar)%locq(1) == 10 ) THEN

      icrd = neq%par(ipar)%locq(4)

      IF ( neq%par(ipar)%locq(5) == 1 ) THEN
        CrdFlag(icrd) = .TRUE.
        nErp(icrd)    = nErp(icrd) + 1

    ! Piecewise linear representation as polygon
        IF ( neq%par(ipar)%time%half == 0d0  )  &
          ErpFlag(icrd) = 1

    ! Piecewise linear representation with offset+drift
      ELSEIF ( neq%par(ipar)%locq(5) == 2 ) THEN
        ErpFlag(icrd) = 2
      END IF

      IF (PoleTimeMin > neq%par(ipar)%time%mean-neq%par(ipar)%time%half) THEN
        PoleTimeMin = neq%par(ipar)%time%mean-neq%par(ipar)%time%half
      END IF
      IF (PoleTimeMax < neq%par(ipar)%time%mean+neq%par(ipar)%time%half) THEN
        PoleTimeMax = neq%par(ipar)%time%mean+neq%par(ipar)%time%half
      END IF
      IF ( neq%par(ipar)%locq(4) == 1 ) THEN
        subnam=neq%par(ipar)%name(1:16)
      END IF
      IF ( neq%par(ipar)%locq(4) == 4 ) THEN
        nutnam=neq%par(ipar)%name(1:16)
      END IF
    END IF
  END DO
  nsta = nsta / 3
  nfix = nfix / 3

  IF (PoleTimeMax == 0.d0) RETURN

! Get the Pole Model Type
! -----------------------
  PoleTimeMean = (PoleTimeMin + PoleTimeMax) / 2.d0
  CALL getpol(PoleTimeMean,0,tabTim,xPol,yPol,ut1utc,gpsutc,deps,dpsi,polTyp)
  polTyp(1) = 2   ! Nutation Model Always Set to "Observed"
  remark = 'GPS'

! Find Number of Sets to be Stored
! --------------------------------
  npol = FLOOR( (PoleTimeMax - PoleTimeMin +1D-7) / (opt%PoleStep/86400.d0) ) + 1

! Add double epochs in case of offset-only or offset+drift
! --------------------------------------------------------
  npol2 = npol
  IF ( ErpFlag(1) == 0 .OR. ErpFlag(1) == 2 ) THEN
    npol2 = npol2 + nErp(1) - 1
  END IF

  ALLOCATE(polTim(npol2), stat=iac)
  CALL alcerr(iac, 'polTim', (/npol2/), 'polstore')
  ALLOCATE(polcoo(5,npol2), stat=iac)
  CALL alcerr(iac, 'polcoo', (/5,npol2/), 'polstore')
  ALLOCATE(rmspol(5,npol2), stat=iac)
  CALL alcerr(iac, 'rmspol', (/5,npol2/), 'polstore')
  ALLOCATE(ipar1(5,npol2), stat=iac)
  CALL alcerr(iac, 'ipar1', (/5,npol2/), 'polstore')
  ALLOCATE(ipar2(5,npol2), stat=iac)
  CALL alcerr(iac, 'ipar2', (/5,npol2/), 'polstore')
  ALLOCATE(coef1(5,npol2), stat=iac)
  CALL alcerr(iac, 'coef1', (/5,npol2/), 'polstore')
  ALLOCATE(coef2(5,npol2), stat=iac)
  CALL alcerr(iac, 'coef2', (/5,npol2/), 'polstore')


! Set epochs to be written into file
! 1. Normal ADDNEQ case (piece-wise linear)
! -----------------------------------------
  DO ipol=1,npol
    polTim(ipol) = PoleTimeMin + (ipol-1) * (opt%PoleStep/86400.d0)
  END DO

! 2. Double epoch at interval boundaries (constant pole or Offset+drift)
! ----------------------------------------------------------------------
  IF ( ErpFlag(1) == 0 .OR. ErpFlag(1) == 2 )  THEN
    DO ipar = 1,neq%misc%npar

      IF ( neq%par(ipar)%locq(1) /= 10 ) CYCLE

      IF ( neq%par(ipar)%locq(4) == 1 .AND. &
           neq%par(ipar)%locq(5) == 1 .AND. &
           ABS(neq%par(ipar)%time%mean - PoleTimeMin) > 1d0/1440d0 .AND. &
           ABS(neq%par(ipar)%time%mean+neq%par(ipar)%time%half - PoleTimeMax) > 1d0/1440d0 ) THEN

        npol = npol + 1
        polTim(npol) = neq%par(ipar)%time%mean + neq%par(ipar)%time%half
      END IF
    END DO
  END IF

  CALL sort(npol,polTim)

! Find the EOP Values for Desired Time
! ------------------------------------
  DO ipol=1,npol
    isubfl = 0
    CALL poldef(polTim(ipol), isubfl, aprPol(1), aprPol(2), aprPol(3),  &
                gpsutc(1))
    CALL cpodef(polTim(ipol), aprPol(4), aprPol(5))

    DO icrd = 1, 5

      aprPol(icrd) = aprPol(icrd) * erpunit(icrd)

    ! Not estimated
      IF (.NOT. CrdFlag(icrd)) THEN
        polcoo(icrd,ipol) = aprPol(icrd)

       ! Scale nutations correctly
        IF ( icrd>=4 ) THEN
          polcoo(icrd,ipol) = polcoo(icrd,ipol) * 1.D-3
        END IF

        rmspol(icrd,ipol) = 0.d0
        CYCLE
      END IF

      CALL whicherp(neq, icrd, polTim(ipol), ErpFlag, ipar1(icrd,ipol), &
                    ipar2(icrd,ipol), coef1(icrd,ipol), coef2(icrd,ipol))

      IF ( ErpFlag(icrd) > 0  .AND. &
           (ipar1(icrd,ipol) == 0 .OR. ipar2(icrd,ipol) == 0) ) CYCLE

      dt1 = coef1(icrd,ipol)
      dt2 = coef2(icrd,ipol)

      ik11 = ikf( ipar1(icrd,ipol), ipar1(icrd,ipol) )
      ik12 = ikf( ipar1(icrd,ipol), ipar2(icrd,ipol) )
      ik22 = ikf( ipar2(icrd,ipol), ipar2(icrd,ipol) )

    ! Piece-wise linear
    ! -----------------
      IF ( ErpFlag(icrd) > 0 ) THEN
        val1 = neq%par(ipar1(icrd,ipol))%x0 + neq%xxx(ipar1(icrd,ipol))
        val2 = neq%par(ipar2(icrd,ipol))%x0 + neq%xxx(ipar2(icrd,ipol))

        ! Test the leap-second
        ! --------------------
        IF (icrd == 3 .AND. neq%par(ipar2(icrd,ipol))%locq(5) == 1) THEN
          IF      ( val1 - val2 >  500.d0) THEN
            val2 = val2 + 1000.d0
          ELSE IF ( val1 - val2 < -500.d0) THEN
            val2 = val2 - 1000.d0
          END IF
        END IF

        polcoo(icrd,ipol) = val1 * dt1 + val2 * dt2

        rmspol(icrd,ipol) = comstat%rms                          *  &
                      SQRT(ABS(       neq%aNor(ik11) * dt1 * dt1 +  &
                                2.0 * neq%aNor(ik12) * dt1 * dt2 +  &
                                      neq%aNor(ik22) * dt2 * dt2    ))

    ! Piece-wise constant
    ! -------------------
      ELSE
        IF ( ipol==1 .OR. polTim(ipol)-polTim(ipol-1) > 1d0/1440d0 ) THEN
          polcoo(icrd,ipol) = aprPol(icrd) + neq%xxx(ipar1(icrd,ipol))
          rmspol(icrd,ipol) = comstat%rms * SQRT(ABS(neq%aNor(ik11)))

      ! Add double epoch
        ELSE
          polcoo(icrd,ipol) = aprPol(icrd) + neq%xxx(ipar2(icrd,ipol))
          rmspol(icrd,ipol) = comstat%rms * SQRT(ABS(neq%aNor(ik22)))
        END IF

      END IF


! Reduce UT1
! ----------
      IF (icrd == 3) THEN
        CALL ut1red(polTim(ipol), korr)
        polcoo(icrd,ipol) = polcoo(icrd,ipol) + korr
      END IF
      polcoo(icrd,ipol) = polcoo(icrd,ipol) / 1000.0d0
      rmspol(icrd,ipol) = rmspol(icrd,ipol) / 1000.0d0

    END DO  ! icrd
  END DO  ! ipol

! Consistency check nutnam and subnam from ERP- and NEQ-File
! ----------------------------------------------------------
  IF ( nutnam == '' .AND. neq%version > 1 ) nutnam = neq%misc%nutmod
  IF ( subnam == '' .AND. neq%version > 1 ) subnam = neq%misc%submod

  IF ( nutnam == '' .AND. subnam == '' ) THEN
    nutnam = 'IAU80'
    subnam = 'RAY'
  ELSEIF ( nutnam == '') THEN
    nutnam = 'IAU2000'
    WRITE(lfnerr,370) nutnam
  ELSEIF ( subnam == '') THEN
    subnam = 'RAY'
    WRITE(lfnerr,371) subnam
  END IF

  CALL readKeys('POLE', keyValue, irc)
  CALL ckoptl(0,'POLE',keyValue,srName,                                   &
          'ERP File',irc,irCode,empty=' ',                    &
           maxLength=32,maxVal=1,result1=polfil)

  CALL opnfil(lfnres,polfil,'UNKNOWN','FORMATTED',' ',' ',ios)
  CALL opnerr(lfnerr,lfnres,ios,polfil,'POLSTORE')
  CALL rdpolh(lfnres,1,title,ptdummy,iform,iend,nutnaerp,subnaerp)
  CLOSE (lfnres)

  IF ( subnam .NE. subnaerp ) THEN
    WRITE(lfnerr,375) subnam,subnaerp
  END IF

  IF ( nutnam .NE. nutnaerp ) THEN
    WRITE(lfnerr,380) nutnam,nutnaerp
  END IF

370       FORMAT(/,' ### SR POLSTORE: No Nutation Model in NQ0 File ',/, &
                16X,'  Using as default: ',A,/)

371       FORMAT(/,' ### SR POLSTORE: No Subdaily Model in NQ0 File ',/, &
                16X,'  Using as default: ',A,/)

375       FORMAT(/,' ### SR POLSTORE: Different Subdaily Pole Models ',/, &
                16X,'  NEQ File: ',A,/,      &
                16X,'  ERP File: ',A,/)

380     FORMAT(/,' ### SR POLSTORE: Different Nutation Models ',/,   &
                16X,'  NEQ File: ',A,/,      &
                16X,'  ERP File: ',A,/)


! Store Pole in Bernese Format
! ----------------------------
  IF (opt%polers /= '') THEN
    CALL opnfil(lfnres,opt%polers,'UNKNOWN','FORMATTED',' ',' ',ios)
    CALL opnerr(lfnerr,lfnres,ios,opt%polers,'POLSTORE')
    filtyp    = 1

    CALL wtpolh(lfnres, filtyp, filtitle, polTyp, nutnam, subnam)

    DO ipol=1,npol

! Skip writing if ipar1, ipar2 = 0
      IF ( ErpFlag(1) > 0  .AND. &
           (ipar1(1,ipol) == 0 .OR. ipar2(1,ipol) == 0) ) CYCLE

      isubfl = 0
      ! Test the leap-second
      ! --------------------
      IF ( ipar1(3,ipol) > 0 .AND. &
           ipar2(3,ipol) > 0 .AND. &
           neq%par(ipar2(3,ipol))%locq(5) == 1 ) THEN

! Round epochs at day boundaries (necessary due to numerical problems for days before days
! with leap seconds and subdaily ERPs with 1 h parameter spacing
        IF (DABS(DNINT(neq%par(ipar1(3,ipol))%time%mean) - &
                       neq%par(ipar1(3,ipol))%time%mean)<1.0d-9) THEN
           neq%par(ipar1(3,ipol))%time%mean = &
                       DNINT(neq%par(ipar1(3,ipol))%time%mean)
        END IF

        CALL poldef(neq%par(ipar1(3,ipol))%time%mean, isubfl, aprPol(1), &
                    aprPol(2), aprPol(3), gpsutc(1))
      ELSE
        CALL poldef(polTim(ipol), isubfl, aprPol(1), aprPol(2), aprPol(3), &
                  gpsutc(1))
      END IF
      gpsutc(1) = dnint(gpsutc(1)*86400.d0)
      CALL wtpoli(lfnres, polTim(ipol), polcoo(:,ipol), gpsutc(1), remark, &
                  rmspol(:,ipol))
    END DO

    CLOSE (lfnres)

  END IF

! Store Pole in IERS Format
! -------------------------
  IF (opt%ierspol /= '') THEN
    CALL opnfil(lfnres,opt%ierspol,'UNKNOWN','FORMATTED',' ',' ',ios)
    CALL opnerr(lfnerr,lfnres,ios,opt%ierspol,'POLSTORE')
    filtyp    = 2
    CALL wtpolh(lfnres, filtyp, filtitle, polTyp,nutnam,subnam)

! Compute the rates
! -----------------
    DO ipol=1,npol

! Skip writing if ipar1, ipar2 = 0
      IF ( ErpFlag(1) > 0  .AND. &
           (ipar1(1,ipol) == 0 .OR. ipar2(1,ipol) == 0) ) CYCLE

      DO icrd=1,5
        IF (ipol < npol) THEN
          ip1 = ipol
          ip2 = ipol+1
        ELSE
          ip1 = ipol-1
          ip2 = ipol
        END IF

      ! Handle double epochs
        IF ( ABS(polTim(ip2) - polTim(ip1)) < 1d0/1440d0 ) THEN
          ip1 = ipol-1
          ip2 = ipol
        END IF

       !! Test if only one offset available
       ! ----------------------------------
        IF ( npol == 1 ) THEN
           CrdFlag(icrd) = .FALSE.

           rate(icrd) = 0.d0

        ELSE

           rate(icrd) = polcoo(icrd,ip2) - polcoo(icrd,ip1)

           ! Test the leap-second
           ! --------------------
           IF (icrd == 3) THEN
             IF      ( rate(icrd) >  0.5d0) THEN
               rate(icrd) = rate(icrd) - 1.d0
             ELSE IF ( rate(icrd) < -0.5d0) THEN
               rate(icrd) = rate(icrd) + 1.d0
             END IF
           END IF

           rate(icrd) = rate(icrd) / ( polTim(ip2) - polTim(ip1) )

        END IF

        IF (CrdFlag(icrd) .AND. ErpFlag(icrd)>0 ) THEN

          ik11 = ikf(ipar1(icrd,ip1),ipar1(icrd,ip2))
          ik12 = ikf(ipar1(icrd,ip1),ipar2(icrd,ip2))
          ik22 = ikf(ipar2(icrd,ip1),ipar2(icrd,ip2))

          hlp  = ( neq%aNor(ik11) * coef1(icrd,ip1) * coef1(icrd,ip2) +   &
                   neq%aNor(ik12) * coef1(icrd,ip1) * coef2(icrd,ip2) +   &
                   neq%aNor(ik12) * coef2(icrd,ip1) * coef1(icrd,ip2) +   &
                   neq%aNor(ik22) * coef2(icrd,ip1) * coef2(icrd,ip2) ) * &
                 comstat%rms**2 / 1.0d6

          rmsrat(icrd) = SQRT( ABS((rmspol(icrd,ip1)**2 - 2.0 * hlp +  &
                               rmspol(icrd,ip2)**2 )           /  &
                         ( polTim(ip2) - polTim(ip1) )))
        ELSE
          rmsrat(icrd) = 0.0d0
        END IF
      END DO

      DO ic1 = 1, 2
        DO ic2 = ic1+1, 3
          IF ( CrdFlag(ic1) .AND. CrdFlag(ic2)  .AND. &
               ErpFlag(ic1)>0 .AND. ErpFlag(ic2)>0   ) THEN
            ik11 = ikf(ipar1(ic1,ipol),ipar1(ic2,ipol))
            ik12 = ikf(ipar1(ic1,ipol),ipar2(ic2,ipol))
            ik22 = ikf(ipar2(ic1,ipol),ipar2(ic2,ipol))

            correl( ic1 + ic2 - 2 ) =                                     &
                ( neq%aNor(ik11) * coef1(ic1,ipol) * coef1(ic2,ipol)   +  &
                  neq%aNor(ik12) * coef1(ic1,ipol) * coef2(ic2,ipol)   +  &
                  neq%aNor(ik12) * coef2(ic1,ipol) * coef1(ic2,ipol)   +  &
                  neq%aNor(ik22) * coef2(ic1,ipol) * coef2(ic2,ipol) ) /  &
                ( rmspol(ic1,ipol) * rmspol(ic2,ipol) * 1.0d6        ) *  &
                comstat%rms**2
          ELSE
            correl( ic1 + ic2 - 2 ) = 0.0d0
          END IF
        END DO
      END DO

      CALL wtpole(lfnres, polTim(ipol), polcoo(:,ipol), rmspol(:,ipol),    &
                  rate(:), rmsrat(:), correl(:), nsta, nfix, nsat)
    END DO

    CLOSE (lfnres)

  END IF

  DEALLOCATE(coef2, stat=iac)
  DEALLOCATE(coef1, stat=iac)
  DEALLOCATE(ipar2, stat=iac)
  DEALLOCATE(ipar1, stat=iac)
  DEALLOCATE(rmspol, stat=iac)
  DEALLOCATE(polcoo, stat=iac)
  DEALLOCATE(polTim, stat=iac)
  DEALLOCATE(keyValue,stat=iac)


END SUBROUTINE polstore

END MODULE
