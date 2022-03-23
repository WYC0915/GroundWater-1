MODULE s_SINSTORE
PRIVATE sinstore_getsn
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE sinstore(neq,regMat,aNor_free,bNor_free)

! -------------------------------------------------------------------------
! Purpose:    This routine stores the results in SINEX format
!
! Author:     L.Mervart
!
! Created:    12-Dec-2005
!
! Changes:    07-Jul-2000 SS: Write station description correctly
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             06-Aug-2002 HU: Order station names, print each antenna once
!                             Write special header for EUREF
!             11-Aug-2002 HU: More statistical infos written, format modif.
!             12-Aug-2002 HU: Sinex headers adapted
!             13-Aug-2002 HU: Sinex header adapted
!             28-Aug-2002 HU: Keyword 'PHASE MEASUREMENTS SIGMA' corrected
!             26-Oct-2002 HU: Warning if writing gcc to SINEX file
!             10-Mar-2003 HU: Write station description
!             17-May-2003 HU: Initialize structure
!             08-Sep-2003 HU: antnam, recnam chr16 -> chr20
!             09-Mar-2004 HU: Writing of solution contents improved
!             09-Mar-2004 SS: Write more than one block from SINEX.-file
!             10-Mar-2004 HU: Error handling for reading SINEX.-file
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             12-Dec-2005 CU: Add npseuel for statistic block
!             23-Feb-2006 AG: Write satellite antenna offsets
!             23-Mar-2006 SS: Write radome codes in GPS_PHASE_CENTER block
!             20-Apr-2006 AG: Reduction of receiver antennas
!             16-Nov-2006 AG: Correct PRN written in SATELLITE/ID block
!             08-Feb-2007 RD: misc%nObs/nParms i4b->r8b
!             12-Feb-2007 AG: Debug output and unused variables removed
!             13-Feb-2007 AG: Write NEQ in SINEX
!             07-May-2007 AG: Store elDepMod in sinstring array
!             22-May-2007 AG: Write SOLUTION/ESTIMATE into SINEX with NEQ
!             13-Feb-2008 RD: Adapt to par%name == chr*20
!             02-Mar-2009 SL: wfact_apr and parameter a&bNor_free added,
!                             parString corrected wrt version 2.01
!             12-Mar-2009 SL: error message extended by neq%par%name
!                             flgCon corrected wrt opt%snxReg
!             02-Jul-2009 DT: Correct Solution content in Header line (X/V->S)
!             06-Jul-2009 DT: Get technique from neq%par; write blocks
!                             SITE/RECEIVER, /ANTENNA, /GPS_PHASE_CENTER
!                             BIAS/EPOCHS technique-dependent;
!                             sampling interval only for technique "P"
!             22-Jul-2009 DT: Set opt%timRefCrd for Coord parameters if no Vel.
!             25-Feb-2010 DT: Use correct solID for troposphere parameters
!             30-Sep-2010 RD: Consider rounding effects for "flgCon"
!             19-Oct-2010 SL: consider antenna S/N from STAINFO file
!             27-Oct-2010 SL: use m_bern with ONLY, removal of unused modules
!             11-Jan-2011 DT: Improve setting of constraint flag
!             27-Mar-2012 RD: Use LISTC1 as module now
!             22-Apr-2012 RD: Nullify pointers
!             28-Sep-2012 SL: Header in old ADDNEQ style changed
!             31-Oct-2012 SS: Alternated sign for LOD/LODR bNor
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnRes, lfnErr, lfnLoc, staNameLength, &
                      keyValueLength
  USE m_global, ONLY: maxsys
  USE d_const,  ONLY: pi
  USE d_stacrx, ONLY: t_stacrux, init_stacrux, undef_i
  USE d_neq,    ONLY: t_neq
  USE d_phaecc, ONLY: init_buf
  USE p_addneq, ONLY: opt,comstat

  USE f_ikf
  USE s_opnfil
  USE s_sinpsort
  USE s_readcrux
  USE s_sinini
  USE f_djul
  USE s_opnerr
  USE s_cordup
  USE f_listc1
  USE f_sinstati
  USE s_sindat
  USE s_sinpartu
  USE s_exitrc
  USE f_ut1_ut1r
  USE s_radgms
  USE s_readkeys

  USE f_tstflg
  USE f_gtweight

  IMPLICIT NONE

! List of Input Parameters
! ------------------------
  TYPE(t_neq)             :: neq
  REAL(r8b), DIMENSION(*) :: regMat
  REAL(r8b), DIMENSION(*) :: aNor_free
  REAL(r8b), DIMENSION(*) :: bNor_free

! Parameters
! ----------
  REAL(r8b),        PARAMETER  :: version     = 2.01

! Local Variables
! ---------------
  CHARACTER(LEN=3)               :: agencyFile
  CHARACTER(LEN=3)               :: agencyData
  CHARACTER(LEN=12)              :: sysTime
  CHARACTER(LEN=12)              :: startTime
  CHARACTER(LEN=12)              :: endTime
  CHARACTER(LEN=12)              :: meanTime
  CHARACTER(LEN=12)              :: refTime
  CHARACTER(LEN=1),DIMENSION(6)  :: parString
  CHARACTER(LEN=6)               :: parType
  CHARACTER(LEN=1)               :: flgCon
  CHARACTER(LEN=1),DIMENSION(neq%misc%npar) :: flgConPar
  CHARACTER(LEN=4)               :: siteCode
  CHARACTER(LEN=2)               :: pointCode
  CHARACTER(LEN=9)               :: domes
  CHARACTER(LEN=1)               :: signum
  CHARACTER(LEN=4)               :: solID
  CHARACTER(LEN=5)               :: antNum
  CHARACTER(LEN=10)              :: elDepMod
  CHARACTER(LEN=3)               :: refSys
  CHARACTER(LEN=4)               :: unit
  CHARACTER(LEN=1)               :: technique
  CHARACTER(LEN=5)               :: serNumber
  CHARACTER(LEN=11)              :: firmware
  CHARACTER(LEN=22)              :: descri
  CHARACTER(LEN=22),DIMENSION(3) :: hlpstr
  CHARACTER(LEN=80)              :: line
  CHARACTER(LEN=80)              :: linhed
  CHARACTER(LEN=26)              :: antchr
  CHARACTER(LEN=9)               :: cosparID
  CHARACTER(LEN=20)              :: satAntTyp
  CHARACTER(LEN=1)               :: pcvTyp
  CHARACTER(LEN=1)               :: pcvMod
  CHARACTER(LEN=26),DIMENSION(neq%misc%nstat_sinex) :: antlst
  CHARACTER(LEN=1),DIMENSION(6)  :: obsCode
  CHARACTER(LEN=keyValueLength),DIMENSION(:), POINTER :: keyValue

  INTEGER(i4b)                   :: ios, irc
  INTEGER(i4b), DIMENSION(8)     :: hlpTime
  INTEGER(i4b)                   :: ipar
  INTEGER(i4b)                   :: ip1
  INTEGER(i4b)                   :: ip2
  INTEGER(i4b)                   :: phi1
  INTEGER(i4b)                   :: phi2
  INTEGER(i4b)                   :: lam1
  INTEGER(i4b)                   :: lam2
  INTEGER(i4b)                   :: ista
  INTEGER(i4b)                   :: ii
  INTEGER(i4b)                   :: jj
  INTEGER(i4b)                   :: nWrite
  INTEGER(i4b)                   :: iWrite
  INTEGER(i4b)                   :: iparSrt
  INTEGER(i4b)                   :: iparSrt1
  INTEGER(i4b)                   :: iparSrt2
  INTEGER(i4b), DIMENSION(neq%misc%npar)        :: sortPar
  INTEGER(i4b), DIMENSION(neq%misc%nstat_sinex) :: staFlg
  INTEGER(i4b), DIMENSION(neq%misc%nstat_sinex) :: istant
  INTEGER(i4b), DIMENSION(neq%misc%nstat_sinex) :: indx
  INTEGER(i4b), DIMENSION(5,2)   :: numPolPar
  INTEGER(i4b)                   :: icrd, ider
  INTEGER(i4b)                   :: nlist
  INTEGER(i4b)                   :: ipos
  INTEGER(i4b)                   :: sataFlg
  INTEGER(i4b)                   :: isat
  INTEGER(i4b)                   :: isys
  INTEGER(i4b), DIMENSION(2)     :: freqCode
  INTEGER(i4b)                   :: individ
  INTEGER(i4b), DIMENSION(5)     :: techFlg
  INTEGER(i4b)                   :: freeCrd


  REAL(r8b)                             :: dayMon
  REAL(r8b)                             :: startMJD
  REAL(r8b)                             :: endMJD
  REAL(r8b), DIMENSION(3)               :: xyz
  REAL(r8b), DIMENSION(3)               :: ell
  REAL(r8b)                             :: phi3
  REAL(r8b)                             :: lam3
  REAL(r8b)                             :: estimate
  REAL(r8b)                             :: apriori
  REAL(r8b)                             :: value
  REAL(r8b)                             :: wfact
  REAL(r8b)                             :: wfact_apr
  REAL(r8b)                             :: vtPv
  REAL(r8b)                             :: dof
  REAL(r8b)                             :: hlpFlgCon
  REAL(r8b), DIMENSION(3,2)             :: antOffset
  REAL(r8b)                             :: wgtTst
  REAL(r8b)                             :: sigApr

  TYPE(t_stacrux)                       :: stCrx

  LOGICAL                               :: writeFlg
  LOGICAL                               :: velFlg


  NULLIFY(keyValue)
  CALL init_stacrux(stCrx)
  CALL init_buf(bufsize=(/25,1/))

  velFlg = .FALSE.

  hlpFlgCon    = 0.d0
  flgConPar(:) = ''

! Arrays for observation technique(s)
! -----------------------------------
  obsCode = (/'P', 'R', 'L', 'L', 'D', 'C'/)
  techFlg(:) = 0
  technique = 'P'  ! Default: GNSS

! Reconstruct statistical information
! -----------------------------------
  neq%misc%nobs = neq%misc%nobs + neq%misc%npseuel

  vTPv = -1d0
!!!  IF (opt%sincont == 2)    &
    vTPv = neq%misc%lTPl - DOT_PRODUCT(neq%xxx(1:neq%misc%npar), &
                                      neq%bNor(1:neq%misc%npar))

  dof = neq%misc%nobs - neq%misc%nparms

  IF (vTPv >= 0d0 .AND. dof > 0d0) THEN
    comstat%rms = SQRT( vTPv / dof )
  ELSE
    comstat%rms = opt%sigma0
  ENDIF

  wfact = comstat%rms * comstat%rms
  wfact_apr = opt%sigma0 * opt%sigma0

! Minimum constraint solution?
! ----------------------------
  freeCrd = 0
  CALL readkeys('RADIO2_3', keyValue, irc)
  IF (irc == 0 .AND. keyValue(1) == '1') THEN
    freeCrd = 1
  ENDIF

! Open the output file
! --------------------
  CALL opnfil(lfnres, opt%sinexrs, 'UNKNOWN', 'FORMATTED', ' ', ' ', ios)
  CALL opnerr(lfnerr, lfnres, ios, opt%sinexrs, 'SINSTORE')

! Get the creation date
! ---------------------
  CALL DATE_AND_TIME(values=hlpTime)
  dayMon = hlpTime(3) + &
    ( ( hlpTime(7) / 60.d0 + hlpTime(6) ) / 60.d0 + hlpTime(5) ) / 24.d0
  CALL sindat(0, djul(hlpTime(1), hlpTime(2), dayMon), sysTime)

! Read the stacrux file, if available
! -----------------------------------
  IF ( opt%stacrux /= ' ') THEN
    CALL readCrux(opt%stacrux, stCrx)
  ENDIF

! Sort the parameters
! -------------------
  CALL sinpsort(neq, sortPar)

! Header line
! -----------
  startMJD  =   HUGE(r8b)
  endMJD    = - HUGE(r8b)
  flgCon    = '2'
  parString = ' '
  sataFlg   = 0

  DO ipar = 1, neq%misc%npar
    IF ( neq%par(ipar)%time%mean - neq%par(ipar)%time%half < startMJD) &
      startMJD = neq%par(ipar)%time%mean - neq%par(ipar)%time%half
    IF ( neq%par(ipar)%time%mean + neq%par(ipar)%time%half > endMJD)   &
      endMJD   = neq%par(ipar)%time%mean + neq%par(ipar)%time%half

  ! Relationship of constraint and sigma_estimate per parameter (if COVA)
    IF ( opt%sincont == 2 ) THEN
      hlpFlgCon = sqrt( (wfact_apr * regMat(ikf(ipar,ipar))) / &
                        (wfact * neq%aNor(ikf(ipar,ipar))) )

  ! Constraint if NEQ written to SINEX
    ELSEIF ( opt%sincont == 1 ) THEN
      hlpFlgCon = 1.e+6
    ENDIF

    ! Set constraint codes:
    !  1) Free Network constraints
    ! ----------------------------
    IF ( freeCrd == 1 .AND. neq%par(ipar)%locq(1) == 1 ) THEN
      wgtTst = gtweight(neq%par(ipar),'A')

      IF ( wgtTst > 0d0 ) THEN
        flgConPar(ipar) = '1'
        IF (flgCon /= '0') flgCon = '1'
      ELSE
        flgConPar(ipar) = '2'
      END IF

    ELSE
      ! 2) Loose constraints / unconstrained
      ! ------------------------------------
      IF ( hlpFlgCon >= 10d0 ) THEN
        flgConPar(ipar) = '2'

      ! 3) fixed / tight constraints
      ! ----------------------------
      ELSEIF ( hlpFlgCon < sqrt(2d0) ) THEN
        ! Do not check UT and nutation for "solution constraint code"
        IF ( neq%par(ipar)%locq(1) == 10 .AND. &
             neq%par(ipar)%locq(4) >= 3  .AND. &
             neq%par(ipar)%locq(5) == 1       )  THEN
          flgConPar(ipar) = '0'
        ELSE
          flgConPar(ipar) = '0'
          flgCon = '0'
        END IF

      ! 4) significant constraints
      ! --------------------------
      ELSE
        flgConPar(ipar) = '1'
        IF (flgCon /= '0') flgCon = '1'
      END IF

    END IF

   ! Set parameter codes
   ! -------------------
    IF ( neq%par(ipar)%locq(1) ==  1 .AND. &
         neq%par(ipar)%locq(1) /=  3)     parString(1) = 'S'  ! Station
    IF ( neq%par(ipar)%locq(1) ==  1 .AND. &
         neq%par(ipar)%locq(1) ==  3)     parString(1) = 'S'  ! Station
    IF ( neq%par(ipar)%locq(1) == 26)     parString(1) = 'S'  ! Range biases
    IF ( neq%par(ipar)%locq(1) ==  3 )    parString(2) = 'O'  ! Orbits
    IF ( neq%par(ipar)%locq(1) == 10 )    parString(3) = 'E'  ! EOP
    IF ( neq%par(ipar)%locq(1) ==  6 )    parString(4) = 'T'  ! Troposphere
    IF ( neq%par(ipar)%locq(1) == 12 )    parString(6) = 'A'  ! Antenna

    IF ( neq%par(ipar)%locq(1) == 12 )    sataFlg=1

   ! Constant coordinates or velocitiy contained?
   ! --------------------------------------------
    IF ( neq%par(ipar)%locq(1) == 1 .AND. neq%par(ipar)%locq(4) > 1 ) &
       velFlg = .TRUE.

   ! Check for GNSS, VLBI, SLR, LLR, DORIS
   ! -------------------------------------
    IF ( technique == 'C' ) CYCLE
    DO ii = 1, 5
      IF ( tstflg(neq%par(ipar)%techn(1),ii-1) ) THEN

        techFlg(ii) = 1

        IF ( SUM(techFlg(:)) > 1 ) THEN
          technique = 'C'
          EXIT
        END IF

        technique = obsCode(ii)
        EXIT
      END IF
    END DO

  END DO

! Squeeze parString
  jj = 0
  DO ii = 1,6
    DO WHILE (ii+jj <= 6)
      IF ( parString(ii+jj) == ' ') THEN
        jj=jj+1
      ELSE
        EXIT
      ENDIF
    ENDDO
    IF (jj > 0) THEN
      IF (ii+jj <= 6) THEN
        parString(ii) = parString(ii+jj)
      ELSE
        parString(ii) = ' '
      ENDIF
    ENDIF
  ENDDO

  CALL sindat(0, startMJD, startTime)
  CALL sindat(0, endMJD,   endTime)

  CALL sinini(agencyFile, agencyData)

  WRITE(lfnres,'(A1,  A1,  A3,  1X,F4.2,  1X,A3,  1X,A12,  1X,A3,          &
         &       1X,A12,  1X,A12,  1X,A1,  1X,I5.5,  1X,A1,  6(1X,A1))' )  &
               '%', '=', 'SNX', version, agencyFile, sysTime, agencyData,  &
               startTime, endTime, technique, neq%misc%npar, flgCon,       &
               parString(1:6)

! Header in old Addneq style
! --------------------------
  IF(opt%ieuref==1) THEN
    WRITE(lfnres,"('*',79('-'),/, &
                 & '* SOLUTION INDEPENDENT EXCHANGE FORMAT (SINEX) ', &
                 &    'FOR SPACE GEODESY.',/, &
                 & '* - SINEX VERSION',F5.2,/, &
                 & '* - FILE CREATED BY PROGRAM ADDNEQ2',/, &
                 & '* - TECHNIQUE IS ',A1,/, &
                 & '* - SOLUTION COMMENTS:',/, &
                 & '*   ',A76,/, &
                 & '*   RMS OF UNIT WEIGHT:',F8.4,2X,'# OBS:',I11,2X, &
                 &    '# UNKNOWNS:',I9)") &
           version,technique,opt%title(1:76),comstat%rms,NINT(neq%misc%nobs), &
           NINT(neq%misc%nparms)
  ENDIF

! Reference Block
! ---------------
  CALL opnfil(lfnloc, opt%sinexin, 'OLD', 'FORMATTED', 'READONLY', ' ', ios)
  CALL opnerr(lfnerr, lfnloc, ios, opt%sinexin, 'SINSTORE')
  iWrite=0
  Loop_1: DO
    READ(lfnloc,'(A)',iostat=ios) line
    IF (ios /= 0) EXIT Loop_1
    IF (line == '+FILE/REFERENCE'        .OR.  &
        line == '+FILE/COMMENT'          .OR.  &
        line == '+INPUT/ACKNOWLEDGMENTS') THEN
      iWrite=1
      linhed=line
      READ(lfnloc,'(A)',iostat=ios) line
      IF (ios /= 0) EXIT Loop_1
      IF (line(1:1) == '-') THEN
        iWrite=0
      ELSE
        WRITE(lfnres,'("*",79("-"))')
        WRITE(lfnres,'(A)') linhed
      ENDIF
    ENDIF
    IF (iWrite == 1) THEN
      WRITE(lfnres,'(A)') line
      IF (line(1:1) == '-') iWrite=0
    ENDIF
  END DO Loop_1
  CLOSE(lfnloc)
  IF (iWrite == 1) THEN
    WRITE(lfnerr,"(/,' *** sr sinstore: Error reading SINEX input file', &
                &  /,'                  File: ',A,/)") TRIM(opt%sinexin)
    CALL exitrc(2)
  ENDIF

! Solution/Statistics Block
! -------------------------
  IF (neq%misc%nobs > 999999999d0) THEN
    WRITE(hlpstr(1),'(E22.15)') neq%misc%nobs
    WRITE(hlpstr(2),'(E22.15)') neq%misc%nparms
    WRITE(hlpstr(3),'(E22.15)') neq%misc%nobs - neq%misc%nparms
  ELSE
    WRITE(hlpstr(1),'(I22)') NINT(neq%misc%nobs)
    WRITE(hlpstr(2),'(I22)') NINT(neq%misc%nparms)
    WRITE(hlpstr(3),'(I22)') NINT(neq%misc%nobs - neq%misc%nparms)
  ENDIF
  WRITE(lfnres,'("*",79("-"))')
  WRITE(lfnres,'(A,/,A,/,3(1X,A30,1X,A,/),           &
                 &        (1X,A30,1X,F22.5))')       &
    '+SOLUTION/STATISTICS',                                      &
    '*_STATISTICAL PARAMETER________ __VALUE(S)____________',    &
        'NUMBER OF OBSERVATIONS        ', hlpstr(1),             &
        'NUMBER OF UNKNOWNS            ', hlpstr(2),             &
        'NUMBER OF DEGREES OF FREEDOM  ', hlpstr(3),             &
        'PHASE MEASUREMENTS SIGMA      ', opt%sigma0

  IF ( techFlg(1)==1 .OR. techFlg(5)==1 ) THEN
    WRITE(lfnres,'(1X,A30,1X,I22)') &
          'SAMPLING INTERVAL (SECONDS)   ', neq%misc%nsmpnq
  END IF

  ! SLR for ILRS
  ! ------------
  IF ( techFlg(3) == 1 ) THEN
    WRITE(lfnres,'(1X,A30,20X,A3)') &
          'SAMPLING INTERVAL (SECONDS)   ', '120'

    WRITE(lfnres,'(1X,A30,1X,F22.15)') &
        'SQUARE SUM OF RESIDUALS (VTPV)', vTPv
  END IF

  IF (opt%sincont == 2) THEN
    WRITE(lfnres,'(1X,A30,1X,F22.15)') &
        'VARIANCE FACTOR               ', (comstat%rms/opt%sigma0)**2
  ELSE IF (opt%sincont == 1) THEN
    WRITE(lfnres,'(1X,A30,1X,F22.15)') &
        'WEIGHTED SQUARE SUM OF O-C    ', neq%misc%lTPl
  ENDIF
  WRITE(lfnres,'("-SOLUTION/STATISTICS")')

! Site/ID Block
! -------------
  WRITE(lfnres,'("*",79("-"))')
  WRITE(lfnres,'(A)') '+SITE/ID'
  WRITE(lfnres,'(A)') &
  '*CODE PT __DOMES__ T _STATION DESCRIPTION__ APPROX_LON_ APPROX_LAT_ _APP_H_'

  DO iparSrt = 1, neq%misc%npar
    ipar=sortPar(iparSrt)

    ista = sinstati(neq, iparSrt, ipar, siteCode, pointCode, domes, solID,   &
                    technique, serNumber, firmware, refSys, startTime,       &
                    endTime, meanTime, refTime, antNum, xyz, ell, staFlg,    &
                    isat, cosparID, satAntTyp, freqCode, antOffset,elDepMod, &
                    pcvTyp,pcvMod)

    IF (ista <= 0) CYCLE

    CALL radgms(1, ell(1), signum, phi1, phi2, phi3)
    IF (signum == '-') phi1 = -phi1

    IF ( ell(2) < 0.d0 ) ell(2) = ell(2) + 2.d0 * PI
    CALL radgms(1, ell(2), signum, lam1, lam2, lam3)

! station description
    descri=neq%par(ipar)%name
    IF ( opt%stacrux /= ' ') THEN
      DO ii=1,stCrx%ninfo
        IF (neq%par(ipar)%name(1:staNameLength) == stCrx%stainfo(ii)%stanam)THEN
          descri=stCrx%stainfo(ii)%descri
        ENDIF
      ENDDO
    ENDIF

    WRITE(lfnres,'(1X,A4, 1X,A2, 1X,A9, 1X, A1, 1X,A22,  &
          &        2(1X,I3,1X,I2,1X,F4.1), 1X,F7.1)' )   &
          siteCode, pointCode, domes, technique, descri, &
          lam1, lam2, lam3, phi1, phi2, phi3, ell(3)

  END DO
  WRITE(lfnres,'(A)') '-SITE/ID'

! Site/Receiver Block (only for GNSS and DORIS)
! ---------------------------------------------
  IF ( techFlg(1)==1 .OR. techFlg(5)==1 ) THEN

    WRITE(lfnres,'("*",79("-"))')
    WRITE(lfnres,'(A)') '+SITE/RECEIVER'
    WRITE(lfnres,'(A)') &
    '*SITE PT SOLN T DATA_START__ DATA_END____ DESCRIPTION_________ S/N__ '//&
                                                                    'FIRMWARE___'

    DO iparSrt = 1, neq%misc%npar
      ipar=sortPar(iparSrt)

      ista = sinstati(neq, iparSrt, ipar, siteCode, pointCode, domes, solID,   &
                      technique, serNumber, firmware, refSys, startTime,       &
                      endTime, meanTime, refTime, antNum, xyz, ell, staFlg,    &
                      isat, cosparID, satAntTyp, freqCode, antOffset,elDepMod, &
                      pcvTyp,pcvMod)

      IF (ista <= 0) CYCLE

      WRITE(lfnres, '(1X,A4, 1X,A2, 1X,A4, 1X,A1, 1X,A12, 1X,A12,      &
            &         1X,A20, 1X,A5, 1X,A11)' )                     &
            siteCode, pointCode, solID, technique, startTime, endTime, &
            neq%misc%sinex(ista)%antrec(1:20), serNumber, firmware

    END DO
    WRITE(lfnres,'(A)') '-SITE/RECEIVER'

  END IF

! Site/Antenna Block (only for GNSS and DORIS)
! --------------------------------------------
  IF ( techFlg(1)==1 .OR. techFlg(5)==1 ) THEN

    WRITE(lfnres,'("*",79("-"))')
    WRITE(lfnres,'(A)') '+SITE/ANTENNA'
    WRITE(lfnres,'(A)') &
    '*SITE PT SOLN T DATA_START__ DATA_END____ DESCRIPTION_________ S/N__'

    DO iparSrt = 1, neq%misc%npar
      ipar=sortPar(iparSrt)

      ista = sinstati(neq, iparSrt, ipar, siteCode, pointCode, domes, solID,   &
                      technique, serNumber, firmware, refSys, startTime,       &
                      endTime, meanTime, refTime, antNum, xyz, ell, staFlg,    &
                      isat, cosparID, satAntTyp, freqCode, antOffset,elDepMod, &
                      pcvTyp,pcvMod)

      IF (ista <= 0) CYCLE

      individ = 0
      DO iSys = 0,maxsys-1
        IF (LEN_TRIM(neq%misc%sinex(ista)%antpcv(iSys)%atxStr) > 0 .AND. &
            neq%misc%sinex(ista)%antpcv(iSys)%individ == 1) THEN
          individ = 1
          EXIT
        ENDIF
      ENDDO

      IF(individ == 0) THEN
        antnum = '-----'
      ELSEIF(opt%stacrux /= ' ' .AND. &
             neq%misc%sinex(ista)%antnum /= undef_i) THEN
        CALL sinstore_getsn(stCrx,neq,ipar,ista,antnum)
      ENDIF

      WRITE(lfnres, '(1X,A4, 1X,A2, 1X,A4, 1X,A1, 1X,A12, 1X,A12,      &
            &         1X,A20, 1X,A5)' )                                &
            siteCode, pointCode, solID, technique, startTime, endTime, &
            neq%misc%sinex(ista)%antsta(1:20), antNum

    END DO
    WRITE(lfnres,'(A)') '-SITE/ANTENNA'

  END IF

! Site/GPS_Phase_Center Block (only for GNSS and DORIS)
! -----------------------------------------------------
  IF ( techFlg(1)==1 .OR. techFlg(5)==1 ) THEN

    WRITE(lfnres,'("*",79("-"))')
    WRITE(lfnres,'(A)') '+SITE/GPS_PHASE_CENTER'
    WRITE(lfnres,'(A,/,A)') &
    '*                           UP____ NORTH_ EAST__ UP____ NORTH_ EAST__', &
    '*DESCRIPTION_________ S/N__ L1->ARP(M)__________ L2->ARP(M)__________'
! !
    nlist=0
    DO iparSrt = 1, neq%misc%npar
      ipar=sortPar(iparSrt)

      ista = sinstati(neq, iparSrt, ipar, siteCode, pointCode, domes, solID,   &
                      technique, serNumber, firmware, refSys, startTime,       &
                      endTime, meanTime, refTime, antNum, xyz, ell, staFlg,    &
                      isat, cosparID, satAntTyp, freqCode, antOffset,elDepMod, &
                      pcvTyp,pcvMod)

      IF (ista <= 0) CYCLE

      IF ( neq%misc%sinex(iSta)%antpcv(0)%individ == 0 ) THEN
        antNum = '-----'
      ELSEIF(opt%stacrux /= ' ' .AND. &
             neq%misc%sinex(ista)%antnum /= undef_i) THEN
        CALL sinstore_getsn(stCrx,neq,ipar,ista,antnum)
      ENDIF

! ! Replace RADOME code with 'NONE' if not calibrated
      IF (neq%misc%sinex(ista)%antpcv(0)%adopted == 1 .AND. opt%antred == 1) THEN
        WRITE(antchr,"(A16,'NONE',1X,A5)") neq%misc%sinex(ista)%antsta,antNum
      ELSE
        WRITE(antchr,"(A20,1X,A5)") neq%misc%sinex(ista)%antsta,antNum
      ENDIF
      ipos=listc1(1,26,neq%misc%nstat_sinex,antlst,antchr,nlist)
      istant(ipos)=ista
    ENDDO

    CALL CORDUP(antlst,nlist,1,26,indx)

    DO ipar=1,nlist
      ista=istant(indx(ipar))
      WRITE(lfnres,'(1X, A26, 6(1X,F6.4), 1X,A10)')&
            antlst(indx(ipar)),                    &
            neq%misc%sinex(ista)%antpcv(0)%antphs(3,1),      &
            neq%misc%sinex(ista)%antpcv(0)%antphs(1,1),      &
            neq%misc%sinex(ista)%antpcv(0)%antphs(2,1),      &
            neq%misc%sinex(ista)%antpcv(0)%antphs(3,2),      &
            neq%misc%sinex(ista)%antpcv(0)%antphs(1,2),      &
            neq%misc%sinex(ista)%antpcv(0)%antphs(2,2),      &
            neq%misc%sinex(ista)%antpcv(0)%atxStr
    END DO
    WRITE(lfnres,'(A)') '-SITE/GPS_PHASE_CENTER'

  END IF

! Site/Eccentricity Block
! -----------------------
  WRITE(lfnres,'("*",79("-"))')
  WRITE(lfnres,'(A)') '+SITE/ECCENTRICITY'
  WRITE(lfnres,'(A,/,A)') &
  '*                                             UP______ NORTH___ EAST____', &
  '*SITE PT SOLN T DATA_START__ DATA_END____ AXE ARP->BENCHMARK(M)_________'

  DO iparSrt = 1, neq%misc%npar
    ipar=sortPar(iparSrt)

    ista = sinstati(neq, iparSrt, ipar, siteCode, pointCode, domes, solID,   &
                    technique, serNumber, firmware, refSys, startTime,       &
                    endTime, meanTime, refTime, antNum, xyz, ell, staFlg,    &
                    isat, cosparID, satAntTyp, freqCode, antOffset,elDepMod, &
                    pcvTyp,pcvMod)

    IF (ista <= 0) CYCLE

    WRITE(lfnres, '(1X,A4, 1X,A2, 1X,A4, 1X,A1, 1X,A12, 1X,A12,          &
          &         1X,A3, 3(1X,F8.4))' )                                &
          siteCode, pointCode, solID, technique, startTime, endTime,     &
          refSys, neq%misc%sinex(ista)%antecc(3),                        &
          neq%misc%sinex(ista)%antecc(1), neq%misc%sinex(ista)%antecc(2)
  END DO

  WRITE(lfnres,'(A)') '-SITE/ECCENTRICITY'

! Satellite/ID Block
! ------------------
  IF (sataFlg == 1) THEN
    WRITE(lfnres,'("*",79("-"))')
    WRITE(lfnres,'(A)') '+SATELLITE/ID'
    WRITE(lfnres,'(A)') &
    '*SITE PR COSPAR___ T DATA_START__ DATA_END____ ANTENNA_____________'

    DO iparSrt = 1, neq%misc%npar
      ipar=sortPar(iparSrt)

      ista = sinstati(neq, iparSrt, ipar, siteCode, pointCode, domes, solID, &
                    technique, serNumber, firmware, refSys, startTime,       &
                    endTime, meanTime, refTime, antNum, xyz, ell, staFlg,    &
                    isat, cosparID, satAntTyp, freqCode, antOffset,elDepMod, &
                    pcvTyp,pcvMod)

      IF (isat <= 0) CYCLE
      WRITE(lfnres, '(1X,A4, 1X,A2, 1X,A9, 1X,A1, 1X,A12, 1X,A12, 1X,A20)')  &
            siteCode, domes, cosparID, technique,   &
            startTime, endTime, satAntTyp
    END DO

    WRITE(lfnres,'(A)') '-SATELLITE/ID'
  ENDIF

! Satellite/Phase_Center Block
! ----------------------------
  IF (sataFlg == 1) THEN
    WRITE(lfnres,'("*",79("-"))')
    WRITE(lfnres,'(A)') '+SATELLITE/PHASE_CENTER'
    WRITE(lfnres,'(A)') &
    '*SITE L SATA_Z SATA_X SATA_Y L SATA_Z SATA_X SATA_Y MODEL_____ T M'

    DO iparSrt = 1, neq%misc%npar
      ipar=sortPar(iparSrt)

      ista = sinstati(neq, iparSrt, ipar, siteCode, pointCode, domes, solID, &
                    technique, serNumber, firmware, refSys, startTime,       &
                    endTime, meanTime, refTime, antNum, xyz, ell, staFlg,    &
                    isat, cosparID, satAntTyp, freqCode, antOffset,elDepMod, &
                    pcvTyp,pcvMod)

      IF (isat <= 0) CYCLE

      WRITE(lfnres, '(1X,A4, 2(1X,I1, 3(1X,F6.4)), 1X,A10, 1X,1A, 1X,1A)' ) &
            siteCode, (freqCode(ii), antOffset(3,ii),                       &
            antOffset(1:2,ii), ii=1,2), elDepMod, pcvTyp, pcvMod
    END DO

    WRITE(lfnres,'(A)') '-SATELLITE/PHASE_CENTER'
  ENDIF

! Solution/Epochs Block
! --------------------
  WRITE(lfnres,'("*",79("-"))')
  WRITE(lfnres,'(A)') '+SOLUTION/EPOCHS'
  WRITE(lfnres,'(A)') &
  '*CODE PT SOLN T _DATA_START_ __DATA_END__ _MEAN_EPOCH_'

  DO iparSrt = 1, neq%misc%npar
    ipar=sortPar(iparSrt)

    ista = sinstati(neq, iparSrt, ipar, siteCode, pointCode, domes, solID,   &
                    technique, serNumber, firmware, refSys, startTime,       &
                    endTime, meanTime, refTime, antNum, xyz, ell, staFlg,    &
                    isat, cosparID, satAntTyp, freqCode, antOffset,elDepMod, &
                    pcvTyp,pcvMod)

    IF (ista <= 0) CYCLE

    WRITE(lfnres, '(1X,A4, 1X,A2, 1X,A4, 1X,A1, 3(1X,A12))' )    &
          siteCode, pointCode, solID, technique, startTime,      &
          endTime, meanTime
  END DO

  WRITE(lfnres,'(A)') '-SOLUTION/EPOCHS'


! Bias/Epochs Block (only for SLR/LLR)
! ------------------------------------
  IF ( techFlg(3)==1 .OR. techFlg(4)==1 ) THEN

    WRITE(lfnres,'("*",79("-"))')
    WRITE(lfnres,'(A)') '+BIAS/EPOCHS'
    WRITE(lfnres,'(A)') &
    '*CODE PT SOLN T _DATA_START_ __DATA_END__ _MEAN_EPOCH_'

    DO iparSrt = 1, neq%misc%npar
      ipar=sortPar(iparSrt)

      IF ( neq%par(ipar)%locq(1) /= 26 ) CYCLE

      ista = sinstati(neq, iparSrt, ipar, siteCode, pointCode, domes, solID,   &
                      technique, serNumber, firmware, refSys, startTime,       &
                      endTime, meanTime, refTime, antNum, xyz, ell, staFlg,    &
                      isat, cosparID, satAntTyp, freqCode, antOffset,elDepMod, &
                      pcvTyp,pcvMod)

      technique = 'R'

      WRITE(lfnres, '(1X,A4, 1X,A2, 1X,A4, 1X,A1, 3(1X,A12))' )  &
            siteCode, pointCode, solID, technique, startTime,    &
            endTime, meanTime

    END DO

    WRITE(lfnres,'(A)') '-BIAS/EPOCHS'

  END IF


! Solution/Normal_Equation_Vector
! -------------------------------
  IF (opt%sincont ==1) THEN
    WRITE(lfnres,'("*",79("-"))')
    WRITE(lfnres,'(A)') '+SOLUTION/NORMAL_EQUATION_VECTOR'
    WRITE(lfnres,'(A)') &
         '*INDEX TYPE__ CODE PT SOLN _REF_EPOCH__ UNIT S __RIGHT_HAND_SIDE____'

    numPolPar(:,:) = 0
    DO iparSrt = 1, neq%misc%npar

      ipar = sortPar(iparSrt)

      CALL sinpartu(neq%par(ipar), parType, unit)

      ista = sinstati(neq, iparSrt, ipar, siteCode, pointCode, domes, solID,   &
           technique, serNumber, firmware, refSys, startTime,       &
           endTime, meanTime, refTime, antNum, xyz, ell, staFlg,    &
           isat, cosparID, satAntTyp, freqCode, antOffset,elDepMod, &
           pcvTyp,pcvMod)

      ista = ABS(ista)

      IF ( ista == 0                                                     .AND. &
           (neq%par(ipar)%locq(1) == 1 .OR. neq%par(ipar)%locq(1) == 6) ) THEN
        WRITE(lfnerr,*) ' *** Sinstore: Station not found in NEQ_VECTOR: ', &
                          neq%par(ipar)%name
        CALL exitrc(2)
      END IF

! SolutionID for EOP and troposphere
      IF ( neq%par(ipar)%locq(1) == 10 ) THEN
        icrd = neq%par(ipar)%locq(4)
        ider = neq%par(ipar)%locq(5)
        numPolPar(icrd,ider) = numPolPar(icrd,ider) + 1
        WRITE(solID,'(i4)') numPolPar(icrd,ider)
      END IF

      IF ( neq%par(ipar)%locq(1) == 6 ) THEN
        WRITE(solID,'(i4)') neq%par(ipar)%locq(6)
      END IF

! Set coordinate reference Time if constant coordinates
      IF ( .NOT. velFlg .AND. neq%par(ipar)%locq(1) == 1 ) &
         CALL sindat(0, opt%timRefCrd, refTime)

! Alternated sign for LOD/LODR bNor
      value = bNor_free(ipar)
      IF (neq%par(ipar)%locq(1) == 10 .AND. &
          neq%par(ipar)%locq(4) ==  3 .AND. &
          neq%par(ipar)%locq(5) ==  2) value = -value

      WRITE(lfnres, '(1X,I5, 1X,A6, 1X,A4, 1X,A2, 1X,A4, 1X,A12, 1X,A4, 1X,A1, &
           &         1X,E21.15, 1X,E11.6)')                                   &
           iparSrt, parType, siteCode, pointCode, solID, refTime, unit,       &
           flgConPar(ipar), value

    END DO
    WRITE(lfnres,'(A)') '-SOLUTION/NORMAL_EQUATION_VECTOR'
  END IF

! Solution/Estimate Block
! -----------------------
!  IF (opt%sincont == 2) THEN
    WRITE(lfnres,'("*",79("-"))')
    WRITE(lfnres,'(A)') '+SOLUTION/ESTIMATE'
    WRITE(lfnres,'(A)') &
         '*INDEX TYPE__ CODE PT SOLN _REF_EPOCH__ UNIT S __ESTIMATED '//&
                                                         'VALUE____ _STD_DEV___'

    numPolPar(:,:) = 0
    DO iparSrt = 1, neq%misc%npar

      ipar = sortPar(iparSrt)

      CALL sinpartu(neq%par(ipar), parType, unit)

      ista = sinstati(neq, iparSrt, ipar, siteCode, pointCode, domes, solID,   &
           technique, serNumber, firmware, refSys, startTime,       &
           endTime, meanTime, refTime, antNum, xyz, ell, staFlg,    &
           isat, cosparID, satAntTyp, freqCode, antOffset,elDepMod, &
           pcvTyp,pcvMod)

      ista = ABS(ista)

      IF ( ista == 0                                                     .AND. &
           (neq%par(ipar)%locq(1) == 1 .OR. neq%par(ipar)%locq(1) == 6) ) THEN
        WRITE(lfnerr,*) ' *** Sinstore: Station not found in ESTIMATE: ', &
                          neq%par(ipar)%name
        CALL exitrc(2)
      END IF

      IF ( neq%par(ipar)%locq(1) == 10 .AND. neq%par(ipar)%locq(4) == 3 ) THEN
        estimate = neq%par(ipar)%x0 + neq%xxx(ipar) + ut1_ut1r(neq%par(ipar))
        IF ( neq%par(ipar)%locq(5) == 2 ) THEN
          estimate = -estimate
! Test leap second
! ----------------
          IF      (estimate >  500.d0) THEN
            estimate = estimate - 1000.d0
          ELSE IF (estimate < -500.d0) THEN
            estimate = estimate + 1000.d0
          END IF
        END IF
      ELSE
        estimate = neq%par(ipar)%x0 + neq%xxx(ipar)
      END IF

! SolutionID for EOP and troposphere
      IF ( neq%par(ipar)%locq(1) == 10 ) THEN
        icrd = neq%par(ipar)%locq(4)
        ider = neq%par(ipar)%locq(5)
        numPolPar(icrd,ider) = numPolPar(icrd,ider) + 1
        WRITE(solID,'(i4)') numPolPar(icrd,ider)
      END IF

      IF ( neq%par(ipar)%locq(1) == 6 ) THEN
        WRITE(solID,'(i4)') neq%par(ipar)%locq(6)
      END IF

! Set coordinate reference Time if constant coordinates
      IF ( .NOT. velFlg .AND. neq%par(ipar)%locq(1) == 1 ) &
         CALL sindat(0, opt%timRefCrd, refTime)

      WRITE(lfnres, '(1X,I5, 1X,A6, 1X,A4, 1X,A2, 1X,A4, 1X,A12, 1X,A4, 1X,A1, &
           &         1X,E21.15, 1X,E11.6)')                                   &
           iparSrt, parType, siteCode, pointCode, solID, refTime, unit,       &
           flgConPar(ipar), estimate,                                         &
           SQRT(ABS( wfact * neq%aNor(ikf(ipar,ipar))))

    END DO
    WRITE(lfnres,'(A)') '-SOLUTION/ESTIMATE'
!  END IF

! Solution/Apriori Block
! ----------------------
  WRITE(lfnres,'("*",79("-"))')
  WRITE(lfnres,'(A)') '+SOLUTION/APRIORI'
  WRITE(lfnres,'(A)') &
  '*INDEX TYPE__ CODE PT SOLN _REF_EPOCH__ UNIT S __APRIORI VALUE______ '//&
                                                                 '_STD_DEV___'

  numPolPar(:,:) = 0
  DO iparSrt = 1, neq%misc%npar

    ipar = sortPar(iparSrt)

    CALL sinpartu(neq%par(ipar), parType, unit)

    ista = sinstati(neq, iparSrt, ipar, siteCode, pointCode, domes, solID,   &
                    technique, serNumber, firmware, refSys, startTime,       &
                    endTime, meanTime, refTime, antNum, xyz, ell, staFlg,    &
                    isat, cosparID, satAntTyp, freqCode, antOffset,elDepMod, &
                    pcvTyp,pcvMod)

    ista = ABS(ista)

    IF ( ista == 0                                                      .AND. &
         (neq%par(ipar)%locq(1) == 1 .OR. neq%par(ipar)%locq(1) == 6) ) THEN
      WRITE(lfnerr,*) ' *** Sinstore: Station not found in APRIORI: ', &
                        neq%par(ipar)%name
      CALL exitrc(2)
    END IF

    IF ( neq%par(ipar)%locq(1) == 10 .AND. neq%par(ipar)%locq(4) == 3 ) THEN
      apriori = neq%par(ipar)%x0 + ut1_ut1r(neq%par(ipar))
      IF ( neq%par(ipar)%locq(5) == 2 ) THEN
        apriori = -apriori
        ! Test leap second
        ! ----------------
        IF      (apriori >  500.d0) THEN
          apriori = apriori - 1000.d0
        ELSE IF (apriori < -500.d0) THEN
          apriori = apriori + 1000.d0
        END IF
      END IF
    ELSE
      apriori = neq%par(ipar)%x0
    END IF

! SolutionID for EOP and troposphere
    IF ( neq%par(ipar)%locq(1) == 10 ) THEN
      icrd = neq%par(ipar)%locq(4)
      ider = neq%par(ipar)%locq(5)
      numPolPar(icrd,ider) = numPolPar(icrd,ider) + 1
      WRITE(solID,'(i4)') numPolPar(icrd,ider)
    END IF

    IF ( neq%par(ipar)%locq(1) == 6 ) THEN
      WRITE(solID,'(i4)') neq%par(ipar)%locq(6)
    END IF

    IF ( neq%par(ipar)%locq(1) == 16 ) THEN
      WRITE(lfnerr,"(/,' ### SR SINSTORE: Writing geocenter coordinates', &
                    &  ' to SINEX file',/)")
    ENDIF

! No apriori sigma for coordinates if Minimum Constraint solution
    IF ( freeCrd == 1 .AND. neq%par(ipar)%locq(1) == 1 ) THEN
      sigApr = 0d0
    ELSE
      sigApr = SQRT(ABS(wfact_apr * regMat(ikf(ipar,ipar))))
    END IF

! Set coordinate reference Time if constant coordinates
      IF ( .NOT. velFlg .AND. neq%par(ipar)%locq(1) == 1 ) &
         CALL sindat(0, opt%timRefCrd, refTime)

    WRITE(lfnres, '(1X,I5, 1X,A6, 1X,A4, 1X,A2, 1X,A4, 1X,A12, 1X,A4, 1X,A1, &
          &         1X,E21.15, 1X,E11.6)')                                   &
          iparSrt, parType, siteCode, pointCode, solID, refTime, unit,       &
          flgConPar(ipar), apriori, sigApr

  END DO
  WRITE(lfnres,'(A)') '-SOLUTION/APRIORI'

! Solution/Normal_Equation_Matrix
! -------------------------------
  IF (opt%sincont == 1) THEN
    WRITE(lfnres,'("*",79("-"))')
    WRITE(lfnres,'(A)') '+SOLUTION/NORMAL_EQUATION_MATRIX L'
    WRITE(lfnres,'(A)') &
         '*PARA1 PARA2 ____PARA2+0__________ ____PARA2+1__________ '//&
                                                        '____PARA2+2__________'

    nWrite = 0
    line   = ''
    DO iparSrt1 = 1, neq%misc%npar
      ip1 = sortPar(iparSrt1)

      DO iparSrt2 = 1, iparSrt1
        ip2 = sortPar(iparSrt2)

        IF ( nWrite == 0 ) THEN
          WRITE(line(1:34), '(2(1X,I5), 1X,E21.14)') &
               iparSrt1, iparSrt2, aNor_free(ikf(ip1,ip2))
        ELSE IF ( nWrite == 1 ) THEN
          WRITE(line(35:56), '(1X,E21.14)') aNor_free(ikf(ip1,ip2))
        ELSE
          WRITE(line(57:78), '(1X,E21.14)') aNor_free(ikf(ip1,ip2))
        END IF

        nWrite = nWrite + 1

        IF (nWrite == 3 .OR. iparSrt1 == iparSrt2) THEN
          WRITE(lfnres,'(A)') line(1:LEN_TRIM(line))
          nWrite = 0
          line =  ''
        END IF

      END DO
    END DO
    WRITE(lfnres,'(A)') '-SOLUTION/NORMAL_EQUATION_MATRIX L'
  END IF

! Solution/Matrix_Estimate Block
! ------------------------------
  IF (opt%sincont == 2) THEN
    WRITE(lfnres,'("*",79("-"))')
    WRITE(lfnres,'(A)') '+SOLUTION/MATRIX_ESTIMATE L COVA'
    WRITE(lfnres,'(A)') &
         '*PARA1 PARA2 ____PARA2+0__________ ____PARA2+1__________ '//&
                                                       '____PARA2+2__________'

    nWrite = 0
    line   = ''
    DO iparSrt1 = 1, neq%misc%npar
      ip1 = sortPar(iparSrt1)

      DO iparSrt2 = 1, iparSrt1
        ip2 = sortPar(iparSrt2)

        IF ( nWrite == 0 ) THEN
          WRITE(line(1:34), '(2(1X,I5), 1X,E21.14)') &
               iparSrt1, iparSrt2, wfact * neq%aNor(ikf(ip1,ip2))
        ELSE IF ( nWrite == 1 ) THEN
          WRITE(line(35:56), '(1X,E21.14)') wfact * neq%aNor(ikf(ip1,ip2))
        ELSE
          WRITE(line(57:78), '(1X,E21.14)') wfact * neq%aNor(ikf(ip1,ip2))
        END IF

        nWrite = nWrite + 1

        IF (nWrite == 3 .OR. iparSrt1 == iparSrt2) THEN
          WRITE(lfnres,'(A)') line(1:LEN_TRIM(line))
          nWrite = 0
          line =  ''
        END IF

      END DO
    END DO
    WRITE(lfnres,'(A)') '-SOLUTION/MATRIX_ESTIMATE L COVA'
  END IF

! Solution/Matrix_Apriori Block
! ------------------------------
  IF (opt%sincont == 2) THEN
    WRITE(lfnres,'("*",79("-"))')
    WRITE(lfnres,'(A)') '+SOLUTION/MATRIX_APRIORI L COVA'
    WRITE(lfnres,'(A)') &
         '*PARA1 PARA2 ____PARA2+0__________ ____PARA2+1__________ '//&
                                                        '____PARA2+2__________'

    nWrite   = 0
    line     = ''
    writeFlg = .FALSE.
    DO iparSrt1 = 1, neq%misc%npar
      ip1 = sortPar(iparSrt1)

      DO iparSrt2 = 1, iparSrt1
        ip2 = sortPar(iparSrt2)

        IF ( regMat(ikf(ip1,ip2)) /= 0.d0 ) writeFlg = .TRUE.

        IF ( nWrite == 0 ) THEN
          WRITE(line(1:34), '(2(1X,I5), 1X,E21.14)') &
               iparSrt1, iparSrt2, wfact * regMat(ikf(ip1,ip2))
        ELSE IF ( nWrite == 1 ) THEN
          WRITE(line(35:56), '(1X,E21.14)') wfact * regMat(ikf(ip1,ip2))
        ELSE
          WRITE(line(57:78), '(1X,E21.14)') wfact * regMat(ikf(ip1,ip2))
        END IF

        nWrite = nWrite + 1

        IF (nWrite == 3 .OR. iparSrt1 == iparSrt2) THEN
          IF ( writeFlg ) WRITE(lfnres,'(A)') line(1:LEN_TRIM(line))
          nWrite   = 0
          line     =  ''
          writeFlg = .FALSE.
        END IF

      END DO
    END DO
    WRITE(lfnres,'(A)') '-SOLUTION/MATRIX_APRIORI L COVA'
  END IF

! End of File
! -----------
  WRITE(lfnres,'(A)') '%ENDSNX'

  CLOSE(lfnres)

END SUBROUTINE sinstore

! -------------------------------------------------------------------------

SUBROUTINE sinstore_getsn(stCrx,neq,ipar,ista,antnum)

! -------------------------------------------------------------------------
!
! Changes:    __-___-____ __:
!
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, staNameLength
  USE d_stacrx, ONLY: t_stacrux
  USE d_neq,    ONLY: t_neq

  TYPE(t_stacrux)         :: stCrx
  TYPE(t_neq)             :: neq
  INTEGER(i4b)            :: ipar
  INTEGER(i4b)            :: ista
  CHARACTER(LEN=5)        :: antNum

  INTEGER(i4b)            :: ii, i1, i2

  DO ii = 1,stCrx%ninfo
    IF(neq%par(ipar)%name(1:staNameLength) == stCrx%stainfo(ii)%stanam .AND. &
       neq%misc%sinex(ista)%antsta(1:20) == stCrx%stainfo(ii)%antnam .AND. &
       neq%misc%sinex(ista)%antnum == stCrx%stainfo(ii)%antnum) THEN
      IF(stCrx%stainfo(ii)%antser == ' ' .OR. &
         INDEX(stCrx%stainfo(ii)%antser,'*') /= 0 .OR. &
         INDEX(stCrx%stainfo(ii)%antser,'?') /= 0 .OR. &
         INDEX(stCrx%stainfo(ii)%antser,'%') /= 0) EXIT
      i2 = LEN_TRIM(stCrx%stainfo(ii)%antser)
      i1 = i2-4
      IF(i1<1) THEN
        i1 = 1
        i2 = 5
      ENDIF
      antnum = stCrx%stainfo(ii)%antser(i1:i2)
    ENDIF
  ENDDO

END SUBROUTINE sinstore_getsn

END MODULE

