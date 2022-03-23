MODULE s_CDINPT
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE cdinpt(stitle, priopt, usemrk, nsampl, secIpl, zenmax, &
                  obswin, itropo, iono  , icorfl, ioutlr, difmax, confid, &
                  mindof, usrsig, niter ,iorbfl, crxfil, clkhed, kinest,  &
                  irCode)

! -------------------------------------------------------------------------
! Purpose:    This is a new version of the old subroutine CDINPT.f that
!             reads the input options of the program CODSPP
!
! Author:     L. Mervart
!
! Created:    31-May-2000
!
! Changes:    08-JAN-2001 RD: Error Handling
!             10-May-2001 RD: Write clock rinex file
!             21-Dec-2001 HU: Use d_const
!             28-Aug-2002 DS: Flag for estimation of KIN coordinates
!             16-Dec-2002 RD: Use mark flag from observ. file is an option
!             23-Apr-2003 CU: Nullify local pointers
!             19-May-2003 RD: Use SR gttimwin instead of SR readsess
!             22-Aug-2003 AJ: Screening options for KIN coordinates
!             05-Feb-2004 HU: Coordinate estimation: combobox
!             23-Mar-2004 HU: Do not test upper bound for nsampl
!             02-Oct-2006 AG: NIELL and GMF implemented
!             24-Nov-2006 AG: TIMSYS and DCBLINE implemented
!             01-Nov-2007 HB: SECIPL implemented
!             30-Jun-2008 RD: VMF added
!             14-Jan-2011 RD: Tropo file required for "ESTIMATED"
!             18-Jan-2011 SL: use m_bern with ONLY
!             06-May-2011 HB: Add rdstdh to initialize model names
!             18-Jun-2012 RD: SECIPL may be empty (no interpolation)
!             18-Jun-2012 RD: Remove unused parameters
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, keyValueLength, fileNameLength, lfnErr
  USE d_const,  ONLY: pi
  USE d_clkrnx, ONLY: t_clkhead
  USE d_stdorb, ONLY: t_stdhead, init_stdHead
  USE s_ckoptl
  USE s_alcerr
  USE s_gttimwin
  USE s_readkeys
  USE s_gtflna
  USE s_rdstdh
  IMPLICIT NONE


! List of Parameters
! ------------------
  CHARACTER(LEN=*)           :: stitle ! short title
  INTEGER(i4b), DIMENSION(*) :: priopt ! priopt(1) == 1: print sat. elevations
                                       ! priopt(2) == 1: print residuals
  INTEGER(i4b)               :: usemrk ! Use mark flags from observation files
  INTEGER(i4b)               :: nsampl ! sampling rate
  REAL(r8b)                  :: secIpl ! maximum interval for clock interpolation  (sec)
  REAL(r8b)                  :: zenmax ! maximum zenith distance (rad)
  REAL(r8b)   , DIMENSION(2) :: obswin ! observation window (mjd)
  INTEGER(i4b)               :: itropo ! tropospheric model
  INTEGER(i4b)               :: iono   ! ionospheric model
  INTEGER(i4b)               :: icorfl ! coord. estimation (yes=1, no=0)
  INTEGER(i4b)               :: ioutlr ! outlier detection (yes=1, no=0)
  REAL(r8b)                  :: difmax ! maximum residual difference
                                       ! allowed to "best" satellite
  REAL(r8b)                  :: confid ! confidence interval (number of
                                       ! sigmas, e.g. 3 for 3-sigma limit)
  INTEGER(i4b)               :: mindof ! minimal degree of freedom allowed
  REAL(r8b)                  :: usrsig ! maximal RMS of epoch solution
  INTEGER(i4b)               :: niter  ! maximum number of iterations
  INTEGER(i4b)               :: iorbfl ! Orbit type 1: Broadcast, 2: Standard
  CHARACTER(LEN=*)           :: crxfil ! name of clock rinex file
  TYPE(t_clkhead)            :: clkhed ! header of clock rinex
  INTEGER(i4b)               :: kinest ! Estimate epoch-wise KIN coordinates
                                       !  =0 NO
                                       !  =1 YES
  INTEGER(i4b)               :: irCode ! return code (0 = O.K.)

! Local Variables
! ---------------
  TYPE(t_stdhead)        :: stdHead,stdLHead   ! Structure of std header info
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER :: keyValue
  CHARACTER(LEN=fileNameLength)                        :: filnam
  INTEGER(i4b)                                         :: leoProc
  INTEGER(i4b)                                         :: icom
  INTEGER(i4b)                                         :: iOrb
  INTEGER(i4b)                                         :: irc
  INTEGER(i4b)                                         :: ios

  NULLIFY(keyValue)

  CALL init_stdHead(stdHead)

! Read all Options
! ----------------

  CALL readkeys('TITLE', keyValue, irc)
  irCode=irCode+irc
  stitle=keyValue(1)

  CALL readkeys('PRTELE', keyValue, irc)
  READ(keyValue(1),*,iostat=ios) priopt(1)
  IF (priopt(1) < 0 .OR. priopt(1) > 1) ios=1
  irCode=irCode+irc+ios
  IF (ios /= 0)                                       &
    write(lfnerr,'(A)')                               &
          ' *** SR CDINPT : Wrong entry for keyword "PRTELE" in input file'

  CALL readkeys('PRTRES', keyValue, irc)
  READ(keyValue(1),*,iostat=ios) priopt(2)
  IF (priopt(2) < 0 .OR. priopt(2) > 1) ios=1
  irCode=irCode+irc+ios
  IF (ios /= 0)                                       &
    write(lfnerr,'(A)')                               &
          ' *** SR CDINPT : Wrong entry for keyword "PRTRES" in input file'

! Use mark flags from observation files
! -------------------------------------
  CALL readkeys('USEMRK' , keyValue, irc)
  READ(keyValue(1),*,iostat=ios) usemrk
  IF (usemrk < 0 .OR. usemrk > 1) ios=1
  irCode=irCode+irc+ios
  IF (ios /= 0)                                       &
    write(lfnerr,'(A)')                               &
          ' *** SR CDINPT : Wrong entry for keyword "USEMRK" in input file'
!
  CALL readkeys('SAMPLE', keyValue, irc)
  READ(keyValue(1),*,iostat=ios) nsampl
  IF (nsampl < 1) ios=1
  irCode=irCode+irc+ios
  IF (ios /= 0)                                       &
    write(lfnerr,'(A)')                               &
          ' *** SR CDINPT : Wrong entry for keyword "SAMPLE" in input file'

  secIpl = 0d0
  CALL readkeys('SECIPL', keyValue, irc)
  IF (LEN_TRIM(keyValue(1)) > 0) THEN
    READ(keyValue(1),*,iostat=ios) secIpl
    IF (secIpl<0.D0) ios=1
    irCode=irCode+irc+ios
    IF (ios /= 0)                                       &
      write(lfnerr,'(A)')                               &
            ' *** SR CDINPT : Wrong entry for keyword "SECIPL" in input file'
  ENDIF

  CALL readkeys('MINEL', keyValue, irc)
  READ(keyValue(1),*,iostat=ios) zenmax
  irCode=irCode+irc+ios
  IF (ios /= 0)                                       &
    write(lfnerr,'(A)')                               &
          ' *** SR CDINPT : Wrong entry for keyword "MINEL" in input file'
  zenmax = (90d0 - zenmax) / 180d0 * PI

! Read Time Options
! -----------------
  CALL gttimwin('WINDOW',(/'RADIO_1','RADIO_2'/),        &
                (/'SESSION_YEAR','SESSION_STRG'/),       &
                (/'STADAT','STATIM','ENDDAT','ENDTIM'/), &
                obswin)


  CALL readkeys('TROPOS', keyValue, irc)
  irCode=irCode+irc
  IF      (irc == 0 .AND. keyValue(1) == 'NONE'         ) THEN
    itropo = 0
  ELSE IF (irc == 0 .AND. keyValue(1) == 'SAASTAMOINEN' ) THEN
    itropo = 1
  ELSE IF (irc == 0 .AND. keyValue(1) == 'HOPFIELD'     ) THEN
    itropo = 2
  ELSE IF (irc == 0 .AND. keyValue(1) == 'MARINI-MUR'   ) THEN
    itropo = 4
  ELSE IF (irc == 0 .AND. keyValue(1) == 'NIELL'        ) THEN
    itropo = 5
  ELSE IF (irc == 0 .AND. keyValue(1) == 'GMF'          ) THEN
    itropo = 6
  ELSE IF (irc == 0 .AND. keyValue(1) == 'VMF'          ) THEN
    itropo = 7
  ELSE IF (irc == 0 .AND. keyValue(1) == 'DRY_SAAST'    ) THEN
    itropo = 11
  ELSE IF (irc == 0 .AND. keyValue(1) == 'DRY_HOPFIELD' ) THEN
    itropo = 12
  ELSE IF (irc == 0 .AND. keyValue(1) == 'DRY_NIELL'    ) THEN
    itropo = 15
  ELSE IF (irc == 0 .AND. keyValue(1) == 'DRY_GMF'      ) THEN
    itropo = 16
  ELSE IF (irc == 0 .AND. keyValue(1) == 'DRY_VMF'      ) THEN
    itropo = 17
  ELSE IF (irc == 0 .AND. keyValue(1) == 'ESTIMATED'    ) THEN
    itropo = 20
    CALL gtflna(1,'TROPEST', filnam, irc)
  ELSE
    WRITE(lfnerr,*) ' *** SR CDINPT: invalid entry for Troposphere Model: ',  &
                      keyValue(1)
    irCode=irCode+1
  END IF

! Ionosphere modelling
! --------------------
  CALL readkeys('IONOS', keyValue, irc)
  READ(keyValue(1),*,iostat=ios) iono
  IF (iono < 0 .OR. iono > 1) ios=1
  irCode=irCode+irc+ios
  IF (ios /= 0)                                       &
    write(lfnerr,'(A)')                               &
          ' *** SR CDINPT : Wrong entry for keyword "IONOS" in input file'

  CALL readkeys('COORDEST', keyValue, irc)
  irCode=irCode+irc
  IF      (keyValue(1) == 'NO'       .OR. keyValue(1) == '0' ) THEN
    icorfl=0
    kinest=0
  ELSE IF (keyValue(1) == 'STATIC'   .OR. keyValue(1) == '1' ) THEN
    icorfl=1
    kinest=0
  ELSE IF (keyValue(1) == 'KINEMATIC' ) THEN
    icorfl=0
    kinest=1
  ELSE
    WRITE(lfnerr,*) ' *** SR CDINPT: invalid entry for coordinate estimation: ',  &
                      keyValue(1)
    irCode=irCode+1
  END IF

  CALL readkeys('CLKPOLY', keyValue, irc)
  irCode=irCode+irc
  IF (kinest==1 .AND. keyValue(1) /= 'E') THEN
    write(lfnerr,'(/,A,/,16X,A,/)') &
         ' *** SR CDINPT: Estimation of kinematic coordinates only allowed ', &
                         'for clock option "E" (one offset per epoch)'
    irCode=irCode+1
  ENDIF
  IF (ios /= 0)                                       &
    write(lfnerr,'(A)')                               &
          ' *** SR CDINPT : Wrong entry for keyword "KINEST" in input file'

  CALL readkeys('OUTDET', keyValue, irc)
  READ(keyValue(1),*,iostat=ios) ioutlr
  IF (ioutlr < 0 .OR. ioutlr > 1) ios=1
  irCode=irCode+irc+ios
  IF (ios /= 0)                                       &
    write(lfnerr,'(A)')                               &
          ' *** SR CDINPT : Wrong entry for keyword "OUTDET" in input file'

  CALL readkeys('RESMAX', keyValue, irc)
  READ(keyValue(1),*,iostat=ios) difmax
  irCode=irCode+irc+ios
  IF (ios /= 0)                                       &
    write(lfnerr,'(A)')                               &
          ' *** SR CDINPT : Wrong entry for keyword "RESMAX" in input file'

  CALL readkeys('CONFIN', keyValue, irc)
  READ(keyValue(1),*,iostat=ios) confid
  irCode=irCode+irc+ios
  IF (ios /= 0)                                       &
    write(lfnerr,'(A)')                               &
          ' *** SR CDINPT : Wrong entry for keyword "CONFIN" in input file'

  CALL readkeys('KINDOF', keyValue, irc)
  READ(keyValue(1),*,iostat=ios) mindof
  IF (mindof < 0) ios=1
  irCode=irCode+irc+ios
  IF (ios /= 0)                                       &
    write(lfnerr,'(A)')                               &
          ' *** SR CDINPT : Wrong entry for keyword "KINDOF" in input file'

  CALL readkeys('KINSIG', keyValue, irc)
  READ(keyValue(1),*,iostat=ios) usrsig
  irCode=irCode+irc+ios
  IF (ios /= 0)                                       &
    write(lfnerr,'(A)')                               &
          ' *** SR CDINPT : Wrong entry for keyword "KINSIG" in input file'

  CALL readkeys('ITRMAX', keyValue, irc)
  READ(keyValue(1),*,iostat=ios) niter
  IF (niter < 1) ios=1
  irCode=irCode+irc+ios
  IF (ios /= 0)                                       &
    write(lfnerr,'(A)')                               &
          ' *** SR CDINPT : Wrong entry for keyword "ITRMAX" in input file'

! Read clock rinex output file
! ----------------------------
  CALL gtflna(0,'CLKRNXRS',crxfil,irc)
  IF (irc /= 0) crxfil=''
  IF (LEN_TRIM(crxfil) > 0) THEN
    CALL Readkeys('RUNBY',keyValue,irc)
    irCode=irCode+irc
    IF (irc==0) clkhed%RunBy = TRIM(keyValue(1))
!
    CALL Readkeys('AC',keyValue,irc)
    irCode=irCode+irc
    IF (irc==0) clkhed%AC = TRIM(keyvalue(1))
!
    CALL Readkeys('ACNAME',keyValue,irc)
    irCode=irCode+irc
    IF (irc==0) clkhed%ACName = TRIM(keyvalue(1))
!
    CALL readkeys('TIMESYS', keyValue, irc)
    IF (irc==0) &
         CALL ckoptl(1,'TIMESYS', keyValue, 'sr wtcrxh', 'Time system', irc,   &
              irCode, maxLength=LEN(ClkHed%timsys), maxval=1, result1=ClkHed%timsys)
!
    CALL readkeys('DCBLINE', keyValue, irc)
    IF (irc==0) &
         CALL ckoptl(0,'DCBLINE', keyValue, 'sr wtcrxh', 'DCB line', irc, irCode, &
            maxLength=LEN(ClkHed%dcbStr), empty=' ', maxval=1, result1=ClkHed%dcbStr)
!
    CALL Readkeys('COMMENT',keyValue,irc)
    irCode=irCode+irc
    IF (irc==0) THEN
      clkhed%nComment   = SIZE(keyvalue)
      ALLOCATE(clkhed%Comment(clkhed%nComment),stat=ios)
      CALL alcerr(ios,'clkhed%Comment',(/clkhed%nComment/),'cclkin')
      DO iCom=1,clkhed%nComment
        clkhed%Comment(iCom) = TRIM(keyValue(iCom))
      END DO
    END IF
  ENDIF

!
!  Check Orbit Type (1:Broadcast,2:Std. Orbit)
!  -------------------------------------------
  iOrbFl=0
  CALL readKeys('RADIO_B',keyValue,irc)
  READ(keyValue(1),*,iostat=ios) iOrb
  IF (iOrb < 0 .OR. iOrb > 1) ios=1
  irCode=irCode+irc+ios
  IF (ios /= 0)                                       &
    write(lfnerr,'(A)')                               &
          ' *** SR CDINPT : Wrong entry for keyword "RADIO_B" in input file'
  IF (irc+ios == 0 .AND. iOrb == 1) iOrbFl=iOrbFl+1
!
  CALL readKeys('RADIO_S',keyValue,irc)
  READ(keyValue(1),*,iostat=ios) iOrb
  IF (iOrb < 0 .OR. iOrb > 1) ios=1
  irCode=irCode+irc+ios
  IF (ios /= 0)                                       &
    write(lfnerr,'(A)')                               &
          ' *** SR CDINPT : Wrong entry for keyword "RADIO_S" in input file'
  IF (irc+ios == 0 .AND. iOrb == 1) iOrbFl=iOrbFl+2
!
  IF (iOrbFl < 1 .OR. iOrbFl > 2) THEN
    write(lfnerr,'(A)')                               &
          ' *** SR CDINPT : Wrong entry for keyword "RADIO_B" or "RADIO_S" in input file'
    irCode=irCode+1
  ENDIF
!
! Get model information from standard orbit file header
! -----------------------------------------------------
  filNam = ' '
  CALL rdstdh(filNam,stdHead,irc)

! Check if LEO should be processed
! --------------------------------
  CALL readkeys('LEOPROC' , keyValue, irc)
  READ(keyValue(1),*,iostat=ios) leoProc
  IF (leoProc < 0 .OR. leoProc > 1) ios=1
  irCode=irCode+irc+ios
  IF (ios /= 0)                                       &
    write(lfnerr,'(A)')                               &
          ' *** SR CDINPT : Wrong entry for keyword "LEOPROC" in input file'

  IF (leoProc == 1) THEN
    CALL gtflna(0,'LEOSTD', filnam, irc)
    IF (irc == 0) THEN
      CALL init_stdHead(stdLHead)
      CALL rdstdh(filNam,stdLHead,irc)
    ENDIF
  ENDIF

  DEALLOCATE(keyValue,stat=irc)

  IF (irCode /= 0) irCode=2
END SUBROUTINE cdinpt

END MODULE
