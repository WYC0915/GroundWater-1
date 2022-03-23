MODULE s_NEQPRT
CONTAINS

! -------------------------------------------------------------------------
! Bernese GNSS Software Version 5.2
! -------------------------------------------------------------------------

SUBROUTINE neqprt(lfn,noInv,ifil,neq)

! -------------------------------------------------------------------------
! Purpose:    This subroutine prints the results of the program ADDNEQ2
!             Remark: an internal subroutine (oneLine) is defined and
!                     used in this routine!
!
! Author:     M.Meindl
!
! Created:    07-Nov-2002
!
! Changes:    22-May-2003 RD: Use maxsat from m_maxdim (instead of p_addneq)
!             27-May-2003 CU: New SR prparlst: print parameter summary
!             10-Jun-2003 MM: Print singular parameters
!             26-Jun-2003 MM: Bugfix wrt singular parameters
!             14-Aug-2003 HU: Write satellite pcv output
!             09-Dec-2003 SS: Chi instead of CHI
!             22-Dec-2003 RS: Print gnroff instead of grpoff
!             14-Jan-2004 HU: Units um/sec for stochastic pulses
!             22-Jan-2004 SS/MM: SPV replaced by SAP
!             23-Jan-2004 HU: Units nm/s**2 for accelerations
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             22-Sep-2005 RD: Use new modules D_NEQ.f90 and D_PAR.f90
!             12-Dec-2005 CU: Print number of pseudo-observations npseu
!             04-Oct-2006 AG: PRN added for satellite antenna PCO/PCV
!             12-Oct-2006 RD: Format statement corrected
!             18-Jan-2007 AG: time%mean instead of 0D0 for SVN2PRN
!             08-Feb-2007 RD: misc%nObs/nParms i4b->r8b
!             25-Jan-2008 RD: add rao/rap parameters
!             13-Feb-2008 RD: Adapt to par%name == chr*20
!             28-May-2008 DT: Add receiver clocks / Time biases (locq(1)=2)
!             13-Nov-2008 DT: Set dyn. orbit parameters according to orbdsc%orbmod
!             08-Apr-2009 DT: Add Range biases (locq(1)=26)
!             04-May-2009 RD: Scaling of loading models added
!             06-May-2009 RD: Repeatability for all parameters in a PLT-file
!             10-May-2009 RD: Receiver clock offsets/biases implemented
!             10-Jun-2009 RD: Add epoch satellite/receiver clocks
!             20-Aug-2009 LO: Cosmetics adapted for long time series
!             21-Nov-2009 RD: Intersystem biases added
!             04-Jan-2010 SL: HOI scaling parameters added
!             12-Aug-2010 DT: Add technique for SAO
!             08-Sep-2010 RD: Merge SLR-time bias option
!             30-Nov-2010 MM: GNSS-specific parameters
!             30-Nov-2010 DT: Add Helmert parameters
!             03-Feb-2011 SL: print 5 digits instead of 4, use m_bern with ONLY
!             21-Jul-2011 LP: Sat-spec. obstypes
!             05-Mar-2012 RD: Use listi4 as module now
!             12-Mar-2012 HB: Layout unification for stochastic parameter
!             01-May-2012 LP: Replace r32r2 by OBSTYPESR3
!             19-Jun-2012 DT: Consequently use lfn; remove lfnPrt
!             05-Oct-2012 RD: Remove unused variables
!             05-Oct-2012 RD: Print parameter statistics also if no solution
!             16-Jul-2013 RD: Correct index for "grep"-keys for clock parameters
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b,r8b,lfnErr,lfnLoc,linelength
  USE m_time,   ONLY: t_timint
  USE m_global, ONLY: g_svnsys,g_syssvn,g_strsys3
  USE m_maxdim, ONLY: maxsat
  USE d_const,  ONLY: C, ars
  USE d_datum,  ONLY: datum
  USE d_par,    ONLY: maxParTyp, is_techn
  USE d_neq,    ONLY: t_neq
  USE p_addneq, ONLY: opt,comstat,prtExt,clkHed
  USE p_orbgen, ONLY: orbdsc
  USE d_rinex3, ONLY: OBSTYPESR3

  USE f_ikf
  USE s_chksin
  USE s_prparlst
  USE s_svn2prn
  USE f_listi4
  USE s_opnfil
  USE s_opnerr
  USE s_timst2

  IMPLICIT NONE

! List of parameters
! ------------------
  INTEGER(i4b)                       :: lfn     ! lfn for output
  INTEGER(i4b)                       :: noInv   ! 1: solution available
                                                ! 0: no solution
  INTEGER(i4b)                       :: ifil    ! index of neq-file
  TYPE(t_neq)                        :: neq

! Local variables
! ---------------
  TYPE(t_timint)                     :: timint

  CHARACTER(LEN=12), DIMENSION(5)    :: hlpStr

  INTEGER(i4b)                       :: iPar
  INTEGER(i4b)                       :: iPos
  INTEGER(i4b)                       :: iExt
  INTEGER(i4b), DIMENSION(maxParTyp) :: numPar
  INTEGER(i4b), DIMENSION(maxSat)    :: satLst
  INTEGER(i4b)                       :: numSta
  INTEGER(i4b)                       :: numSat
  INTEGER(i4b)                       :: parTyp
  INTEGER(i4b)                       :: ios
  INTEGER(i4b)                       :: ilin
  LOGICAL,DIMENSION(maxParTyp)       :: fstPar

! specials for different parameter types
  INTEGER(i4b)                       :: iSol      ! CRD
  CHARACTER(LEN=3)                   :: crdStrHlp

  INTEGER(i4b)                       :: iOrb      ! ORB
  CHARACTER(LEN=4)                   :: empiri    ! ORB dyn.

  INTEGER(i4b)                       :: frq       ! RAO/RAP
  INTEGER(i4b)                       :: sys       ! RAO/RAP
  INTEGER(i4b)                       :: cor       ! RAO
  INTEGER(i4b)                       :: zen       ! RAP
  INTEGER(i4b)                       :: azi       ! RAP

  INTEGER(i4b)                       :: iGrp      ! DCB

  INTEGER(i4b)                       :: iGcc      ! GCC
  REAL(r8b), DIMENSION(3)            :: gccVal
  REAL(r8b), DIMENSION(3)            :: gccErr
  REAL(r8b), DIMENSION(2)            :: gccMjd

  INTEGER(i4b)                       :: iDrift    ! ERP
  INTEGER(i4b)                       :: iReq
  REAL(r8b)                          :: erpTimHlp
  CHARACTER(LEN=1)                   :: chr1      ! Epoch clocks
  INTEGER(i4b)                       :: iRef
  INTEGER(i4b)                       :: prn
  INTEGER(i4b)                       :: indobst
  CHARACTER(LEN=3)                   :: strprn
  CHARACTER(LEN=4)                   :: svnnr
  CHARACTER(LEN=3)                   :: obst1, obst2

  CHARACTER(LEN=6)                   :: hlp_tech  ! SAO
  CHARACTER(LEN=10)                  :: helpstr

! specials for subroutine oneLine
  REAL(r8b), DIMENSION(2)            :: timMjd
  REAL(r8b)                          :: meanMjd
  REAL(r8b)                          :: dspUnit
  CHARACTER(LEN=40)                  :: timStr
  CHARACTER(LEN=7)                   :: parUnit
  CHARACTER(LEN=5)                   :: parGrep

! format and output line
  CHARACTER(LEN=56)                  :: fmtStr =                          &
            '(2(F10.5,1X,F16.5,1X),A7,2(1X,A19),1X,F11.5,1X,I5,1X,A5)'
  CHARACTER(LEN=48)                  :: sngStr =                          &
            '(A39,F16.5,1X,A7,2(1X,A19),1X,F11.5,1X,I5,1X,A5)'
  CHARACTER(LEN=linelength)          :: pLine

! strings
  CHARACTER(LEN=3), DIMENSION(3)     :: xyzStr = (/'  X', '  Y', '  Z'/)
  CHARACTER(LEN=3), DIMENSION(3)     :: velStr = (/' VX', ' VY', ' VZ'/)
  CHARACTER(LEN=3), DIMENSION(3)     :: neuStr = (/'  N', '  E', '  U'/)

  CHARACTER(LEN=8), DIMENSION(15)    :: eleStr =                          &
           (/'A       ', 'E       ', 'I       ', 'Node    ', 'Perigee ',  &
             'Latitude', 'D 0     ', 'Y 0     ', 'X 0     ', 'D cos   ',  &
             'Y cos   ', 'X cos   ', 'D sin   ', 'Y sin   ', 'X sin   '/)

  CHARACTER(LEN=8), DIMENSION(9)     :: rswStr =                          &
           (/'R 0     ', 'S 0     ', 'W 0     ', 'R cos   ', 'S cos   ',  &
             'W cos   ', 'R sin   ', 'S sin   ', 'W sin   '/)

  CHARACTER(LEN=13), DIMENSION(6)    :: stcStr =                          &
           (/'Radial       ', 'Along-track  ', 'Out-of-plane ',           &
             'Direction Sun', 'Y-direction  ', 'X-direction  '/)

  CHARACTER(LEN=9), DIMENSION(5,2)   :: erpStr =                          &
           reshape(source =                                               &
           (/'X        ', 'Y        ', 'dT       ',                       &
             'dEps     ', 'dPsi     ', 'X rate   ',                       &
             'Y rate   ', 'LOD      ', 'dEps rate',                       &
             'dPsi rate'/), shape = (/5,2/))

  CHARACTER(LEN=3), DIMENSION(3,3)   :: grdStr =                          &
           reshape(source =                                               &
           (/'all', '   ', '   ',                       &
             'up ', 'n/e', '   ',                       &
             'u  ', 'n  ', 'e  '/), shape = (/3,3/))

  CHARACTER(LEN=7), DIMENSION(14)    :: units  =                          &
           (/'meters ', 'msec   ', 'nsec   ', 'asec   ',                  &
             'masec  ', 'msec/d ', 'masec/d', 'm/s**2 ',                  &
             'TECU   ', '1      ', 'm/year ', 'um/sec ',                  &
             'nm/s**2', 'ppb    '/)

  CHARACTER(LEN=5), DIMENSION(22)    :: grepStr =                         &
           (/'#DCB ', '#TRP ', '#SAO ', '#GCC ', '#SORB',                 &
             '#ORB ', '#ERP ', '#GIM ', '#CRD ', '#SAP ',                 &
             '#RAO ', '#RAP ', '#RCO ', '#RCK ', '#SCK ',                 &
             '#GRD ', '#HOI ', '#RCL ', '#RGB ', '#GTRA',                 &
             '#GTRP', '#HLM ' /)

  CHARACTER(LEN=157)                 :: delim  =                          &
           ' ---------------------------------------------------'//       &
           '----------------------------------------------------'//       &
           '-----------------------------------------------------'

  CHARACTER(LEN=157)                 :: title =                           &
           '                              Correction  Estimated v'//      &
           'alue  RMS error   A priori value Unit    From      '  //      &
           '          To                  MJD           Num Abb  '

  CHARACTER(LEN=13), DIMENSION(7)    :: hlmStr =                          &
           (/'Translation X', 'Translation Y', 'Translation Z',           &
             'Rotation X   ', 'Rotation Y   ', 'Rotation Z   ',           &
             'Scale        '/)

! Initializations
! ---------------
  fstPar(:) = .true.
  numPar(:) = 0
  numSta    = 0
  numSat    = 0
  iExt      = 133
  iReq      = 0
  erpTimHlp = 0d0
  IF (opt%prt(prtExt)==1 .OR. iFil /= 0) iExt = 157

! Print parameter summary
! -----------------------
  IF (iFil == 0) THEN
    CALL prparlst(2,0,0,neq%par(1)%name,neq%par(1)%locq, &
                  neq%par(1)%time,neq%misc%nparms)
  ENDIF

! Count parameters, stations, and satellites
! ------------------------------------------
  DO iPar=1, neq%misc%nPar
    numPar(neq%par(iPar)%locq(1)) = numPar(neq%par(iPar)%locq(1))+1
    IF (neq%par(iPar)%locq(1)==1 .AND.                                    &
        neq%par(iPar)%locq(3)==1 .AND.                                    &
        neq%par(iPar)%locq(4)==1      ) numSta = numSta+1
    IF (neq%par(iPar)%locq(1)==3)                                         &
         iPos = listi4(1,maxSat,satLst,neq%par(iPar)%locq(3),numSat)
  END DO

! Print statistics
! ----------------
  IF (neq%misc%nObs+DBLE(neq%misc%nPseu) < 999999999.D0) THEN
    WRITE(hlpstr(1),'(I12)') NINT(neq%misc%nObs)
    WRITE(hlpstr(2),'(I12)') NINT(neq%misc%nObs)+neq%misc%nPseu
    WRITE(hlpstr(3),'(I12)') NINT(neq%misc%nObs)+neq%misc%nPseu-&
                                                 NINT(neq%misc%nParms)
    WRITE(hlpstr(4),'(I12)') NINT(neq%misc%nParms)
    WRITE(hlpstr(5),'(I12)') NINT(neq%misc%nParms)-neq%misc%nPar
  ELSE
    WRITE(hlpstr(1),'(ES12.4)') neq%misc%nObs
    WRITE(hlpstr(2),'(ES12.4)') neq%misc%nObs+DBLE(neq%misc%nPseu)
    WRITE(hlpstr(3),'(ES12.4)') (neq%misc%nObs-neq%misc%nParms)+&
                                                 DBLE(neq%misc%nPseu)
    WRITE(hlpstr(4),'(ES12.4)') neq%misc%nParms
    WRITE(hlpstr(5),'(ES12.4)') neq%misc%nParms-DBLE(neq%misc%nPar)
  ENDIF

  IF ( noInv == 0 ) THEN
    WRITE(lfn, '(2(A,/),/,2A,/,A,I12,/,2A,/,/,A,I12,2(/,2A),/,' // &
               '/,3(/,A,I12),//)')            &
      ' Statistics:                           ',                              &
      ' ----------                            ',                              &
      ' Total number of authentic observations', hlpstr(1),                   &
      ' Total number of pseudo-observations   ', neq%misc%nPseu,              &
      ' Total number of observations          ', hlpstr(2),                   &
      ' Total number of explicit parameters   ', neq%misc%nPar,               &
      ' Total number of implicit parameters   ', hlpstr(5),                   &
      ' Total number of parameters            ', hlpstr(4),                   &
      ' Total number of observation files     ', neq%misc%nfTot,              &
      ' Total number of stations              ', numSta,                      &
      ' Total number of satellites            ', numSat

    RETURN
  ENDIF

  WRITE(lfn, '(2(A,/),/,2A,/,A,I12,/,/,A,I12,/,2A,/,/,3(2A,/),' // &
             '/,A,F12.5,A,/,A,F12.2,/,3(/,A,I12),//)')            &
    ' Statistics:                           ',                                &
    ' ----------                            ',                                &
    ' Total number of authentic observations', hlpstr(1),                     &
    ' Total number of pseudo-observations   ', neq%misc%nPseu,                &
    ' Total number of explicit parameters   ', neq%misc%nPar,                 &
    ' Total number of implicit parameters   ', hlpstr(5),                     &
    ' Total number of observations          ', hlpstr(2),                     &
    ' Total number of adjusted parameters   ', hlpstr(4),                     &
    ' Degree of freedom (DOF)               ', hlpstr(3),                     &
    ' A posteriori RMS of unit weight       ', comStat%rms, ' m',             &
    ' Chi**2/DOF                            ', (comStat%rms/opt%sigma0)**2,   &
    ' Total number of observation files     ', neq%misc%nfTot,                &
    ' Total number of stations              ', numSta,                        &
    ' Total number of satellites            ', numSat

! Check Station Inconsistencies
! -----------------------------
  IF (ifil == 0 .AND. opt%snxinc == 1) THEN
    CALL chksin(neq%misc%sinex(1),neq%misc%sinex(1),1,neq)
  END IF

! Get system of dynamic orbit parameters
! --------------------------------------
  IF ( numPar(3) >= 6 ) THEN
    DO ilin = 1, orbdsc%nlin
      IF ( orbdsc%orbmod(ilin)(1:7) == 'EMPIRI:' )  &
        empiri = orbdsc%orbmod(ilin)(9:12)
    ENDDO
  ENDIF

  IF ( empiri == 'RSW ' )  eleStr(7:15) = rswStr(1:9)
  IF ( empiri == 'DRSW' )  eleStr(8:15) = rswStr(2:9)


! Write output for each parameter type
! ------------------------------------
  DO iPar=1, neq%misc%nPar
    pLine   = ' '
    parTyp  = neq%par(iPar)%locq(1)

! get time information for parameter
    meanMjd   = neq%par(iPar)%time%mean
    timMjd(1) = meanMjd-neq%par(iPar)%time%half
    timMjd(2) = meanMjd+neq%par(iPar)%time%half
    IF (neq%par(iPar)%time%half==0d0) timMjd(2) = 1d20
    CALL timst2(1, 2, timMjd, timStr)

    dspUnit = 1d0

    SELECT CASE (parTyp)

! --------------------------
! Coordinates and velocities
! --------------------------
      CASE (1)

! set title line
        IF (fstPar(parTyp)) THEN
          WRITE(title(1:30), '(A)') " Sol Station name         Typ"
          WRITE(lfn, '(//,A,/,A,//,A,/,A)')                            &
             ' Station coordinates and velocities:',                   &
             ' ----------------------------------',                    &
             title(1:iExt), delim(1:iExt)
          fstPar(parTyp) = .false.
        END IF

! get grep string and unit
        parGrep = grepStr(9)
        IF (neq%par(iPar)%locq(4)==3) THEN
          parUnit   = units(11)
          crdStrHlp = velStr(neq%par(iPar)%locq(3))
        ELSE
          parUnit   = units(1)
          crdStrHlp = xyzStr(neq%par(iPar)%locq(3))
        END IF

! solution number hardwired to 1
        iSol = 1

! write first part of output line
        WRITE(pLine(1:30), '(1X,I3,1X,A20,1X,A3,1X)')                     &
          iSol, neq%par(iPar)%name, crdStrHlp

! print the output line
        CALL oneLine

! ----------------------
! Receiver clock offsets/biases
! ----------------------
      CASE (2)

! set title line
        IF (fstPar(parTyp)) THEN
          WRITE(title(1:30), '(A)') " Station name         Typ     "
          WRITE(lfn, '(//,A,/,A,//,A,/,A)')                               &
             ' Receiver clock offset/bias:',                              &
             ' --------------------------',                               &
             title(1:iExt), delim(1:iExt)
          fstPar(parTyp) = .false.
        END IF

! get grep string and unit
        parGrep = grepStr(13)
        parUnit = units(3)
        dspUnit = 1d0/C*1d9

! write first part of output line
        IF (neq%par(iPar)%locq(6) == 0.AND.neq%par(iPar)%locq(7) == 0) THEN
          WRITE(pLine(1:30), '(1X,A16,4X,9X)') neq%par(iPar)%name
        ELSE IF (neq%par(iPar)%locq(6) == 0) THEN
          WRITE(pLine(1:30), '(1X,A16,4X,1X,A3,I4)')                      &
            neq%par(iPar)%name, 'SAT', neq%par(iPar)%locq(7)
        ELSE IF (neq%par(iPar)%locq(6) == 1) THEN
          WRITE(pLine(1:30), '(1X,A16,4X,1X,A3,I4)')                      &
            neq%par(iPar)%name, 'FRQ', neq%par(iPar)%locq(4)
        ELSE IF (neq%par(iPar)%locq(6) == 2) THEN
          WRITE(pLine(1:30), '(1X,A16,4X,1X,A3,I4)')                      &
            neq%par(iPar)%name, 'SAT', neq%par(iPar)%locq(4)
        ELSE IF (neq%par(iPar)%locq(6) == 4) THEN
          WRITE(pLine(1:30), '(1X,A16,4X,1X,A6,I1)')                      &
            neq%par(iPar)%name, 'FRQ n=', neq%par(iPar)%locq(4)-1
        ELSE IF (neq%par(iPar)%locq(6) == 5) THEN
          WRITE(pLine(1:30), '(1X,A16,4X,1X,A,I1)')                       &
            neq%par(iPar)%name, 'ISB L',neq%par(iPar)%locq(3)
        ENDIF

! print the output line
        CALL oneLine

! ----------------
! Orbital elements
! ----------------
      CASE (3)

! set title line
        IF (fstPar(parTyp)) THEN
          WRITE(title(1:30), '(A)') " Arc PRN Parameter            "
          WRITE(lfn, '(//,A,/,A,//,A,/,A)')                            &
             ' Orbital elements:',                                     &
             ' ----------------',                                      &
             title(1:iExt), delim(1:iExt)
          fstPar(parTyp) = .false.
        END IF

! get grep string and unit
        parGrep = grepStr(6)
        parUnit = units(13)
        iOrb    = neq%par(iPar)%locq(4)
        IF (iOrb==1) THEN
          parUnit = units(1)
        ELSE IF (iOrb==2) THEN
          parUnit = units(10)
        ELSE IF (iOrb==3 .OR. iOrb==4 .OR. iOrb==5 .OR. iOrb==6) THEN
          parUnit = units(4)
        END IF

! write first part of output line
        WRITE(pLine(1:30), '(1X,I3,1X,I3,1X,A8,13X)')                     &
          neq%par(iPar)%locq(2), neq%par(iPar)%locq(3),                   &
          eleStr(neq%par(iPar)%locq(4))

! print the output line
        CALL oneLine


! -------------------------
! Receiver antenna offsets
! -------------------------
      CASE (5)

! set title line
        IF (fstPar(parTyp)) THEN
          WRITE(title(1:30), '(A)') " Antenna name          F    T "
          WRITE(lfn, '(//,A,/,A,//,A,/,A)')                            &
             ' Receiver antenna offsets:',                             &
             ' ------------------------',                              &
             title(1:iExt), delim(1:iExt)
          fstPar(parTyp) = .false.
        END IF

! get grep string and unit
        parGrep = grepStr(11)
        parUnit = units(1)

        frq = MOD(neq%par(iPar)%locq(4) , 100)
        sys = MOD(neq%par(iPar)%locq(5) , 100)
        cor = neq%par(iPar)%locq(4) / 100

! write first part of output line
        WRITE(pLine(1:30), '(1X,A20,1X,A,I1,2X,A)')                  &
          neq%par(iPar)%name,g_svnsys(sys),frq,neuStr(cor)

! print the output line
        CALL oneLine


! ----------------------
! Troposphere parameters
! ----------------------
      CASE (6)

! set title line
        IF (fstPar(parTyp)) THEN
          WRITE(title(1:30), '(A)') " Station name         Typ     "
          WRITE(lfn, '(//,A,/,A,//,A,/,A)')                            &
             ' Site-specific troposphere parameters:',                 &
             ' ------------------------------------',                  &
             title(1:iExt), delim(1:iExt)
          fstPar(parTyp) = .false.
        END IF

! get grep string and unit
        parGrep = grepStr(2)
        parUnit = units(1)

! write first part of output line
        WRITE(pLine(1:30), '(1X,A20,1X,A3,5X)')                        &
          neq%par(iPar)%name, neuStr(neq%par(iPar)%locq(4))

! print the output line
        CALL oneLine


! ---------------------------------
! Differential code bias parameters
! ---------------------------------
      CASE (8)

! set title line
        IF (fstPar(parTyp)) THEN
          WRITE(title(1:30), '(A)') " Station Name / PRN   Typ     "
          WRITE(lfn, '(//,A,/,A,//,A,/,A)')                            &
             ' Differential code bias parameters:',                    &
             ' ---------------------------------',                     &
             title(1:iExt), delim(1:iExt)
          fstPar(parTyp) = .false.
        END IF

! get grep string and unit
        parGrep = grepStr(1)
        parUnit = units(3)

! get group, write first part of output line
        iGrp = neq%par(iPar)%locq(2)
        obst1 = '   '
        obst2 = '   '

        IF (iGrp==1) THEN
          IF (neq%par(iPar)%locq(5).EQ.1) THEN
            IF (neq%par(iPar)%locq(7).NE.0) THEN
              obst1 = neq%misc%obst(neq%par(iPar)%locq(7))%obstyp(1)
              obst2 = neq%misc%obst(neq%par(iPar)%locq(7))%obstyp(2)
            ELSE
              obst1 = OBSTYPESR3(4)
              obst2 = OBSTYPESR3(5)
            ENDIF
          ELSE IF (neq%par(iPar)%locq(5).EQ.2) THEN
            obst1 = OBSTYPESR3(4)
            obst2 = OBSTYPESR3(1)
          ELSE IF (neq%par(iPar)%locq(5).EQ.4) THEN
            obst1 = OBSTYPESR3(5)
            obst2 = OBSTYPESR3(2)
          ENDIF

          WRITE(pLine(1:30), '(1X,I3,11X,2(1X,A3),1X,I1,5X)')  &
            neq%par(iPar)%locq(3),obst1,obst2,neq%par(iPar)%locq(5)

        ELSE
          IF (neq%par(iPar)%locq(6).EQ.1) THEN
            IF (neq%par(iPar)%locq(7).NE.0) THEN
              obst1 = neq%misc%obst(neq%par(iPar)%locq(7))%obstyp(1)
              obst2 = neq%misc%obst(neq%par(iPar)%locq(7))%obstyp(2)
            ELSE
              obst1 = OBSTYPESR3(4)
              obst2 = OBSTYPESR3(5)
            ENDIF
          ELSE IF (neq%par(iPar)%locq(6).EQ.2) THEN
            obst1 = OBSTYPESR3(4)
            obst2 = OBSTYPESR3(1)
          ELSE IF (neq%par(iPar)%locq(6).EQ.4) THEN
            obst1 = OBSTYPESR3(5)
            obst2 = OBSTYPESR3(2)
          ENDIF

!          WRITE(pLine(1:30), '(1X,A20,1X,I3,1X,A1,3X)')                         &
!            neq%par(iPar)%name,neq%par(iPar)%locq(6),g_svnsys(neq%par(iPar)%locq(5)-1)
          WRITE(pLine(1:30), '(1X,A14,2(1X,A3),1X,I1,1X,A1,3X)')                         &
            neq%par(iPar)%name,obst1,obst2,neq%par(iPar)%locq(6),g_svnsys(neq%par(iPar)%locq(5)-1)
        END IF

! print the output line
        CALL oneLine


! -------------------------
! Earth rotation parameters
! -------------------------
      CASE (10)

! set title line
        IF (fstPar(parTyp)) THEN
          WRITE(title(1:30), '(A)') " Req Parameter                "
          WRITE(lfn, '(//,A,/,A,//,A,/,A)')                            &
             ' Earth rotation parameters:',                            &
             ' -------------------------',                             &
             title(1:iExt), delim(1:iExt)
          fstPar(parTyp) = .false.
        END IF

! drift or offset?
        iDrift = 0
        IF (neq%par(iPar)%locq(6)==2    .AND.                          &
            neq%par(iPar)%time%half/=0d0) iDrift = 1

! request number
        IF (meanMjd>erpTimHlp) THEN
          erpTimHlp = meanMjd
          iReq = iReq+1
        END IF


! get grep string and unit
        parGrep = grepStr(7)
        parUnit = units(5)
        IF (iDrift==0 .AND. neq%par(iPar)%locq(4)==3) THEN
          parUnit = units(2)
        ELSE IF (iDrift==1 .AND. neq%par(iPar)%locq(4)==3) THEN
          parUnit = units(6)
        ELSE IF (iDrift==1 .AND. neq%par(iPar)%locq(4)/=3) THEN
          parUnit = units(7)
        END IF

! write first part of output line
        WRITE(pLine(1:30), '(I4,1X,A9,16X)')                           &
          iReq, erpStr(neq%par(iPar)%locq(4), iDrift+1)

! write one output line
        CALL oneLine


! ---------------------------
! Stochastic orbit parameters
! ---------------------------
      CASE (11)

! set title line
        IF (fstPar(parTyp)) THEN
          WRITE(title(1:30), '(A)') " PRN Force type               "
          WRITE(lfn, '(//,A,/,A,//,A,/,A)')                            &
             ' Stochastic orbit parameters:',                          &
             ' ---------------------------',                           &
             title(1:iExt), delim(1:iExt)
          fstPar(parTyp) = .false.
        END IF

! get grep string and unit
        parGrep = grepStr(5)
        parUnit = units(12)

! write first part of output line
        WRITE(pLine(1:30), '(1X,I3,1X,A13,12X)')                          &
          neq%par(iPar)%locq(3), stcStr(mod(neq%par(iPar)%locq(5),10))

! print the output line
        CALL oneLine


! -------------------------
! Satellite antenna offsets
! -------------------------
      CASE (12)

! set title line
        IF (fstPar(parTyp)) THEN
          WRITE(title(1:30), '(A)') " Group PRN Typ Techn.         "
          WRITE(lfn, '(//,A,/,A,//,A,/,A)')                            &
             ' Satellite antenna offsets:',                            &
             ' -------------------------',                             &
             title(1:iExt), delim(1:iExt)
          fstPar(parTyp) = .false.
        END IF

! get grep string and unit
        parGrep = grepStr(3)
        parUnit = units(1)

        IF (neq%par(iPar)%locq(4) == 0) THEN
          WRITE(svnnr,"(A1,I3)")g_syssvn(INT(neq%par(iPar)%locq(5)/100)), &
                                              neq%par(iPar)%locq(5)
          CALL svn2prn(6,svnnr,neq%par(iPar)%time%mean,prn,timint,ios)
          WRITE(strprn,"(I3)")prn
        ELSE
          strprn = '   '
        ENDIF

! Get technique
        hlp_tech = '     '
        IF ( is_techn(neq%par(iPar),gnss=1 ) )  &
           WRITE(hlp_tech(1:6),'(A)') ' GNSS '
        IF ( is_techn(neq%par(iPar),slr=1  ) )  &
           WRITE(hlp_tech(1:6),'(A)') ' SLR  '

! write first part of output line
!        WRITE(pLine(1:30), '(1X,I5,1X,A3,1X,A3,1X,A6,9X)')             &
!          neq%par(iPar)%locq(5),strprn,xyzStr(neq%par(iPar)%locq(3)),  &
!          hlp_tech

        obst1 = '   '
        obst2 = '   '
        IF (neq%par(iPar)%locq(7).NE.0) THEN
           obst1 = neq%misc%obst(neq%par(iPar)%locq(7))%obstyp(3)
           obst2 = neq%misc%obst(neq%par(iPar)%locq(7))%obstyp(4)
        ELSE
           obst1 = OBSTYPESR3(6)
           obst2 = OBSTYPESR3(7)
        ENDIF

        WRITE(pLine(1:30), '(1X,I5,1X,A3,1X,A3,1X,A6,2(1X,A3),1X)')    &
          neq%par(iPar)%locq(5),strprn,xyzStr(neq%par(iPar)%locq(3)),  &
          hlp_tech,obst1,obst2

! print the output line
        CALL oneLine

! --------------------------
! Center of mass coordinates
! --------------------------
      CASE (16)

! set title line
        IF (fstPar(parTyp)) THEN
          WRITE(title(1:30), '(A)') " Typ                          "
          WRITE(lfn, '(//,A,/,A,//,A,/,A)')                            &
             ' Geocenter coordinates:',                                &
             ' ---------------------',                                 &
             title(1:iExt), delim(1:iExt)
          fstPar(parTyp) = .false.
        END IF

! get grep string and unit
        parGrep = grepStr(4)
        parUnit = units(1)

! write first part of output line
        WRITE(pLine(1:30), '(1X,A3,26X)')                              &
          xyzStr(neq%par(iPar)%locq(2))

! print the output line
        CALL oneLine

! store values for gcc file
        iGcc         = neq%par(iPar)%locq(2)
        gccMjd       = timMjd
        gccVal(iGcc) = neq%xxx(iPar)+neq%par(iPar)%x0
        gccErr(iGcc) = comstat%rms*SQRT(ABS(neq%aNor(ikf(iPar,iPar))))

! -------------------------
! Receiver antenna pattern
! -------------------------
      CASE (18)

! set title line
        IF (fstPar(parTyp)) THEN
          WRITE(title(1:30), '(A)') " Antenna name          F  Z  A"
          WRITE(lfn, '(//,A,/,A,//,A,/,A)')                            &
             ' Receiver antenna pattern:',                             &
             ' ------------------------',                              &
             title(1:iExt), delim(1:iExt)
          fstPar(parTyp) = .false.
        END IF

! get grep string and unit
        parGrep = grepStr(12)
        parUnit = units(1)

        frq = MOD(neq%par(iPar)%locq(4) , 100)
        sys = MOD(neq%par(iPar)%locq(5) , 100)
        zen = neq%par(iPar)%locq(4) / 100
        azi = neq%par(iPar)%locq(5) / 100

! write first part of output line
        WRITE(pLine(1:30), '(1X,A20,1X,A1,I1,I3,I3)')                  &
          neq%par(iPar)%name,g_svnsys(sys),frq,zen,azi

! print the output line
        CALL oneLine


! ----------------------------------
! Global ionosphere model parameters
! ----------------------------------
      CASE (19)

! set title line
        IF (fstPar(parTyp)) THEN
          WRITE(title(1:30), '(A)') " Model name       Deg   Ord   "
          WRITE(lfn, '(//,A,/,A,//,A,/,A)')                            &
             ' Global ionosphere model parameters:',                   &
             ' ----------------------------------',                    &
             title(1:iExt), delim(1:iExt)
          fstPar(parTyp) = .false.
        END IF

! get grep string and unit
        parGrep = grepStr(8)
        parUnit = units(9)

! write first part of output line
        WRITE(pLine(1:30), '(1X,A16,1X,I5,1X,I5,1X)')                     &
          neq%par(iPar)%name(1:16), neq%par(iPar)%locq(4), neq%par(iPar)%locq(5)

! print the output line
        CALL oneLine

! -------------------------------------
! Scaling factors for Vienna grid files
! -------------------------------------
      CASE (22)

! set title line
        IF (fstPar(parTyp)) THEN
          WRITE(title(1:30), '(A)') " Grid     Station name    Typ "
          WRITE(lfn, '(//,A,/,A,//,A,/,A)')                            &
             ' Scaling factors of Vienna grid files',                  &
             ' ------------------------------------',                  &
             title(1:iExt), delim(1:iExt)
          fstPar(parTyp) = .false.
        END IF

! get grep string and unit
        parGrep = grepStr(16)
        parUnit = units(10)

! write first part of output line
        IF (neq%par(iPar)%locq(3) == 0) THEN
          WRITE(pLine(1:30), '(1X,A8,1X,A16,1X,A3)')                     &
            neq%misc%grdNeq(neq%par(iPar)%locq(2))(1:8),neq%par(iPar)%name(1:16), &
            grdStr(neq%par(iPar)%locq(4), neq%par(iPar)%locq(5))
        ELSEIF (neq%par(iPar)%locq(3) == -1) THEN
          WRITE(pLine(1:30), '(1X,A8,1X,A16,1X,A3)')                     &
            neq%misc%grdNeq(neq%par(iPar)%locq(2))(1:8),'all stations    ',&
            grdStr(neq%par(iPar)%locq(4), neq%par(iPar)%locq(5))
        ELSE
          WRITE(pLine(1:30), '(1X,A8,1X,A13,I3.3,1X,A3)')                &
            neq%misc%grdNeq(neq%par(iPar)%locq(2))(1:8),'station group', &
            grdStr(neq%par(iPar)%locq(4), neq%par(iPar)%locq(5))
        ENDIF

! print the output line
        CALL oneLine

! -------------------------
! Receiver clock parameters
! -------------------------
      CASE(23)

! Set title line
        IF (fstPar(parTyp)) THEN
          WRITE(title(1:30), '(A)') " Station name          #obs   "
          WRITE(lfn, '(//,A,/,A,//,A,/,A)')                            &
               ' Epochwise station clocks:',                           &
               ' ------------------------',                            &
               title(1:iExt), delim(1:iExt)
          fstPar(parTyp) = .false.
        END IF

! get grep string and unit
        parGrep = grepStr(14)
        parUnit = units(3)
        dspUnit = 1d0/C*1d9

! write first part of output line
        chr1 = ' '
        DO iRef = 1,clkHed%ref(1)%nRef
          IF (neq%par(iPar)%name == clkHed%ref(1)%clk(iRef)%name) THEN
            chr1 = 'R'
            EXIT
          ENDIF
        ENDDO

        WRITE(pLine(1:30), '(1X,A,I6,A2,1X)')                          &
                  neq%par(iPar)%name,neq%par(iPar)%locq(6),chr1
        IF (neq%par(iPar)%locq(3) > 0) &
          WRITE(pLine(21:23), '(A)') g_strsys3(neq%par(iPar)%locq(3)-1)

! print the output line
        neq%xxx(iPar)=-neq%xxx(iPar)
        CALL oneLine
        neq%xxx(iPar)=-neq%xxx(iPar)


! --------------------------
! Satellite clock parameters
! --------------------------
      CASE(24)

! Set title line
        IF (fstPar(parTyp)) THEN
          WRITE(title(1:30), '(A)') " Satellite             #obs   "
          WRITE(lfn, '(//,A,/,A,//,A,/,A)')                            &
               ' Epochwise satellite clocks:',                         &
               ' --------------------------',                          &
               title(1:iExt), delim(1:iExt)
          fstPar(parTyp) = .false.
        END IF

! get grep string and unit
        parGrep = grepStr(15)
        parUnit = units(3)
        dspUnit = 1d0/C*1d9

! write first part of output line
        chr1 = ' '
        DO iRef = 1,clkHed%ref(1)%nRef
          IF (neq%par(iPar)%name == clkHed%ref(1)%clk(iRef)%name) THEN
            chr1 = 'R'
            EXIT
          ENDIF
        ENDDO

        WRITE(pLine(1:30), '(1X,A,I6,A2,1X)')                          &
                  neq%par(iPar)%name,neq%par(iPar)%locq(6),chr1

! print the output line
        neq%xxx(iPar)=-neq%xxx(iPar)
        CALL oneLine
        neq%xxx(iPar)=-neq%xxx(iPar)


! --------------------------------
! Satellite antenna phase patterns
! --------------------------------
      CASE (25)

! set title line
        IF (fstPar(parTyp)) THEN
          WRITE(title(1:30), '(A)') " Group PRN   Nad  Azi         "
          WRITE(lfn, '(//,A,/,A,//,A,/,A)')                            &
             ' Satellite antenna phase patterns:',                     &
             ' --------------------------------',                      &
             title(1:iExt), delim(1:iExt)
          fstPar(parTyp) = .false.
        END IF

! get grep string and unit
        parGrep = grepStr(10)
        parUnit = units(1)

        IF (neq%par(iPar)%locq(2) == 0) THEN
          WRITE(svnnr,"(A1,I3)")g_syssvn(INT(neq%par(iPar)%locq(3)/100)), &
                                              neq%par(iPar)%locq(3)
          CALL svn2prn(6,svnnr,neq%par(iPar)%time%mean,prn,timint,ios)
          WRITE(strprn,"(I3)")prn
        ELSE
          strprn = '   '
        ENDIF

! write first part of output line
!        WRITE(pLine(1:30), '(1X,I5,1X,A3,1X,2I5,9X)')                  &
!          neq%par(iPar)%locq(3),strprn,neq%par(iPar)%locq(4:5)

        READ(neq%par(iPar)%name,'(A10,I5)') helpstr,indobst
        obst1 = '   '
        obst2 = '   '
        IF (indobst.NE.0) THEN
           obst1 = neq%misc%obst(indobst)%obstyp(3)
           obst2 = neq%misc%obst(indobst)%obstyp(4)
        ELSE
           obst1 = OBSTYPESR3(6)
           obst2 = OBSTYPESR3(7)
        ENDIF

        WRITE(pLine(1:30), '(1X,I5,1X,A3,1X,2I5,2(1X,A3),1X)')         &
          neq%par(iPar)%locq(3),strprn,neq%par(iPar)%locq(4:5),        &
          obst1,obst2

! print the output line
        CALL oneLine


! ----------------
! SLR Range biases
! ----------------
      CASE (26)

! set title line
        IF (fstPar(parTyp)) THEN
          WRITE(title(1:30), '(A)') " Station             Sat  WL  "
          WRITE(lfn, '(//,A,/,A,//,A,/,A)')                         &
             ' SLR range biases:',                                  &
             ' ----------------',                                   &
             title(1:iExt), delim(1:iExt)
          fstPar(parTyp) = .false.
        END IF

! get grep string and unit
        parGrep = grepStr(19)
        parUnit = units(1)

! write first part of output line
        WRITE(pLine(1:30), '(1X,A16,2X,I5,2X,I2,1X)')    &
          neq%par(iPar)%name(1:16), neq%par(iPar)%locq(5), neq%par(iPar)%locq(4)

! print the output line
        CALL oneLine


! ---------------------------------------------
! Higher-order ionosphere (HOI) scaling factors
! ---------------------------------------------
      CASE (27)

! set title line
        IF (fstPar(parTyp)) THEN
          WRITE(title(1:30), '(A)') " Station name         Typ     "
          WRITE(lfn, '(//,A,/,A,//,A,/,A)')                            &
             ' Higher-order ionosphere scaling factors:',              &
             ' ---------------------------------------',               &
             title(1:iExt), delim(1:iExt)
          fstPar(parTyp) = .false.
        END IF

! get grep string and unit
        parGrep = grepStr(17)
        parUnit = units(10)

! write first part of output line
        WRITE(pLine(1:30), '(1X,A20,1X,I3,5X)')                        &
          neq%par(iPar)%name, neq%par(iPar)%locq(2)

! print the output line
        CALL oneLine


! ------------------
! Helmert parameters
! ------------------
      CASE (28)

! set title line
        IF (fstPar(parTyp)) THEN
          WRITE(title(1:30), '(A)') " Group Parameter              "
          WRITE(lfn, '(//,A,/,A,//,A,/,A)')                            &
             ' Helmert parameters:',                                   &
             ' -------------------',                                   &
             title(1:iExt), delim(1:iExt)
          fstPar(parTyp) = .false.

        END IF

! get grep string, unit and rescale rotations and scale
        parGrep = grepStr(22)

        IF ( neq%par(iPar)%locq(2) > 3 .AND. neq%par(iPar)%locq(2) <= 6 ) THEN
          neq%par(iPar)%x0 = neq%par(iPar)%x0 * ars * 1.D3 / datum%aell
          neq%xxx(iPar)    = neq%xxx(iPar) * ars * 1.D3 / datum%aell
          neq%aNor(ikf(iPar,iPar)) = neq%aNor(ikf(iPar,iPar)) * (ars*1.D3 / datum%aell)**2
          parUnit = units(5)

        ELSE IF ( neq%par(iPar)%locq(2) == 7 ) THEN
          neq%par(iPar)%x0 = neq%par(iPar)%x0 / datum%aell * 1.D9
          neq%xxx(iPar)    = neq%xxx(iPar) / datum%aell * 1.D9
          neq%aNor(ikf(iPar,iPar)) = neq%aNor(ikf(iPar,iPar)) * (1.D9 / datum%aell)**2
          parUnit = units(14)

        ELSE
          parUnit = units(1)

        END IF

! write first part of output line
        WRITE(pLine(1:30), '(1X,A5,1X,A13,10X)')                       &
          neq%par(iPar)%name, hlmStr(neq%par(iPar)%locq(2))

! print the output line
        CALL oneLine


! ---------------------------------------------
! GNSS-specific parameters
! ---------------------------------------------
      CASE (30)

! set title line
        IF (fstPar(parTyp)) THEN
          WRITE(title(1:30), '(A)') " Station name         Sys Cmp "
          WRITE(lfn, '(//,A,/,A,//,A,/,A)')                       &
             ' GNSS-specific parameters:',                        &
             ' ------------------------',                         &
             title(1:iExt), delim(1:iExt)
          fstPar(parTyp) = .false.
        END IF

! Translations
        IF (neq%par(iPar)%locq(3) /= 4) THEN
          parGrep = grepStr(20)
          parUnit = units(1)
          WRITE(pLine(1:30), '(1X,A20,2X,A1,1X,A3)')                   &
            neq%par(iPar)%name,g_svnSys(neq%par(iPar)%locq(4)),        &
            xyzStr(neq%par(iPar)%locq(3))

! Troposphere biases
        ELSEIF(neq%par(iPar)%locq(3) == 4) THEN
          parGrep = grepStr(21)
          parUnit = units(1)
          WRITE(pLine(1:30), '(1X,A20,2X,A1,1X,A3)')                   &
            neq%par(iPar)%name,g_svnSys(neq%par(iPar)%locq(4)),"T"
        ENDIF

! print the output line
        CALL oneLine

    END SELECT
  END DO


! Write GCC file
! --------------
  IF (opt%gccout /= ' ') THEN
    CALL opnfil(lfnloc,opt%gccout,'UNKNOWN',' ',' ',' ',ios)
    CALL opnerr(lfnerr,lfnloc,ios,opt%gccout,'NEQPRT')
    WRITE(lfnloc,"(F9.2,1X,F9.2,3X,3F9.5,3X,3F9.5)")                      &
         gccMjd(1:2),gccVal(1:3),gccErr(1:3)
    CLOSE(lfnloc)
  ENDIF

! ------------------------------------
! Definition of additional subroutines
! ------------------------------------
  CONTAINS

! Write one output line
! ---------------------
  SUBROUTINE oneLine

    IF(neq%aNor(ikf(iPar,iPar))==0.d0 .AND.                               &
       neq%bNor(iPar)==0.d0.AND.neq%xxx(iPar)==0.d0) THEN
      WRITE(pLine(31:157),sngStr)                                         &
        "         -       singular            - ",                        &
        neq%par(iPar)%x0*dspUnit,                                         &
        parUnit,                                                          &
        timStr( 1:19),                                                    &
        timStr(22:40),                                                    &
        meanMjd,                                                          &
        iPar,                                                             &
        parGrep
    ELSE
      WRITE(pLine(31:157), fmtStr)                                        &
        neq%xxx(iPar)*dspUnit,                                            &
        neq%xxx(iPar)*dspUnit+neq%par(iPar)%x0*dspUnit,                   &
        comstat%rms*SQRT(ABS(neq%aNor(ikf(iPar,iPar))))*dspUnit,          &
        neq%par(iPar)%x0*dspUnit,                                         &
        parUnit,                                                          &
        timStr( 1:19),                                                    &
        timStr(22:40),                                                    &
        meanMjd,                                                          &
        iPar,                                                             &
        parGrep
    END IF

    WRITE(lfn, '(A)') pLine(1:iExt)

!    pLine(iExt+1:iExt+4) = '  ' // neq%par(iPar)%type
!    WRITE(lfn, '(A)') pLine(1:iExt+4)

  END SUBROUTINE oneLine

! End subroutine
! --------------
END SUBROUTINE neqprt

END MODULE
