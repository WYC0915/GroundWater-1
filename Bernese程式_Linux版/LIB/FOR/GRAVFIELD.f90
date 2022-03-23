! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE s_gravField

! -------------------------------------------------------------------------
! Desctiption: Reads A Priori Gravity Field
!
! Author:     G.Beutler, L. Mervart
!
! Created:    04-Dec-2006
!
! Changes:    16-JAN-2007 LP: ADD SR init_pot,alloc_pot,getPot2,gravStore2
!             14-AUG-2008 AJ: ADD SR getTimeVar
!             05-Aug-2009 AJ: Save only updates on request in gravStore2
!             06-Aug-2009 AJ: Extensions for BERN format in getTimeVar
!             21-Sep-2010 RD: ST2TIM can be used as a module now
!             16-Dec-2010 RD: t_neq is taken from D_NEQ instead of P_ADDNEQ
!             30-Apr-2012 RD: Nullify keyValue pointers, use m_bern with only
!             30-Apr-2012 RD: Remove unused variables
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

USE m_bern,    ONLY: i4b, r8b, lfnloc, lfnerr, lfnres, &
                     keyValueLength, timStrgLength, &
                     shortLineLength, fileNameLength, lineLength
USE d_const,   ONLY: AE, GM, PI

PRIVATE

TYPE t_gravField
  CHARACTER(LEN=fileNameLength)     :: source ! (e.g. EIGEN2)
  CHARACTER(LEN=shortlineLength)    :: title  ! (needed by ORBGEN)
  INTEGER(i4b)                      :: mjd
  REAL(r8b)                         :: gm
  REAL(r8b)                         :: ae
  LOGICAL                           :: tideFree
  LOGICAL                           :: meanPole
  INTEGER(i4b)                      :: maxDegree
  REAL(r8b), DIMENSION(:), POINTER  :: cCoe
  REAL(r8b), DIMENSION(:), POINTER  :: sCoe
  REAL(r8b), DIMENSION(:), POINTER  :: cSigma
  REAL(r8b), DIMENSION(:), POINTER  :: sSigma
  REAL(r8b), DIMENSION(:), POINTER  :: cApr
  REAL(r8b), DIMENSION(:), POINTER  :: sApr
  INTEGER(i4b)                      :: maxDegreeDot
  REAL(r8b), DIMENSION(:), POINTER  :: cCoeDot
  REAL(r8b), DIMENSION(:), POINTER  :: sCoeDot
  REAL(r8b), DIMENSION(:), POINTER  :: cSigmaDot
  REAL(r8b), DIMENSION(:), POINTER  :: sSigmaDot
  REAL(r8b), DIMENSION(:), POINTER  :: cAprDot
  REAL(r8b), DIMENSION(:), POINTER  :: sAprDot
END TYPE

PUBLIC :: t_gravField, gravField, init_pot,alloc_pot,getPot2,gravStore2

! Module Variables
! ----------------
CHARACTER(LEN=*), PARAMETER  :: mName = 'gravfield' ! module name

! Global Variables
! ----------------
TYPE(t_gravField) :: gravField



CONTAINS

! ----------------------------------------------------------------------

SUBROUTINE init_pot()

  USE s_ckoptb
  USE s_ckopti
  USE s_ckoptl
  USE s_readKeys
  USE f_tstkey

! List of Parameters
! ------------------
  IMPLICIT NONE

  INTEGER(i4b)                :: MPOL
  INTEGER(i4b)                :: MDOT

! Declaration for readKeys
! ------------------------
  INTEGER(i4b)                 :: irCode
  CHARACTER(LEN=8),  PARAMETER :: srName = 'init_pot'
  CHARACTER(LEN=keyValueLength),DIMENSION(:),POINTER :: keyValue
  INTEGER(i4b)                 :: irc
  CHARACTER(LEN=80)            :: RPRFILENAME
  CHARACTER(LEN=1)             :: modFlg ! model flag: " ","A","B","C","L"

  MPOL = 0
  MDOT = 0

  NULLIFY(keyValue)

! ===========================================================================
! Set MEANPOLE convention

  IF (tstKey('MEAN_POLE')) THEN
!     GRAVDET: look for Checkbox "MEAN-POLE"
      CALL ckoptb(1,(/'MEAN_POLE'/),srName,                             &
                'Use mean pole according to IERS2003?',                 &
                irCode,result1=MPOL)
! ==========

  ELSEIF (tstKey('RPROUT')) THEN
!   ORBGEN:  look for Keyword "RPROUT", if RPR-Files are written then
!              MPOL = 1 ELSE look for Checkbox "MEAN_POLE_ORBGEN"
         CALL readKeys('RPROUT', keyValue, irc)
         CALL ckoptl(0,'RPROUT', keyValue,srName, 'Radiation pressure coeff.', &
              irc, irCode, empty="",maxLength=80, maxVal=1, result1=RPRFILENAME)

         CALL readkeys('ORBMODFL', keyValue, irc)
         modFlg=keyValue(1)

!        use meanpole information from panel
         IF ((modFlg .EQ. 'O') .OR. (modFlg .EQ. 'A')) THEN
!           use old meanpole convention for old orbit models
            MPOL = 0
         ELSE
!           check whether RPR-files should be written to decide which meanpole
            IF (RPRFILENAME .eq. "") THEN
!               no RPR-file: manual selection of MPOL-mode using Checkbox "MEAN_POLE_ORBGEN"
                IF (tstKey('MEAN_POLE_ORBGEN')) THEN
                   CALL ckoptb(1,(/'MEAN_POLE_ORBGEN'/),srName,                    &
                   'Use mean pole according to IERS2003?',                     &
                   irCode,result1=MPOL)
                ENDIF
            ELSE
!               RPR-file written (routine mode): MPOL according to IERS2003 convention
                MPOL = 1
            ENDIF
         ENDIF
  ENDIF

  IF (MPOL == 1) THEN
  ! mean pole according to IERS2003
          gravField%meanPole = .TRUE.
  ELSE
  ! mean pole according to IERS1996
          gravField%meanPole = .FALSE.
  ENDIF

! =====================================================================================

  gravField%mjd = 0
  IF (tstKey('MEAN_DOT')) THEN
      CALL ckoptb(1,(/'MEAN_DOT'/),srName,                              &
                'Apply the dot-values to a mean session?',              &
                irCode,result1=MDOT)
  ENDIF

  IF (MDOT == 1) THEN
          CALL readKeys('MEAN_MJD', keyValue, irc)
          CALL ckopti(1,'MEAN_MJD', keyValue, srName,                   &
            'Mean session (MJD) for dot value computation',irc,irCode,  &
             maxVal=1,ge=0,result1=gravField%mjd)
  ENDIF

  IF (ASSOCIATED(keyValue)) DEALLOCATE(keyValue)

  gravField%source        = ''
  gravField%title         = ''
  gravField%gm            = 0.d0
  gravField%ae            = 0.d0
  gravField%tideFree      = .TRUE.
  gravField%maxDegree     = 0
  gravField%maxDegreeDot  = 0

  NULLIFY(gravField%cCoe)
  NULLIFY(gravField%sCoe)
  NULLIFY(gravField%cSigma)
  NULLIFY(gravField%sSigma)
  NULLIFY(gravField%cApr)
  NULLIFY(gravField%sApr)
  NULLIFY(gravField%cCoeDot)
  NULLIFY(gravField%sCoeDot)
  NULLIFY(gravField%cSigmaDot)
  NULLIFY(gravField%sSigmaDot)
  NULLIFY(gravField%cAprDot)
  NULLIFY(gravField%sAprDot)


END SUBROUTINE init_pot

! -----------------------------------------------------------------------

SUBROUTINE alloc_pot()

 USE m_maxdim,  ONLY: maxPot
 USE s_alcerr
 USE s_readKeys
 USE s_ckopti
 USE f_tstkey


  IMPLICIT NONE

! List of Parameters
! ------------------
  INTEGER(i4b) :: nPotMx ! MAXIMUM ORDER OF COEFFICIENTS ALLOWED
  INTEGER(i4b) :: maxDegreeDot ! MAXIMUM DEGREE OF DOT VALUES

! Local Variables
! ---------------
  INTEGER(i4b) :: IMAX
  INTEGER(i4b) :: DMAX
  INTEGER(i4b) :: iac
  INTEGER(i4b) :: lumped                       ! ESTIMATE LUMPED COEFFICIENTS? (0/1)
  INTEGER(i4b) :: lumped_n                     ! MAXIMUM DEGREE OF LUMPED COEFFICIENTS
  INTEGER(i4b) :: est_grav


! Declaration for readKeys
! ------------------------
  CHARACTER(LEN=keyValueLength),DIMENSION(:),POINTER :: keyValue
  INTEGER(i4b) :: irc
  INTEGER(i4b) :: irCode
  CHARACTER(LEN=9),  PARAMETER :: srName = 'alloc_pot'



! READ MAXIMUM DEGREE OF GEOPOTENTIAL COEFFICIENTS FROM INPUT PANEL
! -----------------------------------------------------------------
 NULLIFY(keyValue)

 est_grav=0
 IF (tstKey('UPPER_N')) THEN
     CALL readKeys('UPPER_N', keyValue, irc)
     CALL ckopti(1,'UPPER_N', keyValue, srName,                      &
              'Earth potential upper limits, degree n=',irc,irCode,  &
              maxVal=1,result1=gravField%maxDegree)
     CALL readKeys('POT_EST', keyValue, irc)
     CALL ckopti(1,'POT_EST', keyValue, srName,                      &
              'Estimate gravity field coefficients?',irc,irCode,     &
              maxVal=1,result1=est_grav)

     IF (est_grav == 1 .and. (tstKey('POT_LUMPED'))) THEN
                CALL readKeys('POT_LUMPED', keyValue, irc)
                CALL ckopti(1,'POT_LUMPED', keyValue, srName,        &
                        'Solve for lumped coefficients?',irc,irCode, &
                        maxVal=1,result1=lumped)
                CALL readKeys('POT_LUMPED_N', keyValue, irc)
                CALL ckopti(1,'POT_LUMPED_N', keyValue, srName,      &
                         'Lumped up to degree n=',irc,irCode,        &
                        maxVal=1,result1=lumped_n)
                IF (lumped==1) THEN
                    IF (lumped_n > gravField%maxDegree) THEN
                              gravField%maxDegree = lumped_n
                    ENDIF
                ENDIF
     ENDIF
 ELSEIF (tstKey('MXPOTDEG')) THEN
     CALL readKeys('MXPOTDEG', keyValue, irc)
     CALL ckopti(1,'MXPOTDEG', keyValue, srName,                     &
              'Earth potential upper limits, degree n=',irc,irCode,  &
              maxVal=1,result1=gravField%maxDegree)

 ELSE
     gravField%maxDegree = MAXPOT
 ENDIF




! READ MAXIMUM ORDER OF COEFFICIENTS ALLOWED FROM INPUT PANEL
! -----------------------------------------------------------
!! IF (tstKey('UPPER_N')) THEN
!     CALL readKeys('UPPER_N', keyValue, irc)
!     CALL ckopti(1,'UPPER_N', keyValue, srName,                      &
!              'Earth potential upper limits, degree n=',irc,irCode,  &
!              maxVal=1,result1=nPotMx)
!
! ELSEIF (tstKey('MXPOTDEG')) THEN
!     CALL readKeys('MXPOTDEG', keyValue, irc)
!     CALL ckopti(1,'MXPOTDEG', keyValue, srName,                     &
!              'Earth potential upper limits, degree n=',irc,irCode,  &
!              maxVal=1,result1=nPotMx)
! ELSE
!    nPotMx = MAXPOT
! ENDIF

  IF (ASSOCIATED(keyValue)) DEALLOCATE(keyValue)

  maxDegreeDot=2
  IMAX=(maxPot+1)*(maxPot+2)/2
  DMAX=(maxDegreeDot+1)*(maxDegreeDot+2)/2

  IF ( ASSOCIATED(gravField%cCoe) ) THEN
    DEALLOCATE( gravField%cCoe, stat=iac )
    DEALLOCATE( gravField%sCoe, stat=iac )
    DEALLOCATE( gravField%cSigma, stat=iac )
    DEALLOCATE( gravField%sSigma, stat=iac )
    DEALLOCATE( gravField%cApr, stat=iac )
    DEALLOCATE( gravField%sApr, stat=iac )
  END IF
  IF ( ASSOCIATED(gravField%cCoeDot) ) THEN
    DEALLOCATE( gravField%cCoeDot, stat=iac )
    DEALLOCATE( gravField%sCoeDot, stat=iac )
    DEALLOCATE( gravField%cSigmaDot, stat=iac )
    DEALLOCATE( gravField%sSigmaDot, stat=iac )
    DEALLOCATE( gravField%cAprDot, stat=iac )
    DEALLOCATE( gravField%sAprDot, stat=iac )
  END IF

  IF (gravField%maxDegree > 0) THEN
    ALLOCATE( gravField%cCoe(IMAX), stat=iac )
    CALL alcerr(iac, 'gravField%cCoe', (/IMAX/), 'alloc_pot')
    ALLOCATE( gravField%sCoe(IMAX), stat=iac )
    CALL alcerr(iac, 'gravField%sCoe', (/IMAX/), 'alloc_pot')
    ALLOCATE( gravField%cSigma(IMAX), stat=iac )
    CALL alcerr(iac, 'gravField%cSigma', (/IMAX/), 'alloc_pot')
    ALLOCATE( gravField%sSigma(IMAX), stat=iac )
    CALL alcerr(iac, 'gravField%sSigma', (/IMAX/), 'alloc_pot')
    ALLOCATE( gravField%cApr(IMAX), stat=iac )
    CALL alcerr(iac, 'gravField%cApr', (/IMAX/), 'alloc_pot')
    ALLOCATE( gravField%sApr(IMAX), stat=iac )
    CALL alcerr(iac, 'gravField%sApr', (/IMAX/), 'alloc_pot')

    gravField%cCoe(:)        = 0.d0
    gravField%sCoe(:)        = 0.d0
    gravField%cSigma(:)      = 0.d0
    gravField%sSigma(:)      = 0.d0
    gravField%cApr(:)        = 0.d0
    gravField%sApr(:)        = 0.d0

  END IF

  IF (maxDegreeDot > 0) THEN
    ALLOCATE( gravField%cCoeDot(DMAX), stat=iac )
    CALL alcerr(iac, 'gravField%cCoeDot', (/DMAX/), 'alloc_pot')
    ALLOCATE( gravField%sCoeDot(DMAX), stat=iac )
    CALL alcerr(iac, 'gravField%sCoeDot', (/DMAX/), 'alloc_pot')
    ALLOCATE( gravField%cSigmaDot(DMAX), stat=iac )
    CALL alcerr(iac, 'gravField%cSigmaDot', (/DMAX/), 'alloc_pot')
    ALLOCATE( gravField%sSigmaDot(DMAX), stat=iac )
    CALL alcerr(iac, 'gravField%sSigmaDot', (/DMAX/), 'alloc_pot')
    ALLOCATE( gravField%cAprDot(DMAX), stat=iac )
    CALL alcerr(iac, 'gravField%cAprDot', (/DMAX/), 'alloc_pot')
    ALLOCATE( gravField%sAprDot(DMAX), stat=iac )
    CALL alcerr(iac, 'gravField%sAprDot', (/DMAX/), 'alloc_pot')

    gravField%cCoeDot(:)       = 0.d0
    gravField%sCoeDot(:)       = 0.d0
    gravField%cSigmaDot(:)     = 0.d0
    gravField%sSigmaDot(:)     = 0.d0
    gravField%cAprDot(:)       = 0.d0
    gravField%sAprDot(:)       = 0.d0


  END IF

END SUBROUTINE alloc_pot



! ---------------------------------------------------------------------------



! Read A Priori Gravity Field, store it into the global structure gravField
! -------------------------------------------------------------------------

SUBROUTINE getPot2(tMjd)

! -------------------------------------------------------------------------
! Purpose:    INPUT OF POTENTIAL COEFFICIENTS UP TO DEGREE gravField%maxDegree
!             FROM INPUT DATASET (LFNLOC)
!
!
!  AUTHOR     :  W. GURTNER
!
!  CREATED    :  87/11/30 08:11        LAST MODIFIED :  06-JAN-2004
!
!  CHANGES    :  11-JAN-1993 : USE OF SR "OPNFIL" TO OPEN FILES
!                25-JUN-1996 : TS: ALLOW TWO FORMATS (GEMT3 and JGM3)
!                11-JUL-1996 : TS: ACCOUNT FOR C02 DRIFT IN JGM3
!                21-SEP-1999 : TS: ALLOW FOR GRIM5 FORMAT
!                03-MAY-2000 : HB: INCLUDE I:MAXPOT,CHANGES INDEXING OF
!                                  CPOT AND SPOT (ADD THE FIRST THREE
!                                  TERMS WHICH ARE =0.D0)
!                22-JUN-2000 : HB: ALLOW FOR EGM96 FORMAT
!                30-Jan-2002 : HB: ALLOW FOR TEG4 and EIGEN1S FORMAT,
!                                  REALLY ACTIVATE C20 DRIFT IN JGM3
!                01-Oct-2002 : HB: add zero tide for EGM96 and EIGEN1S models
!                21-DEC-2002 : DS: EIGEN-2 introduced
!                08-MAR-2003 : HU: TITLE AS CHR80 STRING
!                13-MAR-2003 : DS: GRIM5-S1 and GRIM5-C1 introduced
!                13-MAR-2003 : DS: INITIALIZE CPOT(1)=1.D0
!                13-MAR-2003 : DS: DO NOT ADD INITIALIZED CPOT
!                                  WHEN READING EIGEN1S AND EIGEN2
!                13-MAR-2003 : DS: TUM GRAVITY MODEL INTRODUCED
!                11-JUN-2003 : HU: ACTIVATE DOTJ2 CORRECTION
!                14-JUL-2003 : HU: EGM96: PERMANENT TIDE CORRECTION CHANGED
!                                  FROM -4.201D-9 TO -4.173D-9 (IERS2000)
!                18-AUG-2003 : RD: CLOSE FILE
!                19-AUG-2003 : DS: CSR GRACE MODELS GGM01S and GGM01C introduced
!                19-AUG-2003 : DS: GFZ GRACE MODEL EIGEN-G1S introduced
!                22-FEB-2004 : DS: MERGE GPSWS AND LXWS VERSIONS
!                06-JAN-2004 : HU: DO NOT HANDLE PERMANENT TIDE, PARAM IZTID
!
!
! Remark:     F90-Version of old SR GETPOT.f
! Author:     H. Bock
! Created:    19-Oct-2004
! Last mod.:  31-Oct-2008
!
! Changes:    21-Dec-2004 HU: All EIGEN models: read ref epochs, handle
!                             CPOT(1) differently, dot also for SPOT
!             14-Apr-2005 HU: Use interface to alcerr
!             02-Jun-2006 HB: Put 'model_name' of GOCE-HPF-formatted files
!                             in TITLE-string => orbit model for STD-file
!                             Handle comment lines in GOCE-HPF-format
!             07-Aug-2006 HB: Some layout modifications for error messages
!                             Move application of C02 drift to the end
!             26-Aug-2006 HU: Compute mean pole, replace C21, S21
!             15-Nov-2006 HB: mpol=2 means IERS2003 conventions
!             17-Jan-2007 LP: GFZ EIGEN-CG3c and EIGEN-GL04 models introduced
!             31-OCT-2008 AJ: Apply dot terms correctly for GOCE-HPF-format
!
! REMARK:   Old SR GETPOT inherited to the new MODULE GRAVFIELD; structure gravField introduced
! AUTHOR:   L.PRANGE
! CREATED:  22-Jan-2007
! Last mod: 22-Jan-2007
!
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE s_alcerr
  USE m_maxdim, ONLY: MAXPOT
  USE s_exitrc
  USE f_djul
  USE s_opnfil
  USE s_gtflna
  USE s_opnerr
  USE s_meanpol
  USE s_st2tim
  USE f_nextline
  USE s_readKeys
  USE s_ckopti
  USE s_ckoptb
  USE d_const, ONLY: GM,AE,OMEGA
  USE d_satdynmod, ONLY:
  USE f_tstkey


  IMPLICIT NONE

  ! List of Parameters
  ! ------------------
! input:
  REAL(r8b)    :: tMjd   ! TIME OF REQUEST (BECAUSE OF C02 DOT)

! Local Variables
! ---------------
  CHARACTER(LEN=lineLength)      :: line
  CHARACTER(LEN=shortlineLength) :: dummy2
  CHARACTER(LEN=shortLineLength) :: gravFil
  CHARACTER(LEN=shortLineLength) :: modNam
  CHARACTER(LEN=shortLineLength) :: tidSys
  CHARACTER(LEN=timStrgLength)   :: timStr
  CHARACTER(LEN=shortLineLength) :: dummy
  CHARACTER(LEN=10)              :: error
  CHARACTER(LEN=8)               :: TIM
  CHARACTER(LEN=6)               :: RECKEY
  CHARACTER(LEN=shortlineLength) :: title ! Title

  REAL(r8b),DIMENSION(:),ALLOCATABLE :: csTim,sDot,cDot
  REAL(r8b)                          :: DC02,DC03,DC04,DC20,DS20,DC21,DS21
  REAL(r8b)                          :: C,S
  REAL(r8b)                          :: tmjd0, flat,refDat, thelp
  REAL(r8b)                          :: drftc, drfts
  REAL(r8b)                          :: xp, yp

  INTEGER(i4b) :: iostat, iac
  INTEGER(i4b) :: i,n,m
  INTEGER(i4b) :: iMax,iPot,izTid,iYear,imm,iDay,nTerm
  INTEGER(i4b) :: maxDeg,EIGVERS4
  INTEGER(i4b) :: lumped                       ! ESTIMATE LUMPED COEFFICIENTS? (0/1)
  INTEGER(i4b) :: lumped_n                     ! MAXIMUM DEGREE OF LUMPED COEFFICIENTS
  INTEGER(i4b) :: MDOT
  INTEGER(i4b) :: est_grav

! Declaration for readKeys
! ------------------------
  CHARACTER(LEN=keyValueLength),DIMENSION(:),POINTER :: keyValue
  INTEGER(i4b) :: irc
  INTEGER(i4b) :: irCode
  CHARACTER(LEN=7),  PARAMETER :: srName = 'getPot2'


! Change TMJD to a mean session (also in MJD format) if the dot coefficients
! of the a priori potential and the mean pole should refer to the mean session.
! ----------------------------------------------------------------------------
  thelp=tMjd
  MDOT = 0
  NULLIFY(keyValue)

  IF (tstKey('MEAN_DOT')) THEN
      CALL ckoptb(1,(/'MEAN_DOT'/),srName,                              &
                'Apply the dot-values for a mean session?',             &
                irCode,result1=MDOT)
  ENDIF

  IF (MDOT == 1) THEN
      tMjd = gravField%mjd
  ENDIF


! READ MAXIMUM DEGREE OF GEOPOTENTIAL COEFFICIENTS FROM INPUT PANEL
! -----------------------------------------------------------------
!
! est_grav=0
! IF (tstKey('UPPER_N')) THEN
!     CALL readKeys('UPPER_N', keyValue, irc)
!     CALL ckopti(1,'UPPER_N', keyValue, srName,                      &
!              'Earth potential upper limits, degree n=',irc,irCode,  &
!              maxVal=1,result1=gravField%maxDegree)
!     CALL readKeys('POT_EST', keyValue, irc)
!     CALL ckopti(1,'POT_EST', keyValue, srName,                      &
!              'Estimate gravity field coefficients?',irc,irCode,     &
!              maxVal=1,result1=est_grav)
!
!     IF (est_grav == 1 .and. (tstKey('POT_LUMPED'))) THEN
!                CALL readKeys('POT_LUMPED', keyValue, irc)
!                CALL ckopti(1,'POT_LUMPED', keyValue, srName,        &
!                        'Solve for lumped coefficients?',irc,irCode, &
!                        maxVal=1,result1=lumped)
!                CALL readKeys('POT_LUMPED_N', keyValue, irc)
!                CALL ckopti(1,'POT_LUMPED_N', keyValue, srName,      &
!                         'Lumped up to degree n=',irc,irCode,        &
!                        maxVal=1,result1=lumped_n)
!                IF (lumped==1) THEN
!                    IF (lumped_n > gravField%maxDegree) THEN
!                              gravField%maxDegree = lumped_n
!                    ENDIF
!                ENDIF
!     ENDIF
! ELSEIF (tstKey('MXPOTDEG')) THEN
!     CALL readKeys('MXPOTDEG', keyValue, irc)
!     CALL ckopti(1,'MXPOTDEG', keyValue, srName,                     &
!              'Earth potential upper limits, degree n=',irc,irCode,  &
!              maxVal=1,result1=gravField%maxDegree)
!
! ELSE
!     gravField%maxDegree = MAXPOT
! ENDIF

  IF (ASSOCIATED(keyValue)) DEALLOCATE(keyValue)

! OPEN FILE WITH EARTH POTENTIAL COEFFICIENTS
! -------------------------------------------
  CALL GTFLNA(1,'POTCOE ',gravField%source,IRC)
  CALL OPNFIL(LFNLOC,gravField%source,'OLD',' ','READONLY',' ',IOSTAT)
  CALL OPNERR(LFNERR,LFNLOC,IOSTAT,gravField%source,'GETPOT2')

  IF(gravField%maxDegree > MAXPOT) THEN
    WRITE(LFNERR,'(/,A,/,16X,A,/,16X,A,I4,/,16X,A,I4,/)') &
         ' *** SR GETPOT2: ORDER/DEGREE OF EARTH ',&
                         'POTENTIAL TERMS IS TOO LARGE',&
                         'REQUESTED ORDER/DEGREE   >=',gravField%maxDegree,&
                         'MAX. ORDER OF POTENTIAL   :',MAXPOT
    CALL EXITRC(2)
  ENDIF

! INITIALIZATION
! --------------
  IMAX=(gravField%maxDegree+1)*(gravField%maxDegree+2)/2
  NTERM=0
  IPOT=-99
  IZTID=-99
  EIGVERS4=0

! SET gravField%cApr(1)
  gravField%cApr(1)=1.D0

! READ TITLE LINE
! ---------------
  READ(LFNLOC,'(A80)') TITLE

! CHECK FORMAT
! ------------
! IZTID: 0: TIDE FREE   (C02 = -0.484165e-3)
!        1: ZERO TIDE   (C02 = -0.484169e-3)
  IF (TITLE(3:5) == 'GEM') THEN
    IPOT=1
    IZTID=0
  ELSEIF (TITLE(3:5) == 'JGM') THEN
    IPOT=2
    IZTID=1
  ELSEIF (TITLE(3:5) == 'GRI') THEN
    IPOT=3
    IZTID=0
  ELSEIF (TITLE(3:5) == 'EGM') THEN
    IPOT=4
    IZTID=0
  ELSEIF (TITLE(3:5) == 'TEG') THEN
    IPOT=5
    IZTID=1
  ELSEIF (TITLE(3:5) == 'EIG') THEN
    IPOT=6
    IZTID=0
  ELSEIF (TITLE(3:5) == 'GGM') THEN
    IPOT=7
    IZTID=1
  ELSEIF (TITLE(3:5) == 'TUM' .OR. TITLE(3:5) == 'MGM') THEN
    IPOT=8
    IZTID=0 ! ok?
  ELSEIF (TITLE(3:5) == 'BER') THEN
    IPOT=9
    IZTID=0

! 0. IPOT=0 --> New GOCE-HPF gravity field format
! -----------------------------------------------
! Read header of gravity field file
! ---------------------------------
  ELSE
    IF (TITLE(1:12) == 'product_type') THEN
      IPOT=0
      READ(TITLE,*)dummy,dummy2
      gravFil = ADJUSTL(dummy2)
      IF (gravFil /= 'gravity_field') THEN
        WRITE(lfnErr,'(A,A,/,16X,A,/,16X,A,A,/)')&
             '*** SR GETPOT2: The file: ',TRIM(gravField%source),&
                            'contains no gravity field information',&
                            'product type: ',TRIM(gravFil)
        CALL exitrc(2)
      ENDIF
    ENDIF
    DO
      line = nextline(lfnloc,1)
      IF (line(1:11) == 'end_of_head') EXIT
      IF (line(1:3)  == 'EOF')         EXIT
      IF (line(1:12) == 'product_type') THEN
        IPOT=0
        IF (IZTID==-99) IZTID = -1
        READ(line,*)dummy,dummy2
        gravFil = ADJUSTL(dummy2)
        IF (gravFil /= 'gravity_field') THEN
          WRITE(lfnErr,'(A,A,/,16X,A,/,16X,A,A,/)')&
               '*** SR GETPOT2: The file: ',TRIM(gravField%source),&
                              'contains no gravity field information',&
                              'product type: ',TRIM(gravFil)
          CALL exitrc(2)
        ELSE
          CYCLE
        ENDIF
      ENDIF
      IF (line(1:9) == 'modelname') THEN
        READ(line,*)dummy,dummy2
        modNam = ADJUSTL(dummy2)
        title  = ' '
        TITLE(3:18)=modnam

! GRIMC7 - test file
! ******************
        IF (modNam(1:4) == 'GRIM') IZTID=0
! ******************
        CYCLE
      ENDIF
      IF (line(1:22) == 'earth_gravity_constant') THEN
        READ(line,*)dummy2,gravField%gm
        CYCLE
      ENDIF
      IF (line(1:6) == 'radius') THEN
        READ(line,*)dummy2,gravField%ae
        CYCLE
      ENDIF
      IF (line(1:10) == 'max_degree') THEN
        READ(line,*)dummy2,maxDeg
        CYCLE
      ENDIF
      IF (line(1:6) == 'errors') THEN
        READ(line,*)dummy2,error
        CYCLE
      ENDIF
      IF (line(1:4) == 'norm') THEN
        CYCLE
      ENDIF
      IF (line(1:11) == 'tide_system') THEN
        READ(line(12:255),*)dummy2
        tidSys = ADJUSTL(dummy2)
        IF (tidSys == 'tide_free') THEN
          IZTID=0
        ELSEIF (tidSys == 'zero_tide') THEN
          IZTID=1
        ENDIF
        CYCLE
      ENDIF
    ENDDO

    IF (IPOT < 0.OR.line(1:3)=='EOF') THEN
      WRITE(LFNERR,'(A,/,16X,A,A32,/,16X,A,A20,/)')&
           ' *** SR GETPOT2: Unknown Earth gravity file format',&
                           'file  : ',gravField%source,&
                           'title : ',TITLE(1:20)
      CALL EXITRC(2)
    ENDIF

    IF (izTid < 0) THEN
      WRITE(lfnerr,'(A,/,16X,A,/,16X,A)')&
           ' *** SR GETPOT2: Unknown tide system for the ',&
                           'Earth gravity model in file: ',&
                            TRIM(gravField%source)
      CALL EXITRC(2)
    ENDIF

! Allocate csTim, cDot, sDot
! --------------------------
    ALLOCATE(csTim(iMax),stat=iac)
    CALL alcerr(iac,'csTim',(/iMax/),srName)
    ALLOCATE(cDot(iMax),stat=iac)
    CALL alcerr(iac,'cDot',(/iMax/),srName)
    ALLOCATE(sDot(iMax),stat=iac)
    CALL alcerr(iac,'sDot',(/iMax/),srName)
    cDot(:)=0.D0
    sDot(:)=0.D0

! Read coefficients
! -----------------
    DO
      C=0.D0
      S=0.D0
      line = nextline(lfnloc,1)
      IF (line(1:3) == 'EOF') EXIT
      IF (line(1:4) == 'gfc ') THEN
        READ(line,*)dummy,N,M,C,S
        I=N*(N+1)/2+1+M
      ENDIF
      IF (line(1:4) == 'gfct') THEN
        IF (error == 'no') THEN
          READ(line,*)dummy,N,M,C,S,csTIM
        ELSEIF (error == 'calibrated' .OR. error == 'formal') THEN
          READ(line,*)dummy,N,M,C,S,dummy,dummy,TIM
        ENDIF
        I=N*(N+1)/2+1+M
        timStr = TIM(1:4)//'-'//TIM(5:6)//'-'//TIM(7:8)//' 00:00:00'
        CALL ST2TIM(1,1,timStr,csTim(I))
      ENDIF
      IF (line(1:3) == 'dot') THEN
        READ(line,*)dummy,N,M,C,S
        I=N*(N+1)/2+1+M
      ENDIF
      IF(N > gravField%maxDegree.OR.M > gravField%maxDegree) CYCLE
      IF(N == 0.AND.M == 0) CYCLE
      NTERM=MAX0(N,M,NTERM)
!!!!      I=N*(N+1)/2+1+M
      IF (line(1:3) == 'dot') THEN
        cDot(I)=C
        sDot(I)=S
      ELSE
        gravField%cApr(I)=C
        gravField%sApr(I)=S
      ENDIF
    ENDDO
    DO I = 1,iMax
      IF (cDot(I) /= 0.D0 ) THEN
        gravField%cApr(I) = gravField%cApr(I) + cDot(I)*(tMjd - csTim(I))/365.25
      ENDIF
      IF (sDot(I) /= 0.D0 ) THEN
        gravField%sApr(I) = gravField%sApr(I) + sDot(I)*(tMjd - csTim(I))/365.25
      ENDIF
    ENDDO
    DEALLOCATE(csTim,stat=iac)
    DEALLOCATE(cDot,stat=iac)
    DEALLOCATE(sDot,stat=iac)
    GOTO 900
  ENDIF
! 1. IPOT=1 --> GEM-T3N
! ---------------------
  IF (IPOT == 1) THEN

! READ GM AND AE
    READ(LFNLOC,'(19X,F9.2,4X,F8.3)')gravField%gm,gravField%ae

! SET GM AND AE (SINCE THEY ARE ZERO IN THE GEMT3. FILE)
! ------------------------------------------------------
    gravField%gm=398600.4415*1.D9
    gravField%ae=6378.137*1.D3

! INPUT OF COEFFICIENTS
! ---------------------
    DO
      C=0.d0
      S=0.d0
      READ(LFNLOC,'(6X,2I2,2D15.8)',END=900) N,M,C,S
      IF(N > gravField%maxDegree.OR.M > gravField%maxDegree) CYCLE
      IF(N == 0.AND.M == 0) EXIT
      NTERM=MAX0(N,M,NTERM)
      I=N*(N+1)/2+1+M
      gravField%cApr(I)=C
      gravField%sApr(I)=S
    ENDDO

! 1. IPOT=2 --> JGM-3
! -------------------
  ELSEIF (IPOT == 2) THEN

! READ GM AND AE
! --------------
    READ(LFNLOC,'(/,20X,2E20.10,/)')gravField%gm,gravField%ae

! INPUT OF COEFFICIENTS
! ---------------------
    DO
      C=0.d0
      S=0.d0
      READ(LFNLOC,'(6X,2I2,2D21.14)',END=900) N,M,C,S
      IF(N > gravField%maxDegree.OR.M > gravField%maxDegree) CYCLE
      IF(N == 0.AND.M == 0) EXIT
      NTERM=MAX0(N,M,NTERM)
      I=N*(N+1)/2+1+M
      gravField%cApr(I)=C
      gravField%sApr(I)=S
    ENDDO

! 1. IPOT=3 --> GRIM5
! -------------------
  ELSEIF (IPOT == 3) THEN
    READ(LFNLOC,'(//,4E20.14,/,16X,F8.2,//)')gravField%ae,FLAT,gravField%gm,OMEGA,REFDAT
    READ(LFNLOC,'(9X,E21.14)')DC02,DC03,DC04
    IYEAR=NINT(REFDAT)
    TMJD0=DJUL(IYEAR,1,1.0D0)

! INPUT OF COEFFICIENTS
! ---------------------
    DO
      C=0.d0
      S=0.d0
      READ(LFNLOC,'(2I3,3X,2D21.14)',END=900) N,M,C,S
      IF(N > gravField%maxDegree.OR.M > gravField%maxDegree) CYCLE
      IF(N == 0 .AND. M == 0 .AND. C == 0.D0 .AND. S == 0.D0) EXIT

! APPLY C02, C03, C04 DRIFTS IN CASE OF GRIM5
! -------------------------------------------
      IF (N == 2 .AND. M == 0) C=C+DC02*(TMJD-TMJD0)/365.25D0
      IF (N == 3 .AND. M == 0) C=C+DC03*(TMJD-TMJD0)/365.25D0
      IF (N == 4 .AND. M == 0) C=C+DC04*(TMJD-TMJD0)/365.25D0
      NTERM=MAX0(N,M,NTERM)
      I=N*(N+1)/2+1+M
      gravField%cApr(I)=C
      gravField%sApr(I)=S
    ENDDO

! 1. IPOT=4 --> EGM96
! -------------------
  ELSEIF (IPOT == 4) THEN
    READ(LFNLOC,'(//,F9.2,6X,E15.3/,16X,F8.2,//)')gravField%ae,gravField%gm,REFDAT
    READ(LFNLOC,'(11X,E17.10,1X,E19.12)')DC20,DS20,DC21,DS21
    IYEAR=NINT(REFDAT)
    TMJD0=DJUL(IYEAR,1,1.0D0)

! INPUT OF COEFFICIENTS
! ---------------------
    DO
      C=0.d0
      S=0.d0
      READ(LFNLOC,'(2I4,2E20.12)',END=900) N,M,C,S
      IF(N > gravField%maxDegree.OR.M > gravField%maxDegree) CYCLE
      IF(N == 0.AND.M == 0) EXIT

! APPLY C20, C21, S21 DRIFTS IN CASE OF EGM96
! ------------------------------------------------------
      IF (N == 2 .AND. M == 0) THEN
        C = C + DC20*(TMJD-TMJD0)/365.25D0
      ENDIF
      IF (N == 2 .AND. M == 1) THEN
        C=C+DC21*(TMJD-TMJD0)/365.25D0
        S=S+DS21*(TMJD-TMJD0)/365.25D0
      ENDIF
      NTERM=MAX0(N,M,NTERM)
      I=N*(N+1)/2+1+M
      gravField%cApr(I)=C
      gravField%sApr(I)=S
    ENDDO

! 1. IPOT=5 or 8 --> CSR Models: TEG4, GGM01C, GGM01S
! ---------------------------------------------------
  ELSEIF (IPOT == 5 .OR. IPOT == 7) THEN

! READ GM AND AE, REFERENCE EPOCH
! -------------------------------
    READ(LFNLOC,'(/,20X,3E20.10,/)')gravField%gm,gravField%ae,REFDAT

! INPUT OF COEFFICIENTS
! ---------------------
    DO
      C=0.d0
      S=0.d0
      READ(LFNLOC,'(6X,2I3,2D21.14)',END=850) N,M,C,S
      IF(N > gravField%maxDegree.OR.M > gravField%maxDegree) CYCLE
      IF(N == 0.AND.M == 0) EXIT
      NTERM=MAX0(N,M,NTERM)
      I=N*(N+1)/2+1+M
      gravField%cApr(I)=C
      gravField%sApr(I)=S
    ENDDO

! APPLY TIME VARIABLE PART FOR C20 AND C21, S21
! ---------------------------------------------
850 CONTINUE
    IYEAR=NINT(REFDAT)
    TMJD0=DJUL(IYEAR,1,1.0D0)+REFDAT-IYEAR

    gravField%cApr(4)=gravField%cApr(4)+1.162755D-11*(TMJD-TMJD0)/365.25D0

    IF (TITLE(3:7).EQ.'TEG-4') THEN
      gravField%cApr(5)=-2.20D-10
      gravField%sApr(5)=14.51D-10
    END IF
    gravField%cApr(5)=gravField%cApr(5)-0.337000D-11*(TMJD-51544.D0)/365.25D0
    gravField%sApr(5)=gravField%sApr(5)+1.606000D-11*(TMJD-51544.D0)/365.25D0

! 1. IPOT=6 --> EIGEN
! ---------------------
  ELSEIF (IPOT == 6) THEN
! READ GM AND AE
! --------------
    IF (TITLE(3:9)  ==  'EIGEN1S') THEN
      READ(LFNLOC,'(/,6X,1E16.10,1X,1E16.10,1X,I4,I2,I2,13(/))') &
           gravField%gm,gravField%ae,IYEAR,IMM,IDAY
    ELSEIF (TITLE(3:9)  ==  'EIGEN-2') THEN
      READ(LFNLOC,'(///,6X,1E16.10,1X,1E16.10,1X,I4,I2,I2,16(/))') &
           gravField%gm,gravField%ae,IYEAR,IMM,IDAY
    ELSEIF (TITLE(3:9)  ==  'EIGEN-3p') THEN
      READ(LFNLOC,'(///,6X,1E16.10,1X,1E16.10,1X,I4,I2,I2,16(/))') &
           gravField%gm,gravField%ae,IYEAR,IMM,IDAY
    ELSEIF (TITLE(3:12) ==  'EIGEN-G01S' .OR. &
            TITLE(3:16) ==  'EIGEN-GRACE01S') THEN
      READ(LFNLOC,'(////,6X,1E16.10,1X,1E16.10,1X,I4,I2,I2,/)') &
           gravField%gm,gravField%ae,IYEAR,IMM,IDAY
    ELSEIF (TITLE(3:13) ==  'EIGEN_CG03c') THEN
      READ(LFNLOC,'(65X,I4,I2,I2,////,6X,1E16.10,1X,1E16.10,/)') &
           IYEAR,IMM,IDAY,gravField%gm,gravField%ae
    ELSEIF (TITLE(3:13) ==  'EIGEN-GL04C') THEN
      READ(LFNLOC,'(23(/),30X,1E16.10,/,30X,1E16.10,5(/),30X,I4,I2,I2,3(/))') &
           gravField%gm,gravField%ae,IYEAR,IMM,IDAY
      EIGVERS4=1
    ELSEIF (TITLE(3:14) ==  'EIGEN-GL04S1') THEN
      READ(LFNLOC,'(30(/),30X,1E16.10,/,30X,1E16.10,5(/),30X,I4,I2,I2,3(/))') &
           gravField%gm,gravField%ae,IYEAR,IMM,IDAY
      EIGVERS4=1
    ELSE
      WRITE(LFNERR,'(A,/,16X,A,A32,/,16X,A,A20,/)') &
           ' *** SR GETPOT2: Unknown EIGEN gravity file format',&
                           'file  : ',gravField%source,&
                           'title : ',TITLE(1:20)
      CALL EXITRC(2)
    ENDIF
    TMJD0=DJUL(IYEAR,IMM,DBLE(IDAY))

! INPUT OF COEFFICIENTS
! ---------------------
    DO
      C=0.d0
      S=0.d0
      IF (EIGVERS4==0) THEN
         READ(LFNLOC,'(A6,2I5,2(1X,1E18.12))',END=900) RECKEY,N,M,C,S
      ELSE
         READ(LFNLOC,'(A3,2(2X,1I3),2(1X,1E18.12))',END=900) RECKEY,N,M,C,S
      ENDIF

! APPLY ZERO TIDE FOR C20 OF EIGEN1S (-4.201D-9,k=0.30190)
! --------------------------------------------------------
      IF(N > gravField%maxDegree.OR.M > gravField%maxDegree) CYCLE
      NTERM=MAX0(N,M,NTERM)
      I=N*(N+1)/2+1+M

      IF (EIGVERS4==0) THEN
         IF ((RECKEY(1:6)=='GRCOEF') .OR. (RECKEY(1:6)=='GRCOF2')) THEN
            gravField%cApr(I)=C
            gravField%sApr(I)=S
         ELSEIF (RECKEY(1:6)=='GRDOTA') THEN
            DRFTC=C*(TMJD-TMJD0)/365.25D0
            DRFTS=S*(TMJD-TMJD0)/365.25D0
            IF (I > 1) THEN
               gravField%cApr(I)=gravField%cApr(I)+DRFTC
               gravField%sApr(I)=gravField%sApr(I)+DRFTS
            ENDIF
         ENDIF
      ELSE
         IF (RECKEY(1:3)=='gfc') THEN
            gravField%cApr(I)=C
            gravField%sApr(I)=S
         ELSEIF(RECKEY(1:3)=='dot') THEN
            DRFTC=C*(TMJD-TMJD0)/365.25D0
            DRFTS=S*(TMJD-TMJD0)/365.25D0
            IF (I > 1) THEN
               gravField%cApr(I)=gravField%cApr(I)+DRFTC
               gravField%sApr(I)=gravField%sApr(I)+DRFTS
            ENDIF
         ENDIF
      ENDIF
    ENDDO

!
! 1. IPOT=8 --> IAPG MODELS
! -------------------------
  ELSEIF (IPOT.EQ.8) THEN

! READ GM AND AE
! --------------
    IF (TITLE(3:5) .EQ. 'MGM' .OR. TITLE(3:5) .EQ. 'TUM') THEN
      READ(LFNLOC,'(/,9X,2D25.16,D16.8)') gravField%gm,gravField%ae,REFDAT
    ELSE
      WRITE(LFNERR,'(A,/,16X,A,A32,/,16X,A,A20,/)') &
            ' *** SR GETPOT2: Unknown Earth gravity file format',&
                            'file  : ',gravField%source,&
                            'title : ',TITLE(1:20)
      CALL EXITRC(2)
    END IF
    IYEAR=NINT(REFDAT)
    TMJD0=DJUL(IYEAR,1,1.0D0)
!
! INPUT OF COEFFICIENTS
! ---------------------
    DO
      C=0.d0
      S=0.d0
      READ(LFNLOC,'(A1,2X,2I4,2D25.16)',END=900) RECKEY,N,M,C,S
      IF(N > gravField%maxDegree .OR. M > gravField%maxDegree) CYCLE
      NTERM=MAX0(N,M,NTERM)
      I=N*(N+1)/2+1+M
      IF(RECKEY(1:1)=='C') THEN
          gravField%cApr(I)=C
          gravField%sApr(I)=S
      ELSEIF(RECKEY(1:1)=='D') THEN
        DRFTC=C*(TMJD-TMJD0)/365.25D0
        DRFTS=S*(TMJD-TMJD0)/365.25D0
        IF (I > 1) THEN
          gravField%cApr(I)=gravField%cApr(I)+DRFTC
          gravField%sApr(I)=gravField%sApr(I)+DRFTS
        ENDIF
      ENDIF
    ENDDO

! 1. IPOT=9 --> BERN MODEL
! ------------------------
  ELSEIF (IPOT == 9) THEN
    modNam = 'BERN-1'
    READ(LFNLOC,'(///,30X,E16.10,/,30X,E16.10,//////)')gravField%gm,gravField%ae

! INPUT OF COEFFICIENTS
! ---------------------
    DO
      C=0.d0
      S=0.d0
      READ(LFNLOC,'(A6,2I5,2(1X,1E18.12))',END=900) RECKEY,N,M,C,S
      IF(N > gravField%maxDegree.OR.M > gravField%maxDegree) CYCLE
      I=N*(N+1)/2+1+M
      IF (RECKEY(1:6)=='GEOCOE') THEN
         gravField%cApr(I) = C
         gravField%sApr(I) = S
      ENDIF
      NTERM=MAX0(N,M,NTERM)
    ENDDO

  ENDIF

900 CONTINUE

! APPLY C02 DRIFT IN CASE OF JGM3 AND TEG4
! J02 DRIFT = 2.6*10**-11 1/YEAR (REF TIME 1-1-1986 = MJD 46431.0)
! Activated on June 11, 2003
! -----------------------------------------------------------------
  IF (IPOT == 2.OR.IPOT == 5) THEN
    DC02=(2.6D-11/DSQRT(5.0D0))*(TMJD-46431.0D0)/365.25D0
    gravField%cApr(4)=gravField%cApr(4)+DC02
  ENDIF

! CHANGE MEAN POLE ACCORDING TO IERS2003 (if selected)
! ----------------------------------------------------
  IF (gravField%meanPole) THEN
    CALL MEANPOL(2,TMJD,XP,YP)
    gravField%cApr(5)= DSQRT(3D0)*XP*gravField%cApr(4) - XP*gravField%cApr(6) + YP*gravField%sApr(6)
    gravField%sApr(5)=-DSQRT(3D0)*YP*gravField%cApr(4) - YP*gravField%cApr(6) - XP*gravField%sApr(6)
  ENDIF

! CLOSE FILE
! ----------
  CLOSE(LFNLOC)

! APPLY MONTHLY MEANS OF GRACE TIME VARIATIONS
! --------------------------------------------
  CALL getTimeVar(modNam)

! RESCALE gravity field coefficients to the constants of the BERNESE software
! ---------------------------------------------------------------------------

  DO N=0,gravField%maxDegree
     DO M=0,N
        I=N*(N+1)/2+1+M
        gravField%cApr(I)=gravField%cApr(I)*gravField%gm/GM*(gravField%ae/AE)**N
        gravField%sApr(I)=gravField%sApr(I)*gravField%gm/GM*(gravField%ae/AE)**N
     ENDDO
  ENDDO

! Set GM and AE to the values of the BERNESE constants file
! ---------------------------------------------------------
  gravField%gm=GM
  gravField%ae=AE

  IF (IZTID==0) THEN
     gravField%tideFree = .true.
  ELSE
     gravField%tideFree = .false.
  END IF

  gravField%maxDegree = NTERM
  gravField%title = TITLE
  tMjd=thelp
  IF (MDOT==0) THEN
            gravField%mjd = int(tMjd)
  ENDIF

  RETURN

END SUBROUTINE getPot2

!************************************************************************
!
! Write Gravity Field into a File
! -------------------------------
!************************************************************************
!
!
SUBROUTINE gravStore2(neq)
!
  USE d_neq,    ONLY: t_neq
  USE d_par,    ONLY: partype_periodiccos, partype_periodicdrift, &
                      partype_periodicoffset, partype_periodicsin

  USE p_addneq, ONLY: opt, comstat
  USE s_opnfil
  USE s_opnerr
  USE s_readkeys
  USE f_ikf

  IMPLICIT NONE

  ! List of Parameters
  ! ------------------
  TYPE(t_neq)       :: neq

  ! Local Variables
  ! ---------------
  LOGICAL                        :: onlyUp, found
  INTEGER(i4b)                   :: ios, irc, ideg, iord, ii, iii
  INTEGER(i4b)                   :: npotmx, iType
  REAL(r8b)                      :: cVal, sVal, cSig, sSig, omega0, mjd
  CHARACTER(LEN=6), DIMENSION(6) :: keyStr
  CHARACTER(LEN=4)               :: ynStr
  CHARACTER(LEN=keyValueLength), DIMENSION(:), POINTER  :: keyValue

  ! Easy Return
  ! -----------
  NULLIFY(keyValue)
  IF ( opt%gravout == '' ) RETURN

  CALL opnfil(lfnres,opt%gravout,'UNKNOWN',' ', ' ',' ',ios)
  CALL opnerr(lfnerr,lfnres,ios,opt%gravout,'SR gravstore2')

  ! Decide whether to save only updates
  ! -----------------------------------
  onlyUp = .FALSE.
  CALL readKeys('ONLYUP', keyValue, irc)
  IF (irc == 0) THEN
    IF (keyValue(1) == "1") THEN
      onlyUp = .TRUE.
    ENDIF
  END IF
  DEALLOCATE(keyValue)

  ! Easy Return
  ! -----------
  IF ( opt%gravout == '' ) RETURN

  npotmx = 0
  DO iii = 1, neq%misc%npar
     IF (neq%par(iii)%locq(1) == 13) THEN
        IF (neq%par(iii)%locq(5) > npotmx) THEN
            npotmx = neq%par(iii)%locq(5)
        ENDIF
     ENDIF
  ENDDO

  ! Write header
!  WRITE(lfnres,'(2X,A6,//,A,1X,i3,/,A,E18.12,1X,A,E18.12,/,A6,2(4X,A1),2(1X,A3,15X),2(1X,A6,12X))') &
!        'BERN-1','MaxDegree:',npotmx,'GM: ',GM,'AE: ',AE,'info  ','n','m','Cnm','Snm','SigCnm','SigSnm'

  IF (onlyUp) THEN
    WRITE(lfnres,'(2X,A6,//,A,18X,A,/,A,21X,A,/,A,8X,E16.10,/,A,24X,E16.10)') &
          'BERN-1','product_type','gravity_field','modelname','time variations of BERN-1','earth_gravity_constant',GM,'radius',AE
  ELSE
    WRITE(lfnres,'(2X,A6,//,A,18X,A,/,A,21X,A,/,A,8X,E16.10,/,A,24X,E16.10)') &
          'BERN-1','product_type','gravity_field','modelname','BERN-1','earth_gravity_constant',GM,'radius',AE
  ENDIF

  WRITE(lfnres,'(A,20X,i3,/,A,/,A,/,A,//,A6,2(4X,A1),2(1X,A3,15X),2(1X,A6,12X))') &
       'max_degree',npotmx,'norm','tide_free','reference_date','key   ','n','m','Cnm','Snm','SigCnm','SigSnm'

  keyStr(1) = 'GEOCOE'
  keyStr(2) = 'GRDOTA'
  keyStr(3) = 'GRC1AA'
  keyStr(4) = 'GRS1AA'
  keyStr(5) = 'GRC2AA'
  keyStr(6) = 'GRS2AA'
  omega0 = 2.0 * pi / 365.25

  ideg = 0
  iord = 0
  IF (onlyUp) THEN
    cVal = 0.0
  ELSE
    cVal = 1.0
  ENDIF
  sVal = 0.d0
  cSig = 0.d0
  sSig = 0.d0
  ynStr = 'nnnn'
  mjd = 0.0
  WRITE(lfnres,'(A6,2i5,2(1x,E18.12),2(1x,E18.12),1x,a4,1x,f7.1)') &
        keyStr(1), ideg, iord, cVal, sVal, cSig, sSig, ynStr, mjd

  DO ideg = 1,npotmx
    DO iord = 0, ideg
      DO iType = 1, 6
        cVal  = 0.0
        sVal  = 0.0
        cSig  = 0.0
        sSig  = 0.0
        ynStr = 'nnnn'
        found = .FALSE.
        mjd   = 0.0
        DO ii = 1, neq%misc%npar
          IF ( neq%par(ii)%locq(1) == 13    .AND. &
               neq%par(ii)%locq(5) == ideg  .AND. &
               neq%par(ii)%locq(6) == iord ) THEN

            IF (iType            == 1                       .AND. &
                neq%par(ii)%type /= ''                      .AND. &
                neq%par(ii)%type /= parType_periodicOffset) THEN
              CYCLE
            ENDIF
            IF (iType            == 2                      .AND. &
                neq%par(ii)%type /= parType_periodicDrift) THEN
              CYCLE
            ENDIF
            IF ( (iType == 3 .OR. iType == 5)            .AND. &
                neq%par(ii)%type /= parType_periodicCos) THEN
              CYCLE
            ENDIF
            IF ( (iType == 4 .OR. iType == 6)            .AND. &
                neq%par(ii)%type /= parType_periodicSin) THEN
              CYCLE
            ENDIF
            IF ( (iType == 3 .OR. iType == 4)  .AND. &
                neq%par(ii)%omega /= omega0)   THEN
              CYCLE
            ENDIF

            found = .TRUE.

            IF (neq%par(ii)%type /= '') THEN
              mjd = neq%par(ii)%time%mean
            ENDIF

            IF (neq%par(ii)%locq(4) == 1) THEN
              IF (onlyUp) THEN
                cVal = neq%xxx(ii)
              ELSE
                cVal = neq%par(ii)%x0 + neq%xxx(ii)
              ENDIF
              cSig = cSig + comstat%rms * SQRT( ABS(neq%anor(ikf(ii,ii))) )
              ynStr(1:1) = 'y'
              ynStr(3:3) = 'y'
            ELSE
              IF (onlyUp) THEN
                sVal = neq%xxx(ii)
              ELSE
                sVal = neq%par(ii)%x0 + neq%xxx(ii)
              ENDIF
              sSig = sSig + comstat%rms * SQRT( ABS(neq%anor(ikf(ii,ii))) )
              ynStr(2:2) = 'y'
              ynStr(4:4) = 'y'
            ENDIF
          ENDIF
        ENDDO
        IF (iType == 1 .OR. found) THEN
          WRITE(lfnres, &
                '(A6,2i5,2(1x,E18.12),2(1x,E18.12),1x,a4,1x,f7.1)') &
                keyStr(iType), ideg, iord, cVal, sVal, cSig, sSig, ynStr, mjd
        ENDIF
      ENDDO
    ENDDO
  ENDDO
  CLOSE(lfnres)


END SUBROUTINE gravStore2





!************************************************************************
!
! Get Gravity Field Time Variations
! ---------------------------------
!************************************************************************


SUBROUTINE getTimeVar(modNam)
  USE s_exitrc
  USE s_gtflna
  USE s_opnfil
  USE s_opnerr
  USE f_nextline

  IMPLICIT NONE

  ! List of Parameters
  ! ------------------
  ! input:
  CHARACTER(LEN=shortLineLength) :: modNam

  ! Local Variables
  ! ---------------
  CHARACTER(LEN=lineLength)      :: line
  CHARACTER(LEN=fileNameLength)  :: fileName
  CHARACTER(LEN=shortLineLength) :: title, model
  CHARACTER(LEN=shortLineLength) :: gravFil
  CHARACTER(LEN=shortLineLength) :: dummy, dummy2
  CHARACTER(LEN=10)              :: error
  INTEGER(i4b)                   :: iShift, iShift2
  INTEGER(i4b)                   :: irc
  INTEGER(i4b)                   :: ios
  INTEGER(i4b)                   :: ind
  INTEGER(i4b)                   :: n, m
  REAL(r8b)                      :: c, s
  REAL(r8b)                      :: ec, es

  ! Get filename
  ! ------------
  CALL gtflna(0,'POTVAR',fileName,irc)

  ! Something to do ?
  ! -----------------
  IF (irc /= 0) RETURN

  ! Open file with monthly means
  ! ----------------------------
  CALL opnfil(lfnloc,fileName,'OLD',' ', 'READONLY',' ',ios)
  CALL opnerr(lfnerr,lfnloc,ios,fileName,'sr getTimeVar')

  ! Read title line
  ! ---------------
  READ(lfnloc,'(A80)') title

  ! Check basic format issues
  ! -------------------------
  IF (title(1:12) == 'product_type') THEN
    iShift  = 0
    iShift2 = 0
    READ(title,*)dummy,dummy2
  ELSEIF (title(3:5) == 'BER') THEN
    iShift  = 7
    iShift2 = 2
    READ(lfnloc,'(/,A80)') title
    READ(title,*)dummy,dummy2
  ELSE
    WRITE(lfnErr,'(A,A,/,19X,A,/)')                                          &
         '*** sr getTimeVar: The format of the file ',TRIM(fileName),        &
                           'containing the gravity field variations is unknown'
    CALL exitrc(2)
  ENDIF

  ! Check product line
  ! ------------------
  gravFil = ADJUSTL(dummy2)
  IF (gravFil /= 'gravity_field') THEN
    WRITE(lfnErr,'(A,A,/,19X,A,/,19X,A,A,/)')&
         '*** sr getTimeVar: The file ',TRIM(fileName),&
                           'contains no gravity field information.',&
                           'Product type: ',TRIM(gravFil)
    CALL exitrc(2)
  ENDIF

  ! Read model line
  ! ---------------
  READ(lfnloc,'(A80)') model

  ! Check basic model issues
  ! ------------------------
  IF (model(1:9) /= 'modelname') THEN
    WRITE(lfnErr,'(A,A,/,19X,A,/)')                                          &
         '*** sr getTimeVar: The format of the file ',TRIM(fileName),        &
                           'containing the gravity field variations is unknown'
    CALL exitrc(2)
  ELSE

    ! check for time variations
    IF (model(24+iShift:38+iShift) /= 'time variations') THEN
      WRITE(lfnErr,'(A,A,/,19X,A,/)')           &
           '*** sr getTimeVar: The file ',TRIM(fileName), &
                             'does not contain gravity field time variations.'
      CALL exitrc(2)
    ENDIF

    ! check for model consistency
    IF (model(43+iShift:53+iShift2) /= TRIM(modNam)) THEN
      WRITE(lfnErr,'(A,/,19X,A,A,/)')                               &
           '*** sr getTimeVar: The time variations do not correspond with', &
                             'the static gravity field ',TRIM(modNam)
      CALL exitrc(2)
    ENDIF

  ENDIF

  ! Read remaining header information
  ! ---------------------------------
  DO
    line = nextline(lfnloc,1)
    IF (line(1:11) == 'end_of_head' .OR.  &
        line(1:11) == 'key       n') EXIT
  ENDDO

  ! Read and apply the coefficients of time variability
  ! ---------------------------------------------------
  DO
    c = 0.d0
    s = 0.d0
    line = nextline(lfnloc,1)
    IF (line(1:3) == 'EOF') EXIT
    IF (line(1:4) == 'gfc ') THEN
      READ(line,*)dummy,n,m,c,s
    ELSEIF (line(1:4) == 'GEOC') THEN
      READ(line,*)dummy,n,m,c,s,ec,es
    ENDIF
    IF (n > gravField%maxDegree .OR. m > gravField%maxDegree) CYCLE

    ! Update the coefficients
    ind = n*(n+1)/2+1+m
    gravField%cApr(ind) = gravField%cApr(ind) + c
    gravField%sApr(ind) = gravField%sApr(ind) + s
  ENDDO

  ! Close file with monthly means
  ! -----------------------------
  CLOSE(lfnloc)

END SUBROUTINE getTimeVar



END MODULE
