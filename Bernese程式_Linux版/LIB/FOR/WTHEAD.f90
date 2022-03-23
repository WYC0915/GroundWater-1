  MODULE s_wthead
  CONTAINS

! -------------------------------------------------------------------------
! Bernese GNSS Software Version 5.2
! -------------------------------------------------------------------------

  SUBROUTINE wtHead(filHed,meaTyp,nDiff,nFreq,nEpoch,nSatel,&
       cSess,iDeltt,timRef,campgn,title,crDate,&
       crTime,irMark,nEpFlg,iFrmat,&
       staNam,recTyp,antTyp,irUnit,iAnten,&
       oprNam,posEcc,iClock,numSat,numObs,numMrk,&
       numAmb,ambSat,ambIep,ambWlf,&
       ambig,ambCls,USEGEOS,GOBSDEF)

! -------------------------------------------------------------------------
! Purpose:   Wrapper for old SR WTHEAD to new SR WTHEAD2
!
! Author:    H.Bock
!
! Created:   16-Jul-2002
!
! Changes:   30-Jul-2002 HU: Use interface for alcerr
!            08-Sep-2003 HU: antnam, recnam, oprnam chr16 -> chr20
!            26-Jan-2011 LP: Satellite-specific observation types
!            05-Mar-2012 RD: Use WTHEAD as module now
!            26-Apr-2012 LP: Loop over obstyp changed (4->8)
!
! Copyright: Astronomical Institute
!            University of Bern
!            Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b,r8b
  USE d_gpsObs, ONLY: t_obsHead
  USE d_rinex3, ONLY: t_gobsdef
  USE s_alcerr
  USE s_wthead2
  IMPLICIT NONE
  INTEGER(i4b)    :: mxcSat,mxcAmb

! List of parameters
! ------------------
! input:
  CHARACTER(LEN=*) :: filHed                    ! Header file name
  INTEGER(i4b)     :: meaTyp                    ! Measurement type
  INTEGER(i4b)     :: nDiff                     ! Zero- or Single-differences
  INTEGER(i4b)     :: nFreq                     ! Number of frequencies
  INTEGER(i4b)     :: nEpoch                    ! Number of epochs
  INTEGER(i4b)     :: nSatel                    ! Number of satellites
  CHARACTER(LEN=4 ),DIMENSION(2)      :: csess  ! Session defintion
  INTEGER(i4b)     :: iDeltt                    ! Observation interval (sec)
  REAL(r8b)        :: timRef                    ! Reference epoch
  CHARACTER(LEN=16) :: campgn                   ! Campaign
  CHARACTER(LEN=53) :: title                    ! File title
  CHARACTER(LEN=9 ),DIMENSION(2)      :: crDate ! Creation(1) and modif.(2) date
  CHARACTER(LEN=5 ),DIMENSION(2)      :: crTime ! Creation(1) and modif.(2) time
  INTEGER(i4b)     :: irMark                    ! Remark number
  INTEGER(i4b)     :: nEpFlg                    ! Number of flagged epochs
  INTEGER(i4b)     :: iFrmat                    ! File format number
  CHARACTER(LEN=*),DIMENSION(2)       :: staNam ! Station names
  CHARACTER(LEN=20),DIMENSION(2)      :: recTyp ! Receiver types
  CHARACTER(LEN=20),DIMENSION(2)      :: antTyp ! Antenna types
  INTEGER(i4b),DIMENSION(2)           :: irUnit ! Receiver unit numbers
  INTEGER(i4b),DIMENSION(2)           :: iAnten ! Receiver antenna numbers
  CHARACTER(LEN=20),DIMENSION(2)      :: oprNam ! Operator name
  REAL(r8b),DIMENSION(3,2)            :: posecc ! Position eccentricities
  INTEGER(i4b),DIMENSION(2)           :: iClock ! Type of clock parameters
  INTEGER(i4b),DIMENSION(mxcSat)      :: numSat ! Satellite numbers (PRN)
  INTEGER(i4b),DIMENSION(mxcSat,2)    :: numObs ! Number of observations used
  INTEGER(i4b),DIMENSION(mxcSat,2)    :: numMrk ! Number of observations marked
  INTEGER(i4b)     :: numAmb                    ! Total number of ambiguities
  INTEGER(i4b),DIMENSION(*)           :: ambSat ! Satellite numbers (PRN)
  INTEGER(i4b),DIMENSION(*)           :: ambIep ! Ambiguity starting epoch number
  INTEGER(i4b),DIMENSION(mxcAmb,2)    :: ambWlf ! Wavelength factors
  REAL(r8b),DIMENSION(mxcAmb,3)       :: ambig  ! Ambiguities
  INTEGER(i4b),DIMENSION(mxcAmb,3)    :: ambCls ! Ambiguity cluster
  type(t_gobsdef),OPTIONAL            :: GOBSDEF! GIOVE external observation selection information
  INTEGER(i4b),OPTIONAL               :: USEGEOS! Use GIOVE external observation selection

! List of functions
! -----------------
! Local types
! -----------

! Local parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER :: srName = 'wtHead'

! Local Variables
! ---------------
  TYPE(t_obsHead) :: head

  CHARACTER(LEN=6):: mxnSat,mxnAmb

  INTEGER(i4b)    :: iac
  INTEGER(i4b)    :: iSat
  INTEGER(i4b)    :: iAmb
  INTEGER(i4b)    :: iSta
  INTEGER(i4b)    :: usealternative
  INTEGER(i4b)    :: igeos
  INTEGER(i4b)    :: indgeos
  INTEGER(i4b)    :: obs
  INTEGER(i4b)    :: satnumg



  COMMON/MCMSAT/mxcSat,mxnSat
  COMMON/MCMAMB/mxcAmb,mxnAmb

! Transform old variables into head-structure
! -------------------------------------------
  head%meaTyp = meaTyp
  head%nDiff  = nDiff
  head%nFreq  = nFreq
  head%nEpoch = nEpoch
  head%nSatel = nSatel

  head%cSess(:)      = cSess(:)
  head%iDeltt        = iDeltt
  head%timRef        = timRef
  head%campgn        = campgn
  head%title         = title
  head%crDate(:)     = crDate(:)
  head%crTime(:)     = crTime(:)
  head%irMark        = irMark
  head%nEpFlg        = nEpFlg
  head%iFrmat        = iFrmat
  head%sta(:)%staNam = staNam(:)
  head%sta(:)%recTyp = recTyp(:)
  head%sta(:)%antTyp = antTyp(:)
  head%sta(:)%irUnit = irUnit(:)
  head%sta(:)%iAnten = iAnten(:)
  head%sta(:)%oprNam = oprNam(:)
  head%sta(:)%iClock = iClock(:)
  DO iSta = 1,2
    head%sta(iSta)%posEcc(:) = posEcc(:,iSta)
  ENDDO

! Allocate memory
! ---------------
  ALLOCATE(head%sat(head%nSatel),stat=iac)
  CALL alcerr(iac,'head%sat',(/head%nSatel/),srName)

  head%sat(1:head%nSatel)%numSat = numSat(1:head%nSatel)
  DO iSat = 1,head%nSatel
    head%sat(iSat)%numObs(:) = numObs(iSat,:)
    head%sat(iSat)%numMrk(:) = numMrk(iSat,:)

    usealternative=0
    indgeos=0

! Attention: PRESENT-test works only for MODULES, not for SUBROUTINES!!!
    IF ((PRESENT(USEGEOS)).AND.(PRESENT(GOBSDEF))) THEN
     IF ((USEGEOS==1).AND.(gobsdef%norec>0)) THEN
      DO igeos=1,gobsdef%norec
       satnumg=gobsdef%sat(igeos)%sysnum*100+gobsdef%sat(igeos)%satnum

!       WRITE(*,*)'igeos, satnum=', igeos, satnumg

       IF (satnumg==numSat(isat)) THEN
          usealternative=1
          indgeos=igeos
          EXIT
       ENDIF
      ENDDO
     ENDIF
    ENDIF

    DO obs = 1,8
     IF (usealternative==0) head%sat(iSat)%obstyp(obs) = '   '
     IF (usealternative==1) head%sat(iSat)%obstyp(obs) =       &
         gobsdef%sat(indgeos)%obstyp(obs)
    ENDDO

  ENDDO
  head%numAmb = numAmb

! Allocate memory
! ---------------
  ALLOCATE(head%ambigu(numAmb),stat=iac)
  CALL alcerr(iac,'head%ambigu',(/numAmb/),srName)

  DO iAmb = 1,head%numAmb
    head%ambigu(iAmb)%ambSat    = ambSat(iAmb)
    head%ambigu(iAmb)%ambIep    = ambIep(iAmb)
    head%ambigu(iAmb)%ambWlf(:) = ambWlf(iAmb,:)
    head%ambigu(iAmb)%ambigu(:) = ambig (iAmb,:)
    head%ambigu(iAmb)%ambCls(:) = ambCls(iAmb,:)
  ENDDO

! Write header with new SR wtHead2
! -------------------------------
  CALL wtHead2(filHed,head)

! Deallocate memory
! -----------------
  DEALLOCATE(head%sat)
  DEALLOCATE(head%ambigu)

  RETURN
  END SUBROUTINE wtHead

  END MODULE
