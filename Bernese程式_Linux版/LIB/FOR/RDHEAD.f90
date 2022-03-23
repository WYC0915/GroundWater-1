MODULE s_RDHEAD
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE rdHead(filHed,meaTyp,nDiff,nFreq,nEpoch,nSatel,&
       cSess,iDeltt,timRef,campgn,title,crDate,&
       crTime,irMark,nEpFlg,iFrmat,&
       staNam,recTyp,antTyp,irUnit,iAnten,&
       oprNam,posEcc,iClock,numSat,numObs,numMrk,&
       numAmb,ambSat,ambIep,ambWlf,&
       ambig,ambCls,readgeos,usegeos,gobsdef)
! -------------------------------------------------------------------------
! Purpose:   Wrapper for old SR RDHEAD to new SR RDHEAD2
!
! Author:    H.Bock
!
! Created:   15-Jul-2002
!
! Changes:   30-Jul-2002 HU: Copy only infos for available stations
!            02-Aug-2002 HB: Do not initialize common blocks
!            17-Feb-2003 LM: Use m_maxdim
!            15-May-2003 HU: Initialize structures
!            08-Sep-2003 HU: antnam, recnam, oprnam chr16 -> chr20
!            28-Jun-2005 MM: Unused variables removed
!            26-Jan-2011 LP: Sat.-specific obstypes, format 6
!            26-Apr-2012 LP: Loop over obstyp changed (4->8)
!
! Copyright: Astronomical Institute
!            University of Bern
!            Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_gpsObs, ONLY: t_obsHead,init_obshead
  USE d_rinex3, ONLY: t_gobsdef
  USE s_dimtst
  USE s_rdhead2
  USE s_gobsdef,ONLY: init_geos, setgeos
  IMPLICIT NONE

  INTEGER(i4b)    :: mxcSat,mxcAmb

! List of parameters
! ------------------
! input:
  CHARACTER(LEN=*) :: filHed
  INTEGER(i4b)     :: readgeos                   ! 1: read geos info; 0: don't

! output:
  INTEGER(i4b)     :: meaTyp                     ! Measurement type
  INTEGER(i4b)     :: nDiff                      ! Zero- or Single-differences
  INTEGER(i4b)     :: nFreq                      ! Number of frequencies
  INTEGER(i4b)     :: nEpoch                     ! Number of epochs
  INTEGER(i4b)     :: nSatel                     ! Number of satellites
  CHARACTER(LEN=4 ),DIMENSION(2)      :: csess   ! Session defintion
                                                 !  1: Session identifier
                                                 !  2: File ident. of the same
                                                 !     session
  INTEGER(i4b)     :: iDeltt                     ! Observation interval (sec)
  REAL(r8b)        :: timRef                     ! Reference epoch
  CHARACTER(LEN=16) :: campgn                    ! Campaign
  CHARACTER(LEN=53) :: title                     ! File title
  CHARACTER(LEN=9 ),DIMENSION(2)      :: crDate  ! Creation(1) and modif.(2) date
  CHARACTER(LEN=5 ),DIMENSION(2)      :: crTime  ! Creation(1) and modif.(2) time
  INTEGER(i4b)     :: irMark                     ! Remark number
  INTEGER(i4b)     :: nEpFlg                     ! Number of flagged epochs
  INTEGER(i4b)     :: iFrmat                     ! File format number
  CHARACTER(LEN=*),DIMENSION(2)       :: staNam  ! Station names
  CHARACTER(LEN=20),DIMENSION(2)      :: recTyp  ! Receiver types
  CHARACTER(LEN=20),DIMENSION(2)      :: antTyp  ! Antenna types
  INTEGER(i4b),DIMENSION(2)           :: irUnit  ! Receiver unit numbers
  INTEGER(i4b),DIMENSION(2)           :: iAnten  ! Receiver antenna numbers
  CHARACTER(LEN=20),DIMENSION(2)      :: oprNam  ! Operator name
  REAL(r8b),DIMENSION(3,2)            :: posecc  ! Position eccentricities
  INTEGER(i4b),DIMENSION(2)           :: iClock  ! Type of clock parameters
  INTEGER(i4b),DIMENSION(mxcSat)      :: numSat  ! Satellite numbers (PRN)
  INTEGER(i4b),DIMENSION(mxcSat,2)    :: numObs  ! Number of observations used
  INTEGER(i4b),DIMENSION(mxcSat,2)    :: numMrk  ! Number of observations marked
  INTEGER(i4b)     :: numAmb                     ! Total number of ambiguities
  INTEGER(i4b),DIMENSION(*)           :: ambSat  ! Satellite numbers (PRN)
  INTEGER(i4b),DIMENSION(*)           :: ambIep  ! Ambiguity starting epoch number
  INTEGER(i4b),DIMENSION(mxcAmb,2)    :: ambWlf  ! Wavelength factors
  REAL(r8b),DIMENSION(mxcAmb,3)       :: ambig   ! Ambiguities
  INTEGER(i4b),DIMENSION(mxcAmb,3)    :: ambCls  ! Ambiguity cluster
  INTEGER(i4b),OPTIONAL               :: usegeos ! Use Giove Ext. Obs. Sel. file
  TYPE(t_gobsdef),OPTIONAL            :: gobsdef ! Giove Ext. Obs. Sel. info

! List of functions
! -----------------
! Local types
! -----------

! Local parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER :: srName = 'rdHead'

! Local Variables
! ---------------
  TYPE(t_obsHead) :: head

  CHARACTER(LEN=6):: mxnSat,mxnAmb

  INTEGER(i4b)    :: iSat
  INTEGER(i4b)    :: iAmb
  INTEGER(i4b)    :: iSta
  INTEGER(i4b)    :: nSta
  INTEGER(i4b)    :: irc
  INTEGER(i4b)    :: norec ! # of sats with individual obs type selection
  INTEGER(i4b)    :: obs
  INTEGER(i4b)    :: usegeos1

  COMMON/MCMSAT/mxcSat,mxnSat
  COMMON/MCMAMB/mxcAmb,mxnAmb

! Read header with new SR rdHead2
! -------------------------------
  CALL init_obshead(head)
  CALL rdHead2(filHed,head)

! Test maximum dimension of head%nSatel
! -------------------------------------
  CALL dimtst(1,1,2,srName,'head%nSatel','Number of satellites', &
       'Too many satellites in header file.',&
       head%nSatel,mxcSat,irc)

! Test maximum dimension of head%numAmb
! -------------------------------------
  CALL dimtst(1,1,2,srName,'head%numAmb','Number of ambiguities', &
       'Too many ambiguities in header file.',&
       head%numAmb,mxcAmb,irc)

! Transform head-structure into old variables
! -------------------------------------------
  meaTyp = head%meaTyp
  nDiff  = head%nDiff
  nFreq  = head%nFreq
  nEpoch = head%nEpoch
  nSatel = head%nSatel
  nSta   = nDiff+1

  cSess(:)  = head%cSess(:)
  iDeltt    = head%iDeltt
  timRef    = head%timRef
  campgn    = head%campgn
  title     = head%title
  crDate(:) = head%crDate(:)
  crTime(:) = head%crTime(:)
  irMark    = head%irMark
  nEpFlg    = head%nEpFlg
  iFrmat    = head%iFrmat
  staNam(1:nSta) = head%sta(1:nSta)%staNam
  recTyp(1:nSta) = head%sta(1:nSta)%recTyp
  antTyp(1:nSta) = head%sta(1:nSta)%antTyp
  irUnit(1:nSta) = head%sta(1:nSta)%irUnit
  iAnten(1:nSta) = head%sta(1:nSta)%iAnten
  oprNam(1:nSta) = head%sta(1:nSta)%oprNam
  DO iSta = 1,nSta
    posEcc(1:3,iSta) = head%sta(iSta)%posEcc(1:3)
  ENDDO
  iClock(1:nSta)   = head%sta(1:nSta)%iClock
  numSat(1:nSatel) = head%sat(1:nSatel)%numSat

  norec=0
  DO iSat = 1,head%nSatel
    numObs(iSat,:) = head%sat(iSat)%numObs(:)
    numMrk(iSat,:) = head%sat(iSat)%numMrk(:)

    IF (IFRMAT > 5) THEN
      DO obs = 1,8
       IF (head%sat(iSat)%obstyp(obs).NE.'   ') THEN
          norec=norec+1
          EXIT
       ENDIF
      ENDDO
    ENDIF
  ENDDO
! initialize gobsdef and fill it with info from head
  IF ((PRESENT(USEGEOS)).AND.(PRESENT(GOBSDEF)).AND.(readgeos==1)) THEN
    usegeos=0
    IF ((IFRMAT > 5).AND.(norec > 0)) THEN
      CALL init_geos(norec,gobsdef)
      CALL setgeos(head,gobsdef,usegeos1)
      IF (usegeos1==1) usegeos=1
    ENDIF
  ENDIF

  numAmb    = head%numAmb
  DO iAmb = 1,head%numAmb
    ambSat(iAmb)   = head%ambigu(iAmb)%ambSat
    ambIep(iAmb)   = head%ambigu(iAmb)%ambIep
    ambWlf(iAmb,:) = head%ambigu(iAmb)%ambWlf(:)
    ambig (iAmb,:) = head%ambigu(iAmb)%ambigu(:)
    ambCls(iAmb,:) = head%ambigu(iAmb)%ambCls(:)
  ENDDO

! Deallocate memory
! -----------------
  DEALLOCATE(head%ambigu)
  DEALLOCATE(head%sat)

  RETURN
  END SUBROUTINE rdHead

END MODULE
