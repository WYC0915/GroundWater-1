MODULE s_FODIGETI
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE fodigeti(opt,datFil,sCore)

! -------------------------------------------------------------------------
! Purpose:    Insert a priori known elements into %evnt for each station (iSta)
!
! Author:     Luca Ostini
!
! Created:    14-Aug-2008
!
! Changes:    14-Aug-2008 LO: Created this file
!             02-Oct-2008 LO: First revision
!             09-Oct-2008 LO: Third revision
!             05-Dec-2008 LO: Fourth revisio: velocity changes allowed
!             11-Feb-2009 LO: Fifth
!             11-Mar-2009 LO: Fourier Series implemented
!             03-Jul-2009 SL: write statement corrected (comma)
!             25-Sep-2009 LO: Changes for F90 consistency
!             18-Nov-2009 LO: Major changes for consistency
!             21-Dec-2009 LO: FFT removed and several changes apported
!             02-Mar-2010 LO: Major changes do to elimination of FODISUBR
!             07-Apr-2010 LO: Major changes do to algorithm and output file
!             16-Jun-2010 LO: Time series diveded by component
!             26-Aug-2010 LO: Architectural changes
!             03-Jan-2010 LO: INTENT(INOUT) removed and bug fixed
!             24-May-2011 LO: New update of ADDNEQ2 meta-data
!             19-Jul-2011 LO: Test datum defintion added
!             31-Aug-2011 LO: typeNot intervals fixed + 1s sampling
!             19-Sep-2012 RD: Use P_FODITS with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Used Modules
! ------------
! structures, variables:
  USE m_bern,    ONLY: i4b, r8b, shortLineLength, lfnPrt
  USE d_const,   ONLY: pi
  USE d_staCrx,  ONLY: t_renamsta
  USE p_fodits,  ONLY: t_opt, t_datFil, t_sCore, &
                       typejump, infostaf, flagtst, flagset, &
                       scoretypetonum, typenone, scoreflagtonum, &
                       flagest, significant, infoevlf, typeoutl, &
                       typevelo, infoerqf, typeperi, infopanl, infoddef, &
                       scorenumtotype, scorenumtoflag, &
                       scoreinitnewevnt, scorechkpresevntinmod, &
                       scoremodaddevnt, scoremodsortelem

! operator, methods:
!  USE d_debug,   ONLY: debug_entry, debug_exit
  USE m_time,    ONLY: t_timint, OPERATOR(.isIn.)

! subroutines, functions:
  USE s_alcerr
  USE s_timst2

! no implicit
  IMPLICIT NONE

! subroutine name
  CHARACTER(LEN=8), PARAMETER    :: srName = 'fodigeti'


! List of Arguments
! -----------------
! input:

! input/output:
  TYPE(t_opt)                    :: opt        ! Option directory
  TYPE(t_datFil)                 :: datFil     ! I/O files structure
  TYPE(t_sCore)                  :: sCore      ! Data structure

! output:


! Local Types
! -----------


! Local Parameters
! ----------------

! Local Variables
! ---------------
  TYPE(t_renamsta)               :: renCrxTmp

  CHARACTER(LEN=20),DIMENSION(2) :: datstr
  CHARACTER(LEN=1)               :: stationRenamingFlg
  CHARACTER(LEN=1)               :: receiverTypeChangesFlg
  CHARACTER(LEN=1)               :: receiverNumberChangesFlg
  CHARACTER(LEN=1)               :: antennaTypeChangesFlg
  CHARACTER(LEN=1)               :: antennaNumberChangesFlg
  CHARACTER(LEN=1)               :: antennaEccentricitiesFlg

  CHARACTER(LEN=10)              :: period
  CHARACTER(LEN=3)               :: erqMag
  CHARACTER(LEN=5)               :: erqDis
  CHARACTER(LEN=3)               :: erqDep
  CHARACTER(LEN=6)               :: erqLat
  CHARACTER(LEN=7)               :: erqLon

  INTEGER(i4b)                   :: iac
  INTEGER(i4b)                   :: iSta
  INTEGER(i4b)                   :: iMjd
  INTEGER(i4b)                   :: jSta
  INTEGER(i4b)                   :: iPeri
  INTEGER(i4b)                   :: iEvnt
  INTEGER(i4b)                   :: jEvnt
  INTEGER(i4b)                   :: iRen
  INTEGER(i4b)                   :: iInfo
  INTEGER(i4b)                   :: iErq
  INTEGER(i4b)                   :: seen
  INTEGER(i4b)                   :: anyChanges
  INTEGER(i4b)                   :: anyChangesFlag
  INTEGER(i4b)                   :: iTmp, kTmp, jTmp
  INTEGER(i4b)                   :: distInt
  INTEGER(i4b)                   :: iii
  INTEGER(i4b)                   :: nVal
  INTEGER(i4b)                   :: nNum

  REAL(r8b)                      :: dist
  REAL(r8b)                      :: Mdist
  REAL(r8b)                      :: reasonStaJump
  REAL(r8b)                      :: begMjd
  REAL(r8b)                      :: endMjd
  REAL(r8b)                      :: begStaMjd
  REAL(r8b)                      :: endStaMjd


  TYPE t_apri_info
     CHARACTER(LEN=4)                          :: type
     CHARACTER(LEN=3)                          :: flag
     CHARACTER(LEN=6)                          :: info
     REAL(r8b)                                 :: mjd
     REAL(r8b)                                 :: mjd2
     REAL(r8b)                                 :: omega
     CHARACTER(LEN=20),DIMENSION(2)            :: datstr
     CHARACTER(LEN=8)                          :: remark
     CHARACTER(LEN=6)                          :: reason
     REAL(r8b)                                 :: erqMag
     REAL(r8b)                                 :: erqDep
     REAL(r8b)                                 :: erqDis
     REAL(r8b)                                 :: erqLat
     REAL(r8b)                                 :: erqLon
  END TYPE t_apri_info

  TYPE(t_apri_info),DIMENSION(:),ALLOCATABLE   :: aInfo
  TYPE(t_apri_info)                            :: dTmp

! Call debug routine
! ------------------
!  CALL debug_entry(srName)


! Initialization of all variables
! -------------------------------
  nVal = sCore%nVal
  nNum = opt%inTruncStaName

  DO iSta = 1,sCore%nSta
     sCore%sta(iSta)%mod%nEvnt = 0
     NULLIFY(sCore%sta(iSta)%mod%evnt)
  END DO

  ! Load information from STA file
  ! Sort station information file TYPE-001 in terms of (1)oldnam, (2)timint
  DO iTmp = 1,datFil%staIn%nrenam-1
     kTmp = iTmp
     DO jTmp = iTmp+1,datFil%staIn%nrenam
        IF(  datFil%staIn%renamsta(jTmp)%oldnam < &
             datFil%staIn%renamsta(kTmp)%oldnam .OR. &
             ( datFil%staIn%renamsta(jTmp)%oldnam == &
               datFil%staIn%renamsta(kTmp)%oldnam .AND. &
               datFil%staIn%renamsta(jTmp)%timint%t(1) < &
               datFil%staIn%renamsta(kTmp)%timint%t(1) ) )THEN
           kTmp = jTmp
        END IF
     END DO
     IF( kTmp /= iTmp )THEN
        renCrxTmp = datFil%staIn%renamsta(iTmp)
        datFil%staIn%renamsta(iTmp) = datFil%staIn%renamsta(kTmp)
        datFil%staIn%renamsta(kTmp) = renCrxTmp
     END IF
  END DO
  ! Detect discontinuities from STA-file TYPE 001
  ! Encoding of the jump reason:
  !   + %stTst =  1.0D0  : station renaming
  !   + %stTst =  2.0D0  : receiver type changes
  !   + %stTst =  4.0D0  : receiver number changes
  !   + %stTst =  8.0D0  : antenna type changes
  !   + %stTst = 16.0D0  : antenna number changes
  !   + %stTst = 32.0D0  : antenna eccentricities
  !   %stTst contains the sum of all changes.
  DO iRen = 1,datFil%staIn%nrenam-1
     IF(  datFil%staIn%renamsta(iRen)%oldnam == &
          datFil%staIn%renamsta(iRen+1)%oldnam .AND. &
          datFil%staIn%renamsta(iRen)%stanam /= &
          datFil%staIn%renamsta(iRen+1)%stanam )THEN
        ! Read and store the information for the each iSta of sCore
        DO iSta = 1,sCore%nSta
           IF( sCore%sta(iSta)%name == datFil%staIn%renamsta(iRen)%oldnam )THEN
              CALL sCoreInitNewEvnt(nVal,sCore%mdl%evnt)
              sCore%mdl%evnt%type = typeJump
              sCore%mdl%evnt%info = infoStaF
              sCore%mdl%evnt%mjd  = datFil%staIn%renamsta(iRen+1)%timint%t(1)
              sCore%mdl%evnt%flag = flagTst
              IF( opt%tstStaRenamings == 2 )THEN
                 sCore%mdl%evnt%flag = flagSet
              END IF
              sCore%mdl%evnt%remark = 'STA-DISC'
              CALL sCoreChkPresEvntInMod( sCore%mdl%evnt, &
                   sCore%sta(iSta)%mod%evnt, sCore%sta(iSta)%mod%nEvnt, seen )
              IF( seen == 0 .AND. opt%tstStaRenamings /= 0 )THEN
                 CALL sCoreModAddEvnt( sCore%mdl%evnt, &
                      sCore%sta(iSta)%mod%evnt, sCore%sta(iSta)%mod%nEvnt )
              END IF
           END IF
        END DO
     END IF
  END DO

  DO iInfo = 1,datFil%staIn%ninfo-1
     anyChanges = 0
     anyChangesFlag = flagTst
     IF( datFil%staIn%staInfo(iInfo)%stanam /= &
         datFil%staIn%staInfo(iInfo+1)%stanam  )CYCLE
     ! receiver type changes
     IF(  opt%tstRecTypeChanges /= 0 .AND. &
          datFil%staIn%staInfo(iInfo)%recnam /= &
          datFil%staIn%staInfo(iInfo+1)%recnam  )THEN
        anyChanges = anyChanges + 2
        IF( opt%tstRecTypeChanges > 1 ) anyChangesFlag = flagSet
     END IF
     ! receiver number changes
     IF(  opt%tstRecNumberChanges /= 0 .AND. &
          datFil%staIn%staInfo(iInfo)%recnum /= &
          datFil%staIn%staInfo(iInfo+1)%recnum  )THEN
        anyChanges = anyChanges + 4
        IF( opt%tstRecTypeChanges > 1 ) anyChangesFlag = flagSet
     END IF
     ! antenna type changes
     IF(  opt%tstAntNumberChanges /= 0 .AND. &
          datFil%staIn%staInfo(iInfo)%antnam /= &
          datFil%staIn%staInfo(iInfo+1)%antnam  )THEN
        anyChanges = anyChanges + 8
        IF( opt%tstRecTypeChanges > 1 ) anyChangesFlag = flagSet
     END IF
     ! antenna number changes
     IF(  opt%tstAntNumberChanges /= 0 .AND. &
          datFil%staIn%staInfo(iInfo)%antnum /= &
          datFil%staIn%staInfo(iInfo+1)%antnum  )THEN
        anyChanges = anyChanges + 16
        IF( opt%tstRecTypeChanges > 1 ) anyChangesFlag = flagSet
     END IF
     ! antenna eccentricities
     IF(  opt%tstEccChanges /= 0 .AND. &
          ( datFil%staIn%staInfo(iInfo)%antecc(1) /= &
            datFil%staIn%staInfo(iInfo+1)%antecc(1)  .OR. &
            datFil%staIn%staInfo(iInfo)%antecc(2) /= &
            datFil%staIn%staInfo(iInfo+1)%antecc(2)  .OR. &
            datFil%staIn%staInfo(iInfo)%antecc(3) /= &
            datFil%staIn%staInfo(iInfo+1)%antecc(3) )  )THEN
        anyChanges = anyChanges + 32
        IF( opt%tstRecTypeChanges > 1 ) anyChangesFlag = flagSet
     END IF
     IF( anyChanges > 0 )THEN
        DO iSta = 1,sCore%nSta
           ! Filter
           IF( sCore%sta(iSta)%ts%nMjd == 0 )EXIT
           IF( sCore%sta(iSta)%name == &
               datFil%staIn%staInfo(iInfo+1)%stanam .AND. &
               datFil%staIn%staInfo(iInfo+1)%timint%t(1) > &
               sCore%sta(iSta)%ts%mjd(1) .AND. &
               datFil%staIn%staInfo(iInfo+1)%timint%t(1) < &
               sCore%sta(iSta)%ts%mjd(sCore%sta(iSta)%ts%nMjd) )THEN
              seen = 0
              DO iEvnt = 1,sCore%sta(iSta)%mod%nEvnt
                 IF( sCore%sta(iSta)%mod%evnt(iEvnt)%mjd == &
                     datFil%staIn%staInfo(iInfo+1)%timint%t(1) )THEN
                    sCore%sta(iSta)%mod%evnt(iEvnt)%stTst = &
                       sCore%sta(iSta)%mod%evnt(iEvnt)%stTst + REAL(anyChanges)
                    seen = 1
                    EXIT
                 END IF
              END DO
              IF( seen == 0 )THEN
                 CALL sCoreInitNewEvnt(nVal,sCore%mdl%evnt)
           sCore%mdl%evnt%type = typeJump
           sCore%mdl%evnt%info = infoStaF
           sCore%mdl%evnt%flag = anyChangesFlag
           sCore%mdl%evnt%mjd = datFil%staIn%staInfo(iInfo+1)%timint%t(1)
           sCore%mdl%evnt%stTst = REAL(anyChanges)
           sCore%mdl%evnt%remark = 'STA-DISC'
           sCore%mdl%evnt%recChg(1) = datFil%staIn%staInfo(iInfo)%recnam
           sCore%mdl%evnt%recChg(2) = datFil%staIn%staInfo(iInfo+1)%recnam
           sCore%mdl%evnt%antChg(1) = datFil%staIn%staInfo(iInfo)%antnam
           sCore%mdl%evnt%antChg(2) = datFil%staIn%staInfo(iInfo+1)%antnam
           sCore%mdl%evnt%eccChg(:,1) = datFil%staIn%staInfo(iInfo)%antecc(3)
           sCore%mdl%evnt%eccChg(:,2) = datFil%staIn%staInfo(iInfo+1)%antecc(3)
                 CALL sCoreChkPresEvntInMod( sCore%mdl%evnt, &
                      sCore%sta(iSta)%mod%evnt, sCore%sta(iSta)%mod%nEvnt, &
                      seen )
                 IF( seen == 0 )THEN
                    CALL sCoreModAddEvnt( sCore%mdl%evnt, &
                         sCore%sta(iSta)%mod%evnt, sCore%sta(iSta)%mod%nEvnt )
                 END IF
                 EXIT
              END IF
           END IF
        END DO
     END IF
  END DO

  ! datFil%evlIn -> sCore%..%mod (load information from EVL file) opt%tstEvlTst
  DO jSta = 1,datFil%evlIn%nSta
     DO iSta = 1,sCore%nSta
        ! Filter
        IF( sCore%sta(iSta)%ts%nMjd == 0 )EXIT
        IF( datFil%evlIn%sta(jSta)%name(1:nNum) /= &
            sCore%sta(iSta)%name(1:nNum) )CYCLE
        ! Help variables
        begStaMjd = sCore%sta(iSta)%ts%mjd(1)
        endStaMjd = sCore%sta(iSta)%ts%mjd(sCore%sta(iSta)%ts%nMjd)
        ! Flag = NEW, SET, or EST
        DO iEvnt = 1,datFil%evlIn%sta(jSta)%nEvnt
           CALL sCoreInitNewEvnt(nVal,sCore%mdl%evnt)
           sCore%mdl%evnt%type = &
                sCoreTypeToNum(datFil%evlIn%sta(jSta)%evnt(iEvnt)%type)
           IF( sCore%mdl%evnt%type == typeNone )CYCLE
           sCore%mdl%evnt%flag = &
                sCoreFlagToNum(datFil%evlIn%sta(jSta)%evnt(iEvnt)%flag)
           ! Special case for feedback of EVL files
           IF( sCore%mdl%evnt%flag == flagEst .AND. opt%tstEvlTst == 1 .AND. &
               datFil%evlIn%sta(jSta)%evnt(iEvnt)%siTst == significant )THEN
              sCore%mdl%evnt%flag = flagTst
           END IF
           IF( sCore%mdl%evnt%flag == flagEst .AND. opt%tstEvlTst == 2 .AND. &
               datFil%evlIn%sta(jSta)%evnt(iEvnt)%siTst == significant )THEN
              sCore%mdl%evnt%flag = flagSet
           END IF
           sCore%mdl%evnt%info = infoEvlF
           sCore%mdl%evnt%mjd = datFil%evlIn%sta(jSta)%evnt(iEvnt)%timint%t(1)
           sCore%mdl%evnt%mjd2 = datFil%evlIn%sta(jSta)%evnt(iEvnt)%timint%t(2)
           sCore%mdl%evnt%timint%t(:) = &
                datFil%evlIn%sta(jSta)%evnt(iEvnt)%timint%t(:)
           sCore%mdl%evnt%omega = datFil%evlIn%sta(jSta)%evnt(iEvnt)%omega
           sCore%mdl%evnt%remark = datFil%evlIn%sta(jSta)%evnt(iEvnt)%remark
           ! Special handling for outliers
           IF( sCore%mdl%evnt%type == typeOutl )THEN
              DO iMjd = 1,sCore%sta(iSta)%ts%nMjd
                 IF( sCore%sta(iSta)%ts%mjd(iMjd) < &
                     datFil%evlIn%sta(jSta)%evnt(iEvnt)%timint%t(1) )CYCLE
                 IF( sCore%sta(iSta)%ts%mjd(iMjd) > &
                     datFil%evlIn%sta(jSta)%evnt(iEvnt)%timint%t(2) )CYCLE
                 sCore%mdl%evnt%mjd = sCore%sta(iSta)%ts%mjd(iMjd)
                 CALL sCoreChkPresEvntInMod( sCore%mdl%evnt, &
                      sCore%sta(iSta)%mod%evnt,sCore%sta(iSta)%mod%nEvnt,seen)
                 ! Add the new evnt
                 IF( seen == 0 )THEN
                    CALL sCoreModAddEvnt( sCore%mdl%evnt, &
                         sCore%sta(iSta)%mod%evnt, sCore%sta(iSta)%mod%nEvnt)
                 END IF
              END DO
           ELSE
              ! Check whether the event exists already in %evnt
              IF( sCore%mdl%evnt%remark == 'OFFST-TS' .OR. &
                  sCore%mdl%evnt%remark == 'DRIFT-TS' )CYCLE
              ! Filter
              IF( ( sCore%mdl%evnt%type == typeJump .OR. &
                    sCore%mdl%evnt%type == typeVelo ) .AND. &
                  ( sCore%mdl%evnt%mjd <= begStaMjd .OR. &
                    sCore%mdl%evnt%mjd >= endStaMjd ) )CYCLE
              ! Check presence
              CALL sCoreChkPresEvntInMod( sCore%mdl%evnt, &
                   sCore%sta(iSta)%mod%evnt, sCore%sta(iSta)%mod%nEvnt, seen )
              ! Add the new evnt
              IF( seen == 0 )THEN
                 CALL sCoreModAddEvnt( sCore%mdl%evnt, &
                      sCore%sta(iSta)%mod%evnt, sCore%sta(iSta)%mod%nEvnt )
              END IF
           END IF
        END DO
     END DO
  END DO

  ! datFil%erqIn -> sCore%...%mod (load information from ERQ file)
  DO iErq = 1,datFil%erqIn%nErq
     DO iSta = 1,sCore%nSta
        ! Filter
        IF( sCore%sta(iSta)%ts%nMjd == 0 )EXIT
        dist = SQRT( &
             (datFil%erqIn%erq(iErq)%crdXyz(1) - sCore%sta(iSta)%staSta( &
             sCore%sta(iSta)%iRefSta )%inCrdXyz(1))**2 + &
             (datFil%erqIn%erq(iErq)%crdXyz(2) - sCore%sta(iSta)%staSta( &
             sCore%sta(iSta)%iRefSta )%inCrdXyz(2))**2 + &
             (datFil%erqIn%erq(iErq)%crdXyz(3) - sCore%sta(iSta)%staSta( &
             sCore%sta(iSta)%iRefSta )%inCrdXyz(3))**2 )
        Mdist = opt%gvarErqFactA + opt%gvarErqFactB*LOG10(dist)
        begMjd = sCore%sta(iSta)%ts%mjd(1)
        endMjd = sCore%sta(iSta)%ts%mjd(sCore%sta(iSta)%ts%nMjd)
        IF(  datFil%erqIn%erq(iErq)%mag >= Mdist .AND. &
             datFil%erqIn%erq(iErq)%mjd >= begMjd .AND. &
             datFil%erqIn%erq(iErq)%mjd <= endMjd )THEN
           ! Set up a discontinuity
           CALL sCoreInitNewEvnt(nVal,sCore%mdl%evnt)
           sCore%mdl%evnt%type = typeJump
           sCore%mdl%evnt%info = infoErqF
           sCore%mdl%evnt%mjd = datFil%erqIn%erq(iErq)%mjd
           sCore%mdl%evnt%flag = flagTst
           sCore%mdl%evnt%vlTst = datFil%erqIn%erq(iErq)%mag
           sCore%mdl%evnt%remark = 'ERQ-DISC'
           CALL sCoreChkPresEvntInMod( sCore%mdl%evnt, &
                sCore%sta(iSta)%mod%evnt, sCore%sta(iSta)%mod%nEvnt, seen )
           IF( seen == 0 )THEN
              CALL sCoreModAddEvnt( sCore%mdl%evnt, &
                   sCore%sta(iSta)%mod%evnt, sCore%sta(iSta)%mod%nEvnt )
           END IF
           ! Set up a velocity
           CALL sCoreInitNewEvnt(nVal,sCore%mdl%evnt)
           sCore%mdl%evnt%type = typeVelo
           sCore%mdl%evnt%info = infoErqF
           sCore%mdl%evnt%mjd = datFil%erqIn%erq(iErq)%mjd
           sCore%mdl%evnt%flag= flagTst
           sCore%mdl%evnt%vlTst = datFil%erqIn%erq(iErq)%mag
           sCore%mdl%evnt%remark = 'ERQ-VELO'
           CALL sCoreChkPresEvntInMod( sCore%mdl%evnt, &
                sCore%sta(iSta)%mod%evnt, sCore%sta(iSta)%mod%nEvnt, seen )
           IF( seen == 0 )THEN
              CALL sCoreModAddEvnt( sCore%mdl%evnt, &
                   sCore%sta(iSta)%mod%evnt, sCore%sta(iSta)%mod%nEvnt )
           END IF
        END IF
     END DO
  END DO

  ! Periodic functions from PANEL
  DO iSta = 1,sCore%nSta
     ! Seasonal signals
     IF( opt%modPerAnn == 1 )THEN
        DO iPeri = 1,2
           ! Set up the period
           CALL sCoreInitNewEvnt(nVal,sCore%mdl%evnt)
           sCore%mdl%evnt%type = typePeri
           sCore%mdl%evnt%info = infoPanl
           sCore%mdl%evnt%timint%t(1) = sCore%begMjd
           sCore%mdl%evnt%timint%t(2) = sCore%endMjd
           sCore%mdl%evnt%omega = 2.0D0*pi/(365.25D0/REAL(iPeri))
           sCore%mdl%evnt%flag= flagTst
           sCore%mdl%evnt%remark = 'PAN-PERI'
           CALL sCoreChkPresEvntInMod( sCore%mdl%evnt, &
                sCore%sta(iSta)%mod%evnt, sCore%sta(iSta)%mod%nEvnt, seen )
           IF( seen == 0 )THEN
              CALL sCoreModAddEvnt( sCore%mdl%evnt, &
                   sCore%sta(iSta)%mod%evnt, sCore%sta(iSta)%mod%nEvnt )
           END IF
        END DO
     END IF
     ! Additional periods
     DO iPeri = 1,opt%nAddPerParam
        ! Set up the period
        CALL sCoreInitNewEvnt(nVal,sCore%mdl%evnt)
        sCore%mdl%evnt%type = typePeri
        sCore%mdl%evnt%info = infoPanl
        sCore%mdl%evnt%timint%t(1) = sCore%begMjd
        sCore%mdl%evnt%timint%t(2) = sCore%endMjd
        sCore%mdl%evnt%omega = opt%modAddPerParam(iPeri)
        sCore%mdl%evnt%flag= flagTst
        sCore%mdl%evnt%remark = 'PAN-PERI'
        CALL sCoreChkPresEvntInMod( sCore%mdl%evnt, &
             sCore%sta(iSta)%mod%evnt, sCore%sta(iSta)%mod%nEvnt, seen )
        IF( seen == 0 )THEN
           CALL sCoreModAddEvnt( sCore%mdl%evnt, &
                sCore%sta(iSta)%mod%evnt, sCore%sta(iSta)%mod%nEvnt )
        END IF
     END DO
  END DO

  ! Outliers identified in the test of datum definition
  IF( opt%modPreDatTest  == 1 .AND. &
      opt%datRejectCrd == 1 .AND. opt%datRejectOutlCrd > 0 )THEN
     DO iSta = 1,sCore%nSta
        DO iMjd = 1,sCore%sta(iSta)%ts%nMjd
           IF( sCore%sta(iSta)%ts%dDefRej(iMjd) == 2 )THEN
              CALL sCoreInitNewEvnt(nVal,sCore%mdl%evnt)
              sCore%mdl%evnt%type = typeOutl
              sCore%mdl%evnt%info = infoDDef
              sCore%mdl%evnt%flag = flagTst
              IF( opt%datRejectOutlCrd == 2 )THEN
                 sCore%mdl%evnt%flag = flagSet
              END IF
              sCore%mdl%evnt%mjd = sCore%sta(iSta)%ts%mjd(iMjd)
              sCore%mdl%evnt%stTst = 0.0D0
              sCore%mdl%evnt%remark = 'DDF-OUTL'
              CALL sCoreChkPresEvntInMod( sCore%mdl%evnt, &
                   sCore%sta(iSta)%mod%evnt, sCore%sta(iSta)%mod%nEvnt, &
                   seen )
              IF( seen == 0 )THEN
                 CALL sCoreModAddEvnt( sCore%mdl%evnt, &
                      sCore%sta(iSta)%mod%evnt, sCore%sta(iSta)%mod%nEvnt )
              END IF
           END IF
        END DO
     END DO
  END IF

  ! Sort %evnt
  DO iSta = 1,sCore%nSta
     CALL sCoreModSortElem( nVal, &
          sCore%sta(iSta)%mod%evnt,sCore%sta(iSta)%mod%nEvnt)
  END DO

  ! Screen mod%evnt: element within NOT intervals will be removed
  DO iSta = 1,sCore%nSta
     CALL fodigeti_screenModWithNotEvnt( &
          sCore%sta(iSta)%mod%evnt, sCore%sta(iSta)%mod%nEvnt )
  END DO

  ! Sort %evnt
  DO iSta = 1,sCore%nSta
     CALL sCoreModSortElem( nVal, &
          sCore%sta(iSta)%mod%evnt,sCore%sta(iSta)%mod%nEvnt)
  END DO

  ! Screen mod%evnt: afterquakes within few epochs will be removed
  DO iSta = 1,sCore%nSta
     CALL fodigeti_screenModForErqs( iSta, sCore, opt%gvarErqMinDist )
  END DO

  ! Sort %evnt
  DO iSta = 1,sCore%nSta
     CALL sCoreModSortElem( nVal, &
          sCore%sta(iSta)%mod%evnt,sCore%sta(iSta)%mod%nEvnt)
  END DO

  ! Write informations to output file (lfnprt)
  ! ------------------------------------------
  iii = 0
  DO iSta = 1,sCore%nSta
     iii = iii + sCore%sta(iSta)%mod%nEvnt
  END DO
  IF( iii > 0 )THEN
     ! Title
     WRITE(lfnprt,'(/,/,A,/,A)')                &
          ' INPUT: A PRIORI INFORMATION (FROM FILES OR FROM INPUT PANEL)', &
          ' ------------------------------------------------------------'
     WRITE(lfnprt,'(6(A,/),A)')  &
          ' Reasons for equipment changes:', &
          ' A: Station renaming', &
          ' B: Receiver type changes', &
          ' C: Receiver number changes', &
          ' D: Antenna type changes', &
          ' E: Antenna number changes', &
          ' F: Antenna eccentricity changes'

     DO iSta = 1,sCore%nSta

        ! Allocate memory for each station
        IF( sCore%sta(iSta)%mod%nEvnt == 0 )CYCLE
        ALLOCATE(aInfo(sCore%sta(iSta)%mod%nEvnt),stat=iac)
        CALL alcerr(iac,'aInfo',(/sCore%sta(iSta)%mod%nEvnt/),srName)

        DO iEvnt = 1,sCore%sta(iSta)%mod%nEvnt
           ! %type
           aInfo(iEvnt)%type = &
                sCoreNumToType(sCore%sta(iSta)%mod%evnt(iEvnt)%type)
           ! %flag
           aInfo(iEvnt)%flag = &
                sCoreNumToFlag(sCore%sta(iSta)%mod%evnt(iEvnt)%flag)
           ! %datstr(1)
           datstr(1) = ''
           IF(  sCore%sta(iSta)%mod%evnt(iEvnt)%mjd  > 1.0D04 .AND. &
                sCore%sta(iSta)%mod%evnt(iEvnt)%mjd  < 1.0D20 )THEN
              CALL timst2(1,1, &
                   sCore%sta(iSta)%mod%evnt(iEvnt)%mjd,datstr(1))
           END IF
           aInfo(iEvnt)%datstr(1) = datstr(1)
           ! %datstr(2)
           datstr(2) = ''
           IF(  sCore%sta(iSta)%mod%evnt(iEvnt)%mjd2  > 1.0D04 .AND. &
                sCore%sta(iSta)%mod%evnt(iEvnt)%mjd2  < 1.0D20 )THEN
              CALL timst2(1,1, &
                   sCore%sta(iSta)%mod%evnt(iEvnt)%mjd2,datstr(2))
           END IF
           aInfo(iEvnt)%datstr(2) = datstr(2)
           ! %mjd and mjd2
           aInfo(iEvnt)%mjd  = sCore%sta(iSta)%mod%evnt(iEvnt)%mjd
           aInfo(iEvnt)%mjd2 = sCore%sta(iSta)%mod%evnt(iEvnt)%mjd2
           ! %omega
           aInfo(iEvnt)%omega = sCore%sta(iSta)%mod%evnt(iEvnt)%omega
           ! %remark
           aInfo(iEvnt)%remark = sCore%sta(iSta)%mod%evnt(iEvnt)%remark
           ! %info
           aInfo(iEvnt)%info = ''
           IF( sCore%sta(iSta)%mod%evnt(iEvnt)%info == infoStaF )THEN
              ! Get the reason of the jump
              reasonStaJump = sCore%sta(iSta)%mod%evnt(iEvnt)%stTst
              ! antenna eccentricities
              IF( reasonStaJump - 32.0D0 < 0.0D0 )THEN
                 antennaEccentricitiesFlg = '-'
              ELSE
                 antennaEccentricitiesFlg = 'F'
                 reasonStaJump = reasonStaJump - 32.0D0
              END IF
              ! antenna number changes
              IF( reasonStaJump - 16.0D0 < 0.0D0 )THEN
                 antennaNumberChangesFlg = '-'
              ELSE
                 antennaNumberChangesFlg = 'E'
                 reasonStaJump = reasonStaJump - 16.0D0
              END IF
              ! antenna type changes
              IF( reasonStaJump - 8.0D0 < 0.0D0 )THEN
                 antennaTypeChangesFlg = '-'
              ELSE
                 antennaTypeChangesFlg = 'D'
                 reasonStaJump = reasonStaJump - 8.0D0
              END IF
              ! receiver number changes
              IF( reasonStaJump - 4.0D0 < 0.0D0 )THEN
                 receiverNumberChangesFlg = '-'
              ELSE
                 receiverNumberChangesFlg = 'C'
                 reasonStaJump = reasonStaJump - 4.0D0
              END IF
              ! receiver type changes
              IF( reasonStaJump - 2.0D0 < 0.0D0 )THEN
                 receiverTypeChangesFlg = '-'
              ELSE
                 receiverTypeChangesFlg = 'B'
                 reasonStaJump = reasonStaJump - 2.0D0
              END IF
              ! station renaming
              IF( reasonStaJump - 1.0D0 < 0.0D0 )THEN
                 stationRenamingFlg = '-'
              ELSE
                 stationRenamingFlg = 'A'
                 reasonStaJump = reasonStaJump - 1.0D0
              END IF
              WRITE(aInfo(iEvnt)%info,'(6(A1))') &
                   stationRenamingFlg, &
                   receiverTypeChangesFlg, &
                   receiverNumberChangesFlg, &
                   antennaTypeChangesFlg, &
                   antennaNumberChangesFlg, &
                   antennaEccentricitiesFlg
           ELSE IF( sCore%sta(iSta)%mod%evnt(iEvnt)%info == infoEvlF )THEN
              aInfo(iEvnt)%info = "EVLEVT"
           END IF
           ! %erqMag
           aInfo(iEvnt)%erqMag = 0.0D0
           aInfo(iEvnt)%erqDep = 0.0D0
           aInfo(iEvnt)%erqDis = 0.0D0
           aInfo(iEvnt)%erqLat = 999.0D0
           aInfo(iEvnt)%erqLon = 999.0D0
           IF( sCore%sta(iSta)%mod%evnt(iEvnt)%info == infoErqF )THEN
              DO iErq = 1,datFil%erqIn%nErq
                 IF( sCore%sta(iSta)%mod%evnt(iEvnt)%mjd == &
                      datFil%erqIn%erq(iErq)%mjd )THEN
                    ! compute the dist in [km]
                    distInt = INT( SQRT( &
                         ( datFil%erqIn%erq(iErq)%crdXyz(1) - &
                         sCore%sta(iSta)%staSta( &
                         sCore%sta(iSta)%iRefSta)%inCrdXyz(1) )**2 + &
                         ( datFil%erqIn%erq(iErq)%crdXyz(2) - &
                         sCore%sta(iSta)%staSta( &
                         sCore%sta(iSta)%iRefSta)%inCrdXyz(2) )**2 + &
                         ( datFil%erqIn%erq(iErq)%crdXyz(3) - &
                         sCore%sta(iSta)%staSta( &
                         sCore%sta(iSta)%iRefSta)%inCrdXyz(3) )**2 )/1.0D3 )
                    aInfo(iEvnt)%erqMag = datFil%erqIn%erq(iErq)%mag
                    aInfo(iEvnt)%erqDep = datFil%erqIn%erq(iErq)%dep
                    aInfo(iEvnt)%erqDis = distInt
                    aInfo(iEvnt)%erqLat = datFil%erqIn%erq(iErq)%lat
                    aInfo(iEvnt)%erqLon = datFil%erqIn%erq(iErq)%lon
                 END IF
              END DO
           END IF

        END DO

        ! Sort aInfo
        DO iTmp = 1,sCore%sta(iSta)%mod%nEvnt
           kTmp = iTmp
           DO jTmp = iTmp+1,sCore%sta(iSta)%mod%nEvnt
              IF( aInfo(jTmp)%mjd < aInfo(kTmp)%mjd .OR. &
                   ( aInfo(jTmp)%mjd == aInfo(kTmp)%mjd .AND.&
                   aInfo(jTmp)%omega > aInfo(kTmp)%omega ) )THEN
                 kTmp = jTmp
              END IF
           END DO
           IF( kTmp /= iTmp )THEN
              dTmp = aInfo(iTmp)
              aInfo(iTmp) = aInfo(kTmp)
              aInfo(kTmp) = dTmp
           END IF
        END DO

        ! Write output
        WRITE(lfnprt,'(A,/,A,/,A)')  &
     '                                                              &
     &           Period            EqChng Earthquakes                      ',&
     '  Nr Station          Evnt Flg Epoch (From)         Epoch (To)&
     &           [days]   Remark   Reason Mag Dist Dept   Lat    Long  KYWD',&
     '--------------------------------------------------------------&
     &---------------------------------------------------------------------'
        DO iEvnt = 1,sCore%sta(iSta)%mod%nEvnt
           datstr(1) = ''
           IF(  aInfo(iEvnt)%mjd  > 1.0D04 .AND. &
                aInfo(iEvnt)%mjd  < 1.0D20 )THEN
              CALL timst2(1,1,aInfo(iEvnt)%mjd,datstr(1))
           END IF
           aInfo(iEvnt)%datstr(1) = datstr(1)

           datstr(2) = ''
           IF(  aInfo(iEvnt)%mjd2  > 1.0D04 .AND. &
                aInfo(iEvnt)%mjd2  < 1.0D20 )THEN
              CALL timst2(1,1,aInfo(iEvnt)%mjd2,datstr(2))
           END IF
           aInfo(iEvnt)%datstr(2) = datstr(2)

           period = ''
           IF( aInfo(iEvnt)%omega > 0.0D0 )THEN
              WRITE(period,'(F10.4)') 2*pi/aInfo(iEvnt)%omega
           END IF

           erqMag = ''
           IF( aInfo(iEvnt)%erqMag > 0.0D0 )THEN
              WRITE(erqMag,'(F3.1)') aInfo(iEvnt)%erqMag
           END IF

           erqDis = ''
           IF( aInfo(iEvnt)%erqDis > 0.0D0 )THEN
              WRITE(erqDis,'(I5)') INT(aInfo(iEvnt)%erqDis)
           END IF

           erqDep = ''
           IF( aInfo(iEvnt)%erqDep > 0.0D0 )THEN
              WRITE(erqDep,'(I3)') INT(aInfo(iEvnt)%erqDep)
           END IF

           erqLat = ''
           IF( aInfo(iEvnt)%erqLat /= 999.0D0 )THEN
              WRITE(erqLat,'(F6.2)') aInfo(iEvnt)%erqLat
           END IF

           erqLon = ''
           IF( aInfo(iEvnt)%erqLon /= 999.0D0 )THEN
              WRITE(erqLon,'(F7.2)') aInfo(iEvnt)%erqLon
           END IF

           WRITE(lfnprt,'(I04,1X,A16,1X,A4,1X,A3,1X,A20,1X,A19,&
                &A10,1X,A8,1X,A6,1X,A3,1X,A5,1X,A3,1X,A6,1X,A7,1X,A4)') &
                iSta, &
                sCore%sta(iSta)%name, &
                aInfo(iEvnt)%type, &
                aInfo(iEvnt)%flag, &
                aInfo(iEvnt)%datstr(1), &
                aInfo(iEvnt)%datstr(2), &
                period, &
                aInfo(iEvnt)%remark, &
                aInfo(iEvnt)%info, &
                erqMag, &
                erqDis, &
                erqDep, &
                erqLat, &
                erqLon, &
                'APRI'

           IF( LEN_TRIM(aInfo(iEvnt)%info) /= 0 )THEN
              DO jEvnt = 1,sCore%sta(iSta)%mod%nEvnt
                 IF( sCore%sta(iSta)%mod%evnt(jEvnt)%type /= typeJump )CYCLE
                 IF( sCore%sta(iSta)%mod%evnt(jEvnt)%info /= infoStaF .OR. &
                      aInfo(iEvnt)%mjd /= &
                      sCore%sta(iSta)%mod%evnt(jEvnt)%mjd .OR. &
                      aInfo(iEvnt)%mjd2 /= &
                      sCore%sta(iSta)%mod%evnt(jEvnt)%mjd2 )CYCLE

                 WRITE(lfnprt,'(2(1X,A2,A20,A2,A20),3(1X,A3,F8.4),1X,A4)') &
                      'A:', &
                      sCore%sta(iSta)%mod%evnt(jEvnt)%antChg(1), &
                      '->', &
                      sCore%sta(iSta)%mod%evnt(jEvnt)%antChg(2), &
                      'R:', &
                      sCore%sta(iSta)%mod%evnt(jEvnt)%recChg(1), &
                      '->', &
                      sCore%sta(iSta)%mod%evnt(jEvnt)%recChg(2), &
                      'EN:', sCore%sta(iSta)%mod%evnt(jEvnt)%eccChg(1,2) - &
                      sCore%sta(iSta)%mod%evnt(jEvnt)%eccChg(1,1),  &
                      'EE:', sCore%sta(iSta)%mod%evnt(jEvnt)%eccChg(2,2) - &
                      sCore%sta(iSta)%mod%evnt(jEvnt)%eccChg(2,1),  &
                      'EU:', sCore%sta(iSta)%mod%evnt(jEvnt)%eccChg(3,2) - &
                      sCore%sta(iSta)%mod%evnt(jEvnt)%eccChg(3,1),  &
                      'APRJ'
              END DO
           END IF

        END DO
        DEALLOCATE(aInfo,stat=iac)
     END DO
  END IF

  ! Reset %stTst
  DO iSta = 1,sCore%nSta
     DO iEvnt = 1,sCore%sta(iSta)%mod%nEvnt
        sCore%sta(iSta)%mod%evnt(iEvnt)%stTst = 0.0D0
     END DO
  END DO

  ! Print title for search new events
  IF(  opt%outFileVerbose == 1 )THEN
     WRITE(lfnprt,'(/,2(/,A))') &
          ' OUTPUT: SEARCH FOR NEW EVENTS', &
          ' -----------------------------'
  END IF

  ! End of subroutine
  ! -----------------
  !  CALL debug_exit(srName)
  RETURN

END SUBROUTINE fodigeti

! -------------------------------------------------------------------------
! Screen model events with information of intervals of no-events
! -------------------------------------------------------------------------
SUBROUTINE fodigeti_screenModWithNotEvnt( evnt, nEvnt )

  USE m_bern,     ONLY: i4b, r8b
  USE m_time,     ONLY: OPERATOR(.isIn.)

  USE p_fodits,   ONLY: t_evnt, &
                        flagnot, infoinit, typeperi, flagset, &
                        sCoreModAddEvnt, sCoreModDelEvnt

  IMPLICIT NONE

  ! Subroutine arguments
  ! --------------------
  TYPE(t_evnt),DIMENSION(:),POINTER               :: evnt
  INTEGER(i4b)                                    :: nEvnt

  ! Local Variables
  ! ---------------
  INTEGER(i4b)                                    :: iEvnt
  INTEGER(i4b)                                    :: jEvnt
  INTEGER(i4b)                                    :: kEvnt
  INTEGER(i4b)                                    :: seen

  INTEGER(i4b)                                    :: tEvnt
  TYPE(t_evnt),DIMENSION(:),POINTER               :: tmpEvnt

  ! Subroutine
  ! ----------

  ! Fill a temporary array
  tEvnt = 0
  NULLIFY(tmpEvnt)
  DO iEvnt = 1,nEvnt
     CALL sCoreModAddEvnt( evnt(iEvnt), tmpEvnt, tEvnt )
  END DO

  ! Delete events according to the mask
  ! Loop over all masks
  DO iEvnt = 1,tEvnt
     IF( tmpEvnt(iEvnt)%flag /= flagNot )CYCLE
     ! Loop over all evnts
     seen = 0
     DO kEvnt = 1,tEvnt
        DO jEvnt = 1,nEvnt
           ! Filters
           IF( evnt(jEvnt)%info == infoInit )CYCLE
           IF( evnt(jEvnt)%type == typePeri )CYCLE
           IF( evnt(jEvnt)%flag == flagNot )CYCLE
           IF( evnt(jEvnt)%flag == flagSet )CYCLE
           IF( evnt(jEvnt)%type /= tmpEvnt(iEvnt)%type )CYCLE
           IF( evnt(jEvnt)%mjd .isIn. tmpEvnt(iEvnt)%timint )THEN
              CALL sCoreModDelEvnt( evnt, nEvnt, jEvnt )
              seen = 1
              EXIT
           END IF
        END DO
        IF( seen == 0 )EXIT
     END DO
  END DO

  ! Delete temporary array
  CALL sCoreModDelEvnt( tmpEvnt, tEvnt )

  RETURN

END SUBROUTINE fodigeti_screenModWithNotEvnt


! -------------------------------------------------------------------------
! Screen model events for earthquakes: remove afterquakes within few epochs
! -------------------------------------------------------------------------
SUBROUTINE fodigeti_screenModForErqs( iSta, sCore, distErqsMjd )

  USE m_bern,     ONLY: i4b, r8b

  USE p_fodits,   ONLY: t_sCore, t_evnt, &
                        infoerqf, &
                        sCoreModAddEvnt, sCoreModDelEvnt, scoresorteventststup

  IMPLICIT NONE

  ! Subroutine arguments
  ! --------------------
  INTEGER(i4b),INTENT(IN)                         :: iSta
  TYPE(t_sCore)                                   :: sCore
  REAL(r8b),INTENT(IN)                            :: distErqsMjd

  ! Local Variables
  ! ---------------
  INTEGER(i4b)                                    :: iErq
  INTEGER(i4b)                                    :: jErq
  INTEGER(i4b)                                    :: iEvnt
  INTEGER(i4b)                                    :: seen
  INTEGER(i4b)                                    :: seen2

  INTEGER(i4b)                                    :: tErq
  TYPE(t_evnt),DIMENSION(:),POINTER               :: tmpErq

  ! Subroutine
  ! ----------

  ! Fill a temporary array with only earthquake events
  tErq = 0
  NULLIFY(tmpErq)
  DO iEvnt = 1,sCore%sta(iSta)%mod%nEvnt
     IF( sCore%sta(iSta)%mod%evnt(iEvnt)%info /= infoErqF )CYCLE
     CALL sCoreModAddEvnt( sCore%sta(iSta)%mod%evnt(iEvnt), tmpErq, tErq )
  END DO

  ! Remove smaller earthquakes within few (distErqsMjd) days of larger ones
  DO
     ! Sort earthquakes for %mag (=%stTst)
     CALL sCoreSortEventsTstUp( tmpErq, tErq )
     ! Loop over all remained earthquakes
     seen = 0
     DO iErq = 1,tErq
        DO jErq = 1,tErq
           IF( tmpErq(jErq)%mjd <= tmpErq(iErq)%mjd  )CYCLE
           IF( tmpErq(jErq)%mjd - tmpErq(iErq)%mjd < distErqsMjd )THEN
              CALL sCoreModDelEvnt( tmpErq, tErq, jErq )
              seen = 1
              EXIT
           END IF
        END DO
        IF( seen == 1 )EXIT
     END DO
     IF( seen == 0 )EXIT
  END DO

  ! Synchronize the results with input evnt array
  DO
     seen = 0
     DO iEvnt = 1,sCore%sta(iSta)%mod%nEvnt
        IF( sCore%sta(iSta)%mod%evnt(iEvnt)%info /= infoErqF )CYCLE
        seen2 = 0
        DO iErq = 1,tErq
           IF( sCore%sta(iSta)%mod%evnt(iEvnt)%mjd == tmpErq(iErq)%mjd )THEN
              seen2 = 1
              EXIT
           END IF
        END DO
        IF( seen2 == 0 )THEN
           CALL sCoreModDelEvnt( sCore%sta(iSta)%mod%evnt, &
                                 sCore%sta(iSta)%mod%nEvnt, iEvnt )
           seen = 1
           EXIT
        END IF
     END DO
     IF( seen == 0 )EXIT
  END DO

  ! Delete temporary array
  CALL sCoreModDelEvnt( tmpErq, tErq )

  RETURN

END SUBROUTINE fodigeti_screenModForErqs


END MODULE s_FODIGETI
