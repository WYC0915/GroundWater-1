MODULE s_FODIPUTO
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE fodiputo(opt,sCore,datFil)

! -------------------------------------------------------------------------
! Purpose:    Store the ATI model (derived from the FODITS analysis) into
!             STA,EVL,CRD, and VEL files.
!
! Author:     Luca Ostini
!
! Created:    14-Aug-2008
! Last mod.:  28-Oct-2010
!
! Changes:    14-Aug-2008 LO: Created this file
!             02-Oct-2008 LO: First revision
!             09-Oct-2008 LO: Third revision
!             13-Oct-2008 LO: Interval correction for STA-file removed
!             05-Dec-2008 LO: Fourth revisio: velocity changes allowed
!             11-Feb-2009 LO: Fifth revision: major changes
!             11-Mar-2009 LO: Fourier Series implemented
!             15-Jul-2009 LO: Merge information for STA-file changed
!             20-Aug-2009 LO: Addition of outliers due to too few observations
!             25-Sep-2009 LO: Changes for F90 consistency
!             18-Nov-2009 LO: Major changes for consistency
!             21-Dec-2009 LO: FFT removed and several changes apported
!             02-Mar-2010 LO: Major changes do to elimination of FODISUBR
!             07-Apr-2010 LO: Major changes do to algorithm and output file
!             16-Jun-2010 LO: Time series diveded by component
!             26-Aug-2010 LO: Architectural changes
!             28-Oct-2010 SL: use p_fodits with ONLY
!             03-Jan-2010 LO: INTENT(INOUT) removed and bug fixed
!             19-Jul-2011 LO: Test datum defintion added
!             31-Aug-2011 LO: typeNot intervals fixed + 1s sampling
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Used Modules
! ------------
! structures, variables:
  USE m_bern,    ONLY: i4b, r8b, shortLineLength, lfnPrt
  USE m_maxdim,  ONLY: maxsta
  USE m_time,    ONLY: t_timint, OPERATOR(.isIn.)
  USE d_const,   ONLY: pi
  USE d_stacrx,  ONLY: init_staCrux, t_renamSta
  USE p_fodits,  ONLY: t_opt, t_sCore, t_datFil, t_evl_sta_evnt, &
                       flgStaFileForFodits, type3sepline, typeoutl, &
                       flagnot, scorenumtoflag, flagtst, flagest, &
                       scorenumtotype, typerate, typeperi, significant, &
                       infostaf, flagset, infoevlf, infoerqf, infopanl, &
                       typejump, infounkw, typevelo, sCoreTypeToNum

! operator, methods:
!  USE d_debug,   ONLY: debug_entry, debug_exit

! subroutines, functions:
  USE s_timst2
!  USE s_ellecc
  USE s_alcerr

! no implicit
  IMPLICIT NONE

! subroutine name
  CHARACTER(LEN=8), PARAMETER    :: srName = 'fodiputo'
  CHARACTER(LEN=10), PARAMETER   :: txtRmk = 'FODITS'


! List of Arguments
! -----------------
! input:
  TYPE(t_opt)                    :: opt        ! Option structure
  TYPE(t_sCore)                  :: sCore      ! Core structure of FODITS
  TYPE(t_datFil)                 :: datFil     ! I/O files structure

! input/output:

! output:


! Local Types
! -----------


! Local Parameters
! ----------------


! Local Variables
! ---------------
  TYPE(t_renamSta)               :: renStaTmp
  TYPE(t_evl_sta_evnt)           :: dTmp

  CHARACTER(LEN=20)              :: datstr
  CHARACTER(LEN=20)              :: period
  CHARACTER(LEN=1)               :: signif
  CHARACTER(LEN=1)               :: staIsRef

  INTEGER(i4b)                   :: iac
  INTEGER(i4b)                   :: iCnt
  INTEGER(i4b)                   :: jCnt
  INTEGER(i4b)                   :: kCnt
  INTEGER(i4b)                   :: iSta
  INTEGER(i4b)                   :: jSta
  INTEGER(i4b)                   :: iStaSta
  INTEGER(i4b)                   :: iRen
  INTEGER(i4b)                   :: seen
  INTEGER(i4b)                   :: iEvnt
  INTEGER(i4b)                   :: jEvnt
  INTEGER(i4b)                   :: nEvnt
  INTEGER(i4b)                   :: nStaF
  INTEGER(i4b)                   :: nStaFT
  INTEGER(i4b)                   :: nEvlF
  INTEGER(i4b)                   :: nEvlFT
  INTEGER(i4b)                   :: nErqF
  INTEGER(i4b)                   :: nErqFT
  INTEGER(i4b)                   :: nPanF
  INTEGER(i4b)                   :: nPanFT
  INTEGER(i4b)                   :: nDisc
  INTEGER(i4b)                   :: nDiscT
  INTEGER(i4b)                   :: nVelo
  INTEGER(i4b)                   :: nVeloT
  INTEGER(i4b)                   :: nOutl
  INTEGER(i4b)                   :: nOutlT
  INTEGER(i4b)                   :: nPeri
  INTEGER(i4b)                   :: nPeriT
  INTEGER(i4b)                   :: nTotE
  INTEGER(i4b)                   :: nTotET
  INTEGER(i4b)                   :: iTmp, kTmp, jTmp
  INTEGER(i4b)                   :: nTotOutl
  INTEGER(i4b)                   :: nItSt
  INTEGER(i4b)                   :: nDiscTT, nDiscTTT
  INTEGER(i4b)                   :: nVeloTT, nVeloTTT
  INTEGER(i4b)                   :: nTotEvnt
  INTEGER(i4b)                   :: nMaxSta
  INTEGER(i4b)                   :: typeElem

  REAL(r8b)                      :: outlTimeHalf
  REAL(r8b)                      :: oneMinInDays
  REAL(r8b)                      :: meanEpoch
  REAL(r8b)                      :: stTstVal


! Call debug routine
! ------------------
!  CALL debug_entry(srName)


! Initialization of all variables
! -------------------------------

  ! Define 1 minute in days
  oneMinInDays = 1.0D0 / ( 60.0D0 * 24.0D0)
  ! Define the half interval for STA-File TYPE 003 (= 5 seconds)
  outlTimeHalf = 15*oneMinInDays
  ! Correction due to 3-days and weekly solutions
  IF     ( opt%updStaNDays == 3 )THEN
     outlTimeHalf = outlTimeHalf + 1.0D0
  ELSE IF( opt%updStaNDays == 7 )THEN
     outlTimeHalf = outlTimeHalf + 4.0D0
  END IF

  ! Initialization
  CALL init_staCrux(datFil%staOut)
  ! Get the total number of events to allocate memory
  nTotEvnt = 0
  DO iSta = 1,sCore%nSta
     nTotEvnt = nTotEvnt + sCore%sta(iSta)%mod%nEvnt
  END DO
  nMaxSta = datFil%staIn%nrenam + nTotEvnt
  nTotOutl = datFil%staIn%nprob + nTotEvnt

  ! Allocation memory
  ALLOCATE(datFil%staOut%renamsta(nMaxSta), stat=iac)
  CALL alcerr(iac,'datFil%staOut%renamsta', (/nMaxSta/),srName)
  ALLOCATE(datFil%staOut%stainfo(datFil%staIn%ninfo), stat=iac)
  CALL alcerr(iac, 'datFil%staOut%stainfo', (/datFil%staIn%ninfo/),srName)
  ALLOCATE(datFil%staOut%staprob(nTotOutl), stat=iac)
  CALL alcerr(iac, 'datFil%staOut%staprob', (/nTotOutl/),srName)
  ALLOCATE(datFil%staOut%coovel(nMaxSta), stat=iac)
  CALL alcerr(iac, 'datFil%staOut%coovel', (/nMaxSta/),srName)
  ALLOCATE(datFil%staOut%statype(nMaxSta), stat=iac)
  CALL alcerr(iac, 'datFil%StaOut%statype', (/nMaxSta/),srName)

  ! Copy input STA-file (tsCore%stI) to the output STA-file (datFil%staOut):
  ! - TYPE 001, RENAMING OF STATIONS
  datFil%staOut%nrenam = 0
  DO iCnt = 1,datFil%staIn%nrenam
     IF( datFil%staIn%renamsta(iCnt)%remark(1:6) == txtRmk )CYCLE
     datFil%staOut%nrenam = datFil%staOut%nrenam + 1
     datFil%staOut%renamsta(datFil%staOut%nrenam) = datFil%staIn%renamsta(iCnt)
  END DO
  ! - TYPE 002, STATION INFORMATION
  datFil%staOut%ninfo = datFil%staIn%ninfo
  DO iCnt = 1,datFil%staIn%ninfo
     datFil%staOut%stainfo(iCnt) = datFil%staIn%stainfo(iCnt)
  END DO
  ! - TYPE 003, HANDLING OF STATION PROBLEMS
  ! Copy old entries to new entries
  datFil%staOut%nprob = 0
  DO iCnt = 1,datFil%staIn%nprob
     IF( datFil%staIn%staprob(iCnt)%remark(1:6) == txtRmk )CYCLE
     datFil%staOut%nprob = datFil%staOut%nprob + 1
     datFil%staOut%staprob(datFil%staOut%nprob) = datFil%staIn%staprob(iCnt)
  END DO
  datFil%staOut%nprob = datFil%staOut%nprob
  ! - TYPE 004, STATION COORDINATES AND VELOCITIES (ADDNEQ)
  datFil%staOut%ncoovel = 0
  ! - TYPE 005, HANDLING STATION TYPES
  datFil%staOut%nstatype = datFil%staIn%nstatype
  DO iCnt = 1,datFil%staIn%nstatype
     datFil%staOut%statype(iCnt) = datFil%staIn%statype(iCnt)
  END DO

  ! Merge the new information from FODITS to TYPE 001: station renaming
  DO iSta = 1,sCore%nSta
     DO iRen = 1,sCore%sta(iSta)%upd%nRen
        IF( sCore%sta(iSta)%upd%ren(iRen)%rename == '  ' )CYCLE
        datFil%staOut%nrenam = datFil%staOut%nrenam + 1
        datFil%staOut%renamsta(datFil%staOut%nrenam)%oldnam = &
             sCore%sta(iSta)%name
        datFil%staOut%renamsta(datFil%staOut%nrenam)%oldnam(15:16) = '  '
        datFil%staOut%renamsta(datFil%staOut%nrenam)%stanam = &
             sCore%sta(iSta)%name
        datFil%staOut%renamsta(datFil%staOut%nrenam)%stanam(15:16) = &
             sCore%sta(iSta)%upd%ren(iRen)%rename
        datFil%staOut%renamsta(datFil%staOut%nrenam)%flg = &
             flgStaFileForFodits
        datFil%staOut%renamsta(datFil%staOut%nrenam)%timint%t(1) = &
             sCore%sta(iSta)%upd%ren(iRen)%timint%t(1)
        datFil%staOut%renamsta(datFil%staOut%nrenam)%timint%t(2) = &
             sCore%sta(iSta)%upd%ren(iRen)%timint%t(2)
        ! Expand the first t(1) to 0.0D
        IF( iRen == 1 )THEN
           datFil%staOut%renamsta(datFil%staOut%nrenam)%timint%t(1) = 0.0D0
        END IF
        ! Expand the last t(2) to 1.0D20
        IF( iRen == sCore%sta(iSta)%upd%nRen )THEN
           datFil%staOut%renamsta(datFil%staOut%nrenam)%timint%t(2) = 1.0D20
        END IF
        datFil%staOut%renamsta(datFil%staOut%nrenam)%remark = txtRmk
     END DO
  END DO
  ! Sort info in terms of (1st) oldnam and (2nd) timint%t(1)
  DO iCnt = 1,datFil%staOut%nrenam-1
     kCnt = iCnt
     DO jCnt = iCnt+1,datFil%staOut%nrenam
        IF(  datFil%staOut%renamsta(jCnt)%oldnam < &
             datFil%staOut%renamsta(kCnt)%oldnam .OR. &
             (  datFil%staOut%renamsta(jCnt)%oldnam == &
                datFil%staOut%renamsta(kCnt)%oldnam .AND. &
                (  datFil%staOut%renamsta(jCnt)%flg < &
                   datFil%staOut%renamsta(kCnt)%flg .OR. &
                   (  datFil%staOut%renamsta(jCnt)%flg == &
                   datFil%staOut%renamsta(kCnt)%flg .AND. &
                   datFil%staOut%renamsta(jCnt)%timint%t(1) < &
                   datFil%staOut%renamsta(kCnt)%timint%t(1) &
                   ) &
                ) &
             ) &
          )THEN
           kCnt = jCnt
        ENDIF
     END DO
     IF( kCnt /= iCnt )THEN
        renStaTmp = datFil%staOut%renamsta(iCnt)
        datFil%staOut%renamsta(iCnt) = datFil%staOut%renamsta(kCnt)
        datFil%staOut%renamsta(kCnt) = renStaTmp
     ENDIF
  END DO
  ! TYPE 002: The station information
  ! - do nothing

  ! TYPE 003: Station problems
  ! Add a new separator line for FODITS if it does not yet exists
  seen = 0
  DO iCnt = 1,datFil%staOut%nprob
     IF( datFil%staOut%staprob(iCnt)%stanam == type3SepLine )THEN
        seen = 1
        EXIT
     END IF
  END DO
  IF( seen == 0 .AND. LEN_TRIM(opt%outStaFileForAddneq2) /= 0 )THEN
     datFil%staOut%nprob = datFil%staOut%nprob + 1
     datFil%staOut%staprob(datFil%staOut%nprob)%stanam = type3SepLine
     datFil%staOut%staprob(datFil%staOut%nprob)%flg = flgStaFileForFodits
     datFil%staOut%staprob(datFil%staOut%nprob)%timint%t(1) = 0.0D0
     datFil%staOut%staprob(datFil%staOut%nprob)%timint%t(2) = 1.0D20
     datFil%staOut%staprob(datFil%staOut%nprob)%remark = txtRmk
  END IF
  ! Add all significant outliers detected by FODITS
  DO iSta = 1,sCore%nSta
     DO iCnt = 1,sCore%sta(iSta)%mod%nEvnt
        IF( sCore%sta(iSta)%mod%evnt(iCnt)%type /= typeOutl )CYCLE
        IF( sCore%sta(iSta)%mod%evnt(iCnt)%siTst /= 1 .AND. &
             sCore%sta(iSta)%mod%evnt(iCnt)%flag /= flagSet )CYCLE
        datFil%staOut%nprob = datFil%staOut%nprob + 1
        datFil%staOut%staprob(datFil%staOut%nprob)%stanam = &
             sCore%sta(iSta)%name
        datFil%staOut%staprob(datFil%staOut%nprob)%flg = &
             flgStaFileForFodits
        datFil%staOut%staprob(datFil%staOut%nprob)%timint%t(1) = &
             sCore%sta(iSta)%mod%evnt(iCnt)%mjd - outlTimeHalf
        datFil%staOut%staprob(datFil%staOut%nprob)%timint%t(2) = &
             sCore%sta(iSta)%mod%evnt(iCnt)%mjd + outlTimeHalf
        datFil%staOut%staprob(datFil%staOut%nprob)%remark = txtRmk
     END DO
  END DO
  ! Rename the outliers (i.e., stations) according to TYPE 001
  DO iCnt = 1,datFil%staOut%nprob
     IF( datFil%staOut%staprob(iCnt)%remark == type3SepLine .OR. &
          datFil%staOut%staprob(iCnt)%remark /= txtRmk )CYCLE
     DO jCnt = 1,datFil%staOut%nrenam
        IF(  datFil%staOut%renamsta(jCnt)%oldnam == &
             datFil%staOut%staprob(iCnt)%stanam )THEN
           ! check interval
           meanEpoch = ( datFil%staOut%staprob(iCnt)%timint%t(2) + &
                datFil%staOut%staprob(iCnt)%timint%t(1) )/2.0D0
           IF( meanEpoch >= datFil%staOut%renamsta(jCnt)%timint%t(1) .AND. &
                meanEpoch <  datFil%staOut%renamsta(jCnt)%timint%t(2) )THEN
              datFil%staOut%staprob(iCnt)%stanam = &
                   datFil%staOut%renamsta(jCnt)%stanam
           END IF
        END IF
     END DO
  END DO

  ! TYPE 004, Station coordinates and velocities (ADDNEQ)
  ! - done in fodiupdf after have adapted the FIX file
  ! TYPE 005, Handling station types
  ! - do nothing

  ! sCore%...%mod  -> datFil%evlOut (put information from EVL file)
  ! ---------------------------------------------------------------
  ALLOCATE(datFil%evlOut%sta(sCore%nSta),stat=iac)
  CALL alcerr(iac,'datFil%evlOut%sta',(/sCore%nSta/),srName)
  datFil%evlOut%nSta = sCore%nSta

  DO iSta = 1,sCore%nSta

     datFil%evlOut%sta(iSta)%name = sCore%sta(iSta)%name

     ! Number of entries
     nEvnt = 0
     ! NOT entries already in EVL input
     DO jSta = 1,datFil%evlIn%nSta
        IF( datFil%evlIn%sta(jSta)%name == datFil%evlOut%sta(iSta)%name )THEN
           DO iEvnt = 1,datFil%evlIn%sta(jSta)%nEvnt
              IF( datFil%evlIn%sta(jSta)%evnt(iEvnt)%flag == &
                  sCoreNumToFlag(flagNot) )THEN
                 nEvnt = nEvnt + 1
              END IF
           END DO
        END IF
     END DO
     ! add number of events in functional model
     nEvnt = nEvnt + sCore%sta(iSta)%mod%nEvnt
     datFil%evlOut%sta(iSta)%nEvnt = nEvnt

     IF( nEvnt == 0 )CYCLE

     ALLOCATE(datFil%evlOut%sta(iSta)%evnt(nEvnt),stat=iac)
     CALL alcerr(iac,'datFil%evlOut%sta(iSta)%evnt',(/nEvnt/),srName)

     ! Correction of mjd and mjd2 for outliers
     DO iEvnt = 1,sCore%sta(iSta)%mod%nEvnt
        IF( sCore%sta(iSta)%mod%evnt(iEvnt)%type == typeOutl )THEN
           sCore%sta(iSta)%mod%evnt(iEvnt)%mjd2 = &
                sCore%sta(iSta)%mod%evnt(iEvnt)%mjd + &
                sCore%deltaMjd/2.0D0 - outlTimeHalf
           sCore%sta(iSta)%mod%evnt(iEvnt)%mjd = &
                sCore%sta(iSta)%mod%evnt(iEvnt)%mjd - &
                sCore%deltaMjd/2.0D0 + outlTimeHalf
        END IF
     END DO

     nEvnt = 0
     ! Fill EVL with events
     DO iCnt = 1,sCore%sta(iSta)%mod%nEvnt
        ! Filter
        IF( sCore%sta(iSta)%mod%evnt(iCnt)%flag == flagNot )CYCLE
        ! Element
        nEvnt = nEvnt + 1
        IF( sCore%sta(iSta)%mod%evnt(iCnt)%flag == flagTst )THEN
           sCore%sta(iSta)%mod%evnt(iCnt)%flag = flagEst
        END IF
        datFil%evlOut%sta(iSta)%evnt(nEvnt)%flag = &
             sCoreNumToFlag(sCore%sta(iSta)%mod%evnt(iCnt)%flag)
        datFil%evlOut%sta(iSta)%evnt(nEvnt)%type = &
             sCoreNumToType(sCore%sta(iSta)%mod%evnt(iCnt)%type)
        datFil%evlOut%sta(iSta)%evnt(nEvnt)%timint%t(1) = &
             sCore%sta(iSta)%mod%evnt(iCnt)%mjd
        datFil%evlOut%sta(iSta)%evnt(nEvnt)%timint%t(2) = &
             sCore%sta(iSta)%mod%evnt(iCnt)%mjd2
        datFil%evlOut%sta(iSta)%evnt(nEvnt)%omega = &
             sCore%sta(iSta)%mod%evnt(iCnt)%omega
        datFil%evlOut%sta(iSta)%evnt(nEvnt)%val = &
             sCore%sta(iSta)%mod%evnt(iCnt)%val
        datFil%evlOut%sta(iSta)%evnt(nEvnt)%par(:) = &
             sCore%sta(iSta)%mod%evnt(iCnt)%par(:)
        datFil%evlOut%sta(iSta)%evnt(nEvnt)%dVal = &
             sCore%sta(iSta)%mod%evnt(iCnt)%dVal
        datFil%evlOut%sta(iSta)%evnt(nEvnt)%stTst = &
             sCore%sta(iSta)%mod%evnt(iCnt)%stTst
        datFil%evlOut%sta(iSta)%evnt(nEvnt)%siTst = &
             sCore%sta(iSta)%mod%evnt(iCnt)%siTst
        datFil%evlOut%sta(iSta)%evnt(nEvnt)%remark = &
             sCore%sta(iSta)%mod%evnt(iCnt)%remark
        datFil%evlOut%sta(iSta)%evnt(nEvnt)%par(:) = &
             sCore%sta(iSta)%mod%evnt(iCnt)%par(:)
        datFil%evlOut%sta(iSta)%evnt(nEvnt)%val = &
             datFil%evlOut%sta(iSta)%evnt(nEvnt)%val
        datFil%evlOut%sta(iSta)%evnt(nEvnt)%dVal = &
             datFil%evlOut%sta(iSta)%evnt(nEvnt)%dVal
        datFil%evlOut%sta(iSta)%evnt(nEvnt)%phi(:) = &
             sCore%sta(iSta)%mod%evnt(iCnt)%phi(:)
     END DO

     ! Recompute offset to sCore%outTimRefCrd (output reference epoch)
     DO iEvnt = 1,datFil%evlOut%sta(iSta)%nEvnt
       IF( datFil%evlOut%sta(iSta)%evnt(iEvnt)%remark == 'OFFST-TS' )THEN
         DO jEvnt = 1,datFil%evlOut%sta(iSta)%nEvnt
           IF( datFil%evlOut%sta(iSta)%evnt(jEvnt)%remark == 'DRIFT-TS' )THEN
              datFil%evlOut%sta(iSta)%evnt(iEvnt)%par(:) = &
                 datFil%evlOut%sta(iSta)%evnt(iEvnt)%par(:) + &
                 datFil%evlOut%sta(iSta)%evnt(jEvnt)%par(:) * &
                 ( sCore%outTimRefCrd - &
                   datFil%evlOut%sta(iSta)%evnt(jEvnt)%timint%t(1) )
              datFil%evlOut%sta(iSta)%evnt(iEvnt)%val = &
                 datFil%evlOut%sta(iSta)%evnt(iEvnt)%val + &
                 datFil%evlOut%sta(iSta)%evnt(jEvnt)%val * &
                 ( sCore%outTimRefCrd - &
                   datFil%evlOut%sta(iSta)%evnt(jEvnt)%timint%t(1) )
              datFil%evlOut%sta(iSta)%evnt(iEvnt)%dVal = &
                 datFil%evlOut%sta(iSta)%evnt(iEvnt)%dVal + &
                 datFil%evlOut%sta(iSta)%evnt(jEvnt)%dVal * &
                 ( sCore%outTimRefCrd - &
                   datFil%evlOut%sta(iSta)%evnt(jEvnt)%timint%t(1) )
           END IF
         END DO
       END IF
     END DO

     ! Transform typeVelo and typeRate from "m/s" to "m/y"
     DO jEvnt = 1,datFil%evlOut%sta(iSta)%nEvnt
       typeElem = sCoreTypeToNum(datFil%evlOut%sta(iSta)%evnt(jEvnt)%type)
       IF( typeElem /= typeRate .AND. typeElem /= typeVelo )CYCLE
       datFil%evlOut%sta(iSta)%evnt(jEvnt)%par(:) = &
          datFil%evlOut%sta(iSta)%evnt(jEvnt)%par(:) * 365.25D0
       datFil%evlOut%sta(iSta)%evnt(jEvnt)%val = &
          datFil%evlOut%sta(iSta)%evnt(jEvnt)%val * 365.25D0
       datFil%evlOut%sta(iSta)%evnt(jEvnt)%dVal = &
          datFil%evlOut%sta(iSta)%evnt(jEvnt)%dVal * 365.25D0
     END DO

     ! Insert (report from evlIn) the NOT events
     DO jSta = 1,datFil%evlIn%nSta
        IF( datFil%evlIn%sta(jSta)%name == datFil%evlOut%sta(iSta)%name )THEN
           DO iEvnt = 1,datFil%evlIn%sta(jSta)%nEvnt
              IF( datFil%evlIn%sta(jSta)%evnt(iEvnt)%flag == &
                  sCoreNumToFlag(flagNot) )THEN
                 nEvnt = nEvnt + 1
                 datFil%evlOut%sta(iSta)%evnt(nEvnt) = &
                      datFil%evlIn%sta(jSta)%evnt(iEvnt)
              END IF
           END DO
        END IF
     END DO

     ! Assign the true number of events
     datFil%evlOut%sta(iSta)%nEvnt = nEvnt

     ! Sort aInfo
     DO iTmp = 1,nEvnt-1
        kTmp = iTmp
        DO jTmp = iTmp+1,nEvnt
           IF( datFil%evlOut%sta(iSta)%evnt(jTmp)%timint%t(1) < &
               datFil%evlOut%sta(iSta)%evnt(kTmp)%timint%t(1) .OR. &
                ( datFil%evlOut%sta(iSta)%evnt(jTmp)%timint%t(1) == &
                  datFil%evlOut%sta(iSta)%evnt(kTmp)%timint%t(1) .AND. &
                  datFil%evlOut%sta(iSta)%evnt(jTmp)%omega > &
                  datFil%evlOut%sta(iSta)%evnt(kTmp)%omega ) )THEN
              kTmp = jTmp
           END IF
        END DO
        IF( kTmp /= iTmp )THEN
           dTmp = datFil%evlOut%sta(iSta)%evnt(iTmp)
           datFil%evlOut%sta(iSta)%evnt(iTmp) = &
                datFil%evlOut%sta(iSta)%evnt(kTmp)
           datFil%evlOut%sta(iSta)%evnt(kTmp) = dTmp
        END IF
     END DO

  END DO

! Write informations to output file (lfnprt)
! ------------------------------------------

  ! Title
  WRITE(lfnprt,'(/,/,A,/,A)')                &
       ' OUTPUT: A POSTERIORI INFORMATION - ESTIMATES', &
       ' --------------------------------------------'
  DO iSta = 1,sCore%nSta

     IF( datFil%evlOut%sta(iSta)%nEvnt == 0 )CYCLE

     WRITE(lfnprt,'(A,/,A,/,A)')  &
     '                                                    Period            &
     &Estimates [m,m/year]                StdDev        Stat       ',&
     '  Nr Station          Evnt Flg Epoch (From)         [days]   Remark   &
     &North       East        Up          All           test S KYWD',&
     '----------------------------------------------------------------------&
     &-------------------------------------------------------------'
     DO iEvnt = 1,datFil%evlOut%sta(iSta)%nEvnt

        datstr = ''
        IF(  datFil%evlOut%sta(iSta)%evnt(iEvnt)%timint%t(1) > 1.0D04 .AND. &
             datFil%evlOut%sta(iSta)%evnt(iEvnt)%timint%t(1) < 1.0D20 )THEN
           ! Correction due to mean Epoch in ADDNEQ2
           typeElem = sCoreTypeToNum(datFil%evlOut%sta(iSta)%evnt(iEvnt)%type)
           meanEpoch = datFil%evlOut%sta(iSta)%evnt(iEvnt)%timint%t(1)
           IF( typeElem == typeOutl )THEN
              meanEpoch = ( datFil%evlOut%sta(iSta)%evnt(iEvnt)%timint%t(1) + &
                            datFil%evlOut%sta(iSta)%evnt(iEvnt)%timint%t(2) )/&
                           2.0D0
           END IF
           CALL timst2(1,1,meanEpoch,datstr)
        END IF

        period = '9999.9999'
        IF( datFil%evlOut%sta(iSta)%evnt(iEvnt)%omega > 0.0D0 )THEN
           WRITE(period,'(F10.4)') &
                2*pi/datFil%evlOut%sta(iSta)%evnt(iEvnt)%omega
        END IF

        signif = 'N'
        IF( datFil%evlOut%sta(iSta)%evnt(iEvnt)%siTst == significant )THEN
           signif = 'Y'
        END IF

        stTstVal = datFil%evlOut%sta(iSta)%evnt(iEvnt)%stTst
        IF( stTstVal >= 1.0D3 ) stTstVal = 999.9D0

        WRITE(lfnprt,'(I04,1X,A16,1X,A4,1X,A3,1X,A20,&
                      &A9,1X,A8,1X,3(E11.5,1X),E11.5,1X,F6.2,1X,A1,1X,A4)') &
                      iSta, &
                      sCore%sta(iSta)%name, &
                      datFil%evlOut%sta(iSta)%evnt(iEvnt)%type, &
                      datFil%evlOut%sta(iSta)%evnt(iEvnt)%flag, &
                      datstr, &
                      period, &
                      datFil%evlOut%sta(iSta)%evnt(iEvnt)%remark, &
                      datFil%evlOut%sta(iSta)%evnt(iEvnt)%par(:), &
                      datFil%evlOut%sta(iSta)%evnt(iEvnt)%dVal, &
                      stTstVal, &
                      signif, &
                      'APOS'

        ! Additional information on phase
        IF( datFil%evlOut%sta(iSta)%evnt(iEvnt)%type == 'PERI' )THEN
           WRITE(lfnprt,'(I04,1X,A16,1X,A4,1X,A3,1X,A20,&
                        &A9,1X,A8,1X,3(E11.5,1X),E11.5,1X,F6.2,1X,A1,1X,A4)') &
                        iSta, &
                        sCore%sta(iSta)%name, &
                        'PERP', &
                        datFil%evlOut%sta(iSta)%evnt(iEvnt)%flag, &
                        datstr, &
                        period, &
                        datFil%evlOut%sta(iSta)%evnt(iEvnt)%remark, &
                        datFil%evlOut%sta(iSta)%evnt(iEvnt)%phi(:) *  &
                                                         180.D0 / pi, &
                        0.0D0, &
                        0.0D0, &
                        signif, &
                        'APOS'
        END IF

     END DO

  END DO

  ! Title
  WRITE(lfnprt,'(/,/,2(A,/))')  &
       ' SUMMARY OF RESULTS', &
       ' ------------------'

  ! General summary
  WRITE(lfnprt,'(A,/,A,/,A)')  &
       ' General summary:', &
       ' ----------------', &
       ' R: Y = Reference site before analysis'
  WRITE(lfnprt,'(A,/,A,/,A)')  &
  '                               | signf a priori elements |&
  & new identified elements | Total       | Total | #Itr | A posteriori |  ',&
  '  Nr Station          R #Epoch | #staF #evlF #erqF #panF |&
  & #disc #velo #outl #peri | #disc #velo | #elem | step | sigma (m0)   | K', &
  ' ---------------------------------------------------------&
  &------------------------------------------------------------------------'

  nStaFT = 0
  nEvlFT = 0
  nErqFT = 0
  nPanFT = 0
  nDiscT = 0
  nVeloT = 0
  nOutlT = 0
  nPeriT = 0
  nTotET = 0
  nDiscTTT = 0
  nVeloTTT = 0
  nItSt  = 0
  DO iSta = 1,sCore%nSta

     ! Determine whether the station is a reference one
     staIsRef = ''
     DO iStaSta = 1,sCore%sta(iSta)%nStaSta
        IF( sCore%sta(iSta)%staSta(iStaSta)%inCrdFlg == 'W' )THEN
           staIsRef = 'Y'
           EXIT
        END IF
     END DO

     ! Count #staF
     nStaF = 0
     DO iEvnt = 1,sCore%sta(iSta)%mod%nEvnt
        IF( sCore%sta(iSta)%mod%evnt(iEvnt)%info  /= infoStaF )CYCLE
        IF( sCore%sta(iSta)%mod%evnt(iEvnt)%siTst /= significant .AND. &
            sCore%sta(iSta)%mod%evnt(iEvnt)%flag  /= flagSet )CYCLE
        nStaF = nStaF + 1
     END DO
     nStaFT = nStaFT + nStaF
     ! Count #evlF
     nEvlf = 0
     DO iEvnt = 1,sCore%sta(iSta)%mod%nEvnt
        IF( sCore%sta(iSta)%mod%evnt(iEvnt)%info /= infoEvlf )CYCLE
        IF( sCore%sta(iSta)%mod%evnt(iEvnt)%siTst /= significant .AND. &
            sCore%sta(iSta)%mod%evnt(iEvnt)%flag  /= flagSet )CYCLE
        nEvlf = nEvlf + 1
     END DO
     nEvlFT = nEvlFT + nEvlF
     ! Count #erqF
     nErqf = 0
     DO iEvnt = 1,sCore%sta(iSta)%mod%nEvnt
        IF( sCore%sta(iSta)%mod%evnt(iEvnt)%info /= infoErqf )CYCLE
        IF( sCore%sta(iSta)%mod%evnt(iEvnt)%siTst /= significant .AND. &
            sCore%sta(iSta)%mod%evnt(iEvnt)%flag  /= flagSet )CYCLE
        nErqf = nErqf + 1
     END DO
     nErqFT = nErqFT + nErqF
     ! Count #panF
     nPanf = 0
     DO iEvnt = 1,sCore%sta(iSta)%mod%nEvnt
        IF( sCore%sta(iSta)%mod%evnt(iEvnt)%info /= infoPanl )CYCLE
        IF( sCore%sta(iSta)%mod%evnt(iEvnt)%siTst /= significant .AND. &
            sCore%sta(iSta)%mod%evnt(iEvnt)%flag  /= flagSet )CYCLE
        nPanf = nPanf + 1
     END DO
     nPanFT = nPanFT + nPanF

     ! Count #disc
     nDisc = 0
     DO iEvnt = 1,sCore%sta(iSta)%mod%nEvnt
        IF( sCore%sta(iSta)%mod%evnt(iEvnt)%type  /= typeJump )CYCLE
        IF( sCore%sta(iSta)%mod%evnt(iEvnt)%info  /= infoUnkw )CYCLE
        IF( sCore%sta(iSta)%mod%evnt(iEvnt)%siTst /= significant )CYCLE
        nDisc = nDisc + 1
     END DO
     nDiscT = nDiscT +nDisc
     ! Count #velo
     nVelo = 0
     DO iEvnt = 1,sCore%sta(iSta)%mod%nEvnt
        IF( sCore%sta(iSta)%mod%evnt(iEvnt)%type  /= typeVelo )CYCLE
        IF( sCore%sta(iSta)%mod%evnt(iEvnt)%info  /= infoUnkw )CYCLE
        IF( sCore%sta(iSta)%mod%evnt(iEvnt)%siTst /= significant )CYCLE
        nVelo = nVelo + 1
     END DO
     nVeloT = nVeloT +nVelo
     ! Count #outl
     nOutl = 0
     DO iEvnt = 1,sCore%sta(iSta)%mod%nEvnt
        IF( sCore%sta(iSta)%mod%evnt(iEvnt)%type  /= typeOutl )CYCLE
        IF( sCore%sta(iSta)%mod%evnt(iEvnt)%info  /= infoUnkw )CYCLE
        IF( sCore%sta(iSta)%mod%evnt(iEvnt)%siTst /= significant )CYCLE
        nOutl = nOutl + 1
     END DO
     nOutlT = nOutlT +nOutl
     ! Count #peri
     nPeri = 0
     DO iEvnt = 1,sCore%sta(iSta)%mod%nEvnt
        IF( sCore%sta(iSta)%mod%evnt(iEvnt)%type  /= typePeri )CYCLE
        IF( sCore%sta(iSta)%mod%evnt(iEvnt)%info  /= infoUnkw )CYCLE
        IF( sCore%sta(iSta)%mod%evnt(iEvnt)%siTst /= significant )CYCLE
        nPeri = nPeri + 1
     END DO
     nPeriT = nPeriT +nPeri

     ! Count total #disc
     nDiscTT = 0
     DO iEvnt = 1,sCore%sta(iSta)%mod%nEvnt
        IF( sCore%sta(iSta)%mod%evnt(iEvnt)%type  /= typeJump )CYCLE
        IF( sCore%sta(iSta)%mod%evnt(iEvnt)%siTst /= significant )CYCLE
        nDiscTT = nDiscTT + 1
     END DO
     nDiscTTT = nDiscTTT + nDiscTT
     ! Count total #velo
     nVeloTT = 0
     DO iEvnt = 1,sCore%sta(iSta)%mod%nEvnt
        IF( sCore%sta(iSta)%mod%evnt(iEvnt)%type  /= typeVelo )CYCLE
        IF( sCore%sta(iSta)%mod%evnt(iEvnt)%siTst /= significant )CYCLE
        nVeloTT = nVeloTT + 1
     END DO
     nVeloTTT = nVeloTTT + nVeloTT
     ! Count #totE
     nTotE = 0
     DO iEvnt = 1,sCore%sta(iSta)%mod%nEvnt
        IF( sCore%sta(iSta)%mod%evnt(iEvnt)%siTst /= significant .AND. &
            sCore%sta(iSta)%mod%evnt(iEvnt)%flag  /= flagSet )CYCLE
        nTotE = nTotE + 1
     END DO
     nTotET = nTotET +nTotE

     ! Count #nItSt
     nItSt = nItSt + sCore%sta(iSta)%nIterSteps

     WRITE(lfnprt,'(I4,1X,A16,1X,A1,I7,1X,A1,&
                   &4(1X,I5),1X,A1,&
                   &4(1X,I5),1X,A1,&
                   &2(1X,I5),1X,A1,&
                   &1X,I5,1X,A1,&
                   &1X,I4,1X,A1,&
                   &1X,E12.6,1X,A1,&
                   &1X,A1)') &
          iSta, sCore%sta(iSta)%name, staIsRef, sCore%sta(iSta)%ts%nMjd, '|', &
          nStaF, nEvlF, nErqF, nPanF, '|', &
          nDisc, nVelo, nOutl, nPeri, '|', &
          nDiscTT, nVeloTT, '|', &
          nTotE, '|', &
          sCore%sta(iSta)%nIterSteps, '|', &
          sCore%sta(iSta)%modRms, '|',&
          'Z'
  END DO

  WRITE(lfnprt,'(A)')  &
  ' ---------------------------------------------------------&
  &------------------------------------------------------------------------'
  WRITE(lfnprt,'(A14,17X,A1,&
       &4(1X,I5),1X,A1,&
       &4(1X,I5),1X,A1,&
       &2(1X,I5),1X,A1,&
       &1X,I5,1X,A1,&
       &1X,I4,1X,A1)') &
       ' Summary total','|', &
       nStaFT, nEvlFT, nErqFT, nPanFT, '|', &
       nDiscT, nVeloT, nOutlT, nPeriT, '|', &
       nDiscTTT, nVeloTTT, '|', &
       nTotET, '|', &
       nItSt, '|'

! End of subroutine
! -----------------
!  CALL debug_exit(srName)
  RETURN

END SUBROUTINE fodiputo

END MODULE s_FODIPUTO
