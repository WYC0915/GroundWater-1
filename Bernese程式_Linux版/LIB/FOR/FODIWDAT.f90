MODULE s_FODIWDAT
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE fodiwdat(opt,sCore,datFil)

! -------------------------------------------------------------------------
! Purpose:    Tasks:
!             - Write the sCore%sta(iSta)%ts%... into the PLT-file
!             - Write the sCore%sta(iSta)%outCrd into the CRD-file
!             - Write the sCore%sta(iSta)%outVel into the VEL-file
!             Hidden task:
!             - Create synthetic time series for test purposes.
!
! Author:     Luca Ostini
!
! Created:    14-Aug-2008
!
! Changes:    14-Aug-2008 LO: Created this file
!             02-Oct-2008 LO: First revision
!             09-Oct-2008 LO: Third revision
!             05-Dec-2008 LO: Fourth revisio: velocity changes allowed
!             11-Feb-2009 LO: Fifth revision: major changes
!             15-Sep-2009 LO: Write to external file added
!             25-Sep-2009 LO: Changes for F90 consistency
!             18-Nov-2009 LO: Major changes for consistency
!             02-Mar-2010 LO: Major changes do to elimination of FODISUBR
!             16-Jun-2010 LO: Time series diveded by component
!             26-Aug-2010 LO: Architectural changes
!             03-Jan-2010 LO: INTENT(INOUT) removed and bug fixed
!             18-Feb-2011 RD: WTVELO and WTSTAT are not used
!             24-May-2011 LO: New update of ADDNEQ2 meta-data
!             19-Sep-2012 RD: Use P_FODITS with ONLY, remove unused modules
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Used Modules
! ------------
! structures, variables:
  USE m_bern,    ONLY: i4b, r8b, shortLineLength, &
                       lfnPlt, lfnLoc, lfnerr
  USE p_fodits,  ONLY: t_opt, t_score, t_datfil, &
                       inpres

! operator, methods:
!  USE d_debug,   ONLY: debug_entry, debug_exit

! subroutines, functions:
  USE s_timst2
  USE s_opnerr
  USE s_opnfil

! no implicit
  IMPLICIT NONE

! subroutine name
  CHARACTER(LEN=8), PARAMETER    :: srName = 'fodiwdat'


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
  INTEGER(i4b)                   :: ios
  INTEGER(i4b)                   :: iSta
  INTEGER(i4b)                   :: iMjd


! Call debug routine
! ------------------
!  CALL debug_entry(srName)


! Initialization of all variables
! -------------------------------
  ios = 0

  ! Creation of synthetic time series
  IF( opt%selInpTsType == inpRes .AND. &
      LEN_TRIM(opt%inPltFile) == 0 .AND. &
      LEN_TRIM(opt%inCrdFile) == 0 .AND. &
      LEN_TRIM(opt%inVelFile) == 0 .AND. &
      LEN_TRIM(opt%inEvlFile) /= 0 )THEN
     CALL fodiwdat_syntheticTimeSeries(opt,sCore,datFil)
  END IF

  ! Panel 2: OUTPUT FILES: Plot file with residuals (PLT file)
  IF( LEN_TRIM(opt%outPltFile) /= 0 )THEN
     ! Open PLT file
     CALL opnfil(lfnplt,opt%outPltFile,'UNKNOWN','FORMATTED',' ',' ',ios)
     CALL opnerr(lfnerr,lfnloc,ios,opt%outPltFile,srName)
     ! Write the residuals comprised in the stuct sCore%
     DO iSta = 1,sCore%nSta
        DO iMjd = 1,sCore%sta(iSta)%ts%nMjd
           IF( opt%inPltFileVciEna == 0 )THEN
              CALL fodiwdat_pltCrdLine(lfnplt, &
                   sCore%sta(iSta)%name, &
                   iMjd, &
                   sCore%sta(iSta)%ts%mjd(iMjd), &
                   sCore%sta(iSta)%ts%val(iMjd,:), &
                   opt%inPltFileViM0 )
           ELSE IF( opt%inPltFileVciEna == 1 )THEN
              CALL fodiwdat_pltCrdLine(lfnplt, &
                   sCore%sta(iSta)%name, &
                   iMjd, &
                   sCore%sta(iSta)%ts%mjd(iMjd), &
                   sCore%sta(iSta)%ts%val(iMjd,:), &
                   opt%inPltFileViM0, &
                   dVal=sCore%sta(iSta)%ts%dVal(iMjd,:) )
           ELSE IF( opt%inPltFileVciEna == 2 )THEN
              CALL fodiwdat_pltCrdLine(lfnplt, &
                   sCore%sta(iSta)%name, &
                   iMjd, &
                   sCore%sta(iSta)%ts%mjd(iMjd), &
                   sCore%sta(iSta)%ts%val(iMjd,:), &
                   opt%inPltFileViM0, &
                   vci=sCore%sta(iSta)%ts%vci(iMjd,:,:) )
           END IF
        END DO
     END DO
     ! Close inPltFile
     CLOSE (lfnplt)
  END IF

! End of subroutine
! -----------------
!  CALL debug_exit(srName)
  RETURN

END SUBROUTINE fodiwdat

!-----------------------------------------------------------------------------
! Subroutine to write a line into the PLT file
!-----------------------------------------------------------------------------

SUBROUTINE fodiwdat_pltCrdLine(pltFil,staName,iMjd,mjd,val,m0,dVal,vci)

  USE m_bern, ONLY: i4b, r8b, staNameLength

  IMPLICIT NONE

! Subroutine Arguments
! ---------------------
  INTEGER(i4b),INTENT(IN)                  :: pltFil

  CHARACTER(LEN=staNameLength),INTENT(IN)  :: staName
  INTEGER(i4b),INTENT(IN)                  :: iMjd
  REAL(r8b),INTENT(IN)                     :: mjd
  REAL(r8b),DIMENSION(3),INTENT(IN)        :: val
  REAL(r8b),INTENT(IN)                     :: m0
  REAL(r8b),DIMENSION(3),INTENT(IN),OPTIONAL   :: dVal
  REAL(r8b),DIMENSION(3,3),INTENT(IN),OPTIONAL :: vci

! Local Variables
! ---------------
  INTEGER(i4b)                             :: iCrd
  REAL(r8b)                                :: cor
  REAL(r8b)                                :: nor
  REAL(r8b)                                :: rms

! Subroutine
! ----------

  DO iCrd = 1,3

     WRITE(pltFil,'(a16,i5,i2,f10.5,f14.5)',ADVANCE='NO') &
          staName, iMjd, iCrd, val(iCrd), mjd

    ! rms
     IF( PRESENT(dVal) )THEN
        rms = dVal(iCrd)
        WRITE(pltFil,'(F23.5)',ADVANCE='NO') rms
     ELSE IF( PRESENT(vci) )THEN
        rms = m0*SQRT(vci(iCrd,iCrd))
        WRITE(pltFil,'(F23.5)',ADVANCE='NO') rms
     END IF

     IF( PRESENT(vci) )THEN
        ! cor
        IF( iCrd == 1 )THEN
           cor = vci(1,3)/SQRT(vci(1,1)*vci(3,3))
        ELSE IF( iCrd == 2 )THEN
           cor = vci(2,3)/SQRT(vci(2,2)*vci(3,3))
        ELSE IF( iCrd == 3 )THEN
           cor = vci(1,2)/SQRT(vci(1,1)*vci(2,2))
        END IF
        WRITE(pltFil,'(1X,E12.5E2)',ADVANCE='NO')  cor

        ! nor
        nor = val(iCrd)/rms
        WRITE(pltFil,'(1X,E12.5E2)',ADVANCE='NO')  nor
     END IF

     ! Write line-feed
     WRITE(pltFil,*)

  END DO

  RETURN

END SUBROUTINE fodiwdat_pltCrdLine

!-----------------------------------------------------------------------------
! Subroutine to create synthetic time series - 3D only
!-----------------------------------------------------------------------------

SUBROUTINE fodiwdat_syntheticTimeSeries(opt,sCore,datFil)

  USE m_bern,   ONLY: i4b, r8b, lfnerr, shortLineLength

  USE p_fodits, ONLY: t_opt, t_score, t_datFil

  USE s_norrnd
  USE s_alcerr
  USE s_exitrc

  IMPLICIT NONE

  CHARACTER(LEN=shortLineLength) , PARAMETER   :: srName = 'fodiwdat'

! Subroutine Arguments
! ---------------------
  TYPE(t_opt)                    :: opt        ! Option struct
  TYPE(t_sCore)                  :: sCore      ! Core sturct of FODITS
  TYPE(t_datFil)                 :: datFil     ! I/O files struct

! Subroutine
! ----------
  INTEGER(i4b)                   :: iac

  INTEGER(i4b)                   :: iSta
  INTEGER(i4b)                   :: iMjd
  INTEGER(i4b)                   :: iEvnt
  INTEGER(i4b)                   :: jEvnt
  INTEGER(i4b)                   :: seen
  INTEGER(i4b)                   :: sseen
  INTEGER(i4b)                   :: rndIntVal
  REAL(r8b)                      :: rndVal2
  REAL(r8b),DIMENSION(3)         :: ampli
  REAL(r8b),DIMENSION(3)         :: corr
  REAL(r8b),DIMENSION(3)         :: val
  REAL(r8b),DIMENSION(3,3)       :: vci

  CHARACTER(LEN=3)               :: evntFlg
  CHARACTER(LEN=4)               :: evntType

  REAL(r8b)                      :: omega
  REAL(r8b)                      :: valStdDev
  REAL(r8b)                      :: m0

  REAL(r8b)                      :: deltaMjd
  REAL(r8b)                      :: obsMjd
  REAL(r8b)                      :: begMjd
  REAL(r8b)                      :: endMjd
  REAL(r8b)                      :: tVelo
  REAL(r8b)                      :: gapBegMjd
  REAL(r8b)                      :: gapEndMjd

  INTEGER(i4b)                   :: nObs
  INTEGER(i4b)                   :: jObs

  ! Initializations
  rndVal2 = 0.0D0
  deltaMjd = 0.0D0

  ! Allocation
  sCore%nSta = datFil%evlIn%nSta
  ALLOCATE(sCore%sta(sCore%nSta),stat=iac)
  CALL alcerr(iac,'sCore%sta',(/sCore%nSta/),srName)
  ! Loop over all stations
  DO iSta = 1,datFil%evlIn%nSta

     ! Station name
     sCore%sta(iSta)%name = datFil%evlIn%sta(iSta)%name

     ! FLG=TSS, TYPE=SYNT - basic setting of time interval
     seen = 0
     DO iEvnt = 1,datFil%evlIn%sta(iSta)%nEvnt
        evntFlg = datFil%evlIn%sta(iSta)%evnt(iEvnt)%flag
        evntType = datFil%evlIn%sta(iSta)%evnt(iEvnt)%type
        IF( evntFlg == 'TSS' .AND. evntType == 'SYNT' )THEN
           ! Number of observations
           begMjd = datFil%evlIn%sta(iSta)%evnt(iEvnt)%timint%t(1)
           endMjd = datFil%evlIn%sta(iSta)%evnt(iEvnt)%timint%t(2)
           deltaMjd = datFil%evlIn%sta(iSta)%evnt(iEvnt)%omega
           nObs = INT( (endMjd - begMjd) / deltaMjd )
           ALLOCATE(sCore%sta(iSta)%ts%mjd(nObs),stat=iac)
           CALL alcerr(iac,'sCore%sta(iSta)%ts%mjd',(/nObs/),srName)
           ALLOCATE(sCore%sta(iSta)%ts%val(nObs,3),stat=iac)
           CALL alcerr(iac,'sCore%sta(iSta)%ts%val',(/nObs,3/),srName)
           ALLOCATE(sCore%sta(iSta)%ts%vci(nObs,3,3),stat=iac)
           CALL alcerr(iac,'sCore%sta(iSta)%ts%vci',(/nObs,3,3/),srName)
           jObs = 0
           DO iMjd = 1,nObs
              obsMjd = begMjd + deltaMjd*(iMjd-1)
              ! FLG=GAP, TYPE=SYNT
              sseen = 0
              DO jEvnt = 1,datFil%evlIn%sta(iSta)%nEvnt
                 evntFlg = datFil%evlIn%sta(iSta)%evnt(jEvnt)%flag
                 evntType = datFil%evlIn%sta(iSta)%evnt(jEvnt)%type
                 gapBegMjd = datFil%evlIn%sta(iSta)%evnt(jEvnt)%timint%t(1)
                 gapEndMjd = datFil%evlIn%sta(iSta)%evnt(jEvnt)%timint%t(2)
                 IF( evntFlg == 'GAP' .AND. evntType == 'SYNT' .AND. &
                     obsMjd > gapBegMjd .AND. obsMjd < gapEndMjd )THEN
                    sseen = 1
                    EXIT
                 END IF
              END DO
              ! Time in MJD and Offset into time series
              IF( sseen == 0 )THEN
                 jObs = jObs + 1
                 sCore%sta(iSta)%ts%mjd(jObs) = obsMjd
                 sCore%sta(iSta)%ts%val(jObs,:) = &
                      datFil%evlIn%sta(iSta)%evnt(iEvnt)%val
                 sCore%sta(iSta)%ts%vci(jObs,:,:) = 0.0D0
              END IF
           END DO
           sCore%sta(iSta)%ts%nMjd = jObs
           sCore%sta(iSta)%ts%nVal = 3
           seen = 1
           EXIT
        END IF
     END DO
     IF( seen == 0 )THEN
        WRITE(lfnerr,'(/,A,/)') &
             ' *** SR FODIWDAT: no FLG=TSS TYPE=SYNT for station.'
        CALL exitrc(2)
     END IF

     ! FLG=GSN, TYPE=SYNT - normal noise
     DO iEvnt = 1,datFil%evlIn%sta(iSta)%nEvnt
        evntFlg = datFil%evlIn%sta(iSta)%evnt(iEvnt)%flag
        evntType = datFil%evlIn%sta(iSta)%evnt(iEvnt)%type
        IF( evntFlg == 'GSN' .AND. evntType == 'SYNT' )THEN
           rndIntVal = iSta
           ampli(:) = datFil%evlIn%sta(iSta)%evnt(iEvnt)%par(:)
           DO iMjd = 1,sCore%sta(iSta)%ts%nMjd
              CALL NORRND(ampli(1),0.0D0,rndIntVal,val(1),rndVal2)
              CALL NORRND(ampli(2),0.0D0,rndIntVal,val(2),rndVal2)
              CALL NORRND(ampli(3),0.0D0,rndIntVal,val(3),rndVal2)
              sCore%sta(iSta)%ts%val(iMjd,:) = val(:)
           END DO
        END IF
     END DO

     ! FLG=VCI, TYPE=SYNT - common stdDev and Vci
     DO iEvnt = 1,datFil%evlIn%sta(iSta)%nEvnt
        evntFlg = datFil%evlIn%sta(iSta)%evnt(iEvnt)%flag
        evntType = datFil%evlIn%sta(iSta)%evnt(iEvnt)%type
        IF( evntFlg == 'VCI' .AND. evntType == 'SYNT' )THEN
           valStdDev = datFil%evlIn%sta(iSta)%evnt(iEvnt)%omega
           corr(:) = datFil%evlIn%sta(iSta)%evnt(iEvnt)%par(:)
           vci(:,:) = 0.0D0
           m0 = opt%inPltFileViM0
           DO iMjd = 1,sCore%sta(iSta)%ts%nMjd
              ! Diagonal terms
              vci(1,1) = (valStdDev/m0)**2
              vci(2,2) = (valStdDev/m0)**2
              vci(3,3) = (valStdDev/m0)**2
              ! Off-diagonal terms
              vci(1,3) = corr(1)*SQRT(vci(1,1)*vci(3,3))
              vci(3,1) = vci(1,3)
              vci(2,3) = corr(2)*SQRT(vci(2,2)*vci(3,3))
              vci(3,2) = vci(2,3)
              vci(1,2) = corr(3)*SQRT(vci(1,1)*vci(2,2))
              vci(2,1) = vci(1,2)
              ! Assignment
              sCore%sta(iSta)%ts%vci(iMjd,:,:) = vci(:,:)
           END DO
        END IF
     END DO

     ! FLG=XXX, TYPE=DISC - discontinuity
     DO iEvnt = 1,datFil%evlIn%sta(iSta)%nEvnt
        evntFlg = datFil%evlIn%sta(iSta)%evnt(iEvnt)%flag
        evntType = datFil%evlIn%sta(iSta)%evnt(iEvnt)%type
        IF( evntFlg /= 'NOT' .AND. evntType == 'DISC' )THEN
           ampli(:) = datFil%evlIn%sta(iSta)%evnt(iEvnt)%par(:)
           DO iMjd = 1,sCore%sta(iSta)%ts%nMjd
              IF( sCore%sta(iSta)%ts%mjd(iMjd) >= &
                   datFil%evlIn%sta(iSta)%evnt(iEvnt)%timint%t(1) )THEN
                 sCore%sta(iSta)%ts%val(iMjd,:) = &
                      sCore%sta(iSta)%ts%val(iMjd,:) + ampli(:)
              END IF
           END DO
        END IF
     END DO

     ! FLG=XXX, TYPE=VELO - velocity change
     DO iEvnt = 1,datFil%evlIn%sta(iSta)%nEvnt
        evntFlg = datFil%evlIn%sta(iSta)%evnt(iEvnt)%flag
        evntType = datFil%evlIn%sta(iSta)%evnt(iEvnt)%type
        IF( evntFlg /= 'NOT' .AND. evntType == 'VELO' )THEN
           val(:) = datFil%evlIn%sta(iSta)%evnt(iEvnt)%par(:)
           val(:) = val(:) * deltaMjd / 365.25
           tVelo = datFil%evlIn%sta(iSta)%evnt(iEvnt)%timint%t(1)
           DO iMjd = 1,sCore%sta(iSta)%ts%nMjd
              IF( sCore%sta(iSta)%ts%mjd(iMjd) >= tVelo )THEN
                 sCore%sta(iSta)%ts%val(iMjd,:) = &
                      sCore%sta(iSta)%ts%val(iMjd,:) + &
                      val(:) * ( sCore%sta(iSta)%ts%mjd(iMjd) - tVelo )
              END IF
           END DO
        END IF
     END DO

     ! FLG=XXX, TYPE=OUTL - val of single outlier
     DO iEvnt = 1,datFil%evlIn%sta(iSta)%nEvnt
        evntFlg = datFil%evlIn%sta(iSta)%evnt(iEvnt)%flag
        evntType = datFil%evlIn%sta(iSta)%evnt(iEvnt)%type
        IF( evntFlg /= 'NOT' .AND. evntType == 'OUTL' )THEN
           ampli(:) = datFil%evlIn%sta(iSta)%evnt(iEvnt)%par(:)
           DO iMjd = 1,sCore%sta(iSta)%ts%nMjd
              IF( sCore%sta(iSta)%ts%mjd(iMjd) > &
                  datFil%evlIn%sta(iSta)%evnt(iEvnt)%timint%t(1) .AND. &
                  sCore%sta(iSta)%ts%mjd(iMjd) < &
                  datFil%evlIn%sta(iSta)%evnt(iEvnt)%timint%t(2) )THEN
                 sCore%sta(iSta)%ts%val(iMjd,:) = &
                      sCore%sta(iSta)%ts%val(iMjd,:) + ampli(:)
              END IF
           END DO
        END IF
     END DO

     ! FLG=SET, TYPE=OTLD - stdDev and Vci of single outlier
     DO iEvnt = 1,datFil%evlIn%sta(iSta)%nEvnt
        evntFlg = datFil%evlIn%sta(iSta)%evnt(iEvnt)%flag
        evntType = datFil%evlIn%sta(iSta)%evnt(iEvnt)%type
        IF( evntFlg == 'SET' .AND. evntType == 'OTLD' )THEN
           valStdDev = datFil%evlIn%sta(iSta)%evnt(iEvnt)%omega
           corr(:) = datFil%evlIn%sta(iSta)%evnt(iEvnt)%par(:)
           vci(:,:) = 0.0D0
           m0 = opt%inPltFileViM0
           DO iMjd = 1,sCore%sta(iSta)%ts%nMjd
              IF( sCore%sta(iSta)%ts%mjd(iMjd) > &
                  datFil%evlIn%sta(iSta)%evnt(iEvnt)%timint%t(1) .AND. &
                  sCore%sta(iSta)%ts%mjd(iMjd) < &
                  datFil%evlIn%sta(iSta)%evnt(iEvnt)%timint%t(2) )THEN
                 ! Diagonal terms
                 vci(1,1) = (valStdDev/m0)**2
                 vci(2,2) = (valStdDev/m0)**2
                 vci(3,3) = (valStdDev/m0)**2
                 ! Off-diagonal terms
                 vci(1,3) = corr(1)*SQRT(vci(1,1)*vci(3,3))
                 vci(3,1) = vci(1,3)
                 vci(2,3) = corr(2)*SQRT(vci(2,2)*vci(3,3))
                 vci(3,2) = vci(2,3)
                 vci(1,2) = corr(3)*SQRT(vci(1,1)*vci(2,2))
                 vci(2,1) = vci(1,2)
                 ! Assignment
                 sCore%sta(iSta)%ts%vci(iMjd,:,:) = vci(:,:)
              END IF
           END DO
        END IF
     END DO

     ! FLG=XXX, TYPE=PERI - periodic function
     DO iEvnt = 1,datFil%evlIn%sta(iSta)%nEvnt
        evntFlg = datFil%evlIn%sta(iSta)%evnt(iEvnt)%flag
        evntType = datFil%evlIn%sta(iSta)%evnt(iEvnt)%type
        IF( evntFlg /= 'NOT' .AND. evntType == 'PERI' )THEN
           ampli(:) = datFil%evlIn%sta(iSta)%evnt(iEvnt)%par(:)
           omega    = datFil%evlIn%sta(iSta)%evnt(iEvnt)%omega
           DO iMjd = 1,sCore%sta(iSta)%ts%nMjd
              IF( sCore%sta(iSta)%ts%mjd(iMjd) >= &
                   datFil%evlIn%sta(iSta)%evnt(iEvnt)%timint%t(1) .AND. &
                   sCore%sta(iSta)%ts%mjd(iMjd) < &
                   datFil%evlIn%sta(iSta)%evnt(iEvnt)%timint%t(2) )THEN
                 sCore%sta(iSta)%ts%val(iMjd,:) = &
                      sCore%sta(iSta)%ts%val(iMjd,:) + &
                      ampli(:)*DCOS(omega*sCore%sta(iSta)%ts%mjd(iMjd))
              END IF
           END DO
        END IF
     END DO

  END DO

  RETURN

END SUBROUTINE fodiwdat_syntheticTimeSeries

END MODULE s_FODIWDAT



