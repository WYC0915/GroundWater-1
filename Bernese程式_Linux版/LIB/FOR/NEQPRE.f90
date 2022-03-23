    MODULE s_NEQPRE
    CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE neqpre(rectyp,anttyp,ianten,csess,nfreq,stfil,stname, &
                  nftot,nstat,posecc,ncamp,taecmp,timIntF,antsnx)

! -------------------------------------------------------------------------
! Purpose:    Prepare storage in SR NEQWRITE: antenna information per station
!
! Author:     R. Dach
!
! CHANGES_F77    :  02-SEP-94 : EB: REPLACE NFRFIL BY NFREQ
!                   26-MAR-96 : MR: ADD "CSESS" TO CALL GPHECC AND
!                                   TO THE PARAMETER LIST
!                   28-MAY-03 : RD: NEW CALL OF SR GPHECC
!                   11-AUG-03 : RS: RECTYP=' ' IN CALL OF GPHECC
!                   08-SEP-03 : HU: RECNAM, ANTNAM CHR16 -> CHR20
!                   21-JUN-05 : MM: COMLFNUM.inc REMOVED
!                   23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
!                   12-JUN-07 : AG: USE STA_OFF INSTEAD OF GPHECC
!
! Created:    20-Jun-2008
!
! Changes:    20-Jun-2008 RD: Converted from F77; general review
!             26-Jun-2008 RD: System-specific PCV in SINEX record
!             04-Aug-2008 DT: Adopt for SLR stations
!             22-Jul-2009 DT: timIntF added to parameter list;
!                             Set antsnx(istat)%timint station-dependent
!             26-Oct-2011 SL: m_bern with ONLY, ignore Compass and QZSS
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b
  USE m_global, ONLY: maxsys
  USE m_time,   ONLY: t_timint
  USE d_neq,    ONLY: t_sinex
  USE d_phaecc, ONLY: sta_off, antInfo
  USE p_gpsest, ONLY: maxfrq
  USE d_stacrx, ONLY: MTypeSLR

  USE s_getrcv
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*), DIMENSION(:,:)   :: rectyp  ! receiver type
                                                ! rectyp(iSta,iFil)
  CHARACTER(LEN=*), DIMENSION(:,:)   :: anttyp  ! antenna type
                                                ! anttyp(iSta,iFil)
  INTEGER(i4b)    , DIMENSION(:,:)   :: ianten  ! antenna number
                                                ! ianten(iSta,iFil)
  CHARACTER(LEN=*), DIMENSION(:,:)   :: csess   ! session string
                                                ! csess(1:2,iFil)
  INTEGER(i4b)    , DIMENSION(:)     :: nFreq   ! number of frequencies
                                                ! per file: nFreq(iFil)
  INTEGER(i4b)    , DIMENSION(:,:)   :: stFil   ! index in station list
                                                ! stfil(1:2,iFil)
  CHARACTER(LEN=*), DIMENSION(:)     :: stName  ! List of station names
                                                ! stname(iStat)
  INTEGER(i4b)                       :: nftot   ! Number of observ. files
  INTEGER(i4b)                       :: nStat   ! Number of stations in list
  REAL(r8b)       , DIMENSION(:,:,:) :: posecc  ! position ecentricities
                                                ! posecc(1:3,1:2,iFil)
  INTEGER(i4b)                       :: ncamp   ! Number of campaigns
  REAL(r8b)       , DIMENSION(2,*)   :: taecmp  ! Intervals for each campaign
  REAL(r8b)       , DIMENSION(2,*)   :: timIntF ! Interval for each ObsFil

! output
  TYPE(t_sinex),    DIMENSION(:)     :: antsnx  ! SINEX station description
                                                ! antsnx(iStat)

! Local Parameters
! ----------------

! Local Variables
! ---------------
  INTEGER(i4b)                       :: iStat
  INTEGER(i4b)                       :: iFil
  INTEGER(i4b)                       :: iSt
  INTEGER(i4b)                       :: iSyst
  INTEGER(i4b)                       :: nFrq,iClass
  INTEGER(i4b), DIMENSION(MAXFRQ)    :: iCode,iwlFac
  INTEGER(i4b)                       :: iSys
  INTEGER(i4b)                       :: iFrq

  REAL(r8b)                          :: timmin
  REAL(r8b)                          :: timmax


  TYPE(t_timint), DIMENSION(nStat)   :: timIntS


  timmin = MINVAL( taecmp(1,1:ncamp) )
  timmax = MAXVAL( taecmp(2,1:ncamp) )


! Find time interval per station
! ------------------------------
  DO iStat = 1, nStat

    timIntS(iStat)%t(1) =  HUGE(r8b)
    timIntS(iStat)%t(2) = -HUGE(r8b)

    DO iFil = 1, nftot

      IF ( stFil(1,iFil) /= iStat .AND. &
           stFil(2,iFil) /= iStat      ) CYCLE

      IF ( timIntF(1,iFil) < timIntS(iStat)%t(1) )  &
         timIntS(iStat)%t(1) = timIntF(1,iFil)

      IF ( timIntF(2,iFil) > timIntS(iStat)%t(2) )  &
         timIntS(iStat)%t(2) = timIntF(2,iFil)

    END DO ! iFil
  END DO ! iStat


! Loop the stations
! -----------------
  DO iStat=1,nStat

    ! Check whether the station has observations
    DO iFil=1,nftot
      DO ist=1,2
        IF (stFil(ist,iFil) == 0) CYCLE
        IF (stFil(ist,iFil) /= iStat) CYCLE

        ! Copy the information

!!!        antsnx(istat)%timint%t = (/ timmin,timmax /)
        antsnx(istat)%timint   = timIntS(iStat)

        antsnx(istat)%stname = stname(istat)
        antsnx(istat)%antrec = rectyp(ist,iFil)
        antsnx(istat)%antsta = anttyp(ist,iFil)
        antsnx(istat)%antnum = ianten(ist,iFil)
        antsnx(istat)%antecc(1:3) = posecc(1:3,ist,iFil)

        ! Check the tracking capabilities of the receiver
        CALL getrcv(antsnx(istat)%antrec,nFrq,iCode,iwlFac,iClass,iSyst)

        ! Loop the frequencies and systems
        DO iSys = 0,maxsys-1 ! GPS, GLONASS, Galileo, SBAS
                      ! Should never be bigger than maxsys in $I/M_GOBAL.f90

          antsnx(istat)%antpcv(iSys)%atxStr = ''
          antsnx(istat)%antpcv(iSys)%nFrq   = 0

          IF (iSys == 1 .AND. (iSyst == 0 .OR. iSyst == 2)) CYCLE ! no GLONASS
          IF (iSys == 2 .AND. (iSyst == 0 .OR. iSyst == 1)) CYCLE ! no Galileo
          IF (iSys == 3) CYCLE ! no special SBAS at the moment
          IF (iSys == 4) CYCLE ! no Compass
          IF (iSys == 5) CYCLE ! no QZSS

          IF ( antsnx(istat)%antrec /= MTypeSLR .AND. &
               antsnx(istat)%antsta /= MTypeSLR      ) THEN

            CALL antInfo(antsnx(istat)%antsta,antsnx(istat)%antnum,iSys,   &
                         adopted=antsnx(istat)%antpcv(iSys)%adopted,       &
                         individ=antsnx(istat)%antpcv(iSys)%individ,       &
                         sinex=antsnx(istat)%antpcv(iSys)%atxStr)
          END IF

          antsnx(istat)%antpcv(iSys)%nFrq = nFrq

          DO iFrq=1,nFrq
            CALL sta_off(antsnx(istat)%antsta,antsnx(istat)%antnum, &
                         antsnx(istat)%stname,iSys*100,iFrq,        &
                         csess(1,iFil),antsnx(istat)%antpcv(iSys)%antphs(1:3,iFrq))

          ENDDO ! iFrq
        ENDDO   ! iSys
      ENDDO ! ist
    ENDDO   ! iFil
  ENDDO ! iStat

  RETURN
  END SUBROUTINE

END MODULE

