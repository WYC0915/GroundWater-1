MODULE f_sinstati
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

FUNCTION sinstati(neq, isel, ipar, siteCode, pointCode, domes, solID,     &
                  technique, serNumber, firmware, refSys, startTime,      &
                  endTime, meanTime, refTime, antNum, xyz, ell, staFlg,   &
                  isat, cosparID, satAntTyp, freqCode, antOffset,elDepMod,&
                  pcvTyp,pcvMod)

! -------------------------------------------------------------------------
! Purpose:    This function returns the station index "ista" if the
!             parameter "ipar" corresponds to a station, which has flag 0 in
!             array "staFlg" and sets the "staFlg(ista)" to 1. It returns
!             "-ista" if "staFlg(ABS(ista))" was already 1. If no
!             station was found, the function returns 0.
!
! Author:     L. Mervart
!
! Created:    05-Sep-1998
!
! Changes:    30-Nov-2001 SS: Do not provide antenna numbers
!             21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             06-Aug-2002 HU: New parameter for initialization
!             27-Feb-2003 HU: DATUM from D_DATUM
!             09-Mar-2004 HU: Apply no_abc to station names for SINEX
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             29-Mar-2006 SS: Set pointCode using CHAR function
!             20-Apr-2006 AG: Return of satellite information added
!                             Return adopted info if requested
!             24-Apr-2006 AG: PRESENT function for ADOPTED added
!             28-Apr-2006 AG: Correct writing of ANTOFF
!             22-Jun-2006 SS: Do not make use of no_abc option
!             04-Oct-2006 AG: SVN for satellite specific group number
!             31-Oct-2006 AG: Take time interval for coordinates from parameter
!             16-Nov-2006 AG: domes = PRN for satellites
!             05-Dez-2006 AG: INDIVID added
!             12-Feb-2007 AG: INTENT(IN) for NEQ removed
!             07-May-2007 AG: Do not provide antenna numbers if individually calibrated
!             31-May-2007 AG: Replace time interval in case of coordinates only
!             09-Aug-2007 AG: Call antinfo instead of gphecm2
!             13-Feb-2008 RD: Adapt to par%name == chr*20
!             17-Apr-2009 SL: startTime, endTime, and meanTime corrected
!             06-Jul-2009 DT: Range biases added (26) for setting of PointCode
!             23-Jul-2009 DT: Write solID without leading zeros (I4.4 -> I4)
!             29-Oct-2010 SL: use m_bern with ONLY
!             28-Mar-2012 RD: Use LISTC1 as module now
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, lfnErr, staNameLength, staNam2Length
  USE m_maxdim, ONLY: maxsat
  USE m_time,   ONLY: t_timint
  USE m_global, ONLY: g_syssvn
  USE d_datum,  ONLY: datum
  USE d_par,    ONLY: is_techn
  USE d_neq,    ONLY: t_neq
  USE d_satfil, ONLY: typeMWTR
  USE d_phaecc, ONLY: antinfo
  USE d_rgbfil, ONLY: rgbSNX

  USE s_svn2prn
  USE s_prn2cos
  USE s_gtsensor
!  USE s_gphecm2
  USE s_sindat
  USE s_xyzell
  USE f_listc1
  IMPLICIT NONE

! Return Value
! ------------
  INTEGER(i4b) :: sinstati

! List of Parameters
! ------------------
  TYPE(t_neq)                                   :: neq

  INTEGER(i4b),INTENT(IN)                       :: isel    ! 1: initialize
  INTEGER(i4b),INTENT(IN)                       :: ipar
  INTEGER(i4b), DIMENSION(neq%misc%nstat_sinex) :: staFlg
  INTEGER(i4b), DIMENSION(2)                    :: freqCode
  INTEGER(i4b)                                  :: isat

  CHARACTER(LEN=4)        :: siteCode
  CHARACTER(LEN=*)        :: pointCode
  CHARACTER(LEN=*)        :: domes
  CHARACTER(LEN=*)        :: solID
  CHARACTER(LEN=*)        :: technique
  CHARACTER(LEN=*)        :: serNumber
  CHARACTER(LEN=*)        :: firmware
  CHARACTER(LEN=*)        :: refSys
  CHARACTER(LEN=*)        :: startTime
  CHARACTER(LEN=*)        :: endTime
  CHARACTER(LEN=*)        :: meanTime
  CHARACTER(LEN=*)        :: refTime
  CHARACTER(LEN=*)        :: antNum
  CHARACTER(LEN=1)        :: pcvTyp
  CHARACTER(LEN=1)        :: pcvMod
  CHARACTER(LEN=9)        :: cosparID
  CHARACTER(LEN=10)       :: elDepMod
  CHARACTER(LEN=20)       :: satAntTyp

  REAL(r8b)               :: tHlp
  REAL(r8b), DIMENSION(3) :: xyz
  REAL(r8b), DIMENSION(3) :: ell
  REAL(r8b), DIMENSION(3,2) :: antOffset

! Local Variables
! ---------------
  INTEGER(i4b)            :: ista
  INTEGER(i4b)            :: ii
  INTEGER(i4b)            :: irc
  INTEGER(i4b)            :: numDom
  INTEGER(i4b)            :: numAbr
  INTEGER(i4b),SAVE       :: nsat
  INTEGER(i4b)            :: nsav
  INTEGER(i4b)            :: prn
  INTEGER(i4b)            :: svn
  INTEGER(i4b)            :: sensnr
  CHARACTER(LEN=6)        :: satnam
  REAL(r8b)               :: epo
  REAL(r8b),DIMENSION(3)  :: antoff
  TYPE(t_timint)          :: timint

  CHARACTER(LEN=6),DIMENSION(maxsat),SAVE       :: sinSatNam
  CHARACTER(LEN=staNam2Length)                  :: sensor
  CHARACTER(LEN=staNameLength)                  :: sinStaNam


! Set the default return value
! ----------------------------
  sinstati = 0

! Initialize array staFlg
! -----------------------
  IF ( isel == 1 ) staFlg(:) = 0
  IF ( isel == 1 ) nsat=0

! This remains the same
! ---------------------
  serNumber = '-----'
  firmware  = '-----------'
  refSys    = 'UNE'

  CALL sindat(0, neq%par(ipar)%time%mean, refTime)

! Set observation technique
! -------------------------
  technique = ' '
  IF ( is_techn(neq%par(ipar), gnss = 1) ) THEN
    technique = 'P'
  ELSE IF ( technique /= ' ' .AND. &
            is_techn(neq%par(ipar), vlbi = 1) ) THEN
    technique = 'C'
  ELSE IF ( is_techn(neq%par(ipar), vlbi = 1) ) THEN
    technique = 'R'
  ELSE IF ( technique /= ' ' .AND. &
            is_techn(neq%par(ipar), slr = 1) ) THEN
    technique = 'C'
  ELSE IF ( is_techn(neq%par(ipar), slr = 1) ) THEN
    technique = 'L'
  ELSE IF ( technique /= ' ' .AND. &
            is_techn(neq%par(ipar), llr = 1) ) THEN
    technique = 'C'
  ELSE IF ( is_techn(neq%par(ipar), llr = 1) ) THEN
    technique = 'M'
  ELSE IF ( technique /= ' ' .AND. &
            is_techn(neq%par(ipar), doris = 1) ) THEN
    technique = 'C'
  ELSE IF ( is_techn(neq%par(ipar), doris = 1) ) THEN
    technique = 'D'
  ELSE ! default, in case of old NEQs
    technique = 'P'
  ENDIF

! Only coordinate and troposphere parameters are relevant
! -------------------------------------------------------
  IF (neq%par(ipar)%locq(1) /= 1 .AND. neq%par(ipar)%locq(1) /= 6 .AND. &
      neq%par(ipar)%locq(1) /= 12 .AND. neq%par(ipar)%locq(1) /= 26    ) THEN
    solID     = '----'
    siteCode  = '----'
    pointCode = '--'
    CALL sindat(0, neq%par(ipar)%time%mean, meanTime)
    RETURN
  END IF

! Satellite information
! ---------------------
  isat=0
  IF (neq%par(ipar)%locq(1) == 12) THEN
    solID     = '----'
    svn=neq%par(ipar)%locq(5)
    epo=neq%par(ipar)%time%mean
    WRITE(siteCode,"(A1,I3)")g_syssvn(INT(svn/100)),svn
    CALL svn2prn(6,siteCode,epo,prn,timint,irc)
!
    IF (neq%par(ipar)%locq(6) == 1 .OR. neq%par(ipar)%locq(6) ==2) THEN
      WRITE(pointCode,"('L',I1)") neq%par(ipar)%locq(6)
    ELSEIF (neq%par(ipar)%locq(6) == 3) THEN
      pointCode='LC'
    ELSE
      pointCode='L?'
    ENDIF
!
    nsav=nsat
    satnam=siteCode//pointCode
    isat=listc1(1,6,maxsat,sinSatNam,satnam,nsat)
    IF (nsav==nsat) isat=-isat
!
    CALL prn2cos(11,prn,epo,cosparID,irc)          !! murx, only for MW
    CALL gtsensor(prn,epo,typeMWTR,sensor,sensnr, &
                  timint=timint,antoff=antoff,antexna=satAntTyp)
!    CALL gphecm2(sensor,sensnr,elDepMod,pcvMod,pcvTyp)
    CALL antinfo(sensor,sensnr,0,sinex=elDepMod,typ=pcvMod,model=pcvTyp)
    CALL sindat(0, timint%t(1), startTime)
    CALL sindat(0, timint%t(2), endTime)
    CALL sindat(0, neq%par(ipar)%time%mean, meanTime)
    WRITE(domes,"(I2.2)")MOD(prn,100)
!
    DO ii=1,2                                      !! murx, only for 2 freq
      freqCode(ii)    = ii
      antOffset(:,ii) = antoff(:)
    ENDDO
  ENDIF

! Loop over all stations
! ----------------------
  DO ista = 1, neq%misc%nstat_sinex
    sinStaNam=neq%misc%sinex(ista)%stname
    IF ( neq%par(ipar)%name(1:staNameLength) == sinStaNam ) THEN

      IF ( staFlg(ista) == 0 )  THEN
        sinstati     = ista
        staFlg(ista) = 1
      ELSE
        sinstati     = -ista
      END IF

      siteCode  = neq%misc%sinex(ista)%stname(1:4)
      domes     = neq%misc%sinex(ista)%stname(6:14)

      ! Stations with the same Abbreviation but different Dome
      ! ------------------------------------------------------
      numAbr = 0
      DO ii = 1, ista-1
        IF ( siteCode == neq%misc%sinex(ii)%stname(1:4)    .AND. &
             domes    /= neq%misc%sinex(ii)%stname(6:14) ) THEN
          numAbr = numAbr + 1
        END IF
      END DO

      ! Stations with the same Abbreviation and Dome
      ! --------------------------------------------
      numDom = 0
      DO ii = 1, ista-1
        IF ( siteCode == neq%misc%sinex(ii)%stname(1:4)    .AND. &
             domes    == neq%misc%sinex(ii)%stname(6:14) ) THEN
          numDom = numDom + 1
        END IF
      END DO

      WRITE(solId,'(I4)') numDom+1

      IF ( domes == '' ) domes(6:6) = 'M'

      ! PointCode: Range Biases
      ! -----------------------
      IF ( neq%par(ipar)%locq(1) == 26 ) THEN
        IF ( neq%par(ipar)%locq(5)>=951 .AND. neq%par(ipar)%locq(5)<=954 ) THEN
          WRITE(pointCode, '(A2)') rgbSNX(neq%par(ipar)%locq(5)-950)
        ELSE
          WRITE(pointCode, '(A2)') rgbSNX(5)
        END IF

      ! PointCode: Other parameters
      ! ---------------------------
      ELSE
        IF (numAbr <= 25) THEN
          pointCode = ' ' // CHAR(numAbr+65)
        ELSE
          WRITE(lfnerr,*) ' *** sinstati: too many domes: ', siteCode
        END IF

      END IF

      xyz(:) = 0.0
      ell(:) = 0.0
      DO ii = 1, neq%misc%npar
        IF (neq%par(ii)%locq(1) == 1                    .AND. &
            neq%par(ii)%locq(3) == 1                    .AND. &
            neq%par(ii)%locq(4) /= 3                    .AND. &
            neq%par(ii)%name    == neq%par(ipar)%name ) THEN

          xyz(1) = neq%par(ii  )%x0
          xyz(2) = neq%par(ii+1)%x0
          xyz(3) = neq%par(ii+2)%x0

          CALL xyzell(datum%aell, datum%bell, datum%dxell, datum%drell, &
                        datum%scell, xyz, ell)
          EXIT
        END IF
      END DO

      tHlp = ( neq%misc%sinex(ista)%timint%t(1) +         &
               neq%misc%sinex(ista)%timint%t(2) ) / 2.d0

      CALL sindat(0, neq%misc%sinex(ista)%timint%t(1), startTime)
      CALL sindat(0, neq%misc%sinex(ista)%timint%t(2),   endTime)
      CALL sindat(0, tHlp, meanTime)
!!!      CALL sindat(0, neq%par(ipar)%time%mean, meanTime)

      IF ( neq%misc%sinex(ista)%antnum /= 0       .AND. &
           neq%misc%sinex(ista)%antnum <= 99999 ) THEN
        antNum = ''
        WRITE(antNum,'(I5.5)') neq%misc%sinex(ista)%antnum
      ELSE
        antNum = '-----'
      END IF

      RETURN
    END IF
  END DO

END FUNCTION sinstati

END MODULE
