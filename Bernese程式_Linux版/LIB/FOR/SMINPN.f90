MODULE s_SMINPN
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

  SUBROUTINE sminpn(opt)

! ------------------------------------------------------------------------
! Purpose  :  Read all input information for the program GPSSIM
!             from the input file (new version of SR SMINPT)
!
! Author   :  H.Bock
!
! Created  :  30-May-2001
! Last mod.:  06-May-2011
!
! Changes  :  23-Feb-2002 HU: Read mxosvn
!             27-Jun-2002 DS: Add the LEO input options
!             20-Nov-2002 HB: Meteo-file simulation removed
!             17-Jan-2003 CU: Bug fixed (opt%csess(1)=DDDS)
!             23-Apr-2003 RD: Nullify local pointers
!             19-May-2003 RD: Use SR gttimwin instead of SR readsess
!             27-May-2003 RD: Warning if more than one session was selected
!             14-Oct-2003 RD: Use CKOPT-SRs to read input options
!             24-Nov-2003 HU: Read iono on/off flag, write options
!             15-Jan-2004 HU: Use iono(3) as flag
!             21-Jan-2004 RD: Set IEXTRA from TROPEST/METFIL
!             27-Jan-2004 RD: Remove check for old IEXTRA-keyword
!             08-Aug-2005 HB: Use new SR TIMST2 (module)
!             24-Aug-2006 AG: GMF implemented
!             01-Nov-2007 HB: opt%secIpl added
!             30-Jun-2008 RD: VMF added
!             04-May-2009 RD: change opt%tfirst/tlast to opt%obswin
!             14-Oct-2010 RD: Read tropo-model from SR trpopt
!             30-Nov-2010 DT: Add obstype "microwave" to SR trpopt
!             03-Feb-2011 CR: Add periodic relativistic J2-correction
!             06-May-2011 HB: Add rdstdh to initialize model names
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
!----------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE m_maxdim, ONLY: MAXSAT
  USE d_stdorb, ONLY: t_stdhead, init_stdHead
  USE p_gpssim, ONLY: t_gpssim_opt
  USE s_ckoptr
  USE s_gttimwin
  USE s_timst2
  USE s_readkeys
  USE s_exitrc
  USE s_ckoptb
  USE s_ckoptc
  USE s_priwin
  USE s_ckopti
  USE s_gtflna
  USE s_ckoptl
  USE s_trpopt
  USE s_rdstdh
  IMPLICIT NONE

! List of parameters
! ------------------
! out:

  TYPE(t_gpssim_opt) :: opt

! List of Functions
! -----------------

! Local Types
! -----------
  TYPE(t_stdhead)        :: stdHead, stdLHead   ! Structure of std header info

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER :: srName = 'sminpn'

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength),DIMENSION(:),POINTER :: keyValue
  CHARACTER(LEN=timStrgLength)                       :: epost1,epost2
  CHARACTER(LEN=fileNameLength)                      :: filnam

  INTEGER(i4b)              :: iLeos
  INTEGER(i4b)              :: iTrMap
  INTEGER(i4b), DIMENSION(2):: iTrGrd
  INTEGER(i4b)              :: irc
  INTEGER(i4b)              :: irCode


! Init variables
! --------------
  NULLIFY(keyValue)
  CALL init_stdHead(stdHead)
  irCode = 0

! Read session
! ------------
  CALL gttimwin(' ',(/'RADIO_1','RADIO_2'/),                  &
                (/'SESSION_YEAR','SESSION_STRG'/),            &
                (/'STADAT','STATIM','ENDDAT','ENDTIM'/),      &
                opt%obswin%t)

  IF (opt%obswin%t(1) == 0d0 .OR. opt%obswin%t(2) == 1d20) THEN
    CALL timst2(1,1,opt%obswin%t(1),epost1)
    CALL timst2(1,1,opt%obswin%t(2),epost2)
    WRITE(lfnerr,'(/,A,2(/,16X,A),/)')                              &
    ' *** SR SMINPN: For this program a start and an end epoch ' // &
                                            'must be specified!',   &
                    'Start epoch: ' // epost1,                      &
                    'End   epoch: ' // epost2
    CALL exitrc(2)
  ENDIF

! Session string
! --------------
  CALL readKeys('RADIO_1', keyValue, irc)

  IF (irc == 0) THEN
    IF (keyValue(1) == '1') THEN

      CALL readKeys('SESSION_STRG', keyValue, irc)
      CALL ckoptl(1,'SESSION_STRG', keyValue, srName, 'Session string', &
                  irc, irCode, maxLength=4, result1=opt%csess(1))

      ! Only one session allowed
      IF (SIZE(keyValue) > 1) THEN
        WRITE(lfnerr,'(/,A,/,16X,A,/)')                           &
        ' ### SR SMINPN: More than one session selected.',        &
        'For the Bernese observation files will be used session ' &
        // opt%csess(1)
      ENDIF

    ELSE

      CALL readKeys('CSESS', keyValue, irc)
      CALL ckoptl(1,'CSESS', keyValue, srName, 'Session string', &
                  irc, irCode, maxLength=4, maxVal=1, result1=opt%csess(1))
    ENDIF
  ELSE

    opt%csess(1) = "0000"
    irCode = irCode+1

  END IF

  opt%csess(2) = opt%csess(1)(4:4)

! Read campaign name
! ------------------
  CALL readKeys('CAMPGN', keyValue, irc)
  CALL ckoptl(1,'CAMPGN', keyValue, srName,         &
              'Campaign name', irc, irCode,         &
              maxVal=1,result1=opt%campgn)

! Read general title
! ------------------
  CALL readKeys('TITLE', keyValue, irc)
  CALL ckoptl(0,'TITLE', keyValue, srName,          &
              'Program output title', irc, irCode,  &
              empty=" ",maxVal=1,result1=opt%title)

! Sampling interval
! -----------------
  CALL readKeys('INTER', keyValue, irc)
  CALL ckopti(1,'INTER', keyValue, srName,          &
              'Sampling interval', irc, irCode,     &
              ge=1, maxVal=1, result1=opt%inter)

! Minimum elevation
! -----------------
  CALL readKeys('IELEV', keyValue, irc)
  CALL ckopti(1,'IELEV', keyValue, srName,          &
              'Minimum elevation', irc,irCode,      &
              ge=-20, le=90, maxVal=1, result1=opt%ielev)

! Clock interpolation interval
! ----------------------------
  CALL readKeys('SECIPL', keyValue, irc)
  CALL ckoptr(1,'SECIPL', keyValue, srName,          &
              'Clock interpolation interval', irc, irCode,     &
              maxVal=1, result1=opt%secIpl)

! Periodic Relativistic J2-Correction
! -----------------------------------
  CALL ckoptb(1,(/ 'PRELJ2' /),srName,                      &
              'Periodic Relativistic J2-Correction',irCode, &
              error=0, result1=opt%irel2)

! Maximum number of satellites
! ----------------------------
  CALL readKeys('MAXSVN', keyValue, irc)
  CALL ckopti(1,'MAXSVN', keyValue, srName,         &
              'Maximum number of satellites per epoch', irc,irCode,&
              ge=1,le=maxsat, empty=maxsat, result1=opt%mxosvn)

! Read tropospheric model (1: microwave observations)
! -----------------------
  CALL trpopt(1, opt%itropo,opt%iextra,itrmap,itrgrd)

! Read ionospheric model
! ----------------------
  CALL gtflna(0,'IONOS',filnam,irc)
  IF (irc /= 0) THEN
    CALL readKeys('IONO1', keyValue, irc)
    CALL ckopti(1,'IONO1', keyValue, srName,             &
                'Night time electron number',irc,irCode, &
                maxVal=1,empty=0,ge=0,result1=opt%iono(1))

    CALL readKeys('IONO2', keyValue, irc)
    CALL ckopti(1,'IONO2', keyValue, srName,             &
                'Day time electron number',irc,irCode,   &
                maxVal=1,empty=0,ge=0,result1=opt%iono(2))
  ELSE
    opt%iono(1)=0
    opt%iono(2)=0
  ENDIF

  CALL readKeys('IONO3', keyValue, irc)
  CALL ckopti(1,'IONO3', keyValue, srName,                &
              'Variance of electron number',irc,irCode,   &
              maxVal=1,empty=0,ge=0,result1=opt%iono(3))

  CALL readKeys('IONREF', keyValue, irc)
  CALL ckoptl(1,'IONREF', keyValue, srName,           &
              'Reference station for ionosphere variance',irc,irCode, &
              empty=' ',maxLength=16,result1=opt%ionref)

! Statistics: RMS(P1)
! -------------------
  CALL readKeys('RMS1', keyValue, irc)
  CALL ckoptr(1,'RMS1', keyValue, srName,                       &
              'A priori sigma code L1 for stations',irc,irCode, &
              empty=0d0,ge=0d0,maxVal=1,result1=opt%rms(1,1))

! Statistics: RMS(P2)
! -------------------
  CALL readKeys('RMS2', keyValue, irc)
  CALL ckoptr(1,'RMS2', keyValue, srName,                       &
              'A priori sigma code L2 for stations',irc,irCode, &
              empty=0d0,ge=0d0,maxVal=1,result1=opt%rms(2,1))

! Statistics: RMS(L1)
! -------------------
  CALL readKeys('RMS3', keyValue, irc)
  CALL ckoptr(1,'RMS3', keyValue, srName,                        &
              'A priori sigma phase L1 for stations',irc,irCode, &
              empty=0d0,ge=0d0,maxVal=1,result1=opt%rms(1,2))

! Statistics: RMS(L2)
! -------------------
  CALL readKeys('RMS4', keyValue, irc)
  CALL ckoptr(1,'RMS4', keyValue, srName,                        &
              'A priori sigma phase L2 for stations',irc,irCode, &
              empty=0d0,ge=0d0,maxVal=1,result1=opt%rms(2,2))

! Init random number
! ------------------
  CALL readKeys('IXNUM', keyValue, irc)
  CALL ckopti(0,'IXNUM', keyValue, srName,                        &
              'Init random number',irc,irCode,                    &
              empty=1410,error=2003,init=111,maxVal=1,result1=opt%ix)

! Elevation dep. sigma for stations
! ---------------------------------
  CALL ckoptb(1,(/'ICOELV'/),srName,                              &
              'Elevation-dependent sigmas for stations',irCode,   &
              result1=opt%icoelv)

! Statistics: RMS(P1)
! -------------------
  CALL readKeys('RMS1_LEO', keyValue, irc)
  CALL ckoptr(1,'RMS1_LEO', keyValue, srName,                   &
              'A priori sigma code L1 for LEOs',irc,irCode,     &
              empty=0d0,ge=0d0,maxVal=1,result1=opt%leorms(1,1))

! Statistics: RMS(P2)
! -------------------
  CALL readKeys('RMS2_LEO', keyValue, irc)
  CALL ckoptr(1,'RMS2_LEO', keyValue, srName,                   &
              'A priori sigma code L2 for LEOs',irc,irCode,     &
              empty=0d0,ge=0d0,maxVal=1,result1=opt%leorms(2,1))

! Statistics: RMS(L1)
! -------------------
  CALL readKeys('RMS3_LEO', keyValue, irc)
  CALL ckoptr(1,'RMS3_LEO', keyValue, srName,                    &
              'A priori sigma phase L1 for LEOs',irc,irCode,     &
              empty=0d0,ge=0d0,maxVal=1,result1=opt%leorms(1,2))

! Statistics: RMS(L2)
! -------------------
  CALL readKeys('RMS4_LEO', keyValue, irc)
  CALL ckoptr(1,'RMS4_LEO', keyValue, srName,                    &
              'A priori sigma phase L2 for LEOs',irc,irCode,     &
              empty=0d0,ge=0d0,maxVal=1,result1=opt%leorms(2,2))

! Elevation dep. sigma for stations
! ---------------------------------
  CALL ckoptb(1,(/'LEOELV'/),srName,                             &
              'Elevation-dependent sigmas for LEOs',irCode,      &
              result1=opt%leoelv)

! Cycle slips
! -----------
  CALL readKeys('NSLIPS', keyValue, irc)
  CALL ckopti(1,'NSLIPS', keyValue, srName, &
              'Number of cycle slips per file', irc, irCode, &
              empty=0,ge=0,maxVal=1,result1=opt%nslips)

  opt%isize  = 0
  opt%jl12eq = 0
  IF (opt%nslips > 0) THEN

    CALL readKeys('ISIZE', keyValue, irc)
    CALL ckopti(1,'ISIZE', keyValue, srName,                 &
                'Maximum size of a cycle slip', irc, irCode, &
                ge=1,maxVal=1,result1=opt%isize)

    CALL ckoptb(1,(/'JL12EQ'/), srName,                      &
                'Same cycle slips in L1 and L2', irCode,     &
                result1=opt%jl12eq)

  ENDIF

! Get model information from standard orbit file header
! -----------------------------------------------------
  filNam = ' '
  CALL rdstdh(filNam,stdHead,irc)

! If LEO should be processed, check STD-file
! ------------------------------------------
  CALL ckoptb(1,(/'LEOPROC'/),srName,                        &
              'LEO processing',irCode,                       &
              result1=iLeos)
  IF (iLeos == 1) THEN
    CALL gtflna(0,'LEOSTD', filNam, irc)
    IF (irc == 0) THEN
      CALL init_stdHead(stdLHead)
      CALL rdstdh(filNam,stdLHead,irc)
    ENDIF
  ENDIF


! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=irc)

  IF ( irCode /= 0 ) CALL exitrc(2)

! Write time window
! -----------------
  CALL priwin(1,opt%obswin%t)

! Write options
! -------------
  WRITE(lfnprt,"(' OPTIONS', &
            &  /,' -------', &
            & //,1X,79('-'))")
  WRITE(lfnprt,"(' Session string                           : ',A4)") opt%csess(1)
  WRITE(lfnprt,"(' Campaign name                            : ',A)") opt%campgn
  WRITE(lfnprt,"(' Sampling interval                        :',I7)") opt%inter
  WRITE(lfnprt,"(' Minimum elevation                        :',I7)") opt%ielev
  WRITE(lfnprt,"(' Maximum number of satellites per epoch   :',I7)") opt%mxosvn
  WRITE(lfnprt,"(' Troposphere model                        :',I7)") opt%itropo
  WRITE(lfnprt,"(' Troposphere model, meteo data            :',I7)") opt%iextra
  WRITE(lfnprt,"(' Model stochastic ionosphere              :',I7)") opt%iono(4)
  WRITE(lfnprt,"(' Night time electron number               :',I7)") opt%iono(1)
  WRITE(lfnprt,"(' Day time electron number                 :',I7)") opt%iono(2)
  IF (opt%iono(3)/=0) THEN
    WRITE(lfnprt,"(' Variance of electron number              :',I7)") opt%iono(3)
    WRITE(lfnprt,"(' Reference station for ionosphere variance: ',A)") opt%ionref
  ENDIF
  WRITE(lfnprt,"(' A priori sigma code L1 for stations      :',F12.4)") opt%rms(1,1)
  WRITE(lfnprt,"(' A priori sigma code L2 for stations      :',F12.4)") opt%rms(2,1)
  WRITE(lfnprt,"(' A priori sigma phase L1 for stations     :',F12.4)") opt%rms(1,2)
  WRITE(lfnprt,"(' A priori sigma phase L2 for stations     :',F12.4)") opt%rms(2,2)
  WRITE(lfnprt,"(' Init random number                       :',I7)") opt%ix
  WRITE(lfnprt,"(' Elevation-dependent sigmas for stations  :',I7)") opt%icoelv
  WRITE(lfnprt,"(' A priori sigma code L1 for LEOs          :',F12.4)") opt%leorms(1,1)
  WRITE(lfnprt,"(' A priori sigma code L2 for LEOs          :',F12.4)") opt%leorms(2,1)
  WRITE(lfnprt,"(' A priori sigma phase L1 for LEOs         :',F12.4)") opt%leorms(1,2)
  WRITE(lfnprt,"(' A priori sigma phase L2 for LEOs         :',F12.4)") opt%leorms(2,2)
  WRITE(lfnprt,"(' Elevation-dependent sigmas for LEOs      :',I7)") opt%leoelv
  WRITE(lfnprt,"(' Number of cycle slips per file           :',I7)") opt%nslips
  IF (opt%nslips>0) THEN
    WRITE(lfnprt,"(' Maximum size of a cycle slip             :',I7)") opt%isize
    WRITE(lfnprt,"(' Same cycle slips in L1 and L2            :',I7)") opt%jl12eq
  ENDIF
  WRITE(lfnprt,"(1X,79('-'),//)")

  RETURN
  END SUBROUTINE sminpn

END MODULE
