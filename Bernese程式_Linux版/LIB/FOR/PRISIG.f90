MODULE s_PRISIG
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE prisig(parTyp, sigma, nSig, seqSig, nChar)

! -------------------------------------------------------------------------
! Purpose:    Prints a priori sigmas for each parameter type (from input)
!             into ADDNEQ2 output file
!
! Author:     C. Urschl
!
! Created:    12-Nov-2002
! Last mod.:  12-Mar-2012
!
! Changes:    17-Jan-2003 CU: Correct description string for GIM
!             17-Feb-2003 LM: Order of statements corrected
!             22-Dec-2003 RS: Add satellite antenna phase center variations
!             25-Jan-2008 RD: add RAO/RAP parameters
!             13-Nov-2008 DT: Set dyn. orbit parameters according to orbdsc%orbmod
!             04-May-2009 RD: Scaling of loading models added
!             10-May-2009 RD: Receiver clock offsets/biases implemented
!             19-May-2009 DT: Add sigma for LOD
!             10-Jun-2009 RD: Add epoch satellite/receiver clocks
!             24-Jun-2009 RD: Put direct constraints on DCBs
!             13-Aug-2009 DT: Range biases added
!             14-Dec-2009 RD: Do nothing if nSig == 0
!             04-Jan-2010 SL: HOI scaling parameters (Case 27) added
!             30-Nov-2010 MM: GNSS-specific parameters
!             12-Mar-2012 HB: Layout unification for stochastic orbit
!                             parameters
!
! SR used:    exitrc
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE p_orbgen, ONLY: orbdsc
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                      :: parTyp ! Parameter type
  REAL(r8b),    DIMENSION(:)        :: sigma  ! Sigma value
  INTEGER(i4b)                      :: nSig   ! Total number of sigmas
  INTEGER(i4b), DIMENSION(:)        :: seqSig ! Sequence of sigmas
  INTEGER(i4b), OPTIONAL            :: nChar  ! Number of characters for
                                              ! output linelength

! input/output:

! output:

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=52), PARAMETER :: title =                            &
  (' Component                      A priori sigma  Unit')

  INTEGER(i4b),      PARAMETER :: maxComp = 18

! Local Variables
! ---------------
  CHARACTER(LEN=131)                                 :: line
  CHARACTER(LEN=shortLineLength), DIMENSION(maxComp) :: component
  CHARACTER(LEN=shortlineLength)                     :: string
  CHARACTER(LEN=4)                                   :: empiri

  INTEGER(i4b)                                       :: iChar
  INTEGER(i4b)                                       :: iSig
  INTEGER(i4b)                                       :: iComp
  INTEGER(i4b)                                       :: ilin


! Do nothing if no sigmas to be reported
! --------------------------------------
  IF (nSig == 0) RETURN

! Get the string of line
! ----------------------
  line=''
  IF (PRESENT(nChar)) THEN
    DO iChar = 1,nChar
      line=TRIM(line)//'-'
    ENDDO
  ELSE
    DO iChar = 1,131
      line=TRIM(line)//'-'
    ENDDO
  ENDIF

  DO iComp = 1, maxComp
    component(iComp) = ''
  ENDDO

  SELECT CASE ( parTyp )

! Station coordinates
! -------------------
    CASE (1)
      component(1)  = ' DX    Translation  Coordinates                 m       '
      component(2)  = ' DY    Translation  Coordinates                 m       '
      component(3)  = ' DZ    Translation  Coordinates                 m       '
      component(4)  = ' RX    Rotation     Coordinates                 ?       '
      component(5)  = ' RY    Rotation     Coordinates                 ?       '
      component(6)  = ' RZ    Rotation     Coordinates                 ?       '
      component(7)  = ' SC    Scale        Coordinates                         '

! Station velocities
! ------------------
      component(8)  = ' DX_V  Translation  Velocities                  m       '
      component(9)  = ' DY_V  Translation  Velocities                  m       '
      component(10) = ' DZ_V  Translation  Velocities                  m       '
      component(11) = ' RX_V  Rotation     Velocities                  ?       '
      component(12) = ' RY_V  Rotation     Velocities                  ?       '
      component(13) = ' RZ_V  Rotation     Velocities                  ?       '
      component(14) = ' SC_V  Scale        Velocities                          '

! Receiver clock offset
! ---------------------
    CASE (2)
      component(1)  = ' RCO   Receiver clock offset                    msec    '

! Orbital elements
! ----------------
    CASE (3)

    ! Get system for dynamic orbit parameters
    ! ---------------------------------------
      DO ilin = 1, orbdsc%nlin
      IF ( orbdsc%orbmod(ilin)(1:7) == 'EMPIRI:' )  &
         empiri = orbdsc%orbmod(ilin)(9:12)
      ENDDO

      component(1)  = ' A     Semi-major axis                          m       '
      component(2)  = ' E     Eccentricity                                     '
      component(3)  = ' I     Inclination                              arcsec  '
      component(4)  = ' NODE  Ascending node                           arcsec  '
      component(5)  = ' PER   Perigee                                  arcsec  '
      component(6)  = ' U0    Argument of latitude                     arcsec  '

      IF ( empiri == 'DYX ' .OR. empiri == 'DRSW' ) THEN
        component(7)  = ' D0    Direct rpr (constant)                    m/s**2  '
      ELSE
        component(7)  = ' R0    Radial constant                          m/s**2  '
      ENDIF

      IF ( empiri == 'DYX ' ) THEN
        component(8)  = ' Y0    Y-bias rpr (constant)                    m/s**2  '
        component(9)  = ' X0    X-bias rpr (constant)                    m/s**2  '
        component(10) = ' DC    Direct rpr (cos-term)                    m/s**2  '
        component(11) = ' YC    Y-bias rpr (cos-term)                    m/s**2  '
        component(12) = ' XC    X-bias rpr (cos-term)                    m/s**2  '
        component(13) = ' DS    Direct rpr (sin-term)                    m/s**2  '
        component(14) = ' YS    Y-bias rpr (sin-term)                    m/s**2  '
        component(15) = ' XS    X-bias rpr (sin-term)                    m/s**2  '
      ELSE
        component(8)  = ' S0    Along-track constant                     m/s**2  '
        component(9)  = ' W0    Out-of-plane constant                    m/s**2  '
        component(10) = ' Rcos  Radial      opr cos                      m/s**2  '
        component(11) = ' Scos  Along-track opr cos                      m/s**2  '
        component(12) = ' Wcos  Out-of-plane opr cos                     m/s**2  '
        component(13) = ' Rsin  Radial      opr sin                      m/s**2  '
        component(14) = ' Ssin  Along-track opr sin                      m/s**2  '
        component(15) = ' Wsin  Out-of-plane opr sin                     m/s**2  '
      ENDIF

! Receiver antenna offsets
! ------------------------
    CASE (5)
      component(1)  = ' N                                              m       '
      component(2)  = ' E                                              m       '
      component(3)  = ' U                                              m       '

! Site specific troposphere parameters
! ------------------------------------
    CASE (6)
      component(1)  = ' U     absolute                                 m       '
      component(2)  = ' U     relative                                 m       '
      component(3)  = ' N     absolute                                 m       '
      component(4)  = ' N     relative                                 m       '
      component(5)  = ' E     absolute                                 m       '
      component(6)  = ' E     relative                                 m       '

! Differential code biases
! ------------------------
    CASE (8)
      component( 1) = ' P1-P2 sat. (constraint to zero)                ns      '
      component( 2) = ' P1-P2 rec. (constraint to zero)                ns      '
      component( 3) = ' P1-C1 sat. (constraint to zero)                ns      '
      component( 4) = ' P1-C1 rec. (constraint to zero)                ns      '
      component( 5) = ' LC    sat. (constraint to zero)                ns      '
      component( 6) = ' LC    rec. (constraint to zero)                ns      '

      component( 7) = ' P1-P2 sat. (zero mean condition)               ns      '
      component( 8) = ' P1-P2 rec. (zero mean condition)               ns      '
      component( 9) = ' P1-C1 sat. (zero mean condition)               ns      '
      component(10) = ' P1-C1 rec. (zero mean condition)               ns      '
      component(11) = ' LC    sat. (zero mean condition)               ns      '
      component(12) = ' LC    rec. (zero mean condition)               ns      '

! Earth rotation parameters
! -------------------------
    CASE (10)
      component(1)  = ' XPOLE                                          mas     '
      component(2)  = ' YPOLE                                          mas     '
      component(3)  = ' UT1-UTC  first                                 ms      '
      component(4)  = ' UT1-UTC  other                                 ms      '
      component(5)  = ' LOD                                            ms/d    '
      component(6)  = ' DEPS     first                                 mas     '
      component(7)  = ' DEPS     other                                 mas     '
      component(8)  = ' DPSI     first                                 mas     '
      component(9)  = ' DPSI     other                                 mas     '

! Stochastic orbit parameters
! ---------------------------
    CASE (11)
      component(1)  = ' radial                                         m/s     '
      component(2)  = ' along-track                                    m/s     '
      component(3)  = ' out-of-plane                                   m/s     '
      component(4)  = ' dir. to Sun                                    m/s     '
      component(5)  = ' y-direction                                    m/s     '
      component(6)  = ' x-direction                                    m/s     '


! Satellite antenna offsets
! -------------------------
    CASE (12)
      component(1)  = ' X                                              m       '
      component(2)  = ' Y                                              m       '
      component(3)  = ' Z                                              m       '

! Geocenter coordinates
! ---------------------
    CASE (16)
      component(1)  = ' X                                              m       '
      component(2)  = ' Y                                              m       '
      component(3)  = ' Z                                              m       '

! Satellite antenna patterns
! --------------------------
    CASE (18)
      component(1)  = ' POLYGON value                                  m       '
      component(2)  = ' SPHERICAL HARMONIC value                       m       '

! Global ionosphere model parameters
! ----------------------------------
    CASE (19)
      component(1)  = ' V absolue                                      TECU    '
      component(2)  = ' V relative                                     TECU    '

! Scaling factors of Vienna grid files
! ------------------------------------
    CASE (22)
      component( 1) = ' ATM-NT-LOAD: all components                    1       '
      component( 2) = ' ATM-NT-LOAD: vertical                          1       '
      component( 3) = ' ATM-NT-LOAD: horizontal                        1       '
      component( 4) = ' ATM-NT-LOAD: up component                      1       '
      component( 5) = ' ATM-NT-LOAD: north component                   1       '
      component( 6) = ' ATM-NT-LOAD: east component                    1       '

      component( 7) = ' OCN-NT-LOAD: all components                    1       '
      component( 8) = ' OCN-NT-LOAD: vertical                          1       '
      component( 9) = ' OCN-NT-LOAD: horizontal                        1       '
      component(10) = ' OCN-NT-LOAD: up component                      1       '
      component(11) = ' OCN-NT-LOAD: north component                   1       '
      component(12) = ' OCN-NT-LOAD: east component                    1       '

      component(13) = ' HYDR-LOAD: all components                      1       '
      component(14) = ' HYDR-LOAD: vertical                            1       '
      component(15) = ' HYDR-LOAD: horizontal                          1       '
      component(16) = ' HYDR-LOAD: up component                        1       '
      component(17) = ' HYDR-LOAD: north component                     1       '
      component(18) = ' HYDR-LOAD: east component                      1       '

! Epochwise receiver clocks (reference)
! -------------------------------------
    CASE (23)
      component(1)  = ' Reference clock                                ns      '

! Epochwise satellite clocks (reference)
! --------------------------------------
    CASE (24)
      component(1)  = ' Reference clock                                ns      '

! Satellite antenna patterns
! --------------------------
    CASE (25)
      component(1)  = ' POLYGON value                                  m       '

! Range bias parameters
! ---------------------
    CASE (26)
      component(1)  = ' Absolute value                                 m       '

! HOI scaling parameters
! ----------------------
    CASE (27)
      component(1)  = ' 1     second order                             1       '
      component(2)  = ' 2     third order                              1       '
      component(3)  = ' 3     ray path bending                         1       '

! GNSS-specific station translations
! ----------------------------------
    CASE (30)
      component(1)  = ' 1     N                                        m       '
      component(2)  = ' 2     E                                        m       '
      component(3)  = ' 3     U                                        m       '
      component(4)  = ' Absolute value                                 m       '

! Wrong parameter type
! --------------------
    CASE DEFAULT
      WRITE(lfnerr,*) ' *** SR PRISIG: ADDNEQ2 does not support ' // &
                      'the parameter type ', parTyp
      CALL exitrc(2)

  END SELECT


! Print a priori sigma
! --------------------
! print title line
  WRITE(lfnprt,'(A52,/,1X,A)') title, line

! loop over all sigmas, print each sigma on seperate line
  DO iSig = 1, nSig
    IF (seqSig(iSig) > SIZE(component)) THEN
      WRITE(lfnerr,'(/,A,2(/,16X,A),/)') &
      ' *** SR PRISIG: Dear programmer, you have forgotten to adapt the size',&
                      'of the array containing the component descriptions to',&
                      'to your parameter specification.'
      CALL exitrc(2)
    ENDIF

    WRITE(string,'(A80)') component(seqSig(iSig))
    IF (sigma(iSig) >= 1d-5 .OR. sigma(iSig) == 0d0) THEN
      WRITE(string(34:43),'(F10.5)') sigma(iSig)
    ELSE
      WRITE(string(34:43),'(ES10.2)') sigma(iSig)
    ENDIF
    WRITE(lfnprt,'(A)') TRIM(string)
  ENDDO
  WRITE(lfnprt,'(/)')

END SUBROUTINE prisig

END MODULE
