MODULE s_PDDCB
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE pddcb(locq,meatyp,icarr,svn,is12,rectyp, &
                 ndiff,tobs,ahelp)

! -------------------------------------------------------------------------
! Purpose:    Compute partial derivatives with respect to differential
!             code bias (DCB) parameters
!
! Author:     S. Schaer
!
! Created:    01-May-2002
! Last mod.:  09-May-2009
!
! Changes:    08-Sep-2003  HU: recnam chr16 -> chr20
!             21-Jun-2007  SS: Call SR GETRCV with ISYST
!             01-Jul-2007  AG: isys=3 for Galileo implemented
!             04-Feb-2009  SS: Handle AS-free periods
!             09-May-2009  RD: New call of DCBCOR
!
! SR used:    exitrc, dcbcor, getrcv
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE d_const, ONLY: c

  USE s_getrcv
  USE s_dcbcor
  USE s_exitrc
  USE f_asmode
  IMPLICIT NONE

  INCLUDE 'COMFREQ.inc'

! List of parameters
! ------------------
  INTEGER(i4b), DIMENSION(:)      :: locq   ! Parameter description
  INTEGER(i4b)                    :: meatyp ! Measurement type
  INTEGER(i4b)                    :: icarr  ! Frequency
  INTEGER(i4b)                    :: svn    ! Satellite number
  INTEGER(i4b), DIMENSION(:)      :: is12   ! Station indices
  INTEGER(i4b)                    :: ndiff  ! Differencing level
                                            ! =0: ZD
                                            ! =1: SD

  REAL(r8b)                       :: tobs   ! Observation epoch in MJD
  REAL(r8b)                       :: ahelp  ! Partial derivative

  CHARACTER(LEN=20), DIMENSION(:) :: rectyp ! Receiver types

! Local variables
! ---------------
  INTEGER(i4b)                   :: igrp
  INTEGER(i4b)                   :: ityp
  INTEGER(i4b)                   :: ityp0
  INTEGER(i4b)                   :: ista
  INTEGER(i4b)                   :: icls
  INTEGER(i4b)                   :: isys
  INTEGER(i4b)                   :: nfreq
  INTEGER(i4b)                   :: isyst
  INTEGER(i4b), DIMENSION(2)     :: icode
  INTEGER(i4b), DIMENSION(2)     :: iwlfac

  REAL(r8b), DIMENSION(6,3,3)    :: facdcb ! Table of factors depending on
                                           ! - frequency
                                           ! - code bias type
                                           ! - receiver type
  REAL(r8b)                      :: dist

! Return if non-code measurement processed
! ----------------------------------------
  IF (meatyp /=2) RETURN

! Get code bias type
! ------------------
  igrp = locq(2)
  IF (igrp == 1) THEN
    ityp = locq(5)
  ELSEIF (igrp == 2) THEN
    ityp = locq(6)
  ELSE
    WRITE(lfnerr,'(/,A,/)') ' *** SR PDDCB: Illegal code bias group'
    CALL exitrc(2)
  ENDIF
  ityp0 = ABS(ityp)

! Define frequency-dependent factors
! ----------------------------------
  facdcb = 0d0

! P1-P2:
  facdcb(1,1,1:3) = -frq(2,svn)**2/(frq(1,svn)**2-frq(2,svn)**2)
  facdcb(2,1,1:3) = -frq(1,svn)**2/(frq(1,svn)**2-frq(2,svn)**2)
  facdcb(3,1,1:3) =  0d0
  facdcb(4,1,1:3) =  1d0
  facdcb(5,1,1:3) =  frq(1,svn)*frq(2,svn)/(frq(1,svn)**2-frq(2,svn)**2)
  facdcb(6,1,1:3) =  0d0

! P1-C1:
  facdcb(1,2,2)   = -1d0
  facdcb(2,2,2)   = -1d0
  facdcb(3,2,2)   = -1d0
  facdcb(4,2,2)   =  0d0
  facdcb(5,2,2)   = -1d0
  facdcb(6,2,2)   =  1d0

  facdcb(1,2,3)   = -1d0
  facdcb(2,2,3)   =  0d0
  facdcb(3,2,3)   = -frq(1,svn)**2/(frq(1,svn)**2-frq(2,svn)**2)
  facdcb(4,2,3)   = -1d0
  facdcb(5,2,3)   = -frq(1,svn)/(frq(1,svn)-frq(2,svn))
  facdcb(6,2,3)   =  frq(1,svn)/(frq(1,svn)+frq(2,svn))

! LC:
  facdcb(3,3,1:3) =  1d0

! Redefine factors in case of P1-C1 multiplier determination
! ----------------------------------------------------------
  IF (ityp == -2) THEN
   CALL dcbcor(2,0,svn,' ',' ',0,2,tobs,dist)
   facdcb(:,2,:)   =  0d0
   facdcb(3,2,1:3) = -dist
  ELSEIF (ityp < 1 .OR. ityp > 3) THEN
    WRITE(lfnerr,'(/,A,/)') ' *** SR PDDCB: Illegal code bias type'
    CALL exitrc(2)
  ENDIF

  DO ista = 1,1+ndiff

! Distinguish between P1/P2, C1/X2, and C1/P2 receiver class
! ----------------------------------------------------------
    CALL getrcv(rectyp(ista),nfreq,icode,iwlfac,icls,isyst)

    IF (icls == 0) THEN
      IF (icarr == 1) THEN
        icls = icode(1)
      ELSE
        WRITE(lfnerr,'(/,A,/,15X,A,A,/)') &
          ' *** SR PDDCB: Single-frequency receiver not expected', &
          'Receiver type: ',rectyp(ista)
        CALL exitrc(2)
      ENDIF
    ENDIF

! Handle AS-free periods in case of P1-C1 bias and C1/X2 receiver
! ---------------------------------------------------------------
    IF (asmode(tobs,svn) == 0) THEN
      IF (ityp == 2 .AND. icls == 2) icls = 1
    ENDIF

    IF (igrp == 1) THEN

! Satellite parameter
! -------------------
      IF (locq(3) == svn) &
        ahelp = ahelp+(-1d0)**(ista-1)*1d-9*c*facdcb(icarr,ityp0,icls)

    ELSE

! Receiver-specific parameter
! ---------------------------
      IF (svn < 100) THEN
        isys = 1
      ELSEIF (svn < 200) THEN
        isys = 2
      ELSEIF (svn < 300) THEN
        isys = 3
      ENDIF

!     ADD_GNSS_HERE

      IF (locq(3) == is12(ista) .AND. locq(5) == isys) &
        ahelp = ahelp+(-1d0)**(ista-1)*1d-9*c*facdcb(icarr,ityp0,icls)

    ENDIF

  ENDDO

END SUBROUTINE pddcb


END MODULE
