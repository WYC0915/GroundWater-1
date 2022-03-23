! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE s_stdOrb2New

! -------------------------------------------------------------------------
! Purpose:
!
! Author:     L. Mervart
!
! Created:    26-May-2010
!
! Changes:
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

USE s_stdOrbit_t

IMPLICIT NONE

PRIVATE
PUBLIC :: stdOrb2New, stdOrb2NewInit

! Module Variable
! ------------------
TYPE(t_stdOrb), DIMENSION(1), SAVE :: stdOrb2New

CONTAINS

SUBROUTINE stdOrb2NewInit

  USE m_bern
  USE m_maxdim
  USE m_epoch
  USE s_stdOrbit_t
  USE s_gtflna
  USE s_opnfil
  USE s_opnerr
  USE s_alcerr
  USE s_exitrc

  IMPLICIT NONE

  ! Local Variables
  ! ---------------
  INTEGER(i4b)                      :: nArc, iFmt, nLin, nSat, nInt, qq
  INTEGER(i4b)                      :: ii, kk, iSat, iInt, irc, iLin, iq
  INTEGER(i4b)                      :: iVar, iDeg, iArc, iArcNew
  INTEGER(i4b), DIMENSION(MAXSAT)   :: navNum
  INTEGER(i4b)                      :: nVar
  INTEGER(i4b), DIMENSION(6)        :: locq
  REAL(r8b)                         :: ta, tb, zeron, elepress
  REAL(r8b)                         :: scalpa
  REAL(r8b), DIMENSION(3,MAXVAR,MAXINT+1) :: zcoen
  REAL(r8b)                         :: tOsc1, t0, hh
  REAL(r8b), DIMENSION(MAXINT+1)    :: tBound
  REAL(r8b), DIMENSION(7)           :: eleSat
  REAL(r8b), DIMENSION(MAXSAT,3,MAXINT+1) :: coeff
  CHARACTER(LEN=8)                  :: anltyp
  CHARACTER(LEN=80)                 :: line
  CHARACTER(LEN=1), DIMENSION(10)   :: source
  CHARACTER(LEN=fileNameLength)     :: fileName
  TYPE(t_orbArc), POINTER           :: orbArc
  TYPE(t_orbInt), POINTER           :: orbInt

  ! Read Standard-Orbit File
  ! ========================
  CALL gtflna(1, 'STDORB ', fileName, irc)
  CALL opnfil(lfnOrb, fileName, 'OLD', 'UNFORMATTED', 'READONLY', ' ', irc)
  CALL opnerr(lfnerr, lfnOrb, irc, fileName, 'stdOrb2New')

  READ(lfnOrb) nArc
  IF (nArc < 0) THEN
    READ(lfnOrb) iFmt, nArc
    READ(lfnOrb) nLin
    DO iLin = 1, nLin
      READ(lfnOrb) line
    ENDDO
  ENDIF

  ! Osculation Epoch and Interval Boundaries
  ! ----------------------------------------
  DO iArc = 1, nArc
    READ(lfnOrb) nSat, nInt, qq, (navNum(iSat),iSat=1,nSat), &
                 (source(ii),ii=1,10)

    ! Allocate stdOrb2New(1)%arbArc
    ! -----------------------------
    IF (iArc == 1) THEN
      ALLOCATE(stdOrb2New(1)%orbArc(nArc*nSat), STAT=irc)
      CALL alcerr(irc, 'stdOrb2New(1)%orbArc', (/nArc*nSat/), 'stdOrb2New')
    ELSE
      IF (nArc*nSat > SIZE(stdOrb2New(1)%orbArc)) THEN
        WRITE(lfnerr,'(A)') " *** stdOrb2New: changing number of satellites"
        CALL exitrc(2)
      ENDIF
    ENDIF

    READ(lfnOrb) tOsc1, tBound(1)

    DO iInt = 1, nInt+1
      READ(lfnOrb) tBound(iInt)
    ENDDO

    ! Read Osculating Elements, allocate the new Structure
    ! ----------------------------------------------------
    DO iSat = 1, nSat
      READ(lfnOrb) (eleSat(kk), kk = 1, 7)

      orbArc => stdOrb2New(1)%orbArc(iArc*iSat)
      orbArc%svn       = navNum(iSat)
      orbArc%tWin%t(1) = tBound(1)
      orbArc%tWin%t(2) = tBound(nInt+1)

      ALLOCATE(orbArc%orbInt(nInt), STAT=irc)
      CALL alcerr(irc, 'orbArc%orbInt', (/nInt/), 'stdOrb2New')
    ENDDO

    ! Loop over all Partial Intervals
    ! -------------------------------
    DO iInt = 1, nInt

      ! Set of Polynomial Coefficients
      ! ------------------------------
      READ(lfnOrb) t0, hh

      DO iq = 1, qq+1
        READ(lfnOrb) ((coeff(iSat,kk,iq), kk = 1, 3), iSat = 1, nSat)
      ENDDO

      DO iSat = 1, nSat
        orbArc => stdOrb2New(1)%orbArc(iArc*iSat)
        orbInt => orbArc%orbInt(iInt)
        orbInt%left   = ( t0 - .epochToReal.(orbArc%tWin%t(1)) ) * 86400.0
        orbInt%right  = orbInt%left + hh
        orbInt%shadow = .FALSE.
        orbInt%hh     = hh
        orbInt%t0     = orbInt%left
        orbInt%qq     = qq

        ALLOCATE(orbInt%rCoe(3,qq+1), STAT=irc)
        CALL alcerr(irc, 'orbInt%rCoe', (/3,qq+1/), 'stdOrb2New')
        orbInt%rCoe = coeff(iSat,:,:)
      ENDDO

    ENDDO

  ENDDO

  CLOSE(lfnOrb)

  ! Read RPR File
  ! =============
  CALL gtflna(1, 'RPRCOE ', fileName, irc)
  CALL opnfil(lfnRpr, fileName, 'OLD', 'UNFORMATTED', 'READONLY', ' ', irc)
  CALL opnerr(lfnerr, lfnRpr, irc, fileName, 'stdOrb2New')

  READ(lfnRpr) nArc

  DO iArc = 1, nArc
    READ(lfnRpr) nSat, nInt, qq, (navNum(iSat),iSat=1,nSat), &
                 (source(ii),ii=1,10)

    ! Allocate stdOrb2New(1)%orbArcPart
    ! ---------------------------------
    IF (iArc == 1) THEN
      ALLOCATE(stdOrb2New(1)%orbArcPart(nArc*nSat), STAT=irc)
      CALL alcerr(irc, 'stdOrb2New(1)%orbArcPart', (/nArc*nSat/), 'stdOrb2New')
    ELSE
      IF (nArc*nSat > SIZE(stdOrb2New(1)%orbArcPart)) THEN
        WRITE(lfnerr,'(A)') " *** stdOrb2New: changing number of satellites"
        CALL exitrc(2)
      ENDIF
    ENDIF

    READ(lfnRpr) ta, tb, zeron
    READ(lfnRpr) nVar, anltyp

    DO iInt = 1, nInt+1
      READ(lfnRpr) tBound(iInt)
    ENDDO

    DO iSat = 1, nSat
      DO iVar = 1, nVar
        READ(lfnRpr) elepress, scalpa, (locq(kk),kk=1,6)
      ENDDO

      orbArc => stdOrb2New(1)%orbArcPart(iArc*iSat)
      orbArc%svn       = navNum(iSat)
      orbArc%tWin%t(1) = tBound(1)
      orbArc%tWin%t(2) = tBound(nInt+1)
      ALLOCATE(orbArc%orbInt(nInt), STAT=irc)
      CALL alcerr(irc, 'orbArc%orbInt', (/nInt/), 'stdOrb2New')
    ENDDO

    DO iInt = 1, nInt

      READ(lfnRpr) t0, hh

      DO iSat = 1, nSat

        orbArc => stdOrb2New(1)%orbArcPart(iArc*iSat)
        orbInt => orbArc%orbInt(iInt)
        orbInt%left   = ( t0 - .epochToReal.(orbArc%tWin%t(1)) ) * 86400.0
        orbInt%right  = orbInt%left + hh
        orbInt%shadow = .FALSE.
        orbInt%hh     = hh
        orbInt%t0     = orbInt%left
        orbInt%qq     = qq

        ALLOCATE(orbInt%drdeCoe(3,6,qq+1), STAT=irc)
        CALL alcerr(irc, 'orbInt%drdeCoe', (/3,6,qq+1/), 'stdOrb2New')

        DO iVar = 1, nVar
          DO iDeg = 1, qq+1
            READ(lfnRpr) (zcoen(kk,iVar,iDeg), kk=1,3)
          ENDDO
        ENDDO

        orbInt%drdeCoe = zcoen(:,1:6,:)

      ENDDO
    ENDDO

  ENDDO

  CLOSE(lfnRpr)

END SUBROUTINE

END MODULE
