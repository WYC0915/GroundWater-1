MODULE s_SINPSORT
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE sinpsort(neq, sortPar)

! -------------------------------------------------------------------------
! Purpose:    This subroutine creates a sorting index of the parameters
!
! Author:     L. Mervart
!
! Created:    05-Sep-1998
! Last mod.:  06-Jul-2009
!
! Changes:    21-Dec-2001 HU: Use m_bern, ONLY for modules
!             21-Dec-2001 HU: m_addneq replaced by p_addneq
!             06-Aug-2002 HU: Order station names
!             26-Oct-2002 HU: Add geocenter coordinates at end
!             16-Jun-2003 RS: EOP loop: include nutation parameters
!                                       'DO icrd = 1, 5'
!             12-Aug-2003 RS: Correct sorting of troposphere parameters
!             09-Mar-2004 HU: Stop if unhandled parameter encountered
!             28-Jun-2005 MM: Unused variables removed
!             22-Sep-2005 RD: Use new module D_NEQ.f90
!             07-Apr-2006 AG: Satellite antenna offsets added
!             16-Aug-2006 AG: Format corrected
!             16-Nov-2006 AG: Sorting corrected for satellite antenna offsets
!             06-Jul-2009 DT: Add range biases (26)
!
! Copyright:  Astronomical Institute
!              University of Bern
!                  Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE m_time,   ONLY: t_timint
  USE m_global, ONLY: g_syssvn
  USE d_neq,    ONLY: t_neq
  USE p_addneq, ONLY: opt

  USE s_svn2prn
  USE s_exitrc
  IMPLICIT NONE

! List of Parameters
! ------------------
  TYPE(t_neq), INTENT(IN)    :: neq
  INTEGER(i4b), DIMENSION(*) :: sortPar

! Local Variables
! ---------------
  INTEGER(i4b)                 :: irc
  INTEGER(i4b)                 :: ipar
  INTEGER(i4b)                 :: hlp
  INTEGER(i4b)                 :: ipar1
  INTEGER(i4b)                 :: ipar2
  INTEGER(i4b)                 :: ip1
  INTEGER(i4b)                 :: ip2
  INTEGER(i4b)                 :: icrd
  INTEGER(i4b)                 :: ilcq5
  INTEGER(i4b)                 :: lastPar
  INTEGER(i4b)                 :: prevPar
  INTEGER(i4b)                 :: svnnr1
  INTEGER(i4b)                 :: svnnr2
  CHARACTER(LEN=4)             :: svnnr
  CHARACTER(LEN=staNameLength),DIMENSION(2)  :: name
  LOGICAL                      :: iterFlg
  TYPE(t_timint)               :: timint

! Station Coordinates
! -------------------
  lastPar = 0
  DO ipar = 1, neq%misc%npar
    IF (neq%par(ipar)%locq(1) == 1) THEN
      lastPar = lastPar + 1
      sortPar(lastPar) = ipar
    END IF
  END DO

  IF (opt%staSort(1) > 0) THEN
    DO
      iterFlg = .FALSE.
      DO ip1 = 1,lastPar
        ipar1 = sortPar(ip1)
        name(1)=neq%par(ipar1)%name(opt%staSort(1):opt%staSort(2))
        DO ip2 = ip1+1, lastPar
          ipar2 = sortPar(ip2)
          name(2)=neq%par(ipar2)%name(opt%staSort(1):opt%staSort(2))
          IF ( LGT(name(1),name(2)) .OR. &
               (name(1)==name(2) .AND.   &
                neq%par(ipar1)%locq(3) > neq%par(ipar2)%locq(3)) ) THEN
            hlp          = ipar1
            ipar1        = ipar2
            ipar2        = hlp
            sortPar(ip1) = ipar1
            sortPar(ip2) = ipar2
            iterFlg      = .TRUE.
          END IF

        END DO
      END DO
      IF ( .NOT. iterFlg ) EXIT
    END DO
  END IF
  prevPar=lastPar

! Station-Specific Troposphere Parameters
! ---------------------------------------
  DO ipar = 1, neq%misc%npar
    IF (neq%par(ipar)%locq(1) == 6) THEN
      lastPar = lastPar + 1
      sortPar(lastPar) = ipar
    END IF
  END DO

  IF (opt%staSort(1) > 0) THEN
    DO
      iterFlg = .FALSE.
      DO ip1 = prevPar+1,lastPar
        ipar1 = sortPar(ip1)
        name(1)=neq%par(ipar1)%name(opt%staSort(1):opt%staSort(2))
        DO ip2 = ip1+1, lastPar
          ipar2 = sortPar(ip2)
          name(2)=neq%par(ipar2)%name(opt%staSort(1):opt%staSort(2))
          IF ( LGT(name(1),name(2)) .OR. &
               (name(1)==name(2) .AND.   &
                neq%par(ipar1)%locq(6) > neq%par(ipar2)%locq(6)) ) THEN
            hlp          = ipar1
            ipar1        = ipar2
            ipar2        = hlp
            sortPar(ip1) = ipar1
            sortPar(ip2) = ipar2
            iterFlg      = .TRUE.
          END IF

        END DO
      END DO
      IF ( .NOT. iterFlg ) EXIT
    END DO
  END IF
  prevPar=lastPar

! Earth Orientation Parameters
! ----------------------------
  DO icrd = 1, 5
    DO ilcq5 = 1, 2
      DO ipar = 1, neq%misc%npar
        IF ( neq%par(ipar)%locq(1) == 10      .AND.  &
             neq%par(ipar)%locq(4) == icrd    .AND.  &
             neq%par(ipar)%locq(5) == ilcq5 ) THEN
          lastPar = lastPar + 1
          sortPar(lastPar) = ipar
        END IF
      END DO
    END DO
  END DO

  iterFlg = .FALSE.
  DO
    DO ip1 = prevPar+1, lastPar
      ipar1 = sortPar(ip1)

      DO ip2 = ip1+1, lastPar
        ipar2 = sortPar(ip2)

        IF ( neq%par(ipar1)%locq(4) /= neq%par(ipar2)%locq(4)   .OR.  &
             neq%par(ipar1)%locq(5) /= neq%par(ipar2)%locq(5) ) CYCLE

        IF ( neq%par(ipar1)%time%mean > neq%par(ipar2)%time%mean ) THEN
          hlp          = ipar1
          ipar1        = ipar2
          ipar2        = hlp
          sortPar(ip1) = ipar1
          sortPar(ip2) = ipar2
          iterFlg      = .TRUE.
        END IF

      END DO
    END DO
    IF ( .NOT. iterFlg ) EXIT
  END DO
  prevPar=lastPar

! Geocenter Coordinates
! ---------------------
  DO ipar = 1, neq%misc%npar
    IF (neq%par(ipar)%locq(1) == 16) THEN
      lastPar = lastPar + 1
      sortPar(lastPar) = ipar
    END IF
  END DO
  prevPar=lastPar

! Finally Satellite Antenna Offsets
! ---------------------------------
  DO ipar = 1, neq%misc%npar
    IF (neq%par(ipar)%locq(1) == 12) THEN
      IF (neq%par(ipar)%locq(4) /= 0) THEN
        WRITE(lfnerr, "(/,' *** SR SINPSORT: Block-specific satellite ',      &
                        &                'antenna offsets not handled',       &
                        &          /,18x,'for SINEX. Pre-eliminate ',         &
                        &                'parameters before writing the',     &
                        &          /,18x,'SINEX or setup satellite-specific', &
                        &                ' offsets.')")
        CALL exitrc(2)
      ENDIF
      lastPar = lastPar + 1
      sortPar(lastPar) = ipar
    END IF
  END DO

  DO
    iterFlg = .FALSE.
    DO ip1 = prevPar+1, lastPar
      ipar1 = sortPar(ip1)
!      CALL prn2svn(0,neq%par(ipar1)%locq(5), &
!                     neq%par(ipar1)%time%mean,svnnr1,timint,irc)
!      svnnr1=neq%par(ipar1)%locq(5)
      WRITE(svnnr,"(A1,I3)")g_syssvn(INT(neq%par(ipar1)%locq(5)/100)), &
                                                  neq%par(ipar1)%locq(5)
      CALL svn2prn(6,svnnr,neq%par(ipar1)%time%mean, &
                           svnnr1,timint,irc)
      DO ip2 = ip1+1, lastPar
        ipar2 = sortPar(ip2)
!        CALL prn2svn(0,neq%par(ipar2)%locq(5), &
!                       neq%par(ipar2)%time%mean,svnnr2,timint,irc)
!        svnnr2=neq%par(ipar2)%locq(5)
        WRITE(svnnr,"(A1,I3)")g_syssvn(INT(neq%par(ipar2)%locq(5)/100)), &
                                                    neq%par(ipar2)%locq(5)
        CALL svn2prn(6,svnnr,neq%par(ipar2)%time%mean, &
                           svnnr2,timint,irc)

        IF ( svnnr1 > svnnr2 .OR.  &
               (svnnr1==svnnr2 .AND. &
                neq%par(ipar1)%locq(3) > neq%par(ipar2)%locq(3)) ) THEN
          hlp          = ipar1
          ipar1        = ipar2
          ipar2        = hlp
          sortPar(ip1) = ipar1
          sortPar(ip2) = ipar2
          iterFlg      = .TRUE.
        END IF

      END DO
    END DO
    IF ( .NOT. iterFlg ) EXIT
  END DO
  prevPar=lastPar

! SLR Range Biases
! ----------------
  DO ipar = 1, neq%misc%npar
    IF (neq%par(ipar)%locq(1) == 26) THEN
      lastPar = lastPar + 1
      sortPar(lastPar) = ipar
    END IF
  END DO
  prevPar=lastPar

! Remaining Parameters
! --------------------
  DO ipar = 1, neq%misc%npar
    IF (neq%par(ipar)%locq(1) /=  1 .AND. &
        neq%par(ipar)%locq(1) /=  6 .AND. &
        neq%par(ipar)%locq(1) /= 10 .AND.  &
        neq%par(ipar)%locq(1) /= 12 .AND.  &
        neq%par(ipar)%locq(1) /= 16 .AND. &
        neq%par(ipar)%locq(1) /= 26      ) THEN
      WRITE(lfnerr, "(/,' *** SR SINPSORT: Parameter type not handled for SINEX:',I6, &
                   &  /,'                  SINEX file cannot be written.',   &
                   &  /,'                  Pre-eliminate parameter before',  &
                   &                     ' writing the SINEX.')") &
                   neq%par(ipar)%locq(1)
      CALL exitrc(2)
!      lastPar = lastPar + 1
!      sortPar(lastPar) = ipar
    END IF
  END DO

END SUBROUTINE sinpsort


END MODULE
