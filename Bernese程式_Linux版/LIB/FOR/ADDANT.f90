! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE s_addant
CONTAINS

  SUBROUTINE addant(add,anten,oant,isys,oldelv,oldazi,aoadmt,ARCV,&
                    AOAPCV,antbuf,iant)

! -------------------------------------------------------------------------
! Purpose:    Copy of antennas from old buffer in new buffer and
!             conversion from rel to abs if requested
!
! Author:     A. Gaede
!
! Created:    09-Jul-2007
! Last mod:   __-___-____
!
! Changes:    __-___-____ __:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------
! Used modules:
    USE m_bern,     ONLY: i4b, r8b, lfnprt, lfnerr
    USE m_global,   ONLY: g_svnsys
    USE d_phaecc,   ONLY: t_phasfil, recant, alcfrq
    USE s_rdacvinp, ONLY: opt
    USE d_const,    ONLY: date

! No implicits
    IMPLICIT NONE

! In:
    INTEGER(i4b)                              :: add
    TYPE(t_phasfil), DIMENSION(:),POINTER     :: anten
    INTEGER(i4b)                              :: oant
    INTEGER(i4b)                              :: isys
    INTEGER(i4b)                              :: oldelv
    INTEGER(i4b)                              :: oldazi
    INTEGER(i4b)                              :: aoadmt
    INTEGER(i4b)                              :: ARCV
    REAL(r8b), DIMENSION(:,:), POINTER        :: AOAPCV
    TYPE(t_phasfil), DIMENSION(:), POINTER    :: antbuf
    INTEGER(i4b)                              :: iant

! Local variables
    INTEGER(i4b)                              :: ifrq
    INTEGER(i4b)                              :: icor
    INTEGER(i4b)                              :: ielv
    INTEGER(i4b)                              :: iazi

    IF(add == 1 .AND. opt%convert == 1) THEN
      WRITE(lfnerr,"(/,' ### PG ADDANT: Relative pattern', &
                   &   ' for satellite antenna in input phase file.', &
                   & /,16X,'Antenna: ',A20,                           &
                   & /,16X,'Absolute pattern filled with zero!')")    &
                                                           anten(oant)%name
    ENDIF

! Conversion of receiver antennas if requested (for GPS only)
    IF (add == 2 .AND. isys == 0 .AND. opt%convert == 1 .AND. &
        anten(oant)%name(1:6) /= 'SIMULA') THEN
      IF (anten(oant)%sys(0)%resolu(2) /= antbuf(aoadmt)%sys(0)%resolu(2) .OR. &
           anten(oant)%sys(0)%resolu(4) /= antbuf(aoadmt)%sys(0)%resolu(4)) THEN
        WRITE(lfnerr,"(/,' ### ADDANT: Resolution of elevation dependent values ', &
                     &       'differs from AOAD/M_T   NONE antenna.', &
                     & /,16X,'Antenna: ',A20, &
                     & /,16X,'Antenna not converted and not included in output file!',/)") &
                  anten(oant)%name
        RETURN
      ENDIF
      anten(oant)%sys(0)%method = 'CONVERTED           '
      anten(oant)%sys(0)%date   = DATE
      DO ifrq=1,anten(oant)%sys(0)%nfreq
        DO icor=1,3
          anten(oant)%sys(0)%freq(ifrq)%off(0,icor) = &
                    anten(oant)%sys(0)%freq(ifrq)%off(0,icor) - &
                    recant(ARCV)%sys(0)%freq(ifrq)%off(0,icor) + &
                    antbuf(aoadmt)%sys(0)%freq(ifrq)%off(0,icor)
        ENDDO
        DO ielv=1,oldelv
          DO iazi=1,oldazi
            IF (oldazi == 1) THEN
              anten(oant)%sys(0)%freq(ifrq)%pat(0,ielv,iazi) = &
                       anten(oant)%sys(0)%freq(ifrq)%pat(0,ielv,iazi) + &
                       AOAPCV(ielv,ifrq)
            ELSE
              anten(oant)%sys(0)%freq(ifrq)%pat(0,ielv,iazi) = &
                       anten(oant)%sys(0)%freq(ifrq)%pat(0,ielv,iazi) + &
                       antbuf(aoadmt)%sys(0)%freq(ifrq)%pat(0,ielv,iazi)
            ENDIF
          ENDDO
        ENDDO
      ENDDO
      WRITE(lfnprt,"(1X,A20,I7,' --> Converted to absolute')") &
                              anten(oant)%name,anten(oant)%numb
    ELSEIF (anten(oant)%sys(isys)%nfreq /= 0) THEN
      IF (add == 2) THEN
        WRITE(lfnprt,"(1X,A20,I7,1X,A1,' from input phase file (no entry in ANTEX)')") &
                              anten(oant)%name,anten(oant)%numb,g_svnsys(isys)
      ELSE
        WRITE(lfnprt,"(1X,A20,I7,2X,' from input phase file (no entry in ANTEX)')") &
                              anten(oant)%name,anten(oant)%numb
      ENDIF
    ENDIF

! Allocation and copy of antenna
    CALL alcfrq(isys,anten(oant)%sys(isys)%nfreq,anten(oant)%sys(isys)%typ, &
                  anten(oant)%sys(isys)%resolu,antbuf,iant)
    antbuf(iant)%sys(isys) = anten(oant)%sys(isys)

  END SUBROUTINE addant

END MODULE s_addant
