! ------------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! ------------------------------------------------------------------------------

MODULE d_phaecc

! ------------------------------------------------------------------------------
! Purpose:    This module reads/writeslevant antenna phase center information
!
! Author:     A.Gaede
!
! Created:    06-Jun-2007
!
! Changes:    08-Aug-2007 AG: Changes for SLR
!             09-Oct-2007 AG: ii in DO loop instead of isys
!             11-Dec-2007 HB: Modifications for SLR stations =>
!                             no offset or pcv is searched
!             03-Mar-2008 HB: Correct format for error messages
!             11-Mar-2008 HB: Correct format statements for error messages
!             15-Mar-2008 RD: Updates for estimating PCV models
!             27-Mar-2008 RD: Stop if duplicated entries in the antenna file
!             28-Mar-2008 MM: Some DO loops corrected (iTmp instead of iAnt)
!             06-May-2008 DT: Adopt for SLR processing
!             04-Aug-2008 DT: Use MTypeSLR from D_STACRX; remove sta_slr
!             02-Sep-2008 AS: Correct format for error message
!             05-Nov-2008 RD: Correct format for resulting PCV-file
!             06-May-2009 RD: Bugfix when writing satellite PCVs
!             22-Feb-2010 RD: Call GTSENSOR in RDPHASFIL only for satant
!             25-Oct-2010 SL: Trim output (wtphafil+wrt_ant), conv bug (sav_ant)
!             26-Oct-2010 SL: Use m_bern with ONLY, removal of unused mod/pars
!             02-Nov-2010 SL: Use undef_i from d_stacrx
!             26-Nov-2010 SL: If(size(?indx)>0) added in wtphafil
!             30-Nov-2010 SS/SL: Format in error message in search_off corrected
!             17-Dec-2010 RD: Conflict with "PRESENT" solved in wtphafil
!             23-Dec-2010 RD: Write GLONASS-only PCV result files
!             15-Feb-2011 RD: Correct isys counter in SR sav_buf
!             13-Apr-2011 RD: Bugfix individually calib. antennas in sav_ant
!             11-Jul-2011 HB: Use lineLength1024 instead of longLineLength
!                             (due to 1x1 degree maps)
!             07-Oct-2011 SL: MaxSys from m_global
!             24-May-2012 MF: Bugfix individually calib. antennas in sav_ant
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! ------------------------------------------------------------------------------

! Used modules
  USE m_bern,   ONLY: i4b, r8b, lfnLoc, lfnErr, BACKSLASH, &
                      lineLength, fileNameLength, lineLength1024
  USE m_global, ONLY: maxSys, g_svnsys
  USE d_stacrx, ONLY: MTypeSLR, undef_i

! No implicits
  IMPLICIT NONE

! Declare access rights
  PRIVATE
  PUBLIC  :: t_phasfil, t_freq,  t_sys,    recant,   satant,    &
             sta_off,   sta_pcv, sat_off,  sat_pcv,  init_buf,  &
             wtphafil,  antinfo, alcantbu, alcfrq,              &
             updmodel

! Global parameters
  REAL(r8b)                             :: version = 1.00 ! Version number
!!!  CHARACTER(LEN=20), PARAMETER          :: sta_slr = 'SLR                 '

! Phase center file structure
! ---------------------------
  TYPE t_freq
    INTEGER(i4b)                        :: freq   ! frequency number
    REAL(r8b), DIMENSION(:,:), POINTER  :: off    ! antenna offset
                                                  ! (1) = order
                                                  ! (2) = offset
    REAL(r8b), DIMENSION(:,:,:), POINTER:: pat    ! pattern values
                                                  ! (1) = order
                                                  ! (2) = zen
                                                  ! (3) = azi
    REAL(r8b), DIMENSION(:), POINTER    :: fac    ! factor
  END TYPE t_freq

  TYPE t_sys
    TYPE(t_freq), DIMENSION(:), POINTER :: freq   ! frequency structure
    INTEGER(i4b)                        :: nfreq  ! number of allocated
                                                  ! frequencies
    INTEGER(i4b)                        :: typ    ! type of PCV
    INTEGER(i4b), DIMENSION(4)          :: resolu ! relolution of grid/spherical
                                                  ! harmonic coeffitients
    CHARACTER(LEN=10)                   :: sinex  ! SINEX string
    CHARACTER(LEN=20)                   :: method ! calibration method
    CHARACTER(LEN=10)                   :: date   ! calibration date
    CHARACTER(LEN=linelength)           :: remark ! calibration date
  END TYPE t_sys

  TYPE t_phasfil
    CHARACTER(LEN=20)                   :: name   ! antenna name
    INTEGER(i4b)                        :: numb   ! antenna number
    TYPE(t_sys), DIMENSION(:), POINTER  :: sys    ! satellite system
    INTEGER(i4b)                        :: individ! individual calibrated
  END TYPE t_phasfil
  TYPE(t_phasfil), DIMENSION(:), POINTER, SAVE :: recant => null()
  TYPE(t_phasfil), DIMENSION(:), POINTER, SAVE :: satant => null()

CONTAINS

! Public subroutines
! =========================================================================
  SUBROUTINE sta_off(antnam, antnum, sta, prn, rfrq, sessid, offset)
! -------------------------------------------------------------------------
! Purpose :   Find PCO for receiver antenna
!
! Author :    A. Gaede
!
! Created :   06-Jun-2007
!
! Changes :   04-Aug-2008 DT: Use MTypeSLR from D_STACRX; remove sta_slr
!
! Copyright : Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! In:
    CHARACTER(LEN=20)                      :: antnam ! Antenna name
    INTEGER(i4b)                           :: antnum ! Antenna number
    CHARACTER(LEN=16)                      :: sta    ! Station name
    INTEGER(i4b)                           :: prn    ! PRN number
    INTEGER(i4b)                           :: rfrq   ! Requested frequency
    CHARACTER(LEN=4), OPTIONAL             :: sessid ! Session ID
! Out:
    REAL(r8b), DIMENSION(3)                :: offset ! PCO

    offset = 0.d0
    IF (antnam /= MTypeSLR) THEN
      CALL presentbuf(1)
      IF (PRESENT(sessid)) THEN
        CALL search_off(recant,prn,antnam,antnum,sta,rfrq,sessid,offset)
      ELSE
        CALL search_off(recant,prn,antnam,antnum,sta,rfrq,offset=offset)
      ENDIF
    ENDIF

  END SUBROUTINE sta_off
! =========================================================================
  SUBROUTINE sta_pcv(antnam, antnum, sta, prn, rfrq, sessid, zen, azi, corr)
! -------------------------------------------------------------------------
! Purpose :   Find PCV correction for receiver antennas
!
! Author :    A. Gaede
!
! Created :   06-Jun-2007
!
! Changes :   04-Aug-2008 DT: Use MTypeSLR from D_STACRX; remove sta_slr
!
! Copyright : Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! In:
    CHARACTER(LEN=20)                      :: antnam ! Antenna name
    INTEGER(i4b)                           :: antnum ! Antenna number
    CHARACTER(LEN=16)                      :: sta    ! Station name
    INTEGER(i4b)                           :: prn    ! PRN number
    INTEGER(i4b)                           :: rfrq   ! Requested frequency
    CHARACTER(LEN=4), OPTIONAL             :: sessid ! Session ID
    REAL(r8b)                              :: azi    ! Requested azimuth angle
    REAL(r8b)                              :: zen    ! Requested zenith angle
! Out:
    REAL(r8b)                              :: corr   ! PCV correction

    corr = 0.d0
    IF (antnam /= MTypeSLR) THEN
      CALL presentbuf(1)
      IF (PRESENT(sessid)) THEN
        CALL search_pcv(recant,prn,antnam,antnum,zen,azi,sta,rfrq,sessid,corr)
      ELSE
        CALL search_pcv(recant,prn,antnam,antnum,zen,azi,sta,rfrq,corr=corr)
      ENDIF
    ENDIF

  END SUBROUTINE sta_pcv
! =========================================================================
 SUBROUTINE sat_off(prn, epo, mea, rfrq, offset)
! -------------------------------------------------------------------------
! Purpose :   Find PCO for receiver antenna
!
! Author :    A. Gaede
!
! Created :   06-Jun-2007
!
! Changes :   __-___-____ __:
!
! Copyright : Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Used modules
    USE s_gtsensor

! In:
    INTEGER(i4b)                           :: prn
    REAL(r8b)                              :: epo
    CHARACTER(LEN=4)                       :: mea
    INTEGER(i4b)                           :: rfrq
! Out
    REAL(r8b), DIMENSION(3)                :: offset
! Local variables:
    CHARACTER(LEN=20)                      :: antnam
    INTEGER(i4b)                           :: antnum
    CHARACTER(LEN=16)                      :: sta

    CALL presentbuf(2)
    offset = 0.d0
    sta(1:16) = 'Satellite       '
    CALL gtsensor(prn, epo, mea, sensor=antnam, sensnr=antnum)
    CALL search_off(satant,prn,antnam,antnum,sta,rfrq,offset=offset)

 END SUBROUTINE sat_off
! =========================================================================
  SUBROUTINE sat_pcv(prn, epo, mea, rfrq, nad, azi, corr)
! -------------------------------------------------------------------------
! Purpose :   Find PCV correction for satellite antennas
!
! Author :    A. Gaede
!
! Created :   06-Jun-2007
!
! Changes :   __-___-____ __:
!
! Copyright : Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Used modules
    USE s_gtsensor
!
! In:
    INTEGER(i4b)                           :: prn    ! PRN number
    REAL(r8b)                              :: epo    ! Epoch
    CHARACTER(LEN=4)                       :: mea    ! Measurment type
    INTEGER(i4b)                           :: rfrq   ! Requested frequency
    REAL(r8b)                              :: nad    ! Requested nadir angle
    REAL(r8b)                              :: azi    ! Requested azimuth angle
! Out:
    REAL(r8b)                              :: corr   ! PCV correction
! Local variables:
    CHARACTER(LEN=20)                      :: antnam
    INTEGER(i4b)                           :: antnum
    CHARACTER(LEN=16)                      :: sta

    CALL presentbuf(2)
    corr = 0d0
    sta(1:16) = 'Satellite       '
    CALL gtsensor(prn, epo, mea, sensor=antnam, sensnr=antnum)

    CALL search_pcv(satant,prn,antnam,antnum,nad,azi,sta,rfrq,corr=corr)

  END SUBROUTINE sat_pcv
! =========================================================================
  SUBROUTINE antinfo(antnam,antnum,isys,sinex,typ,model,adopted,individ,index)
! -------------------------------------------------------------------------
! Purpose :   Find further information for antenna
!
! Author :    A. Gaede
!
! Created :   06-Jun-2007
! Last mod.:  02-Nov-2010
!
! Changes :   02-Nov-2010 SL: use undef_i
!
! Copyright : Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Used modules

! In:
    CHARACTER(LEN=20)                      :: antnam
    INTEGER(i4b)                           :: antnum
    INTEGER(i4b)                           :: isys
! Out:
    CHARACTER(LEN=10), OPTIONAL            :: sinex
    CHARACTER(LEN=1), OPTIONAL             :: typ
    CHARACTER(LEN=1), OPTIONAL             :: model
    INTEGER(i4b), OPTIONAL                 :: adopted
    INTEGER(i4b), OPTIONAL                 :: individ
    INTEGER(i4b), OPTIONAL                 :: index
! Local variables:
    TYPE(t_phasfil), DIMENSION(:), POINTER :: antbuf
    CHARACTER(LEN=20), DIMENSION(1)        :: aname
    INTEGER(i4b), DIMENSION(1)             :: anumb
    INTEGER(i4b)                           :: iant, iTmp
    INTEGER(i4b)                           :: ifreq
    INTEGER(i4b)                           :: ielv
    INTEGER(i4b)                           :: iazi

! Find index "iant"
! -----------------
    IF(antnam(1:3) == 'MW ' .OR. antnam(1:5) == 'SLR R') THEN
      CALL presentbuf(2)
!      ALLOCATE(antbuf(SIZE(satant)))
      antbuf => satant
    ELSE
      CALL presentbuf(1)
!      ALLOCATE(antbuf(SIZE(recant)))
      antbuf => recant
    ENDIF

    iant=0
    IF (PRESENT(sinex) .OR. PRESENT(typ) .OR. PRESENT(model) .OR. &
        PRESENT(adopted) .OR. PRESENT(individ) .OR. PRESENT(index)) THEN
      iAnt = 0
      DO iTmp=1,SIZE(antbuf)
        IF (antnam == antbuf(iTmp)%name .AND.    &
            antnum == antbuf(iTmp)%numb)THEN
          iAnt = iTmp
          EXIT
        ENDIF
      ENDDO

      IF(iAnt == 0) THEN
        aname(1) = antnam
        anumb(1) = antnum
        IF(antnam(1:3) == 'MW ' .OR. antnam(1:5) == 'SLR R') THEN
          CALL rdphasfil(satant=antbuf,aname=aname,anumb=anumb,indx2=iant)
        ELSE
          CALL rdphasfil(recant=antbuf,aname=aname,anumb=anumb,indx1=iant)
        ENDIF
      ENDIF
    ENDIF
! Index in buffer
! ---------------
    IF (iAnt /= 0 .AND. PRESENT(index)) index = iant
! Sinex string?
! -------------
    IF (iAnt /= 0 .AND. PRESENT(sinex)) sinex = antbuf(iant)%sys(isys)%sinex
! Calibration type?
! -----------------
    IF (iAnt /= 0 .AND. PRESENT(typ)) THEN
      typ = '?'
      IF (isys <= SIZE(antbuf(iant)%sys)-1 ) THEN
        IF (antbuf(iant)%sys(isys)%typ == 1) THEN
          IF (antbuf(iant)%sys(isys)%resolu(3) == 360) THEN
            typ = 'E'
          ELSE
            typ = 'F'
          ENDIF
        ENDIF
      ENDIF
    ENDIF
! Adopted from NONE?
! ------------------
    IF (iAnt /= 0 .AND. PRESENT(adopted)) THEN
      adopted = 0
      IF (isys <= SIZE(antbuf(iant)%sys)-1 ) THEN
        IF (antbuf(iant)%sys(isys)%method(1:7) == 'ADOPTED') THEN
          adopted = 1
        ENDIF
      ENDIF
    ENDIF
! Individual calibrated?
! ----------------------
    IF (iAnt /= 0 .AND. PRESENT(individ)) THEN
      IF (antbuf(iant)%individ == 1) THEN
        individ = 1
      ELSE
        individ = 0
      ENDIF
    ENDIF
! Relative or absolute antenna model?
! -----------------------------------
    IF (iAnt /= 0 .AND. PRESENT(model)) THEN
      model = 'R'
      iAnt = 0
      DO iTmp=1,SIZE(recant)
        IF ((recant(iTmp)%name == 'AOAD/M_T        NONE' .OR. &
             recant(iTmp)%name == 'AOAD/M_T            ').AND. &
             recant(iTmp)%numb == undef_i) THEN
          iAnt = iTmp
          EXIT
        ENDIF
      ENDDO

!       If antenna not found buffer from file
      IF (iAnt == 0) THEN
        aname(1) = 'AOAD/M_T        NONE'
        anumb(1) = undef_i
        CALL rdphasfil(recant=recant,aname=aname,anumb=anumb,indx1=iant)
      ENDIF
!
      IF (recant(iant)%sys(0)%typ /= 0) THEN
        loopfrq: DO ifreq=1,recant(iant)%sys(0)%nfreq
          DO ielv=1,SIZE(recant(iant)%sys(0)%freq(ifreq)%pat,2)
            DO iazi=1,SIZE(recant(iant)%sys(0)%freq(ifreq)%pat,3)
              IF(ABS(recant(iant)%sys(0)%freq(ifreq)%pat(0,ielv,iazi))>1.D-6)THEN
                model = 'A'
                EXIT loopfrq
              ENDIF
            ENDDO
          ENDDO
        ENDDO loopfrq
      ENDIF
    ENDIF
    NULLIFY(antbuf)
  END SUBROUTINE antinfo
! =========================================================================
  SUBROUTINE init_buf(bufsize, antlist, all, err)
! -------------------------------------------------------------------------
! Purpose :   Initialise buffers
!
! Author :    A. Gaede
!
! Created :   06-Jun-2007
!
! Changes :   18-Sep-2012 RD: Correctly deallocate arrays
!
! Copyright : Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Used modules
    USE s_alcerr
    USE m_maxdim, ONLY: maxsaa, maxrec
!
! In:
    INTEGER(i4b), DIMENSION(2), OPTIONAL        :: bufsize ! Size of buffer to
                                                           ! be initialized,
                                                           ! (1)=nrec, (2)=nsat
    CHARACTER(LEN=26), DIMENSION(:), OPTIONAL   :: antlist ! List of antennas to
                                                           ! be buffered
    INTEGER(i4b), OPTIONAL                      :: all     ! Buffer all antennas
                                                           ! if "all" is present
    INTEGER(i4b), OPTIONAL                      :: err     ! Do not abort if
                                                           ! present
! Local variables
    INTEGER(i4b)                                :: nrec
    INTEGER(i4b)                                :: nsat
    INTEGER(i4b)                                :: iant
    INTEGER(i4b)                                :: ii
    INTEGER(i4b)                                :: iac
    CHARACTER(LEN=20), DIMENSION(:),ALLOCATABLE :: aname
    INTEGER(i4b), DIMENSION(:), ALLOCATABLE     :: anumb

! Default buffer size
    nrec = maxrec
    nsat = maxsaa
! Set nsat and nrec if bufsize is present
    IF (PRESENT(bufsize)) THEN
      nrec = bufsize(1)
      nsat = bufsize(2)
    ENDIF
! Count # of sat and rec antennas if antlist is present
    IF (PRESENT(antlist)) THEN
      nsat = 0
      nrec = 0
!     count lines
      DO iant=1,SIZE(antlist)+1
        IF (iant <= SIZE(antlist) ) THEN
          IF (LEN_TRIM(antlist(iant))/= 0) CYCLE
        ENDIF
        ALLOCATE(aname(iant-1),stat=iac)
        CALL alcerr(iac,'aname',(/iant-1/),'d_phaecc:init_buf')
        ALLOCATE(anumb(iant-1),stat=iac)
        CALL alcerr(iac,'anumb',(/iant-1/),'d_phaecc:init_buf')
        aname=''
        anumb=0
        DO ii=1,iant-1
          READ(antlist(ii),"(A20,I6)")aname(ii),anumb(ii)
          IF (aname(ii)(1:3) == 'MW ' .OR. aname(ii)(1:5) == 'SLR R') THEN
            nsat = nsat + 1
          ELSE
            nrec = nrec + 1
          ENDIF
        ENDDO
        EXIT
      ENDDO
    ENDIF
! Set default if all is present
    IF (PRESENT(all)) THEN
      nrec = maxrec
      nsat = maxsaa
    ENDIF
! Allocate recant and satant
    IF (nrec /= 0) THEN
      CALL alcantbu(nrec,maxsys,recant)
    ENDIF
    IF (nsat /= 0) THEN
      CALL alcantbu(nsat,0,satant)
    ENDIF
! Fill buffers if antlist is present
    IF (PRESENT(antlist)) THEN
      IF (nsat /= 0 .AND. nrec /= 0) THEN
        CALL rdphasfil(recant,satant,aname,anumb)
      ELSEIF (nsat /= 0) THEN
        CALL rdphasfil(satant=satant,aname=aname,anumb=anumb)
      ELSEIF (nrec /= 0) THEN
        CALL rdphasfil(recant=recant,aname=aname,anumb=anumb,err=err)
      ENDIF
      DO iant=1,SIZE(antlist)+1
        IF (iant <= SIZE(antlist) ) THEN
          IF (LEN_TRIM(antlist(iant))/= 0) CYCLE
        ENDIF
        DEALLOCATE(aname)
        DEALLOCATE(anumb)
        EXIT
      ENDDO
    ELSEIF (PRESENT(all)) THEN
      CALL rdphasfil(recant,satant)
    ENDIF

  END SUBROUTINE init_buf
! =========================================================================
  SUBROUTINE wtphafil(filphc,saant,reant,title,model,filinfo,sindx,rindx)
! -------------------------------------------------------------------------
! Purpose :   Write phase center information file
!
! Author :    A. Gaede
!
! Created :   06-Jun-2007
! Last mod.:  26-Nov-2010
!
! Changes :   25-Oct-2010 SL: model and filinfo trimmed for output
!             26-Nov-2010 SL: if(size(?indx)>0) added
!             17-Dec-2010 RD: conflict with "PRESENT" solved
!
! Copyright : Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Used modules
    USE s_opnfil
    USE s_opnerr

! In:
    CHARACTER(LEN=fileNameLength)                    :: filphc
    TYPE(t_phasfil), DIMENSION(:), POINTER, OPTIONAL :: reant
    TYPE(t_phasfil), DIMENSION(:), POINTER, OPTIONAL :: saant
    CHARACTER(LEN=80)                                :: title
    CHARACTER(LEN=10)                                :: model
    CHARACTER(LEN=60)                                :: filinfo
    INTEGER(i4b), DIMENSION(:), OPTIONAL             :: sindx
    INTEGER(i4b), DIMENSION(:), OPTIONAL             :: rindx
! Local variables:
    INTEGER(i4b)                                     :: iostat

! Open antenna phase center file
! ------------------------------
    CALL opnfil(lfnloc,filphc,'UNKNOWN','FORMATTED',' ',' ',iostat)
    CALL opnerr(lfnerr,lfnloc,iostat,filphc,'WTPHAFIL')

! Write title lines
! -----------------
    WRITE(lfnloc,"(A80,/,80('-'))") title
! Write format version,antenna model and last input file
    WRITE(lfnloc,"('FORMAT VERSION:',F6.2)") version
    WRITE(lfnloc,"('ANTENNA MODEL: ',A)") TRIM(model)
    WRITE(lfnloc,"('LAST INPUT FILE: ',A,/)") TRIM(filinfo)

! Write format explanations
! -------------------------
    WRITE(lfnloc,"('PHASE CENTER OFFSETS AND MAPS AND/OR ',      &
           &    'COEFFICIENTS OF',                               &
           &    ' SPHERICAL HARMONICS IN MM:',/,79('-'),         &
           & //,'TYPE 1 :  ELEVATION (RESP. NADIR)/AZIMUTH GRID',&
           &  /,'TYPE 2 :  SPHERICAL HARMONICS COEFFICIENTS ',   &
           &    '(UNNORMALIZED)',                                &
           &  /,'TYPE 3 :  SPHERICAL HARMONICS COEFFICIENTS ',   &
           &    '(NORMALIZED)',                                  &
           &  /,'TYPE 4 :  SPHERICAL HARMONICS COEFFICIENTS, ',  &
           &    'UPPER HEMISPHERE ONLY (NORMALIZED)',            &
           & //,'D(Z)   :  ZENITH (RESP. NADIR) TABULAR ',       &
           &    'INTERVAL (DEGREES)',                            &
           &  /,'D(A)   :  AZIMUTH TABULAR INTERVAL (DEGREES)',  &
           &  /,'N(Z)   :  DEGREE OF SPHERICAL HARMONICS DEVE',  &
           &    'LOPMENT',                                       &
           &  /,'M(A)   :  ORDER  OF SPHERICAL HARMONICS DEVE',  &
           &    'LOPMENT',                                       &
           &  /,'M(Z)   :  MAXIMUM ZENITH (RESP. NADIR) ANGLE')")


    IF (PRESENT(saant)) THEN
      IF (PRESENT(sindx)) THEN
        IF (SIZE(sindx)>0) THEN
          CALL wrt_ant(saant,sindx)
        ELSE
          CALL wrt_ant(saant)
        ENDIF
      ELSE
        CALL wrt_ant(saant)
      ENDIF
    ENDIF
    IF (PRESENT(reant)) THEN
      IF (PRESENT(rindx)) THEN
        IF (SIZE(rindx)>0) THEN
          CALL wrt_ant(reant,rindx)
        ELSE
          CALL wrt_ant(reant)
        ENDIF
      ELSE
        CALL wrt_ant(reant)
      ENDIF
    ENDIF

! Add blank lines
    WRITE(lfnloc,'(//)')
!
! Close antenna phase center output file
! --------------------------------------
    CLOSE(lfnloc)

  END SUBROUTINE wtphafil
! =========================================================================
  SUBROUTINE alcantbu(nant,nsys,antbuf)
! -------------------------------------------------------------------------
! Purpose :   Initialize frequency pointer for antbuf(iant)%sys(isys)
!
! Author :    A. Gaede
!
! Created :   06-Jun-2007
!
! Changes :   __-___-____ __:
!
! Copyright : Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Used modules
    USE s_alcerr

! In:
    INTEGER(i4b)                                :: nant
    INTEGER(i4b)                                :: nsys
    TYPE(t_phasfil), DIMENSION(:), POINTER      :: antbuf

! Local variables
    INTEGER(i4b)                                :: iac
    INTEGER(i4b)                                :: iant
    INTEGER(i4b)                                :: isys

    IF (ASSOCIATED(antbuf)) DEALLOCATE(antbuf)
    ALLOCATE(antbuf(nant),stat=iac)
    CALL alcerr(iac,'antbuf(nant)',(/nant/),'ALCANTBU')
    DO iant=1,nant
      antbuf(iant)%name = ''
      antbuf(iant)%numb = 0
      antbuf(iant)%individ = 0
      ALLOCATE(antbuf(iant)%sys(0:nsys),stat=iac)
      CALL alcerr(iac,'antbuf(iant)%sys(0:nsys)',(/nsys+1/),'ALCANTBU')
      DO isys=0,nsys
        NULLIFY(antbuf(iant)%sys(isys)%freq)
        antbuf(iant)%sys(isys)%nfreq  = 0
        antbuf(iant)%sys(isys)%sinex  = ''
        antbuf(iant)%sys(isys)%method = ''
        antbuf(iant)%sys(isys)%date   = ''
        antbuf(iant)%sys(isys)%remark = ''
        antbuf(iant)%sys(isys)%typ    = 0
        antbuf(iant)%sys(isys)%resolu(:) = 0
      ENDDO
    ENDDO

  END SUBROUTINE alcantbu
! =========================================================================
  SUBROUTINE alcfrq(isys,nfreq,typ,resolu,antbuf,iant)
! -------------------------------------------------------------------------
! Purpose :   Initialize frequency pointer for antbuf(iant)%sys(isys)
!
! Author :    A. Gaede
!
! Created :   06-Jun-2007
!
! Changes :   __-___-____ __:
!
! Copyright : Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Used modules
    USE s_alcerr
    USE s_exitrc

! In:
    INTEGER(i4b)                                :: isys
    INTEGER(i4b)                                :: nfreq
    INTEGER(i4b)                                :: typ
    INTEGER(i4b), DIMENSION(4)                  :: resolu
    TYPE(t_phasfil), DIMENSION(:), POINTER      :: antbuf
    INTEGER(i4b)                                :: iant

! Local variables
    INTEGER(i4b)                                :: iac
    INTEGER(i4b)                                :: ifrq
    INTEGER(i4b)                                :: nelv
    INTEGER(i4b)                                :: nazi


    IF (typ == 1) THEN
      IF (resolu(2) == resolu(4)) THEN
        nelv = 1
      ELSE
        nelv = resolu(4) / resolu(2) + 1
      ENDIF
      IF (resolu(3) == 360) THEN
        nazi = 1
      ELSE
        nazi = 360 / resolu(3) + 1
      ENDIF
    ELSEIF (typ >= 2 .AND. typ <= 4) THEN
      nelv = resolu(2)
      nazi = 2*resolu(3) + 1
    ELSEIF ( typ < 0 .OR. typ > 4) THEN
      WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,I8,/,16X,A,I3/)')       &
            ' *** SR ALCFRQ: Unknown PCV type.',                 &
                  'Antenna name:   ' // TRIM(antbuf(iant)%name), &
                  'Antenna number: ',antbuf(iant)%numb,          &
                  'Type number:    ',typ
      CALL exitrc(2)
    ENDIF
    IF (ASSOCIATED(antbuf(iant)%sys(isys)%freq)) DEALLOCATE(antbuf(iant)%sys(isys)%freq)
    ALLOCATE(antbuf(iant)%sys(isys)%freq(nfreq),stat=iac)
    CALL alcerr(iac,'antbuf(iant)%sys(isys)%freq',(/nfreq/),'ALCFRQ')
    DO ifrq=1,nfreq
      antbuf(iant)%sys(isys)%freq(ifrq)%freq = 0
      NULLIFY(antbuf(iant)%sys(isys)%freq(ifrq)%fac)
      ALLOCATE(antbuf(iant)%sys(isys)%freq(ifrq)%fac(0:resolu(1)),stat=iac)
      CALL alcerr(iac,'antbuf(iant)%sys(isys)%freq(ifrq)%fac',(/resolu(1)+1/),'ALCFRQ')
      antbuf(iant)%sys(isys)%freq(ifrq)%fac = 1d0
      NULLIFY(antbuf(iant)%sys(isys)%freq(ifrq)%off)
      ALLOCATE(antbuf(iant)%sys(isys)%freq(ifrq)%off(0:resolu(1),3),stat=iac)
      CALL alcerr(iac,'antbuf(iant)%sys(isys)%freq(ifrq)%off',(/resolu(1)+1,3/),'ALCFRQ')
      antbuf(iant)%sys(isys)%freq(ifrq)%off = 0d0
      IF (typ /= 0) THEN
        NULLIFY(antbuf(iant)%sys(isys)%freq(ifrq)%pat)
        ALLOCATE(antbuf(iant)%sys(isys)%freq(ifrq)%pat(0:resolu(1),nelv,nazi),stat=iac)
        CALL alcerr(iac,'antbuf(iant)%sys(isys)%freq(ifrq)%pat',(/resolu(1)+1,nelv,nazi/),'ALCFRQ')
        antbuf(iant)%sys(isys)%freq(ifrq)%pat = 0d0
      ENDIF
    ENDDO

  END SUBROUTINE alcfrq
! =========================================================================
! Private subroutines
! =========================================================================
  SUBROUTINE presentbuf(antb)
! -------------------------------------------------------------------------
! Purpose :   Check initialization of buffer
!
! Author :    A. Gaede
!
! Created :   06-Jun-2007
!
! Changes :   __-___-____ __:
!
! Copyright : Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Subroutine used:
    USE s_exitrc
! In:
    INTEGER(i4b)                    :: antb

    IF (antb == 1 .AND. .NOT. ASSOCIATED(recant)) THEN
      WRITE(lfnerr,"(/,' *** SR PRESENTBUF: Dear programmer. Please call', &
                   &       ' SR init_buf first in',                        &
                   & /,20X,'your program before using the receiver ',      &
                   &       'antenna buffer',/)")
      Call exitrc(2)
    ELSEIF (antb == 2 .AND. .NOT. ASSOCIATED(satant)) THEN
      WRITE(lfnerr,"(/,' *** SR PRESENTBUF: Dear programmer. Please call', &
                   &       ' SR init_buf first in',                        &
                   & /,20X,'your program before using the satellite ',      &
                   &       'antenna buffer',/)")
      Call exitrc(2)
    ENDIF
  END SUBROUTINE presentbuf
! =========================================================================
  SUBROUTINE wrt_ant(antbuf,indx)
! -------------------------------------------------------------------------
! Purpose :   Write offset values in phase center information file
!
! Author :    A. Gaede
!
! Created :   06-Jun-2007
! Last mod.:  23-Dec-2010
!
! Changes :   25-Oct-2010 SL: remarks trimmed for output
!             28-Oct-2010 SL: bounds error corrected
!             23-Dec-2010 RD: Write GLONASS-only PCV result files
!
! Copyright : Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
    USE s_exitrc
! In:
    TYPE(t_phasfil), DIMENSION(:), POINTER           :: antbuf
    INTEGER(i4b), DIMENSION(:), OPTIONAL             :: indx
! Local variables:
    LOGICAL                                          :: firstl
    INTEGER(i4b)                                     :: ii
    INTEGER(i4b)                                     :: iant
    INTEGER(i4b)                                     :: iant1
    INTEGER(i4b)                                     :: isys
    INTEGER(i4b)                                     :: nsys
    INTEGER(i4b)                                     :: ifreq
    INTEGER(i4b)                                     :: ord
    INTEGER(i4b)                                     :: iord
    INTEGER(i4b)                                     :: ich1
    INTEGER(i4b)                                     :: ich2
    INTEGER(i4b)                                     :: ielv
    INTEGER(i4b)                                     :: iazi
    CHARACTER(LEN=1)                                 :: csys
    CHARACTER(LEN=lineLength1024)                    :: STRNG

! Loop over all antennas in buffer
! --------------------------------
    DO iant1=1,SIZE(antbuf)
      IF (antbuf(iant1)%name == '') EXIT
      IF (PRESENT(indx)) THEN
        IF(SIZE(indx) > 0) iant = indx(iant1)
      ELSE
        iant = iant1
      ENDIF

      IF (antbuf(iant)%name == '') EXIT
      WRITE(lfnloc,"(/,'ANTENNA/RADOME TYPE  NUMBER SYS FRQ TYP D(O)', &
                 &   ' D(Z) D(A) M(Z)  SINEX      METHOD            ', &
                 &   '   DATE       REMARK',                           &
                 & /,'******************** ****** *   *** ***  ***  ', &
                 &   '***  ***  ***  ********** ********************', &
                 &   ' ********** ************************************')")

      IF (antbuf(iant)%name(1:3) == 'MW ' .OR. &
          antbuf(iant)%name(1:5) == 'SLR R') THEN
        nsys=0
      ELSE
        nsys=maxsys
      ENDIF
! Write allocation lines
      firstl = .TRUE.
      DO isys=0,nsys
        IF(isys+1 > SIZE(antbuf(iant)%sys)) EXIT
        IF(antbuf(iant)%sys(isys)%nfreq == 0) CYCLE
        IF (antbuf(iant)%name(1:3) == 'MW ' .OR. &
            antbuf(iant)%name(1:5) == 'SLR R') THEN
          csys = ' '
        ELSE
          csys = g_svnsys(isys)
        ENDIF
        IF (firstl) THEN
          WRITE(lfnloc,"(A20,1X,I6,1X,A1,2X,2(1X,I3),4(2X,I3),2X,A10,  &
                        &   1X,A20,1X,A10,1X,A)") antbuf(iant)%name,   &
                           antbuf(iant)%numb,csys,                     &
                           antbuf(iant)%sys(isys)%nfreq,               &
                           antbuf(iant)%sys(isys)%typ,                 &
                          (antbuf(iant)%sys(isys)%resolu(II),II=1,4),  &
                           antbuf(iant)%sys(isys)%sinex,               &
                           antbuf(iant)%sys(isys)%method,              &
                           antbuf(iant)%sys(isys)%date,                &
                           TRIM(antbuf(iant)%sys(isys)%remark)
          firstl = .false.
        ELSE
          WRITE(lfnloc,"(28X,A1,2X,2(1X,I3),4(2X,I3),2X,A10,           &
                        &   1X,A20,1X,A10,1X,A)") csys,                &
                           antbuf(iant)%sys(isys)%nfreq,               &
                           antbuf(iant)%sys(isys)%typ,                 &
                          (antbuf(iant)%sys(isys)%resolu(II),II=1,4),  &
                           antbuf(iant)%sys(isys)%sinex,               &
                           antbuf(iant)%sys(isys)%method,              &
                           antbuf(iant)%sys(isys)%date,                &
                           TRIM(antbuf(iant)%sys(isys)%remark)
        ENDIF
      ENDDO
! Write offset section
      WRITE(lfnloc,"(/,'      NORTH MM  EAST MM   UP MM   FACTOR', &
                   & /,'      *****.** *****.** *****.**  **********')")
      DO isys=0,nsys
        IF(isys+1 > SIZE(antbuf(iant)%sys)) EXIT
        IF(antbuf(iant)%sys(isys)%nfreq == 0) CYCLE
        IF (antbuf(iant)%name(1:3) == 'MW ' .OR. &
            antbuf(iant)%name(1:5) == 'SLR R') THEN
          csys = ' '
        ELSE
          csys = g_svnsys(isys)
        ENDIF
        DO ifreq=1,antbuf(iant)%sys(isys)%nfreq
          DO ord=0,antbuf(iant)%sys(isys)%resolu(1)
            WRITE(lfnloc,"(A1,I2.2,1X,I1,1X,3(f8.2,1X),1X,E10.3)") &
                     csys,antbuf(iant)%sys(isys)%freq(ifreq)%freq, &
                     antbuf(iant)%sys(isys)%resolu(1),             &
                     (antbuf(iant)%sys(isys)%freq(ifreq)%off(ord,ii),ii=1,3), &
                     antbuf(iant)%sys(isys)%freq(ifreq)%fac(ord)
          ENDDO
        ENDDO
      ENDDO
      WRITE(lfnloc,"()")
! Write pattern section
      DO isys=0,nsys
        IF(isys+1 > SIZE(antbuf(iant)%sys)) EXIT
        IF(antbuf(iant)%sys(isys)%typ == 0) CYCLE
        IF(antbuf(iant)%sys(isys)%nfreq == 0) CYCLE
        IF (antbuf(iant)%name(1:3) == 'MW ' .OR. &
            antbuf(iant)%name(1:5) == 'SLR R') THEN
          csys = ' '
        ELSE
          csys = g_svnsys(isys)
        ENDIF
        DO ifreq=1,antbuf(iant)%sys(isys)%nfreq
          DO ord=0,antbuf(iant)%sys(isys)%resolu(1)
            IF (antbuf(iant)%sys(isys)%typ == 1) THEN
              WRITE(lfnloc,"(A1,I2.2,1X,I1,'  A',A1,'Z',I6,100I7)") csys,     &
                             antbuf(iant)%sys(isys)%freq(ifreq)%freq,         &
                             ord,BACKSLASH,                                   &
                            (antbuf(iant)%sys(isys)%resolu(2)*(II-1),         &
                             II=1,SIZE(antbuf(iant)%sys(isys)%freq(1)%pat,2))
            ELSE
              WRITE(lfnloc,"(A1,I2.2,1X,I1,'  A',A1,'Z',I6,100I7)") csys,     &
                             antbuf(iant)%sys(isys)%freq(ifreq)%freq,         &
                             ord,BACKSLASH,                                   &
                            (II,II=1,SIZE(antbuf(iant)%sys(isys)%freq(1)%pat,2))
            ENDIF
            DO iazi=1,SIZE(antbuf(iant)%sys(isys)%freq(ifreq)%pat,3)
              IF (antbuf(iant)%sys(isys)%typ == 1) THEN
                WRITE(lfnloc,'(A1,I2.2,1X,I1,I4,100F7.2)')                    &
                   csys,antbuf(iant)%sys(isys)%freq(ifreq)%freq,ord,          &
                   antbuf(iant)%sys(isys)%resolu(3)*(iazi-1),                 &
                   (antbuf(iant)%sys(isys)%freq(ifreq)%pat(0,ielv,iazi),      &
                    ielv=1,SIZE(antbuf(iant)%sys(isys)%freq(ifreq)%pat,2))
              ELSE
                STRNG=' '
                iord=iazi-(SIZE(antbuf(iant)%sys(isys)%freq(1)%pat,3)+1)/2
                DO ielv=ABS(iord),SIZE(antbuf(iant)%sys(isys)%freq(1)%pat,2)
                  IF (ielv == 0) CYCLE
                  ICH1=(ielv-1)*7+1
                  ICH2=ICH1+6
                  IF (ICH2 > LEN(STRNG)) THEN
                    WRITE(LFNERR,'(/,A,3(/,17X,A),/)')                        &
                    ' *** SR WRT_ANT: The resulting antenna phase pattern '// &
                        'file will be incomplete ',                           &
                        'because of too many parameters are estimated.',      &
                        'Enlarge the length of the buffer line!',             &
                        '(parameter "lineLength1024" in M_BERN.f)'
                    CALL exitrc(2)
                  ENDIF
                  WRITE(STRNG(ICH1:ICH2),'(F7.2)') &
                        antbuf(iant)%sys(isys)%freq(1)%pat(ord,ielv,iazi)
                ENDDO
                WRITE(lfnloc,'(A1,I2.2,1X,I1,I4,A)')                      &
                      csys,antbuf(iant)%sys(isys)%freq(ifreq)%freq, &
                      ord,iord,TRIM(STRNG)
              ENDIF
!              IF (SIZE(antbuf(iant)%sys(isys)%freq(ifreq)%pat,3) == 2) EXIT
            ENDDO
          ENDDO
        ENDDO
      ENDDO
    ENDDO
  END SUBROUTINE wrt_ant
! =========================================================================
  SUBROUTINE rdphasfil(recant, satant, aname, anumb, sta, indx1, indx2, err)
! -------------------------------------------------------------------------
! Purpose :   Read phase center information file
!
! Author :    A. Gaede
!
! Created :   06-Jun-2007
!
! Changes :   06-May-2008 DT: Adapt for SLR processing
!             22-Feb-2010 RD: Call GTSENSOR only for satant
!             18-Sep-2012 RD: Correctly deallocate arrays
!
! Copyright : Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Used modules
    USE s_exitrc
    USE m_maxdim, ONLY: maxsaa, maxrec
    USE s_alcerr
    USE s_gtflna
    USE s_gtsensor
    USE s_opnfil
    USE s_opnerr
!
! In/Out:
    TYPE(t_phasfil), DIMENSION(:), POINTER, OPTIONAL :: recant ! Buffer for receiver antennas
    TYPE(t_phasfil), DIMENSION(:), POINTER, OPTIONAL :: satant ! Buffer for satellite antennas
! In:
    CHARACTER(LEN=20), DIMENSION(:), OPTIONAL        :: aname  ! list of antennas to be buffered
    INTEGER(i4b), DIMENSION(:), OPTIONAL             :: anumb  ! list of antenna numbers wrt aname
    CHARACTER(LEN=16), OPTIONAL                      :: sta    ! station name
    INTEGER(i4b), OPTIONAL                           :: err    ! do not abort if present
! Out:
    INTEGER(i4b), OPTIONAL, INTENT(OUT)              :: indx1  ! Index of last buffered receiver antenna
    INTEGER(i4b), OPTIONAL, INTENT(OUT)              :: indx2  ! Index of last buffered satellite antenna
! Local variables
    LOGICAL, SAVE                                    :: first = .true.
    INTEGER(i4b), DIMENSION(:), ALLOCATABLE          :: bufnum
    INTEGER(i4b), DIMENSION(:), ALLOCATABLE          :: isbu
    INTEGER(i4b), DIMENSION(:), ALLOCATABLE          :: individ
    INTEGER(i4b)                                     :: buf
    INTEGER(i4b)                                     :: ii
    INTEGER(i4b), SAVE                               :: iantr = 0
    INTEGER(i4b), SAVE                               :: iants = 0
    INTEGER(i4b)                                     :: allant
    INTEGER(i4b)                                     :: irc
    INTEGER(i4b)                                     :: iac
    INTEGER(i4b)                                     :: ilst
    INTEGER(i4b)                                     :: iostat
    INTEGER(i4b)                                     :: sav
    INTEGER(i4b), SAVE                               :: miss_slrsta
    INTEGER(i4b)                                     :: nant_chk
    CHARACTER(LEN=fileNameLength), SAVE              :: filphc
    CHARACTER(LEN=lineLength1024)                    :: STRNG1
    CHARACTER(LEN=80)                                :: title
    CHARACTER(LEN=60)                                :: filinfo
    CHARACTER(LEN=10)                                :: model = ''
    CHARACTER(LEN=10), SAVE                          :: pcvmod
    REAL(r8b)                                        :: vers = 0d0

! Open antenna phase center information file
    IF (first) THEN
      pcvmod = ''
      CALL gtflna(1,'PHASECC',filphc,irc)
      first=.false.
      miss_slrsta = 0
    ENDIF
    IF (PRESENT(satant) .AND. pcvmod == '') CALL gtsensor(pcvmod=pcvmod)
    CALL opnfil(lfnloc,filphc,'OLD','FORMATTED','READONLY',' ',iostat)
    CALL opnerr(lfnerr,lfnloc,iostat,filphc,'RDPHASFIL')

! Read title lines
    READ(lfnloc,"(A80,/)",iostat=irc)title
    IF (irc /= 0) THEN
      WRITE(lfnerr,"(/,' *** SR RDPHASFIL: Error while reading first', &
                                  &       ' lines in phase '           &
                                  & /,19X,'center information file'    &
                                  & /,19X,'Filename: ',A,/)")filphc
      CALL exitrc(2)
    ENDIF
! Read format version, antenna model and filinfo
    DO
      READ(lfnloc,"(A80)",iostat=irc)STRNG1
      IF (STRNG1 == '' .AND. vers == 0d0) THEN
        WRITE(lfnerr,"(/,' *** SR RDPHASFIL: Wrong file format. Can not', &
                     &   ' find the string FORMAT VERSION:', &
                     & /,19X,'in Bernese PCV file', &
                     & /,19X,'Filename: ',A,/)")filphc
        CALL exitrc(2)
      ELSEIF (STRNG1 == '' .AND. model == '') THEN
        WRITE(lfnerr,"(/,' *** SR RDPHASFIL: Wrong file format. Can not', &
                     &   ' find the string ANTENNA MODEL:', &
                     & /,19X,'in Bernese PCV file', &
                     & /,19X,'Filename: ',A,/)")filphc
        CALL exitrc(2)
      ELSEIF (STRNG1(1:15) == 'FORMAT VERSION:') THEN
        READ(STRNG1,"(15X,F6.2)")vers
        IF (vers /= version) THEN
          WRITE(lfnerr,"(/,' *** SR RDPHASFIL: Wrong format version', &
                       &   ' of Bernese PCV file.',                  &
                       & /,19X,'Read format version    : ',F6.2,     &
                       & /,19X,'Expected format version: ',F6.2,/)") &
                                                          vers,version
          CALL exitrc(2)
        ENDIF
      ELSEIF (STRNG1(1:15) == 'ANTENNA MODEL: ') THEN
        READ(STRNG1,"(15X,A10)")model
        IF (pcvmod /= '' .AND. model /= pcvmod) THEN
          WRITE(lfnerr,"(/,' *** SR RDPHASFIL: Inconsistent antenna models',&
                       & ' in Bernese PCV and satellite',                   &
                       & /,19X,'information file',                          &
                       & /,19X,'Model in Bernese PCV model         : ',A10, &
                       & /,19X,'Model in satellite information file: ',A10,/)") &
                                                          model,pcvmod
          CALL exitrc(2)
        ENDIF
      ELSEIF (STRNG1(1:16) == 'LAST INPUT FILE:') THEN
        READ(STRNG1,"(17X,A60)")filinfo
      ELSEIF (STRNG1 == '') THEN
        EXIT
      ENDIF
    ENDDO
    allant = 0
! Allocate "bufnum", "individ" and "isbu"
    IF (PRESENT(aname)) THEN
      ALLOCATE(bufnum(SIZE(aname)),stat=iac)
      CALL alcerr(iac, 'bufnum(SIZE(aname))', (/SIZE(aname)/), 'rdphasfil')
      ALLOCATE(individ(SIZE(aname)),stat=iac)
      CALL alcerr(iac, 'individ(SIZE(aname))', (/SIZE(aname)/), 'rdphasfil')
      ALLOCATE(isbu(SIZE(aname)),stat=iac)
      CALL alcerr(iac, 'isbu(SIZE(aname))', (/SIZE(aname)/), 'rdphasfil')
      isbu = 0
    ELSE
      ALLOCATE(bufnum(1),stat=iac)
      CALL alcerr(iac, 'bufnum(1)', (/1/), 'rdphasfil')
      ALLOCATE(individ(1),stat=iac)
      CALL alcerr(iac, 'individ(1)', (/1/), 'rdphasfil')
    ENDIF

! Set index for buffer
    IF (PRESENT(recant)) THEN
      IF (iantr == SIZE(recant)) iantr = 0
    ENDIF
    IF (PRESENT(satant)) THEN
      IF (iants == SIZE(satant)) iants = 0
    ENDIF
! Loop over all lines in PCV file
! -------------------------------
    DO
      READ(lfnloc,"(A)",iostat=irc) STRNG1
!     At the end of the file check whether all reqested antennas were found in PCV file
      IF (irc < 0) THEN
        IF (PRESENT(aname)) THEN

! No checks for SLR station
          nant_chk = SIZE(aname)
          DO ilst=1,SIZE(aname)
            IF ( aname(ilst)(1:20) == MTypeSLR ) nant_chk=nant_chk-1
          END DO

          IF ( allant < nant_chk )THEN

            DO ilst=1,SIZE(aname)

              IF(aname(ilst)(1:3) == 'MW ' .OR. &
                   aname(ilst)(1:5) == 'SLR R') THEN

                IF (PRESENT(sta)) THEN
                  CALL missing(satant,aname(ilst),anumb(ilst),miss_slrsta,sta)
                ELSE
                  CALL missing(satant,aname(ilst),anumb(ilst),miss_slrsta)
                ENDIF

              ELSE
                IF (PRESENT(sta)) THEN
                  CALL missing(recant,aname(ilst),anumb(ilst),miss_slrsta,sta)
                ELSE
                  CALL missing(recant,aname(ilst),anumb(ilst),miss_slrsta)
                ENDIF
              ENDIF
            ENDDO

            IF (miss_slrsta/=1) WRITE(lfnerr,"(19X,'Filename: ',A)")filphc

! No quit or only SLR station is missing
            IF ( PRESENT(err) .OR. miss_slrsta==1) THEN
              WRITE(lfnerr,*)
              err = 1
            ELSE
              WRITE(lfnerr,"(19X,'Processing stopped!',/)")
              CALL exitrc(2)
            ENDIF
          ENDIF
        ENDIF
        EXIT
      ENDIF
      IF (STRNG1(1:31) /= 'ANTENNA/RADOME TYPE  NUMBER SYS') CYCLE
! Start reading antenna section
! -----------------------------
      DO ii=1,2
        READ(lfnloc,"(A)",iostat=irc) STRNG1
        IF (irc /= 0) THEN
          WRITE(lfnerr,"(/,' *** SR RDPHASFIL: Error while reading ', &
                                   &     'pattern section in phase ', &
                                   & /,19X,'center information file', &
                                   & /,19X,'Filename: ',A,/)")filphc
          CALL exitrc(2)
        ENDIF
      ENDDO
!     Check whether antenna shall buffered,
      bufnum = 0
      individ = 0
      IF (PRESENT(aname)) THEN
!       exit main loop if all requested antennas are buffered
        IF (allant == SIZE(aname)) EXIT
        sav = 0
        CALL tobuffer(aname,anumb,STRNG1,sav,isbu,bufnum,individ)
!       cycle if antenna is not reqested or still buffered
        IF (sav == 0) CYCLE
        allant = allant + sav
      ELSE
        READ(STRNG1,"(21X,I6)") bufnum(1)
      ENDIF
      IF(STRNG1(1:3) == 'MW ' .OR. STRNG1(1:5) == 'SLR R') THEN
!       Too many satellite antennas
        iants = iants + 1
        IF (iants > maxsaa) THEN
          WRITE(LFNERR,"(/,' *** SR RDPHASFIL: Too many satellite '   &
                         &      ,'antennas',                          &
                         & /,19X,'Number of satellite antennas: ',I4, &
                         & /,19X,'Maximum number allowed: ',I4,       &
                         & /,19X,'Increase MAXSAA',/)")iants,maxsaa
          CALL exitrc(2)
        ENDIF
        buf = 2
      ELSE
!       Too many receiver antennas
        iantr = iantr + 1
        IF (iantr > maxrec) THEN
          WRITE(LFNERR,"(/,' *** SR RDPHASFIL: Too many receiver ', &
                 &       'antennas',                                &
                 & /,19X,'Number of receiver antennas: ',I4,        &
                 & /,19X,'Maximum number allowed: ',I4,             &
                 & /,19X,'Increase MAXREC',/)")iantr,maxrec
          CALL exitrc(2)
        ENDIF
        buf = 1
      ENDIF
! Buffer antenna(s)
      IF (PRESENT(satant) .AND. buf == 2) THEN
        CALL sav_ant(satant,iants,STRNG1,bufnum,individ,filphc)
        IF (PRESENT(indx2)) indx2 = iants   ! save index of last buffered satellite antenna
      ELSEIF (PRESENT(recant) .AND. buf == 1) THEN
        CALL sav_ant(recant,iantr,STRNG1,bufnum,individ,filphc)
        IF (PRESENT(indx1)) indx1 = iantr   ! save index of last buffered receiver antenna
      ENDIF
    ENDDO
    CLOSE (lfnloc)
    DEALLOCATE(bufnum)
    DEALLOCATE(individ)
    IF (PRESENT(aname)) DEALLOCATE(isbu)

  END SUBROUTINE rdphasfil
! =========================================================================
  SUBROUTINE missing(antbuf,antnam,antnum,miss_slrsta,sta)
! -------------------------------------------------------------------------
! Purpose :   Write missing antennas in lfnerr
!
! Author :    A. Gaede
!
! Created :   06-Jun-2007
!
! Changes :   06-May-2008 DT: Add error message from rdphasfil; adopt for SLR
!
! Copyright : Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! In:
    TYPE(t_phasfil), DIMENSION(:), POINTER :: antbuf
    CHARACTER(LEN=20)                      :: antnam
    INTEGER(i4b)                           :: antnum
    INTEGER(i4b)                           :: miss_slrsta
    CHARACTER(LEN=16), OPTIONAL            :: sta
! Local variables:
    INTEGER(i4b)                           :: iant

    LOGICAL, SAVE                          :: first_miss = .true.

! SLR station
    IF ( antnam == MTypeSLR ) THEN
       IF (miss_slrsta /= 2) miss_slrsta = 1

       RETURN
    ENDIF

    DO iant=1,SIZE(antbuf)
      IF (antbuf(iant)%name == antnam .AND.    &
           antbuf(iant)%numb == antnum) EXIT

      IF (iant == SIZE(antbuf)) THEN

        IF ( first_miss ) THEN
           WRITE(lfnerr,"(/,' *** SR MISSING: Following antenna(s) ', &
                            &       'not found in phase'              &
                            & /,19X,'center information file:')")
           first_miss = .FALSE.
        ENDIF

        IF (PRESENT(sta)) THEN
          WRITE(lfnerr,"(19X,A20,3X,I6,' at: ',A16)") &
               antnam,antnum,sta
        ELSE
          WRITE(lfnerr,"(19X,A20,3X,I6)")antnam,antnum
        ENDIF
        miss_slrsta = 2
      ENDIF
    ENDDO
  END SUBROUTINE missing
! =========================================================================
  SUBROUTINE tobuffer(aname,anumb,STRNG1,sav,isbu,bufnum,individ)
! -------------------------------------------------------------------------
! Purpose :   Check if antenna shall buffered
!
! Author :    A. Gaede
!
! Created :   06-Jun-2007
! Last mod.:  02-Nov-2010
!
! Changes :   07-May-2008 DT: Return if SLR station
!             02-Nov-2010 SL: use undef_i
!
! Copyright : Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! In:
    CHARACTER(LEN=20), DIMENSION(:)        :: aname
    INTEGER(i4b), DIMENSION(:)             :: anumb
    CHARACTER(LEN=lineLength1024)          :: STRNG1
! In/Out:
    INTEGER(i4b)                           :: sav
    INTEGER(i4b), DIMENSION(:)             :: isbu
    INTEGER(i4b), DIMENSION(:)             :: bufnum
    INTEGER(i4b), DIMENSION(:)             :: individ
! Local variables:
    INTEGER(i4b)                           :: ilst
    INTEGER(i4b)                           :: iant

    READ(STRNG1,"(21X,I6)")iant

! Nothing to do if SLR station
    IF ( STRNG1(1:20) == MTypeSLR ) RETURN

    DO ilst=1,SIZE(aname)
      IF (aname(ilst) == STRNG1(1:20)) THEN
        IF (anumb(ilst) == iant) THEN
          sav = sav + 1
          bufnum(sav) = anumb(ilst)
          isbu(ilst) = 1
          IF (iant /= 0 .AND. iant /= undef_i .AND. &
              STRNG1(1:3) /= 'MW ' .AND. STRNG1(1:5) /= 'SLR R') &
                                                        individ(sav) = 1
!       Check if iant == 0
        ELSEIF (iant == 0 .AND. isbu(ilst) /= 1) THEN
          sav = sav + 1
          bufnum(sav) = anumb(ilst)
          isbu(ilst) = 1
        ENDIF
      ENDIF
    ENDDO
  END SUBROUTINE tobuffer
! =========================================================================
  SUBROUTINE sav_ant(antbuf,iant,STRNG1,bufnum,individ,filphc)
! -------------------------------------------------------------------------
! Purpose :   Save offset values in buffer antbuf
!
! Author :    A. Gaede
!
! Created :   06-Jun-2007
!
! Changes :   07-May-2008 DT: Return if SLR station
!             25-Oct-2010 SL: integer to real conversion bug corrected
!             16-May-2011 HB/SL: String test for 'A/Z' modified,
!                                assignments for structure antbuf%...%freq
!                                modified
!             24-May-2012 MF: Save everything when generate a duplicate
!
! Copyright : Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Used modules
    USE s_exitrc
! In:
    TYPE(t_phasfil), DIMENSION(:), POINTER       :: antbuf
    INTEGER(i4b)                                 :: iant
    CHARACTER(LEN=lineLength1024)                :: STRNG1
    INTEGER(i4b), DIMENSION(:)                   :: bufnum
    INTEGER(i4b), DIMENSION(:)                   :: individ
    CHARACTER(LEN=fileNameLength)                :: filphc
! Local variables:
    CHARACTER(LEN=3),SAVE                        :: azStr
    CHARACTER(LEN=1)                             :: csys
    CHARACTER(LEN=5)                             :: sysfrq
    CHARACTER(LEN=16)                            :: frmt =  '(6X,I3,XX(F7.2))'
    INTEGER(i4b)                                 :: isys
    INTEGER(i4b)                                 :: nsys
    INTEGER(i4b)                                 :: npat
    INTEGER(i4b)                                 :: azii
    INTEGER(i4b)                                 :: nazi
    INTEGER(i4b)                                 :: iazi
    INTEGER(i4b)                                 :: nelv
    INTEGER(i4b)                                 :: ord
    INTEGER(i4b)                                 :: typ
    INTEGER(i4b)                                 :: jj
    INTEGER(i4b)                                 :: isav
    INTEGER(i4b)                                 :: irc
    INTEGER(i4b)                                 :: ifreq
    INTEGER(i4b)                                 :: frq
    INTEGER(i4b)                                 :: ii
    INTEGER(i4b)                                 :: nres
    REAL(r8b), DIMENSION(3)                      :: off
    REAL(r8b)                                    :: fac

    LOGICAL, SAVE                                :: sFirst = .TRUE.

! First call to set test string for A\Z
! -------------------------------------
    IF (sFirst) THEN
      WRITE(azStr,"('A',A1,'Z')")backslash
      sFirst = .FALSE.
    ENDIF

! Nothing to do if SLR station
    IF ( STRNG1(1:20) == MTypeSLR ) RETURN

! Check whether the antenna is already in the list
    DO ii = 1,iAnt-1
      IF (STRNG1(1:20) == antbuf(ii)%name) THEN
        DO jj = 1,SIZE(bufnum)
          IF (bufnum(jj) == 0) EXIT
          IF (bufnum(jj) == antbuf(ii)%numb) THEN
            WRITE(lfnerr,'(/,A,/,17X,A,/,17X,A,I6,/)')                   &
            ' *** SR SAV_ANT: More than one entry for an antenna found', &
            'Antenna name:   ' // antbuf(ii)%name,                     &
            'Antenna number: ',antbuf(ii)%numb
            CALL exitrc(2)
          ENDIF
        ENDDO
      ENDIF
    ENDDO

! Read antenna name and number
    READ(STRNG1,"(A20)",iostat=irc) antbuf(iant)%name
    antbuf(iant)%numb = bufnum(1)
    antbuf(iant)%individ = individ(1)

! Loop over allocation lines and allocate memory
    nsys = 0
    DO
      IF (nsys /= 0) THEN
        READ(lfnloc,"(A)",iostat=irc) STRNG1
        IF (irc /= 0) THEN
          WRITE(lfnerr,"(/,' *** SR SAV_ANT: Error while reading ',     &
                                     &  'allocation section in phase ', &
                                     & /,17X,'center information file', &
                                     & /,17X,'Filename: ',A,/)")filphc
          CALL exitrc(2)
        ENDIF
        IF (STRNG1 == '') EXIT
      ENDIF
      nsys = nsys + 1
      READ(STRNG1,"(28X,A1)") csys
      IF (csys == ' ') THEN
        isys = 0
      ELSE
        DO ii=0,maxsys
          IF (csys == g_svnsys(ii)) THEN
            isys = ii
            EXIT
          ELSEIF (ii == maxsys) THEN
            WRITE(lfnerr,"(/,' *** SR SAV_ANT: Unknown system in phase',&
                           &       ' center information file', &
                           & /,17X,'Antenna: ',A20,            &
                           & /,17X,'Number:  ',I6,/)")antbuf(iant)%name,antbuf(iant)%numb
            CALL exitrc(2)
          ENDIF
        ENDDO
      ENDIF
      READ(STRNG1,"(32X,I3,1X,I3,4(2X,I3),2X,A10,1X,A20,1X,A10,1X,A)") &
           antbuf(iant)%sys(isys)%nfreq,antbuf(iant)%sys(isys)%typ,    &
          (antbuf(iant)%sys(isys)%resolu(ii),ii=1,4),                  &
           antbuf(iant)%sys(isys)%sinex,antbuf(iant)%sys(isys)%method, &
           antbuf(iant)%sys(isys)%date,antbuf(iant)%sys(isys)%remark
      CALL alcfrq(isys,antbuf(iant)%sys(isys)%nfreq, &
                  antbuf(iant)%sys(isys)%typ,        &
                  antbuf(iant)%sys(isys)%resolu,antbuf,iant)
! Set freq(1) = 1 and freq(2) = 2 (GPS/GLONASS) resp. freq(2) = 7 (GALILEO)
! WEIL NUR 2 FREQUENZEN IN 5.x -> MUSS FüR 6.0 ANGEPASST WERDEN!!!
      antbuf(iant)%sys(isys)%freq(1)%freq = 1
      IF (isys < 2  .AND. antbuf(iant)%sys(isys)%nfreq > 1) antbuf(iant)%sys(isys)%freq(2)%freq = 2
      IF (isys == 2 .AND. antbuf(iant)%sys(isys)%nfreq > 1) antbuf(iant)%sys(isys)%freq(2)%freq = 7
      IF ((antbuf(iant)%name(1:9) == 'MW  GIOVE' .OR.     &
          antbuf(iant)%name(1:11) == 'MW  GALILEO') .AND. &
          antbuf(iant)%sys(isys)%nfreq > 1) antbuf(iant)%sys(isys)%freq(2)%freq = 7
    ENDDO
! Read information from offset section
! ------------------------------------
    Main: DO
      READ(lfnloc,"(A)",iostat=irc) STRNG1
      IF (irc /= 0) THEN
        WRITE(lfnerr,"(/,' *** SR SAV_ANT: Error while searching ', &
                               &  'for offset section in phase ',   &
                               & /,17X,'center information file',   &
                               & /,17X,'Antenna: ',A20,             &
                               & /,17X,'Filename: ',A,/)")antbuf(iant)%name,filphc
        CALL exitrc(2)
      ENDIF
      IF (STRNG1(1:40) /= "      NORTH MM  EAST MM   UP MM   FACTOR") CYCLE
      IF (STRNG1(1:40) == "ANTENNA/RADOME TYPE  NUMBER SYS FRQ TYP ") THEN
        WRITE(lfnerr,"(/,' *** SR SAV_ANT: Missing offset values for', &
                               & /,17X,'Antenna: ',A20,                &
                               & /,17X,'Filename: ',A,/)")antbuf(iant)%name,filphc
        CALL exitrc(2)
      ENDIF
      READ(lfnloc,"()")
      DO
        READ(lfnloc,"(A)",iostat=irc) STRNG1
        IF (irc /= 0) THEN
          WRITE(lfnerr,"(/,' *** SR SAV_ANT: Error while reading ', &
                               &  'offset lines in phase ',         &
                               & /,17X,'center information file',   &
                               & /,17X,'Antenna: ',A20,             &
                               & /,17X,'Filename: ',A,/)")antbuf(iant)%name,filphc
          CALL exitrc(2)
        ENDIF
        IF (STRNG1 == '') EXIT Main
        READ(STRNG1,"(A1,I2,1X,I1,3(1X,F8.2),2X,E10.3)")csys,frq,ord,off,fac
        IF (csys == ' ') THEN
          isys = 0
        ELSE
          DO ii=0,maxsys
            IF (csys == g_svnsys(ii)) THEN
              isys = ii
              EXIT
            ELSEIF (ii == maxsys) THEN
              WRITE(lfnerr,"(/,' *** SR SAV_ANT: Unknown system in phase', &
                           &       ' center information file', &
                           & /,17X,'Antenna: ',A20,            &
                           & /,17X,'Number:  ',I6,/)")antbuf(iant)%name,antbuf(iant)%numb
              CALL exitrc(2)
            ENDIF
          ENDDO
        ENDIF
        DO ifreq=1,antbuf(iant)%sys(isys)%nfreq
          IF (antbuf(iant)%sys(isys)%freq(ifreq)%freq == 0 .OR. &
            antbuf(iant)%sys(isys)%freq(ifreq)%freq == frq) EXIT
          IF (ifreq == antbuf(iant)%sys(isys)%nfreq) THEN
            WRITE(lfnerr,"(/,' *** SR SAV_ANT: Frequency array too small', &
                       & /,17X,'Antenna name:   ',A20,                     &
                       & /,17X,'Antenna number: ',I6,                      &
                       & /,17X,'Satellite system: ',A1,/)")                &
                              antbuf(iant)%name,antbuf(iant)%numb,csys
            CALL exitrc(2)
          ENDIF
        ENDDO
        antbuf(iant)%sys(isys)%freq(ifreq)%freq = frq
        antbuf(iant)%sys(isys)%freq(ifreq)%off(ord,:) = off
        antbuf(iant)%sys(isys)%freq(ifreq)%fac(ord) = fac
      ENDDO
    ENDDO Main
! Read information from pattern section
! -------------------------------------
    typ = 0
    npat = 0
    DO isys=0,nsys-1
      IF (antbuf(iant)%sys(isys)%typ /= 0) typ = typ + 1
    ENDDO
    IF (typ == 0) RETURN
    DO
      READ(lfnloc,"(A)",iostat=irc) STRNG1
      IF (irc /= 0) THEN
        WRITE(lfnerr,"(/,' *** SR SAV_ANT: Error while searching ', &
                                &  'for pattern section in phase ', &
                                & /,17X,'center information file',  &
                                & /,17X,'Antenna: ',A20,            &
                                & /,17X,'Filename: ',A,/)")antbuf(iant)%name,filphc
        CALL exitrc(2)
      ENDIF
      IF (STRNG1 == '' .OR. STRNG1(1:40) == "ANTENNA/RADOME TYPE  NUMBER SYS FRQ TYP ") THEN
        IF (npat == 0) THEN
          WRITE(lfnerr,"(/,' *** SR SAV_ANT: Missing pattern values for', &
                               & /,17X,'Antenna: ',A20, &
                               & /,17X,'Filename: ',A,/)")antbuf(iant)%name,filphc
          CALL exitrc(2)
        ELSE
          EXIT
        ENDIF
      ENDIF

! Test if strng1(8:10) /= A\Z
      IF (STRNG1(8:10) /= azStr) CYCLE
      npat = npat + 1
! Read satellite system, frequency and order
      READ(STRNG1,"(A1,I2,1X,I1)",iostat=irc) csys,frq,ord
      IF (irc /= 0) THEN
        WRITE(lfnerr,"(/,' ### SR SAV_ANT: Error while decoding ',     &
                               &            'system and frequency:',/, &
                               & /,A,//,17X,'Processing stopped!',/)")STRNG1
        CALL exitrc(2)
      ENDIF
!      isys = searchsys(csys,antbuf,iant)
      IF (csys == ' ') THEN
        isys = 0
      ELSE
        DO ii=0,maxsys
          IF (csys == g_svnsys(ii)) THEN
            isys = ii
            EXIT
          ELSEIF (ii == maxsys) THEN
            WRITE(lfnerr,"(/,' *** SR SAV_ANT: Unknown system in phase',&
                           &       ' center information file', &
                           & /,17X,'Antenna: ',A20,            &
                           & /,17X,'Number:  ',I6,/,           &
                           & /,17X,'System:  ',A1,/)")         &
                           antbuf(iant)%name,antbuf(iant)%numb,csys
            CALL exitrc(2)
          ENDIF
        ENDDO
      ENDIF
      DO ifreq=1,antbuf(iant)%sys(isys)%nfreq
        IF (antbuf(iant)%sys(isys)%freq(ifreq)%freq == frq) EXIT
        IF (ifreq == antbuf(iant)%sys(isys)%nfreq) THEN
          WRITE(lfnerr,"(/,' *** SR SAV_ANT: Frequency not found', &
               &       ' in pattern section of buffer',            &
               & /,17X,'Antenna name:   ',A20,                     &
               & /,17X,'Antenna number: ',I6,                      &
               & /,17X,'Satellite system: ',A1,/)")                &
               antbuf(iant)%name,antbuf(iant)%numb,csys
          CALL exitrc(2)
        ENDIF
      ENDDO

      IF (antbuf(iant)%sys(isys)%typ == 1) THEN
        nazi = 360 / antbuf(iant)%sys(isys)%resolu(3) + 1
        IF (antbuf(iant)%sys(isys)%resolu(3) == 360) nazi = 1
        nelv = antbuf(iant)%sys(isys)%resolu(4) / &
             antbuf(iant)%sys(isys)%resolu(2) + 1
        IF (antbuf(iant)%sys(isys)%resolu(2) == antbuf(iant)%sys(isys)%resolu(4)) nelv = 1
      ELSE
        nelv = antbuf(iant)%sys(isys)%resolu(2)
        nazi = 2*antbuf(iant)%sys(isys)%resolu(3)+1
      ENDIF
      WRITE(frmt(8:9),"(I2.2)")nelv

! Read frequency section
      DO iazi=1,nazi
        READ(lfnloc,"(A)",iostat=irc)STRNG1
        IF (irc /= 0) THEN
          WRITE(lfnerr,"(/,' *** SR SAV_ANT: Error while reading ',   &
                               &       'pattern section in phase ',   &
                               & /,17X,'center information file for', &
                               & /,17X,'Antenna: ',A20,               &
                               & /,17X,'Number: ',I6,/)")             &
                               antbuf(iant)%name,antbuf(iant)%numb
          CALL exitrc(2)
        ENDIF
        WRITE(sysfrq,"(A1,I2.2,1X,I1)")csys,frq,ord
        IF (STRNG1(1:5) /= sysfrq .OR. STRNG1(8:10) == 'A\Z') THEN
          WRITE(lfnerr,"(/,' *** SR SAV_ANT: Inconsistent pattern ',   &
                                &       'section in phase center ',    &
                                & /,17X,'information file',            &
                                & /,17X,'Antenna: ',A20,               &
                                & /,17X,'Expected frequency: ',A3,     &
                                & /,17X,'Found frequency:    ',A3,/)") &
                                   antbuf(iant)%name,sysfrq,STRNG1(1:3)
          CALL exitrc(2)
        ELSE
          READ(STRNG1,frmt,iostat=irc) azii, &
               (antbuf(iant)%sys(isys)%freq(ifreq)%pat(ord,jj,iazi),jj=1,nelv)
          IF (irc /= 0) THEN
            WRITE(lfnerr,"(/,' *** SR SAV_ANT: Error reading pattern',  &
                                    &       ' values in phase center ', &
                                    & /,17X,'information file',         &
                                    & /,17X,'Antenna: ',A20,            &
                                    & /,17X,'Frequency: ',A3,/)")       &
                                                   antbuf(iant)%name,sysfrq
            CALL exitrc(2)
          ENDIF
        ENDIF
      ENDDO
    ENDDO

! Save all remaining antennas from bufnum
    DO isav=2,SIZE(bufnum)
      IF (bufnum(isav) == 0) EXIT
      DO isys=0,nsys-1
        CALL alcfrq(isys,antbuf(iant)%sys(isys)%nfreq, &
             antbuf(iant)%sys(isys)%typ, &
             antbuf(iant)%sys(isys)%resolu,antbuf,iant+1)
        DO ifreq=1,antbuf(iant)%sys(isys)%nfreq
          antbuf(iant+1)%sys(isys)%freq(ifreq)%off(:,:)   = antbuf(iant)%sys(isys)%freq(ifreq)%off(:,:)
          antbuf(iant+1)%sys(isys)%freq(ifreq)%pat(:,:,:) = antbuf(iant)%sys(isys)%freq(ifreq)%pat(:,:,:)
          antbuf(iant+1)%sys(isys)%freq(ifreq)%fac(:)     = antbuf(iant)%sys(isys)%freq(ifreq)%fac(:)
        ENDDO
        antbuf(iant+1)%sys(isys)%nfreq    = antbuf(iant)%sys(isys)%nfreq
        antbuf(iant+1)%sys(isys)%sinex    = antbuf(iant)%sys(isys)%sinex
        antbuf(iant+1)%sys(isys)%method   = antbuf(iant)%sys(isys)%method
        antbuf(iant+1)%sys(isys)%date     = antbuf(iant)%sys(isys)%date
        antbuf(iant+1)%sys(isys)%remark   = antbuf(iant)%sys(isys)%remark
        antbuf(iant+1)%sys(isys)%typ      = antbuf(iant)%sys(isys)%typ
        antbuf(iant+1)%sys(isys)%resolu(:)= antbuf(iant)%sys(isys)%resolu(:)

! MF: Duplicate also the calibration values
        DO ifreq = 1,antbuf(iant)%sys(isys)%nfreq
          antbuf(iant+1)%sys(isys)%freq(ifreq)%freq = &
            antbuf(iant)%sys(isys)%freq(ifreq)%freq

          nres = antbuf(iant)%sys(isys)%resolu(1)
          antbuf(iant+1)%sys(isys)%freq(ifreq)%fac(0:nres) = &
            antbuf(iant)%sys(isys)%freq(ifreq)%fac(0:nres)
          antbuf(iant+1)%sys(isys)%freq(ifreq)%off(0:nres,1:3) = &
            antbuf(iant)%sys(isys)%freq(ifreq)%off(0:nres,1:3)

          IF (antbuf(iant)%sys(isys)%typ /= 0) THEN
            nelv = SIZE(antbuf(iant)%sys(isys)%freq(ifreq)%pat(0,:,1))
            nazi = SIZE(antbuf(iant)%sys(isys)%freq(ifreq)%pat(0,1,:))
            antbuf(iant+1)%sys(isys)%freq(ifreq)%pat(0:nres,1:nelv,1:nazi) = &
              antbuf(iant)%sys(isys)%freq(ifreq)%pat(0:nres,1:nelv,1:nazi)
          END IF
        END DO
      ENDDO
      antbuf(iant+1)%name             = antbuf(iant)%name
      antbuf(iant+1)%numb             = bufnum(isav)
      antbuf(iant+1)%individ          = individ(isav)
      iant = iant + 1
    ENDDO
  END SUBROUTINE sav_ant

! -------------------------------------------------------------------------
  SUBROUTINE search_off(antbuf,prn,antnam,antnum,sta,rfrq,sessid,offset)
! -------------------------------------------------------------------------
! Purpose :   Find offset in buffered array antbuf
!
! Author :    A. Gaede
!
! Created :   06-Jun-2007
! Last mod.:  30-Nov-2010
!
! Changes :   07-May-2008 DT: Return if SLR station
!             30-Nov-2010 SS/SL: format in error message corrected
!
! Copyright : Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Used modules
    USE s_ddreh
    USE s_dmlmav
    USE s_exitrc
    USE s_getazi

! In:
    TYPE(t_phasfil), DIMENSION(:), POINTER :: antbuf
    INTEGER(i4b)                           :: prn
    CHARACTER(LEN=20)                      :: antnam
    INTEGER(i4b)                           :: antnum
    CHARACTER(LEN=16)                      :: sta
    INTEGER(i4b)                           :: rfrq
    CHARACTER(LEN=4), OPTIONAL             :: sessid
! Out:
    REAL(r8b), DIMENSION(3)                :: offset
! Local variables
    CHARACTER(LEN=20), DIMENSION(1)        :: aname
    INTEGER(i4b), DIMENSION(1)             :: anumb
    INTEGER(i4b)                           :: ord
    INTEGER(i4b)                           :: ii
    INTEGER(i4b)                           :: isys
    INTEGER(i4b)                           :: iant
    INTEGER(i4b)                           :: iantm
    INTEGER(i4b)                           :: ifreq
    INTEGER(i4b)                           :: nfrq
    REAL(r8b)                              :: antazi
    REAL(r8b)                              :: ordoff
    REAL(r8b), DIMENSION(3,3)              :: rotmat
    LOGICAL                                :: isPrn

    INCLUDE 'COMFREQ.inc'

    isys  = INT(prn/100)
    isPrn = (prn/=isys*100)
! Values for satellite antennas are buffered in isys=0
    IF (antnam(1:3) == 'MW ' .OR. antnam(1:5) == 'SLR R') isys = 0
! Take always GPS offsets for receiver antennas
!    isys = 0 ! decomment this line if necessary

    iantm = 0
    DO iant=1,SIZE(antbuf)
      IF (antnam == antbuf(iant)%name .AND. &
          antnum == antbuf(iant)%numb) THEN
        iantm=iant
        EXIT
      ELSEIF(iant == SIZE(antbuf)) THEN
        iantm=iant
        aname(1) = antnam
        anumb(1) = antnum
        IF(antnam(1:3) == 'MW ' .OR. antnam(1:5) == 'SLR R') THEN
          sta(1:16) = 'Satellite       '
          CALL rdphasfil(satant=antbuf,aname=aname,anumb=anumb,sta=sta,indx2=iantm)

! SLR station
        ELSEIF ( antnam(1:20) == MTypeSLR ) THEN
          offset(:) = 0d0
          RETURN

        ELSE
          CALL rdphasfil(recant=antbuf,aname=aname,anumb=anumb,sta=sta,indx1=iantm)
        ENDIF
      ENDIF
    ENDDO
    IF (iantm /= 0) iant = iantm

! Stop if no values for the requested satellite system
    IF (antbuf(iant)%sys(isys)%nfreq == 0) THEN
      WRITE(lfnerr,'(/,A,2(/,20X,A),/,20X,A,I8,/)')                       &
      ' *** SR SEARCH_OFF: No offset values found for ',                  &
                          'satellite system ' // g_svnsys(isys) // ' of', &
                          'Antenna: ' // TRIM(antbuf(iant)%name),         &
                          'Number:  ',antbuf(iant)%numb
      CALL exitrc(2)
    ENDIF
! Phase center offset for frequency "rfrq" FACLIN MUSS ENTSPRECHEND 6.0 ANGEPASST WERDEN
    IF(rfrq < 6 .AND..NOT.(rfrq == 5 .AND. isys == 2)) THEN
      DO ii=1,3
        DO ord=0,antbuf(iant)%sys(isys)%resolu(1)
          IF (isPrn) THEN
            ordoff = faclin(rfrq,1,prn)*antbuf(iant)%sys(isys)%freq(1)%off(ord,ii) * &
                                        antbuf(iant)%sys(isys)%freq(1)%fac(ord)
            IF (antbuf(iant)%sys(isys)%nfreq == 2) ordoff = ordoff + &
                     faclin(rfrq,2,prn)*antbuf(iant)%sys(isys)%freq(2)%off(ord,ii) * &
                                        antbuf(iant)%sys(isys)%freq(2)%fac(ord)
!            ordoff = faclin(rfrq,1,1)*antbuf(iant)%sys(isys)%freq(1)%off(ord,ii) * &
!                                        antbuf(iant)%sys(isys)%freq(1)%fac(ord) +    &
!                     faclin(rfrq,2,1)*antbuf(iant)%sys(isys)%freq(2)%off(ord,ii) * &
!                                        antbuf(iant)%sys(isys)%freq(2)%fac(ord)
          ELSE
            IF (rfrq == 1 .OR. rfrq == 2) THEN
              ordoff = antbuf(iant)%sys(isys)%freq(rfrq)%off(ord,ii) * &
                       antbuf(iant)%sys(isys)%freq(rfrq)%fac(ord)
            ELSE IF (rfrq == 3 .OR. rfrq == 5) THEN
              nFrq   = antbuf(iant)%sys(isys)%nfreq
              ordoff = antbuf(iant)%sys(isys)%freq(1)%off(ord,ii) * &
                       antbuf(iant)%sys(isys)%freq(1)%fac(ord) + &
                       antbuf(iant)%sys(isys)%freq(nfrq)%off(ord,ii) * &
                       antbuf(iant)%sys(isys)%freq(nfrq)%fac(ord)
              ordoff = ordoff / 2d0
            ELSE IF (rfrq == 4) THEN
              nFrq   = antbuf(iant)%sys(isys)%nfreq
              ordoff = antbuf(iant)%sys(isys)%freq(1)%off(ord,ii) * &
                       antbuf(iant)%sys(isys)%freq(1)%fac(ord) - &
                       antbuf(iant)%sys(isys)%freq(nfrq)%off(ord,ii) * &
                       antbuf(iant)%sys(isys)%freq(nfrq)%fac(ord)
            ENDIF
          ENDIF
          offset(ii) = offset(ii) + ordoff / 1000d0
        ENDDO
      ENDDO
    ELSEIF(rfrq < 9) THEN
      DO ifreq=1,antbuf(iant)%sys(isys)%nfreq
        IF (antbuf(iant)%sys(isys)%freq(ifreq)%freq == rfrq) THEN
          DO ii=1,3
            DO ord=0,antbuf(iant)%sys(isys)%resolu(1)
              ordoff = antbuf(iant)%sys(isys)%freq(ifreq)%off(ord,ii) * &
                       antbuf(iant)%sys(isys)%freq(ifreq)%fac(ord)
              offset(ii) = offset(ii) + ordoff / 1000d0
            ENDDO
          ENDDO
          EXIT
        ENDIF
        IF (ifreq == antbuf(iant)%sys(isys)%nfreq) THEN
          WRITE(lfnerr,"(/,' ### SR SEARCH_OFF: No offset values for ', &
                       &       'requested frequency', &
                       & /,20X,'Antenna: ',A20,       &
                       & /,20X,'Number:  ',I6,        &
                       & /,20X,'Frequency: ',I2,        &
                       & /,20X,'Zero values used!!!',/)")antnam,antnum,rfrq
          RETURN
        ENDIF
      ENDDO
    ELSE
      WRITE(lfnerr,'(/,A,/,20X,A,I2,/,20X,A,A1,/)')     &
            ' ### SR SEARCH_OFF: Unknown frequency',    &
                                'Frequency: ',rfrq,     &
                                'System:    ',g_svnsys(isys)
      CALL exitrc(2)
    ENDIF
!
! Correct for antenna orientation (if not oriented to the north)
    IF (PRESENT(sessid)) THEN
      CALL getazi(' ',antnam,antnum,SESSID,antazi)
      IF (antazi /= 0.d0) THEN
        offset(2)=-offset(2)
        CALL ddreh(3,antazi,rotmat)
        CALL dmlmav(offset,rotmat,offset)
        offset(2)=-offset(2)
      ENDIF
    ENDIF
  END SUBROUTINE search_off
! =========================================================================
  SUBROUTINE search_pcv(antbuf,prn,antnam,antnum,zen,azi,sta,rfrq,sessid,corr)
! -------------------------------------------------------------------------
! Purpose :   Find pattern correction in buffered array antbuf
!
! Author :    A. Gaede
!
! Created :   06-Jun-2007
!
! Changes :   07-May-2008 DT: Return if SLR station
!             31-Jul-2008 DT: Set isPrn=.False. if SLR satellite
!
! Copyright : Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Used modules
    USE d_const, ONLY: pi
    USE s_exitrc
    USE s_alcerr
    USE s_getazi
    USE f_aslef2
    USE f_aslefu
! In:
    TYPE(t_phasfil), DIMENSION(:), POINTER :: antbuf
    INTEGER(i4b)                           :: prn
    CHARACTER(LEN=20)                      :: antnam
    INTEGER(i4b)                           :: antnum
    REAL(r8b)                              :: zen
    REAL(r8b)                              :: azi
    CHARACTER(LEN=16)                      :: sta
    INTEGER(i4b)                           :: rfrq
    CHARACTER(LEN=4), OPTIONAL             :: sessid
! Out:
    REAL(r8b)                              :: corr
! Local variables
    CHARACTER(LEN=20), DIMENSION(1)        :: aname
    INTEGER(i4b), DIMENSION(1)             :: anumb
    INTEGER(i4b)                           :: NELV
    INTEGER(i4b)                           :: DELV
    INTEGER(i4b)                           :: IELV
    INTEGER(i4b)                           :: IEL1
    INTEGER(i4b)                           :: IEL2
    INTEGER(i4b)                           :: EL1
    INTEGER(i4b)                           :: NAZI
    INTEGER(i4b)                           :: DAZI
    INTEGER(i4b)                           :: IAZI
    INTEGER(i4b)                           :: IAZ1
    INTEGER(i4b)                           :: IAZ2
    INTEGER(i4b)                           :: AZ1
    INTEGER(i4b)                           :: ifreq
    INTEGER(i4b)                           :: ord
    INTEGER(i4b)                           :: iac
    INTEGER(i4b)                           :: iant
    INTEGER(i4b)                           :: iantm
    INTEGER(i4b)                           :: isys
    INTEGER(i4b)                           :: IMM
    INTEGER(i4b)                           :: NMM
    REAL(r8b)                              :: ordcor
    REAL(r8b)                              :: antazi
    REAL(r8b)                              :: zendeg
    REAL(r8b)                              :: azideg
    REAL(r8b)                              :: COREL1
    REAL(r8b)                              :: COREL2
    REAL(r8b)                              :: AZIOK
    REAL(r8b), DIMENSION(:,:), ALLOCATABLE :: AZELCR
    LOGICAL                                :: isPrn

    INCLUDE 'COMFREQ.inc'

    isys  = INT(prn/100)
    isPrn = (prn/=isys*100)
! Values for satellite antennas are buffered in isys=0
    IF (antnam(1:3) == 'MW ' .OR. antnam(1:5) == 'SLR R') isys = 0

! No linear combination for SLR satellites
    IF ( antnam(1:5) == 'SLR R' ) isPrn = .FALSE.

! Take always GPS offsets for receiver antennas
!    isys = 0 ! decomment this line if necessary

    iantm = 0
    DO iant=1,SIZE(antbuf)
      IF (antnam == antbuf(iant)%name .AND. &
          antnum == antbuf(iant)%numb) THEN
        iantm=iant
        EXIT

! If antenna not found buffer from file
      ELSEIF(iant == SIZE(antbuf)) THEN
        iantm=iant
        aname(1) = antnam
        anumb(1) = antnum

        IF(antnam(1:3) == 'MW ' .OR. antnam(1:5) == 'SLR R') THEN
          sta(1:16) = 'Satellite       '
          CALL rdphasfil(satant=antbuf,aname=aname,anumb=anumb,sta=sta,indx2=iantm)

! SLR station
        ELSEIF ( antnam(1:20) == MTypeSLR ) THEN
          corr = 0d0
          RETURN

        ELSE
          CALL rdphasfil(recant=antbuf,aname=aname,anumb=anumb,sta=sta,indx1=iantm)
        ENDIF
      ENDIF
    ENDDO
    IF (iantm /= 0) iant = iantm

! Stop if no values for the requested satellite system
    IF (antbuf(iant)%sys(isys)%nfreq == 0) THEN
      WRITE(lfnerr,"(/,' *** SR SEARCH_PCV: No pattern values found for ', &
                   &   'satellite system ',A1,' of',                      &
                   & /,20X,'Antenna: ',A20,                               &
                   & /,20X,'Number:  ',I6,/)")g_svnsys(isys),             &
                                     antbuf(iant)%name, antbuf(iant)%numb
      CALL exitrc(2)
    ENDIF

    IF (PRESENT(sessid)) THEN
      CALL getazi(' ',antnam,antnum,sessid,antazi)
    ELSE
      antazi = 0d0
    ENDIF
!
    IF (antbuf(iant)%sys(isys)%typ == 0) THEN
      corr = 0d0
      RETURN
    ENDIF

    NELV = SIZE(antbuf(iant)%sys(isys)%freq(1)%pat,2)
    NAZI = SIZE(antbuf(iant)%sys(isys)%freq(1)%pat,3)
    ALLOCATE(AZELCR(antbuf(iant)%sys(isys)%nfreq,0:antbuf(iant)%sys(isys)%resolu(1)),stat=iac)
    CALL alcerr(iac, 'AZELCR(antbuf(iant)%sys(isys)%nfreq)',&
                     (/antbuf(iant)%sys(isys)%nfreq,antbuf(iant)%sys(isys)%resolu(1)+1/), 'search_pcv')

!   Elevation/azimuth grid
!   ----------------------
    IF (antbuf(iant)%sys(isys)%typ == 1) THEN
      zendeg = zen * 180.d0 / pi
      azideg = (azi - antazi) * 180.d0 / pi
      IF (zendeg > DBLE(antbuf(iant)%sys(isys)%resolu(4))+1D-10) THEN
        WRITE(lfnerr,"(/,' *** SR SEARCH_PCV: Zenith angle ',       &
              & 'exceeds maximum angle for antenna phase center',/, &
              & 20X,'variations contained in antenna ',             &
              &     'phase center information file.',/,             &
              & 20X,'Station name      : ',A,/,                     &
              & 20X,'Antenna name      : ',A,/,                     &
              & 20X,'Max angle allowed : ',F8.4,/,                  &
              & 20X,'Actual angle      : ',F8.4,/,                  &
              & 20X,'Increase minimal elevation or ',               &
              & 'add antenna phase center variation values ',/,     &
              & 20X,'to antenna phase center information file!!',/)") &
            sta,antbuf(iant)%name,DBLE(antbuf(iant)%sys(isys)%resolu(4)),zendeg
        CALL exitrc(2)
      ENDIF

      IF (zendeg <  0.0) zendeg =  0.0
      DO
        IF (azideg < 360.0) EXIT
        azideg = azideg - 360.0
      ENDDO
      DO
        IF (azideg >= 0.0) EXIT
        azideg = azideg + 360.0
      ENDDO
      IF (azideg == 360.0) azideg = 0.0
!
      DELV = antbuf(iant)%sys(isys)%resolu(2)
      DAZI = antbuf(iant)%sys(isys)%resolu(3)
!
      IEL1 = INT(zendeg/DELV) + 1
      IEL2 = IEL1 + 1
      IF (IEL2 > NELV) IEL2 =  NELV
      EL1  = (IEL1-1) * DELV
!
      IAZ1 = INT(azideg/DAZI) + 1
      IAZ2 = IAZ1 + 1
      IF (IAZ2 > NAZI) IAZ2 = NAZI
      AZ1  = (IAZ1-1) * DAZI
!
!     Interpolate between table values
      DO ifreq=1,antbuf(iant)%sys(isys)%nfreq
        DO ord=0,antbuf(iant)%sys(isys)%resolu(1)
          COREL1 = antbuf(iant)%sys(isys)%freq(ifreq)%pat(ord,IEL1,IAZ1) +  &
                   ((zendeg-EL1)/DELV) *                                &
                   (antbuf(iant)%sys(isys)%freq(ifreq)%pat(ord,IEL2,IAZ1) - &
                   antbuf(iant)%sys(isys)%freq(ifreq)%pat(ord,IEL1,IAZ1))
          COREL1 = COREL1 * antbuf(iant)%sys(isys)%freq(ifreq)%fac(ord)
          COREL2 = antbuf(iant)%sys(isys)%freq(ifreq)%pat(ord,IEL1,IAZ2) +  &
                   ((zendeg-EL1)/DELV) *                                &
                   (antbuf(iant)%sys(isys)%freq(ifreq)%pat(ord,IEL2,IAZ2) - &
                   antbuf(iant)%sys(isys)%freq(ifreq)%pat(ord,IEL1,IAZ2))
          COREL2 = COREL2 * antbuf(iant)%sys(isys)%freq(ifreq)%fac(ord)
          AZELCR(ifreq,ord) = COREL1 + ((azideg-AZ1)/DAZI) * (COREL2-COREL1)
        END DO
      END DO
!
!   Spherical harmonics
!   -------------------
    ELSE IF (antbuf(iant)%sys(isys)%typ >= 2 .AND. &
             antbuf(iant)%sys(isys)%typ <= 4) THEN
      AZIOK=azi-antazi
!
      DO ifreq=1,antbuf(iant)%sys(isys)%nfreq
        AZELCR(ifreq,:) = 0.D0
        DO IELV=1,NELV
          NMM=(NAZI-1)/2
          DO IAZI=1,NAZI
            IMM=IAZI-(NAZI+1)/2
            IF (IABS(IMM) > IELV) CYCLE
            DO ord=0,antbuf(iant)%sys(isys)%resolu(1)
              IF (antbuf(iant)%sys(isys)%typ == 2) THEN
                AZELCR(ifreq,ord)=AZELCR(ifreq,ord) +                                 &
                           antbuf(iant)%sys(isys)%freq(ifreq)%pat(ord,IELV,IAZI) *    &
                           antbuf(iant)%sys(isys)%freq(ifreq)%fac(ord) *              &
                           ASLEFU(2*ZEN,AZIOK,-IELV,IMM,0)
              ELSEIF (antbuf(iant)%sys(isys)%typ == 3) THEN
                AZELCR(ifreq,ord)=AZELCR(ifreq,ord) +                                 &
                           antbuf(iant)%sys(isys)%freq(ifreq)%pat(ord,IELV,IAZI) *    &
                           antbuf(iant)%sys(isys)%freq(ifreq)%fac(ord) *              &
                           ASLEF2(2*ZEN,AZIOK,-IELV,IMM,NELV,NMM)
              ELSE
                AZELCR(ifreq,ord)=AZELCR(ifreq,ord) +                                 &
                           antbuf(iant)%sys(isys)%freq(ifreq)%pat(ord,IELV,IAZI) *    &
                           antbuf(iant)%sys(isys)%freq(ifreq)%fac(ord) *              &
                           ASLEF2(ZEN,AZIOK,-IELV,IMM,NELV,NMM)
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
!
    ELSE
      WRITE(lfnerr,"(/,' *** SR SEARCH_PCV: Invalid PCV-model type ',&
                                  &       'found',                   &
                                  & /,20X,'PCV-model type:',I2,      &
                                  & /,20X,'Antenna type  : ',A,/)")  &
                           antbuf(iant)%sys(isys)%typ,antbuf(iant)%name
      CALL exitrc(2)
    ENDIF
!
! PCV correction for frequency "rfrq" FACLIN MUSS ENTSPRECHEND 6.0 ANGEPASST WERDEN
    IF(rfrq < 6 .AND..NOT.(rfrq == 5 .AND. isys == 2)) THEN
      DO ord=0,antbuf(iant)%sys(isys)%resolu(1)
        IF (isPrn) THEN
          ordcor = faclin(rfrq,1,prn)*AZELCR(1,ord) + faclin(rfrq,2,prn)*AZELCR(2,ord)
!          ordcor = faclin(rfrq,1,1)*AZELCR(1,ord) +  faclin(rfrq,2,1)*AZELCR(2,ord)
        ELSE
          IF (rfrq == 1 .OR. rfrq == 2) THEN
            ordcor = AZELCR(rfrq,ord)
          ELSE IF (rfrq == 3 .OR. rfrq == 5) THEN
            ordcor = AZELCR(1,ord)+AZELCR(antbuf(iant)%sys(isys)%nfreq,ord)
            ordcor = ordcor / 2d0
          ELSE IF (rfrq == 4) THEN
            ordcor = AZELCR(1,ord)-AZELCR(antbuf(iant)%sys(isys)%nfreq,ord)
          ENDIF
        ENDIF
        corr = corr + ordcor / 1000d0
      ENDDO
    ELSEIF(rfrq < 9) THEN
      DO ifreq=1,antbuf(iant)%sys(isys)%nfreq
        IF (antbuf(iant)%sys(isys)%freq(ifreq)%freq == rfrq) THEN
          DO ord=0,antbuf(iant)%sys(isys)%resolu(1)
            ordcor = AZELCR(ifreq,ord) * antbuf(iant)%sys(isys)%freq(ifreq)%fac(ord)
            corr = corr + ordcor / 1000d0
          ENDDO
          EXIT
        ENDIF
        IF (ifreq == antbuf(iant)%sys(isys)%nfreq) THEN
          WRITE(lfnerr,"(/,' ### SR SEARCH_PCV: No PCV values for ', &
                       &       'requested frequency', &
                       & /,20X,'Antenna: ',A20,       &
                       & /,20X,'Number:  ',I6,        &
                       & /,20X,'Frequency: ',I2,        &
                       & /,20X,'Zero values used!!!',/)")antnam,antnum,rfrq
          RETURN
        ENDIF
      ENDDO
    ELSE
      WRITE(lfnerr,'(/,A,/,20X,A,I2,/,20X,A,A1,/)')     &
            ' ### SR SEARCH_PCV: Unknown frequency',    &
                                'Frequency: ',rfrq,     &
                                'System:    ',g_svnsys(isys)
      CALL exitrc(2)
    ENDIF
    DEALLOCATE(AZELCR)

  END SUBROUTINE search_pcv
! =========================================================================

! =========================================================================
  SUBROUTINE updmodel(antUpd,antbuf,filphc)
! -------------------------------------------------------------------------
! Purpose :   Update estimated parameters (off+pcv) with the a priori model
!
! Author :    R. Dach
!
! Created :   15-Mar-2008
! Last mod.:  23-Dec-2010
!
! Changes :   23-Dec-2010 RD: Write GLONASS-only PCV result files
!
! Copyright : Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Used modules
    USE d_const,  ONLY: PI
    USE m_global, ONLY: g_strsys3
    USE s_alcerr
! In/out:
    TYPE(t_phasfil), DIMENSION(:), POINTER :: antUpd ! model t obe updated
! In:
    TYPE(t_phasfil), DIMENSION(:), POINTER :: antBuf ! a priori model
    CHARACTER(LEN=fileNameLength)          :: filphc ! Output filename

! Local variables
    CHARACTER(LEN=8), PARAMETER            :: srName = 'updModel'

    INTEGER(i4b)                           :: iAnt,jAnt
    INTEGER(i4b)                           :: iSys
    INTEGER(i4b)                           :: iFrq
    INTEGER(i4b)                           :: iCal
    INTEGER(i4b)                           :: iOrd
    INTEGER(i4b)                           :: iZen,nZen,mZen,dZen
    INTEGER(i4b)                           :: iAzi,nAzi,     dAzi
    INTEGER(i4b)                           :: prn
    INTEGER(i4b)                           :: iac

    REAL(r8b), DIMENSION(:,:,:), POINTER   :: hlp
    REAL(r8b)                              :: corAnt, corUpd
    REAL(r8b)                              :: zen
    REAL(r8b)                              :: dZen1, dZen2
    REAL(r8b)                              :: azi
    REAL(r8b)                              :: dAzi1, dAzi2

    NULLIFY(hlp)

! Update the estmated corrections to a full model
! -----------------------------------------------
    DO jAnt = 1,SIZE(antUpd)
      IF (LEN_TRIM(antUpd(jAnt)%name) == 0) EXIT

      CALL antInfo(antUpd(jAnt)%name,antUpd(jAnt)%numb,0,index=iAnt)
      DO iSys=0,SIZE(antUpd(jAnt)%sys)-1

        ! Check whether the system is available
        IF (iSys > SIZE(antBuf(iAnt)%sys)-1) THEN
          antUpd(jAnt)%sys(iSys)%nFreq = 0
          CYCLE
        ENDIF
        IF (antUpd(jAnt)%sys(iSys)%method(1:10) /= 'estimated ') THEN
          antUpd(jAnt)%sys(iSys)%nFreq = 0
          CYCLE
        ENDIF

        antUpd(jAnt)%sys(iSys)%nFreq = antBuf(iAnt)%sys(iSys)%nFreq
        IF (antBuf(iAnt)%sys(iSys)%nFreq == 0) CYCLE

! Update the offsets
! ------------------
        ! Loop the frequencies
        DO iFrq = 1,antBuf(iAnt)%sys(iSys)%nFreq
          iOrd = 0
          IF (.NOT. ASSOCIATED(antUpd(jAnt)%sys(iSys)%freq(iFrq)%off)) THEN
            ALLOCATE(antUpd(jAnt)%sys(iSys)%freq(iFrq)%off(0:iOrd,3),stat=iac)
            CALL alcerr(iac,'antUpd%sys%freq%off',(/iOrd+1,3/),srName)
            antUpd(jAnt)%sys(iSys)%freq(iFrq)%off = 0d0
          ENDIF

          IF (.NOT. ASSOCIATED(antUpd(jAnt)%sys(iSys)%freq(iFrq)%fac)) THEN
            ALLOCATE(antUpd(jAnt)%sys(iSys)%freq(iFrq)%fac(0:iOrd),stat=iac)
            CALL alcerr(iac,'antUpd%sys%freq%fac',(/iOrd+1/),srName)
            antUpd(jAnt)%sys(iSys)%freq(iFrq)%fac = 1d0
          ENDIF

          ! Update the offsets per frequency
          antUpd(jAnt)%sys(iSys)%freq(iFrq)%freq = antBuf(iAnt)%sys(iSys)%freq(iFrq)%freq
          antUpd(jAnt)%sys(iSys)%freq(iFrq)%fac  = antBuf(iAnt)%sys(iSys)%freq(iFrq)%fac
          DO iCal = 1,3
            IF (antUpd(jAnt)%name(1:3) == 'MW ' .OR. &   ! no offsets
                antUpd(jAnt)%name(1:5) == 'SLR R') EXIT  ! for satellites
            antUpd(jAnt)%sys(iSys)%freq(iFrq)%off(iOrd,iCal) =        &
                   antBuf(iAnt)%sys(iSys)%freq(iFrq)%off(iOrd,iCal) + &
                   antUpd(jAnt)%sys(iSys)%freq(iFrq)%off(iOrd,iCal)/  &
                   antBuf(iAnt)%sys(iSys)%freq(iFrq)%fac(iOrd)
          ENDDO
        ENDDO

! No pattern in the a priori file
! -------------------------------
        IF (antBuf(iAnt)%sys(iSys)%typ == 0) CYCLE

! only offsets are estimated, keep the a priori pattern
! -----------------------------------------------------
        IF (antUpd(jAnt)%sys(iSys)%typ == 0 .AND. &
            antBuf(iAnt)%sys(iSys)%typ /= 0) THEN

          antUpd(jAnt)%sys(iSys)%typ       = antBuf(iAnt)%sys(iSys)%typ
          antUpd(jAnt)%sys(iSys)%resolu(:) = antBuf(iAnt)%sys(iSys)%resolu(:)
          antUpd(jAnt)%sys(iSys)%nFreq     = antBuf(iAnt)%sys(iSys)%nFreq

          DO iFrq = 1,antBuf(iAnt)%sys(iSys)%nFreq
            iOrd = SIZE(antBuf(iAnt)%sys(iSys)%freq(iFrq)%pat,1)-1
            nZen = SIZE(antBuf(iAnt)%sys(iSys)%freq(iFrq)%pat,2)
            nAzi = SIZE(antBuf(iAnt)%sys(iSys)%freq(iFrq)%pat,3)

            IF (.NOT. ASSOCIATED(antUpd(jAnt)%sys(iSys)%freq(iFrq)%pat)) THEN
              ALLOCATE(antUpd(jAnt)%sys(iSys)%freq(iFrq)%pat(0:iOrd,nZen,nAzi),stat=iac)
              CALL alcerr(iac,'antUpd%sys%freq%pat',(/iOrd+1,nZen,nAzi/),srName)
              antUpd(jAnt)%sys(iSys)%freq(iFrq)%pat = 0d0
            ENDIF
            antUpd(jAnt)%sys(iSys)%freq(iFrq)%pat(:,:,:) = &
                antBuf(iAnt)%sys(iSys)%freq(iFrq)%pat(:,:,:)
          ENDDO

! a priori and estimate: grid
! ---------------------------
        ELSE IF (antUpd(jAnt)%sys(iSys)%typ == 1 .AND. &
                 antBuf(iAnt)%sys(iSys)%typ == 1) THEN

          ! Estimated resolution is higher than a priori
          dzen1 = antUpd(jAnt)%sys(iSys)%resolu(2) / &
                  antBuf(iAnt)%sys(iSys)%resolu(2)

          dzen2 = antBuf(iAnt)%sys(iSys)%resolu(2) / &
                  antUpd(jAnt)%sys(iSys)%resolu(2)

          dazi1 = antUpd(jAnt)%sys(iSys)%resolu(3) / &
                  antBuf(iAnt)%sys(iSys)%resolu(3)

          dazi2 = antBuf(iAnt)%sys(iSys)%resolu(3) / &
                  antUpd(jAnt)%sys(iSys)%resolu(3)

          ! Check for comptibility
          IF ((dzen1 - INT(dzen1) > 1d-6 .AND.  dzen2 - INT(dzen2) > 1d-6) .OR. &
              (dazi1 - INT(dazi1) > 1d-6 .AND.  dazi2 - INT(dazi2) > 1d-6)) THEN
            WRITE(lfnerr,'(/,A,/,4(18X,A,/),18X,A,I8,A,/,2(18X,A,I5,A,I3,/))') &
              ' ### SR updmodel: Resolution of the a priori and the ' //       &
                              'estimated PCV ',                                &
              'model is not compatible. Estimated model is not ',              &
              'written into the result file',                                  &
              'File name:             ' // TRIM(filphc),                       &
              'Antenna name:          ' // TRIM(antUpd(jAnt)%name),            &
              'Antenna number/system: ',                                       &
              antUpd(jAnt)%numb,'  (' // g_strsys3(iSys) // ')',               &
              'D(zen) inp/est:        ',antBuf(iAnt)%sys(iSys)%resolu(2),      &
              '  /  ',antUpd(jAnt)%sys(iSys)%resolu(2),                        &
              'D(azi) inp/est:        ',antBuf(iAnt)%sys(iSys)%resolu(3),      &
              '  /  ',antUpd(jAnt)%sys(iSys)%resolu(3)

          ELSE ! Models are compatible

            ! Compute the resolution of the updated model
            iOrd = MAX(antBuf(iAnt)%sys(iSys)%resolu(1), &
                       antUpd(jAnt)%sys(iSys)%resolu(1))
            dZen = MAX(antBuf(iAnt)%sys(iSys)%resolu(2), &
                       antUpd(jAnt)%sys(iSys)%resolu(2))
            dAzi = MAX(antBuf(iAnt)%sys(iSys)%resolu(3), &
                       antUpd(jAnt)%sys(iSys)%resolu(3))
            mZen = MAX(antBuf(iAnt)%sys(iSys)%resolu(4), &
                       antUpd(jAnt)%sys(iSys)%resolu(4))

            nZen = mZen/dZen+1
            nAzi =  360/dAzi+1

            antUpd(jAnt)%sys(iSys)%resolu(1) = iOrd
            antUpd(jAnt)%sys(iSys)%resolu(2) = dZen
            antUpd(jAnt)%sys(iSys)%resolu(3) = dAzi
            antUpd(jAnt)%sys(iSys)%resolu(4) = mZen

            ALLOCATE(hlp(0:iOrd,nZen,nAzi),stat=iac)
            CALL alcerr(iac,'hlp',(/iOrd,nZen,nAzi/),srName)

            ! Compute the updated model for each frequency
            prn = iSys*100
            DO iFrq = 1,antUpd(jAnt)%sys(iSys)%nFreq
              hlp = 0d0
              DO iZen = 1,nZen
                DO iAzi = 1,nAzi
                  zen = (iZen-1)*dZen/180d0*PI
                  azi = (iAzi-1)*dAzi/180d0*PI

                  corAnt = 0d0
                  CALL search_pcv(antBuf,prn,antBuf(iAnt)%name,                &
                                  antBuf(iAnt)%numb,zen,azi,'(estimated)     ',&
                                  antUpd(jAnt)%sys(iSys)%freq(iFrq)%freq,      &
                                  corr=corAnt)

                  corUpd = 0d0
                  CALL search_pcv(antUpd,prn,antUpd(jAnt)%name,                &
                                  antUpd(jAnt)%numb,zen,azi,'(estimated)     ',&
                                  antUpd(jAnt)%sys(iSys)%freq(iFrq)%freq,      &
                                  corr=corUpd)

                  hlp(iOrd,iZen,iAzi) = (corAnt + corUpd)*1000D0
                ENDDO
              ENDDO
              DEALLOCATE(antUpd(jAnt)%sys(iSys)%freq(iFrq)%pat,stat=iac)

              ALLOCATE(antUpd(jAnt)%sys(iSys)%freq(iFrq)%pat(0:iOrd,nZen,nAzi),stat=iac)
              CALL alcerr(iac,'antUpd%sys%freq%pat',(/iOrd,nZen,nAzi/),srName)

              antUpd(jAnt)%sys(iSys)%freq(iFrq)%pat = hlp
            ENDDO

            DEALLOCATE(hlp,stat=iac)
          ENDIF ! End of compatibility check

! a priori and estimate: SH
! -------------------------
        ELSE IF ((antUpd(jAnt)%sys(iSys)%typ == 2 .AND. &
                  antBuf(iAnt)%sys(iSys)%typ == 2) .OR. &
                 (antUpd(jAnt)%sys(iSys)%typ == 3 .AND. &
                  antBuf(iAnt)%sys(iSys)%typ == 3) .OR. &
                 (antUpd(jAnt)%sys(iSys)%typ == 4 .AND. &
                  antBuf(iAnt)%sys(iSys)%typ == 4)) THEN

          ! Check for comptibility
          IF (antUpd(jAnt)%sys(iSys)%resolu(1) /= &
              antBuf(iAnt)%sys(iSys)%resolu(1) .OR. &
              antUpd(jAnt)%sys(iSys)%resolu(2) /= &
              antBuf(iAnt)%sys(iSys)%resolu(2) .OR. &
              antUpd(jAnt)%sys(iSys)%resolu(3) /= &
              antBuf(iAnt)%sys(iSys)%resolu(3) .OR. &
              antUpd(jAnt)%sys(iSys)%resolu(4) /= &
              antBuf(iAnt)%sys(iSys)%resolu(4)) THEN
            WRITE(lfnerr,'(/,A,/,4(18X,A,/),18X,A,I8,A,/,2(18X,A,I5,A,I3,/))') &
              ' ### SR updModel: Resolution of the a priori and the ' //       &
                              'estimated PCV ',                                &
              'model is not compatible. Estimated model is not ',              &
              'written into the result file',                                  &
              'File name:             ' // TRIM(filphc),                       &
              'Antenna name:          ' // TRIM(antUpd(jAnt)%name),            &
              'Antenna number/system: ',                                       &
              antUpd(jAnt)%numb,'  (' // g_strsys3(iSys) // ')',               &
              'D(zen) inp/est:        ',antBuf(iAnt)%sys(iSys)%resolu(2),      &
              '  /  ',antUpd(jAnt)%sys(iSys)%resolu(2),                        &
              'D(azi) inp/est:        ',antBuf(iAnt)%sys(iSys)%resolu(3),      &
              '  /  ',antUpd(jAnt)%sys(iSys)%resolu(3)

          ELSE ! Models are compatible

            ! Compute the updated model for each frequency
            iOrd = antBuf(iAnt)%sys(iSys)%resolu(1)
            DO iFrq = 1,antUpd(jAnt)%sys(iSys)%nFreq
              DO iZen = 1,SIZE(antUpd(jAnt)%sys(iSys)%freq(iFrq)%pat,2)
                DO iAzi = 1,SIZE(antUpd(jAnt)%sys(iSys)%freq(iFrq)%pat,3)
                  antUpd(jAnt)%sys(iSys)%freq(iFrq)%pat(iOrd,iZen,iAzi) = &
                  antUpd(jAnt)%sys(iSys)%freq(iFrq)%pat(iOrd,iZen,iAzi) + &
                  antBuf(iAnt)%sys(iSys)%freq(iFrq)%pat(iOrd,iZen,iAzi)
                ENDDO
              ENDDO
            ENDDO
          ENDIF ! End of compatibility check

! a priori: sh and estimate: grid
! -------------------------------
        ELSE IF (antUpd(jAnt)%sys(iSys)%typ == 1 .AND. &
                 antBuf(iAnt)%sys(iSys)%typ >= 2 .AND. &
                 antBuf(iAnt)%sys(iSys)%typ <= 4) THEN

          ! Compute the resolution of the updated model
          iOrd = antUpd(jAnt)%sys(iSys)%resolu(1)
          dZen = antUpd(jAnt)%sys(iSys)%resolu(2)
          dAzi = antUpd(jAnt)%sys(iSys)%resolu(3)
          mZen = antUpd(jAnt)%sys(iSys)%resolu(4)

          nZen =  90/dZen+1
          nAzi = 360/dAzi+1

          ! Compute the updated model for each frequency
          prn = iSys*100
          DO iFrq = 1,antUpd(jAnt)%sys(iSys)%nFreq
            DO iZen = 1,nZen
              DO iAzi = 1,nAzi
                zen = (iZen-1)*dZen/180d0*PI
                azi = (iAzi-1)*dAzi/180d0*PI

                corAnt = 0d0
                CALL search_pcv(antBuf,prn,antBuf(iAnt)%name,                &
                                antBuf(iAnt)%numb,zen,azi,'(estimated)     ',&
                                antUpd(jAnt)%sys(iSys)%freq(iFrq)%freq,      &
                                corr=corAnt)

                antUpd(jAnt)%sys(iSys)%freq(iFrq)%pat(iOrd,iZen,iAzi) = &
                antUpd(jAnt)%sys(iSys)%freq(iFrq)%pat(iOrd,iZen,iAzi) + &
                corAnt*1000D0
              ENDDO
            ENDDO
          ENDDO


! A priori model cannot be updated to the currently selected model
! for estimation
! ----------------------------------------------------------------
        ELSE IF (antUpd(jAnt)%sys(iSys)%typ /= 0 .OR. &
                 antBuf(iAnt)%sys(iSys)%typ /= 0) THEN
          WRITE(lfnerr,'(/,A,/,3(18X,A,/),18X,A,I8,A,/)')                  &
            ' ### SR updModel: Types of the a priori and the ' //          &
                              'estimated PCV ',                            &
              'model do not allow to write a result file.',                &
              'File name:             ' // TRIM(filphc),                   &
              'Antenna name:          ' // TRIM(antUpd(jAnt)%name),        &
              'Antenna number/system: ',                                   &
              antUpd(jAnt)%numb,'  (' // g_strsys3(iSys) // ')'
        ENDIF
      ENDDO
    ENDDO
  END SUBROUTINE updmodel
! =========================================================================

END MODULE d_phaecc
