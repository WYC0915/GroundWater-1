MODULE s_FODIWEVL
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE fodiwevl(opt,datFil)

! -------------------------------------------------------------------------
! Purpose:    Write the information datFil%evlOut into the EVL-file.
!
! Author:     Luca Ostini
!
! Created:    14-Aug-2008
!
! Changes:    14-Aug-2008 LO: Created this file
!             02-Oct-2008 LO: First revision
!             09-Oct-2008 LO: Third revision
!             05-Dec-2008 LO: Fourth revisio: velocity changes allowed
!             13-Jan-2009 RD: Use '' as EOF-identifier in NEXTLINE
!             11-Feb-2009 LO: Fifth revision: major changes
!             03-Jul-2009 SL: write statement corrected (empty lines removed)
!             25-Sep-2009 LO: Changes for F90 consistency
!             02-Mar-2010 LO: Major changes do to elimination of FODISUBR
!             16-Jun-2010 LO: Time series diveded by component
!             19-Jul-2010 SL: tab characters removed
!             26-Aug-2010 LO: Architectural changes
!             03-Jan-2010 LO: INTENT(INOUT) removed and bug fixed
!             19-Sep-2012 RD: Use P_FODITS with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Used Modules
! ------------
! structures, variables:
  USE m_bern,    ONLY: i4b, r8b, lfn001, lfnErr, shortLineLength
  USE d_const,   ONLY: pi
  USE p_fodits,  ONLY: t_opt, t_datfil

! operator, methods:
!  USE d_debug,   ONLY: debug_entry, debug_exit

! subroutines, functions:
  USE s_opnerr
  USE s_opnfil
  USE s_timst2

! no implicit
  IMPLICIT NONE

! subroutine name
  CHARACTER(LEN=8), PARAMETER    :: srN = 'fodiwevl'


! List of Arguments
! -----------------
! input:
  TYPE(t_opt)                    :: opt        ! Option structure
  TYPE(t_datFil)                 :: datFil     ! I/O files structure

! input/output:

! output:


! Local Types
! -----------


! Local Parameters
! ----------------


! Local Variables
! ---------------
  CHARACTER(LEN=20)              :: datstr
  CHARACTER(LEN=20)              :: datstr2
  CHARACTER(LEN=9)               :: period
  CHARACTER(LEN=1)               :: signif

  INTEGER(i4b)                   :: ios
  INTEGER(i4b)                   :: iSta
  INTEGER(i4b)                   :: iEvnt
  INTEGER(i4b)                   :: iCrd

  REAL(r8b),DIMENSION(3)         :: par

! Call debug routine
! ------------------
!  CALL debug_entry(srName)


! Initialization of all variables
! -------------------------------
  ios = 0

  ! Return if the output EVL file is not present.
  IF( LEN_TRIM(opt%outEvlFile) == 0 )THEN
     RETURN
  END IF

  ! Open EVL file
  CALL opnfil(lfn001,opt%outEvlFile,'UNKNOWN','FORMATTED',' ',' ',ios)
  CALL opnerr(lfnerr,lfn001,ios,opt%outEvlFile,srN)

  ! Write the file header
  WRITE(lfn001,'(A,/,A)') &
       '# FODITS - TIME SERIES EVENT LIST FILE', &
       '# ------------------------------------'
  ! Write the title
  WRITE(lfn001,'(A,A,/,"# ",87("-"))') &
       '# Title: ', opt%title

  ! Write description
  WRITE(lfn001,'(A,10(/,A))') &
  '# Description:', &
  '# ------------', &
  '# Flg :   - TST, tested by FODITS, removed if non-significant.', &
  '#         - SET, tested by FODITS, but always significant.', &
  '#         - NOT, interval of no event.', &
  '#         - EST, estimated by FODITS.', &
  '# Evnt:   - DISC, discontinuity (use "Form Epoch").', &
  '#         - VELO, velocity change (use "From Epoch").', &
  '#         - OUTL, outlier (use "From Epoch" for one outlier, use both "From&
                    & Epoch" and "To Epoch" for a sequence).', &
  '#         - PERI, functional period ("From Epoch" and "To Epoch" are &
               &optional, "Period" must be defined).', &
  '# S:      - Y significant, N non-significant.'

! Additional Flags and Events to create synthetic time series:
!       - TSS SYNT, define the interval and sampling rate of the time series.
!       - GSN SYNT, Guass noise (use Epochs and Amplitudes in RMS).
!       - GAP SYNT, define a gap of data.
!       - VC1 SYNT, additional variance-covariance information
!                   (Period=factor, N:corNU, E:corEU, U:corNE).

  ! Write the residuals of stuct sCore%evlOut
  DO iSta = 1,datFil%evlOut%nSta

     DO iEvnt = 1,datFil%evlOut%sta(iSta)%nEvnt

        IF( iEvnt == 1 )THEN
           ! Write the column descriptions
           WRITE(lfn001,'(A,A,/,A,A,/,A,A,/,A,A)') &
  '-------------------------------------------------------------------', &
  '------------------------------------------------', &
  '-EVL                       From Epoch           To Epoch           ', &
  '  Period      Amplitude [m]             Remark  ', &
  '-STATION NAME    Flg Evnt  YYYY-MM-DD HH:MM:SS  YYYY-MM-DD HH:MM:SS', &
  '  DDDD.DDDD   North   East    Up      S RRRRRRRR', &
  '-------------------------------------------------------------------', &
  '------------------------------------------------'
        END IF

        datstr = ''
        IF(  datFil%evlOut%sta(iSta)%evnt(iEvnt)%timint%t(1) > 1.0D04 .AND. &
             datFil%evlOut%sta(iSta)%evnt(iEvnt)%timint%t(1) < 1.0D20 )THEN
           CALL timst2(1,1,datFil%evlOut%sta(iSta)%evnt(iEvnt)%timint%t(1),&
                datstr)
        END IF

        datstr2 = ''
        IF(  datFil%evlOut%sta(iSta)%evnt(iEvnt)%timint%t(2) > 1.0D04 .AND. &
             datFil%evlOut%sta(iSta)%evnt(iEvnt)%timint%t(2) < 1.0D20 )THEN
           CALL timst2(1,1,datFil%evlOut%sta(iSta)%evnt(iEvnt)%timint%t(2),&
                datstr2)
        END IF

        period = ''
        IF( datFil%evlOut%sta(iSta)%evnt(iEvnt)%omega /= 0.0D0 )THEN
           WRITE(period,'(F9.4)') &
                2*pi/datFil%evlOut%sta(iSta)%evnt(iEvnt)%omega
        END IF

        signif = 'N'
        IF( datFil%evlOut%sta(iSta)%evnt(iEvnt)%siTst /= 0 )THEN
           signif = 'Y'
        END IF

        par(:) = datFil%evlOut%sta(iSta)%evnt(iEvnt)%par(:)
        DO iCrd = 1,3
           IF( par(iCrd) >=  10.0D0 ) par(iCrd) =  9.9999D0
           IF( par(iCrd) <= -10.0D0 ) par(iCrd) = -9.9999D0
        END DO

        WRITE(lfn001,'(A16,1X,A3,1X,A4,2X,A20,1X,A20,1X,A9,2X,&
             &(3(F7.4,1X)),1X,A1,1X,A8)') &
             datFil%evlOut%sta(iSta)%name, &
             datFil%evlOut%sta(iSta)%evnt(iEvnt)%flag, &
             datFil%evlOut%sta(iSta)%evnt(iEvnt)%type, &
             datstr, datstr2, period, &
             par(:), &
             signif, &
             datFil%evlOut%sta(iSta)%evnt(iEvnt)%remark
     END DO

  END DO

  ! Close EVL file
  CLOSE (lfn001)

! End of subroutine
! -----------------
!  CALL debug_exit(srName)
  RETURN

END SUBROUTINE fodiwevl

END MODULE s_FODIWEVL
