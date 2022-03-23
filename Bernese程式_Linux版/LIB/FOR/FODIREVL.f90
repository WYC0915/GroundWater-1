MODULE s_FODIREVL
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE fodirevl(opt,datFil)

! -------------------------------------------------------------------------
! Purpose:    Read the event file EVL
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
!             25-Sep-2009 LO: Changes for F90 consistency
!             21-Dec-2009 LO: FFT removed and several changes apported
!             02-Mar-2010 LO: Major changes do to elimination of FODISUBR
!             11-Mar-2010 SL: staNameLength removed from USE m_maxdim
!             07-Apr-2010 LO: Major changes do to algorithm and output file
!             16-Jun-2010 LO: Time series diveded by component
!             26-Aug-2010 LO: Architectural changes
!             21-Sep-2010 RD: ST2TIM can be used as a module now
!             03-Jan-2010 LO: INTENT(INOUT) removed and bug fixed
!             19-Sep-2012 RD: Use P_FODITS with ONLY, remove unused variables
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Used Modules
! ------------
! structures, variables:
  USE m_bern,    ONLY: i4b, r8b, lfn001, lfnErr, lineLength, staNameLength, &
                       shortLineLength
  USE d_const,   ONLY: pi
  USE p_fodits,  ONLY: t_opt, t_datfil, &
                       nonsignificant, significant

! operator, methods:
!  USE d_debug,   ONLY: debug_entry, debug_exit

! subroutines, functions:
  USE f_nextline
  USE s_alcerr
  USE s_opnerr
  USE s_opnfil
  USE s_st2tim
  USE s_upperc

! no implicit
  IMPLICIT NONE

! subroutine name
  CHARACTER(LEN=shortLineLength), PARAMETER    :: srName = 'fodirevl'


! List of Arguments
! -----------------
! input:
  TYPE(t_opt)                    :: opt        ! Panel option structure
  TYPE(t_datFil)                 :: datFil     ! I/O files structure

! input/output:

! output:


! Local Types
! -----------


! Local Parameters
! ----------------

! Local Variables
! ---------------
  CHARACTER(LEN=lineLength)      :: line
  CHARACTER(LEN=lineLength),     &
    DIMENSION(:),ALLOCATABLE     :: lineBuffer
  CHARACTER(LEN=lineLength)      :: lineTmp
  CHARACTER(LEN=staNameLength)   :: staNameTmp
  CHARACTER(LEN=staNameLength)   :: prevStaName
  CHARACTER(LEN=staNameLength),  &
    DIMENSION(:), ALLOCATABLE    :: staNam
  CHARACTER(LEN=20)              :: datstrB
  CHARACTER(LEN=20)              :: datstrE
  CHARACTER(LEN=9)               :: period
  CHARACTER(LEN=7),DIMENSION(3)  :: ampliTxt
  CHARACTER(LEN=1)               :: signifTxt

  INTEGER(i4b)                   :: ios,iac
  INTEGER(i4b)                   :: iSta
  INTEGER(i4b)                   :: iEvnt
  INTEGER(i4b)                   :: nSta
  INTEGER(i4b), &
    DIMENSION(:), ALLOCATABLE    :: nEvnt
  INTEGER(i4b)                   :: nLine
  INTEGER(i4b)                   :: iLine
  INTEGER(i4b)                   :: jLine
  INTEGER(i4b)                   :: kLine
  INTEGER(i4b)                   :: lineLen

  REAL(r8b),DIMENSION(3)         :: ampliVal

! Call debug routine
! ------------------
!  CALL debug_entry(srName)


! Initialization of all variables
! -------------------------------
  datFil%evlIn%nSta = 0
  ios = 0

  ! Return if the EVL file is not present
  IF( LEN_TRIM(opt%inEvlFile) == 0 )THEN
!     CALL debug_exit(srName)
     RETURN
  END IF

  ! Open EVL file
  CALL opnfil(lfn001,opt%inEvlFile,'UNKNOWN','FORMATTED','READONLY',' ',ios)
  CALL opnerr(lfnerr,lfn001,ios,opt%inEvlFile,srName)

  ! Determine the number of lines of information
  nLine = 0
  DO iLine = 1,999999
     line = nextline(lfn001,0)
     IF( line == '' )EXIT
     IF( line(1:1) == '#' )CYCLE
     IF( line(1:1) == '-' )CYCLE
     IF( LEN_TRIM( line ) < 20 )CYCLE
     nLine = nLine + 1
  END DO

  ! Allocation of memory
  ALLOCATE(nEvnt(nLine),stat=iac)
  CALL alcerr(iac,'nEvnt',(/nLine/),srName)
  ALLOCATE(staNam(nLine),stat=iac)
  CALL alcerr(iac,'staNam', (/nLine/),srName)
  ALLOCATE(lineBuffer(nLine),stat=iac)
  CALL alcerr(iac,'lineBuffer', (/nLine/),srName)

  ! Rewind
  REWIND(lfn001)

  ! Read the Buffer
  nLine = 0
  DO iLine = 1,99999
     line = nextline(lfn001,0)
     IF( line == '' )EXIT
     IF( line(1:1) == '#' )CYCLE
     IF( line(1:1) == '-' )CYCLE
     IF( LEN_TRIM( line ) < 20 )CYCLE
     nLine = nLine + 1
     lineBuffer(nLine) = line
  END DO

  ! Close EVL file
  CLOSE(lfn001)

  ! Sort the stations alphabetically
  DO iLine = 1,nLine-1
     kLine = iLine
     DO jLine = iLine+1,nLine
        IF( lineBuffer(jLine) < lineBuffer(kLine) )THEN
           kLine = jLine
        END IF
     END DO
     IF( kLine /= iLine )THEN
        lineTmp = lineBuffer(iLine)
        lineBuffer(iLine) = lineBuffer(kLine)
        lineBuffer(kLine) = lineTmp
     END IF
  END DO

  ! Determine 'station name' and number of entries for each station
  nSta = 0
  nEvnt(:) = 0
  prevStaName = ''
  ReadDataLoop: DO iLine = 1,nLine
     line = lineBuffer(iLine)

     staNameTmp = line(1:staNameLength)
     IF( staNameTmp /= prevStaName )THEN
        nSta = nSta + 1
        nEvnt(nSta) = 1
        staNam(nSta) = staNameTmp
        prevStaName = staNameTmp
     ELSE
        nEvnt(nSta) = nEvnt(nSta) + 1
     END IF
  END DO ReadDataLoop

  ! Allocation of memory
  datFil%evlIn%nSta = nSta
  ALLOCATE(datFil%evlIn%sta(nSta),stat=iac)
  CALL alcerr(iac,'datFil%evlIn%sta', (/nSta/),srName)
  DO iSta = 1,nSta
     datFil%evlIn%sta(iSta)%nEvnt =  nEvnt(iSta)
     ALLOCATE(datFil%evlIn%sta(iSta)%evnt(nEvnt(iSta)),stat=iac)
     CALL alcerr(iac,'datFil%evlIn%sta(iSta)%evnt', (/nEvnt(iSta)/),srName)
  END DO

  ! Read line
  iLine = 0
  DO iSta = 1,datFil%evlIn%nSta
     datFil%evlIn%sta(iSta)%name = staNam(iSta)
     DO iEvnt = 1,datFil%evlIn%sta(iSta)%nEvnt

        ! Initialization
        datstrB = '1900-01-01 00:00:00'
        datstrE = '2199-01-01 23:59:59'
        period = ''

        ! Read the line
        iLine = iLine + 1
        line = lineBuffer(iLine)

        ! Line length
        lineLen = LEN_TRIM( line )

        ! Read name, flg, type
        IF( lineLen < 25 )CYCLE
        READ(line,'(A16,1X,A3,1X,A4)') &
             datFil%evlIn%sta(iSta)%name, &
             datFil%evlIn%sta(iSta)%evnt(iEvnt)%flag, &
             datFil%evlIn%sta(iSta)%evnt(iEvnt)%type

        ! Make %flag and %type uppercase
        CALL upperc( datFil%evlIn%sta(iSta)%evnt(iEvnt)%flag )
        CALL upperc( datFil%evlIn%sta(iSta)%evnt(iEvnt)%type )

        ! Read parameters of the line
        IF( lineLen >  45 ) READ(line,'(27X,A20)') datstrB
        IF( lineLen >  66 ) READ(line,'(48X,A20)') datstrE
        IF( lineLen >  77 ) READ(line,'(69X,A9)')  period
        IF( lineLen > 102 ) READ(line,'(79X,3(A8))') ampliTxt(:)

        ! Read datastr
        IF( LEN_TRIM(datstrB) /= 0 )THEN
           CALL st2tim(1, 1, datstrB(1:19), &
                datFil%evlIn%sta(iSta)%evnt(iEvnt)%timint%t(1) )
        ELSE
           datFil%evlIn%sta(iSta)%evnt(iEvnt)%timint%t(1) = 0.0D0
        END IF

        ! Read datastr2 (if empty then assign the t(1) value)
        IF( LEN_TRIM(datstrE) /= 0 )THEN
           CALL st2tim(1, 1, datstrE(1:19), &
                datFil%evlIn%sta(iSta)%evnt(iEvnt)%timint%t(2) )
        ELSE
           datFil%evlIn%sta(iSta)%evnt(iEvnt)%timint%t(2) = HUGE(0.0D0)
        END IF

        ! Read period
        datFil%evlIn%sta(iSta)%evnt(iEvnt)%omega = 2*pi/9999.999D0
        IF( LEN_TRIM(period) /= 0 )THEN
           READ(period,'(F9.4)') datFil%evlIn%sta(iSta)%evnt(iEvnt)%omega
           ! Convert period in angular frequency
           IF( datFil%evlIn%sta(iSta)%evnt(iEvnt)%type == 'PERI' .AND. &
               datFil%evlIn%sta(iSta)%evnt(iEvnt)%omega /= 0.0D0 )THEN
              datFil%evlIn%sta(iSta)%evnt(iEvnt)%omega = 2*pi / &
                   datFil%evlIn%sta(iSta)%evnt(iEvnt)%omega
           ELSE IF( datFil%evlIn%sta(iSta)%evnt(iEvnt)%type /= 'SYNT' .AND. &
                    datFil%evlIn%sta(iSta)%evnt(iEvnt)%type /= 'OTLD' )THEN
              datFil%evlIn%sta(iSta)%evnt(iEvnt)%omega = 2*pi/9999.999D0
           END IF
        END IF

        ! Read amplitudes
        ampliVal(:) = 0.0D0
        IF( LEN_TRIM(ampliTxt(1)) /= 0 ) READ(ampliTxt(1),'(F7.4)') ampliVal(1)
        datFil%evlIn%sta(iSta)%evnt(iEvnt)%par(1) = ampliVal(1)
        IF( LEN_TRIM(ampliTxt(2)) /= 0 ) READ(ampliTxt(2),'(F7.4)') ampliVal(2)
        datFil%evlIn%sta(iSta)%evnt(iEvnt)%par(2) = ampliVal(2)
        IF( LEN_TRIM(ampliTxt(3)) /= 0 ) READ(ampliTxt(3),'(F7.4)') ampliVal(3)
        datFil%evlIn%sta(iSta)%evnt(iEvnt)%par(3) = ampliVal(3)

        ! Read reamark
        IF( LEN_TRIM( line ) >= 108 )THEN
           READ(line,'(107X,A8)') datFil%evlIn%sta(iSta)%evnt(iEvnt)%remark
        ELSE IF( datFil%evlIn%sta(iSta)%evnt(iEvnt)%type == 'DISC' )THEN
           datFil%evlIn%sta(iSta)%evnt(iEvnt)%remark = 'EVL-DISC'
        ELSE IF( datFil%evlIn%sta(iSta)%evnt(iEvnt)%type == 'VELO' )THEN
           datFil%evlIn%sta(iSta)%evnt(iEvnt)%remark = 'EVL-VELO'
        ELSE IF( datFil%evlIn%sta(iSta)%evnt(iEvnt)%type == 'OUTL' )THEN
           datFil%evlIn%sta(iSta)%evnt(iEvnt)%remark = 'EVL-OUTL'
        ELSE IF( datFil%evlIn%sta(iSta)%evnt(iEvnt)%type == 'PERI' )THEN
           datFil%evlIn%sta(iSta)%evnt(iEvnt)%remark = 'EVL-PERI'
        ELSE
           datFil%evlIn%sta(iSta)%evnt(iEvnt)%remark = ''
        END IF

        ! Read significance
        datFil%evlIn%sta(iSta)%evnt(iEvnt)%siTst = nonSignificant
        IF( LEN_TRIM( line ) >= 106 )THEN
           READ(line,'(105X,A1)') signifTxt
           IF( signifTxt ==  'Y' )THEN
              datFil%evlIn%sta(iSta)%evnt(iEvnt)%siTst = significant
           END IF
        END IF

     END DO
  END DO

  ! Deallocate memory of lineBuffer
  DEALLOCATE(lineBuffer,stat=iac)

  ! Deallocate variables
  DEALLOCATE(nEvnt,stat=iac)
  DEALLOCATE(staNam,stat=iac)

! End of subroutine
! -----------------
!  CALL debug_exit(srName)
  RETURN

END SUBROUTINE fodirevl

END MODULE s_FODIREVL
