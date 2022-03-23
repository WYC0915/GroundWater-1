MODULE s_FODIRERQ
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE fodirerq(opt,datFil,sCore)

! -------------------------------------------------------------------------
! Purpose:    Read the earthquake file ERQ
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
  USE m_bern,    ONLY: i4b, r8b, lfnErr, lfn001, linelength, &
                       shortLineLength, staNameLength
  USE m_maxdim,  ONLY: maxsta
  USE d_const,   ONLY: pi
  USE d_datum,   ONLY: datum
  USE p_fodits,  ONLY: t_opt, t_datfil, t_sCore

! operator, methods:
!  USE d_debug,   ONLY: debug_entry, debug_exit
  USE m_epoch,   ONLY: OPERATOR(.epochToReal.)

! subroutines, functions:
  USE f_djul
  USE f_nextline
  USE s_exitrc
  USE s_alcerr
  USE s_ellxyz
  USE s_getdat
  USE s_opnerr
  USE s_opnfil

! no implicit
  IMPLICIT NONE

! subroutine name
  CHARACTER(LEN=shortLineLength), PARAMETER    :: srName = 'fodirerq'


! List of Arguments
! -----------------
! input:
  TYPE(t_opt)                    :: opt        ! Option structure
  TYPE(t_datFil)                 :: datFil     ! I/O files structure
  TYPE(t_sCore)                  :: sCore      ! Data structure

! input/output:

! output:


! Local Types
! -----------


! Local Parameters
! ----------------


! Local Variables
! ---------------
  CHARACTER(LEN=lineLength)      :: line

  INTEGER(i4b)                   :: ios,iac
  INTEGER(i4b)                   :: iCnt
  INTEGER(i4b)                   :: nLine
  INTEGER(i4b)                   :: iErq
  INTEGER(i4b)                   :: nErq
  INTEGER(i4b)                   :: rYear
  INTEGER(i4b)                   :: rMonth
  INTEGER(i4b)                   :: rDay

  REAL(r8b)                      :: rOrigTime
  REAL(r8b)                      :: cDay


! Call debug routine
! ------------------
!  CALL debug_entry(srName)


! Initialization of all variables
! -------------------------------
  datFil%erqIn%nErq = 0
  ios = 0

  ! Return if the ERQ file is not present
  IF( LEN_TRIM(opt%inErqFile) == 0 )THEN
     RETURN
  END IF

  ! Get datum
  CALL getdat(sCore%datumName, datum%aell, datum%bell, datum%dxell, &
       datum%drell, datum%scell)

  ! Open ERQ file
  CALL opnfil(lfn001,opt%inErqFile,'UNKNOWN','FORMATTED','READONLY',' ',ios)
  CALL opnerr(lfnerr,lfn001,ios,opt%inErqFile,srName)

  ! Read and skip the file header as well as the column descriptions
  nLine = 0
  ReadHeadLoop: DO iCnt = 1,20
     nLine = nLine + 1
     line = nextline(lfn001,0)
     IF ( line(1:4) == ' CAT' )THEN
        EXIT
     END IF
  END DO ReadHeadLoop
  IF( nLine == 20 )THEN
     WRITE(lfnerr,'(/,A,/)') ' *** SR FODIRERQ: Error reading ERQ input file.'
     CALL exitrc(2)
  ELSE
     READ(lfn001,'(A)') line
  END IF

  ! Read 3 spare lines
  DO iCnt = 1,2
     line = nextline(lfn001,0)
  END DO

  ! Determine the number of earthquake events contained in the ERQ file
  nErq = 0
  DO
     line = nextline(lfn001,0)
     IF( line == '' )THEN
        EXIT
     ELSE
        nErq = nErq + 1
     END IF
  END DO

  ! Allocation of memory
  datFil%erqIn%nErq = nErq
  ALLOCATE(datFil%erqIn%erq(nErq),stat=iac)
  CALL alcerr(iac,'datFil%erqIn%erq', (/nErq/),srName)

  ! Rewind the opened file
  REWIND(lfn001)

  ! Position the line pointer at the first line of data
  DO iCnt = 1,nLine+2
     line = nextline(lfn001,0)
  END DO

  ! Read the table and store it into sCore%evlIn struct
  DO iErq = 1,nErq
     line = nextline(lfn001,0)
     READ(line,'(8X,I4,2X,I2,1X,I2,1X,F9.2,1X,F6.2,1X,F7.2,1X,&
          &I3,2X,F4.1,1X,A5)') &
          rYear, &
          rMonth, &
          rDay, &
          rOrigTime, &
          datFil%erqIn%erq(iErq)%lat, &
          datFil%erqIn%erq(iErq)%lon, &
          datFil%erqIn%erq(iErq)%dep, &
          datFil%erqIn%erq(iErq)%mag, &
          datFil%erqIn%erq(iErq)%magDescr

          cDay = rDay + rOrigTime/10000.0D0/24.D0
          datFil%erqIn%erq(iErq)%mjd = djul(rYear,rMonth,cDay)

          ! Compute the xyzCrd
          CALL ellxyz(datum%aell,datum%bell, &
               datum%dxell,datum%drell,datum%scell, &
               (/ datFil%erqIn%erq(iErq)%lat * pi/180.0D0, &
                  datFil%erqIn%erq(iErq)%lon * pi/180.0D0, &
                  0.0D0 /), &
               datFil%erqIn%erq(iErq)%crdXyz(:) )

  END DO

  ! Close ERQ file
  CLOSE(lfn001)

! End of subroutine
! -----------------
!  CALL debug_exit(srName)
  RETURN

END SUBROUTINE fodirerq

END MODULE s_FODIRERQ
