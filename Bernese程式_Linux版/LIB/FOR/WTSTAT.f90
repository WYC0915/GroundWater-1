! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE s_WTSTAT
CONTAINS

  SUBROUTINE wtstat(iact,filcor,title,datum,nStat,stName, &
                    xStat,staNum,staFlg,timCrd,plate,pldOnly,footer)

! -------------------------------------------------------------------------
! Purpose:    Write a coordinate/velocity result file
!
! Remark:     To write a coordinate file the parameter "timcrd" is
!             required
!
! Author:     M.Rothacher, E.Brockmann
!
! Created:    11-Dec-1987
!
! Changes:    11-Jan-1993 USE OF SR "OPNFIL" TO OPEN FILES
!             27-Sep-1993 WITHOUT APRIORI FILE (KEYWORD: COOR)
!             24-Jan-1994 WRITE REFERENCE EPOCH
!             28-Apr-1994 MR: CHECK END OF FILE FOR COORDINATE
!                             INPUT FILE
!             10-Aug-1994 MR: CALL EXITRC
!              8-Sep-1995 EB: ALLOW FILENAME
!             04-Aug-1999 PF: UNIFIED VERSION. MERGED WTSTAT AND WTSTA2
!                             (ADDITION OF FILCOR AND TIMCRD)
!             09-Nov-2000 CU: SWITCH TO NEW MENU SYSTEM
!             13-Nov-2000 RD: TAKES 'BASLIN' (new menu) OR
!                             'COORD' (old menu) FOR APRIORI INFORMATION
!             13-May-2003 RD: DO NOT CLOSE A CLOSED FILE
!             28-Jun-2004 RD: PARAMETER MAXSTA IS NOT NEEDED IN F90
!             21-Jun-2005 MM: COMLFNUM.inc REMOVED, m_bern ADDED
!             23-Jun-2005 MM: IMPLICIT NONE AND DECLARATIONS ADDED
!             17-Jan-2011 RD: Transfer from F77 to F90
!             17-Jan-2011 RD: Use TIMST2, write stanum modolo 1000
!             17-Jan-2011 RD: Do not use the AUXFIL anymore
!             27-Jan-2011 RD: Add velocity writing option
!             02-Feb-2011 RD: Writing of footer lines added
!             20-Jul-2011 RD: More digits
!             08-Sep-2011 SL/SS: do not return if iUpd==0
!             24-Nov-2011 RD: Check for ASSOCIATED before writing "footer"
!             20-Sep-2012 RD: Use M_BERN with ONLY
!             20-Sep-2012 RD: Remove unused modules
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------
! Used modules:
  USE m_bern,   ONLY: i4b, r8b, lfnerr, lfnloc, &
                      shortLineLength, lineLength, timStrgLength, &
                      staNameLength, staFla2Length
  USE d_const,  ONLY: const_def,date,time

  USE s_opnfil
  USE s_opnerr
  USE s_gtflna
  USE s_timst2
  USE s_getco3
  USE s_alcerr
  USE s_inquire
  IMPLICIT NONE

! Parameters
! ----------
! input:
  INTEGER(i4b)                   :: iact   ! Defines what to do:
                                  ! =0: Writes the list of coord. as they are
                                  ! =1: Updates coord. from keyw. COORD
                                  ! =2: Adds only stations which are not in file
                                  !     registered in keyw. COORD
                                  ! if there is no parameter "TIMCRD" in the
                                  ! call of WTSTAT a velocity file is written.
  CHARACTER(LEN=*)               :: filcor ! Filename
                                  ! if "filcor" is blank, the result filename
                                  ! is taken from keyw. COORDRS/VELORS.
  CHARACTER(LEN=shortLineLength) :: title  ! Title line for coordinate file
  CHARACTER(LEN=16)              :: datum  ! Geodetic datum of coordinate file
  INTEGER(i4b)                   :: nStat  ! Number of stations to be updated
  CHARACTER(staNameLength),       &
                    DIMENSION(*) :: stName ! Station names
  REAL(r8b),      DIMENSION(3,*) :: xStat  ! Station coordinates
  CHARACTER(LEN=*), DIMENSION(*) :: staFlg ! Flag for stations
  INTEGER(i4b),     OPTIONAL,     &
                    DIMENSION(*) :: staNum ! Station numbers
                                  ! If no stanum is present, the station numbers
                                  ! will be the counter in the soted list
  REAL(r8b),        OPTIONAL     :: timCrd ! Reference epoch of coordinate
                                           ! estimation in mod jul days
  CHARACTER(LEN=*), OPTIONAL,     &
                    DIMENSION(*) :: plate  ! Tectonic plate definition
  LOGICAL,          OPTIONAL     :: pldOnly! Write a PLD file
  CHARACTER(LEN=*), OPTIONAL,     &
                    DIMENSION(:), &
                    POINTER      :: footer ! comment lines below the data area

! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER    :: srName = 'wtstat'

! Local variables
! ---------------
  CHARACTER(LEN=linelength)      :: filapr
  CHARACTER(LEN=linelength)      :: line
  CHARACTER(LEN=timStrgLength)   :: tString
  CHARACTER(LEN=16)              :: datum_
  CHARACTER(LEN=lineLength),      &
       POINTER,     DIMENSION(:) :: footer_
  CHARACTER(LEN=staNameLength),   &
       POINTER,     DIMENSION(:) :: stName_
  CHARACTER(LEN=16)              :: datumNw
  CHARACTER(LEN=staNameLength),   &
       ALLOCATABLE, DIMENSION(:) :: stNameNw
  CHARACTER(LEN=4),               &
       POINTER,     DIMENSION(:) :: plate_
  CHARACTER(LEN=4),               &
       ALLOCATABLE, DIMENSION(:) :: plateNw
  CHARACTER(LEN=staFla2Length),   &
       ALLOCATABLE, DIMENSION(:) :: staFlgNw

  INTEGER(i4b)                   :: nStat_
  INTEGER(i4b),                   &
       POINTER,     DIMENSION(:) :: staNum_
  INTEGER(i4b)                   :: nStatNw
  INTEGER(i4b),                   &
       ALLOCATABLE, DIMENSION(:) :: staNumNw
  INTEGER(i4b),                   &
       ALLOCATABLE, DIMENSION(:) :: staIdxNw
  INTEGER(i4b), DIMENSION(nStat) :: found
  INTEGER(i4b)                   :: iLin
  INTEGER(i4b)                   :: iSta,jSta
  INTEGER(i4b)                   :: iUpd
  INTEGER(i4b)                   :: irc,iac,iostat

  REAL(r8b),                      &
     POINTER,     DIMENSION(:,:) :: xStat_
  REAL(r8b),                      &
     ALLOCATABLE, DIMENSION(:,:) :: xStatNw

  LOGICAL                        :: yes
  LOGICAL                        :: sortCrd

! Init variables
! --------------
  NULLIFY(staNum_)
  NULLIFY(stName_)
  NULLIFY(xStat_)
  NULLIFY(plate_)
  NULLIFY(footer_)

! Do not save if no coordinate result file name in file name table
! ----------------------------------------------------------------
  IF (filcor == ' ') THEN
    IF (PRESENT(timcrd)) THEN
      CALL gtflna(0,'COORDRS',filcor,irc)
    ELSE
      CALL gtflna(0,'VELORS',filcor,irc)
    ENDIF
    IF(IRC /= 0) RETURN
  ENDIF
  IF (FILCOR == ' ') RETURN

! Initialize "found"
! ------------------
  found(1:nstat)=0

! READ COORDINATE INPUT FILE (if available)
! -----------------------------------------
  datumNw = datum
  nStat_  = 0
  nStatNw = nStat

  IF (iact /= 0) THEN

    IF ( PRESENT(timcrd) ) THEN
      CALL gtflna(0,'COORD',filapr,irc)
    ELSE
      CALL gtflna(0,'VELAPR',filapr,irc)
    ENDIF
    CALL inquire(FILE=filapr,EXIST=yes)
    IF (irc == 0 .AND. yes) THEN
      CALL getco3(filapr,1,(/ '@' /),datum=datum_,                         &
                  nStat=nStat_,stName=stName_,staNum=staNum_,xStat=xStat_, &
                  plate=plate_,footer=footer_)
      DO iSta = 1,nStat
        DO jSta = 1,nStat_
          IF (stName(iSta) == stName_(jSta)) THEN
            found(iSta) = jSta
            EXIT
          ENDIF
        ENDDO
      ENDDO
      datumNw=datum_

      nStatNw = nStat_+ nStat
      DO iSta = 1,nStat
        IF (found(iSta) /= 0) nStatNw = nStatNw-1
      ENDDO

    ENDIF

  ENDIF

! Allocate the resulting list of coordinates
! ------------------------------------------
  ALLOCATE(staNumNw(nStatNw),stat=iac)
  CALL alcerr(iac,'staNumNw',(/nStatNw/),srName)
  IF (nstat_ > 0) staNumNw(1:nStat_) = staNum_(1:nStat_)
  staNumNw(nStat_+1:nStatNw) = 0

  ALLOCATE(stNameNw(nStatNw),stat=iac)
  CALL alcerr(iac,'stNameNw',(/nStatNw/),srName)
  IF (nstat_ > 0) stNameNw(1:nStat_) = stName_(1:nStat_)
  stNameNw(nStat_+1:nStatNw) = ''

  ALLOCATE(xStatNw(3,nStatNw),stat=iac)
  CALL alcerr(iac,'xStatNw',(/3,nStatNw/),srName)
  IF (nstat_ > 0) xStatNw(1:3,1:nStat_) = xStat_(1:3,1:nStat_)
  xStatNw(1:3,nStat_+1:nStatNw) = 0d0

  ALLOCATE(staFlgNw(nStatNw),stat=iac)
  CALL alcerr(iac,'staFlgNw',(/nStatNw/),srName)
  staFlgNw = ''

  ALLOCATE(plateNw(nStatNw),stat=iac)
  CALL alcerr(iac,'plateNw',(/nStatNw/),srName)
  IF (nstat_ > 0) plateNw(1:nStat_) = plate_(1:nStat_)
  plateNw(nStat_+1:nStatNw) = ''

  ALLOCATE(staIdxNw(nStatNw),stat=iac)
  CALL alcerr(iac,'staIdxNw',(/nStatNw/),srName)
  staIdxNw(1:nStatNw) = (/ (iac,iac=1,nStatNw) /)

! Update the station result record
! --------------------------------
  iUpd = 0
  DO iSta = 1,nStat
    IF (found(iSta) == 0) THEN
      nStat_ = nStat_+1
      jSta = nStat_
      IF (PRESENT(staNum)) staNumNw(jSta)=staNum(iSta)
      stNameNw(jSta)=stName(iSta)
!      IF (PRESENT(plate) .AND. LEN_TRIM(FILAPR) > 0) &
!        plateNw(jSta) =plate_(found(iSta))
    ELSE IF (iact == 2) THEN
      CYCLE
    ELSE
      jSta = found(iSta)
    ENDIF
    iUpd = iUpd + 1
    xStatNw(1:3,jSta) = xStat(1:3,iSta)
    staFlgNw(jSta) = adjustl(staFlg(iSta))
    IF (PRESENT(plate)) plateNw(jSta) = plate(iSta)
  ENDDO

!!!  IF (iUpd == 0) RETURN

! Open coordinate result file
! ---------------------------
  CALL opnfil(lfnloc,filcor,'UNKNOWN','FORMATTED',' ',' ',iostat)
  CALL opnerr(lfnerr,lfnloc,iostat,filcor,srName)

! Write title and datum
! ---------------------
  IF (const_def == 1) THEN
    WRITE(lfnloc,'(A63,2X,A9,1X,A5,/,80("-"))') TITLE(1:63),DATE,TIME
  ELSE
    WRITE(lfnloc,'(A80,/,80("-"))') TITLE
  ENDIF

! Convert modified julian date to date and time
  IF ( PRESENT(timcrd) ) THEN
    CALL timst2(1,1,timCrd,tString)
    WRITE(lfnloc,"(A,A16,2X,A,A19,//,A,11X,A,10X,A,10X,A,5X,A,/)")         &
      "LOCAL GEODETIC DATUM: ",datumNw,"EPOCH: ",tString,                  &
      "NUM  STATION NAME","X (M)","Y (M)","Z (M)","FLAG"
  ELSE
    WRITE(lfnloc,"(A,A16,//,A,11X,A,7X,A,7X,A,2X,A,3X,A,/)")               &
      "LOCAL GEODETIC DATUM: ",datumNw,"NUM  STATION NAME",                &
      "VX (M/Y)","VY (M/Y)","VZ (M/Y)","FLAG","PLATE"
  ENDIF

! Order the list of stations
! --------------------------
  sortCrd = .FALSE.
  DO WHILE (.NOT. sortCrd)
    sortCrd = .TRUE.
    DO iSta = 1,nSTatNw-1
      IF (stNameNw(staIdxNw(iSta)) > stNaMEnW(staIdxNw(iSta+1))) THEN
        sortCrd = .FALSE.
        iac = staIdxNw(iSta)
        staIdxNw(iSta) = staIdxNw(iSta+1)
        staIdxNw(iSta+1) = iac
      ENDIF
    ENDDO
  ENDDO

! Define new station numbers on request
! -------------------------------------
  IF ( .NOT. PRESENT(staNum) ) THEN
    DO iSta = 1,nStatNw
      staNumNw(staIdxNw(iSta)) = iSta
    ENDDO
  ENDIF

! Write all station coordinates
! -----------------------------
  IF (PRESENT(timcrd)) THEN
    DO iSta=1,nStatNw
      line = ''
      WRITE(line,'(I3,2X,A16,3F15.5,4X,A5)') &
            MOD(staNumNw(staIdxNw(iSta)),1000),stNameNw(staIdxNw(iSta)), &
            xStatNw(1:3,staIdxNw(iSta)),staFlgNw(staIdxNw(iSta))
      WRITE(lfnloc,'(A)') TRIM(line)
    ENDDO

! Write all plates for the stations
! ---------------------------------
  ELSE IF (PRESENT(pldOnly)) THEN
    DO iSta=1,nStatNw
      line = ''
      WRITE(line,'(I3,2X,A16,54X,A4)') &
            MOD(staNumNw(staIdxNw(iSta)),1000),stNameNw(staIdxNw(iSta)), &
            plateNw(staIdxNw(iSta))
      WRITE(lfnloc,'(A)') TRIM(line)
    ENDDO

! Write all station velocities
! ----------------------------
  ELSE
    DO iSta=1,nStatNw
      line = ''
      WRITE(line,'(I3,2X,A16,3F15.5,4X,A4,1X,A4)') &
            MOD(staNumNw(staIdxNw(iSta)),1000),stNameNw(staIdxNw(iSta)), &
            xStatNw(1:3,staIdxNw(iSta)),staFlgNw(staIdxNw(iSta))(1:4), &
            plateNw(staIdxNw(iSta))
      WRITE(lfnloc,'(A)') TRIM(line)
    ENDDO
  ENDIF

! Add the comments, if requested
! ------------------------------
  IF (PRESENT(footer)) THEN
    IF (ASSOCIATED(footer)) THEN
      DO iLin = 1, SIZE(footer)
        WRITE(lfnloc,'(A)') TRIM(footer(iLin))
      ENDDO
      IF (SIZE(footer) == 0) WRITE(LFNLOC,'(1X)')
    ELSE
      WRITE(LFNLOC,'(1X)')
    ENDIF
  ELSE IF (LEN_TRIM(FILAPR) > 0 .AND. ASSOCIATED(footer_)) THEN
    DO iLin = 1, SIZE(footer_)
      WRITE(lfnloc,'(A)') TRIM(footer_(iLin))
    ENDDO
  ELSE
    WRITE(LFNLOC,'(1X)')
  ENDIF

! Close file
! ----------
  CLOSE(UNIT=lfnloc)

! Deallocate
! ----------
  DEALLOCATE(staNumNw,STAT=iac)
  DEALLOCATE(stNameNw,STAT=iac)
  DEALLOCATE(xStatNw,STAT=iac)
  DEALLOCATE(plateNw,STAT=iac)
  DEALLOCATE(staFlgNw,STAT=iac)
  DEALLOCATE(staIdxNw,STAT=iac)

  IF (LEN_TRIM(FILAPR) > 0) THEN
    DEALLOCATE(staNum_,STAT=iac)
    DEALLOCATE(stName_,STAT=iac)
    DEALLOCATE(xStat_,STAT=iac)
    DEALLOCATE(plate_,STAT=iac)
    DEALLOCATE(footer_,STAT=iac)
  ENDIF

  RETURN
  END SUBROUTINE wtstat

END MODULE s_wtstat
