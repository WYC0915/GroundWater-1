
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE d_grid

! -------------------------------------------------------------------------
! Purpose:    This module provides the interpolated values from a grid
!
! Author:     R. Dach
!
! Created:    30-Jun-2008
!
! Changes:    30-Sep-2009 RD: Add some TSTKEY in context of GTFILE2
!             30-Sep-2009 SL: t_gridType to PUBLIC, some INT to DBLE
!             04-Jun-2010 PS: Typo in error message corrected
!             15-Sep-2010 RD: Correct closing of lfnloc
!             11-Oct-2010 RD: Some small accelerations
!             31-Oct-2010 RD: Correct PPP/single-file bug
!             18-Jan-2011 MF: Additional check of gridType in sr whichCell
!             13-Apr-2011 RD: Limit extrapolation in time
!             30-Jun-2011 RD: Accept also the middle of a cell
!             30-Jun-2011 RD: The order of components is flexible
!             28-Oct-2011 KS: Bug fixed: interpolation for long < min_lam
!             25-Jan-2012 RD: Write no prtGridInfo header if no grid files
!             10-Aug-2012 DT: Consider scaling factor given in header
!                             (add t_fileGrid%scale)
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern

  IMPLICIT NONE

! Declare access rights
  PRIVATE
  PUBLIC  :: initGridBuffer, getGrid, prtGridInfo, typLen, grdNeq, &
             getGridKeyw, getGridInp, t_gridType,                  &
             grd_vmf1_zh, grd_vmf1_zw, grd_vmf1_ah, grd_vmf1_aw,   &
             grd_aload_n, grd_aload_e, grd_aload_u,                &
             grd_ontld_n, grd_ontld_e, grd_ontld_u,                &
             grd_hload_n, grd_hload_e, grd_hload_u

! Define Keywords to identify the grids
! -------------------------------------
  INTEGER(i4b), PARAMETER   :: typLen = 12 ! Length of the grid type string
         ! REMARK: If you increase this value, do not foreget to adapt the
         ! format string in SR prtGridInfo of this module, below

  TYPE t_gridType
    CHARACTER(LEN=typLen)        :: head   ! Header key in Grid file
    INTEGER(i4b)                 :: indx   ! Column in grid file
    CHARACTER(LEN=5)             :: comp   ! Component label in the header of the grid file
    CHARACTER(LEN=KeyNameLength) :: keyw   ! Keyword in Bernese INP files
  END TYPE

  ! Each keyword needs a type identifier in the header of the grid file
  !                     and the number of column where to find the data
  TYPE(t_gridType) :: grd_vmf1_ah = t_gridType('VMF1        ', 0, 'AH   ', 'VMF1_FILES ')
  TYPE(t_gridType) :: grd_vmf1_aw = t_gridType('VMF1        ', 0, 'AW   ', 'VMF1_FILES ')
  TYPE(t_gridType) :: grd_vmf1_zh = t_gridType('VMF1        ', 0, 'ZHD  ', 'VMF1_FILES ')
  TYPE(t_gridType) :: grd_vmf1_zw = t_gridType('VMF1        ', 0, 'ZWD  ', 'VMF1_FILES ')

  TYPE(t_gridType) :: grd_aload_u = t_gridType('ATM-NT-LOAD ', 0, 'UP   ', 'ALOAD_FILES')
  TYPE(t_gridType) :: grd_aload_n = t_gridType('ATM-NT-LOAD ', 0, 'NORTH', 'ALOAD_FILES')
  TYPE(t_gridType) :: grd_aload_e = t_gridType('ATM-NT-LOAD ', 0, 'EAST ', 'ALOAD_FILES')

  TYPE(t_gridType) :: grd_ontld_u = t_gridType('OCN-NT-LOAD ', 0, 'UP   ', 'ONTLD_FILES')
  TYPE(t_gridType) :: grd_ontld_n = t_gridType('OCN-NT-LOAD ', 0, 'NORTH', 'ONTLD_FILES')
  TYPE(t_gridType) :: grd_ontld_e = t_gridType('OCN-NT-LOAD ', 0, 'EAST ', 'ONTLD_FILES')

  TYPE(t_gridType) :: grd_hload_u = t_gridType('HYDR-LOAD   ', 0, 'UP   ', 'HLOAD_FILES')
  TYPE(t_gridType) :: grd_hload_n = t_gridType('HYDR-LOAD   ', 0, 'NORTH', 'HLOAD_FILES')
  TYPE(t_gridType) :: grd_hload_e = t_gridType('HYDR-LOAD   ', 0, 'EAST ', 'HLOAD_FILES')

  ! Specify the number of columns per data type in the grid file
  TYPE(t_gridType), PARAMETER,   &
       DIMENSION(4) :: maxVal =  (/ t_gridType('VMF1        ', 4, '     ', 'VMF1_FILES '), &
                                    t_gridType('ATM-NT-LOAD ', 3, '     ', 'ALOAD_FILES'), &
                                    t_gridType('OCN-NT-LOAD ', 3, '     ', 'ONTLD_FILES'), &
                                    t_gridType('HYDR-LOAD   ', 3, '     ', 'HLOAD_FILES') /)

  ! Specify the order of the grid keywords for indexing in NEQ-files
  ! (Do never change the order of the existing keywords!!!!)
  CHARACTER(LEN=typLen), DIMENSION(0:3), PARAMETER :: grdNeq = &
          (/ 'VMF1        ','ATM-NT-LOAD ','OCN-NT-LOAD ','HYDR-LOAD   ' /)


  LOGICAL, PARAMETER :: DEBUG = .FALSE.

! Define some local dimension parameters
! --------------------------------------
  INTEGER(i4b), PARAMETER :: maxCells = 100 ! Default number of cells to be
                                            ! buffered. May be overwritten
                                            ! with an optional argument in the
                                            ! first call of getGrid!


! Remember some general information of a GRID-file
! ------------------------------------------------
  TYPE t_fileGrid
    CHARACTER(LEN=keyNameLength)         :: keyWord  = ''   ! Keyword in panel
    CHARACTER(LEN=fileNameLength)        :: fileName = ''   ! Filename
    INTEGER(i4b)                         :: fileIdx  = 0    ! Index of the grid
                                                            ! in the file
    CHARACTER(LEN=typLen)                :: fileType = ''   ! Type of the grid
    REAL(r8b)                            :: minPhi   = 0D0  ! Range of the grid
    REAL(r8b)                            :: maxPhi   = 0D0  !
    REAL(r8b)                            :: minLam   = 0D0  !
    REAL(r8b)                            :: maxLam   = 0D0  !
    REAL(r8b)                            :: stepPhi  = 0D0  ! Resolution of
    REAL(r8b)                            :: stepLam  = 0D0  ! the grid
    REAL(r8b)                            :: epoch           ! Epoch of the grid
    REAL(r8b)                            :: scale           ! Scaling factor
  END TYPE


! Buffer for a list of grid-cells of one type
! -------------------------------------------
  TYPE t_oneGrid
    CHARACTER(LEN=typLen)                     :: gridType
    INTEGER(i4b)                              :: nVal    ! number of values
                                                         ! per record
    INTEGER(i4b)                              :: numDim  ! dimension of the
                                                         ! buffer
    INTEGER(i4b)                              :: lstUpd  ! Index of the last
                                                         ! entry
    TYPE(t_gridCell), DIMENSION(:,:), POINTER :: cells   ! Grid cells of this
                                                         ! type
  END TYPE t_oneGrid

! One single grid cell
! --------------------
  TYPE t_gridCell
    INTEGER(i4b),      DIMENSION(2)      :: source     ! index into gridList
    REAL(r8b),         DIMENSION(2)      :: crdMin     ! phi/lambda, rad
    REAL(r8b),         DIMENSION(2)      :: crdMax     ! phi/lambda, rad
    REAL(r8b),         DIMENSION(2)      :: mjd        ! mjd: min,max
    REAL(r8b),         DIMENSION(2,2,2)  :: coeff      ! 1,1,1: Pmin,Lmin,Tmin
                                                       ! 1,2,1: Pmin,Lmax,Tmin
                                                       ! 2,1,1: Pmax,Lmin,Tmin
                                                       ! ...
                                                       ! 2,2,2: Pmax,Lmax,Tmax
  END TYPE t_gridCell

! Some module variables
! ---------------------
  LOGICAL,                                 SAVE :: first = .TRUE.
  TYPE(t_fileGrid), DIMENSION(:), POINTER, SAVE :: gridList
  TYPE(t_oneGrid),  DIMENSION(:), POINTER, SAVE :: allGrids


CONTAINS



! -----------------------------------------------------------------------------
! Get the grid identified of a specific component and grid type
! -----------------------------------------------------------------------------
  FUNCTION getGridKeyw(grdKeyw, comp)
    ! List of Parameters
    ! input:
    CHARACTER(LEN=typLen)          :: grdKeyw ! Keyword of the grid type
    INTEGER(i4b)                   :: comp    ! Component:
                                              ! 1: up, 2: north, 3: east
    ! output:
    TYPE(t_gridType)               :: getGridKeyw ! Grid identifier record

    IF (grdKeyw == grd_aload_u%head .AND. comp == 1) getGridKeyw = grd_aload_u
    IF (grdKeyw == grd_aload_n%head .AND. comp == 2) getGridKeyw = grd_aload_n
    IF (grdKeyw == grd_aload_e%head .AND. comp == 3) getGridKeyw = grd_aload_e

    IF (grdKeyw == grd_ontld_u%head .AND. comp == 1) getGridKeyw = grd_ontld_u
    IF (grdKeyw == grd_ontld_n%head .AND. comp == 2) getGridKeyw = grd_ontld_n
    IF (grdKeyw == grd_ontld_e%head .AND. comp == 3) getGridKeyw = grd_ontld_e

    IF (grdKeyw == grd_hload_u%head .AND. comp == 1) getGridKeyw = grd_hload_u
    IF (grdKeyw == grd_hload_n%head .AND. comp == 2) getGridKeyw = grd_hload_n
    IF (grdKeyw == grd_hload_e%head .AND. comp == 3) getGridKeyw = grd_hload_e

  END FUNCTION getGridKeyw

! -----------------------------------------------------------------------------
! Get the grid identified of a specific component and grid type
! -----------------------------------------------------------------------------
  FUNCTION getGridInp(grdKeyw)
    ! List of Parameters
    ! input:
    CHARACTER(LEN=typLen)          :: grdKeyw     ! Keyword of the grid type

    ! output:
    CHARACTER(LEN=keyNameLength)   :: getGridInp  ! Grid identifier record

    ! Local Variables
    INTEGER(i4b)                   :: ii

    DO ii = 1,SIZE(maxVal)
      IF (grdKeyw == maxVal(ii)%head) getGridInp = maxVal(ii)%keyw
    ENDDO

  END FUNCTION getGridInp

! -----------------------------------------------------------------------------
! Get the interpolated value from the grid
! -----------------------------------------------------------------------------
  FUNCTION getGrid(grdType, epoch, xstell, maxsta)
    USE d_const, ONLY: RHO, PI
    USE s_exitrc

    ! List of Parameters
    ! input:
    TYPE(t_gridType)               :: grdType ! Grid identifier record
    REAL(r8b)                      :: epoch   ! epoch of the request
    REAL(r8b), DIMENSION(3)        :: xstell  ! 1,2: lat,lon in radiant
                                              ! 3:   hgt in meter
    INTEGER(i4b), OPTIONAL         :: maxSta  ! size of the buffer, only
                                              ! relevant in the first call;
                                              ! if not given the default value
                                              ! from maxCells is used instead
    ! output
    REAL(r8b)                      :: getgrid ! interpolated value from the grid


    ! Local variables
    INTEGER(i4b)                   :: iDim
    INTEGER(i4b)                   :: iGrd, jGrd
    REAL(r8b), DIMENSION(3)        :: coord
    REAL(r8b)                      :: rHelp

IF (DEBUG) write(*,*) 'D_GRID:getGrid start',xStell(1:2)*RHO,epoch
    ! Init te buffer in the case of the first request
    IF (first) THEN
      WRITE(lfnerr,'(/,A,/,2(15X,A,/))') &
      ' *** SR D_GRID/getGrid: The buffer is not initialized.',&
           'Dear programer, ', &
           'please "call initGridBuffer" before using the SR getGrid'
      CALL exitrc(2)
    ENDIF

    ! Check whether the buffer type was specified
    iGrd = whichGrid(grdType%head)
    IF (iGrd == 0) THEN
      getGrid = 0d0
      if (grdType%head == 'VMF1') THEN
        WRITE(lfnerr,'(/,A,2(/,15X,A),/)') &
        ' *** SR D_GRID/getGrid: No grid file with VMF1 coefficients found.', &
        'Either you select another troposphere model or ',                     &
        'you specify a grid file containing VMF1 coefficients.'
        CALL exitrc(2)
      endif
      RETURN
    ENDIF

    coord = xStell
    IF ( coord(2) < 0d0 ) coord(2) = coord(2) + 2d0*PI

    ! Check whether the station is in the buffer
    jGrd = whichCell(allGrids(iGrd),coord)

    ! Add new station coordinates
    IF (jGrd == 0) THEN
      CALL addToGrid(allGrids(iGrd),coord,epoch)
      CALL updateGrid(allGrids(iGrd),coord,epoch,SIZE(allGrids(iGrd)%cells,2))

    ! Check the time interval for this grid
    ELSEIF ( allGrids(iGrd)%cells(1,jGrd)%mjd(1)-1d0/86400d0 > epoch .OR. &
             allGrids(iGrd)%cells(1,jGrd)%mjd(2)+1d0/86400d0 < epoch .OR. &
             allGrids(iGrd)%cells(1,jGrd)%coeff(1,1,1) == 1d20  .OR. &
             allGrids(iGrd)%cells(1,jGrd)%coeff(1,1,2) == 1d20  ) THEN
      CALL updateGrid(allGrids(iGrd),coord,epoch,SIZE(allGrids(iGrd)%cells,2))
    ENDIF

    ! Check which grid is needed for this station
    jGrd = whichCell(allGrids(iGrd),coord)

    ! Interpolate in the cell that was found
    iDim = grdType%indx

    IF (iDim == 0) THEN
      WRITE(lfnerr,'(/,A,2(/,15X,A),/)') &
        ' *** SR D_GRID/getGrid: Component "'//TRIM(grdType%comp)//      &
        '" not defined in the ',                                         &
        '"Data_types" field of grid type "'//TRIM(grdType%head)//'". ',  &
        'Because this component is needed the processing is stopped.'
      CALL exitrc(2)
    ENDIF


    rHelp = interpolate(allGrids(iGrd)%cells(iDim,jGrd),coord(1:2),epoch)
    getGrid = rHelp

IF (DEBUG) write(*,*) 'D_GRID:getGrid ende',grdType,xStell(1:2)*RHO,epoch,rHelp
  END FUNCTION getGrid

! -----------------------------------------------------------------------------
! Initialize the buffer of the grid files
! -----------------------------------------------------------------------------
  SUBROUTINE initGridBuffer ( bufferSize, xStell, timint)
    USE m_time, ONLY: t_timint
    USE d_const, ONLY: PI
    USE f_tstkey
    USE s_gtfile2
    USE s_alcerr

    ! List of Paramters
    ! input:
    INTEGER(i4b),     OPTIONAL      :: buffersize ! size if the buffer
    REAL(r8b),        DIMENSION(:,:),&
                      OPTIONAL      :: xstell     ! 1,iSta: lat in radiant
                                                  ! 2,iSta: lon in radiant
                                                  ! 3,iSta: hgt in meter
    TYPE(t_timint),   OPTIONAL      :: timint     ! epochs if xstell is present

    ! Local variables
    CHARACTER(LEN=keyValueLength), DIMENSION(:),   POINTER :: grdKeys
    CHARACTER(LEN=fileNameLength), DIMENSION(:,:), POINTER :: filLst

    INTEGER(i4b), DIMENSION(:), ALLOCATABLE                :: numGrids
    INTEGER(i4b)                                           :: nFiles
    INTEGER(i4b)                                           :: iFil, nFil
    INTEGER(i4b)                                           :: iKey
    INTEGER(i4b)                                           :: maxBuf
    INTEGER(i4b)                                           :: maxDim
    INTEGER(i4b)                                           :: iGrd,nGrd
    INTEGER(i4b)                                           :: iSta
    INTEGER(i4b)                                           :: ii,jj
    INTEGER(i4b)                                           :: iac

    REAL(r8b),    DIMENSION(3)                             :: coord

IF (DEBUG) write(*,*) 'D_GRID:initGridBuffer start'
    ! Buffers are already initialized
    ! -------------------------------
    IF ( .NOT. first ) THEN
      DEALLOCATE(gridList,stat=iac)
      DO ii = 1,SIZE(allGrids)
        IF (allGrids(ii)%numDim /= 0) THEN
          DEALLOCATE(allGrids(ii)%cells,stat=iac)
        ENDIF
      ENDDO
      DEALLOCATE(allGrids,stat=iac)
    ENDIF

    ! Indicate that the buffer is already initialized
    ! -----------------------------------------------
    NULLIFY(filLst)
    NULLIFY(gridList)
    NULLIFY(allGrids)

    first = .FALSE.

    ! Check which grid-keywords are defined
    ! -------------------------------------
    NULLIFY(grdKeys)
    ALLOCATE(grdKeys(SIZE(maxVal)),stat=iac)
    CALL alcerr(iac,'grdKeys',(/SIZE(maxVal)/),'D_GRID:initGridBuffer')

    DO ii = 1,SIZE(maxVal)
      grdKeys(ii) = maxVal(ii)%keyw
    ENDDO

    ! Get the information from the input files
    ! ----------------------------------------
    nFil = 0
    nGrd = 0
    DO iKey = 1, SIZE(grdKeys)
      IF (.NOT. tstkey(grdKeys(iKey))) CYCLE
      CALL gtfile2(grdKeys(iKey),1,nFiles,filLst)
      DO ii = 1,nFiles
        nFil = nFil + nFiles
      ENDDO
      nGrd = nGrd + 1
      DEALLOCATE(filLst)
    ENDDO

    ALLOCATE(numGrids(nFil),stat=iac)
    CALL alcerr(iac,'numGrids',(/nFil/),'D_GRID:initGridBuffer')

    iFil = 0
    nFil = 0
    DO iKey = 1, SIZE(grdKeys)
      IF (.NOT. tstkey(grdKeys(iKey))) CYCLE
      CALL gtfile2(grdKeys(iKey),1,nFiles,filLst)
      DO ii = 1,nFiles
        iFil = iFil + 1
        numGrids(iFil) = gridFileCount(filLst(1,ii))
        nFil = nFil + numGrids(iFil)
      ENDDO
      DEALLOCATE(filLst)
    ENDDO

    ALLOCATE(gridList(nFil),stat=iac)
    CALL alcerr(iac,'gridList',(/nFil/),'D_GRID:initGridBuffer')

    iFil = 0
    nFil = 0
    DO iKey = 1, SIZE(grdKeys)
      IF (.NOT. tstkey(grdKeys(iKey))) CYCLE
      CALL gtfile2(grdKeys(iKey),1,nFiles,filLst)
      DO ii = 1,nFiles
        iFil = iFil + 1
        DO jj = 1,numGrids(iFil)
          nFil = nFil + 1
          gridList(nFil)%keyWord  = grdKeys(iKey)(1:keyNameLength)
          gridList(nFil)%fileName = filLst(1,ii)
          gridList(nFil)%fileIdx  = jj
        ENDDO
      ENDDO
      DEALLOCATE(filLst)
    ENDDO

    DEALLOCATE(numGrids,stat=iac)

    CALL gridFileInfo()

    ! Compute the size of the buffer
    ! ------------------------------
    IF ( PRESENT (buffersize) .AND. PRESENT (xStell) ) THEN
      maxBuf = MAX(buffersize,SIZE(xStell,2))
    ELSEIF ( PRESENT (buffersize) ) THEN
      maxBuf = buffersize
    ELSEIF ( PRESENT (xStell) ) THEN
      maxBuf = SIZE(xStell,2)
    ELSE
      maxBuf = maxCells
    ENDIF

    ! Allocate the buffer
    ! -------------------
    ALLOCATE(allGrids(nGrd), stat=iac)
    CALL alcerr(iac,'allGrids',(/nGrd/),'D_GRID:initGridBuffer')

    ! Loop all grid files and init one buffer per type
    nGrd = 0
    DO iFil = 1,nFil

      ! Check whether the grid of this type has already a buffer
      ii = 0
      DO iGrd = 1,nGrd
        IF ( gridList(iFil)%fileType == allGrids(iGrd)%gridType ) THEN
          ii = iGrd
          EXIT
        ENDIF
      ENDDO
      IF (ii /= 0) CYCLE

      ! Create a new buffer for this type
      nGrd = nGrd + 1
      allGrids(nGrd)%gridType = gridList(iFil)%fileType

      maxDim = get_numVal(allGrids(nGrd)%gridType)

      NULLIFY(allGrids(nGrd)%cells)
      ALLOCATE(allGrids(nGrd)%cells(maxDim, maxBuf),stat=iac)
      CALL alcerr(iac,'allGrids(nGrd)%cells',(/ maxDim, maxBuf /), &
                  'D_GRID:initGridBuffer')

      allGrids(nGrd)%numDim = maxDim
      allGrids(nGrd)%lstUpd = 0
      DO ii = 1,2
        allGrids(nGrd)%cells(:,:)%crdMin(ii) = 0d0
        allGrids(nGrd)%cells(:,:)%crdMax(ii) = 0d0
        allGrids(nGrd)%cells(:,:)%mjd(ii)    = 0d0
      ENDDO
    ENDDO

    ! Read the Grid values for a given list of stations
    IF (PRESENT (xStell) .AND. PRESENT (timint) ) THEN
      DO iSta = 1,SIZE(xStell,2)
        DO iGrd = 1,nGrd
          coord = xStell(:,iSta)
          IF ( coord(2) < 0d0 ) coord(2) = coord(2) + 2d0*PI
          IF ( whichCell(allGrids(iGrd),coord) == 0 ) &
            CALL addToGrid(allGrids(iGrd),coord,timint%t(1))
        ENDDO
      ENDDO
    ENDIF

IF (DEBUG) write(*,*) 'D_GRID:initGridBuffer ende'
  END SUBROUTINE initGridBuffer



! -----------------------------------------------------------------------------
! Print a grid file information to the program output
! -----------------------------------------------------------------------------
  SUBROUTINE prtGridInfo
    USE d_const, ONLY: RHO
    USE s_timst2
    CHARACTER(LEN=timStrgLength)  :: epoStr
    INTEGER(i4b)                  :: iFil

IF (DEBUG) write(*,*) 'D_GRID:prtGridInfo start'
    IF ( SIZE(gridList) > 0 )                 &
      WRITE(lfnprt,'(//,A,/,A,//,A,/,A,/,A)') &
      ' INFORMATION ON VIENNA GRID FILES:',   &
      ' --------------------------------',    &
      '                                                     Range of ' // &
      'latitude     Range of longitude                                 ', &
      ' Filename                             Type           min     m' // &
      'ax  step      min    max   step          Epoch                  ', &
      ' -------------------------------------------------------------' // &
      '----------------------------------------------------------------------'
    DO iFil = 1,SIZE(gridList)
      CALL timst2(1,2,gridList(iFil)%epoch,epoStr)
      WRITE(lfnprt,'(1X,A32,2X,A12,2(2X,2F8.2,F5.2),2X,A)')      &
      gridList(iFil)%fileName,    gridList(iFil)%fileType,    &
      gridList(iFil)%minPhi*RHO,  gridList(iFil)%maxPhi*RHO,  &
      gridList(iFil)%stepPhi*RHO, gridList(iFil)%minLam*RHO,  &
      gridList(iFil)%maxLam*RHO,  gridList(iFil)%stepLam*RHO, &
      epoStr
    ENDDO
IF (DEBUG) write(*,*) 'D_GRID:prtGridInfo ende'
  END SUBROUTINE prtGridInfo

! -----------------------------------------------------------------------------
! Count number of grids in a file
! -----------------------------------------------------------------------------
  FUNCTION gridFileCount(fileName)
    USE s_opnfil
    USE s_opnerr

    ! List of parameters
    ! ouptut:
    INTEGER(i4b)      :: gridFileCount ! Number of grid files per file

    ! input:
    CHARACTER(LEN=*)  :: fileName      ! Filename

    ! Local variables
    TYPE(t_fileGrid)  :: gridHead
    INTEGER(i4b)      :: numGrid
    INTEGER(i4b)      :: irc,ios

IF (DEBUG) write(*,*) 'D_GRID:gridFileCount start'
    ! Init the variables
    gridHead%fileName = fileName
    gridHead%fileIdx  = 0

    ! Open the file for reading
    CALL opnfil(lfnloc,fileName,'OLD','FORMATTED','READONLY',' ',ios)
    CALL opnerr(lfnerr,lfnloc,ios,fileName,'D_GRID:gridFileCount')

    irc = 0
    numGrid = 0
    DO WHILE (irc == 0)
      CALL readGridHead(lfnloc,gridHead,irc)
      IF (irc /= 0) EXIT
      numGrid = numGrid+1
    ENDDO

    close(lfnloc)

    gridFileCount = numGrid
IF (DEBUG) write(*,*) 'D_GRID:gridFileCount ende',numGrid
  END FUNCTION gridFileCount


! -----------------------------------------------------------------------------
! Collects the information of a grid file
! (grid filenames are known in gridList(ii)%filename)
! -----------------------------------------------------------------------------
  SUBROUTINE gridFileInfo
    USE s_opnfil
    USE s_opnerr

    ! List of parameters
    ! none, uses the global module variable gridList

    ! Local variables
    TYPE(t_fileGrid)  :: gridFiles

    CHARACTER(LEN=fileNameLength) :: lastfile

    INTEGER(i4b)      :: iGrid
    INTEGER(i4b)      :: ios,irc

IF (DEBUG) write(*,*) 'D_GRID:gridFileInfo start'
    ! Init the variables
    lastfile = ''

    DO iGrid = 1, SIZE(gridList)

      ! Open the file for reading
      IF (gridList(iGrid)%filename /= lastfile) THEN
        IF (lastfile /= '') CLOSE(lfnloc)
        lastfile = gridList(iGrid)%filename
        CALL opnfil(lfnloc,lastfile,'OLD','FORMATTED','READONLY',' ',ios)
        CALL opnerr(lfnerr,lfnloc,ios,lastfile,'D_GRID:gridFileInfo')
      ENDIF

      ! Read the next grid header in the file
      CALL readGridHead(lfnloc,gridList(iGrid),irc)

      ! No more headers in this file
      IF (iGrid == SIZE(gridList)) THEN
        close(lfnloc)
        lastfile = ''
      ENDIF
    ENDDO

    close(lfnloc)

IF (DEBUG) write(*,*) 'D_GRID:gridFileInfo ende'
  END SUBROUTINE gridFileInfo


! -----------------------------------------------------------------------------
! Seeks the pointer of the grid file to a certain position
! (given by gridHead%fileIdx) or gives back the next header in file
! (gridHead%fileIdx == 0)
! -----------------------------------------------------------------------------
  SUBROUTINE readGridHead(lfn,gridHead,irc)
    USE d_const, ONLY: RHO
    USE s_opnfil
    USE s_opnerr
    USE s_upperc
    USE s_splarg
    USE s_exitrc
    USE f_djul

    ! List of parameters
    ! input/ouptut:
    TYPE(t_fileGrid)       :: gridHead     ! Content of the file

    ! input:
    INTEGER(i4b)           :: lfn          ! logical filenumber
    INTEGER(i4b)           :: iGrid        ! grid index to which we need to go
                                           ! 0: read through all
    INTEGER(i4b)           :: irc          ! return code 1: record not found,
                                           !             0: record found

    ! Local variables
    TYPE(t_fileGrid)       :: gridFiles

    CHARACTER(LEN= 1)      :: check
    CHARACTER(LEN=20)      :: key
    CHARACTER(LEN=60)      :: val
    CHARACTER(LEN=60)      :: hlp
    CHARACTER(LEN=20),      &
      DIMENSION(:),POINTER :: lst
    INTEGER(i4b)           :: numGrid
    INTEGER(i4b)           :: i1,i2,ii
    INTEGER(i4b)           :: yyyy,mm,dd,hh,min
    INTEGER(i4b)           :: ios
    REAL(r8b)              :: day,sec
    LOGICAL, SAVE          :: msgPrt = .FALSE.

IF (DEBUG) write(*,*) 'D_GRID:readGridHead start',gridHead%fileIdx
    ! Init the variables
    irc = 1
    numGrid = 0
    NULLIFY(lst)

    IF (gridHead%fileIdx /= 0) REWIND(lfn)

    DO
      READ(lfn,'(A,1X,A20,A)',iostat=ios) check,key,val
      IF (ios /= 0) EXIT

      IF (check /= '!') CYCLE

      IF (TRIM(key) == 'Data_types:') THEN
        numGrid = numGrid+1
        IF (gridHead%fileIdx /= 0 .AND. gridHead%fileIdx /= numGrid) CYCLE
        IF ( gridHead%fileIdx == 0 ) THEN
          irc = 0
          EXIT
        ENDIF
        IF ( gridHead%fileIdx < numGrid ) EXIT
        READ(val,*) gridHead%fileType
        gridHead%fileIdx = numGrid
        i1=index(val,'(')+1
        i2=index(val,')')-1
        hlp = val(i1:i2)
        CALL UPPERC(hlp)
        CALL SPLARG(hlp,lst)
        i1 = 0
        i2 = 0
        DO ii = 3,SIZE(lst)
          CALL checkGridHead(i1,i2,ii-2,gridHead%fileType,lst(ii),grd_vmf1_ah)
          CALL checkGridHead(i1,i2,ii-2,gridHead%fileType,lst(ii),grd_vmf1_aw)
          CALL checkGridHead(i1,i2,ii-2,gridHead%fileType,lst(ii),grd_vmf1_zh)
          CALL checkGridHead(i1,i2,ii-2,gridHead%fileType,lst(ii),grd_vmf1_zw)

          CALL checkGridHead(i1,i2,ii-2,gridHead%fileType,lst(ii),grd_aload_u)
          CALL checkGridHead(i1,i2,ii-2,gridHead%fileType,lst(ii),grd_aload_n)
          CALL checkGridHead(i1,i2,ii-2,gridHead%fileType,lst(ii),grd_aload_e)

          CALL checkGridHead(i1,i2,ii-2,gridHead%fileType,lst(ii),grd_ontld_u)
          CALL checkGridHead(i1,i2,ii-2,gridHead%fileType,lst(ii),grd_ontld_n)
          CALL checkGridHead(i1,i2,ii-2,gridHead%fileType,lst(ii),grd_ontld_e)

          CALL checkGridHead(i1,i2,ii-2,gridHead%fileType,lst(ii),grd_hload_u)
          CALL checkGridHead(i1,i2,ii-2,gridHead%fileType,lst(ii),grd_hload_n)
          CALL checkGridHead(i1,i2,ii-2,gridHead%fileType,lst(ii),grd_hload_e)
        ENDDO
        DEALLOCATE(lst)
        IF (i2 == 0 .AND. .NOT.msgPrt) THEN
          WRITE(lfnerr,'(/,A,3(/,15X,A))')                                     &
            ' *** SR D_GRID/readGridHead: Unexpected type identification '//   &
            'found in the ',                                                   &
            '"Data_types" field of grid type: "'//TRIM(gridHead%fileType)//'". ',&
            'The grid file is not considered in the processing!',              &
            'Only the following gridy types are supported:   "'//maxVal(1)%head//'"'
          hlp = ''
          DO ii = 2,SIZE(maxVal)
            hlp = TRIM(hlp) // '  "' // maxVal(ii)%head // '"'
            IF (LEN_TRIM(hlp) > 50) THEN
              WRITE(lfnerr,'(13X,A)') TRIM(hlp)
              hlp = ''
            ENDIF
          ENDDO
          IF (LEN_TRIM(hlp) > 0) WRITE(lfnerr,'(13X,A)') TRIM(hlp)
          WRITE(lfnerr,*)
          msgPrt = .TRUE.
        ELSE
          DO ii = 1,SIZE(maxval)
            IF (maxVal(ii)%head == gridHead%fileType .AND. &
                maxVal(ii)%indx /= i1) THEN
              WRITE(lfnerr,'(/,A,/,15X,A,2(/,15X,A,I5),/)')             &
                ' *** SR D_GRID/readGridHead: Unexpected number of '//  &
                'components found in the ',                             &
                '"Data_types" field of grid type "'                     &
                                        //TRIM(maxVal(ii)%head)//'". ', &
                'Number of expected components:',maxVal(ii)%indx,       &
                'Number of detected components:',i1
              CALL exitrc(2)
            ENDIF
          ENDDO
        ENDIF
        irc = 0
      ENDIF
      IF (gridHead%fileIdx /= 0 .AND. gridHead%fileIdx /= numGrid) CYCLE
      IF (TRIM(key) == 'Epoch:') THEN
        READ(val,*) yyyy,mm,dd,hh,min,sec
        day = DBLE(dd) + (DBLE(hh) + (DBLE(min) + sec/60d0) / 60d0 ) /24d0
        gridHead%epoch = DJUL(yyyy,mm,day)
      ENDIF
      IF (TRIM(key) == 'Range/resolution:') THEN
        READ(val,*) gridHead%minPhi,  gridHead%maxPhi, &
                    gridHead%minLam,  gridHead%maxLam, &
                    gridHead%stepPhi, gridHead%stepLam
        gridHead%minPhi  = gridHead%minPhi  / RHO
        gridHead%maxPhi  = gridHead%maxPhi  / RHO
        gridHead%minLam  = gridHead%minLam  / RHO
        gridHead%maxLam  = gridHead%maxLam  / RHO
        gridHead%stepPhi = gridHead%stepPhi / RHO
        gridHead%stepLam = gridHead%stepLam / RHO
IF ( gridHead%fileIdx > 0 .AND. gridHead%fileIdx == numGrid ) EXIT
      ENDIF

      IF (TRIM(key) == 'Scale_factor:') THEN
        READ(val,*) gridHead%scale
      ENDIF

    ENDDO

IF (DEBUG) write(*,*) 'D_GRID:readGridHead ende'
  END SUBROUTINE readGridHead


! -----------------------------------------------------------------------------
! Checks the order of components in the grids
! -----------------------------------------------------------------------------
  SUBROUTINE checkGridHead(i1,i2,ii,fileType,component,gridType)
    USE s_exitrc

    ! List of parameters
    ! input:
    INTEGER(i4b)     :: i1  ! counter for components found per grid type
    INTEGER(i4b)     :: i2  ! check whether the header string was found
    INTEGER(i4b)     :: ii  ! index of the component in the list
    CHARACTER(LEN=*) :: fileType ! Header entry for the grid type
    CHARACTER(LEN=*) :: component ! Component identifier

    ! input/output:
    TYPE(t_gridType) :: gridType ! Grid type description

    IF (fileType  == gridType%head) THEN
      i2 = 1
      IF ( component == gridType%comp ) THEN
        i1=i1+1
        IF (gridType%indx == 0) THEN
          gridType%indx = ii
        ELSEIF (gridType%indx /= ii) THEN
          WRITE(lfnerr,'(/,A,2(/,15X,A),/)') &
            ' *** SR D_GRID/checkGridHead: The order of the components is '//  &
            'inconsistent between the ',                                       &
            '"Data_types" fields for grid type "'//TRIM(fileType)//'". ',      &
            'Because this is not supported the processing is stopped.'
          CALL exitrc(2)
        ENDIF
      ENDIF
    ENDIF
    END SUBROUTINE


! -----------------------------------------------------------------------------
! Collects the information of a grid file
! -----------------------------------------------------------------------------
  SUBROUTINE readGrid(myGrid,nGrid,epoch,new)
    USE d_const, ONLY: RHO
    USE s_opnfil
    USE s_opnerr
    USE f_djul

    ! List of parameters
    ! input/output:
    TYPE(t_oneGrid)            :: myGrid ! a grid of a type

    ! input:
    INTEGER(i4b)               :: nGrid  ! Number of grids
    REAL(r8b)                  :: epoch  ! Epoch request
    INTEGER(i4b), DIMENSION(:) :: new    ! index of all elements to be updated

    ! Local variables
    TYPE(t_fileGrid)                 :: thisGrid
    CHARACTER(LEN=FileNameLength)    :: fileName
    CHARACTER(LEN=shortLineLength)   :: line
    INTEGER(i4b), DIMENSION(nGrid+1) :: toBeRead
    INTEGER(i4b), DIMENSION(4)       :: idx
    INTEGER(i4b)                     :: iNew
    INTEGER(i4b)                     :: iFil
    INTEGER(i4b)                     :: iVal, nVal
    INTEGER(i4b)                     :: ii,jj
    INTEGER(i4b)                     :: ios, irc
    REAL(r8b)                        :: xLat, xLon
    REAL(r8b), DIMENSION(4)          :: value  ! VMF1 has 4 values
    LOGICAL                          :: doRead
    LOGICAL                          :: sorted

IF (DEBUG) write(*,*) 'D_GRID: readGrid start ',epoch

! Find out which grids need to be read to this update
! ---------------------------------------------------
    toBeRead = 0
    DO iNew = 1,nGrid+1
      IF (new(iNew) == 0) EXIT

      ! Check all cells
      DO ii = 1,2
        IF ( myGrid%cells(1,new(iNew))%coeff(1,1,ii) /= 1D20 ) CYCLE

        DO jj = 1,nGrid+1
          IF (toBeRead(jj) == myGrid%cells(1,new(iNew))%source(ii)) EXIT
          IF (toBeRead(jj) == 0) THEN
            toBeRead(jj) = myGrid%cells(1,new(iNew))%source(ii)
            EXIT
          ENDIF
        ENDDO
      ENDDO
    ENDDO

! Sort the request for reading
! ----------------------------
    sorted = .FALSE.
    DO WHILE (.NOT. sorted)
      sorted = .TRUE.
      DO iNew = 1,nGrid-1
        IF (toBeRead(iNew+1) == 0) EXIT
        IF ((gridList(toBeRead(iNew))%filename >  gridList(toBeRead(iNew+1))%fileName) .OR. &
            (gridList(toBeRead(iNew))%filename == gridList(toBeRead(iNew+1))%fileName .AND.&
             gridList(toBeRead(iNew))%fileIdx  >  gridList(toBeRead(iNew+1))%fileIdx)) THEN
          sorted = .FALSE.
          jj = toBeRead(iNew)
          toBeRead(iNew) = toBeRead(iNew+1)
          toBeRead(iNew+1) = jj
        ENDIF
      ENDDO
    ENDDO

! Read the necessary list of files
! --------------------------------
    fileName = ''
    DO iFil = 1,nGrid+1
      IF (toBeRead(iFil) == 0) EXIT

      IF (fileName /= gridList(toBeRead(iFil))%fileName) THEN
        IF (fileName /= '') CLOSE(lfnloc)

        ! Open the file for reading
        fileName = gridList(toBeRead(iFil))%fileName
        CALL opnfil(lfnloc,fileName,'OLD','FORMATTED','READONLY',' ',ios)
        CALL opnerr(lfnerr,lfnloc,ios,fileName,'D_GRID:readGrid')
      ENDIF

      ! Find the correct header/section of the grid file
      CALL readGridHead(lfnloc,gridList(toBeRead(iFil)),irc)

      ! Read the data part of the grid
      ios = 0
      DO WHILE (ios == 0)
        READ(lfnloc,'(A)',iostat=ios) line
        IF (ios /= 0) EXIT

        ! We are in a header section
        IF (line(1:13) == '! Data_types:') EXIT
        IF (line(1:1) == '!') CYCLE

        ! Read the data records
        nVal = get_numVal(myGrid%gridType)

        ! Read the values from the line
        READ(line,*) xLat, xLon, value(1:nVal)
        xLat = xLat / RHO
        xLon = xLon / RHO

        value(1:nVal) = value(1:nVal) * gridList(toBeRead(iFil))%scale

        ! Put the values into the array (if requested)
        idx = -1
        DO WHILE ( idx(1) /= 0 )
          idx = whichCoeff(myGrid,toBeRead(iFil),new,xLat,xLon,idx(1))
          IF (idx(1) == -1 ) THEN
            EXIT
          ELSE IF (idx(1) /= 0) THEN
            DO iVal = 1,nVal
              IF (idx(4) == 3) THEN
                myGrid%cells(iVal,idx(1))%coeff(idx(2),idx(3),1:2) = value(iVal)
              ELSE
                myGrid%cells(iVal,idx(1))%coeff(idx(2),idx(3),idx(4)) = value(iVal)
              ENDIF
            ENDDO
IF (DEBUG) write(*,*) xlat*RHO,xlon*RHO,gridList(toBeRead(iFil))%epoch,idx
IF (DEBUG) write(*,*) myGrid%cells(1,idx(1))%crdMin(:)*RHO,myGrid%cells(1,idx(1))%crdMax(:)*RHO,myGrid%cells(1,idx(1))%mjd(:)
IF (DEBUG) WRITE(*,*) myGrid%cells(1,idx(1))%coeff(:,:,:)
            IF ( myGrid%cells(1,idx(1))%coeff(1,1,1) /= 1d20 .AND. &
                 myGrid%cells(1,idx(1))%coeff(2,1,1) /= 1d20 .AND. &
                 myGrid%cells(1,idx(1))%coeff(1,2,1) /= 1d20 .AND. &
                 myGrid%cells(1,idx(1))%coeff(2,2,1) /= 1d20 .AND. &
                 myGrid%cells(1,idx(1))%coeff(1,1,2) /= 1d20 .AND. &
                 myGrid%cells(1,idx(1))%coeff(2,1,2) /= 1d20 .AND. &
                 myGrid%cells(1,idx(1))%coeff(1,2,2) /= 1d20 .AND. &
                 myGrid%cells(1,idx(1))%coeff(2,2,2) /= 1d20 ) THEN
              new(idx(1)) = -1
            ENDIF
            IF (idx(1) == SIZE(new)) EXIT  ! the end of the list
            IF (new(idx(1)+1) == 0)  EXIT  ! the last element of the list
IF (DEBUG) write(*,*) new(:)
          ENDIF
        ENDDO  ! Put one grid point into all cells where necessary
        IF (idx(1) == -1 ) EXIT
      ENDDO    ! Next record from the current grid file
    ENDDO      ! Read more grid files
    close(lfnloc)

IF (DEBUG) write(*,*) 'D_GRID: readGrid ende ',filename,epoch
  END SUBROUTINE readGrid


! -----------------------------------------------------------------------------
! Collects the information of a grid file
! -----------------------------------------------------------------------------
  FUNCTION whichCoeff(myGrid,iGrid,new,xLat,xLon,lIdx)
    USE d_const, ONLY: PI
use d_const, ONLY: rho
    ! List of parameters
    ! output:
    INTEGER(i4b), DIMENSION(4) :: whichCoeff ! array of index:
                                         ! 1: cell in myGrid
                                         ! 2: lat in cells%coeff
                                         ! 3: lon in cells%coeff
                                         ! 4: epo in cells%coeff
    ! input:
    TYPE(t_oneGrid)            :: myGrid ! a grid of a type
    INTEGER(i4b)               :: iGrid  ! index in grid list
    INTEGER(i4b), DIMENSION(:) :: new    ! index of all elements to be updated
    REAL(r8b)                  :: xLat   ! latitude  from grid file
    REAL(r8b)                  :: xLon   ! longitude from grid file
    INTEGER(i4b)               :: lIdx   ! index in new from the last call
                                         ! -1: first call

    ! Local variables
    INTEGER(i4b)               :: ii,jj,ll
    INTEGER(i4b), DIMENSION(4) :: idx
    REAL(r8b)                  :: dLat, dLon


IF (DEBUG) write(*,*) 'D_GRID: whichCoeff start',xlat*RHO,xlon*RHO,iGrid
    ! Start the loop after the last request
    ll = lIdx + 1
    IF (ll < 1) ll = 1

    ! Check whether all new elements are found
!    idx(1:4) = -1
    idx(1:4) = 0
    IF (ll == 1) idx(1:4) = -1
!
    DO ii = ll,SIZE(new)
      IF (new(ii) ==  0) EXIT   ! We are at the end of the list
      IF (new(ii) == -1) CYCLE  ! Update is done
      idx(1:4) = 0

      dLat = 1d-8*gridList(iGrid)%stepPhi
      dLon = 1d-8*gridList(iGrid)%stepLam

      DO jj = 1,2
        IF (myGrid%cells(1,new(ii))%source(jj) /= iGrid) CYCLE

        IF (xLat <= myGrid%cells(1,new(ii))%crdMin(1)+dLat .AND. &
            xLat >= myGrid%cells(1,new(ii))%crdMin(1)-dLat) idx(2) = 1
        IF (xLon <= myGrid%cells(1,new(ii))%crdMin(2)+dLon .AND. &
            xLon >= myGrid%cells(1,new(ii))%crdMin(2)-dLon) idx(3) = 1
        IF (xLon <= myGrid%cells(1,new(ii))%crdMin(2)-2d0*PI+dLon .AND. &
            xLon >= myGrid%cells(1,new(ii))%crdMin(2)-2d0*PI-dLon) idx(3) = 1

        IF (xLat <= myGrid%cells(1,new(ii))%crdMax(1)+dLat .AND. &
            xLat >= myGrid%cells(1,new(ii))%crdMax(1)-dLat) idx(2) = 2
        IF (xLon <= myGrid%cells(1,new(ii))%crdMax(2)+dLon .AND. &
            xLon >= myGrid%cells(1,new(ii))%crdMax(2)-dLon) idx(3) = 2
        IF (xLon <= myGrid%cells(1,new(ii))%crdMax(2)-2d0*PI+dLon .AND. &
            xLon >= myGrid%cells(1,new(ii))%crdMax(2)-2d0*PI-dLon) idx(3) = 2

        idx(4) = idx(4) + jj
      ENDDO

      IF (idx(2) /= 0 .AND. idx(3) /= 0 .AND. idx(4) /= 0) THEN
        idx(1) = ii
!        new(ii) = -1
        EXIT
      ENDIF
    ENDDO

    whichCoeff = idx
IF (DEBUG) write(*,*) 'D_GRID: whichCoeff ende',idx
  END FUNCTION whichCoeff

! -----------------------------------------------------------------------------
! Find out which grid is requested
! -----------------------------------------------------------------------------
  FUNCTION whichGrid(gridKey)

    ! List of parameters
    ! ouptut:
    INTEGER(i4b)              :: whichGrid  ! index in list of allGrids

    ! input:
    CHARACTER(LEN=*)          :: gridKey    ! a grid-type

    ! Local variables
    INTEGER(i4b)      :: iGrd
    INTEGER(i4b)      :: ii

IF (DEBUG) write(*,*) 'D_GRID: whichGrid start'
    ! Init the variables
    iGrd = 0

    ! Loop all grid cells
    DO ii = 1,SIZE(allGrids)
      IF ( gridKey == TRIM(allGrids(ii)%gridType) ) THEN
        iGrd = ii
        EXIT
      ENDIF
    ENDDO

    whichGrid = iGrd
IF (DEBUG) write(*,*) 'D_GRID: whichGrid ende',iGrd
  END FUNCTION whichGrid


! -----------------------------------------------------------------------------
! Find out which element in the cell-list fits for the station coordinates
! -----------------------------------------------------------------------------
  FUNCTION whichCell(myGrid,coord)
use d_const, ONLY: rho
    USE d_const, ONLY: PI
    ! List of parameters
    ! ouptut:
    INTEGER(i4b)            :: whichCell    ! index in list of cells

    ! input:
    TYPE(t_oneGrid)         :: myGrid       ! a grid of a type
    REAL(r8b), DIMENSION(3) :: coord        ! Station coordinates
                                            ! 1: lat in radiant
                                            ! 2: lon in radiant
                                            ! 3: hgt in meter
    ! Local variables
    INTEGER(i4b)         :: ii
    INTEGER(i4b)         :: iCell
    INTEGER(i4b), SAVE   :: lCell = 0
    INTEGER(i4b), SAVE   :: lGrid = 0
    REAL(r8b)            :: dLat, dLon, hlon
    REAL(r8b),    SAVE   :: lLat = 1d20
    REAL(r8b),    SAVE   :: lLon = 1d20

IF (DEBUG) write(*,*) 'D_GRID: whichCell start',coord(1:2)*RHO
    ! Init the variables
    iCell = 0

    ! When processing multiple input grids (VMF+ATM+...) the type of the
    ! grid might have changed when calling this subroutine
    IF ( lCell /= 0 .AND. lLat == coord(1) .AND. lLon == coord(2) .AND. &
         lGrid == whichGrid(myGrid%gridType) ) THEN
      iCell = lCell

    ELSE

      ! Loop all grid cells
      DO ii = 1,SIZE(myGrid%cells,2)
        IF ( myGrid%cells(1,ii)%mjd(1) == 0d0 ) CYCLE
        hlon = coord(2)
        IF ( hlon < gridList(myGrid%cells(1,ii)%source(1))%minLam ) hlon = hlon + 2 * PI
IF (DEBUG) write(*,*) 'D_GRID: whichCell check',ii,coord(1)*RHO,hlon*RHO
        dLat = myGrid%cells(1,ii)%crdMax(1) - myGrid%cells(1,ii)%crdMin(1)
        dLon = myGrid%cells(1,ii)%crdMax(2) - myGrid%cells(1,ii)%crdMin(2)
        IF ( myGrid%cells(1,ii)%crdMin(1)-1d-8*dLat-0.5d0*dLat <= coord(1) .AND. &
             myGrid%cells(1,ii)%crdMax(1)+1d-8*dLat+0.5d0*dLat >= coord(1) .AND. &
             myGrid%cells(1,ii)%crdMin(2)-1d-8*dLon <= hlon .AND. &
             myGrid%cells(1,ii)%crdMax(2)+1d-8*dLon >= hlon ) THEN
          iCell = ii
          lCell = ii
          lLat  = coord(1)
          lLon  = coord(2)
          lGrid = whichGrid(myGrid%gridType)
          EXIT
        ENDIF
      ENDDO
    ENDIF

    whichCell = iCell
IF (DEBUG) write(*,*) 'D_GRID: whichCell ende',iCell
  END FUNCTION whichCell


! -----------------------------------------------------------------------------
! Find out which file is required for a coordinate/epoch/grid type
! -----------------------------------------------------------------------------
  FUNCTION whichFile(myGrid,coord,epoch)
    USE d_const, ONLY: RHO
    USE s_timst2
    USE s_exitrc

    ! List of parameters
    ! ouptut:
    INTEGER(i4b), DIMENSION(2) :: whichFile    ! index in list of files
                                               ! left and right epoch

    ! input:
    TYPE(t_oneGrid)            :: myGrid       ! a grid of a type
    REAL(r8b), DIMENSION(3)    :: coord        ! Station coordinates
                                               ! 1: lat in radiant
                                               ! 2: lon in radiant
                                               ! 3: hgt in meter
    REAL(r8b)                  :: epoch        ! epoch of the request

    ! Local variables
    CHARACTER(LEN=timStrgLength) :: epoStr
    INTEGER(i4b)                 :: iFil
    INTEGER(i4b), DIMENSION(2)   :: jFil
    INTEGER(i4b), DIMENSION(2), SAVE :: jFil_sav = (/ 0,0 /)

IF (DEBUG) write(*,*) 'D_GRID: whichFile start',epoch
    ! Init the variables
    jFil(:) = 0

    ! Loop all grid files
    DO iFil = 1,SIZE(gridList)

      IF (myGrid%gridType == gridList(iFil)%fileType .AND. &
          coord(1)  >= gridList(iFil)%minPhi-1.d-8*gridList(iFil)%stepPhi &
                                            -0.5d0*gridList(iFil)%stepPhi .AND. &
          coord(1)  <= gridList(iFil)%maxPhi+1.d-8*gridList(iFil)%stepPhi &
                                            +0.5d0*gridList(iFil)%stepPhi .AND. &
          coord(2)  >= gridList(iFil)%minLam-1.d-8*gridList(iFil)%stepLam &
                                            -0.5d0*gridList(iFil)%stepLam .AND. &
          coord(2)  <= gridList(iFil)%maxLam+1.d-8*gridList(iFil)%stepLam &
                                            +0.5d0*gridList(iFil)%stepLam) THEN

        IF (jFil(1) == 0) THEN
          jFil(1) = iFil
          CYCLE
        ELSE IF (jFil(2) == 0) THEN
          jFil(2) = iFil
        ENDIF

        IF ( epoch >= gridList(jFil(1))%epoch-0.1d0/86400d0 .AND. &
             epoch <= gridList(jFil(2))%epoch+0.1d0/86400d0 ) THEN
          IF ( epoch >= gridList(iFil)%epoch-0.1d0/86400d0 .AND. &
               epoch <= gridList(jFil(2))%epoch+0.1d0/86400d0 .AND. &
               gridList(jFil(2))%epoch-gridList(iFil)%epoch < &
                  gridList(jFil(2))%epoch-gridList(jFil(1))%epoch) THEN
            jFil(1) = iFil
          ELSEIF ( epoch >= gridList(jFil(1))%epoch-0.1d0/86400d0 .AND. &
               epoch <= gridList(iFil)%epoch+0.1d0/86400d0 .AND. &
               gridList(iFil)%epoch-gridList(jFil(1))%epoch < &
                  gridList(jFil(2))%epoch-gridList(jFil(1))%epoch) THEN
            jFil(2) = iFil
          ENDIF

        ELSE IF ( epoch >= gridList(jFil(2))%epoch+0.1d0/86400d0 .AND. &
          gridList(iFil)%epoch-0.1d0/86400d0 >= gridList(jFil(2))%epoch+0.1d0/86400d0) THEN
          jFil(1) = jFil(2)
          jFil(2) = iFil

        ELSE IF ( epoch <= gridList(jFil(1))%epoch-0.1d0/86400d0 .AND. &
          gridList(iFil)%epoch+0.1d0/86400d0 <= gridList(jFil(1))%epoch-0.1d0/86400d0) THEN
          jFil(2) = jFil(1)
          jFil(1) = iFil

        ENDIF

IF (DEBUG) &
write(*,*) 'D_GRID: whichFile:',iFil,SIZE(gridList),jFil,gridList(iFil)%epoch,gridList(jFil(1))%epoch,gridList(jFil(2))%epoch
      ENDIF
    ENDDO

    ! No grid files found for the request
    IF (jFil(1) == 0 .OR. jFil(2) == 0) THEN
      CALL timst2(2,1,epoch,epoStr)
      WRITE(lfnerr,'(/,A,3(/,15X,A),2F9.3,/,15X,A,/)')                &
      ' *** SR D_GRID/whichFile: ' // &
                    'At least two grids are needed ',                 &
                    'for the following request:',                     &
                    'Type of the grid file: ' // myGrid%gridType,     &
                    'Station coordinates:    ',coord(1:2) * RHO,      &
                    'Epoch:                 ' // epoStr
      CALL exitrc(2)
    ENDIF

    ! Extrapolation of the grid interval
    IF (jFil(1) /= jFil(2) .AND.                                      &
        1.5d0*(gridList(jFil(2))%epoch - gridList(jFil(1))%epoch) <   &
        DMIN1( DABS(epoch - gridList(jFil(2))%epoch) ,                &
               DABS(gridList(jFil(1))%epoch - epoch) )) THEN
      CALL timst2(2,1,epoch,epoStr)
      WRITE(lfnerr,'(/,A,/,15X,A,/,15X,A)') &
      ' *** SR D_GRID/whichFile: ' // &
                    'The request is out of the data interval:', &
                    'Type of the grid file: ' // myGrid%gridType,   &
                    'Epoch:                 ' // epoStr
      IF (DABS(epoch - gridList(jFil(2))%epoch)<DABS(gridList(jFil(1))%epoch - epoch)) THEN
        CALL timst2(2,1,gridList(jFil(2))%epoch,epoStr)
        WRITE(lfnerr,'(15X,A,/)') &
                    'End of the grid        ' // epostr
      ELSE
        CALL timst2(2,1,gridList(jFil(1))%epoch,epoStr)
        WRITE(lfnerr,'(15X,A,/)') &
                    'Begin of the grid      ' // epostr
      ENDIF
      CALL exitrc(2)
    ENDIF

    ! Extrapolation of the grid interval
    IF (jFil(1) /= jFil(2) .AND.                                      &
        gridList(jFil(2))%epoch - gridList(jFil(1))%epoch <           &
        DMIN1( DABS(epoch - gridList(jFil(2))%epoch) ,                &
               DABS(gridList(jFil(1))%epoch - epoch) )) THEN
      IF (jFil_sav(1) == 0 .OR. jFil_sav(1) /= jFil(1)) THEN
        CALL timst2(2,1,epoch,epoStr)
        WRITE(lfnerr,'(/,A,/,15X,A,/,15X,A)') &
        ' ### SR D_GRID/whichFile: ' // &
                      'The request is an extrapolation:', &
                      'Type of the grid file: ' // myGrid%gridType,   &
                      'Epoch:                 ' // epoStr
        IF (DABS(epoch - gridList(jFil(2))%epoch)<DABS(gridList(jFil(1))%epoch - epoch)) THEN
          CALL timst2(2,1,gridList(jFil(2))%epoch,epoStr)
          WRITE(lfnerr,'(15X,A,/)') &
                      'End of the grid        ' // epostr
        ELSE
          CALL timst2(2,1,gridList(jFil(1))%epoch,epoStr)
          WRITE(lfnerr,'(15X,A,/)') &
                      'Begin of the grid      ' // epostr
        ENDIF
      ENDIF
      jFil_sav = jFil
    ENDIF

    whichFile = jFil
IF (DEBUG) write(*,*) 'D_GRID: whichFile ende',jFil,gridList(jFil(1))%epoch,gridList(jFil(2))%epoch
  END FUNCTION whichFile


! -----------------------------------------------------------------------------
! Find out how many values per record are given for this grid type
! -----------------------------------------------------------------------------
  FUNCTION get_numVal(gridType)
    USE s_exitrc
    ! List of parameters
    ! input
    CHARACTER(LEN=*)        :: gridType     ! Name of the grid in the header
    !output
    INTEGER(i4b)            :: get_numVal   ! Number of values for this type

    ! Local variables
    INTEGER(i4b)            :: ii

    ! Find the entry in the maxval record
    ii = 0
    DO ii = 1,SIZE(maxVal)
      IF (gridType == maxVal(ii)%head) THEN
        get_numVal = maxVal(ii)%indx
        EXIT
      ENDIF
    ENDDO

    ! Keyword not found in maxval
    IF (ii == 0) THEN
      WRITE(lfnerr,'(/,A,3(/,15X,A),/)')                              &
      ' *** SR D_GRID/get_numVal: ' //                                &
        'No dimension for the number of data columns for the',        &
        'grid type "'//gridType//'"found.',                           &
        'Please check the entries for this grid type in the module',  &
        '"${I}/D_GRID.f90" and recompile the software if necessary.'
      CALL exitrc(2)
    ENDIF

  END FUNCTION get_numVal

! -----------------------------------------------------------------------------
! Find out which element in the cell-list fits for the station coordinates
! -----------------------------------------------------------------------------
  SUBROUTINE addToGrid(myGrid,coord,epoch)
    use d_const, ONLY: PI
use d_const, ONLY: RHO
    ! List of parameters
    ! input/output:
    TYPE(t_oneGrid)         :: myGrid       ! a grid of a type
    !input
    REAL(r8b), DIMENSION(3) :: coord        ! Station coordinates
                                            ! 1: lat in radiant
                                            ! 2: lon in radiant
                                            ! 3: hgt in meter
    REAL(r8b)               :: epoch        ! epoch of request

    ! Local variables
    INTEGER(i4b)               :: next
    INTEGER(i4b)               :: ii,jj
    INTEGER(i4b), DIMENSION(2) :: iFil       ! Left and right epoch
    INTEGER(i4b)               :: iPhi, iLam

IF (DEBUG) write(*,*) 'D_GRID: addToGrid start',coord(1:2)*RHO,epoch
    ! Where goes the next entry?
    next = myGrid%lstUpd + 1
    IF ( next > SIZE(myGrid%cells,2) ) next = 1

    ! Find the file for the requested grid type
    iFil = whichFile(myGrid,coord,epoch)

    DO ii = 1,2

      ! Compute the requested latitude bin
      iPhi = INT((coord(1)-gridList(iFil(ii))%minPhi)/gridList(iFil(ii))%stepPhi)
      myGrid%cells(1,next)%crdMin(1) = gridList(iFil(ii))%minPhi + &
                                       DBLE(iPhi)*gridList(iFil(ii))%stepPhi
      myGrid%cells(1,next)%crdMax(1) = gridList(iFil(ii))%minPhi + &
                                       DBLE(iPhi+1)*gridList(iFil(ii))%stepPhi
      IF (myGrid%cells(1,next)%crdMax(1)*RHO >  90d0) THEN
        myGrid%cells(1,next)%crdMin(1) = myGrid%cells(1,next)%crdMin(1) - gridList(iFil(ii))%stepPhi
        myGrid%cells(1,next)%crdMax(1) = myGrid%cells(1,next)%crdMax(1) - gridList(iFil(ii))%stepPhi
      ENDIF
      IF (myGrid%cells(1,next)%crdMin(1)*RHO < -90d0) THEN
        myGrid%cells(1,next)%crdMin(1) = myGrid%cells(1,next)%crdMin(1) + gridList(iFil(ii))%stepPhi
        myGrid%cells(1,next)%crdMax(1) = myGrid%cells(1,next)%crdMax(1) + gridList(iFil(ii))%stepPhi
      ENDIF

      ! Compute the requested longitude bin
      IF (coord(2)<gridList(iFil(ii))%minLam) coord(2)=coord(2)+2*PI
      iLam = INT((coord(2)-gridList(iFil(ii))%minLam)/gridList(iFil(ii))%stepLam)
      myGrid%cells(1,next)%crdMin(2) = gridList(iFil(ii))%minLam + &
                                       DBLE(iLam)*gridList(iFil(ii))%stepLam
      myGrid%cells(1,next)%crdMax(2) = gridList(iFil(ii))%minLam + &
                                       DBLE(iLam+1)*gridList(iFil(ii))%stepLam

      myGrid%cells(1,next)%source(ii)    = iFil(ii)
      myGrid%cells(1,next)%mjd (ii)      = gridList(iFil(ii))%epoch
      myGrid%cells(1,next)%coeff(:,:,ii) = 1d20
      myGrid%lstUpd                      = next

      ! Update in the multi-dimensional case
      DO jj = 2,myGrid%numDim
        myGrid%cells(jj,next)%crdMin = myGrid%cells(1,next)%crdMin
        myGrid%cells(jj,next)%crdMax = myGrid%cells(1,next)%crdMax
        myGrid%cells(jj,next)%mjd    = myGrid%cells(1,next)%mjd
        myGrid%cells(jj,next)%coeff  = myGrid%cells(1,next)%coeff
      ENDDO
    ENDDO

IF (DEBUG) write(*,*) 'D_GRID: addToGrid ende'
  END SUBROUTINE addToGrid


! -----------------------------------------------------------------------------
! Find out which element in the cell-list fits for the station coordinates
! -----------------------------------------------------------------------------
  SUBROUTINE updateGrid(myGrid,coord,epoch,nGrid)
use d_const, only: rho
    ! List of parameters
    ! input/output:
    TYPE(t_oneGrid)         :: myGrid       ! a grid of a type
    !input
    REAL(r8b), DIMENSION(3) :: coord        ! Station coordinates
                                            ! 1: lat in radiant
                                            ! 2: lon in radiant
                                            ! 3: hgt in meter
    REAL(r8b)               :: epoch        ! epoch of request
    INTEGER(i4b)            :: nGrid        ! size of the grid buffer

    ! Local variables
    INTEGER(i4b)                     :: ii,jj
    INTEGER(i4b)                     :: nNew
    INTEGER(i4b), DIMENSION(nGrid+1) :: new
    INTEGER(i4b)                     :: iGrd
    INTEGER(i4b), DIMENSION(2)       :: iFil

IF (DEBUG) write(*,*) 'D_GRID: updateGrid start', epoch
    nNew = 0
    new  = 0
    DO iGrd = 1,nGrid
IF (DEBUG) write(*,*) 'mjd,coeff:',iGrd,nGrid,myGrid%cells(1,iGrd)%mjd(1:2),myGrid%cells(1,iGrd)%coeff(1,1,1:2)
      IF ( myGrid%cells(1,iGrd)%mjd(1) == 0d0 ) CYCLE
      IF ( myGrid%cells(1,iGrd)%coeff(1,1,1) == 1d20  .OR. &
           myGrid%cells(1,iGrd)%coeff(1,1,2) == 1d20  .OR. &
           myGrid%cells(1,iGrd)%mjd(1)-1d0/86400d0 > epoch .OR. &
           myGrid%cells(1,iGrd)%mjd(2)+1d0/86400d0 < epoch ) THEN

        ! Update the epochs
        IF ( myGrid%cells(1,iGrd)%mjd(1)-1d0/86400d0 > epoch .OR. &
             myGrid%cells(1,iGrd)%mjd(2)+1d0/86400d0 < epoch ) THEN

          ! Find the file for the requested grid type
          iFil = whichFile(myGrid,coord,epoch)

          ! Check where we need another file
          DO ii = 1,2
            IF ( iFil(ii) /= myGrid%cells(1,iGrd)%source(ii) ) THEN
              myGrid%cells(1,iGrd)%source(ii)    = iFil(ii)
              myGrid%cells(1,iGrd)%mjd (ii)      = gridList(iFil(ii))%epoch
              myGrid%cells(1,iGrd)%coeff(:,:,ii) = 1d20

              ! Update in the multi-dimensional case
              DO jj = 2,myGrid%numDim
                myGrid%cells(jj,iGrd)%mjd    = myGrid%cells(1,iGrd)%mjd
                myGrid%cells(jj,iGrd)%coeff(:,:,ii)  = myGrid%cells(1,iGrd)%coeff(:,:,ii)
              ENDDO
            ENDIF
          ENDDO

        ENDIF
IF (DEBUG) write(*,*) igrd,ngrid,mygrid%cells(1,igrd)%crdMin*RHO,mygrid%cells(1,igrd)%crdMax*RHO,mygrid%cells(1,igrd)%mjd, &
           mygrid%cells(1,igrd)%coeff(:,1,1)
        nNew = nNew + 1
        new(nNew) = iGrd
      ENDIF
    ENDDO

    IF (nNew > 0) THEN
      CALL readGrid(myGrid,nGrid,epoch,new)
    ENDIF
IF (DEBUG) write(*,*) 'D_GRID: updateGrid ende'
  END SUBROUTINE updateGrid

! -----------------------------------------------------------------------------
! Get the interpolated value from the grid
! -----------------------------------------------------------------------------
  FUNCTION interpolate(myCell, coord, epoch)
    USE d_const, ONLY: rho, PI
    USE s_timst2
    USE s_exitrc

    ! List of Parameters
    ! output:
    REAL(r8b)                      :: interpolate

    ! input:
    TYPE(t_gridCell)               :: myCell  ! Grid element
    REAL(r8b), DIMENSION(2)        :: coord   ! 1,2: lat,lon in radiant
    REAL(r8b)                      :: epoch   ! epoch of the request


    ! Local variables
    CHARACTER(LEN=timStrgLength)   :: epoStr
    INTEGER(i4b)                   :: ii,jj
    REAL(r8b)                      :: fact
    REAL(r8b), DIMENSION(2,2)      :: xlat
    REAL(r8b), DIMENSION(2)        :: xlon
    REAL(r8b)                      :: coordlng

IF (DEBUG) write(*,*) 'D_GRID: interpolate start'

    ! Stop if invalid coeff are in use
    DO ii = 1,2
      DO jj = 1,2
        IF ( myCell%coeff(1,ii,jj) == 1d20 .OR. &
             myCell%coeff(2,ii,jj) == 1d20 ) THEN
          CALL timst2(2,1,epoch,epoStr)
          WRITE(lfnerr,'(/,A,2(/,15X,A),2F9.3,/,15X,A,/)')        &
          ' *** SR D_GRID/interpolate: ' // &
          'The specified grids are not sufficient',               &
          'to interpolate the values for the following request:', &
          'Station coordinates:    ',coord(1:2) * RHO,            &
          'Epoch:                 ' // epoStr
          WRITE(lfnerr,*) myCell%mjd(1:2)
          WRITE(lfnerr,*) myCell%crdMin(1:2)* RHO, myCell%crdMax(1:2)* RHO
          WRITE(lfnerr,*) ii,jj,myCell%coeff(1:2,1:2,1:2)
          CALL exitrc(2)
        ENDIF
      ENDDO
    ENDDO

    ! Compute the interplation for latitude
    fact = ( coord(1) - myCell%crdMin(1) ) / &
           ( myCell%crdMax(1) - myCell%crdMin(1) )

    DO ii = 1,2
      DO jj = 1,2
        xlat(ii,jj) = myCell%coeff(1,ii,jj) + &
                      fact * ( myCell%coeff(2,ii,jj) - myCell%coeff(1,ii,jj))
      ENDDO
    ENDDO

    coordlng=coord(2)
    IF (coordlng < gridList(1)%minLam) coordlng=coordlng+2*PI
    ! Compute the interplation for longitude

    fact = ( coordlng - myCell%crdMin(2) ) / &
           ( myCell%crdMax(2) - myCell%crdMin(2) )

    DO ii = 1,2
      xlon(ii) = xlat(1,ii) + fact * ( xlat(2,ii) - xlat(1,ii))
    ENDDO


    ! Compute the interplation in Time
    IF (DABS( myCell%mjd(1) - myCell%mjd(2) ) > 0.1d0/86400d0 ) THEN
      fact = ( epoch - myCell%mjd(1) ) / &
             ( myCell%mjd(2) - myCell%mjd(1) )
    ELSE
      fact = 0d0
    ENDIF
    interpolate = xlon(1) + fact * ( xlon(2) - xlon(1) )
IF (DEBUG) write(*,*) 'D_GRID: interpolate ende'
!write(*,*)
!write(*,*) 'D_GRID: interpolate ',epoch,xlon(1) + fact * ( xlon(2) - xlon(1) )
!write(*,*) coord*rho,myCell%crdMin*rho,myCell%crdMax*rho,myCell%source
!write(*,*) 'xlon(1) xlon(2)', xlon(1),  xlon(2)
!write(*,*) myCell%mjd(1),myCell%coeff(1:2,1:2,1)
!write(*,*) myCell%mjd(2),myCell%coeff(1:2,1:2,2)
  END FUNCTION interpolate


END MODULE d_grid
