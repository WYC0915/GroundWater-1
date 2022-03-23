
! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.2
! -------------------------------------------------------------------------

PROGRAM snx2slr

! ----------------------------------------------------------------------------
! Purpose:    This program converts data handling information for SLR
!             observations given in a SINEX file into:
!             1) STACRUX file: Exclusion of data
!             2) SLR correction file: Biases (apriori and estimation)
!
! Author:     D. Thaller
!
! Created:    29-Sep-2009
!
! Changes:    23-Sep-2010 RD: Enable CPU counter
!             14-Jun-2011 KS: Read and save exclusions in .STA files
!             16-Jun-2011 KS: Bug with open windows fixed
!             02-Dec-2011 SL: new title string for pritit, m_bern with ONLY
!             09-Jan-2012 KS: Minimum int. length for station exclusion from panel
!             27-Apr-2012 RD: Nullify pointers, remove unused variables
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! ----------------------------------------------------------------------------

  USE m_bern,   ONLY: i4b, r8b, fileNameLength, keyValueLength, lfnLoc, lfnErr
  USE m_cpu,    ONLY: cpu_start
  USE m_maxdim, ONLY: maxsta
  USE m_global, ONLY: g_svnsys
  USE m_epoch,  ONLY: ASSIGNMENT(=)
  USE d_inpkey, ONLY: inpKey, init_inpkey
  USE d_stacrx, ONLY: t_stacrux, init_stacrux
  USE d_rgbfil, ONLY: t_slrInfo, init_slrFil, nDataFlg, dataSNX, typBSW, &
                      flagBSW, wtSlrFil

  USE s_readrgb
  USE s_readinpf
  USE s_opnsys
  USE s_defcon
  USE s_pritit
  USE s_gtflna
  USE s_prflna
  USE s_opnfil
  USE s_opnerr
  USE s_exitrc
  USE s_alcerr
  USE s_sindat
  USE s_writcrux
  USE s_readcrux
  USE s_inquire
  USE s_readKeys
  USE s_ckopti
  USE f_nextline

  IMPLICIT NONE

! Local variables
! ---------------
  CHARACTER(LEN=10)              :: pgName= 'PG snx2slr'

  CHARACTER(LEN=fileNameLength)        :: snxfile   ! Input SINEX file
  CHARACTER(LEN=fileNameLength)        :: slrMaster ! Input SLR master file
  CHARACTER(LEN=fileNameLength)        :: slrMerge  ! Input SLR merge file
  CHARACTER(LEN=fileNameLength)        :: stafile   ! Output STA file
  CHARACTER(LEN=100)                   :: line
  CHARACTER(LEN=100)                   :: title=''
  CHARACTER(LEN=4)                     :: siteCode
  CHARACTER(LEN=2)                     :: pointCode
  CHARACTER(LEN=1)                     :: pointCode1
  CHARACTER(LEN=3)                     :: pointCode3
  CHARACTER(LEN=9)                     :: domes
  CHARACTER(LEN=1)                     :: technique
  CHARACTER(LEN=4)                     :: unit
  CHARACTER(LEN=3)                     :: satID
  CHARACTER(LEN=1)                     :: flag
  CHARACTER(LEN=12)                    :: startTime
  CHARACTER(LEN=12)                    :: endTime
  CHARACTER(LEN=12)                    :: value
  CHARACTER(LEN=7)                     :: sigma
  CHARACTER(LEN=17)                    :: remark
  CHARACTER(LEN=keyValueLength),DIMENSION(:),POINTER :: keyValue


  CHARACTER(LEN=19), DIMENSION(maxSta) :: station
  CHARACTER(LEN=22), DIMENSION(maxSta) :: descr

  TYPE(t_stacrux)                :: stacrux, master, merged
  TYPE(t_slrInfo)                :: slrInfo   ! for Output
  TYPE(t_slrInfo)                :: slrInfo1  ! Input: Master
  TYPE(t_slrInfo)                :: slrInfo2  ! Input: Merge

  INTEGER(i4b)                   :: irc, iac, ios, irCode
  INTEGER(i4b)                   :: nsta, ista, ii, jj, i1, i2
  INTEGER(i4b)                   :: nexcl=0
  INTEGER(i4b)                   :: nexcl2=0
  INTEGER(i4b)                   :: nLin
  INTEGER(i4b)                   :: nMerge
  INTEGER(i4b)                   :: posSnx1
  INTEGER(i4b), DIMENSION(:), ALLOCATABLE :: idx2
  INTEGER(i4b)                   :: mininte

  REAL(r8b), DIMENSION(2)        :: time

  LOGICAL                        :: found
  LOGICAL                        :: YES


! Start CPU Counter
! -----------------
  CALL cpu_start(.TRUE.)

! Nullify pointers
! ----------------
  NULLIFY(keyValue)

! Get the name of the input file
! ------------------------------
  CALL init_inpkey(inpKey)
  CALL readinpf(' ',inpKey)

! Define system files
! -------------------
  CALL opnsys
  CALL defcon(1)

  CALL pritit('SNX2SLR','Prepare SLR data handling file')
  CALL prflna

! Init structures
! ---------------
  CALL init_slrFil(slrInfo)

  nexcl  = 0
  nMerge = 0


! Read Input Options
! ------------------
  CALL gtflna(0,'SNXFILE', snxfile, irc)
  CALL gtflna(0,'SLRFIL1', slrMaster, irc)
  CALL gtflna(0,'SLRFIL2', slrMerge, irc)

  CALL gtflna(0,'SLR_OUT', slrInfo%slrFile, irc)
  CALL gtflna(0,'STAFILE', stafile, irc)


  CALL readKeys('MININTE', keyValue, irc)
  CALL ckopti(1,'MININTE', keyValue, pgName,        &
             'Minimum length of station exclusion', &
              irc,irCode,maxVal=1,result1=mininte)

  IF ( snxfile == '' .AND. slrMaster == '' ) THEN
    WRITE(lfnerr,"(/,' *** PG SNX2SLR: You have to specify a master file,', &
                    & /,16X,' either a SINEX or an SLR file.',/)")
    CALL exitrc(2)
  END IF

  IF ( snxfile /= '' .AND. slrMaster /= '' ) THEN
    WRITE(lfnerr,"(/,' *** PG SNX2SLR: Only one master file is allowed,', &
                    & /,16X,' either a SINEX or an SLR file.',/)")
    CALL exitrc(2)
  END IF

! ---------------
! Read SATCRUX
! ---------------
  IF ( stafile /= '' ) THEN
   CALL INQUIRE(FILE=stafile,EXIST=YES)


   CALL init_stacrux(master)
!   CALL init_stacrux(staCrux)
   CALL init_stacrux(merged)
   IF (YES) THEN
   call readcrux(stafile, master, title)
   END IF
   stacrux%technique = 'SLR '
   master%technique = 'SLR '
   nexcl2 = master%nprob

  END IF

! ---------------
! Read SINEX File
! ---------------
  IF ( snxfile /= '' ) THEN

    CALL init_slrFil(slrInfo1)

    CALL opnfil(lfnloc,snxfile,'OLD','FORMATTED','READONLY',' ',ios)
    CALL opnerr(lfnerr,lfnloc,ios,snxfile,pgName)

    nLin = 0

    Loop_snx: DO
      nLin = nLin + 1
      line = nextline(lfnloc,0)
      IF (line      == 'EOF' .OR. &
          line(1:7) == '%ENDSNX' ) EXIT  Loop_snx

      IF (line(1:1) /= '+'  ) CYCLE Loop_snx

    ! Read SITE/ID Block
    ! ------------------
      IF (line == '+SITE/ID') THEN
        nsta = 0

        Stat_Loop: DO
          nLin = nLin + 1
          line = nextline(lfnloc,0)
          IF (line(1:1) == '-') CYCLE Loop_snx
          IF (line(1:1) == '*') CYCLE Stat_Loop

          nsta = nsta + 1
          IF (nsta > maxSta) THEN
            WRITE(lfnerr,"(/,' *** PG SNX2SLR: Too many stations ', &
                            & 'in SINEX file ',A,/)") TRIM(snxfile)
            CALL exitrc(2)
          END IF

          READ(line,'(1x,a4, 1x,a2, 1x,a9, 1x,a1, 1x,a22)')       &
                    siteCode, pointCode, domes, &
                    technique, descr(nsta)

          IF (domes == '     M   ') domes = ''
          IF (domes == '     S   ') domes = ''

          WRITE(station(nsta),'(A4,1X,A9,1X,A2,1X,A1)') &
                              siteCode, domes, pointCode, technique
        END DO Stat_Loop
      END IF  ! SITE-ID

    ! Count data handling information
    ! -------------------------------
      IF (line == '+SOLUTION/DATA_HANDLING') THEN

        posSnx1 = nLin

        Loop_data: DO
          nLin = nLin + 1
          line = nextline(lfnloc,0)
          IF (line(1:1) == '-') CYCLE Loop_snx
          IF (line(1:1) == '*') CYCLE Loop_data

          READ(line,'(1x,a4, 1x,a3, 1x,a4, 1x,a1, 2(1x,a12), 1x,a1)' ) &
                    siteCode, pointCode3, unit, pointCode1, startTime, &
                    endTime, flag

         ! Find station name in list
         ! -------------------------
          found = .FALSE.
          DO ista = 1, nsta
            IF ( siteCode   == station(ista)(1:4) .AND. &
                 pointCode1 == station(ista)(17:17)      ) THEN
              found = .TRUE.
              EXIT
            END IF
          ENDDO
          IF ( .NOT. found ) CYCLE Loop_data

         ! Check SINEX flag
         ! -----------------
          found = .FALSE.
          DO ii = 0, nDataFlg
            IF ( flag == dataSNX(ii) ) THEN
              found = .TRUE.
              EXIT
            END IF
          ENDDO
          IF ( .NOT. found ) CYCLE Loop_data

         ! Exclusion of data
         ! -----------------
          IF ( found .AND. ii == 0 )  nexcl = nexcl + 1

         ! Specific data handling
         ! ----------------------
          IF ( found .AND. ii > 0 )  slrInfo1%nrgb = slrInfo1%nrgb + 1

        ENDDO Loop_data

      END IF ! DATA_HANDLING

    ENDDO Loop_snx


  ! Allocate memory
  ! ---------------
    ALLOCATE(slrInfo1%rgb(slrInfo1%nrgb), stat=iac)
    CALL alcerr(iac, 'slrInfo1%rgb', (/slrInfo1%nrgb/), pgName)

    ALLOCATE(stacrux%staprob(nexcl+nexcl2), stat=iac)
    CALL alcerr(iac, 'nexcl+nexcl2', (/nexcl+nexcl2/), pgName)


  ! Read information from SINEX master file into structures
  ! -------------------------------------------------------
    REWIND(lfnloc)

    nexcl = 0
    slrInfo1%nrgb = 0

    DO ii = 1, posSnx1
      line = nextline(lfnloc,0)
    ENDDO

    Loop_rec: DO
      line = nextline(lfnloc,0)
      IF (line      == 'EOF') EXIT  Loop_rec
      IF (line(1:1) == '*'  ) CYCLE Loop_rec
      IF (line(1:1) == '+'  ) CYCLE Loop_rec

      IF ( line(1:1) == '-' ) EXIT  Loop_rec

      READ(line,'(1x,a4, 1x,a3, 1x,a4, 1x,a1, 2(1x,a12), 1x,a1, 1x,a12, 1x,a7, 1x,a17)' ) &
                siteCode, satID, unit, pointCode1, startTime, &
                endTime, flag, value, sigma, remark

    ! Find station name in list
    ! -------------------------
      found = .FALSE.
      DO ista = 1, nsta
        IF ( siteCode   == station(ista)(1:4) .AND. &
             pointCode1 == station(ista)(17:17)      ) THEN
          found = .TRUE.
          EXIT
        END IF
      ENDDO
      IF ( .NOT. found ) CYCLE Loop_rec

    ! Check SINEX flag
    ! -----------------
      found = .FALSE.
      DO ii = 0, nDataFlg
        IF ( flag == dataSNX(ii) ) THEN
          found = .TRUE.
          EXIT
        END IF
      ENDDO
      IF ( .NOT. found ) CYCLE Loop_rec

    ! -----------------
    ! Exclusion of data
    ! -----------------
      IF ( ii == 0 )  THEN
        nexcl = nexcl + 1
        stacrux%staprob(nexcl)%stanam = station(ista)(1:14)
        stacrux%staprob(nexcl)%remark = remark

      ! Set time window
      ! ---------------
        CALL sindat(1, time(1), startTime)
        CALL sindat(1, time(2), endTime)

        stacrux%staprob(nexcl)%timint%t(1) = time(1)
        stacrux%staprob(nexcl)%timint%t(2) = time(2)

      ! If time window smaller than mininte (e.g. 1 day) - do not consider
      ! ------------------------------------------------------------------
        IF ( time(2)-time(1) < mininte .AND.time(2).NE.0d0 ) nexcl = nexcl - 1

        stacrux%nprob = nexcl
        stacrux%staprob(nexcl)%flg= '001'

    ! ----------------------
    ! Specific data handling
    ! ----------------------
      ELSEIF ( ii > 0 )  THEN
        slrInfo1%nrgb = slrInfo1%nrgb + 1

        slrInfo1%rgb(slrInfo1%nrgb)%staNam = ''
        slrInfo1%rgb(slrInfo1%nrgb)%staNam(1:14) = station(ista)(1:14)

        slrInfo1%rgb(slrInfo1%nrgb)%corrTyp = typBSW(ii)
        slrInfo1%rgb(slrInfo1%nrgb)%solFlag = flagBSW(ii)

        IF ( ii == 5 ) THEN
          slrInfo1%rgb(slrInfo1%nrgb)%WLchar = ' '
          slrInfo1%rgb(slrInfo1%nrgb)%WLind  = 0
        ELSE
          slrInfo1%rgb(slrInfo1%nrgb)%WLchar = 'R'
          slrInfo1%rgb(slrInfo1%nrgb)%WLind  = 1
        END IF

        READ(value, '(e21.14)') slrInfo1%rgb(slrInfo1%nrgb)%value
        READ(sigma, '(e21.14)') slrInfo1%rgb(slrInfo1%nrgb)%sigma

        ! Rescale to meter
        ! ----------------
        IF ( unit(1:2) == 'mm' ) THEN
          slrInfo1%rgb(slrInfo1%nrgb)%value = 1d-3 * slrInfo1%rgb(slrInfo1%nrgb)%value
          slrInfo1%rgb(slrInfo1%nrgb)%sigma = 1d-3 * slrInfo1%rgb(slrInfo1%nrgb)%sigma
        END IF

        slrInfo1%rgb(slrInfo1%nrgb)%remark = remark


      ! Get satellite ID
      ! ----------------
        IF ( satID == '---' .OR. satID == '   ' ) THEN
          slrInfo1%rgb(slrInfo1%nrgb)%satNum = 1000

        ELSE
          READ(satID, '(1x,i2)') slrInfo1%rgb(slrInfo1%nrgb)%satNum
!          WRITE(slrInfo1%rgb(slrInfo1%nrgb)%satNum,'(i2)')  satID(2:3)
          DO jj = 0, 10
            IF ( g_svnsys(jj) == satID(1:1) ) THEN
              slrInfo1%rgb(slrInfo1%nrgb)%satNum =  &
                                slrInfo1%rgb(slrInfo1%nrgb)%satNum + jj*100
              EXIT
            END IF
          END DO

        END IF  ! satellite


      ! Set time window
      ! ---------------
        CALL sindat(1, time(1), startTime)
        CALL sindat(1, time(2), endTime)

        slrInfo1%rgb(slrInfo1%nrgb)%timWin%t(1) = time(1)
        slrInfo1%rgb(slrInfo1%nrgb)%timWin%t(2) = time(2)
      ENDIF


    ENDDO Loop_rec

    CLOSE (lfnloc)  ! SINEX file


! ---------------------
! Read SLR master File
! ---------------------
  ELSEIF ( slrMaster /= '' ) THEN

    CALL readrgb(slrMaster, slrInfo1)

  END IF  ! Master file selection


! ----------------------
! SLR File to be merged
! ----------------------
  IF ( slrMerge /= '' ) THEN

    CALL readrgb(slrMerge, slrInfo2)

   ! Index array for merging
   ! -----------------------
    ALLOCATE(idx2(slrInfo2%nrgb), stat=iac)
    CALL alcerr(iac, 'idx2', (/slrInfo2%nrgb/), pgName)
    idx2(:) = 0

   ! Look for entries to be merged
   ! -----------------------------
    Loop_2: DO i2 = 1, slrInfo2%nrgb

      Loop_1: DO i1 = 1, slrInfo1%nrgb

        IF ( slrInfo2%rgb(i2)%corrTyp == slrInfo1%rgb(i1)%corrTyp .AND. &
             slrInfo2%rgb(i2)%staNam  == slrInfo1%rgb(i1)%staNam  .AND. &
             slrInfo2%rgb(i2)%satNum  == slrInfo1%rgb(i1)%satNum  .AND. &
             slrInfo2%rgb(i2)%WLind   == slrInfo1%rgb(i1)%WLind          ) &
          CYCLE Loop_2

      ENDDO Loop_1

      nMerge = nMerge + 1
      idx2(nMerge) = i2

    ENDDO Loop_2

  END IF  ! SLR Merge file


! -------------------------------------
! Copy Master and Merge file to output
! -------------------------------------
  slrInfo%nrgb = slrInfo1%nrgb + nMerge

  ALLOCATE(slrInfo%rgb(slrInfo%nrgb), stat=iac)
  CALL alcerr(iac, 'slrInfo%rgb', (/slrInfo%nrgb/), pgName)

 ! Master file
 ! -----------
  DO ii = 1, slrInfo1%nrgb
    slrInfo%rgb(ii)%corrTyp = slrInfo1%rgb(ii)%corrTyp
    slrInfo%rgb(ii)%staNam  = slrInfo1%rgb(ii)%staNam
    slrInfo%rgb(ii)%satNum  = slrInfo1%rgb(ii)%satNum
    slrInfo%rgb(ii)%WLchar  = slrInfo1%rgb(ii)%WLchar
    slrInfo%rgb(ii)%WLind   = slrInfo1%rgb(ii)%WLind
    slrInfo%rgb(ii)%value   = slrInfo1%rgb(ii)%value
    slrInfo%rgb(ii)%sigma   = slrInfo1%rgb(ii)%sigma
    slrInfo%rgb(ii)%timWin  = slrInfo1%rgb(ii)%timWin
    slrInfo%rgb(ii)%solFlag = slrInfo1%rgb(ii)%solFlag
    slrInfo%rgb(ii)%remark  = slrInfo1%rgb(ii)%remark
  END DO

 ! Merge file
 ! ----------
  IF ( nMerge > 0 ) THEN

    DO ii = 1, nMerge
      jj = slrInfo1%nrgb + ii

      slrInfo%rgb(jj)%corrTyp = slrInfo2%rgb(idx2(ii))%corrTyp
      slrInfo%rgb(jj)%staNam  = slrInfo2%rgb(idx2(ii))%staNam
      slrInfo%rgb(jj)%satNum  = slrInfo2%rgb(idx2(ii))%satNum
      slrInfo%rgb(jj)%WLchar  = slrInfo2%rgb(idx2(ii))%WLchar
      slrInfo%rgb(jj)%WLind   = slrInfo2%rgb(idx2(ii))%WLind
      slrInfo%rgb(jj)%value   = slrInfo2%rgb(idx2(ii))%value
      slrInfo%rgb(jj)%sigma   = slrInfo2%rgb(idx2(ii))%sigma
      slrInfo%rgb(jj)%timWin  = slrInfo2%rgb(idx2(ii))%timWin
      slrInfo%rgb(jj)%solFlag = slrInfo2%rgb(idx2(ii))%solFlag
      slrInfo%rgb(jj)%remark  = slrInfo2%rgb(idx2(ii))%remark

    END DO

    DEALLOCATE(idx2, stat=iac)
    DEALLOCATE(slrInfo2%rgb, stat=iac)

  END IF


! Allocate arrays for combined file
! ---------------------------------

  IF ( stafile /= '' ) THEN
    IF (YES) THEN
    call readcrux(stafile, merged, title)

    ALLOCATE(merged%staprob(master%nprob+stacrux%nprob),stat=iac)
    CALL alcerr(iac,'merged%staprob',(/master%nprob+stacrux%nprob/),pgName)

    merged%renamSta= master%renamSta
    merged%stainfo=  master%stainfo
    merged%coovel =  master%coovel
    merged%statype=  master%statype
    merged%staprob=  master%staprob

    ELSE

    ALLOCATE(merged%staprob(stacrux%nprob),stat=iac)
    CALL alcerr(iac,'merged%staprob',(/stacrux%nprob/),pgName)

    END IF

    DO ista = 1, nexcl
        merged%staprob(ista+master%nprob)%stanam =      stacrux%staprob(ista)%stanam
        merged%staprob(ista+master%nprob)%remark =      stacrux%staprob(ista)%remark
        merged%staprob(ista+master%nprob)%flg =         stacrux%staprob(ista)%flg
        merged%staprob(ista+master%nprob)%timint%t(1) = stacrux%staprob(ista)%timint%t(1)
        merged%staprob(ista+master%nprob)%timint%t(2) = stacrux%staprob(ista)%timint%t(2)
    END DO
    merged%technique = 'SLR '
    merged%nprob=master%nprob+stacrux%nprob
  END IF

! -------------------------
! Write SLR Correction File
! -------------------------
  IF ( slrInfo%nrgb > 0 ) THEN
    CALL wtSlrFil(slrInfo)
  END IF


! -------------------------
! Write merged STA File
! -------------------------
  IF (( nexcl > 0 ).AND.( stafile /= '' ))  THEN
 !   write(*,*) 'nexcl ', nexcl
    CALL WRITCRUX(stafile, merged, title)
  END IF


  DEALLOCATE(slrInfo1%rgb, stat=iac)
  DEALLOCATE(slrInfo%rgb, stat=iac)
  DEALLOCATE(stacrux%staprob, stat=iac)
  DEALLOCATE(merged%staprob, stat=iac)
  DEALLOCATE(master%staprob, stat=iac)

! -------------------
! Write STA-Info File
! -------------------


  CALL exitrc(0)

END PROGRAM snx2slr
