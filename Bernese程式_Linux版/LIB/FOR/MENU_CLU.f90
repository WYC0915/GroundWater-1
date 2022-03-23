MODULE s_MENU_CLU
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE menu_clu(keyWord)

! -------------------------------------------------------------------------
! Purpose:    Is used for creating, editing, and storing of Bernese
!             station cluster files using the menu program
!             (called by the menu program via MENUAUX)
!
! Author:     R. Dach
!
! Created:    08-May-2002
!
! Changes:    25-Sep-2002 HU: Remove i_astlib
!             13-Dec-2002 RD: New quotes-handling in SR writekey
!             19-Feb-2003 RD: Stop with error (new logic in menu)
!             23-Apr-2003 AJ: Nullify local pointers
!             18-Oct-2003 HU: Write date/time to title
!             06-Nov-2003 HB: Check if pointer (keys(ii)%value) is
!                             ASSOCIATED before DEALLOCATE
!             19-Nov-2003 RD: Reread INP-file using READINPF
!             20-Sep-2012 RD: Use M_BERN with ONLY
!             20-Sep-2012 RD: Correctly deallocate arrays
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, t_key, lfnloc, lfnerr, &
                      keyValueLength, staNameLength, lineLength, shortLineLength
  USE d_inpkey, ONLY: inpkey
  USE p_menaux, ONLY: qt
  USE s_alcerr
  USE s_opnfil
  USE s_ckoptu
  USE f_lincount
  USE s_inquire
  USE s_opnerr
  USE s_readinpf
  USE s_dattim
  USE s_getco3
  USE s_writekey
  USE s_readkeys
  USE s_exitrc
  USE s_gtflna
  USE s_ckopti
  USE s_ckoptl
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  CHARACTER(LEN=*)                       :: keyWord     ! what to do

! output:

! Local Types
! -----------
  TYPE t_cluster
    CHARACTER(staNameLength)    :: staNam
    INTEGER(i4b)                :: cluNum
    CHARACTER(shortLineLength)  :: comment
  END TYPE t_cluster

! Local Parameters
! ----------------
  CHARACTER(LEN=8), PARAMETER   :: srName = 'menu_clu'

! Local Variables
! ---------------
  TYPE(t_cluster),               &
      DIMENSION(:), ALLOCATABLE :: cluster
  TYPE(t_key),                   &
      DIMENSION(:), ALLOCATABLE :: keys    ! Writing a PLDEDIT panel

  CHARACTER(LEN=keyValueLength), &
      DIMENSION(:), POINTER     :: keyValue
  CHARACTER(LEN=keyValueLength), &
      DIMENSION(:,:),ALLOCATABLE:: hlpStr

  CHARACTER(LEN=keyValueLength) :: cluFil  ! cluster file name
  CHARACTER(LEN=keyValueLength) :: edtFil  ! EDIT CLU input panel
  CHARACTER(LEN=keyValueLength) :: crdFil  ! Coodinate file name
  CHARACTER(LEN=lineLength)     :: line
  CHARACTER(LEN=staNameLength)  :: stanam
  CHARACTER(LEN=staNameLength),  &
      DIMENSION(:), POINTER     :: stname  ! Station names
  CHARACTER(LEN=shortLineLength):: title
  CHARACTER(LEN=1),              &
      DIMENSION(1)              :: flags
  CHARACTER(LEN=9)              :: date
  CHARACTER(LEN=5)              :: time

  INTEGER(i4b)                  :: maxStaLoc
  INTEGER(i4b)                  :: nstat   ! Number of stations from GTVELO
  INTEGER(i4b)                  :: nflag
  INTEGER(i4b)                  :: numKeys ! Writing a PLDEDIT panel
  INTEGER(i4b)                  :: iSta
  INTEGER(i4b)                  :: ii
  INTEGER(i4b)                  :: irCode
  INTEGER(i4b)                  :: irc, iac, ios

  LOGICAL                       :: yes
  LOGICAL                       :: sorted

  NULLIFY(keyValue)
  NULLIFY(stname)

! Incorrect keyword
! -----------------
  IF (keyWord /= 'CLU_EDIT' .AND. keyWord /= 'CLU_SAVE') RETURN

! Editing/Create a CLU file
! -------------------------
  IF (keyWord == 'CLU_EDIT') THEN

! Get the file names
! ------------------
    CALL gtflna(1,'FILE_EDIT',    cluFil,irc)
    CALL gtflna(1,'FILE_SKELETON',edtFil,irc)
    CALL gtflna(0,'FILE_CRD',     crdFil,irc)

! Does the Cluster File Exist ?
! ------------------------------
    CALL INQUIRE(FILE=cluFil , EXIST=yes)

! Read an Old Cluster File
! -------------------------
    IF (yes) THEN

      nStat = linCount(cluFil,5)

      ALLOCATE(cluster(nStat),stat=iac)
      CALL alcerr(iac,'cluster',(/nStat/),srName)

      CALL opnfil(lfnloc,cluFil,'OLD','FORMATTED','READONLY',' ',irc)
      CALL opnerr(lfnerr,lfnloc,irc,cluFil,srName)

      READ(lfnloc,'(A,////)',iostat=ios) title

      iSta = 0
      DO WHILE (ios == 0 .AND. iSta < nStat)
        READ(lfnloc,'(A)',iostat=ios) line
        IF (ios /= 0 .OR. LEN_TRIM(line) == 0) EXIT

        iSta = iSta+1
        READ(line,'(A16,2X,I3,2X,A)',iostat=irc) &
            cluster(iSta)%staNam, cluster(iSta)%cluNum, cluster(iSta)%comment

        IF (irc /= 0) THEN
          WRITE(lfnerr,'(/,A,/,18X,A,A,/,18X,A,I6,/)')            &
          ' *** SR MENU_CLU: Error reading cluster file records', &
                            'File name:     ', TRIM(cluFil),      &
                            'Record number: ',iSta
          CALL exitrc(2)
        ENDIF

      ENDDO

      CLOSE(lfnloc)

! Create a new cluster file from crd Fil
! ---------------------------------------
    ELSE IF (LEN_TRIM(crdFil) > 0) THEN

      nflag    = 1
      flags(1) = '@'
      CALL getco3(crdFil, nflag, flags, nstat, stname)

      ! Sort the station names:
      sorted = .FALSE.
      DO WHILE (.NOT. sorted)
        sorted = .TRUE.
        DO ii = 1,nStat-1
          IF (stname(ii) > stName(ii+1)) THEN
            stanam=stname(ii)
            stname(ii)=stname(ii+1)
            stname(ii+1)=stanam

            sorted = .FALSE.
          ENDIF
        ENDDO
      ENDDO

      title = ''

      ALLOCATE(cluster(nStat),stat=iac)
      CALL alcerr(iac,'cluster',(/nStat/),srName)

      cluster(:)%staNam = stName
      cluster(:)%cluNum = 1
      cluster(:)%comment= ' '

      DEALLOCATE(stName,stat=iac)

! Init Entries for a new empty Cluster File
! ------------------------------------------
    ELSE
      title        = ''
      nstat        = 0

      ALLOCATE(cluster(1),stat=iac)
      CALL alcerr(iac,'cluster',(/1/),srName)
    ENDIF

    numKeys = 3
    ALLOCATE(keys(numKeys), STAT=iac)
    CALL alcerr(iac, 'keys', (/numKeys/), srName)

    DO ii = 2, numKeys
      ALLOCATE( keys(ii)%value(1), STAT=iac )
      CALL alcerr(iac, 'keys(ii)%value', (/1/), srName)
    END DO

    keys(1)%name = 'STACLU'
    keys(2)%name = 'STACLURS'  ;  keys(2)%value(1) = cluFil
    keys(3)%name = 'TITLE'     ;  keys(3)%value(1) = title(1:64)

    IF (nstat > 0) THEN
      ALLOCATE(keys(1)%value(nstat), stat=iac)
      CALL alcerr(iac, 'keys(1)%value', (/nstat/), srName)
      DO ii = 1, nstat
        WRITE(keys(1)%value(ii),'(a1,a,a1, 2x, a1,i3,a1, 2x, a1,a,a1)') &
                         qt, TRIM(cluster(ii)%stanam), qt,  &
                         qt, cluster(ii)%clunum,       qt,  &
                         qt, TRIM(cluster(ii)%comment),qt
      END DO
    ELSE
      ALLOCATE(keys(1)%value(1), stat=iac)
      CALL alcerr(iac, 'keys(1)%value', (/1/), srName)
      keys(1)%value(1)=''
      WRITE(keys(1)%value(1),'(3(A,1X))') (qt//qt, ii=1,3)
    ENDIF
    CALL writeKey(edtFil, keys, 0, irc)

    DEALLOCATE(cluster,stat=irc)

! Deallocate keys-record
! ----------------------
    DO ii = 1, SIZE(keys)
      IF ( ASSOCIATED(keys(ii)%value) ) THEN
        DEALLOCATE(keys(ii)%value, STAT=irc)
      ENDIF
    END DO
    DEALLOCATE(keys, STAT=irc)

! Store a Cluster File
! ---------------------
  ELSE IF (keyWord == 'CLU_SAVE') THEN

! Get the file names
! ------------------
    CALL gtflna(1,'FILE_OUTPUT', cluFil,irc)
    CALL gtflna(1,'FILE_INPUT',  edtFil,irc)

! Reset the Name of the INP-File
! ------------------------------
    irCode = 0
    CALL readinpf(edtFil,inpKey)
    CALL readkeys('',keyValue,irc)

    ! Title line
    CALL readkeys('TITLE' , keyValue, irc)

    CALL ckoptl(0,'TITLE',keyValue,srName,                 &
                'Cluster file: title line',irc,irCode,     &
                empty=' ',maxVal=1,result1=title)

    ! Cluster record
    CALL readkeys('STACLU', keyValue, irc)
    nstat = SIZE( keyValue )

    maxStaLoc = nStat
    IF (maxStaLoc == 0) maxStaLoc = 1

! Allocate the memory for the cluster records
! --------------------------------------------
    ALLOCATE(hlpStr(3,maxStaLoc),stat=irc)
    CALL alcerr(irc, 'hlpStr', (/3,maxStaLoc/), srName)

    ALLOCATE(cluster(maxStaLoc), stat=irc)
    CALL alcerr(irc, 'cluster', (/maxStaLoc/),   srName)

! Extract the information
! -----------------------
    CALL ckoptu(1,'STACLU',keyValue,srName,                 &
                'Cluster file: data records',irc,irCode,    &
                numCol=SIZE(hlpStr,1),maxVal=SIZE(hlpStr,2),&
                result2=hlpStr)

    IF (irCode /= 0) CALL exitrc(2)

    ! Station name
    CALL ckoptl(1,'STACLU',hlpStr(1,:),srName,              &
                'Cluster file: data records',irc,irCode,    &
                colTit='Station names',maxVal=SIZE(hlpStr,2),&
                empty=' ',result2=cluster(:)%staNam)

    IF (LEN_TRIM(cluster(1)%stanam) == 0) THEN
      nstat = 0
    ELSE
    ! Cluster number
    CALL ckopti(1,'STACLU',hlpStr(2,:),srName,                 &
                'Cluster file: data records',irc,irCode,       &
                colTit='Cluster numbers',maxVal=SIZE(hlpStr,2),&
                ge=1,result2=cluster(:)%cluNum)

    ! Comments
    CALL ckoptl(1,'STACLU',hlpStr(3,:),srName,             &
                'Cluster file: data records',irc,irCode,   &
                colTit='Comments',maxVal=SIZE(hlpStr,2),   &
                empty=' ',result2=cluster(:)%comment)
    ENDIF

    DEALLOCATE(hlpStr, stat=irc)
    IF (irCode /= 0) CALL exitrc(2)

! Date and time in title line
! ---------------------------
    CALL dattim(date,time)
    title(65:80) = ' '//date//' '//time

! Write the new cluster file
! ---------------------------
    CALL opnfil(lfnloc,cluFil,'UNKNOWN','FORMATTED',' ',' ',irc)
    CALL opnerr(lfnerr,lfnloc,irc,cluFil,srName)

    WRITE(lfnloc,'(A,/A,//,A,/,A)') TRIM(title),            &
         '----------------------------------------' //      &
         '----------------------------------------',        &
         'STATION NAME      CLU','****************  ***'

    DO iSta = 1, nStat

      WRITE(lfnloc,'(A16,2X,I3,2X,A)')                  &
            cluster(iSta)%staNam, cluster(iSta)%cluNum, &
            TRIM(cluster(iSta)%comment)

    ENDDO

    CLOSE(lfnloc)

    DEALLOCATE(cluster, stat=irc)

  ENDIF

  DEALLOCATE(keyValue,stat=irc)

  RETURN
END SUBROUTINE menu_clu

END MODULE
