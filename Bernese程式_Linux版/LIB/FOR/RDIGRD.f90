MODULE s_RDIGRD
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE rdigrd(nAllSta, allStaNum, allStaName, opLoad)

! -------------------------------------------------------------------------
! Purpose:    Reads the scaling factor input options for GPSEST
!
! Author:     R. Dach
!
! Created:    30-Apr-2009
!
! Changes:    27-Apr-2012 RD: Nullify all pointers, use m_bern with only
!             07-Jun-2012 MM: Adapted to new keywords
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b, lfnerr, lfnloc, &
                      keyNameLength, keyValueLength, &
                      staNameLength, shortLineLength, fileNameLength
  USE p_gpsest, ONLY: t_optLoad
  USE d_grid,   ONLY: grdNeq, getGridInp
  USE s_readkeys
  USE s_ckoptc
  USE s_ckoptb
  USE s_ckoptr
  USE s_alcerr
  USE s_gtfile2
  USE s_gtflna
  USE s_exitrc
  USE s_gtflna
  USE s_opnfil
  USE s_opnerr
  IMPLICIT NONE

! List of Parameters
! ------------------
! input:
  INTEGER(i4b)                 :: nAllSta    ! number of all stations
  INTEGER(i4b), DIMENSION(*)   :: allStaNum  ! station numbers
  CHARACTER(LEN=staNameLength),&
                DIMENSION(:)   :: allStaName ! station names

! output:
  TYPE(t_optLoad), DIMENSION(3):: opLoad ! Scaling factors for vienna grid files
                                         ! 1: Atmospheric non-tidal loading
                                         ! 2: Ocean non-tidal loading
                                         ! 3: Hydrostatic pressure loading

! List of Functions
! -----------------

! Local Types
! -----------

! Local Parameters
! ----------------
  CHARACTER(LEN=6), PARAMETER :: srName = 'rdigrd'

! Define some keywords
  CHARACTER(LEN=keyNameLength), DIMENSION(3,3), PARAMETER :: grdKeyw = &
  reshape( source =                                                    &
           (/ 'ALOADSET', 'ONTLDSET', 'HLOADSET',                      &
              'ALOADCLU', 'ONTLDCLU', 'HLOADCLU',                      &
              'ALOADDEC', 'ONTLDDEC', 'HLOADDEC' /),shape = (/ 3,3 /) )

  CHARACTER(LEN=keyNameLength), DIMENSION(3,3), PARAMETER :: grdSigma = &
  reshape( source =                                                     &
           (/ 'ALOADSIG1', 'ONTLDSIG1', 'HLOADSIG1',                    &
              'ALOADSIG2', 'ONTLDSIG2', 'HLOADSIG2',                    &
              'ALOADSIG3', 'ONTLDSIG3', 'HLOADSIG3' /),                 &
           shape = (/ 3,3 /) )

! Local Variables
! ---------------
  CHARACTER(LEN=keyValueLength), &
       DIMENSION(:), POINTER    :: keyValue
  CHARACTER(LEN=fileNameLength), &
       DIMENSION(:,:), POINTER  :: filnam
  CHARACTER(LEN=fileNameLength) :: cluFil
  CHARACTER(LEN=shortLineLength):: line
  CHARACTER(LEN=staNameLength)  :: staNam

  INTEGER(i4b)                  :: nFil
  INTEGER(i4b)                  :: iOpt
  INTEGER(i4b)                  :: ii,jj
  INTEGER(i4b)                  :: iSta, jSta
  INTEGER(i4b)                  :: cluNum
  INTEGER(i4b)                  :: irCode
  INTEGER(i4b)                  :: irc, iac, ios

! Init the variables
! ------------------
  irCode = 0
  NULLIFY(keyValue)
  NULLIFY(filnam)

! Read options for the scaling factors
! ------------------------------------
  DO ii = 1, SIZE(grdKeyw,1)

    ! Copy the keyword into the structure
    IF (SIZE(grdNeq) < ii) THEN
      WRITE(lfnerr,'(/,A,2(/,16X,A),/)') &
           ' *** SR RDIGRD: There is an inconcistency between the number of ', &
                           'supported Vienna grid file types between GPSEST ', &
                           'and the list in D_GRID.'
      CALL exitrc(2)
    ENDIF
    opLoad(ii)%keyw = grdNeq(ii)

    ! Check whether there are some grid files
    CALL gtfile2(getGridInp(grdNeq(ii)),1,nFil,filnam)

    ! Scaling factors for stations
    IF ( nFil > 0 ) THEN
      CALL readKeys(grdKeyw(ii,1), keyValue, irc)
      CALL ckoptc(1,grdKeyw(ii,1), keyValue, (/'NONE       ',    &
                  'PER_STATION','PER_GROUP  ','ONE_OVERALL'/),   &
                  srName, 'Scaling factor for grid files', irc, irCode,   &
                  maxVal = 1, result1 = iOpt)
    ENDIF

    ! No Request of a scaling factor
    IF (nFil == 0 .OR. irc /= 0 .OR. iOpt == 1) THEN
      opLoad(ii)%nSta = 0

    ELSE
      ALLOCATE(opLoad(ii)%staLst(nAllSta),stat=iac)
      CALL alcerr(iac,'opLoad%staLst',(/nAllSta/),srName)
      ALLOCATE(opLoad(ii)%staClu(nAllSta),stat=iac)
      CALL alcerr(iac,'opLoad%staClu',(/nAllSta/),srName)

      ! One scaling factor for each station
      IF (iOpt == 2) THEN
        opLoad(ii)%nSta = nAllSta
        opLoad(ii)%staLst = allStaName
        opLoad(ii)%staClu = 0

      ! Scaling factors according to a cluster file
      ELSE IF (iOpt == 3) THEN
        opLoad(ii)%nSta = nAllSta
        opLoad(ii)%staLst = allStaName
        opLoad(ii)%staClu = 0

        CALL gtflna(1,grdKeyw(ii,2),cluFil,irc)
        CALL opnfil(lfnloc,cluFil,'OLD','FORMATTED','READONLY',' ',irc)
        CALL opnerr(lfnerr,lfnloc,irc,cluFil,srName)

        READ(lfnloc,'(////)',iostat=ios)

        iSta = 0
        DO WHILE (ios == 0)
          READ(lfnloc,'(A)',iostat=ios) line
          IF (ios /= 0 .OR. LEN_TRIM(line) == 0) EXIT

          iSta = iSta + 1
          READ(line,'(A16,2X,I3)',iostat=irc) staNam, cluNum

          IF (irc /= 0) THEN
            WRITE(lfnerr,'(/,A,/,16X,A,A,/,16X,A,I6,/)')            &
              ' *** SR RDIGRD: Error reading cluster file records', &
                              'File name:     ', TRIM(cluFil),      &
                              'Record number: ',iSta
            CALL exitrc(2)
          ENDIF

          DO jSta = 1,opLoad(ii)%nSta
            IF (opLoad(ii)%staLst(jSta) == staNam) THEN
              opLoad(ii)%staClu(jSta) = cluNum
              EXIT
            ENDIF
          ENDDO

        ENDDO

        CLOSE(lfnloc)

        DO iSta = 1,opLoad(ii)%nSta
          IF (opLoad(ii)%staClu(iSta) == 0) THEN
            WRITE(lfnerr,'(/,A,/,16X,A,/,16X,A,A)')                           &
              ' *** SR RDIGRD: The following stations are not listet in the', &
                              'cluster file. No scaling parameters are setup',&
                              'for station(s) ', TRIM(opLoad(ii)%staLst(iSta))
            DO jSta = iSta+1,opLoad(ii)%nSta
              IF (opLoad(ii)%staClu(jSta) == 0) THEN
                WRITE(lfnerr,'(31X,A)') TRIM(opLoad(ii)%staLst(jSta))
              ENDIF
            ENDDO
            WRITE(lfnerr,'(16X,A,A,/)')'File name:     ', TRIM(cluFil)
            EXIT
          ENDIF
        ENDDO

        jSta = 0
        DO iSta = 1,opLoad(ii)%nSta
          IF (opLoad(ii)%staClu(iSta) /= 0) THEN
            jSta = jSta + 1
            IF (jSta /= iSta) THEN
              opLoad(ii)%staLst(jSta) = opLoad(ii)%staLst(iSta)
              opLoad(ii)%staClu(jSta) = opLoad(ii)%staClu(iSta)
            ENDIF
          ENDIF
        ENDDO
        opLoad(ii)%nSta = jSta
      ! One scaling factor for each station
      ELSEIF (iOpt == 4) THEN
        opLoad(ii)%nSta = nAllSta
        opLoad(ii)%staLst = allStaName
        opLoad(ii)%staClu = -1
      ENDIF
    ENDIF

    ! What types of scaling parameters are requested?
    IF ( opLoad(ii)%nSta > 0 ) THEN
      CALL readKeys(grdKeyw(ii,3), keyValue, irc)
      CALL ckoptc(1,grdKeyw(ii,3), keyValue, (/'FULL_EFFECT        ', &
                  'HORIZONTAL/VERTICAL','NORTH/EAST/UP      ' /),     &
                  srName, 'Parameter decomposition', irc, irCode,     &
                  maxVal = 1, result1 = opLoad(ii)%nPar)

      ! Read the apriori sigmas
      opLoad(ii)%sigma = 0D0
      DO jj = 1,opLoad(ii)%nPar
        CALL readKeys(grdSigma(ii,jj), keyValue, irc)
        CALL ckoptr(1,grdSigma(ii,jj), keyValue, srName,  &
                   'Sigma for scaling factors', irc, irCode,              &
                    maxVal = 1, ge = 0d0, empty = 0d0,                    &
                    result1 = opLoad(ii)%sigma(jj))
      ENDDO
    ENDIF
  ENDDO

! Deallocate local pointers
! -------------------------
  DEALLOCATE(keyValue,stat=iac)

  IF (irCode /= 0) CALL exitrc(2)

  RETURN
END SUBROUTINE rdigrd

END MODULE
